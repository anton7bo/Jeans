unit FibEngine;
//*22.01.20 ConnectDB, GenSPParams

interface

Uses
  Classes,
  pFIBDatabase,
  pFIBQuery,
  LogEngine;

const
  errDeadlock = -2;
  errOther = -1;
  errNone = 0;

  sqlSPInParams = 'SELECT distinct r.rdb$parameter_name, f.rdb$field_length, r.rdb$parameter_number, f.rdb$field_type, t.rdb$type_name '+  //*16.04.19
              'FROM rdb$procedure_parameters r '+
              'left join rdb$fields f on f.rdb$field_name = r.rdb$field_source '+
              'left join rdb$types t on t.rdb$type = f.rdb$field_type and t.rdb$field_name = ''RDB$FIELD_TYPE'' '+  //+16.04.19
              'WHERE r.rdb$procedure_name = Upper(''%s'') '+
              '  and r.rdb$parameter_type = 0 '+
              'order by r.rdb$parameter_number';

type
//  TOnWriteLogEnvent = procedure (Mess: String; const Indent: Integer = 0; const WithTime: boolean = True) of object;

  TFibEngine = class
  private
    FDbName: string;
    FDbUser: string;
    FDbPassword: string;

    FDb: TpFIBDatabase;
    FReadTransaction: TpFIBTransaction;
    FUpdateTransaction: TpFIBTransaction;

    FLogEngine: TLogEngine;  //+09.09.19
    FErrMessages: TStringList;  //+23.09.19
    procedure WriteLog(Mess: String; const Indent: Integer = 1; const WithTime: boolean = True);  //+09.09.19
    procedure DbAfterConnect(Sender: TObject);
    procedure DbAfterDisconnect(Sender: TObject);
  public
    property ErrMessages: TStringList read FErrMessages;  //+23.09.19
    constructor Create(ADbName: string; ADbUser: string; ADbPassword: string); overload;
    constructor Create(ADbName: string; ADbUser: string; ADbPassword: string; LogEngine: TLogEngine);  overload;
    destructor Destroy; override;

    procedure SetDb(ADbName: string; ADbUser: string; ADbPassword: string);
    procedure ConnectDb(const DoReconnect: Boolean = False);
    procedure Disconnect;

    function CreateReadQuery: TpFIBQuery;
    function CreateWriteQuery: TpFIBQuery;
    function ExecScript(Script:TStrings; StopOnErr: Boolean):Integer; overload;
    function ExecScript(FName:String; StopOnErr: Boolean):Integer; overload;

    function GenSPParams(SPName: String; const Prefix: String = ''):String;  //входящие параметры для процедуры через запятую
    function GenSPParamsMapValues(SPName: String; FldValues: TStrings):String;  //на основе списка типа Fld=Val формирует значения входящих параметров для процедуры по именам полей
    function IsEmptyRecord(Query: TpFibQuery): Boolean;  //+28.05.19
    procedure QuerySetParams(Query: TpFibQuery; FldValues: TStringList);  //+02.10.19
    function GetSQLParams(SQL: String): String;  //+02.10.19

    function QueryValueAsStr(const aSQL: string; FieldNo: Integer):String;  //+05.11.19
  end;

implementation
Uses
  iBase,
  Fib,
  FibDataBase,
  Forms,
//  MailUnit,
//  MyUtils,
  SysUtils;

procedure TFibEngine.SetDb(ADbName, ADbUser, ADbPassword: string);
begin
  FDbName := ADbName;
  FDbUser := ADbUser;
  FDbPassword := ADbPassword;
end;

procedure TFibEngine.ConnectDb(const DoReconnect: Boolean = False);
begin
  if Assigned(FDb) then
  begin
    If FDB.Connected then
    begin
      If DoReconnect then FDB.Close
      else Exit;
    end;
  end
  else
  begin
    FDb := TpFIBDatabase.Create(nil);
    FReadTransaction   := TpFIBTransaction.Create(nil);
    FReadTransaction.TimeoutAction:=TARollback;
    FReadTransaction.TRParams.Clear;
    FReadTransaction.TRParams.Add('read');
    FReadTransaction.TRParams.Add('nowait');
    FReadTransaction.TRParams.Add('rec_version');
    FReadTransaction.TRParams.Add('read_committed');

    FUpdateTransaction := TpFIBTransaction.Create(nil);
    FUpdateTransaction.TimeoutAction:=TACommit;  //*22.01.20
    FUpdateTransaction.TRParams.Clear;  //*22.01.20
    FUpdateTransaction.TRParams.Add('write');  //*22.01.20
    FUpdateTransaction.TRParams.Add('nowait');  //*22.01.20
    FUpdateTransaction.TRParams.Add('no_rec_version');  //*22.01.20
    FUpdateTransaction.TRParams.Add('read_committed');  //*22.01.20

    FDb.DefaultTransaction := FReadTransaction;
    FDb.DefaultUpdateTransaction := FUpdateTransaction;  //+22.01.20
    FReadTransaction.DefaultDatabase   := FDb;
    FUpdateTransaction.DefaultDatabase := FDb;
  end;

  FDb.DBName := FDbName;
  FDb.ConnectParams.UserName := FDbUser;
  FDb.ConnectParams.Password := FDbPassword;
  FDb.AfterConnect:=DbAfterConnect;
  FDb.AfterDisconnect:=DbAfterDisconnect;

  try
    FDb.Open;
    FDb.DefaultTransaction.StartTransaction;
  except
    raise;
  end;
end;

constructor TFibEngine.Create(ADbName: string; ADbUser: string; ADbPassword: string);
begin
  FDb := nil;
  SetDB(ADbName, ADbUser, ADbPassword);
  FErrMessages := TStringList.Create;
end;

constructor TFibEngine.Create(ADbName, ADbUser, ADbPassword: string;
  LogEngine: TLogEngine);
begin
  Create(ADbName, ADbUser, ADbPassword);
  FLogEngine := LogEngine;
end;

function TFibEngine.CreateReadQuery: TpFIBQuery;
begin
  If not FDB.Connected then ConnectDb;

  Result := TpFIBQuery.Create(nil);
  Result.Database := FDb;
  Result.Transaction := FReadTransaction;
end;

function TFibEngine.CreateWriteQuery: TpFIBQuery;
begin
  If not FDB.Connected then ConnectDb;

  Result := TpFIBQuery.Create(nil);
  Result.Database := FDb;
  Result.Transaction := FUpdateTransaction;
  If not FUpdateTransaction.Active then FUpdateTransaction.StartTransaction;  //+18.01.16
end;

destructor TFibEngine.Destroy;
begin
  If Assigned(FDb) then
    If FDB.Connected then FDb.Connected:=False;
  If Assigned(FReadTransaction) then FReadTransaction.Free;
  If Assigned(FUpdateTransaction) then FUpdateTransaction.Free;
  If Assigned(FDb) then FDb.Free;
  FErrMessages.Free;
end;

//в скрипте каждая операция должны быть отдельной строкой (в том числе commit)
function TFibEngine.ExecScript(Script:TStrings; StopOnErr: Boolean):Integer;
Var
  I:Integer;
  Query : TpFibQuery;
  IsDead: Boolean;
  ErrCnt: Integer;
  Params: TStringList;
  J: Integer;

  procedure DoOnError(E: EFibError; LineInd: Integer);
  begin
    Inc(ErrCnt);
    If Query.Transaction.Active then Query.Transaction.Rollback;

    if (E.SQLCode=-913) or (Pos(Uppercase('Lock conflict on no wait transaction.'), UpperCase(E.Message)) > 0) then IsDead:=True;
    FErrMessages.Add(Format('ОШИБКА выполнения скрипта %s в строке %d. %s', [E.Message, LineInd, Query.SQL.Text]));
    WriteLog(FErrMessages[FErrMessages.Count - 1], 2);
  end;

begin
  Result:=errNone;
  IsDead:=False;
  ErrCnt:=0;
  If Script.Count = 0 then Exit;
  FErrMessages.Clear;

  try
    Query:=CreateWriteQuery;
    Params := TStringList.Create;  //+06.05.20
    try
      for i:=0 to Script.Count-1 do
      begin
        If (Pos('EXECUTE', AnsiUpperCase(Script[I])) > 0)
          or (Pos('INSERT', AnsiUpperCase(Script[I])) > 0)
          or (Pos('DELETE', AnsiUpperCase(Script[I])) > 0)
          or (Pos('UPDATE', AnsiUpperCase(Script[I])) > 0)
          or (Pos('SELECT', AnsiUpperCase(Script[I])) > 0)
          or (Pos('COMMIT', AnsiUpperCase(Script[I])) > 0)
          then
        try
          If not Query.Transaction.Active then Query.Transaction.StartTransaction;

          If (Pos('COMMIT', AnsiUpperCase(Script[I])) <> 0) then Query.Transaction.Commit
          else
          begin
            Query.Close;
            Query.SQL.Text:=Script[I];

            //+06.05.20
            For J:=0 to Query.ParamCount-1 do
            begin
              if Params.IndexOfName(Query.Params[J].Name) > -1 then
                Query.Params[J].AsString := Params.Values[Query.Params[J].Name];
            end;
            //end +06.05.20

            Query.ExecQuery;

            //+06.05.20
            For J:=0 to Query.FieldCount-1 do
            begin
              if Params.IndexOfName(Query.Fields[J].Name) > -1 then
                Params.Values[Query.Fields[J].Name] := Query.Fields[J].AsString
              else
                Params.Add(Format('%s=%s', [Query.Fields[J].Name, Query.Fields[J].AsString]));
            end;
            //end +06.05.20
          end
        except
          On e:EFIBError do
          begin
            DoOnError(E, I);
            If StopOnErr then raise;
          end;
        end;
      end;
      If Query.Transaction.Active then
      try
        Query.Transaction.Commit;
      except
        On e:EFIBError do
        begin
          DoOnError(E, -1);
          If StopOnErr then raise;
        end;
    end;
    finally
      Params.Free;
      Query.Free;
    end;
  except
///    on E:Exception do
///      If not IsDead then SendMailMessage2('', Application.Title + '. Ошибка выполнения скрипта', Format(' ОШИБКА. %s %s', [E.Message, Query.SQL.Text]), nil, True, True);
  end;

  If ErrCnt > 0 then
  begin
    If IsDead then Result:=errDeadlock
    else Result:=errOther;
///    If (not IsDead) and (not StopOnErr) then SendMailMessage2('', Application.Title + '. Ошибка выполнения скрипта', Format(' ОШИБОК: %d', [ErrCnt]), nil, True, True);
  end;
end;

function TFibEngine.ExecScript(FName: String; StopOnErr: Boolean): Integer;
Var
  Script: TStrings;
begin
  If not FileExists(FName) then Exit;

  Script:=TStringList.Create;
  try
    Script.LoadFromFile(FName);
    Result:=ExecScript(Script, StopOnErr);
  finally
    Script.Free;
  end;
end;

procedure TFibEngine.DbAfterConnect(Sender: TObject);
begin
  WriteLog(Format('Подключение к базе %s / %s', [FDB.DBName, FDb.ConnectParams.UserName]), 1);
end;

procedure TFibEngine.DbAfterDisconnect(Sender: TObject);
begin
  Writelog('Отключение от базы', 1);
  WriteLog('---------------', 1);
end;

procedure TFibEngine.Disconnect;
begin
  If Assigned(FDb) then
    If FDB.Connected then FDb.Connected:=False;
end;

function TFibEngine.GenSPParams(SPName: String; const Prefix: String = ''):String;  //входящие параметры для процедуры через запятую
Var
  I: Integer;
  Query: TpFibQuery;
  Found: Boolean;
begin
  Result:='';
  Query := CreateReadQuery;
  try
    Query.SQL.Text:=Format(sqlSPInParams, [AnsiUpperCase(SPName)]);
    Query.ExecQuery;
    While not Query.Eof do
    try
      If Result = '' then Result := Prefix + Trim(Query.FieldByName('rdb$parameter_name').AsString)
      else Result := Result + Format(',%s%s', [Prefix, Trim(Query.FieldByName('rdb$parameter_name').AsString)]);
    finally
      Query.Next;
    end;
  finally
    Query.Free;  //+22.01.20
  end;
end;

function TFibEngine.GenSPParamsMapValues(SPName: String; FldValues: TStrings):String;  //на основе списка типа Fld=Val формирует значения входящих параметров для процедуры по именам полей
Var
  I: Integer;
  Query: TpFibQuery;
  Found: Boolean;
  AddSymb: String;
  List: TStringList;
begin
  Result:='';
  Query := CreateReadQuery;
  try
    Query.SQL.Text:=Format(sqlSPInParams, [AnsiUpperCase(SPName)]);
    Query.ExecQuery;
    List := TStringList.Create;
    try
      List.Delimiter := ',';

      While not Query.Eof do
      try
        Found := False;
        If FldValues <> nil then
        begin
          For I:=0 to FldValues.Count-1 do
          begin
            AddSymb:= '';
            If AnsiSameText(Trim(Query.FieldByname('rdb$parameter_name').AsString), FldValues.Names[I]) then
            begin
              Found := True;
              case Query.FieldByname('rdb$field_type').AsInteger of
      //          blr_short, blr_long, blr_quad, blr_int64: AddSymb:= '';
      //          blr_float, blr_double, blr_d_float:
                blr_timestamp, blr_sql_date, blr_sql_time: AddSymb:= '''';
                blr_varying, blr_varying2, blr_blob, blr_cstring, blr_cstring2, blr_text, blr_text2: AddSymb:= '''';
              end;

              If FldValues.Values[FldValues.Names[I]] = '' then List.Add('null')  //+19.04.19
              else List.Add(Format('%s%s%s', [AddSymb, FldValues.Values[FldValues.Names[I]], AddSymb]));

              Break;
            end;
          end;
        end;
        If not Found then
          List.Add('Null');
      finally
        Query.Next;
      end;

      Result := List.DelimitedText;
    finally
      List.free;
    end;
  finally
    Query.Free;
  end;
end;

function TFibEngine.IsEmptyRecord(Query: TpFibQuery): Boolean;  //+28.05.19
Var
  I: Integer;
begin
  Result := True;
  For I:=0 to Query.FieldCount-1 do
  begin
    If not Query.Fields[I].IsNull then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TFibEngine.WriteLog(Mess: String; const Indent: Integer = 1; const WithTime: boolean = True);  //+29.05.19
begin
  If (FLogEngine <> nil) then FLogEngine.WriteLog(Mess, Indent, WithTime);
end;

procedure TFibEngine.QuerySetParams(Query: TpFibQuery; FldValues: TStringList);
Var
  I: Integer;
  ParamInd: INteger;
begin
  For I := 0 to FldValues.Count - 1 do
    If Query.ParamExist(FldValues.Names[I], ParamInd) then
      Query.ParamByName(FldValues.Names[I]).AsString := FldValues.Values[FldValues.Names[I]];
end;

function TFibEngine.QueryValueAsStr(const aSQL: string; FieldNo: Integer): String;
begin
  Result := FDb.QueryValueAsStr(aSQL, FieldNo);
end;

function TFibEngine.GetSQLParams(SQL: String): String;
Var
  Query: TpFibQuery;
  I: Integer;
  ParamInd: INteger;
  List: TStringList;
begin
  Result := '';
  Query := CreateReadQuery;
  List := TStringList.Create;
  try
    Query.SQL.Text := SQL;
    List.Delimiter := ',';
    For I := 0 to Query.ParamCount - 1 do
      List.Add(Query.ParamName(I));
    Result := List.DelimitedText;
  finally
    Query.Free;
    List.free;
  end;
end;

end.
