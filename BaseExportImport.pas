unit BaseExportImport;
//*22.01.20 ImportOrdersToOCO


interface
Uses
  Classes,
  Config,
  FibEngine,
  LogEngine,
  Mailer,
  SysUtils;

type
  TCharSet = Set of Char;  //from rxStrUtils

  //базовый класс для хранения основных настроек FTP, работы с БД
  TBaseExportImport = class
  private
    FLogEngine: TLogEngine;
    FDoFreeConfig: boolean;
    FDoFreeMailer: boolean;
    procedure Init(LogEngine: TLogEngine; Mailer: TSmartMailer);
  protected
    FConfig: TConfig;
    FFibEngine: TFibEngine;
    FMailer: TSmartMailer;

    function WordCount(const S: string; const WordDelims: TCharSet): Integer;
    function ExtractWord(Num:integer;const Str: string;const  WordDelims:TCharSet):string;

    procedure MoveFileToDone(FileName: String; NoErr: Boolean; const DoCopy: Boolean = False);
    procedure WriteLog(Mess: String; const Indent: Integer = 1; const WithTime: boolean = True);

    function TryGetTransformFile(XmlFileName: String): TFileName; virtual;
    procedure FilesToScripts(DoMoveFile: Boolean = True);  //*05.11.19
    procedure ExecScripts;
    procedure ImportOrdersToOCO;
  public
    constructor Create(IniFileName: String; LogEngine: TLogEngine; Mailer: TSmartMailer); overload;
    constructor Create(Config: TConfig; LogEngine: TLogEngine; Mailer: TSmartMailer); overload;
    destructor Destroy; override;
  end;

const
  cDelims = [',', ';'];
  sqlSelect: String = 'select distinct * from %s';
  sqlImportOrders = 'select * from %s(%d)';

implementation
Uses
  FileUtil,
  pFibQuery,
  XMLTransform;

//from rxStrUtils
function TBaseExportImport.WordCount(const S: string; const WordDelims: TCharSet): Integer;
var
  SLen, I: Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);
  while I <= SLen do
  begin
    while (I <= SLen) and (S[I] in WordDelims) do Inc(I);
    if I <= SLen then Inc(Result);
    while (I <= SLen) and not(S[I] in WordDelims) do Inc(I);
  end;
end;

function TBaseExportImport.ExtractWord(Num:integer;const Str: string;const  WordDelims:TCharSet):string;
var
  SLen, I: Cardinal;
  wc: Integer;
begin
  Result := '';
  I := 1; wc:=0;
  SLen := Length(Str);
  while I <= SLen do
  begin
    while (I <= SLen) and (Str[I] in WordDelims) do Inc(I);
    if I <= SLen then Inc(wc);
    if wc=Num then Break;
    while (I <= SLen) and not(Str[I] in WordDelims) do Inc(I);
  end;
  if (wc=0) and (Num=1) then
   Result:=Str
  else
  if wc<>0 then
  begin
     while (I <= SLen) and not (Str[I] in WordDelims) do
     begin
       Result:=Result+Str[I];
       Inc(I);
     end;
  end;
end;
//end from rxStrUtils

constructor TBaseExportImport.Create(IniFileName: String; LogEngine: TLogEngine; Mailer: TSmartMailer);
begin
  FConfig := TConfig.Create(IniFileName);
  FDoFreeConfig := True;
  Init(LogEngine, Mailer);
end;

constructor TBaseExportImport.Create(Config: TConfig; LogEngine: TLogEngine; Mailer: TSmartMailer);
begin
  If Config = nil then exception.Create('Не задан класс TConfig при создании экпорта/импорта');
  FConfig := Config;
  FDoFreeConfig := False;
  Init(LogEngine, Mailer);
end;

procedure TBaseExportImport.Init(LogEngine: TLogEngine; Mailer: TSmartMailer);
begin
  FFibEngine := TFibEngine.Create(FConfig.DBName, FConfig.DBUser, FConfig.DBPass, LogEngine);
  FLogEngine := LogEngine;
  FDoFReeMailer := False;
  If Mailer <> nil then FMailer := Mailer
  else
  begin
    Mailer := TSmartMailer.Create('', FLogEngine, FConfig.LogLevel);
    FDoFReeMailer := True;
  end;
  WriteLog(Format('Параметры загружены %s (LogLevel = %d / схемы в %s)', [FConfig.IniFileName, FConfig.LogLevel, FConfig.TransfersIni]));
end;

destructor TBaseExportImport.Destroy;
begin
  If FDoFreeConfig then FConfig.Free;
  If FDoFreeMailer then FMailer.Free;
  FFibEngine.Free;
  
  inherited;  //+27.10.20
end;

procedure TBaseExportImport.MoveFileToDone(FileName: String; NoErr: Boolean; const DoCopy: Boolean = False);
Var
  Year, Month, Day, Hour, Minute, Sec, MSec: Word;
  NewDir: String;
  ErrPostfix: String;
begin
  try
    If not FileExists(FileName) then Exit;

    DecodeDate(Now, Year, Month, Day);
    DecodeTime(Now, Hour, Minute, Sec, MSec);

    if NoErr then ErrPostfix := ''
    else ErrPostfix := '-Err';

    NewDir:= ExtractFilePath(FileName);
    If not DirectoryExists(NewDir) then CreateDir(NewDir);
    NewDir:=Format('%s%s-%s%s\', [NewDir, FormatFloat('0000', Year), FormatFloat('00', Month), ErrPostfix]);
    If not DirectoryExists(NewDir) then CreateDir(NewDir);
    NewDir:=Format('%s%s\', [NewDir, FormatFloat('00', Day)]);
    If not DirectoryExists(NewDir) then CreateDir(NewDir);

    If DoCopy then FileUtil.CopyFile(FileName, Format('%s%s-%s-%s-%s', [NewDir, FormatFloat('00', Hour), FormatFloat('00', Minute), FormatFloat('00', Sec), ExtractFileName(FileName)]), nil)
    else FileUtil.MoveFile(FileName, Format('%s%s-%s-%s-%s', [NewDir, FormatFloat('00', Hour), FormatFloat('00', Minute), FormatFloat('00', Sec), ExtractFileName(FileName)]));
  except
    on E:Exception do
      WriteLog(Format('ОШИБКА в методе %s.MoveFileToDone(%s). %s', [Self.ClassName, FileName, E.Message]), 1);
  end;
end;

function TBaseExportImport.TryGetTransformFile(XmlFileName: String): TFileName;
Var
  I: Integer;
  TheName: String;
begin
  TheName := StringReplace(ExtractFileName(XmlFileName), ExtractFileExt(XmlFileName), '', []);

  Result := Format('%s%s.xsl', [FConfig.Path, TheName]);
  If FileExists(Result) then Exit;

  for I:=Length(TheName) downto 1 do
  begin
    If TheName[I] in ['0'..'9'] then Delete(TheName, I, 1);
    Result := Format('%s%s.xsl', [FConfig.Path, TheName]);
    If FileExists(Result) then Exit;
  end;
  Result := '';
end;

procedure TBaseExportImport.WriteLog(Mess: String; const Indent: Integer = 1; const WithTime: boolean = True);
begin
  If (FConfig.LogLevel > 0) and (FLogEngine <> nil) then FLogEngine.WriteLog(Mess, Indent, WithTime);
end;

procedure TBaseExportImport.FilesToScripts(DoMoveFile: Boolean = True);  //*05.11.19
Var
  sr: TSearchRec;
  FName: String;
  TransformFileName: String;
  XMLTrans: TXMLTransform;
begin
  try
    WriteLog(Format('Обработка входящих файлов в папке %s', [FConfig.ImportFolder]), 1);
    //геренарция скриптов из файлов, сохранение их
    if FindFirst(Format('%s\*.xml', [FConfig.ImportFolder]), faAnyFile, sr) = 0 then
    try
      repeat
        if sr.Attr in [faDirectory] then Continue;

        FName:=Format('%s%s', [FConfig.ImportFolder, sr.Name]);
        TransformFileName := TryGetTransformFile(sr.Name);  //если xml файл прилетел с доп. цифрами (для уникальности), то пытаюсь найти файл трансформации не по точному совпадению

        if TransformFileName > '' then  //FileExists(TransformFileName) если есть файл трансформации (xsl), то продолжаем с этим обменом. Если файла нет, то этот обмен игнорируем (в будущем можно просто подкладывать xsl файлы)
        begin
          XMLTrans := TXMLTransform.Create(FName, TransformFileName);
          try
            //файл скрипта сохраняется сразу в TXMLTransform.Create если он не пустой
            If XMLTRans.ResFileName > '' then WriteLog(Format('Создан скрипт %s', [XMLTRans.ResFileName]), 2);
          finally
            XMLTrans.Free;
          end;
        end
        else WriteLog(Format('Файл %s не обработан. Нет файла трансформации %s', [sr.Name, ChangeFileExt(sr.Name, '.xsl')]), 2);
        If DoMoveFile then MoveFileToDone(FName, True);  //*05.11.19
      until FindNext(sr) <> 0;
    finally
      SysUtils.FindClose(sr);
    end
    else WriteLog('.. нет файлов', 2);
  except
    on E:Exception do
    begin
      WriteLog(Format('ОШИБКА в методе %s.XMLToScripts. %s', [Self.ClassName, E.Message]), 2);
      FMailer.SendErrMessage(rctAdmin, Format('ОШИБКА в методе %s.XMLToScripts', [Self.ClassName]), E.Message, nil);
    end;
  end;
end;

procedure TBaseExportImport.ExecScripts;
Var
  sr: TSearchRec;
  Res: Integer;
  FName: String;
begin
  //выполнение скриптов из ранее сохраненных
  try
    WriteLog('Выполнение скриптов из входящей папки', 1);
    try
      if FindFirst(Format('%s\*.sql', [FConfig.ImportFolder]), faAnyFile, sr) = 0 then
      begin
        FFibEngine.ConnectDb(False);
        try
          repeat
            if sr.Attr in [faDirectory] then Continue;

            FName:=Format('%s%s', [FConfig.ImportFolder, sr.Name]);

            Res := FFibEngine.ExecScript(FName, True);

            If Res = errNone then WriteLog(Format('Скрипт выполнен успешно %s', [sr.Name]), 2)
            else
            begin
              If Res = errOther then WriteLog(Format('Скрипт выполнен c ошибками %s', [sr.Name]), 2);
              If FFibEngine.ErrMessages.Count > 0 then FMailer.SendErrMessage(rctAdmin, 'Ошибка выполнения скрипта', FFibEngine.ErrMessages.DelimitedText, nil);
            end;

            MoveFileToDone(FName, Res = errNone);
          until FindNext(sr) <> 0;
        finally
          FFibEngine.Disconnect;
        end;
      end
      else WriteLog('.. нет файлов', 2);
    finally
      SysUtils.FindClose(sr);
    end;
  except
    on E:Exception do
    begin
      WriteLog(Format('ОШИБКА в методе %s.ExecScripts. %s', [Self.ClassName, E.Message]), 2);
      FMailer.SendErrMessage(rctAdmin, Format('ОШИБКА в методе %s.ExecScripts', [Self.ClassName]), E.Message, nil);
    end;
  end;
end;

//SQL запрос должен содержать на выходе id/no заказа в системе поставщика и oco_guid. если импорт не прошел то oco_guid = -1 или null. также желательно третье поле с текстом ошибки
procedure TBaseExportImport.ImportOrdersToOCO;  //*22.01.20
Var
  Query: TpFibQuery;
  Mess: String;
  Cnt: Integer;
  WarnMess: String;
  ErrMess: String;
begin
  try
    WriteLog(Format('Импорт заявок в oco$* (SP = %s, DaysAgo = %d)', [FConfig.ImportToOCOStoredProc, FConfig.ImportOrdersDaysAgo]), 1);

    if FConfig.ImportToOCOStoredProc = '' then Exit;

    FFibEngine.ConnectDb(False);
    try
      Query := FFibEngine.CreateWriteQuery;
      try
        Query.SQL.Text := Format(sqlImportOrders, [FConfig.ImportToOCOStoredProc, FConfig.ImportOrdersDaysAgo]);
        Query.ExecQuery;

        Cnt := 0;
        While not Query.Eof do
        try
          if Query.FieldsCount > 2 then ErrMess := Query.Fields[2].AsString
          else ErrMess := '';
          If (Query.Fields[1].Value <= 0) or (Query.Fields[1].IsNull) then  //OCO_GUID
          begin
            If Mess = '' then Mess:=Format('ОШИБКА импорта заявок: %s (%s)', [Query.Fields[0].AsString, ErrMess])  //ORDERID / ORDERNO
            else Mess:=Format('%s, %s (%s)', [Mess, Query.Fields[0].AsString, ErrMess]);
          end
          else
          If ErrMess > '' then
          begin
            If WarnMess = '' then WarnMess := Format('Предупреждения. %s' , [ErrMess])
            else WarnMess := Format('%s, %s', [WarnMess, ErrMess]);
          end;
          Inc(Cnt);
        finally
          Query.Next;
        end;

        If Query.Transaction.Active then Query.Transaction.Commit;
        WriteLog(Format('Обработано заявок: %d', [Cnt]), 2);

        If Mess > '' then WriteLog(Mess, 2);
        If WarnMess > '' then WriteLog(WarnMess, 2);
        If (Mess > '') or (WarnMess > '') then
          FMailer.SendErrMessage(rctAll, 'Ошибки импорта заявок в нашу учетную систему', Mess + #13#10 + WarnMess, nil);
      finally
        Query.Free;
      end;
    finally
      FFibEngine.Disconnect;
    end;
  except
    on E:Exception do
    begin
      WriteLog(Format('ОШИБКА в методе %s.ImportOrdersToOCO. %s', [Self.ClassName, E.Message]), 2);
      FMailer.SendErrMessage(rctAdmin, Format('ОШИБКА в методе %s.ImportOrdersToOCO', [Self.ClassName]), E.Message, nil);
    end;
  end;
end;

end.
