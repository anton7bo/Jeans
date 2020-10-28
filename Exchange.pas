unit Exchange;

interface
Uses
  BaseRTTI,
  Classes,
  Contnrs,
  IniFiles;

type

{$M+}
//  TExchangeType = (etImport, etExport);

  TExchange = class(TBaseRTTI)  //содержит информацию по обмену данными с какой-либо внешней системой, загружается из ini-файла
  private
//    FName     :String;  //название обмена - уже есть в базовом классе
    FTimes    :String;  //в какое время запускать
    FPeriod   :Integer;  //с какой периодичностью запускать
    FDateFrom :String;  //параметр sql - выборка данных с
    FDateTo   :String;  //параметр sql - выборка данных по
    FLinkedExchanges :String;  //сразу после этого обмена выполнить следующие обмены (имена)
    FScheme   :String;  //см. SQLScheme (имена схем через разделить)
//    FProc     :Pointer;  //для выполнения обмена выполнить процедуру по указателю
    FDoTransfer: Boolean;  //признак что обмен необходимо выполнить
    FInPartialExchange: integer;  //1 - входит в частичный обмен
    FExchangeType   : Integer;  //1 - импорт, 2 - экспорт данных, 3 - прочее, -1 - набор импортов, -2 - набор экспортов 
    function ParseDateParam(Param: String): String;
  public
    constructor Load(Name: String; Ini: TIniFile);
  published
    property Times    :String read FTimes write FTimes;
    property Period   :Integer read FPeriod write FPeriod;
    property DateFrom :String read FDateFrom write FDateFrom;
    property DateTo   :String read FDateTo write FDateTo;
    property LinkedExchanges :String read FLinkedExchanges write FLinkedExchanges;
    property Scheme   :String read FScheme write FScheme;
//    property Proc     :Pointer read FProc;
    property DoTranfer: Boolean read FDoTransfer write FDoTransfer;
    property InPartialExchange: integer read FInPartialExchange write FInPartialExchange;
    property ExchangeType: Integer read FExchangeType write FExchangeType;
  end;

  TExchangeList = class (TObjectList)  //список обменов
  protected
    procedure Put(Index: Integer; Item: TExchange);
    function Get(Index: Integer): TExchange;
  public
    property Items[Index: Integer]: TExchange read Get write Put; default;
    function Add(Obj: TExchange): Integer;
    procedure Insert(Index: Integer; Obj: TExchange);
    procedure LoadFromIni(IniFileName: String);
    function IndexOf(Name: String): TExchange;
    procedure SetNeedTransfer(MinutesLeft: Integer);  //по текущему времени и MinutesLeft ставит признак на обмены которые необходимо выполнить 
  end;

implementation
Uses
  DateUtils,
  SysUtils,
  StrUtils;

{ TExchangeClass }

constructor TExchange.Load(Name: String; Ini: TIniFile);
begin
  inherited Create(Name);
  FDoTransfer := False;
  LoadPropertiesFromIni(Ini);

  FDateFrom := ParseDateParam(FDateFrom);
  FDateTo := ParseDateParam(FDateTo);
end;

function TExchange.ParseDateParam(Param: String): String;
Var
  I: Integer;
begin
  Param := Trim(Param);
  If Param = '' then Exit;

  If UpperCase(Param) = 'NOW' then Result := DateToStr(Date)
  else
  If UpperCase(Param) = 'STARTOFTHEWEEK' then Result := DateToStr(StartOfTheWeek(Date))  //+28.05.20
  else
  try
    I := StrToInt(Param);
    Result := DateToStr(Date + I);
  except
    try
      StrToDate(Param);
      Result := Param;
    except
    end;
  end;
end;

{ TExchangeList }

function TExchangeList.Add(Obj: TExchange): Integer;
begin
  Result := inherited Add(Obj);
end;

function TExchangeList.Get(Index: Integer): TExchange;
begin
  if Count-1 >= Index then Result := TExchange(inherited Items[Index])
  else Result := nil;
end;

function TExchangeList.IndexOf(Name: String): TExchange;
Var
  I: Integer;
begin
  Result := nil;
  For I := 0 to Count - 1 do
    if AnsiSameText(Name, Items[I].Name) then
    begin
      Result := Items[I];
      Break;
    end;
end;

procedure TExchangeList.Insert(Index: Integer; Obj: TExchange);
begin
  inherited Insert(Index, Obj);
end;

procedure TExchangeList.LoadFromIni(IniFileName: String);
Var
  Ini: TIniFile;
  I: Integer;
  SectionList: TStringList;
  Exchange: TExchange;
begin
  If not FileExists(IniFileName) then raise Exception.CreateFmt('Не найден файл с настройками обменов %s', [IniFileName]);

  Ini := TIniFile.Create(IniFileName);
  SectionList := TStringList.Create;
  try
    Ini.ReadSections(SectionList);
    For I:=SectionList.Count-1 downto 0 do
      If SectionList.Count > I then
        If ContainsText(SectionList[I], sScheme) then SectionList.Delete(I);

    For I:=0 to SectionList.Count-1 do
    begin
      Exchange := TExchange.Load(SectionList[I], Ini);
      Add(Exchange);
    end;
//ПРОВЕРКА НА ЗАПОЛНЕННОСТЬ НЕОБХОДИМЫХ ПОЛЕЙ!!!
  finally
    SectionList.Free;
    Ini.Free;
  end;
end;

procedure TExchangeList.Put(Index: Integer; Item: TExchange);
begin
  inherited Items[Index] := Item;
end;

procedure TExchangeList.SetNeedTransfer(MinutesLeft: Integer);
Var
  I: Integer;
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Time, Hour, Min, Sec, MSec);

  For I:=0 to Self.Count-1 do
  begin
    if (AnsiContainsText(Self[I].Times, Format('%s:%s', [FormatFloat('00', Hour), FormatFloat('00', Min)]))) then Self[I].DoTranfer:=True
    else
    if (Self[I].Period > 0) and (MinutesLeft >= Self[I].Period) and ((MinutesLeft mod Self[I].Period) = 0) then Self[I].DoTranfer:=True;  //*22.01.20
  end;
end;

end.
