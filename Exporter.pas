unit Exporter;

interface
Uses
  JnsBaseExportImport,
  Classes,
  Config,
  DRPService,
  DRPService5,
  Exchange,
  Mailer,
  MSXML2_TLB,
  LogEngine,
  pFibQuery,
  SQLScheme,
  SysUtils;

type
  TExporter = class(TJnsBaseExportImport)
  private
    procedure HTTPRIOBeforeExecute(const MethodName: string; var SOAPRequest: WideString);
    procedure HTTPRIOAfterExecute(const MethodName: string; SOAPResponse: TStream);

    function CreateOrdersStatuses(Query: TpFibQuery): DRPService.ArrayOfSendStatuses;
    function CreateDeliveryPositions(Query: TpFibQuery): DRPService.ArrayOfDeliveryPosition;
    function CreateInvoices(Query: TpFibQuery; DetailSchemeNames: String): DRPService.ArrayOfInvoice;
    function CreateWarehousesBalance(Query: TpFibQuery): DRPService.ArrayOfBalance;
    function CreateAllContragentDebet(Query:TpFibQuery): DRPService.ArrayOfDistributorDebet;
    function CreateWarehouseBalanceKeg(Query:TpFibQuery): DRPService5.ArrayOfWarehouseBalanceKeg;
    function CreateSalePointBalanceKeg(Query:TpFibQuery): DRPService5.ArrayOfSalePointBalanceKeg;

    function SendOrdersStatuses(Query: TpFibQuery): TFileName;
    function SendDocuments(Query: TpFibQuery; Exchange: TExchange; Scheme: TSQLScheme): TFileName;
    function SendWarehousesBalance(Query: TpFibQuery): TFileName;
    function SendAllContragentDebet(Query:TpFibQuery): TFileName;
    function SendWarehouseBalanceKeg(Query:TpFibQuery): TFileName;
    function SendSalePointBalanceKeg(Query:TpFibQuery): TFileName;
  protected
    procedure Init; virtual;  //!!!*07.08.20
    procedure DoExportScheme(Exchange: TExchange; Scheme: TSQLScheme; MasterFldValues: TStringList); virtual;  //!!!*07.08.20
    function QuerySetSQL(Query: TpFibQuery; Scheme: TSQLScheme; ParamValues: TStringList): String;
  public
    constructor Create(IniFileName: String; LogEngine: TLogEngine; Mailer: TSmartMailer); overload;
    constructor Create(Config: TConfig; LogEngine: TLogEngine; Mailer: TSmartMailer); overload;
    procedure DoExport(Exchange: TExchange; const Level: Integer = 0); overload;
    procedure DoExport(ExchangeName: String); overload;
    procedure MultyExport(IsFull: boolean);
  end;

implementation

Uses
  BaseExportImport,
  StrUtils,
  TypInfo,
  XSBuiltins,
  Windows;

function ConvertDateBack(DateTime: TDateTime; const WithTime: Boolean = False):String;
Var
 Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Min, Sec, MSec);
  if WithTime then Result:=Format('%s-%s-%s%s%s%s', [FormatFloat('0000', Year), FormatFloat('00', Month), FormatFloat('00', Day), FormatFloat('00', Hour), FormatFloat('00', Min), FormatFloat('00', Sec)])
  else Result:=Format('%s-%s-%s', [FormatFloat('0000', Year), FormatFloat('00', Month), FormatFloat('00', Day)]);
end;

//------------------------------------------------------------------------------

constructor TExporter.Create(IniFileName: String; LogEngine: TLogEngine; Mailer: TSmartMailer);
begin
  inherited;
  Init;
end;

constructor TExporter.Create(Config: TConfig; LogEngine: TLogEngine; Mailer: TSmartMailer);
begin
  inherited;
  Init;
end;

procedure TExporter.Init;
begin
  FHttpRio.OnBeforeExecute := HTTPRIOBeforeExecute;
  FHttpRio.OnAfterExecute := HTTPRIOAfterExecute;
  FHttpRio5.OnBeforeExecute := HTTPRIOBeforeExecute;
  FHttpRio5.OnAfterExecute := HTTPRIOAfterExecute;
end;

function TExporter.QuerySetSQL(Query: TpFibQuery; Scheme: TSQLScheme; ParamValues: TStringList): String;
Var
  Params: String;
begin
  If Scheme.SQL > '' then Result := Scheme.SQL
  else If Scheme.StoredProc > '' then
  begin
    Result := Format(sqlSelect, [Scheme.StoredProc]);
    Params := FFibEngine.GenSPParams(Scheme.StoredProc, ':');
    If Params > '' then Result := Format('%s (%s)', [Result, Params]);
  end
  else raise Exception.CreateFmt('Не задан SQL или StoredProc в ини-файле. Не знаю откуда брать данные', []);

  If Assigned(Query) then
  begin
    Query.Close;
    Query.SQL.Text := Result;
    FFibEngine.QuerySetParams(Query, ParamValues);
  end;

  If FConfig.LogLevel > 1 then WriteLog(Format('SQL = %s / %s', [Result, ParamValues.DelimitedText]), 2);  //Query.Params
end;

procedure TExporter.DoExport(ExchangeName: String);
Var
  Exchange: TExchange;
begin
  try
    Exchange := FExchangeList.IndexOf(ExchangeName);
    If Exchange = nil then raise Exception.CreateFmt('Не найден обмен %s в ини-файле', [ExchangeName]);
    DoExport(Exchange);
  except
    on E: Exception do
    begin
      WriteLog(Format('ОШИБКА в методе %s.DoExport. %s', [Self.ClassName, E.Message]), 1);
      FMailer.SendErrMessage(rctAdmin, Format('ОШИБКА в методе %s.DoExport', [Self.ClassName]), E.Message, nil);
    end;
  end;
end;

procedure TExporter.DoExport(Exchange: TExchange; const Level: Integer = 0);
Var
  I, J: Integer;
  OneSchemeName: String;
  Scheme: TSQLScheme;
  SQLParams: String;
  ParamsList: TStringList;
  ParamName: String;
  LinkedExchangeName: String;
  LinkedExchange: TExchange;
begin
  try
    If Exchange = nil then raise Exception.Create('Не указан обмен (TExchange)');

    WriteLog(Format('Обмен %s (%s - %s)', [Exchange.Name, Exchange.DateFrom, Exchange.DateTo]), 1 + Level);

    ParamsList := TStringList.Create;
    try
      For I := 1 to WordCount(Exchange.Scheme, cDelims) do
      begin
        OneSchemeName := ExtractWord(I, Exchange.Scheme, cDelims);
        Scheme := FSchemeList.IndexOf(OneSchemeName);
        If Scheme = nil then raise Exception.CreateFmt('Не найдена схема экспорта %s по имени в ини-файле', [OneSchemeName]);

        ParamsList.Clear;
        If Scheme.SQL > '' then
          SQLParams := FFibEngine.GetSQLParams(Scheme.SQL)
        else
          If Scheme.StoredProc > '' then
            SQLParams := FFibEngine.GenSPParams(Scheme.StoredProc);
        For J := 1 to WordCount(SQLParams, cDelims) do
        begin
          ParamName := ExtractWord(J, SQLParams, cDelims);
          if MatchText(ParamName, ['DATE_FROM', 'DAY_FROM', 'DF', 'FROM_DATE', 'FROM_DAY']) then ParamsList.Add(Format('%s=%s', [ParamName, Exchange.DateFrom]))
          else If MatchText(ParamName, ['DATE_TO', 'DAY_TO', 'DT', 'TO_DATE', 'TO_DAY']) then ParamsList.Add(Format('%s=%s', [ParamName, Exchange.DateTo]))
        end;

        DoExportScheme(Exchange, Scheme, ParamsList);
      end;

      //обработаем LinkedExchanges 
      For I := 1 to WordCount(Exchange.LinkedExchanges, cDelims) do
      begin
        LinkedExchangeName := ExtractWord(I, Exchange.LinkedExchanges, cDelims);
        If AnsiSameText(Exchange.Name, LinkedExchangeName) then Continue;
        LinkedExchange := FExchangeList.IndexOf(LinkedExchangeName);
        If LinkedExchange = nil then raise Exception.CreateFmt('Не найден LinkedExchange обмен %s в обмене %s по имени в ини-файле', [LinkedExchangeName, Exchange.Name]);

        //передадим в LinkedExchange даты базового обмена (только если базовый обмен не содержит своей схемы обмена, т.е. обмен создан искусственно только для объединения группы других обменов)
        If Exchange.ExchangeType = -2 then  //If Exchange.Scheme = '' then
        begin
          LinkedExchange.DateFrom := Exchange.DateFrom;
          LinkedExchange.DateTo := Exchange.DateTo;
        end;

        DoExport(LinkedExchange, Level+1);
      end;

      Exchange.DoTranfer := False;
    finally
      ParamsList.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLog(Format('ОШИБКА в методе %s.DoExport. %s', [Self.ClassName, E.Message]), 2 + Level);
      FMailer.SendErrMessage(rctAdmin, Format('ОШИБКА в методе %s.DoExport', [Self.ClassName]), E.Message, nil);
    end;
  end;
end;

procedure TExporter.DoExportScheme(Exchange: TExchange; Scheme: TSQLScheme; MasterFldValues: TStringList);
Var
  Query: TpFibQuery;
  dc: Char;
begin
  try
    dc := DecimalSeparator;
    DecimalSeparator := '.';
    try
      Query := FFibEngine.CreateReadQuery;
      try
        QuerySetSQL(Query, Scheme, MasterFldValues);

        Query.ExecQuery;

        If FFibEngine.IsEmptyRecord(Query) then
        begin
          If FConfig.LogLevel > 1 then WriteLog('.. нет данных', 2);  //*22.10.19
          Exit;
        end;

//        If AnsiSameText(Exchange.Name, 'GetVersion') then ShowMessage(FDrp.GetVersion)
//        else
        If AnsiSameText(Exchange.Name, 'DRP_SendOrdersStatuses') then SendOrdersStatuses(Query)
        else
        If AnsiSameText(Exchange.Name, 'DRP_SendSellOut') or AnsiSameText(Exchange.Name, 'DRP_SendSellIn') or AnsiSameText(Exchange.Name, 'DRP_SendReturn') then SendDocuments(Query, Exchange, Scheme)
        else
        If AnsiSameText(Exchange.Name, 'DRP_SendWarehousesBalance') then SendWarehousesBalance(Query)
        else
        If AnsiSameText(Exchange.Name, 'DRP_SendAllContragentDebet') then SendAllContragentDebet(Query)
        else
        If AnsiSameText(Exchange.Name, 'SendWarehouseBalanceKeg') then SendWarehouseBalanceKeg(Query)
        else
        If AnsiSameText(Exchange.Name, 'SendSalepointBalanceKeg') then SendSalepointBalanceKeg(Query)
        else
        raise Exception.CreateFmt('Неизвестный обмен (метод) %s', [Exchange.Name]);
      finally
        Query.Free;
      end;
    finally
      decimalSeparator := dc;
    end;
  except
    on E: Exception do
    begin
      WriteLog(Format('ОШИБКА в методе %s.DoExportScheme. %s', [Self.ClassName, E.Message]), 1);
      FMailer.SendErrMessage(rctAdmin, Format('ОШИБКА в методе %s.DoExportScheme', [Self.ClassName]), E.Message, nil);
    end;
  end;
end;

procedure TExporter.HTTPRIOBeforeExecute(const MethodName: string;
  var SOAPRequest: WideString);
Var
  FName:String;
  Doc: DomDocument;
  r: IXMLDOMElement;
  Node: IXMLDOMNode;
  ParseError: IXMLDOMParseError;
const
  excAutoSend = 'Файл не отправлен. AutoSend = 0';
begin
  try
    Doc:=CoDOMDocument.Create;
    try
      Doc.Set_async(false);
//      Doc.loadXML(Utf8ToAnsi(SOAPRequest));
//      SOAPRequest := StringReplace(SOAPRequest, '<Number xmlns="http://microsoft.com/wsdl/types/">', '<Number>', [rfReplaceAll]);  //+13.05.20
      Doc.loadXML(SOAPRequest);
//      if Doc = nil then raise Exception.Create('Неверный формат отправляемого файла');
      ParseError := Doc.parseError;  //+13.05.20
      If (ParseError.errorCode <> 0) then raise exception.CreateFmt('Ошибка в отправляемом файле %s (позиция %d - %d). Метод %s.HTTPRIOBeforeExecute', [ParseError.reason, ParseError.Get_line, ParseError.Get_linepos, Self.ClassName]);  //+18.02.20

      r := Doc.Get_documentElement;
      if r = nil then raise exception.Create('Неверный формат отправляемого файла - нет корневого элемента');

      Node:=r.firstChild.firstChild;
      If Node <> nil then FName := Format('%s%s.xml', [FConfig.ExportFolder, Node.nodeName])
      else FName := Format('%s%s', [FConfig.ExportFolder, MakeFileName('', 'xml')]);

      If not DirectoryExists(ExtractFilePath(FName)) then CreateDir(ExtractFilePath(FName));
      Doc.save(FName);
      WriteLog(Format('Сохранен файл %s', [FName]), 2);
      MoveFileToDone(FName, True);
    finally
      Doc:=nil;
    end;
    If FConfig.AutoSend = 0 then raise Exception.Create(excAutoSend); //SOAPRequest := '';  //чтоб не отправлять файл делаем эксепшн
  except
    on E:Exception do
    begin
      if E.Message = excAutoSend then raise  //чтоб не отправлять файл делаем эксепшн
      else WriteLog(Format('ОШИБКА в методе %s.HTTPRIOBeforeExecute. %s', [Self.ClassName, E.Message]), 2);
    end;
  end;
end;

procedure TExporter.HTTPRIOAfterExecute(const MethodName: string;
  SOAPResponse: TStream);
begin
  WriteLog('Файл отправлен', 2)
end;

procedure TExporter.MultyExport(IsFull: boolean);
Var
  I: Integer;
  TypeSt: String;
begin
  try
    If IsFull then TypeSt := 'Полный'
    else TypeSt := 'Частичный';

    WriteLog('---------------', 1);
    WriteLog(Format('Начало экспорта. %s ...', [TypeSt]), 1);

    FFibEngine.ConnectDb;  //+20.08.19
    try
      For I := 0 to FExchangeList.Count - 1 do
      begin
        If (FExchangeList[I].ExchangeType = 2) and (IsFull or (FExchangeList[I].InPartialExchange = 1)) then
          DoExport(FExchangeList[I])
      end;

      WriteLog(Format('%s экспорт завершен', [TypeSt]), 1);

      CheckFiles;
    finally
      FFibEngine.Disconnect;
    end;
  except
    on E: exception do
    begin
      WriteLog(Format('ОШИБКА в методе %s.Export. %s', [Self.ClassName, E.Message]), 1);
      FMailer.SendErrMessage(rctAdmin, Format('ОШИБКА в методе %s.Export', [Self.ClassName]), E.Message, nil);
    end;
  end;
end;

function TExporter.CreateOrdersStatuses(Query: TpFibQuery): DRPService.ArrayOfSendStatuses;
Var
  I: Integer;
begin
  I := -1;
  While not Query.Eof do
  try
    Inc(I);
    SetLength(Result, I+1);
    Result[I] := DRPService.SendStatuses.Create;
    Result[I].Number := Query.FieldByName('Number').AsString;
    Result[I].Status := Statuses(GetEnumValue(TypeInfo(Statuses), Query.FieldByName('Status').AsString));
//    Result[I].Comments := AnsiToUtf8(Query.FieldByName('Comments').AsString);
    Result[I].Comments := Query.FieldByName('Comments').AsString;
  finally
    Query.Next;
  end;
end;

function TExporter.CreateDeliveryPositions(Query: TpFibQuery): DRPService.ArrayOfDeliveryPosition;
Var
  I: Integer;
begin
  I := -1;
  While not Query.Eof do
  try
    Inc(I);
    SetLength(Result, I+1);
    Result[I] := DRPService.DeliveryPosition.Create;
    Result[I].SkuID := Query.FieldByName('SkuID').AsString;
    Result[I].Volume := Query.FieldByName('Volume').AsString;
    Result[I].Count := Query.FieldByName('Count_').AsString;
    Result[I].Rubles := Query.FieldByName('Rubles').AsString;
    Result[I].Comments := Query.FieldByName('Comments').AsString;
    Result[I].HasPromo := Query.FieldByName('HasPromo').AsString;  //+20.03.20
  finally
    Query.Next;
  end;
end;

function TExporter.CreateInvoices(Query: TpFibQuery; DetailSchemeNames: String): DRPService.ArrayOfInvoice;
Var
  I, J: Integer;
  OrderDeliveriesArr: DRPService.ArrayOfOrderDelivery;
  DistrDeliveriesArr: DRPService.ArrayOfDistributorDelivery;
  DistrSPDeliveriesArr: DRPService.ArrayOfDistributorSalePointDelivery;
  PositionArr: DRPService.ArrayOfDeliveryPosition;
  QueryInt: TpFibQuery;
  FldValues: TstringList;
  DetailSchemeName: String;
  DetailScheme: TSQLScheme;
begin
  If DetailSchemeNames > '' then
  begin
    DetailSchemeName := ExtractWord(1, DetailSchemeNames, cDelims);
    DetailScheme := FSchemeList.IndexOf(DetailSchemeName);
    if DetailScheme = nil then raise Exception.CreateFmt('Не найдена схема экспорта %s по имени в ини-файле', [DetailSchemeName]);
  end
  else raise Exception.Create('Не указана схема экспорта содержимого документов в ини-файле');

  QueryInt := FFibEngine.CreateReadQuery;
  FldValues := TStringList.Create;
  try
    I := -1;
    While not Query.Eof do
    try
      Inc(I);
      SetLength(Result, I+1);

      Result[I] := DRPService.Invoice.Create;
      Result[I].InvoiceNum := Query.FieldByName('InvoiceNum').AsString;
      Result[I].InvoiceDate := ConvertDateBack(Query.FieldByName('InvoiceDate').AsDateTime);

      If AnsiSameText(Query.FieldByName('DocType').AsString, 'DRP_SendSellIn') then  //приходы
      begin
        DistrSPDeliveriesArr := nil;
        SetLength(DistrSPDeliveriesArr, 1);
        Result[I].DistributorSalePointDeliveries := DistrSPDeliveriesArr;
        DistrSPDeliveriesArr[0] := DRPService.DistributorSalePointDelivery.Create;
        DistrSPDeliveriesArr[0].DistributorOrderID := Query.FieldByName('DistributorOrderID').AsString;
        DistrSPDeliveriesArr[0].WarehouseID := Query.FieldByName('WarehouseID').AsString;
      end
      else
      If not Query.FieldByName('OrderID').IsNull then  //отгрузки по заявкам Jeans
      begin
        OrderDeliveriesArr := nil;
        SetLength(OrderDeliveriesArr, 1);
        Result[I].OrderDeliveries := OrderDeliveriesArr;
        OrderDeliveriesArr[0] := DRPService.OrderDelivery.Create;
        OrderDeliveriesArr[0].OrderID := Query.FieldByName('OrderID').AsString;
      end
      else
      if not Query.FieldByName('SalePointID').IsNull then  //отгрузки без заявок Jeans, но по связанным ТТ
      begin
        DistrDeliveriesArr := nil;
        SetLength(DistrDeliveriesArr, 1);
        Result[I].DistributorDeliveries := DistrDeliveriesArr;
        DistrDeliveriesArr[0] := DRPService.DistributorDelivery.Create;
        DistrDeliveriesArr[0].DistributorOrderID := Query.FieldByName('DistributorOrderID').AsString;
        DistrDeliveriesArr[0].SalePointID := Query.FieldByName('SalePointID').AsString;
      end
      else  //отгрузки без заявок Jeans и по несвязанным ТТ
      begin
        DistrSPDeliveriesArr := nil;
        SetLength(DistrSPDeliveriesArr, 1);
        Result[I].DistributorSalePointDeliveries := DistrSPDeliveriesArr;
        DistrSPDeliveriesArr[0] := DRPService.DistributorSalePointDelivery.Create;
        DistrSPDeliveriesArr[0].DistributorOrderID := Query.FieldByName('DistributorOrderID').AsString;
        DistrSPDeliveriesArr[0].DistributorSalePointID := Query.FieldByName('DistributorSalePointID').AsString;
        DistrSPDeliveriesArr[0].SalePointName := Query.FieldByName('SalePointName').AsString;
        DistrSPDeliveriesArr[0].SalePointAddress := Query.FieldByName('SalePointAddress').AsString;
        DistrSPDeliveriesArr[0].WarehouseID := Query.FieldByName('WarehouseID').AsString;
      end;

      FldValues.Clear;
      For J := 0 to Query.FieldCount-1 do
        FldValues.Add(Format('%s=%s', [UpperCase(Query.Fields[J].Name), Query.Fields[J].AsString]));

      QuerySetSQL(QueryInt, DetailScheme, FldValues);
      QueryInt.ExecQuery;
      PositionArr := CreateDeliveryPositions(QueryInt);

      If AnsiSameText(Query.FieldByName('DocType').AsString, 'DRP_SendSellIn') then  //приходы
        DistrSPDeliveriesArr[0].DeliveryPositions := PositionArr
      else
      If not Query.FieldByName('OrderID').IsNull then  //отгрузки по заявкам Jeans
        OrderDeliveriesArr[0].DeliveryPositions := PositionArr
      else
      if not Query.FieldByName('SalePointID').IsNull then  //отгрузки без заявок Jeans, но по связанным ТТ
        DistrDeliveriesArr[0].DeliveryPositions := PositionArr
      else
        DistrSPDeliveriesArr[0].DeliveryPositions := PositionArr;
    finally
      Query.Next;
    end;
  finally
    FldValues.Free;
    QueryInt.Free;
  end;
end;

function TExporter.CreateSalePointBalanceKeg(Query: TpFibQuery): DRPService5.ArrayOfSalePointBalanceKeg;
Var
  I: Integer;
begin
  I := -1;
  While not Query.Eof do
  try
    Inc(I);
    SetLength(Result, I+1);
    Result[I] := DRPService5.SalePointBalanceKeg.Create;
    Result[I].SalePointID := Query.FieldByName('SalePointID').AsInteger;
    Result[I].SkuID := Query.FieldByName('SkuID').AsInteger;
    Result[I].Amount := Query.FieldByName('Amount').AsInteger;
    Result[I].Date := TXSDateTime.Create;
    Result[I].Date.AsDateTime := Date;
{      If Query.ParamCount > 0 then
    try
      xsDate.AsDateTime := StrToDate(Query.ParamValue(0));
//        Result[I].Date.AsDateTime := ConvertDateBack(Query.ParamValue(0));
    except
      xsDate.AsDateTime := Date;
//        Result[I].Date := ConvertDateBack(Date);
    end;}
  finally
    Query.Next;
  end;
end;

function TExporter.CreateWarehouseBalanceKeg(Query: TpFibQuery): DRPService5.ArrayOfWarehouseBalanceKeg;
Var
  I: Integer;
//  xsDate: TXSDateTime;
begin
  I := -1;
//  xsDate := TXSDateTime.Create;
  try
    While not Query.Eof do
    try
      Inc(I);
      SetLength(Result, I+1);
      Result[I] := DRPService5.WarehouseBalanceKeg.Create;
      Result[I].WarehouseID := Query.FieldByName('WarehouseID').AsInteger;
      Result[I].SkuID := Query.FieldByName('SkuID').AsInteger;
      Result[I].Amount := Query.FieldByName('Amount').AsInteger;

//      xsDate := TXSDateTime.Create;
      Result[I].Date := TXSDateTime.Create;
//      xsDate.AsDateTime := Date;
      Result[I].Date.AsDateTime := Date;
{      If Query.ParamCount > 0 then
      try
        xsDate.AsDateTime := StrToDate(Query.ParamValue(0));
//        Result[I].Date.AsDateTime := ConvertDateBack(Query.ParamValue(0));
      except
        xsDate.AsDateTime := Date;
//        Result[I].Date := ConvertDateBack(Date);
      end;}
//      Result[I].Date := xsDate;
    finally
      Query.Next;
    end;
  finally
//    xsDate.Free;
  end;
end;

function TExporter.CreateWarehousesBalance(Query: TpFibQuery):DRPService.ArrayOfBalance;
Var
  I: Integer;
begin
  I := -1;
  While not Query.Eof do
  try
    Inc(I);
    SetLength(Result, I+1);
    Result[I] := DRPService.Balance.Create;
    Result[I].SkuID := Query.FieldByName('SkuID').AsInteger;
    Result[I].WarehouseID := Query.FieldByName('WarehouseID').AsInteger;
    Result[I].Amount := Query.FieldByName('Amount').AsInteger;
  finally
    Query.Next;
  end;
end;

function TExporter.CreateAllContragentDebet(Query: TpFibQuery): DRPService.ArrayOfDistributorDebet;
Var
  I: Integer;
begin
  I := -1;
  While not Query.Eof do
  try
    Inc(I);
    SetLength(Result, I+1);
    Result[I] := DRPService.DistributorDebet.Create;
    Result[I].ContragentID := Query.FieldByName('ContragentID').AsInteger;
    Result[I].DebetSum := Query.FieldByName('DebetSum').AsFloat;
    Result[I].DebetLimit := Query.FieldByName('DebetLimit').AsFloat;
  finally
    Query.Next;
  end;
end;

function TExporter.SendOrdersStatuses(Query: TpFibQuery): TFileName;
Var
  StatusArr: ArrayOfSendStatuses;
  Response: WideString;
  I: Integer;
begin
  try
    StatusArr := CreateOrdersStatuses(Query);

    Response := Fdrp.DRP_SendOrdersStatuses(FConfig.DRPUser, FConfig.DRPPass, StatusArr);

    Result := SaveResponseToFile(Response);
  finally
    For I:=0 to Length(StatusArr)-1 do
      StatusArr[i].Free;
    SetLength(StatusArr, 0);
  end;
end;

function TExporter.SendDocuments(Query: TpFibQuery; Exchange: TExchange; Scheme: TSQLScheme): TFileName;
Var
  InvoiceArr: DRPService.ArrayOfInvoice;
  Response: WideString;
  I: Integer;
begin
  try
    InvoiceArr := CreateInvoices(Query, Scheme.Details);

    If AnsiSameText(Exchange.Name, 'DRP_SendSellIn') then Response := Fdrp.DRP_SendSellIn(FConfig.DRPUser, FConfig.DRPPass, InvoiceArr)
    else
    If AnsiSameText(Exchange.Name, 'DRP_SendSellOut') then Response := Fdrp.DRP_SendSellOut(FConfig.DRPUser, FConfig.DRPPass, InvoiceArr)
    else
    If AnsiSameText(Exchange.Name, 'DRP_SendReturn') then Response := Fdrp.DRP_SendReturn(FConfig.DRPUser, FConfig.DRPPass, InvoiceArr);

    Result := SaveResponseToFile(Response);
  finally
    For I:=0 to Length(InvoiceArr)-1 do
      InvoiceArr[i].Free;
    SetLength(InvoiceArr, 0);
  end;
end;

function TExporter.SendSalePointBalanceKeg(Query: TpFibQuery): TFileName;
Var
  KegArr: ArrayOfSalePointBalanceKeg;
  Response: WideString;
  I: Integer;
begin
  try
    KegArr := CreateSalepointBalanceKeg(Query);

    Response := Fdrp5.SendSalepointBalanceKeg(FConfig.DRPUser, FConfig.DRPPass, KegArr);

    Result := SaveResponseToFile(Response);
  finally
    For I:=0 to Length(KegArr)-1 do
      KegArr[i].Free;
    SetLength(KegArr, 0);
  end;
end;

function TExporter.SendWarehouseBalanceKeg(Query: TpFibQuery): TFileName;
Var
  KegArr: ArrayOfWarehouseBalanceKeg;
  Response: WideString;
  I: Integer;
begin
  try
    KegArr := CreateWarehouseBalanceKeg(Query);

    Response := Fdrp5.SendWarehouseBalanceKeg(FConfig.DRPUser, FConfig.DRPPass, KegArr);

    Result := SaveResponseToFile(Response);
  finally
    For I:=0 to Length(KegArr)-1 do
      KegArr[i].Free;
    SetLength(KegArr, 0);
  end;
end;

function TExporter.SendWarehousesBalance(Query: TpFibQuery): TFileName;
Var
  BalArr: DRPService.ArrayOfBalance;
  Response: WideString;
  I: Integer;
  xsDate: TXSDateTime;
begin
  try
    BalArr := CreateWarehousesBalance(Query);

    xsDate := TXSDateTime.Create;
    try
      xsDate.AsDateTime := Date;
      If Query.ParamCount > 0 then
      try
        xsDate.AsDateTime := StrToDate(Query.ParamValue(0));
      except
        xsDate.AsDateTime := Date;
      end;

      Response := Fdrp.DRP_SendWarehousesBalance(FConfig.DRPUser, FConfig.DRPPass, BalArr, xsDate);
    finally
      xsDate.Free;
    end;

    Result := SaveResponseToFile(Response);
  finally
    For I:=0 to Length(BalArr)-1 do
      BalArr[i].Free;
    SetLength(BalArr, 0);
  end;
end;


function TExporter.SendAllContragentDebet(Query:TpFibQuery): TFileName;
Var
  DebetArr: DRPService.ArrayOfDistributorDebet;
  Response: WideString;
  I: Integer;
begin
  try
    DebetArr := CreateAllContragentDebet(Query);

    Response := Fdrp.DRP_SendAllContragentDebet(FConfig.DRPUser, FConfig.DRPPass, DebetArr);

    Result := SaveResponseToFile(Response);
  finally
    For I:=0 to Length(DebetArr)-1 do
      DebetArr[i].Free;
    SetLength(DebetArr, 0);
  end;
end;

{procedure TExporter.SendDocsPeriod(FromDate, ToDate: TDateTime);
begin
  DeleteDocsPeriod(FromDate, ToDate);
  DoExport('DRP_SendSellIn');
  DoExport('DRP_SendSellOut');
  DoExport('DRP_SendReturn');
end;}

end.
