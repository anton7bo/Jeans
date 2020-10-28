unit Importer;

interface
Uses
  JnsBaseExportImport,
  Exchange,
  SysUtils;

type
  TImporter = class(TJnsBaseExportImport)
  private
    FNeedImportToOCO: Boolean;
  public
    function GetFiles(ExchangeName: String): TFileName; overload;
    function GetFiles(Exchange: TExchange; const Level: Integer = 0): TFileName; overload;
    procedure ImportOrdersToOCO;
    procedure Process;
  end;

implementation
uses
//  ActiveX,
  BaseExportImport,
  FibEngine,
  Mailer,
  pFibQuery,
  StrUtils,
  XMLTransform,
  XSBuiltins;

const
  sqlImportOrders = 'select * from %s(%d)';

function TImporter.GetFiles(ExchangeName: String): TFileName;
Var
  Exchange: TExchange;
begin
  try
    Exchange := FExchangeList.IndexOf(ExchangeName);
    If Exchange = nil then raise Exception.CreateFmt('�� ������ ����� %s � ���-�����', [ExchangeName]);
    Result := GetFiles(Exchange);
  except
    on E:Exception do
    begin
      WriteLog(Format('������ � ������ %s.GetFiles. %s', [Self.ClassName, E.Message]), 2);
      FMailer.SendErrMessage(rctAdmin, Format('������ � ������ %s.GetFiles', [Self.ClassName]), E.Message, nil);
    end;
  end;
end;

function TImporter.GetFiles(Exchange: TExchange; const Level: Integer = 0): TFileName;
Var
  xsDate: TXSDateTime;
  Response: WideString;
  WorkDate, DateFrom, DateTo: TDateTime;
  I: Integer;
  LinkedExchangeName: String;
  LinkedExchange: TExchange;
begin
  If Exchange = nil then raise Exception.Create('�� ������ ����� (TExchange)');

  try
    WriteLog(Format('����� %s (%s - %s)', [Exchange.Name, Exchange.DateFrom, Exchange.DateTo]), 1 + Level);

    DateFrom := 0;
    DateTo := 0;
    If Exchange.DateFrom > '' then DateFrom := StrToDate(Exchange.DateFrom);
    If Exchange.DateTo > '' then DateTo := StrToDate(Exchange.DateTo);

    If DateFrom = 0 then DateFrom := DateTo;
    If DateTo = 0 then DateTo := DateFrom;
//    WriteLog(Format('���� %s - %s', [DateToStr(DateFrom), DateToStr(DateTo)]), 2);

//    CoInitialize(nil);
    xsDate := TXSDateTime.Create;
    try
      For I:=0 to Trunc(DateTo - DateFrom) do  //���� ��� ������� � ����� (��������), ��� ��������� �� ���� ������ ���� ����� 1 ��������
      begin
        //������ ��� ���� (�� ���� ������ ���� DateFrom = DateTo = 0)
        If AnsiSameText(Exchange.Name, 'DRP_GetOrdersDemo') then Response := Fdrp.DRP_GetOrdersDemo(FConfig.drpUser, FConfig.drpPass)
        else
        If AnsiSameText(Exchange.Name, 'DRP_GetOrders') then Response := Fdrp.DRP_GetOrders(FConfig.drpUser, FConfig.drpPass)
        else
        If AnsiSameText(Exchange.Name, 'DRP_GetEncashments') then  Response := Fdrp.DRP_GetEncashments(FConfig.drpUser, FConfig.drpPass)
        else
        If AnsiSameText(Exchange.Name, 'DRP_GetSalePoints') then  Response := Fdrp.DRP_GetSalePoints(FConfig.drpUser, FConfig.drpPass)
//        else
//        If AnsiSameText(Exchange.Name, 'DRP_GetSalePoints2') then  Response := Fdrp.DRP_GetSalePoints2(FConfig.drpUser, FConfig.drpPass)
        else
        If AnsiSameText(Exchange.Name, 'DRP_GetSkuAssortment') then  Response := Fdrp.DRP_GetSkuAssortment(FConfig.drpUser, FConfig.drpPass)
        else
        If AnsiSameText(Exchange.Name, 'DRP_GetWarehouses') then  Response := Fdrp.DRP_GetWarehouses(FConfig.drpUser, FConfig.drpPass)
        else
        begin //������ � �����
          If (Exchange.DateFrom = '') and (Exchange.DateTo = '') then raise Exception.CreateFmt('��� ������ %s �� ������ ����', [Exchange.Name]);

          xsDate.AsDateTime := DateFrom + I;

          If AnsiSameText(Exchange.Name, 'DRP_GetOrdersByDate') then Response := Fdrp.DRP_GetOrdersByDate(FConfig.drpUser, FConfig.drpPass, xsDate)
          else
          If AnsiSameText(Exchange.Name, 'DRP_GetEncashmentsByDate') then Response := Fdrp.DRP_GetEncashmentsByDate(FConfig.drpUser, FConfig.drpPass, xsDate.NativeToXS)
          else
          //������ � �����, ���� ������� � �������� ������ ����
          If AnsiSameText(Exchange.Name, 'DRP_DeleteSellOut') then Response := Fdrp.DRP_DeleteSellOut(FConfig.DRPUser, FConfig.DRPPass, xsDate.NativeToXS, '')
          else
          If AnsiSameText(Exchange.Name, 'DRP_DeleteSellIn') then Response := Fdrp.DRP_DeleteSellIn(FConfig.DRPUser, FConfig.DRPPass, xsDate.NativeToXS, '')
          else
          If AnsiSameText(Exchange.Name, 'DRP_DeleteReturn') then Response := Fdrp.DRP_DeleteReturn(FConfig.DRPUser, FConfig.DRPPass, xsDate.NativeToXS, '')
          else
          If AnsiSameText(Exchange.Name, 'GetKegDiscrepancy') then Response := Fdrp5.GetKegDiscrepancy(FConfig.drpUser, FConfig.drpPass, xsDate, True)
          else
          If Exchange.ExchangeType = 1 then raise Exception.CreateFmt('����������� ����� (�����) %s', [Exchange.Name]);  //���� ������ � ����� �����������, �� ����������. ���� ��� ����� ������� (=-1), �� ��� ����������
        end;

        If AnsiContainsText(Exchange.Name, 'DRP_GetOrders') then FNeedImportToOCO := True;

        If Response > '' then
        begin
          Result := SaveResponseToFile(Response);
          WriteLog(Format(' .. �������� ���� %s', [ExtractFileName(Result)]), 1);
//          If (I < 4) then WriteLog(Format('�������� ���� %s', [ExtractFileName(Result)]), 1)  //�� ��� ����������
//          else If (I = Trunc(DateTo - DateFrom)) then WriteLog(Format('... �������� ���� %s', [ExtractFileName(Result)]), 1);
        end;
      end;

      //���������� LinkedExchanges
      For I := 1 to WordCount(Exchange.LinkedExchanges, cDelims) do
      begin
        LinkedExchangeName := ExtractWord(I, Exchange.LinkedExchanges, cDelims);
        LinkedExchange := FExchangeList.IndexOf(LinkedExchangeName);
        If LinkedExchange = nil then raise Exception.CreateFmt('�� ������ LinkedExchange ����� %s � ������ %s �� ����� � ���-�����', [LinkedExchangeName, Exchange.Name]);

        //��������� � LinkedExchange ���� �������� ������ (������ ���� ������� ����� �� �������� ����� ����� ������, �.�. ����� ������ ������������ ������ ��� ����������� ������ ������ �������)
        If Exchange.ExchangeType = -1 then
        begin
          LinkedExchange.DateFrom := Exchange.DateFrom;
          LinkedExchange.DateTo := Exchange.DateTo;
        end;

        Result := GetFiles(LinkedExchange, Level+1);
      end;
    finally
      xsDate.Free;
//      CoUnInitialize;
    end;
  except
    on E:Exception do
    begin
      WriteLog(Format('������ � ������ %s.GetFiles. %s', [Self.ClassName, E.Message]), 2 + Level);
      FMailer.SendErrMessage(rctAdmin, Format('������ � ������ %s.GetFiles', [Self.ClassName]), E.Message, nil);
    end;
  end;
end;

procedure TImporter.ImportOrdersToOCO;
Var
  Query: TpFibQuery;
  Mess: String;
  WarnMess: String;
  ErrMess: String;
  Cnt: Integer;
begin
  try
    WriteLog(Format('������ ������ � oco$* (SP = %s, DaysAgo = %d)', [FConfig.ImportToOCOStoredProc, FConfig.ImportOrdersDaysAgo]), 1);

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
            If Mess = '' then Mess:=Format('������ ������� ������: %s (%s)', [Query.Fields[0].AsString, ErrMess])  //ORDERID / ORDERNO
            else Mess:=Format('%s, %s (%s)', [Mess, Query.Fields[0].AsString, ErrMess]);
          end
          else
          If ErrMess > '' then
          begin
            If WarnMess = '' then WarnMess := Format('��������������. %s' , [ErrMess])
            else WarnMess := Format('%s, %s', [WarnMess, ErrMess]);
          end;
          Inc(Cnt);
        finally
          Query.Next;
        end;

        If Query.Transaction.Active then Query.Transaction.Commit;
        WriteLog(Format('���������� ������: %d', [Cnt]), 2);

        If Mess > '' then WriteLog(Mess, 2);
        If WarnMess > '' then WriteLog(WarnMess, 2);
        If (Mess > '') or (WarnMess > '') then
          FMailer.SendErrMessage(rctAll, '������ ������� ������ � ���� ������� �������', Mess + #13#10 + WarnMess, nil);
      finally
        Query.Free;
      end;
    finally
      FFibEngine.Disconnect;
    end;

    FNeedImportToOCO := False;
  except
    on E:Exception do
    begin
      WriteLog(Format('������ � ������ %s.ImportOrdersToOCO. %s', [Self.ClassName, E.Message]), 2);
      FMailer.SendErrMessage(rctAdmin, Format('������ � ������ %s.ImportOrdersToOCO', [Self.ClassName]), E.Message, nil);
    end;
  end;
end;

procedure TImporter.Process;
begin
  CheckFiles;
  FilesToScripts;
  ExecScripts;
  If FNeedImportToOCO then ImportOrdersToOCO;
end;

end.

