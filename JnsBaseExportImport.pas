unit JnsBaseExportImport;

interface
Uses
  BaseExportImport,
  Config,
  DRPService,
  DRPService5,
  LogEngine,
  Mailer,
  Exchange,
  SOAPHTTPClient,
  SQLScheme,
  SysUtils;

type
  //������� ����� ��� �������� �������� ��������, ������ � ��
  TJnsBaseExportImport = class(TBaseExportImport)
  private
    procedure Init; //(LogEngine: TLogEngine; Mailer: TSmartMailer);
    procedure LoadSchemeConfig(IniFileName: String);
    function PrepareRio: THTTPRio;
  protected
    FHttpRio: THTTPRIO;
    FHttpRio5: THTTPRIO;
    Fdrp: DRPService.DRPServiceSoap;
    Fdrp5: DRPService5.DRPServiceSoap5;  //+13.08.20
    FExchangeList: TExchangeList;
    FSchemeList: TSQLSchemeList;
    function SaveResponseToFile(Data: WideString): TFileName;
    function MakeFileName(Postfix, Ext: String): TFileName;
    function MakeFileNameVersion(FileName: TFileName; BaseName: String): TFileName;
  public
    property ExchangeList: TExchangeList read FExchangeList;
    property SchemeList: TSQLSchemeList read FSchemeList;
    constructor Create(IniFileName: String; LogEngine: TLogEngine; Mailer: TSmartMailer); overload;
    constructor Create(Config: TConfig; LogEngine: TLogEngine; Mailer: TSmartMailer); overload;
    destructor Destroy;
    procedure CheckFiles;
  end;

implementation
Uses
  Classes,
  DRPXMLChecker,
  MSXML2_TLB,
  OPConvert;

{ TJnsBaseExportImport }

constructor TJnsBaseExportImport.Create(IniFileName: String; LogEngine: TLogEngine; Mailer: TSmartMailer);
begin
  inherited;
  Init;
end;

constructor TJnsBaseExportImport.Create(Config: TConfig; LogEngine: TLogEngine; Mailer: TSmartMailer);
begin
  inherited;
  Init;
end;

procedure TJnsBaseExportImport.Init;
begin
  inherited;

  FExchangeList := TExchangeList.Create;
  FSchemeList := TSQLSchemeList.Create;
  LoadSchemeConfig(FConfig.TransfersIni);

{  FHttpRio := THttpRio.Create(nil);
  FHttpRio.HTTPWebNode.ConnectTimeout := 60000;
  FHttpRio.HTTPWebNode.SendTimeout := 300000;
  FHttpRio.HTTPWebNode.ReceiveTimeout := 300000;
  FHttpRio.Converter.Encoding := FConfig.XMLEncoding; //'UTF-8';  ����� � ��������� xml ����� ��� ������ encoding
  FHttpRio.Converter.Options := [soSendMultiRefObj, soTryAllSchema, soRootRefNodesToBody, soCacheMimeResponse, soUTF8InHeader, soUTF8EncodeXML];  //����� � ��������� xml ����� ��� ������ encoding
  FHttpRio.HTTPWebNode.UseUTF8InHeader := True;
//  FHttpRio.FHttpRio.HTTPWebNode.InvokeOptions := [soIgnoreInvalidCerts, soAutoCheckAccessPointViaUDDI]}
  FHttpRio := PrepareRio;
  FHttpRio5 := PrepareRio;

  Fdrp := DRPService.GetDRPServiceSoap(False, '', FHTTPRIO);
  Fdrp5 := DRPService5.GetDRPServiceSoap(False, '', FHTTPRIO5);
end;

function TJnsBaseExportImport.PrepareRio: THTTPRio;
begin
  Result:= THttpRio.Create(nil);
  Result.HTTPWebNode.ConnectTimeout := 60000;
  Result.HTTPWebNode.SendTimeout := 300000;
  Result.HTTPWebNode.ReceiveTimeout := 300000;
  Result.Converter.Encoding := FConfig.XMLEncoding; //'UTF-8';  ����� � ��������� xml ����� ��� ������ encoding
  Result.Converter.Options := [soSendMultiRefObj, soTryAllSchema, soRootRefNodesToBody, soCacheMimeResponse, soUTF8InHeader, soUTF8EncodeXML];  //����� � ��������� xml ����� ��� ������ encoding
  Result.HTTPWebNode.UseUTF8InHeader := True;
//  Result.FHttpRio.HTTPWebNode.InvokeOptions := [soIgnoreInvalidCerts, soAutoCheckAccessPointViaUDDI]
end;

destructor TJnsBaseExportImport.Destroy;
begin
  inherited;

  FExchangeList.Free;
  FSchemeList.Free;

  Fdrp := nil;
  Fdrp5 := nil;

//  FHttpRio.Free;  //HTTPRIO.Create(AOwner) - The AOwner parameter specifies another component (usually a form or data module) that is responsible for freeing the THTTPRIO instance. It becomes the value of the Owner property. If the THTTPRIO instance is created with AOwner set to nil (Delphi) or NULL (C++), then it is automatically freed when its reference count drops to zero.
end;

procedure TJnsBaseExportImport.LoadSchemeConfig(IniFileName: String);
begin
  FExchangeList.LoadFromIni(IniFileName);
  FSchemeList.LoadFromIni(IniFileName);
end;

procedure TJnsBaseExportImport.CheckFiles;
Var
  sr: TSearchRec;
  Res: Integer;
  FName: String;
  Checker: TDRPXMLChecker;
  FList:TStringList;
begin
  //�������� ���������� xml ������ �� ������
  try
    WriteLog('�������� ���������� xml ������ �� ������', 1);
    Checker := TDRPXMLChecker.Create('');
    try
      if FindFirst(Format('%s\*.xml', [FConfig.ImportFolder]), faAnyFile, sr) = 0 then
      try
        repeat
          if sr.Attr in [faDirectory] then Continue;

          FName:=Format('%s%s', [FConfig.ImportFolder, sr.Name]);
          WriteLog(Format('������ ����� %s', [ExtractFileName(FName)]), 2);

          Checker.FileName := FName;
          Checker.CheckFileForError;
          If (AnsiSameText(Checker.ProcessingType, 'Readonly')) then  //��� ������ ������ �� ������� ����� (��, �����, ������, ������)
          begin
            If (Checker.ErrorID = -1) then
            begin
              WriteLog('.. �� �������� ������', 3);
              DeleteFile(FName);  //MoveFileToDone(FName, True);
            end
            else WriteLog('.. ok', 3);  //���� ������ ��� �� �������� ������, �� ���� ��������� �� ����� ��� ������������ �������������� � ������ � ����������� ������� (������ ������)
          end
          else  //��������� �������� ������ �� ������ �����
          begin
            If (Checker.ErrorID <> 0) then  //���� ������
            begin
              WriteLog(Format('������. %s', [Checker.ErrorList.Text]), 3);

              FList:=TStringList.Create;
              try
                FList.Add(FName);
                FMailer.SendErrMessage(rctAdmin, Format('������ ErrorID = %d / Severity = %d � ����� %s', [Checker.ErrorID, Checker.Severity, FName]),
                  Format('������. %s #13#10 %s', [Checker.ErrorList.DelimitedText, Checker.ErrForData.DelimitedText]), FList);
              finally
                FList.Free;
              end;
            end
            else WriteLog('.. ok', 3);  //��� ������
            MoveFileToDone(FName, Checker.ErrorID = 0);
          end
        until FindNext(sr) <> 0;
      finally
        SysUtils.FindClose(sr);  //+22.01.20
      end
      else WriteLog('.. ��� ������', 2);
    finally
      Checker.Free;
    end;
  except
    on E:Exception do
    begin
      WriteLog(Format('������ � ������ %s.CheckFiles. %s', [Self.ClassName, E.Message]), 2);
      FMailer.SendErrMessage(rctAdmin, Format('������ � ������ %s.CheckFiles', [Self.ClassName]), E.Message, nil);
    end;
  end;
end;

function TJnsBaseExportImport.MakeFileName(Postfix, Ext: String): TFileName;
Var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  Separator: String;
begin
  Separator := '-';
  DecodeDate(Now, Year, Month, Day);
  DecodeTime(Now, Hour, Min, Sec, MSec);
  Result:=Format('%s%s%s%s%s%s%s%s%s%s.%s', [FormatFloat('0000', Year), FormatFloat('00', Month), FormatFloat('00', Day),  //*12.08.11
    Separator, FormatFloat('00', Hour), FormatFloat('00', Min), FormatFloat('00', Sec), FormatFloat('000', MSec), Separator, Postfix, Ext]);
end;

function TJnsBaseExportImport.MakeFileNameVersion(FileName: TFileName; BaseName: String): TFileName;
Var
  Path: String;
  Ext: String;
  I: Integer;
begin
  Path := ExtractFilePath(FileName);
  Ext := ExtractFileExt(FileName);
  I := 0;
  Repeat
    Inc(I);
    Result := Format('%s%s%d%s', [Path, BaseName, I, Ext]);
  Until not FileExists(Result);
end;

function TJnsBaseExportImport.SaveResponseToFile(Data: WideString): TFileName;
Var
  ResDoc: DomDocument;
  r: IXMLDOMElement;
begin
  ResDoc := CoDOMDocument.Create;
  try
    ResDoc.Set_async(false);
    ResDoc.loadXML(Data);
    if ResDoc = nil then raise Exception.Create('�� ���� ��������� ����. �������� ����� �� DRP ������� (�� XML ��������)');

    r := ResDoc.Get_documentElement;
    if r = nil then raise exception.Create('�� ���� ��������� ����. �������� ����� �� DRP ������� - ��� ��������� ��������');

    Result := Format('%s%s.xml', [FConfig.ImportFolder, r.nodeName]);
    If FileExists(Result) then Result := MakeFileNameVersion(Result, r.nodeName);
    ResDoc.save(Result);
  finally
    ResDoc := nil;
  end;
end;

end.
