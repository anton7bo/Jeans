unit Main;

interface

uses
  Classes,
  SvcMgr,
  ExtCtrls,
  IdBaseComponent,
  IdComponent,
  IdContext,
  IdCustomTCPServer,
  IdTelnetServer,

  Config,
  Exporter,
  Importer,
  LogEngine,
  Mailer;

type
  TKrimJeans = class(TService)
    MainTimer: TTimer;
    TelnetServer: TIdTelnetServer;
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure MainTimerTimer(Sender: TObject);
    procedure TelnetServerAuthentication(AContext: TIdContext; const AUsername,
      APassword: string; var AAuthenticated: Boolean);
    procedure TelnetServerDisconnect(AContext: TIdContext);
    procedure TelnetServerExecute(AContext: TIdContext);
  private
    { Private declarations }
    FBusy: Boolean;
    FCurDate: TDateTime;  //текущая дата
    FMinutesLeft: Integer; //счетчик минут

    FConfig: TConfig;
    FExporter: TExporter;
    FImporter: TImporter;

    FLog : TLogEngine;
    FMailer : TSmartMailer;

    procedure DeleteOldFiles;
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  KrimJeans: TKrimJeans;

implementation
Uses
  OldFilesRemover,

  ActiveX,
  Registry,
  StrUtils,
  SysUtils,
  Windows;

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  KrimJeans.Controller(CtrlCode);
end;

function TKrimJeans.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TKrimJeans.MainTimerTimer(Sender: TObject);
Var
  Hour, Min, Sec, MSec: Word;
  I: Integer;
  ResFileName: String;
begin
  try
    If FBusy then Exit;

    FBusy:=True;
    try
      If Trunc(FCurDate) < Trunc(Date) then  //смена суток
      begin
        FCurDate := Date;
        FMinutesLeft := 0;
        FLog.NewLogFiles;
        DeleteOldFiles;
      end;

      DecodeTime(Time, Hour, Min, Sec, MSec);

      Inc(FMinutesLeft);
      FConfig := TConfig.Create('');
      try
    {      //импорт заявок
        if (FMinutesLeft = 1) or ((FMinutesLeft > FConfig.ImportPeriod) and ((FMinutesLeft mod FConfig.ImportPeriod) = 0)) then
        begin
          FImporter := TImporter.Create(FConfig, FLog, FMailer);
          try
            FImporter.GetFiles('DRP_GetOrders');
            FImporter.Process;
          finally
            FImporter.Free;
          end;
        end;}

        FImporter := TImporter.Create(FConfig, FLog, FMailer);
        FExporter := TExporter.Create(FConfig, FLog, FMailer);
        try
          FImporter.ExchangeList.SetNeedTransfer(FMinutesLeft);
          FExporter.ExchangeList.SetNeedTransfer(FMinutesLeft);

          //импорт
          For I:=0 to FImporter.ExchangeList.Count-1 do
          begin
            If Abs(FImporter.ExchangeList[I].ExchangeType) = 1 then  //1-импорт, -1-набор обменов для импорта (через LinkedExchange)
              If FImporter.ExchangeList[I].DoTranfer then
              begin
                ResFileName := FImporter.GetFiles(FImporter.ExchangeList[I]);
              end;
          end;
          If ResFileName > '' then FImporter.Process;

          //экспорт
          if (AnsiContainsText(FConfig.ExportTimesPartial, Format('%s:%s', [FormatFloat('00', Hour), FormatFloat('00', Min)]))) then FExporter.MultyExport(False);
          if (AnsiContainsText(FConfig.ExportTimesFull, Format('%s:%s', [FormatFloat('00', Hour), FormatFloat('00', Min)]))) then FExporter.MultyExport(True);

          For I:=0 to FExporter.ExchangeList.Count-1 do
          begin
            If Abs(FExporter.ExchangeList[I].ExchangeType) = 2 then  //;2-экспорт, -2-набор обменов для экпорта (через LinkedExchange)
              If FExporter.ExchangeList[I].DoTranfer then FExporter.DoExport(FExporter.ExchangeList[I].Name);
          end;
        finally
          FExporter.Free;
          FImporter.Free;
        end;
      finally
        FConfig.Free;
      end;
    finally
      FBusy:=False;
    end;
  except
    on E:Exception do
    begin
      FLog.WriteLog(Format('ОШИБКА в методе %s.MainTimerTimer. %s', [Self.ClassName, E.Message]), 2);
      FMailer.SendErrMessage(rctAdmin, Format('ОШИБКА в методе %s.MainTimerTimer', [Self.ClassName]), E.Message, nil);
    end;
  end;
end;

procedure TKrimJeans.ServiceAfterInstall(Sender: TService);
var
  Reg: TRegIniFile;
begin
  try
    Reg := TRegIniFile.Create(KEY_ALL_ACCESS);
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      // Создаём системный лог для себя
      Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\Eventlog\Application\' + Name, True);
      Reg.WriteString('\SYSTEM\CurrentControlSet\Services\Eventlog\Application\' + Name, 'EventMessageFile', ParamStr(0));
      TRegistry(Reg).WriteInteger('TypesSupported', 7);
      // Прописываем себе описание
      Reg.WriteString('\SYSTEM\CurrentControlSet\Services\' + Name, 'Description', 'Служба осуществляет обмен данными между системой учета "Крыма" и системой Jeans.');
    finally
      FreeAndNil(Reg);
    end;
  except

  end;
end;

procedure TKrimJeans.ServiceAfterUninstall(Sender: TService);
var
  Reg: TRegIniFile;
begin
  try
    Reg := TRegIniFile.Create(KEY_ALL_ACCESS);
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      // Удалим свой системный лог
      Reg.EraseSection('\SYSTEM\CurrentControlSet\Services\Eventlog\Application\' + Name);
    finally
      FreeAndNil(Reg);
    end;
  except

  end;
end;

procedure TKrimJeans.ServiceExecute(Sender: TService);
begin
  FLog := TLogEngine.Create('');
  FLog.WriteLog('Запуск сервиса', 1);
  FCurDate:=Date;
  FConfig := TConfig.Create('');
  try
    FMailer := TSmartMailer.Create('', FLog, FConfig.LogLevel);
  finally
    FConfig.Free;
  end;

  try
    Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED));  //нужно только для telnet так как вызов обмена по команде из telnet FImporter.GetFiles происходит в отдельном потоке
    TelnetServer.LoginMessage := Self.DisplayName;
    TelnetServer.Active := True;
  except
  end;

  while not Terminated do begin
    ServiceThread.ProcessRequests(True);
  end;
end;

procedure TKrimJeans.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  FMailer.Free;
  FLog.WriteLog('Стоп сервис', 1);
  FLog.Free;
  TelnetServer.Active := False;
  CoUnInitialize;
end;

procedure TKrimJeans.DeleteOldFiles;
Var
  OldRemover: TOldFilesRemover;
begin
  try
    FConfig := TConfig.Create('');
    try
      If FConfig.StoreFileDays < 1 then Exit;

      OldRemover := TOldFilesRemover.Create(FConfig.Path, FConfig.StoreFileDays);
      try
        OldRemover.DeleteOldFiles;
        FLog.WriteLog('Старые файлы удалены', 1);
      finally
        OldRemover.Free;
      end;
    finally
      FConfig.Free;
    end;
  except
  end;
end;

procedure TKrimJeans.TelnetServerAuthentication(AContext: TIdContext;
  const AUsername, APassword: string; var AAuthenticated: Boolean);
begin
  try
    FLog.WriteLog(Format('Авторизация telnet. Пользователь: %s', [AUsername]), 1);
    if (Uppercase(AUsername) = 'SYSDBA') and ((UpperCase(APassword) = UpperCase('masterke')) or (UpperCase(APassword) = UpperCase('masterkey'))) then
    begin
      AAuthenticated:=True;
      FLog.WriteLog(Format('... Авторизация успешна', [AUsername]), 2);
      AContext.Connection.IOHandler.WriteLn('Welcome!');
    end;
  except
  end;
end;

procedure TKrimJeans.TelnetServerDisconnect(AContext: TIdContext);
begin
  try
    FLog.WriteLog('Сеанс telnet завершен', 1);
  except
  end;
end;

procedure TKrimJeans.TelnetServerExecute(AContext: TIdContext);
Var
  Cmd: String;
  ResFileName: String;
  ExchangeName: String;
begin
  try
    Cmd := AContext.Connection.IOHandler.ReadLn();
    FLog.WriteLog(Format('Получена команда: %s', [Cmd]), 2);

    if ((UpperCase('shops') = Uppercase(Cmd)) or (UpperCase('skus') = Uppercase(Cmd))) then
    begin
      FImporter := TImporter.Create('', FLog, FMailer);
      try
        if (UpperCase('shops') = Uppercase(Cmd)) then ExchangeName := 'DRP_GetSalePoints'
        else if (UpperCase('skus') = Uppercase(Cmd)) then ExchangeName := 'DRP_GetSkuAssortment';
        ResFileName := FImporter.GetFiles(ExchangeName);
        If ResFileName > '' then FImporter.Process;
      finally
        FImporter.Free;
      end;
      AContext.Connection.IOHandler.WriteLn(Format('%s done', [ExchangeName]));
    end
    else AContext.Connection.IOHandler.WriteLn('Don''t get you');
  except
  end;
end;

end.
