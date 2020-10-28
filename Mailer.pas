unit Mailer;

interface
uses
  Classes,
  ExtCtrls,
  IdSMTP,
  IdSSLOPenSSL,
  LogEngine;

type

  TMailer = class
  private
    FSMTP: TIdSMTP;
    FSSLHandler: TIdSSLIOHandlerSocketOpenSSL;
    FSelfEmail: String;
    FDoFreeSMTP: boolean;
    FLogEngine: TLogEngine;
    FLogLevel: Integer;
    procedure LoadConfig(IniFileName: String);  //из ини-файла приложения
  protected
    procedure WriteLog(Mess: String; const Indent: Integer = 1; const WithTime: boolean = True);
  public
    constructor Create(IniFileName: String; LogEngine: TLogEngine; const LogLevel: Byte = 1); overload;
    constructor Create(SMTP: TIdSMTP; LogEngine: TLogEngine; const LogLevel: Byte = 1); overload;
    destructor Destroy; override;
    function SendMessage(Recipients, Subj, Body: String; FileNameList:TStringList):boolean; overload;
    function SendMessage(Recipients, Subj: String; Body, FileNameList:TStringList):boolean; overload;
  end;

//  TMessType = (mstCommon, mstError);  //mstCommon - отправляем всегда (без проверок), mstError - оправляем только если проходит проверку
  TRecipType = (rctAdmin, rctOperator, rctAll);

  TSmartMailer = class(TMailer)
  private
    FRecipientsAdmin: TStringList;
    FRecipientsOper: TStringList;
    FIgnoreStrings: TStringList;  //список строк, при наличии которых в теле письма, ничего не отправлять
    FSendedMessages: TStringList;  //текст отправленных сообщений
    FErrPeriodMinute: Integer;  //период в течение которого отправлять одно и тоже сообщение не более 1 раза (только для mstError) по умолчанию 1 раз в сутки
    FTimer: TTimer;
    FMinutesLeft: LongInt;

    procedure LoadConfig(IniFileName: String);  //из ини-файла приложения
    function NeedToSend(Body: String): Boolean;  //проверяет нужно ли отправлять
    procedure TimerOnTime(Sender: TObject);
  public
    property IgnoreStrings: TStringList read FIgnoreStrings;
    property ErrPeriodMinute: Integer read FErrPeriodMinute write FErrPeriodMinute;

    constructor Create(IniFileName: String; LogEngine: TLogEngine; const LogLevel: Byte = 1);
    destructor Destroy; override;
    procedure ClearSendedMessages;
    function SendCommonMessage(RecipientType: TRecipType; Subj, Body: String; FileNameList:TStringList):boolean;
    function SendErrMessage(RecipientType: TRecipType; Subj, Body: String; FileNameList:TStringList):boolean;
  end;

implementation
Uses
  Forms,
  IdAttachmentFile,
  IdExplicitTLSClientServerBase,
  IdMessage,
  IniFiles,
  StrUtils,
  SysUtils,
  Windows;

const
  rctsAdmin = 'admin';
  rctsOperator = 'operator';
  rctsAll = 'all';

function IniReadString(Ini: TIniFile; Section, Ident, Default: String):String;
begin
  If Ini.ValueExists(Section, Ident) then Result:=Ini.ReadString(Section, Ident, Default)
  else
  begin
    Result:=Default;
    Ini.WriteString(Section, Ident, Default);
  end;
end;

function IniReadInteger(Ini: TIniFile; Section, Ident: String; Default: Longint):Longint;
begin
  If Ini.ValueExists(Section, Ident) then Result:=Ini.ReadInteger(Section, Ident, Default)
  else
  begin
    Result:=Default;
    Ini.WriteInteger(Section, Ident, Default);
  end;
end;

//------------------------------------------------------------------------------
{ TMailer }

constructor TMailer.Create(IniFileName: String; LogEngine: TLogEngine; const LogLevel: Byte = 1);
begin
  FSMTP := TIdSMTP.Create;
  FSSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create;
  FDoFreeSMTP := True;
  FLogEngine := LogEngine;
  FLogLevel := LogLevel;

  LoadConfig(IniFileName);
end;

constructor TMailer.Create(SMTP: TIdSMTP; LogEngine: TLogEngine; const LogLevel: Byte = 1);
begin
  FDoFreeSMTP := False;
  FSMTP := SMTP;
  FLogEngine := LogEngine;
  FLogLevel := LogLevel;
end;

destructor TMailer.Destroy;
begin
  If FDoFreeSMTP then
  begin
    FSSLHandler.Free;
    FSMTP.Free;
  end;
end;

procedure TMailer.LoadConfig(IniFileName: String);
Var
  Ini : TIniFile;
  Path : String;
  SmtpAuthType: Integer;
  UseSSL: Boolean;
begin
  Path:=ExtractFilePath(ParamStr(0));
  If Path[Length(Path)] <> '\' then Path:=Path + '\';

  If IniFileName = '' then IniFileName := Path + 'settings.ini';
  Ini := TIniFile.Create(IniFileName);
  try
    FSMTP.Host := IniReadString(Ini, 'Smtp', 'ServerName', '');
    FSMTP.Port := IniReadInteger(Ini, 'Smtp', 'ServerPort', 25);
    FSMTP.UserName := IniReadString(Ini, 'Smtp', 'ServerUser', '');
    FSMTP.Password := IniReadString(Ini, 'Smtp', 'ServerPassword', '');
    SmtpAuthType := IniReadInteger(Ini, 'Smtp', 'SMTPAuthenticationType', 0);
    case SmtpAuthType of
      0: FSMTP.AuthType := atNone;
      1: FSMTP.AuthType := atDefault;
    end;
///    FSMTP.OnDisconnected:= SMTPDisconnected;  //+15.10.14

    UseSSL := IniReadInteger(Ini, 'Smtp', 'UseSSL', 0) = 1;
    if (UseSSL) then
    begin
      FSSLHandler.Host := FSMTP.Host;
      FSSLHandler.Port := FSMTP.Port;
      FSSLHandler.SSLOptions.Method := sslvTLSv1;
      FSSLHandler.SSLOptions.Mode := sslmUnassigned;
      FSMTP.IOHandler := FSSLHandler;
      FSMTP.UseTLS := utUseImplicitTLS;
    end;

    FSelfEmail:=IniReadString(Ini, 'Smtp', 'Email', '');
  finally
    Ini.Free;
  end;
end;

function TMailer.SendMessage(Recipients, Subj: String; Body,
  FileNameList: TStringList): boolean;
begin
  Result := SendMessage(Recipients, Subj, Body.Text, FileNameList);
end;

procedure TMailer.WriteLog(Mess: String; const Indent: Integer; const WithTime: boolean);
begin
  If (FLogEngine <> nil) then FLogEngine.WriteLog(Mess, Indent, WithTime);
end;

function TMailer.SendMessage(Recipients, Subj, Body: String; FileNameList: TStringList): boolean;
Var
  LID: Integer;
  I: Integer;
  Msg: TIdMessage;
begin
  Result:=False;
  If Recipients = '' then raise Exception.CreateFmt('ОШИБКА. Не указаны адресаты для отправки почты в методе %s.SendMessage', [Self.ClassName]);

  If not FSMTP.Connected then
  try
    FSMTP.Connect;
  except
    on E:Exception do
    begin
      If (FLogLevel > 0) then WriteLog(Format('ОШИБКА. Не удалось соединиться с сервером %s. %s', [FSMTP.Host, E.Message]), 3);
      Exit;
    end;
  end;

  LID := SysLocale.PriLangID;
  Msg := TIdMessage.Create;
  try
    SysLocale.PriLangID := LANG_UKRAINIAN;  //????

    if FSelfEmail > '' then Msg.From.Text := FSelfEmail //+13.08.11
    else Msg.From.Text := Format('%s@%s', [FSMTP.Username, FSMTP.Host]);

    Msg.Recipients.EMailAddresses := Recipients;
    Msg.Subject := Format('%s. %s', [Application.Title, Subj]);
    Msg.Body.Text := Body;
    Msg.ReceiptRecipient.Text := '';

    If (FileNameList <> nil) and (FileNameList.Count > 0) then
    begin
      For I:=0 to FileNameList.Count - 1 do
      begin
        If (FileNameList[I] <> '') and (FileExists(FileNameList[I])) then
        begin
          TIdAttachmentFile.Create(Msg.MessageParts, FileNameList[I]);
        end;
      end;
    end;

    try
      FSMTP.Send(Msg);
      Result:=True;
      If (FLogLevel > 1) then WriteLog(Format('Сообщение отправлено на %s', [Msg.Recipients.EMailAddresses]), 3);  //*18.05.12
    except
      on E:Exception do
      begin
        If (FLogLevel > 0) then WriteLog(Format('ОШИБКА. Не удалось отправить сообщение на %s', [Msg.Recipients.EMailAddresses]), 3);  //*13.10.14
        Exit;
      end;
    end;
  finally
    SysLocale.PriLangID := LID;  //+28.03.11
    Msg.Free;
    FSMTP.Disconnect;
  end;
end;

{ TSmartMailer }

procedure TSmartMailer.ClearSendedMessages;
begin
  FSendedMessages.Clear;  //возможно утечка помяти??? не освобождает Object в это списке
  FMinutesLeft := 0;
end;

constructor TSmartMailer.Create(IniFileName: String; LogEngine: TLogEngine; const LogLevel: Byte = 1);
begin
  inherited;

  FRecipientsAdmin := TStringList.Create;
  FRecipientsOper := TStringList.Create;
  FRecipientsAdmin.Delimiter := ';';
  FRecipientsOper.Delimiter := ';';

  FIgnoreStrings := TStringList.Create;
  FSendedMessages := TStringList.Create;
  FErrPeriodMinute := 60*24;

  FMinutesLeft := 0;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 60000;  //1 мин
  FTimer.OnTimer := TimerOnTime;
  FTimer.Enabled := True;

  FLogLevel := LogLevel;

  LoadConfig(IniFileName);
end;

destructor TSmartMailer.Destroy;
begin
  FRecipientsAdmin.Free;
  FRecipientsOper.Free;
  FIgnoreStrings.Free;
  FSendedMessages.Free;
  FTimer.Free;
  inherited;
end;

procedure TSmartMailer.LoadConfig(IniFileName: String);
Var
  Ini: TIniFile;
  Path: String;
  Recip: TStringList;
  I: Integer;
begin
  Path:=ExtractFilePath(ParamStr(0));
  If Path[Length(Path)] <> '\' then Path:=Path + '\';


  FRecipientsAdmin.Clear;
  FRecipientsOper.Clear;

  If IniFileName = '' then IniFileName := Path + 'settings.ini';
  Ini := TIniFile.Create(IniFileName);
  try
    Recip := TStringList.Create;
    try
      Ini.ReadSectionValues('NOTIFICATION.Recipients', Recip);
      For I:=0 to Recip.Count - 1 do
        if AnsiSameText(Recip.Values[Recip.Names[I]], rctsAdmin) then FRecipientsAdmin.Add(Recip.Names[I])
        else FRecipientsOper.Add(Recip.Names[I]);
    finally
      Recip.Free;
    end;

    FErrPeriodMinute := IniReadInteger(Ini, 'NOTIFICATION', 'ErrPeriodMinute', FErrPeriodMinute);
    Ini.ReadSectionValues('NOTIFICATION.IgnoreMessStrings', FIgnoreStrings);
  finally
    Ini.Free;
  end;
end;

function TSmartMailer.NeedToSend(Body: String): Boolean;
Var
  I: Integer;
begin
  Result := False;

  For I:=0 to FIgnoreStrings.Count-1 do
    if AnsiContainsText(Body, FIgnoreStrings.Values[FIgnoreStrings.Names[I]]) then Exit;

  For I:=0 to FSendedMessages.Count-1 do
    if AnsiSameText(FSendedMessages[I], Body) then Exit;

  Result := True;
end;

function TSmartMailer.SendCommonMessage(RecipientType: TRecipType; Subj, Body: String; FileNameList: TStringList): boolean;
Var
  Res2: Boolean;
begin
  Result := False;
  try
    if RecipientType in [rctAdmin, rctAll] then Result := SendMessage(FRecipientsAdmin.DelimitedText, Subj, Body, FileNameList);
    if RecipientType in [rctOperator, rctAll] then Res2 := SendMessage(FRecipientsOper.DelimitedText, Subj, Body, FileNameList);
    Result := Result or Res2;
  except
  end;
end;

function TSmartMailer.SendErrMessage(RecipientType: TRecipType; Subj, Body: String; FileNameList: TStringList): boolean;
begin
  Result := False;
  If NeedToSend(Body) then Result := SendCommonMessage(RecipientType, Subj, Body, FileNameList);
  if Result then FSendedMessages.AddObject(Body, Pointer(FMinutesLeft));  //сохраняем оправленное сообщение и кол-во прошедших минут с запуска программы
end;

procedure TSmartMailer.TimerOnTime(Sender: TObject);
Var
  I: Integer;
begin
  Inc(FMinutesLeft);

  try
    For I:=FSendedMessages.Count-1 downto 0 do
      if ((FMinutesLeft - Integer(FSendedMessages.Objects[I])) mod FErrPeriodMinute) = 0 then FSendedMessages.Delete(I);
  except
  end;
end;

end.
