unit LogEngine;

interface
Uses
  SysUtils;

type
//  TOnWriteLogEnvent = procedure (Mess: String; const Indent: Integer = 0; const WithTime: boolean = True) of object;  //перенести в целевой модуль

  TLogEngine = class
    FLogFileName: string;
    FLogErrFileName: string;
    FLogActive: Boolean;
    FLogErrActive: Boolean;
    FLogH: TextFile;
    FLogErrH: TextFile;
    FLogLevel: Integer;  //0-не писать логи, 1-обычные логи, 2-

    procedure OpenLogFiles;
    procedure CloseLogFiles;
//    procedure DeleteOldFiles(Path:String; const FilesStoreDays: integer = 30);  //+28.03.11
    procedure GetHost(Var HostName, Addr: String);
  public
    property LogFileName: String read FLogFileName write FLogFileName;
    property LogErrFileName: String read FLogErrFileName write FLogErrFileName;

    constructor Create(APath: String; const ALogLevel: Integer = 1; const ALogFileName: TFileName = ''; const ALogErrFileName: TFileName = '');
    destructor Destroy; override;
    procedure WriteLog(Mess: String; const Indent: Integer = 1; const WithTime: boolean = True);
    procedure NewLogFiles;
  end;

implementation
Uses
  FileUtil,
  StrUtils,
  Windows,
  WinSock;

constructor TLogEngine.Create(APath: String; const ALogLevel: Integer = 1; const ALogFileName: TFileName = ''; const ALogErrFileName: TFileName = '');
Var
  Path: String;
  HostName: String;
  Addr:String;
begin
  if APath > '' then Path := APath
  else Path := ExtractFilePath(ParamStr(0));
  If RightStr(Path, 1) <> '\' then Path := Path + '\';
  If not DirectoryExists(Path) then CreateDir(Path);

  If ALogFileName > '' then FLogFileName := ExtractFileName(ALogFileName);
  If ALogErrFileName > '' then FLogErrFileName := ExtractFileName(ALogErrFileName);

  If FLogFileName = '' then
  begin
    GetHost(HostName, Addr);
    If HostName = '' then FLogFileName := Format('%s\%s', [Path, ChangeFileExt(ExtractFileName(ParamStr(0)), '.log')])
    else FLogFileName := Format('%s\%s_%s.log', [Path, ChangeFileExt(ExtractFileName(ParamStr(0)), ''), HostName]);
  end;
  If FLogErrFileName = '' then FLogErrFileName := Format('%s\%s_err.log', [Path, ChangeFileExt(ExtractFileName(FLogFileName), '')]);

  FLogLevel := ALogLevel;
  If not (FLogLevel in [0,1,2]) then FLogLevel := 1;

  if FLogLevel = 0 then Exit;  //+17.09.19
  OpenLogFiles;
  WriteLog('-----------------------------', 0, False);
  WriteLog('Старт', 0);
end;

destructor TLogEngine.Destroy;
begin
  if FLogLevel = 0 then Exit;
  try
    WriteLog('Стоп', 0);
    WriteLog('-----------------------------', 0, False);
    CloseLogFiles;
  except
  end;
end;

procedure TLogEngine.GetHost(Var HostName, Addr: String);
Var
  Name:array[0..255] of Char;//PChar;
  Error: DWORD;
  HostEntry: PHostEnt;
  Data: WSAData;
  Address: In_Addr;
begin
  Addr:='-1';
  HostName:='Unknown';
  Error:=WSAStartup(MakeWord(1, 1), Data);
  try
    if Error = 0 then
    begin
      If GetHostName(Name, SizeOf(Name)) = 0 then
      begin
        HostName:=Name;
        HostEntry:=WinSock.gethostbyname(Name);
        Error:=GetLastError();
        if Error = 0 then
        begin
          Address:=PInAddr(HostEntry^.h_addr_list^)^;
          Addr:=inet_ntoa(Address);
        end
        else Addr:='127.0.0.1';
      end
      else Addr:='127.0.0.1';
    end;
  finally
    WSACleanup();
  end;
end;

procedure TLogEngine.OpenLogFiles;
begin
//  if FLogLevel = 0 then Exit;  -17.09.19

  FLogActive:=False;
  try
    try
      AssignFile(FLogH, FLogFileName);
      If FileExists(FLogFileName) then Append(FLogH)
      else Rewrite(FLogH);

      AssignFile(FLogErrH, FLogErrFileName);
      If FileExists(FLogErrFileName) then Append(FLogErrH)
      else Rewrite(FLogErrH);
    finally
      FLogActive:=True;
    end;
  except
  end;
end;

procedure TLogEngine.CloseLogFiles;
begin
//  if FLogLevel = 0 then Exit;  -17.09.19

  If FLogActive then
  try
    FLogActive:=False;
    Flush(FLogH);
    CloseFile(FLogH);
    Flush(FLogErrH);
    CloseFile(FLogErrH);
  except
  end;
end;

procedure TLogEngine.WriteLog(Mess: String; const Indent: Integer = 1; const WithTime: boolean = True);
Var
  Spaces: String;
begin
  if FLogLevel = 0 then Exit;

  try
    Spaces := StringOfChar(' ', Indent*2);
    if WithTime then Mess := Format('%s %s%s', [DateTimeToStr(Now), Spaces, Mess])
    else Mess := Spaces + Mess;
    Writeln(FLogH, Mess);
    Flush(FLogH);

    If ((AnsiContainsText(Mess, 'ОШИБКА')) or (AnsiContainsText(Mess, 'WARNING')) or (AnsiContainsText(Mess, 'ERROR'))) then
    begin
      Writeln(FLogErrH, Mess);
      Flush(FLogErrH);
    end;
  except
  end;
end;

procedure TLogEngine.NewLogFiles;
Var
  Year, Month, Day: Word;
  NewName: String;
  Path: String;
begin
  try
    try
      CloseLogFiles;
      Path := ExtractFilePath(FLogFileName)+'\Log\';
      If not DirectoryExists(Path) then CreateDir(Path);
      DecodeDate(Date, Year, Month, Day);
      NewName:=Format('%s%0.2d%0.2d%0.2d%s', [Path, Year, Month, Day, ExtractFileName(FLogFileName)]);
      FileUtil.MoveFile(FLogFileName, NewName);

      NewName:=Format('%s%0.2d%0.2d%0.2d%s', [Path, Year, Month, Day, ExtractFileName(FLogErrFileName)]);
      If FileUtil.GetFileSize(FLogErrFileName) > 0 then FileUtil.MoveFile(FLogErrFileName, NewName);
    finally
      OpenLogFiles;
    end;
  except
  end;
end;

end.
