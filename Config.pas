unit Config;

interface
Uses
  Classes,
  IniFiles;

type
  //класс для хранения параметров
  TConfig = class
  private
    FPath: String;  //путь к exe
    FIniFileName: String;

    FDBName: String;
    FDBUser: String;
    FDBPass: String;

    FdrpURL: String;
    FdrpUser: String;
    FdrpPass: String;
    FAutoSend: Integer;  //1 - отправлять данные автоматом или 0 - только сохранять файлы локально и не отправлять
    FXMLEncoding: String;

    FExportFolder: String;
    FImportFolder: String;

    FImportOrdersDaysAgo: Integer;

    FTransfersIni: String;  //ини-файл с настройками обменов

    FExportTimesFull: String;  //время полной выгрузки
    FExportTimesPartial: String;  //время частичной выгрузки
    FImportPeriod: Integer;  //период загрузки данных
    FImportToOCOStoredProc: String;  //имя процедуры для импорта заявок в OCO

    FLogLevel: integer;
    FStoreFileDays: integer;

    function PrepareFolder(Folder: String): String;
  protected
  public
    property Path: String read FPath;
    property IniFileName: String read FIniFileName;

    property DBName: String read FDBName;
    property DBUser: String read FDBUser;
    property DBPass: String read FDBPass;

    property drpURL: String read FdrpURL;
    property drpUser: String read FdrpUser;
    property drpPass: String read FdrpPass;
    property AutoSend: Integer read FAutoSend;
    property XMLEncoding: String read FXMLEncoding;

    property ExportFolder: String read FExportFolder;
    property ImportFolder: String read FImportFolder;
    property ImportOrdersDaysAgo: Integer read FImportOrdersDaysAgo;
    property TransfersIni: String read FTransfersIni;

    property ExportTimesFull: String read FExportTimesFull;
    property ExportTimesPartial: String read FExportTimesPartial;
    property ImportPeriod: Integer read FImportPeriod;
    property ImportToOCOStoredProc: String read FImportToOCOStoredProc;

    property LogLevel: integer read FLogLevel;
    property StoreFileDays: integer read FStoreFileDays;

    constructor Create(IniFileName: String{; const AOnWriteLog: TOnWriteLogEnvent = nil});
    procedure LoadConfig(IniFileName: String);  //из ини-файла приложения
  end;

implementation
Uses
  FileUtil,
  SysUtils;

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

{ TConfig }

constructor TConfig.Create(IniFileName: String);
begin
  LoadConfig(IniFileName);
end;

procedure TConfig.LoadConfig(IniFileName: String);
Var
  Ini : TIniFile;
begin
  FPath:=ExtractFilePath(ParamStr(0));
  If FPath[Length(FPath)] <> '\' then FPath:=FPath + '\';

  If IniFileName = '' then IniFileName := FPath + 'settings.ini';
  FIniFileName := IniFileName;
  Ini := TIniFile.Create(FIniFileName);
  try
    FDBName := IniReadString(Ini, 'DB', 'path', '');
    FDBUser := IniReadString(Ini, 'DB', 'user', 'SERVICE');
    FDBPass := IniReadString(Ini, 'DB', 'pass', '');

    FdrpURL := IniReadString(Ini, 'DRPSERVICE', 'URL', 'https://jeans.heineken.com/DISTR4/DRPService.asmx');
    FdrpUser := IniReadString(Ini, 'DRPSERVICE', 'User', '');
    FdrpPass := IniReadString(Ini, 'DRPSERVICE', 'Pass', '');

    FAutoSend := IniReadInteger(Ini, 'Export', 'AutoSend', 1);

    FXMLEncoding := IniReadString(Ini, 'Export', 'Encoding', 'utf-8');

    FExportFolder := IniReadString(Ini, 'Export', 'Folder', 'Export');
    FExportFolder := PrepareFolder(FExportFolder);

    FImportFolder := IniReadString(Ini, 'Import', 'Folder', 'Import');
    FImportFolder := PrepareFolder(FImportFolder);

    FTransfersIni := IniReadString(Ini, 'Export', 'TransfersIni', 'transfers.ini');
    if ExtractFilePath(FTransfersIni) = '' then FTransfersIni := FPath + FTransfersIni;

    FExportTimesFull := IniReadString(Ini, 'Export', 'TimesFull', '01:00');
    FExportTimesPartial := IniReadString(Ini, 'Export', 'TimesPartial', '');
    FImportPeriod := IniReadInteger(Ini, 'Import', 'Period', 15);
    FImportOrdersDaysAgo := IniReadInteger(Ini, 'Import', 'DaysAgo', 1);
    FImportToOCOStoredProc := IniReadString(Ini, 'Import', 'ToOcoStoredProc', '');

    FLogLevel := IniReadInteger(Ini, 'Files', 'LogLevel', 1);
    FStoreFileDays := IniReadInteger(Ini, 'Files', 'StoreDays', 20);
  finally
    Ini.Free;
  end;
end;

function TConfig.PrepareFolder(Folder: String): String;
begin
  if ExtractFilePath(Folder) = '' then Result := FPath + Folder
  else Result := Folder;
  If Result[Length(Result)] <> '\' then Result := Result + '\';
  If not DirectoryExists(Result) then CreateDir(Result);
end;

end.
