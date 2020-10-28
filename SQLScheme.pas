unit SQLScheme;

interface
Uses
  BaseRTTI,
  Classes,
  Contnrs,
  IniFiles;

type
{$M+}

  TSQLScheme = class(TBaseRTTI)  //схема получения данных из БД (целого обмена или части обмена когда есть вложенные данные типа мастер-детэйл)
  private
    FSQL      :String;  //запрос
    FStoredProc:String;  //имя хранимой процедуры
    FDetails    :String;   //список имен вложенных TSQLScheme
  protected  
    FFldAliases: TStringList;  //список переименования имен полей
  public
    constructor Load(Name: String; Ini: TIniFile);
    destructor Destroy; override;
  published
    property SQL      :String read FSQL write FSQL;
    property StoredProc:String read FStoredProc write FStoredProc;
    property Details: String read FDetails write FDetails;
    property FldAliases: TStringList read FFldAliases;
  end;

  TSQLSchemeList = class (TObjectList)
  protected
    procedure Put(Index: Integer; Item: TSQLScheme);
    function Get(Index: Integer): TSQLScheme;
  public
    property Items[Index: Integer]: TSQLScheme read Get write Put; default;
    function Add(Obj: TSQLScheme): Integer;
    procedure Insert(Index: Integer; Obj: TSQLScheme);
    procedure LoadFromIni(IniFileName: String);
    function IndexOf(Name: String): TSQLScheme;
  end;

implementation
Uses
  SysUtils,
  StrUtils;

{ TSQLScheme }

constructor TSQLScheme.Load(Name: String; Ini: TIniFile);
begin
  inherited Create(Name);
  FFldAliases := TStringList.Create;
  LoadPropertiesFromINI(Ini);
end;

destructor TSQLScheme.Destroy;
begin
  FFldAliases.Free;
end;


{ TSQLSchemeList }

function TSQLSchemeList.Add(Obj: TSQLScheme): Integer;
begin
  Result := inherited Add(Obj);
end;

function TSQLSchemeList.Get(Index: Integer): TSQLScheme;
begin
  if Count-1 >= Index then Result := TSQLScheme(inherited Items[Index])
  else Result := nil;
end;

function TSQLSchemeList.IndexOf(Name: String): TSQLScheme;
Var
  I: Integer;
begin
  Result := nil;
  For I := 0 to Count - 1 do
    if (AnsiSameText(Name, Items[I].Name)) {or (AnsiSameText(Format('%s%s', [sScheme, Name]), Items[I].Name) = 0)} then
    begin
      Result := Items[I];
      Break;
    end;
end;

procedure TSQLSchemeList.Insert(Index: Integer; Obj: TSQLScheme);
begin
  inherited Insert(Index, Obj);
end;

procedure TSQLSchemeList.LoadFromIni(IniFileName: String);
Var
  Ini: TIniFile;
  I, J: Integer;
  Ind: Integer;
  SectionList: TStringList;
  SchemeFldList: TStringList;
  TransferScheme : TSQLScheme;
begin
  If not FileExists(IniFileName) then raise Exception.CreateFmt('Не найден файл с настройками обменов %s', [IniFileName]);

  Ini := TIniFile.Create(IniFileName);
  SectionList := TStringList.Create;
  try
    Ini.ReadSections(SectionList);
    For I:=SectionList.Count-1 downto 0 do
      If SectionList.Count > I then
        If not ContainsText(SectionList[I], sScheme) then SectionList.Delete(I);  //оставим только схемы (это то из чего состоят обмены)

    For I:=0 to SectionList.Count-1 do
    begin
      TransferScheme := TSQLScheme.Load(SectionList[I], Ini);
      Add(TransferScheme);

      Ini.ReadSectionValues(SectionList[I], TransferScheme.FFldAliases);

      SchemeFldList := GetClassProperties(TransferScheme);
      If SchemeFldList <> nil then
      begin
        For J := 0 to SchemeFldList.Count - 1 do
        begin
          Ind := TransferScheme.FFldAliases.IndexOfName(SchemeFldList[J]);
          if Ind > -1 then TransferScheme.FFldAliases.Delete(Ind);
        end;
      end;
    end;
//ПРОВЕРКА НА ЗАПОЛНЕННОСТЬ НЕОБХОДИМЫХ ПОЛЕЙ!!!
  finally
    SectionList.Free;
    Ini.Free;
  end;
end;

procedure TSQLSchemeList.Put(Index: Integer; Item: TSQLScheme);
begin
  inherited Items[Index] := Item;
end;

end.
