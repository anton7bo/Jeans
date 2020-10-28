unit BaseRTTI;

interface
{$M+}

Uses
  Classes,
  IniFiles;

type
  TBaseRTTI = class(TPersistent)
  private
    FName: String;
    function TrySetProperty(Key, Value: String): Boolean;
  public
    constructor Create(AName: String);
    procedure LoadPropertiesFromIni(Ini: TiniFile);
  published
    property Name: String read FName write FName;
  end;

const
  sScheme = 'scheme.';

  function GetClassProperties(AObject: TObject): TStringList;
  function IniReadString(Ini: TIniFile; Section, Ident, Default: String):String;
  function IniReadInteger(Ini: TIniFile; Section, Ident: String; Default: Longint):Longint;

implementation
Uses
//Dialogs,
  SysUtils,
  TypInfo;

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

function GetClassProperties(AObject: TObject): TStringList;
var
  i, iCount: integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
begin
  If AObject.ClassInfo = nil then Exit;
  iCount := GetTypeData(AObject.ClassInfo)^.PropCount;  // получаем количество публичных свойств объекта

  if iCount > 0 then
  begin
    Result := TStringList.Create;

    GetMem(PropList, iCount * SizeOf(Pointer));      // запрашиваем кусочек памяти для хранения списка свойств
    try
      GetPropInfos(AObject.ClassInfo, PropList);  // и получаем их в PropList
      for i := 0 to iCount - 1 do  // пробегаемся по списку свойств
      begin
        PropInfo := PropList^[i];
        if PropInfo = nil then break;

        Result.Add(PropInfo.Name);
      end;
    finally
      FreeMem(PropList, iCount * SizeOf(Pointer));  // и в самом конце освобождаем занятую списком память
    end;
  end;
end;

{TBaseRTTI}

constructor TBaseRTTI.Create(AName: String);
begin
  FName := AName;
end;

procedure TBaseRTTI.LoadPropertiesFromIni(Ini: TiniFile);
Var
  I: Integer;
  ValuesList: TStringList;
begin
  ValuesList := TStringList.Create;
  try
    Ini.ReadSectionValues(Self.FName, ValuesList);
    For I := 0 to ValuesList.Count - 1 do
    begin
      TrySetProperty(ValuesList.Names[I], ValuesList.Values[ValuesList.Names[I]]);
    end
  finally
    ValuesList.Free;
  end;
end;

function TBaseRTTI.TrySetProperty(Key, Value: String): Boolean;
var
  TypeData:PTypeData;
  PropList:PPropList;
  i, PropCount:Integer;
  PropVal: Variant;
  Ind: Integer;
  NewKey: String;
  VarValue: Variant;
begin
  If Self.ClassInfo = nil then Exit;

  TypeData:=GetTypeData(Self.ClassInfo);
  PropCount := TypeData.PropCount;
  if TypeData.PropCount <= 0 then exit;

  GetMem(PropList, SizeOf(PPropInfo)*PropCount);
  try
    GetPropInfos(TObject(TypeData).ClassInfo, PropList);
    for i:=0 to PropCount-1 do
    begin
      If PropList[I].PropType^.Kind = tkClass then Continue;

      if PropList[I].Name[1] = '_' then NewKey := '_' + Key
      else NewKey := Key;

      if (CompareText(PropList[I].Name, NewKey) = 0) and (Value > '')then
      begin
        try
          VarValue := Value;
          SetPropValue(Self, PropList[I].Name, VarValue);
        except
//          on E: Exception do
//            ShowMessage(Format('Не удалось установить значение свойства %s в %s (нет разрешения на запись / нет сеттера). %s', [PropList[I].Name, Value, E.Message]));
        end;
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

end.
