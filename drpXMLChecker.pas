unit DRPXMLChecker;

interface
uses
  Classes,
  MSXML2_TLB;

type
  TDRPXMLChecker = class
  private
    FFileName: String;
    FErrorID: Integer;
    FSeverity: Integer;
    FErrorList: TStringList;
    FErrForData: TStringList;
    FProcessingType: String;
    function ExtractError(Element: IXMLDOMNode): Integer;
    procedure SetFileName(FileName: String);
  public
    property FileName: String read FFileName write SetFileName;
    property ErrorID: Integer read FErrorID;
    property Severity: Integer read FSeverity;
    property ErrorList: TStringList read FErrorList;
    property ErrForData: TStringList read FErrForData;
    property ProcessingType: String read FProcessingType;

    constructor Create(FileName: String);
    destructor Free;

    procedure CheckFileForError;
  end;

implementation
uses
  SysUtils;

{ TDRPXMLChecker }

constructor TDRPXMLChecker.Create(FileName: String);
begin
  FFileName := FileName;
  FErrorList := TStringList.Create;
  FErrForData := TStringList.Create;
end;

procedure TDRPXMLChecker.CheckFileForError;
Var
  Doc: DomDocument;
  r: IXMLDOMElement;
  ReflectData: IXMLDOMNode;
  ErrList: IXMLDomNodeList;
  I: Integer;
  PrevSibling: IXMLDOMNode;
  ResObjChildren: IXMLDomNodeList;
  ProcType: IXMLDOMNode;
begin
  If not FileExists(FFileName) then raise Exception.CreateFmt('Не найден файл %s', [FFileName]);

  FErrorID := 0;
  FSeverity := 0;
  FErrorList.Clear;
  FErrForData.Clear;
  FProcessingType := '';

  Doc:=CoDOMDocument.Create;
  try
    Doc.Set_async(false);
    Doc.load(FFileName);

    if Doc = nil then raise Exception.CreateFmt('Не удалось открыть файл %s', [FFileName]);

    r := Doc.Get_documentElement;
    if r = nil then raise exception.CreateFmt('Ошибка в файле %s - нет корневого элемента', [FFileName]);

    ProcType := r.selectSingleNode('ProcessingType');
    If ProcType <> nil then FProcessingType := ProcType.Text;

    FErrorID := ExtractError(r);

    If FErrorID <> 0 then
    begin
      ReflectData:=r.selectSingleNode('ReflectData');
      If ReflectData <> nil then
      begin
        ErrList := ReflectData.selectNodes('ErrorList');
        If (ErrList = nil) or (ErrList.length = 0) then ErrList := ReflectData.selectNodes('*/ErrorList');

        For I:=0 to ErrList.length - 1 do
        begin
          ExtractError(ErrList[I]);

          PrevSibling := ErrList[I].previousSibling;
          While PrevSibling <> nil do
          begin
            FErrForData.Add(PrevSibling.xml);
            PrevSibling := PrevSibling.previousSibling;
          end;
        end;
      end;
    end
    else
    begin
      If FProcessingType = 'Readonly' then  //это запрос данных (ТТ, товар, склады, заявки)
      begin
        ResObjChildren := r.selectNodes('ResultObject/*');
        If (ResObjChildren = nil) or (ResObjChildren.length = 0) then FErrorID := -1;  //Файл не содержит данных
      end
    end
  finally
    Doc:=nil;
  end;
end;

destructor TDRPXMLChecker.Free;
begin
  FErrForData.Free;
  FErrorList.Free;
end;

procedure TDRPXMLChecker.SetFileName(FileName: String);
begin
  If not FileExists(FileName) then Exception.CreateFmt('Не найден файл %s', [FileName]);
  FFileName := FileName;

  FErrorID := 0;
  FSeverity := 0;
  FErrorList.Clear;
  FErrForData.Clear;
  FProcessingType := '';
end;

function TDRPXMLChecker.ExtractError(Element: IXMLDOMNode): Integer;
Var
  Node: IXMLDOMNode;

  function GetValue(Name: String): Integer;
  Var
    Node: IXMLDOMNode;
  begin
    Node := Element.selectSingleNode(Name);
    If (Node = nil) then Node := Element.selectSingleNode(Format('Error/%s', [Name]));
    If (Node <> nil) then
    try
      Result := StrToInt(Node.text);
    except
      Result:=0;
    end
    else Result:=0;
  end;

begin
  Result := GetValue('ErrorID');
  If FSeverity = 0 then FSeverity := GetValue('Severity');

  Node:=Element.selectSingleNode('ErrorMessage');
  If (Node = nil) then Node := Element.selectSingleNode('Error/ErrorMessage');
  If (Node <> nil) then FErrorList.Add(Node.text);
end;

end.
