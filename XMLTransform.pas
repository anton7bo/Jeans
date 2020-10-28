unit XMLTransform;
//*22.01.20  Free

interface
Uses
  Classes,
  MSXML2_TLB;

type

  TXMLTRansform = class
  private
    FXMLFileName: String;  //исходный xml
    FTransformFileName: String;  //файл трансформации xls
    FResFileName: String;  //файл результата 
    FScript: TStringList;  //строки результата
    FDoc: IXMLDomDocument;

    function XmlTransform:boolean;
  public
    constructor Create(XMLFileName, TransformFileName: String);
    destructor Destroy; override;
    property ResFileName: String read FResFileName;
    property Script: TStringList read FScript;
  end;

implementation
Uses
  ActiveX,
  MSHTML,
  SysUtils,
  Variants;


{ TXMLTRansform }

constructor TXMLTRansform.Create(XMLFileName, TransformFileName: String);
begin
  if not FileExists(XMLFileName) then raise Exception.CreateFmt('ОШИБКА. Не найден xml-файл %s. Метод %s.Create', [XMLFileName, Self.ClassName]);
  if not FileExists(TransformFileName) then raise Exception.CreateFmt('ОШИБКА. Не найден xsl-файл %s. Метод %s.Create', [TransformFileName, Self.ClassName]);

  FXMLFileName := XMLFileName;
  FTransformFileName := TransformFileName;

  FScript:=TStringList.Create;
  FDoc:=CoDOMDocument.Create;

  XmlTransform;
end;

destructor TXMLTRansform.Destroy;
begin
  FScript.Free;
  FDoc := nil;  //+22.01.20
end;

function TXMLTRansform.XmlTransform: boolean;
Var
  XSLDoc, ResDoc : IXMLDomDocument;
  r: IXMLDOMElement;
  Html: IHTMLDocument2;
  V: OleVariant;
  DocType: Variant;
  PathIn: String;
  ParseError: IXMLDOMParseError;
begin
  Result := False;

  FDoc.Set_async(false);
  FDoc.load(FXMLFileName);

//  r:=FDoc.Get_documentElement;
//  if r = nil then raise exception.CreateFmt('Ошибка в файле (%s) - нет корневого элемента. Метод %s.XmlTransform', [FXMLFileName, Self.ClassName]);
  ParseError := FDoc.parseError;  //+18.02.20
  If (ParseError.errorCode <> 0) then raise exception.CreateFmt('Ошибка в файле %s %s (позиция %d - %d). Метод %s.XmlTransform', [FXMLFileName, ParseError.reason, ParseError.Get_line, ParseError.Get_linepos, Self.ClassName]);  //+18.02.20

  XSLDoc := CoDOMDocument.Create;
  try
    XSLDoc.Set_async(false);
    XSLDoc.load(FTransformFileName);

    ResDoc := CoDOMDocument.Create;
    try
      ResDoc.Set_async(false);
      ResDoc.validateOnParse:=True;

      FDoc.transformNodeToObject(XSLDoc, ResDoc);
//        ResDoc.save('C:\Work19\MercuryTemplate\XSL\test.sql');

      Html := coHTMLDocument.Create as IHTMLDocument2;
      try
        V := VarArrayCreate([0, 0], varVariant);
        V[0]:=ResDoc.xml;
        Html.write(PSafeArray(TVarData(v).VArray));  //через html чтоб в каждой строке был один SQL

        If html.Body <> nil then
        begin
          Script.Text := html.Body.innerText;

          FResFileName := Format('%s%s', [ExtractFilePath(FXMLFileName), ChangeFileExt(ExtractFileName(FXMLFileName), '.sql')]);
          Script.SaveToFile(FResFileName);
          Result := True;
        end;
      finally
        Html := nil;
      end;

    finally
      ResDoc := nil;
    end;
  finally
    XSLDoc := nil;
  end;
end;

end.
