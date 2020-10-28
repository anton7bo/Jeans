unit XMLTransform;
//*22.01.20  Free

interface
Uses
  Classes,
  MSXML2_TLB;

type

  TXMLTRansform = class
  private
    FXMLFileName: String;  //�������� xml
    FTransformFileName: String;  //���� ������������� xls
    FResFileName: String;  //���� ���������� 
    FScript: TStringList;  //������ ����������
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
  if not FileExists(XMLFileName) then raise Exception.CreateFmt('������. �� ������ xml-���� %s. ����� %s.Create', [XMLFileName, Self.ClassName]);
  if not FileExists(TransformFileName) then raise Exception.CreateFmt('������. �� ������ xsl-���� %s. ����� %s.Create', [TransformFileName, Self.ClassName]);

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
//  if r = nil then raise exception.CreateFmt('������ � ����� (%s) - ��� ��������� ��������. ����� %s.XmlTransform', [FXMLFileName, Self.ClassName]);
  ParseError := FDoc.parseError;  //+18.02.20
  If (ParseError.errorCode <> 0) then raise exception.CreateFmt('������ � ����� %s %s (������� %d - %d). ����� %s.XmlTransform', [FXMLFileName, ParseError.reason, ParseError.Get_line, ParseError.Get_linepos, Self.ClassName]);  //+18.02.20

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
        Html.write(PSafeArray(TVarData(v).VArray));  //����� html ���� � ������ ������ ��� ���� SQL

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
