unit uXmlSysOption;

interface
uses windows, Classes, SysUtils, XmlDoc, XMLIntf;

{$M+}
type
  TParseXml = class(TObject)
  protected
    FXmldoc: TXMLDocument;
    FXmlNodeList: IXMLNodeList;
    FFileName: string;

    procedure SetChildValue(const AParentNodeName, AChildNodeName, AChildNodeValue: string; const AFlashBuf: Boolean = True);
    function GetChildValue(const AParentNodeName, AChildNodeName: string; const ADefaultValue: string = '';
      const AAutoCreate: Boolean = True): string;
  private
    FHeadNodeName: string;
    FXmlText: string;
    FAutoSave: Boolean;
    procedure SetFileName(const Value: string);
    function GetNodeCount: Integer;
    procedure SetXmlText(const Value: string);
    procedure ReBulidFile;
  public
    constructor Create(AOwnerComponent: TComponent; const AHeadName: string = 'Terry');
    destructor Destroy; override;

    procedure SaveToFile(const AFileName: string = '');
  published
    property FileName: string read FFileName write SetFileName;
    property XmlText: string read FXmlText write SetXmlText;
    property Count: Integer read GetNodeCount;
    property AutoSaveToFile: Boolean read FAutoSave write FAutoSave;
  end;

implementation

constructor TParseXml.Create(AOwnerComponent: TComponent; const AHeadName: string);
begin
  FXmldoc := TXMLDocument.Create(AOwnerComponent);
  if Trim(AHeadName) = '' then
    FHeadNodeName := 'Terry'
  else
    FHeadNodeName := AHeadName;
  FAutoSave := False;
end;

destructor TParseXml.Destroy;
begin
  if FAutoSave then
    SaveToFile;
  FXmldoc.Active := False;
  FXmldoc.Free;
  inherited;
end;

function TParseXml.GetChildValue(const AParentNodeName, AChildNodeName: string; const ADefaultValue: string;
  const AAutoCreate: Boolean): string;
var
  xmlNode: IXMLNode;
begin
  xmlNode := FXmlNodeList.FindNode(AParentNodeName);
  if xmlNode = nil then
  begin
    xmlNode := FXmldoc.DocumentElement.AddChild(AParentNodeName);
    xmlNode.AddChild(AChildNodeName).Text := ADefaultValue;
    Result := ADefaultValue;
  end
  else
  begin
    xmlNode := xmlNode.ChildNodes.FindNode(AChildNodeName);
    if (xmlNode = nil) and AAutoCreate then
    begin
      SetChildValue(AParentNodeName, AChildNodeName, ADefaultValue);
      Result := ADefaultValue
    end
    else
      Result := xmlNode.Text;
  end;
end;

function TParseXml.GetNodeCount: Integer;
begin
  Result := FXmldoc.DocumentElement.ChildNodes.Count;
end;

procedure TParseXml.ReBulidFile;
var
  fs: TStringList;
begin
  fs := TStringList.Create;
  try
    fs.Add('<?xml version="1.0" encoding="gb2312"?>');
    fs.Add('<' + FHeadNodeName + ' Version="1.0">');
    fs.Add('');
    fs.Add('</' + FHeadNodeName + '>');
    fs.SaveToFile(FFileName);
  finally
    fs.Free;
  end;
end;

procedure TParseXml.SaveToFile(const AFileName: string);
begin
  if AFileName = '' then
    FXmldoc.SaveToFile(FFileName)
  else
    FXmldoc.SaveToFile(AFileName);
end;

procedure TParseXml.SetChildValue(const AParentNodeName, AChildNodeName, AChildNodeValue: string; const AFlashBuf: Boolean);
var
  xmlNode, xmlTmp: IXMLNode;
  bChange: Boolean;
begin
  xmlNode := FXmlNodeList.FindNode(AParentNodeName);
  bChange := False;
  if xmlNode = nil then
  begin
    xmlNode := FXmldoc.DocumentElement.AddChild(AParentNodeName);
    xmlNode.AddChild(AChildNodeName).Text := AChildNodeValue;
    bChange := True;
  end
  else
  begin
    xmlTmp := xmlNode.ChildNodes.FindNode(AChildNodeName);
    if xmlTmp = nil then
    begin
      xmlNode.AddChild(AChildNodeName).Text := AChildNodeValue;
      bChange := True;
    end
    else if CompareText(AChildNodeValue, xmlTmp.Text) <> 0 then
    begin
      xmlTmp.Text := AChildNodeValue;
      bChange := True;
    end;
  end;

  if bChange and AFlashBuf and FileExists(FFileName) then
    SaveToFile;
end;

procedure TParseXml.SetFileName(const Value: string);
begin
  FFileName := Value;
  FXmldoc.FileName := FFileName;
  try
    FXmldoc.Active := True;
  except
    ReBulidFile;
    FXmldoc.Active := True;
  end;
  FXmlNodeList := FXmldoc.DocumentElement.ChildNodes;
  FHeadNodeName := FXmldoc.DocumentElement.NodeName;
end;

procedure TParseXml.SetXmlText(const Value: string);
begin
  FXmlText := Value;
  FXmldoc.XML.Text := FXmlText;
  FXmldoc.Active := True;
  FXmlNodeList := FXmldoc.DocumentElement.ChildNodes;
  FHeadNodeName := FXmldoc.DocumentElement.NodeName;
end;

end.
