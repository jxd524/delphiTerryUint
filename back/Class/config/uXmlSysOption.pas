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

    function FindSpecifyNode(const AParentNodeName: array of string; const AAutoCreate: Boolean): IXmlNode;
    function GetTopNodeChildCount(const ANodeName: string): Integer;
    function SetChildValue(const AParentNodeName: array of string; AChildNodeName, AChildNodeValue: string;
                            const AFlashBuf: Boolean = True; const AAutoCreate: Boolean = False): Boolean; overload;
    function GetChildValue(const AParentNodeName: array of string; AChildNodeName: string;
                           const ADefaultValue: string = ''; const AAutoCreate: Boolean = True): string; overload;
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
{$M-}
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

function TParseXml.FindSpecifyNode(const AParentNodeName: array of string; const AAutoCreate: Boolean): IXMLNode;
var
  i: Integer;
  xmlParentNode: IXMLNode;
  xmlNode: IXMLNode;
  xmlNodeList: IXMLNodeList;
begin
  xmlNodeList := FXmldoc.DocumentElement.ChildNodes;
  xmlParentNode := FXmldoc.DocumentElement;
  xmlNode := nil;
  for i := Low(AParentNodeName) to High(AParentNodeName) do
  begin
    xmlNode := xmlNodeList.FindNode(AParentNodeName[i]);
    if xmlNode = nil then
    begin
      if AAutoCreate then
        xmlNode := xmlParentNode.AddChild(AParentNodeName[i])
      else
        Break;
    end;
    xmlParentNode := xmlNode;
    xmlNodeList := xmlParentNode.ChildNodes;
  end;
  Result := xmlNode;
end;


function TParseXml.GetChildValue(const AParentNodeName: array of string; AChildNodeName: string;
  const ADefaultValue: string; const AAutoCreate: Boolean): string;
var
  xmlParentNode: IXMLNode;
  xmlNode: IXMLNode;
  xmlNodeList: IXMLNodeList;
begin
  Result := '';
  xmlNode := FindSpecifyNode(AParentNodeName, AAutoCreate);
  if xmlNode = nil then Exit;
  xmlParentNode := xmlNode;
  xmlNodeList := xmlNode.ChildNodes;
  xmlNode := xmlNodeList.FindNode(AChildNodeName);
  if xmlNode = nil then
  begin
    xmlNode := xmlParentNode.AddChild(AChildNodeName);
    xmlNode.Text := ADefaultValue;
    Result := ADefaultValue;
  end
  else
    Result := xmlNode.Text;
end;

function TParseXml.GetTopNodeChildCount(const ANodeName: string): Integer;
var
  Node: IXMLNode;
begin
  Node := FXmlNodeList.FindNode(ANodeName);
  if Node = nil then
    Result := 0
  else
    Result := Node.ChildNodes.Count;
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

function TParseXml.SetChildValue(const AParentNodeName: array of string; AChildNodeName,
  AChildNodeValue: string; const AFlashBuf, AAutoCreate: Boolean): Boolean;
var
  xmlParentNode: IXMLNode;
  xmlNode: IXMLNode;
  xmlNodeList: IXMLNodeList;
begin
  Result := False;
  xmlNode := FindSpecifyNode(AParentNodeName, AAutoCreate);
  if xmlNode = nil then Exit;
  xmlParentNode := xmlNode;
  xmlNodeList := xmlNode.ChildNodes;
  xmlNode := xmlNodeList.FindNode(AChildNodeName);
  if xmlNode = nil then
    xmlNode := xmlParentNode.AddChild(AChildNodeName);
  xmlNode.Text := AChildNodeValue;

  if AFlashBuf and FileExists(FFileName) then
    SaveToFile;
  Result := True;
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
  FXmlText := FXmldoc.XML.Text;
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
