unit uJxdSystemConfig;

interface
uses windows, Classes, SysUtils, Controls, XmlDoc, XMLIntf;

{$M+}
type
  PSysParamInfo = ^TSysParamInfo;
  TSysParamInfo = record
    FName: string;
    FValue: string;
    FNext: PSysParamInfo;
  end;
  PSysNode1Info = ^TSysNode1Info;
  TSysNode1Info = record
    FName: string;
    FNodeAttInfos: PSysParamInfo;
  end;
  PSysNode2Info = ^TSysNode2Info;
  TSysNode2Info = record
    FName: string;
    FNodeList: TList; //PSysNode1Info
  end;

  TOnParse = procedure(const ANode: IXMLNode) of object;
  TxdSystemConfig = class
  public
    constructor Create(const AOwner: TWinControl); virtual;
    destructor  Destroy; override;
    procedure LoadFormFile(const AWinCtrl: TWinControl; const AFileName: string);
    procedure SaveToFile(const AWinCtrl: TWinControl; const AFileName: string);

    procedure AddParam(const ANode, AName, AValue: string); overload;
    procedure AddParam(const ANode, AName: string; const AValue: Integer); overload;
    procedure AddParam(const AParentNode, ANode, AName, AValue: string); overload;
    procedure AddParam(const AParentNode, ANode, AName: string; const AValue: Integer);overload; 

    function  GetParamValue(const ANode, AName: string): string; overload;
    function  GetParamValue(const AParentNode, ANode, AName: string): string; overload;
    function  GetParamValueAsInteger(const ANode, AName: string; const ADefaulValue: Integer): Integer; overload;
    function  GetParamValueAsInteger(const AParentNode, ANode, AName: string; const ADefaulValue: Integer): Integer; overload;
        
    function  Count(const AName: string): Integer;    
  protected
    FFileName: string;
    procedure InitConfig; virtual;
    function  FindNode1Info(const Alt: TList; const AName: string): PSysNode1Info;
    function  FindNode2Info(const AName: string): PSysNode2Info;
    procedure AddSysParamInfo(var Ap: PSysParamInfo; const AName, AValue: string);
    function  FindSysParamInfo(Ap: PSysParamInfo; const AName: string): string;
    procedure FreeSysParamInfos(var Ap: PSysParamInfo);

    class function  GetAttribute(const ANode: IXMLNode; const AName: string; var AValue: string): Boolean; inline;
    class function  AttributeToBoolean(const ANode: IXMLNode; const AName: string; const ADefault: Boolean): Boolean;
    class function  AttributeToInteger(const ANode: IXMLNode; const AName: string; const ADefault: Integer): Integer;
    class function  GetValues(const ANode: IXMLNode; const ANames: array of string; var AValus: array of string): Boolean;
  private
    FParam1List: TList;
    FParam2List: TList;

    FOwner: TWinControl;

    procedure ParseBigStyle(const ANode: IXMLNode; AParse: TOnParse);
    procedure ParseParam1(const ANode: IXMLNode);
    procedure AddAtt(const ANode: IXMLNode; var Ap: PSysParamInfo); overload;
    procedure ParseParam2(const ANode: IXMLNode);

    procedure SaveNode1Info(const AList: TList; const ANode: IXMLNode);
    procedure SaveParam1(const ANode: IXMLNode);
    procedure SaveParam2(const ANode: IXMLNode);
    procedure SetFileName(const Value: string);
  public
    property FileName: string read FFileName write SetFileName;
  end;

implementation

{ TxdSystemConfig }
const
  CtSeccion_Param1 = 'Param1';
  CtSeccion_Param2 = 'Param2';

procedure TxdSystemConfig.AddAtt(const ANode: IXMLNode; var Ap: PSysParamInfo);
var
  i: Integer;
  n: IXMLNode;
  strName, strValue: string;
begin
  for i := 0 to ANode.AttributeNodes.Count - 1 do
  begin
    n := ANode.AttributeNodes.Get( i );
    try
      strName := n.NodeName;
      strValue := n.NodeValue;
    except
      strValue := '';
    end;
    AddSysParamInfo( Ap, strName, strValue );
    n := nil;
  end;
end;

procedure TxdSystemConfig.AddSysParamInfo(var Ap: PSysParamInfo; const AName, AValue: string);
var
  p: PSysParamInfo;
begin
  if Ap = nil then
  begin
    New( Ap );
    Ap^.FName := AName;
    Ap^.FValue := AValue;
    Ap^.FNext := nil;
    Exit;
  end;

  p := Ap;
  while p <> nil do
  begin
    if CompareText(AName, p^.FName) = 0 then
    begin
      p^.FValue := AValue;
      Exit;
    end;
    if p^.FNext = nil then Break;
    p := p^.FNext;
  end;
  New( p^.FNext );
  p^.FNext^.FName := AName;
  p^.FNext^.FValue := AValue;
  p^.FNext^.FNext := nil;
end;

procedure TxdSystemConfig.AddParam(const AParentNode, ANode, AName, AValue: string);
var
  p: PSysNode2Info;
  p1: PSysNode1Info;
begin
  p := FindNode2Info( AParentNode );
  if p = nil then
  begin
    New( p );
    p^.FName := AParentNode;
    p^.FNodeList := TList.Create;
    FParam2List.Add( p );
  end;

  p1 := FindNode1Info( p^.FNodeList, ANode );
  if p1 = nil then
  begin
    New( p1 );
    p1^.FName := ANode;
    p1^.FNodeAttInfos := nil;
    p^.FNodeList.Add( p1 );
  end;
  AddSysParamInfo( p1^.FNodeAttInfos, AName, AValue );
end;

procedure TxdSystemConfig.AddParam(const ANode, AName: string; const AValue: Integer);
begin
  AddParam( ANode, AName, IntToStr(AValue) );
end;

class function TxdSystemConfig.AttributeToBoolean(const ANode: IXMLNode; const AName: string;
  const ADefault: Boolean): Boolean;
var
  strTemp: string;
begin
  if GetAttribute(ANode, AName, strTemp) then
    Result := StrToIntDef( strTemp, Integer(ADefault) ) <> 0
  else
    Result := ADefault;
end;

class function TxdSystemConfig.AttributeToInteger(const ANode: IXMLNode; const AName: string;
  const ADefault: Integer): Integer;
var
  strTemp: string;
begin
  if GetAttribute(ANode, AName, strTemp) then
    Result := StrToIntDef( strTemp, ADefault )
  else
    Result := ADefault;
end;

procedure TxdSystemConfig.AddParam(const ANode, AName, AValue: string);
var
  p: PSysNode1Info;
begin
  p := FindNode1Info( FParam1List, ANode );
  if p = nil then
  begin
    New( p );
    p^.FName := ANode;
    p^.FNodeAttInfos := nil;
    FParam1List.Add( p );
  end;
  AddSysParamInfo( p^.FNodeAttInfos, AName, AValue );
end;

function TxdSystemConfig.Count(const AName: string): Integer;
var
  p: PSysNode2Info;
begin
  p := FindNode2Info( AName );
  if Assigned(p) then
    Result := p^.FNodeList.Count
  else
    Result := 0;
end;

constructor TxdSystemConfig.Create(const AOwner: TWinControl);
begin
  FParam1List := TList.Create;
  FParam2List := TList.Create;
  FOwner := AOwner;
  if FFileName = '' then  
    FFileName := ExtractFilePath(ParamStr(0)) + 'SysConfig.xml';
  LoadFormFile( FOwner, FFileName );
end;

destructor TxdSystemConfig.Destroy;
var
  i, j: Integer;
  p1: PSysNode1Info;
  p2: PSysNode2Info;
begin
  SaveToFile( FOwner, FFileName );
  for i := 0 to FParam1List.Count - 1 do
  begin
    p1 := FParam1List[i];
    FreeSysParamInfos( p1^.FNodeAttInfos );
    Dispose( p1 );
  end;
  for i := 0 to FParam2List.Count - 1 do
  begin
    p2 := FParam2List[i];
    for j := 0 to p2^.FNodeList.Count - 1 do
    begin
      p1 := p2^.FNodeList[j];
      FreeSysParamInfos( p1^.FNodeAttInfos );
      Dispose( p1 );
    end;
    p2^.FNodeList.Free;
    Dispose( p2 );
  end;
  inherited;
end;

function TxdSystemConfig.FindNode1Info(const Alt: TList; const AName: string): PSysNode1Info;
var
  i: Integer;
  p: PSysNode1Info;
begin
  Result := nil;
  for i := 0 to Alt.Count - 1 do
  begin
    p := Alt[i];
    if CompareText(p^.FName, AName) = 0 then
    begin
      Result := p;
      Break;
    end;
  end;
end;

function TxdSystemConfig.FindNode2Info(const AName: string): PSysNode2Info;
var
  i: Integer;
  p: PSysNode2Info;
begin
  Result := nil;
  for i := 0 to FParam2List.Count - 1 do
  begin
    p := FParam2List[i];
    if CompareText(p^.FName, AName) = 0 then
    begin
      Result := p;
      Break;
    end;
  end;
end;

function TxdSystemConfig.FindSysParamInfo(Ap: PSysParamInfo; const AName: string): string;
begin
  Result := '';
  while Assigned(Ap) do
  begin
    if CompareText(Ap^.FName, AName) = 0 then
    begin
      Result := Ap^.FValue;
      Break;
    end;
    Ap := Ap^.FNext;
  end;
end;

procedure TxdSystemConfig.FreeSysParamInfos(var Ap: PSysParamInfo);
var
  p: PSysParamInfo;
begin
  while Ap <> nil do
  begin
    p := Ap^.FNext;
    Dispose( Ap );
    Ap := p;
  end;
end;

function TxdSystemConfig.GetParamValue(const ANode, AName: string): string;
var
  p: PSysNode1Info;
begin
  p := FindNode1Info( FParam1List, ANode );
  if Assigned(p) then
    Result := FindSysParamInfo( p^.FNodeAttInfos, AName )
  else
    Result := '';
end;

class function TxdSystemConfig.GetAttribute(const ANode: IXMLNode; const AName: string; var AValue: string): Boolean;
begin
  try
    AValue := ANode.Attributes[AName];
    Result := True;
  except
    Result := False;
    AValue := '';
  end;
end;

function TxdSystemConfig.GetParamValue(const AParentNode, ANode, AName: string): string;
var
  p: PSysNode2Info;
  p1: PSysNode1Info;
begin
  Result := '';
  p := FindNode2Info( AParentNode );
  if Assigned(p) then
  begin
    p1 := FindNode1Info( p^.FNodeList, ANode );
    if Assigned(p1) then
      Result := FindSysParamInfo( p1^.FNodeAttInfos, AName )
  end;
end;

function TxdSystemConfig.GetParamValueAsInteger(const ANode, AName: string; const ADefaulValue: Integer): Integer;
begin
  Result := StrToIntDef( GetParamValue(ANode, AName), ADefaulValue );
end;

function TxdSystemConfig.GetParamValueAsInteger(const AParentNode, ANode, AName: string; const ADefaulValue: Integer): Integer;
begin
  Result := StrToIntDef( GetParamValue(AParentNode, ANode, AName), ADefaulValue );
end;

class function TxdSystemConfig.GetValues(const ANode: IXMLNode; const ANames: array of string;
  var AValus: array of string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(ANames) to High(ANames) do
  begin
    if GetAttribute(ANode, ANames[i], AValus[i]) then
    begin
      if not Result then
        Result := True;
    end;
  end;
end;

procedure TxdSystemConfig.InitConfig;
begin

end;

procedure TxdSystemConfig.LoadFormFile(const AWinCtrl: TWinControl; const AFileName: string);
var
  Xmldoc: TXMLDocument;
  NodeList: IXMLNodeList;
  Node: IXMLNode;
  i, nCount: Integer;
  GatherName: string;
begin
  if FileExists(AFileName) then
  begin
    Xmldoc := TXMLDocument.Create( AWinCtrl );
    try
      Xmldoc.FileName := AFileName;
      Xmldoc.Active := True;
      NodeList := Xmldoc.DocumentElement.ChildNodes;
      nCount := NodeList.Count;
      for i := 0 to nCount - 1 do
      begin
        Node := NodeList.Get(i);
        GatherName := Node.NodeName;
        if CompareText(GatherName, CtSeccion_Param1) = 0 then
          ParseBigStyle( Node, ParseParam1 )
        else if CompareText(GatherName, CtSeccion_Param2) = 0 then
          ParseBigStyle( Node, ParseParam2 );
      end;
    finally
      NodeList := nil;
      Xmldoc.Active := False;
      Xmldoc.Free;
    end;
  end;
  InitConfig;
end;

procedure TxdSystemConfig.ParseBigStyle(const ANode: IXMLNode; AParse: TOnParse);
var
  NodeList: IXMLNodeList;
  Node: IXMLNode;
  i, nCount: Integer;
begin
  NodeList := ANode.ChildNodes;
  try
    nCount := NodeList.Count;
    for i := 0 to nCount - 1 do
    begin
      Node := NodeList.Get( i );
      AParse( Node );
    end;
  finally
    NodeList := nil;
  end;
end;

procedure TxdSystemConfig.ParseParam1(const ANode: IXMLNode);
var
  strNode: string;
  p: PSysNode1Info;
begin
  strNode := ANode.NodeName;
  p := FindNode1Info( FParam1List, strNode );
  if p = nil then
  begin
    New( p );
    p^.FName := strNode;
    p^.FNodeAttInfos := nil;
    FParam1List.Add( p );
  end;
  AddAtt( ANode, p^.FNodeAttInfos );
end;

procedure TxdSystemConfig.ParseParam2(const ANode: IXMLNode);
var
  strNode: string;
  p: PSysNode2Info;
  p1: PSysNode1Info;
  i, nCount: Integer;
  NodeList: IXMLNodeList;
  Node: IXMLNode;
begin
  strNode := ANode.NodeName;
  p := FindNode2Info( strNode );
  if p = nil then
  begin
    New( p );
    p^.FName := strNode;
    p^.FNodeList := TList.Create;
    FParam2List.Add( p );
  end;

  NodeList := ANode.ChildNodes;
  try
    nCount := NodeList.Count;
    for i := 0 to nCount - 1 do
    begin
      Node := NodeList.Get( i );
      strNode := Node.NodeName;
      p1 := FindNode1Info( p^.FNodeList, strNode );
      if p1 = nil then
      begin
        New( p1 );
        p1^.FName := strNode;
        p1^.FNodeAttInfos := nil;
        p^.FNodeList.Add( p1 );
      end;
      AddAtt( Node, p1^.FNodeAttInfos );
    end;
  finally
    NodeList := nil;
  end;
end;

procedure TxdSystemConfig.SaveNode1Info(const AList: TList; const ANode: IXMLNode);
var
  i: Integer;
  p: PSysNode1Info;
  p1: PSysParamInfo;
  node: IXMLNode;
begin
  for i := 0 to AList.Count - 1 do
  begin
    p := AList[i];
    node := ANode.AddChild( p^.FName );
    p1 := p^.FNodeAttInfos;
    while p1 <> nil do
    begin
      node.Attributes[ p1^.FName ] := p1^.FValue;
      p1 := p1^.FNext;
    end;
    node := nil;
  end;
end;

procedure TxdSystemConfig.SaveParam1(const ANode: IXMLNode);
begin
  SaveNode1Info( FParam1List, ANode );  
end;

procedure TxdSystemConfig.SaveParam2(const ANode: IXMLNode);
var
  i: Integer;
  node: IXMLNode;
  p: PSysNode2Info;
begin
  for i := 0 to FParam2List.Count - 1 do
  begin
    p := FParam2List[i];
    node := ANode.AddChild( p^.FName );
    SaveNode1Info( p^.FNodeList, node );
    node := nil;
  end;
end;

procedure TxdSystemConfig.SaveToFile(const AWinCtrl: TWinControl; const AFileName: string);
var
  doc:  IXmlDocument;
  root: IXmlNode;
  node: IXmlNode;
  strPath: string;
  procedure CheckNode;
  begin
    if not node.HasChildNodes then
      root.ChildNodes.Remove( node );
    node := nil;
  end;
begin
  strPath := ExtractFilePath( AFileName );
  if not DirectoryExists(strPath) then
    ForceDirectories( strPath );
    
  doc := NewXmlDocument();
  try
    doc.Encoding := 'gb2312';
    root := doc.AddChild( 'SystemConfig' );

    node := root.AddChild( CtSeccion_Param1 );
    SaveParam1( node );
    CheckNode;

    node := root.AddChild( CtSeccion_Param2 );
    SaveParam2( node );
    CheckNode;
    
    doc.SaveToFile( AFileName );
  finally
    doc := nil;
    root := nil;
    node := nil;
  end;
end;

procedure TxdSystemConfig.SetFileName(const Value: string);
begin
  FFileName := Value;
  if FileExists(FFileName) and Assigned(FOwner) then
    LoadFormFile(FOwner, FFileName);
end;

procedure TxdSystemConfig.AddParam(const AParentNode, ANode, AName: string; const AValue: Integer);
begin
  AddParam( AParentNode, ANode, AName, IntToStr(AValue) );
end;

end.
