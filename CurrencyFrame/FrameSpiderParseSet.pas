unit FrameSpiderParseSet;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls, uSpiderParse, uXmlSysOption, XmlDoc, XMLIntf, uStringHandle, ComCtrls,
  uSysSub, AES;

{$M+}
type
  TFrmSpiderParseSet = class(TFrame)
    pnlTop: TPanel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edtFS: TEdit;
    edtBS: TEdit;
    chkEdgeVisible: TCheckBox;
    cbbDelWay: TComboBox;
    edtFieldName: TEdit;
    chkBigContent: TCheckBox;
    Label1: TLabel;
    edtBigFS: TEdit;
    Label2: TLabel;
    edtBigBS: TEdit;
    gbFieldContent: TGroupBox;
    rbNone: TRadioButton;
    rbReplaceString: TRadioButton;
    rbDelString: TRadioButton;
    rbJs: TRadioButton;
    pnlTotal: TPanel;
    pnlReplace: TPanel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    btnAddReplace: TButton;
    edtOldValue: TEdit;
    edtNewValue: TEdit;
    edtReplaceCount: TEdit;
    btnClearReplace: TButton;
    lstReplace: TListBox;
    pnlDelString: TPanel;
    Label7: TLabel;
    Edit4: TEdit;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    ListBox2: TListBox;
    Button2: TButton;
    pnlJs: TPanel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Edit5: TEdit;
    Edit6: TEdit;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    pnlClient: TPanel;
    sgFieldInfo: TStringGrid;
    Panel2: TPanel;
    btnAddField: TButton;
    btnDel: TButton;
    Button1: TButton;
    pnlURL: TPanel;
    Label14: TLabel;
    edtURL: TEdit;
    pnlAllLeft: TPanel;
    pnlAllClient: TPanel;
    tvSpiderTask: TTreeView;
    btnDelTask: TButton;
    btnAdd: TButton;
    lbl1: TLabel;
    edtTag: TEdit;
    procedure lstReplaceClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure sgFieldInfoClick(Sender: TObject);
    procedure btnClearReplaceClick(Sender: TObject);
    procedure btnAddReplaceClick(Sender: TObject);
    procedure btnAddFieldClick(Sender: TObject);
    procedure edtURLChange(Sender: TObject);
    procedure chkBigContentClick(Sender: TObject);
    procedure tvSpiderTaskChange(Sender: TObject; Node: TTreeNode);
    procedure tvSpiderTaskEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure tvSpiderTaskCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    procedure btnAddClick(Sender: TObject);
    procedure btnDelTaskClick(Sender: TObject);
    procedure tvSpiderTaskEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure rbNoneClick(Sender: TObject);
  private
    FSpiderFile: string;
    FCurTaskID: Integer;
    FCurHandleInfo: string;
    FCurFieldType: TFieldHandleStyle;

    procedure SetCurFieldType(const Value: TFieldHandleStyle);
    procedure SetCurTaskID(const Value: Integer);
    procedure SetSpiderFile(const Value: string);
    function GetFieldWay: TFieldHandleStyle;
    procedure SetFieldWay(const Value: TFieldHandleStyle);
    procedure SetSpiderInfo(const AEnable: Boolean);
  private
    procedure InitSpiderState;
    procedure LoadTaskInfo;

    procedure ShowFieldInfo(const ARow: Integer);
    procedure ShowReplayStyle(const ARow: Integer);

    function  TypeToStr(ADelWay: TDeleteWay): string; overload;
    function  StrToType(AStr: string): TDeleteWay; overload;

    function ShowError(AEdt: TEdit; AError: string): Boolean;
    function CheckAddFieldValue: Boolean;

    property  FieldDealWay: TFieldHandleStyle read GetFieldWay write SetFieldWay;
    property  CurTaskID: Integer read FCurTaskID write SetCurTaskID;
    property  CurFieldType: TFieldHandleStyle read FCurFieldType write SetCurFieldType;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SpiderTaskFile: string read FSpiderFile write SetSpiderFile;
  end;

{
<SpiderTask ID=0 name="name">
  <UrL>http://sdjfl[sjdkf]</Url>
  <MainContent Enable=0 FrontSign="fs" BackSign="skd" />
  <FieldInfo name="name" FrontSign="sk" BackSign="sjkdf" DeleteWay=0 DelEdgeUnVisible=0 HandleInfo="[RP]sklso" Tag=0 />
  <FieldInfo name="name" FrontSign="sk" BackSign="sjkdf" DeleteWay=1 DelEdgeUnVisible=0 HandleInfo="[RP]sklso" Tag=0 />
  <FieldInfo name="name" FrontSign="sk" BackSign="sjkdf" DeleteWay=0 DelEdgeUnVisible=0 HandleInfo="[RP]sklso" Tag=0 />
</SpiderTask>
}

  TSpiderXml = class(TParseXml)
  private
    FSpiderTaskID: TStringList;
    FCurNode: IXMLNode;
    FCurMaxID: Integer;
    procedure LoadAllTaskID;
    function  FindNodeByID(const ANodeID: Integer): IXMLNode;
    function  FindChildNode(const ANodeID: Integer; const AChildName: string): IXMLNode;
    function  FindField(AParentNode: IXMLNode; AIndex: Integer): IXMLNode;
    function  MakeSureChildNode(AParentNode: IXMLNode; const AChildName: string): IXMLNode;

    function  GetSpiderTaskName(AID: Integer): string;
    procedure SetSpiderTaskName(AID: Integer; const Value: string);
    function  GetSpiderURL(AID: Integer): string;
    procedure SetSpiderURL(AID: Integer; const Value: string);
    function  GetEnableMainContent(AID: Integer): Boolean;
    procedure SetEnableMainContent(AID: Integer; const Value: Boolean);
    function  GetFieldCount(AID: Integer): Integer;
  public
    function  TaskExists(const AID: Integer): Boolean;
    function  SetCurTask(const AID: Integer): Boolean;
    function  AddTask: Integer;
    procedure DeleteTask(const AID: Integer);
    function  DeleteFieldInfo(const AIndex: Integer): Boolean;
    function  GetMainContent(var AStrFrontSign, AStrBackSign: string): Boolean;
    function  SetMainContent(AStrFrontSign, AStrBackSign: string): Boolean;
    function  GetFieldInfo(const AIndex: Integer; var AFieldName, AFrontSign, ABackSign, AHandleInfo: string;
                           var ADelWay: TDeleteWay; var ADelEdgeUnVisible: Boolean; var ATag: Integer): Boolean;
    function  SetFieldInfo(const AIndex: Integer; AFieldName, AFrontSign, ABackSign, AHandleInfo: string;
                           ADelWay: TDeleteWay; ADelEdgeUnVisible: Boolean; ATag: Integer): Boolean;

    procedure GetAllTaskID(lt: TStringList);
  public
    constructor Create(AOwnerComponent: TComponent; AXmlFileName: string);
    destructor Destroy; override;


    property SpiderURL[AID: Integer]: string read GetSpiderURL write SetSpiderURL;
    property SpiderTaskName[AID: Integer]: string read GetSpiderTaskName write SetSpiderTaskName;
    property EnableMainContent[AID: Integer]: Boolean read GetEnableMainContent write SetEnableMainContent;
    property FieldCount[AID: Integer]: Integer read GetFieldCount;
  end;

var
  GSpiderTask: TSpiderXml = nil;

implementation

{$R *.dfm}

const
  CtDeleteForward = '删除前面';
  CtDeleteBack = '删除后面';
  CtNotDelete = '不删除';
  CtReplaceSing = '==>';
  CtLeftSing = '[';
  CtRightSing = ']';

{ TFrmSpiderParseSet }

procedure TFrmSpiderParseSet.btnAddClick(Sender: TObject);
var
  ID: Integer;
  Node: TTreeNode;
  strCaption: string;
begin
  if not Assigned(GSpiderTask) then Exit;
  ID := GSpiderTask.AddTask;
  strCaption := '采集新任务';
  GSpiderTask.SpiderTaskName[ID] := strCaption;
  Node := tvSpiderTask.Items.AddChild( tvSpiderTask.TopItem, strCaption );
  Node.Data := Pointer( ID );
  tvSpiderTask.FullExpand;
  Node.EditText;
end;

procedure TFrmSpiderParseSet.btnAddFieldClick(Sender: TObject);
var
  strFieldName, strFS, strBS: string;
  DelWay: TDeleteWay;
  bDelUnVisible: Boolean;
  nTag: Integer;
  nRow: Integer;
begin
  if not CheckAddFieldValue then Exit;
  strFieldName := edtFieldName.Text;
  strFS := edtFS.Text;
  strBS := edtFS.Text;
  nTag := StrToIntDef( edtTag.Text, 0 );
  DelWay := StrToType( cbbDelWay.Text );
  bDelUnVisible := chkEdgeVisible.Checked;
  nRow := GSpiderTask.FieldCount[CurTaskID];
  GSpiderTask.SetFieldInfo( nRow, strFieldName, strFS, strBS,
                            FCurHandleInfo, DelWay, bDelUnVisible, nTag );
  with sgFieldInfo do
  begin
    RowCount := RowCount + 1;
    Cells[0, nRow + 1] := strFieldName;
    Cells[1, nRow + 1] := IntToStr( nTag );
    Cells[2, nRow + 1] := strFS;
    Cells[3, nRow + 1] := strBS;
    Cells[4, nRow + 1] := TypeToStr( DelWay );
    Cells[5, nRow + 1] := IntToStr( Integer(bDelUnVisible) );
    Cells[6, nRow + 1] := FCurHandleInfo;
  end;
  ShowFieldInfo( nRow + 1 );
end;

procedure TFrmSpiderParseSet.btnAddReplaceClick(Sender: TObject);
var
  nCount: Integer;
begin
  if not ShowError(edtOldValue, '原值不能为空') then Exit;
  nCount := StrToIntDef(edtReplaceCount.Text, -1);
  TSpiderParse.MakeReplaceInfo( FCurHandleInfo, edtOldValue.Text, edtNewValue.Text, nCount );
  lstReplace.Items.Add( edtOldValue.Text + CtReplaceSing + edtNewValue.Text + CtLeftSing + IntToStr(nCount) + CtRightSing );
end;

procedure TFrmSpiderParseSet.btnClearReplaceClick(Sender: TObject);
begin
  lstReplace.Clear;
  FCurHandleInfo := '';
  edtOldValue.Text := '';
  edtNewValue.Text := '';
  edtReplaceCount.Text := '';
end;

procedure TFrmSpiderParseSet.btnDelClick(Sender: TObject);
var
  nRow: Integer;
begin
  nRow := sgFieldInfo.Row;
  GSpiderTask.DeleteFieldInfo( nRow - 1 );
  LoadTaskInfo;
end;

procedure TFrmSpiderParseSet.btnDelTaskClick(Sender: TObject);
var
  SelNode: TTreeNode;
  ID: Integer;
begin
  SelNode := tvSpiderTask.Selected;
  if (not Assigned(GSpiderTask) ) or (SelNode = nil) or SelNode.IsFirstNode then Exit;
  ID := Integer( SelNode.Data );
  GSpiderTask.DeleteTask( ID );
  InitSpiderState;
end;

function TFrmSpiderParseSet.CheckAddFieldValue: Boolean;
begin
  Result := False;
  if not ShowError( edtFieldName, '字段名不能为空' ) then Exit;
  if not ShowError( edtFS, '前标识不能为空') then Exit;
  if not ShowError( edtBS, '后标识不能为空') then Exit;
  Result := True;
end;

procedure TFrmSpiderParseSet.chkBigContentClick(Sender: TObject);
begin
  edtBigFS.Enabled := chkBigContent.Checked;
  edtBigBS.Enabled := chkBigContent.Checked;
  if Assigned(GSpiderTask) then
    GSpiderTask.EnableMainContent[CurTaskID] := chkBigContent.Checked;
end;

constructor TFrmSpiderParseSet.Create(AOwner: TComponent);
begin
  inherited;
  FSpiderFile := '';
  SetSpiderInfo( False );
  with sgFieldInfo do
  begin
    ColCount := 7;
    FixedRows := 1;
    RowCount := 2;
    Cells[0, 0] := '字段名称';
    Cells[1, 0] := '字段标识';
    Cells[2, 0] := '前标识';
    Cells[3, 0] := '后标识';
    Cells[4, 0] := '删除方向';
    Cells[5, 0] := '不可见字符';
    Cells[6, 0] := '处理方式';
  end;
  FieldDealWay := fsNone;
end;

destructor TFrmSpiderParseSet.Destroy;
begin
  if Assigned(GSpiderTask) then
    GSpiderTask.Free;
  inherited;
end;

procedure TFrmSpiderParseSet.edtURLChange(Sender: TObject);
var
  nTag: Integer;
begin
  if Assigned(GSpiderTask) and GSpiderTask.TaskExists(CurTaskID) then
  begin
    nTag := (Sender as TEdit).Tag;
    if nTag = 0 then
      GSpiderTask.SpiderURL[CurTaskID] := (Sender as TEdit).Text
    else if nTag in [1, 2] then
      GSpiderTask.SetMainContent( edtBigFS.Text, edtBigBS.Text );
  end;
end;

function TFrmSpiderParseSet.GetFieldWay: TFieldHandleStyle;
begin
  if rbNone.Checked then
    Result := fsNone
  else if rbReplaceString.Checked then
    Result := fsReplace
  else if rbDelString.Checked then
    Result := fsDelete
  else
    Result := fsJs;  
end;

procedure TFrmSpiderParseSet.InitSpiderState;
var
  lt: TStringList;
  i, CurID: Integer;
  ParentTreeNode, TreeNode: TTreeNode;
begin
  if not Assigned(GSpiderTask) then Exit;
  lt := TStringList.Create;
  tvSpiderTask.Items.Clear;
  tvSpiderTask.Items.AddFirst(nil, '任务列表');
  try
    GSpiderTask.GetAllTaskID( lt );
    ParentTreeNode := tvSpiderTask.Items.GetFirstNode;
    for i := lt.Count - 1 downto 0 do
    begin
      CurID := StrToInt( lt[i] );
      TreeNode := tvSpiderTask.Items.AddChild( ParentTreeNode, GSpiderTask.SpiderTaskName[CurID] );
      TreeNode.Data := Pointer( CurID );
    end;
    tvSpiderTask.FullExpand;
  finally
    lt.Free;
  end;
end;

procedure TFrmSpiderParseSet.LoadTaskInfo;
var
  strFS, strBS, strName, strHandleInfo: string;
  bDelEdge: Boolean;
  nDelWay: TDeleteWay;
  i, nCount, nTag: Integer;
begin
  if (not Assigned(GSpiderTask) ) or (not GSpiderTask.TaskExists(CurTaskID)) then Exit;
  GSpiderTask.SetCurTask( CurTaskID );
  edtURL.Text := GSpiderTask.SpiderURL[CurTaskID];
  chkBigContent.Checked := GSpiderTask.EnableMainContent[CurTaskID];
  GSpiderTask.GetMainContent( strFS, strBS );
  edtBigFS.Text := strFS;
  edtBigBS.Text := strBS;
  nCount := GSpiderTask.FieldCount[CurTaskID];
  if nCount = 0 then
    sgFieldInfo.RowCount := 2
  else
    sgFieldInfo.RowCount := nCount + 1;
  for i := 0 to nCount - 1 do
  begin
    GSpiderTask.GetFieldInfo( i, strName, strFS, strBS, strHandleInfo, nDelWay, bDelEdge, nTag );
    with sgFieldInfo do
    begin
      Cells[0, i + 1] := strName;
      Cells[1, i + 1] := IntToStr( nTag );
      Cells[2, i + 1] := strFS;
      Cells[3, i + 1] := strBS;
      Cells[4, i + 1] := TypeToStr( nDelWay );
      Cells[5, i + 1] := IntToStr( Integer(bDelEdge) );
      Cells[6, i + 1] := strHandleInfo;
    end;
  end;
  ShowFieldInfo( 1 );
end;

procedure TFrmSpiderParseSet.lstReplaceClick(Sender: TObject);
begin
  ShowReplayStyle( lstReplace.ItemIndex );
end;

procedure TFrmSpiderParseSet.rbNoneClick(Sender: TObject);
begin
  FieldDealWay := FieldDealWay;
end;

procedure TFrmSpiderParseSet.SetCurFieldType(const Value: TFieldHandleStyle);
begin
  FCurFieldType := Value;
  case FCurFieldType of
    fsError, fsNone:  rbNone.Checked := True;
    fsReplace:        rbReplaceString.Checked := True;
    fsDelete:         rbDelString.Checked := True ;
    fsJs:             rbJs.Checked := True  ;
  end;
end;

procedure TFrmSpiderParseSet.SetCurTaskID(const Value: Integer);
begin
  FCurTaskID := Value;
  if Assigned(GSpiderTask) and GSpiderTask.TaskExists(FCurTaskID) then
  begin
    SetSpiderInfo( True );
    LoadTaskInfo;
  end
  else
    SetSpiderInfo( False );
end;

procedure TFrmSpiderParseSet.SetFieldWay(const Value: TFieldHandleStyle);
begin
  case Value of
    fsError, fsNone:
    begin
      pnlReplace.Visible := False;
      pnlDelString.Visible := False;
      pnlJs.Visible := False;
      rbNone.Checked := True;
      gbFieldContent.Height := 56;
      pnlTop.Height := 253;
    end;
    fsReplace:
    begin
      pnlReplace.Visible := True;
      pnlDelString.Visible := False;
      pnlJs.Visible := False;
      rbReplaceString.Checked := True;
      pnlReplace.Height := 122;
      gbFieldContent.Height := 176;
      pnlTop.Height := 370;
      edtOldValue.Text := '';
      edtNewValue.Text := '';
      edtReplaceCount.Text := '';
      lstReplace.Clear;
    end;
    fsDelete:
    begin
      pnlReplace.Visible := False;
      pnlDelString.Visible := True;
      pnlJs.Visible := False;
      rbDelString.Checked := True;
      pnlDelString.Height := 122;
      gbFieldContent.Height := 176;
      pnlTop.Height := 369;
    end;
    fsJs:
    begin
      pnlReplace.Visible := False;            
      pnlDelString.Visible := False;
      pnlJs.Visible := True;
      rbJs.Checked := True;
      pnlJs.Height := 89;
      gbFieldContent.Height := 143;
      pnlTop.Height := 341;
    end;
  end;
  FCurHandleInfo := '';
end;

procedure TFrmSpiderParseSet.SetSpiderFile(const Value: string);
begin
  if CompareText(FSpiderFile, Value) <> 0 then
  begin
    if Assigned(GSpiderTask) then
    begin
      GSpiderTask.Free;
      GSpiderTask := nil;
    end;
    FSpiderFile := Value;
    GSpiderTask := TSpiderXml.Create( Self, FSpiderFile );
    InitSpiderState;
  end;
end;

procedure TFrmSpiderParseSet.SetSpiderInfo(const AEnable: Boolean);
  procedure SetCtrl(ACtrl: TWinControl);
  var
    i: Integer;
  begin
    for i := 0 to ACtrl.ControlCount - 1 do
    begin
      if ACtrl.Controls[i] is TWinControl then
        SetCtrl( ACtrl.Controls[i] as TWinControl );
    end;
    ACtrl.Enabled := AEnable;
  end;
begin
  SetCtrl( pnlAllClient );
end;

procedure TFrmSpiderParseSet.sgFieldInfoClick(Sender: TObject);
begin
  ShowFieldInfo( sgFieldInfo.Row );
end;

{
Cells[0, 0] := '字段名称';
    Cells[1, 0] := '字段标识';
    Cells[2, 0] := '前标识';
    Cells[3, 0] := '后标识';
    Cells[4, 0] := '删除方向';
    Cells[5, 0] := '不可见字符';
    Cells[6, 0] := '处理方式';
}
function TFrmSpiderParseSet.ShowError(AEdt: TEdit; AError: string): Boolean;
begin
  Result := AEdt.Text <> '' ;
  if not Result then ShowMsg( AError );
end;

procedure TFrmSpiderParseSet.ShowFieldInfo(const ARow: Integer);
var
  strHandle: string;
  HandleTyep: TFieldHandleStyle;
  lt: TStringList;
  i: Integer;
begin
  if (sgFieldInfo.RowCount < ARow) or (sgFieldInfo.Cells[1, ARow] = '') then Exit;
  with sgFieldInfo do
  begin
    edtFieldName.Text := Cells[0, ARow];
    edtTag.Text := Cells[1, ARow];
    edtFS.Text := Cells[2, ARow];
    edtBS.Text := Cells[3, ARow];
    case StrToType(Cells[4, ARow]) of
      dwFroward:   cbbDelWay.ItemIndex := 0;
      dwBack:      cbbDelWay.ItemIndex := 1;
      dwNotDelete: cbbDelWay.ItemIndex := 2;
    end;
    chkEdgeVisible.Checked := Boolean( StrToIntDef( Cells[5, ARow], 1 ) );
    strHandle := Cells[6, ARow];
    StringReplace( strHandle, #0, '');
    FCurHandleInfo := strHandle;
    lt := TStringList.Create;
    try
      TSpiderParse.ParseHandleInfo( strHandle, HandleTyep, lt );
      CurFieldType := HandleTyep;
      if HandleTyep = fsReplace then
      begin
        i := 0;
        lstReplace.Clear;
        while i <> lt.Count do
        begin
          lstReplace.Items.Add( lt[i] + CtReplaceSing + lt[i + 1] + CtLeftSing + lt[i + 2] + CtRightSing );
          Inc(i, 3);
        end;
        ShowReplayStyle(0);
      end
      else if HandleTyep = fsDelete then
      begin

      end;
    finally
      lt.Free;
    end;
  end;
end;

procedure TFrmSpiderParseSet.ShowReplayStyle(const ARow: Integer);
var
  strInfo, strNew, strOld, strCount: string;
begin
  if lstReplace.Count <= ARow then Exit;
  strInfo := lstReplace.Items[ARow];
  if strInfo = '' then Exit;
  //旧值, 新值, 次数
  strOld := GetRangString( strInfo, CtReplaceSing );
  strNew := GetRangString( strInfo, CtLeftSing );
  strCount := GetRangString( strInfo, CtReplaceSing );
  edtOldValue.Text := strOld;
  edtNewValue.Text := strNew;
  edtReplaceCount.Text := strCount;
end;

function TFrmSpiderParseSet.StrToType(AStr: string): TDeleteWay;
begin
  if CompareText(CtDeleteForward, AStr) = 0 then
    Result := dwFroward
  else if CompareText(CtDeleteBack, AStr) = 0 then
    Result := dwBack
  else
    Result := dwNotDelete;
end;

procedure TFrmSpiderParseSet.tvSpiderTaskChange(Sender: TObject; Node: TTreeNode);
var
  CurID: Integer;
begin
  if Node.IsFirstNode or (not Assigned(GSpiderTask) ) then
  begin
    SetSpiderInfo( False );
    Exit;
  end;
  CurID := Integer( Node.Data );
  GSpiderTask.SetCurTask( CurID );
  CurTaskID := CurID;
end;

procedure TFrmSpiderParseSet.tvSpiderTaskCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse := False;
end;

procedure TFrmSpiderParseSet.tvSpiderTaskEdited(Sender: TObject; Node: TTreeNode; var S: string);
var
  ID: Integer;
begin
  if Assigned(GSpiderTask) then
  begin
    ID := Integer( Node.Data );
    GSpiderTask.SpiderTaskName[ID] := S;
  end;
end;

procedure TFrmSpiderParseSet.tvSpiderTaskEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit := True;
  if Node.IsFirstNode then
    AllowEdit := False;
end;

function TFrmSpiderParseSet.TypeToStr(ADelWay: TDeleteWay): string;
begin
  case ADelWay of
    dwFroward:   Result := CtDeleteForward;
    dwBack:      Result := CtDeleteBack;
    dwNotDelete: Result := CtNotDelete;
  end;
end;

{ TSpiderXml }

const
  CtSpiderTask = 'SpiderTask';
  CtID = 'ID';
  CtName = 'name';
  CtURL = 'Url';
  CtMaincontent = 'MainContent';
  CtEnable = 'Enable';
  CtFrontSign = 'FrontSign';
  CtBackSign = 'BackSign';
  CtFieldInfo = 'FieldInfo';
  CtDeleteWay = 'DeleteWay';
  CtDelEdgeUnVisible = 'DelEdgeUnVisible';
  CtHandleInfo = 'HandleInfo';
  CtTag = 'Tag';
  
function TSpiderXml.AddTask: Integer;
var
  Node: IXMLNode;
begin
  Result := FCurMaxID + 1;
  try
    Node := FXmldoc.DocumentElement.AddChild( CtSpiderTask );
    Node.Attributes[CtID] :=  Result;
    Inc(FCurMaxID);
    FSpiderTaskID.Add( IntToStr(Result) );
  except
    Result := -1;
  end;
end;

constructor TSpiderXml.Create(AOwnerComponent: TComponent; AXmlFileName: string);
begin
  inherited Create( AOwnerComponent, 'SpiderByTerry' );
  FCurNode := nil;
  FSpiderTaskID := TStringList.Create;
  FileName := AXmlFileName;
  LoadAllTaskID;
  AutoSaveToFile := True;
end;

function TSpiderXml.DeleteFieldInfo(const AIndex: Integer): Boolean;
var
  node: IXMLNode;
begin
  Result := False;
  if FCurNode = nil then Exit;
  node := FindField(FCurNode, AIndex);
  if node = nil then Exit;
  FCurNode.ChildNodes.Remove( node );
  Result := True;
end;

procedure TSpiderXml.DeleteTask(const AID: Integer);
var
  node: IXMLNode;
begin
  node := FindNodeByID( AID );
  if node <> nil then
    FXmlNodeList.Remove(node);
  LoadAllTaskID;
end;

destructor TSpiderXml.Destroy;
begin
  FSpiderTaskID.Free;
  inherited;
end;

function TSpiderXml.FindChildNode(const ANodeID: Integer; const AChildName: string): IXMLNode;
var
  Node, NewNode: IXMLNode;
begin
  Node := FindNodeByID(ANodeID);
  if Node = nil then
    raise Exception.Create('not find the spider task by ' + IntToStr(ANodeID) );
  if Node.ChildNodes = nil then
    Node := Node.AddChild( AChildName )
  else
  begin
    NewNode := Node.ChildNodes.FindNode( AChildName );
    if NewNode = nil then
      Node := Node.AddChild( AChildName )
    else
      Node := NewNode;
  end;
  Result := Node;
end;

function TSpiderXml.FindField(AParentNode: IXMLNode; AIndex: Integer): IXMLNode;
var
  ChildNode: IXMLNode;
  i: Integer;
begin
  Result := nil;
  if (AParentNode = nil) or (AParentNode.ChildNodes = nil) then Exit;
  for i := 0 to AParentNode.ChildNodes.Count - 1 do
  begin
    ChildNode := AParentNode.ChildNodes.Get(i);
    if CompareText( ChildNode.NodeName, CtFieldInfo ) = 0 then
    begin
      if AIndex = 0 then
      begin
        Result := ChildNode;
        Break;
      end;
      Dec(AIndex);
    end;
  end;  
end;

function TSpiderXml.FindNodeByID(const ANodeID: Integer): IXMLNode;
var
  i: Integer;
  Node: IXMLNode;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Node := FXmlNodeList.Get(i);
    if Node.HasAttribute(CtID) then
    begin
      if Node.Attributes[CtID] = ANodeID then
      begin
        Result := Node;
        Break;
      end; 
    end;
  end;
end;

procedure TSpiderXml.GetAllTaskID(lt: TStringList);
begin
  lt.Assign( FSpiderTaskID );
end;

function TSpiderXml.GetEnableMainContent(AID: Integer): Boolean;
var
  Node: IXMLNode;
begin
  Result := False;
  try
    Node := FindChildNode(AID, CtMaincontent);
  except
  end;
  if Node <> nil then
  begin
    if Node.HasAttribute(CtEnable) then
      Result := Node.Attributes[CtEnable];
  end;
end;

function TSpiderXml.GetFieldCount(AID: Integer): Integer;
var
  Node, ChildNode: IXMLNode;
  i: Integer;
begin
  Result := 0;
  Node := FindNodeByID( AID );
  if (Node = nil) or (Node.ChildNodes = nil) then Exit;
  for i := Node.ChildNodes.Count - 1 downto 0 do
  begin
    ChildNode := Node.ChildNodes.Get(i);
    if CompareText( ChildNode.NodeName, CtFieldInfo ) = 0 then
      Inc(Result);    
  end;  
end;

function TSpiderXml.GetFieldInfo(const AIndex: Integer; var AFieldName, AFrontSign, ABackSign, AHandleInfo: string;
  var ADelWay: TDeleteWay; var ADelEdgeUnVisible: Boolean; var ATag: Integer): Boolean;
var
  node: IXMLNode;
  TempValue: OleVariant;
  procedure GetAttribute(AAttibuteName: string);
  begin
    TempValue := EmptyParam;
    if node.HasAttribute(AAttibuteName) then
      TempValue := node.Attributes[AAttibuteName];
  end;
begin
//<FieldInfo name="name" FrontSign="sk" BackSign="sjkdf" DeleteWay=0 DelEdgeUnVisible=0 HandleInfo="[RP]sklso" Tag=0 />
  Result := False;
  node := FindField(FCurNode, AIndex);
  if node = nil then Exit;
  Result := True;

  GetAttribute( CtName );
  if TempValue <> NULL then AFieldName := TempValue else AFieldName := '';

  GetAttribute( CtFrontSign );
  if TempValue <> NULL then AFrontSign := TempValue else AFrontSign := '';

  GetAttribute( CtBackSign );
  if TempValue <> NULL then ABackSign := TempValue else ABackSign := '';

  GetAttribute( CtDeleteWay );
  if TempValue <> NULL then ADelWay := TempValue else ADelWay := dwFroward;

  GetAttribute( CtDelEdgeUnVisible );
  if TempValue <> NULL then ADelEdgeUnVisible := TempValue else ADelEdgeUnVisible := True;

  if node.HasAttribute( CtHandleInfo ) then
    AHandleInfo := node.Attributes[CtHandleInfo];
  if AHandleInfo <> '' then AHandleInfo := DecryptString(AHandleInfo, 'TerrySpiderInfo');

  GetAttribute( CtTag );
  if TempValue <> NULL then ATag := TempValue else ATag := 0;
end;

function TSpiderXml.GetMainContent(var AStrFrontSign, AStrBackSign: string): Boolean;
var
  Node: IXMLNode;
begin
  Result := False;
  if FCurNode <> nil then
  begin
    Node := MakeSureChildNode(FCurNode, CtMaincontent);
    if Node.HasAttribute(CtFrontSign) then
      AStrFrontSign := Node.Attributes[CtFrontSign];
    if Node.HasAttribute(CtBackSign) then
      AStrBackSign := Node.Attributes[CtBackSign];
    Result := True;
  end;
end;

function TSpiderXml.GetSpiderTaskName(AID: Integer): string;
var
  Node: IXMLNode;
begin
  Result := '';
  Node := FindNodeByID( AID );
  if Node <> nil then
  begin
    if Node.HasAttribute(CtName) then
      Result := Node.Attributes[CtName];
  end;
end;

function TSpiderXml.GetSpiderURL(AID: Integer): string;
var
  Node: IXMLNode;
begin
  Result := '';
  try
    Node := FindChildNode(AID, CtURL);
  except
  end;
  if (Node <> nil) and (Node.NodeValue <> NULL) then
    Result := Node.NodeValue
end;

procedure TSpiderXml.LoadAllTaskID;
var
  i: Integer;
  Node: IXMLNode;
begin
  FSpiderTaskID.Clear;
  FCurMaxID := 0;
  for i := Count - 1 downto 0 do
  begin
    Node := FXmlNodeList.Get(i);
    if Node.HasAttribute(CtID) then
    begin
      if Node.Attributes[CtID] > FCurMaxID then
        FCurMaxID := Node.Attributes[CtID];
      FSpiderTaskID.Add( Node.Attributes[CtID] )
    end
    else
      FXmlNodeList.Delete( i );
  end;
end;

function TSpiderXml.MakeSureChildNode(AParentNode: IXMLNode; const AChildName: string): IXMLNode;
begin
  if AParentNode.ChildNodes <> nil then
    Result := AParentNode.ChildNodes.FindNode( AChildName )
  else
    Result := AParentNode.AddChild( AChildName );
  if Result = nil then
    Result := AParentNode.AddChild( AChildName );
end;

function TSpiderXml.SetCurTask(const AID: Integer): Boolean;
var
  Node: IXMLNode;
begin
  Result := False;
  Node := FindNodeByID( AID );
  if Node <> nil then
  begin
    FCurNode := Node;
    Result := True;
  end;
end;

procedure TSpiderXml.SetEnableMainContent(AID: Integer; const Value: Boolean);
var
  Node: IXMLNode;
begin
  Node := FindChildNode(AID, CtMaincontent);
  if Node <> nil then
    Node.Attributes[CtEnable] := Value;
end;

function TSpiderXml.SetFieldInfo(const AIndex: Integer; AFieldName, AFrontSign, ABackSign, AHandleInfo: string;
  ADelWay: TDeleteWay; ADelEdgeUnVisible: Boolean; ATag: Integer): Boolean;
var
  node: IXMLNode;
begin
//<FieldInfo name="name" FrontSign="sk" BackSign="sjkdf" DeleteWay=0 DelEdgeUnVisible=0 HandleInfo="[RP]sklso" Tag=0 />
  Result := FCurNode <> nil;
  node := FindField(FCurNode, AIndex);
  if node = nil then
    node := FCurNode.AddChild( CtFieldInfo );
  node.Attributes[CtName] := AFieldName;
  node.Attributes[CtFrontSign] := AFrontSign;
  node.Attributes[CtBackSign] := ABackSign;
  node.Attributes[CtDeleteWay] := ADelWay;
  node.Attributes[CtDelEdgeUnVisible] := ADelEdgeUnVisible;
  node.Attributes[CtHandleInfo] := EncryptString(AHandleInfo, 'TerrySpiderInfo');
  node.Attributes[CtTag] := ATag;
end;

function TSpiderXml.SetMainContent(AStrFrontSign, AStrBackSign: string): Boolean;
var
  Node: IXMLNode;
begin
  Result := False;
  if FCurNode <> nil then
  begin
    Node := MakeSureChildNode(FCurNode, CtMaincontent);
    Node.Attributes[CtFrontSign] := AStrFrontSign;
    Node.Attributes[CtBackSign]  := AStrBackSign;
    Result := True;
  end;
end;

procedure TSpiderXml.SetSpiderTaskName(AID: Integer; const Value: string);
var
  Node: IXMLNode;
begin
  Node := FindNodeByID( AID );
  if Node <> nil then
    Node.Attributes[CtName] := Value;
end;

procedure TSpiderXml.SetSpiderURL(AID: Integer; const Value: string);
var
  Node: IXMLNode;
begin
  Node := FindChildNode(AID, CtURL);
  if Node <> nil then
    Node.NodeValue := Value;
end;

function TSpiderXml.TaskExists(const AID: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FSpiderTaskID.Count - 1 do
  begin
    if StrToInt(FSpiderTaskID[i]) = AID then
    begin
      Result := True;
      Break;
    end;
  end;
end;

end.
