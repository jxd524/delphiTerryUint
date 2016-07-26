unit JxdHtmlEdit;

interface
  uses Classes, SHDocVw, ComCtrls, mshtml, Variants, ActiveX, StdCtrls, ExtCtrls,
       SysUtils, Graphics;

type
//JustifyFull JustifyNone: Not currently supported.
  TJustify = (tyCenter = 0, tyFull, tyLeft, tyNone, tyRight);
  TJxdHtmlEdit = class(TWebBrowser)
  private
    { private declarations }
    HTMLDocument: IHTMLDocument2;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    function GetHtml: WideString;
    procedure SetHtml(const Value: WideString);
    function GetText: WideString;
    procedure SetCurFontColor(const Value: TColor);
    function GetCurFontColor: TColor;
    function GetCurFontSize: Integer;
    procedure SetCurFontSize(const Value: Integer);
    function GetCurFontName: string;
    procedure SetCurFontName(const Value: string);
    function GetCurFontBold: Boolean;
    procedure SetCurFontBold(const Value: Boolean);
    function GetCurFontItalic: Boolean;
    procedure SetCurFontItalic(const Value: Boolean);
    procedure SetCurJustify(const Value: TJustify);
    function GetCurUnderLine: Boolean;
    procedure SetCurUnderLine(const Value: Boolean);
    function GetOrderedList: Boolean;
    procedure SetOrderedList(const Value: Boolean);
    function GetCurBackColor: TColor;
    procedure SetCurBackColor(const Value: TColor);
  protected
    procedure ExecCmd(const ACmd: string; AValue: Variant);
    function GetCmdValue(const ACmd: string): Variant;
    procedure InsertHTML(const AHTML: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(AHtmlStrem: TStream);
    procedure SaveToStream(Stream: TStream);
    {操作}
    procedure InsertImage; overload;
    procedure InsertImage(const AImageFileName: string); overload;
    procedure SetURL;
    {常规}
    procedure SelectAll;
    procedure Undo;
    procedure Redo;
    procedure Unselect;
    procedure Paste;
    procedure Copy;
    procedure Cut;
    procedure Delete;
    procedure Refresh;
    procedure Removeformat;
    {外部设备}
    procedure PrintPageSetup;
    procedure PrintPreview;
    procedure Print(const APreview: Boolean);
  published
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property HtmlString: WideString read GetHtml write SetHtml;
    property SimpleText: WideString read GetText;
    {字体相关}
    property CurFontColor: TColor read GetCurFontColor write SetCurFontColor;
    property CurBackColor: TColor read GetCurBackColor write SetCurBackColor;
    property CurFontSize: Integer read GetCurFontSize write SetCurFontSize;
    property CurFontName: string read GetCurFontName write SetCurFontName;
    property CurFontBold: Boolean read GetCurFontBold write SetCurFontBold;
    property CurFontItalic: Boolean read GetCurFontItalic write SetCurFontItalic;
    property CurUnderLine: Boolean read GetCurUnderLine write SetCurUnderLine;
    {布局相关}
    property CurJustify: TJustify write SetCurJustify;
    property InsertOrderedList: Boolean read GetOrderedList write SetOrderedList;
  end;
implementation

{ TJxdHtmlEdit }
const
  CtCurFontColor = 'ForeColor';
  CtCurBackColor = 'BackColor';
  CtCurFontSize = 'FontSize';
  CtCurFontName = 'FontName';
  CtCurFontBold = 'Bold';
  CtCurFontItalic = 'Italic';
  CtInsertImage = 'InsertImage';
  CtRemoveformat = 'Removeformat';
  CtUnderline = 'Underline';
  CtSelectAll = 'SelectAll';
  CtUndo = 'Undo';
  CtUnselect = 'Unselect';
  CtRedo = 'Redo';
  CtPaste = 'Paste';
  CtCopy = 'Copy';
  CtCut = 'Cut';
  CtRefresh = 'Refresh';
  CtDelete = 'Delete';
  CtInsertOrderedList = 'InsertOrderedList';
  CtJustifyAry: array[0..4] of string = ('JustifyCenter', 'JustifyFull', 'JustifyLeft', 'JustifyNone', 'JustifyRight');

procedure TJxdHtmlEdit.Copy;
begin
  ExecCmd(CtCopy, EmptyParam);
end;

constructor TJxdHtmlEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Navigate('about:blank');
  HTMLDocument := Self.Document as IHTMLDocument2;
  HTMLDocument.designMode := 'On';
end;

procedure TJxdHtmlEdit.Cut;
begin
  ExecCmd(CtCut, EmptyParam);
end;

procedure TJxdHtmlEdit.Delete;
begin
  ExecCmd(CtDelete, EmptyParam);
end;

destructor TJxdHtmlEdit.Destroy;
begin

  inherited;
end;

procedure TJxdHtmlEdit.ExecCmd(const ACmd: string; AValue: Variant);
begin
  HTMLDocument.execCommand(ACmd, TRUE, AValue);
end;

function TJxdHtmlEdit.GetCmdValue(const ACmd: string): Variant;
begin
  Result := HTMLDocument.queryCommandValue(ACmd);
end;

function TJxdHtmlEdit.GetCurBackColor: TColor;
begin
  try
    Result := GetCmdValue(CtCurBackColor);
  except
    Result := 0;
  end;
end;

function TJxdHtmlEdit.GetCurFontBold: Boolean;
begin
  try
    Result := GetCmdValue(CtCurFontBold);
  except
    Result := False;
  end;
end;

function TJxdHtmlEdit.GetCurFontColor: TColor;
begin
  try
    Result := GetCmdValue(CtCurFontColor);
  except
    Result := -1;
  end;
end;

function TJxdHtmlEdit.GetCurFontItalic: Boolean;
begin
  try
    Result := GetCmdValue(CtCurFontItalic);
  except
    Result := False;
  end;
end;

function TJxdHtmlEdit.GetCurFontName: string;
begin
  try
    Result := GetCmdValue(CtCurFontName);
  except
    Result := '';
  end;
end;

function TJxdHtmlEdit.GetCurFontSize: Integer;
begin
  try
    Result := GetCmdValue(CtCurFontSize);
  except
    Result := -1;
  end;
end;

function TJxdHtmlEdit.GetCurUnderLine: Boolean;
begin
  try
    Result := GetCmdValue(CtUnderline);
  except
    Result := False;
  end;
end;

function TJxdHtmlEdit.GetHtml: WideString;
begin
  Result := HTMLDocument.body.outerHTML;
end;

function TJxdHtmlEdit.GetOrderedList: Boolean;
begin
  try
    Result := GetCmdValue(CtInsertOrderedList);
  except
    Result := False;
  end;
end;

function TJxdHtmlEdit.GetReadOnly: Boolean;
begin
  if HTMLDocument.designMode = 'On' then
    Result := False
  else
    Result := True;
end;

function TJxdHtmlEdit.GetText: WideString;
begin
  Result := HTMLDocument.body.outerText;
end;

procedure TJxdHtmlEdit.InsertHTML(const AHTML: string);
begin
 HTMLDocument.body.insertAdjacentHTML('beforeEnd', AHTML);
end;

procedure TJxdHtmlEdit.InsertImage(const AImageFileName: string);
begin
  InsertHTML('<IMG src="file://' + AImageFileName + '">');
end;

procedure TJxdHtmlEdit.InsertImage;
begin
  ExecCmd(CtInsertImage, EmptyParam);
end;

procedure TJxdHtmlEdit.LoadFromFile(const AFileName: string);
var
  Stream: TStream;
begin
  if FileExists(AFileName) then
  begin
    Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TJxdHtmlEdit.LoadFromStream(AHtmlStrem: TStream);
var
  Size: Integer;
  S: string;
begin
  try
    Size := AHtmlStrem.Size - AHtmlStrem.Position;
    SetString(S, nil, Size);
    AHtmlStrem.Read(Pointer(S)^, Size);
    Self.OleObject.document.close();
    Self.OleObject.document.clear();
    Self.OleObject.document.write(S);
  except
  
  end;
end;

procedure TJxdHtmlEdit.Paste;
begin
  ExecCmd(CtPaste, EmptyParam);
end;

procedure TJxdHtmlEdit.Print(const APreview: Boolean);
begin
  if APreview then
    ExecWB(OLECMDID_PRINTPREVIEW, OLECMDEXECOPT_DODEFAULT)
  else
    ExecWB(OLECMDID_PRINT, OLECMDEXECOPT_DODEFAULT);
end;

procedure TJxdHtmlEdit.PrintPageSetup;
begin
  ExecWB(OLECMDID_PAGESETUP, OLECMDEXECOPT_DODEFAULT);
end;

procedure TJxdHtmlEdit.PrintPreview;
begin
  ExecWB(OLECMDID_PRINTPREVIEW, OLECMDEXECOPT_DODEFAULT);
end;

procedure TJxdHtmlEdit.Redo;
begin
  ExecCmd(CtRedo, EmptyParam);
end;

procedure TJxdHtmlEdit.Refresh;
begin
  ExecCmd(CtRefresh, EmptyParam);
end;

procedure TJxdHtmlEdit.Removeformat;
begin
  ExecCmd(CtRemoveformat, EmptyParam);
end;

procedure TJxdHtmlEdit.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJxdHtmlEdit.SaveToStream(Stream: TStream);
var
  S: string;
begin
  S := string(HtmlString);
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TJxdHtmlEdit.SelectAll;
begin
  ExecCmd(CtSelectAll, EmptyParam);
end;

procedure TJxdHtmlEdit.SetCurBackColor(const Value: TColor);
begin
  ExecCmd(CtCurBackColor, Value);
end;

procedure TJxdHtmlEdit.SetCurFontBold(const Value: Boolean);
var
  bState: Boolean;
begin
  bState := CurFontBold;
  if Value and (not bState) then
    ExecCmd(CtCurFontBold, EmptyParam)
  else if (not Value) and bState then
    ExecCmd(CtCurFontBold, EmptyParam);
end;

procedure TJxdHtmlEdit.SetCurFontColor(const Value: TColor);
begin
  ExecCmd(CtCurFontColor, Value);
end;

procedure TJxdHtmlEdit.SetCurFontItalic(const Value: Boolean);
var
  bState: Boolean;
begin
  bState := CurFontItalic;
  if Value and (not bState) then
    ExecCmd(CtCurFontItalic, EmptyParam)
  else if (not Value) and bState then
    ExecCmd(CtCurFontItalic, EmptyParam);
end;

procedure TJxdHtmlEdit.SetCurFontName(const Value: string);
begin
  ExecCmd(CtCurFontName, Value);
end;

procedure TJxdHtmlEdit.SetCurFontSize(const Value: Integer);
var
  nSize: Integer;
begin
  if not (Value in [1..7]) then
    nSize := 3
  else
    nSize := Value;
  ExecCmd(CtCurFontSize, nSize);
end;

procedure TJxdHtmlEdit.SetCurJustify(const Value: TJustify);
begin
  ExecCmd(CtJustifyAry[ Integer(Value) ], EmptyParam);
end;

procedure TJxdHtmlEdit.SetCurUnderLine(const Value: Boolean);
var
  bState: Boolean;
begin
  bState := CurUnderLine;
  if Value and (not bState) then
    ExecCmd(CtUnderline, EmptyParam)
  else if (not Value) and bState then
    ExecCmd(CtUnderline, EmptyParam);
end;

procedure TJxdHtmlEdit.SetHtml(const Value: WideString);
var
  Html: Variant;
begin
  Html := VarArrayCreate([0, 0], varVariant);
  Html[0] := Value;
  HTMLDocument.write(pSafearray(TVarData(Html).VArray));
end;

procedure TJxdHtmlEdit.SetOrderedList(const Value: Boolean);
var
  bState: Boolean;
begin
  bState := InsertOrderedList;
  if Value and (not bState) then
    ExecCmd(CtInsertOrderedList, EmptyParam)
  else if (not Value) and bState then
    ExecCmd(CtInsertOrderedList, EmptyParam);
end;

procedure TJxdHtmlEdit.SetReadOnly(const Value: Boolean);
begin
  if Value then
    HTMLDocument.designMode := 'Off'
  else
    HTMLDocument.designMode := 'On';
end;

procedure TJxdHtmlEdit.SetURL;
begin
  ExecCmd('CreateLink', EmptyParam);
end;

procedure TJxdHtmlEdit.Undo;
begin
  ExecCmd(CtUndo, EmptyParam);
end;

procedure TJxdHtmlEdit.Unselect;
begin
  ExecCmd(CtUnselect, EmptyParam);
end;

initialization
  OleInitialize(nil);
finalization
  try
    OleUninitialize;
  except
  end;
  
end.
