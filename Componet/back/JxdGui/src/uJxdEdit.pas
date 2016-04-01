{
单元名称: JxdEdit
单元作者: 江晓德(jxd524@163.com)
网    址: www.geysoft.com
说    明: 编辑框
开始时间: 2007-11-25
修改时间: 2008-3-17 (最后修改) 
}
unit uJxdEdit;

interface

uses
  SysUtils, Windows, Messages, Classes, Controls, StdCtrls, Forms, Graphics;

{$M+}
type
  TJxdEdit = class(TCustomEdit)
  private
    { Private declarations }
    FFrameColor: TColor;
    FFrameBorderWidth: Integer;
    FMulti: Boolean;
    FOnlyNumber: Boolean;
    FOldFontColor: TColor;
    FGrayColor: TColor;
    procedure SetMulti(const Value: Boolean);
    procedure SetFrameBorderWidth(const Value: Integer);
    procedure SetFrameColor(const Value: TColor);
    procedure SetEditRect;
    procedure SetGreyColor(const Value: TColor);
  protected
    { Protected declarations }
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetEnabled(Value: Boolean); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    property Multi: Boolean read FMulti write SetMulti default False;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property GreyColor: TColor read FGrayColor write SetGreyColor;
    property FrameBorderWidth: Integer read FFrameBorderWidth write SetFrameBorderWidth;
    property OnlyNumber: Boolean read FOnlyNumber write FOnlyNumber;
  published
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelWidth;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses uBitmapHandle;


{ TJxdEdit }

constructor TJxdEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  FFrameColor := $B99D7E;
  FFrameBorderWidth := 1;
  FMulti := False;
  FOnlyNumber := False;
  FOldFontColor := Font.Color;
  FGrayColor := $C8D0D4;
end;

procedure TJxdEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or ES_MULTILINE;
end;

procedure TJxdEdit.SetEditRect;
var
  R: TRect;
begin
  R := ClientRect;
  R.Top := R.Top + 2;
  R.Left := R.Left + 5;
  R.Right := R.Right - 5;
  SendMessage(Handle, EM_SETRECT, 0, LongInt(@R));  //EM_SETRECTNP
end;

procedure TJxdEdit.SetEnabled(Value: Boolean);
begin
  inherited;
  if Enabled then
    Font.Color := FOldFontColor
  else
    Font.Color := FGrayColor;
end;

procedure TJxdEdit.SetFrameBorderWidth(const Value: Integer);
begin
  if FFrameBorderWidth <> Value then
  begin
    FFrameBorderWidth := Value;
    Invalidate;
  end;
end;

procedure TJxdEdit.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;

procedure TJxdEdit.SetGreyColor(const Value: TColor);
begin
  FGrayColor := Value;
  if not Enabled then
    Font.Color := FGrayColor;
  Invalidate;
end;

procedure TJxdEdit.SetMulti(const Value: Boolean);
begin
  FMulti := Value;
  if not FMulti then
    Text := StringReplace(Text, #13, '', [rfReplaceAll]);
end;

procedure TJxdEdit.WMChar(var Message: TWMChar);
begin
  if FOnlyNumber then
  begin
    if not (Message.CharCode in [48..57]) then
      Exit;
  end;
  if not FMulti then
  begin
    if Message.CharCode = 13 then
      Exit;
  end;
  inherited;
end;

procedure TJxdEdit.WMPaint(var Message: TWMPaint);
var
  Canvas: TControlCanvas;
begin
  SetEditRect;
  inherited;
  Canvas := TControlCanvas.Create;
  try
    Canvas.Control := Self;
    if Enabled then
      DrawFrameBorder(Canvas, FFrameColor, FFrameBorderWidth, ClientRect)
    else
      DrawFrameBorder(Canvas, FGrayColor, FFrameBorderWidth, ClientRect);
  finally
    Canvas.Free;
  end;
end;

end.
