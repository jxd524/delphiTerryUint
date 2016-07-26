unit uKKEdit;

interface

uses
  TntStdCtrls, SysUtils, Windows, Messages, Classes, Controls, StdCtrls, Forms, Graphics, uKKBitmapHandle, uKKGuiDef;

{$M+}
type
  TKKEdit = class(TTntCustomEdit)//TCustomEdit)
  private
    { Private declarations }
    FFrameColor: TColor;
    FFrameBorderWidth: Integer;
    FOldFontColor: TColor;
    FGrayColor: TColor;
    FTopSpace: Integer;
    FLefSpace: Integer;
    FChangeColor: TChangeToColor;
    procedure SetFrameBorderWidth(const Value: Integer);
    procedure SetFrameColor(const Value: TColor);
    procedure SetEditRect;
    procedure SetGreyColor(const Value: TColor);
    procedure SetTopSpace(const Value: Integer);
    procedure SetLeftSpace(const Value: Integer);
  protected
    { Protected declarations }
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetEnabled(Value: Boolean); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TopSpace: Integer read FTopSpace write SetTopSpace default 2;
    property LeftSpace: Integer read FLefSpace write SetLeftSpace default 5;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property GreyColor: TColor read FGrayColor write SetGreyColor;
    property FrameBorderWidth: Integer read FFrameBorderWidth write SetFrameBorderWidth;
    property ChangeColor: TChangeToColor read FChangeColor write FChangeColor;
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
    property Text;
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

{ TJxdEdit }

constructor TKKEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  FFrameColor := $B99D7E;
  FFrameBorderWidth := 1;
  FOldFontColor := Font.Color;
  FGrayColor := $C8D0D4;
  FTopSpace := 2;
  FLefSpace := 5;
  FChangeColor := TChangeToColor.Create( Self );
end;

procedure TKKEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or ES_MULTILINE;
end;

destructor TKKEdit.Destroy;
begin
  FChangeColor.Free;
  inherited;
end;

procedure TKKEdit.SetEditRect;
var
  R: TRect;
  nWidth, nHeight: Integer;
begin
  R := ClientRect;
  nWidth := WidthOfRect( R ) - ( FFrameBorderWidth + FLefSpace ) * 2;
  nHeight := HeightOfRect( R ) - ( FFrameBorderWidth + FTopSpace ) * 2;
  R.Top := R.Top + FFrameBorderWidth + FTopSpace;
  R.Left := R.Left + FFrameBorderWidth + FLefSpace;
  R.Bottom := R.Top + nHeight;
  R.Right := R.Left + nWidth;
  SendMessage(Handle, EM_SETRECT, 0, LongInt(@R));  //EM_SETRECTNP
end;

procedure TKKEdit.SetEnabled(Value: Boolean);
begin
  inherited;
//  if Enabled then
//    Font.Color := FOldFontColor
//  else
//    Font.Color := FGrayColor;
end;

procedure TKKEdit.SetFrameBorderWidth(const Value: Integer);
begin
  if FFrameBorderWidth <> Value then
  begin
    FFrameBorderWidth := Value;
    Invalidate;
  end;
end;

procedure TKKEdit.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;

procedure TKKEdit.SetGreyColor(const Value: TColor);
begin
  FGrayColor := Value;
  if not Enabled then
    Font.Color := FGrayColor;
  Invalidate;
end;

procedure TKKEdit.SetLeftSpace(const Value: Integer);
begin
  FLefSpace := Value;
  Invalidate;
end;

procedure TKKEdit.SetTopSpace(const Value: Integer);
begin
  FTopSpace := Value;
  Invalidate;
end;

procedure TKKEdit.WMPaint(var Message: TWMPaint);
var
  Canvas: TControlCanvas;
  nColor: TColor;
begin
  SetEditRect;
  inherited;
  Canvas := TControlCanvas.Create;
  try
    Canvas.Control := Self;
    if Enabled then
      if ChangeColor.IsChange then
        nColor := ChangedRGB( FFrameColor, ChangeColor.ChangeToColor )
      else
        nColor := FFrameColor
    else
      nColor := FGrayColor;
    DrawFrameBorder(Canvas, nColor, FFrameBorderWidth, ClientRect)
  finally
    Canvas.Free;
  end;
end;

end.
