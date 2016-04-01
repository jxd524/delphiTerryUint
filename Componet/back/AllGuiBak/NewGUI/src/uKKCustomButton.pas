unit uKKCustomButton;

interface
uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Graphics, Messages, StdCtrls, uKKGuiDef;

type
  TKKControlState = ( kcsNormal, kcsActive, kcsDown );
  TKKCustomButton = class(TGraphicControl)
  private
    FTransparent: Boolean;
    FAutoGrap: Boolean;
    FCaptionLeftSpace: Integer;
    FGlyphCount: Integer;
    FChangeColor: TChangeToColor;
    FCaption: TWideCaption;
    procedure SetCaptionLeftSpace(const Value: Integer);
    procedure SetGrap(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure SetWideCaption(const Value: TWideCaption);
  protected
    FMouseState: TKKControlState;

    procedure DrawCaption(ACanvas: TCanvas);

    procedure Paint; override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  protected
    procedure DrawCustomButton(ABufBitmap: TBitmap; AMouseState: TKKControlState); virtual; //派生类需要实现的
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoGrap: Boolean read FAutoGrap write SetGrap;
    property Caption: TWideCaption read FCaption write SetWideCaption;
    property CaptionLeftSpace: Integer read FCaptionLeftSpace write SetCaptionLeftSpace;
    property Transparence: Boolean read FTransparent write SetTransparent;
    property ChangeColor: TChangeToColor read FChangeColor write FChangeColor;
  published
    property Align;
    property Font;
    property Anchors;
    property Visible;
    property Enabled;
    property OnClick;
    property OnDblClick;
    property OnCanResize;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;
implementation

uses uKKBitmapHandle;

{ TKKCustomButton }

procedure TKKCustomButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Enabled and (FMouseState <> kcsActive) then
  begin
    FMouseState := kcsActive;
    Invalidate;
  end;
end;

procedure TKKCustomButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Enabled and (FMouseState <> kcsNormal) then
  begin
    FMouseState := kcsNormal;
    Invalidate;
  end;
end;

procedure TKKCustomButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

constructor TKKCustomButton.Create(AOwner: TComponent);
begin
  inherited;
  FTransparent   := False;
  FCaptionLeftSpace := 0;
  FMouseState       := kcsNormal;
  FAutoGrap         := True;
  FGlyphCount       := 1;
  FChangeColor      := TChangeToColor.Create( Self );
end;

destructor TKKCustomButton.Destroy;
begin
  FChangeColor.Free;
  inherited;
end;

procedure TKKCustomButton.DrawCaption(ACanvas: TCanvas);
var
  R: TRect;
begin
  if Caption <> '' then
  begin
    ACanvas.Font := Font;
    if FChangeColor.IsChange then
      ACanvas.Font.Color := ChangedRGB( Font.Color, FChangeColor.ChangeToColor );
    ACanvas.Brush.Style := bsClear;
    R := ClientRect;
    R.Left := R.Left + FCaptionLeftSpace;
    DrawTextW(ACanvas.Handle, PWideChar(Caption), Length(Caption), R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TKKCustomButton.DrawCustomButton(ABufBitmap: TBitmap; AMouseState: TKKControlState);
begin

end;


procedure TKKCustomButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and Enabled then
  begin
    FMouseState := kcsDown;
    Invalidate;
  end;
end;

procedure TKKCustomButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  inherited;
  if Enabled and PtInRect( ClientRect, Point(X, Y) ) and ( FMouseState <> kcsActive ) then
  begin
    GetCursorPos( pt );
    pt := ScreenToClient( pt );
    if PtInRect( ClientRect, pt )  then
      FMouseState := kcsActive
    else
      FMouseState := kcsNormal;
    Invalidate;
  end;
end;

procedure TKKCustomButton.Paint;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Width := Width;
    Bmp.Height := Height;
    DrawCustomButton( Bmp, FMouseState );
    DrawCaption( Bmp.Canvas );
    if FAutoGrap and (not Enabled) then
      GrapCanvas( Bmp.Canvas, Bmp.Width, Bmp.Height, ChangeColor.TransColor );
    //显示
    if Transparence then
    begin
      Canvas.Brush.Style := bsClear;
      SetBkMode(Canvas.Handle, TRANSPARENT);
      Canvas.BrushCopy( ClientRect, Bmp, ClientRect, ChangeColor.TransColor);
    end
    else
    begin
      Canvas.CopyRect( ClientRect, Bmp.Canvas, ClientRect);
    end;
  finally
    Bmp.FreeImage;
  end;
end;

procedure TKKCustomButton.SetCaptionLeftSpace(const Value: Integer);
begin
  if FCaptionLeftSpace <> Value then
  begin
    FCaptionLeftSpace := Value;
    Invalidate;
  end;
end;

procedure TKKCustomButton.SetGrap(const Value: Boolean);
begin
  if FAutoGrap <> Value then
  begin
    FAutoGrap := Value;
    Invalidate;
  end;
end;

procedure TKKCustomButton.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TKKCustomButton.SetWideCaption(const Value: TWideCaption);
begin
  FCaption := Value;
  Invalidate;
end;

end.
