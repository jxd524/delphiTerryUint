{
单元名称: JxdCustomButton
单元作者: 江晓德(jxd524@163.com)
网    址: www.geysoft.com
说    明: 自绘按钮父类
开始时间: 2007-10-24
修改时间: 2009-6-5 (最后修改) 
}
unit uJxdCustomButton;

interface

uses
  Windows,SysUtils, Classes, Controls, StdCtrls,Graphics,Messages, uJxdGuiDef, uBitmapHandle;

type
  TJxdControlState = ( xcsNormal, xcsActive, xcsDown );
  TJxdCustomButton = class(TGraphicControl)
  private
    FTransparent: Boolean;
    FAutoGrap: Boolean;
    FCaptionLeftSpace: Integer;
    FGlyphCount: Integer;
    FChangeColor: TChangeToColor;
    procedure SetCaptionLeftSpace(const Value: Integer);
    procedure SetGrap(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
  protected
    FMouseState: TJxdControlState;

    procedure DrawCaption(ACanvas: TCanvas);

    procedure Paint; override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  protected
    procedure DrawCustomButton(ABufBitmap: TBitmap; AMouseState: TJxdControlState); virtual; //派生类需要实现的
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoGrap: Boolean read FAutoGrap write SetGrap;
    property CaptionLeftSpace: Integer read FCaptionLeftSpace write SetCaptionLeftSpace;
    property Transparence: Boolean read FTransparent write SetTransparent;
    property ChangeColor: TChangeToColor read FChangeColor write FChangeColor;
  published
    property Align;
    property Font;
    property Anchors;
    property Caption;
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

{ TKKCustomButton }

procedure TJxdCustomButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Enabled and (FMouseState <> xcsActive) then
  begin
    FMouseState := xcsActive;
    Invalidate;
  end;
end;

procedure TJxdCustomButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Enabled and (FMouseState <> xcsNormal) then
  begin
    FMouseState := xcsNormal;
    Invalidate;
  end;
end;

procedure TJxdCustomButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

constructor TJxdCustomButton.Create(AOwner: TComponent);
begin
  inherited;
  FTransparent   := False;
  FCaptionLeftSpace := 0;
  FMouseState       := xcsNormal;
  FAutoGrap         := True;
  FGlyphCount       := 1;
  FChangeColor      := TChangeToColor.Create( Self );
end;

destructor TJxdCustomButton.Destroy;
begin
  FChangeColor.Free;
  inherited;
end;

procedure TJxdCustomButton.DrawCaption(ACanvas: TCanvas);
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
    DrawText(ACanvas.Handle, PChar(Caption), Length(Caption), R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TJxdCustomButton.DrawCustomButton(ABufBitmap: TBitmap; AMouseState: TJxdControlState);
begin

end;


procedure TJxdCustomButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and Enabled then
  begin
    FMouseState := xcsDown;
    Invalidate;
  end;
end;

procedure TJxdCustomButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  inherited;
  if Enabled and PtInRect( ClientRect, Point(X, Y) ) and ( FMouseState <> xcsActive ) then
  begin
    GetCursorPos( pt );
    pt := ScreenToClient( pt );
    if PtInRect( ClientRect, pt )  then
      FMouseState := xcsActive
    else
      FMouseState := xcsNormal;
    Invalidate;
  end;
end;

procedure TJxdCustomButton.Paint;
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

procedure TJxdCustomButton.SetCaptionLeftSpace(const Value: Integer);
begin
  if FCaptionLeftSpace <> Value then
  begin
    FCaptionLeftSpace := Value;
    Invalidate;
  end;
end;

procedure TJxdCustomButton.SetGrap(const Value: Boolean);
begin
  if FAutoGrap <> Value then
  begin
    FAutoGrap := Value;
    Invalidate;
  end;
end;

procedure TJxdCustomButton.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

end.
