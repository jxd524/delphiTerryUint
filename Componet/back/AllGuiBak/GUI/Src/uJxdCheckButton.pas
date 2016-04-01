unit uJxdCheckButton;
//´´½¨¡¡13 * 13
interface
uses
  SysUtils, Classes, Windows, Controls, Types, ExtCtrls, Graphics, Messages, StdCtrls, uJxdCustomButton;

type
  TJxdCheckButton = class(TJxdCustomButton)
  private
    FBoxOrgX: Integer;
    FBoxOrgY: Integer;
    FIsCheck: Boolean;
    FBackColor: TColor;
    FBorderLineWide: Integer;
    FBoxColor: TColor;
    FBorderLineColor: TColor;
    FOnCheckChange: TNotifyEvent;
    procedure SetChectk(const Value: Boolean);
    procedure SetBoxOrgX(const Value: Integer);
    procedure SetBoxOrgY(const Value: Integer);
    procedure SetBackColor(const Value: TColor);
    procedure SetBorderLineColor(const Value: TColor);
    procedure SetBorderLineWide(const Value: Integer);
    procedure SetBoxColor(const Value: TColor);
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
  protected
    procedure DrawCustomButton(ABufBitmap: TBitmap; AMouseState: TJxdControlState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Check: Boolean read FIsCheck write SetChectk default False;
    property BoxOrgX: Integer read FBoxOrgX write SetBoxOrgX default 2;
    property BoxOrgY: Integer read FBoxOrgY write SetBoxOrgY default 3;
    property BorderLineWide: Integer read FBorderLineWide write SetBorderLineWide;
    property BorderLineColor: TColor read FBorderLineColor write SetBorderLineColor;
    property BoxColor: TColor read FBoxColor write SetBoxColor;
    property BackColor: TColor read FBackColor write SetBackColor;
    property OnCheckChange: TNotifyEvent read FOnCheckChange write FOnCheckChange;
  end;

implementation

uses uBitmapHandle;

{ TKKCheckButton }
const
  CtBoxWidth  = 13;
  CtBoxHeight = 13;

constructor TJxdCheckButton.Create(AOwner: TComponent);
begin
  inherited;
  FBoxOrgX := 2;
  FBoxOrgY := 3;
  FIsCheck := False;
  FBackColor := clWhite;
  FBorderLineWide := 1;
  FBorderLineColor := RGB(84, 84, 84);
  FBoxColor := RGB(255, 108, 0);
  CaptionLeftSpace := FBoxOrgX + CtBoxWidth + 2;
  Width := 105;
  Height := 20;
end;

procedure TJxdCheckButton.DrawCustomButton(ABufBitmap: TBitmap; AMouseState: TJxdControlState);
var
  R: TRect;
  nBackColor, nLineColor, nBoxColor: TColor;
begin
  if ChangeColor.IsChange then
  begin
    if FBackColor <> ChangeColor.TransColor then
      nBackColor := ChangedRGB( FBackColor, ChangeColor.ChangeToColor )
    else
      nBackColor := FBackColor;
    nLineColor := ChangedRGB( FBorderLineColor, ChangeColor.ChangeToColor );
    nBoxColor  := ChangedRGB( FBoxColor, ChangeColor.ChangeToColor );
  end
  else
  begin
    nBackColor := FBackColor;
    nLineColor := FBorderLineColor;
    nBoxColor := FBoxColor;
  end;
  ABufBitmap.Canvas.Brush.Color := nBackColor;
  ABufBitmap.Canvas.FillRect( ClientRect );
  R := Rect( FBoxOrgX, FBoxOrgY, FBoxOrgX + CtBoxWidth, FBoxOrgY + CtBoxHeight );
  DrawFrameBorder( ABufBitmap.Canvas, nLineColor, FBorderLineWide, R);
  if FIsCheck then
  begin
    R.Left := R.Left + 2;
    R.Top  := R.Top + 2;
    R.Right := R.Right - 2;
    R.Bottom := R.Bottom - 2;
    ABufBitmap.Canvas.Brush.Color := nBoxColor;
    ABufBitmap.Canvas.FillRect( R );
  end;
end;

procedure TJxdCheckButton.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;

procedure TJxdCheckButton.SetBorderLineColor(const Value: TColor);
begin
  if FBorderLineColor <> Value then
  begin
    FBorderLineColor := Value;
    Invalidate;
  end;
end;

procedure TJxdCheckButton.SetBorderLineWide(const Value: Integer);
begin
  if FBorderLineWide <> Value then
  begin
    FBorderLineWide := Value;
    Invalidate;
  end;
end;

procedure TJxdCheckButton.SetBoxColor(const Value: TColor);
begin
  if FBoxColor <> Value then
  begin
    FBoxColor := Value;
    Invalidate;
  end;
end;

procedure TJxdCheckButton.SetBoxOrgX(const Value: Integer);
begin
  if FBoxOrgX <> Value then
  begin
    FBoxOrgX := Value;
    Invalidate;
  end;
end;

procedure TJxdCheckButton.SetBoxOrgY(const Value: Integer);
begin
  if FBoxOrgY <> Value then
  begin
    FBoxOrgY := Value;
    Invalidate;
  end;
end;

procedure TJxdCheckButton.SetChectk(const Value: Boolean);
begin
  if FIsCheck <> Value then
  begin
    FIsCheck := Value;
    Invalidate;
    if Assigned(FOnCheckChange) then
      FOnCheckChange(Self);
  end;
end;

procedure TJxdCheckButton.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if PtInRect(ClientRect, SmallPointToPoint(Message.Pos)) then
  begin
    Check := not Check;
  end;
  inherited;
end;

end.
