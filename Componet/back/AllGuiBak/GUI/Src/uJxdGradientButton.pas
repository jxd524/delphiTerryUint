{
单元名称: JxdGradientButton
单元作者: 江晓德(jxd524@163.com)
网    址: www.geysoft.com
说    明: 渐变色按钮
开始时间: 2007-11-3
修改时间: 2009-6-5 (最后修改)
}
unit uJxdGradientButton;

interface

uses
  SysUtils, Classes, Controls, Windows, Graphics, StdCtrls, uJxdCustomButton, uBitmapHandle, uJxdParseGradient,
  uJxdGradientPanel;

type
  TJxdGradientButton = class(TJxdCustomButton)
  private
    FGray: TBitmap;
    FGrayLeft: Integer;
    FGrayTop: Integer;
    FGrayTransColor: TColor;

    FFontLeft, FFontTop: Integer;

    FFrameLienVisible: Boolean;
    FFrameLienColor: TColor;
    FFrameLienWidth: Integer;

    FGradientNormalText: string;
    FGradientNormalWay: TGradientWay;
    FParseGradientNormal: TParseGradient;

    FGradientHoverText: string;
    FGradientHoverWay: TGradientWay;
    FParseGradientHover: TParseGradient;

    FGradientMouseDownText: string;
    FGradientMouseDownWay: TGradientWay;
    FParseGradientMouseDown: TParseGradient;
    procedure SetFontLeftSpace(const Value: Integer);
    procedure SetFontTopSpace(const Value: Integer);
    procedure SetGrayTransColor(const Value: TColor);
    procedure SetGray(const Value: TBitmap);
    procedure SetGrayLeftSpace(const Value: Integer);
    procedure SetGrayTopSpace(const Value: Integer);
    procedure SetFrameLienVisible(const Value: Boolean);
    procedure SetFrameLineColor(const Value: TColor);
    procedure SetFrameLineWidth(const Value: Integer);
    procedure SetGradienHovertWay(const Value: TGradientWay);
    procedure SetGradientHoverText(const Value: string);
    procedure SetGradientMouseDownText(const Value: string);
    procedure SetGradientMouseDownWay(const Value: TGradientWay);
    procedure SetGradientNormalText(const Value: string);
    procedure SetGradientNormalWay(const Value: TGradientWay);
  protected
    procedure DrawColorGradient(const Index: Integer; ABmp: TBitmap);
    procedure DrawPanelLien(ABmp: TBitmap);
    procedure DrawGray(ABmp: TBitmap);

    procedure DrawCustomButton(ABufBitmap: TBitmap; AMouseState: TJxdControlState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property GradientNormalText: string read FGradientNormalText write SetGradientNormalText;
    property GradientNormalWay: TGradientWay read FGradientNormalWay write SetGradientNormalWay;
    property GradientHoverText: string read FGradientHoverText write SetGradientHoverText;
    property GradientHoverWay: TGradientWay read FGradientHoverWay write SetGradienHovertWay;
    property GradientMouseDownText: string read FGradientMouseDownText write SetGradientMouseDownText;
    property GradientMouseDownWay: TGradientWay read FGradientMouseDownWay write SetGradientMouseDownWay;

    property FrameLienVisible: Boolean read FFrameLienVisible write SetFrameLienVisible;
    property FrameLienColor: TColor read FFrameLienColor write SetFrameLineColor;
    property FrameLienWidth: Integer read FFrameLienWidth write SetFrameLineWidth;

    property Gray: TBitmap read FGray write SetGray;
    property GrayLeftSpace: Integer read FGrayLeft write SetGrayLeftSpace;
    property GrayTopSpace: Integer read FGrayTop write SetGrayTopSpace;
    property GrayTransColor: TColor read FGrayTransColor write SetGrayTransColor;

    property FontLeftSpace: Integer read FFontLeft write SetFontLeftSpace;
    property FontTopSpace: Integer read FFontTop write SetFontTopSpace;
  end;


implementation


{ TJxdGradientButton }

constructor TJxdGradientButton.Create(AOwner: TComponent);
begin
  inherited;
  FFrameLienColor := 0;
  FFrameLienWidth := 1;
  FFontTop := 0;
  FFontLeft := 0;

  FParseGradientNormal := TParseGradient.Create;
  GradientNormalText := '#Gradient{fromColor: $F7F7F7; ColorTo: $F1E5DD; percent: 100%}';

  FParseGradientHover := TParseGradient.Create;
  GradientHoverText := '#Gradient{fromColor: $FFFFFF; ColorTo: $F9F3F0; percent: 100%}';

  FParseGradientMouseDown := TParseGradient.Create;
  GradientMouseDownText := '#Gradient{fromColor: $EFE2D9; ColorTo: $F9F3F0; percent: 100%}';

  FGrayTransColor := clFuchsia;
  FGray := TBitmap.Create;
  FGrayLeft := 0;
  FGrayTop := 0;
end;

destructor TJxdGradientButton.Destroy;
begin
  FParseGradientNormal.Free;
  FParseGradientHover.Free;
  FParseGradientMouseDown.Free;
  inherited;
end;

procedure TJxdGradientButton.DrawColorGradient(const Index: Integer; ABmp: TBitmap);
var
  R: TRect;
  iLoop, nStep, nOldHeight, nHeight: Integer;
  p: PGradient;
  bDriection: Boolean;

  GradientWay: TGradientWay;
  ParseGradient: TParseGradient;
begin
  case Index of
  0:
  begin
    GradientWay := FGradientNormalWay;
    ParseGradient := FParseGradientNormal;
  end;
  1:
  begin
    GradientWay := FGradientHoverWay;
    ParseGradient := FParseGradientHover;
  end;
  2:
  begin
    GradientWay := FGradientMouseDownWay;
    ParseGradient := FParseGradientMouseDown;
  end;
  else
    Exit;
  end;

  if GradientWay = gwLeftToRigth then
    bDriection := True
  else
    bDriection := False;

  nOldHeight := 0;

  for iLoop := 0 to ParseGradient.GradientList.Count - 1 do
  begin
    p := ParseGradient.GradientList[iLoop];
    if iLoop = ParseGradient.GradientList.Count - 1 then
      nHeight := Height - nOldHeight
    else
      nHeight := Round(Height * p^.FPercent);
    nStep := nHeight div 2 + 1;
    R := Rect(0, nOldHeight, Width, nHeight + nOldHeight);
    DrawGradient(ABmp.Canvas, p^.FFromColor, p^.FColorTo, nStep, R, bDriection);
    nOldHeight := nOldHeight + nHeight;
  end;
end;


procedure TJxdGradientButton.DrawGray(ABmp: TBitmap);
var
  X, Y: Integer;
begin
  if not FGray.Empty then
  begin
    X := FGrayLeft;
    Y := FGrayTop;
    if FFrameLienVisible then
    begin
      X := X + FFrameLienWidth + 1;
      Y := Y + FFrameLienWidth + 1;
    end;
    FGray.TransparentColor := FGrayTransColor;
    FGray.Transparent := True;
    ABmp.Canvas.Draw(X, Y, FGray);
  end;
end;

procedure TJxdGradientButton.DrawCustomButton(ABufBitmap: TBitmap; AMouseState: TJxdControlState);
begin
  if not Enabled then
    DrawColorGradient(0, ABufBitmap)
  else if AMouseState = xcsDown then
    DrawColorGradient(2, ABufBitmap)
  else if AMouseState = xcsActive then
    DrawColorGradient(1, ABufBitmap)
  else
    DrawColorGradient(0, ABufBitmap);

  if FFrameLienVisible then
    DrawPanelLien(ABufBitmap);
  DrawGray(ABufBitmap);
  if not Enabled then
    GrapCanvas(ABufBitmap.Canvas, ABufBitmap.Width, ABufBitmap.Height);
end;

procedure TJxdGradientButton.DrawPanelLien(ABmp: TBitmap);
var
  R: TRect;
begin
  R := GetClientRect;
  DrawFrameBorder(ABmp.Canvas, FFrameLienColor, FFrameLienWidth, R);
end;

procedure TJxdGradientButton.SetFontLeftSpace(const Value: Integer);
begin
  if FFontLeft <> Value then
  begin
    FFontLeft := Value;
    if Caption <> '' then
      Invalidate;
  end;
end;

procedure TJxdGradientButton.SetFontTopSpace(const Value: Integer);
begin
  if FFontTop <> Value then
  begin
    FFontTop := Value;
    if Caption <> '' then
      Invalidate;
  end;
end;

procedure TJxdGradientButton.SetFrameLienVisible(const Value: Boolean);
begin
  if FFrameLienVisible <> Value then
  begin
    FFrameLienVisible := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientButton.SetFrameLineColor(const Value: TColor);
begin
  if FFrameLienColor <> Value then
  begin
    FFrameLienColor := Value;
    if FFrameLienVisible then
      Invalidate;
  end;
end;

procedure TJxdGradientButton.SetFrameLineWidth(const Value: Integer);
begin
  if FFrameLienWidth <> Value then
  begin
    FFrameLienWidth := Value;
    if FFrameLienVisible then
      Invalidate;
  end;
end;

procedure TJxdGradientButton.SetGradienHovertWay(const Value: TGradientWay);
begin
  if FGradientHoverWay <> Value then
  begin
    FGradientHoverWay := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientButton.SetGradientHoverText(const Value: string);
begin
  if (Value <> '') and (CompareText(FGradientHoverText, Value) <> 0) then
    begin
      FParseGradientHover.GradientMsg := Value;
      FGradientHoverText := Value;
      Invalidate;
    end;
end;

procedure TJxdGradientButton.SetGradientMouseDownText(const Value: string);
begin
  if (Value <> '') and (CompareText(FGradientMouseDownText, Value) <> 0) then
  begin
    FParseGradientMouseDown.GradientMsg := Value;
    FGradientMouseDownText := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientButton.SetGradientMouseDownWay(const Value: TGradientWay);
begin
  if FGradientMouseDownWay <> Value then
  begin
    FGradientMouseDownWay := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientButton.SetGradientNormalText(const Value: string);
begin
  if (Value <> '') and (CompareText(FGradientNormalText, Value) <> 0) then
  begin
    FParseGradientNormal.GradientMsg := Value;
    FGradientNormalText := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientButton.SetGradientNormalWay(const Value: TGradientWay);
begin
  if FGradientNormalWay <> Value then
  begin
    FGradientNormalWay := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientButton.SetGray(const Value: TBitmap);
begin
  FGray.Assign(Value);
  FGray.TransparentColor := FGrayTransColor;
  FGray.Transparent := True;
end;

procedure TJxdGradientButton.SetGrayLeftSpace(const Value: Integer);
begin
  if FGrayLeft <> Value then
  begin
    FGrayLeft := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientButton.SetGrayTopSpace(const Value: Integer);
begin
  if FGrayTop <> Value then
  begin
    FGrayTop := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientButton.SetGrayTransColor(const Value: TColor);
begin
  if FGrayTransColor <> Value then
  begin
    FGrayTransColor := Value;
    FGray.TransparentColor := FGrayTransColor;
    FGray.Transparent := True;
  end;
end;

end.
