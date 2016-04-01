{
渐变色面板
作者: Terry(江晓德)
QQ:   67068633
Email:jxd524@163.com
2007-11-3 晚上2:00完成
}
unit uJxdGradientPanel;

interface

uses
  SysUtils, Classes, ExtCtrls, Controls, Windows, Graphics, Messages, uBitmapHandle, uJxdParseGradient, uJxdCustomPanel;

type
  TGradientWay = (gwUpToDown, gwLeftToRigth);
  TJxdGradientPanel = class(TJxdCustomPanel)
  private
    FIsRoundRectRgn: Boolean;
    FRoundRectWidth: Integer;
    FRoundRectHeight: Integer;
    FFrameLienVisible: Boolean;
    FFrameLienColor: TColor;
    FFrameLienWidth: Integer;
    FGradientText: string;
    FGradientWay: TGradientWay;
    FParseGradient: TParseGradient;
    procedure SetFrameLienVisible(const Value: Boolean);
    procedure SetFrameLineColor(const Value: TColor);
    procedure SetFrameLineWidth(const Value: Integer);
    procedure SetGradientWay(const Value: TGradientWay);
    procedure SetGradientText(const Value: string);
    procedure SetRoundRectRgn(const Value: Boolean);
    procedure SetRoundRect(const Index, Value: Integer);
  private
    procedure DrawColorGradient(ACanvas: TCanvas);
    procedure DrawPanelLien(ACanvas: TCanvas);
    procedure DrawCaption(ACanvas: TCanvas);
  protected
    procedure DrawPanel(ABufBitmap: TBitmap); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property RoundRectRgn: Boolean read FIsRoundRectRgn write SetRoundRectRgn;
    property RoundRectWidth: Integer index 0 read FRoundRectWidth write SetRoundRect;
    property RoundRectHeight: Integer index 1 read FRoundRectHeight write SetRoundRect;
    property GradientText: string read FGradientText write SetGradientText;
    property GradientWay: TGradientWay read FGradientWay write SetGradientWay;
    property FrameLienVisible: Boolean read FFrameLienVisible write SetFrameLienVisible;
    property FrameLienColor: TColor read FFrameLienColor write SetFrameLineColor;
    property FrameLienWidth: Integer read FFrameLienWidth write SetFrameLineWidth;
  end;


implementation

constructor TJxdGradientPanel.Create(AOwner: TComponent);
begin
  inherited;
  FDoubleBuffered := True;
  FIsRoundRectRgn := False;
  FRoundRectWidth := 20;
  FRoundRectHeight := 20;
  FFrameLienVisible := True;
  FFrameLienColor := $00FCB37E;
  FFrameLienWidth := 1;
  FParseGradient := TParseGradient.Create;
  GradientText := '#Gradient{fromColor: $E0A77E; ColorTo: $FDFDFE; percent: 40%}' +
                  '#Gradient{fromColor: $FDFDFE; ColorTo: $E09865; percent: 60%}';
end;

destructor TJxdGradientPanel.Destroy;
begin
  FParseGradient.Free;
  inherited;
end;

procedure TJxdGradientPanel.DrawCaption(ACanvas: TCanvas);
var
  R: TRect;
begin
  if Caption <> '' then
  begin
    R := GetClientRect;
    Canvas.Font := Font;
    Canvas.Brush.Style := bsClear;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R, DT_CENTER or DT_SINGLELINE or DT_VCENTER);
  end;
end;

procedure TJxdGradientPanel.DrawColorGradient(ACanvas: TCanvas);
var
  R: TRect;
  iLoop, nStep, nOldTemp, nTemp: Integer;
  p: PGradient;
begin
  nOldTemp := 0;
  if FGradientWay = gwLeftToRigth then
  begin
    for iLoop := 0 to FParseGradient.GradientList.Count - 1 do
    begin
      p := FParseGradient.GradientList[iLoop];
      if iLoop = FParseGradient.GradientList.Count - 1 then
        nTemp := Width - nOldTemp
      else
        nTemp := Round( Width * p^.FPercent );
      nStep := nTemp;
      R := Rect(nOldTemp, 0, nTemp + nOldTemp, Height);
      DrawGradient(ACanvas, p^.FFromColor, p^.FColorTo, nStep, R, True);
      nOldTemp := nOldTemp + nTemp;
    end;
  end
  else
  begin
    for iLoop := 0 to FParseGradient.GradientList.Count - 1 do
    begin
      p := FParseGradient.GradientList[iLoop];
      if iLoop = FParseGradient.GradientList.Count - 1 then
        nTemp := Height - nOldTemp
      else
        nTemp := Round( Height * p^.FPercent );
      nStep := nTemp;
      R := Rect(0, nOldTemp, Width, nTemp + nOldTemp);
      DrawGradient(ACanvas, p^.FFromColor, p^.FColorTo, nStep, R, False);
      nOldTemp := nOldTemp + nTemp;
    end;
  end;
end;

procedure TJxdGradientPanel.DrawPanelLien(ACanvas: TCanvas);
var
  Brush: TBrush;
  R: TRect;
  WinHRGN: HRGN;
begin
  R := GetClientRect;
  if FIsRoundRectRgn then
  begin
    WinHRGN := CreateRoundRectRgn(0, 0, width, Height, FRoundRectWidth, FRoundRectHeight);
    Brush := TBrush.Create;
    try
      Brush.Color := FFrameLienColor;
      Brush.Style := bsSolid;
      if WinHRGN <> 0 then
        FrameRgn(ACanvas.Handle, WinHRGN, Brush.Handle, FFrameLienWidth, FFrameLienWidth);
    finally
      Brush.Free;
      DeleteObject(WinHRGN);
    end;
  end
  else
  begin
    DrawFrameBorder(ACanvas, FFrameLienColor, FFrameLienWidth, R);
  end;
end;

procedure TJxdGradientPanel.DrawPanel(ABufBitmap: TBitmap);
begin
  inherited;
  DrawColorGradient( ABufBitmap.Canvas );
  if FFrameLienVisible then
    DrawPanelLien( ABufBitmap.Canvas );
  DrawCaption( ABufBitmap.Canvas );
end;

procedure TJxdGradientPanel.SetFrameLienVisible(const Value: Boolean);
begin
  if FFrameLienVisible <> Value then
  begin
    FFrameLienVisible := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientPanel.SetFrameLineColor(const Value: TColor);
begin
  if FFrameLienColor <> Value then
  begin
    FFrameLienColor := Value;
    if FFrameLienVisible then
      Invalidate;
  end;
end;

procedure TJxdGradientPanel.SetFrameLineWidth(const Value: Integer);
begin
  if FFrameLienWidth <> Value then
  begin
    FFrameLienWidth := Value;
    if FFrameLienVisible then
      Invalidate;
  end;
end;

procedure TJxdGradientPanel.SetGradientText(const Value: string);
begin
  if (Value <> '') and (CompareText(FGradientText, Value) <> 0) then
  begin
    FParseGradient.GradientMsg := Value;
    FGradientText := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientPanel.SetGradientWay(const Value: TGradientWay);
begin
  if FGradientWay <> Value then
  begin
    FGradientWay := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientPanel.SetRoundRect(const Index, Value: Integer);
var
  bChange: Boolean;
  winHRGN: HRGN;
begin
  if Value < 0 then
    raise Exception.Create('值要了正数');
  bChange := False;
  if Index = 0 then
  begin
    if Value <> FRoundRectWidth then
    begin
      FRoundRectWidth := Value;
      bChange := True;
    end;
  end
  else if Index = 1 then
  begin
    if Value <> FRoundRectHeight then
    begin
      FRoundRectHeight := Value;
      bChange := True;
    end;
  end;
  if FIsRoundRectRgn and bChange then
  begin
    winHRGN := CreateRoundRectRgn(0, 0, Width, Height, FRoundRectWidth, FRoundRectHeight);
    SetWindowRgn(Handle, winHRGN, True);
    Invalidate;
  end;
end;

procedure TJxdGradientPanel.SetRoundRectRgn(const Value: Boolean);
var
  winHRGN: HRGN;
begin
  if FIsRoundRectRgn <> Value then
  begin
    FIsRoundRectRgn := Value;
    if FIsRoundRectRgn then
      winHRGN := CreateRoundRectRgn(0, 0, Width, Height, FRoundRectWidth, FRoundRectHeight)
    else
      winHRGN := CreateRoundRectRgn(0, 0, Width, Height, 0, 0);
    SetWindowRgn(Handle, winHRGN, True);
    Invalidate;
  end;
end;

end.
