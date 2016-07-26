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
  SysUtils, Classes, ExtCtrls, Controls, Windows, Graphics, Messages, uBitmapHandle, uJxdParseGradient;

type
  TGradientWay = (gwUpToDown, gwLeftToRigth);
  TJxdGradientPanel = class(TCustomPanel)
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
    procedure DrawColorGradient;
    procedure DrawPanelLien;
    procedure DrawCaption;
  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
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
    //Delphi panel property
  public
    property DockManager;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property VerticalAlignment;
    property Visible;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


implementation


procedure TJxdGradientPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited;
  Rect := GetClientRect;
end;


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

procedure TJxdGradientPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

destructor TJxdGradientPanel.Destroy;
begin
  FParseGradient.Free;
  inherited;
end;

procedure TJxdGradientPanel.DrawCaption;
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

procedure TJxdGradientPanel.DrawColorGradient;
var
  R: TRect;
  iLoop, nStep, nOldHeight, nHeight: Integer;
  p: PGradient;
  bDriection: Boolean;
begin
  if FGradientWay = gwLeftToRigth then
    bDriection := True
  else
    bDriection := False;

  nOldHeight := 0;

  for iLoop := 0 to FParseGradient.GradientList.Count - 1 do
  begin
    p := FParseGradient.GradientList[iLoop];
    if iLoop = FParseGradient.GradientList.Count - 1 then
      nHeight := Height - nOldHeight
    else
      nHeight := Round(Height * p^.FPercent);
    nStep := nHeight;
    R := Rect(0, nOldHeight, Width, nHeight + nOldHeight);
    DrawGradient(Canvas, p^.FFromColor, p^.FColorTo, nStep, R, bDriection);
    nOldHeight := nOldHeight + nHeight;
  end;
end;

procedure TJxdGradientPanel.DrawPanelLien;
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
        FrameRgn(Canvas.Handle, WinHRGN, Brush.Handle, FFrameLienWidth, FFrameLienWidth);
    finally
      Brush.Free;
      DeleteObject(WinHRGN);
    end;
  end
  else
  begin
    DrawFrameBorder(Canvas, FFrameLienColor, FFrameLienWidth, R);
  end;
end;

procedure TJxdGradientPanel.Paint;
begin
  inherited;
  DrawColorGradient;
  if FFrameLienVisible then
    DrawPanelLien;
  DrawCaption;
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

procedure TJxdGradientPanel.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

procedure TJxdGradientPanel.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  if FIsRoundRectRgn then
  begin
    RoundRectRgn := False;
    RoundRectRgn := True;
  end;
  inherited;
end;

end.
