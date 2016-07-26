unit YYPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, ExtCtrls, YYGraphUtil;

type
  TDrawPanelEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  TPanelStyle = (psAdvance,psMain, psSmall, psChange, psSetOpt, psOwnerDraw);

  TFYPanel = class(TCustomControl)
  private
    FFrameColor: TColor;
    FBrushColor: TColor;
    FRoundValue: Integer;
    FOnPaint: TDrawPanelEvent;
    FOnDrawPanel: TDrawPanelEvent;
    FIndent: Integer;
    FFullRepaint: Boolean;
    FEndColor: TColor;
    FStartColor: TColor;
    FFillDirection: TFillDirection;
    FPanelStyle: TPanelStyle;
    FContainLeft: WORD;
    FContainTop: WORD;
    FContainRight: WORD;
    FContainBottom: WORD;
    FScrollText: string;
    FScrollTime: string;
    FScrollOffset: Integer;
    FClientBottom: WORD;
    FClientTop: WORD;
    FClientLeft: WORD;
    FClientRight: WORD;
    FChangedColor: Boolean;

    FLineColor: TColor;
    FLineWidth: Integer;
    FLeftLine: Boolean;
    FRightLine: Boolean;
    FTopLine: Boolean;
    FBottonLine: Boolean;
    procedure SetLineWidth(const Value: Integer);
    procedure SetBottonLine(const Value: Boolean);
    procedure SetLeftLine(const Value: Boolean);
    procedure SetRightLine(const Value: Boolean);
    procedure SetTopLine(const Value: Boolean);

    procedure SetLineColor(const Value: TColor);

    procedure SetChangedColor(const Value: Boolean);
    procedure SetBrushColor(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetRoundValue(const Value: Integer);
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure SetIndent(const Value: Integer);
    procedure SetEndColor(const Value: TColor);
    procedure SetFillDirection(const Value: TFillDirection);
    procedure SetStartColor(const Value: TColor);
    procedure SetPanelStyle(const Value: TPanelStyle);
    procedure DrawAdvPanel(ACanvas: TCanvas);
    procedure DrawMainPanel(ACanvas: TCanvas);
    procedure DrawSmallPanel(ACanvas: TCanvas);
    procedure DrawChangePanel(ACanvas: TCanvas);
    procedure DrawSetOptPanel(ACanvas: TCanvas);
    procedure SetContainSize(const Index: Integer; const Value: WORD);
    procedure SetClientSize(const Index: Integer; const Value: WORD);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure DrawPanel(ACanvas: TCanvas); virtual;
    procedure AdjustClientRect(var Rect: TRect); override;
  public
    property Canvas;
    property DockManager;
    constructor Create(AOwner: TComponent); override;
    procedure ScrollText(const AText: string; const ATime: string);
  published
    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;
    property LeftLine: Boolean read FLeftLine write SetLeftLine default True;
    property RightLine: Boolean read FRightLine write SetRightLine default True;
    property TopLine: Boolean read FTopLine write SetTopLine default True;
    property BottonLine: Boolean read FBottonLine write SetBottonLine default True;

    property LineColor: TColor read FLineColor write SetLineColor default $D5BCA2;
    property ChangedColor: Boolean read FChangedColor write SetChangedColor default True;
    property PanelStyle: TPanelStyle read FPanelStyle write SetPanelStyle;
    property StartColor: TColor read FStartColor write SetStartColor;
    property EndColor: TColor read FEndColor write SetEndColor;
    property FillDirection: TFillDirection read FFillDirection write SetFillDirection;
    property OnPaint: TDrawPanelEvent read FOnPaint write FOnPaint;
    property OnDrawPanel: TDrawPanelEvent read FOnDrawPanel write FOnDrawPanel;
    property DoubleBuffered default True;
    property FrameColor: TColor read FFrameColor write SetFrameColor default $00D1A483;
    property BrushColor: TColor read FBrushColor write SetBrushColor default $00FDDFCA;
    property RoundValue: Integer read FRoundValue write SetRoundValue default 5;
    property Indent: Integer read FIndent write SetIndent default 0;
    property FullRepaint: Boolean read FFullRepaint write FFullRepaint default True;
    property Font;
    property Color;
    property Caption;
    property ContainLeft: WORD index 0 read FContainLeft write SetContainSize default 7;
    property ContainTop: WORD index 1 read FContainTop write SetContainSize default 88;
    property ContainRight: WORD index 2 read FContainRight write SetContainSize default 7;
    property ContainBottom: WORD index 3 read FContainBottom write SetContainSize default 65;
    property ClientLeft: WORD index 0 read FClientLeft write SetClientSize default 0;
    property ClientTop: WORD index 1 read FClientTop write SetClientSize default 0;
    property ClientRight: WORD index 2 read FClientRight write SetClientSize default 0;
    property ClientBottom: WORD index 3 read FClientBottom write SetClientSize default 0;
    property Align;
    property Anchors;
    property AutoSize;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

{ TFYPanel }
{$R FYPanel.RES}
{$R Smallpanel.RES}
{$R ChangePanel.RES}
{$R SetOptPnl.RES}

procedure TFYPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  Inc(Rect.Left, FClientLeft);
  Inc(Rect.Top, FClientTop);
  Dec(Rect.Right, FClientRight);
  Dec(Rect.Bottom, FClientBottom);
end;

constructor TFYPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csParentBackground, csDoubleClicks, csReplicatable];
  FDoubleBuffered := True;
  FFullRepaint := True;
  FChangedColor := True;
  FFrameColor := $00D1A483;
  FBrushColor := $00FDDFCA;
  FLineColor := $00D5BCA2;
  FLeftLine := True;
  FRightLine := True;
  FTopLine := True;
  FBottonLine := True;
  FLineWidth := 1;

  Brush.Color := FBrushColor;
  RoundValue := 5;
  FIndent := 0;
  Width := 185;
  Height := 200;
  FScrollOffset := -1;
  FPanelStyle := psAdvance;
  FContainLeft := 7;
  FContainRight := 7;
  FContainTop := 88;
  FContainBottom := 65;
//  FContainLeft := 22;
//  FContainRight := 22;
//  FContainTop := 77;
//  FContainBottom := 74;

  UseDockManager := True;
end;

procedure TFYPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TFYPanel.DrawAdvPanel(ACanvas: TCanvas);
var
  CR: TRect;
  ResBmp: TBitmap;
begin
  ResBmp := TBitmap.Create;
  try
    ResBmp.LoadFromResourceName(HInstance, 'FYPANEL');
    with ACanvas do
    begin
      Brush.Color := RGB(238,243,250);
      FillRect(Rect(0, 0, Width, Height));
      //Draw Top
      CopyRect(Rect(0, 0, 7, 88), ResBmp.Canvas, Rect(0, 0, 7, 88));
      CopyRect(Rect(7, 0, Width - 7, 88), ResBmp.Canvas, Rect(7, 0, 9, 88));
      CopyRect(Rect(Width - 7, 0, Width, 88), ResBmp.Canvas, Rect(9, 0, 16, 88));
      //Draw Bottom
      CopyRect(Rect(0, Height - 65, 7, Height), ResBmp.Canvas, Rect(0, 88, 7, 153));
      CopyRect(Rect(7, Height - 65, Width - 7, Height), ResBmp.Canvas, Rect(7, 88, 9, 153));
      CopyRect(Rect(Width - 7, Height - 65, Width, Height), ResBmp.Canvas, Rect(9, 88, 16, 153));

      //DrawLeftRight
      CopyRect(Rect(0, 88, 7, Height - 65), ResBmp.Canvas, Rect(0, 153, 7, 155));
      CopyRect(Rect(Width - 7, 88, Width, Height - 65), ResBmp.Canvas, Rect(0, 153, 7, 155));
      //Draw Contain---
      CopyRect(Rect(FContainLeft, Height - FContainBottom, Width - FContainRight, Height - 65), ResBmp.Canvas, Rect(16, 0, 18, 155));
    end;
  finally
    ResBmp.Free;
  end;


  //»­°üº¬µÄÈÝÆ÷
  CR := Rect(FContainLeft, FContainTop, Width - FContainRight, Height - FContainBottom);
  with ACanvas do
  begin
    InflateRect(CR, -1, -1);
    Brush.Color :=  RGB(238,243,250);
    FrameRect(CR);
//    if csDesigning in ComponentState then
//    begin
//      InflateRect(CR, -1, -1);
//      Brush.Color := clFuchsia;
//      FillRect(CR);
//    end;
  end;
end;

procedure TFYPanel.DrawChangePanel(ACanvas: TCanvas);
  procedure DrawPanelLine(ACanvas: TCanvas);
  begin
    with ACanvas do
    begin
      Pen.Color := FLineColor;
      Pen.Width := FLineWidth;
      if FLeftLine then
      begin
        MoveTo(Width - 1, 0);
        LineTo(Width - 1, Height - 1);
      end;
      if FBottonLine then
      begin
        MoveTo(Width - 1, Height - 1);
        LineTo(0, Height - 1);
      end;
      if FRightLine then
      begin
        MoveTo(0, Height);
        LineTo(0, 0);
      end;
      if FTopLine then
      begin
        MoveTo(0, 0);
        LineTo(Width, 0);
      end;
    end;
  end;
  procedure DrawChangeColor(ACanvas: TCanvas);
  var
    ResBmp: TBitmap;
  begin
    ResBmp := TBitmap.Create;
    try
      ResBmp.LoadFromResourceName(HInstance, 'ChangePanel');
      ACanvas.CopyRect(Rect(0, 0, Width, Height), ResBmp.Canvas, Rect(0, 0, 1,29));
    finally
      ResBmp.Free;
    end;
  end;
begin
  if FChangedColor then
    DrawChangeColor(ACanvas)
  else
  begin
    ACanvas.Brush.Color := Color;
    ACanvas.FillRect(ClientRect);
  end;
  DrawPanelLine(ACanvas);
end;

procedure TFYPanel.DrawMainPanel(ACanvas: TCanvas);
var
  CR: TRect;
  ResBmp: TBitmap;
  h:Integer;
begin
  ResBmp := TBitmap.Create;
  ResBmp.LoadFromResourceName(HInstance, 'MAINPANEL');
  with ACanvas do
  begin
    Brush.Color := RGB(245,248,249);
    FillRect(Rect(0, 0, Width, Height));
    h := ResBmp.Height div 2;
    //Draw Top
    CopyRect(Rect(0, 0, 22, h-1), ResBmp.Canvas, Rect(0, 0, 22, h-1));
    CopyRect(Rect(22, 0, Width - 22, h-1), ResBmp.Canvas, Rect(22, 0, 24, h-1));
    CopyRect(Rect(Width - 22, 0, Width, h-1), ResBmp.Canvas, Rect(24, 0, 46, h-1));
    //Draw Bottom
    CopyRect(Rect(0, Height-h+1, 22, Height), ResBmp.Canvas, Rect(0, h+1, 22, 2*h));
    CopyRect(Rect(22, Height-h+1, Width - 22, Height), ResBmp.Canvas, Rect(22, h+1, 24, 2*h));
    CopyRect(Rect(Width - 22, Height-h+1, Width, Height), ResBmp.Canvas, Rect(24, h+1, 46, 2*h));

    //DrawLeftRight
    CopyRect(Rect(0, h-1, 14, Height - h+1), ResBmp.Canvas, Rect(0, h-1, 14, h+1));
    CopyRect(Rect(Width - 14, h-1, Width, Height - h+1), ResBmp.Canvas, Rect(32, h-1, 46, h+1));
    //Draw Contain---
    CopyRect(Rect(FContainLeft, Height - FContainBottom, Width - FContainRight, Height - h+1), ResBmp.Canvas, Rect(46, 0, 48, 2*h));
  end;
  ResBmp.Free;

  //»­°üº¬µÄÈÝÆ÷
  CR := Rect(FContainLeft, FContainTop, Width - FContainRight, Height - FContainBottom);
  with ACanvas do
  begin
    InflateRect(CR, -1, -1);
    Brush.Color :=  RGB(245,248,249);
    FrameRect(CR);
  end;
end;

procedure TFYPanel.DrawPanel(ACanvas: TCanvas);
var
  pStyle: TPanelStyle;
begin
  pStyle := FPanelStyle;
  if (pStyle = psOwnerDraw) and not Assigned(FOnPaint) then
    pStyle := psAdvance;
  with ACanvas do
  begin
    case pStyle of
      psAdvance: DrawAdvPanel(ACanvas);
      psMain: DrawMainPanel(ACanvas);
      psSmall: DrawSmallPanel(ACanvas);
      psChange: DrawChangePanel(ACanvas);
      psSetOpt: DrawSetOptPanel(ACanvas);
    end;
    if Assigned(FOnDrawPanel) then
      FOnDrawPanel(Self, ACanvas);
  end;
end;

procedure TFYPanel.DrawSetOptPanel(ACanvas: TCanvas);
var
  ResBmp: TBitmap;
  n, nWidth, nHeight: Integer;
begin
  ResBmp := TBitmap.Create;
  try
    ResBmp.LoadFromResourceName(HInstance, 'SetOptPnl');
    nWidth := ResBmp.Width;
    nHeight := ResBmp.Height;
    with ACanvas do
    begin
      //Top
      CopyRect(Rect(0, 0, 10, 18), ResBmp.Canvas, Rect(0, 0, 10, 18));
      CopyRect(Rect(10, 0, Width - 10, 18), ResBmp.Canvas, Rect(10, 0, 12, 18));
      CopyRect(Rect(Width - 10, 0, Width, 18), ResBmp.Canvas, Rect(20, 0, ResBmp.Width, 18));

      //Botton
      CopyRect(Rect(0, Height - 10, 10, Height), ResBmp.Canvas, Rect(0, nHeight - 10, 10, nHeight));
      CopyRect(Rect(10, Height - 10, Width - 10, Height), ResBmp.Canvas, Rect(10, nHeight - 10, nWidth - 10, nHeight));
      CopyRect(Rect(Width - 10, Height - 10, Width, Height), ResBmp.Canvas, Rect(nWidth - 10, nHeight - 10, nWidth, nHeight));

      n := Height - 18 * 2;
      if n > 0 then
      begin
        n := (n + 18 * 2) div 2;
        CopyRect(Rect(0, 18, 3, n), ResBmp.Canvas, Rect(0, 18, 3, 60));
        CopyRect(Rect(3, 18, Width - 3, n), ResBmp.Canvas, Rect(3, 18 , 4, 60));
        CopyRect(Rect(Width - 3, 18, Width, n), ResBmp.Canvas, Rect(nWidth - 3, 18, nWidth, 60));
      end
      else
        n := 18;

      CopyRect(Rect(0, n, 3, Height - 10), ResBmp.Canvas, Rect(0, 61, 3, 66));
      CopyRect(Rect(3, n, Width - 3, Height - 10), ResBmp.Canvas, Rect(3, 61, 4, 66));
      CopyRect(Rect(Width - 3, n , Width, Height - 10), ResBmp.Canvas, Rect(nWidth - 3, 61, nWidth, 66));
    end;
  finally
    ResBmp.Free;
  end;
end;

procedure TFYPanel.DrawSmallPanel(ACanvas: TCanvas);
var
  ResBmp: TBitmap;
  R: TRect;
begin
  ResBmp := TBitmap.Create;
  try
    ResBmp.LoadFromResourceName(HInstance, 'Small');
    ACanvas.CopyRect(Rect(0, 0, 1, Height), ResBmp.Canvas, Rect(0, 0, 1, 22));
    ACanvas.CopyRect(Rect(1, 0, Width - 1, Height), ResBmp.Canvas, Rect(1, 0, 2, 22));
    ACanvas.CopyRect(Rect(Width - 1, 0, Width, Height), ResBmp.Canvas, Rect(0, 0, 1, 22));
    R := Rect(0, 0, Width, Height);
    ACanvas.Brush.Style := bsClear;
    DrawText(ACanvas.Handle, PChar(Caption),Length(Caption), R, DT_VCENTER or DT_SINGLELINE or DT_CENTER);
  finally
    ResBmp.Free;
  end;
end;

procedure TFYPanel.Paint;
begin
  DrawPanel(Canvas);
end;

procedure TFYPanel.ScrollText(const AText: string; const ATime: string);
var
  R: TRect;
begin
  FScrollText := AText;
  FScrollTime := ATime;
  R := Rect(FContainLeft, FContainTop, Width - FContainRight, Height - FContainBottom);
  Invalidate; //Rect(Handle, @R, True);
end;

procedure TFYPanel.SetBottonLine(const Value: Boolean);
begin
  if FBottonLine <> Value then
  begin
    FBottonLine := Value;
    Invalidate;
  end;
end;

procedure TFYPanel.SetBrushColor(const Value: TColor);
begin
  if Value <> FBrushColor then
  begin
    FBrushColor := Value;
    Brush.Color := FBrushColor;
    Invalidate;
  end;
end;

procedure TFYPanel.SetChangedColor(const Value: Boolean);
begin
  if FChangedColor <> Value then
  begin
    FChangedColor := Value;
    Invalidate;
  end;
end;

procedure TFYPanel.SetClientSize(const Index: Integer; const Value: WORD);
begin
  case Index of
    0: FClientLeft := Value;
    1: FClientTop := Value;
    2: FClientRight := Value;
    3: FClientBottom := Value;
  end;
  Realign;
  Invalidate;
end;

procedure TFYPanel.SetContainSize(const Index: Integer; const Value: WORD);
begin
  case Index of
    0: FContainLeft := Value;
    1: FContainTop := Value;
    2: FContainRight := Value;
    3: FContainBottom := Value;
  end;
  Invalidate;
end;

procedure TFYPanel.SetEndColor(const Value: TColor);
begin
  FEndColor := Value;
  Paint;
end;

procedure TFYPanel.SetFillDirection(const Value: TFillDirection);
begin
  FFillDirection := Value;
  Paint;
end;

procedure TFYPanel.SetFrameColor(const Value: TColor);
begin
  FFrameColor := Value;
  Paint;
end;

procedure TFYPanel.SetIndent(const Value: Integer);
begin
  FIndent := Value;
  Paint;
end;

procedure TFYPanel.SetLeftLine(const Value: Boolean);
begin
  if FLeftLine <> Value then
  begin
    FLeftLine := Value;
    Invalidate;
  end;
end;

procedure TFYPanel.SetLineColor(const Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Invalidate;
  end;
end;

procedure TFYPanel.SetLineWidth(const Value: Integer);
begin
  if FLineWidth <> Value then
  begin
    FLineWidth := Value;
    Invalidate;
  end;
end;

procedure TFYPanel.SetPanelStyle(const Value: TPanelStyle);
begin
  FPanelStyle := Value;
  if csDesigning in ComponentState then
  begin
    case FPanelStyle of
      psAdvance:
        begin
          FContainLeft := 7;
          FContainRight := 7;
          FContainTop := 88;
          FContainBottom := 65;
        end;
      psMain:
        begin
          FContainLeft := 22;
          FContainRight := 22;
          FContainTop := 77;
          FContainBottom := 74;
        end;
      else
        begin
        FContainLeft := 50;
        FContainRight := Width - 10;
        FContainTop := 0;
        FContainBottom := Height;
        end;
    end;
  end;
  Paint;
end;


procedure TFYPanel.SetRightLine(const Value: Boolean);
begin
  if FRightLine <> Value then
  begin
    FRightLine := Value;
    Invalidate;
  end;
end;

procedure TFYPanel.SetRoundValue(const Value: Integer);
begin
  if Value <> FRoundValue then
  begin
    FRoundValue := Value;
    Invalidate;
  end;
end;

procedure TFYPanel.SetStartColor(const Value: TColor);
begin
  FStartColor := Value;
  Invalidate;
end;

procedure TFYPanel.SetTopLine(const Value: Boolean);
begin
  if FTopLine <> Value then
  begin
    FTopLine := Value;
    Invalidate;
  end;
end;

procedure TFYPanel.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

procedure TFYPanel.WMWindowPosChanged(var Message: TWMWindowPosChanged);
var
  BevelPixels: Integer;
  Rect: TRect;
begin
  if FullRepaint then
    Invalidate
  else begin
    BevelPixels := Indent + 1;
    Rect.Right := Width;
    Rect.Bottom := Height;
    if Message.WindowPos^.cx <> Rect.Right then
    begin
      if FPanelStyle = psAdvance then
        BevelPixels := FContainRight + 7
      else if FPanelStyle = psMain then
        BevelPixels := FContainBottom + 22;
      Rect.Top := 0;
      Rect.Left := Rect.Right - BevelPixels - 1;
      InvalidateRect(Handle, @Rect, True);
    end;
    if Message.WindowPos^.cy <> Rect.Bottom then
    begin
      Rect.Left := 0;
      if FPanelStyle = psAdvance then
        BevelPixels := FContainBottom + 7
      else if FPanelStyle = psMain then
        BevelPixels := FContainBottom + 22;

      Rect.Top := Rect.Bottom - BevelPixels - 1;
      InvalidateRect(Handle, @Rect, True);
    end;
  end;
  inherited;
end;

end.

