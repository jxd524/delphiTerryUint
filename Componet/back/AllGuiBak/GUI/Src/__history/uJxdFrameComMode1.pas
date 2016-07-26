//画标准界面,如下所示
//                -------------------------
//                |       top             |
//                |-----------------------|
//                |     |          |      |
//                |     |          |      |
//                | left| client   |right |
//                |     |          |      |
//                |     |          |      |
//                |-----------------------|
//                |      bottom           |
//                -------------------------
//
unit uJxdFrameComMode1;

interface

uses
  SysUtils, Classes, Controls, Graphics, uJxdGradientPanel;

type
  TJxdFrameComMode1 = class(TJxdGradientPanel)
  private
    { Private declarations }
    FTopHeight: Integer;
    FTopLineVisible: Boolean;
    FTopLineColor: TColor;
    FTopLineWidth: Integer;

    FBottomHeight: Integer;
    FBottomLineVisible: Boolean;
    FBottomLineColor: TColor;
    FBottomLineWidth: Integer;

    FLeftWidth: Integer;
    FLeftLineVisible: Boolean;
    FLeftLineWidth: Integer;
    FLeftLineColor: TColor;
    
    FRightLineWidth: Integer;
    FRightLineColor: TColor;
    FRightLineVisible: Boolean;
    FRightWidth: Integer;

    procedure DrawVerticalLine(const ALeftWidthSpace, LineWidth: Integer; const ALineColor: TColor);
    procedure DrawHorizontalLine(const ATopSpace, LineWidth: Integer; const ALineColor: TColor);

    procedure SetLeftWidth(const Value: Integer);
    procedure SetTopHeight(const Value: Integer);
    procedure SetBottomHeight(const Value: Integer);
    procedure SetTopLineVisible(const Value: Boolean);
    procedure SetTopLineColor(const Value: TColor);
    procedure SetTopLineWidth(const Value: Integer);
    procedure SetBottomLineColor(const Value: TColor);
    procedure SetBottomLineVisible(const Value: Boolean);
    procedure SetBottomLineWidth(const Value: Integer);
    procedure SetLeftLineColor(const Value: TColor);
    procedure SetLeftLineVisible(const Value: Boolean);
    procedure SetLeftLineWidth(const Value: Integer);
    procedure SetRightLineColor(const Value: TColor);
    procedure SetRightLineVisible(const Value: Boolean);
    procedure SetRightLineWidth(const Value: Integer);
    procedure SetRightWidth(const Value: Integer);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
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
    property GradientText;
    property FrameLienColor;
    property FrameLienWidth;
    property FrameLienVisible;

    property TopHeight: Integer read FTopHeight write SetTopHeight default 35;
    property TopLineVisible: Boolean read FTopLineVisible write SetTopLineVisible;
    property TopLineColor: TColor read FTopLineColor write SetTopLineColor;
    property TopLineWidth: Integer read FTopLineWidth write SetTopLineWidth;

    property BottomHeight: Integer read FBottomHeight write SetBottomHeight default 35;
    property BottomLineVisible: Boolean read FBottomLineVisible write SetBottomLineVisible;
    property BottomLineColor: TColor read FBottomLineColor write SetBottomLineColor;
    property BottomLineWidth: Integer read FBottomLineWidth write SetBottomLineWidth;

    property LeftWidth: Integer read FLeftWidth write SetLeftWidth default 145;
    property LeftLineVisible: Boolean read FLeftLineVisible write SetLeftLineVisible;
    property LeftLineColor: TColor read FLeftLineColor write SetLeftLineColor;
    property LeftLineWidth: Integer read FLeftLineWidth write SetLeftLineWidth;

    property RightWidth: Integer read FRightWidth write SetRightWidth default 145;
    property RightLineVisible: Boolean read FRightLineVisible write SetRightLineVisible;
    property RightLineColor: TColor read FRightLineColor write SetRightLineColor;
    property RightLineWidth: Integer read FRightLineWidth write SetRightLineWidth;
  end;

implementation


{ TJxdFrameComMode1 }

constructor TJxdFrameComMode1.Create(AOwner: TComponent);
begin
  inherited;
  //变量初始化
  TopLineVisible := True;
  TopHeight := 35;
  TopLineWidth := 1;
  TopLineColor := $00FCB37E;

  BottomLineVisible := True;
  BottomHeight := 35;
  BottomLineWidth := 1;
  BottomLineColor := $00FCB37E;

  LeftLineVisible := True;
  LeftWidth := 145;
  LeftLineWidth := 1;
  LeftLineColor := $00FCB37E;

  RightLineVisible := True;
  RightWidth := 145;
  RightLineWidth := 1;
  RightLineColor := $00FCB37E;
end;

destructor TJxdFrameComMode1.Destroy;
begin

  inherited;
end;

procedure TJxdFrameComMode1.DrawHorizontalLine(const ATopSpace, LineWidth: Integer; const ALineColor: TColor);
var
  X, X1, Y: Integer;
  OldColor: Tcolor;
  OldWidth: Integer;
begin
  if FrameLienVisible then
  begin
    X := FrameLienWidth;
    X1 := Width - X;
  end
  else
  begin
    X := 0;
    X1 := Width;
  end;
  Y := ATopSpace;

  with Canvas do
  begin
    OldColor := Pen.Color;
    OldWidth := Pen.Width;
    Pen.Color := ALineColor;
    Pen.Width := LineWidth;
    MoveTo(X, Y);
    LineTo(X1, Y);
    Pen.Color := OldColor;
    Pen.Width := OldWidth;
  end;
end;

procedure TJxdFrameComMode1.DrawVerticalLine(const ALeftWidthSpace, LineWidth: Integer;
  const ALineColor: TColor);
var
  X, Y, Y1: Integer;
  OldColor: Tcolor;
  OldWidth: Integer;
begin
  X := ALeftWidthSpace;
  Y := TopHeight;
  Y1 := Height - BottomHeight; 

  with Canvas do
  begin
    OldColor := Pen.Color;
    OldWidth := Pen.Width;
    Pen.Color := ALineColor;
    Pen.Width := LineWidth;
    MoveTo(X, Y);
    LineTo(X, Y1);
    Pen.Color := OldColor;
    Pen.Width := OldWidth;
  end;
end;

procedure TJxdFrameComMode1.Paint;
begin
  inherited;
  if TopLineVisible then DrawHorizontalLine( TopHeight, TopLineWidth, TopLineColor );
  if BottomLineVisible then DrawHorizontalLine( BottomHeight, BottomLineWidth, BottomLineColor );
  if LeftLineVisible then DrawVerticalLine( LeftLineWidth, LeftLineWidth, LeftLineColor );
  
end;

procedure TJxdFrameComMode1.SetBottomHeight(const Value: Integer);
begin
  if FBottomHeight <> Value then
  begin
    FBottomHeight := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetBottomLineColor(const Value: TColor);
begin
  if FBottomLineColor <> Value then
  begin
    FBottomLineColor := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetBottomLineVisible(const Value: Boolean);
begin
  if FBottomLineVisible <> Value then
  begin
    FBottomLineVisible := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetBottomLineWidth(const Value: Integer);
begin
  if FBottomLineWidth <> Value then
  begin
    FBottomLineWidth := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetLeftLineColor(const Value: TColor);
begin
  if FLeftLineColor <> Value then
  begin
    FLeftLineColor := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetLeftLineVisible(const Value: Boolean);
begin
  if FLeftLineVisible <> Value then
  begin
    FLeftLineVisible := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetLeftLineWidth(const Value: Integer);
begin
  if FLeftLineWidth <> Value then
  begin
    FLeftLineWidth := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetLeftWidth(const Value: Integer);
begin
  if FLeftWidth <> Value then
  begin
    FLeftWidth := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetRightLineColor(const Value: TColor);
begin
  if FRightLineColor <> Value then
  begin
    FRightLineColor := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetRightLineVisible(const Value: Boolean);
begin
  if FRightLineVisible <> Value then
  begin
    FRightLineVisible := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetRightLineWidth(const Value: Integer);
begin
  if FRightLineWidth <> Value then
  begin
    FRightLineWidth := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetRightWidth(const Value: Integer);
begin
  if FRightWidth <> Value then
  begin
    FRightWidth := Value;
    Invalidate
  end;
end;

procedure TJxdFrameComMode1.SetTopHeight(const Value: Integer);
begin
  if FTopHeight <> Value then
  begin
    FTopHeight := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetTopLineColor(const Value: TColor);
begin
  if FTopLineColor <> Value then
  begin
    FTopLineColor := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetTopLineVisible(const Value: Boolean);
begin
  if FTopLineVisible <> Value then
  begin
    FTopLineVisible := Value;
    Invalidate;
  end;
end;

procedure TJxdFrameComMode1.SetTopLineWidth(const Value: Integer);
begin
  if FTopLineWidth <> Value then
  begin
    FTopLineWidth := Value;
    Invalidate;
  end;
end;

end.
