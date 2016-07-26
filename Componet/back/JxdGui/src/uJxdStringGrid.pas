unit uJxdStringGrid;

interface
uses
  Windows, Messages, uBitmapHandle, Grids, Classes, StdCtrls, uJxdScrollBar, Controls, ExtCtrls, uJxdParseGradient,
  SysUtils, Graphics;

type
  TJxdStringGrid = class(TStringGrid)
  private
    FScrollTime: TTimer;
    FFontCenter: Boolean;
    procedure DoScrollTimer(Sender: TObject);
  private
    {垂直滚动条}
    FVScrollBar: TJxdScrollBar;
    FVScrollChaning: Boolean;
    procedure ReDrawVScrollBar;
    procedure DoVScrollBarPosChange(Sender: TObject);
  private
    {水平滚动条}
    FHScrollBar: TJxdScrollBar;
    FHScrollChaning: Boolean;
    procedure ReDrawHScrollBar;
    procedure DoHScrollBarPosChange(Sender: TObject);
  private
    {Fixed Draw}
    FFixedRowParseGradient: TParseGradient;
    FFixedColGradientWay: TGradientWay;
    FFixedColGradientText: string;

    FFixedColParseGradient: TParseGradient;
    FFixedRowGradientWay: TGradientWay;
    FFixedRowGradientText: string;

    FActiveCellParseGradient: TParseGradient;
    FActiveCellGradientWay: TGradientWay;
    FActiveCellGradientText: string;
    FCellLineColor: TColor;

    procedure SetVScrollBar(const Value: TJxdScrollBar);
    procedure SetHScrollBar(const Value: TJxdScrollBar);
    procedure SetFixedRowGradientText(const Value: string);
    procedure SetFixedRowGradientWay(const Value: TGradientWay);
    procedure SetFixedColGradientText(const Value: string);
    procedure SetFixedColGradientWay(const Value: TGradientWay);
    procedure SetFontCenter(const Value: Boolean);
    procedure SetActiveCellGradientText(const Value: string);
    procedure SetActiveCellGradientWay(const Value: TGradientWay);
    procedure SetCellLineColor(const Value: TColor);
  protected
    procedure Paint; override;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure ColWidthsChanged; override;
    procedure RowHeightsChanged; override;
    procedure TopLeftChanged; override;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FontCenter: Boolean read FFontCenter write SetFontCenter;
    property VScrollBar: TJxdScrollBar read FVScrollBar write SetVScrollBar;
    property HScrollBar: TJxdScrollBar read FHScrollBar write SetHScrollBar;
    property FixedRowGradientText: string read FFixedRowGradientText write SetFixedRowGradientText;
    property FixedRowGradientWay: TGradientWay read FFixedRowGradientWay write SetFixedRowGradientWay;
    property FixedColGradientText: string read FFixedColGradientText write SetFixedColGradientText;
    property FixedColGradientWay: TGradientWay read FFixedColGradientWay write SetFixedColGradientWay;
    property ActiveCellGradientText: string read FActiveCellGradientText write SetActiveCellGradientText;
    property ActiveCellGradientWay: TGradientWay read FActiveCellGradientWay write SetActiveCellGradientWay;
    property CellLineColor: TColor read FCellLineColor write SetCellLineColor;
  end;

implementation

{ TJxdStringGrid }

procedure TJxdStringGrid.ColWidthsChanged;
begin
  inherited;
  ReDrawVScrollBar;
  ReDrawHScrollBar;
  InvalidateGrid;
end;

constructor TJxdStringGrid.Create(AOwner: TComponent);
begin
  inherited;
  FVScrollBar := nil;
  ScrollBars := ssNone;
  DefaultRowHeight := 21;
  FVScrollChaning := False;
  FScrollTime := nil;
  FHScrollChaning := False;
  FHScrollBar := nil;
  FFontCenter := True;
  FCellLineColor := $00FFCC75;

  FFixedRowParseGradient := TParseGradient.Create;
  FixedRowGradientText := '#Gradient{fromColor: $E0A77E; ColorTo: $FDFDFE; percent: 40%}' +
                  '#Gradient{fromColor: $FDFDFE; ColorTo: $E09865; percent: 60%}';
  FFixedColParseGradient := TParseGradient.Create;
  FixedColGradientText := '#Gradient{fromColor: $E0A77E; ColorTo: $FDFDFE; percent: 40%}' +
                  '#Gradient{fromColor: $FDFDFE; ColorTo: $E09865; percent: 60%}';
  FActiveCellParseGradient := TParseGradient.Create;
  ActiveCellGradientText := '#Gradient{fromColor: $FFEFD3; ColorTo: $FED999; percent: 20%}' +
                            '#Gradient{fromColor: $FED999; ColorTo: $FFCD76; percent: 20%}' +
                            '#Gradient{fromColor: $FFCD76; ColorTo: $FFCC75; percent: 20%}' +
                            '#Gradient{fromColor: $FFCC75; ColorTo: $FFCD76; percent: 20%}' +
                            '#Gradient{fromColor: $FFCD76; ColorTo: $FFEFD3; percent: 20%}';
  ActiveCellGradientWay := gwLeftToRigth;
end;

destructor TJxdStringGrid.Destroy;
begin
  FreeAndNil( FActiveCellParseGradient );
  FreeAndNil( FFixedRowParseGradient );
  FreeAndNil( FFixedColGradientWay );
  inherited;
end;

procedure TJxdStringGrid.DoHScrollBarPosChange(Sender: TObject);
begin
 if FVScrollChaning then Exit;
  FVScrollChaning := True;
  try
    LeftCol := FHScrollBar.Position + FixedCols;
  finally
    FVScrollChaning := False;
  end;
end;

procedure TJxdStringGrid.DoVScrollBarPosChange(Sender: TObject);
begin
  if FVScrollChaning then Exit;
  FVScrollChaning := True;
  try
    TopRow := FVScrollBar.Position + FixedRows;
  finally
    FVScrollChaning := False;
  end;
end;

procedure TJxdStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  strText: string;
  nStyle: Cardinal;
begin
  if (FixedRows > 0) and (FixedRows - ARow > 0) then
  begin
    DrawColorGradient( Canvas, FFixedRowParseGradient, FFixedRowGradientWay, ARect )
  end
  else if (FixedCols > 0) and (FixedCols - ACol > 0) then
    DrawColorGradient( Canvas, FFixedColParseGradient, FFixedColGradientWay, ARect )
  else
  begin
    if (gdSelected in AState) or (gdFocused in AState) then
       DrawColorGradient( Canvas, FActiveCellParseGradient, FActiveCellGradientWay, ARect );
  end;
  strText := Cells[ACol, ARow];
  Canvas.Brush.Style := bsClear;
  if FFontCenter then
    nStyle := DT_CENTER or DT_VCENTER or DT_SINGLELINE
  else
    nStyle := DT_SINGLELINE;
  DrawText( Canvas.Handle, PChar(strText), Length(strText), ARect, nStyle );
  Canvas.Pen.Color := FCellLineColor;
  Canvas.Pen.Width := GridLineWidth;
  Canvas.MoveTo( ARect.Left, ARect.Bottom );
  Canvas.LineTo( ARect.Right, ARect.Bottom );
  Canvas.LineTo( ARect.Right, ARect.Top );
end;

procedure TJxdStringGrid.Paint;
begin;
  inherited;
  DrawFrameBorder( Canvas, FCellLineColor, GridLineWidth, ClientRect );
end;

procedure TJxdStringGrid.DoScrollTimer(Sender: TObject);
begin
  (Sender as TTimer).Enabled := False;
  ReDrawVScrollBar;
  ReDrawHScrollBar;
  FScrollTime.Free;;
  FScrollTime := nil;
end;

procedure TJxdStringGrid.ReDrawHScrollBar;
begin
  if FHScrollChaning then Exit;
  FHScrollChaning := True;
  try
    if Assigned(FHScrollBar) then
    begin
      FHScrollBar.Visible := ColCount <> (FixedCols + VisibleColCount);
      if FHScrollBar.Visible then
      begin
        FHScrollBar.Max := ColCount - FixedCols - VisibleColCount;
        FHScrollBar.Position := LeftCol - FixedCols;
      end;
    end;
  finally
    FHScrollChaning := False;
  end;
end;

procedure TJxdStringGrid.ReDrawVScrollBar;
begin
  if FVScrollChaning then Exit;
  FVScrollChaning := True;
  try
    if Assigned(FVScrollBar) then
    begin
      FVScrollBar.Visible := RowCount <> (FixedRows + VisibleRowCount);
      if FVScrollBar.Visible then
      begin
        FVScrollBar.Max := RowCount - FixedRows - VisibleRowCount;
        FVScrollBar.Position := TopRow - FixedRows;
      end;
    end;
  finally
    FVScrollChaning := False;
  end;
end;

procedure TJxdStringGrid.RowHeightsChanged;
begin
  inherited;
  ReDrawVScrollBar;
  ReDrawHScrollBar;
  InvalidateGrid;
end;

procedure TJxdStringGrid.SetActiveCellGradientText(const Value: string);
begin
  if (Value <> '') and (CompareText(FActiveCellGradientText, Value) <> 0) then
  begin
    FActiveCellParseGradient.GradientMsg := Value;
    FActiveCellGradientText := Value;
    Invalidate;
  end;
end;

procedure TJxdStringGrid.SetActiveCellGradientWay(const Value: TGradientWay);
begin
  if FActiveCellGradientWay <> Value then
  begin
    FActiveCellGradientWay := Value;
    Invalidate;
  end;
end;

procedure TJxdStringGrid.SetCellLineColor(const Value: TColor);
begin
  if CellLineColor <> Value then
  begin
    FCellLineColor := Value;
    InvalidateGrid;
  end;
end;

procedure TJxdStringGrid.SetFixedColGradientText(const Value: string);
begin
  if (Value <> '') and (CompareText(FFixedColGradientText, Value) <> 0) then
  begin
    FFixedColParseGradient.GradientMsg := Value;
    FFixedColGradientText := Value;
    Invalidate;
  end;
end;

procedure TJxdStringGrid.SetFixedColGradientWay(const Value: TGradientWay);
begin
  if FFixedColGradientWay <> Value then
  begin
    FFixedColGradientWay := Value;
    Invalidate;
  end;
end;

procedure TJxdStringGrid.SetFixedRowGradientText(const Value: string);
begin
  if (Value <> '') and (CompareText(FFixedRowGradientText, Value) <> 0) then
  begin
    FFixedRowParseGradient.GradientMsg := Value;
    FFixedRowGradientText := Value;
    Invalidate;
  end;
end;

procedure TJxdStringGrid.SetFixedRowGradientWay(const Value: TGradientWay);
begin
  if FFixedRowGradientWay <> Value then
  begin
    FFixedRowGradientWay := Value;
    Invalidate;
  end;
end;

procedure TJxdStringGrid.SetFontCenter(const Value: Boolean);
begin
  if FFontCenter <> Value then
  begin
    FFontCenter := Value;
    InvalidateGrid;
  end;
end;

procedure TJxdStringGrid.SetHScrollBar(const Value: TJxdScrollBar);
begin
  if FHScrollBar <> Value then
  begin
    FHScrollBar := Value;
    if Assigned(FHScrollBar) then
    begin
      FHScrollBar.Min := 0;
      FHScrollBar.Position := 0;
      FHScrollBar.OnPosChange := DoHScrollBarPosChange;
    end;
  end;
end;

procedure TJxdStringGrid.SetVScrollBar(const Value: TJxdScrollBar);
begin
  if FVScrollBar <> Value then
  begin
    FVScrollBar := Value;
    if Assigned(FVScrollBar) then
    begin
      FVScrollBar.Min := 0;
      FVScrollBar.Position := 0;
      FVScrollBar.OnPosChange := DoVScrollBarPosChange;
    end;
  end;
end;

procedure TJxdStringGrid.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  inherited;
  ReDrawVScrollBar;
  ReDrawHScrollBar;
  InvalidateGrid;
end;

procedure TJxdStringGrid.TopLeftChanged;
begin
  inherited;
  ReDrawVScrollBar;
  ReDrawHScrollBar;
  InvalidateGrid;
end;

procedure TJxdStringGrid.WMSize(var Msg: TWMSize);
begin
  inherited;
  if ( Assigned(FVScrollBar) or Assigned(FHScrollBar) ) and not Assigned(FScrollTime) then
  begin
    FScrollTime := TTimer.Create( Self );
    FScrollTime.OnTimer := DoScrollTimer;
    FScrollTime.Interval := 10;
    FScrollTime.Enabled := True;
  end;
  InvalidateGrid;
end;

end.
