unit uJxdStringGrid;

interface
uses
  Windows, Messages, Grids, Classes, StdCtrls, uJxdScrollBar, Controls, ExtCtrls, uJxdParseGradient, uJxdGuiStyle,
  SysUtils, Graphics, uJxdDrawSub, Forms, uJxdCustomScrollBar;

type
  TxdStringGrid = class(TStringGrid)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    property BevelInner;
    procedure Paint; override;
    procedure Loaded; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    procedure TopLeftChanged; override;
    procedure Resize; override;
  private
    FHorizontalScrollBar: TxdScrollBar;
    procedure CheckScrollBar;
    procedure ChangedScrollBarPos;
    procedure DoDrawObjectChanged(Sender: TObject);
    procedure DoHorizontalScrollPosChanged(Sender: TObject; const IsChangedBySelf: Boolean);
    procedure InitSetting;
  private
    FLineDrawInfo: TLineDrawInfo;
    FFixedGradientDrawInfo: TGradientDrawInfo;
    FFixedBitmap: TBitmapInfo;
    FTransColor: TColor;
    FIsTransColor: Boolean;
    FCellGradientDrawInfo: TGradientDrawInfo;
    FCellBitmap: TBitmapInfo;
    FSBHorBmpFileName: string;
    procedure SetLineDrawInfo(const Value: TLineDrawInfo);
    function  GetFixedRows: Integer;
    procedure SetFixedRows(const Value: Integer);
    function  GetFixedCols: Integer;
    procedure SetfixedCols(const Value: Integer);
    procedure SetFixedRowDrawInfo(const Value: TGradientDrawInfo);
    procedure SetFixedBitmap(const Value: TBitmapInfo);
    procedure SetIsTransColor(const Value: Boolean);
    procedure SetTransColor(const Value: TColor);
    procedure SetCellGradientDrawInfo(const Value: TGradientDrawInfo);
    procedure SetCellBitmap(const Value: TBitmapInfo);
    procedure SetSBHorBmpFileName(const Value: string);
  published
    property FixedRows: Integer read GetFixedRows write SetFixedRows;
    property FixedCols: Integer read GetFixedCols write SetfixedCols;
    property FixedGradientDrawInfo: TGradientDrawInfo read FFixedGradientDrawInfo write SetFixedRowDrawInfo;
    property FixedBitmap: TBitmapInfo read FFixedBitmap write SetFixedBitmap;
    property CellGradientDrawInfo: TGradientDrawInfo read FCellGradientDrawInfo write SetCellGradientDrawInfo;
    property CellBitmap: TBitmapInfo read FCellBitmap write SetCellBitmap;

    property ScrollBarHorizontalBitmapFileName: string read FSBHorBmpFileName write SetSBHorBmpFileName;

    property LineDrawInfo: TLineDrawInfo read FLineDrawInfo write SetLineDrawInfo;
    property TransColor: TColor read FTransColor write SetTransColor default clFuchsia;
    property IsTransColor: Boolean read FIsTransColor write SetIsTransColor default True;
  end;

implementation

{ TxdStringGrid }

constructor TxdStringGrid.Create(AOwner: TComponent);
begin
  inherited;
  FSBHorBmpFileName := '';
  FIsTransColor := True;
  FTransColor := clFuchsia;
  InitSetting;
  FLineDrawInfo := TLineDrawInfo.Create;
  FLineDrawInfo.OnChange := DoDrawObjectChanged;
  FFixedGradientDrawInfo := TGradientDrawInfo.Create;
  FFixedGradientDrawInfo.OnChange := DoDrawObjectChanged;
  FFixedBitmap := TBitmapInfo.Create;
  FFixedBitmap.OnChange := DoDrawObjectChanged;
  FCellGradientDrawInfo := TGradientDrawInfo.Create;
  FCellGradientDrawInfo.OnChange := DoDrawObjectChanged;
  FCellBitmap := TBitmapInfo.Create;
  FCellBitmap.OnChange := DoDrawObjectChanged;
  FHorizontalScrollBar := nil;
end;

procedure TxdStringGrid.ChangedScrollBarPos;
begin
  if Assigned(FHorizontalScrollBar) then
  begin
    FHorizontalScrollBar.OnPositionChanged := nil;
    FHorizontalScrollBar.Position := TopRow - FixedRows;
    FHorizontalScrollBar.OnPositionChanged := DoHorizontalScrollPosChanged;
  end;
end;

procedure TxdStringGrid.CheckScrollBar;
var
  bVisible: Boolean;
  nCount: Integer;
begin
  nCount := RowCount - FixedRows;
  bVisible := nCount <> VisibleRowCount;
  if bVisible then
  begin
    if FHorizontalScrollBar = nil then
    begin
      FHorizontalScrollBar := TxdScrollBar.Create( Self );
      FHorizontalScrollBar.Parent := Self;
      FHorizontalScrollBar.OnPositionChanged := DoHorizontalScrollPosChanged;
      if (FSBHorBmpFileName <> '') and FileExists(FSBHorBmpFileName) then
        FHorizontalScrollBar.ScrollBarBitmap.Bitmap.LoadFromFile( FSBHorBmpFileName );
      FHorizontalScrollBar.ScrollBarBitmap.BitmapDrawStyle := dsStretchyUpToDown;
      FHorizontalScrollBar.ScrollBarStyle := ssHorizontal;
      FHorizontalScrollBar.Align := alRight;
      FHorizontalScrollBar.Width := FHorizontalScrollBar.ScrollBarBitmap.Bitmap.Width;
      FHorizontalScrollBar.Position := 0;
      FHorizontalScrollBar.Visible := True;
    end;
    FHorizontalScrollBar.Max := nCount - VisibleRowCount;
  end
  else if Assigned(FHorizontalScrollBar) then
    FreeAndNil( FHorizontalScrollBar );
end;

destructor TxdStringGrid.Destroy;
begin
  FLineDrawInfo.Free;
  FFixedGradientDrawInfo.Free;
  FCellGradientDrawInfo.Free;
  FCellBitmap.Free;
  inherited;
end;

procedure TxdStringGrid.DoDrawObjectChanged(Sender: TObject);
begin
  InvalidateGrid;
end;

procedure TxdStringGrid.DoHorizontalScrollPosChanged(Sender: TObject; const IsChangedBySelf: Boolean);
begin
  if IsChangedBySelf and Assigned(FHorizontalScrollBar) then
    TopRow := FHorizontalScrollBar.Position;
end;

procedure TxdStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  BmpInfo: TBitmapInfo;
  DrawInfo: TGradientDrawInfo;

  bDrawBitmap, bDriection: Boolean;
  Gradient: TParseGradient;
  SrcR: TRect;

  nH: Integer;
  strText: string;
begin
  if gdFixed in AState then
  begin
    BmpInfo := FixedBitmap;
    DrawInfo := FixedGradientDrawInfo;
  end
  else
  begin
    BmpInfo := CellBitmap;
    DrawInfo := CellGradientDrawInfo;
  end;

  bDrawBitmap := (BmpInfo.Bitmap.Width > 0) and (BmpInfo.Bitmap.Height > 0);
  if bDrawBitmap then
  begin
    nH := BmpInfo.Bitmap.Height div BmpInfo.BitmapCount;
    SrcR := Rect( 0, 0, BmpInfo.Bitmap.Width, nH );
    if gdFocused in AState then
    begin
      if BmpInfo.BitmapCount >= 3 then
        OffsetRect( SrcR, 0, nH * 2 )
      else if BmpInfo.BitmapCount >= 3 then
        OffsetRect( SrcR, 0, nH );
    end
    else if gdSelected in AState then
    begin
      if BmpInfo.BitmapCount >= 2 then
        OffsetRect( SrcR, 0, nH );
    end;
    DrawRectangle( BmpInfo.Bitmap, Canvas, SrcR, ARect, BmpInfo.BitmapDrawStyle, IsTransColor, TransColor )
  end
  else
  begin
    bDriection := DrawInfo.GradientWay = gwLeftToRigth;
    if gdFocused in AState then
      Gradient := DrawInfo.ParseGradientMouseDown
    else if gdSelected in AState then
      Gradient := DrawInfo.ParseGradientHover
    else
      Gradient := DrawInfo.ParseGradientNormal;
    DrawGradientInfo( Canvas, Gradient, bDriection, ARect.Left, ARect.Top, WidthOfRect(ARect), HeightOfRect(ARect) );
  end;

  strText := Cells[ACol, ARow];
  if strText <> '' then
  begin
    Canvas.Brush.Style := bsClear;
    DrawText( Canvas.Handle, PChar(strText), Length(strText), ARect, DT_CENTER or DT_VCENTER or DT_SINGLELINE );
  end;
end;

function TxdStringGrid.GetFixedCols: Integer;
begin
  Result := inherited FixedCols;
end;

function TxdStringGrid.GetFixedRows: Integer;
begin
  Result := inherited FixedRows;
end;

procedure TxdStringGrid.InitSetting;
begin
  BevelInner := bvNone;
  BevelKind := bkNone;
  BevelOuter := bvNone;
  BorderStyle := bsNone;
  Ctl3D := False;
  DefaultDrawing := False;
  ScrollBars := ssNone;
end;

procedure TxdStringGrid.Loaded;
begin
  inherited;
  InitSetting;
end;

procedure TxdStringGrid.Paint;
begin
  inherited;
  DrawLinesInfo( Canvas, FLineDrawInfo, Width, Height );
end;

procedure TxdStringGrid.Resize;
begin
  inherited;
  CheckScrollBar;
end;

procedure TxdStringGrid.SetCellBitmap(const Value: TBitmapInfo);
begin
  FCellBitmap := Value;
  Invalidate;
end;

procedure TxdStringGrid.SetCellGradientDrawInfo(const Value: TGradientDrawInfo);
begin
  FCellGradientDrawInfo := Value;
  Invalidate;
end;

procedure TxdStringGrid.SetFixedBitmap(const Value: TBitmapInfo);
begin
  FFixedBitmap := Value;
  Invalidate;
end;

procedure TxdStringGrid.SetfixedCols(const Value: Integer);
begin
  if Value <= 1 then
    inherited FixedCols := Value;
end;

procedure TxdStringGrid.SetFixedRowDrawInfo(const Value: TGradientDrawInfo);
begin
  FFixedGradientDrawInfo := Value;
end;

procedure TxdStringGrid.SetFixedRows(const Value: Integer);
begin
  if Value <= 1 then
    inherited FixedRows := Value;
end;

procedure TxdStringGrid.SetIsTransColor(const Value: Boolean);
begin
  if FIsTransColor <> Value then
  begin
    FIsTransColor := Value;
    InvalidateGrid;
  end;
end;

procedure TxdStringGrid.SetLineDrawInfo(const Value: TLineDrawInfo);
begin
  FLineDrawInfo := Value;
end;

procedure TxdStringGrid.SetSBHorBmpFileName(const Value: string);
begin
  if FileExists(Value) then
  begin
    FSBHorBmpFileName := Value;
    if Assigned(FHorizontalScrollBar) then
    begin
      FHorizontalScrollBar.ScrollBarBitmap.Bitmap.LoadFromFile( FSBHorBmpFileName );
      FHorizontalScrollBar.Width := FHorizontalScrollBar.ScrollBarBitmap.Bitmap.Width;
    end;
  end;
end;

procedure TxdStringGrid.SetTransColor(const Value: TColor);
begin
  if FTransColor <> Value then
  begin
    FTransColor := Value;
    InvalidateGrid;
  end;
end;

procedure TxdStringGrid.SizeChanged(OldColCount, OldRowCount: Integer);
var
  i, nH, nTemp: Integer;
begin
  inherited;
  if TopRow > FixedRows then
  begin
    nH := Height;
    for i := 0 to FixedRows - 1 do
      Dec( nH, RowHeights[i] );
    nTemp := 0;
    for i := TopRow to TopRow + VisibleRowCount - 1 do
      nTemp := nTemp + RowHeights[i];
    nTemp := nH - nTemp;
    if nTemp > 0 then
    begin
      for i := TopRow downto FixedRows do
      begin
        Dec( nTemp, RowHeights[i] );
        if nTemp <= 10  then Break;
      end;
      TopRow := i;
    end;
  end;
  CheckScrollBar;
end;

procedure TxdStringGrid.TopLeftChanged;
begin
  if Assigned(FHorizontalScrollBar) then
  begin
    FHorizontalScrollBar.Invalidate;
  end;
  inherited;
  ChangedScrollBarPos;
end;

end.
