{
自绘StringGrid

    ============单元绘制参数==============
CellGradientDrawInfo
  GradientNormalText:    奇数行背景
  GradientMouseDownText: 偶数行背景
  GradientHoverText:     鼠标在某一行时背景
CellGradientDrawInfoEx
  GradientNormalText:    选中时背景
  GradientMouseDownText: 高亮时背景

CellBitmap图片信息：
   第一列使用图片的左边一半，中间的列使用图片中间一列元素来拉伸，最后一个列使用右边一半
   如果只有一列，则使用指定范围的图片来绘制
1：奇数行背景
2：偶数行背景
3：鼠标在某一行时背景
4：选中时背景
5：高亮时背景


   ==========固定行列绘制参数==============
FixedGradientDrawInfo
  只使用 GradientNormalText 进行绘制
FixedBitmap
  当 FixedBitmap.BitmapCount 为 1 时, 所有固定列都使用整个FixedBitmap来绘制
  当 FixedBitmap.BitmapCount > 1 时, 使用整个 FixedBitmap的左边一半来绘制第一个固定列，
    中间一个像素来绘制中间所有固定列，右边一半来绘制最后一个固定列
  
}
unit uJxdGpStringGrid;

interface
uses
  Windows, Messages, Grids, Classes, StdCtrls, uJxdGpScrollBar, Controls, ExtCtrls, uJxdGpStyle,
  SysUtils, Graphics, Forms, ShellAPI;

type
  {事件模式定义}
  TOnChangedRowState = procedure(Sender: TObject; const ACurCol, ANewRow, AOldRow: Integer) of object;
  TOnColSizing = procedure(Sender: TObject; const AColIndex: Integer; var AAllow: Boolean) of object;
  TOnDragFiles = procedure(Sender: TObject; const AFiles: TStringList) of object;
  TOnDrawText = procedure(Sender: TObject; ACol, ARow: Integer; ARect: TRect; const AState: TGridDrawState; var AOwnerDraw: Boolean) of object;

  TxdStringGrid = class(TStringGrid)
  public
    function  IsRowSelected(const ARow: Integer): Boolean;
    function  SelectedCount: Integer;
    function  GetSelectedIndex(const AIndex: Integer): Integer;
    procedure AddSelected(const ARowIndex: Integer);
    procedure UnSelectedAll;
    procedure DeleteAllSelected;
    procedure DeleteRow(ARow: Longint); override;
    procedure HighRow(const ARow: Integer);  //使用： GradientHoverText
    procedure InvalidateCell(ACol, ARow: Longint);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    property  BevelInner;
    procedure Paint; override;
    procedure Loaded; override;
    procedure DblClick; override;

    {绘制实现}
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure DrawFixed(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); virtual;
    procedure DrawRowCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); virtual;
    procedure DrawCellText(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); virtual;
    
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    procedure TopLeftChanged; override;
    procedure Resize; override;
    procedure ColWidthsChanged; override;
    function  SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure CalcSizingState(X, Y: Integer; var State: TGridState;
      var Index: Longint; var SizingPos, SizingOfs: Integer;
      var FixedInfo: TGridDrawInfo); override;

    procedure WMDragDropFiles(var msg:TMessage); message WM_DROPFILES;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DoChangedMouseOnIndex(ACurColIndex, ANewIndex: Integer);
  private
    FHighRow: Integer;
    FMouseOnRow: Integer;
    FDeleteSelected: Boolean;
    FSelectedRows: array of Integer;
    FSelectRect: TRect;
    FDownRow: Integer;
    FDownCol: Integer;
    procedure CheckSelectRow(const ARow: Integer);

    procedure CheckScrollBarHor;
    procedure ChangedScrollBarPosHor;
    procedure CheckScrollBarVer;
    procedure ChangedScrollBarPosVer;

    procedure DoDrawObjectChanged(Sender: TObject);
    procedure DoHorizontalScrollPosChanged(Sender: TObject; const IsChangedBySelf: Boolean);
    procedure DoVericalScrollPosChanged(Sender: TObject; const IsChangedBySelf: Boolean);
    procedure InitSetting;
  private
    FScrollBarHorizontal: TxdScrollBar;
    FScrollBarVerical: TxdScrollBar;
    FOnChangedMouseOnRow: TOnChangedRowState;
    FOnChangedHighRow: TOnChangedRowState;
    FOnColSizing: TOnColSizing;
    FDragAcceptFile: Boolean;
    FOnDragFiles: TOnDragFiles;
    FHighRowHeight: Integer;
    FOnDrawText: TOnDrawText;
    FFixedImage: TImageInfo;
    FCellImage: TImageInfo;
    FFixedImageDrawMethod: TDrawMethod;
    procedure SetScrollBarHorizontal(const Value: TxdScrollBar);
    procedure SetScrollBarVerical(const Value: TxdScrollBar);
    procedure SetDragAcceptFile(const Value: Boolean);
    procedure SetHighRowHeight(const Value: Integer);
    procedure SetCellImage(const Value: TImageInfo);
    procedure SetFixedBitmap(const Value: TImageInfo);
    procedure SetFixedImageDrawMethod(const Value: TDrawMethod);
  published
    property OnResize;
    
    property FixedRows;
    property FixedCols;

    property FixedImage: TImageInfo read FFixedImage write SetFixedBitmap;
    property FixedImageDrawMethod: TDrawMethod read FFixedImageDrawMethod write SetFixedImageDrawMethod;
    property CellImage: TImageInfo read FCellImage write SetCellImage;

    property HighRowHeight: Integer read FHighRowHeight write SetHighRowHeight;
    property CurHighRow: Integer read FHighRow;

    property ScrollBarHorizontal: TxdScrollBar read FScrollBarHorizontal write SetScrollBarHorizontal;
    property ScrollBarVerical: TxdScrollBar read FScrollBarVerical write SetScrollBarVerical;

    property MouseOnRowIndex: Integer read FMouseOnRow;

    property DragAcceptFile: Boolean read FDragAcceptFile write SetDragAcceptFile;
    property OnDragFiles: TOnDragFiles read FOnDragFiles write FOnDragFiles; 

    property OnChangedMouseOnRow: TOnChangedRowState read FOnChangedMouseOnRow write FOnChangedMouseOnRow;
    property OnChangedHighRow: TOnChangedRowState read FOnChangedHighRow write FOnChangedHighRow;
    property OnColSizing: TOnColSizing read FOnColSizing write FOnColSizing;

    property OnDrawText: TOnDrawText read FOnDrawText write FOnDrawText;   
  end;

implementation

{ TxdStringGrid }
function IsCtrlDown: Boolean;
var
  n: Smallint;
begin
  n := GetKeyState(VK_CONTROL) and 128;
  Result := n = 128;
end;

function IsShiftDown: Boolean;
var
  n: Smallint;
begin
  n := GetKeyState(VK_SHIFT) and 128;
  Result := n = 128;
end;


constructor TxdStringGrid.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  InitSetting;
  FScrollBarHorizontal := nil;
  FScrollBarVerical := nil;
  SetLength( FSelectedRows, 0 );
  FDeleteSelected := False;
  FSelectRect := Rect( -1, -1, -1, -1 );
  FDownRow := -1;
  FHighRow := -1;
  FMouseOnRow := -1;
  FHighRowHeight := DefaultRowHeight * 2; 
  FDragAcceptFile := False;
end;

procedure TxdStringGrid.AddSelected(const ARowIndex: Integer);
var
  n: Integer;
begin
  if not IsRowSelected(ARowIndex) then
  begin
    n := Length( FSelectedRows );
    SetLength( FSelectedRows, n + 1 );
    FSelectedRows[n] := ARowIndex;
    InvalidateRow( ARowIndex );
  end;
end;

procedure TxdStringGrid.CalcSizingState(X, Y: Integer; var State: TGridState; var Index, SizingPos, SizingOfs: Integer;
  var FixedInfo: TGridDrawInfo);
var
  bAllow: Boolean;
begin
  inherited;
  if (State = gsColSizing) and Assigned(OnColSizing) then
  begin
    bAllow := True;
    OnColSizing( Self, Index, bAllow );
    if not bAllow then
      State := gsNormal;
  end;
end;

procedure TxdStringGrid.ChangedScrollBarPosHor;
begin
  if Assigned(FScrollBarHorizontal) then
  begin
    with FScrollBarHorizontal do
    begin
//      OnPositionChanged := nil;
      Position := TopRow - FixedRows;
//      OnPositionChanged := DoHorizontalScrollPosChanged;
    end;
  end;
end;

procedure TxdStringGrid.ChangedScrollBarPosVer;
begin
  if Assigned(FScrollBarVerical) then
  begin
    with FScrollBarVerical do
    begin
//      OnPositionChanged := nil;
      Position := LeftCol;
//      OnPositionChanged := DoVericalScrollPosChanged;
    end;
  end;
end;

procedure TxdStringGrid.CheckSelectRow(const ARow: Integer);
var
  i, j, nCount: Integer;
  nMin, nMax: Integer;
  bExsits: Boolean;
  nReDrawRows: array of Integer;
begin
  if ARow < FixedRows then Exit;
  if (goRangeSelect in Options) and IsCtrlDown then
  begin
    //按下Ctrl
    bExsits := False;
    nCount := Length(FSelectedRows);
    for i := Low(FSelectedRows) to nCount - 1 do
    begin
      if ARow = FSelectedRows[i] then
      begin
        bExsits := True;
        SetLength( nReDrawRows, 1 );
        nReDrawRows[0] := FSelectedRows[i];
        if i <> (nCount - 1) then
          Move( FSelectedRows[i + 1], FSelectedRows[i], 4 * (nCount - i) );
        SetLength( FSelectedRows, nCount - 1 );
        Break;
      end;
    end;
    if not bExsits then
    begin
      SetLength( nReDrawRows, 1 );
      nReDrawRows[0] := ARow;
      SetLength( FSelectedRows, nCount + 1 );
      FSelectedRows[nCount] := ARow;
    end;
  end
  else if (goRangeSelect in Options) and IsShiftDown then
  begin
    //按下Shift键
    nMin := MaxInt;
    nMax := -1;
    for i := Low(FSelectedRows) to High(FSelectedRows) do
    begin
      if FSelectedRows[i] < nMin then nMin := FSelectedRows[i];
      if FSelectedRows[i] > nMax then nMax := FSelectedRows[i];
    end;

    if nMax = -1 then
    begin
      //没有选中行
      nMin := ARow;
      nMax := ARow;
    end
    else
    begin
      if nMin > ARow then
        nMin := ARow
      else if nMax < ARow then
        nMax := ARow
      else
      begin
        if FSelectedRows[High(FSelectedRows)] > ARow then
        begin
          nMin := ARow;
          nMax := FSelectedRows[High(FSelectedRows)];
        end
        else
        begin
          nMin := FSelectedRows[High(FSelectedRows)];
          nMax := ARow;
        end;
      end;
    end;

    SetLength( nReDrawRows, Length(FSelectedRows) );
    Move( FSelectedRows[0], nReDrawRows[0], 4 * Length(FSelectedRows) );
    SetLength( FSelectedRows, nMax + 1 - nMin );
    nCount := 0;
    for i := nMin to nMax do
    begin
      FSelectedRows[nCount] := i;
      Inc( nCount );
      bExsits := False;
      for j := Low(nReDrawRows) to High(nReDrawRows) do
      begin
        if nReDrawRows[j] = i then
        begin
          bExsits := True;
          Break;
        end;
      end;
      if not bExsits then
      begin
        SetLength( nReDrawRows, Length(nReDrawRows) + 1 );
        nReDrawRows[ High(nReDrawRows) ] := i;
      end;
    end;
  end
  else
  begin
    SetLength( nReDrawRows, Length(FSelectedRows) );
    Move( FSelectedRows[0], nReDrawRows[0], 4 * Length(FSelectedRows) );
    SetLength( FSelectedRows, 1 );
    FSelectedRows[0] := ARow;
    InvalidateRow( ARow );
  end;
  for i := Low(nReDrawRows) to High(nReDrawRows) do
    InvalidateRow( nReDrawRows[i] );
  SetLength( nReDrawRows, 0 );
end;

procedure TxdStringGrid.CheckScrollBarHor;
var
  nCount: Integer;
begin
  if Assigned(FScrollBarHorizontal) then
  begin
    nCount := RowCount - FixedRows;
    FScrollBarHorizontal.Visible := nCount > VisibleRowCount;
    FScrollBarHorizontal.Max := nCount - VisibleRowCount;
//    FScrollBarHorizontal.OnPositionChanged := DoHorizontalScrollPosChanged;
  end;
end;

procedure TxdStringGrid.CheckScrollBarVer;
var
  nCount: Integer;
  bVisble: Boolean;
begin
  if Assigned(FScrollBarVerical) then
  begin
    nCount := ColCount - FixedCols;
    bVisble := nCount > VisibleColCount;
    if bVisble then
      FScrollBarVerical.Max := nCount - VisibleColCount
    else
      FScrollBarVerical.Position := 0;
//    FScrollBarVerical.OnPositionChanged := DoVericalScrollPosChanged;
    FScrollBarVerical.Visible := bVisble;
  end;
end;

procedure TxdStringGrid.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FSelectRect := Rect( -1, -1, -1, -1 );
  FDownRow := -1;
  FMouseOnRow := -1;
  InvalidateGrid;
end;

procedure TxdStringGrid.ColWidthsChanged;
begin
  inherited;
  CheckScrollBarVer;
end;

procedure TxdStringGrid.DblClick;
begin
  inherited;
//  PostMessage( Handle, WM_LBUTTONUP, 0, 0 );
end;

procedure TxdStringGrid.DeleteAllSelected;
var
  i: Integer;
begin
  for i := High(FSelectedRows) downto Low(FSelectedRows) do
    DeleteRow( FSelectedRows[i] );
end;

procedure TxdStringGrid.DeleteRow(ARow: Integer);
var
  i, nLen: Integer;
begin
  if (ARow < 0) or (ARow >= RowCount) then Exit;
  i := Low(FSelectedRows);
  while i <= High(FSelectedRows) do
  begin
    if FSelectedRows[i] = ARow then
    begin
      nLen := 4 * ( Length( FSelectedRows ) - i );
      if nLen > 0 then
        Move( FSelectedRows[i + 1], FSelectedRows[i], nLen );
      SetLength( FSelectedRows, Length( FSelectedRows ) - 1 );
    end
    else
    begin
      if FSelectedRows[i] > ARow then
        FSelectedRows[i] := FSelectedRows[i] - 1;
      Inc( i );
    end;
  end;
  FDeleteSelected := True;
  inherited DeleteRow( ARow );
  FDeleteSelected := False;
end;

destructor TxdStringGrid.Destroy;
begin
  SetLength( FSelectedRows, 0 );
  inherited;
end;

procedure TxdStringGrid.DoChangedMouseOnIndex(ACurColIndex, ANewIndex: Integer);
var
  nOldIndex: Integer;
begin
  if FMouseOnRow <> ANewIndex then
  begin
    nOldIndex := FMouseOnRow;
    FMouseOnRow := ANewIndex;
    if nOldIndex <> -1 then
      InvalidateRow( nOldIndex );
    if FMouseOnRow <> -1 then
      InvalidateRow( FMouseOnRow );
    if Assigned(OnChangedMouseOnRow) then
      OnChangedMouseOnRow( Self, ACurColIndex, FMouseOnRow, nOldIndex );
  end;
end;

procedure TxdStringGrid.DoDrawObjectChanged(Sender: TObject);
begin
  InvalidateGrid;
end;

procedure TxdStringGrid.DoHorizontalScrollPosChanged(Sender: TObject; const IsChangedBySelf: Boolean);
begin
  if IsChangedBySelf and Assigned(FScrollBarHorizontal) then
  begin
    TopRow := FScrollBarHorizontal.Position + FixedRows;
    CheckScrollBarHor;
  end;
end;

procedure TxdStringGrid.DoVericalScrollPosChanged(Sender: TObject; const IsChangedBySelf: Boolean);
begin
  if IsChangedBySelf and Assigned(FScrollBarVerical) then
  begin
    LeftCol := FScrollBarVerical.Position;
    CheckScrollBarVer;
  end;
end;

procedure TxdStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
begin
  if ARow < 0 then Exit;
  if gdFixed in AState then
    DrawFixed( ACol, ARow, ARect, AState )
  else
    DrawRowCell( ACol, ARow, ARect, AState );
  if Assigned(OnDrawCell) then
    OnDrawCell( Self, ACol, ARow, ARect, AState );
end;

procedure TxdStringGrid.DrawCellText(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  strText: string;
  bOwnerDraw: Boolean;
begin
  bOwnerDraw := False;
  if Assigned(OnDrawText) then
    OnDrawText( Self, ACol, ARow, ARect, AState, bOwnerDraw );
  if not bOwnerDraw then
  begin
    strText := Cells[ACol, ARow];
    if strText <> '' then
    begin
      ARect.Left := ARect.Left + 2;
      Canvas.Brush.Style := bsClear;
      DrawText( Canvas.Handle, PChar(strText), Length(strText), ARect, DT_VCENTER or DT_SINGLELINE );
    end;
  end;
end;

procedure TxdStringGrid.DrawFixed(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  bDrawBitmap, bDriection: Boolean;
  SrcR: TRect;
begin
//  bDrawBitmap := (FFixedBitmap.Bitmap.Width > 0) and (FFixedBitmap.Bitmap.Height > 0);
  if bDrawBitmap then
  begin
    {
    当 FixedBitmap.BitmapCount 为 1 时, 所有固定列都使用整个FixedBitmap来绘制
    当 FixedBitmap.BitmapCount > 1 时, 使用整个 FixedBitmap的左边一半来绘制第一个固定列，
       中间一个像素来绘制中间所有固定列，右边一半来绘制最后一个固定列
    }
//    SrcR := Rect( 0, 0, FFixedBitmap.Bitmap.Width, FFixedBitmap.Bitmap.Height );
//    if (FFixedBitmap.BitmapCount > 1) and (ColCount > 1) then
    begin
      //图片自动拉伸参数设置
      if ARow < FixedRows then
      begin
        //全行都是固定
        if ACol = 0 then
          SrcR.Right := SrcR.Left + (SrcR.Right - SrcR.Left) div 2
        else if ACol = ColCount - 1 then
          SrcR.Left := SrcR.Left + (SrcR.Right - SrcR.Left) div 2
        else
        begin
          SrcR.Left := SrcR.Left + (SrcR.Right - SrcR.Left) div 2;
          SrcR.Right := SrcR.Left + 1;
        end;
      end
      else
      begin
        //只有部分是固定的
        if FixedCols > 1 then
        begin
          if ACol = 0 then
            SrcR.Right := SrcR.Left + (SrcR.Right - SrcR.Left) div 2
          else if ACol = FixedCols - 1 then
            SrcR.Left := SrcR.Left + (SrcR.Right - SrcR.Left) div 2
          else
          begin
            SrcR.Left := SrcR.Left + (SrcR.Right - SrcR.Left) div 2;
            SrcR.Right := SrcR.Left + 1;
          end;
        end;
      end;
    end;
//    DrawRectangle( FFixedBitmap.Bitmap, Canvas, SrcR, ARect, FFixedBitmap.BitmapDrawStyle, IsTransColor, TransColor )
  end
  else
  begin
//    bDriection := FFixedGradientDrawInfo.GradientWay = gwLeftToRigth;
//    DrawGradientInfo( Canvas, FFixedGradientDrawInfo.ParseGradientNormal, bDriection, ARect.Left, ARect.Top,
//      WidthOfRect(ARect), HeightOfRect(ARect) );
  end;

  DrawCellText( ACol, ARow, ARect, AState );
end;

procedure TxdStringGrid.DrawRowCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  bDrawBitmap, bDriection: Boolean;
  SrcR: TRect;

  n, nH: Integer;
begin
//  bDrawBitmap := (FCellBitmap.Bitmap.Width > 0) and (FCellBitmap.Bitmap.Height > 0);
  if bDrawBitmap then
  begin
    //使用图片来绘制界面
    {
    CellBitmap图片信息：
      1：奇数行背景
      2：偶数行背景
      3：鼠标在某一行时背景
      4：选中时背景
      5：高亮时背景
  end;}
//    nH := FCellBitmap.Bitmap.Height div FCellBitmap.BitmapCount;

    if FHighRow = ARow then
      n := 5
    else if IsRowSelected(ARow) then
      n := 4
    else if FMouseOnRow = ARow then
      n := 3
    else if (ARow - FixedRows) mod 2 <> 0 then
      n := 2
    else
      n := 1;

//    if n > FCellBitmap.BitmapCount then
//      n := FCellBitmap.BitmapCount;

//    if n > 0 then
//      SrcR := Rect( 0, nH * (n - 1), FCellBitmap.Bitmap.Width, nH * n )
//    else
//      SrcR := Rect( 0, 0, FCellBitmap.Bitmap.Width, nH );

    if (ColCount > 1) and (goRowSelect in Options) then
    begin
      //图片自动拉伸参数设置
      if ACol - FixedCols = 0 then
      begin
        //第一列
        SrcR.Right := SrcR.Left + (SrcR.Right - SrcR.Left) div 2;
      end
      else if (ACol + 1) = ColCount then
      begin
        //最后一列
        SrcR.Left := SrcR.Left + (SrcR.Right - SrcR.Left) div 2;
      end
      else
      begin
        //中间
        SrcR.Left := SrcR.Left + (SrcR.Right - SrcR.Left) div 2;
        SrcR.Right := SrcR.Left + 1;
      end;
    end;
//    DrawRectangle( FCellBitmap.Bitmap, Canvas, SrcR, ARect, FCellBitmap.BitmapDrawStyle, IsTransColor, TransColor )
  end
  else
  begin
    //使用自定义颜色值来绘制界面
    {
    CellGradientDrawInfo
      GradientNormalText:    奇数行背景
      GradientMouseDownText: 偶数行背景
      GradientHoverText:     鼠标在某一行时背景
    CellGradientDrawInfoEx
      GradientNormalText:    选中时背景
      GradientMouseDownText: 高亮时背景
  end;}
//    if FHighRow = ARow then
//      Gradient := FCellGradientDrawInfoEx.ParseGradientMouseDown
//    else if IsRowSelected(ARow) then
//      Gradient := FCellGradientDrawInfoEx.ParseGradientNormal
//    else if FMouseOnRow = ARow then
//      Gradient := FCellGradientDrawInfo.ParseGradientHover
//    else if (ARow - FixedRows) mod 2 <> 0 then
//      Gradient := FCellGradientDrawInfo.ParseGradientNormal
//    else
//      Gradient := FCellGradientDrawInfo.ParseGradientMouseDown;
//
//    bDriection := FCellGradientDrawInfo.GradientWay = gwLeftToRigth;
//    DrawGradientInfo( Canvas, Gradient, bDriection, ARect.Left, ARect.Top, WidthOfRect(ARect), HeightOfRect(ARect) );
  end;

  DrawCellText( ACol, ARow, ARect, AState );
end;

function TxdStringGrid.GetSelectedIndex(const AIndex: Integer): Integer;
begin
  if (AIndex >= Low(FSelectedRows)) and (AIndex <= High(FSelectedRows)) then
    Result := FSelectedRows[AIndex]
  else
    Result := -1;
end;

procedure TxdStringGrid.HighRow(const ARow: Integer);
var
  nOldRow: Integer;
begin
  if FHighRow <> ARow then
  begin
    nOldRow := FHighRow;
    FHighRow := ARow;
    if (nOldRow >= FixedRows) and (nOldRow < RowCount) then
    begin
      RowHeights[nOldRow] := DefaultRowHeight;
//      InvalidateRow( nOldRow );
    end;
    if FHighRow > -1 then
    begin
      RowHeights[ARow] := FHighRowHeight;
//      InvalidateRow( FHighRow );
    end;
    if Assigned(OnChangedHighRow) then
      OnChangedHighRow( Self, 0, FHighRow, nOldRow );
  end;
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
  DoubleBuffered := True;
end;

procedure TxdStringGrid.InvalidateCell(ACol, ARow: Integer);
begin
  inherited InvalidateCell(ACol, ARow);
end;

function TxdStringGrid.IsRowSelected(const ARow: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if goRowSelect in Options then
  begin
    for i := 0 to Length(FSelectedRows) - 1 do
    begin
      if ARow = FSelectedRows[i] then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TxdStringGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  i, n: Integer;
begin
  if not (goRowSelect in Options) then
  begin
    inherited;
    Exit;
  end;

  if goRangeSelect in Options then
  begin
    //可多选
    if (ssCtrl in Shift) and ( (Key = Ord('a')) or (Key = Ord('A')) ) then
    begin
      //全选
      SetLength( FSelectedRows, RowCount - FixedRows );
      n := 0;
      for i := FixedRows to RowCount - 1 do
      begin
        FSelectedRows[n] := i;
        Inc( n );
      end;
      InvalidateGrid;
      Exit;
    end;
  end;

  case Key of
    VK_ESCAPE: UnSelectedAll;
    VK_HOME: TopRow := FixedRows;
    VK_END: TopRow := RowCount - VisibleRowCount;
    VK_UP, VK_DOWN:
    begin
      if Assigned(FScrollBarHorizontal) and FScrollBarHorizontal.Visible then
      begin
        if Key = VK_UP then
          FScrollBarHorizontal.Position := FScrollBarHorizontal.Position - 1
        else
          FScrollBarHorizontal.Position := FScrollBarHorizontal.Position + 1;
        DoHorizontalScrollPosChanged( FScrollBarHorizontal, True );
      end;
    end;
    VK_LEFT, VK_RIGHT:
    begin
      if Assigned(FScrollBarVerical) and FScrollBarVerical.Visible then
      begin
        if Key = VK_LEFT then
          FScrollBarVerical.Position := FScrollBarVerical.Position - 1
        else
          FScrollBarVerical.Position := FScrollBarVerical.Position + 1;
        DoVericalScrollPosChanged( FScrollBarVerical, True );
      end;
    end;
  end;
end;

procedure TxdStringGrid.Loaded;
begin
  inherited;
  InitSetting;
  CheckScrollBarHor;
  ChangedScrollBarPosHor;
  CheckScrollBarVer;
  ChangedScrollBarPosVer;
  DoVericalScrollPosChanged( nil, True );
end;

procedure TxdStringGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  nRow, nCol: Integer;
begin

  if Button = mbLeft then
  begin
    MouseToCell( X, Y, nCol, nRow );
    if (nRow >= FixedRows) and (nCol >= FixedCols) then
    begin
      CheckSelectRow( nRow );
      FDownRow := nRow;
      FDownCol := nCol;
      FSelectRect := Rect( X, Y, -1, -1 );
    end;
  end
  else
  begin
    if SelectedCount = 0 then
    begin
      MouseToCell( X, Y, nCol, nRow );
      CheckSelectRow( nRow );
    end;
  end;
  inherited ;
end;

procedure TxdStringGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  nRow1, nRow2, nCol: Integer;
  i: Integer;
begin  
  inherited;
  MouseToCell( x, y, nCol, nRow1 );
  if nRow1 <> FMouseOnRow then
    DoChangedMouseOnIndex( nCol, nRow1 );
  if (goRangeSelect in Options) and (FSelectRect.Left <> -1) and (FSelectRect.Top <> -1) then
  begin 
    FSelectRect.Right := X;
    FSelectRect.Bottom := Y; 
    
    nRow1 := FDownRow;
    MouseToCell( 1, FSelectRect.Bottom, nCol, nRow2 );    

    if nRow2 = -1 then
      nRow2 := RowCount;
    if nRow1 > nRow2 then
    begin
      nCol := nRow1;
      nRow1 := nRow2;
      nRow2 := nCol;
    end;

    if nRow2 > nRow1 then
    begin
      SetLength( FSelectedRows, nRow2 - nRow1 + 1 );
      for i := Low(FSelectedRows) to High(FSelectedRows) do
      begin
        FSelectedRows[i] := nRow1;
        Inc( nRow1 );
      end;
    end;

    InvalidateGrid;
  end;
end;

procedure TxdStringGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectRect := Rect( -1, -1, -1, -1 );
  FDownRow := -1;
  InvalidateGrid;
  inherited;
end;

procedure TxdStringGrid.Paint;
var
  ps: TPenStyle;
  bs: TBrushStyle;
  nColor: TColor;
begin
  inherited;
//  DrawLinesInfo( Canvas, FLineDrawInfo, Width, Height );

  if (goRangeSelect in Options) and (FSelectRect.Left <> -1) and (FSelectRect.Top <> -1) and
     (FSelectRect.Right <> -1) and (FSelectRect.Bottom <> -1) then
  begin
    ps := Canvas.Pen.Style;
    bs := Canvas.Brush.Style;
    nColor := Canvas.Pen.Color;

    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := 0;    
    Canvas.Rectangle( FSelectRect.Left, FSelectRect.Top, FSelectRect.Right, FSelectRect.Bottom );
  
    Canvas.Pen.Style := ps;
    Canvas.Pen.Color := nColor;
    Canvas.Brush.Style := bs;
  end;
end;

procedure TxdStringGrid.Resize;
begin
  CheckScrollBarHor;
  CheckScrollBarVer;
  inherited Resize;
end;

function TxdStringGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  Result := inherited SelectCell(ACol, ARow);
  if FDeleteSelected then
    Result := False;
end;

function TxdStringGrid.SelectedCount: Integer;
begin
  Result := Length( FSelectedRows );
end;

procedure TxdStringGrid.SetCellImage(const Value: TImageInfo);
begin
  FCellImage := Value;
end;

procedure TxdStringGrid.SetDragAcceptFile(const Value: Boolean);
begin
  if FDragAcceptFile <> Value then
  begin
    FDragAcceptFile := Value;
    if FDragAcceptFile then
      DragAcceptFiles( Handle, True )
    else
      DragAcceptFiles( Handle, False );
  end;
end;

procedure TxdStringGrid.SetFixedBitmap(const Value: TImageInfo);
begin
  FFixedImage := Value;
end;

procedure TxdStringGrid.SetFixedImageDrawMethod(const Value: TDrawMethod);
begin
  FFixedImageDrawMethod.Assign( Value );
end;

procedure TxdStringGrid.SetHighRowHeight(const Value: Integer);
begin
  FHighRowHeight := Value;
end;

procedure TxdStringGrid.SetScrollBarHorizontal(const Value: TxdScrollBar);
begin
  FScrollBarHorizontal := Value;
  CheckScrollBarHor;
  ChangedScrollBarPosHor;
end;

procedure TxdStringGrid.SetScrollBarVerical(const Value: TxdScrollBar);
begin
  FScrollBarVerical := Value;
  CheckScrollBarVer;
  ChangedScrollBarPosVer;
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
  CheckScrollBarHor;

  nTemp := RowCount;
  if nTemp < OldRowCount then
  begin
    i := 0;
    while i <= High(FSelectedRows) do
    begin
      if FSelectedRows[i] > nTemp then
      begin
        nH := Length(FSelectedRows);
        if nH - i > 0 then
          Move( FSelectedRows[i + 1], FSelectedRows[i], 4 * (nH - i) );
        SetLength( FSelectedRows, nH - 1 );
      end
      else
        Inc( i );
    end;
  end;
end;

procedure TxdStringGrid.TopLeftChanged;
var
  R: TRect;
begin
  inherited;
  ChangedScrollBarPosHor;
  if (FDownRow <> -1) and (FDownCol <> -1) then
  begin
    R := CellRect( FDownCol, FDownRow );
    FSelectRect.Top := R.Top;
    if R.Top = 0 then
    begin
      if FDownRow < TopRow then
        FSelectRect.Top := RowHeights[FixedRows]
      else
        FSelectRect.Top := Height;
    end;
  end;
end;

procedure TxdStringGrid.UnSelectedAll;
var
  nReDrawRows: array of Integer;
  i, nLen: Integer;
begin
  nLen := Length(FSelectedRows);
  if nLen > 0 then
  begin
    SetLength( nReDrawRows, nLen );
    Move( FSelectedRows[0], nReDrawRows[0], 4 * nLen );
    SetLength( FSelectedRows, 0 );
    for i := 0 to nLen - 1 do
      InvalidateRow( nReDrawRows[i] );
    SetLength( nReDrawRows, 0 );
  end;
end;

procedure TxdStringGrid.WMDragDropFiles(var msg: TMessage);
var
  FileName: array[0..MAX_PATH] of Char;
  i, Sum: Integer;
  Files: TStringList;
begin
  inherited;
  if not Assigned(OnDragFiles) then Exit;
  Files := TStringList.Create;
  try
    //   获得拖拉的文件数目，该功能由第二个参数决定
    Sum:=DragQueryFile( msg.WParam, $FFFFFFFF, nil, 0 );
    for i := 0 to Sum - 1 do
    begin
      //读取文件名
      FillChar( FileName, MAX_PATH, 0 );
      DragQueryFile( msg.WParam, i, FileName, MAX_PATH );
      Files.Add( FileName );
    end;
    OnDragFiles( Self, Files );
  finally
    DragFinish(msg.WParam);
    Files.Free;
  end;
end;

procedure TxdStringGrid.WMMouseWheel(var Message: TWMMouseWheel);
var
  nTopRow, nSize: Integer;
begin
  if IsCtrlDown then
    nSize := 5
  else
    nSize := 1;

  if Message.WheelDelta > 0 then
    nTopRow := TopRow - nSize
  else
    nTopRow := TopRow + nSize;
  if (nTopRow > 0) and (nTopRow + VisibleRowCount <= RowCount) then
    TopRow := nTopRow
  else if nSize = 5 then
  begin
    nSize := 1;
    if Message.WheelDelta > 0 then
      nTopRow := TopRow - nSize
    else
      nTopRow := TopRow + nSize;
    if (nTopRow > 0) and (nTopRow + VisibleRowCount <= RowCount) then
      TopRow := nTopRow
  end;
end;

end.
