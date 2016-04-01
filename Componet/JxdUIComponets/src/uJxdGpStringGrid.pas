{
自绘StringGrid

    ============单元绘制参数==============
CellImage图片信息
CellImageDrawMethod.DrawStyle 只支持：dsPaste, dsStretchByVH, dsStretchAll

CellImage.ImageCount由上到下图片表示意义 固定为 5 
1：奇数行背景
2：偶数行背景
3：鼠标在某一行时背景
4：选中时背景
5：高亮时背景


   ==========固定行列绘制参数==============
FFixedImage
FFixedImageDrawMethod

FFixedImageDrawMethod.DrawStyle 只支持：dsPaste, dsStretchByVH, dsStretchAll
FFixedImage.ImageCount 决定绘制的状态：最大为3；

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
  SysUtils, Graphics, Forms, ShellAPI, GDIPAPI, GDIPOBJ, Math;

type
  TCellInfo = record
    FRowIndex: Integer;
    FColIndex: Integer;
  end;
  {事件模式定义}
  TOnGridOpt = procedure(Sender: TObject; const ACellInfo: TCellInfo) of object;
  TOnColSizing = procedure(Sender: TObject; const AColIndex: Integer; var AAllow: Boolean) of object;
  TOnDragFiles = procedure(Sender: TObject; const AFiles: TStringList) of object;
  TOnXdDrawGrid = procedure(Sender: TObject; const AGh: TGPGraphics;
                          const ACol, ARow: Integer; var AGpRect: TGPRect; 
                          const AState: TGridDrawState; var ADefaultDraw: Boolean) of object;

  TxdStringGrid = class(TStringGrid)
  public   
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  IsRowSelected(const ARow: Integer): Boolean;
    function  SelectedCount: Integer;
    function  GetSelectedIndex(const AIndex: Integer): Integer;
    procedure AddSelected(const ARowIndex: Integer);
    procedure UnSelectedAll;
    procedure DeleteAllSelected;
    procedure DeleteRow(ARow: Longint); override;
    procedure HighRow(const ARow: Integer);  //使用： GradientHoverText
    procedure InvalidateCell(ACol, ARow: Longint);
    procedure InvalidateRow(const ARow: Integer);
  protected
    {父类处理}
    property  BevelInner;
    procedure Paint; override;
    procedure Loaded; override;
    procedure DblClick; override;
  protected
    {绘制实现}
    procedure ParentPaint;
    procedure DrawEmptyBK(ADestR, AUpdateR: TRect); 
    procedure DrawEmptyRowEx(ADestR: TRect);
    
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;    
    procedure DrawFixedBK(const AGh: TGPGraphics; ACol, ARow: Longint; ADestR: TGPRect; AState: TGridDrawState); virtual;
    procedure DrawRowCellBK(const AGh: TGPGraphics; ACol, ARow: Longint; ADestR: TGPRect; AState: TGridDrawState); virtual;    
    procedure DrawCellText(const AGh: TGPGraphics; const AFontInfo: TFontInfo; ACol, ARow: Longint; ADestR: TGPRect; AState: TGridDrawState); virtual;

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
    
    procedure DoMousePosChanged(const ANewRowIndex, ANewCellIndex: Integer);  //鼠标位置变动
    procedure DoDownPosChanged(const ANewRowIndex, AnewColIndex: Integer); //按下位置变动

    {相关对象事件处理}
    procedure DoObjectChanged(Sender: TObject);
    procedure DoHorizontalScrollPosChanged(Sender: TObject; const AChangedStyle: TxdChangedStyle);
    procedure DoVericalScrollPosChanged(Sender: TObject; const AChangedStyle: TxdChangedStyle);
  private
    FPainting: Boolean;
    FHighRow: Integer;
    FDeleteSelected: Boolean;
    FSelectedRows: array of Integer;
    FSelectRect: TRect;
    FCurDownCell: TCellInfo; //当前按下所在位置
    FCurMouseCell: TCellInfo; //当前鼠标所在位置
    procedure CheckSelectRow(const ARow: Integer);

    procedure CheckScrollBarHor;
    procedure ChangedScrollBarPosHor;
    procedure CheckScrollBarVer;
    procedure ChangedScrollBarPosVer;
    
    procedure InitSetting;

    function IsActiveControl: Boolean;
  private
    FScrollBarHorizontal: TxdScrollBar;
    FScrollBarVerical: TxdScrollBar;
    FOnColSizing: TOnColSizing;
    FDragAcceptFile: Boolean;
    FOnDragFiles: TOnDragFiles;
    FHighRowHeight: Integer;
    FOnDrawText: TOnXdDrawGrid;
    FFixedImage: TImageInfo;
    FCellImage: TImageInfo;
    FFixedImageDrawMethod: TDrawMethod;
    FFixedFont: TFontInfo;
    FCellFont: TFontInfo;
    FCellImageDrawMethod: TDrawMethod;
    FOnXdDblClickCell: TOnGridOpt;
    FOnXdDrawCellBefor: TOnXdDrawGrid;
    FOnXdDrawCellAfter: TOnXdDrawGrid;
    procedure SetScrollBarHorizontal(const Value: TxdScrollBar);
    procedure SetScrollBarVerical(const Value: TxdScrollBar);
    procedure SetDragAcceptFile(const Value: Boolean);
    procedure SetHighRowHeight(const Value: Integer);
    procedure SetCellImage(const Value: TImageInfo);
    procedure SetFixedBitmap(const Value: TImageInfo);
    procedure SetFixedImageDrawMethod(const Value: TDrawMethod);
    procedure SetFixedFont(const Value: TFontInfo);
    procedure SetCellFont(const Value: TFontInfo);
    procedure SetCellImageDrawMethod(const Value: TDrawMethod);
  published
    {父类}
    property OnResize;
    property FixedRows;
    property FixedCols;
    
    //外部提供滚动条对象
    property ScrollBarHorizontal: TxdScrollBar read FScrollBarHorizontal write SetScrollBarHorizontal;
    property ScrollBarVerical: TxdScrollBar read FScrollBarVerical write SetScrollBarVerical;
    //固定行绘制属性
    property FixedImage: TImageInfo read FFixedImage write SetFixedBitmap;
    property FixedImageDrawMethod: TDrawMethod read FFixedImageDrawMethod write SetFixedImageDrawMethod;
    property FixedFont: TFontInfo read FFixedFont write SetFixedFont;
    //信息显示行绘制属性
    property CellImage: TImageInfo read FCellImage write SetCellImage;
    property CellImageDrawMethod: TDrawMethod read FCellImageDrawMethod write SetCellImageDrawMethod;
    property CellFont: TFontInfo read FCellFont write SetCellFont;
    //高亮行信息    
    property HighRowHeight: Integer read FHighRowHeight write SetHighRowHeight;
    property CurHighRow: Integer read FHighRow;    
    property CurDownCell: TCellInfo read FCurDownCell; //当前按下所在位置
    property CurMouseCell: TCellInfo read FCurMouseCell; //当前鼠标所在位置
    //外部拖拉操作
    property DragAcceptFile: Boolean read FDragAcceptFile write SetDragAcceptFile;
    property OnDragFiles: TOnDragFiles read FOnDragFiles write FOnDragFiles; 

    //事件
    property OnColSizing: TOnColSizing read FOnColSizing write FOnColSizing; //列大小改变
    property OnXdDblClickCell: TOnGridOpt read FOnXdDblClickCell write FOnXdDblClickCell; //双击
    property OnXdDrawCellBefor: TOnXdDrawGrid read FOnXdDrawCellBefor write FOnXdDrawCellBefor; //绘制单元
    property OnXdDrawCellAfter: TOnXdDrawGrid read FOnXdDrawCellAfter write FOnXdDrawCellAfter; //绘制单元
    property OnXdDrawText: TOnXdDrawGrid read FOnDrawText write FOnDrawText; //字体绘制事件
  end;

implementation

uses
  uJxdGpSub;

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
  FPainting := False;
  FScrollBarHorizontal := nil;
  FScrollBarVerical := nil;
  SetLength( FSelectedRows, 0 );
  FDeleteSelected := False;
  FSelectRect := Rect( -1, -1, -1, -1 );
  FCurDownCell.FRowIndex := -1;
  FCurDownCell.FColIndex := -1;
  FCurMouseCell.FRowIndex := -1;
  FCurMouseCell.FColIndex := -1;
  FHighRowHeight := DefaultRowHeight * 2; 
  FDragAcceptFile := False;
  Options := Options - [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
  Options := Options + [goRowSelect];
  
  FFixedImage := TImageInfo.Create;
  FFixedImageDrawMethod := TDrawMethod.Create;
  FFixedFont := TFontInfo.Create;
  FCellImage := TImageInfo.Create;
  FCellFont := TFontInfo.Create;
  FCellImageDrawMethod := TDrawMethod.Create;

  FFixedImage.OnChange := DoObjectChanged;
  FFixedImageDrawMethod.OnChange := DoObjectChanged;  
  FFixedFont.OnChange := DoObjectChanged;
  FCellImage.OnChange := DoObjectChanged;
  FCellFont.OnChange := DoObjectChanged;
  FCellImageDrawMethod.OnChange := DoObjectChanged;
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
      OnPosChanged := nil;
      Position := TopRow - FixedRows;
      OnPosChanged := DoHorizontalScrollPosChanged;
    end;
  end;
end;

procedure TxdStringGrid.ChangedScrollBarPosVer;
begin
  if Assigned(FScrollBarVerical) then
  begin
    with FScrollBarVerical do
    begin
      OnPosChanged := nil;
      Position := LeftCol;
      OnPosChanged := DoVericalScrollPosChanged;
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
    FScrollBarHorizontal.OnPosChanged := DoHorizontalScrollPosChanged;
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
    FScrollBarVerical.OnPosChanged := DoVericalScrollPosChanged;
    FScrollBarVerical.Visible := bVisble;
  end;
end;

procedure TxdStringGrid.CMMouseLeave(var Message: TMessage);
var
  nRow: Integer;
begin
  inherited;
  nRow := FCurMouseCell.FRowIndex;
  FSelectRect := Rect( -1, -1, -1, -1 );
  FCurDownCell.FRowIndex := -1;
  FCurDownCell.FColIndex := -1;
  FCurMouseCell.FRowIndex := -1;
  FCurMouseCell.FColIndex := -1;
  if nRow <> -1 then  
    InvalidateRow(nRow);
end;

procedure TxdStringGrid.ColWidthsChanged;
begin
  inherited;
  CheckScrollBarVer;
end;

procedure TxdStringGrid.DblClick;
begin
  inherited DblClick;
  if Assigned(OnXdDblClickCell) then
    OnXdDblClickCell( Self, FCurMouseCell );
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
  FreeAndNil( FFixedImage );
  FreeAndNil( FFixedImageDrawMethod );
  FreeAndNil( FFixedFont );
  FreeAndNil( FCellImage );
  FreeAndNil( FCellFont );
  FreeAndNil( FCellImageDrawMethod );
  inherited;
end;

procedure TxdStringGrid.DoMousePosChanged(const ANewRowIndex, ANewCellIndex: Integer);
var
  old: TCellInfo;
begin
  if goRowSelect in Options then
  begin
    //整行选择
    if FCurMouseCell.FRowIndex <> ANewRowIndex then
    begin
      old := FCurMouseCell;
      FCurMouseCell.FRowIndex := ANewRowIndex;
      FCurMouseCell.FColIndex := ANewCellIndex;

      if old.FRowIndex <> FCurMouseCell.FRowIndex then
      begin
        if old.FRowIndex <> -1 then
          InvalidateRow( old.FRowIndex );
        if FCurMouseCell.FRowIndex <> -1 then
          InvalidateRow( FCurMouseCell.FRowIndex );
      end;
    end;
  end
  else
  begin
    if (FCurMouseCell.FRowIndex <> ANewRowIndex) or (FCurMouseCell.FColIndex <> ANewCellIndex) then
    begin
      old := FCurMouseCell;
      FCurMouseCell.FRowIndex := ANewRowIndex;
      FCurMouseCell.FColIndex := ANewCellIndex;

      if (old.FRowIndex <> FCurMouseCell.FRowIndex) or (old.FColIndex <> FCurMouseCell.FColIndex) then
      begin
        if (old.FRowIndex <> -1) and (old.FColIndex <> -1) then
          InvalidateCell( old.FColIndex, old.FRowIndex );
        if (FCurMouseCell.FRowIndex <> -1) and (FCurMouseCell.FColIndex <> -1) then
          InvalidateCell( FCurMouseCell.FColIndex, FCurMouseCell.FRowIndex );
      end;
    end;
  end;
//    if (old.FRowIndex <> -1) and then
//      InvalidateRow( nOldIndex );
//    if FCurMouseCell.FRowIndex <> -1 then
//      InvalidateRow( FCurMouseCell.FRowIndex );
//    if Assigned(OnChangedMouseOnRow) then
//      OnChangedMouseOnRow( Self, ACurColIndex, FCurMouseCell.FRowIndex, nOldIndex );
//    OutputDebugString( PChar('当前鼠标位于：' + IntToStr(FCurMouseCell.FRowIndex) + ' ' + 
//      IntToStr(FCurMouseCell.FColIndex)) );
end;

procedure TxdStringGrid.DoDownPosChanged(const ANewRowIndex, AnewColIndex: Integer);
var
  old: TCellInfo;
begin
  if goRowSelect in Options then
  begin
    //整行选择
    if FCurDownCell.FRowIndex <> ANewRowIndex then
    begin
      old := FCurDownCell;
      FCurDownCell.FRowIndex := ANewRowIndex;
      FCurDownCell.FColIndex := AnewColIndex;

      if old.FRowIndex <> FCurDownCell.FRowIndex then
      begin
        if old.FRowIndex <> -1 then
          InvalidateRow( old.FRowIndex );
        if FCurDownCell.FRowIndex <> -1 then
          InvalidateRow( FCurDownCell.FRowIndex );
      end;
    end;
  end
  else
  begin
    if (FCurDownCell.FRowIndex <> ANewRowIndex) or (FCurDownCell.FColIndex <> AnewColIndex) then
    begin
      old := FCurDownCell;
      FCurDownCell.FRowIndex := ANewRowIndex;
      FCurDownCell.FColIndex := AnewColIndex;

      if (old.FRowIndex <> FCurDownCell.FRowIndex) or (old.FColIndex <> FCurDownCell.FColIndex) then
      begin
        if (old.FRowIndex <> -1) and (old.FColIndex <> -1) then
          InvalidateCell( old.FColIndex, old.FRowIndex );
        if (FCurDownCell.FRowIndex <> -1) and (FCurDownCell.FColIndex <> -1) then
          InvalidateCell( FCurDownCell.FColIndex, FCurDownCell.FRowIndex );
      end;
    end;
  end;
end;

procedure TxdStringGrid.DoHorizontalScrollPosChanged(Sender: TObject; const AChangedStyle: TxdChangedStyle);
begin
  if (AChangedStyle <> csNull) and Assigned(FScrollBarHorizontal) then
  begin
    TopRow := FScrollBarHorizontal.Position + FixedRows;
    CheckScrollBarHor;
  end;
end;

procedure TxdStringGrid.DoObjectChanged(Sender: TObject);
begin
  if not FPainting then  
    InvalidateGrid;
end;

procedure TxdStringGrid.DoVericalScrollPosChanged(Sender: TObject; const AChangedStyle: TxdChangedStyle);
begin
  if (AChangedStyle <> csNull) and Assigned(FScrollBarVerical) then
  begin
    LeftCol := FScrollBarVerical.Position;
    CheckScrollBarVer;
  end;
end;

procedure TxdStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  R: TGPRect;
  G: TGPGraphics;
  bDefaultDraw: Boolean;
begin
  if ARow < 0 then Exit;
//  OutputDebugString( PChar('Draw Cell: ' + InttoStr(ACol) +  '-' + IntToStr(ARow)) );
  bDefaultDraw := True;
  R := MakeRect( ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top );
  G := TGPGraphics.Create( Canvas.Handle );
  try
    if Assigned(OnXdDrawCellBefor) then
      OnXdDrawCellBefor( Self, G, ACol, ARow, R, AState, bDefaultDraw ); 
      
    if bDefaultDraw then
    begin
      if gdFixed in AState then
        DrawFixedBK( G, ACol, ARow, R, AState )
      else
        DrawRowCellBK( G, ACol, ARow, R, AState );
    end;

    if Assigned(OnXdDrawCellAfter) then
      OnXdDrawCellAfter( Self, G, ACol, ARow, R, AState, bDefaultDraw );   

    if gdFixed in AState then
      DrawCellText( G, FFixedFont, ACol, ARow, R, AState )
    else
      DrawCellText( G, FCellFont, ACol, ARow, R, AState );
  finally
    G.Free;
  end;  
  if Assigned(OnDrawCell) then
    OnDrawCell( Self, ACol, ARow, ARect, AState );
end;

procedure TxdStringGrid.DrawCellText(const AGh: TGPGraphics; const AFontInfo: TFontInfo; ACol, ARow: Longint; ADestR: TGPRect; AState: TGridDrawState);
var
  strText: string;
  bDefaultDraw: Boolean;
  R: TGPRectF;
begin
  bDefaultDraw := True;
  if Assigned(OnXdDrawText) then
    OnXdDrawText( Self, AGh, ACol, ARow, ADestR, AState, bDefaultDraw );
  if bDefaultDraw then
  begin
    strText := Cells[ACol, ARow];
    if strText <> '' then
    begin
      R.X := ADestR.X; 
      R.Y := ADestR.Y;
      R.Width := ADestR.Width;
      R.Height := ADestR.Height;
      AGh.DrawString( strText, -1, AFontInfo.Font, R, AFontInfo.Format, AFontInfo.FontBrush )
    end;
  end;
end;

procedure TxdStringGrid.DrawEmptyBK(ADestR, AUpdateR: TRect);
var
  nRow, nCount: Integer;
  DestR: TGPRect;
  CalcUpDateR, DR, R: TRect;
  i, nTemp, nX, nY, nSrcX, nSrcY, nW, nH: Integer;
  SrcBmpR: TGPRect;
  G: TGPGraphics;
  bmp: TGPBitmap;
begin
//  OutputDebugString( 'DrawEmptyBK' );
//  OutputDebugString( PChar('UpdateR-Left: ' + IntToStr(AUpdateR.Left) + ' Rigth: ' + IntTostr(AUpdateR.Right) +
//                     'Top: ' + IntToStr(AUpdateR.Top) + 'Bottom: ' + IntToStr(AUpdateR.Bottom))  );
  if not Assigned(FCellImage.Image) then 
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect( ADestR );
    Exit;
  end;
    
  nW := ADestR.Right - ADestR.Left;
  nH := ADestR.Bottom - ADestR.Top;
  CalcUpDateR := AUpdateR;
  CalcUpDateR.Left := ADestR.Left;
  CalcUpDateR.Right := ADestR.Right;

  nRow := RowCount + 1 - FixedRows;
  nCount := nRow + (nH + DefaultRowHeight - 1) div DefaultRowHeight;
  DestR := MakeRect( ADestR.Left, 0, ADestR.Right - ADestR.Left, DefaultRowHeight );

  bmp := TGPBitmap.Create( nW, nH );
  G := TGPGraphics.Create( bmp );

  for i := nRow to nCount - 1 do
  begin
    if i mod 2 = 0 then 
    begin
      SrcBmpR := MakeRect(0, 0, Integer(FCellImage.Image.GetWidth), Integer(FCellImage.Image.GetHeight) div 5);
      Inc( SrcBmpR.Y, SrcBmpR.Height );
    end
    else
      SrcBmpR := MakeRect(0, 0, Integer(FCellImage.Image.GetWidth), Integer(FCellImage.Image.GetHeight) div 5);
      
    DR := Rect( ADestR.Left, DestR.Y + ADestR.Top, ADestR.Right, DestR.Y + ADestR.Top + DestR.Height );

    if IntersectRect( R, DR, CalcUpDateR ) then
    begin
//      OutputDebugString( PChar('DR-Left: ' + IntToStr(DR.Left) + ' Rigth: ' + IntTostr(DR.Right) +
//                     'Top: ' + IntToStr(DR.Top) + 'Bottom: ' + IntToStr(DR.Bottom))  );
      DrawStretchImage( G, FCellImage.Image, DestR, SrcBmpR );
    end;
//    DrawStretchImage( G, FCellImage.Image, DestR, SrcBmpR );  
    Inc(DestR.Y, DefaultRowHeight);
  end;
  G.Free;  
//  bmp.Save( 'E:\CompanyWork\MusicT\KBox2.1\Bin\t.bmp', GetEncoderClsid(ImgFormatBitmap) );
  nX := AUpdateR.Left;
  nY := AUpdateR.Top;
  if nY < ADestR.Top then
    nY := ADestR.Top;
  nW := AUpdateR.Right - AUpdateR.Left;
  nH := AUpdateR.Bottom - AUpdateR.Top;
  if nH > Integer(bmp.GetHeight) then
    nH := Integer(bmp.GetHeight);

  nSrcX := nX;
  if ADestR.Top < AUpdateR.Top then
    nSrcY := AUpdateR.Top - ADestR.Top
  else
    nSrcY := 0;
//  OutputDebugString( PChar('SrcLeft: ' + IntToStr(nSrcX) + ' SrcTop: ' + IntTostr(nSrcY) ));
//  OutputDebugString( PChar('DestLeft: ' + IntToStr(nX) + ' DestRTop: ' + IntTostr(nY) ));
//  OutputDebugString( PChar('W: ' + IntToStr(nW) + ' H: ' + IntTostr(nH) ));
  G := TGPGraphics.Create( Canvas.Handle );
  G.DrawImage( bmp, nX, nY, nSrcX, nSrcY, nW, nH, UnitPixel );
//  G.DrawImage( bmp, ADestR.Left, ADestR.Top, 0, 0, nW, nH, UnitPixel );
  bmp.Free;
  G.Free;
end;

procedure TxdStringGrid.DrawEmptyRowEx(ADestR: TRect);
var
  i, nIndex, nW, nH: Integer;
  R: TRect;
  DestR: TGPRect;
  bmp: TGPImage;
  BmpR: TGPRect;
  G: TGPGraphics;
  s: TxdGpDrawStyle;
begin
//  OutputDebugString( PChar('DrawEmptyRowEx') );
  Inc( ADestR.Left, -2 );
  G := TGPGraphics.Create( Canvas.Handle );
  s := FCellImageDrawMethod.DrawStyle;
  for i := 0 to RowCount - 1 do
  begin
    if i < FixedRows then
    begin
      //Fixed
      bmp := FixedImage.Image;
      if not Assigned(bmp) then Continue;      
      BmpR := MakeRect(0, 0, Integer(bmp.GetWidth), Integer(bmp.GetHeight) div FFixedImage.ImageCount);
      s := FCellImageDrawMethod.DrawStyle;
    end
    else
    begin
      //Cell
      bmp := CellImage.Image;
      if not Assigned(bmp) then Continue;
      BmpR := MakeRect(0, 0, Integer(bmp.GetWidth), Integer(bmp.GetHeight) div 5);
      s := FCellImageDrawMethod.DrawStyle;

      nIndex := 0;
  
      if FHighRow = i then
        nIndex := 4
      else if IsRowSelected(i) then
        nIndex := 3
      else 
      begin
        if FCurMouseCell.FRowIndex = i then
          nIndex := 2;
        
        if nIndex = 0 then
        begin
          if i mod 2 = 0 then 
            nIndex := 1
        end;
      end;

      Inc( BmpR.Y, BmpR.Height * nIndex );
    end;
    

    R := CellRect(ColCount - 1, i);
    DestR.X := ADestR.Left;
    DestR.Y := R.Top;
    DestR.Width := ADestR.Right - ADestR.Left;
    DestR.Height := R.Bottom - R.Top;

    case s of
      dsPaste: 
      begin
        if BmpR.Width > DestR.Width then
          nW := DestR.Width
        else
        begin
          nW := BmpR.Width;
          DestR.Width := nW;
        end;

        if BmpR.Height > DestR.Height then
          nH := BmpR.Height
        else
        begin
          nH := BmpR.Height;
          DestR.Height := nH;
        end;        
        G.DrawImage( bmp, DestR, BmpR.X, BmpR.Y, nW, nH, UnitPixel );
      end;
      dsStretchByVH: 
      begin
        if goRowSelect in Options then
        begin
          //整行选择
          //图片自动拉伸参数设置
          if ColCount = 1 then
            DrawStretchImage( G, bmp, DestR, BmpR )
          else
          begin
            Inc( BmpR.X, BmpR.Width div 2 );
            BmpR.Width := 1;
            G.DrawImage( bmp, DestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );
          end;
        end
        else      
          DrawStretchImage( G, bmp, DestR, BmpR );
      end
      else  G.DrawImage( bmp, DestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );
    end;
  end;

  G.Free;
end;

procedure TxdStringGrid.DrawFixedBK(const AGh: TGPGraphics; ACol, ARow: Integer; ADestR: TGPRect; AState: TGridDrawState);
var
  BmpR: TGPRect;
  nTemp, nH, nW: Integer;
begin
  if not Assigned(FFixedImage.Image) then Exit;

  {
    FFixedImageDrawMethod.DrawStyle 只支持：dsPaste, dsStretchByVH, dsStretchAll
    FFixedImage.ImageCount 决定绘制的状态：最大为3；
  }
  BmpR := MakeRect(0, 0, Integer(FFixedImage.Image.GetWidth), 
    Integer(FFixedImage.Image.GetHeight) div FFixedImage.ImageCount);
  
  if goRowSelect in Options then
  begin
    if FCurDownCell.FRowIndex = ARow then
    begin
      if FFixedImage.ImageCount >= 3 then
        Inc(BmpR.Y, BmpR.Height * 2)
      else if FFixedImage.ImageCount >= 2 then
        Inc(BmpR.Y, BmpR.Height);
    end
    else if FCurMouseCell.FRowIndex = ARow then
    begin
      if FFixedImage.ImageCount >= 2 then
        Inc(BmpR.Y, BmpR.Height);
    end;
  end
  else
  begin
    if (FCurDownCell.FRowIndex = ARow) and (FCurDownCell.FColIndex = ACol) then
    begin
      if FFixedImage.ImageCount >= 3 then
        Inc(BmpR.Y, BmpR.Height * 2)
      else if FFixedImage.ImageCount >= 2 then
        Inc(BmpR.Y, BmpR.Height);
    end
    else if (FCurMouseCell.FRowIndex = ARow) and (FCurMouseCell.FColIndex = ACol) then
    begin
      if FFixedImage.ImageCount >= 2 then
        Inc(BmpR.Y, BmpR.Height);
    end;
  end;

  case FFixedImageDrawMethod.DrawStyle of
    dsPaste: 
    begin
      if BmpR.Width > ADestR.Width then
        nW := ADestR.Width
      else
      begin
        nW := BmpR.Width;
        ADestR.Width := nW;
      end;

      if BmpR.Height > ADestR.Height then
        nH := BmpR.Height
      else
      begin
        nH := BmpR.Height;
        ADestR.Height := nH;
      end;        
      AGh.DrawImage( FFixedImage.Image, ADestR, BmpR.X, BmpR.Y, nW, nH, UnitPixel );
    end;
    dsStretchByVH: 
    begin
      if goRowSelect in Options then
      begin
        //整行选择
        //图片自动拉伸参数设置
        if ColCount = 1 then
          DrawStretchImage( AGh, FFixedImage.Image, ADestR, BmpR )
        else
        begin
          if ACol = 0 then
          begin
            //最左边
            nW := ADestR.Width;
            nTemp := BmpR.Width div 2;
            BmpR.Width := nTemp;
            ADestR.Width :=  BmpR.Width;
            AGh.DrawImage( FFixedImage.Image, ADestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );

            Inc(BmpR.X, nTemp); 
            Inc(ADestR.X, nTemp);
            BmpR.Width := 1;
            ADestR.Width := nW - nTemp;
            AGh.DrawImage( FFixedImage.Image, ADestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );
          end
          else if ACol = ColCount - 1 then
          begin
            //最右边
            nW := BmpR.Width div 2;
            nTemp := ADestR.Width - nW + BmpR.Width mod 2;
            Inc(BmpR.X, nW + BmpR.Width mod 2);
            BmpR.Width := 1;            
            ADestR.Width := nTemp;
            AGh.DrawImage( FFixedImage.Image, ADestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );

            Inc(BmpR.X, 1); 
            Inc(ADestR.X, nTemp);
            BmpR.Width := nW;
            ADestR.Width := nW;
            AGh.DrawImage( FFixedImage.Image, ADestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );
          end
          else
          begin
            Inc( BmpR.X, BmpR.Width div 2 );
            BmpR.Width := 1;
            AGh.DrawImage( FFixedImage.Image, ADestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );
          end;
        end;
      end
      else      
        DrawStretchImage( AGh, FFixedImage.Image, ADestR, BmpR );
    end
    else  
      AGh.DrawImage( FFixedImage.Image, ADestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );
  end;  
end;

procedure TxdStringGrid.DrawRowCellBK(const AGh: TGPGraphics; ACol, ARow: Integer; ADestR: TGPRect; AState: TGridDrawState);
var
  nTemp, nW, nH: Integer;
  BmpR: TGPRect;
begin
  if not Assigned(FCellImage.Image) then Exit;
    {
    CellBitmap图片信息：
      1：奇数行背景
      2：偶数行背景
      3：鼠标在某一行时背景
      4：选中时背景
      5：高亮时背景
  end;}
    //使用图片来绘制界面
  BmpR := MakeRect(0, 0, Integer(FCellImage.Image.GetWidth), 
    Integer(FCellImage.Image.GetHeight) div 5);
      
  nTemp := 0;
  
  if FHighRow = ARow then
    nTemp := 4
  else if IsRowSelected(ARow) then
    nTemp := 3
  else 
  begin
    if goRowSelect in Options then
    begin
      if FCurMouseCell.FRowIndex = ARow then
        nTemp := 2;
    end
    else
    begin
      if (FCurMouseCell.FRowIndex = ARow) and (FCurMouseCell.FColIndex = ACol) then
        nTemp := 2;
    end;
    if nTemp = 0 then
    begin
      if ARow mod 2 = 0 then 
        nTemp := 1
    end;
  end;

  Inc( BmpR.Y, BmpR.Height * nTemp );

  case FCellImageDrawMethod.DrawStyle of
    dsPaste: 
    begin
      if BmpR.Width > ADestR.Width then
        nW := ADestR.Width
      else
      begin
        nW := BmpR.Width;
        ADestR.Width := nW;
      end;

      if BmpR.Height > ADestR.Height then
        nH := BmpR.Height
      else
      begin
        nH := BmpR.Height;
        ADestR.Height := nH;
      end;        
      AGh.DrawImage( FCellImage.Image, ADestR, BmpR.X, BmpR.Y, nW, nH, UnitPixel );
    end;
    dsStretchByVH: 
    begin
      if goRowSelect in Options then
      begin
        //整行选择
        //图片自动拉伸参数设置
        if ColCount = 1 then
          DrawStretchImage( AGh, FCellImage.Image, ADestR, BmpR )
        else
        begin
          if ACol = 0 then
          begin
            //最左边
            nW := ADestR.Width;
            nTemp := BmpR.Width div 2;
            BmpR.Width := nTemp;
            ADestR.Width :=  BmpR.Width;
            AGh.DrawImage( FCellImage.Image, ADestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );

            Inc(BmpR.X, nTemp); 
            Inc(ADestR.X, nTemp);
            BmpR.Width := 1;
            ADestR.Width := nW - nTemp;
            AGh.DrawImage( FCellImage.Image, ADestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );
          end
          else if ACol = ColCount - 1 then
          begin
            //最右边
            nW := BmpR.Width div 2;
            nTemp := ADestR.Width - nW + BmpR.Width mod 2;
            Inc(BmpR.X, nW + BmpR.Width mod 2);
            BmpR.Width := 1;            
            ADestR.Width := nTemp;
            AGh.DrawImage( FCellImage.Image, ADestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );

            Inc(BmpR.X, 1); 
            Inc(ADestR.X, nTemp);
            BmpR.Width := nW;
            ADestR.Width := nW;
            AGh.DrawImage( FCellImage.Image, ADestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );
          end
          else
          begin
            Inc( BmpR.X, BmpR.Width div 2 );
            BmpR.Width := 1;
            AGh.DrawImage( FCellImage.Image, ADestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );
          end;
        end;
      end
      else      
        DrawStretchImage( AGh, FCellImage.Image, ADestR, BmpR );
    end
    else  AGh.DrawImage( FCellImage.Image, ADestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );
  end;  
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

procedure TxdStringGrid.InvalidateRow(const ARow: Integer);
var
  R: TRect;
begin
  if not HandleAllocated then Exit;
  R := CellRect(0, ARow);
  R.Right := Width;
  Windows.InvalidateRect(Handle, @R, False);
end;

function TxdStringGrid.IsActiveControl: Boolean;
var
  H: Hwnd;
  ParentForm: TCustomForm;
begin
  Result := False;
  ParentForm := GetParentForm(Self);
  if Assigned(ParentForm) then
  begin
    if (ParentForm.ActiveControl = Self) then
      Result := True
  end
  else
  begin
    H := GetFocus;
    while IsWindow(H) and (Result = False) do
    begin
      if H = WindowHandle then
        Result := True
      else
        H := GetParent(H);
    end;
  end;
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
        DoHorizontalScrollPosChanged( FScrollBarHorizontal, csKey );
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
        DoVericalScrollPosChanged( FScrollBarVerical, csKey );
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
  DoVericalScrollPosChanged( nil, csMouse );
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
      FSelectRect := Rect( X, Y, -1, -1 );
    end;
    DoDownPosChanged( nRow, nCol );
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
  R: TRect;
  nOldX, nOldY: Integer;
begin  
  inherited;
  MouseToCell( x, y, nCol, nRow1 );
  DoMousePosChanged( nRow1, nCol );
  if (goRangeSelect in Options) and (FSelectRect.Left <> -1) and (FSelectRect.Top <> -1) then
  begin 
    R := FSelectRect;
    nOldX := R.Right;
    nOldY := R.Bottom;
    FSelectRect.Right := X;
    FSelectRect.Bottom := Y; 
    
    nRow1 := FCurDownCell.FRowIndex;
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
    if R.Right < FSelectRect.Right then
      R.Right := FSelectRect.Right;
    if R.Bottom < FSelectRect.Bottom then
      R.Bottom := FSelectRect.Bottom;
    if R.Left > R.Right then
    begin
      i := R.Left;
      R.Left := R.Right;
      R.Right := i;
      if R.Left > nOldX then
        R.Left := nOldX;
    end;
    if R.Top > R.Bottom then
    begin
      i := R.Top;
      R.Top := R.Bottom;
      R.Bottom := i; 
      if R.Top > nOldY then
        R.Top := nOldY;
    end;
    InvalidateRect(Handle, @R, False );    
  end;
end;

procedure TxdStringGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  R := FSelectRect;
  FSelectRect := Rect( -1, -1, -1, -1 );
  FCurDownCell.FRowIndex := -1;
  FCurDownCell.FColIndex := -1;
  InvalidateRect(Handle, @R, False );
  inherited;
end;

procedure TxdStringGrid.Paint;
var
  ps: TPenStyle;
  bs: TBrushStyle;
  nColor: TColor;
begin
//  Exit;
  if FPainting then Exit;  
  FPainting := True;
  try
    ParentPaint; //用来代替父类的绘制 
  //  inherited Paint;

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
  finally
    FPainting := False;
  end;
end;

function PointInGridRect(Col, Row: Longint; const Rect: TGridRect): Boolean;
begin
  Result := (Col >= Rect.Left) and (Col <= Rect.Right) and (Row >= Rect.Top)
    and (Row <= Rect.Bottom);
end;

procedure TxdStringGrid.ParentPaint;
var
  DrawInfo: TGridDrawInfo;
  Sel: TGridRect;
  UpdateRect, DrawR, R: TRect;

  procedure DrawCells(ACol, ARow: Longint; StartX, StartY, StopX, StopY: Integer;
    Color: TColor; IncludeDrawState: TGridDrawState);
  var
    CurCol, CurRow: Longint;
    Where: TRect;
    DrawState: TGridDrawState;
    Focused: Boolean;
  begin
    CurRow := ARow;
    Where.Top := StartY;
    while (Where.Top < StopY) and (CurRow < RowCount) do
    begin
      CurCol := ACol;
      Where.Left := StartX;
      Where.Bottom := Where.Top + RowHeights[CurRow];
//      OutputDebugString( PChar('Where.Left:' + IntToStr(Where.Left) + ' StopX: ' + IntToStr(StopX) ) );
      while (Where.Left < StopX) and (CurCol < ColCount) do
      begin
        Where.Right := Where.Left + ColWidths[CurCol];
        if (Where.Right > Where.Left) and RectVisible(Canvas.Handle, Where) then
        begin
          DrawState := IncludeDrawState;
          Focused := IsActiveControl;
          if Focused and (CurRow = Row) and (CurCol = Col)  then
          begin
            SetCaretPos(Where.Left, Where.Top);          
            Include(DrawState, gdFocused);
          end;
          if PointInGridRect(CurCol, CurRow, Sel) then
            Include(DrawState, gdSelected);
          if not (gdFocused in DrawState) or not (goEditing in Options) or
            not EditorMode or (csDesigning in ComponentState) then
          begin

            DrawCell(CurCol, CurRow, Where, DrawState);
          end;
        end;
        Where.Left := Where.Right + DrawInfo.Horz.EffectiveLineWidth;
        Inc(CurCol);
      end;
      Where.Top := Where.Bottom + DrawInfo.Vert.EffectiveLineWidth;
      Inc(CurRow);
    end;
  end;

begin
  if UseRightToLeftAlignment then ChangeGridOrientation(True);

  UpdateRect := Canvas.ClipRect;
//  OutputDebugString( PChar('Left: ' + IntToStr(UpdateRect.Left) + ' Rigth: ' + IntTostr(UpdateRect.Right) +
//                           'Top: ' + IntToStr(UpdateRect.Top) + 'Bottom: ' + IntToStr(UpdateRect.Bottom))  );
  CalcDrawInfo(DrawInfo);
  with DrawInfo do
  begin
    //不绘制线条

    { Draw the cells in the four areas }
    Sel := Selection;
    DrawCells(0, 0, 0, 0, Horz.FixedBoundary, Vert.FixedBoundary, FixedColor, [gdFixed]);
    DrawCells(LeftCol, 0, Horz.FixedBoundary, 0, Horz.GridBoundary, Vert.FixedBoundary, FixedColor, [gdFixed]);
    DrawCells(0, TopRow, 0, Vert.FixedBoundary, Horz.FixedBoundary, Vert.GridBoundary, FixedColor, [gdFixed]);
    DrawCells(LeftCol, TopRow, Horz.FixedBoundary, Vert.FixedBoundary, Horz.GridBoundary, Vert.GridBoundary, Color, []);

    if Horz.GridBoundary < Horz.GridExtent then
    begin      
      DrawR := Rect(Horz.GridBoundary, 0, Horz.GridExtent, Vert.GridBoundary);
      if IntersectRect( R, DrawR, UpdateRect ) then
        DrawEmptyRowEx(R);
    end;
    if Vert.GridBoundary < Vert.GridExtent then
    begin
      DrawR := Rect(0, Vert.GridBoundary, Horz.GridExtent, Vert.GridExtent);
//      OutputDebugString( PChar('R-Left: ' + IntToStr(DrawR.Left) + ' Rigth: ' + IntTostr(DrawR.Right) +
//                           'Top: ' + IntToStr(DrawR.Top) + 'Bottom: ' + IntToStr(DrawR.Bottom))  );
      if IntersectRect( R, DrawR, UpdateRect ) then
        DrawEmptyBK( DrawR, UpdateRect );
//      DrawEmptyBK( UpdateRect );
    end;
  end;

  if UseRightToLeftAlignment then ChangeGridOrientation(False);
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

procedure TxdStringGrid.SetCellFont(const Value: TFontInfo);
begin
  FCellFont.Assign( Value );
end;

procedure TxdStringGrid.SetCellImage(const Value: TImageInfo);
begin
  FCellImage.Assign( Value );
end;

procedure TxdStringGrid.SetCellImageDrawMethod(const Value: TDrawMethod);
begin
  FCellImageDrawMethod.Assign( Value );
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

procedure TxdStringGrid.SetFixedFont(const Value: TFontInfo);
begin
  FFixedFont.Assign( Value );
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
  if (FCurDownCell.FRowIndex <> -1) and (FCurDownCell.FColIndex <> -1) then
  begin
    R := CellRect( FCurDownCell.FColIndex, FCurDownCell.FRowIndex );
    FSelectRect.Top := R.Top;
    if R.Top = 0 then
    begin
      if FCurDownCell.FRowIndex < TopRow then
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
