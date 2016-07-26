{
TabSet
作者: Terry(江晓德)
QQ:   67068633
Email:jxd524@163.com
最后修改时间 2012-1-30
}
unit uJxdGpTabSet;

interface
uses
  Classes, Windows, Controls, Graphics, Messages, ExtCtrls, SysUtils,
  GDIPAPI, GDIPOBJ, uJxdGpBasic, uJxdGpStyle;

type
  //每个页的信息
  PTabSetInfo = ^TTabSetInfo;
  TTabSetInfo = record
    FTitle: string;
    FData: Pointer;
    FPos: TRect;
  end;
  //鼠标移动的信息
  PMousePosInfo = ^TMousePosInfo;
  TMousePosInfo = record
    FIndex: Integer;
    FMouseState: TxdGpUIState;
    FIsInCloseRect: Boolean;
  end;
  //移动页时所需要信息
  PMoveTabSetInfo = ^TMoveTabSetInfo;
  TMoveTabSetInfo = record
    FMoveIndex: Integer;
    FMoveToLeft: Boolean;
  end;

  TOnTabSet = function(Sender: TObject; const AIndex: Integer): Boolean of object;
  TOnCurSelectIndexChanged = procedure(Sender: TObject; const ACurSelIndex, ABeforSelIndex: Integer) of object;
  TOnDrawIcon = procedure(Sender: TObject; ABmp: TBitmap; const AIconRect: TRect) of object;

  TxdTabSet = class( TxdGraphicsBasic )
  public
    //主要操作函数
    function  AddTabSet(const ATitle: string; AData: Pointer = nil): Integer;
    function  UpdateTabSetData(const AIndex: Integer; AData: Pointer): Boolean;
    function  UpdateTabSetText(const AIndex: Integer; const ATitle: string): Boolean;
    function  GetTabSetData(const AIndex: Integer): Pointer;
    function  CanMoveTabSet(const AMoveToLeft: Boolean): Boolean;
    procedure MoveTabSet(const AMoveToLeft: Boolean);
    procedure Delete(const AIndex: Integer);

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    //消息处理
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDown;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    //主要绘画函数
    procedure DrawGraphics(const AGh: TGPGraphics); override;
    procedure DoControlStateChanged(const AOldState, ANewState: TxdGpUIState; const ACurPt: TPoint); override;

    procedure DrawPanel(ABufBitmap: TBitmap);
    procedure DrawTabSet(const ATabSetIndex: Integer; p: PTabSetInfo; ABmp: TBitmap);
    //运行时加载
    procedure Loaded; override;
  private
    //自定义变量
    FTabSetList: TList;             //存放页链表
    FMouseMoveInfo: TMousePosInfo;  //当前鼠标移动位置信息
    FTabSetHeight: Integer;   //页高
    FCurTabSetWidth: Integer; //当前页宽

    FTabSetCloseBmpWidth, FTabSetCloseBmpHeight: Integer;
    FCurTabSetCloseRIndex: Integer;

    //自定义函数
    function  CalcPos(pt: TPoint; var ATabSetIndex: Integer; var AIsInCloseRect: Boolean): Boolean;
    function  CheckAllTabSetWidth: Boolean;
    procedure ReCalcCurTabSetWidth;
    procedure InvalidateTabSet(const AIndex: Integer; const AIsOnlyRedrawCloseRect: Boolean);
    procedure MoveToIndex(const AIndex: Integer; const AMoveToLeft: Boolean); overload;
    procedure MoveToIndex(const AIndex: Integer); overload;
    function  GetMoveTabSetIndex(const AMoveToLeft: Boolean): Integer;
    function  IsTabSetVisible(const ATabSetIndex: Integer): Boolean;
    function  FormatTitle(const ATitle: WideString): string;
    function  TabSetRectToCloseRect(const ARect: TRect): TRect;
    function  IconRect(const ATabSetRect: TRect): TRect;

    procedure DoMoveTabSetEvent(Sender: TObject);
    procedure DoMoveTabSetByDeleteEvent(Sender: TObject);

    function  DoDeleteTabSet(const AIndex: Integer): Boolean;
    procedure DoTabSetChanged(const ABeforSelIndex: Integer);
    procedure DoShowTabSetHint;
    procedure DoDrawIcon(ABmp: TBitmap; const ABmpRect: TRect);
  private
    //自动生成变量
    FCurSelIndex: Integer;
    FLeftSpace: Integer;
    FRightSpace: Integer;
    FTabSetSpace: Integer;
    FMoveSpeed: Integer;
    FTabSetBitmap: TBitmap;
    FTabSetCloseBitmap: TBitmap;
    FTabSetCloseTopSpace: Integer;
    FTabSetCloseRightSpace: Integer;
    FFontMargins: TRect;
    FOnTabSetChanged: TOnCurSelectIndexChanged;
    FOnDeleteTabSet: TOnTabSet;
    FOnHintTabSet: TOnTabSet;
    FOnTabSetDrawIcon: TOnDrawIcon;
    FIconHeight: Integer;
    FIconWidth: Integer;
    FIconLeftSpace: Integer;
    FIconDraw: Boolean;
    FIconTopSpace: Integer;
    //自动生成函数
    function  GetTabSetCount: Integer;
    procedure SetLeftSpace(const Value: Integer);
    procedure SetMoveSpeed(const Value: Integer);
    procedure SetTabSetBmp(const Value: TBitmap);
    procedure SetFontMargins(const Value: TRect);
    procedure SetRightSpace(const Value: Integer);
    procedure SetTabSetCloseBitmap(const Value: TBitmap);
    procedure SetCurSelTabSetIndex(const Value: Integer);
    procedure SetTabSetCloseTopSpace(const Value: Integer);
    procedure SetTabSetCloseRightSpace(const Value: Integer);
    procedure SetOnTabSetChanged(const Value: TOnCurSelectIndexChanged);
    procedure SetTabSetSpace(const Value: Integer);
    procedure SetIconHeight(const Value: Integer);
    procedure SetIconWidth(const Value: Integer);
    procedure SetIconLeftSpace(const Value: Integer);
    procedure SetIconDraw(const Value: Boolean);
    procedure SetIconTopSpace(const Value: Integer);
  published
    property CurSelTabSetIndex: Integer read FCurSelIndex write SetCurSelTabSetIndex;
    property LeftSpace: Integer read FLeftSpace write SetLeftSpace;
    property RightSpace: Integer read FRightSpace write SetRightSpace;
    property MoveSpeed: Integer read FMoveSpeed write SetMoveSpeed;
    property TabSetSpace: Integer read FTabSetSpace write SetTabSetSpace;
    property TabSetBitmap: TBitmap read FTabSetBitmap write SetTabSetBmp;
    property TabSetCloseBitmap: TBitmap read FTabSetCloseBitmap write SetTabSetCloseBitmap;
    property TabSetCloseTopSpace: Integer read FTabSetCloseTopSpace write SetTabSetCloseTopSpace;
    property TabSetCloseRightSpace: Integer read FTabSetCloseRightSpace write SetTabSetCloseRightSpace;
    property CurTabSetCount: Integer read GetTabSetCount;
    property FontMargins: TRect read FFontMargins write SetFontMargins;
    property IconDraw: Boolean read FIconDraw write SetIconDraw;
    property IconWidth: Integer read FIconWidth write SetIconWidth;
    property IconHeight: Integer read FIconHeight write SetIconHeight;
    property IconLeftSpace: Integer read FIconLeftSpace write SetIconLeftSpace;
    property IconTopSpace: Integer read FIconTopSpace write SetIconTopSpace;
    //事件
    property OnTabSetChanged: TOnCurSelectIndexChanged read FOnTabSetChanged write SetOnTabSetChanged;
    property OnTabSetDelete: TOnTabSet read FOnDeleteTabSet write FOnDeleteTabSet;
    property OnTabSetShowHint: TOnTabSet read FOnHintTabSet write FOnHintTabSet;
    property OnTabSetDrawIcon: TOnDrawIcon read FOnTabSetDrawIcon write FOnTabSetDrawIcon; 
  end;

implementation

const
  CtMaxTabSetWidth = 250;
  CtMinTabSetWidth = 100;

{ TJxdGradientTabSet }

function TxdTabSet.AddTabSet(const ATitle: string; AData: Pointer): Integer;
var
  p, pPre: PTabSetInfo;
begin
  New( p );
  p^.FTitle := ATitle;
  p^.FData := AData;
  if FTabSetList.Count > 0 then
  begin
    pPre := FTabSetList[FTabSetList.Count - 1];
    with p^.FPos do
    begin
      Left := pPre^.FPos.Right + FTabSetSpace;
      Right := Left + FCurTabSetWidth;
      Top := pPre^.FPos.Top;
      Bottom := pPre^.FPos.Bottom;
    end;
  end
  else
  begin
    with p^.FPos do
    begin
      Left := 0;
      Right := Left + FCurTabSetWidth;
      Top := Self.Height - FTabSetHeight;
      Bottom := Top + FTabSetHeight;
    end;
  end;
  Result := FTabSetList.Add( p );
  CheckAllTabSetWidth;
  if FCurSelIndex = -1 then
  begin
    FCurSelIndex := Result;
    DoTabSetChanged( -1 );
  end;
  Invalidate;
end;

function TxdTabSet.CalcPos(pt: TPoint; var ATabSetIndex: Integer; var AIsInCloseRect: Boolean): Boolean;
var
  p: PTabSetInfo;
  R: TRect;
  nCount: Integer;
begin
  Result := False;
  nCount := FTabSetList.Count;
  if (nCount > 0) and     //判断List
     (pt.Y <= Height) and (pt.Y >= Height - FTabSetHeight) and //判断上下位置
     (pt.X <= Width - FRightSpace) and (pt.X >= FLeftSpace) then //判断左右位置
  begin
    p := FTabSetList[ 0 ];
    pt.X := pt.X - FLeftSpace;
    ATabSetIndex := ( pt.X + abs(p^.FPos.Left) ) div ( FCurTabSetWidth + FTabSetSpace );
    if (ATabSetIndex >= 0) and (ATabSetIndex < nCount) then
    begin
      p := FTabSetList[ ATabSetIndex ];
      if not PtInRect(p^.FPos, pt) then Exit;
      R := TabSetRectToCloseRect( p^.FPos );
      AIsInCloseRect := PtInRect( R, pt );
      Result := True;
    end;
  end;
end;

function TxdTabSet.CanMoveTabSet(const AMoveToLeft: Boolean): Boolean;
var
  p: PTabSetInfo;
begin
  Result := False;
  if FTabSetList.Count <= 1 then Exit;

  if AMoveToLeft then
  begin
    p := FTabSetList[ FTabSetList.Count - 1 ];
    Result := p^.FPos.Right > Width - FRightSpace;
  end
  else
  begin
    p := FTabSetList[ 0 ];
    Result := p^.FPos.Left < 0;
  end;
end;

function TxdTabSet.CheckAllTabSetWidth: Boolean;
var
  i: Integer;
  p, pPre: PTabSetInfo;
begin
  Result := False;
  if FTabSetList.Count = 0 then Exit;
  ReCalcCurTabSetWidth;

  pPre := FTabSetList[0];
  pPre^.FPos.Right := pPre^.FPos.Left + FCurTabSetWidth;
  for i := 1 to FTabSetList.Count - 1 do
  begin
    p := FTabSetList[i];
    p^.FPos.Left := pPre^.FPos.Right + FTabSetSpace;
    p^.FPos.Right := p^.FPos.Left + FCurTabSetWidth;
    pPre := p;
  end;
end;

procedure TxdTabSet.CMHintShow(var Message: TMessage);
begin
  if FMouseMoveInfo.FIndex <> -1 then
    DoShowTabSetHint;
end;

procedure TxdTabSet.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TxdTabSet.CMMouseLeave(var Message: TMessage);
var
  nIndex: Integer;
begin
  inherited;
  if FMouseMoveInfo.FIndex <> -1 then
  begin
    nIndex := FMouseMoveInfo.FIndex;
    FMouseMoveInfo.FIndex := -1;
    InvalidateTabSet( nIndex, False );
  end;
end;

constructor TxdTabSet.Create(AOwner: TComponent);
begin
  inherited;
  Caption                := '';
  FTabSetList            := TList.Create;
  FLeftSpace             := 20;
  FRightSpace            := 40;
  FTabSetSpace           := 4;
  FTabSetHeight          := 29;
  FMoveSpeed             := 20;
  FCurTabSetWidth        := CtMaxTabSetWidth;
  FCurSelIndex           := -1;
  FTabSetBitmap          := TBitmap.Create;
  FFontMargins.Left      := 5;
  FFontMargins.Right     := 5;
  FFontMargins.Top       := 5;
  FFontMargins.Bottom    := 0;
  FTabSetCloseBitmap     := TBitmap.Create;
  FTabSetCloseBmpWidth   := 0;
  FTabSetCloseBmpHeight  := 0;
  FTabSetCloseTopSpace   := 4;
  FTabSetCloseRightSpace := 4;
  FCurTabSetCloseRIndex  := -1;
  FIconHeight            := 16;
  FIconWidth             := 16;
  FIconLeftSpace         := 3;
  FIconTopSpace          := -1;
  FIconDraw              := True;
  FMouseMoveInfo.FIndex := -1;
  ShowHint := True;
end;

procedure TxdTabSet.Delete(const AIndex: Integer);
  //重新计算TabSet各个页的位置，ABeginIndex指定的页的Left属性需要在外部进行设置
  procedure ReCalcTabSet(const ABeginIndex, AEndIndex: Integer);
  var
    i: Integer;
    pre, p: PTabSetInfo;
  begin
    pre := FTabSetList[ ABeginIndex ];
    pre^.FPos.Right := pre^.FPos.Left + FCurTabSetWidth;
    for i := ABeginIndex + 1 to AEndIndex do
    begin
      p := FTabSetList[i];
      p^.FPos.Left := pre^.FPos.Right + FTabSetSpace;
      p^.FPos.Right := p^.FPos.Left + FCurTabSetWidth;
      pre := p;
    end;
  end;
var
  p, pTemp: PTabSetInfo;
  nCount, nOldWidth: Integer;
  R: TRect;
begin
  if (AIndex < 0) or (AIndex >= FTabSetList.Count) or (not DoDeleteTabSet(AIndex)) then Exit;

  p := FTabSetList[ AIndex ];
  nCount := FTabSetList.Count;
  FTabSetList.Delete( AIndex );
  Dispose( p );
  nOldWidth := FCurTabSetWidth;
  ReCalcCurTabSetWidth;

  if FTabSetList.Count = 0 then
  begin
    nCount := CurSelTabSetIndex;
    FCurSelIndex := -1;
    DoTabSetChanged( nCount );
    Invalidate;
    Exit;
  end;


  //删除选中页
  if AIndex = FCurSelIndex then
  begin
    CurSelTabSetIndex := AIndex mod FTabSetList.Count;
    DoTabSetChanged( -1 );
  end
  else if AIndex < FCurSelIndex then
    Dec( FCurSelIndex ); 
  
  if AIndex = 0 then
  begin
    //删除第一项
    ReCalcTabSet( 0, FTabSetList.Count - 1 );
    MoveToIndex( 0 );
  end
  else if AIndex = nCount - 1 then
  begin
    //删除最后一项
    ReCalcTabSet( 0, FTabSetList.Count - 1 );
    Invalidate;
  end
  else
  begin
    //删除中间某一页
    ReCalcTabSet( 0, AIndex - 1 );
    p := FTabSetList[ AIndex - 1 ];
    R.Left := FLeftSpace;
    R.Top := p^.FPos.Top;
    R.Bottom := p^.FPos.Bottom;
    R.Right := p^.FPos.Right + FLeftSpace;
    InvalidateRect( R );

    pTemp := FTabSetList[ AIndex ];
    pTemp^.FPos.Left := p^.FPos.Right + FTabSetSpace + nOldWidth;
    ReCalcTabSet( AIndex, FTabSetList.Count - 1 );
    MoveToIndex( AIndex );
  end;
end;

destructor TxdTabSet.Destroy;
var
  i: Integer;
begin
  for i := 0 to FTabSetList.Count - 1 do
    Dispose( PTabSetInfo(FTabSetList[i]) );
  FTabSetList.Free;
  FTabSetBitmap.Free;
  FTabSetCloseBitmap.Free;
  inherited;
end;

procedure TxdTabSet.DoControlStateChanged(const AOldState, ANewState: TxdGpUIState; const ACurPt: TPoint);
begin

end;

function TxdTabSet.DoDeleteTabSet(const AIndex: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnTabSetDelete) then
    Result := OnTabSetDelete( Self, AIndex ); 
end;

procedure TxdTabSet.DoDrawIcon(ABmp: TBitmap; const ABmpRect: TRect);
begin
  if Assigned(OnTabSetDrawIcon) then
    OnTabSetDrawIcon( Self, ABmp, IconRect(ABmpRect) );
end;

procedure TxdTabSet.DoMoveTabSetByDeleteEvent(Sender: TObject);
var
  tmr: TTimer;
  i, nValue, nMoveIndex, nMoveLeftMost: Integer;
  bFinished: Boolean;
  pTabSet: PTabSetInfo;
  R: TRect;
begin
  if not (Sender is TTimer) then Exit;
  tmr := Sender as TTimer;
  tmr.Enabled := False;
  nMoveIndex := tmr.Tag;
  if ( nMoveIndex < 0) or (nMoveIndex > FTabSetList.Count) then
  begin
    tmr.Free;
    Exit;
  end;
  if nMoveIndex = 0 then
    nMoveLeftMost := 0
  else
  begin
    pTabSet := FTabSetList[ nMoveIndex - 1 ];
    nMoveLeftMost := pTabSet^.FPos.Right + FTabSetSpace;
  end;
  bFinished := False;
  R.Left := nMoveLeftMost;
  try
    pTabSet := FTabSetList[ nMoveIndex ];
    R.Top := pTabSet^.FPos.Top;
    R.Bottom := pTabSet^.FPos.Bottom;
    if pTabSet^.FPos.Left - FMoveSpeed > nMoveLeftMost then
      nValue := FMoveSpeed
    else
    begin
      nValue := pTabSet^.FPos.Left - nMoveLeftMost;
      bFinished := True;
    end;
    nValue := -nValue;
    for i := nMoveIndex to FTabSetList.Count - 1 do
    begin
      pTabSet := FTabSetList[i];
      Inc( pTabSet^.FPos.Left, nValue );
      Inc( pTabSet^.FPos.Right, nValue );
    end;
    R.Right := Width - FRightSpace;
    InvalidateRect( R );
  finally
    if bFinished then
      tmr.Free
    else
      tmr.Enabled := True;
  end;
end;

procedure TxdTabSet.DoMoveTabSetEvent(Sender: TObject);
var
  tmr: TTimer;
  i, nValue: Integer;
  pTabSet: PTabSetInfo;
  pMove: PMoveTabSetInfo;
  bFinished: Boolean;
begin
  if not (Sender is TTimer) then Exit;
  tmr := Sender as TTimer;
  tmr.Enabled := False;
  pMove := PMoveTabSetInfo( tmr.Tag );
  if (not Assigned(pMove)) or ( pMove^.FMoveIndex < 0) or (pMove^.FMoveIndex > FTabSetList.Count) then
  begin
    tmr.Free;
    Exit;
  end;
  bFinished := False;
  try
    pTabSet := FTabSetList[ pMove^.FMoveIndex ];
    if pMove^.FMoveToLeft then
    begin
      if pTabSet^.FPos.Right + FLeftSpace - FMoveSpeed > Width - FRightSpace then
        nValue := FMoveSpeed
      else
      begin
        nValue := pTabSet^.FPos.Right + FLeftSpace - Width + FRightSpace;
        bFinished := True;
      end;
      nValue := -nValue;
    end
    else
    begin
      if pTabSet^.FPos.Left + FMoveSpeed < 0 then
        nValue := FMoveSpeed
      else
      begin
        nValue := - pTabSet^.FPos.Left;
        bFinished := True;
      end;
    end;

    for i := 0 to FTabSetList.Count - 1 do
    begin
      pTabSet := FTabSetList[i];
      Inc( pTabSet^.FPos.Left, nValue );
      Inc( pTabSet^.FPos.Right, nValue );
    end;
    Invalidate;
  finally
    if bFinished then
    begin
      tmr.Free;
      Dispose( pMove );
    end
    else
      tmr.Enabled := True;
  end;
end;

procedure TxdTabSet.DoShowTabSetHint;
begin
  if Assigned(OnTabSetShowHint) then
    OnTabSetShowHint( Self, FMouseMoveInfo.FIndex );
end;

procedure TxdTabSet.DoTabSetChanged(const ABeforSelIndex: Integer);
begin
  if Assigned(FOnTabSetChanged) then
    FOnTabSetChanged( Self, CurSelTabSetIndex, ABeforSelIndex );
end;

procedure TxdTabSet.DrawGraphics(const AGh: TGPGraphics);
begin

end;

procedure TxdTabSet.DrawPanel(ABufBitmap: TBitmap);
var
  i: Integer;
  p: PTabSetInfo;
  nRight: Integer;
  Bmp: TBitmap;
  xDest, xSrc, nW: Integer;
begin
  inherited;
  if FTabSetList.Count = 0 then Exit;
  nRight := Width - FRightSpace;
  Bmp := TBitmap.Create;
  try
    Bmp.Width := FCurTabSetWidth;
    Bmp.Height := FTabSetHeight;
    for i := 0 to FTabSetList.Count - 1 do
    begin
      if not IsTabSetVisible( i ) then Continue;
      p := FTabSetList[i];
      DrawTabSet( i, p, Bmp );

      if p^.FPos.Left >= 0 then
      begin
        xDest := p^.FPos.Left + FLeftSpace;
        xSrc := 0;
        nW := FCurTabSetWidth;
      end
      else
      begin
        xDest := FLeftSpace;
        xSrc := abs(p^.FPos.Left);
        nW := FCurTabSetWidth - xSrc;
      end;
      if xDest + nW > nRight then
        nW := nRight - xDest;
      ABufBitmap.Canvas.Brush.Style := bsClear;
      SetBkMode( ABufBitmap.Canvas.Handle, TRANSPARENT );
      ABufBitmap.Canvas.BrushCopy( Rect(xDest, p^.FPos.Top, xDest + nW, p^.FPos.Top + FTabSetHeight ),
                                   Bmp, Rect(xSrc, 0, xSrc + nW, FTabSetHeight ), clFuchsia );
    end;
  finally
    Bmp.Free;
  end;
end;

procedure TxdTabSet.DrawTabSet(const ATabSetIndex: Integer; p: PTabSetInfo; ABmp: TBitmap);
var
  TabSetSrcBmpRt, TabSetCloseSrcBmpRt, R, CloseDestBmpRt: TRect;
  strTitle: string;
  nSpace: Integer;
begin
  R := Rect( 0, 0, ABmp.Width, ABmp.Height );
  CloseDestBmpRt := R;
  if not FTabSetBitmap.Empty then
  begin
    if ATabSetIndex = FCurSelIndex then
    begin
      //绘画当前选中页
      TabSetSrcBmpRt := Rect(0, FTabSetHeight * 2, FTabSetBitmap.Width, FTabSetHeight * 3 );
    end
    else if FMouseMoveInfo.FIndex = ATabSetIndex then
    begin
      //鼠标移动到当前绘画页
      TabSetSrcBmpRt := Rect(0, FTabSetHeight, FTabSetBitmap.Width, FTabSetHeight * 2 );
    end
    else
    begin
      //平时状态
      TabSetSrcBmpRt := Rect(0, 0, FTabSetBitmap.Width, FTabSetHeight );
    end;
    //画背景
//    DrawRectangle( ABmp.Canvas, TabSetSrcBmpRt, R, FTabSetBitmap );
  end
  /////////////////////////////////无背景图时//////////////////////////////////////////
  else
  if ATabSetIndex = FCurSelIndex then
  begin
    ABmp.Canvas.Brush.Color := $2D4FFF;
    ABmp.Canvas.FillRect( Rect(0, 0, ABmp.Width, Abmp.Height) );
  end
  else
  begin
    ABmp.Canvas.Brush.Color := Random( GetTickCount );
    ABmp.Canvas.FillRect( Rect(0, 0, ABmp.Width, Abmp.Height) );
  end;
  /////////////////////////////结束无背景图////////////////////////////////////////////

  //绘制小图标
  if IconDraw then
  begin
    DoDrawIcon( ABmp, Rect(0, 0, ABmp.Width, ABmp.Height) );
    nSpace := FIconWidth;
  end
  else
    nSpace := 0;

  //绘制文字
  if p^.FTitle <> '' then
  begin
    strTitle := FormatTitle( p^.FTitle );
    Inc( R.Left, FFontMargins.Left + nSpace );
    Inc( R.Top, FFontMargins.Top );
    Dec( R.Right, FFontMargins.Right );
    Dec( R.Right, FTabSetCloseBmpWidth );
    ABmp.Canvas.Font := Font;
    ABmp.Canvas.Brush.Style := bsClear;
    DrawText(ABmp.Canvas.Handle, PChar(strTitle), Length(strTitle), R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

  //绘制关闭按钮
  if not FTabSetCloseBitmap.Empty then
  begin
    //关闭按钮图片位置
    TabSetCloseSrcBmpRt := Rect(0, 0, FTabSetCloseBmpWidth, FTabSetCloseBmpHeight );
    if (ATabSetIndex = FMouseMoveInfo.FIndex) and FMouseMoveInfo.FIsInCloseRect then
    begin
      case FMouseMoveInfo.FMouseState of
        uiActive: OffsetRect( TabSetCloseSrcBmpRt, 0, FTabSetCloseBmpHeight );
        uiDown: OffsetRect( TabSetCloseSrcBmpRt, 0, FTabSetCloseBmpHeight * 2 );
      end;
    end;
    //以透明方式画出按钮
    CloseDestBmpRt := TabSetRectToCloseRect( CloseDestBmpRt );
    ABmp.Canvas.Brush.Style := bsClear;
    SetBkMode( ABmp.Canvas.Handle, TRANSPARENT );
    ABmp.Canvas.BrushCopy( CloseDestBmpRt, FTabSetCloseBitmap, TabSetCloseSrcBmpRt, clFuchsia );
  end;
end;

function TxdTabSet.FormatTitle(const ATitle: WideString): string;
var
  nW, nFontLen, n: Integer;
begin
  nW := FCurTabSetWidth - FFontMargins.Left - FFontMargins.Right;
  if IconDraw then
    Dec( nW, FIconWidth );
  if FTabSetCloseBmpWidth > 0 then
    Dec( nW, FTabSetCloseBmpWidth );
    
//  nFontLen := Canvas.TextWidth( ATitle );
  if nFontLen <= nW then
    Result := ATitle
  else
  begin
    Result := Copy( ATitle, 1, Length(ATitle) - 4 ) + '...';
//    nFontLen := Canvas.TextWidth( Result );
    n := 4;
    while nFontLen > nW do
    begin
      Inc( n, 2 );
      Result := Copy( ATitle, 1, Length(ATitle) - n ) + '...';
//      nFontLen := Canvas.TextWidth( Result );
    end;
  end;
end;

function TxdTabSet.GetMoveTabSetIndex(const AMoveToLeft: Boolean): Integer;
var
  p0, p1, nLen, nIndex: Integer;
  p: PTabSetInfo;
begin
  Result := -1;
  if AMoveToLeft then
  begin
    p := FTabSetList[ FTabSetList.Count - 1 ];
    p0 := Width - FRightSpace;
    p1 := p^.FPos.Right;
  end
  else
  begin
    p := FTabSetList[ 0 ];
    p0 := FLeftSpace;
    p1 := p^.FPos.Right;
  end;
  nLen := abs(p0 - p1);
  nIndex := nLen div (FCurTabSetWidth + FTabSetSpace);
  if AMoveToLeft then
    nIndex := FTabSetList.Count - 1 - nIndex;
  if (nIndex >= 0) and (nIndex <= FTabSetList.Count - 1) then
    Result := nIndex;
end;

function TxdTabSet.GetTabSetCount: Integer;
begin
  if Assigned(FTabSetList) then
    Result := FTabSetList.Count
  else
    Result := 0;
end;

function TxdTabSet.GetTabSetData(const AIndex: Integer): Pointer;
begin
  if (AIndex < 0) or (AIndex >= FTabSetList.Count) then
    Result := nil
  else
    Result :=  PTabSetInfo(FTabSetList[AIndex])^.FData;
end;

function TxdTabSet.IconRect(const ATabSetRect: TRect): TRect;
begin
  if (FIconHeight = 0) or (FIconWidth = 0) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  Result.Left := ATabSetRect.Left + FIconLeftSpace;
  Result.Right := Result.Left + FIconWidth;
  if FIconTopSpace = -1 then
    Result.Top := (ATabSetRect.Bottom - ATabSetRect.Top - FIconHeight) div 2
  else
    Result.Top := ATabSetRect.Top + FIconTopSpace ;
  Result.Bottom := Result.Top + FIconHeight;
end;

procedure TxdTabSet.InvalidateTabSet(const AIndex: Integer; const AIsOnlyRedrawCloseRect: Boolean);
var
  p: PTabSetInfo;
  R: TRect;
begin
  if (AIndex >= 0) and (AIndex < FTabSetList.Count) then
  begin
    p := FTabSetList[ AIndex ];
    R := p^.FPos;
    Inc( R.Left, FLeftSpace );
    Inc( R.Right, FLeftSpace ); 
    if AIsOnlyRedrawCloseRect then
      R := TabSetRectToCloseRect( R );
    InvalidateRect( R );
  end;
end;

function TxdTabSet.IsTabSetVisible(const ATabSetIndex: Integer): Boolean;
var
  p: PTabSetInfo;
  nW: Integer;
begin
  nW := Width - FRightSpace;
  p := FTabSetList[ ATabSetIndex ];
  Result := ( (p^.FPos.Left < 0) and (p^.FPos.Right > 0) ) or
            ( (p^.FPos.Left >= 0) and (p^.FPos.Left <= nW) );
end;

procedure TxdTabSet.Loaded;
begin
  inherited;
  if Assigned(FTabSetBitmap) and (not FTabSetBitmap.Empty) then  
    FTabSetHeight := FTabSetBitmap.Height div 3;
  if Assigned(FTabSetCloseBitmap) and (not FTabSetCloseBitmap.Empty) then
  begin
    FTabSetCloseBmpWidth := FTabSetCloseBitmap.Width;
    FTabSetCloseBmpHeight := FTabSetCloseBitmap.Height div 3;
  end;
  ShowHint := True;
end;

procedure TxdTabSet.WMLButtonDown(var Message: TWMLButtonDown);
var
  nIndex: Integer;
  bInCloseBtn: Boolean;
begin
  inherited;
  if FMouseMoveInfo.FIndex <> -1 then
  begin
    if CalcPos( Point(Message.XPos, Message.YPos), nIndex, bInCloseBtn ) then
    begin
      if bInCloseBtn then
      begin
        //在关闭按钮按下
        if FMouseMoveInfo.FMouseState <> uiDown then
        begin
          FMouseMoveInfo.FMouseState := uiDown;
          FMouseMoveInfo.FIsInCloseRect := True;
          InvalidateTabSet( FMouseMoveInfo.FIndex, True );
        end;
      end
      else
      begin
        //在页上页按下鼠标
        if FMouseMoveInfo.FMouseState <> uiDown then
        begin
          FMouseMoveInfo.FMouseState := uiDown;
          FMouseMoveInfo.FIsInCloseRect := False;
          InvalidateTabSet( FMouseMoveInfo.FIndex, False );
        end;
      end;
    end;
  end;
end;

procedure TxdTabSet.WMLButtonUp(var Message: TWMLButtonUp);
var
  nIndex: Integer;
  bInCloseBtn: Boolean;
begin
  inherited;
  if CalcPos( Point(Message.XPos, Message.YPos), nIndex, bInCloseBtn ) then
  begin
    FMouseMoveInfo.FMouseState := uiActive;
    if not bInCloseBtn then
      CurSelTabSetIndex := nIndex
    else
      Delete( nIndex );
  end;
end;

procedure TxdTabSet.WMMouseMove(var Message: TWMMouseMove);
var
  nTemp, nIndex: Integer;
  bIsInCloseRt: Boolean;
begin
  inherited;
  if not CalcPos( Point(Message.XPos, Message.YPos), nIndex, bIsInCloseRt ) then
  begin
    if FMouseMoveInfo.FIndex <> -1 then
    begin
      nIndex := FMouseMoveInfo.FIndex;
      FMouseMoveInfo.FIndex := -1;
      InvalidateTabSet( nIndex, False );
    end;
  end
  else
  begin
    if FMouseMoveInfo.FIndex = -1 then
    begin
      FMouseMoveInfo.FIndex := nIndex;
      FMouseMoveInfo.FIsInCloseRect := bIsInCloseRt;
      FMouseMoveInfo.FMouseState := uiActive;
      InvalidateTabSet( nIndex, False );
    end
    else if (FMouseMoveInfo.FIndex = nIndex) and (FMouseMoveInfo.FIsInCloseRect <> bIsInCloseRt) then
    begin
      FMouseMoveInfo.FIsInCloseRect := bIsInCloseRt;
      InvalidateTabSet( nIndex, True );
    end
    else if FMouseMoveInfo.FIndex <> nIndex then
    begin
      nTemp := FMouseMoveInfo.FIndex;
      FMouseMoveInfo.FIndex := nIndex;
      FMouseMoveInfo.FMouseState := uiActive;
      FMouseMoveInfo.FIsInCloseRect := bIsInCloseRt;
      nIndex := nTemp;
      InvalidateTabSet( nIndex, False );
      InvalidateTabSet( FMouseMoveInfo.FIndex, False );
    end;
  end;
end;

procedure TxdTabSet.MoveTabSet(const AMoveToLeft: Boolean);
var
  nMoveIndex: Integer;
begin
  if not CanMoveTabSet(AMoveToLeft) then Exit;
  nMoveIndex := GetMoveTabSetIndex( AMoveToLeft );
  if nMoveIndex = -1 then Exit;
  MoveToIndex( nMoveIndex, AMoveToLeft );
end;

procedure TxdTabSet.MoveToIndex(const AIndex: Integer);
begin
  with TTimer.Create( Self ) do
  begin
    OnTimer := DoMoveTabSetByDeleteEvent;
    Tag := AIndex;
    Interval := 50;
    Enabled := True;
  end;
end;

procedure TxdTabSet.MoveToIndex(const AIndex: Integer; const AMoveToLeft: Boolean);
var
  pMoveInfo: PMoveTabSetInfo;
begin
  New( pMoveInfo );
  pMoveInfo^.FMoveIndex := AIndex;
  pMoveInfo^.FMoveToLeft := AMoveToLeft;
  with TTimer.Create( Self ) do
  begin
    OnTimer := DoMoveTabSetEvent;
    Tag := Integer( PMoveInfo );
    Interval := 50;
    Enabled := True;
  end;
end;

procedure TxdTabSet.ReCalcCurTabSetWidth;
var
  nWidth, nUseWidth: Integer;
begin
  FCurTabSetWidth := CtMaxTabSetWidth;
  if FTabSetList.Count = 0 then Exit;
  nUseWidth := Width - FLeftSpace - FRightSpace;
  if FTabSetList.Count > 1 then
    nUseWidth := nUseWidth - (FTabSetList.Count - 1) * FTabSetSpace;

  nWidth := nUseWidth div FTabSetList.Count;

  if nWidth > CtMaxTabSetWidth then
    FCurTabSetWidth := CtMaxTabSetWidth
  else if nWidth >= CtMinTabSetWidth then
    FCurTabSetWidth := nWidth
  else
    FCurTabSetWidth := CtMinTabSetWidth;
end;

procedure TxdTabSet.SetCurSelTabSetIndex(const Value: Integer);
var
  p: PTabSetInfo;
  bMoveToLeft: Boolean;
  nTemp: Integer;
begin
  if (Value >= 0) and (Value < FTabSetList.Count) and (FCurSelIndex <> Value) then
  begin
    p := FTabSetList[Value];
    bMoveToLeft := p^.FPos.Right > Width - FRightSpace;
    if (p^.FPos.Left < 0) or bMoveToLeft then
      MoveToIndex( Value, bMoveToLeft);
    nTemp := FCurSelIndex;
    FCurSelIndex := Value;
    DoTabSetChanged( nTemp );
    InvalidateTabSet( nTemp, False );
    InvalidateTabSet( FCurSelIndex, False );
  end;
end;

procedure TxdTabSet.SetFontMargins(const Value: TRect);
begin
  FFontMargins := Value;
  Invalidate;
end;

procedure TxdTabSet.SetIconDraw(const Value: Boolean);
begin
  if FIconDraw <> Value then
  begin
    FIconDraw := Value;
    Invalidate;
  end;
end;

procedure TxdTabSet.SetIconHeight(const Value: Integer);
begin
  if (FIconHeight <> Value) and (Value >= 0) then
  begin
    FIconHeight := Value;
    Invalidate;
  end;
end;

procedure TxdTabSet.SetIconLeftSpace(const Value: Integer);
begin
  if (Value <> FIconLeftSpace) and (Value >= 0) then
  begin
    FIconLeftSpace := Value;
    Invalidate;
  end;
end;

procedure TxdTabSet.SetIconTopSpace(const Value: Integer);
begin
  if (Value <> FIconTopSpace) and (Value >= -1) then
  begin
    FIconTopSpace := Value;
    Invalidate;
  end;
end;

procedure TxdTabSet.SetIconWidth(const Value: Integer);
begin
  if (FIconWidth <> Value) and (Value >= 0) then
  begin
    FIconWidth := Value;
    Invalidate;
  end;
end;

procedure TxdTabSet.SetLeftSpace(const Value: Integer);
begin
  if (FLeftSpace <> Value) and (Value >= 0) then
  begin
    FLeftSpace := Value;
    Invalidate;
  end;
end;

procedure TxdTabSet.SetMoveSpeed(const Value: Integer);
begin
  if (FMoveSpeed <> Value) and (Value > 0) then
    FMoveSpeed := Value;
end;

procedure TxdTabSet.SetOnTabSetChanged(const Value: TOnCurSelectIndexChanged);
begin
  FOnTabSetChanged := Value;
end;

procedure TxdTabSet.SetRightSpace(const Value: Integer);
begin
  if (FRightSpace <> Value) and (Value >= 0) then
  begin
    FRightSpace := Value;
    Invalidate;
  end;
end;

procedure TxdTabSet.SetTabSetBmp(const Value: TBitmap);
begin
  FTabSetBitmap.Assign( Value );
  Invalidate;
end;

procedure TxdTabSet.SetTabSetCloseBitmap(const Value: TBitmap);
begin
  FTabSetCloseBitmap.Assign( Value );
  Invalidate;
end;

procedure TxdTabSet.SetTabSetCloseRightSpace(const Value: Integer);
begin
  if FTabSetCloseRightSpace <> Value then
  begin
    FTabSetCloseRightSpace := Value;
    Invalidate;
  end;
end;

procedure TxdTabSet.SetTabSetCloseTopSpace(const Value: Integer);
begin
  if FTabSetCloseTopSpace <> Value then
  begin
    FTabSetCloseTopSpace := Value;
    Invalidate;
  end;
end;

procedure TxdTabSet.SetTabSetSpace(const Value: Integer);
begin
  if (FTabSetSpace <> Value) and (Value >= 0) then
  begin
    FTabSetSpace := Value;
    CheckAllTabSetWidth;
    Invalidate;
  end;
end;

function TxdTabSet.TabSetRectToCloseRect(const ARect: TRect): TRect;
begin
  if (not Assigned(FTabSetCloseBitmap)) or FTabSetCloseBitmap.Empty then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  Result := ARect;
  Result.Left := Result.Right - FTabSetCloseBmpWidth - FTabSetCloseRightSpace;
  Result.Top := Result.Top + FTabSetCloseTopSpace;
  Result.Right := Result.Left + FTabSetCloseBmpWidth;
  Result.Bottom := Result.Top + FTabSetCloseBmpHeight;
end;

function TxdTabSet.UpdateTabSetData(const AIndex: Integer; AData: Pointer): Boolean;
begin
  if (AIndex < 0) or (AIndex >= FTabSetList.Count) then
    Result := False
  else
  begin
    PTabSetInfo(FTabSetList[AIndex])^.FData := AData;
    Result := True;
  end;
end;

function TxdTabSet.UpdateTabSetText(const AIndex: Integer; const ATitle: string): Boolean;
begin
  if (AIndex < 0) or (AIndex >= FTabSetList.Count) then
    Result := False
  else
  begin
    PTabSetInfo(FTabSetList[AIndex])^.FTitle := ATitle;
    InvalidateRect( PTabSetInfo(FTabSetList[AIndex])^.FPos );
    Result := True;
  end;
end;

procedure TxdTabSet.WMSize(var Message: TWMSize);
begin
  inherited;
  CheckAllTabSetWidth;
end;

end.
