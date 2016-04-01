unit uJxdTabSet;

interface
uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Graphics, uJxdGraphicBaseClass, uJxdGuiStyle,
  uJxdParseGradient, Messages, uJxdDrawSub;

type
  PTabSetInfo = ^TTabSetInfo;
  TTabSetInfo = record
  private
    FCaption: string;
    FTag: Integer;
    FRect: TRect;
    FSelected: Boolean;
  public
    property TabCaption: string read FCaption ;
    property TabTag: Integer read FTag;
    property TabSelected: Boolean read FSelected;
    property TabRect: TRect read FRect;
  end;

  TOnCloseTab = procedure(Sender: TObject; const Ap: PTabSetInfo) of object;
  TOnTabSelected = procedure(Sender: TObject; const Ap: PTabSetInfo) of object;
  TOnTabClick = procedure(Sender: TObject; const Ap: PTabSetInfo) of object;

  TxdTabSet = class(TxdGraphicBase)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  AddTabHeader(const ACaption: string; const ATag: Integer = 0; const ASelected: Boolean = False): Integer;
    procedure DeleteTabSet(const AIndex: Integer);
    function  IsExsitsTabHeader(const ATag: Integer): Boolean;
    function  FindTabHeaderIndex(const ATag: Integer): Integer;
    function  FindTabHeaderInfo(const ATag: Integer): PTabSetInfo;
    function  GetTabHeaderInfo(const AIndex: Integer): PTabSetInfo;
    procedure UpdateTabHeaderCaption(const AIndex: Integer; const ACaption: string);
    procedure SetTabHeaderSelected(const ATag: Integer);
  private
    FDeling: Boolean;
    FDelBmpWidth, FDelBmpHeight: Integer;
    FCurMouseOnIndex: Integer;
    FCurDrawFirstIndex: Integer;
    FCurLeftOffset: Integer;
    FTabSetInfoList: TList;
    FMoveTime: TTimer;
    FCurMoveSize: Integer;
    procedure ClearList;
    function  GetCurTabWidth: Integer;
    procedure ReCalcTabSetPosition;
    function  MouseToTabSet(const x, y: Integer): Integer;
    function  IsMouseInTabClose(const AIndex, x, y: Integer): Boolean;

    procedure SetCurLeftOffset(const Value: Integer);
    procedure SetCurMouseOnIndex(const Value: Integer);

    procedure DrawGradientBack(ABufBmp: TBitmap);
    procedure DrawBitmapBack(ABufBmp: TBitmap; const ADrawRect: TRect);
    procedure DrawTab(ABufBmp: TBitmap);
    procedure DrawTabHeader(ABmp: TBitmap; const AIndex: Integer; const ApTabInfo: PTabSetInfo);

    procedure DoDrawInfoChanged(Sender: TObject);
    procedure DoTimerMove(Sender: TObject);
  protected
    procedure Resize; override;
    procedure DrawGraphiControl(ABufBmp: TBitmap); override;
    procedure DoControlStateChanged(const AOldState, ANewState: TxdComponentState; const ACurPt: TPoint); override;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure InvalidateTab(const AIndex: Integer);
    procedure InvalidateTabClose(const AIndex: Integer);

    procedure DoClickTab(const AIndex, Ax, Ay: Integer);

    property  CurLeftOffset: Integer read FCurLeftOffset write SetCurLeftOffset;
    property  CurMouseOnIndex: Integer read FCurMouseOnIndex write SetCurMouseOnIndex;
  private
    FTabMinWidth: Integer;
    FTabMaxWidth: Integer;
    FTabSpace: Integer;
    FTabSetLeftSpace: Integer;
    FTabSetTopSpace: Integer;
    FTabSetRightSpace: Integer;
    FTabSetBottomSpace: Integer;
    FTabSetBackGradient: TGradientDrawInfo;
    FTabSetBackBmpInfo: TBitmapInfo;
    FTabMoveSpeed: Integer;
    FTabDelBmpInfo: TBitmapInfo;
    FTabDelRightSpace: Integer;
    FTabDelTopSpace: Integer;
    FOnTabSelected: TOnTabSelected;
    FOnTabClosed: TOnCloseTab;
    FOnTabClick: TOnTabClick;
    FTabHeaderBmpInof: TBitmapInfo;
    procedure SetTabMaxWidth(const Value: Integer);
    procedure SetTabMinWidth(const Value: Integer);
    procedure SetTabSpace(const Value: Integer);
    procedure SetTabSetLeftSpace(const Value: Integer);
    procedure SetTabSetTopSpace(const Value: Integer);
    procedure SetTabSetRightSpace(const Value: Integer);
    procedure SetTabSetBottomSpace(const Value: Integer);
    procedure SetTabSetBackGradient(const Value: TGradientDrawInfo);
    procedure SetTabSetBackBmpInfo(const Value: TBitmapInfo);
    procedure SetTabMoveSpeed(const Value: Integer);
    function  GetTabCount: Integer;
    function  GetTabSelected(AIndex: Integer): Boolean;
    procedure SetTabSelected(AIndex: Integer; const Value: Boolean);
    procedure SetTabDelBmpInfo(const Value: TBitmapInfo);
    procedure SetTabDelRightSpace(const Value: Integer);
    procedure SetTabDelTopSpace(const Value: Integer);
    procedure SetTabHeaderBmpInof(const Value: TBitmapInfo);
  published
    property TabMoveSpeed: Integer read FTabMoveSpeed write SetTabMoveSpeed;
    property TabMaxWidth: Integer read FTabMaxWidth write SetTabMaxWidth;
    property TabMinWidth: Integer read FTabMinWidth write SetTabMinWidth;

    property TabSpace: Integer read FTabSpace write SetTabSpace; //Tab之间的距离
    property TabSetLeftSpace: Integer read FTabSetLeftSpace write SetTabSetLeftSpace;//第一个显示的 Tab 离Left的距离
    property TabSetTopSpace: Integer read FTabSetTopSpace write SetTabSetTopSpace; //每一个Tab离Top的距离
    property TabSetRightSpace: Integer read FTabSetRightSpace write SetTabSetRightSpace; //最后一个显示的Tab离Right的最小距离
    property TabSetBottomSpace: Integer read FTabSetBottomSpace write SetTabSetBottomSpace; //每个Tab离Bottom的最小距离
    property TabDelTopSpace: Integer read FTabDelTopSpace write SetTabDelTopSpace;
    property TabDelRightSpace: Integer read FTabDelRightSpace write SetTabDelRightSpace; 

    property TabCount: Integer read GetTabCount;

    property TabDeleteBitmap: TBitmapInfo read FTabDelBmpInfo write SetTabDelBmpInfo;
    property TabSetBackBitmap: TBitmapInfo read FTabSetBackBmpInfo write SetTabSetBackBmpInfo;
    property TabSetBackGradient: TGradientDrawInfo read FTabSetBackGradient write SetTabSetBackGradient;

    property TabHeaderBitmap: TBitmapInfo read FTabHeaderBmpInof write SetTabHeaderBmpInof;

    property OnTabSelected: TOnTabSelected read FOnTabSelected write FOnTabSelected;
    property OnTabClosed: TOnCloseTab read FOnTabClosed write FOnTabClosed;
    property OnTabClick: TOnTabClick read FOnTabClick write FOnTabClick;  
  public
    property TabSelected[AIndex: Integer]: Boolean read GetTabSelected write SetTabSelected;
  end;


implementation

const
  CtTabMaxWidthDefault = 150;
  CtTabMinWidthDefault = 40;
  
{ TxdTabSet }

function TxdTabSet.AddTabHeader(const ACaption: string; const ATag: Integer; const ASelected: Boolean): Integer;
var
  p: PTabSetInfo;
begin
  New( p );
  p^.FCaption := ACaption;
  p^.FTag := ATag;
  if FTabSetInfoList.Count = 0 then
    p^.FRect.Left := 0;

  p^.FSelected := False;
  Result := FTabSetInfoList.Add( p );
  ReCalcTabSetPosition;
  if ASelected then
    TabSelected[Result] := True;

  Invalidate;
end;

procedure TxdTabSet.ClearList;
var
  i: Integer;
begin
  for i := 0 to FTabSetInfoList.Count - 1 do
    Dispose( FTabSetInfoList[i] );
  FTabSetInfoList.Clear;
end;

constructor TxdTabSet.Create(AOwner: TComponent);
begin
  inherited;
  FTabSetInfoList := TList.Create;
  FTabMinWidth := CtTabMinWidthDefault;
  FTabMaxWidth := CtTabMaxWidthDefault;
  FTabSpace := 0;
  FTabSetLeftSpace := 5;
  FTabSetTopSpace := 3;
  FTabSetBottomSpace := 0;
  FTabSetRightSpace := 20;
  FCurLeftOffset := 0;
  FCurDrawFirstIndex := 0;
  FCurMoveSize := 0;
  FTabMoveSpeed := 2;
  FCurMouseOnIndex := -1;
  FMoveTime := nil;
  FDelBmpWidth := 16;
  FDelBmpHeight := 16;
  FTabDelTopSpace := 6;
  FTabDelRightSpace := 6;

  FTabSetBackGradient := TGradientDrawInfo.Create;
  FTabSetBackGradient.OnChange := DoDrawInfoChanged;

  FTabSetBackBmpInfo := TBitmapInfo.Create;
  FTabSetBackBmpInfo.OnChange := DoDrawInfoChanged;

  FTabDelBmpInfo := TBitmapInfo.Create;
  FTabDelBmpInfo.OnChange := DoDrawInfoChanged;

  FTabHeaderBmpInof := TBitmapInfo.Create;
  FTabHeaderBmpInof.OnChange := DoDrawInfoChanged;
end;

procedure TxdTabSet.DeleteTabSet(const AIndex: Integer);
var
  p: PTabSetInfo;
  nIndex, nLeft: Integer;
  bReCalc, bSel: Boolean;
begin
  FDeling := True;
  try
    if (AIndex >= 0) and (AIndex < FTabSetInfoList.Count) then
    begin
      bReCalc := AIndex = 0;
      p := FTabSetInfoList[AIndex];
      nLeft := p^.FRect.Left;
      bSel := p^.FSelected;
      FTabSetInfoList.Delete( AIndex );
      if Assigned( OnTabClosed ) then
        OnTabClosed( Self, p );
      Dispose( p );

      if FTabSetInfoList.Count > 0 then
      begin
        if bReCalc then
          PTabSetInfo( FTabSetInfoList[0] )^.FRect.Left := nLeft;
        ReCalcTabSetPosition;

        FCurMouseOnIndex := -1;
        CurLeftOffset := 0;

        if bSel then
        begin
          if AIndex < FTabSetInfoList.Count then
            nIndex := AIndex
          else
            nIndex := AIndex - 1;
          TabSelected[nIndex] := True;
        end;
      end;
      Invalidate;
    end;
  finally
    FDeling := False;
  end;
end;

destructor TxdTabSet.Destroy;
begin
  ClearList;
  FreeAndNil( FTabSetInfoList );
  FTabSetBackGradient.Free;
  inherited;
end;

procedure TxdTabSet.DoClickTab(const AIndex, Ax, Ay: Integer);
var
  bClickClose: Boolean;
begin
  bClickClose := IsMouseInTabClose( AIndex, Ax, Ay);
  if bClickClose then
  begin
    DeleteTabSet( AIndex );
  end
  else
  begin
    TabSelected[ AIndex ] := True;
    if Assigned(OnTabClick) then
      OnTabClick( Self, FTabSetInfoList[AIndex] );
  end;
end;

procedure TxdTabSet.DoControlStateChanged(const AOldState, ANewState: TxdComponentState; const ACurPt: TPoint);
var
  nIndex: Integer;
begin
  if (AOldState = csDown)  then
  begin
    nIndex := MouseToTabSet(ACurPt.X, ACurPt.Y);
    if nIndex <> -1 then
      DoClickTab( nIndex, ACurPt.X, ACurPt.Y );
  end;
  if ANewState = csNormal then
    FCurMouseOnIndex := -1;
  Invalidate;
end;

procedure TxdTabSet.DrawBitmapBack(ABufBmp: TBitmap; const ADrawRect: TRect);
var
  SrcR: TRect;
  state: TxdComponentState;
  nH: Integer;
begin
  nH := FTabSetBackBmpInfo.Bitmap.Height div FTabSetBackBmpInfo.BitmapCount;
  SrcR := Rect( 0, 0, FTabSetBackBmpInfo.Bitmap.Width, nH );
  state := GetCurControlState;
  if state = csDown then
  begin
    if FTabSetBackBmpInfo.BitmapCount in [3, 2] then
      OffsetRect( SrcR, 0, nH * (FTabSetBackBmpInfo.BitmapCount - 1) );
  end
  else if state = csActive then
  begin
    if FTabSetBackBmpInfo.BitmapCount = 3 then
      OffsetRect( SrcR, 0, nH * (FTabSetBackBmpInfo.BitmapCount - 2) )
    else if FTabSetBackBmpInfo.BitmapCount = 2 then
      OffsetRect( SrcR, 0, nH * (FTabSetBackBmpInfo.BitmapCount - 1) );
  end;
  DrawRectangle( FTabSetBackBmpInfo.Bitmap, ABufBmp.Canvas, SrcR, ADrawRect, FTabSetBackBmpInfo.BitmapDrawStyle, IsTransColor, TransColor );
end;

procedure TxdTabSet.DrawGradientBack(ABufBmp: TBitmap);
var
  ParseGradient: TParseGradient;
  state: TxdComponentState;
begin
  state := GetCurControlState;
  case state of
    csNormal: ParseGradient := FTabSetBackGradient.ParseGradientNormal;
    csActive: ParseGradient := FTabSetBackGradient.ParseGradientHover;
    else      ParseGradient := FTabSetBackGradient.ParseGradientMouseDown;
  end;
  DrawGradientInfo( ABufBmp.Canvas, ParseGradient, FTabSetBackGradient.GradientWay = gwLeftToRigth, 0, 0, Width, Height );
end;

procedure TxdTabSet.DrawGraphiControl(ABufBmp: TBitmap);
begin
  if FTabSetBackBmpInfo.Bitmap.Empty then
    DrawGradientBack( ABufBmp )
  else
    DrawBitmapBack( ABufBmp, Rect(0, 0, ABufBmp.Width, ABufBmp.Height) );
  DrawTab( ABufBmp );
end;

procedure TxdTabSet.DrawTab(ABufBmp: TBitmap);
var
  i, nLeft, nMaxWidth: Integer;
  p: PTabSetInfo;
  SrcR, DestR: TRect;
  bmp: TBitmap;
  nSrcLeft, nSrcWidth, nDestLeft, nDestWidth: Integer;
begin
  if FDeling or (FTabSetInfoList.Count = 0) then Exit;

  if not FTabDelBmpInfo.Bitmap.Empty then
  begin
    FDelBmpWidth := FTabDelBmpInfo.Bitmap.Width;
    FDelBmpHeight := FTabDelBmpInfo.Bitmap.Height div FTabDelBmpInfo.BitmapCount;
  end;

  bmp := TBitmap.Create;
  try
    if (FCurDrawFirstIndex < 0) or (FCurDrawFirstIndex >= FTabSetInfoList.Count) then Exit;

    p := FTabSetInfoList[FCurDrawFirstIndex];
    bmp.Width := WidthOfRect( p^.FRect );
    bmp.Height := HeightOfRect( p^.FRect );

    if p^.FRect.Left > FCurLeftOffset then
      nLeft := FTabSetLeftSpace + p^.FRect.Left - FCurLeftOffset
    else
      nLeft := FTabSetLeftSpace;

    nMaxWidth := Width - FTabSetRightSpace;
    for I := FCurDrawFirstIndex to FTabSetInfoList.Count - 1 do
    begin
      p := FTabSetInfoList[i];
      if nLeft > nMaxWidth then Break;

      DrawTabHeader( bmp, i, p );

      if p^.FRect.Left < FCurLeftOffset then
        nSrcLeft := FCurLeftOffset - p^.FRect.Left
      else
        nSrcLeft := 0;

      if nMaxWidth - nLeft > bmp.Width then
        nSrcWidth := bmp.Width
      else
        nSrcWidth := nMaxWidth - nLeft;
      SrcR := Rect( nSrcLeft, 0, nSrcWidth, bmp.Height );

      nDestLeft := nLeft;
      nDestWidth := WidthOfRect( SrcR );
      DestR := Rect( nDestLeft, p^.FRect.Top, nDestLeft + nDestWidth, p^.FRect.Bottom );

      DrawRectangle( bmp, ABufBmp.Canvas, SrcR, DestR, dsPaste, IsTransColor, TransColor );

      Inc( nLeft, nDestWidth + FTabSpace );
    end;
  finally
    bmp.Free;
  end;
end;

procedure TxdTabSet.DrawTabHeader(ABmp: TBitmap; const AIndex: Integer; const ApTabInfo: PTabSetInfo);
var
  R: TRect;
  pt: TPoint;
  SrcR: TRect;
  h: Integer;
  state: TxdComponentState;
begin
  SrcR := Rect(0, 0, ABmp.Width, ABmp.Height);
  if not FTabHeaderBmpInof.Bitmap.Empty then
  begin
    ABmp.Canvas.Brush.Color := TransColor;
    ABmp.Canvas.FillRect( SrcR );

    h := FTabHeaderBmpInof.Bitmap.Height div FTabHeaderBmpInof.BitmapCount;
    R := Rect( 0, 0, FTabHeaderBmpInof.Bitmap.Width, h );
    if ApTabInfo^.FSelected then
    begin
      if FTabHeaderBmpInof.BitmapCount >= 3 then
        OffsetRect( R, 0, h * 2 )
      else if FTabHeaderBmpInof.BitmapCount >= 2 then
        OffsetRect( R, 0, h );
    end
    else if AIndex = CurMouseOnIndex then
    begin
       if FTabHeaderBmpInof.BitmapCount >= 2 then
        OffsetRect( R, 0, h );
    end;
    DrawRectangle( FTabHeaderBmpInof.Bitmap, ABmp.Canvas, R, SrcR, FTabHeaderBmpInof.BitmapDrawStyle, IsTransColor, TransColor );
  end
  else
  begin
    if ApTabInfo^.FSelected then
      ABmp.Canvas.Brush.Color := RGB(255, 0, 0)
    else if AIndex = CurMouseOnIndex then
      ABmp.Canvas.Brush.Color := RGB(12, 123, 31)
    else
      ABmp.Canvas.Brush.Color := RGB(255, 255, 0);
    ABmp.Canvas.FillRect( SrcR );
  end;

  //绘制文字
  if ApTabInfo^.FCaption <> '' then
  begin
    ABmp.Canvas.Font := Font;
    ABmp.Canvas.Brush.Style := bsClear;
    DrawText(ABmp.Canvas.Handle, PChar(ApTabInfo^.FCaption), Length(ApTabInfo^.FCaption),
      SrcR, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

  //绘制关闭按键
  if not FTabDelBmpInfo.Bitmap.Empty then
  begin
    R := SrcR;
    R.Top := R.Top + FTabDelTopSpace;
    R.Left := R.Right - FDelBmpWidth - FTabDelRightSpace;
    R.Right := R.Left + FDelBmpWidth;
    R.Bottom := R.Top + FDelBmpHeight;

    SrcR := Rect( 0, 0, FTabDelBmpInfo.Bitmap.Width, FDelBmpHeight );

    state := GetCurControlState;
    if (FCurMouseOnIndex = AIndex) then
    begin
      GetCursorPos( pt );
      pt := ScreenToClient( pt );
      if IsMouseInTabClose(AIndex, pt.X, pt.Y) then
      begin
        if state = csDown then
        begin
          if FTabDelBmpInfo.BitmapCount in [3, 2] then
            OffsetRect( SrcR, 0, FDelBmpHeight * (FTabDelBmpInfo.BitmapCount - 1) );
        end
        else 
        begin
          if FTabDelBmpInfo.BitmapCount = 3 then
            OffsetRect( SrcR, 0, FDelBmpHeight * (FTabDelBmpInfo.BitmapCount - 2) )
          else if FTabDelBmpInfo.BitmapCount = 2 then
            OffsetRect( SrcR, 0, FDelBmpHeight * (FTabDelBmpInfo.BitmapCount - 1) );
        end;
      end;
    end;
    DrawRectangle( FTabDelBmpInfo.Bitmap, ABmp.Canvas, SrcR, R, FTabDelBmpInfo.BitmapDrawStyle, IsTransColor, TransColor );
  end;
end;

function TxdTabSet.FindTabHeaderIndex(const ATag: Integer): Integer;
var
  i: Integer;
  p: PTabSetInfo;
begin
  Result := -1;
  for i := 0 to FTabSetInfoList.Count - 1 do
  begin
    p := FTabSetInfoList[i];
    if p^.FTag = ATag then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TxdTabSet.FindTabHeaderInfo(const ATag: Integer): PTabSetInfo;
var
  i: Integer;
  p: PTabSetInfo;
begin
  Result := nil;
  for i := 0 to FTabSetInfoList.Count - 1 do
  begin
    p := FTabSetInfoList[i];
    if p^.FTag = ATag then
    begin
      Result := p;
      Break;
    end;
  end;
end;

function TxdTabSet.GetCurTabWidth: Integer;
begin
  //外部调用要确保 FTabSetInfoList.Count > 0; 不然报错 
  Result := Width - FTabSetLeftSpace - FTabSetRightSpace;
  Result := (Result + FTabSpace) div FTabSetInfoList.Count - FTabSpace;
  if Result < FTabMinWidth then
    Result := FTabMinWidth
  else if Result > FTabMaxWidth then
    Result := FTabMaxWidth;
end;

function TxdTabSet.GetTabCount: Integer;
begin
  Result := FTabSetInfoList.Count;
end;

function TxdTabSet.GetTabHeaderInfo(const AIndex: Integer): PTabSetInfo;
begin
  if (AIndex >= 0) and (AIndex < FTabSetInfoList.Count) then
    Result := FTabSetInfoList[AIndex]
  else
    Result := nil;
end;

function TxdTabSet.GetTabSelected(AIndex: Integer): Boolean;
var
  p: PTabSetInfo;
begin
  Result := False;
  if (AIndex >= 0) and (AIndex < FTabSetInfoList.Count) then
  begin
    p := FTabSetInfoList[AIndex];
    Result := p^.FSelected;
  end;
end;

procedure TxdTabSet.InvalidateTab(const AIndex: Integer);
var
  p: PTabSetInfo;
  R: TRect;
begin
  if (AIndex < 0) or (AIndex >= FTabSetInfoList.Count) then Exit;
  p := FTabSetInfoList[AIndex];
  R := p^.FRect;
  OffsetRect( R, -FCurLeftOffset + FTabSetLeftSpace, 0 );
  InvalidateGraphicRect( @R );
end;

procedure TxdTabSet.InvalidateTabClose(const AIndex: Integer);
var
  p: PTabSetInfo;
  R: TRect;
begin
  if (FDelBmpWidth > 0) and (FDelBmpHeight > 0) then
  begin
    p := FTabSetInfoList[AIndex];
    R := p^.FRect;
    OffsetRect( R, -FCurLeftOffset + FTabSetLeftSpace, 0 );
    R.Top := R.Top + FTabDelTopSpace;
    R.Left := R.Right - FDelBmpWidth - FTabDelRightSpace;
    R.Right := R.Left + FDelBmpWidth;
    R.Bottom := R.Top + FDelBmpHeight;
    InvalidateGraphicRect( @R );
  end;
end;

function TxdTabSet.IsExsitsTabHeader(const ATag: Integer): Boolean;
var
  i: Integer;
  p: PTabSetInfo;
begin
  Result := False;
  for i := 0 to FTabSetInfoList.Count - 1 do
  begin
    p := FTabSetInfoList[i];
    if p^.FTag = ATag then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TxdTabSet.IsMouseInTabClose(const AIndex, x, y: Integer): Boolean;
var
  p: PTabSetInfo;
  R: TRect;
begin
  Result := False;
  if (FDelBmpWidth > 0) and (FDelBmpHeight > 0) then
  begin
    p := FTabSetInfoList[AIndex];
    R := p^.FRect;
    OffsetRect( R, -FCurLeftOffset + FTabSetLeftSpace, 0 );
    R.Top := R.Top + FTabDelTopSpace;
    R.Left := R.Right - FDelBmpWidth - FTabDelRightSpace;
    R.Right := R.Left + FDelBmpWidth;
    R.Bottom := R.Top + FDelBmpHeight;
    Result := PtInRect( R, Point(x, y) );
  end;
end;

function TxdTabSet.MouseToTabSet(const x, y: Integer): Integer;
var
  i, nLeft: Integer;
  p: PTabSetInfo;
  R: TRect;
begin
  Result := -1;
  if FTabSetInfoList.Count = 0 then Exit;
  if (FCurDrawFirstIndex < 0) or (FCurDrawFirstIndex >= FTabSetInfoList.Count) then Exit;
  R := ClientRect;
  R.Left := R.Left + FTabSetLeftSpace;
  R.Top := R.Top + FTabSetTopSpace;
  R.Right := R.Right - FTabSetRightSpace;
  R.Bottom := R.Bottom - FTabSetBottomSpace;

  if PtInRect(R, Point(x, y)) then
  begin
    p := FTabSetInfoList[0];
    nLeft := FCurLeftOffset + x;
    i := nLeft div ( WidthOfRect(p^.FRect) + FTabSpace );
    if (i >= 0) and (i < FTabSetInfoList.Count) then
    begin
      p := FTabSetInfoList[i];
      R := p^.FRect;
      OffsetRect( R, -FCurLeftOffset, 0 );
      if PtInRect( R, Point(x, y) ) then
        Result := i;
    end;
  end;
end;

procedure TxdTabSet.DoDrawInfoChanged(Sender: TObject);
begin
  Invalidate;
end;
procedure TxdTabSet.DoTimerMove(Sender: TObject);
var
  n: Integer;
begin
  //移动直到：CurLeftOffset = CurMoveSize
  if CurLeftOffset - FCurMoveSize = 0 then
  begin
    FreeAndNil( FMoveTime );
    Exit;
  end;
  if FCurMoveSize > CurLeftOffset then
  begin
    n := FTabMoveSpeed;
    if CurLeftOffset + n > FCurMoveSize then
      n := FCurMoveSize - CurLeftOffset;
  end
  else
  begin
    n := -FTabMoveSpeed;
    if CurLeftOffset - n < FCurMoveSize then
      n := CurLeftOffset - FCurMoveSize;
  end;
  CurLeftOffset := CurLeftOffset + n;
  Invalidate;
end;

procedure TxdTabSet.ReCalcTabSetPosition;
var
  i, nWidth, nH, nSelectedIndex, nSize: Integer;
  p: PTabSetInfo;
  SelectedR, R: TRect;
begin
  if FTabSetInfoList.Count = 0 then Exit;
  nWidth := GetCurTabWidth;
  nH := Height;
  nSize := nWidth + FTabSpace;
  nSelectedIndex := 0;//GetCurSelectedTabIndex;
  //优先选中Tab
  p := FTabSetInfoList[ nSelectedIndex ];
  p^.FRect.Right := p^.FRect.Left + nWidth;
  p^.FRect.Top := FTabSetTopSpace;
  p^.FRect.Bottom := nH - FTabSetBottomSpace;
  SelectedR := p^.FRect;

  //设置选中Tab左边其它Tab
  R := SelectedR;
  for i := nSelectedIndex - 1 downto 0 do
  begin
    OffsetRect( R, -nSize, 0 );
    p := FTabSetInfoList[i];
    p^.FRect := R;
  end;

  //设置选中Tab右边其它Tab
  R := SelectedR;
  for i := nSelectedIndex + 1 to FTabSetInfoList.Count - 1 do
  begin
    OffsetRect( R, nSize, 0 );
    p := FTabSetInfoList[i];
    p^.FRect := R;
  end;
end;

procedure TxdTabSet.Resize;
begin
  ReCalcTabSetPosition;
  inherited;
  Invalidate;
end;

procedure TxdTabSet.SetTabSelected(AIndex: Integer; const Value: Boolean);
var
  i, nOldSelIndex: Integer;
  p: PTabSetInfo;
  bMove: Boolean;
begin
  if (AIndex >= 0) and (AIndex < FTabSetInfoList.Count) then
  begin
    p := FTabSetInfoList[ AIndex ];
    if p^.FSelected then Exit;

    nOldSelIndex := -1;
    for i := 0 to FTabSetInfoList.Count - 1 do
    begin
      p := FTabSetInfoList[i];
      if p^.FSelected then
        nOldSelIndex := i;
      p^.FSelected := False;
    end;

    p := FTabSetInfoList[ AIndex ];
    p^.FSelected := True;
    if Assigned(OnTabSelected) then
      OnTabSelected( Self, p );
    if p^.FRect.Right - FCurLeftOffset > Width - FTabSetRightSpace then
    begin
      FCurMoveSize := p^.FRect.Right - Width + FTabSetRightSpace + FTabSetLeftSpace;
      bMove := True;
    end
    else if p^.FRect.Left < FCurLeftOffset then
    begin
      FCurMoveSize := p^.FRect.Left;
      bMove := True;
    end
    else
      bMove := False;

    if bMove then
    begin
      if not Assigned(FMoveTime) then
        FMoveTime := TTimer.Create( nil );
      with FMoveTime do
      begin
        Interval := 100;;
        OnTimer := DoTimerMove;
        Enabled := True;
      end;
    end
    else
    begin
      if nOldSelIndex <> -1 then
        InvalidateTab( nOldSelIndex );
      InvalidateTab( AIndex );
    end;
  end;
end;

procedure TxdTabSet.SetTabSetBackBmpInfo(const Value: TBitmapInfo);
begin
  FTabSetBackBmpInfo.Assign( Value );
end;

procedure TxdTabSet.SetTabSetBackGradient(const Value: TGradientDrawInfo);
begin
  FTabSetBackGradient.Assign( Value );
end;

procedure TxdTabSet.SetTabSetBottomSpace(const Value: Integer);
begin
  if (Value > 0) and (FTabSetBottomSpace <> Value) then
  begin
    FTabSetBottomSpace := Value;
    ReCalcTabSetPosition;
  end;
end;

procedure TxdTabSet.SetTabSetLeftSpace(const Value: Integer);
begin
  if (Value > 0) and (FTabSetLeftSpace <> Value) then
  begin
    FTabSetLeftSpace := Value;
    ReCalcTabSetPosition;
  end;
end;

procedure TxdTabSet.SetCurLeftOffset(const Value: Integer);
var
  nPerWidth: Integer;
  p: PTabSetInfo;
begin
//  if FCurLeftOffset <> Value then
  begin
    FCurLeftOffset := Value;
    FCurDrawFirstIndex := -1;
    if FTabSetInfoList.Count = 0 then Exit;
    p := FTabSetInfoList[0];
    nPerWidth := WidthOfRect( p^.FRect );
    if nPerWidth >= FCurLeftOffset then
      FCurDrawFirstIndex := 0
    else
      FCurDrawFirstIndex := (FCurLeftOffset - nPerWidth) div (nPerWidth + FTabSpace) + 1;
  end;
end;

procedure TxdTabSet.SetCurMouseOnIndex(const Value: Integer);
var
  nTemp: Integer;
begin
  if FCurMouseOnIndex <> Value then
  begin
    nTemp := FCurMouseOnIndex;
    FCurMouseOnIndex := Value;
    if nTemp <> -1 then
      InvalidateTab( nTemp );
    if FCurMouseOnIndex <> -1 then
      InvalidateTab( FCurMouseOnIndex );
  end;
end;

procedure TxdTabSet.SetTabDelBmpInfo(const Value: TBitmapInfo);
begin
  FTabDelBmpInfo.Assign( Value );
end;

procedure TxdTabSet.SetTabDelRightSpace(const Value: Integer);
begin
  if FTabDelRightSpace <> Value then
  begin
    FTabDelRightSpace := Value;
    Invalidate;
  end;
end;

procedure TxdTabSet.SetTabDelTopSpace(const Value: Integer);
begin
  if FTabDelTopSpace <> Value then
  begin
    FTabDelTopSpace := Value;
    Invalidate;
  end;
end;

procedure TxdTabSet.SetTabHeaderBmpInof(const Value: TBitmapInfo);
begin
  FTabHeaderBmpInof.Assign( Value );
  Invalidate;
end;

procedure TxdTabSet.SetTabHeaderSelected(const ATag: Integer);
var
  i: Integer;
begin
  i := FindTabHeaderIndex( ATag );
  TabSelected[i] := True;
end;

procedure TxdTabSet.SetTabMaxWidth(const Value: Integer);
begin
  if (FTabMaxWidth <> Value) and (Value > FTabMinWidth) then
  begin
    FTabMaxWidth := Value;
    ReCalcTabSetPosition;
  end;
end;

procedure TxdTabSet.SetTabMinWidth(const Value: Integer);
begin
  if (FTabMinWidth <> Value) and (Value < FTabMaxWidth) then
  begin
    FTabMinWidth := Value;
    ReCalcTabSetPosition;
  end;
end;

procedure TxdTabSet.SetTabMoveSpeed(const Value: Integer);
begin
  FTabMoveSpeed := Value;
end;

procedure TxdTabSet.SetTabSetRightSpace(const Value: Integer);
begin
  if (Value > 0) and (FTabSetRightSpace <> Value) then
  begin
    FTabSetRightSpace := Value;
    ReCalcTabSetPosition;
  end;
end;

procedure TxdTabSet.SetTabSpace(const Value: Integer);
begin
  if (Value > 0) and (FTabSpace <> Value) then
  begin
    FTabSpace := Value;
    ReCalcTabSetPosition;
  end;
end;

procedure TxdTabSet.UpdateTabHeaderCaption(const AIndex: Integer; const ACaption: string);
var
  p: PTabSetInfo;
begin
  p := GetTabHeaderInfo( AIndex );
  if CompareText(p^.FCaption, ACaption) <> 0 then
  begin
    p^.FCaption := ACaption;
    InvalidateTab( AIndex );
  end;
end;

procedure TxdTabSet.WMMouseMove(var Message: TWMMouseMove);
var
  nIndex: Integer;
begin
  nIndex := MouseToTabSet( Message.XPos, Message.YPos );

  if (nIndex >= 0) and (nIndex < FTabSetInfoList.Count) then
    InvalidateTabClose( nIndex );
  CurMouseOnIndex := nIndex;

  inherited;
end;

procedure TxdTabSet.SetTabSetTopSpace(const Value: Integer);
begin
  if (Value > 0) and (FTabSetTopSpace <> Value) then
  begin
    FTabSetTopSpace := Value;
    ReCalcTabSetPosition;
  end;
end;

end.
