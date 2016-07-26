{
渐变色TabSet
作者: Terry(江晓德)
QQ:   67068633
Email:jxd524@163.com
2010-5-8 早上11点创建
}
unit uJxdGradientTabSet;

interface
uses
  uJxdGradientPanel, Classes, Windows, Controls, Graphics, Messages, ExtCtrls, uBitmapHandle;

type
  PTabSetInfo = ^TTabSetInfo;
  TTabSetInfo = record
    FTitle: string;
    FData: Pointer;
    FPos: TRect;
  end;
  TOnDeleteTabSet = procedure(Sender: TObject; const ADelIndex: Integer) of object;
  TJxdGradientTabSet = class( TJxdGradientPanel )
  private
    FTabSetList: TList;
    FLeftSpace, FRightSpace, FTabSetSpace: Integer;
    FTabSetHeight, FCurTabSetWidth, FCurSelIndex, FOldSelIndex: Integer;
    FMoveTime, FDeleteTime: TTimer;
    FDelMoveIndex: Integer;
    FDelRect: TRect;
    FIsMoveTabSet, FIsCanDelTabSet: Boolean;
    FMoveTabSetIndex: Integer;
    FMoveSpeed: Integer;
    FOldMouseInTabSet: Integer;
    FTabSetBitmap: TBitmap;
    FFontMargins: TRect;
    FOnTabSetChanged: TNotifyEvent;
    FOnDeleteTabSet: TOnDeleteTabSet;
    FOnTabSetBeforeChanged: TNotifyEvent;
    FTabSetCloseBitmap: TBitmap;
    FTabSetCloseBmpWidth, FTabSetCloseBmpHeight: Integer;
    FTabSetCloseTopSpace, FTabSetCloseRightSpace: Integer;
    FCurTabSetCloseRIndex: Integer;

    function  FormatTitle(const ATitle: WideString): string;
    procedure CreateMoveTime;
    function  CheckAllTabSetWidth: Boolean;
    function  MouseToTabSet(pt: TPoint): Integer;
    function  MouseToTabSetClosePosIndex(pt: TPoint): Integer;
    function  TabSetRectToCloseRect(const ARect: TRect): TRect;
    procedure DoMoveTabSetEvent(Sender: TObject);
    procedure DoDelTabSetEvent(Sender: TObject);
    procedure SetCurSelTabSetIndex(const Value: Integer);
    procedure SetLeftSpace(const Value: Integer);
    procedure SetRightSpace(const Value: Integer);
    procedure SetMoveSpeed(const Value: Integer);
    procedure SetTabSetBmp(const Value: TBitmap);
    function  GetTabSetCount: Integer;
    procedure SetFontMargins(const Value: TRect);
    procedure SetOnTabSetChanged(const Value: TNotifyEvent);
    procedure DoTabSetChanged;
    procedure DoTabSetBeforeChanged;
    procedure DoDeleteTabSet(const AIndex: Integer);
    procedure SetOnTabSetBeforeChanged(const Value: TNotifyEvent);
    procedure SetTabSetCloseBitmap(const Value: TBitmap);
    procedure SetTabSetCloseRightSpace(const Value: Integer);
    procedure SetTabSetCloseTopSpace(const Value: Integer);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure DrawPanel(ABufBitmap: TBitmap); override;
    procedure DrawTabSet(p: PTabSetInfo; ABmp: TBitmap); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  AddTabSet(const ATitle: string; AData: Pointer = nil): Integer;
    function  UpdateTabSetData(const AIndex: Integer; AData: Pointer): Boolean;
    function  UpdateTabSetText(const AIndex: Integer; const ATitle: string): Boolean;
    function  GetTabSetData(const AIndex: Integer): Pointer;
    function  CanMoveTabSet(const ALeftToRight: Boolean): Integer;
    procedure MoveTabSet(const ALeftToRight: Boolean);
    procedure Delete(const AIndex: Integer);
  published
    property CurSelTabSetIndex: Integer read FCurSelIndex write SetCurSelTabSetIndex;
    property OldSelTabSetIndex: Integer read FOldSelIndex;
    property LeftSpace: Integer read FLeftSpace write SetLeftSpace;
    property RightSpace: Integer read FRightSpace write SetRightSpace;
    property MoveSpeed: Integer read FMoveSpeed write SetMoveSpeed;
    property TabSetBitmap: TBitmap read FTabSetBitmap write SetTabSetBmp;
    property TabSetCloseBitmap: TBitmap read FTabSetCloseBitmap write SetTabSetCloseBitmap;
    property TabSetCloseTopSpace: Integer read FTabSetCloseTopSpace write SetTabSetCloseTopSpace;
    property TabSetCloseRightSpace: Integer read FTabSetCloseRightSpace write SetTabSetCloseRightSpace;
    property CurTabSetCount: Integer read GetTabSetCount;
    property FontMargins: TRect read FFontMargins write SetFontMargins;
    property OnTabSetBeforeChanged: TNotifyEvent read FOnTabSetBeforeChanged write SetOnTabSetBeforeChanged; 
    property OnTabSetChanged: TNotifyEvent read FOnTabSetChanged write SetOnTabSetChanged;
    property OnDeleteTabSet: TOnDeleteTabSet read FOnDeleteTabSet write FOnDeleteTabSet;
  end;

implementation

const
  CtMaxTabSetWidth = 250;
  CtMinTabSetWidth = 100;

{ TJxdGradientTabSet }

function TJxdGradientTabSet.AddTabSet(const ATitle: string; AData: Pointer): Integer;
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
      Left := FLeftSpace;
      Right := Left + FCurTabSetWidth;
      Top := Self.Height - FTabSetHeight;
      Bottom := Top + FTabSetHeight;
    end;
  end;
  Result := FTabSetList.Add( p );
  CheckAllTabSetWidth;
  if FCurSelIndex = -1 then
  begin
    DoTabSetBeforeChanged;
    FCurSelIndex := Result;
    DoTabSetChanged;
  end;
  Invalidate;
end;

function TJxdGradientTabSet.CanMoveTabSet(const ALeftToRight: Boolean): Integer;
var
  i: Integer;
  p: PTabSetInfo;
  nW: Integer;
begin
  Result := -1;
  if ALeftToRight then
  begin
    for i := 0 to FTabSetList.Count - 1 do
    begin
      p := FTabSetList[i];
      if p^.FPos.Left >= FLeftSpace then
      begin
        Result := i - 1;
        Break;
      end;
    end;
  end
  else
  begin
    nW := Width - FRightSpace;
    for i := FTabSetList.Count - 1 downto 0 do
    begin
      p := FTabSetList[i];
      if p^.FPos.Right <= nW then
      begin
        Result := i + 1;
        break;
      end;
    end;
  end;
  if (Result < 0) or (Result >= FTabSetList.Count) then Result := -1;
end;

function TJxdGradientTabSet.CheckAllTabSetWidth: Boolean;
var
  nWidth, nUseWidth, i: Integer;
  p, pPre: PTabSetInfo;
begin
  Result := False;
  if FTabSetList.Count = 0 then Exit;
  nUseWidth := Width - FLeftSpace - FRightSpace;
  if FTabSetList.Count > 1 then
    nUseWidth := nUseWidth - (FTabSetList.Count - 1) * FTabSetSpace;

  nWidth := nUseWidth div FTabSetList.Count;

  if nWidth > CtMaxTabSetWidth then
  begin
    FCurTabSetWidth := CtMaxTabSetWidth;
    Result := False;
  end
  else if nWidth >= CtMinTabSetWidth then
  begin
    FCurTabSetWidth := nWidth;
    Result := False;
  end
  else
  begin
    FCurTabSetWidth := CtMinTabSetWidth;
    Result := True;
  end;

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

procedure TJxdGradientTabSet.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TJxdGradientTabSet.CMMouseLeave(var Message: TMessage);
var
  p: PTabSetInfo;
begin
  inherited;
  if FOldMouseInTabSet <> -1 then
  begin
    p := FTabSetList[FOldMouseInTabSet];
    FOldMouseInTabSet := -1;
    InvalidateRect( Handle, @p^.FPos, False );
  end;
  if FCurTabSetCloseRIndex <> -1 then
  begin
    p := FTabSetList[ FCurTabSetCloseRIndex ];
    FCurTabSetCloseRIndex := -1;
    InvalidateRect( Handle, @p^.FPos, False );
  end;
end;

constructor TJxdGradientTabSet.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered         := True;
  FTabSetList            := TList.Create;
  FLeftSpace             := 20;
  FRightSpace            := 40;
  FTabSetSpace           := 0;
  FTabSetHeight          := 29;
  FMoveSpeed             := 20;
  FCurTabSetWidth        := CtMaxTabSetWidth;
  FMoveTime              := nil;
  FIsMoveTabSet          := False;
  FCurSelIndex           := -1;
  FOldSelIndex           := -1;
  FOldMouseInTabSet      := -1;
  FTabSetBitmap          := TBitmap.Create;
  FFontMargins.Left      := 5;
  FFontMargins.Right     := 5;
  FFontMargins.Top       := 5;
  FFontMargins.Bottom    := 0;
  FIsCanDelTabSet        := False;
  FTabSetCloseBitmap     := TBitmap.Create;
  FTabSetCloseBmpWidth   := 0;
  FTabSetCloseBmpHeight  := 0;
  FTabSetCloseTopSpace   := 4;
  FTabSetCloseRightSpace := 4;
  FCurTabSetCloseRIndex  := -1;
end;

procedure TJxdGradientTabSet.CreateMoveTime;
begin
  if FMoveTime = nil then
  begin
    FMoveTime := TTimer.Create( Self );
    FMoveTime.OnTimer := DoMoveTabSetEvent;
    FMoveTime.Interval := 50;
  end;
end;

procedure TJxdGradientTabSet.Delete(const AIndex: Integer);
var
  p, pTemp: PTabSetInfo;
  i: Integer;
  R: TRect;
  bChangedSelIndex: Boolean;
begin
  if (AIndex < 0) or (AIndex >= FTabSetList.Count) then Exit;
  p := FTabSetList[AIndex];
  bChangedSelIndex := False;
  if AIndex = CurSelTabSetIndex then
  begin
    if 1 = FTabSetList.Count then
    begin
      DoTabSetBeforeChanged;
      FCurSelIndex := -1;
      DoDeleteTabSet( AIndex );
      R := p^.FPos;
      Dispose( p );
      FTabSetList.Delete( AIndex );
      InvalidateRect( Handle, @R, True );
      Exit;
    end
    else if AIndex + 1 >= FTabSetList.Count then
    begin
      DoTabSetBeforeChanged;
      FCurSelIndex := AIndex - 1;
    end;
    bChangedSelIndex := True;
  end
  else if AIndex < CurSelTabSetIndex then
  begin
    DoTabSetBeforeChanged;
    FCurSelIndex := FCurSelIndex - 1;
  end;

  if p^.FPos.Left + WidthOfRect(p^.FPos) < FLeftSpace then
  begin
    for i := 0 to AIndex - 1 do
    begin
      pTemp := FTabSetList[i];
      pTemp^.FPos.Left := pTemp^.FPos.Left + FCurTabSetWidth;
      pTemp^.FPos.Right := pTemp^.FPos.Left + FCurTabSetWidth;
    end;
    DoDeleteTabSet( AIndex );
    Dispose( p );
    FTabSetList.Delete( AIndex );
    CheckAllTabSetWidth;
  end
  else if p^.FPos.Left > Width - FRightSpace then
  begin
    for i := AIndex + 1 to FTabSetList.Count - 1 do
    begin
      pTemp := FTabSetList[i];
      pTemp^.FPos.Left := pTemp^.FPos.Left - FCurTabSetWidth;
      pTemp^.FPos.Right := pTemp^.FPos.Left - FCurTabSetWidth;
    end;
    DoDeleteTabSet( AIndex );
    Dispose( p );
    FTabSetList.Delete( AIndex );
    CheckAllTabSetWidth;
  end
  else if AIndex = FTabSetList.Count - 1 then
  begin
    DoDeleteTabSet( AIndex );
    Dispose( p );
    FTabSetList.Delete( AIndex );
    CheckAllTabSetWidth;
    bChangedSelIndex := True;
    Invalidate;
//    p := FTabSetList[0];
//    if p^.FPos.Left < FLeftSpace then
//      MoveTabSet( False )
//    else
//      Invalidate;
  end
  else
  begin
    DoDeleteTabSet( AIndex );
    FDelRect := p^.FPos;
    FDelMoveIndex := AIndex;
    Dispose( p );
    FTabSetList.Delete( AIndex );

    if FDelRect.Left < FLeftSpace then
      FDelRect.Left := FLeftSpace;
    FDeleteTime := TTimer.Create( Self );
    FDeleteTime.Interval := 10;
    FDeleteTime.OnTimer := DoDelTabSetEvent;
    FDeleteTime.Enabled := True;
  end;
  if bChangedSelIndex then DoTabSetChanged;
end;

destructor TJxdGradientTabSet.Destroy;
var
  i: Integer;
begin
  for i := 0 to FTabSetList.Count - 1 do
    Dispose( PTabSetInfo(FTabSetList[i]) );
  FTabSetList.Free;
  FTabSetBitmap.Free;
  inherited;
end;

procedure TJxdGradientTabSet.DoDeleteTabSet(const AIndex: Integer);
begin
  if Assigned(OnDeleteTabSet) then
    OnDeleteTabSet( Self, AIndex ); 
end;

procedure TJxdGradientTabSet.DoDelTabSetEvent(Sender: TObject);
var
  t: TTimer;
  i, n: Integer;
  p: PTabSetInfo;
  R: TRect;
begin
  if not (Sender is TTimer) then Exit;
  t := Sender as TTimer;
  t.Enabled := False;
  try
    p := FTabSetList[ FDelMoveIndex ];
    if p^.FPos.Left - FMoveSpeed >= FDelRect.Left then
      n := FMoveSpeed
    else
      n := p^.FPos.Left - FDelRect.Left;
    R := FDelRect;
    R.Right := Width - FRightSpace;
    for i := FDelMoveIndex to FTabSetList.Count - 1 do
    begin
      p := FTabSetList[i];
      Dec( p^.FPos.Left, n );
      Dec( p^.FPos.Right, n );
    end;
    InvalidateRect( Handle, @R, True );
  finally
    p := FTabSetList[ FDelMoveIndex ];
    t.Enabled := p^.FPos.Left <> FDelRect.Left;
    FIsCanDelTabSet := not t.Enabled;
    if FIsCanDelTabSet then
    begin
      t.Free;
      FDeleteTime := nil;
      CheckAllTabSetWidth;
      Invalidate;
    end;
  end;
end;

procedure TJxdGradientTabSet.DoMoveTabSetEvent(Sender: TObject);
var
  LeftToRight: Boolean;
  t: TTimer;
  i, n: Integer;
  p: PTabSetInfo;
begin
  if not (Sender is TTimer) then Exit;
  t := Sender as TTimer;
  t.Enabled := False;
  LeftToRight := t.Tag = 0;
  try
    if LeftToRight then
    begin
      p := FTabSetList[ FMoveTabSetIndex ];
      if p^.FPos.Left + FMoveSpeed <= FLeftSpace then
        n := FMoveSpeed
      else
        n := FLeftSpace - p^.FPos.Left;
      for i := 0 to FTabSetList.Count - 1 do
      begin
        p := FTabSetList[i];
        Inc( p^.FPos.Left, n );
        Inc( p^.FPos.Right, n );
      end;
    end
    else
    begin
      p := FTabSetList[ FMoveTabSetIndex ];
      if p^.FPos.Right - FMoveSpeed >= Width - FRightSpace then
        n := FMoveSpeed
      else
        n := p^.FPos.Right - Width + FRightSpace;
      for i := 0 to FTabSetList.Count - 1 do
      begin
        p := FTabSetList[i];
        Dec( p^.FPos.Left, n );
        Dec( p^.FPos.Right, n );
      end;
    end;
    Invalidate;
  finally
    p := FTabSetList[ FMoveTabSetIndex ];
    if LeftToRight then
    begin
      t.Enabled := p^.FPos.Left < FLeftSpace;
    end
    else
      t.Enabled := p^.FPos.Right > Width - FRightSpace;
    FIsMoveTabSet := t.Enabled;
    if not FIsMoveTabSet then
    begin
      t.Free;
      FMoveTime := nil;
    end;
  end;
end;

procedure TJxdGradientTabSet.DoTabSetBeforeChanged;
begin
  FOldSelIndex := FCurSelIndex;
  if Assigned(FOnTabSetBeforeChanged) then
    FOnTabSetBeforeChanged( Self );
end;

procedure TJxdGradientTabSet.DoTabSetChanged;
begin
  if FOldSelIndex >= FTabSetList.Count then
    FOldSelIndex := -1;
  if Assigned(FOnTabSetChanged) then
    FOnTabSetChanged( Self );
end;

procedure TJxdGradientTabSet.DrawPanel(ABufBitmap: TBitmap);
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
      p := FTabSetList[i];
      if (p^.FPos.Right < FLeftSpace) or (p^.FPos.Left > nRight) then Continue;
      DrawTabSet( p, Bmp );

      if p^.FPos.Left < FLeftSpace then
      begin
        xDest := FLeftSpace;
        xSrc := FLeftSpace - p^.FPos.Left;
        nW := p^.FPos.Right - p^.FPos.Left - (FLeftSpace - p^.FPos.Left);
      end
      else
      begin
        xDest := p^.FPos.Left;
        xSrc := 0;
        nW := FCurTabSetWidth;
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

procedure TJxdGradientTabSet.DrawTabSet(p: PTabSetInfo; ABmp: TBitmap);
var
  BmpR, BmpCloseR, R, CloseR: TRect;
  strTitle: string;
begin
  R := Rect( 0, 0, ABmp.Width, ABmp.Height );
  CloseR := TabSetRectToCloseRect( R );
  if not FTabSetBitmap.Empty then
  begin
    if p = FTabSetList[ FCurSelIndex ] then
    begin
      BmpR := Rect(0, FTabSetHeight * 2, FTabSetBitmap.Width, FTabSetHeight * 3 );
      if not FTabSetCloseBitmap.Empty then
        BmpCloseR := Rect(0, FTabSetCloseBmpHeight * 2, FTabSetCloseBmpWidth, FTabSetCloseBmpHeight * 3 );
    end
    else if (FOldMouseInTabSet <> -1) and (FTabSetList[FOldMouseInTabSet] = p) then
    begin
      BmpR := Rect(0, FTabSetHeight, FTabSetBitmap.Width, FTabSetHeight * 2 );
      if not FTabSetCloseBitmap.Empty then
        BmpCloseR := Rect(0, FTabSetCloseBmpHeight, FTabSetCloseBmpWidth, FTabSetCloseBmpHeight * 2 );
    end
    else
    begin
      BmpR := Rect(0, 0, FTabSetBitmap.Width, FTabSetHeight );
      if not FTabSetCloseBitmap.Empty then
        BmpCloseR := Rect(0, 0, FTabSetCloseBmpWidth, FTabSetCloseBmpHeight );
    end;

    DrawRectangle( ABmp.Canvas, BmpR, R, FTabSetBitmap );
  end
  else
  if p = FTabSetList[ FCurSelIndex ] then
  begin
    ABmp.Canvas.Brush.Color := $2D4FFF;
    ABmp.Canvas.FillRect( Rect(0, 0, ABmp.Width, Abmp.Height) );
  end
  else
  begin
    ABmp.Canvas.Brush.Color := Random( GetTickCount );
    ABmp.Canvas.FillRect( Rect(0, 0, ABmp.Width, Abmp.Height) );
  end;

  if p^.FTitle <> '' then
  begin
    Inc( R.Left, FFontMargins.Left );
    Inc( R.Top, FFontMargins.Top );
    Dec( R.Right, FFontMargins.Right );
    ABmp.Canvas.Font := Font;
    ABmp.Canvas.Brush.Style := bsClear;
    strTitle := FormatTitle( p^.FTitle );
    DrawText(ABmp.Canvas.Handle, PChar(strTitle), Length(strTitle), R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
  if not FTabSetCloseBitmap.Empty then
  begin
    ABmp.Canvas.Brush.Style := bsClear;
    SetBkMode( ABmp.Canvas.Handle, TRANSPARENT );
    ABmp.Canvas.BrushCopy( CloseR, FTabSetCloseBitmap, BmpCloseR, clFuchsia );
  end;
end;

function TJxdGradientTabSet.FormatTitle(const ATitle: WideString): string;
var
  nW, nFontLen, n: Integer;
begin
  nW := FCurTabSetWidth - FFontMargins.Left - FFontMargins.Right;
  nFontLen := Canvas.TextWidth( ATitle );
  if nFontLen <= nW then
    Result := ATitle
  else
  begin
    Result := Copy( ATitle, 1, Length(ATitle) - 4 ) + '...';
    nFontLen := Canvas.TextWidth( Result );
    n := 4;
    while nFontLen > nW do
    begin
      Inc( n, 2 );
      Result := Copy( ATitle, 1, Length(ATitle) - n ) + '...';
      nFontLen := Canvas.TextWidth( Result );
    end;
  end;
end;

function TJxdGradientTabSet.GetTabSetCount: Integer;
begin
  if Assigned(FTabSetList) then
    Result := FTabSetList.Count
  else
    Result := 0;
end;

function TJxdGradientTabSet.GetTabSetData(const AIndex: Integer): Pointer;
begin
  if (AIndex < 0) or (AIndex >= FTabSetList.Count) then
    Result := nil
  else
    Result :=  PTabSetInfo(FTabSetList[AIndex])^.FData;
end;

procedure TJxdGradientTabSet.Loaded;
begin
  inherited;
  if Assigned(FTabSetBitmap) and (not FTabSetBitmap.Empty) then  
    FTabSetHeight := FTabSetBitmap.Height div 3;
  if Assigned(FTabSetCloseBitmap) and (not FTabSetCloseBitmap.Empty) then
  begin
    FTabSetCloseBmpWidth := FTabSetCloseBitmap.Width;
    FTabSetCloseBmpHeight := FTabSetCloseBitmap.Height div 3;
  end;
  AutoMoveForm := False;
end;

procedure TJxdGradientTabSet.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TJxdGradientTabSet.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  n: Integer;
  p: PTabSetInfo;
begin
  inherited;
  n := MouseToTabSet( Point(x, y) );
  if n <> -1  then
  begin
    if n <> FOldMouseInTabSet then
    begin
      if FOldMouseInTabSet <> -1 then
      begin
        if FCurSelIndex = FOldMouseInTabSet then
        begin
          FOldMouseInTabSet := n;
          p := FTabSetList[FOldMouseInTabSet];
          InvalidateRect( Handle, @p^.FPos, False );
        end
        else if FCurSelIndex = n then
        begin
          p := FTabSetList[FOldMouseInTabSet];
          InvalidateRect( Handle, @p^.FPos, False );
          FOldMouseInTabSet := n;
        end
        else
        begin
          p := FTabSetList[FOldMouseInTabSet];
          FOldMouseInTabSet := n;
          InvalidateRect( Handle, @p^.FPos, False );
          p := FTabSetList[FOldMouseInTabSet];
          InvalidateRect( Handle, @p^.FPos, False );
        end;
      end
      else
      begin
        if FCurSelIndex <> n then
        begin
          FOldMouseInTabSet := n;
          p := FTabSetList[FOldMouseInTabSet];
          InvalidateRect( Handle, @p^.FPos, False );
        end;
      end;
    end;
  end
  else if FOldMouseInTabSet <> -1 then
  begin
    p := FTabSetList[FOldMouseInTabSet];
    FOldMouseInTabSet := -1;
    InvalidateRect( Handle, @p^.FPos, False );
  end;

end;

function TJxdGradientTabSet.MouseToTabSet(pt: TPoint): Integer;
var
  i: Integer;
  p: PTabSetInfo;
begin
  Result := -1;
  for i := 0 to FTabSetList.Count - 1 do
  begin
    p := FTabSetList[i];
    if PtInRect(p^.FPos, pt) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TJxdGradientTabSet.MouseToTabSetClosePosIndex(pt: TPoint): Integer;
var
  i: Integer;
  p: PTabSetInfo;
  R: TRect;
begin
  Result := -1;
  if (FTabSetCloseBmpWidth = 0) or (FTabSetCloseBmpHeight = 0) then Exit;

  for i := 0 to FTabSetList.Count - 1 do
  begin
    p := FTabSetList[i];
    R := TabSetRectToCloseRect( p^.FPos );
    if PtInRect(R, pt) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TJxdGradientTabSet.WMLButtonUp(var Message: TWMLButtonUp);
var
  n: Integer;
  p: PTabSetInfo;
begin
  inherited;
  n := MouseToTabSet( Point(Message.XPos, Message.YPos) );
  if n <> -1 then
  begin
    if FCurSelIndex <> n then
    begin
      p := FTabSetList[ FCurSelIndex ];
      InvalidateRect( Handle, @p^.FPos, False );
      CurSelTabSetIndex := n;
    end;
  end;
end;

procedure TJxdGradientTabSet.MoveTabSet(const ALeftToRight: Boolean);
var
  n: Integer;
begin
  if (FCurTabSetWidth > CtMinTabSetWidth) or FIsMoveTabSet then Exit;
  n := CanMoveTabSet( ALeftToRight );
  if n = -1 then Exit;
  FMoveTabSetIndex := n;
  FIsMoveTabSet := True;
  CreateMoveTime;
  if ALeftToRight then
    FMoveTime.Tag := 0
  else
    FMoveTime.Tag := 1;
  FMoveTime.Enabled := True;
end;

procedure TJxdGradientTabSet.SetCurSelTabSetIndex(const Value: Integer);
var
  p: PTabSetInfo;
begin
  if (Value < 0) or (Value >= FTabSetList.Count) or FIsMoveTabSet then Exit;
  if FCurSelIndex <> Value then
  begin
    p := FTabSetList[Value];
    if (p^.FPos.Left < FLeftSpace) or (p^.FPos.Right > Width - FRightSpace) then
    begin
      FMoveTabSetIndex := Value;
      CreateMoveTime;
      if Value < FCurSelIndex then
        FMoveTime.Tag := 0
      else
        FMoveTime.Tag := 1;
      DoTabSetBeforeChanged;
      FCurSelIndex := Value;
      DoTabSetChanged;
      FMoveTime.Enabled := True;
    end
    else
    begin
      DoTabSetBeforeChanged;
      p := FTabSetList[FCurSelIndex];
      FCurSelIndex := Value;
      InvalidateRect( Handle, @p^.FPos, False );
      p := FTabSetList[FCurSelIndex];
      InvalidateRect( Handle, @p^.FPos, False );
      DoTabSetChanged;
    end;
  end;
end;

procedure TJxdGradientTabSet.SetFontMargins(const Value: TRect);
begin
  FFontMargins := Value;
  Invalidate;
end;

procedure TJxdGradientTabSet.SetLeftSpace(const Value: Integer);
begin
  if (FLeftSpace <> Value) and (Value >= 0) then
  begin
    FLeftSpace := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientTabSet.SetMoveSpeed(const Value: Integer);
begin
  if (FMoveSpeed <> Value) and (Value > 0) then
    FMoveSpeed := Value;
end;

procedure TJxdGradientTabSet.SetOnTabSetBeforeChanged(const Value: TNotifyEvent);
begin
  FOnTabSetBeforeChanged := Value;
end;

procedure TJxdGradientTabSet.SetOnTabSetChanged(const Value: TNotifyEvent);
begin
  FOnTabSetChanged := Value;
end;

procedure TJxdGradientTabSet.SetRightSpace(const Value: Integer);
begin
  if (FRightSpace <> Value) and (Value >= 0) then
  begin
    FRightSpace := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientTabSet.SetTabSetBmp(const Value: TBitmap);
begin
  FTabSetBitmap.Assign( Value );
  Invalidate;
end;

procedure TJxdGradientTabSet.SetTabSetCloseBitmap(const Value: TBitmap);
begin
  FTabSetCloseBitmap.Assign( Value );
  Invalidate;
end;

procedure TJxdGradientTabSet.SetTabSetCloseRightSpace(const Value: Integer);
begin
  if FTabSetCloseRightSpace <> Value then
  begin
    FTabSetCloseRightSpace := Value;
    Invalidate;
  end;
end;

procedure TJxdGradientTabSet.SetTabSetCloseTopSpace(const Value: Integer);
begin
  if FTabSetCloseTopSpace <> Value then
  begin
    FTabSetCloseTopSpace := Value;
    Invalidate;
  end;
end;

function TJxdGradientTabSet.TabSetRectToCloseRect(const ARect: TRect): TRect;
begin
  Result := ARect;
  Result.Left := Result.Right - FTabSetCloseBmpWidth - FTabSetCloseRightSpace;
  Result.Top := Result.Top + FTabSetCloseTopSpace;
  Result.Right := Result.Left + FTabSetCloseBmpWidth;
  Result.Bottom := Result.Top + FTabSetCloseBmpHeight;
end;

function TJxdGradientTabSet.UpdateTabSetData(const AIndex: Integer; AData: Pointer): Boolean;
begin
  if (AIndex < 0) or (AIndex >= FTabSetList.Count) then
    Result := False
  else
  begin
    PTabSetInfo(FTabSetList[AIndex])^.FData := AData;
    Result := True;
  end;
end;

function TJxdGradientTabSet.UpdateTabSetText(const AIndex: Integer; const ATitle: string): Boolean;
begin
  if (AIndex < 0) or (AIndex >= FTabSetList.Count) then
    Result := False
  else
  begin
    PTabSetInfo(FTabSetList[AIndex])^.FTitle := ATitle;
    InvalidateRect( Handle, @PTabSetInfo(FTabSetList[AIndex])^.FPos, True );
    Result := True;
  end;
end;

procedure TJxdGradientTabSet.WMSize(var Message: TWMSize);
begin
  inherited;
  CheckAllTabSetWidth;
end;

end.
