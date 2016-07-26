unit uJxdHintList;

interface
uses
  Classes, Windows, Messages, Controls, Graphics, SysUtils, uJxdGradientTabSet, uBitmapHandle, uJxdCustomButton;

type
  PHintListInfo = ^THIntListInfo;
  THintListInfo = record
    FCaption: string;
    FInfo: string;
    FTag: Integer;
  end;
{$M+}
  TJxdHintList = class(TCustomControl)
  public
    procedure AddHintInfo(const ACaption, AInfo: string; const ATag: Integer);
    procedure DeleteHintInfo(const AIndex: Integer);

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure DrawHintList(ABmp: TBitmap); virtual;
    procedure DrawRow(ABmp: TBitmap; pInfo: PHintListInfo; const AIndex: Integer); virtual;
    function  DoDrawIcon(ABmp: TBitmap; const AIndex: Integer): Boolean;
    function  GetHintRowRect(const AIndex: Integer; var ARect: TRect): Boolean;
    procedure InvalidateRow(const AIndex: Integer; const AIsOnlyRedrawCloseRect: Boolean);

    function  CalcPos(const Apt: TPoint; var AIndex: Integer; var AInCloseBtn: Boolean): Boolean;
    function  GetCloseRect(const AIndex: Integer): TRect;
    function  FormatTitle(const ACanvas: TCanvas; const ATitle: WideString): string;
    procedure DoShowScrollBar;

    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDown;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
  private
    FHintList: TList;
    FHintRowHeight: Integer;
    FCurTopPos: Integer;
    FCloseBtnWidth: Integer;
    FCloseBtnHeight: Integer;
    FMouseMoveInfo: TMousePosInfo;  //当前鼠标移动位置信息
    FCloseBmpWidth, FCloseBmpHeight: Integer;
    procedure InitFont;
    procedure ReSetOwnerSize;
  private
    FCaptionFont: TFont;
    FInfoFont: TFont;
    FLineColor: TColor;
    FCloseBtnBmp: TBitmap;
    FMaxShowRowCount: Integer;
    FOnDrawIcon: TOnDrawIcon;
    FIconHeight: Integer;
    FIconWidth: Integer;
    FRowSpace: TRect;
    FRowBackBmp: TBitmap;
    FIconSpace: TRect;
    FIconDraw: Boolean;
    FCloseTopSpace: Integer;
    FCloseRightSpace: Integer;
    procedure SetCaptionFont(const Value: TFont);
    procedure SetInfoFont(const Value: TFont);
    procedure SetLineColor(const Value: TColor);
    procedure SetCloseBtnBmp(const Value: TBitmap);
    procedure SetMaxShowRowCount(const Value: Integer);
    procedure SetDrawIcon(const Value: TOnDrawIcon);
    procedure SetIconHeight(const Value: Integer);
    procedure SetIconWidth(const Value: Integer);
    procedure SetRowSpace(const Value: TRect);
    procedure SetRowBackBmp(const Value: TBitmap);
    procedure SetIconSpace(const Value: TRect);
    procedure SetIconDraw(const Value: Boolean);
    procedure SetCloseRightSpace(const Value: Integer);
    procedure SetCloseTopSpace(const Value: Integer);
  published
    property Align;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property InfoFont: TFont read FInfoFont write SetInfoFont;
    property LineColor: TColor read FLineColor write SetLineColor;
    property RowBackBmp: TBitmap read FRowBackBmp write SetRowBackBmp;
    property CloseBtnBmp: TBitmap read FCloseBtnBmp write SetCloseBtnBmp;
    property CloseTopSpace: Integer read FCloseTopSpace write SetCloseTopSpace;
    property CloseRightSpace: Integer read FCloseRightSpace write SetCloseRightSpace;

    property MaxShowRowCount: Integer read FMaxShowRowCount write SetMaxShowRowCount;
    property RowSpace: TRect read FRowSpace write SetRowSpace;
    property IconSpace: TRect read FIconSpace write SetIconSpace;
    property IconDraw: Boolean read FIconDraw write SetIconDraw;
    property IconWidth: Integer read FIconWidth write SetIconWidth;
    property IconHeight: Integer read FIconHeight write SetIconHeight;
    property OnDrawIcon: TOnDrawIcon read FOnDrawIcon write SetDrawIcon;
  end;
{$M-}

implementation

{ TJxdList }

procedure TJxdHintList.AddHintInfo(const ACaption, AInfo: string; const ATag: Integer);
var
  p: PHintListInfo;
begin
  New( p );
  p^.FCaption := ACaption;
  p^.FInfo := AInfo;
  p^.FTag := ATag;
  FHintList.Add( p );
  ReSetOwnerSize;
end;

function TJxdHintList.CalcPos(const Apt: TPoint; var AIndex: Integer; var AInCloseBtn: Boolean): Boolean;
var
  R: TRect;
begin
  AIndex := (FCurTopPos + Apt.Y) div FHintRowHeight;
  Result := (AIndex >= 0) and (AIndex < FHintList.Count);
  if Result then
  begin
    R := GetCloseRect(AIndex);
    Dec( R.Top, FCurTopPos );
    Dec( R.Bottom, FCurTopPos ); 
    AInCloseBtn := PtInRect( R, Apt );
  end
  else
    AIndex := -1;
end;

procedure TJxdHintList.CMMouseLeave(var Message: TMessage);
begin
  inherited;

end;

constructor TJxdHintList.Create(AOwner: TComponent);
begin
  inherited;
  FHintList := TList.Create;
  FCaptionFont := TFont.Create;
  FInfoFont := TFont.Create;
  InitFont;
  FHintRowHeight := FCaptionFont.Height + FInfoFont.Height + 6;
  FLineColor := $00EEEEEE;
  FCurTopPos := 0;
  FRowBackBmp := TBitmap.Create;
  FCloseBtnBmp := TBitmap.Create;
  FCloseBtnWidth := 0;
  FCloseBtnHeight := 0;
  FMaxShowRowCount := 8;
  FIconHeight := 16;
  FIconWidth := 16;
  FMouseMoveInfo.FIndex := -1;
  FRowSpace := Rect(1, 1, 1, 1);
  FIconSpace := Rect( 3, 5, 3, 0 );
  FIconDraw := True;
  FCloseBtnWidth := 0;
  FCloseBtnHeight := 0;
end;

procedure TJxdHintList.DeleteHintInfo(const AIndex: Integer);
var
  p: PHintListInfo;
begin
  if (AIndex >= 0) and (AIndex < FHintList.Count) then
  begin
    p := FHintList[AIndex];
    Dispose( p );
    ReSetOwnerSize;
  end;
end;

destructor TJxdHintList.Destroy;
var
  i: Integer;
begin
  for i := 0 to FHintList.Count - 1 do
    Dispose( PHintListInfo(FHintList[i]) );
  FHintList.Free;
  FCaptionFont.Free;
  FInfoFont.Free;
  FCloseBtnBmp.Free;
  FRowBackBmp.Free;
  inherited;
end;

function TJxdHintList.DoDrawIcon(ABmp: TBitmap; const AIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TJxdHintList.DoShowScrollBar;
begin

end;

procedure TJxdHintList.DrawHintList(ABmp: TBitmap);
var
  nH, nW, nX, nY, nIndex, nCount: Integer;
  bInCloseBtn: Boolean;
  TempBmp: TBitmap;
  DestR, SrcR: TRect;
begin
  ABmp.Canvas.Brush.Color := clWhite;
  ABmp.Canvas.FillRect( ClientRect );
  nH := ABmp.Height;
  nW := ABmp.Width;

  ABmp.Canvas.Pen.Color := FLineColor;
  nX := 1;
  nY := FHintRowHeight;
  CalcPos( Point(0, FCurTopPos), nIndex, bInCloseBtn );
  nCount := 0;
  TempBmp := TBitmap.Create;
  try
    while nH - nY >= 0 do
    begin
      ABmp.Canvas.MoveTo( nX, nY );
      ABmp.Canvas.LineTo( nX + nW - 2, nY );
      if GetHintRowRect( nIndex, DestR ) then
      begin
        TempBmp.Width := DestR.Right - DestR.Left;
        TempBmp.Height := DestR.Bottom - DestR.Top;

        SrcR := Rect(0, 0, TempBmp.Width, TempBmp.Height);

        TempBmp.Canvas.Brush.Color := clFuchsia;
        TempBmp.Canvas.FillRect( SrcR );

        DrawRow( TempBmp, PHintListInfo(FHintList[nIndex]), nIndex );
        Inc( nCount );

        if DestR.Bottom > nH then
        begin
          SrcR.Bottom := SrcR.Bottom - (DestR.Bottom - nH);
          DestR.Bottom := nH;
        end;

        ABmp.Canvas.Brush.Style := bsClear;
        SetBkMode( ABmp.Canvas.Handle, TRANSPARENT );
        ABmp.Canvas.BrushCopy( DestR, TempBmp, SrcR, clFuchsia );
      end;
      if nIndex + 1 < FHintList.Count then
        Inc( nIndex )
      else
        Break;
      if nCount = MaxShowRowCount then
        Break;
      Inc( nY, FHintRowHeight );
    end;
  finally
    TempBmp.Free;
  end;
end;

procedure TJxdHintList.DrawRow(ABmp: TBitmap; pInfo: PHintListInfo; const AIndex: Integer);
var
  R: TRect;
  nLeft: Integer;
  strText: string;
begin
  R := Rect( 0, 0, ABmp.Width, ABmp.Height );
  if Assigned(FRowBackBmp) and (not FRowBackBmp.Empty) then
  begin
    //画背景
    if FMouseMoveInfo.FIndex = AIndex then
      DrawRectangle( ABmp.Canvas, Rect(0, 0, FRowBackBmp.Width, FRowBackBmp.Height), R, FRowBackBmp );
  end
  else
  begin
    ABmp.Canvas.Brush.Color := RGB(255, 255, 0);
    ABmp.Canvas.FillRect( R );
  end;


  if DoDrawIcon(ABmp, AIndex) then
    nLeft := FIconWidth
  else
    nLeft := 0;
  Inc( nLeft, FRowSpace.Left );
  Inc( nLeft, FIconSpace.Right );

  if pInfo^.FCaption <> '' then
  begin
    ABmp.Canvas.Font := FCaptionFont;
    ABmp.Canvas.Brush.Style := bsClear;
    strText := FormatTitle( ABmp.Canvas, pInfo^.FCaption );
    R := Rect( nLeft, 0, ABmp.Width, ABmp.Height div 2 );
    DrawText(ABmp.Canvas.Handle, PChar(strText), Length(strText), R, DT_LEFT or DT_VCENTER or DT_SINGLELINE);
  end;

  if pInfo^.FInfo <> '' then
  begin
    ABmp.Canvas.Font := FInfoFont;
    ABmp.Canvas.Brush.Style := bsClear;
    strText := pInfo^.FInfo;
    R := Rect( nLeft, ABmp.Height div 2, ABmp.Width, ABmp.Height );
    DrawText(ABmp.Canvas.Handle, PChar(strText), Length(strText), R, DT_LEFT or DT_VCENTER or DT_SINGLELINE);
  end;
end;

function TJxdHintList.FormatTitle(const ACanvas: TCanvas; const ATitle: WideString): string;
var
  nW, nFontLen, n: Integer;
begin
  nW := Width - FRowSpace.Left - FRowSpace.Right;
  if IconDraw then
    Dec( nW, FIconWidth + FIconSpace.Left + FIconSpace.Right );
  if FCloseBtnWidth > 0 then
    Dec( nW, FCloseBtnWidth );
    
  nFontLen := ACanvas.TextWidth( ATitle );
  if nFontLen <= nW then
    Result := ATitle
  else
  begin
    Result := Copy( ATitle, 1, Length(ATitle) - 4 ) + '...';
    nFontLen := ACanvas.TextWidth( Result );
    n := 4;
    while nFontLen > nW do
    begin
      Inc( n, 2 );
      Result := Copy( ATitle, 1, Length(ATitle) - n ) + '...';
      nFontLen := ACanvas.TextWidth( Result );
    end;
  end;
end;

function TJxdHintList.GetCloseRect(const AIndex: Integer): TRect;
begin
  if (FCloseBtnWidth = 0) or (FCloseBtnHeight = 0) then
  begin
    Result := Rect( 0, 0, 0, 0 );
    Exit;
  end;
  Result.Left := Width - FCloseBtnWidth - 2;
  Result.Right := Result.Left + FCloseBtnWidth;
  Result.Top := AIndex * FHintRowHeight + (FHintRowHeight - FCloseBtnHeight) div 2;
  Result.Bottom := Result.Top + FCloseBtnHeight;
end;

function TJxdHintList.GetHintRowRect(const AIndex: Integer; var ARect: TRect): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FHintList.Count);
  if Result then
  begin
    with ARect do
    begin
      Left := FRowSpace.Left;
      Right := Width - FRowSpace.Right;
      Top := AIndex * FHintRowHeight - FCurTopPos + FRowSpace.Top;
      Bottom := Top + FHintRowHeight - FRowSpace.Bottom;
    end;
  end;
end;

procedure TJxdHintList.InitFont;
begin
  FCaptionFont.Name := 'Default';
  FCaptionFont.Height := 14;
  FCaptionFont.Color := clBlack;
  FCaptionFont.Style := [];

  FInfoFont.Name := 'Default';
  FInfoFont.Height := 13;
  FInfoFont.Color := $1E451F;
  FInfoFont.Style := [];
end;

procedure TJxdHintList.InvalidateRow(const AIndex: Integer; const AIsOnlyRedrawCloseRect: Boolean);
var
  R: TRect;
begin
  if GetHintRowRect(AIndex, R) then
  begin
    if AIsOnlyRedrawCloseRect then
      R := GetCloseRect(AIndex);
    InvalidateRect( Handle, @R, False );
  end;
end;

procedure TJxdHintList.Loaded;
begin
  inherited;
  if Assigned(FCloseBtnBmp) and (not FCloseBtnBmp.Empty) then
  begin
    FCloseBtnWidth := FCloseBtnBmp.Width;
    FCloseBtnHeight := FCloseBtnBmp.Height div 3;
  end;
  if Assigned(FRowBackBmp) and (not FRowBackBmp.Empty) then
    FHintRowHeight := FRowBackBmp.Height + FRowSpace.Top + FRowSpace.Bottom;
end;

procedure TJxdHintList.Paint;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Height := Height;
    Bmp.Width := Width;
    DrawHintList(bmp);
  finally
    Canvas.Draw(0, 0, Bmp);
    Bmp.Free;
  end;
end;

procedure TJxdHintList.ReSetOwnerSize;
var
  obj: TWinControl;
begin
  obj := Owner as TWinControl;
  if FHintList.Count > FMaxShowRowCount then
  begin
    obj.Height := FMaxShowRowCount * FHintRowHeight;
    DoShowScrollBar;
  end
  else
    obj.Height := FHintList.Count * FHintRowHeight;
end;

procedure TJxdHintList.SetCaptionFont(const Value: TFont);
begin
  FCaptionFont := Value;
end;

procedure TJxdHintList.SetCloseBtnBmp(const Value: TBitmap);
begin
  FCloseBtnBmp.Assign( Value );
  Invalidate;
end;

procedure TJxdHintList.SetCloseRightSpace(const Value: Integer);
begin
  FCloseRightSpace := Value;
end;

procedure TJxdHintList.SetCloseTopSpace(const Value: Integer);
begin
  FCloseTopSpace := Value;
end;

procedure TJxdHintList.SetDrawIcon(const Value: TOnDrawIcon);
begin
  FOnDrawIcon := Value;
end;

procedure TJxdHintList.SetIconDraw(const Value: Boolean);
begin
  FIconDraw := Value;
end;

procedure TJxdHintList.SetIconHeight(const Value: Integer);
begin
  FIconHeight := Value;
end;

procedure TJxdHintList.SetIconSpace(const Value: TRect);
begin
  FIconSpace := Value;
end;

procedure TJxdHintList.SetIconWidth(const Value: Integer);
begin
  FIconWidth := Value;
end;

procedure TJxdHintList.SetInfoFont(const Value: TFont);
begin
  FInfoFont := Value;
end;

procedure TJxdHintList.SetLineColor(const Value: TColor);
begin
  FLineColor := Value;
end;

procedure TJxdHintList.SetMaxShowRowCount(const Value: Integer);
begin
  FMaxShowRowCount := Value;
end;

procedure TJxdHintList.SetRowBackBmp(const Value: TBitmap);
begin
  FRowBackBmp.Assign( Value );
  Invalidate;
end;

procedure TJxdHintList.SetRowSpace(const Value: TRect);
begin
  FRowSpace := Value;
end;

procedure TJxdHintList.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
end;

procedure TJxdHintList.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
end;

procedure TJxdHintList.WMMouseMove(var Message: TWMMouseMove);
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
      InvalidateRow( nIndex, False );
    end;
  end
  else
  begin
    if FMouseMoveInfo.FIndex = -1 then
    begin
      FMouseMoveInfo.FIndex := nIndex;
      FMouseMoveInfo.FIsInCloseRect := bIsInCloseRt;
      FMouseMoveInfo.FMouseState := xcsActive;
      InvalidateRow( nIndex, False );
    end
    else if (FMouseMoveInfo.FIndex = nIndex) and (FMouseMoveInfo.FIsInCloseRect <> bIsInCloseRt) then
    begin
      FMouseMoveInfo.FIsInCloseRect := bIsInCloseRt;
      InvalidateRow( nIndex, True );
    end
    else if FMouseMoveInfo.FIndex <> nIndex then
    begin
      nTemp := FMouseMoveInfo.FIndex;
      FMouseMoveInfo.FIndex := nIndex;
      FMouseMoveInfo.FMouseState := xcsActive;
      FMouseMoveInfo.FIsInCloseRect := bIsInCloseRt;
      nIndex := nTemp;
      InvalidateRow( nIndex, False );
      InvalidateRow( FMouseMoveInfo.FIndex, False );
    end;
  end;
end;

end.
