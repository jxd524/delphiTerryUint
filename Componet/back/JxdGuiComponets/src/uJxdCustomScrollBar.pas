unit uJxdCustomScrollBar;

interface
uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Graphics, Messages, StdCtrls, uJxdDrawSub, uJxdGraphicBaseClass, uJxdGuiStyle;

type
  TScrollStyle = (ssVerical, ssHorizontal);
  TMousePosition = (mpNormal, mpFirstButton, mpSlipButton, mpSencondButton);
  TScrollBarButtonSize = record
    FWidth, FHeight: Integer;
  end;

  TxdCustomScrollBar = class(TxdGraphicBase)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Click; override;
  protected
    procedure DoScrollStyleChanged; virtual;
    procedure SetScrollBarButtonSize(const ABtnWidth, ABtnHeight: Integer); virtual;
    function  GetFirstButtonRect(var ARect: TRect): Boolean; overload;
    function  GetSencondButtonRect(var ARect: TRect): Boolean; overload;
    function  GetSlipRect(var ARect: TRect): Boolean; overload;
    function  GetCurMousePosition: TMousePosition;
    function  MouseToPos(const Apt: TPoint; var APos: Integer): Boolean;

    procedure Resize; override;
    procedure DoControlStateChanged(const AOldState, ANewState: TxdComponentState; const ACurPt: TPoint); override;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
  private
    FScrollBarBtnSize: TScrollBarButtonSize;
    FFirstBtnRect, FSencondBtnRect: TRect;
    FSlipRect: TRect; //滑动位置
    FPrePosPiexl: Double;
    FCurMousePosition: TMousePosition;
    FMouseDownPt: TPoint;
    FChangedBySelf: Boolean;
    FChangedPosTimer: TTimer;
    procedure CreateTimeToMovePos(const ATag: Integer);
    procedure CalcSlipRect;
    procedure ReCalcAllScrollBarInfo;
    procedure SetCurMousePostion(const pt: TPoint);
    procedure DoTimeChangedPos(Sender: TObject);
    procedure DoPositionChanged;
  private
    FScrollBarStyle: TScrollStyle;
    FMax: Integer;
    FPos: Integer;
    FScrollBtnVisible: Boolean;
    FOnPositionChanged: TxdComponentChanged;
    procedure SetScrollBarStyle(const Value: TScrollStyle);
    procedure SetMax(const Value: Integer);
    procedure SetPos(const Value: Integer);
    procedure SetScrollBtnVisible(const Value: Boolean);
  published
    property ScrollBarStyle: TScrollStyle read FScrollBarStyle write SetScrollBarStyle;
    property Max: Integer read FMax write SetMax;
    property Position: Integer read FPos write SetPos;
    property ScrollButtonVisible: Boolean read FScrollBtnVisible write SetScrollBtnVisible;
    property OnPositionChanged: TxdComponentChanged read FOnPositionChanged write FOnPositionChanged; 
  end;

implementation

{ TJxdCustomScrollBar }

const
  CtMinSlipSize = 38;     //滑块最小大小
  CtMinPerSlipPiexl = 2;  //最小滚动像素点

procedure TxdCustomScrollBar.CalcSlipRect;
var
  nLen, nBtnSize, nSlipSize, nSlipBackLen: Integer;
begin
  if Max <= 0 then Exit;
  if FScrollBarStyle = ssVerical then
  begin
    nLen := Width;
    nBtnSize := WidthOfRect( FFirstBtnRect );
  end
  else
  begin
    nLen := Height;
    nBtnSize := HeightOfRect( FFirstBtnRect );
  end;
  if FScrollBtnVisible then
    Dec( nLen, nBtnSize * 2 )
  else
    nBtnSize := 0;

  nSlipBackLen := (Max - 1) * CtMinPerSlipPiexl;
  if nSlipBackLen + CtMinSlipSize > nLen then
  begin
    FPrePosPiexl := ( nLen - CtMinSlipSize ) / Max;
    nSlipSize := nLen - Round( Max * FPrePosPiexl );
  end
  else
  begin
    FPrePosPiexl := CtMinPerSlipPiexl;
    nSlipSize := nLen - Max * CtMinPerSlipPiexl;
  end;

  if FScrollBarStyle = ssVerical then
  begin
    with FSlipRect do
    begin
      Top := 0;
      Left := nBtnSize + Round( FPrePosPiexl * FPos );
      Right := Left + nSlipSize;
      Bottom := Height;
    end;
  end
  else
  begin
    with FSlipRect do
    begin
      Top := nBtnSize + Round( FPrePosPiexl * FPos );
      Left := 0;
      Right := Width;
      Bottom := Top + nSlipSize;
    end;
  end;
end;

procedure TxdCustomScrollBar.Click;
var
  pt: TPoint;
  R: TRect;
begin
  inherited;
  GetCursorPos( pt );
  pt := ScreenToClient(pt);
  if PtInRect(FFirstBtnRect, pt) then
  begin
    FChangedBySelf := True;
    Position := Position - 1;
    R := FFirstBtnRect;
    InvalidateGraphicRect( @R );
  end
  else if PtInRect(FSencondBtnRect, pt) then
  begin
    FChangedBySelf := True;
    Position := Position + 1;
    R := FSencondBtnRect;
    InvalidateGraphicRect( @R );
  end
end;

constructor TxdCustomScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  FScrollBarStyle := ssVerical;
  Width := 100;
  Height := 7;
  FMax := 100;
  FPos := 0;
  FPrePosPiexl := 0.0;
  FScrollBtnVisible := True;
  FScrollBarBtnSize.FWidth := 0;
  FScrollBarBtnSize.FHeight := 0;
  FFirstBtnRect := Rect( 0, 0, FScrollBarBtnSize.FWidth, FScrollBarBtnSize.FHeight );
  FSencondBtnRect := FFirstBtnRect;
  FSlipRect := FFirstBtnRect;
  FCurMousePosition := mpNormal;
  FMouseDownPt.X := 0;
  FMouseDownPt.Y := 0;
  FChangedPosTimer := nil;
  FChangedBySelf := False;
end;

procedure TxdCustomScrollBar.CreateTimeToMovePos(const ATag: Integer);
begin
  if FChangedPosTimer = nil then
  begin
    FChangedPosTimer := TTimer.Create( Self );
    FChangedPosTimer.Interval := 100;
    FChangedPosTimer.OnTimer := DoTimeChangedPos;
  end;
  FChangedPosTimer.Tag := ATag;
  FChangedPosTimer.Enabled := True;
end;

destructor TxdCustomScrollBar.Destroy;
begin

  inherited;
end;

procedure TxdCustomScrollBar.DoControlStateChanged(const AOldState, ANewState: TxdComponentState; const ACurPt: TPoint);
var
  R: TRect;
begin
  FMouseDownPt := Point( 0, 0 );
//  case ANewState of
//    csNormal: OutputDebugString( 'normal' );
//    csActive: OutputDebugString( 'active' );
//    csDown:   OutputDebugString( 'down' );
//  end;
  if ANewState = csDown then
  begin
    if PtInRect(FSlipRect, ACurPt) then
    begin
      FMouseDownPt := ACurPt;
      R := FSlipRect;
      InvalidateGraphicRect( @R );
    end
    else if PtInRect(FFirstBtnRect, ACurPt) then
    begin
      CreateTimeToMovePos( -1 );
      R := FFirstBtnRect;
      InvalidateGraphicRect( @R );
    end
    else if PtInRect(FSencondBtnRect, ACurPt) then
    begin
      CreateTimeToMovePos( 1 );
      R := FSencondBtnRect;
      InvalidateGraphicRect( @R );
    end
    else
    begin
      if MouseToPos(ACurPt, R.Left) then
      begin
        InvalidateGraphicRect( nil );
        FChangedBySelf := True;
        Position := R.Left;
//        CreateTimeToMovePos( 1 );
      end;
    end;
  end
  else
  begin
    if Assigned(FChangedPosTimer) then
      FreeAndNil( FChangedPosTimer );

    if ANewState = csNormal then
      FCurMousePosition := mpNormal;
    InvalidateGraphicRect( nil );
  end;
end;

procedure TxdCustomScrollBar.DoPositionChanged;
begin
  if Assigned(OnPositionChanged) then
    OnPositionChanged( Self, FChangedBySelf );
end;

procedure TxdCustomScrollBar.DoScrollStyleChanged;
begin

end;

procedure TxdCustomScrollBar.DoTimeChangedPos(Sender: TObject);
var
  tr: TTimer;
begin
  if Sender is TTimer then
  begin
    tr := Sender as TTimer;
    tr.Enabled := False;
    try
      FChangedBySelf := True;
      Position := Position + tr.Tag;
    finally
      tr.Enabled := (Position <> Max) or (Position <> 0);
      if not tr.Enabled then
        tr.Free;
    end;
  end;
end;

function TxdCustomScrollBar.GetCurMousePosition: TMousePosition;
begin
  Result := FCurMousePosition;
end;

function TxdCustomScrollBar.GetFirstButtonRect(var ARect: TRect): Boolean;
begin
  Result := ( WidthOfRect(FFirstBtnRect) > 0 ) and ( HeightOfRect(FFirstBtnRect) > 0 );
  if Result then
    ARect := FFirstBtnRect;
end;

function TxdCustomScrollBar.GetSencondButtonRect(var ARect: TRect): Boolean;
begin
  Result := ( WidthOfRect(FSencondBtnRect) > 0 ) and ( HeightOfRect(FSencondBtnRect) > 0 );
  if Result then
    ARect := FSencondBtnRect;
end;

function TxdCustomScrollBar.GetSlipRect(var ARect: TRect): Boolean;
begin
  Result := ( WidthOfRect(FSlipRect) > 0 ) and ( HeightOfRect(FSlipRect) > 0 );
  if Result then
    ARect := FSlipRect;
end;

function TxdCustomScrollBar.MouseToPos(const Apt: TPoint; var APos: Integer): Boolean;
var
  nCurLen, nBtnSize, nSlipSize: Integer;
  nState: Integer; //0: 前 1: 后 other error
begin
  Result := False;
  if FScrollBarStyle = ssVerical then
  begin
    if Apt.X < FSlipRect.Left then
      nState := 0
    else if Apt.X + WidthOfRect(FSlipRect) > FSlipRect.Right then
      nState := 1
    else Exit;

    nCurLen := Apt.X;
    nBtnSize := FScrollBarBtnSize.FWidth;
    nSlipSize := WidthOfRect( FSlipRect );
  end
  else
  begin
    if Apt.Y < FSlipRect.Top then
      nState := 0
    else if Apt.Y + HeightOfRect(FSlipRect) > FSlipRect.Bottom then
      nState := 1
    else Exit;

    nCurLen := Apt.Y;
    nBtnSize := FScrollBarBtnSize.FHeight;
    nSlipSize := HeightOfRect( FSlipRect );
  end;
  if FScrollBtnVisible then
    Dec( nCurLen, nBtnSize );
  if nState = 1 then
    Dec( nCurLen, nSlipSize );

  APos := Round( nCurLen / FPrePosPiexl );
  Result := ( APos >= 0 ) and ( APos <= Max );
end;

procedure TxdCustomScrollBar.ReCalcAllScrollBarInfo;
begin
  if (FScrollBarBtnSize.FWidth > 0) and (FScrollBarBtnSize.FHeight > 0) then
  begin
    SetScrollBarButtonSize( FScrollBarBtnSize.FWidth, FScrollBarBtnSize.FHeight );
    CalcSlipRect;
  end;
end;

procedure TxdCustomScrollBar.Resize;
begin
  inherited;
  ReCalcAllScrollBarInfo;
end;

procedure TxdCustomScrollBar.SetCurMousePostion(const pt: TPoint);
  procedure ChangedMousePosState(const AState: TMousePosition; ARect: TRect);
  begin
    FCurMousePosition := AState;
    InvalidateGraphicRect( nil );
  end;
begin
  if PtInRect(FFirstBtnRect, pt) then
  begin
    if FCurMousePosition <> mpFirstButton then
      ChangedMousePosState( mpFirstButton, FFirstBtnRect )
  end
  else if PtInRect(FSencondBtnRect, pt) then
  begin
   if FCurMousePosition <> mpSencondButton then
     ChangedMousePosState( mpSencondButton, FSencondBtnRect );
  end
  else if PtInRect(FSlipRect, pt) then
  begin
    if FCurMousePosition <> mpSlipButton then
      ChangedMousePosState( mpSlipButton, FSlipRect )
  end
  else
  begin
    if FCurMousePosition <> mpNormal then
      ChangedMousePosState( mpNormal, Rect(0, 0, Width, Height) );                    
  end;
end;

procedure TxdCustomScrollBar.SetMax(const Value: Integer);
begin
  if (FMax <> Value) and (Value >= 0) then
  begin
    FMax := Value;
    if FPos >= FMax then
    begin
      FChangedBySelf := True;
      Position := Max;
    end;
    ReCalcAllScrollBarInfo;
    InvalidateGraphicRect( nil );
  end;
end;

procedure TxdCustomScrollBar.SetPos(const Value: Integer);
var
  R1, R2: TRect;
begin
  if (Value >= 0) and (FPos <> Value) and (Value <= Max) then
  begin
    FPos := Value;
    R1 := FSlipRect;
    CalcSlipRect;
    R2 := FSlipRect;

    if FScrollBarStyle = ssVerical then
    begin
      if R1.Left > R2.Left then
        R1.Left := R2.Left
      else
        R1.Right := R2.Right;
    end
    else
    begin
      if R1.Top > R2.Top then
        R1.Top := R2.Top
      else
        R1.Bottom := R2.Bottom;
    end;
    InvalidateGraphicRect( @R1 );
    DoPositionChanged;
  end;
  FChangedBySelf := False;
end;

procedure TxdCustomScrollBar.SetScrollBarButtonSize(const ABtnWidth, ABtnHeight: Integer);
begin
  case FScrollBarStyle of
    ssVerical:
    begin
      FScrollBarBtnSize.FWidth := ABtnWidth;
      if ABtnHeight <= 0 then
        FScrollBarBtnSize.FHeight := Height
      else
        FScrollBarBtnSize.FHeight := ABtnHeight;

      FFirstBtnRect := Rect( 0, 0, FScrollBarBtnSize.FWidth, FScrollBarBtnSize.FHeight );
      FSencondBtnRect := FFirstBtnRect;
      OffsetRect( FSencondBtnRect, Width - FScrollBarBtnSize.FWidth, 0 );
    end;
    ssHorizontal:
    begin
      FScrollBarBtnSize.FHeight := ABtnHeight;
      if ABtnWidth <= 0 then
        FScrollBarBtnSize.FWidth := Width
      else
        FScrollBarBtnSize.FWidth := ABtnWidth;

      FFirstBtnRect := Rect( 0, 0, FScrollBarBtnSize.FWidth, FScrollBarBtnSize.FHeight );
      FSencondBtnRect := FFirstBtnRect;
      OffsetRect( FSencondBtnRect, 0, Height - FScrollBarBtnSize.FHeight );
    end;
  end;
  CalcSlipRect;
end;

procedure TxdCustomScrollBar.SetScrollBarStyle(const Value: TScrollStyle);
var
  n: Integer;
begin
  if FScrollBarStyle <> Value then
  begin
    FScrollBarStyle := Value;
    if (FScrollBarBtnSize.FWidth > 0) and (FScrollBarBtnSize.FHeight > 0) then
    begin
      n := FScrollBarBtnSize.FWidth;
      FScrollBarBtnSize.FWidth := FScrollBarBtnSize.FHeight;
      FScrollBarBtnSize.FHeight := n;
      SetScrollBarButtonSize( FScrollBarBtnSize.FWidth, FScrollBarBtnSize.FHeight );
      DoScrollStyleChanged;
      InvalidateGraphicRect( nil );
    end;
  end;
end;

procedure TxdCustomScrollBar.SetScrollBtnVisible(const Value: Boolean);
begin
  if FScrollBtnVisible <> Value then
  begin
    FScrollBtnVisible := Value;
    CalcSlipRect;
    InvalidateGraphicRect( nil );
  end;
end;

procedure TxdCustomScrollBar.WMMouseMove(var Message: TWMMouseMove);
var
  nLen, nPos: Integer;
begin
  inherited;
  SetCurMousePostion( Point(Message.XPos, Message.YPos) );
  if (FMouseDownPt.X <> 0) and (FMouseDownPt.Y <> 0) then
  begin
    if FScrollBarStyle = ssVerical then
      nLen := Message.XPos - FMouseDownPt.X
    else
      nLen := Message.YPos - FMouseDownPt.Y;
    nPos := Round( nLen / FPrePosPiexl );
    if abs(nPos) > 0 then
    begin
      FMouseDownPt.X := Message.XPos;
      FMouseDownPt.Y := Message.YPos;
      FChangedBySelf := True;
      Position := Position + nPos;
    end;
  end;
end;

end.
