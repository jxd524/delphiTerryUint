unit uJxdCustomScrollBar;

interface
uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Graphics, Messages, StdCtrls;

type
  TScrollStyle = (ssVerical, ssHorizontal);
  TMouseState = (msNormal, msActive, msDonw);
  TMousePosition = (mpNormal, mpUpButton, mpScrollButton, mpDownButton);
  TJxdCustomScrollBar = class(TGraphicControl) //(TCustomControl)//
  private
    FPosition: Integer;
    FMax: Integer;
    FMin: Integer;
    FMouseDownPos: Integer;
    FMouseDownInSltp: Boolean;
    FUpDownVisiable: Boolean;  //上下按钮是否存在
    FTimeMove: TTimer;
    FScrollStyle: TScrollStyle;
    FCurMousePos: TMousePosition;
    FMouseDowsMP: TMousePosition;
    FPosChange: TNotifyEvent;
    procedure Change;
    procedure DoTimeMove(Sender: TObject);
    function  GetMousePos(pt: TPoint): TMousePosition; overload;
    function  GetMousePos(X, Y: Integer): TMousePosition; overload;
    procedure SetUpDownVisiable(const Value: Boolean);
    procedure SetArea(const Index, Value: Integer);
    procedure SetPos(const Value: Integer);
    procedure SetSCrollStyle(const Value: TScrollStyle);
    procedure SetCurMousePos(const Value: TMousePosition);
    procedure ChangPos(const APos: Integer);
  protected
    FCurScrollRect: TRect;
    FScrollBtnSize: Integer; //Verical: 长度; Horizontal: 宽度

    FUpDownSize: TSize;
    FUpBtnRect: TRect;
    FDownBtnRect: TRect;
    FPosPiexl: Double; //1 position = FPosPiexl Piexl

    FCurMouseState: TMouseState;

    property CurMousePos: TMousePosition read FCurMousePos write SetCurMousePos;

    procedure DoScrollStyleChange; virtual;
    procedure CalcScrollInfo;
    procedure CalcScrollPos;
  protected    
    procedure Resize; override;
    procedure AdjustSize; override;
    procedure RequestAlign; override;

//    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property Position: Integer read FPosition write SetPos;
    property Max: Integer index 1 read FMax write SetArea;
    property Min: Integer index 2 read FMin write SetArea;
    property UpDownVisiable: Boolean read FUpDownVisiable write SetUpDownVisiable;
    property ScrollStyle: TScrollStyle read FScrollStyle write SetSCrollStyle;
    property OnPosChange: TNotifyEvent read FPosChange write FPosChange;
  published
    property Align;
    property Anchors;
    property Visible;
    property Enabled;
    property OnClick;
    property OnDblClick;
    property OnCanResize;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
  end;
implementation

uses uBitmapHandle;

{ TKKScrollBar }

const
  CtFitLenByPos = 20; // 1 Pos 最适合的长度
  CtMinScrollLen = 40;

procedure TJxdCustomScrollBar.AdjustSize;
begin
  inherited;
  CalcScrollInfo;
end;


procedure TJxdCustomScrollBar.CalcScrollInfo;
var
  nLen, nCount, nFitLen: Integer;
begin
  if FMax = FMin then Exit;
  nLen := 0;
  //计算向上,向下, 向左和向右按钮位置
  case FScrollStyle of
    ssVerical:
    begin
      nLen := Height;
      if FUpDownVisiable then
      begin
        nLen := nLen - FUpDownSize.cy * 2;
        FUpBtnRect := Rect(0, 0, FUpDownSize.cx, FUpDownSize.cy);
        FDownBtnRect := Rect(FUpBtnRect.Left, Height - HeightOfRect(FUpBtnRect),
                             WidthOfRect(FUpBtnRect) + FUpBtnRect.Left, Height );
      end;
    end;
    ssHorizontal:
    begin
      nLen := Width;
      if FUpDownVisiable then
      begin
        nLen := nLen - FUpDownSize.cx * 2;
        FUpBtnRect := Rect(0, 0, FUpDownSize.cx, FUpDownSize.cy);
        FDownBtnRect := Rect(Width - WidthOfRect(FUpBtnRect), FUpBtnRect.Top,
                             Width, FUpBtnRect.Bottom );
      end;
    end;
  end;
  //计算转换单位和滑块大小
  nCount := FMax - FMin ;
  nFitLen := nCount * CtFitLenByPos;
  if nFitLen + CtMinScrollLen <= nLen then
  begin
    FScrollBtnSize := nLen - nFitLen;
    FPosPiexl := CtFitLenByPos;
  end
  else
  begin
    FPosPiexl := (nLen - CtMinScrollLen) / nCount;
    FScrollBtnSize := Round(nLen - FPosPiexl * nCount);
  end;
  CalcScrollPos;
end;

procedure TJxdCustomScrollBar.CalcScrollPos;
var
  n: Double;
  nBegin, nLen: Integer;
begin
  n := FPosition * FPosPiexl;
  case FScrollStyle of
    ssVerical:
    begin
      if FUpDownVisiable then
      begin
        nBegin := FUpBtnRect.Left;
        nLen := WidthOfRect(FUpBtnRect);
      end
      else
      begin
        nBegin := 0;
        nLen := Width;
      end;
      n := n + nLen;
      with FCurScrollRect do
      begin
        Left := nBegin;
        Top := Round(n);
        Right := Left + nLen;
        Bottom := Top + FScrollBtnSize;
      end;
    end;
    ssHorizontal:
    begin
      if FUpDownVisiable then
      begin
        nBegin := FUpBtnRect.Top;
        nLen := HeightOfRect(FUpBtnRect);
      end
      else
      begin
        nBegin := 0;
        nLen := Height;
      end;
      n := n + nLen;
      with FCurScrollRect do
      begin
        Left := Round(n);
        Top := nBegin;
        Right := Left + FScrollBtnSize;
        Bottom := nLen;
      end;
    end;
  end;
end;

procedure TJxdCustomScrollBar.Change;
begin
  if Assigned(FPosChange) then FPosChange(Self);
end;

procedure TJxdCustomScrollBar.ChangPos(const APos: Integer);
var
  nPos: Integer;
begin
  nPos := APos;
  if FPosition * FPosPiexl < nPos then
    nPos := nPos - FScrollBtnSize;
  case FScrollStyle of
    ssVerical: if FUpDownVisiable then nPos := nPos - HeightOfRect(FUpBtnRect);
    ssHorizontal: if FUpDownVisiable then nPos := nPos - WidthOfRect(FUpBtnRect);
    end;
  Position := Round(nPos / FPosPiexl);
end;

constructor TJxdCustomScrollBar.Create(AOwner: TComponent);
begin
  inherited;
//  DoubleBuffered := True;
  FTimeMove := TTimer.Create( Self );
  FTimeMove.OnTimer := DoTimeMove;
  FTimeMove.Enabled := False;
  FPosPiexl := 0;
  FScrollStyle := ssVerical;
  FUpDownSize.cx := 17;
  FUpDownSize.cy := 17;
  Width := FUpDownSize.cy;
  Height := 150;
  FPosition := 0;
  FMax := 100;
  FMin := 0;
  FUpDownVisiable := True;
  FScrollBtnSize := 20;  
  FCurMouseState := msNormal;
  FCurMousePos := mpNormal;
  FMouseDowsMP := mpNormal;
  FMouseDownInSltp := False;

  CalcScrollInfo;
end;

destructor TJxdCustomScrollBar.Destroy;
begin
  FTimeMove.Enabled := False;
  FTimeMove.Free;
  inherited;
end;

procedure TJxdCustomScrollBar.DoScrollStyleChange;
begin
  CalcScrollInfo;
end;

procedure TJxdCustomScrollBar.DoTimeMove(Sender: TObject);
var
  tr: TTimer;
  pt: TPoint;
begin
  if not (Sender is TTimer) then Exit;
  tr := Sender as TTimer;
  tr.Enabled := False;
  try
    case CurMousePos of
      mpUpButton: Position := Position - 1;
      mpDownButton: Position := Position + 1;
    end;
  finally
    case CurMousePos of
      mpUpButton: tr.Enabled := Position <> FMin;
      mpDownButton: tr.Enabled := Position <> FMax;
      else
      begin
        tr.Enabled := False;
        Invalidate;
      end;
    end;
    GetCursorPos( pt );
    pt := ScreenToClient(pt);
    CurMousePos := GetMousePos( pt );
  end;
end;

function TJxdCustomScrollBar.GetMousePos(X, Y: Integer): TMousePosition;
var
  pt: TPoint;
begin
  pt.X := X;
  pt.Y := Y;
  Result := GetMousePos( pt );
end;

function TJxdCustomScrollBar.GetMousePos(pt: TPoint): TMousePosition;
begin
  if FUpDownVisiable and PtInRect(FUpBtnRect, pt) then
    Result := mpUpButton
  else if PtInRect(FCurScrollRect, pt) then
    Result := mpScrollButton
  else if FUpDownVisiable and PtInRect(FDownBtnRect, pt) then
    Result := mpDownButton
  else
    Result := mpNormal;
end;

procedure TJxdCustomScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FCurMouseState <> msDonw then
  begin
    FCurMouseState := msDonw;
    if FScrollStyle = ssVerical then
      FMouseDownPos := Y
    else
      FMouseDownPos := X;
    CurMousePos := GetMousePos( X, Y );
    if CurMousePos in [mpUpButton, mpDownButton] then
    begin
      FTimeMove.Interval := 50;
      FTimeMove.Enabled := True;
    end
    else if CurMousePos = mpNormal then
      ChangPos( FMouseDownPos );
    FMouseDowsMP := CurMousePos;
    Invalidate;
  end;
end;

procedure TJxdCustomScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FTimeMove.Enabled then
    FTimeMove.Enabled := False;
  FCurMouseState := msActive;
  CurMousePos := GetMousePos( X, Y );
  Invalidate;
end;

procedure TJxdCustomScrollBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  n, nLen, nPos: Integer;
  bPosAdd: Boolean;
begin
  inherited;
  CurMousePos := GetMousePos( X, Y );
  if (FCurMouseState = msDonw) and (FMouseDowsMP = mpScrollButton) then
  begin
    if FScrollStyle = ssVerical then
      n := Y
    else
      n := X;
    nLen := n - FMouseDownPos;
    if abs(nLen) > FPosPiexl then
    begin
      bPosAdd := nLen > 0;
      nLen := abs(nLen);
      nPos := Round(nLen / FPosPiexl);
      if bPosAdd then
        Position := Position + nPos
      else
        Position := Position - nPos;
      FMouseDownPos := n;
    end;
  end;
  Invalidate;
end;

procedure TJxdCustomScrollBar.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Enabled and (FCurMouseState <> msActive) and (not FMouseDownInSltp) then
  begin
    FCurMouseState := msActive;
    Invalidate;
  end;
end;

procedure TJxdCustomScrollBar.CMMouseLeave(var Message: TMessage);
begin
  if Enabled and (FCurMouseState <> msNormal) and (not FMouseDownInSltp) then
  begin
    FCurMouseState := msNormal;
    Invalidate;
  end;
end;

procedure TJxdCustomScrollBar.RequestAlign;
begin
  inherited;
  CalcScrollInfo;
end;

procedure TJxdCustomScrollBar.Resize;
begin
  inherited;
  CalcScrollInfo;
end;

procedure TJxdCustomScrollBar.SetArea(const Index, Value: Integer);
begin
  if Value < 0 then Exit;
  if Index = 1 then
  begin
    FMax := Value;
    if FPosition > FMax then
      FPosition := FMax;
  end
  else if Index = 2 then
  begin
    FMin := Value;
    if FPosition < FMin then
      FPosition := FMin;
  end;
  CalcScrollInfo;
  Invalidate;
end;


procedure TJxdCustomScrollBar.SetCurMousePos(const Value: TMousePosition);
begin
  FCurMousePos := Value;
  case Value of
    mpNormal: Cursor := crDefault;
    mpUpButton, mpScrollButton, mpDownButton: Cursor := crHandPoint;
  end;
end;

procedure TJxdCustomScrollBar.SetPos(const Value: Integer);
begin
  if (FPosition <> Value) and (Value >= FMin) and (Value <= FMax) then
  begin
    FPosition := Value;
    CalcScrollPos;
    Change;
    Invalidate;
  end;
end;

procedure TJxdCustomScrollBar.SetSCrollStyle(const Value: TScrollStyle);
begin
  if FScrollStyle <> Value then
  begin
    FScrollStyle := Value;
    DoScrollStyleChange;
    Invalidate;
  end;
end;

procedure TJxdCustomScrollBar.SetUpDownVisiable(const Value: Boolean);
begin
  FUpDownVisiable := Value;
  Invalidate;
end;

//procedure TJxdCustomScrollBar.WMPaint(var Message: TWMPaint);
//var
//  dc: HDC;
//begin
//  inherited;
//  dc := GetDC( Handle );
//  Paint( dc );
//  ReleaseDC( Handle, dc );
//end;

end.
