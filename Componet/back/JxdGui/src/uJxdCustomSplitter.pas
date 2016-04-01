unit uJxdCustomSplitter;

interface
  uses SysUtils, Classes, Windows, Controls, Types, ExtCtrls, Graphics, Messages, uJxdCustomButton;

type
  TSplitterStyle = ( ssHorizontal, ssVertical );
  TSplitterWay = (swLeft, swRight, swUp, swDown);
  TJxdCustomSplitter = class(TSplitter)
  private
    FOldCursor: TCursor;
    FButtonLen: Integer;
    FButtonClick: TNotifyEvent;
    FSplitterStyle: TSplitterStyle;
    FSplitterWay: TSplitterWay;
    procedure SetbuttonLen(const Value: Integer);
    procedure SetSplitterStyle(const Value: TSplitterStyle);
    procedure BtnClick;
    procedure SetSplitterWay(const Value: TSplitterWay);
  protected
    procedure DrawSplitter(ACanvas: TCanvas); virtual;
    property  ButtonLen: Integer read FButtonLen write SetbuttonLen;
  protected
    FMouseState: TJxdControlState;
    FButtonRect: TRect;
    procedure Paint; override;

    procedure CalcCtrlSize; virtual;
    function CheckMouseInButton: Boolean;
    procedure Resize; override;
    procedure AdjustSize; override;
    procedure RequestAlign; override;
    procedure WMSysKeyDown(var Message: TWMKeyDown); message WM_SYSKEYDOWN;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property SplitterWay: TSplitterWay read FSplitterWay write SetSplitterWay;
    property SplitterStyle: TSplitterStyle read FSplitterStyle write SetSplitterStyle;
    property OnButtonClick: TNotifyEvent read FButtonClick write FButtonClick;
  end;


implementation

{ TKKCheckButton }

procedure TJxdCustomSplitter.AdjustSize;
begin
  inherited;
  CalcCtrlSize;
end;

procedure TJxdCustomSplitter.BtnClick;
begin
  if Assigned(FButtonClick) then FButtonClick(Self);
end;

procedure TJxdCustomSplitter.CalcCtrlSize;
begin
  case SplitterStyle of
    ssHorizontal:
    begin
      with FButtonRect do
      begin
        Left := (Width - FButtonLen) div 2;
        Top := 0;
        Right := Left + FButtonLen;
        Bottom := Height;
      end;
    end;
    ssVertical:
    begin
      with FButtonRect do
      begin
        Left := 0;
        Top := (Height - FButtonLen) div 2;
        Right := Width;
        Bottom := Top + FButtonLen;
      end;
    end;
  end;
end;

function TJxdCustomSplitter.CheckMouseInButton: Boolean;
var
  pt: TPoint;
begin
  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  if PtInRect(FButtonRect, pt) then
  begin
    if Cursor <> crHandPoint then
    begin
      FOldCursor := Cursor;
      Cursor := crHandPoint;
    end;
    Result := True;
  end
  else
  begin
    if (FOldCursor <> 0) and (FOldCursor <> Cursor) then
    begin
      Cursor := FOldCursor;
      FOldCursor := 0;
    end;
    Result := False;
  end;
end;

procedure TJxdCustomSplitter.CMMouseEnter(var Message: TMessage);
begin
  if Enabled and (FMouseState <> xcsActive) then
  begin
    FMouseState := xcsActive;
    CheckMouseInButton;
    Invalidate;
  end;
end;

procedure TJxdCustomSplitter.CMMouseLeave(var Message: TMessage);
begin
  if Enabled and (FMouseState <> xcsNormal) then
  begin
    FMouseState := xcsNormal;
    Invalidate;
  end;
end;

constructor TJxdCustomSplitter.Create(AOwner: TComponent);
begin
  if (AOwner <> nil) and (AOwner is TWinControl) and ( not (AOwner as TWinControl).DoubleBuffered ) then
    (AOwner as TWinControl).DoubleBuffered := True;
  inherited;
  FButtonLen := 68;
  FOldCursor := 0;
end;

destructor TJxdCustomSplitter.Destroy;
begin
  inherited;
end;

procedure TJxdCustomSplitter.DrawSplitter(ACanvas: TCanvas);
begin

end;

procedure TJxdCustomSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and Enabled then
  begin
    FMouseState := xcsDown;
    CheckMouseInButton;
  end;
  Inherited;
end;

procedure TJxdCustomSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  CheckMouseInButton;
  Invalidate;
end;

procedure TJxdCustomSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Enabled then
  begin
    if CheckMouseInButton then
      BtnClick ;
//    else
//      inherited;
    if PtInRect( ClientRect, Point(X, Y) ) and ( FMouseState <> xcsActive ) then
    begin
      FMouseState := xcsActive;
      Invalidate;
    end;
  end;
end;

procedure TJxdCustomSplitter.Paint;
var
  BufBmp: TBitmap;
begin
  BufBmp := TBitmap.Create;
  try
    BufBmp.Width := Width;
    BufBmp.Height := Height;
    DrawSplitter(BufBmp.Canvas);
    Canvas.Draw(0, 0, BufBmp);
  finally
    BufBmp.Free;
  end;
end;

procedure TJxdCustomSplitter.RequestAlign;
begin
  inherited;
  CalcCtrlSize;
end;

procedure TJxdCustomSplitter.Resize;
begin
  CalcCtrlSize;
  inherited;
end;

procedure TJxdCustomSplitter.SetbuttonLen(const Value: Integer);
begin
  if FButtonLen <> Value then
  begin
    FButtonLen := Value;
    CalcCtrlSize;
  end;
end;

procedure TJxdCustomSplitter.SetSplitterStyle(const Value: TSplitterStyle);
begin
  if FSplitterStyle <> Value then
  begin
    FSplitterStyle := Value;
    CalcCtrlSize;
    Invalidate;
  end;
end;

procedure TJxdCustomSplitter.SetSplitterWay(const Value: TSplitterWay);
begin
  FSplitterWay := Value;
  Invalidate;
end;

procedure TJxdCustomSplitter.WMSysKeyDown(var Message: TWMKeyDown);
begin

end;

end.
