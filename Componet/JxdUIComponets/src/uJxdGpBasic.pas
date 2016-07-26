unit uJxdGpBasic;

interface
uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, GDIPAPI, GDIPOBJ, Graphics, uJxdGpStyle;

type
  TxdGraphicsBasic = class(TControl)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Click; override;
    procedure   ClearDrawRect; 
    procedure   InvalidateRect(const AR: TGPRect); overload;
    procedure   InvalidateRect(const AR: TGPRectF); overload;
    procedure   InvalidateRect(const AR: TRect); overload;
    procedure   Invalidate; override;
  protected
    FCurReDrawRect: TGPRect;
    //子类实现
    procedure DrawGraphics(const AGh: TGPGraphics); virtual;
    procedure DoControlStateChanged(const AOldState, ANewState: TxdGpUIState; const ACurPt: TPoint); virtual;
    //绘制接口
    procedure SetParent(AParent: TWinControl); override;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMSetText(var Message: TMessage); message WM_SETTEXT;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    //功能
    function  GetCurControlState: TxdGpUIState; inline;
    function  IsXolGpRect(const AR: TGPRect): Boolean; //指定的范围是否在当前需要绘制的位置
  private
    FDelBufBmpTimer: TTimer;
    FBufBmp: TGPBitmap;
    FDownPt: TPoint;
    FLButtonDown: Boolean;
    FCurControlState: TxdGpUIState;
    FAllowMoveByMouse: Boolean;
    procedure ChangedControlState(const ACurState: TxdGpUIState; const ACurPt: TPoint);
    procedure SetAllowMoveByMouse(const Value: Boolean);

    procedure CheckToDelBufBmpByTimer;
    procedure DoTimerToDelBufBitmap(Sender: TObject);
  published
    property Align;
    property Anchors;
    property Caption;
    property ShowHint;
    property Visible;
    property Enabled;
    property OnClick;
    property OnDblClick;
    property OnCanResize;
    property OnMouseDown;
    property OnMouseLeave;
    property OnMouseEnter;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;

    property AllowMoveByMouse: Boolean read FAllowMoveByMouse write SetAllowMoveByMouse;
  end;

implementation

uses
  uJxdGpSub;

{ TxdGraphicsBasic }

procedure TxdGraphicsBasic.ChangedControlState(const ACurState: TxdGpUIState; const ACurPt: TPoint);
var
  oldState: TxdGpUIState;
begin
  if Enabled and (FCurControlState <> ACurState) then
  begin
    oldState := FCurControlState;
    FCurControlState := ACurState;
    DoControlStateChanged( oldState, FCurControlState, ACurPt );
  end;
end;


procedure TxdGraphicsBasic.CheckToDelBufBmpByTimer;
begin
  if Assigned(FDelBufBmpTimer) then
  begin
    FDelBufBmpTimer.Enabled := False;
    FDelBufBmpTimer.Enabled := True;
  end
  else
  begin
    FDelBufBmpTimer := TTimer.Create( Self );
    FDelBufBmpTimer.Interval := 800;
    FDelBufBmpTimer.OnTimer := DoTimerToDelBufBitmap;
    FDelBufBmpTimer.Enabled := True;
  end;
end;

procedure TxdGraphicsBasic.ClearDrawRect;
begin
  FCurReDrawRect := MakeRect(0, 0, 0, 0);
end;

procedure TxdGraphicsBasic.Click;
begin
  inherited;

end;

procedure TxdGraphicsBasic.CMMouseEnter(var Message: TMessage);
var
  pt: TPoint;
begin
  inherited;
  GetCursorPos( pt );
  ChangedControlState( uiActive, ScreenToClient(pt) );
end;

procedure TxdGraphicsBasic.CMMouseLeave(var Message: TMessage);
var
  pt: TPoint;
begin
  inherited;
  if FCurControlState <> uiDown then
  begin
    GetCursorPos( pt );
    ChangedControlState( uiNormal, ScreenToClient(pt) );
  end;
end;

constructor TxdGraphicsBasic.Create(AOwner: TComponent);
begin
  inherited;
  FCurControlState := uiNormal;
  FDownPt.X := 0;
  FDownPt.Y := 0;
  FLButtonDown := False;
  FAllowMoveByMouse := False;
  FBufBmp := nil;
  FDelBufBmpTimer := nil;
end;

destructor TxdGraphicsBasic.Destroy;
begin
  FreeAndNil( FBufBmp );
  FreeAndNil( FDelBufBmpTimer );
  inherited;
end;

procedure TxdGraphicsBasic.DoControlStateChanged(const AOldState, ANewState: TxdGpUIState; const ACurPt: TPoint);
begin
  OutputDebugString( PChar('OldState: ' + IntToStr(Integer(AOldState)) +
    ';  NewState: ' + IntToStr(Integer(ANewState)) + 'Changed Point:' + IntToStr(ACurPt.X) + ', ' +  IntToStr(ACurPt.Y)) );
  Invalidate;
end;

procedure TxdGraphicsBasic.DoTimerToDelBufBitmap(Sender: TObject);
begin
  FreeAndNil( FDelBufBmpTimer );
  FreeAndNil( FBufBmp );
//  OutputDebugString( 'DoTimerToDelBufBitmap' );
end;

procedure TxdGraphicsBasic.DrawGraphics(const AGh: TGPGraphics);
begin

end;

function TxdGraphicsBasic.GetCurControlState: TxdGpUIState;
begin
  if Enabled then
    Result := FCurControlState
  else
    Result := uiNormal;
end;

procedure TxdGraphicsBasic.Invalidate;
begin
  FreeAndNil( FDelBufBmpTimer );
  FreeAndNil( FBufBmp );  
  inherited Invalidate;
end;

procedure TxdGraphicsBasic.InvalidateRect(const AR: TGPRectF);
var
  R: TRect;
begin
  FCurReDrawRect := GpRect(AR);
  if Assigned(Parent) then
  begin
    R.Left := Round(AR.X) + Left;
    R.Top := Round(AR.Y) + Top;
    R.Bottom := Round(R.Top) + Round(AR.Height);
    R.Right := Round(R.Left) + Round(AR.Width);
    FreeAndNil( FDelBufBmpTimer );
    FreeAndNil( FBufBmp );
    Windows.InvalidateRect( Parent.Handle, @R, False );
  end
  else
    Invalidate;
end;

function TxdGraphicsBasic.IsXolGpRect(const AR: TGPRect): Boolean;
begin
  if (FCurReDrawRect.X = 0) and (FCurReDrawRect.Y = 0) and (FCurReDrawRect.Width = 0) and (FCurReDrawRect.Height = 0) then
  begin
    Result := True;
    Exit;
  end;
  Result := ((AR.X + AR.Width) >= FCurReDrawRect.X) and (AR.X <= (FCurReDrawRect.X + FCurReDrawRect.Width)) and
            ((AR.Y + AR.Height) >= FCurReDrawRect.Y) and (AR.Y <= (FCurReDrawRect.Y + FCurReDrawRect.Height));
end;

procedure TxdGraphicsBasic.InvalidateRect(const AR: TGPRect);
var
  R: TRect;
begin
  FCurReDrawRect := AR;
  if Assigned(Parent) then
  begin
    R.Left := AR.X + Left;
    R.Top := AR.Y + Top;
    R.Bottom := R.Top + AR.Height;
    R.Right := R.Left + AR.Width;
    FreeAndNil( FDelBufBmpTimer );
    FreeAndNil( FBufBmp );
    Windows.InvalidateRect( Parent.Handle, @R, False );
  end
  else
    Invalidate;
end;

procedure TxdGraphicsBasic.SetAllowMoveByMouse(const Value: Boolean);
begin
  FAllowMoveByMouse := Value;
end;

procedure TxdGraphicsBasic.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(Parent) and not Parent.DoubleBuffered then
    Parent.DoubleBuffered := True;
end;

procedure TxdGraphicsBasic.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

procedure TxdGraphicsBasic.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if FAllowMoveByMouse and not FLButtonDown then
  begin
    FLButtonDown := True;
    FDownPt.X := Message.XPos;
    FDownPt.Y := Message.YPos;
  end;
  ChangedControlState( uiDown, Point(Message.XPos, Message.YPos) );
  inherited;
end;

procedure TxdGraphicsBasic.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if FLButtonDown then
    FLButtonDown := False;
  if PtInRect(ClientRect, Point(Message.XPos, Message.YPos)) then
    ChangedControlState( uiActive, Point(Message.XPos, Message.YPos) )
  else
    ChangedControlState( uiNormal, Point(Message.XPos, Message.YPos) );
  inherited;
end;

procedure TxdGraphicsBasic.WMMouseMove(var Message: TWMMouseMove);
begin
  if FAllowMoveByMouse and FLButtonDown then
  begin
    Left := Left + Message.XPos - FDownPt.X;
    Top := Top + Message.YPos - FDownPt.Y;
  end;
  inherited;
end;

procedure TxdGraphicsBasic.WMPaint(var Message: TWMPaint);
var
  G: TGPGraphics;
  memGh: TGPGraphics;
  R: TRect;
begin
  if (Message.DC <> 0) and not (csDestroying in ComponentState) then
  begin
    GetClipBox( Message.DC, R );
//    OutputDebugString( PChar('Left: ' + IntToStr(R.Left) + ' Rigth: ' + IntTostr(R.Right) +
//                             'Top: ' + IntToStr(R.Top) + 'Bottom: ' + IntToStr(R.Bottom))  );
    G := TGPGraphics.Create( Message.DC );
    try
      if not Assigned(FBufBmp) or (Integer(FBufBmp.GetWidth) <> Width) or (Integer(FBufBmp.GetHeight) <> Height) then
      begin
//        OutputDebugString( 'Create New Buffer bitmap to Draw' );
        FreeAndNil( FBufBmp );
        FBufBmp := TGPBitmap.Create( Width, Height );
        
        memGh := TGPGraphics.Create( FBufBmp );
        try
          DrawGraphics( memGh );
        finally
          memGh.Free;
        end;
      end;
      if not Enabled then
        ChangedBmpToGray( FBufBmp );
      G.DrawImage( FBufBmp, R.Left, R.Top, R.Left, R.Top, 
          R.Right - R.Left, R.Bottom - R.Top, UnitPixel );
//      FreeAndNil( FBufBmp );
      CheckToDelBufBmpByTimer;
    finally
      G.Free;
      FCurReDrawRect := MakeRect(0, 0, 0, 0);
    end;
  end;
end;

procedure TxdGraphicsBasic.WMSetText(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TxdGraphicsBasic.InvalidateRect(const AR: TRect);
begin
  FCurReDrawRect := MakeRect(AR.Left, AR.Top, AR.Right - AR.Left, AR.Bottom - AR.Top);
  if Assigned(Parent) then
  begin
    FreeAndNil( FDelBufBmpTimer );
    FreeAndNil( FBufBmp );
    Windows.InvalidateRect( Parent.Handle, @AR, False );
  end
  else
    Invalidate;
end;

end.
