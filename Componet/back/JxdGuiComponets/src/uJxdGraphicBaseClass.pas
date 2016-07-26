{
单元名称: uJxdGraphicBaseClass
单元作者: 江晓德(jxd524@163.com)
说    明: 无句柄自绘父类
开始时间: 2010-06-08
修改时间: 2010-06-08 (最后修改)
}
unit uJxdGraphicBaseClass;

interface

uses
  Windows,SysUtils, Classes, Controls, StdCtrls,Graphics,Messages, uJxdGuiStyle;

type
  TxdGraphicBase = class(TGraphicControl)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Repaint; override;
    procedure Click; override;
  protected
    //绘制接口
    procedure Paint; override;
    procedure DrawGraphiControl(ABufBmp: TBitmap); virtual;
    procedure InvalidateGraphicRect(ApRect: PRect);
    //当前控件状态
    function  GetCurControlState: TxdComponentState;
    procedure DoControlStateChanged(const AOldState, ANewState: TxdComponentState; const ACurPt: TPoint); virtual;
    //消息处理
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMSetText(var Message: TMessage); message WM_SETTEXT;
  private
    FCurControlState: TxdComponentState;
    FTransColor: TColor;
    FIsTransColor: Boolean;
    procedure ChangedControlState(const ACurState: TxdComponentState; const ACurPt: TPoint);
    procedure SetIsTransColor(const Value: Boolean);
    procedure SetTransColor(const Value: TColor);
  published
    property Align;
    property Anchors;
    property Font;
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

    property TransColor: TColor read FTransColor write SetTransColor default clFuchsia;
    property IsTransColor: Boolean read FIsTransColor write SetIsTransColor default True;
  end;
implementation

{ TxdGraphiBase }

procedure TxdGraphicBase.ChangedControlState(const ACurState: TxdComponentState; const ACurPt: TPoint);
var
  oldState: TxdComponentState;
begin
  if Enabled and (FCurControlState <> ACurState) then
  begin
    oldState := FCurControlState;
    FCurControlState := ACurState;
    DoControlStateChanged( oldState, FCurControlState, ACurPt );
  end;
end;

procedure TxdGraphicBase.Click;
begin
  inherited;
end;

procedure TxdGraphicBase.CMMouseEnter(var Message: TMessage);
var
  pt: TPoint;
begin
  inherited;
  GetCursorPos( pt );
  ChangedControlState( csActive, ScreenToClient(pt) );
end;

procedure TxdGraphicBase.CMMouseLeave(var Message: TMessage);
var
  pt: TPoint;
begin
  inherited;
  GetCursorPos( pt );
  ChangedControlState( csNormal, ScreenToClient(pt) );
end;

constructor TxdGraphicBase.Create(AOwner: TComponent);
var
  WinObj: TWinControl;
begin
  if not (AOwner is TWinControl) then
    raise TxdGuiComponetsError.Create( 'xdGraphi control''s parent must be win control' );
  inherited;
  WinObj := AOwner as TWinControl;
  if not WinObj.DoubleBuffered then
    WinObj.DoubleBuffered := True;
  FCurControlState := csNormal;
  ShowHint := True;
  FIsTransColor := True;
  FTransColor := clFuchsia;
end;

destructor TxdGraphicBase.Destroy;
begin

  inherited;
end;

procedure TxdGraphicBase.DoControlStateChanged(const AOldState, ANewState: TxdComponentState; const ACurPt: TPoint);
begin
  OutputDebugString( PChar('OldState: ' + IntToStr(Integer(AOldState)) +
    ';  NewState: ' + IntToStr(Integer(ANewState)) + 'Changed Point:' + IntToStr(ACurPt.X) + ', ' +  IntToStr(ACurPt.Y)) );
end;

procedure TxdGraphicBase.DrawGraphiControl(ABufBmp: TBitmap);
begin

end;

function TxdGraphicBase.GetCurControlState: TxdComponentState;
begin
  if Enabled then
    Result := FCurControlState
  else
    Result := csNormal;
end;

procedure TxdGraphicBase.InvalidateGraphicRect(ApRect: PRect);
var
  p: PRect;
  R: TRect;
begin
  if Parent = nil then
    Invalidate
  else
  begin
    if ApRect = nil then
    begin
      R := Rect(Left, Top, Left + Width, Top + Height);
      p := @R;
    end
    else
    begin
      p := ApRect;
      OffsetRect( p^, Left, Top );
    end;
    InvalidateRect( Parent.Handle, p, False );
  end;
end;

procedure TxdGraphicBase.Paint;
var
  BufferBmp: TBitmap;
//  r: TRect;
begin
//  GetClipBox( Canvas.Handle, r );
//  OutputDebugString( PChar('TxdGraphicBase.Paint: Left=' + IntToStr(r.Left) + '; Top=' + IntToStr(r.Top) + '; Right=' + IntToStr(r.Right) + '; Bottom=' + IntToStr(r.Bottom)) );
//  r := GetClientRect;
//  OutputDebugString( PChar('TxdGraphicBase.Paint: Left=' + IntToStr(r.Left) + '; Top=' + IntToStr(r.Top) + '; Right=' + IntToStr(r.Right) + '; Bottom=' + IntToStr(r.Bottom)) );


  BufferBmp := TBitmap.Create;
  try
    if (Width <= 0) or (Height <= 0) then Exit;
    BufferBmp.Width := Width;
    BufferBmp.Height := Height;

    BufferBmp.Transparent := IsTransColor;
    if IsTransColor then
    begin
      BufferBmp.TransparentColor := TransColor;
      BufferBmp.TransparentMode := tmFixed;
      BufferBmp.Canvas.Brush.Color := TransColor;
      BufferBmp.Canvas.FillRect( Rect(0, 0, BufferBmp.Width, BufferBmp.Height) );
    end;

    DrawGraphiControl( BufferBmp );
    Canvas.Draw( 0, 0, BufferBmp );
  finally
    FreeAndNil( BufferBmp );
  end;
end;

procedure TxdGraphicBase.Repaint;
begin
  Paint;
end;

procedure TxdGraphicBase.SetIsTransColor(const Value: Boolean);
begin
  if FIsTransColor <> Value then
  begin
    FIsTransColor := Value;
    Invalidate;
  end;
end;

procedure TxdGraphicBase.SetTransColor(const Value: TColor);
begin
  if FTransColor <> Value then
  begin
    FTransColor := Value;
    Invalidate;
  end;
end;

procedure TxdGraphicBase.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  ChangedControlState( csDown, Point(Message.XPos, Message.YPos) );
end;

procedure TxdGraphicBase.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if PtInRect(Rect(Left, Top, Left + Width, Top + Height), Point(Message.XPos, Message.YPos)) then
    ChangedControlState( csActive, Point(Message.XPos, Message.YPos) )
  else
    ChangedControlState( csNormal, Point(Message.XPos, Message.YPos) );
  inherited;
end;

procedure TxdGraphicBase.WMSetText(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

end.
