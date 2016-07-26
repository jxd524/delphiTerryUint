unit uJxdCustomTrackBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ComCtrls, Graphics, uJxdGuiDef;


type
  TTrackBarChangeEvent = procedure(Sender: TObject; AChangePos: Integer; var ACanChange: Boolean) of object;
  TrackPositionChangedEvent = procedure(Sender: TObject; ANewPosition: Integer) of object;

  TJxdCustomTrackBar = class(TCustomControl)
  private
    FOrientation: TTrackBarOrientation;
    FMax: Integer;
    FFrequency: Integer;
    FPosition: Integer;
    FSelStart: Integer;
    FSelEnd: Integer;
    FOnChange: TTrackBarChangeEvent;
    FSpacing: Integer;
    FTracking: Boolean;
    FXOffset, FYOffset: Integer;
    FMouseInControl: Boolean;
    FOnMovetoEnd: TNotifyEvent;
    FOnPositionChanged: TrackPositionChangedEvent;
    FChangeColor: TChangeToColor;
    procedure SetOnPositionChanged(const Value: TrackPositionChangedEvent);
    procedure SetOnMovetoEnd(const Value: TNotifyEvent);
    procedure SetOrientation(Value: TTrackBarOrientation);
    procedure SetPosition(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetFrequency(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetSelEnd(Value: Integer);
    function  CheckValue(Value: Integer): Integer;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CMFocusChanged(var Message: TMessage); message CM_FOCUSCHANGED;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure AdjustSliderRect;
  protected
    FIsDown:Boolean;
    FSliderSize: TSize;
    FSliderRect: TRect;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMoveToEnd;
    procedure Changed(AChangePos: Integer; var ACanChange: Boolean); dynamic;

    property MouseInControl: Boolean read FMouseInControl;

    procedure Paint; override;
    procedure DrawTrackBar; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ChangeColor: TChangeToColor read FChangeColor write FChangeColor;
    property Align;
    property Anchors;
    property BorderWidth;
    property Cursor;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Constraints;
    property Color default $00FDE8CD;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop default True;

    property Max: Integer read FMax write SetMax default 100;
    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation default trHorizontal;
    property Frequency: Integer read FFrequency write SetFrequency default 1;
    property Position: Integer read FPosition write SetPosition default 0;
    property SelEnd: Integer read FSelEnd write SetSelEnd default 0;
    property SelStart: Integer read FSelStart write SetSelStart default 0;

    property OnChange: TTrackBarChangeEvent read FOnChange write FOnChange;
    property OnMovetoEnd: TNotifyEvent read FOnMovetoEnd write SetOnMovetoEnd;
    property OnPositionChanged: TrackPositionChangedEvent read FOnPositionChanged write SetOnPositionChanged;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
  end;

implementation

{ TKKTrackBar }

procedure TJxdCustomTrackBar.AdjustSliderRect;
var
  Offset: Integer;
begin
  if FMax<=0 then Exit;
  if FOrientation = trHorizontal then
  begin
    Offset := (Width - FSliderSize.cx) * FPosition div FMax;
    FSliderRect := Rect(Offset, 0, Offset + FSliderSize.cx, Height);
  end
  else begin
    Offset := (Height - FSliderSize.cx) * FPosition div FMax;
    FSliderRect := Rect(Offset, 0, Offset + FSliderSize.cx, Width);
  end;
end;

procedure TJxdCustomTrackBar.Changed(AChangePos: Integer; var ACanChange: Boolean);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, AChangePos, ACanChange);
end;

function TJxdCustomTrackBar.CheckValue(Value: Integer): Integer;
begin
  if Value < 0 then
    Value := 0
  else if Value > FMax then
    Value := FMax;
  Result := Value;
end;

procedure TJxdCustomTrackBar.CMFocusChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJxdCustomTrackBar.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    Repaint;
  end;
end;

procedure TJxdCustomTrackBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl then
  begin
    FMouseInControl := False;
    Repaint;
  end;
end;

constructor TJxdCustomTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSliderSize.cx := 8;
  FSliderSize.cy := 14;
  DoubleBuffered := True;
  FTracking := False;
  FMax := 100;
  FFrequency := 1;
  FPosition := 0;
  FSelStart := 0;
  FSelEnd := 0;
  FSpacing := 5;
  TabStop := True;
  FIsDown := False;
  FMouseInControl := False;
  Color := $00FDE8CD;
  FChangeColor := TChangeToColor.Create( Self );
end;

procedure TJxdCustomTrackBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

destructor TJxdCustomTrackBar.Destroy;
begin
  FChangeColor.Free;  
  inherited;
end;

procedure TJxdCustomTrackBar.DoMoveToEnd;
begin
  if Assigned(FOnMoveToEnd) then
    FOnMoveToEnd(self);
end;

procedure TJxdCustomTrackBar.DrawTrackBar;
begin

end;

procedure TJxdCustomTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    FIsDown := True;
    SetFocus;
    if FOrientation = trHorizontal then
    begin
      if PtInRect(FSliderRect, Point(X, Y)) then
      begin
        FXOffset := X - FSliderRect.Left;
        FYOffset := Y - FSliderRect.Top;
        MouseCapture := True;
        FTracking := True;
      end
      else begin
        FXOffset := FSliderSize.cx div 2;
        FYOffset := FSliderSize.cy div 2;
        MouseCapture := True;
        FTracking := True;
        MouseMove(Shift, X, Y);
      end;
    end
    else
    begin
      if PtInRect(FSliderRect, Point(Y, X)) then
      begin
        FXOffset := Y - FSliderRect.Left;
        FYOffset := X - FSliderRect.Top;
        MouseCapture := True;
        FTracking := True;
      end
      else begin
        FXOffset := FSliderSize.cx div 2;
        FYOffset := FSliderSize.cy div 2;
        MouseCapture := True;
        FTracking := True;
        MouseMove(Shift, X, Y);
      end;
    end;
  end;
end;

procedure TJxdCustomTrackBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  CurPos: Integer;
  CanChange: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if FTracking then
  begin
    if FOrientation = trHorizontal then
      CurPos := (X - FXOffset) * FMax div (Width - FSliderSize.cx)
    else
      CurPos := FMax - ((Y - FXOffset) * FMax div (Height - FSliderSize.cx));
    CurPos := CheckValue(CurPos);
    if CurPos <> FPosition then
    begin
      CanChange := True;
      Changed(CurPos, CanChange);
      if CanChange then
      begin
        FPosition := CurPos;
        Invalidate;
      end;
    end;
  end;
end;

procedure TJxdCustomTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FIsDown := False;
  if FTracking then
  begin
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self, FPosition);
    FTracking := False;
    MouseCapture := False;
    Invalidate;
  end;
end;

procedure TJxdCustomTrackBar.Paint;
begin
  AdjustSliderRect;
  DrawTrackBar;
end;

procedure TJxdCustomTrackBar.SetFrequency(Value: Integer);
begin

end;

procedure TJxdCustomTrackBar.SetMax(Value: Integer);
begin
  FMax := Value;
  Invalidate;
end;

procedure TJxdCustomTrackBar.SetOnMovetoEnd(const Value: TNotifyEvent);
begin
  FOnMovetoEnd := Value;
end;

procedure TJxdCustomTrackBar.SetOnPositionChanged(
  const Value: TrackPositionChangedEvent);
begin
  FOnPositionChanged := Value;
end;

procedure TJxdCustomTrackBar.SetOrientation(Value: TTrackBarOrientation);
var
  Temp: Integer;
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if ((FOrientation = trHorizontal) and (Height > Width)) or
       ((FOrientation = trVertical) and (Height < Width)) then
    begin
      Temp := Width;
      Width := Height;
      Height := Temp;
    end;
    Invalidate;
  end;
end;

procedure TJxdCustomTrackBar.SetPosition(Value: Integer);
begin
  Value := CheckValue(Value);
  if not FTracking and (Value <> FPosition) then
  begin
    FPosition := Value;
    Invalidate;
    if FPosition = FMax then
    begin
      DoMoveToEnd;
    end;
  end;
end;

procedure TJxdCustomTrackBar.SetSelEnd(Value: Integer);
begin
  Value := CheckValue(Value);
  if FSelEnd <> Value then
  begin
    FSelEnd := Value;
    Invalidate;
  end;
end;

procedure TJxdCustomTrackBar.SetSelStart(Value: Integer);
begin
  Value := CheckValue(Value);
  if FSelStart <> Value then
  begin
    FSelStart := Value;
    Invalidate;
  end;
end;

procedure TJxdCustomTrackBar.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

procedure TJxdCustomTrackBar.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  Invalidate;
  inherited;
end;

end.

