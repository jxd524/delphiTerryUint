unit CustomGCButton;

interface

uses Windows, Messages, Classes, Controls, Forms, Graphics, StdCtrls,
  ExtCtrls, CommCtrl, Buttons;

type
  TCustomGCButton = class(TGraphicControl)
  private
    FGroupIndex: Integer;
    FGlyph: TBitmap;
    FDown: Boolean;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FSpacing: Integer;
    FMouseInControl: Boolean;
    procedure UpdateExclusive;
    procedure SetGlyph(Value: TBitmap);
    procedure SetDown(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetSpacing(Value: Integer);
    procedure UpdateTracking;
    procedure WMLButtonDblClk(var Message: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    FState: TButtonState;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetPalette: HPALETTE; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property MouseInControl: Boolean read FMouseInControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Action;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Caption;
    property Enabled;
    property Font;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses Consts, SysUtils, ActnList, ImgList;

{ TCustomKGButton }

procedure TCustomGCButton.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia; //! for lack of a better color
      Canvas.FillRect(Rect(0, 0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
    end;
  end;

begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if CheckDefaults or (Self.GroupIndex = 0) then
        Self.GroupIndex := GroupIndex;
      { Copy image from action's imagelist }
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;
end;

procedure TCustomGCButton.Click;
begin
  inherited Click;
end;

procedure TCustomGCButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TCustomGCButton;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TCustomGCButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        if (Action is TCustomAction) then
          TCustomAction(Action).Checked := False;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TCustomGCButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled and Visible and
      (Parent <> nil) and Parent.Showing then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TCustomGCButton.CMEnabledChanged(var Message: TMessage);
begin
  UpdateTracking;
  Repaint;
end;

procedure TCustomGCButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TCustomGCButton.CMMouseEnter(var Message: TMessage);
var
  NeedRepaint: Boolean;
begin
  inherited;
  { Don't draw a border if DragMode <> dmAutomatic since this button is meant to
    be used as a dock client. }
  NeedRepaint := not FMouseInControl and Enabled and (DragMode <> dmAutomatic) and (GetCapture = 0);
  if NeedRepaint and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    if Enabled then
      Repaint;
  end;
end;

procedure TCustomGCButton.CMMouseLeave(var Message: TMessage);
var
  NeedRepaint: Boolean;
begin
  inherited;
  NeedRepaint := FMouseInControl and Enabled and not FDragging;
  if NeedRepaint then
  begin
    FMouseInControl := False;
    if Enabled then
      Repaint;
  end;
end;

procedure TCustomGCButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

constructor TCustomGCButton.Create(AOwner: TComponent);
begin
  FGlyph := TBitmap.Create;
  FGlyph.Transparent := True;
  FGlyph.TransparentColor := clFuchsia;
  inherited Create(AOwner);
  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;
  Color := clBtnFace;
  FSpacing := 4;
end;

destructor TCustomGCButton.Destroy;
begin
  inherited Destroy;
  FGlyph.Free;
end;

function TCustomGCButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TSpeedButtonActionLink;
end;

function TCustomGCButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

procedure TCustomGCButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if not FDown then
    begin
      FState := bsDown;
      Invalidate;
    end;
    FDragging := True;
  end;
end;

procedure TCustomGCButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if not FDown then NewState := bsUp
    else NewState := bsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then NewState := bsExclusive else NewState := bsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end
  else if not FMouseInControl then
    UpdateTracking;
end;

procedure TCustomGCButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      { Redraw face in-case mouse is captured }
      FState := bsUp;
      FMouseInControl := False;
      if DoClick and not (FState in [bsExclusive, bsDown]) then
        Invalidate;
    end
    else
      if DoClick then
      begin
        SetDown(not FDown);
        if FDown then Repaint;
      end
      else
      begin
        if FDown then FState := bsExclusive;
        Repaint;
      end;
    if DoClick then Click;
    UpdateTracking;
  end;
end;

procedure TCustomGCButton.Paint;
begin
end;

procedure TCustomGCButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TCustomGCButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = bsUp then Invalidate;
      FState := bsExclusive
    end
    else
    begin
      FState := bsUp;
      Repaint;
    end;
    if Value then UpdateExclusive;
  end;
end;

procedure TCustomGCButton.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
  FGlyph.Transparent := True;
  FGlyph.TransparentColor := clFuchsia;
  Invalidate;
end;

procedure TCustomGCButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TCustomGCButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TCustomGCButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TCustomGCButton.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then
  begin
    GetCursorPos(P);
    FMouseInControl := not (FindDragTarget(P, True) = Self);
    if FMouseInControl then
      Perform(CM_MOUSELEAVE, 0, 0)
    else
      Perform(CM_MOUSEENTER, 0, 0);
  end;
end;

procedure TCustomGCButton.WMLButtonDblClk(var Message: TWMLButtonDown);
begin
  inherited;
  if FDown then DblClick;
end;

end.

