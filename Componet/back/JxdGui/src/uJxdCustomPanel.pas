unit uJxdCustomPanel;

interface

uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Messages, Forms, Graphics;

type
  TJxdCustomPanel = class(TCustomPanel)
  private
    FMoveForm: Boolean;
    procedure MoveMainWindow;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED; //不处理字体
    procedure SetMoveForm(const Value: Boolean);    
  protected
    FParentForm: TForm;
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    //派生类需要完成的接口
    procedure DrawPanel(ABufBitmap: TBitmap); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoMoveForm: Boolean read FMoveForm write SetMoveForm default True;
    property DockManager;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property VerticalAlignment;
    property Visible;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

{ TKKPanel }

procedure TJxdCustomPanel.CMTextChanged(var Message: TMessage);
begin
  Message.Result := 1;
end;

constructor TJxdCustomPanel.Create(AOwner: TComponent);
begin
  inherited;
  if (AOwner <> nil) and (AOwner is TForm) then
    FParentForm := (AOwner as TForm)
  else
    FParentForm := nil;
  FMoveForm := True;
end;

procedure TJxdCustomPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TJxdCustomPanel.DrawPanel(ABufBitmap: TBitmap);
begin

end;

procedure TJxdCustomPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FMoveForm then
    MoveMainWindow;
end;

procedure TJxdCustomPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TJxdCustomPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TJxdCustomPanel.MoveMainWindow;
begin
  if (FParentForm <> nil) and ( FParentForm.WindowState <> wsMaximized) then
  begin
    ReleaseCapture;
    PostMessage(FParentForm.Handle, WM_SYSCOMMAND, 61458, 0);
  end;
end;

procedure TJxdCustomPanel.Paint;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Height := Height;
    Bmp.Width := Width;
    DrawPanel(bmp);
  finally
    Canvas.Draw(0, 0, Bmp);
    Bmp.Free;
  end;
end;

procedure TJxdCustomPanel.SetMoveForm(const Value: Boolean);
begin
  FMoveForm := Value;
end;

procedure TJxdCustomPanel.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

end.
