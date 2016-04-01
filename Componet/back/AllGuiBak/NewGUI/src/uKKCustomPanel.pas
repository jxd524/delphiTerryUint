unit uKKCustomPanel;

interface

uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Messages, Forms, Graphics;

type
  TKKCustomPanel = class(TCustomPanel)
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
  published  //导出属性
    property Align;
    property Alignment;
    property Anchors;
    property Enabled;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

implementation

{ TKKPanel }

procedure TKKCustomPanel.CMTextChanged(var Message: TMessage);
begin
  Message.Result := 1;
end;

constructor TKKCustomPanel.Create(AOwner: TComponent);
begin
  inherited;
  if (AOwner <> nil) and (AOwner is TForm) then
    FParentForm := (AOwner as TForm)
  else
    FParentForm := nil;
  FMoveForm := True;
end;

procedure TKKCustomPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TKKCustomPanel.DrawPanel(ABufBitmap: TBitmap);
begin

end;

procedure TKKCustomPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FMoveForm then
    MoveMainWindow;
end;

procedure TKKCustomPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TKKCustomPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TKKCustomPanel.MoveMainWindow;
begin
  if (FParentForm <> nil) and ( FParentForm.WindowState <> wsMaximized) then
  begin
    ReleaseCapture;
    PostMessage(FParentForm.Handle, WM_SYSCOMMAND, 61458, 0);
  end;
end;

procedure TKKCustomPanel.Paint;
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

procedure TKKCustomPanel.SetMoveForm(const Value: Boolean);
begin
  FMoveForm := Value;
end;

procedure TKKCustomPanel.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

end.
