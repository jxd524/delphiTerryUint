unit uJxdPanel;

interface

uses
  SysUtils, Classes, ExtCtrls, uBitmapHandle, Windows, Graphics, Messages;

type
  TJxdPanel = class(TCustomPanel)
  private
    FWinHRGN: HRGN;
    FIsRoundRectRgn: Boolean;
    FRoundRectWidth: Integer;
    FRoundRectHeight: Integer;
    procedure SetRoundRectRgn(const Value: Boolean);
    procedure SetRoundRect(const Index, Value: Integer);
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property RoundRectRgn: Boolean read FIsRoundRectRgn write SetRoundRectRgn;
    property RoundRectWidth: Integer index 0 read FRoundRectWidth write SetRoundRect;
    property RoundRectHeight: Integer index 1 read FRoundRectHeight write SetRoundRect;
    //Delphi panel property
  public
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


{ TJxdPanel }

constructor TJxdPanel.Create(AOwner: TComponent);
begin
  inherited;
  FWinHRGN := 0;
  FRoundRectWidth := 20;
  FRoundRectHeight := 20;
end;

destructor TJxdPanel.Destroy;
begin

  inherited;
end;

procedure TJxdPanel.SetRoundRect(const Index, Value: Integer);
var
  bChange: Boolean;
begin
  bChange := False;
  if Index = 0 then
  begin
    if Value <> FRoundRectWidth then
    begin
      FRoundRectWidth := Value;
      bChange := True;
    end;
  end
  else if Index = 1 then
  begin
    if Value <> FRoundRectHeight then
    begin
      FRoundRectHeight := Value;
      bChange := True;
    end;
  end;
  if FIsRoundRectRgn and bChange then
  begin
    FWinHRGN := CreateRoundRectRgn(0, 0, Width, Height, FRoundRectWidth, FRoundRectHeight);
    SetWindowRgn(Handle, FWinHRGN, True);
    Invalidate;
  end;
end;

procedure TJxdPanel.SetRoundRectRgn(const Value: Boolean);
begin
  if FIsRoundRectRgn <> Value then
  begin
    FIsRoundRectRgn := Value;
    if FIsRoundRectRgn then
      FWinHRGN := CreateRoundRectRgn(0, 0, Width, Height, FRoundRectWidth, FRoundRectHeight)
    else
      FWinHRGN := CreateRoundRectRgn(0, 0, Width, Height, 0, 0);
    SetWindowRgn(Handle, FWinHRGN, True);
    Invalidate;
  end;
end;

procedure TJxdPanel.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

procedure TJxdPanel.WMPaint(var Message: TWMPaint);
var
  Brush: TBrush;
begin
  Canvas.MoveTo(10, 10);
  Canvas.LineTo(100, 100);
  //Exit;
  //inherited;
  Brush := TBrush.Create;
  try
    Brush.Color := RGB(255, 20, 20);
    Brush.Style := bsSolid;
   // FillRgn(Canvas.Handle, FWinHRGN, Brush.Handle);
    FrameRgn(Canvas.Handle, FWinHRGN, Brush.Handle, 20, 20);
  finally
    Brush.Free;
  end;
end;

end.
