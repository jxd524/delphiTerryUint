unit uJxdGpComboBox;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Messages, Windows, Graphics, uJxdGpStyle;

type
  TxdComboBox = class(TCustomComboBox)
  protected
    procedure InvaliScroll;

    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseLeave(var Message: TMessage); message WM_MOUSELEAVE;
    procedure WMMouseMove(var Message: TMessage); message WM_MOUSEMOVE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }   
    property Text;
    property Align;
    property AutoComplete default True;
    property AutoCompleteDelay default 500;
    property AutoDropDown default False;
    property AutoCloseUp default False;
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Style default csOwnerDrawVariable;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
  end;

implementation

{ TJxdComboBox }

constructor TxdComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TxdComboBox.Destroy;
begin
  inherited;
end;

procedure TxdComboBox.InvaliScroll;
//var
//  R: TRect;
begin
//  R := FScrollRect;
//  InvalidateRect(Handle, @R, False);
end;

procedure TxdComboBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
//  if FMouseInSroll then
//  begin
//    FSrollState := msDown;
//    InvaliScroll;
//  end
//  else
//    FMouseMoveInScrollRect := False;
end;

procedure TxdComboBox.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
//  if FMouseInSroll then
//  begin
//    FSrollState := msActive;
//    InvaliScroll;
//  end
//  else
//  begin
//    FMouseMoveInScrollRect := False;
//    FSrollState := msNormal;
//    InvaliScroll;
//  end;
end;

procedure TxdComboBox.WMMouseLeave(var Message: TMessage);
begin
  inherited;
//  FMouseMoveInScrollRect := False;
//  FSrollState := msNormal;
//  InvaliScroll;
end;


procedure TxdComboBox.WMMouseMove(var Message: TMessage);
begin
  inherited;
//  if FMouseInSroll then
//  begin
//    if not FMouseMoveInScrollRect then
//    begin
//      FMouseMoveInScrollRect := True;
//      InvaliScroll;
//    end;
//  end
//  else
//  begin
//    FMouseMoveInScrollRect := False;
//    InvaliScroll;
//  end;
end;

procedure TxdComboBox.WMPaint(var Message: TWMPaint);
//var
//  R: TRect;
begin
  inherited;
//  R := GetClientRect;
//  DrawClearRectagle(R);
//  if FSrollState = msDown then
//    OnMouseDownScroll(FScrollRect)
//  else if FMouseInSroll then
//    OnMouseInScroll(FScrollRect)
//  else
//    OnNormalScroll(FScrollRect);
//  if not ( Assigned(FScrollBmp) and (not FScrollBmp.Empty) ) then
//    DrawTrigon;
//  Canvas.Brush.Color := Color;
end;

procedure TxdComboBox.WMSize(var Message: TWMSize);
begin
  inherited;
end;

end.
