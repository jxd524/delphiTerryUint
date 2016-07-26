{
单元名称: JxdComboBox
单元作者: 江晓德(jxd524@163.com)
网    址: www.geysoft.com
说    明: 自绘下拉列表,可使用图片代替所写的下拉形状
开始时间: 2007-11-2
修改时间: 2008-2-21 (最后修改) 
}
unit uJxdComboBox;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Messages, Windows, Graphics;

type
  TMouseState = (msUp, msDown, msExclusive);
  TTrigon = array[0..4] of TPoint;

  TJxdComboBox = class(TCustomComboBox)
  private
    { Private declarations }
    FMouseMoveInScrollRect: Boolean;
    FFrameColor: TColor;

    FScrollDownBkColor: TColor;
    FScrollActiveBkColor: TColor;
    FScrollBkColor: TColor;

    FTrigonColor: TColor;
    FTrigonLine: TColor;
    FTrigonArray: TTrigon;

    FScrollBmp: TBitmap;
    FScrollTransColor: TColor;
    FScrollBmpTopSpace: Integer;
    FScrollBmpLeftSpace: Integer;

    procedure SetFrameColor(const Value: TColor);
    procedure DrawClearRectagle(R: TRect);
    function GetSrollRect: TRect;
    procedure SetScrollBkColor(const Value: TColor);
    function GetMousePos: Boolean;
    procedure SetScrollAcitveBkColor(const Value: TColor);
    procedure SetScrollDownBkColor(const Value: TColor);
    procedure SetTrigonColor(const Value: TColor);
    procedure SetTrigonLine(const Value: TColor);
    procedure DrawTrigon;
    procedure DrawScrollBitmap;
    procedure SetScrollBitmap(const Value: TBitmap);
    procedure SetScrollBmpSpace(const Index, Value: Integer);
    procedure SetScrollBitmapTransColor(const Value: TColor);
  protected
    FSrollState: TMouseState;

    FScrollRect: TRect;
    property FMouseInSroll: Boolean read GetMousePos;

    procedure InvaliScroll;

    procedure OnNormalScroll(const ASrcollRect: TRect); virtual;
    procedure OnMouseInScroll(const AScrollRect: TRect); virtual;
    procedure OnMouseDownScroll(const AScrollRect: TRect); virtual;

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
    property FrameColor: TColor read FFrameColor write SetFrameColor default clNavy;
    property ScrollBkColor: TColor read FScrollBkColor write SetScrollBkColor;
    property ScrollActiveBkColor: TColor read FScrollActiveBkColor write SetScrollAcitveBkColor;
    property ScrollDownBKColor: TColor read FScrollDownBkColor write SetScrollDownBkColor;
    property ScrollBitmap: TBitmap read FScrollBmp write SetScrollBitmap;
    property ScrollBitmapTransColor: TColor read FScrollTransColor write SetScrollBitmapTransColor;
    property ScrollBitmapTopSpace: Integer index 0 read FScrollBmpTopSpace write SetScrollBmpSpace;
    property ScrollBitmapLeftSpace: Integer index 1 read FScrollBmpLeftSpace write SetScrollBmpSpace;
    property TrigonColor: TColor read FTrigonColor write SetTrigonColor default 0;
    property TrigonLine: TColor read FTrigonLine write SetTrigonLine default 0;

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
    property Text;
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

const CtScrollWidth = 20;
{ TJxdComboBox }


constructor TJxdComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFrameColor := clNavy;
  Style := csOwnerDrawVariable;

  FScrollBkColor := $FFFFFF;
  FScrollDownBkColor := $FFFFFF;
  FScrollActiveBkColor := $FFFFFF;

  FTrigonColor := 0;
  FTrigonLine := 0;

  FScrollTransColor := clFuchsia;
  FScrollBmpTopSpace := 0;
  FScrollBmpLeftSpace := 0;

  BevelInner := bvNone;
  BevelOuter := bvNone;
  BevelKind := bkFlat;

  FScrollBmp := TBitmap.Create;
  FScrollBmp.TransparentColor := FScrollTransColor;
  FScrollBmp.Transparent := True;
end;

destructor TJxdComboBox.Destroy;
begin
  if Assigned(FScrollBmp) then
    FScrollBmp.Free;
  inherited;
end;

procedure TJxdComboBox.DrawClearRectagle(R: TRect);
begin
  if Enabled then
    Canvas.Pen.Color := FFrameColor
  else
    Canvas.Pen.Color := $C8D0D4;

  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(R);
end;

procedure TJxdComboBox.DrawScrollBitmap;
var
  X, Y: Integer;
begin
  X := FScrollRect.Left + FScrollBmpLeftSpace;
  Y := FScrollRect.Top + FScrollBmpTopSpace;
  Canvas.Draw(X, Y, FScrollBmp);
end;

procedure TJxdComboBox.DrawTrigon;
var
  hRgnPoly: HRGN;
  Brush: TBrush;
  OldPenColor: TColor;
  TrigonArray: TTrigon;
begin
  TrigonArray := FTrigonArray;
  TrigonArray[0].X := TrigonArray[0].X + 1;
  TrigonArray[0].Y := TrigonArray[0].Y + 1;
  TrigonArray[1].X := TrigonArray[1].X - 1;
  TrigonArray[1].Y := TrigonArray[0].Y;
  TrigonArray[3].X := TrigonArray[0].X;
  TrigonArray[3].Y := TrigonArray[0].Y;

  Brush := TBrush.Create;
  if Enabled then
    Brush.Color := FTrigonColor
  else
    Brush.Color := $C8D0D4;
  hRgnPoly := CreatePolygonRgn(TrigonArray, 4, ALTERNATE);
  FillRgn(Canvas.Handle, hRgnPoly, Brush.Handle);
  Brush.Free;
  OldPenColor := Canvas.pen.Color;
  if Enabled then
    Canvas.pen.Color := FTrigonLine
  else
    Canvas.pen.Color := $C8D0D4;
  Polyline(Canvas.Handle, FTrigonArray, 4);
  Canvas.pen.Color := OldpenColor;
end;

function TJxdComboBox.GetMousePos: Boolean;
var
  pt: TPoint;
begin
  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  if PtInRect(FScrollRect, pt) then
    Result := True
  else
    Result := False;
end;

function TJxdComboBox.GetSrollRect: TRect;
var
  R: TRect;
begin
  R := GetClientRect;
  Result := R;
  Result.Left := R.Right - R.Left - CtScrollWidth + Canvas.Pen.Width;
  Result.Top := R.Top + Canvas.Pen.Width;
  Result.Right := Result.Right - Canvas.Pen.Width;
  Result.Bottom := Result.Bottom - Canvas.Pen.Width;
end;

procedure TJxdComboBox.InvaliScroll;
var
  R: TRect;
begin
  R := FScrollRect;
  InvalidateRect(Handle, @R, False);
end;

procedure TJxdComboBox.OnMouseDownScroll(const AScrollRect: TRect);
begin
  Canvas.Brush.Color := FScrollDownBkColor;
  Canvas.FillRect(AScrollRect);
end;

procedure TJxdComboBox.OnMouseInScroll(const AScrollRect: TRect);
begin
  Canvas.Brush.Color := FScrollActiveBkColor;
  Canvas.FillRect(AScrollRect);
end;

procedure TJxdComboBox.OnNormalScroll(const ASrcollRect: TRect);
begin
  Canvas.Brush.Color := FScrollBkColor;
  Canvas.FillRect(ASrcollRect);
end;

procedure TJxdComboBox.SetFrameColor(const Value: TColor);
begin
  FFrameColor := Value;
  Invalidate;
end;

procedure TJxdComboBox.SetScrollAcitveBkColor(const Value: TColor);
begin
  FScrollActiveBkColor := Value;
end;

procedure TJxdComboBox.SetScrollBitmap(const Value: TBitmap);
begin
  FScrollBmp.Assign(Value);
  FScrollBmp.TransparentColor := FScrollTransColor;
  FScrollBmp.Transparent := True;
end;

procedure TJxdComboBox.SetScrollBitmapTransColor(const Value: TColor);
begin
  FScrollTransColor := Value;
  if Assigned(FScrollBmp) then
    FScrollBmp.TransparentColor := FScrollTransColor;
  InvaliScroll;
end;

procedure TJxdComboBox.SetScrollBkColor(const Value: TColor);
begin
  FScrollBkColor := Value;
  Invalidate;
end;

procedure TJxdComboBox.SetScrollBmpSpace(const Index, Value: Integer);
begin
  if Index = 0 then
    FScrollBmpTopSpace := Value
  else if Index = 1 then
    FScrollBmpLeftSpace := Value;
  InvaliScroll;
end;

procedure TJxdComboBox.SetScrollDownBkColor(const Value: TColor);
begin
  FScrollDownBkColor := Value;
end;

procedure TJxdComboBox.SetTrigonColor(const Value: TColor);
begin
  FTrigonColor := Value;
  InvaliScroll;
end;

procedure TJxdComboBox.SetTrigonLine(const Value: TColor);
begin
  FTrigonLine := Value;
  InvaliScroll;
end;

procedure TJxdComboBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if FMouseInSroll then
  begin
    FSrollState := msDown;
    InvaliScroll;
  end
  else
    FMouseMoveInScrollRect := False;
end;

procedure TJxdComboBox.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  if FMouseInSroll then
  begin
    FSrollState := msUp;
    InvaliScroll;
  end
  else
  begin
    FMouseMoveInScrollRect := False;
    FSrollState := msExclusive;
    InvaliScroll;
  end;
end;

procedure TJxdComboBox.WMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseMoveInScrollRect := False;
  FSrollState := msExclusive;
  InvaliScroll;
end;

procedure TJxdComboBox.WMMouseMove(var Message: TMessage);
begin
  inherited;
  if FMouseInSroll then
  begin
    if not FMouseMoveInScrollRect then
    begin
      FMouseMoveInScrollRect := True;
      InvaliScroll;
    end;
  end
  else
  begin
    FMouseMoveInScrollRect := False;
    InvaliScroll;
  end;
end;

procedure TJxdComboBox.WMPaint(var Message: TWMPaint);
var
  R: TRect;
begin
  inherited;
  R := GetClientRect;
  DrawClearRectagle(R);
  if FSrollState = msDown then
    OnMouseDownScroll(FScrollRect)
  else if FMouseInSroll then
    OnMouseInScroll(FScrollRect)
  else
    OnNormalScroll(FScrollRect);
  if Assigned(FScrollBmp) and (not FScrollBmp.Empty) then
    DrawScrollBitmap
  else
    DrawTrigon;
  Canvas.Brush.Color := Color;
end;

procedure TJxdComboBox.WMSize(var Message: TWMSize);
begin
  inherited;
  FScrollRect := GetSrollRect;

  FTrigonArray[0].X := FScrollRect.Left + (FScrollRect.Right - FScrollRect.Left) div 5;
  FTrigonArray[0].Y := (FScrollRect.Bottom - FScrollRect.Top) div 4;
  FTrigonArray[1].X := FTrigonArray[0].X + (FScrollRect.Right - FScrollRect.Left) div 5 * 4;
  FTrigonArray[1].Y := (FScrollRect.Bottom - FScrollRect.Top) div 4;
  FTrigonArray[2].X := FTrigonArray[0].X + (FTrigonArray[1].X - FTrigonArray[0].X) div 2 ;
  FTrigonArray[2].Y := FTrigonArray[1].Y + FTrigonArray[1].Y * 2;
  FTrigonArray[3].X := FTrigonArray[0].X;
  FTrigonArray[3].Y := FTrigonArray[0].Y;
end;

end.
