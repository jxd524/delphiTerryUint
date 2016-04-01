unit uKKComboBox;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Messages, Windows, Graphics, uKKGuiDef;

type
  TMouseState = (msActive, msDown, msNormal);
  TTrigon = array[0..4] of TPoint;

  TKKComboBox = class(TCustomComboBox)
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
    FScrollBmpHeight: Integer;
    FScrollTransColor: TColor;
    FScrollBmpTopSpace: Integer;
    FScrollBmpLeftSpace: Integer;
    FScrollTransparent: Boolean;
    FChangeColor: TChangeToColor;
    FText: TWideCaption;

    procedure SetFrameColor(const Value: TColor);
    procedure DrawClearRectagle(R: TRect);
    function GetSrollRect: TRect;
    function GetScroolBmpRect(const AState: Integer): TRect;
    procedure SetScrollBkColor(const Value: TColor);
    function GetMousePos: Boolean;
    procedure SetScrollAcitveBkColor(const Value: TColor);
    procedure SetScrollDownBkColor(const Value: TColor);
    procedure SetTrigonColor(const Value: TColor);
    procedure SetTrigonLine(const Value: TColor);
    procedure DrawTrigon;
    procedure DrawScrollBitmap(const AState: Integer); //1: 正常; 2: 活动; 3: 按下
    procedure SetScrollBitmap(const Value: TBitmap);
    procedure SetScrollBmpSpace(const Index, Value: Integer);
    procedure SetScrollBitmapTransColor(const Value: TColor);
    procedure SetScrollBitmapTransparent(const Value: Boolean);
    procedure SetText(const Value: TWideCaption);
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
    property ScrollBitmapTransparent: Boolean read FScrollTransparent write SetScrollBitmapTransparent;
    property ScrollBitmapTopSpace: Integer index 0 read FScrollBmpTopSpace write SetScrollBmpSpace;
    property ScrollBitmapLeftSpace: Integer index 1 read FScrollBmpLeftSpace write SetScrollBmpSpace;
    property TrigonColor: TColor read FTrigonColor write SetTrigonColor default 0;
    property TrigonLine: TColor read FTrigonLine write SetTrigonLine default 0;
    property ChangeColor: TChangeToColor read FChangeColor write FChangeColor;
    property Text: TWideCaption read FText write SetText;

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

uses uKKBitmapHandle;

const CtScrollWidth = 20;
{ TJxdComboBox }


constructor TKKComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
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
  FScrollBmpHeight := 14;
  FScrollBmp.TransparentColor := FScrollTransColor;
  FScrollBmp.Transparent := True;

  FChangeColor := TChangeToColor.Create( Self );
end;

destructor TKKComboBox.Destroy;
begin
  FChangeColor.Free;
  if Assigned(FScrollBmp) then
    FScrollBmp.Free;
  inherited;
end;

procedure TKKComboBox.DrawClearRectagle(R: TRect);
begin
  if Enabled then
    if not FChangeColor.IsChange then
      Canvas.Pen.Color := ChangedRGB( FFrameColor, FChangeColor.ChangeToColor )
    else
      Canvas.Pen.Color := FFrameColor
  else
    Canvas.Pen.Color := $C8D0D4;

  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(R);
end;

procedure TKKComboBox.DrawScrollBitmap(const AState: Integer);
var
  X, Y: Integer;
  R: TRect;
  Bmp: TBitmap;
begin
  X := FScrollRect.Left + FScrollBmpLeftSpace;
  Y := FScrollRect.Top + FScrollBmpTopSpace;
  R := Rect( X, Y, X + FScrollBmp.Width, Y + FScrollBmpHeight );
  if not ChangeColor.IsChange then
  begin
    if ScrollBitmapTransparent then
    begin
      Canvas.Brush.Style := bsClear;
      SetBkMode(Canvas.Handle, TRANSPARENT);
      Canvas.BrushCopy( R, FScrollBmp, GetScroolBmpRect(AState), FScrollTransColor);
    end
    else
      Canvas.CopyRect( R, FScrollBmp.Canvas, GetScroolBmpRect(AState));
  end
  else
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.Assign( FScrollBmp );
      ChangeColor.ChangeBitmap( Bmp );
      if ScrollBitmapTransparent then
      begin
        Canvas.Brush.Style := bsClear;
        SetBkMode(Canvas.Handle, TRANSPARENT);
        Canvas.BrushCopy( R, Bmp, GetScroolBmpRect(AState), FScrollTransColor);
      end
      else
        Canvas.CopyRect( R, Bmp.Canvas, GetScroolBmpRect(AState));
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TKKComboBox.DrawTrigon;
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
    if not FChangeColor.IsChange then
      Canvas.Pen.Color := ChangedRGB( FTrigonColor, FChangeColor.ChangeToColor )
    else
      Canvas.Pen.Color := FTrigonColor
  else
    Brush.Color := $C8D0D4;
  hRgnPoly := CreatePolygonRgn(TrigonArray, 4, ALTERNATE);
  FillRgn(Canvas.Handle, hRgnPoly, Brush.Handle);
  Brush.Free;
  OldPenColor := Canvas.pen.Color;
  if Enabled then
    if not FChangeColor.IsChange then
      Canvas.Pen.Color := ChangedRGB( FTrigonLine, FChangeColor.ChangeToColor )
    else
      Canvas.Pen.Color := FTrigonLine
  else
    Canvas.pen.Color := $C8D0D4;
  Polyline(Canvas.Handle, FTrigonArray, 4);
  Canvas.pen.Color := OldpenColor;
end;

function TKKComboBox.GetMousePos: Boolean;
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

function TKKComboBox.GetScroolBmpRect(const AState: Integer): TRect;
begin
  case AState of
    1: Result := Rect(0, 0, FScrollBmp.Width, FScrollBmpHeight);
    2:
    begin
      if FScrollBmp.Height > FScrollBmpHeight * 2 then
        Result := Rect(0, FScrollBmpHeight + 1, FScrollBmp.Width, FScrollBmpHeight * 2 + 1)
    end;
    3:
    begin
      if FScrollBmp.Height > FScrollBmpHeight * 3 then
        Result := Rect(0, FScrollBmpHeight * 2 + 2, FScrollBmp.Width, FScrollBmpHeight * 3 + 2)
      else if FScrollBmp.Height > FScrollBmpHeight * 2 then
        Result := Rect(0, FScrollBmpHeight + 1, FScrollBmp.Width, FScrollBmpHeight * 2 + 1);
    end;
  end;
end;

function TKKComboBox.GetSrollRect: TRect;
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

procedure TKKComboBox.InvaliScroll;
var
  R: TRect;
begin
  R := FScrollRect;
  InvalidateRect(Handle, @R, False);
end;

procedure TKKComboBox.OnMouseDownScroll(const AScrollRect: TRect);
begin
  Canvas.Brush.Color := FScrollDownBkColor;
  Canvas.FillRect(AScrollRect);
  DrawScrollBitmap(3);
end;

procedure TKKComboBox.OnMouseInScroll(const AScrollRect: TRect);
begin
  Canvas.Brush.Color := FScrollActiveBkColor;
  Canvas.FillRect(AScrollRect);
  DrawScrollBitmap(2);
end;

procedure TKKComboBox.OnNormalScroll(const ASrcollRect: TRect);
begin
  Canvas.Brush.Color := FScrollBkColor;
  Canvas.FillRect(ASrcollRect);
  DrawScrollBitmap(1);
end;

procedure TKKComboBox.SetFrameColor(const Value: TColor);
begin
  FFrameColor := Value;
  Invalidate;
end;

procedure TKKComboBox.SetScrollAcitveBkColor(const Value: TColor);
begin
  FScrollActiveBkColor := Value;
end;

procedure TKKComboBox.SetScrollBitmap(const Value: TBitmap);
begin
  FScrollBmp.Assign(Value);
  if not FScrollBmp.Empty then
  begin
    DoubleBuffered := True;
    FScrollBmpHeight := CalcBmpSize(FScrollBmp.Canvas, FScrollBmp.Width div 2, 0,
                                    FScrollBmp.Height, ScrollBitmapTransColor, False);
  end;
  InvaliScroll;
end;

procedure TKKComboBox.SetScrollBitmapTransColor(const Value: TColor);
begin
  FScrollTransColor := Value;
  if not FScrollBmp.Empty then
    FScrollBmpHeight := CalcBmpSize(FScrollBmp.Canvas, FScrollBmp.Width div 2, 0,
                                    FScrollBmp.Height, ScrollBitmapTransColor, False);
  InvaliScroll;
end;

procedure TKKComboBox.SetScrollBitmapTransparent(const Value: Boolean);
begin
  FScrollTransparent := Value;
  InvaliScroll;
end;

procedure TKKComboBox.SetScrollBkColor(const Value: TColor);
begin
  FScrollBkColor := Value;
  Invalidate;
end;

procedure TKKComboBox.SetScrollBmpSpace(const Index, Value: Integer);
begin
  if Index = 0 then
    FScrollBmpTopSpace := Value
  else if Index = 1 then
    FScrollBmpLeftSpace := Value;
  InvaliScroll;
end;

procedure TKKComboBox.SetScrollDownBkColor(const Value: TColor);
begin
  FScrollDownBkColor := Value;
end;

procedure TKKComboBox.SetText(const Value: TWideCaption);
begin
  FText := Value;
  Invalidate;
end;

procedure TKKComboBox.SetTrigonColor(const Value: TColor);
begin
  FTrigonColor := Value;
  InvaliScroll;
end;

procedure TKKComboBox.SetTrigonLine(const Value: TColor);
begin
  FTrigonLine := Value;
  InvaliScroll;
end;

procedure TKKComboBox.WMLButtonDown(var Message: TWMLButtonDown);
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

procedure TKKComboBox.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  if FMouseInSroll then
  begin
    FSrollState := msActive;
    InvaliScroll;
  end
  else
  begin
    FMouseMoveInScrollRect := False;
    FSrollState := msNormal;
    InvaliScroll;
  end;
end;

procedure TKKComboBox.WMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseMoveInScrollRect := False;
  FSrollState := msNormal;
  InvaliScroll;
end;


procedure TKKComboBox.WMMouseMove(var Message: TMessage);
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

procedure TKKComboBox.WMPaint(var Message: TWMPaint);
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
  if not ( Assigned(FScrollBmp) and (not FScrollBmp.Empty) ) then
    DrawTrigon;
  Canvas.Brush.Color := Color;
end;

procedure TKKComboBox.WMSize(var Message: TWMSize);
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
