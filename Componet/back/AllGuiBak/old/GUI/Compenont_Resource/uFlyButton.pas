unit uFlyButton;

interface

uses Windows, SysUtils, Messages, Classes, Graphics,
  uFlyCustomButton;

type TFlyButtonStyle = (bsStandard, bsToolStyle, bsComboBox, bsSearch, bsSoftSearch, bsBitmapDropList);
type TGlphPos = (gpLeft, gpCenter);

type TFlyButton = class(TFlyCustomButton)
  private
    FStyle: TFlyButtonStyle;
    FBmp: TBitmap;
    FNormalBmp: TBitmap;
    FMouseInBmp: TBitmap;
    FMouseDownBmp: TBitmap;
    FSelectMouseInBmp: TBitmap;
    FGrayBmp: TBitmap;
    FGlphBmp: TBitmap;
    FGlphPos: TGlphPos;
    FGlphLeftSpace: Integer;
    FGlphTopOffset: Integer;
    FGlphRightSpace: Integer;
    FTextTopOffset: Integer;
    FTextLeftOffset: Integer;
    FSelect: Boolean;
    FAutoSize: Boolean;
    FShowText: Boolean;
    procedure SetTextLeftOffset(const Value: Integer);
    procedure SetTextTopOffset(const Value: Integer);
    procedure SetGlphTopOffset(const Value: Integer);
    procedure SetShowText(const Value: Boolean);
    procedure SetGlphLeftSpace(const Value: Integer);
    procedure SetGlphRightSpace(const Value: Integer);
    procedure SetGlphBmp(const Value: TBitmap);
    procedure SetGlphPos(const Value: TGlphPos);
    procedure SetGrayBmp(const Value: TBitmap);
    procedure SetBmpAutoSize(const Value: Boolean);
    procedure SetSelect(const Value: Boolean);
    procedure SetStyle(const Value: TFlyButtonStyle);
    procedure SetMouseDownBmp(const Value: TBitmap);
    procedure SetMouseInBmp(const Value: TBitmap);
    procedure SetNormalBmp(const Value: TBitmap);
    procedure SetSelectBmp(const Value: TBitmap);
    procedure DrawToolStyle(const ABmp: TBitmap; AIsDown: Boolean);
    procedure DrawStandard(ABmp: TBitmap; AIsDown: Boolean);
    procedure DrawComboBox(ABmp: TBitmap);
    procedure DrawSearch(ABmp: TBitmap; AIsDown: Boolean);
    procedure DrawSoftSearch(ABmp: TBitmap; AIsDown: Boolean);
    procedure DrawBitmapDropList(ABmp: TBitmap; AIsDown: Boolean);
    function GetPopupPoint(ALeft: Boolean = True): TPoint;
  protected
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct); override;
    procedure WMLButtonDown(var message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMSize(var message: TMessage); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TextTopOffset: Integer read FTextTopOffset write SetTextTopOffset default 4;
    property TextLeftOffset: Integer read FTextLeftOffset write SetTextLeftOffset default 4;
    property ShowText: Boolean read FShowText write SetShowText default false;
    property GlphBmp: TBitmap read FGlphBmp write SetGlphBmp;
    property GlphPos: TGlphPos read FGlphPos write SetGlphPos default gpLeft;
    property GlphLeftSpace: Integer read FGlphLeftSpace write SetGlphLeftSpace default 4;
    property GlphTopOffset: Integer read FGlphTopOffset write SetGlphTopOffset;
    property GlphRightSpace: Integer read FGlphRightSpace write SetGlphRightSpace default 4;
    property Select: Boolean read FSelect write SetSelect;
    property AutoSize: Boolean read FAutoSize write SetBmpAutoSize;
    property Style: TFlyButtonStyle read FStyle write SetStyle default bsStandard;
    property NormalBmp: TBitmap read FNormalBmp write SetNormalBmp;
    property MouseInBmp: TBitmap read FMouseInBmp write SetMouseInBmp;
    property MouseDownBmp: TBitmap read FMouseDownBmp write SetMouseDownBmp;
    property GrayBmp: TBitmap read FGrayBmp write SetGrayBmp;
    property SelectMouseInBmp: TBitmap read FSelectMouseInBmp write SetSelectBmp;
  end;

implementation
{$R ComboButton.RES}
{$R Search.RES}
uses Controls, StdCtrls, YYGraphUtil, Types;

{ TFlyButton }

constructor TFlyButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBmp := TBitmap.Create;
  FBmp.Transparent := True;
  FBmp.TransparentColor := clFuchsia;
  FNormalBmp := TBitmap.Create;
  FNormalBmp.Transparent := True;
  FNormalBmp.TransparentColor := clFuchsia;
  FMouseInBmp := TBitmap.Create;
  FMouseInBmp.Transparent := True;
  FMouseInBmp.TransparentColor := clFuchsia;
  FMouseDownBmp := TBitmap.Create;
  FMouseDownBmp.Transparent := True;
  FMouseDownBmp.TransparentColor := clFuchsia;
  FSelectMouseInBmp := TBitmap.Create;
  FSelectMouseInBmp.Transparent := True;
  FSelectMouseInBmp.TransparentColor := clFuchsia;
  FGrayBmp := TBitmap.Create;
  FAutoSize := False;
  FGlphBmp := TBitmap.Create;
  FGlphBmp.Transparent := True;
  FGlphBmp.TransparentColor := clFuchsia;
  FGlphPos := gpLeft;
  FGlphLeftSpace := 4;
  FGlphRightSpace := 4;
  FTextTopOffset := 4;
  FTextLeftOffset := 4;
  FShowText := False;
end;

destructor TFlyButton.Destroy;
begin
  FBmp.Free;
  FGrayBmp.Free;
  FNormalBmp.Free;
  FMouseInBmp.Free;
  FMouseDownBmp.Free;
  FSelectMouseInBmp.Free;
  FGlphBmp.Free;
  inherited;
end;

procedure TFlyButton.DrawBitmapDropList(ABmp: TBitmap; AIsDown: Boolean);
begin
  DrawStandard(ABmp, AIsDown);
end;

procedure TFlyButton.DrawComboBox(ABmp: TBitmap);
var
  SouR, DesR: TRect;
  nTop: Integer;
begin
  FNormalBmp.LoadFromResourceName(HInstance, 'ComboButton');
  with Abmp.Canvas do
  begin
    //Left
//    Draw(0, 0, FNormalBmp);
//    MoveTo(0, 0);
//    LineTo(Width, Height);
//    Exit;
    SouR := Rect(0, 0, 10, 22);
    DesR := SouR;
    CopyRect(DesR, FNormalBmp.Canvas, SouR);
    //Right
    SouR := Rect(10, 0, 27, 22);
    DesR := Rect(Self.Width - 17, 0, Self.Width, Self.Height);
    CopyRect(DesR, FNormalBmp.Canvas, SouR);
    //Center
    SouR := Rect(10, 0, 11, 22);
    DesR := Rect(10, 0, Self.Width - 17, 22);
    CopyRect(DesR, FNormalBmp.Canvas, SouR);
    //DrawGlph
    if not FGlphBmp.Empty then
    begin
      nTop := Abs(Height - FGlphBmp.Height) div 2;
      nTop := nTop + FGlphTopOffset;
      Draw(FGlphLeftSpace, nTop, FGlphBmp);
    end;
    //Draw Text
    DesR.Left := DesR.Left + FTextLeftOffset;
    DesR.Top := DesR.Top + FTextTopOffset;
    Brush.Style := bsClear;
    DrawText(Handle, PChar(Caption), Length(Caption), DesR, DT_SINGLELINE or DT_LEFT);
  end;
end;

procedure TFlyButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  IsDown: Boolean;
  dc: HDC;
begin
  dc := DrawItemStruct.hDC;
  IsDown := DrawItemStruct.itemState and ODS_SELECTED <> 0;
  FBmp.Width := Width;
  FBmp.Height := Height;
  case FStyle of
    bsStandard:       DrawStandard(FBmp, IsDown);
    bsToolStyle:      DrawToolStyle(FBmp, IsDown);
    bsComboBox:       DrawComboBox(FBmp);
    bsSearch:         DrawSearch(FBmp, IsDown);
    bsSoftSearch:     DrawSoftSearch(FBmp, IsDown);
    bsBitmapDropList: DrawBitmapDropList(FBmp, IsDown);
  end;
  BitBlt(dc, 0, 0, Width, Height, FBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TFlyButton.DrawSearch(ABmp: TBitmap; AIsDown: Boolean);
var
  ResBmp: TBitmap;
  ResR: TRect;
begin
  ResBmp := TBitmap.Create;
  try
    ResBmp.LoadFromResourceName(HInstance, 'Search');
    Width := ResBmp.Width;
    Height := ResBmp.Height div 3;
    ResR := Rect(0, 0, ResBmp.Width, ResBmp.Height);
    if AIsDown then
    begin
      ResR.Top := (ResBmp.Height div 3) * 2;
      ResR.Bottom := ResBmp.Height;
    end
    else if IsMouseInButton then
    begin
      ResR.Top := ResBmp.Height div 3;
      ResR.Bottom := (ResBmp.Height div 3) * 2;
    end
    else
    begin
      ResR.Bottom := ResBmp.Height div 3;
    end;
    ABmp.Canvas.CopyRect(Rect(0, 0, Width, Height), ResBmp.Canvas, ResR);
    if not FGlphBmp.Empty then
    begin
      ABmp.Canvas.Draw(FGlphLeftSpace, FGlphTopOffset, FGlphBmp);
    end;
  finally
    ResBmp.Free;
  end;
end;

procedure TFlyButton.DrawSoftSearch(ABmp: TBitmap; AIsDown: Boolean);
var
  ResBmp: TBitmap;
  ResR: TRect;
begin
  ResBmp := TBitmap.Create;
  try
    ResBmp.LoadFromResourceName(HInstance, 'SoftSearch');
    Width := ResBmp.Width;
    Height := ResBmp.Height div 3;
    ResR := Rect(0, 0, ResBmp.Width, ResBmp.Height);
    if AIsDown then
    begin
      ResR.Top := (ResBmp.Height div 3) * 2;
      ResR.Bottom := ResBmp.Height;
    end
    else if IsMouseInButton then
    begin
      ResR.Top := ResBmp.Height div 3;
      ResR.Bottom := (ResBmp.Height div 3) * 2;
    end
    else
    begin
      ResR.Bottom := ResBmp.Height div 3;
    end;
    ABmp.Canvas.CopyRect(Rect(0, 0, Width, Height), ResBmp.Canvas, ResR);
    if not FGlphBmp.Empty then
    begin
      ABmp.Canvas.Draw(FGlphLeftSpace, FGlphTopOffset, FGlphBmp);
    end;
  finally
    ResBmp.Free;
  end;
end;

procedure TFlyButton.DrawStandard(ABmp: TBitmap; AIsDown: Boolean);
  procedure DrawCenterGlph(ASouBmp, AResBmp: TBitmap);
  var
    nLeft, nTop: Integer;
  begin
    nLeft := abs(ASouBmp.Width - AResBmp.Width) div 2;
    nTop := abs(ASouBmp.Height - AResBmp.Height) div 2;
    ASouBmp.Canvas.Draw(nLeft, nTop, AResBmp);
  end;
  procedure DrawLeftGlph(ASouBmp, AResBmp: TBitmap);
  var
    nTop: Integer;
  begin
    nTop := abs(ASouBmp.Height - AResBmp.Height) div 2 + FGlphTopOffset;
    ASouBmp.Canvas.Draw(FGlphLeftSpace, nTop, AResBmp);
  end;

  procedure DrawGlphBmp(ASouBmp: TBitmap);
  var
    Bmp: TBitmap;
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.Width := FGlphBmp.Width;
      Bmp.Height := FGlphBmp.Height;
      Bmp.Assign(FGlphBmp);
      if not Enabled then
        GrayBitmap(Bmp);
      Bmp.TransparentColor := clFuchsia;
      Bmp.Transparent := True;
      if FGlphPos = gpLeft then
      begin
        DrawLeftGlph(ASouBmp, Bmp);
      end
      else if FGlphPos = gpCenter then
        DrawCenterGlph(ABmp, FGlphBmp);
    finally
      Bmp.Free;
    end;
  end;

  procedure StretchBmp(ABmp: TBitmap);
  var
    nWidth: Integer;
    tpBmp: TBitmap;
  begin
    nWidth := ABmp.Width div 8;
    tpBmp := TBitmap.Create;
    try
      tpBmp.Width := ABmp.Width;
      tpBmp.Height := ABmp.Height;
      tpBmp.Canvas.Draw(0, 0, ABmp);
      ABmp.Width := Width;
      ABmp.Height := Height;
      with ABmp.Canvas do
      begin
        CopyRect(Rect(0, 0, nWidth, Height),
          tpBmp.Canvas,
          Rect(0, 0, nWidth, tpBmp.Height));
        CopyRect(Rect(nWidth, 0, Width - nWidth, Height),
          tpBmp.Canvas,
          Rect(nWidth, 0, tpBmp.Width - nWidth, tpBmp.Height));
        CopyRect(Rect(Width - nWidth, 0, Width, Height),
          tpBmp.Canvas,
          Rect(tpBmp.Width - nWidth, 0, tpBmp.Width, tpBmp.Height));
      end;
    finally
      tpBmp.Free;
    end;
  end;
var
  R: TRect;
begin
  if AIsDown then
  begin
    if not FMouseDownBmp.Empty then
    begin
      ABmp.Width := FMouseDownBmp.Width;
      ABmp.Height := FMouseDownBmp.Height;
      ABmp.Canvas.Draw(0, 0, FMouseDownBmp);
    end;
  end
  else
  begin
    if IsMouseInButton then
    begin
      if not FMouseInBmp.Empty then
      begin
        ABmp.Width := FMouseInBmp.Width;
        ABmp.Height := FMouseInBmp.Height;
        ABmp.Canvas.Draw(0, 0, FMouseInBmp);
      end;
    end
    else //else if
    begin
      if not FNormalBmp.Empty then
      begin
        ABmp.Width := FNormalBmp.Width;
        ABmp.Height := FNormalBmp.Height;
        ABmp.Canvas.Draw(0, 0, FNormalBmp);
      end;
    end;
  end;

  if not FAutoSize then //¿≠…ÏÕº∆¨
  begin
    try
      StretchBmp(ABmp);
    except
    end;
  end;

  if not FGlphBmp.Empty then
  begin
    DrawGlphBmp(ABmp);
  end;

  if FShowText then
    with ABmp.Canvas do
    begin
      Font.Assign(Self.Font);
      if not Enabled then
        Font.Color := $00575757;
      Brush.Style := bsClear;
      if Assigned(FGlphBmp) and not FGlphBmp.Empty then
        R.Left := FGlphLeftSpace + FGlphBmp.Width + FGlphRightSpace
      else
        R.Left := FGlphLeftSpace + FGlphRightSpace;
      R.Top := 0;
      R.Right := Width;
      R.Bottom := Self.Height;
      DrawText(Handle, PChar(Caption), Length(Caption), R, DT_CENTER or DT_SINGLELINE or DT_VCENTER);
    end;
end;

procedure TFlyButton.DrawToolStyle(const ABmp: TBitmap; AIsDown: Boolean);
begin
  if FSelect then
  begin
    if IsMouseInButton then
    begin
      if not FSelectMouseInBmp.Empty then
        ABmp.Assign(FSelectMouseInBmp);
    end
    else
    begin
      if not FMouseDownBmp.Empty then
        ABmp.Assign(FMouseDownBmp);
    end;
  end
  else //else if
    if AIsDown then
    begin
      if not FMouseDownBmp.Empty then
        ABmp.Assign(FMouseDownBmp);
    end
    else //else if
      if IsMouseInButton then
      begin
        if not FMouseInBmp.Empty then
          ABmp.Assign(FMouseInBmp);
      end
      else //else if
      begin
        if not FNormalBmp.Empty then
          ABmp.Assign(FNormalBmp);
      end;
end;

function TFlyButton.GetPopupPoint(ALeft: Boolean = True): TPoint;
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  if ALeft then
    Result.X := R.Left
  else
    Result.X := R.Right;
  Result.Y := R.Bottom;
end;

procedure TFlyButton.SetBmpAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if Value then
    begin
      if not FNormalBmp.Empty then
      begin
        Width := FNormalBmp.Width;
        Height := FNormalBmp.Height;
      end
      else
        if not FMouseInBmp.Empty then
        begin
          Width := FMouseInBmp.Width;
          Height := FMouseInBmp.Height;
        end
        else
          if not FMouseDownBmp.Empty then
          begin
            Width := FMouseDownBmp.Width;
            Height := FMouseDownBmp.Height;
          end;
    end;
  end;
end;

procedure TFlyButton.SetGlphBmp(const Value: TBitmap);
begin
  FGlphBmp.Assign(Value);
  FGlphBmp.TransparentColor := clFuchsia;
  FGlphBmp.Transparent := True;
  Invalidate;
end;

procedure TFlyButton.SetGlphLeftSpace(const Value: Integer);
begin
  if FGlphLeftSpace <> Value then
  begin
    FGlphLeftSpace := Value;
    Invalidate;
  end;
end;

procedure TFlyButton.SetGlphPos(const Value: TGlphPos);
begin
  if FGlphPos <> Value then
  begin
    FGlphPos := Value;
    Invalidate;
  end;
end;

procedure TFlyButton.SetGlphRightSpace(const Value: Integer);
begin
  if FGlphRightSpace <> Value then
  begin
    FGlphRightSpace := Value;
    Invalidate;
  end;
end;

procedure TFlyButton.SetGlphTopOffset(const Value: Integer);
begin
  if FGlphTopOffset <> Value then
  begin
    FGlphTopOffset := Value;
    Invalidate;
  end;
end;

procedure TFlyButton.SetGrayBmp(const Value: TBitmap);
begin
  FGrayBmp.Assign(Value);
  Invalidate;
end;

procedure TFlyButton.SetMouseDownBmp(const Value: TBitmap);
begin
  FMouseDownBmp.Assign(Value);
  Invalidate;
end;

procedure TFlyButton.SetMouseInBmp(const Value: TBitmap);
begin
  FMouseInBmp.Assign(Value);
  Invalidate;
end;

procedure TFlyButton.SetNormalBmp(const Value: TBitmap);
begin
  FNormalBmp.Assign(Value);
  Invalidate;
end;

procedure TFlyButton.SetSelect(const Value: Boolean);
begin
  if FSelect <> Value then
  begin
    FSelect := Value;
    Invalidate;
  end;
end;

procedure TFlyButton.SetSelectBmp(const Value: TBitmap);
begin
  FSelectMouseInBmp.Assign(Value);
  Invalidate;
end;

procedure TFlyButton.SetShowText(const Value: Boolean);
begin
  if FShowText <> Value then
  begin
    FShowText := Value;
    Invalidate
  end;
end;

procedure TFlyButton.SetStyle(const Value: TFlyButtonStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    if FStyle = bsComboBox then
    begin
      Height := 22;
    end;
    Invalidate;
  end;
end;

procedure TFlyButton.SetTextLeftOffset(const Value: Integer);
begin
  if FTextLeftOffset <> Value then
  begin
    FTextLeftOffset := Value;
    Invalidate;
  end;
end;

procedure TFlyButton.SetTextTopOffset(const Value: Integer);
begin
  if FTextTopOffset <> Value then
  begin
    FTextTopOffset := Value;
    Invalidate;
  end;
end;

procedure TFlyButton.WMLButtonDown(var message: TWMLButtonDown);
var
  pt: TPoint;
  R: TRect;
begin
  if (FStyle = bsComboBox) and Assigned(PopupMenu) then
  begin
    inherited;
    pt := GetPopupPoint;
    PopupMenu.Popup(pt.X, pt.Y);
  end
  else if (FStyle = bsSearch) or (FStyle = bsSoftSearch)
    or (FStyle = bsBitmapDropList) and Assigned(PopupMenu) then
  begin
    R := Rect(0, 0, Width - 11, Height);
    pt.X := message.XPos;
    pt.Y := message.YPos;
    if not PtInRect(R, pt) then
    begin
      pt := GetPopupPoint(FStyle = bsSoftSearch);
      PopupMenu.Popup(pt.X, pt.Y);
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TFlyButton.WMSize(var message: TMessage);
begin
  inherited;
  Invalidate;
end;

end.

