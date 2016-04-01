{
类似千千的歌词秀控件
用法:
     1: LoadLyric(const ALyricName: string); 载入lrc文件
     2: ScrollLyricShow(ATime: Integer); 不断的调用些函数,让歌词秀动起来
作者: Terry(江晓德)
QQ:   67068633
Email:jxd524@163.com
}
unit JxdLyricShow;

interface

uses
  SysUtils, Windows, Classes, Controls, JxdParseLyric, Graphics, Messages;

const
  FontColorStep = 50;
  DelayChanage = 1800;

type
  TLyricShowKind = (lskHorizontal, lskSingleLIne, lskVertical);
  TuMouseButtons = (umbNULL, umbLeft, umbMiddle, umbRigth);
  TChangePositionEvent = procedure(Sender: TObject; MouseButton: TuMouseButtons;
    ATime: Integer) of object;

  TJxdLyricShow = class(TGraphicControl)
  private
    { Private declarations }
    FParseLyric: TParseLyric;
//    FLyricMaxLength: Integer; //歌词最长的宽度(以像素点宽度表示)
    FBitmap: TBitmap;
    FBlackBmp: TBitmap;
    FBlackColor: TColor; //背景颜色
    FNormalFontColor: TColor; //正常字体颜色
    FBrightColor: TColor; //滚动高亮颜色
    FBlendValue: Integer; //混合颜色
    FCurTime: Integer;
    FCurLineInfo: TLineInfo;
    FKind: TLyricShowKind;
    FLMouseBeginDragPoint: TPoint;
    FColorTable: array[0..FontColorStep - 1] of TColor;
    FOnChangePosition: TChangePositionEvent;
    function GetLyricMaxLength: Integer;

    procedure SetBackColor(const Value: TColor);
    procedure SetNormFontColor(const Value: TColor);
    procedure SetBrightColor(const Value: TColor);
    procedure InitBitmap;
    procedure DrawCursor;
    procedure SetLyricShowKind(const Value: TLyricShowKind);
    procedure SrcollSingLine(ATime: Integer);
    procedure SrcollHorizontal(ATime: Integer; ADrawCursor: Boolean = False);
    procedure SrcollVertical(ATime: Integer; ADrawCursor: Boolean = False);
    procedure TextOutLyric(AText: string; AColor: TColor; ARect: TRect; AFormat: DWORD);
  private
    FMouseButton: TuMouseButtons;
    FMouseMove: Boolean;
    function WidthToTime(AWidth: Integer): Integer;
    function HeightToTime(AHeight: Integer): Integer;
    procedure LMouseDragingLRC(X, Y: Integer);
    function TextHeight(const AText: string): Integer;

  protected
    { Protected declarations }
//    procedure MouseWheelRotated(var Message: TMessage); message CM_MOUSEWHEEL;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure Paint; override;
    procedure FileColorTable;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  public
    procedure LoadLyric(const ALyricName: string);
    procedure RevertAllValue;
    procedure ScrollLyricShow(ATime: Integer);
  published
    { Published declarations }
    property Font;
    property Align;
    property Anchors;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Constraints;
    property Visible;
    property PopupMenu;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property LyricShowKind: TLyricShowKind read FKind write SetLyricShowKind default lskVertical;
    property BlackColor: TColor read FBlackColor write SetBackColor stored True default clBlack;
    property NormalFontColor: TColor read FNormalFontColor write SetNormFontColor
      stored True default clHotLight;
    property BrightColor: TColor read FBrightColor write SetBrightColor
      stored True default clLime;
    property BlendValue: Integer read FBlendValue write FBlendValue default 60;
    property ParseLyric: TParseLyric read FParseLyric;
    property OnMouseChangedPosition: TChangePositionEvent read FOnChangePosition write FOnChangePosition;
    property LyricMaxLength: Integer read GetLyricMaxLength;
  end;

procedure BlendBitmap(BmpA, BmpB: TBitmap; ABlendWidth: Integer; AHorizontal: Boolean);

implementation

procedure BlendBitmap(BmpA, BmpB: TBitmap; ABlendWidth: Integer; AHorizontal: Boolean);
var
  PA, PB, PA1, PB1: PByteArray;

  I, J: Integer;
  K, W: Integer;
  function BlendValue(V1, V2: Byte; Step: Integer): Byte;
  begin
    Result := (V1 * Step div ABlendWidth) + (ABlendWidth - Step) * V2 div ABlendWidth;
  end;
begin
  if AHorizontal then
  begin
    W := BmpA.Width;
    if 2 * ABlendWidth > W then
      Exit;
    for I := 0 to BmpA.Height - 1 do
    begin
      PA := BmpA.ScanLine[I];
      PB := BmpB.ScanLine[I];
      for J := 0 to ABlendWidth - 1 do
      begin
        PB[J * 3 + 2] := BlendValue(PB[J * 3 + 2], PA[J * 3 + 2], J);
        PB[J * 3 + 1] := BlendValue(PB[J * 3 + 1], PA[J * 3 + 1], J);
        PB[J * 3] := BlendValue(PB[J * 3], PA[J * 3], J);

        K := W - J;
        PB[K * 3 + 2] := BlendValue(PB[K * 3 + 2], PA[K * 3 + 2], J);
        PB[K * 3 + 1] := BlendValue(PB[K * 3 + 1], PA[K * 3 + 1], J);
        PB[K * 3] := BlendValue(PB[K * 3], PA[K * 3], J);
      end;
    end;
  end
  else begin
    W := BmpA.Height;
    if 2 * ABlendWidth > W then
      Exit;
    for I := 0 to ABlendWidth - 1 do
    begin
      PA := BmpA.ScanLine[I];
      PB := BmpB.ScanLine[I];
      PA1 := BmpA.ScanLine[W - I - 1];
      PB1 := BmpB.ScanLine[W - I - 1];
      for J := 0 to BmpA.Width - 1 do
      begin
        PB[J * 3 + 2] := BlendValue(PB[J * 3 + 2], PA[J * 3 + 2], I);
        PB[J * 3 + 1] := BlendValue(PB[J * 3 + 1], PA[J * 3 + 1], I);
        PB[J * 3] := BlendValue(PB[J * 3], PA[J * 3], I);

        PB1[J * 3 + 2] := BlendValue(PB1[J * 3 + 2], PA1[J * 3 + 2], I);
        PB1[J * 3 + 1] := BlendValue(PB1[J * 3 + 1], PA1[J * 3 + 1], I);
        PB1[J * 3] := BlendValue(PB1[J * 3], PA1[J * 3], I);

      end;
    end;
  end;
end;



{ TFlyLyricShow }

procedure TJxdLyricShow.CMFontChanged(var Message: TMessage);
begin
  Canvas.Font.Assign(Font);
  FBitmap.Canvas.Font.Assign(Font);
end;

constructor TJxdLyricShow.Create(AOwner: TComponent);
begin
  inherited;
  FMouseMove := False;
  FParseLyric := TParseLyric.Create;
  FBitmap := TBitmap.Create;
  FBlackBmp := TBitmap.Create;
  FBlackBmp.PixelFormat := pf24bit;
  FBitmap.PixelFormat := pf24bit; //半透明支持
  FMouseButton := umbNULL;
  RevertAllValue;
  FileColorTable;
end;

destructor TJxdLyricShow.Destroy;
begin
  FParseLyric.Free;
  FBitmap.Free;
  FBlackBmp.Free;
  inherited;
end;

procedure TJxdLyricShow.DrawCursor;
var
  XOffset: Integer;
  R: TRect;
  TextStr: string;
  StrWidth, StrHeight: Integer;
  Color: TColor;
begin
  TextStr := FParseLyric.TimeToStr(FCurTime);
  TextStr := ChangeFileExt(TextStr, '');
  StrWidth := Canvas.TextWidth(TextStr);
  StrHeight := Canvas.TextHeight(TextStr);
  Color := FColorTable[FontColorStep div 2];

  case FKind of
    lskHorizontal, lskSingleLIne: begin
        XOffset := (Width - 7) div 2;
        with FBitmap.Canvas do
        begin
          Pen.Color := Color;
          MoveTo(XOffset, 2);
          LineTo(XOffset + 7, 2);
          MoveTo(XOffset, Height - 2);
          LineTo(XOffset + 7, Height - 2);
          MoveTo(XOffset + 3, 2);
          LineTo(XOffset + 3, Height - 2);
          R := Rect(XOffset + 5, Height - StrHeight - 1, XOffset + StrWidth + 5, Height - 1);
        end;
      end;
    lskVertical: begin
        XOffset := (Height - 7) div 2;
        with FBitmap.Canvas do
        begin
          Pen.Color := Color;
          MoveTo(1, XOffset);
          LineTo(Width - 4, XOffset);
          MoveTo(1, XOffset - 4);
          LineTo(1, XOffset + 4);
          MoveTo(Width - 4, XOffset - 4);
          LineTo(Width - 4, XOffset + 4);
          R := Rect(Width - StrWidth - 3, XOffset + 1, Width - 3, XOffset + StrHeight + 2);
        end;
      end;
  end;
  with FBitmap.Canvas do
  begin
    Font.Color := Color;
    DrawText(Handle, PChar(TextStr), Length(TextStr), R, DT_SINGLELINE or DT_RIGHT);
  end;

end;

procedure TJxdLyricShow.FileColorTable;
var
  I, Step: Integer;
  R1, G1, B1, R2, G2, B2: Byte;
  C1, C2: Cardinal;
  function BlendValue(V1, V2: Byte): Byte;
  begin
    Result := (V1 * I) div Step + (V2 * (Step - I) div Step);
  end;
begin
  C1 := ColorToRGB(FBrightColor);
  C2 := ColorToRGB(FNormalFontColor);
  R1 := GetRValue(C1); G1 := GetGValue(C1); B1 := GetBValue(C1);
  R2 := GetRValue(C2); G2 := GetGValue(C2); B2 := GetBValue(C2);
  Step := Length(FColorTable);
  for I := 0 to Step - 1 do
    FColorTable[I] := RGB(BlendValue(R1, R2), BlendValue(G1, G2), BlendValue(B1, B2));
end;

function TJxdLyricShow.GetLyricMaxLength: Integer;
var
  i: Integer;
  LineString: string;
  nTmpLength: Integer;
begin
  Result := -1;
  if (not Assigned(FParseLyric)) or (not FileExists(FParseLyric.LyricName)) then Exit;

  for i := 0 to FParseLyric.LyricLineCount - 1 do
  begin
    if not FParseLyric.GetLyricLineString(i, LineString) then break;
    nTmpLength := Canvas.TextWidth(LineString);
    if Result < nTmpLength then
      Result := nTmpLength;
  end;
end;

function TJxdLyricShow.HeightToTime(AHeight: Integer): Integer;
begin
  Result := (AHeight * FCurLineInfo.ScrollTime) div Canvas.TextHeight(FCurLineInfo.ScrollString + ' ');
end;

procedure TJxdLyricShow.InitBitmap;
begin
  FBitmap.Width := Width;
  FBitmap.Height := Height;
  FBitmap.Canvas.Brush.Style := bsClear;
  FBlackBmp.Width := Width;
  FBlackBmp.Height := Height;
  FBlackBmp.Canvas.Brush.Color := FBlackColor;
  FBlackBmp.Canvas.FillRect(ClientRect);
  Bitblt(FBitmap.Canvas.Handle, 0, 0, Width, Height, FBlackBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TJxdLyricShow.LMouseDragingLRC(X, Y: Integer);
begin
  if FKind = lskVertical then //如果是拖动垂直
  begin
    if Y > FLMouseBeginDragPoint.Y then
    begin
      Dec(FCurTime, HeightToTime(Y - FLMouseBeginDragPoint.Y));
      if FCurTime < 0 then
        FCurTime := 0
      else if FCurTime > FParseLyric.LYricLastTime then
        FCurTime := FParseLyric.LYricLastTime;

      SrcollVertical(FCurTime, True);
    end
    else if Y < FLMouseBeginDragPoint.Y then
    begin
      Inc(FCurTime, HeightToTime(FLMouseBeginDragPoint.Y - Y));
      if FCurTime < 0 then
        FCurTime := 0
      else if FCurTime > FParseLyric.LYricLastTime then
        FCurTime := FParseLyric.LYricLastTime;

      SrcollVertical(FCurTime, True);
    end;
  end else begin //水平拖动的
    if X > FLMouseBeginDragPoint.X then
    begin
      Dec(FCurTime, WidthToTime(X - FLMouseBeginDragPoint.X));
      if FCurTime < 0 then
        FCurTime := 0
      else if FCurTime > FParseLyric.LYricLastTime then
        FCurTime := FParseLyric.LYricLastTime;

      SrcollHorizontal(FCurTime, True);
    end
    else if X < FLMouseBeginDragPoint.X then
    begin
      Inc(FCurTime, WidthToTime(FLMouseBeginDragPoint.X - X));
      if FCurTime < 0 then
        FCurTime := 0
      else if FCurTime > FParseLyric.LYricLastTime then
        FCurTime := FParseLyric.LYricLastTime;

      SrcollHorizontal(FCurTime, True);
    end;
  end; // end if FKind = slVertical
end;

procedure TJxdLyricShow.LoadLyric(const ALyricName: string);
begin
  FParseLyric.LoadLyricFile(ALyricName);
end;

procedure TJxdLyricShow.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Enabled then
  begin
    case Button of
      mbLeft:
        begin
          FMouseButton := umbLeft;
          FLMouseBeginDragPoint := Point(X, Y);
        end;

      mbRight:
        begin
          FMouseButton := umbRigth;
        end;

      mbMiddle:
        begin
          FMouseButton := umbMiddle;
        end;
    end;
  end;
end;

procedure TJxdLyricShow.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  case FMouseButton of
    umbLeft:
      begin
        if (FLMouseBeginDragPoint.X <> X) or (FLMouseBeginDragPoint.Y <> Y) then
          FMouseMove := True;
        if FMouseMove then
        begin
          LMouseDragingLRC(X, Y);
          FLMouseBeginDragPoint := Point(X, Y);
        end;
      end;
    umbRigth:
      begin
        //
      end;
    umbMiddle:
      begin
        //
      end;
  end;
end;

procedure TJxdLyricShow.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if FMouseButton <> umbNULL then
  begin
    if FMouseButton = umbLeft then
    begin
      if FMouseMove then
        if Assigned(FOnChangePosition) then
          FOnChangePosition(Self, FMouseButton, FCurTime);
    end else
      if Assigned(FOnChangePosition) then
        FOnChangePosition(Self, FMouseButton, FCurTime);
    FMouseButton := umbNULL;
    FMouseMove := False;
  end;
end;

//procedure TFlyLyricShow.MouseWheelRotated(var Message: TMessage);
//begin
//  inherited;
//  FBlackColor := 0;
//end;


procedure TJxdLyricShow.Paint;
begin
  Bitblt(Canvas.Handle, 0, 0, Width, Height, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TJxdLyricShow.RevertAllValue;
begin
//  FBlackColor := 0;
//  FBlendValue := 10;
//  FNormalFontColor := $C08000;
//  FBrightColor := $FF00;
  ParseLyric.ClearAllValue;
  InitBitmap;
end;

procedure TJxdLyricShow.ScrollLyricShow(ATime: Integer);
begin
  if FParseLyric.LyricName = '' then Exit;
  
  if Enabled and (FMouseButton = umbNULL) then
    case FKind of
      lskHorizontal: SrcollHorizontal(ATime);
      lskVertical: SrcollVertical(ATime);
      lskSingleLIne: SrcollSingLine(ATime);
    end;
end;

procedure TJxdLyricShow.SetBackColor(const Value: TColor);
begin
  if Value <> FBlackColor then
  begin
    FBlackColor := Value;
    InitBitmap;
    ScrollLyricShow(FCurTime);
  end;
end;

procedure TJxdLyricShow.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if (FBitmap.Width <> Width) or (FBitmap.Height <> Height) then
    InitBitmap;
end;

procedure TJxdLyricShow.SetBrightColor(const Value: TColor);
begin
  if Value <> FBrightColor then
  begin
    FBrightColor := Value;
    FileColorTable;
  end;
end;

procedure TJxdLyricShow.SetLyricShowKind(const Value: TLyricShowKind);
begin
  if Value <> FKind then
  begin
    FKind := Value;
    InitBitmap;
  end;
end;

procedure TJxdLyricShow.SetNormFontColor(const Value: TColor);
begin
  if Value <> FNormalFontColor then
  begin
    FNormalFontColor := Value;
    FileColorTable;
  end;
end;

procedure TJxdLyricShow.SrcollHorizontal(ATime: Integer; ADrawCursor: Boolean);
var
  CurWidth: Integer;
  LineInfo: TLineInfo;
  CurOffset: Integer;
  LeftOffset: Integer;
  RightWidth: Integer;
  LeftRect, RightRect, CurRect: TRect;
  LeftLRC, RightLRC: string;
  I: Integer;
  ColorIndex: WORD;
begin
  if not FParseLyric.GetLineInfo(ATime, FCurLineInfo) then
    Exit;
  FCurTime := ATime;
  if FCurLineInfo.ScrollTime = 0 then
    Exit;
  FBitmap.Canvas.CopyRect(ClientRect, FBlackBmp.Canvas, ClientRect);
  if ATime > FCurLineInfo.StartTime then
  begin
    ColorIndex := (ATime - FCurLineInfo.StartTime) * FontColorStep div DelayChanage;
    if ColorIndex >= FontColorStep then
      ColorIndex := FontColorStep - 1;
  end else
    ColorIndex := 0;

  LeftLRC := '';
  RightLRC := '';
  CurWidth := Canvas.TextWidth(FCurLineInfo.ScrollString + '　');
  CurOffset := (ATime - FCurLineInfo.StartTime) * CurWidth div FCurLineInfo.ScrollTime;
  LeftOffset := (Width div 2) - CurOffset;

  LeftRect := Rect(0, 0, LeftOffset, Height);
  CurRect := Rect(LeftOffset, 0, LeftOffset + CurWidth, Height);
  RightRect := Rect(CurRect.Right, 0, Width, Height);

  I := FCurLineInfo.Index;
  while LeftOffset > 0 do
  begin
    if not FParseLyric.GetLineInfoByIndex(I - 1, LineInfo) then
      Break
    else begin
      if I = FCurLineInfo.Index then //如果是第一行
      begin
        TextOutLyric(LineInfo.ScrollString + '　', FColorTable[FontColorStep - ColorIndex - 1],
          LeftRect, DT_RIGHT);
        Dec(LeftRect.Right, Canvas.TextWidth(LineInfo.ScrollString + '　'));
      end else
        LeftLRC := LineInfo.ScrollString + '　' + LeftLRC;

      Dec(I);
      Dec(LeftOffset, Canvas.TextWidth(LineInfo.ScrollString + '　'));
    end;
  end;

  RightWidth := (RightRect.Right - RightRect.Left);
  I := FCurLineInfo.Index;
  while RightWidth > 0 do
  begin
    if not FParseLyric.GetLineInfoByIndex(I + 1, LineInfo) then
      Break
    else begin
      Inc(I);
      RightLRC := RightLRC + LineInfo.ScrollString + '　';
      Dec(RightWidth, Canvas.TextWidth(LineInfo.ScrollString + '　'));
    end;
  end;

  if LeftOffset > 0 then
    LeftRect.Left := LeftOffset;
  if LeftLRC <> '' then
    TextOutLyric(LeftLRC, FNormalFontColor, LeftRect, DT_RIGHT);
  TextOutLyric(FCurLineInfo.ScrollString + '　', FColorTable[ColorIndex], CurRect, 0);

  if RightLRC <> '' then
    TextOutLyric(RightLRC, FNormalFontColor, RightRect, DT_LEFT);

  if FBlendValue > 0 then
    BlendBitmap(FBlackBmp, FBitmap, FBlendValue, True);
  if ADrawCursor then
    DrawCursor;
  Bitblt(Canvas.Handle, 0, 0, Width, Height, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TJxdLyricShow.SrcollSingLine(ATime: Integer);
var
  CurWidth: Integer;
begin
  if not FParseLyric.GetLineInfo(ATime, FCurLineInfo) then
    Exit;
  FCurTime := ATime;
  FBitmap.Canvas.CopyRect(ClientRect, FBlackBmp.Canvas, ClientRect);
  CurWidth := Canvas.TextWidth(FCurLineInfo.ScrollString);
  if CurWidth > Width then
    TextOutLyric(FCurLineInfo.ScrollString, FBrightColor, ClientRect, DT_LEFT)
  else
    TextOutLyric(FCurLineInfo.ScrollString, FBrightColor, ClientRect, DT_CENTER);

  Bitblt(Canvas.Handle, 0, 0, Width, Height, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TJxdLyricShow.SrcollVertical(ATime: Integer; ADrawCursor: Boolean);
var
  CurHeight: Integer;
  LineInfo: TLineInfo;
  CurOffset: Integer;
  UpOffset: Integer;
  DownHeight: Integer;
  UpRect, DownRect, CurRect: TRect;
  UpLRC, DownLRC: string;
  I: Integer;
  ColorIndex: WORD;
  TempLRC: string;
begin
  if not FParseLyric.GetLineInfo(ATime, FCurLineInfo) then
    Exit;
  if FCurLineInfo.ScrollTime = 0 then
    Exit;

  FCurTime := ATime;
  FBitmap.Canvas.CopyRect(ClientRect, FBlackBmp.Canvas, ClientRect);
  if ATime > FCurLineInfo.StartTime then
  begin
    ColorIndex := (ATime - FCurLineInfo.StartTime) * FontColorStep div DelayChanage;
    if ColorIndex >= FontColorStep then
      ColorIndex := FontColorStep - 1;
  end else
    ColorIndex := 0;

  UpLRC := '';
  DownLRC := '';
  CurHeight := TextHeight(FCurLineInfo.ScrollString);

  CurOffset := (ATime - FCurLineInfo.StartTime) * CurHeight div FCurLineInfo.ScrollTime;
  UpOffset := (Height div 2) - CurOffset;

  UpRect := Rect(0, 0, Width, UpOffset);
  CurRect := Rect(0, UpOffset, Width, UpOffset + CurHeight);
  DownRect := Rect(0, CurRect.Bottom, Width, Height);

  I := FCurLineInfo.Index;
  while UpOffset > 0 do
  begin
    if not FParseLyric.GetLineInfoByIndex(I - 1, LineInfo) then
      Break
    else begin
      if I = FCurLineInfo.Index then //如果是第一行
      begin
        UpRect.Top := UpRect.Bottom - TextHeight(LineInfo.ScrollString);
        TextOutLyric(LineInfo.ScrollString, FColorTable[FontColorStep - ColorIndex - 1],
          UpRect, DT_CENTER);
      end else begin
        UpLRC := LineInfo.ScrollString;
        if UpOffset > 0 then
          UpRect.Top := UpOffset - TextHeight(LineInfo.ScrollString);
        TextOutLyric(UpLRC, FNormalFontColor, UpRect, DT_CENTER);
      end;
      Dec(I);
      Dec(UpOffset, TextHeight(LineInfo.ScrollString));
    end;
  end;

  DownHeight := (DownRect.Bottom - DownRect.Top);
  I := FCurLineInfo.Index;
  while DownHeight > 0 do
  begin
    if not FParseLyric.GetLineInfoByIndex(I + 1, LineInfo) then
      Break
    else begin
      Inc(I);
      TempLRC := DownLRC;
      DownLRC := LineInfo.ScrollString;
      TextOutLyric(DownLRC, FNormalFontColor, DownRect, DT_CENTER);
      DownRect.Top := DownRect.Top + TextHeight(LineInfo.ScrollString);
      Dec(DownHeight, TextHeight(LineInfo.ScrollString));
    end;
  end;

  TextOutLyric(FCurLineInfo.ScrollString, FColorTable[ColorIndex], CurRect, DT_CENTER);

  if FBlendValue > 0 then
    BlendBitmap(FBlackBmp, FBitmap, FBlendValue, False);
  if ADrawCursor then
    DrawCursor;
  Bitblt(Canvas.Handle, 0, 0, Width, Height, FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

function TJxdLyricShow.TextHeight(const AText: string): Integer;
var
  Size: TSize;
begin
  GetTextExtentPoint32(Canvas.Handle, PChar(AText), Length(AText), Size);
  Result := Size.cy + 5;
end;

procedure TJxdLyricShow.TextOutLyric(AText: string; AColor: TColor;
  ARect: TRect; AFormat: DWORD);
begin
  with FBitmap.Canvas do
  begin
    Font.Color := AColor;
    case FKind of
      lskHorizontal, lskSingleLine:
        DrawText(Handle, PChar(AText), Length(AText), ARect, DT_VCENTER or DT_SINGLELINE or AFormat);
      lskVertical:
        DrawText(Handle, PChar(AText), Length(AText), ARect, DT_SINGLELINE or AFormat);
    end;
  end;
end;

function TJxdLyricShow.WidthToTime(AWidth: Integer): Integer;
begin
  Result := (AWidth * FCurLineInfo.ScrollTime) div Canvas.TextWidth(FCurLineInfo.ScrollString + '　');
end;

end.

