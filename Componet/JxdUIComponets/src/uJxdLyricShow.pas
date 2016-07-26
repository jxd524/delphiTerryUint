unit uJxdLyricShow;

interface

uses
  Windows, Messages, Classes, uJxdLyricParse, Graphics, sysUtils, uJxdDrawSub, uJxdWndHook;

type
  {$M+}
  TLyricShowStyle = (lskHorizontal, lskVertical);
  TxdLyricShow = class
  public
    constructor Create;
    destructor  Destroy; override;

    function  BeginLyricShow(const AFileName: string): Boolean;
    procedure ScrollLyricShow(const ATime: Cardinal);
    procedure EndLyricShow;

    procedure ReChangedFontInfo;
  protected
    FActive: Boolean;
    FLyrParse: TxdLyricParse;
    FOwnerSize: TRect;
//    FBackBmp,
    FCurBmp: TBitmap;
    FCurTime: Integer; //当前时间
    FCurFontHeight: Integer; //当前字体高度
    FCurWidth, FCurHeight: Integer; //指定宽度与高度
    FBrightColorTable: array of TColor;
    FBlendColorTable: array of TColor;

    procedure HookDrawWnd;
    procedure CalcSize;
    procedure FillBrighColorTable;
    procedure FillBlendColorTable;

    procedure DrawVerticalLyricFrame;
    procedure DrawHorizontalLyricFram;

    procedure BlendBitmap;

    procedure OnDrawWndMessage(AwParam, AlParam: Integer);

    function  TextWidth(const AText: string): Integer; inline;
    function  GetBrightColor(const AStartTime: Integer): TColor;
    procedure TextOutLyric(const AText: string; const AColor: TColor; ARect: TRect; const AFormat: DWORD);
  private
    FDrawOwerHandle: Cardinal;
    FBackColor: TColor;
    FFontBrightColorFrom: TColor;
    FFontNormalColor: TColor;
    FColorTableStep: Integer;
    FFontBrightColorTo: TColor;
    FBlendTableStep: Integer;
    FLyricShowStyle: TLyricShowStyle;
    procedure SetDrawOwnerHandle(const Value: Cardinal);
    procedure SetBackColor(const Value: TColor);
    procedure SetFontBrightColorFrom(const Value: TColor);
    procedure SetFontNormalColor(const Value: TColor);
    procedure SetColorTableStep(const Value: Integer);
    procedure SetFontBrightColorTo(const Value: TColor);
    procedure SetBlendTableStep(const Value: Integer);
    procedure SetLyricShowStyle(const Value: TLyricShowStyle);
  published
    property Active: Boolean read FActive;
    property CurBitmap: TBitmap read FCurBmp;
    property DrawOwnerHandle: Cardinal read FDrawOwerHandle write SetDrawOwnerHandle;

    property LyrParse: TxdLyricParse read FLyrParse;
    property LyricShowStyle: TLyricShowStyle read FLyricShowStyle write SetLyricShowStyle;

    property ColorTableStep: Integer read FColorTableStep write SetColorTableStep;  //高亮显示数据
    property BlendTableStep: Integer read FBlendTableStep write SetBlendTableStep;  //混合步骤
    property FontNormalColor: TColor read FFontNormalColor write SetFontNormalColor;//正常字体颜色
    property FontBrightColorFrom: TColor read FFontBrightColorFrom write SetFontBrightColorFrom; //高亮字体开始颜色
    property FontBrightColorTo: TColor read FFontBrightColorTo write SetFontBrightColorTo; //高亮字体结束颜色
    property BackColor: TColor read FBackColor write SetBackColor; //背景颜色
  end;
  {$M-}
implementation

{ TxdLyricShow }

procedure GradientColor(var AColorTable: array of TColor; const AColorFrom, AColorTo: TColor);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb: Real;
  i: Word;
  Steps: Integer;
  FromColor,ToColor: Longint;
begin
  Steps := Length( AColorTable );
  if Steps = 0 then Exit;  

  FromColor := ColorToRGB( AColorFrom );
  ToColor := ColorToRGB( AColorTo );

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;

  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  for i := 0 to steps - 1 do
  begin
    endr := startr + Round(rstepr * i);
    endg := startg + Round(rstepg * i);
    endb := startb + Round(rstepb * i);
    AColorTable[i] := endr + (endg shl 8) + (endb shl 16);
  end;
end;

procedure TxdLyricShow.BlendBitmap;
var
  i, j, nRigth, nIndex: Integer;
  p: PRGBTriple;
  color: TColor;
begin
  if FBlendTableStep <= 0 then Exit;
  case LyricShowStyle of
    lskHorizontal:
    begin
      //水平混合
      nIndex := 0;
      nRigth := FCurBmp.Width - FBlendTableStep;
      for i := 0 to FCurBmp.Height - 1 do
      begin
        p := FCurBmp.ScanLine[i];
        for j := 0 to FCurBmp.Width - 1 do
        begin
          if j < FBlendTableStep then
            nIndex := j
          else if j > nRigth then
            nIndex := FBlendTableStep - (j - nRigth)
          else
          begin
            Inc( p );
            Continue;
          end;

          color := RGB( p^.rgbtRed, p^.rgbtGreen, p^.rgbtBlue );
          if color = FFontNormalColor then
          begin
            p^.rgbtRed := GetRValue( FBlendColorTable[nIndex] );
            p^.rgbtGreen := GetGValue( FBlendColorTable[nIndex] );
            p^.rgbtBlue := GetBValue( FBlendColorTable[nIndex] );
          end;
          Inc(P);
        end;
      end;
    end;
    lskVertical:
    begin
      //垂直混合
      for i := 0 to FBlendTableStep - 1 do
      begin
        p := FCurBmp.ScanLine[i];
        for j := 0 to FCurWidth - 1 do
        begin
          color := RGB( p^.rgbtRed, p^.rgbtGreen, p^.rgbtBlue );
          if color = FFontNormalColor then
          begin
            p^.rgbtRed := GetRValue( FBlendColorTable[i] );
            p^.rgbtGreen := GetGValue( FBlendColorTable[i] );
            p^.rgbtBlue := GetBValue( FBlendColorTable[i] );
          end;
          Inc(P);
        end;
      end;

      nIndex := FBlendTableStep - 1;
      for i := FCurHeight - FBlendTableStep to FCurHeight - 1 do
      begin
        p := FCurBmp.ScanLine[i];
        for j := 0 to FCurWidth - 1 do
        begin
          color := RGB( p^.rgbtRed, p^.rgbtGreen, p^.rgbtBlue );
          if color = FFontNormalColor then
          begin
            p^.rgbtRed := GetRValue( FBlendColorTable[nIndex] );
            p^.rgbtGreen := GetGValue( FBlendColorTable[nIndex] );
            p^.rgbtBlue := GetBValue( FBlendColorTable[nIndex] );
          end;
          Inc(P);
        end;
        Dec( nIndex );
      end;
    end;
  end;
end;

procedure TxdLyricShow.CalcSize;
begin
  GetClientRect( FDrawOwerHandle, FOwnerSize );
  FCurWidth := WidthOfRect( FOwnerSize );
  FCurHeight := HeightOfRect( FOwnerSize );
  FCurBmp.Width := FCurWidth;
  FCurBmp.Height := FCurHeight;

//  FBackBmp.Width := FCurBmp.Width;
//  FBackBmp.Height := FCurBmp.Height;
//  SendMessage(FDrawOwerHandle, WM_ERASEBKGND, FBackBmp.Canvas.Handle, 0);
//  SendMessage(FDrawOwerHandle, WM_PAINT, FBackBmp.Canvas.Handle, 0);

//  FBackBmp.SaveToFile( 'a.bmp' );
end;

constructor TxdLyricShow.Create;
var
  size: TSize;
begin
  FActive := False;
  FLyrParse := TxdLyricParse.Create;
  FCurBmp := TBitmap.Create;
//  FBackBmp := TBitmap.Create;
//  FBackBmp.PixelFormat := pf24bit;
  FCurBmp.PixelFormat := pf24bit;
  FCurBmp.Canvas.Brush.Style := bsClear;
  GetTextExtentPoint32( FCurBmp.Canvas.Handle, PChar('国'), 1, Size);
  FCurFontHeight := size.cy + size.cy div 5;
  FColorTableStep := FCurFontHeight;
  FFontNormalColor := RGB(2, 34, 129);
  FBackColor := clGrayText;
  FFontBrightColorFrom := RGB(255, 0, 0);
  FFontBrightColorTo := RGB(0, 0, 255);
  FBlendTableStep := 40;
end;

destructor TxdLyricShow.Destroy;
begin
  EndLyricShow;
  FLyrParse.Free;
  FCurBmp.Free;
  inherited;
end;

procedure TxdLyricShow.DrawHorizontalLyricFram;
var
  strText: string;
  nIndex, CurOffset, nWidth, nLeftOffset: Integer;
  CurLineInfo: TLineInfo;
  LeftRect, CurRect, RightRect: TRect;
begin
  //当前行
  if not FLyrParse.GetLineInfo(FCurTime, CurLineInfo) then Exit;
  if CurLineInfo.ScrollTime = 0 then Exit;

  nWidth := TextWidth( CurLineInfo.ScrollString );
  CurOffset := ( FCurTime - CurLineInfo.StartTime ) * nWidth div CurLineInfo.ScrollTime;
  nLeftOffset := ( FCurWidth div 2 ) - CurOffset;

  LeftRect := Rect( 0, 0, nLeftOffset, FCurHeight );
  CurRect := Rect(nLeftOffset, 0, nLeftOffset + nWidth, FCurHeight);
  RightRect := Rect(CurRect.Right, 0, FCurWidth, FCurHeight);

  //当前
  FCurBmp.Canvas.Font.Color := GetBrightColor( CurLineInfo.StartTime );
  DrawText( FCurBmp.Canvas.Handle, PChar(CurLineInfo.ScrollString), Length(CurLineInfo.ScrollString), CurRect,
    DT_VCENTER or DT_SINGLELINE or DT_LEFT );

  //左边
  nIndex := CurLineInfo.Index - 1;
  FCurBmp.Canvas.Font.Color := FFontNormalColor;
  while (nIndex > 0) and (LeftRect.Right > 0) do
  begin
    if not FLyrParse.GetLyricLineString(nIndex, strText) then Break;
    strText := strText + '  ';
    DrawText( FCurBmp.Canvas.Handle, PChar(strText), Length(strText), LeftRect, DT_VCENTER or DT_SINGLELINE or DT_RIGHT );
    Dec( nIndex );
    Dec( LeftRect.Right, TextWidth(strText) );
  end;

  //右边
  nIndex := CurLineInfo.Index + 1;
  while RightRect.Left < FCurWidth do
  begin
    if not FLyrParse.GetLyricLineString(nIndex, strText) then Break;
    strText := '  ' + strText;
    DrawText( FCurBmp.Canvas.Handle, PChar(strText), Length(strText), RightRect, DT_VCENTER or DT_SINGLELINE or DT_LEFT );
    Inc( nIndex );
    Inc( RightRect.Left, TextWidth(strText) );
  end;
  BlendBitmap;
end;

procedure TxdLyricShow.DrawVerticalLyricFrame;
var
  strText: string;
  nIndex, nCurOffset: Integer;
  TextR, R: TRect;
  CurLineInfo: TLineInfo;
begin
  if not FLyrParse.GetLineInfo(FCurTime, CurLineInfo) then Exit;
  if CurLineInfo.ScrollTime = 0 then Exit;
  R := Rect( 0, 0, FCurWidth, FCurFontHeight );
  //当前行
  TextR := R;
  nCurOffset := (FCurTime - CurLineInfo.StartTime) * FCurFontHeight div CurLineInfo.ScrollTime;
  OffsetRect( TextR, 0, (FCurHeight - FCurFontHeight) div 2 - nCurOffset );
  TextOutLyric( CurLineInfo.ScrollString, GetBrightColor(CurLineInfo.StartTime), TextR, DT_CENTER );
  //上面
  nIndex := CurLineInfo.Index - 1;
  R := TextR;
  OffsetRect( R, 0, -FCurFontHeight );
  while (nIndex > 0) and (R.Bottom > 0) do
  begin
    if not FLyrParse.GetLyricLineString(nIndex, strText) then Break;
    TextOutLyric( strText, FFontNormalColor, R, DT_CENTER );
    Dec( nIndex );
    OffsetRect( R, 0, -FCurFontHeight );
  end;
  //下面
  nIndex := CurLineInfo.Index + 1;
  R := TextR;
  OffsetRect( R, 0, FCurFontHeight );
  while R.Top < FCurHeight do
  begin
    if not FLyrParse.GetLyricLineString(nIndex, strText) then Break;
    TextOutLyric( strText, FFontNormalColor, R, DT_CENTER );
    Inc( nIndex );
    OffsetRect( R, 0, FCurFontHeight );
  end;
  BlendBitmap;
end;

procedure TxdLyricShow.EndLyricShow;
begin
  if Active then
  begin
    TxdWndHook.RemoveHook( WH_CALLWNDPROC, OnDrawWndMessage );
    FActive := False;
  end;
end;

procedure TxdLyricShow.FillBlendColorTable;
begin
  SetLength( FBlendColorTable, FBlendTableStep );
  GradientColor( FBlendColorTable, FBackColor, FFontNormalColor );
end;

procedure TxdLyricShow.FillBrighColorTable;
var
  at: array of TColor;
  n: Integer;
begin
  SetLength( FBrightColorTable, FColorTableStep );

  n := FColorTableStep div 2;
  SetLength( at, n );
  GradientColor( at, FFontNormalColor, FFontBrightColorFrom );
  Move( at[0], FBrightColorTable[0], SizeOf(TColor) * n );

  n := FColorTableStep - n;
  SetLength( at, n );
  GradientColor( at, FFontBrightColorFrom, FFontBrightColorTo );
  Move( at[0], FBrightColorTable[n], SizeOf(TColor) * n );

//  GradientColor( FBrightColorTable, FFontBrightColorFrom, FFontBrightColorTo );
end;

function TxdLyricShow.GetBrightColor(const AStartTime: Integer): TColor;
var
  nIndex: Integer;
begin
  if FCurTime > AStartTime then
  begin
    nIndex := (FCurTime - AStartTime) * FColorTableStep div 2000;
    if nIndex >= FColorTableStep then
      nIndex := FColorTableStep - 1;
  end
  else
    nIndex := 0;
  Result := FBrightColorTable[nIndex];
end;

procedure TxdLyricShow.HookDrawWnd;
begin
  CalcSize;
  TxdWndHook.RemoveHook( WH_CALLWNDPROC, OnDrawWndMessage );
  if not TxdWndHook.AddHook( WH_CALLWNDPROC, OnDrawWndMessage ) then
    OutputDebugString( 'errrrrrrrrrrrror ' );
end;

procedure TxdLyricShow.OnDrawWndMessage(AwParam, AlParam: Integer);
var
  p: PCWPStruct;
begin
  if not Active then Exit;
  p := PCWPStruct( AlParam );
  if p^.hwnd = FDrawOwerHandle then
  begin
    if p^.message = WM_SIZE then
    begin
      CalcSize;
    end;
  end;  
end;

procedure TxdLyricShow.ReChangedFontInfo;
var
  size: TSize;
begin
  GetTextExtentPoint32( FCurBmp.Canvas.Handle, PChar('国'), 1, Size);
  FCurFontHeight := size.cy + size.cy div 5;
  ColorTableStep := FCurFontHeight;
end;

function TxdLyricShow.BeginLyricShow(const AFileName: string): Boolean;
begin
  EndLyricShow;
  if not IsWindow(FDrawOwerHandle) then
  begin
    Result := False;
    Exit;
  end;
  Result := FLyrParse.LoadLyricFile( AFileName );
  if Result then
  begin
    FActive := True;
    HookDrawWnd;
    FillBrighColorTable;
    FillBlendColorTable;
  end;
end;

procedure TxdLyricShow.ScrollLyricShow(const ATime: Cardinal);
var
  dc: HDC;
begin
  if not FActive then Exit;
  FCurTime := ATime;
//  FCurBmp.Canvas.Draw( 0, 0, FBackBmp );
//  FCurBmp.Canvas.Brush.Style := bsClear;
  FCurBmp.Canvas.Brush.Color := FBackColor;
  FCurBmp.Canvas.FillRect( Rect(0, 0, FCurBmp.Width, FCurBmp.Height) );
  //绘制当前
  case FLyricShowStyle of
    lskHorizontal: DrawHorizontalLyricFram;
    lskVertical:   DrawVerticalLyricFrame;
  end;
  dc := GetDC( FDrawOwerHandle );
  try
    BitBlt( dc, 0, 0, FCurBmp.Width, FCurBmp.Height, FCurBmp.Canvas.Handle, 0, 0, SRCCOPY );
  finally
    DeleteDC( dc );
  end;
end;

procedure TxdLyricShow.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    if Active then
      FillBlendColorTable;
  end;
end;

procedure TxdLyricShow.SetBlendTableStep(const Value: Integer);
begin
  if FBlendTableStep <> Value then
  begin
    FBlendTableStep := Value;
    if Active then
      FillBlendColorTable;
  end;
end;

procedure TxdLyricShow.SetColorTableStep(const Value: Integer);
begin
  if (Value > 0) and (FColorTableStep <> Value) then
  begin
    FColorTableStep := Value;
    FillBrighColorTable;
  end;
end;

procedure TxdLyricShow.SetDrawOwnerHandle(const Value: Cardinal);
begin
  FDrawOwerHandle := Value;
  if Active then
    HookDrawWnd;
end;

procedure TxdLyricShow.SetFontBrightColorFrom(const Value: TColor);
begin
  if FFontBrightColorFrom <> Value then
  begin
    FFontBrightColorFrom := Value;
    if Active then
      FillBrighColorTable;
  end;
end;

procedure TxdLyricShow.SetFontBrightColorTo(const Value: TColor);
begin
  if FFontBrightColorTo <> Value then
  begin
    FFontBrightColorTo := Value;
    if Active then
      FillBrighColorTable;
  end;
end;

procedure TxdLyricShow.SetFontNormalColor(const Value: TColor);
begin
  if FFontNormalColor <> Value then
  begin
    FFontNormalColor := Value;
    if Active then
      FillBlendColorTable;
  end;
end;

procedure TxdLyricShow.SetLyricShowStyle(const Value: TLyricShowStyle);
begin
  FLyricShowStyle := Value;
end;

procedure TxdLyricShow.TextOutLyric(const AText: string; const AColor: TColor; ARect: TRect;
  const AFormat: DWORD);
begin
  with FCurBmp.Canvas do
  begin
    Font.Color := AColor;
    DrawText(Handle, PChar(AText), Length(AText), ARect, DT_SINGLELINE or AFormat);
  end;
end;

function TxdLyricShow.TextWidth(const AText: string): Integer;
begin
  Result := FCurBmp.Canvas.TextWidth( AText );
end;

end.
