{
单元名称: uJxdDrawSub
单元作者: 江晓德(jxd524@163.com)
说    明: 绘制图片
开始时间: 2010-07-06
修改时间: 2010-07-07 (最后修改)
功    能：对图片进行相应操作
}
unit uJxdDrawSub;

interface
uses
  Windows, Messages, Graphics, uJxdGuiStyle, Classes, SysUtils, Controls;

type
  PRGBArray  = ^TRGBArray;
  TRGBArray   = array[0..65536 - 1] OF TRGBTriple;

//渐变颜色,从 FromColor 渐变成 ToColor;
procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);

//画矩形图
procedure DrawRectangle(ASrcBitmap: TBitmap; ADestCanvas: TCanvas; const ASrcRect, ADestRect: TRect; ADrawStyle: TDrawStyle;
                        AIsTransColor: Boolean = False; ATransColor: TColor = clFuchsia);

//将Canvas 变灰
procedure GrapCanvas(ACanvas: TCanvas; const AWidth, AHeight: Integer; ATransColor: TColor = clFuchsia);

//用pen 来画边框
procedure DrawFrameBorder(Canvas: TCanvas; const LienColor: TColor; const LienWidth: Integer; R: TRect);

//图像旋转90度
procedure ImageRotate90(Bitmap: TBitmap);

//得到控件背景图
procedure GetControlBackground(AControl: TWinControl; ADestBmp: TBitmap; ACopyRect: TRect);

//将两张图片进行混合


function  WidthOfRect(const R: TRect): Integer;
function  HeightOfRect(const R: TRect): Integer;

implementation

function  WidthOfRect(const R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function  HeightOfRect(const R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

procedure DrawRectangle(ASrcBitmap: TBitmap; ADestCanvas: TCanvas; const ASrcRect, ADestRect: TRect; ADrawStyle: TDrawStyle;
                        AIsTransColor: Boolean; ATransColor: TColor);
var
  SrcR, DestR: TRect;
  nSrcH, nSrcW, nW, nH: Integer;
  bFinished: Boolean;
begin
  case ADrawStyle of
    dsPaste:
    begin
      //使用帖图的方式绘制源图片到目标图片
      if AIsTransColor then
      begin
        ADestCanvas.Brush.Style := bsClear;
        ADestCanvas.BrushCopy( ADestRect, ASrcBitmap, ASrcRect, ATransColor )
      end
      else
        ADestCanvas.CopyRect( ADestRect, ASrcBitmap.Canvas, ASrcRect );
      //结束
    end;
    dsStretchyAll:
    begin
      //使用角落贴图，中间拉伸的方式将源图片绘制到目标图片中
      nSrcH := HeightOfRect( ASrcRect );
      nSrcW := WidthOfRect( ASrcRect );
      if nSrcW mod 2 = 0 then
        nW := 0
      else
        nW := 1;
      if nSrcH mod 2 = 0 then
        nH := 0
      else
        nH := 1;
        
      //左上角贴图
      SrcR := Rect( ASrcRect.Left, ASrcRect.Top, ASrcRect.Left + nSrcW div 2, ASrcRect.Top + nSrcH div 2 );
      DestR := Rect( ADestRect.Left, ADestRect.Top, ADestRect.Left + WidthOfRect(SrcR), ADestRect.Top + HeightOfRect(SrcR) );
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );
      //右上角贴图
      OffsetRect( SrcR, nSrcW div 2 + nW, 0 );
      DestR.Left := ADestRect.Left + WidthOfRect( ADestRect ) - nSrcW div 2;
      DestR.Right := DestR.Left + nSrcW div 2;
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );
      //右下角贴图
      OffsetRect( SrcR, 0, nSrcH div 2 + nH);
      DestR.Top := ADestRect.Top + HeightOfRect( ADestRect ) - nSrcH div 2;
      DestR.Bottom := DestR.Top + nSrcH div 2;
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );
      //左下角贴图
      OffsetRect( SrcR, -nSrcW div 2 - nW, 0 );
      DestR.Left := ADestRect.Left;
      DestR.Right := DestR.Left + nSrcW div 2;
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );

      //对中间末绘制部分进行拉伸
      //上
      SrcR := Rect( ASrcRect.Left + nSrcW div 2, ASrcRect.Top, ASrcRect.Left + nSrcW div 2 + 1, ASrcRect.Top + nSrcH div 2);
      DestR := Rect( ADestRect.Left + nSrcW div 2, ADestRect.Top, WidthOfRect(ADestRect) - nSrcW div 2, ADestRect.Top + HeightOfRect(SrcR) );
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );
      //下
      OffsetRect( SrcR, 0, nSrcH div 2 + nH );
      OffsetRect( DestR, 0, HeightOfRect(ADestRect) - nSrcH div 2 );
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );
      //左
      SrcR := Rect( ASrcRect.Left, ASrcRect.Top + nSrcH div 2, ASrcRect.Left + nSrcW div 2 , ASrcRect.Top + nSrcH div 2 + 1);
      DestR := Rect( ADestRect.Left, ADestRect.Top + nSrcH div 2, ADestRect.Left + WidthOfRect(SrcR), ADestRect.Top + HeightOfRect(ADestRect) - nSrcH div 2 );
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );
      //右
      OffsetRect( SrcR, nSrcW div 2 + nW, 0 );
      OffsetRect( DestR, WidthOfRect(ADestRect) - nSrcW div 2, 0 );
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );
      //中间一个像素拉伸
      SrcR := Rect( ASrcRect.Left + nSrcW div 2, ASrcRect.Top + nSrcH div 2, ASrcRect.Left + nSrcW div 2 + 1, ASrcRect.Top + nSrcH div 2 + 1);
      DestR := Rect( ADestRect.Left + nSrcW div 2, ADestRect.Top + nSrcH div 2, ADestRect.Left + WidthOfRect(ADestRect) - nSrcW div 2, ADestRect.Top + HeightOfRect(ADestRect) - nSrcH div 2 );
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );

      //结束
    end;
    dsStretchyUpToDown:
    begin
      //以上下贴图，中间拉伸的方式进行绘制
      nSrcH := HeightOfRect( ASrcRect );
      nSrcW := WidthOfRect( ASrcRect );
      if nSrcH mod 2 = 0 then
        nH := 0
      else
        nH := 1;
      bFinished := False;
      //上边贴图
      SrcR := Rect( ASrcRect.Left, ASrcRect.Top, ASrcRect.Left + nSrcW, ASrcRect.Top + nSrcH  div 2);
      DestR := Rect( ADestRect.Left, ADestRect.Top, ADestRect.Left + WidthOfRect(SrcR), ADestRect.Top + HeightOfRect(SrcR) );
      if DestR.Bottom >= ADestRect.Bottom then
      begin
        DestR.Bottom := ADestRect.Bottom;
        bFinished := True;
      end;
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );
      if bFinished then Exit;
      //下边贴图
      OffsetRect( SrcR, 0, nSrcH div 2 + nH );
      DestR.Top := ADestRect.Top + HeightOfRect( ADestRect ) - nSrcH div 2;
      DestR.Bottom := DestR.Top + nSrcH div 2;
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );
      //中间拉伸
      SrcR := Rect( ASrcRect.Left, ASrcRect.Top + nSrcH div 2, ASrcRect.Right, ASrcRect.Top + nSrcH div 2 + 1 );
      DestR.Top := ADestRect.Top + nSrcH div 2;
      DestR.Bottom := ADestRect.Top + HeightOfRect( ADestRect ) - nSrcH div 2;
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );
      //结束
    end;
    dsStretchyLeftToRight:
    begin
      //以左右贴图，中间拉伸的方式进行绘制
      nSrcH := HeightOfRect( ASrcRect );
      nSrcW := WidthOfRect( ASrcRect );
      if nSrcW mod 2 = 0 then
        nW := 0
      else
        nW := 1;
      bFinished := False;
      //左边贴图
      SrcR := Rect( ASrcRect.Left, ASrcRect.Top, ASrcRect.Left + nSrcW div 2, ASrcRect.Top + nSrcH );
      DestR := Rect( ADestRect.Left, ADestRect.Top, ADestRect.Left + WidthOfRect(SrcR), ADestRect.Top + HeightOfRect(SrcR) );
      if DestR.Right >= ADestRect.Right then
      begin
        DestR.Right := ADestRect.Right;
        bFinished := True;
      end;
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );
      if bFinished then Exit;
      
      //右边贴图
      OffsetRect( SrcR, nSrcW div 2 + nW, 0 );
      DestR.Left := ADestRect.Left + WidthOfRect( ADestRect ) - nSrcW div 2;
      DestR.Right := DestR.Left + nSrcW div 2;
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );
      //中间拉伸
      SrcR := Rect( ASrcRect.Left + nSrcW div 2, ASrcRect.Top, ASrcRect.Left + nSrcW div 2 + 1,ASrcRect.Top + nSrcH );
      DestR.Left := ADestRect.Left + nSrcW div 2;
      DestR.Right := ADestRect.Left + WidthOfRect( ADestRect ) - nSrcW div 2;
      DrawRectangle( ASrcBitmap, ADestCanvas, SrcR, DestR, dsPaste, AIsTransColor, ATransColor );
      //结束
    end;
  end;
end;

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;
begin
  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

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

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      else
        Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
    end;
  end;
end;

procedure GrapCanvas(ACanvas: TCanvas; const AWidth, AHeight: Integer; ATransColor: TColor);
var
  x, y, Gray: Integer;
  R, G, B: Byte;
  Pixel: Cardinal;
begin
  for y := 0 to AHeight - 1 do
  begin
    for x := 0 to AWidth - 1 do
    begin
      Pixel := GetPixel( ACanvas.Handle, x, y );
      if Pixel = Cardinal(ATransColor) then Continue;
      if Pixel = 0 then
      begin
        ACanvas.Pixels[X,Y] := $00797978;
        Continue;
      end;
      R := GetRValue( Pixel );
      G := GetGValue( Pixel );
      B := GetBValue( Pixel );
      Gray := ( R + G + B ) div 3;
      ACanvas.Pixels[X,Y] := RGB( Gray,Gray,Gray );
    end;
  end;
end;

procedure DrawFrameBorder(Canvas: TCanvas; const LienColor: TColor; const LienWidth: Integer; R: TRect);
var
  OldPenColor: TColor;
  OldPenWidth: Integer;
  nSpace: Integer;
begin
  OldPenColor := Canvas.Pen.Color;
  OldPenWidth := Canvas.Pen.Width;
  try
    Canvas.Pen.Color := LienColor;
    Canvas.Pen.Width := LienWidth;
    if LienWidth > 1 then
    begin
      nSpace := LienWidth div 2;
      with Canvas do
      begin
        {上}
        MoveTo(R.Left, R.Top + nSpace);
        LineTo(R.Right, R.Top + nSpace);
        {右}
        MoveTo(R.Right - nSpace, R.Top + nSpace);
        LineTo(R.Right - nSpace, R.Bottom);
        {底}
//        MoveTo(R.Right - nSpace, R.Bottom - nSpace);
        LineTo(R.Left, R.Bottom - nSpace);
        {左}
        MoveTo(R.Left + nSpace, R.Bottom);
        LineTo(R.Left + nSpace, R.Top);
      end;
    end
    else
    begin
      with Canvas do
      begin
        {上}
        MoveTo(R.Left, R.Top);
        LineTo(R.Right, R.Top);
        {右}
        MoveTo(R.Right - 1, R.Top);
        LineTo(R.Right - 1, R.Bottom);
        {底}
        MoveTo(R.Right - 1, R.Bottom - 1);
        LineTo(R.Left, R.Bottom - 1);
        {左}
        MoveTo(R.Left, R.Bottom - 1);
        LineTo(R.Left, R.Top - 1);
      end;
    end;
  finally
    Canvas.Pen.Color := OldPenColor;
    Canvas.Pen.Width := OldPenWidth;
  end;
end;

const
  BitsPerByte   =   8;

function GetPixelSize(aBitmap: TBitmap): Integer;
var   
  nBitCount, nMultiplier: integer;
begin
  case aBitmap.PixelFormat of
    pfDevice:
    begin
      nBitCount := GetDeviceCaps(aBitmap.Canvas.Handle, BITSPIXEL);
      nMultiplier := nBitCount div BitsPerByte;
      if (nBitCount mod BitsPerByte) > 0 then Inc(nMultiplier);
    end;
    pf1bit: nMultiplier := 1;
    pf4bit: nMultiplier := 1;
    pf8bit: nMultiplier := 1;
    pf15bit: nMultiplier := 2;
    pf16bit: nMultiplier := 2;
    pf24bit: nMultiplier := 3;
    pf32bit: nMultiplier := 4;
    else
      raise   Exception.Create('Bitmap pixelformat is unknown.');
  end;
  Result := nMultiplier;
end;

procedure ImageRotate90(Bitmap: TBitmap);
var
  nIdx, nOfs, x, y, i, nMultiplier: Integer;
  nMemWidth, nMemHeight, nMemSize,nScanLineSize: LongInt;
  aScnLnBuffer: PChar;
  aScanLine: PByteArray;
begin
  nMultiplier := GetPixelSize(Bitmap);
  nMemWidth := Bitmap.Height;
  nMemHeight := Bitmap.Width;
  nMemSize := nMemWidth * nMemHeight * nMultiplier;
  GetMem(aScnLnBuffer, nMemSize);
  try   
    nScanLineSize := Bitmap.Width * nMultiplier;
    GetMem(aScanLine, nScanLineSize);
    try   
      for y := 0 to Bitmap.Height - 1 do
      begin
        Move(Bitmap.ScanLine[y]^, aScanLine^, nScanLineSize);
        for x := 0 to Bitmap.Width-1 do
        begin
          nIdx := ((Bitmap.Width - 1) - x) * nMultiplier;
          nOfs := (x * nMemWidth * nMultiplier) +   //   y   component   of   the   dst
                  (y * nMultiplier);   //   x   component   of   the   dst
          for i := 0 to nMultiplier - 1 do
            Byte( aScnLnBuffer[nOfs + i] ) := aScanLine[nIdx+i];
        end;
      end;
      Bitmap.Height := nMemHeight;
      Bitmap.Width := nMemWidth;
      for y := 0 to nMemHeight - 1 do
      begin
        nOfs := y * nMemWidth * nMultiplier;
        Move( (@(aScnLnBuffer[nOfs]))^, Bitmap.ScanLine[y]^, nMemWidth * nMultiplier );
      end;
    finally
      FreeMem(aScanLine, nScanLineSize);
    end;
  finally
    FreeMem(aScnLnBuffer,   nMemSize);
  end;
end;

procedure GetControlBackground(AControl: TWinControl; ADestBmp: TBitmap; ACopyRect: TRect);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Width := AControl.Width;
    Bmp.Height := AControl.Height;
    AControl.Perform(WM_ERASEBKGND, Bmp.Canvas.Handle, 0);
    AControl.Perform(WM_PAINT, Bmp.Canvas.Handle, 0);
    Bitblt(ADestBmp.Canvas.Handle, 0, 0, WidthOfRect(ACopyRect), HeightOfRect(ACopyRect),
           Bmp.Canvas.Handle, ACopyRect.Left, ACopyRect.Top, SRCCOPY);
  finally
    Bmp.Free;
  end;
end;

end.
