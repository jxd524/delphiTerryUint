unit uKKBitmapHandle;

interface
  uses Windows, Messages, Classes, Controls, Graphics, SysUtils;

type
  TRGBColor = record
    Red,
    Green,
    Blue: integer;
  end;
  THSBColor = record
    Hue,
    Saturnation,
    Brightness   :   Double;
  end;
  EInvalidPixelFormat = class(Exception);  
  //将Canvas 变灰
  procedure GrapCanvas(ACanvas: TCanvas; const AWidth, AHeight: Integer; ATransColor: TColor);
  //用pen 来画边框
  procedure DrawFrameBorder(Canvas: TCanvas; const LienColor: TColor; const LienWidth: Integer; R: TRect);

  //扫描计算长度  从上向下扫描
  function CalcBmpSize(Canvas: TCanvas; const AOrgX, AOrgY, AMaxPos: Integer;
                       const ATransColor: TColor; AIsCalcTransColor: Boolean): Integer;
  //从上向下画,中间取一行像素进行拉伸
  procedure DrawRectangle(ADesCanvas, ASrcCanvas: TCanvas; const ADesR, ASrcR: TRect; AUpToDown: Boolean); overload;
  procedure DrawRectangle(ACanvas: TCanvas; const ABmpR, AResR: TRect; AResBmp: TBitmap); overload;
  //图像旋转90度
  procedure ImageRotate90(Bitmap: TBitmap);
  procedure ImageFlipH(Bitmap: TBitmap);
  procedure ImageFlipV(Bitmap: TBitmap);
  //换色
  procedure TransColor(ABmp: TBitmap; AColor: TColor); overload;
  procedure TransColor(ABmp: TBitmap; AColor, ATransparentColor: TColor); overload;

  procedure GetControlBackground(AControl: TWinControl; ADestBmp: TBitmap; ACopyRect: TRect);

  function HSB2RGB(H: THSBColor): TRGBColor;
  function RGB2HSB(R: TRGBColor): THSBColor;
  function ChangedRGB(R: TColor; HSB: THSBColor): TColor; overload;
  function ChangedRGB(OrgColor, NewColor: TColor): TColor; overload;

  function GetBitmapHSB(ABmp: TBitmap): THSBColor; //图像平均HSB值
  procedure SetBitmapHSB(ABmp: TBitmap; HSB: THSBColor); overload;//设置图像HSB
  procedure SetBitmapHSB(ACanvas: TCanvas; Width, Height: Integer; HSB: THSBColor; ATransparentColor: TColor); overload;

  function  WidthOfRect(const R: TRect): Integer;                        
  function  HeightOfRect(const R: TRect): Integer;
implementation

Type
  pRGBArray  = ^TRGBArray;
  TRGBArray   = array[0..65536 - 1] OF TRGBTriple;
const   
  BitsPerByte   =   8; 

function  WidthOfRect(const R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function  HeightOfRect(const R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

procedure GrapCanvas(ACanvas: TCanvas;const AWidth, AHeight: Integer; ATransColor: TColor);
type
  TRGBArray = array[0..32767] of TRGBTriple;
  PRGBArray = ^TRGBArray;
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
//        ACanvas.Pixels[X,Y] := $00797978;
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
        LineTo(R.Left, R.Top);
      end;
    end;
  finally
    Canvas.Pen.Color := OldPenColor;
    Canvas.Pen.Width := OldPenWidth;
  end;
end;

function CalcBmpSize(Canvas: TCanvas; const AOrgX, AOrgY, AMaxPos: Integer;
                     const ATransColor: TColor; AIsCalcTransColor: Boolean): Integer;
var
  i: Integer;
begin
  Result := 0;
  if AIsCalcTransColor then
  begin
    for i := AOrgY to AMaxPos - 1 do
    begin
      if Cardinal( ATransColor ) = GetPixel( Canvas.Handle, AOrgX, i ) then
        Inc(Result)
      else
        Break;
    end;
  end
  else
  begin
    for i := AOrgY to AMaxPos - 1 do
    begin
      if Cardinal( ATransColor ) = GetPixel( Canvas.Handle, AOrgX, i ) then
        Break
      else
        Inc(Result);
    end;
  end;
end;


procedure DrawRectangle(ADesCanvas, ASrcCanvas: TCanvas; const ADesR, ASrcR: TRect; AUpToDown: Boolean);
  procedure DrawUpToDown;
  var
    n, h: Integer;
    SrcWidth, DesWidth, DesHeight: Integer;
  begin
    SrcWidth := ASrcR.Right - ASrcR.Left;
    DesWidth := ADesR.Right - ADesR.Left;
    DesHeight := ADesR.Bottom - ADesR.Top;
    n := (ASrcR.Bottom - ASrcR.Top) mod 2;
    h := (ASrcR.Bottom - ASrcR.Top + n) div 2;
    //上
    StretchBlt( ADesCanvas.Handle, ADesR.Left, ADesR.Top, DesWidth, h,
                ASrcCanvas.Handle, ASrcR.Left, ASrcR.Top, SrcWidth, h, SRCCOPY);
    //中
    n := DesHeight - h;
    StretchBlt( ADesCanvas.Handle, ADesR.Left, ADesR.Top + h, DesWidth, n,
                ASrcCanvas.Handle, ASrcR.Left, ASrcR.Top + h - 1, SrcWidth, 1, SRCCOPY);
//    //下
    StretchBlt( ADesCanvas.Handle, ADesR.Left, ADesR.Top + n, DesWidth, h,
                ASrcCanvas.Handle, ASrcR.Left, ASrcR.Top + h - 1, SrcWidth, h, SRCCOPY);
  end;

  procedure DrawLeftToRight;
  var
    SrcHeight, DesWidth, DesHeight: Integer;
    n, w: Integer;
  begin
    SrcHeight := HeightOfRect( ASrcR );
    DesWidth := WidthOfRect( ADesR );
    DesHeight := HeightOfRect( ADesR );
    n := WidthOfRect( ASrcR ) mod 2;
    w := (WidthOfRect( ASrcR ) + n) div 2;
    //左
    StretchBlt( ADesCanvas.Handle, ADesR.Left, ADesR.Top, w, DesHeight,
                ASrcCanvas.Handle, ASrcR.Left, ASrcR.Top, w, SrcHeight, SRCCOPY);
    //中
    n := DesWidth - w;
    StretchBlt( ADesCanvas.Handle, ADesR.Left + w, ADesR.Top, n, DesHeight,
                ASrcCanvas.Handle, ASrcR.Left + w - 1, ASrcR.Top, 1, SrcHeight, SRCCOPY);
    //右
    StretchBlt( ADesCanvas.Handle, ADesR.Left + n, ADesR.Top, w, DesHeight,
                ASrcCanvas.Handle, ASrcR.Left + w - 1, ASrcR.Top, w, SrcHeight, SRCCOPY);
  end;
  ////////////////////////////////////////
begin
  if AUpToDown then
    DrawUpToDown
  else
    DrawLeftToRight;
end;


procedure DrawRectangle(ACanvas: TCanvas; const ABmpR, AResR: TRect; AResBmp: TBitmap);
var
  BmpR, DesR: TRect;
  w: Integer;
begin
  BmpR := ABmpR;
  DesR := AResR;
  w := ( AResR.Right - AResR.Left + 1 ) div 2;
  //左边
  BmpR.Right := BmpR.Left + w;
  DesR.Right := DesR.Left + w;
  ACanvas.CopyRect( BmpR, AResBmp.Canvas, DesR );
  //中间, 拉伸
  BmpR.Left := BmpR.Right;
  BmpR.Right := ABmpR.Right - w + 1;
  DesR.Left := DesR.Right - w mod 2;
  DesR.Right := DesR.Left + 1;
  ACanvas.CopyRect( BmpR, AResBmp.Canvas, DesR );
//  //右边
  BmpR.Left := BmpR.Right;
  BmpR.Right := ABmpR.Right;
  DesR.Left := DesR.Right;
  DesR.Right := AResR.Right;
  ACanvas.CopyRect( BmpR, AResBmp.Canvas, DesR );
end;


  //HSB2RGB  
function HSB2RGB(H: THSBColor): TRGBColor;
Var  
  HN,SN,LN,RD,GD,BD,V,M,SV,Fract,VSF,Mid1,Mid2:double;  
  R,G,B:Integer;  
  Sextant:integer;  
begin  
  HN:=H.Hue/239;  
  SN:=H.Saturnation/240;  
  LN:=H.Brightness/240;  
  if LN<0.5 then
    V:=LN*(1.0+SN)
  else
    V:=LN+SN-LN*SN;
  if V <= 0 then
  begin
    RD:=0.0;
    GD:=0.0;
    BD:=0.0;
  end
  else
  begin
    M:=LN+LN-V;
    SV:=(V-M)/V;
    HN:=HN*6.0;
    Sextant:=Trunc(HN);
    Fract:=HN-Sextant;
    VSF:=V*SV*Fract;
    Mid1:=M+VSF;
    Mid2:=V-VSF;
    case   Sextant   of
      0:
      begin
        RD:=V;
        GD:=Mid1;
        BD:=M;
      end;
      1:
      begin
        RD:=Mid2;
        GD:=V;
        BD:=M;
      end;
      2:
      begin
        RD:=M;
        GD:=V;
        BD:=Mid1;
      end;
      3:
      begin
        RD:=M;
        GD:=Mid2;
        BD:=V;
      end;
      4:
      begin
        RD:=Mid1;
        GD:=M;
        BD:=V;
      end;
      5:
      begin
        RD:=V;
        GD:=M;
        BD:=Mid2;
      end;
      else
      begin
        RD:=V;
        GD:=Mid1;
        BD:=M;
      end;
    end;   //end   case
  end;       //end   fi
  if   RD>1.0   then   RD:=1.0;
  if   GD>1.0   then   GD:=1.0;
  if   BD>1.0   then   BD:=1.0;
  R:=Round(RD*255);  
  G:=Round(GD*255);  
  B:=Round(BD*255);  
  result.Red:=R;  
  result.Green:=G;  
  result.Blue:=B;  
end;



function RGB2HSB(R:   TRGBColor):   THSBColor;
Var
  Dif,CCmax,CCmin,RC,GC,BC,TempH,TempS,TempL:double;  
begin  
  RC:=R.Red/255.0;
  GC:=R.Green/255.0;  
  BC:=R.Blue/255.0;  
  if   RC>GC   then   CCmax:=RC   else   CCmax:=GC;  
  if   BC>CCmax   then   CCmax:=BC;  
  if   RC<GC   then   CCmin:=RC   else   CCmin:=GC;  
  if   BC<CCmin   then   CCmin:=BC;  
  TempL:=(CCmax+CCmin)/2.0;  
  if   CCmax=CCmin   then  
  begin  
      TempS:=0;  
      TempH:=0;  
  end  
  else
  begin  
      Dif:=CCmax-CCmin;  
      if   TempL<0.5   then   TempS:=Dif/(CCmax+CCmin)   else   TempS:=Dif/(2.0-CCmax-CCmin);  
      if   RC=CCmax   then  
          TempH:=(GC-BC)/Dif  
      else   if   GC=CCmax   then   TempH:=2.0+(BC-RC)/Dif   else   TempH:=4.0+(RC-GC)/Dif;  
      TempH:=TempH/6;  
      if   TempH<0   then   TempH:=TempH+1;
  end;  
  result.Hue:=Round(240*TempH);  
  result.Saturnation:=Round(240*TempS);  
  result.Brightness:=Round(240*TempL);  
end;

function GetBitmapHSB(ABmp: TBitmap): THSBColor;
var
  i, j: Integer;
  RGBRow: pRGBArray;
  R: TRGBColor;
  HSB: THSBColor;
  H, S, B: Double;
  n: Integer;
begin
  n := 0;
  H := 0;
  S := 0;
  B := 0;
  for i := 0 to ABmp.Height - 1 do
  begin
    RGBRow := pRGBArray( ABmp.ScanLine[i] );
    for j := 0 to ABmp.Width - 1 do
    begin
      Inc(n);
      R.Red := RGBRow[j].rgbtRed;
      R.Green := RGBRow[j].rgbtGreen;
      R.Blue := RGBRow[j].rgbtBlue;
      HSB := RGB2HSB( R );
      H := H + HSB.Hue;
      S := S + HSB.Saturnation;
      B := B + HSB.Brightness;
    end;
  end;
  H := H / n;
  S := S / n;
  B := B / n;
  Result.Hue := H;
  Result.Saturnation := S;
  Result.Brightness := B;
end;

procedure SetBitmapHSB(ABmp: TBitmap; HSB: THSBColor);
var
  i, j: Integer;
  RGBRow: pRGBArray;
  R: TRGBColor;
  H: THSBColor;
begin
  for i := 0 to ABmp.Height - 1 do
  begin
    RGBRow := pRGBArray( ABmp.ScanLine[i] );
    for j := 0 to ABmp.Width - 1 do
    begin
      R.Red := RGBRow[j].rgbtRed;
      R.Green := RGBRow[j].rgbtGreen;
      R.Blue := RGBRow[j].rgbtBlue;
      H := RGB2HSB( R );
      H.Hue := H.Hue + HSB.Hue;
      H.Saturnation := H.Saturnation + HSB.Saturnation;
      H.Brightness := H.Brightness + HSB.Brightness;
      R := HSB2RGB( H );
      RGBRow[j].rgbtRed := R.Red;
      RGBRow[j].rgbtGreen := R.Green;
      RGBRow[j].rgbtBlue := R.Blue;
    end;
  end;
end;

procedure SetBitmapHSB(ACanvas: TCanvas; Width, Height: Integer; HSB: THSBColor; ATransparentColor: TColor);
var
  i, j: Integer;
  nColor: TColor;
begin
  for i := 0 to Width - 1 do
  begin
    for j := 0 to Height - 1 do
    begin
      nColor := ACanvas.Pixels[i, j];
      if nColor = ATransparentColor then Continue;
      ACanvas.Pixels[i, j] := ChangedRGB(nColor, HSB);
    end;
  end;
end;

function ChangedRGB(R: TColor; HSB: THSBColor): TColor;
var
  RGBColor: TRGBColor;
  H: THSBColor;
begin
  RGBColor.Red := GetRValue(R);
  RGBColor.Green := GetGValue(R);
  RGBColor.Blue := GetBValue(R);
  H := RGB2HSB( RGBColor );
  H.Hue := H.Hue + HSB.Hue;
  H.Saturnation := H.Saturnation + HSB.Saturnation;
  H.Brightness := H.Brightness + HSB.Brightness;
  RGBColor := HSB2RGB( H );
  Result := RGB( RGBColor.Red, RGBColor.Green, RGBColor.Blue );
end;

function ChangedRGB(OrgColor, NewColor: TColor): TColor;
var
  H: THSBColor;
  RGBColor: TRGBColor;
begin
  RGBColor.Red := GetRValue(NewColor);
  RGBColor.Green := GetGValue(NewColor);
  RGBColor.Blue := GetBValue(NewColor);
  H := RGB2HSB( RGBColor );
  Result := ChangedRGB(OrgColor, H);
end;

procedure TransColor(ABmp: TBitmap; AColor: TColor);
var
  i, j: Integer;
  RGBRow: pRGBArray;
  R, G, B: Byte;
begin
  ABmp.PixelFormat := pf24bit;
  R := GetRValue( AColor );
  G := GetGValue( AColor );
  B := GetBValue( AColor );
  for i := 0 to ABmp.Height - 1 do
  begin
    RGBRow := pRGBArray( ABmp.ScanLine[i] );
    for j := 0 to ABmp.Width - 1 do
    begin
      RGBRow[j].rgbtRed   := 255 - (255 - RGBRow[j].rgbtRed) * (255 - R) div 255;
      RGBRow[j].rgbtGreen := 255 - (255 - RGBRow[j].rgbtGreen) * (255 - G) div 255;
      RGBRow[j].rgbtBlue  := 255 - (255 - RGBRow[j].rgbtBlue) * (255 - B) div 255;
    end;
  end;
end;

procedure TransColor(ABmp: TBitmap; AColor, ATransparentColor: TColor);
var
  i, j: Integer;
  RGBRow: pRGBArray;
  R, G, B: Byte;
  TransR, TransG, TransB: Byte;
begin
  ABmp.PixelFormat := pf24bit;
  R := GetRValue( AColor );
  G := GetGValue( AColor );
  B := GetBValue( AColor );

  TransR := GetRValue( ATransparentColor );
  TransG := GetGValue( ATransparentColor );
  TransB := GetBValue( ATransparentColor );

  for i := 0 to ABmp.Height - 1 do
  begin
    RGBRow := pRGBArray( ABmp.ScanLine[i] );
    for j := 0 to ABmp.Width - 1 do
    begin
      if (TransR = RGBRow[j].rgbtRed) and (TransG  = RGBRow[j].rgbtGreen) and (TransB  = RGBRow[j].rgbtBlue) then Continue;
      RGBRow[j].rgbtRed   := 255 - (255 - RGBRow[j].rgbtRed) * (255 - R) div 255;
      RGBRow[j].rgbtGreen := 255 - (255 - RGBRow[j].rgbtGreen) * (255 - G) div 255;
      RGBRow[j].rgbtBlue  := 255 - (255 - RGBRow[j].rgbtBlue) * (255 - B) div 255;
    end;
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

function GetPixelSize(aBitmap: TBitmap): integer;
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
      raise   EInvalidPixelFormat.Create('Bitmap pixelformat is unknown.');
  end;
  Result := nMultiplier;
end;

procedure   ImageRotate90(Bitmap:   TBitmap);
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

procedure   ImageFlipH(Bitmap:   TBitmap);
var
  nMultiplier, nMemSize, y, y2, nHalfHeight, nFullHeight: Integer;
  aScanLine: PByteArray;
begin
  nMultiplier := GetPixelSize(Bitmap);
  nMemSize := Bitmap.Width * nMultiplier;
  GetMem(aScanLine, nMemSize);
  try
    nFullHeight := Bitmap.Height;
    nHalfHeight := nFullHeight   div   2;
    for y := 0 to nHalfHeight do
    begin
      Move(Bitmap.ScanLine[y]^, aScanLine^, nMemSize);
      y2 := nFullHeight - y - 1;
      Move(Bitmap.ScanLine[y2]^, Bitmap.ScanLine[y]^, nMemSize);
      Move(aScanLine^, Bitmap.ScanLine[y2]^, nMemSize);
    end;
  finally
    FreeMem(aScanLine);
  end;
end;

procedure ReverseScanLine(AScanLine: PByteArray; nSize, nElemWidth: Integer);
var
  i,   j,   w,   w2,   x1,   x2:   integer;
  aByte:   Byte;
begin
  w := nSize div nElemWidth;
  w2 := w div 2;
  for i := 0 to w2 do
  begin
    x1 := i * nElemWidth;
    x2 := (w - i - 1) * nElemWidth;
    for j := 0 to nElemWidth - 1 do
    begin
      aByte := AScanLine[x1 + j];
      AScanLine[x1 + j] := AScanLine[x2 + j];
      AScanLine[x2 + j] := aByte;
    end;
  end;
end;

procedure ImageFlipV(Bitmap: TBitmap);
var
  nMultiplier, nMemSize, y, nFullHeight: Integer;
  aScanLine: PByteArray;
begin
  nMultiplier := GetPixelSize(Bitmap);
  nMemSize := Bitmap.Width * nMultiplier;
  GetMem(aScanLine, nMemSize);
  try
    nFullHeight := Bitmap.Height;
    for y := 0 to nFullHeight-1 do
    begin
      Move(Bitmap.ScanLine[y]^, aScanLine^, nMemSize);
      ReverseScanLine(aScanLine, nMemSize, nMultiplier);
      Move(aScanLine^, Bitmap.ScanLine[y]^, nMemSize);
    end;
  finally
    FreeMem(aScanLine);
  end;
end;

end.
