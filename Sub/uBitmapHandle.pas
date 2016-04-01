unit uBitmapHandle;

interface
uses
  Windows, Classes, Graphics, SysUtils, Forms, Jpeg, ZLib, Controls, Messages;

{调试开关}
//{$define OutPutImage}  //灰度图像时,将它保存到磁盘上
//
type
  TDrawStyle = (dsAll);
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

  TViewMode = (vmColor4, vmGray4, vmGray8, vmColor24, vmDefault);

  //得到指定句柄窗口的图像
  function GetBitmapFromHandle(const AMainHandle, AHandle: HWND;
                               const ASoftWidth, ASoftHeigth: Integer; var ABmp: TBitmap): Boolean;
  //将图片变成黑白
  procedure BitmapChangeBlackAndWhite(var ABmp: TBitmap);
  //改变图片的亮度
  //ABrightValue: 亮度: -255(黑) ~~ 255(白)
  procedure ChangeBrightness(const ABmp:TBitmap; ABrightValue: Integer);
  //将Canvas 变灰
  procedure GrapCanvas(ACanvas: TCanvas; const AWidth, AHeight: Integer; ATransColor: TColor = clFuchsia);

  //得到某一张图片跟原图片的相似度. 百分之百为全相同
  //IsStrict : 是否严格对比
  //ErrorValue: 容错范围  当 IsStrict 时有效
  function GetBitmapConform(const AOrgBmp, ADesBmp: TBitmap; IsStrict: Boolean = True; ErrorValue: Integer = 200000): Integer;

  //渐变颜色,从 FromColor 渐变成 ToColor;
  procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
//  procedure DrawColorGradient(Canvas: TCanvas; AGradient: TParseGradient; AWay: TGradientWay; ARect: TRect);

  //用pen 来画边框
  procedure DrawFrameBorder(Canvas: TCanvas; const LienColor: TColor; const LienWidth: Integer; R: TRect);

  procedure OutPutImage(const ABmp: TBitmap; const AFileName: string = '');

  //图像旋转90度
  procedure ImageRotate90(Bitmap: TBitmap);
  procedure ImageFlipH(Bitmap: TBitmap);
  procedure ImageFlipV(Bitmap: TBitmap);

  function HSB2RGB(H: THSBColor): TRGBColor;
  function RGB2HSB(R: TRGBColor): THSBColor;
  function ChangedRGB(R: TColor; HSB: THSBColor): TColor; overload;
  function ChangedRGB(OrgColor, NewColor: TColor): TColor; overload;

  function  GetBitmapHSB(ABmp: TBitmap): THSBColor; //图像平均HSB值
  procedure SetBitmapHSB(ABmp: TBitmap; HSB: THSBColor); overload;//设置图像HSB
  procedure SetBitmapHSB(ACanvas: TCanvas; Width, Height: Integer; HSB: THSBColor; ATransparentColor: TColor); overload;
  //
  procedure GetControlBackground(AControl: TWinControl; ADestBmp: TBitmap; ACopyRect: TRect);
  
  procedure DrawRectangle(ACanvas: TCanvas; const ABmpR, ADestR: TRect; AResBmp: TBitmap); overload;
  procedure DrawRectangle(ACanvas: TCanvas; const ABmpR, ADestR: TRect; AResBmp: TBitmap; const ATransColor: TColor); overload;
  procedure DrawRectangle(ADesCanvas, ASrcCanvas: TCanvas; const ADestR, ASrcR: TRect; AUpToDown: Boolean); overload;

  //换色
  procedure TransColor(ABmp: TBitmap; AColor: TColor); overload;
  procedure TransColor(ABmp: TBitmap; AColor, ATransparentColor: TColor); overload;

  function  WidthOfRect(const R: TRect): Integer;
  function  HeightOfRect(const R: TRect): Integer;

  //改变图片像素
  procedure ChangeBitmap(var bmp: TBitmap; ViewMode: TViewMode);
  //获取桌面图片(jpg)的流
  procedure GetDesktopStream(const AWidth, AHeight, ACompression: Integer; AStream: TStream);
  //获取桌面指定位置图像
  procedure GetDesktopBitmap(const AR: TRect; var ABmp: TBitmap);

  //扫描计算长度  从上向下扫描
  function CalcBmpSize(Canvas: TCanvas; const AOrgX, AOrgY, AMaxPos: Integer;
                       const ATransColor: TColor; AIsCalcTransColor: Boolean): Integer;
  //得到图片表示的内部区域
  function CreateRgnByBitmap(const ABmp: TBitmap; const AMaskColor: TColor): HRGN;

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

procedure OutPutImage(const ABmp: TBitmap; const AFileName: string = '');
var
  strFileName: string;
begin
  strFileName := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  if AFileName = '' then
    strFileName := Format('%s%d %d.bmp', [strFileName, ABmp.Width, ABmp.Height])
  else
    strFileName := strFileName + AFileName;
  ABmp.SaveToFile(strFileName);
end;

function GetBitmapFromHandle(const AMainHandle, AHandle: HWND;
                             const ASoftWidth, ASoftHeigth: Integer; var ABmp: TBitmap): Boolean;
var
  dc: HDC;
  bVisible, bChangePos: Boolean;
  rtOldMain, rtNewMain, rtChild: TRect;
begin
  if IsWindow(AHandle) then
    Result := True
  else
  begin
    Result := False;
    Exit;
  end;
  
  bVisible := IsWindowVisible(AMainHandle);
  bChangePos := False;
  try
    GetWindowRect(AMainHandle, rtOldMain);
    SetWindowPos(AMainHandle, 0, 0, 0, ASoftWidth, ASoftHeigth, SWP_NOMOVE);
    GetWindowRect(AMainHandle, rtNewMain);
    GetWindowRect(AHandle, rtChild);
    ABmp.width := rtChild.Right - rtChild.Left; 
    ABmp.height := rtChild.Bottom - rtChild.Top;

    if not bVisible then
    begin
      bChangePos := True;
      ShowWindow(AMainHandle, SW_SHOW);
    end;
    SetWindowPos(AMainHandle, 0, 10, 10, 0, 0, SWP_NOSIZE);
    Sleep(100);
    
    dc := GetDC(AHandle);
    Bitblt(ABmp.Canvas.Handle, 0, 0, ABmp.Width, ABmp.Height, dc, 0, 0,SRCCOPY);
    ReleaseDC(AHandle, dc);
    if bChangePos then
      ShowWindow(AMainHandle, SW_HIDE);
    SetWindowPos(AMainHandle, 0, rtOldMain.Left, rtOldMain.Top, 0, 0, SWP_NOSIZE);
    SetWindowPos(AMainHandle, 0, 0, 0, rtOldMain.Right - rtOldMain.Left, rtOldMain.Bottom - rtOldMain.Top, SWP_NOMOVE);

{$ifdef OutPutImage}
    OutPutImage(ABmp, 'BitmapFromHandle.bmp');
{$endif}

  except
    ABmp.free;
    raise Exception.Create('截取指定句柄窗口图像出错');
  end;
end;

procedure GrapCanvas(ACanvas: TCanvas; const AWidth, AHeight: Integer; ATransColor: TColor);
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

procedure BitmapChangeBlackAndWhite(var ABmp: TBitmap);
  function CalcBmpGrayValue: Integer;
  var
    p: PRGBTriple;
    xPos, xCount: Integer;
    yPos, yCount: Integer;
    fResult: Extended;
  begin
    fResult := 0.0;
    yCount := ABmp.Height;
    xCount := ABmp.Width;
    for yPos := 0 to yCount - 1 do
    begin
      p := ABmp.ScanLine[yPos];
      for xPos := 0 to xCount- 1 do
      begin
        fResult := (p^.rgbtRed + p^.rgbtGreen + p^.rgbtBlue) / 3 + fResult;
        inc(p);
      end;
    end;
    Result := Round(fResult / (xCount * yCount));
  end;
var
  xPos, yPos, xCount, yCount: Integer;
  cPixColor: TColor;
  nGrayAVG: Integer;
begin
  try

{$ifdef OutPutImage}
    OutPutImage(ABmp, 'OrgBitmap.bmp');
{$endif}
    ABmp.Canvas.Lock;
    try
      xCount := ABmp.Width;
      yCount := ABmp.Height;
      nGrayAVG := CalcBmpGrayValue;
      for yPos := 0 to yCount - 1 do
      begin
        for xPos := 0 to xCount - 1 do
        begin
          cPixColor := Abmp.Canvas.Pixels[xPos, yPos] ;
          cPixColor := (GetRValue(cPixColor) + GetGValue(cPixColor) + GetBValue(cPixColor)) div 3;
          if cPixColor > nGrayAVG then
            Abmp.Canvas.Pixels[xPos, yPos] := $FFFFFF
          else
            Abmp.Canvas.Pixels[xPos, yPos] := 0;
        end;
      end;
    finally
      ABmp.Canvas.Unlock;
    end;
{$ifdef OutPutImage}
    OutPutImage(ABmp, 'BlackAndWhiteBitmap.bmp');
{$endif}
  except
    raise Exception.Create('灰度图像时出错');
  end;
end;

function GetBitmapConform(const AOrgBmp, ADesBmp: TBitmap; IsStrict: Boolean = True; ErrorValue: Integer = 200000): Integer;
var
  x, xCount, y, yCount: Integer;
  cOrgPix, cDesPic: TColor;
begin
  Result := 0;
  try
    xCount := AOrgBmp.Width;
    yCount := AOrgBmp.Height;
    if xCount > ADesBmp.Width then
      xCount := ADesBmp.Width;
    if yCount > ADesBmp.Height then
      yCount := ADesBmp.Height;

    if IsStrict then
    begin
      for x := 0 to xCount - 1 do //从左向右
        for y := 0 to yCount - 1 do //从上向下
        begin
          if AOrgBmp.Canvas.Pixels[x, y] = ADesBmp.Canvas.Pixels[x, y] then
            inc(Result);
        end;
    end
    else
    begin
      for x := 0 to xCount - 1 do //从左向右
        for y := 0 to yCount - 1 do //从上向下
        begin
          cOrgPix := AOrgBmp.Canvas.Pixels[x, y];
          cDesPic := ADesBmp.Canvas.Pixels[x, y];
          if abs(cOrgPix - cDesPic) <= ErrorValue then
            inc(Result);
        end;
    end;
    Result :=  round((Result / (xCount * yCount)) * 100) ;
  except
    raise Exception.Create('对比图像出错');
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

//procedure DrawColorGradient(Canvas: TCanvas; AGradient: TParseGradient; AWay: TGradientWay; ARect: TRect);
//var
//  iLoop, nStep, nTempLen, nLen: Integer;
//  p: PGradient;
//  bDriection: Boolean;
//  R: TRect;
//begin
//  R := ARect;
//  bDriection := AWay = gwLeftToRigth;
//  if bDriection then
//    nLen := WidthOfRect( ARect )
//  else
//    nLen := HeightOfRect( ARect );
//
//  for iLoop := 0 to AGradient.GradientList.Count - 1 do
//  begin
//    p := AGradient.GradientList[iLoop];
//    if iLoop = AGradient.GradientList.Count - 1 then
//    begin
//      if bDriection then
//      begin
//        R.Right := ARect.Right;
//        nTempLen := WidthOfRect( R );
//      end
//      else
//      begin
//        R.Bottom := ARect.Bottom;
//        nTempLen := HeightOfRect( R );
//      end;
//    end
//    else
//    begin
//      nTempLen := Round( nLen * p^.FPercent );
//      if bDriection then
//        R.Right := R.Left + nTempLen
//      else
//        R.Bottom := R.Top + nTempLen;
//    end;
//    if nTempLen > 100 then
//      nStep := nTempLen div 2
//    else
//      nStep := nTempLen;
//    DrawGradient(Canvas, p^.FFromColor, p^.FColorTo, nStep, R, bDriection);
//    if bDriection then
//      R.Left := R.Right
//    else
//      R.Top := R.Bottom;
//  end;
//end;

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
        MoveTo(R.Left, R.Bottom);
        LineTo(R.Left, R.Top);
      end;
    end;
  finally
    Canvas.Pen.Color := OldPenColor;
    Canvas.Pen.Width := OldPenWidth;
  end;
end;

procedure DrawRectangle(ACanvas: TCanvas; const ABmpR, ADestR: TRect; AResBmp: TBitmap);
var
  BmpR, DesR: TRect;
  w, n: Integer;
begin
  BmpR := ABmpR;
  DesR := ADestR;
  n := WidthOfRect( ABmpR );
  w := ( n + 1 ) div 2;
  if (n - w * 2) = 0 then
    n := 2
  else
    n := 1;
  //左边
  BmpR.Right := BmpR.Left + w;
  DesR.Right := DesR.Left + w;
  ACanvas.CopyRect( DesR, AResBmp.Canvas, BmpR );
  //中间, 拉伸
  BmpR.Left := BmpR.Right - 1;
  BmpR.Right := BmpR.Left + n;
  DesR.Left := DesR.Right;
  DesR.Right := DesR.Left + WidthOfRect(ADestR) - 2 * (w - 1);
  ACanvas.CopyRect( DesR, AResBmp.Canvas, BmpR );

  //右边
  BmpR.Left := BmpR.Right;
  BmpR.Right := ABmpR.Right;
  DesR.Left := ADestR.Right - WidthOfRect( BmpR );
  DesR.Right := ADestR.Right ;
  ACanvas.CopyRect( DesR, AResBmp.Canvas, BmpR );
end;

procedure DrawRectangle(ACanvas: TCanvas; const ABmpR, ADestR: TRect; AResBmp: TBitmap; const ATransColor: TColor);
  var
  BmpR, DesR: TRect;
  w: Integer;
begin
  ACanvas.Brush.Style := bsClear;
  SetBkMode( ACanvas.Handle, TRANSPARENT );

  BmpR := ABmpR;
  DesR := ADestR;
  w := ( ABmpR.Right - ABmpR.Left + 1 ) div 2;
  //左边
  BmpR.Right := BmpR.Left + w;
  DesR.Right := DesR.Left + w;
  ACanvas.BrushCopy( DesR, AResBmp, BmpR, ATransColor );

  //中间, 拉伸
  BmpR.Left := BmpR.Right;
  BmpR.Right := BmpR.Left + 1;
  DesR.Left := DesR.Right;
  DesR.Right := WidthOfRect( ADestR ) - w;
  ACanvas.BrushCopy( DesR, AResBmp, BmpR, ATransColor );

//  //右边
  BmpR.Left := BmpR.Right;
  BmpR.Right := ABmpR.Right;
  DesR.Left := DesR.Right;
  DesR.Right := ADestR.Right;
  ACanvas.BrushCopy( DesR, AResBmp, BmpR, ATransColor );
end;

procedure DrawRectangle(ADesCanvas, ASrcCanvas: TCanvas; const ADestR, ASrcR: TRect; AUpToDown: Boolean);
  procedure DrawUpToDown;
  var
    n, h: Integer;
    SrcWidth, DesWidth, DesHeight: Integer;
  begin
    SrcWidth := ASrcR.Right - ASrcR.Left;
    DesWidth := ADestR.Right - ADestR.Left;
    DesHeight := ADestR.Bottom - ADestR.Top;
    n := (ASrcR.Bottom - ASrcR.Top) mod 2;
    h := (ASrcR.Bottom - ASrcR.Top + n) div 2;
    //上
    StretchBlt( ADesCanvas.Handle, ADestR.Left, ADestR.Top, DesWidth, h,
                ASrcCanvas.Handle, ASrcR.Left, ASrcR.Top, SrcWidth, h, SRCCOPY);
    //中
    n := DesHeight - h;
    StretchBlt( ADesCanvas.Handle, ADestR.Left, ADestR.Top + h, DesWidth, n,
                ASrcCanvas.Handle, ASrcR.Left, ASrcR.Top + h - 1, SrcWidth, 1, SRCCOPY);
//    //下
    StretchBlt( ADesCanvas.Handle, ADestR.Left, ADestR.Top + n, DesWidth, h,
                ASrcCanvas.Handle, ASrcR.Left, ASrcR.Top + h - 1, SrcWidth, h, SRCCOPY);
  end;

  procedure DrawLeftToRight;
  var
    SrcHeight, DesWidth, DesHeight: Integer;
    n, w: Integer;
  begin
    SrcHeight := HeightOfRect( ASrcR );
    DesWidth := WidthOfRect( ADestR );
    DesHeight := HeightOfRect( ADestR );
    n := WidthOfRect( ASrcR ) mod 2;
    w := (WidthOfRect( ASrcR ) + n) div 2;
    //左
    StretchBlt( ADesCanvas.Handle, ADestR.Left, ADestR.Top, w, DesHeight,
                ASrcCanvas.Handle, ASrcR.Left, ASrcR.Top, w, SrcHeight, SRCCOPY);
    //中
    n := DesWidth - w;
    StretchBlt( ADesCanvas.Handle, ADestR.Left + w, ADestR.Top, n, DesHeight,
                ASrcCanvas.Handle, ASrcR.Left + w - 1, ASrcR.Top, 1, SrcHeight, SRCCOPY);
    //右
    StretchBlt( ADesCanvas.Handle, ADestR.Left + n, ADestR.Top, w, DesHeight,
                ASrcCanvas.Handle, ASrcR.Left + w - 1, ASrcR.Top, w, SrcHeight, SRCCOPY);
  end;
  ////////////////////////////////////////
begin
  if AUpToDown then
    DrawUpToDown
  else
    DrawLeftToRight;
end;

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

procedure ChangeBrightness(const ABmp:TBitmap; ABrightValue: Integer);
  function BrightValue(ARGB: Integer): Integer;
  begin
    Result := ARGB + ABrightValue;
    if Result < 0 then
      Result := 0
    else if Result > 255 then
      Result := 255;
  end;
var
  i, j: integer;
  pRGB: pRGBTriple;
begin
  for i := 0 to ABmp.Height   -   1   do
  begin
    pRGB := ABmp.ScanLine[i];
    for j := 0 to ABmp.Width - 1 do
    begin
      pRGB.rgbtBlue := BrightValue(pRGB.rgbtBlue);
      pRGB.rgbtGreen := BrightValue(pRGB.rgbtGreen);
      pRGB.rgbtRed := BrightValue(pRGB.rgbtRed);
      Inc(pRGB);
    end;
  end;
end;

function GammaConv(Value: double; Gamma: double): double; inline;
begin
   if Value <> 0 then Result := Exp(Ln(Value) / Gamma)
      else Result := 0;
end;

function CreateGrayPalette(Num: integer; Gamma: double): HPalette;
var
   lPal  : PLogPalette;
   i     : integer;
begin
   // Add the Grayscale palette
   lPal := AllocMem(sizeof(TLogPalette) + Num * sizeof(TPaletteEntry));
   lPal.palVersion   := $300;
   lPal.palNumEntries := Num;
   for i := 0 to Num-1 do with lPal.palPalEntry[i] do begin
      peRed    := Round(255 * GammaConv(i / (Num-1), Gamma));
      peGreen  := Round(255 * GammaConv(i / (Num-1), Gamma));
      peBlue   := Round(255 * GammaConv(i / (Num-1), Gamma));
      peFlags  := 0;
   end;
   Result := CreatePalette(lPal^);
   FreeMem(lPal);
end;

procedure ConvertToGray_16(bmp: TBitmap);
var
   gm       : TBitmap;  // Destination grayscale bitmap
   x, y     : integer;
   p1       : PRGBArray;
   p2       : PByteArray;
   c        : integer;
begin
   bmp.PixelFormat := pf24bit;

   // Convert to Grayscale
   gm := TBitmap.Create;
   gm.PixelFormat := pf4bit;
   gm.Width  := bmp.Width;
   gm.Height := bmp.Height;

   gm.Palette := CreateGrayPalette(16, 1.4);

   for y := 0 to bmp.Height-1 do begin
      p1 := bmp.ScanLine[y];
      p2 := gm.ScanLine[y];
      for x := 0 to bmp.Width-1 do
      begin
        with p1^[x] do
        begin
          c := ( rgbtRed * 3 + rgbtGreen * 4 + rgbtBlue ) div (8 * 16);
          if (x and 1) = 1 then begin
            p2^[x div 2] := p2^[x div 2] and (not 15) or c;
          end
          else
          begin
            p2^[x div 2] := p2^[x div 2] and (15) or (c shl 4);
          end;
        end;
      end;
   end;

   bmp.Assign(gm);
   gm.Free;
end;

procedure ConvertToGray_256(bmp: TBitmap);
var
  gm       : TBitmap;  // Destination grayscale bitmap
  x, y     : integer;
  p1       : PRGBArray;
  p2       : PByteArray;
begin
  bmp.PixelFormat := pf24bit;

  // Convert to Grayscale
  gm := TBitmap.Create;
  gm.PixelFormat := pf8bit;
  gm.Width  := bmp.Width;
  gm.Height := bmp.Height;

  gm.Palette := CreateGrayPalette(256, 1.4);

  for y := 0 to bmp.Height-1 do begin
    p1 := bmp.ScanLine[y];
    p2 := gm.ScanLine[y];
    for x := 0 to bmp.Width-1 do
    begin
      with p1^[x] do
        p2^[x] := ( rgbtRed * 3 + rgbtGreen * 4 + rgbtBlue ) div 8;
    end;
  end;

  bmp.Assign(gm);
  gm.Free;
end;

procedure ChangeBitmap(var bmp: TBitmap; ViewMode: TViewMode);
begin
  case ViewMode of
    vmColor4    : bmp.PixelFormat := pf4bit;
    vmGray4     : ConvertToGray_16(bmp);
    vmGray8     : ConvertToGray_256(bmp);
    vmColor24   : bmp.PixelFormat := pf24bit;
    vmDefault   : bmp.HandleType := bmDIB;
   end;
end;

procedure GetDesktopStream(const AWidth, AHeight, ACompression: Integer; AStream: TStream);
var
  dc: HDC;
  bmp: TBitmap;
  jpg: TJPEGImage;
  //压缩
//  cs: TCompressionStream;
begin
  dc := GetDC( 0 );
  bmp := TBitmap.Create;
  jpg := TJPEGImage.Create;

  try
    bmp.Width := AWidth;
    bmp.Height := AHeight;
    StretchBlt( bmp.Canvas.Handle, 0, 0, bmp.Width, bmp.Height, dc, 0, 0, Screen.Width, Screen.Height, SRCCOPY);

//    ConvertToGray_16( bmp );
//    bmp.SaveToFile( 'a.bmp' );

    jpg.Assign( bmp );
    jpg.CompressionQuality := ACompression;
//    jpg.SaveToFile( 'a.jpg' );
    jpg.SaveToStream( AStream );

//    bmp.SaveToFile( 'a.bmp' );
//    bmp.SaveToStream( AStream );

    AStream.Position := 0;
//    cs := TCompressionStream.Create( clMax, AStream );
    AStream.Position := 0;

//    bmp.LoadFromStream( AStream );
//    bmp.SaveToFile( 'b.bmp' );
    jpg.LoadFromStream( AStream );
//    jpg.SaveToFile( 'b.jpg' );
//    cs.Free;
  finally
    ReleaseDC( 0, dc );
    bmp.Free;
    jpg.Free;
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
      raise   EInvalidPixelFormat.Create('Bitmap pixelformat is unknown.');
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

procedure ImageFlipH(Bitmap: TBitmap);
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

function CreateRgnByBitmap(const ABmp: TBitmap; const AMaskColor: TColor): HRGN;
var
  i, j, k, yPos0, yPos1: Integer;
  tmpRgn: HRGN;
begin
  Result := CreateRectRgn( 0, 0, 0, 0 );
  for i := 0 to ABmp.Width - 1 do
  begin
    for j := 0 to ABmp.Height - 1 do
    begin
      if ABmp.Canvas.Pixels[i, j] = AMaskColor then
      begin
        yPos0 := j;
        yPos1 := ABmp.Height - 1;
        for k := yPos1 downto yPos0 + 1 do
        begin
          if ABmp.Canvas.Pixels[i, k] = AMaskColor then
          begin
            yPos1 := k;
            Break;
          end;
        end;
        tmpRgn := CreateRectRgn( i, yPos0, i + 1, yPos1 );
        CombineRgn( Result, Result, tmpRgn, RGN_OR );
        Break;
      end;
    end;
  end;
end;

procedure GetDesktopBitmap(const AR: TRect; var ABmp: TBitmap);
var
  dc: HDC;
begin
  if ABmp = nil then
    ABmp := TBitmap.Create;

  dc := GetDC( 0 );
  try
    ABmp.Width := WidthOfRect( AR );
    ABmp.Height := HeightOfRect( AR );
    StretchBlt( ABmp.Canvas.Handle, 0, 0, ABmp.Width, ABmp.Height, dc, AR.Left, AR.Top, ABmp.Width, ABmp.Height, SRCCOPY);
  finally
    DeleteDC( dc );
  end;
end;

end.
