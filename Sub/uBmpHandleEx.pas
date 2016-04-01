unit uBmpHandleEx;

interface
uses
  Windows, Graphics, SysUtils, uBitmapHandle;

//图片二值化
procedure BmpTwoValue(ABmp: TBitmap); 
//提取图片轮廓
procedure BmpOutline(ABmp: TBitmap);
//图片细化
function BmpXihua(ABmp: TBitmap): Boolean;
//图片拉伸
procedure BmpZoom(ABmp: TBitmap; const ANewWidth, ANewHeight: Integer);

implementation

type
   TRGBArray = array[0..32767] of TRGBTriple;
   PRGBArray = ^TRGBArray;

procedure BmpTwoValue(ABmp: TBitmap); 
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
  X, Y: integer;
  P: PRGBTriple;
  nGrayAVG: Integer;
  cPixColor: TColor;
begin
  nGrayAVG := CalcBmpGrayValue;
  for Y := 0 to ABmp.Height - 1 do
  begin
    P := ABmp.ScanLine[Y];
    for X := 0 to ABmp.Width - 1 do
    begin
      cPixColor := Abmp.Canvas.Pixels[x, y] ;
      cPixColor := (GetRValue(cPixColor) + GetGValue(cPixColor) + GetBValue(cPixColor)) div 3;
      if cPixColor > nGrayAVG then
        Abmp.Canvas.Pixels[x, y] := $FFFFFF
      else
        Abmp.Canvas.Pixels[x, y] := 0;
//      cPixColor := ( p^.rgbtRed + P^.rgbtGreen + P^.rgbtBlue ) div 3;
//      
//      if cPixColor > nGrayAVG then
//      begin
//        p^.rgbtRed := 255;
//        P^.rgbtGreen := 255;
//        P^.rgbtBlue := 255;
//      end
//      else
//      begin
//        p^.rgbtRed := 0;
//        P^.rgbtGreen := 0;
//        P^.rgbtBlue := 0;
//      end;
//      Inc( p );      
    end;
  end;
end;

procedure BmpOutline(ABmp: TBitmap);
var
  b0, b1: Tbitmap;
  i, j: Integer;
  p1, p2, p3, p4: pbyteArray;
begin
  b0 := Tbitmap.Create;
  b1 := Tbitmap.Create;
  try
    b0.Assign( ABmp );
    b1.Assign( ABmp );
    b0.PixelFormat := pf24bit;
    b1.PixelFormat := pf24bit;
    for i := 1 to b0.Height - 2 do
    begin
      p1 := b0.ScanLine[i - 1];
      p2 := b0.ScanLine[i];
      p3 := b0.ScanLine[i + 1];
      p4 := b1.ScanLine[i];
      for j := 1 to b0.Width - 2 do
      begin
        if (p2[3 * j + 2] = 0) and (p2[3 * j + 1] = 0) and (p2[3 * j] = 0) then
        begin
          if ((p2[3 * (j - 1) + 2] = 0) and (p2[3 * (j - 1) + 1] = 0) and (p2[3 * (j - 1)] = 0)) and
             ((p2[3 * (j + 1) + 2] = 0) and (p2[3 * (j + 1) + 1] = 0) and (p2[3 * (j + 1)] = 0)) and
             ((p1[3 * (j + 1) + 2] = 0) and (p1[3 * (j + 1) + 1] = 0) and (p1[3 * (j + 1)] = 0)) and
             ((p1[3 * (j) + 2] = 0)     and (p1[3 * (j) + 1] = 0)     and (p1[3 * (j)] = 0))     and
             ((p1[3 * (j - 1) + 2] = 0) and (p1[3 * (j - 1) + 1] = 0) and (p1[3 * (j - 1)] = 0)) and
             ((p3[3 * (j - 1) + 2] = 0) and (p3[3 * (j - 1) + 1] = 0) and (p3[3 * (j - 1)] = 0)) and
             ((p3[3 * (j) + 2] = 0)     and (p3[3 * (j) + 1] = 0)     and (p3[3 * (j)] = 0))     and
             ((p3[3 * (j + 1) + 2] = 0) and (p3[3 * (j + 1) + 1] = 0) and (p3[3 * (j + 1)] = 0)) then
          begin
             p4[3 * j + 2] := 255;
             p4[3 * j + 1] := 255;
             p4[3 * j] := 255;
          end;
        end;
      end;
      ABmp.Assign(b1);
    end;
  finally
    b1.Free;
    b0.Free;
  end;
end;

function BmpXihua(ABmp: TBitmap): Boolean;
var
  bmp: TBitmap;
  X, Y: integer;
  O, T, C, B: pRGBArray;
  nb: array[1..3, 1..3] of integer;
  c1, c2, c3, c4: boolean;
  ncount: integer;
begin
  Result := True;
  bmp := TBitmap.Create;
  try
    bmp.Assign(ABmp);
    for Y := 1 to bmp.Height - 2 do
    begin
      O := bmp.ScanLine[Y];
      T := ABmp.ScanLine[Y - 1];
      C := ABmp.ScanLine[Y];
      B := ABmp.ScanLine[Y + 1];
      for X := 1 to bmp.Width - 2 do
      begin
        c1 := false;
        c2 := false;
        c3 := false;
        c4 := false;
        // 设立四个条件的初始值
        nb[1, 1] := T[X - 1].rgbtRed div 255;
        nb[1, 2] := T[X].rgbtRed div 255;
        nb[1, 3] := T[X + 1].rgbtRed div 255;
        nb[2, 1] := C[X - 1].rgbtRed div 255;
        nb[2, 2] := C[X].rgbtRed div 255;
        nb[2, 3] := C[X + 1].rgbtRed div 255;
        nb[3, 1] := B[X - 1].rgbtRed div 255;
        nb[3, 2] := B[X].rgbtRed div 255;
        nb[3, 3] := B[X + 1].rgbtRed div 255;
        //将[x,y]周围的八个象素点和它自己0-1化
        nCount := nb[1, 1] + nb[1, 2] + nb[1, 3] + 
                  nb[2, 1] + nb[2, 3] + 
                  nb[3, 1] + nb[3, 2] + nb[3, 3];
        // 获得ncount的值
        if (ncount >= 2) and (ncount <= 6) then c1 := True;
        //condition1
        ncount := 0;
        if (nb[1, 1] = 0) and (nb[1, 2] = 1) then Inc(ncount);
        if (nb[1, 2] = 0) and (nb[1, 3] = 1) then Inc(ncount);
        if (nb[1, 3] = 0) and (nb[2, 3] = 1) then Inc(ncount);
        if (nb[2, 3] = 0) and (nb[3, 3] = 1) then Inc(ncount);
        if (nb[3, 3] = 0) and (nb[3, 2] = 1) then Inc(ncount);
        if (nb[3, 2] = 0) and (nb[3, 1] = 1) then Inc(ncount);
        if (nb[3, 1] = 0) and (nb[2, 1] = 1) then Inc(ncount);
        if (nb[2, 1] = 0) and (nb[1, 1] = 1) then Inc(ncount);
        if ncount = 1 then c2 := true;
        //condition2
        if (nb[1, 2] * nb[3, 2] * nb[2, 3] = 0) then c3 := true;
        // condition3
        if (nb[2, 1] * nb[2, 3] * nb[3, 2] = 0) then c4 := true;
        //condition4
        if (c1 and c2 and c3 and c4) then
        begin
          O[X].rgbtRed := 255;
          O[X].rgbtGreen := 255;
          O[X].rgbtBlue := 255;
          if not Result then Result := True;          
        end;
      end;
    end;
    ABmp.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

procedure BmpZoom(ABmp: TBitmap; const ANewWidth, ANewHeight: Integer);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;  
  try
    bmp.Width := ANewWidth;
    bmp.Height := ANewHeight;
    SetStretchBltMode(bmp.Canvas.Handle, HalfTone);
    Stretchblt(bmp.Canvas.Handle, 0, 0, bmp.Width, bmp.Height,
               ABmp.Canvas.Handle, 0, 0, ABmp.Width, ABmp.Height, SRCCOPY);
    ABmp.Assign( bmp );
  finally
    bmp.Free;
  end;
end;

end.
