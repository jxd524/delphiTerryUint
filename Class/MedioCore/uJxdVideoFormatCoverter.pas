{
说    明：
                                       YUV 格式说明
           打包（packed）格式和平面（planar）格式。前者将YUV分量存放在同一个数组中，通常是几个相邻的像素
           组成一个宏像素（macro-pixel）；而后者使用三个数组分开存放YUV三个分量，就像是一个三维平面一样。
           YUY2到Y211都是打包格式，而 IF09到YVU9都是平面格式
           
           YUY2视频的内存格式：
           4个字节表示两个像素
           例子：
             Y0 U0 Y1 V0
             像素点1： Y0 U0 V0
             像素点2： Y1 U0 V0
             Y2 U2 Y3 V2
             像素点1： Y2 U2 V2
             像素点2： Y3 U2 V2

           YVYU视频的内存格式：（与YUY2相似，只是存放位置有点差别）
           4个字节表示两个像素
           例子：
             Y0 V0 Y1 U0
             像素点1： Y0 U0 V0
             像素点2： Y1 U0 V0
             Y2 V2 Y3 U2
             像素点1： Y2 U2 V2
             像素点2： Y3 U2 V2

           UYVY视频的内存格式：（与YUY2相似，只是存放位置有点差别）
           4个字节表示两个像素
           例子：
             U0 Y0 V1 Y1
             像素点1： Y0 U0 V0
             像素点2： Y1 U0 V0
             U2 Y2 V3 Y2
             像素点1： Y2 U2 V2
             像素点2： Y3 U2 V2

           AYUV格式带有一个Alpha通道，并且为每个像素都提取YUV分量，图像数据格式如下
             A0 Y0 U0 V0    A1 Y1 U1 V1 …

           Y41P（和Y411）格式为每个像素保留Y分量，而UV分量在水平方向上每4个像素采样一次。一个宏像素为12个字节，
           实际表示8个像素。图像数据中YUV分量排列顺序如下：
             U0 Y0 V0 Y1    U4 Y2 V4 Y3    Y4 Y5 Y6 Y7 …
             像素点：Y0 U0 V0
                     Y1 U0 V0
                     Y2 U0 V0
                     Y3 U0 V0
                     Y4 U4 V4
                     Y5 U4 V4
                     Y6 U4 V4
                     Y7 U4 V4 

           Y211格式在水平方向上Y分量每2个像素采样一次，而UV分量每4个像素采样一次。一个宏像素为 4个字节，
           实际表示4个像素。图像数据中YUV分量排列顺序如下：
              Y0 U0 Y2 V0    Y4 U4 Y6 V4
//              像素点：Y0 U0 V0
//                      Y0 U0 V0
//                      Y2 U0 V0
//                      Y3 U4 V4
//                      Y5 U4 V4
//                      Y5 U4 V4
//                      Y6 U4 V4
//                      Y7 U4 V4

          YVU9格式为每个像素都提取Y分量，而在UV分量的提取时，首先将图像分成若干个4 x 4的宏块，然后每个宏块提取一
          个U分量和一个V分量。图像数据存储时，首先是整幅图像的Y分量数组，然后就跟着U分量数组，
          以及V分量数组。IF09格式与YVU9类似。

          ¨ IYUV格式为每个像素都提取Y分量，而在UV分量的提取时，首先将图像分成若干个2 x 2的宏块，然后每个宏块提取
          一个U分量和一个V分量。YV12格式与IYUV类似。

          ¨YUV411、YUV420格式多见于DV数据中，前者用于NTSC制，后者用于PAL 制。YUV411为每个像素都提取Y分量，而UV分
          量在水平方向上每4个像素采样一次。YUV420并非V分量采样为0，而是跟YUV411相比，在水平方向上提高一倍色差采
          样频率，在垂直方向上以U/V间隔的方式减小一半色差采样




YUV 与 RGB 的转换公式:

           C = Y - 16
           D = U - 128
           E = V - 128

           R = clip(( 298 * C           + 409 * E + 128) >> 8)
           G = clip(( 298 * C - 100 * D - 208 * E + 128) >> 8)
           B = clip(( 298 * C + 516 * D           + 128) >> 8)

           其中 clip()为限制函数,将其取值限制在0-255之间.

           Y = ( (  66 * R + 129 * G +  25 * B + 128) >> 8) +  16
           U = ( ( -38 * R -  74 * G + 112 * B + 128) >> 8) + 128
           V = ( ( 112 * R -  94 * G -  18 * B + 128) >> 8) + 128

感谢 单少爷(28000972)  15:31:12 2010-06-28提供

[Y,Cb,Cr] -> [R,G,B] 转换
-------------------------

R = Y                    + 1.402  *(Cr-128)
G = Y - 0.34414*(Cb-128) - 0.71414*(Cr-128)
B = Y + 1.772  *(Cb-128)

[R G B] -> [Y Cb Cr] 转换
-------------------------

(R,G,B 都是 8bit unsigned)

        | Y  |     |  0.299       0.587       0.114 |   | R |     | 0 |
        | Cb |  =  |- 0.1687    - 0.3313      0.5   | * | G |   + |128|
        | Cr |     |  0.5       - 0.4187    - 0.0813|   | B |     |128|

Y = 0.299*R + 0.587*G + 0.114*B  (亮度)
Cb =  - 0.1687*R - 0.3313*G + 0.5   *B + 128
Cr =    0.5   *R - 0.4187*G - 0.0813*B + 128
}

unit uJxdVideoFormatCoverter;

interface

procedure Yuy2ToRgb24(const ApYuy2Data, ApRgb24Data: PByte; const AWidth, AHeight: Integer);
procedure Rgb24ToYuy2(const ApYuy2data, ApRgb24Data: PByte; const AWidth, AHeight: Integer);

implementation

function CheckValue(const AValue: Integer): Byte; inline;
begin
  if AValue > 255 then
    Result := 255
  else if AValue < 0 then
    Result := 0
  else
    Result := AValue;
end;

procedure Rgb24ToYuy2(const ApYuy2data, ApRgb24Data: PByte; const AWidth, AHeight: Integer);
var
  i, j, nW: Integer;
  Y1, U1, Y2, V1: Byte;
  nYuy2Addr, nRgb24Addr: Integer;
  R1, R2, G1, G2, B1, B2: Byte;
begin
  nW := AWidth div 2;
  for i := 0 to AHeight - 1 do
  begin
    nYuy2Addr := Integer( ApYuy2Data ) + i * AWidth * 2;
    nRgb24Addr := Integer( ApRgb24Data ) + (AHeight - i - 1 ) * AWidth * 3;
    for j := 0 to nW - 1 do
    begin
      B1 := PByte( nRgb24Addr )^;
      G1 := PByte( nRgb24Addr + 1 )^;
      R1 := PByte( nRgb24Addr + 2 )^;
      B2 := PByte( nRgb24Addr + 3 )^;
      G2 := PByte( nRgb24Addr + 4 )^;
      R2 := PByte( nRgb24Addr + 5 )^;

      Y1 := CheckValue( Round(0.299 * R1 + 0.587 * G1 + 0.114 * B1) );
      Y2 := CheckValue( Round(0.299 * R2 + 0.587 * G2 + 0.114 * B2) );
      U1 := CheckValue( Round(-0.1687 * R1 - 0.3313 * G1 + 0.5 * B1 + 128) );
      V1 := CheckValue( Round(0.5 * R1 - 0.4187 * G1 - 0.0813 * B1 + 128) );

      PByte( nYuy2Addr )^ := Y1;
      PByte( nYuy2Addr + 1 )^ := U1;
      PByte( nYuy2Addr + 2 )^ := Y2;
      PByte( nYuy2Addr + 3 )^ := V1;

      Inc( nYuy2Addr, 4 );
      Inc( nRgb24Addr, 6 );
    end;
  end;
end;

procedure Yuy2ToRgb24(const ApYuy2Data, ApRgb24Data: PByte; const AWidth, AHeight: Integer);
var
  i, j, nW: Integer;
  Y1, U1, Y2, V1: Byte;
  nYuy2Addr, nRgb24Addr: Integer;
  D, E: Integer;
  R1, R2, G1, G2, B1, B2: Byte;
  nTemp, nTempR, nTempG, nTempB: Integer;
begin
  nW := AWidth div 2;
  for i := 0 to AHeight - 1 do
  begin
    nYuy2Addr := Integer( ApYuy2Data ) + i * AWidth * 2;
    nRgb24Addr := Integer( ApRgb24Data ) + (AHeight - i - 1 ) * AWidth * 3; 
    for j := 0 to nW - 1 do
    begin
      Y1 := PByte( nYuy2Addr )^;
      U1 := PByte( nYuy2Addr + 1 )^;
      Y2 := PByte( nYuy2Addr + 2 )^ ;
      V1 := PByte( nYuy2Addr + 3 )^;

      D := U1 - 128;
      E := V1 - 128;

      R1 := CheckValue( Round(Y1 + 1.402 * E) );
      G1 := CheckValue( Round(Y1 - 0.34414 * D - 0.71414 * E) );
      B1 := CheckValue( Round(Y1 + 1.772 * D) );

      R2 := CheckValue( Round(Y2 + 1.402 * E) );
      G2 := CheckValue( Round(Y2 - 0.34414 * D - 0.71414 * E) );
      B2 := CheckValue( Round(Y2 + 1.772 * D) );
      
      PByte( nRgb24Addr )^ := B1;
      PByte( nRgb24Addr + 1)^ := G1;
      PByte( nRgb24Addr + 2)^ := R1;
      PByte( nRgb24Addr + 3)^ := B2;
      PByte( nRgb24Addr + 4)^ := G2;
      PByte( nRgb24Addr + 5)^ := R2;

      Inc( nYuy2Addr, 4 );
      Inc( nRgb24Addr, 6 );
    end;
  end;
end;

end.

//
////////////////////////////////////////////////////////////////////////////
//// YUV2RGB
//// pYUV   point to the YUV data
//// pRGB   point to the RGB data
//// width  width of the picture
//// height  height of the picture
//// alphaYUV  is there an alpha channel in YUV
//// alphaRGB  is there an alpha channel in RGB
////////////////////////////////////////////////////////////////////////////
//int YUV2RGB(void* pYUV, void* pRGB, int width, int height, bool alphaYUV, bool alphaRGB)
//{
// if (NULL == pYUV)
// {
//  return -1;
// }
// unsigned char* pYUVData = (unsigned char *)pYUV;
// unsigned char* pRGBData = (unsigned char *)pRGB;
// if (NULL == pRGBData)
// {
//  if (alphaRGB)
//  {
//   pRGBData = new unsigned char[width*height*4];
//  }
//  else
//   pRGBData = new unsigned char[width*height*3];
// }
// int Y1, U1, V1, Y2, alpha1, alpha2, R1, G1, B1, R2, G2, B2;
// int C1, D1, E1, C2;
// if (alphaRGB)
// {
//  if (alphaYUV)
//  {
//   for (int i=0; i<height; ++i)
//   {
//    for (int j=0; j<width/2; ++j)
//    {
//     Y1 = *(pYUVData+i*width*3+j*6);
//     U1 = *(pYUVData+i*width*3+j*6+1);
//     Y2 = *(pYUVData+i*width*3+j*6+2);
//     V1 = *(pYUVData+i*width*3+j*6+3);
//     alpha1 = *(pYUVData+i*width*3+j*6+4);
//     alpha2 = *(pYUVData+i*width*3+j*6+5);
//     C1 = Y1-16;
//     C2 = Y2-16;
//     D1 = U1-128;
//     E1 = V1-128;
//     R1 = ((298*C1 + 409*E1 + 128)>>8>255 ? 255 : (298*C1 + 409*E1 + 128)>>8);
//     G1 = ((298*C1 - 100*D1 - 208*E1 + 128)>>8>255 ? 255 : (298*C1 - 100*D1 - 208*E1 + 128)>>8); 
//     B1 = ((298*C1+516*D1 +128)>>8>255 ? 255 : (298*C1+516*D1 +128)>>8); 
//     R2 = ((298*C2 + 409*E1 + 128)>>8>255 ? 255 : (298*C2 + 409*E1 + 128)>>8);
//     G2 = ((298*C2 - 100*D1 - 208*E1 + 128)>>8>255 ? 255 : (298*C2 - 100*D1 - 208*E1 + 128)>>8);
//     B2 = ((298*C2 + 516*D1 +128)>>8>255 ? 255 : (298*C2 + 516*D1 +128)>>8); 
//     *(pRGBData+(height-i-1)*width*4+j*8+2) = R1<0 ? 0 : R1;
//     *(pRGBData+(height-i-1)*width*4+j*8+1) = G1<0 ? 0 : G1;
//     *(pRGBData+(height-i-1)*width*4+j*8) = B1<0 ? 0 : B1;
//     *(pRGBData+(height-i-1)*width*4+j*8+3) = alpha1; 
//     *(pRGBData+(height-i-1)*width*4+j*8+6) = R2<0 ? 0 : R2;
//     *(pRGBData+(height-i-1)*width*4+j*8+5) = G2<0 ? 0 : G2;
//     *(pRGBData+(height-i-1)*width*4+j*8+4) = B2<0 ? 0 : B2;
//     *(pRGBData+(height-i-1)*width*4+j*8+7) = alpha2; 
//    }
//   } 
//  }
//  else
//  {
//   int alpha = 255;
//   for (int i=0; i<height; ++i)
//   {
//    for (int j=0; j<width/2; ++j)
//    {
//     Y1 = *(pYUVData+i*width*2+j*4);
//     U1 = *(pYUVData+i*width*2+j*4+1);
//     Y2 = *(pYUVData+i*width*2+j*4+2);
//     V1 = *(pYUVData+i*width*2+j*4+3);
//     C1 = Y1-16;
//     C2 = Y2-16;
//     D1 = U1-128;
//     E1 = V1-128;
//     R1 = ((298*C1 + 409*E1 + 128)>>8>255 ? 255 : (298*C1 + 409*E1 + 128)>>8);
//     G1 = ((298*C1 - 100*D1 - 208*E1 + 128)>>8>255 ? 255 : (298*C1 - 100*D1 - 208*E1 + 128)>>8); 
//     B1 = ((298*C1+516*D1 +128)>>8>255 ? 255 : (298*C1+516*D1 +128)>>8); 
//     R2 = ((298*C2 + 409*E1 + 128)>>8>255 ? 255 : (298*C2 + 409*E1 + 128)>>8);
//     G2 = ((298*C2 - 100*D1 - 208*E1 + 128)>>8>255 ? 255 : (298*C2 - 100*D1 - 208*E1 + 128)>>8);
//     B2 = ((298*C2 + 516*D1 +128)>>8>255 ? 255 : (298*C2 + 516*D1 +128)>>8); 
//     *(pRGBData+(height-i-1)*width*4+j*8+2) = R1<0 ? 0 : R1;
//     *(pRGBData+(height-i-1)*width*4+j*8+1) = G1<0 ? 0 : G1;
//     *(pRGBData+(height-i-1)*width*4+j*8) = B1<0 ? 0 : B1;
//     *(pRGBData+(height-i-1)*width*4+j*8+3) = alpha; 
//     *(pRGBData+(height-i-1)*width*4+j*8+6) = R2<0 ? 0 : R2;
//     *(pRGBData+(height-i-1)*width*4+j*8+5) = G2<0 ? 0 : G2;
//     *(pRGBData+(height-i-1)*width*4+j*8+4) = B2<0 ? 0 : B2;
//     *(pRGBData+(height-i-1)*width*4+j*8+7) = alpha; 
//    }
//   } 
//  }
// }
// else
// {
//  if (alphaYUV)
//  {
//   for (int i=0; i<height; ++i)
//   {
//    for (int j=0; j<width/2; ++j)
//    {
//     Y1 = *(pYUVData+i*width*3+j*4);
//     U1 = *(pYUVData+i*width*3+j*4+1);
//     Y2 = *(pYUVData+i*width*3+j*4+2);
//     V1 = *(pYUVData+i*width*3+j*4+3);
//     C1 = Y1-16;
//     C2 = Y2-16;
//     D1 = U1-128;
//     E1 = V1-128;
//     R1 = ((298*C1 + 409*E1 + 128)>>8>255 ? 255 : (298*C1 + 409*E1 + 128)>>8);
//     G1 = ((298*C1 - 100*D1 - 208*E1 + 128)>>8>255 ? 255 : (298*C1 - 100*D1 - 208*E1 + 128)>>8); 
//     B1 = ((298*C1+516*D1 +128)>>8>255 ? 255 : (298*C1+516*D1 +128)>>8); 
//     R2 = ((298*C2 + 409*E1 + 128)>>8>255 ? 255 : (298*C2 + 409*E1 + 128)>>8);
//     G2 = ((298*C2 - 100*D1 - 208*E1 + 128)>>8>255 ? 255 : (298*C2 - 100*D1 - 208*E1 + 128)>>8);
//     B2 = ((298*C2 + 516*D1 +128)>>8>255 ? 255 : (298*C2 + 516*D1 +128)>>8); 
//     *(pRGBData+(height-i-1)*width*3+j*6+2) = R1<0 ? 0 : R1;
//     *(pRGBData+(height-i-1)*width*3+j*6+1) = G1<0 ? 0 : G1;
//     *(pRGBData+(height-i-1)*width*3+j*6) = B1<0 ? 0 : B1;
//     *(pRGBData+(height-i-1)*width*3+j*6+5) = R2<0 ? 0 : R2;
//     *(pRGBData+(height-i-1)*width*3+j*6+4) = G2<0 ? 0 : G2;
//     *(pRGBData+(height-i-1)*width*3+j*6+3) = B2<0 ? 0 : B2;
//    }
//   }
//  }
//  else
//  {
//   for (int i=0; i<height; ++i)
//   {
//    for (int j=0; j<width/2; ++j)
//    {
//     Y1 = *(pYUVData+i*width*2+j*4);
//     U1 = *(pYUVData+i*width*2+j*4+1);
//     Y2 = *(pYUVData+i*width*2+j*4+2);
//     V1 = *(pYUVData+i*width*2+j*4+3);
//     C1 = Y1-16;
//     C2 = Y2-16;
//     D1 = U1-128;
//     E1 = V1-128;
//     R1 = ((298*C1 + 409*E1 + 128)>>8>255 ? 255 : (298*C1 + 409*E1 + 128)>>8);
//     G1 = ((298*C1 - 100*D1 - 208*E1 + 128)>>8>255 ? 255 : (298*C1 - 100*D1 - 208*E1 + 128)>>8); 
//     B1 = ((298*C1+516*D1 +128)>>8>255 ? 255 : (298*C1+516*D1 +128)>>8); 
//     R2 = ((298*C2 + 409*E1 + 128)>>8>255 ? 255 : (298*C2 + 409*E1 + 128)>>8);
//     G2 = ((298*C2 - 100*D1 - 208*E1 + 128)>>8>255 ? 255 : (298*C2 - 100*D1 - 208*E1 + 128)>>8);
//     B2 = ((298*C2 + 516*D1 +128)>>8>255 ? 255 : (298*C2 + 516*D1 +128)>>8); 
//     *(pRGBData+(height-i-1)*width*3+j*6+2) = R1<0 ? 0 : R1;
//     *(pRGBData+(height-i-1)*width*3+j*6+1) = G1<0 ? 0 : G1;
//     *(pRGBData+(height-i-1)*width*3+j*6) = B1<0 ? 0 : B1;
//     *(pRGBData+(height-i-1)*width*3+j*6+5) = R2<0 ? 0 : R2;
//     *(pRGBData+(height-i-1)*width*3+j*6+4) = G2<0 ? 0 : G2;
//     *(pRGBData+(height-i-1)*width*3+j*6+3) = B2<0 ? 0 : B2;
//    }
//   } 
//  }
// }
// return 0;
//}
//
////////////////////////////////////////////////////////////////////////////
//// RGB2YUV
//// pRGB   point to the RGB data
//// pYUV   point to the YUV data
//// width  width of the picture
//// height  height of the picture
//// alphaYUV  is there an alpha channel in YUV
//// alphaRGB  is there an alpha channel in RGB
////////////////////////////////////////////////////////////////////////////
//int RGB2YUV(void* pRGB, void* pYUV, int width, int height, bool alphaYUV, bool alphaRGB)
//{
// if (NULL == pRGB)
// {
//  return -1;
// }
// unsigned char* pRGBData = (unsigned char *)pRGB;
// unsigned char* pYUVData = (unsigned char *)pYUV;
// if (NULL == pYUVData)
// {
//  if (alphaYUV)
//  {
//   pYUVData = new unsigned char[width*height*3];
//  }
//  else
//   pYUVData = new unsigned char[width*height*2];
// }
// int R1, G1, B1, R2, G2, B2, Y1, U1, Y2, V1;
// int alpha1, alpha2;
// if (alphaYUV)
// {
//  if (alphaRGB)
//  {
//   for (int i=0; i<height; ++i)
//   {
//    for (int j=0; j<width/2; ++j)
//    {
//     B1 = *(pRGBData+(height-i-1)*width*4+j*8);
//     G1 = *(pRGBData+(height-i-1)*width*4+j*8+1);
//     R1 = *(pRGBData+(height-i-1)*width*4+j*8+2);
//     alpha1 = *(pRGBData+(height-i-1)*width*4+j*8+3);
//     B2 = *(pRGBData+(height-i-1)*width*4+j*8+4);
//     G2 = *(pRGBData+(height-i-1)*width*4+j*8+5);
//     R2 = *(pRGBData+(height-i-1)*width*4+j*8+6);
//     alpha2 = *(pRGBData+(height-i-1)*width*4+j*8+7);
//     Y1 = (((66*R1+129*G1+25*B1+128)>>8) + 16) > 255 ? 255 : (((66*R1+129*G1+25*B1+128)>>8) + 16);
//     U1 = ((((-38*R1-74*G1+112*B1+128)>>8)+((-38*R2-74*G2+112*B2+128)>>8))/2 + 128)>255 ? 255 : ((((-38*R1-74*G1+112*B1+128)>>8)+((-38*R2-74*G2+112*B2+128)>>8))/2 + 128);
//     Y2 = (((66*R2+129*G2+25*B2+128)>>8) + 16)>255 ? 255 : ((66*R2+129*G2+25*B2+128)>>8) + 16;
//     V1 = ((((112*R1-94*G1-18*B1+128)>>8) + ((112*R2-94*G2-18*B2+128)>>8))/2 + 128)>255 ? 255 : ((((112*R1-94*G1-18*B1+128)>>8) + ((112*R2-94*G2-18*B2+128)>>8))/2 + 128);
//     *(pYUVData+i*width*3+j*6) = Y1;
//     *(pYUVData+i*width*3+j*6+1) = U1;
//     *(pYUVData+i*width*3+j*6+2) = Y2;
//     *(pYUVData+i*width*3+j*6+3) = V1;
//     *(pYUVData+i*width*3+j*6+4) = alpha1;
//     *(pYUVData+i*width*3+j*6+5) = alpha2;
//    }
//   } 
//  }
//  else
//  {
//   unsigned char alpha = 255;
//   for (int i=0; i<height; ++i)
//   {
//    for (int j=0; j<width/2; ++j)
//    {
//     B1 = *(pRGBData+(height-i-1)*width*3+j*6);
//     G1 = *(pRGBData+(height-i-1)*width*3+j*6+1);
//     R1 = *(pRGBData+(height-i-1)*width*3+j*6+2);
//     B2 = *(pRGBData+(height-i-1)*width*3+j*6+3);
//     G2 = *(pRGBData+(height-i-1)*width*3+j*6+4);
//     R2 = *(pRGBData+(height-i-1)*width*3+j*6+5);
//     Y1 = ((66*R1+129*G1+25*B1+128)>>8) + 16;
//     U1 = ((-38*R1-74*G1+112*B1+128)>>8+(-38*R2-74*G2+112*B2+128)>>8)/2 + 128;
//     Y2 = ((66*R2+129*G2+25*B2+128)>>8) + 16;
//     V1 = ((112*R1-94*G1-18*B1+128)>>8 + (112*R2-94*G2-18*B2+128)>>8)/2 + 128;
//     Y1 = (((66*R1+129*G1+25*B1+128)>>8) + 16) > 255 ? 255 : (((66*R1+129*G1+25*B1+128)>>8) + 16);
//     U1 = ((((-38*R1-74*G1+112*B1+128)>>8)+((-38*R2-74*G2+112*B2+128)>>8))/2 + 128)>255 ? 255 : ((((-38*R1-74*G1+112*B1+128)>>8)+((-38*R2-74*G2+112*B2+128)>>8))/2 + 128);
//     Y2 = (((66*R2+129*G2+25*B2+128)>>8) + 16)>255 ? 255 : ((66*R2+129*G2+25*B2+128)>>8) + 16;
//     V1 = ((((112*R1-94*G1-18*B1+128)>>8) + ((112*R2-94*G2-18*B2+128)>>8))/2 + 128)>255 ? 255 : ((((112*R1-94*G1-18*B1+128)>>8) + ((112*R2-94*G2-18*B2+128)>>8))/2 + 128);
//     *(pYUVData+i*width*3+j*6) = Y1;
//     *(pYUVData+i*width*3+j*6+1) = U1;
//     *(pYUVData+i*width*3+j*6+2) = Y2;
//     *(pYUVData+i*width*3+j*6+3) = V1;
//     *(pYUVData+i*width*3+j*6+4) = alpha;
//     *(pYUVData+i*width*3+j*6+5) = alpha;
//    }
//   } 
//  }
// }
// else
// {
//  if (alphaRGB)
//  {
//   for (int i=0; i<height; ++i)
//   {
//    for (int j=0; j<width/2; ++j)
//    {
//     B1 = *(pRGBData+(height-i-1)*width*4+j*8);
//     G1 = *(pRGBData+(height-i-1)*width*4+j*8+1);
//     R1 = *(pRGBData+(height-i-1)*width*4+j*8+2);
//     B2 = *(pRGBData+(height-i-1)*width*4+j*8+4);
//     G2 = *(pRGBData+(height-i-1)*width*4+j*8+5);
//     R2 = *(pRGBData+(height-i-1)*width*4+j*8+6);
//     Y1 = (((66*R1+129*G1+25*B1+128)>>8) + 16) > 255 ? 255 : (((66*R1+129*G1+25*B1+128)>>8) + 16);
//     U1 = ((((-38*R1-74*G1+112*B1+128)>>8)+((-38*R2-74*G2+112*B2+128)>>8))/2 + 128)>255 ? 255 : ((((-38*R1-74*G1+112*B1+128)>>8)+((-38*R2-74*G2+112*B2+128)>>8))/2 + 128);
//     Y2 = (((66*R2+129*G2+25*B2+128)>>8) + 16)>255 ? 255 : ((66*R2+129*G2+25*B2+128)>>8) + 16;
//     V1 = ((((112*R1-94*G1-18*B1+128)>>8) + ((112*R2-94*G2-18*B2+128)>>8))/2 + 128)>255 ? 255 : ((((112*R1-94*G1-18*B1+128)>>8) + ((112*R2-94*G2-18*B2+128)>>8))/2 + 128);
//     *(pYUVData+i*width*2+j*4) = Y1;
//     *(pYUVData+i*width*2+j*4+1) = U1;
//     *(pYUVData+i*width*2+j*4+2) = Y2;
//     *(pYUVData+i*width*2+j*4+3) = V1;
//    }
//   } 
//  }
//  else
//  {
//   for (int i=0; i<height; ++i)
//   {
//    for (int j=0; j<width/2; ++j)
//    {
//     B1 = *(pRGBData+(height-i-1)*width*3+j*6);
//     G1 = *(pRGBData+(height-i-1)*width*3+j*6+1);
//     R1 = *(pRGBData+(height-i-1)*width*3+j*6+2);
//     B2 = *(pRGBData+(height-i-1)*width*3+j*6+3);
//     G2 = *(pRGBData+(height-i-1)*width*3+j*6+4);
//     R2 = *(pRGBData+(height-i-1)*width*3+j*6+5);
//     Y1 = (((66*R1+129*G1+25*B1+128)>>8) + 16) > 255 ? 255 : (((66*R1+129*G1+25*B1+128)>>8) + 16);
//     U1 = ((((-38*R1-74*G1+112*B1+128)>>8)+((-38*R2-74*G2+112*B2+128)>>8))/2 + 128)>255 ? 255 : ((((-38*R1-74*G1+112*B1+128)>>8)+((-38*R2-74*G2+112*B2+128)>>8))/2 + 128);
//     Y2 = (((66*R2+129*G2+25*B2+128)>>8) + 16)>255 ? 255 : ((66*R2+129*G2+25*B2+128)>>8) + 16;
//     V1 = ((((112*R1-94*G1-18*B1+128)>>8) + ((112*R2-94*G2-18*B2+128)>>8))/2 + 128)>255 ? 255 : ((((112*R1-94*G1-18*B1+128)>>8) + ((112*R2-94*G2-18*B2+128)>>8))/2 + 128);
//     *(pYUVData+i*width*2+j*4) = Y1;
//     *(pYUVData+i*width*2+j*4+1) = U1;
//     *(pYUVData+i*width*2+j*4+2) = Y2;
//     *(pYUVData+i*width*2+j*4+3) = V1;
//    }
//   } 
//  }
// }
// return 0;
//}
//
////////////////////////////////////////////////////////////////////////////
//// pGBYUV   point to the background YUV data
//// pFGYUV   point to the foreground YUV data
//// width   width of the picture
//// height   height of the picture
//// alphaBG   is there an alpha channel in background YUV data
//// alphaFG   is there an alpha channel in fourground YUV data
////////////////////////////////////////////////////////////////////////////
//int YUVBlending(void* pBGYUV, void* pFGYUV, int width, int height, bool alphaBG, bool alphaFG)
//{
// if (NULL == pBGYUV || NULL == pFGYUV)
// {
//  return -1;
// }
// unsigned char* pBGData = (unsigned char*)pBGYUV;
// unsigned char* pFGData = (unsigned char*)pFGYUV;
// if (!alphaFG)
// {
//  if (!alphaBG)
//  {
//   memcpy(pBGData, pFGData, width*height*2);
//  }
//  else
//  {
//   for (int i=0; i<height; ++i)
//   {
//    for (int j=0; j<width/2; ++j)
//    {
//     *(pBGData+i*width*2+j*4) = *(pFGData+i*width*2+j*4);
//     *(pBGData+i*width*2+j*4+1) = *(pFGData+i*width*2+j*4+1);
//     *(pBGData+i*width*2+j*4+2) = *(pFGData+i*width*2+j*4+2);
//     *(pBGData+i*width*2+j*4+3) = *(pFGData+i*width*2+j*4+3);
//    }
//   }
//  }
// }
// int Y11, U11, V11, Y12, Y21, U21, V21, Y22;
// int alpha1, alpha2;
// if (!alphaBG)
// {
//  for (int i=0; i<height; ++i)
//  {
//   for (int j=0; j<width/2; ++j)
//   {
//    Y11 = *(pBGData+i*width*2+j*4);
//    U11 = *(pBGData+i*width*2+j*4+1);
//    Y12 = *(pBGData+i*width*2+j*4+2);
//    V11 = *(pBGData+i*width*2+j*4+3);
//
//    Y21 = *(pFGData+i*width*3+j*6);
//    U21 = *(pFGData+i*width*3+j*6+1);
//    Y22 = *(pFGData+i*width*3+j*6+2);
//    V21 = *(pFGData+i*width*3+j*6+3);
//    alpha1 = *(pFGData+i*width*3+j*6+4);
//    alpha2 = *(pFGData+i*width*3+j*6+5);
//
//    *(pBGData+i*width*2+j*4) = (Y21-16)*alpha1/255+(Y11-16)*(255-alpha1)/255+16;
//    *(pBGData+i*width*2+j*4+1) = ((U21-128)*alpha1/255+(U11-128)*(255-alpha1)/255 + (U21-128)*alpha2/255+(U11-128)*(255-alpha2)/255)/2+128;
//    *(pBGData+i*width*2+j*4+3) = ((V21-128)*alpha1/255+(V11-128)*(255-alpha1)/255 + (V21-128)*alpha2/255+(V11-128)*(255-alpha2)/255)/2+128;
//    *(pBGData+i*width*2+j*4+2) = (Y22-16)*alpha2/255+(Y12-16)*(255-alpha2)/255+16;
//   }
//  }
// }
// else
// {
//  for (int i=0; i<height; ++i)
//  {
//   for (int j=0; j<width/2; ++j)
//   {
//    Y11 = *(pBGData+i*width*3+j*6);
//    U11 = *(pBGData+i*width*3+j*6+1);
//    Y12 = *(pBGData+i*width*3+j*6+2);
//    V11 = *(pBGData+i*width*3+j*6+3);
//
//    Y21 = *(pFGData+i*width*3+j*6);
//    U21 = *(pFGData+i*width*3+j*6+1);
//    Y22 = *(pFGData+i*width*3+j*6+2);
//    V21 = *(pFGData+i*width*3+j*6+3);
//    alpha1 = *(pFGData+i*width*3+j*6+4);
//    alpha2 = *(pFGData+i*width*3+j*6+5);
//
//    *(pBGData+i*width*3+j*6) = (Y21-16)*alpha1/255+(Y11-16)*(255-alpha1)/255+16;
//    *(pBGData+i*width*3+j*6+1) = ((U21-128)*alpha1/255+(U11-128)*(255-alpha1)/255 + (U21-128)*alpha2/255+(U11-128)*(255-alpha2)/255)/2+128;
//    *(pBGData+i*width*3+j*6+3) = ((V21-128)*alpha1/255+(V11-128)*(255-alpha1)/255 + (V21-128)*alpha2/255+(V11-128)*(255-alpha2)/255)/2+128;
//    *(pBGData+i*width*3+j*6+2) = (Y22-16)*alpha2/255+(Y12-16)*(255-alpha2)/255+16;
//   }
//  }
// }
// return 0;
//}

