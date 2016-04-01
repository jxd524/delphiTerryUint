unit uJxdGpSub;

interface
uses
  GDIPAPI, GDIPOBJ, SysUtils, Windows, Classes, uJxdGpStyle;

const
  ImgFormatBitmap = 'image/bmp';
  ImgFormatJpg = 'image/jpeg';
  ImgFormatGif = 'image/gif';
  ImgFormatTiff = 'image/tiff';
  ImgFormatPng = 'image/png';

//指定类型图片的GUID
function GetEncoderClsid(const AImgFormat: WideString; var AClsid: TGUID): Boolean; overload;
function GetEncoderClsid(const AImgFormat: WideString): TGUID; overload;

procedure SetGpRect(var AR: TGPRect; const AOffsetX: Integer; const AOffsetY: Integer;
  const AOffsetW: Integer = 0; const AOffsetH: Integer = 0);
function PtInGpRect(const X, Y: Integer; const AR: TGPRect): Boolean; inline; overload;
function PtInGpRect(const X, Y: Integer; const AR: TGPRectF): Boolean; inline; overload;

//生成距型
function GpRectF(const AR: TGPRect): TGPRectF;
function GpRect(const AR: TGPRectF): TGPRect;

//计算字符串的大小
function CalcStringSize(const AGh: TGPGraphics; const AFont: TGPFont; const AInfo: string): TSize;

//根据信息自动拉伸图片
procedure DrawStretchImage(const ADestGp: TGPGraphics; const ASrcImg: TGPImage; const ADestRect, ASrcRect: TGPRect);

//常规图片绘制
procedure DrawImageCommon(const AGh: TGPGraphics; const ADestR: TGPRect; const AImgInfo: TImageInfo; const ADrawMethod: TDrawMethod; 
  const AOnGetState: TOnGetDrawState; const AOnIsDrawSubItem: TOnIsDrawSubItem; const AOnDrawItemText: TOnDrawText;
  const AOnChangedSrcBmpRect: TOnChangedSrcBmpRect);

//倒影图片
procedure InvertedImage(const ASrcBmp, ADestBmp: TGPBitmap; const ASrcR, ADestR: TGPRect;
  const AChangedAlpha: Integer = 0; const AAplhaPer: Double = 1.0);

//改变图片指定位置的Aplha值变化方式
procedure ChangedAplha(const ABmp: TGPBitmap; const AChangedAplhaStyle: Integer; const AChangedDestR: TGPRect; const AAplhaPer: Double = 1.0);

//使用指定的图片来创建形状
function CreateStyleByBitmap(const ABmp: TGPBitmap; const AOnCombiFuc: TOnArgbInfo): HRGN;

//将图片变成灰色
procedure ChangedBmpToGray(const ABmp: TGPBitmap);

//从资源文件载入图片
function LoadFromRes(const AResName: string; AResType: PChar): TGPBitmap;

implementation

uses
  uSysSub;

var
  _TempFileName: TStringList = nil;

function LoadFromRes(const AResName: string; AResType: PChar): TGPBitmap;
var
  res: TResourceStream;
  strFileName: string;
begin
  res := TResourceStream.Create( HInstance, AResName, AResType );
  try
    strFileName := GetRandomTempFileName;
    if not Assigned(_TempFileName) then
      _TempFileName := TStringList.Create;
    _TempFileName.Add( strFileName );
    res.SaveToFile( strFileName );
    Result := TGPBitmap.Create( strFileName );
  finally
    res.Free;
  end;
end;
//var
//  res: TResourceStream;
//  ism: TStreamAdapter;
//begin
//  res := TResourceStream.Create( HInstance, AResName, AResType );
//  try
//    ism := TStreamAdapter.Create( res, soReference );
//    Result := TGPBitmap.Create( ism );
//  finally
//    res.Free;
//  end;
//end;

procedure ChangedBmpToGray(const ABmp: TGPBitmap);
var
  bmpData: TBitmapData;
  P: PArghInfo;
  x, y, nW, nH, Gray: Integer;
begin  
  nW := ABmp.GetWidth;
  nH := ABmp.GetHeight;
  if (nW = 0) or (nH = 0) then Exit;

  ABmp.LockBits( MakeRect(0, 0, nW, nH), ImageLockModeRead, PixelFormat32bppARGB, bmpData);
  try
    P := bmpData.Scan0;
    for y := 0 to bmpData.Height - 1 do
    begin
      for x := 0 to bmpData.Width - 1 do
      begin
        if P^.FAlpha = 0 then Continue;
        
        Gray := ( P^.FRed + P^.FGreen + P^.FBlue ) div 3;        
        PCardinal(P)^ := MakeColor( p^.FAlpha, Gray, Gray, Gray);
        Inc(P);
      end;
    end;
  finally
    ABmp.UnlockBits( bmpData );
  end;
end;

procedure DrawImageCommon(const AGh: TGPGraphics; const ADestR: TGPRect; const AImgInfo: TImageInfo; const ADrawMethod: TDrawMethod; 
  const AOnGetState: TOnGetDrawState; const AOnIsDrawSubItem: TOnIsDrawSubItem; const AOnDrawItemText: TOnDrawText;
  const AOnChangedSrcBmpRect: TOnChangedSrcBmpRect);
var
  BmpR: TGPRect;
  nTemp, nH, nW: Integer;
  i: Integer;
  pItem: PDrawInfo;
  DestR, srcR: TGPRect;
  R: TGPRectF;
  st: TxdGpUIState;

  function GetDrawState(const Ap: PDrawInfo): TxdGpUIState;
  begin
    if Assigned(AOnGetState) then
      Result := AOnGetState( Ap )
    else
      Result := uiNormal;    
  end;

  function IsDrawSubItem(const Ap: PDrawInfo): Boolean;
  begin
    if Assigned(AOnIsDrawSubItem) then
      Result := AOnIsDrawSubItem(Ap)
    else
      Result := False;
  end;

  procedure DoChangedSrcBmp;
  begin
    if Assigned(AOnChangedSrcBmpRect) then
      AOnChangedSrcBmpRect(st, BmpR );
  end;
  
begin
  if not Assigned(AImgInfo.Image) then Exit;

  {
    先处理以下类型的绘制
    dsPaste: ;
    dsStretchByVH: ;
    dsStretchAll: ;
  }
  DestR := ADestR;
  if ADrawMethod.DrawStyle <> dsDrawByInfo then
  begin
    nTemp := Integer(AImgInfo.Image.GetHeight) div AImgInfo.ImageCount;
    st :=  GetDrawState(nil);
    case st of
      uiDown:
      begin
        if AImgInfo.ImageCount >= 3 then
          BmpR := MakeRect(0, nTemp * 2, Integer(AImgInfo.Image.GetWidth), nTemp)
        else
          bmpR := MakeRect(0, nTemp, Integer(AImgInfo.Image.GetWidth), nTemp)
      end;
      uiActive:
      begin
        if AImgInfo.ImageCount >= 2 then
          bmpR := MakeRect(0, nTemp, Integer(AImgInfo.Image.GetWidth), nTemp)
        else
          bmpR := MakeRect(0, 0, Integer(AImgInfo.Image.GetWidth), nTemp)
      end;
      else
        bmpR := MakeRect(0, 0, Integer(AImgInfo.Image.GetWidth), nTemp);
    end;

    DoChangedSrcBmp;

    case ADrawMethod.DrawStyle of
      dsPaste: 
      begin
        if BmpR.Width > DestR.Width then
          nW := DestR.Width
        else
        begin
          nW := BmpR.Width;
          DestR.Width := nW;
        end;

        if BmpR.Height > DestR.Height then
          nH := BmpR.Height
        else
        begin
          nH := BmpR.Height;
          DestR.Height := nH;
        end;

        if ADrawMethod.CenterOnPaste then
        begin
          DestR.X := (ADestR.Width - DestR.Width) div 2;
          DestR.Y := (ADestR.Height - DestR.Height) div 2;
        end;
        
        if (nW > 0) and (nH > 0) and (DestR.Width > 0) and (DestR.Height > 0) then  
          AGh.DrawImage( AImgInfo.Image, DestR, BmpR.X, BmpR.Y, nW, nH, UnitPixel );
      end;
      dsStretchByVH: 
      begin
        if (BmpR.Width > 0) and (BmpR.Height > 0) and (DestR.Width > 0) and (DestR.Height > 0) then 
          DrawStretchImage( AGh, AImgInfo.Image, DestR, BmpR );
      end;
      dsStretchAll:  
      begin
        if (BmpR.Width > 0) and (BmpR.Height > 0) and (DestR.Width > 0) and (DestR.Height > 0) then 
          AGh.DrawImage( AImgInfo.Image, DestR, BmpR.X, BmpR.Y, BmpR.Width, BmpR.Height, UnitPixel );
      end;
    end;
  end
  else
  begin
    //绘制 dsDrawByInfo 类型, 从底层向最顶层绘制
    for i := 0 to ADrawMethod.CurDrawInfoCount - 1 do
    begin
      pItem := ADrawMethod.GetDrawInfo(i);
      if Assigned(pItem) and IsDrawSubItem(pItem) then 
      begin
        //开始计算绘制
        DestR := pItem^.FDestRect;
//        if DestR.X + DestR.Width < ADestR.X then Continue;
//        if DestR.X > ADestR.X + ADestR.Width then Continue;

        if DestR.X < 0 then DestR.X := 0;
        if DestR.Y < 0 then DestR.Y := 0;
        if DestR.Width + DestR.X > ADestR.Width then DestR.Width := ADestR.Width - DestR.X;
        if DestR.Height + DestR.Y > ADestR.Height then DestR.Height := ADestR.Height - DestR.Y;
  
        st := GetDrawState(pItem);
        case st of
          uiDown:
            begin
              if (pItem^.FDownSrcRect.Width > 0) and (pItem^.FDownSrcRect.Height > 0) then
                srcR := pItem^.FDownSrcRect
              else if (pItem^.FActiveSrcRect.Width > 0) and (pItem^.FActiveSrcRect.Height > 0) then
                srcR := pItem^.FActiveSrcRect
              else
                srcR := pItem^.FNormalSrcRect
            end;
          uiActive:
            begin
              if (pItem^.FActiveSrcRect.Width > 0) and (pItem^.FActiveSrcRect.Height > 0) then
                srcR := pItem^.FActiveSrcRect
              else
                srcR := pItem^.FNormalSrcRect;
            end;
          else
            srcR := pItem^.FNormalSrcRect;
        end;

        DoChangedSrcBmp;

        //只处理类型： dsPaste, dsStretchByVH, 直接绘制 其它类型将不进行处理
        if (srcR.Width > 0) and (srcR.Height > 0) then
        begin
          case pItem^.FDrawStyle of
            dsPaste: 
            begin
              if srcR.Width > DestR.Width then
                nW := DestR.Width
              else
              begin
                nW := srcR.Width;
                DestR.Width := nW;
              end;

              if srcR.Height > DestR.Height then
                nH := srcR.Height
              else
              begin
                nH := srcR.Height;
                DestR.Height := nH;
              end;   
              if (nW > 0) and (nH > 0) and (DestR.Width > 0) and (DestR.Height > 0) then     
                AGh.DrawImage( AImgInfo.Image, DestR, srcR.X, srcR.Y, nW, nH, UnitPixel );
            end;
            dsStretchByVH: 
            begin
              if (srcR.Width > 0) and (srcR.Height > 0) and (DestR.Width > 0) and (DestR.Height > 0) then              
                DrawStretchImage( AGh, AImgInfo.Image, DestR, srcR );
            end
            else
            begin
              if (srcR.Width > 0) and (srcR.Height > 0) and (DestR.Width > 0) and (DestR.Height > 0) then 
                AGh.DrawImage( AImgInfo.Image, DestR, srcR.X, srcR.Y, srcR.Width, srcR.Height, UnitPixel );
            end;
          end;
        end;
        //信息绘制
        if (pItem^.FText <> '') and Assigned(AOnDrawItemText) then
        begin
          R.X :=DestR.X;
          R.Y := DestR.Y;
          if DestR.Width <= 0 then
            R.Width := ADestR.Width
          else
            R.Width := DestR.Width;
    
          if DestR.Height <= 0 then
            R.Height := ADestR.Height
          else
            R.Height := DestR.Height;
          AOnDrawItemText( AGh, pItem^.FText, R, st );
        end;        
      end; //End: if Assigned(pItem) and AOnIsDrawSubItem(pItem) then 
    end;
  end;
end;

function CalcStringSize(const AGh: TGPGraphics; const AFont: TGPFont; const AInfo: string): TSize;
var
  R: TGPRectF;
begin
  AGh.MeasureString( AInfo, -1, AFont, MakeRect(0.0, 0, 0, 0), R );
  Result.cx := Round(R.Width);
  Result.cy := Round(R.Height);
end;

function GpRectF(const AR: TGPRect): TGPRectF;
begin
  Result.X := AR.X;
  Result.Y := AR.Y;
  Result.Width := AR.Width;
  Result.Height := AR.Height;
end;

function GpRect(const AR: TGPRectF): TGPRect;
begin
  Result.X := Round(AR.X);
  Result.Y := Round(AR.Y);
  Result.Width := Round(AR.Width);
  Result.Height := Round(AR.Height);
end;

procedure SetGpRect(var AR: TGPRect; const AOffsetX, AOffsetY, AOffsetW, AOffsetH: Integer);
begin
  Inc( AR.X, AOffsetX );
  Inc( AR.Y, AOffsetY );
  Inc( AR.Width, AOffsetW );
  Inc( AR.Height, AOffsetH );
end;

function PtInGpRect(const X, Y: Integer; const AR: TGPRect): Boolean; 
begin
  Result := (X >= AR.X) and (X < AR.X + AR.Width) and
       (Y >= AR.Y) and (Y < AR.Y + AR.Height);
end;

function PtInGpRect(const X, Y: Integer; const AR: TGPRectF): Boolean;
begin
  Result := (X >= AR.X) and (X <= AR.X + AR.Width) and
       (Y >= AR.Y) and (Y <= AR.Y + AR.Height);
end;

procedure DrawStretchImage(const ADestGp: TGPGraphics; const ASrcImg: TGPImage; const ADestRect, ASrcRect: TGPRect);
var
  SrcR, DestR: TGPRect;
  nSrcW2, nSrcH2: Integer;
begin  
  if ADestRect.Width <= ASrcRect.Width then
  begin
    //目标宽度比源宽度小或相等
    if ADestRect.Height <= ASrcRect.Height then
    begin
      ADestGp.DrawImage( ASrcImg, ADestRect, ASrcRect.X, ASrcRect.Y, ASrcRect.Width, ASrcRect.Height, UnitPixel )
    end
    else
    begin
      //上
      SrcR := ASrcRect;
      SrcR.Height := SrcR.Height div 2;
      DestR := ADestRect;
      DestR.Height := SrcR.Height;
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );

      //中
      SrcR.Y := SrcR.Y + SrcR.Height;
      SrcR.Height := 1;
      DestR.Y := DestR.Y + DestR.Height;
      DestR.Height := ADestRect.Height - DestR.Height * 2;
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );

      //下
      SrcR.Y := SrcR.Y;
      SrcR.Height := ASrcRect.Height - (SrcR.Y - ASrcRect.Y);
      DestR.Y := DestR.Y + DestR.Height;
      DestR.Height := SrcR.Height;
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );
    end;
  end
  else //END: if ADestRect.Width <= ASrcRect.Width then
  begin
    //目标宽度比源宽度大
    if ADestRect.Height <= ASrcRect.Height then
    begin
      SrcR := ASrcRect;
      SrcR.Width := SrcR.Width div 2;
      DestR := ADestRect;
      DestR.Width := SrcR.Width;
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );

      SrcR.X := SrcR.X + SrcR.Width;
      SrcR.Width := 1;
      DestR.X := DestR.X + DestR.Width;
      DestR.Width := ADestRect.Width - DestR.Width * 2;
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );

//      SrcR.X := SrcR.X;
      SrcR.Width := (ASrcRect.X + ASrcRect.Width) - SrcR.X + SrcR.Width mod 2;
      DestR.X := DestR.X + DestR.Width;
      DestR.Width := SrcR.Width;
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );
    end
    else
    begin
      //目标宽度与高度都比源宽度高度大时
      nSrcW2 := ASrcRect.Width div 2;
      nSrcH2 := ASrcRect.Height div 2;
      
      //左上角
      SrcR := MakeRect( ASrcRect.X, ASrcRect.Y, nSrcW2, nSrcH2 );
      DestR := MakeRect( ADestRect.X, ADestRect.Y, nSrcW2, nSrcH2 );
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );
      //右上角
      SrcR.X := nSrcW2 + ASrcRect.Width mod 2;
      Inc(DestR.X, ADestRect.Width - DestR.Width);
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );
      //上拉伸      
      SrcR.X := ASrcRect.X + nSrcW2;
      SrcR.Width := 1;
      DestR.X := ADestRect.X + nSrcW2;
      DestR.Width := ADestRect.Width - 2 * nSrcW2;
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );

      //左下角
      SrcR := MakeRect( ASrcRect.X, ASrcRect.Y + nSrcH2, nSrcW2, nSrcH2 + ASrcRect.Height mod 2 );
      DestR := MakeRect( ADestRect.X, ADestRect.Y + ADestRect.Height - nSrcH2, nSrcW2, nSrcH2 );
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );
      //右下角
      SrcR.X := nSrcW2 + ASrcRect.Width mod 2;
      Inc(DestR.X, ADestRect.Width - DestR.Width);
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );
      //下拉伸      
      SrcR.X := ASrcRect.X + nSrcW2;
      SrcR.Width := 1;
      DestR.X := ADestRect.X + nSrcW2;
      DestR.Width := ADestRect.Width - 2 * nSrcW2;
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );

      //左边 - 中间绘制
      SrcR := MakeRect( ASrcRect.X, ASrcRect.Y + nSrcH2 + ASrcRect.Height mod 2, nSrcW2, 1 );
      DestR := MakeRect( ADestRect.X, ADestRect.Y + nSrcH2, nSrcW2, ADestRect.Height - nSrcH2 * 2 );
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );
      //右边 - 中间绘制
      SrcR.X := nSrcW2 + ASrcRect.Width mod 2;
      Inc(DestR.X, ADestRect.Width - DestR.Width);
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );
      //中间 - 中间绘制
      SrcR := MakeRect( ASrcRect.X + nSrcW2 + ASrcRect.Width mod 2, ASrcRect.Y + nSrcH2 + ASrcRect.Height mod 2, 1, 1 );
      DestR.X := ADestRect.X + nSrcW2;
      DestR.Width := ADestRect.Width - 2 * nSrcW2;
      ADestGp.DrawImage( ASrcImg, DestR, SrcR.X, SrcR.Y, SrcR.Width, SrcR.Height, UnitPixel );
    end;
  end;
end;

function GetEncoderClsid(const AImgFormat: WideString; var AClsid: TGUID): Boolean;
var
  nNum, nSize, i: Cardinal;
  p, pTemp: PImageCodecInfo;
begin
  Result := False;
  nNum := 0;
  nSize := 0;
  FillChar( AClsid, SizeOf(TGuID), 0 );
  GetImageEncodersSize( nNum, nSize );
  if (nNum = 0) or (nSize = 0) then Exit;
  GetMem( p, nSize );
  try
    GetImageEncoders( nNum, nSize, p );
    pTemp := p;
    for i := 0 to nNum - 1 do
    begin
      if CompareText(pTemp^.MimeType, AImgFormat) = 0 then
      begin
        AClsid := pTemp^.Clsid;
        Result := True;
        Break;
      end;
      Inc( pTemp );
    end;
  finally
    FreeMem( p, nSize );
  end;
end;

function GetEncoderClsid(const AImgFormat: WideString): TGUID; overload;
begin
  GetEncoderClsid( AImgFormat, Result );
end;

procedure ChangedAplha(const ABmp: TGPBitmap; const AChangedAplhaStyle: Integer; const AChangedDestR: TGPRect; const AAplhaPer: Double = 1.0);
var
  nStep: Double;
  bmpData: TBitmapData;
  row, col: Integer;
  pPixel: PCardinal;
  cl, nAplha: Cardinal;
begin
  {
  AChangedAlpha: 0 不改变
  AChangedAlpha：1 从上向下透明
  AChangedAlpha：2：从下向上透明

  AChangedAlpha：3：从左向右透明
  AChangedAlpha：其它：从右向左透明
end;}
  if AChangedAplhaStyle <> 0 then
  begin
    if AChangedAplhaStyle in [1, 2] then
    begin
      //上下
      nStep := 255 / AChangedDestR.Height;
      ABmp.LockBits( AChangedDestR, ImageLockModeRead or ImageLockModeWrite, PixelFormat32bppARGB, bmpData );
      pPixel := bmpData.Scan0;
      for row := 0 to AChangedDestR.Height - 1 do
      begin
        nAplha := Round(row * nStep) mod 255;
        if AChangedAplhaStyle = 1 then
          nAplha := 255 - nAplha;
        nAplha := Round( nAplha * AAplhaPer);
        nAplha := nAplha shl 24;
        for col := 0 to AChangedDestR.Width - 1 do
        begin
          cl := PCardinal( Integer(pPixel) + (row * bmpData.Stride div 4 + col)*4 )^;
          if cl shr 24 <> 0 then
          begin
            cl := (cl and $00FFFFFF) or nAplha;
            PCardinal( Integer(pPixel) + (row * bmpData.Stride div 4 + col)*4 )^ := cl;
          end;
        end;
      end;
      ABmp.UnlockBits( bmpData );
    end
    else
    begin
      //左右
      nStep := 255 / AChangedDestR.Width;

      //从左向右透明
      if AChangedAplhaStyle = 3 then
      begin
        ABmp.LockBits( AChangedDestR, ImageLockModeRead or ImageLockModeWrite, PixelFormat32bppARGB, bmpData );
        pPixel := bmpData.Scan0;
        for row := 0 to AChangedDestR.Height - 1 do
        begin
          for col := 0 to AChangedDestR.Width - 1 do
          begin
            cl := PCardinal( Integer(pPixel) + (row * bmpData.Stride div 4 + col)*4 )^;
            if cl shr 24 <> 0 then
            begin
              nAplha := Round(col * nStep) mod 255;
              nAplha := Round( nAplha * AAplhaPer);
              nAplha := nAplha shl 24;
              cl := (cl and $00FFFFFF) or nAplha;
              PCardinal( Integer(pPixel) + (row * bmpData.Stride div 4 + col)*4 )^ := cl;
            end;
          end;
        end;
        ABmp.UnlockBits( bmpData );
      end
      else
      begin
        ABmp.LockBits( AChangedDestR, ImageLockModeRead or ImageLockModeWrite, PixelFormat32bppARGB, bmpData );
        pPixel := bmpData.Scan0;
        for row := 0 to AChangedDestR.Height - 1 do
        begin
          for col := 0 to AChangedDestR.Width - 1 do
          begin
            cl := PCardinal( Integer(pPixel) + (row * bmpData.Stride div 4 + col)*4 )^;
            if cl shr 24 <> 0 then
            begin
              nAplha := 255 - Round(col * nStep) mod 255;
              nAplha := Round( nAplha * AAplhaPer);
              nAplha := nAplha shl 24;
              cl := (cl and $00FFFFFF) or nAplha;
              PCardinal( Integer(pPixel) + (row * bmpData.Stride div 4 + col)*4 )^ := cl;
            end;
          end;
        end;
        ABmp.UnlockBits( bmpData );
      end;
    end;
  end;
end;

procedure InvertedImage(const ASrcBmp, ADestBmp: TGPBitmap; const ASrcR, ADestR: TGPRect;
  const AChangedAlpha: Integer; const AAplhaPer: Double);
var
  gh: TGPGraphics;
  pts: array[0..2] of TGPPoint;
  nStep: Double;
  bmpData: TBitmapData;
  row, col: Integer;
  pPixel: PCardinal;
  cl, nAplha: Cardinal;
begin
  gh := TGPGraphics.Create( ADestBmp );
  pts[0] := MakePoint(ADestR.X, ADestR.Y + ADestR.Height);
  pts[1] := MakePoint(ADestR.X + ADestR.Width, Pts[0].Y );
  pts[2] := MakePoint(ADestR.X, ADestR.Y);
  gh.DrawImage( ASrcBmp, @pts, 3, ASrcR.X, ASrcR.Y, ASrcR.Width, ASrcR.Height, UnitPixel );
  gh.Free;
  {
  AChangedAlpha: 0 不改变
  AChangedAlpha：1 从上向下透明
  AChangedAlpha：2：从下向上透明

  AChangedAlpha：3：从左向右透明
  AChangedAlpha：其它：从右向左透明
end;}
  if AChangedAlpha <> 0 then
  begin
    if AChangedAlpha in [1, 2] then
    begin
      //上下
      nStep := 255 / ADestR.Height;
      ADestBmp.LockBits( ADestR, ImageLockModeRead or ImageLockModeWrite, PixelFormat32bppARGB, bmpData );
      pPixel := bmpData.Scan0;
      for row := 0 to ADestR.Height - 1 do
      begin
        nAplha := Round(row * nStep) mod 255;
        if AChangedAlpha = 1 then
          nAplha := 255 - nAplha;
        nAplha := Round( nAplha * AAplhaPer);
        nAplha := nAplha shl 24;
        for col := 0 to ADestR.Width - 1 do
        begin
          cl := PCardinal( Integer(pPixel) + (row * bmpData.Stride div 4 + col)*4 )^;
          if cl shr 24 <> 0 then
          begin
            cl := (cl and $00FFFFFF) or nAplha;
            PCardinal( Integer(pPixel) + (row * bmpData.Stride div 4 + col)*4 )^ := cl;
          end;
        end;
      end;
      ADestBmp.UnlockBits( bmpData );
    end
    else
    begin
      //左右
      nStep := 255 / ADestR.Width;

      //从左向右透明
      if AChangedAlpha = 3 then
      begin
        ADestBmp.LockBits( ADestR, ImageLockModeRead or ImageLockModeWrite, PixelFormat32bppARGB, bmpData );
        pPixel := bmpData.Scan0;
        for row := 0 to ADestR.Height - 1 do
        begin
          for col := 0 to ADestR.Width - 1 do
          begin
            cl := PCardinal( Integer(pPixel) + (row * bmpData.Stride div 4 + col)*4 )^;
            if cl shr 24 <> 0 then
            begin
              nAplha := Round(col * nStep) mod 255;
              nAplha := Round( nAplha * AAplhaPer);
              nAplha := nAplha shl 24;
              cl := (cl and $00FFFFFF) or nAplha;
              PCardinal( Integer(pPixel) + (row * bmpData.Stride div 4 + col)*4 )^ := cl;
            end;
          end;
        end;
        ADestBmp.UnlockBits( bmpData );
      end
      else
      begin
        ADestBmp.LockBits( ADestR, ImageLockModeRead or ImageLockModeWrite, PixelFormat32bppARGB, bmpData );
        pPixel := bmpData.Scan0;
        for row := 0 to ADestR.Height - 1 do
        begin
          for col := 0 to ADestR.Width - 1 do
          begin
            cl := PCardinal( Integer(pPixel) + (row * bmpData.Stride div 4 + col)*4 )^;
            if cl shr 24 <> 0 then
            begin
              nAplha := 255 - Round(col * nStep) mod 255;
              nAplha := Round( nAplha * AAplhaPer);
              nAplha := nAplha shl 24;
              cl := (cl and $00FFFFFF) or nAplha;
              PCardinal( Integer(pPixel) + (row * bmpData.Stride div 4 + col)*4 )^ := cl;
            end;
          end;
        end;
        ADestBmp.UnlockBits( bmpData );
      end;
    end;
  end;
end;

function CreateStyleByBitmap(const ABmp: TGPBitmap; const AOnCombiFuc: TOnArgbInfo): HRGN;
var
  tempH: HRGN;  
  bmpData: TBitmapData;
  P: PArghInfo;
  x, y, nW, nH: Integer;
begin  
  Result := 0;
  
  nW := ABmp.GetWidth;
  nH := ABmp.GetHeight;
  if (nW = 0) or (nH = 0) then Exit;
  
  
  Result := CreateRectRgn(0, 0, nW, nH );
  ABmp.LockBits( MakeRect(0, 0, nW, nH), ImageLockModeRead, PixelFormat32bppARGB, bmpData);
  try
    P := bmpData.Scan0;
    for y := 0 to bmpData.Height - 1 do
    begin
      for x := 0 to bmpData.Width - 1 do
      begin
        if AOnCombiFuc(p) then
        begin
          tempH := CreateRectRgn(x, y, x+1, y+1);
          CombineRgn(Result, Result, tempH,  RGN_XOR);
          DeleteObject( tempH );
        end;
        Inc(P);
      end;
    end;
  finally
    ABmp.UnlockBits( bmpData );
  end;
end;

initialization
finalization
  FreeAndNil( _TempFileName );
end.
