unit uJxdCapEffect;

interface
uses
  Windows, SysUtils, Graphics, Classes, uJxdCapEffectBasic, GDIPAPI, GDIPOBJ;

type
  PArgbInfo = ^TArgbInfo;
  TArgbInfo = packed record
    FBlue, 
    FGreen, 
    FRed, 
    FAlpha: Byte;
  end;
  PBmpInfo = ^TBmpInfo;
  TBmpInfo = record
    FBmp: HBITMAP;
    FPauseTime: Cardinal;
  end;
  TAryBmpInfos = array of TBmpInfo;

  PEffectBitmapItem = ^TEffectBitmapItem;
  TEffectBitmapItem = record
    FID: Integer; 
    FBmpDC: HDC; //使用的内存DC
    FCurIndex: Integer; //当前指向的项
    FBmpCount: Integer; //图像数量
    FShowRect: TGPRect; //显示位置
    FNextChangedTime: Cardinal; //下次超时换图像时间
    FBmpInfos: TAryBmpInfos; //BMP的信息
  end;
  
  TxdCapEffect = class(TxdCapEffectBasic)
  public    
    constructor Create;
    destructor  Destroy; override;

    //拍照
    function SnapShot(var ABmp: TGPBitmap): Boolean;

    //添加特效，返回特效ID; AForceAlpha: 强制指定的图像半透明，否则根据图像格式进行转变
    function  AddEffectFile(const AFileName: string; const AShowPos: TPoint; const AForceAlpha: Boolean = False): Integer; overload;
    function  AddEffectFile(const AImg: TGPBitmap; const AShowPos: TPoint; const AForceAlpha: Boolean = False): Integer; overload;
    //修改特效显示位置
    procedure ChangedEffectPos(const AID: Integer; const ANewPos: TPoint); 
    //删除特效
    procedure DeleteEffect(const AID: Integer);   

    //实现接口
    procedure DoOverlayEffer(Sender: TObject; MemDC: HDC; const AWidth, AHeight: Integer); override;  
  private
    FLock: TRTLCriticalSection;
    FEffectList: TList;
    FCurID: Integer;
    FIsShapShot: Boolean;
    FShapShotBmp: TBitmap;
    procedure FreeEffectItem(Ap: PEffectBitmapItem);
  end;  

implementation

uses
  Forms;

{ TxdCapEffect }

function TxdCapEffect.AddEffectFile(const AFileName: string; const AShowPos: TPoint; const AForceAlpha: Boolean): Integer;
var
  bmp: TGPBitmap;  
begin
  bmp := TGPBitmap.Create( AFileName );
  try
    Result := AddEffectFile( bmp, AShowPos, AForceAlpha );
  finally
    FreeAndNil( bmp );
  end;
end;

function TxdCapEffect.AddEffectFile(const AImg: TGPBitmap; const AShowPos: TPoint; const AForceAlpha: Boolean): Integer;
var
  G: TGPGraphics;
  temp: TGPBitmap;  
  i, nW, nH, nCount, x, y: Integer;
  guid: TGUID;
  pItem: PEffectBitmapItem;
  bmpData: TBitmapData;
  P: PCardinal;
  nSize: Integer;
  pTimeItem: PPropertyItem;
  pPauseTime: PInteger;
begin
  Result := -1;
  try
    if AImg.GetPixelFormat = 0 then Exit;

    nW := AImg.GetWidth;
    nH := AImg.GetHeight;
    if (nW = 0) or (nH = 0) then Exit;
    
    AImg.GetFrameDimensionsList( @guid, 1 );
    nCount := AImg.GetFrameCount( guid );
    if nCount = 0 then Exit;
    
    //新创建特效
    New( pItem );
    pItem^.FID := FCurID;
    Inc( FCurID );
    pItem^.FBmpDC := CreateCompatibleDC( 0 );
    pItem^.FCurIndex := -1;
    pItem^.FBmpCount := nCount;
    pItem^.FShowRect := MakeRect(AShowPos.X, AShowPos.Y, nW, nH);
    pItem^.FNextChangedTime := 0;
    SetLength(pItem^.FBmpInfos, nCount);

    //获取特效的HBitmap
    temp := TGPBitmap.Create( nW, nH );
    G := TGPGraphics.Create( temp );    
    for i := 0 to nCount - 1 do
    begin
      AImg.SelectActiveFrame( guid, i );
      G.DrawImage( AImg, 0, 0, nW, nH );

      if AForceAlpha then
      begin
        temp.LockBits( MakeRect(0, 0, nW, nH), ImageLockModeRead or ImageLockModeWrite, PixelFormat32bppARGB, bmpData);
        try
          P := bmpData.Scan0;
          for y := 1 to bmpData.Height do
          begin
            for x := 1 to bmpData.Width do
            begin
              //计算不透明度，三种方法都试了
              with TArgbInfo(P^) do
              begin
    //          FAlpha := Max(FRed, Max(FGreen, FBlue));
//                FAlpha := (FRed + FGreen + FBlue) div 3;
                FAlpha := (306 * FRed + 601 * FGreen + 117 * FBlue) div 1024;
              end;
              Inc(P);
            end;
          end;
        finally
          temp.UnlockBits(bmpData);
        end;
      end;  //end: if AForceAlpha then
      temp.GetHBITMAP( 0, pItem^.FBmpInfos[i].FBmp );
    end;

    //获取特效存在时间
    if pItem^.FBmpCount = 1 then
      pItem^.FBmpInfos[0].FPauseTime := $FFFFFFFF
    else
    begin
      nSize := AImg.GetPropertyItemSize( PropertyTagFrameDelay );
      if nSize = 0 then Exit;
      GetMem( pTimeItem, nSize );
      try
        AImg.GetPropertyItem( PropertyTagFrameDelay, nSize, pTimeItem ); 
        pPauseTime := pTimeItem^.value;
        for i := 0 to nCount - 1 do
        begin
          pItem^.FBmpInfos[i].FPauseTime := pPauseTime^ * 10;
          Inc( pPauseTime );
        end;
      finally
        FreeMem( pTimeItem );
      end;  
    end;

    EnterCriticalSection( FLock );
    try
      FEffectList.Add( pItem );
      Result := pItem^.FID;
    finally
      LeaveCriticalSection( FLock );
    end;
  finally
    FreeAndNil( temp );
    FreeAndNil( G );
  end;
end;

procedure TxdCapEffect.ChangedEffectPos(const AID: Integer; const ANewPos: TPoint);
var
  i: Integer;
  p: PEffectBitmapItem;
begin
  EnterCriticalSection( FLock );
  try
    for i := 0 to FEffectList.Count - 1 do
    begin
      p := FEffectList[i];
      if p^.FID = AID then
      begin
        p^.FShowRect.X := ANewPos.X;
        p^.FShowRect.Y := ANewPos.Y;
        Break;
      end;
    end;  
  finally
    LeaveCriticalSection( FLock );
  end;
end;

constructor TxdCapEffect.Create;
begin
  FEffectList := TList.Create;
  FCurID := GetTickCount;
  FIsShapShot := False;
  FShapShotBmp := nil;
  InitializeCriticalSection( FLock );
end;

procedure TxdCapEffect.DeleteEffect(const AID: Integer);
var
  i: Integer;
  p: PEffectBitmapItem;
begin
  EnterCriticalSection( FLock );
  try
    for i := 0 to FEffectList.Count - 1 do
    begin
      p := FEffectList[i];
      if p^.FID = AID then
      begin
        FreeEffectItem( p );
        FEffectList.Delete( i );
        Break;
      end;
    end;  
  finally
    LeaveCriticalSection( FLock );
  end;
end;

destructor TxdCapEffect.Destroy;
begin
  DeleteCriticalSection( FLock );
  inherited;
end;

procedure TxdCapEffect.DoOverlayEffer(Sender: TObject; MemDC: HDC; const AWidth, AHeight: Integer);
var
  i: Integer;
  p: PEffectBitmapItem;
  blend: TBlendFunction;
  dwTime: Cardinal;  
begin
  if FEffectList.Count = 0 then Exit;
  
  EnterCriticalSection( FLock );
  try
    dwTime := GetTickCount;
    for i := 0 to FEffectList.Count - 1 do
    begin
      p := FEffectList[i];
      if p^.FNextChangedTime = 0 then
      begin
        p^.FCurIndex := 0;
        p^.FNextChangedTime := p^.FBmpInfos[p^.FCurIndex].FPauseTime + dwTime;
        SelectObject( p^.FBmpDC, p^.FBmpInfos[p^.FCurIndex].FBmp );
      end
      else if dwTime >= p^.FNextChangedTime then
      begin
        p^.FCurIndex := (p^.FCurIndex + 1) mod p^.FBmpCount;
        p^.FNextChangedTime := p^.FBmpInfos[p^.FCurIndex].FPauseTime + dwTime;
        SelectObject( p^.FBmpDC, p^.FBmpInfos[p^.FCurIndex].FBmp );
      end;

      Blend.BlendOp := AC_SRC_OVER;
      Blend.BlendFlags := 0;
      Blend.AlphaFormat := AC_SRC_ALPHA ;
      Blend.SourceConstantAlpha := $FF;
      
      AlphaBlend( MemDC, p^.FShowRect.X, p^.FShowRect.Y, p^.FShowRect.Width, p^.FShowRect.Height, 
        p^.FBmpDC, 0, 0, p^.FShowRect.Width, p^.FShowRect.Height, blend );

      if FIsShapShot then
      begin
        FIsShapShot := False;
        if not Assigned(FShapShotBmp) then
        begin
          FShapShotBmp := TBitmap.Create;
          FShapShotBmp.Width := AWidth;
          FShapShotBmp.Height := AHeight;
        end;
        BitBlt( FShapShotBmp.Canvas.Handle, 0, 0, AWidth, AHeight, MemDC, 0, 0, SRCCOPY );
      end;
    end;
  finally
    LeaveCriticalSection( FLock );
  end;
end;

procedure TxdCapEffect.FreeEffectItem(Ap: PEffectBitmapItem);
var
  i: Integer;
begin
  for i := Low(Ap^.FBmpInfos) to High(Ap^.FBmpInfos) do
    DeleteObject( Ap^.FBmpInfos[i].FBmp );    
  DeleteObject( Ap^.FBmpDC );  
  Dispose( Ap );
end;

function TxdCapEffect.SnapShot(var ABmp: TGPBitmap): Boolean;
var
  nMaxCount: Integer;
begin
  Result := False;
  if FIsShapShot then Exit;
  nMaxCount := 0;
  FIsShapShot := True;

  Sleep( 5 );
  while FIsShapShot and (nMaxCount < 200) do
  begin
    Inc( nMaxCount );
    Sleep( 5 );
    Application.ProcessMessages;
  end;

  Result := not FIsShapShot;
  if Result then
  begin
    ABmp := TGPBitmap.Create( FShapShotBmp.Handle, FShapShotBmp.Palette ); 
    FreeAndNil( FShapShotBmp );
  end;
end;

end.
