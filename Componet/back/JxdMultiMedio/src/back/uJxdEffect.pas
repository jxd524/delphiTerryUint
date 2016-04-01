unit uJxdEffect;

interface
uses
  Windows, Graphics, Classes, GDIPOBJ, GDIPAPI, GDIPUTIL;

type
  TxdEffect = class
  public
    procedure DoDraw(Sender: TObject; AMemDC: HDC; const AWidth, AHeight: Integer);

    constructor Create(AOwnerHandle: THandle);
    destructor  Destroy; override;
  private
    FSrcImage: TGpBitmap;
    g: TGPGraphics;
    ImgAtt: TGPImageAttributes;

    FMemCanvas: TCanvas;
    FBmp: TBitmap;
  end;

implementation

{ TxdEffect }

constructor TxdEffect.Create(AOwnerHandle: THandle);
var
  Data: TBitmapData;
   p: PByte;
  x, y, Offset: Integer;
  W, H: Integer;
  R: TGPRect;
begin
  FSrcImage := TGpBitmap.Create( 'Snap1.bmp' );
  FBmp := TBitmap.Create;
  FBmp.LoadFromFile( 'Snap1.bmp' );
  FMemCanvas := TCanvas.Create;
  g := nil;

  R.X := 0;
    R.Y := 0;
    R.Width := FSrcImage.GetWidth;
    R.Height := FSrcImage.GetHeight;

  W := FSrcImage.GetWidth;
  H := FSrcImage.GetHeight;
  FSrcImage.LockBits(R, 1 or 2, PixelFormat32bppARGB, Data );
  p := Data.Scan0;
  inc(p, 3);
  Offset := Data.Stride - W * 4;
  for y := 0 to H - 1 do
  begin
    for x := 0 to W - 1 do
    begin
      p^ := 255 * x div W;
      Inc(p, 4);
    end;
    Inc(p, Offset);
  end;
  FSrcImage.UnlockBits(Data);

end;

destructor TxdEffect.Destroy;
begin
  FSrcImage.Free;
  inherited;
end;

const
  ColorMatrix: TColorMatrix =
   ((1.0, 0.0, 0.0, 0.0, 0.0),
    (0.0, 1.0, 0.0, 0.0, 0.0),
    (0.0, 0.0, 1.0, 0.0, 0.0),
    (0.0, 0.0, 0.0, 0.5, 0.0),
    (0.0, 0.0, 0.0, 0.0, 1.0));


procedure TxdEffect.DoDraw(Sender: TObject; AMemDC: HDC; const AWidth, AHeight: Integer);
var
  BmpRect: TRect;
  R: TGPRect;
begin
  BmpRect := Rect( 0, 0, FSrcImage.GetWidth, FSrcImage.GetHeight );
  if g = nil then
  begin
    g := TGPGraphics.Create( AMemDC );
    FMemCanvas.Handle := AMemDC;
    ImgAtt := TGPImageAttributes.Create;
    ImgAtt.SetColorMatrix(ColorMatrix);
    R.X := 0;
    R.Y := 0;
    R.Width := FSrcImage.GetWidth;
    R.Height := FSrcImage.GetHeight;
  end;
  FMemCanvas.Brush.Style := bsClear;
  FMemCanvas.BrushCopy( BmpRect, FBmp, BmpRect, clFuchsia );
//  g.DrawImage( FSrcImage, 0, 0, FSrcImage.GetWidth, FSrcImage.GetHeight  );
//  g.DrawImage(FSrcImage, R,
//              0, 0, FSrcImage.GetWidth, FSrcImage.GetHeight, UnitPixel, ImgAtt);
end;

end.
