unit uJxdOverlayMuxFilter;

interface

uses
  Windows, Classes, SysUtils, DirectShow9, BaseClass, Math,Graphics;//, dxGDIPlusClasses;

const
  VideoFilter_Name = 'Video Filter';
  VideoFilter_CLSID: TGUID = '{20590A48-B154-40C0-81E9-AD09F1E59400}';

type
  TOnDraw = procedure(Sender: TObject; AMemDC: HDC; const AWidth, AHeight: Integer) of object;
  
  TOverlayMuxFilter = class(TBCTransInPlaceFilter)
  private
    FBmp: array[0..10] of TBitmap;//TdxPNGImage;
  protected

  public
    MuxDraw: Boolean;
    constructor Create(out hr: HRESULT);
    destructor Destroy; override;

    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
    function Transform(Sample: IMediaSample): HRESULT; override;

    function Stop: HRESULT; override; stdcall;
    function StartStreaming: HRESULT; override;
//    function StartEnd
  end;

implementation

{ TVideoFilter }

function TOverlayMuxFilter.CheckInputType(mtIn: PAMMediaType): HRESULT;
const
  CSupportType: array[0..3] of TGUID = (
    '{32315659-0000-0010-8000-00AA00389B71}', //MEDIASUBTYPE_YV12 : TGUID =
    (D1:$E436EB7D;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70)), //MEDIASUBTYPE_RGB24
    (D1:$E436EB7E;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70)), //MEDIASUBTYPE_RGB32
    (D1:$773c9ac0;D2:$3274;D3:$11d0;D4:($b7,$24,$00,$aa,$00,$6c,$1a,$1 )));//MEDIASUBTYPE_ARGB32
var
  I: Integer;
begin
  Result := S_OK;
  if IsEqualGUID(mtIn^.majortype, MEDIATYPE_Video) then
    if IsEqualGUID(mtIn^.subtype, MEDIASUBTYPE_RGB24) then
      Result := S_OK;
  Exit;
    for I := Low(CSupportType) to High(CSupportType) do
      if IsEqualGUID(mtIn^.subtype, CSupportType[I]) then
      begin
        Result := S_OK;
        Exit;
      end;
end;

constructor TOverlayMuxFilter.Create(out hr: HRESULT);
var
  I: Integer;
begin
  inherited Create(ClassName, nil, VideoFilter_CLSID, hr);
  for I := 0 to 11 - 1 do
  begin
    FBmp[I] := TBitmap.Create;
    FBmp[I].LoadFromFile(Format('E:\CompanyWork\MusicT\KBox2.0\Components\Third Components\dsPack234\Demos\D6-D7\videocap\bitmap\%d.bmp', [I + 1]));
  end;
end;

destructor TOverlayMuxFilter.Destroy;
begin

  inherited;
end;

function TOverlayMuxFilter.StartStreaming: HRESULT;
begin
  inherited StartStreaming;
  Result := S_OK;
end;

function TOverlayMuxFilter.Stop: HRESULT; stdcall;
begin
  inherited Stop;
  Result := NOERROR;
end;

function TOverlayMuxFilter.Transform(Sample: IMediaSample): HRESULT;
var
  pData: PByte;
  cbData: Longint;
  hScrDC, hMemDC : HDC;
  hBmp : HBitmap;
  pvih: PVIDEOINFOHEADER;
  Start, Stop: REFERENCE_TIME;
  R: TRect;
  ms: IMediaSeeking;
  c: Int64;
begin
//  Result := S_OK;
//  Exit;
  if (Sample = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  Assert(IsEqualGUID(InputPin.AMMediaType.formattype, FORMAT_VideoInfo));
  pvih := InputPin.AMMediaType.pbFormat;

  pVih.bmiHeader.biSizeImage := min(pVih.bmiHeader.biSizeImage, cbData);

  if True then //Succeeded(GRaph.QueryInterface(IID_IMediaSeeking, ms)) then
  begin
//    ms.GetCurrentPosition(C);
//    C := C div 10000000;
//    ms := nil;
    Sample.GetPointer(pData);
    cbData := Sample.GetSize;
    hScrDC := CreateDC('DISPLAY', nil, nil, nil);
    hMemDC := CreateCompatibleDC(hScrDC);
    hBmp := CreateCompatibleBitmap(hScrDC, pvih.bmiHeader.biWidth, pvih.bmiHeader.biHeight);
    SelectObject(hMemDC, hBmp);
    SetDIBits(hMemDC, hBmp, 0, pvih.bmiHeader.biHeight, pData, PBitmapInfo(@(pVih^.bmiHeader))^, DIB_RGB_COLORS);
//    R := Rect(16, 16, FPNG[0].Width + 16, FPNG[0].Height + 16);
//    FPNG[C div 600].StretchDraw(hMemDC, R);
//    OffsetRect(R, FPNG[0].Width, 0);
//    FPNG[C div 60 mod 10].StretchDraw(hMemDC, R);
//    OffsetRect(R, FPNG[0].Width + FPNG[10].Width, 0);
//    FPNG[C mod 60 div 10].StretchDraw(hMemDC, R);
//    OffsetRect(R, FPNG[0].Width, 0);
//    FPNG[C mod 60 mod 10].StretchDraw(hMemDC, R);
//    R := Rect(16 + FPNG[0].Width * 2, 13, 16 + FPNG[0].Width * 2 + FPNG[10].Width, FPNG[10].Height + 13);
//    FPNG[10].StretchDraw(hMemDC, R);
    BitBlt( hMemDC, 0, 0, FBmp[C mod 10].Width, FBmp[C mod 10].Height, FBmp[C mod 10].Canvas.Handle,
            0, 0, SRCCOPY );

    GetDIBits(hMemDC, hBmp, 0, pvih.bmiHeader.biHeight, pData, PBitmapInfo(@(pVih^.bmiHeader))^, DIB_RGB_COLORS);
    DeleteObject(hBmp);
    DeleteDC(hScrDC);
    DeleteDC(hMemDC);
  end;
  Result := S_OK;
end;

end.
