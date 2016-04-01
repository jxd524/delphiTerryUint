unit uJxdOverlayMuxFilter;

interface

uses
  Windows, Classes, SysUtils, DirectShow9, BaseClass, Math, DSPack, ExtCtrls, Graphics;

type
  TOnDraw = procedure(Sender: TObject; AMemDC: HDC; const AWidth, AHeight: Integer) of object;

  TOverlayMuxFilter = class(TBCTransInPlaceFilter)
  public
    OwnerHandle: HWND;
    constructor Create(out hr: HRESULT);
    destructor Destroy; override;

    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
    function Transform(Sample: IMediaSample): HRESULT; override;
  protected
    procedure DoDrawFilter(AMemDC: HDC; const AWidth, AHeight: Integer); virtual;
  private
    FSrcBmp, FTempBmp: TBitmap;

    FOnDraw: TOnDraw;
    FMuxDraw: Boolean;
    FFilterGraph: TFilterGraph;
    procedure SetFilterGraph(const AFilterGraph: TFilterGraph);
 public
    property MuxDraw: Boolean read FMuxDraw write FMuxDraw;
    property OnDraw: TOnDraw read FOnDraw write FOnDraw;
    property FilterGraph: TFilterGraph read FFilterGraph write SetFilterGraph;
  end;

implementation

uses
  uJxdREC, DSUtil, uDShowSub;

const
  CtOverlayMuxFilterName = 'Overlay Mux Filter';
  CtOverlayMuxFilterCLSID: TGUID = '{3BC16F7C-D499-4878-BCA4-39E2C0821F82}';
  CtMILLISECONDS = 1000;            // 10 ^ 3
  CtNANOSECONDS = 1000000000;       // 10 ^ 9
  CtUNITS = CtNANOSECONDS / 100;      // 10 ^ 7

{ TVideoFilter }

function TOverlayMuxFilter.CheckInputType(mtIn: PAMMediaType): HRESULT;
begin
  Result := E_INVALIDARG;
  if not IsEqualGUID(FORMAT_VideoInfo, mtIn^.formattype) then Exit;
  if IsEqualGUID( MEDIASUBTYPE_YUY2, mtIn^.subtype ) or
     IsEqualGUID( MEDIASUBTYPE_RGB24, mtIn^.subtype ) or
     IsEqualGUID( MEDIASUBTYPE_RGB32, mtIn^.subtype ) or
     IsEqualGUID( MEDIASUBTYPE_ARGB32, mtIn^.subtype ) then  Result := S_OK;

  if Result <> S_OK then
  begin
    DebugOut( 'TOverlayMuxFilter.CheckInputType can not find subtype: ' + GUIDToString(mtIn^.subtype) );
//    MuxDraw := False;
    Result := S_OK;
  end;
end;

constructor TOverlayMuxFilter.Create(out hr: HRESULT);
begin
  inherited Create( CtOverlayMuxFilterName + ClassName, nil, CtOverlayMuxFilterCLSID, hr, True );
  FMuxDraw := True;
  FSrcBmp := TBitmap.Create;
  FTempBmp := TBitmap.Create;
  FSrcBmp.LoadFromFile( '11.bmp' );
end;

destructor TOverlayMuxFilter.Destroy;
begin

  inherited;
end;

procedure TOverlayMuxFilter.DoDrawFilter(AMemDC: HDC; const AWidth, AHeight: Integer);
begin
  DebugOut( 'TOverlayMuxFilter.DoDrawFilter' );
  if Assigned(OnDraw) then
    OnDraw( Self, AMemDC, AWidth, AHeight );
end;

procedure TOverlayMuxFilter.SetFilterGraph(const AFilterGraph: TFilterGraph);
begin
  if AFilterGraph = FFilterGraph then exit;
  FFilterGraph := AFilterGraph;
  if Failed((FFilterGraph as IGraphBuilder).AddFilter( Self as IBaseFilter, CtOverlayMuxFilterName)) then
    DebugOut( 'AddFilter Failed' );
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
//    DoDrawFilter( hMemDC, pvih.bmiHeader.biWidth, pvih.bmiHeader.biHeight );
    FTempBmp.Assign( FSrcBmp );
    if not BitBlt( hMemDC, 0, 0, FTempBmp.Width, FTempBmp.Height, FTempBmp.Canvas.Handle, 0, 0, SRCCOPY ) then
      DebugOut( 'overlay mux fail' );

    GetDIBits(hMemDC, hBmp, 0, pvih.bmiHeader.biHeight, pData, PBitmapInfo(@(pVih^.bmiHeader))^, DIB_RGB_COLORS);
    DeleteObject(hBmp);
    DeleteDC(hScrDC);
    DeleteDC(hMemDC);
  end;
  Result := S_OK;
end;


//var
//  hScrDC, hMemDC : HDC;
//  hBmp : HBitmap;
//  pVih: PVIDEOINFOHEADER;
//  pData: PByte;
//  pMT: PAMMediaType;
//  hRes: HRESULT;
//begin
//  if not MuxDraw then
//  begin
//    Result := S_OK;
//    Exit;
//  end;
//  if (Sample = nil) then
//  begin
//    Result := E_POINTER;
//    Exit;
//  end;
//  Assert( IsEqualGUID(InputPin.AMMediaType.formattype, FORMAT_VideoInfo) );
//
//
//  Result := S_OK;
//  pvih := InputPin.AMMediaType.pbFormat;
//  Sample.GetPointer( pData );
//
////  pvih^.rcSource
//
//  if FDC = 0 then
//  begin
//    hScrDC := GetDC( OwnerHandle );
//    FDC := CreateCompatibleDC( hScrDC );
//    FDibSection := CreateCompatibleBitmap( hScrDC, FWidth, FHeight );
//    ReleaseDC( OwnerHandle, hScrDC );
//    SelectObject( FDC, FDibSection );
//  end;
//
//
////  hScrDC := GetDC( OwnerHandle );
////  hMemDC := CreateCompatibleDC( hScrDC );
////  hBmp := CreateCompatibleBitmap( hScrDC, FWidth, FHeight );
//  try
////    SelectObject( hMemDC, hBmp );
//    SetDIBits( FDC, FDibSection, 0, FHeight, pData, PBitmapInfo(@(pVih^.bmiHeader))^, DIB_RGB_COLORS );
//
//    DoDrawFilter( FDC, FWidth, FHeight );
//
//    GetDIBits( FDC, FDibSection, 0, FHeight, pData, PBitmapInfo(@(pVih^.bmiHeader))^, DIB_RGB_COLORS );
//  finally
////    DeleteObject( hBmp );
////    DeleteDC( hMemDC );
////    ReleaseDC( OwnerHandle, hScrDC );
//  end;
//  Sample.SetSyncPoint( True );
//end;

end.
