{
单元名称： uJxdVideoFilter
作    者： 江晓德
QQ      ： 67068633
Email   :  jxd524@163.com
创建日期： 2010-06-24
功    能： 对视频流进行数据重叠，需要解释视频的格式，一般需要转成RGB，进行DC操作，之后再
           重新转化成指定视频格式，回写到内存流中

}

unit uJxdVideoFilter;

interface

{$define OverlayDebug}

uses
  Windows, Classes, SysUtils, DirectShow9, BaseClass, Math, Graphics, DSUtil, uJxdVideoFormatCoverter
  {$ifdef OverlayDebug}
  , uDebugInfo
  {$endIf};

type
  TOnRgb24FilterDraw = procedure(Sender: TObject; MemDC: HDC; const AWidth, AHeight: Integer) of object;
  TVideoOverlayFilter = class(TBCTransInPlaceFilter)
  public
    constructor Create(out hr: HRESULT);
    destructor  Destroy; override;

    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
    function Transform(Sample: IMediaSample): HRESULT; override;

    function GetPin(n: integer): TBCBasePin; override;
    function GetPinCount: integer; override;
  protected
    function DoMuxOverlaySample(Sample: IMediaSample): HRESULT; virtual;
  private
    FOutPin2: TBCTransInPlaceOutputPin;
    FOnDraw: TOnRgb24FilterDraw;
    procedure DeliverOutPin2(Sample: IMediaSample);
    function  Copy2(Source: IMediaSample): IMediaSample;
  public                                                    
    property OnDraw: TOnRgb24FilterDraw read FOnDraw write FOnDraw;  //由子类来调用，并由外部实现
  end;

  TRgb24OverlayFilter = class(TVideoOverlayFilter)
  public
    OwnerHandle: THandle;
    constructor Create(out hr: HRESULT);
    destructor  Destroy; override;
    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
  protected
    function DoMuxOverlaySample(Sample: IMediaSample): HRESULT; override;
  private
    FMemDC: HDC;
    FMemBmp: HBITMAP;
    procedure CheckMemGDI(const AWidth, AHeight: Integer);
  end;

  TYuY2OverlayFilter = class(TVideoOverlayFilter)
  public
    OwnerHandle: THandle;
    constructor Create(out hr: HRESULT);
    destructor  Destroy; override;
    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
  protected
    function DoMuxOverlaySample(Sample: IMediaSample): HRESULT; override;
  private
    FBmpDate: PByte;
    FBmpSize: Integer;
    FMemDC: HDC;
    FMemBmp: HBITMAP;
    FBmpInfoHeader: TBitmapInfoHeader;
    procedure CheckMemGDI(const AWidth, AHeight: Integer);
  end;

implementation

{ TOverlayFilter }
const
  CtOverlayName = 'Overlay Mux Filter';
  CtOverLayGUID: TGUID = '{4E18088C-40DC-4B14-865C-30B582FECF34}';

procedure Debug(const AInfo: string);
begin
  {$ifdef OverlayDebug}
  _Log( AInfo, 'OverlayMux_debug.txt' );
  {$endIf}
  OutputDebugString( PChar(AInfo) );
end;

function TVideoOverlayFilter.CheckInputType(mtIn: PAMMediaType): HRESULT;
begin
  Result := S_OK;
end;

function TVideoOverlayFilter.Copy2(Source: IMediaSample): IMediaSample;
var
  Start, Stop: TReferenceTime;
  Time: boolean;
  pStartTime, pEndTime: PReferenceTime;
  TimeStart, TimeEnd: Int64;
  Flags: DWORD;
  Sample2: IMediaSample2;
  props: PAMSample2Properties;
  MediaType: PAMMediaType;
  DataLength: LongInt;
  SourceBuffer, DestBuffer: PByte;
  SourceSize, DestSize: LongInt;
  hr: hresult;
begin
  Time := (Source.GetTime(Start, Stop) = S_OK);
  // this may block for an indeterminate amount of time
  if Time then
  begin
    pStartTime := @Start;
    pEndTime   := @Stop;
  end
  else
  begin
    pStartTime := nil;
    pEndTime   := nil;
  end;
  if FSampleSkipped then Flags := AM_GBF_PREVFRAMESKIPPED else Flags := 0;
  hr := FOutPin2.PeekAllocator.GetBuffer(result, pStartTime, pEndTime, Flags);

  if FAILED(hr) then exit;

  ASSERT(result <> nil);
  if(SUCCEEDED(result.QueryInterface(IID_IMediaSample2, Sample2))) then
    begin
      props :=  FInput.SampleProps;
      hr := Sample2.SetProperties(SizeOf(TAMSample2Properties) - (4*2), props^);
      Sample2 := nil;
      if FAILED(hr) then
      begin
        result := nil;
        exit;
      end;
    end
  else
    begin
      if Time then result.SetTime(@Start, @Stop);
      if (Source.IsSyncPoint = S_OK) then result.SetSyncPoint(True);
      if ((Source.IsDiscontinuity = S_OK) or FSampleSkipped) then result.SetDiscontinuity(True);
      if (Source.IsPreroll = S_OK) then result.SetPreroll(True);
      // Copy the media type
      if (Source.GetMediaType(MediaType) = S_OK) then
        begin
          result.SetMediaType(MediaType^);
          DeleteMediaType(MediaType);
        end;

    end;

  FSampleSkipped := FALSE;

  // Copy the sample media times
  if (Source.GetMediaTime(TimeStart, TimeEnd) = NOERROR) then
    result.SetMediaTime(@TimeStart,@TimeEnd);

  // Copy the actual data length and the actual data.
  DataLength := Source.GetActualDataLength;

  result.SetActualDataLength(DataLength);

  // Copy the sample data
  SourceSize := Source.GetSize;
  DestSize   := result.GetSize;

  // milenko start get rid of compiler warnings
  if (DestSize < SourceSize) then
  begin
  end;
  // milenko end

  ASSERT(DestSize >= SourceSize, format('DestSize (%d) < SourceSize (%d)',[DestSize, SourceSize]));
  ASSERT(DestSize >= DataLength);

  Source.GetPointer(SourceBuffer);
  result.GetPointer(DestBuffer);
  ASSERT((DestSize = 0) or (SourceBuffer <> nil) and (DestBuffer <> nil));
  CopyMemory(DestBuffer, SourceBuffer, DataLength);
end;

constructor TVideoOverlayFilter.Create(out hr: HRESULT);
begin
  inherited Create( ClassName, nil, CtOverLayGUID, hr, True );
  hr := S_OK;
end;

procedure TVideoOverlayFilter.DeliverOutPin2(Sample: IMediaSample);
var
  Sample2: IMediaSample;
begin
  if FOutPin2.IsConnected then
  begin
    Sample2 := Copy2( Sample );
    FOutPin2.Deliver( Sample2 );
    Sample2 := nil;
  end;
end;

destructor TVideoOverlayFilter.Destroy;
begin

  inherited;
end;

function TVideoOverlayFilter.DoMuxOverlaySample(Sample: IMediaSample): HRESULT;
begin
  Result := S_OK;
end;

function TVideoOverlayFilter.GetPin(n: integer): TBCBasePin;
var
  hr: HRESULT;
begin
  if n in [0, 1] then
    Result := inherited GetPin( n )
  else if n = 2 then
  begin
    if FOutPin2 = nil then
    begin
      FOutPin2 := TBCTransInPlaceOutputPin.Create('TransInPlace output pin', self,  hr, 'Output'); // Pin name
      ASSERT(SUCCEEDED(hr));
      if(FOutPin2 = nil) then
      begin
        FInput.Free;
        FInput := nil;
      end;
    end;
    Result := FOutPin2;
  end
  else
    Result := nil;
end;

function TVideoOverlayFilter.GetPinCount: integer;
begin
  Result := 3;
end;

function TVideoOverlayFilter.Transform(Sample: IMediaSample): HRESULT;
begin
  DoMuxOverlaySample( Sample );
  DeliverOutPin2( Sample );
  Result := S_OK;
end;

{ TYuY2OverlayFilter }

function TYuY2OverlayFilter.CheckInputType(mtIn: PAMMediaType): HRESULT;
begin
  Result := VFW_E_TYPE_NOT_ACCEPTED;
  if IsEqualGUID(mtIn^.majortype, MEDIATYPE_Video) and IsEqualGUID(mtIn^.subtype, MEDIASUBTYPE_YUY2) then
    Result := S_OK;
end;

procedure TYuY2OverlayFilter.CheckMemGDI(const AWidth, AHeight: Integer);
var
  dc: HDC;
begin
  if FMemDC = 0 then
  begin
    if OwnerHandle = 0 then
      raise Exception.Create( 'must set TYuY2OverlayFilter OwnerHandle frits!' );
    dc := GetDC( OwnerHandle );
    try
      FMemDC := CreateCompatibleDC( dc );
      FMemBmp := CreateCompatibleBitmap( dc, AWidth, AHeight );
      SelectObject( FMemDC, FMemBmp );

      with FBmpInfoHeader do
      begin
        biSize := SizeOf(FBmpInfoHeader);
        biWidth := AWidth;
        biHeight := AHeight;
        biPlanes := 1;
        biBitCount := 24;
        biCompression := 0;
        biSizeImage := biWidth * biHeight * 3;
        biXPelsPerMeter := 0;
        biYPelsPerMeter := 0;
        biClrUsed := 0;
        biClrImportant := 0;
      end;
    finally
      DeleteDC( dc );
    end;
  end;
end;

constructor TYuY2OverlayFilter.Create(out hr: HRESULT);
begin
  inherited Create( hr );
  FMemDC := 0;
  FMemBmp := 0;
  OwnerHandle := 0;
end;

destructor TYuY2OverlayFilter.Destroy;
begin
  if FBmpDate <> nil then
    FreeMem( FBmpDate, FBmpSize );
  FBmpDate := nil;
  inherited;
end;

function TYuY2OverlayFilter.DoMuxOverlaySample(Sample: IMediaSample): HRESULT;
var
  pYuY2Data: PByte;
  pVih: PVideoInfoHeader;
begin
  //YuY2格式视频数据处理加工
  Result := S_OK;
  if (Sample = nil) or (not Assigned(OnDraw)) then Exit;

  if not IsEqualGUID(InputPin.AMMediaType^.subtype, MEDIASUBTYPE_YUY2) then
  begin
    Debug( 'error SubType at TYuY2OverlayFilter: ' + GUIDToString(InputPin.AMMediaType^.subtype) );
    Exit;
  end;
  pVih := PVideoInfoHeader( InputPin.AMMediaType.pbFormat );
  if FBmpDate = nil then
  begin
    FBmpSize := pVih.bmiHeader.biWidth * pVih.bmiHeader.biHeight * 3;
    GetMem( FBmpDate, FBmpSize );
  end;

  CheckMemGDI( pVih.bmiHeader.biWidth, pVih.bmiHeader.biHeight );
    
  Sample.GetPointer( pYuY2Data );
  //转化成RGB24位进行处理
  Yuy2ToRgb24( pYuY2Data, FBmpDate, pVih.bmiHeader.biWidth, pVih.bmiHeader.biHeight );

  try
    if 0 = SetDIBits( FMemDC, FMemBmp, 0, pvih.bmiHeader.biHeight, FBmpDate, PBitmapInfo(@FBmpInfoHeader)^, DIB_RGB_COLORS) then
    begin
      Debug( Format('SetDIBits error: MemDC: %d, MemBmp: %d, BmpHeight: %d, BmpWidth: %d',
                     [FMemDC, FMemBmp, pVih.bmiHeader.biWidth, pVih.bmiHeader.biHeight]) );
      Exit;
    end;

    OnDraw( Self, FMemDC, pVih.bmiHeader.biWidth, pVih.bmiHeader.biHeight );

    if 0 = GetDIBits( FMemDC, FMemBmp, 0, pvih.bmiHeader.biHeight, FBmpDate, PBitmapInfo(@FBmpInfoHeader)^, DIB_RGB_COLORS) then
      Debug( 'GetDIBits error' );
  except
    Debug( 'GetDIBits error' );
  end;
  //将RGB24位位图数据写回
  Rgb24ToYuy2( pYuY2Data, FBmpDate, pVih.bmiHeader.biWidth, pVih.bmiHeader.biHeight );
end;

{ TRgb24OverlayFilter }

function TRgb24OverlayFilter.CheckInputType(mtIn: PAMMediaType): HRESULT;
begin
  Result := VFW_E_TYPE_NOT_ACCEPTED;
  if IsEqualGUID(mtIn^.majortype, MEDIATYPE_Video) and IsEqualGUID(mtIn^.subtype, MEDIASUBTYPE_RGB24) then
    Result := S_OK;
end;

procedure TRgb24OverlayFilter.CheckMemGDI(const AWidth, AHeight: Integer);
var
  dc: HDC;
begin
  if FMemDC = 0 then
  begin
    if OwnerHandle = 0 then
      raise Exception.Create( 'must set TRgb24OverlayFilter OwnerHandle frits!' );
    dc := GetDC( OwnerHandle );
    try
      FMemDC := CreateCompatibleDC( dc );
      FMemBmp := CreateCompatibleBitmap( dc, AWidth, AHeight );
      SelectObject( FMemDC, FMemBmp );
    finally
      DeleteDC( dc );
    end;
  end;
end;

constructor TRgb24OverlayFilter.Create(out hr: HRESULT);
begin
  inherited Create( hr );
  FMemDC := 0;
  FMemBmp := 0;
  OwnerHandle := 0;
end;

destructor TRgb24OverlayFilter.Destroy;
begin
  if FMemDC <> 0 then
    DeleteDC( FMemDC );
  if FMemBmp <> 0 then
    DeleteObject( FMemBmp );
  inherited;
end;

function TRgb24OverlayFilter.DoMuxOverlaySample(Sample: IMediaSample): HRESULT;
var
  pRgb24Data: PByte;
  pVih: PVideoInfoHeader;
begin
  //RGB24格式视频数据处理加工
  Result := S_OK;
  if (Sample = nil) or (not Assigned(OnDraw)) then Exit;

  if not IsEqualGUID(InputPin.AMMediaType^.subtype, MEDIASUBTYPE_RGB24) then
  begin
    Debug( 'error SubType at TRgb24OverlayFilter: ' + GUIDToString(InputPin.AMMediaType^.subtype) );
    Exit;
  end;
  //Check for Create Mem GDI
  pVih := PVideoInfoHeader( InputPin.AMMediaType.pbFormat );
  CheckMemGDI( pVih.bmiHeader.biWidth, pVih.bmiHeader.biHeight );
  try
    Sample.GetPointer( pRgb24Data );
    if 0 = SetDIBits( FMemDC, FMemBmp, 0, pvih.bmiHeader.biHeight, pRgb24Data, PBitmapInfo(@(pVih^.bmiHeader))^, DIB_RGB_COLORS) then
    begin
      Debug( Format('SetDIBits error: MemDC: %d, MemBmp: %d, BmpHeight: %d, BmpWidth: %d',
                     [FMemDC, FMemBmp, pVih.bmiHeader.biWidth, pVih.bmiHeader.biHeight]) );
      Exit;
    end;

    OnDraw( Self, FMemDC, pVih.bmiHeader.biWidth, pVih.bmiHeader.biHeight );

    if 0 = GetDIBits( FMemDC, FMemBmp, 0, pvih.bmiHeader.biHeight, pRgb24Data, PBitmapInfo(@(pVih^.bmiHeader))^, DIB_RGB_COLORS) then
      Debug( 'GetDIBits error' );
  except
  end;
end;

end.
