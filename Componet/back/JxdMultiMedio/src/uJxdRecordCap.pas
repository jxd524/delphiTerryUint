{
单元名称： uJxdREC
作    者： 江晓德
QQ      ： 67068633
Email   :  jxd524@163.com
创建日期： 2010-06-23
功    能： 对视频头与声卡立体声混音两者进行录制，并保存为ASF文件格式（WMV）。支持预览
说    明：
}

unit uJxdRecordCap;

interface

//{$Define xdDebug}

uses
  Windows, Classes, ExtCtrls, Controls, Messages, DirectShow9, SysUtils, ActiveX, DSPack,
  uDShowSub, Graphics, WMF9, uJxdAsfXml, Forms, uJxdVideoFilter, uJxdOverlayEffect
  {$IfDef xdDebug}
  ,uDebugInfo
  {$EndIf}
  ;

type
  TRecordStyle = (rsNULL, rsRecord, rsPreview);
  //录制源设置
  TSourceFilterSetting = class(TPersistent)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  private
    FBitCount: Word;
    FPixel: Byte;
    FVideoWidth: Integer;
    FVideoHeight: Integer;
    FVideoFrame: Integer;
    FVideoCapIndex: Integer;
    FAudioCapIndex: Integer;
    FAudioCapInPinIndex: Integer;
    procedure SetVideoWidth(const Value: Integer);
    procedure SetVideoHeight(const Value: Integer);
    procedure SetVideoFrame(const Value: Integer);
    procedure SetVideoCapIndex(const Value: Integer);
    procedure SetAudioCapIndex(const Value: Integer);
    procedure SetAudioCapInPinIndex(const Value: Integer);
  published
    //视频源设置
    property VideoCapIndex: Integer read FVideoCapIndex write SetVideoCapIndex;
    property VideoWidth: Integer read FVideoWidth write SetVideoWidth;
    property VideoHeight: Integer read FVideoHeight write SetVideoHeight;
    property VideoFrame: Integer read FVideoFrame write SetVideoFrame;
    //音频设置
    property AudioCapIndex: Integer read FAudioCapIndex write SetAudioCapIndex;
    property AudioCapInPinIndex: Integer read FAudioCapInPinIndex write SetAudioCapInPinIndex;
  end;

  //录制输出设置
  TOutPutFilterSetting = class(TPersistent)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  private
    FAudioBitRate: Integer;
    FAudioChannels: Integer;
    FVideoWidth: Integer;
    FVideoBitRate: Integer;
    FVideoMaxKeyFrameSpacing: Integer;
    FVideoQuality: Integer;
    FVideoHeight: Integer;
    FOutPutFileName: WideString;
    procedure SetAudioBitRate(const Value: Integer);
    procedure SetAudioChannels(const Value: Integer);
    procedure SetVideoBitRate(const Value: Integer);
    procedure SetVideoHeight(const Value: Integer);
    procedure SetVideoMaxKeyFrameSpacing(const Value: Integer);
    procedure SetVideoQuality(const Value: Integer);
    procedure SetVideoWidth(const Value: Integer);
    procedure SetOutPutFileName(const Value: WideString);
  published
    property AudioBitRate: Integer read FAudioBitRate write SetAudioBitRate;
    property AudioChannels: Integer read FAudioChannels write SetAudioChannels;
    property VideoBitRate: Integer read FVideoBitRate write SetVideoBitRate;
    property VideoWidth: Integer read FVideoWidth write SetVideoWidth;
    property VideoHeight: Integer read FVideoHeight write SetVideoHeight;
    property VideoQuality: Integer read FVideoQuality write SetVideoQuality;
    property VideoMaxKeyFrameSpacing: Integer read FVideoMaxKeyFrameSpacing write SetVideoMaxKeyFrameSpacing;
    property OutPutFileName: WideString read FOutPutFileName write SetOutPutFileName;
  end;

  TxdRecordCap = class(TCustomPanel)
  public
    procedure Stop;
    function  StartRecord: Boolean;
    function  StartPreview: Boolean;
    procedure SetOverlayEffer(const AEfferClass: TOverlayEffect);
    function  GetRecordTime(var Hour, Min, Sec, MSec: Word): Boolean;

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    procedure Loaded; override;
    procedure CheckHResult(ARes: HRESULT);
    //对象创建
    function  CreateBaseFilter: Boolean;
    function  CheckVideoCapFilter: Boolean;
    function  CheckAudioCapFilter: Boolean;
    procedure CheckOverlayFilter;
    procedure AddAudioCap;
    //对象配置
    procedure DoConfigAudioSourceFilter; virtual;//config audio filter
    procedure DoConfigVideoSourceFilter; virtual;//config video filter
    procedure DoConfigOutputFile; virtual; //config out file video
    //连接回路
    function RecordingByOverlay: Boolean;
    function PreviewByOverlay: Boolean;

    procedure NotifyEvent(AEvent: TNotifyEvent);

    procedure Paint; override;
  private
    FCaptureFilterGraph: TFilterGraph;  //管理器
    FVideoWindow: TVideoWindow;         //视频窗口
    FVideoCapFilter: TFilter;           //视频源
    FAudioCapFilter: TFilter;           //音频源
    FmuxFilter: IBaseFilter;            //输出文件
    FOverlayFilter: TVideoOverlayFilter;
    FOverlayEffect: TOverlayEffect;

    FSourceFilterSetting: TSourceFilterSetting;
    FRecordStyle: TRecordStyle;
    FOutPutFilterSetting: TOutPutFilterSetting;
    FBackBitmap: TBitmap;
    FOnStop: TNotifyEvent;
    FOnBeginRecord: TNotifyEvent;
    FOnBeginPriview: TNotifyEvent;
    procedure SetSourceFilterSetting(const Value: TSourceFilterSetting);
    procedure SetOutPutFilterSetting(const Value: TOutPutFilterSetting);
    procedure SetBackBitmap(const Value: TBitmap);
  published
    property Align;
    property Alignment;
    property Anchors;
    property Color;
    property TabStop;
    property OnClick;
    property OnDblClick;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnCanResize;
    
    property BackBitmap: TBitmap read FBackBitmap write SetBackBitmap;
    property RecordStyle: TRecordStyle read FRecordStyle;
    property SourceFilterSetting: TSourceFilterSetting read FSourceFilterSetting write SetSourceFilterSetting;
    property OutPutFilterSetting: TOutPutFilterSetting read FOutPutFilterSetting write SetOutPutFilterSetting;

    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnBeginRecord: TNotifyEvent read FOnBeginRecord write FOnBeginRecord;
    property OnBeginPriview: TNotifyEvent read FOnBeginPriview write FOnBeginPriview; 
  end;

function GetVideoCapFilters: string;
function GetAudioCapFilters: string;
function GetAudioCapPins(const AAudioCapIndex: Integer; const APinDirection: TPinDirection): string;

procedure DebugOut(const AInfo: string);

implementation

uses
  DSUtil;
{ TKBoxREC }



procedure DebugOut(const AInfo: string);
begin
  OutputDebugString( PChar(AInfo) );
  {$IfDef xdDebug}
  _Log( AInfo, 'DebugLog.txt' );
{$EndIf}
end;

var
  _CapEnum: TSysDevEnum = nil;

procedure InitCapRecordInfo;
begin
  if not Assigned(_CapEnum) then
  begin
    CoInitialize( nil );
    _CapEnum := TSysDevEnum.Create( CLSID_AudioInputDeviceCategory );
  end;
end;

procedure FreeCapRecordInfo;
begin
  if Assigned(_CapEnum) then
  begin
    _CapEnum.Free;
    _CapEnum := nil;
    CoUninitialize;
  end;
end;

function GetFiltersFriendlyName(const AGUID: TGUID): string;
var
  i: integer;
begin
  if not Assigned(_CapEnum) then InitCapRecordInfo;
  _CapEnum.SelectGUIDCategory( AGUID );
  for i := 0 to _CapEnum.CountFilters - 1 do
    Result := Result + _CapEnum.Filters[i].FriendlyName + #13#10;
end;


function GetVideoCapFilters: string;
begin
  Result := GetFiltersFriendlyName( CLSID_VideoInputDeviceCategory );
end;

function GetAudioCapFilters: string;
begin
  Result := GetFiltersFriendlyName( CLSID_AudioInputDeviceCategory );
end;

function GetAudioCapPins(const AAudioCapIndex: Integer; const APinDirection: TPinDirection): string;
var
  FilterGraph: TFilterGraph;
  AudioFilter: TFilter;
  EnumPins: IEnumPins;
  pin: IPin;
  PinInfo: TPinInfo;
begin
  Result := '';
  if not Assigned(_CapEnum) then InitCapRecordInfo;
  _CapEnum.SelectGUIDCategory( CLSID_AudioInputDeviceCategory );
  if (AAudioCapIndex = -1) or (AAudioCapIndex >= _CapEnum.CountFilters) then
  begin
    DebugOut( 'please set Audio capture frist' );
    Exit;
  end;

  FilterGraph := TFilterGraph.Create( nil );
  AudioFilter := TFilter.Create( nil );
  try
    AudioFilter.BaseFilter.Moniker := _CapEnum.GetMoniker( AAudioCapIndex );
    FilterGraph.Active := True;
    AudioFilter.FilterGraph := FilterGraph;
    (AudioFilter as IBaseFilter).EnumPins( EnumPins );    
    while EnumPins.Next(1, pin, nil) = S_OK do
    begin
      if S_OK = Pin.QueryPinInfo(PinInfo) then
      begin
        if PinInfo.dir = APinDirection then
          Result := Result + PinInfo.achName + #13#10;
      end;
      Pin := nil;
    end;
  finally
    FilterGraph.Active := False;
    EnumPins := nil;
    FilterGraph.Free;
    AudioFilter.Free;
  end;
end;


{ TxdRecordCap }

procedure TxdRecordCap.AddAudioCap;
begin
  if FAudioCapFilter <> nil then
  begin
    if FAudioCapFilter.FilterGraph = nil then
      FAudioCapFilter.FilterGraph := FCaptureFilterGraph;
    with FCaptureFilterGraph as IcaptureGraphBuilder2 do
      CheckHResult( RenderStream(nil, nil, FAudioCapFilter as IBaseFilter, nil, FmuxFilter) );
  end;
end;

function TxdRecordCap.CheckAudioCapFilter: Boolean;
begin
  Result := False;
  if FCaptureFilterGraph = nil then
  begin
    DebugOut( 'please create base filter first' );
    Exit;
  end;
  _CapEnum.SelectGUIDCategory( CLSID_AudioInputDeviceCategory );
  if (FSourceFilterSetting.AudioCapIndex = -1) or (FSourceFilterSetting.AudioCapIndex >= _CapEnum.CountFilters) then
  begin
    DebugOut( 'please set Audio capture frist' );
    Exit;
  end;
  if FAudioCapFilter = nil then
    FAudioCapFilter := TFilter.Create( Self );
  FAudioCapFilter.BaseFilter.Moniker := _CapEnum.GetMoniker( FSourceFilterSetting.AudioCapIndex );
  FCaptureFilterGraph.Active := True;
  FAudioCapFilter.FilterGraph := FCaptureFilterGraph;
  FCaptureFilterGraph.Active := False;
  Result := True;
end;

procedure TxdRecordCap.CheckHResult(ARes: HRESULT);
begin
  if Failed(ARes) then
  begin
    Stop;
    DebugOut( 'error: ' + GetAMErrorText(ARes) );
    raise Exception.Create( 'error' );
  end;
end;

procedure TxdRecordCap.CheckOverlayFilter;
var
  pVidoeConfig: IAMStreamConfig;
  hRes: HRESULT;
  pMT: PAMMediaType;
begin
  hRes := (FCaptureFilterGraph as ICaptureGraphBuilder2).FindInterface( @PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video,
          FVideoCapFilter as IBAseFilter, IID_IAMStreamConfig, pVidoeConfig );
  CheckHResult( hRes );
  CheckHResult( pVidoeConfig.GetFormat(pMT) );
  try
    hRes := E_NOTIMPL;
    if IsEqualGUID(MEDIATYPE_Video, pMT^.majortype) then
    begin
      if IsEqualGUID(MEDIASUBTYPE_RGB24, pMT^.subtype) then
      begin
        FOverlayFilter := TRgb24OverlayFilter.Create( hRes );
        if Assigned(FOverlayEffect) then
        begin
          (FOverlayFilter as TRgb24OverlayFilter).OnDraw := FOverlayEffect.DoOverlayEffer;
          (FOverlayFilter as TRgb24OverlayFilter).OwnerHandle := Handle;
        end;
        DebugOut( 'use overlay filter: Rgb24 format subtype' );
      end
      else if IsEqualGUID(MEDIASUBTYPE_YUY2, pMT^.subtype) then
      begin
        FOverlayFilter := TYuY2OverlayFilter.Create( hRes );
        if Assigned(FOverlayEffect) then
        begin
          (FOverlayFilter as TYuY2OverlayFilter).OnDraw := FOverlayEffect.DoOverlayEffer;
          (FOverlayFilter as TYuY2OverlayFilter).OwnerHandle := Handle;
        end;
        DebugOut( 'use overlay filter : Yuy2 format subtype' );
      end;
    end;
    CheckHResult( hRes );
  finally
    DeleteMediaType( pMT );
  end;
end;

function TxdRecordCap.CheckVideoCapFilter: Boolean;
begin
  Result := False;
  if FCaptureFilterGraph = nil then
  begin
    DebugOut( 'must create Capture filter graph first' );
    Exit;
  end;
  _CapEnum.SelectGUIDCategory( CLSID_VideoInputDeviceCategory );
  if (FSourceFilterSetting.VideoCapIndex = -1) or (FSourceFilterSetting.VideoCapIndex >= _CapEnum.CountFilters) then
  begin
    DebugOut( 'please set video capture frist' );
    Exit;
  end;
  if FVideoCapFilter = nil then
    FVideoCapFilter := TFilter.Create( Self );
  FVideoCapFilter.BaseFilter.Moniker := _CapEnum.GetMoniker( FSourceFilterSetting.VideoCapIndex );
  FCaptureFilterGraph.Active := True;
  FVideoCapFilter.FilterGraph := FCaptureFilterGraph;
  FCaptureFilterGraph.Active := False;
  Result := True;
end;

constructor TxdRecordCap.Create(AOwner: TComponent);
begin
  inherited;
  FSourceFilterSetting := TSourceFilterSetting.Create;
  FOutPutFilterSetting := TOutPutFilterSetting.Create;
  FRecordStyle := rsNULL;
  FOverlayEffect := nil;
  FBackBitmap := TBitmap.Create;
end;

function TxdRecordCap.CreateBaseFilter: Boolean;
begin
  Result := True;
  try
    FCaptureFilterGraph := TFilterGraph.Create( Self );
    FCaptureFilterGraph.Mode := gmCapture;
    FVideoWindow := TVideoWindow.Create( Self );
    FVideoWindow.Parent := Self;
    FVideoWindow.Align := alClient;
    FVideoWindow.FilterGraph := FCaptureFilterGraph;
    FVideoWindow.Visible := True;
  except
    Result := False;
  end;
end;

destructor TxdRecordCap.Destroy;
begin
  Stop;
  FSourceFilterSetting.Free;
  FOutPutFilterSetting.Free;
  FBackBitmap.Free;
  inherited;
end;

procedure TxdRecordCap.DoConfigAudioSourceFilter;
var
  EnumPins: IEnumPins;
  pin: IPin;
  PinInfo: TPinInfo;
  Mixer: IAMAudioInputMixer;
  b: BOOL;
  nIndex: Integer;
  hr: HRESULT;
begin
  if -1 = FSourceFilterSetting.AudioCapInPinIndex then Exit;
   
  hr := (FAudioCapFilter as IBAseFilter).EnumPins(EnumPins);
  if Failed(hr) then
  begin
    Stop;
    raise Exception.Create( 'Can not EnumPins' );
  end;
  try
    nIndex := 0;
    while EnumPins.Next(1, pin, nil) = S_OK do
    begin
      if S_OK = Pin.QueryPinInfo(PinInfo) then
      begin
        if PinInfo.dir = PINDIR_INPUT then
        begin
          if nIndex = FSourceFilterSetting.AudioCapInPinIndex then
          begin
            if Succeeded(pin.QueryInterface(IID_IAMAudioInputMixer, Mixer)) then;
            begin
              Mixer.get_Enable( b );
              if not b then
                Mixer.put_Enable( not b );
              Mixer := nil;
            end;
            Break;
          end;
          Inc( nIndex );
        end;
      end;
    Pin := nil;
    end;
  finally
    EnumPins := nil;
  end;
end;

procedure TxdRecordCap.DoConfigOutputFile;
var
  WMProfile: IWMProfile;
  WMProfileManager: IWMProfileManager;
  WMProfileManager2: IWMProfileManager2;
  ConfigAsfWriter: IConfigAsfWriter;

  ProfileBuffer: pWideChar;
  ProfileSize: LongWord;
  Profile: String;

  XML: TASFXML;
  pcXML: pOleStr;
begin
  if Failed(FmuxFilter.QueryInterface(IID_IConfigAsfWriter, ConfigAsfWriter)) then
  begin
    DebugOut( 'Can not queryInterface: IID_IConfigAsfWrite' );
    Exit;
  end;
  if Failed (WMCreateProfileManager(WMProfileManager)) then
  begin
    DebugOut( 'Can not Create ProfileManage' );
    ConfigAsfWriter := nil;
    Exit;
  end;
  if Succeeded (WMProfileManager.QueryInterface (IWMProfileManager2, WMProfileManager2)) then
  begin
    WMProfileManager2.SetSystemProfileVersion (WMT_VER_8_0);
    WMProfileManager2 := nil;
  end;

  if Failed(ConfigASFWriter.GetCurrentProfile(WMProfile)) then
  begin
    DebugOut( 'Can not GetCurrentProfile' );
    ConfigAsfWriter := nil;
    WMProfileManager := nil;
    Exit;
  end;
  ProfileSize := 0;
  if Succeeded (WMProfileManager.SaveProfile (WMProfile, nil, ProfileSize)) then
  begin
    GetMem (ProfileBuffer, ProfileSize shl 1);
    try
    if Succeeded (WMProfileManager.SaveProfile (WMProfile, ProfileBuffer, ProfileSize)) then
      Profile := ProfileBuffer;
    finally
      FreeMem (ProfileBuffer);
    end;
  end;
  if assigned (WMProfile) then WMProfile := nil;

  XML := TASFXML.Create;
  try
    XML.ParseXML (Profile);

    if not XML.FilterStreams(True, True) then
    begin
      DebugOut( 'XML.FilterStreams failed' );
      Exit;
    end;

    if OutPutFilterSetting.AudioBitRate <> -1 then
      XML.UpdateASFValue( 'BitRate', IntToStr(OutPutFilterSetting.AudioBitRate), xdt_AudioStream );

    if OutPutFilterSetting.AudioChannels in [1..2] then
      XML.UpdateASFValue( 'nChannels', IntToStr(OutPutFilterSetting.AudioChannels), xdt_AudioStream );

    if OutPutFilterSetting.VideoBitRate <> -1 then
    begin
      XML.UpdateASFValue('BitRate', IntToStr(OutPutFilterSetting.VideoBitRate), xdt_VideoStream );
      XML.UpdateASFValue('dwBitRate', IntToStr(OutPutFilterSetting.VideoBitRate), xdt_VideoStream );
    end;

    if (OutPutFilterSetting.VideoWidth >= 0) and (OutPutFilterSetting.VideoHeight >= 0) then
    begin
      XML.UpdateASFValue('biwidth', IntToStr(OutPutFilterSetting.VideoWidth), xdt_VideoStream );
      XML.UpdateASFValue('right', IntToStr(OutPutFilterSetting.VideoWidth), xdt_VideoStream );
      XML.UpdateASFValue('biheight', IntToStr(OutPutFilterSetting.VideoHeight), xdt_VideoStream );
      XML.UpdateASFValue('bottom', IntToStr(OutPutFilterSetting.VideoHeight), xdt_VideoStream );
    end;

    if OutPutFilterSetting.VideoQuality <> -1 then
      XML.UpdateASFValue( 'Quality', IntToStr(OutPutFilterSetting.VideoQuality), xdt_VideoStream );

    if OutPutFilterSetting.VideoMaxKeyFrameSpacing <> -1 then
      XML.UpdateASFValue( 'MaxKeyFrameSpacing', IntToStr(OutPutFilterSetting.VideoMaxKeyFrameSpacing), xdt_VideoStream );


    if not XML.GenerateXML then Exit;

    pcXML := StringTOOleStr( XML.GetGeneratedXML );
    if Failed(WMProfileManager.LoadProfileByData(pcXML, WMProfile)) then
    begin
      DebugOut( 'WMProfileManager.LoadProfileByData Fail' );
      WMProfileManager := nil;
      WMProfile := nil;
      Exit;
    end;
    if Failed(ConfigAsfWriter.ConfigureFilterUsingProfile(WMProfile)) then
    begin
      DebugOut( 'ConfigAsfWriter.ConfigureFilterUsingProfile Fail' );
      WMProfileManager := nil;
      WMProfile := nil;
      Exit;
    end;
  finally
    XML.Free;
  end;

  if WMProfile <> nil then WMProfile := nil;
  if WMProfileManager <> nil then WMProfileManager := nil;
  if WMProfileManager2 <> nil then WMProfileManager2 := nil;
  if ConfigAsfWriter <> nil then ConfigAsfWriter := nil;
end;

procedure TxdRecordCap.DoConfigVideoSourceFilter;
var
  pVideoConfig: IAMStreamConfig;
  hr: HRESULT;
  nPiCount, nPiSize: Integer;
  Scc: TVideoStreamConfigCaps;
  pMT: PAMMediaType;

  function SetVideoConfig(const AMainType, ASubType: TGUID): Boolean;
  var
    i: Integer;
    pVih: PVideoInfoHeader;
  begin
    Result := False;
    for i := 0 to nPiCount - 1 do
    begin
      hr := pVideoConfig.GetStreamCaps( i, pMT, Scc );
      if Succeeded(hr) then
      begin
        try
          if  IsEqualGUID(pMT^.formattype, AMainType) and IsEqualGUID(pMT^.subtype, ASubType) then
          begin
            Result := Succeeded( pVideoConfig.SetFormat(pMT^) ); //Config first: uses maintype subtype
            if not Result then Continue;
            pVih := PVideoInfoHeader( pMT^.pbFormat );
            pVih^.AvgTimePerFrame := Round(10000000 / FSourceFilterSetting.VideoFrame);
            pVih^.bmiHeader.biWidth := FSourceFilterSetting.VideoWidth;
            pVih^.bmiHeader.biHeight := FSourceFilterSetting.VideoHeight;
            pVih^.bmiHeader.biBitCount := FSourceFilterSetting.FBitCount;
            pVih^.bmiHeader.biSizeImage := pVih^.bmiHeader.biWidth * pVih^.bmiHeader.biHeight * FSourceFilterSetting.FPixel;
            pMT.lSampleSize := pVih^.bmiHeader.biSizeImage;
            if Succeeded(pVideoConfig.SetFormat(pMT^)) then
              Break;
          end;
        finally
          DeleteMediaType( pMT );
        end;
      end;
    end;
  end;
begin
  hr := (FCaptureFilterGraph as ICaptureGraphBuilder2).FindInterface( @PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video,
         FVideoCapFilter as IBAseFilter, IID_IAMStreamConfig, pVideoConfig);
  if Failed(hr) then
  begin
    DebugOut( 'Can not find Interface: IAMStreamConfig' );
    Exit;
  end;
  pVideoConfig.GetNumberOfCapabilities( nPiCount, nPiSize );
  if nPiSize = SizeOf(TVideoStreamConfigCaps) then
  begin
    //RGB24 位优先
    FSourceFilterSetting.FBitCount := 24;
    FSourceFilterSetting.FPixel := 3;
    if not SetVideoConfig(FORMAT_VideoInfo, MEDIASUBTYPE_RGB24) then
    begin
      //YUY2 视频流
      FSourceFilterSetting.FBitCount := 16;
      FSourceFilterSetting.FPixel := 2;
      if not SetVideoConfig(FORMAT_VideoInfo, MEDIASUBTYPE_YUY2) then
      begin
        Stop;
        pVideoConfig := nil;
        raise Exception.Create( '不支持的视频头，请选择支持RGB24或YUV格式的摄像头' );
      end;
    end;
  end;
  pVideoConfig := nil;
end;

function TxdRecordCap.GetRecordTime(var Hour, Min, Sec, MSec: Word): Boolean;
var
  position: int64;
const
  MiliSecInOneDay = 86400000;
begin
  Result := FCaptureFilterGraph.Active;
  if Result then
  begin
    with FCaptureFilterGraph as IMediaSeeking do
      GetCurrentPosition(position);
    DecodeTime(position div 10000 / MiliSecInOneDay, Hour, Min, Sec, MSec);
  end;
end;

procedure TxdRecordCap.Loaded;
begin
  inherited;
end;

procedure TxdRecordCap.NotifyEvent(AEvent: TNotifyEvent);
begin
  if Assigned(AEvent) then
    AEvent( Self );
end;

procedure TxdRecordCap.Paint;
begin
  inherited;
  case FRecordStyle of
    rsNULL:
    begin
      if (FBackBitmap.Width > 0) and (FBackBitmap.Height > 0) then
        Canvas.CopyRect( Rect(0, 0, Width, Height), FBackBitmap.Canvas, Rect(0, 0, FBackBitmap.Width, FBackBitmap.Height) )
      else
        inherited;
    end;
    rsRecord,
    rsPreview: inherited;
  end;
end;

function TxdRecordCap.PreviewByOverlay: Boolean;
begin
  Result := False;
  try
    CheckOverlayFilter;
  except
    Exit;
  end;

  with FCaptureFilterGraph as IGraphBuilder do
    AddFilter( FOverlayFilter, 'Mux Overlay Filter' );

  with FCaptureFilterGraph as IcaptureGraphBuilder2 do
  begin
    CheckHResult( RenderStream( @PIN_CATEGORY_PREVIEW, nil, FVideoCapFilter as IBaseFilter, nil, FOverlayFilter as IBaseFilter) );
    CheckHResult( RenderStream( nil, nil, FOverlayFilter as IBaseFilter, nil, FVideoWindow as IBaseFilter) );
  end;
  Result := True;
end;

function TxdRecordCap.RecordingByOverlay: Boolean;
var
  sinkFilter: IFileSinkFilter;
begin
  Result := False;
  try
    CheckOverlayFilter;
  except
    Exit;
  end;

  with FCaptureFilterGraph as IGraphBuilder do
    AddFilter( FOverlayFilter, 'Mux Overlay Filter' );

  with FCaptureFilterGraph as IcaptureGraphBuilder2 do
  begin
    CheckHResult( SetOutputFileName( MEDIASUBTYPE_Asf, PWideChar(FOutPutFilterSetting.OutPutFileName), FmuxFilter, sinkFilter ) );
    DoConfigOutputFile;

    CheckHResult( RenderStream( @PIN_CATEGORY_CAPTURE, nil, FVideoCapFilter as IBaseFilter, FOverlayFilter as IBaseFilter, FmuxFilter) );
    CheckHResult( RenderStream( nil, nil, FOverlayFilter as IBaseFilter, nil, FVideoWindow as IBaseFilter) );
  end;
  Result := True;
end;

procedure TxdRecordCap.SetBackBitmap(const Value: TBitmap);
begin
  FBackBitmap.Assign( Value );
  Invalidate;
end;

procedure TxdRecordCap.SetOutPutFilterSetting(const Value: TOutPutFilterSetting);
begin
  FOutPutFilterSetting.Assign( Value );
end;

procedure TxdRecordCap.SetOverlayEffer(const AEfferClass: TOverlayEffect);
begin
  FOverlayEffect := AEfferClass;
end;

procedure TxdRecordCap.SetSourceFilterSetting(const Value: TSourceFilterSetting);
begin
  FSourceFilterSetting.Assign( Value );
end;

function TxdRecordCap.StartPreview: Boolean;
begin
  Result := False;
  if RecordStyle <> rsNULL then Exit;
  if (not CreateBaseFilter) or (not CheckVideoCapFilter) then
  begin
    Result := False;
    Stop;
    Exit;
  end;
  Result := True;
  try
    FCaptureFilterGraph.Active := True;
    DoConfigVideoSourceFilter;
    PreviewByOverlay;
    FCaptureFilterGraph.Play;
  except
    Result := False;
    DebugOut( '无法创建预览回路' );
  end;
  if Result then
  begin
    FRecordStyle := rsPreview;
    NotifyEvent( OnBeginPriview );
  end;
end;

function TxdRecordCap.StartRecord: Boolean;
begin
  if RecordStyle = rsRecord then
  begin
    Result := False;
    DebugOut( 'Recording...' );
    Exit;
  end
  else if RecordStyle = rsPreview then
    Stop;
    
  //创建录制使用对象
  if (not CreateBaseFilter) or (not CheckVideoCapFilter) or (not CheckAudioCapFilter) then
  begin
    Result := False;
    Stop;
    Exit;
  end;
  Result := True;
  try
    Set8087CW($133f);
    FCaptureFilterGraph.Active := True;
    DoConfigAudioSourceFilter;
    DoConfigVideoSourceFilter;
    RecordingByOverlay;
    AddAudioCap;
    FCaptureFilterGraph.Play;
  except
    on e:Exception do
    begin
      Result := False;
      DebugOut( Format('无法创建录制回路: %s', [e.Message]) );
    end;
  end;
  if Result then
  begin
    FRecordStyle := rsRecord;
    NotifyEvent( OnBeginRecord );
  end;
end;

procedure TxdRecordCap.Stop;
begin
  try
    if FCaptureFilterGraph <> nil then
    begin
      FCaptureFilterGraph.Stop;
      FCaptureFilterGraph.Active := False;

      FAudioCapFilter := nil;
      FVideoCapFilter := nil;
      FreeAndNil( FVideoWindow );
      FreeAndNil( FCaptureFilterGraph );
      FreeCapRecordInfo;
      InitCapRecordInfo;
      FmuxFilter := nil;
      NotifyEvent( OnStop );
    end;
    FRecordStyle := rsNULL;
  except

  end;
end;

{ TSourceFilterSetting }

procedure TSourceFilterSetting.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSourceFilterSetting then
  begin
    with Source as TSourceFilterSetting do
    begin
      Self.VideoWidth := VideoWidth;
      Self.VideoHeight := VideoHeight;
      Self.VideoFrame := VideoFrame;
      Self.FBitCount := FBitCount;
      Self.FPixel := FPixel;
      Self.VideoCapIndex := VideoCapIndex;
      Self.AudioCapIndex := AudioCapIndex;
      Self.AudioCapInPinIndex := AudioCapInPinIndex;
    end;
  end;
end;

constructor TSourceFilterSetting.Create;
begin
  FVideoWidth := 320;
  FVideoHeight := 240;
  FVideoFrame := 25;
  FVideoCapIndex := -1;
  FAudioCapIndex := -1;
  FAudioCapInPinIndex := -1;
end;

destructor TSourceFilterSetting.Destroy;
begin

  inherited;
end;

procedure TSourceFilterSetting.SetAudioCapIndex(const Value: Integer);
begin
  FAudioCapIndex := Value;
end;

procedure TSourceFilterSetting.SetAudioCapInPinIndex(const Value: Integer);
begin
  FAudioCapInPinIndex := Value;
end;

procedure TSourceFilterSetting.SetVideoCapIndex(const Value: Integer);
begin
  FVideoCapIndex := Value;
end;

procedure TSourceFilterSetting.SetVideoFrame(const Value: Integer);
begin
  FVideoFrame := Value;
end;

procedure TSourceFilterSetting.SetVideoHeight(const Value: Integer);
begin
  FVideoHeight := Value;
end;

procedure TSourceFilterSetting.SetVideoWidth(const Value: Integer);
begin
  FVideoWidth := Value;
end;

{ TOutPutFilterSetting }

procedure TOutPutFilterSetting.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TOutPutFilterSetting then
  begin
    with Source as TOutPutFilterSetting do
    begin
      Self.AudioBitRate := AudioBitRate;
      Self.AudioChannels := AudioChannels;
      Self.VideoBitRate := VideoBitRate;
      Self.VideoWidth := VideoWidth;
      Self.VideoHeight := VideoHeight;
      Self.VideoMaxKeyFrameSpacing := VideoMaxKeyFrameSpacing;
      Self.VideoQuality := VideoQuality;
      Self.OutPutFileName := OutPutFileName;
    end;
  end;
end;

constructor TOutPutFilterSetting.Create;
begin
  FAudioBitRate            := -1;
  FAudioChannels           := 2;
  FVideoWidth              := 320;
  FVideoBitRate            := -1;
  FVideoMaxKeyFrameSpacing := -1;
  FVideoQuality            := -1;
  FVideoHeight             := 240;
  FOutPutFileName          := ExtractFilePath( ParamStr(0) ) + 'xdTest.wmv';
end;

destructor TOutPutFilterSetting.Destroy;
begin

  inherited;
end;

procedure TOutPutFilterSetting.SetAudioBitRate(const Value: Integer);
begin
  FAudioBitRate := Value;
end;

procedure TOutPutFilterSetting.SetAudioChannels(const Value: Integer);
begin
  FAudioChannels := Value;
end;

procedure TOutPutFilterSetting.SetOutPutFileName(const Value: WideString);
begin
  FOutPutFileName := Value;
end;

procedure TOutPutFilterSetting.SetVideoBitRate(const Value: Integer);
begin
  FVideoBitRate := Value;
end;

procedure TOutPutFilterSetting.SetVideoHeight(const Value: Integer);
begin
  FVideoHeight := Value;
end;

procedure TOutPutFilterSetting.SetVideoMaxKeyFrameSpacing(const Value: Integer);
begin
  FVideoMaxKeyFrameSpacing := Value;
end;

procedure TOutPutFilterSetting.SetVideoQuality(const Value: Integer);
begin
  FVideoQuality := Value;
end;

procedure TOutPutFilterSetting.SetVideoWidth(const Value: Integer);
begin
  FVideoWidth := Value;
end;

initialization
  InitCapRecordInfo;

finalization
  FreeCapRecordInfo;

end.
