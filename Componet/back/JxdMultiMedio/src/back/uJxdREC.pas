{
单元名称： uJxdREC
作    者： 江晓德
QQ      ： 67068633
Email   :  jxd524@163.com
创建日期： 2010-06-23
功    能： 对视频头与声卡立体声混音两者进行录制，并保存为ASF文件格式（WMV）。支持预览
说    明：
           1：VideoFrameRate 属性只是对视频源的设置，设置每秒的帧频
           2：VideoWidth 和 VideoHeight 对视频源与输出文件进行设置
           3：其它属性只针对输出文件设置。
           4：MuxDraw 属性可以设置对视频流进行叠加操作发现，当它可用时，对 OnDrawCaptureVideo 进行操作，就是对视频流进行操作
           5：AudioChannels：1：单声道；2：立体声
           6：属性设置为-1，就使用系统默认来设置视频源与输出文件
           7：GetVideoCapFilters 和 GetAudioCapFilters 分别可以得到系统有关视频与声卡的硬件信息
           8: FileName 设置输出文件的名称
}

unit uJxdREC;

interface

{$Define KBoxDebug}

uses
  Windows, Classes, ExtCtrls, Controls, Messages, DirectShow9, SysUtils, ActiveX, DSPack,
  uJxdOverlayMuxFilter, uDShowSub, Graphics, WMF9, uJxdAsfXml, Forms
  {$IfDef KBoxDebug}
  ,uDebugInfo
  {$EndIf}
  ;

type
  TCaptureBitmap = record
    FBmp: TBitmap;
    FWaitFor: Boolean;
    FMux: Boolean;
    FFinished: Boolean;
  end;

  TRecordStyle = (rsNULL, rsRecord, rsPreview);
  TxdREC = class(TCustomPanel)
  public
    procedure Stop;
    function  StartRecord: Boolean;
    function  StartPreview: Boolean;
    function  GetRecordTime(var Hour, Min, Sec, MSec: Word): Boolean;
    function  GetVideoBitmap(var ABmp: TBitmap): Boolean;

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    procedure Loaded; override;
    function  CheckCapVideo: Boolean;
    function  CheckCapAudio: Boolean;
    function  CheckOverlayMuxFilter: Boolean;
    procedure DoErrorInfo(const AErrorMsg: string);
    procedure DoConfigCapture; virtual;
    procedure DoConfigWriteFilter;
    procedure DoConfigCapSourceFilter;
  protected
    FVidoeMajorType, FVidoeSubType: TGUID;
    FCaptureGraph: TFilterGraph;
    FVideoWindow: TVideoWindow;
    FVideoSourceFilter: TFilter;
    FAudioSourceFilter: TFilter;
    FWMWriterFilter: IBaseFilter;
    FOverlayMuxFilter: TOverlayMuxFilter;
    FSmartTee: IBaseFilter;
  private
    function  CreateRecordObject: Boolean;
    function  RenderCaptureStream: Boolean;
    function  RenderPreviewStream: Boolean;
    procedure FreeRecordObject;
    procedure DoVideoDraw(Sender: TObject; AMemDC: HDC; const AWidth, AHeight: Integer);
  private
    FRecordStyle: TRecordStyle;
    FOutputFileName: WideString;
    FCapVideoIndex: Integer;
    FCapAudioIndex: Integer;
    FOnDrawCaptureVideo: TOnDraw;
    FMuxDraw: Boolean;
    FAudioBitRate: Integer;
    FAudioChannels: Integer;
    FVideoBitRate: Integer;
    FVideoWidth: Integer;
    FVideoHeight: Integer;
    FVideoQuality: Integer;
    FVideoMaxKeyFrameSpacing: Integer;
    FOnBeginRecord: TNotifyEvent;
    FOnEndRecord: TNotifyEvent;
    FVideoFrameRate: Double;
    FCaptureBmp: TCaptureBitmap;

    procedure SetOutputFileName(const Value: WideString);
    procedure SetCapVideoIndex(const Value: Integer);
    procedure SetCapAudioIndex(const Value: Integer);
    procedure SetMuxDraw(const Value: Boolean);
    procedure SetAudioBitRate(const Value: Integer);
    procedure SetAudioChannels(const Value: Integer);
    procedure SetVideoBitRate(const Value: Integer);
    procedure SetVideoHeight(const Value: Integer);
    procedure SetVideoWidth(const Value: Integer);
    procedure SetVideoQuality(const Value: Integer);
    procedure SetVideoMaxKeyFrameSpacing(const Value: Integer);
    procedure SetVideoFrameRate(const Value: Double);
  published
    property Align;
    property Alignment;
    property Anchors;
    property Color;
    property TabStop;

    property AudioBitRate: Integer read FAudioBitRate write SetAudioBitRate;
    property AudioChannels: Integer read FAudioChannels write SetAudioChannels;
    property VideoFrameRate: Double read FVideoFrameRate write SetVideoFrameRate;    //只设置视频源
    property VideoBitRate: Integer read FVideoBitRate write SetVideoBitRate;
    property VideoWidth: Integer read FVideoWidth write SetVideoWidth;
    property VideoHeight: Integer read FVideoHeight write SetVideoHeight;
    property VideoQuality: Integer read FVideoQuality write SetVideoQuality;
    property VideoMaxKeyFrameSpacing: Integer read FVideoMaxKeyFrameSpacing write SetVideoMaxKeyFrameSpacing;
    property MuxDraw: Boolean read FMuxDraw write SetMuxDraw;
    property CapVideoIndex: Integer read FCapVideoIndex write SetCapVideoIndex;
    property CapAudioIndex: Integer read FCapAudioIndex write SetCapAudioIndex;
    property FileName: WideString read FOutputFileName write SetOutputFileName;

    property OnDrawCaptureVideo: TOnDraw read FOnDrawCaptureVideo write FOnDrawCaptureVideo;
    property OnBeginRecord: TNotifyEvent read FOnBeginRecord write FOnBeginRecord;
    property OnEndRecord: TNotifyEvent read FOnEndRecord write FOnEndRecord; 

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
  end;

function GetVideoCapFilters: string;
function GetAudioCapFilters: string;

procedure DebugOut(const AInfo: string);

implementation

uses
  DSUtil;
{ TKBoxREC }

{$IfDef KBoxDebug}
const
  CtDebugLog = 'DebugLog.txt';
{$EndIf}

var
  _CapEnum: TSysDevEnum;

procedure DebugOut(const AInfo: string);
begin
  OutputDebugString( PChar(AInfo) );
  {$IfDef KBoxDebug}
  _Log( AInfo, CtDebugLog );
  {$EndIf}
end;

function TxdREC.CheckCapAudio: Boolean;
begin
  Result := False;
  if FCaptureGraph = nil then
  begin
    DoErrorInfo( 'please create base filter first' );
    Exit;
  end;
  _CapEnum.SelectGUIDCategory( CLSID_AudioInputDeviceCategory );
  if (FCapAudioIndex = -1) or (FCapAudioIndex >= _CapEnum.CountFilters) then
  begin
    DoErrorInfo( 'please set Audio capture frist' );
    Exit;
  end;
  if FAudioSourceFilter = nil then
    FAudioSourceFilter := TFilter.Create( Self );
  FAudioSourceFilter.BaseFilter.Moniker := _CapEnum.GetMoniker( FCapAudioIndex );
  FCaptureGraph.Active := True;
  FAudioSourceFilter.FilterGraph := FCaptureGraph;
  FCaptureGraph.Active := False;
  Result := True;
end;

function TxdREC.CheckCapVideo: Boolean;
begin
  Result := False;
  if FCaptureGraph = nil then
  begin
    DoErrorInfo( 'please create base filter first' );
    Exit;
  end;
  _CapEnum.SelectGUIDCategory( CLSID_VideoInputDeviceCategory );
  if (FCapVideoIndex = -1) or (FCapVideoIndex >= _CapEnum.CountFilters) then
  begin
    DoErrorInfo( 'please set video capture frist' );
    Exit;
  end;
  if FVideoSourceFilter = nil then
    FVideoSourceFilter := TFilter.Create( Self );
  FVideoSourceFilter.BaseFilter.Moniker := _CapEnum.GetMoniker( FCapVideoIndex );
  FCaptureGraph.Active := True;
  FVideoSourceFilter.FilterGraph := FCaptureGraph;
  FCaptureGraph.Active := False;
  Result := True;
end;

function TxdREC.CheckOverlayMuxFilter: Boolean;
var
  hRes: HRESULT;
begin
  Result := False;
  if FCaptureGraph = nil then
  begin
    DoErrorInfo( 'please create base filter first' );
    Exit;
  end;
  if FOverlayMuxFilter = nil then
  begin
    FOverlayMuxFilter := TOverlayMuxFilter.Create( hRes );
    if Failed(hRes) then
    begin
      DoErrorInfo( 'Mux overlay filter create fail!' );
      Exit;
    end;
//    FOverlayMuxFilter.OnDraw := DoVideoDraw;
//    FOverlayMuxFilter.OwnerHandle := Handle;
//    FOverlayMuxFilter.MuxDraw := FMuxDraw;
  end;
//  FOverlayMuxFilter.FilterGraph := FCaptureGraph;
//  FCaptureGraph.Active := True;
  (FCaptureGraph as IGraphBuilder).AddFilter( FOverlayMuxFilter, 'xxxxx' );
//  FCaptureGraph.Active := False;
  Result := True;
end;

constructor TxdREC.Create(AOwner: TComponent);
begin
  inherited;
  FRecordStyle := rsNULL;
  FCapVideoIndex := 1;
  FCapAudioIndex := 0;
  FVidoeMajorType := MEDIATYPE_Video;
  FVidoeSubType := MEDIASUBTYPE_RGB24;
  FOutputFileName := ExtractFilePath( ParamStr(0) ) + 'xdTestRec.wmv';
  FMuxDraw := True;
  FAudioBitRate := -1;
  FAudioChannels := 2;
  FVideoFrameRate := 30.0;
  FVideoBitRate := -1;
  FVideoWidth := 320;
  FVideoHeight := 240;
  VideoQuality := -1;
  VideoMaxKeyFrameSpacing := -1;
  FCaptureBmp.FBmp := nil;
  FCaptureBmp.FWaitFor := False;
end;

function TxdREC.CreateRecordObject: Boolean;
begin
  Result := True;
  try
    FCaptureGraph := TFilterGraph.Create( Self );
    with FCaptureGraph do
    begin
      Mode := gmCapture;
      LinearVolume := True;
    end;

    FVideoWindow := TVideoWindow.Create( Self );
    FVideoWindow.Parent := Self;
    with FVideoWindow do
    begin
      Align := alClient;
      FilterGraph := FCaptureGraph;
      Mode := vmNormal;
      Visible := True;
    end;
  except
    Result := False;
  end;
end;

destructor TxdREC.Destroy;
begin
  if FRecordStyle <> rsNULL then
    Stop;
  inherited;
end;


procedure TxdREC.DoConfigCapSourceFilter;
var
  pVidoeConfig: IAMStreamConfig;
  hRes: HRESULT;
  pMT: PAMMediaType;
  pHeader: PVideoInfoHeader;
  PinList: TPinList;
  VideoMediaTypes: TEnumMediaType;
  i: Integer;
begin
  //Audio config
  PinList := TPinList.Create(FAudioSourceFilter as IBaseFilter);
  i := 0;
    while i < PinList.Count do
      if PinList.PinInfo[i].dir = PINDIR_OUTPUT then
        begin
          if 0 <> -1 then  ;
//            with (PinList.Items[i] as IAMStreamConfig) do
//              SetFormat(AudioMediaTypes.Items[AudioFormats.ItemIndex].AMMediaType^);
          PinList.Delete(i);
        end else inc(i);
  with (PinList.Items[0] as IAMAudioInputMixer) do
      put_Enable(true);
  PinList.Free;

  //Vidoe config
  hRes := (FCaptureGraph as ICaptureGraphBuilder2).FindInterface( @PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video,
                                                       FVideoSourceFilter as IBAseFilter, IID_IAMStreamConfig, pVidoeConfig );
  if Failed(hRes) then
  begin
    DoErrorInfo( 'QueryInterface IID_IAMStreamConfig Fail: ' + GetAMErrorText(hRes) );
    Exit;
  end;

  PinList := TPinList.Create( FVideoSourceFilter as IBaseFilter );
  VideoMediaTypes := TEnumMediaType.Create;
  VideoMediaTypes.Assign(PinList.First);
  with (PinList.First as IAMStreamConfig) do
    SetFormat(VideoMediaTypes.Items[4].AMMediaType^);
  PinList.Free;
  VideoMediaTypes.Free;
  Exit;

  if Failed(pVidoeConfig.GetFormat(pMT)) then
  begin
    DoErrorInfo( 'pVidoeConfig.GetFormat Fail: ' + GetAMErrorText(hRes) );
    if pVidoeConfig <> nil then pVidoeConfig := nil;
    Exit;
  end;
  if IsEqualGUID(FORMAT_VideoInfo, pMT^.formattype) then
  begin
//    pMT^.subtype := MEDIASUBTYPE_RGB24;
    DebugOut( 'DoConfigCapSourceFilter subtype: ' + GUIDToString(pMT^.subtype) );
    pHeader := PVideoInfoHeader( pMT^.pbFormat );
    pHeader^.bmiHeader.biWidth := VideoWidth;
    pHeader^.bmiHeader.biHeight := VideoHeight;
    pHeader^.AvgTimePerFrame := Round( 10000000 / VideoFrameRate );
  end;
  hRes := pVidoeConfig.SetFormat( pMT^ );
  if Failed(hRes) then
    DoErrorInfo( 'pVidoeConfig.SetFormat Fail: ' + GetAMErrorText(hRes) );

  if pMT <> nil then DeleteMediaType( pMT );
  if pVidoeConfig <> nil then pVidoeConfig := nil;
end;
//var
//  pVidoeConfig: IAMStreamConfig;
//  hRes: HRESULT;
//  pMT: PAMMediaType;
//  pHeader: PVideoInfoHeader;
//begin
//  //Vidoe config
//  hRes := (FCaptureGraph as ICaptureGraphBuilder2).FindInterface( @PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video,
//                                                       FVideoSourceFilter as IBAseFilter, IID_IAMStreamConfig, pVidoeConfig );
//  if Failed(hRes) then
//  begin
//    DoErrorInfo( 'QueryInterface IID_IAMStreamConfig Fail: ' + GetAMErrorText(hRes) );
//    Exit;
//  end;
//
//  if Failed(pVidoeConfig.GetFormat(pMT)) then
//  begin
//    DoErrorInfo( 'pVidoeConfig.GetFormat Fail: ' + GetAMErrorText(hRes) );
//    if pVidoeConfig <> nil then pVidoeConfig := nil;
//    Exit;
//  end;
//  if IsEqualGUID(FORMAT_VideoInfo, pMT^.formattype) then
//  begin
////    pMT^.subtype := MEDIASUBTYPE_RGB24;
//    DebugOut( 'DoConfigCapSourceFilter subtype: ' + GUIDToString(pMT^.subtype) );
//    pHeader := PVideoInfoHeader( pMT^.pbFormat );
//    pHeader^.bmiHeader.biWidth := VideoWidth;
//    pHeader^.bmiHeader.biHeight := VideoHeight;
//    pHeader^.AvgTimePerFrame := Round( 10000000 / VideoFrameRate );
//  end;
//  hRes := pVidoeConfig.SetFormat( pMT^ );
//  if Failed(hRes) then
//    DoErrorInfo( 'pVidoeConfig.SetFormat Fail: ' + GetAMErrorText(hRes) );
//
//  if pMT <> nil then DeleteMediaType( pMT );
//  if pVidoeConfig <> nil then pVidoeConfig := nil;
//end;

procedure TxdREC.DoConfigCapture;
begin
  DoConfigCapSourceFilter;
  DoConfigWriteFilter;
end;


procedure TxdREC.DoConfigWriteFilter;
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
  if Failed(FWMWriterFilter.QueryInterface(IID_IConfigAsfWriter, ConfigAsfWriter)) then
  begin
    DoErrorInfo( 'Can not queryInterface: IID_IConfigAsfWrite' );
    Exit;
  end;
  if Failed (WMCreateProfileManager(WMProfileManager)) then
  begin
    DoErrorInfo( 'Can not Create ProfileManage' );
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
    DoErrorInfo( 'Can not GetCurrentProfile' );
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
      DoErrorInfo( 'XML.FilterStreams failed' );
      Exit;
    end;

    if AudioBitRate <> -1 then
      XML.UpdateASFValue ('BitRate', IntToStr (AudioBitRate), xdt_AudioStream);

    if AudioChannels in [1..2] then
      XML.UpdateASFValue ('nChannels', IntToStr (AudioChannels), xdt_AudioStream);

    if VideoBitRate <> -1 then
    begin
      XML.UpdateASFValue ('BitRate', IntToStr (VideoBitRate), xdt_VideoStream);
      XML.UpdateASFValue ('dwBitRate', IntToStr (VideoBitRate), xdt_VideoStream);
    end;

    if (VideoWidth >= 0) and (VideoHeight >= 0) then begin
      XML.UpdateASFValue ('biwidth', IntToStr (VideoWidth), xdt_VideoStream);
      XML.UpdateASFValue ('right', IntToStr (VideoWidth), xdt_VideoStream);
      XML.UpdateASFValue ('biheight', IntToStr (VideoHeight), xdt_VideoStream);
      XML.UpdateASFValue ('bottom', IntToStr (VideoHeight), xdt_VideoStream);
    end;

    if VideoQuality <> -1 then
      XML.UpdateASFValue ('Quality', IntToStr (VideoQuality), xdt_VideoStream);

    if VideoMaxKeyFrameSpacing <> -1 then
      XML.UpdateASFValue ('MaxKeyFrameSpacing', IntToStr (VideoMaxKeyFrameSpacing), xdt_VideoStream);


    if not XML.GenerateXML then Exit;

    pcXML := StringTOOleStr (XML.GetGeneratedXML);
    if Failed(WMProfileManager.LoadProfileByData(pcXML, WMProfile)) then
    begin
      DoErrorInfo( 'WMProfileManager.LoadProfileByData Fail' );
      WMProfileManager := nil;
      WMProfile := nil;
      Exit;
    end;
    if Failed(ConfigAsfWriter.ConfigureFilterUsingProfile(WMProfile)) then
    begin
      DoErrorInfo( 'ConfigAsfWriter.ConfigureFilterUsingProfile Fail' );
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

procedure TxdREC.DoErrorInfo(const AErrorMsg: string);
begin
  DebugOut( AErrorMsg );
end;

procedure TxdREC.DoVideoDraw(Sender: TObject; AMemDC: HDC; const AWidth, AHeight: Integer);
begin
  if FCaptureBmp.FWaitFor and not FCaptureBmp.FFinished then
  begin
    if FCaptureBmp.FMux then
    begin
      if Assigned(FOnDrawCaptureVideo) then
        OnDrawCaptureVideo( Self, AMemDC, AWidth, AHeight );
    end
    else
      FOverlayMuxFilter.MuxDraw := False;

    if FCaptureBmp.FBmp = nil then
      FCaptureBmp.FBmp := TBitmap.Create;
    FCaptureBmp.FBmp.Width := AWidth;
    FCaptureBmp.FBmp.Height := AHeight;
    BitBlt( FCaptureBmp.FBmp.Canvas.Handle, 0, 0, AWidth, AHeight, AMemDC, 0, 0, SRCCOPY );
    FCaptureBmp.FWaitFor := False;
    FCaptureBmp.FFinished := True;
    Exit;
  end;
  if Assigned(FOnDrawCaptureVideo) then
    OnDrawCaptureVideo( Self, AMemDC, AWidth, AHeight );
end;

procedure TxdREC.FreeRecordObject;
begin
  if FVideoSourceFilter <> nil then
    FreeAndNil( FVideoSourceFilter );
  if FAudioSourceFilter <> nil then
    FreeAndNil( FAudioSourceFilter );
  if FSmartTee <> nil then
    FSmartTee := nil;

  if FVideoWindow <> nil then
  begin
    FVideoWindow.FilterGraph := nil;
    FreeAndNil( FVideoWindow );
  end;
  if FCaptureGraph <> nil then
    FreeAndNil( FCaptureGraph );

  if FOverlayMuxFilter <> nil then
    FOverlayMuxFilter := nil;
end;

function TxdREC.GetRecordTime(var Hour, Min, Sec, MSec: Word): Boolean;
var
  position: int64;
const
  MiliSecInOneDay = 86400000;
begin
  Result := FCaptureGraph.Active;
  if Result then
  begin
    with FCaptureGraph as IMediaSeeking do
      GetCurrentPosition(position);
    DecodeTime(position div 10000 / MiliSecInOneDay, Hour, Min, Sec, MSec);
  end;
end;

function TxdREC.GetVideoBitmap(var ABmp: TBitmap): Boolean;
var
  bMuxDraw: Boolean;
  nTryCount: Integer;
begin
  Result := (FRecordStyle = rsRecord) and Assigned(FCaptureGraph) and FCaptureGraph.Active;
  if not Result then Exit;

  bMuxDraw := FOverlayMuxFilter.MuxDraw;
  if FCaptureBmp.FBmp = nil then
    FCaptureBmp.FBmp := TBitmap.Create;
  FCaptureBmp.FMux := bMuxDraw;
  FCaptureBmp.FFinished := False;
  FCaptureBmp.FWaitFor := True;
  if not FOverlayMuxFilter.MuxDraw then
    FOverlayMuxFilter.MuxDraw := True;

  nTryCount := 0;
  while not FCaptureBmp.FFinished do
  begin
    Application.ProcessMessages;
    Sleep( 50 );
    Inc( nTryCount );
    if nTryCount >= 30 then
    begin
      Result := False;
      FCaptureBmp.FFinished := False; 
      FCaptureBmp.FWaitFor := False;
      Break;
    end;
  end;
  if Result then
  begin
    if ABmp = nil then
      ABmp := TBitmap.Create;
    ABmp.Width := FCaptureBmp.FBmp.Width;
    ABmp.Height := FCaptureBmp.FBmp.Height;
    ABmp.Assign( FCaptureBmp.FBmp );
  end;
  FreeAndNil( FCaptureBmp.FBmp );
end;

procedure TxdREC.Loaded;
begin
  inherited;
  Ctl3D := False;
  BevelInner := bvNone;
  BevelKind := bkNone;
  BevelOuter := bvNone;
  BorderStyle := bsNone;
end;


function TxdREC.RenderCaptureStream: Boolean;
var
  Writer: IFileSinkFilter;
  hRes: HRESULT;
begin
  Result := True;
  try
    DoConfigCapSourceFilter;
    CheckOverlayMuxFilter;
    with FCaptureGraph as IcaptureGraphBuilder2 do
    begin
      // set the output filename
      hRes := SetOutputFileName(MEDIASUBTYPE_Asf, PWideChar(FOutputFileName), FWMWriterFilter, Writer);
      if Failed(hRes) then
      begin
        DoErrorInfo( 'Can not set the output filename' );
        Result := False;
        Exit;
      end;
      //Config ASF Record info; it change FWMWriterFilter config infomation
//      DoConfigCapture;

      RenderStream(@PIN_CATEGORY_PREVIEW, nil, FVideoSourceFilter as IBaseFilter, nil , FVideoWindow as IBaseFilter);

      //connect VideoCapture -> OverlayMux -> SmartTee
      hRes := RenderStream( @PIN_CATEGORY_CAPTURE, nil, FVideoSourceFilter as IBaseFilter,
                            FOverlayMuxFilter as IBaseFilter, FWMWriterFilter as IBaseFilter );
      if Failed(hRes) then
      begin
        DoErrorInfo( 'Can not RenderStream for OverlayMux and to SmartTee!: ' + GetAMErrorText(hRes) );
        Result := False;
        Exit;
      end;





      // Connect Audio capture streams
      hRes := RenderStream(nil, nil, FAudioSourceFilter as IBaseFilter, nil, FWMWriterFilter);
      if Failed(hRes) then
      begin
        DoErrorInfo( 'Can not RenderStream for Audio Record' );
        Result := False;
        Exit;
      end;
    end;
  except
    Result := False;
  end;
end;
//var
//  Writer: IFileSinkFilter;
//  hRes: HRESULT;
//  pin: IPin;
//begin
//  Result := True;
//  try
//    AddFilterByGuid( FCaptureGraph as IGraphBuilder, CLSID_SmartTee, 'SmartTerr Filter', FSmartTee );
//    with FCaptureGraph as IcaptureGraphBuilder2 do
//    begin
//      // set the output filename
//      hRes := SetOutputFileName(MEDIASUBTYPE_Asf, PWideChar(FOutputFileName), FWMWriterFilter, Writer);
//      if Failed(hRes) then
//      begin
//        DoErrorInfo( 'Can not set the output filename' );
//        Result := False;
//        Exit;
//      end;
//      //Config ASF Record info; it change FWMWriterFilter config infomation
//      DoConfigCapture;
//
//      //connect VideoCapture -> OverlayMux -> SmartTee
//      hRes := RenderStream( @PIN_CATEGORY_CAPTURE, nil, FVideoSourceFilter as IBaseFilter,
//                            FOverlayMuxFilter as IBaseFilter, FSmartTee );
//      if Failed(hRes) then
//      begin
//        DoErrorInfo( 'Can not RenderStream for OverlayMux and to SmartTee!: ' + GetAMErrorText(hRes) );
//        Result := False;
//        Exit;
//      end;
//
//      //SmartTee connect
//      FSmartTee.FindPin( 'Capture', pin );
//      hRes := RenderStream( nil, nil, pin, nil, FWMWriterFilter );
//      if Failed(hRes) then
//      begin
//        DoErrorInfo( 'Can not RenderStream for File write' );
//        Result := False;
//        Exit;
//      end;
//      pin := nil;
//
//      FSmartTee.FindPin( 'Preview', pin );
//      hRes := RenderStream( nil, nil, pin, nil, FVideoWindow as IBaseFilter );
//      if Failed(hRes) then
//      begin
//        DoErrorInfo( 'Can not RenderStream for VideoWindow' );
//        Result := False;
//        Exit;
//      end;
//      pin := nil;
//
//      // Connect Audio capture streams
//      hRes := RenderStream(nil, nil, FAudioSourceFilter as IBaseFilter, nil, FWMWriterFilter);
//      if Failed(hRes) then
//      begin
//        DoErrorInfo( 'Can not RenderStream for Audio Record' );
//        Result := False;
//        Exit;
//      end;
//    end;
//  except
//    Result := False;
//  end;
//end;

function TxdREC.RenderPreviewStream: Boolean;
var
  hRes: HRESULT;
begin
  Result := True;
  try
    with FCaptureGraph as IcaptureGraphBuilder2 do
    begin
      //connect VideoCapture -> OverlayMux -> SmartTee
      hRes := RenderStream( @PIN_CATEGORY_CAPTURE, nil, FVideoSourceFilter as IBaseFilter,
                            nil, FVideoWindow as IBaseFilter );
      if Failed(hRes) then
      begin
        DoErrorInfo( 'Can not RenderStream for OverlayMux and to SmartTee!' );
        Result := False;
        Exit;
      end;
    end;
  except
    Result := False;
  end;
end;

procedure TxdREC.SetAudioBitRate(const Value: Integer);
begin
  FAudioBitRate := Value;
end;

procedure TxdREC.SetAudioChannels(const Value: Integer);
begin
  FAudioChannels := Value;
end;

procedure TxdREC.SetCapAudioIndex(const Value: Integer);
begin
  FCapAudioIndex := Value;
end;

procedure TxdREC.SetCapVideoIndex(const Value: Integer);
begin
  FCapVideoIndex := Value;
end;

procedure TxdREC.SetMuxDraw(const Value: Boolean);
begin
  FMuxDraw := Value;
  if FRecordStyle = rsRecord then
    FOverlayMuxFilter.MuxDraw := FMuxDraw;
end;

procedure TxdREC.SetOutputFileName(const Value: WideString);
var
  strPath: WideString;
begin
  strPath := ExtractFilePath(Value);
  if not DirectoryExists(strPath) then
    ForceDirectories( strPath );
  FOutputFileName := Value;
end;

procedure TxdREC.SetVideoBitRate(const Value: Integer);
begin
  FVideoBitRate := Value;
end;

procedure TxdREC.SetVideoFrameRate(const Value: Double);
begin
  FVideoFrameRate := Value;
end;

procedure TxdREC.SetVideoHeight(const Value: Integer);
begin
  FVideoHeight := Value;
end;

procedure TxdREC.SetVideoMaxKeyFrameSpacing(const Value: Integer);
begin
  FVideoMaxKeyFrameSpacing := Value;
end;

procedure TxdREC.SetVideoQuality(const Value: Integer);
begin
  FVideoQuality := Value;
end;

procedure TxdREC.SetVideoWidth(const Value: Integer);
begin
  FVideoWidth := Value;
end;

function TxdREC.StartPreview: Boolean;
begin
  Result := False;

  case FRecordStyle of
    rsRecord:
    begin
      DoErrorInfo( 'Recording...' );
      Exit;
    end;
    rsPreview:
    begin
      DoErrorInfo( 'Preview...' );
      Exit;
    end;
  end;

  try
    if not CreateRecordObject then
    begin
      DoErrorInfo( 'Can not Create Record object' );
      Exit;
    end;
    FCaptureGraph.Active := True;

    if not CheckCapVideo then Exit;

    Result := RenderPreviewStream and FCaptureGraph.Play;
    if not Result then
    begin
      FreeRecordObject;
      FRecordStyle := rsNULL;
    end;
  except
    Result := False;
    FRecordStyle := rsNULL;
  end;

  if Result then
    FRecordStyle := rsPreview
  else
    FRecordStyle := rsNULL;
end;

function TxdREC.StartRecord: Boolean;
begin
  if FRecordStyle = rsRecord then
  begin
    DoErrorInfo( 'Recording...' );
    Result := False;
    Exit;
  end
  else if FRecordStyle = rsPreview then
    Stop;

  Result := False;
  try
    if not CreateRecordObject then
    begin
      DoErrorInfo( 'Can not Create Record object' );
      Exit;
    end;


    if not CheckCapVideo or not CheckCapAudio then Exit;
//    if not CheckOverlayMuxFilter then Exit;
    FCaptureGraph.Active := True;

    Result := RenderCaptureStream and FCaptureGraph.Play;
    if not Result then
    begin
      FreeRecordObject;
      FRecordStyle := rsNULL;
    end;
  except
    Result := False;
    FRecordStyle := rsNULL;
  end;

  if Result then
  begin
    FRecordStyle := rsRecord;
    if Assigned(OnBeginRecord) then
      OnBeginRecord( Self );
  end
  else
    FRecordStyle := rsNULL;
end;


procedure TxdREC.Stop;
begin
  if (FRecordStyle <> rsNULL) and (FCaptureGraph <> nil) then
  begin
    FCaptureGraph.Stop;
    FreeRecordObject;

    if (FRecordStyle = rsRecord) and Assigned(OnEndRecord) then
      OnEndRecord( Self );
  end;
  FRecordStyle := rsNULL;
end;

function GetFiltersFriendlyName(const AGUID: TGUID): string;
var
  i: integer;
begin
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

procedure InitCapRecordInfo;
begin
  CoInitialize( nil );
  _CapEnum := TSysDevEnum.Create( CLSID_AudioInputDeviceCategory );
end;
procedure FreeCapRecordInfo;
begin
  _CapEnum.Free;
  CoUninitialize;
end;


initialization
  InitCapRecordInfo;

finalization
  FreeCapRecordInfo;

end.
