{
单元名称: uJxdPlayer
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: 播放器，系统需要安装DirectX9.0c
开始时间: 2010-11-01
修改时间: 2010-11-11 (最后修改)

    播放器，支持本地播放，也可以直接播放流（需要TxdAsyncFileStream源来支持）。
    播放流的方式：
        WMV：     Stream -> ASF splitter filter(1932C124-77DA-4151-99AA-234FEA09F463) -> 智能连接
        MP3:      Stream -> ASF splitter filter -> MPEG Layer-3 Decoder(38BE3000-DBF4-11D0-860E-00A024CFEF6D) -> 智能连接
        MPEG:     Stream -> MPEG Splitter(DC257063-045F-4BE2-BD5B-E12279C464F0) -> 智能连接
        RMVB/RM:  Stream -> RealMedia Splitter(E21BE468-5C18-43EB-B0CC-DB93A847D769) -> 智能连接
        FLV/F4V:  Stream -> FLV Splitter(47E792CF-0BBE-4F7A-859C-194B0768650A) -> 智能连接
        MP4:      Stream -> Haali Splitter(564FD788-86C9-4444-971E-CC4A243DA150) -> 智能连接

备注：
      不同格式文件流播放所要创建的表（如果失败，则使用智能创建方式）
      1：WMV，一般除了 ASF splitter filter 之外，不需要再安装其它的Filter也能正常播放)
      2: MP3, MPEG 可能还需要 ffdshow 过滤器的帮助(文件名称：filters\ffdshow.ax）
}
unit uJxdPlayer;

interface

//{$DEFINE DEBUG}

uses
  Windows, Classes, Messages, SysUtils, Controls, Graphics, DirectShow9, ActiveX,
  Math, ShellAPI, DSUtil, uJxdAudioFilter, ExtCtrls, MMSystem, Forms, uJxdGpPanel,
  uJxdAsyncSource, uJxdFileSegmentStream, uDShowSub, GDIPAPI, GDIPOBJ
  {$IFDEF DEBUG}
  ,uDebugInfo
  {$ENDIF};

const
  WM_GraphNotify = WM_USER + $9878;
  WM_StopPlayer = WM_GraphNotify + 1;
  
type
  TMusicStyle = (msWMV, msMPEG, msRMVB, msFLV, msMP4, msHelp_ffdShow, msNULL);
  TPlayState = (psOpening, psOpened, psStop, psPlay, psPause, psCancel);
  TVideoMode = (vmDefault, vmVMR9);
  //vmVMR9: 使用了Direct3D 9的技术，性能比VMR-7更加强劲（VMR9需要更多的系统资源）
  //vmDefault: 优选创建VMR-7,如果创建失败，再创建旧的渲染器; 一般不会失败

  PVideoMixerBitmapInfo = ^TVideoMixerBitmapInfo;
  TVideoMixerBitmapInfo = record
    FDC: HDC;
    FSrcRect: TRect;
    FDestLeft: Single;
    FDestTop: Single;
    FDestRight: Single;
    FDestBottom: Single;
    FAlphaValue: single;
    FclrSrcKey: COLORREF;
  end;

  TOnDragFiles = procedure(Sender: TObject; const AFiles: TStringList) of object;
  TOnPaint = procedure(Sender: TObject; ADC: HDC) of object;
{$M+}
  TxdPlayer = class(TxdPanel)//(TCustomControl)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure ReChangedParent; //当改变播放器所属父类时，需要调用此方法，以便通知视频和事件过滤器
    procedure ShowCursor(const AShow: Boolean);

    function  Play: Boolean;
    procedure Pause;
    procedure Stop;

    {播放}
    function OpenFileStream(const AFileStreamID: Cardinal; const AMaxWaitTime: Cardinal = 1000 * 30): Boolean;
    function OpenFile(const AFileName: string): Boolean;

    {混合图片 AutoDisplayTime: 指定多久之后自动取消。<= 0为不自动取消}    
    function  MixerString(const APos: TPoint; const AMixerString: string; const AlphaValue: Single = 1.0; 
      const AutoDisplayTime: Integer = 0): Boolean;
    function  MixerBitmap(const AMixerInfo: TVideoMixerBitmapInfo): Boolean;
    function  GetMixerBitmapInfo(var AMixerInfo: TVideoMixerBitmapInfo): Boolean;
    procedure MixerDisable(const AAnimateBmp: Boolean);

    {截图}
    function GetCurrentBitmap(var ABmpStream: TStream): Boolean;
  protected
    procedure Resize; override;
    procedure DblClick; override;

    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMDragDropFiles(var msg: TMessage); message WM_DROPFILES;
  private
    {DirectShow接口}
    FGraph: IGraphBuilder;
    FRenderVideo: IBaseFilter;
    FRenderAudio: IBasicAudio;
    FMediaControl: IMediaControl;
    FSeeking: IMediaSeeking;
    FEvent: IMediaEventEx;
    FSpliter: IBaseFilter;
    FVMR9WinlessCtrl: IVMRWindowlessControl9;
    FVMRWinlessCtrl: IVMRWindowlessControl;
    FAsyncSourceFilter: TxdAsyncFilter;
    FAudioChannelFilter: TxdAudioChannelFilter;

    FGraphEditID: Integer;
    FFileStream: TxdP2SPFileStreamBasic;
    FFullSrceenForm: TForm;
    FMixerBitmapTimer: TTimer;
    FFullMouseStopPoint: TPoint;
    FFullSrccenShowCtrlTimer: TTimer;

    function  OpenDS: Boolean;
    procedure CloseDS;
    procedure ReleaseVideoFilter;
    //载入媒体文件之前
    procedure ConfigDefaultVMR; //配置默认VMR，可能是VMR7或其它的渲染器
    procedure ConfigVMR9; //配置VMR9渲染器
    //载入媒体文件之后
    function  ConfigPlayerAfterLoadFile: Boolean; //载入文件之后再配置播放器
    function  OpenPlayered(const AConfigOK: Boolean): Boolean;

    function  BuildSplitterFilterToGraph(const guid: TGUID): Boolean;
    function  CreateFilter(const guid: TGUID; const AIsAddToGraph: Boolean; const AFilterName: WideString; var AFilter: IBaseFilter): Boolean;
    function  RenderOutPin(var AFilter: IBaseFilter): Boolean;
    function  GetStreamSplitter(const AFileName: string): TGUID; //选择分离器，将影响在线播放能力
    procedure ChangedVideoPosition(const AShowWinHandle: HWND; const ApRect: PRect = nil);
    procedure WMGraphNotify(var Message: TMessage); message WM_GraphNotify;
    procedure WMStopPlayer(var message: TMessage); message WM_StopPlayer;

    procedure PlayerDebug;
    procedure DoErrorInfo(const AInfo: string); overload;
    procedure DoErrorInfo(const AInfo: string; const Args: array of const); overload;

    function  GetMixerBitmapInterface(var AMixerBmp: IVMRMixerBitmap): Boolean; overload;
    function  GetMixerBitmapInterface(var AMixerBmp: IVMRMixerBitmap9): Boolean; overload;
    procedure VMRToMixerBmpInfo(const AVMR: TVMRAlphaBitmap; AMixerBmpInfo: TVideoMixerBitmapInfo); overload;
    procedure VMRToMixerBmpInfo(const AVMR9: TVMR9AlphaBitmap; AMixerBmpInfo: TVideoMixerBitmapInfo); overload;

    {Timer 事件}    
    procedure DoTimeToDisableMixerBitmap(Sender: TObject);

    {全屏播放相关}
    procedure CreateFullSrceen;
    procedure ReleaseFullScreen;
    procedure DoFullSreenPlayCtrlTimer(Sender: TObject);
    procedure DoFullSrceenFormDbClick(Sender: TObject);
    procedure DoFullSrceenFormKeyPress(Sender: TObject; var Key: Char);
    procedure DoFullSrceenFormFormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoFullSrceenFormPaint(Sender: TObject);
    procedure DoFullSrceenFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoFreeFullScreenForm(Sender: TObject);
    procedure DoFullSrceenFormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure BeginShowPlayCtrlSetting;
    function  AnimalShowFullPlayCtrl: Boolean;
    function  AnimalHideFullPlayCtrl: Boolean;
  private
    FVideoMode: TVideoMode;
    FPlayState: TPlayState;
    FGraphEdit: Boolean;
    FIsContainVideo: Boolean;
    FFullScreen: Boolean;
    FAudioChannel: TAudioChannelStyle;
    FCurShowWinHandle: HWND;
    FVolume: Integer;
    FLinearVolume: Boolean;
    FMute: Boolean;
    FDuration: Cardinal;
    FIsCanSeek: Boolean;
    FCloseByPlayer: Boolean;
    FEnableMixerBmp: Boolean;
    FOnPlayerPlay: TNotifyEvent;
    FOnPlayerPause: TNotifyEvent;
    FOnPlayerFullSrceenChanged: TNotifyEvent;
    FOnPlayerOpen: TNotifyEvent;
    FOnPlayerClose: TNotifyEvent;
    FOnPlayerStop: TNotifyEvent;
    FDragAcceptFile: Boolean;
    FOnDragFiles: TOnDragFiles;
    FOnPaint: TOnPaint;
    FEnableFullScreen: Boolean;
    FFileName: string;
    FVideoSrcHeight: Integer;
    FVideoSrcWidth: Integer;
    FFullPlayCtrlParent: TWinControl;
    FHintPlayInfoPos: TPoint;
    FAutoHintPlayInfo: Boolean;
    FHintPlayTime: Cardinal;

    procedure SetVideoMode(const Value: TVideoMode);
    procedure SetGraphEdit(const Value: Boolean);
    procedure SetFullScreen(const Value: Boolean);
    procedure SetAudioChannel(const Value: TAudioChannelStyle);
    procedure SetShowVideoByWinHandle(const Value: HWND);
    procedure SetVolume(const Value: Integer);
    procedure SetMute(const Value: Boolean);
    function  GetPosition: Cardinal;
    procedure SetPosition(const Value: Cardinal);
    procedure SetEnableMixerBmp(const Value: Boolean);
    procedure SetDragAcceptFile(const Value: Boolean);
    procedure SetEnableFullScreen(const Value: Boolean);
    procedure SetFullPlayCtrlParent(const Value: TWinControl);
    procedure SetHintPlayTime(const Value: Cardinal);
  published
    {父类属性}
    property Font;
    
    property CloseByPlayer: Boolean read FCloseByPlayer; //是否是播放到最后，播放器自己停止
    property PlayState: TPlayState read FPlayState; //当前播放状态
    property Duration: Cardinal read FDuration;
    property IsContainVideo: Boolean read FIsContainVideo; //是否包含视频流
    property IsCanSeek: Boolean read FIsCanSeek;
    property FileName: string read FFileName;

    {绘制}
    property AutoHintPlayInfo: Boolean read FAutoHintPlayInfo write FAutoHintPlayInfo; //自动提示播放信息
    property HintPlayInfoPos: TPoint read FHintPlayInfoPos write FHintPlayInfoPos; //播放信息位置
    property HintPlayTime: Cardinal read FHintPlayTime write SetHintPlayTime; //提示时长
    {动作}
    property FullPlayCtrlParent: TWinControl read FFullPlayCtrlParent write SetFullPlayCtrlParent;
    property DragAcceptFile: Boolean read FDragAcceptFile write SetDragAcceptFile;
    property GraphEdit: Boolean read FGraphEdit write SetGraphEdit default False; //远程查看连表
    property Position: Cardinal read GetPosition write SetPosition;
    {视频属性}
    property VideoMode: TVideoMode read FVideoMode write SetVideoMode default vmDefault; //视频渲染模式
    property VideoSrcWidth: Integer read FVideoSrcWidth;
    property VideoSrcHeight: Integer read FVideoSrcHeight;
    property EnableVideoMixerBitmap: Boolean read FEnableMixerBmp write SetEnableMixerBmp; //是否使用图片来混合视频
    property FullScreen: Boolean read FFullScreen write SetFullScreen default False; //全屏模式
    property EnableFullScreen: Boolean read FEnableFullScreen write SetEnableFullScreen;
    property ShowVideoByWinHandle: HWND read FCurShowWinHandle write SetShowVideoByWinHandle; //视频显示在指定窗口中
    {音频属性}
    property AudioChannel: TAudioChannelStyle read FAudioChannel write SetAudioChannel; //音频声道控制
    property Volume: Integer read FVolume write SetVolume default 100; //音量控制
    property LinearVolume: Boolean read FLinearVolume write FLinearVolume default True;
    property Mute: Boolean read FMute write SetMute; //静音
    {事件}
    property OnPaint: TOnPaint read FOnPaint write FOnPaint;
    property OnPlayerOpen: TNotifyEvent read FOnPlayerOpen write FOnPlayerOpen; //打开播放器
    property OnPlayerPlay: TNotifyEvent read FOnPlayerPlay write FOnPlayerPlay; //调用play成功时调用
    property OnPlayerPause: TNotifyEvent read FOnPlayerPause write FOnPlayerPause; //调用pause成功时调用
    property OnPlayerStop: TNotifyEvent read FOnPlayerStop write FOnPlayerStop; //调用stop成功时调用
    property OnPlayerClose: TNotifyEvent read FOnPlayerClose write FOnPlayerClose; //关闭播放器
    property OnPlayerFullSrceenChanged: TNotifyEvent read FOnPlayerFullSrceenChanged write FOnPlayerFullSrceenChanged;
    property OnDragFiles: TOnDragFiles read FOnDragFiles write FOnDragFiles;

    property OnMouseUp;
  end;
{$M-}

implementation

//需要第三方支持的GUID
type
  TFilterInfo = record
    Style: TMusicStyle;
    SplitterCLSID: string;
    SplitterFileName: string;
  end;

const
  CtDisplaySpeed = 80;
  CtAryFilters: array[0..5] of TFilterInfo =
  (
    (Style: msWMV; SplitterCLSID: '{1932C124-77DA-4151-99AA-234FEA09F463}'; SplitterFileName: 'asfsplliter.ax'),
    (Style: msMPEG; SplitterCLSID: '{DC257063-045F-4BE2-BD5B-E12279C464F0}'; SplitterFileName: 'MpegSplitter.ax'),
    (Style: msRMVB; SplitterCLSID: '{E21BE468-5C18-43EB-B0CC-DB93A847D769}'; SplitterFileName: 'RealMediaSplitter.ax'),
    (Style: msFLV; SplitterCLSID: '{47E792CF-0BBE-4F7A-859C-194B0768650A}'; SplitterFileName: 'FLVSplitter.ax'),
    (Style: msMP4; SplitterCLSID: '{564FD788-86C9-4444-971E-CC4A243DA150}'; SplitterFileName: 'HaaliSplitter.ax'),
    (Style: msHelp_ffdShow; SplitterCLSID: '{0B0EFF97-C750-462C-9488-B10E7D87F1A6}'; SplitterFileName: 'ffdshow.ax')
    );

{$IFDEF DEBUG}
var
  _Index: Integer = 0;
{$ENDIF}

{ TxdPlayer }

function  GetFileMusicStyle(const AFileName: string): TMusicStyle;
var
  strExtFile: string;
  mStyle: TMusicStyle;
  function CheckFileStyle(const AStyle: TMusicStyle; const AExts: array of string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := Low(AExts) to High(AExts) do
    begin
      if Pos(AExts[i], strExtFile) > 0 then
      begin
        Result := True;
        mStyle := AStyle;
        Break;
      end;
    end;
  end;
begin
  //此函数决定在线播放的能力
  mStyle := msNULL;
  strExtFile := LowerCase( ExtractFileName(AFileName) );

  if not CheckFileStyle(msWMV, ['.wmv']) then
    if not CheckFileStyle(msMPEG, ['.mpg', '.mpeg']) then
      if not CheckFileStyle(msRMVB, ['.rm', '.rmvb']) then
        if not CheckFileStyle(msFLV, ['.flv', '.f4v']) then
          if not CheckFileStyle(msMP4, ['.mp4']) then ;

  Result := mStyle;
end;

procedure Dbg(const AInfo: string); overload;
begin
  {$IFDEF Debug}
  _Log( IntToStr(_Index) + '  ' + AInfo, 'JxdPlayer_Debug.txt' );
  InterlockedIncrement( _Index );
  {$ENDIF}
  OutputDebugString( PChar(AInfo) );
end;

procedure Dbg(const AInfo: string; const Args: array of const); overload;
begin
  Dbg( Format(AInfo, Args) );
end;

function TxdPlayer.AnimalHideFullPlayCtrl: Boolean;
var
  nTemp: Integer;
begin
  nTemp := 5;
  Result := False;
  if FFullPlayCtrlParent.Top > FFullSrceenForm.Height then
  begin
    FFullPlayCtrlParent.Visible := False;
    Result := True;
  end
  else
    FFullPlayCtrlParent.Top := FFullPlayCtrlParent.Top + nTemp;
end;

function TxdPlayer.AnimalShowFullPlayCtrl: Boolean;
var
  nTemp: Integer;
begin
  nTemp := 5;
  Result := False;
  if FFullPlayCtrlParent.Top + FFullPlayCtrlParent.Height - nTemp < FFullSrceenForm.Height then
  begin
    nTemp := FFullPlayCtrlParent.Top + FFullPlayCtrlParent.Height - FFullSrceenForm.Height;
    Result := True;
  end;
  FFullPlayCtrlParent.Top := FFullPlayCtrlParent.Top - nTemp;
end;

procedure TxdPlayer.BeginShowPlayCtrlSetting;
var
  nLeft: Integer;
begin
  if not Assigned(FFullSrceenForm) then
    Exit;
  try
    if FFullPlayCtrlParent.Parent <> FFullSrceenForm then
      FFullPlayCtrlParent.Parent := FFullSrceenForm;
    nLeft := (FFullSrceenForm.Width - FFullPlayCtrlParent.Width) div 2;
    if FFullPlayCtrlParent.Left <> nLeft then
      FFullPlayCtrlParent.Left := nLeft;
    FFullPlayCtrlParent.Top := FFullSrceenForm.Height;
    FFullPlayCtrlParent.Visible := True;
    FFullPlayCtrlParent.BringToFront;
  except

  end;
end;

function TxdPlayer.BuildSplitterFilterToGraph(const guid: TGUID): Boolean;
begin
  Result := CreateFilter(guid, True, 'xdSplitter(Net): ' + GUIDToString(guid), FSpliter);
  if not Result then  
  begin
    DoErrorInfo('无法创建自定义Filter或找不到指定Filter(%s)', [GUIDToString(guid)]);
    Exit;
  end;
  
  Result := Succeeded( ConnectFilters(FGraph, FAsyncSourceFilter as IBaseFilter, FSpliter) );
  if not Result then
  begin
    DoErrorInfo('无法使用指定的Filter作为当前文件的分离器; GUID= %s', [GUIDToString(guid)]);
    FGraph.RemoveFilter(FSpliter);
    FSpliter := nil;
    Exit;
  end;
  
  if not RenderOutPin(FSpliter) then
  begin
    Result := False;
    FGraph.RemoveFilter(FSpliter);
    FSpliter := nil;
    DoErrorInfo('无法连接指定Fitler, 文件名：%s, 需要连接的Filter: %s', [FFileStream.FileName, GUIDToString(guid)]);
  end;

  if Result then
  begin
    Dbg( '使用指定的Filter作为当前的分离器; GUID= %s', [GUIDToString(guid)] );
  end;
end;

procedure TxdPlayer.ChangedVideoPosition(const AShowWinHandle: HWND; const ApRect: PRect);
var
  R: TRect;
  dc: HDC;
  bSuccess: Boolean;
begin
  if ApRect = nil then
    Windows.GetClientRect(AShowWinHandle, R)
  else
    R := ApRect^;
  bSuccess := False;
  case VideoMode of
    vmDefault:
      begin
        if Assigned(FVMRWinlessCtrl) then
        begin
          with FVMRWinlessCtrl do
          begin
            SetVideoClippingWindow(AShowWinHandle);
            SetVideoPosition(nil, @R);
            if PlayState = psPause then
            begin
              dc := GetDC(AShowWinHandle);
              RepaintVideo(AShowWinHandle, dc);
              ReleaseDC(AShowWinHandle, dc);
            end;
          end;
          bSuccess := True;
        end;
      end;
    vmVMR9:
      begin
        if Assigned(FVMR9WinlessCtrl) then
        begin
          with FVMR9WinlessCtrl do
          begin
            SetVideoClippingWindow(AShowWinHandle);
            SetVideoPosition(nil, @R);
            if PlayState = psPause then
            begin
              dc := GetDC(AShowWinHandle);
              RepaintVideo(AShowWinHandle, dc);
              ReleaseDC(AShowWinHandle, dc);
            end;
          end;
          bSuccess := True;
        end;
      end;
  end;
  if bSuccess then
    FCurShowWinHandle := AShowWinHandle
  else
    FCurShowWinHandle := Handle;
end;

procedure TxdPlayer.CloseDS;
begin
  FFileName := '';
  FPlayState := psStop;

  FreeAndNil(FMixerBitmapTimer);
  if FGraphEditID <> 0 then
  begin
    RemoveGraphFromRot(FGraphEditID);
    FGraphEditID := 0;
  end;
  if Assigned(FEvent) then
    FEvent.SetNotifyWindow(0, 0, 0);

  if Assigned(FAsyncSourceFilter) then
  begin
    FAsyncSourceFilter.IsCurConnecttingFilter := False;
    FAsyncSourceFilter.FilterStop := True; //防止一直等待
  end;  

  if Assigned(FMediaControl) then
    FMediaControl.Stop;

  if Assigned(FRenderVideo) then
    FGraph.RemoveFilter(FRenderVideo);

  FDuration := 0;
  FIsCanSeek := False;

  FMediaControl := nil;
  FEvent := nil;
  FSeeking := nil;
  FRenderAudio := nil;
  FSpliter := nil;
  FAudioChannelFilter := nil;  //直接nil就可以，不需要去free
  FVMR9WinlessCtrl := nil;
  FVMRWinlessCtrl := nil;
  FAsyncSourceFilter := nil;
  if ((VideoMode = vmVMR9) and IsContainVideo) or (VideoMode = vmDefault) then
    FRenderVideo := nil;
  FGraph := nil;

  if Assigned(FFileStream) then
  begin
    StreamManage.ReleaseFileStream(FFileStream);
    FFileStream := nil;
  end;
  Windows.InvalidateRect(FCurShowWinHandle, nil, True);
  if Assigned(OnPlayerClose) then
    OnPlayerClose(Self);
end;

procedure TxdPlayer.ConfigDefaultVMR;
var
  cfg: IVMRFilterConfig;
  bWndLess: Boolean;
begin  
  bWndLess := False;
  if Succeeded(FRenderVideo.QueryInterface(IID_IVMRFilterConfig, cfg)) then
  begin
    try
      if EnableVideoMixerBitmap then
        cfg.SetNumberOfStreams(1); //可混合图像流
      bWndLess := Succeeded(cfg.SetRenderingMode(VMRMode_Windowless));
    finally
      cfg := nil;
    end;
  end;
  if bWndLess and Succeeded(FRenderVideo.QueryInterface(IID_IVMRWindowlessControl, FVMRWinlessCtrl)) then
  begin
    FVMRWinlessCtrl.SetBorderColor(0);
//    FVMRWinlessCtrl.SetColorKey( 0 );
    if FFullScreen then
    begin
      CreateFullSrceen;
      ChangedVideoPosition(FFullSrceenForm.Handle);
    end
    else
      ChangedVideoPosition(Handle);
  end;
end;

function TxdPlayer.ConfigPlayerAfterLoadFile: Boolean;
var
  pin: IPin;
  dwCapabilities: Cardinal;
  t: Int64;
  nTemp: Integer;
//  b: IBaseFilter;
begin
  pin := nil;
  FVideoSrcWidth := -1;
  FVideoSrcHeight := -1;
  FIsContainVideo := Assigned(FRenderVideo) and Succeeded(GetConnectedPin(FRenderVideo, PINDIR_INPUT, pin, 0));
  if not FIsContainVideo then
  begin
    pin := nil;
    ReleaseVideoFilter;
    FCurShowWinHandle := Handle;
  end
  else
  begin
    case FVideoMode of
      vmDefault:
        begin
          if Assigned(FVMRWinlessCtrl) then
            FVMRWinlessCtrl.GetNativeVideoSize(FVideoSrcWidth, FVideoSrcHeight, nTemp, nTemp);
        end;
      vmVMR9:
        begin
          if Assigned(FVMR9WinlessCtrl) then
            FVMR9WinlessCtrl.GetNativeVideoSize(FVideoSrcWidth, FVideoSrcHeight, nTemp, nTemp);
        end;
    end;
  end;
  SetAudioChannel(FAudioChannel);
  SetVolume(FVolume);
  if Assigned(FSeeking) then
  begin
    FSeeking.GetCapabilities(dwCapabilities);
    FIsCanSeek := dwCapabilities and AM_SEEKING_CanSeekAbsolute <> 0;
    FSeeking.GetDuration(t);
    FDuration := t div 10000;
  end
  else
  begin
    FIsCanSeek := False;
    FDuration := 0;
  end;
//  FindAudioRenderer( FGraph, b );
//  if Assigned(b) then
//  begin
//    OutputDebugString( PChar( GetFilterName( b ) ));
//    FindNextFilter( b, PINDIR_INPUT, b );
//    OutputDebugString( PChar( GetFilterName( b ) ));
//  end;
  Result := True;
  PlayerDebug;
end;

procedure TxdPlayer.ConfigVMR9;
var
  cfg: IVMRFilterConfig9;
begin
  if Succeeded(FRenderVideo.QueryInterface(IID_IVMRFilterConfig9, cfg)) then
  begin
    try
      if EnableVideoMixerBitmap then
        cfg.SetNumberOfStreams(2) //混合图片时需要设定为2 否则为1
      else
        cfg.SetNumberOfStreams(1);
      cfg.SetRenderingMode(VMR9Mode_Windowless);
    finally
      cfg := nil;
    end;
  end;
  if Succeeded(FRenderVideo.QueryInterface(IID_IVMRWindowlessControl9, FVMR9WinlessCtrl)) then
  begin
    FVMR9WinlessCtrl.SetBorderColor(0);    
//    FVMR9WinlessCtrl.SetColorKey( 0 );

    if FFullScreen then
    begin
      CreateFullSrceen;
      ChangedVideoPosition(FFullSrceenForm.Handle);
    end
    else
      ChangedVideoPosition(Handle);
  end;
end;

constructor TxdPlayer.Create(AOwner: TComponent);
begin
  inherited;
  Canvas.Brush.Color := clBlack;
  Width := 320;
  Height := 240;
  DoubleBuffered := True;

  FVideoMode := vmDefault;
  FPlayState := psStop;
  FGraphEdit := False;
  FGraphEditID := 0;
  FFullScreen := False;
  FEnableFullScreen := True;
  FFileStream := nil;
  FFullSrceenForm := nil;
  FCurShowWinHandle := 0;
  FEnableMixerBmp := False;
  FMixerBitmapTimer := nil;

  FGraph := nil;
  FMediaControl := nil;
  FEvent := nil;
  FRenderVideo := nil;
  FRenderAudio := nil;
  FSeeking := nil;
  FSpliter := nil;
  FVMR9WinlessCtrl := nil;

  FDragAcceptFile := False;
  FIsCanSeek := False;
  FDuration := 0;
  FVolume := 100;
  FLinearVolume := True;
  FFileName := '';

  BevelInner := bvNone;
  BevelOuter := bvNone;
  BevelKind := bkNone;
  Ctl3D := False;
  BorderWidth := 0;
  Color := 0;

  FFullPlayCtrlParent := nil;
  FFullSrccenShowCtrlTimer := nil;
  FFullMouseStopPoint := Point(-1, -1);

  FAutoHintPlayInfo := True;
  FHintPlayInfoPos := Point( 10, 10 );
  FHintPlayTime := 3 * 1000;
end;

function TxdPlayer.CreateFilter(const guid: TGUID; const AIsAddToGraph: Boolean; const AFilterName: WideString; var AFilter: IBaseFilter): Boolean;
begin
  Result := Succeeded(CoCreateInstance(guid, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, AFilter));
  if AIsAddToGraph and Result and (FGraph <> nil) then
  begin
    Result := Succeeded(FGraph.AddFilter(AFilter, PWideChar(AFilterName)));
    if not Result then
      AFilter := nil;
  end;
end;

procedure TxdPlayer.CreateFullSrceen;
begin
  if not Assigned(FFullSrceenForm) then
  begin
    FFullSrceenForm := TForm.Create(nil);
    with FFullSrceenForm do
    begin
      BorderIcons := [];
      BorderStyle := bsNone;
      Caption := '';
      Color := 0;
      FormStyle := fsStayOnTop;
      OnDblClick := DoFullSrceenFormDbClick;
      OnKeyPress := DoFullSrceenFormKeyPress;
      OnClose := DoFullSrceenFormClose;
      OnMouseMove := DoFullSrceenFormMouseMove;
      OnMouseUp := DoFullSrceenFormFormMouseUp;
      OnPaint := DoFullSrceenFormPaint;
    end;
  end;
  FFullSrceenForm.Visible := True;
  FFullSrceenForm.Show;
  FFullSrceenForm.WindowState := wsMaximized;
end;

procedure TxdPlayer.DblClick;
begin
  inherited;
  FullScreen := not FullScreen;
end;

destructor TxdPlayer.Destroy;
begin
  Stop;
  if Assigned(FFullSrceenForm) then
    FreeAndNil(FFullSrceenForm);
  inherited;
end;

procedure TxdPlayer.DoErrorInfo(const AInfo: string);
begin
  Dbg(AInfo);
end;

procedure TxdPlayer.DoErrorInfo(const AInfo: string; const Args: array of const);
begin
  DoErrorInfo(Format(AInfo, Args));
end;

procedure TxdPlayer.DoFreeFullScreenForm(Sender: TObject);
begin
  if Sender is TTimer then
    (Sender as TTimer).Free;
  if Assigned(FFullSrceenForm) and not FullScreen then
  begin
    FreeAndNil(FFullSrccenShowCtrlTimer);
    if Assigned(FFullPlayCtrlParent) then
    begin
      FFullPlayCtrlParent.Parent := nil;
      FFullPlayCtrlParent.Visible := False;
    end;
    FreeAndNil(FFullSrceenForm);
    FFullMouseStopPoint := Point(-1, -1);
  end;
end;

procedure TxdPlayer.DoFullSrceenFormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FullScreen then
    FullScreen := False;
end;

procedure TxdPlayer.DoFullSrceenFormDbClick(Sender: TObject);
begin
  FullScreen := not FullScreen;
end;

procedure TxdPlayer.DoFullSrceenFormFormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseUp) then
    OnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TxdPlayer.DoFullSrceenFormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) and FullScreen then
    FullScreen := False;
end;

procedure TxdPlayer.DoFullSrceenFormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(FFullPlayCtrlParent) then
    Exit;

  if not FFullPlayCtrlParent.Visible then
  begin
    //显示
    if (FFullMouseStopPoint.X <> X) and (FFullMouseStopPoint.Y <> Y) and not Assigned(FFullSrccenShowCtrlTimer) then
    begin
      FFullSrccenShowCtrlTimer := TTimer.Create(FFullSrceenForm);
      FFullSrccenShowCtrlTimer.Tag := 1;
      FFullSrccenShowCtrlTimer.OnTimer := DoFullSreenPlayCtrlTimer;
      FFullSrccenShowCtrlTimer.Interval := 10;
      BeginShowPlayCtrlSetting;
      FFullSrccenShowCtrlTimer.Enabled := True;
    end;
  end
  else
  begin
    if not Assigned(FFullSrccenShowCtrlTimer) then
    begin
      FFullMouseStopPoint := Point(X, Y);
      FFullSrccenShowCtrlTimer := TTimer.Create(FFullSrceenForm);
      FFullSrccenShowCtrlTimer.Tag := 0;
      FFullSrccenShowCtrlTimer.OnTimer := DoFullSreenPlayCtrlTimer;
      FFullSrccenShowCtrlTimer.Interval := 1000;
      FFullSrccenShowCtrlTimer.Enabled := True;
    end;
  end;
end;

procedure TxdPlayer.DoFullSrceenFormPaint(Sender: TObject);
var
  dc: HDC;
  PS: TPaintStruct;
begin
  DC := BeginPaint(FFullSrceenForm.Handle, PS);
  try
    if not (FPlayState in [psPlay, psPause]) or not IsContainVideo then
    begin
      if Assigned(OnPaint) then
        OnPaint(Self, dc)
      else
        FillRect(dc, Rect(0, 0, Width, Height), 0);
    end
    else
    begin
      if (FVideoMode = vmVMR9) and Assigned(FVMR9WinlessCtrl) then
        FVMR9WinlessCtrl.RepaintVideo(FFullSrceenForm.Handle, dc)
      else if (FVideoMode = vmDefault) and Assigned(FVMRWinlessCtrl) then
        FVMRWinlessCtrl.RepaintVideo(FFullSrceenForm.Handle, dc);
    end;
  finally
    EndPaint(FFullSrceenForm.Handle, PS);
  end;
end;

procedure TxdPlayer.DoFullSreenPlayCtrlTimer(Sender: TObject);
var
  pt: TPoint;
  R: TRect;
begin
  if not FullScreen then
  begin
    FreeAndNil(FFullSrccenShowCtrlTimer);
    FFullPlayCtrlParent.Visible := False;
    Exit;
  end;

  if FFullSrccenShowCtrlTimer.Tag = 1 then
  begin
    ShowCursor(True);
    if AnimalShowFullPlayCtrl then
    begin
      FreeAndNil(FFullSrccenShowCtrlTimer);
      GetCursorPos(FFullMouseStopPoint);
    end;
  end
  else
  begin
    GetCursorPos(pt);
    R := Rect(FFullPlayCtrlParent.Left, FFullPlayCtrlParent.Top,
      FFullPlayCtrlParent.Left + FFullPlayCtrlParent.Width, FFullPlayCtrlParent.Top + FFullPlayCtrlParent.Height);
    if PtInRect(R, pt) then
      Exit;
    if (pt.X = FFullMouseStopPoint.X) and (pt.Y = FFullMouseStopPoint.Y) then
    begin
      ShowCursor(False);
      FFullSrccenShowCtrlTimer.Interval := 10;
      if AnimalHideFullPlayCtrl then
        FreeAndNil(FFullSrccenShowCtrlTimer);
    end
    else
      FFullMouseStopPoint := pt;
  end;
end;

procedure TxdPlayer.DoTimeToDisableMixerBitmap(Sender: TObject);
  procedure DisableDefault;
  var
    iBmp: IVMRMixerBitmap;
    param: TVMRAlphaBitmap;
  begin
    if GetMixerBitmapInterface(iBmp) then
    begin
      if Failed(iBmp.GetAlphaBitmapParameters(param)) then
      begin
        FreeAndNil(FMixerBitmapTimer);
        Exit;
      end;
      if param.fAlpha <= 0 then
      begin
        FreeAndNil(FMixerBitmapTimer);
        MixerDisable(False);
        Exit;
      end;
      param.dwFlags := VMRBITMAP_HDC or VMRBITMAP_SRCCOLORKEY;    
      param.fAlpha := param.fAlpha - 0.1;

      if Failed(iBmp.UpdateAlphaBitmapParameters(@param)) then
      begin
        FreeAndNil(FMixerBitmapTimer);
        MixerDisable(False);
      end;
    end;
  end;

  procedure DisableVMR9;
  var
    iBmp: IVMRMixerBitmap9;
    param: TVMR9AlphaBitmap;
  begin
    if GetMixerBitmapInterface(iBmp) then
    begin
      if Failed(iBmp.GetAlphaBitmapParameters(param)) then
      begin
        FreeAndNil(FMixerBitmapTimer);
        Exit;
      end;
      if param.fAlpha <= 0 then
      begin
        FreeAndNil(FMixerBitmapTimer);
        MixerDisable(False);
        Exit;
      end;
      param.dwFlags := VMRBITMAP_HDC or VMRBITMAP_SRCCOLORKEY;
      param.fAlpha := param.fAlpha - 0.1;

      if Failed(iBmp.UpdateAlphaBitmapParameters(@param)) then
      begin
        FreeAndNil(FMixerBitmapTimer);
        MixerDisable(False);
      end;
    end;
  end;
begin  
  if FVideoMode = vmDefault then
    DisableDefault
  else
    DisableVMR9;

  if Assigned(FMixerBitmapTimer) and (FMixerBitmapTimer.Interval <> CtDisplaySpeed) then
    FMixerBitmapTimer.Interval := CtDisplaySpeed;  
end;

function TxdPlayer.GetMixerBitmapInterface(var AMixerBmp: IVMRMixerBitmap): Boolean;
begin
  Result := IsContainVideo and Assigned(FRenderVideo) and
    Succeeded(FRenderVideo.QueryInterface(IID_IVMRMixerBitmap, AMixerBmp));
end;

function TxdPlayer.GetCurrentBitmap(var ABmpStream: TStream): Boolean;
var
  Image: PBitmapInfoHeader;
  BFH: TBITMAPFILEHEADER;

  function DibSize: cardinal;
  begin
    result := (Image.biSize + Image.biSizeImage + Image.biClrUsed * sizeof(TRGBQUAD));
  end;
  function DibNumColors: cardinal;
  begin
    if (image.biClrUsed = 0) and (image.biBitCount <= 8) then
      result := 1 shl integer(image.biBitCount)
    else
      result := image.biClrUsed;
  end;
  function DibPaletteSize: cardinal;
  begin
    result := (DibNumColors * sizeof(TRGBQUAD))
  end;

begin
  if FVideoMode = vmDefault then
    Result := Assigned(FVMRWinlessCtrl) and Succeeded(FVMRWinlessCtrl.GetCurrentImage(PByte(Image)))
  else
    Result := Assigned(FVMR9WinlessCtrl) and Succeeded(FVMR9WinlessCtrl.GetCurrentImage(PByte(Image)));

  if Result then
  begin
    if ABmpStream = nil then
      ABmpStream := TMemoryStream.Create;

    BFH.bfType := $4D42; // BM
    BFH.bfSize := DibSize + sizeof(TBITMAPFILEHEADER);
    BFH.bfReserved1 := 0;
    BFH.bfReserved2 := 0;
    BFH.bfOffBits := sizeof(TBITMAPFILEHEADER) + image.biSize + DibPaletteSize;
    ABmpStream.Write(BFH, SizeOf(TBITMAPFILEHEADER));
    ABmpStream.Write(image^, BFH.bfSize);
    ABmpStream.Position := 0;
    CoTaskMemFree(image);
  end;
end;

function TxdPlayer.GetMixerBitmapInterface(var AMixerBmp: IVMRMixerBitmap9): Boolean;
begin
  Result := IsContainVideo and Assigned(FRenderVideo) and
    Succeeded(FRenderVideo.QueryInterface(IID_IVMRMixerBitmap9, AMixerBmp));
end;

function TxdPlayer.GetPosition: Cardinal;
var
  nPos: Int64;
begin
  Result := 0;
  if (PlayState = psStop) or not Assigned(FSeeking) then
    Exit;
  Result := FSeeking.GetCurrentPosition(nPos);
  if Result <> S_OK then
  begin
    Result := 0;
    Exit;
  end;
  Result := nPos div 10000;
end;

function TxdPlayer.GetStreamSplitter(const AFileName: string): TGUID;
var
  mStyle: TMusicStyle;
  i: Integer;
begin
  //此函数决定在线播放的能力
  mStyle := GetFileMusicStyle(AFileName);
  if mStyle <> msNULL then
  begin
    for i := Low(CtAryFilters) to High(CtAryFilters) do
    begin
      if CtAryFilters[i].Style = mStyle then
      begin
        Result := StringToGUID(CtAryFilters[i].SplitterCLSID);
        Exit;
      end;
    end;
  end;
  Result := GUID_NULL;
end;

function TxdPlayer.OpenDS: Boolean;
var
  hr: HRESULT;

  //----------创建一般的视频渲染器
  function ConfigDefaultVideoRenderer: Boolean;
  begin
    Result := False;
    if not CreateFilter(CLSID_VideoRendererDefault, True, 'xd Default Video Renderer', FRenderVideo) then
    begin
      DoErrorInfo('无法创建默认的视频渲染器: %s', [GUIDToString(CLSID_VideoRendererDefault)]);
      Exit;
    end;
    Result := True;
  end;

begin
  FCloseByPlayer := False;
  Result := False;
  if not Succeeded(CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IGraphBuilder, FGraph)) then
  begin
    DoErrorInfo('无法创建 IGraphBuilder，请先安装DirectX');
    Exit;
  end;
  //创建渲染器
  if FVideoMode = vmDefault then
  begin
    if not ConfigDefaultVideoRenderer then
      Exit;
    //配置默认渲染器
    ConfigDefaultVMR;
  end
  else
  begin
    if Assigned(FRenderVideo) then
      FGraph.AddFilter(FRenderVideo, 'xd Video Mixing Renderer9')
    else if not CreateFilter(CLSID_VideoMixingRenderer9, True, 'xd Video Mixing Renderer9', FRenderVideo) then
    begin
      FVideoMode := vmDefault;
      DoErrorInfo('无法创建视频宣染器9: %s.将尝试使用系统默认的视频宣染器', [GUIDToString(CLSID_VideoMixingRenderer9)]);
      if not ConfigDefaultVideoRenderer then
        Exit
      else
        ConfigDefaultVMR;
    end;

    //配置Video Mixing Renderer9 渲染器属性
    if Assigned(FRenderVideo) then
      ConfigVMR9;
  end;

  hr := NOERROR;
  try
    hr := hr or FGraph.QueryInterface(IID_IMediaControl, FMediaControl); //媒体控制
    hr := hr or FGraph.QueryInterface(IID_IMediaEventEx, FEvent); //事件
    hr := hr or FGraph.QueryInterface(IID_IMediaSeeking, FSeeking); //拖动
    hr := hr or FGraph.QueryInterface(IID_IBasicAudio, FRenderAudio); //音频处理
    if FSeeking <> nil then
      FSeeking.SetTimeFormat(TIME_FORMAT_MEDIA_TIME);
    if not Succeeded(hr) then
      Dbg('无法查询基本DirectShow接口, 在播放时可能有些操作无法正常进行');
    FEvent.SetNotifyFlags(0);
    FEvent.SetNotifyWindow(Handle, WM_GRAPHNOTIFY, 0);

    FAudioChannelFilter := TxdAudioChannelFilter.Create;
    FGraph.AddFilter( FAudioChannelFilter as IBaseFilter, '音频过滤器' );

    Result := True;
  except
    Result := False;
  end;  
end;

function TxdPlayer.OpenFile(const AFileName: string): Boolean;
var
  h: HRESULT;
  guid: TGUID;
  bCreateFilter: Boolean;
  pin: IPin;
begin
  Result := False;
  if FPlayState = psCancel then
    Exit;
  //停止工作
  if FPlayState <> psStop then
    Stop;
  //重新创建
  FPlayState := psOpening;
  if not OpenDS then
  begin
    FCloseByPlayer := True;
    CloseDS;
    Exit;
  end;

  FFileName := AFileName;

  //先准备已经配置好的链路
  guid := GetStreamSplitter(AFileName);
  bCreateFilter := not IsEqualGUID(guid, GUID_NULL);
  if bCreateFilter then
  begin
    if not CreateFilter(guid, True, 'xdSplitter: ' + GUIDToString(guid), FSpliter) then
    begin
      bCreateFilter := False;
      DoErrorInfo('无法创建自定义Filter或找不到指定Filter(%s)，将尝试使用系统智能连接', [GUIDToString(guid)]);
    end;
  end
  else
  begin
    OutputDebugString( '无法找到指定分离器' );
  end;

  h := FGraph.RenderFile(StringToOleStr(AFileName), nil);
  Result := Succeeded(h);
  if bCreateFilter and Succeeded(GetUnConnectedPin(FSpliter, PINDIR_INPUT, pin, 0)) then
  begin
    FGraph.RemoveFilter(FSpliter);
    FSpliter := nil;
  end;
  if not Result then
  begin
    OutputDebugString(PChar(GetAMErrorText(h)));
    CloseDS;
    Exit;
  end;
  //配置相关属性
  Result := OpenPlayered(ConfigPlayerAfterLoadFile);
end;

function TxdPlayer.OpenFileStream(const AFileStreamID: Cardinal; const AMaxWaitTime: Cardinal): Boolean;
var
  guid, g2: TGUID;
  bAutoConnect: Boolean;
  filter: IBaseFilter;
  i: Integer;
label
  llAutoConnect;
begin
  Result := False;
  //停止工作
  if FPlayState = psCancel then
    Exit;

  if FPlayState <> psStop then
    Stop;

  FPlayState := psOpening;
  FFileStream := StreamManage.QueryFileStream(AFileStreamID);
  if FFileStream = nil then
  begin
    FPlayState := psStop;
    DoErrorInfo('找不到指定流');
    Exit;
  end;
  //重新创建
  if not OpenDS then
  begin
    FCloseByPlayer := True;
    CloseDS;
    Exit;
  end;
  if FPlayState <> psOpening then
  begin
    CloseDS;
    Exit;
  end;

  //创建播放所需要条件
  FAsyncSourceFilter := TxdAsyncFilter.Create;
  FAsyncSourceFilter.SetAsyncFileStream(FFileStream);
  FAsyncSourceFilter.Load(nil, nil);

  if Failed(FGraph.AddFilter(FAsyncSourceFilter as IBaseFilter, 'xdSourceFilter')) then
  begin
    DoErrorInfo('无法添加xdSourceFilter');
    CloseDS;
    Exit;
  end;

  FAsyncSourceFilter.IsCurConnecttingFilter := True;
  //连接所需要的Splitter
  FFileName := FFileStream.FileName;
  guid := GetStreamSplitter(FFileStream.FileName);  
  bAutoConnect := IsEqualGUID(guid, GUID_NULL);
  if not bAutoConnect then
  begin
    if not BuildSplitterFilterToGraph(guid) then
    begin
      bAutoConnect := True;
      for i := Low(CtAryFilters) to High(CtAryFilters) do
      begin
        g2 := StringToGUID(CtAryFilters[i].SplitterCLSID);
        if not IsEqualGUID(guid, g2) then
        begin
          if BuildSplitterFilterToGraph(g2) then
          begin
            bAutoConnect := False;
            Break;
          end;
        end;
      end;
    end;
  end;
  
  if bAutoConnect then
  begin
    DoErrorInfo('使用智能连接去尝试在线播放文件：%s', [FFileStream.FileName]);
    filter := FAsyncSourceFilter as IBaseFilter;
    Result := RenderOutPin(filter);
    if not Result then
    begin
      DoErrorInfo('无法使用智能连接去在线播放文件：%s', [FFileStream.FileName]);
      CloseDS;
      Exit;
    end;
  end;

  FAsyncSourceFilter.IsCurConnecttingFilter := False;
  //配置相关属性
  Result := OpenPlayered(ConfigPlayerAfterLoadFile);
end;

function TxdPlayer.OpenPlayered(const AConfigOK: Boolean): Boolean;
begin
  Result := AConfigOK;
  if Result then
  begin
    if FGraphEdit then
      AddGraphToRot(FGraph, FGraphEditID);
    FPlayState := psOpened;
    if Assigned(OnPlayerOpen) then
      OnPlayerOpen(Self);
  end
  else
  begin
    CloseDS;
    DoErrorInfo('无法配置播放器属性');
  end;
end;

function TxdPlayer.Play: Boolean;
begin
  Result := False;
  if FPlayState = psCancel then
    Exit;
  if Assigned(FMediaControl) then
  begin
    if (FPlayState = psOpened) or (FPlayState = psPause) then
    begin
      if Succeeded(FMediaControl.Run) then
      begin
        Result := True;
        FPlayState := psPlay;
        if Assigned(OnPlayerPlay) then
          OnPlayerPlay(Self);
        if AutoHintPlayInfo then        
          MixerString( FHintPlayInfoPos, '播放文件: ' + ExtractFileName(FileName), 1, FHintPlayTime );
      end;
    end;
  end;
end;

procedure TxdPlayer.PlayerDebug;
var
  info: TStringList;
  i: Integer;
begin
  Exit;
  
  info := TStringList.Create;

  EnumFilters(FGraph, info);
  Dbg('记录当前加载Filter信息');
  for i := info.Count - 1 downto 0 do
    Dbg(info[i]);
  Dbg('==========结束============');
  info.Free;
end;

procedure TxdPlayer.ReChangedParent;
begin
  ChangedVideoPosition(Handle);
  if Assigned(FEvent) then
  begin
    FEvent.SetNotifyFlags(0);
    FEvent.SetNotifyWindow(Handle, WM_GRAPHNOTIFY, 0);
  end;
end;

procedure TxdPlayer.ReleaseFullScreen;
begin
  if Assigned(FFullSrceenForm) then
  begin
    ShowCursor(True);
    FFullSrceenForm.Visible := False;
    with TTimer.Create(Self) do
    begin
      OnTimer := DoFreeFullScreenForm;
      Interval := 30000; //30秒之后自动释放
      Enabled := True;
    end;
  end;
end;

procedure TxdPlayer.ReleaseVideoFilter;
begin
  //VMR9 视频渲染器在没有视频源的时候暂时不知如果释放内存，只能一直保留它，等到有视频源之后再释放
  FVMR9WinlessCtrl := nil;
  FVMRWinlessCtrl := nil;
  FGraph.RemoveFilter(FRenderVideo);
  if FVideoMode = vmDefault then
    FRenderVideo := nil;
end;

function TxdPlayer.RenderOutPin(var AFilter: IBaseFilter): Boolean;
var
  pinEnum: IEnumPins;
  pin: IPin;
  fetchCount: Cardinal;
  pinInfo: TPinInfo;
begin
  Result := False;
  if not Succeeded(AFilter.EnumPins(pinEnum)) then
    Exit;
  pinEnum.Reset;

  fetchCount := 0;
  while Succeeded(pinEnum.Next(1, pin, @fetchCount)) and (fetchCount <> 0) do
  begin
    if Succeeded(pin.QueryPinInfo(pinInfo)) then
    begin
      pinInfo.pFilter := nil;
      if pinInfo.dir = PINDIR_OUTPUT then
      begin
        if Succeeded(FGraph.Render(pin)) then
          Result := True;
      end;
    end;
    pin := nil;
  end;
  pinEnum := nil;
end;

procedure TxdPlayer.Resize;
var
  R: TRect;
begin
  inherited;
  if FPlayState <> psStop then
  begin
    R := GetClientRect;
    if (FVideoMode = vmVMR9) and Assigned(FVMR9WinlessCtrl) then
      FVMR9WinlessCtrl.SetVideoPosition(nil, @R)
    else if (FVideoMode = vmDefault) and Assigned(FVMRWinlessCtrl) then
      FVMRWinlessCtrl.SetVideoPosition(nil, @R);
  end;
end;

function TxdPlayer.MixerBitmap(const AMixerInfo: TVideoMixerBitmapInfo): Boolean;
  function SetDefaulMixer: Boolean;
  var
    iBmp: IVMRMixerBitmap;
    param: TVMRAlphaBitmap;
    h: HRESULT;
  begin
    Result := GetMixerBitmapInterface(iBmp);
    if Result then
    begin
      param.dwFlags := VMRBITMAP_HDC or VMRBITMAP_SRCCOLORKEY;
      param.hdc := AMixerInfo.FDC;
      param.pDDS := nil;
      param.rSrc := AMixerInfo.FSrcRect;
      param.rDest.left := AMixerInfo.FDestLeft;
      param.rDest.top := AMixerInfo.FDestTop;
      param.rDest.right := AMixerInfo.FDestRight;
      param.rDest.bottom := AMixerInfo.FDestBottom;
      param.fAlpha := AMixerInfo.FAlphaValue;
      param.clrSrcKey := AMixerInfo.FclrSrcKey;

      h := ibmp.SetAlphaBitmap(param);
      if Failed(h) then
        DoErrorInfo('Error：%s', [GetAMErrorText(h)])
      else
        Result := True;
      iBmp := nil;
    end;
  end;

  function SetVMR9Mixer: Boolean;
  var
    iBmp9: IVMRMixerBitmap9;
    param9: TVMR9AlphaBitmap;
    h: HRESULT;
  begin
    Result := GetMixerBitmapInterface(iBmp9);
    if Result then
    begin
      param9.dwFlags := VMRBITMAP_HDC or VMRBITMAP_SRCCOLORKEY;
      param9.hdc := AMixerInfo.FDC;
      param9.pDDS := nil;
      param9.rSrc := AMixerInfo.FSrcRect;
      param9.rDest.left := AMixerInfo.FDestLeft;
      param9.rDest.top := AMixerInfo.FDestTop;
      param9.rDest.right := AMixerInfo.FDestRight;
      param9.rDest.bottom := AMixerInfo.FDestBottom;
      param9.fAlpha := AMixerInfo.FAlphaValue;
      param9.clrSrcKey := AMixerInfo.FclrSrcKey;

      h := ibmp9.SetAlphaBitmap(@param9);
      if Failed(h) then
        DoErrorInfo('Error：%s', [GetAMErrorText(h)])
      else
        Result := True;
      ibmp9 := nil;
    end;
  end;
begin
  if FVideoMode = vmDefault then
    Result := SetDefaulMixer
  else
    Result := SetVMR9Mixer;
end;

function TxdPlayer.GetMixerBitmapInfo(var AMixerInfo: TVideoMixerBitmapInfo): Boolean;
var
  iBmp: IVMRMixerBitmap;
  param: TVMRAlphaBitmap;
  iBmp9: IVMRMixerBitmap9;
  param9: TVMR9AlphaBitmap;
begin
  if FVideoMode = vmDefault then
  begin
    Result := GetMixerBitmapInterface(iBmp);
    if Result then
    begin
      iBmp.GetAlphaBitmapParameters(param);
      VMRToMixerBmpInfo(param, AMixerInfo);
    end;
  end
  else
  begin
    Result := GetMixerBitmapInterface(iBmp9);
    if Result then
    begin
      iBmp9.GetAlphaBitmapParameters(param9);
      VMRToMixerBmpInfo(param9, AMixerInfo);
    end;
  end;
end;

procedure TxdPlayer.MixerDisable(const AAnimateBmp: Boolean);
  function SetDefaulMixer: Boolean;
  var
    iBmp: IVMRMixerBitmap;
    param: TVMRAlphaBitmap;
    h: HRESULT;
  begin
    Result := GetMixerBitmapInterface(iBmp);
    if Result then
    begin
      ZeroMemory(@param, SizeOf(param));
      param.dwFlags := VMRBITMAP_DISABLE;
      h := ibmp.SetAlphaBitmap(param);
      if Failed(h) then
        DoErrorInfo('Error：%s', [GetAMErrorText(h)])
      else
        Result := True;
      iBmp := nil;
    end;
  end;

  function SetVMR9Mixer: Boolean;
  var
    iBmp9: IVMRMixerBitmap9;
    param9: TVMR9AlphaBitmap;
    h: HRESULT;
  begin
    Result := GetMixerBitmapInterface(iBmp9);
    if Result then
    begin
      ZeroMemory(@param9, SizeOf(param9));
      param9.dwFlags := VMRBITMAP_DISABLE;
      h := ibmp9.SetAlphaBitmap(@param9);
      if Failed(h) then
        DoErrorInfo('Error：%s', [GetAMErrorText(h)])
      else
        Result := True;
      ibmp9 := nil;
    end;
  end;
begin
  if not AAnimateBmp then
  begin
    if FVideoMode = vmDefault then
      SetDefaulMixer
    else
      SetVMR9Mixer;
  end
  else
  begin
    if not Assigned(FMixerBitmapTimer) then
    begin
      FMixerBitmapTimer := TTimer.Create(Self);
      FMixerBitmapTimer.Interval := CtDisplaySpeed;
      FMixerBitmapTimer.OnTimer := DoTimeToDisableMixerBitmap;
      FMixerBitmapTimer.Enabled := True;
    end;
  end;
end;

function TxdPlayer.MixerString(const APos: TPoint; const AMixerString: string; const AlphaValue: Single;
  const AutoDisplayTime: Integer): Boolean;
var
  dc, memDC: HDC;
  nStrW, nStrH: Integer;
  s: TSize;
  memBmp: HBITMAP;
  info: TVideoMixerBitmapInfo;
  clrSrcKey: Cardinal;
begin  
  Result := False;

  if not (FPlayState in [psPlay, psPause]) or 
    (VideoSrcHeight = 0) or (VideoSrcWidth = 0) then Exit;
    
  clrSrcKey := clFuchsia; //被透明的颜色值
  
  dc := GetDC( Handle );
  memDC := CreateCompatibleDC( dc );
  
  SelectObject(memDC, Font.Handle );
  GetTextExtentPoint32( memDC, PChar(AMixerString), Length(AMixerString), s );
  nStrW := s.cx;
  nStrH := s.cy;

  memBmp := CreateCompatibleBitmap( dc, nStrW, nStrH ); 
  SelectObject( memDC, memBmp );
  SetTextColor( memDC, Font.Color );
  SetBkColor( memDC, clrSrcKey );

  TextOut( memDC, 0, 0, PChar(AMixerString), Length(AMixerString) );
  

  info.FDC := memDC;
  info.FSrcRect := Rect(0, 0, nStrW, nStrH);
  //Dest信息按百分比计算
  info.FDestLeft := APos.X / VideoSrcWidth;  
  info.FDestRight := (APos.X + nStrW) / VideoSrcWidth;
  info.FDestTop := APos.Y / VideoSrcHeight;
  info.FDestBottom := (APos.Y + nStrH) / VideoSrcHeight;
  info.FAlphaValue := AlphaValue;
  info.FclrSrcKey := clrSrcKey;
  Result := MixerBitmap( info );

  DeleteObject( memBmp );
  DeleteDC( memDC );
  ReleaseDC( Handle, dc );

  if AutoDisplayTime > 0 then
  begin
    FreeAndNil( FMixerBitmapTimer );
    FMixerBitmapTimer := TTimer.Create( Self );
    FMixerBitmapTimer.Interval := AutoDisplayTime;
    FMixerBitmapTimer.OnTimer := DoTimeToDisableMixerBitmap;
    FMixerBitmapTimer.Enabled := True;
  end;
end;

procedure TxdPlayer.SetAudioChannel(const Value: TAudioChannelStyle);
begin
  FAudioChannel := Value;
  if Assigned(FAudioChannelFilter) then
    FAudioChannelFilter.AudioChannelStyle := FAudioChannel;
end;

procedure TxdPlayer.SetFullPlayCtrlParent(const Value: TWinControl);
begin
  FFullPlayCtrlParent := Value;
  if Assigned(FFullPlayCtrlParent) then
    FFullPlayCtrlParent.Visible := False;
end;

procedure TxdPlayer.SetDragAcceptFile(const Value: Boolean);
begin
  if FDragAcceptFile <> Value then
  begin
    FDragAcceptFile := Value;
    if FDragAcceptFile then
      DragAcceptFiles(Handle, True)
    else
      DragAcceptFiles(Handle, False);
  end;
end;

procedure TxdPlayer.SetEnableFullScreen(const Value: Boolean);
begin
  FEnableFullScreen := Value;
  if not FEnableFullScreen and FullScreen then
    FullScreen := False;
end;

procedure TxdPlayer.SetEnableMixerBmp(const Value: Boolean);
begin
  if (FEnableMixerBmp <> Value) and (FPlayState = psStop) then
    FEnableMixerBmp := Value;
end;

procedure TxdPlayer.SetFullScreen(const Value: Boolean);
var
  h: HWND;
begin
  if Value and not FEnableFullScreen then
    Exit;

  if FFullScreen <> Value then
  begin
    FFullScreen := Value;
    if Value then
    begin
      CreateFullSrceen;
      h := FFullSrceenForm.Handle;
    end
    else
    begin
      ReleaseFullScreen;
      h := Handle;
    end;
    ChangedVideoPosition(h);
//    ShowCursor( not Value );

    if Assigned(OnPlayerFullSrceenChanged) then
      OnPlayerFullSrceenChanged(Self);
  end;
end;

procedure TxdPlayer.SetGraphEdit(const Value: Boolean);
begin
  if FGraphEdit <> Value then
  begin
    FGraphEdit := Value;
    if Assigned(FGraph) then
    begin
      if FGraphEdit then
        AddGraphToRot(FGraph, FGraphEditID)
      else if FGraphEditID <> 0 then
      begin
        RemoveGraphFromRot(FGraphEditID);
        FGraphEditID := 0;
      end;
    end;
  end;
end;

procedure TxdPlayer.SetHintPlayTime(const Value: Cardinal);
begin
  if FHintPlayTime > 0 then
    FHintPlayTime := Value;
end;

procedure TxdPlayer.SetMute(const Value: Boolean);
begin
  if FMute <> Value then
  begin
    FMute := Value;
    SetVolume(FVolume);
  end;
end;

procedure TxdPlayer.SetPosition(const Value: Cardinal);
var
  nPos, nDur: Int64;
begin
  if PlayState = psStop then
    Exit;
  if Assigned(FSeeking) and FIsCanSeek then
  begin
    nPos := Int64(Value) * 10000;
    nDur := Int64(FDuration) * 10000;
    if Assigned(FSeeking) then
      FSeeking.SetPositions(nPos, AM_SEEKING_AbsolutePositioning, nDur, AM_SEEKING_AbsolutePositioning);
  end;
end;

procedure TxdPlayer.SetShowVideoByWinHandle(const Value: HWND);
begin
  if Value = 0 then
    ChangedVideoPosition(Handle)
  else if FCurShowWinHandle <> Value then
    ChangedVideoPosition(Value);
end;

procedure TxdPlayer.SetVideoMode(const Value: TVideoMode);
var
  vmr9: IBaseFilter;
begin
  if (FPlayState = psStop) and (FVideoMode <> Value) then
  begin
    if Value = vmVMR9 then
    begin
      if CreateFilter(CLSID_VideoMixingRenderer9, False, '', vmr9) then
      begin
        FVideoMode := vmVMR9;
        vmr9 := nil;
      end;
    end
    else
      FVideoMode := Value;
  end;
end;

procedure TxdPlayer.SetVolume(const Value: Integer);
begin
  FVolume := EnsureRange(Value, 0, 100);
  if not Assigned(FRenderAudio) then
    Exit;
  if FMute then
    FRenderAudio.put_Volume(-10000)
  else if FLinearVolume then
    FRenderAudio.put_Volume(Round(1085.73 * ln(FVolume * 100 + 1)) - 10000)
  else
    FRenderAudio.put_Volume(FVolume * 100 - 10000);
end;

procedure TxdPlayer.ShowCursor(const AShow: Boolean);
var
  info: TCursorInfo;
begin
//  Exit;
  info.cbSize := SizeOf(TCursorInfo);
  GetCursorInfo(info);
  if AShow then
  begin
    if info.flags = 0 then
      Windows.ShowCursor(True);
  end
  else
  begin
    if info.flags = CURSOR_SHOWING then
      Windows.ShowCursor(False);
  end;
end;

procedure TxdPlayer.Stop;
var
  p: IPin;
begin
  if FPlayState in [psCancel, psStop] then
    Exit;
  if FPlayState = psOpening then
  begin
    FPlayState := psCancel;
    if Assigned(FAsyncSourceFilter) then
    begin
      FAsyncSourceFilter.FilterStop := True;
      p := GetPin(FAsyncSourceFilter as IBaseFilter, PINDIR_OUTPUT, 0);
      if Assigned(p) then
      begin
        p.Disconnect;
        p := nil;
      end;
    end;
    if Assigned(FGraph) then
      FGraph.Abort;
  end
  else if FPlayState <> psStop then
  begin
    CloseDS;
    Invalidate;
    if Assigned(OnPlayerStop) then
      OnPlayerStop(Self);
  end;
end;

procedure TxdPlayer.VMRToMixerBmpInfo(const AVMR9: TVMR9AlphaBitmap; AMixerBmpInfo: TVideoMixerBitmapInfo);
begin
  AMixerBmpInfo.FDC := AVMR9.hdc;
  AMixerBmpInfo.FSrcRect := AVMR9.rSrc;
  AMixerBmpInfo.FDestLeft := AVMR9.rDest.left;
  AMixerBmpInfo.FDestTop := AVMR9.rDest.top;
  AMixerBmpInfo.FDestRight := AVMR9.rDest.right;
  AMixerBmpInfo.FDestBottom := AVMR9.rDest.bottom;
  AMixerBmpInfo.FAlphaValue := AVMR9.fAlpha;
  AMixerBmpInfo.FclrSrcKey := AVMR9.clrSrcKey;
end;

procedure TxdPlayer.VMRToMixerBmpInfo(const AVMR: TVMRAlphaBitmap; AMixerBmpInfo: TVideoMixerBitmapInfo);
begin
  AMixerBmpInfo.FDC := AVMR.hdc;
  AMixerBmpInfo.FSrcRect := AVMR.rSrc;
  AMixerBmpInfo.FDestLeft := AVMR.rDest.left;
  AMixerBmpInfo.FDestTop := AVMR.rDest.top;
  AMixerBmpInfo.FDestRight := AVMR.rDest.right;
  AMixerBmpInfo.FDestBottom := AVMR.rDest.bottom;
  AMixerBmpInfo.FAlphaValue := AVMR.fAlpha;
  AMixerBmpInfo.FclrSrcKey := AVMR.clrSrcKey;
end;

procedure TxdPlayer.Pause;
begin
  if FPlayState = psCancel then
    Exit;
  if Assigned(FMediaControl) then
  begin
    if (FPlayState = psPlay) then
    begin
      if AutoHintPlayInfo then        
      begin
        MixerString( FHintPlayInfoPos, '暂停', 1, FHintPlayTime );
        Sleep( 100 );
      end;
      if Succeeded(FMediaControl.Pause) then
      begin        
        FPlayState := psPause;
        if Assigned(OnPlayerPause) then
          OnPlayerPause(Self);
      end;      
    end;
  end;
end;


procedure TxdPlayer.WMDragDropFiles(var msg: TMessage);
var
  FileName: array[0..MAX_PATH] of Char;
  i, Sum: Integer;
  Files: TStringList;
begin
  inherited;
  if not Assigned(OnDragFiles) then
    Exit;
  Files := TStringList.Create;
  try
    Sum := DragQueryFile(msg.WParam, $FFFFFFFF, nil, 0);
    for i := 0 to Sum - 1 do
    begin
      //读取文件名
      FillChar(FileName, MAX_PATH, 0);
      DragQueryFile(msg.WParam, i, FileName, MAX_PATH);
      Files.Add(FileName);
    end;
    OnDragFiles(Self, Files);
  finally
    DragFinish(msg.WParam);
    Files.Free;
  end;
end;

procedure TxdPlayer.WMGraphNotify(var Message: TMessage);
var
  EventCode, Param1, Param2: Integer;
begin
  if Assigned(FEvent) then
  begin
    EventCode := 0;
    Param1 := 0;
    Param2 := 0;
    while FEvent.GetEvent(EventCode, Param1, Param2, 0) = S_OK do
    begin
      FEvent.FreeEventParams(EventCode, Param1, Param2);
      Dbg('DirectShow 事件：%d', [EventCode]);
      case EventCode of
        EC_COMPLETE: PostMessage(Handle, WM_StopPlayer, 0, 0);
//        EC_USERABORT,//:  OutputDebugString( 'EC_USERABORT');
//        EC_ERRORABORT: //OutputDebugString( 'EC_ERRORABORT');

      end;
    end;
  end;
end;

procedure TxdPlayer.WMPaint(var Message: TWMPaint);
var
  dc: HDC;
  PS: TPaintStruct;
begin
  if not (FPlayState in [psPlay, psPause]) or not IsContainVideo then
    inherited
  else
  begin
    dc := BeginPaint(Handle, PS);
    try
      if (FVideoMode = vmVMR9) and Assigned(FVMR9WinlessCtrl) then
        FVMR9WinlessCtrl.RepaintVideo(Handle, dc)
      else if (FVideoMode = vmDefault) and Assigned(FVMRWinlessCtrl) then
        FVMRWinlessCtrl.RepaintVideo(Handle, dc);
    finally
      EndPaint(Handle, PS);
    end;
  end;
end;

procedure TxdPlayer.WMStopPlayer(var message: TMessage);
begin
  FCloseByPlayer := True;
  Stop;
end;
//
//procedure InitPlayerFilter;
//var
//  strPath, strFilter: string;
//  i: Integer;
//begin
//  CoInitialize(nil);
//  Exit;
//  strPath := ExtractFilePath(ParamStr(0));
//  for i := Low(CtAryFilters) to High(CtAryFilters) do
//  begin
//    if not IsFillterRegistered(StringToGUID(CtAryFilters[i].SplitterCLSID)) then
//    begin
//      strFilter := strPath + CtFilterPath + CtAryFilters[i].SplitterFileName;
//      if FileExists(strFilter) then
//        RegisterFilter(strFilter);
//    end;
//  end;
//end;

initialization
  CoInitialize(nil);
finalization
  CoUninitialize;

end.


