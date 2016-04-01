unit uxdREC;

interface

{$Define KBoxDebug}

uses
  Windows, Classes, uDShowSub, ExtCtrls, Messages, DirectShow9, SysUtils, ActiveX
  {$IfDef KBoxDebug}
  ,uDebugInfo
  {$EndIf}
  ;

const
  WM_GRAPHEVENT = WM_USER + 524;

type
  TxdREC = class(TCustomPanel)
  public
    function  StartPreview: Boolean;
    function  StartCapture: Boolean;
    procedure Stop;

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    procedure Run;

    procedure Loaded; override;
    procedure DoErrorInfo(const AErrorMsg: string);
  private
    function  BuildPreviewGraph: Boolean;
    function  BuildCapture: Boolean;
    function  CompleteGraphBuilding: Boolean;
    function  CreateCaptureFilter: Boolean;
    
    procedure WMGraphEvent(var Message: TMessage); message WM_GRAPHEVENT;

    procedure FreeAllFilter;
  private
    //Base Filter
    FGraph: IGraphBuilder;
    FBuild: ICaptureGraphBuilder2;

    //Video and Audio filter
    FCaptureFilter: IBaseFilter;
    FPreviewWindow: IVideoWindow;

    FASFFileMux: IBaseFilter;
    FASFFileSink: IFileSinkFilter;

    //Event Handle
    FMediaEvent: IMediaEventEx;
    FCaptureIndex: Integer;

    FOutputFileName: String;

    procedure SetCaptureIndex(const Value: Integer);
  published
    property Color;
    property CaptureIndex: Integer read FCaptureIndex write SetCaptureIndex;
  end;

implementation

{ TKBoxREC }

const
  CtCaptureFilterName = 'KBoxCapture Filter';
{$IfDef KBoxDebug}
  CtDebugLog = 'DebugLog.txt';
{$EndIf}

procedure DebugOut(const AInfo: string);
begin
  OutputDebugString( PChar(AInfo) );
  {$IfDef KBoxDebug}
  _Log( AInfo, CtDebugLog );
  {$EndIf}
end;

function OleS(s: string; var OleStr: pOleStr): pOleStr;
begin
   Result := StringToOleStr (s);
   OleStr := Result;
end;


function TxdREC.BuildCapture: Boolean;
var
  hRes: HRESULT;
  OleStr: pOleStr;
  pConfig: IConfigAsfWriter;
  pAMCfg: IAMStreamConfig;
  pmt: PAMMediaType;
  pVi: PVideoInfoHeader;
  stop: TReferenceTime;
begin
  //build preview graph
  Result := Succeeded( InitCaptureGraphBuilder(FGraph, FBuild) ) and CreateCaptureFilter;
  if not Result then
  begin
    FreeAllFilter;
    Exit;
  end;


  hRes := FBuild.SetOutputFileName( MEDIASubtype_ASF, OleS(FOutputFileName, OleStr), FASFFileMux, FASFFileSink);
//  hRes := FBuild.SetOutputFileName( MEDIASUBTYPE_Avi, OleS(FOutputFileName, OleStr), FASFFileMux, FASFFileSink);
  if Failed(hRes) then
  begin
    Result := False;
    DoErrorInfo( GetAMErrorText(hRes) );
    FreeAllFilter;
    Exit;
  end;

//  hRes := FASFFileMux.QueryInterface( IID_IConfigAsfWriter, pConfig );
//  if Failed(hRes) then
//  begin
//    Result := False;
//    DoErrorInfo( GetAMErrorText(hRes) );
//    FreeAllFilter;
//    Exit;
//  end;
//  pConfig := nil;
//
  hRes := FBuild.RenderStream( @PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video, FCaptureFilter, nil, FASFFileMux );
//  if Failed(hRes) then
//  begin
//    Result := False;
//    DoErrorInfo( GetAMErrorText(hRes) );
//    FreeAllFilter;
//    Exit;
//  end;
//
//  hRes := FBuild.FindInterface( @PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video, FCaptureFilter,
//                                IID_IAMStreamConfig, pAMCfg );
//  if Failed(hRes) then
//  begin
//    Result := False;
//    DoErrorInfo( GetAMErrorText(hRes) );
//    FreeAllFilter;
//    Exit;
//  end;
//
//  pAMCfg.GetFormat( pmt );
//  if IsEqualGUID(pmt^.formattype, FORMAT_VideoInfo) then
//  begin
//    pVi := PVideoInfoHeader(pmt^.pbFormat);
//    pvi^.AvgTimePerFrame := LONGLONG(10000000 div 30);
//    hRes := pAMCfg.SetFormat( pmt^ );
//    if Failed(hRes) then
//    begin
//      Result := False;
//      DoErrorInfo( GetAMErrorText(hRes) );
//      FreeAllFilter;
//      Exit;
//    end;
//  end;
//  DeleteMediaType( pmt );

  Result := CompleteGraphBuilding;

//  FBuild.ControlStream( @PIN_CATEGORY_CAPTURE, nil,
//            nil, nil, @stop, 0, 0)
end;

function TxdREC.BuildPreviewGraph: Boolean;
var
  hRes: HRESULT;
begin
  //build preview graph
  Result := Succeeded( InitCaptureGraphBuilder(FGraph, FBuild) ) and CreateCaptureFilter;
  if not Result then
  begin
    FreeAllFilter;
    Exit;
  end;
  hRes := FBuild.RenderStream( @PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video, FCaptureFilter, nil, nil );
  if Failed(hRes) then
  begin
    Result := False;
    DoErrorInfo( GetAMErrorText(hRes) );
    FreeAllFilter;
    Exit;
  end;
  //finished
  Result := CompleteGraphBuilding;
end;

function TxdREC.CompleteGraphBuilding: Boolean;
var
  hRes: HRESULT;
begin
  //Video Window setting
  hRes := FGraph.QueryInterface( IID_IVideoWindow, FPreviewWindow );
  if Failed(hRes) then
  begin
    Result := False;
    DoErrorInfo( GetAMErrorText(hRes) );
    FreeAllFilter;
    Exit;
  end;
  FPreviewWindow.put_Owner( OAHWND(Handle) );
  FPreviewWindow.put_WindowStyle( WS_CHILD );
  FPreviewWindow.SetWindowPosition( 0, 0, Width, Height );
  FPreviewWindow.put_Visible( True );

  //Event Handle
  hRes := FGraph.QueryInterface( IID_IMediaEventEx, FMediaEvent );
  if Failed(hRes) then
  begin
    Result := False;
    DoErrorInfo( GetAMErrorText(hRes) );
    FreeAllFilter;
    Exit;
  end;
  FMediaEvent.SetNotifyWindow( OAHWND(Handle), WM_GRAPHEVENT, 0 );

  Result := True;
end;

constructor TxdREC.Create(AOwner: TComponent);
begin
  inherited;
  FGraph := nil;
  FBuild := nil;
  FCaptureFilter := nil;
  FPreviewWindow := nil;
  FMediaEvent := nil;
  FCaptureIndex := 1;
  FOutputFileName := 'E:\CompanyWork\MusicT\Small Project\Player_Rec_2\bin\a.wmv';
end;

function TxdREC.CreateCaptureFilter: Boolean;
var
  hRes: HRESULT;
begin
  Result := False;
  if FCaptureIndex = -1 then Exit;
  if not GetHardwareFilterByCategories( CLSID_VideoInputDeviceCategory, FCaptureIndex, FCaptureFilter ) then
  begin
    DoErrorInfo( 'Can not find filter at position: ' + IntToStr(FCaptureIndex) );
    Exit;
  end;

  hRes := FGraph.AddFilter( FCaptureFilter, CtCaptureFilterName );
  if Failed(hRes) then
  begin
    DoErrorInfo( GetAMErrorText(hRes) );
    Exit;
  end;
  Result := True;
end;

destructor TxdREC.Destroy;
begin
  Stop;
  inherited;
end;

procedure TxdREC.DoErrorInfo(const AErrorMsg: string);
begin
  DebugOut( AErrorMsg );
end;

procedure TxdREC.FreeAllFilter;
begin
  if FGraph <> nil then RemoveAllFilter( FGraph );
  if FMediaEvent <> nil then FMediaEvent := nil;
  if FPreviewWindow <> nil then
  begin
    FPreviewWindow.put_Owner( OAHWND(0) );
    FPreviewWindow.put_Visible( False );
    FPreviewWindow := nil;
  end;
  if FGraph <> nil then FGraph := nil;
  if FBuild <> nil then FBuild := nil;
  if FCaptureFilter <> nil then FCaptureFilter := nil;
end;

procedure TxdREC.Loaded;
begin
  inherited;
end;

procedure TxdREC.Run;
var
  Ctrl: IMediaControl;
  hRes: HRESULT;
begin
  if FGraph <> nil then
  begin
    FGraph.QueryInterface( IID_IMediaControl, Ctrl );
    try
      hRes := Ctrl.Run;
      if Failed(hRes) then
        DoErrorInfo( GetAMErrorText(hRes) );
    finally
      Ctrl := nil;
    end;
  end;
end;

procedure TxdREC.SetCaptureIndex(const Value: Integer);
begin
  FCaptureIndex := Value;
end;

function TxdREC.StartCapture: Boolean;
begin
  Result := BuildCapture;
  if Result then
  begin
//    Sleep(1000);
    Run;
  end;
end;

function TxdREC.StartPreview: Boolean;
begin
  Result := BuildPreviewGraph;
  if Result then
    Run;
end;

procedure TxdREC.Stop;
var
  Ctrl: IMediaControl;
  hRes: HRESULT;
begin
  if FGraph <> nil then
  begin
    FGraph.QueryInterface( IID_IMediaControl, Ctrl );
    try
      hRes := Ctrl.Stop;
      if Failed(hRes) then
        DoErrorInfo( GetAMErrorText(hRes) );
    finally
      Ctrl := nil;
    end;
    FreeAllFilter;
  end;
end;

procedure TxdREC.WMGraphEvent(var Message: TMessage);
var
  EventCode, Param1, Param2: Integer;
begin
  if Assigned(FMediaEvent) then
  begin
    EventCode:= 0;
    Param1:= 0;
    Param2:= 0;
    while FMediaEvent.GetEvent(EventCode,Param1,Param2,0) = S_OK do
    begin
      FMediaEvent.FreeEventParams(EventCode,Param1,Param2);
      case EventCode of
        EC_COMPLETE:    DebugOut('EC_COMPLETE');
        EC_DEVICE_LOST: DebugOut('EC_DEVICE_LOST');
        EC_ERRORABORT:  DebugOut('EC_ERRORABORT');
        EC_ACTIVATE:    DebugOut('EC_ACTIVATE');
        else
          DebugOut( 'WMGraphEvent' );
      end;
    end;
  end;
end;

end.
