unit uPlayMusic;

interface
uses
  Windows, Classes, SysUtils, Messages, DirectShow9, uDShowSub, ActiveX, DSUtil, Controls;

const
  WM_GRAPHEVENT = WM_USER + 2;
type
  TOnBeforePlay = procedure(Sender: TObject; GraphBuilder: IGraphBuilder) of object;
  TOnUnableToRender = function(Sender: TObject; Pin: IPin): Boolean of Object;
  TOnSelectedFilter = function(Sender: TObject; Moniker: IMoniker; FilterName: WideString; ClassID: TGuid): Boolean of Object;
  TOnCreatedFilter  = function(Sender: TObject; Filter: IBaseFilter; ClassID: TGuid): Boolean of Object;
  TOnErrorMsg = procedure(Sender: TObject; const AMsg: string) of object;

  TPlayMusic = class(TWinControl, IAMFilterGraphCallback, IAMGraphBuilderCallback)
  private
    FGraphBuilder: IGraphBuilder;
    FMediaEvent: IMediaEventEx;
    FOnPlayFinish: TNotifyEvent;
    FCurPlayFileName: WideString;
    FOnErrorMsg: TOnErrorMsg;

    //Interfaces for Controlling a Filter Graph
    function  GetMediaControl: IMediaControl;
    function  GetMediaSeeking: IMediaSeeking;

    // IAMFilterGraphCallback
    function UnableToRender(ph1, ph2: integer; pPin: IPin): HResult; //when fails to render a pin
    //IAMGraphBuilderCallback
    function SelectedFilter(pMon: IMoniker): HResult; stdcall;
    function CreatedFilter(pFil: IBaseFilter): HResult; stdcall;
  private
    FOnUnableToRender: TOnUnableToRender;
    FOnSelectedFilter: TOnSelectedFilter;
    FOnCreatedFilter: TOnCreatedFilter;
    FOnBeforePlay: TOnBeforePlay;
    function  GetDuration: Int64;
    function  GetCurPlayPos: Int64;
    procedure SetCurPlayPos(const Value: Int64);
    function  GetCurState: FILTER_STATE;
    procedure SetCurSate(const Value: FILTER_STATE);
    procedure SetPlayFileName(const Value: WideString);
    function  GetCurTimeFormat: TGUID;
  protected
    procedure BuildCallBackEvent;
    procedure BuildGraphManage;
    procedure CloseGraphManage;
    procedure DoPlayFinish; virtual;
    procedure DoErrorMsg(const AMsg: string); virtual;
    procedure WMGraphEvent(var Message: TMessage); message WM_GRAPHEVENT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Play;
    procedure Stop;
    procedure Paused;

    procedure SaveGraphManageToFile(AFileName: string);

    class function StandardPos(APos: Int64): Integer; //转成毫秒
    class function StandardPosToPlayPos(APos: Integer): Int64; //转成 十亿分之一 秒
    class function PosToTimeString(APos: Int64): string; //APos 单位: 十亿分之一 秒
    class function TimeFormatString(ATimeFormat: TGUID): string;
  published
    property Duration: Int64 read GetDuration;                         //单位: 十亿分之一 秒
    property CurPlayPos: Int64 read GetCurPlayPos write SetCurPlayPos; //单位: 十亿分之一 秒
    property CurState: FILTER_STATE read GetCurState write SetCurSate;   //与上面 play, Stop, Pause 功能相似
    property CurTimeFormat: TGUID read GetCurTimeFormat;
    property CurPlayFileName: WideString read FCurPlayFileName write SetPlayFileName;
    //事件
    property OnPlayFinish: TNotifyEvent read FOnPlayFinish write FOnPlayFinish;
    property OnErrorMsg: TOnErrorMsg read FOnErrorMsg write FOnErrorMsg;
    property OnUnableToRender: TOnUnableToRender read FOnUnableToRender write FOnUnableToRender;
    property nSelectedFilter: TOnSelectedFilter read FOnSelectedFilter write FOnSelectedFilter;
    property OnCreatedFilter: TOnCreatedFilter read FOnCreatedFilter write FOnCreatedFilter;
    property OnBeforePlay: TOnBeforePlay read FOnBeforePlay write FOnBeforePlay;
  end;

implementation

uses Unit1;

{ TPlayMusic }
const
  IID_IObjectWithSite: TGuid = '{FC4801A3-2BA9-11CF-A229-00AA003D7352}';

procedure TPlayMusic.BuildCallBackEvent;
var
  obj: IObjectWithSite;
  fg: IAMFilterGraphCallback;
  gb:  IAMGraphBuilderCallback;
begin
  if Succeeded(QueryInterface(IID_IObjectWithSite, obj)) then
  begin
    QueryInterface(IID_IAMFilterGraphCallback, fg);
    if Assigned(fg) then
    begin
      obj.SetSite(fg);
      fg := nil;
    end;
    QueryInterface(IID_IAMGraphBuilderCallback, gb);
    if Assigned(gb) then
    begin
      obj.SetSite(gb);
      gb := nil;
    end;
  end;
end;

procedure TPlayMusic.BuildGraphManage;
begin
  if FGraphBuilder <> nil then CloseGraphManage;
  if Failed( CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IGraphBuilder, FGraphBuilder) ) then
  begin
    DoErrorMsg('无法创建播放器');
    Exit;
  end;
  if Succeeded(FGraphBuilder.QueryInterface(IID_IMediaEventEx, FMediaEvent)) then
  begin
    FMediaEvent.SetNotifyFlags(0);
    FMediaEvent.SetNotifyWindow(OAHWND(Self.Handle), WM_GRAPHEVENT, 0);
  end;
  BuildCallBackEvent;
end;

procedure TPlayMusic.CloseGraphManage;
begin
  Stop;
  RemoveAllFilter(FGraphBuilder);
  FGraphBuilder := nil;
end;

constructor TPlayMusic.Create(AOwner: TComponent);
begin
  inherited;
  FGraphBuilder := nil;
  FMediaEvent := nil;
  if AOwner is TWinControl then
    Parent := AOwner as TWinControl;
end;

function TPlayMusic.CreatedFilter(pFil: IBaseFilter): HResult;
var
  guid: TGuid;
begin
  //在 Filter Manage 创建一个Filter, 但没有连接它时调用
  //S_OK: 同意进行连接
  //S_FAIL: 不同意
  OutputDebugString(PChar('CreatedFilter'));
  if Assigned(FOnCreatedFilter) then
  begin
    pfil.GetClassID(guid);
    if FOnCreatedFilter(Self, pFil,guid) then
      Result := S_OK
    else
      Result := E_FAIL;
  end
  else
    Result := S_OK;
end;

destructor TPlayMusic.Destroy;
begin
  if CurState <> State_Stopped then
    Stop;
  RemoveAllFilter(FGraphBuilder);
  FMediaEvent := nil;
  FGraphBuilder := nil;
  inherited;
end;

procedure TPlayMusic.DoErrorMsg(const AMsg: string);
begin
  if Assigned(FOnErrorMsg) then
    FOnErrorMsg(Self, AMsg);
end;

procedure TPlayMusic.DoPlayFinish;
begin
  CurState := State_Stopped;
  if Assigned(FOnPlayFinish) then
    FOnPlayFinish(Self);
end;

function TPlayMusic.GetCurPlayPos: Int64;
var
  pMediaSeek: IMediaSeeking;
begin
  Result := 0;
  try
    pMediaSeek := GetMediaSeeking;
    if pMediaSeek <> nil then
      pMediaSeek.GetCurrentPosition(Result)
  finally
    pMediaSeek := nil;
  end;
end;

function TPlayMusic.GetCurState: FILTER_STATE;
var
  pCtrl: IMediaControl;
begin
  Result := State_Stopped;
  try
    pCtrl := GetMediaControl;
    if pCtrl <> nil then
      pCtrl.GetState(10, Result);
  finally
    pCtrl := nil;
  end;
end;

function TPlayMusic.GetCurTimeFormat: TGUID;
var
  pMediaSeek: IMediaSeeking;
begin
  try
    pMediaSeek := GetMediaSeeking;
    if pMediaSeek <> nil then
      pMediaSeek.GetTimeFormat(Result);
  finally
    pMediaSeek := nil;
  end;
end;

function TPlayMusic.GetDuration: Int64;
var
  pMediaSeek: IMediaSeeking;
begin
  Result := 0;
  try
    pMediaSeek := GetMediaSeeking;
    if not ((pMediaSeek <> nil) and (S_OK = pMediaSeek.GetDuration(Result))) then
      Result := 0;
  finally
    pMediaSeek := nil;
  end;
end;

function TPlayMusic.GetMediaControl: IMediaControl;
begin
  Result := nil;
  if not (Assigned(FGraphBuilder) and Succeeded(FGraphBuilder.QueryInterface(IID_IMediaControl, Result))) then
    Result := nil;
end;

function TPlayMusic.GetMediaSeeking: IMediaSeeking;
begin
  Result := nil;
  if not (Assigned(FGraphBuilder) and Succeeded(FGraphBuilder.QueryInterface(IID_IMediaSeeking, Result))) then
    Result := nil;
end;

procedure TPlayMusic.Paused;
var
  pCtrl: IMediaControl;
begin
  if CurState <> State_Running then
  begin
    DoErrorMsg('Plaese play the music first');
    Exit;
  end;
  try
    pCtrl := GetMediaControl;
    if pCtrl <> nil then
      pCtrl.Pause;
  finally
    pCtrl := nil;
  end;
end;

procedure TPlayMusic.Play;
var
  pCtrl: IMediaControl;
  p: IVMRWindowlessControl;
//  pr: PRect;
begin
  if CurState <> State_Stopped then
  begin
    DoErrorMsg('Must Stop play first!');
    Exit;
  end;
  BuildGraphManage;
  if not Assigned(FGraphBuilder) then
  begin
    DoErrorMsg('Init Graph Manage Fail!');
    Exit;
  end;
  InitWindowlessVMR(Form1.Panel1.Handle, FGraphBuilder, p);
//  New(pr);
//  pr.Left := Form1.Panel1.Left;
//  pr.Top := Form1.Panel1.Top;
//  pr.Right := Form1.Panel1.Left + Form1.Panel1.Width;
//  pr.Bottom := Form1.Panel1.Top + Form1.Panel1.Height;
//  p.SetVideoPosition(pr, pr);
  try
    if Succeeded(FGraphBuilder.RenderFile(StringToOleStr(CurPlayFileName), nil)) then
    begin
      if Assigned(OnBeforePlay) then
        OnBeforePlay(Self, FGraphBuilder);
      pCtrl := GetMediaControl;
      if pCtrl <> nil then
        pCtrl.Run;
    end
    else
      DoErrorMsg('Can''t play the music of ' + CurPlayFileName);
  finally
    pCtrl := nil;
  end;
end;

procedure TPlayMusic.SaveGraphManageToFile(AFileName: string);
begin
  SaveGraphFile(FGraphBuilder, AFileName);
end;

function TPlayMusic.SelectedFilter(pMon: IMoniker): HResult;
var
  PropBag: IPropertyBag;
  Name: OleVariant;
  vGuid: OleVariant;
  Guid: TGUID;
begin
  //锁定,不更改 Filter 状态
  //返回值
  //S_OK: 接受此 Filter, 接下来将创建它,并尝试连接此 Filter
  //S_Fail: 拒绝此 Filter
  OutputDebugString(PChar('SelectedFilter'));
  
  if Assigned(FOnSelectedFilter) then
  begin
    pMon.BindToStorage(nil, nil, IID_IPropertyBag, PropBag);
    if PropBag.Read('CLSID',vGuid,nil) = S_OK then
      Guid := StringToGUID(vGuid)
    else
      Guid := GUID_NULL;
    if PropBag.Read('FriendlyName', Name, nil) <> S_OK then
      Name := '';
    PropBag := nil;
    if FOnSelectedFilter(Self, pMon,Name,Guid) then
      Result := S_OK
    else
      Result := E_FAIL;
  end
  else
    Result := S_OK;
end;

procedure TPlayMusic.SetCurPlayPos(const Value: Int64);
var
  pMediaSeek: IMediaSeeking;
  nValue: Int64;
begin
  try
    pMediaSeek := GetMediaSeeking;
    if pMediaSeek <> nil then
    begin
      nValue := Value;
      pMediaSeek.SetPositions(nValue, AM_SEEKING_AbsolutePositioning, nValue, AM_SEEKING_NoPositioning);
    end;
  finally
    pMediaSeek := nil;
  end;
end;

procedure TPlayMusic.SetCurSate(const Value: FILTER_STATE);
begin
  if Value <> CurState then
  begin
    if Value = State_Stopped then
      Stop
    else if Value = State_Paused then
      Paused
    else
      Play;
  end;
end;

procedure TPlayMusic.SetPlayFileName(const Value: WideString);
begin
  if FileExists(Value) and (WideCompareText(FCurPlayFileName, Value) <> 0) then
  begin
    FCurPlayFileName := Value;
    CurState := State_Stopped;
  end;
end;

class function TPlayMusic.StandardPos(APos: Int64): Integer;
begin
  Result := APos div 10000; //转成毫秒
end;

class function TPlayMusic.StandardPosToPlayPos(APos: Integer): Int64;
begin
  Result := APos * 10000;
end;

procedure TPlayMusic.Stop;
var
  pCtrl: IMediaControl;
begin
  if CurState = State_Stopped then
  begin
    DoErrorMsg('Plaese play the music first');
    Exit;
  end;
  
  try
    pCtrl := GetMediaControl;
    if pCtrl <> nil then
      pCtrl.Stop;
  finally
    pCtrl := nil;
  end;
end;

class function TPlayMusic.TimeFormatString(ATimeFormat: TGUID): string;
begin
  Result := 'Unknow Time Format!';
  if IsEqualGUID(ATimeFormat, TIME_FORMAT_MEDIA_TIME) then
    Result := 'TIME_FORMAT_MEDIA_TIME'
  else if IsEqualGUID(ATimeFormat, TIME_FORMAT_NONE) then
    Result := 'TIME_FORMAT_NONE'
  else if IsEqualGUID(ATimeFormat, TIME_FORMAT_FRAME) then
    Result := 'TIME_FORMAT_FRAME'
  else if IsEqualGUID(ATimeFormat, TIME_FORMAT_BYTE) then
    Result := 'TIME_FORMAT_BYTE'
  else if IsEqualGUID(ATimeFormat, TIME_FORMAT_SAMPLE) then
    Result := 'TIME_FORMAT_SAMPLE'
  else if IsEqualGUID(ATimeFormat, TIME_FORMAT_FIELD) then
    Result := 'TIME_FORMAT_FIELD';
end;

class function TPlayMusic.PosToTimeString(APos: Int64): string;
var
  wHour, wMinute, wSecond: WORD;
  dt: TDateTime;
begin
  APos := APos div 10000000; //转成秒
  wHour := APos div 3600;
  wMinute := APos mod 3600 div 60;
  wSecond := APos - wHour * 3600 - wMinute * 60;
  dt := EncodeTime(wHour, wMinute, wSecond, 0);
  if (wHour = 0) then
    Result := FormatDateTime('nn:ss', dt)
  else
    Result := FormatDateTime('hh:nn:ss', dt);
end;

function TPlayMusic.UnableToRender(ph1, ph2: integer; pPin: IPin): HResult;
begin
  // S_OK: 重试
  // 其它: 忽略
  //在此回调中, 注意为"独占式"回调. 以免发生死锁
  DoErrorMsg('无法为指定的PIN打到对应的过滤器');
  Result := S_FALSE;
  if Assigned(FOnUnableToRender) then
  begin
    if FOnUnableToRender(Self, pPin) then
      Result := S_OK
    else
      Result := S_FALSE;
  end;
end;

procedure TPlayMusic.WMGraphEvent(var Message: TMessage);
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
      case EventCode of
        EC_COMPLETE: DoPlayFinish;
      end;
      FMediaEvent.FreeEventParams(EventCode,Param1,Param2);

      OutputDebugString(PChar('WMGraphEvent'));
    end;
  end;
end;

end.
