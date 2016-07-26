{
单元名称: uBasicUDP
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: 基本通信能力, 使用事件模式
开始时间: 2009-1-13
修改时间: 2009-1-14 (最后修改)

基本使用方法: 
  Server:
    FUDP := TBasicUDP.Create;
    FUDP.IsBind := True;
    FUDP.OnRecvBuffer := DoReadBuffer;
    FUDP.Port := 6868;
    FUDP.IP := inet_addr('192.168.1.52');
  client:
    FUDP := TBasicUDP.Create;
    FUDP.IsBind := True;
    FUDP.IsExclusitve := True;
    FUDP.OnRecvBuffer := DoReadBuffer;
}
unit uBasicUDP;

interface
uses windows, WinSock2, SysUtils, RTLConsts, Classes, uSocketSub;

type
  TOnNotifyInfo = procedure(Sender: TObject; const AInfo: PChar) of object;
  EUDPError = class(Exception);

  TUDPRecvThread = class;

  {$M+}  
  TBasicUDP = class(TObject)
  private
    FActive: Boolean;
    FSocket: TSocket;
    FIsBind: Boolean; //是否绑定在本地
    FPort: Word; //本地字节序端口号
    FIP: Cardinal;
    FIsExclusitve: Boolean;
    FRecvThread: TUDPRecvThread;
    FOnError: TOnNotifyInfo;
    FIsAutoIncPort: Boolean;

    procedure InitAllVar;
    procedure InitSocket;
    procedure FreeSocket;
    procedure Open;
    procedure Close;

    procedure SetPort(const Value: Word);
    procedure SetActive(const Value: Boolean);
    procedure SetIP(const Value: Cardinal);
    procedure SetIsBind(const Value: Boolean);
    procedure SetExclusitve(const Value: Boolean);
    procedure SetIsAutoIncPort(const Value: Boolean);
  protected
    procedure DoRecvBuffer; virtual;   //由线程调用
    function  DoBeforOpenUDP: Boolean; virtual;  //初始化UDP前; True: 允许初始化; False: 不允许初始化
    procedure DoAfterOpenUDP; virtual;
    procedure DoBeforCloseUDP; virtual;
    procedure DoAfterCloseUDP; virtual; //UDP关闭之后
    procedure DoErrorInfo(const AInfo: PAnsiChar); virtual;
  public
    function __SendBuffer(AIP: Cardinal; AHostShortPort: word; var ABuffer; const ABufferLen: Integer): Integer;
    function __RecvBuffer(var ABuffer; var ABufferLen: Integer; var ASockAddr: TSockAddr): Boolean;

    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive;
    property Port: Word read FPort write SetPort;
    property IP: Cardinal read FIP write SetIP;                                //多网址可设置
    property IsAutoIncPort: Boolean read FIsAutoIncPort write SetIsAutoIncPort;//当指定端口无法绑定时,是否自动增加
    property IsBind: Boolean read FIsBind write SetIsBind;                     //如果想接收数据,设置为真
    property IsExclusitve: Boolean read FIsExclusitve write SetExclusitve;     //防止套接字被别人监听

    property OnError: TOnNotifyInfo read FOnError write FOnError;
  end;
  {$M-}

  TUDPRecvThread = class(TThread)
  private
    FOwner: TBasicUDP;
    FhEvent: WSAEvent;
    FClose: Boolean;
    FIsEventSuccess: Boolean;
  protected
    procedure DoUDPRead;
    procedure Execute; override;
  public
    constructor Create(AOwner: TBasicUDP);
    destructor Destroy; override;
  end;

implementation

{ TUDP }

const
  CtSockAddrLen = SizeOf(TSockAddr);

procedure RaiseWinSocketError(AErrCode: Integer; AAPIName: PChar);
begin
  raise EUDPError.Create( Format(sWindowsSocketError, [SysErrorMessage(AErrCode), AErrCode, AAPIName]) );
end;

procedure RaiseError(const AErrString: string);
begin
  raise EUDPError.Create( AErrString );
end;

procedure TBasicUDP.Close;
begin
  if FActive then
  begin
    if FRecvThread <> nil then
      FreeAndNil(FRecvThread);
    FreeSocket;
  end;
end;

constructor TBasicUDP.Create;
begin
  InitAllVar;
end;

destructor TBasicUDP.Destroy;
begin
  Close;
  inherited;
end;

procedure TBasicUDP.FreeSocket;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    shutdown(FSocket, SD_BOTH);
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
end;

procedure TBasicUDP.InitAllVar;
begin
  FActive := False;
  FSocket := INVALID_SOCKET;
  FIsBind := False;
  FPort := 8888;
  FIP := ADDR_ANY;
  FIsExclusitve := True;
  FRecvThread := nil;
  FIsAutoIncPort := False;
end;

procedure TBasicUDP.InitSocket;
const
  CtMaxTestCount = 8;
var
  SockAddr: TSockAddr;
  nMaxTestCount: Integer;
Label
  TryIncPort;
begin
  if Port = 0 then
    RaiseError( '监听的端口号为0!' );
  FSocket := WSASocket( AF_INET, SOCK_DGRAM, 0, nil, 0, WSA_FLAG_OVERLAPPED );
  if FSocket = INVALID_SOCKET then
    RaiseWinSocketError( WSAGetLastError, 'WSASocket' );
  if FIsExclusitve and (not SetSocketExclusitveAddr( FSocket )) then
    RaiseError( '无法设置独占式端口!' );
  if IsBind then
  begin
    nMaxTestCount := 0;
    TryIncPort:
    if nMaxTestCount >= CtMaxTestCount then
      RaiseWinSocketError( WSAGetLastError, 'bind' );
    SockAddr := InitSocketAddr( IP, Port );
    if SOCKET_ERROR = bind( FSocket, @SockAddr, CtSockAddrLen ) then
    begin
      Inc( FPort );
      Inc( nMaxTestCount );
      goto TryIncPort;
    end;
  end;
end;

procedure TBasicUDP.DoAfterCloseUDP;
begin

end;

procedure TBasicUDP.DoAfterOpenUDP;
begin

end;

procedure TBasicUDP.DoBeforCloseUDP;
begin

end;

function TBasicUDP.DoBeforOpenUDP: Boolean;
begin
  Result := True;
end;

procedure TBasicUDP.DoErrorInfo(const AInfo: PAnsiChar);
begin
  if Assigned( OnError ) then
    OnError( Self, AInfo );
end;

procedure TBasicUDP.DoRecvBuffer;
begin

end;

procedure TBasicUDP.Open;
begin
  if not FActive then
  begin
    InitSocket;
    if IsBind then
      FRecvThread := TUDPRecvThread.Create(Self);
  end;
end;

function TBasicUDP.__RecvBuffer(var ABuffer; var ABufferLen: Integer; var ASockAddr: TSockAddr): Boolean;
var
  AddrLen: Integer;
begin
  Result := False;
  if not Active then
  begin
    DoErrorInfo( 'TBasicUDP外于非活动状态!( Active := False )' );
    Exit;
  end;
  AddrLen := CtSockAddrLen;
  try
    ABufferLen := recvfrom( FSocket, ABuffer, ABufferLen, 0, ASockAddr, AddrLen );
  except
  end;
  Result := ABufferLen <> SOCKET_ERROR;
end;

function TBasicUDP.__SendBuffer(AIP: Cardinal; AHostShortPort: word; var ABuffer; const ABufferLen: Integer): Integer;
var
  SockAddr: TSockAddr;
begin
  Result := -1;
  if not Active then
  begin
    DoErrorInfo( 'TBasicUDP外于非活动状态!( Active := False )' );
    Exit;
  end;
  if (FSocket = INVALID_SOCKET) or ( (AIP = ADDR_ANY) or (AIP = INADDR_NONE) ) or
     ( AHostShortPort = 0 ) or ( ABufferLen < 0 ) then
  begin
    DoErrorInfo( 'TBasicUDP.__SendBuffer中有参数不对!' );
    Exit;
  end;

  SockAddr := InitSocketAddr( AIP, AHostShortPort );
  Result := sendto( FSocket, ABuffer, ABufferLen, 0, SockAddr, CtSockAddrLen );
end;

procedure TBasicUDP.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
    begin
      if DoBeforOpenUDP then
        Open
      else
        Exit;
      DoAfterOpenUDP;
    end
    else
    begin
      DoBeforCloseUDP;
      Close;
      DoAfterCloseUDP;
    end;
    FActive := Value;
  end;
end;

procedure TBasicUDP.SetExclusitve(const Value: Boolean);
begin
  if (not Active) and (FIsExclusitve <> Value) then
    FIsExclusitve := Value;
end;

procedure TBasicUDP.SetIP(const Value: Cardinal);
begin
  if (not Active) and (FIP <> Value) then
    FIP := Value;
end;

procedure TBasicUDP.SetIsAutoIncPort(const Value: Boolean);
begin
  if (not Active) and (FIsAutoIncPort <> Value) then
    FIsAutoIncPort := Value;
end;

procedure TBasicUDP.SetIsBind(const Value: Boolean);
begin
  if (not Active) and (FIsBind <> Value) then
    FIsBind := Value;
end;

procedure TBasicUDP.SetPort(const Value: Word);
begin
  if (not Active) and (FPort <> Value) then
    FPort := Value;
end;

{ TUDPRecvThread }

constructor TUDPRecvThread.Create(AOwner: TBasicUDP);
begin
  FClose := False;
  FOwner := AOwner;
  FIsEventSuccess := True;
  FhEvent := CreateEvent(nil, False, False, ''); //不使用 WSACreateEvent 的原因.是为了使用自动重置,不然当包过多,来不及处理时.
  if FhEvent = WSA_INVALID_EVENT then            //此处会产生丢包.
    RaiseError( Format('TUDPRecvThread.Create WSACreateEvent error,Code: %d', [WSAGetLastError]) );
  if WSAEventSelect( FOwner.FSocket, FhEvent, FD_READ ) = SOCKET_ERROR then
    RaiseError( Format('TUDPRecvThread.Create WSAEventSelect error,Code: %d', [WSAGetLastError()]) );
  inherited Create(False);
end;

destructor TUDPRecvThread.Destroy;
begin
  Terminate;
  WSASetEvent(FhEvent);
  while not FClose do
    WaitForSingleObject( Self.Handle, 300 );
  if FhEvent <> WSA_INVALID_EVENT then
  begin
    WSACloseEvent( FhEvent );
    FhEvent := WSA_INVALID_EVENT;
  end;
  inherited;
end;

procedure TUDPRecvThread.DoUDPRead;
begin
  FIsEventSuccess := True;
  FOwner.DoRecvBuffer;
end;

procedure TUDPRecvThread.Execute;
var
  Code: Cardinal;
  NetEvent: TWSANetworkEvents;
begin
  while not Terminated do
  begin
    Code := WSAWaitForMultipleEvents(1, @FhEvent, True, INFINITE, False) - WSA_WAIT_EVENT_0;
    if Terminated or (Code = WSA_WAIT_FAILED) then
    begin
      FClose := True;
      Exit;
    end;
    FIsEventSuccess := False;
    if 0 = WSAEnumNetworkEvents( FOwner.FSocket, FhEvent, @NetEvent ) then
    begin
      if ( (NetEvent.lNetworkEvents and FD_READ) > 0 ) and ( NetEvent.iErrorCode[FD_READ_BIT] = 0 ) then
      begin
        //读事件有效, 以上判断去掉也不影响
        if Code = WSA_WAIT_EVENT_0 then
          DoUDPRead;
      end;
    end;
//    WSAResetEvent(FhEvent);
    if not FIsEventSuccess then
      FOwner.DoErrorInfo( PChar(Format( 'TUDPRecvThread.Execute中WSAEnumNetworkEvents失败. NetEvent.lNetworkEvents = %d; ' +
                          'NetEvent.iErrorCode[FD_READ_BIT] := %d; Code := %d', [NetEvent.lNetworkEvents,
                          NetEvent.iErrorCode[FD_READ_BIT], Code] )) );
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
procedure Startup;
var
  ErrorCode: Integer;
  WSAData: TWSAData;
begin
  ErrorCode := WSAStartup($0202, WSAData);
  if ErrorCode <> 0 then
    RaiseWinSocketError(ErrorCode, 'WSAStartup');
end;

procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    RaiseWinSocketError(ErrorCode, 'WSACleanup');
end;


initialization
  Startup;
finalization
  Cleanup;
end.
