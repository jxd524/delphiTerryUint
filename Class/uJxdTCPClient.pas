{
��Ԫ����: uJxdTCPClient
��Ԫ����: ������(Terry)
��    ��: jxd524@163.com
˵    ��: �¼�ģʽ���첽IO TCP�ͻ��˻���ͨ��ģ�飬ʹ���̼߳���
��ʼʱ��: 2010-09-26
�޸�ʱ��: 2010-09-26(����޸�)
}
unit uJxdTCPClient;

interface
  uses Windows, SysUtils, Classes, RTLConsts, uJxdWinSock, uSocketSub;

type
  {$M+}
  ETCPError = class(Exception);
  TJxdTCPRecvThread = class;
  TOnRecvBuffer = procedure(Sender: TObject; const ApBuffer: PAnsiChar; const ABufferLen: Cardinal) of object;
  TxdTCPClient = class
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    function  SendBuffer(var ABuffer: pChar; ALen: Integer): Integer;
  protected
    FSocket: TSocket;
    function  DoBeforOpenTCP: Boolean; virtual;  //��ʼ��TCPǰ; True: ������ʼ��; False: ��������ʼ��
    procedure DoAfterOpenTCP; virtual;
    procedure DoBeforCloseTCP; virtual;
    procedure DoAfterCloseTCP; virtual; //TCP�ر�֮��
    procedure DoErrorInfo(const AInfo: PAnsiChar); virtual;
    procedure DoRecvBuffer(const ApBuffer: PAnsiChar; const ABufferLen: Cardinal); virtual;
  private
    procedure InitAllVar;
    procedure FreeSocket;
    function  Open: Boolean;
    procedure Close;
    procedure _DoRecvBuffer;     //���̵߳���
  private
    FPort: Word;
    FActive: Boolean;
    FIP: Cardinal;
    FIsExclusitve: Boolean;
    FRecvThread: TJxdTCPRecvThread;
    FOnRecvBuffer: TOnRecvBuffer;
    procedure SetActive(const Value: Boolean);
    procedure SetIP(const Value: Cardinal);
    procedure SetPort(const Value: Word);
    procedure SetExclusitve(const Value: Boolean);
  published
    property Active: Boolean read FActive write SetActive;
    property Port: Word read FPort write SetPort;
    property IP: Cardinal read FIP write SetIP;
    property IsExclusitve: Boolean read FIsExclusitve write SetExclusitve;     //��ֹ�׽��ֱ����˼���
    property OnRecvBuffer: TOnRecvBuffer read FOnRecvBuffer write FOnRecvBuffer;
  end;
  {$M+}
  
  TJxdTCPRecvThread = class(TThread)
  private
    FOwner: TxdTCPClient;
    FhEvent: WSAEvent;
    FClose: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TxdTCPClient);
    destructor Destroy; override;
  end;

procedure RaiseWinSocketError(AErrCode: Integer; AAPIName: PChar);

implementation

{ TJxdTCPClient }
const
  CtSockAddrLen = SizeOf(TSockAddr);
  CtMaxBufferLen = 1024;

procedure RaiseError(const AErrString: string);
begin
  raise ETCPError.Create( AErrString );
end;

procedure RaiseWinSocketError(AErrCode: Integer; AAPIName: PChar);
begin
  raise ETCPError.Create( Format(sWindowsSocketError, [SysErrorMessage(AErrCode), AErrCode, AAPIName]) );
end;

procedure TxdTCPClient.Close;
begin
  if FActive then
  begin
    if FRecvThread <> nil then
      FreeAndNil(FRecvThread);
    FreeSocket;
  end;
end;

constructor TxdTCPClient.Create;
begin
  InitAllVar;
end;

destructor TxdTCPClient.Destroy;
begin
  Active := False;
  inherited;
end;


procedure TxdTCPClient.DoAfterCloseTCP;
begin

end;

procedure TxdTCPClient.DoAfterOpenTCP;
begin
end;

procedure TxdTCPClient.DoBeforCloseTCP;
begin

end;

function TxdTCPClient.DoBeforOpenTCP: Boolean;
begin
  Result := True;
end;

procedure TxdTCPClient.DoErrorInfo(const AInfo: PAnsiChar);
begin

end;

procedure TxdTCPClient.DoRecvBuffer(const ApBuffer: PAnsiChar; const ABufferLen: Cardinal);
begin
  OutputDebugStringA( ApBuffer );
  if Assigned(FOnRecvBuffer) then
    FOnRecvBuffer( Self, ApBuffer, ABufferLen );
end;

procedure TxdTCPClient.FreeSocket;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    shutdown(FSocket, SD_BOTH);
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
end;

procedure TxdTCPClient.InitAllVar;
begin
  FActive := False;
  FSocket := INVALID_SOCKET;
  FPort := 0;
  FIP := ADDR_ANY;
  FIsExclusitve := True;
  FRecvThread := nil;
end;

function TxdTCPClient.Open: Boolean;
var
  SockAddr: TSockAddr;
begin
  Result := False;
  if not FActive then
  begin
    if Port = 0 then
      RaiseError( 'must set connect server port frist' );
    FSocket := WSASocket( AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED );
    if FSocket = INVALID_SOCKET then
      RaiseWinSocketError( WSAGetLastError, 'WSASocket' );
    if FIsExclusitve and (not SetSocketExclusitveAddr( FSocket )) then
      DoErrorInfo( 'can not set exclusitve addreess. somebody can listen this network data' );

    SockAddr := InitSocketAddr( IP, Port );
    if SOCKET_ERROR = connect( FSocket, @SockAddr, CtSockAddrLen ) then
    begin
      FreeSocket;
      DoErrorInfo( 'can not connect to TCP Server' );
      Exit;
    end;
    Result := True;
    FRecvThread := TJxdTCPRecvThread.Create(Self);
  end;
end;

function TxdTCPClient.SendBuffer(var ABuffer: pChar; ALen: Integer): Integer;
begin
  if not Active then
  begin
    Result := -1;
    Exit;
  end;
  Result := send( FSocket, ABuffer^, ALen, 0 );
end;

procedure TxdTCPClient.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
    begin
      if DoBeforOpenTCP and Open then
        DoAfterOpenTCP
      else
        Exit;
    end
    else
    begin
      DoBeforCloseTCP;
      Close;
      DoAfterCloseTCP;
    end;
    FActive := Value;
  end;
end;

procedure TxdTCPClient.SetExclusitve(const Value: Boolean);
begin
  FIsExclusitve := Value;
end;

procedure TxdTCPClient.SetIP(const Value: Cardinal);
begin
  FIP := Value;
end;

procedure TxdTCPClient.SetPort(const Value: Word);
begin
  FPort := Value;
end;

procedure TxdTCPClient._DoRecvBuffer;
var
  Package: array[0..CtMaxBufferLen - 1] of AnsiChar;
  wsaBuffer: WSABUF;
  dwRecvByte, dwFlags: DWORD;
  nRecvResult: Integer;
begin
  while True do
  begin
    wsaBuffer.len := CtMaxBufferLen;
    wsaBuffer.buf := @Package;
    dwFlags := 0;
    nRecvResult := WSARecv( FSocket, @wsaBuffer, 1, dwRecvByte, dwFlags, nil, nil);
    if (nRecvResult = -1) or (dwRecvByte <= 0) then
      Break;
    DoRecvBuffer( @Package, dwRecvByte );
    Break;
  end;
end;

{ TJxdTCPRecvThread }

constructor TJxdTCPRecvThread.Create(AOwner: TxdTCPClient);
begin
  FClose := False;
  FOwner := AOwner;
  FhEvent := WSACreateEvent; 
  if FhEvent = WSA_INVALID_EVENT then            
    RaiseError( Format('TJxdTCPRecvThread.Create WSACreateEvent error,Code: %d', [WSAGetLastError]) );
  if WSAEventSelect( FOwner.FSocket, FhEvent, {FD_READ or FD_CLOSE}FD_ALL_EVENTS ) = SOCKET_ERROR then
    RaiseError( Format('TJxdTCPRecvThread.Create WSAEventSelect error,Code: %d', [WSAGetLastError()]) );
  inherited Create(False);
end;

destructor TJxdTCPRecvThread.Destroy;
begin
  Terminate;
  WSASetEvent(FhEvent);
  while not FClose do
    WaitForSingleObject( Self.Handle, 50 );
  if FhEvent <> WSA_INVALID_EVENT then
  begin
    WSACloseEvent( FhEvent );
    FhEvent := WSA_INVALID_EVENT;
  end;
  inherited;
end;

procedure TJxdTCPRecvThread.Execute;
var
  Code: Cardinal;
  NetEvent: TWSANetworkEvents;
begin
  while not Terminated do
  begin
    Code := WSAWaitForMultipleEvents(1, @FhEvent, True, 1000, False) - WSA_WAIT_EVENT_0;
    if Code = WSA_WAIT_TIMEOUT then
      Continue;
    if Terminated or (Code = WSA_WAIT_FAILED) then
    begin
      FClose := True;
      Exit;
    end;
    if 0 = WSAEnumNetworkEvents( FOwner.FSocket, FhEvent, @NetEvent ) then
    begin
      if ( (NetEvent.lNetworkEvents and FD_READ) > 0 ) and ( NetEvent.iErrorCode[FD_READ_BIT] = 0 ) then
      begin
        //�׽��������ݿɶ�
        FOwner._DoRecvBuffer;
      end
      else if ( (NetEvent.lNetworkEvents and FD_CLOSE) > 0 ) and ( NetEvent.iErrorCode[FD_CLOSE_BIT] = 0 ) then
      begin
        //�׽��ֹر�
        Break;
      end;     
    end;
  end;
  FClose := True;
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