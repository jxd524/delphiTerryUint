{
单元名称: uJxdTCPCompletionPortServer
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: 使用 WSAEventSelect IO 模式的TCP服务器
开始时间: 2010-09-26
修改时间: 2010-09-26(最后修改)

说    明：

服务器使用方式：
  首先需要子类化，处理：
    procedure OnConnection(const Ap: PClientSocketInfo; var AIsAllow: Boolean); virtual; //有新客户端连接
    procedure OnDisConnection(const Ap: PClientSocketInfo); virtual; //客户端断开
    procedure OnRecvBuffer(const Ap: PClientSocketInfo; const ApBuffer: PChar; const ABufferLen: Integer; var ANewPostRecvCount: Integer); virtual; //客户端接收到数据
    procedure OnSendBufferFinished(const Ap: PClientSocketInfo; const ApBuffer: PChar; const ABufferLen: Integer; var ANewPostRecvCount: Integer); virtual; //客户端接收到数据
    procedure OnDeletePendingClient(const Ap: PClientSocketInfo); virtual; //投递接受连接事件不够时并且客户端连接之后没有超时没有发送数据时触发

  with FServer do
  begin
    Port := 9829;
    IsExclusitve := True;
    IocpWorkThreadCount := 4;
    MaxWaitTime := 2000;
    MaxClientCount := 10000;
    MaxOverlapBufferCount := 100000;
    MaxOverlapBufferLength := 1024;
    MaxAcceptCount := 50;
    Active := True;
  end;

服务器流程
  启动服务器，IOCP的工作线程是：DoIocpWorkThread，根据不同的参数处理不同的事件。
              普通工作线程：DoListentSocketThread，当投递接受不足时，触发FD_ACCEPT事件，之后处理 OnCheckPendingTime

  _OnNewClientConnection： 当有新连接，并且新连接发送了第一块数据时，触发此事件
  _OnClientOptCompletion： 当连接有数据到来，或者此连接发送数据完成时触发
  以上两个事件为主要处理事件

测试结果：

       时间             网络                客户端数量         测试方式
2010-10-09下午      局域网内                  1000            客户连接后发送数据，服务器接收到数据返回，数据不停返回<-->

    服务器端：  参数如下
    with FServer do
    begin
      Port := 9829;
      IsExclusitve := True;
      IocpWorkThreadCount := 4;
      MaxWaitTime := 2000;
      MaxClientCount := 10000;
      MaxOverlapBufferCount := 100000;
      MaxOverlapBufferLength := 1024;
      MaxAcceptCount := 50;
      Active := True;
    end;

连接速度过慢，投递接收方式需要改变
当开多个程序进行测试时，情况明显。并且在客户端大量，同时关闭的情况，有些内存无法回收，可能的情况：
1：网络原因，服务器完全不知道客户端已经断开，此时无法回收被占用内存。网络层使用心跳（还没有添加）
2：应用程序原因：逻辑没有处理好。


}
unit uJxdTCPCompletionPortServer;

{$DEFINE Debug}

interface
uses
  Windows, Classes, SysUtils, RTLConsts, WinSock2, Forms,
  uSocketSub, uJxdMemoryManage, uJxdThread, uJxdDataStruct
  {$IFDEF Debug}, uDebugInfo {$ENDIF}
  ;

type
  TOverlapStyle = (osAccept, osRecv, osSend, osClose);

  //Completaion port key
  PClientSocketInfo = ^TClientSocketInfo;
  TClientSocketInfo = record
  private
    FSocket: TSocket;
    FLocalAddress: TSockAddr;
    FRemoteAddress: TSockAddr;
    FPostRecvCount: Integer;       //=0时，自动断开SOCKET，回收相应内存块
    FLock: TRTLCriticalSection;
  public
    function IsCloseByServer: Boolean;
    property Socket: TSocket read FSocket;
    property LocalAddress: TSockAddr read FLocalAddress;
    property RemoteAddress: TSockAddr read FRemoteAddress;
  end;

  //Overlapper Param
  POverlapBufferInfo = ^TOverlapBufferInfo;
  TOverlapBufferInfo = record
    FOverlap: TOverlapped;
    FClientSocketInfo: PClientSocketInfo;
    FStyle: TOverlapStyle;
    FBufferLen: Integer;
    FBuffer: PChar; 
  end;

  {$M+}
  TxdCompletionPortServer = class;
  TClientSocketManage = class
  public
    constructor Create(const AOwner: TxdCompletionPortServer; const AMaxCount: Integer);
    destructor  Destroy; override;

    function  GetClientSocket(var Ap: PClientSocketInfo): Boolean;
    procedure FreeClientSocket(const Ap: PClientSocketInfo);

    procedure LockClientSocket(const Ap: PClientSocketInfo; const ALock: Boolean);
  private
    FManage: TxdFixedMemoryManagerEx;
    FOwner: TxdCompletionPortServer;
    FLock: TRTLCriticalSection;
    procedure LockManage(const ALock: Boolean);
    const CtClientSocketInfoSize = SizeOf(TClientSocketInfo);
  end;

  TOverlapBufferManage = class
  public
    constructor Create(const AOwner: TxdCompletionPortServer; const ABufferLen, AMaxCount: Integer);
    destructor  Destroy; override;

    function  BufferLength: Integer;
    function  Count: Integer;
    function  GetOverlapBuffer(var Ap: POverlapBufferInfo): Boolean;
    procedure FreeOverlapBuffer(const Ap: POverlapBufferInfo);
  private
    FManage: TxdFixedMemoryManager;
    FOwner: TxdCompletionPortServer;
    FOverlapBufferSize: Integer;
    FBufferLen: Integer;
    FLock: TRTLCriticalSection;
    procedure LockManage(const ALock: Boolean);
    const CtOverlapSize = SizeOf(TOverlapped);
  end;

  TPendingClientManage = class
  public
    constructor Create(const AOwner: TxdCompletionPortServer);
    destructor  Destroy; override;

    function  Count: Integer;
    procedure CheckPending;
    function  AddPending(const Ap: POverlapBufferInfo): Boolean;
    procedure DeletePending(const Ap: POverlapBufferInfo);
  private
    FManage: THashArray;
    FOwner: TxdCompletionPortServer;
    FLock: TRTLCriticalSection;
    procedure LockManage(const ALock: Boolean);
    procedure OnLoopManage(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel: Boolean; var AFindNext: Boolean);
  end;

  TxdCompletionPortServer = class
  public
    constructor Create;
    destructor  Destroy; override;
  protected
    {正常情况出现的情况}
    procedure OnConnection(const Ap: PClientSocketInfo; var AIsAllow: Boolean); virtual; //有新客户端连接
    procedure OnDisConnection(const Ap: PClientSocketInfo); virtual; //客户端断开
    procedure OnRecvBuffer(const Ap: PClientSocketInfo; const ApBuffer: PChar; const ABufferLen: Integer; var ANewPostRecvCount: Integer); virtual; //客户端接收到数据
    procedure OnSendBufferFinished(const Ap: PClientSocketInfo; const ApBuffer: PChar; const ABufferLen: Integer; var ANewPostRecvCount: Integer); virtual; //客户端接收到数据

    {异常情况出现的情况}
    procedure OnDeletePendingClient(const Ap: PClientSocketInfo); virtual; //投递接受连接事件不够时并且客户端连接之后没有超时没有发送数据时触发

    {给子类调用的常用函数}
    procedure PostAcceptEx(const ACount: Integer); //投递指定数量的socket准备接收新连接
    procedure PostRecvEx(const Ap: PClientSocketInfo; const ACount: Integer); //为指定的客户端投递接收数据缓存
    function  PostSendEx(const pSocket: PClientSocketInfo; const ABuffer: PChar; const ABufferLen: Integer): Boolean;

    procedure CloseConnectClient(const Ap: PClientSocketInfo); //直接关闭已经连接的客户端
    procedure ActiveBefor; virtual;
    procedure ActiveAfter; virtual;
    procedure UnActiveBefor; virtual;
    procedure UnActiveAfter; virtual;

    function  CreateOverLappedSocket: TSocket; inline;
    function  CreateIOCP: THandle; inline;
    function  RelatingToCompletionPort(const ASocket: TSocket; const ACompletionKey: Cardinal): Boolean;
    procedure ReuseSocket(var ASocket: TSocket);
    procedure CloseSocketEx(var ASocket: TSocket);
    function  GetConnectTime(const ASocket: TSocket): Integer;
    const IocpKeyStyle_ListenSocket = $FFFF;
    const IocpKeyStyle_ClientSocket = $EEEE;
    const IocpKeyStyle_CancelIO     = $EEFF;
 protected
   {Winsock2 扩展函数指针函数}
    FAcceptEx: TAcceptEx; //AcceptEx函数地址
    FGetAcceptSockAddrs: TGetAcceptExSockAddrs;  //GetAcceptExSockaddrs函数地址
    FDisConnectEx: TDisconnectEx; //DisConnectEx函数地址;关闭套接字,并让套接字可被重用
    {Winsock2 扩展函数}
    procedure DeleteSocketExFunction;
    procedure LoadWinSocetkExFunction;
    function  __AcceptEx(const ANewSock: TSocket; const AOverlapped: POverlapped; const ABuffer: PChar; const ABufferLen: DWORD): Boolean;
    procedure __GetAcceptSockAddr(const AFirstBuffer: PChar; const ABufLen: Integer; var ALocalAddr, ARemoteAddr: TSockAddrIn);
    function  __DisConnectEx(const ASocket: TSocket; const lpOverlapped: POverlapped): Boolean;
    function  __RecvBufferEx(const ASocket: TSocket; const AOverlapped: POverlapped; const ABuffer: PChar; const ABufferLen: DWORD): Boolean;
    function  __SendBufferEx(const ASocket: TSocket; const AOverlapped: POverlapped; const ABuffer: PChar; const ABufferLen: DWORD): Boolean;
    procedure __PostNotifyIO; inline;
  private
    FCompletionPort: THandle; //完成端口
    FCurIocpThreadCount: Integer; //当前工作在完成端口上的线程数量
    FListenSocket: TSocket;   //监听socket
    FListenEvent: Cardinal;   //监听FListenSocket的FD_ACCEPT事件
    FListeningEvent: Boolean; //是否正在运行机制监听事件线程
    FClientSocketManage: TClientSocketManage;   //客户端套接字信息管理
    FOverlapBufferManage: TOverlapBufferManage; //投递内存管理
    FPendingManage: TPendingClientManage;       //未决客户端管理



    procedure ActiveServer;
    procedure UnActiveServer;

    procedure WinSocketError(AErrCode: Integer; AAPIName: PChar);
    procedure ReclaimClientSocket(const Ap: PClientSocketInfo; const ALock: Boolean);
    procedure ReclaimRecvOverlapBuffer(const Ap: POverlapBufferInfo);
    procedure OnError(const AErrorInfo: string);
    procedure OnCheckPendingTime(const Ap: POverlapBufferInfo; var ADel: Boolean);  //由DoListentSocketThread调用
    procedure OnCancelPendingIo(const Ap: POverlapBufferInfo); //取消IO后由 DoIocpWorkThread 调用 仅当关闭服务器时被调用
    procedure _OnNewClientConnection(const Ap: POverlapBufferInfo; const ABufLen: Cardinal);
    procedure _OnClientOptCompletion(const Ap: POverlapBufferInfo; const ABufLen: Cardinal);
    procedure _OnRecvClientBuffer(const Ap: POverlapBufferInfo);
    procedure _OnSendFinishedBuffer(const Ap: POverlapBufferInfo);

    procedure DoIocpWorkThread;      //服务完成端口的工作线程
    procedure DoListentSocketThread; //监听请求连接事件线程，投递来接收的socket不够或短时间有大量连接或有恶意连接时
  private
    FIsExclusitve: Boolean;
    FPort: Word;
    FActive: Boolean;
    FIP: Cardinal;
    FIocpThreadCount: Integer;
    FListenCount: Integer;
    FMaxWaitTime: Cardinal;
    FMaxClientCount: Integer;
    FMaxOverlapBufferCount: Integer;
    FMaxOverlapBufferLength: Integer;
    FMaxAcceptCount: Integer;
    FMaxPendingTime: Integer;
    procedure SetActive(const Value: Boolean);
    procedure SetExclusitve(const Value: Boolean);
    procedure SetIP(const Value: Cardinal);
    procedure SetPort(const Value: Word);
    procedure SetIocpThreadCount(const Value: Integer);
    procedure SetListenCount(const Value: Integer);
    procedure SetMaxWaitTime(const Value: Cardinal);
    procedure SetMaxClientCount(const Value: Integer);
    procedure SetMaxOverlapBufferCount(const Value: Integer);
    procedure SetMaxOverlapBufferLength(const Value: Integer);
    procedure SexMaxAcceptCount(const Value: Integer);
    procedure SetMaxPendingTime(const Value: Integer);
    function  GetPAC: Integer;
    function GetOBC: Integer;
  published
    property Active: Boolean read FActive write SetActive;
    property Port: Word read FPort write SetPort;
    property IP: Cardinal read FIP write SetIP;
    property ListenCount: Integer read FListenCount write SetListenCount default 0;
    property IsExclusitve: Boolean read FIsExclusitve write SetExclusitve;     //防止套接字被别人监听

    {服务器设置}
    property IocpWorkThreadCount: Integer read FIocpThreadCount write SetIocpThreadCount; //IOCP工作线程数量
    property MaxWaitTime: Cardinal read FMaxWaitTime write SetMaxWaitTime default 3000;  //所有等待函数最长等待时长
    property MaxClientCount: Integer read FMaxClientCount write SetMaxClientCount; //最大允许客户端数量
    property MaxOverlapBufferCount: Integer read FMaxOverlapBufferCount write SetMaxOverlapBufferCount; //最大允许重叠缓存块数量
    property MaxOverlapBufferLength: Integer read FMaxOverlapBufferLength write SetMaxOverlapBufferLength; //每块缓存最大长度
    property MaxAcceptCount: Integer read FMaxAcceptCount write SexMaxAcceptCount; //最大允许同时连接数量
    property MaxPendingTime: Integer read FMaxPendingTime write SetMaxPendingTime; //连接后最大允许不发数据间隔时间（当投递接收不够用时）

    {数据统计}
    property CurPostAcceptCount: Integer read GetPAC;    //当前投递接收数量
    property CurOverlapBufferCount: Integer read GetOBC; //当前可用缓存数量
  end;
  {$M-}

implementation

const
  CtSockAddLength = SizeOf(TSockAddr);

procedure RaiseWinSocketError(AErrCode: Integer; AAPIName: PChar);
begin
  raise Exception.Create( Format(sWindowsSocketError, [SysErrorMessage(AErrCode), AErrCode, AAPIName]) );
end;

{ TxdCompletionPortServer }

function TxdCompletionPortServer.__AcceptEx(const ANewSock: TSocket; const AOverlapped: POverlapped; const ABuffer: PChar;
  const ABufferLen: DWORD): Boolean;
var
  dwByteRece: DWORD;
begin
  Result := False;
  if (FListenSocket <> INVALID_SOCKET) and Assigned(FAcceptEx) then
  begin
    Result := FAcceptEx( FListenSocket, ANewSock, ABuffer, ABufferLen - (CtSockAddLength + 16) * 2,
                         CtSockAddLength + 16, CtSockAddLength + 16, @dwByteRece, AOverlapped);
    if (not Result) and ( WSAGetLastError <> WSA_IO_PENDING ) then
      Result := False
    else
      Result := True;
  end;
end;

procedure TxdCompletionPortServer.ActiveAfter;
begin

end;

procedure TxdCompletionPortServer.ActiveBefor;
begin

end;

procedure TxdCompletionPortServer.ActiveServer;
var
  SockAddr: TSockAddr;
  i: Integer;
begin
  try
    ActiveBefor;
    DeleteSocketExFunction;
    //创建完成端口
    FCompletionPort := CreateIOCP;
    //创建重叠监听套接字
    FListenSocket := CreateOverLappedSocket;
    if FListenSocket = INVALID_SOCKET then
    begin
      WinSocketError( WSAGetLastError, 'socket' );
      RaiseWinSocketError( WSAGetLastError, 'socket' );
    end;
    //监听连接请求事件
    FListenEvent := WSACreateEvent;
    if SOCKET_ERROR = WSAEventSelect( FListenSocket, FListenEvent, FD_ACCEPT ) then
    begin
      WinSocketError( WSAGetLastError, 'WSAEventSelect' );
      RaiseWinSocketError( WSAGetLastError, 'WSAEventSelect' );
    end;
    FListeningEvent := True;
    RunningByThread( DoListentSocketThread );
    //载入socket扩展函数
    LoadWinSocetkExFunction;
    //独占式套接字
    if FIsExclusitve and (not SetSocketExclusitveAddr( FListenSocket )) then
    begin
      WinSocketError( WSAGetLastError, 'SetSocketExclusitveAddr' );
      RaiseWinSocketError( WSAGetLastError, 'SetSocketExclusitveAddr' );
    end;
    //配置本地绑定地址
    SockAddr := InitSocketAddr( IP, Port );
    //绑定地址
    if SOCKET_ERROR = bind( FListenSocket, @SockAddr, CtSockAddLength ) then
    begin
      WinSocketError( WSAGetLastError, 'bind' );
      RaiseWinSocketError( WSAGetLastError, 'bind' );
    end;
    //开始监听
    if ListenCount <= 0 then
      i := SOMAXCONN
    else
      i := ListenCount;
    if SOCKET_ERROR = listen( FListenSocket, i ) then
    begin
      WinSocketError( WSAGetLastError, 'listen' );
      RaiseWinSocketError( WSAGetLastError, 'listen' );
    end;
    //工作在完成端口上的线程
    FCurIocpThreadCount := FIocpThreadCount;
    for i := 0 to FIocpThreadCount - 1 do
      RunningByThread( DoIocpWorkThread );

    //将监听Socket与Iocp关联起来
    RelatingToCompletionPort( FListenSocket, IocpKeyStyle_ListenSocket );
    //客户端socket池创建
    FClientSocketManage := TClientSocketManage.Create( Self, MaxClientCount );
    //投递缓存池创建
    FOverlapBufferManage := TOverlapBufferManage.Create( Self, MaxOverlapBufferLength, MaxOverlapBufferCount );
    //未决客户端管理创建
    FPendingManage := TPendingClientManage.Create( Self );

    //开始投递
    PostAcceptEx( MaxAcceptCount );
    ActiveAfter;
    FActive := True;
  except
    UnActiveServer;
  end;
end;

procedure TxdCompletionPortServer.CloseConnectClient(const Ap: PClientSocketInfo);
var
  p: POverlapBufferInfo;
begin
  if FOverlapBufferManage.GetOverlapBuffer(p) then
  begin
    p^.FClientSocketInfo := Ap;
    p^.FStyle := osClose;
    shutdown( Ap^.FSocket, SD_BOTH );
    if not __DisConnectEx( Ap^.FSocket, @p^.FOverlap ) then
      FOverlapBufferManage.FreeOverlapBuffer( p );
  end;
end;

procedure TxdCompletionPortServer.CloseSocketEx(var ASocket: TSocket);
begin
  if ASocket <> INVALID_SOCKET then
  begin
    shutdown( ASocket, SD_BOTH );
    closesocket( ASocket );
    ASocket := INVALID_SOCKET;
  end;
end;

constructor TxdCompletionPortServer.Create;
var
  sysInfo: TSystemInfo;
begin
  GetSystemInfo( sysInfo );
  FIocpThreadCount := sysInfo.dwNumberOfProcessors * 2 + 2;
  FIsExclusitve := False;
  FPort := 9239;
  FActive := False;
  FIP := 0;
  FCompletionPort := INVALID_HANDLE_VALUE;
  FListenSocket := INVALID_SOCKET;
  FListenCount := 0;
  FMaxWaitTime := 3000;
  FMaxClientCount := 1000;
  FMaxOverlapBufferCount := 1000;
  FMaxOverlapBufferLength := 1024;
  FMaxAcceptCount := 10;
  FAcceptEx := nil;
  FDisConnectEx := nil;
  FGetAcceptSockAddrs := nil;
  FListenEvent := INVALID_HANDLE_VALUE;
  FMaxPendingTime := 3000;
end;

function TxdCompletionPortServer.CreateIOCP: THandle;
begin
  Result := CreateIoCompletionPort( INVALID_HANDLE_VALUE, 0, 0, 0 );
end;

function TxdCompletionPortServer.CreateOverLappedSocket: TSocket;
begin
  Result := WSASocket( AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED );
end;

procedure TxdCompletionPortServer.DeleteSocketExFunction;
begin
  FAcceptEx := nil;
  FDisConnectEx := nil;
  FGetAcceptSockAddrs := nil;
end;

destructor TxdCompletionPortServer.Destroy;
begin
  Active := False;
  inherited;
end;

function TxdCompletionPortServer.__DisConnectEx(const ASocket: TSocket; const lpOverlapped: POverlapped): Boolean;
begin
  Result := False;
  if Assigned(FDisConnectEx) then
    Result := FDisConnectEx( ASocket, lpOverlapped, TF_REUSE_SOCKET, 0 ) or (WSAGetLastError = ERROR_IO_PENDING);
end;

procedure TxdCompletionPortServer.DoIocpWorkThread;
var
  TransByte: Cardinal;
  nErr, CompletionKey: Cardinal;
  pOverlapData: POverlapped;
  bResult: Boolean;
begin
  try
    while True do
    begin
      bResult := GetQueuedCompletionStatus( FCompletionPort, TransByte, CompletionKey, pOverlapData, FMaxWaitTime );
      if not Active then Break;
      if not bResult then
      begin
        nErr := GetLastError;
        case nErr of
          ERROR_OPERATION_ABORTED: //取消重叠IO
          begin
            case CompletionKey of
              IocpKeyStyle_ListenSocket: OnCancelPendingIo( POverlapBufferInfo(pOverlapData) ); //取消所有IO
              IocpKeyStyle_ClientSocket:  ReclaimRecvOverlapBuffer( POverlapBufferInfo(pOverlapData) ); //服务器取消客户端重叠IO
            end;
          end;
          ERROR_NETNAME_DELETED://指定的网络名不再可用。
          begin
            if pOverlapData <> nil then
            begin
              if CompletionKey = IocpKeyStyle_ListenSocket then
                FPendingManage.DeletePending( POverlapBufferInfo(pOverlapData) );
              ReclaimRecvOverlapBuffer( POverlapBufferInfo(pOverlapData) ); //客户端断网
            end;
          end;
        end;
        Continue;
      end;

      case CompletionKey of
        IocpKeyStyle_ListenSocket: _OnNewClientConnection( POverlapBufferInfo(pOverlapData), TransByte );
        IocpKeyStyle_ClientSocket: _OnClientOptCompletion( POverlapBufferInfo(pOverlapData), TransByte );
        IocpKeyStyle_CancelIO:    ;   //特殊IO包，不需要处理 仅是通知IOCP处理信息
        else
          OnError( '无法识别的完成键' );
      end;
    end;
  finally
    InterlockedDecrement( FCurIocpThreadCount );
  end;
end;

procedure TxdCompletionPortServer.DoListentSocketThread;
var
  waitResult: Cardinal;
  netEvent: TWSANetworkEvents;
begin
  try
    while True do
    begin
      waitResult := WaitForSingleObject( FListenEvent, MaxWaitTime );
      if not Active then Break;
      if waitResult = WAIT_TIMEOUT then Continue;

      if SOCKET_ERROR = WSAEnumNetworkEvents( FListenSocket, FListenEvent, @netEvent ) then
      begin
        WinSocketError( WSAGetLastError, 'WSAEnumNetworkEvents' );
        Active := False;
        Break;
      end;

      if (netEvent.lNetworkEvents and FD_ACCEPT) <> 0 then
      begin
        //有用户连接上服务器
        if netEvent.iErrorCode[FD_ACCEPT_BIT] <> 0 then
        begin
          OnError( Format( 'FD_ACCEPT failed with error %d',[netEvent.iErrorCode[FD_ACCEPT_BIT] ] ));
          Continue;
        end;
        OutputDebugString( 'FD_ACCEPT，PostAcceptEx' );
        PostAcceptEx( MaxAcceptCount );
        FPendingManage.CheckPending;
      end
      else
        OutputDebugString( '未知信息，不应该出现的情况' );
    end;
  finally
    FListeningEvent := False;
  end;
end;

procedure TxdCompletionPortServer.__GetAcceptSockAddr(const AFirstBuffer: PChar; const ABufLen: Integer; var ALocalAddr,
  ARemoteAddr: TSockAddrIn);
var
  dwLocalLen, dwRemoteLen: DWORD;
  pLocal, pRemote: pSockAddr;
begin
  if Assigned(FGetAcceptSockAddrs) then
  begin
    //ABufLen = AccepteEx中的 BufferLen
    FGetAcceptSockAddrs( Pointer(AFirstBuffer), ABufLen - (CtSockAddLength + 16) * 2, CtSockAddLength + 16,
                         CtSockAddLength + 16, pLocal, @dwLocalLen,  pRemote, @dwRemoteLen);
    Move( pLocal^, ALocalAddr, dwLocalLen );
    Move( pRemote^, ARemoteAddr, dwRemoteLen );
  end;
end;

procedure TxdCompletionPortServer.__PostNotifyIO;
begin
  PostQueuedCompletionStatus( FCompletionPort, 0, IocpKeyStyle_CancelIO, nil );
end;

function TxdCompletionPortServer.GetConnectTime(const ASocket: TSocket): Integer;
var
  nSeconds, nLen, nErr: Integer;
begin
  nSeconds := 0;
  nLen := sizeof(nSeconds);
  nErr := getsockopt( ASocket, SOL_SOCKET, SO_CONNECT_TIME, pChar(@nSeconds), nLen);
  if nErr <> NO_ERROR then
    Result := -1
  else
    Result := nSeconds;
end;

function TxdCompletionPortServer.GetOBC: Integer;
begin
  if Active then
    Result := FOverlapBufferManage.Count
  else
    Result := 0;
end;

function TxdCompletionPortServer.GetPAC: Integer;
begin
  if Active then
    Result := FPendingManage.Count
  else
    Result := 0;
end;

procedure TxdCompletionPortServer.LoadWinSocetkExFunction;
begin
  if FListenSocket = INVALID_SOCKET then
  begin
    OnError( 'FListenSocket = INVALID_SOCKET' );
    Exit;
  end;
  if not Assigned(FAcceptEx) then
    FAcceptEx := WSAGetExtensionFunctionPointer( FListenSocket, WSAID_ACCEPTEX );
  if not Assigned(FGetAcceptSockAddrs) then
    FGetAcceptSockAddrs := WSAGetExtensionFunctionPointer( FListenSocket, WSAID_GETACCEPTEXSOCKADDRS );
  if not Assigned(FDisConnectEx) then
    FDisConnectEx := WSAGetExtensionFunctionPointer( FListenSocket, WSAID_DISCONNECTEX );
end;

procedure TxdCompletionPortServer.OnCancelPendingIo(const Ap: POverlapBufferInfo);
begin
  FPendingManage.DeletePending( Ap );
  FClientSocketManage.FreeClientSocket( Ap^.FClientSocketInfo );
  FOverlapBufferManage.FreeOverlapBuffer( Ap );
end;

procedure TxdCompletionPortServer.OnCheckPendingTime(const Ap: POverlapBufferInfo; var ADel: Boolean);
begin
  ADel := GetConnectTime( Ap^.FClientSocketInfo^.FSocket ) >= MaxPendingTime;
  if ADel then
  begin
    OnDeletePendingClient( Ap^.FClientSocketInfo );
    ReclaimClientSocket( Ap^.FClientSocketInfo, True );
  end;
end;

procedure TxdCompletionPortServer.OnConnection(const Ap: PClientSocketInfo; var AIsAllow: Boolean);
begin

end;

procedure TxdCompletionPortServer.OnDeletePendingClient(const Ap: PClientSocketInfo);
begin
  OutputDebugString( PChar( 'OnDeletePendingClient' ) );
end;

procedure TxdCompletionPortServer.OnDisConnection(const Ap: PClientSocketInfo);
begin

end;

procedure TxdCompletionPortServer.OnError(const AErrorInfo: string);
begin
  OutputDebugString( Pchar(AErrorInfo) );
  {$IFDEF Debug}
  _Log( AErrorInfo, 'xdCompletionPortServer.txt' );
  {$ENDIF}
end;

procedure TxdCompletionPortServer.OnRecvBuffer(const Ap: PClientSocketInfo; const ApBuffer: PChar; const ABufferLen: Integer; var ANewPostRecvCount: Integer);
begin
  ApBuffer[ ABufferLen ] := #0;
  OutputDebugString( ApBuffer );
  ANewPostRecvCount := 1;
end;

procedure TxdCompletionPortServer.OnSendBufferFinished(const Ap: PClientSocketInfo; const ApBuffer: PChar;
  const ABufferLen: Integer; var ANewPostRecvCount: Integer);
begin

end;

procedure TxdCompletionPortServer.PostAcceptEx(const ACount: Integer);
var
  i: Integer;
  pSocket: PClientSocketInfo;
  pBuffer: POverlapBufferInfo;
begin
  for i := 0 to ACount - 1 do
  begin
    if not FClientSocketManage.GetClientSocket(pSocket) then
    begin
      OnError( '无法继续投递套接字进行监听, GetClientSocket失败, 函数: PostAcceptEx' );
      Break;
    end;
    if not FOverlapBufferManage.GetOverlapBuffer(pBuffer) then
    begin
      FClientSocketManage.FreeClientSocket( pSocket );
      OnError( '无法继续投递套接字进行监听, GetOverlapBuffer, 函数: PostAcceptEx' );
      Break;
    end;
    pSocket^.FPostRecvCount := 1;
    pBuffer^.FClientSocketInfo := pSocket;
    pBuffer^.FStyle := osAccept;
    if __AcceptEx( pSocket^.FSocket, @pBuffer^.FOverlap, pBuffer^.FBuffer, pBuffer^.FBufferLen ) then
    begin
      if not FPendingManage.AddPending(pBuffer) then
      begin
        OnError( '无法将投递AcceptEx的内存对象加入到未决表中，此内存结点无法进行检查，当前未决表长度：' + IntToStr(FPendingManage.Count) );
        //无法检查，但并不影响使用，如果不是恶意连接（只连接不发数据）。不会出问题，否则服务器资源可能当恶意消光
      end;
    end
    else
    begin
      FClientSocketManage.FreeClientSocket( pSocket );
      FOverlapBufferManage.FreeOverlapBuffer( pBuffer );
      OnError( PChar(Format('AcceptEx失败: %d, 函数: PostAcceptEx', [WSAGetLastError])) );
    end;
  end;
end;

procedure TxdCompletionPortServer.PostRecvEx(const Ap: PClientSocketInfo; const ACount: Integer);
var
  i: Integer;
  pBuffer: POverlapBufferInfo;
begin
  for i := 0 to ACount - 1 do
  begin
    if not FOverlapBufferManage.GetOverlapBuffer(pBuffer) then
    begin
      OnError( '无可用缓冲区进行投递接收数据, PostRecvEx失败' );
      Break;
    end;
    pBuffer^.FClientSocketInfo := Ap;
    pBuffer^.FStyle := osRecv;
    if __RecvBufferEx( Ap^.FSocket, @pBuffer^.FOverlap, pBuffer^.FBuffer, pBuffer^.FBufferLen ) then
    begin
      FClientSocketManage.LockClientSocket( Ap, True );
      try
        Inc( ap^.FPostRecvCount );
      finally
        FClientSocketManage.LockClientSocket( Ap, False );
      end;
    end
    else
    begin
      FOverlapBufferManage.FreeOverlapBuffer( pBuffer );
      OnError( PChar(Format('PostRecvEx: %d', [WSAGetLastError])) );
    end;
  end;
end;

function TxdCompletionPortServer.PostSendEx(const pSocket: PClientSocketInfo; const ABuffer: PChar; const ABufferLen: Integer): Boolean;
var
  p: POverlapBufferInfo;
begin
  Result := False;
  if (not Active) or (ABufferLen > FOverlapBufferManage.BufferLength) or (ABufferLen < 0) then Exit;
  if not FOverlapBufferManage.GetOverlapBuffer(p) then
  begin
    OnError( '无可用缓冲区进行发送数据, PostSendEx失败' );
    Exit;
  end;
  p^.FClientSocketInfo := pSocket;
  p^.FBufferLen := ABufferLen;
  p^.FStyle := osSend;
  Move( ABuffer^, p^.FBuffer^, ABufferLen );
  Result := __SendBufferEx( pSocket^.FSocket, @p^.FOverlap, p^.FBuffer, p^.FBufferLen );
  if not Result then
  begin
    FOverlapBufferManage.FreeOverlapBuffer( p );
    OnError( PChar(Format('PostSendEx: %d', [WSAGetLastError])) );
  end;
end;

function TxdCompletionPortServer.__RecvBufferEx(const ASocket: TSocket; const AOverlapped: POverlapped; const ABuffer: PChar;
  const ABufferLen: DWORD): Boolean;
var
  wsaBuffer: WSABUF;
  dwRecvByte, dwFlags: DWORD;
  nRecvResult: Integer;
begin
  wsaBuffer.len := ABufferLen;
  wsaBuffer.buf := ABuffer;
  dwFlags := 0;
  nRecvResult := WSARecv( ASocket, @wsaBuffer, 1, dwRecvByte, dwFlags, LPWSAOVERLAPPED(AOverlapped), nil);
  Result := ( (nRecvResult = SOCKET_ERROR) and (WSAGetLastError = WSA_IO_PENDING) ) or (nRecvResult = 0); 
end;

procedure TxdCompletionPortServer.ReclaimClientSocket(const Ap: PClientSocketInfo; const ALock: Boolean);
begin
  if ALock then
    FClientSocketManage.LockClientSocket( Ap, True );
  try
    OnDisConnection( Ap );
    ReuseSocket( Ap^.FSocket );
//    CloseConnectClient( Ap );
    FClientSocketManage.FreeClientSocket( Ap );
  finally
    if ALock then
      FClientSocketManage.LockClientSocket( Ap, False );
  end;
end;

procedure TxdCompletionPortServer.ReclaimRecvOverlapBuffer(const Ap: POverlapBufferInfo);
var
  pSocket: PClientSocketInfo;
begin
  pSocket := Ap^.FClientSocketInfo;
  FOverlapBufferManage.FreeOverlapBuffer( Ap );
  FClientSocketManage.LockClientSocket( pSocket, True );
  try
    if pSocket^.FPostRecvCount <= 0 then
    begin
      OutputDebugString( 'ReclaimRecvOverlapBuffer error' );
      Exit; //已经被释放
    end;
    Dec( pSocket^.FPostRecvCount );
    if pSocket^.FPostRecvCount = 0 then
      ReclaimClientSocket( pSocket, False );
  finally
    FClientSocketManage.LockClientSocket( pSocket, False );
  end;
end;

function TxdCompletionPortServer.RelatingToCompletionPort(const ASocket: TSocket; const ACompletionKey: Cardinal): Boolean;
begin
  Result := False;
  if FCompletionPort <> INVALID_HANDLE_VALUE then
    Result := CreateIoCompletionPort( ASocket, FCompletionPort, ACompletionKey, 0 ) <> 0;
end;

procedure TxdCompletionPortServer.ReuseSocket(var ASocket: TSocket);
begin
  CloseSocketEx( ASocket );
  ASocket := CreateOverLappedSocket;
end;

function TxdCompletionPortServer.__SendBufferEx(const ASocket: TSocket; const AOverlapped: POverlapped; const ABuffer: PChar;
  const ABufferLen: DWORD): Boolean;
var
  wsaBuffer: WSABUF;
  dwSendByte, dwFlags: DWORD;
  nRecvResult: Integer;
begin
  wsaBuffer.len := ABufferLen;
  wsaBuffer.buf := ABuffer;
  dwFlags := 0;
  nRecvResult := WSASend( ASocket, @wsaBuffer, 1, dwSendByte, dwFlags, LPWSAOVERLAPPED(AOverlapped), nil);
  Result := ( (nRecvResult = SOCKET_ERROR) and (WSAGetLastError = WSA_IO_PENDING) ) or (nRecvResult = 0); 
end;

procedure TxdCompletionPortServer.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      ActiveServer
    else
      UnActiveServer;
  end;
end;

procedure TxdCompletionPortServer.SetMaxClientCount(const Value: Integer);
begin
  if not Active and (Value > 0) then
    FMaxClientCount := Value;
end;

procedure TxdCompletionPortServer.SetMaxOverlapBufferCount(const Value: Integer);
begin
  if not Active and (Value > 0) then
    FMaxOverlapBufferCount := Value;
end;

procedure TxdCompletionPortServer.SetMaxOverlapBufferLength(const Value: Integer);
begin
  if not Active and (Value > 0) then
    FMaxOverlapBufferLength := Value;   
end;

procedure TxdCompletionPortServer.SetMaxPendingTime(const Value: Integer);
begin
  if Value > 0 then
    FMaxPendingTime := Value;
end;

procedure TxdCompletionPortServer.SetMaxWaitTime(const Value: Cardinal);
begin
  FMaxWaitTime := Value;
end;

procedure TxdCompletionPortServer.SetExclusitve(const Value: Boolean);
begin
  if not Active then
    FIsExclusitve := Value;
end;

procedure TxdCompletionPortServer.SetIocpThreadCount(const Value: Integer);
begin
  if not Active and (Value > 0) then
    FIocpThreadCount := Value;
end;

procedure TxdCompletionPortServer.SetIP(const Value: Cardinal);
begin
  if not Active then
    FIP := Value;
end;

procedure TxdCompletionPortServer.SetListenCount(const Value: Integer);
begin
  if not Active and (Value > 0) then
    FListenCount := Value;
end;

procedure TxdCompletionPortServer.SetPort(const Value: Word);
begin
  if not Active then
    FPort := Value;
end;

procedure TxdCompletionPortServer.SexMaxAcceptCount(const Value: Integer);
begin
  if Value > 0 then
    FMaxAcceptCount := Value;
end;

procedure TxdCompletionPortServer.UnActiveAfter;
begin

end;

procedure TxdCompletionPortServer.UnActiveBefor;
begin

end;

procedure TxdCompletionPortServer.UnActiveServer;
begin
  try
    UnActiveBefor;
    //释放监听套接字
    CloseSocketEx( FListenSocket );
    //取消关联
    __PostNotifyIO;
    
    while FPendingManage.Count > 0 do
    begin
      Sleep( 10 );
      Application.ProcessMessages;
    end;
    
    FActive := False;
    //释放完成端口
    while FCurIocpThreadCount > 0 do
    begin
      Sleep( 10 );
      Application.ProcessMessages;
    end;
    while FListeningEvent do
    begin
      Sleep( 10 );
      Application.ProcessMessages;
    end;
    if FCompletionPort <> INVALID_HANDLE_VALUE then
    begin
      CloseHandle( FCompletionPort );
      FCompletionPort := INVALID_HANDLE_VALUE;
    end;
    if FListenEvent <> INVALID_HANDLE_VALUE then
    begin
      CloseHandle( FListenEvent );
      FListenEvent := INVALID_HANDLE_VALUE;
    end;

    FClientSocketManage.Free;
    FOverlapBufferManage.Free;
    FPendingManage.Free;
    UnActiveAfter;
  except
  end;
end;

procedure TxdCompletionPortServer.WinSocketError(AErrCode: Integer; AAPIName: PChar);
begin
  OnError( Format(sWindowsSocketError, [SysErrorMessage(AErrCode), AErrCode, AAPIName]) );
end;


procedure TxdCompletionPortServer._OnClientOptCompletion(const Ap: POverlapBufferInfo; const ABufLen: Cardinal);
begin
  Ap^.FBufferLen := ABufLen;
  case Ap^.FStyle of
    osRecv: _OnRecvClientBuffer( Ap );
    osSend: _OnSendFinishedBuffer( Ap );
    osClose: OutputDebugString( 'aa' );
  end;
end;

procedure TxdCompletionPortServer._OnNewClientConnection(const Ap: POverlapBufferInfo; const ABufLen: Cardinal);
var
  bAllow: Boolean;
begin
  FPendingManage.DeletePending( Ap );
  if FPendingManage.Count < MaxAcceptCount then
    PostAcceptEx( 1 );
  __GetAcceptSockAddr( Ap^.FBuffer, Ap^.FBufferLen, Ap^.FClientSocketInfo^.FLocalAddress, Ap^.FClientSocketInfo^.FRemoteAddress );
  bAllow := True;
  OnConnection( Ap^.FClientSocketInfo, bAllow );
  if not bAllow then
  begin
    ReuseSocket( Ap^.FClientSocketInfo^.FSocket ); //直接断开，不重用SOCKET，防止恶意连接
    FClientSocketManage.FreeClientSocket( Ap^.FClientSocketInfo );
    FOverlapBufferManage.FreeOverlapBuffer( Ap );
    Exit;
  end;
  RelatingToCompletionPort( Ap^.FClientSocketInfo^.FSocket, IocpKeyStyle_ClientSocket );
  Ap^.FBufferLen := ABufLen;
  _OnRecvClientBuffer( Ap );
end;

procedure TxdCompletionPortServer._OnRecvClientBuffer(const Ap: POverlapBufferInfo);
var
  nPostRectCount: Integer;
begin
  nPostRectCount := 1;
  OnRecvBuffer( Ap^.FClientSocketInfo, Ap^.FBuffer, Ap^.FBufferLen, nPostRectCount );
  if nPostRectCount > 0 then PostRecvEx( Ap^.FClientSocketInfo, nPostRectCount );
  ReclaimRecvOverlapBuffer( Ap );
end;

procedure TxdCompletionPortServer._OnSendFinishedBuffer(const Ap: POverlapBufferInfo);
var
  nPostRectCount: Integer;
begin
  nPostRectCount := 0;
  OnSendBufferFinished( Ap^.FClientSocketInfo, Ap^.FBuffer, Ap^.FBufferLen, nPostRectCount );
  if nPostRectCount > 0 then PostRecvEx( Ap^.FClientSocketInfo, nPostRectCount );
  FOverlapBufferManage.FreeOverlapBuffer( Ap );
end;

{ TClientSocketManage }

constructor TClientSocketManage.Create(const AOwner: TxdCompletionPortServer; const AMaxCount: Integer);
var
  i: Integer;
  p: PClientSocketInfo;
begin
  FOwner := AOwner;
  FManage := TxdFixedMemoryManagerEx.Create( CtClientSocketInfoSize, AMaxCount );
  for i := 0 to AMaxCount - 1 do
  begin
    p := nil;
    FManage.GetMem( Pointer(p) );
    with p^ do
    begin
      FSocket := FOwner.CreateOverLappedSocket;
      FLocalAddress.sin_port := 0;
      FRemoteAddress.sin_port := 0;
      FPostRecvCount := 0;
      InitializeCriticalSection( FLock );
    end;
    FManage.FreeMem( p );
  end;
  InitializeCriticalSection( FLock );
end;

destructor TClientSocketManage.Destroy;
var
  i: Integer;
  p: PClientSocketInfo;
begin
  for i := 0 to FManage.Capacity - 1 do
  begin
    p := FManage.Item(i);
    if Assigned(p) then
    begin
      FOwner.CloseSocketEx( p^.FSocket );
      DeleteCriticalSection( p^.FLock );
    end;
  end;
  FManage.Free;
  DeleteCriticalSection( FLock );
end;

procedure TClientSocketManage.FreeClientSocket(const Ap: PClientSocketInfo);
begin
  LockManage( True );
  try
    FManage.FreeMem( Ap );
  finally
    LockManage( False );
  end;
end;

function TClientSocketManage.GetClientSocket(var Ap: PClientSocketInfo): Boolean;
begin
  LockManage( True );
  try
    Result := FManage.GetMem( Pointer(Ap) );
    if Result then
      Ap^.FPostRecvCount := 0;
  finally
    LockManage( False );
  end;
end;

procedure TClientSocketManage.LockClientSocket(const Ap: PClientSocketInfo; const ALock: Boolean);
begin
  if ALock then
    EnterCriticalSection( Ap^.FLock )
  else
    LeaveCriticalSection( Ap^.FLock );
end;

procedure TClientSocketManage.LockManage(const ALock: Boolean);
begin
  if ALock then
    EnterCriticalSection( FLock )
  else
    LeaveCriticalSection( FLock );
end;

{ TOverlapBufferManage }

function TOverlapBufferManage.BufferLength: Integer;
begin
  Result := FBufferLen
end;

function TOverlapBufferManage.Count: Integer;
begin
  Result := FManage.Count;
end;

constructor TOverlapBufferManage.Create(const AOwner: TxdCompletionPortServer; const ABufferLen, AMaxCount: Integer);
var
  i: Integer;
  nHeadSize: Integer;
  p: POverlapBufferInfo;
begin
  nHeadSize := SizeOf(TOverlapBufferInfo);
  FOverlapBufferSize := nHeadSize + ABufferLen;
  FBufferLen := ABufferLen;
  FOwner := AOwner;
  FManage := TxdFixedMemoryManager.Create( FOverlapBufferSize, AMaxCount );
  for i := 0 to AMaxCount - 1 do
  begin
    p := nil;
    FManage.GetMem( Pointer(p) );
    with p^ do
    begin
      FBufferLen := ABufferLen;
      FBuffer := PChar(p) + nHeadSize;
    end;
    FManage.FreeMem( p );
  end;
  InitializeCriticalSection( FLock );
end;

destructor TOverlapBufferManage.Destroy;
begin
  FManage.Free;
  DeleteCriticalSection( FLock );
  inherited;
end;

procedure TOverlapBufferManage.FreeOverlapBuffer(const Ap: POverlapBufferInfo);
begin
  LockManage( True );
  try
    FillChar( Ap^.FOverlap, CtOverlapSize, 0 );
    Ap^.FBufferLen := BufferLength;
    FManage.FreeMem( Ap );
  finally
    LockManage( False );
  end;
end;

function TOverlapBufferManage.GetOverlapBuffer(var Ap: POverlapBufferInfo): Boolean;
begin
  LockManage( True );
  try
    Result := FManage.GetMem( Pointer(Ap) );
  finally
    LockManage( False );
  end;
end;

procedure TOverlapBufferManage.LockManage(const ALock: Boolean);
begin
  if ALock then
    EnterCriticalSection( FLock )
  else
    LeaveCriticalSection( FLock );
end;

{ TPendingClientManage }

function TPendingClientManage.AddPending(const Ap: POverlapBufferInfo): Boolean;
begin
  LockManage( True );
  try
    Result := FManage.Add( Ap^.FClientSocketInfo^.FSocket, Ap );
  finally
    LockManage( False );
  end;
end;

procedure TPendingClientManage.CheckPending;
begin
  LockManage( True );
  try
    FManage.Loop( OnLoopManage );
  finally
    LockManage( False );
  end;
end;

function TPendingClientManage.Count: Integer;
begin
  Result := FManage.Count;
end;

constructor TPendingClientManage.Create(const AOwner: TxdCompletionPortServer);
begin
  InitializeCriticalSection( FLock );
  FManage := THashArray.Create;
  FOwner := AOwner;
  with FManage do
  begin
    MaxHashNodeCount := FOwner.MaxAcceptCount * 5;
    HashTableCount := FOwner.MaxAcceptCount * 3;
    UniquelyID := True;
    Active := True;
  end;
end;

destructor TPendingClientManage.Destroy;
begin
  FManage.Free;
  DeleteCriticalSection( FLock );
  inherited;
end;

procedure TPendingClientManage.DeletePending(const Ap: POverlapBufferInfo);
var
  p: Pointer;
begin
  LockManage( True );
  try
    FManage.Find( Ap^.FClientSocketInfo^.FSocket, p, True );
  finally
    LockManage( False );
  end;
end;

procedure TPendingClientManage.LockManage(const ALock: Boolean);
begin
  if ALock then
    EnterCriticalSection( FLock )
  else
    LeaveCriticalSection( FLock );  
end;

procedure TPendingClientManage.OnLoopManage(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel, AFindNext: Boolean);
begin
  FOwner.OnCheckPendingTime( pData, ADel );
end;




///////////////////////////////////////////启动WinSock环境/////////////////////////////////////////////////////////////////////////
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

{ TClientSocketInfo }

function TClientSocketInfo.IsCloseByServer: Boolean;
begin
  Result := FSocket = INVALID_SOCKET;
end;

initialization
  Startup;
finalization
  Cleanup;

end.
