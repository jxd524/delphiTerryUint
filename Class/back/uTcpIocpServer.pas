unit uTcpIocpServer;

interface
uses
  Windows, Classes, SysUtils, ExtCtrls, WinSock2, uJxdMemoryManage;

//{$I TCPSERVER.inc}

type
  TNotifyInfo = procedure(Sender: TObject; pInfo: pChar) of object;
  TOperationStyle = (osAccept, osSend, osRecv);
  TTcpIocpServer = class;
  {Completion Key define}
  pClientSocket = ^TClientSocket;
  TClientSocket = record
    SocketHandle: TSocket;
    SockAddIn: TSockAddrIn;
    Lock: TRTLCriticalSection;
    LastActiveTime: Cardinal;
    Checking: Boolean;
    RecvByte: Int64;
    SendByte: Int64;
    Data: Pointer;
  end;

  TOnInitClientSocket = procedure(Sender: TObject; p: pClientSocket) of object;
  TClientSocketClass = class
  private
    FOwner: TTcpIocpServer;
    FRecordSize: Integer;
    FMemoryManage: TxdFixedMemoryManager;
    FMaxSocketCount: Integer;
    FOnDestroyClientSocket: TOnInitClientSocket;
    FOnReclaimClientSocket: TOnInitClientSocket;
  public
    function  GetClientSocket(var p: pClientSocket): Boolean;
    procedure ReclaimClientSocket(p: pClientSocket);
    property  OnDestroyClientSocket: TOnInitClientSocket read FOnDestroyClientSocket write FOnDestroyClientSocket;
    property  OnReclaimClientSocket: TOnInitClientSocket read FOnReclaimClientSocket write FOnReclaimClientSocket;
    function  UsedCount: Integer; inline;//已使用数量
    function  SpaceCount: Integer; inline;//未使用数量
  public
    constructor Create(AOwner: TTcpIocpServer; AMaxSocketCount: Integer; NotifyInit: TOnInitClientSocket);
    destructor Destroy; override;
  end;

  {Per Handle IO define}
  pHandleIO = ^THandleIOBuffer;
  THandleIOBuffer = record
    Overlap: OVERLAPPED;
    pClient: pClientSocket;
    OptStyle: TOperationStyle;
    CurBuffer: WSABUF;
  end;
  THandleIOClass = class
  private
    FRecordSize: Integer;
    FMaxBufferLength: Integer;
    FMemoryManage: TxdFixedMemoryManager;
  public
    function  GetHandleBuffer(var p: pHandleIO): Boolean;
    procedure ReclaimHandleBuffer(p: pHandleIO);
    function  UsedCount: Integer; inline;//已使用数量
    function  SpaceCount: Integer; inline;//未使用数量
  public
    constructor Create(AMaxBufferCount, AMaxBufferLength: Integer);
    destructor Destroy; override;
  end;

  {异常类定义}
  TTcpIocpException = class(Exception);

  {工作线程}
  TWorkThread = class(TThread)
  private
    FOwner: TTcpIocpServer;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TTcpIocpServer);
    destructor Destroy; override;
  end;

  TCheckThread = class(TThread)
  private
    FOwner: TTcpIocpServer;
    FOnExecute: TNotifyEvent;
    FWaitEvent: THandle;
    FCheckSpaceTime: Integer;
    procedure SetCheckSpaceTime(const Value: Integer);
  protected
    procedure Execute; override;
  public
    procedure Terminate;
    constructor Create(AOwner: TTcpIocpServer; ANotifyEvent: TNotifyEvent; ACheckSpaceTime: Integer);
    destructor Destroy; override;
    property CheckSpaceTime: Integer read FCheckSpaceTime write SetCheckSpaceTime;
  end;

  {完成端口定义类}
  TTcpIocpServer = class( {$IFDEF InheritedComponent} TComponent {$ELSE} TObject {$ENDIF})
  private
    FActive: Boolean;
    FCompletionPort: THandle; //完成端口
    FPort: u_short;
    FCmdSocket: TSocket; //用于监听的套接字
    FInitListenCount: Integer;
    FInitPostAccept: Integer;

    {内存相关}
    FMaxSocketCount: Integer;
    FMaxBufferCount: Integer;
    FMaxBufferLength: Cardinal;

    {数据统计相关}
    FCurActiveThreadCount: Integer;
    FWorkThreadCount: Integer;
    FTotalRecvByte: Int64;
    FRecvByteLock: TRTLCriticalSection;
    FTotalSendByte: Int64;
    FSendByteLock: TRTLCriticalSection;
    {未决连接}
    FPendingSocket: TList;
    FPendingLock: TRTLCriticalSection;
    FCheckPendingThread: TCheckThread; //检查线程
    FCheckPendingSpaceTime: Integer;     //每检查 FPendingSocket.Count 的间隔时间
    FCurPendingIndex: Integer;
    FCheckPendingCount: Integer;        //检查次数
    FAllowmaxNoActive_Pending: Integer; //客户端连接后在 FAllowmaxNoActive_Pending 秒内无发送数据则关闭

    {已决连接}
    FConnectSocket: TList;
    FConnectLock: TRTLCriticalSection;
    FCheckConnectedThread: TCheckThread; //检查线程
    FCheckConnectedSpaceTime: Integer;   //每检查 FConnectSocket.Count 的间隔时间
    FCurConnectedIndex: Integer;
    FCheckConnectedCount: Integer;       //检查次数
    FAllowMaxNoActive_Con: Integer;      //此间隔时间后还无活动的客户调用 OnCheckConnectedClient;

    {工作线程}
    FWorkThread: array of TWorkThread;
    {Winsock2 扩展函数指针函数}
    FAcceptEx: TAcceptEx; //AcceptEx函数地址
    FGetAcceptSockAddrs: TGetAcceptExSockAddrs;  //GetAcceptExSockaddrs函数地址
    FDisConnectEx: TDisconnectEx; //DisConnectEx函数地址;关闭套接字,并让套接字可被重用

    {事件}
    FNotifyInfo: TNotifyInfo;

    procedure SetActive(const Value: Boolean);
    procedure SetPort(const Value: u_short);
    procedure SetInitListenCount(const Value: Integer);
    procedure SetInitPostAccept(const Value: Integer);
    procedure SetWorkThreadCount(const Value: Integer);
    procedure SetAllowTimeOut(const Value: Integer);
    procedure SetCheckConnectedSpaceTime(const Value: Integer);
    procedure SetMaxSocketCount(const Value: Integer);
    procedure SetMaxbufferCount(const Value: Integer);
    procedure SetMaxBufferLength(const Value: Cardinal);
    function GetSpaceBufferCount: Integer;
    function GetUsedBufferCount: Integer;
    function GetSpaceSocketCount: Integer;
    function GetOnlineCount: Integer;
    function GetPostPendingCount: Integer;
  public
    constructor Create{$IFDEF InheritedComponent} (AOwner: TComponent); override {$ENDIF};
    destructor Destroy; override;
  private
    function  OpenTcpIocp: Boolean;
    procedure CloseTcpIocp;

    {事件处理}
    procedure DoAcceptNewClient(pSocket: pClientSocket; pHandleBuffer: pHandleIO; dwTransByte: Cardinal);
    procedure DoRecvClienBuffer(pSocket: pClientSocket; pHandleBuffer: pHandleIO; dwTransByte: Cardinal);
    procedure DoSendFinish(pSocket: pClientSocket; pHandleBuffer: pHandleIO; dwTransByte: Cardinal);
    procedure DoClientClose(pSocket: pClientSocket; pHandleBuffer: pHandleIO);

    {线程函数}
    procedure DoWorkThreadRunning(Sender: TObject); //工作线程
    procedure DoCheckPandingSocket(Sender: TObject); //检查未决套接字定时器
    procedure DoCheckConnectedSocket(Sender: TObject); //检查已连接套接字

    {函数处理}
    procedure InitStat;
    procedure InitList(var AList: TList);
    procedure GetClientAddr(pSocket: pClientSocket; pHandleBuffer: pHandleIO);
    function  DeletePendingSocket(const pSocket: pClientSocket): Boolean;
    function  DeleteConnectedSocket(const pSocket: pClientSocket): Boolean;
  protected
    FClientSocketManage: TClientSocketClass;  //连接管理对象
    FHandleIOManage: THandleIOClass;          //缓冲区管理对象
    procedure DoNotifyMessage(const AMsgInfo: PChar);
    procedure DoInitClientSocket(Sender: TObject; p: pClientSocket); virtual;
    procedure DoReclaimClientSocket(Sender: TObject; p: pClientSocket); virtual;
    procedure DoDestroyClientSocket(Sender: TObject; p: pClientSocket); virtual;

    {投递相关操作}
    procedure PostAcceptEx(const ACount: Integer);
    procedure PostRecvEx(const pSocket: pClientSocket; const ACount: Integer);
    function  PostSendEx(const pSocket: pClientSocket; wsaBuffer: WSABUF): Boolean;
    
    {子类需要处理的事件, 其中 pSocket 都已锁定}

    //判断是否可登录 True: 可登录
    function  OnJudgeLogin(const pSocket: pClientSocket; const wsaBuffer: WSABUF): Boolean; virtual;
    //登录成功
    procedure OnLoginSuccess(const pSocket: pClientSocket); virtual;
    //登录失败
    procedure OnLoginFail; virtual;
    //接收到客户端数据
    procedure OnRecvClientBuffer(const pSocket: pClientSocket; const wsaBuffer: WSABUF; var nPostRecvCount: Integer); virtual;
    //数据发送成功
    procedure OnSendFinishBuffer(const pSocket: pClientSocket; const wsaBuffer: WSABUF; var nPostRecvCount: Integer); virtual;
    //关闭已连接的对象(在连接后一定时间内没有发送数据)
    procedure OnClosePendingClient(const pSocket: pClientSocket); virtual;
    //已连接的对象关闭(主动关闭或被动关闭)
    procedure OnClientClose(const pSocket: pClientSocket); virtual;
    //已连接的对象在一段时间内没有活动,将触发此事件
    procedure OnCheckConnectedClient(const pSocket: pClientSocket); virtual;


    {Winsock2 扩展函数}
    procedure LoadWinSockExFunction;
    function  CreateOverLappedSocket: TSocket; inline;
    function  CreateIOCP: THandle; inline;
    function  RelatingToCompletionPort(const ASocket: TSocket; const ACompletionKey: Pointer): Boolean;
    function  AcceptEx(const ANewSock: TSocket; const AOverlapped: POverlapped; const ABuffer: PChar; const ABufferLen: DWORD): Boolean;
    function  RecvBufferEx(const ASocket: TSocket; const AOverlapped: POverlapped; const ABuffer: PChar; const ABufferLen: DWORD): Boolean;
    function  SendBufferEx(const ASocket: TSocket; const AOverlapped: POverlapped; const ABuffer: PChar; const ABufferLen: DWORD): Boolean;
    procedure GetAcceptSockAddr(const AFirstBuffer: PChar; const ABufLen: Integer; var ALocalAddr, ARemoteAddr: TSockAddrIn);
    function  DisConnectEx(const ASocket: TSocket; const lpOverlapped: POverlapped): Boolean;
    function  GracefullyCloseSocket(const ASocket: TSocket; const AMaxWaitTime: u_short = 10): Boolean; //True: 成功优雅关闭
    function  ReadySocket(const ASocket: TSocket; const ABindPort: u_short; const AListenCount: Integer): Boolean; //True: 绑定并监听成功
    function  GetConnectTime(const ASocket: TSocket): Integer;
{$IFDEF InheritedComponent}
  published
{$ELSE}
  public
{$ENDIF}
    property Active: Boolean read FActive write SetActive;
    property Port: u_short read FPort write SetPort; //绑定端口号
    property InitPostAccept: Integer read FInitPostAccept write SetInitPostAccept;   //初始化投递接收数
    property InitListenCount: Integer read FInitListenCount write SetInitListenCount; //初始化监听数
    property WorkThreadCount: Integer read FWorkThreadCount write SetWorkThreadCount; //工作线程数
    {事件定义}
    property NotifyInfo: TNotifyInfo read FNotifyInfo write FNotifyInfo;

    {未决连接设置}
    property CheckPendingSpaceTime: Integer read FCheckPendingSpaceTime write FCheckPendingSpaceTime; //单位: 毫秒 检查 FPendingSocket 间隔时间
    property Pending_AllowMaxNoActive: Integer read FAllowmaxNoActive_Pending write SetAllowTimeOut; // 单位: 秒   连接与第一块缓冲区允许的最长时间,超出却关闭客户端

    {已连接设置}
    property CheckConnectedSpaceTime: Integer read FCheckConnectedSpaceTime write SetCheckConnectedSpaceTime;//每检查 FConnectSocket.Count 的间隔时间
    property Con_AllowMaxNoActive: Integer read FAllowMaxNoActive_Con write FAllowMaxNoActive_Con;//此间隔时间后还无活动的客户调用 OnCheckConnectedClient;
    {统计信息}
    property CurActiveThreadCount: Integer read FCurActiveThreadCount; //当前活动的工作线程数
    property CheckPendingCount: Integer read FCheckPendingCount;       //检查未决对象次数 
    property CheckConnectedCount: Integer read FCheckConnectedCount;   //检查连接对象次数
    property SpaceSocketCount: Integer read GetSpaceSocketCount;       //可用客户对象数量
    property OnlineCount: Integer read GetOnlineCount;                 //在线用户数
    property PostPendingCount: Integer read GetPostPendingCount;       //已投递接受套接字数量
    property SpaceBufferCount: Integer read GetSpaceBufferCount;       //可用内存缓冲区数量
    property UsedBufferCount: Integer read GetUsedBufferCount;         //正在使用的内存缓冲区数量
    property TotalRecvByteCount: Int64 read FTotalRecvByte;            //服务器接收到总的字节数
    property TotalSendByteCount: Int64 read FTotalSendByte;            //服务器发送的总的字节数
    {内存设置}
    property MaxSocketCount: Integer read FMaxSocketCount write SetMaxSocketCount default 1024 * 2;   //最大可连接客户端数
    property MaxBufferCount: Integer read FMaxBufferCount write SetMaxbufferCount default 1024 * 6;   //最大缓冲区数量
    property MaxBufferLength: Cardinal read FMaxBufferLength write SetMaxBufferLength default 1024 * 8;//缓冲区大小
  end;

implementation

{ TTcpIocpServer }
const
  CtSockAddLength = 16; { Sizeof(SOCKADDR) }
  CtListenCompletionKey = $FFFFFFFF;

function TTcpIocpServer.AcceptEx(const ANewSock: TSocket; const AOverlapped: POverlapped; const ABuffer: PChar;
  const ABufferLen: DWORD): Boolean;
var
  dwByteRece: DWORD;
begin
  Result := False;
  if (FCmdSocket <> INVALID_SOCKET) and Assigned(FAcceptEx) then
  begin
    Result := FAcceptEx( FCmdSocket, ANewSock, ABuffer, ABufferLen - (CtSockAddLength + 16) * 2,
                         CtSockAddLength + 16, CtSockAddLength + 16, @dwByteRece, AOverlapped);
    if (not Result) and ( WSAGetLastError <> WSA_IO_PENDING ) then
      Result := False
    else
      Result := True;
  end;
end;

constructor TTcpIocpServer.Create;//(AOwner: TComponent);
begin
{$IFDEF InheritedComponent}
  inherited Create( AOwner );
{$ELSE}
  inherited Create;
{$ENDIF}
  FCmdSocket := INVALID_SOCKET;
  FCompletionPort := INVALID_HANDLE_VALUE;
  FActive := False;
  FAcceptEx := nil;
  FGetAcceptSockAddrs := nil;
  FDisConnectEx := nil;
  FPendingSocket := nil;
  FConnectSocket := nil;
  FCheckPendingThread := nil;
  FCheckConnectedThread := nil;
  FPort := 8989;
  FInitListenCount := 200;
  FInitPostAccept := 64;
  FWorkThreadCount := 2;
  FCheckPendingSpaceTime := 1 * 1000 * 60; //3分钟检查一次
  FAllowmaxNoActive_Pending := 30; //秒
  FCheckConnectedSpaceTime :=  1000 * 20;
  FAllowMaxNoActive_Con := 2 * 1000 * 60 + 1000 * 30;
  FMaxSocketCount := 1024 * 2;
  FMaxBufferCount := 1024 * 6;
  FMaxBufferLength := 1024 * 8;
  FTotalRecvByte := 0;
  FTotalSendByte := 0;
  InitializeCriticalSection( FPendingLock );
  InitializeCriticalSection( FConnectLock );
  InitializeCriticalSection( FRecvByteLock );
  InitializeCriticalSection( FSendByteLock );
end;

function TTcpIocpServer.CreateIOCP: THandle;
begin
  Result := CreateIoCompletionPort( INVALID_HANDLE_VALUE, 0, 0, 0 );
end;

function TTcpIocpServer.CreateOverLappedSocket: TSocket;
begin
  Result := WSASocket( AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED );
end;

function TTcpIocpServer.DeleteConnectedSocket(const pSocket: pClientSocket): Boolean;
var
  nIndex: Integer;
begin
  EnterCriticalSection( FConnectLock );
  try
    nIndex := FConnectSocket.IndexOf( pSocket );
    Result := nIndex <> -1;
    if Result then
    begin
      FConnectSocket.Delete( nIndex );
      if nIndex < FCurConnectedIndex then
        Dec(FCurConnectedIndex);
    end;
  finally
    LeaveCriticalSection( FConnectLock );
  end;
end;


function TTcpIocpServer.DeletePendingSocket(const pSocket: pClientSocket): Boolean;
var
  nIndex: Integer;
begin
  EnterCriticalSection( FPendingLock );
  try
    nIndex := FPendingSocket.IndexOf( pSocket );
    Result := nIndex <> -1;
    if Result then
    begin
      FPendingSocket.Delete( nIndex );
      if nIndex < FCurPendingIndex then
        Dec(FCurPendingIndex);
    end;
  finally
    LeaveCriticalSection( FPendingLock );
  end;
end;

destructor TTcpIocpServer.Destroy;
begin
  if Active then CloseTcpIocp;
  if Assigned(FPendingSocket) then
    FPendingSocket.Free;
  if Assigned(FConnectSocket) then
    FConnectSocket.Free;
  DeleteCriticalSection( FPendingLock );
  DeleteCriticalSection( FConnectLock );
  DeleteCriticalSection( FRecvByteLock );
  DeleteCriticalSection( FSendByteLock );
  inherited;
end;

function TTcpIocpServer.DisConnectEx(const ASocket: TSocket; const lpOverlapped: POverlapped): Boolean;
begin
  Result := False;
  if Assigned(FDisConnectEx) then
    Result := FDisConnectEx( ASocket, lpOverlapped, TF_REUSE_SOCKET, 0 )
end;

procedure TTcpIocpServer.DoAcceptNewClient(pSocket: pClientSocket; pHandleBuffer: pHandleIO; dwTransByte: Cardinal);
var
  nPostRecvCount: Integer;
begin
  //New client is connect to server and send the first buffer data!
  {删除末决连接列表}
  EnterCriticalSection( pSocket^.Lock );
  try
    if not DeletePendingSocket( pSocket ) then
    begin
      DoNotifyMessage( PChar('对象已被删除') );
      FHandleIOManage.ReclaimHandleBuffer( pHandleBuffer );
      Exit;
    end;
    pSocket^.LastActiveTime := GetTickCount;
    pHandleBuffer^.CurBuffer.len := dwTransByte;
    GetClientAddr( pSocket, pHandleBuffer );
    if OnJudgeLogin( pSocket, pHandleBuffer^.CurBuffer ) then
    begin
      //登录成功
      if not RelatingToCompletionPort( pSocket^.SocketHandle, pSocket) then
      begin
        DoNotifyMessage( PChar( Format('关联完成端口失败: %d', [WSAGetLastError])) );
        FClientSocketManage.ReclaimClientSocket( pSocket );
        FHandleIOManage.ReclaimHandleBuffer( pHandleBuffer );
        Exit;
      end;
      OnLoginSuccess( pSocket );
      {添加到已决列表}
      EnterCriticalSection( FConnectLock );
      try
        FConnectSocket.Add( pSocket );
      finally
        LeaveCriticalSection( FConnectLock );
      end;
      nPostRecvCount := 1;
      OnRecvClientBuffer( pSocket, pHandleBuffer^.CurBuffer, nPostRecvCount );
      FHandleIOManage.ReclaimHandleBuffer( pHandleBuffer );
      PostRecvEx( pSocket, nPostRecvCount);
    end
    else
    begin
      //登录失败
      FClientSocketManage.ReclaimClientSocket( pSocket );
      FHandleIOManage.ReclaimHandleBuffer( pHandleBuffer );
      OnLoginFail;
    end;
  finally
    LeaveCriticalSection( pSocket^.Lock );
  end;
  //暂时以每个新连接的到来再投递一个连接
  PostAcceptEx( 1 );
end;

procedure TTcpIocpServer.DoCheckConnectedSocket(Sender: TObject);
var
  pSocket: pClientSocket;
  bCanCheck, bCheck: Boolean;
begin
  if not (Sender is TCheckThread) then Exit;
  EnterCriticalSection( FConnectLock );
  try
    if FConnectSocket.Count = 0 then Exit;
    if FCurConnectedIndex < FConnectSocket.Count then
    begin
      Inc(FCheckConnectedCount);
      pSocket := FConnectSocket[FCurConnectedIndex];
      bCanCheck := TryEnterCriticalSection( pSocket^.Lock );
      if bCanCheck then
      begin
        try
          bCheck := (GetTickCount - pSocket^.LastActiveTime) > Cardinal(FAllowMaxNoActive_Con);
          if bCheck then
          begin
            if not pSocket^.Checking then
            begin
              pSocket^.Checking := True;
              OnCheckConnectedClient( pSocket );
            end
            else
            begin
              FConnectSocket.Delete( FCurConnectedIndex );
              OnClientClose( pSocket );
              FClientSocketManage.ReclaimClientSocket( pSocket );
            end;
          end;
        finally
          LeaveCriticalSection( pSocket^.Lock );
        end;
      end;
    end;
    if FCurConnectedIndex = FConnectSocket.Count - 1 then
      (Sender as TCheckThread).CheckSpaceTime := FCheckConnectedSpaceTime
    else
      (Sender as TCheckThread).CheckSpaceTime := 10;
    if FConnectSocket.Count = 0 then
      FCurConnectedIndex := 0
    else
      FCurConnectedIndex := (FCurConnectedIndex + 1) mod FConnectSocket.Count;
  finally
    LeaveCriticalSection( FConnectLock );
  end;
end;

procedure TTcpIocpServer.DoCheckPandingSocket(Sender: TObject);
var
  pSocket: pClientSocket;
  bCanCheck, bDel: Boolean;
begin
  if not (Sender is TCheckThread) then Exit;
  EnterCriticalSection( FPendingLock );
  try
    if FPendingSocket.Count = 0 then Exit;
    Inc(FCheckPendingCount);
    if FCurPendingIndex < FPendingSocket.Count then
    begin
      pSocket := FPendingSocket[FCurPendingIndex];
      bCanCheck := TryEnterCriticalSection( pSocket^.Lock );
      if bCanCheck then
      begin
        try
          bDel := GetConnectTime(pSocket^.SocketHandle) > FAllowmaxNoActive_Pending;
          if bDel then
          begin
            OnClosePendingClient( pSocket );
            FClientSocketManage.ReclaimClientSocket( pSocket );
            FPendingSocket.Delete( FCurPendingIndex );
          end;
        finally
          LeaveCriticalSection( pSocket^.Lock );
        end;
      end;
    end;
    if FCheckPendingCount = FPendingSocket.Count - 1 then
      (Sender as TCheckThread).CheckSpaceTime := FCheckPendingSpaceTime
    else
      (Sender as TCheckThread).CheckSpaceTime := 10;
    FCurPendingIndex := (FCurPendingIndex + 1) mod FPendingSocket.Count;
  finally
    LeaveCriticalSection( FPendingLock );
  end;
end;

procedure TTcpIocpServer.DoClientClose(pSocket: pClientSocket; pHandleBuffer: pHandleIO);
begin
  if (pSocket^.LastActiveTime <> 0) and (GetConnectTime(pSocket^.SocketHandle) <> -1) then
  begin
    EnterCriticalSection( pSocket^.Lock );
    try
      {删除列表中该对象}
      if (pSocket^.LastActiveTime <> 0) and (GetConnectTime(pSocket^.SocketHandle) <> -1) then
      begin
        if not DeleteConnectedSocket( pSocket ) then
          DeletePendingSocket( pSocket );
        OnClientClose( pSocket );
        FClientSocketManage.ReclaimClientSocket( pSocket );
      end;
    finally
      LeaveCriticalSection( pSocket^.Lock );
    end;
  end;
  FHandleIOManage.ReclaimHandleBuffer( pHandleBuffer );
end;

procedure TTcpIocpServer.DoDestroyClientSocket(Sender: TObject; p: pClientSocket);
begin

end;

procedure TTcpIocpServer.DoReclaimClientSocket(Sender: TObject; p: pClientSocket);
begin

end;

procedure TTcpIocpServer.DoInitClientSocket(Sender: TObject; p: pClientSocket);
begin

end;

procedure TTcpIocpServer.DoNotifyMessage(const AMsgInfo: PChar);
begin
  OutputDebugString( AMsgInfo);
  if Assigned(FNotifyInfo) then
    FNotifyInfo(Self, AMsgInfo);
end;

procedure TTcpIocpServer.DoRecvClienBuffer(pSocket: pClientSocket; pHandleBuffer: pHandleIO; dwTransByte: Cardinal);
var
  nPostRecvCount: Integer;
begin

  EnterCriticalSection( FRecvByteLock );
  try
    Inc( FTotalRecvByte, dwTransByte );
  finally
    LeaveCriticalSection( FRecvByteLock );
  end;

  EnterCriticalSection( pSocket^.Lock );
  try
    pSocket^.LastActiveTime := GetTickCount;
    Inc(pSocket^.RecvByte, dwTransByte);
    if pSocket^.Checking then pSocket^.Checking := False;
    pHandleBuffer^.CurBuffer.len := dwTransByte;
    nPostRecvCount := 1;
    OnRecvClientBuffer( pSocket, pHandleBuffer^.CurBuffer, nPostRecvCount );
    FHandleIOManage.ReclaimHandleBuffer( pHandleBuffer );
    PostRecvEx( pSocket, nPostRecvCount);
  finally
    LeaveCriticalSection( pSocket^.Lock );
  end;
end;

procedure TTcpIocpServer.DoSendFinish(pSocket: pClientSocket; pHandleBuffer: pHandleIO; dwTransByte: Cardinal);
var
  nPostRecvCount: Integer;
begin
  EnterCriticalSection( FSendByteLock );
  try
    Inc( FTotalSendByte, dwTransByte );
  finally
    LeaveCriticalSection( FSendByteLock );
  end;

  EnterCriticalSection( pSocket^.Lock );
  try
    pSocket^.LastActiveTime := GetTickCount;
    if pSocket^.Checking then pSocket^.Checking := False;
    pHandleBuffer^.CurBuffer.len := dwTransByte;
    nPostRecvCount := 1;
    OnSendFinishBuffer( pSocket, pHandleBuffer^.CurBuffer, nPostRecvCount );
    FHandleIOManage.ReclaimHandleBuffer( pHandleBuffer );
    PostRecvEx( pSocket, nPostRecvCount);
  finally
    LeaveCriticalSection( pSocket^.Lock );
  end;
end;

procedure TTcpIocpServer.DoWorkThreadRunning(Sender: TObject);
var
  dwTransByte: Cardinal;
  pSocket: pClientSocket;
  pHandleBuffer: pHandleIO;
  bOK: Boolean;
  nErr: Integer;
begin
  if not (Sender is TWorkThread) then Exit;
  if not GetQueuedCompletionStatus(FCompletionPort, dwTransByte, Cardinal(pSocket), pOverlapped(pHandleBuffer), INFINITE) then
  begin
    nErr := WSAGetLastError;
    DoNotifyMessage( PChar( Format('GetQueuedCompletionStatus失败: %d', [nErr]) ) );
    case nErr of
      64://指定的网络名不再可用。
      begin
        //客户端断网
        if Cardinal(pSocket) = CtListenCompletionKey then
          pSocket := pHandleBuffer^.pClient;
        DoClientClose( pSocket, pHandleBuffer );
      end;
      6, 955: //由于线程退出或应用程序请求，已放弃 I/O 操作。 ;
      begin
        //关闭 FCompetionPort 或者程序退出
        (Sender as TWorkThread).Terminate;
      end;
    end;
    Exit;
  end;
  InterlockedIncrement( FCurActiveThreadCount );

  //服务器主动发包表明停止
  if (dwTransByte = 0) and (Cardinal(pSocket) = 0) and (pHandleBuffer = nil) then
  begin
    (Sender as TWorkThread).Terminate;
    InterlockedDecrement( FCurActiveThreadCount );
    Exit;
  end;

  //客户端主动关闭
  if (dwTransByte = 0) and (pSocket <> nil) and (pHandleBuffer <> nil) then
  begin
    if Cardinal(pSocket) = CtListenCompletionKey then
      pSocket := pHandleBuffer^.pClient;
    DoClientClose( pSocket, pHandleBuffer );
    InterlockedDecrement( FCurActiveThreadCount );
    Exit;
  end;
  
  bOK := False;
  if ( Cardinal(pSocket) = CtListenCompletionKey ) and (pHandleBuffer <> nil) and (pHandleBuffer^.OptStyle = osAccept) then
  begin
    {新连接}
    pSocket := pHandleBuffer^.pClient;
    DoAcceptNewClient(pSocket, pHandleBuffer, dwTransByte);
    bOK := True;
  end
  else if (pSocket <> nil) and (pHandleBuffer <> nil) then
  begin
    bOK := True;
    if pHandleBuffer^.OptStyle = osSend then
      DoSendFinish(pSocket, pHandleBuffer, dwTransByte)
    else if pHandleBuffer^.OptStyle = osRecv then
      DoRecvClienBuffer(pSocket, pHandleBuffer, dwTransByte)
    else
      bOK := False;
  end;

  if not bOK then
  begin
    DoNotifyMessage( PChar( Format('工作线程出错: %d', [WSAGetLastError]) ) );
    if pHandleBuffer <> nil then
      FHandleIOManage.ReclaimHandleBuffer( pHandleBuffer );
    if pSocket <> nil then
      FClientSocketManage.ReclaimClientSocket( pSocket );
  end;
  InterlockedDecrement( FCurActiveThreadCount );
end;

procedure TTcpIocpServer.GetAcceptSockAddr(const AFirstBuffer: PChar; const ABufLen: Integer; var ALocalAddr,
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

function TTcpIocpServer.GetSpaceBufferCount: Integer;
begin
  Result := -1;
  if Assigned(FHandleIOManage) then
    Result := FHandleIOManage.SpaceCount;
end;

function TTcpIocpServer.GetSpaceSocketCount: Integer;
begin
  Result := -1;
  if Assigned(FClientSocketManage) then
    Result := FClientSocketManage.SpaceCount;
end;

function TTcpIocpServer.GetUsedBufferCount: Integer;
begin
  Result := -1;
  if Assigned(FHandleIOManage) then
    Result := FHandleIOManage.SpaceCount;
end;

procedure TTcpIocpServer.GetClientAddr(pSocket: pClientSocket; pHandleBuffer: pHandleIO);
var
  Addr: TSockAddrIn;
begin
  GetAcceptSockAddr(pHandleBuffer^.CurBuffer.buf, MaxBufferLength,
                    Addr, pSocket^.SockAddIn);
end;

function TTcpIocpServer.GetConnectTime(const ASocket: TSocket): Integer;
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

function TTcpIocpServer.GetOnlineCount: Integer;
begin
  Result := -1;
  if Assigned(FConnectSocket) then
    Result := FConnectSocket.Count;  
end;

function TTcpIocpServer.GetPostPendingCount: Integer;
begin
  Result := -1;
  if Assigned(FPendingSocket) then
    Result := FPendingSocket.Count;
end;

procedure TTcpIocpServer.LoadWinSockExFunction;
begin
  if FCmdSocket = INVALID_SOCKET then
  begin
    DoNotifyMessage( 'FCmdSocket = INVALID_SOCKET' );
    Exit;
  end;
  if not Assigned(FAcceptEx) then
    FAcceptEx := WSAGetExtensionFunctionPointer( FCmdSocket, WSAID_ACCEPTEX );
  if not Assigned(FGetAcceptSockAddrs) then
    FGetAcceptSockAddrs := WSAGetExtensionFunctionPointer( FCmdSocket, WSAID_GETACCEPTEXSOCKADDRS );
  if not Assigned(FDisConnectEx) then
    FDisConnectEx := WSAGetExtensionFunctionPointer( FCmdSocket, WSAID_DISCONNECTEX );
end;

procedure TTcpIocpServer.OnCheckConnectedClient(const pSocket: pClientSocket);
begin

end;

procedure TTcpIocpServer.OnClientClose(const pSocket: pClientSocket);
begin

end;

procedure TTcpIocpServer.OnClosePendingClient(const pSocket: pClientSocket);
begin

end;

function TTcpIocpServer.OnJudgeLogin(const pSocket: pClientSocket; const wsaBuffer: WSABUF): Boolean;
begin
  Result := True;
end;

procedure TTcpIocpServer.OnLoginFail;
begin

end;

procedure TTcpIocpServer.OnLoginSuccess(const pSocket: pClientSocket);
begin

end;

procedure TTcpIocpServer.OnRecvClientBuffer(const pSocket: pClientSocket; const wsaBuffer: WSABUF;
  var nPostRecvCount: Integer);
begin
  nPostRecvCount := 1;
end;

procedure TTcpIocpServer.OnSendFinishBuffer(const pSocket: pClientSocket; const wsaBuffer: WSABUF;
  var nPostRecvCount: Integer);
begin

end;

function TTcpIocpServer.OpenTcpIocp: Boolean;
var
  i: Integer;
begin
  Result := False;
  if FActive then Exit;
  InitStat;
  FCmdSocket := CreateOverLappedSocket;
  if not ReadySocket( FCmdSocket, Port, InitListenCount ) then Exit;

  {创建内存}
  if Assigned(FHandleIOManage) then FHandleIOManage.Free;
  FHandleIOManage := THandleIOClass.Create( FMaxBufferCount, FMaxBufferLength );
  if Assigned(FClientSocketManage) then FClientSocketManage.Free;
  FClientSocketManage := TClientSocketClass.Create(Self, FMaxSocketCount, DoInitClientSocket);
  FClientSocketManage.OnReclaimClientSocket := DoReclaimClientSocket;
  FClientSocketManage.OnDestroyClientSocket := DoDestroyClientSocket;

  {列表}
  InitList( FPendingSocket );
  InitList( FConnectSocket );

  FCompletionPort := CreateIOCP;
  RelatingToCompletionPort( FCmdSocket, pChar(CtListenCompletionKey));

  {工作线程, 线程自动释放}
  FWorkThreadCount := 1;  //for debug define
  SetLength(FWorkThread, FWorkThreadCount);
  for i := 0 to FWorkThreadCount - 1 do
    FWorkThread[i] := TWorkThread.Create( Self );

  {检查线程}
  FCheckConnectedThread := TCheckThread.Create( Self, DoCheckConnectedSocket, FCheckConnectedSpaceTime );
  FCheckPendingThread := TCheckThread.Create( Self, DoCheckPandingSocket, FCheckPendingSpaceTime );

  LoadWinSockExFunction;
  PostAcceptEx( InitPostAccept );
  Result := True;
end;

procedure TTcpIocpServer.PostAcceptEx(const ACount: Integer);
var
  i: Integer;
  pSocket: pClientSocket;
  pHandleBuffer: pHandleIO;
begin
  for i := 0 to ACount - 1 do
  begin
    if not FClientSocketManage.GetClientSocket( pSocket ) then
    begin
      DoNotifyMessage( '无法继续投递套接字进行监听, GetClientSocket失败, 函数: PostAcceptEx' );
      Break;
    end;
    if not FHandleIOManage.GetHandleBuffer(pHandleBuffer) then
    begin
      FClientSocketManage.ReclaimClientSocket( pSocket );
      DoNotifyMessage( '无法继续投递套接字进行监听, GetHandleBuffer失败, 函数: PostAcceptEx' );
      Break;
    end;
    pHandleBuffer^.pClient := pSocket;
    pHandleBuffer^.OptStyle := osAccept;
    if AcceptEx( pSocket^.SocketHandle, @pHandleBuffer^.Overlap, pHandleBuffer^.CurBuffer.buf, pHandleBuffer^.CurBuffer.len ) then
    begin
      EnterCriticalSection( FPendingLock );
      try
        FPendingSocket.Add( pSocket );
      finally
        LeaveCriticalSection( FPendingLock );
      end;
    end
    else
    begin
      FClientSocketManage.ReclaimClientSocket( pSocket );
      FHandleIOManage.ReclaimHandleBuffer( pHandleBuffer );
      DoNotifyMessage( PChar(Format('AcceptEx失败: %d, 函数: PostAcceptEx', [WSAGetLastError])) );
    end;
  end;
end;

procedure TTcpIocpServer.PostRecvEx(const pSocket: pClientSocket; const ACount: Integer);
var
  i: Integer;
  pHandleBuffer: pHandleIO;
begin
  if (ACount <= 0) or (GetConnectTime(pSocket^.SocketHandle) = -1) then Exit;
  for i := 0 to ACount - 1 do
  begin
    if not FHandleIOManage.GetHandleBuffer(pHandleBuffer) then
    begin
      DoNotifyMessage( '无可用缓冲区进行投递接收数据, PostRecvEx失败' );
      Break;
    end;
    pHandleBuffer^.pClient := pSocket;
    pHandleBuffer^.OptStyle := osRecv;
    if not RecvBufferEx( pSocket^.SocketHandle, @pHandleBuffer^.Overlap, pHandleBuffer^.CurBuffer.buf, pHandleBuffer^.CurBuffer.len ) then
    begin
      FHandleIOManage.ReclaimHandleBuffer( pHandleBuffer );
      DoNotifyMessage( PChar(Format('PostRecvEx: %d', [WSAGetLastError])) );
    end;
  end;
end;

function TTcpIocpServer.PostSendEx(const pSocket: pClientSocket; wsaBuffer: WSABUF): Boolean;
var
  p: pHandleIO;
  nLen: DWORD;
  nPos: Integer;
begin
  Result := False;
  nPos := 0;
  while wsaBuffer.len > 0 do
  begin
    Result := FHandleIOManage.GetHandleBuffer( p );
    if not Result then
    begin
      DoNotifyMessage( '无可用缓冲区进行发送数据, PostSendEx失败' );
      Break;
    end;
    if Result then
    begin
      if wsaBuffer.len > p^.CurBuffer.len then
        nLen := p^.CurBuffer.len
      else
        nLen := wsaBuffer.len;
      wsaBuffer.len := nLen - wsaBuffer.len;
      p^.CurBuffer.len := nLen;
      Move( wsaBuffer.buf[nPos], p^.CurBuffer.buf^, nLen);
      p^.OptStyle := osSend;
      Result := SendBufferEx( pSocket^.SocketHandle, @p^.Overlap, p^.CurBuffer.buf, p^.CurBuffer.len );
      Inc(nPos, nLen);
    end;
  end;
end;

function TTcpIocpServer.ReadySocket(const ASocket: TSocket; const ABindPort: u_short; const AListenCount: Integer): Boolean;
var
  SocketIn: TSockAddrIn;
begin
  Result := True;
  with SocketIn do
  begin
    sin_family := AF_INET;
    sin_port := htons( ABindPort );
    sin_addr.S_addr := INADDR_ANY;
  end;
  if SOCKET_ERROR = bind(ASocket, pSockAddr(@SocketIn), CtSockAddLength) then
  begin
    Result := False;
    Exit;
  end;
  if SOCKET_ERROR = listen(ASocket, AListenCount) then
  begin
    Result := False;
    Exit;
  end;
end;

function TTcpIocpServer.RecvBufferEx(const ASocket: TSocket; const AOverlapped: POverlapped; const ABuffer: PChar;
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

function TTcpIocpServer.RelatingToCompletionPort(const ASocket: TSocket; const ACompletionKey: Pointer): Boolean;
begin
  Result := False;
  if FCompletionPort <> INVALID_HANDLE_VALUE then
    Result := CreateIoCompletionPort( ASocket, FCompletionPort, Cardinal(ACompletionKey), 0 ) <> 0;
end;

function TTcpIocpServer.SendBufferEx(const ASocket: TSocket; const AOverlapped: POverlapped; const ABuffer: PChar;
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

procedure TTcpIocpServer.SetActive(const Value: Boolean);
begin
//  if (FActive <> Value) and (not (csDesigning in ComponentState) ) then
  if FActive <> Value then
  begin
    if Value then
    begin
      if not OpenTcpIocp then
      begin
        CloseTcpIocp;
        FActive := False;
      end;
      FActive := True;
    end
    else
    begin
      CloseTcpIocp;
      FActive := False;
    end;
  end;
end;

procedure TTcpIocpServer.SetAllowTimeOut(const Value: Integer);
begin
  FAllowmaxNoActive_Pending := Value;
end;

procedure TTcpIocpServer.SetCheckConnectedSpaceTime(const Value: Integer);
begin
  FCheckConnectedSpaceTime := Value;
end;

procedure TTcpIocpServer.SetInitListenCount(const Value: Integer);
begin
  if (not Active) and (FInitListenCount <> Value) then
    FInitListenCount := Value;
end;

procedure TTcpIocpServer.SetInitPostAccept(const Value: Integer);
begin
  if (not Active) and (FInitPostAccept <> Value) then
    FInitPostAccept := Value;
end;

procedure TTcpIocpServer.SetMaxbufferCount(const Value: Integer);
begin
  if not Active then
    FMaxBufferCount := Value;
end;

procedure TTcpIocpServer.SetMaxBufferLength(const Value: Cardinal);
begin
  if not Active then
    FMaxBufferLength := Value;
end;

procedure TTcpIocpServer.SetMaxSocketCount(const Value: Integer);
begin
  if not Active then
    FMaxSocketCount := Value;
end;

procedure TTcpIocpServer.SetPort(const Value: u_short);
begin
  if (not Active) and (FPort <> Value) then
    FPort := Value;
end;

procedure TTcpIocpServer.SetWorkThreadCount(const Value: Integer);
begin
  if (not Active) and (FWorkThreadCount <> Value) then
    FWorkThreadCount := Value;
end;

procedure TTcpIocpServer.CloseTcpIocp;
begin
  if Assigned(FCheckConnectedThread) then
  begin
    FCheckConnectedThread.Terminate;
    FCheckConnectedThread := nil;
  end;
  if Assigned(FCheckPendingThread) then
  begin
    FCheckPendingThread.Terminate;
    FCheckPendingThread := nil;
  end;

  if FCmdSocket <> INVALID_SOCKET then
  begin
    GracefullyCloseSocket( FCmdSocket );
    FCmdSocket := INVALID_SOCKET;
  end;
  if FCompletionPort <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle( FCompletionPort );
    FCompletionPort := INVALID_HANDLE_VALUE;
  end;
  if Assigned(FClientSocketManage) then
  begin
    FClientSocketManage.Free;
    FClientSocketManage := nil;
  end;
  if Assigned(FHandleIOManage) then
  begin
    FHandleIOManage.Free;
    FHandleIOManage := nil;
  end;
  if Assigned(FPendingSocket) then
    FPendingSocket.Clear;
  if Assigned(FConnectSocket) then
    FConnectSocket.Clear;
end;

function TTcpIocpServer.GracefullyCloseSocket(const ASocket: TSocket; const AMaxWaitTime: u_short): Boolean;
var
  WaitTime:TLinger;
begin
  Result := False;
  shutdown( ASocket, SD_BOTH );
  if AMaxWaitTime > 0 then
    WaitTime.l_onoff := 1
  else
    WaitTime.l_onoff := 0;
  WaitTime.l_linger:= AMaxWaitTime;
  SetSockOpt( ASocket, SOL_SOCKET, SO_LINGER, @WaitTime, sizeOf(TLinger) );
//  Result := DisConnectEx( ASocket, nil ); //会阻塞,故直接关闭套按字
  if not Result then
    closesocket( ASocket );
end;

procedure TTcpIocpServer.InitList(var AList: TList);
begin
  if Assigned(AList) then
    AList.Clear
  else
    AList := TList.Create;
end;

procedure TTcpIocpServer.InitStat;
begin
  FCurActiveThreadCount := 0;
  FCurPendingIndex := 0;
  FCheckPendingCount := 0;
  FCurConnectedIndex := 0;
  FCheckConnectedCount := 0;
  FTotalRecvByte := 0;
  FTotalSendByte := 0;
end;

procedure Startup;
var
  ErrorCode: Integer;
  WSAData: TWSAData;
begin
  ErrorCode := WSAStartup(WINSOCK_VERSION, WSAData);
  if ErrorCode <> 0 then
    raise TTcpIocpException.CreateFmt('WSAStartup Error: %d', [ErrorCode]);
end;

procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    raise TTcpIocpException.CreateFmt('WSACleanup Error: %d', [ErrorCode]);
end;

{ TClientSocketClass }

constructor TClientSocketClass.Create(AOwner: TTcpIocpServer; AMaxSocketCount: Integer; NotifyInit: TOnInitClientSocket);
var
  i: Integer;
  p: pClientSocket;
begin
  FOwner := AOwner;
  FRecordSize := sizeof(TClientSocket);
  FMaxSocketCount := AMaxSocketCount;
  FMemoryManage := TxdFixedMemoryManager.Create( FRecordSize, AMaxSocketCount );
  for i := 0 to FMaxSocketCount - 1 do
  begin
//    p := FMemoryManage.Item[i];
    p^.SocketHandle := FOwner.CreateOverLappedSocket;
    p^.Checking := False;
    InitializeCriticalSection( p^.Lock );
    if Assigned(NotifyInit) then
      NotifyInit(Self, p);
  end;
end;

destructor TClientSocketClass.Destroy;
var
  i: Integer;
  p: pClientSocket;
begin
  for i := 0 to FMaxSocketCount - 1 do
  begin
//    p := pClientSocket( FMemoryManage.Item[i] );
    if p <> nil then
    begin
      shutdown( p^.SocketHandle, SD_BOTH );
      closesocket( p^.SocketHandle );
      if Assigned(FOnDestroyClientSocket) then
        FOnDestroyClientSocket(Self, p);
      p^.SocketHandle := INVALID_SOCKET;
      DeleteCriticalSection( p^.Lock );
    end;
  end;
  FMemoryManage.Free;
  inherited;
end;

procedure TClientSocketClass.ReclaimClientSocket(p: pClientSocket);
var
  s: TSocket;
  Lock: TRTLCriticalSection;
  pData: Pointer;
begin
  if not FOwner.GracefullyCloseSocket(p^.SocketHandle, 0) then
    p^.SocketHandle := FOwner.CreateOverLappedSocket;
  s := p^.SocketHandle;
  Lock := p^.Lock;
  pData := p^.Data;
  ZeroMemory(p, FRecordSize);
  p^.SocketHandle := s;
  p^.Lock := Lock;
  p^.Checking := False;
  p^.Data := pData;
  FMemoryManage.FreeMem( p );
  if Assigned(FOnReclaimClientSocket) then
    FOnReclaimClientSocket(Self, p);
end;

function TClientSocketClass.GetClientSocket(var p: pClientSocket): Boolean;
begin
  Result := FMemoryManage.GetMem( Pointer(p) );
end;

function TClientSocketClass.SpaceCount: Integer; 
begin
//  Result := FMemoryManage.SpaceCount;
end;

function TClientSocketClass.UsedCount: Integer;
begin
//  Result := FMemoryManage.UsedCount;
end;

{ THandleIOClass }

constructor THandleIOClass.Create(AMaxBufferCount, AMaxBufferLength: Integer);
var
  i: Integer;
  p: pHandleIO;
  nLen: Integer;
begin
  FMaxBufferLength := AMaxBufferLength;
  nLen := sizeof(THandleIOBuffer);
  FRecordSize := nLen + AMaxBufferLength;
  FMemoryManage := TxdFixedMemoryManager.Create( FRecordSize, AMaxBufferCount );
  for i := 0 to AMaxBufferCount - 1 do
  begin
//    p := FMemoryManage.Item[i];
    p^.CurBuffer.buf := PChar(p) + nLen;
    p^.CurBuffer.len := AMaxBufferLength;
  end;
end;

destructor THandleIOClass.Destroy;
begin
  FMemoryManage.Free;
  inherited;
end;

procedure THandleIOClass.ReclaimHandleBuffer(p: pHandleIO);
begin
  ZeroMemory( p, FRecordSize );
  p^.CurBuffer.buf := PChar(p) + Sizeof(THandleIOBuffer);
  p^.CurBuffer.len := FMaxBufferLength;
  FMemoryManage.FreeMem( p );
end;

function THandleIOClass.GetHandleBuffer(var p: pHandleIO): Boolean;
begin
  Result := FMemoryManage.GetMem( Pointer(p) );
end;

function THandleIOClass.SpaceCount: Integer;
begin
//  Result := FMemoryManage.SpaceCount;
end;

function THandleIOClass.UsedCount: Integer;
begin
//  Result := FMemoryManage.UsedCount;
end;

{ TWorkThread }

constructor TWorkThread.Create(AOwner: TTcpIocpServer);
begin
  FOwner := AOwner;
  FreeOnTerminate := True;
  inherited Create( False );
end;

destructor TWorkThread.Destroy;
begin

  inherited;
end;

procedure TWorkThread.Execute;
begin
  while not Terminated do
  begin
    FOwner.DoWorkThreadRunning( Self );
  end;
end;

{ TCheckThread }

constructor TCheckThread.Create(AOwner: TTcpIocpServer; ANotifyEvent: TNotifyEvent; ACheckSpaceTime: Integer);
begin
  FOwner := AOwner;
  FOnExecute := ANotifyEvent;
  FreeOnTerminate := True;
  FWaitEvent := CreateEvent( nil, False, False, nil );
  FCheckSpaceTime := ACheckSpaceTime;
  inherited Create( False );
end;

destructor TCheckThread.Destroy;
begin
  CloseHandle( FWaitEvent );
  inherited;
end;

procedure TCheckThread.Execute;
begin
  if not Assigned(FOnExecute) then Exit;
  while not Terminated do
  begin
    WaitForSingleObject( FWaitEvent, FCheckSpaceTime );
    if Terminated then Break;
    FOnExecute(Self);
  end;
end;

procedure TCheckThread.SetCheckSpaceTime(const Value: Integer);
begin
  FCheckSpaceTime := Value;
end;

procedure TCheckThread.Terminate;
begin
  inherited;
  SetEvent( FWaitEvent );
end;

initialization
  Startup;
finalization
  Cleanup;

end.
