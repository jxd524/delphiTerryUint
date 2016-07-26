{
单元名称: uJxdTCPEventSelectServer
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: 使用 WSAEventSelect IO 模式的TCP服务器
开始时间: 2010-09-26
修改时间: 2010-09-26(最后修改)

说    明：

分成监听线程与客户端套接字线程（自动生成N个）
监听线程只针对监听套接字 监听事件：ACCEPT
客户端接收线程：每个线程最多监听64个客户端； 监听事件：READ CLORE



                                     =======================
　　                   =======================触发条件说明=======================
                                     =======================
　　[1]FD_READ事件触发条件：
　　1.在数据到达socket后，并且从来没有触发过FD_READ(也就是最开始的阶段)
　　2.在数据到达socket后，并且前一个recv()调用后
　　3.调用recv()后，缓冲区还有未读完的数据
　　第3点过程如下：
　　1.100 bytes 数据到达,winsock2发出FD_READ
　　2.程序用recv()只读入50 bytes,还剩下50 bytes
　　3.winsock2继续发出FD_READ消息
　　recv()返回WSAEWOULDBLOCK的情况：
　　1.有数据到达，FD_READ触发，该消息加入程序的消息队列
　　2.在还没处理该消息前，程序就把数据recv()了
　　3.等到处理该FD_READ消息时，程序调用recv()就会返回WSAEWOULDBLOCK(因为数据在这之前就recv()了)
　　注意：
　　1.winsock2发出一个FD_READ后，如果程序没有用recv()，即使还有数据没接收FD_READ也不会再触发另一个FD_READ，要等到recv()调用后FD_READ才会发出。
　　2.对一个FD_READ多次recv()的情形：如果程序对一个FD_READ多次recv()将会造成触发多个空的FD_READ，所以程序在第 2次recv()前要关掉FD_READ(可以使用WSAAsynSelect关掉FD_READ)，然后再多次recv()。
　　3.recv()返回WSAECONNABORTED,WSAECONNRESET...等消息，可以不做任何处理，可以等到FD_CLOSE事件触发时再处理
　　=====================
　　[2]FD_ACCEPT事件触发条件：
　　1.当有请求建立连接，并且从来没有触发过FD_ACCEPT(也就是最开始的阶段)
　　2.当有请求建立连接，并且前一个accept()调用后
　　注意：当FD_ACCEPT触发后，如果程序没有调用accept(),即使还有建立连接的请求FD_ACCEPT也不会触发，要直到accept()调用后
　　========================
　　[3]FD_WRITE事件触发条件：
　　1.第一次connect()或accept()后(即连接建立后)
　　2.调用send()返回WSAEWOULDBLOCK，并且直到发送缓冲区准备好(为空)后
　　注意：当前一次调用send()没有返回WSAEWOULDBLOCK时，如果缓冲区准备好了，也不会触发FD_WRITE的
　　========================
　　[4]FD_CLOSE事件触发条件：自己或对端中断连接后
　　注意：closesocket()调用后FD_CLOSE不会触发
　　========================
　　[5]FD_CONNECT事件触发条件：调用了connect()，并且连接建立后。
}

unit uJxdTCPEventSelectServer;

{$DEFINE Debug}

interface
uses
  Windows, Classes, SysUtils, RTLConsts, uSocketSub, WinSock2, uJxdThread, Forms
  {$IFDEF Debug}, uDebugInfo {$ENDIF}
  ;

type
  PClientSocketInfo = ^TClientSocketInfo;
  TClientSocketInfo = record
    FSocket: TSocket;
    FAddr: TSockAddr;
  end;

  {$M+}
  TxdEventSelectServer = class;
   ETCPError = class(Exception);

  TxdClientSocketThread = class(TThread)
  public
    constructor Create(AOwner: TxdEventSelectServer);
    destructor  Destroy; override;

    //Result:
    //>= 0: 正常； -1: 没有空位；-2：函数调用失败
    function AddClientSocket(const ASocket: TSocket; const AAddr: TSockAddr): Integer;
    function DeleteSocket(const ASocket: TSocket): Integer;
    function Count: Integer;
  protected
    FCount: Integer;
    FOwner: TxdEventSelectServer;
    FLock: TRTLCriticalSection;
    FClientSocks: array[0..WSA_MAXIMUM_WAIT_EVENTS - 1] of TClientSocketInfo;
    FSocketEvents: array[0..WSA_MAXIMUM_WAIT_EVENTS - 1] of Cardinal;
    procedure Execute; override;
    procedure LockClientSockets(const ALock: Boolean);
    procedure OnDeleteClient(AIndex: Integer);
    procedure OnRecvBuffer(AIndex: Integer);
    procedure RemoveArray(const AIndex: Integer);
    procedure FreeClientSockets;
  end;

  TxdEventSelectServer = class
  public
    constructor Create;
    destructor  Destroy; override;

    procedure DeleteClientSocket(const ASocket: TSocket);
    function _SendBuffer(const ASocket: TSocket; const ABuffer: PChar; const ABufLen: Integer): Integer;
  protected
    //子类处理函数
    {是否接收新的客户端连接 Result: True 接收此套接字，False: 直接关闭此套接字}
    function  OnJuageCleint(const ASocket: TSocket; const AAddr: TSockAddr): Boolean; virtual;
    {新增连接}
    procedure OnNewClient(const ASocket: TSocket; const AAddr: TSockAddr); virtual;
    {接收到客户端数据}
    procedure OnRecvBuffer(const ASocket: TSocket; const AAddr: TSockAddr; const ABuffer: PChar; const ABufLen: Integer); virtual;
    {客户端退出}
    procedure OnSocketDisconnect(const ASocket: TSocket; const AAddr: TSockAddr); virtual;
  private
    FClientSocketThreads: array of TxdClientSocketThread;
    FColseListenSocket: Boolean;
    FListenSocket: TSocket;
    FListenEvent: Cardinal;
    procedure ActiveServer;
    procedure UnActiveServer;
    procedure WinSocketError(AErrCode: Integer; AAPIName: PChar);
    procedure OnError(const AErrorInfo: string);

    {由监听线程调用 当有新连接时}
    procedure _OnNewSocketConnect;
    {由客户端处理线程调用 当删除客户时}
    procedure _OnDeleteSocket(pSocket: PClientSocketInfo);
    {由客户端处理线程调用  当可以接收数据时}
    procedure _OnRecvBuffer(pSocket: PClientSocketInfo);

    procedure DoThreadListentSocket;
  private
    FIsExclusitve: Boolean;
    FPort: Word;
    FActive: Boolean;
    FIP: Cardinal;
    FListenCount: Integer;
    FEventWaitTime: Cardinal;
    FICCSTCount: Integer;
    procedure SetActive(const Value: Boolean);
    procedure SetExclusitve(const Value: Boolean);
    procedure SetIP(const Value: Cardinal);
    procedure SetPort(const Value: Word);
    procedure SetListenCount(const Value: Integer);
    procedure SetEventWaitTime(const Value: Cardinal);
    function  GetClientCount: Integer;
  published
    property Active: Boolean read FActive write SetActive;
    property Port: Word read FPort write SetPort;
    property IP: Cardinal read FIP write SetIP;
    property InitCreateClientSockThreadCount: Integer read FICCSTCount write FICCSTCount;// 初始化创建客户端接收线程，每个线程可监听64个客户端操作
    property EventWaitTime: Cardinal read FEventWaitTime write SetEventWaitTime default 3000;
    property ListenCount: Integer read FListenCount write SetListenCount default 0;
    property IsExclusitve: Boolean read FIsExclusitve write SetExclusitve;     //防止套接字被别人监听

    property OnlineClientCount: Integer read GetClientCount;  //在线客户端数量
  end;
  {$M-}

procedure RaiseWinSocketError(AErrCode: Integer; AAPIName: PChar);

implementation

procedure RaiseWinSocketError(AErrCode: Integer; AAPIName: PChar);
begin
  raise ETCPError.Create( Format(sWindowsSocketError, [SysErrorMessage(AErrCode), AErrCode, AAPIName]) );
end;

const
  CtSockAddrLen = SizeOf(TSockAddr);
  CtClientSocketInfoSize = SizeOf(TClientSocketInfo);
  CtMaxBufferLen = 1024;

{ TxdEventSelectServer }

procedure TxdEventSelectServer.ActiveServer;
var
  SockAddr: TSockAddr;
  i: Integer;
begin
  try
    //创建Socket
    FListenSocket := socket( AF_INET, SOCK_STREAM, 0 );
    if FListenSocket = INVALID_SOCKET then
    begin
      WinSocketError( WSAGetLastError, 'socket' );
      RaiseWinSocketError( WSAGetLastError, 'socket' );
    end;
    //独占式套接字
    if FIsExclusitve and (not SetSocketExclusitveAddr( FListenSocket )) then
    begin
      WinSocketError( WSAGetLastError, 'SetSocketExclusitveAddr' );
      RaiseWinSocketError( WSAGetLastError, 'SetSocketExclusitveAddr' );
    end;
    FListenEvent := WSACreateEvent;
    if SOCKET_ERROR = WSAEventSelect( FListenSocket, FListenEvent, FD_ACCEPT ) then
    begin
      WinSocketError( WSAGetLastError, 'WSAEventSelect' );
      RaiseWinSocketError( WSAGetLastError, 'WSAEventSelect' );
    end;
    //配置本地绑定地址
    SockAddr := InitSocketAddr( IP, Port );
    //绑定地址
    if SOCKET_ERROR = bind( FListenSocket, @SockAddr, CtSockAddrLen ) then
    begin
      WinSocketError( WSAGetLastError, 'bind' );
      RaiseWinSocketError( WSAGetLastError, 'bind' );
    end;
    //启动监听线程
    FColseListenSocket := False;
    RunningByThread( DoThreadListentSocket );
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
    //创建
    if FICCSTCount > 0 then
    begin
      SetLength( FClientSocketThreads, FICCSTCount );
      for i := 0 to FICCSTCount - 1 do
        FClientSocketThreads[i] := TxdClientSocketThread.Create( Self );
    end;
    FActive := True;
  except
    UnActiveServer;
  end;
end;

constructor TxdEventSelectServer.Create;
begin
  FListenSocket := INVALID_SOCKET;
  FPort := 9829;
  FIP := 0;
  FListenEvent := 0;
  FActive := False;
  FListenCount := 0;
  FEventWaitTime := 3000;
  FIsExclusitve := False;
  FICCSTCount := 1;
end;

procedure TxdEventSelectServer.DeleteClientSocket(const ASocket: TSocket);
var
  i, j: Integer;
begin
  Exit; //暂时不处理，处理时需要注册同步问题，事件同步，对象同步
  if not Active then Exit;
  for i := Low(FClientSocketThreads) to High(FClientSocketThreads) do
  begin
    if Assigned(FClientSocketThreads[i]) then
    begin
      for j := 0 to FclientSocketThreads[i].Count - 1 do
      begin
        if FClientSocketThreads[i].DeleteSocket(ASocket) >= 0 then
          Exit;
      end;
    end;
  end;
end;

destructor TxdEventSelectServer.Destroy;
begin
   Active := False;
  inherited;
end;

procedure TxdEventSelectServer.DoThreadListentSocket;
var
  waitResult: Cardinal;
  netEvent: TWSANetworkEvents;
begin
  while True do
  begin
    waitResult := WaitForSingleObject( FListenEvent, EventWaitTime );
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
      _OnNewSocketConnect;
    end
    else
      OutputDebugString( '未知信息，不应该出现的情况' );
  end;
  FColseListenSocket := True;
end;

function TxdEventSelectServer.GetClientCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Low(FClientSocketThreads) to High(FClientSocketThreads) do
  begin
    if Assigned(FClientSocketThreads[i]) then
      Inc( Result, FClientSocketThreads[i].Count );
  end;
end;

procedure TxdEventSelectServer._OnDeleteSocket(pSocket: PClientSocketInfo);
begin
  OnSocketDisconnect( pSocket^.FSocket, pSocket^.FAddr );
end;

procedure TxdEventSelectServer.OnError(const AErrorInfo: string);
begin
  {$IFDEF Debug}
  _Log( AErrorInfo, 'xdEventSelectServerDebug.txt' );
  {$ENDIF}
end;

function TxdEventSelectServer.OnJuageCleint(const ASocket: TSocket; const AAddr: TSockAddr): Boolean;
begin
  Result := True;
end;

procedure TxdEventSelectServer.OnNewClient(const ASocket: TSocket; const AAddr: TSockAddr);
begin

end;

procedure TxdEventSelectServer._OnNewSocketConnect;
var
  sAccept: TSocket;
  SockAddr: TSockAddr;
  i, addrLen, addFlat: Integer;
  bOK: Boolean;
begin
  //只有一个线程对它进行操作，不需要加锁
  addrLen := CtSockAddrLen;
  sAccept := accept( FListenSocket, SockAddr, addrLen );
  if sAccept = INVALID_SOCKET then
  begin
    WinSocketError( WSAGetLastError, 'accept' );
    Exit;
  end;
  if not OnJuageCleint( sAccept, SockAddr) then
  begin
    closesocket( sAccept );
    Exit;
  end;
  //OutputDebugString( PChar('有用户连接上服务器: ' + IpToStr( SockAddr.sin_addr.S_addr, SockAddr.sin_port )) );
  bOK := False;
  addFlat := -1;
  for i := Low(FClientSocketThreads) to High(FClientSocketThreads) do
  begin
    addFlat := FClientSocketThreads[i].AddClientSocket( sAccept, SockAddr );
    if addFlat >= 0 then
    begin
      bOK := True;
      Break;
    end;
    if addFlat = -2 then
    begin
      OnError( '无法添加新连接' );
      Exit;
    end;
  end;
  if not bOK and (addFlat < 0) then
  begin
    i := Length(FClientSocketThreads) + 1;
    SetLength( FClientSocketThreads, i );
    Dec( i );
    FClientSocketThreads[i] := TxdClientSocketThread.Create( Self );
    if -2 = FClientSocketThreads[i].AddClientSocket(sAccept, SockAddr) then
    begin
      OnError( '无法添加新连接' );
      Exit;
    end;
  end;
  OnNewClient( sAccept, SockAddr );
end;

procedure TxdEventSelectServer.OnRecvBuffer(const ASocket: TSocket; const AAddr: TSockAddr; const ABuffer: PChar;
  const ABufLen: Integer);
begin
  OutputDebugString( PChar('接收到数据：' + IpToStr(AAddr.sin_addr.S_addr, AAddr.sin_port) + ABuffer) );
end;

procedure TxdEventSelectServer.OnSocketDisconnect(const ASocket: TSocket; const AAddr: TSockAddr);
begin
  OutputDebugString( PChar('客户端退出：' + IpToStr(AAddr.sin_addr.S_addr, AAddr.sin_port)) );
end;

procedure TxdEventSelectServer._OnRecvBuffer(pSocket: PClientSocketInfo);
var
  buf: WSABUF;
  aryBuf: array[0..CtMaxBufferLen - 1] of Byte;
  recvByte, flat: Cardinal;
begin
  buf.len := CtMaxBufferLen;
  buf.buf := @aryBuf;
  flat := 0;
  if SOCKET_ERROR = WSARecv(pSocket^.FSocket, @buf, 1, recvByte, flat, nil, nil) then
  begin
    WinSocketError( WSAGetLastError, 'WSARecv' );
    Exit;
  end;
  OnRecvBuffer( pSocket^.FSocket, pSocket^.FAddr, @aryBuf, recvByte );
end;

function TxdEventSelectServer._SendBuffer(const ASocket: TSocket; const ABuffer: PChar; const ABufLen: Integer): Integer;
var
  buf: WSABUF;
  SendByte: Cardinal;
begin
  Result := -1;
  if not Active then Exit;
  buf.len := ABufLen;
  buf.buf := ABuffer;
  if SOCKET_ERROR = WSASend(ASocket, @buf, 1, SendByte, 0, nil, nil) then
    WinSocketError( WSAGetLastError, 'WSASend' );
  Result := SendByte;
end;

procedure TxdEventSelectServer.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      ActiveServer
    else
      UnActiveServer;
  end;
end;

procedure TxdEventSelectServer.SetEventWaitTime(const Value: Cardinal);
begin
  FEventWaitTime := Value;
end;

procedure TxdEventSelectServer.SetExclusitve(const Value: Boolean);
begin
  if not Active then
    FIsExclusitve := Value;
end;

procedure TxdEventSelectServer.SetIP(const Value: Cardinal);
begin
  if not Active then
    FIP := Value;       
end;

procedure TxdEventSelectServer.SetListenCount(const Value: Integer);
begin
  if not Active then
    FListenCount := Value;
end;

procedure TxdEventSelectServer.SetPort(const Value: Word);
begin
  if not Active then
    FPort := Value;
end;

procedure TxdEventSelectServer.UnActiveServer;
var
  i: Integer;
begin
  try
    FActive := False; //关闭所有线程
    //等待监听线程退出
    while not FColseListenSocket do
    begin
      Sleep( 20 );
      Application.ProcessMessages;
    end;
    //释放监听Socket资源
    if FListenSocket <> INVALID_SOCKET then
    begin
      shutdown( FListenSocket, SD_BOTH );
      closesocket( FListenSocket );
      FListenSocket := INVALID_SOCKET;
    end;
    //释放监听所有事件
    if 0 <> FListenEvent then
    begin
      CloseHandle( FListenEvent );
      FListenEvent := 0;
    end;
    //释放所有客户端连接信息
    for i := Low(FClientSocketThreads) to High(FClientSocketThreads) do
      FClientSocketThreads[i].Free;
    SetLength( FClientSocketThreads, 0 );
  finally
    FActive := False;
  end;
end;

procedure TxdEventSelectServer.WinSocketError(AErrCode: Integer; AAPIName: PChar);
begin
  OnError( Format(sWindowsSocketError, [SysErrorMessage(AErrCode), AErrCode, AAPIName]) );
end;

{ TxdClientSocketThread }

function TxdClientSocketThread.AddClientSocket(const ASocket: TSocket; const AAddr: TSockAddr): Integer;
begin
  //Result:
  //>= 0: 正常； -1: 没有空位；-2：函数调用失败
  Result := -1;
  LockClientSockets( True );
  try
    if FCount >= WSA_MAXIMUM_WAIT_EVENTS then Exit;
    with FClientSocks[FCount] do
    begin
      FSocket := ASocket;
      FAddr := AAddr;
      FSocketEvents[FCount] := WSACreateEvent;
      if SOCKET_ERROR = WSAEventSelect(FSocket, FSocketEvents[FCount], FD_READ or FD_CLOSE) then
      begin
        FOwner.WinSocketError( WSAGetLastError, 'WSAEventSelect' );
        Result := -2;
        closesocket( FSocket );
        CloseHandle( FSocketEvents[FCount] );
        Exit;
      end;
    end;
    Result := FCount;
    Inc( FCount );
  finally
    LockClientSockets( False );
  end;
end;

function TxdClientSocketThread.Count: Integer;
begin
  Result := FCount;
end;

constructor TxdClientSocketThread.Create(AOwner: TxdEventSelectServer);
begin
  FOwner := AOwner;
  FCount := 0;
  InitializeCriticalSection( FLock );
  inherited Create( False );
end;

function TxdClientSocketThread.DeleteSocket(const ASocket: TSocket): Integer;
var
  i: Integer;
begin
  //Result:
  //>= 0: 正常； -1: 没有找到；-2：函数调用失败
  Result := -1; 
  for i := 0 to FCount - 1 do
  begin
    if FClientSocks[i].FSocket = ASocket then
    begin
      Result := i;
      Break;
    end;
  end;
end;

destructor TxdClientSocketThread.Destroy;
begin
  FreeClientSockets;
  DeleteCriticalSection( FLock );
  inherited;
end;

procedure TxdClientSocketThread.Execute;
var
  waitResult, nIndex: Cardinal;
  netEvent: TWSANetworkEvents;
begin
  while FOwner.Active do
  begin
    if FCount = 0 then
    begin
      Sleep( 10 );
      Continue;
    end;
    waitResult := WaitForMultipleObjects( FCount, @FSocketEvents, False, FOwner.EventWaitTime );
    if waitResult = WSA_WAIT_TIMEOUT then Continue;

    nIndex := waitResult - WAIT_OBJECT_0;

    if SOCKET_ERROR = WSAEnumNetworkEvents( FClientSocks[nIndex].FSocket, FSocketEvents[nIndex], @netEvent ) then
    begin
      FOwner.WinSocketError( WSAGetLastError, 'WSAEnumNetworkEvents' );
      OnDeleteClient( nIndex );
      LockClientSockets( True );
      try
        RemoveArray( nIndex );
      finally
        LockClientSockets( False );
      end;
      Break;
    end;

    if (netEvent.lNetworkEvents and FD_READ) <> 0 then
    begin
      //有数据到来, 可读
      if netEvent.iErrorCode[FD_READ_BIT] <> 0 then
      begin
        FOwner.OnError( Format( 'FD_READ failed with error %d',[netEvent.iErrorCode[FD_ACCEPT_BIT] ] ));
        Continue;
      end;
      OnRecvBuffer( nIndex );
    end
    else if (netEvent.lNetworkEvents and FD_CLOSE) <> 0 then
    begin
      //关闭连接
      OnDeleteClient( nIndex );
      LockClientSockets( True );
      try
        RemoveArray( nIndex );
      finally
        LockClientSockets( False );
      end;
    end;
  end;
end;

procedure TxdClientSocketThread.FreeClientSockets;
var
  i: Integer;
begin
  if FCount = 0 then Exit;
  for i := 0 to FCount - 1 do
    OnDeleteClient( i );
  FCount := 0;
end;

procedure TxdClientSocketThread.LockClientSockets(const ALock: Boolean);
begin
  if ALock then
    EnterCriticalSection( FLock )
  else
    LeaveCriticalSection( FLock );
end;

procedure TxdClientSocketThread.OnDeleteClient(AIndex: Integer);
var
  pSocket: PClientSocketInfo;
begin
  pSocket := @FClientSocks[AIndex];
  FOwner._OnDeleteSocket( pSocket );
  shutdown( pSocket^.FSocket, SD_BOTH );
  closesocket( pSocket^.FSocket );
  CloseHandle( FSocketEvents[AIndex] );
end;

procedure TxdClientSocketThread.OnRecvBuffer(AIndex: Integer);
begin
  FOwner._OnRecvBuffer( @FClientSocks[AIndex] );
end;

procedure TxdClientSocketThread.RemoveArray(const AIndex: Integer);
begin
  if (FCount > 1) and (AIndex >= 0) and (AIndex < FCount - 1) then
  begin
    Move( FClientSocks[AIndex + 1], FClientSocks[AIndex], (FCount - AIndex - 1) * CtClientSocketInfoSize );
    Move( FSocketEvents[AIndex + 1], FSocketEvents[AIndex], (FCount - AIndex - 1) * 4 );
  end;
  Dec( FCount );
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
