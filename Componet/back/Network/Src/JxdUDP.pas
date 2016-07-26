unit JxdUDP;

interface

uses
  Windows, Classes, SysUtils, WinSock2, JxdProxy;

const
  CtUDPDefaultBufferSize = 4096 - 8; //特意减少8个字节 IP Port BufferLength

type

  TJxdUDPRecvThread = class;
  TJxdUDP = class;

  TPPUDPException = class(Exception);

  TPeerAddr = record
    PeerIP: string;
    PeerPort: integer;
  end;

  TJxdUDPErrorEvent = procedure(Sender: TObject; const AErrorMessage: string) of object;

  { 读数据事件 }
  TJxdUDPReadEvent = procedure(Sender: TObject; const PeerInfo: TPeerAddr) of object;


  //主要的UDP类
  TJxdUDP = class(TComponent)
  private
    FSocket: TSocket;
    FDefaultPort: integer;
    //错误处理事件
    FOnUDPError: TJxdUDPErrorEvent;
    //读数据事件
    FOnUDPRead: TJxdUDPReadEvent;
    //发送和接受缓冲大小
    FBufferSize: Integer;
    //记录接受到数据的远程机器的信息
    FPeerInfo: TPeerAddr;

    //判断是否打开了套接字
    FActive: Boolean;
    FBroadcast: Boolean;
    FProxySettings: TProxySettings;
    //使用代理时保持连接的Tcp Socket
    FTcpSocket: TSocket;
    //代理服务器上的Udp映射地址信息
    FUdpProxyAddr: TSockAddrIn;
    FAutoIncPort: Boolean;
    FEnableProxy: Boolean;

    procedure InitSocket;
    procedure FreeSocket;
    procedure SetActive(const Value: Boolean);
    procedure DoUDPRead;
    procedure DoUDPError(AErrorMessage: string);
    //连接代理服务器
    function ConnectToProxy: Boolean;
    //Tcp握手
    function Handclasp(ASocket: TSocket; AuthenType: TAuthenType): Boolean;
    //建立Udp映射通道
    function MapUdpChannel(ASocket: TSocket;var AUdpProxyAddr: TSockAddrIn): Boolean;
    //通过Proxy发送数据
    function SendByProxy(ASocket: TSocket; var ABuffer; ABufferSize: Integer; ARemoteHost: TInAddr;
      ARemotePort: Word): Integer;
    //从Proxy接收数据
    procedure RecvByProxy(ASocket: TSocket; var Abuffer; var ABufferSize: Integer;  var ASockAddr: TSockAddr);
    procedure SetProxySettings(const Value: TProxySettings);
  protected
    FUDPRecvThread: TJxdUDPRecvThread;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    //测试代理
    function TestProxy(const AHost:string;APort:Word;const AUserName:string;const AUserPass:string):Boolean;
    //发送缓冲区数据
    function SendBuf(AHost: TInAddr; APort: WORD;var ABuffer; ABufferLength: Integer): Boolean;
    //发送文本
    function SendText(AHost: TInAddr; APort: Word;AText: string): Boolean;
    //两个发送广播消息的函数
    function BroadcastBuf(var ABuffer; ABufferLength: Integer; APort: Word): Boolean;
    function BroadcastText(AText: string; APort: Word): Boolean;
    //接收函数
    procedure RecvBuf(var ABuffer; var ABufferLength: Integer; var ASockAddr: TSockAddr);
    //接受到远程数据的Client信息
    property PeerInfo: TPeerAddr read FPeerInfo;
published
    //发送和接收缓冲区大小
    property BufferSize: Integer read FBufferSize write FBufferSize default CtUDPDefaultBufferSize;
    //监听端口
    property DefaultPort: Integer read FDefaultPort write FDefaultPort;
    property AutoIncPort: Boolean read FAutoIncPort write FAutoIncPort default False;
    //等待数据超时间 默认是$FFFFFFFF;
    //打开套接字
    property Active: Boolean read FActive write SetActive;
    //是否可以广播
    property EnableBroadcast: Boolean read FBroadcast write FBroadcast;
    //代理配置
    property ProxySettings: TProxySettings read FProxySettings write SetProxySettings;
    //有数据到达的事件
    property OnUDPRead: TJxdUDPReadEvent read FOnUDPRead write FOnUDPRead;
    //套接字发生错误事件
   property OnUDPError: TJxdUDPErrorEvent read FOnUDPError write FOnUDPError;
   property EnableProxy:Boolean read FEnableProxy write FEnableProxy default False;
  end;

  TJxdUDPRecvThread = class(TThread)
  private
    FOwner: TJxdUDP;
    FEvent: WSAEvent;
    FSocket: TSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TJxdUDP);
    destructor Destroy; override;
  end;

  TJxdUDPBag = class
  private
    FLSH: Int64;
    FLastRecvTime: Cardinal;
    FRecvText: string;
    FStrList: TStrings; //接受数据时保存数据的
    FBagCount: Integer;
  public
    constructor Create(ABagCount: Integer);
    destructor Destroy; override;
    //接收一个数据包
    procedure RecvABag(AText: string; ABagIndex: Integer);
    //得到合并后的字符串
    function UnitRecvStr: string;
    function RecvFinish: Boolean;
    property NewLSH: Int64 read FLSH write FLSH;
    property LastRecvTime: Cardinal read FLastRecvTime write FLastRecvTime;
    property BagCount: Integer read FBagCount write FBagCount;
    property RecvText: string read FRecvText write FRecvText;
  end;
  
implementation
uses
  RTLConsts;

var
  WSAData: TWSAData;

function GetSocketErrorMessage(AErrorCode: Cardinal; Op: string): string;
begin
  Result := Format(sWindowsSocketError, [SysErrorMessage(AErrorCode), AErrorCode, Op]);
end;

procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0202, WSAData);
  if ErrorCode <> 0 then
    raise TPPUDPException.CreateResFmt(@sWindowsSocketError,
      [SysErrorMessage(ErrorCode), ErrorCode, 'WSAStartup']);
end;

procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    raise TPPUDPException.CreateResFmt(@sWindowsSocketError,
      [SysErrorMessage(ErrorCode), ErrorCode, 'WSACleanup']);
end;

{ ThxUDPSocket }

function TJxdUDP.BroadcastBuf(var ABuffer; ABufferLength:Integer; APort: Word): Boolean;
var
  ret, ErrorCode: Integer;
  SockAddr: TSockAddr;
begin
  Result:= False;

  with SockAddr do
  begin
    sin_family := AF_INET;
    sin_port := htons(APort);
    sin_addr.S_addr := htonl(INADDR_BROADCAST);
  end;

  if FProxySettings.Enabled then
    ret:= SendByProxy(FSocket, ABuffer, ABufferLength, SockAddr.sin_addr, ntohs(SockAddr.sin_port))
  else
    ret:= sendto(FSocket, ABuffer, ABufferLength, 0, SockAddr, SizeOf(SockAddr));
  if ret = SOCKET_ERROR then
  begin
    ErrorCode:= GetLastError;
    if ErrorCode <> WSAEWOULDBLOCK then
    begin
      DoUDPError(GetSocketErrorMessage(ErrorCode, 'Sendto'));
    end;
  end
  else
    Result:= True;
end;

function TJxdUDP.BroadcastText(AText: string; APort: Word): Boolean;
begin
  Result:= BroadcastBuf(AText[1], Length(AText), APort);
end;

constructor TJxdUDP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FSocket := INVALID_SOCKET;
  FTcpSocket:= INVALID_SOCKET;
  FAutoIncPort := False;
  FBufferSize := CtUDPDefaultBufferSize;
  FDefaultPort := 0;
  ZeroMemory(@FPeerInfo,SizeOf(TPeerAddr));
  FProxySettings:= TProxySettings.Create;
  FEnableProxy := False;
end;

destructor TJxdUDP.Destroy;
begin
  FProxySettings.Free;
  Close;
  inherited;
end;

procedure TJxdUDP.DoUDPError(AErrorMessage: string);
begin
  if Assigned(FOnUDPError) then
    FOnUDPError(Self, AErrorMessage);
end;

procedure TJxdUDP.FreeSocket;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    shutdown(FSocket, SD_BOTH);
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
  if FTcpSocket <> INVALID_SOCKET then
  begin
    shutdown(FTcpSocket,SD_BOTH);
    closesocket(FTcpSocket);
    FTcpSocket:= INVALID_SOCKET;
  end;
end;

procedure TJxdUDP.InitSocket;
var
  SockAddr: TSockAddr;
  ErrorCode: Integer;
  BufferSize: Integer;
begin
  if FDefaultPort = 0 then
    raise TPPUDPException.Create('监听的端口号为0!');
  FSocket := WSASocket(AF_INET, SOCK_DGRAM, 0, nil, 0, WSA_FLAG_OVERLAPPED);
  if FSocket = INVALID_SOCKET then
    raise TPPUDPException.Create(GetSocketErrorMessage(WSAGetLastError, 'WSASocket'));
  while True do
  begin
    SockAddr.sin_family := AF_INET;
    SockAddr.sin_addr.S_addr := htonl(INADDR_ANY);
    SockAddr.sin_port := htons(FDefaultPort);
    if bind(FSocket, @SockAddr, SizeOf(SockAddr)) = SOCKET_ERROR then
    begin
      ErrorCode := WSAGetLastError;
      if ((ErrorCode = WSAEADDRINUSE) or (ErrorCode = WSAEINVAL)) and FAutoIncPort then
      begin
        Inc(FDefaultPort);
        Continue;
      end
      else begin
        FreeSocket;
        raise TPPUDPException.Create(GetSocketErrorMessage(ErrorCode, 'bind'));
      end;
    end
    else
      Break;
  end;
  BufferSize := 8192;
  if (setsockopt(FSocket, SOL_SOCKET, SO_SNDBUF, @BufferSize, SizeOf(BufferSize)) = SOCKET_ERROR) or
    (setsockopt(FSocket, SOL_SOCKET, SO_RCVBUF, @BufferSize, SizeOf(BufferSize)) = SOCKET_ERROR) then
  begin
    ErrorCode := WSAGetLastError;
    FreeSocket;
    raise TPPUDPException.Create(GetSocketErrorMessage(ErrorCode, 'setsockopt'));
  end;

  //有代理时需先建立Udp映射通道
  if FProxySettings.Enabled then
  begin
    if not ConnectToProxy then
    begin
      DoUDPError('连接代理服务器失败！');
//      Exit;
    end;
  end;
end;

procedure TJxdUDP.DoUDPRead;
begin
  if Assigned(FOnUDPRead) then
  try
    FOnUDPRead(Self, FPeerInfo);
  except
    on E: Exception do
      DoUDPError(Format('OnUDPRead "%s" Excpetion: %s', [E.ClassName, E.Message]));
  end;
end;

procedure TJxdUDP.RecvBuf(var ABuffer; var ABufferLength: Integer;var ASockAddr: TSockAddr);
var
  Code: DWORD;
  SockAddrSize: Integer;
begin
  SockAddrSize := Sizeof(TSockAddr);
  if FProxySettings.Enabled then
    RecvByProxy(FSocket, ABuffer, ABufferLength, ASockAddr)
  else
    ABufferLength := recvfrom(FSocket, ABuffer, FBufferSize, 0, ASockAddr, SockAddrSize);

  with FPeerInfo do
  begin
    PeerIP:= inet_ntoa(ASockAddr.sin_addr);
    PeerPort:= ntohs(ASockAddr.sin_port);
  end;

  if ABufferLength = SOCKET_ERROR then
  begin
    Code := WSAGetLastError;
    if (Code <> WSAECONNRESET) and (Code <> WSAEWOULDBLOCK) then //暂时忽略这个问题
      DoUDPError(GetSocketErrorMessage(Code, 'recvfrom'));
  end;
end;

function TJxdUDP.SendBuf(AHost: TInAddr; APort: WORD;var ABuffer; ABufferLength: Integer): Boolean;
var
  ErrorCode: Integer;
  SockAddr: TSockAddr;
begin
//可以发送0长度的数据
  Result := False;
  if (AHost.S_addr = INADDR_NONE) or (APort = 0) or (AHost.S_addr = INADDR_ANY) or (ABufferLength < 0) then
    Exit;
  with SockAddr do
  begin
    sin_family := AF_INET;
    sin_port := htons(APort);
    sin_addr.S_addr := AHost.S_addr;
  end;
  if FProxySettings.Enabled then
    Result:= SendByProxy(FSocket, ABuffer, ABufferLength, AHost, APort)<> SOCKET_ERROR
  else
    Result := sendto(FSocket, ABuffer, ABufferLength, 0, SockAddr, Sizeof(SockAddr)) <> SOCKET_ERROR;
  if not Result then
  begin
    ErrorCode := WSAGetLastError;
    if ErrorCode <> WSAEWOULDBLOCK then
      DoUDPError(GetSocketErrorMessage(ErrorCode, 'sendto'));
  end;
end;

function TJxdUDP.SendText(AHost: TInAddr; APort: Word;AText:string): Boolean;
begin
  Result := SendBuf(AHost, APort,Pointer(AText)^, Length(AText));
end;

procedure TJxdUDP.SetActive(const Value: Boolean);
begin
  if not ((csDesigning in ComponentState) or (csLoading in ComponentState)) then
  begin
    if Value then
      Open
    else
      Close;
  end
  else begin
    FActive := Value;
  end;
end;

function TJxdUDP.TestProxy(const AHost:string;APort:Word;const AUserName:string;const AUserPass:string): Boolean;
var
  ATcpSocket: TSocket;
  saProxy: TSockAddrIn;
  ret: Integer;
  bRet: Boolean;
  aUdpProxyAddr: TSockAddrIn;
begin
  //建立到Proxy的Tcp连接
  ATcpSocket:= socket(AF_INET, SOCK_STREAM, 0);

  saProxy.sin_family:= AF_INET;
  saProxy.sin_port:= htons(APort);
  saProxy.sin_addr.S_addr:= inet_addr(PChar(AHost));
  ret:= connect(ATcpSocket, @saProxy, SizeOf(saProxy));
  if ret = SOCKET_ERROR then
    raise Exception.CreateFmt('无法连接到代理服务器，错误码是%d', [WSAGetLastError]);

  {代理服务器是否需要身份验证}
  if Trim(AUsername) <> '' then
    bRet:= Handclasp(ATcpSocket, atUserPass)
   else
    bRet:= Handclasp(ATcpSocket, atNone);

  if not bRet then
  begin
    closesocket(ATcpSocket);
    raise Exception.CreateFmt('代理服务器身份验证失败!错误码是%d', [WSAGetLastError]);
  end;

  //建立UDP映射通道
  if not MapUdpChannel(ATcpSocket,aUdpProxyAddr) then
  begin
    closesocket(ATcpSocket);
    raise Exception.CreateFmt('代理服务器不支持UDP!错误码是%d', [WSAGetLastError]);
  end;
  if ATcpSocket <> INVALID_SOCKET then
  begin
    closesocket(ATcpSocket);
  end;
  Result:= True;
end;

procedure TJxdUDP.Close;
begin
  if FActive then
  begin
    FActive := False;
    FreeAndNil(FUDPRecvThread);
    FreeSocket;
  end;
end;

function TJxdUDP.ConnectToProxy: Boolean;
var
  saProxy: TSockAddrIn;
  ret: Integer;
  bRet: Boolean;
begin
  //建立到Proxy的Tcp连接
  if FTcpSocket = INVALID_SOCKET then
    FTcpSocket:= socket(AF_INET, SOCK_STREAM, 0);

  saProxy.sin_family:= AF_INET;
  saProxy.sin_port:= htons(FProxySettings.Port);
  saProxy.sin_addr.S_addr:= inet_addr(PChar(FProxySettings.Host));

  ret:= connect(FTcpSocket, @saProxy, SizeOf(saProxy));
  if ret = SOCKET_ERROR then
    raise Exception.CreateFmt('无法连接到代理服务器，错误码是%d', [WSAGetLastError]);

  {代理服务器是否需要身份验证}
  if Trim(FProxySettings.Username) <> '' then
    bRet:= Handclasp(FTcpSocket, atUserPass)
   else
    bRet:= Handclasp(FTcpSocket, atNone);

  if not bRet then
  begin
    closesocket(FTcpSocket);
    raise Exception.CreateFmt('代理服务器身份验证失败!错误码是%d', [WSAGetLastError]);
  end;

  //建立UDP映射通道
  if not MapUdpChannel(FTcpSocket,FUdpProxyAddr) then
  begin
    closesocket(FTcpSocket);
    raise Exception.CreateFmt('代理服务器不支持UDP!错误码是%d', [WSAGetLastError]);
  end;
  Result:= True;
  FEnableProxy := True;
end;

function TJxdUDP.Handclasp(ASocket: TSocket; AuthenType: TAuthenType): Boolean;
var
  Buf: array[0..255] of Byte;
  I, Ret: Integer;
  Username, Password: string;
begin
  Result:= False;
  case AuthenType of
    // 无需验证
    atNone:
    begin
      Buf[0]:= $05;
      Buf[1]:= $01;
      Buf[2]:= $00;
      Ret:= send(ASocket, Buf, 3, 0);
      if Ret = -1 then Exit;
      FillChar(Buf, 256, #0);
      Ret:= recv(ASocket, Buf, 256, 0);
      if Ret < 2 then Exit;
      if Buf[1] <> $00 then Exit;
      Result:= True;
    end;
    // 用户名密码验证
    atUserPass:
    begin
      Buf[0]:= $05; // Socks版本号
      Buf[1]:= $02; // 两种认证方法
      Buf[2]:= $00; // 无需校验
      Buf[3]:= $02; // 需用户名密码校验
      Ret:= send(ASocket, Buf, 4, 0);
      if Ret = -1 then Exit;
      FillChar(Buf, 256, #0);
      Ret:= recv(ASocket, Buf, 256, 0);
      if Ret < 2 then Exit;
      if Buf[1] <> $02 then Exit;
      Username:= FProxySettings.Username;
      Password:= FProxySettings.Password;
      FillChar(Buf, 256, #0);
      Buf[0]:= $01;
      Buf[1]:= Length(Username);
      for I:= 0 to Buf[1] - 1 do
        Buf[2 + I]:= Ord(Username[I + 1]);
      Buf[2 + Length(Username)]:= Length(Password);
      for I:= 0 to Buf[2 + Length(Username)] - 1 do
        Buf[3 + Length(Username) + I]:= Ord(Password[I + 1]);
      Ret:= send(ASocket, Buf, Length(Username) + Length(Password) + 3, 0);
      if Ret = -1 then Exit;
      Ret:= recv(ASocket, Buf, 256, 0);
      if Ret = -1 then Exit;
      if Buf[1] <> $00 then Exit;
      Result:= True;
    end;
  end;
end;

function TJxdUDP.MapUdpChannel(ASocket: TSocket;var AUdpProxyAddr: TSockAddrIn): Boolean;
var
  saLocal: TSockAddrIn;
  NameLen: Integer;
  ProxyAddr: TInAddr;
  ProxyPort: Word;
  Buf: array[0..255] of Byte;
begin
  Result:= False;
  NameLen:= SizeOf(saLocal);
  getsockname(FSocket, saLocal, NameLen);
  Buf[0]:= $05; //协议版本Socks5
  Buf[1]:= $03; //Socks命令:UDP
  Buf[2]:= $00; //保留
  Buf[3]:= $01; //地址类型IPv4
  CopyMemory(@Buf[4], @saLocal.sin_addr, 4);
  CopyMemory(@Buf[8], @saLocal.sin_port, 2);
  send(ASocket, Buf, 10, 0);
  FillChar(Buf, 256, #0);
  recv(ASocket, Buf, 256, 0);
  if (Buf[0] <> $05) and (Buf[1] <> $00) then
    Exit;
  CopyMemory(@ProxyAddr, @Buf[4], 4); //获取Proxy的映射地址
  CopyMemory(@ProxyPort, @Buf[8], 2); //获取Proxy的映射端口号

  AUdpProxyAddr.sin_family:= AF_INET;
  AUdpProxyAddr.sin_port:= ProxyPort;
  AUdpProxyAddr.sin_addr:= ProxyAddr;

  Result:= True;
end;


procedure TJxdUDP.Open;
begin
  if not FActive then
  begin
    InitSocket;
    FUDPRecvThread := TJxdUDPRecvThread.Create(Self);
    FUDPRecvThread.Resume;
    FActive := True;
  end;
end;

function TJxdUDP.SendByProxy(ASocket: TSocket; var ABuffer; ABufferSize: Integer;
  ARemoteHost: TInAddr; ARemotePort: Word): Integer;
var
  TempBuf: array[0..8092-1] of Byte;
  saRemote: TSockAddrIn;
begin
  Result := -1;
  if (not FEnableProxy) and (not ConnectToProxy) then
    Exit;
  saRemote.sin_family:= AF_INET;
  saRemote.sin_port:= htons(ARemotePort);
  saRemote.sin_addr.S_addr:= ARemoteHost.S_addr;
  // 加上报头
  //FillChar(TempBuf, 8092, $0);
  ZeroMemory(@TempBuf,SizeOf(TempBuf));
  TempBuf[0]:= $00;  //保留
  TempBuf[1]:= $00;  //保留
  TempBuf[2]:= $00;  //是否分段重组(此处不用)
  TempBuf[3]:= $01;  //IPv4
  CopyMemory(@TempBuf[4], @saRemote.sin_addr, 4);    //远程服务器地址
  CopyMemory(@TempBuf[8], @saRemote.sin_port, 2);  //远程服务器端口
  CopyMemory(@TempBuf[10], @ABuffer, ABufferSize); //实际数据
  Result:= sendto(ASocket, TempBuf, ABufferSize + 10, 0, FUdpProxyAddr, SizeOf(FUdpProxyAddr));
  if Result = SOCKET_ERROR then
    raise Exception.CreateFmt('发送数据错误!错误号是%d', [WSAGetLastError]);
end;

procedure TJxdUDP.RecvByProxy(ASocket: TSocket; var Abuffer;var ABufferSize: Integer;
   var ASockAddr: TSockAddr);
var
  TempBuf: array[0..8092-1] of Byte;
  SockAddrSize: Integer;
begin
  if (not FEnableProxy) and (not ConnectToProxy) then
    Exit;
  SockAddrSize := Sizeof(TSockAddr);
  ZeroMemory(@TempBuf,SizeOf(TempBuf));

  ABufferSize:= recvfrom(ASocket, TempBuf, FBufferSize, 0, ASockAddr, SockAddrSize);
  if ABufferSize = SOCKET_ERROR then
    raise Exception.CreateFmt('接收数据错误!错误号是%d', [WSAGetLastError]);
  Assert(TempBuf[0] = $00);  //保留
  Assert(TempBuf[1] = $00);  //保留
  Assert(TempBuf[2] = $00);  //是否分段重组
  Assert(TempBuf[3] = $01);  //IPv4
  CopyMemory(@ASockAddr.sin_addr, @TempBuf[4], 4);  //代理服务器地址
  CopyMemory(@ASockAddr.sin_port, @TempBuf[8], 2);  //代理服务器端口
  ABufferSize := ABufferSize-10;
  CopyMemory(@Abuffer, @TempBuf[10], ABufferSize); //实际数据
end;

procedure TJxdUDP.SetProxySettings(const Value: TProxySettings);
begin
  FProxySettings.Assign(Value);
end;

{ TPPUDPRecvThread } 
constructor TJxdUDPRecvThread.Create(AOwner: TJxdUDP);
begin
  FOwner := AOwner;
  FSocket := FOwner.FSocket;
  FEvent := WSACreateEvent;
  if FEvent = WSA_INVALID_EVENT then
    TPPUDPException.CreateFmt('监听线程 WSACreateEvent error,Code:%d', [WSAGetLastError]);
  if WSAEventSelect(FSocket, FEvent, FD_READ) = SOCKET_ERROR then
    raise TPPUDPException.CreateFmt('监听线程 WSAEventSelect error,code:%d', [WSAGetLastError()]);
  inherited Create(True);
end;

destructor TJxdUDPRecvThread.Destroy;
begin
  Terminate;
  WSASetEvent(FEvent);
  WaitForSingleObject(Self.Handle, 5000); //等待线程执行完毕
  if FEvent <> WSA_INVALID_EVENT then
  begin
    WSACloseEvent(FEvent);
    FEvent := WSA_INVALID_EVENT;
  end;
  inherited;
end;

procedure TJxdUDPRecvThread.Execute;
var
  Code: Cardinal;
begin
  while not Terminated do
  begin
    Code := WSAWaitForMultipleEvents(1, @FEvent, False, INFINITE, False);
    if Terminated or (Code = WAIT_IO_COMPLETION) or (Code = WSA_WAIT_FAILED) then
      Exit;
    WSAResetEvent(FEvent);
    if Code = WAIT_OBJECT_0 then
      FOwner.DoUDPRead;
  end;
end;

{ TPPOldUDPBag }

constructor TJxdUDPBag.Create(ABagCount: Integer);
var
  i: Integer;
begin
  FLSH := -1;
  FLastRecvTime := GetTickCount;
  FRecvText := '';
  FBagCount := ABagCount;
  FStrList := TStringList.Create;
  for i := 0 to ABagCount - 1 do
  begin
    FStrList.Add('0');
  end;
end;

destructor TJxdUDPBag.Destroy;
begin
  FLSH := -1;
  FRecvText := '';
  FStrList.Free;
  inherited;
end;

procedure TJxdUDPBag.RecvABag(AText: string; ABagIndex: Integer);
begin
  if ABagIndex > FBagCount then
    Exit;
  if FBagCount = 1 then
  begin
    FRecvText := AText;
    Exit;
  end;
  FStrList.Strings[ABagIndex - 1] := AText;
end;

function TJxdUDPBag.RecvFinish: Boolean;
var
  i: Integer;
begin
  if FBagCount = 1 then
  begin
    Result := True;
    Exit;
  end;

  for i := 0 to FStrList.Count - 1 do
  begin
    if FStrList[i] = '0' then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
  //是接受完了，合拼一下数据
  FRecvText := UnitRecvStr;
end;

function TJxdUDPBag.UnitRecvStr: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FStrList.Count - 1 do
  begin
    Result := Result + FStrList[i];
  end;
end;


initialization
  Startup;
finalization
  Cleanup;
  
end.
