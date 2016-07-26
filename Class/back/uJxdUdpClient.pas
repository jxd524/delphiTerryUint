unit uJxdUdpClient;

{$DEFINE DebugLog}

interface
uses
  Windows, SysUtils, uJxdUdpIOHandle, WinSock2, uCmdStream, Encrypt, uSocketSub, Classes
  {$IFDEF DebugLog}, uDebugInfo{$EndIf}
  ;

type
  TAuthenType = (atNone, atUserPass);
  TProxyInfo = record
    Address: string;
    Port: Integer;
    Username: string;
    Password: string;
    AuthenType: TAuthenType;
  end;

  TxdUdpClient = class(TxdUdpIOHandle)
  public
    {代理测试}
    function TestProxy:Boolean;
    
    constructor Create;
    destructor  Destroy; override;
  protected
    //连接代理服务器
    function ConnectToProxy: Boolean;
    //Tcp握手
    function Handclasp(ASocket: TSocket): Boolean;
    //建立Udp映射通道
    function MapUdpChannel(ASocket: TSocket;var AUdpProxyAddr: TSockAddrIn): Boolean;
    function SendByProxy(ASocket: TSocket; var ABuffer; ABufferSize: Integer; ARemoteHost: TInAddr;
      ARemotePort: Word): Integer;

    function __SendTo(s: TSocket; var Buf; len, flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer; override;
  private
    //使用代理时保持连接的Tcp Socket
    FTcpSocket: TSocket;
    //代理服务器上的Udp映射地址信息
    FUdpProxyAddr: TSockAddrIn;
  private
    FProxyInfo: TProxyInfo;
    FProxyEnabled: Boolean;
    procedure SetProxyActive(const Value: Boolean);
  published
    property ProxyEnabled: Boolean read FProxyEnabled write SetProxyActive default False;
    property ProxyInfo: TProxyInfo read FProxyInfo write FProxyInfo;
  end;

implementation

procedure Debug(const AInfo: string); overload;
begin
{$IFDEF DebugLog}
  _Log( AInfo, 'TxdUdpClient_DebugInfo.txt' );
{$EndIf}
  OutputDebugString( PChar(AInfo) );
end;
procedure Debug(const AInfo: string; const Args: array of const); overload;
begin
  Debug( Format(AInfo, Args) );
end;

{ TxdUdpClient }

function TxdUdpClient.ConnectToProxy: Boolean;
var
  saProxy: TSockAddrIn;
  ret: Integer;
  bRet: Boolean;
begin
  //建立到Proxy的Tcp连接
  if FTcpSocket = INVALID_SOCKET then
    FTcpSocket:= socket( AF_INET, SOCK_STREAM, 0 );

  saProxy.sin_family:= AF_INET;
  saProxy.sin_port:= htons( ProxyInfo.Port );
  saProxy.sin_addr.S_addr:= inet_addr( PChar(ProxyInfo.Address) );

  ret:= connect( FTcpSocket, @saProxy, SizeOf(saProxy) );
  if ret = SOCKET_ERROR then
    raise Exception.CreateFmt( '无法连接到代理服务器，错误码是%d', [WSAGetLastError] );

  {代理服务器是否需要身份验证}
  bRet:= Handclasp( FTcpSocket );

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
  FProxyEnabled := True;
end;

constructor TxdUdpClient.Create;
begin
  FTcpSocket:= INVALID_SOCKET;
end;

destructor TxdUdpClient.Destroy;
begin

  inherited;
end;

function TxdUdpClient.Handclasp(ASocket: TSocket): Boolean;
var
  Buf: array[0..255] of Byte;
  I, Ret: Integer;
  Username, Password: string;
begin
  Result:= False;
  case ProxyInfo.AuthenType of
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
      Username:= ProxyInfo.Username;
      Password:= ProxyInfo.Password;
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

function TxdUdpClient.MapUdpChannel(ASocket: TSocket; var AUdpProxyAddr: TSockAddrIn): Boolean;
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

function TxdUdpClient.SendByProxy(ASocket: TSocket; var ABuffer; ABufferSize: Integer; ARemoteHost: TInAddr;
  ARemotePort: Word): Integer;
var
  TempBuf: array[0..8092-1] of Byte;
  saRemote: TSockAddrIn;
begin
  Result := -1;
  if (not FProxyEnabled) and (not ConnectToProxy) then Exit;
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

procedure TxdUdpClient.SetProxyActive(const Value: Boolean);
begin
  FProxyEnabled := Value;
end;

function TxdUdpClient.TestProxy: Boolean;
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
  saProxy.sin_port:= htons( ProxyInfo.Port );
  saProxy.sin_addr.S_addr:= inet_addr( PChar(ProxyInfo.Address) );
  ret:= connect(ATcpSocket, @saProxy, SizeOf(saProxy));
  if ret = SOCKET_ERROR then
    raise Exception.CreateFmt('无法连接到代理服务器，错误码是%d', [WSAGetLastError]);

  {代理服务器是否需要身份验证}
  bRet:= Handclasp( ATcpSocket );

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

function TxdUdpClient.__SendTo(s: TSocket; var Buf; len, flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer;
begin
//  if ProxyEnabled then
//    Result := SendByProxy()
//  else
    Result := inherited __SendTo( s, Buf, len, flags, addrto, tolen );
end;

end.
