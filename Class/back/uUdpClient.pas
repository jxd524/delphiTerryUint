{
单元名称: uUdpClient
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: UDP包的处理类.
开始时间: 2009-2-2
修改时间: 2009-2-4 (最后修改)
作    用: 处理UDP客户端,从 TUdpIOHandle 继承.添加同步发送机制, 利用流水号与时间戳
          对父类数据缓冲进行一般化合理初始化
引用单元:
          uUdpIOHandle
}

unit uUdpClient;

interface
uses Forms, WinSock2, uUdpIOHandle, Classes, Windows, uConversion;

type
  TUdpClient = class;
  
  TAuthenType = (atNone, atUserPass);
  TProxyInfo = record
    Address: string;
    Port: Integer;
    Username: string;
    Password: string;
  end;

  TReplyState = (rsWaitting, rsSuccess, rsLengthTooSmall);
  pSyncData = ^TSyncData; //同步数据
  TSyncData = record
    FTimeStamp: Cardinal;
    FSerialNum: Word;
    FReplyOK: TReplyState; //0: 等待中; 1: 成功; 2: FBufferLen提供的长度不够; 3: 找不到信息
    FBufferLen: Cardinal;
    FBuffer: Pointer;
  end;

  TProxyConnectThread = class(TThread)
  private
    FOwner: TUdpClient;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TUdpClient);
    destructor Destroy; override;
  end;

  TSyncDataList = class
  private
    FSyncLock: TRTLCriticalSection;
    FSyncDataList: TList;
    function  FindSyncData(const ATimeStamp: Cardinal; const ASerialNum: Word): Integer;
  public
    procedure AddSyncData(const ATimeStamp: Cardinal; const ASerialNum: Word; const ABufferLen: Cardinal; const AWaitSyncBuffer: Pointer);
    function  ModifySyncData(const ATimeStamp: Cardinal; const ASerialNum: Word; const ABufferLen: Cardinal; const ABuffer: Pointer): Boolean;
    function  IsSyncDataOK(const ATimeStamp: Cardinal; const ASerialNum: Word; const AForceDelete: Boolean): Integer; //-1: rsWaitting; -2: rsLengthTooSmall; -3无找到信息; > 0: 成功

    constructor Create;
    destructor Destroy; override;
  end;

  TOnHandleAsynBuffer = procedure(Sender: TObject; ARecvInfo: TRecvInfo) of object;
  TUdpClient = class(TUdpIOHandle)
  private
    FOnHandleAsynBuffer: TOnHandleAsynBuffer;
    FCurSerialNum: Word;
 private
    {sock5 代理}
    FTcpSocket: TSocket;
    FConnected: Integer;
    FUdpProxyAddr: TSockAddrIn;
    FProxyEnabled: Boolean;
    FProxyThread: TProxyConnectThread;
    FProxyInfo: TProxyInfo;
    procedure ConnectToProxy;
    function MapUdpChannel(Socket: TSocket): Boolean;
    function Handclasp(Socket: TSocket; AuthenType: TAuthenType): Boolean;
    procedure FreeTcpSocket;
    function  TestConnectToProxy: Boolean;
    procedure SetProxyActive(const Value: Boolean);
  protected
    FSyncDataList: TSyncDataList;
    function __SendTo(s: TSocket; var Buf; len, flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer; override;
    function  DoBeforOpenUDP: Boolean; override;  //初始化UDP前; True: 允许初始化; False: 不允许初始化
    procedure DoAfterCloseUDP; override; //UDP关闭之后
    procedure OnRecvBuffer(ARecvInfo: TRecvInfo); override; //同步, 异步数据在此分离
    procedure DoHandleAnsyBuffer(ARecvInfo: TRecvInfo); virtual; //异步数据处理
    function SendByProxy(const Socket: TSocket; var buf; len: Integer; saRemote: TSockAddr): Integer;
  public

    //发送同步包
    function  SendBufferBySync(AIP: Cardinal; AHostShortPort: word; ASendBuf, AReplyBuf: Pointer; const ASendBufLen: Integer;
                               var AReplyBufLen: Integer; ASerialNum: Word = 0; AWaitTime: Integer = 4000): Boolean;

    constructor Create; override;
    destructor Destroy; override;
  public
    property ProxyEnabled: Boolean read FProxyEnabled write SetProxyActive default False;
    property ProxyInfo: TProxyInfo read FProxyInfo write FProxyInfo;
    property OnHandleAsynBuffer: TOnHandleAsynBuffer read FOnHandleAsynBuffer write FOnHandleAsynBuffer;
    property UserID: Cardinal read FUserID;
    property CurSerialNum: Word read FCurSerialNum;
  end;


implementation

{ TSyncDataList }

procedure TSyncDataList.AddSyncData(const ATimeStamp: Cardinal; const ASerialNum: Word; const ABufferLen: Cardinal; const AWaitSyncBuffer: Pointer);
var
  p: pSyncData;
begin
  New( p );
  p^.FTimeStamp := ATimeStamp;
  p^.FSerialNum := ASerialNum;
  p^.FBufferLen := ABufferLen;
  p^.FBuffer := AWaitSyncBuffer;
  p^.FReplyOK := rsWaitting;
  EnterCriticalSection( FSyncLock );
  try
    FSyncDataList.Add( p );
  finally
    LeaveCriticalSection( FSyncLock );
  end;
end;

constructor TSyncDataList.Create;
begin
  FSyncDataList := TList.Create;
  InitializeCriticalSection( FSyncLock );
end;

destructor TSyncDataList.Destroy;
var
  i: Integer;
begin
  EnterCriticalSection( FSyncLock );
  try
    for i := FSyncDataList.Count - 1 downto 0 do
      Dispose( FSyncDataList[i] );
  finally
    LeaveCriticalSection( FSyncLock );
  end;
  FSyncDataList.Free;
  DeleteCriticalSection( FSyncLock );
  inherited;
end;

//-1: rsWaitting; -2: rsLengthTooSmall; -3无找到信息; > 0: 成功
function TSyncDataList.IsSyncDataOK(const ATimeStamp: Cardinal; const ASerialNum: Word; const AForceDelete: Boolean): Integer;
var
  nIndex: Integer;
  p: pSyncData;
begin
  Result := -3;
  EnterCriticalSection( FSyncLock );
  try
    nIndex := FindSyncData( ATimeStamp, ASerialNum );
    if nIndex = -1 then Exit;
    p := FSyncDataList[nIndex];
    if p^.FReplyOK = rsSuccess then
    begin
      Result := p^.FBufferLen;
      FSyncDataList.Delete( nIndex );
      Dispose(p);
    end
    else if p^.FReplyOK = rsWaitting then
      Result := -1
    else if p^.FReplyOK = rsLengthTooSmall then
      Result := -2;

    if (p^.FReplyOK = rsLengthTooSmall) or AForceDelete then
    begin
      FSyncDataList.Delete( nIndex );
      Dispose(p);
    end;
  finally
    LeaveCriticalSection( FSyncLock );
  end;
end;


function TSyncDataList.ModifySyncData(const ATimeStamp: Cardinal; const ASerialNum: Word; const ABufferLen: Cardinal; const ABuffer: Pointer): Boolean;
var
  nIndex: Integer;
  p: pSyncData;
begin
  EnterCriticalSection( FSyncLock );
  try
    nIndex := FindSyncData( ATimeStamp, ASerialNum );
    Result := nIndex <> -1;
    if not Result then Exit;
    p := FSyncDataList[nIndex];
    if p^.FBufferLen < ABufferLen then
    begin
      p^.FReplyOK := rsLengthTooSmall;
      Exit;
    end;
    p^.FBufferLen := ABufferLen;
    Move( ABuffer^, p^.FBuffer^, ABufferLen );
    p^.FReplyOK := rsSuccess;
  finally
    LeaveCriticalSection( FSyncLock );
  end;
end;

function TSyncDataList.FindSyncData(const ATimeStamp: Cardinal; const ASerialNum: Word): Integer;
var
  i: Integer;
  p: pSyncData;
begin
  Result := -1;
  for i := 0 to FSyncDataList.Count - 1 do
  begin
    p := FSyncDataList[i];
    if (p^.FSerialNum = ASerialNum) and (p^.FTimeStamp = ATimeStamp) then
    begin
      Result := i;
      Break;
    end;
  end;
end;
{ TUdpClient }

procedure TUdpClient.ConnectToProxy;
var
  saProxy: TSockAddrIn;
  ret: Integer;
  bRet: Boolean;
begin
  FreeTcpSocket;
  if FTcpSocket = INVALID_SOCKET then
    FTcpSocket := socket(AF_INET, SOCK_STREAM, 0);

  saProxy.sin_family := AF_INET;
  saProxy.sin_port := htons(FProxyInfo.Port);
  saProxy.sin_addr.S_addr := inet_addr(PChar(FProxyInfo.Address));

  ret := connect(FTcpSocket, @saProxy, SizeOf(saProxy));
  if ret = SOCKET_ERROR then
  begin
    closesocket(FTcpSocket);
    FTcpSocket := INVALID_SOCKET;
    FConnected := 2;
    Exit;
  end;

  if FProxyInfo.Username <> '' then
    bRet := Handclasp(FTcpSocket, atUserPass)
  else
    bRet := Handclasp(FTcpSocket, atNone);

  if not bRet then
  begin
    closesocket(FTcpSocket);
    FTcpSocket := INVALID_SOCKET;
    FConnected := 2;
    Exit;
  end;

  if not MapUdpChannel(FTcpSocket) then
  begin
    closesocket(FTcpSocket);
    FTcpSocket := INVALID_SOCKET;
    FConnected := 2;
    Exit;
  end;
  FConnected := 1;
end;

constructor TUdpClient.Create;
begin
  inherited;
  FProxyEnabled := False;
//  ProxyInfo.Address := '';
//  ProxyInfo.Port := 0;
//  ProxyInfo.Username := '';
//  ProxyInfo.Password := '';
  
  IsBind := True;
  IsExclusitve := True;

  SinglePackageThreadCount := 1;
  SinglePackageMaxNodeCount := 64;
  SinglePackageQuerySpaceTime := 100;

  MulitPackageEnable := True;
  MulitPackageHashTableCount := 521;
  MulitPackageMaxHashNodeCount := 64;
  MulitPackageThreadCount := 0;
  MulitPackageQuerySpaceTime :=100;
  MulitPackageCheckThread := True;
  MulitPackageCheckSpaceTime := 60 * 1000;
  MulitPackageMaxWaitTime := 5000;
end;

destructor TUdpClient.Destroy;
begin

  inherited;
end;

procedure TUdpClient.DoAfterCloseUDP;
begin
  inherited DoAfterCloseUDP;
  FSyncDataList.Free;
  ZeroMemory( @FUDPProxyAddr, Sizeof(TSockAddrIn) );
end;

function TUdpClient.DoBeforOpenUDP: Boolean;
begin
  Result := inherited DoBeforOpenUDP;
  if Result then
    FSyncDataList := TSyncDataList.Create;
end;

procedure TUdpClient.DoHandleAnsyBuffer(ARecvInfo: TRecvInfo);
begin
  if Assigned(OnHandleAsynBuffer) then
    OnHandleAsynBuffer( Self, ARecvInfo );
end;

procedure TUdpClient.FreeTcpSocket;
begin
  if FTcpSocket <> INVALID_SOCKET then
  begin
    shutdown(FTcpSocket, SD_BOTH);
    closesocket(FTcpSocket);
    FTcpSocket := INVALID_SOCKET;
    ZeroMemory(@FUDPProxyAddr, Sizeof(TSockAddrIn));
  end;
end;

function TUdpClient.Handclasp(Socket: TSocket; AuthenType: TAuthenType): Boolean;
var
  Buf: array[0..255] of Byte;
  I, Ret: Integer;
  Username, Password: string;
begin
  Result := False;
  case AuthenType of
    atNone:
      begin
        Buf[0] := $05;
        Buf[1] := $01;
        Buf[2] := $00;
        Ret := send(Socket, Buf, 3, 0);
        if Ret = -1 then Exit;
        FillChar(Buf, 256, #0);
        Ret := recv(Socket, Buf, 256, 0);
        if Ret < 2 then Exit;
        if Buf[1] <> $00 then Exit;
        Result := True;
      end;
    atUserPass:
      begin
        Buf[0] := $05;
        Buf[1] := $02;
        Buf[2] := $00;
        Buf[3] := $02;
        Ret := send(Socket, Buf, 4, 0);
        if Ret = -1 then Exit;
        FillChar(Buf, 256, #0);
        Ret := recv(Socket, Buf, 256, 0);
        if Ret < 2 then Exit;
        if Buf[1] <> $02 then Exit;
        Username := FProxyInfo.Username;
        Password := FProxyInfo.Password;
        FillChar(Buf, 256, #0);
        Buf[0] := $01;
        Buf[1] := Length(Username);
        for I := 0 to Buf[1] - 1 do
          Buf[2 + I] := Ord(Username[I + 1]);
        Buf[2 + Length(Username)] := Length(Password);
        for I := 0 to Buf[2 + Length(Username)] - 1 do
          Buf[3 + Length(Username) + I] := Ord(Password[I + 1]);
        Ret := send(Socket, Buf, Length(Username) + Length(Password) + 3, 0);
        if Ret = -1 then Exit;
        Ret := recv(Socket, Buf, 256, 0);
        if Ret = -1 then Exit;
        if Buf[1] <> $00 then Exit;
        Result := True;
      end;
  end;
end;

function TUdpClient.MapUdpChannel(Socket: TSocket): Boolean;
var
  saLocal: TSockAddrIn;
  NameLen: Integer;
  ProxyAddr: TInAddr;
  ProxyPort: Word;
  Buf: array[0..255] of Byte;
begin
  Result := False;
  NameLen := SizeOf(saLocal);
  getsockname(FSocket, saLocal, NameLen);
  Buf[0] := $05;
  Buf[1] := $03; //UDP
  Buf[2] := $00;
  Buf[3] := $01; //IPv4

  PInteger(@Buf[4])^ := saLocal.sin_addr.S_addr;
  PWord(@Buf[8])^ := saLocal.sin_port;

  send(Socket, Buf, 10, 0);
  FillChar(Buf, 256, #0);
  recv(Socket, Buf, 256, 0);
  if (Buf[0] <> $05) and (Buf[1] <> $00) then
    Exit;

  ProxyAddr.S_addr := PInteger(@Buf[4])^;
  ProxyPort := PWORD(@Buf[8])^;

  ZeroMemory(@FUDPProxyAddr, Sizeof(TSockAddrIn));
  FUdpProxyAddr.sin_family := AF_INET;
  FUdpProxyAddr.sin_port := ProxyPort;
  FUdpProxyAddr.sin_addr := ProxyAddr;

  if (FUdpProxyAddr.sin_addr.S_addr = 0) or (FUdpProxyAddr.sin_port = 0) then Exit; //判断是否真的是代理地址
  Result := True;

end;

procedure TUdpClient.OnRecvBuffer(ARecvInfo: TRecvInfo);
begin
  if not Boolean(ARecvInfo.FIsAsynch) then
  begin //同步包
    if FSyncDataList.ModifySyncData( ARecvInfo.FTimeStamp, ARecvInfo.FSerialNum, ARecvInfo.FBufferLength, ARecvInfo.FBuffer ) then
      Exit;
  end;
  DoHandleAnsyBuffer( ARecvInfo );
end;

function TUdpClient.SendBufferBySync(AIP: Cardinal; AHostShortPort: word; ASendBuf, AReplyBuf: Pointer; const ASendBufLen: Integer;
  var AReplyBufLen: Integer; ASerialNum: Word; AWaitTime: Integer): Boolean;
var
  nSleepTime: Integer;
  nTimeStamp: Cardinal;
begin
  nTimeStamp := GetTimeStamp + Cardinal( Random(MaxInt) );
  if ASerialNum = 0 then
    ASerialNum := GetSerialNum;
  FSyncDataList.AddSyncData( nTimeStamp, ASerialNum, AReplyBufLen, Pointer(AReplyBuf) );
  Result := inherited SendBufferBySync( AIP, AHostShortPort, ASendBuf, ASendBufLen, ASerialNum, nTimeStamp ) = ASendBufLen;
  if not Result then
  begin
    FSyncDataList.IsSyncDataOK( nTimeStamp, ASerialNum, True );
    Exit;
  end;
  nSleepTime := AWaitTime div 10 + 1;
  repeat
    AReplyBufLen := FSyncDataList.IsSyncDataOK( nTimeStamp, ASerialNum, AWaitTime < 0 ); //-1: rsWaitting; -2: rsLengthTooSmall; -3无找到信息; > 0: 成功

    if AReplyBufLen > 0 then Break;
    if AReplyBufLen = -2 then
    begin
      DoErrorInfo( '同步命令提供接收内存长度过长' );
      Break;
    end
    else if AReplyBufLen = -3 then
    begin
      DoErrorInfo( '同步命令列表节点丢失' );
      Break;
    end;

    Sleep( nSleepTime );
    Dec( AWaitTime, nSleepTime );
  until ( (AReplyBufLen >= 0) or (AWaitTime <= 0) );
  
  Result := AReplyBufLen >= 0;
end;

function TUdpClient.SendByProxy(const Socket: TSocket; var buf; len: Integer; saRemote: TSockAddr): Integer;
var
  TempBuf: array[0..CtUdpPackageSize - 1] of Byte;
begin
  TempBuf[0] := $00;
  TempBuf[1] := $00;
  TempBuf[2] := $00;
  TempBuf[3] := $01;
  PInteger(@TempBuf[4])^ := saRemote.sin_addr.S_addr;
  PWORD(@TempBuf[8])^ := saRemote.sin_port;
  Move(Buf, TempBuf[10], len);
  Result := inherited __SendTo(Socket, TempBuf, len + 10, 0, FUdpProxyAddr, SizeOf(FUdpProxyAddr));
end;

procedure TUdpClient.SetProxyActive(const Value: Boolean);
begin
  if (FProxyEnabled <> Value) then
  begin
    FProxyEnabled := Value;
    if FProxyEnabled then
      TestConnectToProxy
    else
      FreeTcpSocket;
  end;
end;

function TUdpClient.TestConnectToProxy: Boolean;
var
  stTime: Cardinal;
begin
  FConnected := 0;
  FProxyThread := TProxyConnectThread.Create(self);
  FProxyThread.Resume;
  stTime := GetTickCount;
  while (FConnected = 0) and (GetTickCount - stTime < 5000) do
  begin
    Application.ProcessMessages;
    Sleep(1);
  end;
  Result := FConnected = 1;
  if FConnected = 0 then
  begin
    FreeTcpSocket;
    TerminateThread(FProxyThread.Handle, 0);
  end;
  FProxyThread.Free;
end;

function TUdpClient.__SendTo(s: TSocket; var Buf; len, flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer;
begin
  if ProxyEnabled then
    Result := SendByProxy( s, Buf, len, addrto )
  else
    Result := inherited __SendTo( s, Buf, len, flags, addrto, tolen );
end;

{ TProxyConnectThread }

constructor TProxyConnectThread.Create(AOwner: TUdpClient);
begin
  FOwner := AOwner;
  inherited Create(True);
end;

destructor TProxyConnectThread.Destroy;
begin

  inherited;
end;

procedure TProxyConnectThread.Execute;
begin
  inherited;
  FOwner.ConnectToProxy;
end;

Initialization
  Randomize;

end.
