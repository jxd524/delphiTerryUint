unit uSocketSub;

interface
uses Windows, SysUtils, WinSock2, RTLConsts;

function  SetSocketExclusitveAddr(const ASocket: TSocket): Boolean; //设置套接字独占地址
function  InitSocketAddr(const AIP: Cardinal; const AHostPort: Word): TSockAddr; //初始化SockAdd, 标示: AF_INET;
function  GetSocketRecvBufSize(const ASocket: TSocket): Integer; //套接字接受使用内存大小
function  GetSocketSendBufSize(const ASocket: TSocket): Integer;
function  SetSocketRecvBufSize(const ASocket: TSocket; ARecvBufSize: Cardinal): Boolean;
function  SetSocketSendBufSize(const ASocket: TSocket; ASendBufSize: Cardinal): Boolean;
procedure GetLocalAddr(var APeerIP, ALocalIP: Cardinal; var AInNAT: Boolean);

implementation

const
  CtSockAddrLen = SizeOf(TSockAddr);
  
procedure RaiseWinSocketError(AErrCode: Integer; AAPIName: PChar);
begin
  raise Exception.Create( Format(sWindowsSocketError, [SysErrorMessage(AErrCode), AErrCode, AAPIName]) );
end;

function SetSocketExclusitveAddr(const ASocket: TSocket): Boolean;
var
  BoolValue: Boolean;
begin
  BoolValue := True;
  Result := setsockopt( ASocket, SOL_SOCKET, SO_EXCLUSIVEADDRUSE, @BoolValue, SizeOf(BoolValue) ) <> SOCKET_ERROR
end;

function InitSocketAddr(const AIP: Cardinal; const AHostPort: Word): TSockAddr;
begin
  ZeroMemory( @Result, CtSockAddrLen );
  with Result do
  begin
    sin_family := AF_INET;
    sin_port := htons( AHostPort );
    sin_addr.S_addr := AIP;
  end;
end;

function GetSocketRecvBufSize(const ASocket: TSocket): Integer;
var
  nLen: Integer;
begin
  Result := -1;
  nLen := SizeOf(Result);
  getsockopt( ASocket, SOL_SOCKET, SO_RCVBUF, @Result, nLen );
end;

function GetSocketSendBufSize(const ASocket: TSocket): Integer;
var
  nLen: Integer;
begin
  Result := -1;
  nLen := SizeOf(Result);
  getsockopt( ASocket, SOL_SOCKET, SO_SNDBUF, @Result, nLen );
end;

function SetSocketRecvBufSize(const ASocket: TSocket; ARecvBufSize: Cardinal): Boolean;
begin
  Result := setsockopt( ASocket, SOL_SOCKET, SO_RCVBUF, @ARecvBufSize, SizeOf(Cardinal) ) <> SOCKET_ERROR;
end;

function SetSocketSendBufSize(const ASocket: TSocket; ASendBufSize: Cardinal): Boolean;
begin
  Result := setsockopt( ASocket, SOL_SOCKET, SO_RCVBUF, @ASendBufSize, SizeOf(Cardinal) ) <> SOCKET_ERROR;
end;

function CheckInAddr(AIP: TInAddr): Integer;
begin
  with AIP.S_un_b do
  begin
    if (AIP.S_addr = 0) or (AIP.S_addr = $FFFFFFFF) or
      ((s_b1 = 127) and (s_b2 = 0) and (s_b3 = 0) and (s_b4 = 1)) or
      ((s_b1 = 169) and (s_b2 = 254)) then
      Result := -1
    else if (s_b1 = 10) or
      ((s_b1 = 172) and (s_b2 >= 16) and (s_b2 <= 31)) or
      ((s_b1 = 192) and (s_b2 = 168)) then
      Result := 0
    else
      Result := 1;
  end;
end;

procedure GetLocalAddr(var APeerIP, ALocalIP: Cardinal; var AInNAT: Boolean);
type
  TaPInAddr = array[0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  Buffer: array[0..63] of char;
  I: Integer;
  InAddr: TInAddr;
  CheckResult: Integer;
  LocalResult: Integer;
begin
  AInNAT := True;
  LocalResult := -1;
//  if WSAStartup($0101, wsaData) <> 0 then exit;
  try
    GetHostName(Buffer, SizeOf(Buffer));
    phe := GetHostByName(buffer);
    if phe = nil then Exit;
    pptr := PaPInAddr(Phe^.h_addr_list);
    I := 0;
    while pptr^[I] <> nil do
    begin
      InAddr := pptr^[I]^;
      Inc(I);
      CheckResult := CheckInAddr(InAddr);
      if CheckResult = 1 then //有外网IP地址
      begin
        APeerIP := InAddr.S_addr;
        AInNAT := False;
      end
      else if CheckResult >= LocalResult then
      begin
        ALocalIP := InAddr.S_addr;
        if AInNAT then
          APeerIP := InAddr.S_addr;
      end;
      Break;
    end;
    if (I = 1) and not AInNAT then //只有一个外网IP;
      ALocalIP := APeerIP;
  finally
//    WSACleanup;
  end;
end;





////////////////////////////////////////////
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
