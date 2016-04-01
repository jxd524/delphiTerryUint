unit uJxdUdpOnlineServer;

interface
uses
  Windows, Classes, SysUtils, WinSock2, uJxdDataStream, uJxdThread,
  uJxdUdpBasic, uJxdUdpsynchroBasic, uJxdOnlineUserManage, uJxdServerManage, uJxdCmdDefine;
  
type
  {$M+}
  TCmdStatBasic = class
  public
    constructor Create; virtual;
    destructor  Destroy; override;
  private
    FRegisterCmd: Integer;
    FHeartbeatRecvCmd: Integer;
    FLogoutCmd: Integer;
    FLoginCmd: Integer;
    FHeartbeatSendCmd: Integer;
  published
    property RegisterCmd: Integer read FRegisterCmd;
    property LoginCmd: Integer read FLoginCmd;
    property LogoutCmd: Integer read FLogoutCmd;
    property HeartbeatRecvCmd: Integer read FHeartbeatRecvCmd; //接收到的心跳包数量
    property HeartbeatSendCmd: Integer read FHeartbeatSendCmd; //发送的心跳包数量
  end;
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ///                         TxdUdpCommonServer
  ///
  ///  线程安全
  ///  P2SP基本服务器
  ///  实现命令：注册, 登录，退出，心跳，P2P打洞
  ///  提供在线用户管理功能，提供命令统计
  ///
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TxdUdpOnlineServer = class(TxdUdpSynchroBasic)
  public
    constructor Create; override;
    destructor  Destroy; override;
    function SetBeginRegisterUserID(const AMinID: Cardinal): Boolean;
  protected
    FOnlineUserManage: TOnlineUserManage;
    FCmdStat: TCmdStatBasic;
    {子类实现功能}
    procedure DoHandleCmd(const AIP: Cardinal; const APort: Word; const ApCmdHead: PCmdHead;
      const ABuffer: pAnsiChar; const ABufLen: Cardinal; const AIsSynchroCmd: Boolean; const ASynchroID: Word); virtual;
    function  CreateCmdStatObject: TCmdStatBasic; virtual; //
    function  CheckClientVersion(const AVersion: Word): Boolean; virtual;
    {提供功能}
    procedure SendStream(const AIP: Cardinal; APort: Word; AStream: TxdMemoryHandle);

    function  DoBeforOpenUDP: Boolean; override;
    procedure DoAfterCloseUDP; override;

    procedure OnCommonRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); override;
  private
    FRegisterManage: TRegisterManage;
    FServerManage: TxdServerManage;
    {客户端命令处理}
    procedure DoHandleCmd_Register(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word);  //注册
    procedure DoHandleCmd_Login(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word);  //登录
    procedure DoHandleCmd_Logout(const ABuffer: pAnsiChar; const ABufLen: Cardinal);    //退出
    procedure DohandleCmd_Heartbeat(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); //心跳包
    procedure DoHandleCmd_GetRandomUsers(const AIP, AUserID: Cardinal; const APort: Word; const AIsSynchroCmd: Boolean;
      const ASynchroID: Word); //随机用户
    procedure DoHandleCmd_CallMe(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); //CallMe
    procedure DoHandleCmd_GetServerAddr(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); //GetServerAddr
    {与其它服务器的沟通}
    procedure DoHandleCmd_ServerOnline(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); //服务器在线通知
    procedure DoHandleCmdReply_HelloServer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal); //服务器响应Hello
  private
    FCheckServerThread: TThreadCheck;

    procedure DoThreadToCheckServerManage;
    procedure CheckCurMaxOnlineCount;
  private
    FMinUserID: Cardinal;
    FMaxOnlineCount: Integer;
    FProtocolErrorCount: Integer;
    FInvalidUserIDCount: Integer;
    FCurMaxOnlineCount: Integer;
    FCurMaxOnlineTime: Cardinal;
    procedure SetMaxOnlineCount(const Value: Integer);
    procedure SetServerID(const Value: Cardinal);
    function  GetCurOnlineCount: Integer;
    function  GetTimeoutUserCount: Integer;
    function  GetCurRegUserID: Cardinal;
    procedure SetCurMaxOnlineCount(const Value: Integer);
    procedure SetCurMaxonlineTime(const Value: Cardinal);
    function  GetSelfID: Cardinal; inline;
  published
    property CmdStat: TCmdStatBasic read FCmdStat;
    property CurRegUserID: Cardinal read GetCurRegUserID;
    property ServerID: Cardinal read GetSelfID write SetServerID; //服务器使用ID
    property ProtocolErrorCount: Integer read FProtocolErrorCount; //协议出错数量
    property MaxOnlineCount: Integer read FMaxOnlineCount write SetMaxOnlineCount; //最大允许在线人数

    property ServerManage: TxdServerManage read FServerManage;

    {统计}
    property CurMaxOnlineCount: Integer read FCurMaxOnlineCount write SetCurMaxOnlineCount; //最大在线人数
    property CurMaxOnlineTime: Cardinal read FCurMaxOnlineTime write SetCurMaxonlineTime; //最大在线人数时间
    property InvalidUserIDCount: Integer read FInvalidUserIDCount; //接收到命令中用户ID无效次数
    property CurOnlineCount: Integer read GetCurOnlineCount; //当前在线人数
    property TimeoutUserCount: Integer read GetTimeoutUserCount; //因超时服务器自动删除次数
  end;
  {$M-}

implementation

uses
  uConversion;

const
  CtServerManageFileName = 'ServerInfo.dat';

{ TxdUdpCommonServer }

function TxdUdpOnlineServer.CheckClientVersion(const AVersion: Word): Boolean;
begin
  Result := True;
end;

procedure TxdUdpOnlineServer.CheckCurMaxOnlineCount;
begin
  if FCurMaxOnlineCount < FOnlineUserManage.Count then
  begin
    InterlockedExchange( FCurMaxOnlineCount, FOnlineUserManage.Count );
    FCurMaxOnlineTime := GetTimeStamp;
  end;
end;

constructor TxdUdpOnlineServer.Create;
begin
  inherited;
  FMaxOnlineCount := 500000;
  FCurMaxOnlineCount := 0;
  FCurMaxOnlineTime := 0;
  FMinUserID := CtMinUserID;
  FSelfID := CtOnlineServerID;
  SynchroCmd := CtCmdSynchroPackage;
  FServerManage := TxdServerManage.Create;
  FServerManage.FileName := ExtractFilePath(ParamStr(0)) + CtServerManageFileName;
end;

function TxdUdpOnlineServer.CreateCmdStatObject: TCmdStatBasic;
begin
  Result := TCmdStatBasic.Create;
end;

destructor TxdUdpOnlineServer.Destroy;
begin
  FreeAndNil( FServerManage );
  inherited;
end;

procedure TxdUdpOnlineServer.DoAfterCloseUDP;
begin
  inherited;
  FreeAndNil( FOnlineUserManage );
  FreeAndNil( FRegisterManage );
  FreeAndNil( FCmdStat );
end;

function TxdUdpOnlineServer.DoBeforOpenUDP: Boolean;
begin
  Result := inherited DoBeforOpenUDP;
  if Result then
  begin
    try
      FProtocolErrorCount := 0;
      FInvalidUserIDCount := 0;
      FOnlineUserManage := TOnlineUserManage.Create;
      with FOnlineUserManage do
      begin
         MaxOnlineCount := FMaxOnlineCount;
         Active := True;
      end;
      FRegisterManage := TRegisterManage.Create( FMinUserID );
      FCmdStat := CreateCmdStatObject;
      FCheckServerThread := TThreadCheck.Create( DoThreadToCheckServerManage, 1000 * 10 );
    except
      Result := False;
    end;
  end;
end;

procedure TxdUdpOnlineServer.DoHandleCmd(const AIP: Cardinal; const APort: Word; const ApCmdHead: PCmdHead; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
begin
  DoErrorInfo( '接收到未知的命令' );
end;

procedure TxdUdpOnlineServer.DoHandleCmdReply_HelloServer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
var
  pCmd: PCmdS2SReplyHelloServerInfo;
  info: TServerManageInfo;
begin
  if ABufLen <> CtCmdS2SReplyHelloServerInfoSize then
  begin
    DoErrorInfo( 'S2SReplyHelloServerInfo 命令长度不正确' );
    Exit;
  end;
  pCmd := PCmdS2SReplyHelloServerInfo(ABuffer);
  info.FServerStyle := pCmd^.FServerStyle;
  info.FServerID := pCmd^.FCmdHead.FUserID;
  info.FServerIP := AIP;
  info.FServerPort := APort;
  FServerManage.AddServerInfo( @info, GetTickCount );
end;

procedure TxdUdpOnlineServer.DoHandleCmd_CallMe(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  oSendStream: TxdStaticMemory_64Byte;
  pCmd: PCmdCallMeInfo;
  UserA, UserB: TUserNetInfo;
  opt: TOptResult;
  Replysign: TReplySign;
begin
  if ABufLen <> CtCmdCallMeInfoSize then
  begin
    DoErrorInfo( '接收到错误的CallMe命令' );
    Exit;
  end;
  //接收到A的命令，向A回应 CtCmdReply_CallMe，内容包含B的网络信息
  //向B发送 CtCmd_CallFriend, 内容包含A的网络信息
  pCmd := PCmdCallMeInfo(ABuffer);
  UserA.FUserID := pCmd^.FCmdHead.FUserID;
  UserB.FUserID := pCmd^.FCallUserID;
  opt := FOnlineUserManage.GetOnlineUserInfo( userA, UserB );
  case opt of
    orSuccess: Replysign := rsSuccess;
    orNotFindUser: Replysign := rsNotExistsID;
    else
      Replysign := rsError;
  end;
  if Replysign = rsError then Exit; //命令发送者无效,可以回复让用户重新登录的命令

  oSendStream := TxdStaticMemory_64Byte.Create;
  try
    //回复A
    if AIsSynchroCmd then
      AddSynchroSign( oSendStream, ASynchroID );
    AddCmdHead( oSendStream, CtCmdReply_CallMe );
    oSendStream.WriteByte( Byte(Replysign) );
    oSendStream.WriteLong( UserB, CtUserNetInfoSize );
    SendStream( AIP, APort, oSendStream ); //回复

    //要求B
    oSendStream.Clear;
    AddCmdHead( oSendStream, CtCmd_CallFriend );
    oSendStream.WriteLong( UserA, CtUserNetInfoSize );
    SendStream( UserB.FPublicIP, UserB.FPublicPort, oSendStream );
  finally
    oSendStream.Free;
  end;
end;

procedure TxdUdpOnlineServer.DoHandleCmd_GetRandomUsers(const AIP, AUserID: Cardinal; const APort: Word; const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  oSendStream: TxdStaticMemory_1K;
  nSignPos, nCountPos, nLen: Integer;
  nReplyCount: Byte;
begin
  oSendStream := TxdStaticMemory_1K.Create;
  try
    if AIsSynchroCmd then
      AddSynchroSign( oSendStream, ASynchroID );
    AddCmdHead( oSendStream, CtCmdReply_GetRandomUsers );
    nSignPos := oSendStream.Position;
    oSendStream.Position := oSendStream.Position + 1;

    oSendStream.WriteCardinal( FOnlineUserManage.Count );

    nCountPos := oSendStream.Position;
    oSendStream.Position := oSendStream.Position + 1;

    nReplyCount := FOnlineUserManage.GetRandomUserInfo( AUserID, oSendStream );
    nLen := oSendStream.Position;

    oSendStream.Position := nSignPos;
    if nReplyCount > 0 then
      oSendStream.WriteByte( Byte(rsSuccess) )
    else
      oSendStream.WriteByte( Byte(rsError) );

    oSendStream.Position := nCountPos;
    oSendStream.WriteByte( nReplyCount );
    SendBuffer( AIP, APort, oSendStream.Memory, nLen );
  finally
    oSendStream.Free;
  end;
end;

procedure TxdUdpOnlineServer.DoHandleCmd_GetServerAddr(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  oSendStream: TxdStaticMemory_4K;
  n, n1: Integer;
begin
  if ABufLen <> CtMinPackageLen then
  begin
    DoErrorInfo( 'DoHandleCmd_GetServerAddr 长度不正确' );
    Exit;
  end;
  oSendStream := TxdStaticMemory_4K.Create;
  try
    if AIsSynchroCmd then
      AddSynchroSign( oSendStream, ASynchroID );
    AddCmdHead( oSendStream, CtCmdReply_GetServerAddr );
    n := oSendStream.Position;
    oSendStream.WriteByte( Byte(rsSuccess) );
    FServerManage.CopyToStream( oSendStream );
    n1 := oSendStream.Position;
    if n = n1 then
      oSendStream.WriteByte( Byte(rsNotFind) );
    SendStream( AIP, APort, oSendStream );
  finally
    oSendStream.Free;
  end;
end;

procedure TxdUdpOnlineServer.DohandleCmd_Heartbeat(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdHeartbeatInfo;
  buf: array[0..CtMaxReplyHeartbeatPackageSize - 1] of Byte;
  Stream: TxdOuterMemory;
begin
  if ABufLen <> CtCmdHeartbeatInfoSize then
  begin
    InterlockedIncrement( FProtocolErrorCount );
    DoErrorInfo( '接收到非法的心跳包' );
    Exit;
  end;
  pCmd := PCmdHeartBeatInfo( ABuffer );
  if not FOnlineUserManage.ActiveOnlineUser(pCmd^.FCmdHead.FUserID) then
  begin
    InterlockedIncrement( FInvalidUserIDCount );
    AddCmdHead( PAnsiChar(pCmd), CtCmd_ClientRelogin );
    SendBuffer( AIP, APort, PAnsiChar(pCmd), CtCmdClientReLoginToServerInfoSize );
    Exit;
  end;
  
  InterlockedIncrement( FCmdStat.FHeartbeatRecvCmd );
  if pCmd^.FNeedReply then
  begin
    Stream := TxdOuterMemory.Create;
    try
      Stream.InitMemory( @buf, CtMaxReplyHeartbeatPackageSize );
      if AIsSynchroCmd then
        AddSynchroSign( Stream, ASynchroID );
      AddCmdHead( Stream, CtCmdReply_Heartbeat );
      Stream.WriteByte( Byte(False) );
      SendStream( AIP, APort, Stream );
      InterlockedIncrement( FCmdStat.FHeartbeatSendCmd );
    finally
      Stream.Free;
    end;
  end;
end;

procedure TxdUdpOnlineServer.DoHandleCmd_Login(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdLoginInfo;
  ReplySign: TReplySign;
  buf: array[0..CtMaxReplyLoginPackageSize - 1] of Byte;
  Stream: TxdOuterMemory;
  addResult: TAddUserResult;
  nSaftSign: Cardinal;
begin
  if ABufLen <> CtCmdLoginInfoSize then
  begin
    InterlockedIncrement( FProtocolErrorCount );
    DoErrorInfo( '接收到非法的登录命令' );
    Exit;
  end;
  InterlockedIncrement( FCmdStat.FLoginCmd );
  pCmd := PCmdLoginInfo( ABuffer );


  if pCmd^.FCmdHead.FUserID <= CtMinUserID then
    ReplySign := rsMustRegNewID
  else if not CheckClientVersion(pCmd^.FClientVersion) then
    ReplySign := rsOverdueVersion
  else
    ReplySign := rsSuccess;
    
  if ReplySign = rsSuccess then
  begin
    addResult := FOnlineUserManage.AddOnlineUser(pCmd^.FCmdHead.FUserID, AIP, pCmd^.FLocalIP, APort, pCmd^.FLocalPort,
      pCmd^.FClientVersion, pCmd^.FClientHash, nSaftSign);
    if addResult <> auSuccess then
    begin
      ReplySign := rsError;
      if addResult = auIDExists then
        DoErrorInfo( '无法添加到在线管理列表中：已经存在的ID'  )
      else
        DoErrorInfo( '无法添加到在线管理列表中：内存不足' );
    end
    else
      CheckCurMaxOnlineCount;
  end;

  Stream := TxdOuterMemory.Create;
  try
    Stream.InitMemory( @buf, CtMaxReplyLoginPackageSize );
    if AIsSynchroCmd then
      AddSynchroSign( Stream, ASynchroID );
    AddCmdHead( Stream, CtCmdReply_Login );
    Stream.WriteByte( Byte(ReplySign) );
    Stream.WriteCardinal( AIP );
    Stream.WriteWord( APort );
    Stream.WriteCardinal( nSaftSign );
    SendStream( AIP, APort, Stream );
  finally                                       
    Stream.Free;
  end;
end;

procedure TxdUdpOnlineServer.DoHandleCmd_Logout(const ABuffer: pAnsiChar; const ABufLen: Cardinal);
var
  pCmd: PCmdLogoutInfo;
  delResult: TDeleteUserResult;
begin
  if ABufLen <> CtCmdLogoutInfoSize then
  begin
    InterlockedIncrement( FProtocolErrorCount );
    DoErrorInfo( '接收到非法的退出命令' );
    Exit;
  end;
  pCmd := PCmdLogoutInfo(ABuffer);
  delResult := FOnlineUserManage.DeleteOnlineUser( pCmd^.FCmdHead.FUserID, pCmd^.FSafeSign );
  if delResult <> duSuccess then
  begin
    if delResult = duNotFindUser then
      DoErrorInfo( '找不到要删除的用户' )
    else
      DoErrorInfo( '提供的安全码不正确，无法删除用户' );
  end
  else
    InterlockedIncrement( FCmdStat.FLogoutCmd );
end;

procedure TxdUdpOnlineServer.DoHandleCmd_Register(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  buf: array[0..CtMaxReplyRegisterPackageSize - 1] of Byte;
  Stream: TxdOuterMemory;
  nRegID: Cardinal;
begin
  if ABufLen <> CtCmdRegisterInfoSize then
  begin
    InterlockedIncrement( FProtocolErrorCount );
    DoErrorInfo( '接收到非法的注册命令' );
    Exit;
  end;
  InterlockedIncrement( FCmdStat.FRegisterCmd );
  nRegID := FRegisterManage.GetNewUserID;

  Stream := TxdOuterMemory.Create;
  try
    Stream.InitMemory( @Buf, CtMaxReplyRegisterPackageSize );
    if AIsSynchroCmd then
      AddSynchroSign( Stream, ASynchroID );
    AddCmdHead( Stream, CtCmdReply_Register );
    Stream.WriteByte( Byte(rsSuccess) );
    Stream.WriteCardinal( nRegID );
    SendStream( AIP, APort, Stream );
  finally
    Stream.Free;
  end;
end;

procedure TxdUdpOnlineServer.DoHandleCmd_ServerOnline(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdS2SServerOnlineInfo;
  info: TServerManageInfo;
begin
  if ABufLen <> CtCmdS2SServerOnlineInfoSize then
  begin
    DoErrorInfo( 'TCmdS2SServerOnlineInfo 命令长度不正确' );
    Exit;
  end;
  pCmd := PCmdS2SServerOnlineInfo(ABuffer);
  info.FServerStyle := pCmd^.FServerStyle;
  info.FServerID := pCmd^.FCmdHead.FUserID;
  info.FServerIP := AIP;
  info.FServerPort := APort;
  FServerManage.AddServerInfo( @info, GetTickCount );
end;

procedure TxdUdpOnlineServer.DoThreadToCheckServerManage;
const
  CtCheckSpaceTime = 1000 * 60 * 15; //每15分钟查一次服务器
//  CtCheckSpaceTime = 1000 * 10;
var
  i: Integer;
  lt: TList;
  p: PServerManageInfo;
  cmd: TCmdS2SHelloServerInfo;
  nCurTime: Cardinal;
begin
  lt := FServerManage.LockManage;
  try
    cmd.FCmdHead.FCmdID := CtCmdS2S_HelloServer;
    cmd.FCmdHead.FUserID := ServerID;
    nCurTime := GetTickCount;

    for i := lt.Count - 1 downto 0 do
    begin
      p := lt[i];
      if (p^.FTag = 0) or ( (nCurTime - p^.FTag > CtCheckSpaceTime) and (nCurTime - p^.FTag <= CtCheckSpaceTime * 2))  then
      begin
        SendBuffer( p^.FServerIP, p^.FServerPort, PAnsiChar(@Cmd), CtCmdS2SHelloServerInfoSize );
        if p^.FTag = 0 then
          p^.FTag := GetTickCount;
      end
      else if nCurTime - p^.FTag > CtCheckSpaceTime * 2 then
      begin
        lt.Delete( i );
        Dispose( p );
      end;
    end;
  finally
    FServerManage.UnlockManage;
  end;
  if FCheckServerThread.SpaceTime <> CtCheckSpaceTime then
    FCheckServerThread.SpaceTime := CtCheckSpaceTime;
end;

function TxdUdpOnlineServer.GetCurOnlineCount: Integer;
begin
  if Active then
    Result := FOnlineUserManage.Count
  else
    Result := 0;
end;

function TxdUdpOnlineServer.GetCurRegUserID: Cardinal;
begin
  if Assigned(FRegisterManage) then
    Result := FRegisterManage.UserID
  else
    Result := CtMinUserID;
end;

function TxdUdpOnlineServer.GetSelfID: Cardinal;
begin
  Result := FSelfID;
end;

function TxdUdpOnlineServer.GetTimeoutUserCount: Integer;
begin
  if Active then
    Result := FOnlineUserManage.TimeoutUserCount
  else
    Result := 0;
end;

procedure TxdUdpOnlineServer.OnCommonRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pHead: PCmdHead;
begin
  if ABufLen < CtMinPackageLen then
  begin
    DoErrorInfo( '接收到的包长度过小，丢弃此包' );
    Exit;
  end;
  pHead := PCmdHead( ABuffer );
  case pHead^.FCmdID of
    CtCmd_Register: DoHandleCmd_Register( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_Login: DoHandleCmd_Login( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_Logout: DoHandleCmd_Logout( ABuffer, ABufLen );
    CtCmd_Heartbeat: DohandleCmd_Heartbeat( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_GetRandomUsers: DoHandleCmd_GetRandomUsers(AIP, pHead^.FUserID, APort, AIsSynchroCmd, ASynchroID );
    CtCmd_CallMe: DoHandleCmd_CallMe( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_GetServerAddr: DoHandleCmd_GetServerAddr( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmdS2S_ServerOnline: DoHandleCmd_ServerOnline( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmdS2SReply_HelloServer: DoHandleCmdReply_HelloServer( AIP, APort, ABuffer, ABufLen );
    else
      DoHandleCmd( AIP, APort, pHead, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
  end;
end;

procedure TxdUdpOnlineServer.SendStream(const AIP: Cardinal; APort: Word; AStream: TxdMemoryHandle);
begin
  SendBuffer( AIP, APort, AStream.Memory, AStream.Position );
end;

function TxdUdpOnlineServer.SetBeginRegisterUserID(const AMinID: Cardinal): Boolean;
begin
  Result := not Active and (AMinID >= CtMinUserID) and (AMinID <> FMinUserID);
  if Result then
    FMinUserID := AMinID;
end;

procedure TxdUdpOnlineServer.SetCurMaxOnlineCount(const Value: Integer);
begin
  FCurMaxOnlineCount := Value;
end;

procedure TxdUdpOnlineServer.SetCurMaxonlineTime(const Value: Cardinal);
begin
  FCurMaxOnlineTime := Value;
end;

procedure TxdUdpOnlineServer.SetMaxOnlineCount(const Value: Integer);
begin
  if not Active then
    FMaxOnlineCount := Value;
end;

procedure TxdUdpOnlineServer.SetServerID(const Value: Cardinal);
begin
  if not Active then
    FSelfID := Value;
end;

{ TCmdStatBasic }

constructor TCmdStatBasic.Create;
begin
  FRegisterCmd := 0;
  FLoginCmd := 0;
  FLogoutCmd := 0;
  FHeartbeatRecvCmd := 0;
end;

destructor TCmdStatBasic.Destroy;
begin

  inherited;
end;

end.
