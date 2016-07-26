unit uJxdUdpCommonClient;

interface
uses
  Windows, SysUtils, Classes, WinSock2, uJxdDataStream,
  uJxdUdpBasic, uJxdUdpIoHandle, uJxdUdpSynchroClient, uJxdCmdDefine, uJxdP2PUserManage, uJxdFileUploadManage,
  uJxdFileShareManage, uJxdDownTaskManage, uJxdServerManage;

type
  TxdUdpCommonClient = class(TxdUdpSynchroClient)
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure GetRandomOnlineUsers;
    function  ConnectToClient(const AUserID: Cardinal): TConnectState; //请求P2P连接
    procedure SendStringToClient(const AUserID: Cardinal; const AInfo: string); //向指定用户发送P2P字符信息，无返回值
  protected
    {提供功能}
    function  SendToServer(const ABuffer: PAnsiChar; const ABufLen: Integer): Boolean; overload;
    function  SendToServer(AStream: TxdMemoryHandle): Boolean; overload;

    function  SendStream(const AIP: Cardinal; APort: Word; AStream: TxdMemoryHandle): Boolean;

    procedure DoAfterOpenUDP; override;
    procedure DoBeforCloseUDP; override;
    procedure DoAfterCloseUDP; override;

    {子类可实现}
    procedure LoginToServer; virtual;
    procedure LogoutFromServer; virtual;
    procedure DoRecvP2PStringInfo(const AUserID: Cardinal; const ARecvInfo: string); virtual;
    procedure DoHandleCmd(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); virtual;


    {先对数据长度进行判断, 最后更新接收时间}
    procedure OnCommonRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); override;
    {实现}
    procedure DoHandleRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); override;
  private
    FServerManage: TxdServerManage;
    FFileShareManage: TxdFileShareManage;
    FUploadFileManage: TxdFileUploadManage;
    FDownTaskManage: TxdDownTaskManage;
    
    FKeepServerLiveEvent: Cardinal;
    FKeepSrvThreadClose: Boolean;
    FP2PUserManage: TxdP2PUserManage;  //在登录成功之后创建，只在关闭UDP之后才会释放
    FClientHash: array[0..15] of Byte;
    {发送命令}
    function  SendCmd_Register: Cardinal; //返回注册到ID；
    procedure SendCmd_Login; //登录命令
    procedure SendCmd_Logout;//退出命令
    function  SendCmd_Heartbeat(const AIP: Cardinal; const APort: Word; const ANeedReply: Boolean): Boolean; //心跳包
    procedure SendCmd_GetServerAddr;
    {接收到的服务器命令处理}
    procedure DoHandleCmd_Heartbeat(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word);
    procedure DoHandleCmd_ReplyGetRandomUsers(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
    procedure DoHandleCmd_ReplyCallMe(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
    procedure DoHandleCmd_CallFriend(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
    procedure DoHandleCmd_ReplyGetServerAddr(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
    {接收到其它客户端信息}
    procedure DoHandleCmdP2P_Hello(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word);
    procedure DoHandleCmdP2P_ReplyHello(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word);
    procedure DoHandleCmdP2P_StringInfo(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
    {检查线程}
    procedure DoThreadCheckKeepLive; //登录之后开始保持与服务器之间的连接

    procedure OnFailToConnectedServer; //当太长时间接收不到服务器信息时调用

    {P2P列表事件}
    procedure DoUpdateClientInfo(Ap: PClientPeerInfo); //当更新用户信息
    procedure DoDeleteUser(Ap: PClientPeerInfo; const AReason: TDeleteUserReason); //当删除用户时
    procedure DoP2PConnected(Ap: PClientPeerInfo); //当P2P连接成功时
    procedure DoP2PConnectFail(Ap: PClientPeerInfo); //当P2P连接失败时
    procedure DoCheckP2PUser(const ACurTime: Cardinal; Ap: PClientPeerInfo; var ADel: Boolean); //对用户进行检查，包含连接与非连接
  private
    {网络信息}
    FLogin: Boolean;
    FSafeSign: Cardinal;
    FServerIP: Cardinal;
    FServerPort: Word;
    FLastServerReplySign: TReplySign;
    FServerID: Cardinal;
    FClientVersion: Word;
    FCheckKeepLiveTimeSpace: Cardinal;
    FMaxSendIdleTime: Cardinal;
    FMaxRecvIdleTime: Cardinal;
    FPublicIP: Cardinal;
    FLocalPort: Word;
    FLocalIP: Cardinal;
    FPublicPort: Word;
    FLastRecvActiveTime: Cardinal;
    FLastSendActiveTime: Cardinal;
    FCurOnlineCount: Cardinal;
    FP2PMaxWaitRecvTime: Cardinal;
    FP2PHeartbeatSpaceTime: Cardinal;
    FP2PCheckConnectSpaceTime: Cardinal;
    FP2PConnectingTimeout: Cardinal;
    FP2PConnectMaxTimeoutCount: Integer;
    procedure SetUserID(const Value: Cardinal);
    procedure SetLogin(const Value: Boolean);
    procedure SetServerIP(const Value: Cardinal);
    procedure SetServerPort(const Value: Word);
    procedure SetClientVersion(const Value: Word);
    procedure SetCheckKeepLiveTimeSpace(const Value: Cardinal);
    procedure SetMaxSendIdleTime(const Value: Cardinal);
    procedure SetMaxRecvIdleTime(const Value: Cardinal);
    procedure SetP2PMaxWaitRecvTime(const Value: Cardinal);
    procedure SetP2PHeartbeatSpaceTime(const Value: Cardinal);
    procedure SetP2PCheckConnectSpaceTime(const Value: Cardinal);
    procedure SetP2PConnectingTimeout(const Value: Cardinal);
    procedure SetP2PConnectMaxTimeoutCount(const Value: Integer);
    function GetSelfID: Cardinal; inline;
  published
    {服务器信息}
    property ServerID: Cardinal read FServerID;
    property ServerIP: Cardinal read FServerIP write SetServerIP;
    property ServerPort: Word read FServerPort write SetServerPort;
    property CurOnlineCount: Cardinal read FCurOnlineCount; 
    property LastServerReplySign: TReplySign read FLastServerReplySign; //服务器最后返回命令标志
    
    {网络信息}
    property UserID: Cardinal read GetSelfID write SetUserID;
    property PublicIP: Cardinal read FPublicIP;
    property PublicPort: Word read FPublicPort;
    property LocalIP: Cardinal read FLocalIP;
    property LocalPort: Word read FLocalPort;
    property ClientVersion: Word read FClientVersion write SetClientVersion;
    property LastRecvActiveTime: Cardinal read FLastRecvActiveTime;
    property LastSendActiveTime: Cardinal read FLastSendActiveTime;
    property SafeSign: Cardinal read FSafeSign; //登录时由服务器指定的安全码

    {检查在线情况设置- 与服务器端的连接}
    property CheckKeepLiveTimeSpace: Cardinal read FCheckKeepLiveTimeSpace write SetCheckKeepLiveTimeSpace; //在线检查间隔
    property MaxSendIdleTime: Cardinal read FMaxSendIdleTime write SetMaxSendIdleTime; //最大发送空闲时间，超过此设置，发送不需要返回的心跳包
    property MaxRecvIdleTime: Cardinal read FMaxRecvIdleTime write SetMaxRecvIdleTime; //最大接收空闲时间，超过此设置，发送要求返回的心跳包
    {检查P2P在线情况设置}
    property P2PCheckConnectSpaceTime: Cardinal read FP2PCheckConnectSpaceTime write SetP2PCheckConnectSpaceTime; //P2P连接检查间隔
    property P2PConnectingTimeout: Cardinal read FP2PConnectingTimeout write SetP2PConnectingTimeout; //P2P连接超时时间
    property P2PConnectMaxTimeoutCount: Integer read FP2PConnectMaxTimeoutCount write SetP2PConnectMaxTimeoutCount; //P2P连接重试次数
    property P2PHeartbeatSpaceTime: Cardinal read FP2PHeartbeatSpaceTime write SetP2PHeartbeatSpaceTime; //P2P心跳包间隔
    property P2PMaxWaitRecvTime: Cardinal read FP2PMaxWaitRecvTime write SetP2PMaxWaitRecvTime; //超过则表示已经断开P2P连接


    property Login: Boolean read FLogin write SetLogin; //同步登录命令

    property P2PUserManage: TxdP2PUserManage read FP2PUserManage;

    {外部提供对象}
    property ServerManage: TxdServerManage read FServerManage write FServerManage;
    property FileShareManage: TxdFileShareManage read FFileShareManage write FFileShareManage;
    property UploadFileManage: TxdFileUploadManage read FUploadFileManage write FUploadFileManage;
    property DownTaskManage: TxdDownTaskManage read FDownTaskManage write FDownTaskManage;
  end;


implementation

uses
  uSysInfo, uSocketSub, uJxdThread;

{ TxdUdpCommonClient }

function TxdUdpCommonClient.ConnectToClient(const AUserID: Cardinal): TConnectState;
var
  pUser: PClientPeerInfo;
  HelloCmd: TCmdP2PHelloInfo;
  CallMeCmd: TCmdCallMeInfo;
begin
  Result := csNULL;
  if not Assigned(FP2PUserManage) then Exit;
  FP2PUserManage.LockList;
  try
    pUser := FP2PUserManage.FindUserInfo( AUserID );
    if not Assigned(pUser) then Exit;
    if pUser^.FConnectState = csConnetSuccess then
    begin
      Result := csConnetSuccess;
      Exit;
    end;
    //向用户的网络发包
    HelloCmd.FCmdHead.FCmdID := CtCmdP2P_Hello;
    HelloCmd.FCmdHead.FUserID := FSelfID;
    HelloCmd.FCallUserID := pUser^.FUserID;
    HelloCmd.FSelfNetInfo.FUserID := FSelfID;
    HelloCmd.FSelfNetInfo.FPublicIP := FPublicIP;
    HelloCmd.FSelfNetInfo.FLocalIP := FLocalIP;
    HelloCmd.FSelfNetInfo.FPublicPort := FPublicPort;
    HelloCmd.FSelfNetInfo.FLocalPort := FLocalPort;

    SendBuffer( pUser^.FPublicIP, pUser^.FPublicPort, @HelloCmd, CtCmdP2PHelloInfoSize );
    Sleep(2);
    SendBuffer( pUser^.FLocalIP, pUser^.FLocalPort, @HelloCmd, CtCmdP2PHelloInfoSize );
    Sleep(2);

    //向服务器报告
    CallMeCmd.FCmdHead.FCmdID := CtCmd_CallMe;
    CallMeCmd.FCmdHead.FUserID := FSelfID;
    CallMeCmd.FCallUserID := pUser^.FUserID;
    SendToServer( @CallMeCmd, CtCmdCallMeInfoSize );

    if pUser^.FConnectState = csNULL then
      pUser^.FTimeoutCount := 0
    else
      Inc( pUser^.FTimeoutCount );
    pUser^.FConnectState := csConneting;
    pUser^.FLastSendActiveTime := GetTickCount;
    Result := pUser^.FConnectState;
  finally
    FP2PUserManage.UnLockList;
  end;
end;

constructor TxdUdpCommonClient.Create;
var
  strHash: string;
begin
  inherited;
  FServerID := 0;
  strHash := GetComputerStr;
  Move( strHash[1], FClientHash[0], 16 );
  FSelfID := 0;
  FClientVersion := 100;
  SynchroCmd := CtCmdSynchroPackage;
  FCheckKeepLiveTimeSpace := 18 * 1000;
  FMaxSendIdleTime := 15 * 1000;
  FMaxRecvIdleTime := 60 * 1000;
  FKeepSrvThreadClose := True;

  FP2PCheckConnectSpaceTime := 16 * 1000;
  FP2PConnectingTimeout := 10 * 1000;
  FP2PConnectMaxTimeoutCount := 3;
  FP2PHeartbeatSpaceTime := 15 * 1000;
  FP2PMaxWaitRecvTime := 70 * 1000;
end;

destructor TxdUdpCommonClient.Destroy;
begin
  inherited;
end;

procedure TxdUdpCommonClient.DoAfterCloseUDP;
begin
  inherited;
  FreeAndNil( FP2PUserManage );
end;

procedure TxdUdpCommonClient.DoAfterOpenUDP;
var
  IPs: array[0..10] of Cardinal;
begin
  inherited;
  GetLocalIPs( IPs );
  FLocalIP := IPs[0];
  FLocalPort := Port;
end;

procedure TxdUdpCommonClient.DoBeforCloseUDP;
begin
  inherited;
  Login := False;
end;

procedure TxdUdpCommonClient.DoHandleCmd(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
begin

end;

procedure TxdUdpCommonClient.DoHandleCmdP2P_Hello(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdP2PHelloInfo;
  oSendStream: TxdStaticMemory_128Byte;
  info: TUserNetInfo;
begin
  if ABufLen <> CtCmdP2PHelloInfoSize then
  begin
    DoErrorInfo( '接收到无效的HELLO命令' );
    Exit;
  end;
  pCmd := PCmdP2PHelloInfo(ABuffer);
  if pCmd^.FCallUserID = UserID then
  begin
    if pCmd^.FSelfNetInfo.FUserID <> pCmd^.FCmdHead.FUserID then
    begin
      DoErrorInfo( '接收到HELLO命令有错误' );
      Exit;
    end;
    if FP2PUserManage.AddConnectingUser(AIP, APort, @pCmd^.FSelfNetInfo) then
    begin
      //添加成功，回复命令
      oSendStream := TxdStaticMemory_128Byte.Create;
      try
        if AIsSynchroCmd then
          AddSynchroSign( oSendStream, ASynchroID );
        AddCmdHead( oSendStream, CtCmdP2P_ReplyHello );
        oSendStream.WriteCardinal( pCmd^.FCmdHead.FUserID );
        info.FUserID := UserID;
        info.FPublicIP := FPublicIP;
        info.FLocalIP := FLocalIP;
        info.FPublicPort := FPublicPort;
        info.FLocalPort := FLocalPort;
        oSendStream.WriteLong( info, CtUserNetInfoSize );
        SendBuffer( AIP, APort, oSendStream.Memory, oSendStream.Position );
      finally
        oSendStream.Free;
      end;
    end
    else
      DoErrorInfo( '接收到的Hello包有错误(%s)', [IpToStr(AIP, APort)] );
  end
  else
    DoErrorInfo( '接收到其它客户的P2P命令, 直接丢弃' );
end;

procedure TxdUdpCommonClient.DoHandleCmdP2P_ReplyHello(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdP2PReplyHelloInfo;
begin
  if ABufLen <> CtCmdP2PReplyHelloInfoSize then
  begin
    DoErrorInfo( '接收到不正确的P2P回复命令' );
    Exit;
  end;
  pCmd := PCmdP2PReplyHelloInfo(ABuffer);
  if pCmd^.FCallUserID <> UserID then
  begin
    DoErrorInfo( '接收到非此用户的P2P回复命令' );
    Exit;
  end;
  FP2PUserManage.AddConnectingUser( AIP, APort, @pCmd^.FSelfNetInfo );
end;

procedure TxdUdpCommonClient.DoHandleCmdP2P_StringInfo(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
var
  cmdStream: TxdOuterMemory;
  strInfo: string;
  nID:Cardinal;
begin
  if ABufLen < CtMinP2PStringInfoSize then
  begin
    DoErrorInfo( '接收到的P2PStringInfo命令有错误' );
    Exit;
  end;
  
  cmdStream := TxdOuterMemory.Create;
  try
    cmdStream.InitMemory( ABuffer, ABufLen );
    cmdStream.Position := 2;
    nID := cmdStream.ReadCardinal;
    strInfo := cmdStream.ReadStringEx;
  finally
    cmdStream.Free;
  end;
  DoRecvP2PStringInfo( nID, strInfo );
end;

procedure TxdUdpCommonClient.DoHandleCmd_CallFriend(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
var
  pCmd: PCmdCallFriendInfo;
begin
  if ABufLen <> CtCmdCallFriendInfoSize then
  begin
    DoErrorInfo( '接收到的CallFriend包无法识别' );
    Exit;
  end;
  pCmd := PCmdCallFriendInfo(ABuffer);
  if FP2PUserManage.AddUserInfo(@pCmd^.FUserNetInfo) then
    ConnectToClient( pCmd^.FUserNetInfo.FUserID );
end;

procedure TxdUdpCommonClient.DoHandleCmd_Heartbeat(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdHeartbeatInfo;
  oSendMem: TxdStaticMemory_16Byte;
begin
  if ABufLen <> CtCmdHeartbeatInfoSize then
  begin
    DoErrorInfo( '接收到非法的心跳包命令' );
    Exit;
  end;
  pCmd := PCmdHeartbeatInfo( ABuffer );
  if pCmd^.FNeedReply then
  begin
    oSendMem := TxdStaticMemory_16Byte.Create;
    try
      if AIsSynchroCmd then
        AddSynchroSign( oSendMem, ASynchroID );
      AddCmdHead( oSendMem, CtCmdReply_Heartbeat );
      oSendMem.WriteByte( Byte(False) );
      SendStream( AIP, APort, oSendMem );
    finally
      oSendMem.Free;
    end;
  end;
end;

procedure TxdUdpCommonClient.DoHandleCmd_ReplyCallMe(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
var
  pCmd: PCmdReplyCallMeInfo;
  pUser: PUserNetInfo;
begin
  if ABufLen <> CtCmdReplyCallMeInfoSize then
  begin
    DoErrorInfo( '接收CallMe返回信息出错' );
    Exit;
  end;
  pCmd := PCmdReplyCallMeInfo(ABuffer);
  pUser := @pCmd^.FUserNetInfo;
  if pCmd^.FReplySign = rsNotExistsID then
  begin
    //要连接的用户已经不存在了
    FP2PUserManage.DeleteUser( pUser^.FUserID, drInvalideID );
    Exit;
  end;
  if pCmd^.FReplySign = rsSuccess then
    FP2PUserManage.UpdateUserInfo( pUser );
end;

procedure TxdUdpCommonClient.DoHandleCmd_ReplyGetRandomUsers(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
var
  pCmd: PCmdReplyGetRandomUsersInfo;
  nCount: Byte;
  pUser: PUserNetInfo;
begin
  if (ABufLen < CtMinReplyGetRandomPackageSize) or (ABufLen > CtMaxReplyGetRandomPackageSize) then
  begin
    DoErrorInfo( '接收到的随机在线用户信息命令出错' );
    Exit;
  end;
  pCmd := PCmdReplyGetRandomUsersInfo( ABuffer );
  if pCmd^.FReplySign <> rsSuccess then
  begin
    FCurOnlineCount := pCmd^.FOnlineUserCount;
    DoErrorInfo( '服务器返回不可用的随机在线用户信息' );
    Exit;
  end;
  nCount := pCmd^.FReplyCount;
  if nCount > CtMaxSearchRandomUserCount then
  begin
    DoErrorInfo( '接收到的随机在线用户信息命令出错' );
    Exit;
  end;
  FCurOnlineCount := pCmd^.FOnlineUserCount;
  pUser := @pCmd^.FUserInfo;
  while nCount > 0 do
  begin
    FP2PUserManage.AddUserInfo( pUser );
    pUser := PUserNetInfo( Integer(pUser) + CtUserNetInfoSize );
    nCount := nCount - 1;
  end;
end;

procedure TxdUdpCommonClient.DoHandleCmd_ReplyGetServerAddr(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
var
  pCmd: PCmdReplyGetServerAddrInfo;
  i: integer;
  info: TServerManageInfo;
begin
  if ABufLen < CtCmdReplyGetServerAddrInfoSize then
  begin
    DoErrorInfo( 'DoHandleCmd_ReplyGetServerAddr 长度不正确' );
    Exit;
  end;
  pCmd := PCmdReplyGetServerAddrInfo(ABuffer);
  if pCmd^.FReplySign = rsSuccess then
  begin
    for i := 0 to pCmd^.FServerCount - 1 do
    begin
      info.FServerStyle := pCmd^.FServerInfo[i].FServerStyle;
      info.FServerID := 0;
      info.FServerIP := pCmd^.FServerInfo[i].FServerIP;
      info.FServerPort := pCmd^.FServerInfo[i].FServerPort;
      info.FTag := 0;
      FServerManage.AddServerInfo( @info, 0 );
    end;
  end;
end;

procedure TxdUdpCommonClient.DoHandleRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pHead: PCmdHead;
begin
  //已确定包的最小长度为 4
  pHead := PCmdHead( ABuffer );
  case pHead^.FCmdID of
    CtCmd_Heartbeat: DoHandleCmd_Heartbeat( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmdReply_GetRandomUsers: DoHandleCmd_ReplyGetRandomUsers( AIP, APort, ABuffer, ABufLen );
    CtCmdReply_CallMe: DoHandleCmd_ReplyCallMe( AIP, APort, ABuffer, ABufLen );
    CtCmd_CallFriend: DoHandleCmd_CallFriend( AIP, APort, ABuffer, ABufLen );
    CtCmdReply_GetServerAddr: DoHandleCmd_ReplyGetServerAddr( AIP, APort, ABuffer, ABufLen );

    CtCmdP2P_Hello: DoHandleCmdP2P_Hello( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmdP2P_ReplyHello: DoHandleCmdP2P_ReplyHello( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmdP2P_StringInfo: DoHandleCmdP2P_StringInfo( AIP, APort, ABuffer, ABufLen );

    CtCmd_RequestFileData: FUploadFileManage.DoHandleCmd_RequestFileData( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_FileExists: FUploadFileManage.DoHandleCmd_FileExists( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );

    CtCmdReply_RequestFileData: FDownTaskManage.DoHandleCmdReply_RequestFileData( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmdReply_FileExists: FDownTaskManage.DoHandleCmdReply_FileExists( AIP, APort, ABuffer, ABufLen );
    CtCmdReply_GetFileSegmentHash: FDownTaskManage.DoHandleCmdReply_FileSegmentHash( AIP, APort, ABuffer, ABufLen );
    else
      DoHandleCmd( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );  
  end;
end;

procedure TxdUdpCommonClient.DoCheckP2PUser(const ACurTime: Cardinal; Ap: PClientPeerInfo; var ADel: Boolean);
var
  nIP: Cardinal;
  nPort: Word;
begin
  //检查线程中，

  {维持已连接客户端}
  if Ap^.FConnectState = csConnetSuccess then
  begin
    //只处理已经连接的P2P用户
    if ACurTime - Ap^.FLastRecvActiveTime > P2PMaxWaitRecvTime then
    begin
      //超时断开
      ADel := True;
      if Assigned(FP2PUserManage.OnDelUser) then
        FP2PUserManage.OnDelUser( Ap, drTimeout );
      Exit;
    end;
    if ACurTime - Ap^.FLastSendActiveTime >= P2PHeartbeatSpaceTime then
    begin
      //重发心跳包
      GetClientIP(Ap, nIp, nPort);
      SendCmd_Heartbeat(nIp, nPort, False );
      Ap^.FLastSendActiveTime := GetTickCount;
    end;
  end
  else if Ap^.FConnectState = csConneting then
  begin
    //
    if ACurTime - Ap^.FLastSendActiveTime >= P2PConnectingTimeout then
    begin
      //P2P连接超时，
      if Ap^.FTimeoutCount < P2PConnectMaxTimeoutCount then
        ConnectToClient( Ap^.FUserID )
      else
      begin
        Ap^.FConnectState := csConnetFail;
        DoP2PConnectFail( Ap );
      end;
    end;
  end;
  //结束
end;

procedure TxdUdpCommonClient.DoDeleteUser(Ap: PClientPeerInfo; const AReason: TDeleteUserReason);
var
  strInfo: string;
begin
  case AReason of
    drInvalideID: strInfo := '无效的ID';
    drSelf: strInfo := '自身删除用户';
    drTimeout: strInfo := '超时删除用户';
  end;
  JxdDbg( '删除指定客户端(%d): %s', [Ap^.FUserID, strInfo] );
end;

procedure TxdUdpCommonClient.DoP2PConnected(Ap: PClientPeerInfo);
begin
  JxdDbg( '已经建立起P2P连接' );
end;

procedure TxdUdpCommonClient.DoP2PConnectFail(Ap: PClientPeerInfo);
begin

end;

procedure TxdUdpCommonClient.DoRecvP2PStringInfo(const AUserID: Cardinal; const ARecvInfo: string);
begin
  JxdDbg( '接收到P2P(%d)信息: %s', [AUserID, ARecvInfo] );
end;

procedure TxdUdpCommonClient.DoThreadCheckKeepLive;
var
  i: Integer;
  bOK: Boolean;
  nTemp: Cardinal;
begin
  FKeepSrvThreadClose := False;
  while Login and Active do
  begin
    WaitForSingleObject( FKeepServerLiveEvent, FCheckKeepLiveTimeSpace );
    if (not Login) or (not Active) then Break;

    //检查自己与服务器之间的连接情况
    nTemp := GetTickCount;
    if (nTemp > FLastRecvActiveTime) and (nTemp - FLastRecvActiveTime > MaxRecvIdleTime) then
    begin
      //超出最大接收空闲，发送需要返回的接收包
      if SendCmd_Heartbeat(FServerIP, FServerPort, True) then
      begin
        FLastSendActiveTime := GetTickCount;
        FLastRecvActiveTime := FLastSendActiveTime;
      end
      else
      begin
        //无法接收到返回的包，表示网络不通或服务器可能已经关闭
        //再重试几次
        for i := 0 to 10 do
        begin
          bOK := SendCmd_Heartbeat(FServerIP, FServerPort, True);
          if bOK then Break;
        end;
        if not bOK then
          OnFailToConnectedServer;  //无法接收到服务器的心跳包，退出
      end;
    end
    else if GetTickCount - FLastSendActiveTime >= MaxSendIdleTime then
    begin
      //超出最大发送空闲，发送不需要服务器返回的心跳包
      SendCmd_Heartbeat( FServerIP, FServerPort, False );
      FLastSendActiveTime := GetTickCount;
    end;
  end;
  FKeepSrvThreadClose := True;
end;

procedure TxdUdpCommonClient.DoUpdateClientInfo(Ap: PClientPeerInfo);
begin
  JxdDbg( '更新在线用户信息' );
  if Ap^.FConnectState = csConneting then
  begin
    Ap^.FTimeoutCount := 0;
    ConnectToClient( Ap^.FUserID );
  end;
end;

procedure TxdUdpCommonClient.GetRandomOnlineUsers;
var
  cmd: TCmdGetRandomUsersInfo;
begin
  if Login then
  begin
    cmd.FCmdHead.FCmdID := CtCmd_GetRandomUsers;
    cmd.FCmdHead.FUserID := UserID;
    SendToServer( @Cmd, 6 );
  end
  else
    DoErrorInfo( '请先登录服务器' );
end;

function TxdUdpCommonClient.GetSelfID: Cardinal;
begin
  Result := FSelfID;
end;

procedure TxdUdpCommonClient.LoginToServer;
begin
  SendCmd_Login;
  if FLogin then
  begin
    FLastSendActiveTime := GetTickCount;
    FLastRecvActiveTime := FLastSendActiveTime;
    FKeepServerLiveEvent := CreateEvent( nil, False, False, nil );

    FP2PUserManage := TxdP2PUserManage.Create;
    with FP2PUserManage do
    begin
      CheckThreadSpaceTime := FP2PCheckConnectSpaceTime;
      OnUpdateUserNetInfo := DoUpdateClientInfo;
      OnP2PConnected := DoP2PConnected;
      OnCheckP2PUser := DoCheckP2PUser;
      OnDelUser := DoDeleteUser;
    end;
    SendCmd_GetServerAddr;
    RunningByThread( DoThreadCheckKeepLive );
  end;
end;

procedure TxdUdpCommonClient.LogoutFromServer;
begin
  SendCmd_Logout;
  if not FKeepSrvThreadClose then
  begin
    SetEvent( FKeepServerLiveEvent );
    while not FKeepSrvThreadClose do
    begin
      Sleep( 10 );
      SetEvent( FKeepServerLiveEvent );
    end;
  end;
  CloseHandle( FKeepServerLiveEvent );
end;

procedure TxdUdpCommonClient.OnCommonRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdHead;
begin
  if ABufLen < CtMinPackageLen then
  begin
    DoErrorInfo( '接收到的包长度过小，丢弃此包' );
    Exit;
  end;

  //对命令进行处理
  inherited OnCommonRecvBuffer( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );

  //更新最后接收时间
  pCmd := PCmdHead(ABuffer);
  if (pCmd^.FUserID = ServerID) and (AIP = ServerIP) and (APort = ServerPort) then
  begin
    //接收到来自服务器的命令
    FLastRecvActiveTime := GetTickCount;
  end
  else
  begin
    //P2P命令
    if Assigned(FP2PUserManage) then
      FP2PUserManage.ActiveUserRecvTime( pCmd^.FUserID );
  end;
end;

procedure TxdUdpCommonClient.OnFailToConnectedServer;
begin
  Login := False;
  DoErrorInfo( '与服务器断开联系' );
end;

procedure TxdUdpCommonClient.SendCmd_GetServerAddr;
var
  cmd: TCmdGetServerAddrInfo;
  nCurTime: Cardinal;
begin
  if not Assigned(FServerManage) then Exit;
  nCurTime := GetTickCount;
  if (FServerManage.LastUpdateTime = 0) or ((nCurTime > FServerManage.LastUpdateTime) and (nCurTime - FServerManage.LastUpdateTime > 1000 * 10)) then
  begin
    FServerManage.LastUpdateTime := GetTickCount;
    cmd.FCmdHead.FCmdID := CtCmd_GetServerAddr;
    cmd.FCmdHead.FUserID := UserID;
    SendBuffer( FServerIP, FServerPort, PAnsiChar(@Cmd), CtCmdGetServerAddrInfoSize );
  end;
end;

function TxdUdpCommonClient.SendCmd_Heartbeat(const AIP: Cardinal; const APort: Word; const ANeedReply: Boolean): Boolean;
var
  oSendMem: TxdStaticMemory_16Byte;
  aRecvMem: array[0..16] of Byte;
  pReplyBuf: PAnsiChar;
  nRecvLen: Integer;
  SendResult: TSynchroResult;
  pCmd: PCmdHeartbeatInfo;
begin
  Result := False;
  SendResult := srSuccess;
  oSendMem := TxdStaticMemory_16Byte.Create;
  try
    if ANeedReply then
      oSendMem.Position := 4;
    AddCmdHead( oSendMem, CtCmd_Heartbeat );
    oSendMem.WriteByte( Byte(ANeedReply) );
    if ANeedReply then
    begin
      pReplyBuf := @aRecvMem;
      nRecvLen := 16;
      SendResult := SendSynchroBuffer( AIP, APort, oSendMem.Memory, oSendMem.Position, pReplyBuf, nRecvLen, False );
      case SendResult of
        srNoEnoughSynchroID, srFail:
        begin
          DoErrorInfo( '无法响应心跳包' );
          Exit;;
        end;
      end;
      if nRecvLen <> CtCmdHeartbeatInfoSize then
      begin
        DoErrorInfo( '同步心跳包协议出错' );
        Exit;
      end;
      Result := True;
      pCmd := PCmdHeartBeatInfo( pReplyBuf );
      if pCmd^.FNeedReply then
      begin
        oSendMem.Clear;
        AddCmdHead( oSendMem, CtCmd_Heartbeat );
        oSendMem.WriteByte( Byte(False) );
        SendStream( AIP, APort, oSendMem );
      end;
    end
    else
      Result := SendStream( AIP, APort, oSendMem );
  finally
    if SendResult = srSystemBuffer then
      ReleaseSynchroRecvBuffer( pReplyBuf );
    oSendMem.Free;
  end;
end;

procedure TxdUdpCommonClient.SendCmd_Login;
var
  oSendMem: TxdStaticMemory_64Byte;
  aRecvMem: array[0..64] of Byte;
  SendResult: TSynchroResult;
  pReplyBuf: PAnsiChar;
  nRecvLen: Integer;
  pReplyCmd: PCmdReplyLoginInfo;
begin
  //先判断是否要注册ID
  if UserID < CtMinUserID then
  begin
    UserID := SendCmd_Register;
    if UserID < CtMinUserID then
    begin
      DoErrorInfo( '无法注册ID' );
      Exit;
    end;
  end;
  //开始尝试登录
  pReplyBuf := @aRecvMem;
  nRecvLen := 64;
  SendResult := srFail;
  oSendMem := TxdStaticMemory_64Byte.Create;
  try
    oSendMem.Position := 4;  //预留四个字节写入同步包信息
    AddCmdHead( oSendMem, CtCmd_Login );
    oSendMem.WriteWord( FClientVersion );
    oSendMem.WriteCardinal( LocalIP );
    oSendMem.WriteWord( LocalPort );
    oSendMem.WriteLong( FClientHash[0], 16 );

    SendResult := SendSynchroBuffer( FServerIP, FServerPort, oSendMem.Memory, oSendMem.Position, pReplyBuf, nRecvLen );
    case SendResult of
      srNoEnoughSynchroID, srFail:
      begin
        DoErrorInfo( '无法登录服务器' );
        Exit;;
      end;
    end;
    if nRecvLen <> CtCmdReplyLoginInfoSize then
    begin
      DoErrorInfo( '登录返回的命令无法解析' );
      Exit;;
    end;
    pReplyCmd := PCmdReplyLoginInfo( pReplyBuf );
    FLastServerReplySign := pReplyCmd^.FReplySize;
    if FLastServerReplySign <> rsSuccess then
    begin
      DoErrorInfo( '无法登录服务器' );
      Exit;
    end;
    FPublicIP := pReplyCmd^.FPublicIP;
    FPublicPort := pReplyCmd^.FPublicPort;
    FSafeSign := pReplyCmd^.FSafeSign;
    FLogin := UserID > CtMinUserID;
    if FLogin then
      FServerID := pReplyCmd^.FCmdHead.FUserID;
  finally
    if SendResult = srSystemBuffer then
      ReleaseSynchroRecvBuffer( pReplyBuf );
    oSendMem.Free;
  end;
end;

procedure TxdUdpCommonClient.SendCmd_Logout;
var
  cmd: TCmdLogoutInfo;
begin
  if FLogin then
  begin
    cmd.FCmdHead.FCmdID := CtCmd_Logout;
    cmd.FCmdHead.FUserID := UserID;
    cmd.FSafeSign := FSafeSign;
    SendBuffer( FServerIP, FServerPort, @Cmd, CtCmdLogoutInfoSize );
    FLogin := False;
  end;
end;

function TxdUdpCommonClient.SendCmd_Register: Cardinal;
var
  oSendMem: TxdStaticMemory_32Byte;
  aRecvMem: array[0..31] of Byte;
  SendResult: TSynchroResult;
  pReplyBuf: PAnsiChar;
  nRecvLen: Integer;
  pReplyCmd: PCmdReplyRegisterInfo;
begin
  pReplyBuf := @aRecvMem;
  nRecvLen := 32;
  SendResult := srFail;
  oSendMem := TxdStaticMemory_32Byte.Create;
  try
    oSendMem.Position := 4;  //预留四个字节写入同步包信息
    AddCmdHead( oSendMem, CtCmd_Register );
    oSendMem.WriteLong( FClientHash[0], 16 );
    SendResult := SendSynchroBuffer( FServerIP, FServerPort, oSendMem.Memory, oSendMem.Position, pReplyBuf, nRecvLen );
    case SendResult of
      srNoEnoughSynchroID, srFail:
      begin
        DoErrorInfo( '无法注册用户ID' );
        Result := 0;
        Exit;;
      end;
    end;
    if nRecvLen <> CtCmdReplyRegisterInfoSize then
    begin
      DoErrorInfo( '注册返回的命令无法解析' );
      Result := 0;
      Exit;;
    end;
    pReplyCmd := PCmdReplyRegisterInfo( pReplyBuf );
    FLastServerReplySign := pReplyCmd^.FReplySign;
    Result := pReplyCmd^.FRegisterID;
  finally
    if SendResult = srSystemBuffer then
      ReleaseSynchroRecvBuffer( pReplyBuf );
    FreeAndNil( oSendMem );
  end;
end;

function TxdUdpCommonClient.SendStream(const AIP: Cardinal; APort: Word; AStream: TxdMemoryHandle): Boolean;
begin
  Result := SendBuffer( AIP, APort, AStream.Memory, AStream.Position ) = AStream.Position;
end;

procedure TxdUdpCommonClient.SendStringToClient(const AUserID: Cardinal; const AInfo: string);
var
  oSendStream: TxdStaticMemory_4K;
  pUser: PClientPeerInfo;
  nIP: Cardinal;
  nPort: Word;
  bOK: Boolean;
begin
  if Length(AInfo) > 1024 * 4 then
  begin
    DoErrorInfo( '发送的字符串数量过多' );
    Exit;
  end;
  FP2PUserManage.LockList;
  try
    pUser := FP2PUserManage.FindUserInfo( AUserID );
    if not Assigned(pUser) then
    begin
      DoErrorInfo( '找不到用户' );
      Exit;
    end;
    if pUser^.FConnectState <> csConnetSuccess then
    begin
      DoErrorInfo( '指定用户还没有建立P2P连接' );
      Exit;
    end;
    bOK := True;
    GetClientIP( pUser, nIP, nPort );
  finally
    FP2PUserManage.UnLockList;
  end;
  if bOK then
  begin
    oSendStream := TxdStaticMemory_4K.Create;
    try
      AddCmdHead( oSendStream, CtCmdP2P_StringInfo );
      oSendStream.WriteStringEx( AInfo );
      SendBuffer( nIP, nPort, oSendStream.Memory, oSendStream.Position );
    finally
      oSendStream.Free;
    end;
  end;
end;

function TxdUdpCommonClient.SendToServer(AStream: TxdMemoryHandle): Boolean;
begin
  Result := SendToServer( AStream.Memory, AStream.Position );
end;

function TxdUdpCommonClient.SendToServer(const ABuffer: PAnsiChar; const ABufLen: Integer): Boolean;
begin
  Result := SendBuffer( ServerIP, ServerPort, ABuffer, ABufLen ) = ABufLen;
  if Result then
    FLastSendActiveTime := GetTickCount;
end;

procedure TxdUdpCommonClient.SetCheckKeepLiveTimeSpace(const Value: Cardinal);
begin
  FCheckKeepLiveTimeSpace := Value;
end;

procedure TxdUdpCommonClient.SetClientVersion(const Value: Word);
begin
  if not Login then
    FClientVersion := Value;
end;

procedure TxdUdpCommonClient.SetLogin(const Value: Boolean);
begin
  if FLogin <> Value then
  begin
    if Value then
      LoginToServer
    else
      LogoutFromServer;
  end;
end;

procedure TxdUdpCommonClient.SetMaxRecvIdleTime(const Value: Cardinal);
begin
  FMaxRecvIdleTime := Value;
end;

procedure TxdUdpCommonClient.SetMaxSendIdleTime(const Value: Cardinal);
begin
  FMaxSendIdleTime := Value;
end;

procedure TxdUdpCommonClient.SetP2PCheckConnectSpaceTime(const Value: Cardinal);
begin
  FP2PCheckConnectSpaceTime := Value;
  if Assigned(FP2PUserManage) then
    FP2PUserManage.CheckThreadSpaceTime := FP2PCheckConnectSpaceTime;
end;

procedure TxdUdpCommonClient.SetP2PConnectingTimeout(const Value: Cardinal);
begin
  FP2PConnectingTimeout := Value;
end;

procedure TxdUdpCommonClient.SetP2PConnectMaxTimeoutCount(const Value: Integer);
begin
  FP2PConnectMaxTimeoutCount := Value;
end;

procedure TxdUdpCommonClient.SetP2PHeartbeatSpaceTime(const Value: Cardinal);
begin
  FP2PHeartbeatSpaceTime := Value;
end;

procedure TxdUdpCommonClient.SetP2PMaxWaitRecvTime(const Value: Cardinal);
begin
  FP2PMaxWaitRecvTime := Value;
end;

procedure TxdUdpCommonClient.SetServerIP(const Value: Cardinal);
begin
  if not Login then
    FServerIP := Value;
end;

procedure TxdUdpCommonClient.SetServerPort(const Value: Word);
begin
  if not Login then
    FServerPort := Value;
end;

procedure TxdUdpCommonClient.SetUserID(const Value: Cardinal);
begin
  if not Login then
    FSelfID := Value;
end;

end.
