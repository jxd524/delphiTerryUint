unit uJxdP2PUserManage;

interface

uses
  Windows, SysUtils, Classes, WinSock2, uJxdDataStream,
  uJxdUdpBasic, uJxdUdpSynchroClient, uJxdUdpDefine;

type
  {客户端节点信息, P2P使用}  
  TConAddrStyle = (caPublic, caLocal, caBoth);
  PClientPeerInfo = ^TClientPeerInfo;
  TClientPeerInfo = record
    FUserID: Cardinal;              //客户端ID
    FPublicIP, FLocalIP: Cardinal;  //网络地址
    FPublicPort, FLocalPort: Word;
    FConnectState: TConnectState;   //当前连接状态
    FConAddrStyle: TConAddrStyle;   //指明使用那个地址来通讯, 当 FConnectState = csConnetSuccess 时有效
    FClientVersion: Word;           //当前使用版本号
    FLastSendActiveTime: Cardinal;  //最后发送时间
    FLastRecvActiveTime: Cardinal;  //最后接收时间
    FTimeoutCount: Integer;         //超时次数
  end;

  TOnClientPeerInfo = procedure(Ap: PClientPeerInfo) of object;
  TOnCheckP2PUser = procedure(const ACurTime: Cardinal; Ap: PClientPeerInfo; var ADel: Boolean) of object;
  TDeleteUserReason = (drInvalideID, drSelf, drTimeout, drNotify);
  TOnDeleteUser = procedure(Ap: PClientPeerInfo; const AReason: TDeleteUserReason) of object;

  TxdP2PUserManage = class
  public
    constructor Create;
    destructor  Destroy; override;

    procedure LockList; inline;
    procedure UnLockList; inline;

    function  FindUserInfo(const AUserID: Cardinal): PClientPeerInfo; //非锁定状态, 外部手工调用Lock
    function  AddUserInfo(const ApUser: PUserNetInfo): Boolean; //自动进入锁定状态
    function  AddConnectingUser(const AIP: Cardinal; const APort: Word; pUser: PUserNetInfo): Boolean; //锁定，判断是否有效，并加入到列表
    procedure DeleteUser(const AUserID: Cardinal; const AReason: TDeleteUserReason);
    procedure UpdateUserInfo(const ApNet: PUserNetInfo); //在未连接时可进行更新
    procedure ActiveUserRecvTime(const AUserID: Cardinal); //只对已连接用户有效
  private
    FCloseing, FThreadRunning: Boolean;
    FCheckEvent: Cardinal;
    procedure DoThreadCheck;
    procedure DoUpdateUserInfo(ApUser: PClientPeerInfo; ApNet: PUserNetInfo);
  private
    FUserList: TList;
    FLock: TRTLCriticalSection;
    FCheckThreadSpaceTime: Cardinal;
    FOnAddUser: TOnClientPeerInfo;
    FOnDelUser: TOnDeleteUser;
    FOnUpdateUserNetInfo: TOnClientPeerInfo;
    FOnP2PConnected: TOnClientPeerInfo;
    FOnCheckP2PUser: TOnCheckP2PUser;
    function GetCount: Integer;
    function GetItem(index: Integer): PClientPeerInfo;
    procedure SetCheckThreadSpaceTime(const Value: Cardinal);
  public
    property CheckThreadSpaceTime: Cardinal read FCheckThreadSpaceTime write SetCheckThreadSpaceTime;
    property Count: Integer read GetCount;
    property Item[index: Integer]: PClientPeerInfo read GetItem;

    property OnAddUser: TOnClientPeerInfo read FOnAddUser write FOnAddUser;
    property OnDelUser: TOnDeleteUser read FOnDelUser write FOnDelUser;
    property OnUpdateUserNetInfo: TOnClientPeerInfo read FOnUpdateUserNetInfo write FOnUpdateUserNetInfo;
    property OnP2PConnected: TOnClientPeerInfo read FOnP2PConnected write FOnP2PConnected;
    property OnCheckP2PUser: TOnCheckP2PUser read FOnCheckP2PUser write FOnCheckP2PUser;
  end;

procedure GetClientIP(const Ap: PClientPeerInfo; var AIP: Cardinal; var APort: Word);
function GetConnectString(AState: TConnectState): string;

implementation

uses
  uSysInfo, uSocketSub, uJxdThread;

procedure GetClientIP(const Ap: PClientPeerInfo; var AIP: Cardinal; var APort: Word);
begin
  case Ap^.FConAddrStyle of
    caPublic:
    begin
      AIP := Ap^.FPublicIP;
      APort := Ap^.FPublicPort;
    end
    else
    begin
      AIP := Ap^.FLocalIP;
      APort := Ap^.FLocalPort;
    end;
  end;
end;

function GetConnectString(AState: TConnectState): string;
begin
  case AState of
    csNULL:  Result := '未连接';
    csConneting: Result := '正在建立连接';
    csConnetFail: Result := '无法建立连接';
    csConnetSuccess: Result := '已经成功建立连接';
    else
      Result := '未知的状态（出错)';
  end;
end;

const
  CtClientPeerInfoSize = SizeOf(TClientPeerInfo);

{ TxdP2PUserManage }

function TxdP2PUserManage.AddUserInfo(const ApUser: PUserNetInfo): Boolean;
var
  i: Integer;
  p: PClientPeerInfo;
  bAdd: Boolean;
begin
  Result := False;
  LockList;
  try
    bAdd := True;
    for i := 0 to FUserList.Count - 1 do
    begin
      p := FUserList[i];
      if p^.FUserID = ApUser^.FUserID then
      begin
        bAdd := False;
        //更新状态
        DoUpdateUserInfo( p, ApUser );
        Break;
      end;
    end;
    if bAdd then
    begin
      New( p );
      p^.FUserID := ApUser^.FUserID;
      p^.FPublicIP := ApUser^.FPublicIP;
      p^.FLocalIP := ApUser^.FLocalIP;
      p^.FPublicPort := ApUser^.FPublicPort;
      p^.FLocalPort := ApUser^.FLocalPort;
      p^.FConnectState := csNULL;
      p^.FClientVersion := 0;
      p^.FLastSendActiveTime := 0;
      p^.FLastRecvActiveTime := 0;
      p^.FTimeoutCount := 0;
      if Assigned(OnAddUser) then
        OnAddUser( p );
      FUserList.Add(p);
      Result := True;
    end;
  finally
    UnLockList;
  end;
end;

procedure TxdP2PUserManage.ActiveUserRecvTime(const AUserID: Cardinal);
var
  p: PClientPeerInfo;
begin
  LockList;
  try
    p := FindUserInfo( AUserID );
    if Assigned(p) then
      p^.FLastRecvActiveTime := GetTickCount;
  finally
    UnLockList;
  end;
end;

function TxdP2PUserManage.AddConnectingUser(const AIP: Cardinal; const APort: Word; pUser: PUserNetInfo): Boolean;
var
  p: PClientPeerInfo;
  addrStyle: TConAddrStyle;
begin
  if (pUser^.FPublicIP = AIP) and (pUser^.FPublicPort = APort) then
    addrStyle := caPublic
  else if (pUser^.FLocalIP = AIP) and (pUser^.FLocalPort = APort) then
    addrStyle := caLocal
  else
  begin
    //直接当成是public信息处理
    addrStyle := caPublic;
    pUser^.FPublicIP := AIP;
    pUser^.FPublicPort := APort;    
  end;
  

  LockList;
  try
    p := FindUserInfo( pUser^.FUserID );
    if Assigned(p) then
    begin
      //已经存在的节点
      if p^.FConnectState = csConneting then
      begin
        p^.FConnectState := csConnetSuccess;
        p^.FConAddrStyle := addrStyle;
        p^.FTimeoutCount := 0;
      end
      else if p^.FConnectState = csConnetSuccess then
      begin
        if p^.FConAddrStyle <> addrStyle then
          p^.FConAddrStyle := caBoth;
        Result := True;
        Exit;
      end
      else
        p^.FConAddrStyle := addrStyle;
    end
    else
    begin
      //新增节点
      New( p );
      p^.FUserID := pUser^.FUserID;
      p^.FConnectState := csConnetSuccess;
      p^.FConAddrStyle := addrStyle;
      p^.FClientVersion := 0;
      p^.FLastSendActiveTime := 0;
      p^.FTimeoutCount := 0;
      FUserList.Add( p );
    end;
    
    p^.FPublicIP := pUser^.FPublicIP;
    p^.FLocalIP := pUser^.FLocalIP;
    p^.FPublicPort := pUser^.FPublicPort;
    p^.FLocalPort := pUser^.FLocalPort;
    p^.FLastRecvActiveTime := GetTickCount;
    Result := True;
    if Assigned(OnP2PConnected) then
      OnP2PConnected( p );
  finally
    UnLockList;
  end;
end;

constructor TxdP2PUserManage.Create;
begin
  FUserList := TList.Create;
  InitializeCriticalSection( FLock );
  CheckThreadSpaceTime := 10 * 1000;
  FCloseing := False;
  FCheckEvent := CreateEvent( nil, False, False, nil );
  FThreadRunning := False;
  RunningByThread( DoThreadCheck );
end;

procedure TxdP2PUserManage.DeleteUser(const AUserID: Cardinal; const AReason: TDeleteUserReason);
var
  i: Integer;
  p: PClientPeerInfo;
begin
  LockList;
  try
    for i := 0 to FUserList.Count - 1 do
    begin
      p := FUserList[i];
      if p^.FUserID = AUserID then
      begin
        if (AReason = drInvalideID) and (p^.FConnectState = csConnetSuccess) then Exit;
        
        //释放此用户
        FUserList.Delete( i );
        if Assigned(OnDelUser) then
          OnDelUser( p, AReason );
        Dispose( p );
        Break;
      end;
    end;
  finally
    UnLockList;
  end;
end;

destructor TxdP2PUserManage.Destroy;
var
  i: Integer;
begin
  FCloseing := True;
  if FCheckEvent <> 0  then
  begin
    SetEvent( FCheckEvent );
    while FThreadRunning do
    begin
      Sleep( 10 );
      SetEvent( FCheckEvent );
    end;
    CloseHandle( FCheckEvent );
    FCheckEvent := 0;
  end;
  for i := 0 to FUserList.Count - 1 do
    Dispose( FUserList[i] );
  FUserList.Free;
  DeleteCriticalSection( FLock );
  inherited;
end;

procedure TxdP2PUserManage.DoThreadCheck;
var
  i: Integer;
  p: PClientPeerInfo;
  bDel: Boolean;
  CurTime: Cardinal;
begin
  FThreadRunning := True;
  try
    while not FCloseing do
    begin
      WaitForSingleObject( FCheckEvent, CheckThreadSpaceTime );
      //开始检查P2P连接
      LockList;
      try
        CurTime := GetTickCount;
        for i := FUserList.Count - 1 downto 0 do
        begin
          p := FUserList[i];
          bDel := False;
          if Assigned(OnCheckP2PUser) then
            OnCheckP2PUser( CurTime, p, bDel );
          if bDel then
          begin
            Dispose( p );
            FUserList.Delete( i );
          end;
        end;
      finally
        UnLockList;
      end;
    end;
  finally
    FThreadRunning := False;
  end;
end;

procedure TxdP2PUserManage.DoUpdateUserInfo(ApUser: PClientPeerInfo; ApNet: PUserNetInfo);
var
  bChanged: Boolean;
begin
  bChanged := False;
  if ApUser^.FConnectState <> csConnetSuccess then
  begin
    //更新公网
    if (ApNet^.FPublicIP > 0) and (ApNet^.FPublicPort > 0) and
       ( (ApUser^.FPublicIP <> ApNet^.FPublicIP) or (ApUser^.FPublicPort <> ApNet^.FPublicPort) ) then
    begin
      ApUser^.FPublicIP := ApNet^.FPublicIP;
      ApUser^.FPublicPort := ApNet^.FPublicPort;
      bChanged := True;
    end;
    //更新本地
    if (ApNet^.FLocalIP > 0) and (ApNet^.FLocalPort > 0) and
       ( (ApUser^.FLocalIP <> ApNet^.FLocalIP) or (ApUser^.FLocalPort <> ApNet^.FLocalPort) ) then
    begin
      ApUser^.FLocalIP := ApNet^.FLocalIP;
      ApUser^.FLocalPort := ApNet^.FLocalPort;
      bChanged := True;
    end;
  end;
  if bChanged then
    if Assigned(OnUpdateUserNetInfo) then
      OnUpdateUserNetInfo( ApUser );
end;

function TxdP2PUserManage.FindUserInfo(const AUserID: Cardinal): PClientPeerInfo;
var
  i: Integer;
  p: PClientPeerInfo;
begin
  Result := nil;
  for i := 0 to FUserList.Count - 1 do
  begin
    p := FUserList[i];
    if p^.FUserID = AUserID then
    begin
      Result := p;
      Break;
    end;
  end;
end;

function TxdP2PUserManage.GetCount: Integer;
begin
  Result := FUserList.Count;
end;

function TxdP2PUserManage.GetItem(index: Integer): PClientPeerInfo;
begin
  if (index >= 0) and (index < FUserList.Count) then
    Result := FUserList[index]
  else
    Result := nil;
end;

procedure TxdP2PUserManage.LockList;
begin
  EnterCriticalSection( FLock );
end;

procedure TxdP2PUserManage.SetCheckThreadSpaceTime(const Value: Cardinal);
begin
  FCheckThreadSpaceTime := Value;
end;

procedure TxdP2PUserManage.UnLockList;
begin
  LeaveCriticalSection( FLock );
end;

procedure TxdP2PUserManage.UpdateUserInfo(const ApNet: PUserNetInfo);
var
  i: Integer;
  p: PClientPeerInfo;
begin
  LockList;
  try
    for i := 0 to FUserList.Count - 1 do
    begin
      p := FUserList[i];
      if p^.FUserID = ApNet^.FUserID then
      begin
        DoUpdateUserInfo( p, ApNet );
        Break;
      end;
    end;
  finally
    UnLockList;
  end;
end;

end.
