unit uJxdOnlineUserManage;

interface

uses
  Windows, Classes, SysUtils, uJxdDataStruct, uJxdMemoryManage, uJxdThread, uJxdDataStream,
  uJxdCmdDefine;

type
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ///                         TServerHashList
  ///
  ///  非线程安全
  ///
  ///  服务器在线管理使用的数据结构
  ///
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TServerHashList = class(THashArray)
  public
    //随机获取N个在线用户的信息, 返回取得用户数量
    function GetRandomUserInfo(const AExceptionID: Cardinal; AStream: TxdMemoryHandle): Integer;
  end;

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ///                         TRegisterManage
  ///
  ///  线程安全
  ///  用户需要的ID从1000开始，1000以内的保留
  ///  提供注册服务，直接调用 GetNewUserID 可获取一个新的用户ID
  ///
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  {$M+}
  TRegisterManage = class
  public
    constructor Create(const AMinUserID: Cardinal = CtMinUserID);
    destructor  Destroy; override;
    function GetNewUserID: Cardinal; inline;
  private
    FUserID: Cardinal;
    FLock: TRTLCriticalSection;
  published
    property UserID: Cardinal read FUserID;
  end;
  {$M-}
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ///                         TOnlineUserManage
  ///
  ///  线程安全
  ///  在线用户管理类
  ///  提供注册服务，直接调用 GetNewUserID 可获取一个新的用户ID
  ///
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  POnlineUserInfo = ^TOnlineUserInfo;
  TOnlineUserInfo = record
    FUserID: Cardinal;                 //用户ID
    FPublicIP, FLocalIP: Cardinal;   //用户IP信息
    FPublicPort, FLocalPort: Word;   //用户端口信息
    FClientVersion: Word;              //客户端版本号
    FTimeoutCount: Word;               //用户超时次数
    FLoginTime: Cardinal;              //登录时间
    FLastActiveTime: Cardinal;         //最后活动时间, 接收到数据的最后时间
    FSaftSign: Cardinal;               //安全码 
    FClientHash: array[0..15] of Byte; //客户端HASH
    FData: Pointer;                    //其它信息，由处部指定，指向内存为外部申请，释放
  end;

  TAddUserResult = (auSuccess, auIDExists, auNoEnoughMem); //添加用户时返回值
  TDeleteUserResult = (duSuccess, duNotFindUser, duErrorSafeSign);//删除用户时返回值
  TOptResult = (orSuccess, orNotExistsID, orNotFindUser, orError); //操作返回结果
  TOnUser = procedure(const ApUserInfo: POnlineUserInfo) of object; 
  {$M+}
  TOnlineUserManage = class
  public
    constructor Create;
    destructor  Destroy; override;

    function  AddOnlineUser(const AUserID, APublicIP, ALocalIP: Cardinal; const APublicPort, ALocalPort, AClientVersion: Word;
      const AClientHash: array of Byte; var ASaftSize: Cardinal): TAddUserResult; //添加在线用户
    function  ActiveOnlineUser(const AUserID: Cardinal): Boolean; //修改用户最后活动时间
    function  DeleteOnlineUser(const AUserID, ASaftSign: Cardinal): TDeleteUserResult; //删除在线用户
    function  GetRandomUserInfo(const AExceptionID: Cardinal; AStream: TxdMemoryHandle): Integer; //随机获取在线用户
    function  GetOnlineUserInfo(var AUserAInfo, AUserBInfo: TUserNetInfo): TOptResult; //获取指定用户信息，并且更新A用户的时间
  private
    FCheckEvent: Cardinal;
    FCheckTime: Cardinal;
    FCheckThreadCount: Integer;
    procedure ActiveManage;
    procedure UnActiveManage;
    procedure LockOnline; inline;
    procedure UnLockOnline; inline;
    {线程执行函数}
    procedure DoThreadToCheck;
    procedure DoThreadCheckOnlinActiveTime(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel: Boolean; var AFindNext: Boolean);
  private
    FOnlineList: TServerHashList;
    FOnlineMem: TxdFixedMemoryManager;
    FOnlineLock: TRTLCriticalSection;
    FMaxOnlineCount: Integer;
    FActive: Boolean;
    FOnAddUser: TOnUser;
    FOnDeleteUser: TOnUser;
    FMaxIdleTime: Cardinal;
    FCheckSpaceTime: Cardinal;
    FTimeoutUserCount: Integer;
    procedure SetMaxOnlineCount(const Value: Integer);
    procedure SetActive(const Value: Boolean);
    function GetCount: Integer;
    procedure SetMaxIdleTime(const Value: Cardinal);
    procedure SetCheckSpaceTime(const Value: Cardinal);
  published
    property Active: Boolean read FActive write SetActive;
    property MaxOnlineCount: Integer read FMaxOnlineCount write SetMaxOnlineCount; //最大允许在线人数
    property CheckSpaceTime: Cardinal read FCheckSpaceTime write SetCheckSpaceTime; //检查间隔
    property MaxIdleTime: Cardinal read FMaxIdleTime write SetMaxIdleTime; //最大空闲时间，超过此值，将自动删除用户

    property TimeoutUserCount: Integer read FTimeoutUserCount; //超时用户数量
    property Count: Integer read GetCount; //当前在线人数

    property OnAddUser: TOnUser read FOnAddUser write FOnAddUser; //当添加一个新用户时，以锁定状态被调用
    property OnDeleteUser: TOnUser read FOnDeleteUser write FOnDeleteUser; //当删除一个用户时，以锁定状态被调用
  end;
  {$M-}
implementation

const
  CtOnlineUserInfoSize = SizeOf(TOnlineUserInfo);

{ TRegisterManage }

constructor TRegisterManage.Create(const AMinUserID: Cardinal);
begin
  InitializeCriticalSection( FLock );
  FUserID := AMinUserID;
end;

destructor TRegisterManage.Destroy;
begin
  DeleteCriticalSection( FLock );
  inherited;
end;

function TRegisterManage.GetNewUserID: Cardinal;
begin
  EnterCriticalSection( FLock );
  try
    FUserID := FUserID + 1;
    Result := FUserID;
  finally
    LeaveCriticalSection( FLock );
  end;
end;

{ TOnlineUserManage }

procedure TOnlineUserManage.ActiveManage;
begin
  try
    InitializeCriticalSection( FOnlineLock );
    FOnlineList := TServerHashList.Create;
    with FOnlineList do
    begin
      HashTableCount := 131313;
      MaxHashNodeCount := FMaxOnlineCount;
      Active := True;
    end;
    FOnlineMem := TxdFixedMemoryManager.Create( CtOnlineUserInfoSize, MaxOnlineCount );
    FCheckEvent := CreateEvent( nil, False, False, nil );
    FActive := True;
    FCheckThreadCount := 0;
    FTimeoutUserCount := 0;
    RunningByThread( DoThreadToCheck );
  except
    UnActiveManage;
  end;
end;

function TOnlineUserManage.ActiveOnlineUser(const AUserID: Cardinal): Boolean;
var
  p: POnlineUserInfo;
  bExists: Boolean;
begin
  LockOnline;
  try
    bExists := FOnlineList.Find( AUserID, Pointer(p), False );
    if bExists then
    begin
      Result := True;
      p^.FLastActiveTime := GetTickCount;
      p^.FTimeoutCount := 0;
    end
    else
      Result := False;
  finally
    UnLockOnline;
  end;
end;

function TOnlineUserManage.AddOnlineUser(const AUserID, APublicIP, ALocalIP: Cardinal; const APublicPort,
  ALocalPort, AClientVersion: Word; const AClientHash: array of Byte; var ASaftSize: Cardinal): TAddUserResult;
var
  p: POnlineUserInfo;
  bExists: Boolean;
begin
  LockOnline;
  try
    bExists := FOnlineList.Find( AUserID, Pointer(p), False );
    if bExists then
    begin
      //当重复登录（可能因为网络原因）,如果信息相同，则直接更新活动时间
      if (p^.FPublicIP = APublicIP) and (p^.FLocalIP = ALocalIP) and
         (p^.FPublicPort = APublicPort) and (p^.FLocalPort = APublicPort) and
         (AClientVersion = p^.FClientVersion) and CompareMem( @AClientHash, @p^.FClientHash, 16) then
      begin
        p^.FLastActiveTime := GetTickCount;
        ASaftSize := p^.FSaftSign;
        Result := auSuccess;
        Exit;
      end;
      
      Result := auIDExists;
      Exit;
    end;
    if not FOnlineMem.GetMem(Pointer(p)) then
    begin
      Result := auNoEnoughMem;
      Exit;
    end;
    p^.FUserID := AUserID;
    p^.FPublicIP := APublicIP;
    p^.FPublicPort := APublicPort;
    p^.FLocalIP := ALocalIP;
    p^.FLocalPort := ALocalPort;
    p^.FClientVersion := AClientVersion;
    p^.FSaftSign := Random( MaxLongint ) * Round( Random * 131313131 );
    Move( AClientHash[0], p^.FClientHash[0], 16 );
    p^.FData := nil;
    
    if Assigned(OnAddUser) then
      OnAddUser( p );

    p^.FLoginTime := GetTickCount;
    p^.FLastActiveTime := p^.FLoginTime;
    p^.FTimeoutCount := 0;
    FOnlineList.Add( AUserID, p );
    Result := auSuccess;
    ASaftSize := p^.FSaftSign;
  finally
    UnLockOnline;
  end;
end;

constructor TOnlineUserManage.Create;
begin
  FActive := False;
  FMaxOnlineCount := 500000;
  FMaxIdleTime := 60 * 1000;
  FCheckSpaceTime := 70 * 1000;
end;

function TOnlineUserManage.DeleteOnlineUser(const AUserID, ASaftSign: Cardinal): TDeleteUserResult;
var
  p: POnlineUserInfo;
  bExists: Boolean;
begin
  Result := duNotFindUser;
  LockOnline;
  try
    bExists := FOnlineList.Find( AUserID, Pointer(p), False );
    if bExists then
    begin
      if p^.FSaftSign = ASaftSign then
      begin
        FOnlineList.Find( AUserID, Pointer(p), True );
        if Assigned(OnDeleteUser) then
          OnDeleteUser( p );
        FOnlineMem.FreeMem( p );
        Result := duSuccess;
      end
      else
        Result := duErrorSafeSign;
    end;
  finally
    UnLockOnline;
  end;
end;

destructor TOnlineUserManage.Destroy;
begin
  Active := False;
  inherited;
end;

procedure TOnlineUserManage.DoThreadCheckOnlinActiveTime(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel, AFindNext: Boolean);
var
  p: POnlineUserInfo;
begin
  if not Active then
  begin
    AFindNext := False;
    Exit;
  end;
  p := pData;
  if (FCheckTime > p^.FLastActiveTime) and (FCheckTime - p^.FLastActiveTime > FMaxIdleTime) then
  begin
    ADel := True;
    if Assigned(OnDeleteUser) then
      OnDeleteUser( p );
    FOnlineMem.FreeMem( p );
    InterlockedIncrement( FTimeoutUserCount );
  end;
end;

procedure TOnlineUserManage.DoThreadToCheck;
begin
  InterlockedIncrement( FCheckThreadCount );
  try
    while Active do
    begin
      WaitForSingleObject( FCheckEvent, FCheckSpaceTime );
      if not Active then Break;
      if FOnlineList.Count > 0 then
      begin
        LockOnline;
        try
          FCheckTime := GetTickCount;
          FOnlineList.Loop( DoThreadCheckOnlinActiveTime );
        finally
          UnLockOnline;
        end;
      end;
    end;
  finally
    InterlockedDecrement( FCheckThreadCount );
  end;
end;

function TOnlineUserManage.GetRandomUserInfo(const AExceptionID: Cardinal; AStream: TxdMemoryHandle): Integer;
var
  p: POnlineUserInfo;
  bExists: Boolean;
begin
  Result := 0;
  LockOnline;
  try
    bExists := FOnlineList.Find( AExceptionID, Pointer(p), False );
    if bExists then
    begin
      Result := FOnlineList.GetRandomUserInfo( AExceptionID, AStream );
      p^.FLastActiveTime := GetTickCount;
      p^.FTimeoutCount := 0;
    end
  finally
    UnLockOnline;
  end;
end;

function TOnlineUserManage.GetCount: Integer;
begin
  if Active then
    Result := FOnlineList.Count
  else
    Result := 0;
end;

function TOnlineUserManage.GetOnlineUserInfo(var AUserAInfo, AUserBInfo: TUserNetInfo): TOptResult;
var
  p1, p2: POnlineUserInfo;
begin
  LockOnline;
  try
    if FOnlineList.Find(AUserAInfo.FUserID, Pointer(p1), False) then
    begin
      if FOnlineList.Find(AUserBInfo.FUserID, Pointer(p2), False) then
      begin
        AUserAInfo.FPublicIP := p1^.FPublicIP;
        AUserAInfo.FLocalIP := p1^.FLocalIP;
        AUserAInfo.FPublicPort := p1^.FPublicPort;
        AUserAInfo.FLocalPort := p1^.FLocalPort;

        AUserBInfo.FPublicIP := p2^.FPublicIP;
        AUserBInfo.FLocalIP := p2^.FLocalIP;
        AUserBInfo.FPublicPort := p2^.FPublicPort;
        AUserBInfo.FLocalPort := p2^.FLocalPort;
        Result := orSuccess;
      end
      else
        Result := orNotFindUser;
      p1^.FLastActiveTime := GetTickCount;
    end
    else
      Result := orNotExistsID;
  finally
    UnLockOnline;
  end;
end;

procedure TOnlineUserManage.LockOnline;
begin
  EnterCriticalSection( FOnlineLock );
end;

procedure TOnlineUserManage.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      ActiveManage
    else
      UnActiveManage;
  end;
end;

procedure TOnlineUserManage.SetCheckSpaceTime(const Value: Cardinal);
begin
  FCheckSpaceTime := Value;
end;

procedure TOnlineUserManage.SetMaxIdleTime(const Value: Cardinal);
begin
  FMaxIdleTime := Value;
end;

procedure TOnlineUserManage.SetMaxOnlineCount(const Value: Integer);
begin
  if not Active and (FMaxOnlineCount <> Value) and (Value > 0) then
    FMaxOnlineCount := Value;
end;

procedure TOnlineUserManage.UnActiveManage;
begin
  FActive := False;
  SetEvent( FCheckEvent );
  while FCheckThreadCount > 0 do
  begin
    Sleep( 10 );
    SetEvent( FCheckEvent );
  end;
  CloseHandle( FCheckEvent );
  FreeAndNil( FOnlineList );
  FreeAndNil( FOnlineMem );
  DeleteCriticalSection( FOnlineLock );
end;

procedure TOnlineUserManage.UnLockOnline;
begin
  LeaveCriticalSection( FOnlineLock );
end;

{ TServerHashList }

function TServerHashList.GetRandomUserInfo(const AExceptionID: Cardinal; AStream: TxdMemoryHandle): Integer;
var
  aTempV: array[0..CtMaxSearchRandomUserCount - 1] of Integer; //随机生成要定位的位置
  i, nMaxCount, nFirstIndex, nCurNodeCount, nIndex, nTempValue, nWriteToStreamCount: Integer;
  pNode: pHashNode;
  p: POnlineUserInfo;
  bDown: Boolean;

  //
  function IsOkRow(const AValue: Integer): Boolean;
  var
    k: Integer;
  begin
    Result := True;
    for k := 0 to nIndex - 1 do
    begin
      if AValue = aTempV[k] then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

  //判断当前找到的用户是否写入流中
  function IsOkUser: Boolean;
  var
    k: Integer;
  begin
    Result := False;
    for k := 0 to nMaxCount - 1 do
    begin
      if nIndex = aTempV[k] then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

  //将用户信息写入流中
  procedure WriteInfoToStream(Ap: POnlineUserInfo);
  begin
    with AStream do
    begin
      WriteCardinal( Ap^.FUserID );
      WriteCardinal( Ap^.FPublicIP );
      WriteCardinal( Ap^.FLocalIP );
      WriteWord( ap^.FPublicPort );
      WriteWord( Ap^.FLocalPort );
    end;
    Inc( nWriteToStreamCount );
    Inc( Result );
  end;

  //判断是否要写入流中
  procedure CheckToWriteStream;
  begin
    pNode := FHashTable[i];
    while Assigned(pNode) do
    begin
      if IsOkUser then
      begin
        p := pNode^.NodeData;
        if p^.FUserID <> AExceptionID then
          WriteInfoToStream( p );
      end;
      Inc( nIndex );
      pNode := pNode^.Next;
    end;
  end;


begin
  Result := 0;
  //随机设置要返回的数量
  nMaxCount := Random( CtMaxSearchRandomUserCount );
  if nMaxCount < CtMinSearchRandomUserCount then
    nMaxCount := CtMinSearchRandomUserCount;

  nCurNodeCount := Count;

  if nMaxCount >= nCurNodeCount + 1 then
  begin
    //当在线量比要求的量小时, 将返回此时所有在线用户(除去AExceptionID)
    nMaxCount := nCurNodeCount;
    //begin for
    for i := FFirstNodeIndex to HashTableCount - 1 do
    begin
      if nMaxCount = 0 then Break;
      pNode := FHashTable[i];
      while  Assigned(pNode) do
      begin
        p := pNode^.NodeData;
        if p^.FUserID <> AExceptionID then
          WriteInfoToStream( p );
        Dec(nMaxCount);
        pNode := pNode^.Next;
      end;
    end;
    //end for
  end
  else
  begin
    //在线多于请求数量时, 随机定位J，随机向上U或向下D搜索，随机取N个值v1,v2,v3 其中 v: [0..Count)
    //确保V无重复, 遍历一次i，当 i = v 时，并且 v的ID <> AExceptionID 时，此ID可写入流中。
    nIndex := 0;
    for i := 0 to nMaxCount - 1 do
    begin
      nTempValue := Random( nCurNodeCount );
      while not IsOkRow(nTempValue) do
        nTempValue := Random( nCurNodeCount );
      aTempV[i] := nTempValue;
      Inc( nIndex );
    end;

    nFirstIndex := Random(HashTableCount);
    if nFirstIndex < FFirstNodeIndex then
      nFirstIndex := FFirstNodeIndex;
    nWriteToStreamCount := 0;
    nIndex := 0;
    bDown := Random(100) > 50;
    //定位到需要的行
    if bDown then
    begin
      for i := nFirstIndex to HashTableCount - 1 do
        CheckToWriteStream;
      if nWriteToStreamCount < nMaxCount then
      begin
        for i := nFirstIndex - 1 downto 0 do
          CheckToWriteStream;
      end;
    end
    else
    begin
      for i := nFirstIndex downto 0 do
        CheckToWriteStream;
      if nWriteToStreamCount < nMaxCount then
      begin
        for i := nFirstIndex + 1 to HashTableCount - 1 do
          CheckToWriteStream;
      end;
    end;
  end;
end;

initialization
  Randomize;

end.
