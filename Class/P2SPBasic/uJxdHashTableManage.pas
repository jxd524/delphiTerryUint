unit uJxdHashTableManage;

interface

uses
  Windows, SysUtils, uJxdUdpDefine, uJxdHashCalc, uJxdDataStruct, uJxdDataStream, uJxdUdpSynchroBasic, uJxdMemoryManage;

type
  PUserListInfo = ^TUserListInfo;
  PHashListInfo = ^THashListInfo;
  PUserTableInfo = ^TUserTableInfo;
  PHashTableInfo = ^THashTableInfo;

  TUserListInfo = record  //SizeOf: 8
    FUserInfo: PUserTableInfo;
    FNext: PUserListInfo;
  end;

  THashListInfo = record  //SizeOf: 8
    FHashInfo: PHashTableInfo;
    FNext: PHashListInfo;
  end;

  //用户表
  TUserTableInfo = record
    FUserID: Cardinal;
    FIP: Cardinal;
    FPort: Word;
    FHashCount: Integer;
    FHashList: PHashListInfo;
  end;

  //文件HASH表
  THashTableInfo = record
    FFileHash: TxdHash;
    FWebHash: TxdHash;
    FUserCount: Integer;
    FUserList: PUserListInfo;
  end;

  TxdHashTableManage = class
  public
    constructor Create;
    destructor  Destroy; override;
    {处理命令}
    //用户更新HASH表
    procedure DoHandleCmd_UpdateFileHashTable(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word);
    //在线服务器通知用户下线
    procedure DoHandleCmd_ClientShutdown(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word);
    //用户搜索HASH
    procedure DoHandleCmd_SearchFileUser(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word);

    //操作
    function  CurShareUserCount: Integer; inline;
    procedure LoopUserList(const ALoopCallBack: TOnLoopNode); 
  private
    {内存管理对象}
    FUserTableMem: TxdFixedMemoryManager;  //用户表内存管理器
    FHashTableMem: TxdFixedMemoryManager;  //HASH表内存管理器
    FLinkNodeMem: TxdFixedMemoryManager; //TUserListInfo THashListInfo 两者使用内存管理器
    {数据操作对象}
    FUserList: THashArray; //用户列表
    FFileHashList: THashArrayEx; //文件HASH列表
    FWebHashList: THashArrayEx;  //WEB HASH 列表
    
    FLock: TRTLCriticalSection;

    FMaxUserTableCount: Integer;
    FMaxHashTableCount: Integer;
    FActive: Boolean;
    FAllowDelHashIP: Cardinal;
    FUDP: TxdUdpSynchroBasic;
    procedure SetMaxHashTableCount(const Value: Integer);
    procedure SetMaxUserTableCount(const Value: Integer);
    procedure SetActive(const Value: Boolean);

    procedure Lock; inline;
    procedure UnLock; inline;
    function  NewUserTable: PUserTableInfo; inline;  //申请新的用户表
    function  NewHashTable: PHashTableInfo; inline;  //申请HASH共享表
    function  NewUserLinkNode: PUserListInfo; inline; //连接结点
    function  NewHashLinkNode: PHashListInfo; inline; //连接结点

    function  FindHashInfo(const AFileHash: Boolean; const AHash: TxdHash; const AIsDelNode: Boolean): PHashTableInfo;
    //User表 与 Hash表之间的关联
    function  RelationUser_Hash(const AHash: PHashTableInfo; const AUser: PUserTableInfo): Boolean; //True: 新增; False: 已经存在
    
    procedure ActiveManage;
    procedure UnActiveManage;
  public
    property Active: Boolean read FActive write SetActive;
    property Udp: TxdUdpSynchroBasic read FUDP write FUDP;
    property AllowDelHashIP: Cardinal read FAllowDelHashIP write FAllowDelHashIP; //此IP用于删除用户共享信息，指定0则所有IP都可以删除
    property MaxUserTableCount: Integer read FMaxUserTableCount write SetMaxUserTableCount;  //最大用户表数量
    property MaxHashTableCount: Integer read FMaxHashTableCount write SetMaxHashTableCount;  //最大共享HASH数量
  end;

implementation

uses
  uJxdFileShareManage;

const
  CtUserTableInfoSize = SizeOf(TUserTableInfo);
  CtHashTableInfoSize = SizeOf(THashTableInfo);
  CtLinkNodeSize = 8;

{ TxdHashTableManage }

procedure TxdHashTableManage.ActiveManage;
var
  nCount: Integer;
begin
  try
    if not Assigned(FUDP) then
    begin
      OutputDebugString( '必须设置UDP' );
      Exit;
    end;
    InitializeCriticalSection( FLock );
    FUserTableMem := TxdFixedMemoryManager.Create( CtUserTableInfoSize, FMaxUserTableCount );
    FHashTableMem := TxdFixedMemoryManager.Create( CtHashTableInfoSize, FMaxHashTableCount );
    if FMaxUserTableCount > FMaxHashTableCount then
      nCount := FMaxUserTableCount
    else
      nCount := FMaxHashTableCount;
    FLinkNodeMem := TxdFixedMemoryManager.Create( CtLinkNodeSize, nCount );

    FUserList := THashArray.Create;
    FUserList.HashTableCount := FMaxUserTableCount div 3;
    FUserList.MaxHashNodeCount := FMaxUserTableCount;
    FUserList.Active := True;

    FFileHashList := THashArrayEx.Create;
    FFileHashList.HashTableCount := FMaxHashTableCount div 3;
    FFileHashList.MaxHashNodeCount := FMaxHashTableCount;
    FFileHashList.Active := True;

    FWebHashList := THashArrayEx.Create;
    FWebHashList.HashTableCount := FMaxHashTableCount div 3;
    FWebHashList.MaxHashNodeCount := FMaxHashTableCount;
    FWebHashList.Active := True;

    FActive := True;
  except
    UnActiveManage;
  end;
end;

function TxdHashTableManage.RelationUser_Hash(const AHash: PHashTableInfo; const AUser: PUserTableInfo): Boolean;
var
  p, pParent: PUserListInfo;
  p1, pHashLink: PHashListInfo;
begin
  Result := True;
  p := AHash^.FUserList;
  pParent := p;
  while Assigned(p) do
  begin
    if Integer(p^.FUserInfo) = Integer(AUser) then
    begin
      Result := False;
      Break;
    end;
    pParent := p;
    p := p^.FNext;
  end;

  if Result then
  begin
    p := NewUserLinkNode;
    p^.FUserInfo := AUser;
    p^.FNext := nil;
    Inc(AHash^.FUserCount);
    if Assigned(pParent) then
      pParent^.FNext := p
    else
      AHash^.FUserList := p;

    pHashLink := NewHashLinkNode;
    pHashLink^.FHashInfo := AHash;
    pHashLink^.FNext := nil;
    Inc( AUser^.FHashCount );
    
    p1 := AUser^.FHashList;
    if Assigned(p1) then
    begin
      while Assigned(p1^.FNext) do
        p1 := p1^.FNext;
      p1^.FNext := pHashLink;
    end
    else
      AUser^.FHashList := pHashLink;
  end;
end;

constructor TxdHashTableManage.Create;
begin
  FMaxUserTableCount := 10000;
  FMaxHashTableCount := 50000;
  FUserTableMem := nil;
  FHashTableMem := nil;
  FLinkNodeMem := nil;
  FActive := False;
  FAllowDelHashIP := 0;
end;

function TxdHashTableManage.CurShareUserCount: Integer;
begin
  if Assigned(FUserList) then
    Result := FUserList.Count
  else
    Result := 0;
end;

destructor TxdHashTableManage.Destroy;
begin
  Active := False;
  inherited;
end;

procedure TxdHashTableManage.DoHandleCmd_ClientShutdown(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdClientShutdownInfo;
  pUser, p1: PUserTableInfo;
  pHashLink, pTemp: PHashListInfo;
  pHash: PHashTableInfo;
  pParentLink, pUserLink: PUserListInfo;
begin
  if (FAllowDelHashIP <> 0) and (AIP <> FAllowDelHashIP) then
  begin
    OutputDebugString( '非指定IP要求删除用户共享HASH，不允许的操作' );
    Exit;
  end;
  if ABufLen <> CtCmdClientShutdownInfoSize then
  begin
    OutputDebugString( 'ClientShutdown命令长度不正确' );
    Exit;
  end;
  pCmd := PCmdClientShutdownInfo(ABuffer);

  Lock;
  try
    if not FUserList.Find(pCmd^.FShutDownID, Pointer(pUser), True) then Exit;

    pHashLink := pUser^.FHashList;
    while Assigned(pHashLink) do
    begin
      pHash := pHashLink^.FHashInfo;

      if pHash^.FUserCount <= 1 then
      begin
        if not HashCompare(pHash^.FFileHash, CtEmptyHash) then
          FindHashInfo(True, pHash^.FFileHash, True);
        if not HashCompare(pHash^.FWebHash, CtEmptyHash) then
          FindHashInfo(False, pHash^.FWebHash, True);
        pUserLink := pHash^.FUserList;
        FLinkNodeMem.FreeMem( pUserLink );
        FHashTableMem.FreeMem( pHash );
      end
      else
      begin
        Dec( pHash^.FUserCount );
        pUserLink := pHash^.FUserList;
        pParentLink := nil;
        while Assigned(pUserLink) do
        begin
          p1 := pUserLink^.FUserInfo;
          if Integer(p1) = Integer(pUser) then
          begin
            if Assigned(pParentLink) then
              pParentLink^.FNext := pUserLink^.FNext
            else
              pHash^.FUserList := pHash^.FUserList^.FNext;
            FLinkNodeMem.FreeMem( pUserLink );
            Break;
          end;
          pParentLink := pUserLink;
          pUserLink := pUserLink^.FNext;
        end;
      end;

      //释放连接结点
      pTemp := pHashLink;
      pHashLink := pTemp^.FNext;
      FLinkNodeMem.FreeMem( pTemp );
    end;

    //释放用户占用内存
    FUserTableMem.FreeMem(pUser);
  finally
    UnLock;
  end;
end;

procedure TxdHashTableManage.DoHandleCmd_SearchFileUser(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdSearchFileUserInfo;
  pHash: PHashTableInfo;
  oSendStream: TxdStaticMemory_16K;
  i: Integer;
  nSendUserCount: Word;
  pLink: PUserListInfo;
  pUser: PUserTableInfo;
  nPos, nSize: Int64;
begin
  if ABufLen <> CtCmdSearchFileUserInfoSize then
  begin
    OutputDebugString( '搜索命令长度不正确' );
    Exit;
  end;
  pCmd := PCmdSearchFileUserInfo( ABuffer );

  oSendStream := TxdStaticMemory_16K.Create;
  Lock;
  try
    if pCmd^.FHashStyle = hsFileHash then
      pHash := FindHashInfo(True, TxdHash(pCmd^.FHash), False)
    else
      pHash := FindHashInfo(False, TxdHash(pCmd^.FHash), False);

    if AIsSynchroCmd then
      FUDP.AddSynchroSign( oSendStream, ASynchroID );
    FUDP.AddCmdHead( oSendStream, CtCmdReply_SearchFileUser );
    oSendStream.WriteByte( Byte(pCmd^.FHashStyle) );
    oSendStream.WriteLong( pCmd^.FHash, CtHashSize );    
    if not Assigned(pHash) then
    begin
      oSendStream.WriteWord( 0 );
      nSize := oSendStream.Position;
    end
    else
    begin
      nPos := oSendStream.Position; 
      oSendStream.Position := oSendStream.Position + 2;
      nSendUserCount := 0;
      pLink := pHash^.FUserList;
      for i := 0 to pHash^.FUserCount - 1 do
      begin        
        if nSendUserCount >= CtMaxSearchUserCount then
          Break;
        pUser := pLink^.FUserInfo;
        if pUser^.FUserID <> pCmd^.FCmdHead.FUserID then
        begin
          oSendStream.WriteCardinal( pUser^.FUserID );
          nSendUserCount := nSendUserCount + 1;
        end;
        pLink := pLink^.FNext;
      end;
      nSize := oSendStream.Position;
      oSendStream.Position := nPos;
      oSendStream.WriteWord( nSendUserCount );
    end;
    FUDP.SendBuffer( AIP, APort, oSendStream.Memory, nSize );
  finally
    UnLock;
    oSendStream.Free;
  end;
end;

procedure TxdHashTableManage.DoHandleCmd_UpdateFileHashTable(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdUpdateFileHashTableInfo;
  Stream: TxdOuterMemory;
  nUserID: Cardinal;
  FileHash, WebHash: TxdHash;
  pUser: PUserTableInfo;
  pHash: PHashTableInfo;
  s: TReplySign;
  oSendStream: TxdStaticMemory_64Byte;
  bFindByFileHash, bFileHashOK, bWebHashOK: Boolean;
label
  llSendCmd;

begin
  s := rsSuccess;
  if ABufLen < CtCmdUpdateFileHashTableInfoSize then
  begin
    s := rsError;
    OutputDebugString( '命令：TCmdUpdateFileHashTableInfo 长度不正确' );
    goto llSendCmd;
  end;
  pCmd := PCmdUpdateFileHashTableInfo(ABuffer);
  nUserID := pCmd^.FCmdHead.FUserID;

  //8
  if pCmd^.FHashCount * CtHashSize * 2 <> ABufLen - 8 then
  begin
    s := rsError;
    OutputDebugString( '命令：TCmdUpdateFileHashTableInfo 中指定HASH数量与内存长度不一致' );
    goto llSendCmd;
  end;

  Lock;
  Stream := TxdOuterMemory.Create;
  try
    if not FUserList.Find(nUserID, Pointer(pUser), False) then
    begin
      pUser := NewUserTable;
      pUser^.FUserID := nUserID;
      pUser^.FIP := AIP;
      pUser^.FPort := APort;
      pUser^.FHashCount := 0;
      pUser^.FHashList := nil;
      FUserList.Add( nUserID, pUser );
    end;

    Stream.InitMemory( ABuffer, ABufLen );
    Stream.Position := 8; //超过包头
    
    while (Stream.Size - Stream.Position) >= CtHashSize * 2 do
    begin
      Stream.ReadLong( FileHash, CtHashSize );
      Stream.ReadLong( WebHash, CtHashSize );
      
      bFindByFileHash := False;
      bFileHashOK := not IsEmptyHash(FileHash);
      bWebHashOK := not IsEmptyHash(WebHash);
      pHash := nil;

      if not bFileHashOK and not bWebHashOK then Continue;      

      if bFileHashOK  then
        pHash := FindHashInfo( True, FileHash, False );

      if Assigned(pHash) then
        bFindByFileHash := True
      else if bWebHashOK then
        pHash := FindHashInfo( False, WebHash, False );    

      if not Assigned(pHash) then
      begin
        //新增
        pHash := NewHashTable;
        pHash^.FFileHash := FileHash;
        pHash^.FWebHash := WebHash;
        pHash^.FUserCount := 0;
        pHash^.FUserList := nil;

        if bFileHashOK then
          FFileHashList.Add( HashToID(pHash^.FFileHash), pHash );
        if bWebHashOK then
          FWebHashList.Add( HashToID(pHash^.FWebHash), pHash );
      end
      else
      begin
        //已经存在的，进行更新, 当由FileHash查找到节点时，只能修改WebHash
        //当从WebHash查找到节点时，除非FileHash为空，否则不能修改FileHash
        if bFindByFileHash then
        begin
          if bWebHashOK and not HashCompare(pHash^.FWebHash, WebHash) then
          begin
            if not IsEmptyHash(pHash^.FWebHash) then            
              FindHashInfo( False, pHash^.FWebHash, True );
            pHash^.FWebHash := WebHash;
            FWebHashList.Add( HashToID(pHash^.FWebHash), pHash );
          end;
        end
        else
        begin
          if bFileHashOK and IsEmptyHash(pHash^.FFileHash) then
          begin
            pHash^.FFileHash := FileHash;
            FFileHashList.Add( HashToID(pHash^.FFileHash), pHash );
          end;
        end;
      end;
      RelationUser_Hash(pHash, pUser);
    end;
  finally
    UnLock;
    Stream.Free;
  end;

llSendCmd:
  oSendStream := TxdStaticMemory_64Byte.Create;
  try
    if AIsSynchroCmd then
      FUDP.AddSynchroSign( oSendStream, ASynchroID );
    FUDP.AddCmdHead( oSendStream, CtCmdReply_UpdateFileHashTable );
    oSendStream.WriteByte( Byte(s) );
    FUDP.SendBuffer( AIP, APort, oSendStream.Memory, oSendStream.Position );
  finally
    oSendStream.Free;
  end;
end;

function TxdHashTableManage.FindHashInfo(const AFileHash: Boolean; const AHash: TxdHash; const AIsDelNode: Boolean): PHashTableInfo;
var
  nID: Cardinal;
  pNode: PHashNode;
  p: PHashTableInfo;
begin
  Result := nil;
  nID := HashToID(AHash);
  if AFileHash then
  begin
    pNode := FFileHashList.FindBegin(nID);
    try
      while Assigned(pNode) do
      begin
        p := pNode^.NodeData;
        if HashCompare(p^.FFileHash, AHash) then
        begin
          Result := p;
          if AIsDelNode then
            FFileHashList.FindDelete( pNode );
          Break;
        end;
        pNode := FFileHashList.FindNext( pNode );
      end;
    finally
      FFileHashList.FindEnd;
    end;
  end
  else
  begin
    pNode := FWebHashList.FindBegin(nID);
    try
      while Assigned(pNode) do
      begin
        p := pNode^.NodeData;
        if HashCompare(p^.FWebHash, AHash) then
        begin
          Result := p;
          if AIsDelNode then
            FWebHashList.FindDelete( pNode );
          Break;
        end;
        pNode := FWebHashList.FindNext( pNode );
      end;
    finally
      FWebHashList.FindEnd;
    end;
  end;
end;

procedure TxdHashTableManage.Lock;
begin
  EnterCriticalSection( FLock );
end;

procedure TxdHashTableManage.LoopUserList(const ALoopCallBack: TOnLoopNode);
begin
  if Assigned(FUserList) then
    FUserList.Loop( ALoopCallBack );
end;

function TxdHashTableManage.NewHashLinkNode: PHashListInfo;
begin
  if not FLinkNodeMem.GetMem(Pointer(Result)) then
    Result := nil;
end;

function TxdHashTableManage.NewHashTable: PHashTableInfo;
begin
  if not FHashTableMem.GetMem(Pointer(Result)) then
    Result := nil;
end;

function TxdHashTableManage.NewUserLinkNode: PUserListInfo;
begin
  if not FLinkNodeMem.GetMem(Pointer(Result)) then
    Result := nil;
end;

function TxdHashTableManage.NewUserTable: PUserTableInfo;
begin
  if not FUserTableMem.GetMem(Pointer(Result)) then
    Result := nil;
end;

procedure TxdHashTableManage.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      ActiveManage
    else
      UnActiveManage;
  end;
end;

procedure TxdHashTableManage.SetMaxHashTableCount(const Value: Integer);
begin
  FMaxHashTableCount := Value;
end;

procedure TxdHashTableManage.SetMaxUserTableCount(const Value: Integer);
begin
  FMaxUserTableCount := Value;
end;

procedure TxdHashTableManage.UnActiveManage;
begin
  FActive := False;
  DeleteCriticalSection( FLock );
  FreeAndNil( FUserTableMem );
  FreeAndNil( FHashTableMem );
  FreeAndNil( FLinkNodeMem );
  FreeAndNil( FUserList );
  FreeAndNil( FFileHashList );
  FreeAndNil( FWebHashList );
end;

procedure TxdHashTableManage.UnLock;
begin
  LeaveCriticalSection( FLock );
end;

end.
