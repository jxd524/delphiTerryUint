{
单元名称: uJxdUdpFileServer
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明: 封装与分析同步包命令
开始时间: 2011-09-19
修改时间: 2011-09-19 (最后修改时间)
类说明  :
   服务器端文件共享管理器
   为请求者提供数据, 数据从 TxdFileShareManage 对象中获取
}
unit uJxdServerUpShareManage;

interface

uses
  Windows, ExtCtrls, uJxdHashCalc, uJxdDataStruct, uJxdDataStream, uJxdUdpSynchroBasic, uJxdFileSegmentStream, 
  uJxdFileShareManage, uJxdUdpDefine, uJxdMemoryManage, uJxdUdpFileServer, uJxdThread;

type
  PUpShareInfo = ^TUpShareInfo;
  TUpShareInfo = record
    FFileStream: TxdP2SPFileStreamBasic;
    FLastActiveTime: Cardinal;
    FRefCount: Integer;
  end;
  
  TxdServerUpShareManage = class
  public
    constructor Create(const AMaxShareCount: Integer = 1000; const AHashTable: Integer = 1313); 
    destructor  Destroy; override;

    procedure LoopShareInfo(ALoop: TOnLoopNode);
  private  
    {对象定义}  
    FShareMemory: TxdFixedMemoryManager;
    FShareList: THashArrayEx;
    FLock: TRTLCriticalSection;   
    FCheckThread: TThreadCheck; 

    {变量定义}
    FLastQueryUpShareInfo: PUpShareInfo;
    FCurQueryWebHash: TxdHash;
    FCurQueryResult: Boolean;
    FCurCheckTime: Cardinal;

    procedure LockManage; inline;
    procedure UnLockManage; inline;

    {删除超时共享项线程}
    procedure DoThreadToCheckUnActiveTime;
    procedure DoThreadLoopToDeleteTimeout(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel: Boolean; var AFindNext: Boolean);
    
    {对ShareList的相关操作}
    procedure DoLoopToFindInfoByWebHash(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel: Boolean; var AFindNext: Boolean);
    procedure DoLoopToDeleteAllItem(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel: Boolean; var AFindNext: Boolean);

    {通知}
    procedure DoMaxShareFile;
    procedure DoErrorInfo(const AInfo: string);

    {查找上传共享信息}
    function  FindUpShareInfo(const AHashStyle: THashStyle; const AHash: TxdHash): PUpShareInfo;
    procedure ReleaseShareInfo(const Ap: PUpShareInfo);
    
    {命令处理上传共享事件}
    procedure DoHandleShareCmdEvent(const ACmd: Word; const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
                                      const AIsSynchroCmd: Boolean; const ASynchroID: Word);
    {处理命令}
    //查询文件信息命令
    procedure DoHandleCmd_QueryFileInfo(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word);
    //查询文件进度（在服务器端，直接返回全部已经完成）
    procedure DoHandleCmd_QueryFileProgress(const AIP: Cardinal; const APort: Word; const ABuffer: PAnsiChar; const ABufLen: Cardinal;
      const AIssynchroCmd: Boolean; const ASynchroID: Word);
    //请求文件数据命令
    procedure DoHandleCmd_RequestFileData(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word);
    //获取文件HASH信息命令
    procedure DoHandleCmd_GetFileSegmentHash(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word);
  private
    FUDP: TxdUdpFileServer;
    FFileShareManage: TxdFileShareManage;
    FUpShareMaxUnActiveTime: Cardinal;
    FUpShareSize: Integer;
    procedure SetUDP(const Value: TxdUdpFileServer);
    function  GetMaxShareCount: Integer;
    procedure SetUpShareMaxUnActiveTime(const Value: Cardinal);
    function  GetCurUpShareCount: Integer;
  public
    {由外部设置对象}
    property Udp: TxdUdpFileServer read FUDP write SetUDP;
    property FileShareManage: TxdFileShareManage read FFileShareManage write FFileShareManage;

    property CurUpShareCount: Integer read GetCurUpShareCount; //当前上传数量
    property CurUpShareSize: Integer read FUpShareSize; //当前已经共享信息大小
    property MaxShareCount: Integer read GetMaxShareCount; //最大共享数量
    property UpShareMaxUnActiveTime: Cardinal read FUpShareMaxUnActiveTime write SetUpShareMaxUnActiveTime; //共享最长保存时间
  end;

implementation

const
  CtUpShareInfoSize = SizeOf(TUpShareInfo);

{ TxdServerFileShareManage }

constructor TxdServerUpShareManage.Create(const AMaxShareCount: Integer; const AHashTable: Integer);
var
  nCount, nHashTableCount: Integer;
begin
  FLastQueryUpShareInfo := nil;
  FUpShareMaxUnActiveTime := 60 * 1000;
  if AMaxShareCount > 0 then
    nCount := AMaxShareCount
  else
    nCount := 1000;
  if AHashTable > 0 then
    nHashTableCount := AHashTable
  else
    nHashTableCount := 1313;
  
  FShareMemory := TxdFixedMemoryManager.Create( CtUpShareInfoSize, nCount );
  FShareList := THashArrayEx.Create;
  with FShareList do
  begin
    HashTableCount := nHashTableCount;
    MaxHashNodeCount := nCount;
    Active := True;
  end;
  
  InitializeCriticalSection( FLock );

  FCheckThread := TThreadCheck.Create( DoThreadToCheckUnActiveTime, FUpShareMaxUnActiveTime );
end;

destructor TxdServerUpShareManage.Destroy;
begin
  LockManage;
  try
    FShareList.Loop( DoLoopToDeleteAllItem );    
  finally
    UnLockManage;
  end;
  FCheckThread.Free;
  FShareMemory.Free;
  DeleteCriticalSection( FLock );
  inherited;
end;

procedure TxdServerUpShareManage.DoErrorInfo(const AInfo: string);
begin
  OutputDebugString( PChar(AInfo) );
end;

procedure TxdServerUpShareManage.DoHandleCmd_GetFileSegmentHash(const AIP: Cardinal; const APort: Word;
  const ABuffer: pAnsiChar; const ABufLen: Cardinal; const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdGetFileSegmentHashInfo;
  p: PFileShareInfo;
  oSendStream: TxdStaticMemory_2K;
  i: Integer;
begin
  if ABufLen <> CtCmdGetFileSegmentHashInfoSize then
  begin
    DoErrorInfo( 'GetFileSegmentHash 命令长度不正确' );
    Exit;
  end;
  pCmd := PCmdGetFileSegmentHashInfo(ABuffer);

  if not Assigned(FFileShareManage) then Exit;

  FFileShareManage.LockShareManage;
  try
    p := FFileShareManage.FindShareInfoByFileHash( TxdHash(pCmd^.FHash) );
    if not Assigned(p) then Exit;
    oSendStream := TxdStaticMemory_2K.Create;
    try
      if AIsSynchroCmd then
        FUdp.AddSynchroSign( oSendStream, ASynchroID );
      FUdp.AddCmdHead( oSendStream, CtCmdReply_GetFileSegmentHash );
      oSendStream.WriteLong( pCmd^.FHash[0], CtHashSize );
      oSendStream.WriteCardinal( p^.FHashCheckSegmentSize );
      for i := 0 to p^.FHashCheckSegmentCount - 1 do
        oSendStream.WriteLong( p^.FHashCheckTable[i].v[0], CtHashSize );
      FUdp.SendBuffer( AIP, APort, oSendStream.Memory, oSendStream.Position );
    finally
      oSendStream.Free;
    end;
  finally
    FFileShareManage.UnLockShareManage;
  end;
  
end;

procedure TxdServerUpShareManage.DoHandleCmd_QueryFileInfo(const AIP: Cardinal; const APort: Word;
  const ABuffer: pAnsiChar; const ABufLen: Cardinal; const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdQueryFileInfo;
  p: PUpShareInfo;
  oSendStream: TxdStaticMemory_512Byte;
  ReplySign: TReplySign;
begin
  if ABufLen <> CtCmdQueryFileInfoSize then
  begin
    DoErrorInfo( '查询文件信息命令长度不正确: CtCmd_QueryFileInfo' );
    Exit;
  end;
  pCmd := PCmdQueryFileInfo(ABuffer);
  p := FindUpShareInfo( pCmd^.FHashStyle, TxdHash(pCmd^.FHash) );
  try
    if Assigned(p) then
      ReplySign := rsSuccess
    else
      ReplySign := rsNotFind;

    oSendStream := TxdStaticMemory_512Byte.Create;
    try
      if AIsSynchroCmd then
        FUdp.AddSynchroSign( oSendStream, ASynchroID );
      FUdp.AddCmdHead( oSendStream, CtCmdReply_QueryFileInfo );
      oSendStream.WriteByte( Byte(pCmd^.FHashStyle) );
      oSendStream.WriteLong( pCmd^.FHash, CtHashSize );
      oSendStream.WriteByte( Byte(ReplySign) );
      if ReplySign = rsSuccess then
      begin
        oSendStream.WriteInt64( p^.FFileStream.FileSize );
        oSendStream.WriteCardinal( p^.FFileStream.SegmentSize );
        if pCmd^.FHashStyle = hsWebHash then
          oSendStream.WriteLong( p^.FFileStream.FileHash, CtHashSize );
      end;
      FUdp.SendBuffer( AIP, APort, oSendStream.Memory, oSendStream.Position );
    finally
      oSendStream.Free;
    end;
  finally
    ReleaseShareInfo( p );
  end;
end;

procedure TxdServerUpShareManage.DoHandleCmd_QueryFileProgress(const AIP: Cardinal; const APort: Word;
  const ABuffer: PAnsiChar; const ABufLen: Cardinal; const AIssynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdQueryFileProgressInfo;
  p: PUpShareInfo;
  oSendStream: TxdStaticMemory_512Byte;
  ReplySign: TReplySign;
begin
  if ABufLen <> CtCmdQueryFileProgressInfoSize then
  begin
    DoErrorInfo( 'QueryFileProgress 命令长度不正确' );
    Exit;
  end;
  pCmd := PCmdQueryFileProgressInfo(ABuffer);
  p := FindUpShareInfo( pCmd^.FHashStyle, TxdHash(pCmd^.FHash) );
  try
    if Assigned(p) then
      ReplySign := rsSuccess
    else
      ReplySign := rsNotFind;

    oSendStream := TxdStaticMemory_512Byte.Create;
    try
      if AIsSynchroCmd then
        FUdp.AddSynchroSign( oSendStream, ASynchroID );
      FUdp.AddCmdHead( oSendStream, CtCmdReply_QueryFileProgress );
      oSendStream.WriteByte( Byte(pCmd^.FHashStyle) );
      oSendStream.WriteLong( pCmd^.FHash, CtHashSize );
      oSendStream.WriteByte( Byte(ReplySign) );
      if ReplySign = rsSuccess then
        oSendStream.WriteByte( Byte(0) ); //表示此文件已经完成
      FUdp.SendBuffer( AIP, APort, oSendStream.Memory, oSendStream.Position );
    finally
      oSendStream.Free;
    end;
  finally
    ReleaseShareInfo( p );
  end;
end;

procedure TxdServerUpShareManage.DoHandleCmd_RequestFileData(const AIP: Cardinal; const APort: Word;
  const ABuffer: pAnsiChar; const ABufLen: Cardinal; const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdRequestFileDataInfo;
  p: PUpShareInfo;
  stream: TxdP2SPFileStreamBasic;
  oSendStream: TxdStaticMemory_16K;
  i: Integer;
  buf: PChar;
  nSize, nSendByteCount: Integer;
begin
  if ABufLen < CtCmdRequestFileDataInfoSize then
  begin
    DoErrorInfo( '请求数据命令长度不正确' );
    Exit;
  end;
  pCmd := PCmdRequestFileDataInfo(ABuffer);
  oSendStream := TxdStaticMemory_16K.Create;
  p := FindUpShareInfo( pCmd^.FHashStyle, TxdHash(pCmd^.FHash) );
  try
    if not Assigned(p) then Exit;
    stream := p^.FFileStream;
    if Assigned(stream) then
    begin
      //查找成功
      GetMem( buf, stream.SegmentSize );
      try
        for i := 0 to pCmd^.FRequestCount - 1 do
        begin
          if stream.ReadBlockBuffer(pCmd^.FRequestInfo[i].FSegmentIndex, pCmd^.FRequestInfo[i].FBlockIndex, PByte(buf), nSize) then
          begin
            oSendStream.Clear;
            FUdp.AddCmdHead( oSendStream, CtCmdReply_RequestFileData );
            oSendStream.WriteByte(Byte(pCmd^.FHashStyle));
            oSendStream.WriteLong(pCmd^.FHash, 16);
            oSendStream.WriteByte(Byte(rsSuccess));
            oSendStream.WriteInteger(pCmd^.FRequestInfo[i].FSegmentIndex);
            oSendStream.WriteWord(pCmd^.FRequestInfo[i].FBlockIndex);
            oSendStream.WriteWord(nSize);
            oSendStream.WriteLong(buf^, nSize);

            nSendByteCount := FUdp.SendBuffer(AIP, APort, oSendStream.Memory, oSendStream.Position);
            if nSendByteCount = -1 then Continue;  //发送失败，直接发另一个请求包，如果是系统缓存不足，程序会自动重发
            InterlockedExchangeAdd( FUpShareSize, nSendByteCount ); 
          end
          else
          begin
            DoErrorInfo( '无法读取流信息: ' );
          end;
        end;
      finally
        FreeMem( buf );
      end;
    end
    else
    begin
      //查找不到指定文件
      FUdp.AddCmdHead( oSendStream, CtCmdReply_RequestFileData );
      oSendStream.WriteLong(pCmd^.FHash, 16);
      oSendStream.WriteByte(Byte(rsNotFind));
      FUdp.SendBuffer( AIP, APort, oSendStream.Memory, oSendStream.Position );
    end;
  finally
    oSendStream.Free;
    ReleaseShareInfo( p );
  end;
end;

procedure TxdServerUpShareManage.DoHandleShareCmdEvent(const ACmd: Word; const AIP: Cardinal; const APort: Word;
  const ABuffer: pAnsiChar; const ABufLen: Cardinal; const AIsSynchroCmd: Boolean; const ASynchroID: Word);
begin
  case ACmd of
    CtCmd_RequestFileData:     DoHandleCmd_RequestFileData( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_QueryFileInfo:       DoHandleCmd_QueryFileInfo( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_QueryFileProgress:   DoHandleCmd_QueryFileProgress( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_GetFileSegmentHash:  DoHandleCmd_GetFileSegmentHash( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
  end;
end;

procedure TxdServerUpShareManage.DoLoopToDeleteAllItem(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel,
  AFindNext: Boolean);
var
  p: PUpShareInfo;
begin
  p := pData;
  StreamManage.ReleaseFileStream( p^.FFileStream );
  Dispose( p );
  ADel := True;
end;

procedure TxdServerUpShareManage.DoLoopToFindInfoByWebHash(Sender: TObject; const AID: Cardinal; pData: Pointer;
  var ADel, AFindNext: Boolean);
var
  p: PUpShareInfo;
begin
  p := pData;
  AFindNext := not HashCompare( p^.FFileStream.WebHash, FCurQueryWebHash );
  if not AFindNext then //查找成功  
    FLastQueryUpShareInfo := p;
end;


procedure TxdServerUpShareManage.DoMaxShareFile;
begin
  OutputDebugString( '共享数量已经达到最大值，无法再添加数据提供' );
end;

procedure TxdServerUpShareManage.DoThreadLoopToDeleteTimeout(Sender: TObject; const AID: Cardinal; pData: Pointer;
  var ADel, AFindNext: Boolean);
var
  p: PUpShareInfo;
begin
  p := pData;
  if (p^.FRefCount <= 0) and (p^.FLastActiveTime + FUpShareMaxUnActiveTime >= FCurCheckTime) then
  begin
    ADel := True;
    if p = FLastQueryUpShareInfo then
      FLastQueryUpShareInfo := nil;
    StreamManage.ReleaseFileStream( p^.FFileStream );
  end;
end;


procedure TxdServerUpShareManage.DoThreadToCheckUnActiveTime;
begin
  LockManage;
  try
    FCurCheckTime := GetTickCount;
    FShareList.Loop( DoThreadLoopToDeleteTimeout );
  finally
    UnLockManage;
  end;
end;

function TxdServerUpShareManage.FindUpShareInfo(const AHashStyle: THashStyle; const AHash: TxdHash): PUpShareInfo;
var
  p: PUpShareInfo;
  pNode: PHashNode;
  bFind: Boolean;
  pShare: PFileShareInfo;
begin
  bFind := False;
  Result := nil;
  LockManage;
  try
    //判断最后一次查询是否是同一个内容
    if Assigned(FLastQueryUpShareInfo) then
    begin
      if AHashStyle = hsFileHash then
        bFind := HashCompare(AHash, FLastQueryUpShareInfo^.FFileStream.FileHash )
      else
        bFind := HashCompare(AHash, FLastQueryUpShareInfo^.FFileStream.WebHash );
    end;

    if not bFind then
    begin
      //在本类列表中进行HASH查询，查询不到信息才向文件共享类进行查询
      
      //使用FileHash进行查询
      if AHashStyle = hsFileHash then
      begin
        pNode := FShareList.FindBegin( HashToID(AHash) );
        try
          while Assigned(pNode) do
          begin
            p := pNode^.NodeData;
            if HashCompare(p^.FFileStream.FileHash, AHash) then
            begin
              bFind := True;
              FLastQueryUpShareInfo := p;
              Break;
            end;
            pNode := FShareList.FindNext( pNode );
          end;
        finally
          FShareList.FindEnd;
        end;
      end
      else
      begin
        //使用WebHash进行查询
        FCurQueryWebHash := AHash;
        FCurQueryResult := False;
        FShareList.Loop( DoLoopToFindInfoByWebHash );
        bFind := FCurQueryResult;
      end;
      //结束本类查找

      //向文件共享类进行查询
      if not bFind and Assigned(FFileShareManage) then
      begin        
        FFileShareManage.LockShareManage;
        try
          if AHashStyle = hsFileHash then          
            pShare := FFileShareManage.FindShareInfoByFileHash( AHash )
          else
            pShare := FFileShareManage.FindShareInfoByWebHash( AHash );

          if Assigned(pShare) then
          begin
            New( p );
            p^.FFileStream := StreamManage.CreateFileStream( pShare^.FFileName, pShare^.FFileHash, pShare^.FSegmentSize );
            if not Assigned(p^.FFileStream) then
            begin
              Dispose( p );
              Exit;
            end;
            p^.FFileStream.WebHash := pShare^.FWebHash;
            p^.FRefCount := 0;
            
            if not FShareList.Add( HashToID(p^.FFileStream.FileHash), p ) then
            begin
              DoMaxShareFile;
              StreamManage.ReleaseFileStream( p^.FFileStream );
              Dispose( p );
              Exit;
            end;

            FLastQueryUpShareInfo := p;
            bFind := True;
          end;
        finally
          FFileShareManage.UnLockShareManage;
        end;              
      end;
      //结束文件共享类查找      
    end;


    if bFind then
    begin
      Result := FLastQueryUpShareInfo;
      Result^.FLastActiveTime := GetTickCount;
      Inc( Result^.FRefCount );
    end;
  finally
    UnLockManage;
  end;
end;

function TxdServerUpShareManage.GetCurUpShareCount: Integer;
begin
  if Assigned(FShareList) then
    Result := FShareList.Count
  else
    Result := 0;
end;

function TxdServerUpShareManage.GetMaxShareCount: Integer;
begin
  if Assigned(FShareMemory) then
    Result := FShareMemory.Capacity
  else
    Result := 0;
end;

procedure TxdServerUpShareManage.LockManage;
begin
  EnterCriticalSection( FLock );
end;

procedure TxdServerUpShareManage.LoopShareInfo(ALoop: TOnLoopNode);
begin
  LockManage;
  try
    FShareList.Loop( ALoop );
  finally
    UnLockManage;
  end;
end;

procedure TxdServerUpShareManage.ReleaseShareInfo(const Ap: PUpShareInfo);
begin
  if Assigned(Ap) then
  begin
    LockManage;
    try
      Dec( Ap^.FRefCount );
      Ap^.FLastActiveTime := GetTickCount;
    finally
      UnLockManage;
    end;
  end;
end;

procedure TxdServerUpShareManage.SetUDP(const Value: TxdUdpFileServer);
begin
  FUDP := Value;
  if Assigned(FUDP) then
    FUDP.OnFileShareCmdEvent := DoHandleShareCmdEvent;
end;

procedure TxdServerUpShareManage.SetUpShareMaxUnActiveTime(const Value: Cardinal);
begin
  if FUpShareMaxUnActiveTime <> Value then
  begin
    FUpShareMaxUnActiveTime := Value;
    if FUpShareMaxUnActiveTime > 15 * 60 * 1000 then
    begin
      if Assigned(FCheckThread) then
        FCheckThread.SpaceTime := FUpShareMaxUnActiveTime div 2
    end
    else
    begin
      if Assigned(FCheckThread) then
        FCheckThread.SpaceTime := FUpShareMaxUnActiveTime div 3;
    end;
  end;
end;

procedure TxdServerUpShareManage.UnLockManage;
begin
  LeaveCriticalSection( FLock );
end;

end.
