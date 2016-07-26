{
单元名称: uJxdClientUpShareManage
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明: 负责执行P2P下载，任务释放缓存处理，任务信息的保存与载入
开始时间: 2011-09-14
修改时间: 2011-09-14 (最后修改时间)
类说明  :
    管理文件上传，上传信息超过一定时间无动作，则删除。上传数量可被限制
    查找顺序：自身 -> StreamManage -> FileShareManage
    非自身查找到的数据转成自身所需要的结构体
}
unit uJxdClientUpShareManage;

interface

uses
  Windows, SysUtils, Classes, uJxdUdpDefine, uJxdHashCalc, uJxdDataStream, uJxdUdpSynchroBasic, 
  uJxdThread, uJxdServerManage, uJxdUdpCommonClient, uJxdFileSegmentStream, uJxdFileShareManage;

type
  PUpShareInfo = ^TUpShareInfo;
  TUpShareInfo = record
    FFileStream: TxdP2SPFileStreamBasic;
    FLastActiveTime: Cardinal;
    FRefCount: Integer;
    FUpSize: Integer;
  end;
  
  TxdClientUpShareManage = class
  public
    constructor Create;
    destructor  Destroy; override; 

    function  GetUpShareInfo(const AIndex: Integer): PUpShareInfo;
    procedure ReleaseUpShareInfo(const Ap: PUpShareInfo);
  private
    FLock: TRTLCriticalSection;
    FTaskList: TList;
    FAutoDeleteThread: TThreadCheck;
    FUpShareSize: Integer;

    procedure LockManage; inline;
    procedure UnlockManage; inline;

    {释放共享信息}
    procedure FreeUpShareInfo(const Ap: PUpShareInfo); inline;

    {查找上传共享信息，FindUpShareInfo与ReleaseUpShareInfo 需要配合使用}
    function  FindUpShareInfo(const AHashStyle: THashStyle; const AHash: TxdHash): PUpShareInfo;
    

    {错误显示信息}
    procedure DoErrorInfo(const AInfo: string);

    {线程自动删除超时无响应上传信息}
    procedure DoThreadToDeleteTimeoutUpShare;        
  private
    {命令处理上传共享事件}
    procedure DoHandleUpShareCmdEvent(const ACmd: Word; const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
                                      const AIsSynchroCmd: Boolean; const ASynchroID: Word);
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
    FUDP: TxdUdpCommonClient;
    FMaxUpTaskCount: Integer;
    FFileShareManage: TxdFileShareManage;
    FMaxUnActiveTime: Cardinal;
    procedure SetUDP(const Value: TxdUdpCommonClient);
    procedure SetMaxUpTaskCount(const Value: Integer);
    procedure SetMaxUnActiveTime(const Value: Cardinal);
    function  GetCurUpShareCount: Integer; inline;
  public
    {外部对象定义}
    property UDP: TxdUdpCommonClient read FUDP write SetUDP;
    property FileShareManage: TxdFileShareManage read FFileShareManage write FFileShareManage; 

    {限制}
    property MaxUpTaskCount: Integer read FMaxUpTaskCount write SetMaxUpTaskCount; //最大同时上传任务数量
    property MaxUnActiveTime: Cardinal read FMaxUnActiveTime write SetMaxUnActiveTime; //最大无动作时间

    {只读属性}
    property CurUpShareCount: Integer read GetCurUpShareCount; //当前共享数量
    property CurUpShareSize: Integer read FUpShareSize; //上传数量
  end;

implementation

const
  CtMinUpTaskCount = 3;

{ TxdClientUpShareManage }

constructor TxdClientUpShareManage.Create;
begin
  FMaxUpTaskCount := 10;
  FMaxUnActiveTime := 60 * 1000;
  FAutoDeleteThread := nil;
  FUpShareSize := 0;
  FTaskList := TList.Create;  
  InitializeCriticalSection( FLock );
end;

function TxdClientUpShareManage.GetCurUpShareCount: Integer;
begin
  Result := FTaskList.Count;
end;

function TxdClientUpShareManage.GetUpShareInfo(const AIndex: Integer): PUpShareInfo;
begin
  Result := nil;
  LockManage;
  try
    if (AIndex < 0) or (AIndex >= FTaskList.Count) then Exit;
    Result := FTaskList[AIndex];
    Inc( Result^.FRefCount );
  finally
    UnlockManage;
  end;
end;

destructor TxdClientUpShareManage.Destroy;
var
  i: Integer;
begin  
  LockManage;
  try
    for i := 0 to FTaskList.Count - 1 do
      FreeUpShareInfo( FTaskList[i] );
    FTaskList.Clear;
  finally
    UnlockManage;
  end;
  //FTaskList.Count = 0 时，将自动释放 FAutoDeleteThread
  while Assigned(FAutoDeleteThread) do
  begin
    FAutoDeleteThread.ActiveToCheck;
    Sleep( 10 );
  end;
  
  FTaskList.Free;
  DeleteCriticalSection( FLock );
  inherited;
end;

procedure TxdClientUpShareManage.DoErrorInfo(const AInfo: string);
begin
  OutputDebugString( PChar(AInfo) );
end;

procedure TxdClientUpShareManage.DoHandleCmd_GetFileSegmentHash(const AIP: Cardinal; const APort: Word;
  const ABuffer: pAnsiChar; const ABufLen: Cardinal; const AIsSynchroCmd: Boolean; const ASynchroID: Word);
begin

end;

procedure TxdClientUpShareManage.DoHandleCmd_QueryFileInfo(const AIP: Cardinal; const APort: Word;
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
    begin
      if p^.FFileStream is TxdLocalFileStream then      
        ReplySign := rsSuccess
      else
      begin
        if (p^.FFileStream as TxdFileSegmentStream).SegmentTable.IsCompleted then
          ReplySign := rsSuccess
        else
          ReplySign := rsPart;
      end;
    end
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
      if ReplySign <> rsNotFind then
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
    ReleaseUpShareInfo( p );
  end;
end;

procedure TxdClientUpShareManage.DoHandleCmd_QueryFileProgress(const AIP: Cardinal; const APort: Word;
  const ABuffer: PAnsiChar; const ABufLen: Cardinal; const AIssynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdQueryFileProgressInfo;
  p: PUpShareInfo;
  oSendStream: TxdStaticMemory_512Byte;
  ReplySign: TReplySign;
  table: TxdSegmentStateTable;
begin
  if ABufLen <> CtCmdQueryFileProgressInfoSize then
  begin
    DoErrorInfo( 'QueryFileProgress 命令长度不正确' );
    Exit;
  end;
  pCmd := PCmdQueryFileProgressInfo(ABuffer);
  p := FindUpShareInfo( pCmd^.FHashStyle, TxdHash(pCmd^.FHash) );
  try
    table := nil;
    if Assigned(p) then
    begin
      ReplySign := rsSuccess;
      if p^.FFileStream is TxdLocalFileStream then
        table := nil
      else
        table := (p^.FFileStream as TxdFileSegmentStream).SegmentTable.SegmentStateTable;
    end
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
      begin
        if Assigned(table) then
        begin
          oSendStream.WriteInteger( table.BitMemLen );
          oSendStream.WriteLong( PChar(table.BitMem)^, table.BitMemLen );
        end
        else
          oSendStream.WriteInteger( 0 ); //表示此文件已经完成        
      end;
      FUdp.SendBuffer( AIP, APort, oSendStream.Memory, oSendStream.Position );
    finally
      oSendStream.Free;
    end;
  finally
    ReleaseUpShareInfo( p );
  end;
end;

procedure TxdClientUpShareManage.DoHandleCmd_RequestFileData(const AIP: Cardinal; const APort: Word;
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
            Inc( p^.FUpSize, nSendByteCount );
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
    ReleaseUpShareInfo( p );
  end;
end;

procedure TxdClientUpShareManage.DoHandleUpShareCmdEvent(const ACmd: Word; const AIP: Cardinal; const APort: Word;
  const ABuffer: pAnsiChar; const ABufLen: Cardinal; const AIsSynchroCmd: Boolean; const ASynchroID: Word);
begin
  case ACmd of
    CtCmd_QueryFileInfo:      DoHandleCmd_QueryFileInfo( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_QueryFileProgress:  DoHandleCmd_QueryFileProgress( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_RequestFileData:    DoHandleCmd_RequestFileData( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_GetFileSegmentHash: DoHandleCmd_GetFileSegmentHash( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
  end;
end;

procedure TxdClientUpShareManage.DoThreadToDeleteTimeoutUpShare;
var
  i: Integer;
  p: PUpShareInfo;
  dwTime: Cardinal;
begin
  dwTime := GetTickCount;
  
  LockManage;
  try
    for i := FTaskList.Count - 1 downto 0 do
    begin
      p := FTaskList[i];
      if (p^.FRefCount <= 0) and (p^.FLastActiveTime + MaxUnActiveTime > dwTime) then
      begin
        FreeUpShareInfo( p );
        FTaskList.Delete( i );
      end;
    end;     
  finally
    UnlockManage;
  end;

  if FTaskList.Count = 0 then
  begin
    FAutoDeleteThread.AutoFreeSelf;
    FAutoDeleteThread := nil;
  end;
end;

function TxdClientUpShareManage.FindUpShareInfo(const AHashStyle: THashStyle; const AHash: TxdHash): PUpShareInfo;
var
  i: Integer;
  p: PUpShareInfo;
  pFileShare: PFileShareInfo;
  bOK: Boolean;
  stream: TxdP2SPFileStreamBasic;
begin
  Result := nil;
  
  LockManage;
  try
    bOK := False;
    p := nil;
    for i := 0 to FTaskList.Count - 1 do
    begin
      p := FTaskList[i];
      if AHashStyle = hsFileHash then
      begin
        if HashCompare(p^.FFileStream.FileHash, AHash) then
        begin
          bOK := True;
          Break;
        end;
      end
      else
      begin
        if HashCompare(p^.FFileStream.WebHash, AHash) then
        begin
          bOK := True;
          Break;
        end;
      end;
    end;

    //没有查找到时
    if not bOK then
    begin
      if FTaskList.Count >= FMaxUpTaskCount then Exit;     
      
      //开始查找流
      stream := StreamManage.QueryFileStream( AHashStyle, AHash );
      if not Assigned(stream) and Assigned(FFileShareManage) then
      begin
        //从共享管理器中查找
        FFileShareManage.LockShareManage;
        try
          if AHashStyle = hsFileHash then
            pFileShare := FFileShareManage.FindShareInfoByFileHash( AHash )
          else
            pFileShare := FFileShareManage.FindShareInfoByWebHash( AHash );

          if Assigned(pFileShare) then
          begin
            stream := StreamManage.CreateFileStream( pFileShare^.FFileName, pFileShare^.FFileHash, pFileShare^.FSegmentSize);
            if Assigned(stream) then
              stream.WebHash := pFileShare^.FWebHash;
          end;
        finally
          FFileShareManage.UnLockShareManage;
        end;
      end;
      //流查找结束

      if Assigned(stream) then
      begin
        New( p );
        p^.FFileStream := stream;
        p^.FRefCount := 0;
        p^.FUpSize := 0;
        bOK := True;
        FTaskList.Add( p );
      end;
    end; //End if not bOK then

    if bOK then
    begin
      Inc( p^.FRefCount );
      p^.FLastActiveTime := GetTickCount;
      Result := p;
    end;   
  finally
    UnlockManage;
  end;
end;

procedure TxdClientUpShareManage.FreeUpShareInfo(const Ap: PUpShareInfo);
begin
  if Assigned(Ap) then
  begin
    StreamManage.ReleaseFileStream( Ap^.FFileStream );
    Dispose( Ap );
  end; 
end;

procedure TxdClientUpShareManage.LockManage;
begin
  EnterCriticalSection( FLock );
end;

procedure TxdClientUpShareManage.ReleaseUpShareInfo(const Ap: PUpShareInfo);
begin
  if not Assigned(Ap) then Exit;
  LockManage;
  try
    Dec( Ap^.FRefCount );
    Ap^.FLastActiveTime := GetTickCount;

    if Ap^.FRefCount <= 0 then
    begin
      if not Assigned(FAutoDeleteThread) then
        FAutoDeleteThread := TThreadCheck.Create( DoThreadToDeleteTimeoutUpShare, FMaxUnActiveTime + 100 );
    end;
  finally
    UnlockManage;
  end;
end;

procedure TxdClientUpShareManage.SetMaxUnActiveTime(const Value: Cardinal);
begin
  if (Value <> FMaxUnActiveTime) and (Value > 1000 * 60) then
    FMaxUnActiveTime := Value;
end;

procedure TxdClientUpShareManage.SetMaxUpTaskCount(const Value: Integer);
begin
  if (Value >= CtMinUpTaskCount) and (FMaxUpTaskCount <> Value) then
    FMaxUpTaskCount := Value;
end;

procedure TxdClientUpShareManage.SetUDP(const Value: TxdUdpCommonClient);
begin
  FUDP := Value;
  if Assigned(FUDP) then
    FUDP.OnUpShareCmdEvent := DoHandleUpShareCmdEvent;
end;

procedure TxdClientUpShareManage.UnlockManage;
begin
  LeaveCriticalSection( FLock );
end;

end.
