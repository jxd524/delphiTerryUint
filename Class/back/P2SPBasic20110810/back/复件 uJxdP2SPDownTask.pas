{
单元名称: uJxdP2SPDownTask
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明:
开始时间: 2011-04-11
修改时间: 2011-04-19 (最后修改时间)
类说明  :

    单个下载任务的管理，包括P2P下载，HTTP下载，FTP下载的管理，文件分段下载方式
        下载所需要的所有信息由外部提供，类功能只需要根据参数进行合理的调度，下载
}
unit uJxdP2SPDownTask;

interface

uses
  Windows, Classes, SysUtils, MD4, uJxdDataStream, uJxdFileSegmentStream, uJxdCmdDefine, uJxdUdpSynchroBasic, uJxdServerManage;

type
  {P2P源信息结构}
  PP2PSource = ^TP2PSource;
  TP2PSource = record //P2P源由外部进行心跳维护
    FUserID: Cardinal; //P2P源ID
    FUserIP: Cardinal; //P2P用户ID
    FUserPort: Word; //P2P用户端口
    FSendByte: Cardinal; //请求数据的大小（文件数据大小）
    FSendTime: Cardinal; //请求数据时间
    FRecvByte: Cardinal; //接收到数据量（单指文件数据大小）
    FRecvTime: Cardinal; //请求数据后第一次接收时间
    FSendBlockCount: Integer; //上次请求的分块数量
    FTimeoutCount: Integer; //超时次数，请求数据后，无接收到数据时使用
    FSegTableState: TxdSegmentStateTable; //P2P用户下载文件分段信息表
  end;
  {$M+}
  TxdP2SPDownTask = class
  public
    constructor Create;
    destructor  Destroy; override;
    //初始化任务信息
    function  SetFileInfo(const ASaveFileName: string; const AFileHash: TMD4Digest): Boolean;
    function  SetSegmentTable(const ATable: TxdFileSegmentTable): Boolean;
    function  SetUdp(AUdp: TxdUdpSynchroBasic): Boolean;
    procedure SaveAs(const ANewFileName: string); //下载完成后改变
    //添加P2P源
    procedure AddP2PSource(const AUserID: Cardinal; const AIP: Cardinal; const APort: Word);
    //接收数据接口
    procedure DoRecvFileData(const AIP: Cardinal; const APort: Word; const ABuf: PByte; const ABufLen: Integer);
    procedure DoRecvFileSegmentHash(const AIP: Cardinal; const APort: Word; const ABuf: PByte; const ABufLen: Integer);
    //执行下载函数
    procedure DoExcuteDownTask;
  private
    FUdp: TxdUdpSynchroBasic;
    FFileStream: TxdFileSegmentStream;
    FSegmentTable: TxdFileSegmentTable;
    FP2PSourceList: TThreadList;
    FCtrlEvent: Cardinal;

    FLastCalcSpeekTime: Cardinal;
    FCalcRecvByte: Integer;
    FLastCheckTime: Cardinal;

    FSaveAsFileName: string;

    //分段HASH验证信息
    FCheckSegmentHashing: Boolean;
    FLasCheckSegmentHashTime: Cardinal;
    FCheckSegmentHashSize: Cardinal;
    FRecvSegmentHash: array of TMD4Digest;

    procedure ActiveDownTask;
    procedure UnActiveDownTask;

    procedure ClearP2PSource;
    procedure DoErrorInfo(const AInfo: string);
    function  GetHashCheckSegmentPeer: PP2PSource;
    procedure DoInvalidP2PSource(Ap: PP2PSource); //多次超时的P2P源

    procedure DoDownSuccess;
    procedure DoCheckSegmentHash(Ap: PP2PSource);
    procedure DoCompareSegmentHash;

    //线程函数
    procedure DoThreadControlP2SPDown;
    procedure CheckP2PSource(const ACurTime: Cardinal);

    procedure CalcTaskSpeek; //计算任务总的下载速度
    function  GetMaxBlockCount(p: PP2PSource): Integer; //计算出P2P分块数量
    function  IsCanGetMostNeedBlock(p: PP2PSource): Boolean; //是否能请求最需要分块
  private
    FActive: Boolean;
    FFileName: string;
    FFileSize: Int64;
    FFileHash: TMD4Digest;
    FFileSegmentSize: Integer;
    FMaxRequestBlockCount: Integer;
    FCurThreadCount: Integer;
    FSpeekLimit: Integer;
    FCurSpeek: Integer;
    FMaxP2PSource: Integer;
    FMaxSearchCount: Integer;
    procedure SetActive(const Value: Boolean);
    procedure SetMaxRequestBlockCount(const Value: Integer);
    procedure SetSpeekLimit(const Value: Integer);
    procedure SetMaxP2PSource(const Value: Integer);
    procedure SetMaxSearchCount(const Value: Integer);
  published
    property Active: Boolean read FActive write SetActive; //是否下载中
    property FileName: string read FFileName; //文件名称
    property FileSize: Int64 read FFileSize; //文件大小
    property FileHash: TMD4Digest read FFileHash; //分块HASH
    property FileSegmentSize: Integer read FFileSegmentSize; //文件分段大小
    property SegmentTable: TxdFileSegmentTable read FSegmentTable; //文件分段表
    property CurThreadCount: Integer read FCurThreadCount; //当前使用线程数量
    property MaxRequestBlockCount: Integer read FMaxRequestBlockCount write SetMaxRequestBlockCount;  //P2P下载最大请求分块数量, 分块大小由TxdFileSegmentTable指定
    property SpeekLimit: Integer read FSpeekLimit write SetSpeekLimit; //速度限制 单位：KB/S
    property CurSpeek: Integer read FCurSpeek; //发前速度 单位：KB/S
    property MaxP2PSource: Integer read FMaxP2PSource write SetMaxP2PSource; //最大P2P源(包含文件服务器源)
    property MaxSearchCount: Integer read FMaxSearchCount write SetMaxSearchCount; //最大搜索次数
  end;
  {$M-}
  
implementation

uses
  uJxdThread;

{ TxdP2SPDownTask }

procedure TxdP2SPDownTask.ActiveDownTask;
begin
  try
    FFileStream := StreamManage.CreateFileStream(FFileName, FSegmentTable);
    if not Assigned(FFileStream) then Exit;

    FFileStream.SetFileHash( FFileHash );
    FFileStream.RenameFileName := FSaveAsFileName;
    
    FCurThreadCount := 0;
    FCalcRecvByte := 0;
    FLasCheckSegmentHashTime := 0;
    FLastCheckTime := 0;
    FCheckSegmentHashing := False;

    FCtrlEvent := CreateEvent(nil, False, False, nil);
    RunningByThread( DoThreadControlP2SPDown );
    
    FActive := True;
  except
    FActive := False;
    UnActiveDownTask;
  end;
end;

procedure TxdP2SPDownTask.AddP2PSource(const AUserID, AIP: Cardinal; const APort: Word);
var
  p: PP2PSource;
  bFind: Boolean;
  lt: TList;
  i: Integer;
begin
  bFind := False;
  lt := FP2PSourceList.LockList;
  try
    for i := 0 to lt.Count - 1 do
    begin
      p := lt[i];
      if (p^.FUserIP = AIP) and (p^.FUserPort = APort) then
      begin
        bFind := True;
        p^.FUserID := AUserID;
        p^.FRecvTime := 0;
        p^.FSendTime := 0;
        Break;
      end;
    end;
    if not bFind then
    begin
      New( p );
      p^.FUserID := AUserID;
      p^.FUserIP := AIP;
      p^.FUserPort := APort;
      p^.FRecvByte := 0;
      p^.FSendByte := 0;
      p^.FRecvTime := 0;
      p^.FSendTime := 0;
      lt.Add( p );
    end;
  finally
    FP2PSourceList.UnlockList;
  end;
end;

procedure TxdP2SPDownTask.CalcTaskSpeek;
var
  nTemp: Cardinal;
begin
  if FCalcRecvByte = 0 then
    FLastCalcSpeekTime := GetTickCount
  else
  begin
    nTemp := GetTickCount;
    if nTemp - FLastCalcSpeekTime > 1000 then
    begin
      FCurSpeek := Round( (FCalcRecvByte / 1024) / ((nTemp - FLastCalcSpeekTime) * 1000) );
      FLastCalcSpeekTime := nTemp;
      InterlockedExchange( FCalcRecvByte, 0 );
    end;
  end;
end;

procedure TxdP2SPDownTask.CheckP2PSource(const ACurTime: Cardinal);
begin

end;

procedure TxdP2SPDownTask.ClearP2PSource;
var
  i: Integer;
  lt: TList;
begin
  lt := FP2PSourceList.LockList;
  try
    for i := 0 to lt.Count - 1 do
      Dispose( lt[i] );
    lt.Clear;
  finally
    FP2PSourceList.UnlockList;
  end;
end;

constructor TxdP2SPDownTask.Create;
begin
  FActive := False;
  FMaxRequestBlockCount := 8;
  FCurThreadCount := 0;
  FLastCalcSpeekTime := 0;
  FCalcRecvByte := 0;
  FMaxP2PSource := 10;
  FMaxSearchCount := 5;
  FP2PSourceList := TThreadList.Create;
end;

destructor TxdP2SPDownTask.Destroy;
begin
  Active := False;
  ClearP2PSource;
  FP2PSourceList.Free;
  inherited;
end;

procedure TxdP2SPDownTask.DoCheckSegmentHash(Ap: PP2PSource);
var
  cmd: TCmdGetFileSegmentHashInfo;
begin
  if GetTickCount - FLasCheckSegmentHashTime >= 5000 then
  begin
    if not Assigned(Ap) then
      Ap := GetHashCheckSegmentPeer;
    if not Assigned(Ap) then Exit;
    FUdp.AddCmdHead( @Cmd, CtCmd_GetFileSegmentHash );
    Move( FFileHash, cmd.FFileHash, CMD4Size );
    FLasCheckSegmentHashTime := GetTickCount;
    FUdp.SendBuffer( Ap^.FUserIP, Ap^.FUserPort, @Cmd, CtCmdGetFileSegmentHashInfoSize );
  end;
end;

procedure TxdP2SPDownTask.DoCompareSegmentHash;
var
  i, j, nCount, nSegIndex: Integer;
  aryCalcSegHash: array of TMD4Digest;
  buf: PByte;
  nReadSize: Integer;
begin
  FCheckSegmentHashing := False;
  FLasCheckSegmentHashTime := 0;

  nCount := Length(FRecvSegmentHash);
  if nCount = 0 then Exit;
  GetMem( buf, FCheckSegmentHashSize );
  try
    for i := 0 to nCount - 1 do
    begin
      if i = nCount - 1 then
        nReadSize := FFileSize - Cardinal(i) * FCheckSegmentHashSize
      else
        nReadSize := FCheckSegmentHashSize;
      FFileStream.ReadBuffer(Cardinal(i) * FCheckSegmentHashSize, nReadSize, buf);
      aryCalcSegHash[i] := MD4Buffer( buf^, nReadSize );
    end;
  finally
    FreeMem( buf, FCheckSegmentHashSize );
  end;

  for i := 0 to nCount - 1 do
  begin
    if not MD4DigestCompare(aryCalcSegHash[i], FRecvSegmentHash[i]) then
    begin
      nSegIndex := (Cardinal(i) * FCheckSegmentHashSize + FSegmentTable.SegmentSize - 1) div FSegmentTable.SegmentSize;
      for j := nSegIndex to nSegIndex + Integer((FCheckSegmentHashSize + FSegmentTable.SegmentSize - 1) div FSegmentTable.SegmentSize) do
        FSegmentTable.ResetSegment( j );
    end;
  end;
end;

procedure TxdP2SPDownTask.DoDownSuccess;
begin
  OutputDebugString( '文件下载成功' );
  StreamManage.ReleaseFileStream( FFileStream );
end;

procedure TxdP2SPDownTask.DoErrorInfo(const AInfo: string);
begin
  OutputDebugString( PChar(AInfo) );
end;

procedure TxdP2SPDownTask.DoExcuteDownTask;
var
  i, nCurIndex, nCount: Integer;
  p: PP2PSource;
  lt: TList;
  oSendStream: TxdStaticMemory_2K;
  nCountPos, nRequestCount: Integer;
  bOK, bGetMostNeedBlock: Boolean;
  nSegIndex, nBlockIndex, nLen, nBlockSize: Integer;
  dwCurTime: Cardinal;
begin
  dwCurTime := GetTickCount;
  if FLastCheckTime = 0 then
    FLastCheckTime := dwCurTime;
    
  if GetTickCount - FLastCheckTime > FSegmentTable.MaxWaitTime then
  begin
    //检查超时
    FSegmentTable.CheckDownReplyWaitTime;
    FLastCheckTime := GetTickCount;
  end;
  CalcTaskSpeek;

  oSendStream := TxdStaticMemory_2K.Create;
  try

      
      //计算用户可以申请的分块数量
      lt := FP2PSourceList.LockList;
      try
        p := lt[nCurIndex];
        bGetMostNeedBlock := IsCanGetMostNeedBlock(p);
        nCount := GetMaxBlockCount( p );
        nCurIndex := (nCurIndex + 1) mod lt.Count;
      finally
        FP2PSourceList.UnlockList;
      end;
      //封装数据包
      nLen := 0;
      if (nCount > 0) and (nCount <= CtMaxRequestFileBlockCount) then
      begin
        oSendStream.Clear;
        FUdp.AddCmdHead(oSendStream, CtCmd_RequestFileData);
        oSendStream.WriteLong(FFileHash, CMD4Size);
        nCountPos := oSendStream.Position;
        oSendStream.Position := oSendStream.Position + 2;
        nRequestCount := 0;
        for i := 0 to nCount - 1 do
        begin
          if bGetMostNeedBlock then
            bOK := FSegmentTable.GetNeedMostBlockInfo(nSegIndex, nBlockIndex)
          else
            bOK := FSegmentTable.GetP2PBlockInfo(nSegIndex, nBlockIndex, p^.FSegTableState);
          if bOK then
          begin
            if FSegmentTable.GetBlockSize(nSegIndex, nBlockIndex, nBlockSize) then
            begin
              Inc(nLen, nBlockSize);
              Inc(nRequestCount);
              oSendStream.WriteInteger(nSegIndex);
              oSendStream.WriteWord(nBlockIndex);
            end;
          end;
        end;
        if nRequestCount > 0 then
        begin
          p^.FSendTime := GetTickCount;
          p^.FSendByte := nLen;
          p^.FSendBlockCount := nRequestCount;

          nLen := oSendStream.Position;
          oSendStream.Position := nCountPos;
          oSendStream.WriteWord( Word(nRequestCount) );
          OutputDebugString( Pchar('发送长度：' + IntToStr(nLen)) );
          FUdp.SendBuffer(p^.FUserIP, p^.FUserPort, oSendStream.Memory, nLen);
        end
        else
        begin
          p^.FSendTime := 0;
          p^.FSendByte := 0;
          p^.FSendBlockCount := 0;
        end;
      end;

      if FSegmentTable.IsCompleted then
      begin
        if (FLasCheckSegmentHashTime = 0) and MD4DigestCompare( FFileStream.CalcFileHash(nil), FFileHash ) then
        begin
          //文件下载成功
          DoDownSuccess;
          Break;
        end
        else
        begin
          //一般情况很少需要用到分段HASH验证的
          if FCheckSegmentHashing then
          begin
            //计算自身分段信息
            DoCompareSegmentHash;
          end
          else
            DoCheckSegmentHash(nil);
        end;
      end;
  finally
    oSendStream.Free;
  end;
end;


procedure TxdP2SPDownTask.DoInvalidP2PSource(Ap: PP2PSource);
begin

end;

procedure TxdP2SPDownTask.DoRecvFileData(const AIP: Cardinal; const APort: Word; const ABuf: PByte; const ABufLen: Integer);
var
  pCmd: PCmdReplyRequestFileInfo;
  nPos: Int64;
  nSize: Cardinal;
  i: Integer;
  lt: TList;
  p: PP2PSource;
begin
  if ABufLen < CtMinPackageLen + CMD4Size + 1 then
  begin
    DoErrorInfo( '接收到的P2P文件传输数据长度不正确' );
    Exit;
  end;
  pCmd := PCmdReplyRequestFileInfo(ABuf);
  if pCmd^.FReplySign <> rsSuccess then
  begin
    DoErrorInfo( '请求不到指定的P2P数据' );
    Exit;
  end;
  if not FSegmentTable.GetBlockSize(pCmd^.FSegmentIndex, pCmd^.FBlockIndex, nPos, nSize) then
  begin
    DoErrorInfo( '接收到的P2P数据的分段或分块序号不正确' );
    Exit;
  end;
  if nSize <> pCmd^.FBufferLen then
  begin
    DoErrorInfo( '接收到的P2P文件数据的长度与本地计算的不一致' );
    Exit;
  end;
  //更新
  lt := FP2PSourceList.LockList;
  try
    for i := 0 to lt.Count - 1 do
    begin
      p := lt[i];
      if (p^.FUserIP = AIP) and (p^.FUserPort = APort) then
      begin
        p^.FRecvByte := p^.FRecvByte + pCmd^.FBufferLen;
        p^.FRecvTime := GetTickCount;
        Break;
      end;
    end;
  finally
    FP2PSourceList.UnlockList;
  end;
  //写入文件
  FFileStream.WriteBlockBuffer( pCmd^.FSegmentIndex, pCmd^.FBlockIndex, @pCmd^.FBuffer, pCmd^.FBufferLen );
end;

procedure TxdP2SPDownTask.DoRecvFileSegmentHash(const AIP: Cardinal; const APort: Word; const ABuf: PByte; const ABufLen: Integer);
var
  pCmd: PCmdReplyGetFileSegmentHashInfo;
  i, nCount: Integer;
begin
  if ABufLen < CtCmdReplyGetFileSegmentHashInfoSize then
  begin
    DoErrorInfo( '接收到的分段HASH验证信息出错' );
    Exit;
  end;
  pCmd := PCmdReplyGetFileSegmentHashInfo( ABuf );
  FCheckSegmentHashSize := pCmd^.FHashCheckSegmentSize;
  if (FCheckSegmentHashSize = 0) or (FCheckSegmentHashSize > FFileSize) then
  begin
    DoErrorInfo( '接收到的HASH计算分段不正确' );
    Exit;
  end;
  nCount := (FFileSize + FCheckSegmentHashSize - 1) div FCheckSegmentHashSize;
  SetLength( FRecvSegmentHash, nCount );
  for i := 0 to nCount - 1 do
    Move( pCmd^.FSegmentHashs[i], FRecvSegmentHash[i], CMD4Size );
  FCheckSegmentHashing := True;
end;

procedure TxdP2SPDownTask.DoThreadControlP2SPDown;
var
  i, nCurIndex, nCount: Integer;
  p: PP2PSource;
  lt: TList;
  nWaitTime: Cardinal;
  oSendStream: TxdStaticMemory_2K;
  nCountPos, nRequestCount: Integer;
  bOK, bGetMostNeedBlock: Boolean;
  nSegIndex, nBlockIndex, nLen, nBlockSize: Integer;
  nLastCheckTime: Cardinal;
begin
  InterlockedIncrement( FCurThreadCount );
  nWaitTime := 100;
  nCurIndex := 0;
  nLastCheckTime := GetTickCount;
  oSendStream := TxdStaticMemory_2K.Create;
  try
    while Active do
    begin
      if GetTickCount - nLastCheckTime > FSegmentTable.MaxWaitTime then
      begin
        //检查超时
        FSegmentTable.CheckDownReplyWaitTime;
        nLastCheckTime := GetTickCount;
      end;
      CalcTaskSpeek;
      
      //计算用户可以申请的分块数量
      lt := FP2PSourceList.LockList;
      try
        p := lt[nCurIndex];
        bGetMostNeedBlock := IsCanGetMostNeedBlock(p);
        nCount := GetMaxBlockCount( p );
        nCurIndex := (nCurIndex + 1) mod lt.Count;
      finally
        FP2PSourceList.UnlockList;
      end;
      //封装数据包
      nLen := 0;
      if (nCount > 0) and (nCount <= CtMaxRequestFileBlockCount) then
      begin
        oSendStream.Clear;
        FUdp.AddCmdHead(oSendStream, CtCmd_RequestFileData);
        oSendStream.WriteLong(FFileHash, CMD4Size);
        nCountPos := oSendStream.Position;
        oSendStream.Position := oSendStream.Position + 2;
        nRequestCount := 0;
        for i := 0 to nCount - 1 do
        begin
          if bGetMostNeedBlock then
            bOK := FSegmentTable.GetNeedMostBlockInfo(nSegIndex, nBlockIndex)
          else
            bOK := FSegmentTable.GetP2PBlockInfo(nSegIndex, nBlockIndex, p^.FSegTableState);
          if bOK then
          begin
            if FSegmentTable.GetBlockSize(nSegIndex, nBlockIndex, nBlockSize) then
            begin
              Inc(nLen, nBlockSize);
              Inc(nRequestCount);
              oSendStream.WriteInteger(nSegIndex);
              oSendStream.WriteWord(nBlockIndex);
            end;
          end;
        end;
        if nRequestCount > 0 then
        begin
          p^.FSendTime := GetTickCount;
          p^.FSendByte := nLen;
          p^.FSendBlockCount := nRequestCount;

          nLen := oSendStream.Position;
          oSendStream.Position := nCountPos;
          oSendStream.WriteWord( Word(nRequestCount) );
          OutputDebugString( Pchar('发送长度：' + IntToStr(nLen)) );
          FUdp.SendBuffer(p^.FUserIP, p^.FUserPort, oSendStream.Memory, nLen);
        end
        else
        begin
          p^.FSendTime := 0;
          p^.FSendByte := 0;
          p^.FSendBlockCount := 0;
        end;
      end;

      if FSegmentTable.IsCompleted then
      begin
        if (FLasCheckSegmentHashTime = 0) and MD4DigestCompare( FFileStream.CalcFileHash(nil), FFileHash ) then
        begin
          //文件下载成功
          DoDownSuccess;
          Break;
        end
        else
        begin
          //一般情况很少需要用到分段HASH验证的
          if FCheckSegmentHashing then
          begin
            //计算自身分段信息
            DoCompareSegmentHash;
          end
          else
            DoCheckSegmentHash(nil);
        end;
      end;

      WaitForSingleObject( FCtrlEvent, nWaitTime );
    end;
  finally
    oSendStream.Free;
    InterlockedDecrement( FCurThreadCount );
  end;
end;

function TxdP2SPDownTask.GetHashCheckSegmentPeer: PP2PSource;
var
  i: Integer;
  lt: TList;
begin
  Result := nil;
  lt := FP2PSourceList.LockList;
  try
    for i := 0 to lt.Count - 1 do
      Result := lt[i];
  finally
    FP2PSourceList.UnlockList;
  end;
end;

function TxdP2SPDownTask.GetMaxBlockCount(p: PP2PSource): Integer;
const
  CtTimeOutTimeSpace = 2000;
  CtMaxTimeoutCount = 10;
var
  nSpeed1, nSpeed2: Integer;
  nTimeSpace: Cardinal;
  nCurTime: Cardinal;
begin
  //P2P下载速度控制函数，在局域中，由以下算法造成的下载速度大概为1M/S，
  //此时需要发送方的组合包数据达到至少3500个
//  Result := 3;
//  Exit;

  Result := 0;
  if p^.FSendByte = 0 then
    Result := 1
  else
  begin
    nCurTime := GetTickCount;
    if p^.FRecvByte = 0 then
    begin
      //已经请求数据，但对方还没有发送过来, 如果超过2秒还没有接收到数据，则认为超时
      if nCurTime - p^.FSendTime > CtTimeOutTimeSpace then
      begin
        if p^.FTimeoutCount > CtMaxTimeoutCount then
        begin
          DoInvalidP2PSource( p );
          Result := 0;
        end
        else
        begin
          Inc( p^.FTimeoutCount );
          Result := 1;
        end;
      end;
    end
    else
    begin
      //已经接收到数据咯
      nTimeSpace := nCurTime - p^.FSendTime;
      if nTimeSpace = 0 then nTimeSpace := 1;
      nSpeed1 := p^.FSendByte div nTimeSpace;

      nTimeSpace := p^.FRecvTime - p^.FSendTime;
      if nTimeSpace = 0 then nTimeSpace := 1;
      nSpeed2 := p^.FRecvByte div nTimeSpace;
      
      if nSpeed2 >= nSpeed1 then
      begin
        //提高速度
        if p^.FTimeoutCount = 0 then
          Result := p^.FSendBlockCount + 1
        else
        begin
          Result := nSpeed2 * 100 div CtBlockSize;
          if Result > p^.FSendBlockCount then
            Result := p^.FSendBlockCount + 1
          else if Result < p^.FSendBlockCount then
            Result := p^.FSendBlockCount - 1;
        end;
          
        p^.FTimeoutCount := 0;
        if Result <= 0 then
          Result := 1
        else if Result > CtMaxRequestFileBlockCount then
          Result := p^.FSendBlockCount + 1;
      end
      else
      begin
        //速度达不到预期
        if (p^.FTimeoutCount < CtMaxTimeoutCount) and ((nCurTime - p^.FSendTime) < CtTimeOutTimeSpace) then
        begin
          //暂停下载
          Result := 0;
          Inc( p^.FTimeoutCount );
        end
        else
        begin
          p^.FTimeoutCount := 0;
          Result := (Cardinal(nSpeed2) * nTimeSpace + CtMaxRequestPackageSize - 1) div CtMaxRequestPackageSize;
          if Result <= 0 then
            Result := 1
          else if Result > p^.FSendBlockCount then
            Result := p^.FSendBlockCount + 1;
        end;
      end;
    end;
  end;
  if Result > 0 then
  begin
    p^.FRecvByte := 0;
    p^.FRecvTime := 0;
  end;
end;

function TxdP2SPDownTask.IsCanGetMostNeedBlock(p: PP2PSource): Boolean;
begin
  Result := True;
end;

procedure TxdP2SPDownTask.SaveAs(const ANewFileName: string);
begin
  FSaveAsFileName := ANewFileName;
  if Assigned(FFileStream) then
    FFileStream.RenameFileName := FSaveAsFileName;
end;

procedure TxdP2SPDownTask.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      ActiveDownTask
    else
      UnActiveDownTask;
  end;
end;

function TxdP2SPDownTask.SetFileInfo(const ASaveFileName: string; const AFileHash: TMD4Digest): Boolean;
var
  strDir: string;
begin
  Result := False;
  if not Active then
  begin
    strDir := ExtractFilePath(ASaveFileName);
    if not DirectoryExists(strDir) then
      if not ForceDirectories(strDir) then Exit;
    FFileName := ASaveFileName;
    FFileHash := AFileHash;
  end;
end;

procedure TxdP2SPDownTask.SetMaxP2PSource(const Value: Integer);
begin
  if (FMaxP2PSource <> Value) and (Value > 0) then
    FMaxP2PSource := Value;
end;

procedure TxdP2SPDownTask.SetMaxRequestBlockCount(const Value: Integer);
begin
  if (FMaxRequestBlockCount <> Value) and (Value > 0) and (Value <= CtMaxRequestFileBlockCount) then
    FMaxRequestBlockCount := Value;
end;

procedure TxdP2SPDownTask.SetMaxSearchCount(const Value: Integer);
begin
  if (FMaxSearchCount <> Value) and (Value > 0) then
    FMaxSearchCount := Value;
end;

function TxdP2SPDownTask.SetSegmentTable(const ATable: TxdFileSegmentTable): Boolean;
begin
  Result := (not Active) and Assigned(ATable);
  if Result then
  begin
    FSegmentTable := ATable;
    FFileSize := FSegmentTable.FileSize;
    FFileSegmentSize := FSegmentTable.SegmentSize;
  end;
end;

procedure TxdP2SPDownTask.SetSpeekLimit(const Value: Integer);
begin
  if FSpeekLimit <> Value then
    FSpeekLimit := Value;
end;

function TxdP2SPDownTask.SetUdp(AUdp: TxdUdpSynchroBasic): Boolean;
begin
  Result := not Active and Assigned(AUdp);
  if Result then
    FUdp := AUdp;
end;

procedure TxdP2SPDownTask.UnActiveDownTask;
begin
  FActive := False;
  if FCtrlEvent <> 0 then
    SetEvent( FCtrlEvent );
  while FCurThreadCount > 0 do
    Sleep( 10 );
  if FCtrlEvent <> 0 then
    CloseHandle( FCtrlEvent );
  FCtrlEvent := 0;
  if Assigned(FFileStream) then
    StreamManage.ReleaseFileStream( FFileStream );
end;

end.
