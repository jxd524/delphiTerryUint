{
单元名称: uJxdP2SPDownTask
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明:
开始时间: 2011-04-11
修改时间: 2011-05-06 (最后修改时间)
类说明  :

    单个下载任务的管理，包括P2P下载，HTTP下载，FTP下载的管理，文件分段下载方式
        下载所需要的所有信息由外部提供，类功能只需要根据参数进行合理的调度，下载

编译选项
    ExclusionP2P: 不使用P2P下载, 只使用HTTP
}

unit uJxdP2SPDownTask;

interface

uses
  Windows, Classes, SysUtils, uJxdHashCalc, uJxdDataStream, uJxdFileSegmentStream, uJxdCmdDefine, uJxdUdpSynchroBasic, uJxdServerManage,
  idHttp, IdComponent;

type
  //请求分块管理器
  TRequestBlockManage = class
  public
    procedure Clear;
    procedure AddRequestBlock(const ASegIndex, ABlockIndex: Integer);
    procedure FinishedBlock(const ASegIndex, ABlockIndex: Integer);
  private
    FRequestBlockCount,
    FRequestCount,
    FFinishedCount: Integer;
    FBlocks: array of Cardinal;
    FLastRequestTime: Cardinal;
    function  CalcID(const ASegIndex, ABlockIndex: Integer): Cardinal; inline;
  public
    property LastRequestTime: Cardinal read FLastRequestTime;
    property RequestCount: Integer read FRequestCount; //请求分块数量
    property FinishedCount: Integer read FFinishedCount; //完成分块数量
  end;

  {P2P源信息结构}
  TSourceState = (ssUnkown, ssChecking, ssSuccess);
  PP2PSource = ^TP2PSource;
  TP2PSource = record
    FIP: Cardinal; //P2P用户ID
    FPort: Word; //P2P用户端口
    FState: TSourceState; //P2P状态
    FLastCheckStateTime: Cardinal;
    FTimeoutCount: Integer; //超时次数，请求数据后，无接收到数据时使用
    FRequestBlockManage: TRequestBlockManage; //请求分块管理器
    FSegTableState: TxdSegmentStateTable; //P2P用户下载文件分段信息表
    FTotalRecvByteCount: Integer; //总共接收到的数据长度
  end;

  {Http源信息结构}
  PHttpSource = ^THttpSource;
  THttpSource = record
    FUrl: string;
    FReferUrl: string;
    FCookies: string;
    FTotalRecvByteCount: Integer;
  end;
  {$M+}
  TxdP2SPDownTask = class                               
  public
    constructor Create;
    destructor  Destroy; override;
    //初始化任务信息
    function  SetFileInfo(const ASaveFileName: string; const AFileHash: TxdHash; const AWebHash: TxdHash): Boolean;
    function  SetSegmentTable(const ATable: TxdFileSegmentTable): Boolean;
    function  SetUdp(AUdp: TxdUdpSynchroBasic): Boolean;
    procedure SaveAs(const ANewFileName: string); //下载完成后改变
    
    {$IFNDEF ExclusionP2P}
    //P2P源
    procedure AddP2PSource(const AIP: Cardinal; const APort: Word; const AState: TSourceState = ssUnkown; const ATotalByteCount: Integer = -1);
    procedure AddServerSource(const AIP: Cardinal; const APort: Word; const AFileSize: Int64; const ASegmentSize: Integer;
                              const AFileHash: TxdHash);
    function  IsExistsSource(const AIP: Cardinal; const APort: Word): Boolean; overload;
    function  GetP2PSourceInfo(var AInfo: TAryServerInfo): Integer; overload;
    function  GetP2PSourceInfo(const AIndex: Integer; AInfo: PP2PSource): Boolean; overload;
    procedure DeleteP2PSource(const AIP: Cardinal; const APort: Word);
    {$ENDIF}

    //Http源
    procedure AddHttpSource(const AUrl, ARefer, ACookies: string; const ATotalByteCount: Integer = -1);
    function  IsExistsSource(const AUrl: string): Boolean; overload;
    function  GetHttpSourceInfo(const AIndex: Integer; AInfo: PHttpSource): Boolean;
    //接收数据接口
    procedure DoRecvReplyFileExistsInfo(const AIP: Cardinal; const APort: Word; const ACmd: PCmdReplyFileExistsInfo);
    procedure DoRecvFileData(const AIP: Cardinal; const APort: Word; const ABuf: PByte; const ABufLen: Integer);
    procedure DoRecvFileSegmentHash(const AIP: Cardinal; const APort: Word; const ABuf: PByte; const ABufLen: Integer);
    //执行下载函数
    procedure DoExcuteDownTask; //此函数由外部不断调用
  private
    FUdp: TxdUdpSynchroBasic;
    FFileStream: TxdFileSegmentStream;
    FSegmentTable: TxdFileSegmentTable;

    FP2PLastUsedIndex: Integer;
    FP2PSourceList: TThreadList;

    FHttpLastUsedIndex: Integer;
    FHttpGettingFileSize: Boolean;
    FHttpSourceList: TThreadList;
    FHttpThreadList: TList;

    FLastExcuteTime: Cardinal; //最后执行时间
    FLastCheckSegmentTableTime: Cardinal; //最后分段表检查时间

    FLastCalcSpeekTime: Cardinal;
    FCalcRecvByte: Integer;


    FIsEmptyWebHash: Boolean;

    FSaveAsFileName: string;

    //分段HASH验证信息
    FCheckSegmentHashing: Boolean;
    FLasCheckSegmentHashTime: Cardinal;
    FCheckSegmentHashSize: Cardinal;
    FRecvSegmentHash: array of TxdHash;
    FLockCalcWebHash: TRTLCriticalSection;

    procedure ClearList(const AList: TThreadList);
    procedure ClearHttpThread;

    procedure ActiveDownTask;
    procedure UnActiveDownTask;

    procedure CalcWebHash;
    procedure CheckFileStream;
    procedure DoErrorInfo(const AInfo: string);
    function  GetHashCheckSegmentPeer: PP2PSource;
    procedure DoInvalidP2PSource(Ap: PP2PSource); //通知无效的P2P源
    procedure DeleteP2PSourceFromList(Ap: PP2PSource); //把P2P源从列表中删除
    procedure FreeP2PSourceMem(var p: PP2PSource); //释放P2P源占用内存
    procedure ResetP2PSourceStat;

    //下载执行函数
    procedure DownFileDataByP2P;
    procedure CheckHttpDownThreadControl;
    procedure DownFileDataByHttp;
    procedure DoThreadToGetFileSizeByHttp;

    //控制检查
    function  DoCheckTaskSuccess: Boolean;
    procedure DoCheckTaskSpeed;  //计算任务总的下载速度
    procedure DoCheckSegmentTable; //检验分段表中已经超时的信息
    procedure DoCheckFileSegmentInfo(const AFileSize: Int64; const ASegmentSize: Integer);

    //Hash 检验
    procedure DoDownSuccess;
    procedure DoCheckSegmentHash(Ap: PP2PSource);
    procedure DoCompareSegmentHash;
    
    procedure GetFileInfoByP2P(p: PP2PSource); //向指定用户获取文件信息
    function  GetMaxBlockCount(p: PP2PSource): Integer; //计算出P2P分块数量
    function  IsCanGetMostNeedBlock(p: PP2PSource): Boolean; //是否能请求最需要分块

    procedure RelationSegmentTableEvent;
    //事件处理
    procedure DoCompletedSegment(const ASegIndex: Integer); //某一分段下载完成
  private
    FActive: Boolean;
    FFileName: string;
    FFileSize: Int64;
    FFileHash: TxdHash;
    FFileSegmentSize: Integer;
    FMaxRequestBlockCount: Integer;
    FSpeekLimit: Integer;
    FCurSpeek: Integer;
    FMaxP2PSource: Integer;
    FMaxSearchCount: Integer;
    FWebHash: TxdHash;
    FFinished: Boolean;
    FHttpThreadCount: Integer;
    FTaskName: string;
    procedure SetActive(const Value: Boolean);
    procedure SetMaxRequestBlockCount(const Value: Integer);
    procedure SetSpeekLimit(const Value: Integer);
    procedure SetMaxP2PSource(const Value: Integer);
    procedure SetMaxSearchCount(const Value: Integer);
    function  GetCurP2PSourceCount: Integer;
    procedure SetHttpThreadCount(const Value: Integer);
    function GetCurHttpThreadCount: Integer;
    function GetHttpSourceCount: Integer;
    function GetFileStreamID: Integer;
  published
    property Active: Boolean read FActive write SetActive; //是否下载中
    property Finished: Boolean read FFinished; //任务是否完成
    property TaskName: string read FTaskName write FTaskName; //任务名称
    property FileName: string read FFileName; //文件名称
    property SaveAsFileName: string read FSaveAsFileName; //文件下载完成后另存名称
    property FileSize: Int64 read FFileSize; //文件大小
    property FileHash: TxdHash read FFileHash; //分块HASH
    property WebHash: TxdHash read FWebHash; //Web HASH
    property FileSegmentSize: Integer read FFileSegmentSize; //文件分段大小
    property SegmentTable: TxdFileSegmentTable read FSegmentTable; //文件分段表, 类释放时释放此表
    property MaxRequestBlockCount: Integer read FMaxRequestBlockCount write SetMaxRequestBlockCount;  //P2P下载最大请求分块数量, 分块大小由TxdFileSegmentTable指定
    property FileStreamID: Integer read GetFileStreamID;

    property SpeekLimit: Integer read FSpeekLimit write SetSpeekLimit; //速度限制 单位：KB/S
    property CurSpeek: Integer read FCurSpeek; //当前速度 单位：B/MS 

    {P2P源}
    property CurP2PSourceCount: Integer read GetCurP2PSourceCount;
    property MaxP2PSource: Integer read FMaxP2PSource write SetMaxP2PSource; //最大P2P源(包含文件服务器源)
    property MaxSearchCount: Integer read FMaxSearchCount write SetMaxSearchCount; //最大搜索次数

    {Http源}
    property CurHttpSourceCount: Integer read GetHttpSourceCount;

    property CurHttpThreadCount: Integer read GetCurHttpThreadCount; //当前HTTP下载使用线程数量
    property HttpThreadCount: Integer read FHttpThreadCount write SetHttpThreadCount; //Http下载线程数量
  end;
  {$M-}
  
implementation

uses
  uJxdThread, uSocketSub, uConversion;

const
  CtP2PSourceSize = SizeOf(TP2PSource);
  CtHttpSourceSize = SizeOf(THttpSource);
  CtTimeOutTimeSpace = 2000;
  CtMaxTimeoutCount = 10;

{ TxdP2SPDownTask }

procedure TxdP2SPDownTask.ActiveDownTask;
begin
  try
    CheckFileStream;
    ClearHttpThread;

    InitializeCriticalSection( FLockCalcWebHash );
    FCalcRecvByte := 0;
    FLasCheckSegmentHashTime := 0;
    FLastCheckSegmentTableTime := 0;
    FP2PLastUsedIndex := -1;
    FLastExcuteTime := 0;
    FLastCalcSpeekTime := 0;
    FHttpGettingFileSize := False;
    FCheckSegmentHashing := False;
    
    FActive := True;
  except
    FActive := False;
    UnActiveDownTask;
  end;
end;

procedure TxdP2SPDownTask.AddHttpSource(const AUrl, ARefer, ACookies: string; const ATotalByteCount: Integer);
var
  p: PHttpSource;
  lt: TList;
  i: Integer;
  bFind: Boolean;
begin
  if AUrl = '' then Exit;
  bFind := False;
  lt := FHttpSourceList.LockList;
  try
    for i := 0 to lt.Count - 1 do
    begin
      p := lt[i];
      if CompareText(AUrl, p^.FUrl) = 0 then
      begin
        bFind := True;
        Break;
      end;
    end;
    if not bFind then
    begin
      New( p );
      p^.FUrl := AUrl;
      p^.FReferUrl := ARefer;
      p^.FCookies := ACookies;
      if ATotalByteCount > 0 then
        p^.FTotalRecvByteCount := ATotalByteCount;
      lt.Add( p );
    end;
  finally
    FHttpSourceList.UnlockList;
  end;
end;

{$IFNDEF ExclusionP2P}
procedure TxdP2SPDownTask.AddP2PSource(const AIP: Cardinal; const APort: Word; const AState: TSourceState; const ATotalByteCount: Integer);
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
      if (p^.FIP = AIP) and (p^.FPort = APort) then
      begin
        if ATotalByteCount > 0 then
          p^.FTotalRecvByteCount := ATotalByteCount;
        p^.FState := AState;
        bFind := True;
        Break;
      end;
    end;
    if not bFind then
    begin
      New( p );
      FillChar( p^, CtP2PSourceSize, 0 );
      p^.FIP := AIP;
      p^.FState := AState;
      p^.FPort := APort;
      p^.FRequestBlockManage := TRequestBlockManage.Create;
      p^.FRequestBlockManage.Clear;
      if ATotalByteCount > 0 then
        p^.FTotalRecvByteCount := ATotalByteCount;
      lt.Add( p );
    end;
  finally
    FP2PSourceList.UnlockList;
  end;
end;
{$ENDIF}

{$IFNDEF ExclusionP2P}
procedure TxdP2SPDownTask.AddServerSource(const AIP: Cardinal; const APort: Word; const AFileSize: Int64; const ASegmentSize: Integer;
  const AFileHash: TxdHash);
begin
  AddP2PSource( AIP, APort, ssSuccess );
  DoCheckFileSegmentInfo( AFileSize, ASegmentSize );
  if HashCompare(FFileHash, CtEmptyHash) then
    FFileHash := AFileHash;
end;
{$ENDIF}

procedure TxdP2SPDownTask.CalcWebHash;
var
  bCanCalc: Boolean;
begin
  if Active and FIsEmptyWebHash then
  begin
    EnterCriticalSection( FLockCalcWebHash );
    try
      if Active and FIsEmptyWebHash then
      begin
        case FSegmentTable.SegmentCount of
          1: bCanCalc := (PSegmentInfo(FSegmentTable.SegmentList[0])^.FSegmentState = ssCompleted);
          2: bCanCalc := (PSegmentInfo(FSegmentTable.SegmentList[0])^.FSegmentState = ssCompleted) and
                         (PSegmentInfo(FSegmentTable.SegmentList[1])^.FSegmentState = ssCompleted);
          else
             bCanCalc := (PSegmentInfo(FSegmentTable.SegmentList[0])^.FSegmentState = ssCompleted) and
                      (PSegmentInfo(FSegmentTable.SegmentList[FSegmentTable.SegmentCount div 2])^.FSegmentState = ssCompleted) and
                      (PSegmentInfo(FSegmentTable.SegmentList[FSegmentTable.SegmentCount - 1])^.FSegmentState = ssCompleted);
        end;
        if bCanCalc then
          CalcFileWebHash( FFileStream, FWebHash, nil )
        else
          Exit;
        FIsEmptyWebHash := HashCompare( FWebHash, CtEmptyHash );
        if not FIsEmptyWebHash then
        begin
          OutputDebugString( PChar(HashToStr(FWebHash)) );
        end
        else
          OutputDebugString( 'WEB HASH 值 为空' );
      end;
    finally
      LeaveCriticalSection( FLockCalcWebHash );
    end;
  end;
end;

procedure TxdP2SPDownTask.CheckFileStream;
begin
  if Assigned(FSegmentTable) and not Assigned(FFileStream) then
  begin
    FFileStream := StreamManage.CreateFileStream(FFileName, FSegmentTable);    
    FFileStream.SetFileHash( FFileHash );
    FFileStream.RenameFileName := FSaveAsFileName;
  end;
end;

procedure TxdP2SPDownTask.CheckHttpDownThreadControl;
var
  i, nCount: Integer;
  obj: TThreadCheck;
begin
  nCount := FHttpSourceList.LockList.Count;
  FHttpSourceList.UnlockList;
  if nCount = 0 then Exit;
  if not Assigned(FSegmentTable) and not FHttpGettingFileSize then
  begin
    FHttpGettingFileSize := True;
    RunningByThread( DoThreadToGetFileSizeByHttp );
    Exit;
  end;
  nCount := HttpThreadCount - FHttpThreadList.Count;
  for i := 0 to nCount - 1 do
  begin
    obj := TThreadCheck.Create( DownFileDataByHttp, 10 );
    FHttpThreadList.Add( obj );
  end;
end;

procedure TxdP2SPDownTask.ClearHttpThread;
var
  i: Integer;
  obj: TThreadCheck;
begin
  for i := 0 to FHttpThreadList.Count - 1 do
  begin
    obj := FHttpThreadList[i];
    obj.Free;
  end;
  FHttpThreadList.Clear;
end;

procedure TxdP2SPDownTask.ClearList(const AList: TThreadList);
var
  i: Integer;
  lt: TList;
begin
  lt := AList.LockList;
  try
    for i := 0 to lt.Count - 1 do
      Dispose( lt[i] );
    lt.Clear;
  finally
    AList.UnlockList;
  end;
end;

constructor TxdP2SPDownTask.Create;
begin
  FActive := False;
  FMaxRequestBlockCount := 8;
  FLastCalcSpeekTime := 0;
  FCalcRecvByte := 0;
  FMaxP2PSource := 10;
  FMaxSearchCount := 5;
  FIsEmptyWebHash := True;
  FFinished := False;
  FHttpThreadCount := 1;
  FP2PSourceList := TThreadList.Create;
  FHttpSourceList := TThreadList.Create;
  FHttpThreadList := TList.Create;
end;

procedure TxdP2SPDownTask.DeleteP2PSource(const AIP: Cardinal; const APort: Word);
var
  p: PP2PSource;
  i: Integer;
  lt: TList;
begin
  lt := FP2PSourceList.LockList;
  try
    for i := 0 to lt.Count - 1 do
    begin
      p := lt[i];
      if (p^.FIP = AIP) and (p^.FPort = APort) then
      begin
        lt.Delete( i );
        FreeP2PSourceMem( p );
        Break;
      end;
    end;
  finally
    FP2PSourceList.UnlockList;
  end;
end;

procedure TxdP2SPDownTask.DeleteP2PSourceFromList(Ap: PP2PSource);
var
  lt: TList;
  nIndex: Integer;
begin
  lt := FP2PSourceList.LockList;
  try
    nIndex := lt.IndexOf( Ap );
    if nIndex <> -1 then
    begin
      lt.Delete( nIndex );
//      Dispose( Ap );
    end;
  finally
    FP2PSourceList.UnlockList;
  end;
end;

destructor TxdP2SPDownTask.Destroy;
var
  i: Integer;
  p: PP2PSource;
  lt: TList;
begin
  Active := False;
  lt := FP2PSourceList.LockList;
  try
    for i := 0 to lt.Count - 1 do
    begin
      p := lt[i];
      FreeP2PSourceMem( p );
    end;
  finally
    FP2PSourceList.UnlockList;
  end;
  FreeAndNil( FP2PSourceList );
  
  ClearList( FHttpSourceList );
  FreeAndNil( FHttpSourceList );
  ClearHttpThread;
  FreeAndNil( FHttpThreadList );
//  FreeAndNil( FSegmentTable );
  inherited;
end;

procedure TxdP2SPDownTask.DoCheckFileSegmentInfo(const AFileSize: Int64; const ASegmentSize: Integer);
begin
  if not Assigned(FSegmentTable) then
  begin
    FSegmentTable := TxdFileSegmentTable.Create( AFileSize, ASegmentSize );
    FFileSize := FSegmentTable.FileSize;
    FFileSegmentSize := FSegmentTable.SegmentSize;
    RelationSegmentTableEvent;
  end
  else if FSegmentTable.FileSize <> AFileSize then
  begin
    if FSegmentTable.IsEmpty then
    begin
      FSegmentTable.Free;
      FSegmentTable := TxdFileSegmentTable.Create( AFileSize, ASegmentSize );
      FFileSize := FSegmentTable.FileSize;
      FFileSegmentSize := FSegmentTable.SegmentSize;
      RelationSegmentTableEvent;
    end;
  end;
  if Active then CheckFileStream;
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
    Move( FFileHash, cmd.FFileHash, CtHashSize );
    FLasCheckSegmentHashTime := GetTickCount;
    FUdp.SendBuffer( Ap^.FIP, Ap^.FPort, @Cmd, CtCmdGetFileSegmentHashInfoSize );
  end;
end;

procedure TxdP2SPDownTask.DoCheckSegmentTable;
begin
  if FLastCheckSegmentTableTime = 0 then
    FLastCheckSegmentTableTime := FLastExcuteTime;
  if FLastExcuteTime - FLastCheckSegmentTableTime > FSegmentTable.MaxWaitTime then
  begin
    //检查超时
    FSegmentTable.CheckDownReplyWaitTime;
    FLastCheckSegmentTableTime := GetTickCount;
  end;
end;

procedure TxdP2SPDownTask.DoCheckTaskSpeed;
var
  nTemp: Cardinal;
begin
  if FLastCalcSpeekTime = 0 then
    FLastCalcSpeekTime := GetTickCount
  else
  begin
    nTemp := GetTickCount;
    if nTemp - FLastCalcSpeekTime > 1000 then
    begin
      FCurSpeek := FCalcRecvByte div Integer(nTemp - FLastCalcSpeekTime);
      OutputDebugString( PChar('下载速度：' + FormatSpeek(FCurSpeek)) );
      FLastCalcSpeekTime := GetTickCount;
      InterlockedExchange( FCalcRecvByte, 0 );
    end;
  end;
end;

function TxdP2SPDownTask.DoCheckTaskSuccess: Boolean;
begin
  //文件下载完成
  Result := FSegmentTable.IsCompleted;
  if Result then
  begin
    //无文件HASH，表示单纯从HTTP下载
    if (HashCompare(CtEmptyHash, FFileHash)) or ((FLasCheckSegmentHashTime = 0) and HashCompare(FFileStream.CalcFileHash(nil), FFileHash)) then
    begin
      //文件下载成功
      DoDownSuccess;
    end
    else
    begin
      OutputDebugString( 'Hash 验证失败' );
      //一般情况很少需要用到分段HASH验证的
      if FCheckSegmentHashing then
      begin
        //计算自身分段信息
        DoCompareSegmentHash;
      end
      else
      begin
        //请求分段HASH
        DoCheckSegmentHash(nil);
      end;
    end;
  end;
end;

procedure TxdP2SPDownTask.DoCompareSegmentHash;
var
  i, j, nCount, nSegIndex: Integer;
  aryCalcSegHash: array of TxdHash;
  buf: PByte;
  nReadSize: Integer;
begin
  FCheckSegmentHashing := False;
  FLasCheckSegmentHashTime := 0;

  nCount := Length(FRecvSegmentHash);
  if nCount = 0 then Exit;
  SetLength( aryCalcSegHash, nCount );
  GetMem( buf, FCheckSegmentHashSize );
  try
    for i := 0 to nCount - 1 do
    begin
      if i = nCount - 1 then
        nReadSize := FFileSize - Cardinal(i) * FCheckSegmentHashSize
      else
        nReadSize := FCheckSegmentHashSize;
      FFileStream.ReadBuffer(Cardinal(i) * FCheckSegmentHashSize, nReadSize, buf);
      aryCalcSegHash[i] := HashBuffer( buf, nReadSize );
    end;
  finally
    FreeMem( buf, FCheckSegmentHashSize );
  end;

  for i := 0 to nCount - 1 do
  begin
    if not HashCompare(aryCalcSegHash[i], FRecvSegmentHash[i]) then
    begin
      //OutputDebugString( PChar('Hash 不相同: ' + IntToStr(i)) );
      nSegIndex := (Cardinal(i) * FCheckSegmentHashSize + FSegmentTable.SegmentSize - 1) div FSegmentTable.SegmentSize;
      for j := nSegIndex to nSegIndex + Integer((FCheckSegmentHashSize + FSegmentTable.SegmentSize - 1) div FSegmentTable.SegmentSize) do
        FSegmentTable.ResetSegment( j );
    end;
  end;
end;

procedure TxdP2SPDownTask.DoCompletedSegment(const ASegIndex: Integer);
begin
  if (ASegIndex = 0) or (ASegIndex = FSegmentTable.SegmentCount - 1) or (ASegIndex = FSegmentTable.SegmentCount div 2) then
    CalcWebHash;
end;

procedure TxdP2SPDownTask.DoDownSuccess;
begin
  ClearHttpThread;
  if HashCompare(CtEmptyHash, FFileHash) then
    FFileHash := FFileStream.CalcFileHash(nil);
  FFinished := True;
  if Assigned(FFileStream) then
  begin
    FFileStream.CompletedFile;
    StreamManage.ReleaseFileStream( FFileStream );
    FFileStream := nil;
  end;

  OutputDebugString( '文件下载成功' );
  OutputDebugString( PChar('web Hash: ' + HashToStr(FWebHash)) );
  OutputDebugString( PChar('File Hash: ' + HashToStr(FFileHash)) );
end;

procedure TxdP2SPDownTask.DoErrorInfo(const AInfo: string);
begin
  OutputDebugString( PChar(AInfo) );
end;

procedure TxdP2SPDownTask.DoExcuteDownTask;
const
  CtMinSpaceTime = 80;
var
  dwTime: Cardinal;
begin
  if not Active then Exit;
  dwTime := GetTickCount;
  if (dwTime > FLastExcuteTime) and (dwTime - FLastExcuteTime < CtMinSpaceTime) then Exit;
  FLastExcuteTime := dwTime;
  CheckHttpDownThreadControl;
  DownFileDataByP2P; 
  if not Assigned(FSegmentTable) then Exit;
  if DoCheckTaskSuccess then Exit;
  DoCheckTaskSpeed;  //计算速度
  DoCheckSegmentTable; //检查超时
end;


procedure TxdP2SPDownTask.DoInvalidP2PSource(Ap: PP2PSource);
begin
  OutputDebugString( PChar('无效的下载源：' + IpToStr(Ap^.FIP, Ap^.FPort)) );
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
  if ABufLen < CtMinPackageLen + CtHashSize + 1 then
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
      if (p^.FIP = AIP) and (p^.FPort = APort) then
      begin
        p^.FRequestBlockManage.FinishedBlock( pCmd^.FSegmentIndex, pCmd^.FBlockIndex );
        Inc( p^.FTotalRecvByteCount, pCmd^.FBufferLen );
        Break;
      end;
    end;
  finally
    FP2PSourceList.UnlockList;
  end;

  //写入文件
  FFileStream.WriteBlockBuffer( pCmd^.FSegmentIndex, pCmd^.FBlockIndex, @pCmd^.FBuffer, pCmd^.FBufferLen );
  InterlockedExchangeAdd( FCalcRecvByte, pCmd^.FBufferLen );
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
    Move( pCmd^.FSegmentHashs[i * CtHashSize], FRecvSegmentHash[i], CtHashSize );
  FCheckSegmentHashing := True;
end;

procedure TxdP2SPDownTask.DoRecvReplyFileExistsInfo(const AIP: Cardinal; const APort: Word; const ACmd: PCmdReplyFileExistsInfo);
var
  md: TxdHash;
begin
  if ACmd^.FHashStyle = hsWebHash then
    md := TxdHash(ACmd^.FFileHash)
  else
    md := TxdHash(ACmd^.FHash);
  if ACmd^.FReplySign = rsSuccess then
    AddServerSource( AIP, APort, ACmd^.FFileSize, ACmd^.FFileSegmentSize, md )
  else
    DeleteP2PSource( AIP, APort );
end;

procedure TxdP2SPDownTask.DoThreadToGetFileSizeByHttp;
  function  GetFileSizeByHttp(const AURL: string): Int64;
  var
    http: TIdHTTP;
  begin
    http := TIdHTTP.Create( nil );
    try
      http.Head( AURL );
      Result := http.Response.ContentLength;
      http.Disconnect;
      http.Free;
    except
      Result := 0;
      http.Free;
    end;
  end;
var
  p: PHttpSource;
  lt: TList;
  nSize: Int64;
begin
  lt := FHttpSourceList.LockList;
  try
    if lt.Count = 0 then
    begin
      FHttpGettingFileSize := False;
      Exit;
    end;
    p := lt[0];
  finally
    FHttpSourceList.UnlockList;
  end;
  nSize := GetFileSizeByHttp( p^.FUrl );
  DoCheckFileSegmentInfo( nSize, FFileSegmentSize );
  if Assigned(FSegmentTable) then
  begin
    //先下载计算WEB HASH 所需要的内容
    FSegmentTable.AddPriorityDownInfo( 0 );
    FSegmentTable.AddPriorityDownInfo( FSegmentTable.SegmentCount div 2 );
    FSegmentTable.AddPriorityDownInfo( FSegmentTable.SegmentCount - 1 );
  end;
  FHttpGettingFileSize := False;
end;

procedure TxdP2SPDownTask.DownFileDataByHttp;
var
  nSegIndex, nBlockIndex: Integer;
  http: TIdHTTP;
  nPos: Int64;
  nSize: Cardinal;
  p: PHttpSource;
  lt: TList;
  ms: TMemoryStream;
  bGetSegBuffer: Boolean;
begin
  lt := FHttpSourceList.LockList;
  try
    if lt.Count = 0 then Exit;
    FHttpLastUsedIndex := (FHttpLastUsedIndex + 1) mod lt.Count;
    p := lt[FHttpLastUsedIndex];
  finally
    FHttpSourceList.UnlockList;
  end;

  nPos := 0;
  nSize := 0;
  if not FSegmentTable.GetNeedMostSegmentInfo(nSegIndex) then
  begin
    if not FSegmentTable.GetNeedMostBlockInfo(nSegIndex, nBlockIndex) then Exit;
    FSegmentTable.GetBlockSize( nSegIndex, nBlockIndex, nPos, nSize );
    bGetSegBuffer := False;
  end
  else
  begin
    FSegmentTable.GetSegmentSize( nSegIndex, nPos, nSize );
    bGetSegBuffer := True;
  end;
  if nSize = 0 then Exit;

  http := TIdHTTP.Create( nil );
  ms := TMemoryStream.Create;
  try
    ms.Size := nSize;
    ms.Position := 0;
    with http do
    begin
      Request.Clear;
      Request.Referer := p^.FReferUrl;
      Request.ContentRangeStart := nPos;
      Request.ContentRangeEnd := nPos + nSize - 1;
      xdCookies := p^.FCookies;
    end;
    http.Get( p^.FUrl, ms );

    if (Cardinal(http.Response.ContentLength) = nSize) and (ms.Position = nSize) then
    begin
      InterlockedExchangeAdd( FCalcRecvByte, nSize );
      InterlockedExchangeAdd( p^.FTotalRecvByteCount, nSize );
      if bGetSegBuffer then
        FFileStream.WriteSegmentBuffer( nSegIndex, ms.Memory, nSize )
      else
        FFileStream.WriteBlockBuffer( nSegIndex, nBlockIndex, ms.Memory, nSize );
    end;
  finally
    FreeAndNil( ms );
    FreeAndNil( http );
  end;
end;

procedure TxdP2SPDownTask.DownFileDataByP2P;
var
  i, nCount: Integer;
  p: PP2PSource;
  lt: TList;
  oSendStream: TxdStaticMemory_2K;
  nCountPos, nRequestCount: Integer;
  bOK, bGetMostNeedBlock: Boolean;
  nSegIndex, nBlockIndex, nLen, nBlockSize: Integer;

  function FindOkP2PSource: Boolean;
  const
    CtMinSpaceTime = 80;
  var
    i: Integer;
    dwCurTime: Cardinal;
  begin
    Result := False;
    dwCurTime := GetTickCount;
    for i := 0 to lt.Count - 1 do
    begin
      FP2PLastUsedIndex := (FP2PLastUsedIndex + 1) mod lt.Count;
      p := lt[FP2PLastUsedIndex];
      if p^.FState = ssSuccess then
      begin
        if Assigned(p^.FRequestBlockManage) and (dwCurTime - p^.FRequestBlockManage.LastRequestTime >= CtMinSpaceTime) then
        begin
          Result := True;
          Break;
        end;
      end
      else
        GetFileInfoByP2P( p );
    end;
  end;

begin
  //计算用户可以申请的分块数量
  p := nil;
  bOK := False;
  lt := FP2PSourceList.LockList;
  try
    if lt.Count > 0 then
    begin
      //
      //向指定用户请求数据
      if FindOkP2PSource and Assigned(FSegmentTable) then
      begin
        bOK := True;
        bGetMostNeedBlock := IsCanGetMostNeedBlock(p);
        nCount := GetMaxBlockCount( p );
        if (nCount > 0) and (nCount <= CtMaxRequestFileBlockCount) then
        begin
          oSendStream := TxdStaticMemory_2K.Create;
          try
            //封装数据包
            begin
              oSendStream.Clear;
              FUdp.AddCmdHead(oSendStream, CtCmd_RequestFileData);
              oSendStream.WriteLong(FFileHash, CtHashSize);
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
                    Inc(nRequestCount);
                    oSendStream.WriteInteger(nSegIndex);
                    oSendStream.WriteWord(nBlockIndex);

                    if p^.FRequestBlockManage.FinishedCount > 0 then
                      p^.FRequestBlockManage.Clear;
                    p^.FRequestBlockManage.AddRequestBlock( nSegIndex, nBlockIndex );
                  end;
                end;
              end;
              if nRequestCount > 0 then
              begin
                nLen := oSendStream.Position;
                oSendStream.Position := nCountPos;
                oSendStream.WriteWord( Word(nRequestCount) );
      //          OutputDebugString( Pchar(Format( '向%s请求%d块数据', [IpToStr(p^.FUserIP, p^.FUserPort), nRequestCount])) );
                FUdp.SendBuffer(p^.FIP, p^.FPort, oSendStream.Memory, nLen);
              end;
            end;
          finally
            oSendStream.Free;
          end;
        end;
      end;
      //结束 向指定用户请求数据
    end;
  finally
    FP2PSourceList.UnlockList;
  end;

  if not bOK or not Assigned(FSegmentTable) then
    GetFileInfoByP2P( p )
end;

procedure TxdP2SPDownTask.FreeP2PSourceMem(var p: PP2PSource);
begin
  if Assigned(p) then
  begin
    if Assigned(p^.FRequestBlockManage) then
      p^.FRequestBlockManage.Free;
    if Assigned(p^.FSegTableState) then
      p^.FSegTableState.Free;
    Dispose( p );
    p := nil;
  end;
end;

function TxdP2SPDownTask.GetCurHttpThreadCount: Integer;
begin
  Result := FHttpThreadList.Count;
end;

function TxdP2SPDownTask.GetCurP2PSourceCount: Integer;
begin
  Result := FP2PSourceList.LockList.Count;
  FP2PSourceList.UnlockList;
end;

procedure TxdP2SPDownTask.GetFileInfoByP2P(p: PP2PSource);
  procedure SendCheckFileCmd;
  var
    cmd: TCmdFileExistsInfo;
  begin
    FUdp.AddCmdHead( @Cmd, CtCmd_FileExists );
    if not HashCompare(FileHash, CtEmptyHash) then
    begin
      cmd.FHashStyle := hsFileHash;
      Move( FileHash, Cmd.FHash, CtHashSize );
    end
    else if not HashCompare(WebHash, CtEmptyHash) then
    begin
      cmd.FHashStyle := hsWebHash;
      Move( WebHash, Cmd.FHash, CtHashSize );
    end
    else
      Exit;
    p^.FState := ssChecking;
    p^.FLastCheckStateTime := GetTickCount;
    Inc( p^.FTimeoutCount );
    FUdp.SendBuffer( p^.FIP, p^.FPort, PAnsiChar(@cmd), CtCmdFileExistsInfoSize );
  end;
const
  CtReTryTimeSpace = 1000 * 5;
begin
  if not Assigned(p) then Exit;
  case p^.FState of
    ssUnkown: SendCheckFileCmd;
    ssChecking:
    begin
      if p^.FTimeoutCount > 10 then
      begin
        DeleteP2PSourceFromList( p );
        DoInvalidP2PSource( p );        
        FreeP2PSourceMem( p );
      end
      else if GetTickCount - p^.FLastCheckStateTime >= CtReTryTimeSpace then
        SendCheckFileCmd;
    end;
  end;
end;

function TxdP2SPDownTask.GetFileStreamID: Integer;
begin
  if Assigned(FFileStream) then
    Result := FFileStream.StreamID
  else
    Result := -1;
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

function TxdP2SPDownTask.GetHttpSourceCount: Integer;
begin
  Result := FHttpSourceList.LockList.Count;
  FHttpSourceList.UnlockList;
end;

function TxdP2SPDownTask.GetHttpSourceInfo(const AIndex: Integer; AInfo: PHttpSource): Boolean;
var
  p: PHttpSource;
  lt: TList;
begin
  Result := False;
  lt := FHttpSourceList.LockList;
  try
    if (AIndex >= 0) and (AIndex < lt.Count) then
    begin
      p := lt[AIndex];
      AInfo^.FUrl := p^.FUrl;
      AInfo^.FReferUrl := p^.FReferUrl;
      AInfo^.FCookies := p^.FCookies;
      AInfo^.FTotalRecvByteCount := p^.FTotalRecvByteCount;
    end;
  finally
    FHttpSourceList.UnlockList;
  end;
end;

function TxdP2SPDownTask.GetMaxBlockCount(p: PP2PSource): Integer;
begin
  if p^.FRequestBlockManage.RequestCount = 0 then
    Result := 1
  else
  begin
    if p^.FRequestBlockManage.RequestCount < p^.FRequestBlockManage.FinishedCount then
      Result := p^.FRequestBlockManage.FinishedCount
    else
      Result := p^.FRequestBlockManage.FinishedCount + 1;
  end;
//  OutputDebugString( PChar( '请求分块数量：' + IntToStr(Result)) );
end;

function TxdP2SPDownTask.GetP2PSourceInfo(const AIndex: Integer; AInfo: PP2PSource): Boolean;
var
  p: PP2PSource;
  lt: TList;
begin
  lt := FP2PSourceList.LockList;
  try
    Result := (AIndex >= 0) and (AIndex < lt.Count);
    if Result then
    begin
      p := lt[AIndex];
      Move( p^, AInfo^, CtP2PSourceSize );
    end;
  finally
    FP2PSourceList.UnlockList;
  end;
end;

{$IFNDEF ExclusionP2P}
function TxdP2SPDownTask.GetP2PSourceInfo(var AInfo: TAryServerInfo): Integer;
var
  lt: TList;
  i, j, nCount: Integer;
  p: PP2PSource;
  bAdd: Boolean;
begin
  nCount := Length( AInfo );
  Result := 0;
  lt := FP2PSourceList.LockList;
  try
    for i := 0 to lt.Count - 1 do
    begin
      p := lt[i];
      bAdd := True;
      for j := 0 to nCount - 1 do
      begin
        if (p^.FIP = AInfo[j].FServerIP) and (p^.FPort = AInfo[j].FServerPort) then
        begin
          bAdd := False;
          Break;
        end;
      end;
      if bAdd then
      begin
        SetLength( AInfo, nCount + Result + 1 );
        AInfo[nCount + Result].FServerStyle := srvFileShare;
        AInfo[nCount + Result].FServerID := 0;
        AInfo[nCount + Result].FServerIP := p^.FIP;
        AInfo[nCount + Result].FServerPort := p^.FPort;
        AInfo[nCount + Result].FTag := 0;
        Inc( Result );
      end;
    end;
  finally
    FP2PSourceList.UnlockList;
  end;
end;
{$ENDIF}

function TxdP2SPDownTask.IsCanGetMostNeedBlock(p: PP2PSource): Boolean;
begin
  Result := True;
end;

function TxdP2SPDownTask.IsExistsSource(const AUrl: string): Boolean;
var
  p: PHttpSource;
  lt: TList;
  i: Integer;
begin
  Result := False;
  if AUrl = '' then Exit;
  lt := FHttpSourceList.LockList;
  try
    for i := 0 to lt.Count - 1 do
    begin
      p := lt[i];
      if CompareText(AUrl, p^.FUrl) = 0 then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    FHttpSourceList.UnlockList;
  end;
end;

{$IFNDEF ExclusionP2P}
function TxdP2SPDownTask.IsExistsSource(const AIP: Cardinal; const APort: Word): Boolean;
var
  p: PP2PSource;
  lt: TList;
  i: Integer;
begin
  Result := False;
  lt := FP2PSourceList.LockList;
  try
    for i := 0 to lt.Count - 1 do
    begin
      p := lt[i];
      if (p^.FIP = AIP) and (p^.FPort = APort) then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    FP2PSourceList.UnlockList;
  end;
end;
{$ENDIF}

procedure TxdP2SPDownTask.RelationSegmentTableEvent;
begin
  if Assigned(FSegmentTable) then
  begin
    FSegmentTable.OnSegmentCompleted := DoCompletedSegment;
  end;
end;

procedure TxdP2SPDownTask.ResetP2PSourceStat;
var
  p: PP2PSource;
  lt: TList;
  i: Integer;
begin
  lt := FP2PSourceList.LockList;
  try
    for i := 0 to lt.Count - 1 do
    begin
      p := lt[i];
      p^.FRequestBlockManage.Clear;
      p^.FTimeoutCount := 0;
    end;
  finally
    FP2PSourceList.UnlockList;
  end;
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

function TxdP2SPDownTask.SetFileInfo(const ASaveFileName: string; const AFileHash: TxdHash; const AWebHash: TxdHash): Boolean;
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
    FWebHash := AWebHash;
    FIsEmptyWebHash := HashCompare( FWebHash, CtEmptyHash );
  end;
end;

procedure TxdP2SPDownTask.SetHttpThreadCount(const Value: Integer);
begin
  if (Value <> FHttpThreadCount) and (Value > 0) then
    FHttpThreadCount := Value;                         
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
    RelationSegmentTableEvent;
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
  ClearHttpThread;
  DeleteCriticalSection( FLockCalcWebHash );
  if Assigned(FFileStream) then
  begin
    StreamManage.ReleaseFileStream( FFileStream );
    FFileStream := nil;
  end;
  ResetP2PSourceStat;
  FCurSpeek := 0;
end;

{ TRequestBlockManage }

procedure TRequestBlockManage.AddRequestBlock(const ASegIndex, ABlockIndex: Integer);
begin
  FRequestBlockCount := Length(FBlocks);
  SetLength( FBlocks, FRequestBlockCount + 1 );
  FBlocks[FRequestBlockCount] := CalcID( ASegIndex, ABlockIndex );
  Inc( FRequestCount );
  Inc( FRequestBlockCount );
  FLastRequestTime := GetTickCount; 
end;

function TRequestBlockManage.CalcID(const ASegIndex, ABlockIndex: Integer): Cardinal;
begin
  Result := ASegIndex * 1000 + ABlockIndex;
end;

procedure TRequestBlockManage.Clear;
begin
  SetLength( FBlocks, 0 );
  FRequestBlockCount := 0;
  FRequestCount := 0;
  FFinishedCount := 0;
  FLastRequestTime := 0;
end;

procedure TRequestBlockManage.FinishedBlock(const ASegIndex, ABlockIndex: Integer);
var
  i: Integer;
begin
  for i := 0 to FRequestBlockCount - 1 do
  begin
    if FBlocks[i] = CalcID(ASegIndex, ABlockIndex) then
    begin
      FBlocks[i] := MAXDWORD;
      Inc( FFinishedCount );
      Break;
    end;
  end;
end;

end.
