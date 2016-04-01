{
单元名称: uJxdFileSegmentStream
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明:
开始时间: 2011-04-06
修改时间: 2011-04-06 (最后修改时间)
类说明  :
    文件分段下载表
    |-----------------------------|
    |          |        |         |
    |   分段1  |  分段2 | 分段N...|
    |          |        |         |
    |-----------------------------|
    分段内容
    分块1，分块2...分块n
    分段表：TxdFileSegmentTable
        线程安全
        对文件进行分段分块处理，并提供支持在线播放最优下载方式

    分段表提供给文件流使用，文件流将用于：下载（P2P，HTTP，FTP），上传（P2P），在线播放

    文件流基类：TxdP2SPFileStreamBasic
        本地文件流操作： TxdLocalFileStream，只提供读操作
        下载上传文件流： TxdFileSegmentStream，提供多种接口，以便不同的应用
    TxdFileSegmentStream提供的接口, 类不创建线程
    下载需要接口：GetEmptySegmentInfo, 得到一块需要下载的位置信息, 向提供者请求数据，用于HTTP，FTP，BT等下载
                  GetEmptyBlockInfo, 得到一小块需要下载的位置信息，向提供者请求数据，用于P2SP下载
                  WriteBlockBuffer, 得到小分块数据之后，写入指定位置
                  WriteSegmentBuffer, 得到分段数据之后，写入指定位置
                  CheckSegmentHashInfo, 判断已经完成的分段信息HASH值是否正确，不正确则此分块重新下载
    上传需要接口：GetFinishedSegmentInfo, 得到此时已经完成的所有分段的序号， Finished指数据已通过Hash检验
                  ReadBlockBuffer, 得到指定的已经完成小分块的数据
    在线播放接口：CurPlayPostion, 当前播放位置
                  ReadPlayBuffer, 获取当前要播放的数据

    TxdFileStreamManage对需要用到的 TxdP2SPFileStreamBasic 进行管理，外部一般不需要自己去创建 TxdFileSegmentStream，
        统一由 TxdFileStreamManage 进行创建和管理。
        自动创建需要的流，并且在不需要的时候自动释放（如果是由管理者创建的话），管理者中的流具体为
            TxdLocalFileStream
            TxdFileSegmentStream
    TxdFileStreamManage在本单元自动创建 FileStreamManage 对象，外部直接引用，不能对其进行释放，由本单元自行释放
}
unit uJxdFileSegmentStream;

interface

uses
  Windows, Classes, SysUtils, uJxdHashCalc, uSysSub, uJxdDataStream, uJxdMemoryManage, uJxdUdpIOHandle, uJxdCmdDefine;

{$I JxdUdpOpt.inc}

type

  _TOnCheckPosInfo = procedure(const ASegmentIndex, ABlockIndex: Integer; var AIsContinue: Boolean; AData: Pointer) of object;
  TSegmentState = (ssEmpty, ssFullSegment, ssBlockSegment, ssCompleted);
  TBlockState = (bsEmpty, bsWaitReply, bsComplete);

  {文件分段表结构信息}
  PSegmentInfo = ^TSegmentInfo;
  TSegmentInfo = record
    FSegmentIndex: Integer;  //分段序号
    FSegmentBeginPos: Int64; //分段开始位置
    FSegmentSize: Cardinal;  //分段大小
    FSegmentState: TSegmentState; //分段状态
    FSegmentActiveTime: Cardinal; //当前分段激动时间
    FBlockCount: Integer; //分块总数量
    FRecvBlockCount: Integer; //当前已经收到的分块数量
    FBlockState: array[0..0] of TBlockState; //每一分块的具体状态
  end;

  {最优先下载表结构信息}
  PPriorityTableInfo = ^TPriorityTableInfo;
  TPriorityTableInfo = record
    FPrioritySegmentIndex: Integer; //优先下载分段序号
    FPriorityBlockIndex: Integer;   //优先下载分段中的分块序号，< 0时表示：优先下载整个分段
  end;

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ///                                      分段状态信息表
  ///  整个文件的分段信息状态管理，
  ///  每一位的0与1表示对应分段是否已经拥有数据
  ///  方便网络的传输
  ///
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  {$M+}
  TxdSegmentStateTable = class
  public
    constructor Create; overload;
    constructor Create(const ASegmentCount: Integer; ABitsBuf: Pointer = nil; ALen: Integer = 0); overload;

    destructor Destroy; override;
    procedure MakeByMem(const ASegmentCount: Integer; ABitsBuf: Pointer = nil; ALen: Integer = 0);
  private
    FBits: Pointer;
    FBitsMemLen: Integer;
    FSize: Integer;
    procedure Error;
    function  GetBit(Index: Integer): Boolean;
    procedure SetBit(Index: Integer; const Value: Boolean);
    procedure SetSize(const Value: Integer);
  public
    property BitMemLen: Integer read FBitsMemLen;
    property BitMem: Pointer read FBits;

    property SegmentCompleted[Index: Integer]: Boolean read GetBit write SetBit; default;
    property Size: Integer read FSize write SetSize;
  end;
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ///                                      文件分段表
  ///  对文件进行虚拟分段，包括大段跟小分块
  ///
  ///
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TOnSegmentCompleted = procedure(const ASegIndex: Integer) of object;
  TxdFileSegmentTable = class
  public
    constructor Create(const AFileSize: Int64; const ASegmentSize: Cardinal = 0); //(CtMaxCombiPackageSize - CtCmdTransmitFileInfoSize) * 64
    destructor  Destroy; override;

    //初始化分段文件信息
    //由外部进行初始化，提供SegmentList属性, 外部初始化之后必须调用CheckSegmentTable进行同步
    procedure CheckSegmentTable;
    function  CheckCanRead(const APosition: Int64; const ASize: Cardinal): Boolean; //判断指定位置是否可读

    //添加需要优先下载的信息, 可多次设置, APrioritySize = 0时，表示优先下载APriorityBeginPos所在的所有分段数据
    procedure AddPriorityDownInfo(const APriorityBeginPos: Int64; const APrioritySize: Cardinal); overload; //优先级最高
    procedure AddPriorityDownInfo(const ASegmentIndex: Integer); overload;

    //获取当前最需要下载的信息, 函数不拆分
    function GetNeedMostSegmentInfo(var ASegmentIndex: Integer): Boolean; //可提供给HTTP，FTP等
    function GetNeedMostBlockInfo(var ASegmentIndex, ABlockIndex: Integer): Boolean; //可提供给P2S
    function GetP2PBlockInfo(var ASegmentIndex, ABlockIndex: Integer; ASegTableState: TxdSegmentStateTable): Boolean; //可提供给：P2P 在指定分段中查找

    //获取指定位置信息
    function GetSegmentSize(const ASegmentIndex: Integer; var ABeginPos: Int64; var ASize: Cardinal): Boolean; inline;
    function GetBlockSize(const ASegmentIndex, ABlockIndex: Integer; var ABeginPos: Int64; var ASize: Cardinal): Boolean; overload; inline;
    function GetBlockSize(const ASegmentIndex, ABlockIndex: Integer; var ASize: Integer): Boolean; overload; inline; 

    //完成数据
    function CompletedSegment(const ASegmentIndex: Integer; const ASize: Cardinal): Boolean;
    function CompletedBlock(const ASegmentIndex, ABlockIndex: Integer; const ASize: Cardinal): Boolean;
    //判断
    procedure CheckDownReplyWaitTime; //判断下载请求是否超时
    procedure ResetSegment(const ASegIndex: Integer); //把指定分段的状态重置为空，重新下载
    function  IsEmpty: Boolean;
  protected
    procedure CheckLastCompleteSegmentIndex;
    procedure PackedPriorityList; //对优先下载的分段信息进行整理
    procedure DoCheckPosInfo(const APos: Int64; const ASize: Cardinal; ACheckSub: _TOnCheckPosInfo; AData: Pointer);
    procedure DoCheckToAddList(const ASegmentIndex, ABlockIndex: Integer; var AIsContinue: Boolean; AData: Pointer);
    procedure DoCheckCanRead(const ASegmentIndex, ABlockIndex: Integer; var AIsContinue: Boolean; AData: Pointer);
  private
    FCanRead: Boolean;
    FLock: TRTLCriticalSection;
    FSegmentList: TList;
    FPriorityList: TList;
    FSegmentMen: TxdFixedMemoryManager;
    FSegmentStateTable: TxdSegmentStateTable;
    FInvalideBufferSize: Cardinal;
    FFileSize: Int64;
    FSegmentCount: Integer;
    FBlockMaxCount: Integer;
    FSegmentSize: Cardinal;
    FPriorityDownSegmentIndex: Integer;
    FLastFinishedSegmentIndex: Integer;
    FMaxWaitTime: Cardinal;
    FIsCompleted: Boolean;
    FOnSegmentCompleted: TOnSegmentCompleted;
    FCompletedFileSize: Int64;
  published
    property SegmentStateTable: TxdSegmentStateTable read FSegmentStateTable; //分段状态表，提供给其它用户选择下载使用，每一位表示一个分段状态
    property IsCompleted: Boolean read FIsCompleted; //是否是已经完成的分段信息表
    property LastFinishedSegmentIndex: Integer read FLastFinishedSegmentIndex; //已完成连续分段的最后一个分段序号 
    property PriorityDownSegmentIndex: Integer read FPriorityDownSegmentIndex; //优先下载开始分段序号
    property SegmentList: TList read FSegmentList; //分段信息列表
    property FileSize: Int64 read FFileSize; //文件大小
    property SegmentSize: Cardinal read FSegmentSize; //分段大小
    property SegmentCount: Integer read FSegmentCount; //分段数量
    property BlockMaxCount: Integer read FBlockMaxCount; //每个分段最大分块数量, 其中分块的大小为编译时确定
    property InvalideBufferSize: Cardinal read FInvalideBufferSize; //接收到无效或已经不需要的数据长度
    property CompletedFileSize: Int64 read FCompletedFileSize; //已经完成的字节数 

    property MaxWaitTime: Cardinal read FMaxWaitTime write FMaxWaitTime; //等待允许最长时间, 超过则表示等待超时

    property OnSegmentCompleted: TOnSegmentCompleted read FOnSegmentCompleted write FOnSegmentCompleted; //某一分段下载完成 
  end;


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ///                                      使用文件分段表的文件流
  ///  根据文件分段表的信息对文件内容进行存取
  ///
  ///
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //文件流基类，只提供接口
  TxdP2SPFileStreamBasic = class
  public
    destructor  Destroy; override;
    function ReadBuffer(const APos: Int64; const ASize: Integer; ABuffer: PByte): Boolean; virtual; abstract;
    function ReadBlockBuffer(const ASegmentIndex, ABlockIndex: Integer; const ABuffer: PByte; var ABufLen: Integer): Boolean; virtual; abstract;
  protected
    FSegmentSize: Integer;
    FStreamID: Integer;
    FFileHash: TxdHash;
    FFileName: string;
    FFileSize: Int64;
    FIsComplete, FIsOnlyRead: Boolean;
  private
    FRenameFileName: string;
    procedure SetRenameFileName(const Value: string);
  published
    property FileHash: TxdHash read FFileHash;
    property FileName: string read FFileName;
    property RenameFileName: string read FRenameFileName write SetRenameFileName; 
    property FileSize: Int64 read FFileSize;
    property StreamID: Integer read FStreamID;
    property IsComplete: Boolean read FIsComplete;
    property IsOnlyRead: Boolean read FIsOnlyRead;
    property SegmentSize: Integer read FSegmentSize;
  end;

  {一个只读内存映射文件的内存读操作，以适合多线程读取操作而不需要锁定}
  TxdMemReadFile = class(TxdMemoryFile)
  public
    procedure ReadMemory(const APos: Int64; const ASize: Integer; ABuffer: PByte);
  end;
  {一个本地文件的流操作, 只提供文件的读操作}
  TxdLocalFileStream = class(TxdP2SPFileStreamBasic)
  public
    constructor Create(const AFileName: string; const AFileHash: TxdHash; const ASegmentSize: Integer = 0);
    destructor  Destroy; override;

    function ReadBuffer(const APos: Int64; const ASize: Integer; ABuffer: PByte): Boolean; override;
    function ReadBlockBuffer(const ASegmentIndex, ABlockIndex: Integer; const ABuffer: PByte; var ABufLen: Integer): Boolean; override;
  private
    FSegmentCount: Integer;
    FBlockMaxCount: Integer;
    FFileStream: TxdMemReadFile;
  end;

  {文件分段流, 边下载边上传使用, 可进行读跟写，适合下载文件时使用}
  TxdFileSegmentStream = class(TxdP2SPFileStreamBasic)
  public
    constructor Create(AFileStream: TxdMemoryFile; ASegmentTable: TxdFileSegmentTable);
    destructor  Destroy; override;

    procedure SetFileHash(const AHash: TxdHash);
    function  CalcFileHash(PAbort: PBoolean): TxdHash;

    procedure CompletedFile;

    //直接写入一段内存
    function WriteSegmentBuffer(const ASegmentIndex: Integer; const ABuffer: PByte; const ABufLen: Integer): Boolean;
    function WriteBlockBuffer(const ASegmentIndex, ABlockIndex: Integer; const ABuffer: PByte; const ABufLen: Integer): Boolean;
    //读取指定位置
    function ReadBuffer(const APos: Int64; const ASize: Integer; ABuffer: PByte): Boolean; override;
    function ReadBlockBuffer(const ASegmentIndex, ABlockIndex: Integer; const ABuffer: PByte; var ABufLen: Integer): Boolean; override;
  private
    FAutoFreeStream, FAutoFreeSegmentTable: Boolean;  //给同单元外部调用
    
    FLock: TRTLCriticalSection;
    FFileStream: TxdMemoryFile; //内存映射文件
    FSegmentTable: TxdFileSegmentTable;
    procedure LockStream; inline;
    procedure UnLockStream; inline;
  end;

  {TxdFileStreamManage, 对文件的创建，查找，释放问题进行操作}
  PStreamManageInfo = ^TStreamManageInfo;
  TStreamManageInfo = record
    FStream: TxdP2SPFileStreamBasic;
    FCount: Integer;
  end;
  TxdFileStreamManage = class
  public
    constructor Create;
    destructor  Destroy; override;

    //查找流，当返回的流有效时，引用自动加1，在用完流之后，必须调用 ReleaseFileSegmentStream 函数
    procedure ReleaseFileStream(const AStream: TxdP2SPFileStreamBasic);
    function  QueryFileStream(const AFileHash: TxdHash): TxdP2SPFileStreamBasic; overload;
    function  QueryFileStream(const AStreamID: Integer): TxdP2SPFileStreamBasic; overload;
    function  CreateFileStream(const AFileName: string; const AFileHash: TxdHash; const ASegmentSize: Integer = 0): TxdLocalFileStream; overload;
    function  CreateFileStream(const AFileName: string; const ASegmentTable: TxdFileSegmentTable): TxdFileSegmentStream; overload;
  private
    FCurStreamID: Integer;
    FLockManage: TRTLCriticalSection;
    FManageList: TList; //暂时先用LIST，服务器端可使用THashArray
    procedure LockManage; inline;
    procedure UnLockManage; inline;
  end;
  {$M-}

var
  StreamManage: TxdFileStreamManage;

{限制}
const
  //CtBlockSize 的定义将影响发包，将分为单包发送或组合包
  CtSegmentDefaultSize = 256 * 1024; //默认分段大小256K

{$IFDEF SendFileBySinglePackage}
  CtBlockSize = CtSinglePackageSize - CtCmdReplyRequestFileInfoSize; //最大单包中的数据长度
{$ELSE}
  CtBlockSize = CtMaxCombiPackageSize - CtCmdReplyRequestFileInfoSize; //最大可发送组合包中的数据长度
{$ENDIF}
//
  CtSegmentStateSize = SizeOf(TSegmentState);
  CtBlockStateSize = SizeOf(TBlockState);

function GetSegmentStateString(const AState: TSegmentState): string;
function GetBlockStateString(const AState: TBlockState): string;

//计算文件HASH值
function CalcFileHash(AStream: TxdP2SPFileStreamBasic; var AHash: TxdHash; PAbort: PBoolean = nil): Boolean;
//计算文件的WEB HASH值
function CalcFileWebHash(AStream: TxdP2SPFileStreamBasic; var AHash: TxdHash; PAbort: PBoolean = nil): Boolean;

implementation


function CalcFileHash(AStream: TxdP2SPFileStreamBasic; var AHash: TxdHash; PAbort: PBoolean): Boolean;
var
  Context: TxdHashContext;
  Buffer: array[0..16383] of Byte;
  ReadBytes: Integer;
  nPos: Int64;

  function CheckSize: Boolean;
  begin
    if nPos + ReadBytes > AStream.FileSize then
      ReadBytes := AStream.FileSize - nPos;
    Result := ReadBytes > 0;
  end;

begin
  Result := True;
  HashInit(Context);
  nPos := 0;
  ReadBytes := SizeOf(Buffer);
  while CheckSize and AStream.ReadBuffer(nPos, ReadBytes, @Buffer) do
  begin
    HashUpdate(Context, @Buffer, ReadBytes);
    if Assigned(PAbort) and PAbort^ then
    begin
      Result := False;
      Exit;
    end;
    nPos := nPos + ReadBytes;
  end;
  HashFinal(Context, AHash);
end;

function CalcFileWebHash(AStream: TxdP2SPFileStreamBasic; var AHash: TxdHash; PAbort: PBoolean): Boolean;

type
  TCalcSegHashInfo = record
    FPos: Int64;
    FSize: Integer;
    FSegHash: TxdHash;
  end;
var
  CalsHashBuffer: array[0..16 * 3 - 1] of Byte;
  i, nCount, nMaxSegCount, nSeg: Integer;
  nCalcPos: Integer;
  SegHash: array of TCalcSegHashInfo;


  //
  function CalcSegHash(var ASegInfo: TCalcSegHashInfo): Boolean;
  var
    Context: TxdHashContext;
    Buffer: array[0..16383] of Byte;
    ReadBytes: Integer;
    nPos, nCalcSize: Int64;

    function CheckSize: Boolean;
    begin
      if nPos + ReadBytes > nCalcSize then
        ReadBytes := nCalcSize - nPos;
      Result := ReadBytes > 0;
      if not Result then
        OutputDebugString( 'xxx' );
    end;

  begin
    Result := True;
    nCalcSize := ASegInfo.FPos + ASegInfo.FSize;
    HashInit(Context);
    nPos := ASegInfo.FPos;
    ReadBytes := SizeOf(Buffer);
    while CheckSize do
    begin
      if not AStream.ReadBuffer(nPos, ReadBytes, @Buffer) then
      begin
        Result := False;
        Break;
      end;
      HashUpdate(Context, @Buffer, ReadBytes);
      if Assigned(PAbort) and PAbort^ then
      begin
        Result := False;
        Exit;
      end;
      nPos := nPos + ReadBytes;
    end;
    HashFinal(Context, ASegInfo.FSegHash);
  end;


begin
  OutputDebugString( PChar(AStream.FileName) );
  nMaxSegCount := (AStream.FileSize + CtSegmentDefaultSize - 1) div CtSegmentDefaultSize;
  if nMaxSegCount > 3 then
    nCount := 3
  else
    nCount := nMaxSegCount;
  SetLength( SegHash, nCount );
  nCalcPos := 0;
  for i := 0 to nCount - 1 do
  begin
    //计算3块分块，头部(255K) 中间(<=255K) 最后(<=255K)
    case i of
      0: nSeg := 0;
      1: nSeg := nMaxSegCount div 2;
      else
        nSeg := nMaxSegCount - 1;  
    end;
    SegHash[i].FPos := Int64(nSeg) * CtSegmentDefaultSize;
    if AStream.FileSize - SegHash[i].FPos >= CtSegmentDefaultSize then
      SegHash[i].FSize := CtSegmentDefaultSize
    else
      SegHash[i].FSize := AStream.FileSize - SegHash[i].FPos;

    if not CalcSegHash( SegHash[i] ) then
    begin
      nCalcPos := 0;
      Break;
    end;
    Move( SegHash[i].FSegHash.v[0], CalsHashBuffer[nCalcPos], CtHashSize );
    Inc( nCalcPos, CtHashSize );
  end;
  Result := (nCalcPos > 0);
  if Assigned(PAbort) and PAbort^ then Exit;
  if Result then
    AHash := HashBuffer( @CalsHashBuffer, nCalcPos );
end;

{ TxdFileSegmentTable }

function GetSegmentStateString(const AState: TSegmentState): string;
begin
  case AState of
    ssEmpty: Result := 'ssEmpty';
    ssFullSegment: Result := 'FullSegment';
    ssBlockSegment: Result := 'BlockSegment';
    ssCompleted: Result := 'Completed';
  end;
end;
function GetBlockStateString(const AState: TBlockState): string;
begin
  case AState of
    bsEmpty: Result := 'bsEmpty';
    bsWaitReply: Result := 'bsWaitReply';
    bsComplete: Result := 'bsComplete';
  end;
end;

procedure TxdFileSegmentTable.AddPriorityDownInfo(const APriorityBeginPos: Int64; const APrioritySize: Cardinal);
begin
//  OutputDebugString( PChar('添加优先位置：' + IntToStr(APriorityBeginPos) + ' 大小：' + IntToStr(APrioritySize)) );
  DoCheckPosInfo( APriorityBeginPos, APrioritySize, DoCheckToAddList, nil );
//  PackedPriorityList;
end;

procedure TxdFileSegmentTable.AddPriorityDownInfo(const ASegmentIndex: Integer);
var
  b: Boolean;
begin
  if (ASegmentIndex >= 0) and (ASegmentIndex < FSegmentCount) then
    DoCheckToAddList( ASegmentIndex, -1, b, nil );
end;

function TxdFileSegmentTable.CheckCanRead(const APosition: Int64; const ASize: Cardinal): Boolean;
begin
  FCanRead := True;
  DoCheckPosInfo(APosition, ASize, DoCheckCanRead, nil);
  Result := FCanRead;
end;

procedure TxdFileSegmentTable.CheckDownReplyWaitTime;
var
  i, j: Integer;
  p: PSegmentInfo;
  nCurTime: Cardinal;
begin
  nCurTime := GetTickCount;
  for i := FLastFinishedSegmentIndex to FSegmentCount - 1 do
  begin
    p := FSegmentList[i];
    if p^.FSegmentState in [ssFullSegment, ssBlockSegment] then
    begin
      if nCurTime - p^.FSegmentActiveTime > MaxWaitTime then
      begin
        if p^.FSegmentState = ssFullSegment then
        begin
          p^.FSegmentActiveTime := 0;
          p^.FSegmentState := ssEmpty;
        end
        else
        begin
          for j := 0 to p^.FBlockCount - 1 do
          begin
            if p^.FBlockState[j] = bsWaitReply then
              p^.FBlockState[j] := bsEmpty;
          end;
          p^.FSegmentActiveTime := GetTickCount;
        end;
      end;
    end;
  end;
end;

procedure TxdFileSegmentTable.CheckLastCompleteSegmentIndex;
var
  i: Integer;
  p: PSegmentInfo;
begin
  for i := FLastFinishedSegmentIndex to FSegmentCount - 1 do
  begin
    p := FSegmentList[i];
    if p^.FSegmentState = ssCompleted then
    begin
      Inc( FLastFinishedSegmentIndex );
      if FLastFinishedSegmentIndex = FSegmentCount then
      begin
        FIsCompleted := True;
        Break;
      end;
    end
    else
      Break;
  end;
end;

procedure TxdFileSegmentTable.CheckSegmentTable;
var
  i, j: Integer;
  p: PSegmentInfo;
  nPos: Int64;
  nSize: Cardinal;
begin
  EnterCriticalSection( FLock );
  try
    FLastFinishedSegmentIndex := 0;
    FCompletedFileSize := 0;
    FIsCompleted := False;
    for i := 0 to FSegmentCount - 1 do
    begin
      p := FSegmentList[i];
      FSegmentStateTable.SegmentCompleted[i] := p^.FSegmentState = ssCompleted;
      if (p^.FSegmentState = ssCompleted) then
      begin
        FCompletedFileSize := FCompletedFileSize + p^.FSegmentSize;
        if (FLastFinishedSegmentIndex = i) or (FLastFinishedSegmentIndex = i - 1) then
        begin
          Inc( FLastFinishedSegmentIndex );
          if FLastFinishedSegmentIndex = FSegmentCount - 1 then
          begin
            p := FSegmentList[FLastFinishedSegmentIndex];
            FIsCompleted := p^.FSegmentState = ssCompleted;
          end;
        end;
      end
      else if p^.FSegmentState = ssBlockSegment then
      begin
        for j := 0 to p^.FBlockCount - 1 do
        begin
          if p^.FBlockState[j] = bsComplete then
          begin
            GetBlockSize( i, j, nPos, nSize );
            FCompletedFileSize := FCompletedFileSize + nSize;
          end;
        end;
      end;
    end;
  finally
    LeaveCriticalSection( FLock );
  end;
end;

function TxdFileSegmentTable.CompletedBlock(const ASegmentIndex, ABlockIndex: Integer; const ASize: Cardinal): Boolean;
var
  p: PSegmentInfo;
  nPos: Int64;
  nSize: Cardinal;
  i: Integer;
  bSegFinished: Boolean;
  pPriority: PPriorityTableInfo;
begin
  Result := False;
  EnterCriticalSection( FLock );
  try
    if (ASegmentIndex < 0) or (ASegmentIndex >= FSegmentCount) then
    begin
      FInvalideBufferSize := FInvalideBufferSize + ASize;
      Exit;
    end;
    p := FSegmentList[ASegmentIndex];
    if ABlockIndex >= p^.FBlockCount then
    begin
      FInvalideBufferSize := FInvalideBufferSize + ASize;
      Exit;
    end;
    Result := GetBlockSize( ASegmentIndex, ABlockIndex, nPos, nSize ) and (p^.FBlockState[ABlockIndex] <> bsComplete);
    if Result then
    begin
      p^.FBlockState[ABlockIndex] := bsComplete;
      FCompletedFileSize := FCompletedFileSize + nSize;
      bSegFinished := True;
      for i := 0 to p^.FBlockCount - 1 do
      begin
        if p^.FBlockState[i] <> bsComplete then
        begin
          bSegFinished := False;
          Break;
        end;
      end;
      if bSegFinished then
      begin
        //此分段已经完成
        p^.FSegmentState := ssCompleted;
        FSegmentStateTable[ASegmentIndex] := True;
        for i := FPriorityList.Count - 1 downto 0 do
        begin
          pPriority := FPriorityList[i];
          if pPriority^.FPrioritySegmentIndex = ASegmentIndex then
          begin
            Dispose( pPriority );
            FPriorityList.Delete(i);
          end;
        end;
        CheckLastCompleteSegmentIndex;
      end
      else
      begin
        //只完成某一小分块
        for i := FPriorityList.Count - 1 downto 0 do
        begin
          pPriority := FPriorityList[i];
          if (pPriority^.FPrioritySegmentIndex = ASegmentIndex) and (pPriority^.FPriorityBlockIndex = ABlockIndex) then
          begin
            Dispose( pPriority );
            FPriorityList.Delete(i);
            Break;
          end;
        end;
      end;
    end
    else
      FInvalideBufferSize := FInvalideBufferSize + ASize;
  finally
    LeaveCriticalSection( FLock );
  end;
end;

function TxdFileSegmentTable.CompletedSegment(const ASegmentIndex: Integer; const ASize: Cardinal): Boolean;
var
  p: PSegmentInfo;
  i: Integer;
  pPriority: PPriorityTableInfo;
begin
  Result := False;
  EnterCriticalSection( FLock );
  try
    if (ASegmentIndex < 0) or (ASegmentIndex >= FSegmentCount) then
    begin
      FInvalideBufferSize := FInvalideBufferSize + ASize;
      Exit;
    end;
    p := FSegmentList[ASegmentIndex];
    if p^.FSegmentSize <> ASize then
    begin
      FInvalideBufferSize := FInvalideBufferSize + ASize;
      Exit;
    end;

    case p^.FSegmentState of
      ssEmpty,
      ssFullSegment,
      ssBlockSegment:
      begin
        Result := True;
        p^.FSegmentState := ssCompleted;
        FSegmentStateTable[ASegmentIndex] := True;
        FCompletedFileSize := FCompletedFileSize + p^.FSegmentSize; 
        for i := FPriorityList.Count - 1 downto 0 do
        begin
          pPriority := FPriorityList[i];
          if pPriority^.FPrioritySegmentIndex = ASegmentIndex then
          begin
            Dispose( pPriority );
            FPriorityList.Delete(i);
          end;
        end;
        CheckLastCompleteSegmentIndex;
      end;
      ssCompleted:
      begin
        FInvalideBufferSize := FInvalideBufferSize + ASize;
        Exit;
      end;
    end;
  finally
    LeaveCriticalSection( FLock );
  end;
end;

constructor TxdFileSegmentTable.Create(const AFileSize: Int64; const ASegmentSize: Cardinal);
var
  nSegmentInfoSize: Integer;
  p: PSegmentInfo;
  i: Integer;
begin
  InitializeCriticalSection( FLock );
  FFileSize := AFileSize;
  FIsCompleted := False;
  FPriorityDownSegmentIndex := -1;
  FLastFinishedSegmentIndex := 0;
  FInvalideBufferSize := 0;
  FCompletedFileSize := 0;
  FMaxWaitTime := 3000;
  FSegmentSize := ASegmentSize;
  if FSegmentSize <= CtBlockSize then
    FSegmentSize := CtSegmentDefaultSize;
  FSegmentCount := (FFileSize + FSegmentSize - 1) div FSegmentSize;
  FBlockMaxCount := (FSegmentSize + CtBlockSize - 1) div CtBlockSize;
  nSegmentInfoSize := SizeOf(TSegmentInfo) + CtBlockStateSize * (FBlockMaxCount - 1);
  FSegmentMen := TxdFixedMemoryManager.Create( nSegmentInfoSize, FSegmentCount );
  FSegmentList := TList.Create;
  FPriorityList := TList.Create;
  for i := 0 to FSegmentCount - 1 do
  begin
    if not FSegmentMen.GetMem( Pointer(p) ) then
      raise Exception.Create( 'TxdFileSegmentTable无法申请到内存，程序严重问题' );
    p^.FSegmentIndex := i;
    p^.FSegmentBeginPos := Int64(i) * FSegmentSize;
    if FFileSize - p^.FSegmentBeginPos >= FSegmentSize then
      p^.FSegmentSize := FSegmentSize
    else
      p^.FSegmentSize := FFileSize - p^.FSegmentBeginPos;
    p^.FSegmentState := ssEmpty;
    p^.FSegmentActiveTime := 0;
    p^.FRecvBlockCount := 0;
    p^.FBlockCount := (p^.FSegmentSize + CtBlockSize - 1) div CtBlockSize;
    FillChar( p^.FBlockState[0], p^.FBlockCount * CtBlockStateSize, bsEmpty );
    FSegmentList.Add( p );
  end;
  FSegmentStateTable := TxdSegmentStateTable.Create;
  FSegmentStateTable.Size := FSegmentCount;
end;

destructor TxdFileSegmentTable.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPriorityList.Count - 1 do
    Dispose( FPriorityList[i] );
  FreeAndNil( FPriorityList );
  FreeAndNil( FSegmentMen );
  FreeAndNil( FSegmentList );
  FreeAndNil( FSegmentStateTable );
  DeleteCriticalSection( FLock );
  inherited;
end;

procedure TxdFileSegmentTable.DoCheckCanRead(const ASegmentIndex, ABlockIndex: Integer; var AIsContinue: Boolean; AData: Pointer);
var
  pSeg: PSegmentInfo;
begin
  AIsContinue := True;
  pSeg := FSegmentList[ASegmentIndex];
  if pSeg^.FSegmentState = ssCompleted then Exit;

  if (ABlockIndex >= 0) and (pSeg^.FBlockState[ABlockIndex] <> bsComplete) then
  begin
    //不能读
    FCanRead := False;
    AIsContinue := False;
    if pSeg^.FBlockState[ABlockIndex] = bsWaitReply then
      pSeg^.FBlockState[ABlockIndex] := bsEmpty;
//    OutputDebugString( PChar( '无法读取位置 ' + IntTostr(ASegmentIndex) + ':' + IntToStr(ABlockIndex) + ' 状态: '  + IntToStr(Integer(pSeg^.FBlockState[ABlockIndex]))));
  end;
end;

procedure TxdFileSegmentTable.DoCheckPosInfo(const APos: Int64; const ASize: Cardinal; ACheckSub: _TOnCheckPosInfo; AData: Pointer);
var
  i, j, nSegmentIndex, nBlockIndex, nIndex: Integer;
  nLen: Cardinal;
  p: PSegmentInfo;
  bContinue: Boolean;
begin
  //ACheckSub： BlockIndex < 0 时代表所有

  if ASize = 0 then
    nLen := 0
  else
    nLen := ASize - 1;
  if (APos >= FFileSize) or (APos + nLen > FFileSize) then Exit;

  nSegmentIndex := APos div FSegmentSize;
  if (nSegmentIndex < 0) or (nSegmentIndex >= FSegmentCount) then Exit;


  if nLen = 0 then
  begin
    //只处理指定位置所在的分段
    bContinue := True;
    ACheckSub( nSegmentIndex, -1, bContinue, AData );
  end
  else
  begin
    //处理指定位置开始的分块到结束大小的分块
    p := FSegmentList[nSegmentIndex];
    nIndex := (APos + nLen) div FSegmentSize;
    nBlockIndex := (APos - p^.FSegmentBeginPos) div CtBlockSize;

    if nSegmentIndex = nIndex then
    begin
      //如果要处理的范围在同一个分段之中
      nIndex := (APos + nLen - p^.FSegmentBeginPos) div CtBlockSize;
      for i := nBlockIndex to nIndex do
      begin
        bContinue := True;
        ACheckSub( nSegmentIndex, i, bContinue, AData );
        if not bContinue then
          Break;
      end;

    end
    else
    begin
      {要处理的范围跨越多个分段}

      //第一个要被处理的范围。
      if nBlockIndex = 0 then
      begin
        bContinue := True;
        ACheckSub( nSegmentIndex, -1, bContinue, AData );
      end
      else
      begin
        for i := nBlockIndex to p^.FBlockCount - 1 do
        begin
          bContinue := True;
          ACheckSub( nSegmentIndex, i, bContinue, AData );
          if not bContinue then
            Break;
        end;
      end;

      if not bContinue then Exit;

      //其它要被处理的范围
      for i := nSegmentIndex + 1 to nIndex do
      begin
        bContinue := True;
        p := FSegmentList[i];
        if i = nIndex then
        begin
          //最后一个
          nBlockIndex := (APos + nLen - p^.FSegmentBeginPos) div CtBlockSize;
          if nBlockIndex = p^.FBlockCount - 1 then
          begin
            bContinue := True;
            ACheckSub( i, -1, bContinue, AData );
            if not bContinue then
              Break;
          end
          else
          begin
            for j := 0 to nBlockIndex do
            begin
              bContinue := True;
              ACheckSub( i, j, bContinue, AData );
              if not bContinue then
                Break;
            end;
          end;

        end
        else
        begin
          //中间，处理所有
          bContinue := True;
          ACheckSub( i, -1, bContinue, AData );
        end;
        
        if not bContinue then
          Break;
      end;
    end;
  end;
end;

procedure TxdFileSegmentTable.DoCheckToAddList(const ASegmentIndex, ABlockIndex: Integer; var AIsContinue: Boolean; AData: Pointer);
var
  i: Integer;
  p: PPriorityTableInfo;
  bAdd: Boolean;
  pSeg: PSegmentInfo;
begin
  AIsContinue := True;
  EnterCriticalSection( FLock );
  try
    pSeg := FSegmentList[ASegmentIndex];
    if pSeg^.FSegmentState = ssCompleted then Exit; //需要优先的分段已经下载完成，不需要添加
    if ABlockIndex >= 0 then
    begin
      //只添加某一小分块时
      case pSeg^.FBlockState[ABlockIndex] of
        bsWaitReply, bsComplete:
        begin
          //此分块已经不需要再添加到列表中
          Exit;
        end;  
      end;
    end;
  
    bAdd := True;
    for i := 0 to FPriorityList.Count - 1 do
    begin
      p := FPriorityList[i];
      if (p^.FPrioritySegmentIndex = ASegmentIndex) and (p^.FPriorityBlockIndex = ABlockIndex) then
      begin
        bAdd := False;
        Break;
      end;
    end;
    if bAdd then
    begin
      New( p );
      p^.FPrioritySegmentIndex := ASegmentIndex;
      p^.FPriorityBlockIndex := ABlockIndex;
      FPriorityList.Add( p );
    end;
    OutputDebugString( PChar('当前优先处理：' + IntToStr(ASegmentIndex) + ':' + IntToStr(ABlockIndex) + ' 当前优先数量：' + IntToStr(FPriorityList.Count) ) );
//    OutputDebugString( PChar('当前需要优先处理数量：' + IntToStr(FPriorityList.Count)) );
  finally
    LeaveCriticalSection( FLock );
  end;
end;

function TxdFileSegmentTable.GetBlockSize(const ASegmentIndex, ABlockIndex: Integer; var ABeginPos: Int64; var ASize: Cardinal): Boolean;
var
  p: PSegmentInfo;
begin
  Result := False;
  if (ASegmentIndex < 0) or (ASegmentIndex >= FSegmentCount) then Exit;
  p := FSegmentList[ASegmentIndex];
  if (ABlockIndex < 0) or (ABlockIndex >= p^.FBlockCount) then Exit;
  ABeginPos := p^.FSegmentBeginPos + ABlockIndex * CtBlockSize;
  if p^.FSegmentBeginPos + p^.FSegmentSize - ABeginPos >= CtBlockSize then
    ASize := CtBlockSize
  else
    ASize := p^.FSegmentBeginPos + p^.FSegmentSize - ABeginPos;
  p^.FSegmentActiveTime := GetTickCount;
  Result := True;
end;

function TxdFileSegmentTable.GetBlockSize(const ASegmentIndex, ABlockIndex: Integer; var ASize: Integer): Boolean;
var
  p: PSegmentInfo;
  nPos: Int64;
begin
  Result := False;
  if (ASegmentIndex < 0) or (ASegmentIndex >= FSegmentCount) then Exit;
  p := FSegmentList[ASegmentIndex];
  if (ABlockIndex < 0) or (ABlockIndex >= p^.FBlockCount) then Exit;
  nPos := p^.FSegmentBeginPos + ABlockIndex * CtBlockSize;
  if p^.FSegmentBeginPos + p^.FSegmentSize - nPos >= CtBlockSize then
    ASize := CtBlockSize
  else
    ASize := p^.FSegmentBeginPos + p^.FSegmentSize - nPos;
  Result := True;
end;

function TxdFileSegmentTable.GetNeedMostBlockInfo(var ASegmentIndex, ABlockIndex: Integer): Boolean;
  function CheckBlock(pSeg: PSegmentInfo): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to pSeg^.FBlockCount - 1 do
    begin
      if pSeg^.FBlockState[i] = bsEmpty then
      begin
        ASegmentIndex := pSeg^.FSegmentIndex;
        ABlockIndex := i;
        Result := True;
        pSeg^.FBlockState[i] := bsWaitReply;
        Break;
      end;
    end;
  end;
  function CheckSegmentInfo(const AIndex: Integer): Boolean;
  var
    pSeg: PSegmentInfo;
  begin
    Result := False;
    pSeg := FSegmentList[AIndex];
    if (pSeg^.FSegmentState in [ssEmpty, ssBlockSegment]) and CheckBlock(pSeg) then
    begin
      Result := True;
      if pSeg^.FSegmentState = ssEmpty then
        pSeg^.FSegmentState := ssBlockSegment;
    end;
  end;
var
  i, nTemp, nBegin: Integer;
  pPriority: PPriorityTableInfo;
  pSeg: PSegmentInfo;
begin
  Result := False;
  EnterCriticalSection( FLock );
  try
    {下载：最优->指定位置->其它}
    //最优
    for i := 0 to FPriorityList.Count - 1 do
    begin
      pPriority := FPriorityList[i];
      if pPriority^.FPriorityBlockIndex >= 0 then
      begin
        pSeg := FSegmentList[pPriority^.FPrioritySegmentIndex];
        if pSeg^.FBlockState[pPriority^.FPriorityBlockIndex] = bsEmpty then
        begin
          Result := True;
          ASegmentIndex := pPriority^.FPrioritySegmentIndex;
          ABlockIndex := pPriority^.FPriorityBlockIndex;
          pSeg^.FBlockState[pPriority^.FPriorityBlockIndex] := bsWaitReply;
          Break;
        end;
      end
      else
        OutputDebugString( '无处理错误' );
    end;
    if not Result then
    begin
      //指定
      nBegin := MaxInt;
      if (FPriorityDownSegmentIndex >= 0) and (FPriorityDownSegmentIndex < FSegmentCount) then
        nTemp := 1
      else
        nTemp := 0;
      if nTemp = 1 then
      begin
        if FLastFinishedSegmentIndex > FPriorityDownSegmentIndex then
          nBegin := FLastFinishedSegmentIndex
        else
          nBegin := FPriorityDownSegmentIndex;
        for i := nBegin to FSegmentCount - 1 do
        begin
          if CheckSegmentInfo(i) then
          begin
            Result := True;
            Break;
          end;
        end;
      end;
      //
      if not Result then
      begin
        if nBegin = FLastFinishedSegmentIndex then Exit;
        if nTemp = 1 then
          nTemp := FPriorityDownSegmentIndex
        else
          nTemp := FSegmentCount;
        for i := FLastFinishedSegmentIndex to nTemp - 1 do
        begin
          if CheckSegmentInfo(i) then
          begin
            Result := True;
            Break;
          end;
        end;
      end;
    end;
  finally
//    OutputDebugString( PChar('请求 ' + IntToStr(ASegmentIndex) + ':' + IntToStr(ABlockIndex)) );
    LeaveCriticalSection( FLock );
  end;
end;

function TxdFileSegmentTable.GetNeedMostSegmentInfo(var ASegmentIndex: Integer): Boolean;
var
  i, nTemp, nBegin: Integer;
  pPriority: PPriorityTableInfo;
  pSeg: PSegmentInfo;
begin
  Result := False;
  EnterCriticalSection( FLock );
  try
    {下载：最优->指定位置->其它}
    //最优下载
    for i := 0 to FPriorityList.Count - 1 do
    begin
      pPriority := FPriorityList[i];
      if pPriority^.FPriorityBlockIndex < 0 then
      begin
        pSeg := FSegmentList[pPriority^.FPrioritySegmentIndex];
        if pSeg^.FSegmentState = ssEmpty then
        begin
          Result := True;
          ASegmentIndex := pSeg^.FSegmentIndex;
          pSeg^.FSegmentState := ssFullSegment;
          Break;
        end;
      end;
    end;

    if not Result and (FPriorityList.Count > 0) then Exit;


    if not Result then
    begin
      //指定下载序号
      nBegin := MaxInt;
      if (FPriorityDownSegmentIndex >= 0) and (FPriorityDownSegmentIndex < FSegmentCount) then
        nTemp := 1
      else
        nTemp := 0;

      if nTemp = 1 then
      begin
        //需要优先下载指定位置
        if FLastFinishedSegmentIndex > FPriorityDownSegmentIndex then
          nBegin := FLastFinishedSegmentIndex
        else
          nBegin := FPriorityDownSegmentIndex;
        
        for i := nBegin to FSegmentCount - 1 do
        begin
          pSeg := FSegmentList[i];
          if pSeg^.FSegmentState = ssEmpty then
          begin
            Result := True;
            ASegmentIndex := pSeg^.FSegmentIndex;
            pSeg^.FSegmentState := ssFullSegment;
            Break;
          end;
        end;
      end;
      if not Result then
      begin

        if nBegin = FLastFinishedSegmentIndex then
          Exit;
        //从头查找
        if nTemp = 1 then
          nTemp := FPriorityDownSegmentIndex
        else
          nTemp := FSegmentCount;
        for i := FLastFinishedSegmentIndex to nTemp - 1 do
        begin
          pSeg := FSegmentList[i];
          if pSeg^.FSegmentState = ssEmpty then
          begin
            Result := True;
            ASegmentIndex := pSeg^.FSegmentIndex;
            pSeg^.FSegmentState := ssFullSegment;
            Break;
          end;
        end;
      end;
    end;
  finally
    LeaveCriticalSection( FLock );
  end;
end;

function TxdFileSegmentTable.GetP2PBlockInfo(var ASegmentIndex, ABlockIndex: Integer; ASegTableState: TxdSegmentStateTable): Boolean;
var
  i, nTemp: Integer;
  pPriority: PPriorityTableInfo;
  
  function CheckBlock(pSeg: PSegmentInfo): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to pSeg^.FBlockCount - 1 do
    begin
      if pSeg^.FBlockState[i] = bsEmpty then
      begin
        ASegmentIndex := pSeg^.FSegmentIndex;
        ABlockIndex := i;
        Result := True;
        pSeg^.FBlockState[i] := bsWaitReply;
        Break;
      end;
    end;
  end;
  function CheckSegmentInfo(const AIndex: Integer): Boolean;
  var
    pSeg: PSegmentInfo;
  begin
    Result := False;
    if not ASegTableState.SegmentCompleted[AIndex] then Exit;
    pSeg := FSegmentList[AIndex];
    if (pSeg^.FSegmentState in [ssEmpty, ssBlockSegment]) and CheckBlock(pSeg) then
    begin
      Result := True;
      if pSeg^.FSegmentState = ssEmpty then
        pSeg^.FSegmentState := ssBlockSegment;
    end;
  end;
begin
  {下载：查找最不需要的分段->找定位置->最优}
  //查找最不需要的分段定义：从中间到最后
  Result := False;
  EnterCriticalSection( FLock );
  try
    if not Assigned(ASegTableState) then Exit;
    if (FPriorityDownSegmentIndex >= 0) and (FPriorityDownSegmentIndex < FSegmentCount) then
      nTemp := FPriorityDownSegmentIndex + (FSegmentCount - FPriorityDownSegmentIndex) div 2
    else
      nTemp := FLastFinishedSegmentIndex + (FSegmentCount - FLastFinishedSegmentIndex) div 2;

    if nTemp < FLastFinishedSegmentIndex then
      nTemp := FLastFinishedSegmentIndex;
    for i := nTemp to FSegmentCount - 1 do
    begin
      if CheckSegmentInfo(i) then
      begin
        Result := True;
        Break;
      end;
    end;

    if not Result then
    begin
      //最优
      for i := 0 to FPriorityList.Count - 1 do
      begin
        pPriority := FPriorityList[i];
        if pPriority^.FPriorityBlockIndex >= 0 then
        begin
          if CheckSegmentInfo(pPriority^.FPrioritySegmentIndex) then
          begin
            Result := True;
            Break;
          end;
        end;
      end;
      if not Result then
      begin
        //所有
        for i := FLastFinishedSegmentIndex to FSegmentCount - 1 do
        begin
          if CheckSegmentInfo(i) then
          begin
            Result := True;
            Break;
          end;
        end;
      end;
    end;
  finally
    LeaveCriticalSection( FLock );
  end;
end;

function TxdFileSegmentTable.GetSegmentSize(const ASegmentIndex: Integer; var ABeginPos: Int64; var ASize: Cardinal): Boolean;
var
  p: PSegmentInfo;
begin
  Result := False;
  if (ASegmentIndex < 0) or (ASegmentIndex >= FSegmentCount) then Exit;
  p := FSegmentList[ASegmentIndex];
  ABeginPos := p^.FSegmentBeginPos;
  ASize := p^.FSegmentSize;
  p^.FSegmentActiveTime := GetTickCount;
  Result := True;
end;

function TxdFileSegmentTable.IsEmpty: Boolean;
var
  i: Integer;
  p: PSegmentInfo;
begin
  Result := True;
  for i := FLastFinishedSegmentIndex to FSegmentCount - 1 do
  begin
    p := FSegmentList[i];
    if p^.FSegmentState in [ssFullSegment, ssBlockSegment, ssCompleted] then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TxdFileSegmentTable.PackedPriorityList;
  procedure DeleteOther(const p: PPriorityTableInfo);
  var
    i: Integer;
    p1: PPriorityTableInfo;
  begin
    for i := FPriorityList.Count - 1 downto 0 do
    begin
      p1 := FPriorityList[i];
      if (p1^.FPrioritySegmentIndex = p^.FPrioritySegmentIndex) and (p1 <> p) then
      begin
        FPriorityList.Delete(i);
        Dispose(p1);
      end;
    end;
  end;
var
  i: Integer;
  pPriority: PPriorityTableInfo;
begin
  for i := FPriorityList.Count - 1 downto 0 do
  begin
    pPriority := FPriorityList[i];
    if pPriority^.FPriorityBlockIndex < 0 then
      DeleteOther( pPriority )
  end;
end;

procedure TxdFileSegmentTable.ResetSegment(const ASegIndex: Integer);
var
  p: PSegmentInfo;
  i, nSize: Integer;
  bDec: Boolean;
begin
  if (ASegIndex < 0) or (ASegIndex >= FSegmentCount) then Exit;
  EnterCriticalSection( FLock );
  try
    p := FSegmentList[ASegIndex];
    if p^.FSegmentState = ssCompleted then
    begin
      FCompletedFileSize := FCompletedFileSize - p^.FSegmentSize;
      bDec := False;
    end
    else
      bDec := True;
    p^.FSegmentState := ssEmpty;
    for i := 0 to p^.FBlockCount - 1 do
    begin
      if bDec and (p^.FBlockState[i] = bsComplete) then
      begin
        GetBlockSize( ASegIndex, i, nSize );
        FCompletedFileSize := FCompletedFileSize - nSize;
      end;
      p^.FBlockState[i] := bsEmpty;
    end;
    if FLastFinishedSegmentIndex > ASegIndex then
      FLastFinishedSegmentIndex := ASegIndex;
    if FIsCompleted then
      FIsCompleted := False;
  finally
    LeaveCriticalSection( FLock );
  end;
end;

{ TxdFileSegmentStream }

function TxdFileSegmentStream.CalcFileHash(PAbort: PBoolean): TxdHash;
var
  Context: TxdHashContext;
  Buffer: array[0..16383] of Byte;
  nPos: Int64;
  nSize, nReadSize: Integer;
begin
  HashInit(Context);
  nPos := 0;
  nSize := SizeOf(Buffer);
  nReadSize := nSize;
  repeat
    if nPos + nReadSize > FFileSize then
    begin
      nReadSize := FFileSize - nPos;
      if nReadSize <= 0 then Break;
    end;

    //读数据
    LockStream;
    try
      FFileStream.Position := nPos;
      FFileStream.ReadLong( Buffer, nReadSize );
    finally
      UnLockStream;
    end;

    //计算
    HashUpdate(Context, @Buffer, nReadSize);
    if Assigned(PAbort) and PAbort^ then Exit;

    nPos := nPos + nReadSize;
  until (nReadSize <> nSize);

  HashFinal( Context, Result );
end;

procedure TxdFileSegmentStream.CompletedFile;
begin
  FIsComplete := True;
end;

constructor TxdFileSegmentStream.Create(AFileStream: TxdMemoryFile; ASegmentTable: TxdFileSegmentTable);
begin
  InitializeCriticalSection( FLock );
  FFileStream := AFileStream;
  FSegmentTable := ASegmentTable;
  //直接映射所有文件(或可映射部分)
  FFileStream.MapFileToMemory( 0, 0 );
  FFileSize := FFileStream.FileSize;
  FFileName := FFileStream.FileName;
  FIsComplete := FSegmentTable.IsCompleted;
  FIsOnlyRead := False;
end;

destructor TxdFileSegmentStream.Destroy;
begin
  if FAutoFreeStream then
    FreeAndNil( FFileStream );
  if FAutoFreeSegmentTable and FSegmentTable.IsCompleted then
    FreeAndNil( FSegmentTable );
  DeleteCriticalSection( FLock );
  inherited;
end;

procedure TxdFileSegmentStream.LockStream;
begin
  EnterCriticalSection( FLock );
end;

function TxdFileSegmentStream.ReadBlockBuffer(const ASegmentIndex, ABlockIndex: Integer; const ABuffer: PByte; var ABufLen: Integer): Boolean;
var
  pSeg: PSegmentInfo;
  nPos: Int64;
  nSize: Cardinal;
begin
  Result := False;
  if (ASegmentIndex < 0) or (ASegmentIndex >= FSegmentTable.SegmentCount) then Exit;
  pSeg := FSegmentTable.SegmentList[ASegmentIndex];
  if (ABlockIndex < 0) or (ABlockIndex >= pSeg^.FBlockCount) then Exit;

  LockStream;
  try
    Result := (pSeg^.FSegmentState = ssCompleted) or (pSeg^.FBlockState[ABlockIndex] = bsComplete);//不需要验证HASH就可以共享数据
    if Result then
    begin
      FSegmentTable.GetBlockSize(ASegmentIndex, ABlockIndex, nPos, nSize);
      FFileStream.Position := nPos;
      FFileStream.ReadLong(ABuffer^, nSize);
      ABufLen := nSize;
    end;
  finally
    UnLockStream;
  end;
end;

function TxdFileSegmentStream.ReadBuffer(const APos: Int64; const ASize: Integer; ABuffer: PByte): Boolean;
begin
  LockStream;
  try
    Result := FSegmentTable.CheckCanRead(APos, ASize);
    if Result then
    begin
      FFileStream.Position := APos;
      FFileStream.ReadLong(ABuffer^, ASize);
    end
    else
      FSegmentTable.AddPriorityDownInfo( APos, ASize );
  finally
    UnLockStream;
  end;
end;

procedure TxdFileSegmentStream.UnLockStream;
begin
  LeaveCriticalSection( FLock );
end;

procedure TxdFileSegmentStream.SetFileHash(const AHash: TxdHash);
begin
  FFileHash := AHash;
end;

function TxdFileSegmentStream.WriteBlockBuffer(const ASegmentIndex, ABlockIndex: Integer; const ABuffer: PByte; const ABufLen: Integer): Boolean;
var
  pSeg: PSegmentInfo;
  nPos: Int64;
  nSize: Cardinal;
begin
  LockStream;
  try
    Result := FSegmentTable.CompletedBlock(ASegmentIndex, ABlockIndex, ABufLen);
    if Result then
    begin
      FSegmentTable.GetBlockSize(ASegmentIndex, ABlockIndex, nPos, nSize);
      FFileStream.Position := nPos;
      FFileStream.WriteLong(ABuffer^, nSize);
      pSeg := FSegmentTable.SegmentList[ASegmentIndex];
      if (pSeg^.FSegmentState = ssCompleted) and Assigned(FSegmentTable.OnSegmentCompleted) then
        FSegmentTable.OnSegmentCompleted( ASegmentIndex );
      pSeg^.FSegmentActiveTime := GetTickCount;
    end;
  finally
    UnLockStream;
  end;
end;

function TxdFileSegmentStream.WriteSegmentBuffer(const ASegmentIndex: Integer; const ABuffer: PByte; const ABufLen: Integer): Boolean;
var
  pSeg: PSegmentInfo;
  nPos: Int64;
  nSize: Cardinal;
begin
  LockStream;
  try
    Result := FSegmentTable.CompletedSegment(ASegmentIndex, ABufLen);
    if Result then
    begin
      FSegmentTable.GetSegmentSize(ASegmentIndex, nPos, nSize);
      FFileStream.Position := nPos;
      FFileStream.WriteLong(ABuffer^, nSize);
      pSeg := FSegmentTable.SegmentList[ASegmentIndex];
      if (pSeg^.FSegmentState = ssCompleted) and Assigned(FSegmentTable.OnSegmentCompleted) then
        FSegmentTable.OnSegmentCompleted( ASegmentIndex );
      pSeg^.FSegmentActiveTime := GetTickCount;
    end;
  finally
    UnLockStream;
  end;
end;
{ TxdLocalFileStream }

constructor TxdLocalFileStream.Create(const AFileName: string; const AFileHash: TxdHash; const ASegmentSize: Integer);
begin
  if ASegmentSize <= 0 then
    FSegmentSize := CtSegmentDefaultSize
  else
    FSegmentSize := ASegmentSize;
  FFileStream := TxdMemReadFile.Create(AFileName, 0, False, True );
  FFileStream.MapFileToMemory(0, 0);
  FFileName := AFileName;
  FFileSize := FFileStream.FileSize;
  FFileHash := AFileHash;
  FSegmentCount := (FFileSize + FSegmentSize - 1) div FSegmentSize;
  FBlockMaxCount := (FSegmentSize + CtBlockSize - 1) div CtBlockSize;
  FIsComplete := True;
  FIsOnlyRead := True;
end;

destructor TxdLocalFileStream.Destroy;
begin
  FreeAndNil( FFileStream );
  inherited;
end;

function TxdLocalFileStream.ReadBlockBuffer(const ASegmentIndex, ABlockIndex: Integer; const ABuffer: PByte; var ABufLen: Integer): Boolean;
var
  nPos, nTemp: Int64;
  nSize, nCurMaxBlockCount: Integer;
begin
  Result := False;
  if (ASegmentIndex < 0) or (ASegmentIndex >= FSegmentCount) then Exit;
  nTemp := FFileSize - ASegmentIndex * FSegmentSize;
  if nTemp < FSegmentSize then
    nCurMaxBlockCount := (nTemp + CtBlockSize - 1) div CtBlockSize
  else
    nCurMaxBlockCount := FBlockMaxCount;
    
  if (ABlockIndex < 0) or (ABlockIndex >= nCurMaxBlockCount) then Exit;

  nPos := ASegmentIndex * FSegmentSize + ABlockIndex * CtBlockSize;

  if ABlockIndex <> (nCurMaxBlockCount - 1) then
    nSize := CtBlockSize
  else
  begin
    nTemp := (ASegmentIndex + 1) * FSegmentSize;
    if nTemp > FFileSize then
      nTemp := FFileSize;
    nSize := nTemp - nPos;
  end;
  Result := ReadBuffer(nPos, nSize, ABuffer);
  if Result then
    ABufLen := nSize;
end;

function TxdLocalFileStream.ReadBuffer(const APos: Int64; const ASize: Integer; ABuffer: PByte): Boolean;
begin
  Result := (APos >= 0) and (APos + ASize <= FFileSize);
  if Result then
    FFileStream.ReadMemory(APos, ASize, ABuffer);
end;

{ TxdMemReadFile }

procedure TxdMemReadFile.ReadMemory(const APos: Int64; const ASize: Integer; ABuffer: PByte);
begin
  Move( PByte(Integer(FMemory) + APos)^, ABuffer^, ASize );
end;

{ TxdFileStreamManage }

constructor TxdFileStreamManage.Create;
begin
  FCurStreamID := GetTickCount;
  FManageList := TList.Create;
  InitializeCriticalSection( FLockManage );
end;

function TxdFileStreamManage.CreateFileStream(const AFileName: string; const AFileHash: TxdHash; const ASegmentSize: Integer): TxdLocalFileStream;
var
  i: Integer;
  p: PStreamManageInfo;
begin
  Result := nil;
  if not FileExists(AFileName) or HashCompare(AFileHash, CtEmptyHash) then Exit;

  LockManage;
  try
    for i := 0 to FManageList.Count - 1 do
    begin
      p := FManageList[i];
      if HashCompare(AFileHash, p^.FStream.FileHash) then
      begin
        Inc(p^.FCount);
        Result := p^.FStream as TxdLocalFileStream;
        Break;
      end;
    end;
    if not Assigned(Result) then
    begin
      Inc( FCurStreamID );
      New( p );
      p^.FStream := TxdLocalFileStream.Create( AFileName, AFileHash, ASegmentSize );
      p^.FStream.FStreamID := FCurStreamID;
      p^.FCount := 1;
      Result := p^.FStream as TxdLocalFileStream;
      FManageList.Add( p );
    end;
  finally
    UnLockManage;
  end;
end;

function TxdFileStreamManage.CreateFileStream(const AFileName: string; const ASegmentTable: TxdFileSegmentTable): TxdFileSegmentStream;
var
  i: Integer;
  p: PStreamManageInfo;
  f: TxdMemoryFile;
begin
  Result := nil;
  if ASegmentTable = nil then Exit;
  LockManage;
  try
    for i := 0 to FManageList.Count - 1 do
    begin
      p := FManageList[i];
      if CompareText(AFileName, p^.FStream.FFileName) = 0 then
      begin
        Inc(p^.FCount);
        Result := p^.FStream as TxdFileSegmentStream;
        Break;
      end;
    end;
    if not Assigned(Result) then
    begin
      try
        f := TxdMemoryFile.Create( AFileName, ASegmentTable.FileSize );
      except
        Exit;
      end;
      Inc( FCurStreamID );
      New( p );
      p^.FStream := TxdFileSegmentStream.Create( f, ASegmentTable );
      p^.FStream.FStreamID := FCurStreamID;
      p^.FCount := 1;
      Result := p^.FStream as TxdFileSegmentStream;
      Result.FAutoFreeStream := True;
      Result.FAutoFreeSegmentTable := True;
      FManageList.Add( p );
    end;
  finally
    UnLockManage;
  end;
end;

destructor TxdFileStreamManage.Destroy;
var
  i: Integer;
  p: PStreamManageInfo;
begin
  for i := 0 to FManageList.Count - 1 do
  begin
    p := FManageList[i];
    FreeAndNil( p^.FStream );
    Dispose( p );
  end;
  FreeAndNil( FManageList );
  DeleteCriticalSection( FLockManage );
  inherited;
end;

procedure TxdFileStreamManage.LockManage;
begin
  EnterCriticalSection( FLockManage );
end;

function TxdFileStreamManage.QueryFileStream(const AStreamID: Integer): TxdP2SPFileStreamBasic;
var
  i: Integer;
  p: PStreamManageInfo;
begin
  Result := nil;
  LockManage;
  try
    for i := 0 to FManageList.Count - 1 do
    begin
      p := FManageList[i];
      if p^.FStream.StreamID = AStreamID then
      begin
        Inc(p^.FCount);
        Result := p^.FStream;
        Break;
      end;
    end;
  finally
    UnLockManage;
  end;
end;

function TxdFileStreamManage.QueryFileStream(const AFileHash: TxdHash): TxdP2SPFileStreamBasic;
var
  i: Integer;
  p: PStreamManageInfo;
begin
  Result := nil;
  LockManage;
  try
    for i := 0 to FManageList.Count - 1 do
    begin
      p := FManageList[i];
      if HashCompare(p^.FStream.FileHash, AFileHash) then
      begin
        Inc(p^.FCount);
        Result := p^.FStream;
        Break;
      end;
    end;
  finally
    UnLockManage;
  end;
end;

procedure TxdFileStreamManage.ReleaseFileStream(const AStream: TxdP2SPFileStreamBasic);
var
  i: Integer;
  p: PStreamManageInfo;
begin
  LockManage;
  try
    for i := 0 to FManageList.Count - 1 do
    begin
      p := FManageList[i];
      if p^.FStream = AStream then
      begin
        Dec( p^.FCount );
        if p^.FCount <= 0 then
        begin
          p^.FStream.Free;
          Dispose( p );
          FManageList.Delete( i );
        end;
        Break;
      end;
    end;
  finally
    UnLockManage;
  end;
end;

procedure TxdFileStreamManage.UnLockManage;
begin
  LeaveCriticalSection( FLockManage );
end;

{ TxdSegmentStateTable }

const
  BitsPerInt = SizeOf(Integer) * 8;

type
  TBitEnum = 0..BitsPerInt - 1;
  TBitSet = set of TBitEnum;
  PBitArray = ^TBitArray;
  TBitArray = array[0..4096] of TBitSet;

constructor TxdSegmentStateTable.Create(const ASegmentCount: Integer; ABitsBuf: Pointer; ALen: Integer);
begin
  MakeByMem(ASegmentCount, ABitsBuf, ALen);
end;

constructor TxdSegmentStateTable.Create;
begin

end;

destructor TxdSegmentStateTable.Destroy;
begin
  SetSize( 0 );
  inherited;
end;

procedure TxdSegmentStateTable.Error;
begin
  raise EBitsError.Create( 'TxdSegmentStateTable index out of range' );
end;

function TxdSegmentStateTable.GetBit(Index: Integer): Boolean;
asm
        CMP     Index,[EAX].FSize
        JAE     TBits.Error
        MOV     EAX,[EAX].FBits
        BT      [EAX],Index
        SBB     EAX,EAX
        AND     EAX,1
end;

procedure TxdSegmentStateTable.MakeByMem(const ASegmentCount: Integer; ABitsBuf: Pointer; ALen: Integer);
begin
  Size := ASegmentCount;
  if Assigned(ABitsBuf) and (ALen > 0) then
    Move( ABitsBuf^, FBits^, ALen );
end;

procedure TxdSegmentStateTable.SetBit(Index: Integer; const Value: Boolean);
asm
        CMP     Index,[EAX].FSize
        JAE     @@Size

@@1:    MOV     EAX,[EAX].FBits
        OR      Value,Value
        JZ      @@2
        BTS     [EAX],Index
        RET

@@2:    BTR     [EAX],Index
        RET

@@Size: CMP     Index,0
        JL      TBits.Error
        PUSH    Self
        PUSH    Index
        PUSH    ECX {Value}
        INC     Index
        CALL    TBits.SetSize
        POP     ECX {Value}
        POP     Index
        POP     Self
        JMP     @@1
end;

procedure TxdSegmentStateTable.SetSize(const Value: Integer);
var
  NewMem: Pointer;
  NewMemSize: Integer;
  OldMemSize: Integer;

  function Min(X, Y: Integer): Integer;
  begin
    Result := X;
    if X > Y then Result := Y;
  end;

begin
  if Value <> Size then
  begin
    if Value < 0 then Error;
    NewMemSize := ((Value + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer);
    OldMemSize := ((Size + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer);
    if NewMemSize <> OldMemSize then
    begin
      NewMem := nil;
      if NewMemSize <> 0 then
      begin
        GetMem(NewMem, NewMemSize);
        FillChar(NewMem^, NewMemSize, 0);
      end;
      if OldMemSize <> 0 then
      begin
        if NewMem <> nil then
          Move(FBits^, NewMem^, Min(OldMemSize, NewMemSize));
        FreeMem(FBits, OldMemSize);
      end;
      FBits := NewMem;
      FBitsMemLen := NewMemSize;
    end;
    FSize := Value;
  end;
end;

{ TxdP2SPFileStreamBasic }

destructor TxdP2SPFileStreamBasic.Destroy;
var
  i: Integer;
  bOK: Boolean;
  strPath: string;
begin
  inherited;
  if IsComplete and (FRenameFileName <> '') then
  begin
    strPath := ExtractFilePath(FRenameFileName);
    bOK := True;
    if not DirectoryExists(strPath) then
      bOK := ForceDirectories(strPath);
    if bOK then
    begin
      for i := 0 to 10 do
      begin
        bOK := RenameFile( FFileName, FRenameFileName );
        if bOK then Break;
      end;
    end;
  end;
end;

procedure TxdP2SPFileStreamBasic.SetRenameFileName(const Value: string);
begin
  FRenameFileName := Value;
end;

initialization
  StreamManage := TxdFileStreamManage.Create;

finalization
  StreamManage.Free;

end.
