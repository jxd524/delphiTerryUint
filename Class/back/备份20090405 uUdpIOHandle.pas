{
单元名称: uUdpIOHandle
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: UDP包的处理类.
开始时间: 2009-1-20
修改时间: 2009-2-5 (最后修改)
作    用: 处理UDP包头信息,分析包,组合包.CRC加解密;
          多包组合使用散列加单链表;
          包的通知方式都使用线程通知
          提供发客户端发送同步和异步命令函数.但都是使用异步接收数据
引用单元:
          uDataNotifyQueues:
            单包模式: TSimpleNotify
            多包模式: TMulitNotify

UDP 包头(TUdpHeader)处理
字节顺序	       内容	           说明
0	         UDP协议版本	    便于以后升级协议
1	         包的总数	        1:此为独立包. <= 0: 错误. > 1: 组包
2	         此包的序号	     >=0
3	         是否为同步包     1: 异步 asynchronism, 0: 同步 synchronization
4,5        流水号	         仅作用于组包模式, 同一包的流水号是相同的
6,7 	     UDP包长度	       此数据包长度
8~11       用户ID	          客户端的唯一标识
12~15	     UDP数据的CRC	   要发送数据(原UDP包内容)的CRC32码
16~20      时间戳           记录此包生成的时间标志,如果是同步包.此标志必须返回 

同步包的特点:
    回复的流水号和时间戳相同
}

unit uUdpIOHandle;

interface
uses uBasicUDP, uQueues, Winsock2, Windows, SysUtils, Encrypt, uDataNotifyQueues, Classes, uConversion;

type
  //包头 CtUdpHeaderLength
  pUdpHeader = ^TUdpHeader;
  TUdpHeader = packed record
    FUdpVer: Byte;         //UDP协议版本号
    FBagCount: Byte;       //包的个数,如果是独立包,则为 1; >1: 组合包; 其它: 错误
    FBagSerial: Byte;      //包的序号.从 0 开始;
    FIsAsynch: Byte;       //同否是异步包 1: 异步; 0: 同步
    FSerialNum: Word;      //流水号
    FPackageSize: Word;    //UDP整体包的长度  Header + Data
    FUserID: Cardinal;     //用户ID; ID 从 1000 开始
    FCrc32Code: Cardinal;  //UDP数据的CRC32码
    FTimeStamp: Cardinal;  //时间戳; 同步: 返回发送过来的时间戳; 否则创建新的时间戳
  end;

const
  CtUdpPackageSize = 1500 - 8; // 8: 原UDP头
  CtUdpHeaderLength = SizeOf(TUdpHeader);
  CtUdpDataLength = CtUdpPackageSize - CtUdpHeaderLength;
  CtMaxCombinationCount = 8; //如果需要组包,则分包总量最大为 CtMaxCombinationCount 个
  CtMaxUdpDataLength = CtMaxCombinationCount * CtUdpDataLength;
type
  //UDP整体包
  pUdpPackage = ^TUdpPackage;
  TUdpPackage = record
    case Boolean of
      True:
      (
        FUdpPackageContent: array[0..CtUdpPackageSize - 1] of Byte;
      );
      False:
      (
        FUdpHeader: TUdpHeader;
        FUdpData: array[0..CtUdpDataLength - 1] of Byte;
      );
  end;

  //UDP包全部信息
  pUdpPackageInfo = ^TUdpPackageInfo;
  TUdpPackageInfo = packed record
    FSockIp: Cardinal;
    FSockPort: Word;
    FUdpLength: Word;   //接收到的数据长度, 包含包头等
    FUdpPackage: TUdpPackage;
  end;

  //接收到数据
  pRecvInfo = ^TRecvInfo;
  TRecvInfo = record
     FIP: Cardinal;
     FPort: Word;
     FIsAsynch: Boolean;
     FSerialNum: Word;
     FUserID: Cardinal;
     FTimeStamp: Cardinal;
     FBufferLength: Cardinal;
     FBuffer: Pointer;
  end;

  {$M+}
  TUdpIOHandle = class(TBasicUDP)
  private
    FMaxUdpPackageCount: Word;
    FUdpPackageMem: TStaticMemoryManager;
    FSingleQueue: TSimpleNotify; //此队列的包已被确定每一个都是独立的,单独的
    FMulitQueue: TMulitNotify;  //此队列的包需要由多个包组成

    FSpThreadCount: Word;
    FSpMaxNodeCount: Cardinal;
    FSpQuerySpaceTime: Cardinal;
    FMtHashTableCount: Cardinal;
    FMtMaxHashNodeCount: Cardinal;
    FMtThreadCount: Word;
    FMtQuerySpaceTime: Cardinal;
    FMtCheckThread: Boolean;
    FMtMaxWaitTime: Cardinal;
    FMtCheckSpaceTime: Cardinal;
    FMtEnable: Boolean;

    procedure InitStatVar; //初始化统计变量
    procedure HandlePackage(const AlpUdpPackage: pUdpPackageInfo);
    procedure FreeUdpPackage(const p: pUdpPackageInfo); inline;

    procedure SetMaxUdpPageCount(const Value: Word);
    procedure SetSinglePackageThreadCount(const Value: Word);
    procedure SetSpMaxNodeCount(const Value: Cardinal);
    procedure SetSpQuerySpaceTime(const Value: Cardinal);
    procedure SetMtHashTableCount(const Value: Cardinal);
    procedure SetMtMaxHashNodeCount(const Value: Cardinal);
    procedure SetMtThreadCount(const Value: Word);
    procedure SetMtQuerySpaceTime(const Value: Cardinal);
    procedure SetMtCheckThread(const Value: Boolean);
    procedure SetMtMaxWaitTime(const Value: Cardinal);
    procedure SetMtCheckSpaceTime(const Value: Cardinal);
    procedure SetMtEnable(const Value: Boolean);
  protected
    //统计数据
    FRecvByteCount: Cardinal;
    FSendByteCount: Cardinal;
    FRecvErrorPackageCount: Integer;
    FSendTotalPackageCount: Integer;
    FShortagePackageCount: Integer;
    FErrorPackageCount: Integer;
    FRecvTotalPackageCount: Integer;
    
    FUserID: Cardinal;  //用户ID, 由子类操作
    FCurSerialNum: Word;
    FSerialNumLock: TRTLCriticalSection;
    FRecvByteLock: TRTLCriticalSection;
    FSendByteLock: TRTLCriticalSection;

    procedure AddRecvByte(const AByteCount: Cardinal);
    procedure AddSendByte(const AByteCount: Cardinal);
    function  GetSerialNum: Word; virtual; //包的流水号, 子类可重新实现
    procedure ReclaimErrorPackageMem(const AlpUdpPackage: pUdpPackageInfo; const strErrorInfo: PAnsiChar);

    function  _SendBuffer(AIP: Cardinal; AHostShortPort: word; var ABuffer; const ABufferLen: Integer;
                          const AIsAsynch: Boolean; const ASerialNum: Word; const ATimeStamp: Cardinal): Integer;

    procedure DoRecvBuffer; override;   //由线程调用
    function  DoBeforOpenUDP: Boolean; override;  //初始化UDP前; True: 允许初始化; False: 不允许初始化
    procedure DoAfterCloseUDP; override; //UDP关闭之后

    //数据包分析完毕
    procedure DoHandleSinglPackage(Sender: TObject; ANotifyPointer: Pointer); virtual;//解析每一个独立的UDP包
    function  DoMultiPackageHandleOK(Sender: TObject; const ADataArray: array of Pointer; ALen: Integer): Boolean; virtual;//多个包处理完成
    function  DoMultiPackageHandleFail(Sender: TObject; const ADataArray: array of Pointer; ALen: Integer): Boolean; virtual;//多包处理失败

    procedure OnRecvBuffer(ARecvInfo: TRecvInfo); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    //发送
    function  SendBuffer(const AIP: Cardinal; AHostShortPort: word; ABuffer: Pointer; const ABufferLen: Integer;
                         const AIsAsynch: Boolean; ASerialNum: Word = 0; ATimeStamp: Cardinal = 0): Integer; overload;
    function  SendBuffer(const ABuffer: Pointer; const ASize: Integer; const ARecvInfo: TRecvInfo): Integer; overload;
    //异步发送
    function  SendBufferByAnsy(const AIP: Cardinal; AHostShortPort: word; ABuffer: Pointer; const ABufferLen: Integer;
                               ASerialNum: Word = 0): Integer; inline;
    //同步发送
    function  SendBufferBySync(const AIP: Cardinal; AHostShortPort: word; ABuffer: Pointer; const ABufferLen: Integer;
                               ASerialNum: Word = 0; ATimeStamp: Cardinal = 0): Integer; inline;
  published
    property MaxUdpPackageCount: Word read FMaxUdpPackageCount write SetMaxUdpPageCount default 512; //队列最大UDP包数

    //独立包使用队列相关设置
    property SinglePackageThreadCount: Word read FSpThreadCount write SetSinglePackageThreadCount; //独立包处理线程数; 0: 使用时钟
    property SinglePackageMaxNodeCount: Cardinal read FSpMaxNodeCount write SetSpMaxNodeCount; //队列节点数
    property SinglePackageQuerySpaceTime: Cardinal read FSpQuerySpaceTime write SetSpQuerySpaceTime; //查询间隔时间
    //组合包使用队列参数
    property MulitPackageEnable: Boolean read FMtEnable write SetMtEnable; //是否支持多包
    property MulitPackageHashTableCount: Cardinal read FMtHashTableCount write SetMtHashTableCount; //HASH桶数量
    property MulitPackageMaxHashNodeCount: Cardinal read FMtMaxHashNodeCount write SetMtMaxHashNodeCount; //最大Hash结点数
    property MulitPackageThreadCount: Word read FMtThreadCount write SetMtThreadCount; //多包分析对通知线程数 0: 使用时钟; > 0: 使用线程
    property MulitPackageQuerySpaceTime: Cardinal read FMtQuerySpaceTime write SetMtQuerySpaceTime;//通知外部线程查询时间
    property MulitPackageCheckThread: Boolean read FMtCheckThread write SetMtCheckThread;
    property MulitPackageCheckSpaceTime: Cardinal read FMtCheckSpaceTime write SetMtCheckSpaceTime; //检查线程间隔时间
    property MulitPackageMaxWaitTime: Cardinal read FMtMaxWaitTime write SetMtMaxWaitTime; //多包中的分包最长时间间隔
    //数据统计
    property RecvByteCount: Cardinal read FRecvByteCount;
    property SendByteCount: Cardinal read FSendByteCount;
    property RecvTotalPackageCount: Integer read FRecvTotalPackageCount;                //发送包的总量
    property SendTotalPackageCount: Integer read FSendTotalPackageCount;                //接收包的总量 
    property RecvErrorPackageCount: Integer read FRecvErrorPackageCount;           //接收数据包失败记录
    property ShortagePackageCount: Integer read FShortagePackageCount;           //由于内存不足而丢弃的包的数量
    property ErrorPackageCount: Integer read FErrorPackageCount;                 //由于包内容有误与丢弃的数量
  end;
  {$M-}

procedure FreeObject(var obj);

implementation

const
  CtCurVer = 1; //当前版本号
  CtUdpInfoSize = SizeOf(TUdpPackageInfo);
  CtErrorVer = '协议所使用版本号不正确,请更新应用程序!';

{ TUdpIOHandle }

procedure TUdpIOHandle.AddRecvByte(const AByteCount: Cardinal);
begin
  EnterCriticalSection( FRecvByteLock );
  try
    Inc( FRecvByteCount, AByteCount );
  finally
    LeaveCriticalSection( FRecvByteLock );
  end;
end;

procedure TUdpIOHandle.AddSendByte(const AByteCount: Cardinal);
begin
  EnterCriticalSection( FSendByteLock );
  try
    Inc( FSendByteCount, AByteCount );
  finally
    LeaveCriticalSection( FSendByteLock );
  end;
end;

constructor TUdpIOHandle.Create;
begin
  inherited;
  FMaxUdpPackageCount := 512;
  FUdpPackageMem := nil;

  FUserID := 0;
  FCurSerialNum := 0;

  FSingleQueue := nil;
  FSpThreadCount := 1;
  FSpMaxNodeCount := 128;
  FSpQuerySpaceTime := 300;

  FMulitQueue := nil;
  FMtEnable := True;
  FMtHashTableCount := 1024 * 4;
  FMtMaxHashNodeCount := 64;
  FMtThreadCount := 1;
  FMtQuerySpaceTime := 300;
  FMtCheckThread := True;
  FMtMaxWaitTime := 5 * 1000;
  FMtCheckSpaceTime := 3 * 1000;

  InitStatVar;
  InitializeCriticalSection( FSerialNumLock );
  InitializeCriticalSection( FRecvByteLock );
  InitializeCriticalSection( FSendByteLock );
end;

destructor TUdpIOHandle.Destroy;
begin
  DeleteCriticalSection( FSerialNumLock );
  DeleteCriticalSection( FRecvByteLock );
  DeleteCriticalSection( FSendByteLock );
  inherited;
end;

function TUdpIOHandle.DoBeforOpenUDP: Boolean;
begin
  InitStatVar;
  //UDP缓存
  FUdpPackageMem := TStaticMemoryManager.Create( CtUdpInfoSize, MaxUdpPackageCount );
  //独立包使用队列
  FSingleQueue := TSimpleNotify.Create;
  with FSingleQueue do
  begin
    MaxNodeCount := SinglePackageMaxNodeCount;
    OnSimpleNotifyEvent := DoHandleSinglPackage;
    ThreadCount := SinglePackageThreadCount;
    SpaceTime := SinglePackageQuerySpaceTime;
    Active := True;
  end;

  //多包组合使用队列
//  MulitPackageCheckThread := False;
  if MulitPackageEnable then
  begin
    FMulitQueue := TMulitNotify.Create;
    with FMulitQueue do
    begin
      HashTableCount := MulitPackageHashTableCount;
      MaxHashNodeCount := MulitPackageMaxHashNodeCount;
      PeenDataMaxCount := CtMaxCombinationCount;
      NotifyThreadCount := MulitPackageThreadCount;
      NotifyThreadSpaceTime := MulitPackageQuerySpaceTime;
      CheckThread := MulitPackageCheckThread;
      MaxWaitTime := MulitPackageMaxWaitTime;
      CheckSpaceTime := MulitPackageCheckSpaceTime;
      OnNotifyHandleOK := DoMultiPackageHandleOK;
      OnNotifyHandleFail := DoMultiPackageHandleFail;
      Active := True;
    end;
  end;
  Result := True;
end;

procedure TUdpIOHandle.DoAfterCloseUDP;
begin
  inherited;
  InitStatVar;
  FreeObject( FUdpPackageMem );
  FreeObject( FSingleQueue );
  FreeObject( FMulitQueue );
end;

procedure TUdpIOHandle.DoHandleSinglPackage(Sender: TObject; ANotifyPointer: Pointer);
var
  p: pUdpPackageInfo;
  RecvInfo: TRecvInfo;
begin
  p := ANotifyPointer;
  try
    RecvInfo.FIP := p^.FSockIp;
    RecvInfo.FPort := p^.FSockPort;
    RecvInfo.FIsAsynch := p^.FUdpPackage.FUdpHeader.FIsAsynch = 1;
    RecvInfo.FSerialNum := p^.FUdpPackage.FUdpHeader.FSerialNum;
    RecvInfo.FUserID := p^.FUdpPackage.FUdpHeader.FUserID;
    RecvInfo.FTimeStamp := p^.FUdpPackage.FUdpHeader.FTimeStamp;
    RecvInfo.FBufferLength := p^.FUdpLength - CtUdpHeaderLength;
    RecvInfo.FBuffer := @p^.FUdpPackage.FUdpData;
    OnRecvBuffer( RecvInfo );
  finally
    FreeUdpPackage( p );
  end;
end;

function TUdpIOHandle.DoMultiPackageHandleFail(Sender: TObject; const ADataArray: array of Pointer; ALen: Integer): Boolean;
var
  i: Integer;
  p: pUdpPackageInfo;
begin
  for i := 0 to ALen - 1 do
  begin
    p := ADataArray[i];
    if p <> nil then
      ReclaimErrorPackageMem( p, PAnsiChar(Format('分包错误, IP: %d, Port: %d',[p^.FSockIp, p^.FSockPort] )) )
  end;
  Result := True;
end;

function TUdpIOHandle.DoMultiPackageHandleOK(Sender: TObject; const ADataArray: array of Pointer; ALen: Integer): Boolean;
var
  Buffer: array[0..CtUdpDataLength * CtMaxCombinationCount - 1] of Byte;
  i, nTmpLen, nTotalByteCount: Integer;
  p: pUdpPackageInfo;
  RecvInfo: TRecvInfo;
begin
  Result := True;
  nTotalByteCount := 0;

  p := ADataArray[0];
  RecvInfo.FIP := p^.FSockIp;
  RecvInfo.FPort := p^.FSockPort;
  RecvInfo.FIsAsynch := p^.FUdpPackage.FUdpHeader.FIsAsynch = 1;
  RecvInfo.FSerialNum := p^.FUdpPackage.FUdpHeader.FSerialNum;
  RecvInfo.FUserID := p^.FUdpPackage.FUdpHeader.FUserID;
  RecvInfo.FTimeStamp := p^.FUdpPackage.FUdpHeader.FTimeStamp;

  for i := 0 to ALen - 1 do
  begin
    p := ADataArray[i];
    nTmpLen := p^.FUdpLength - CtUdpHeaderLength;
    Move( p^.FUdpPackage.FUdpData[0], Buffer[nTotalByteCount], nTmpLen );
    Inc( nTotalByteCount, nTmpLen );
    FreeUdpPackage( p );
  end;

  RecvInfo.FBufferLength := nTotalByteCount;
  RecvInfo.FBuffer := @Buffer;
  OnRecvBuffer( RecvInfo );
end;

procedure TUdpIOHandle.FreeUdpPackage(const p: pUdpPackageInfo);
begin
  FillChar( p^, CtUdpPackageSize, 0 ); //
  FUdpPackageMem.FreeMem( p )
end;

function TUdpIOHandle.GetSerialNum: Word;
begin
  EnterCriticalSection( FSerialNumLock );
  try
    FCurSerialNum := FCurSerialNum mod 65535 + 1;
  finally
    LeaveCriticalSection( FSerialNumLock );
  end;
  Result := FCurSerialNum;
end;

procedure TUdpIOHandle.DoRecvBuffer;
var
  lpBag: pUdpPackageInfo;
  UdpInfo: TUdpPackageInfo;
  Addr: TSockAddr;
  nLen: Integer;
begin
  InterlockedIncrement(FRecvTotalPackageCount);
  if not __RecvBuffer(UdpInfo.FUdpPackage.FUdpPackageContent, nLen, Addr) then
  begin
    InterlockedIncrement( FRecvErrorPackageCount );
    DoErrorInfo( PChar(Format('接收UDP包信息错误: %d', [WSAGetLastError])) );
    Exit;
  end;
  AddRecvByte( nLen - CtUdpHeaderLength );
  if not FUdpPackageMem.GetMem( Pointer(lpBag) ) then
  begin
    InterlockedIncrement( FShortagePackageCount );
    DoErrorInfo( 'UDP队列满,无法读取内存,将丢弃UDP包' );
    Exit;
  end;
  lpBag^.FSockIp := Addr.sin_addr.S_addr;
  lpBag^.FSockPort := htons(Addr.sin_port);
  lpBag^.FUdpLength := nLen;
  Move( UdpInfo.FUdpPackage.FUdpPackageContent, lpBag^.FUdpPackage.FUdpPackageContent, nLen );
  HandlePackage( lpBag );
end;

procedure TUdpIOHandle.HandlePackage(const AlpUdpPackage: pUdpPackageInfo);
var
  pHeader: pUdpHeader;
  nDataLen, nMtResult: Integer;
begin
  pHeader := @AlpUdpPackage^.FUdpPackage.FUdpHeader;
  if pHeader^.FUdpVer <> CtCurVer then
  begin
    ReclaimErrorPackageMem( AlpUdpPackage, CtErrorVer );
    Exit;
  end;

  //检验包: 长度判断
  if pHeader^.FPackageSize <> AlpUdpPackage^.FUdpLength then
  begin
    ReclaimErrorPackageMem( AlpUdpPackage, '包标识长度与接收到长度不一致!' );
    Exit;
  end;
  //检验包: CRC检验
  nDataLen := AlpUdpPackage^.FUdpLength - CtUdpHeaderLength;
  if nDataLen <= 0 then
  begin
    FreeUdpPackage( AlpUdpPackage );
    Exit;
  end;
  if not DecodeBuffer( pHeader^.FCrc32Code, @AlpUdpPackage^.FUdpPackage.FUdpData, nDataLen ) then
  begin
    ReclaimErrorPackageMem( AlpUdpPackage, 'CRC32 检验失败' );
    Exit;
  end;


  if 1 = pHeader^.FBagCount then //独立包
  begin
    if 0 <> pHeader^.FBagSerial then
    begin
      ReclaimErrorPackageMem( AlpUdpPackage, '独立包的序号不正确!' );
      Exit;
    end;
    FSingleQueue.Add( AlpUdpPackage );
  end
  else if pHeader^.FBagCount > 1  then //组合包
  begin
    if MulitPackageEnable then
    begin
      nMtResult := FMulitQueue.Add( pHeader^.FUserID, pHeader^.FSerialNum, pHeader^.FBagCount, pHeader^.FBagSerial, AlpUdpPackage );
      if nMtResult <> 0 then
      begin
        ReclaimErrorPackageMem( AlpUdpPackage, PAnsiChar(Format('多包数据有误: %d', [nMtResult])) );
      end;
    end
    else
      ReclaimErrorPackageMem( AlpUdpPackage, '不支持组合包形式的UDP数据' );
  end
  else
  begin
    ReclaimErrorPackageMem( AlpUdpPackage, 'UDP包头错误' );
    Exit;
  end;
end;

procedure TUdpIOHandle.InitStatVar;
begin
  FRecvByteCount := 0;
  FSendByteCount := 0;
  FRecvErrorPackageCount := 0;
  FSendTotalPackageCount := 0;
  FShortagePackageCount := 0;
  FErrorPackageCount := 0;
  FRecvTotalPackageCount := 0;
end;

procedure TUdpIOHandle.OnRecvBuffer(ARecvInfo: TRecvInfo);
begin
//  OutputDebugString( Pchar( Format('收到包数量: %d', [ABufferLen])) );
//  OutputDebugString( PChar(@ABuffer) );
end;

procedure TUdpIOHandle.ReclaimErrorPackageMem(const AlpUdpPackage: pUdpPackageInfo; const strErrorInfo: PAnsiChar);
begin
  FreeUdpPackage( AlpUdpPackage );
  InterlockedIncrement( FErrorPackageCount );
  if strErrorInfo <> nil then
    DoErrorInfo( strErrorInfo );
end;

function TUdpIOHandle.SendBuffer(const AIP: Cardinal; AHostShortPort: word; ABuffer: Pointer; const ABufferLen: Integer;
  const AIsAsynch: Boolean; ASerialNum: Word; ATimeStamp: Cardinal): Integer;
begin
  if ASerialNum = 0 then
    ASerialNum := GetSerialNum;
  if ATimeStamp = 0 then
    ATimeStamp := GetTimeStamp;
  Result := _SendBuffer( AIP, AHostShortPort, ABuffer, ABufferLen, AIsAsynch, ASerialNum, ATimeStamp );
end;

function TUdpIOHandle.SendBuffer(const ABuffer: Pointer; const ASize: Integer; const ARecvInfo: TRecvInfo): Integer;
begin
  Result := SendBuffer( ARecvInfo.FIP, ARecvInfo.FPort, ABuffer, ASize, ARecvInfo.FIsAsynch,
                        ARecvInfo.FSerialNum, ARecvInfo.FTimeStamp );
end;

function TUdpIOHandle.SendBufferByAnsy(const AIP: Cardinal; AHostShortPort: word; ABuffer: Pointer; const ABufferLen: Integer;
  ASerialNum: Word): Integer;
begin
  Result := SendBuffer( AIP, AHostShortPort, ABuffer, ABufferLen, True, ASerialNum, 0 );
end;

function TUdpIOHandle.SendBufferBySync(const AIP: Cardinal; AHostShortPort: word; ABuffer: Pointer; const ABufferLen: Integer;
  ASerialNum: Word; ATimeStamp: Cardinal): Integer;
begin
  Result := SendBuffer( AIP, AHostShortPort, ABuffer, ABufferLen, False, ASerialNum, ATimeStamp );
end;

procedure TUdpIOHandle.SetMaxUdpPageCount(const Value: Word);
begin
  if (not Active) and (FMaxUdpPackageCount <> Value) then
    FMaxUdpPackageCount := Value;
end;


procedure TUdpIOHandle.SetMtCheckSpaceTime(const Value: Cardinal);
begin
  if (not Active) and (FMtCheckSpaceTime <> Value) then
    FMtCheckSpaceTime := Value;
end;

procedure TUdpIOHandle.SetMtCheckThread(const Value: Boolean);
begin
  if (not Active) and (FMtCheckThread <> Value) then
    FMtCheckThread := Value;
end;

procedure TUdpIOHandle.SetMtEnable(const Value: Boolean);
begin
  if (not Active) and (FMtEnable <> Value) then
    FMtEnable := Value;
end;

procedure TUdpIOHandle.SetMtHashTableCount(const Value: Cardinal);
begin
  if (not Active) and (FMtHashTableCount <> Value) then
    FMtHashTableCount := Value;
end;

procedure TUdpIOHandle.SetMtMaxHashNodeCount(const Value: Cardinal);
begin
  if (not Active) and (FMtHashTableCount <> Value) then
    FMtMaxHashNodeCount := Value;
end;

procedure TUdpIOHandle.SetMtMaxWaitTime(const Value: Cardinal);
begin
  if (not Active) and (FMtMaxWaitTime <> Value) then
    FMtMaxWaitTime := Value;
end;

procedure TUdpIOHandle.SetMtQuerySpaceTime(const Value: Cardinal);
begin
  if (not Active) and (FMtQuerySpaceTime <> Value) then
    FMtQuerySpaceTime := Value;
end;

procedure TUdpIOHandle.SetMtThreadCount(const Value: Word);
begin
  if (not Active) and (FMtThreadCount <> Value) then
    FMtThreadCount := Value;
end;

procedure TUdpIOHandle.SetSinglePackageThreadCount(const Value: Word);
begin
  if (not Active) and (SinglePackageThreadCount <> Value) then
    FSpThreadCount := Value;
end;

procedure TUdpIOHandle.SetSpMaxNodeCount(const Value: Cardinal);
begin
  if (not Active) and (FSpMaxNodeCount <> Value) then
    FSpMaxNodeCount := Value;
end;

procedure TUdpIOHandle.SetSpQuerySpaceTime(const Value: Cardinal);
begin
  if (not Active) and (FSpQuerySpaceTime <> Value) then
    FSpQuerySpaceTime := Value;
end;

function TUdpIOHandle._SendBuffer(AIP: Cardinal; AHostShortPort: word; var ABuffer; const ABufferLen: Integer; const AIsAsynch: Boolean;
  const ASerialNum: Word; const ATimeStamp: Cardinal): Integer;
var
  UdpPackage: TUdpPackage;
  i, nBagCount: Byte;
  p: PAnsiChar;
  nLen: Integer;
  nCount: Integer;
begin
  Result := 0;
  if ABufferLen > CtUdpDataLength * CtMaxCombinationCount then
  begin
    DoErrorInfo( PChar( Format('发送数据不能超过 %d 字节(%dK)', [CtUdpDataLength * CtMaxCombinationCount, ConverByte(CtUdpDataLength * CtMaxCombinationCount) ]) ) );
    Exit;
  end;

  nBagCount := ABufferLen div CtUdpDataLength + 1;
  if nBagCount > 1 then
  begin
    nLen := CtUdpDataLength;
  end
  else
  begin
    nLen := ABufferLen;
  end;

  FillChar( UdpPackage.FUdpPackageContent, CtUdpPackageSize, 0 );
  UdpPackage.FUdpHeader.FUserID := FUserID;
  with UdpPackage.FUdpHeader do
  begin
    FUdpVer := CtCurVer;
    FBagCount := nBagCount;
    FSerialNum := ASerialNum;
    FIsAsynch := Byte(AIsAsynch);
    FTimeStamp := ATimeStamp;
  end;

  p := PAnsiChar( ABuffer );
  for i := 0 to nBagCount - 1 do
  begin
    Move( p^, UdpPackage.FUdpData[0], nLen );
    UdpPackage.FUdpHeader.FPackageSize := nLen + CtUdpHeaderLength;
    UdpPackage.FUdpHeader.FBagSerial := i;
    UdpPackage.FUdpHeader.FCrc32Code := EncodeBuffer( @UdpPackage.FUdpData, nLen );
    nCount := __SendBuffer( AIP, AHostShortPort, UdpPackage.FUdpPackageContent, UdpPackage.FUdpHeader.FPackageSize );
    if nCount <> -1 then
    begin
      Dec( nCount, CtUdpHeaderLength);
      AddSendByte( nCount );
      InterlockedIncrement( FSendTotalPackageCount );
      Result := Result + nCount;
      p := p + nLen;
      if i = (nBagCount - 2) then
        nLen := ABufferLen - CtUdpDataLength * (nBagCount - 1);
    end
    else
      Break;
  end;
end;

procedure FreeObject(var obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  if Assigned( Temp) then
  begin
    Pointer(Obj) := nil;
    Temp.Free;
  end;
end;

end.
