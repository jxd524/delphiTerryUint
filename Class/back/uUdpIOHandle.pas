{
单元名称: uUdpIOHandle
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明:
开始时间: 2009-4-8
修改时间: 2009-4-26 (最后修改时间)
类说明  :
          1: TSendCombiBufferManage
            提供发送组合包时,使用组合包唯一标志(WORD) 和 缓存数据处理
            缓存数据作用: 给接收端提供丢包机制
          2:同步包解决方案 1个字节
            0  0000000: 第1位; 0: 异步包; 1: 同步包.当第一位为 1 时: 后七位表示此同步包的标志
          3: 组合包解决方案 1个字节
            0000  0000: 前四位表示: 包的个数; 0: 独立包; 1 ~ 15: 表示组合包, 接下来的两个字节标志组合包的标志
                        后四位: 当为组合包时,表示包的序号 0 ~ 15
          4: 组合包丢包解决方案
            发送端对发送的组合包进行缓存 由 TSendCombiBufferManage 类管理
            接收端以Hash表进行重组,发现丢包则要求重发 FD/TryCount/LastTime/BagCount/CurRecvCount/package_1..package_n
对子类的要求:
          1: 每个独立包的前两个字节必须为命令ID, 否则有可能与本单元的命令冲突,详细见下面
          2: 子类需要实现 FindAddressID 函数的实现,它的作用: 当接收到组合包时,由IP和PORT获取对应的ID,
             返回值: TRUE表示查找正确,此时ID代表IP和PORT
                     FALSE表示找不到对应的ID,此时无法进行组合包发送
          3: OnRecvBuffer: 处理得到一个完整的数据, 子类主要是实现此函数,对各种命令进行处理
本单元命令:
          1: 发送组合包时的丢包处理命令,它是独立包,命令ID: CtLosePackageIndexCmd = 10
          2: 子类的命令从 CtBasicCmdID 增长
          3: 建议: 命令ID由两个字节表示
限制    :
          1: 同步包只能同时(N个线程同时发送) 发送 128 个同步包
          2: 组合包最大组合只能 16 个包, 长度: CtCombiPackageSize * 16
          3: 独立包没有进行丢包处理
          4: 组合包丢包处理跟发送端的设置有关, CombiSendMaxKeepBufferTime 决定发送组合包缓存时间
                                               CombiRecvFailTractCount    决定丢包时,重新要求发丢包次数
协议头:
      单独包  版本(1) - 此包总长度(2) - CRC32码(4) - 是否同步包(1) - 包个数(1)[值: 0]
      组合包  版本(1) - 此包总长度(2) - CRC32码(4) - 是否同步包(1) - 包个数(1)[值: 1 ~ 15] - 标志(2)
          说明: 同一组合包 标志 相同,



          P2P 网络
NAT分为四种类型：

1. Full Cone：来自相同的内部地址的请求消息映射为相同的外部地址，与外部地址(目的地址)无关。映射关系为P:p↔A:b，
              任何外部主机可通过(A:b)发送到数据到(P:p)上。
2. Restricted Cone：来自相同的内部地址的请求消息映射为相同的外部地址，返回的数据只接受该内部节点曾发数据的那个
                    目的计算机地址X。映射关系为P:p↔A:b↔X，只有来自X的数据包才可通过(A:b)发送到数据到(P:p)上。
3. Port Restricted Cone：来自相同的内部地址的请求消息映射为相同的外部地址，返回的数据只接受该内部节点曾发数据
                         的那个目的地址X:x。映射关系为P:p↔A:b↔X:x，只有来自X:x的数据包才可通过(A:b)发送到数据到(P:p)上。      
4. Symmetric(对称) NAT：只有来自相同的内部地址(P:p)，并且发送到同一个地址(X:x) 的请求消息，才被映射为相同的外部地址(A:b)，
                        返回的数据只接受该内部节点曾发数据的那个目的地址X:x。映射关系为P:p↔A:b↔X:x，
                        当 (P:p)访问(Y:y)时，映射为P:p↔B:c↔Y:y。     

实例：UDP穿越NAT：

　　A登录Server，NAT A分配端口11000，Server得到A的地址为100.10.10.10:11000

　　B登录Server，NAT B分配端口22000，Server得到B的地址为200.20.20.20:22000

　　此时B会把直接来自A的包丢弃，所以要在NAT B上打一个方向为A的洞，那么A就可以向200.20.20.20:22000发送数据了

　　打洞的指令来自Server。B向A的地址100.10.10.10:11000发一个UDP报文，被NAT A丢弃，但在NAT B上建立映射记录，NAT B不在丢弃来自A的报文。

　　Server通知A可以通讯，A发起数据UDP包给B，NAT B放行，B收到A的包，双方开始通讯

　　注：若是对称NAT，当B向A打洞的端口要重新分配(NAT A不会再分配11000端口)，B无法获取这个端口，所以不适用本方法。

实例：TCP穿越NAT：

　　A登录Server，NAT A分配端口11000，Server得到A的地址为100.10.10.10:11000

　　B登录Server，NAT B分配端口22000，Server得到B的地址为200.20.20.20:22000

　　A向B发送TCP数据包SYN:192.168.10.11:1234=>200.20.20.20:22000，在NAT A上打洞

　　B向A发送TCP数据包SYN:192.168.20.22:1234=>100.10.10.10:11000，在NAT B上打洞

　　通道建立，A与B三次握手建立TCP连接
}

unit uUdpIOHandle;

{$I UdpIoHandleOpt.inc}

interface
uses
  uJxdBasicUDP, WinSock2, Windows, Classes, uQueues, uDataNotifyQueues, SysUtils, uJxdDataStruct,
  Encrypt {$IFDEF LOGINFO} ,uDebugInfo {$ENDIF};

const
{设计协议本身限制设置值}
  CtMaxSyncPackageCount = 128; //同步包并发不能超过 128 个(0000000 ~ 1111111); 7位
  CtMaxCombiPackageID = 65536; //最大组合包标志总量 1 ~ 65536
  CtMaxCombiPackageCount = 16;   { 组合包不能超过 16个, 数值为0..15; A: 0000 B: 0000;
                                前四位:
                                        A区: 包个数; 0: 单独包; 1 ~ 15: 2 ~ 16个包
                                后四位:
                                        B区: 包的序号; 对单独包无用,组合包序号: (0000 ~ 1111) 0 ~ 15 }

{广域网UDP限制}
  CtMTU = 1500;      //中国大部分路由MTU值; 避免IP分片
  CtRawIpHead = 20;  //TCP/IP协议中原始IP头
  CtRawUdpHead = 8;  //TCP/IP协议中原始UDP头
{本身使用常规常量}
  CtMaxUdpSize = CtMTU - CtRawIpHead - CtRawUdpHead; //最大发送包长度
  CtSinglePackageHeadSize = 8;                       //单独包包头长度  版本(1) - 此包总长度(2) - CRC32码(4) - 包个数(1)[值: 0]
  CtSinglePackageSize = CtMaxUdpSize - CtSinglePackageHeadSize; //独立包最大数据区长度
  CtCombiPackageHeadSize = 10;                       //组合包包头长度  版本(1) - 此包总长度(2) - CRC32码(4) - 包个数(1)[值: 1 ~ 15] - 标志(2)
  CtCombiPackageSize = CtMaxUdpSize - CtCombiPackageHeadSize;   //组合包最大数据区长度
  CtBasicCmdID = 100; //命令ID，100以下为此单元命令处理ID，如： 重发丢包数据

type
  TUdpIoHandle = class;

  {
  单独包  版本(1) - 此包总长度(2) - CRC32码(4) - 是否同步包(1) - 包个数(1)[值: 0]
  组合包  版本(1) - 此包总长度(2) - CRC32码(4) - 是否同步包(1) - 包个数(1)[值: 1 ~ 15] - 标志(2)
      说明: 同一组合包 标志 相同,此标志由发送方的组合ID
  }

  { ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    发送端缓存发送数据管理类                           TSendCombiBufferManage
    仅针对组合包有用
    提供组合包的缓存,以便接收包要求重发丢失的包
    发送端组合包序号管理, 2个字节的组合包标志,限制了最多并发 65536 个组合包

  } ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  
  //组合包信息
  pCombiBufInfo = ^TCombiBufInfo;
  TCombiBufInfo = record
    FCombiID: Word;
    FLock: Boolean;
    FActiveTime: Cardinal;
    FPackages: array[0..0] of Pointer;
  end;
  TOnFreeCombiBuffer = procedure(const ACount: Byte; const APackage: array of Pointer) of object;
  TSendCombiBufferManage = class
  private
    FCapacity: Integer;
    {发送端组合序号管理}
    FCombiIdList: array of Word;
    FHead: Word;
    FTail: Word;
    FCount: Integer;
    FCombiIdLock: TRTLCriticalSection;
    function  IsCanReclaimIndex(const AIndex: Word): Boolean;
  private
    {发送端缓存机制}
    FMaxCombiCount: Byte;
    FCombiBufInfoSize: Word;
    FCombiBufList: TList;
    FCheckCombiBuf: TCheckThread;
    FCombiBufListLock: TRTLCriticalSection;
    FCombiBufMem: TStaticMemoryManager;
    procedure DoCheckCombiBufLock(Sender: TObject);
    function  FindCombiBuffer(const ACombiID: Word; var AOrgLockState: Boolean): pCombiBufInfo;
    procedure FreeCombiBuffer(const p: pCombiBufInfo);
  private
    {自动生成}
    FCheckSpaceTime: Cardinal;
    FMaxKeepBufferTime: Cardinal;
    FOnFreeCombiBuffer: TOnFreeCombiBuffer;
    procedure SetCheckSpaceTime(const Value: Cardinal);
    procedure SetMaxKeepBufferTime(const Value: Cardinal);
    function  GetUsedCombiIdCount: Integer;
    function  GetCombiBufferCount: Integer;
    function  GetPackageCount: Integer;
  public
    constructor Create(const ACapacity: Integer; const AMaxCombiCount: Byte);
    destructor Destroy; override;
    {发送组合包序号管理}
    function  GetCombiID(var AID: Word): Boolean;
    procedure ReclaimCombiID(const ACombiID: Word);
    {发送组合包缓存处理}
    function  AddCombiBuffer(const ACombiID: Word; const ALock: Boolean; const APackageIndex: Byte; const ApPackage: Pointer): Boolean; //ALock: 自己主动释放
    function  GetCombiPackage(const ACombiID: Word; const APackageIndex: Byte; var p: Pointer): Boolean;
    procedure RelcaimCombiBuffer(const ACombiID: Word);  //主动释放, 连同组合包序号也释放
    {缓存相关设置参数}
    property CheckSpaceTime: Cardinal read FCheckSpaceTime write SetCheckSpaceTime;
    property MaxKeepBufferTime: Cardinal read FMaxKeepBufferTime write SetMaxKeepBufferTime;
    {统计信息}
    property UsedCombiIdCount: Integer read GetUsedCombiIdCount; //组合序号可用数量
    property CombiBufferCount: Integer read GetCombiBufferCount; //缓存数量
    property PackageCount: Integer read GetPackageCount;         //缓存包的数量
    {事件}
    property OnFreeCombiBuffer: TOnFreeCombiBuffer read FOnFreeCombiBuffer write FOnFreeCombiBuffer;
  end;

  { ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    接收端处理组合包类
    提供: 组合包重组,丢包重发
  } ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  pCombiInfo = ^TCombiInfo;
  TCombiInfo = record
    FIP: Cardinal;
    FPort: Word;
    FSyncSige: Byte;
    FCombiID: Word;
  end;
  TRecvCombiBufferManage = class
  private
    FOwner: TUdpIoHandle;
    FCombiPackage: TCombiNotify;
    FCombiInfoMem: TStaticMemoryManager;
    FMaxTractCount: Cardinal;
    FWaitTime: Cardinal;
    function  DoCombiPackageRecvOK(Sender: TObject; const ApData: Pointer; const ADataArray: array of Pointer; const ALen: Integer; const AAryCount: Byte): Boolean;
    function  DoCombiPackageRecvFail(Sender: TObject; const ATractCount: Byte; const ApData: Pointer; const ADataArray: array of Pointer; const AAryCount: Byte): Boolean;
    procedure SetMaxtractCount(const Value: Cardinal);
    procedure SetWaitTime(const Value: Cardinal);
    function  GetCurCount: Integer;
  public
    constructor Create(AOwner: TUdpIoHandle);
    destructor  Destroy; override;
    function  AddRecvCombiPackage(const AIP: Cardinal; const APort, APackageID: Word; const ASyncSige: Byte; const APackageCount, APackageIndex: Byte;
                                  const ApPackage: Pointer; const ApPackageLen: Word): Integer;
    property MaxTractCount: Cardinal read FMaxTractCount write SetMaxtractCount;
    property MaxWaitTime: Cardinal read FWaitTime write SetWaitTime;
    property CurCount: Integer read GetCurCount;
  end;

  { ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    Udp处理管理类                                    TUdpIoHandle
    实现: 单包发送
          组合包发送,提供丢包处理
          重组数据操作
          同步异步处理

  } ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TUdpIoHandle = class(TxdBasicUDP)
  protected
    FErrorPackageCount: Integer;
    FCombiSendBuffer: TSendCombiBufferManage;
    FCombiRecvBuffer: TRecvCombiBufferManage;
  private
    FCombiMem: TStaticMemoryManager; //提供发送组合包重发机制内存，及接收到组合包的内存缓存
    procedure DoFreeCombiPackage(const ACount: Byte; const APackage: array of Pointer);
    procedure OnRecvCombiPackage(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Word;
                                 const ASyncSign, APackageCount, APackageIndex: Byte; const ACombiSign: Word);
    procedure DoHandleCmd_LosePackageIndex(const AIP: Cardinal; const APort: Word; const ApBuffer: PAnsiChar;
                                           const ABufLen: Word; const ASyncSign: Byte);
  private
    {统计信息}
    FSendByteCount: Cardinal;
    FSendPackaegCount: Cardinal;
    FSendStatLock: TRTLCriticalSection;
    FRecvPackageCount: Cardinal;
    FRecvByteCount: Cardinal;
    FRecvStatLock: TRTLCriticalSection;
  private
    {属性自动生成信息}
    FCombiMaxPackageCount: Byte;
    FCombiRecvHashTableCount: Cardinal;
    FCombiRecvMaxHashNode: Cardinal;
    FCombiRecvCheckTST: Cardinal;
    FCombiRecvMaxWaitTime: Cardinal;
    FCombiRecvFailTractCount: Byte;
    FCombiSendCheckSpaceTime: Cardinal;
    FCombiSendMaxKeepBufferTime: Cardinal;
    FMaxCombiMemCount: Cardinal;
    FActiveTime: Cardinal;
    procedure SetCombiMaxPackageCount(const Value: Byte);
    procedure SetCombiRecvHashTableCount(const Value: Cardinal);
    procedure SetCombiRecvMaxHashNode(const Value: Cardinal);
    procedure SetCombiRecvCheckTST(const Value: Cardinal);
    procedure SetCombiRecvMaxWaitTime(const Value: Cardinal);
    procedure SetCombiRecvFailTractCount(const Value: Byte);
    procedure SetCombiSendCheckSpaceTime(const Value: Cardinal);
    procedure SetCombiSendMaxKeepBufferTime(const Value: Cardinal);
    function  GetCombiBufferCount: Integer;
    function  GetCombiUsedIdCount: Integer;
    function  GetCombiPackageCount: Integer;
    procedure SetMaxCombiMemCount(const Value: Cardinal);
    function  GetCombiRecvCurCount: Integer;
  protected
    {父类虚函数}
    procedure DoRecvBuffer; override;   //由线程调用
    function  DoBeforOpenUDP: Boolean; override;  //初始化UDP前; True: 允许初始化; False: 不允许初始化
    procedure DoAfterOpenUDP; override;
    procedure DoBeforCloseUDP; override;
    procedure DoAfterCloseUDP; override; //UDP关闭之后
    {自身实现功能}
    function  _SendBuffer(const AIP: Cardinal; const APort: Word; const ApBuffer: PAnsiChar; const ABufLen: Word; const ASyncSign: Byte = 0): Integer;
    function  _DoSendPackage(const AIP: Cardinal; const APort: Word; const ApPackageAddr: pAnsiChar; const APackageLen: Word): Integer;
    function  _SendSinglePackage(const AIP: Cardinal; const APort: Word; const ApPackageAddr: pAnsiChar;
                                 const ASyncSign: Byte; const ApBuffer: pAnsiChar; const ABufferSize: Integer): Integer;
    function  _SendCombiPackage(const AIP: Cardinal; const APort: Word; const ASyncSign: Byte; const ACombiID: Word;
                                const ApBuffer: pAnsiChar; const ABufferSize: Word): Integer;
    {需要子类实现}
    function  FindAddressID(const AIP: Cardinal; const APort: Word; var AID: Cardinal): Boolean; virtual;  //当接收到组合包时使用
    procedure OnRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
                           const ASyncSign: Byte); virtual; //接收到完整数据总处理处
  protected
    {$IFDEF LOGINFO}
    procedure DoErrorInfo(const AInfo: PAnsiChar); override;
    {$ENDIF}
  public
    constructor Create; override;
    destructor  Destroy; override;

    function SendBuffer(const AIP: Cardinal; const APort: Word; const ApBuffer: PAnsiChar; const ABufLen: Word): Integer;

    {数据处理内存参数}
    property MaxCombiMemCount: Cardinal read FMaxCombiMemCount write SetMaxCombiMemCount;  //最大组合包内存申请数量
    {组合包相关属性}
    property CombiMaxPackageCount: Byte read FCombiMaxPackageCount write SetCombiMaxPackageCount; //最多允许组合包的个数
    
    property CombiRecvHashTableCount: Cardinal read FCombiRecvHashTableCount write SetCombiRecvHashTableCount;   //接收端组合包的Hash桶数
    property CombiRecvMaxHashNode: Cardinal read FCombiRecvMaxHashNode write SetCombiRecvMaxHashNode; //接收组合包最大允许值 (存放包的指针)
    property CombiRecvCheckThreadSpaceTime: Cardinal read FCombiRecvCheckTST write SetCombiRecvCheckTST; //组合包检查线程间隔时间
    property CombiRecvMaxWaitTime: Cardinal read FCombiRecvMaxWaitTime write SetCombiRecvMaxWaitTime;  //组合包最大等待时间
    property CombiRecvFailTractCount: Byte read FCombiRecvFailTractCount write SetCombiRecvFailTractCount; //组合包接收失败最多重发取数据次数

    property CombiSendMaxKeepBufferTime: Cardinal read FCombiSendMaxKeepBufferTime write SetCombiSendMaxKeepBufferTime; //组合包最大存活时间
    property CombiSendCheckSpaceTime: Cardinal read FCombiSendCheckSpaceTime write SetCombiSendCheckSpaceTime; //发送端缓存检查时间间隔
    {统计信息}
         {流量信息}
    property SendPackageCount: Cardinal read FSendPackaegCount;
    property SendByteCount: Cardinal read FSendByteCount;
    property RecvPackageCount: Cardinal read FRecvPackageCount;
    property RecvByteCount: Cardinal read FRecvByteCount;
    property ErrorPackageCount: Integer read FErrorPackageCount;
         {内存信息}
    property CombiSendIdUsedCount: Integer read GetCombiUsedIdCount;  //组合序号已使用数量
    property CombiSendBufferCount: Integer read GetCombiBufferCount;  //缓存数量,还保存在内存中的发送信息
    property CombiSendPackageCount: Integer read GetCombiPackageCount;//缓存包的数量, 一个组合包有多个缓存包

    property CombiRecvCurCount: Integer read GetCombiRecvCurCount;

    property ActiveTime: Cardinal read FActiveTime;
  end;

function  FormatSyncSign(const AIsSync: Boolean; const ASyncID: Byte): Byte;
function  FormatPackageSign(const APackageCount, APackageIndex: Byte): Byte;
procedure ParseSyncSign(const ASign: Byte; var AIsSync: Boolean; var ASyncID: Byte);
procedure ParsePackageSign(const ASign: Byte; var APackageCount: Byte; var APackageIndex: Byte);

procedure UdpIoHandleDebug(const AInfo: string); overload;
procedure UdpIoHandleDebug(const AInfo: string; const Args: array of const); overload;

implementation

procedure UdpIoHandleDebug(const AInfo: string);
begin
  OutputDebugString( PChar(AInfo) );
  {$IFDEF LOGINFO}
  _Log( AInfo, 'UdpIoHandle.txt');
  {$ENDIF}
end;

procedure UdpIoHandleDebug(const AInfo: string; const Args: array of const);
begin
  UdpIoHandleDebug( Format(AInfo, Args) );
end;

const
  {类自身使用常量}
  CtCombiBufInfoSize = SizeOf( TCombiBufInfo );
  CtCombiInfoSize = SizeOf( TCombiInfo );
  CtCurUDPVerNumber = 1;


type
  pLosePackageIndex = ^TLosePackageIndex;
  TLosePackageIndex = packed record
    FCmd: Word;
    FCombiID: Word;
    FLoseCount: Byte;
    FLoseIndexs: array[0..0] of Byte;
  end;
const
  CtLosePackageIndexCmd = 10;      //要求重发丢包信息
  CtLosePackageSize = SizeOf(TLosePackageIndex);


function FormatSyncSign(const AIsSync: Boolean; const ASyncID: Byte): Byte;
begin
  Result := 0;
  if AIsSync then
  begin
    Result := Result or ASyncID;
    Result := Result or (1 shl 7);
  end;
end;

function FormatPackageSign(const APackageCount, APackageIndex: Byte): Byte;
begin
  Result := 0;
  Result := Result or APackageIndex;
  Result := Result or ( APackageCount shl 4);
end;

procedure ParseSyncSign(const ASign: Byte; var AIsSync: Boolean; var ASyncID: Byte);
begin
  AIsSync := ASign and (1 shl 7) <> 0;
  if AIsSync then
    ASyncID := ASign xor (1 shl 7)
  else
    ASyncID := 0;
end;

procedure ParsePackageSign(const ASign: Byte; var APackageCount: Byte; var APackageIndex: Byte);
begin
  APackageCount := ASign shr 4;
  APackageIndex := ASign shl 4;
  APackageIndex := APackageIndex shr 4;
end;

{ TUdpIoHandle }

constructor TUdpIoHandle.Create;
begin
  inherited;
  FCombiMaxPackageCount := 8;
  FCombiRecvHashTableCount := 1024;
  FCombiRecvMaxHashNode := 512;
  FCombiRecvCheckTST := 300;
  FCombiRecvMaxWaitTime := 3 * 1000;
  FCombiSendCheckSpaceTime := 1000;
  FCombiSendMaxKeepBufferTime := 3000;
  FMaxCombiMemCount := 1024;
  FActiveTime := 0;
end;

destructor TUdpIoHandle.Destroy;
begin

  inherited;
end;

procedure TUdpIoHandle.DoAfterCloseUDP;
begin
  inherited;
  FActiveTime := 0;
  FreeAndNil( FCombiSendBuffer );
  FreeAndNil( FCombiRecvBuffer );
  FreeAndNil( FCombiMem );
  DeleteCriticalSection( FSendStatLock );
  DeleteCriticalSection( FRecvStatLock );
end;

procedure TUdpIoHandle.DoAfterOpenUDP;
begin
  FActiveTime := GetTickCount;
end;

procedure TUdpIoHandle.DoBeforCloseUDP;
begin

end;

function TUdpIoHandle.DoBeforOpenUDP: Boolean;
begin
  Result := True;
  try
    FSendByteCount := 0;
    FSendPackaegCount := 0;
    FRecvPackageCount := 0;
    FRecvByteCount := 0;
    FErrorPackageCount := 0;
    InitializeCriticalSection( FSendStatLock );
    InitializeCriticalSection( FRecvStatLock );

    FCombiSendBuffer := TSendCombiBufferManage.Create( CtMaxCombiPackageID, CombiMaxPackageCount );
    FCombiSendBuffer.MaxKeepBufferTime := CombiSendMaxKeepBufferTime;
    FCombiSendBuffer.CheckSpaceTime := CombiSendCheckSpaceTime;
    FCombiSendBuffer.OnFreeCombiBuffer := DoFreeCombiPackage;
    
    FCombiMem := TStaticMemoryManager.Create( CtMaxUdpSize, MaxCombiMemCount );

    FCombiRecvBuffer := TRecvCombiBufferManage.Create( Self );
    FCombiRecvBuffer.MaxTractCount := CombiRecvFailTractCount;
    FCombiRecvBuffer.MaxWaitTime := 1024;
  except
    Result := False;
  end;
end;

{$IFDEF LOGINFO}
procedure TUdpIoHandle.DoErrorInfo(const AInfo: PAnsiChar);
begin
  UdpIoHandleDebug( AInfo );
end;
{$ENDIF}

procedure TUdpIoHandle.DoFreeCombiPackage(const ACount: Byte; const APackage: array of Pointer);
var
  i: Byte;
begin
  for i := 0 to ACount - 1 do
  begin
    if APackage[i] <> nil then
      FCombiMem.FreeMem( APackage[i] );
  end;
end;

procedure TUdpIoHandle.DoHandleCmd_LosePackageIndex(const AIP: Cardinal; const APort: Word; const ApBuffer: PAnsiChar; const ABufLen: Word;
  const ASyncSign: Byte);
var
  p: pLosePackageIndex;
  nIndex: Byte;
  i: Integer;
  pData: Pointer;
  nLen: Word;
begin
  p := pLosePackageIndex( ApBuffer );
  for i := 0 to p^.FLoseCount - 1 do
  begin
    nIndex := p^.FLoseIndexs[i];
    if FCombiSendBuffer.GetCombiPackage(p^.FCombiID, nIndex, pData) then
    begin
      DoErrorInfo( '成功请求重新发送丢失的包' );
      Move( PAnsiChar(Integer(pData) + 1)^, nLen, 2 );
      _DoSendPackage( AIP, APort, PAnsiChar(pData), nLen );
    end
    else
    begin
      DoErrorInfo( '请求丢失的包已经被遗弃' );
    end;
  end;
end;

procedure TUdpIoHandle.DoRecvBuffer;
var
  Package: array[0..CtMaxUdpSize - 1] of AnsiChar;
  nLen: Integer;
  addr: TSockAddrIn;
  nVer, nSyncSign, nPackageSign, nPackageCount, nPackageIndex: Byte;
  nCmd, nPackageLen, nCombiSign: Word;
  nCrc32Code: Cardinal;
  pData: PAnsiChar;
  nIP: Cardinal;
  nPort: Word;
begin
  nLen := CtMaxUdpSize;
  if not __RecvBuffer( Package, nLen, addr ) then
  begin
    DoErrorInfo( WSAGetLastError, '__RecvBuffer' );
    Exit;
  end;

  EnterCriticalSection( FRecvStatLock );
  try
    Inc( FRecvPackageCount );
    Inc( FRecvByteCount, nLen );
  finally
    LeaveCriticalSection( FRecvStatLock );
  end;

  if nLen <= CtSinglePackageHeadSize then
  begin
    InterlockedIncrement( FErrorPackageCount );
    DoErrorInfo( '接收到的数据长度过小' );
    Exit;
  end;
  //单包   版本(1) - 此包总长度(2) - CRC32码(4) - 包个数(1)[值: 0]
  //组合包 版本(1) - 此包总长度(2) - CRC32码(4) - 包个数(1)[前四位：个数； 后四位: 包顺序] - 标志(2)
  nVer := Byte(Package[0]);
  if nVer <> CtCurUDPVerNumber then
  begin
    InterlockedIncrement( FErrorPackageCount );
    DoErrorInfo( '当前协议版本有误' );
    Exit;
  end;
  Move( Package[1], nPackageLen, 2 );
  if nPackageLen <> nLen then
  begin
    InterlockedIncrement( FErrorPackageCount );
    DoErrorInfo( '包的长度有误' );
    Exit;
  end;
  Move( Package[3], nCrc32Code, 4 );
//  Move( Package[7], nSyncSign, 1 );
  Move( package[7], nPackageSign, 1);

  if nPackageSign = 0 then
  begin
    //独立包
    pData := @Package[CtSinglePackageHeadSize];
    nLen := nLen - CtSinglePackageHeadSize;
    if not DecodeBuffer( nCrc32Code, pData, nLen ) then
    begin
      InterlockedIncrement( FErrorPackageCount );
      DoErrorInfo( 'CRC32解密出错' );
      Exit;
    end;
    Move( pData^, nCmd, 2 );
    nIP := addr.sin_addr.S_addr;
    nPort := htons(addr.sin_port);
    if nCmd = CtLosePackageIndexCmd then
      DoHandleCmd_LosePackageIndex( nIP, nPort, pData, nLen, nSyncSign )
    else
      OnRecvBuffer( nIP, nPort, pData, nLen, nSyncSign )
  end
  else
  begin
    //组合包
    if nLen <= CtCombiPackageHeadSize then
    begin
      InterlockedIncrement( FErrorPackageCount );
      DoErrorInfo( '接收到的数据长度过小' );
      Exit;
    end;
    Move( Package[8], nCombiSign, 2 );
    pData := @Package[CtCombiPackageHeadSize];
    nLen := nLen - CtCombiPackageHeadSize;
    if not DecodeBuffer( nCrc32Code, pData, nLen ) then
    begin
      InterlockedIncrement( FErrorPackageCount );
      DoErrorInfo( 'CRC32解密出错' );
      Exit;
    end;
    ParsePackageSign( nPackageSign, nPackageCount, nPackageIndex );
    nIP := addr.sin_addr.S_addr;
    nPort := htons(addr.sin_port);
    OnRecvCombiPackage( nIP, nPort, pData, nLen, nSyncSign, nPackageCount + 1, nPackageIndex, nCombiSign );
  end;
end;

function TUdpIoHandle.FindAddressID(const AIP: Cardinal; const APort: Word; var AID: Cardinal): Boolean;
begin
  Result := True;
  AID := 0;
end;

function TUdpIoHandle.GetCombiBufferCount: Integer;
begin
  if Assigned(FCombiSendBuffer) then
    Result := FCombiSendBuffer.CombiBufferCount
  else
    Result := 0;
end;

function TUdpIoHandle.GetCombiUsedIdCount: Integer;
begin
  if Assigned(FCombiSendBuffer) then
    Result := FCombiSendBuffer.UsedCombiIdCount
  else
    Result := 0;
end;

procedure TUdpIoHandle.OnRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const ASyncSign: Byte);
begin
  DoErrorInfo( ABuffer );
end;

procedure TUdpIoHandle.OnRecvCombiPackage(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Word; const ASyncSign, APackageCount,
  APackageIndex: Byte; const ACombiSign: Word);
var
  p: pAnsiChar;
begin
  if FCombiMem.GetMem(Pointer(p)) then
  begin
    //添加两个字节的数据长度在数据区前面
    Move( ABufLen, p^, 2 );
    Move( ABuffer^, PAnsiChar(p + 2)^, ABufLen );
    FCombiRecvBuffer.AddRecvCombiPackage( AIP, APort, ACombiSign, ASyncSign, APackageCount, APackageIndex, p, ABufLen );
  end
  else
  begin
    DoErrorInfo( 'IO处理类内存不足以接收到来的数据' );
  end;
end;

function TUdpIoHandle.GetCombiPackageCount: Integer;
begin
  if Assigned(FCombiSendBuffer) then
    Result := FCombiSendBuffer.PackageCount
  else
    Result := 0;
end;

function TUdpIoHandle.GetCombiRecvCurCount: Integer;
begin
  if Active then
    Result := FCombiRecvBuffer.CurCount
  else
    Result := 0;
end;

function TUdpIoHandle._SendBuffer(const AIP: Cardinal; const APort: Word; const ApBuffer: PAnsiChar; const ABufLen: Word; const ASyncSign: Byte): Integer;
var
  pg: array[0..CtMaxUdpSize] of AnsiChar;
  nCombiID: Word;
begin
  if ABufLen <= CtSinglePackageSize then
    Result := _SendSinglePackage( AIP, APort, PAnsiChar(@pg), ASyncSign, ApBuffer, ABufLen )
  else
  begin
    //新创建一个组合包标志，由 FCombiSendBuffer 对其进行回收
    if not FCombiSendBuffer.GetCombiID(nCombiID) then
    begin
      DoErrorInfo( '无法获取组合包序号' );
      Result := -1;
      Exit;
    end;
    Result := _SendCombiPackage( AIP, APort, ASyncSign, nCombiID, ApBuffer, ABufLen );
  end;
end;

function TUdpIoHandle.SendBuffer(const AIP: Cardinal; const APort: Word; const ApBuffer: PAnsiChar; const ABufLen: Word): Integer;
begin
  Result := _SendBuffer( AIP, APort, ApBuffer, ABufLen );
end;

procedure TUdpIoHandle.SetCombiMaxPackageCount(const Value: Byte);
begin
  if (not Active) and (Value > 1) and (Value <= CtMaxCombiPackageCount) then
    FCombiMaxPackageCount := Value;
end;

procedure TUdpIoHandle.SetCombiRecvCheckTST(const Value: Cardinal);
begin
  if (not Active) and (Value <> FCombiRecvCheckTST) then
    FCombiRecvCheckTST := Value;
end;

procedure TUdpIoHandle.SetCombiRecvFailTractCount(const Value: Byte);
begin
  if (not Active) and (Value <> FCombiRecvFailTractCount) then
  begin
    FCombiRecvFailTractCount := Value;
    if Assigned(FCombiRecvBuffer) then
      FCombiRecvBuffer.MaxTractCount := FCombiRecvFailTractCount;
  end;
end;

procedure TUdpIoHandle.SetCombiRecvHashTableCount(const Value: Cardinal);
begin
  if (not Active) and (Value <> FCombiRecvHashTableCount) then
    FCombiRecvHashTableCount := Value;
end;

procedure TUdpIoHandle.SetCombiRecvMaxHashNode(const Value: Cardinal);
begin
  if (not Active) and (Value <> FCombiRecvMaxHashNode) then
    FCombiRecvMaxHashNode := Value;
end;

procedure TUdpIoHandle.SetCombiRecvMaxWaitTime(const Value: Cardinal);
begin
  if Value <> FCombiRecvMaxWaitTime then
  begin
    FCombiRecvMaxWaitTime := Value;
    if Assigned(FCombiRecvBuffer) then
      FCombiRecvBuffer.MaxWaitTime := Value;
  end;
end;

procedure TUdpIoHandle.SetCombiSendCheckSpaceTime(const Value: Cardinal);
begin
  if FCombiSendCheckSpaceTime <> Value then
  begin
    FCombiSendCheckSpaceTime := Value;
    if Assigned(FCombiSendBuffer) then
      FCombiSendBuffer.CheckSpaceTime := FCombiSendCheckSpaceTime;
  end;
end;

procedure TUdpIoHandle.SetCombiSendMaxKeepBufferTime(const Value: Cardinal);
begin
  if FCombiSendMaxKeepBufferTime <> Value then
  begin
    FCombiSendMaxKeepBufferTime := Value;
    if Assigned(FCombiSendBuffer) then
      FCombiSendBuffer.MaxKeepBufferTime := FCombiSendMaxKeepBufferTime;
  end;
end;

procedure TUdpIoHandle.SetMaxCombiMemCount(const Value: Cardinal);
begin
  if (not Active) and (Value <> FMaxCombiMemCount) then
    FMaxCombiMemCount := Value;
end;

function TUdpIoHandle._DoSendPackage(const AIP: Cardinal; const APort: Word; const ApPackageAddr: pAnsiChar; const APackageLen: Word): Integer;
var
  nErrorCode: Integer;
label
  llTryWouldBlock;
begin
llTryWouldBlock:
  Result := __SendBuffer( AIP, APort, ApPackageAddr^, APackageLen );
  if Result <> -1 then
  begin
    EnterCriticalSection( FSendStatLock );
    try
      Inc( FSendByteCount, Result );
      Inc( FSendPackaegCount );
    finally
      LeaveCriticalSection( FSendStatLock );
    end;
  end
  else
  begin
    nErrorCode := WSAGetLastError;
    if nErrorCode = WSAEWOULDBLOCK then
    begin
      DoErrorInfo( '发送缓冲区已满，程序将自动重试' );
      Sleep( 5 );
      goto llTryWouldBlock;
    end
    else
      DoErrorInfo( nErrorCode, '__SendBuffer' );
  end;
end;

function TUdpIoHandle._SendCombiPackage(const AIP: Cardinal; const APort: Word; const ASyncSign: Byte; const ACombiID: Word; const ApBuffer: pAnsiChar;
  const ABufferSize: Word): Integer;
var
  pPackage: PAnsiChar;
  nPackageSing, nPackageIndex, nPackageCount, _nPackageCount: Byte;
  nPackageLen, nDataLen: Word;
  nCrc32Code: Cardinal;
  pBuffer: pAnsiChar;
  nSendByteCount: Integer;
begin
  //版本(1) - 此包总长度(2) - CRC32码(4) - 是否同步包(1) - 包个数(1)[值: 1 ~ 15] - 标志(2)
  nPackageCount := (ABufferSize + CtCombiPackageSize - 1) div CtCombiPackageSize;
  if nPackageCount = 1 then
  begin
    Result := -1;
    DoErrorInfo( '发送数据量过少，请使用单独包发送' );
    Exit;
  end;
  if nPackageCount > CombiMaxPackageCount then
  begin
    Result := -1;
    DoErrorInfo( '发送数据量过多，超过最多许可，无法发送' );
    Exit;
  end;
  
  Result := 0;
  _nPackageCount := nPackageCount - 1;
  pBuffer := ApBuffer;
  for nPackageIndex := 0 to nPackageCount - 1 do
  begin
    if not FCombiMem.GetMem(Pointer(pPackage)) then
    begin
      DoErrorInfo( '申请的缓存不足，请等待' );
      FCombiSendBuffer.ReclaimCombiID( ACombiID );
      Exit;
    end;
    if nPackageIndex = nPackageCount - 1 then
      nDataLen := ABufferSize - (nPackageCount - 1) * CtCombiPackageSize
    else
      nDataLen := CtCombiPackageSize;
    nCrc32Code := EncodeBuffer( pBuffer, nDataLen );
    nPackageLen := CtCombiPackageHeadSize + nDataLen;
    nPackageSing := FormatPackageSign( _nPackageCount, nPackageIndex );

    pPackage^ := Char( CtCurUDPVerNumber );                              //协议版本号
    Move( nPackageLen, pAnsiChar( Integer(pPackage) + 1 )^, 2 );         //此包总长度
    Move( nCrc32Code, pAnsiChar( Integer(pPackage) + 3 )^, 4 );          //CRC32码
//    Move( ASyncSign, pAnsiChar( Integer(pPackage) + 7 )^, 1 );           //是否同步包
//    Move( nPackageSing, pAnsiChar( Integer(pPackage) + 8 )^, 1 );        //包个数
//    Move( ACombiID, pAnsiChar( Integer(pPackage) + 9 )^, 2 );            //组合包标志
    Move( nPackageSing, pAnsiChar( Integer(pPackage) + 7 )^, 1 );        //包个数
    Move( ACombiID, pAnsiChar( Integer(pPackage) + 8 )^, 2 );            //组合包标志
    Move( pBuffer^, pAnsiChar( Integer(pPackage) + CtCombiPackageHeadSize )^, nDataLen ); //数据

    nSendByteCount := _DoSendPackage( AIP, APort, pPackage, nPackageLen );
    if nSendByteCount = -1 then  //发送失败
    begin
      DoErrorInfo( '发送组合包失败' );
      FCombiSendBuffer.ReclaimCombiID( ACombiID );
      FCombiMem.FreeMem( pPackage );
      Result := -1;
      Exit;
    end
    else  //发送成功
    begin
      Dec( nSendByteCount, CtCombiPackageHeadSize );
      Inc( Result, nSendByteCount );
      if not FCombiSendBuffer.AddCombiBuffer( ACombiID, nPackageIndex + 1 <> nPackageCount, nPackageIndex, pPackage ) then
      begin
        FCombiMem.FreeMem( pPackage );
        DoErrorInfo( PChar(Format('无法加入缓存列表，组合包%d, 序号%d 将无法提供重发机制', [ACombiID, nPackageIndex])) );
      end;
    end;

    pBuffer := pAnsiChar( Integer(pBuffer) + nDataLen );
  end;
end;

function TUdpIoHandle._SendSinglePackage(const AIP: Cardinal; const APort: Word; const ApPackageAddr: pAnsiChar; const ASyncSign: Byte;
  const ApBuffer: pAnsiChar; const ABufferSize: Integer): Integer;
var
  nPackageLen: Word;
  nCrc32Code: Cardinal;
  nPackageCount: Byte;
begin
  //发送单个包,数据长度小于等待 CtSinglePackageSize 包头 9 个字节
  //版本(1) - 此包总长度(2) - CRC32码(4) - 是否同步包(1) - 包个数(1)[值: 0]
  nPackageLen := CtSinglePackageHeadSize + ABufferSize;
  nCrc32Code := EncodeBuffer( ApBuffer, ABufferSize );
  nPackageCount := 0;

  ApPackageAddr^ := Char( CtCurUDPVerNumber );                              //版本
  Move( nPackageLen, pAnsiChar( Integer(ApPackageAddr) + 1 )^, 2 );         //此包总长度
  Move( nCrc32Code, pAnsiChar( Integer(ApPackageAddr) + 3 )^, 4 );          //CRC32码
//  Move( ASyncSign, pAnsiChar( Integer(ApPackageAddr) + 7 )^, 1 );           //是否同步包
  Move( nPackageCount, pAnsiChar( Integer(ApPackageAddr) + 7 )^, 1 );       //包个数
  Move( ApBuffer^, pAnsiChar( Integer(ApPackageAddr) + CtSinglePackageHeadSize )^, ABufferSize ); //数据

  Result := _DoSendPackage( AIP, APort, ApPackageAddr, nPackageLen );
  if Result <> -1 then
    Dec( Result, CtSinglePackageHeadSize );
end;

{ TSendCombiBufferManage }

function TSendCombiBufferManage.AddCombiBuffer(const ACombiID: Word; const ALock: Boolean; const APackageIndex: Byte; const ApPackage: Pointer): Boolean;
var
  p: pCombiBufInfo;
  bNew: Boolean;
begin
  if APackageIndex >= FMaxCombiCount then
  begin
    Result := False;
    Exit;
  end;
  p := FindCombiBuffer( ACombiID, bNew );
  if p = nil then
  begin
    Result := FCombiBufMem.GetMem( Pointer(p) );
    if Result then
    begin
      FillChar( p^, FCombiBufInfoSize, 0 );
      p^.FCombiID := ACombiID;
    end;
    bNew := True;
  end
  else
  begin
    Result := True;
    bNew := False;
  end;
  if not Result then Exit;
  p^.FActiveTime := GetTickCount;
  if p^.FPackages[APackageIndex] = nil then
    p^.FPackages[APackageIndex] := ApPackage
  else
  begin
    OnFreeCombiBuffer( 1, [p^.FPackages[APackageIndex]] );
    p^.FPackages[APackageIndex] := ApPackage;
  end;
  p^.FLock := ALock;
  if bNew then
  begin
    EnterCriticalSection( FCombiBufListLock );
    try
      FCombiBufList.Add( p );
    finally
      LeaveCriticalSection( FCombiBufListLock );
    end;
  end;
end;

constructor TSendCombiBufferManage.Create(const ACapacity: Integer; const AMaxCombiCount: Byte);
var
  i: Word;
begin
  {组合包标志处理}
  {范围：0~65535}
  if ACapacity > CtMaxCombiPackageID then
    raise Exception.CreateFmt( 'SendCombiBufferManage.Create中, 容量不能超过%d', [ACapacity] );
  FCapacity := ACapacity;
  SetLength( FCombiIdList, FCapacity );
  FHead := 0;
  FTail := 0;
  InitializeCriticalSection( FCombiIdLock );
  for i := 0 to FCapacity - 1 do
    FCombiIdList[i] := i;
  FCount := FCapacity;

  {组合包缓存相关参数默认值}
  FMaxKeepBufferTime := 2500; //缓存数据最长保存2.5秒
  FCheckSpaceTime := 1000;     //每 1 秒检查一次数据
  FMaxCombiCount := AMaxCombiCount;
  InitializeCriticalSection( FCombiBufListLock );

  {发送组合包缓存处理对象}
  FCombiBufInfoSize := CtCombiBufInfoSize + (FMaxCombiCount - 1) * 4;
  FCombiBufMem := TStaticMemoryManager.Create( FCombiBufInfoSize, FCapacity );
  FCombiBufList := TList.Create;
  FCombiBufList.Capacity := FCapacity;
  {自动回收缓存}
  FCheckCombiBuf := TCheckThread.Create( DoCheckCombiBufLock );
  FCheckCombiBuf.SpaceTime := FCheckSpaceTime;
  FCheckCombiBuf.Active := True;
end;

destructor TSendCombiBufferManage.Destroy;
begin
  SetLength( FCombiIdList, 0 );
  FreeAndNil( FCheckCombiBuf );
  FreeAndNil( FCombiBufList );
  FreeAndNil( FCombiBufMem );
  DeleteCriticalSection( FCombiIdLock );
  DeleteCriticalSection( FCombiBufListLock );
  inherited;
end;

procedure TSendCombiBufferManage.DoCheckCombiBufLock(Sender: TObject);
var
  i: Word;
  nCur: Cardinal;
  p: pCombiBufInfo;
begin
  EnterCriticalSection( FCombiBufListLock );
  try
    if FCombiBufList.Count = 0 then Exit;
    nCur := GetTickCount;
    i := 0;
    while i <> FCombiBufList.Count do
    begin
      p := FCombiBufList[i];
      if p^.FLock then
      begin
        Inc( i );
        Continue;
      end;

      if nCur - p^.FActiveTime >= MaxKeepBufferTime then
      begin
        FCombiBufList.Delete( i );
        FreeCombiBuffer( p );
      end
      else
      begin
        Break;
      end;
    end;
  finally
    LeaveCriticalSection( FCombiBufListLock );
  end;
end;

function TSendCombiBufferManage.FindCombiBuffer(const ACombiID: Word; var AOrgLockState: Boolean): pCombiBufInfo;
var
  i: Integer;
  p: pCombiBufInfo;
begin
  Result := nil;
  EnterCriticalSection( FCombiBufListLock );
  try
    i := 0;
    while i <> FCombiBufList.Count do
    begin
      p := FCombiBufList[i];
      if p^.FCombiID = ACombiID then
      begin
        AOrgLockState := p^.FLock;
        p^.FLock := True;
        Result := p;
        Break;
      end;
      Inc( i );
      end;
  finally
    LeaveCriticalSection( FCombiBufListLock );
  end;
end;

procedure TSendCombiBufferManage.FreeCombiBuffer(const p: pCombiBufInfo);
begin
  OnFreeCombiBuffer( FMaxCombiCount, p^.FPackages );
  ReclaimCombiID( p^.FCombiID );
  FCombiBufMem.FreeMem( p );
end;

function TSendCombiBufferManage.GetPackageCount: Integer;
begin
  Result := FCombiBufList.Count;
end;

function TSendCombiBufferManage.GetCombiPackage(const ACombiID: Word; const APackageIndex: Byte; var p: Pointer): Boolean;
var
  pCombi: pCombiBufInfo;
  bOrgLock: Boolean;
begin
  if APackageIndex >= FMaxCombiCount then
  begin
    Result := False;
    Exit;
  end;
  pCombi := FindCombiBuffer( ACombiID, bOrgLock );
  if pCombi = nil then
  begin
    Result := False;
    Exit;
  end;
  p := pCombi^.FPackages[APackageIndex];
  Result := p <> nil;
  pCombi^.FLock := bOrgLock;
end;

function TSendCombiBufferManage.GetCombiBufferCount: Integer;
var
  i, j: Integer;
  p: pCombiBufInfo;
begin
  Result := 0;
  EnterCriticalSection( FCombiBufListLock );
  try
    for i := 0 to FCombiBufList.Count - 1 do
    begin
      p := FCombiBufList[i];
      for j := 0 to FMaxCombiCount - 1 do
      begin
        if p^.FPackages[j] <> nil then
          Inc( Result );
      end;
    end;
  finally
    LeaveCriticalSection( FCombiBufListLock );
  end;
end;

function TSendCombiBufferManage.GetUsedCombiIdCount: Integer;
begin
  Result := FCapacity - FCount;
end;

function TSendCombiBufferManage.GetCombiID(var AID: Word): Boolean;
begin
  Result := False;
  EnterCriticalSection( FCombiIdLock );
  try
    if FCount = 0 then Exit;
    AID := FCombiIdList[ FHead ];
    FHead := ( FHead + 1 ) mod FCapacity;
    Dec( FCount );
    Result := True;
  finally
    LeaveCriticalSection( FCombiIdLock );
  end;
end;

function TSendCombiBufferManage.IsCanReclaimIndex(const AIndex: Word): Boolean;
var
  i, nCount: Word;
begin
  Result := False;
  if FCount > 0 then
  begin
    nCount := UsedCombiIdCount;
    i := FTail;
    while nCount <> 0 do
    begin
      if FCombiIdList[i] = AIndex then
      begin
        if i <> FTail then
          FCombiIdList[i] := FCombiIdList[FTail];
        Result := True;
        Break;
      end;
      i := (i + 1) mod FCapacity;
      Dec( nCount );
    end;
  end;
end;

procedure TSendCombiBufferManage.ReclaimCombiID(const ACombiID: Word);
begin
  EnterCriticalSection( FCombiIdLock );
  try
    if IsCanReclaimIndex(ACombiID) then
    begin
      FCombiIdList[FTail] := ACombiID;
      FTail := (FTail + 1) mod FCapacity;
      Inc(FCount);
    end
    else
      UdpIoHandleDebug( '无法回收组合ID: %d; Tail: %d; Count: %d', [ACombiID, UsedCombiIdCount, FTail] );
  finally
    LeaveCriticalSection( FCombiIdLock );
  end;
end;

procedure TSendCombiBufferManage.RelcaimCombiBuffer(const ACombiID: Word);
var
  i: Word;
  p: pCombiBufInfo;
begin
  EnterCriticalSection( FCombiBufListLock );
  try
    for i := 0 to FCombiBufList.Count - 1 do
    begin
      p := FCombiBufList[i];
      if p^.FCombiID = ACombiID then
      begin
        FCombiBufList.Delete( i );
        FreeCombiBuffer( p );
        Break;
      end;
    end;
  finally
    LeaveCriticalSection( FCombiBufListLock );
  end;
end;

procedure TSendCombiBufferManage.SetCheckSpaceTime(const Value: Cardinal);
begin
  if FCheckSpaceTime <> Value then
  begin
    FCheckSpaceTime := Value;
    FCheckCombiBuf.SpaceTime := FCheckSpaceTime;
  end;
end;

procedure TSendCombiBufferManage.SetMaxKeepBufferTime(const Value: Cardinal);
begin
  if FMaxKeepBufferTime <> Value then
    FMaxKeepBufferTime := Value;
end;


{ TRecvCombiBufferManage }

function TRecvCombiBufferManage.AddRecvCombiPackage(const AIP: Cardinal; const APort, APackageID: Word; const ASyncSige: Byte; const APackageCount,
  APackageIndex: Byte; const ApPackage: Pointer; const ApPackageLen: Word): Integer;
var
  nID: Cardinal;
  pData: Pointer;
  pInfo: pCombiInfo;
begin
  if not FOwner.FindAddressID(AIP, APort, nID) then
  begin
    FOwner.DoErrorInfo( '无法找到对应的ID' );
    Result := -1;
    Exit;
  end;
  Result := FCombiPackage.Add( nID, APackageID, APackageCount, APackageIndex, ApPackage, ApPackageLen, pData );
  if Result = 5 then
  begin
    OutputDebugString( '添加组合包 成功' );
    pInfo := pCombiInfo(pData^);
    if pInfo = nil then
    begin
      if not FCombiInfoMem.GetMem( Pointer(pInfo) ) then
      begin
        FOwner.DoErrorInfo( 'CombiInfo Mem 不足' );
        Exit;
      end;
      pInfo^.FIP := AIP;
      pInfo^.FPort := APort;
      pInfo^.FSyncSige := ASyncSige;
      pInfo^.FCombiID := APackageID;
      Pointer(pData^) := Pointer(pInfo);
    end;
  end
  else if Result <> 0 then
  begin
    //0: 正确
    //1: 计算出的Index不正确( UserID 有误 )
    //2: 内存不足( 申请不到HashNode )
    //3: 包有误 包的总量要相等
    //4: ANodeIndex 索引有误
    //5: ApDataAdress 对就的 pData 为空； 属于正确范围
    FOwner.DoErrorInfo( PChar('AddRecvCombiPackage失败: ' + IntToStr(Result)) );
  end;
end;

constructor TRecvCombiBufferManage.Create(AOwner: TUdpIoHandle);
begin
  FOwner := AOwner;
  MaxTractCount := FOwner.CombiRecvFailTractCount;
  FCombiInfoMem := TStaticMemoryManager.Create( CtCombiInfoSize, FOwner.CombiRecvMaxHashNode );
  FCombiPackage := TCombiNotify.Create;
  with FCombiPackage do
  begin
    HashTableCount := FOwner.CombiRecvHashTableCount;
    MaxHashNodeCount := FOwner.CombiRecvMaxHashNode;
    PeenDataMaxCount := FOwner.CombiMaxPackageCount;
    NotifyThreadCount := 1;
    NotifyThreadSpaceTime := 300;
    CheckThread := True;
    MaxWaitTime := FOwner.CombiRecvMaxWaitTime;
    CheckSpaceTime := FOwner.CombiRecvCheckThreadSpaceTime;
    OnNotifyHandleOK := DoCombiPackageRecvOK;
    OnNotifyHandleFail := DoCombiPackageRecvFail;
    Active := True;
  end;
end;

destructor TRecvCombiBufferManage.Destroy;
begin
  FreeAndNil( FCombiPackage );
  FreeAndNil( FCombiInfoMem );
  inherited;
end;

function TRecvCombiBufferManage.DoCombiPackageRecvFail(Sender: TObject; const ATractCount: Byte; const ApData: Pointer;
  const ADataArray: array of Pointer; const AAryCount: Byte): Boolean;
var
  AryLoseIndex: array[0..CtMaxCombiPackageCount - 1] of Byte;
  i, nLoseCount: Byte;
  pCmd: pLosePackageIndex;
  p: pCombiInfo;
begin
  Result := ATractCount = MaxTractCount; //True: 删除
  p := ApData;
  if p = nil then
  begin
    Result := True;
    FOwner.DoErrorInfo( '组合包有误，DATA不能为空' );
    Exit;
  end;
  if not Result then
  begin
    //发送重发信息
    nLoseCount := 0;
    for i := 0 to AAryCount - 1 do
    begin
      if ADataArray[i] = nil then
      begin
        AryLoseIndex[ nLoseCount ] := i;
        Inc( nLoseCount );
      end;
    end;

    if nLoseCount > 0 then
    begin
      GetMem( pCmd, CtLosePackageSize + nLoseCount - 1 );
      try
        pCmd^.FCmd := CtLosePackageIndexCmd;
        pCmd^.FCombiID := p^.FCombiID;
        pCmd^.FLoseCount := nLoseCount;
        Move( AryLoseIndex[0], pCmd^.FLoseIndexs[0], nLoseCount );
        FOwner._SendBuffer( p^.FIP, p^.FPort, PAnsiChar(pCmd), CtLosePackageSize + nLoseCount - 1, p^.FSyncSige );
      finally
        FreeMem( pCmd );
      end;
    end;
  end
  else
  begin
    //完全失败,回收内存
    FOwner.DoFreeCombiPackage( AAryCount, ADataArray );
    FCombiInfoMem.FreeMem( p );
    FOwner.DoErrorInfo( '组合包数据不全，超时直接删除' );
  end;
end;

function TRecvCombiBufferManage.DoCombiPackageRecvOK(Sender: TObject; const ApData: Pointer; const ADataArray: array of Pointer;
  const ALen: Integer; const AAryCount: Byte): Boolean;
var
  p: pCombiInfo;
  pMem: pAnsiChar;
  i, nPos, nTatolLen: Integer;
  nLen: Word;
begin
  Result := True; //返回值无用
  p := ApData;
  if p = nil then
  begin
    FOwner.DoErrorInfo( '组合包有误，DATA不能为空' );
    Exit;
  end;
  GetMem( pMem, ALen );
  FillChar( pMem^, ALen, 1 );
  nTatolLen := ALen;
  nPos := 0;
  try
    for i := 0 to AAryCount - 1 do
    begin
      if ADataArray[i] <> nil then
      begin
        Move( ADataArray[i]^, nLen, 2);
        Dec( nTatolLen, nLen );
        if nTatolLen >= 0 then
        begin
          Move( PAnsiChar(Integer(ADataArray[i]) + 2)^, PAnsiChar(Integer(pMem) + nPos)^, nLen );
          Inc( nPos, nLen );
        end
        else
        begin
          FOwner.DoErrorInfo( '组合包合并出错' );
          Exit;
        end;
      end;
    end;
    if nTatolLen = 0 then
      FOwner.OnRecvBuffer( p^.FIP, p^.FPort, pMem, ALen, p^.FSyncSige )
    else
      FOwner.DoErrorInfo( '合并组合包时长度有误' );
  finally
    FOwner.DoFreeCombiPackage( AAryCount, ADataArray );
    FreeMem( pMem, ALen );
  end;
  FCombiInfoMem.FreeMem( p );
end;

function TRecvCombiBufferManage.GetCurCount: Integer;
begin
  Result := FCombiPackage.Count;
end;

procedure TRecvCombiBufferManage.SetMaxtractCount(const Value: Cardinal);
begin
  if FMaxTractCount <> Value then
    FMaxTractCount := Value;
end;

procedure TRecvCombiBufferManage.SetWaitTime(const Value: Cardinal);
begin
  if FWaitTime <> Value then
    FWaitTime := Value;
end;

end.
