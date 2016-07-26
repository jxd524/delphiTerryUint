{
单元名称: uJxdUdpIOHandle
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明:
开始时间: 2009-4-8
修改时间: 2011-3-21 (最后修改时间)
类说明  :
          1: 包含类
             TWordIdManage, TCombiBufferBasic, TSendCombiBufferManage, TRecvCombiBufferManage, TxdUdpIoHandle
          2: TWordIdManage: 组合包ID管理器，可对ID进行回收，并对要回收的ID进行判断
          3: TCombiBufferBasic: 组合缓存管理器，对需要缓存的数据进行管理，使用事件和线程的方式定期清理
               TSendCombiBufferManage：由TCombiBufferBasic派生，处理发送组合包缓存，提供重发所需要数据
               TRecvCombiBufferManage: 由TCombiBufferBasic派生，处理接收组合包缓存，提供合并数据功能
          4: TxdUdpIoHandle: UDP包处理服务中心，对数据进行协议封装。提供大数据的发送与接收重组；
          5：UDP重组通信协议：
               单包   版本(1) - 此包总长度(2) - CRC32码(4) - 包信息(1)[前四位：0； 后四位: 协议命令，当为0时，表示需要由子类处理]
               组合包 版本(1) - 此包总长度(2) - CRC32码(4) - 包信息(1)[前四位：个数； 后四位: 包顺序] - 标志(2)
          6：组合包解决方案
                0000  0000: 前四位表示: 包的个数; 0: 独立包; 1 ~ 15: 表示组合包, 接下来的两个字节标志组合包的标志
                        后四位: 当为组合包时,表示包的序号 0 ~ 15
          7: 组合包丢包解决方案
            发送端对发送的组合包进行缓存 由 TSendCombiBufferManage 类管理
            接收端以Hash表进行重组,发现丢包则要求重发 FD/TryCount/LastTime/BagCount/CurRecvCount/package_1..package_n
            请求重发命令是一个独立包，命令包含在协议中的：包信息中；
          8: TSendCombiBufferManage
            提供发送组合包时,使用组合包唯一标志(WORD) 和 缓存数据处理
            缓存数据作用: 给接收端提供丢包机制

虚函数说明:
          1: DoCalcCombiIDWhenRecvCombBuffer
            当接收到组合包时，由提供的参数，得到一个有效的ID，以便TRecvCombiBufferManage进行处理
            TxdUdpIoHandle的处理方式是简单的让各参数相加后返回此值
          2: OnRecvBuffer
            当接收到一个完整的数据包时，此函数被调用

限制:
          1: 同步包只能同时(N个线程同时发送) 发送 128 个同步包
          2: 组合包最大组合只能 16 个包, 长度: CtCombiPackageSize * 16
          3: 独立包没有进行丢包处理
          4: 组合包丢包处理跟发送端的设置有关, CombiSendMaxKeepBufferTime 决定发送组合包缓存时间
                                               CombiRecvFailTractCount    决定丢包时,重新要求发丢包次数

协议头:
      //单包   版本(1) - 此包总长度(2) - CRC32码(4) - 包信息(1)[前四位：0； 后四位: 协议命令，当为0时，表示需要由子类处理]
      //组合包 版本(1) - 此包总长度(2) - CRC32码(4) - 包信息(1)[前四位：个数； 后四位: 包顺序] - 标志(2)
          说明: 同一组合包 标志 相同, 此标志为发送方的组合ID，接收到组合包时，
                应该根据IP，port及CombiID 三者来确定对应的组合包位置


                

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


    一些典型的MTU值：

    网络：                              MTU字节
    超通道                              65535
    16Mb/s信息令牌环（IBM）             17914
    4Mb/s令牌环（IEEE802.5）            4464
    FDDI                                4352
    以太网                              1500
    IEEE802.3/802.2                     1492
    X.25                                576         //我国大部分公众网都是这种类型
    点对点（低时延）                    296

经测试,局域网环境下,UDP包大小为1024*8, 丢包情况理想. CtMTU = 1024 * 8 + 20 + 8
外网环境下,UDP包大小为548,速度理想,丢包情况理想  CtMTU = 548 + 20 + 8

}

unit uJxdUdpIOHandle;

interface


uses
  uJxdUdpBasic, WinSock2, Windows, Forms, Classes, SysUtils, uJxdDataStruct, uJxdThread, uJxdMemoryManage,
  Encrypt;

{$I JxdUdpOpt.inc}

const
{协议通信信息}
  CtCurUDPVerNumber = 1;   //一个字节
  
{设计协议本身限制设置值}
{$IFNDEF CombiPackageCount}
  CtMaxCombiPackageCount = 8;  //默认值
{$ENDIF}
                               { 组合包不能超过 16个, 数值为0..15; A: 0000 B: 0000;
                                前四位:
                                        A区: 包个数; 0: 单独包; 1 ~ 15: 2 ~ 16个包
                                后四位:
                                        B区: 包的序号; 对单独包无用,组合包序号: (0000 ~ 1111) 0 ~ 15 }

{广域网UDP限制}
  {
    在普通的局域网环境下，我建议将UDP的数据控制在1472字节以下为好
    Internet上的标准MTU值为576字节
  }
{$IFNDEF MTU}
  CtMTU = 576;      //中国大部分路由MTU值; 避免IP分片
{$ENDIF}

  CtRawIpHead = 20;  //TCP/IP协议中原始IP头
  CtRawUdpHead = 8;  //TCP/IP协议中原始UDP头
{本身使用常规常量}
  CtMaxUdpSize = CtMTU - CtRawIpHead - CtRawUdpHead; //最大发送包长度
  CtSinglePackageHeadSize = 8;                       //单独包包头长度  版本(1) - 此包总长度(2) - CRC32码(4) - 包个数(1)[值: 0]
  CtSinglePackageSize = CtMaxUdpSize - CtSinglePackageHeadSize; //独立包最大数据区长度
  CtCombiPackageHeadSize = 10;                       //组合包包头长度  版本(1) - 此包总长度(2) - CRC32码(4) - 包个数(1)[值: 1 ~ 15] - 标志(2)
  CtCombiPackageSize = CtMaxUdpSize - CtCombiPackageHeadSize;   //组合包最大数据区长度

type
  { ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //                                          TWordIdManage
    //组合包序号管理
    //
  } ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  {$M+}
  TWordIdManage = class
  public
    constructor Create;
    destructor  Destroy; override;

    function  GetWordID(var AID: Word): Boolean;
    procedure ReclaimWordID(const AID: Word);
  private
    FIDTable: array[0..MAXWORD - 1] of Word;
    FHead: Word;
    FTail: Word;
    FCount: Integer;
    FLock: TRTLCriticalSection;
    function  IsCanReclaimIndex(const AIndex: Word): Boolean;
    function  GetCaption: Integer;
  published
    property Count: Integer read FCount;
    property Caption: Integer read GetCaption;
  end;

  { ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //                                           TCombiBufferBasic
    //组合包的缓存信息，发送或接收到的组合包都由此数据结构进行缓存
    //
  } ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TOnCalcCombiID = function(const AIP: Cardinal; const APort, ACombiID: Word): Cardinal of object;
  PCombiBufferInfo = ^TCombiBufferInfo;
  TCombiBufferInfo = record
    FCombiID: Word;             //组合包ID
    FCombiPackageCount: Byte;   //组合包数量
    FCurCombiPackCount: Byte;   //当前已经存在缓存数量
    FCombiIP: Cardinal;         //组合包对应IP地址： - 对于发送方来说，此IP跟PORT 并没有用处
    FCombiPort: Word;           //组合包对应Port     - 对于接收方来说，此IP跟PORT指定的是发送方的信息
    FTimeOutCount: Byte;        //超时次数: 发送方不需要用到
    FLastActiveTime: Cardinal;  //组合包最后活动时间
    FPackageLens: array[0..CtMaxCombiPackageCount - 1] of Word; //组合包对应的内存大小
    FPackages: array[0..CtMaxCombiPackageCount - 1] of Pointer; //组合包内存指针
  end;
  TOnCombiBuffer = procedure(const Ap: PCombiBufferInfo) of object;
  TCombiBufferBasic = class
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure CancelCombiBuffer(var Ap: PCombiBufferInfo);
  protected
    FHashListLock: TRTLCriticalSection;
    FHashBasic: THashArrayBasic;                   //表中数据为: ID, PCombiBufferInfo
    FCombiHashBufferManage: TxdFixedMemoryManager; //申请的数据为： TCombiBufferInfo

    function  ActiveBuffer: Boolean; virtual;
    procedure UnActiveBuffer; virtual;
    procedure DoHandleTimeoutCombiBuffer(ApCombiBuffer: PCombiBufferInfo; var ADel: Boolean); virtual; //当缓存超时，函数被调用
    procedure DoDeleteFormHashList(const ApCombiBuffer: PCombiBufferInfo); virtual;  //子类需要处理，删除

    procedure ActiveThread(const ADelayTime: Cardinal); //激活线程工作
    procedure DoReclaimCombiBuffer(ApCombiBuffer: PCombiBufferInfo; const ADelFromHashList: Boolean); //回收指定的内存
    
    {被线程执行的函数}
    procedure DoCheckCombiBufferTimer; //检查缓存存在时间，判断是否要删除超时的缓存
    procedure DoLoopHashTableToCheckBuffer(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel: Boolean; var AFindNext: Boolean);
  private
    FWaitEvent: Cardinal;
    FRuningThread: Boolean;
    FCurWaitTime: Cardinal;
    FCheckBeginTime: Cardinal;//开始检查数据时时间

    FActive: Boolean;
    FHashTableCount: Integer;
    FMaxBufferCount: Integer;
    FMaxKeepBufferTime: Cardinal;
    FOnDeleteCombiBuffer: TOnCombiBuffer;
    procedure SetActive(const Value: Boolean);
    procedure SetFHashTableCount(const Value: Integer);
    procedure SetMaxBufferCount(const Value: Integer);
    function  GetCount: Integer;
    procedure SetMaxKeepBufferTime(const Value: Cardinal);
  published
    property Active: Boolean read FActive write SetActive;
    property HashTableCount: Integer read FHashTableCount write SetFHashTableCount;  //Hash桶数量
    property MaxBufferCount: Integer read FMaxBufferCount write SetMaxBufferCount;   //最大缓存数量
    property MaxKeepBufferTime: Cardinal read FMaxKeepBufferTime write SetMaxKeepBufferTime; //缓存中的数据最长可保存时间

    property Count: Integer read GetCount; //正在使用的缓存节点数量
    property OnDeleteCombiBuffer: TOnCombiBuffer read FOnDeleteCombiBuffer write FOnDeleteCombiBuffer; //删除缓存结构
  end;

  { ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    发送端缓存发送数据管理类                           TSendCombiBufferManage
    仅针对组合包有用
    提供组合包的缓存,以便接收包要求重发丢失的包
  } ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  TSendCombiBufferManage = class(TCombiBufferBasic)
  public
    function  GetCombiBuffer(const ACombiID: Word; const ACreateNewWhenNoExists: Boolean): PCombiBufferInfo;
    procedure FinishedCombiBuffer(const Ap: PCombiBufferInfo);
  protected
    function  ActiveBuffer: Boolean; override;
    procedure DoHandleTimeoutCombiBuffer(ApCombiBuffer: PCombiBufferInfo; var ADel: Boolean); override;
    procedure DoDeleteFormHashList(const ApCombiBuffer: PCombiBufferInfo); override;
  private
    FHashListEx: THashArray;
  end;
  { ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    接收端缓存组合包管理类                           TRecvCombiBufferManage
    仅针对组合包有用
    提供组合包的缓存,以便接收到完整的组合包，在允许的情况下，进行丢包处理
  } ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TOnRecvFinishedCombiBuffer = procedure(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal) of object;
  TRecvCombiBufferManage = class(TCombiBufferBasic)
  public
    function  GetCombiBuffer(const AIP: Cardinal; const APort, ACombiID: Word; const ACreateNewWhenNoExists: Boolean): PCombiBufferInfo;
    procedure FinishedCombiBuffer(const Ap: PCombiBufferInfo);
  protected
    function  ActiveBuffer: Boolean; override;
    function  DoCalcCombiID(const AIP: Cardinal; const APort, ACombiID: Word): Cardinal; //计算组合ID
    procedure DoHandleTimeoutCombiBuffer(ApCombiBuffer: PCombiBufferInfo; var ADel: Boolean); override;
    procedure DoDeleteFormHashList(const ApCombiBuffer: PCombiBufferInfo); override;
  private
    FHashListEx: THashArrayEx;

    FOnCalcCombiID: TOnCalcCombiID;
    FOnRecvFinishedCombiBuffer: TOnRecvFinishedCombiBuffer;
    FMaxTimeoutCount: Integer;
    FOnReGetComibBuffer: TOnCombiBuffer;
    FOnRecvTimeoutToDelete: TOnCombiBuffer;
    procedure SetMaxTimeoutCount(const Value: Integer);
  published
    //最大超时次数
    property MaxTimeoutCount: Integer read FMaxTimeoutCount write SetMaxTimeoutCount;

    property OnCalcCombiID: TOnCalcCombiID read FOnCalcCombiID write FOnCalcCombiID; //计算组合包ID，可允许重复
    property OnRecvTimeoutToDelete: TOnCombiBuffer read FOnRecvTimeoutToDelete write FOnRecvTimeoutToDelete; 
    property OnReGetComibBuffer: TOnCombiBuffer read FOnReGetComibBuffer write FOnReGetComibBuffer; //因超时，要求外部重新发送组合包
    property OnRecvFinishedCombiBuffer: TOnRecvFinishedCombiBuffer read FOnRecvFinishedCombiBuffer write FOnRecvFinishedCombiBuffer; //当接收到全部组合包时，通知外部
  end;

  { ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    UDP发送接收处理类                           TxdUdpIoHandle
      通信格式，CRC32
      单包发送
      组合包发送,提供丢包处理
  } ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TxdUdpIoHandle = class(TxdUdpBasic)
  public
    constructor Create; override;
    destructor  Destroy; override;

    function SendBuffer(const AIP: Cardinal; const APort: Word; const ApBuffer: PAnsiChar; const ABufLen: Integer): Integer;
  protected
    {父类虚函数}
    procedure DoRecvBuffer; override;   //由线程调用

    function  DoBeforOpenUDP: Boolean; override;  //初始化UDP前; True: 允许初始化; False: 不允许初始化
    procedure DoAfterOpenUDP; override;
    procedure DoBeforCloseUDP; override;
    procedure DoAfterCloseUDP; override; //UDP关闭之后
    procedure DoErrorInfo(const AInfo: PAnsiChar); override;

    {发送数据包}
    function  _SendSinglePackage(const AIP: Cardinal; const APort: Word; const ApBuffer: pAnsiChar; const ABufferSize: Integer): Integer;
    function  _SendCombiPackage(const AIP: Cardinal; const APort: Word; const ApBuffer: pAnsiChar; const ABufferSize: Integer): Integer;
    function  _DoSendPackage(const AIP: Cardinal; const APort: Word; const ApPackageAddr: pAnsiChar; const APackageLen: Word): Integer;

    {让子类处理的虚函数}
       //当接收到组合包时，由提供的参数，得到一个有效的ID，以便TRecvCombiBufferManage进行处理
    function  DoCalcCombiIDWhenRecvCombBuffer(const AIP: Cardinal; const APort, ACombiID: Word): Cardinal; virtual;
      //接收到完整的数据时
    procedure OnRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal); virtual;
  private
  {对象定义}
    FSendCombiBufferManage: TSendCombiBufferManage;  //发送端组合包数据管理对象
    FRecvCombiBufferManage: TRecvCombiBufferManage;  //接收端组合包数据管理对象
    FCombiPackageBufferManage: TxdFixedMemoryManager;       //组合包内存管理对象
    FCombiIDManage: TWordIdManage;                  //组合包序号

    procedure InitStatValue;

  {事件处理函数}
    {发送缓存事件处理}
    procedure DoReclaimSendcombiBuffer(const Ap: PCombiBufferInfo); //回收发送缓存

    {接收缓存时需要处理的一些事件}
    procedure DoReGetCombiBuffer(const Ap: PCombiBufferInfo); //重新请求组合包
    procedure DoReclaimRecvCombiBuffer(const Ap: PCombiBufferInfo); //回收接收缓存，超时或完成时调用
    procedure DoRecvCombiBufferTimeount(const Ap: PCombiBufferInfo); //当接收缓存完全失败时调用
    procedure DoRecvCombiBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal); //接收到完整的组合包

    //接收到组合包时
    procedure OnRecvCombiPackage(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Word;
                                 const APackageCount, APackageIndex: Byte; const ACombiID: Word);

    //发送协议自身定义的通信包
    function  _SendProtocolCmdSinglePackage(const AIP: Cardinal; const APort: Word; const ACmd: Byte;
      const ApBuffer: pAnsiChar; const ABufferSize: Integer): Integer;

    {命令处理}
    //当接收到要求重发丢失的包
    procedure DoRecvReGetCombiPackage(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Word);
  private
    FSendCombiHashTableCount: Integer;
    FSendCombiMaxNodeCount: Integer;
    FSendCombiMaxKeepTime: Cardinal;
    FRecvCombiMaxKeepTime: Cardinal;
    FRecvCombiMaxNodeCount: Integer;
    FRecvCombiHashTableCount: Integer;
    FCombiMaxBufferCount: Integer;
    FRecvErrorPackageCount: Integer;
    FRecvCombiMaxTimeoutCount: Integer;
    FSendSuccessPackageCount: Integer;
    FSendFailPackageCount: Integer;
    FRecvPackageCount: Integer;
    FReGetLostCmdCount: Integer;
    FRespondReGetLostCmdFailCount: Integer;
    FRespondReGetLostCmdSuccessCount: Integer;
    FRecvFullPackageCount: Integer;
    FSendFullPackageCount: Integer;
    FDropRecvCombiPackageCount: Integer;
    procedure SetSendCombiHashTableCount(const Value: Integer);
    procedure SetSendCombiMaxNodeCount(const Value: Integer);
    procedure SetSendCombiMaxKeepTime(const Value: Cardinal);
    function  GetSendCombiCount: Integer;
    function  GetRecvCombiCount: Integer;
    procedure SetRecvCombiHashTableCount(const Value: Integer);
    procedure SetRecvCombiMaxNodeCount(const Value: Integer);
    procedure SetRecvCombiMaxKeepTime(const Value: Cardinal);
    procedure SetCombiMaxBufferCount(const Value: Integer);
    procedure SetRecvCombiMaxTimeoutCount(const Value: Integer);
    function  GetCombiMaxPackageCount: Integer;
    function  GetCombiIDCount: Integer;
    function  GetCombiMaxIDCount: Integer;
    function  GetCombiBufferCount: Integer;
  published
  {组合包缓存内存属性}
    property CombiMaxBufferCount: Integer read FCombiMaxBufferCount write SetCombiMaxBufferCount; //发送与接收组合包缓存数量
    property CombiMaxPackageCount: Integer read GetCombiMaxPackageCount; //最大组合包数据，不能超出16
    property CombiBufferCount: Integer read GetCombiBufferCount; //缓存可以使用数量

  {发送组合包相关属性}
    property SendCombiHashTableCount: Integer read FSendCombiHashTableCount write SetSendCombiHashTableCount; //Hash桶数量
    property SendCombiMaxNodeCount: Integer read FSendCombiMaxNodeCount write SetSendCombiMaxNodeCount; //最大缓存节点数量
    property SendCombiMaxKeepTime: Cardinal read FSendCombiMaxKeepTime write SetSendCombiMaxKeepTime; //缓存节点最长保存时间
    property SendCombiCount: Integer read GetSendCombiCount; //缓存使用数量

  {接收组合包相关属性}
    property RecvCombiHashTableCount: Integer read FRecvCombiHashTableCount write SetRecvCombiHashTableCount; //Hash桶数量
    property RecvCombiMaxNodeCount: Integer read FRecvCombiMaxNodeCount write SetRecvCombiMaxNodeCount; //最大缓存节点数量
    property RecvCombiMaxKeepTime: Cardinal read FRecvCombiMaxKeepTime write SetRecvCombiMaxKeepTime; //缓存节点最长保存时间
    property RecvCombiCount: Integer read GetRecvCombiCount; //缓存使用数量
    property RecvCombiMaxTimeoutCount: Integer read FRecvCombiMaxTimeoutCount write SetRecvCombiMaxTimeoutCount; //接收缓存最多超时次数

  {统计信息}
    property SendSuccessPackageCount: Integer read FSendSuccessPackageCount;         //发送成功包总数量
    property SendFailPackageCount: Integer read FSendFailPackageCount;               //发送失败包总数量
    property RecvPackageCount: Integer read FRecvPackageCount;                       //接收数据包总数量
    property DropRecvCombiPackageCount: Integer read FDropRecvCombiPackageCount;     //丢弃不完整的组合包数量
    property RecvErrorPackageCount: Integer read FRecvErrorPackageCount;             //接收到错误包的总数量 错误是指协议层的错误，应用层的错误不在此计数中

    property SendFullPackageCount: Integer read FSendFullPackageCount; //发送完整包数量
    property RecvFullPackageCount: Integer read FRecvFullPackageCount; //接收到完整包的数量

    property ReGetLostCmdCount: Integer read FReGetLostCmdCount; //请求重新发送丢失包数量
    property RespondReGetLostCmdSuccessCount: Integer read FRespondReGetLostCmdSuccessCount; //成功响应丢包次数
    property RespondReGetLostCmdFailCount: Integer read FRespondReGetLostCmdFailCount; //请求重发的数据已经被删除, 无法响应重发机制

    property CombiIDCount: Integer read GetCombiIDCount; //序号使用数量
    property CombiMaxIDCount: Integer read GetCombiMaxIDCount; //最多允许使用组合ID数量
  end;
  {$M-}
  
implementation



type
  {重新请求需要的组合包，由协议自己处理}
  PReGetCombiInfo = ^TReGetCombiInfo;
  TReGetCombiInfo = packed record
    FCmd: Word;
    FCombiID: Word;
    FCount: Byte;
    FIndexs: array[0..CtMaxCombiPackageCount - 1] of Byte; //0 ~ FCount -1 有效
  end;
const
  CtReGetCombiInfoCmd = 15;  //将写在独立包的包个数中 [前四位：0； 后四位: 包命令]，命令只由 4 位 二进制表示，不能超过 15
  CtReGetCombiInfoSize = SizeOf(TReGetCombiInfo);
  CtCombiBufferInfoSize = SizeOf(TCombiBufferInfo);

{格式化包信息}
function FormatPackageInfo(const APackageCount, AIndexOrCmd: Byte): Byte;
begin
//单包   版本(1) - 此包总长度(2) - CRC32码(4) - 包信息(1)[前四位：0； 后四位: 协议命令，当为0时，表示需要由子类处理]
//组合包 版本(1) - 此包总长度(2) - CRC32码(4) - 包信息(1)[前四位：个数； 后四位: 包顺序] - 标志(2)
  Result := 0;
  Result := Result or AIndexOrCmd;
  Result := Result or ( APackageCount shl 4);
end;

{还原包信息}
procedure ParsePackageInfo(const ASign: Byte; var APackageCount: Byte; var APackageIndex: Byte);
begin
  APackageCount := ASign shr 4;
  APackageIndex := ASign shl 4;
  APackageIndex := APackageIndex shr 4;
end;

{ TCombiBufferBasic }

function TCombiBufferBasic.ActiveBuffer: Boolean;
begin
  Result := True;
  try
    InitializeCriticalSection( FHashListLock );
    FWaitEvent := CreateEvent( nil, True, False, nil ); //使用手工重置的方式创建事件
    FRuningThread := False;
//    FHashList := THashArrayEx.Create; //由子类去创建 具体的HASH表类型
    with FHashBasic do
    begin
      HashTableCount := HashTableCount;
      MaxHashNodeCount := MaxBufferCount;
      Active := True;
    end;
    FCombiHashBufferManage := TxdFixedMemoryManager.Create( CtCombiBufferInfoSize, MaxBufferCount );
    RunningByThread( DoCheckCombiBufferTimer );
  except
    Result := False;
    if Assigned(FHashBasic) then
      FreeAndNil( FHashBasic );
    if Assigned(FCombiHashBufferManage) then
      FreeAndNil( FCombiHashBufferManage );
  end;
end;

procedure TCombiBufferBasic.ActiveThread(const ADelayTime: Cardinal);
begin
  FCurWaitTime := ADelayTime;
  SetEvent( FWaitEvent );
end;

procedure TCombiBufferBasic.CancelCombiBuffer(var Ap: PCombiBufferInfo);
begin
  EnterCriticalSection( FHashListLock );
  try
    DoReclaimCombiBuffer( Ap, True );
  finally
    LeaveCriticalSection( FHashListLock );
  end;
  Ap := nil;
end;

constructor TCombiBufferBasic.Create;
begin
  FActive := False;
  FHashTableCount := 131;
  FMaxBufferCount := 1024;
  FMaxKeepBufferTime := 3000;
  FCurWaitTime := FMaxKeepBufferTime div 2;
  FHashBasic := nil;
  FCombiHashBufferManage := nil;
end;

destructor TCombiBufferBasic.Destroy;
begin
  Active := False;
  inherited;
end;

procedure TCombiBufferBasic.DoCheckCombiBufferTimer;
var
  nWaitTime: Cardinal;
begin
  FRuningThread := True;
  try
    nWaitTime := INFINITE;
    WaitForSingleObject( FWaitEvent, nWaitTime );
    if not FActive then Exit;
    while True do
    begin
      ResetEvent( FWaitEvent );
      if FHashBasic.Count = 0 then
      begin
        nWaitTime := INFINITE;
        OutputDebugString( '无缓存需要检查，线程将一直等待' );
      end
      else
        nWaitTime := FCurWaitTime;
      WaitForSingleObject( FWaitEvent, nWaitTime );
      if not FActive then Break;
      if FHashBasic.Count = 0 then Continue;
      EnterCriticalSection( FHashListLock );
      try
        OutputDebugString( '线程检查 DoLoopHashTableToCheckBuffer' );
        FCheckBeginTime := GetTickCount;
        FHashBasic.Loop( DoLoopHashTableToCheckBuffer );
      finally
        LeaveCriticalSection( FHashListLock );
      end;
      Sleep( 100 ); //防止大量并发事件发生
    end;
  finally
    FRuningThread := False;
  end;
end;

procedure TCombiBufferBasic.DoDeleteFormHashList(const ApCombiBuffer: PCombiBufferInfo);
begin

end;

procedure TCombiBufferBasic.DoHandleTimeoutCombiBuffer(ApCombiBuffer: PCombiBufferInfo; var ADel: Boolean);
begin

end;

procedure TCombiBufferBasic.DoLoopHashTableToCheckBuffer(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel, AFindNext: Boolean);
var
  p: PCombiBufferInfo;
begin
  p := pData;
  if p^.FLastActiveTime = 0 then
  begin
    OutputDebugString( '不需要检查的项' );
  end
  else
  begin
    if FCheckBeginTime - p^.FLastActiveTime > MaxKeepBufferTime then
    begin
      DoHandleTimeoutCombiBuffer( p, ADel );
      if ADel then
      begin
        OnDeleteCombiBuffer( p );
        DoReclaimCombiBuffer( p, False ); //由类内部去删除
      end;
    end;
  end;
end;

procedure TCombiBufferBasic.DoReclaimCombiBuffer(ApCombiBuffer: PCombiBufferInfo; const ADelFromHashList: Boolean);
begin
  if ADelFromHashList then
  begin
    OnDeleteCombiBuffer( ApCombiBuffer );
    DoDeleteFormHashList( ApCombiBuffer );
  end;
  FillChar( ApCombiBuffer^, CtCombiBufferInfoSize, 0 );
  FCombiHashBufferManage.FreeMem( ApCombiBuffer );
end;

function TCombiBufferBasic.GetCount: Integer;
begin
  if Active then
    Result := FCombiHashBufferManage.Capacity - FCombiHashBufferManage.Count
  else
    Result := 0;
end;

procedure TCombiBufferBasic.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      FActive := ActiveBuffer
    else
    begin
      FActive := Value;
      UnActiveBuffer;
    end;
  end;
end;

procedure TCombiBufferBasic.SetFHashTableCount(const Value: Integer);
begin
  if not Active and (FHashTableCount <> Value) then
    FHashTableCount := Value;
end;

procedure TCombiBufferBasic.SetMaxBufferCount(const Value: Integer);
begin
  if not Active and (FMaxBufferCount <> Value) then
    FMaxBufferCount := Value;
end;

procedure TCombiBufferBasic.SetMaxKeepBufferTime(const Value: Cardinal);
begin
  if FMaxKeepBufferTime <> Value then
  begin
    FMaxKeepBufferTime := Value;
    FCurWaitTime := FMaxKeepBufferTime div 2;
  end;
end;

procedure TCombiBufferBasic.UnActiveBuffer;
begin
  while FRuningThread do
  begin
    ActiveThread( 0 );
    Sleep( 10 );
    Application.ProcessMessages;
  end;
  DeleteCriticalSection( FHashListLock );
  FreeAndNil( FHashBasic );
  FreeAndNil( FCombiHashBufferManage );
end;

{ TCombiIDManage }

constructor TWordIdManage.Create;
var
  i: Integer;
begin
  InitializeCriticalSection( FLock );
  for i := 0 to MAXWORD - 1 do
    FIDTable[i] := i;
  FTail := 0;
  FHead := 0;
  FCount := 0;
end;

destructor TWordIdManage.Destroy;
begin
  DeleteCriticalSection( FLock );
  inherited;
end;

function TWordIdManage.GetCaption: Integer;
begin
  Result := MAXWORD;
end;

function TWordIdManage.GetWordID(var AID: Word): Boolean;
begin
  Result := False;
  EnterCriticalSection( FLock );
  try
    if FCount = MAXWORD then Exit;
    AID := FIDTable[ FHead ];
    FHead := ( FHead + 1 ) mod MAXWORD;
    Inc( FCount );
    Result := True;
  finally
    LeaveCriticalSection( FLock );
  end;
end;

function TWordIdManage.IsCanReclaimIndex(const AIndex: Word): Boolean;
var
  i, nCount: Word;
begin
  Result := False;
  if FCount > 0 then
  begin
    nCount := FCount;
    i := FTail;
    while nCount <> 0 do
    begin
      if FIDTable[i] = AIndex then
      begin
        if i <> FTail then
          FIDTable[i] := FIDTable[FTail];
        Result := True;
        Break;
      end;
      i := (i + 1) mod MAXWORD;
      Dec( nCount );
    end;
  end;
end;

procedure TWordIdManage.ReclaimWordID(const AID: Word);
begin
  EnterCriticalSection( FLock );
  try
    if IsCanReclaimIndex(AID) then
    begin
      FIDTable[FTail] := AID;
      FTail := (FTail + 1) mod MAXWORD;
      Dec(FCount);
    end
    else
      JxdDbg( '无法回收组合ID: %d; Tail: %d; Count: %d', [AID, FTail, FCount] );
  finally
    LeaveCriticalSection( FLock );
  end;
end;

{ TxdUdpIoHandle }

constructor TxdUdpIoHandle.Create;
begin
  inherited;
  {发送组合包相关设置}
  FSendCombiHashTableCount := 1313;
  FSendCombiMaxNodeCount := 512;
  FSendCombiMaxKeepTime := 5000;
  FRecvCombiHashTableCount := 1313;
  FRecvCombiMaxNodeCount := 512;
  FRecvCombiMaxKeepTime := 2000;
  FCombiMaxBufferCount := 1024;
  FRecvCombiMaxTimeoutCount := 2;
  {统计信息}
  InitStatValue;
end;

destructor TxdUdpIoHandle.Destroy;
begin

  inherited;
end;

procedure TxdUdpIoHandle.DoAfterCloseUDP;
begin
  inherited;
  FreeAndNil( FCombiIDManage );
  FreeAndNil( FSendCombiBufferManage );
  FreeAndNil( FRecvCombiBufferManage );
  FreeAndNil( FCombiPackageBufferManage );
end;

procedure TxdUdpIoHandle.DoAfterOpenUDP;
begin
  InitStatValue;
end;

procedure TxdUdpIoHandle.DoBeforCloseUDP;
begin

end;

function TxdUdpIoHandle.DoBeforOpenUDP: Boolean;
begin
  Result := True;
  try
    {组合缓存内存管理}
    FCombiPackageBufferManage := TxdFixedMemoryManager.Create( CtMaxUdpSize, FCombiMaxBufferCount );
    {发送缓存数据结构}
    FSendCombiBufferManage := TSendCombiBufferManage.Create;
    with FSendCombiBufferManage do
    begin
      HashTableCount := SendCombiHashTableCount;
      MaxBufferCount := SendCombiMaxNodeCount;
      MaxKeepBufferTime := SendCombiMaxKeepTime;
      OnDeleteCombiBuffer := DoReclaimSendcombiBuffer;
      Active := True;
    end;
    {接收缓存数据结构}
    FRecvCombiBufferManage := TRecvCombiBufferManage.Create;
    with FRecvCombiBufferManage do
    begin
      HashTableCount := RecvCombiHashTableCount;
      MaxBufferCount := RecvCombiMaxNodeCount;
      MaxKeepBufferTime := RecvCombiMaxKeepTime;
      MaxTimeoutCount := RecvCombiMaxTimeoutCount;
      OnCalcCombiID := DoCalcCombiIDWhenRecvCombBuffer;
      OnRecvTimeoutToDelete := DoRecvCombiBufferTimeount;
      OnRecvFinishedCombiBuffer := DoRecvCombiBuffer;
      OnReGetComibBuffer := DoReGetCombiBuffer;
      OnDeleteCombiBuffer := DoReclaimRecvCombiBuffer;
      Active := True;
    end;
    {组合包序号管理}
    FCombiIDManage := TWordIdManage.Create;
  except
    Result := False;
  end;
end;

function TxdUdpIoHandle.DoCalcCombiIDWhenRecvCombBuffer(const AIP: Cardinal; const APort, ACombiID: Word): Cardinal;
begin
  Result := AIP + ACombiID + APort;
end;

procedure TxdUdpIoHandle.DoReclaimSendcombiBuffer(const Ap: PCombiBufferInfo);
var
  i: Integer;
begin
  for i := 0 to CtMaxCombiPackageCount - 1 do
  begin
    if Assigned(Ap^.FPackages[i]) then
      FCombiPackageBufferManage.FreeMem( Ap^.FPackages[i] );
  end;
  FCombiIDManage.ReclaimWordID( Ap^.FCombiID );
end;

procedure TxdUdpIoHandle.DoReclaimRecvCombiBuffer(const Ap: PCombiBufferInfo);
var
  i: Integer;
begin
  for i := 0 to CtMaxCombiPackageCount - 1 do
  begin
    if Assigned(Ap^.FPackages[i]) then
      FCombiPackageBufferManage.FreeMem( Ap^.FPackages[i] );
  end;
end;

procedure TxdUdpIoHandle.DoErrorInfo(const AInfo: PAnsiChar);
begin
  JxdDbg( AInfo );
end;

procedure TxdUdpIoHandle.DoRecvBuffer;
var
  Package: array[0..CtMaxUdpSize - 1] of Byte;
  nLen: Integer;
  addr: TSockAddrIn;
  nVer, nPackageInfo, nPackageCount, nPackageIndex, nCmd: Byte;
  nPackageLen, nCombiID: Word;
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

  InterlockedIncrement( FRecvPackageCount );

  if nLen <= CtSinglePackageHeadSize then
  begin
    InterlockedIncrement( FRecvErrorPackageCount );
    DoErrorInfo( '接收到的数据长度过小' );
    Exit;
  end;
  //单包   版本(1) - 此包总长度(2) - CRC32码(4) - 包信息(1)[前四位：0； 后四位: 协议命令，当为0时，表示需要由子类处理]
  //组合包 版本(1) - 此包总长度(2) - CRC32码(4) - 包信息(1)[前四位：个数； 后四位: 包顺序] - 标志(2)
  nVer := Byte(Package[0]);
  if nVer <> CtCurUDPVerNumber then
  begin
    InterlockedIncrement( FRecvErrorPackageCount );
    DoErrorInfo( '当前协议版本有误' );
    Exit;
  end;
  Move( Package[1], nPackageLen, 2 );
  if nPackageLen <> nLen then
  begin
    InterlockedIncrement( FRecvErrorPackageCount );
    DoErrorInfo( '包的长度有误' );
    Exit;
  end;
  Move( Package[3], nCrc32Code, 4 );
  Move( package[7], nPackageInfo, 1);
  ParsePackageInfo( nPackageInfo, nPackageCount, nPackageIndex );
  if nPackageCount = 0 then
  begin
    //独立包
    pData := @Package[CtSinglePackageHeadSize];
    nLen := nLen - CtSinglePackageHeadSize;
    if not DecodeBuffer( nCrc32Code, pData, nLen ) then
    begin
      InterlockedIncrement( FRecvErrorPackageCount );
      DoErrorInfo( 'CRC32解密出错' );
      Exit;
    end;
    InterlockedIncrement( FRecvFullPackageCount );
    nIP := addr.sin_addr.S_addr;
    nPort := htons(addr.sin_port);
    nCmd := nPackageIndex;
    if nCmd = 0 then
      OnRecvBuffer( nIP, nPort, pData, nLen )
    else
    begin
      case nCmd of
        CtReGetCombiInfoCmd: DoRecvReGetCombiPackage( nIP, nPort, pData, nLen );
        else
        begin
          InterlockedIncrement( FRecvErrorPackageCount );
          JxdDbg( '接收到一个未知的协议包，将丢弃此数据包' );
        end;
      end;
    end;
  end
  else
  begin
    //组合包
    if nLen <= CtCombiPackageHeadSize then
    begin
      InterlockedIncrement( FRecvErrorPackageCount );
      DoErrorInfo( '接收到的数据长度过小' );
      Exit;
    end;
    Move( Package[8], nCombiID, 2 );
    pData := @Package[CtCombiPackageHeadSize];
    nLen := nLen - CtCombiPackageHeadSize;
    if not DecodeBuffer( nCrc32Code, pData, nLen ) then
    begin
      InterlockedIncrement( FRecvErrorPackageCount );
      DoErrorInfo( 'CRC32解密出错' );
      Exit;
    end;
    nIP := addr.sin_addr.S_addr;
    nPort := htons(addr.sin_port);
    OnRecvCombiPackage( nIP, nPort, pData, nLen, nPackageCount, nPackageIndex, nCombiID );
  end;
end;

procedure TxdUdpIoHandle.DoRecvCombiBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
begin
  InterlockedIncrement( FRecvFullPackageCount );
  OnRecvBuffer( AIP, APort, ABuffer, ABufLen );
end;

procedure TxdUdpIoHandle.DoRecvCombiBufferTimeount(const Ap: PCombiBufferInfo);
begin
  InterlockedIncrement( FDropRecvCombiPackageCount );
end;

procedure TxdUdpIoHandle.OnRecvCombiPackage(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Word; const APackageCount,
  APackageIndex: Byte; const ACombiID: Word);
var
  pCombi: PCombiBufferInfo;
  pPackage: PAnsiChar;
begin
  pCombi := FRecvCombiBufferManage.GetCombiBuffer( AIP, APort, ACombiID, True );
  if Assigned(pCombi)	 then
  begin
    //此时 FRecvCombiBufferManage 处于锁定状态，一定要调用 FRecvCombiBufferManage.FinishedCombiBuffer，不然多线程会死锁
    try
      if pCombi^.FCombiPackageCount = 0 then
      begin
        pCombi^.FCombiID := ACombiID;
        pCombi^.FCombiPackageCount := APackageCount;
        pCombi^.FCurCombiPackCount := 0;
        pCombi^.FCombiIP := AIP;
        pCombi^.FCombiPort := APort;
      end
      else
      begin
        if pCombi^.FCombiPackageCount <> APackageCount then
        begin
          DoErrorInfo( '搜索到的组合缓存的包个数与接收到的包个数不一致，将丢弃接收到的包和已经存放到缓存中的组合包' );
          FRecvCombiBufferManage.CancelCombiBuffer( pCombi );
          Exit;
        end;
      end;
      if pCombi^.FPackages[APackageIndex] = nil then
      begin
        if not FCombiPackageBufferManage.GetMem(Pointer(pPackage)) then
        begin
          DoErrorInfo( '接收到组合包后，无法申请存放组合的内存，将不做处理，直接丢弃刚接收到的组合包' );
          Exit;
        end;
        Move( ABuffer^, pPackage^, ABufLen );
        pCombi^.FPackages[APackageIndex] := pPackage;
        pCombi^.FPackageLens[APackageIndex] := ABufLen;

        pCombi^.FCurCombiPackCount := pCombi^.FCurCombiPackCount + 1;
      end;
    finally
      FRecvCombiBufferManage.FinishedCombiBuffer( pCombi );
    end;
  end;
end;

procedure TxdUdpIoHandle.DoRecvReGetCombiPackage(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Word);
var
  pCombi: PCombiBufferInfo;
  pBuf: PReGetCombiInfo;
  i: Byte;
  nIndex: Integer;
  bOK: Boolean;
begin
  if ABufLen <> CtReGetCombiInfoSize then
  begin
    InterlockedIncrement( FRespondReGetLostCmdFailCount );
    DoErrorInfo( '处理重新找回丢包时，接收到的长度与规定不一致，丢弃数据' );
    Exit;
  end;
  pBuf := PReGetCombiInfo( ABuffer );
  if pBuf^.FCmd <> CtReGetCombiInfoCmd then
  begin
    InterlockedIncrement( FRespondReGetLostCmdFailCount );
    DoErrorInfo( '处理重新找回丢包时，命令不正确，丢弃数据' );
    Exit;
  end;
  if pBuf^.FCount > 15 then //四位表示最大的值
  begin
    InterlockedIncrement( FRespondReGetLostCmdFailCount );
    DoErrorInfo( '处理重新找回丢包时，命令pBuf^.FCount不正确，丢弃数据' );
    Exit;
  end;
  bOK := False;
  pCombi := FSendCombiBufferManage.GetCombiBuffer( pBuf^.FCombiID, False );
  if Assigned(pCombi) then
  begin
    try
      for i := 0 to pBuf^.FCount - 1 do
      begin
        nIndex := pBuf^.FIndexs[i];
        if (pCombi^.FPackageLens[nIndex] = 0) or (not Assigned(pCombi^.FPackages[nIndex])) then
        begin
          DoErrorInfo( '数据可能已经被删除，无法重发指定缓存' );  //程序发现此问题，多线程时发生
          Continue;
        end;
        _DoSendPackage( AIp, APort, pCombi^.FPackages[nIndex], pCombi^.FPackageLens[nIndex] );
        bOK := True;
      end;
    finally
      FSendCombiBufferManage.FinishedCombiBuffer( pCombi );
    end;
  end;
  if bOK then
    InterlockedIncrement( FRespondReGetLostCmdSuccessCount )
  else
    InterlockedIncrement( FRespondReGetLostCmdFailCount );
end;

procedure TxdUdpIoHandle.DoReGetCombiBuffer(const Ap: PCombiBufferInfo);
var
  i, nIndex: Integer;
  cmd: TReGetCombiInfo;
  bOK: Boolean;
begin
  //发送单个包,数据长度小于等待 CtSinglePackageSize 包头 9 个字节
  //版本(1) - 此包总长度(2) - CRC32码(4) - 包个数(1)[值: 0]
  cmd.FCmd := CtReGetCombiInfoCmd;
  cmd.FCombiID := Ap^.FCombiID;
  cmd.FCount := Ap^.FCombiPackageCount - Ap^.FCurCombiPackCount;
  nIndex := 0;
  bOK := False;
  for i := 0 to Ap^.FCombiPackageCount - 1 do
  begin
    if not Assigned(Ap^.FPackages[i]) then
    begin
      bOK := True;
      cmd.FIndexs[nIndex] := i;
      Inc( nIndex );
    end;
  end;
  //如果没有找到，则是数据已经接收到,此时不需要再重发数据
  if bOK then
  begin
    _SendProtocolCmdSinglePackage( Ap^.FCombiIP, Ap^.FCombiPort, CtReGetCombiInfoCmd, @cmd, CtReGetCombiInfoSize );
    InterlockedIncrement( FReGetLostCmdCount );
  end;
end;

function TxdUdpIoHandle.GetCombiBufferCount: Integer;
begin
  if Active then
    Result := FCombiPackageBufferManage.Count
  else
    Result := 0;
end;

function TxdUdpIoHandle.GetCombiIDCount: Integer;
begin
  if Active then
    Result := FCombiIDManage.Count
  else
    Result := 0;
end;

function TxdUdpIoHandle.GetCombiMaxIDCount: Integer;
begin
  if Active then
    Result := FCombiIDManage.Caption
  else
    Result := 0;
end;

function TxdUdpIoHandle.GetCombiMaxPackageCount: Integer;
begin
  Result := CtMaxCombiPackageCount;
end;

function TxdUdpIoHandle.GetRecvCombiCount: Integer;
begin
  if Active then
    Result := FRecvCombiBufferManage.Count
  else
    Result := 0;
end;

function TxdUdpIoHandle.GetSendCombiCount: Integer;
begin
  if Active then
    Result := FSendCombiBufferManage.Count
  else
    Result := 0;
end;

procedure TxdUdpIoHandle.InitStatValue;
begin
  FRecvErrorPackageCount := 0;
  FSendSuccessPackageCount := 0;
  FSendFailPackageCount := 0;
  FRecvPackageCount := 0;
  FReGetLostCmdCount := 0;
  FRespondReGetLostCmdSuccessCount := 0;
  FRespondReGetLostCmdFailCount := 0;
  FRecvFullPackageCount := 0;
  FSendFullPackageCount := 0;
  FDropRecvCombiPackageCount := 0;
end;

procedure TxdUdpIoHandle.OnRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
begin
//  JxdDbg( ABuffer );
end;

function TxdUdpIoHandle.SendBuffer(const AIP: Cardinal; const APort: Word; const ApBuffer: PAnsiChar; const ABufLen: Integer): Integer;
begin
  if ABufLen <= CtSinglePackageSize then
    Result := _SendSinglePackage( AIP, APort, ApBuffer, ABufLen )
  else
    Result := _SendCombiPackage( AIP, APort, ApBuffer, ABufLen );
  if Result = ABufLen then
    InterlockedIncrement( FSendFullPackageCount );
end;

procedure TxdUdpIoHandle.SetCombiMaxBufferCount(const Value: Integer);
begin
  if not Active and (FCombiMaxBufferCount <> Value) and (Value > 0) then
    FCombiMaxBufferCount := Value;
end;

procedure TxdUdpIoHandle.SetRecvCombiHashTableCount(const Value: Integer);
begin
  if not Active and (FRecvCombiHashTableCount <> Value) and (Value > 0) then
    FRecvCombiHashTableCount := Value;
end;

procedure TxdUdpIoHandle.SetRecvCombiMaxNodeCount(const Value: Integer);
begin
  if not Active and (FRecvCombiMaxNodeCount <> Value) and (Value > 0) then
    FRecvCombiMaxNodeCount := Value;
end;

procedure TxdUdpIoHandle.SetRecvCombiMaxTimeoutCount(const Value: Integer);
begin
  if (FRecvCombiMaxTimeoutCount <> Value) and (Value >= 0) then
  begin
    FRecvCombiMaxTimeoutCount := Value;
    if Active then
      FRecvCombiBufferManage.MaxTimeoutCount := FRecvCombiMaxTimeoutCount;
  end;
end;

procedure TxdUdpIoHandle.SetRecvCombiMaxKeepTime(const Value: Cardinal);
begin
  if FRecvCombiMaxKeepTime <> Value then
  begin
    FRecvCombiMaxKeepTime := Value;
    if Active then
      FRecvCombiBufferManage.MaxKeepBufferTime := FRecvCombiMaxKeepTime;
  end;
end;

procedure TxdUdpIoHandle.SetSendCombiHashTableCount(const Value: Integer);
begin
  if not Active and (FSendCombiHashTableCount <> Value) and (Value > 0) then
    FSendCombiHashTableCount := Value;
end;

procedure TxdUdpIoHandle.SetSendCombiMaxNodeCount(const Value: Integer);
begin
  if not Active and (FSendCombiMaxNodeCount <> Value) and (Value > 0) then
    FSendCombiMaxNodeCount := Value;
end;

function TxdUdpIoHandle._DoSendPackage(const AIP: Cardinal; const APort: Word; const ApPackageAddr: pAnsiChar; const APackageLen: Word): Integer;
var
  nErrorCode: Integer;
label
  llTryWouldBlock;
begin
llTryWouldBlock:
  Result := __SendBuffer( AIP, APort, ApPackageAddr^, APackageLen );
  if Result = -1 then
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
  if Result = APackageLen then
    InterlockedIncrement( FSendSuccessPackageCount )
  else
    InterlockedIncrement( FSendFailPackageCount );
end;

function TxdUdpIoHandle._SendCombiPackage(const AIP: Cardinal; const APort: Word; const ApBuffer: pAnsiChar; const ABufferSize: Integer): Integer;
var
  nCombiID: Word;
  pPackage, pDataTempBuf: PAnsiChar;
  nPackageCount, nPackageInfo: Byte;
  nPackageLen, nDataLen: Word;
  nCrc32Code: Cardinal;
  pBuffer: pAnsiChar;
  i, nSendByteCount: Integer;
  pCombi: PCombiBufferInfo;
begin
  nPackageCount := (ABufferSize + CtCombiPackageSize - 1) div CtCombiPackageSize;
  if nPackageCount > CtMaxCombiPackageCount then
  begin
    Result := -1;
    DoErrorInfo( '发送数据量过多，超过最多许可，无法发送' );
    Exit;
  end;
  if not FCombiIDManage.GetWordID(nCombiID) then
  begin
    DoErrorInfo( '无法申请组合包序号' );
    Result := -1;
    Exit;
  end;
  Result := 0;
  pBuffer := ApBuffer;
  pCombi := FSendCombiBufferManage.GetCombiBuffer( nCombiID, True );
  if Assigned(pCombi) then
  begin
    try
      if pCombi^.FCurCombiPackCount <> 0 then
      begin
        DoErrorInfo( '申请的发送缓存数据不正确' );
        FCombiIDManage.ReclaimWordID( nCombiID );
        FSendCombiBufferManage.CancelCombiBuffer( pCombi );
        Result := -1;
        Exit;
      end;
      pCombi^.FCombiPackageCount := nPackageCount;
      for i := 0 to nPackageCount - 1 do
      begin
        if not FCombiPackageBufferManage.GetMem(Pointer(pPackage)) then
        begin
          DoErrorInfo( '申请的缓存不足，无法发送组合包' );
          FSendCombiBufferManage.CancelCombiBuffer( pCombi );  //pCombi 被重置为 nil
          Result := -1;
          Exit;
        end;

        //开始组包发送
        if i = nPackageCount - 1 then
          nDataLen := ABufferSize - (nPackageCount - 1) * CtCombiPackageSize
        else
          nDataLen := CtCombiPackageSize;

        //组合包 版本(1) - 此包总长度(2) - CRC32码(4) - 包信息(1)[前四位：个数； 后四位: 包顺序] - 标志(2)
        nPackageInfo := FormatPackageInfo( nPackageCount, i );
        nPackageLen := CtCombiPackageHeadSize + nDataLen;
        pDataTempBuf := pAnsiChar(Integer(pPackage) + CtCombiPackageHeadSize);
        Move( pBuffer^, pDataTempBuf^, nDataLen ); //数据
        nCrc32Code := EncodeBuffer( pDataTempBuf, nDataLen );

        pPackage^ := Char( CtCurUDPVerNumber );                              //协议版本号
        Move( nPackageLen, pAnsiChar( Integer(pPackage) + 1 )^, 2 );         //此包总长度
        Move( nCrc32Code, pAnsiChar( Integer(pPackage) + 3 )^, 4 );          //CRC32码
        Move( nPackageInfo, pAnsiChar( Integer(pPackage) + 7 )^, 1 );        //包信息(1)[前四位：个数； 后四位: 包顺序]
        Move( nCombiID, pAnsiChar( Integer(pPackage) + 8 )^, 2 );            //组合包标志

        //先记录
        pCombi^.FPackageLens[i] := nPackageLen;
        pCombi^.FPackages[i] := pPackage ;

        //后发送
        nSendByteCount := _DoSendPackage( AIP, APort, pPackage, nPackageLen );

        if nSendByteCount = nPackageLen then
        begin
          Dec( nSendByteCount, CtCombiPackageHeadSize );
          Inc( Result, nSendByteCount );
        end
        else
        begin
          DoErrorInfo( '发送组合包失败' );
          FSendCombiBufferManage.CancelCombiBuffer( pCombi );  //pCombi 被重置为 nil
          Result := -1;
          Break;
        end;
        pBuffer := pAnsiChar(Integer(pBuffer) + nDataLen);
      end;
    finally
      FSendCombiBufferManage.FinishedCombiBuffer( pCombi );
    end;
  end
  else
  begin
    //
    DoErrorInfo( '申请不到发送缓存的数据结构，无法进行组合包发送' );
  end;
end;

function TxdUdpIoHandle._SendProtocolCmdSinglePackage(const AIP: Cardinal; const APort: Word; const ACmd: Byte; const ApBuffer: pAnsiChar;
  const ABufferSize: Integer): Integer;
var
  nPackageLen: Word;
  nCrc32Code: Cardinal;
  buf: array[0..CtMaxUdpSize - 1] of Byte;
  nPackageInfo: Byte;
begin
  //发送单个包,数据长度小于等待 CtSinglePackageSize 包头 9 个字节
  //版本(1) - 此包总长度(2) - CRC32码(4) - 包个数(1)[值: 0]
  nPackageLen := CtSinglePackageHeadSize + ABufferSize;
  nPackageInfo := FormatPackageInfo( 0, ACmd );

  Move( ApBuffer^, buf[8], ABufferSize );               //写入要发送数据
  nCrc32Code := EncodeBuffer( @buf[8], ABufferSize );
  buf[0] := CtCurUDPVerNumber;                          //版本
  Move( nPackageLen, buf[1], 2 );                       //此包总长度
  Move( nCrc32Code, buf[3], 4 );                        //CRC32码
  Move( nPackageInfo, buf[7], 1 );                      //包信息

  Result := _DoSendPackage( AIP, APort, @buf, nPackageLen );
  if Result <> -1 then
    Dec( Result, CtSinglePackageHeadSize );
end;

function TxdUdpIoHandle._SendSinglePackage(const AIP: Cardinal; const APort: Word; const ApBuffer: pAnsiChar; const ABufferSize: Integer): Integer;
var
  nPackageLen: Word;
  nCrc32Code: Cardinal;
  buf: array[0..CtMaxUdpSize - 1] of Byte;
  nCount: Byte;
begin
  //发送单个包,数据长度小于等待 CtSinglePackageSize 包头 9 个字节
  //版本(1) - 此包总长度(2) - CRC32码(4) - 包个数(1)[值: 0]
  nPackageLen := CtSinglePackageHeadSize + ABufferSize;
  nCount := 0;

  Move( ApBuffer^, buf[8], ABufferSize );               //写入要发送数据
  nCrc32Code := EncodeBuffer( @buf[8], ABufferSize );
  buf[0] := CtCurUDPVerNumber;                          //版本
  Move( nPackageLen, buf[1], 2 );                       //此包总长度
  Move( nCrc32Code, buf[3], 4 );                        //CRC32码
  Move( nCount, buf[7], 1 );                            //包个数

  Result := _DoSendPackage( AIP, APort, @buf, nPackageLen );
  if Result <> -1 then
    Dec( Result, CtSinglePackageHeadSize );
end;

procedure TxdUdpIoHandle.SetSendCombiMaxKeepTime(const Value: Cardinal);
begin
  if FSendCombiMaxKeepTime <> Value then
  begin
    FSendCombiMaxKeepTime := Value;
    if Active then
      FSendCombiBufferManage.MaxKeepBufferTime := FSendCombiMaxKeepTime;
  end;
end;

{ TSendCombiBufferManage }

function TSendCombiBufferManage.ActiveBuffer: Boolean;
begin
  FHashBasic := THashArray.Create;
  Result := inherited ActiveBuffer;
  FHashListEx := FHashBasic as THashArray;
end;

procedure TSendCombiBufferManage.DoDeleteFormHashList(const ApCombiBuffer: PCombiBufferInfo);
var
  p: Pointer;
begin
  FHashListEx.Find( ApCombiBuffer^.FCombiID, p, True );
end;

procedure TSendCombiBufferManage.DoHandleTimeoutCombiBuffer(ApCombiBuffer: PCombiBufferInfo; var ADel: Boolean);
begin
  //当发送缓存超时，只需要简单把它删除就可以，由父类释放相应的内存
  ADel := True;
end;

procedure TSendCombiBufferManage.FinishedCombiBuffer(const Ap: PCombiBufferInfo);
begin
  if Assigned(Ap) then
  begin
    Ap^.FLastActiveTime := GetTickCount;
    ActiveThread( MaxKeepBufferTime div 2 );
  end;
end;

function TSendCombiBufferManage.GetCombiBuffer(const ACombiID: Word; const ACreateNewWhenNoExists: Boolean): PCombiBufferInfo;
var
  pData: PCombiBufferInfo;
begin
  Result := nil;
  EnterCriticalSection( FHashListLock );
  try
    if not FHashListEx.Find(ACombiID, Pointer(pData), False) then
    begin
      if ACreateNewWhenNoExists then
      begin
        if not FCombiHashBufferManage.GetMem(Pointer(pData)) then
        begin
          JxdDbg( '发送端组合包缓存内存消耗光，无法再缓存发送数据（需要缓存的组合ID：%d）', [ACombiID] );
          Exit;
        end;
        pData^.FCombiID := ACombiID;
        pData^.FCombiIP := 0;
        pData^.FCombiPort := 0;
        pData^.FLastActiveTime := 0; //指定为0时，检查线程将不对它进行判断
        FHashListEx.Add( ACombiID, pData );
        Result := pData;
      end
    end
    else
    begin
      pData^.FLastActiveTime := 0;
      Result := pData;
    end;
  finally
    LeaveCriticalSection( FHashListLock );
  end;
end;

{ TRecvCombiBufferManage }

function TRecvCombiBufferManage.ActiveBuffer: Boolean;
begin
  FHashBasic := THashArrayEx.Create;
  Result := inherited ActiveBuffer;
  FHashListEx := FHashBasic as THashArrayEx;
end;

function TRecvCombiBufferManage.DoCalcCombiID(const AIP: Cardinal; const APort, ACombiID: Word): Cardinal;
begin
  Result := OnCalcCombiID( AIP, APort, ACombiID );
//  if Assigned(OnCalcCombiID) then
//    Result := OnCalcCombiID( AIP, APort, ACombiID )
//  else
//    Result := AIP + ACombiID + APort;
end;

procedure TRecvCombiBufferManage.DoDeleteFormHashList(const ApCombiBuffer: PCombiBufferInfo);
var
  p: PHashNode;
begin
  p := FHashListEx.FindBegin( DoCalcCombiID(ApCombiBuffer^.FCombiIP, ApCombiBuffer^.FCombiPort, ApCombiBuffer^.FCombiID) );
  try
    while Assigned(p) do
    begin
      if Integer(p^.NodeData) = Integer(ApCombiBuffer) then
      begin
        FHashListEx.FindDelete( p );
        Break;
      end;
      p := FHashListEx.FindNext( p );
    end;
  finally
    FHashListEx.FindEnd;
  end;
end;

procedure TRecvCombiBufferManage.DoHandleTimeoutCombiBuffer(ApCombiBuffer: PCombiBufferInfo; var ADel: Boolean);
begin
  if ApCombiBuffer^.FTimeOutCount > MaxTimeoutCount then
  begin
    OnRecvTimeoutToDelete( ApCombiBuffer );
    ADel := True;
    JxdDbg( '组合包完全失败，将删除此不完整的组合包' );
  end
  else
  begin
    ADel := False;
    OnReGetComibBuffer( ApCombiBuffer );
    ApCombiBuffer^.FTimeOutCount := ApCombiBuffer^.FTimeOutCount + 1;
    ApCombiBuffer^.FLastActiveTime := GetTickCount;
  end;
end;

procedure TRecvCombiBufferManage.FinishedCombiBuffer(const Ap: PCombiBufferInfo);
var
  i, nBufLen, nPos: Integer;
  pBuf: PAnsiChar;
begin
  if Assigned(Ap) then
  begin
    //处于锁定状态
    try
      if Ap^.FCombiPackageCount = Ap^.FCurCombiPackCount then
      begin
        //当接收到全部组合包时
        nBufLen := 0;
        for i := 0 to Ap^.FCombiPackageCount - 1 do
        begin
          if Ap^.FPackageLens[i] = 0 then
          begin
            JxdDbg( '合并组合包时，出现某个包长度为: 0; 说明此包有错误，将直接将此组合缓存回收' );
            DoReclaimCombiBuffer( Ap, True );
            Exit;
          end;
          if Ap^.FPackages[i] = nil then
          begin
            JxdDbg( '合并组合包时，出现某个包的地址为nil; 说明此包有错误，将直接将此组合缓存回收' );
            DoReclaimCombiBuffer( Ap, True );
            Exit;
          end;
          nBufLen := nBufLen + Ap^.FPackageLens[i];
        end;
        GetMem( pBuf, nBufLen );
        try
          nPos := 0;
          for i := 0 to Ap^.FCombiPackageCount - 1 do
          begin
            Move( PAnsiChar(Ap^.FPackages[i])^, pBuf[nPos], Ap^.FPackageLens[i] );
            nPos := nPos + Ap^.FPackageLens[i];
          end;
          OnRecvFinishedCombiBuffer( Ap^.FCombiIP, Ap^.FCombiPort, pBuf, nBufLen );
        finally
          FreeMem( pBuf );
        end;
        DoReclaimCombiBuffer( Ap, True );
      end
      else
      begin
        Ap^.FLastActiveTime := GetTickCount;
        ActiveThread( MaxKeepBufferTime div 2 );
      end;
    finally
      LeaveCriticalSection( FHashListLock );
    end;
  end;
end;

function TRecvCombiBufferManage.GetCombiBuffer(const AIP: Cardinal; const APort, ACombiID: Word; const ACreateNewWhenNoExists: Boolean): PCombiBufferInfo;
var
  nID: Cardinal;
  pHash: PHashNode;
  pData: PCombiBufferInfo;
  bFind: Boolean;
begin
  nID := DoCalcCombiID( AIP, APort, ACombiID );
  bFind := False;
  Result := nil;
  EnterCriticalSection( FHashListLock );   //锁定, 如果找到结果则一直锁定，直到调用FinishedCombiBuffer; 否则离开函数里解锁
  try
    pHash := FHashListEx.FindBegin( nID );
    try
      while Assigned(pHash) do
      begin
        pData := PCombiBufferInfo( pHash^.NodeData );
        if (pData^.FCombiID = ACombiID) and (pData^.FCombiIP = AIP) and (pData^.FCombiPort = APort) then
        begin
          pData^.FLastActiveTime := 0;
          Result := pData;
          bFind := True;
          Break;
        end;
        pHash := FHashListEx.FindNext( pHash );
      end;
    finally
      FHashListEx.FindEnd;
    end;

    if ACreateNewWhenNoExists and not bFind then
    begin
      if FCombiHashBufferManage.GetMem(Pointer(pData)) then
      begin
        pData^.FCombiID := ACombiID;
        pData^.FCombiIP := AIP;
        pData^.FCombiPort := APort;
        pData^.FLastActiveTime := 0; //指定为0时，检查线程将不对它进行判断
        FHashListEx.Add( nID, pData );
        Result := pData;
      end
      else
        JxdDbg( '发送端组合包缓存内存消耗光，无法再缓存发送数据（需要缓存的组合ID：%d）', [ACombiID] )
    end;
  finally
    if not Assigned(Result) then
      LeaveCriticalSection( FHashListLock );
  end;
end;

procedure TRecvCombiBufferManage.SetMaxTimeoutCount(const Value: Integer);
begin
  if (FMaxTimeoutCount <> Value) and (Value >= 0) then
    FMaxTimeoutCount := Value;
end;

end.

