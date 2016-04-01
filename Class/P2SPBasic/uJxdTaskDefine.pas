{
TRequestBlockManage: 
  非安全模式，需要外部提供锁定操作。
  滑动窗口，自己适应请求的大小
}
unit uJxdTaskDefine;

interface

uses
  Windows, SysUtils, uJxdUdpDefine, uJxdFileSegmentStream, uJxdHashCalc;
  
type
  TDownTaskStyle = (dssDefaul, dssSoftUpdata);
  ///////////////////////////////////////////////////////////////////////////////////////////////////
  ///                                                                                            ///
  //请求分块管理器, 每次请求数据时重置。实际是一个不定长的滑动窗口                               ///
  ///                                                                                            ///
  //////////////////////////////////////////////////////////////////////////////////////////////////
  PRequestTableInfo = ^TRequestTableInfo;
  TRequestTableInfo = record    
    FCurRequestBlockCount: Integer; //当前请求分块的数量
    FCurRespondBlockCount: Integer; //当前响应分场的数量    
    FBlockTables: array of Cardinal; //记录每次请求小分块的信息
  end;
  TRequestBlockManage = class
  public
    constructor Create(const ATableCount: Integer = 2; const AInitBlockCount: Integer = 32 );
    destructor  Destroy; override;
    
    //请求开始
    function  BeginRequest: Integer; //开始请求分块, 返回可请求数量
    function  AddRequestBlock(const ASegIndex, ABlockIndex: Integer): Boolean; //添加请求,记录请求信息    
    procedure FinishedRequestBlock(const ASegIndex, ABlockIndex: Integer; const ARecvByteCount: Cardinal); //完成请求
  private
    FTables: array of TRequestTableInfo;
    FTableCount: Integer;
    FCurTableIndex: Integer;
    FBlockCount: Integer;
    FMaxContiguousCount: Integer; //连续保存最大请求数量
    FLessThanHalfContiguousCount: Integer; //连续小于最大值的一半数量
    FAutoChangedBlockCount: Boolean;
    FRecvByteCount: Integer; 
    FBeginRequestTime: Cardinal;
    FCurSize: Cardinal;
    FCurSpeed: Integer;
    function  CalcID(const ASegIndex, ABlockIndex: Integer): Cardinal; inline;
    procedure ReBuildBlockTable(const ANewBlockCount: Integer);
  public
    property CurSpeed: Integer read FCurSpeed;   //当前速度
    property RecvByteCount: Integer read FRecvByteCount; //总共下载大小
    property AutoChangedBlockCount: Boolean read FAutoChangedBlockCount write FAutoChangedBlockCount; //是否自动扩展请求列表
  end;
  
  {
                                               P2P源信息结构
  }
  TSourceState = (ssUnkown, ssChecking, ssSuccess, ssFail);
  PP2PSourceInfo = ^TP2PSourceInfo;
  TP2PSourceInfo = record
    FIP: Cardinal; //P2P用户ID
    FPort: Word; //P2P用户端口
    FState: TSourceState; //P2P状态
    FServerSource: Boolean; //是否是服务源，服务器源表示此源对此文件拥有所有数据，否则需要定时查询此源的分段信息  
    FNextTimeoutTick: Cardinal; //下次超时时间
    FTimeoutCount: Integer; //超时次数，请求数据后，无接收到数据时使用
    FRequestBlockManage: TRequestBlockManage; //请求分块管理器（另类的滑动窗口）
    FSegTableState: TxdSegmentStateTable; //P2P用户下载文件分段信息表, 如果是服务器源则不需要
  end;
  
  {
                                              Http源信息结构
  }
  PHttpSourceInfo = ^THttpSourceInfo;
  THttpSourceInfo = record
    FUrl: string;
    FReferUrl: string;
    FCookies: string;
    FCheckSizeStyle: TSourceState; //数据源状态
    FTotalRecvByteCount: Cardinal; //接收到数据大小
    FRefCount: Integer; //引用数量
    FErrorCount: Integer; //连接无法获取数据次数
    FCheckingSize: Boolean; //是否正在检查大小
  end;
  
  {
                                   等待查询可用性的P2P数据源结构体
  }
  PCheckSourceInfo = ^TCheckSourceInfo;
  TCheckSourceInfo = record
    FUserID: Cardinal;    
    FIP: Cardinal;
    FPort: Word;
    FCheckState: TConnectState; 
    FLastActiveTime: Cardinal;   
  end;
  THashThreadState = (htsNULL, htsRunning, htsFinished);

  {
                                            添加下载任务参数
  }
  PdtpP2PSourceInfo = ^TdtpP2PSourceInfo;
  TdtpP2PSourceInfo = record
    FIP: Cardinal;
    FPort: Word;
  end;
  TArydtpP2PSourceInfo = array of TdtpP2PSourceInfo;  

  PdtpHttpSourceInfo = ^TdtpHttpSourceInfo;
  TdtpHttpSourceInfo = record
    FUrl: string;
    FReferUrl: string;
    FCookie: string;
    FTotoalByteCount: Integer;
  end;
  TArydtpHttpSourceInfo = array of TdtpHttpSourceInfo;
  
  PDownTaskParam = ^TDownTaskParam;
  TDownTaskParam = record
    FTaskID: Integer;
    FTaskStyle: TDownTaskStyle; //类型
    FTaskName: string; //任务名称 可为空
    FFileName: string; //下载任务保存文件名称 下载全文件名，不能为空
    FSegmentSize: Integer;
    FFileSize: Int64;
    FFileHash, FWebHash: TxdHash;
    FP2PSource: TArydtpP2PSourceInfo; //P2P源    
    FHttpSource: TArydtpHttpSourceInfo; //Http源
    FFileFinishedInfos: TAryFileFinishedInfos; //文件已经完成的信息
    FTaskData: Pointer;
  end;

  {
                                           下载任务信息结构体
  }
  PTaskProgressInfo = ^TTaskProgressInfo;
  TTaskProgressInfo = record
    FTaskID: Integer;
    FActive: Boolean;
    FFail: Boolean;
    FTaskName: string;
    FFileName: string;
    FFileSize, FCompletedSize: Int64; //文件大小，完成大小
    FCurSpeed, FAdvSpedd: Integer; //当前速度，副平均速度
    FTaskStyle: TDownTaskStyle; //任务的类型
  end;

  {
                                        下载任务详细信息结构体  
  }
  TP2PDownDetailInfo = record
    FIP: Cardinal;
    FPort: Word;
    FCurSpeed: Integer; //当前速度 b/ms
    FTotalByteCount: Integer; 
  end;  
  TAryP2PDownDetailInfos = array of TP2PDownDetailInfo;
  
  TOtherDownDetailInfo = record
    FProviderInfo: string; //提供者信息
    FCurSpeed: Integer;
    FTotalByteCount: Integer;
  end;
  TAryOtherDownDetailInfos = array of TOtherDownDetailInfo;
  
  PTaskDownDetailInfo = ^TTaskDownDetailInfo;
  TTaskDownDetailInfo = record
    FTaskID: Integer;
    FInvalideBufferSize: Cardinal; //无效数据
    FP2PDownDetails: TAryP2PDownDetailInfos;
    FOtherDownDetails: TAryOtherDownDetailInfos
  end;
  
const
  CtP2PSourceInfoSize = SizeOf(TP2PSourceInfo);
  CtHttpSourceInfoSize = SizeOf(THttpSourceInfo);
  CtRequestTableInfoSize = SizeOf(TRequestTableInfo);
  CtDownTaskParamSize = SizeOf(TDownTaskParam);

implementation

const
  CtContiguousCount = 32; //连接保存指定次数，则更改长度

{ TRequestBlockManage }

function TRequestBlockManage.AddRequestBlock(const ASegIndex, ABlockIndex: Integer): Boolean;
var
  p: PRequestTableInfo;
begin
  p := @FTables[FCurTableIndex];
  Result := p^.FCurRequestBlockCount + 1 <= FBlockCount;
  if Result then
  begin
    p^.FBlockTables[p^.FCurRequestBlockCount] := CalcID( ASegIndex, ABlockIndex );
    Inc( p^.FCurRequestBlockCount );
  end;
end;

function TRequestBlockManage.BeginRequest: Integer;
var
  p: PRequestTableInfo;
  dwTime: Cardinal;
begin
  FCurTableIndex := (FCurTableIndex + 1) mod FTableCount;
  p := @FTables[FCurTableIndex];
  Result := p^.FCurRespondBlockCount + 1;
  if Result > FBlockCount then
    Result := FBlockCount; 

  if AutoChangedBlockCount then
  begin
    if Result = FBlockCount then
    begin
      FLessThanHalfContiguousCount := 0;
      Inc( FMaxContiguousCount );
      if FMaxContiguousCount >= CtContiguousCount then
      begin
        ReBuildBlockTable( FBlockCount * 2 ); 
        Result := FBlockCount + 1;
      end;  
    end
    else
    begin
      FMaxContiguousCount := 0;
      if Result < FBlockCount div 2 then
      begin
        Inc( FLessThanHalfContiguousCount );
        if FLessThanHalfContiguousCount >= CtContiguousCount * 2 then
        begin
          ReBuildBlockTable( FBlockCount div 2 );
          if Result > FBlockCount then
            Result := FBlockCount;
        end;
      end;
    end;
  end;
  
  FillChar( p^.FBlockTables[0], FBlockCount * 4, 0 );
  p^.FCurRequestBlockCount := 0;
  p^.FCurRespondBlockCount := 0;
  dwTime := GetTickCount;
  if (dwTime - FBeginRequestTime > 0) and (FCurSize <> 0) then
    FCurSpeed := FCurSize div (dwTime - FBeginRequestTime);
  FBeginRequestTime := dwTime;
  FCurSize := 0;
end;

function TRequestBlockManage.CalcID(const ASegIndex, ABlockIndex: Integer): Cardinal;
begin
  Result := ASegIndex * 10000 + ABlockIndex;
end;

constructor TRequestBlockManage.Create(const ATableCount: Integer; const AInitBlockCount: Integer);
var
  i: Integer;
begin  
  if AInitBlockCount <= 0 then
    FBlockCount := 32 //默认情况
  else
    FBlockCount := AInitBlockCount;
  AutoChangedBlockCount := True;
  if ATableCount >= 1 then
    FTableCount := ATableCount
  else
    FTableCount := 2;
  SetLength( FTables, FTableCount );
  for i := 0 to FTableCount - 1 do    
  begin
    SetLength( FTables[i].FBlockTables, FBlockCount );
    FillChar( FTables[i].FBlockTables[0], FBlockCount * 4, 0 );
    FTables[i].FCurRequestBlockCount := 0;
    FTables[i].FCurRespondBlockCount := 0;
  end;
  FCurTableIndex := -1;
  FMaxContiguousCount := 0;
  FLessThanHalfContiguousCount := 0;
  FBeginRequestTime := 0;
  FCurSize := 0;
  FCurSpeed := 0;
end;

destructor TRequestBlockManage.Destroy;
var
  i: Integer;
begin
  for i := 0 to FTableCount - 1 do
    SetLength( FTables[i].FBlockTables, 0 );
  SetLength( FTables, 0 );
  inherited;
end;

procedure TRequestBlockManage.FinishedRequestBlock(const ASegIndex, ABlockIndex: Integer; const ARecvByteCount: Cardinal);
var
  i, j: Integer;
  p: PRequestTableInfo;
  nID: Cardinal;
begin  
  Inc( FRecvByteCount, ARecvByteCount );
  FCurSize := FCurSize + ARecvByteCount;
  nID := CalcID( ASegIndex, ABlockIndex );
  for i := 0 to FTableCount - 1 do
  begin
    p := @FTables[i];
    for j := 0 to p^.FCurRequestBlockCount - 1 do
    begin
      if p^.FBlockTables[j] = nID then
      begin
        Inc( p^.FCurRespondBlockCount );        
        Exit;
      end;
    end;
  end;
end;

procedure TRequestBlockManage.ReBuildBlockTable(const ANewBlockCount: Integer);
var
  i: Integer;
begin
  FMaxContiguousCount := 0;
  FLessThanHalfContiguousCount := 0;
  for i := 0 to FTableCount - 1 do
  begin
    SetLength( FTables[i].FBlockTables, ANewBlockCount );
    if FTables[i].FCurRequestBlockCount > ANewBlockCount then
      FTables[i].FCurRequestBlockCount := ANewBlockCount;
    if FTables[i].FCurRespondBlockCount > ANewBlockCount then
      FTables[i].FCurRespondBlockCount := ANewBlockCount;
  end;
  FBlockCount := ANewBlockCount;
  OutputDebugString( PChar('更改大小: ' + IntToStr(ANewBlockCount)) );
end;

end.
