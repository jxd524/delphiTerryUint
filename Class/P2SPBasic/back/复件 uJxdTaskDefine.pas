{
TRequestBlockManage: 
  非安全模式，需要外部提供锁定操作。
  滑动窗口，自己适应请求的大小
}
unit uJxdTaskDefine;

interface

uses
  Windows, SysUtils, uJxdUdpDefine, uJxdFileSegmentStream;

const
  CtMaxRequestBlockCount = 64; //请求表最大数量
  
type
  //请求分块管理器, 每次请求数据时重置。实际是一个不定长的滑动窗口
  PRequestTableInfo = ^TRequestTableInfo;
  TRequestTableInfo = record
    FBlockTables: array[0..CtMaxRequestBlockCount - 1] of Cardinal; //记录每次请求小分块的信息
    FCurRequestBlockCount: Integer; //当前请求分块的数量
    FCurRespondBlockCount: Integer; //当前响应分场的数量    
  end;
  TRequestBlockManage = class
  public
    constructor Create(const ATableCount: Integer = 2 );
    destructor  Destroy; override;
    
    //请求开始
    function  BeginRequest: Integer; //开始请求分块, 返回可请求数量
    function  AddRequestBlock(const ASegIndex, ABlockIndex: Integer): Boolean; //添加请求,记录请求信息    
    procedure FinishedRequestBlock(const ASegIndex, ABlockIndex: Integer); //完成请求
  private
    FTables: array of TRequestTableInfo;
    FTableCount: Integer;
    FCurTableIndex: Integer;
    function  CalcID(const ASegIndex, ABlockIndex: Integer): Cardinal; inline;
  end;
  
  {P2P源信息结构}
  TSourceState = (ssUnkown, ssChecking, ssSuccess);
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
    FTotalRecvByteCount: Integer; //总共接收到的数据长度
  end;
  
  {Http源信息结构}
  PHttpSourceInfo = ^THttpSourceInfo;
  THttpSourceInfo = record
    FUrl: string;
    FReferUrl: string;
    FCookies: string;
    FTotalRecvByteCount: Integer;
  end;
  
  //等待查询可用性的P2P数据源结构体
  PCheckSourceInfo = ^TCheckSourceInfo;
  TCheckSourceInfo = record
    FUserID: Cardinal;    
    FIP: Cardinal;
    FPort: Word;
    FCheckState: TConnectState; 
    FLastActiveTime: Cardinal;   
  end;
  THashThreadState = (htsNULL, htsRunning, htsFinished);

const
  CtP2PSourceInfoSize = SizeOf(TP2PSourceInfo);
  CtHttpSourceInfoSize = SizeOf(THttpSourceInfo);
  CtRequestTableInfoSize = SizeOf(TRequestTableInfo);

implementation

{ TRequestBlockManage }

function TRequestBlockManage.AddRequestBlock(const ASegIndex, ABlockIndex: Integer): Boolean;
var
  p: PRequestTableInfo;
begin
  p := @FTables[FCurTableIndex];
  Result := p^.FCurRequestBlockCount + 1 <= CtMaxRequestBlockCount;
  if Result then
  begin
    p^.FBlockTables[p^.FCurRequestBlockCount] := CalcID( ASegIndex, ABlockIndex );
    Inc( p^.FCurRequestBlockCount );
  end;
end;

function TRequestBlockManage.BeginRequest: Integer;
var
  p: PRequestTableInfo;
begin
  FCurTableIndex := (FCurTableIndex + 1) mod FTableCount;
  p := @FTables[FCurTableIndex];
  Result := p^.FCurRespondBlockCount + 1;
  if Result > CtMaxRequestBlockCount then
    Result := CtMaxRequestBlockCount;   
  FillChar( p^, CtRequestTableInfoSize, 0 );
  
  if Result = CtMaxRequestBlockCount then  
  OutputDebugString( PChar('当前请求最大为：' + InttoStr(Result)) );
end;

function TRequestBlockManage.CalcID(const ASegIndex, ABlockIndex: Integer): Cardinal;
begin
  Result := ASegIndex * 10000 + ABlockIndex;
end;

constructor TRequestBlockManage.Create(const ATableCount: Integer);
var
  i: Integer;
begin  
  if CtMaxRequestBlockCount > CtMaxRequestFileBlockCount then
    raise Exception.Create( 'CtMaxRequestBlockCount > CtMaxRequestFileBlockCount' );
  
  if ATableCount >= 1 then
    FTableCount := ATableCount
  else
    FTableCount := 2;
  SetLength( FTables, FTableCount );
  for i := 0 to FTableCount - 1 do    
    FillChar( FTables[i], CtRequestTableInfoSize, 0 );
  FCurTableIndex := -1;
end;

destructor TRequestBlockManage.Destroy;
begin
  SetLength( FTables, 0 );
  inherited;
end;

procedure TRequestBlockManage.FinishedRequestBlock(const ASegIndex, ABlockIndex: Integer);
var
  i, j: Integer;
  p: PRequestTableInfo;
  nID: Cardinal;
begin  
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

end.
