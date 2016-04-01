{
单元名称: uUdpClient
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: UDP包的处理类.
开始时间: 2009-2-2
修改时间: 2009-2-4 (最后修改)
作    用: 处理UDP客户端,从 TUdpIOHandle 继承.添加同步发送机制, 利用流水号与时间戳
          对父类数据缓冲进行一般化合理初始化
引用单元:
          uUdpIOHandle
}

unit uUdpClient;

interface
uses uUdpIOHandle, Classes, Windows, uConversion;

type
  TReplyState = (rsWaitting, rsSuccess, rsLengthTooSmall);
  pSyncData = ^TSyncData; //同步数据
  TSyncData = record
    FTimeStamp: Cardinal;
    FSerialNum: Word;
    FReplyOK: TReplyState; //0: 等待中; 1: 成功; 2: FBufferLen提供的长度不够; 3: 找不到信息
    FBufferLen: Cardinal;
    FBuffer: Pointer;
  end;

  TSyncDataList = class
  private
    FSyncLock: TRTLCriticalSection;
    FSyncDataList: TList;
    function  FindSyncData(const ATimeStamp: Cardinal; const ASerialNum: Word): Integer;
  public
    procedure AddSyncData(const ATimeStamp: Cardinal; const ASerialNum: Word; const ABufferLen: Cardinal; const AWaitSyncBuffer: Pointer);
    function  ModifySyncData(const ATimeStamp: Cardinal; const ASerialNum: Word; const ABufferLen: Cardinal; const ABuffer: Pointer): Boolean;
    function  IsSyncDataOK(const ATimeStamp: Cardinal; const ASerialNum: Word; const AForceDelete: Boolean): Integer; //-1: rsWaitting; -2: rsLengthTooSmall; -3无找到信息; > 0: 成功

    constructor Create;
    destructor Destroy; override;
  end;

  TOnHandleAsynBuffer = procedure(Sender: TObject; ARecvInfo: TRecvInfo) of object;
  TUdpClient = class(TUdpIOHandle)
  private
    FOnHandleAsynBuffer: TOnHandleAsynBuffer;
    procedure SetUserID(const Value: Cardinal);
  protected
    FSyncDataList: TSyncDataList;
    function  DoBeforOpenUDP: Boolean; override;  //初始化UDP前; True: 允许初始化; False: 不允许初始化
    procedure DoAfterCloseUDP; override; //UDP关闭之后
    procedure OnRecvBuffer(ARecvInfo: TRecvInfo); override; //同步, 异步数据在此分离
    procedure DoHandleAnsyBuffer(ARecvInfo: TRecvInfo); virtual;
  published //异步数据处理
  public
    //发送同步包
    function  SendBufferBySync(AIP: Cardinal; AHostShortPort: word; ASendBuf, AReplyBuf: Pointer; const ASendBufLen: Integer;
                               var AReplyBufLen: Integer; ASerialNum: Word = 0; AWaitTime: Integer = 5000): Boolean;

    constructor Create; override;
    destructor Destroy; override;

    property OnHandleAsynBuffer: TOnHandleAsynBuffer read FOnHandleAsynBuffer write FOnHandleAsynBuffer;
    property UserID: Cardinal read FUserID write SetUserID;
    property CurSerialNum: Word read FCurSerialNum;
  end;


implementation

{ TSyncDataList }

procedure TSyncDataList.AddSyncData(const ATimeStamp: Cardinal; const ASerialNum: Word; const ABufferLen: Cardinal; const AWaitSyncBuffer: Pointer);
var
  p: pSyncData;
begin
  New( p );
  p^.FTimeStamp := ATimeStamp;
  p^.FSerialNum := ASerialNum;
  p^.FBufferLen := ABufferLen;
  p^.FBuffer := AWaitSyncBuffer;
  p^.FReplyOK := rsWaitting;
  EnterCriticalSection( FSyncLock );
  try
    FSyncDataList.Add( p );
  finally
    LeaveCriticalSection( FSyncLock );
  end;
end;

constructor TSyncDataList.Create;
begin
  FSyncDataList := TList.Create;
  InitializeCriticalSection( FSyncLock );
end;

destructor TSyncDataList.Destroy;
var
  i: Integer;
begin
  EnterCriticalSection( FSyncLock );
  try
    for i := FSyncDataList.Count - 1 downto 0 do
      Dispose( FSyncDataList[i] );
  finally
    LeaveCriticalSection( FSyncLock );
  end;
  FSyncDataList.Free;
  DeleteCriticalSection( FSyncLock );
  inherited;
end;

//-1: rsWaitting; -2: rsLengthTooSmall; -3无找到信息; > 0: 成功
function TSyncDataList.IsSyncDataOK(const ATimeStamp: Cardinal; const ASerialNum: Word; const AForceDelete: Boolean): Integer;
var
  nIndex: Integer;
  p: pSyncData;
begin
  Result := -3;
  EnterCriticalSection( FSyncLock );
  try
    nIndex := FindSyncData( ATimeStamp, ASerialNum );
    if nIndex = -1 then Exit;
    p := FSyncDataList[nIndex];
    if p^.FReplyOK = rsSuccess then
    begin
      Result := p^.FBufferLen;
      FSyncDataList.Delete( nIndex );
      Dispose(p);
    end
    else if p^.FReplyOK = rsWaitting then
      Result := -1
    else if p^.FReplyOK = rsLengthTooSmall then
      Result := -2;

    if (p^.FReplyOK = rsLengthTooSmall) or AForceDelete then
    begin
      FSyncDataList.Delete( nIndex );
      Dispose(p);
    end;
  finally
    LeaveCriticalSection( FSyncLock );
  end;
end;


function TSyncDataList.ModifySyncData(const ATimeStamp: Cardinal; const ASerialNum: Word; const ABufferLen: Cardinal; const ABuffer: Pointer): Boolean;
var
  nIndex: Integer;
  p: pSyncData;
begin
  EnterCriticalSection( FSyncLock );
  try
    nIndex := FindSyncData( ATimeStamp, ASerialNum );
    Result := nIndex <> -1;
    if not Result then Exit;
    p := FSyncDataList[nIndex];
    if p^.FBufferLen < ABufferLen then
    begin
      p^.FReplyOK := rsLengthTooSmall;
      Exit;
    end;
    p^.FBufferLen := ABufferLen;
    Move( ABuffer^, p^.FBuffer^, ABufferLen );
    p^.FReplyOK := rsSuccess;
  finally
    LeaveCriticalSection( FSyncLock );
  end;
end;

function TSyncDataList.FindSyncData(const ATimeStamp: Cardinal; const ASerialNum: Word): Integer;
var
  i: Integer;
  p: pSyncData;
begin
  Result := -1;
  for i := 0 to FSyncDataList.Count - 1 do
  begin
    p := FSyncDataList[i];
    if (p^.FSerialNum = ASerialNum) and (p^.FTimeStamp = ATimeStamp) then
    begin
      Result := i;
      Break;
    end;
  end;
end;
{ TUdpClient }

constructor TUdpClient.Create;
begin
  inherited;
  IsBind := True;
  IsExclusitve := True;

  SinglePackageThreadCount := 1;
  SinglePackageMaxNodeCount := 64;
  SinglePackageQuerySpaceTime := 100;

  MulitPackageEnable := True;
  MulitPackageHashTableCount := 521;
  MulitPackageMaxHashNodeCount := 64;
  MulitPackageThreadCount := 0;
  MulitPackageQuerySpaceTime :=100;
  MulitPackageCheckThread := True;
  MulitPackageCheckSpaceTime := 60 * 1000;
  MulitPackageMaxWaitTime := 5000;
end;

destructor TUdpClient.Destroy;
begin

  inherited;
end;

procedure TUdpClient.DoAfterCloseUDP;
begin
  inherited DoAfterCloseUDP;
  FSyncDataList.Free;
end;

function TUdpClient.DoBeforOpenUDP: Boolean;
begin
  Result := inherited DoBeforOpenUDP;
  if Result then
    FSyncDataList := TSyncDataList.Create;
end;

procedure TUdpClient.DoHandleAnsyBuffer(ARecvInfo: TRecvInfo);
begin
  if Assigned(OnHandleAsynBuffer) then
    OnHandleAsynBuffer( Self, ARecvInfo );
end;

procedure TUdpClient.OnRecvBuffer(ARecvInfo: TRecvInfo);
begin
  if not Boolean(ARecvInfo.FIsAsynch) then
  begin //同步包
    if FSyncDataList.ModifySyncData( ARecvInfo.FTimeStamp, ARecvInfo.FSerialNum, ARecvInfo.FBufferLength, ARecvInfo.FBuffer ) then
      Exit;
  end;
  DoHandleAnsyBuffer( ARecvInfo );
end;

function TUdpClient.SendBufferBySync(AIP: Cardinal; AHostShortPort: word; ASendBuf, AReplyBuf: Pointer; const ASendBufLen: Integer;
  var AReplyBufLen: Integer; ASerialNum: Word; AWaitTime: Integer): Boolean;
var
  nSleepTime: Integer;
  nTimeStamp: Cardinal;
begin
  nTimeStamp := GetTimeStamp + Cardinal( Random(MaxInt) );
  if ASerialNum = 0 then
    ASerialNum := GetSerialNum;
  FSyncDataList.AddSyncData( nTimeStamp, ASerialNum, AReplyBufLen, Pointer(AReplyBuf) );
  Result := inherited SendBufferBySync( AIP, AHostShortPort, ASendBuf, ASendBufLen, ASerialNum, nTimeStamp ) = ASendBufLen;
  if not Result then Exit;
  nSleepTime := AWaitTime div 10 + 1;
  repeat
    Sleep( nSleepTime );
    Dec( AWaitTime, nSleepTime );
    AReplyBufLen := FSyncDataList.IsSyncDataOK( nTimeStamp, ASerialNum, AWaitTime < 0 ); //-1: rsWaitting; -2: rsLengthTooSmall; -3无找到信息; > 0: 成功

    if AReplyBufLen = -2 then
    begin
      DoErrorInfo( '同步命令提供接收内存长度过长' );
      Break;
    end
    else if AReplyBufLen = -3 then
    begin
      DoErrorInfo( '同步命令列表节点丢失' );
      Break;
    end;

  until ( (AReplyBufLen >= 0) or (AWaitTime <= 0) );
  Result := AReplyBufLen >= 0;
end;

procedure TUdpClient.SetUserID(const Value: Cardinal);
begin
  if FUserID <> Value then
    FUserID := Value;
end;

Initialization
  Randomize;

end.
