unit uJxdUdpSynchroClient;

interface

uses
  Windows, SysUtils, Forms,
  uJxdUdpIoHandle, uJxdUdpSynchroBasic, uJxdDataStruct, uJxdDataStream;

type
  TSynchroResult = (srSuccess{成功}, srSystemBuffer{系统动态申请内存，外部要调用ReleaseSynchroRecvBuffer},
                    srNoEnoughSynchroID{同步ID不够用}, srFail{失败});
  //同步信息， 当接收到同步命令时，需要去判断，当IP，PORT，SynchroID三者相同时，才为同一同步命令
  PSynchroPackageInfo = ^TSynchroPackageInfo;
  TSynchroPackageInfo = record
    FSynchroID: Word;              //同步ID
    FSendPort: Word;               //发送端口
    FSendIP: Cardinal;             //发送IP
    FSendBufLen: Integer;          //需要发送数据的长度
    FSendCount: Integer;           //已经发送的次数
    FRecvLen: Integer;             //外部指定用于接收数据的长度
    FRecvLenEx: Integer;           //系统动态申请的内存大小
    FRecvFinished: Boolean;        //指定是否已经接收到指定同步包
    FSendBuffer: PAnsiChar;        //需要发送的数据  动态申请，已经包含同步命令
    FRecvBuffer: PAnsiChar;        //接收到的数据 可由处部指定，当指定的内存不足接收数据时，由系统动态申请
    FRecvBufferEx: PAnsiChar;      //系统动态申请
  end;

  TxdUdpSynchroClient = class(TxdUdpSynchroBasic)
  public
    constructor Create; override;
    //释放同步包返回的内存
    procedure ReleaseSynchroRecvBuffer(ARecvBuffer: PAnsiChar);

    //发送同步包，
    //ASendBuffer数据包最前面必须预留四个字节.
    //ASendBuffer包含预留的四个字节和需要发送的数据长度
    //ARecvBuffer: 接收缓存，可以指定，或者由系统动态申请。
    //ARecvLen: 配合ARecvBuffer使用
    //AllowReSendCount: 最多允许重发次数
    //ATimeoutSpace: 每次发送后，等待时间超过此设定，则表示超过, 单位：毫秒
    //AProMsg: 在等待中，是否调用Application.ProccessMessage.如果是主线程，可为True. 单独线程设置为False
    function SendSynchroBuffer(const AIP: Cardinal; const APort: Word; const ASendBuffer: PAnsiChar;
      const ASendBufLen: Integer; var ARecvBuffer: PAnsiChar; var ARecvLen: Integer; AProMsg: Boolean = True;
      AllowReSendCount: Integer = 3; const ATimeoutSpace: Cardinal = 2000): TSynchroResult;
  protected
    function  DoBeforOpenUDP: Boolean; override;  //初始化UDP前; True: 允许初始化; False: 不允许初始化
    procedure DoAfterCloseUDP; override; //UDP关闭之后
    procedure OnCommonRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); override;

    {子类只需要处理此虚函数，有效的同步命令已经被处理了, 如果此时还有同步命令，则可能是另一客户端发过来的}
    procedure DoHandleRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); virtual;
  private
    FSynchroIdManage: TWordIdManage;
    FSynchroLock: TRTLCriticalSection;
    FSynchroList: THashArrayEx;
    procedure ClearSynchroList;
  private
    FSynchroHashTableCount: Integer;
    FSynchroHashNodeCount: Integer;
    FReSendSynchroPackageCount: Integer;
    FReRecvsynchroPackageCount: Integer;
    FSendSynchroPackageFailCount: Integer;
    FDropRecvOverdueSynchroPackageCount: Integer;
    procedure SetSynchroHashTableCount(const Value: Integer);
    procedure SetSynchroHashNodeCount(const Value: Integer);
  published
    property ReRecvSynchroPackageCount: Integer read FReRecvsynchroPackageCount;//重复接收到相同同步包次数
    property ReSendSynchroPackageCount: Integer read FReSendSynchroPackageCount; //重发同步包次数
    property SendSynchroPackageFailCount: Integer read FSendSynchroPackageFailCount; //发送同步包失败次数
    property DropRecvOverdueSynchroPackageCount: Integer read FDropRecvOverdueSynchroPackageCount; //丢弃过期包数量
    property SynchroHashTableCount: Integer read FSynchroHashTableCount write SetSynchroHashTableCount;
    property SynchroHashNodeCount: Integer read FSynchroHashNodeCount write SetSynchroHashNodeCount; 
  end;

implementation

const
  CtSynchroPackageInfoSize = SizeOf( TSynchroPackageInfo );

{ TxdUdpSynchroClient }

procedure TxdUdpSynchroClient.ClearSynchroList;
begin

end;

constructor TxdUdpSynchroClient.Create;
begin
  inherited;
  FSynchroHashTableCount := 131;
  FSynchroHashNodeCount := 512;
end;

procedure TxdUdpSynchroClient.DoAfterCloseUDP;
begin
  inherited;
  ClearSynchroList;
  FreeAndNil( FSynchroList );
  FreeAndNil( FSynchroIdManage );
  DeleteCriticalSection( FSynchroLock );
end;

function TxdUdpSynchroClient.DoBeforOpenUDP: Boolean;
begin
  Result := inherited DoBeforOpenUDP;
  if Result then
  begin
    try
      InitializeCriticalSection( FSynchroLock );
      FSynchroIdManage := TWordIdManage.Create;
      FSynchroList := THashArrayEx.Create;
      with FSynchroList do
      begin
        HashTableCount := SynchroHashTableCount;
        MaxHashNodeCount := SynchroHashNodeCount;
        Active := True;
      end;
      FReSendSynchroPackageCount := 0;
      FReRecvsynchroPackageCount := 0;
      FSendSynchroPackageFailCount := 0;
      FDropRecvOverdueSynchroPackageCount := 0;
    except
      Result := False;
    end;
  end;
end;

procedure TxdUdpSynchroClient.DoHandleRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
begin

end;

procedure TxdUdpSynchroClient.OnCommonRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pNode: PHashNode;
  bFind: Boolean;
  pSynchro: PSynchroPackageInfo;
begin
  bFind := False;
  if AIsSynchroCmd then
  begin
    //处理需要的同步包
    pSynchro := nil;
    EnterCriticalSection( FSynchroLock );
    pNode := FSynchroList.FindBegin( ASynchroID );
    try
      while Assigned(pNode) do
      begin
        pSynchro := PSynchroPackageInfo( pNode^.NodeData );
        if Assigned(pSynchro) and (pSynchro^.FSynchroID = ASynchroID) and (pSynchro^.FSendIP = AIP)
          and (pSynchro^.FSendPort = APort) then
        begin
          bFind := True;
          Break;
        end;
        pNode := FSynchroList.FindNext( pNode );
      end;
      if bFind then
      begin
        if not pSynchro^.FRecvFinished then
        begin
          if Assigned(pSynchro^.FRecvBuffer) and (pSynchro^.FRecvLen >= ABufLen) then
          begin
            Move( ABuffer^, pSynchro^.FRecvBuffer^, ABufLen );
            pSynchro^.FRecvLen := ABufLen;
            pSynchro^.FRecvFinished := True;
          end
          else
          begin
            GetMem( pSynchro^.FRecvBufferEx, ABufLen );
            Move( ABuffer^, pSynchro^.FRecvBufferEx^, ABufLen );
            pSynchro^.FRecvLenEx := ABufLen;
            pSynchro^.FRecvFinished := True;
          end;
        end
        else
          InterlockedIncrement( FReRecvsynchroPackageCount );
      end;
    finally
      FSynchroList.FindEnd;
      LeaveCriticalSection( FSynchroLock );
    end;
  end;
  if not bFind then
    DoHandleRecvBuffer( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
end;

procedure TxdUdpSynchroClient.ReleaseSynchroRecvBuffer(ARecvBuffer: PAnsiChar);
begin
  if Assigned(ARecvBuffer) then
    FreeMem( ARecvBuffer );
end;

function TxdUdpSynchroClient.SendSynchroBuffer(const AIP: Cardinal; const APort: Word; const ASendBuffer: PAnsiChar; const ASendBufLen: Integer;
  var ARecvBuffer: PAnsiChar; var ARecvLen: Integer; AProMsg: Boolean; AllowReSendCount: Integer; const ATimeoutSpace: Cardinal): TSynchroResult;
var
  SynchroInfo: TSynchroPackageInfo;
  i, nCount: Integer;
  pNode: PHashNode;
  pSynchro: PSynchroPackageInfo;
  nTemp: Cardinal;
begin
  Result := srFail;
  FillChar( SynchroInfo, CtSynchroPackageInfoSize, 0 );
  //申请ID
  if not FSynchroIdManage.GetWordID(SynchroInfo.FSynchroID) then
  begin
    DoErrorInfo( '无法申请同步ID，不能进行同步包的发送' );
    Result := srNoEnoughSynchroID;
    Exit;
  end;

  nCount := AllowReSendCount;
  if nCount <= 0 then
    nCount := 3;

  //初始化参数
  SynchroInfo.FSendPort := APort;
  SynchroInfo.FSendIP := AIP;
  SynchroInfo.FRecvFinished := False;
  SynchroInfo.FSendBufLen := ASendBufLen;
  SynchroInfo.FSendBuffer := ASendBuffer;
  AddSynchroSign( SynchroInfo.FSendBuffer, SynchroInfo.FSynchroID );
  if Assigned(ARecvBuffer) and (ARecvLen > 0) then
  begin
    SynchroInfo.FRecvBuffer := ARecvBuffer;
    SynchroInfo.FRecvLen := ARecvLen;
  end;

 //添加到列表
  EnterCriticalSection( FSynchroLock );
  try
    if not FSynchroList.Add(SynchroInfo.FSynchroID, @SynchroInfo) then
    begin
      FSynchroIdManage.ReclaimWordID( SynchroInfo.FSynchroID );
      Result := srFail;
      DoErrorInfo( '无法添加到同步HASH表中' );
      Exit;
    end;
  finally
    LeaveCriticalSection( FSynchroLock );
  end;

  //开始发送
  for i := 0 to nCount - 1 do
  begin
    nTemp := GetTickCount;
    if SendBuffer(SynchroInfo.FSendIP, SynchroInfo.FSendPort, SynchroInfo.FSendBuffer, SynchroInfo.FSendBufLen) <> SynchroInfo.FSendBufLen then
    begin
      DoErrorInfo( '无法发送数据' );
      Break;
    end;
    //等待返回,不使用内核事件通知，只要简单的用询问方式就可
    while not SynchroInfo.FRecvFinished do
    begin
      Sleep(10);
      if AProMsg then
        Application.ProcessMessages;
      if SynchroInfo.FRecvFinished then Break; //完成，退出
      if GetTickCount - nTemp > ATimeoutSpace then Break; //超时，重发
    end;
    if SynchroInfo.FRecvFinished then
      Break
    else if i <> nCount - 1 then
      InterlockedIncrement( FReSendSynchroPackageCount );
  end;


  //从Hash表中删除
  EnterCriticalSection( FSynchroLock );
  pNode := FSynchroList.FindBegin( SynchroInfo.FSynchroID );
  try
    while Assigned(pNode) do
    begin
      pSynchro := PSynchroPackageInfo( pNode^.NodeData );
      if Assigned(pSynchro) and (pSynchro^.FSynchroID = SynchroInfo.FSynchroID) and (pSynchro^.FSendIP = AIP)
        and (pSynchro^.FSendPort = APort) then
      begin
        FSynchroList.FindDelete( pNode );
        Break;
      end;
      pNode := FSynchroList.FindNext( pNode );
    end;
  finally
    FSynchroList.FindEnd;
    LeaveCriticalSection( FSynchroLock );
  end;

  //释放ID
  FSynchroIdManage.ReclaimWordID( SynchroInfo.FSynchroID );

  //判断数据
  if SynchroInfo.FRecvFinished then
  begin
    //接收到数据
    if Assigned(SynchroInfo.FRecvBuffer) and (SynchroInfo.FRecvLen > 0) then
    begin
      ARecvBuffer := SynchroInfo.FRecvBuffer;
      ARecvLen := SynchroInfo.FRecvLen;
      Result := srSuccess
    end
    else
    begin
      ARecvBuffer := SynchroInfo.FRecvBufferEx;
      ARecvLen := SynchroInfo.FRecvLenEx;
      Result := srSystemBuffer;
    end;
  end
  else
    InterlockedIncrement( FSendSynchroPackageFailCount );
end;

procedure TxdUdpSynchroClient.SetSynchroHashNodeCount(const Value: Integer);
begin
  if not Active then
    FSynchroHashNodeCount := Value;
end;

procedure TxdUdpSynchroClient.SetSynchroHashTableCount(const Value: Integer);
begin
  if not Active then
    FSynchroHashTableCount := Value;
end;

end.
