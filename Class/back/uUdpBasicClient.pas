{
单元名称: uUdpBasicClient
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明:
开始时间: 2009-4-26
修改时间: 2009-4-26 (最后修改时间)
类说明  :
          从TUdpIoHandle承继,
          添加同步发送数据的方法
          添加代理方法
}
unit uUdpBasicClient;

interface
uses
  uUdpIOHandle, Windows, SysUtils, uDataStructure;

type
  { ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                    TSyncDataHandle
    同步数据处理类

  }
  TSyncState = (ssWaitting, ssSuccess, ssLenToSmall, ssError);
  pSyncData = ^TSyncData;
  TSyncData = record
    FSyncState: TSyncState;
    FNotifyEvent: THandle; //由外部传入，此类不创建
    FBuffer: Pointer;
    FBufferLen: Integer;
  end;
  TSyncDataHandle = class
  private
    FSyncDataHash: array[0..CtMaxSyncPackageCount - 1] of TSyncData;
    FTempSyncData: array[0..CtMaxSyncPackageCount - 1] of pSyncData;
    procedure FreeSyncData(var pSync: pSyncData); overload;
    procedure AddTempSyncData(const ASyncSign: Byte; const ABuffer: Pointer; const ABufLen: Integer);
  public
    function  AddSyncData(const ASyncSign: Byte; const AEventHandle: THandle; const ABufToWrite: Pointer; const ABufLen: Integer): Boolean;
    procedure ModifySyncData(const ASyncSign: Byte; const ABuffer: Pointer; const ABufLen: Integer);
    function  GetSyncState(const ASyncSign: Byte): TSyncState;
    procedure FreeSyncData(const ASyncSign: Byte); overload;
    function  GetNewSyncData(const ASyncSign: Byte; var ACallBackBuf: Pointer; var ABufLength: Integer): Boolean;

    constructor Create;
    destructor Destroy; override;
  end;
  { ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
                                                    TUdpClient
    添加同步处理包
    添加支持代理
  }
  TUdpBasicClient = class(TUdpIoHandle)
  private
    FSyncIndexManage: TByteIndexManage;
    FSyncDataHandle: TSyncDataHandle;
    function GetSyncSignUsercount: Byte;
  protected
    function  DoBeforOpenUDP: Boolean; override;
    procedure DoAfterCloseUDP; override;
    procedure OnRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
                           const ASyncSign: Byte); override;
    procedure OnRecvSyncBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal; const ASyncSign: Byte); virtual;
    procedure OnRecvAsynBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal); virtual;
  public
    //同步包发送函数, 只能发给服务器端的同步发包函数
    function  SendSyncBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
                             var ACallBackBuffer: pAnsiChar; var ACallBackBufLen: Integer;
                             const ATryCount: Byte = 3; const AMaxWaitTime: Cardinal = 5 * 1000): TSyncState;
    constructor Create; 
    destructor  Destroy; override;

    property SyncSignUsefulCount: Byte read GetSyncSignUserCount; //同步包标志可使用数量
  end;

implementation
const
  CtSyncDataSize = SizeOf(TSyncData);

{ TSyncDataHandle }

function TSyncDataHandle.AddSyncData(const ASyncSign: Byte; const AEventHandle: THandle; const ABufToWrite: Pointer; const ABufLen: Integer): Boolean;
begin
  if ASyncSign < CtMaxSyncPackageCount then
  begin
    Result := FSyncDataHash[ASyncSign].FBuffer = nil;
    if Result then
    begin
      FSyncDataHash[ASyncSign].FSyncState := ssWaitting;
      FSyncDataHash[ASyncSign].FNotifyEvent := AEventHandle;
      FSyncDataHash[ASyncSign].FBuffer := ABufToWrite;
      FSyncDataHash[ASyncSign].FBufferLen := ABufLen;
    end;
  end
  else
    Result := False;
end;

procedure TSyncDataHandle.ModifySyncData(const ASyncSign: Byte; const ABuffer: Pointer; const ABufLen: Integer);
begin
  if ASyncSign < CtMaxSyncPackageCount then
  begin
    if FSyncDataHash[ASyncSign].FBuffer <> nil then
    begin
      if ABufLen <= FSyncDataHash[ASyncSign].FBufferLen then
      begin
        Move( ABuffer^, FSyncDataHash[ASyncSign].FBuffer^, ABufLen );
        FSyncDataHash[ASyncSign].FBufferLen := ABufLen;
        FSyncDataHash[ASyncSign].FSyncState := ssSuccess;
      end
      else
      begin
        AddTempSyncData( ASyncSign, ABuffer, ABufLen );
        FSyncDataHash[ASyncSign].FBufferLen := ABufLen;
        FSyncDataHash[ASyncSign].FSyncState := ssLenToSmall;
      end;
      SetEvent( FSyncDataHash[ASyncSign].FNotifyEvent );
    end;
  end
  else
    OutputDebugString( PChar('ModifySyncData 失败：ASyncSign := ' + IntToStr(ASyncSign)) );
end;

procedure TSyncDataHandle.AddTempSyncData(const ASyncSign: Byte; const ABuffer: Pointer; const ABufLen: Integer);
var
  p: pSyncData;
begin
  p := FTempSyncData[AsyncSign];
  FreeSyncData( p );
  New( p );
  p^.FSyncState  := ssSuccess;
  p^.FNotifyEvent := 0;
  p^.FBufferLen := ABufLen;
  GetMem( p^.FBuffer, p^.FBufferLen );
  Move( ABuffer^, p^.FBuffer^, p^.FBufferLen );
  FTempSyncData[AsyncSign] := p;
end;

constructor TSyncDataHandle.Create;
begin
  FillChar( FSyncDataHash[0], SizeOf(FSyncDataHash[0]) * CtMaxSyncPackageCount, 0 );
  FillChar( FTempSyncData[0], SizeOf(FTempSyncData[0]) * CtMaxSyncPackageCount, 0 );
end;

destructor TSyncDataHandle.Destroy;
begin

  inherited;
end;

procedure TSyncDataHandle.FreeSyncData(const ASyncSign: Byte);
var
  p: pSyncData;
begin
  if ASyncSign > CtMaxSyncPackageCount then Exit;
  FillChar( FSyncDataHash[AsyncSign], CtSyncDataSize, 0 );
  p := FTempSyncData[ASyncSign];
  FreeSyncData( p );
  FTempSyncData[ASyncSign] := nil;
end;

procedure TSyncDataHandle.FreeSyncData(var pSync: pSyncData);
begin
  if pSync <> nil then
  begin
    if pSync^.FBuffer <> nil then
      FreeMem( pSync^.FBuffer );
    Dispose( pSync );
    pSync := nil;
  end;
end;

function TSyncDataHandle.GetNewSyncData(const ASyncSign: Byte; var ACallBackBuf: Pointer; var ABufLength: Integer): Boolean;
var
  p: pSyncData;
begin
  Result := False;
  if ASyncSign > CtMaxSyncPackageCount then Exit;
  p := FTempSyncData[ASyncSign];
  if p = nil then Exit;
  Move( p^.FBuffer^, ACallBackBuf^, p^.FBufferLen );
  ABufLength := p^.FBufferLen;
end;

function TSyncDataHandle.GetSyncState(const ASyncSign: Byte): TSyncState;
var
  SyncData: TSyncData;
begin
  Result := ssError;
  if ASyncSign > CtMaxSyncPackageCount then Exit;
  SyncData := FSyncDataHash[AsyncSign];
  Result := SyncData.FSyncState;
end;

{ TUdpClient }

constructor TUdpBasicClient.Create;
begin
  inherited;

end;

destructor TUdpBasicClient.Destroy;
begin

  inherited;
end;

procedure TUdpBasicClient.DoAfterCloseUDP;
begin
  inherited;
  FreeAndNil( FSyncIndexManage );
  FreeAndNil( FSyncDataHandle );
end;

function TUdpBasicClient.DoBeforOpenUDP: Boolean;
begin
  Result := inherited DoBeforOpenUDP;
  if Result then
  begin
    try
      FSyncIndexManage := TByteIndexManage.Create;
      FSyncDataHandle := TSyncDataHandle.Create;
    except
      Result := False;
    end;
  end;
end;

function TUdpBasicClient.GetSyncSignUsercount: Byte;
begin
  if Assigned(FSyncIndexManage) then
    Result := FSyncIndexManage.Count
  else
    Result := 0;
end;

procedure TUdpBasicClient.OnRecvAsynBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
begin

end;

procedure TUdpBasicClient.OnRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal; const ASyncSign: Byte);
var
  bIsSyncPackage: Boolean;
  nSyncSign: Byte;
begin
  ParseSyncSign( ASyncSign, bIsSyncPackage, nSyncSign );
  if bIsSyncPackage then
    OnRecvSyncBuffer( AIP, APort, ABuffer, ABufLen, nSyncSign )
  else
    OnRecvAsynBuffer( AIP, APort, ABuffer, ABufLen );
end;

procedure TUdpBasicClient.OnRecvSyncBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  const ASyncSign: Byte);
begin
  FSyncDataHandle.ModifySyncData( ASyncSign, ABuffer, ABufLen );
end;

function TUdpBasicClient.SendSyncBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
  var ACallBackBuffer: pAnsiChar; var ACallBackBufLen: Integer; const ATryCount: Byte; const AMaxWaitTime: Cardinal): TSyncState;
var
  pSendBuffer: PAnsiChar;
  nSyncSign: Byte;
  WaitEvent: THandle;
  i: Integer;
  WaitResult: Cardinal;
  nState: TSyncState;
  nSendByteCount: Integer;
begin
  Result := ssError;
  if not FSyncIndexManage.GetIndex( nSyncSign ) then
  begin
    DoErrorInfo( '无法获取同步包序号，请稍等' );
    Exit;
  end;

  WaitEvent := CreateEvent( nil, False, False, nil );
  if WaitEvent = 0 then
  begin
    DoErrorInfo( '无法创建等待事件，请重试' );
    FSyncIndexManage.ReclaimIndex( nSyncSign );
    Exit;
  end;

  if not FSyncDataHandle.AddSyncData( nSyncSign, WaitEvent, ACallBackBuffer, ACallBackBufLen ) then
  begin
    CloseHandle( WaitEvent );
    FSyncIndexManage.ReclaimIndex( nSyncSign );
    DoErrorInfo( '无法加入异步等待对象，请重试' );
    Exit;
  end;

  GetMem( pSendBuffer, ABufLen );
  try
    for i := 0 to ATryCount - 1 do
    begin
      Move( ABuffer^, pSendBuffer^, ABufLen );
      nSendByteCount := _SendBuffer( AIP, APort, pSendBuffer, ABufLen, FormatSyncSign( True, nSyncSign ) );
      if nSendByteCount <> Integer(ABufLen) then Exit;

      WaitResult := WaitForSingleObject( WaitEvent, AMaxWaitTime );
      if WAIT_OBJECT_0 = WaitResult then
      begin
        nState := FSyncDataHandle.GetSyncState( nSyncSign );
        case nState of
          ssWaitting:
          begin
            DoErrorInfo( '无法找到返回的信息!, 将重新发信息信息' );
            Continue;
          end;
          ssSuccess:
          begin
            Result := ssSuccess;
            Break;
          end;
          ssLenToSmall:
          begin
            Result := ssLenToSmall;
            DoErrorInfo( '提供接收的内存过小，不足以接收数据' );
            DoErrorInfo( '数据已经存放到 TSyncData 类中，可以调用: FSyncDataHandle.GetNewSyncData 获取数据 暂时还没有处理' );
            Break;
          end;
          ssError:
          begin
            DoErrorInfo( '同步命令发现末知错误!' );
            Break;
          end;
        end;
      end;
    end;
  finally
    FSyncIndexManage.ReclaimIndex( nSyncSign );
    FSyncDataHandle.FreeSyncData( nSyncSign );
    CloseHandle( WaitEvent );
    FreeMem( pSendBuffer, ABufLen );
  end;
end;

end.
