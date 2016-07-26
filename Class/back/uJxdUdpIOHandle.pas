{
单元名称: uJxdUdpIOHandle
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: 对通信数据做一些处理
开始时间: 2010-09-01
修改时间: 2010-09-01 (最后修改)
功    能：
          只支持单包发送，对发送的数据做简单的处理，加入协议版本号，数据CRC32号等。
          不对包提供缓存，组合，同步等命令，如果需要这些，可以使用 uUdpIOHandle 单元，它相对来说比较复杂。


          协议：  版本(1字节) - 此包总长度(2) - CRC32码(4) - 要处理的数据(n) - 结束信息(1)
}
unit uJxdUdpIOHandle;

{$DEFINE DebugLog}

interface
uses
  Windows, SysUtils, uJxdBasicUDP, WinSock2, Encrypt, uSocketSub, Classes, uJxdDataStream
  {$IFDEF DebugLog}, uDebugInfo{$EndIf}
  ;

const
{广域网UDP限制}
  CtMTU = 1500;      //中国大部分路由MTU值; 避免IP分片
  CtRawIpHead = 20;  //TCP/IP协议中原始IP头
  CtRawUdpHead = 8;  //TCP/IP协议中原始UDP头
{本身使用常规常量}
  CtMaxUdpSize = CtMTU - CtRawIpHead - CtRawUdpHead; //最大发送包长度
  CtProtocalSize = 8;                                //协议需要占用的固定字节数
  CtMaxPackageSize = CtMaxUdpSize - CtProtocalSize;  //独立包最大数据区长度

type
  TxdUdpIOHandle = class( TxdBasicUDP )
  public
    constructor Create; override;
    destructor Destroy; override;
    {发送不超过 CtMaxPackageSize 大小的包，返回值为：实际发送数据长度 - CtProtocalSize }
    function  SendBuffer(const AIP: Cardinal; const AHostPort: Word; const ApBuffer: PAnsiChar; const ABufLen: Word): Integer;
  protected
    procedure DoRecvBuffer; override;
    function  DoBeforOpenUDP: Boolean; override;

    procedure DoErrorInfo(const AInfo: PAnsiChar); overload; override;
    procedure DoErrorInfo(const AInfo: string; const Args: array of const); overload;

    procedure OnRecvBuffer(const AIP: Cardinal; const APort: Word; CmdStream: TxdOuterMemory); virtual;
    procedure StatisticalData(const AIP: Cardinal; const APort: Word; const ARecvBufLen: Integer); virtual;
  private
    FRecvPackageCount: Integer;
    FSendPackageCount: Integer;
  published
    property SendPackageCount: Integer read FSendPackageCount;
    property RecvPackageCount: Integer read FRecvPackageCount; 
  end;

implementation

const
  CtProtocalVersion: Byte = $01;
  CtProtocalEnd: Byte     = $ED;

procedure Debug(const AInfo: string); overload;
begin
{$IFDEF DebugLog}
  _Log( AInfo, 'TxdUdpIOHandle_DebugInfo.txt' );
{$EndIf}
  OutputDebugString( PChar(AInfo) );
end;
procedure Debug(const AInfo: string; const Args: array of const); overload;
begin
  Debug( Format(AInfo, Args) );
end;

{ TxdUdpIOHandle }

constructor TxdUdpIOHandle.Create;
begin
  inherited;
  FSendPackageCount := 0;
end;

destructor TxdUdpIOHandle.Destroy;
begin

  inherited;
end;

function TxdUdpIOHandle.DoBeforOpenUDP: Boolean;
begin
  Result := True;
  FSendPackageCount := 0;
  FRecvPackageCount := 0;
end;

procedure TxdUdpIOHandle.DoErrorInfo(const AInfo: string; const Args: array of const);
begin
  DoErrorInfo( PChar(Format(AInfo, Args)) );
end;

procedure TxdUdpIOHandle.DoErrorInfo(const AInfo: PAnsiChar);
begin
  Debug( AInfo );
  inherited;
end;

procedure TxdUdpIOHandle.DoRecvBuffer;
var
  Package: array[0..CtMaxUdpSize - 1] of AnsiChar;
  nLen, nDataLen: Integer;
  addr: TSockAddrIn;
  nIP: Cardinal;
  nPort: Word;
  CmdStream: TxdOuterMemory;
  dPackageLen: Word;
  nCrc32Code: Cardinal;
  pData: PAnsiChar;
begin
  nLen := CtMaxUdpSize;
  if not __RecvBuffer( Package, nLen, addr ) then
  begin
    DoErrorInfo( WSAGetLastError, '__RecvBuffer' );
    Exit;
  end;
  InterlockedIncrement( FRecvPackageCount );
  
  nIP := addr.sin_addr.S_addr;
  nPort := ntohs(addr.sin_port);

  if nLen < CtProtocalSize then
  begin
    DoErrorInfo( 'Recv buffer len is too small, only %d byte, from: %s', [nLen, IpToStr(nIP, nPort)] );
    Exit;
  end;

  StatisticalData( nIP, nPort, nLen );
  
  CmdStream := TxdOuterMemory.Create;
  try
    with CmdStream do
    begin
      InitMemory( @Package, nLen );
      //版本(1字节) - 此包总长度(2) - CRC32码(4) - 要处理的数据(n) - 结束信息(1)
      if ReadByte <> CtProtocalVersion then
      begin
        DoErrorInfo( 'protocal version number error.(first byte error)' );
        Exit;
      end;
      dPackageLen := ntohs( ReadWord );
      if nLen <> Integer(dPackageLen) then
      begin
        DoErrorInfo( 'packaged len(%d) is not equal to recv len(%d)', [dPackageLen, nLen] );
        Exit;
      end;
      nCrc32Code := ntohl( ReadCardinal );
      nDataLen := nLen - CtProtocalSize;

      Position := Position + nDataLen;
      if ReadByte <> CtProtocalEnd then
      begin
        DoErrorInfo( 'protocal version number error.(End byte error)' );
        Exit;
      end;

      pData := PChar(Memory + CtProtocalSize - 1);
      if not DecodeBuffer(nCrc32Code, pData, nDataLen) then
      begin
        DoErrorInfo( 'Decode crc32 code error' );
        Exit;
      end;

      InitMemory( pData, nDataLen );
      OnRecvBuffer( nIP, nPort, CmdStream );
    end;
  finally
    CmdStream.Free;
  end;
end;

procedure TxdUdpIOHandle.OnRecvBuffer(const AIP: Cardinal; const APort: Word; CmdStream: TxdOuterMemory);
begin
//  SendBuffer( AIP, APort, CmdStream.Memory, CmdStream.Size );
//  OutputDebugString( 'RecvBuffer...........' );
end;

function TxdUdpIOHandle.SendBuffer(const AIP: Cardinal; const AHostPort: Word; const ApBuffer: PAnsiChar;
  const ABufLen: Word): Integer;
var
  Package: array[0..CtMaxUdpSize - 1] of AnsiChar;
  CmdStream: TxdOuterMemory;
  dLen: Word;
  nCrc32Code: Cardinal;
  pData: PAnsiChar;
  nPos: Integer;
begin
  if ABufLen + CtProtocalSize > CtMaxPackageSize then
  begin
    DoErrorInfo( 'send buffer len(%d) is too long, maxBufferSize is %d', [ABufLen, CtMaxPackageSize - CtProtocalSize ] );
    Result := -2;
    Exit;
  end;
  CmdStream := TxdOuterMemory.Create;
  try
    with CmdStream do
    begin
      InitMemory( @Package, CtMaxUdpSize );
      //版本(1字节) - 此包总长度(2) - CRC32码(4) - 要处理的数据(n) - 结束信息(1)
      WriteByte( CtProtocalVersion );
      dLen := ABufLen + CtProtocalSize;
      WriteWord( htons(dLen) );
      nPos := Position;
      WriteCardinal( 0 );
      WriteLong( ApBuffer, ABufLen );
      WriteByte( CtProtocalEnd );
      pData := Memory + CtProtocalSize - 1;
      nCrc32Code := EncodeBuffer( pData, ABufLen );
      Position := nPos;
      WriteCardinal( htonl(nCrc32Code) );
      pData := Memory;
    end;
    Result := __SendBuffer( AIP, AHostPort, pData^, dLen );
    if Result = dLen then
    begin
      InterlockedIncrement( FSendPackageCount );
      Result := dLen - CtProtocalSize;
    end;
  finally
    CmdStream.Free;
  end;
end;

procedure TxdUdpIOHandle.StatisticalData(const AIP: Cardinal; const APort: Word; const ARecvBufLen: Integer);
begin

end;

end.
