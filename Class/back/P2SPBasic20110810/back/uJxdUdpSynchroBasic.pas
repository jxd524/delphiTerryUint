{
单元名称: uJxdUdpSynchroCommon
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明: 封装与分析同步包命令
开始时间: 2009-3-21
修改时间: 2011-3-21 (最后修改时间)
类说明  :
  只负责判断是否是同步包和提供子类使用的两个把数据封装成同步包的方法
  实现方式：
    在数据的前面加上四个字节，前两个字节表示同步命令，由外部定义；后两位表示同步的标志
  通信协议要求格式：
    1: 每一个包的最前面两个字节必须为命令

}

unit uJxdUdpSynchroBasic;

interface

uses
  Windows, uJxdUdpIOHandle, uJxdDataStream;

type
  TxdUdpSynchroBasic = class(TxdUdpIoHandle)
  public
    constructor Create; override;
    function  AddSynchroSign(AStream: TxdMemoryHandle; const ASynchroID: Word): Boolean; overload;
    procedure AddSynchroSign(ABuf: PAnsiChar; const ASynchroID: Word); overload;
    procedure AddCmdHead(AStream: TxdMemoryHandle; ACmdID: Word); overload;
    procedure AddCmdHead(ABuf: PAnsiChar; ACmdID: Word); overload;
  private
    FSynchroCmd: Word;
    FRecvSynchroPackageCount: Integer;
    procedure SetSynchroCmd(const Value: Word);
  protected
    FSelfID: Cardinal;
    {定义子类处理的虚函数}
    procedure  OnCommonRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); virtual;

    {实现父类方法}
    function  DoBeforOpenUDP: Boolean; override;
    //接收到完整的数据时
    procedure OnRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal); override;
    property SynchroCmd: Word read FSynchroCmd write SetSynchroCmd;
  published
    property RecvSynchroPackageCount: Integer read FRecvSynchroPackageCount;//接收到同步包数量
  end;

implementation

{ TxdUdpSynchroCommon }

function TxdUdpSynchroBasic.AddSynchroSign(AStream: TxdMemoryHandle; const ASynchroID: Word): Boolean;
begin
  Result := AStream.Size - AStream.Position >= 4;
  if Result then
  begin
    AStream.Position := 0;
    AStream.WriteWord( FSynchroCmd );
    AStream.WriteWord( ASynchroID );
  end;
end;

procedure TxdUdpSynchroBasic.AddCmdHead(AStream: TxdMemoryHandle; ACmdID: Word);
begin
  AStream.WriteWord( ACmdID );
  AStream.WriteCardinal( FSelfID );
end;

procedure TxdUdpSynchroBasic.AddCmdHead(ABuf: PAnsiChar; ACmdID: Word);
begin
  Move( ACmdID, ABuf^, 2 );
  Move( FSelfID, PAnsiChar(ABuf + 2)^, 4 );
end;

procedure TxdUdpSynchroBasic.AddSynchroSign(ABuf: PAnsiChar; const ASynchroID: Word);
begin
  Move( FSynchroCmd, ABuf[0], 2 );
  Move( ASynchroID, ABuf[2], 2 );
end;

constructor TxdUdpSynchroBasic.Create;
begin
  inherited;
  SynchroCmd := 999;
end;

function TxdUdpSynchroBasic.DoBeforOpenUDP: Boolean;
begin
  Result := inherited DoBeforOpenUDP;
  if Result then
    FRecvSynchroPackageCount := 0;
end;

procedure TxdUdpSynchroBasic.OnCommonRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer;
  const AIsSynchroCmd: Boolean; const ASynchroID: Word);
begin

end;

procedure TxdUdpSynchroBasic.OnRecvBuffer(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal);
var
  nCmd, nSynchroID: Word;
  bIsSynchroCmd: Boolean;
  pBuf: PAnsiChar;
  nBufLen: Cardinal;
begin
  bIsSynchroCmd := False;
  pBuf := ABuffer;
  nBufLen := ABufLen;
  if ABufLen > 4 then
  begin
    Move( ABuffer[0], nCmd, 2 );
    if nCmd = FSynchroCmd then
    begin
      Move( ABuffer[2], nSynchroID, 2 );
      bIsSynchroCmd := True;
      pBuf := PAnsiChar( Integer(ABuffer) + 4 );
      nBufLen := nBufLen - 4;
      InterlockedIncrement( FRecvSynchroPackageCount );
    end;
  end;
  OnCommonRecvBuffer( AIP, APort, pBuf, nBufLen, bIsSynchroCmd, nSynchroID );
end;

procedure TxdUdpSynchroBasic.SetSynchroCmd(const Value: Word);
begin
  if not Active then
    FSynchroCmd := Value;
end;

end.
