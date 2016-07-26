{
单元名称: uJxdUdpFileServer
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明: 封装与分析同步包命令
开始时间: 2011-04-19
修改时间: 2011-04-19 (最后修改时间)
类说明  :
   提供文件服务，向需要者提供文件内容
   处理文件传输协议
}
unit uJxdUdpFileServer;

interface

uses
  Windows, SysUtils, WinSock2, uJxdDataStream,
  uJxdUdpBasic, uJxdUdpsynchroBasic, uJxdServerCommon, uJxdCmdDefine, uJxdFileUploadManage;

type
  {$M+}
  TxdUdpFileServer = class(TxdServerCommon)
  public
    constructor Create; override;
  protected
    function  DoBeforOpenUDP: Boolean; override;
    procedure DoHandleCmd(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); override;
  private
    FFileUploadManage: TxdFileUploadManage;
    procedure SetFileUploadManage(const Value: TxdFileUploadManage);
  published
    property FileUploadManage: TxdFileUploadManage read FFileUploadManage write SetFileUploadManage;
  end;
  {$M-}

implementation

{ TxdUdpFileServer }

constructor TxdUdpFileServer.Create;
begin
  inherited;
  FServerStyle := srvFileShare;
  FSelfID := CtFileShareServerID;
  FOnlineServerIP := inet_addr('192.168.1.100');
  FOnlineServerPort := 8989;
end;

function TxdUdpFileServer.DoBeforOpenUDP: Boolean;
begin
  if not Assigned(FFileUploadManage) then
  begin
    Result := False;
    OutputDebugString( 'TxdUdpFileServer 没有设置 FileUploadManage, 文件服务器无法启动' );
  end
  else
    Result := inherited DoBeforOpenUDP;
end;

procedure TxdUdpFileServer.DoHandleCmd(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer; const AIsSynchroCmd: Boolean;
  const ASynchroID: Word);
var
  pCmd: PCmdHead;
begin
  pCmd := PCmdHead(ABuffer);
  case pCmd^.FCmdID of
    CtCmd_RequestFileData: FFileUploadManage.DoHandleCmd_RequestFileData( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_QueryFileInfo: FFileUploadManage.DoHandleCmd_QueryFileInfo( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_GetFileSegmentHash: FFileUploadManage.DoHandleCmd_GetFileSegmentHash( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
  end;
end;

procedure TxdUdpFileServer.SetFileUploadManage(const Value: TxdFileUploadManage);
begin
  if Assigned(Value) then
    FFileUploadManage := Value;
end;

end.
