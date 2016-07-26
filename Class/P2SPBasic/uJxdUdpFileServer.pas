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
  uJxdUdpBasic, uJxdUdpsynchroBasic, uJxdServerCommon, uJxdUdpDefine;

type
  {$M+}
  TxdUdpFileServer = class(TxdServerCommon)
  public
    constructor Create; override;
  protected
    procedure DoHandleCmd(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); override;
  private
    FOnFileShareCmdEvent: TOnFileTrasmintEvent;
  published
    property OnFileShareCmdEvent: TOnFileTrasmintEvent read FOnFileShareCmdEvent write FOnFileShareCmdEvent;
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

procedure TxdUdpFileServer.DoHandleCmd(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer; const AIsSynchroCmd: Boolean;
  const ASynchroID: Word);
var
  pCmd: PCmdHead;
begin
  pCmd := PCmdHead(ABuffer);
  case pCmd^.FCmdID of
    CtCmd_RequestFileData,
    CtCmd_QueryFileInfo,
    CtCmd_QueryFileProgress,
    CtCmd_GetFileSegmentHash:
    begin
      if Assigned(OnFileShareCmdEvent) then
        OnFileShareCmdEvent( pCmd^.FCmdID, AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    end;
//    CtCmd_RequestFileData: FFileUploadManage.DoHandleCmd_RequestFileData( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
//    CtCmd_QueryFileInfo: FFileUploadManage.DoHandleCmd_QueryFileInfo( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
//    CtCmd_GetFileSegmentHash: FFileUploadManage.DoHandleCmd_GetFileSegmentHash( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
  end;
end;

end.
