unit uJxdUdpHashServer;

interface

uses
  Windows, SysUtils, WinSock2, uJxdDataStream,
  uJxdUdpBasic, uJxdUdpsynchroBasic, uJxdServerCommon, uJxdCmdDefine, uJxdHashTableManage;

type
  TxdUdpHashServer = class(TxdServerCommon)
  public
    constructor Create; override;
  protected
    function  DoBeforOpenUDP: Boolean; override;
    procedure DoAfterCloseUDP; override;
    
    procedure DoHandleCmd(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer;
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); override;
  private
    FHashManage: TxdHashTableManage;
    procedure SetHashManage(const Value: TxdHashTableManage);
  public
    property HashManage: TxdHashTableManage read FHashManage write SetHashManage;
  end;


implementation

{ TxdUdpHashServer }

constructor TxdUdpHashServer.Create;
begin
  inherited;
  FServerStyle := srvHash; //设置服务器类型,不可改变
  FSelfID := CtHashServerID;
  FOnlineServerIP := inet_addr('192.168.1.100');
  FOnlineServerPort := 8989;
end;

procedure TxdUdpHashServer.DoAfterCloseUDP;
begin
  inherited;
  FHashManage.Active := False;
end;

function TxdUdpHashServer.DoBeforOpenUDP: Boolean;
begin
  if not Assigned(FHashManage) then
    raise Exception.Create( 'must set the hash manage' );
  Result := inherited DoBeforOpenUDP;
  FHashManage.Active := Result;
end;

procedure TxdUdpHashServer.DoHandleCmd(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Integer; const AIsSynchroCmd: Boolean;
  const ASynchroID: Word);
var
  pCmd: PCmdHead;
begin
  pCmd := PCmdHead(ABuffer);
  case pCmd^.FCmdID of
    CtCmd_UpdateFileHashTable: FHashManage.DoHandleCmd_UpdateFileHashTable( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_SearchFileUser: FHashManage.DoHandleCmd_SearchFileUser( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmd_ClientShutDown: FHashManage.DoHandleCmd_ClientShutdown( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
  end;
end;

procedure TxdUdpHashServer.SetHashManage(const Value: TxdHashTableManage);
begin
  if not Active then
    FHashManage := Value;
end;

end.
