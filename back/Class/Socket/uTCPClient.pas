unit uTCPClient;

interface

  uses Windows, Classes, SysUtils, Sockets;

type
  TJxdTCPClient = class;
  TRecvThread = class(TThread)
  private
    FOwner: TJxdTCPClient;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TJxdTCPClient);
    destructor Destroy; override;
  end;
  {$M+}
  TOnRecvBuffer = procedure(Sender: TObject; const pRecvBuffer: PChar; const nBufLen: Integer) of object;
  TJxdTCPClient = class(TComponent)
  private
    FRecvBuffer: pChar;
    FRecvMaxBufferLen: Integer;
    FRecvThread: TRecvThread;
    FOnRecvBuffer: TOnRecvBuffer;
    procedure SetRecvMaxBufferLength(const Value: Integer);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure DoRecvThread(Sender: TObject);
  protected
    FCmdSock: TTcpClient;
    procedure DoRecvBuffer(const ARecvBuffer: PChar; const ARecvByte: Integer); virtual;
  public
    function  BindToServer(const AServer: string; const APort: Integer): Boolean;
    function  SendBuffer(var Buf; const ABufferLen: Integer): Integer;
    function  SendString(AStrBuf: string): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read GetActive write SetActive;
    property MaxRecvBufferLength: Integer read FRecvMaxBufferLen write SetRecvMaxBufferLength;
    property OnRecvBuffer: TOnRecvBuffer read FOnRecvBuffer write FOnRecvBuffer;
  end;
  {$M-}
implementation

{ TJxdTCPClient }

function TJxdTCPClient.BindToServer(const AServer: string; const APort: Integer): Boolean;
begin
  Result := not FCmdSock.Active;
  if Result then
  begin
    FCmdSock.RemoteHost := AServer;
    FCmdSock.RemotePort := IntToStr(APort);
  end;
end;

constructor TJxdTCPClient.Create(AOwner: TComponent);
begin
  Inherited;
  FCmdSock := TTcpClient.Create( Self );
  FRecvMaxBufferLen := 1024 * 12;
  GetMem( FRecvBuffer, FRecvMaxBufferLen );
  FRecvThread := nil;
end;

destructor TJxdTCPClient.Destroy;
begin
  if FCmdSock.Active then FCmdSock.Active := False;
  FCmdSock.Free;
  FreeMem( FRecvBuffer, FRecvMaxBufferLen );
  inherited;
end;

procedure TJxdTCPClient.DoRecvBuffer(const ARecvBuffer: PChar; const ARecvByte: Integer);
begin
  if Assigned(FOnRecvBuffer) then
    FOnRecvBuffer(Self, ARecvBuffer, ARecvByte );
end;

procedure TJxdTCPClient.DoRecvThread(Sender: TObject);
var
  nRecvByte: Integer;
begin
  ZeroMemory( FRecvBuffer, FRecvMaxBufferLen );
  nRecvByte := FCmdSock.ReceiveBuf( FRecvBuffer^, FRecvMaxBufferLen );
  if nRecvByte > 0 then
    DoRecvBuffer( FRecvBuffer, nRecvByte )
  else if nRecvByte = 0 then
  begin
    FRecvThread.Terminate;
  end;
end;

function TJxdTCPClient.GetActive: Boolean;
begin
  Result := FCmdSock.Active;
end;

function TJxdTCPClient.SendBuffer(var Buf; const ABufferLen: Integer): Integer;
begin
  Result := FCmdSock.SendBuf( Buf, ABufferLen );
end;

function TJxdTCPClient.SendString(AStrBuf: string): Integer;
begin
  Result := FCmdSock.SendBuf( AStrBuf[1], Length(AStrBuf) );
end;

procedure TJxdTCPClient.SetActive(const Value: Boolean);
begin
  if Value then
  begin
    if (Value <> Active) and (FCmdSock.RemoteHost <> '') and (FCmdSock.RemotePort <> '') then
    begin
      FCmdSock.Active := True;
      if FCmdSock.Connected then
      FRecvThread := TRecvThread.Create( Self );
    end;
  end
  else
  begin
    FCmdSock.Active := False;
    FRecvThread.Terminate;
    FRecvThread := nil;
  end;
end;

procedure TJxdTCPClient.SetRecvMaxBufferLength(const Value: Integer);
begin
  if not Active then
  begin
    if Assigned(FRecvBuffer) then
      FreeMem( FRecvBuffer );
    GetMem( FRecvBuffer, Value );
    FRecvMaxBufferLen := Value;
  end;
end;

{ TRecvThread }

constructor TRecvThread.Create(AOwner: TJxdTCPClient);
begin
  FOwner := AOwner;
  FreeOnTerminate := True;
  inherited Create( False );
end;

destructor TRecvThread.Destroy;
begin

  inherited;
end;

procedure TRecvThread.Execute;
begin
  while (not Terminated) and FOwner.Active do
    FOwner.DoRecvThread( Self );
end;

end.
