unit JxdProxy;

interface
  uses Classes;

  {代理服务器验证类型}
type
  TAuthenType = (atNone, atUserPass);
  TProxySettings = class(TPersistent)
  protected
    FEnabled: Boolean;
    FHost, FUserName, FPassword: String;
    FPort: Integer;
    FAuthenType:TAuthenType;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled:Boolean read FEnabled write FEnabled default False;
    property  AuthenType: TAuthenType read FAuthenType write FAuthenType default atNone;
    property  Host: String read FHost write FHost;
    property  UserName: String read FUserName write FUserName;
    property  Password: String read FPassword write FPassword;
    property  Port: Integer read FPort write FPort;
  end;

implementation

procedure TProxySettings.Assign(Source: TPersistent);
begin
  if Source is TProxySettings then begin
    with TProxySettings(Source) do begin
      Self.Enabled := Enabled;
      Self.FAuthenType  := AuthenType;
      Self.FHost := Host;
      Self.FUserName := UserName;
      Self.FPassword := Password;
      Self.FPort := Port;
    end;
  end
  else begin
    inherited Assign(Source);
  end;
end;

end.
