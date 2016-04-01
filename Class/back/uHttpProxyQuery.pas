unit uHttpProxyQuery;

interface
  uses Classes, uSimulateBSWeb, IdHTTPHeaderInfo;

type
  TOnQueryTest = procedure(Sender: TObject; AIsGetQuery: Boolean; AText: string) of object;
  THttpProxyQuery = class(TSimulateWeb)
  private
    FGetServer: string;
    FPostServer: string;
    FOnQueryText: TOnQueryTest;
    function GetReadTimeout: Integer;
    procedure SetReadTimeout(const Value: Integer);
    function GetConnectTimeout: Integer;
    procedure SetConnectTimeout(const Value: Integer);
    function GetProxyParam: TIdProxyConnectionInfo;
    procedure SetProxyParam(const Value: TIdProxyConnectionInfo);
    procedure NotifyQueryText(AIsGetQuery: Boolean; AText: string);
  public
    constructor Create(AOwner: TComponent); override;
    function TestGet: Boolean;
    function TestPost: Boolean;
  published
    property GetServer: string read FGetServer write FGetServer;
    property PostServer: string read FPostServer write FPostServer;
    property ProxyParam: TIdProxyConnectionInfo read GetProxyParam write SetProxyParam;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
    property OnQueryText: TOnQueryTest read FOnQueryText write FOnQueryText;
  end;

implementation

{ TProxyQuery }

constructor THttpProxyQuery.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );
  FGetServer := 'http://www.google.cn/';
  FPostServer := 'http://reg.163.com/login.jsp';
  FHttpWeb.ConnectTimeout := 3000;
  FHttpWeb.ReadTimeout := 3000;
  FPostData.Text := 'j=xd';
end;

function THttpProxyQuery.GetConnectTimeout: Integer;
begin
  Result := FHttpWeb.ConnectTimeout;
end;

function THttpProxyQuery.GetProxyParam: TIdProxyConnectionInfo;
begin
  Result := FHttpWeb.ProxyParams;
end;

function THttpProxyQuery.GetReadTimeout: Integer;
begin
  Result := FHttpWeb.ReadTimeout;
end;

procedure THttpProxyQuery.NotifyQueryText(AIsGetQuery: Boolean; AText: string);
begin
  if Assigned(FOnQueryText) then
    FOnQueryText( Self, AIsGetQuery, AText );
end;

procedure THttpProxyQuery.SetConnectTimeout(const Value: Integer);
begin
  FHttpWeb.ConnectTimeout := Value;
end;

procedure THttpProxyQuery.SetProxyParam(const Value: TIdProxyConnectionInfo);
begin
  FHttpWeb.ProxyParams.Assign( Value );
end;

procedure THttpProxyQuery.SetReadTimeout(const Value: Integer);
begin
  FHttpWeb.ReadTimeout := Value;
end;

function THttpProxyQuery.TestGet: Boolean;
var
  strText: string;
begin
  Result := False;
  try
    strText := FHttpWeb.Get( FGetServer );
    Result := True;
  except
    if (FHttpWeb.ResponseCode > 100) and (FHttpWeb.ResponseCode < 400) then
      Result := True;
  end;
  NotifyQueryText( True, strText );
end;

function THttpProxyQuery.TestPost: Boolean;
var
  strText: string;
begin
  Result := False;
  try
    strText := FHttpWeb.Post( FPostServer, FPostData );
    Result := True;
  except
    if (FHttpWeb.ResponseCode > 100) and (FHttpWeb.ResponseCode < 400) then
      Result := True;
  end;
  NotifyQueryText( False, strText );
end;

end.
