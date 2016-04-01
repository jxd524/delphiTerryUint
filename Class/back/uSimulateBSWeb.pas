unit uSimulateBSWeb;

interface
uses
  Classes, SysUtils, IdHttp, IdCookieManager, IdCookie, IdHTTPHeaderInfo, Windows, uStringHandle;

const
  CookieName = 'Cookie: ';

type
  TSimulateWeb = class
  private
    FIsInitWeb: Boolean;
    function GetCookiesss: string;
    procedure SetCookie(const Value: string);
  protected
    FOwner: TComponent;
    FHttpWeb: TIdHttp;
    FPostData: TStringList;

    procedure InitWeb;
    procedure FreeWeb;
    procedure DoError(const AErrInfo: string); virtual;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor  Destroy; override;

    procedure AddCookie(const ACookies: string);

    property IsInitWeb: Boolean read FIsInitWeb;
    property Cookie: string read GetCookiesss write SetCookie;
  end;

implementation

{ TTieBaDrumbeating }

procedure TSimulateWeb.AddCookie(const ACookies: string);
var
  lt: TStringList;
  i: Integer;
begin
  if (not Assigned(FHttpWeb)) or (ACookies = '') then Exit;
  lt := TStringList.Create;
  try
    ParseFormatString( ACookies, ';', lt, True, True );
//    for i := 0 to lt.Count - 1 do
//      FHttpWeb.AddCookie( lt[i] );
  finally
    lt.Free;
  end;
end;

constructor TSimulateWeb.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  InitWeb;
  FPostData := TStringList.Create;
end;

destructor TSimulateWeb.Destroy;
begin
  FreeWeb;
  inherited;
end;

procedure TSimulateWeb.DoError(const AErrInfo: string);
begin
  OutputDebugString( PChar(AErrInfo) );
end;

procedure TSimulateWeb.FreeWeb;
begin
  FreeAndNil(FHttpWeb);
  FIsInitWeb := False;
end;

function TSimulateWeb.GetCookiesss: string;
begin
  if Assigned(FHttpWeb) then
    Result := ''//FHttpWeb.GetCookies
  else
    Result := '';
end;

procedure TSimulateWeb.InitWeb;
begin
  FreeWeb;
  FIsInitWeb := True;
  FHttpWeb := TIdHTTP.Create( FOwner );
  with FHttpWeb do
  begin
    HTTPOptions := [hoKeepOrigProtocol];
    AllowCookies := False;
//    JxdHandleCookies := True;
    Request.UserAgent := 'Mozilla/5.0 (Windows; U; Windows NT 5.2; zh-CN; rv:1.8.1.20) Gecko/20081217 Firefox/2.0.0.20';
    Port := 80;
    HandleRedirects := False;
  end;
end;

procedure TSimulateWeb.SetCookie(const Value: string);
begin
  if not Assigned(FHttpWeb) then Exit;
  AddCookie( Value );
end;

end.

