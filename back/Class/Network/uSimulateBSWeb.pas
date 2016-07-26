unit uSimulateBSWeb;

interface
uses
  Classes, SysUtils, IdHttp, IdCookieManager, IdHTTPHeaderInfo, Windows;
  
type
  TSimulateWeb = class
  protected
    FOwner: TComponent;
    FHttpWeb: TIdHttp;
    FCookieManager: TIdCookieManager;
    FOldImage: string;
    FSaveImagePath: string;
    FPostData: TStringList;

    procedure InitWeb;
    procedure FreeWeb;
    procedure DeleteOldImage;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor  Destroy; override;
  end;

implementation

{ TTieBaDrumbeating }

constructor TSimulateWeb.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  InitWeb;
  if not DirectoryExists(FSaveImagePath) then
    ForceDirectories( FSaveImagePath );
  FPostData := TStringList.Create;
end;

procedure TSimulateWeb.DeleteOldImage;
begin
  if (FOldImage <> '') and ( FileExists(FOldImage) ) then
  begin
    SysUtils.DeleteFile( FOldImage );
    FOldImage := '';
  end;
end;

destructor TSimulateWeb.Destroy;
begin

  inherited;
end;

procedure TSimulateWeb.FreeWeb;
begin
  if Assigned(FCookieManager) then
  begin
    FCookieManager.Free;
    FCookieManager := nil;
  end;
  if Assigned(FHttpWeb) then
  begin
    FHttpWeb.Free;
    FHttpWeb := nil;
  end;
  DeleteOldImage;
end;

procedure TSimulateWeb.InitWeb;
begin
  FreeWeb;
  FHttpWeb := TIdHTTP.Create( FOwner );
  FCookieManager := TIdCookieManager.Create( FOwner );
  FHttpWeb.CookieManager := FCookieManager;
  with FHttpWeb do
  begin
    HTTPOptions := [hoKeepOrigProtocol];
    AllowCookies := True;
    Request.UserAgent := 'Mozilla/3.5 (compatible; By Terry Make)';
    Port := 80;
    HandleRedirects := False;
  end;
  FSaveImagePath := ExtractFilePath( ParamStr(0) ) + 'TempVC\';
end;

end.

