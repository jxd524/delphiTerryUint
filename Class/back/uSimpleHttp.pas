unit uSimpleHttp;

interface
  uses uBasicTCPClient;

type
  TJxdHttp = class
  private
    procedure DoRequest(const AURL: string);
  public
    constructor Create;
    destructor  Destroy; override;

    function Get(const AURL: string): string;
  end;

implementation

{ TJxdHttp }

constructor TJxdHttp.Create;
begin

end;

destructor TJxdHttp.Destroy;
begin

  inherited;
end;

procedure TJxdHttp.DoRequest(const AURL: string);
begin

end;

function TJxdHttp.Get(const AURL: string): string;
begin
  DoRequest(AURL);
end;

end.
