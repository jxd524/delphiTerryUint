unit JxdReg;

interface

uses
  Windows, Messages, SysUtils, Classes;

procedure Register;

implementation

uses JxdUDPServer;


procedure Register;
begin
  RegisterComponents('JxdNetworkCore',[TJxdUDPServer]);
end;

end.
