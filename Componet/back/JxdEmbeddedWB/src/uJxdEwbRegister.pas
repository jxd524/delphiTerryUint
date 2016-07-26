unit uJxdEwbRegister;

interface

uses
   Classes, uJxdEmbeddedWB;

procedure Register;

implementation

uses
   SysUtils, ActnList;

procedure Register;
begin
   RegisterComponents( 'JxdEmbedded Web Browser', [TJxdEmbeddedWB] );
end;

end.
