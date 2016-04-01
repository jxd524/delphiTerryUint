unit uJxdAppReg;

interface

uses
  Windows, Messages, SysUtils, Classes, ComCtrls;

procedure Register;

implementation

uses JxdDialUp, uJxdVidGrab;

procedure Register;
begin
  RegisterComponents('JxdAppComponent', [TJxdDialUp, TJxdVideoGrabber]);
end;

end.
