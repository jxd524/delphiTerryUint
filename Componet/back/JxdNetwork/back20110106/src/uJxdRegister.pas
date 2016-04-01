unit uJxdRegister;

interface

uses
  Windows, Messages, SysUtils, Classes, ComCtrls;

procedure Register;

implementation

uses uJxdWebBrowser, uJxdHttpDownManage, uJxdFtpUpTaskManage;

procedure Register;
begin
  RegisterComponents('Jxd Network2.0', [TxdWebBroser, TxdHttpDownManage, TxdFtpUpTaskManage]);
end;

end.
