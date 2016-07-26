unit uJxdGuiReg;

interface

uses
  Windows, Messages, SysUtils, Classes, ComCtrls;

procedure Register;

implementation

uses uJxdTrayIcon, uJxdLyricShow, uJxdComboBox, uJxdProgressBar, uJxdGradientButton, uJxdGradientPanel, uJxdEdit,
  uJxdCommonMode1, uJxdHtmlEdit, uJxdScrollBar, uJxdButton, uJxdCheckButton;

procedure Register;
begin
  RegisterComponents('JxdGuiComponent', [TJxdTrayIcon, TJxdLyricShow, TJxdComboBox, TJxdScrollBar, TJxdGradientButton, TJxdGradientPanel,
                                         TJxdEdit, TJxdHtmlEdit, TJxdCommonMode1, TJxdScrollBar, TJxdButton, TJxdCheckButton, TJxdComboBox]);
end;

end.
