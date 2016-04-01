unit JxdGuiReg;

interface

uses
  Windows, Messages, SysUtils, Classes, ComCtrls;

procedure Register;

implementation

uses JxdTrayIcon, JxdLyricShow, JxdComboBox, JxdProgressBar, JxdGradientButton, JxdGradientPanel, JxdEdit,
  JxdCommonMode1, JxdHtmlEdit, JxdStringGrid, JxdScrollBar;

procedure Register;
begin
  RegisterComponents('JxdGuiComponent', [TJxdTrayIcon, TJxdLyricShow, TJxdComboBox, TJxdScrollBar, TJxdGradientButton, TJxdGradientPanel,
                                         TJxdEdit, TJxdHtmlEdit, TJxdCommonMode1, TJxdStringGrid, TJxdScrollBar]);
end;

end.
