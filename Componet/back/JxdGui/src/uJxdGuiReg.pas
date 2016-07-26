unit uJxdGuiReg;

interface

uses
  Windows, Messages, SysUtils, Classes, ComCtrls;

procedure Register;

implementation

uses uJxdTrayIcon, uJxdLyricShow, uJxdComboBox, uJxdProgressBar, uJxdGradientButton, uJxdGradientPanel, uJxdEdit,
  uJxdCommonMode1, uJxdHtmlEdit, uJxdScrollBar, uJxdBitmapButton, uJxdCheckButton, uJxdStreatchPanel, uJxdGroupBox,
  uJxdStringGrid, uJxdGradientTabSet, uJxdHintList;

procedure Register;
begin
  RegisterComponents('JxdGuiComponents', [TJxdTrayIcon, TJxdLyricShow, TJxdComboBox, TJxdScrollBar, TJxdGradientButton, TJxdGradientPanel,
                                         TJxdEdit, TJxdHtmlEdit, TJxdCommonMode1, TJxdScrollBar, TJxdBitmapButton, TJxdCheckButton, TJxdComboBox,
                                         TJxdStreatchPanel, TJxdGroupBox, TJxdStringGrid, TJxdGradientTabSet, TJxdHintList]);
end;

end.
