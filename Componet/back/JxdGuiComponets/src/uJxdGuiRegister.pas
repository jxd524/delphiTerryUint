{
单元名称: uJxdGUIRegister
单元作者: 江晓德(jxd524@163.com)
说    明: 组件注册
开始时间: 2010-06-08
修改时间: 2010-12-17 (最后修改)
}
unit uJxdGUIRegister;

interface

uses
  Windows, Messages, SysUtils, Classes, ComCtrls;

procedure Register;

implementation

uses uJxdTrayIcon, uJxdButton, uJxdProgressBar, uJxdPanel, uJxdScrollBar, uJxdStringGrid, uJxdTabSet;

procedure Register;
begin
  RegisterComponents('Jxd GUI2.0', [TxdTrayIcon, TxdButton, TxdPanel, TxdProgressBar, TxdScrollBar,
                                    TxdStringGrid, TxdTabSet]);
end;

end.
