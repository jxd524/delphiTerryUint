{
单元名称: uJxdGUIRegister
单元作者: 江晓德(jxd524@163.com)
说    明: 组件注册
开始时间: 2011-09-20
修改时间: 2011-09-20 (最后修改)
}
unit uJxdGpRegister;

interface

uses
  Windows, Messages, SysUtils, Classes, ComCtrls, DesignIntf, DesignConst,DesignEditors;

procedure Register;

implementation

uses
  uJxdGpButton, uJxdGpScrollBar, uJxdGpTrackBar, uJxdGpStringGrid, 
  uJxdGpGifShow, uJxdGpPanel, uJxdGpForm, uJxdGpComboBox, uJxdGpTabSet;

procedure Register;
begin
  RegisterComponents('Terry GdiPlus Components', 
    [TxdButton, TxdScrollBar, TxdTrackBar, TxdStringGrid, 
     TxdGifShow, TxdPanel, TxdGraphicsPanel, TxdComboBox, TxdGpTabSet]);
  RegisterCustomModule( TxdForm, TCustomModule );
end;

initialization
  
  
end.
