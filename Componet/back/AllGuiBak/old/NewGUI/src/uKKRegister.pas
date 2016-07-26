unit uKKRegister;

interface

uses
  SysUtils, Classes, Controls, StdCtrls;

procedure Register;

implementation

uses uKKButton, uKKCheckButton, uKKSplitter, uKKComboBox, uKKEdit, uKKPanel, uKKTrackBar, uKKScrollBar, uKKStringGrid,
  uKKStreatchPanel, uKKTrayIcon, uKKLabel;

procedure Register;
begin
  RegisterComponents('NewKKGui', [TKKButton, TKKCheckButton, TKKSplitter, TKKSplitter, TKKComboBox, TKKEdit, TKKPanel,
                               TKKTrackBar, TKKScrollBar, TKKStringGrid, TKKStreatchPanel, TKKTrayIcon, TKKLabel]);
end;

end.

