unit FrameMode1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, JxdGradientPanel;

type
  TframeComMode1 = class(TFrame)
    pnlTop: TJxdGradientPanel;
    pnlLeft: TJxdGradientPanel;
    pnlClient: TJxdGradientPanel;
    pnlBottom: TJxdGradientPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
