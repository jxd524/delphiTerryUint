unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, OleCtrls, SHDocVw_EWB, EwbCore, EmbeddedWB, uJxdWebBrowser;

type
  TForm1 = class(TForm)
    xdWebBroser1: TxdWebBroser;
    pm1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  xdWebBroser1.Navigate( 'E:\Delphi\MyProject\MySoftTool\browser\xdBrowser\Demo\百度一下，你就知道.htm' );
end;

end.
