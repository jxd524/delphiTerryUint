unit FrameAutoDialUp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, JxdCustomButton, JxdGradientButton, JxdComboBox, JxdEdit, ExtCtrls, JxdGradientPanel, JxdDialUp;

type
  TFrmAutoDialUp = class(TFrame)
    JxdGradientPanel2: TJxdGradientPanel;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    lbl6: TLabel;
    edtUserName: TJxdEdit;
    edtUserPass: TJxdEdit;
    chkNetLogin: TCheckBox;
    cbConnectionName: TJxdComboBox;
    edtFrequency: TJxdEdit;
    btnTestCon: TJxdGradientButton;
    btnDisCon: TJxdGradientButton;
    JxdDialUp: TJxdDialUp;
    btnDialup: TJxdGradientButton;
    procedure btnDialupClick(Sender: TObject);
    procedure btnTestConClick(Sender: TObject);
    procedure btnDisConClick(Sender: TObject);
    procedure chkNetLoginClick(Sender: TObject);
  private
    { Private declarations }
  public
    function DialUp: Boolean;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses uNetwork;
{$R *.dfm}

procedure TFrmAutoDialUp.btnDisConClick(Sender: TObject);
begin
  JxdDialUp.GoOffline;
end;

procedure TFrmAutoDialUp.btnTestConClick(Sender: TObject);
begin
  if DialUp then
  begin
   Sleep(100);
   ShowMessage('连接成功,本机IP: ' + GetLocalIP + #13#10 + '测试打开www.geysoft.com');
   OpenExplorer('http://www.geysoft.com');
  end;
end;

procedure TFrmAutoDialUp.chkNetLoginClick(Sender: TObject);
begin
  cbConnectionName.Enabled := chkNetLogin.Checked;
  edtUserName.Enabled := chkNetLogin.Checked;
  edtUserPass.Enabled := chkNetLogin.Checked;
  edtFrequency.Enabled := chkNetLogin.Checked;
  btnDisCon.Enabled := chkNetLogin.Checked;
  btnTestCon.Enabled := chkNetLogin.Checked;
  btnDialup.Enabled := chkNetLogin.Checked;
end;

constructor TFrmAutoDialUp.Create(AOwner: TComponent);
begin
  inherited;
  cbConnectionName.Items := JxdDialUp.PossibleConnections;
end;

function TFrmAutoDialUp.DialUp: Boolean;
begin
  JxdDialUp.GoOffline;
  Sleep(100);
  JxdDialUp.Username:=Trim(edtUserName.Text);
  JxdDialUp.Password:=Trim(edtUserPass.Text);
  JxdDialUp.ConnectTo:=Trim(cbConnectionName.Text);
  Result := JxdDialUp.GoOnline
end;

procedure TFrmAutoDialUp.btnDialupClick(Sender: TObject);
begin
  DialUp;
end;

end.
