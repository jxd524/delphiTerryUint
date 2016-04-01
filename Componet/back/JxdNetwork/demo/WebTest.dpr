program WebTest;

uses
  Forms,
  uMain in 'uMain.pas' {Form1},
  uJxdWebPageInfo in '..\src\uJxdWebPageInfo.pas' {frmWebPageInfo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
