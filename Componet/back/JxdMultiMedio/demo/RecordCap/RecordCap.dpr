program RecordCap;

uses
  Forms,
  uMain in 'uMain.pas' {Form1},
  uKBoxEffect in 'uKBoxEffect.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
