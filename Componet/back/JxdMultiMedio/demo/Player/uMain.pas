unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, uJxdPlayer, StdCtrls, uJxdAudioFilter, uJxdDrawPanel;

type
  TForm1 = class(TForm)
    xdPlayer1: TxdPlayer;
    pnl1: TPanel;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    rb1: TRadioButton;
    rb2: TRadioButton;
    rb3: TRadioButton;
    edt1: TEdit;
    btn4: TButton;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure rb1Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
   procedure WMMove(var Message: TWMMove); message WM_MOVE;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  xdPlayer1.Open( edt1.Text );
  xdPlayer1.Play;
  xdPlayer1.CurPlayPos := xdPlayer1.Duration - 100000000;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  xdPlayer1.Stop;
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  xdPlayer1.Pause;
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
//  xdPlayer1.
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  xdPlayer1.Stop;
end;

procedure TForm1.rb1Click(Sender: TObject);
begin
  if rb1.Checked then
    xdPlayer1.AudioState := asAll
  else if rb2.Checked then
    xdPlayer1.AudioState := asLeft
  else if rb3.Checked then
    xdPlayer1.AudioState := asRight;
end;

procedure TForm1.WMMove(var Message: TWMMove);
begin
  inherited;
  xdPlayer1.RepaintVideo;
end;

end.
