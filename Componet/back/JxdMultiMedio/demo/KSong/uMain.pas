unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uJxdKSong, ExtCtrls, StdCtrls, PngImage;

type
  TForm1 = class(TForm)
    pnl1: TPanel;
    xdKSong1: TxdKSong;
    btn1: TButton;
    rbPlayerMode: TRadioButton;
    rbRecordMode: TRadioButton;
    rbOnlyRecordMode: TRadioButton;
    rbOnlyPlayerMode: TRadioButton;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    lbl1: TLabel;
    lblCurPosition: TLabel;
    lbl3: TLabel;
    lblDuration: TLabel;
    lbl5: TLabel;
    lblRate: TLabel;
    lbl7: TLabel;
    lblBalance: TLabel;
    lbl9: TLabel;
    lblVolume: TLabel;
    tmr1: TTimer;
    procedure btn1Click(Sender: TObject);
    procedure rbPlayerModeClick(Sender: TObject);
    procedure rbRecordModeClick(Sender: TObject);
    procedure rbOnlyPlayerModeClick(Sender: TObject);
    procedure rbOnlyRecordModeClick(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
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

procedure TForm1.btn1Click(Sender: TObject);
begin
  xdKSong1.Play( 'E:\CompanyWork\MusicT\Kbox\bin\Share\ÒôÀÖ\Brian Holland-You Keep Me Hanging On.wmv' );
//  xdKSong1.Play( 'D:\music\Ð»öª·æ\one inch closer - Ð»öª·æ 05.ËÕÈýÏëËµ.mp3' );
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  xdKSong1.VideoCapIndex := 1;
  xdKSong1.StartRecord( ExtractFilePath(ParamStr(0)) + 'xdKsong.wmv' );
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  xdKSong1.Stop;
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
  xdKSong1.VideoCapIndex := 1;
  xdKSong1.PlayAndRecord( 'E:\CompanyWork\MusicT\Kbox\bin\Share\ÒôÀÖ\Brian Holland-You Keep Me Hanging On.wmv',
    ExtractFilePath(ParamStr(0)) + 'xdKsong_playAndRec.wmv' )
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  obj: TPNGObject;
begin
  obj := TPNGObject.Create;
  obj.LoadFromFile( 'E:\CompanyWork\MusicT\KBox2.0\Resource\²¥·Å´°¿Ú.png' );
//  xdKSong1.PlayBKBitmap := obj;
  obj.Free;
end;

procedure TForm1.rbOnlyPlayerModeClick(Sender: TObject);
begin
  xdKSong1.ShowMode := smOlnyPlayer;
end;

procedure TForm1.rbOnlyRecordModeClick(Sender: TObject);
begin
  xdKSong1.ShowMode := smOlnyRecord;
end;

procedure TForm1.rbPlayerModeClick(Sender: TObject);
begin
  xdKSong1.ShowMode := smPlayer;
end;

procedure TForm1.rbRecordModeClick(Sender: TObject);
begin
  xdKSong1.ShowMode := smRecord;
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  lblCurPosition.Caption := IntToStr( xdKSong1.PlayCurPosition );
  lblDuration.Caption := IntToStr( xdKSong1.PlayDuration );
  lblRate.Caption := FloatToStr( xdKSong1.PlayRate );
  lblBalance.Caption := IntToStr( xdKSong1.PlayBalance );
  lblVolume.Caption := IntToStr( xdKSong1.PlayVolume );
end;

end.
