unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uJxdSystemAudio, StdCtrls, MMSystem, uKBoxAudioMixer, ExtCtrls, uJxdGraphicBaseClass, uJxdProgressBar;

type
  TForm1 = class(TForm)
    cbb1: TComboBox;
    mmo1: TMemo;
    tmr1: TTimer;
    btn1: TButton;
    btn2: TButton;
    lbl1: TLabel;
    edtLeft: TEdit;
    lbl2: TLabel;
    edtRight: TEdit;
    chk1: TCheckBox;
    btn3: TButton;
    pbMIC: TxdProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure xdAudioMixer1ControlChange(Sender: TObject; MixerH, ID: Integer);
    procedure xdAudioMixer1LineChange(Sender: TObject; MixerH, ID: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tmr1Timer(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
  private
    FSysAudio: TKBoxAudioMixer;
    procedure ShowMainSystemAudioInfo;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  tmr1.Enabled := not tmr1.Enabled;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  FSysAudio.SetMainVolume( StrtoInt(edtLeft.Text), StrtoInt(edtRight.Text), chk1.Checked );
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  FSysAudio.GetWaveInLinesInfo( cbb1.Items );
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FSysAudio.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSysAudio := TKBoxAudioMixer.Create;
end;

procedure TForm1.ShowMainSystemAudioInfo;
var
  LeftVol, RightVol, Mute:Integer;
  Stereo, VolDisabled, MuteDisabled, MuteIsSelect:Boolean;
  function BooleanToStr(const ABoolean: Boolean): string;
  begin
    if ABoolean then
      Result := 'True'
    else
      Result := 'False';
  end;
begin
  mmo1.Clear;
  if FSysAudio.GetMainVolume( LeftVol, RightVol, Mute, Stereo,
    VolDisabled, MuteDisabled, MuteIsSelect) then
  begin
    mmo1.Lines.Add( 'LeftVol:      ' + IntToStr(LeftVol) );
    mmo1.Lines.Add( 'RightVol:     ' + IntToStr(RightVol) );
    mmo1.Lines.Add( 'Mute:         ' + IntToStr(Mute) );
    mmo1.Lines.Add( 'Stereo:       ' + BooleanToStr(Stereo) );
    mmo1.Lines.Add( 'VolDisabled:  ' + BooleanToStr(VolDisabled) );
    mmo1.Lines.Add( 'MuteDisabled: ' + BooleanToStr(MuteDisabled) );
    mmo1.Lines.Add( 'MuteIsSelect: ' + BooleanToStr(MuteIsSelect) );
  end
  else
    mmo1.Text := 'Error';
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  tmr1.Enabled := False;
  ShowMainSystemAudioInfo;
  tmr1.Enabled := True;
end;

procedure TForm1.xdAudioMixer1ControlChange(Sender: TObject; MixerH, ID: Integer);
begin
//
end;

procedure TForm1.xdAudioMixer1LineChange(Sender: TObject; MixerH, ID: Integer);
begin
//
end;

end.
