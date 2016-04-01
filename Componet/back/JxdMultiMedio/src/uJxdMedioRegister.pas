unit uJxdMedioRegister;

interface

uses
  Windows, Messages, SysUtils, Classes, ComCtrls;

procedure Register;

implementation

uses uJxdRecordCap, uJxdPlayer, uJxdSystemAudio;

procedure Register;
begin
  RegisterComponents('Jxd MultiMedio', [TxdRecordCap, TxdPlayer, TxdAudioMixer]);
end;

end.
