unit uKBoxAudioMixer;

interface
uses
  Classes, uSysAudio, uJxdSystemAudio;

type
  TKBoxAudioMixer = class(TSysAudio)
  public
    constructor Create;
    destructor  Destroy; override;

    function  GetLineVolume(const AIndex: Integer; var LeftVol, RightVol, Mute:Integer; var Stereo, VolDisabled, MuteDisabled, MuteIsSelect:Boolean):Boolean;
    function  SetLineVolume(const AIndex: Integer; const LeftVol, RightVol:Integer; const Mute: Boolean): Boolean;
    function  GetWaveInLinesInfo(const Alt: TStrings): Integer;
  private

  end;

implementation

{ TKBoxAudioMixer }

constructor TKBoxAudioMixer.Create;
begin
  inherited;
end;

destructor TKBoxAudioMixer.Destroy;
begin

  inherited;
end;

function TKBoxAudioMixer.GetLineVolume(const AIndex: Integer; var LeftVol, RightVol, Mute: Integer; var Stereo, VolDisabled,
  MuteDisabled, MuteIsSelect: Boolean): Boolean;
begin
  try
    if FWaveInMixerIndex = -1 then
    begin
      Result := False;
      Exit;
    end;
    FSysAudio.MixerId := FWaveInMixerIndex;
    Result := FSysAudio.GetVolume( FWIDestIndex, AIndex, LeftVol, RightVol, Mute, Stereo, VolDisabled, MuteDisabled, MuteIsSelect );
  except
    Result := False;
  end;
end;

function TKBoxAudioMixer.GetWaveInLinesInfo(const Alt: TStrings): Integer;
var
  i: Integer;
  MixerDest: TMixerDestination;
begin
  Result := 0;
  try
    FSysAudio.MixerId := FWaveInMixerIndex;
    if FSysAudio.MixerId <> FWaveInMixerIndex then Exit;

    MixerDest := FSysAudio.Destinations[ FWIDestIndex ];
    for i := 0 to MixerDest.Connections.Count - 1 do
    begin
      Alt.Add( MixerDest.Connections[i].Data.szName );
      Inc( Result );
    end;
  except
    Result := -1;
  end;
end;

function TKBoxAudioMixer.SetLineVolume(const AIndex: Integer; const LeftVol, RightVol: Integer; const Mute: Boolean): Boolean;
begin
  try
    if FWaveInMixerIndex = -1 then
    begin
      Result := False;
      Exit;
    end;
    FSysAudio.MixerId := FWaveInMixerIndex;
    Result := FSysAudio.SetVolume( FWIDestIndex, AIndex, LeftVol, RightVol, Integer(Mute) );
  except
    Result := False;
  end;
end;

end.
