unit uSysAudio;

interface
uses
  Windows, Classes, SysUtils, uJxdSystemAudio, MMSystem;

type
  TSysAudio = class
  public
    constructor Create;
    destructor  Destroy; override;
    {系统主音量}
    function  GetMainVolume(var LeftVol, RightVol, Mute:Integer; var Stereo, VolDisabled, MuteDisabled, MuteIsSelect:Boolean):Boolean;
    function  SetMainVolume(const LeftVol, RightVol:Integer; const Mute: Boolean): Boolean;

    {}
    function GetVolume (ADestination, AConnection:Integer; var LeftVol, RightVol, Mute:Integer; var Stereo, VolDisabled, MuteDisabled, MuteIsSelect:Boolean):Boolean;
    function SetVolume (ADestination, AConnection:Integer; LeftVol, RightVol, Mute:Integer):Boolean;
  protected
    FSysAudio: TxdAudioMixer;
    FMainVolumeMixerIndex, FMAVDestIndex: Integer; //主音量索引
    FWaveInMixerIndex, FWIDestIndex: Integer;
  end;

implementation

{ TSysAudio }

constructor TSysAudio.Create;
var
  i, j: Integer;
  MixerDest: TMixerDestination;
begin
  FSysAudio := TxdAudioMixer.Create( nil );
  FMainVolumeMixerIndex := -1;
  FMAVDestIndex := -1;
  for i := 0 to FSysAudio.MixerCount - 1 do
  begin
    FSysAudio.MixerId := i;
    for j := 0 to FSysAudio.Destinations.Count - 1 do
    begin
      MixerDest := FSysAudio.Destinations[j];
      case MixerDest.Data.dwComponentType of
        MIXERLINE_COMPONENTTYPE_DST_SPEAKERS:
        begin
          //Volume
          FMainVolumeMixerIndex := i;
          FMAVDestIndex := j;
        end;
        MIXERLINE_COMPONENTTYPE_DST_WAVEIN:
        begin
          //Record
          FWaveInMixerIndex := i;
          FWIDestIndex := j;
        end;
      end;
    end;
  end;
end;

destructor TSysAudio.Destroy;
begin
  FSysAudio.Free;
  inherited;
end;

function TSysAudio.GetMainVolume(var LeftVol, RightVol, Mute: Integer; var Stereo, VolDisabled, MuteDisabled,
  MuteIsSelect: Boolean): Boolean;
begin
  try
    if FMainVolumeMixerIndex = -1 then
    begin
      Result := False;
      Exit;
    end;
    FSysAudio.MixerId := FMainVolumeMixerIndex;
    Result := FSysAudio.GetVolume( FMAVDestIndex, -1, LeftVol, RightVol, Mute, Stereo, VolDisabled, MuteDisabled, MuteIsSelect );
  except
    Result := False;
  end;
end;

function TSysAudio.GetVolume(ADestination, AConnection: Integer; var LeftVol, RightVol, Mute: Integer; var Stereo, VolDisabled,
  MuteDisabled, MuteIsSelect: Boolean): Boolean;
begin
  Result := FSysAudio.GetVolume( ADestination, AConnection, LeftVol, RightVol, Mute, Stereo, VolDisabled, MuteDisabled, MuteIsSelect );
end;

function TSysAudio.SetMainVolume(const LeftVol, RightVol: Integer; const Mute: Boolean): Boolean;
begin
  try
    if FMainVolumeMixerIndex = -1 then
    begin
      Result := False;
      Exit;
    end;
    FSysAudio.MixerId := FMainVolumeMixerIndex;
    Result := FSysAudio.SetVolume( FMAVDestIndex, -1, LeftVol, RightVol, Integer(Mute) );
  except
    Result := False;
  end;
end;

function TSysAudio.SetVolume(ADestination, AConnection, LeftVol, RightVol, Mute: Integer): Boolean;
begin
  Result := FSysAudio.SetVolume( ADestination, AConnection, LeftVol, RightVol, Mute );
end;

end.
