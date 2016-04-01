unit uJxdAudioFilter;

interface

uses
  Windows, SysUtils, BaseClass, MMSystem, DirectShow9, ActiveX;

type
  TAudioChannelStyle = (asAll, asLeft, asRight);
  TxdAudioChannelFilter = class(TBCTransInPlaceFilter)
  public
    AudioChannelStyle: TAudioChannelStyle;
    
    constructor Create;
    destructor Destroy; override;
    
    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
    function Transform(Sample: IMediaSample): HRESULT; override;
  private
    procedure PCMWaveData(Buf: PByte; Length: Integer; P: PWaveFormatEx);
  end;

implementation

const
  CtAudioChannelFilterName = 'Audio Filter';
  CtAudioChannelGUID : TGUID = '{3F06D3B4-F953-463B-BF42-FE835BEE9AB2}';

function TxdAudioChannelFilter.CheckInputType(mtIn: PAMMediaType): HRESULT;
const
  WAVE_FORMAT_IEEE_FLOAT      = $0003;
  WAVE_FORMAT_DOLBY_AC3_SPDIF = $0092;
var
  P: PWaveFormatEx;
begin
  Result := VFW_E_TYPE_NOT_ACCEPTED;
  if not IsEqualGUID(mtIn^.formattype, FORMAT_WaveFormatEx) then
    Exit;
  P := PWaveFormatEx(mtIn^.pbFormat);
  if (P^.nChannels > 2) and (P^.wFormatTag <> WAVE_FORMAT_EXTENSIBLE) then
  begin
    Result := VFW_E_INVALIDMEDIATYPE;
    Exit;
  end;
  if IsEqualGUID(mtIn^.majortype, MEDIATYPE_Audio) and (P^.wBitsPerSample in [8, 16, 24, 32]) and
    ((P^.wFormatTag = WAVE_FORMAT_PCM) or (P^.wFormatTag = WAVE_FORMAT_IEEE_FLOAT) or
    (P^.wFormatTag = WAVE_FORMAT_DOLBY_AC3_SPDIF) or (P^.wFormatTag = WAVE_FORMAT_EXTENSIBLE)) then
    Result := S_OK;
end;

constructor TxdAudioChannelFilter.Create;
var
  hr: HRESULT;
begin
  inherited Create(CtAudioChannelFilterName, nil, CtAudioChannelGUID, hr);
  AudioChannelStyle := asAll;
end;

destructor TxdAudioChannelFilter.destroy;
begin
  inherited;
end;

procedure TxdAudioChannelFilter.PCMWaveData(Buf: PByte; Length: Integer; P: PWaveFormatEx);
var
  I, J, K, B: Word;
begin
  if (AudioChannelStyle = asAll) or (P.nChannels = 1) or (not P.wBitsPerSample in [8, 16, 24, 32]) then Exit;
  try    
    B := P.wBitsPerSample shr 2;
    K := B shr 2;
    for I := 0 to Length - 1 do
    begin
      if I mod B = 0 then
      begin
        for J := 0  to B shr 1 - 1 do
          if AudioChannelStyle = asLeft then
            PByte(Integer(Buf) + I + J + K + 1)^ := PByte(Integer(Buf) + I + J)^
          else
            PByte(Integer(Buf) + I + J)^ := PByte(Integer(Buf) + I + J + K + 1)^;
      end;
    end;
  except
  end;
end;

function TxdAudioChannelFilter.Transform(Sample: IMediaSample): HRESULT;
var
  Buf: PByte;
begin
  Result := S_OK;
  try
    Sample.GetPointer(Buf);
    PCMWaveData(Buf, Sample.GetActualDataLength, FInput.CurrentMediaType.MediaType.pbFormat);
  except
    Result := S_FALSE;
  end;
end;


end.

