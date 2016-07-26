unit uJxdAudioFilter;

interface

uses
  BaseClass, MMSystem, DirectShow9, ActiveX;

const
  AudioFilter_Name = 'Audio Filter';
  AudioFilter_CLSID : TGUID = '{3F06D3B4-F953-463B-BF42-FE835BEE9AB2}';

type
  TAudioState = (asAll, asLeft, asRight);
  IAudioState = interface
  ['{7B25969B-ED93-4AFD-9113-4C63275531C5}']
    function put_State(State: TAudioState): HRESULT; stdcall;
    function get_State(out State: TAudioState): HRESULT; stdcall;
  end;

type
  TAudioFilter = class(TBCTransInPlaceFilter, IAudioState)  
  public
    constructor Create;
    destructor Destroy; override;
    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
    function Transform(Sample: IMediaSample): HRESULT; override;

    function put_State(State: TAudioState): HRESULT; stdcall;
    function get_State(out State: TAudioState): HRESULT; stdcall;
  private
    FAudioState: TAudioState;
    procedure PCMWaveData(Buf: PByte; Length: Integer; P: PWaveFormatEx);
  end;

implementation

function TAudioFilter.CheckInputType(mtIn: PAMMediaType): HRESULT;
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

constructor TAudioFilter.Create;
var
  hr: HRESULT;
begin
  inherited Create(ClassName, nil, AudioFilter_CLSID, hr);
end;

destructor TAudioFilter.destroy;
begin
  inherited;
end;

function TAudioFilter.get_State(out State: TAudioState): HRESULT;
begin
  Result := S_OK;
  State := FAudioState;
end;

procedure TAudioFilter.PCMWaveData(Buf: PByte; Length: Integer; P: PWaveFormatEx);
var
  I, J, K, B: Word;
begin
//  B := P.wBitsPerSample shr 2;
//  K := B shr 2;
//  for I := 0 to Length - 1 do
//  begin
//    if I mod B = 0 then
//    begin
//      for J := 0  to B shr 1 - 1 do
//      begin
//        PByte(Integer(Buf) + I + J)^ := PByte(Integer(Buf) + I * 2 + J div 2)^;
//      end;
//    end;
//  end;
//  Exit;  
  if (FAudioState = asAll) or (P.nChannels = 1) or (not P.wBitsPerSample in [8, 16, 24, 32]) then Exit;
  try    
    B := P.wBitsPerSample shr 2;
    K := B shr 2;
    for I := 0 to Length - 1 do
    begin
      if I mod B = 0 then
      begin
        for J := 0  to B shr 1 - 1 do
          if FAudioState = asLeft then
            PByte(Integer(Buf) + I + J + K + 1)^ := PByte(Integer(Buf) + I + J)^
          else
            PByte(Integer(Buf) + I + J)^ := PByte(Integer(Buf) + I + J + K + 1)^;
      end;
    end;
  except
  end;
end;

function TAudioFilter.put_State(State: TAudioState): HRESULT;
begin
  Result := S_OK;
  FAudioState := State;
end;

function TAudioFilter.Transform(Sample: IMediaSample): HRESULT;
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

