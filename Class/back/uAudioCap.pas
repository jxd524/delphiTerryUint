unit uAudioCap;

interface
uses
  Windows, Classes, SysUtils, Messages, DirectShow9, uDShowSub, ActiveX, DSUtil;

type
  TAudioCap = class(TComponent)
  private
    FGraphBuilder: IGraphBuilder;
    FAudioCapture: IBaseFilter;
    FAudioReadner: IBaseFilter;

    FAudioCapName: WideString;
    procedure SetAudioCapName(const Value: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UseDefaulAudioCap;
    procedure MyTest;
    procedure SetCapture;
    function CaptureEnable: Boolean;
  published
    property AudioCapName: WideString read FAudioCapName write SetAudioCapName;
  end;

implementation

{ TAudioCap }

procedure ClearList(AList: TList);
var
  i: Integer;
begin
  for i := AList.Count - 1 downto 0 do
  begin
    Dispose(AList[i]);
    AList.Delete(i);
  end;
end;

function TAudioCap.CaptureEnable: Boolean;
var
  pMix: IAMAudioInputMixer;
begin
  Result := False;
  if Succeeded(FAudioCapture.QueryInterface(IID_IAMAudioInputMixer, pMix)) then
  begin
    Result := Succeeded(pMix.put_Enable(True));
    pMix := nil;
  end;
end;

constructor TAudioCap.Create(AOwner: TComponent);
begin
  inherited;
  UseDefaulAudioCap;
end;

destructor TAudioCap.Destroy;
begin
  FGraphBuilder := nil;
  inherited;
end;

procedure TAudioCap.MyTest;
var
  pMediaCtrl: IMediaControl;
begin
  if Assigned(FGraphBuilder) then FGraphBuilder := nil;
  CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IGraphBuilder, FGraphBuilder);
  FGraphBuilder.AddFilter( FAudioCapture, 'Audio Capture' );
  AddFilterByGuid(FGraphBuilder, CLSID_AudioRender, 'Audio Readner', FAudioReadner);
  SetCapture;
  ConnectFilters(FGraphBuilder, FAudioCapture, FAudioReadner);
  FGraphBuilder.QueryInterface(IID_IMediaControl, pMediaCtrl);
  pMediaCtrl.Run;
end;

procedure TAudioCap.SetAudioCapName(const Value: WideString);
var
  lt: TList;
  i: Integer;
  bFind: Boolean;
begin
  lt := TList.Create;
  try
    bFind := False;
    GetHardwareFilterByCategories(CLSID_AudioInputDeviceCategory, lt);
    for i := 0 to lt.Count - 1 do
    begin
      if WideCompareText(pHandwareFilter(lt[i])^.FriendlyName, Value) = 0 then
      begin
        if Assigned(FAudioCapture) then FAudioCapture := nil;
        FAudioCapture := pHandwareFilter(lt[i])^.pFilter;
        FAudioCapName := pHandwareFilter(lt[i])^.FriendlyName;
        bFind := True;
        Break;
      end;
    end;

    if not bFind then
    begin
      for i := 0 to lt.Count - 1 do
      begin
        if Pos(Value, pHandwareFilter(lt[i])^.FriendlyName) > 0 then
        begin
          if Assigned(FAudioCapture) then FAudioCapture := nil;
          FAudioCapture := pHandwareFilter(lt[i])^.pFilter;
          FAudioCapName := pHandwareFilter(lt[i])^.FriendlyName;
          Break;
        end;
      end;
    end;

    for i := 0 to lt.Count - 1 do
      pHandwareFilter(lt[i])^.pFilter := nil;
  finally
    ClearList(lt);
    lt.Free;
  end;
end;

procedure TAudioCap.SetCapture;
var
  pNeg: IAMBufferNegotiation;
  prop: ALLOCATOR_PROPERTIES;
  pPin: IPin;
begin
  if Succeeded(GetUnConnectedPin(FAudioCapture, PINDIR_OUTPUT, pPin)) then
  begin
    if Succeeded(pPin.QueryInterface(IID_IAMBufferNegotiation, pNeg)) then
    begin
      prop.cBuffers := 4;
      prop.cbBuffer := 1024;
      prop.cbAlign := 1;
      prop.cbPrefix := 56;
      pNeg.SuggestAllocatorProperties(prop);
      pNeg := nil;
    end;
    pPin := nil;
  end;

//  prop.cBuffers := 6;
end;

procedure TAudioCap.UseDefaulAudioCap;
var
  lt: TList;
  i: Integer;
begin
  lt := TList.Create;
  try
    GetHardwareFilterByCategories(CLSID_AudioInputDeviceCategory, lt);
    if lt.Count = 0 then Exit;
    if Assigned(FAudioCapture) then FAudioCapture := nil;
    FAudioCapture := pHandwareFilter(lt[0])^.pFilter;
    FAudioCapName := pHandwareFilter(lt[0])^.FriendlyName;
    for i := 0 to lt.Count - 1 do
      pHandwareFilter(lt[i])^.pFilter := nil;
  finally
    ClearList(lt);
    lt.Free;
  end;
end;

end.
