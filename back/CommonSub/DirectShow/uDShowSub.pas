//Create By Terry at 2008-11-24
unit uDShowSub;

interface
uses
  Windows, SysUtils, Classes, DirectShow9, DSUtil, ActiveX, MMSystem, Registry;

type
  pHardwareFilter = ^THardwareFilter;
  THardwareFilter = record
    pFilter: IBaseFilter;
    FriendlyName: WideString;
  end;
  pPinInfo = ^TPinInfo;

  pFilterHeader = ^TFilterHeader;
  TFilterHeader = record
    dwVession:  DWORD;
    dwMerit:    DWORD;
    dwPinCount: DWORD;
    dwReserved: DWORD;
  end;

//Register
function IsFillterRegistered(AFilterGUID: TGUID): Boolean;
function RegisterFilter(const AFilterAx: string): Boolean;
function UnRegisterFilter(const AFilterAx: string): Boolean;

//Merit
function GetFilterInfo(AFilterGUID: TGUID; var AFilterInfo: TFilterHeader): Boolean;
function SetFilterMerit(AFilterGUID: TGUID; const AMerit: DWORD): Boolean;

//Grf
function SaveGraphToFile(pGraph: IGraphBuilder; const AFileName: WideString): HRESULT; // .grf
function LoadGraphFromFile(pGraph: IGraphBuilder; const AFileName: WideString): HRESULT;

//Graph Filiter Manage
function  GraphBuildToBuild2(pGraph: IGraphBuilder; var pBuild: ICaptureGraphBuilder2): HRESULT;
function  FindFilterInterface(pGraph: IGraphBuilder; const IID: TGUID; out Obj): HRESULT;
procedure RemoveAllFilter(pGraph: IGraphBuilder);

//Filter
function  GetFilterName(pFilter: IBaseFilter): string;
function  EnumFilters(pGraph: IFilterGraph; AFilterNameList: TStringList): HRESULT;
function  AddFilterByGuid(pGraph: IGraphBuilder; const AFilterGuid: TGUID; const AFilterName: WideString; var ppF: IBaseFilter): HRESULT;
function  ConnectFilters(pGraph: IGraphBuilder; pUpStreamFilter, pDownStreamFilter: IBaseFilter): HRESULT; overload;
function  ConnectFilters(pGraph: IGraphBuilder; pUpStreamOutPin: IPin; pDownStreamFilter: IBaseFilter): HRESULT; overload;


//Hardware Filter
//Audio: CLSID_AudioInputDeviceCategory
function  GetHardwareFilterByCategories(const GUID: TGUID; AList: TList): Boolean;
function  GetAudioInputFilterByName(var AFriendlyName: WideString): IBaseFilter;
function  GetAudioInputFilterByIndex(const AIndex: Integer; var AFriendlyName: WideString): IBaseFilter;
//通道为 APinFriendlyName 的为 AEnable; 其它的 not AEnable;
function  SetAudioPinEnable(AAudioFilter: IBaseFilter; const APinFriendlyName: WideString; const AEnable: Boolean): Boolean;
//只设置一个通道,其它不理
function  SetAudioPinEnableOnly(AAudioFilter: IBaseFilter; const APinFriendlyName: WideString; const AEnable: Boolean): Boolean;

function  GetVideoInputFilterByName(var AFriendlyName: WideString): IBaseFilter;
function  GetVideoInputFilterByIndex(const AIndex: Integer; var AFriendlyName: WideString): IBaseFilter;

//Pin
//FindConnectTo: 此Pin连接的Filter
function GetFilterByPin(pPin: IPin; var PinInfo: TPinInfo; FindConnectTo: Boolean = True): HRESULT;
function GetPin(pFilter: IBaseFilter; PinDir: PIN_DIRECTION; ASkipNum: UINT): IPin;
function GetUnConnectedPin(pFilter: IBaseFilter; PinDir: PIN_DIRECTION; var ppPin: IPin; ASkipNum: UINT = 0): HRESULT;
function GetAllPinInfo(pFilter: IBaseFilter; AList: TList): Integer;

//Vedia Reander
function InitWindowlessVMR(hwndApp: HWND; pGraph: IGraphBuilder; var ppWc: IVMRWindowlessControl): HRESULT;

//Media Seek
function IsMediaCanSeekAbsolute(pMediaSeek: IMediaSeeking): Boolean;

//Audio Capture
//设置: Stereo, 16, 441000HZ
function SetAudioCaptureDelayMillSecond(pAudioCap: IBaseFilter; nDelayMillSecond: Double = 0.05; IsStero: Boolean = True; nBytesPerSample: Integer = 2; nFrequency: Integer = 44100): HRESULT;

//WDM & VFW
function IsVFWCard(pDeviceFilter: IBaseFilter): Boolean;
function IsWDMCard(pDeviceFilter: IBaseFilter): Boolean;


procedure ClearList(AList: TList);

implementation

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

function IsFillterRegistered(AFilterGUID: TGUID): Boolean;
var
  pFilter: IBaseFilter;
  hr: HRESULT;
begin
  hr := CoCreateInstance( AFilterGUID, nil, CLSCTX_INPROC, IID_IBaseFilter, pFilter);
  Result := Succeeded(hr);
  if Result then
    pFilter := nil;
end;

function GetFilterInfo(AFilterGUID: TGUID; var AFilterInfo: TFilterHeader): Boolean;
const
  CtRegPath = 'CLSID\{083863F1-70DE-11d0-BD40-00A0C911CE86}\Instance\';
  CtFilterData = 'FilterData';
var
  strEntry: string;
  Reg: TRegistry;
  ArData: array[0..1024] of Byte;
begin
  Result := False;
  strEntry := CtRegPath + GUIDToString(AFilterGUID);
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if Reg.OpenKey(strEntry, False) then
    begin
      Result := True;
      ZeroMemory(@ArData, 1024);
      ZeroMemory(@AFilterInfo, SizeOf(TfilterHeader));
      Reg.ReadBinaryData(CtFilterData, ArData, 1024);
      Move(ArData, AFilterInfo, SizeOf(TFilterHeader));
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function SetFilterMerit(AFilterGUID: TGUID; const AMerit: DWORD): Boolean;
const
  CtRegPath = 'CLSID\{083863F1-70DE-11d0-BD40-00A0C911CE86}\Instance\';
  CtFilterData = 'FilterData';
var
  strEntry: string;
  Reg: TRegistry;
  ArData: array[0..1024] of Byte;
  nDataLen: Integer;
  pFilterInfo: pFilterHeader;
begin
  Result := False;
  strEntry := CtRegPath + GUIDToString(AFilterGUID);
  nDataLen := 1024;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if Reg.OpenKey(strEntry, False) then
    begin
      ZeroMemory(@ArData, nDataLen);
      nDataLen := Reg.ReadBinaryData(CtFilterData, ArData, nDataLen);
      pFilterInfo := @ArData;
      if pFilterInfo <> nil then
      begin
        pFilterInfo^.dwMerit := AMerit;
        Reg.WriteBinaryData(CtFilterData, ArData, nDataLen);
        Result := True;
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function RegAx(const Ax: string; AReg: Boolean): Boolean;
  type TDllRunSrv = function: HRESULT; stdcall;
var
  DllRunSrv: TDllRunSrv;
  hMd: HMODULE;
  FunName: string;
begin
  Result := False;
  hMd := LoadLibrary( pChar(Ax) );
  if hMd = 0 then Exit;
  try
    if AReg then
      FunName := 'DllRegisterServer'
    else
      FunName := 'DllUnregisterServer';
    DllRunSrv := TDllRunSrv( GetProcAddress(hMD, PChar(FunName)) );
    if Assigned(DllRunSrv) then
    begin
      DllRunSrv;
      Result := True;
    end;
  finally
    FreeLibrary(hMd);
  end;
end;

function RegisterFilter(const AFilterAx: string): Boolean;
begin
  Result := RegAx( AFilterAx, True );
end;

function UnRegisterFilter(const AFilterAx: string): Boolean;
begin
  Result := RegAx( AFilterAx, False );
end;

function SaveGraphToFile(pGraph: IGraphBuilder; const AFileName: WideString): HRESULT;
var
  pStorage: IStorage;
  pStream: IStream;
  pPersist: IPersistStream;
begin
  Result := StgCreateDocfile(PWideChar(AFileName), STGM_CREATE or STGM_TRANSACTED or STGM_READWRITE or STGM_SHARE_EXCLUSIVE, 0, pStorage);
  if Failed(Result) then Exit;
  Result := pStorage.CreateStream('ActiveMovieGraph', STGM_WRITE or STGM_CREATE or STGM_SHARE_EXCLUSIVE, 0, 0, pStream);
  if Failed(Result) then
  begin
    pStorage := nil;
    Exit;
  end;
  pGraph.QueryInterface(IID_IPersistStream, pPersist);
  Result := pPersist.Save(pStream, True);
  pStream := nil;
  pPersist := nil;
  if Succeeded(Result) then
    Result := pStorage.Commit(STGC_DEFAULT);
  pStorage := nil;
end;

function LoadGraphFromFile(pGraph: IGraphBuilder; const AFileName: WideString): HRESULT;
var
  pStorage: IStorage;
  pPersist: IPersistStream;
  pStream: IStream;
begin
  Result := E_FAIL;
  if S_OK <> StgIsStorageFile(pWideChar(AFileName)) then Exit;
  Result := StgOpenStorage(pWideChar(AFileName), nil, STGM_TRANSACTED or STGM_READ or STGM_SHARE_DENY_WRITE, nil, 0, pStorage);
  if Failed(Result) then Exit;
  Result := pGraph.QueryInterface(IID_IPersistStream, pPersist);
  if Succeeded(Result) then
  begin
    Result := pStorage.OpenStream('ActiveMovieGraph', nil, STGM_READ or STGM_SHARE_EXCLUSIVE, 0, pStream);
    if Succeeded(Result) then
    begin
      Result := pPersist.Load(pStream);
      pStream := nil;
    end;
    pPersist := nil;
  end;
  pStorage := nil;
end;

function GraphBuildToBuild2(pGraph: IGraphBuilder; var pBuild: ICaptureGraphBuilder2): HRESULT;
begin
  Result := CoCreateInstance(CLSID_CaptureGraphBuilder2, nil, CLSCTX_INPROC_SERVER, IID_ICaptureGraphBuilder2, pBuild);
  if Succeeded(Result) then
    pBuild.SetFiltergraph(pGraph);
end;

function AddFilterByGuid(pGraph: IGraphBuilder; const AFilterGuid: TGUID; const AFilterName: WideString; var ppF: IBaseFilter): HRESULT;
begin
  Result := E_POINTER;
  if not Assigned(pGraph) then Exit;
  Result := CoCreateInstance(AFilterGuid, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, ppF);
  if Succeeded(Result) then
    Result := pGraph.AddFilter(ppF, pWideChar(AFilterName));
  if Failed(Result) then
    ppF := nil;
end;

function InitWindowlessVMR(hwndApp: HWND; pGraph: IGraphBuilder; var ppWc: IVMRWindowlessControl): HRESULT;
var
  pVmr: IBaseFilter;
  pConfig: IVMRFilterConfig;
begin
  Result := E_POINTER;
  if not Assigned(pGraph) then Exit;
  Result := CoCreateInstance(CLSID_VideoMixingRenderer, nil, CLSCTX_INPROC, IID_IBaseFilter, pVmr);
  if Failed(Result) then Exit;
  Result := pGraph.AddFilter(pVmr, 'Video Mixing Renderer');
  if Failed(Result) then Exit;
  Result := pVmr.QueryInterface(IID_IVMRFilterConfig, pConfig);
  if Succeeded(Result) then
  begin
    Result := pConfig.SetRenderingMode(VMRMode_Windowless);
    pConfig := nil;
  end;
  if Succeeded(Result) then
    Result := pVmr.QueryInterface(IID_IVMRWindowlessControl, ppWc);
  pVmr := nil;
end;

function  GetFilterName(pFilter: IBaseFilter): string;
var
  FilterInfo: FILTER_INFO;
begin
  Result := '';
  if Succeeded(pFilter.QueryFilterInfo(FilterInfo)) then
  begin
    Result := FilterInfo.achName;
    if FilterInfo.pGraph <> nil then
      FilterInfo.pGraph := nil;
  end;
end;

function EnumFilters(pGraph: IFilterGraph; AFilterNameList: TStringList): HRESULT;
var
  pEnum: IEnumFilters;
  pFilter: IBaseFilter;
  cFetched: ULONG;
  FilterInfo: FILTER_INFO;
begin
  Result := pGraph.EnumFilters(pEnum);
  if Failed(Result) then Exit;
  while pEnum.Next(1, pFilter, @cFetched) = S_OK do
  begin
    Result := pFilter.QueryFilterInfo(FilterInfo);
    if Failed(Result) then Continue;
    AFilterNameList.Add(FilterInfo.achName);
    if FilterInfo.pGraph <> nil then
      FilterInfo.pGraph := nil;
    pFilter := nil;
  end;
  pEnum := nil;
end;

function GetPin(pFilter: IBaseFilter; PinDir: PIN_DIRECTION; ASkipNum: UINT): IPin;
var
  pEnum: IEnumPins;
  hr: HRESULT;
  CurPinDir: PIN_DIRECTION;
begin
  Result := nil;
  hr := pFilter.EnumPins(pEnum);
  if Failed(hr) then Exit;
  while pEnum.Next(1, Result, nil) = S_OK do
  begin
    Result.QueryDirection(CurPinDir);
    if CurPinDir = PinDir then
    begin
      if ASkipNum <= 0 then
      begin
        pEnum := nil;
        Break;
      end;
      Dec(ASkipNum);
    end;
    Result := nil;
  end;
  pEnum := nil;
end;

function GetAllPinInfo(pFilter: IBaseFilter; AList: TList): Integer;
var
  pEnum: IEnumPins;
  pPin: IPin;
  hr: HRESULT;
  p: pPinInfo;
  PinInfo: TPinInfo;
begin
  Result := 0;
  pPin := nil;

  if not Assigned(AList) then Exit;
  hr := pFilter.EnumPins(pEnum);
  if Failed(hr) then Exit;

  while pEnum.Next(1, pPin, nil) = S_OK do
  begin
    if S_OK = pPin.QueryPinInfo(PinInfo) then
    begin
      New(p);
      Move(PinInfo, p^, sizeof(TPinInfo));
      AList.Add(p);
//      p^.pFilter._AddRef;
      Inc(Result);
    end;
    pPin := nil;
  end;
  pEnum := nil;
end;

function GetFilterByPin(pPin: IPin; var PinInfo: TPinInfo; FindConnectTo: Boolean): HRESULT;
var
  pConnectToPin: IPin;
begin
  Result := E_FAIL;
  if not Assigned(pPin) then Exit;
  if not FindConnectTo then
    Result := pPin.QueryPinInfo(PinInfo) //查询自身所属 Filter
  else
  begin
    Result := pPin.ConnectedTo(pConnectToPin);
    if Succeeded(Result) then
    begin
      Result := pConnectToPin.QueryPinInfo(PinInfo);
      pConnectToPin := nil;
    end;
  end;
end;

function GetUnConnectedPin(pFilter: IBaseFilter; PinDir: PIN_DIRECTION; var ppPin: IPin; ASkipNum: UINT): HRESULT;
var
  pEnum: IEnumPins;
  pTest: IPin;
  CurPinDir: PIN_DIRECTION;
begin
  ppPin := nil;
  Result := pFilter.EnumPins(pEnum);
  if Failed(Result) then Exit;
  while pEnum.Next(1, ppPin, nil) = S_OK  do
  begin
    ppPin.QueryDirection(CurPinDir);
    if CurPinDir <> PinDir then
    begin
      ppPin := nil;
      Continue;
    end;
    pTest := nil;
    if Failed(ppPin.ConnectedTo(pTest)) then
    begin
      if ASkipNum <= 0 then
      begin
        Result := S_OK;
        Break;
      end;
      Dec(ASkipNum);
    end
    else
    begin
      pTest := nil;
      ppPin := nil;
      Result := E_FAIL;
    end;
  end;
  pEnum := nil;
end;

function ConnectFilters(pGraph: IGraphBuilder; pUpStreamFilter, pDownStreamFilter: IBaseFilter): HRESULT;
var
  pUpStreamOutPin: IPin;
begin
  Result := GetUnConnectedPin(pUpStreamFilter, PINDIR_OUTPUT, pUpStreamOutPin);
  if Failed(Result) then Exit;
  Result := ConnectFilters(pGraph, pUpStreamOutPin, pDownStreamFilter);
end;

function ConnectFilters(pGraph: IGraphBuilder; pUpStreamOutPin: IPin; pDownStreamFilter: IBaseFilter): HRESULT;
var
  pDownStreamInPin: IPin;
begin
  Result := E_FAIL;
  if (not Assigned(pGraph)) or (not Assigned(pUpStreamOutPin)) or (not Assigned(pDownStreamFilter)) then Exit;
  Result := GetUnConnectedPin(pDownStreamFilter, PINDIR_INPUT, pDownStreamInPin);
  if Failed(Result) then Exit;
  Result := pGraph.Connect(pUpStreamOutPin, pDownStreamInPin);
  pDownStreamInPin := nil;
end;

function FindFilterInterface(pGraph: IGraphBuilder; const IID: TGUID; out Obj): HRESULT;
var
  pEnum: IEnumFilters;
  pFilter: IBaseFilter;
begin
  Result := E_FAIL;
  if Failed(pGraph.EnumFilters(pEnum)) then Exit;
  while S_OK = pEnum.Next(1, pFilter, nil) do
  begin
    Result := pFilter.QueryInterface(IID, Obj);
    pFilter := nil;
    if Succeeded(Result) then Break;
  end;
  pEnum := nil;
end;

procedure RemoveAllFilter(pGraph: IGraphBuilder);
var
  pEnum: IEnumFilters;
  pFilter: IBaseFilter;
begin
  if (not Assigned(pGraph)) or Failed(pGraph.EnumFilters(pEnum)) then Exit;
  while S_OK = pEnum.Next(1, pFilter, nil)  do
  begin
    pGraph.RemoveFilter(pFilter);
    pFilter := nil;
    pEnum.Reset;
  end;
  pEnum := nil;
end;

function IsMediaCanSeekAbsolute(pMediaSeek: IMediaSeeking): Boolean;
var
  dwCap: DWORD;
begin
  Result := Succeeded(pMediaSeek.GetCapabilities(dwCap)) and (AM_SEEKING_CanSeekAbsolute and dwCap = 1);
end;

function GetHardwareFilterByCategories(const GUID: TGUID; AList: TList): Boolean;
var
  SysEnum: TSysDevEnum;
  i, nCount: Integer;
  p: pHardwareFilter;
begin
  SysEnum := TSysDevEnum.Create(GUID);
  try
    nCount := SysEnum.CountFilters;
    Result := nCount > 0;
    for i := 0 to nCount - 1 do
    begin
      New(p);
      p^.FriendlyName := SysEnum.Filters[i].FriendlyName;
      p^.pFilter := SysEnum.GetBaseFilter(i);
      AList.Add(p);
    end;
  finally
    SysEnum.Free;
  end;
end;

function FindFilterFromList(var AFriendlyName: WideString; AList: TList): IBaseFilter;
var
  i: Integer;
  bFind: Boolean;
begin
  Result := nil;
  bFind := False;
  for i := 0 to AList.Count - 1 do
  begin
    if WideCompareText(AFriendlyName, pHardwareFilter(AList[i])^.FriendlyName) = 0 then
    begin
      Result := pHardwareFilter(AList[i])^.pFilter;
      Result._AddRef;
      bFind := True;
      Break;
    end;
  end;

  if not bFind then
  begin
    for i := 0 to AList.Count - 1 do
    begin
      if Pos(AFriendlyName, pHardwareFilter(AList[i])^.FriendlyName) > 0 then
      begin
        Result := pHardwareFilter(AList[i])^.pFilter;
        AFriendlyName := pHardwareFilter(AList[i])^.FriendlyName;
        Result._AddRef;
        Break;
      end;
    end;
  end;
  for i := 0 to AList.Count - 1 do
    pHardwareFilter(AList[i])^.pFilter := nil;
end;

function GetAudioInputFilterByName(var AFriendlyName: WideString): IBaseFilter;
var
  lt: TList;
begin
  Result := nil;
  lt := TList.Create;
  try
    GetHardwareFilterByCategories(CLSID_AudioInputDeviceCategory, lt);
    Result := FindFilterFromList(AFriendlyName, lt);
  finally
    ClearList(lt);
    lt.Free;
  end;
end;

function  GetAudioInputFilterByIndex(const AIndex: Integer; var AFriendlyName: WideString): IBaseFilter;
var
  lt: TList;
begin
  Result := nil;
  AFriendlyName := '';
  lt := TList.Create;
  try
    GetHardwareFilterByCategories(CLSID_AudioInputDeviceCategory, lt);
    if (AIndex >= 0) and (AIndex < lt.Count) then
    begin
      Result := pHardwareFilter(lt[AIndex])^.pFilter;
      AFriendlyName := pHardwareFilter(lt[AIndex])^.FriendlyName;
    end;
  finally
    ClearList(lt);
    lt.Free;
  end;
end;

function  SetAudioPinEnableOnly(AAudioFilter: IBaseFilter; const APinFriendlyName: WideString; const AEnable: Boolean): Boolean;
var
  pEnumPin: IEnumPins;
  pPin: IPin;
  PinInfo: TPinInfo;
  pMixer: IAMAudioInputMixer;
begin
  Result := False;
  if (not Assigned(AAudioFilter)) or Failed(AAudioFilter.EnumPins(pEnumPin)) then Exit;
  while S_OK = pEnumPin.Next(1, pPin, nil) do
  begin
    pPin.QueryPinInfo(PinInfo);
    if WideCompareText(PinInfo.achName, APinFriendlyName) = 0 then
    begin
      if Succeeded(pPin.QueryInterface(IID_IAMAudioInputMixer, pMixer)) then
      begin
        Result := True;
        pMixer.put_Enable(AEnable);
        pMixer := nil;
        Break;
      end;
    end;
    pPin := nil;
  end;
  pEnumPin := nil;
end;

function  SetAudioPinEnable(AAudioFilter: IBaseFilter; const APinFriendlyName: WideString; const AEnable: Boolean): Boolean;
var
  pEnumPin: IEnumPins;
  pPin: IPin;
  PinInfo: TPinInfo;
  pMixer: IAMAudioInputMixer;
begin
  Result := False;
  if (not Assigned(AAudioFilter)) or Failed(AAudioFilter.EnumPins(pEnumPin)) then Exit;
  while S_OK = pEnumPin.Next(1, pPin, nil) do
  begin
    pPin.QueryPinInfo(PinInfo);
    if WideCompareText(PinInfo.achName, APinFriendlyName) = 0 then
    begin
      if Succeeded(pPin.QueryInterface(IID_IAMAudioInputMixer, pMixer)) then
      begin
        Result := True;
        pMixer.put_Enable(AEnable);
        pMixer := nil;
      end
    end
    else
    begin
      if Succeeded(pPin.QueryInterface(IID_IAMAudioInputMixer, pMixer)) then
      begin
        pMixer.put_Enable(not AEnable);
        pMixer := nil;
      end
    end;
    pPin := nil;
  end;
end;

function  GetVideoInputFilterByName(var AFriendlyName: WideString): IBaseFilter;
var
  lt: TList;
begin
  Result := nil;
  lt := TList.Create;
  try
    GetHardwareFilterByCategories(CLSID_VideoInputDeviceCategory, lt);
    Result := FindFilterFromList(AFriendlyName, lt);
  finally
    ClearList(lt);
    lt.Free;
  end;
end;

function  GetVideoInputFilterByIndex(const AIndex: Integer; var AFriendlyName: WideString): IBaseFilter;
var
  lt: TList;
begin
  Result := nil;
  lt := TList.Create;
  try
    GetHardwareFilterByCategories(CLSID_VideoInputDeviceCategory, lt);
    if (AIndex >= 0) and (AIndex < lt.Count) then
    begin
      Result := pHardwareFilter(lt[AIndex])^.pFilter;
      AFriendlyName := pHardwareFilter(lt[AIndex])^.FriendlyName;
    end;
  finally
    ClearList(lt);
    lt.Free;
  end;
end;

function SetAudioCaptureDelayMillSecond(pAudioCap: IBaseFilter; nDelayMillSecond: Double; IsStero: Boolean; nBytesPerSample: Integer; nFrequency: Integer): HRESULT;
var
  pNeg: IAMBufferNegotiation;
  pCfg: IAMStreamConfig;
  prop: ALLOCATOR_PROPERTIES;
  pPin: IPin;
  i: Integer;
  lBufferSize, lBytesPerSecond: Longint;
  nChannels: Integer;
  pmt: PAMMediaType;
  pW: pWaveFormatEx;
begin
  Result := E_FAIL;
  if IsStero then
    nChannels := 2
  else
    nChannels := 1;

  lBufferSize := Round(nFrequency * nDelayMillSecond); //0.05: DEFAULT_BUFFER_TIME
  lBytesPerSecond := nBytesPerSample * nFrequency * nChannels;
  for i := 0 to 1 do
  begin
    pPin := GetPin(pAudioCap, PINDIR_OUTPUT, i);
    if not Assigned(pPin) then
    begin
      pPin := nil;
      Break;
    end;
    if Succeeded(pPin.QueryInterface(IID_IAMBufferNegotiation, pNeg)) then
    begin
      prop.cBuffers := 8;
      prop.cbBuffer := lBufferSize;
      prop.cbAlign := nBytesPerSample * nChannels;
      prop.cbPrefix := 1;
      pNeg.SuggestAllocatorProperties(prop);
      pNeg := nil;
    end;

    Result := pPin.QueryInterface(IID_IAMStreamConfig, pCfg);
    if Failed(Result) then
    begin
      pPin := nil;
      Break;
    end;
    Result := pCfg.GetFormat(pmt);
    if Succeeded(Result) then
    begin
      pw := pWaveFormatEx(pmt.pbFormat);
      pw.nChannels := nChannels;
      pw.nSamplesPerSec := nFrequency;
      pw.nAvgBytesPerSec := lBytesPerSecond;
      pw.wBitsPerSample := nBytesPerSample * 8;
      pw.nBlockAlign := nBytesPerSample * nChannels;
      Result := pCfg.SetFormat(pmt^);
      DeleteMediaType(pmt);
    end;

    pCfg := nil;
    pPin := nil;
  end;
end;

function IsVFWCard(pDeviceFilter: IBaseFilter): Boolean;
var
  pVfw: IAMVfwCaptureDialogs;
begin
  Result := Succeeded(pDeviceFilter.QueryInterface(IID_IAMVfwCaptureDialogs, pVfw));
  if Result then
    pVfw := nil;
end;

function IsWDMCard(pDeviceFilter: IBaseFilter): Boolean;
var
  pWdm: IAMAnalogVideoDecoder;
begin
  Result := Succeeded(pDeviceFilter.QueryInterface(IID_IAMAnalogVideoDecoder, pWdm));
  if Result then
    pWdm := nil;
end;

end.
