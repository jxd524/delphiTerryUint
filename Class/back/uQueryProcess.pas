unit uQueryProcess;

interface
uses
  Windows, SysUtils, Classes, PsAPI, uProcessSub;

type
  pMemoryBlockInfo = ^TMemoryBlockInfo;
  TMemoryBlockInfo = record
    Address: Pointer;
    RegionSize: Cardinal;
    AllocationProtect: Cardinal;
    Protect: Cardinal;
    State: Cardinal;
    MemType: Cardinal;
  end;

  pMemoryAreaInfo = ^TMemoryAreaInfo;
  TMemoryAreaInfo = record
    BaseAddress: Pointer;             //基地址
    FileName: array[0..255] of Char;
    AreaRegionSize: Cardinal;         //范围
    BlockCount: Cardinal;             //此地址包含的区域块
    BlockInfos: array of pMemoryBlockInfo;
  end;

  {$M+}
  TQueryProcess = class
  private
    FMemList: TList;
    FProcessID: Cardinal;
    FProcessHandle: HWND;
    procedure SetProcessID(const Value: Cardinal);
    procedure ParseProcessMemory;
    class procedure ClearMemList(AList: TList);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   ReferProcessInfo;
    class procedure ZeroMemoryAreaInfo(p: pMemoryAreaInfo);
    class function  QueryProcessInfo(hProcess: HWND; lpBaseAddress: Pointer; pInfo: pMemoryAreaInfo): Pointer;
  published
    property ProcessHandle: HWND read FProcessHandle;
    property ProcessID: Cardinal read FProcessID write SetProcessID;
    property MemList: TList read FMemList;
  end;

  TProcessMemorySearch = class
  private
    FMemList: TList;
    FSearchResultList: TList;
    FProcessHandle: HWND;
    FProcessID: Cardinal;
    procedure SetProcessID(const Value: Cardinal);
    function CheckProcessMemory(lpBasicAddress: Pointer; nSize: Int64): Boolean;
    procedure SearchProcess(lpBuffer: Pointer; nBufferLen: Cardinal;
                            lpBasicAddress: Pointer; nRegionSize: Cardinal;
                            ASearchValue: Pointer; AValueSize: Cardinal);
    procedure MemorySearch(const pProcessBasicAddress, pMem, pValue: Pointer; const nMemSize, nValueSize: Cardinal);
  public
    constructor Create;
    destructor  Destroy; override;
    function SearchProcessMemory(lpBasicAddress: Pointer; nSize: Cardinal; ASearchValue: Pointer; AValueSize: Cardinal): Integer;
  published
    property ProcessHandle: HWND read FProcessHandle;
    property ProcessID: Cardinal read FProcessID write SetProcessID;
  end;
  {$M-}

function StateToString(AState: Cardinal): string;
function ProtectToString(AProtect: Cardinal): string;
function TypeToString(AType: Cardinal): string;

implementation

{ TQueryProcess }

class procedure TQueryProcess.ClearMemList(AList: TList);
var
  i: Integer;
  p: pMemoryAreaInfo;
begin
  for i := AList.Count - 1 downto 0 do
  begin
    p := AList[i];
    ZeroMemoryAreaInfo(p);
    Dispose(p);
    AList.Delete(i);
  end;
end;

constructor TQueryProcess.Create;
begin
  FProcessID := 0;
  FProcessHandle := 0;
  FMemList := TList.Create;
end;

destructor TQueryProcess.Destroy;
begin
  if FProcessHandle <> 0 then
    CloseHandle(FProcessHandle);
  ClearMemList(FMemList);
  FMemList.Free;
  inherited;
end;

procedure TQueryProcess.ParseProcessMemory;
var
  lpBaseAddress: pointer;
  p: pMemoryAreaInfo;
begin
  if FProcessHandle = 0 then
  begin
    FProcessID := 0;
    raise Exception.CreateFmt('无法打开进程: %d', [GetLastError]);
  end;
  lpBaseAddress := nil;
  while True do
  begin
    New(p);
    lpBaseAddress := QueryProcessInfo(FProcessHandle, lpBaseAddress, p);
    if lpBaseAddress = nil then
    begin
      Dispose(p);
      Break;
    end;
    FMemList.Add(p);
  end;
end;

class function TQueryProcess.QueryProcessInfo(hProcess: HWND; lpBaseAddress: Pointer; pInfo: pMemoryAreaInfo): Pointer;
var
  nSize: Cardinal;
  p: pMemoryBlockInfo;
  Info: TMemoryBasicInformation;
begin
  ZeroMemoryAreaInfo(pInfo);
  nSize := SizeOf(TMemoryBasicInformation);
  Result := nil;
  if nSize <> VirtualQueryEx(hProcess, lpBaseAddress, Info, nSize) then Exit;
  if Info.State = MEM_FREE then
    pInfo^.BaseAddress := Info.BaseAddress
  else
    pInfo^.BaseAddress := Info.AllocationBase;
  if Info.BaseAddress = Info.AllocationBase then
  begin
    if 0 = GetMappedFileName(hProcess, Info.BaseAddress, pInfo^.FileName, High(pInfo^.FileName)) then
      pInfo^.FileName := '';
  end;
  while True do
  begin
    Inc(pInfo^.BlockCount);
    Inc(pInfo^.AreaRegionSize, Info.RegionSize);
    SetLength(pInfo^.BlockInfos, pInfo^.BlockCount);
    New(p);
    p^.Address := Info.BaseAddress;
    p^.RegionSize := Info.RegionSize;
    p^.AllocationProtect := Info.AllocationProtect;
    p^.Protect := Info.Protect;
    p^.State := Info.State;
    p^.MemType := Info.Type_9;
    pInfo^.BlockInfos[ pInfo^.BlockCount - 1 ] := p;

    ZeroMemory(@Info, nSize);
    lpBaseAddress := PAnsiChar(lpBaseAddress) + p^.RegionSize;
    if nSize <> VirtualQueryEx(hProcess, lpBaseAddress, Info, nSize) then
    begin
      Result := PAnsiChar(pInfo^.BaseAddress) + pInfo^.AreaRegionSize;
      Break;
    end
    else
    begin
      if Info.AllocationBase <> pInfo^.BaseAddress then
      begin
        Result := PAnsiChar(lpBaseAddress);
        Break;
      end;
    end;
  end;
end;

procedure TQueryProcess.ReferProcessInfo;
begin
  ClearMemList(FMemList);
  ParseProcessMemory;
end;

procedure TQueryProcess.SetProcessID(const Value: Cardinal);
begin
  if FProcessID <> Value then
  begin
    ClearMemList(FMemList);
    FProcessID := Value;
    if FProcessHandle <> 0 then
      CloseHandle(FProcessHandle);
    FProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION, False, Value);
    ParseProcessMemory;
  end;
end;

class procedure TQueryProcess.ZeroMemoryAreaInfo(p: pMemoryAreaInfo);
begin
  if p <> nil then
  begin
    if High(p^.BlockInfos) > 0 then
      SetLength(p^.BlockInfos, 0);
    ZeroMemory(p, SizeOf(TMemoryAreaInfo));
  end;
end;

function StateToString(AState: Cardinal): string;
begin
  case AState of
    MEM_COMMIT:   Result := 'MEM_COMMIT';
    MEM_RESERVE:  Result := 'MEM_RESERVE';
    MEM_DECOMMIT: Result := 'MEM_DECOMMIT';
    MEM_RELEASE:  Result := 'MEM_RELEASE';
    MEM_FREE:     Result := 'MEM_FREE';
    MEM_PRIVATE:  Result := 'MEM_PRIVATE';
    MEM_MAPPED:   Result := 'MEM_MAPPED';
    MEM_RESET:    Result := 'MEM_RESET';
    MEM_IMAGE:    Result := 'MEM_IMAGE';
    else
      Result := 'Unkown ' + IntToHex(Integer(AState), 0);
  end;
end;
function ProtectToString(AProtect: Cardinal): string;
begin
  case AProtect of
    PAGE_NOACCESS:           Result := 'PAGE_NOACCESS';
    PAGE_READONLY:           Result := 'PAGE_READONLY';
    PAGE_READWRITE:          Result := 'PAGE_READWRITE';
    PAGE_WRITECOPY:          Result := 'PAGE_WRITECOPY';
    PAGE_EXECUTE:            Result := 'PAGE_EXECUTE';
    PAGE_EXECUTE_READ:       Result := 'PAGE_EXECUTE_READ';
    PAGE_EXECUTE_READWRITE:  Result := 'PAGE_EXECUTE_READWRITE';
    PAGE_EXECUTE_WRITECOPY:  Result := 'PAGE_EXECUTE_WRITECOPY';
    PAGE_GUARD:              Result := 'PAGE_GUARD';
    PAGE_NOCACHE:            Result := 'PAGE_NOCACHE';
    else
      Result := 'Unkown ' + IntToHex(Integer(AProtect), 0);
  end;
end;

function TypeToString(AType: Cardinal): string;
begin
  case AType of
    MEM_IMAGE:   Result := 'MEM_IMAGE';
    MEM_MAPPED:  Result := 'MEM_MAPPED';
    MEM_PRIVATE: Result := 'MEM_PRIVATE';
    else
      Result := 'Unkown ' + IntToHex(Integer(AType), 0);
  end;
end;


{ TProcessMemorySearch }

function TProcessMemorySearch.CheckProcessMemory(lpBasicAddress: Pointer; nSize: Int64): Boolean;
var
  lpAddress: Pointer;
  p: pMemoryAreaInfo;
  i, j: Integer;
  nLen: Cardinal;
begin
  TQueryProcess.ClearMemList(FMemList);
  lpAddress := lpBasicAddress;
  if nSize = 0 then nSize := $FFFFFFFF;
  while nSize > 0 do
  begin
    New(p);
    lpAddress :=  TQueryProcess.QueryProcessInfo(FProcessHandle, lpAddress, p);
    nLen := p^.AreaRegionSize;
    for i := High(p^.BlockInfos) downto Low(p^.BlockInfos) do
    begin
      if p^.BlockInfos[i]^.State = MEM_FREE then
       begin
         Dispose(p^.BlockInfos[i]);
         Dec(p^.AreaRegionSize, p^.BlockInfos[i]^.RegionSize);
         for j := i to High(p^.BlockInfos) - 1 do
           p^.BlockInfos[j] := p^.BlockInfos[j + 1];
         SetLength(p^.BlockInfos, Length(p^.BlockInfos) - 1);
       end;
    end;
    FMemList.Add(p);
    Dec(nSize, nLen);
    if lpAddress = nil then Break;
  end;
  Result := FMemList.Count > 0;
end;

constructor TProcessMemorySearch.Create;
begin
  FProcessHandle := 0;
  FProcessID := 0;
  FMemList := TList.Create;
  FSearchResultList := TList.Create;
end;

destructor TProcessMemorySearch.Destroy;
begin
  if FProcessHandle <> 0 then
    CloseHandle(FProcessHandle);
  TQueryProcess.ClearMemList(FMemList);
  FSearchResultList.Free;
  FMemList.Free;
  inherited;
end;

procedure TProcessMemorySearch.MemorySearch(const pProcessBasicAddress, pMem, pValue: Pointer; const nMemSize, nValueSize: Cardinal);
var
  p: Pointer;
  nSize: Int64;
begin
  p := pMem;
  nSize := nMemSize;
  while (nSize > 0) and (nSize >= nValueSize) do
  begin
    if CompareMem(p, pValue, nValueSize) then
    begin
      p := PAnsiChar(pProcessBasicAddress) + Integer(p) - Integer(pMem);
      OutputDebugString(PChar(IntToHex(Integer(p), 0)));
      FSearchResultList.Add(p);
    end;
    p := PAnsiChar(p) + 1;
    Dec(nSize, 1);
  end;
end;

procedure TProcessMemorySearch.SearchProcess(lpBuffer: Pointer; nBufferLen: Cardinal; lpBasicAddress: Pointer; nRegionSize: Cardinal;
  ASearchValue: Pointer; AValueSize: Cardinal);
var
  nLen: Int64;
  nMustReadByte: Int64;
  nReadByte, nSpace: Cardinal;
begin
  nMustReadByte := nRegionSize;
  nSpace := 0;//AValueSize - 1;
  if nBufferLen >= nMustReadByte then
  begin
    nLen := nMustReadByte;
  end
  else
    nLen := nBufferLen;
  while (nLen > 0) and (nMustReadByte > 0) and
         ReadProcessMemory(FProcessHandle, lpBasicAddress, lpBuffer, nLen, nReadByte) do
  begin
    MemorySearch(lpBasicAddress, lpBuffer, ASearchValue, nReadByte, AValueSize);
    Dec(nMustReadByte, nReadByte);
    if nMustReadByte <= 0 then Break;
    Inc(nMustReadByte, nSpace);
    if nBufferLen >= nMustReadByte then
    begin
      nLen := nMustReadByte;
    end
    else
      nLen := nBufferLen;
    lpBasicAddress := PAnsiChar(lpBasicAddress) + nReadByte - nSpace;
  end;
end;

function TProcessMemorySearch.SearchProcessMemory(lpBasicAddress: Pointer; nSize: Cardinal; ASearchValue: Pointer;
  AValueSize: Cardinal): Integer;
const
  CtMaxBufferSize = 65535; //$80000; //最大申请内存 521K
var
  lpBuffer: PAnsiChar;
  i, j: Integer;
  p: pMemoryAreaInfo;
  pBlock: pMemoryBlockInfo;
begin
  Result := -1;
  if (FProcessHandle = 0) or (not CheckProcessMemory(lpBasicAddress, nSize)) then Exit;
  GetMem(lpBuffer, CtMaxBufferSize);
  for i := 0 to FMemList.Count - 1 do
  begin
    p := FMemList[i];
    for j := Low(p^.BlockInfos) to High(p^.BlockInfos) do
    begin
      pBlock := p^.BlockInfos[j];
      ZeroMemory(lpBuffer, CtMaxBufferSize);
      SearchProcess(lpBuffer, CtMaxBufferSize, pBlock^.Address, pBlock^.RegionSize, ASearchValue, AValueSize);
    end;
  end;
  FreeMem(lpBuffer);
end;

procedure TProcessMemorySearch.SetProcessID(const Value: Cardinal);
begin
  if FProcessID <> Value then
  begin
    FProcessID := Value;
    if FProcessHandle <> 0 then
      CloseHandle(FProcessHandle);
    FProcessHandle := OpenProcess(PROCESS_ALL_ACCESS, False, FProcessID);
    if FProcessHandle = 0 then
      raise Exception.CreateFmt('无法打开进程: %d', [GetLastError]);
  end;
end;

end.
