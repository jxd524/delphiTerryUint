unit uProcessSub;

interface
  uses
    Windows, Messages, SysUtils, Classes, CommCtrl, TLHelp32, Psapi, ShlObj, WinInet;

  type
    pProcessInfo = ^TProcessInfo;
    TProcessInfo = record
      FileName: string;
      WindowHandle: HWND;
      ProcessInfo: TProcessEntry32;
    end;

  function GetRunningProcessInfo(AList: TList): Integer;
  function GetRunningProcessName(AList: TStringList): Integer;
  function IsProcessRun(const AFileName: string): Boolean;
  function GetFileNameByProcessID(const AProcessID: DWORD): string;
  function GetFileNameByWindowHandle(const AWinHandle: HWND): string;
  function WinHandleToProcessID(AWinHandle: HWND): Cardinal;
  function ProcessIDToWinHandle(AProcessID: Cardinal): HWND;
  function APPTitleToProcessID(AAPPTitle: string): Cardinal;

implementation

function WinHandleToProcessID(AWinHandle: HWND): Cardinal;
begin
  GetWindowThreadProcessId(AWinHandle, Result);
end;

type
  pEnumParam = ^TEnumParam;
  TEnumParam = record
    WParam: Integer;
    LParam: Pointer;
  end;

function EnumWindowsProc(AHandle: HWND; LParam: Integer): Boolean; stdcall;
var
  nID: Integer;
  pParam: pEnumParam;
begin
  Result := True;
  pParam := pEnumParam(LParam);
  nID := WinHandleToProcessID(AHandle);
  if nID = pParam^.WParam then
  begin
    Move(AHandle, pParam^.LParam^, SizeOf(HWND));
    Result := False;
  end;
end;
function ProcessIDToWinHandle(AProcessID: Cardinal): HWND;
var
  p: pEnumParam;
begin
  New(p);
  Result := 0;
  p^.WParam := AProcessID;
  p^.LParam := @Result;
  EnumWindows(@EnumWindowsProc, Integer(p));
end;

function APPTitleToProcessID(AAPPTitle: string): Cardinal;
begin
  Result := WinHandleToProcessID(FindWindow(nil, PChar(AAPPTitle)));
end;

function GetFileNameByWindowHandle(const AWinHandle: HWND): string;
var
  dwProcessID: DWORD;
begin
  GetWindowThreadProcessId(AWinHandle, dwProcessID);
  Result := GetFileNameByProcessID(dwProcessID);
end;

function GetFileNameByProcessID(const AProcessID: DWORD): string;
var
  hProcessHandle: HWND;
  cPath: array[0..MAX_PATH] of char;
  hProcessModule: HMODULE;

  dwModulCount: DWORD;
begin
  Result := '';
  hProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, AProcessID);
  try
    EnumProcessModules(hProcessHandle, @hProcessModule, sizeof(hProcessModule), dwModulCount);
    if 0 <> GetModuleFileNameEx(hProcessHandle, hProcessModule, cPath, MAX_PATH) then
      Result := StrPas(cPath);
  finally
    CloseHandle(hProcessHandle);
  end;
end;

function GetRunningProcessInfo(AList: TList): Integer;
var
  lppe: TProcessEntry32;
  p: pProcessInfo;
  Hand: HWND;
  bOK: Boolean;
  nSize: Integer;
begin
  Result := 0;
  nSize := sizeof(TProcessEntry32);
  Hand := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);
  try
    lppe.dwSize := nSize;
    bOK := Process32First(Hand, lppe);
    while bOK do
    begin
      New(p);
      p^.FileName := GetFileNameByProcessID(lppe.th32ProcessID);
      p^.WindowHandle := ProcessIDToWinHandle(lppe.th32ProcessID);
      if p^.FileName = '' then
        p^.FileName := StrPas(lppe.szExeFile);
      Move(lppe, p^.ProcessInfo, nSize);
      AList.Add(p);
      Inc(Result);
      bOK := Process32Next(Hand, lppe);
    end;
  finally
    CloseHandle(Hand)
  end;
end;

function GetRunningProcessName(AList: TStringList): Integer;
var
  lppe: TProcessEntry32;
  Hand: HWND;
  bOK: Boolean;
  strFilePath: string;
  nCount: Integer;
begin
  Result := 0;
  nCount := sizeof(TProcessEntry32);
  Hand := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);
  try
    lppe.dwSize := nCount;
    bOK := Process32First(Hand, lppe);
    while bOK do
    begin
      strFilePath := GetFileNameByProcessID(lppe.th32ProcessID);
      AList.Add(strFilePath);
      Inc(Result);
      bOK := Process32Next(Hand, lppe);
    end;
  finally
    CloseHandle(Hand)
  end;
end;

function IsProcessRun(const AFileName: string): Boolean;
var
  lppe: TProcessEntry32;
  found: Boolean;
  Hand: HWND;
  strFilePath: string;
  nCount: Integer;
begin
  Result := False;
  nCount := sizeof(TProcessEntry32);
  Hand := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);
  try
    lppe.dwSize := nCount;
    found := Process32First(Hand, lppe);
    while found do
    begin
      strFilePath := StrPas(lppe.szExeFile);
      if (CompareText(strFilePath, AFileName) = 0) or (CompareText(ExtractFileName(strFilePath), AFileName) = 0) then
      begin
        Result := True;
        Break;
      end;
      found := Process32Next(Hand, lppe);
    end;
  finally
    CloseHandle(Hand)
  end;
end;
end.
