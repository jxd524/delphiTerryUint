unit uSysSub;

interface
  uses
    Windows, Messages, SysUtils, Classes, CommCtrl, TLHelp32, Psapi, ShlObj, WinInet;
  
  function FindWindowHandle(const AClassName, ACaption: string): HWND;
  function FindWindowHandleEx(const AParentHandle, AChildAfterHandle: HWND; const AClassName, ACaption: string): HWND;

  //一层一层查找
  function FindSpectifyHandle(const ABeginHandle: HWND; const AIsClassName: Boolean; const ASignNames: array of string): HWND; overload;
  function FindSpectifyHandle(const ABeginHandle: HWND; const AChildClassNames, AChildCaption: array of string): HWND; overload;
  function FindSpectifyHandle(const ABeginAppClassName, ABeginAppCaption: string; const AChildClassNames, AChildCaption: array of string): HWND; overload;
  function FindSpectifyHandle(const ABeginHandle: HWND; const AClassNameSign: array of string): HWND; overload;
  function FindHandle(const AParent: HWND; const AControlIndex: Integer; const AControlClassName: string): HWND;
  //在同一层查找
  function FindSpectifyHandleEx(const ABeginHandle: HWND; const ASign: array of string; const IsClassSign: Boolean = True): HWND;

  function GetWindowText(const AHandle: HWND): string;
  function GetWindowTextEx(const AHandle: HWND): string;
  function GetClassName(const AHandle: HWND): string;
  procedure SelfSleep(AMilliSecond: Integer);
  procedure ForceCloseWindow(AHandle: HWND);

  function GetTempPath: string;
  function GetRandomTempFileName(const AExtentName: string = 'tmp'): string;

  //Control operation
  procedure WMSetText(const AHandle: HWND; const AText: string); inline;
  function GetListBoxText(mHandle: THandle; mStrings: TStrings): Boolean;
  function GetComboBoxText(const AComboBoxHandle: HWND; const AItemIndex: Integer): string;
  procedure SetComboBoxText(const AComboBoxHandle: HWND; const AText: string); inline;
  function GetComboBoxItemCount(const AComboBoxHandle: HWND): Integer; inline;
  procedure SetComboBoxIndex(const AComboBoxHandle: HWND; const AIndex: Integer); inline;
  //ListView
  function GetListViewItemText(const AListViewHandle: HWND; const ARowIndex, AColIndex: Integer; const AMaxBufSize: Integer = 256): string;
  //
  procedure SetButtonCheckState(const AButtonHandle: HWND; const ACheck: Boolean);

  function GetTempDirectory: string;
  function GetWindowsDir: string;
  function GetSysDir: string;
  procedure GetHardDesks(AList: TStrings);

  function FilterUserHalt: Boolean; //是否有人在调试
  function ExtractRes(ResType, ResName, ResNewName: string): boolean; //释放资源
  procedure ExceCmdByBat(const AComandMsg: string);  //利用BAT执行CMD命令

  function GetFileNameByProcessID(const AProcessID: DWORD): string;  //由进程ID得到进程路径
  function GetFileNameByWindowHandle(const AWinHandle: HWND): string; //由窗口句柄得到进程路径

  
  function IsProcessRun(const AFileName: string): Boolean; //判断某个进程是否在运行
  procedure InputMsgByKeyboard(const AText: string; const AHandle: HWND = 0); //拥有焦点的窗口得到输入文本
  function IsCapsLockOn: Boolean;  //Caps Lock 是否有按下
  //弹出确定窗口
  function ShowMsg(const AText: string; const ACaption: string = '提示'; const AHandle: HWND = 0; const AType: UINT = MB_OK or MB_ICONINFORMATION): Integer;

  function SelectDirectory(Ahandle: HWND; const Caption: string; const Root: WideString; out Directory: string): Boolean;
  function DeleteFolder(APath: String): Boolean; //删除整个目录(小心使用)
  procedure ExploreFile(const AFileName: string);
  function Regsvr32(AFileName: string;AInstall: boolean=true):Cardinal;

  function GetCacheVerifyCodeFile(VerifyCodeURL: String; Var CacheVerifyCodeFile: String): Boolean; //返回指定URL在缓冲区的本地文件路径

  function GetFileSizeEx(const AFileName: string): Cardinal;
  function GetOSVer: Word; //获取操作系统版本号
implementation

uses uStringHandle, uRandomInfo, uConversion;

type
  TOSVersion = (osUnknown=1, os9x=2, os2k=4, osXp=8, osXp64=16, os2003=32,
    os200364=64, osVista=128, osVista64=256, os2008=512, os200864=1024);
  POSVersionInfoEx = ^TOSVersionInfoEx;
  TOSVersionInfoEx = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of AnsiChar; { Maintenance AnsiString for PSS usage }
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;

const
  VER_NT_WORKSTATION = 1;
  PROCESSOR_ARCHITECTURE_INTEL = 0;
  PROCESSOR_ARCHITECTURE_MIPS = 1;
  PROCESSOR_ARCHITECTURE_ALPHA = 2;
  PROCESSOR_ARCHITECTURE_PPC = 3;
  PROCESSOR_ARCHITECTURE_SHX = 4;
  PROCESSOR_ARCHITECTURE_ARM = 5;
  PROCESSOR_ARCHITECTURE_IA64 = 6;
  PROCESSOR_ARCHITECTURE_ALPHA64 = 7;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 = 10;
  PROCESSOR_ARCHITECTURE_UNKNOWN = $FFFF;
function MyGetVersionEx(pos: POSVersionInfoEx): Boolean; stdcall; external 'kernel32.dll' name 'GetVersionExA';

function FilterUserHalt: Boolean;
begin
  Result := True;
  asm
    mov eax, large fs:18h
    mov eax, [eax+30h]
    movzx   eax, byte ptr [eax+2]
    retn
  end;
end;

procedure ExceCmdByBat(const AComandMsg: string);
var
  st: TStringList;
  strFileName: string;
begin
  st := TStringList.Create;
  try
    strFileName := GetRandomTempFileName('bat');
    st.Add(AComandMsg);
    st.SaveToFile(strFileName);
    WinExec(pchar(strFileName), SW_HIDE);
  finally
    FreeAndNil(st);
  end;
end;

function ExtractRes(ResType, ResName, ResNewName: string): boolean;
var
  Res: TResourceStream;
begin
  try
    Res := TResourceStream.Create(Hinstance, Resname, Pchar(ResType));
    try
      Res.SavetoFile(ResNewName);
      Result := true;
    finally
      Res.Free;
    end;
  except
    Result := false;
  end;
end;

function GetTempDirectory: string;
var
  TempDir: array[0..255] of Char;
begin
  Windows.GetTempPath(255, @TempDir);
  Result := StrPas(TempDir);
end;

function GetSysDir: string;
var
  ac: array[1..MAX_PATH] of Char;
  pc: PChar;
begin
  pc := @ac;
  GetSystemDirectory(pc, MAX_PATH);
  Result := pc;
end;


function GetWindowsDir: string;
var
  ac: array[1..MAX_PATH] of Char;
  pc: PChar;
begin
  pc := @ac;
  GetWindowsDirectory(pc, MAX_PATH);
  Result := pc;
end;

procedure GetHardDesks(AList: TStrings);
var
  I: Integer;
  drive: string;
begin
  if AList = nil then
    AList := TStringList.Create;
  AList.Clear;
  for I := 97 to 122 do
  begin
    drive := Chr(i) + ':\';
    if GetDriveType(PChar(drive)) = DRIVE_FIXED then
      AList.Add(drive);
  end;
end;

function GetWindowText(const AHandle: HWND): string;
var
  nLen: Integer;
  pBuf: PChar;
begin
  Result := '';
  nLen := GetWindowTextLength(AHandle);
  if nLen <= 0 then
    Exit;
  GetMem(pBuf, nLen + 1);
  try
    Windows.GetWindowText(AHandle, pBuf, nLen + 1);
    Result := pBuf;
  finally
    FreeMem(pBuf);
  end;
end;

function GetWindowTextEx(const AHandle: HWND): string;
var
  buf: PChar;
  nLen: Integer;
begin
  nLen := SendMessage(AHandle, WM_GETTEXTLENGTH, 0, 0);
  if nLen <= 0 then
    Exit;
  GetMem(buf, nLen + 1);
  try
    SendMessage(AHandle, WM_GETTEXT, nLen + 1, Integer(buf));
    Result := buf;
  finally
    FreeMem(buf);
  end;
end;


function GetClassName(const AHandle: HWND): string;
var
  pBuf: PChar;
begin
  Result := '';
  GetMem(pBuf, 255);
  try
    if Windows.GetClassName(AHandle, pBuf, 255) <> 0 then
      Result := pBuf;
  finally
    FreeMem(pBuf);
  end;
end;

function FindSpectifyHandle(const ABeginHandle: HWND; const AIsClassName: Boolean; const ASignNames: array of string): HWND;
var
  AryOtherSign: array of string;
begin
  Setlength(AryOtherSign, Length(ASignNames));
  if AIsClassName then
    Result := FindSpectifyHandle(ABeginHandle, ASignNames, AryOtherSign)
  else
    Result := FindSpectifyHandle(ABeginHandle, AryOtherSign, ASignNames);
end;

function FindSpectifyHandle(const ABeginHandle: HWND; const AChildClassNames, AChildCaption: array of string): HWND;
var
  iLoop: Integer;
  hPreParent, hCurParent, hPreChild, hCurChild: HWND;
begin
  hPreParent := ABeginHandle;
  hCurParent := hPreParent;
  hPreChild := 0;
  iLoop := 0;
  Result := 0;
  while iLoop <= High(AChildClassNames) do
  begin
    hCurChild := FindWindowHandleEx(hCurParent, hPreChild, AChildClassNames[iLoop], AChildCaption[iLoop]);
    if hCurChild = 0 then
    begin
      Dec(iLoop);
      if iLoop < 0 then
      begin
        Result := 0;
        Break;
      end;
      hPreChild := hCurParent;
      hCurParent := hPreParent;
    end
    else
    begin
      inc(iLoop);
      Result := hCurChild;
      hPreParent := hCurParent;
      hCurParent := hCurChild;
      hPreChild := 0;
    end;
  end;
end;

function FindWindowHandleEx(const AParentHandle, AChildAfterHandle: HWND; const AClassName, ACaption: string): HWND;
begin
  if (AClassName = '') and (ACaption = '') then
    Result := 0
  else if (AClassName = '') and (ACaption <> '') then
    Result := FindWindowEx(AParentHandle, AChildAfterHandle, nil, PChar(ACaption))
  else if (AClassName <> '') and (ACaption = '') then
    Result := FindWindowEx(AParentHandle, AChildAfterHandle, PChar(AClassName), nil)
  else
    Result := FindWindowEx(AParentHandle, AChildAfterHandle, PChar(AClassName), PChar(ACaption));
end;

function FindWindowHandle(const AClassName, ACaption: string): HWND;
begin
  if (AClassName = '') and (ACaption = '') then
    Result := 0
  else if (AClassName = '') and (ACaption <> '') then
    Result := FindWindow(nil, PChar(ACaption))
  else if (AClassName <> '') and (ACaption = '') then
    Result := FindWindow(PChar(AClassName), nil)
  else
    Result := FindWindow(PChar(AClassName), PChar(ACaption));
end;

function FindSpectifyHandle(const ABeginAppClassName, ABeginAppCaption: string; const AChildClassNames, AChildCaption: array of string): HWND;
var
  iLoop: Integer;
  hPreParent, hCurParent, hPreChild, hCurChild: HWND;
begin
  hPreParent := FindWindowHandle(ABeginAppClassName, ABeginAppCaption);
  if hPreParent = 0 then
  begin
    Result := 0;
    Exit;
  end;
  hCurParent := hPreParent;
  hPreChild := 0;
  iLoop := 0;
  Result := hPreParent;
  while iLoop <= High(AChildClassNames) do
  begin
    hCurChild := FindWindowHandleEx(hCurParent, hPreChild, AChildClassNames[iLoop], AChildCaption[iLoop]);
    if hCurChild = 0 then
    begin
      Dec(iLoop);
      if iLoop < Low(AChildClassNames) then
      begin
        Result := 0;
        Break;
      end;
      hPreChild := hCurParent;
      hCurParent := hPreParent;
    end
    else
    begin
      inc(iLoop);
      Result := hCurChild;
      hPreParent := hCurParent;
      hCurParent := hCurChild;
      hPreChild := 0;
    end;
  end;
end;

procedure SelfSleep(AMilliSecond: Integer);
begin
  while True do
  begin
    if AMilliSecond - 50 > 0 then
    begin
      Sleep(50);
      AMilliSecond := AMilliSecond - 50;
    end
    else
    begin
      Sleep(AMilliSecond);
      Break;
    end;
  end;
end;

function FindSpectifyHandle(const ABeginHandle: HWND; const AClassNameSign: array of string): HWND;
var
  hPreChild, hChild: HWND;
  strClassName: string;
  nIndex: Integer;
begin
  nIndex := 0;
  Result := 0;
  hPreChild := 0;
  hChild := GetWindow(ABeginHandle, GW_CHILD);
  while nIndex <= High(AClassNameSign) do
  begin
    if hChild = 0 then
    begin
      if hPreChild = 0 then
        Break
      else
      begin
        hChild := GetWindow(hPreChild, GW_HWNDNEXT);
        Dec(nIndex);
      end;
    end;
    strClassName := GetClassName(hChild);
    if (CompareText(strClassName, AClassNameSign[nIndex]) = 0) or
      (SearchSignPositon(strClassName, AClassNameSign[nIndex]) > 0) then
    begin
      hPreChild := hChild;
      hChild := GetWindow(hChild, GW_CHILD);
      Inc(nIndex);
      Result := hPreChild;
    end
    else
    begin
      hChild := GetWindow(hChild, GW_HWNDNEXT);
    end;
  end;
end;

function FindHandle(const AParent: HWND; const AControlIndex: Integer; const AControlClassName: string): HWND;
var
  AryClassName: array of string;
  iLoop: Integer;
begin
  SetLength(AryClassName, AControlIndex);
  for iLoop := 0 to AControlIndex - 1 do
    AryClassName[iLoop] := AControlClassName;
 Result := FindSpectifyHandleEx(AParent, AryClassName);
end;

function FindSpectifyHandleEx(const ABeginHandle: HWND; const ASign: array of string; const IsClassSign: Boolean): HWND;
var
  iLoop: Integer;
begin
  Result := 0;
  for iLoop := 0 to High(ASign) do
  begin
    if IsClassSign then
      Result := FindWindowHandleEx(ABeginHandle, Result, ASign[iLoop], '')
    else
      Result := FindWindowHandleEx(ABeginHandle, Result, '', ASign[iLoop]);
    if Result = 0 then
      Break;
  end;
end;

procedure ForceCloseWindow(AHandle: HWND);
begin
  if IsWindow(AHandle) then
  begin
    SendMessage(AHandle, WM_CLOSE, 0, 0);
    SelfSleep(100);
  end;

  if IsWindow(AHandle) then
  begin
    DestroyWindow(AHandle);
    SelfSleep(100);
  end;
end;

function GetListBoxText(mHandle: THandle; mStrings: TStrings): Boolean;
var
  vItemCount: Integer;
  I: Integer;
  S: string;
begin
  Result := False;
  if not Assigned(mStrings) then
    Exit;
  mStrings.BeginUpdate;
  try
    mStrings.Clear;
    vItemCount := SendMessage(mHandle, LB_GETCOUNT, 0, 0);
    for I := 0 to vItemCount - 1 do
    begin
      SetLength(S, SendMessage(mHandle, LB_GETTEXTLEN, I, 0));
      SendMessage(mHandle, LB_GETTEXT, I, Integer(@S[1]));
      mStrings.Add(S);
    end;
    SetLength(S, 0);
  finally
    mStrings.EndUpdate;
  end;
  Result := True;
end; { GetListBoxText }

function GetComboBoxText(const AComboBoxHandle: HWND; const AItemIndex: Integer): string;
begin
  SetLength(Result, SendMessage(AComboBoxHandle, CB_GETLBTEXTLEN, AItemIndex - 1, 0));
  SendMessage(AComboBoxHandle, CB_GETLBTEXT, AItemIndex - 1, Integer(@Result[1]));
end;

procedure WMSetText(const AHandle: HWND; const AText: string);
begin
  SendMessage(AHandle, WM_SETTEXT, 0, Integer(AText));
end;

procedure SetComboBoxText(const AComboBoxHandle: HWND; const AText: string);
begin
  WMSetText(AComboBoxHandle, AText);
end;

procedure SetComboBoxIndex(const AComboBoxHandle: HWND; const AIndex: Integer);
begin
  SendMessage(AComboBoxHandle, CB_SETCURSEL, AIndex, 0);
end;

function GetComboBoxItemCount(const AComboBoxHandle: HWND): Integer;
begin
  Result := SendMessage(AComboBoxHandle, CB_GETCOUNT, 0, 0);
end;

function GetTempPath: string;
var
  nLen: Integer;
begin
  nLen := Windows.GetTempPath(0, PChar(Result));
  SetLength(Result, nLen);
  Windows.GetTempPath(nLen, PChar(Result));
  SetLength(Result, nLen - 1);
  Result := Trim(IncludeTrailingPathDelimiter(Result));
end;

function GetRandomTempFileName(const AExtentName: string): string;
var
  strName: string;
begin
  Result := GetTempPath;
  while True do
  begin
    strName := Format('%s%d%s.%s', ['jxd', GetRandomNum(0, 999999), 'pn', AExtentName]);
    if not FileExists(Result + strName) then
    begin
      Result := Result + strName;
      Break;
    end;
  end;
end;

function GetListViewItemText(const AListViewHandle: HWND; const ARowIndex, AColIndex: Integer; const AMaxBufSize: Integer): string;
var
  pLocaBuf: PChar;
  dwProcessID, dwByteCount: DWORD;
  hProcess: HWND;
  pMem: Pointer;
  nAllocSize: Integer;
  lvItem: TLVItem;
begin
  GetWindowThreadProcessId(AListViewHandle, dwProcessID);
  hProcess := OpenProcess(PROCESS_VM_OPERATION or PROCESS_VM_READ or PROCESS_VM_WRITE, False, dwProcessID);
  if hProcess = 0 then
    raise Exception.create('无法打开进程');
  GetMem(pLocaBuf, AMaxBufSize);
  pMem := nil;
  nAllocSize := Sizeof(TLVItem) + AMaxBufSize;
  try
    pMem := VirtualAllocEx(hProcess, nil, nAllocSize, MEM_COMMIT, PAGE_READWRITE);

    lvItem.iSubItem := AColIndex + 1;
    lvItem.cchTextMax := AMaxBufSize;
    lvItem.pszText := PChar(Integer(pMem) + Sizeof(TLVItem));

    WriteProcessMemory(hProcess, pMem, @lvItem, sizeof(TLVItem), dwByteCount);
    SendMessage(AListViewHandle, LVM_GETITEMTEXT, ARowIndex, Integer(pMem));
    ReadProcessMemory(hProcess, Pointer(Integer(pMem) + Sizeof(TLVItem)), pLocaBuf, AMaxBufSize, dwByteCount);
    Result := pLocaBuf;
  finally
    if pMem <> nil then
    begin
      VirtualFreeEx(hProcess, pMem, nAllocSize, MEM_DECOMMIT);
      VirtualFreeEx(hProcess, pMem, 0, MEM_RELEASE);
    end;
    FreeMem(pLocaBuf);
    CloseHandle(hProcess);
  end;
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
    GetModuleFileNameEx(hProcessHandle, hProcessModule, cPath, MAX_PATH);
    Result := cPath;
  finally
    CloseHandle(hProcessHandle);
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

function IsCapsLockOn: Boolean;
begin
  Result := GetKeyState(VK_CAPITAL) > 0;
end;

procedure InputMsgByKeyboard(const AText: string; const AHandle: HWND = 0);
var
  iLoop: Integer;
  nByte: Byte;
  bShift, bxx: Boolean;
  strUpper: string;
  xx: char;
begin
  strUpper := UpperCase(AText);
  for iLoop := 1 to Length(AText) do
  begin
    case strUpper[iLoop] of
        '~': begin xx := '`'; bxx := True; end;
        '!': begin xx := '1'; bxx := True; end;
        '@': begin xx := '2'; bxx := True; end;
        '#': begin xx := '3'; bxx := True; end;
        '$': begin xx := '4'; bxx := True; end;
        '%': begin xx := '5'; bxx := True; end;
        '^': begin xx := '6'; bxx := True; end;
        '&': begin xx := '7'; bxx := True; end;
        '*': begin xx := '8'; bxx := True; end;
        '(': begin xx := '9'; bxx := True; end;
        ')': begin xx := '0'; bxx := True; end;
        '_': begin xx := '-'; bxx := True; end;
        '+': begin xx := '='; bxx := True; end;
        '|': begin xx := '\'; bxx := True; end;
        ':': begin xx := ';'; bxx := True; end;
        '"': begin xx := ''''; bxx := True; end;
        '?': begin xx := '/'; bxx := True; end;
        '<': begin xx := ','; bxx := True; end;
        '>': begin xx := '.'; bxx := True; end;
        '{': begin xx := '['; bxx := True; end;
        '}': begin xx := ']'; bxx := True; end;
        else
        begin
          xx := strUpper[iLoop];
          bxx := False;
        end;
      end;

    if xx = '-' then
      nByte := 189
    else if xx = '=' then
      nByte := 187
    else if xx = '\' then
      nByte := 220
    else if xx = ';' then
      nByte := 186
    else if xx = '''' then
      nByte := 222
    else if xx = '/' then
      nByte := 191
    else if xx = '.' then
      nByte := 190
    else if xx = ',' then
      nByte := 188
    else if xx = '[' then
      nByte := 219
    else if xx = ']' then
      nByte := 221
    else if xx = '`' then
      nByte := 192
    else
      nByte := Ord(xx);

    if bxx then
    begin
      Keybd_event(VK_SHIFT, 0, 0, 0);
      bShift := True;
    end
    else
    begin
      if ((not IsCapsLockOn) and IsUpperLetter(AText[iLoop])) or
        (IsCapsLockOn and IsLowerLetter(AText[iLoop])) then
      begin
        Keybd_event(VK_SHIFT, 0, 0, 0);
        bShift := True;
      end
      else
        bShift := False;
    end;

    if IsWindow(AHandle) then
    begin
      SetForegroundWindow(AHandle);
      BringWindowToTop(AHandle);
      SetFocus(AHandle);
    end;

    Keybd_event(nByte, MapVirtualKey(nByte, 0), 0, 0);
    Keybd_event(nByte, MapVirtualKey(nByte, 0), KEYEVENTF_KEYUP, 0);

    if bShift then
      Keybd_event(VK_SHIFT, 0, KEYEVENTF_KEYUP, 0);

    Sleep(10);
  end;
end;

function ShowMsg(const AText: string; const ACaption: string = '提示'; const AHandle: HWND = 0; const AType: UINT = MB_OK or MB_ICONINFORMATION): Integer;
begin
  Result := MessageBox(AHandle, PChar(AText), PChar(ACaption), AType);
end;

procedure SetButtonCheckState(const AButtonHandle: HWND; const ACheck: Boolean);
var
  nButtonState: Integer;
begin
  nButtonState := SendMessage(AButtonHandle, BM_GETSTATE, 0, 0);
  if (nButtonState = BST_CHECKED) then
  begin
   if not ACheck then
    SendMessage(AButtonHandle, BM_SETCHECK, BST_UNCHECKED, 0);
  end
  else
  begin
    if ACheck then
      SendMessage(AButtonHandle, BM_SETCHECK, BST_CHECKED, 0);
  end;
end;

function SelectDirectory(Ahandle: HWND; const Caption: string; const Root: WideString; out Directory: string): Boolean;
var
  lpbi: _browseinfo;
  buf: array[0..MAX_PATH] of char;
  id: ishellfolder;
  eaten, att: cardinal;
  rt: pitemidlist;
  initdir: pwidechar;
begin
  result := false;
  lpbi.hwndOwner := Ahandle;
  lpbi.lpfn := nil;
  lpbi.lpszTitle := pchar(caption);
  lpbi.ulFlags := BIF_RETURNONLYFSDIRS + 64;
  SHGetDesktopFolder(id);
  initdir := pwchar(root);
  id.ParseDisplayName(0, nil, initdir, eaten, rt, att);
  lpbi.pidlRoot := rt;
  getmem(lpbi.pszDisplayName, MAX_PATH);
  try
    result := shgetpathfromidlist(shbrowseforfolder(lpbi), buf);
  except
    freemem(lpbi.pszDisplayName);
  end;
  if Result then
  begin
    Directory := IncludeTrailingPathDelimiter(buf);
  end;
end;
{
function ListView_SetItemState_Ex(hwndLV: HWND; i: Integer; data, mask: UINT): Bool;
var
  LVItem: TLVItem;
  ProcessID, ProcessHD, Temp: DWORD;
  MemPoint: Pointer;
begin
  GetWindowThreadProcessId(hwndLV, ProcessID);

  ProcessHD := OpenProcess(
    PROCESS_VM_OPERATION or PROCESS_VM_READ or PROCESS_VM_WRITE, FALSE, ProcessID);

  MemPoint := VirtualAllocEx(ProcessHD, nil, SizeOf(TLVItem), MEM_COMMIT, PAGE_READWRITE);

  LVItem.stateMask := mask;
  LVItem.state := data;
  WriteProcessMemory(ProcessHD, MemPoint, @LVItem, SizeOf(TLVItem), Temp);

  Result := (SendMessage(hwndLV, LVM_SETITEMSTATE, i, Integer(MemPoint)) <> 0);

  VirtualFreeEx(ProcessHD, MemPoint, SizeOf(TLVItem), MEM_DECOMMIT);
  VirtualFreeEx(ProcessHD, MemPoint, 0, MEM_RELEASE);
end;

  // 扩展的ListView项目读取函数
function ListView_GetItemText_Ex(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PChar; cchTextMax: Integer): Integer;
var
  LVItem: TLVItem;
  ProcessID, ProcessHD, Temp: DWORD;
  MemPoint: Pointer;
begin
  GetWindowThreadProcessId(hwndLV, ProcessID);

  ProcessHD := OpenProcess(
    PROCESS_VM_OPERATION or PROCESS_VM_READ or PROCESS_VM_WRITE,
    FALSE, ProcessID);

  MemPoint := VirtualAllocEx(ProcessHD, nil, SizeOf(TLVItem) + cchTextMax,
    MEM_COMMIT, PAGE_READWRITE);

  LVItem.iSubItem := iSubItem;
  LVItem.cchTextMax := cchTextMax;
  LVItem.pszText := PChar(Integer(MemPoint) + SizeOf(TLVItem));

  WriteProcessMemory(ProcessHD, MemPoint, @LVItem, SizeOf(TLVItem), Temp);
  Result := SendMessage(hwndLV, LVM_GETITEMTEXT, i, Integer(MemPoint));

  ReadProcessMemory(ProcessHD, Pointer(Integer(MemPoint) + SizeOf(TLVItem)),
    pszText, cchTextMax, Temp);

  VirtualFreeEx(ProcessHD, MemPoint, SizeOf(TLVItem) + cchTextMax, MEM_DECOMMIT);
  VirtualFreeEx(ProcessHD, MemPoint, 0, MEM_RELEASE);
end;

  // 扩展的TreeView_GetItem (取得TreeView指定子项目)
function TreeView_GetItem_Ex(hwndTV: HWND; var TVItem: TTVItem): Bool;
var
  ProcessID, ProcessHD, Temp: DWORD;
  pszText, MemPoint: Pointer;
begin
  Result := FALSE;

  GetWindowThreadProcessId(hwndTV, ProcessID);

  ProcessHD := OpenProcess(
    PROCESS_VM_OPERATION or PROCESS_VM_READ or PROCESS_VM_WRITE, FALSE, ProcessID);
  if (ProcessHD = 0) then Exit;

  MemPoint := VirtualAllocEx(ProcessHD, nil,
    SizeOf(TTVItem) + TVItem.cchTextMax + 1,
    MEM_COMMIT, PAGE_READWRITE);
  if (MemPoint = nil) then Exit;

  pszText := TVItem.pszText; // 保存本进程地址
  PChar(pszText)^ := #0;
  TVItem.pszText := PChar(Integer(MemPoint) + SizeOf(TTVItem));
  if (WriteProcessMemory(ProcessHD, MemPoint, @TVItem, SizeOf(TVItem), Temp) = FALSE) then Exit;
  if (WriteProcessMemory(ProcessHD, TVItem.pszText, pszText, 1, Temp) = FALSE) then Exit;

  Result := (SendMessage(hwndTV, TVM_GETITEM, 0, LongInt(MemPoint)) <> 0);

  if (ReadProcessMemory(ProcessHD, TVItem.pszText, pszText, TVItem.cchTextMax + 1, Temp) = FALSE) then
  begin
    Result := FALSE;
    Exit;
  end;
  TVItem.pszText := pszText; // 恢复本进程地址

  VirtualFreeEx(ProcessHD, MemPoint, SizeOf(TTVItem) + TVItem.cchTextMax + 1, MEM_DECOMMIT);
  VirtualFreeEx(ProcessHD, MemPoint, 0, MEM_RELEASE);
end;

  // 取指定TreeView子项目文字
function TreeView_GetItem_Text(hwndTV: HWND; hitem: HTreeItem): string;
var
  Buffer: array[0..50] of Char;
  TVItem: TTVItem;
begin
  TVItem.hItem := hitem;
  TVItem.mask := TVIF_TEXT;
  TVItem.pszText := @Buffer[0];
  TVItem.cchTextMax := 50;

  if TreeView_GetItem_Ex(hwndTV, TVItem) then
    Result := Buffer
  else
    Result := '';
end;

  // 扩展的ListView_FindItem
function ListView_FindItem_Ex(hwndLV: HWND; iStart: Integer; var LVFindInfo: TLVFindInfo): Integer;
var
  ProcessID, ProcessHD, Temp: DWORD;
  psz, MemPoint: Pointer;
begin
  Result := -1; // 默认查找失败

  GetWindowThreadProcessId(hwndLV, ProcessID);

  ProcessHD := OpenProcess(
    PROCESS_VM_OPERATION or PROCESS_VM_READ or PROCESS_VM_WRITE, FALSE, ProcessID);
  if (ProcessHD = 0) then Exit;

  MemPoint := VirtualAllocEx(
    ProcessHD, nil, SizeOf(TLVFindInfo) + StrLen(LVFindInfo.psz) + 1, MEM_COMMIT, PAGE_READWRITE);
  if (MemPoint = nil) then Exit;

  psz := LVFindInfo.psz; // 保存本进程地址
  LVFindInfo.psz := PChar(Integer(MemPoint) + SizeOf(TLVFindInfo));
  if (WriteProcessMemory(ProcessHD, MemPoint, @LVFindInfo, SizeOf(TLVFindInfo), Temp) = FALSE) then Exit;
  if (WriteProcessMemory(ProcessHD, LVFindInfo.psz, psz, StrLen(psz) + 1, Temp) = FALSE) then Exit;

  Result := SendMessage(hWndLv, LVM_FINDITEM, iStart, LongInt(MemPoint));

  LVFindInfo.psz := psz; // 恢复本进程地址

  VirtualFreeEx(ProcessHD, MemPoint, SizeOf(TLVFindInfo) + StrLen(psz) + 1, MEM_DECOMMIT);
  VirtualFreeEx(ProcessHD, MemPoint, 0, MEM_RELEASE);
end;
}

function GetCacheVerifyCodeFile(VerifyCodeURL: String; Var CacheVerifyCodeFile: String): Boolean;
Var
  lpEntryInfo:   PInternetCacheEntryInfo;   
  dwEntrySize, dwLastError, Hwd: LongWORD;
  i, j: Integer;
  f:   String;
begin
  Result := false;
  dwEntrySize := 0;
  j := 0;
  CacheVerifyCodeFile := '';
  FindFirstUrlCacheEntry( nil, TInternetCacheEntryInfo( nil^ ), dwEntrySize );
  GetMem( lpEntryInfo, dwEntrySize );
  Hwd := FindFirstUrlCacheEntry( nil, lpEntryInfo^, dwEntrySize );
  if Hwd <> 0 then
  begin
  
    repeat
      dwEntrySize   :=   0;
      FindNextUrlCacheEntry(Hwd, TInternetCacheEntryInfo( nil^ ), dwEntrySize );
      dwLastError := GetLastError();
      if dwLastError = ERROR_INSUFFICIENT_BUFFER then
      Begin
        GetMem( lpEntryInfo, dwEntrySize );
        if FindNextUrlCacheEntry( Hwd, lpEntryInfo^, dwEntrySize ) then
        begin
          if Pos(UpperCase(VerifyCodeURL), UpperCase( lpEntryInfo.lpszSourceUrlName )) > 0 then
          begin
            i := FileAge( lpEntryInfo.lpszLocalFileName );
            if i > j then
            begin
              j := i;
              f := lpEntryInfo.lpszLocalFileName;
            end
            else
              DeleteUrlCacheEntry(lpEntryInfo.lpszSourceUrlName);
          end;
        end;
      end;
    until ( dwLastError = ERROR_NO_MORE_ITEMS );
    
    if FileExists(f) then
    begin
      CacheVerifyCodeFile := f;
      Result := true;
    End;
  End;
  FreeMem(lpEntryInfo);
  FindCloseUrlCache(Hwd);
End;

function DeleteFolder(APath: String): Boolean;
var
  F: TSearchRec;
begin
  Result := True;
  APath := IncludeTrailingPathDelimiter(APath);
  if FindFirst(APath + '*.*', faAnyFile, F) = 0 then
  begin
    repeat
      if (F.Name = '.') or (F.Name = '..') then Continue;
      if f.Attr and faDirectory <> 0 then
        Result := Result and DeleteFolder(APath + F.Name)
      else
      begin
        SetFileAttributes(PChar(APath + F.Name), 0);
        Result := Result and DeleteFile(APath + F.Name);
      end;
    until FindNext(F) <> 0;
    FindClose(F);
  end;
  Result := Result and RemoveDir(APath);
end;

function GetFileSizeEx(const AFileName: string): Cardinal;
var
  SearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(AFileName), faAnyFile, SearchRec) = 0 then
  begin
    Result := SearchRec.Size;
    FindClose(SearchRec);
  end
  else
    Result := 0;
end;

function GetOSVer: Word;
var
  b64Bit: Boolean;
  SysInfo: TSystemInfo;
  ovsi: TOSVersionInfoEx;
begin
  ZeroMemory(@SysInfo,SizeOf(TSystemInfo));
  GetSystemInfo(SysInfo);
  b64Bit := (SysInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_IA64)
    or (SysInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_ALPHA64)
    or (SysInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64)
    or (SysInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_IA32_ON_WIN64);

  Result := 1; //Other
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    Result := 2 //9x
  else if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    if (Win32MajorVersion = 5) and (Win32MinorVersion = 0) then
      Result := 4 // 2000
    else if (Win32MajorVersion = 5) and (Win32MinorVersion = 1) then
    begin
      if b64Bit then
        Result := 16 //XP64
      else
        Result := 8 //XP
    end
    else if (Win32MajorVersion = 5) and (Win32MinorVersion = 2) then
    begin
      if b64Bit then
        Result := 64 //XP2003-64
      else
        Result := 32 //XP2003
    end
    else if (Win32MajorVersion = 6) and (Win32MinorVersion = 0) then
    begin
      ZeroMemory(@ovsi,SizeOf(TOSVersionInfoEx));
      ovsi.dwOSVersionInfoSize := sizeof(TOSVersionInfoEx);
      MyGetVersionEx(@ovsi);
      if ovsi.wProductType = VER_NT_WORKSTATION then
      begin
        if b64Bit then
          Result := 256 //vista64
        else
          Result := 128 //vista32
      end
      else begin
        if b64Bit then
          Result := 1024 //200864
        else
          Result := 512 //2008
      end;
    end
  end;
end;

procedure ExploreFile(const AFileName: string);
var
  strExecute: string;
begin
  strExecute := Format('EXPLORER.EXE /select,%s', [AFileName]);
  WinExec(PChar(strExecute), SW_SHOWNORMAL);
end;

function Regsvr32(AFileName: string;AInstall: boolean=true):Cardinal;
const
  SRegsvr32CMD = 'regsvr32.exe /s "%s"';
  SUnRegsvr32CMD = 'regsvr32.exe /u /s "%s"';
begin
  if AInstall then
    Result := WinExec(PChar(format(SRegsvr32CMD,[AFileName])), SW_HIDE)
  else
    Result := WinExec(PChar(format(SUnRegsvr32CMD,[AFileName])), SW_HIDE);
end;

end.

