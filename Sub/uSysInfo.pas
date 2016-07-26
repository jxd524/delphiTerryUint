unit uSysInfo;

interface

uses SysUtils, Windows, Classes, Winsock, JxdMD5, Registry;

type
  TVendor = array [0..11] of char;
  TSysInfo = class
  public
    class function GetMacAddress: string;          //获取本机的MAC地址
    class function GetOSInfo: string;              //获取windows操作系统版本信息
    class function GetCPUIDStr: string;            //获取cpu标识字符串
    class function GetIPs: TStrings;               //获取本机IP地址
    class function GetIdeSerialNumber: string;     //获取第一个IDE硬盘的序列号
    class function GetWindowsProductID: string;    //Windows产品ID
    class function GetMemoryTotalSize : string;    //获取内存总量
    class function GetCPUSpeed: Double;            //获取CPU速率函数 floattostr(GetCPUSpeed)+'MHz';
    class function GetCPUName : string ;           //获取CPU名称
    class function GetDisplayFrequency: Integer;   //这个函数返回的显示刷新率是以Hz为单位的 IntToStr(GetDisplayFrequency)+' Hz';
    class function GetDiskSize : string;           //硬盘空间
    class function GetBios(value: integer): String;//以下函数用以获得计算机BIOS系统信息。
  end;

function GetComputerMD5: TMD5Digest;
function GetComputerStr: string;
function GetCPUVendor : TVendor; assembler; register;


implementation

//以下函数用以获得计算机BIOS系统信息。
class function TSysInfo.GetBios(value: integer): String;
// 1...Bios Type
// 2.. Bios Copyright
// 3.. Bios Date
// 4.. Bios Extended Info
// 5.. Bustype
// 6.. MachineType
begin
  Result:='(unavailable)';
  case value of
    1: result:=String(Pchar(Ptr($FE061)));
    2: result:=String(Pchar(Ptr($FE091)));
    3: result:=String(Pchar(Ptr($FFFF5)));
    4: result:=String(Pchar(Ptr($FEC71)));
  end;
end;

function GetCPUVendor : TVendor; assembler; register;
//获取CPU生产厂家函数
//调用方法:EDIT.TEXT:='Current CPU Vendor:'+GetCPUVendor;
asm
  PUSH EBX {Save affected register}
  PUSH EDI
  MOV EDI,EAX {@Result (TVendor)}
  MOV EAX,0
  DW $A20F {CPUID Command}
  MOV EAX,EBX
  XCHG EBX,ECX {save ECX result}
  MOV ECX,4
  @1:
  STOSB
  SHR EAX,8
  LOOP @1
  MOV EAX,EDX
  MOV ECX,4
  @2:
  STOSB
  SHR EAX,8
  LOOP @2
  MOV EAX,EBX
  MOV ECX,4
  @3:
  STOSB
  SHR EAX,8
  LOOP @3
  POP EDI {Restore registers}
  POP EBX
end;


function GetComputerMD5:TMD5Digest;
var
  tmpstr:string;
begin
// tmpstr := TSysInfo.GetCPUIDStr + TSysInfo.GetIdeSerialNumber + TSysInfo.GetMacAddress;
 tmpstr := TSysInfo.GetIdeSerialNumber + TSysInfo.GetMacAddress;
 Result := MD5String(tmpstr);
end;

function GetComputerStr:string;
begin
 Result := MD5Print(GetComputerMD5);
end;

{ TSysInfo }

/// 获取cpu标识字符串
class function TSysInfo.GetCPUIDStr: string;
type
  TCPUID = array[1..4] of Longint;
  TVendor = array[0..11] of char;
  function GetCPUID: TCPUID; assembler; register;
  asm
   PUSH    EBX         {Save affected register}
   PUSH    EDI
   MOV     EDI,EAX     {@Resukt}
   MOV     EAX,1
   DW      $A20F       {CPUID Command}
   STOSD             {CPUID[1]}
   MOV     EAX,EBX
   STOSD               {CPUID[2]}
   MOV     EAX,ECX
   STOSD               {CPUID[3]}
   MOV     EAX,EDX
   STOSD               {CPUID[4]}
   POP     EDI     {Restore registers}
   POP     EBX
  end;

 /// 获取制造商信息
  function GetCPUVendor: TVendor; assembler; register;
  asm
   PUSH    EBX     {Save affected register}
   PUSH    EDI
   MOV     EDI,EAX   {@Result (TVendor)}
   MOV     EAX,0
   DW      $A20F    {CPUID Command}
   MOV     EAX,EBX
   XCHG  EBX,ECX     {save ECX result}
   MOV   ECX,4
 @1:
   STOSB
   SHR     EAX,8
   LOOP    @1
   MOV     EAX,EDX
   MOV   ECX,4
 @2:
   STOSB
   SHR     EAX,8
   LOOP    @2
   MOV     EAX,EBX
   MOV   ECX,4
 @3:
   STOSB
   SHR     EAX,8
   LOOP    @3
   POP     EDI     {Restore registers}
   POP     EBX
  end;
var
  CPUID: TCPUID;
  I: Integer;
  S: TVendor;
begin
  Result := '';
  try
    for I := Low(CPUID) to High(CPUID) do
      CPUID[I] := -1;
    CPUID := GetCPUID;
    Result := Result + IntToHex(CPUID[1], 8);
    Result := Result + IntToHex(CPUID[2], 8);
    Result := Result + IntToHex(CPUID[3], 8);
    Result := Result + IntToHex(CPUID[4], 8);
    S := GetCPUVendor;
  except
  end;
  Result := Trim(S + Result);
end;

///获取windows操作系统版本信息

class function TSysInfo.GetOSInfo: string;
var
  VI: TOSVersionInfo;
begin
  Result := '';
  VI.dwOSVersionInfoSize := SizeOf(VI);
  GetVersionEx(VI); //取得正在运行的Windeows和Win32操作系统的版本
  Result := Result + Format(' %d.%d.%d', [VI.dwMajorVersion, VI.dwMinorVersion, VI.dwBuildNumber]);
  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS: Result := 'Windows 95/98' + Result;
    VER_PLATFORM_WIN32_NT: Result := 'Windows NT' + Result;
  else
    Result := 'Windows32' + Result;
  end;
end;

class function TSysInfo.GetWindowsProductID: string;
var
  reg:TRegistry;
begin
  Result := '';
  reg := TRegistry.Create;
  try
    with reg do
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey('Software\Microsoft\Windows\CurrentVersion', False);
      Result := ReadString('ProductID');
    end;
  finally
    reg.Free;
  end;
end;

/// 获取本机IP地址

class function TSysInfo.GetIPs: TStrings;
type
  TaPInAddr = array[0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  Buffer: array[0..63] of Char;
  I: Integer;
  GInitData: TWSAData;
begin
  WSAStartup($101, GInitData);
  Result := TStringList.Create;
  Result.Clear;
  GetHostName(Buffer, SizeOf(Buffer));
  phe := GetHostByName(buffer);
  if phe = nil then Exit;
  pPtr := PaPInAddr(phe^.h_addr_list);
  I := 0;
  while pPtr^[I] <> nil do
  begin
    Result.Add(inet_ntoa(pptr^[I]^));
    Inc(I);
  end;
  WSACleanup;
end;

///获取本机的MAC地址
///MAC地址以'XX-XX-XX-XX-XX-XX'的格式返回

class function TSysInfo.GetMacAddress: string;
type
  TNetTransportEnum = function(pszServer: PWideChar;
    Level: DWORD;
    var pbBuffer: pointer;
    PrefMaxLen: LongInt;
    var EntriesRead: DWORD;
    var TotalEntries: DWORD;
    var ResumeHandle: DWORD): DWORD; stdcall;

  TNetApiBufferFree = function(Buffer: pointer): DWORD; stdcall;

  PTransportInfo = ^TTransportInfo;
  TTransportInfo = record
    quality_of_service: DWORD;
    number_of_vcs: DWORD;
    transport_name: PWChar;
    transport_address: PWChar;
    wan_ish: boolean;
  end;

var
  E, ResumeHandle, EntriesRead, TotalEntries: DWORD;
  FLibHandle: THandle;
  sMachineName, sMacAddr, Retvar: string;
  pBuffer: pointer;
  pInfo: PTransportInfo;
  FNetTransportEnum: TNetTransportEnum;
  FNetApiBufferFree: TNetApiBufferFree;
  pszServer: array[0..128] of WideChar;
  i, ii, iIdx: integer;
begin
  sMachineName := '';
  Retvar := '00-00-00-00-00-00';
  try
 // Setup and load from DLL
    pBuffer := nil;
    ResumeHandle := 0;
    FLibHandle := LoadLibrary('NETAPI32.DLL');

 // Execute the external function
    if FLibHandle <> 0 then
    begin
      @FNetTransportEnum := GetProcAddress(FLibHandle, 'NetWkstaTransportEnum');
      @FNetApiBufferFree := GetProcAddress(FLibHandle, 'NetApiBufferFree');
      E := FNetTransportEnum(StringToWideChar(sMachineName, pszServer, 129), 0,
        pBuffer, -1, EntriesRead, TotalEntries, Resumehandle);

      if E = 0 then
      begin
        pInfo := pBuffer;

         // Enumerate all protocols - look for TCPIP
        for i := 1 to EntriesRead do
        begin
          if pos('TCPIP', UpperCase(pInfo^.transport_name)) <> 0 then
          begin
                // Got It - now format result 'xx-xx-xx-xx-xx-xx'
            iIdx := 1;
            sMacAddr := pInfo^.transport_address;

            for ii := 1 to 12 do
            begin
              Retvar[iIdx] := sMacAddr[ii];
              inc(iIdx);
              if iIdx in [3, 6, 9, 12, 15] then inc(iIdx);
            end;
          end;
          inc(pInfo);
        end;
        if pBuffer <> nil then FNetApiBufferFree(pBuffer);
      end;
      try
        FreeLibrary(FLibHandle);
      except
       // 错误处理
      end;
    end;
  except
  end;
  result := Trim(Retvar);
end;

class function TSysInfo.GetMemoryTotalSize: string;
var
  msMemory : TMemoryStatus;
  iPhysicsMemoryTotalSize : DWORD ;
const
  GB=1024*1024*1024;
begin
  msMemory.dwLength := SizeOf(msMemory);
  GlobalMemoryStatus(msMemory);
  iPhysicsMemoryTotalSize := msMemory.dwTotalPhys;
  Result := Format('%.2fGB',[iPhysicsMemoryTotalSize /GB]);;
end;

//获取第一个IDE硬盘的序列号

class function TSysInfo.GetCPUName: string;
var
  myreg:TRegistry;
begin
  Result := 'UnKnow';
  myreg:=TRegistry.Create;
  myreg.RootKey:=HKEY_LOCAL_MACHINE;
  if myreg.OpenKey('Hardware\Description\System\CentralProcessor\0',true) then
  begin
    if myreg.ValueExists('ProcessorNameString') then
    begin
      Result :=  myreg.ReadString('ProcessorNameString') ;
      myreg.CloseKey;
    end;
  end;
end ;

class function TSysInfo.GetCPUSpeed: Double;
//获取CPU速率函数
//调用方法:EDIT.TEXT:='Current CPU Speed:'+floattostr(GetCPUSpeed)+'MHz';
const
  DelayTime = 500; // 时间单位是毫秒
var
  TimerHi, TimerLo: DWORD;
  PriorityClass, Priority: Integer;
begin
  PriorityClass := GetPriorityClass(GetCurrentProcess);
  Priority := GetThreadPriority(GetCurrentThread);
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);
  Sleep(10);
  asm
    dw 310Fh // rdtsc
    mov TimerLo, eax
    mov TimerHi, edx
  end;
  Sleep(DelayTime);
  asm
    dw 310Fh // rdtsc
    sub eax, TimerLo
    sbb edx, TimerHi
    mov TimerLo, eax
    mov TimerHi, edx
  end;

  SetThreadPriority(GetCurrentThread, Priority);
  SetPriorityClass(GetCurrentProcess, PriorityClass);
  Result := TimerLo / (1000.0 * DelayTime);
end;

class function TSysInfo.GetDiskSize: string;
var
  Available,Total,Free: Int64;
  AvailableT,TotalT: real;
  Drive: Char;
const
  GB=1024*1024*1024;
begin
  AvailableT := 0;
  TotalT := 0;
  for Drive:='C' to 'Z' do
  begin
    if GetDriveType(Pchar(Drive+':\'))=DRIVE_FIXED then
    begin
      GetDiskFreeSpaceEx( PChar(Drive+':\'), Available, Total, @Free );
      AvailableT := AvailableT+Available;
      TotalT := TotalT+Total;
    end;
  end;
  Result := Format('%.2fGB',[TotalT/GB]);
end ;

class function TSysInfo.GetDisplayFrequency: Integer;
// 这个函数返回的显示刷新率是以Hz为单位的
//调用方法:EDIT.TEXT:='Current DisplayFrequency:'+inttostr(GetDisplayFrequency)+' Hz';
var
  DeviceMode: TDeviceMode;
begin
  EnumDisplaySettings(nil, Cardinal(-1), DeviceMode);
  Result := DeviceMode.dmDisplayFrequency;
end;

class function TSysInfo.GetIdeSerialNumber: string;
const IDENTIFY_BUFFER_SIZE = 512;
type
  TIDERegs = packed record
    bFeaturesReg: BYTE; // Used for specifying SMART "commands".
    bSectorCountReg: BYTE; // IDE sector count register
    bSectorNumberReg: BYTE; // IDE sector number register
    bCylLowReg: BYTE; // IDE low order cylinder value
    bCylHighReg: BYTE; // IDE high order cylinder value
    bDriveHeadReg: BYTE; // IDE drive/head register
    bCommandReg: BYTE; // Actual IDE command.
    bReserved: BYTE; // reserved for future use.  Must be zero.
  end;
  TSendCmdInParams = packed record
    // Buffer size in bytes
    cBufferSize: DWORD;
    // Structure with drive register values.
    irDriveRegs: TIDERegs;
    // Physical drive number to send command to (0,1,2,3).
    bDriveNumber: BYTE;
    bReserved: array[0..2] of Byte;
    dwReserved: array[0..3] of DWORD;
    bBuffer: array[0..0] of Byte; // Input buffer.
  end;
  TIdSector = packed record
    wGenConfig: Word;
    wNumCyls: Word;
    wReserved: Word;
    wNumHeads: Word;
    wBytesPerTrack: Word;
    wBytesPerSector: Word;
    wSectorsPerTrack: Word;
    wVendorUnique: array[0..2] of Word;
    sSerialNumber: array[0..19] of CHAR;
    wBufferType: Word;
    wBufferSize: Word;
    wECCSize: Word;
    sFirmwareRev: array[0..7] of Char;
    sModelNumber: array[0..39] of Char;
    wMoreVendorUnique: Word;
    wDoubleWordIO: Word;
    wCapabilities: Word;
    wReserved1: Word;
    wPIOTiming: Word;
    wDMATiming: Word;
    wBS: Word;
    wNumCurrentCyls: Word;
    wNumCurrentHeads: Word;
    wNumCurrentSectorsPerTrack: Word;
    ulCurrentSectorCapacity: DWORD;
    wMultSectorStuff: Word;
    ulTotalAddressableSectors: DWORD;
    wSingleWordDMA: Word;
    wMultiWordDMA: Word;
    bReserved: array[0..127] of BYTE;
  end;
  PIdSector = ^TIdSector;
  TDriverStatus = packed record
    // 驱动器返回的错误代码，无错则返回0
    bDriverError: Byte;
    // IDE出错寄存器的内容，只有当bDriverError 为 SMART_IDE_ERROR 时有效
    bIDEStatus: Byte;
    bReserved: array[0..1] of Byte;
    dwReserved: array[0..1] of DWORD;
  end;
  TSendCmdOutParams = packed record
    // bBuffer的大小
    cBufferSize: DWORD;
    // 驱动器状态
    DriverStatus: TDriverStatus;
    // 用于保存从驱动器读出的数据的缓冲区，实际长度由cBufferSize决定
    bBuffer: array[0..0] of BYTE;
  end;
var hDevice: THandle;
  cbBytesReturned: DWORD;
  SCIP: TSendCmdInParams;
  aIdOutCmd: array[0..(SizeOf(TSendCmdOutParams) + IDENTIFY_BUFFER_SIZE - 1) - 1] of Byte;
  IdOutCmd: TSendCmdOutParams absolute aIdOutCmd;
  procedure ChangeByteOrder(var Data; Size: Integer);
  var ptr: PChar;
    i: Integer;
    c: Char;
  begin
    ptr := @Data;
    for i := 0 to (Size shr 1) - 1 do begin
      c := ptr^;
      ptr^ := (ptr + 1)^;
      (ptr + 1)^ := c;
      Inc(ptr, 2);
    end;
  end;
begin
  Result := ''; // 如果出错则返回空串
  try
    if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then begin // Windows NT, Windows 2000
        // 提示! 改变名称可适用于其它驱动器，如第二个驱动器： '\\.\PhysicalDrive1\'
      hDevice := CreateFile('\\.\PhysicalDrive0', GENERIC_READ or GENERIC_WRITE,
        FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    end else // Version Windows 95 OSR2, Windows 98
      hDevice := CreateFile('\\.\SMARTVSD', 0, 0, nil, CREATE_NEW, 0, 0);
    if hDevice = INVALID_HANDLE_VALUE then Exit;
    try
      FillChar(SCIP, SizeOf(TSendCmdInParams) - 1, #0);
      FillChar(aIdOutCmd, SizeOf(aIdOutCmd), #0);
      cbBytesReturned := 0;
      // Set up data structures for IDENTIFY command.
      with SCIP do begin
        cBufferSize := IDENTIFY_BUFFER_SIZE;
  //      bDriveNumber := 0;
        with irDriveRegs do begin
          bSectorCountReg := 1;
          bSectorNumberReg := 1;
  //      if Win32Platform=VER_PLATFORM_WIN32_NT then bDriveHeadReg := $A0
  //      else bDriveHeadReg := $A0 or ((bDriveNum and 1) shl 4);
          bDriveHeadReg := $A0;
          bCommandReg := $EC;
        end;
      end;
      if not DeviceIoControl(hDevice, $0007C088, @SCIP, SizeOf(TSendCmdInParams) - 1,
        @aIdOutCmd, SizeOf(aIdOutCmd), cbBytesReturned, nil) then Exit;
    finally
      CloseHandle(hDevice);
    end;
    with PIdSector(@IdOutCmd.bBuffer)^ do
    begin
      ChangeByteOrder(sSerialNumber, SizeOf(sSerialNumber));
      (PChar(@sSerialNumber) + SizeOf(sSerialNumber))^ := #0;
      Result := Trim(PChar(@sSerialNumber));
    end;
  except
  end;
end;


end.

