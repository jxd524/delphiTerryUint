unit uJxdDialUp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

const
  DNLEN = 15;
  UNLEN = 256;
  PWLEN = 256;

  RAS_MaxEntryName = 256;
  RAS_MaxDeviceType = 16;
  RAS_MaxDeviceName = 128;
  RAS_MaxPhoneNumber = 128;
  RAS_MaxCallbackNumber = RAS_MaxPhoneNumber;

  RASCS_PAUSED = $1000;
  RASCS_DONE = $2000;

  RASCS_OpenPort = 0;
  RASCS_PortOpened = 1;
  RASCS_ConnectDevice = 2;
  RASCS_DeviceConnected = 3;
  RASCS_AllDevicesConnected = 4;
  RASCS_Authenticate = 5;
  RASCS_AuthNotify = 6;
  RASCS_AuthRetry = 7;
  RASCS_AuthCallback = 8;
  RASCS_AuthChangePassword = 9;
  RASCS_AuthProject = 10;
  RASCS_AuthLinkSpeed = 11;
  RASCS_AuthAck = 12;
  RASCS_ReAuthenticate = 13;
  RASCS_Authenticated = 14;
  RASCS_PrepareForCallback = 15;
  RASCS_WaitForModemReset = 16;
  RASCS_WaitForCallback = 17;
  RASCS_Projected = 18;
  RASCS_StartAuthentication = 19;
  RASCS_CallbackComplete = 20;
  RASCS_LogonNetwork = 21;
  RASCS_Interactive = RASCS_PAUSED;
  RASCS_RetryAuthentication = RASCS_PAUSED + 1;
  RASCS_CallbackSetByCaller = RASCS_PAUSED + 2;
  RASCS_PasswordExpired = RASCS_PAUSED + 3;
  RASCS_Connected = RASCS_DONE;
  RASCS_Disconnected = RASCS_DONE + 1;

type
  THRasConn = Longint;

  LPRasConnA = ^TRasConnA;
TRasConnA = record
  dwSize : Longint;
  hrasconn : THRasConn;
  szEntryName : array[0..RAS_MaxEntryName] of AnsiChar;
  szDeviceType : array[0..RAS_MaxDeviceType] of AnsiChar;
  szDeviceName : array[0..RAS_MaxDeviceName] of AnsiChar;
end;

LPRasConn = ^TRasConn;
TRasConn = TRasConnA;

LPRasConnState = ^TRasConnState;
TRasConnState = Integer;

LPRasConnStatusA = ^TRasConnStatusA;
TRasConnStatusA = record
  dwSize : Longint;
  rasconnstate : TRasConnState;
  dwError : LongInt;
  szDeviceType : array[0..RAS_MaxDeviceType] of AnsiChar;
  szDeviceName : array[0..RAS_MaxDeviceName] of AnsiChar;
end;

LPRasConnStatus = ^TRasConnStatus;
TRasConnStatus = TRasConnStatusA;

LPRasEntryNameA = ^TRasEntryNameA;
TRasEntryNameA = record
  dwSize : Longint;
  szEntryName : array[0..RAS_MaxEntryName] of AnsiChar;
end;

LPRasEntryName = ^TRasEntryName;
TRasEntryName = TRasEntryNameA;

LPRasDialParamsA = ^TRasDialParamsA;
TRasDialParamsA = record
  dwSize : LongInt;
  szEntryName : array[0..RAS_MaxEntryName] of AnsiChar;
  szPhoneNumber : array[0..RAS_MaxPhoneNumber] of AnsiChar;
  szCallbackNumber : array[0..RAS_MaxCallbackNumber] of AnsiChar;
  szUserName : array[0..UNLEN] of AnsiChar;
  szPassword : array[0..PWLEN] of AnsiChar;
  szDomain : array[0..DNLEN] of AnsiChar;
end;

LPRasDialParams = ^TRasDialParams;
TRasDialParams = TRasDialParamsA;

LPRasDialExtensions = ^TRasDialExtensions;
TRasDialExtensions = record
  dwSize : LongInt;
  dwfOptions : LongInt;
  hwndParent : HWnd;
  reserved : LongInt;
end;

type
TOnStatusEvent = procedure(Sender: TObject; MessageText: String; Error: Boolean) of object;

TJxdDialUp = class(TComponent)
private
  FTimer: TTimer;
  FAbout: String;
  FPassword: String;
  FUsername: String;
  FConnectTo: String;
  hRasDLL: THandle;
  StatusStr: String;
  ErrorStat: Boolean;
  AsyncStatus: Boolean;
  FLangStrList: TStringList;
  FPossibleConnections: TStringList;
  FOnStatusEvent: TOnStatusEvent;
  function StatusString(State: TRasConnState; Error: Integer; var ES: Boolean): String;
  function GetActiveConnection: String;
  procedure SetLangStrList(Value: TStringList);
  function GetCurrentConnection: String;
  procedure SetCurrentConnection(Value: String);
  procedure SetPossibleConnections(Value: TStringList);
  function GetPossibleConnections: TStringList;
  procedure GetConnections(var SL: TStringList);
  function GetRasInstalled: Boolean;
protected
  procedure Timer(Sender: TObject); virtual;
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  function GoOnline: Boolean;
  procedure GoOffline;
published
  property About: String read FAbout write FAbout;// stored False;
  property Password: String read FPassword write FPassword;
  property Username: String read FUsername write FUsername;
  property CurrentConnection: String read GetCurrentConnection write SetCurrentConnection;
  property ConnectTo: String read FConnectTo write FConnectTo;
  property PossibleConnections: TStringList read GetPossibleConnections write SetPossibleConnections;
  property LangStrList: TStringList read FLangStrList write SetLangStrList;
  property OnStatusEvent: TOnStatusEvent read FOnStatusEvent write FOnStatusEvent;
  property RasInstalled: Boolean read GetRasInstalled stored False;
end;

implementation

var
  xSelf: Pointer;

  RasHangUp: function (hConn: THRasConn): Longint; stdcall;
  RasEnumConnections: function (RasConnArray: LPRasConn; var lpcb: Longint; var lpcConnections: Longint): Longint; stdcall;
  RasGetConnectStatus: function (hConn: THRasConn; var lpStatus: TRasConnStatus): Longint; stdcall;
  RasEnumEntries: function (Reserved: PAnsiChar; lpszPhoneBook: PAnsiChar; EntryNamesArray: LPRasEntryNameA; var lpcb: Longint; var lpcEntries: Longint): Longint; stdcall;
  RasGetEntryDialParams: function (lpszPhoneBook: PAnsiChar; var lpDialParams: TRasDialParams; var lpfPassword: LongBool): Longint; stdcall;
  RasGetErrorString: function (ErrorValue: Integer; ErrorString: PAnsiChar; cBufSize: Longint): Longint; stdcall;
  RasDial: function (lpRasDialExt: LPRasDialExtensions; lpszPhoneBook: PAnsiChar; var Params: TRasDialParams; dwNotifierType: Longint; lpNotifier: Pointer; var RasConn: THRasConn): Longint; stdcall;
  RasSetEntryDialParams: function (lpszPhoneBook: PAnsiChar; var lpDialParams: TRasDialParams; fRemovePassword: LongBool): Longint; stdcall;

procedure TJxdDialUp.Timer(Sender: TObject);
begin
  FTimer.Enabled := False;
  if AsyncStatus = False then
    Exit;
  if Assigned(FOnStatusEvent) then
    FOnStatusEvent(TJxdDialUp(xSelf), StatusStr, ErrorStat);
  AsyncStatus:=False;
end;

procedure RasCallback(Msg: Integer; State: TRasConnState; Error: Integer); stdcall;
begin
  while TJxdDialUp(xSelf).AsyncStatus = True do ;
  TJxdDialUp(xSelf).AsyncStatus := True;
  TJxdDialUp(xSelf).FTimer.Enabled := True;
  TJxdDialUp(xSelf).StatusStr := TJxdDialUp(xSelf).StatusString(State, Error, TJxdDialUp(xSelf).ErrorStat);
end;

constructor TJxdDialUp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AsyncStatus := False;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 1;
  FTimer.OnTimer := Timer;
  FPossibleConnections := TStringList.Create;
  FLangStrList := TStringList.Create;
  FLangStrList.Add('Connecting to %s...');
  FLangStrList.Add('Verifying username and password...');
  FLangStrList.Add('An error occured while trying to connect to %s.');

  // Attempt to load the RASAPI32 DLL. If the DLL loads, hRasDLL will
  // be non-zero. Otherwise, hRasDLL will be zero.

  hRasDLL := LoadLibrary('RASAPI32.DLL');

  // Assign function pointers for the RAS functions.

  @RasEnumConnections := GetProcAddress(hRasDLL, 'RasEnumConnectionsA');
  @RasHangUp := GetProcAddress(hRasDLL, 'RasHangUpA');
  @RasGetConnectStatus := GetProcAddress(hRasDLL, 'RasGetConnectStatusA');
  @RasEnumEntries := GetProcAddress(hRasDLL, 'RasEnumEntriesA');
  @RasGetEntryDialParams := GetProcAddress(hRasDLL, 'RasGetEntryDialParamsA');
  @RasGetErrorString := GetProcAddress(hRasDLL, 'RasGetErrorStringA');
  @RasDial := GetProcAddress(hRasDLL, 'RasDialA');
  @RasSetEntryDialParams := GetProcAddress(hRasDLL, 'RasSetEntryDialParamsA');
end;

destructor TJxdDialUp.Destroy;
begin

  // If the RASAPI32 DLL was loaded, then free it.

  if RasInstalled then
    FreeLibrary(hRasDLL);

  FLangStrList.Free;
  FPossibleConnections.Free;
  FTimer.Free;
  inherited Destroy;
end;

function TJxdDialUp.GetRasInstalled: Boolean;
begin
  Result := hRasDLL <> 0;
end;

function TJxdDialUp.GetCurrentConnection: String;
begin
  Result := GetActiveConnection;
end;

procedure TJxdDialUp.SetCurrentConnection(Value: String);
begin
end;

procedure TJxdDialUp.SetPossibleConnections(Value: TStringList);
begin
end;

function TJxdDialUp.GetPossibleConnections: TStringList;
begin
  FPossibleConnections.Clear;
  GetConnections(FPossibleConnections);
  Result := FPossibleConnections;
end;

procedure TJxdDialUp.SetLangStrList(Value: TStringList);
begin
  FLangStrList.Assign(Value);
end;

function TJxdDialUp.GoOnline: Boolean;
var
  hRAS: ThRASConn;
  B: LongBool;
  R: Integer;
  C: array[0..100] of Char;
  DialParams: TRasDialParams;
begin
  Result := False;
  if not RasInstalled then
    exit;

  try
    GoOffline;
    FillChar(DialParams, SizeOf(TRasDialParams), 0);
    DialParams.dwSize := Sizeof(TRasDialParams);
    StrPCopy(DialParams.szEntryName, FConnectTo);
    B := False;
    R := RasGetEntryDialParams(nil, DialParams, B);
    if R <> 0 then
    begin
      Result := False;
      GoOffline;
      if Assigned(FOnStatusEvent) then
        FOnStatusEvent(Self, FLangStrList[28], True);
      Exit;
    end;
//    DialParams.dwSize := Sizeof(TRasDialParams);
//    StrPCopy(DialParams.szUserName, FUsername);
//    StrPCopy(DialParams.szPassword, FPassword);
//    R := RasSetEntryDialParams(nil, DialParams, False);
//    if R <> 0 then
//    begin
//      Result := False;
//      GoOffline;
//      if Assigned(FOnStatusEvent) then
//        FOnStatusEvent(Self, FLangStrList[29], True);
//      Exit;
//    end;
    xSelf := Self;
    AsyncStatus := False;
    hRAS := 0;
    R := RasDial(nil, nil, DialParams, 0, @RasCallback, hRAS);
    if R <> 0 then
    begin
      Result := False;
      RasGetErrorString(R, C, 100);
      GoOffline;
      if Assigned(FOnStatusEvent) then FOnStatusEvent(Self, C, True);
      Exit;
    end;
    Result := True;
  except
    on E: Exception do begin
      GoOffline;
      if Assigned(FOnStatusEvent) then FOnStatusEvent(Self, E.Message, True);
    end;
  end;
end;

procedure TJxdDialUp.GetConnections(var SL: TStringList);
var
  BuffSize, Entries, R, I: Integer;
  Entry: array[1..100] of TRasEntryName;
begin
  if not RasInstalled then
    exit;
  SL.Clear;
  Entry[1].dwSize := SizeOf(TRasEntryName);
  BuffSize := SizeOf(TRasEntryName) * 100;
  R := RasEnumEntries(nil, nil, @Entry[1], BuffSize, Entries);
  if (R = 0) and (Entries > 0) then
    for I := 1 to Entries do
      SL.Add(Entry[I].szEntryName);
end;

function TJxdDialUp.GetActiveConnection: String;
var
  BufSize, NumEntries, I, R: Integer;
  Entries: array[1..100] of TRasConn;
  Stat: TRasConnStatus;
begin
  Result := '';
  if not RasInstalled then
    exit;
  Entries[1].dwSize := SizeOf(TRasConn);
  BufSize := SizeOf(TRasConn)*100;
  FillChar(Stat, Sizeof(TRasConnStatus), 0);
  Stat.dwSize := Sizeof(TRasConnStatus);
  R := RasEnumConnections(@Entries[1], BufSize, NumEntries);
  if R = 0 then
  if NumEntries > 0 then
    for I := 1 to NumEntries do
    begin
      RasGetConnectStatus(Entries[I].HRasConn, Stat);
      if Stat.RasConnState = RASCS_Connected then
        Result := Entries[I].szEntryName;//+' ('+Entries[I].szDeviceName+')'
  end;
end;

procedure TJxdDialUp.GoOffline;
var
  Entries: array[1..100] of TRasConn;
  BufSize, NumEntries, R, I, E: Integer;
begin
  if not RasInstalled then
    exit;
  for E := 0 to 6 do
  begin
      Entries[1].dwSize := SizeOf(TRasConn);
    R := RasEnumConnections(@Entries[1], BufSize, NumEntries);
    if R = 0 then begin
      if NumEntries > 0 then
        for I := 1 to NumEntries do RasHangUp(Entries[I].HRasConn);
  end;
    Application.ProcessMessages;
  end;
end;

function TJxdDialUp.StatusString(State: TRasConnState; Error: Integer; var ES: Boolean): String;
var
  C: array[0..100] of Char;
  S: String;
begin
  S := 'Something went wrong...';
  ES := False;

  if not RasInstalled then
    exit;

  if Error <> 0 then
  begin
    RasGetErrorString(Error, C, 100);
    ES := True;
    S := C;
    end else begin
    case State of
    //connecting
    RASCS_OpenPort, RASCS_PortOpened, RASCS_ConnectDevice, RASCS_DeviceConnected,
    RASCS_AllDevicesConnected, RASCS_PrepareForCallback, RASCS_WaitForModemReset,
    RASCS_WaitForCallback, RASCS_Projected, RASCS_CallbackComplete, RASCS_LogonNetwork,
    RASCS_Interactive, RASCS_CallbackSetByCaller, RASCS_Connected: S := Format(FLangStrList[0], [FConnectTo]);
    //authenticateing
    RASCS_Authenticate, RASCS_StartAuthentication, RASCS_Authenticated: S := FLangStrList[1];
    //error
    RASCS_AuthNotify, RASCS_AuthRetry, RASCS_AuthCallback, RASCS_AuthChangePassword,
    RASCS_AuthProject, RASCS_AuthLinkSpeed, RASCS_AuthAck, RASCS_ReAuthenticate,
    RASCS_RetryAuthentication, RASCS_Disconnected, RASCS_PasswordExpired: S := Format(FLangStrList[2], [FConnectTo]);
    end;
  end;
  Result := S;
end;

end.