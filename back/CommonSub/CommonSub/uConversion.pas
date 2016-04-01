unit uConversion;

interface
uses SysUtils, Windows;

function ConverByte(const AByteCount: Int64): string;
function ConverSpeek(const AByteCount: Cardinal): string;

function IsNumber(const AChar: char): Boolean; overload; //是否是数字
function IsNumber(const AByte: Byte): Boolean; overload;
function IsLowerLetter(const AChar: char): Boolean; overload; //是否为小字字母
function IsLowerLetter(const AByte: Byte): Boolean; overload;
function IsUpperLetter(const AChar: char): Boolean; overload; //是否为大字字母
function IsUpperLetter(const AByte: Byte): Boolean; overload;
function IsLetter(const AChar: char): Boolean; overload; //是否为字母
function IsLetter(const AByte: Byte): Boolean; overload;
function IsLetter(const AStrText: string): Boolean; overload;
function IsVisibleChar(const AChar: Char): Boolean; overload;    //是否为可见字符
function IsVisibleChar(const AChar: Byte): Boolean; overload;

//时间戳
function GetTimeStamp: Cardinal;
function SystemTimeToTimeStamp(const ASystemTime: TSystemTime): Cardinal;
function TimeStampToStr(ATimeStamp: Cardinal): string;

implementation

function ConverByte(const AByteCount: Int64): string;
begin
  if AByteCount <= 0 then
    Result := '0'
  else if AByteCount > 1024 * 1024 * 1024 then
    Result := Format('%0.2fG', [AByteCount / (1024 * 1024 * 1024)])
  else if AByteCount > 1024 * 1024 then
    Result := Format('%0.2fM', [AByteCount / (1024 * 1024)])
  else if AByteCount > 1024 then
    Result := Format('%0.2fK', [AByteCount / 1024])
  else
    Result := Format('%3dB', [AByteCount]);
end;

function ConverSpeek(const AByteCount: Cardinal): string;
begin
  if AByteCount < 1024 then
    Result := Format('%dB/S', [AByteCount])
  else
    Result := Format('%dK/S', [AByteCount div 1024]);
end;

function IsCharInArea(const AChar: char; const AMin, AMax: Byte): Boolean; inline; overload;
var
  cByte: Byte;
begin
  cByte := Ord(AChar);
  if (cByte >= AMin) and (cByte <= AMax) then
    Result := True
  else
    Result := False;
end;

function IsCharInArea(const AByte: Byte; const AMin, AMax: Byte): Boolean; inline; overload;
begin
  if (AByte >= AMin) and (AByte <= AMax) then
    Result := True
  else
    Result := False;
end;

function IsNumber(const AChar: char): Boolean;
begin
  Result := IsCharInArea(AChar, $30, $39);
end;

function IsNumber(const AByte: Byte): Boolean;
begin
  Result := IsCharInArea(AByte, $30, $39);
end;

function IsLowerLetter(const AChar: char): Boolean;
begin
  Result := IsCharInArea(AChar, $61, $7A);
end;

function IsLowerLetter(const AByte: Byte): Boolean; overload;
begin
  Result := IsCharInArea(AByte, $61, $7A);
end;

function IsUpperLetter(const AChar: char): Boolean;
begin
  Result := IsCharInArea(AChar, $41, $5A);
end;

function IsUpperLetter(const AByte: Byte): Boolean; overload;
begin
  Result := IsCharInArea(AByte, $41, $5A);
end;

function IsLetter(const AChar: char): Boolean; overload;
begin
  Result := IsLowerLetter(AChar) or IsUpperLetter(AChar);
end;

function IsLetter(const AStrText: string): Boolean; overload;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(AStrText) do
    if not IsLetter(AStrText[i]) then
    begin
      Result := False;
      Break;
    end;
end;

function IsLetter(const AByte: Byte): Boolean; overload;
begin
  Result := IsLowerLetter(AByte) or IsUpperLetter(AByte);
end;

function IsVisibleChar(const AChar: Byte): Boolean; overload;
begin
  Result := AChar in [33..126];
end;

function IsVisibleChar(const AChar: Char): Boolean;
begin
  Result := IsVisibleChar(AChar);
end;

function GetTimeStamp: Cardinal;
var
  I: Integer;
  SystemTime: TSystemTime;
  DayTable: PDayTable;
begin
  GetLocalTime(SystemTime);
  for I := 2000 to SystemTime.wYear - 1 do
    if IsLeapYear(I) then
      Inc(SystemTime.wDay, 366)
    else
      Inc(SystemTime.wDay, 365);
  DayTable := @MonthDays[IsLeapYear(SystemTime.wYear)];
  for I := 1 to SystemTime.wMonth - 1 do
    Inc(SystemTime.wDay, DayTable^[I]);
  Result := (SystemTime.wDay - 1) * 24 * 3600 + SystemTime.wHour * 3600 +
    SystemTime.wMinute * 60 + SystemTime.wSecond;
end;

function SystemTimeToTimeStamp(const ASystemTime: TSystemTime): Cardinal;
var
  I: Integer;
  DayTable: PDayTable;
  wDay: Cardinal;
begin
  wDay := ASystemTime.wDay;
  for I := 2000 to ASystemTime.wYear - 1 do
    if IsLeapYear(I) then
      Inc(wDay, 366)
    else
      Inc(wDay, 365);
  DayTable := @MonthDays[IsLeapYear(ASystemTime.wYear)];
  for I := 1 to ASystemTime.wMonth - 1 do
    Inc(wDay, DayTable^[I]);
  Result := (wDay - 1) * 24 * 3600 + ASystemTime.wHour * 3600 +
    ASystemTime.wMinute * 60 + ASystemTime.wSecond;  
end;

procedure DivMod(Dividend, Divisor: Integer; var Result, Remainder: Integer);
begin
  Result := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
end;

function TimeStampToStr(ATimeStamp: Cardinal): string;
var
  wYear, wMonth, wDay, wHour, wMinute, wSecond: Integer;
  DayTable: PDayTable;
  K: Integer;
begin
  DivMod(ATimeStamp, 3600 * 24, wDay, Integer(ATimeStamp));
  DivMod(ATimeStamp, 3600, wHour, Integer(ATimeStamp));
  DivMod(ATimeStamp, 60, wMinute, wSecond);
  wYear := 2000;
  while True do
  begin
    if IsLeapYear(wYear) then
      K := 366
    else
      K := 365;
    if wDay >= K then
    begin
      Dec(wDay, K);
      Inc(wYear);
    end
    else
      Break;
  end;
  DayTable := @MonthDays[IsLeapYear(wYear)];
  for wMonth := 1 to 12 do
  begin
    if wDay >= DayTable^[wMonth] then
      Dec(wDay, DayTable^[wMonth])
    else
      Break;
  end;
  Inc(wDay);
  Result := Format('%0.4d-%0.2d-%0.2d %0.2d:%0.2d:%0.2d',
    [wYear, wMonth, wDay, wHour, wMinute, wSecond]);
end;
end.
