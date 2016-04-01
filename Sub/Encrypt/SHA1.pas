unit SHA1;

interface

uses
  SysUtils, Windows, Classes;

type

  PSHA1Digest = ^TSHA1Digest;
  TSHA1Digest = packed record
    case Integer of
      0: (A, B, C, D, E: DWORD);
      1: (V: array[0..19] of Byte);
  end;

  TSHA1Context = record
    Hash: array[0..4] of DWORD;
    Hi, Lo: Integer;
    Buffer: array[0..63] of Byte;
    Index: Integer;
  end;

function SHA1Buffer(const Buffer; Size: Integer): TSHA1Digest;
function SHA1DigestCompare(const Digest1, Digest2: TSHA1Digest): Boolean;
function SHA1Stream(const Stream: TStream; var Digest: TSHA1Digest; PAbort: PBoolean = nil): Boolean;
function SHA1File(const FileName: string; var Digest: TSHA1Digest; PAbort: PBoolean = nil): Boolean;
function SHA1String(const S: string): TSHA1Digest;
function EmptySHA1(const Digest: TSHA1Digest): Boolean;
function SHA1DigestToStr(const Digest: TSHA1Digest): string;
function StrToSHA1Digest(const Str: string): TSHA1Digest;

//******************************************************************************
implementation
{$R-}

function LRot16(X: word; c: Integer): word; assembler;
asm
  mov ecx,&c
  mov ax,&X
  rol ax,cl
  mov &Result,ax
end;

function RRot16(X: word; c: Integer): word; assembler;
asm
  mov ecx,&c
  mov ax,&X
  ror ax,cl
  mov &Result,ax
end;

function LRot32(X: DWORD; c: Integer): DWORD; register; assembler;
asm
  mov ecx, edx
  rol eax, cl
end;

function RRot32(X: DWORD; c: Integer): DWORD; register; assembler;
asm
  mov ecx, edx
  ror eax, cl
end;

procedure XorBlock(I1, I2, O1: PByteArray; Len: Integer);
var
  i: Integer;
begin
  for i := 0 to Len - 1 do
    O1[i] := I1[i] xor I2[i];
end;

procedure IncBlock(P: PByteArray; Len: Integer);
begin
  Inc(P[Len - 1]);
  if (P[Len - 1] = 0) and (Len > 1) then
    IncBlock(P, Len - 1);
end;

//******************************************************************************

function F1(x, y, z: DWORD): DWORD;
begin
  Result := z xor (x and (y xor z));
end;

function F2(x, y, z: DWORD): DWORD;
begin
  Result := x xor y xor z;
end;

function F3(x, y, z: DWORD): DWORD;
begin
  Result := (x and y) or (z and (x or y));
end;

//******************************************************************************

function RB(A: DWORD): DWORD;
begin
  Result := (A shr 24) or ((A shr 8) and $FF00) or ((A shl 8) and $FF0000) or (A shl 24);
end;

procedure SHA1Compress(var Data: TSHA1Context);
var
  A, B, C, D, E, T: DWORD;
  W: array[0..79] of DWORD;
  i: Integer;
begin
  Move(Data.Buffer, W, Sizeof(Data.Buffer));
  for i := 0 to 15 do
    W[i] := RB(W[i]);
  for i := 16 to 79 do
    W[i] := LRot32(W[i - 3] xor W[i - 8] xor W[i - 14] xor W[i - 16], 1);
  A := Data.Hash[0]; B := Data.Hash[1]; C := Data.Hash[2]; D := Data.Hash[3]; E := Data.Hash[4];
  for i := 0 to 19 do
  begin
    T := LRot32(A, 5) + F1(B, C, D) + E + W[i] + $5A827999;
    E := D; D := C; C := LRot32(B, 30); B := A; A := T;
  end;
  for i := 20 to 39 do
  begin
    T := LRot32(A, 5) + F2(B, C, D) + E + W[i] + $6ED9EBA1;
    E := D; D := C; C := LRot32(B, 30); B := A; A := T;
  end;
  for i := 40 to 59 do
  begin
    T := LRot32(A, 5) + F3(B, C, D) + E + W[i] + $8F1BBCDC;
    E := D; D := C; C := LRot32(B, 30); B := A; A := T;
  end;
  for i := 60 to 79 do
  begin
    T := LRot32(A, 5) + F2(B, C, D) + E + W[i] + $CA62C1D6;
    E := D; D := C; C := LRot32(B, 30); B := A; A := T;
  end;
  Data.Hash[0] := Data.Hash[0] + A;
  Data.Hash[1] := Data.Hash[1] + B;
  Data.Hash[2] := Data.Hash[2] + C;
  Data.Hash[3] := Data.Hash[3] + D;
  Data.Hash[4] := Data.Hash[4] + E;
  FillChar(W, Sizeof(W), 0);
  FillChar(Data.Buffer, Sizeof(Data.Buffer), 0);
end;

//******************************************************************************

procedure SHA1Init(var Context: TSHA1Context);
begin
  Context.Hi := 0; Context.Lo := 0;
  Context.Index := 0;
  FillChar(Context.Buffer, Sizeof(Context.Buffer), 0);
  Context.Hash[0] := $67452301;
  Context.Hash[1] := $EFCDAB89;
  Context.Hash[2] := $98BADCFE;
  Context.Hash[3] := $10325476;
  Context.Hash[4] := $C3D2E1F0;
end;

//******************************************************************************

procedure SHA1UpdateLen(var Context: TSHA1Context; Len: Integer);
var
  i, k: Integer;
begin
  for k := 0 to 7 do
  begin
    i := Context.Lo;
    Inc(Context.Lo, Len);
    if Context.Lo < i then
      Inc(Context.Hi);
  end;
end;

//******************************************************************************

procedure SHA1Update(var Context: TSHA1Context; Buffer: pointer; Len: Integer);
type
  PByte = ^Byte;
begin
  SHA1UpdateLen(Context, Len);
  while Len > 0 do
  begin
    Context.Buffer[Context.Index] := PByte(Buffer)^;
    Inc(PByte(Buffer));
    Inc(Context.Index);
    Dec(Len);
    if Context.Index = 64 then
    begin
      Context.Index := 0;
      SHA1Compress(Context);
    end;
  end;
end;

//******************************************************************************

procedure SHA1Final(var Context: TSHA1Context; var Digest: TSHA1Digest);
type
  PDWORD = ^DWORD;
begin
  Context.Buffer[Context.Index] := $80;
  if Context.Index >= 56 then
    SHA1Compress(Context);
  PDWORD(@Context.Buffer[56])^ := RB(Context.Hi);
  PDWORD(@Context.Buffer[60])^ := RB(Context.Lo);
  SHA1Compress(Context);
  Context.Hash[0] := RB(Context.Hash[0]);
  Context.Hash[1] := RB(Context.Hash[1]);
  Context.Hash[2] := RB(Context.Hash[2]);
  Context.Hash[3] := RB(Context.Hash[3]);
  Context.Hash[4] := RB(Context.Hash[4]);
  Move(Context.Hash, Digest, Sizeof(Digest));
  FillChar(Context, Sizeof(Context), 0);
end;

function SHA1Buffer(const Buffer; Size: Integer): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PByteArray(@Buffer), Size);
  SHA1Final(Context, Result);
end;

function SHA1DigestCompare(const Digest1, Digest2: TSHA1Digest): Boolean;
begin
  Result := CompareMem(@Digest1, @Digest2, Sizeof(Digest1));
end;

function SHA1Stream(const Stream: TStream; var Digest: TSHA1Digest; PAbort: PBoolean = nil): Boolean;
var
  Context: TSHA1Context;
  Buffer: array[0..4095] of Byte;
  Size: Integer;
  ReadBytes: Integer;
  TotalBytes: Integer;
  SavePos: Integer;
  K: Integer;
begin
  Result := True;
  SHA1Init(Context);
  Size := Stream.Size;
  SavePos := Stream.Position;
  TotalBytes := 0;
  K := 0;
  try
    Stream.Seek(0, soFromBeginning);
    repeat
      ReadBytes := Stream.Read(Buffer, SizeOf(Buffer));
      Inc(TotalBytes, ReadBytes);
      SHA1Update(Context, @Buffer, ReadBytes);
      Inc(K);
      if K mod 3 = 0 then
      begin
        if (PAbort <> nil) and PAbort^ then //是否停止计算
        begin
          Result := False;
          Exit;
        end;
        Sleep(1);
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
  finally
    Stream.Seek(SavePos, soFromBeginning);
  end;
  SHA1Final(Context, Digest);
end;

function SHA1File(const FileName: string; var Digest: TSHA1Digest; PAbort: PBoolean = nil): Boolean;
var
  hFile: THandle;
  Stream: TFileStream;
begin
  Result := False;
  //这样打开，可以优化文件读取，而且文件打开失败(不存在)也不会触发异常
  hFile := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if hFile = INVALID_HANDLE_VALUE then //文件打开失败
    Exit;
  Stream := TFileStream.Create(hFile);
  try
    Result := SHA1Stream(Stream, Digest, PAbort);
  finally
    Stream.Free;
  end;
end;

function SHA1String(const S: string): TSHA1Digest;
begin
  Result := SHA1Buffer(PChar(S)^, Length(S));
end;

function EmptySHA1(const Digest: TSHA1Digest): Boolean;
begin
  with Digest do
    Result := (A = 0) and (B = 0) and (C = 0) and (D = 0) and (E = 0);
end;

function SHA1DigestToStr(const Digest: TSHA1Digest): string;
begin
  SetLength(Result, Sizeof(TSHA1Digest) * 2);
  BinToHex(@Digest, PChar(Result), Sizeof(TSHA1Digest));
end;

function StrToSHA1Digest(const Str: string): TSHA1Digest;
begin
  ZeroMemory(@Result, Sizeof(TSHA1Digest));
  if Length(Str) <> Sizeof(TSHA1Digest) * 2 then
    Exit;
  HexToBin(PChar(Str), @Result, Sizeof(TSHA1Digest));
end;


end.

