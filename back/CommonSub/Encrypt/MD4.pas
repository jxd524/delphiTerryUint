unit MD4;

interface

uses
  Windows, Classes, Sysutils, Forms;//,DebugLog;


type

  PMD4Digest = ^TMD4Digest;
  TMD4Digest = packed record
    case Integer of
      0: (A, B, C, D: DWORD);
      1: (v: array[0..15] of Byte);
  end;

  TMD4Context = record
    Hash: array[0..3] of DWORD;
    Hi, Lo: longword;
    Buffer: array[0..63] of Byte;
    Index: DWord;
  end;

const
  CEmptyMD4: TMD4Digest = (A: 0; B: 0; C: 0; D: 0);

function MD4String(const S: string): TMD4Digest;
function MD4File(const FileName: string; var Digest: TMD4Digest; PAbort: PBoolean = nil; AFastCalc: Boolean = True): Boolean;
function MD4Stream(const Stream: TStream; var Digest: TMD4Digest; PAbort: PBoolean = nil; AFastCalc: Boolean = True): Boolean;
function MD4Buffer(const Buffer; Size: Integer): TMD4Digest;
function MD4DigestToStr(const Digest: TMD4Digest): string;
function StrToMD4Digest(str: string): TMD4Digest;
function MD4DigestCompare(const Digest1, Digest2: TMD4Digest): Boolean;
function EmptyMD4(const Digest: TMD4Digest): Boolean;
procedure MD4Init(var Context: TMD4Context);
procedure MD4Update(var Context: TMD4Context; const Buffer; Len: longword);
procedure MD4Final(var Context: TMD4Context; var Digest: TMD4Digest);

/// <summary>
/// 计算WebHash，取三块数据进行计算，每块大小小于等于256K
///  如果只有一块就只算一块，如果只有两块就算两块，多于三块就算
///  前中后三块
/// </summary>
/// <param name="FileName">文件名</param>
/// <param name="WebHash">WebHash</param>
/// <returns>成功返回True、失败False</returns>
function GetWebHash(const FileName: string; var WebHash: TMD4Digest):Boolean;

/// <summary>
/// 计算分段Hash
/// </summary>
/// <param name="Stream">文件流</param>
/// <param name="AStartPos">计算起始位置</param>
/// <param name="ALen">计算数据长度</param>
/// <param name="ASegmentHash">分段Hash</param>
/// <param name="PAbort">进程结束标志</param>
/// <param name="AFastCalc">是否快速计算</param>
/// <returns>成功返回True、失败False</returns>
function ed2kSegmentHash(const Stream: TStream; AStartPos: Int64; ALen: Int64; var ASegmentHash: TMD4Digest;
  PAbort: PBoolean = nil; AFastCalc: Boolean = True): Boolean;

/// <summary>
/// 计算ed2kHash
/// </summary>
/// <param name="FileName">计算名</param>
/// <param name="Digest">计算结果Hash</param>
/// <param name="ASegmentHash">分块Hash串</param>
/// <param name="PAbort">进程结束标志</param>
/// <param name="AFastCalc">是否快速计算</param>
/// <returns>成功返回True、失败False</returns>
function ed2kFile(const FileName: string; var Digest: TMD4Digest;
  ASegmentHash: PChar; PAbort: PBoolean = nil; AFastCalc: Boolean = True): Boolean;
/// <summary>
/// 计算ed2kHash
/// </summary>
/// <param name="Stream">文件流</param>
/// <param name="Digest">计算结果Hash</param>
/// <param name="ASegmentHash">分块Hash串</param>
/// <param name="PAbort">进程结束标志</param>
/// <param name="AFastCalc">是否快速计算</param>
/// <returns>成功返回True、失败False</returns>
function ed2kStream(const Stream: TStream; var Digest: TMD4Digest;
  ASegmentHash: PChar; PAbort: PBoolean = nil; AFastCalc: Boolean = True): Boolean;


implementation
{$R-}{$Q-}

function LRot32(a, b: longword): longword;
begin
  Result := (a shl b) or (a shr (32 - b));
end;

procedure MD4Compress(var Context: TMD4Context);
var
  Data: array[0..15] of dword;
  A, B, C, D: dword;
begin
  Move(Context.Buffer, Data, Sizeof(Data));
  A := Context.Hash[0];
  B := Context.Hash[1];
  C := Context.Hash[2];
  D := Context.Hash[3];

  A := LRot32(A + (D xor (B and (C xor D))) + Data[0], 3);
  D := LRot32(D + (C xor (A and (B xor C))) + Data[1], 7);
  C := LRot32(C + (B xor (D and (A xor B))) + Data[2], 11);
  B := LRot32(B + (A xor (C and (D xor A))) + Data[3], 19);
  A := LRot32(A + (D xor (B and (C xor D))) + Data[4], 3);
  D := LRot32(D + (C xor (A and (B xor C))) + Data[5], 7);
  C := LRot32(C + (B xor (D and (A xor B))) + Data[6], 11);
  B := LRot32(B + (A xor (C and (D xor A))) + Data[7], 19);
  A := LRot32(A + (D xor (B and (C xor D))) + Data[8], 3);
  D := LRot32(D + (C xor (A and (B xor C))) + Data[9], 7);
  C := LRot32(C + (B xor (D and (A xor B))) + Data[10], 11);
  B := LRot32(B + (A xor (C and (D xor A))) + Data[11], 19);
  A := LRot32(A + (D xor (B and (C xor D))) + Data[12], 3);
  D := LRot32(D + (C xor (A and (B xor C))) + Data[13], 7);
  C := LRot32(C + (B xor (D and (A xor B))) + Data[14], 11);
  B := LRot32(B + (A xor (C and (D xor A))) + Data[15], 19);

  A := LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[0] + $5A827999, 3);
  D := LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[4] + $5A827999, 5);
  C := LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[8] + $5A827999, 9);
  B := LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[12] + $5A827999, 13);
  A := LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[1] + $5A827999, 3);
  D := LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[5] + $5A827999, 5);
  C := LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[9] + $5A827999, 9);
  B := LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[13] + $5A827999, 13);
  A := LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[2] + $5A827999, 3);
  D := LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[6] + $5A827999, 5);
  C := LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[10] + $5A827999, 9);
  B := LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[14] + $5A827999, 13);
  A := LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[3] + $5A827999, 3);
  D := LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[7] + $5A827999, 5);
  C := LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[11] + $5A827999, 9);
  B := LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[15] + $5A827999, 13);

  A := LRot32(A + (B xor C xor D) + Data[0] + $6ED9EBA1, 3);
  D := LRot32(D + (A xor B xor C) + Data[8] + $6ED9EBA1, 9);
  C := LRot32(C + (D xor A xor B) + Data[4] + $6ED9EBA1, 11);
  B := LRot32(B + (C xor D xor A) + Data[12] + $6ED9EBA1, 15);
  A := LRot32(A + (B xor C xor D) + Data[2] + $6ED9EBA1, 3);
  D := LRot32(D + (A xor B xor C) + Data[10] + $6ED9EBA1, 9);
  C := LRot32(C + (D xor A xor B) + Data[6] + $6ED9EBA1, 11);
  B := LRot32(B + (C xor D xor A) + Data[14] + $6ED9EBA1, 15);
  A := LRot32(A + (B xor C xor D) + Data[1] + $6ED9EBA1, 3);
  D := LRot32(D + (A xor B xor C) + Data[9] + $6ED9EBA1, 9);
  C := LRot32(C + (D xor A xor B) + Data[5] + $6ED9EBA1, 11);
  B := LRot32(B + (C xor D xor A) + Data[13] + $6ED9EBA1, 15);
  A := LRot32(A + (B xor C xor D) + Data[3] + $6ED9EBA1, 3);
  D := LRot32(D + (A xor B xor C) + Data[11] + $6ED9EBA1, 9);
  C := LRot32(C + (D xor A xor B) + Data[7] + $6ED9EBA1, 11);
  B := LRot32(B + (C xor D xor A) + Data[15] + $6ED9EBA1, 15);

  Inc(Context.Hash[0], A);
  Inc(Context.Hash[1], B);
  Inc(Context.Hash[2], C);
  Inc(Context.Hash[3], D);
  Context.Index := 0;
  FillChar(Context.Buffer, Sizeof(Context.Buffer), 0);
end;

procedure MD4Burn(var Context: TMD4Context);
begin
  Context.Hi := 0; Context.Lo := 0;
  Context.Index := 0;
  FillChar(Context.Buffer, Sizeof(Context.Buffer), 0);
  FillChar(Context.Hash, Sizeof(Context.Hash), 0);
end;

procedure MD4Init(var Context: TMD4Context);
begin
  MD4Burn(Context);
  Context.Hash[0] := $67452301;
  Context.Hash[1] := $EFCDAB89;
  Context.Hash[2] := $98BADCFE;
  Context.Hash[3] := $10325476;
end;


procedure MD4Update(var Context: TMD4Context; const Buffer; Len: longword);
var
  PBuf: ^byte;
begin
  Inc(Context.Hi, Len shr 29);
  Inc(Context.Lo, Len * 8);
  if Context.Lo < (Len * 8) then
    Inc(Context.Hi);

  PBuf := @Buffer;
  while Len > 0 do
  begin
    if (Sizeof(Context.Buffer) - Context.Index) <= DWord(Len) then
    begin
      Move(PBuf^, Context.Buffer[Context.Index], Sizeof(Context.Buffer) - Context.Index);
      Dec(Len, Sizeof(Context.Buffer) - Context.Index);
      Inc(PBuf, Sizeof(Context.Buffer) - Context.Index);
      MD4Compress(Context);
    end
    else
    begin
      Move(PBuf^, Context.Buffer[Context.Index], Len);
      Inc(Context.Index, Len);
      Len := 0;
    end;
  end;
end;

procedure MD4Final(var Context: TMD4Context; var Digest: TMD4Digest);
begin
  Context.Buffer[Context.Index] := $80;
  if Context.Index >= 56 then
    MD4Compress(Context);
  PDWord(@Context.Buffer[56])^ := Context.Lo;
  PDWord(@Context.Buffer[60])^ := Context.Hi;
  MD4Compress(Context);
  Move(Context.Hash, Digest, Sizeof(Context.Hash));
  MD4Burn(Context);
end;

function MD4Buffer(const Buffer; Size: Integer): TMD4Digest;
var
  Context: TMD4Context;
begin
  MD4Init(Context);
  MD4Update(Context, Buffer, Size);
  MD4Final(Context, Result);
end;

function MD4String(const S: string): TMD4Digest;
begin
  Result := MD4Buffer(PChar(S)^, Length(S));
end;

function MD4File(const FileName: string; var Digest: TMD4Digest; PAbort: PBoolean; AFastCalc: Boolean): Boolean;
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
    Result := MD4Stream(Stream, Digest, PAbort, AFastCalc);
  finally
    Stream.Free;
  end;
end;

function MD4Stream(const Stream: TStream; var Digest: TMD4Digest; PAbort: PBoolean; AFastCalc: Boolean): Boolean;
var
  Context: TMD4Context;
  Buffer: array[0..16383] of Byte;
  ReadBytes: Int64;
  SavePos: Int64;
begin
  Result := True;
  MD4Init(Context);
  SavePos := Stream.Position;
  try
    Stream.Seek(0, soFromBeginning);
    repeat
      ReadBytes := Stream.Read(Buffer, SizeOf(Buffer));
      MD4Update(Context, Buffer, ReadBytes);
      if (PAbort <> nil) and PAbort^ then //是否停止计算
      begin
        Result := False;
        Exit;
      end;
      if (not AFastCalc) then
      begin
        if GetCurrentThreadID = MainThreadID then
          Application.ProcessMessages;
        Sleep(3);
      end;
    until (ReadBytes <> SizeOf(Buffer));
  finally
    Stream.Seek(SavePos, soFromBeginning);
  end;
  MD4Final(Context, Digest);
end;

function MD4DigestToStr(const Digest: TMD4Digest): string;
begin
  SetLength(Result, Sizeof(TMD4Digest) * 2);
  BinToHex(@Digest, PChar(Result), Sizeof(TMD4Digest));
end;

function StrToMD4Digest(str: string): TMD4Digest;
begin
  ZeroMemory(@Result, Sizeof(TMD4Digest));
  if Length(Str) <> Sizeof(TMD4Digest) * 2 then
    Exit;
  HexToBin(PChar(Str), @Result, Sizeof(TMD4Digest));
end;

function MD4DigestCompare(const Digest1, Digest2: TMD4Digest): Boolean;
begin
  Result := CompareMem(@Digest1, @Digest2, Sizeof(TMD4Digest));
end;

function EmptyMD4(const Digest: TMD4Digest): Boolean;
begin
  Result := (Digest.A = 0) and (Digest.B = 0) and (Digest.C = 0) and (Digest.D = 0);
end;

function ed2kFile(const FileName: string; var Digest: TMD4Digest;
  ASegmentHash: PChar; PAbort: PBoolean = nil; AFastCalc: Boolean = True): Boolean;
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
    Result := ed2kStream(Stream, Digest, ASegmentHash, PAbort, AFastCalc);
  finally
    Stream.Free;
  end;
end;

function GetWebHash(const FileName: string; var WebHash: TMD4Digest):Boolean;
type
  TSegment = record
    SegmentID: Integer; //从0开始
    SegmentSize: Int64;
    SegmentHash: TMD4Digest;
  end;
  TFileSegmentInfo = record
    FileSize: Int64;
    SegmentSize: Int64;
    SegmentCount: Integer;
    Segments: array of TSegment;
  end;
const
  CSegmentSize = 256*1024;
var
  hFile: THandle;
  Stream: TFileStream;
  AInfo: TFileSegmentInfo;
  MemStream: TMemoryStream;
  I,ModCount:Integer;
begin
  Result := False;
  //这样打开，可以优化文件读取，而且文件打开失败(不存在)也不会触发异常
  hFile := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if hFile = INVALID_HANDLE_VALUE then //文件打开失败
    Exit;
  Stream := TFileStream.Create(hFile);
  MemStream := TMemoryStream.Create;
  try
    AInfo.FileSize := Stream.Size;
    ModCount := 0;
    AInfo.SegmentSize := CSegmentSize;
    if AInfo.FileSize <= CSegmentSize then
    begin
      AInfo.SegmentCount := 1;
      AInfo.SegmentSize := AInfo.FileSize;
    end else
    begin
      AInfo.SegmentSize := CSegmentSize;
      ModCount := AInfo.FileSize mod AInfo.SegmentSize;
      AInfo.SegmentCount := AInfo.FileSize div AInfo.SegmentSize;
      if ModCount > 0 then
        Inc(AInfo.SegmentCount);
    end;

    SetLength(AInfo.Segments, AInfo.SegmentCount);
    ZeroMemory(@AInfo.Segments[0], Sizeof(TSegment) * AInfo.SegmentCount);

    for I := 0 to AInfo.SegmentCount - 1 do
    begin
      AInfo.Segments[I].SegmentID := I;
      if (I < AInfo.SegmentCount - 1) or (ModCount = 0) then
      begin
        AInfo.Segments[I].SegmentSize := AInfo.SegmentSize;
        if (I =0) or (I = AInfo.SegmentCount - 1) or
          (I = ((AInfo.SegmentCount - 1) div 2)) then
        begin
          Result := ed2kSegmentHash(Stream, I * CSegmentSize, AInfo.Segments[I].SegmentSize, AInfo.Segments[I].SegmentHash);
          if not Result then
            Exit;
          MemStream.WriteBuffer(AInfo.Segments[I].SegmentHash, SizeOf(TMD4Digest));
          //AddLogStrToFile(Format('第%d块Hash：%s',[I,MD4DigestToStr(AInfo.Segments[I].SegmentHash)]));
        end;
      end
      else
      begin
        AInfo.Segments[I].SegmentSize := ModCount;
        if (I =0) or (I = AInfo.SegmentCount - 1) or
          (I = ((AInfo.SegmentCount - 1) div 2)) then
        begin
          Result := ed2kSegmentHash(Stream, I * CSegmentSize, AInfo.Segments[I].SegmentSize, AInfo.Segments[I].SegmentHash);
          if not Result then
            Exit;
          MemStream.WriteBuffer(AInfo.Segments[I].SegmentHash, SizeOf(TMD4Digest));
          //AddLogStrToFile(Format('第%d块Hash：%s',[I,MD4DigestToStr(AInfo.Segments[I].SegmentHash)]));
        end;
      end;
    end;
    Result := MD4Stream(MemStream,WebHash);
    //AddLogStrToFile(Format('WebHash：%s',[MD4DigestToStr(WebHash)]));

  finally
    Stream.Free;
    MemStream.free;
  end;
end;

function ed2kSegmentHash(const Stream: TStream; AStartPos: Int64; ALen: Int64; var ASegmentHash: TMD4Digest;
  PAbort: PBoolean = nil; AFastCalc: Boolean = True): Boolean;
var
  ReadBytes: Integer;
  LeftBytes: Integer;
  Context: TMD4Context;
  SavePos: Int64;
  Buffer: array[0..16383] of Byte;
begin
  Result := False;
  SavePos := Stream.Position;
  if (Stream = nil) or (ALen <= 0) then
    Exit;
  try
    LeftBytes := ALen;
    ZeroMemory(@ASegmentHash, SizeOf(TMD4Digest));
    MD4Init(Context);
    Stream.Seek(AStartPos, soFromBeginning);
    repeat
      if LeftBytes >= SizeOf(Buffer) then
        ReadBytes := Stream.Read(Buffer, SizeOf(Buffer))
      else
        ReadBytes := Stream.Read(Buffer, LeftBytes);
      LeftBytes := LeftBytes - ReadBytes;
      MD4Update(Context, Buffer, ReadBytes);
      if (PAbort <> nil) and PAbort^ then //是否停止计算
      begin
        Exit;
      end;
      if (not AFastCalc) then
      begin
        if GetCurrentThreadID = MainThreadID then
          Application.ProcessMessages;
        Sleep(3);
      end;
    until (ReadBytes = 0) or (LeftBytes <= 0);
    MD4Final(Context, ASegmentHash);
    Result := True;
  finally
    Stream.Seek(SavePos, soFromBeginning);
  end;
end;

function ed2kStream(const Stream: TStream; var Digest: TMD4Digest;
  ASegmentHash: PChar; PAbort: PBoolean = nil; AFastCalc: Boolean = True): Boolean;
type
  TSegment = record
    SegmentID: Integer; //从0开始
    SegmentSize: Int64;
    SegmentHash: TMD4Digest;
  end;
  TFileSegmentInfo = record
    FileSize: Int64;
    SegmentSize: Int64;
    SegmentCount: Integer;
    Segments: array of TSegment;
  end;
const
  CSegmentSize = 9728000;
var
  MemStream: TMemoryStream;
  AInfo: TFileSegmentInfo;
  I: Integer;

  ModCount: Integer;
  Buffer: array[0..16383] of Byte;
  SavePos: Int64;

  function CalcHash(ASegmentID: Integer; var ASegmentHash: TMD4Digest): Boolean;
  var
    ReadBytes: Integer;
    LeftBytes: Integer;
    Context: TMD4Context;
  begin
    Result := True;
    if (Stream = nil) or (ASegmentID < 0) or (ASegmentID >= AInfo.SegmentCount) then
      Exit;
    LeftBytes := AInfo.Segments[ASegmentID].SegmentSize;
    ZeroMemory(@ASegmentHash, SizeOf(TMD4Digest));
    MD4Init(Context);
    Stream.Seek(ASegmentID * AInfo.SegmentSize, soFromBeginning);
    repeat
      if LeftBytes >= SizeOf(Buffer) then
        ReadBytes := Stream.Read(Buffer, SizeOf(Buffer))
      else
        ReadBytes := Stream.Read(Buffer, LeftBytes);
      LeftBytes := LeftBytes - ReadBytes;
      MD4Update(Context, Buffer, ReadBytes);
      if (PAbort <> nil) and PAbort^ then //是否停止计算
      begin
        Result := False;
        Exit;
      end;
      if (not AFastCalc) then
      begin
        if GetCurrentThreadID = MainThreadID then
          Application.ProcessMessages;
        Sleep(3);
      end;
    until (ReadBytes = 0) or (LeftBytes <= 0);
    MD4Final(Context, ASegmentHash);
  end;
begin
  Result := True;
  SavePos := Stream.Position;
  MemStream := TMemoryStream.Create;
  try
    AInfo.FileSize := Stream.Size;
    ModCount := 0;
    AInfo.SegmentSize := CSegmentSize;
    if AInfo.FileSize <= CSegmentSize then
    begin
      AInfo.SegmentCount := 1;
      AInfo.SegmentSize := AInfo.FileSize;
    end else
    begin
      AInfo.SegmentSize := CSegmentSize;
      ModCount := AInfo.FileSize mod AInfo.SegmentSize;
      AInfo.SegmentCount := AInfo.FileSize div AInfo.SegmentSize;
      if ModCount > 0 then
        Inc(AInfo.SegmentCount);
    end;

    SetLength(AInfo.Segments, AInfo.SegmentCount);
    ZeroMemory(@AInfo.Segments[0], Sizeof(TSegment) * AInfo.SegmentCount);
    for I := 0 to AInfo.SegmentCount - 1 do
    begin
      AInfo.Segments[I].SegmentID := I;
      if (I < AInfo.SegmentCount - 1) or (ModCount = 0) then
      begin
        AInfo.Segments[I].SegmentSize := AInfo.SegmentSize;
        Result := CalcHash(I, AInfo.Segments[I].SegmentHash);
        if not Result then
          Exit;
      end
      else
      begin
        AInfo.Segments[I].SegmentSize := ModCount;
        Result := CalcHash(I, AInfo.Segments[I].SegmentHash);
        if not Result then
          Exit;
      end;
      if (PAbort <> nil) and PAbort^ then //是否停止计算
      begin
        Result := False;
        Exit;
      end;
      if (not AFastCalc) then
      begin
        if GetCurrentThreadID = MainThreadID then
          Application.ProcessMessages;
        Sleep(3);
      end;
      MemStream.WriteBuffer(AInfo.Segments[I].SegmentHash, SizeOf(TMD4Digest));
      if ASegmentHash <> nil then
        Move(AInfo.Segments[I].SegmentHash, ASegmentHash[I * Sizeof(TMD4Digest)], SizeOf(TMD4Digest));
    end;
    if AInfo.SegmentCount > 1 then
      Result := MD4Stream(MemStream, Digest, PAbort, AFastCalc)
    else if AInfo.SegmentCount = 1 then
    begin
      Digest := AInfo.Segments[0].SegmentHash;
      Result := True;
    end;
  finally
    Stream.Seek(SavePos, soFromBeginning);
    SetLength(AInfo.Segments, 0);
    MemStream.Free;
  end;
end;

end.

