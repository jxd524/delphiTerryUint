unit JxdEncrypt;

interface

uses Windows,SysUtils;
  //数据加密
procedure OldEncode(Source: PChar; Len: Integer);overload; //返回CheckSum
function OldDecode(Source: PChar; Len: Integer): Boolean;overload;

  //数据加密
function OldEncode(source: string): string;overload;
  //数据解密
function OldDecode(source: string): string;overload;

//数据加密
function OldEncode1(source:String):String;
//数据解密
function OldDecode1(source:String):String;

procedure OldEncode2(Source: PChar; Len: Integer);
function OldDecode2(Source: PChar; Len: Integer): Boolean;

function decode(source: string): string;
function encode(source: string): string;


function NewEncode(ABuffer: PChar; ALen: Integer): WORD;
function NewDecode(ABuffer: PChar; ALen: Integer): WORD;

function Encode3(ABuffer: PChar; ALen: Integer): DWORD;
function Decode3(ABuffer: PChar; ALen: Integer; ACRC32: DWORD): Boolean;

function CalcCRC32(ABuffer: PChar; ALen: Integer): DWORD;

implementation

Uses HCMngr;

const

  CipherKey = 'po*ijadfa$%%^';
  CipherKeyLen = Length(CipherKey);

  CRC16Tab: array[0..255] of Word = (
    $0000, $1189, $2312, $329B, $4624, $57AD, $6536, $74BF,
    $8C48, $9DC1, $AF5A, $BED3, $CA6C, $DBE5, $E97E, $F8F7,
    $1081, $0108, $3393, $221A, $56A5, $472C, $75B7, $643E,
    $9CC9, $8D40, $BFDB, $AE52, $DAED, $CB64, $F9FF, $E876,
    $2102, $308B, $0210, $1399, $6726, $76AF, $4434, $55BD,
    $AD4A, $BCC3, $8E58, $9FD1, $EB6E, $FAE7, $C87C, $D9F5,
    $3183, $200A, $1291, $0318, $77A7, $662E, $54B5, $453C,
    $BDCB, $AC42, $9ED9, $8F50, $FBEF, $EA66, $D8FD, $C974,
    $4204, $538D, $6116, $709F, $0420, $15A9, $2732, $36BB,
    $CE4C, $DFC5, $ED5E, $FCD7, $8868, $99E1, $AB7A, $BAF3,
    $5285, $430C, $7197, $601E, $14A1, $0528, $37B3, $263A,
    $DECD, $CF44, $FDDF, $EC56, $98E9, $8960, $BBFB, $AA72,
    $6306, $728F, $4014, $519D, $2522, $34AB, $0630, $17B9,
    $EF4E, $FEC7, $CC5C, $DDD5, $A96A, $B8E3, $8A78, $9BF1,
    $7387, $620E, $5095, $411C, $35A3, $242A, $16B1, $0738,
    $FFCF, $EE46, $DCDD, $CD54, $B9EB, $A862, $9AF9, $8B70,
    $8408, $9581, $A71A, $B693, $C22C, $D3A5, $E13E, $F0B7,
    $0840, $19C9, $2B52, $3ADB, $4E64, $5FED, $6D76, $7CFF,
    $9489, $8500, $B79B, $A612, $D2AD, $C324, $F1BF, $E036,
    $18C1, $0948, $3BD3, $2A5A, $5EE5, $4F6C, $7DF7, $6C7E,
    $A50A, $B483, $8618, $9791, $E32E, $F2A7, $C03C, $D1B5,
    $2942, $38CB, $0A50, $1BD9, $6F66, $7EEF, $4C74, $5DFD,
    $B58B, $A402, $9699, $8710, $F3AF, $E226, $D0BD, $C134,
    $39C3, $284A, $1AD1, $0B58, $7FE7, $6E6E, $5CF5, $4D7C,
    $C60C, $D785, $E51E, $F497, $8028, $91A1, $A33A, $B2B3,
    $4A44, $5BCD, $6956, $78DF, $0C60, $1DE9, $2F72, $3EFB,
    $D68D, $C704, $F59F, $E416, $90A9, $8120, $B3BB, $A232,
    $5AC5, $4B4C, $79D7, $685E, $1CE1, $0D68, $3FF3, $2E7A,
    $E70E, $F687, $C41C, $D595, $A12A, $B0A3, $8238, $93B1,
    $6B46, $7ACF, $4854, $59DD, $2D62, $3CEB, $0E70, $1FF9,
    $F78F, $E606, $D49D, $C514, $B1AB, $A022, $92B9, $8330,
    $7BC7, $6A4E, $58D5, $495C, $3DE3, $2C6A, $1EF1, $0F78
    );

  CRC32Table: array[0..255] of DWORD = (
    $00000000, $77073096, $EE0E612C, $990951BA,
    $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
    $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
    $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
    $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116,
    $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

    $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
    $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
    $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
    $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
    $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086,
    $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
    $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
    $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
    $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
    $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
    $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
    $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
    $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

    $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
    $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
    $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
    $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
    $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
    $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

//数据加密

function NewEncode(ABuffer: PChar; ALen: Integer): WORD;
var
  I: Integer;
  Ch: Byte;
  function UpdateCRC16(AValue: Byte; ACRC16: Word): Word;
  begin
    Result := ((ACRC16 shr 8) and $00FF) xor
      CRC16Tab[Byte(ACRC16 xor (Word(AValue)) and $00FF)];
  end;
begin
  Result := $FFFF;
  try
    for I := 0 to ALen - 1 do
    begin
      Ch := PByte(Integer(ABuffer) + I)^;
      Ch := Ch xor Ord(Cipherkey[(i mod CipherKeyLen) + 1]);
      PByte(Integer(ABuffer) + I)^ := Ch;
      Result := UpdateCRC16(Ch, Result);
    end;
  except
  end;
end;

function NewDecode(ABuffer: PChar; ALen: Integer): WORD;
var
  I: Integer;
  Ch: Byte;
  function UpdateCRC16(AValue: Byte; ACRC16: Word): Word;
  begin
    Result := ((ACRC16 shr 8) and $00FF) xor
      CRC16Tab[Byte(ACRC16 xor (Word(AValue)) and $00FF)];
  end;
begin
  Result := $FFFF;
  try
    for I := 0 to ALen - 1 do
    begin
      Ch := PByte(Integer(ABuffer) + I)^;
      Result := UpdateCRC16(Ch, Result);
      Ch := Ch xor Ord(Cipherkey[(i mod CipherKeyLen) + 1]);
      PByte(Integer(ABuffer) + I)^ := Ch;
    end;
  except
  end;
end;

function CalcCRC32(ABuffer: PChar; ALen: Integer): DWORD;
var
  I: Integer;
  P: PByte;
begin
  Result := $FFFFFFFF;
  P := PByte(ABuffer);
  for I := 0 to ALen - 1 do
  begin
    Result := (Result shr 8) xor CRC32Table[P^ xor (Result and $000000FF)];
    Inc(P);
  end;
end;

function Encode3(ABuffer: PChar; ALen: Integer): DWORD;
var
  I: Integer;
  K: DWORD;
  P: PByte;
  Keys: array of Byte;
begin
  Keys := @CRC32Table;
  Result := CalcCRC32(ABuffer, ALen);
  K := Result;
  P := PByte(ABuffer);
  for I := 0 to ALen - 1 do
  begin
    P^ := P^ xor Keys[(K mod 1024) + 1];
    Inc(P);
    Inc(K);
  end;
end;

function Decode3(ABuffer: PChar; ALen: Integer; ACRC32: DWORD): Boolean;
var
  I: Integer;
  K: DWORD;
  P: PByte;
  CRC32: DWORD;
  Keys: array of Byte;
begin
  K := ACRC32;
  P := PByte(ABuffer);
  Keys := @CRC32Table;
  for I := 0 to ALen - 1 do
  begin
    P^ := P^ xor Keys[(K mod 1024) + 1];
    Inc(P);
    Inc(K);
  end;
  CRC32 := CalcCRC32(ABuffer, ALen);
  Result := CRC32 = ACRC32;
end;

procedure OldEncode(Source: PChar; Len: Integer);
var
  aSource:String;
  aResult:String;
begin
  SetString(aSource,Source,Len);
  aResult := OldEncode1(aSource);
  StrPCopy(Source,aResult);
  //Source := PChar(aResult);
  aSource :='';
  aResult :='';
end; 

function OldDecode(Source: PChar; Len: Integer): Boolean;
var
  aSource:String;
  aResult:String;
begin
  SetString(aSource,Source,Len);
  aResult := OldDecode1(aSource);
  StrPCopy(Source,aResult);
  //Source := PChar(aResult);
  aSource :='';
  aResult :='';
  Result := True;
end;

function OldEncode(source: string): string;
begin
  Result := OldEncode1(source);
end;

function OldDecode(source: string): string;
begin
  Result := OldDecode1(source);
end;

function OldEncode1(source:String):String;
var
  THash:THashManager;
  TCipher:TCipherManager;
  hash:String;
  cipher:String;
begin
  THash:=THashManager.Create(nil);
  TCipher:=TCipherManager.Create(nil);
  TCipher.InitKey(cipherkey,nil);
  hash:=THash.Hash.CalcString(source);
  cipher:=TCipher.Cipher.EncodeString(hash+source);
  Result:=cipher;
  THash.Free;
  TCipher.Free;
end;

function OldDecode1(source:String):String;
var
  THash:THashManager;
  TCipher:TCipherManager;
  hash:String;
  cipher:String;
begin
  THash:=THashManager.Create(nil);
  TCipher:=TCipherManager.Create(nil);
  TCipher.InitKey(cipherkey,nil);
  cipher:=TCipher.Cipher.DecodeString(source);
  hash:=Copy(cipher,1,44);
  cipher:=Copy(cipher,45,Length(cipher));
  if THash.hash.CalcString(cipher)=hash then
    Result:=cipher
  else
    Result:='';
  THash.Free;
  TCipher.Free;
end;

procedure OldEncode2(Source: PChar; Len: Integer);
var
  I: Integer;
  Ch: Byte;
  CheckSum: Byte;
begin
  Checksum := 100;
  for I := 0 to Len - 1 do
  begin
    Ch := Ord(source[I]);
    CheckSum := CheckSum xor Ch;
    Ch := Ch xor Ord(Cipherkey[(I + 1) mod CipherKeyLen + 1]);
    Source[i] := Chr(ch);
  end;
  Source[Len] := Chr(CheckSum);
  Source[Len + 1] := '9';
end;

function OldDecode2(Source: PChar; Len: Integer): Boolean;
var
  I: Integer;
  Ch: Byte;
  CheckSum: Byte;
begin
  Result := False;
  if (Len < 2) or (Source[Len - 1] <> '9') then
    Exit;
  Checksum := 100;
  for I := 0 to Len - 3 do
  begin
    Ch := Ord(source[i]);
    Ch := Ch xor Ord(Cipherkey[(I + 1) mod CipherKeyLen + 1]);
    checksum := checksum xor Ch;
    Source[i] := Chr(ch);
  end;
  Result := Chr(CheckSum) = Source[Len - 2];
end;

function encode(source: string): string;
var
  keylen: Integer;
  sourcelen: Integer;
  i: Integer;
  ch: Byte;
  checksum: Byte;
begin
  keylen := Length(CipherKey);
  sourcelen := Length(source);
  checksum := 100;
  for i := 1 to sourcelen do
  begin
    ch := Ord(source[i]);
    checksum := checksum xor ch;
    ch := ch xor Ord(Cipherkey[i mod keylen + 1]);
    source[i] := Chr(ch);
  end;
  Result := source + Chr(checksum) + '9';
end;

//数据解密

function decode(source: string): string;
var
  keylen: Integer;
  sourcelen: Integer;
  i: Integer;
  ch: Byte;
  checksum: Byte;
begin
  keylen := Length(CipherKey);
  sourcelen := Length(source);
  if (sourcelen < 2) then
  begin
    Result := '';
    Exit;
  end;
  if not (source[sourcelen] = '9') then
  begin
    Result := '';
    Exit;
  end;
  checksum := 100;
  for i := 1 to sourcelen - 2 do
  begin
    ch := Ord(source[i]);
    ch := ch xor Ord(Cipherkey[i mod keylen + 1]);
    checksum := checksum xor ch;
    source[i] := Chr(ch);
  end;
  if not (Chr(checksum) = source[sourcelen - 1]) then
  begin
    Result := '';
    Exit;
  end;
  Result := Copy(source, 1, sourcelen - 2);
end;

end.

