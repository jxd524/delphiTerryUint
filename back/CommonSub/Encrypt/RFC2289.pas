{Copyright:      Hagen Reddmann  mailto:HaReddmann@AOL.COM
 Author:         Hagen Reddmann
 Remarks:        freeware, but this Copyright must be included
 known Problems: none
 Version:        3.0, Delphi Encryption Compendium
                 Delphi 2-4, BCB 3-4, designed and testet under D3 and D4
 Description:    RFC1938 Standard One Time Password Routines  "otp-"
                 RFC2289 Standard One Time Password Routines  "otp-"
                 RFC2444 Standard One Time Password Routines  "otp-" "ext"
                 RFC1760 S/Key One Time Password Routines     "s/key"
                 RFC1760 Six Word String Converting

 Remarks:        The RFC1760 Six Word Converting in these Library is an
                 extended (modified) Version. Normaly works the Standard
                 RFC1760 ONLY with 64 Bit (8 Bytes) Inputs, Six Words Output (66 Bit)
                 and a Dictionary with 2048 Entries.
                 The here implemented Version is absolutly compatible with
                 Standard RFC1760 but works with any Sizes and other
                 userdefined Dictionary's with variable Entrycount.

 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}
unit RFC2289;

interface

uses Windows, SysUtils, Classes, Hash, DECUtil;

{$I VER.INC}

const
  fmtRFC1760   = 1760;   // New StringFormat

type
  EOTPException  = class(Exception);

  TOneTimePassword = class(TComponent)
  private
    FSeed: String;
    FIdent: String;
    FHash: THashClass;
    FCount: Cardinal;
    FExtended: Boolean;
    FFormat: Integer;
    FLastOTP: String;
    function GetChallenge: String;
    procedure SetChallenge(Value: String);
    function GetAlgorithm: String;
    procedure SetAlgorithm(Value: String);
    procedure SetIdent(Value: String);
    procedure SetHash(Value: THashClass);
    procedure SetSeed(Value: String);
  protected
    function Calc(const Value: String; ACount: Integer): String;
  public
    constructor Create(AOwner: TComponent); override;

    function  Execute(const Password: String): String;
    function  Check(const OTP: String): Boolean;
    procedure Next(const OTP: String);

    function  NextPhrase(const OTP: String; ACount: Integer): String;
    function  FirstPhrase(const Password: String): String;

    property LastOTP: String read FLastOTP write FLastOTP;
    property Hash: THashClass read FHash write SetHash stored False;
  published
    property Algorithm: String read GetAlgorithm write SetAlgorithm stored False;
    property Ident: String read FIdent write SetIdent stored False;
    property Count: Cardinal read FCount write FCount stored False;
    property Seed: String read FSeed write SetSeed stored False;
    property Extended: Boolean read FExtended write FExtended stored False;
    property Format: Integer read FFormat write FFormat default fmtRFC1760;

    property Challenge: String read GetChallenge write SetChallenge;
  end;

  PDictionary = ^TDictionary;
  TDictionary = packed record
                  EntryCRC: LongWord;  // computed CRC
                  EntryCount: Integer; // count of Words in Entries
                  EntrySize: Integer;  // size of one Word in Entries
                  Entries: array[0..0] of Char; // the Dictionary
                end;

//  TStringFormat_RFC1760Class = class of TStringFormat_RFC1760;

  TStringFormat_RFC1760 = class(TStringFormat)
  private
    class function GetDict: PDictionary;
  protected
    class function Dictionary: PDictionary; dynamic;
  public
    class function ToStr(Value: PChar; Len: Integer): String; override;
    class function StrTo(Value: PChar; Len: Integer): String; override;
    class function Format: Integer; override;
    class function IsValid(Value: PChar; Len: Integer; ToStr: Boolean): Boolean; override;
  end;


// convert any OTP- HEX or WORD Format to ToFormat
function OTPFormat(const Value: String; ToFormat: Integer): String;

implementation

uses DECConst;

{$R RFC1760.RES}

const
  SepChars    = [#0,#9,#10,#13,#32,',',';',':','$','(',')','[',']'] +
                ['{','}','-','"','''','\','/','+','*'];
  RFC1760Dict : PDictionary = nil; // Standard RFC1760 Dictionary Resource

function OTPFormat(const Value: String; ToFormat: Integer): String;
var
  I: Integer;
begin
  Result := Value;
  if Result = '' then Exit;
  I := Pos(sOTPWord, AnsiLowerCase(Result));
  if I > 0 then
  begin
    Delete(Result, 1, I + Length(sOTPWord) -1);
    I := fmtRFC1760;
  end else
  begin
    I := Pos(sOTPHex, AnsiLowerCase(Result));
    if I > 0 then
    begin
      Delete(Result, 1, I + Length(sOTPHex) -1);
      I := fmtHEX;
    end else I := fmtRFC1760;
  end;
  try
    Result := FormatToStr(PChar(Result), -1, I);
  except
    if I <> fmtRFC1760 then raise
      else Result := FormatToStr(PChar(Result), -1, fmtHEX)
  end;
  Result := StrToFormat(PChar(Result), Length(Result), ToFormat);
end;

function TOneTimePassword.GetChallenge: String;
begin
  if FHash = nil then FHash := THash_MD4;
  if Trim(FSeed) = '' then
  begin
    SetLength(FSeed, 2);
    RndXORBuffer(RndTimeSeed, PChar(FSeed)^, 2);
    FSeed := StrToFormat(PChar(FSeed), 2, fmtHEX);
  end;
  Result := FIdent;
  if AnsiCompareText(FIdent, sSKeyIdent) <> 0 then
    Result := Result + GetShortClassName(FHash);
  Result := Result + ' ' + IntToStr(FCount) + ' ' + FSeed;
  if FExtended then Result := Result + ' ' + sOTPExt;
end;

procedure TOneTimePassword.SetChallenge(Value: String);
type
  TCharSet = set of Char;

  function CheckAlpha(const Value: String; Chars: TCharSet): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to Length(Value) do
      if not (Value[I] in Chars) then Exit;
    Result := True;
  end;

var
  I: Integer;
  P,T,L: PChar;
  S: String;
  H: THashClass;
  C: Integer;
  E: Boolean;
begin
  if Trim(Value) = '' then
  begin
    FHash := THash_MD4;
    FIdent := sOTPIdent;
    FExtended := False;
    FSeed := '';
    FCount := 0;
    Exit;
  end;
  H  := nil;
  C  := -1;
  E  := False;
  try
    P := PChar(Value);
    L := StrEnd(P);
    I := Pos(sOTPIdent, AnsiLowerCase(Value)); // check of "otp-"
    if I > 0 then                                 // it's a RFC2289 Challenge
    begin
      Inc(P, I + Length(sOTPIdent) - 1);
      T := P;
      if T >= L then Exit;
      while not (T^ in SepChars) do Inc(T);       // find End
      if T >= L then Exit;
      S := P;
      SetLength(S, T - P);                        // Algo Ident currect used "md4", "md5", "sha1"
      H := GetHashClass(S);
    end else
    begin
      I := Pos(sSKeyIdent, AnsiLowerCase(Value));  // check of "s/key"
      if I = 0 then Exit;                         // isn't a RFC1760 Challenge
      Inc(P, I + Length(sSKeyIdent) - 1);
      T := P;
      if T >= L then Exit;
      H := THash_MD4;
    end;
    while T^ in SepChars do Inc(T);        // find next Begin
    if T >= L then Exit;
    P := T;
    while not (T^ in SepChars) do Inc(T);  // find End
    if T >= L then Exit;
    S := P;
    SetLength(S, T - P);                   // extract Count Value
    C := StrToIntDef(S, -1);           // convert to Integer
    while T^ in SepChars do Inc(T);        // find next Begin
    if T > L then Exit;
    P := T;
    while not (T^ in SepChars) do Inc(T);  // find End
    if T > L then Exit;
    S := P;
    SetLength(S, T - P);                    // extract Seed Value
    while T^ in SepChars do Inc(T);         // find next Begin
    if T > L then Exit;
    E := StrLIComp(T, PChar(sOTPExt), Length(sOTPExt)) = 0;
  finally
    if (H = nil) or (C < 0) then
      raise EOTPException.Create(sInvalidChallenge);
    if (S = '') or not CheckAlpha(S, ['a'..'z', 'A'..'Z', '0'..'9']) then
      raise EOTPException.Create(sInvalidSeed);
    FHash  := H;
    FSeed  := S;
    FCount := C;
    FExtended := E;
  end;
end;

function TOneTimePassword.GetAlgorithm: String;
begin
  Result := GetHashName(FHash);
end;

procedure TOneTimePassword.SetAlgorithm(Value: String);
begin
  SetHash(GetHashClass(Value));
end;

procedure TOneTimePassword.SetIdent(Value: String);
begin
  FIdent := Value;
  if AnsiCompareText(FIdent, sSKeyIdent) = 0 then FHash := THash_MD4;
end;

procedure TOneTimePassword.SetHash(Value: THashClass);
begin
  FHash := Value;
  if (FHash = nil) or
    (FHash.DigestKeySize < 16) or
     FHash.InheritsFrom(TChecksum) then FHash := THash_MD4 else
  if (AnsiCompareText(FIdent, sSKeyIdent) = 0) and (FHash <> THash_MD4) then
    FIdent := sOTPIdent;
end;

procedure TOneTimePassword.SetSeed(Value: String);
var
  I: Integer;
begin
  FSeed := Value;
  for I := Length(FSeed) downto 1 do
    if not (FSeed[I] in ['a'..'z', 'A'..'Z', '0'..'9']) then Delete(FSeed, I, 1);
end;

constructor TOneTimePassword.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetChallenge('');
  FFormat := fmtRFC1760;
end;

function TOneTimePassword.Calc(const Value: String; ACount: Integer): String;
var
  I: Integer;
begin
  if (FHash = nil) or (ACount < 0) then
    raise EOTPException.Create(sInvalidCalc);
  Result := Value;
  repeat
    Result := FHash.CalcBuffer(PChar(Result)^, Length(Result), nil, fmtCOPY);
// Fold the Digest to 8 Bytes
    I := 8;
    repeat
      XORBuffers(PChar(Result), @PChar(Result)[I], 8, PChar(Result));
      Inc(I, 8);
    until I >= FHash.DigestKeySize;
// convert Endianes
    if FHash.InheritsFrom(THash_SHA) then
      SwapIntegerBuffer(PChar(Result), PChar(Result), 2);
// truncate the Result
    SetLength(Result, 8);
    Dec(ACount);
  until ACount < 0;
end;

function TOneTimePassword.Execute(const Password: String): String;
begin
  Result := Calc(AnsiLowerCase(FSeed) + Password, Count);
  Result := StrToFormat(PChar(Result), 8, Format);
  if FExtended then
    with StringFormat(Format) do
      if InheritsFrom(TStringFormat_RFC1760) then Result := sOTPWord + Result else
        if InheritsFrom(TStringFormat_HEX) then Result := sOTPHex + Result;
end;

procedure TOneTimePassword.Next(const OTP: String);
begin
  FLastOTP := OTP;
  Dec(FCount);
end;

function TOneTimePassword.Check(const OTP: String): Boolean;
begin
  Result := Calc(OTPFormat(OTP, fmtCOPY), 0) = OTPFormat(FLastOTP, fmtCOPY);
end;

function TOneTimePassword.FirstPhrase(const Password: String): String;
begin
  Result := Calc(AnsiLowerCase(FSeed) + Password, 0);
  Result := StrToFormat(PChar(Result), 8, Format);
end;

function TOneTimePassword.NextPhrase(const OTP: String; ACount: Integer): String;
begin
  Result := Calc(OTPFormat(OTP, fmtCOPY), ACount);
  Result := StrToFormat(PChar(Result), 8, Format);
end;

// RFC1760 Six Word String Format
class function TStringFormat_RFC1760.GetDict: PDictionary;
begin
  Result := Dictionary;
  if (Result = nil) or
     (Result.EntryCount <= 0) or
     (Result.EntrySize <= 0) then
    raise EStringFormat.CreateFMT(sInvalidDictionary, [GetShortClassName(Self)]);
end;

class function TStringFormat_RFC1760.Dictionary: PDictionary;
{ Standard RFC1760 Dictionary Format

  TDictionary = packed record
                  EntryCount = 2048;
                  EntrySize  = 4;
                  EntryCRC   = $94CE8163; a standard CRC32 Checksum
                  Entries    = array[0..2047] of array[0..3] of Char; see RFC1760.INC
                end;
}
begin
  if RFC1760Dict = nil then // Load Dictionary from resource
  begin
    RFC1760Dict := LockResource(LoadResource(HInstance, FindResource(HInstance,
                                  PChar(GetShortClassName(Self)), RT_RCDATA)));
    if RFC1760Dict <> nil then
      with RFC1760Dict^ do
        if EntryCRC <> not CRC32($FFFFFFFF, @Entries, EntryCount * EntrySize) then
        begin // Resource is modified, any Hacker present ?! :-)
          FreeResource(Integer(RFC1760Dict));
          RFC1760Dict := nil;
        end;
  end;
  Result := RFC1760Dict;
end;

function ExtractBits(Value: PChar; BitOffset, Bits, MaxLen: Integer): Integer;
var // Extract from BitOffset count Bits
  I: Integer;
begin
  Result := 0;
  for I := BitOffset div 8 -1 to BitOffset div 8 + 2 do
  begin
    Result := Result shl 8;
    if (I >= 0) and (I * 8 <= MaxLen) then
      Result := Result or PByteArray(Value)[I];
  end;
  Result := (Result shr (24 - Bits - BitOffset mod 8)) and (1 shl Bits -1);
  if BitOffset + Bits > MaxLen then
    Result := Result and not (1 shl (BitOffset + Bits - MaxLen) - 1);
end;

class function TStringFormat_RFC1760.ToStr(Value: PChar; Len: Integer): String;

  function FindEntry(Dictionay: PDictionary; Entry: PChar): Integer; register;
  asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        PUSH    EBP

        MOV     ESI,EDX
        MOV     DL,[ESI]
        MOV     EBX,[EAX].TDictionary.EntrySize
        MOV     EBP,EBX
        MOV     ECX,[EAX].TDictionary.EntryCount
        LEA     EDI,[EAX].TDictionary.Entries
        XOR     EAX,EAX
        JMP     @@2

@@1:    ADD     EDI,EBP
@@2:    CMP     DL,[EDI]
        JZ      @@4
@@3:    INC     EAX
        DEC     ECX
        JNZ     @@1
        MOV     EAX,-1
        JMP     @@6
@@4:    MOV     EBX,EBP
@@5:    DEC     EBX
        JZ      @@6
        MOV     DH,[ESI + EBX]
        CMP     DH,[EDI + EBX]
        JNZ     @@3
        JMP     @@5

@@6:    POP     EBP
        POP     EBX
        POP     ESI
        POP     EDI
  end;

var
  Last,T,R: PChar;
  Entry: String;
  Dict: PDictionary;
  Index: Integer;
  Bits: Integer;
  Offs: Integer;
  Parity: Integer;
begin
  Result := '';
  Dict := Dictionary;
  if Dict = nil then Exit;
  Bits := MSBit(Dict.EntryCount);
  Last := Value + Len;
  SetLength(Entry, Dict.EntrySize);
  SetLength(Result, Len * Dict.EntrySize);  // allocate enough Space
  FillChar(PChar(Result)^, Len * Dict.EntrySize, 0);
  R := PChar(Result);
  Offs := 0;
  repeat
// Extract one Word
    while Value^ in SepChars do Inc(Value);
    if Value >= Last then
      raise EStringFormat.CreateFmt(sInvalidStringFormat, [GetShortClassName(Self)]);
    T := Value;
    while not (Value^ in SepChars) do Inc(Value);
    if Value - T > Dict.EntrySize then
      raise EStringFormat.CreateFmt(sInvalidStringFormat, [GetShortClassName(Self)]);
// Move and Uppercase Word to Entrybuffer
    FillChar(PChar(Entry)^, Dict.EntrySize, 0);
    Move(T^, PChar(Entry)^, Value - T);
    CharUpperBuff(PChar(Entry), Dict.EntrySize);
// Lookup Entry in Dictionary
    Index := FindEntry(Dict, PChar(Entry));
    if Index < 0 then
      raise EStringFormat.CreateFmt(sInvalidStringFormat, [GetShortClassName(Self)]);
// put the Result
    asm
         MOV   EAX,R           // R
         MOV   EDX,Offs        // Offs
         SHR   EDX,3           // Offs div 8
         ADD   EAX,EDX         // @R[Offs div 8]
         MOV   ECX,32          // 32
         SUB   ECX,Bits        // 32 - Bits
         MOV   EDX,Offs        // Offs
         AND   EDX,07h         // Offs mod 8
         SUB   ECX,EDX         // 32 - Bits - Offs mod 8
         MOV   EDX,Index       // Index
         SHL   EDX,CL          // Index shl (32 - Bits - Offs mod 8)
         OR    [EAX + 3],DL
         OR    [EAX + 2],DH
         SHR   EDX,16
         OR    [EAX + 1],DL
         OR    [EAX + 0],DH
    end;
    Inc(Offs, Bits);
  until Value >= Last;
  Entry := '';

// calculate Parity
  Index := (Offs div 8) and not 1;
  Bits  := Offs - Index * 8;
  if Bits = 0 then Bits := MSBit(Dict.EntryCount);
  Dec(Offs, Bits);
  Len   := ExtractBits(R, Offs, Bits, Offs + Bits);
  Index := 0;
  Parity := 0;
  while Index < Offs do
  begin
    Inc(Parity, ExtractBits(R, Index, Bits, Offs));
    Inc(Index, Bits);
  end;
  SetLength(Result, Offs div 8);
  Index := Length(Result);
  if (Result[Index] = #0) and (Index <> 8) then
    SetLength(Result, Index-1);
  Parity := Parity and (1 shl Bits -1);
  if Len <> Parity then
    raise EStringFormat.CreateFmt(sInvalidStringFormat, [GetShortClassName(Self)]);
end;

class function TStringFormat_RFC1760.StrTo(Value: PChar; Len: Integer): String;
var
  Dict: PDictionary;
  Bits: Integer;
  BitLen: Integer;
  Parity: Integer;
  Offset: Integer;
  Entry: Integer;
  Text: String;
  Temp: String;
begin
  Result := '';
  Dict   := GetDict;
  SetLength(Text, Dict.EntrySize);
  SetLength(Temp, Len +2);
  FillChar(PChar(Temp)^, Len +2, 0);
  Move(Value^, PChar(Temp)^, Len);
  Value := PChar(Temp);
// calculate the Parity (Checksum )
  BitLen := ((Len +1) and not 1) * 8;   // Input Length in Bits
  Offset := 0;
  Parity := 0;
  Bits   := MSBit(Dict.EntryCount);     // Standard = 2048 -> 11 Bits
  Bits   := Bits - BitLen mod Bits;     // Standard = 8 Byte Input -> 64 Bits -> 2 Bits Parity -> effective 6 * 11 Bits Output
  while Offset < BitLen do
  begin
    Inc(Parity, ExtractBits(Value, Offset, Bits, BitLen));
    Inc(Offset, Bits);
  end;
// calculate the Result
  Offset := 0;
  Parity := Parity and (1 shl Bits -1);  // Standard = 2 Bits used
  Bits   := MSBit(Dict.EntryCount);      // Standard = 11 Bits
  while Offset <= BitLen do
  begin
    Entry := ExtractBits(Value, Offset, Bits, BitLen);
    Inc(Offset, Bits);
    if Offset > BitLen then Entry := Entry or Parity;
    Move(Dict.Entries[Entry * Dict.EntrySize], PChar(Text)^, Dict.EntrySize);
    Result := Result + PChar(Text) + ' ';
  end;
  SetLength(Result, Length(Result) -1);
end;

class function TStringFormat_RFC1760.Format: Integer;
begin
  Result := fmtRFC1760;
end;

class function TStringFormat_RFC1760.IsValid(Value: PChar; Len: Integer; ToStr: Boolean): Boolean;
begin
  Result := False;
  try
    if Dictionary = nil then Exit;
    if ToStr then Self.ToStr(Value, Len);
    Result := True;
  except
  end;
end;
{
procedure SaveDictionary(Format: TStringFormat_RFC1760Class);
//save a Dictionary as Resource File
var
  Dict: PDictionary;
  HeaderSize: Integer;
  Origin, ImageSize: Longint;
  Header: array[0..79] of Char;
begin
  Dict := Format.GetDict;
  with TFileStream.Create(GetShortClassName(Format) + '.RES', fmCreate) do
  try
    Byte((@Header[0])^) := $FF;
    Word((@Header[1])^) := 10;
    HeaderSize := StrLen(StrUpper(StrPLCopy(@Header[3], GetShortClassName(Format), 63))) + 10;
    Word((@Header[HeaderSize - 6])^) := $1030;
    Longint((@Header[HeaderSize - 4])^) := 0;
    WriteBuffer(Header, HeaderSize);
    Origin := Position;
    Dict.EntryCRC := not CRC32(-1, @Dict.Entries, Dict.EntryCount * Dict.EntrySize);
    WriteBuffer(Dict^, SizeOf(TDictionary) + Dict.EntryCount * Dict.EntrySize);
    ImageSize := Position - Origin;
    Position := Origin - 4;
    WriteBuffer(ImageSize, SizeOf(Longint));
  finally
    Free;
  end;
end;
}
initialization
//  SaveDictionary(TStringFormat_RFC1760);
  RegisterStringFormats([TStringFormat_RFC1760]);
finalization
  if RFC1760Dict <> nil then FreeResource(Integer(RFC1760Dict));
end.
