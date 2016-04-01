{Copyright:      Hagen Reddmann  mailto:HaReddmann@AOL.COM
 Author:         Hagen Reddmann
 Remarks:        freeware, but this Copyright must be included
 known Problems: none
 Version:        3.0,  Part I from Delphi Encryption Compendium  ( DEC Part I)
                 Delphi 2-4, designed and testet under D3 & D4
 Description:    Include a Selection of various Cipher's (Encryption Algo)
                 impl. Algo:
                   Cast128, Cast256, Mars, Misty 1, RC2, RC4, RC5, RC6,
                   FROG, Rijndael, Skipjack, Single DES, Double DES,
                   Triple DES, Double DES 16byte Plain, Triple DES 16,
                   Triple DES 24, DESX, NewDES, Diamond II,
                   Diamond II Lite, Sapphire II

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
unit Cipher1;

interface

{$I VER.INC}

uses SysUtils, DECUtil, Cipher, Hash;

type
  TCipher_Cast128      = class;
  TCipher_Cast256      = class;
  TCipher_Mars         = class;
  TCipher_Misty        = class;
  TCipher_RC2          = class;
  TCipher_RC4          = class;  {Streamcipher}
  TCipher_RC5          = class;
  TCipher_RC6          = class;
  TCipher_FROG         = class;
  TCipher_Rijndael     = class;
{$IFDEF VER_D3H}
  TCipher_Skipjack     = class;
{$ENDIF}
  TCipher_1DES         = class;  {Single DES  8 byte Blocksize,  8 byte Keysize  56 bits relevant}
  TCipher_2DES         = class;  {Double DES  8 byte Blocksize, 16 byte Keysize 112 bits relevant}
  TCipher_3DES         = class;  {Triple DES  8 byte Blocksize, 24 byte Keysize 168 bits relevant}
  TCipher_2DDES        = class;  {Double DES 16 byte Blocksize, 16 byte Keysize 112 bits relevant}
  TCipher_3DDES        = class;  {Triple DES 16 byte Blocksize, 24 byte Keysize 168 bits relevant}
  TCipher_3TDES        = class;  {Triple DES 24 byte Blocksize, 24 byte Keysize 168 bits relevant}
  TCipher_DESX         = class;  {Single DES  8 byte Blocksize, by RSA added XOR Keying}
  TCipher_NewDES       = class;
  TCipher_Diamond2     = class;  {very slow, but demonstrate a cipher without}
  TCipher_Diamond2Lite = class;  {static- and keydepends S-Boxes}
  TCipher_Sapphire     = class;  {Stream Cipher, eq. Design from german ENIGMA Machine}

  TCipher_Cast128 = class(TCipher) {Carlisle Adams and Stafford Tavares }
  private
    FRounds: Byte;
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

  TCipher_Cast256 = class(TCipher)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

  TCipher_Mars = class(TCipher)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

  TCipher_Misty = class(TCipher)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

  TCipher_RC2 = class(TCipher)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

  TCipher_RC4 = class(TCipher)
  private
    FI: Byte;
    FJ: Byte;
    FSI: Byte;
    FSJ: Byte;
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
    procedure Done; override;
  end;

  TCipher_RC5 = class(TCipher)
  private
    FRounds: Integer; {8-16, default 12}
    procedure SetRounds(Value: Integer);
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
    property Rounds: Integer read FRounds write SetRounds;
  end;

  TCipher_RC6 = class(TCipher)
  private
    FRounds: Integer; {16-24, default 20}
    procedure SetRounds(Value: Integer);
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
    property Rounds: Integer read FRounds write SetRounds;
  end;

  TCipher_FROG = class(TCipher)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

  TCipher_Rijndael = class(TCipher)
  private
    FRounds: Integer;
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;
{$IFDEF VER_D3H}
  TCipher_Skipjack = class(TCipher)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;
{$ENDIF}
  TCipher_1DES = class(TCipher)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
    procedure MakeKey(const Data: array of Byte; Key: PInteger; Reverse: Boolean);
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

  TCipher_2DES = class(TCipher_1DES)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

  TCipher_3DES = class(TCipher_1DES)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

  TCipher_2DDES = class(TCipher_2DES)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  end;

  TCipher_3DDES = class(TCipher_3DES)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  end;

  TCipher_3TDES = class(TCipher_3DES)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  end;

  TCipher_DESX = class(TCipher_1DES)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
    procedure InitNew(const Key, Whitening; Size: Integer; IVector: Pointer);
  end;

{ This algorithm resembles the Data Encryption Standard (DES), but is easier
  to implement in software and is supposed to be more secure.
  It is not to be confused with another algorithm--known by the
  same name--which is simply DES without the initial and final
  permutations.  The NewDES here is a completely different algorithm.}

  TCipher_NewDES = class(TCipher)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
  end;

  TCipher_Diamond2 = class(TCipher)
  private
    FRounds: Integer;
    FBoxE: PByteArray; {dynamicly allociated Boxes}
    FBoxD: PByteArray;
    procedure SetRounds(Value: Integer); virtual; {5-15, default is 10}
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
    procedure Protect; override;
    property Rounds: Integer read FRounds write SetRounds;
  end;

  TCipher_Diamond2Lite = class(TCipher_Diamond2)
  private
    procedure SetRounds(Value: Integer); override; {4-30, default is 10}
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
  end;

  TCipher_Sapphire = class(TCipher)
  protected
    class procedure GetContext(var ABufSize, AKeySize, AUserSize: Integer); override;
    class function TestVector: Pointer; override;
    procedure Encode(Data: Pointer); override;
    procedure Decode(Data: Pointer); override;
  public
    procedure Init(const Key; Size: Integer; IVector: Pointer); override;
    procedure Done; override;
  end;

implementation

{$I *.inc}

type
  PCipherRec = ^TCipherRec;
  TCipherRec = packed record
                  case Integer of
                    0: (X: array[0..7] of Byte);
                    1: (A, B: LongWord);
                end;

class procedure TCipher_Cast128.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 16;
  AUserSize := 128;
end;

class function TCipher_Cast128.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    06Ch,027h,0D1h,04Ch,0F6h,0BAh,076h,0E7h
         DB    0A4h,078h,01Ch,020h,018h,08Ch,030h,0BCh
         DB    0D2h,09Ah,0F6h,02Ah,063h,01Fh,0FDh,004h
         DB    089h,03Fh,0C7h,00Eh,007h,0A9h,094h,09Bh
end;

procedure TCipher_Cast128.Encode(Data: Pointer);
var
  T,I,A,B: LongWord;
  K: PInteger;
begin
  K := User;
  A := SwapInteger(PCipherRec(Data).A);
  B := SwapInteger(PCipherRec(Data).B);
  for I := 0 to 2 do
  begin
    T := ROL(K^ + B, PIntArray(K)[16]);
    A := A xor (Cast128_Data[0, T shr 24] xor
                Cast128_Data[1, T shr 16 and $FF] -
                Cast128_Data[2, T shr  8 and $FF] +
                Cast128_Data[3, T and $FF]);
    Inc(K);
    T := ROL(K^ xor A, PIntArray(K)[16]);
    B := B xor (Cast128_Data[0, T shr 24] -
                Cast128_Data[1, T shr 16 and $FF] +
                Cast128_Data[2, T shr  8 and $FF] xor
                Cast128_Data[3, T and $FF]);
    Inc(K);
    T := ROL(K^ - B, PIntArray(K)[16]);
    A := A xor (Cast128_Data[0, T shr 24] +
                Cast128_Data[1, T shr 16 and $FF] xor
                Cast128_Data[2, T shr  8 and $FF] -
                Cast128_Data[3, T and $FF]);
    Inc(K);
    T := ROL(K^ + A, PIntArray(K)[16]);
    B := B xor (Cast128_Data[0, T shr 24] xor
                Cast128_Data[1, T shr 16 and $FF] -
                Cast128_Data[2, T shr  8 and $FF] +
                Cast128_Data[3, T and $FF]);
    if I = 2 then Break;
    Inc(K);
    T := ROL(K^ xor B, PIntArray(K)[16]);
    A := A xor (Cast128_Data[0, T shr 24] -
                Cast128_Data[1, T shr 16 and $FF] +
                Cast128_Data[2, T shr  8 and $FF] xor
                Cast128_Data[3, T and $FF]);
    Inc(K);
    T := ROL(K^ - A, PIntArray(K)[16]);
    B := B xor (Cast128_Data[0, T shr 24] +
                Cast128_Data[1, T shr 16 and $FF] xor
                Cast128_Data[2, T shr  8 and $FF] -
                Cast128_Data[3, T and $FF]);
    Inc(K);
    if (I = 1) and (FRounds <= 12) then Break;
  end;
  PCipherRec(Data).B := SwapInteger(A);
  PCipherRec(Data).A := SwapInteger(B);
end;

procedure TCipher_Cast128.Decode(Data: Pointer);
var
  T,I,A,B: LongWord;
  K: PInteger;
label
  Start;
begin
  K := @PIntArray(User)[15];
  B := SwapInteger(PCipherRec(Data).A);
  A := SwapInteger(PCipherRec(Data).B);
  I := 2;
  if FRounds <= 12 then Dec(K, 4) else goto Start;
  while I > 0 do
  begin
    Dec(I);
    T := ROL(K^ - A, PIntArray(K)[16]);
    B := B xor (Cast128_Data[0, T shr 24] +
                Cast128_Data[1, T shr 16 and $FF] xor
                Cast128_Data[2, T shr  8 and $FF] -
                Cast128_Data[3, T and $FF]);
    Dec(K);
    T := ROL(K^ xor B, PIntArray(K)[16]);
    A := A xor (Cast128_Data[0, T shr 24] -
                Cast128_Data[1, T shr 16 and $FF] +
                Cast128_Data[2, T shr  8 and $FF] xor
                Cast128_Data[3, T and $FF]);
    Dec(K);
Start:
    T := ROL(K^ + A, PIntArray(K)[16]);
    B := B xor (Cast128_Data[0, T shr 24] xor
                Cast128_Data[1, T shr 16 and $FF] -
                Cast128_Data[2, T shr  8 and $FF] +
                Cast128_Data[3, T and $FF]);
    Dec(K);
    T := ROL(K^ - B, PIntArray(K)[16]);
    A := A xor (Cast128_Data[0, T shr 24] +
                Cast128_Data[1, T shr 16 and $FF] xor
                Cast128_Data[2, T shr  8 and $FF] -
                Cast128_Data[3, T and $FF]);
    Dec(K);
    T := ROL(K^ xor A, PIntArray(K)[16]);
    B := B xor (Cast128_Data[0, T shr 24] -
                Cast128_Data[1, T shr 16 and $FF] +
                Cast128_Data[2, T shr  8 and $FF] xor
                Cast128_Data[3, T and $FF]);
    Dec(K);
    T := ROL(K^ + B, PIntArray(K)[16]);
    A := A xor (Cast128_Data[0, T shr 24] xor
                Cast128_Data[1, T shr 16 and $FF] -
                Cast128_Data[2, T shr  8 and $FF] +
                Cast128_Data[3, T and $FF]);
    Dec(K);
  end;
  PCipherRec(Data).A := SwapInteger(A);
  PCipherRec(Data).B := SwapInteger(B);
end;

procedure TCipher_Cast128.Init(const Key; Size: Integer; IVector: Pointer);
var
  Z,X,T: array[0..3] of LongWord;
  K: PIntArray;
  I: LongWord;
begin
  InitBegin(Size);
  if Size <= 10 then FRounds := 12 else FRounds := 16;
  K := User;
  FillChar(X, SizeOf(X), 0);
  Move(Key, X, Size);
  SwapIntegerBuffer(@X, @X, 4);
//  for I := 0 to 3 do X[I] := SwapInteger(X[I]);
  I := 0;
  while I < 32 do
  begin
    if I and 4 = 0 then
    begin
      Z[0] := X[0] xor Cast128_Key[0, X[3] shr 16 and $FF] xor
                       Cast128_Key[1, X[3] and $FF] xor
                       Cast128_Key[2, X[3] shr 24] xor
                       Cast128_Key[3, X[3] shr  8 and $FF] xor
                       Cast128_Key[2, X[2] shr 24];
      T[0] := Z[0];
      Z[1] := X[2] xor Cast128_Key[0, Z[0] shr 24] xor
                       Cast128_Key[1, Z[0] shr  8 and $FF] xor
                       Cast128_Key[2, Z[0] shr 16 and $FF] xor
                       Cast128_Key[3, Z[0] and $FF] xor
                       Cast128_Key[3, X[2] shr  8 and $FF];
      T[1] := Z[1];
      Z[2] := X[3] xor Cast128_Key[0, Z[1] and $FF] xor
                       Cast128_Key[1, Z[1] shr  8 and $FF] xor
                       Cast128_Key[2, Z[1] shr 16 and $FF] xor
                       Cast128_Key[3, Z[1] shr 24] xor
                       Cast128_Key[0, X[2] shr 16 and $FF];
      T[2] := Z[2];
      Z[3] := X[1] xor Cast128_Key[0, Z[2] shr  8 and $FF] xor
                       Cast128_Key[1, Z[2] shr 16 and $FF] xor
                       Cast128_Key[2, Z[2] and $FF] xor
                       Cast128_Key[3, Z[2] shr 24] xor
                       Cast128_Key[1, X[2] and $FF];
      T[3] := Z[3];
    end else
    begin
      X[0] := Z[2] xor Cast128_Key[0, Z[1] shr 16 and $FF] xor
                       Cast128_Key[1, Z[1] and $FF] xor
                       Cast128_Key[2, Z[1] shr 24] xor
                       Cast128_Key[3, Z[1] shr  8 and $FF] xor
                       Cast128_Key[2, Z[0] shr 24];
      T[0] := X[0];
      X[1] := Z[0] xor Cast128_Key[0, X[0] shr 24] xor
                       Cast128_Key[1, X[0] shr  8 and $FF] xor
                       Cast128_Key[2, X[0] shr 16 and $FF] xor
                       Cast128_Key[3, X[0] and $FF] xor
                       Cast128_Key[3, Z[0] shr  8 and $FF];
      T[1] := X[1];
      X[2] := Z[1] xor Cast128_Key[0, X[1] and $FF] xor
                       Cast128_Key[1, X[1] shr  8 and $FF] xor
                       Cast128_Key[2, X[1] shr 16 and $FF] xor
                       Cast128_Key[3, X[1] shr 24] xor
                       Cast128_Key[0, Z[0] shr 16 and $FF];
      T[2] := X[2];
      X[3] := Z[3] xor Cast128_Key[0, X[2] shr  8 and $FF] xor
                       Cast128_Key[1, X[2] shr 16 and $FF] xor
                       Cast128_Key[2, X[2] and $FF] xor
                       Cast128_Key[3, X[2] shr 24] xor
                       Cast128_Key[1, Z[0] and $FF];
      T[3] := X[3];
    end;
    case I and 12 of
      0,12:
        begin
          K[I +0] := Cast128_Key[0, T[2] shr 24] xor
                     Cast128_Key[1, T[2] shr 16 and $FF] xor
                     Cast128_Key[2, T[1] and $FF] xor
                     Cast128_Key[3, T[1] shr  8 and $FF];
          K[I +1] := Cast128_Key[0, T[2] shr  8 and $FF] xor
                     Cast128_Key[1, T[2] and $FF] xor
                     Cast128_Key[2, T[1] shr 16 and $FF] xor
                     Cast128_Key[3, T[1] shr 24];
          K[I +2] := Cast128_Key[0, T[3] shr 24] xor
                     Cast128_Key[1, T[3] shr 16 and $FF] xor
                     Cast128_Key[2, T[0] and $FF] xor
                     Cast128_Key[3, T[0] shr  8 and $FF];
          K[I +3] := Cast128_Key[0, T[3] shr  8 and $FF] xor
                     Cast128_Key[1, T[3] and $FF] xor
                     Cast128_Key[2, T[0] shr 16 and $FF] xor
                     Cast128_Key[3, T[0] shr 24];
        end;
      4,8:
        begin
          K[I +0] := Cast128_Key[0, T[0] and $FF] xor
                     Cast128_Key[1, T[0] shr  8 and $FF] xor
                     Cast128_Key[2, T[3] shr 24] xor
                     Cast128_Key[3, T[3] shr 16 and $FF];
          K[I +1] := Cast128_Key[0, T[0] shr 16 and $FF] xor
                     Cast128_Key[1, T[0] shr 24] xor
                     Cast128_Key[2, T[3] shr  8 and $FF] xor
                     Cast128_Key[3, T[3] and $FF];
          K[I +2] := Cast128_Key[0, T[1] and $FF] xor
                     Cast128_Key[1, T[1] shr  8 and $FF] xor
                     Cast128_Key[2, T[2] shr 24] xor
                     Cast128_Key[3, T[2] shr 16 and $FF];
          K[I +3] := Cast128_Key[0, T[1] shr 16 and $FF] xor
                     Cast128_Key[1, T[1] shr 24] xor
                     Cast128_Key[2, T[2] shr  8 and $FF] xor
                     Cast128_Key[3, T[2] and $FF];
        end;
    end;
    case I and 12 of
      0: begin
           K[I +0] := K[I +0] xor Cast128_Key[0, Z[0] shr  8 and $FF];
           K[I +1] := K[I +1] xor Cast128_Key[1, Z[1] shr  8 and $FF];
           K[I +2] := K[I +2] xor Cast128_Key[2, Z[2] shr 16 and $FF];
           K[I +3] := K[I +3] xor Cast128_Key[3, Z[3] shr 24];
         end;
      4: begin
           K[I +0] := K[I +0] xor Cast128_Key[0, X[2] shr 24];
           K[I +1] := K[I +1] xor Cast128_Key[1, X[3] shr 16 and $FF];
           K[I +2] := K[I +2] xor Cast128_Key[2, X[0] and $FF];
           K[I +3] := K[I +3] xor Cast128_Key[3, X[1] and $FF];
         end;
      8: begin
           K[I +0] := K[I +0] xor Cast128_Key[0, Z[2] shr 16 and $FF];
           K[I +1] := K[I +1] xor Cast128_Key[1, Z[3] shr 24];
           K[I +2] := K[I +2] xor Cast128_Key[2, Z[0] shr  8 and $FF];
           K[I +3] := K[I +3] xor Cast128_Key[3, Z[1] shr  8 and $FF];
         end;
     12: begin
          K[I +0] := K[I +0] xor Cast128_Key[0, X[0] and $FF];
          K[I +1] := K[I +1] xor Cast128_Key[1, X[1] and $FF];
          K[I +2] := K[I +2] xor Cast128_Key[2, X[2] shr 24];
          K[I +3] := K[I +3] xor Cast128_Key[3, X[3] shr 16 and $FF];
        end;
    end;
    if I >= 16 then
    begin
      K[I +0] := K[I +0] and $1F;
      K[I +1] := K[I +1] and $1F;
      K[I +2] := K[I +2] and $1F;
      K[I +3] := K[I +3] and $1F;
    end;
    Inc(I, 4);
  end;
  FillChar(X, SizeOf(X), 0);
  FillChar(Z, SizeOf(Z), 0);
  FillChar(T, SizeOf(T), 0);
  InitEnd(IVector);
end;

class procedure TCipher_Cast256.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 16;
  AKeySize := 32;
  AUserSize := 384;
end;

class function TCipher_Cast256.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0C5h,0F1h,095h,087h,08Ah,031h,01Eh,042h
         DB    01Ch,045h,010h,007h,049h,0DFh,0F2h,02Dh
         DB    075h,012h,005h,0B7h,0A9h,0A3h,030h,0DAh
         DB    09Eh,0F1h,03Bh,010h,0FEh,062h,04Fh,079h
end;

type
  PCast256Rec = ^TCast256Rec;
  TCast256Rec = packed record
                  case Integer of
                    0: (A,B,C,D: Integer);
                    1: (Z: array[0..3] of Integer);
                end;

procedure TCipher_Cast256.Encode(Data: Pointer);
var
  I,T,A,B,C,D: LongWord;
  K: PInteger;
begin
  K := User;
  SwapIntegerBuffer(Data, Data, 4);
  A := PCast256Rec(Data).A;
  B := PCast256Rec(Data).B;
  C := PCast256Rec(Data).C;
  D := PCast256Rec(Data).D;
  for I := 0 to 5 do
  begin
    T := ROL(K^ + D, PIntArray(K)[48]);
    C := C xor (Cast256_Data[0, T shr 24] xor
                Cast256_Data[1, T shr 16 and $FF] -
                Cast256_Data[2, T shr  8 and $FF] +
                Cast256_Data[3, T and $FF]);
    Inc(K);
    T := ROL(K^ xor C, PIntArray(K)[48]);
    B := B xor (Cast256_Data[0, T shr 24] -
                Cast256_Data[1, T shr 16 and $FF] +
                Cast256_Data[2, T shr  8 and $FF] xor
                Cast256_Data[3, T and $FF]);
    Inc(K);
    T := ROL(K^ - B, PIntArray(K)[48]);
    A := A xor (Cast256_Data[0, T shr 24] +
                Cast256_Data[1, T shr 16 and $FF] xor
                Cast256_Data[2, T shr  8 and $FF] -
                Cast256_Data[3, T and $FF]);
    Inc(K);
    T := ROL(K^ + A, PIntArray(K)[48]);
    D := D xor (Cast256_Data[0, T shr 24] xor
                Cast256_Data[1, T shr 16 and $FF] -
                Cast256_Data[2, T shr  8 and $FF] +
                Cast256_Data[3, T and $FF]);
    Inc(K);
  end;
  for I := 0 to 5 do
  begin
    T := ROL(K^ + A, PIntArray(K)[48]);
    D := D xor (Cast256_Data[0, T shr 24] xor
                Cast256_Data[1, T shr 16 and $FF] -
                Cast256_Data[2, T shr  8 and $FF] +
                Cast256_Data[3, T and $FF]);
    Inc(K);
    T := ROL(K^ - B, PIntArray(K)[48]);
    A := A xor (Cast256_Data[0, T shr 24] +
                Cast256_Data[1, T shr 16 and $FF] xor
                Cast256_Data[2, T shr  8 and $FF] -
                Cast256_Data[3, T and $FF]);
    Inc(K);
    T := ROL(K^ xor C, PIntArray(K)[48]);
    B := B xor (Cast256_Data[0, T shr 24] -
                Cast256_Data[1, T shr 16 and $FF] +
                Cast256_Data[2, T shr  8 and $FF] xor
                Cast256_Data[3, T and $FF]);
    Inc(K);
    T := ROL(K^ + D, PIntArray(K)[48]);
    C := C xor (Cast256_Data[0, T shr 24] xor
                Cast256_Data[1, T shr 16 and $FF] -
                Cast256_Data[2, T shr  8 and $FF] +
                Cast256_Data[3, T and $FF]);
    Inc(K);
  end;
  PCast256Rec(Data).A := A;
  PCast256Rec(Data).B := B;
  PCast256Rec(Data).C := C;
  PCast256Rec(Data).D := D;
  SwapIntegerBuffer(Data, Data, 4);
end;

procedure TCipher_Cast256.Decode(Data: Pointer);
var
  I,T,A,B,C,D: LongWord;
  K: PInteger;
begin
  K := @PIntArray(User)[47];
  SwapIntegerBuffer(Data, Data, 4);
  A := PCast256Rec(Data).A;
  B := PCast256Rec(Data).B;
  C := PCast256Rec(Data).C;
  D := PCast256Rec(Data).D;
  for I := 0 to 5 do
  begin
    T := ROL(K^ + D, PIntArray(K)[48]);
    C := C xor (Cast256_Data[0, T shr 24] xor
                Cast256_Data[1, T shr 16 and $FF] -
                Cast256_Data[2, T shr  8 and $FF] +
                Cast256_Data[3, T and $FF]);
    Dec(K);
    T := ROL(K^ xor C, PIntArray(K)[48]);
    B := B xor (Cast256_Data[0, T shr 24] -
                Cast256_Data[1, T shr 16 and $FF] +
                Cast256_Data[2, T shr  8 and $FF] xor
                Cast256_Data[3, T and $FF]);
    Dec(K);
    T := ROL(K^ - B, PIntArray(K)[48]);
    A := A xor (Cast256_Data[0, T shr 24] +
                Cast256_Data[1, T shr 16 and $FF] xor
                Cast256_Data[2, T shr  8 and $FF] -
                Cast256_Data[3, T and $FF]);
    Dec(K);
    T := ROL(K^ + A, PIntArray(K)[48]);
    D := D xor (Cast256_Data[0, T shr 24] xor
                Cast256_Data[1, T shr 16 and $FF] -
                Cast256_Data[2, T shr  8 and $FF] +
                Cast256_Data[3, T and $FF]);
    Dec(K);
  end;
  for I := 0 to 5 do
  begin
    T := ROL(K^ + A, PIntArray(K)[48]);
    D := D xor (Cast256_Data[0, T shr 24] xor
                Cast256_Data[1, T shr 16 and $FF] -
                Cast256_Data[2, T shr  8 and $FF] +
                Cast256_Data[3, T and $FF]);
    Dec(K);
    T := ROL(K^ - B, PIntArray(K)[48]);
    A := A xor (Cast256_Data[0, T shr 24] +
                Cast256_Data[1, T shr 16 and $FF] xor
                Cast256_Data[2, T shr  8 and $FF] -
                Cast256_Data[3, T and $FF]);
    Dec(K);
    T := ROL(K^ xor C, PIntArray(K)[48]);
    B := B xor (Cast256_Data[0, T shr 24] -
                Cast256_Data[1, T shr 16 and $FF] +
                Cast256_Data[2, T shr  8 and $FF] xor
                Cast256_Data[3, T and $FF]);
    Dec(K);
    T := ROL(K^ + D, PIntArray(K)[48]);
    C := C xor (Cast256_Data[0, T shr 24] xor
                Cast256_Data[1, T shr 16 and $FF] -
                Cast256_Data[2, T shr  8 and $FF] +
                Cast256_Data[3, T and $FF]);
    Dec(K);
  end;
  PCast256Rec(Data).A := A;
  PCast256Rec(Data).B := B;
  PCast256Rec(Data).C := C;
  PCast256Rec(Data).D := D;
  SwapIntegerBuffer(Data, Data, 4);
end;

procedure TCipher_Cast256.Init(const Key; Size: Integer; IVector: Pointer);
var
  X: array[0..7] of LongWord;
  M, R, I, J, T: LongWord;
  K: PInteger;
begin
  InitBegin(Size);
  FillChar(X, SizeOf(X), 0);
  Move(Key, X, Size);
//  SwapIntegerBuffer(@X, @X, 8);
  for I := 0 to 7 do X[I] := SwapInteger(X[I]);
  K := User;
  M := $5A827999;
  R := 19;
  for I := 0 to 11 do
  begin
    for J := 0 to 1 do
    begin
      T    := ROL(M + X[7], R);
      X[6] := X[6] xor (Cast256_Data[0, T shr 24] xor
                        Cast256_Data[1, T shr 16 and $FF] -
                        Cast256_Data[2, T shr  8 and $FF] +
                        Cast256_Data[3, T and $FF]);
      Inc(M, $6ED9EBA1);
      Inc(R, 17);
      T    := ROL(M xor X[6], R);
      X[5] := X[5] xor (Cast256_Data[0, T shr 24] -
                        Cast256_Data[1, T shr 16 and $FF] +
                        Cast256_Data[2, T shr  8 and $FF] xor
                        Cast256_Data[3, T and $FF]);
      Inc(M, $6ED9EBA1);
      Inc(R, 17);
      T    := ROL(M - X[5], R);
      X[4] := X[4] xor (Cast256_Data[0, T shr 24] +
                        Cast256_Data[1, T shr 16 and $FF] xor
                        Cast256_Data[2, T shr  8 and $FF] -
                        Cast256_Data[3, T and $FF]);
      Inc(M, $6ED9EBA1);
      Inc(R, 17);
      T    := ROL(M + X[4], R);
      X[3] := X[3] xor (Cast256_Data[0, T shr 24] xor
                        Cast256_Data[1, T shr 16 and $FF] -
                        Cast256_Data[2, T shr  8 and $FF] +
                        Cast256_Data[3, T and $FF]);
      Inc(M, $6ED9EBA1);
      Inc(R, 17);
      T    := ROL(M xor X[3], R);
      X[2] := X[2] xor (Cast256_Data[0, T shr 24] -
                        Cast256_Data[1, T shr 16 and $FF] +
                        Cast256_Data[2, T shr  8 and $FF] xor
                        Cast256_Data[3, T and $FF]);
      Inc(M, $6ED9EBA1);
      Inc(R, 17);
      T    := ROL(M - X[2], R);
      X[1] := X[1] xor (Cast256_Data[0, T shr 24] +
                        Cast256_Data[1, T shr 16 and $FF] xor
                        Cast256_Data[2, T shr  8 and $FF] -
                        Cast256_Data[3, T and $FF]);
      Inc(M, $6ED9EBA1);
      Inc(R, 17);
      T    := ROL(M + X[1], R);
      X[0] := X[0] xor (Cast256_Data[0, T shr 24] xor
                        Cast256_Data[1, T shr 16 and $FF] -
                        Cast256_Data[2, T shr  8 and $FF] +
                        Cast256_Data[3, T and $FF]);
      Inc(M, $6ED9EBA1);
      Inc(R, 17);
      T    := ROL(M xor X[0], R);
      X[7] := X[7] xor (Cast256_Data[0, T shr 24] -
                        Cast256_Data[1, T shr 16 and $FF] +
                        Cast256_Data[2, T shr  8 and $FF] xor
                        Cast256_Data[3, T and $FF]);
      Inc(M, $6ED9EBA1);
      Inc(R, 17);
    end;
    if I < 6 then
    begin
      PIntArray(K)[48] := X[0] and $1F;
      PIntArray(K)[49] := X[2] and $1F;
      PIntArray(K)[50] := X[4] and $1F;
      PIntArray(K)[51] := X[6] and $1F;
      PIntArray(K)[0] := X[7];
      PIntArray(K)[1] := X[5];
      PIntArray(K)[2] := X[3];
      PIntArray(K)[3] := X[1];
    end else
    begin
      PIntArray(K)[48] := X[6] and $1F;
      PIntArray(K)[49] := X[4] and $1F;
      PIntArray(K)[50] := X[2] and $1F;
      PIntArray(K)[51] := X[0] and $1F;
      PIntArray(K)[0] := X[1];
      PIntArray(K)[1] := X[3];
      PIntArray(K)[2] := X[5];
      PIntArray(K)[3] := X[7];
    end;
    Inc(K, 4);
  end;
  FillChar(X, SizeOf(X), 0);
  InitEnd(IVector);
end;

class procedure TCipher_Mars.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 16;
  AKeySize := 156;
  AUserSize := 160;
end;

class function TCipher_Mars.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0D8h,06Ah,0DFh,00Ch,0B5h,066h,024h,00Dh
         DB    0C5h,000h,0E0h,0A5h,0D8h,0C1h,0C2h,013h
         DB    045h,0EDh,0F1h,04Fh,0AFh,05Ch,075h,002h
         DB    01Dh,00Ah,08Dh,010h,06Dh,031h,05Ch,0D5h
end;

type
  PMarsRec = ^TMarsRec;
  TMarsRec = packed record
               case Integer of
                 0: (X: array[0..3] of LongWord);
                 1: (A,B,C,D: LongWord);
             end;

procedure TCipher_Mars.Encode(Data: Pointer);
var
  K: PInteger;
  I,L,R,A,B,C,D: LongWord;
begin
  K := User;
  A := PMarsRec(Data).A + K^; Inc(K);
  B := PMarsRec(Data).B + K^; Inc(K);
  C := PMarsRec(Data).C + K^; Inc(K);
  D := PMarsRec(Data).D + K^; Inc(K);
  for I := 0 to 1 do
  begin
    B := B xor Mars_Data[A and $FF] + Mars_Data[A shr 8 and $FF + 256];
    Inc(C, Mars_Data[A shr 16 and $FF]);
    D := D xor Mars_Data[A shr 24 + 256];
    A := (A shr 24 or A shl 8) + D;
    C := C xor Mars_Data[B and $FF] + Mars_Data[B shr 8 and $FF + 256];
    Inc(D, Mars_Data[B shr 16 and $FF]);
    A := A xor Mars_Data[B shr 24 + 256];
    B := (B shr 24 or B shl 8) + C;
    D := D xor Mars_Data[C and $FF] + Mars_Data[C shr 8 and $FF + 256];
    Inc(A, Mars_Data[C shr 16 and $FF]);
    B := B xor Mars_Data[C shr 24 + 256];
    C := C shr 24 or C shl 8;
    A := A xor Mars_Data[D and $FF] + Mars_Data[D shr 8 and $FF + 256];
    Inc(B, Mars_Data[D shr 16 and $FF]);
    C := C xor Mars_Data[D shr 24 + 256];
    D := D shr 24 or D shl 8;
  end;
  for I := 0 to 3 do
  begin
    L := A + K^;                    Inc(K);
    A := A shl 13 or A shr 19;
    R := A * K^;                    Inc(K);
    R := R shl 5 or R shr 27;
    Inc(C, L shl R or L shr (32 - R));
    L := Mars_Data[L and $1FF] xor R;
    R := R shl 5 or R shr 27;
    L := L xor R;
    L := L shl R or L shr (32 - R);
    if I <= 1 then
    begin
      Inc(B, L);
      D := D xor R;
    end else
    begin
      Inc(D, L);
      B := B xor R;
    end;
    L := B + K^;                    Inc(K);
    B := B shl 13 or B shr 19;
    R := B * K^;                    Inc(K);
    R := R shl 5 or R shr 27;
    Inc(D, L shl R or L shr (32 - R));
    L := Mars_Data[L and $1FF] xor R;
    R := R shl 5 or R shr 27;
    L := L xor R;
    L := L shl R or L shr (32 - R);
    if I <= 1 then
    begin
      Inc(C, L);
      A := A xor R;
    end else
    begin
      Inc(A, L);
      C := C xor R;
    end;
    L := C + K^;                    Inc(K);
    C := C shl 13 or C shr 19;
    R := C * K^;                    Inc(K);
    R := R shl 5 or R shr 27;
    Inc(A, L shl R or L shr (32 - R));
    L := Mars_Data[L and $1FF] xor R;
    R := R shl 5 or R shr 27;
    L := L xor R;
    L := L shl R or L shr (32 - R);
    if I <= 1 then
    begin
      Inc(D, L);
      B := B xor R;
    end else
    begin
      Inc(B, L);
      D := D xor R;
    end;
    L := D + K^;                    Inc(K);
    D := D shl 13 or D shr 19;
    R := D * K^;                    Inc(K);
    R := R shl 5 or R shr 27;
    Inc(B, L shl R or L shr (32 - R));
    L := Mars_Data[L and $1FF] xor R;
    R := R shl 5 or R shr 27;
    L := L xor R;
    L := L shl R or L shr (32 - R);
    if I <= 1 then
    begin
      Inc(A, L);
      C := C xor R;
    end else
    begin
      Inc(C, L);
      A := A xor R;
    end;
  end;
  for I := 0 to 1 do
  begin
    B := B xor Mars_Data[A and $FF + 256];
    Dec(C, Mars_Data[A shr 24]);
    D := D - Mars_Data[A shr 16 and $FF + 256] xor Mars_Data[A shr 8 and $FF];
    A := A shl 24 or A shr 8;
    C := C xor Mars_Data[B and $FF + 256];
    Dec(D, Mars_Data[B shr 24]);
    A := A - Mars_Data[B shr 16 and $FF + 256] xor Mars_Data[B shr 8 and $FF];
    B := B shl 24 or B shr 8;
    Dec(C, B);
    D := D xor Mars_Data[C and $FF + 256];
    Dec(A, Mars_Data[C shr 24]);
    B := B - Mars_Data[C shr 16 and $FF + 256] xor Mars_Data[C shr 8 and $FF];
    C := C shl 24 or C shr 8;
    Dec(D, A);
    A := A xor Mars_Data[D and $FF + 256];
    Dec(B, Mars_Data[D shr 24]);
    C := C - Mars_Data[D shr 16 and $FF + 256] xor Mars_Data[D shr 8 and $FF];
    D := D shl 24 or D shr 8;
  end;
  PMarsRec(Data).A := A - K^; Inc(K);
  PMarsRec(Data).B := B - K^; Inc(K);
  PMarsRec(Data).C := C - K^; Inc(K);
  PMarsRec(Data).D := D - K^;
end;

procedure TCipher_Mars.Decode(Data: Pointer);
var
  K: PInteger;
  I,L,R,A,B,C,D: LongWord;
begin
  K := @PIntArray(User)[39];
  D := PMarsRec(Data).D + K^; Dec(K);
  C := PMarsRec(Data).C + K^; Dec(K);
  B := PMarsRec(Data).B + K^; Dec(K);
  A := PMarsRec(Data).A + K^; Dec(K);
  for I := 0 to 1 do
  begin
    D := D shr 24 or D shl 8;
    C := C xor Mars_Data[D shr 8 and $FF] + Mars_Data[D shr 16 and $FF + 256];
    Inc(B, Mars_Data[D shr 24]);
    A := A xor Mars_Data[D and $FF + 256];
    Inc(D, A);
    C := C shr 24 or C shl 8;
    B := B xor Mars_Data[C shr 8 and $FF] + Mars_Data[C shr 16 and $FF + 256];
    Inc(A, Mars_Data[C shr 24]);
    D := D xor Mars_Data[C and $FF + 256];
    Inc(C, B);
    B := B shr 24 or B shl 8;
    A := A xor Mars_Data[B shr 8 and $FF] + Mars_Data[B shr 16 and $FF + 256];
    Inc(D, Mars_Data[B shr 24]);
    C := C xor Mars_Data[B and $FF + 256];
    A := A shr 24 or A shl 8;
    D := D xor Mars_Data[A shr 8 and $FF] + Mars_Data[A shr 16 and $FF + 256];
    Inc(C, Mars_Data[A shr 24]);
    B := B xor Mars_Data[A and $FF + 256];
  end;
  for I := 0 to 3 do
  begin
    R := D * K^;                      Dec(K);
    R := R shl 5 or R shr 27;
    D := D shr 13 or D shl 19;
    L := D + K^;                      Dec(K);
    Dec(B, L shl R or L shr (32 - R));
    L := Mars_Data[L and $1FF] xor R;
    R := R shl 5 or R shr 27;
    L := L xor R;
    L := L shl R or L shr (32 - R);
    if I <= 1 then
    begin
      Dec(C, L);
      A := A xor R;
    end else
    begin
      Dec(A, L);
      C := C xor R;
    end;
    R := C * K^;                      Dec(K);
    R := R shl 5 or R shr 27;
    C := C shr 13 or C shl 19;
    L := C + K^;                      Dec(K);
    Dec(A, L shl R or L shr (32 - R));
    L := Mars_Data[L and $1FF] xor R;
    R := R shl 5 or R shr 27;
    L := L xor R;
    L := L shl R or L shr (32 - R);
    if I <= 1 then
    begin
      Dec(B, L);
      D := D xor R;
    end else
    begin
      Dec(D, L);
      B := B xor R;
    end;
    R := B * K^;                      Dec(K);
    R := R shl 5 or R shr 27;
    B := B shr 13 or B shl 19;
    L := B + K^;                      Dec(K);
    Dec(D, L shl R or L shr (32 - R));
    L := Mars_Data[L and $1FF] xor R;
    R := R shl 5 or R shr 27;
    L := L xor R;
    L := L shl R or L shr (32 - R);
    if I <= 1 then
    begin
      Dec(A, L);
      C := C xor R;
    end else
    begin
      Dec(C, L);
      A := A xor R;
    end;
    R := A * K^;                      Dec(K);
    R := R shl 5 or R shr 27;
    A := A shr 13 or A shl 19;
    L := A + K^;                      Dec(K);
    Dec(C, L shl R or L shr (32 - R));
    L := Mars_Data[L and $1FF] xor R;
    R := R shl 5 or R shr 27;
    L := L xor R;
    L := L shl R or L shr (32 - R);
    if I <= 1 then
    begin
      Dec(D, L);
      B := B xor R;
    end else
    begin
      Dec(B, L);
      D := D xor R;
    end;
  end;
  for I := 0 to 1 do
  begin
    D := D shl 24 or D shr 8;
    C := C xor Mars_Data[D shr 24 + 256];
    Dec(B, Mars_Data[D shr 16 and $FF]);
    A := A - Mars_Data[D shr 8 and $FF + 256] xor Mars_Data[D and $FF];
    C := C shl 24 or C shr 8;
    B := B xor Mars_Data[C shr 24 + 256];
    Dec(A, Mars_Data[C shr 16 and $FF]);
    D := D - Mars_Data[C shr 8 and $FF + 256] xor Mars_Data[C and $FF];
    Dec(B, C);
    B := B shl 24 or B shr 8;
    A := A xor Mars_Data[B shr 24 + 256];
    Dec(D, Mars_Data[B shr 16 and $FF]);
    C := C - Mars_Data[B shr 8 and $FF + 256] xor Mars_Data[B and $FF];
    Dec(A, D);
    A := A shl 24 or A shr 8;
    D := D xor Mars_Data[A shr 24 + 256];
    Dec(C, Mars_Data[A shr 16 and $FF]);
    B := B - Mars_Data[A shr 8 and $FF + 256] xor Mars_Data[A and $FF];
  end;
  PMarsRec(Data).D := D - K^; Dec(K);
  PMarsRec(Data).C := C - K^; Dec(K);
  PMarsRec(Data).B := B - K^; Dec(K);
  PMarsRec(Data).A := A - K^;
end;

procedure TCipher_Mars.Init(const Key; Size: Integer; IVector: Pointer);

  procedure Mask(var X, M: LongWord);
  var
    U: LongWord;
  begin
    U := X and (X shr 1);
    U := U and (U shr 2);
    U := U and (U shr 4);
    U := U and (U shr 1) and (U shr 2);
    M := U;
    U := (X xor $FFFFFFFF) and ((X xor $FFFFFFFF) shr 1);
    U := U and (U shr 2);
    U := U and (U shr 4);
    U := U and (U shr 1) and (U shr 2);
    U := U or M;
    M := (U shl 1) or (U shl 2) or (U shl 3) or (U shl 4) or
         (U shl 5) or (U shl 6) or (U shl 7) or (U shl 8);
    M := (M or U or (U shl 9)) and ((X xor $FFFFFFFF) xor (X shl 1)) and ((X xor $FFFFFFFF) xor (X shr 1));
    M := M and $FFFFFFFC;
  end;

var
  T: array[-7..39] of LongWord;
  I,J: Integer;
  M,U,W: LongWord;
  K,B: PIntArray;
begin
  InitBegin(Size);
  K := User;
  B := @Mars_Data;
  Move(Key, K^, Size);
  Move(Mars_Key, T, SizeOf(Mars_Key));
  Size := Size div 4;
  for I := 0 to 38 do
  begin
    U    := T[I - 7] xor T[I - 2];
    T[I] := ROL(U, 3) xor K[I mod Size] xor LongWord(I);
  end;
  T[39] := Size;
  for J := 0 to 6 do
  begin
    for I := 1 to 39 do
    begin
      U := T[I] + B[T[I - 1] and $1FF];
      T[I] := ROL(U, 9);
    end;
    U := T[0] + B[T[39] and $1FF];
    T[0] := ROL(U, 9);
  end;
  for I := 0 to 39 do K[(I * 7) mod 40] := T[I];
  I := 5;
  repeat
    U := B[256 + (K[I] and $3)];
    J := K[I + 3] and $1F;
    W := K[I] or $3;
    Mask(W, M);
    K[I] := W xor (ROL(U, J) and M);
    Inc(I, 2);
  until I >= 37;
  InitEnd(IVector);
end;

class procedure TCipher_Misty.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 16;
  AUserSize := 128;
end;

class function TCipher_Misty.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    064h,07Bh,0C5h,0C6h,049h,045h,0AAh,095h
         DB    05Dh,064h,0CDh,056h,07Ch,06Ch,0B6h,047h
         DB    081h,057h,0FEh,08Ch,0F4h,084h,019h,0BCh
         DB    027h,060h,00Ch,0A6h,079h,085h,00Fh,0C9h
end;

function Misty_I(Value, Key: LongWord): LongWord;
begin
  Result := Misty_Data9[Value shr 7 and $1FF] xor (Value and $7F);
  Value := (Misty_Data7[Value and $7F] xor Result and $7F) xor (Key shr 9 and $7F);
  Result := Misty_Data9[Result xor (Key and $1FF)] xor Value or Value shl 9;
end;

function Misty_O(Value, K: LongWord; Key: PIntArray): LongWord;
begin
  Result := Misty_I((Value shr 16) xor Key[K], Key[(K + 5) and 7 + 8]) xor (Value and $FFFF);
  Value  := Misty_I((Value and $FFFF) xor Key[(K + 2) and 7], Key[(K + 1) and 7 + 8]) xor Result;
  Result := Misty_I(Result xor Key[(K + 7) and 7], Key[(K + 3) and 7 + 8]) xor Value;
  Result := Result or (Value xor Key[(k+4) and 7]) shl 16;
end;

function Misty_E(Value, K: LongWord; Key: PIntArray): LongWord;
begin
  Result := Value shr 16;
  Value  := Value and $FFFF;
  if K and 1 <> 0 then
  begin
    K      := K shr 1;
    Value  := Value  xor (Result and Key[(K + 2) and 7 + 8]);
    Result := Result xor (Value  or  Key[(K + 4) and 7]);
  end else
  begin
    K      := K shr 1;
    Value  := Value  xor (Result and Key[K]);
    Result := Result xor (Value  or  Key[(K + 6) and 7 + 8]);
  end;
  Result:= (Result shl 16) or Value;
end;

function Misty_D(Value, K: LongWord; Key: PIntArray): LongWord;
begin
  Result := Value shr 16;
  Value  := Value and $FFFF;
  if K and 1 <> 0 then
  begin
    K      := K shr 1;
    Result := Result xor (Value  or  Key[(K + 4) and 7]);
    Value  := Value  xor (Result and Key[(K + 2) and 7 + 8]);
  end else
  begin
    K      := K shr 1;
    Result := Result xor (Value  or  Key[(K +6) and 7 + 8]);
    Value  := Value  xor (Result and Key[K]);
  end;
  Result:= (Result shl 16) or Value;
end;

procedure TCipher_Misty.Encode(Data: Pointer);
var
  A,B: LongWord;
  Key: PIntArray;
begin
  Key := User;
  A := PCipherRec(Data).A;
  B := PCipherRec(Data).B;
  A := Misty_E(A, 0, Key);
  B := Misty_E(B, 1, Key) xor Misty_O(A, 0, Key);
  A := A xor Misty_O(B, 1, Key);
  A := Misty_E(A, 2, Key);
  B := Misty_E(B, 3, Key) xor Misty_O(A, 2, Key);
  A := A xor Misty_O(B, 3, Key);
  A := Misty_E(A, 4, Key);
  B := Misty_E(B, 5, Key) xor Misty_O(A, 4, Key);
  A := A xor Misty_O(B, 5, Key);
  A := Misty_E(A, 6, Key);
  B := Misty_E(B, 7, Key) xor Misty_O(A, 6, Key);
  A := A xor Misty_O(B, 7, Key);
  PCipherRec(Data).B := Misty_E(A, 8, Key);
  PCipherRec(Data).A := Misty_E(B, 9, Key);
end;

procedure TCipher_Misty.Decode(Data: Pointer);
var
  A,B: LongWord;
  Key: PIntArray;
begin
  Key := User;
  B := Misty_D(PCipherRec(Data).A, 9, Key);
  A := Misty_D(PCipherRec(Data).B, 8, Key);
  A := A xor Misty_O(B, 7, Key);
  B := Misty_D(B xor Misty_O(A, 6, Key), 7, Key);
  A := Misty_D(A, 6, Key);
  A := A xor Misty_O(B, 5, Key);
  B := Misty_D(B xor Misty_O(A, 4, Key), 5, Key);
  A := Misty_D(A, 4, Key);
  A := A xor Misty_O(B, 3, Key);
  B := Misty_D(B xor Misty_O(A, 2, Key), 3, Key);
  A := Misty_D(A, 2, Key);
  A := A xor Misty_O(B, 1, Key);
  PCipherRec(Data).B := Misty_D(B xor Misty_O(A, 0, Key), 1, Key);
  PCipherRec(Data).A := Misty_D(A, 0, Key);
end;

procedure TCipher_Misty.Init(const Key; Size: Integer; IVector: Pointer);
var
  K: array[0..15] of Byte;
  D: PIntArray;
  I: Integer;
begin
  InitBegin(Size);
  FillChar(K, SizeOf(K), 0);
  Move(Key, K, Size);
  D := User;
  for I := 0 to 7 do D[I] := K[I * 2] * 256 + K[I * 2 +1];
  for I := 0 to 7 do
  begin
    D[I +  8] := Misty_I(D[I], D[(I + 1) and 7]);
    D[I + 16] := D[I + 8] and $1FF;
    D[I + 24] := D[I + 8] shr 9;
  end;
  FillChar(K, SizeOf(K), 0);
  InitEnd(IVector);
end;

class procedure TCipher_RC2.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 128;
  AUserSize := 128;
end;

class function TCipher_RC2.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    00Eh,0C8h,0E5h,04Fh,0CCh,032h,0EAh,0BEh
         DB    029h,064h,0EFh,0BEh,053h,014h,0EDh,0DCh
         DB    0D7h,0ADh,0CFh,010h,047h,067h,0EEh,061h
         DB    0D9h,073h,0F2h,013h,0FCh,031h,008h,0EAh
end;

type
  PRC2Rec = ^TRC2Rec;
  TRC2Rec = packed record
              case Integer of
               0: (X: array[0..3] of Word);
               1: (A,B,C,D: Word);
            end;

procedure TCipher_RC2.Encode(Data: Pointer);

  function ROLADD16(Value, Add, Shift: Integer): Word; assembler;
  asm
      ADD  EAX,EDX
      ROL  AX,CL
  end;

var
  I: Integer;
  K: PWord;
  L: PWordArray;
begin
  K := User;
  L := User;
  with PRC2Rec(Data)^ do
  begin
    for I := 0 to 4 do
    begin
      A := ROLADD16(A, (B and not D) + (C and D) + K^, 1); Inc(K);
      B := ROLADD16(B, (C and not A) + (D and A) + K^, 2); Inc(K);
      C := ROLADD16(C, (D and not B) + (A and B) + K^, 3); Inc(K);
      D := ROLADD16(D, (A and not C) + (B and C) + K^, 5); Inc(K);
    end;
    Inc(A, L[D and $3F]);
    Inc(B, L[A and $3F]);
    Inc(C, L[B and $3F]);
    Inc(D, L[C and $3F]);
    for I := 0 to 5 do
    begin
      A := ROLADD16(A, (B and not D) + (C and D) + K^, 1); Inc(K);
      B := ROLADD16(B, (C and not A) + (D and A) + K^, 2); Inc(K);
      C := ROLADD16(C, (D and not B) + (A and B) + K^, 3); Inc(K);
      D := ROLADD16(D, (A and not C) + (B and C) + K^, 5); Inc(K);
    end;
    Inc(A, L[D and $3F]);
    Inc(B, L[A and $3F]);
    Inc(C, L[B and $3F]);
    Inc(D, L[C and $3F]);
    for I := 0 to 4 do
    begin
      A := ROLADD16(A, (B and not D) + (C and D) + K^, 1); Inc(K);
      B := ROLADD16(B, (C and not A) + (D and A) + K^, 2); Inc(K);
      C := ROLADD16(C, (D and not B) + (A and B) + K^, 3); Inc(K);
      D := ROLADD16(D, (A and not C) + (B and C) + K^, 5); Inc(K);
    end;
  end;
end;

procedure TCipher_RC2.Decode(Data: Pointer);

  function RORSUB16(Value, Sub, Shift: Integer): Word; assembler;
  asm
      ROR  AX,CL
      SUB  EAX,EDX
  end;

var
  I: Integer;
  K: PWord;
  L: PWordArray;
begin
  L := User;
  K := @L[63];
  with PRC2Rec(Data)^ do
  begin
    for I := 0 to 4 do
    begin
      D := RORSUB16(D, (A and not C) + (B and C) + K^, 5); Dec(K);
      C := RORSUB16(C, (D and not B) + (A and B) + K^, 3); Dec(K);
      B := RORSUB16(B, (C and not A) + (D and A) + K^, 2); Dec(K);
      A := RORSUB16(A, (B and not D) + (C and D) + K^, 1); Dec(K);
    end;
    Dec(D, L[C and $3F]);
    Dec(C, L[B and $3F]);
    Dec(B, L[A and $3F]);
    Dec(A, L[D and $3F]);
    for I := 0 to 5 do
    begin
      D := RORSUB16(D, (A and not C) + (B and C) + K^, 5); Dec(K);
      C := RORSUB16(C, (D and not B) + (A and B) + K^, 3); Dec(K);
      B := RORSUB16(B, (C and not A) + (D and A) + K^, 2); Dec(K);
      A := RORSUB16(A, (B and not D) + (C and D) + K^, 1); Dec(K);
    end;
    Dec(D, L[C and $3F]);
    Dec(C, L[B and $3F]);
    Dec(B, L[A and $3F]);
    Dec(A, L[D and $3F]);
    for I := 0 to 4 do
    begin
      D := RORSUB16(D, (A and not C) + (B and C) + K^, 5); Dec(K);
      C := RORSUB16(C, (D and not B) + (A and B) + K^, 3); Dec(K);
      B := RORSUB16(B, (C and not A) + (D and A) + K^, 2); Dec(K);
      A := RORSUB16(A, (B and not D) + (C and D) + K^, 1); Dec(K);
    end;
  end;
end;

procedure TCipher_RC2.Init(const Key; Size: Integer; IVector: Pointer);
var
  I: Integer;
  K: PByteArray;
begin
  InitBegin(Size);
  K := User;
  Move(Key, K^, Size);
  for I := Size to 127 do
    K[I] := RC2_Data[(K[I - Size] + K[I - 1]) and $FF];
  K[0] := RC2_Data[K[0]];
  InitEnd(IVector);
end;

const
  RC4_BufSize = 16;

class procedure TCipher_RC4.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := RC4_BufSize;
  AKeySize := 256;
  AUserSize := 256 * 2;
end;

class function TCipher_RC4.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    02Dh,08Fh,0EEh,042h,087h,07Bh,0AEh,072h
         DB    0F8h,02Bh,08Ch,0A5h,012h,014h,0A8h,07Eh
         DB    07Eh,08Ch,0DBh,05Eh,096h,049h,06Ch,09Ch
         DB    0EEh,05Eh,020h,06Ch,07Ah,067h,002h,05Dh
end;

procedure TCipher_RC4.Encode(Data: Pointer);
var
  D: PByteArray;
  B: PByte;
  X,S: Byte;
begin
  D := User;
  B := Data;
  for X := 0 to RC4_BufSize -1 do
  begin
    Inc(FI);
    S := D[FI];
    Inc(FJ, S);
    D[FI] := D[FJ];
    D[FJ] := S;
    B^    := B^ xor D[(D[FI] + S) and $FF];
    Inc(B);
  end;
end;

procedure TCipher_RC4.Decode(Data: Pointer);
begin
  Encode(Data);
end;

procedure TCipher_RC4.Init(const Key; Size: Integer; IVector: Pointer);
var
  K: array[0..255] of Byte;
  D: PByteArray;
  I,J,S: Integer;
begin
  InitBegin(Size);
  FI := 0;
  FJ := 0;
  J := 0;
  D := User;
  for I := 0 to 255 do
  begin
    D[I] := I;
    K[I] := PByteArray(@Key)[I mod Size];
  end;
  for I := 0 to 255 do
  begin
    J := (J + D[I] + K[I]) and $FF;
    S := D[I];
    D[I] := D[J];
    D[J] := S;
  end;
  InitEnd(IVector);
{Save the Key}
  Move(D[0], D[256], 256);
  FSI := FI;
  FSJ := FJ;
  FillChar(K, SizeOf(K), 0);
end;

procedure TCipher_RC4.Done;
begin
  inherited Done;
  FI := FSI;
  FJ := FSJ;
  Move(PByteArray(User)[256], User^, 256);
end;

procedure TCipher_RC5.SetRounds(Value: Integer);
begin
  if Value < 8 then Value := 8 else
    if Value > 16 then Value := 16;
  FRounds := Value;
end;

class procedure TCipher_RC5.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 256;
  AUserSize := 136;
end;

class function TCipher_RC5.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    010h,039h,02Ch,0E0h,00Bh,05Fh,009h,07Fh
         DB    0D6h,0B1h,06Ch,00Eh,0B9h,075h,0D5h,0CCh
         DB    0FCh,0BEh,0B5h,08Dh,041h,0ACh,054h,07Ch
         DB    083h,030h,026h,09Dh,0ACh,0CBh,00Ah,069h
end;

procedure TCipher_RC5.Encode(Data: Pointer);

  function XORROLADD(Value, Add, Shift: Integer): Integer; assembler;
  asm
      XOR  EAX,ECX
      ROL  EAX,CL
      ADD  EAX,EDX
  end;

var
  K: PInteger;
  I,A,B: LongWord;
begin
  K := User;
  A := PCipherRec(Data).A + K^; Inc(K);
  B := PCipherRec(Data).B + K^; Inc(K);
  for I := 1 to FRounds do
  begin
    A := XORROLADD(A, K^, B); Inc(K);
    B := XORROLADD(B, K^, A); Inc(K); 
  end;
  PCipherRec(Data).A := A;
  PCipherRec(Data).B := B;
end;

procedure TCipher_RC5.Decode(Data: Pointer);

  function RORSUBXOR(Value, Sub, Shift: Integer): Integer; assembler;
  asm
     SUB  EAX,EDX
     ROR  EAX,CL
     XOR  EAX,ECX
  end;

var
  I,A,B: LongWord;
  K: PInteger;
begin
  K := @PIntArray(User)[FRounds shl 1 +1];
  A := PCipherRec(Data).A;
  B := PCipherRec(Data).B;
  for I := 1 to FRounds do
  begin
    B := RORSUBXOR(B, K^, A); Dec(K);
    A := RORSUBXOR(A, K^, B); Dec(K);
  end;
  PCipherRec(Data).B := B - K^; Dec(K);
  PCipherRec(Data).A := A - K^;
end;

procedure TCipher_RC5.Init(const Key; Size: Integer; IVector: Pointer);
var
  K: array[0..63] of LongWord;
  D: PIntArray;
  I,J,L,A,B,Z: LongWord;
begin
  InitBegin(Size);
  if (FRounds < 8) or (FRounds > 16) then FRounds := 12;
  D := User;
  FillChar(K, SizeOf(K), 0);
  Move(Key, K, Size);
  L := Size shr 2;
  if Size and 3 <> 0 then Inc(L);
  J := $B7E15163;
  for I := 0 to (FRounds + 1) * 2 do
  begin
    D[I] := J;
    Inc(J, $9E3779B9);
  end;
  if L > LongWord(FRounds + 1) * 2 then Z := L * 3 else Z := (FRounds + 1) * 6;
  I := 0;
  J := 0;
  A := 0;
  B := 0;
  for Z := Z downto 1 do
  begin
    A := ROL(D[I] + A + B, 3);
    D[I] := A;
    B := ROL(K[J] + A + B, A + B);
    K[J] := B;
    I := (I + 1) mod (LongWord(FRounds + 1) * 2);
    J := (J + 1) mod L;
  end;
  FillChar(K, SizeOf(K), 0);
  InitEnd(IVector);
end;

procedure TCipher_RC6.SetRounds(Value: Integer);
begin
  if Value < 16 then Value := 16 else
    if Value > 24 then Value := 24;
  FRounds := Value;
end;

class procedure TCipher_RC6.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 16;
  AKeySize := 256;
  AUserSize := 272;
end;

class function TCipher_RC6.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    098h,071h,065h,0A1h,010h,0FEh,0BDh,0F9h
         DB    007h,085h,03Eh,0FCh,021h,0DBh,0FCh,0A1h
         DB    08Fh,05Fh,08Bh,0F7h,045h,028h,081h,00Dh
         DB    0EFh,09Ah,022h,07Ah,0F0h,062h,02Ch,0C6h
end;

type
  PRC6Rec = ^TRC6Rec;
  TRC6Rec = packed record
              case Integer of
                0: (X: array[0..3] of LongWord);
                1: (A,B,C,D: LongWord);
            end;

function MULROL(Value: Integer): Integer; assembler;
asm
      MOV  EDX,EAX
      SHL  EDX,1
      INC  EDX
      IMUL EAX,EDX
      ROL  EAX,5
end;

procedure TCipher_RC6.Encode(Data: Pointer);
var
  K: PInteger;
  I,T,U: LongWord;
begin
  K := User;
  with PRC6Rec(Data)^ do
  begin
    Inc(B, K^); Inc(K);
    Inc(D, K^); Inc(K);
    for I := 1 to FRounds do
    begin
      T := MULROL(B);
      U := MULROL(D);
      A := ROLADD(A xor T, K^, U); Inc(K);
      C := ROLADD(C xor U, K^, T); Inc(K);

      T := A; A := B; B := C; C := D; D := T;
    end;
    Inc(A, K^); Inc(K);
    Inc(C, K^);
  end;
end;

procedure TCipher_RC6.Decode(Data: Pointer);
var
  I,U,T: LongWord;
  K: PInteger;
begin
  K := @PIntArray(User)[FRounds * 2 +3];
  with PRC6Rec(Data)^ do
  begin
    Dec(C, K^); Dec(K);
    Dec(A, K^); Dec(K);
    for I := 1 to FRounds do
    begin
      T := A; A := D; D := C; C := B; B := T;

      U := MULROL(D);
      T := MULROL(B);
      C := ROR(C - K^, T) xor U;      Dec(K);
      A := ROR(A - K^, U) xor T;      Dec(K);
    end;
    Dec(D, K^); Dec(K);
    Dec(B, K^);
  end;
end;

procedure TCipher_RC6.Init(const Key; Size: Integer; IVector: Pointer);
var
  K: array[0..63] of LongWord;
  D: PIntArray;
  I,J,L,A,B,Z: LongWord;
begin
  InitBegin(Size);
  if (FRounds < 16) or (FRounds > 24) then FRounds := 20;
  D := User;
  FillChar(K, SizeOf(K), 0);
  Move(Key, K, Size);
  L := Size shr 2;
  if Size and 3 <> 0 then Inc(L);
  J := $B7E15163;
  for I := 0 to (FRounds + 2) * 2 do
  begin
    D[I] := J;
    Inc(J, $9E3779B9);
  end;
  if L > LongWord(FRounds + 2) * 2 then Z := L * 3 else Z := (FRounds + 2) * 6;
  I := 0;
  J := 0;
  A := 0;
  B := 0;
  for Z := Z downto 1 do
  begin
    A := ROL(D[I] + A + B, 3);
    D[I] := A;
    B := ROL(K[J] + A + B, A + B);
    K[J] := B;
    I := (I + 1) mod (LongWord(FRounds + 2) * 2);
    J := (J + 1) mod L;
  end;
  FillChar(K, SizeOf(K), 0);
  InitEnd(IVector);
end;

const
  FROG_BufSize =  32;  {change this to 8,16,32,64,128}
  FROG_Rounds  =   8;

class procedure TCipher_FROG.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := FROG_BufSize;
  AKeySize := 125;
  AUserSize := (FROG_BufSize * 2 + 256) * FROG_Rounds * 2;
end;

class function TCipher_FROG.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    022h,02Dh,006h,067h,016h,031h,09Ch,0C8h
         DB    0A5h,056h,04Bh,04Ch,0F7h,069h,0B0h,07Ah
         DB    0D5h,01Fh,09Fh,0B8h,0D0h,00Eh,0EDh,0D1h
         DB    070h,050h,09Bh,0C8h,00Bh,035h,028h,04Ah
end;

procedure TCipher_FROG.Encode(Data: Pointer);
var
  D,U: PByteArray;
  I,J,A: Integer;
  X,Y: PByte;
begin
  D := Data;
  U := User;
  for I := 1 to FROG_Rounds do
  begin
    X := Pointer(U);
    Inc(PChar(U), BufSize);
    Y := Pointer(PChar(U) + 256);
    for J := 0 to BufSize-2 do
    begin
      A        := D[J];          {holds in A, Compiler make faster Code}
      A        := U[A xor X^];
      Inc(X);
      D[J + 1] := D[J + 1] xor A;
      D[Y^]    := D[Y^] xor A;
      Inc(Y);
      D[J]     := A;
    end;
    J     := BufSize-1;
    A     := D[J];
    A     := U[A xor X^];
    D[0]  := D[0] xor A;
    D[Y^] := D[Y^] xor A;
    D[J]  := A;
    Inc(PChar(U), BufSize + 256);
  end;
end;

procedure TCipher_FROG.Decode(Data: Pointer);
var
  D,U: PByteArray;
  I,J,A: Integer;
  X,Y: PByte;
begin
  D := Data;
  U := User;
  Inc(PChar(U), UserSize);
  for I := 1 to FROG_Rounds do
  begin
    Y := Pointer(U);
    Dec(PChar(U), BufSize + 256);
    X := Pointer(U);
    J := BufSize -1;
    Dec(Y);
    A     := D[J];          {holds in A, Compiler make faster Code}
    D[Y^] := D[Y^] xor A;
    D[0]  := D[0] xor A;
    Dec(X);
    D[J]  := U[A] xor X^;
    repeat
      Dec(J);
      Dec(Y);
      A       := D[J];
      D[Y^]   := D[Y^] xor A;
      D[J +1] := D[J +1] xor A;
      Dec(X);
      D[J]    := U[A] xor X^;
    until J = 0;
    Dec(PChar(U), BufSize);
  end;
end;

procedure TCipher_FROG.Init(const Key; Size: Integer; IVector: Pointer);

  procedure MakeKey(D: PByteArray; Invert: Boolean);

    procedure InvertPermutation(D: PByteArray);
    var
      Inv: array[0..255] of Byte;
      I: Integer;
    begin
      for I := 0 to 255 do Inv[D[I]] := I;
      Move(Inv, D^, 256);
    end;

    procedure MakePermutation(D: PByteArray; Size: Integer);
    var
      Use: array[0..255] of Byte;
      I,J,Last: Integer;
    begin
      for I := 0 to 255 do Use[I] := I;
      Last := Size-1;
      J := 0;
      for I := 0 to Size-2 do
      begin
        J := (J + D[I]) mod (Last + 1);
        D[I] := Use[J];
        if J < Last then Move(Use[J + 1], Use[J], Last - J);
        Dec(Last);
        if J > Last then J := 0;
      end;
      D[Size-1] := Use[0];
    end;

  var
    Used: array[0..127] of Boolean;
    X,J,I,K,L: Integer;
    P: PByteArray;
  begin
    P := D;
    for X := 1 to FROG_Rounds do
    begin
      Inc(PChar(P), BufSize);
      MakePermutation(P, 256);
      if Invert then InvertPermutation(P);
      Inc(PChar(P), 256);
      MakePermutation(P, BufSize);
      FillChar(Used, BufSize, False);
      J := 0;
      for I := 0 to BufSize -2 do
      begin
        if P[J] = 0 then
        begin
          K := J;
          repeat
            K := (K + 1) mod BufSize;
          until not Used[K];
          P[J] := K;
          L := K;
          while P[L] <> K do L := P[L];
          P[L] := 0;
        end;
        Used[J] := True;
        J := P[J];
      end;
      for I := 0 to BufSize - 1 do
        if P[I] = (I + 1) mod BufSize then
          P[I] := (I + 2) mod BufSize;
      Inc(PChar(P), BufSize);
    end;
  end;

  procedure HashKey(D: PByteArray);
  var
    I,J,K: Integer;
    P: PByte;
  begin
    J := 0;
    K := 0;
    P := User;
    for I := 0 to UserSize div 2 -1 do
    begin
      P^ := FROG_Data[J] xor TByteArray(Key)[K];
      Inc(P);
      if J < 250 then Inc(J) else J := 0;
      if K < Size-1 then Inc(K) else K := 0;
    end;
    MakeKey(PByteArray(User), False);
    I := Size;
    if I > BufSize then I := BufSize;
    XORBuffers(Buffer, @Key, I, Buffer);
    PByte(Buffer)^ := PByte(Buffer)^ xor Size;
    I := UserSize div 2;
    P := Pointer(D);
    while I > 0 do
    begin
      Encode(Buffer);
      J := I;
      if J > BufSize then J := BufSize;
      Move(Buffer^, P^, J);
      Inc(P, J);
      Dec(I, J);
    end;
  end;

var
  UKey: PByteArray;
begin
  InitBegin(Size);
  UKey := AllocMem(UserSize shr 1);
  try
    HashKey(UKey);
    Protect;
    Move(UKey^, User^, UserSize shr 1);
    MakeKey(User, False);    {encyption Key}
    MakeKey(UKey, True);     {decryption Key}
    Move(UKey^, PByteArray(User)[UserSize shr 1], UserSize shr 1);
    InitEnd(IVector);
  finally
    ReallocMem(UKey, 0);
  end;
end;

const
{don't change this}
  Rijndael_Blocks =  4;
  Rijndael_Rounds = 14;

class procedure TCipher_Rijndael.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := Rijndael_Blocks * 4;
  AKeySize := 32;
  AUserSize := (Rijndael_Rounds + 1) * Rijndael_Blocks * SizeOf(Integer) * 2;
end;

class function TCipher_Rijndael.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    094h,06Dh,02Bh,05Eh,0E0h,0ADh,01Bh,05Ch
         DB    0A5h,023h,0A5h,013h,095h,08Bh,03Dh,02Dh
         DB    093h,087h,0F3h,037h,045h,051h,0F6h,058h
         DB    09Bh,0E7h,090h,01Bh,036h,087h,0F9h,0A9h
end;

procedure TCipher_Rijndael.Encode(Data: Pointer);
var
  P,K: PInteger;
  I,A,B,C,D: Integer;
begin
  P := User;
  K := Data;
  for I := 2 to FRounds do
  begin
    A := K^ xor P^;                             Inc(P); Inc(K);
    B := K^ xor P^;                             Inc(P); Inc(K);
    C := K^ xor P^;                             Inc(P); Inc(K);
    D := K^ xor P^;                             Inc(P);

    K^ := Rijndael_T[0, D and $FF]        xor
          Rijndael_T[1, A shr  8 and $FF] xor
          Rijndael_T[2, B shr 16 and $FF] xor
          Rijndael_T[3, C shr 24];                      Dec(K);
    K^ := Rijndael_T[0, C and $FF]        xor
          Rijndael_T[1, D shr  8 and $FF] xor
          Rijndael_T[2, A shr 16 and $FF] xor
          Rijndael_T[3, B shr 24];                      Dec(K);
    K^ := Rijndael_T[0, B and $FF]        xor
          Rijndael_T[1, C shr  8 and $FF] xor
          Rijndael_T[2, D shr 16 and $FF] xor
          Rijndael_T[3, A shr 24];                      Dec(K);
    K^ := Rijndael_T[0, A and $FF]        xor
          Rijndael_T[1, B shr  8 and $FF] xor
          Rijndael_T[2, C shr 16 and $FF] xor
          Rijndael_T[3, D shr 24];
  end;

  A := K^ xor P^;                                       Inc(P); Inc(K);
  B := K^ xor P^;                                       Inc(P); Inc(K);
  C := K^ xor P^;                                       Inc(P); Inc(K);
  D := K^ xor P^;                                       Inc(P);

  K^ := Rijndael_S[0, D and $FF]               or
        Rijndael_S[0, A shr  8 and $FF] shl  8 or
        Rijndael_S[0, B shr 16 and $FF] shl 16 or
        Rijndael_S[0, C shr 24]         shl 24;                 Dec(K);
  K^ := Rijndael_S[0, C and $FF]               or
        Rijndael_S[0, D shr  8 and $FF] shl  8 or
        Rijndael_S[0, A shr 16 and $FF] shl 16 or
        Rijndael_S[0, B shr 24]         shl 24;                 Dec(K);
  K^ := Rijndael_S[0, B and $FF]               or
        Rijndael_S[0, C shr  8 and $FF] shl  8 or
        Rijndael_S[0, D shr 16 and $FF] shl 16 or
        Rijndael_S[0, A shr 24]         shl 24;                 Dec(K);
  K^ := Rijndael_S[0, A and $FF]               or
        Rijndael_S[0, B shr  8 and $FF] shl  8 or
        Rijndael_S[0, C shr 16 and $FF] shl 16 or
        Rijndael_S[0, D shr 24]         shl 24;

  for I := 1 to Rijndael_Blocks do
  begin
    K^ := K^ xor P^;
    Inc(P);
    Inc(K);
  end;
end;

procedure TCipher_Rijndael.Decode(Data: Pointer);
var
  P,K: PInteger;
  I,A,B,C,D: Integer;
begin
  P := Pointer(PChar(User) + UserSize shr 1);
  Inc(P, FRounds * 4 +3);
  K := Pointer(PChar(Data) + 12);
  for I := 2 to FRounds do
  begin
    D := K^ xor P^;                             Dec(P); Dec(K);
    C := K^ xor P^;                             Dec(P); Dec(K);
    B := K^ xor P^;                             Dec(P); Dec(K);
    A := K^ xor P^;                             Dec(P);

    K^ := Rijndael_T[4, A and $FF]        xor
          Rijndael_T[5, D shr  8 and $FF] xor
          Rijndael_T[6, C shr 16 and $FF] xor
          Rijndael_T[7, B shr 24];                      Inc(K);
    K^ := Rijndael_T[4, B and $FF]        xor
          Rijndael_T[5, A shr  8 and $FF] xor
          Rijndael_T[6, D shr 16 and $FF] xor
          Rijndael_T[7, C shr 24];                      Inc(K);
    K^ := Rijndael_T[4, C and $FF]        xor
          Rijndael_T[5, B shr  8 and $FF] xor
          Rijndael_T[6, A shr 16 and $FF] xor
          Rijndael_T[7, D shr 24];                      Inc(K);
    K^ := Rijndael_T[4, D and $FF]        xor
          Rijndael_T[5, C shr  8 and $FF] xor
          Rijndael_T[6, B shr 16 and $FF] xor
          Rijndael_T[7, A shr 24];
  end;

  D := K^ xor P^;                                       Dec(P); Dec(K);
  C := K^ xor P^;                                       Dec(P); Dec(K);
  B := K^ xor P^;                                       Dec(P); Dec(K);
  A := K^ xor P^;                                       Dec(P);

  K^ := Rijndael_S[1, A and $FF]               or
        Rijndael_S[1, D shr  8 and $FF] shl  8 or
        Rijndael_S[1, C shr 16 and $FF] shl 16 or
        Rijndael_S[1, B shr 24]         shl 24;                 Inc(K);
  K^ := Rijndael_S[1, B and $FF]               or
        Rijndael_S[1, A shr  8 and $FF] shl  8 or
        Rijndael_S[1, D shr 16 and $FF] shl 16 or
        Rijndael_S[1, C shr 24]         shl 24;                 Inc(K);
  K^ := Rijndael_S[1, C and $FF]               or
        Rijndael_S[1, B shr  8 and $FF] shl  8 or
        Rijndael_S[1, A shr 16 and $FF] shl 16 or
        Rijndael_S[1, D shr 24]         shl 24;                 Inc(K);
  K^ := Rijndael_S[1, D and $FF]               or
        Rijndael_S[1, C shr  8 and $FF] shl  8 or
        Rijndael_S[1, B shr 16 and $FF] shl 16 or
        Rijndael_S[1, A shr 24]         shl 24;

  for I := 0 to 3 do
  begin
    K^ := K^ xor P^;
    Dec(P);
    Dec(K);
  end;
end;

procedure TCipher_Rijndael.Init(const Key; Size: Integer; IVector: Pointer);
var
  K: array[0..7] of Integer;

  procedure BuildEncodeKey;
  const
    RND_Data: array[0..29] of Byte = (
      $01,$02,$04,$08,$10,$20,$40,$80,$1B,$36,$6C,$D8,$AB,$4D,$9A,
      $2F,$5E,$BC,$63,$C6,$97,$35,$6A,$D4,$B3,$7D,$FA,$EF,$C5,$91);
  var
    T,R: Integer;

    procedure NextRounds;
    var
      J: Integer;
    begin
      J := 0;
      while (J < FRounds-6) and (R <= FRounds) do
      begin
        while (J < FRounds-6) and (T < Rijndael_Blocks) do
        begin
          PIntArray(User)[R * Rijndael_Blocks + T] := K[J];
          Inc(J);
          Inc(T);
        end;
        if T = Rijndael_Blocks then
        begin
          T := 0;
          Inc(R);
        end;
      end;
    end;

  var
    RND: PByte;
    B: PByte;
    I: Integer;
  begin
    R := 0;
    T := 0;
    RND := @RND_Data;
    NextRounds;
    while R <= FRounds do
    begin
      B  := @K;
      B^ := B^ xor Rijndael_S[0, K[FRounds -7] shr  8 and $FF] xor RND^; Inc(B);
      B^ := B^ xor Rijndael_S[0, K[FRounds -7] shr 16 and $FF];          Inc(B);
      B^ := B^ xor Rijndael_S[0, K[FRounds -7] shr 24];                  Inc(B);
      B^ := B^ xor Rijndael_S[0, K[FRounds -7] and $FF];
      Inc(RND);
      if FRounds = 14 then
      begin
        for I := 1 to 7 do K[I] := K[I] xor K[I -1];
        B  := @K[4];
        B^ := B^ xor Rijndael_S[0, K[3] and $FF];         Inc(B);
        B^ := B^ xor Rijndael_S[0, K[3] shr  8 and $FF];  Inc(B);
        B^ := B^ xor Rijndael_S[0, K[3] shr 16 and $FF];  Inc(B);
        B^ := B^ xor Rijndael_S[0, K[3] shr 24];
        for I := 5 to 7 do K[I] := K[I] xor K[I -1];
      end else
        for I := 1 to FRounds -7 do K[I] := K[I] xor K[I -1];
      NextRounds;
    end;
  end;

  procedure BuildDecodeKey;
  var
    I: Integer;
    D: PInteger;
  begin
    D := Pointer(PChar(User) + UserSize shr 1);
    Move(User^, D^, UserSize shr 1);
    Inc(D, 4);
    for I := 0 to FRounds * 4 - 5 do
    begin
      D^ :=     Rijndael_Key[D^ and $FF] xor
            ROL(Rijndael_Key[D^ shr  8 and $FF],  8) xor
            ROL(Rijndael_Key[D^ shr 16 and $FF], 16) xor
            ROL(Rijndael_Key[D^ shr 24],         24);
      Inc(D);
    end;
  end;

begin
  InitBegin(Size);
  if Size <= 16 then FRounds := 10 else
    if Size <= 24 then FRounds := 12 else FRounds := 14;
  FillChar(K, SizeOf(K), 0);
  Move(Key, K, Size);
  BuildEncodeKey;
  BuildDecodeKey;
  FillChar(K, SizeOf(K), 0);
  InitEnd(IVector);
end;
{$IFDEF VER_D3H}
type
  PSkipjackTab = ^TSkipjackTab;
  TSkipjackTab = array[0..255] of Byte;

class procedure TCipher_Skipjack.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 10;
  AUserSize := $A00;
end;

class function TCipher_Skipjack.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0D5h,013h,0A6h,092h,0ECh,024h,035h,0E8h
         DB    017h,04Eh,02Bh,055h,05Eh,08Dh,027h,0DAh
         DB    0C9h,09Ah,0A9h,0B9h,021h,03Dh,0A0h,001h
         DB    018h,002h,0B3h,00Eh,0B7h,0B5h,051h,0EAh
end;

procedure TCipher_Skipjack.Encode(Data: Pointer);
var
  Tab,Min: PSkipjackTab;
  Max: PChar;
  K,T,A,B,C,D: Integer;
begin
  Min := User;
  Max := PChar(Min) + 9 * 256;
  Tab := Min;
  A   := Swap(PWordArray(Data)[0]); {holds as Integer, Compiler make faster Code}
  B   := Swap(PWordArray(Data)[1]);
  C   := Swap(PWordArray(Data)[2]);
  D   := Swap(PWordArray(Data)[3]);
  K   := 0;
  repeat
    Inc(K);
    T := A;
    T := T xor Tab[T and $FF] shl 8;   Inc(Tab); if Tab > Max then Tab := Min;
    T := T xor Tab[T shr 8];           Inc(Tab); if Tab > Max then Tab := Min;
    T := T xor Tab[T and $FF] shl 8;   Inc(Tab); if Tab > Max then Tab := Min;
    T := T xor Tab[T shr 8];           Inc(Tab); if Tab > Max then Tab := Min;
    A := T xor D xor K;
    D := C;
    C := B;
    B := T;
  until K = 8;
  repeat
    Inc(K);
    T := A;
    A := D;
    D := C;
    C := T xor B xor K;
    T := T xor Tab[T and $FF] shl 8;   Inc(Tab); if Tab > Max then Tab := Min;
    T := T xor Tab[T shr 8];           Inc(Tab); if Tab > Max then Tab := Min;
    T := T xor Tab[T and $FF] shl 8;   Inc(Tab); if Tab > Max then Tab := Min;
    T := T xor Tab[T shr 8];           Inc(Tab); if Tab > Max then Tab := Min;
    B := T;
  until K = 16;
  repeat
    Inc(K);
    T := A;
    T := T xor Tab[T and $FF] shl 8;   Inc(Tab); if Tab > Max then Tab := Min;
    T := T xor Tab[T shr 8];           Inc(Tab); if Tab > Max then Tab := Min;
    T := T xor Tab[T and $FF] shl 8;   Inc(Tab); if Tab > Max then Tab := Min;
    T := T xor Tab[T shr 8];           Inc(Tab); if Tab > Max then Tab := Min;
    A := T xor D xor K;
    D := C;
    C := B;
    B := T;
  until K = 24;
  repeat
    Inc(K);
    T := A;
    A := D;
    D := C;
    C := T xor B xor K;
    T := T xor Tab[T and $FF] shl 8;   Inc(Tab); if Tab > Max then Tab := Min;
    T := T xor Tab[T shr 8];           Inc(Tab); if Tab > Max then Tab := Min;
    T := T xor Tab[T and $FF] shl 8;   Inc(Tab); if Tab > Max then Tab := Min;
    T := T xor Tab[T shr 8];           Inc(Tab); if Tab > Max then Tab := Min;
    B := T;
  until K = 32;
  PWordArray(Data)[0] := Swap(A);
  PWordArray(Data)[1] := Swap(B);
  PWordArray(Data)[2] := Swap(C);
  PWordArray(Data)[3] := Swap(D);
end;

procedure TCipher_Skipjack.Decode(Data: Pointer);
var
  Tab,Max: PSkipjackTab;
  Min: PChar;
  K,T,A,B,C,D: Integer;
begin
  Min := User;
  Max := Pointer(Min + 9 * 256);
  Tab := Pointer(Min + 7 * 256);
  A   := Swap(PWordArray(Data)[0]); {holds as Integer, Compiler make faster Code}
  B   := Swap(PWordArray(Data)[1]);
  C   := Swap(PWordArray(Data)[2]);
  D   := Swap(PWordArray(Data)[3]);
  K   := 32;
  repeat
    T := B;
    T := T xor Tab[T shr 8];           Dec(Tab); if Tab < Min then Tab := Max;
    T := T xor Tab[T and $FF] shl 8;   Dec(Tab); if Tab < Min then Tab := Max;
    T := T xor Tab[T shr 8];           Dec(Tab); if Tab < Min then Tab := Max;
    T := T xor Tab[T and $FF] shl 8;   Dec(Tab); if Tab < Min then Tab := Max;
    B := T xor C xor K;
    C := D;
    D := A;
    A := T;
    Dec(K);
  until K = 24;
  repeat
    T := B;
    B := C;
    C := D;
    D := T xor A xor K;
    T := T xor Tab[T shr 8];           Dec(Tab); if Tab < Min then Tab := Max;
    T := T xor Tab[T and $FF] shl 8;   Dec(Tab); if Tab < Min then Tab := Max;
    T := T xor Tab[T shr 8];           Dec(Tab); if Tab < Min then Tab := Max;
    T := T xor Tab[T and $FF] shl 8;   Dec(Tab); if Tab < Min then Tab := Max;
    A := T;
    Dec(K);
  until K = 16;
  repeat
    T := B;
    T := T xor Tab[T shr 8];           Dec(Tab); if Tab < Min then Tab := Max;
    T := T xor Tab[T and $FF] shl 8;   Dec(Tab); if Tab < Min then Tab := Max;
    T := T xor Tab[T shr 8];           Dec(Tab); if Tab < Min then Tab := Max;
    T := T xor Tab[T and $FF] shl 8;   Dec(Tab); if Tab < Min then Tab := Max;
    B := C xor T xor K;
    C := D;
    D := A;
    A := T;
    Dec(K);
  until K = 8;
  repeat
    T := B;
    B := C;
    C := D;
    D := T xor A xor K;
    T := T xor Tab[T shr 8];           Dec(Tab); if Tab < Min then Tab := Max;
    T := T xor Tab[T and $FF] shl 8;   Dec(Tab); if Tab < Min then Tab := Max;
    T := T xor Tab[T shr 8];           Dec(Tab); if Tab < Min then Tab := Max;
    T := T xor Tab[T and $FF] shl 8;   Dec(Tab); if Tab < Min then Tab := Max;
    A := T;
    Dec(K);
  until K = 0;
  PWordArray(Data)[0] := Swap(A);
  PWordArray(Data)[1] := Swap(B);
  PWordArray(Data)[2] := Swap(C);
  PWordArray(Data)[3] := Swap(D);
end;

procedure TCipher_Skipjack.Init(const Key; Size: Integer; IVector: Pointer);
var
  K: array[0..9] of Byte;
  D: PByte;
  I,J: Integer;
begin
  InitBegin(Size);
  FillChar(K, SizeOf(K), 0);
  Move(Key, K, Size);
  D := User;
  for I := 0 to 9 do
    for J := 0 to 255 do
    begin
      D^ := Skipjack_Data[J xor K[I]];
      Inc(D);
    end;
  InitEnd(IVector);
end;
{$ENDIF}
{DES}
procedure DES_Func(Data: PIntArray; Key: PInteger); register;
var
  L,R,X,Y,I: LongWord;
begin
  L := SwapInteger(Data[0]);
  R := SwapInteger(Data[1]);

  X := (L shr  4 xor R) and $0F0F0F0F; R := R xor X; L := L xor X shl  4;
  X := (L shr 16 xor R) and $0000FFFF; R := R xor X; L := L xor X shl 16;
  X := (R shr  2 xor L) and $33333333; L := L xor X; R := R xor X shl  2;
  X := (R shr  8 xor L) and $00FF00FF; L := L xor X; R := R xor X shl  8;

  R := R shl 1 or R shr 31;
  X := (L xor R) and $AAAAAAAA;
  R := R xor X;
  L := L xor X;
  L := L shl 1 or L shr 31;

  for I := 0 to 7 do
  begin
    X := (R shl 28 or R shr 4) xor Key^; Inc(Key);
    Y := R xor Key^;                     Inc(Key);
    L := L xor (DES_Data[0, X        and $3F] or DES_Data[1, X shr  8 and $3F] or
                DES_Data[2, X shr 16 and $3F] or DES_Data[3, X shr 24 and $3F] or
                DES_Data[4, Y        and $3F] or DES_Data[5, Y shr  8 and $3F] or
                DES_Data[6, Y shr 16 and $3F] or DES_Data[7, Y shr 24 and $3F]);

    X := (L shl 28 or L shr 4) xor Key^; Inc(Key);
    Y := L xor Key^;                     Inc(Key);
    R := R xor (DES_Data[0, X        and $3F] or DES_Data[1, X shr  8 and $3F] or
                DES_Data[2, X shr 16 and $3F] or DES_Data[3, X shr 24 and $3F] or
                DES_Data[4, Y        and $3F] or DES_Data[5, Y shr  8 and $3F] or
                DES_Data[6, Y shr 16 and $3F] or DES_Data[7, Y shr 24 and $3F]);
  end;

  R := R shl 31 or R shr 1;
  X := (L xor R) and $AAAAAAAA;
  R := R xor X;
  L := L xor X;
  L := L shl 31 or L shr 1;

  X := (L shr  8 xor R) and $00FF00FF; R := R xor X; L := L xor X shl  8;
  X := (L shr  2 xor R) and $33333333; R := R xor X; L := L xor X shl  2;
  X := (R shr 16 xor L) and $0000FFFF; L := L xor X; R := R xor X shl 16;
  X := (R shr  4 xor L) and $0F0F0F0F; L := L xor X; R := R xor X shl  4;

  Data[0] := SwapInteger(R);
  Data[1] := SwapInteger(L);
end;

class procedure TCipher_1DES.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 8;
  AUserSize := 32 * 4 * 2;
end;

class function TCipher_1DES.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0ADh,069h,042h,0BBh,0F6h,068h,020h,04Dh
         DB    053h,0CDh,0C7h,062h,013h,093h,098h,0C0h
         DB    030h,00Dh,085h,00Bh,0E2h,0AAh,072h,009h
         DB    06Fh,0DBh,05Fh,08Eh,0D3h,0E4h,0CFh,08Ah
end;

procedure TCipher_1DES.Encode(Data: Pointer);
begin
  DES_Func(Data, User);
end;

procedure TCipher_1DES.Decode(Data: Pointer);
begin
  DES_Func(Data, @PIntArray(User)[32]);
end;

procedure TCipher_1DES.MakeKey(const Data: array of Byte; Key: PInteger; Reverse: Boolean);
const
  ROT: array[0..15] of Byte = (1,2,4,6,8,10,12,14,15,17,19,21,23,25,27,28);
var
  I,J,L,M,N: LongWord;
  PC_M, PC_R: array[0..55] of Byte;
  K: array[0..31] of LongWord;
begin
  FillChar(K, SizeOf(K), 0);
  for I := 0 to 55 do
    if Data[DES_PC1[I] shr 3] and ($80 shr (DES_PC1[I] and $07)) <> 0 then PC_M[I] := 1
      else PC_M[I] := 0;
  for I := 0 to 15 do
  begin
    if Reverse then M := (15 - I) shl 1 else M := I shl 1;
    N := M + 1;
    for J := 0 to 27 do
    begin
      L := J + ROT[I];
      if L < 28 then PC_R[J] := PC_M[L] else PC_R[J] := PC_M[L - 28];
    end;
    for J := 28 to 55 do
    begin
      L := J + ROT[I];
      if L < 56 then PC_R[J] := PC_M[L] else PC_R[J] := PC_M[L - 28];
    end;
    L := $1000000;
    for J := 0 to 23 do
    begin
      L := L shr 1;
      if PC_R[DES_PC2[J     ]] <> 0 then K[M] := K[M] or L;
      if PC_R[DES_PC2[J + 24]] <> 0 then K[N] := K[N] or L;
    end;
  end;
  for I := 0 to 15 do
  begin
    M := I shl 1; N := M + 1;
    Key^ := K[M] and $00FC0000 shl  6 or
            K[M] and $00000FC0 shl 10 or
            K[N] and $00FC0000 shr 10 or
            K[N] and $00000FC0 shr  6;
    Inc(Key);
    Key^ := K[M] and $0003F000 shl 12 or
            K[M] and $0000003F shl 16 or
            K[N] and $0003F000 shr  4 or
            K[N] and $0000003F;
    Inc(Key);
  end;
end;

procedure TCipher_1DES.Init(const Key; Size: Integer; IVector: Pointer);
var
  K: array[0..7] of Byte;
begin
  InitBegin(Size);
  FillChar(K, SizeOf(K), 0);
  Move(Key, K, Size);
  MakeKey(K, User, False);
  MakeKey(K, @PIntArray(User)[32], True);
  FillChar(K, SizeOf(K), 0);
  InitEnd(IVector);
end;

class procedure TCipher_2DES.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 16;
  AUserSize := 32 * 4 * 2 * 2;
end;

class function TCipher_2DES.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    066h,05Ch,079h,027h,0E9h,01Ch,08Bh,0A0h
         DB    0A9h,0E4h,099h,05Ah,015h,08Ch,0BDh,046h
         DB    05Ch,09Ch,075h,091h,03Ch,038h,006h,09Dh
         DB    075h,0B4h,07Eh,068h,0E9h,047h,0FDh,0ABh
end;

procedure TCipher_2DES.Encode(Data: Pointer);
begin
  DES_Func(Data, User);
  DES_Func(Data, @PIntArray(User)[32]);
  DES_Func(Data, User);
end;

procedure TCipher_2DES.Decode(Data: Pointer);
begin
  DES_Func(Data, @PIntArray(User)[64]);
  DES_Func(Data, @PIntArray(User)[96]);
  DES_Func(Data, @PIntArray(User)[64]);
end;

procedure TCipher_2DES.Init(const Key; Size: Integer; IVector: Pointer);
var
  K: array[0..15] of Byte;
  P: PInteger;
begin
  InitBegin(Size);
  FillChar(K, SizeOf(K), 0);
  Move(Key, K, Size);
  P := User;
  MakeKey(K[0], P, False); Inc(P, 32);
  MakeKey(K[8], P, True);  Inc(P, 32);
  MakeKey(K[0], P, True);  Inc(P, 32);
  MakeKey(K[8], P, False);
  FillChar(K, SizeOf(K), 0);
  InitEnd(IVector);
end;

class procedure TCipher_3DES.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 24;
  AUserSize := 32 * 4 * 2 * 3;
end;

class function TCipher_3DES.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    007h,04Ch,014h,0F3h,0E2h,02Eh,008h,0D9h
         DB    064h,0BFh,06Fh,082h,0B5h,0DFh,0F0h,0A2h
         DB    02Fh,02Dh,03Bh,0DBh,017h,0DBh,025h,0B6h
         DB    0B5h,01Eh,0FAh,071h,037h,02Fh,0D1h,072h
end;

procedure TCipher_3DES.Encode(Data: Pointer);
begin
  DES_Func(Data, User);
  DES_Func(Data, @PIntArray(User)[32]);
  DES_Func(Data, @PIntArray(User)[64]);
end;

procedure TCipher_3DES.Decode(Data: Pointer);
begin
  DES_Func(Data, @PIntArray(User)[96]);
  DES_Func(Data, @PIntArray(User)[128]);
  DES_Func(Data, @PIntArray(User)[160]);
end;

procedure TCipher_3DES.Init(const Key; Size: Integer; IVector: Pointer);
var
  K: array[0..23] of Byte;
  P: PInteger;
begin
  InitBegin(Size);
  FillChar(K, SizeOf(K), 0);
  Move(Key, K, Size);
  P := User;
  MakeKey(K[ 0], P, False); Inc(P, 32);
  MakeKey(K[ 8], P, True);  Inc(P, 32);
  MakeKey(K[16], P, False); Inc(P, 32);
  MakeKey(K[16], P, True);  Inc(P, 32);
  MakeKey(K[ 8], P, False); Inc(P, 32);
  MakeKey(K[ 0], P, True);
  FillChar(K, SizeOf(K), 0);
  InitEnd(IVector);
end;

class procedure TCipher_2DDES.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  inherited GetContext(ABufSize, AKeySize, AUserSize);
  ABufSize := 16;
end;

class function TCipher_2DDES.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    093h,06Ch,0F6h,043h,0C6h,0A7h,07Fh,0EDh
         DB    04Dh,0B4h,070h,04Ah,0E2h,0A6h,006h,08Bh
         DB    075h,013h,019h,0AFh,0E1h,082h,0EDh,035h
         DB    04Eh,013h,0F6h,088h,0A4h,06Bh,033h,026h
end;

procedure TCipher_2DDES.Encode(Data: Pointer);
var
  T: Integer;
begin
  DES_Func(Data, User);
  DES_Func(@PIntArray(Data)[2], User);
  T := PIntArray(Data)[1]; PIntArray(Data)[1] := PIntArray(Data)[2]; PIntArray(Data)[2] := T;
  DES_Func(Data, @PIntArray(User)[32]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[32]);
  T := PIntArray(Data)[1]; PIntArray(Data)[1] := PIntArray(Data)[2]; PIntArray(Data)[2] := T;
  DES_Func(Data, User);
  DES_Func(@PIntArray(Data)[2], User);
end;

procedure TCipher_2DDES.Decode(Data: Pointer);
var
  T: Integer;
begin
  DES_Func(Data, @PIntArray(User)[64]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[64]);
  T := PIntArray(Data)[1]; PIntArray(Data)[1] := PIntArray(Data)[2]; PIntArray(Data)[2] := T;
  DES_Func(Data, @PIntArray(User)[96]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[96]);
  T := PIntArray(Data)[1]; PIntArray(Data)[1] := PIntArray(Data)[2]; PIntArray(Data)[2] := T;
  DES_Func(Data,  @PIntArray(User)[64]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[64]);
end;

class procedure TCipher_3DDES.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  inherited GetContext(ABufSize, AKeySize, AUserSize);
  ABufSize := 16;
end;

class function TCipher_3DDES.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    02Fh,05Ah,05Eh,0D4h,05Eh,08Ah,0AAh,04Eh
         DB    0D2h,066h,059h,048h,01Dh,0E1h,095h,094h
         DB    02Ah,09Fh,0CCh,01Fh,04Dh,0E6h,014h,0F0h
         DB    050h,004h,003h,064h,066h,09Ah,077h,08Eh
end;

procedure TCipher_3DDES.Encode(Data: Pointer);
var
  T: Integer;
begin
  DES_Func(Data, User);
  DES_Func(@PIntArray(Data)[2], User);
  T := PIntArray(Data)[1]; PIntArray(Data)[1] := PIntArray(Data)[2]; PIntArray(Data)[2] := T;
  DES_Func(Data, @PIntArray(User)[32]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[32]);
  T := PIntArray(Data)[1]; PIntArray(Data)[1] := PIntArray(Data)[2]; PIntArray(Data)[2] := T;
  DES_Func(Data,  @PIntArray(User)[64]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[64]);
end;

procedure TCipher_3DDES.Decode(Data: Pointer);
var
  T: Integer;
begin
  DES_Func(Data, @PIntArray(User)[96]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[96]);
  T := PIntArray(Data)[1]; PIntArray(Data)[1] := PIntArray(Data)[2]; PIntArray(Data)[2] := T;
  DES_Func(Data, @PIntArray(User)[128]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[128]);
  T := PIntArray(Data)[1]; PIntArray(Data)[1] := PIntArray(Data)[2]; PIntArray(Data)[2] := T;
  DES_Func(Data,  @PIntArray(User)[160]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[160]);
end;

class procedure TCipher_3TDES.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  inherited GetContext(ABufSize, AKeySize, AUserSize);
  ABufSize := 24;
end;

class function TCipher_3TDES.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    00Bh,012h,0E4h,08Bh,0D9h,0CDh,008h,0BFh
         DB    0CAh,0AEh,03Eh,05Fh,0F6h,0FEh,013h,0CDh
         DB    03Fh,070h,06Eh,0CDh,053h,056h,03Fh,05Ah
         DB    080h,00Fh,01Bh,01Eh,0FBh,09Ah,057h,096h
end;

procedure TCipher_3TDES.Encode(Data: Pointer);
var
  T: Integer;
begin
  DES_Func(@PIntArray(Data)[0], User);
  DES_Func(@PIntArray(Data)[2], User);
  DES_Func(@PIntArray(Data)[4], User);
  T := PIntArray(Data)[1]; PIntArray(Data)[1] := PIntArray(Data)[2]; PIntArray(Data)[2] := T;
  T := PIntArray(Data)[3]; PIntArray(Data)[3] := PIntArray(Data)[4]; PIntArray(Data)[3] := T;
  DES_Func(@PIntArray(Data)[0], @PIntArray(User)[32]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[32]);
  DES_Func(@PIntArray(Data)[4], @PIntArray(User)[32]);
  T := PIntArray(Data)[1]; PIntArray(Data)[1] := PIntArray(Data)[2]; PIntArray(Data)[2] := T;
  T := PIntArray(Data)[3]; PIntArray(Data)[3] := PIntArray(Data)[4]; PIntArray(Data)[3] := T;
  DES_Func(@PIntArray(Data)[0], @PIntArray(User)[64]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[64]);
  DES_Func(@PIntArray(Data)[4], @PIntArray(User)[64]);
end;

procedure TCipher_3TDES.Decode(Data: Pointer);
var
  T: Integer;
begin
  DES_Func(@PIntArray(Data)[0], @PIntArray(User)[96]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[96]);
  DES_Func(@PIntArray(Data)[4], @PIntArray(User)[96]);
  T := PIntArray(Data)[1]; PIntArray(Data)[1] := PIntArray(Data)[2]; PIntArray(Data)[2] := T;
  T := PIntArray(Data)[3]; PIntArray(Data)[3] := PIntArray(Data)[4]; PIntArray(Data)[3] := T;
  DES_Func(@PIntArray(Data)[0], @PIntArray(User)[128]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[128]);
  DES_Func(@PIntArray(Data)[4], @PIntArray(User)[128]);
  T := PIntArray(Data)[1]; PIntArray(Data)[1] := PIntArray(Data)[2]; PIntArray(Data)[2] := T;
  T := PIntArray(Data)[3]; PIntArray(Data)[3] := PIntArray(Data)[4]; PIntArray(Data)[3] := T;
  DES_Func(@PIntArray(Data)[0], @PIntArray(User)[160]);
  DES_Func(@PIntArray(Data)[2], @PIntArray(User)[160]);
  DES_Func(@PIntArray(Data)[4], @PIntArray(User)[160]);
end;

class procedure TCipher_DESX.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 8 * 2; {Key 8 bytes and Whitening 8 bytes}
  AUserSize := 32 * 4 * 2 + 2 * 4 * 2;
end;

class function TCipher_DESX.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    039h,049h,0ECh,05Bh,065h,01Bh,01Eh,01Fh
         DB    08Eh,085h,0FDh,0D9h,06Fh,0D1h,0D5h,0FBh
         DB    015h,003h,009h,027h,0B0h,069h,067h,0F9h
         DB    0A6h,0C8h,01Ch,02Ah,09Fh,08Eh,04Fh,06Eh
end;

procedure TCipher_DESX.Encode(Data: Pointer);
begin
  PIntArray(Data)[0] := PIntArray(Data)[0] xor PIntArray(User)[64];
  PIntArray(Data)[1] := PIntArray(Data)[1] xor PIntArray(User)[65];
  DES_Func(Data, User);
  PIntArray(Data)[0] := PIntArray(Data)[0] xor PIntArray(User)[66];
  PIntArray(Data)[1] := PIntArray(Data)[1] xor PIntArray(User)[67];
end;

procedure TCipher_DESX.Decode(Data: Pointer);
begin
  PIntArray(Data)[0] := PIntArray(Data)[0] xor PIntArray(User)[66];
  PIntArray(Data)[1] := PIntArray(Data)[1] xor PIntArray(User)[67];
  DES_Func(Data, @PIntArray(User)[32]);
  PIntArray(Data)[0] := PIntArray(Data)[0] xor PIntArray(User)[64];
  PIntArray(Data)[1] := PIntArray(Data)[1] xor PIntArray(User)[65];
end;

procedure TCipher_DESX.Init(const Key; Size: Integer; IVector: Pointer);
var
  K: array[0..15] of Byte;
begin
  FillChar(K, SizeOf(K), 0);
  if Size > SizeOf(K) then Size := SizeOf(K);
  Move(Key, K, Size);
  InitNew(K, K[8], 8, IVector);
  FillChar(K, SizeOf(K), 0);
end;

procedure TCipher_DESX.InitNew(const Key, Whitening; Size: Integer; IVector: Pointer);
const
  Clorox: array[0..255] of Byte = (
   189, 86,234,242,162,241,172, 42,176,147,209,156, 27, 51,253,208,
    48,  4,182,220,125,223, 50, 75,247,203, 69,155, 49,187, 33, 90,
    65,159,225,217, 74, 77,158,218,160,104, 44,195, 39, 95,128, 54,
    62,238,251,149, 26,254,206,168, 52,169, 19,240,166, 63,216, 12,
   120, 36,175, 35, 82,193,103, 23,245,102,144,231,232,  7,184, 96,
    72,230, 30, 83,243,146,164,114,140,  8, 21,110,134,  0,132,250,
   244,127,138, 66, 25,246,219,205, 20,141, 80, 18,186, 60,  6, 78,
   236,179, 53, 17,161,136,142, 43,148,153,183,113,116,211,228,191,
    58,222,150, 14,188, 10,237,119,252, 55,107,  3,121,137, 98,198,
   215,192,210,124,106,139, 34,163, 91,  5, 93,  2,117,213, 97,227,
    24,143, 85, 81,173, 31, 11, 94,133,229,194, 87, 99,202, 61,108,
   180,197,204,112,178,145, 89, 13, 71, 32,200, 79, 88,224,  1,226,
    22, 56,196,111, 59, 15,101, 70,190,126, 45,123,130,249, 64,181,
    29,115,248,235, 38,199,135,151, 37, 84,177, 40,170,152,157,165,
   100,109,122,212, 16,129, 68,239, 73,214,174, 46,221,118, 92, 47,
   167, 28,201,  9,105,154,131,207, 41, 57,185,233, 76,255, 67,171);

var
  K,H,W: array[0..7] of Byte;
  I: Integer;
  T: Byte;
begin
  Size := Size * 2;
  InitBegin(Size);
  Size := Size shr 1;
  FillChar(K, SizeOf(K), 0); Move(Key, K, Size);
  FillChar(H, SizeOf(H), 0); Move(Whitening, H, Size);
  MakeKey(K, @PIntArray(User)[0], False);
  MakeKey(K, @PIntArray(User)[32], True);

  Move(H, PIntArray(User)[64], 8);
  FillChar(W, SizeOf(W), 0);
  for I := 0 to 7 do
  begin
    T := W[0] xor W[1];
    Move(W[1], W[0], 7);
    W[7] := Clorox[T] xor K[I];
  end;
  for I := 0 to 7 do
  begin
    T := W[0] xor W[1];
    Move(W[1], W[0], 7);
    W[7] := Clorox[T] xor H[I];
  end;
  Move(W, PIntArray(User)[66], 8);
  InitEnd(IVector);
end;

class procedure TCipher_NewDES.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 15;
  AUserSize := 60 * 2;
end;

class function TCipher_NewDES.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    0D5h,091h,04Fh,09Ch,074h,035h,046h,0FBh
         DB    0D5h,0ADh,091h,031h,075h,014h,064h,0FEh
         DB    0A7h,079h,021h,06Ah,029h,099h,047h,089h
         DB    0D2h,00Dh,076h,00Ch,073h,09Ch,0CDh,017h
end;


procedure NewDES_Func(Data: Pointer; Key: PByte);
type
  PNewDESRec = ^TNewDESRec;
  TNewDESRec = packed record
                 A,B,C,D,E,F,G,H: Byte;
               end;
var
  I: Integer;
begin
  with PNewDESRec(Data)^ do
  begin
    for I := 0 to 7 do
    begin
      E := E xor NewDES_Data[A xor Key^]; Inc(Key);
      F := F xor NewDES_Data[B xor Key^]; Inc(Key);
      G := G xor NewDES_Data[C xor Key^]; Inc(Key);
      H := H xor NewDES_Data[D xor Key^]; Inc(Key);

      B := B xor NewDES_Data[E xor Key^]; Inc(Key);
      C := C xor NewDES_Data[F xor E];
      D := D xor NewDES_Data[G xor Key^]; Inc(Key);
      A := A xor NewDES_Data[H xor Key^]; Inc(Key);
    end;
    E := E xor NewDES_Data[A xor Key^]; Inc(Key);
    F := F xor NewDES_Data[B xor Key^]; Inc(Key);
    G := G xor NewDES_Data[C xor Key^]; Inc(Key);
    H := H xor NewDES_Data[D xor Key^];
  end;
end;

procedure TCipher_NewDES.Encode(Data: Pointer);
begin
  NewDES_Func(Data, User);
end;

procedure TCipher_NewDES.Decode(Data: Pointer);
begin
  NewDES_Func(Data, @PByteArray(User)[60]);
end;

procedure TCipher_NewDES.Init(const Key; Size: Integer; IVector: Pointer);
var
  K: array[0..14] of Byte;
  E: PByte;
  I: Integer;
begin
  InitBegin(Size);
  FillChar(K, SizeOf(K), 0);
  Move(Key, K, Size);
  E := User;
  Move(K, E^, 15); Inc(E, 15);
  Move(K, E^, 15); Inc(E, 15);
  Move(K, E^, 15); Inc(E, 15);
  Move(K, E^, 15); Inc(E, 15);
  I := 11;
  repeat
    E^ := K[I]; Inc(E); Inc(I);
    if I = 15 then I := 0;
    E^ := K[I]; Inc(E); Inc(I);
    if I = 15 then I := 0;
    E^ := K[I]; Inc(E); Inc(I);
    if I = 15 then I := 0;
    E^ := K[I]; Inc(E);
    I := (I + 9) mod 15;
    if I = 12 then Break;
    E^ := K[I]; Inc(E); Inc(I);
    E^ := K[I]; Inc(E); Inc(I);
    E^ := K[I]; Inc(E);
    I := (I + 9) mod 15;
  until False;
  FillChar(K, SizeOf(K), 0);
  InitEnd(IVector);
end;

procedure TCipher_Diamond2.SetRounds(Value: Integer);
begin
  if (Value < 5) or (Value > 15) then Value := 10;
  if Value = FRounds then Exit;
  FRounds := Value;
end;

class procedure TCipher_Diamond2.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 16;
  AKeySize := 256;
  AUserSize := 0;
end;

class function TCipher_Diamond2.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    020h,0B3h,058h,0B4h,0CEh,022h,0FBh,076h
         DB    02Dh,03Bh,093h,069h,005h,09Fh,006h,0C7h
         DB    0E4h,0F7h,0C6h,09Ch,0E1h,0DBh,055h,023h
         DB    052h,070h,0A1h,0DFh,0B1h,05Eh,0EDh,08Dh
end;

procedure TCipher_Diamond2.Encode(Data: Pointer);
var
  I,J: Integer;
  Key: PByteArray;
  D: PByteArray;
  Z: array[0..15] of Byte;
begin
  Key := FBoxE;
  D   := Data;
  for J := 0 to BufSize-1 do
  begin
    D[J] := Key[D[J]];
    Inc(PByte(Key), 256);
  end;
  for I := 0 to FRounds-2 do
  begin
    if BufSize = 8 then
    begin
      Z[ 0] := D[ 0] and   1 or D[ 1] and   2 or D[ 2] and   4 or D[ 3] and   8 or
               D[ 4] and  16 or D[ 5] and  32 or D[ 6] and  64 or D[ 7] and 128;
      Z[ 1] := D[ 1] and   1 or D[ 2] and   2 or D[ 3] and   4 or D[ 4] and   8 or
               D[ 5] and  16 or D[ 6] and  32 or D[ 7] and  64 or D[ 0] and 128;
      Z[ 2] := D[ 2] and   1 or D[ 3] and   2 or D[ 4] and   4 or D[ 5] and   8 or
               D[ 6] and  16 or D[ 7] and  32 or D[ 0] and  64 or D[ 1] and 128;
      Z[ 3] := D[ 3] and   1 or D[ 4] and   2 or D[ 5] and   4 or D[ 6] and   8 or
               D[ 7] and  16 or D[ 0] and  32 or D[ 1] and  64 or D[ 2] and 128;
      Z[ 4] := D[ 4] and   1 or D[ 5] and   2 or D[ 6] and   4 or D[ 7] and   8 or
               D[ 0] and  16 or D[ 1] and  32 or D[ 2] and  64 or D[ 3] and 128;
      Z[ 5] := D[ 5] and   1 or D[ 6] and   2 or D[ 7] and   4 or D[ 0] and   8 or
               D[ 1] and  16 or D[ 2] and  32 or D[ 3] and  64 or D[ 4] and 128;
      Z[ 6] := D[ 6] and   1 or D[ 7] and   2 or D[ 0] and   4 or D[ 1] and   8 or
               D[ 2] and  16 or D[ 3] and  32 or D[ 4] and  64 or D[ 5] and 128;
      Z[ 7] := D[ 7] and   1 or D[ 0] and   2 or D[ 1] and   4 or D[ 2] and   8 or
               D[ 3] and  16 or D[ 4] and  32 or D[ 5] and  64 or D[ 6] and 128;
    end else
    begin
      Z[ 0] := D[ 0] and   1 or D[ 1] and   2 or D[ 2] and   4 or D[ 3] and   8 or
               D[ 4] and  16 or D[ 5] and  32 or D[ 6] and  64 or D[ 7] and 128;
      Z[ 1] := D[ 1] and   1 or D[ 2] and   2 or D[ 3] and   4 or D[ 4] and   8 or
               D[ 5] and  16 or D[ 6] and  32 or D[ 7] and  64 or D[ 8] and 128;
      Z[ 2] := D[ 2] and   1 or D[ 3] and   2 or D[ 4] and   4 or D[ 5] and   8 or
               D[ 6] and  16 or D[ 7] and  32 or D[ 8] and  64 or D[ 9] and 128;
      Z[ 3] := D[ 3] and   1 or D[ 4] and   2 or D[ 5] and   4 or D[ 6] and   8 or
               D[ 7] and  16 or D[ 8] and  32 or D[ 9] and  64 or D[10] and 128;
      Z[ 4] := D[ 4] and   1 or D[ 5] and   2 or D[ 6] and   4 or D[ 7] and   8 or
               D[ 8] and  16 or D[ 9] and  32 or D[10] and  64 or D[11] and 128;
      Z[ 5] := D[ 5] and   1 or D[ 6] and   2 or D[ 7] and   4 or D[ 8] and   8 or
               D[ 9] and  16 or D[10] and  32 or D[11] and  64 or D[12] and 128;
      Z[ 6] := D[ 6] and   1 or D[ 7] and   2 or D[ 8] and   4 or D[ 9] and   8 or
               D[10] and  16 or D[11] and  32 or D[12] and  64 or D[13] and 128;
      Z[ 7] := D[ 7] and   1 or D[ 8] and   2 or D[ 9] and   4 or D[10] and   8 or
               D[11] and  16 or D[12] and  32 or D[13] and  64 or D[14] and 128;
      Z[ 8] := D[ 8] and   1 or D[ 9] and   2 or D[10] and   4 or D[11] and   8 or
               D[12] and  16 or D[13] and  32 or D[14] and  64 or D[15] and 128;
      Z[ 9] := D[ 9] and   1 or D[10] and   2 or D[11] and   4 or D[12] and   8 or
               D[13] and  16 or D[14] and  32 or D[15] and  64 or D[ 0] and 128;
      Z[10] := D[10] and   1 or D[11] and   2 or D[12] and   4 or D[13] and   8 or
               D[14] and  16 or D[15] and  32 or D[ 0] and  64 or D[ 1] and 128;
      Z[11] := D[11] and   1 or D[12] and   2 or D[13] and   4 or D[14] and   8 or
               D[15] and  16 or D[ 0] and  32 or D[ 1] and  64 or D[ 2] and 128;
      Z[12] := D[12] and   1 or D[13] and   2 or D[14] and   4 or D[15] and   8 or
               D[ 0] and  16 or D[ 1] and  32 or D[ 2] and  64 or D[ 3] and 128;
      Z[13] := D[13] and   1 or D[14] and   2 or D[15] and   4 or D[ 0] and   8 or
               D[ 1] and  16 or D[ 2] and  32 or D[ 3] and  64 or D[ 4] and 128;
      Z[14] := D[14] and   1 or D[15] and   2 or D[ 0] and   4 or D[ 1] and   8 or
               D[ 2] and  16 or D[ 3] and  32 or D[ 4] and  64 or D[ 5] and 128;
      Z[15] := D[15] and   1 or D[ 0] and   2 or D[ 1] and   4 or D[ 2] and   8 or
               D[ 3] and  16 or D[ 4] and  32 or D[ 5] and  64 or D[ 6] and 128;
    end;           
    for J := 0 to BufSize-1 do
    begin
      D[J] := Key[Z[J]];
      Inc(PByte(Key), 256);
    end;
  end;
end;

procedure TCipher_Diamond2.Decode(Data: Pointer);
var
  I,J: Integer;
  Key: PByteArray;
  D: PByteArray;
  Z: array[0..15] of Byte;
begin
  D   := Data;
  Key := FBoxD;
  for J := 0 to BufSize-1 do
  begin
    D[J] := Key[D[J]];
    Inc(PByte(Key), 256);
  end;
  for I := 0 to FRounds-2 do
  begin
    if BufSize = 8 then
    begin
      Z[ 0] := D[ 0] and   1 or D[ 7] and   2 or D[ 6] and   4 or D[ 5] and   8 or
               D[ 4] and  16 or D[ 3] and  32 or D[ 2] and  64 or D[ 1] and 128;
      Z[ 1] := D[ 1] and   1 or D[ 0] and   2 or D[ 7] and   4 or D[ 6] and   8 or
               D[ 5] and  16 or D[ 4] and  32 or D[ 3] and  64 or D[ 2] and 128;
      Z[ 2] := D[ 2] and   1 or D[ 1] and   2 or D[ 0] and   4 or D[ 7] and   8 or
               D[ 6] and  16 or D[ 5] and  32 or D[ 4] and  64 or D[ 3] and 128;
      Z[ 3] := D[ 3] and   1 or D[ 2] and   2 or D[ 1] and   4 or D[ 0] and   8 or
               D[ 7] and  16 or D[ 6] and  32 or D[ 5] and  64 or D[ 4] and 128;
      Z[ 4] := D[ 4] and   1 or D[ 3] and   2 or D[ 2] and   4 or D[ 1] and   8 or
               D[ 0] and  16 or D[ 7] and  32 or D[ 6] and  64 or D[ 5] and 128;
      Z[ 5] := D[ 5] and   1 or D[ 4] and   2 or D[ 3] and   4 or D[ 2] and   8 or
               D[ 1] and  16 or D[ 0] and  32 or D[ 7] and  64 or D[ 6] and 128;
      Z[ 6] := D[ 6] and   1 or D[ 5] and   2 or D[ 4] and   4 or D[ 3] and   8 or
               D[ 2] and  16 or D[ 1] and  32 or D[ 0] and  64 or D[ 7] and 128;
      Z[ 7] := D[ 7] and   1 or D[ 6] and   2 or D[ 5] and   4 or D[ 4] and   8 or
               D[ 3] and  16 or D[ 2] and  32 or D[ 1] and  64 or D[ 0] and 128;
    end else
    begin
      Z[ 0] := D[ 0] and   1 or D[15] and   2 or D[14] and   4 or D[13] and   8 or
               D[12] and  16 or D[11] and  32 or D[10] and  64 or D[ 9] and 128;
      Z[ 1] := D[ 1] and   1 or D[ 0] and   2 or D[15] and   4 or D[14] and   8 or
               D[13] and  16 or D[12] and  32 or D[11] and  64 or D[10] and 128;
      Z[ 2] := D[ 2] and   1 or D[ 1] and   2 or D[ 0] and   4 or D[15] and   8 or
               D[14] and  16 or D[13] and  32 or D[12] and  64 or D[11] and 128;
      Z[ 3] := D[ 3] and   1 or D[ 2] and   2 or D[ 1] and   4 or D[ 0] and   8 or
               D[15] and  16 or D[14] and  32 or D[13] and  64 or D[12] and 128;
      Z[ 4] := D[ 4] and   1 or D[ 3] and   2 or D[ 2] and   4 or D[ 1] and   8 or
               D[ 0] and  16 or D[15] and  32 or D[14] and  64 or D[13] and 128;
      Z[ 5] := D[ 5] and   1 or D[ 4] and   2 or D[ 3] and   4 or D[ 2] and   8 or
               D[ 1] and  16 or D[ 0] and  32 or D[15] and  64 or D[14] and 128;
      Z[ 6] := D[ 6] and   1 or D[ 5] and   2 or D[ 4] and   4 or D[ 3] and   8 or
               D[ 2] and  16 or D[ 1] and  32 or D[ 0] and  64 or D[15] and 128;
      Z[ 7] := D[ 7] and   1 or D[ 6] and   2 or D[ 5] and   4 or D[ 4] and   8 or
               D[ 3] and  16 or D[ 2] and  32 or D[ 1] and  64 or D[ 0] and 128;
      Z[ 8] := D[ 8] and   1 or D[ 7] and   2 or D[ 6] and   4 or D[ 5] and   8 or
               D[ 4] and  16 or D[ 3] and  32 or D[ 2] and  64 or D[ 1] and 128;
      Z[ 9] := D[ 9] and   1 or D[ 8] and   2 or D[ 7] and   4 or D[ 6] and   8 or
               D[ 5] and  16 or D[ 4] and  32 or D[ 3] and  64 or D[ 2] and 128;
      Z[10] := D[10] and   1 or D[ 9] and   2 or D[ 8] and   4 or D[ 7] and   8 or
               D[ 6] and  16 or D[ 5] and  32 or D[ 4] and  64 or D[ 3] and 128;
      Z[11] := D[11] and   1 or D[10] and   2 or D[ 9] and   4 or D[ 8] and   8 or
               D[ 7] and  16 or D[ 6] and  32 or D[ 5] and  64 or D[ 4] and 128;
      Z[12] := D[12] and   1 or D[11] and   2 or D[10] and   4 or D[ 9] and   8 or
               D[ 8] and  16 or D[ 7] and  32 or D[ 6] and  64 or D[ 5] and 128;
      Z[13] := D[13] and   1 or D[12] and   2 or D[11] and   4 or D[10] and   8 or
               D[ 9] and  16 or D[ 8] and  32 or D[ 7] and  64 or D[ 6] and 128;
      Z[14] := D[14] and   1 or D[13] and   2 or D[12] and   4 or D[11] and   8 or
               D[10] and  16 or D[ 9] and  32 or D[ 8] and  64 or D[ 7] and 128;
      Z[15] := D[15] and   1 or D[14] and   2 or D[13] and   4 or D[12] and   8 or
               D[11] and  16 or D[10] and  32 or D[ 9] and  64 or D[ 8] and 128;
    end;
    for J := 0 to BufSize-1 do
    begin
      D[J] := Key[Z[J]];
      Inc(PByte(Key), 256);
    end;
  end;
end;

procedure TCipher_Diamond2.Init(const Key; Size: Integer; IVector: Pointer);
var
  KI,Accum: Integer;
  Prev: PByteArray;

  function KeyRand(const Max: Byte): Byte;
  var
    I, M: Integer;
    B: Byte;
  begin
    Result := 0;
    if Max = 0 then Exit;
    M := 0;
    I := Max;
    while I > 0 do
    begin
      M := M shl 1 or 1;
      I := I shr 1;
    end;
    I := 0;
    repeat
      if Prev <> nil then B := Prev[TByteArray(Key)[KI]]
        else B := TByteArray(Key)[KI];
      Accum := CRC32(Accum, @B, 1);
      Inc(KI);
      if KI >= Size then
      begin
        KI := 0;
        B := Size       and $FF; Accum := CRC32(Accum, @B, 1);
        B := Size shr 8 and $FF; Accum := CRC32(Accum, @B, 1);
      end;
      Result := Accum and M;
      Inc(I);
      if (I > 97) and (Result > Max) then Dec(Result, Max);
    until Result <= Max;
  end;

var
  I,J: Integer;
  P,M,N: Byte;
  D,E: PByteArray;
  F: array[0..255] of Boolean;
begin
  SetRounds(FRounds);
  InitBegin(Size);
  ReallocMem(FBoxE, FRounds * BufSize * 256);
  ReallocMem(FBoxD, FRounds * BufSize * 256);
  Accum := Integer($FFFFFFFF);
  Prev  := nil;
  KI := 0;
  E  := FBoxE;
  for I := 0 to FRounds-1 do
  begin
    for J := 0 to BufSize-1 do
    begin
      FillChar(F, SizeOf(F), False);
      for N := 255 downto 0 do
      begin
        M := KeyRand(N);
        P := 0;
        while F[P] do Inc(P);
        while M > 0 do
        begin
          Dec(M);
          Inc(P);
          while F[P] do Inc(P);
        end;
        E[P] := N;
        F[P] := True;
      end;
      Prev := E;
      Inc(PByte(E), 256);
    end;
  end;
  E := FBoxE;
  D := FBoxD;
  Inc(PByte(D), FRounds * BufSize * 256);
  for I := 0 to FRounds -1 do
  begin
    Dec(PByte(D), BufSize * 256);
    for J := 0 to BufSize -1 do
    begin
      for N := 0 to 255 do
      begin
        D[PByte(E)^] := N;
        Inc(PByte(E));
      end;
      Inc(PByte(D), 256);
    end;
    Dec(PByte(D), BufSize * 256);
  end;
  InitEnd(IVector);
end;

procedure TCipher_Diamond2.Protect;
begin
  inherited Protect;
  ReallocMem(FBoxE, 0);
  ReallocMem(FBoxD, 0);
end;

procedure TCipher_Diamond2Lite.SetRounds(Value: Integer);
begin
  if (Value < 4) or (Value > 30) then Value := 10;
  if Value = FRounds then Exit;
  FRounds := Value;
end;

class procedure TCipher_Diamond2Lite.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 8;
  AKeySize := 256;
  AUserSize := 0;
end;

class function TCipher_Diamond2Lite.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    06Bh,02Dh,03Dh,0D1h,0F2h,0B1h,029h,0FFh
         DB    055h,0C8h,081h,0A9h,0A5h,04Ch,027h,07Fh
         DB    08Eh,0ABh,089h,071h,034h,0DCh,0D1h,0D7h
         DB    09Bh,02Dh,01Fh,0CEh,09Bh,043h,0C1h,045h
end;

type
  PSapphireKey = ^TSapphireKey;
  TSapphireKey = packed record
                   Rotor: Byte;
                   Ratchet: Byte;
                   Avalanche: Byte;
                   Plain: Byte;
                   Cipher: Byte;
                   Cards: array[0..255] of Byte;
                 end;

class procedure TCipher_Sapphire.GetContext(var ABufSize, AKeySize, AUserSize: Integer);
begin
  ABufSize := 32;
  AKeySize := 1024;
  AUserSize := SizeOf(TSapphireKey) * 2;
end;

class function TCipher_Sapphire.TestVector: Pointer;
asm
         MOV   EAX,OFFSET @Vector
         RET
@Vector: DB    07Ch,082h,028h,0B7h,039h,045h,043h,09Dh
         DB    044h,0E0h,09Ch,019h,083h,0D0h,039h,0ACh
         DB    0B9h,0B7h,075h,018h,02Bh,05Dh,0F3h,06Ah
         DB    0B2h,02Dh,024h,007h,05Fh,041h,0CDh,073h
end;

procedure TCipher_Sapphire.Encode(Data: Pointer);
var
  I,C: Integer;
  T: Byte;
begin
  with PSapphireKey(User)^ do
  begin
    C := Cipher;
    for I := 0 to 31 do
    begin
      Inc(Ratchet, Cards[Rotor]);
      Inc(Rotor);
      T := Cards[C];
      Cards[C] := Cards[Ratchet];
      Cards[Ratchet] := Cards[Plain];
      Cards[Plain] := Cards[Rotor];
      Cards[Rotor] := T;
      Inc(Avalanche, Cards[T]);
      T := Cards[Plain] + Cards[C] + Cards[Avalanche];
      Plain := PByte(Data)^;
      C := PByte(Data)^ xor Cards[Cards[T]] xor
                            Cards[(Cards[Ratchet] + Cards[Rotor]) and $FF];
      PByte(Data)^ := C;
      Inc(PByte(Data));
    end;
    Cipher := C;
  end;
end;

procedure TCipher_Sapphire.Decode(Data: Pointer);
var
  I,C: Integer;
  T: Byte;
begin
  with PSapphireKey(User)^ do
  begin
    C := Cipher;
    for I := 0 to 31 do
    begin
      Inc(Ratchet, Cards[Rotor]);
      Inc(Rotor);
      T := Cards[C];
      Cards[C] := Cards[Ratchet];
      Cards[Ratchet] := Cards[Plain];
      Cards[Plain] := Cards[Rotor];
      Cards[Rotor] := T;
      Inc(Avalanche, Cards[T]);
      T := Cards[Plain] + Cards[C] + Cards[Avalanche];
      C := PByte(Data)^;
      Plain := C xor Cards[Cards[T]] xor Cards[(Cards[Ratchet] + Cards[Rotor]) and $FF];
      PByte(Data)^ := Plain;
      Inc(PByte(Data));
    end;
    Cipher := C;
  end;
end;

procedure TCipher_Sapphire.Init(const Key; Size: Integer; IVector: Pointer);
var
  Sum: Byte;
  P: Integer;

  function KeyRand(const Max: Integer): Byte;
  var
    I, M: Integer;
  begin
    Result := 0;
    if Max = 0 then Exit;
    I := 0;
    M := 1;
    while M < Max do M := M shl 1 or 1;
    repeat
      Inc(Sum, TByteArray(Key)[P]);
      Inc(P);
      if P >= Size then
      begin
        P := 0;
        Inc(Sum, Size);
      end;
      Result := M and Sum;
      Inc(I);
      if I > 11 then Result := Result mod Max;
    until Result <= Max;
  end;

var
  I,S,T: Integer;
begin
  InitBegin(Size);
  with PSapphireKey(User)^ do
    if Size <= 0 then
    begin
      Rotor := 1;
      Ratchet := 3;
      Avalanche := 5;
      Plain := 7;
      Cipher := 11;
      for I := 0 to 255 do Cards[I] := 255 - I;
    end else
    begin
      for I := 0 to 255 do Cards[I] := I;
      P   := 0;
      Sum := 0;
      for I := 255 downto 1 do
      begin
        S := KeyRand(I);
        T := Cards[I];
        Cards[I] := Cards[S];
        Cards[S] := T;
      end;
      Rotor := Cards[1];
      Ratchet := Cards[3];
      Avalanche := Cards[5];
      Plain := Cards[7];
      Cipher := Cards[Sum];
    end;
  InitEnd(IVector);
  Move(User^, Pointer(PChar(User) + SizeOf(TSapphireKey))^, SizeOf(TSapphireKey));
end;

procedure TCipher_Sapphire.Done;
begin
  inherited Done;
  Move(Pointer(PChar(User) + SizeOf(TSapphireKey))^, User^, SizeOf(TSapphireKey));
end;

initialization
{$IFNDEF ManualRegisterClasses}
  RegisterCipher(TCipher_Cast128, 'Cast 128', '');
  RegisterCipher(TCipher_Cast256, 'Cast 256', 'Patented');
  RegisterCipher(TCipher_1DES, 'DES Single 8byte', '');
  RegisterCipher(TCipher_2DES, 'DES Double 8byte', '');
  RegisterCipher(TCipher_2DDES, 'DES Double 16byte', '');
  RegisterCipher(TCipher_3DES, 'DES Triple 8byte', '');
  RegisterCipher(TCipher_3DDES, 'DES Triple 16byte', '');
  RegisterCipher(TCipher_3TDES, 'DES Triple 24byte', '');
  RegisterCipher(TCipher_DESX, 'DESX', 'Copyright by RSA');
  RegisterCipher(TCipher_Diamond2, 'Diamond II', '');
  RegisterCipher(TCipher_Diamond2Lite, 'Diamond II Lite', '');
  RegisterCipher(TCipher_FROG, '', '');
  RegisterCipher(TCipher_Mars, '', 'Patented');
  RegisterCipher(TCipher_Misty, 'Misty 1', 'free for non-commercial');
  RegisterCipher(TCipher_NewDES, '', '');
  RegisterCipher(TCipher_RC2, '', '');
  RegisterCipher(TCipher_RC4, '', 'Patented by RSA');
  RegisterCipher(TCipher_RC5, '', 'Patented by RSA');
  RegisterCipher(TCipher_RC6, '', 'Patented by RSA');
  RegisterCipher(TCipher_Rijndael, '', '');
  RegisterCipher(TCipher_Sapphire, 'Sapphire II', '');
  {$IFDEF VER_D3H}
  RegisterCipher(TCipher_Skipjack, '', '');
  {$ENDIF}
{$ENDIF}
end.
