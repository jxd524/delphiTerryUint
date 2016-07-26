unit uJxdHashCalc;

interface

{$DEFINE MD5}

uses
  SysUtils,
  Classes,
  {$IFDEF MD5}
  MD5
  {$ELSE}
  MD4
  {$ENDIF}
  ;

type
  {$IFDEF MD5}
  TxdHash = TMD5Digest;
  TxdHashContext = MD5Context;
  {$ELSE}
  TxdHash = TMD4Digest;
  TxdHashContext = TMD4Context;
  {$ENDIF}

const
  CtHashSize = 16;
  CtEmptyHash: TxdHash = (A: 0; B: 0; C: 0; D: 0 );

function  HashBuffer(const Buffer: PByte; const Size: Integer): TxdHash;
function  HashCompare(const D1, D2: TxdHash): Boolean;
function  HashToStr(const D: TxdHash): string;
function  StrToHash(const AStrHash: string; var AHash: TxdHash): Boolean;
procedure EmptyHash(var AHash: TxdHash);
function  IsEmptyHash(const AHash: TxdHash): Boolean;

procedure HashInit(var Context: TxdHashContext);
procedure HashUpdate(var Context: TxdHashContext; const Buffer: PByte; const Len: Integer);
procedure HashFinal(var Context: TxdHashContext; var Digest: TxdHash);

function HashString(AStr: string): TxdHash;
function HashFile(AFileName: string): TxdHash;

implementation

function HashString(AStr: string): TxdHash;
begin
  {$IFDEF MD5}
  Result := MD5String(Astr);
  {$ELSE}
  Result := MD4String(AStr);
  {$ENDIF}
end;

function HashFile(AFileName: string): TxdHash;
begin
  {$IFDEF MD5}
  Result := MD5File(AFileName, -1);
  {$ELSE}
  Result := CtEmptyHash;
  MD4File(AFileName, Result );
  {$ENDIF}
end;

function HashBuffer(const Buffer: PByte; const Size: Integer): TxdHash;
var
  context: TxdHashContext;
begin
  HashInit(context);
  HashUpdate(context, Buffer, Size);
  HashFinal(context, Result);
end;

function HashCompare(const D1, D2: TxdHash): Boolean;
begin
  Result := CompareMem(@D1, @D2, CtHashSize);
end;

function  IsEmptyHash(const AHash: TxdHash): Boolean;
begin
  Result := HashCompare(CtEmptyHash, AHash);
end;

function HashToStr(const D: TxdHash): string;
begin
  SetLength(Result, CtHashSize * 2);
  BinToHex(@D, PChar(Result), CtHashSize);
end;

procedure EmptyHash(var AHash: TxdHash);
begin
  FillChar( AHash, CtHashSize, 0 );
end;

function StrToHash(const AStrHash: string; var AHash: TxdHash): Boolean;
begin
  Result := False;
  if Length(AStrHash) <> CtHashSize * 2 then Exit;
  EmptyHash(AHash);
  HexToBin( PChar(AStrHash), @AHash, CtHashSize );
  Result := True;
end;

procedure HashInit(var Context: TxdHashContext);
begin
  {$IFDEF MD5}
  MD5Init(Context);
  {$ELSE}
  MD4Init(Context);
  {$ENDIF}
end;

procedure HashUpdate(var Context: TxdHashContext; const Buffer: PByte; const Len: Integer);
begin
  {$IFDEF MD5}
  MD5Update(Context, PAnsiChar(Buffer), Len);
  {$ELSE}
  MD4Update(Context, Buffer^, Len);
  {$ENDIF}
end;

procedure HashFinal(var Context: TxdHashContext; var Digest: TxdHash);
begin
  {$IFDEF MD5}
  MD5Final(Context, Digest);
  {$ELSE}
  MD4Final(Context, Digest);
  {$ENDIF}
end;

end.
