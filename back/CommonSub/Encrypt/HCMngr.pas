{Copyright:      Hagen Reddmann  mailto:HaReddmann@AOL.COM
 Author:         Hagen Reddmann
 Remarks:        freeware, but this Copyright must be included
 known Problems: none
 Version:        3.0,  Part I from Delphi Encryption Compendium
                 Delphi 2-4, designed and testet under D3
 Description:    included the Designtime Components
                 THashManager for the THash-Administration
                 TCipherManager for the TCipher-Administration
                 for Programmers with "Drop and Design any Component Fealing"
                 the normaly way is without any Manager's

 Version 3.0
     Versionsmanagement, no chnages on TManager Classes

 Version 2.2
                 added Progress Event to fill a gauge

      bug fixes: THashManager memory leak, .Destroy frees now FDigest

 Version 2.1
                 TCipherManager: Methods added
                   EncodeBuffer(), DecodeBuffer()
                   EncodeString(), DecodeString()

}
unit HCMngr;

interface

{$I VER.INC}

uses Classes, DECUtil, Hash, Cipher;

type
  TProgressEvent = procedure(Sender: TObject; Current, Maximal: Integer) of Object;

  THashManager = class(TComponent)
  private
    FHashClass: THashClass;
    FHash: THash;
    FDigest: String;
    FOnProgress: TProgressEvent;
    function GetInfo(Index: Integer): String;
    procedure SetInfo(Index: Integer; Value: String);
    function GetClass: THashClass;
    procedure SetClass(Value: THashClass);
    function GetHash: THash;
    function GetAlgorithm: String;
    procedure SetAlgorithm(Value: String);
    function GetDigestStr(Index: Integer): String;
    function GetDigestSize: Integer;
    procedure ReadHash(Reader: TReader);
    procedure WriteHash(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    destructor Destroy; override;
    procedure CalcBuffer(const Buffer; BufferSize: Integer);
    procedure CalcStream(const Stream: TStream; StreamSize: Integer);
    procedure CalcString(const Data: String);
    procedure CalcFile(const FileName: String);

    property Digest: String read FDigest;
    property DigestSize: Integer read GetDigestSize;
    property DigestString[Format: Integer]: String read GetDigestStr;
    property HashClass: THashClass read GetClass write SetClass;
    property Hash: THash read GetHash;
  published
    property Algorithm: String read GetAlgorithm write SetAlgorithm stored False;
    property Description: String index 0 read GetInfo write SetInfo stored False;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

  TCipherManager = class(TComponent)
  private
    FHashManager: THashManager;
    FCipherClass: TCipherClass;
    FCipher: TCipher;
    FMode: TCipherMode;
    FOnProgress: TProgressEvent;
    procedure SetHashManager(Value: THashManager);
    procedure SetMode(Value: TCipherMode);
    function GetCipher: TCipher;
    function GetClass: TCipherClass;
    procedure SetClass(Value: TCipherClass);
    function GetAlgorithm: String;
    procedure SetAlgorithm(Value: String);
    function GetInfo(Index: Integer): String;
    procedure SetInfo(Index: Integer; Value: String);
    procedure ReadCipher(Reader: TReader);
    procedure WriteCipher(Writer: TWriter);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    destructor Destroy; override;
    procedure InitKey(const Key: String; IVector: Pointer);

    procedure EncodeFile(const Source, Dest: String);
    procedure DecodeFile(const Source, Dest: String);
    procedure EncodeStream(Source, Dest: TStream; DataSize: Integer);
    procedure DecodeStream(Source, Dest: TStream; DataSize: Integer);
    procedure EncodeBuffer(const Source; var Dest; DataSize: Integer);
    procedure DecodeBuffer(const Source; var Dest; DataSize: Integer);
    function EncodeString(const Source: String): String;
    function DecodeString(const Source: String): String;

    property Cipher: TCipher read GetCipher;
    property CipherClass: TCipherClass read GetClass write SetClass;
  published
    property Algorithm: String read GetAlgorithm write SetAlgorithm stored False;
    property Description: String index 0 read GetInfo write SetInfo stored False;
    property Mode: TCipherMode read FMode write SetMode default cmCBC;
    property HashManager: THashManager read FHashManager write SetHashManager;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

uses SysUtils, Windows;

procedure THashManager.ReadHash(Reader: TReader);
var
  CN: String;
  I: Integer;
begin
  CN := Reader.ReadString;
  for I := 0 to HashList.Count-1 do
    if CN = THashClass(HashList.Objects[I]).ClassName then
    begin
      SetClass(THashClass(HashList.Objects[I]));
      Exit;
    end;
end;

procedure THashManager.WriteHash(Writer: TWriter);
begin
  Writer.WriteString(HashClass.ClassName);
end;

procedure THashManager.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Hash', ReadHash, WriteHash, FHashClass <> nil);
end;

function THashManager.GetInfo(Index: Integer): String;
begin
  Result := '';
  case Index of
    0: begin
         Index := HashList.IndexOfObject(Pointer(HashClass));
         if Index >= 0 then
         begin
           Result := Trim(HashList[Index]);
           Delete(Result, 1, Pos('=', Result));
         end;
         if Length(Result) > 0 then Result := Result + ', ';
         Result := Result + IntToStr(DigestSize * 8) + 'bit Digestsize';
       end;
  end;
end;

procedure THashManager.SetInfo(Index: Integer; Value: String);
begin
end;

function THashManager.GetDigestStr(Index: Integer): String;
begin
  Result := StrToFormat(PChar(FDigest), Length(FDigest), Index);
end;

function THashManager.GetDigestSize: Integer;
begin
  Result := HashClass.DigestKeySize;
end;

function THashManager.GetClass: THashClass;
begin
  if FHashClass = nil then SetClass(DefaultHashClass);
  Result := FHashClass;
end;

function THashManager.GetHash: THash;
begin
  if FHash = nil then FHash := HashClass.Create(nil);
  Result := FHash;
end;

procedure THashManager.SetClass(Value: THashClass);
begin
  if Value <> FHashClass then
  begin
    if Value = nil then Value := DefaultHashClass;
    FHashClass := Value;
    FHash.Free;
    FHash := nil;
    FDigest := '';
  end;
end;

function THashManager.GetAlgorithm: String;
begin
  Result := GetHashName(HashClass);
end;

procedure THashManager.SetAlgorithm(Value: String);
begin
  SetClass(GetHashClass(Value));
end;

destructor THashManager.Destroy;
begin
  FHash.Free;
  FHash := nil;
  inherited Destroy;
end;

procedure THashManager.CalcBuffer(const Buffer; BufferSize: Integer);
begin
  FDigest := HashClass.CalcBuffer(Buffer, BufferSize, nil, fmtCOPY);
end;

procedure THashManager.CalcStream(const Stream: TStream; StreamSize: Integer);
begin
  Progress := FOnProgress;
  try
    FDigest := HashClass.CalcStream(Stream, StreamSize, nil, fmtCOPY);
  finally
    Progress := nil;
  end;
end;

procedure THashManager.CalcString(const Data: String);
begin
  FDigest := HashClass.CalcString(Data, nil, fmtCOPY);
end;

procedure THashManager.CalcFile(const FileName: String);
begin
  Progress := FOnProgress;
  try
    FDigest := HashClass.CalcFile(FileName, nil, fmtCOPY);
  finally
    Progress := nil;
  end;
end;

procedure TCipherManager.SetHashManager(Value: THashManager);
begin
  if Value <> FHashManager then
  begin
    FHashManager := Value;
    if FHashManager <> nil then FHashManager.FreeNotification(Self);
  end;
end;

procedure TCipherManager.SetMode(Value: TCipherMode);
begin
  FMode := Value;
  if FCipher <> nil then FCipher.Mode := FMode;
end;

function TCipherManager.GetCipher: TCipher;
begin
  if FCipher = nil then
  begin
    FCipher := CipherClass.Create('', nil);
    FCipher.Mode := FMode;
  end;  
  Result := FCipher;
end;

function TCipherManager.GetClass: TCipherClass;
begin
  if FCipherClass = nil then FCipherClass := DefaultCipherClass;
  Result := FCipherClass;
end;

procedure TCipherManager.SetClass(Value: TCipherClass);
begin
  if Value <> CipherClass then
  begin
    FCipher.Free;
    FCipher := nil;
    FCipherClass := Value;
  end;
end;

function TCipherManager.GetAlgorithm: String;
begin
  Result := GetCipherName(CipherClass);
end;

procedure TCipherManager.SetAlgorithm(Value: String);
begin
  SetClass(GetCipherClass(Value));
end;

function TCipherManager.GetInfo(Index: Integer): String;
begin
  Result := '';
  case Index of
    0: begin
         Index := CipherList.IndexOfObject(Pointer(CipherClass));
         if Index >= 0 then
         begin
           Result := Trim(CipherList[Index]);
           Delete(Result, 1, Pos('=', Result));
         end;
         if Length(Result) > 0 then Result := Result + ', ';
         Result := Result + IntToStr(CipherClass.MaxKeySize * 8) + 'bit Key';
       end;
  end;
end;

procedure TCipherManager.SetInfo(Index: Integer; Value: String);
begin
end;

procedure TCipherManager.ReadCipher(Reader: TReader);
var
  CN: String;
  I: Integer;
begin
  CN := Reader.ReadString;
  for I := 0 to CipherList.Count-1 do
    if CN = TCipherClass(CipherList.Objects[I]).ClassName then
    begin
      SetClass(TCipherClass(CipherList.Objects[I]));
      Exit;
    end;
end;

procedure TCipherManager.WriteCipher(Writer: TWriter);
begin
  Writer.WriteString(CipherClass.ClassName);
end;

procedure TCipherManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FHashManager) then
    HashManager := nil;
end;

procedure TCipherManager.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Cipher', ReadCipher, WriteCipher, FCipherClass <> nil);
end;

destructor TCipherManager.Destroy;
begin
  FCipher.Free;
  FCipher := nil;
  inherited Destroy;
end;

procedure TCipherManager.InitKey(const Key: String; IVector: Pointer);
begin
  if FHashManager <> nil then Cipher.HashClass := FHashManager.HashClass;
  Cipher.InitKey(Key, IVector);
end;

procedure TCipherManager.EncodeFile(const Source, Dest: String);
begin
  Progress := FOnProgress;
  try
    Cipher.CodeFile(Source, Dest, paEncode);
  finally
    Progress := nil;
  end;
end;

procedure TCipherManager.DecodeFile(const Source, Dest: String);
begin
  Progress := FOnProgress;
  try
    Cipher.CodeFile(Source, Dest, paDecode);
  finally
    Progress := nil;
  end;
end;

procedure TCipherManager.EncodeStream(Source, Dest: TStream; DataSize: Integer);
begin
  Progress := FOnProgress;
  try
    Cipher.CodeStream(Source, Dest, DataSize, paEncode);
  finally
    Progress := nil;
  end;
end;

procedure TCipherManager.DecodeStream(Source, Dest: TStream; DataSize: Integer);
begin
  Progress := FOnProgress;
  try
    Cipher.CodeStream(Source, Dest, DataSize, paDecode);
  finally
    Progress := nil;
  end;
end;

procedure TCipherManager.EncodeBuffer(const Source; var Dest; DataSize: Integer);
begin
  Cipher.EncodeBuffer(Source, Dest, DataSize);
end;

procedure TCipherManager.DecodeBuffer(const Source; var Dest; DataSize: Integer);
begin
  Cipher.DecodeBuffer(Source, Dest, DataSize);
end;

function TCipherManager.EncodeString(const Source: String): String;
begin
  Result := Cipher.CodeString(Source, paEncode, fmtDEFAULT);
end;

function TCipherManager.DecodeString(const Source: String): String;
begin
  Result := Cipher.CodeString(Source, paDecode, fmtDEFAULT);
end;


end.
