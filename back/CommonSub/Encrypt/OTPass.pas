unit OTPass;

interface

uses
  Windows, Messages, SysUtils, Classes, DECUtil, BigNum, BNG, Cipher;

type
  EProtector = class(Exception);

  TProtectionEvent = procedure(Sender: TObject; var Protection: TProtection) of Object;

  TOneTimePassword = class(TComponent)
  private
    FSeed: String;
    FKeySize: Word;
    FKeyBlocks: Word;
    FKeyBase: TBase;
    FKeySeparator: Char;
    FID: Integer;
    FBBS: TRandom_BBS;
    FOnGetProtection: TProtectionEvent;
    procedure SetID(Value: Integer);
    function GetMaxID: Integer;
    procedure SetMaxID(Value: Integer);
    function GetSeed: String;
    procedure SetSeed(Value: String);
    function GetKey: String;
    procedure WriteInternal(Writer: TWriter);
    procedure ReadInternal(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Key: String read GetKey;
    property BBS: TRandom_BBS read FBBS;
  published
    property Seed: String read GetSeed write SetSeed stored False;
    property KeySize: Word read FKeySize write FKeySize default 8;
    property KeyBlocks: Word read FKeyBlocks write FKeyBlocks default 4;
    property KeyBase: TBase read FKeyBase write FKeyBase default 36;
    property KeySpearator: Char read FKeySeparator write FKeySeparator default '-';
    property ID: Integer read FID write SetID;
    property MaxID: Integer read GetMaxID write SetMaxID stored False;

    property OnGetProtection: TProtectionEvent read FOnGetProtection write FOnGetProtection; 
  end;

implementation

uses RNG, Dialogs, Forms, DECConst;

function TOneTimePassword.GetMaxID: Integer;
begin
  Result := MaxInt div FKeySize;
end;

procedure TOneTimePassword.SetMaxID(Value: Integer);
begin
  if Value > 0 then FKeySize := MaxInt div Value;
end;

procedure TOneTimePassword.SetID(Value: Integer);
begin
  if (Value < 0) or (Value > MaxID) then
    raise EProtector.Create(sIDOutOfRange);
  FID := Value;
end;

function TOneTimePassword.GetSeed: String;
var
  CRC,sCRC: Word;
begin
  if FSeed <> '' then
  begin
    Result := FormatToStr(PChar(FSeed), Length(FSeed), fmtMIME64);
//    BufferDecode(PChar(Result)^, Length(Result), ClassName);
    CRC := not CRC16($FFFF, PChar(Result), Length(Result) -4);
    Delete(Result, 1, 4);
    try
      sCRC := StrToInt('$' + Copy(Result, Length(Result)-3, 4));
      SetLength(Result, Length(Result)-4);
      if sCRC <> CRC then Abort;
    except
      raise EProtector.Create(sInvalidState);
    end;
  end else Result := FSeed;
end;

procedure TOneTimePassword.SetSeed(Value: String);
var
  CRC: Word;
begin
  if Value <> '' then
  begin
    FSeed := Value;
    if ComponentState * [csDesigning, csDestroying] = [] then
      FBBS.Seed(PChar(FSeed)^, Length(FSeed));
    RndXORBuffer(RndTimeSeed, CRC, SizeOf(CRC));
    FSeed := IntToHex(CRC, 4) + FSeed;
    CRC := not CRC16($FFFF, PChar(FSeed), Length(FSeed));
    FSeed := FSeed + IntToHex(CRC, 4);
//    BufferEncode(PChar(FSeed)^, Length(FSeed), ClassName);
    FSeed := StrToFormat(PChar(FSeed), Length(FSeed), fmtMIME64);
  end else FSeed := Value;
end;

function TOneTimePassword.GetKey: String;
var
  Settings: TNumSettings;
  ValidSeed: Boolean;
begin
  Settings := NumSettings;
  ValidSeed := False;
  try
    with NumSettings do
    begin
      StrBase := FKeyBase;
      StrBlocks := FKeyBlocks;
      StrSeparator[0] := FKeySeparator;
      StrSeparator[1] := #0;
      StrPadding := '0';
    end;
    FBBS.Seekable := True;
    Result := Seed;
    if Result <> '' then
    begin
      FBBS.Seed(PChar(Result)^, Length(Result));
      RndXORBuffer(RndTimeSeed, PChar(Result)^, Length(Result));
      SetLength(Result, 0);
      ValidSeed := True;
    end;
    FBBS.Seek(FID * FKeySize);
    Result := FBBS.Str(FKeySize);
  finally
    NumSettings := Settings;
    if ValidSeed then
    begin
      FBBS.NumSeed.SetToZero;
      FBBS.NumRegister.SetToZero;
    end;
  end;
end;

procedure TOneTimePassword.WriteInternal(Writer: TWriter);
begin
  Writer.WriteString(FSeed);
end;

procedure TOneTimePassword.ReadInternal(Reader: TReader);
begin
  FSeed := Reader.ReadString;
end;

procedure TOneTimePassword.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('State', ReadInternal, WriteInternal, True);
end;

constructor TOneTimePassword.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBBS := TRandom_BBS.Create('', 0, False);
  FBBS.Size := 512;
  FBBS.Seekable := True;
  FKeySize := 8;
  FKeyBlocks := 4;
  FKeyBase := 36;
  FKeySeparator := '-';
end;

destructor TOneTimePassword.Destroy;
begin
  FBBS.Free;
  FBBS := nil;
  inherited Destroy;
end;

end.
