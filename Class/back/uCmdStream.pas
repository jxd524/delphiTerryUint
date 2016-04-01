unit uCmdStream;

interface

uses
  Windows, SysUtils, Classes, WinSock2;

type
  TCmdStream = class
  private
    FMemory: PChar;
    FMaxSize: Integer;
    FSize: Integer;
    FPosition: Integer;
    FTag: Cardinal;
    FData: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitWrite(ABuffer: PChar; AMaxSize: Integer); overload;
    procedure InitRead(ABuffer: PChar; ASize: Integer); overload;
    procedure InitRead(const ABuffer: string); overload;
    procedure InitRead(ASize: Integer); overload;

    function Seek(AOffset: Longint; AOrigin: Word): Integer;
    //--------------------------数据的读取--------------------------------------
    //下面的过程是读取固定长度的数据，
    function ReadByte(DefaultValue: Byte = 0): Byte;
    function ReadWord(DefaultValue: WORD = 0): WORD;
    function ReadCardinal(DefaultValue:Cardinal =0):Cardinal;
    function ReadInt64(DefaultValue: Int64 = 0): Int64;
    function ReadLong(ABuffer: PChar; ASize: Integer): Boolean;
    //下面的过程是读取不定长的数据，前面有一个字节来标识数据的长度
    function ReadBuffer(ABuffer: PChar): Boolean;
    function ReadBufferEx(ABuffer: PChar; const Ahtons: Boolean = False): Boolean;
    function ReadString: string;
    function ReadStringEx(const Ahtons: Boolean = False): string;

    //--------------------------数据的写入--------------------------------------
    //下面是写入固定长度的数据
    procedure WriteByte(const Value: Byte; AModify: Boolean = False);
    procedure WriteWord(const Value: WORD; AModify: Boolean = False);
    procedure WriteCardinal(const Value: Cardinal; AModify: Boolean = False);
    procedure WriteInt64(const Value: Int64);
    procedure WriteLong(ABuffer: PChar; ASize: Integer);
    //下面的过程是写入不定长的数据，前面有一个字节来标识数据的长度
    procedure WriteBuffer(ABuffer: PChar; ASize: Byte);
    procedure WriteBufferEx(ABuffer: PChar; ASize: Word; const Ahtons: Boolean = False);

    procedure WriteString(ABuffer: string);
    procedure WriteStringEx(ABuffer: string);

    //一些属性
    property Position: Integer read FPosition;
    property Size: Integer read FSize;
    property MaxSize: Integer read FMaxSize;
    property Memory: PChar read FMemory;
    property Tag: Cardinal read FTag write FTag;
    property Data: Cardinal read FData write FData;
  end;

  TCmdStreamEx = class
  private
    FMem: PChar;
    FMemSize: Integer;
    FCmdStream: TCmdStream;
  public
    constructor Create(const AMemSize: Integer);
    destructor  Destroy; override;
    property CmdStream: TCmdStream read FCmdStream;
  end;
implementation

{ TCmdStream }

constructor TCmdStream.Create;
begin
  FMemory := nil;
  FMaxSize := 0;
  FSize := 0;
  FPosition := 0;
end;

destructor TCmdStream.Destroy;
begin
  FMemory := nil;
  FSize := 0;
  FPosition := 0;
  inherited;
end;

procedure TCmdStream.InitRead(ABuffer: PChar; ASize: Integer);
begin
  FPosition := 0;
  FMemory := ABuffer;
  FSize := ASize;
  FMaxSize := ASize;
end;

procedure TCmdStream.InitRead(const ABuffer: string);
begin
  InitRead(@ABuffer[1], Length(ABuffer));
end;

procedure TCmdStream.InitRead(ASize: Integer);
begin
  InitRead(FMemory, ASize);
end;

procedure TCmdStream.InitWrite(ABuffer: PChar; AMaxSize: Integer);
begin
  FPosition := 0;
  FSize := 0;
  FMaxSize := AMaxSize;
  FMemory := ABuffer;
end;

function TCmdStream.ReadBuffer(ABuffer: PChar): Boolean;
var
  Size: Byte;
begin
  Size := ReadByte;
  if Size > 0 then
    Result := ReadLong(ABuffer, Size)
  else
    Result := False;
end;

function TCmdStream.ReadBufferEx(ABuffer: PChar; const Ahtons: Boolean = False): Boolean;
var
  Size: Word;
begin
  if Ahtons then
    Size := htons( ReadWord )
  else
    Size := ReadWord;
  if Size > 0 then
    Result := ReadLong(ABuffer, Size)
  else
    Result := False;
end;


function TCmdStream.ReadByte(DefaultValue: Byte): Byte;
begin
  if FPosition + 1 <= FSize then
  begin
    Result := PByte(FMemory + FPosition)^;
    Inc(FPosition);
  end
  else
    Result := DefaultValue;
end;

function TCmdStream.ReadCardinal(DefaultValue: Cardinal): Cardinal;
begin
  if FPosition + 4 <= FSize then
  begin
    Result := PCardinal(FMemory + FPosition)^;
    Inc(FPosition, 4);
  end
  else
    Result := DefaultValue;
end;

function TCmdStream.ReadInt64(DefaultValue: Int64): Int64;
begin
  if FPosition + 8 <= FSize then
  begin
    Result := PInt64(FMemory + FPosition)^;
    Inc(FPosition, 8);
  end
  else
    Result := DefaultValue;
end;

function TCmdStream.ReadLong(ABuffer: PChar; ASize: Integer): Boolean;
begin
  Result := FPosition + ASize <= FSize;
  if Result then
  begin
    Move(PChar(FMemory + FPosition)^, ABuffer^, ASize);
    Inc(FPosition, ASize);
  end;
end;

function TCmdStream.ReadString: string;
var
  Size: Byte;
begin
  Size := ReadByte;
  if Size > 0 then
  begin
    SetLength(Result, Size);
    ReadLong(@Result[1], Size);
  end
  else
    Result := '';
end;

function TCmdStream.ReadStringEx(const Ahtons: Boolean): string;
var
  Size: Byte;
begin
  if Ahtons then
    Size := htons( ReadWord )
  else
    Size := ReadWord;
  if Size > 0 then
  begin
    SetLength(Result, Size);
    ReadLong(@Result[1], Size);
  end
  else
    Result := '';
end;

function TCmdStream.ReadWord(DefaultValue: WORD): WORD;
begin
  if FPosition + 2 <= FSize then
  begin
    Result := PWORD(FMemory + FPosition)^;
    Inc(FPosition, 2);
  end
  else
    Result := DefaultValue;
end;

function TCmdStream.Seek(AOffset: Integer; AOrigin: Word): Integer;
begin
  case AOrigin of
    soFromBeginning: FPosition := AOffset;
    soFromCurrent: Inc(FPosition, AOffset);
    soFromEnd: FPosition := FSize + AOffset;
  end;
  if FPosition < 0 then
    FPosition := 0
  else if FPosition > FMaxSize then
    FPosition := FMaxSize;
  Result := FPosition;
end;

procedure TCmdStream.WriteBuffer(ABuffer: PChar; ASize: Byte);
begin
  WriteByte(ASize);
  WriteLong(ABuffer, ASize);
end;

procedure TCmdStream.WriteBufferEx(ABuffer: PChar; ASize: Word; const Ahtons: Boolean);
begin
  if Ahtons then
    WriteWord( htons(ASize) )
  else
    WriteWord(ASize);
  WriteLong(ABuffer, ASize);
end;

procedure TCmdStream.WriteByte(const Value: Byte; AModify: Boolean = False);
begin
  if FPosition + 1 <= FMaxSize then
  begin
    PByte(FMemory + FPosition)^ := Value;
    Inc(FPosition);
    if not AModify then
      Inc(FSize);
  end;
end;

procedure TCmdStream.WriteCardinal(const Value: Cardinal; AModify: Boolean);
begin
  if FPosition + 4 <= FMaxSize then
  begin
    PCardinal(FMemory + FPosition)^ := Value;
    Inc(FPosition, 4);
    if not AModify then
      Inc(FSize, 4);
  end;
end;

procedure TCmdStream.WriteInt64(const Value: Int64);
begin
  if FPosition + 8 <= FMaxSize then
  begin
    PInt64(FMemory + FPosition)^ := Value;
    Inc(FPosition, 8);
    Inc(FSize, 8);
  end;
end;

procedure TCmdStream.WriteLong(ABuffer: PChar; ASize: Integer);
begin
  if FPosition + ASize <= FMaxSize then
  begin
    Move(ABuffer^, PChar(FMemory + FPosition)^, ASize);
    Inc(FPosition, ASize);
    Inc(FSize, ASize);
  end;
end;

procedure TCmdStream.WriteString(ABuffer: string);
begin
  WriteBuffer(PChar(ABuffer), Length(ABuffer));
end;

procedure TCmdStream.WriteStringEx(ABuffer: string);
begin
  WriteBufferEx(PChar(ABuffer), Length(ABuffer));
end;

procedure TCmdStream.WriteWord(const Value: WORD; AModify: Boolean = False);
begin
  if FPosition + 2 <= FMaxSize then
  begin
    PWORD(FMemory + FPosition)^ := Value;
    Inc(FPosition, 2);
    if not AModify then
      Inc(FSize, 2);
  end;
end;

{ TCmdStreamEx }

constructor TCmdStreamEx.Create(const AMemSize: Integer);
begin
  FMemSize := AMemSize;
  if FMemSize <= 0 then
    FMemSize := 1024;
  GetMem( FMem, FMemSize );
  FCmdStream := TCmdStream.Create;
  FCmdStream.InitWrite( FMem, FMemSize );
end;

destructor TCmdStreamEx.Destroy;
begin
  FreeMem( FMem );
  FCmdStream.Free;
  inherited;
end;

end.

