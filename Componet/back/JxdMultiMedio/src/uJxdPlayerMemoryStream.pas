unit uJxdPlayerMemoryStream;

interface
uses
  Windows, SysUtils, Classes, DSUtil, BaseClass, DirectShow9, MMSystem, Math,
  uAsyncIO, uAsyncRdr;
type

  TOnReadBuffer = procedure(Sender: TObject; AIsEncrypt: Boolean; APosition, AReadLength, ALength: Integer) of object;
  TJxdPlayerMemStream = class(TBCAsyncStream)
  private
    (*FKKPlayer: TKKPlayer;*)
    FLock: TBCCritSec;
    FLength: LONGLONG;
    FPosition: LONGLONG;
    FReadBuffer: TOnReadBuffer;
    FOnReadFileError: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;

    property Position: LONGLONG read FPosition;
    property OnReadBuffer: TOnReadBuffer read FReadBuffer write FReadBuffer;
    property OnReadFileError: TNotifyEvent read FOnReadFileError write FOnReadFileError;

    function SetPointer(APos: LONGLONG): HResult; override;
    function Read(ABuffer: PByte; ABytesToRead: DWord; AAlign: Boolean; out ABytesRead: DWord): HResult; override;
    function Size(out ASizeAvailable: LONGLONG): LONGLONG; override;
    function Alignment: DWord; override;
    procedure Lock; override;
    procedure Unlock; override;
  end;

  TJxdMemStreamReader = class(TBCAsyncReader)
  public
    function Register: HRESULT; override; stdcall;
    function Unregister: HRESULT; override; stdcall;

    constructor Create(AStream: TJxdPlayerMemStream; Amt: PAMMediaType; out hr: HResult);
  end;

implementation

constructor TJxdPlayerMemStream.Create;
begin
  FLength   := 0;//$7fffffffff;//1024 * 1024 * 10;
  FPosition := 0;
  FLock := TBCCritSec.Create;
  Inherited Create;
end;

destructor TJxdPlayerMemStream.Destroy;
begin
  if Assigned(FLock) then
    FreeAndNil(FLock);
  Inherited ;
end;

function TJxdPlayerMemStream.SetPointer(APos: LONGLONG): HResult;
begin
//  Result := S_OK; Exit;

  if (APos < 0) or (APos > FLength) then
    Result := S_FALSE
  else
  begin
    FPosition := APos;
    Result := S_OK;
  end;
end;

//var
//  n: Integer = 0;

function TJxdPlayerMemStream.Read(ABuffer: PByte; ABytesToRead: DWord;
  AAlign: Boolean; out ABytesRead: DWord): HResult;
var
  fSrc: TFileStream;
  nLen: Integer;
  p: PChar;
begin
  fSrc := TFileStream.Create( 'E:\CompanyWork\MusicT\Small Project\REC\bin\test1500000.wmv', fmOpenRead );
  GetMem( p, ABytesToRead );
  try

    if fSrc.Size - FPosition - ABytesToRead <= 0 then
    begin
      FreeAndNil( fSrc );
      Sleep(1000);
      Result := S_FALSE;
      Exit;
    end;

    fSrc.Seek( FPosition, soFromBeginning );
    nLen := fSrc.Read( p^, ABytesToRead );
    Move( p^, ABuffer^, nLen );

    if (nLen = 0) or (nLen <> ABytesToRead) then
    begin
      Result := S_FALSE;
      Exit;
    end;
    ABytesRead := nLen;
    FPosition := FPosition + ABytesRead;
    FLength := FPosition;
  finally
    FreeMem( p );
    FreeAndNil( fSrc );
  end;
  Result := S_OK;
end;

function TJxdPlayerMemStream.Size(out ASizeAvailable: LONGLONG): LONGLONG;
begin
  ASizeAvailable := FLength;
  Result := $7fffffffff;
  Result := FLength;
end;

function TJxdPlayerMemStream.Alignment: DWord;
begin
  Result := 1;
end;

procedure TJxdPlayerMemStream.Lock;
begin
  FLock.Lock;
end;

procedure TJxdPlayerMemStream.Unlock;
begin
  FLock.UnLock;
end;

// --- TBCMemFileReader ---

constructor TJxdMemStreamReader.Create(AStream: TJxdPlayerMemStream;
  Amt: PAMMediaType; out hr: HResult);
begin
  Inherited Create( 'Jxd MemoryStream Reader', nil, AStream, hr );
  Fmt := Amt^;
end;

function TJxdMemStreamReader.Register: HResult;
begin
  Result := S_OK;
end;

function TJxdMemStreamReader.UnRegister: HResult;
begin
  Result := S_OK;
end;

end.


