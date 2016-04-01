unit uJxdAsyncSource;

{$Define DEBUG}

interface
uses
  BaseClass, DirectShow9, Windows, SysUtils, MMSystem, Math, ActiveX,
  UAsyncRdr, UAsyncIo, DSUtil, uJxdAsyncFileStream, uConversion, Forms
  {$IfDEF DEBUG}
  , uJxdPlayerConsts
  {$ENDIF};


const
  CtAsyncSourceName = 'AsyncSource Filter';
type
  TxdAsyncFilter = class;

  TxdMemStream = class(TBCAsyncStream)
  public
    constructor Create;
    destructor Destroy; override;

    //  Initialization
    procedure Init(AOwner: TxdAsyncFilter; AFileStream: TxdAsyncFileStream; AKBPerSec: DWord = INFINITE);

    function  Read(ABuffer: PByte; ABytesToRead: DWord; AAlign: Boolean; out ABytesRead: DWord): HResult; override;
    function  SetPointer(APos: LONGLONG): HResult; override;
    function  Size(out ASizeAvailable: LONGLONG): LONGLONG; override;
    function  Alignment: DWord; override;
    procedure Lock; override;
    procedure Unlock; override;
  private
    FCSLock: TBCCritSec;
    FKBPerSec: DWord;
    FTimeStart: DWord;
    FOwnerFilter: TxdAsyncFilter;

    FFileStream: TxdAsyncFileStream;
  end;

  TxdAsyncFilter = class(TBCAsyncReader, IFileSourceFilter)
  public
    constructor Create; overload;
    constructor Create(ObjName: string; Unk: IUnKnown; out hr : HRESULT); overload;
    constructor CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown); override;

    destructor Destroy; override;
    function NonDelegatingQueryInterface(const IID: TGUID; out Obj): HResult; override;

    // IFileSourceFilter methods
    function Load(AFileName: PWideChar; const Amt: PAMMediaType): HResult; stdcall;
    function GetCurFile(out AFileName: PWideChar; Amt: PAMMediaType): HResult; stdcall;

    //需要外部支持, 应当在Create之后调用
    procedure SetAsyncFileStream(const AObject: TxdAsyncFileStream);
  private
    FStream: TxdMemStream;
    FStopFilter: Boolean;
    FBuffering: Boolean;
  public
    property FilterStop: Boolean read FStopFilter write FStopFilter;
    property Buffering: Boolean read FBuffering;
  end;

implementation

// --- TBCMemStream ---

constructor TxdMemStream.Create;
begin
  Inherited;

  FCSLock := TBCCritSec.Create;
end;

destructor TxdMemStream.Destroy;
begin
  if Assigned(FCSLock) then
    FreeAndNil(FCSLock);

  Inherited Destroy;
end;

procedure TxdMemStream.Init(AOwner: TxdAsyncFilter; AFileStream: TxdAsyncFileStream; AKBPerSec: DWord = INFINITE);
begin
  FOwnerFilter := AOwner;
  FFileStream := AFileStream;
  FKBPerSec := AKBPerSec;
  FTimeStart := timeGetTime;
end;

function TxdMemStream.SetPointer(APos: LONGLONG): HResult;
begin
  if (APos < 0) or (APos > FFileStream.Size) then
  begin
    Result := S_FALSE;
//    Dbg( 'Error SetPointer: %d', [APos] );
  end
  else
  begin
    FFileStream.CurReadPos := APos;
    Result := S_OK;
  end;
end;

function TxdMemStream.Read(ABuffer: PByte; ABytesToRead: DWord;
  AAlign: Boolean; out ABytesRead: DWord): HResult;
begin
  Result := S_OK;
  FOwnerFilter.FBuffering := True;
  while not FFileStream.ReadBuffer(ABuffer, ABytesToRead, ABytesRead) do
  begin
    Sleep(10);
    Application.ProcessMessages;
    if FOwnerFilter.FilterStop then
    begin
      Result := S_FALSE;
      Break;
    end;
  end;
  FOwnerFilter.FBuffering := False;
end;

function TxdMemStream.Size(out ASizeAvailable: LONGLONG): LONGLONG;
var
  _CurrentAvailable: LONGLONG;
  nSize: Cardinal;
begin
  _CurrentAvailable := UInt32x32To64(timeGetTime - FTimeStart, FKBPerSec);

  if Assigned(FFileStream) then
    nSize := FFileStream.Size
  else
    nSize := 0;

  ASizeAvailable := min(nSize, _CurrentAvailable);
  Result := nSize;
end;

function TxdMemStream.Alignment: DWord;
begin
  Result := 1;
end;

procedure TxdMemStream.Lock;
begin
  FCSLock.Lock;
end;

procedure TxdMemStream.Unlock;
begin
  FCSLock.UnLock;
end;

// --- TBCAsyncFilter ---

constructor TxdAsyncFilter.Create(ObjName: string; Unk: IUnKnown; out hr : HRESULT);
begin
  try
    FStream := TxdMemStream.Create;

    Inherited Create(ObjName, Unk, FStream, hr);
    hr := NOERROR;
  except
    hr := E_OUTOFMEMORY;
  end;
  FBuffering := True;
end;

constructor TxdAsyncFilter.Create;
var
  hr: HRESULT;
begin
  Create( CtAsyncSourceName, nil, hr );
end;

constructor TxdAsyncFilter.CreateFromFactory(Factory: TBCClassFactory;
  const Controller: IUnknown);
var
  hr: HRESULT;
begin
  Create(Factory.Name, Controller, hr);
end;

destructor TxdAsyncFilter.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TxdAsyncFilter.NonDelegatingQueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if IsEqualGUID(IID, IID_IFileSourceFilter) then
    if GetInterface(IID_IFileSourceFilter, Obj) then
      Result := S_OK
    else
      Result := E_FAIL
  else
    Result := Inherited NonDelegatingQueryInterface(IID, Obj);
end;

function TxdAsyncFilter.Load(AFileName: PWideChar; const Amt: PAMMediaType): HResult;
var
  _mt: TAMMediaType;
begin
  //AFileName：不需要
  FCSFilter.Lock;
  try
    if (Amt = nil) then
    begin
      ZeroMemory(@_mt, SizeOf(TAMMediaType));
      _mt.majortype := MEDIATYPE_Stream;
      _mt.subtype := MEDIASUBTYPE_NULL;
    end
    else
      CopyMemory(@_mt, Amt, SizeOf(TAMMediaType));

    CopyMemory(@Fmt, @_mt, SizeOf(TAMMediaType));
    Fmt.bTemporalCompression := True;
    Fmt.lSampleSize := 1;
    Result := S_OK;
  finally
    FCSFilter.UnLock;
  end;
end;

function TxdAsyncFilter.GetCurFile(out AFileName: PWideChar;
  Amt: PAMMediaType): HResult;
var
  n: DWord;
begin
  AFileName := nil;

  if Assigned(FStream) then
  begin
    n := Length(FStream.FFileStream.FileName) * 2;
    AFileName := CoTaskMemAlloc(n);
    if Assigned(AFileName) then
      CopyMemory(AFileName, PChar(FStream.FFileStream.FileName), n);
  end;

  if Assigned(Amt) then
    CopyMemory(Amt, @Fmt, SizeOf(TAMMediaType));
  
  Result := NOERROR;
end;

procedure TxdAsyncFilter.SetAsyncFileStream(const AObject: TxdAsyncFileStream);
begin
  FStopFilter := False;
  FStream.Init( Self, AObject );
end;

end.

