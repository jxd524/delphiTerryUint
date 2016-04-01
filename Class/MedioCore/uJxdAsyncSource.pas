unit uJxdAsyncSource;

interface
uses
  BaseClass, DirectShow9, Windows, SysUtils, MMSystem, Math, ActiveX,
  UAsyncRdr, UAsyncIo, DSUtil, uJxdFileSegmentStream,  uConversion, Forms;

const
  CtAsyncSourceName = 'AsyncSource Filter';
  
type
  TxdAsyncFilter = class;

  TxdMemStream = class(TBCAsyncStream)
  public
    constructor Create;
    destructor Destroy; override;

    //  Initialization
    procedure Init(AOwner: TxdAsyncFilter; AFileStream: TxdP2SPFileStreamBasic; AKBPerSec: DWord = INFINITE);

    function  Read(ABuffer: PByte; ABytesToRead: DWord; AAlign: Boolean; out ABytesRead: DWord): HResult; override;
    function  SetPointer(APos: LONGLONG): HResult; override;
    function  Size(out ASizeAvailable: LONGLONG): LONGLONG; override;
    function  Alignment: DWord; override;
    procedure Lock; override;
    procedure Unlock; override;
  private
    FWaitCount: Integer;
    FCSLock: TBCCritSec;
    FKBPerSec: DWord;
    FTimeStart: DWord;
    FOwnerFilter: TxdAsyncFilter;
    FCurReadPos: Int64;

    FFileStream: TxdP2SPFileStreamBasic;
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
    procedure SetAsyncFileStream(const AObject: TxdP2SPFileStreamBasic);
  private
    FStream: TxdMemStream;
    FStopFilter: Boolean;
    FBuffering: Boolean;
    FIsCurConnecttingFilter: Boolean;
  public
    {由外部设置，交给Fstream去使用}
    property IsCurConnecttingFilter: Boolean read FIsCurConnecttingFilter write FIsCurConnecttingFilter; //当前是否正在连接Filter中
    property FilterStop: Boolean read FStopFilter write FStopFilter; //停止流
    property Buffering: Boolean read FBuffering; //是否正在缓存中
  end;

implementation

// --- TBCMemStream ---

constructor TxdMemStream.Create;
begin
  Inherited;
  FCurReadPos := 0;
  FWaitCount := 0;
  FCSLock := TBCCritSec.Create;
end;

destructor TxdMemStream.Destroy;
begin
  if Assigned(FCSLock) then
    FreeAndNil(FCSLock);

  Inherited Destroy;
end;

procedure TxdMemStream.Init(AOwner: TxdAsyncFilter; AFileStream: TxdP2SPFileStreamBasic; AKBPerSec: DWord = INFINITE);
begin
  FOwnerFilter := AOwner;
  FFileStream := AFileStream;
  FKBPerSec := AKBPerSec;
  FTimeStart := timeGetTime;
end;

function TxdMemStream.SetPointer(APos: LONGLONG): HResult;
begin
  if (APos < 0) or (APos > FFileStream.FileSize) then
  begin
    Result := S_FALSE;
//    Dbg( 'Error SetPointer: %d', [APos] );
  end
  else
  begin
    FCurReadPos := APos;
    Result := S_OK;
  end;
end;

function TxdMemStream.Read(ABuffer: PByte; ABytesToRead: DWord;
  AAlign: Boolean; out ABytesRead: DWord): HResult;
var
  i: Integer;
  nCompletedCount: Int64;
  s: TxdFileSegmentStream;
begin
  Result := S_OK;  
  while not FFileStream.ReadBuffer(FCurReadPos, ABytesToRead, ABuffer) do
  begin
    FOwnerFilter.FBuffering := True;

    if FOwnerFilter.IsCurConnecttingFilter then
    begin
      //正在连接Filter的过程中, 如果下载
      if FFileStream is TxdFileSegmentStream then
      begin
        s := FFileStream as TxdFileSegmentStream;
        if Assigned(s.SegmentTable) then
        begin
          nCompletedCount := s.SegmentTable.CompletedFileSize;
          if s.SegmentTable.FileSize > 0 then
          begin
             if nCompletedCount / s.SegmentTable.FileSize >= 0.1 then
             begin
//               Result := S_FALSE;
//               ABytesRead := 0;
//               Break;
//               Inc( FWaitCount );
//               if FWaitCount > 30 then
//               begin
//                 Result := S_FALSE;
//                 ABytesRead := 0;
//                 FWaitCount := 0;
//                 Break;
//               end;
             end;
          end;
        end;
      end;
    end;

    
//    OutputDebugString( PChar('wait to read: ' + IntToStr(FCurReadPos) + '  Size: ' + IntToStr(ABytesToRead)) );
    for i := 0 to 4 do
    begin
      Application.ProcessMessages;
      Sleep(1);
    end;

    if FOwnerFilter.FilterStop then
    begin
      Result := S_FALSE;
      ABytesRead := 0;
      Break;
    end;
  end;
  if Result = S_OK then
    ABytesRead := ABytesToRead;
  FOwnerFilter.FBuffering := False;
end;

function TxdMemStream.Size(out ASizeAvailable: LONGLONG): LONGLONG;
var
  _CurrentAvailable: LONGLONG;
  nSize: Cardinal;
begin
  _CurrentAvailable := UInt32x32To64(timeGetTime - FTimeStart, FKBPerSec);

  if Assigned(FFileStream) then
    nSize := FFileStream.FileSize
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

procedure TxdAsyncFilter.SetAsyncFileStream(const AObject: TxdP2SPFileStreamBasic);
begin
  FStopFilter := False;
  FStream.Init( Self, AObject );
end;

end.

