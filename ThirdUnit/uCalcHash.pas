unit uCalcHash;

//{$DEFINE ThreadClass}

interface

uses
  Windows, SysUtils, Classes,MD4,Forms;

type
  TCalcEd2kHashProgressEvent = procedure(Sender:TObject;ACalcBytes:Int64;ATitalBytes:Int64;var AStop:Boolean) of object;
  TCalcFinishEvent = procedure(Sender:TObject) of object;

  TCalcEd2kHashThread = class( {$IFDEF ThreadClass} TThread {$ELSE} TObject {$ENDIF} )
  private
    FFileName:string;
    FCalcBytes:Int64;
    FStop:Boolean;
    FFileHash:TMD4Digest;
    FFileSize:Int64;
    FFastCalc:Boolean;
    FOnCalcHashProgress: TCalcEd2kHashProgressEvent;
    FOnCalcFinish :TCalcFinishEvent;
    FWebHash: TMD4Digest;
    FFullHash: string;
    procedure SynchronizeProgress;
    procedure DoCalcFinish;
    function CalcHash(const FileName: string; var Digest: TMD4Digest;
      PAbort: PBoolean; AFastCalc: Boolean): Boolean;
    function CalcHashStream(const Stream: TStream; var Digest: TMD4Digest; PAbort: PBoolean = nil; AFastCalc: Boolean = True): Boolean;
  protected
    {$IFDEF ThreadClass}
    procedure Execute; override;
    {$ELSE}
    procedure Execute;
    {$ENDIF}

  public
    constructor Create(const AFileName:string;AFastCalc:Boolean=True);
    property OnCalcHashProgress:TCalcEd2kHashProgressEvent read FOnCalcHashProgress write FOnCalcHashProgress;
    property OnCalcFinish:TCalcFinishEvent read FOnCalcFinish write FOnCalcFinish;
    property FileHash:TMD4Digest read FFileHash;
    property WebHash:TMD4Digest read FWebHash;
    property FullHash:string read FFullHash;
    property FileName:string read FFileName;
    property FileSize:Int64 read FFileSize;
  end;

{$IFNDEF ThreadClass}
  procedure CalcFileHash(const AFileName: string; var AFileHash: TMD4Digest; var AFileSize: Int64);
{$ENDIF}

implementation


{ TCalcHashThread }

procedure CalcFileHash(const AFileName: string; var AFileHash: TMD4Digest; var AFileSize: Int64);
begin
  with TCalcEd2kHashThread.Create(AFileName, True) do
  begin
    Execute;
    AFileHash := FileHash;
    AFileSize := FileSize;
  end;
end;

function TCalcEd2kHashThread.CalcHash(const FileName: string;
  var Digest: TMD4Digest; PAbort: PBoolean; AFastCalc: Boolean): Boolean;
var
  hFile: THandle;
  Stream: TFileStream;
begin
  Result := False;
  OutputDebugString(PChar(Format('FileName:%s',[FileName])));
  //这样打开，可以优化文件读取，而且文件打开失败(不存在)也不会触发异常
  hFile := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if hFile = INVALID_HANDLE_VALUE then //文件打开失败
    Exit;
  Stream := TFileStream.Create(hFile);
  try
    Result := CalcHashStream(Stream, Digest, PAbort, AFastCalc);
  finally
    Stream.Free;
  end;
end;

function TCalcEd2kHashThread.CalcHashStream(const Stream: TStream;
  var Digest: TMD4Digest; PAbort: PBoolean; AFastCalc: Boolean): Boolean;
type
  TSegment = record
    SegmentID: Integer; //从0开始
    SegmentSize: Int64;
    SegmentHash: TMD4Digest;
  end;
  TFileSegmentInfo = record
    FileSize: Int64;
    SegmentSize: Int64;
    SegmentCount: Integer;
    Segments: array of TSegment;
  end;
const
  CSegmentSize = 9728000;
var
  MemStream: TMemoryStream;
  AInfo: TFileSegmentInfo;
  I: Integer;

  ModCount: Integer;
  Buffer: array[0..16383] of Byte;
  SavePos: Int64;

  function CalcHash(ASegmentID: Integer;var ASegmentHash:TMD4Digest): Boolean;
  var
    ReadBytes:Integer;
    LeftBytes:Integer;
    Context:TMD4Context;
  begin
    Result := True;
    if (Stream = nil) or (ASegmentID < 0) or (ASegmentID >= AInfo.SegmentCount) then
      Exit;
    LeftBytes :=AInfo.Segments[ASegmentID].SegmentSize;
    ZeroMemory(@ASegmentHash, SizeOf(TMD4Digest));
    MD4Init(Context);
    Stream.Seek(ASegmentID * AInfo.SegmentSize, soFromBeginning);
    repeat
      if LeftBytes >= SizeOf(Buffer) then
        ReadBytes := Stream.Read(Buffer, SizeOf(Buffer))
      else
        ReadBytes := Stream.Read(Buffer, LeftBytes);
      LeftBytes := LeftBytes - ReadBytes;
      MD4Update(Context, Buffer, ReadBytes);
      FCalcBytes := FCalcBytes + ReadBytes;
      //Synchronize(SynchronizeProgress);
      SynchronizeProgress;
      if (PAbort <> nil) and PAbort^ then //是否停止计算
      begin
        Result := False;
        Exit;
      end;
      if (not AFastCalc) then
      begin
        if GetCurrentThreadID = MainThreadID then
          Application.ProcessMessages;
        Sleep(3);
      end;
    until (ReadBytes = 0) or (LeftBytes <= 0);
    MD4Final(Context,ASegmentHash);
  end;
begin
  Result := True;
  //Synchronize(SynchronizeProgress);
  SynchronizeProgress;
  SavePos := Stream.Position;
  MemStream := TMemoryStream.Create;
  try
    AInfo.FileSize := Stream.Size;
    FFileSize := AInfo.FileSize;
    FCalcBytes := 0;
    ModCount :=0;
    AInfo.SegmentSize := CSegmentSize;
    if AInfo.FileSize <= CSegmentSize then
    begin
      AInfo.SegmentCount := 1;
      AInfo.SegmentSize := AInfo.FileSize;
    end else
    begin
      AInfo.SegmentSize := CSegmentSize;
      ModCount := AInfo.FileSize mod AInfo.SegmentSize;
      AInfo.SegmentCount := AInfo.FileSize div AInfo.SegmentSize;
      if ModCount > 0 then
        Inc(AInfo.SegmentCount);
    end;

    SetLength(AInfo.Segments, AInfo.SegmentCount);
    ZeroMemory(@AInfo.Segments[0], Sizeof(TSegment) * AInfo.SegmentCount);
    for I := 0 to AInfo.SegmentCount - 1 do
    begin
      AInfo.Segments[I].SegmentID := I;
      if (I < AInfo.SegmentCount - 1) or (ModCount = 0) then
      begin
        AInfo.Segments[I].SegmentSize := AInfo.SegmentSize;
        Result := CalcHash(I,AInfo.Segments[I].SegmentHash);
        if not Result then
          Exit;
      end
      else
      begin
        AInfo.Segments[I].SegmentSize := ModCount;
        Result := CalcHash(I,AInfo.Segments[I].SegmentHash);
        if not Result then
          Exit;
      end;
      if (PAbort <> nil) and PAbort^ then //是否停止计算
      begin
        Result := False;
        Exit;
      end;
      if (not AFastCalc) then
      begin
        if GetCurrentThreadID = MainThreadID then
          Application.ProcessMessages;
        Sleep(3);
      end;
      MemStream.WriteBuffer(AInfo.Segments[I].SegmentHash, SizeOf(TMD4Digest));
      FFullHash := FFullHash + MD4DigestToStr(AInfo.Segments[I].SegmentHash);
    end;
    if AInfo.SegmentCount>1 then
      Result := MD4Stream(MemStream, Digest, PAbort, AFastCalc)
    else if AInfo.SegmentCount =1 then
    begin
      Digest := AInfo.Segments[0].SegmentHash;
      Result := True;
    end;
  finally
    Stream.Seek(SavePos, soFromBeginning);
    SetLength(AInfo.Segments, 0);
    MemStream.Free;
  end;
end;

constructor TCalcEd2kHashThread.Create(const AFileName: string;AFastCalc:Boolean);
begin
  {$IFDEF ThreadClass}
  inherited Create(True);
  FreeOnTerminate := True;
  {$ENDIF}
  FFileName := AFileName;
  FFullHash := '';
  FCalcBytes := 0;
  FFastCalc := AFastCalc;
  FStop := False;
  ZeroMemory(@FFileHash,SizeOf(TMD4Digest));
  ZeroMemory(@FWebHash,SizeOf(TMD4Digest));
end;

procedure TCalcEd2kHashThread.DoCalcFinish;
begin
  if Assigned(FOnCalcFinish) then
    FOnCalcFinish(Self);
end;

procedure TCalcEd2kHashThread.Execute;
begin
  CalcHash(FFileName,FFileHash,@FStop,FFastCalc);
  GetWebHash(FFileName,FWebHash);
  DoCalcFinish;
end;

procedure TCalcEd2kHashThread.SynchronizeProgress;
begin
  if Assigned(FOnCalcHashProgress) then
    FOnCalcHashProgress(Self,FCalcBytes,FFileSize,FStop);
end;


end.
