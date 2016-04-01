{
单元名称: uJxdFileShareManage
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明:
开始时间: 2011-04-08
修改时间: 2011-04-08 (最后修改时间)
类说明  :
    管理本地文件的共享，记录文件的HASH，验证文件用到的HASH，分9.28M为一个验证单位 跟emule的分块大小保持一样

    配置保存方案
        共享数量(Integer), 多个以下结构
        begin
          文件名称（word + string)
          文件大小(Int64)
          文件分段大小(Integer)
          文件Hash( 16 )
          if Assigned(TSegmentState) then
            标志（1）
            分段1状态(TSegmentState),
            if 分段1状态 = ssWaitToCheckHash, ssCheckingHash, ssCompleted then
              SegmentHash(16)
            else
              BlockState1(array[0..FBlockCount-1] of TBlockState)
          else
            标志(0)
        end;

}
unit uJxdFileShareManage;

interface

{$DEFINE ServerFileShareManage}

uses
  Windows, Classes, SysUtils, MD4, uSysSub, uJxdDataStream, uJxdFileSegmentStream, uJxdDataStruct
  {$IFDEF ServerFileShareManage}
  , uJxdMemoryManage
  {$ENDIF}
  ;

type
  PFileShareInfo = ^TFileShareInfo;
  TFileShareInfo = record
    FShareID: Cardinal;
    FName: string;
    FSize: Int64;
    FSegmentSize: Integer;
    FHash: TMD4Digest;
    FSegmentTable: TxdFileSegmentTable;
    FStream: TxdFileSegmentStream;
    FLastStreamActiveTime: Cardinal;
  end;
  {$M+}
  TxdFileShareManage = class
  public
    constructor Create;
    destructor  Destroy; override;

    function  AddLocalFileToShare(const AFileName: string; const ASegmentSize: Integer = 0): Boolean; //添加一个本地的文件来共享, 不对文件进行判断
    procedure LoopShareFileInfo(ALoop: TOnLoopNode); //遍历共享表中所有内容
    function  FindSegmentTable(const AHash: TMD4Digest): TxdFileSegmentStream;
  private
    FCloseingManage: Boolean;
    FCalcLock: TRTLCriticalSection;
    FShareLock: TRTLCriticalSection;
    FCalcingHash: Boolean;
    FCalcHashList: TList;
    FShareFileList: THashArrayEx;
    procedure ActiveShareManage;
    procedure UnActiveShareManage;
    procedure LoadFromFile;
    procedure SaveToFile;
  protected
    function  GetShareInfoMen: PFileShareInfo;
    procedure FreeShareInfoMem(var Ap: PFileShareInfo);

    procedure DoAddShareFileInfo(const Ap: PFileShareInfo);
    procedure DoFreeShareFileInfo(Ap: PFileShareInfo);

    procedure DoNotifyExistsFile(const Ap: PFileShareInfo); //通知已存在的共享文件
    procedure DoNotifyEmptyHashFile(const Ap: PFileShareInfo); //计算不出HASH的共享文件

    procedure DoLoopSaveShareInfoToFile(const AParamNode: Pointer; Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel: Boolean; var AFindNext: Boolean);
    procedure DoLoopToFreeAllShareInfo(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel: Boolean; var AFindNext: Boolean);
    procedure DoThreadCalcFileHash;

    procedure WaitThreadExit;
    function  CalcSegmentTable(Ap: PFileShareInfo; AStream: THandleStream): Boolean;

  {$IFDEF ServerFileShareManage}
  private
    FShareFileMen: TxdFixedMemoryManager;
  {$ENDIF}

  private
    FFileName: string;
    FActive: Boolean;
    FHashTableCount: Integer;
    FMaxShareFileCount: Integer;
    FAutoCreateFileSegmentTable: Boolean;
    FCurThreadCount: Integer;
    procedure SetMaxShareFileCount(const Value: Integer);
    procedure SetFileName(const Value: string);
    procedure SetActive(const Value: Boolean);
    procedure SetHashTableCount(const Value: Integer);
    procedure SetAutoCreateFileSegmentTable(const Value: Boolean);
  published
    property Active: Boolean read FActive write SetActive;
    property CurThreadCount: Integer read FCurThreadCount;
    property AutoCreateFileSegmentTable: Boolean read FAutoCreateFileSegmentTable write SetAutoCreateFileSegmentTable;
    property FileName: string read FFileName write SetFileName;
    property HashTableCount: Integer read FHashTableCount write SetHashTableCount;
    property MaxShareFileCount: Integer read FMaxShareFileCount write SetMaxShareFileCount;
  end;
  {$M-}

function  HashToID(const AHash: TMD4Digest): Cardinal; inline;

implementation

uses
  uJxdThread;

const
  CtConfigVersion = 100;
  CtFileShareInfoSize = SizeOf(TFileShareInfo);

function HashToID(const AHash: TMD4Digest): Cardinal;
begin
  Result := AHash.A + AHash.B + AHash.C + AHash.D;
end;

{ TxdFileShareManage }

procedure TxdFileShareManage.ActiveShareManage;
begin
  try
    FCurThreadCount := 0;
    FCalcHashList := TList.Create;
    FShareFileList := THashArrayEx.Create;
    FShareFileList.MaxHashNodeCount := MaxShareFileCount;
    FShareFileList.HashTableCount := HashTableCount;
    FShareFileList.Active := True;
    {$IFDEF ServerFileShareManage}
    FShareFileMen := TxdFixedMemoryManager.Create( CtFileShareInfoSize, MaxShareFileCount );
    FCalcHashList.Capacity := 1024 * 2;
    {$ENDIF}
    LoadFromFile;
    FActive := True;
  except
    UnActiveShareManage;
  end;
end;

function TxdFileShareManage.AddLocalFileToShare(const AFileName: string; const ASegmentSize: Integer): Boolean;
var
  p: PFileShareInfo;
begin
  Result := False;
  p := GetShareInfoMen;
  if not Assigned(p) then Exit;
  p^.FName := AFileName;
  p^.FSegmentSize := ASegmentSize;
  EnterCriticalSection( FCalcLock );
  try
    FCalcHashList.Add( p );
  finally
    LeaveCriticalSection( FCalcLock );
  end;

  if not FCalcingHash then
    RunningByThread( DoThreadCalcFileHash );
end;

function TxdFileShareManage.CalcSegmentTable(Ap: PFileShareInfo; AStream: THandleStream): Boolean;
var
  hFile, nSegmentSize: Cardinal;
  pBuf: PByte;
  i: Integer;
  pSeg: PSegmentInfo;
  bCreateStream: Boolean;
begin
  Result := True;
  bCreateStream := not Assigned(AStream);
  if bCreateStream then
  begin
    hFile := CreateFile(PChar(Ap^.FName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
    if hFile = INVALID_HANDLE_VALUE then
    begin
      Result := False;
      Exit;
    end;
    AStream := TFileStream.Create(hFile);
  end;
  Ap^.FShareID := HashToID( Ap^.FHash );
  Ap^.FSegmentTable := TxdFileSegmentTable.Create( Ap^.FSize, Ap^.FSegmentSize );
  Ap^.FSegmentSize := Ap^.FSegmentTable.SegmentSize;
  nSegmentSize := Ap^.FSegmentTable.SegmentSize;
  GetMem( pBuf, nSegmentSize );
  try
    for i := 0 to Ap^.FSegmentTable.SegmentCount - 1 do
    begin
      pSeg := Ap^.FSegmentTable.SegmentList[i];
      pSeg^.FSegmentState := ssCompleted;
      AStream.Position := pSeg^.FSegmentBeginPos;
      AStream.ReadBuffer( pBuf^, pSeg^.FSegmentSize );
      pSeg^.FSegmentHash := MD4Buffer( pBuf^, pSeg^.FSegmentSize );
      pSeg^.FRecvBlockCount := pSeg^.FBlockCount;
    end;
  finally
    if Assigned(pBuf) then
      FreeMem( pBuf, nSegmentSize );
    if bCreateStream then
      AStream.Free;
  end;
end;

constructor TxdFileShareManage.Create;
begin
  FFileName := '';
  FActive := False;
  FHashTableCount := 547;
  FMaxShareFileCount := 10000;
  FCalcingHash := False;
  FCloseingManage := False;
  FAutoCreateFileSegmentTable := True;
  InitializeCriticalSection( FCalcLock );
  InitializeCriticalSection( FShareLock );
end;

destructor TxdFileShareManage.Destroy;
begin
  Active := False;
  DeleteCriticalSection( FCalcLock );
  DeleteCriticalSection( FShareLock );
  inherited;
end;

procedure TxdFileShareManage.DoAddShareFileInfo(const Ap: PFileShareInfo);
var
  pNode: PHashNode;
  bFind: Boolean;
  pF: PFileShareInfo;
begin
  bFind := False;
  EnterCriticalSection( FShareLock );
  try
    if not Active then
    begin
      //当程序要退出时，先将信息保存到列表中，此时只有文件名称跟分段大小有效。下次启动时再去计算
      FShareFileList.Add( Ap^.FShareID, Ap );
    end
    else
    begin
      if MD4DigestCompare(Ap^.FHash, CEmptyMD4) then
      begin
        //计算不出HASH的文件断定它无效
        DoNotifyEmptyHashFile(Ap);
        DoFreeShareFileInfo( Ap );
      end
      else
      begin
        pNode := FShareFileList.FindBegin( Ap^.FShareID );
        while Assigned(pNode) do
        begin
          pF := pNode^.NodeData;
          if MD4DigestCompare(Ap^.FHash, pF^.FHash) then
          begin
            bFind := True;
            Break;
          end;
          pNode := FShareFileList.FindNext( pNode );
        end;
        if not bFind then
        begin
          Ap^.FShareID := HashToID( Ap^.FHash );
          FShareFileList.Add( Ap^.FShareID, Ap );
        end;
      end;
    end;
  finally
    FShareFileList.FindEnd;
    LeaveCriticalSection( FShareLock );
  end;
  
  if bFind then
  begin
    //已经存在的节点
    DoNotifyExistsFile( Ap );
    DoFreeShareFileInfo( Ap );
  end;
end;

procedure TxdFileShareManage.DoFreeShareFileInfo(Ap: PFileShareInfo);
begin
  FreeAndNil( Ap^.FSegmentTable );
  ReleaseFileSegmentStream( Ap^.FStream );
  FreeShareInfoMem( Ap );
end;

procedure TxdFileShareManage.DoLoopSaveShareInfoToFile(const AParamNode: Pointer; Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel,
  AFindNext: Boolean);
var
  f: TxdStreamHandle;
  p: PFileShareInfo;
  i, j: Integer;
  pSeg: PSegmentInfo;
begin
  p := PFileShareInfo(pData);
  f := TxdStreamHandle(AParamNode);
  with f do
  begin
    {
      FShareID: Cardinal;
      FName: string;
      FSize: Int64;
      FSegmentSize: Integer;
      FHash: TMD4Digest;
      FSegmentTable: TxdFileSegmentTable;
  end;}
    WriteCardinal(p^.FShareID);
    WriteStringEx(p^.FName);
    WriteInt64(p^.FSize);
    if Assigned(p^.FSegmentTable) then
      WriteInteger(p^.FSegmentTable.SegmentSize)
    else
      WriteInteger(p^.FSegmentSize);
    WriteLong(p^.FHash, CMD4Size);
    if Assigned(p^.FSegmentTable) then
    begin
      WriteByte(1);
      for i := 0 to p^.FSegmentTable.SegmentCount - 1 do
      begin
        pSeg := p^.FSegmentTable.SegmentList[i];
        WriteByte( Byte(pSeg^.FSegmentState) );
        if pSeg^.FSegmentState in [ssWaitToCheckHash, ssCheckingHash, ssCompleted] then
          WriteLong(pSeg^.FSegmentHash, CMD4Size)
        else
        begin
          WriteInteger(pSeg^.FBlockCount);
          for j := 0 to pSeg^.FBlockCount - 1 do
            WriteByte( Byte(pSeg^.FBlockState[j]) );
        end;
      end;
    end
    else
      WriteByte(0);
  end;
end;

procedure TxdFileShareManage.DoLoopToFreeAllShareInfo(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel,
  AFindNext: Boolean);
var
  p: PFileShareInfo;
begin
  ADel := True;
  AFindNext := True;
  p := pData;
  DoFreeShareFileInfo( p );
end;

procedure TxdFileShareManage.DoNotifyEmptyHashFile(const Ap: PFileShareInfo);
begin
  OutputDebugString( '计算不出HASH的共享文件' );
end;

procedure TxdFileShareManage.DoNotifyExistsFile(const Ap: PFileShareInfo);
begin
  OutputDebugString( '已存在的共享文件' );
end;

procedure TxdFileShareManage.DoThreadCalcFileHash;
var
  p: PFileShareInfo;
  hFile: THandle;
  Stream: TFileStream;
  i: Integer;
begin
  FCalcingHash := True;
  InterlockedIncrement( FCurThreadCount );
  try
//    Sleep(100);
//    Sleep(100);
//    Sleep(100);
//    Sleep(100);
    Sleep(100);
    while FCalcHashList.Count > 0 do
    begin
      EnterCriticalSection( FCalcLock );
      try
        p := FCalcHashList[ FCalcHashList.Count - 1 ];
        FCalcHashList.Delete( FCalcHashList.Count - 1 );
      finally
        LeaveCriticalSection( FCalcLock );
      end;

      hFile := CreateFile(PChar(p^.FName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
      if hFile = INVALID_HANDLE_VALUE then Break;

      Stream := TFileStream.Create(hFile);
      try
        p^.FSize := Stream.Size;
        MD4Stream( Stream, p^.FHash, @FCloseingManage );
        p^.FShareID := HashToID(p^.FHash);

        if Active and AutoCreateFileSegmentTable then
          CalcSegmentTable( p, Stream );

        DoAddShareFileInfo( p );
      finally
        Stream.Free;
      end;

      if not Active then
      begin
        EnterCriticalSection( FCalcLock );
        try
          for i := FCalcHashList.Count - 1 downto 0 do
          begin
            p := FCalcHashList[i];
            FCalcHashList.Delete( i );
            p^.FShareID := 0;
            DoAddShareFileInfo( p );
          end;
        finally
          LeaveCriticalSection( FCalcLock );
        end;
        Break;
      end;
    end;
  finally
    InterlockedDecrement( FCurThreadCount );
    FCalcingHash := False;
  end;
end;

function TxdFileShareManage.FindSegmentTable(const AHash: TMD4Digest): TxdFileSegmentStream;
var
  pNode: PHashNode;
  nID: Cardinal;
  p: PFileShareInfo;
begin
  Result := nil;
  nID := HashToID(AHash);
  EnterCriticalSection( FShareLock );
  try
    pNode := FShareFileList.FindBegin( nID );
    while Assigned(pNode) do
    begin
      p := pNode^.NodeData;
      if MD4DigestCompare(AHash, p^.FHash) then
      begin

        Break;
      end;
      pNode := FShareFileList.FindNext( pNode );
    end;
  finally
    FShareFileList.FindEnd;
    LeaveCriticalSection( FShareLock );
  end;
end;

procedure TxdFileShareManage.FreeShareInfoMem(var Ap: PFileShareInfo);
begin
  {$IFDEF ServerFileShareManage}
    FillChar( Ap^, CtFileShareInfoSize, 0 );
    FShareFileMen.FreeMem( Ap );
  {$ELSE}
    FreeMem( Ap, CtFileShareInfoSize );
  {$ENDIF}
end;

function TxdFileShareManage.GetShareInfoMen: PFileShareInfo;
begin
  {$IFDEF ServerFileShareManage}
    FShareFileMen.GetMem( Pointer(Result) );
  {$ELSE}
    GetMem( Pointer(Result), CtFileShareInfoSize );
    FillChar( Result^, CtFileShareInfoSize, 0 );
  {$ENDIF}
end;

procedure TxdFileShareManage.LoadFromFile;
var
  f: TxdFileStream;
  i, j, k, nCount: Integer;
  p: PFileShareInfo;
  pSeg: PSegmentInfo;
  bFlat: Byte;
begin
  if FileExists(FFileName) then
  begin
    f := TxdFileStream.Create(FFileName, fmOpenRead);
    try
      if CtConfigVersion <> f.ReadInteger then
      begin
        OutputDebugString( '共享文件版本号出错' );
        Exit;
      end;
      nCount := f.ReadInteger;
      for i := 0 to nCount - 1 do
      begin
        p := GetShareInfoMen;
        try
          p^.FShareID := f.ReadCardinal;
          p^.FName := f.ReadStringEx;
          p^.FSize := f.ReadInt64;
          p^.FSegmentSize := f.ReadInteger;
          f.ReadLong( p^.FHash, CMD4Size );
          bFlat := f.ReadByte;
          if bFlat = 1 then
          begin
            p^.FSegmentTable := TxdFileSegmentTable.Create( p^.FSize, p^.FSegmentSize );
            for j := 0 to p^.FSegmentTable.SegmentCount - 1 do
            begin
              pSeg := p^.FSegmentTable.SegmentList[j];
              pSeg^.FSegmentState := TSegmentState(f.ReadByte);
              if pSeg^.FSegmentState in [ssWaitToCheckHash, ssCheckingHash, ssCompleted] then
              begin
                f.ReadLong(pSeg^.FSegmentHash, CMD4Size);
                pSeg^.FRecvBlockCount := pSeg^.FBlockCount;
              end
              else
              begin
                pSeg^.FBlockCount := f.ReadInteger;
                for k := 0 to pSeg^.FBlockCount - 1 do
                begin
                  pSeg^.FBlockState[j] := TBlockState(f.ReadByte);
                  if pSeg^.FBlockState[j] = bsWaitReply then
                    pSeg^.FBlockState[j] := bsEmpty
                  else if pSeg^.FBlockState[j] = bsComplete then
                    Inc(pSeg^.FRecvBlockCount);
                end;
              end;
            end;
          end
          else
            p^.FSegmentTable := nil;
        except
          FreeShareInfoMem( p );
          Exit;
        end;
        if MD4DigestCompare(p^.FHash, CEmptyMD4) then
          FCalcHashList.Add(p)
        else
          FShareFileList.Add( p^.FShareID, p );
      end;
    finally
      f.Free;
    end;

    if FCalcHashList.Count <> 0 then
      RunningByThread( DoThreadCalcFileHash );
  end;
end;

procedure TxdFileShareManage.LoopShareFileInfo(ALoop: TOnLoopNode);
begin
  FShareFileList.Loop(ALoop);
end;

procedure TxdFileShareManage.SaveToFile;
var
  f: TxdFileStream;
begin
  f := TxdFileStream.Create( FFileName, fmCreate );
  try
    f.WriteInteger( CtConfigVersion );
    f.WriteInteger( FShareFileList.Count );
    FShareFileList.Loop( DoLoopSaveShareInfoToFile, Pointer(f) );
  finally
    f.Free;
  end;
end;

procedure TxdFileShareManage.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      ActiveShareManage
    else
      UnActiveShareManage;
  end;
end;

procedure TxdFileShareManage.SetAutoCreateFileSegmentTable(const Value: Boolean);
begin
  if FAutoCreateFileSegmentTable <> Value then
    FAutoCreateFileSegmentTable := Value;
end;

procedure TxdFileShareManage.SetFileName(const Value: string);
var
  strPath: string;
begin
  strPath := ExtractFilePath(Value);
  if not DirectoryExists(strPath) then
    if not ForceDirectories(strPath) then Exit;

  FFileName := Value;
end;

procedure TxdFileShareManage.SetHashTableCount(const Value: Integer);
begin
  if not Active then
    FHashTableCount := Value;
end;

procedure TxdFileShareManage.SetMaxShareFileCount(const Value: Integer);
begin
  if not Active then
    FMaxShareFileCount := Value;
end;

procedure TxdFileShareManage.UnActiveShareManage;
begin
  FCloseingManage := True;
  FActive := False;
  WaitThreadExit;
  SaveToFile;
  FShareFileList.Loop( DoLoopToFreeAllShareInfo );
  FreeAndNil(FCalcHashList);
  FreeAndNil( FShareFileList );
  {$IFDEF ServerFileShareManage}
  FreeAndNil( FShareFileMen );
  {$ENDIF}
end;

procedure TxdFileShareManage.WaitThreadExit;
var
  nMaxWaitCount: Integer;
begin
  nMaxWaitCount := 1000000;

  while (FCurThreadCount > 0) and (nMaxWaitCount > 0) do
  begin
    Sleep(10);
    Dec( nMaxWaitCount );
  end;
end;

end.
