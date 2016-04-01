{
单元名称: uJxdFileShareManage
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明:
开始时间: 2011-04-08
修改时间: 2011-04-08 (最后修改时间)
类说明  :
    管理本地文件的共享，记录文件的HASH，验证文件用到的HASH，
      验证文件HASH的分段大小为 SegmentSize * 38 默认情况，HASH检验分段大小为: 9.5M (256K * 38)
      大部分情况下是不需要用到分段HASH检验的。提供分段HASH检验功能

    内存暂时使用动态申请的，如想优化，可做不定长内存分配处理

    配置保存方案
        文件版本号 共享数量(Integer), 多个以下结构
        begin
          文件名称（word + string)
          文件大小(Int64)
          文件分段大小(Integer)
          Hash检验分段大小(Integer)
          文件Hash( 16 )
          验证分段数量（4）
            分段1HASH
            分段2HASH
        end;

}
unit uJxdFileShareManage;

interface

uses
  Windows, Classes, SysUtils, uJxdHashCalc, uSysSub, uJxdDataStream, uJxdFileSegmentStream, uJxdDataStruct;

const
  CtCalcHashSegmentCount = 32; // * SegmentSize 为一个大分段的HASH验证大小

type
  PFileShareInfo = ^TFileShareInfo;
  TFileShareInfo = record
    FShareID: Cardinal;
    FShareTag: string;
    FFileName: string;
    FFileSize: Int64;
    FSegmentSize: Integer;
    FFileHash, FWebHash: TxdHash;
    FHashCheckSegmentSize: Integer;    //Hash分段的检验大小
    FHashCheckSegmentCount: Integer;   //HASH检验数量
    FHashCheckTable: array[0..0] of TxdHash; //HASH检验表
  end;
  {$M+}
  TxdFileShareManage = class
  public
    constructor Create;
    destructor  Destroy; override;

    function  AddLocalFileToShare(const AFileName: string; const AShareTag: string = ''; const ASegmentSize: Integer = 0): Boolean; //添加一个本地的文件来共享, 不对文件进行判断
    procedure LoopShareFileInfo(ALoop: TOnLoopNode); //遍历共享表中所有内容
    function  FindShareInfoByFileHash(const AHash: TxdHash): PFileShareInfo;
    function  FindShareInfoByWebHash(const AHash: TxdHash): PFileShareInfo;
  private
    FCurFindWebHash: TxdHash;
    FFindShareInfo: PFileShareInfo;
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
    function  GetFileShareMem(const ACheckHashSegmentCount: Integer): PFileShareInfo;
    procedure FreeFileShareMem(Ap: PFileShareInfo);

    procedure DoAddShareFileInfo(const Ap: PFileShareInfo);
    procedure DoFreeShareFileInfo(Ap: PFileShareInfo);

    procedure DoNotifyExistsFile(const Ap: PFileShareInfo); //通知已存在的共享文件
    procedure DoNotifyEmptyHashFile(const Ap: PFileShareInfo); //计算不出HASH的共享文件

    procedure DoLoopSaveShareInfoToFile(const AParamNode: Pointer; Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel: Boolean; var AFindNext: Boolean);
    procedure DoLoopToFreeAllShareInfo(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel: Boolean; var AFindNext: Boolean);
    procedure DoLoopToFindInfoByWebHash(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel: Boolean; var AFindNext: Boolean);
    procedure DoThreadCalcFileHash;

    procedure WaitThreadExit;
  private
    FFileName: string;
    FActive: Boolean;
    FHashTableCount: Integer;
    FMaxShareFileCount: Integer;
    FCurThreadCount: Integer;
    FMinShareFileSize: Integer;
    FCurCalcHashFileName: string;
    procedure SetMaxShareFileCount(const Value: Integer);
    procedure SetFileName(const Value: string);
    procedure SetActive(const Value: Boolean);
    procedure SetHashTableCount(const Value: Integer);
    procedure SetMinShareFileSize(const Value: Integer);
    function GetCurShareFileCount: Integer;
    function GetCurCalcHashCount: Integer;
    function GetShareFileList: THashArrayEx;
  published
    property CurThreadCount: Integer read FCurThreadCount;
    property CurShareFileCount: Integer read GetCurShareFileCount;
    property CurCalcHashCount: Integer read GetCurCalcHashCount;
    property CurCalcHashFileName: string read FCurCalcHashFileName;
    property ShareFileList: THashArrayEx read GetShareFileList;

    property Active: Boolean read FActive write SetActive;
    property MinShareFileSize: Integer read FMinShareFileSize write SetMinShareFileSize;
    property FileName: string read FFileName write SetFileName;
    property HashTableCount: Integer read FHashTableCount write SetHashTableCount;
    property MaxShareFileCount: Integer read FMaxShareFileCount write SetMaxShareFileCount;
  end;
  {$M-}

function  HashToID(const AHash: TxdHash): Cardinal; 

implementation

uses
  uJxdThread, uHashFun;

const
  CtConfigVersion = 100;
  CtFileShareInfoSize = SizeOf(TFileShareInfo);

function HashToID(const AHash: TxdHash): Cardinal;
begin
//  Result := AHash.A + AHash.B + AHash.C + AHash.D;
  Result := HashFun_BKDR( @AHash.v, CtHashSize );
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
    LoadFromFile;
    FActive := True;
  except
    UnActiveShareManage;
  end;
end;

function TxdFileShareManage.AddLocalFileToShare(const AFileName: string; const AShareTag: string; const ASegmentSize: Integer): Boolean;
var
  p: PFileShareInfo;
  nCheckHashSegmentCount: Integer;
  nSegSize: Integer;
  nCalcHashSegmentSize: Cardinal;
  nFileSize: Int64;
begin
  Result := False;
  nFileSize := GetFileSizeEx(AFileName);
  if nFileSize <= MinShareFileSize then Exit;
  if ASegmentSize <= 0 then
    nSegSize := CtSegmentDefaultSize
  else
    nSegSize := ASegmentSize;
  nCalcHashSegmentSize := nSegSize * CtCalcHashSegmentCount;
  if nCalcHashSegmentSize > nFileSize then
    nCalcHashSegmentSize := nFileSize;
  nCheckHashSegmentCount := (nFileSize + nCalcHashSegmentSize - 1) div nCalcHashSegmentSize;
  p := GetFileShareMem( nCheckHashSegmentCount );
  p^.FShareTag := AShareTag;
  p^.FFileName := AFileName;
  p^.FFileSize := nFileSize;
  p^.FSegmentSize := nSegSize;
  p^.FHashCheckSegmentSize := nCalcHashSegmentSize;
  p^.FHashCheckSegmentCount := nCheckHashSegmentCount;

  if p^.FShareTag = '' then
    p^.FShareTag := ExtractFileName( p^.FFileName );

  EnterCriticalSection( FCalcLock );
  try
    FCalcHashList.Add( p );
  finally
    LeaveCriticalSection( FCalcLock );
  end;

  if not FCalcingHash then
    RunningByThread( DoThreadCalcFileHash );
end;

constructor TxdFileShareManage.Create;
begin
  FFileName := '';
  FCurCalcHashFileName := '';
  FActive := False;
  FHashTableCount := 547;
  FMaxShareFileCount := 10000;
  FMinShareFileSize := 100;
  FCalcingHash := False;
  FCloseingManage := False;
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
  bFind, bValidFile: Boolean;
  pF: PFileShareInfo;
begin
  bFind := False;
  bValidFile := False;
  EnterCriticalSection( FShareLock );
  try
    if not Active then
    begin
      //当程序要退出时，先将信息保存到列表中，此时只有文件名称跟分段大小有效。下次启动时再去计算
      FShareFileList.Add( Ap^.FShareID, Ap );
    end
    else
    begin
      if HashCompare(Ap^.FFileHash, CtEmptyHash) then
      begin
        //计算不出HASH的文件断定它无效
        bValidFile := True;
      end
      else
      begin
        pNode := FShareFileList.FindBegin( Ap^.FShareID );
        while Assigned(pNode) do
        begin
          pF := pNode^.NodeData;
          if HashCompare(Ap^.FFileHash, pF^.FFileHash) then
          begin
            bFind := True;
            Break;
          end;
          pNode := FShareFileList.FindNext( pNode );
        end;
        if not bFind then
          FShareFileList.Add( Ap^.FShareID, Ap );
      end;
    end;
  finally
    FShareFileList.FindEnd;
    LeaveCriticalSection( FShareLock );
  end;

  if bValidFile then
  begin
    DoNotifyEmptyHashFile(Ap);
    DoFreeShareFileInfo( Ap );
  end
  else if bFind then
  begin
    //已经存在的节点
    DoNotifyExistsFile( Ap );
    DoFreeShareFileInfo( Ap );
  end;
end;

procedure TxdFileShareManage.DoFreeShareFileInfo(Ap: PFileShareInfo);
begin
//  FreeAndNil( Ap^.FSegmentTable );
//  ReleaseFileSegmentStream( Ap^.FStream );
  FreeFileShareMem( Ap );
end;

procedure TxdFileShareManage.DoLoopSaveShareInfoToFile(const AParamNode: Pointer; Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel,
  AFindNext: Boolean);
var
  f: TxdStreamHandle;
  p: PFileShareInfo;
  i: Integer;
begin
  p := PFileShareInfo(pData);
  f := TxdStreamHandle(AParamNode);
  with f do
  begin
    {
      FShareID: Cardinal;
      FShareTag: string;
      FFileName: string;
      FFileSize: Int64;
      FSegmentSize: Integer;
      FFileHash, FWebHash: TMD4Digest;
      FHashCheckSegmentSize: Integer;
      FHashCheckSegmentCount: Integer;
      FHashCheckTable: array[0..0] of TMD4Digest;
  end;}
    WriteStringEx(p^.FShareTag);
    WriteStringEx(p^.FFileName);
    WriteInt64(p^.FFileSize);
    WriteInteger(p^.FSegmentSize);
    WriteLong(p^.FFileHash, CtHashSize);
    WriteLong(p^.FWebHash, CtHashSize);
    for i := 0 to p^.FHashCheckSegmentCount - 1 do
      WriteLong(p^.FHashCheckTable[i], CtHashSize);
  end;
end;

procedure TxdFileShareManage.DoLoopToFindInfoByWebHash(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel, AFindNext: Boolean);
var
  p: PFileShareInfo;
begin
  p := pData;
  AFindNext := not HashCompare( p^.FWebHash, FCurFindWebHash );
  if not AFindNext then
    FFindShareInfo := p;
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
  i, nSize: Integer;
  buf: PByte;
  Stream: TxdLocalFileStream;
begin
  FCalcingHash := True;
  FCurCalcHashFileName := '';
  InterlockedIncrement( FCurThreadCount );
  try
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

      FCurCalcHashFileName := p^.FFileName;
      //计算本地文件的HASH信息
      Stream := TxdLocalFileStream.Create( p^.FFileName, CtEmptyHash, p^.FSegmentSize );
      try
        if Active and HashCompare(P^.FFileHash, CtEmptyHash) then
          CalcFileHash( Stream, p^.FFileHash, @FCloseingManage );
        if Active and HashCompare(p^.FWebHash, CtEmptyHash) then
          CalcFileWebHash( Stream, p^.FWebHash, @FCloseingManage );
        p^.FShareID := HashToID(p^.FFileHash);

        if Active then
        begin
          if p^.FHashCheckSegmentCount = 1 then
          begin
            p^.FHashCheckTable[0] := p^.FFileHash;
          end
          else
          begin
            GetMem( buf, p^.FHashCheckSegmentSize );
            try
              for i := 0 to p^.FHashCheckSegmentCount - 1 do
              begin
                if HashCompare(CtEmptyHash, p^.FHashCheckTable[i]) then
                begin
                  if i = p^.FHashCheckSegmentCount - 1 then
                    nSize := p^.FFileSize - i * p^.FHashCheckSegmentSize
                  else
                    nSize := p^.FHashCheckSegmentSize;
                  Stream.ReadBuffer( i * p^.FHashCheckSegmentSize, nSize, buf  );
                  p^.FHashCheckTable[i] := HashBuffer( buf, nSize );
                end;
                if not Active then Break;
              end;
            finally
              FreeMem( buf, p^.FHashCheckSegmentSize );
            end;
          end;
        end;
        DoAddShareFileInfo( p );
      finally
        Stream.Free;
      end;
      //结束

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
    FCurCalcHashFileName := '';
  end;
end;

function TxdFileShareManage.FindShareInfoByFileHash(const AHash: TxdHash): PFileShareInfo;
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
      if HashCompare(AHash, p^.FFileHash) then
      begin
        Result := p;
        Break;
      end;
      pNode := FShareFileList.FindNext( pNode );
    end;
  finally
    FShareFileList.FindEnd;
    LeaveCriticalSection( FShareLock );
  end;
end;

function TxdFileShareManage.FindShareInfoByWebHash(const AHash: TxdHash): PFileShareInfo;
begin
  EnterCriticalSection( FShareLock );
  try
    FCurFindWebHash := AHash;
    FFindShareInfo := nil;
    FShareFileList.Loop( DoLoopToFindInfoByWebHash );
    Result := FFindShareInfo;
  finally
    LeaveCriticalSection( FShareLock );
  end;
end;

procedure TxdFileShareManage.FreeFileShareMem(Ap: PFileShareInfo);
begin
  if Assigned(Ap) then
    FreeMem( Ap );
end;

function TxdFileShareManage.GetCurCalcHashCount: Integer;
begin
  if Assigned(FCalcHashList) then
    Result := FCalcHashList.Count
  else
    Result := 0;
end;

function TxdFileShareManage.GetCurShareFileCount: Integer;
begin
  if Assigned(FShareFileList) then
    Result := FShareFileList.Count
  else
    Result := 0;
end;

function TxdFileShareManage.GetFileShareMem(const ACheckHashSegmentCount: Integer): PFileShareInfo;
begin
  if ACheckHashSegmentCount >= 1 then
  begin
    GetMem( Pointer(Result), CtFileShareInfoSize + (ACheckHashSegmentCount - 1) * CtHashSize );
    FillChar( Result^, CtFileShareInfoSize + (ACheckHashSegmentCount - 1) * CtHashSize, 0 );
  end
  else
    Result := nil;
end;

function TxdFileShareManage.GetShareFileList: THashArrayEx;
begin
  EnterCriticalSection( FShareLock );
  Result := FShareFileList;
end;

procedure TxdFileShareManage.LoadFromFile;
var
  f: TxdFileStream;
  p: PFileShareInfo;
  nCheckHashSegmentCount: Integer;
  nSegSize: Integer;
  nCalcHashSegmentSize: Cardinal;
  nFileSize: Int64;
  i, j, nCount: Integer;
  bCalcHash: Boolean;
  strFileName, strTag: string;
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
        {
          WriteStringEx(p^.FShareTag);
          WriteStringEx(p^.FFileName);
          WriteInt64(p^.FFileSize);
          WriteInteger(p^.FSegmentSize);
          WriteLong(p^.FFileHash, CMD4Size);
          WriteLong(p^.FWebHash, CMD4Size);
          for i := 0 to p^.FHashCheckSegmentCount - 1 do
            WriteLong(p^.FHashCheckTable[i], CMD4Size);
      end;}
        strTag := f.ReadStringEx;
        strFileName := f.ReadStringEx;
        nFileSize := f.ReadInt64;
        nSegSize := f.ReadInteger;

        if nSegSize <= 0 then
          nSegSize := CtSegmentDefaultSize;

        if FileExists(strFileName) and (nFileSize <> GetFileSizeEx(strFileName)) then
        begin
          nFileSize := GetFileSizeEx(strFileName);
          if nFileSize <= MinShareFileSize then Continue;
          nSegSize := CtSegmentDefaultSize;
        end;

        nCalcHashSegmentSize := nSegSize * CtCalcHashSegmentCount;
        if nCalcHashSegmentSize > nFileSize then
          nCalcHashSegmentSize := nFileSize;
        nCheckHashSegmentCount := (nFileSize + nCalcHashSegmentSize - 1) div nCalcHashSegmentSize;

        p := GetFileShareMem( nCheckHashSegmentCount );
        p^.FShareTag := strTag;
        p^.FFileName := strFileName;
        p^.FFileSize := nFileSize;
        p^.FSegmentSize := nSegSize;
        p^.FHashCheckSegmentSize := nCalcHashSegmentSize;
        p^.FHashCheckSegmentCount := nCheckHashSegmentCount;
        f.ReadLong( p^.FFileHash, CtHashSize );
        f.ReadLong( p^.FWebHash, CtHashSize );
        bCalcHash := HashCompare(p^.FFileHash, CtEmptyHash);
        for j := 0 to p^.FHashCheckSegmentCount - 1 do
        begin
          f.ReadLong(p^.FHashCheckTable[j], CtHashSize);
          if not bCalcHash then
            bCalcHash := HashCompare(p^.FHashCheckTable[j], CtEmptyHash);
        end;

        if not FileExists(strFileName) then
        begin
          FreeFileShareMem( p );
          Continue;
        end;

        if bCalcHash then
          FCalcHashList.Add(p)
        else
        begin
          p^.FShareID := HashToID(p^.FFileHash);
          FShareFileList.Add( p^.FShareID, p );
        end;
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
  EnterCriticalSection( FShareLock );
  try
    FShareFileList.Loop(ALoop);
  finally
    LeaveCriticalSection( FShareLock );
  end;
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

procedure TxdFileShareManage.SetMinShareFileSize(const Value: Integer);
begin
  if (FMinShareFileSize <> Value) and (Value >= 0) then
     FMinShareFileSize := Value;
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
