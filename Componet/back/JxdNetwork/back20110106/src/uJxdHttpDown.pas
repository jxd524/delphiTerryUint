{
单元名称: uJxdHttpDown
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: 支持多线程，多服务器的HTTP协议下载类
开始时间: 2010-05-26
修改时间: 2010-05-31 (最后修改)

//4K 以下文件不要使用些单元下载，< 20K 的文件也不建议使用此类下载


下载文件结构：

文件数据 nFileSize
Http下载源URL 字串长度 nLen1
SrcURL1
Http下载源URL 字串长度 nLen2
SrcURL2

文件HASH       16
数据源数量     4
已经下载长度   8
文件长度       8
数据源长度     n

使用方法：
    1： 创建对象
    2： 设置保存文件名
    3： 添加HTTP数据源
    4:  开始下载

}
unit uJxdHttpDown;

//{$define JxdDebug}

interface

uses
  {$ifdef JxdDebug}uDebugInfo, {$endIf}
  Classes, SysUtils, IdHTTP, Windows, MD4, uQueues, IdComponent, Forms, uJxdThread;

type
  PHttpSourceInfo = ^THttpSourceInfo;
  THttpSourceInfo = record
    FSrcUrl: string;
    FReferenceUrl: string;
    FCount: Integer;   //<0: 无验证HASH，需要验证HASH之后才能使用
    FUsedCount: Integer;
  end;
  THttpData = class;

  TOnHttpSourceInfo = procedure(Sender: TObject; const Ap: PHttpSourceInfo) of object;
  TOnReadedBuffer = procedure(Sender: TObject; const ApBuffer: PChar; const ABeginPos: Int64; const ABufferLen: Integer) of object;

  TJxdHttpDownState = ( dsNULL, dsBeginForCheckFile, dsTransmiting, dsComplete, dsError, dsPause, dsStop, dsExit );
{$M+}
  TxdHttpDown = class
  public
    procedure AddHttpSource(const ASrcUrl, AReferenceUrl: string);
    function  GetHttpSourceInfo(const AIndex: Integer; var ASrcUrl, AReferUrl: string): Boolean;
    procedure Pause;
    constructor Create;
    destructor  Destroy; override;
  protected
    procedure DoThread_CheckHttpDownInfo;
    procedure DoThread_CheckHttpSource;
    procedure DoThread_DowningFile;

    procedure DoBeginDownFile;
    procedure DoDownFinished;
    procedure DoStopDownFile;
    function  DoGetHttpSource(var Ap: PHttpSourceInfo): Boolean;
    procedure DoFreeHttpSource(const Ap: PHttpSourceInfo; const AIsSuccess: Boolean);
    procedure DoInvalidationHttpSource(const Ap: PHttpSourceInfo);

    function  GetUrlFileSize(const AUrl, AReferUrl: string): Int64;
    function  GetUrlFileHash(const AFileSize: Int64; const AUrl, AReferUrl: string; var AHash: TMD4Digest): Boolean;
    procedure ChangedHttpDownState(const AState: TJxdHttpDownState);
    procedure WriteDataToFile(ApBuf: PChar; const ABufferLen: Integer; const ABeginPos: Int64);
    procedure DoError(const AErrorInfo: string);
    procedure DoAddHttpSource(var p: PHttpSourceInfo);
    procedure DoDownFileThreadTerminate(Sender: TObject);
  private
    procedure ActiveHttpDown;
    procedure UnActiveHttpDown;
    procedure LoadDownInfo;
    procedure SaveDownInfo;
    procedure FreeList(var AList: TList; const AIsFreeList: Boolean = True);
    procedure LockHttpSourceList(const ALock: Boolean);
    procedure LockFileStream(const ALock: Boolean);
    procedure AddCurSize(const ALen: Integer);
    procedure DoNotifyEvent(const AEvent: TNotifyEvent);
  private
    FActive: Boolean;
    FState: TJxdHttpDownState;
    FHttpSourceList: TList;
    FCurWriteFileSize, FCurSize, FFileSize: Int64;
    FSaveFileName: string;
    FCheckFileHash: TMD4Digest;
    FDownFileThreadCount: Integer;
    FDownFileThreadList: TList;
    FHttpDownInfo: THttpData;
    FOnDownFinished: TNotifyEvent;
    FSaveFileStream: TFileStream;
    FFileStreamLock, FThreadLock,
    FSourceListLock, FStatLock: TRTLCriticalSection;
    FActiveTime: Cardinal;
    FOnDelInvaliSource: TOnHttpSourceInfo;
    FIsWaitCheckHttpSource: Boolean;
    FCurDownSize: Int64;
    FOnBeginDown: TNotifyEvent;
    FOnReadedBuffer: TOnReadedBuffer;
    FTaskID: Integer;

    procedure SetActive(const Value: Boolean);
    procedure SetSaveFileName(const Value: string);
    function  GetHttpSourceCount: Integer;
    function  GetThreadCount: Integer;
    function  GetAverageSpeed: string;
    procedure SetThreadCount(const Value: Integer);
  published
    property Active: Boolean read FActive write SetActive;
    property SaveFileName: string read FSaveFileName write SetSaveFileName;
    //信息统计
    property ActiveTime: Cardinal read FActiveTime;
    property CurWriteFileSize: Int64 read FCurWriteFileSize;
    property CurSize: Int64 read FCurSize;
    property CurDownSize: Int64 read FCurDownSize;
    property CurState: TJxdHttpDownState read FState;
    property CurThreadCount: Integer read GetThreadCount write SetThreadCount;
    property AverageSpeed: string read GetAverageSpeed;
    property FileSize: Int64 read FFileSize;
    property HttpSourceCount: Integer read GetHttpSourceCount;
    property TaskID: Integer read FTaskID write FTaskID;
    //事件
    property OnBeginDown: TNotifyEvent read FOnBeginDown write FOnBeginDown;
    property OnReadedBuffer: TOnReadedBuffer read FOnReadedBuffer write FOnReadedBuffer; 
    property OnDownFinished: TNotifyEvent read FOnDownFinished write FOnDownFinished; //下载完成
    property OnDeleteInvalidationHttpSource: TOnHttpSourceInfo read FOnDelInvaliSource write FOnDelInvaliSource;
  end;
{$M-}




  PHttpDataInfo = ^THttpDataInfo;
  THttpDataInfo = record
    FBeginPos: Int64;
    FIndex: Integer;
    FLen: Integer;
    FBuf: PChar;
  end;
  THttpData = class
  public
    function  GetHttpDataToWrite(var p: PHttpDataInfo): Boolean;
    procedure SetHttpDataToMemory(const p: PHttpDataInfo; const AIsSuccess: Boolean);

    constructor Create(AOwner: TxdHttpDown);
    destructor  Destroy; override;
  private
    procedure DoCheckHttpData;
    procedure WriteDataToMemory;
    procedure ReclaimData(const p: PHttpDataInfo);
    procedure LockBufferManage(const AIsLock: Boolean);
    procedure LockSuccessList(const AIsLock: Boolean);
    procedure LockFailList(const AIsLock: Boolean);
  private
    FLockBufferManage: TRTLCriticalSection;
    FOwner: TxdHttpDown;
    FBufferCount: Integer;
    FLastIndex: Integer;
    FCurWriteIndex: Integer;
    FBufferManage: TStaticMemoryManager;
    FSuccessList, FFailList: TList;
    FSuccessListLock, FFailLisLock: TRTLCriticalSection;
  end;


implementation

{ TJxdHttpDown }
const
  CtMaxBufferLen = 1024 * 32;
  CtMaxBufferCount = 128;  //内存大小总为 CtMaxBufferCount * CtMaxBufferLen, 当前为 4M 的内存缓存
  CtWriteMomeryToFileSize = 1024 * 1024 * 2; //2M写一次文件

  CtFileSizeInfo = SizeOf( Int64 );
{$ifdef JxdDebug}
  CtDebugFileName = 'JxdHttpDown_Debug.txt';
{$endIf}

procedure LogJxdHttpDownDebug(const AErrorInfo: string);
begin
  OutputDebugString( PChar(AErrorInfo) );
  {$ifdef JxdDebug}
  _Log( AErrorInfo, CtDebugFileName );
  {$endIf}
end;

function ListSort(Item1, Item2: Pointer): Integer;
var
  p1, p2: PHttpDataInfo;
begin
  p1 := Item1;
  p2 := Item2;
  Result := p1^.FIndex - p2^.FIndex;
end;


procedure TxdHttpDown.ActiveHttpDown;
begin
  if FActive then
  begin
    if FState = dsPause then
      ChangedHttpDownState( dsTransmiting );
    Exit;
  end;
  if not Assigned(FSaveFileStream) then
  begin
    if FSaveFileName <> '' then
      SaveFileName := FSaveFileName;

    if not Assigned(FSaveFileStream) then
    begin
      ChangedHttpDownState( dsError );
      DoError( 'please set the file name for save the http data' );
      Exit;
    end;
  end;
  FActive := True;
  FActiveTime := GetTickCount;
  RunningByThread( DoThread_CheckHttpDownInfo );
end;

procedure TxdHttpDown.AddCurSize(const ALen: Integer);
begin
  EnterCriticalSection( FStatLock );
  try
    Inc( FCurSize, ALen );
    Inc( FCurDownSize, ALen );
  finally
    LeaveCriticalSection( FStatLock );
  end;
end;

procedure TxdHttpDown.AddHttpSource(const ASrcUrl, AReferenceUrl: string);
var
  p: PHttpSourceInfo;
begin
  if not Assigned(FSaveFileStream) then
  begin
    ChangedHttpDownState( dsError );
    DoError( 'Please set the property SaveFileName first!' );
    Exit;
  end;
  if ASrcUrl = '' then
  begin
    DoError( 'http source url can not be empty!' );
    Exit;
  end;
  New( p );
  p^.FCount := -1;
  p^.FSrcUrl := ASrcUrl;
  p^.FReferenceUrl := AReferenceUrl;
  p^.FUsedCount := 0;
  DoAddHttpSource( p );
end;

procedure TxdHttpDown.ChangedHttpDownState(const AState: TJxdHttpDownState);
begin
  LogJxdHttpDownDebug( '更改状态' );
  FState := AState;
  case FState of
    dsNULL: ;
    dsBeginForCheckFile: ;
    dsTransmiting: ;
    dsComplete: ;
    dsError: Active := False;
    dsPause: ;
    dsStop: ;
  end;
end;

constructor TxdHttpDown.Create;
begin
  inherited;
  FActive              := False;
  FCurWriteFileSize    := 0;
  FCurSize             := 0;
  FFileSize            := 0;
  FSaveFileName        := '';
  FState               := dsNULL;
  FHttpSourceList      := nil;
  FCheckFileHash       := CEmptyMD4;
  FDownFileThreadCount := 1;
  FHttpDownInfo        := nil;
  FSaveFileStream      := nil;
  FActiveTime          := 0;
  FHttpSourceList      := TList.Create;
  FCurDownSize         := 0;
  InitializeCriticalSection( FSourceListLock );
  InitializeCriticalSection( FFileStreamLock );
  InitializeCriticalSection( FThreadLock );
  InitializeCriticalSection( FStatLock );
end;

destructor TxdHttpDown.Destroy;
begin
  if Active then
  begin
    Active := False;
    ChangedHttpDownState( dsExit );
    while FDownFileThreadList.Count <> 0 do
    begin
      Application.ProcessMessages;
      Sleep( 50 );
    end;
  end;

  if FState <> dsComplete then
    SaveDownInfo;
  FreeList( FHttpSourceList );
  FreeAndNil( FSaveFileStream );
  DeleteCriticalSection( FSourceListLock );
  DeleteCriticalSection( FFileStreamLock );
  DeleteCriticalSection( FThreadLock );
  DeleteCriticalSection( FStatLock );
  inherited;
end;

procedure TxdHttpDown.DoAddHttpSource(var p: PHttpSourceInfo);
var
  pInfo: PHttpSourceInfo;
  i: Integer;
  bOK: Boolean;
begin
  LockHttpSourceList( True );
  try
    for i := 0 to FHttpSourceList.Count - 1 do
    begin
      pInfo := FHttpSourceList[i];
      if CompareText(pInfo^.FSrcUrl, p^.FSrcUrl) = 0 then
      begin
        bOK := False;
        Dispose( p );
        Exit;
      end;
    end;
    bOK := FHttpSourceList.Add( p ) <> -1;
  finally
    LockHttpSourceList( False );
  end;
  if Active and bOK  and (not FIsWaitCheckHttpSource) and (p^.FCount < 0) then
    RunningByThread( DoThread_CheckHttpSource );
end;

procedure TxdHttpDown.DoBeginDownFile;
var
  i: Integer;
  ThreadObj: TRunOneThread;
begin
  ChangedHttpDownState( dsTransmiting );
  FSaveFileStream.Size := FileSize;
  FCurDownSize := 0;
  FIsWaitCheckHttpSource := True;
  TRunOneThread.Create( DoThread_CheckHttpSource );
  if not Assigned(FHttpDownInfo) then
    FHttpDownInfo := THttpData.Create( Self );
  if Assigned(FDownFileThreadList) then
  begin
    for i := FDownFileThreadList.Count - 1 downto 0 do
      TRunOneThread(FDownFileThreadList[i]).Free;
    FDownFileThreadList.Clear;
  end
  else
    FDownFileThreadList := TList.Create;

  for i := 0 to FDownFileThreadCount - 1 do
  begin
    ThreadObj := TRunOneThread.Create( DoThread_DowningFile );
    ThreadObj.OnTerminate := DoDownFileThreadTerminate;
    FDownFileThreadList.Add( ThreadObj );
  end;
  DoNotifyEvent( OnBeginDown );
end;

procedure TxdHttpDown.DoDownFinished;
begin
  DoError( '下载完成' );
  FSaveFileStream.Size := FFileSize;
  ChangedHttpDownState( dsComplete );
  DoStopDownFile;
  if Assigned(OnDownFinished) then
    OnDownFinished( Self );
  Active := False;
end;

procedure TxdHttpDown.DoError(const AErrorInfo: string);
begin
  LogJxdHttpDownDebug(AErrorInfo);
end;

procedure TxdHttpDown.DoFreeHttpSource(const Ap: PHttpSourceInfo; const AIsSuccess: Boolean);
begin
  LockHttpSourceList( True );
  try
    if AIsSuccess then
      Inc( Ap^.FCount );
  finally
    LockHttpSourceList( False );
  end;
end;

function TxdHttpDown.DoGetHttpSource(var Ap: PHttpSourceInfo): Boolean;
var
  i: Integer;
  p, pMaxCount, pOk: PHttpSourceInfo;
begin
  pMaxCount := nil;
  pOk := nil;
  Ap := nil;
  LockHttpSourceList( True );
  try
    for i := 0 to FHttpSourceList.Count - 1 do
    begin
      p := FHttpSourceList[i];
      if p^.FCount < 0 then Continue;
      if pMaxCount = nil then
        pMaxCount := p;
      if pMaxCount^.FCount < p^.FCount then
        pMaxCount := p;

      if pOK = nil then
        pOk := p;
      if p^.FUsedCount < pOK^.FUsedCount then
        pOk := p;
    end;
    if pOk <> nil then
    begin
      Inc( pOk^.FUsedCount );
      Inc( pOk^.FCount );
      Ap := pOK;
    end
    else if pMaxCount <> nil then
    begin
      Inc( pMaxCount^.FUsedCount );
      Inc( pMaxCount^.FCount );
      Ap := pMaxCount;
    end;
  finally
    LockHttpSourceList( False );
  end;
  Result := Ap <> nil;
end;

procedure TxdHttpDown.DoInvalidationHttpSource(const Ap: PHttpSourceInfo);
begin
  LogJxdHttpDownDebug( Format('Invalidation Http Source. Delete: %s', [Ap^.FSrcUrl]) );
  if Assigned(OnDeleteInvalidationHttpSource) then
    OnDeleteInvalidationHttpSource( Self, Ap );
end;

procedure TxdHttpDown.DoNotifyEvent(const AEvent: TNotifyEvent);
begin
  if Assigned(AEvent) then
    AEvent( Self );
end;

procedure TxdHttpDown.DoStopDownFile;
begin
  if (FState <> dsComplete) and (FState <> dsError) then
  begin
    FHttpDownInfo.WriteDataToMemory;
    SaveDownInfo;
  end
  else
    FSaveFileStream.Size := FFileSize;
  FreeList( FHttpSourceList, False );
  FreeAndNil( FHttpDownInfo );
  FreeAndNil( FSaveFileStream );
end;

procedure TxdHttpDown.DoDownFileThreadTerminate(Sender: TObject);
var
  bFreeMem: Boolean;
  nIndex: Integer;
begin
  EnterCriticalSection( FThreadLock );
  try
    nIndex := FDownFileThreadList.IndexOf( Sender as TRunOneThread );
    if nIndex <> -1 then
      FDownFileThreadList.Delete( nIndex )
    else
      DoError( 'can not find the thread object in FDownFileThread''list ' );
    bFreeMem := FDownFileThreadList.Count = 0;
  finally
    LeaveCriticalSection( FThreadLock );
  end;
  if bFreeMem then
  begin
    case FState of
      dsNULL: ;
      dsBeginForCheckFile: ;
      dsTransmiting: ;
      dsComplete: DoDownFinished;
      dsError: ;
      dsPause: ;
      dsStop: DoStopDownFile;
    end;
    FActive := False;
  end;
end;

procedure TxdHttpDown.DoThread_CheckHttpDownInfo;
var
  p: PHttpSourceInfo;
  nFileSize: Int64;
  i: Integer;
  mdHash: TMD4Digest;
begin
  ChangedHttpDownState( dsBeginForCheckFile );
  if FHttpSourceList.Count = 0 then
  begin
    ChangedHttpDownState( dsError );
    DoError( 'Empty http source' );
    Exit;
  end;
  for i := 0 to FHttpSourceList.Count - 1 do
  begin
    p := FHttpSourceList[i];
    nFileSize := GetUrlFileSize( p^.FSrcUrl, p^.FReferenceUrl );
    if nFileSize = 0 then
    begin
      p^.FSrcUrl := '';
      Continue;
    end;
    if FFileSize = 0 then
    begin
      FFileSize := nFileSize;
      FCurWriteFileSize := 0;
    end;
    if FFileSize <> nFileSize then
    begin
      if FileExists(FSaveFileName) then
        DeleteFile( PChar(FSaveFileName) );
      FFileSize := nFileSize;
      FCurWriteFileSize := 0;
      FCheckFileHash := CEmptyMD4;
    end;

    if not GetUrlFileHash(nFileSize, p^.FSrcUrl, p^.FReferenceUrl, mdHash) then
    begin
      FFileSize := 0;
      FCurWriteFileSize := 0;
      p^.FSrcUrl := '';
      Continue;
    end;
    if EmptyMD4(FCheckFileHash) then
    begin
      FCheckFileHash := mdHash;
      p^.FCount := 0;    //the url is success! Not need to check again in other thread
      Break;
    end;
    if not MD4DigestCompare(mdHash, FCheckFileHash) then
    begin
      FFileSize := 0;
      FCurWriteFileSize := 0;
      p^.FSrcUrl := '';
      Continue;
    end;
    Break;
  end;

  if (FFileSize <> 0) and (not EmptyMD4(FCheckFileHash)) then
    DoBeginDownFile
  else
  begin
    ChangedHttpDownState( dsError );    
    DoError( 'can''t find usefull http source!' );
  end;
end;

procedure TxdHttpDown.DoThread_CheckHttpSource;
  procedure DeleteHttpSource(const p: PHttpSourceInfo);
  var
    nIndex: Integer;
  begin
    LockHttpSourceList( True );
    try
      nIndex := FHttpSourceList.IndexOf( p );
      if nIndex <> -1 then
        FHttpSourceList.Delete( nIndex );
    finally
      LockHttpSourceList( False );
    end;
  end;
var
  i: Integer;
  p: PHttpSourceInfo;
  TempList: TList;
  nFileSize: Int64;
  mdHash: TMD4Digest;
begin
  if (FFileSize = 0) or EmptyMD4(FCheckFileHash) then Exit;
  FIsWaitCheckHttpSource := True;
  Sleep( 1000 ); //wait one sencend for
  FIsWaitCheckHttpSource := False;
  TempList := nil;
  //Http resources that need to check and to add them to the temporary list
  LockHttpSourceList( True );
  try
    for i := FHttpSourceList.Count - 1 downto 0 do
    begin
      p := FHttpSourceList[i];
      if not Assigned(p) then
      begin
        FHttpSourceList.Delete( i );
        Continue;
      end;
      if p^.FSrcUrl = '' then
      begin
        Dispose( p );
        FHttpSourceList.Delete( i );
        Continue;
      end;
      if p^.FCount < 0 then
      begin
        if not Assigned(TempList) then
          TempList := TList.Create;
        TempList.Add( p );
      end;
    end;
  finally
    LockHttpSourceList( False );
  end;

  //need to check the http source has been stored in the temporary list
  if Assigned(TempList) then
  begin
    for i := TempList.Count - 1 downto 0 do
    begin
      p := TempList[i];
      nFileSize := GetUrlFileSize( p^.FSrcUrl, p^.FReferenceUrl );
      if nFileSize <> FFileSize then
      begin
        DoInvalidationHttpSource( p );
        DeleteHttpSource( p );
        Dispose( p );
        Continue;
      end;
      if (not GetUrlFileHash( nFileSize, p^.FSrcUrl, p^.FReferenceUrl, mdHash)) or
         (not MD4DigestCompare(FCheckFileHash, mdHash)) then
      begin
        DoInvalidationHttpSource( p );
        DeleteHttpSource( p );
        Dispose( p );
        Continue;
      end;
      LockHttpSourceList( True );
      try
        p^.FCount := 0;
      finally
        LockHttpSourceList( False );
      end;
    end;
    TempList.Free;
  end;
  SaveDownInfo;
end;

procedure TxdHttpDown.DoThread_DowningFile;
var
  pHttpData: PHttpDataInfo;
  pHttpSource: PHttpSourceInfo;
  http: TIdHTTP;
  ms: TMemoryStream;
  bOK: Boolean;
begin
  http := TIdHTTP.Create( nil );
  ms := TMemoryStream.Create;
  try
    ms.Size := CtMaxBufferLen;
    while FState in [dsTransmiting, dsPause] do
    begin
      if FState = dsPause then
      begin
        FHttpDownInfo.WriteDataToMemory;
        Sleep( 100 );
        Continue;
      end;
      if not FHttpDownInfo.GetHttpDataToWrite( pHttpData ) then
      begin
        DoError( 'can not get http data to down file' );
        FHttpDownInfo.WriteDataToMemory;
        Break;
      end;
      if not DoGetHttpSource( pHttpSource ) then
      begin
        DoError( 'can not get http source to down file' );
        Break;
      end;

      http.Request.Clear;
      http.Request.Referer := pHttpSource^.FReferenceUrl;
      http.Request.ContentRangeStart := pHttpData^.FBeginPos;
      http.Request.ContentRangeEnd := pHttpData^.FBeginPos + pHttpData^.FLen - 1;
      try
        ms.Position := 0;
        http.Get( pHttpSource^.FSrcUrl, ms );
        if FState = dsExit then Exit;        
        bOK := (http.Response.ContentLength = pHttpData^.FLen) and (ms.Position = pHttpData^.FLen);
        if bOk then
        begin
          Move( ms.Memory^, pHttpData^.FBuf^, pHttpData^.FLen );
          if Assigned(OnReadedBuffer) then
            OnReadedBuffer( Self, pHttpData^.FBuf, pHttpData^.FBeginPos, pHttpData^.FLen ); 
          AddCurSize( pHttpData^.FLen );
        end;
      except
        bOk := False;
      end;
      if not bOK then
        DoError( format('Dowing file occur error, ContentRangeStart: %d, ContentRangeEnd: %d',
                         [http.Request.ContentRangeStart, http.Request.ContentRangeEnd] ) );
      http.Disconnect;
      FHttpDownInfo.SetHttpDataToMemory( pHttpData, bOK );
      DoFreeHttpSource( pHttpSource, bOK );
      if not bOK then
      begin
        FreeAndNil( http );
        http := TIdHTTP.Create( nil );
      end;
    end;
    FHttpDownInfo.WriteDataToMemory;
  finally
    FActive := False;
    FreeAndNil( ms );
    FreeAndNil( http );
  end;
end;

procedure TxdHttpDown.FreeList(var AList: TList; const AIsFreeList: Boolean);
var
  i: Integer;
begin
  if not Assigned(AList) then Exit;  
  for i := 0 to AList.Count - 1 do
    Dispose( AList[i] );
  if AIsFreeList then
    FreeAndNil( AList )
  else
    AList.Clear;
end;

function TxdHttpDown.GetUrlFileHash(const AFileSize: Int64; const AUrl, AReferUrl: string; var AHash: TMD4Digest): Boolean;
var
  http: TIdHTTP;
  ms: TMemoryStream;
  nSize: Integer;
const
  CtK = 1024;
begin
  Result := False;
  nSize := 4 * CtK;
  http := TIdHTTP.Create( nil );
  ms := TMemoryStream.Create;
  try
    ms.Size := nSize;
    ms.Position := 0;
    http.Request.Referer := AReferUrl;

    //头   1K
    //尾   1K
    //中间 2K
    http.Request.ContentRangeStart := 0;
    http.Request.ContentRangeEnd   := CtK - 1;
    try
      http.Get( AUrl, ms );
    except
      DoError( Format('Can not get data to calc hash which url is "%s", and rangeStart: %d, rangeEnd: %d. FileSize: %d',
                      [AUrl, http.Request.ContentRangeStart, http.Request.ContentRangeEnd, AFileSize]) );
      Exit;
    end;

    http.Request.ContentRangeStart := AFileSize - CtK;
    http.Request.ContentRangeEnd   := http.Request.ContentRangeStart + CtK - 1;
    try
      http.Get( AUrl, ms );
    except
      DoError( Format('Can not get data to calc hash which url is "%s", and rangeStart: %d, rangeEnd: %d. FileSize: %d',
                      [AUrl, http.Request.ContentRangeStart, http.Request.ContentRangeEnd, AFileSize]) );
      Exit;
    end;

    http.Request.ContentRangeStart := (AFileSize - 2 * CtK) div 2;
    http.Request.ContentRangeEnd   := http.Request.ContentRangeStart + 2 * CtK - 1;
    try
      http.Get( AUrl, ms );
    except
      DoError( Format('Can not get data to calc hash which url is "%s", and rangeStart: %d, rangeEnd: %d. FileSize: %d',
                      [AUrl, http.Request.ContentRangeStart, http.Request.ContentRangeEnd, AFileSize]) );
      Exit;
    end;

    MD4Stream( ms, AHash );
    Result := True;
  finally
    FreeAndNil( http );
    FreeAndNil( ms );
  end;
end;

function TxdHttpDown.GetAverageSpeed: string;
var
  dwTime, n: Double;
  bSize: Int64;
const
  CtMB = 1024 * 1024;
  CtK  = 1024;
begin
  if (not Active) or (dsPause = FState) then
  begin
    Result := '';
    Exit;
  end;
  dwTime := (GetTickCount - ActiveTime) / 1000;
  bSize := CurDownSize;

  n := bSize div CtMB / dwTime;
  if n >= 1.0 then
    Result := Format( '%0.2f MB/S', [n] )
  else
  begin
    n := bSize div CtK / dwTime;
    if n >= 1.0 then
      Result := Format( '%0.2f KB/S', [n] )
    else
      Result := Format( '%0.2f B/S', [bSize / dwTime] );
  end;
end;

function TxdHttpDown.GetHttpSourceCount: Integer;
begin
  Result := FHttpSourceList.Count;
end;

function TxdHttpDown.GetHttpSourceInfo(const AIndex: Integer; var ASrcUrl, AReferUrl: string): Boolean;
var
  p: PHttpSourceInfo;
begin
  Result := False;
  if (AIndex >= 0) and (AIndex < FHttpSourceList.Count) then
  begin
    try
      p := FHttpSourceList[AIndex];
      ASrcUrl := p^.FSrcUrl;
      AReferUrl := p^.FReferenceUrl;
      Result := True;
    except
    end;
  end;  
end;

function TxdHttpDown.GetThreadCount: Integer;
begin
  Result := 0;
  if Assigned(FDownFileThreadList) then
    Result := FDownFileThreadList.Count;
end;

function TxdHttpDown.GetUrlFileSize(const AUrl, AReferUrl: string): Int64;
var
  http: TIdHTTP;
begin
  Result := 0;
  http := TIdHTTP.Create( nil );
  try
    http.Request.Referer := AReferUrl;
    try
      http.Head( AUrl );
    except
      FState := dsError;
      DoError( 'DoThread_CheckHttpDownInfo: can''t send http command: Head. which URL is ' + AUrl );
      Exit;
    end;
    Result := http.Response.ContentLength;
    http.Disconnect;
  finally
    FreeAndNil( http );
  end;
end;

procedure TxdHttpDown.LoadDownInfo;
var
  nSize: Int64;
  i, nLen, nCount, nHttpSourceLen: Integer;
  p: PHttpSourceInfo;
  pTemp: array[0..1024 * 128] of Char;
const
  CtMinLen = CtFileSizeInfo * 2 + SizeOf(nCount) * 2 + Sizeof(FCheckFileHash);
begin
  if not Assigned(FSaveFileStream) then Exit;
  nSize := FSaveFileStream.Size;
  nLen := CtMinLen;
  if nSize <= nLen then Exit;
  FSaveFileStream.Position := nSize - nLen;

{
  FSaveFileStream.WriteBuffer( FCheckFileHash, SizeOf(FCheckFileHash) );
  FSaveFileStream.WriteBuffer( nCount, SizeOf(nCount) );
  FSaveFileStream.WriteBuffer( FCurSize, CtFileSizeInfo );
  FSaveFileStream.WriteBuffer( FFileSize, CtFileSizeInfo );
  FSaveFileStream.WriteBuffer( nHttpSourceLen, CtFileSizeInfo );
}

  FSaveFileStream.ReadBuffer( FCheckFileHash, SizeOf(FCheckFileHash) );
  FSaveFileStream.ReadBuffer( nCount, SizeOf(nCount) );
  FSaveFileStream.ReadBuffer( FCurWriteFileSize, CtFileSizeInfo );
  FSaveFileStream.ReadBuffer( FFileSize, CtFileSizeInfo );
  FSaveFileStream.ReadBuffer( nHttpSourceLen, SizeOf(nHttpSourceLen) );

  if nSize - FFileSize - nLen <> nHttpSourceLen then
  begin
    FCurWriteFileSize := 0;
    FFileSize := 0;
    FCheckFileHash := CEmptyMD4;
    Exit;
  end;
  FCurSize := FCurWriteFileSize;
  FSaveFileStream.Position := FFileSize;
  LockHttpSourceList( True );
  try
    for i := 0 to nCount - 1 do
    begin
      New( p );
      p^.FUsedCount := 0;

      //Src URL
      FSaveFileStream.ReadBuffer( nLen, SizeOf(nLen) );
      FSaveFileStream.ReadBuffer( pTemp[0], nLen );
      pTemp[nLen] := #0;
      p^.FSrcUrl := pTemp;

      //Reference URL
      FSaveFileStream.ReadBuffer( nLen, SizeOf(nLen) );
      if nLen > 0 then
      begin
        FSaveFileStream.ReadBuffer( pTemp[0], nLen );
        pTemp[nLen] := #0;
        p^.FReferenceUrl := pTemp;
      end;

      //Count
      FSaveFileStream.ReadBuffer( p^.FCount, SizeOf(p^.FCount) );
      DoAddHttpSource( p );
    end;
  finally
    LockHttpSourceList( False );
  end;
end;

procedure TxdHttpDown.LockFileStream(const ALock: Boolean);
begin
  if ALock then
    EnterCriticalSection( FFileStreamLock )
  else
    LeaveCriticalSection( FFileStreamLock );
end;

procedure TxdHttpDown.LockHttpSourceList(const ALock: Boolean);
begin
  if ALock then
    EnterCriticalSection( FSourceListLock )
  else
    LeaveCriticalSection( FSourceListLock );
end;

procedure TxdHttpDown.Pause;
begin
  if Active then
    ChangedHttpDownState( dsPause ); 
end;

procedure TxdHttpDown.SaveDownInfo;
var
  nLen, nHttpSourceLen: Integer;
  i, nCount: Integer;
  p: PHttpSourceInfo;
begin
  if FFileSize = 0 then
  begin
    DoError( 'File size if zero, can not save the file down info' );
    ChangedHttpDownState( dsError );
    Exit;
  end;
  if not Assigned(FSaveFileStream) then
  begin
    DoError( 'not open file stream,can not save the file down info' );
    ChangedHttpDownState( dsError );
    Exit;
  end;
  LockFileStream( True );
  LockHttpSourceList( True );
  try
    FSaveFileStream.Size := FFileSize;
    FSaveFileStream.Position := FFileSize;
    //
    nCount := 0;
    nHttpSourceLen := 0;
    for i := 0 to FHttpSourceList.Count - 1 do
    begin
      p := FHttpSourceList[i];
      if p^.FSrcUrl = '' then Continue;
      //Source URL
      nLen := Length( p^.FSrcUrl );
      FSaveFileStream.WriteBuffer( nLen, SizeOf(nLen) );
      FSaveFileStream.WriteBuffer( p^.FSrcUrl[1], nLen );
      Inc( nHttpSourceLen, nLen + SizeOf(nLen) );
      //Reference URL
      nLen := Length( p^.FReferenceUrl );
      FSaveFileStream.WriteBuffer( nLen, SizeOf(nLen) );
      Inc( nHttpSourceLen, SizeOf(nLen) );
      if nLen > 0 then
      begin
        FSaveFileStream.WriteBuffer( p^.FReferenceUrl[1], nLen );
        Inc( nHttpSourceLen, nLen );
      end;

      //Is Check FileHash
      FSaveFileStream.WriteBuffer( p^.FCount, SizeOf(p^.FCount) );
      Inc( nHttpSourceLen, SizeOf(p^.FCount) );

      Inc( nCount );
    end;
    FSaveFileStream.WriteBuffer( FCheckFileHash, SizeOf(FCheckFileHash) );
    FSaveFileStream.WriteBuffer( nCount, SizeOf(nCount) );
    FSaveFileStream.WriteBuffer( FCurWriteFileSize, CtFileSizeInfo );
    FSaveFileStream.WriteBuffer( FFileSize, CtFileSizeInfo );
    FSaveFileStream.WriteBuffer( nHttpSourceLen, SizeOf(nHttpSourceLen) );
    FSaveFileStream.Size := FSaveFileStream.Position;
  finally
    LockHttpSourceList( False );
    LockFileStream( False );
  end;
end;

procedure TxdHttpDown.SetActive(const Value: Boolean);
var
  nMax: Integer;
begin
  if Value then
    ActiveHttpDown
  else
  begin
    UnActiveHttpDown;
    nMax := 100;
    while FActive and (nMax > 0) do
    begin
      Application.ProcessMessages;
      Sleep( 10 );
      Dec( nMax );
    end;
  end;
end;


procedure TxdHttpDown.SetSaveFileName(const Value: string);
var
  fsTemp: TFileStream;
  bOK: Boolean;
begin
  //the file open with share read
  if not Active then
  begin
    fsTemp := nil;
    try
      if FileExists(Value) then
        fsTemp := TFileStream.Create( Value, fmOpenReadWrite or fmShareDenyNone )
      else
      begin
        fsTemp := TFileStream.Create( Value, fmCreate or fmShareDenyNone );
        FreeList( FHttpSourceList, False );
      end;
      bOK := True;
    except
      bOK := False;
    end;
    if bOK then
    begin
      if Assigned(FSaveFileStream) then
        FreeAndNil( FSaveFileStream );
      FSaveFileStream := fsTemp;
      FSaveFileName := FSaveFileStream.FileName;
      LoadDownInfo;
    end;
  end;
end;

procedure TxdHttpDown.SetThreadCount(const Value: Integer);
begin
  if (not Active) and (FDownFileThreadCount <> Value) and (Value > 0) then
    FDownFileThreadCount := Value;
end;

procedure TxdHttpDown.UnActiveHttpDown;
begin
  ChangedHttpDownState( dsStop );
end;

procedure TxdHttpDown.WriteDataToFile(ApBuf: PChar; const ABufferLen: Integer; const ABeginPos: Int64);
begin
  if not Assigned(FSaveFileStream) then
  begin
    DoError( 'FileStream is not exsits.Can not write buffer to file' );
    Exit;
  end;
  LockFileStream( True );
  try
    if FSaveFileStream.Size < ABeginPos + ABufferLen then
      FSaveFileStream.Size := ABeginPos + ABufferLen;
    FSaveFileStream.Position := ABeginPos;
    FSaveFileStream.WriteBuffer( ApBuf^, ABufferLen );
    Inc( FCurWriteFileSize, ABufferLen );
    if FCurWriteFileSize = FFileSize then
      ChangedHttpDownState( dsComplete );
  finally
    LockFileStream( False );
  end;
end;

{ THttpData }

constructor THttpData.Create(AOwner: TxdHttpDown);
var
  i, nBlockSize: Integer;
  p: PHttpDataInfo;
  nSize, nTemp: Int64;
begin
  FOwner := AOwner;
  InitializeCriticalSection( FLockBufferManage );
  InitializeCriticalSection( FSuccessListLock );
  InitializeCriticalSection( FFailLisLock );
  FSuccessList := TList.Create;
  FFailList := TList.Create;

  FCurWriteIndex := FOwner.CurWriteFileSize div CtMaxBufferLen;
  FLastIndex := FCurWriteIndex;
  nBlockSize := SizeOf(THttpDataInfo) - SizeOf(PChar) + CtMaxBufferLen;
  nSize := FOwner.FileSize - FOwner.CurWriteFileSize;
  nTemp := (nSize + CtMaxBufferLen - 1) div CtMaxBufferLen;
  if nTemp > CtMaxBufferCount then
    FBufferCount := CtMaxBufferCount
  else
    FBufferCount := nTemp;
  nSize := FOwner.FileSize;
  FBufferManage := TStaticMemoryManager.Create( nBlockSize, FBufferCount );
  for i := 0 to FBufferCount - 1 do
  begin
    FBufferManage.GetMem( Pointer(p) );
    p^.FIndex := FLastIndex;
    p^.FBeginPos := p^.FIndex * CtMaxBufferLen;
    if i = FBufferCount - 1 then
    begin
      if nSize - p^.FBeginPos > CtMaxBufferLen then
        p^.FLen := CtMaxBufferLen
      else
        p^.FLen := nSize - p^.FBeginPos;
    end
    else
      p^.FLen := CtMaxBufferLen;
    p^.FBuf := PChar( Integer(p) +  SizeOf(THttpDataInfo) - SizeOf(PChar) );
    FBufferManage.FreeMem( Pointer(p) );
    Inc( FLastIndex );
  end;
end;

destructor THttpData.Destroy;
begin
  FreeAndNil( FSuccessList );
  FreeAndNil( FFailList );
  FBufferManage.Free;
  DeleteCriticalSection( FLockBufferManage );
  DeleteCriticalSection( FSuccessListLock );
  DeleteCriticalSection( FFailLisLock );
  inherited;
end;

procedure THttpData.DoCheckHttpData;
begin
  if FSuccessList.Count >= 64 then
    WriteDataToMemory;
end;

function THttpData.GetHttpDataToWrite(var p: PHttpDataInfo): Boolean;
begin
  if FFailList.Count > 0 then
  begin
    LockFailList( True );
    try
      p := FFailList[0];
      FFailList.Delete( 0 );
      Result := True;
      Exit;
    finally
      LockFailList( False );
    end;
  end;

  LockBufferManage( True );
  try
    Result := FBufferManage.GetMem( Pointer(p) );
  finally
    LockBufferManage( False );
  end;
end;

procedure THttpData.LockBufferManage(const AIsLock: Boolean);
begin
  if AIsLock then
    EnterCriticalSection( FLockBufferManage )
  else
    LeaveCriticalSection( FLockBufferManage );
end;

procedure THttpData.LockFailList(const AIsLock: Boolean);
begin
  if AIsLock then
    EnterCriticalSection( FFailLisLock )
  else
    LeaveCriticalSection( FFailLisLock );
end;

procedure THttpData.LockSuccessList(const AIsLock: Boolean);
begin
  if AIsLock then
    EnterCriticalSection( FSuccessListLock )
  else
    LeaveCriticalSection( FSuccessListLock );
end;

procedure THttpData.ReclaimData(const p: PHttpDataInfo);
var
  nLen, nPos: Int64;
begin
  nLen := FOwner.FileSize - FLastIndex * CtMaxBufferLen;
  if nLen > 0 then
  begin
    if nLen > CtMaxBufferLen then
    begin
      nLen := CtMaxBufferLen;
      nPos := FLastIndex * CtMaxBufferLen;
    end
    else
    begin
      nPos := FOwner.FileSize - nLen;
    end;
    p^.FIndex := FLastIndex;
    p^.FBeginPos := nPos;
    p^.FLen := nLen;
    Inc( FLastIndex );
    LockBufferManage( True );
    try
      FBufferManage.FreeMem( Pointer(p) );
    finally
      LockBufferManage( False );
    end;
  end; 
end;

procedure THttpData.SetHttpDataToMemory(const p: PHttpDataInfo; const AIsSuccess: Boolean);
begin
  if AIsSuccess then
  begin
//    LogJxdHttpDownDebug( Format('SetHttpDataToMemory: Success; BeginPos: %d, Len: %d', [p^.FBeginPos, p^.FLen]) );
    LockSuccessList( True );
    try
      FSuccessList.Add( p );
    finally
      LockSuccessList( False );
    end;
  end
  else
  begin
    LogJxdHttpDownDebug( Format('SetHttpDataToMemory: Fail; BeginPos: %d, Len: %d', [p^.FBeginPos, p^.FLen]) );
    LockFailList( True );
    try
      FFailList.Add( p );
    finally
      LockFailList( False );
    end;
  end;
  DoCheckHttpData;
end;

procedure THttpData.WriteDataToMemory;
var
  p: PHttpDataInfo;
  pBuf, pTemp: PChar;
  nLen, nMaxBufLen: Integer;
  nBeginPos: Int64;
begin
  nBeginPos := -1;
  LockSuccessList( True );
  try
    if FSuccessList.Count = 0 then Exit;
    FSuccessList.Sort( ListSort );
    if FSuccessList.Count * CtMaxBufferLen > CtWriteMomeryToFileSize then
      nMaxBufLen := CtWriteMomeryToFileSize
    else
      nMaxBufLen := FSuccessList.Count * CtMaxBufferLen;
    GetMem( pBuf, nMaxBufLen );
    pTemp := pBuf;
    nLen := 0;
    while FSuccessList.Count <> 0 do
    begin
      p := FSuccessList[0];
      if p^.FIndex = FCurWriteIndex then
      begin
        if nBeginPos = -1 then
          nBeginPos := p^.FBeginPos;
        if nLen + p^.FLen > nMaxBufLen then
        begin
          FOwner.WriteDataToFile( pBuf, nLen, nBeginPos );
          pTemp := pBuf;
          nLen := 0;
          nBeginPos := p^.FBeginPos;
        end;
        Move( p^.FBuf^, pTemp^, p^.FLen );
        Inc( nLen, p^.FLen );
        Inc( pTemp, p^.FLen );

        Inc( FCurWriteIndex );
        ReclaimData( p );
        FSuccessList.Delete( 0 );
      end
      else
        Break;
    end;
  finally
    LockSuccessList( False );
  end;
  if (nBeginPos <> -1) and (pBuf <> nil) then
    FOwner.WriteDataToFile( pBuf, nLen, nBeginPos );
  if pBuf <> nil then
    FreeMem( pBuf );
end;


end.
