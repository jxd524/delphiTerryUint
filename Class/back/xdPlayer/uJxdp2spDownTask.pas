unit uJxdp2spDownTask;

interface

uses
  Windows, Classes, SysUtils, Forms, uJxdThread, uJxdDataStream, idHttp,
  uJxdFileBlock, uJxdAsyncFileStream, uJxdPlayerConsts;

type
  PHttpDownInfo = ^THttpDownInfo;
  THttpDownInfo = record
    FURL: string[255];
    FReferURL: string[255];
  end;

  TxdDownTask = class
  public
    constructor Create;
    destructor  Destroy; override;

    procedure SetDownFileInfo(const AFileName: string; const AFileSize: Int64 = 0);
    function  AddHttpSource(const AURL: string; const AReferURL: string = ''): Boolean;

    function  FindHttpSource(const AURL: string): Boolean;
    function  HttpSourceCount: Integer;
    function  HttpSourceItem(const AIndex: Integer; var AURL, ARefer: string): Boolean;

    class function GetFileSizeByHttp(const AURL: string): Int64;
  private
    FFileStream: TxdAsyncFileStream;
    FHttpDownInfoList: TList;
    FActiveHttpThreadCount: Integer;
    FBeginDowing: Boolean;

    procedure ActiveDownTask;
    procedure UnActiveDownTask;
    function  CheckDownInfo: Boolean;
    procedure DoErrorInfo(const AInfo: string);
    procedure DoDownFinished;
    procedure DoDownFail;

    //线程开始启动
    procedure DoThreadActive;
    //使用HTTP方式下载
    procedure DoThreadHttpDown;
    function  DoGetURL(var AURL, AReferURL: string): Boolean;
    function  DoGetDownInfo(var APosition: Int64; var ADataLen: Cardinal): Boolean;
  private
    FActive: Boolean;
    FFileName: string;
    FFileSize: Int64;
    FFileStreamID: Cardinal;
    FHttpThreadCount: Integer;
    FTaskID: Cardinal;
    FOnDownSuccess: TNotifyEvent;
    FOnDownFail: TNotifyEvent;
    FHttpMaxTryCount: Integer;
    FData: Pointer;
    FWaitCompleteActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetHttpThreadCount(const Value: Integer);
    procedure SetHttpMaxTryCount(const Value: Integer);
  public
    property Active: Boolean read FActive write SetActive;
    property WaitCompleteActive: Boolean read FWaitCompleteActive write FWaitCompleteActive;
    property FileName: string read FFileName;
    property FileSize: Int64 read FFileSize;
    property FileStreamID: Cardinal read FFileStreamID;
    property ActiveHttpThreadCount: Integer read FActiveHttpThreadCount;
    property HttpThreadCount: Integer read FHttpThreadCount write SetHttpThreadCount;
    property HttpMaxTryCount: Integer read FHttpMaxTryCount write SetHttpMaxTryCount;
    property TaskID: Cardinal read FTaskID write FTaskID;
    property Data: Pointer read FData write FData;

    property OnDownSuccess: TNotifyEvent read FOnDownSuccess write FOnDownSuccess;
    property OnDownFail: TNotifyEvent read FOnDownFail write FOnDownFail;
  end;

implementation

{ TxdDownTask }

procedure TxdDownTask.ActiveDownTask;
begin
  FBeginDowing := False;
  FActive := True;
  RunningByThread( DoThreadActive );
  if WaitCompleteActive then
  begin
    while not FBeginDowing do
    begin
      Sleep( 10 );
      Application.ProcessMessages;
    end;
  end;
end;

function TxdDownTask.AddHttpSource(const AURL, AReferURL: string): Boolean;
var
  p: PHttpDownInfo;
begin
  Result := FindHttpSource( AURL );
  if Result then
  begin
    Result := False;
    Exit;
  end;
  New( p );
  p^.FURL := AURL;
  p^.FReferURL := AReferURL;
  FHttpDownInfoList.Add( p );
  Result := True;
end;

function TxdDownTask.CheckDownInfo: Boolean;
var
  strURL, strRefer: string;
begin
  Result := False;
  FFileStreamID := 0;
  FFileStream := nil;
  //总线程开始，创建或判断要下载文件的相关信息，是否需要网络验证
  if not FileExists(FFileName) then
  begin
    //文件不存在时
    if FFileSize = 0 then
    begin
      if DoGetURL(strURL, strRefer) then
        FFileSize := GetFileSizeByHttp( strURL );
    end;
    if FFileSize = 0 then Exit;
    Result := TxdAsyncFileStream.CreateFileStream( FFileName, FFileSize, GetSegmentSize(FFileName), FFileStream, FFileStreamID );
  end
  else
  begin
    //断点下载, 存在配置文件的可断点下载，否则重新下载
    Result := TxdAsyncFileStream.CreateFileStream( FFileName, FFileSize, GetSegmentSize(FFileName), FFileStream, FFileStreamID );
    
    if not Result then
    begin
      if FFileSize = 0 then
      begin
        if DoGetURL(strURL, strRefer) then
          FFileSize := GetFileSizeByHttp( strURL );
      end;
      if FFileSize = 0 then Exit;
      Result := TxdAsyncFileStream.CreateFileStream( FFileName, FFileSize, GetSegmentSize(FFileName), FFileStream, FFileStreamID );
    end;
  end;
end;

constructor TxdDownTask.Create;
begin
  FFileStream := nil;
  FFileName := '';
  FHttpDownInfoList := TList.Create;
  FFileStreamID := 0;
  FHttpThreadCount := 2;
  FHttpMaxTryCount := 10;
  WaitCompleteActive := False;
end;

destructor TxdDownTask.Destroy;
var
  i: Integer;
begin
  Active := False;
  for i := 0 to FHttpDownInfoList.Count - 1 do
    Dispose( PHttpDownInfo(FHttpDownInfoList[i]) );
  FHttpDownInfoList.Free;
  inherited;
end;

procedure TxdDownTask.DoDownFail;
begin
  if Assigned(OnDownFail) then
    OnDownFail( Self );
  if Assigned(FFileStream) then
  begin
    TxdAsyncFileStream.ReleaseFileStream( FFileStream );
    FFileStream := nil;
  end;
end;

procedure TxdDownTask.DoDownFinished;
begin
  if Assigned(FFileStream) and FFileStream.IsFileCompleted then
  begin
    OutputDebugString( '文件下载完成' );
    if Assigned(OnDownSuccess) then
      OnDownSuccess( Self );
  end
  else
  begin
    OutputDebugString( '文件未下载完成' );
  end;
  if Assigned(FFileStream) then
  begin
    TxdAsyncFileStream.ReleaseFileStream( FFileStream );
    FFileStream := nil;
  end;
end;

procedure TxdDownTask.DoErrorInfo(const AInfo: string);
begin
  Dbg( AInfo );
end;

function TxdDownTask.DoGetDownInfo(var APosition: Int64; var ADataLen: Cardinal): Boolean;
var
  p: PSegmentBlockInfo;
begin
  Result := False;
  if Assigned(FFileStream) then
  begin
    p := FFileStream.GetEmptySegmentInfo;
    Result := p <> nil;
    if Result then
    begin
      APosition := p^.BeginPosition;
      ADataLen := p^.SegmentSize;
    end;
  end;
end;

function TxdDownTask.DoGetURL(var AURL, AReferURL: string): Boolean;
var
  p: PHttpDownInfo;
begin
  p := FHttpDownInfoList[0];
  Result := True;
  AURL := p^.FURL;
  AReferURL := p^.FReferURL;
end;

procedure TxdDownTask.DoThreadActive;
var
  i: Integer;
begin
  FActiveHttpThreadCount := 0;
  if not CheckDownInfo then
  begin
    FActiveHttpThreadCount := 0;
    UnActiveDownTask;
    DoDownFail;
    Exit;
  end;
  //开始下载
  FBeginDowing := True;
  for i := 0 to HttpThreadCount - 2 do
    RunningByThread( DoThreadHttpDown );
  DoThreadHttpDown;
end;

procedure TxdDownTask.DoThreadHttpDown;
var
  http: TIdHTTP;
  ms: TMemoryStream;
  strURL, strRefer: string;
  nDataLen: Cardinal;
  nPosition: Int64;
  bOK: Boolean;
  nMaxTryCount: Integer;
begin
  if not Assigned(FFileStream) then Exit;
  InterlockedIncrement( FActiveHttpThreadCount );

  nMaxTryCount := FHttpMaxTryCount;
  http := TIdHTTP.Create( nil );
  ms := TMemoryStream.Create;
  try
    ms.Size := FFileStream.BlockSize;
    bOK := True;
    while True do
    begin
      if not DoGetURL(strURL, strRefer) then
      begin
        DoErrorInfo( '无可用HTTP源' );
        Break;
      end;

      if bOK then
        if not DoGetDownInfo(nPosition, nDataLen) then Break;

      with http do
      begin
        Request.Clear;
        Request.Referer := strRefer;
        Request.ContentRangeStart := nPosition;
        Request.ContentRangeEnd := nPosition + nDataLen - 1;
      end;
      
      try
        ms.Position := 0;
        if not FActive then Break;        
        http.Get( strURL, ms );

        bOK := (Cardinal(http.Response.ContentLength) = nDataLen) and (ms.Position = nDataLen);
        if bOk then
        begin
          FFileStream.WriteBuffer( ms.Memory, nPosition, nDataLen );
          nMaxTryCount := FHttpMaxTryCount;
        end;
        if not FActive then Break;
      except
        bOk := False;
      end;
      if not bOK then
      begin
        DoErrorInfo( 'Http get error' );
        FreeAndNil( http );
        Dec( nMaxTryCount );
        if nMaxTryCount <= 0 then
          Break;
        http := TIdHTTP.Create( nil );
        Sleep( 100 );
      end;
    end;
  finally
    FreeAndNil( ms );
    FreeAndNil( http );
  end;
  InterlockedDecrement( FActiveHttpThreadCount );
  if FActiveHttpThreadCount = 0 then
  begin
    DoDownFinished;
    if not bOK then
      DoDownFail;
    UnActiveDownTask;
  end;
end;

function TxdDownTask.FindHttpSource(const AURL: string): Boolean;
var
  i: Integer;
  p: PHttpDownInfo;
begin
  Result := False;
  for i := 0 to FHttpDownInfoList.Count - 1 do
  begin
    p := FHttpDownInfoList[i];
    if CompareText(AURL, p^.FURL) = 0 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

class function TxdDownTask.GetFileSizeByHttp(const AURL: string): Int64;
var
  http: TIdHTTP;
begin
  http := TIdHTTP.Create( nil );
  try
    http.Head( AURL );
    Result := http.Response.ContentLength;
    http.Disconnect;
    http.Free;
  except
    Result := 0;
    http.Free;
  end;
end;

function TxdDownTask.HttpSourceCount: Integer;
begin
  Result := FHttpDownInfoList.Count;
end;

function TxdDownTask.HttpSourceItem(const AIndex: Integer; var AURL, ARefer: string): Boolean;
var
  p: PHttpDownInfo;
begin
  Result := (AIndex >= 0) and (AIndex < FHttpDownInfoList.Count);
  if Result then
  begin
    p := FHttpDownInfoList[AIndex];
    AURL := p^.FURL;
    ARefer := p^.FReferURL;
  end;
end;

procedure TxdDownTask.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      ActiveDownTask
    else
      UnActiveDownTask;
  end;
end;

procedure TxdDownTask.SetDownFileInfo(const AFileName: string; const AFileSize: Int64);
begin
  if Active then Exit;
  FFileName := AFileName;
  FFileSize := AFileSize;
end;

procedure TxdDownTask.SetHttpMaxTryCount(const Value: Integer);
begin
  if (FHttpMaxTryCount <> Value) and (Value > 0) then
    FHttpMaxTryCount := Value;
end;

procedure TxdDownTask.SetHttpThreadCount(const Value: Integer);
begin
  if (not Active) and (FHttpThreadCount <> Value) then
    FHttpThreadCount := Value;
end;

procedure TxdDownTask.UnActiveDownTask;
begin
  try
    FActive := False;
    while WaitCompleteActive and (FActiveHttpThreadCount > 0) do
    begin
      Sleep( 10 );
      Application.ProcessMessages;
    end;
  except
  end;
end;

end.
