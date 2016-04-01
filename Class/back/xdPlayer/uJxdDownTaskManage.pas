unit uJxdDownTaskManage;

interface

uses
  Windows, SysUtils, Classes, Forms,
  uJxdp2spDownTask, uJxdDataStream;

type
  TTaskParam = record
    FSaveFileName: string;
    FMainURL, FMainReferURL: string;
    FFileSize: Int64;
  end;
  TOnDownTaskFinished = procedure(const ATaskID: Cardinal; const AIsOK: Boolean) of object;
  TxdDownTaskManage = class
  {$M+}
  public
    constructor Create;
    destructor  Destroy; override;
    //添加下载任务，返回任务ID；0表示添加不成功
    function  AddDownTask(const AParam: TTaskParam): Cardinal;
    procedure AddDownTaskSource(const ATaskID: Cardinal; const AURL, AReferURL: string);
    //由下载任务ID得到所属的文件流ID
    function  TaskIDToFileStreamID(const ATaskID: Cardinal): Cardinal;
    //删除指定任务
    procedure DeleteTask(const ATaskID: Cardinal);
    //全力下载指定任务（其它任务暂停）
    function  FullSwingTask(const ATaskID: Cardinal; var AFileStreamID: Cardinal; const AWait: Boolean): Boolean;
    //下载指定任务
    procedure StartTask(const ATaskID: Cardinal);
    //停止指定任务
    procedure StopTask(const ATaskID: Cardinal);
    //获取任务ID
    function GetTaskID(const AIndex: Integer): Cardinal;
  private
    FFileName: string;
    FTaskIndex: Cardinal;
    FTaskList: TList;
    FLock: TRTLCriticalSection;
    FOnDownTaskFinished: TOnDownTaskFinished;
    procedure LockManage(const ALock: Boolean);
    function  FindDownTask(const AFileName: string; const AURL: string): Cardinal;
    procedure DoTaskSuccess(ASender: TObject);
    procedure DoTaskFail(ASender: TObject);
    procedure DoTaskFinished(ATask: TxdDownTask; const AIsSuccess: Boolean);

    procedure HandleTask(const ATaskID: Cardinal; const AStart: Boolean);
    function  GetTaskCount: Integer;

    procedure SaveToFile(const AFileName: string);
    procedure LoadFormFile(const AFileName: string);
    procedure SetManageFileName(const Value: string);
    function GetActiveTaskCount: Integer;
  published
    property TaskCount: Integer read GetTaskCount;
    property ActiveTaskCount: Integer read GetActiveTaskCount;
    property ManageFile: string read FFileName write SetManageFileName;
    property OnDownTaskFinished: TOnDownTaskFinished read FOnDownTaskFinished write FOnDownTaskFinished; 
  end;
  {$M-}

implementation

const
  CtManageVersion: Word = 8723;
  CtManageDefaultFileName = 'DownFiles\DownTaskManage.dat';

{ TxdDownTaskManage }

function TxdDownTaskManage.AddDownTask(const AParam: TTaskParam): Cardinal;
var
  task: TxdDownTask;
begin
  LockManage( True );
  try
    Result := FindDownTask( AParam.FSaveFileName, AParam.FMainURL );
    if Result = 0 then
    begin
      task := TxdDownTask.Create;
      with task do
      begin
        SetDownFileInfo( AParam.FSaveFileName, AParam.FFileSize );
        AddHttpSource( AParam.FMainURL, AParam.FMainReferURL );
        TaskID := FTaskIndex;
        OnDownSuccess := DoTaskSuccess;
        OnDownFail := DoTaskFail;
      end;
      FTaskIndex := FTaskIndex + 1;
      FTaskList.Add( task );
      Result := task.TaskID;
    end;
  finally
    LockManage( False );
  end;
end;

procedure TxdDownTaskManage.AddDownTaskSource(const ATaskID: Cardinal; const AURL, AReferURL: string);
var
  i: Integer;
  task: TxdDownTask;
begin

  LockManage( True );
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.TaskID = ATaskID then
      begin
        task.AddHttpSource( AURL, AReferURL );
        Break;
      end;
    end;
  finally
    LockManage( False );
  end;
end;

constructor TxdDownTaskManage.Create;
begin
  InitializeCriticalSection( FLock );
  FTaskList := TList.Create;
  FTaskIndex := GetTickCount;
  FFileName := ExtractFilePath( ParamStr(0) ) + CtManageDefaultFileName;
  LoadFormFile( FFileName );
end;

procedure TxdDownTaskManage.DeleteTask(const ATaskID: Cardinal);
var
  i: Integer;
  task: TxdDownTask;
begin
  LockManage( True );
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.TaskID = ATaskID then
      begin
        FreeAndNil( task );
        FTaskList.Delete( i );
        Break;
      end;
    end;
  finally
    LockManage( False );
  end;
end;

destructor TxdDownTaskManage.Destroy;
var
  i: Integer;
  task: TxdDownTask;
begin
  SaveToFile( FFileName );
  LockManage( True );
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := TxdDownTask( FTaskList[i] );
      task.WaitCompleteActive := True;
      task.Active := False;
      task.Free;
    end;
    FreeAndNil( FTaskList );
  finally
    LockManage( False );
  end;
  DeleteCriticalSection( FLock );
  inherited;
end;

procedure TxdDownTaskManage.DoTaskFail(ASender: TObject);
begin
  if Assigned(OnDownTaskFinished) then
    OnDownTaskFinished( (ASender as TxdDownTask).TaskID, False );
  DoTaskFinished( ASender as TxdDownTask, False );
end;

procedure TxdDownTaskManage.DoTaskFinished(ATask: TxdDownTask; const AIsSuccess: Boolean);
begin
  DeleteTask( ATask.TaskID );
end;

procedure TxdDownTaskManage.DoTaskSuccess(ASender: TObject);
begin
  if Assigned(OnDownTaskFinished) then
    OnDownTaskFinished( (ASender as TxdDownTask).TaskID, True );
  DoTaskFinished( ASender as TxdDownTask, True );
end;

function TxdDownTaskManage.FindDownTask(const AFileName: string; const AURL: string): Cardinal;
var
  i: Integer;
  task: TxdDownTask;
begin
  Result := 0;
  for i := 0 to FTaskList.Count - 1 do
  begin
    task := FTaskList[i];
    if (CompareText(AFileName, task.FileName) = 0) or task.FindHttpSource(AURL) then
    begin
      Result := task.TaskID;
      Break;
    end;
  end;
end;

function TxdDownTaskManage.FullSwingTask(const ATaskID: Cardinal; var AFileStreamID: Cardinal; const AWait: Boolean): Boolean;
var
  i: Integer;
  task: TxdDownTask;
  bFind: Boolean;
begin
  AFileStreamID := 0;
  LockManage( True );
  try
    //启动下载
    bFind := False;
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.TaskID = ATaskID then
      begin
        task.WaitCompleteActive := AWait;
        task.Active := True;
        AFileStreamID := task.FileStreamID;
        bFind := True;
        Break;
      end;
    end;
    if not bFind then 
    begin
      Result := False;
      Exit;
    end;
    //停止其它任务
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.TaskID <> ATaskID then
      begin
        task.WaitCompleteActive := False;
        task.Active := False;
      end;
    end;
  finally
    LockManage( False );
  end;
  Result := AFileStreamID <> 0;
end;

function TxdDownTaskManage.GetActiveTaskCount: Integer;
var
  i: Integer;
  task: TxdDownTask;
begin
  LockManage( True );
  try
    Result := 0;
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.Active then
        Inc( Result );
    end;
  finally
    LockManage( False );
  end;
end;

function TxdDownTaskManage.GetTaskCount: Integer;
begin
  Result := FTaskList.Count;
end;

function TxdDownTaskManage.GetTaskID(const AIndex: Integer): Cardinal;
begin
  LockManage( True );
  try
    if (AIndex >= 0) and (AIndex < FTaskList.Count) then
      Result := TxdDownTask( FTaskList[AIndex] ).TaskID
    else
      Result := 0;
  finally
    LockManage( False );
  end;
end;

procedure TxdDownTaskManage.HandleTask(const ATaskID: Cardinal; const AStart: Boolean);
var
  i: Integer;
  task: TxdDownTask;
begin
  for i := 0 to FTaskList.Count - 1 do
  begin
    task := FTaskList[i];
    if task.TaskID = ATaskID then
    begin
      task.Active := AStart;
      Break;
    end;
  end;
end;

procedure TxdDownTaskManage.LoadFormFile(const AFileName: string);
var
  f: TxdFileStream;
  i, j, nCount, n2: Integer;
  nSize: Int64;
  task: TxdDownTask;
  strFileName, strURL, strReferURL: string;
begin
  if not FileExists(AFileName) then Exit;
  f := TxdFileStream.Create( AFileName, fmOpenRead );
  try
    with f do
    begin
      if Size <= 4 then
      begin
        DeleteFile( AFileName );
        Exit;
      end;
      if ReadWord <> CtManageVersion then
      begin
        OutputDebugString( 'DownTaskManage load form file error.' );
        Exit;
      end;
      nCount := ReadInteger;

      for i := 0 to nCount - 1 do
      begin
        nSize := ReadInt64;
        strFileName := ReadString;
        n2 := ReadInteger;
        task := TxdDownTask.Create;
        task.SetDownFileInfo( strFileName, nSize );
        for j := 0 to n2 - 1 do
        begin
          strURL := ReadString;
          strReferURL := ReadString;
          task.AddHttpSource( strURL, strReferURL );
        end;
        task.OnDownSuccess := DoTaskSuccess;
        task.OnDownFail := DoTaskFail;

        LockManage( True );
        try
          task.TaskID := FTaskIndex;
          if -1 = FTaskList.Add( task ) then
            task.Free
          else
            Inc( FTaskIndex );
        finally
          LockManage( False );
        end;
      end;
    end;
  finally
    f.Free;
  end;
end;

procedure TxdDownTaskManage.LockManage(const ALock: Boolean);
begin
  if ALock then
    EnterCriticalSection( FLock )
  else
    LeaveCriticalSection( FLock );
end;

procedure TxdDownTaskManage.SaveToFile(const AFileName: string);
var
  f: TxdFileStream;
  i, j, nCount: Integer;
  task: TxdDownTask;
  nPos1, nPos2: Int64;
  strURL, strReferURL: string;
begin
  if FTaskList.Count = 0 then
  begin
    if FileExists(AFileName) then
    begin
      DeleteFile( AFileName );
      Exit;
    end;
  end;
  f := TxdFileStream.Create( AFileName, fmCreate );
  try
    with f do
    begin
      WriteWord( CtManageVersion );          //版本号
      WriteInteger( FTaskList.Count );       //任务数量
      for i := 0 to FTaskList.Count - 1 do
      begin
        task := FTaskList[i];
        WriteInt64( task.FileSize );        //任务大小
        WriteString( task.FileName );       //任务文件名
        nPos1 := Position;
        nCount := task.HttpSourceCount;
        WriteInteger( task.HttpSourceCount );//第N个任务拥有的HTTP源数量
        for j := 0 to task.HttpSourceCount - 1 do
        begin
          if not task.HttpSourceItem(j, strURL, strReferURL) then
            Dec( nCount )
          else
          begin                               //HTTP源地址
            WriteString( strURL );
            WriteString( strReferURL );
          end;
        end;
        if nCount <> task.HttpSourceCount then
        begin
          nPos2 := Position;
          Position := nPos1;
          WriteInteger( nCount );
          Position := nPos2;
        end;
      end;
    end;
  finally
    f.Free;
  end;
end;

procedure TxdDownTaskManage.SetManageFileName(const Value: string);
begin
  FFileName := Value;
  LoadFormFile( FFileName );
end;

procedure TxdDownTaskManage.StartTask(const ATaskID: Cardinal);
begin
  HandleTask( ATaskID, True );
end;

procedure TxdDownTaskManage.StopTask(const ATaskID: Cardinal);
begin
  HandleTask( ATaskID, False );
end;

function TxdDownTaskManage.TaskIDToFileStreamID(const ATaskID: Cardinal): Cardinal;
var
  i: Integer;
  task: TxdDownTask;
begin
  Result := 0;
  for i := 0 to FTaskList.Count - 1 do
  begin
    task := FTaskList[i];
    if task.TaskID = ATaskID then
    begin
      Result := task.FileStreamID;
      Break;
    end;
  end;
end;

end.
