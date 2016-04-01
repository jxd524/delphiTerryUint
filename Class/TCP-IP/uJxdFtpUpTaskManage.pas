unit uJxdFtpUpTaskManage;

interface
uses
  Windows, Classes, SysUtils, Forms, uJxdFtpUpTask;

type
  {$M+}
  TUpTaskParam = record
    UpFileName: string;
    UpDirPath: string;
    UserName: string;
    UserPassWord: string;
    TaskOwnerName: string;
    Host: string;
    Port: Integer;
    Tag: Integer;
  end;
  TOnTask = procedure(Sender: TObject; const ATask: TxdFtpUpTask; var AAllow: Boolean) of object;
  TOnTaskEvent = procedure(Sender: TObject; const ATask: TxdFtpUpTask) of object;
  TOnTaskState = procedure(Sender: TObject; const ATask: TxdFtpUpTask; const AState: string) of object;
  
  TxdFtpUpTaskManage = class(TComponent)
  public
    procedure SaveUpList;
    function  GetUpTask(const AIndex: Integer): TxdFtpUpTask;
    function  AddUpTask(const AParam: TUpTaskParam; const AIsAutoRun: Boolean): Integer;
    function  FindUpTask(const AUpFileName: string; const AHost: string): TxdFtpUpTask; overload;
    function  FindUpTask(const ATaskID: Integer): TxdFtpUpTask; overload;
    procedure DeleteTask(const ATask: TxdFtpUpTask); overload;
    procedure DeleteTask(const ATaskID: Integer); overload;
    procedure StarteUpTask(const ATaskID: Integer);
    procedure StopUpTask(const ATaskID: Integer);
    procedure StartAllUpTask;
    procedure StopAllUpTask;
    
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    function  GetTaskID: Integer;
    procedure DoUpTaskFinished(Sender: TObject);
    procedure DoUpTaskStateChanged(Sender: TObject);
    procedure DoAllowReUploadFile(Sender: TObject; var AIsReUpload: Boolean);
    function  IsCanAddUpTask(const ATask: TxdFtpUpTask): Boolean;
    function  IsCanDeleteTask(const ATask: TxdFtpUpTask): Boolean;
    procedure DoEncryptUpTask(Sender: TObject; var Buffer; Count: Integer);
    function  GetActiveTaskCount: Integer;
  private
    FTaskList: TList;
    FTaskIDs: Integer;
    FTaskIDLock: TRTLCriticalSection;
    FTaskListLock: TRTLCriticalSection;
    FUpTaskListFileName: string;
    FOnAddUpTask: TOnTask;
    FOnDeleteTask: TOnTask;
    FOnUpTaskFinished: TOnTaskEvent;
    FOnUpTaskStateChanged: TOnTaskState;
    FOnCanReUpTask: TOnTask;
    FOnEncryptBuffer: TOnEncryptBuffer;
    FMaxTaskCount: Integer;
    function  GetTaskCount: Integer;
    procedure SetUpTaskListFileName(const Value: string);
    procedure SetMaxTaskCount(const Value: Integer);
  published
    property MaxTaskCount: Integer read FMaxTaskCount write SetMaxTaskCount;
    property TaskCount: Integer read GetTaskCount;
    property UpTaskListFileName: string read FUpTaskListFileName write SetUpTaskListFileName;

    property OnAddUpTask: TOnTask read FOnAddUpTask write FOnAddUpTask;
    property OnDeleteTask: TOnTask read FOnDeleteTask write FOnDeleteTask;
    property OnUpTaskFinished: TOnTaskEvent read FOnUpTaskFinished write FOnUpTaskFinished;
    property OnUpTaskStateChanged: TOnTaskState read FOnUpTaskStateChanged write FOnUpTaskStateChanged;
    property OnCanReUpTask: TOnTask read FOnCanReUpTask write FOnCanReUpTask;
    property OnEncryptBuffer: TOnEncryptBuffer read FOnEncryptBuffer write FOnEncryptBuffer;
  end;
  {$M-}

const
  {
  TFtpUpTaskState = (tsNULL, tsInit, tsConnecting, tsTransportData, tsCompletion, tsStop, tsError,
                     tsFileExist, tsCheckHash);
                     }
  CtTaskStateString: array[0..8] of string = ( '无信息', '初始化', '连接', '数据传输', '完成', '停止', '错误', '文件已经存在', '检查HASH'); 

implementation

const
  CtDefaultUpTaskListFileName = 'xdUpTaskList.dat';

{ TxdFtpUpTaskManage }

function TxdFtpUpTaskManage.AddUpTask(const AParam: TUpTaskParam; const AIsAutoRun: Boolean): Integer;
var
  task: TxdFtpUpTask;
begin
  Result := -1;
  if not FileExists(AParam.UpFileName) then Exit;
  task := FindUpTask( AParam.UpFileName, AParam.Host );
  if task = nil then
  begin
    if AParam.Host = '' then Exit;
    task := TxdFtpUpTask.Create;
    task.TaskID := GetTaskID;
    task.Tag := AParam.Tag;
    task.TaskOwnerName := AParam.TaskOwnerName;
    with task do
    begin
      FreeOnFinished := True;
      Host := AParam.Host;
      Port := AParam.Port;
      UserName := AParam.UserName;
      UserPassWord := AParam.UserPassWord;
      UpDirPath := AParam.UpDirPath;
      UpFileName := AParam.UpFileName;      
      OnUploadFinished := DoUpTaskFinished;
      OnStateChanged := DoUpTaskStateChanged;
      OnReUploadFile := DoAllowReUploadFile;
      OnEncryptBuffer := DoEncryptUpTask;
    end;
    if not IsCanAddUpTask(task) then
    begin
      Result := -1;
      task.Free;
      Exit;
    end
  end;
  Result := task.TaskID;
  EnterCriticalSection( FTaskListLock );
  try
    FTaskList.Add( task );
  finally
    LeaveCriticalSection( FTaskListLock );
  end;
  if AIsAutoRun and ((FMaxTaskCount <= 0) or (FMaxTaskCount < MaxTaskCount)) then
    task.Active := True;
end;

constructor TxdFtpUpTaskManage.Create(AOwner: TComponent);
begin
  FTaskList := TList.Create;
  FTaskIDs := 0;
  FMaxTaskCount := -1;
  FUpTaskListFileName := CtDefaultUpTaskListFileName;
  InitializeCriticalSection( FTaskIDLock );
  InitializeCriticalSection( FTaskListLock );
  inherited;
end;

procedure TxdFtpUpTaskManage.DeleteTask(const ATask: TxdFtpUpTask);
var
  nIndex: Integer;
begin
  EnterCriticalSection( FTaskListLock );
  try
    nIndex := FTaskList.IndexOf( ATask );
    if nIndex <> -1 then
    begin
      if IsCanDeleteTask(ATask) then
      begin
        FTaskList.Delete( nIndex );
        ATask.Free;
      end;
    end;
  finally
    LeaveCriticalSection( FTaskListLock );
  end;
end;

procedure TxdFtpUpTaskManage.DeleteTask(const ATaskID: Integer);
var
  i: Integer;
  task: TxdFtpUpTask;
begin
  EnterCriticalSection( FTaskListLock );
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if (task.TaskID = ATaskID) and IsCanDeleteTask(task) then
      begin
        FTaskList.Delete( i );
        task.Active := False;
        task.Free;
        Break;
      end;
    end;
  finally
    LeaveCriticalSection( FTaskListLock );
  end;
end;

destructor TxdFtpUpTaskManage.Destroy;
var
  i: Integer;
  task: TxdFtpUpTask;
begin
//  SaveUpList;
  for i := FTaskList.Count - 1 downto 0 do
  begin
    task := FTaskList[i];
    if task <> nil then
    begin
      task.Active := False;
      task.Free;
    end;
  end;
  FTaskList.Free;
  DeleteCriticalSection( FTaskIDLock );
  DeleteCriticalSection( FTaskListLock );
  inherited;
end;

procedure TxdFtpUpTaskManage.DoAllowReUploadFile(Sender: TObject; var AIsReUpload: Boolean);
begin
  AIsReUpload := False;
  if Assigned(OnCanReUpTask) and (Sender is TxdFtpUpTask) then
    OnCanReUpTask( Self, Sender as TxdFtpUpTask, AIsReUpload );
end;

procedure TxdFtpUpTaskManage.DoEncryptUpTask(Sender: TObject; var Buffer; Count: Integer);
begin
  if Assigned(OnEncryptBuffer) then
    OnEncryptBuffer( Self, Buffer, Count );
end;

procedure TxdFtpUpTaskManage.DoUpTaskFinished(Sender: TObject);
var
  task: TxdFtpUpTask;
  nIndex: Integer;
begin
  //线程中运行
  if Sender is TxdFtpUpTask then
  begin
    task := Sender as TxdFtpUpTask;
    if task.FreeOnFinished then
    begin
      EnterCriticalSection( FTaskListLock );
      try
        nIndex := FTaskList.IndexOf( task );
        if nIndex <> -1 then
          FTaskList.Delete( nIndex );
      finally
        LeaveCriticalSection( FTaskListLock );
      end;
    end;
    if Assigned(OnUpTaskFinished) then
      OnUpTaskFinished( Self, task );
  end;
end;

procedure TxdFtpUpTaskManage.DoUpTaskStateChanged(Sender: TObject);
var
  task: TxdFtpUpTask;
  strState: string;
begin
  if Sender is TxdFtpUpTask then
  begin
    task := Sender as TxdFtpUpTask;
    if Assigned(OnUpTaskStateChanged) then
    begin
      strState := CtTaskStateString[ Integer(task.CurState) ];
      OnUpTaskStateChanged( Self, task, strState );
    end;
  end;
end;

function TxdFtpUpTaskManage.FindUpTask(const ATaskID: Integer): TxdFtpUpTask;
var
  i: Integer;
  task: TxdFtpUpTask;
begin
  Result := nil;
  EnterCriticalSection( FTaskListLock );
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.TaskID = ATaskID then
      begin
        Result := task;
        Break;
      end;
    end;
  finally
    LeaveCriticalSection( FTaskListLock );
  end;
end;

function TxdFtpUpTaskManage.FindUpTask(const AUpFileName, AHost: string): TxdFtpUpTask;
var
  i: Integer;
  task: TxdFtpUpTask;
begin
  Result := nil;
  EnterCriticalSection( FTaskListLock );
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if (CompareText(task.UpFileName, AUpFileName) = 0) then
      begin
        if AHost <> '' then
        begin
          if CompareText(task.Host, AHost) = 0 then
          begin
            Result := task;
            Break;
          end;
        end
        else
        begin
          Result := task;
          Break;
        end;
      end;
    end;
  finally
    LeaveCriticalSection( FTaskListLock );
  end;
end;

function TxdFtpUpTaskManage.GetActiveTaskCount: Integer;
var
  i: Integer;
  task: TxdFtpUpTask;
begin
  EnterCriticalSection( FTaskListLock );
  try
    Result := 0;
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.Active then
        Inc( Result );
    end;
  finally
    LeaveCriticalSection( FTaskListLock );
  end;
end;

function TxdFtpUpTaskManage.GetTaskCount: Integer;
begin
  Result := FTaskList.Count;
end;

function TxdFtpUpTaskManage.GetTaskID: Integer;
begin
  EnterCriticalSection( FTaskIDLock );
  try
    Result := FTaskIDs;
    Inc( FTaskIDs );
  finally
    LeaveCriticalSection( FTaskIDLock );
  end;
end;

function TxdFtpUpTaskManage.GetUpTask(const AIndex: Integer): TxdFtpUpTask;
begin
  if (AIndex >= 0) and (AIndex < FTaskList.Count) then
    Result := FTaskList[AIndex]
  else
    Result := nil;
end;

function TxdFtpUpTaskManage.IsCanAddUpTask(const ATask: TxdFtpUpTask): Boolean;
begin
  Result := True;
  if Assigned(OnAddUpTask) then
    OnAddUpTask( Self, ATask, Result);
end;

function TxdFtpUpTaskManage.IsCanDeleteTask(const ATask: TxdFtpUpTask): Boolean;
begin
  Result := True;
  if Assigned(OnDeleteTask) then
    OnAddUpTask( Self, ATask, Result );
end;

procedure TxdFtpUpTaskManage.SaveUpList;
var
  i: Integer;
  task: TxdFtpUpTask;
  FileList: TStringList;
begin
  FileList := TStringList.Create;
  EnterCriticalSection( FTaskListLock );
  try
    if FUpTaskListFileName = '' then
      FUpTaskListFileName := ExtractFilePath(ParamStr(0)) + CtDefaultUpTaskListFileName;
    if not DirectoryExists(ExtractFilePath(FUpTaskListFileName)) then
    begin
      try
        ForceDirectories( ExtractFilePath(FUpTaskListFileName) );
      except
        Exit;
      end;
    end;

    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task <> nil then
        FileList.Add( task.UpFileName );
    end;
    if FileList.Count > 0 then
      FileList.SaveToFile( FUpTaskListFileName );
  finally
    LeaveCriticalSection( FTaskListLock );
    FileList.Free;
  end;
end;

procedure TxdFtpUpTaskManage.SetMaxTaskCount(const Value: Integer);
var
  i, nMax: Integer;
  task: TxdFtpUpTask;
begin
  FMaxTaskCount := Value;
  if FMaxTaskCount <= 0 then Exit;
  nMax := FMaxTaskCount;
  EnterCriticalSection( FTaskListLock );
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.Active then
      begin
        Dec( nMax );
        if nMax < 0 then
        begin
          task.Active := False;
          Inc( nmax );
        end;
      end;
    end;
  finally
    LeaveCriticalSection( FTaskListLock );
  end;
end;

procedure TxdFtpUpTaskManage.SetUpTaskListFileName(const Value: string);
var
  FileList: TStringList;
  i: Integer;
  param: TUpTaskParam;
begin
  if FileExists(Value) then
  begin
    FUpTaskListFileName := Value;
    with param do
    begin
      UpFileName := '';
      UpDirPath := '';
      UserName := '';
      UserPassWord := '';
      Host := '';
      Port := 21;
    end;
    FileList := TStringList.Create;
    try
      FileList.LoadFromFile( FUpTaskListFileName );
      for i := 0 to FileList.Count - 1 do
      begin
        param.UpFileName := FileList[i];
        AddUpTask( param, False );
      end;
    finally
      FileList.Free;
    end;
  end
  else
  begin
    FUpTaskListFileName := Value;
  end;
end;

procedure TxdFtpUpTaskManage.StartAllUpTask;
var
  i, nMax: Integer;
  task: TxdFtpUpTask;
begin
  EnterCriticalSection( FTaskListLock );
  try
    nMax := MaxTaskCount;
    if nMax <= 0 then
      nMax := GetActiveTaskCount + 10;
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FtaskList[i];
      if nMax > 0 then
      begin
        Dec( nMax );
        task.Active := True;
      end;
    end;
  finally
    LeaveCriticalSection( FTaskListLock );
  end;
end;

procedure TxdFtpUpTaskManage.StarteUpTask(const ATaskID: Integer);
var
  task: TxdFtpUpTask;
begin
  task := FindUpTask( ATaskID );
  if task <> nil then
  begin
    if (FMaxTaskCount <= 0) or (FMaxTaskCount < MaxTaskCount) then
      task.Active := True;
  end;
end;

procedure TxdFtpUpTaskManage.StopAllUpTask;
var
  i: Integer;
  task: TxdFtpUpTask;
begin
  EnterCriticalSection( FTaskListLock );
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FtaskList[i];
      task.Active := False;
    end;
  finally
    LeaveCriticalSection( FTaskListLock );
  end;
end;

procedure TxdFtpUpTaskManage.StopUpTask(const ATaskID: Integer);
var
  task: TxdFtpUpTask;
begin
  task := FindUpTask( ATaskID );
  if task <> nil then
    task.Active := False;
end;
end.
