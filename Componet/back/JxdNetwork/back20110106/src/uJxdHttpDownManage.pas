{
单元名称: uJxdHttpDown
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: 支持多线程，多服务器的HTTP协议下载管理类
开始时间: 2010-07-22
修改时间: 2010-07-23 (最后修改)
}

unit uJxdHttpDownManage;

interface
uses
  Windows, Classes, SysUtils, uJxdHttpDown, ExtCtrls, Forms;

type
  TOnNotifyTaskEvent = procedure(Sender: TObject; ATask: TxdHttpDown) of object;
  TxdHttpDownManage = class(TComponent)
  public
    procedure SaveDownList;
    {任务}
    function AddHttpDownTask(const ASaveFileName, ASrcUrl, AReferenceUrl: string): Integer;
    function GetHttpDownTask(const AIndex: Integer): TxdHttpDown;
    function DeleteDownTask(const ATask: TxdHttpDown): Boolean;
    function FindHttpDownTask(const AFileName: string): Integer; overload;
    function FindHttpDownTask(const ATaskID: Integer): TxdHttpDown; overload;
    {操作}
    procedure StartTask(const ATaskID: Integer);
    procedure StopTask(const ATaskID: Integer);
    procedure PauseTask(const ATaskID: Integer);
    procedure DeleteTask(const ATaskID: Integer);
    procedure AllTaskStart;
    procedure AllTaskStop;
    procedure AllTaskPause;

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    procedure LockDownTaskList(const ALock: Boolean);
    procedure DoTaskDownFinished(Sender: TObject);
    procedure DoTimerToDelectTask(Sender: TObject);
    function  ActiveTaskCount: Integer;
  private
    FTaskID: Integer;
    FDownTaskList: TList;
    FListLock: TRTLCriticalSection;
    FDownFileListFileName: string;
    FOnAddTask: TOnNotifyTaskEvent;
    FOnDeleteTask: TOnNotifyTaskEvent;
    FOnCompeleteDownTask: TOnNotifyTaskEvent;
    FMaxTaskCount: Integer;
    procedure SetDownFileListFileName(const Value: string);
    function  GetTaskCount: Integer;
    procedure SetMaxTaskCount(const Value: Integer);
  published
    property MaxTaskCount: Integer read FMaxTaskCount write SetMaxTaskCount;
    property TaskCount: Integer read GetTaskCount;
    property DownFileListFileName: string read FDownFileListFileName write SetDownFileListFileName;

    property OnCompeleteDownTask: TOnNotifyTaskEvent read FOnCompeleteDownTask write FOnCompeleteDownTask; 
    property OnDeleteTask: TOnNotifyTaskEvent read FOnDeleteTask write FOnDeleteTask;
    property OnAddTask: TOnNotifyTaskEvent read FOnAddTask write FOnAddTask;
  end;

implementation

const
  CtDefaultSaveFileName = 'xdDownFileList.dat';

{ TxdHttpDownManage }

function TxdHttpDownManage.ActiveTaskCount: Integer;
var
  i: Integer;
  task: TxdHttpDown;
begin
  Result := 0;
  LockDownTaskList( True );
  try
    for i := 0 to FDownTaskList.Count - 1 do
    begin
      task := FDownTaskList[i];
      if task.Active then
        Inc( Result );
    end;
  finally
    LockDownTaskList( False );
  end;
end;

function TxdHttpDownManage.AddHttpDownTask(const ASaveFileName, ASrcUrl, AReferenceUrl: string): Integer;
var
  task: TxdHttpDown;
begin
  Result := FindHttpDownTask( ASaveFileName );
  if Result <> -1 then Exit;
  task := TxdHttpDown.Create;
  task.CurThreadCount := 1;
  task.SaveFileName := ASaveFileName;
  task.AddHttpSource( ASrcUrl, AReferenceUrl );
  task.OnDownFinished := DoTaskDownFinished;
  if task.HttpSourceCount > 0 then
  begin
    LockDownTaskList( True );
    try
      if FDownTaskList.Add( task ) <> -1 then
      begin
        task.TaskID := FTaskID;
        InterlockedIncrement( FTaskID );
        Result := task.TaskID;
      end
      else
      begin
        Result := -1;
        task.Free;
      end;
    finally
      LockDownTaskList( False );
    end;
  end
  else
  begin
    Result := -1;
    task.Free;
  end;
  if (Result <> -1) and Assigned(OnAddTask) then
    OnAddTask( Self, task );
end;

procedure TxdHttpDownManage.AllTaskPause;
var
  i: Integer;
  task: TxdHttpDown;
begin
  LockDownTaskList( True );
  try
    for i := 0 to FDownTaskList.Count - 1 do
    begin
      task := FDownTaskList[i];
      task.Pause;
    end;
  finally
    LockDownTaskList( False );
  end;
end;

procedure TxdHttpDownManage.AllTaskStart;
var
  i, nMax: Integer;
  task: TxdHttpDown;
begin
  LockDownTaskList( True );
  try
    nMax := MaxTaskCount;
    if nMax <= 0 then
      nMax := FDownTaskList.Count + 100;;
    for i := 0 to FDownTaskList.Count - 1 do
    begin
      task := FDownTaskList[i];
      Dec( nMax );
      if nMax >= 0 then
        task.Active := True
      else
        Break;
    end;
  finally
    LockDownTaskList( False );
  end;
end;

procedure TxdHttpDownManage.AllTaskStop;
var
  i: Integer;
  task: TxdHttpDown;
begin
  LockDownTaskList( True );
  try
    for i := 0 to FDownTaskList.Count - 1 do
    begin
      task := FDownTaskList[i];
      task.Active := False;
    end;
  finally
    LockDownTaskList( False );
  end;
end;

constructor TxdHttpDownManage.Create(AOwner: TComponent);
begin
  inherited;
  FDownTaskList := TList.Create;
  InitializeCriticalSection( FListLock );
  FTaskID := 0;
  FMaxTaskCount := -1;
end;

function TxdHttpDownManage.DeleteDownTask(const ATask: TxdHttpDown): Boolean;
var
  nIndex: Integer;
begin
  LockDownTaskList( True );
  try
    nIndex := FDownTaskList.IndexOf( ATask );
    Result := nIndex <> -1;
    if Result then
    begin
      FDownTaskList.Delete( nIndex );
      if Assigned(OnDeleteTask) then
        OnDeleteTask( Self, ATask );
    end;
  finally
    LockDownTaskList( False );
  end;
end;

procedure TxdHttpDownManage.DeleteTask(const ATaskID: Integer);
var
  i: Integer;
  task: TxdHttpDown;
  bFind: Boolean;
begin
  bFind := False;
  task := nil;
  LockDownTaskList( True );
  try
    for i := FDownTaskList.Count - 1 downto 0 do
    begin
      task := FDownTaskList[i];
      if task.TaskID = ATaskID then
      begin
        bFind := True;
        FDownTaskList.Delete( i );
        Break;
      end;
    end;
  finally
    LockDownTaskList( False );
  end;
  if bFind then
  begin
    if Assigned(OnDeleteTask) then
      OnDeleteTask( Self, task );
    task.Free;
  end;
end;

destructor TxdHttpDownManage.Destroy;
var
  i: Integer;
  task: TxdHttpDown;
begin
  SaveDownList;
  for i := FDownTaskList.Count - 1 downto 0 do
  begin
    task := FDownTaskList[i];
    task.Active := False;
    task.Free;
  end;
  FDownTaskList.Free;
  DeleteCriticalSection( FListLock );
  inherited;
end;

procedure TxdHttpDownManage.DoTaskDownFinished(Sender: TObject);
var
  task: TxdHttpDown;
begin
  if Sender is TxdHttpDown then
  begin
    task := Sender as TxdHttpDown;
    DeleteDownTask( task );
    with TTimer.Create(Self) do
    begin
      Interval := 100;
      OnTimer := DoTimerToDelectTask;
      Tag := Integer( task );
      Enabled := True;
    end;
  end;
  SaveDownList;
end;

procedure TxdHttpDownManage.DoTimerToDelectTask(Sender: TObject);
var
  task: TxdHttpDown;
begin
  if Sender is TTimer then
  begin
    (Sender as TTimer).Enabled := False;
    task := TxdHttpDown( (Sender as TTimer).Tag );
    if task <> nil then
    begin
      if Assigned(OnCompeleteDownTask) then
        OnCompeleteDownTask( Self, task );
      task.Free;
    end;
    (Sender as TTimer).Free;
  end;
end;

function TxdHttpDownManage.FindHttpDownTask(const ATaskID: Integer): TxdHttpDown;
var
  i: Integer;
  task: TxdHttpDown;
begin
  Result := nil;
  LockDownTaskList( True );
  try
    for i := FDownTaskList.Count - 1 downto 0 do
    begin
      task := FDownTaskList[i];
      if task.TaskID = ATaskID then
      begin
        Result := task;
        Break;
      end;
    end;
  finally
    LockDownTaskList( False );
  end;
end;

function TxdHttpDownManage.GetHttpDownTask(const AIndex: Integer): TxdHttpDown;
begin
  if (AIndex >= 0) and (AIndex < TaskCount) then
    Result := FDownTaskList[AIndex]
  else
    Result := nil;
end;

function TxdHttpDownManage.GetTaskCount: Integer;
begin
  Result := FDownTaskList.Count;
end;

function TxdHttpDownManage.FindHttpDownTask(const AFileName: string): Integer;
var
  i: Integer;
  task: TxdHttpDown;
begin
  Result := -1;
  LockDownTaskList( True );
  try
    for i := FDownTaskList.Count - 1 downto 0 do
    begin
      task := FDownTaskList[i];
      if CompareText(AFileName, task.SaveFileName) = 0 then
      begin
        Result := i;
        Break;
      end;
    end;
  finally
    LockDownTaskList( False );
  end;
end;

procedure TxdHttpDownManage.LockDownTaskList(const ALock: Boolean);
begin
  if ALock then
    EnterCriticalSection( FListLock )
  else
    LeaveCriticalSection( FListLock );
end;

procedure TxdHttpDownManage.PauseTask(const ATaskID: Integer);
var
  task: TxdHttpDown;
begin
  task := FindHttpDownTask( ATaskID );
  if task <> nil then
    task.Pause;
end;

procedure TxdHttpDownManage.SaveDownList;
var
  i: Integer;
  task: TxdHttpDown;
  FileList: TStringList;
begin
  if FileExists(FDownFileListFileName) then
    DeleteFile( FDownFileListFileName );
  FileList := TStringList.Create;
  LockDownTaskList( True );
  try
    if FDownFileListFileName = '' then
      FDownFileListFileName := ExtractFilePath(ParamStr(0)) + CtDefaultSaveFileName;

    for i := 0 to FDownTaskList.Count - 1 do
    begin
      task := FDownTaskList[i];
      if task <> nil then
        FileList.Add( task.SaveFileName );
    end;
    if FileList.Count > 0 then
      FileList.SaveToFile( FDownFileListFileName );
  finally
    LockDownTaskList( False );
    FileList.Free;
  end;
end;

procedure TxdHttpDownManage.SetDownFileListFileName(const Value: string);
var
  FileList: TStringList;
  i: Integer;
begin
  if FileExists(Value) then
  begin
    FDownFileListFileName := Value;
    FileList := TStringList.Create;
    try
      FileList.LoadFromFile( FDownFileListFileName );
      for i := 0 to FileList.Count - 1 do
        AddHttpDownTask( FileList[i], '', '' );
    finally
      FileList.Free;
    end;
  end
  else
    FDownFileListFileName := Value;     
end;

procedure TxdHttpDownManage.SetMaxTaskCount(const Value: Integer);
var
  i, nActiveCount: Integer;
  task: TxdHttpDown;
begin
  FMaxTaskCount := Value;
  if (FMaxTaskCount = -1) or (FDownTaskList.Count <= FMaxTaskCount) then Exit;
  nActiveCount := 0;
  for i := 0 to FDownTaskList.Count - 1 do
  begin
    task := FDownTaskList[i];
    if task.Active then
    begin
      Inc( nActiveCount );
      if nActiveCount > FMaxTaskCount then
      begin
        task.Active := False;
        Dec( nActiveCount );
      end;
    end;
  end;  
end;

procedure TxdHttpDownManage.StartTask(const ATaskID: Integer);
var
  task: TxdHttpDown;
begin
  task := FindHttpDownTask( ATaskID );
  if task <> nil then
  begin
    if (FMaxTaskCount <= 0) or (FMaxTaskCount < ActiveTaskCount)  then
      task.Active := True;
  end;
end;

procedure TxdHttpDownManage.StopTask(const ATaskID: Integer);
var
  task: TxdHttpDown;
begin
  task := FindHttpDownTask( ATaskID );
  if task <> nil then
    task.Active := False;
end;

end.
