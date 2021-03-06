{
单元名称: uJxdDownTaskManage
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明: 负责执行P2P下载，任务释放缓存处理，任务信息的保存与载入
开始时间: 2011-09-05
修改时间: 2011-09-13 (最后修改时间)
类说明  :
    管理多个下载任务，处理UDP文件传输命令，自动识别命令，并调用相应的下载任务函数。
    为下载任务提供服务器信息的查询，P2P下载过程中的P2P连接
}
unit uJxdDownTaskManage;

interface
uses
  Windows, Classes, SysUtils, uJxdHashCalc, uJxdDataStream, uJxdFileSegmentStream, uJxdUdpDefine, uJxdUdpCommonClient, 
  uJxdServerManage, uJxdThread, uJxdDownTask, uJxdP2PUserManage, WinSock2, uJxdTaskDefine, ActiveX, uJxdClientHashManage,
  uJxdFileShareManage; 
  
type
  TDownTaskOpt = (optStart, optStop, optDelete, optReverse);
  TOnGetTaskStreamID = procedure(const ATaskID: Integer; const AStreamID: Cardinal) of object;
  TOnTaskFinished = procedure(const ATask: TxdDownTask; const ASuccess: Boolean) of object;
  TOnWriteConfig = procedure(AStream: TxdFileStream; const ATaskData: Pointer) of object;
  TOnReadConfig = procedure(AStream: TxdFileStream; var ATaskData: Pointer) of object;

  PHandleTashParam = ^THandleTaskParam;
  THandleTaskParam = record
    FTask: TxdDownTask;
    FFullPower: Boolean;
    FAutoStart: Boolean;
  end;
  
  TxdDownTaskManage = class
  public
    constructor Create;
    destructor  Destroy; override;

    procedure LockManage; inline;  //锁定管理器
    procedure UnlockManage; inline; //解锁

    //查找指定的任务, 函数内无加锁
    function  FindTask(const AHashStyle: THashStyle; const AHash: TxdHash): TxdDownTask; overload; 
    function  FindTask(const AUrl: string): TxdDownTask; overload;
    function  IsExistsTask(const ATaskID: Integer): Boolean; //无加锁

    {任务操作}
    //添加下载任务, 参数 ApDownInfo.TaskID 可指定具体的数据，如果为0，则表示为管理器自自己加载, 如果指定的值已存在，则自动更新
    function  AddDownTask(ApDownInfo: PDownTaskParam; const AAutoStartTask: Boolean = False): Boolean;
    function  OptDownTask(const ATaskID: Integer; const AOpt: TDownTaskOpt; const ALock: Boolean = True): Boolean;
    function  StartTask(const ATaskID: Integer): Boolean;
    function  StopTask(const ATaskID: Integer): Boolean;
    function  ReverseTask(const ATaskID: Integer): Boolean; //任务Active := not Active;
    function  DeleteTask(const ATaskID: Integer): Boolean;
    procedure FullPowerDownTask(const ATaskID: Integer; const ALock: Boolean); //只下载指定的任务, 其它任务全部停止
    function  GetSoftUpdateTaskID: Integer; //软件自动升级类型的下载任务ID

    //获取指定任务的StreamID 函数回调：OnGetTaskStreamID
    function  GetTaskStreamID(const ATaskID: Integer; const AFullPower, AAutoStartTask, AByThread: Boolean): Boolean; overload;
    function  GetTaskStreamID(const ATaskID: Integer): Integer; overload;

    {获取指定任务信息}
    function  GetTaskProgressByIndex(const ATaskIndex: Integer; var AInfo: TTaskProgressInfo): Boolean; //按位置查询任务进度
    function  GetTaskProgressByTaskID(const ATaskID: Integer; var AInfo: TTaskProgressInfo): Boolean; //按任务ID查询任务进度
    function  GetTaskDownDetailByIndex(const ATaskIndex: Integer; var AInfo: TTaskDownDetailInfo): Boolean; //查询任务下载详情
    function  GetTaskDownDetailByTaskID(const ATaskID: Integer; var AInfo: TTaskDownDetailInfo): Boolean;
    function  GetTaskDataByTaskID(const ATaskID: Integer): Pointer; //获取指定任务的TaskData属性值
    
    function  MyTestDown(AHash: string; const AIsFileHash: Boolean): Integer;
  private
    {对象对象}
    FLock: TRTLCriticalSection;
    FTaskList: TList;
    FManageThread: TThreadCheck;

    {任务变量}
    FMinTaskID: Integer; //当前最小任务ID，无任务则此ID初始化为GetTickCount
    
    {管理器总体控制}
    procedure ActiveManage;
    procedure UnActiveManage;
    function  DoAddDownTask(const ATask: TxdDownTask): Boolean; //把任务添加到管理器中

    {线程执行函数}
    procedure DoThreadToDownTasks; 
    procedure DoHandleTask(Ap: Pointer);

    {文件传输处理事件}
    procedure DoHandleDownTaskCmdEvent(const ACmd: Word; const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
                                        const AIsSynchroCmd: Boolean; const ASynchroID: Word);
    {P2P连接处理事件}
    procedure DoP2PConnectedStateEvent(const AUserID: Cardinal; const AConnectState: TConnectState; const AParam: Pointer);
    
    {接收到命令处理}
    procedure DoHandleCmdReply_QueryFileInfo(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal); //CtCmdReply_ QueryFileInfo
    procedure DoHandleCmdReply_QueryFileProgress(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal); //CtCmdReply_QueryFileProgress
    procedure DoHandleCmdReply_FileSegmentHash(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal); //CtCmdReply_GetFileSegmentHash    
    procedure DoHandleCmdReply_RequestFileData(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal; 
      const AIsSynchroCmd: Boolean; const ASynchroID: Word); //CtCmdReply_RequestFileData
    procedure DoHandleCmdReply_SearchFileUser(const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal); //CtCmdReply_ SearchFileUser

    {错误信息}
    procedure DoErrorInfo(const AInfo: string);

    {处理下载任务所需要的事件}
    function  DoTaskToGetServerInfo(const AServerStyle: TServerStyle; var AServerInfos: TAryServerInfo): Boolean;
    procedure DoCheckP2PConnectState(Sender: TxdDownTask; const AUserID: Cardinal);
    procedure DoUpdateHashToServer(Sender: TObject; const AFileHash, AWebHash: TxdHash);
    procedure DoStreamFree(Sender: TObject);

    {下载任务事件}
    procedure DoTaskDownSuccess(const ATask: TxdDownTask);
    procedure DoTaskDownFail(const ATask: TxdDownTask);

    {任务信息加载与保存}
    procedure LoadFromFile;
    procedure SaveToFile;
    procedure DoSaveTaskToFileByOwnData(AStream: TxdFileStream; const ATaskData: Pointer); //由外部根据需要保存到配置文件
    procedure DoLoadTaskFromFileByOwnData(AStream: TxdFileStream; var ATaskData: Pointer); //由外部根据需要读取配置文件

    {任务查询相关}
    procedure FillProgressInfo(const ATask: TxdDownTask; var AInfo: TTaskProgressInfo); //填充AInfo
    function  GetMaxTaskID: Integer; //获取当前列表中最大的任务ID
  private
    {自动生成信息}
    FActive: Boolean;
    FUdp: TxdUdpCommonClient;
    FServerManage: TxdServerManage;
    FTaskMaxP2PSourceCount: Integer;
    FOnGetTaskStreamID: TOnGetTaskStreamID;
    FPriorityDownWebHash: Boolean;
    FTaskInitRequestMaxBlockCount: Integer;
    FFileName: string;
    FHashManage: TxdClientHashManage;
    FThreadExcuteSpaceTime: Cardinal;
    FFileShareManage: TxdFileShareManage;
    FTaskInitRequestTableCount: Integer;
    FOnTaskFinished: TOnTaskFinished;
    FOnReadConfig: TOnReadConfig;
    FOnWriteConfig: TOnWriteConfig;
    procedure SetActive(const Value: Boolean);
    procedure SetUdp(const Value: TxdUdpCommonClient);
    procedure SetTaskMaxP2PSourceCount(const Value: Integer);
    procedure SetFileName(const Value: string);
    function  GetTaskCount: Integer;
    procedure SetThreadExcuteSpaceTime(const Value: Cardinal);
    procedure SetTaskInitRequestTableCount(const Value: Integer);
    procedure SetInitRequestMaxBlockCount(const Value: Integer);
    function  GetIsActiveTask: Boolean;
  public    
    {}
    property Active: Boolean read FActive write SetActive;
    property FileName: string read FFileName write SetFileName; //保存各个任务下载信息文件名称
    property ThreadExcuteSpaceTime: Cardinal read FThreadExcuteSpaceTime write SetThreadExcuteSpaceTime; //线程执行间隔

    {外部提供对象设置}
    property Udp: TxdUdpCommonClient read FUdp write SetUdp;
    property ServerManage: TxdServerManage read FServerManage write FServerManage;
    property HashManage: TxdClientHashManage read FHashManage write FHashManage;
    property FileShareManage: TxdFileShareManage read FFileShareManage write FFileShareManage;

    {任务初始化时设置使用属性}
    property TaskMaxP2PSourceCount: Integer read FTaskMaxP2PSourceCount write SetTaskMaxP2PSourceCount; //P2P数据源最大值
    property TaskPriorityDownWebHash: Boolean read FPriorityDownWebHash write FPriorityDownWebHash;
    property TaskInitRequestTableCount: Integer read FTaskInitRequestTableCount write SetTaskInitRequestTableCount;
    property TaskInitRequestMaxBlockCount: Integer read FTaskInitRequestMaxBlockCount write SetInitRequestMaxBlockCount;

    {读取}
    property TaskCount: Integer read GetTaskCount;
    property IsActiveTask: Boolean read GetIsActiveTask;

    {事件}
    property OnGetTaskStreamID: TOnGetTaskStreamID read FOnGetTaskStreamID write FOnGetTaskStreamID; //调用GetTaskStreamID时，调用的事件
    property OnTaskFinished: TOnTaskFinished read FOnTaskFinished write FOnTaskFinished;
    property OnReadConfig: TOnReadConfig read FOnReadConfig write FOnReadConfig;
    property OnWriteConfig: TOnWriteConfig read FOnWriteConfig write FOnWriteConfig;
  end;

implementation

const
  CtDownTaskManageVersion = 100;
  CtDefaultFileName = 'dtDownTaskInfo.dat';

{ TxdDownTaskManage }

procedure TxdDownTaskManage.ActiveManage;
begin
  try
    FManageThread := TThreadCheck.Create( DoThreadToDownTasks, ThreadExcuteSpaceTime );
//    TThreadCheck.Create( DoThreadToDownTasks, ThreadExcuteSpaceTime );
//    TThreadCheck.Create( DoThreadToDownTasks, ThreadExcuteSpaceTime );
    FActive := True;
  except
    UnActiveManage;
  end;
end;

function TxdDownTaskManage.AddDownTask(ApDownInfo: PDownTaskParam; const AAutoStartTask: Boolean): Boolean;
var
  i: Integer;
  task: TxdDownTask;
begin
  Result := (ApDownInfo^.FTaskName <> '') and (ApDownInfo^.FFileName <> '');
  if not Result then Exit;
  if Length(ApDownInfo^.FHttpSource) = 0 then
  begin
    if IsEmptyHash(ApDownInfo^.FFileHash) and IsEmptyHash(ApDownInfo^.FWebHash) then
    begin
      Result := False;
      Exit;
    end;
  end;
  
  LockManage;
  try
    //查找是否存在任务
    task := nil;
    if not IsEmptyHash(ApDownInfo^.FFileHash) then
      task := FindTask( hsFileHash, ApDownInfo^.FFileHash )
    else if not IsEmptyHash(ApDownInfo^.FWebHash) then
      task := FindTask( hsWebHash, ApDownInfo^.FWebHash )
    else
    begin
      for i := 0 to Length(ApDownInfo^.FHttpSource) - 1 do
      begin
        task := FindTask( ApDownInfo^.FHttpSource[i].FUrl );
        if Assigned(task) then
          Break;
      end;
    end;

    if Assigned(task) then
    begin
      ApDownInfo^.FTaskID := task.TaskID;
      if AAutoStartTask and not task.Active then
        task.Active := True;
      Result := True;
    end
    else
    begin
      //添加全新的下载任务，设置必要的参数
      task := TxdDownTask.Create;
      task.TaskData := ApDownInfo^.FTaskData;
      task.TaskID := ApDownInfo^.FTaskID;      
      if (ApDownInfo^.FTaskStyle >= Low(TDownTaskStyle)) and (ApDownInfo^.FTaskStyle <= High(TDownTaskStyle)) then      
        task.TaskStyle := ApDownInfo^.FTaskStyle
      else
        task.TaskStyle := dssDefaul;
      task.TaskName := ApDownInfo^.FTaskName;
      task.FileName := ApDownInfo^.FFileName;
      task.FileHash := ApDownInfo^.FFileHash;
      task.WebHash := ApDownInfo^.FWebHash;
      task.FileSize := ApDownInfo^.FFileSize;
      task.SegmentSize := ApDownInfo^.FSegmentSize;
      if FileExists(task.FileName) or FileExists(task.DownTempFileName) then
        task.InitFileFinishedInfos := ApDownInfo^.FFileFinishedInfos;

      for i := 0 to Length(ApDownInfo^.FP2PSource) - 1 do
        task.AddP2PSource( ApDownInfo^.FP2PSource[i].FIP, ApDownInfo^.FP2PSource[i].FPort, False );
      for i := 0 to Length(ApDownInfo^.FHttpSource) - 1 do
        task.AddHttpSource( ApDownInfo^.FHttpSource[i].FUrl, ApDownInfo^.FHttpSource[i].FReferUrl, 
                            ApDownInfo^.FHttpSource[i].FCookie, ApDownInfo^.FHttpSource[i].FTotoalByteCount );
              
      Result := DoAddDownTask( task );

      if Result then      
      begin
        ApDownInfo^.FTaskID := task.TaskID;
        if AAutoStartTask then
          task.Active := True;
      end;
    end;
  finally
    UnlockManage;
  end;
end;

constructor TxdDownTaskManage.Create;
begin
  FActive := False;
  FManageThread := nil;
  FTaskList := TList.Create;
  InitializeCriticalSection( FLock );
  FTaskMaxP2PSourceCount := 10;
  FTaskInitRequestTableCount := 2;
  FTaskInitRequestMaxBlockCount := 128;
  FThreadExcuteSpaceTime := 20;
  FTaskInitRequestTableCount := 0;
  FMinTaskID := GetTickCount;
  TaskPriorityDownWebHash := False;  
  FFileName := ExtractFilePath( ParamStr(0) ) + CtDefaultFileName; 
  LoadFromFile;
end;

function TxdDownTaskManage.DeleteTask(const ATaskID: Integer): Boolean;
begin
  Result := OptDownTask( ATaskID, optDelete );
end;

destructor TxdDownTaskManage.Destroy;
begin
  Active := False;
  SaveToFile;
  DeleteCriticalSection( FLock );
  inherited;
end;
                                      
function TxdDownTaskManage.DoAddDownTask(const ATask: TxdDownTask): Boolean;
begin
  with ATask do
  begin
    if (TaskID <= 0) or IsExistsTask(TaskID) then    
    begin
      TaskID := FMinTaskID;
      Inc( FMinTaskID )
    end
    else
    begin
      FMinTaskID := GetMaxTaskID;
      if TaskID > FMinTaskID then
      begin
        FMinTaskID := TaskID;
        Inc( FMinTaskID );
      end;
    end;
    UDP := FUdp;
    PriorityDownWebHash := TaskPriorityDownWebHash;
    InitRequestTableCount := TaskInitRequestTableCount;
    InitRequestMaxBlockCount := TaskInitRequestMaxBlockCount;
    MaxP2PSourceCount := FTaskMaxP2PSourceCount;
    OnGetServerInfo := DoTaskToGetServerInfo;
    OnCheckP2PConnectState := DoCheckP2PConnectState;
    OnUpdateHashInfo := DoUpdateHashToServer;
    OnStreamFree := DoStreamFree;
  end;
  Result := FTaskList.Add( ATask ) <> -1;
  if not Result then
    ATask.Free;
end;

procedure TxdDownTaskManage.DoCheckP2PConnectState(Sender: TxdDownTask; const AUserID: Cardinal);
begin
  if Assigned(FUdp) then
  begin
    if FUdp.ConnectToClient(AUserID, DoP2PConnectedStateEvent, Sender) = csNULL then
      Sender.SettingP2PSource( AUserID, 0, 0, False );;
  end;
end;

procedure TxdDownTaskManage.DoErrorInfo(const AInfo: string);
begin
  OutputDebugString( PChar(AInfo) );
end;

procedure TxdDownTaskManage.DoHandleCmdReply_FileSegmentHash(const AIP: Cardinal; const APort: Word;
  const ABuffer: pAnsiChar; const ABufLen: Cardinal);
var
  pCmd: PCmdReplyGetFileSegmentHashInfo;
  task: TxdDownTask;
begin
  if ABufLen < CtCmdReplyGetFileSegmentHashInfoSize then
  begin
    DoErrorInfo( '接收到的分段HASH验证信息出错' );
    Exit;
  end;
  pCmd := PCmdReplyGetFileSegmentHashInfo( ABuffer );
  LockManage;  
  try
    task := FindTask( hsFileHash, TxdHash(pCmd^.FFileHash) );
    if Assigned(task) then
      task.Cmd_RecvFileSegmentHash( AIP, APort, PByte(ABuffer), ABufLen );
  finally
    UnlockManage;
  end;
end;

procedure TxdDownTaskManage.DoHandleCmdReply_QueryFileInfo(const AIP: Cardinal; const APort: Word;
  const ABuffer: pAnsiChar; const ABufLen: Cardinal);
var
  task: TxdDownTask;
  pCmd: PCmdReplyQueryFileInfo;
begin
  if ABufLen < CtMinPackageLen + CtHashSize + 2 then
  begin
    DoErrorInfo( 'DoHandleCmdReply_FileExists 长度不正确' );
    Exit;
  end;
  
  pCmd := PCmdReplyQueryFileInfo(ABuffer);
  LockManage;
  try
    task := FindTask( pCmd^.FHashStyle, TxdHash(pCmd^.FHash) );
    if Assigned(task) then
      task.Cmd_RecvFileInfo( AIP, APort, pCmd );
  finally
    UnlockManage;
  end;                                                       
end;                                                                

procedure TxdDownTaskManage.DoHandleCmdReply_QueryFileProgress(const AIP: Cardinal; const APort: Word;
  const ABuffer: pAnsiChar; const ABufLen: Cardinal);
var
  pCmd: PCmdReplyQueryFileProgressInfo;
  task: TxdDownTask;
begin
  if ABufLen <= CtCmdReplyQueryFileProgressInfoSize then
  begin
    DoErrorInfo( 'DoHandleCmdReply_QueryFileProgress 接收到的数据长度过小' );
    Exit;
  end;
  
  pCmd := PCmdReplyQueryFileProgressInfo(ABuffer);
  LockManage;
  try
    task := FindTask( pCmd^.FHashStyle, TxdHash( pCmd^.FHash) );
    if Assigned(task) then
      task.Cmd_RecvFileProgress( AIP, APort, pCmd );
  finally
    UnlockManage;
  end;
end;

procedure TxdDownTaskManage.DoHandleCmdReply_RequestFileData(const AIP: Cardinal; const APort: Word;
  const ABuffer: pAnsiChar; const ABufLen: Cardinal; const AIsSynchroCmd: Boolean; const ASynchroID: Word);
var
  pCmd: PCmdReplyRequestFileInfo;
  task: TxdDownTask;
begin
  pCmd := PCmdReplyRequestFileInfo(ABuffer);
  LockManage;
  try
    task := FindTask( pCmd^.FHashStyle, TxdHash( pCmd^.FHash) );
    if Assigned(task) then
      task.Cmd_RecvFileData( AIP, APort, PByte(ABuffer), ABufLen );
  finally
    UnlockManage;
  end;
end;

procedure TxdDownTaskManage.DoHandleCmdReply_SearchFileUser(const AIP: Cardinal; const APort: Word;
  const ABuffer: pAnsiChar; const ABufLen: Cardinal);
var
  pCmd: PCmdReplySearchFileUserInfo;
  task: TxdDownTask;
begin
  if ABufLen < CtCmdReplySearchFileUserInfoSize - 4 then
  begin
    DoErrorInfo( '命令 eply_SearchFileUser 长度不正确' );
    Exit;
  end;
  pCmd := pCmdReplySearchFileUserInfo(ABuffer);
  if pCmd^.FUserCount = 0 then Exit;

  
  LockManage;
  try
    task := FindTask( pCmd^.FHashStyle, TxdHash(pCmd^.FHash) );  
    if Assigned(task) then
      task.Cmd_RecvSearchFileUser( AIP, APort, ABuffer, ABufLen );
  finally
    UnlockManage;
  end;
end;

procedure TxdDownTaskManage.DoHandleDownTaskCmdEvent(const ACmd: Word; const AIP: Cardinal; const APort: Word;
  const ABuffer: pAnsiChar; const ABufLen: Cardinal; const AIsSynchroCmd: Boolean; const ASynchroID: Word);
begin
  case ACmd of
    CtCmdReply_QueryFileInfo: DoHandleCmdReply_QueryFileInfo( AIP, APort, ABuffer, ABufLen );
    CtCmdReply_QueryFileProgress: DoHandleCmdReply_QueryFileProgress( AIP, APort, ABuffer, ABufLen );
    CtCmdReply_RequestFileData: DoHandleCmdReply_RequestFileData( AIP, APort, ABuffer, ABufLen, AIsSynchroCmd, ASynchroID );
    CtCmdReply_GetFileSegmentHash: DoHandleCmdReply_FileSegmentHash( AIP, APort, ABuffer, ABufLen );
    CtCmdReply_SearchFileUser: DoHandleCmdReply_SearchFileUser( AIP, APort, ABuffer, ABufLen );
  end;
end;

procedure TxdDownTaskManage.DoHandleTask(Ap: Pointer);
var
  p: PHandleTashParam;
  nStreamID: Cardinal;
  bReGet: Boolean;
  nTryCount: Integer;

  procedure DoGetTaskStreamID;
  begin
    LockManage;
    try
      if FTaskList.IndexOf(p^.FTask) <> -1 then
      begin
        if not p^.FTask.Active then
        begin
          if p^.FFullPower then
            FullPowerDownTask( p^.FTask.TaskID, False )
          else if p^.FAutoStart then
            p^.FTask.Active := True;
        end;

        if p^.FTask.Active then
        begin
          nStreamID := p^.FTask.StreamID;
          bReGet := nStreamID = 0;
        end;
      end;
    finally
      UnlockManage;
    end;
  end;
  
begin
  p := Ap;
  bReGet := False;
  
  if not Assigned(p) then Exit;  

  try
    DoGetTaskStreamID;
    nTryCount := 0;
    while bReGet and (nTryCount < 100) do
    begin
      Inc( nTryCount );
      Sleep( 100 );
      DoGetTaskStreamID;
    end;

    if Assigned(OnGetTaskStreamID) then
      OnGetTaskStreamID( p^.FTask.TaskID, nStreamID );
  finally
    if Assigned(p) then
      Dispose( p );
  end;
end;

procedure TxdDownTaskManage.DoLoadTaskFromFileByOwnData(AStream: TxdFileStream; var ATaskData: Pointer);
begin
  if Assigned(OnReadConfig) then
    OnReadConfig( AStream, ATaskData );
end;

procedure TxdDownTaskManage.DoP2PConnectedStateEvent(const AUserID: Cardinal; const AConnectState: TConnectState;
  const AParam: Pointer);
var
  task: TxdDownTask;
  p: PClientPeerInfo;
  nIP: Cardinal;
  nPort: Word;
begin  
  LockManage; 
  try
    if -1 <> FTaskList.IndexOf( AParam ) then
    begin
      if Assigned(FUdp) and Assigned(FUdp.P2PUserManage) then
      begin
        task := AParam;
        FUdp.P2PUserManage.LockList;
        try
          p := FUdp.P2PUserManage.FindUserInfo( AUserID );
          if Assigned(p) then
          begin
            GetClientIP( p, nIP, nPort );
            task.SettingP2PSource( AUserID, nIP, nPort, p^.FConnectState = csConnetSuccess );
          end
          else
            task.SettingP2PSource( AUserID, 0, 0, False );
        finally
          FUdp.P2PUserManage.UnLockList;
        end;
      end;
    end;
  finally
    UnlockManage;
  end;
end;

procedure TxdDownTaskManage.DoSaveTaskToFileByOwnData(AStream: TxdFileStream; const ATaskData: Pointer);
begin
  if Assigned(OnWriteConfig) then
    OnWriteConfig( AStream, ATaskData );
end;

procedure TxdDownTaskManage.DoStreamFree(Sender: TObject);
var
  stream: TxdFileSegmentStream;
begin
  if Assigned(FileShareManage) then
  begin
    if (Sender is TxdFileSegmentStream) then
    begin
      stream := Sender as TxdFileSegmentStream;
      if stream.IsComplete then      
        FileShareManage.AddLocalFileToShare( Sender as TxdFileSegmentStream, '任务下载' );
    end;
  end;
end;

procedure TxdDownTaskManage.DoTaskDownFail(const ATask: TxdDownTask);
begin
  if Assigned(OnTaskFinished) then
    OnTaskFinished( ATask, False );
  OutputDebugString( '任务下载失败' );
end;

procedure TxdDownTaskManage.DoTaskDownSuccess(const ATask: TxdDownTask);
begin
  if Assigned(OnTaskFinished) then
    OnTaskFinished( ATask, True );
  OutputDebugString( '任务下载完成' );
  OutputDebugString( PChar( 'FileHash: ' + HashToStr(ATask.FileHash)) );
  OutputDebugString( PChar( 'WebHash: ' + HashToStr(ATask.WebHash)) );
end;

function TxdDownTaskManage.DoTaskToGetServerInfo(const AServerStyle: TServerStyle;
  var AServerInfos: TAryServerInfo): Boolean;
begin
  if Assigned(FServerManage) then
    Result := FServerManage.GetServerGroup( AServerStyle, AServerInfos ) > 0
  else
    Result := False;
end;

procedure TxdDownTaskManage.DoThreadToDownTasks;
var
  i: Integer;
  task: TxdDownTask;
begin  
  LockManage;
  try
    for i := FTaskList.Count - 1 downto 0 do
    begin
      task := FTaskList[i];
      if task.Active then
        task.DoDownTaskThreadExecute;
      
      if task.DownSuccess then
      begin
        //任务下载成功
        DoTaskDownSuccess( task );
        task.Active := False;
        task.Free;
        FTaskList.Delete( i );
      end
      else if task.DownFail then
      begin
        task.Active := False;
        DoTaskDownFail( task );
      end;
    end;
  finally
    UnlockManage;
  end;
end;

procedure TxdDownTaskManage.DoUpdateHashToServer(Sender: TObject; const AFileHash, AWebHash: TxdHash);
begin
  if Assigned(FHashManage) then
    FHashManage.AddHash( AFileHash, AWebHash );
end;

procedure TxdDownTaskManage.FillProgressInfo(const ATask: TxdDownTask; var AInfo: TTaskProgressInfo);
begin
  AInfo.FTaskID := ATask.TaskID;
  AInfo.FActive := ATask.Active;
  AInfo.FTaskName := ATask.TaskName;
  AInfo.FFileName := ATask.FileName;
  AInfo.FFileSize := ATask.FileSize;
  AInfo.FCompletedSize := ATask.CurFinishedFileSize;
  AInfo.FCurSpeed := ATask.CurSpeed;
  AInfo.FAdvSpedd := ATask.Speed;
  AInfo.FFail := ATask.DownFail;
  AInfo.FTaskStyle := ATask.TaskStyle;
end;

function TxdDownTaskManage.FindTask(const AUrl: string): TxdDownTask;
var
  i: Integer;
  task: TxdDownTask;
begin
  Result := nil;
  for i := 0 to FTaskList.Count - 1 do
  begin
    task := FTaskList[i];
    if task.IsExistsHttpSource(AUrl) then
    begin
      Result := task;
      Break;
    end;
  end;
end;

procedure TxdDownTaskManage.FullPowerDownTask(const ATaskID: Integer; const ALock: Boolean);
var
  i: Integer;
  task: TxdDownTask;
  bOK: Boolean;
begin    
  if ALock then  
    LockManage;
  try
    bOK := False;
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.TaskID = ATaskID then
      begin
        bOK := True;
        task.Active := True;
        Break;
      end;
    end;
    if bOK then
    begin
      for i := 0 to FTaskList.Count - 1 do
      begin
        task := FTaskList[i];
        if task.TaskID <> ATaskID then
          task.Active := False;
      end;
    end;
  finally
    if ALock then    
      UnlockManage;
  end;
end;

function TxdDownTaskManage.GetIsActiveTask: Boolean;
var
  i: Integer;
  task: TxdDownTask;
begin
  Result := False;
  for i := 0 to FTaskList.Count - 1 do
  begin
    task := FTaskList[i];
    if task.Active then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TxdDownTaskManage.GetMaxTaskID: Integer;
var
  i: Integer;
  task: TxdDownTask;
begin
  Result := 0;
  for i := 0 to FTaskList.Count - 1 do
  begin
    task := FTaskList[i];
    if task.TaskID > Result then
      Result := task.TaskID;
  end;
end;

function TxdDownTaskManage.GetSoftUpdateTaskID: Integer;
var
  i: Integer;
  task: TxdDownTask;
begin
  Result := -1;
  LockManage;
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.TaskStyle = dssSoftUpdata then
      begin
        Result := task.TaskID;
        Break;
      end;
    end;
  finally
    UnlockManage;
  end;
end;

function TxdDownTaskManage.GetTaskCount: Integer;
begin
  Result := FTaskList.Count;
end;

function TxdDownTaskManage.GetTaskDataByTaskID(const ATaskID: Integer): Pointer;
var
  i: Integer;
  task: TxdDownTask;
begin
  Result := nil;
  LockManage;
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.TaskID = ATaskID then
      begin
        Result := task.TaskData;
        Break;
      end;
    end;
  finally
    UnlockManage;
  end;
end;

function TxdDownTaskManage.GetTaskDownDetailByIndex(const ATaskIndex: Integer; var AInfo: TTaskDownDetailInfo): Boolean;
var
  task: TxdDownTask;
begin
  LockManage;
  try
    Result := (ATaskIndex >= 0) and (ATaskIndex < FTaskList.Count);
    if Result then
    begin
      task := FTaskList[ATaskIndex];
      task.GetTaskDownDetail( AInfo );
    end;
  finally
    UnlockManage;
  end;
end;

function TxdDownTaskManage.GetTaskDownDetailByTaskID(const ATaskID: Integer; var AInfo: TTaskDownDetailInfo): Boolean;
var
  i: Integer;
  task: TxdDownTask;
begin
  Result := False;
  task := nil;
  LockManage;
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.TaskID = ATaskID then
      begin
        Result := True;
        Break;
      end;
    end;
    if Result then
      task.GetTaskDownDetail( AInfo );
  finally
    UnlockManage;
  end;
end;

function TxdDownTaskManage.GetTaskProgressByIndex(const ATaskIndex: Integer; var AInfo: TTaskProgressInfo): Boolean;
var
  task: TxdDownTask;
begin
  LockManage;
  try
    Result := (ATaskIndex >= 0) and (ATaskIndex < FTaskList.Count);
    if Result then
    begin
      task := FTaskList[ATaskIndex];
      FillProgressInfo( task, AInfo );
    end;
  finally
    UnlockManage;
  end;
end;

function TxdDownTaskManage.GetTaskProgressByTaskID(const ATaskID: Integer; var AInfo: TTaskProgressInfo): Boolean;
var
  i: Integer;
  task: TxdDownTask;
begin
  Result := False;
  task := nil;
  LockManage;
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.TaskID = ATaskID then
      begin
        Result := True;
        Break;
      end;
    end;
    if Result then
      FillProgressInfo( task, AInfo );
  finally
    UnlockManage;
  end;
end;

function TxdDownTaskManage.GetTaskStreamID(const ATaskID: Integer): Integer;
var
  task: TxdDownTask;
  i: Integer;
begin
  Result := -1;
  LockManage;
  try    
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.TaskID = ATaskID then
      begin
        Result := task.StreamID;        
        Break;
      end;
    end;      
  finally
    UnlockManage;
  end;
end;

function TxdDownTaskManage.GetTaskStreamID(const ATaskID: Integer; const AFullPower, AAutoStartTask, AByThread: Boolean): Boolean;
var
  i: Integer;
  task: TxdDownTask;
  p: PHandleTashParam;
begin
  Result := False;
  task := nil;
  LockManage;
  try    
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.TaskID = ATaskID then
      begin
        Result := True;        
        Break;
      end;
    end;      
  finally
    UnlockManage;
  end;

  if Result then
  begin
    New( p );
    p^.FTask := task;
    p^.FFullPower := AFullPower;
    p^.FAutoStart := AAutoStartTask;
    if AByThread then
      RunningByThread(DoHandleTask, p )
    else
      DoHandleTask( p );
  end;
end;

function TxdDownTaskManage.IsExistsTask(const ATaskID: Integer): Boolean;
var
  i: Integer;
  task: TxdDownTask;
begin
  Result := False;
  for i := 0 to FTaskList.Count - 1 do
  begin
    task := FTaskList[i];
    if task.TaskID = ATaskID then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TxdDownTaskManage.FindTask(const AHashStyle: THashStyle; const AHash: TxdHash): TxdDownTask;
var
  i: Integer;
  task: TxdDownTask;
begin
  Result := nil;
  for i := 0 to FTaskList.Count - 1 do
  begin
    task := FTaskList[i];
    case AHashStyle of
      hsFileHash: 
      begin
        if HashCompare(AHash, task.FileHash) then
        begin
          Result := task;
          Break;
        end;
      end;
      hsWebHash: 
      begin
        if HashCompare(AHash, task.WebHash) then
        begin
          Result := task;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TxdDownTaskManage.LoadFromFile;
var
  f: TxdFileStream;
  i, j, nCount, nHttpCount, nFinishedCount: Integer;
  param: TDownTaskParam;
begin
  if not FileExists(FFileName) then Exit;
{
    配置保存方案
        配置版本号(Integer) 下载数量(Integer), 多个以下结构
        begin        
          任务ID( Integer )
          任务类型( Byte )
          任务名称（word + string)
          文件名称（word + string)
          文件大小(Int64)
          文件分段大小(Integer)
          文件Hash( 16 )
          Web Hasn( 16 )
          Http服务器数量(Integer)
            URL( string )
            Refer( string )
            cookie( string )
            TotalByteCount(Integer)
          已下载完成信息数量(Integer)
            BeginPos( Int64 )
            Size( Int64 )
        end;
end;}
  SetLength( param.FP2PSource, 0 );
  f := TxdFileStream.Create( FFileName, fmOpenRead );
  try
    if f.ReadInteger <> CtDownTaskManageVersion then
    begin
      DoErrorInfo( '读取文件版本号不正确' );
      Exit;
    end;
    nCount := f.ReadInteger;
    for i := 0 to nCount - 1 do
    begin
      param.FTaskID := f.ReadInteger;
      param.FTaskStyle := TDownTaskStyle( f.ReadByte );
      param.FTaskName := f.ReadStringEx;
      param.FFileName := f.ReadStringEx;
      param.FFileSize := f.ReadInt64;
      param.FSegmentSize := f.ReadInteger;
      f.ReadLong( param.FFileHash, CtHashSize ); //文件HASH
      f.ReadLong( param.FWebHash, CtHashSize );
      
      nHttpCount := f.ReadInteger;
      SetLength( param.FHttpSource, nHttpCount );
      for j := 0 to nHttpCount - 1 do
      begin
        param.FHttpSource[j].FUrl :=  f.ReadStringEx;
        param.FHttpSource[j].FReferUrl := f.ReadStringEx;
        param.FHttpSource[j].FCookie := f.ReadStringEx;
        param.FHttpSource[j].FTotoalByteCount := f.ReadInteger;
      end;
      nFinishedCount := f.ReadInteger;
      SetLength( param.FFileFinishedInfos, nFinishedCount );
      for j := 0 to nFinishedCount - 1 do
      begin
        param.FFileFinishedInfos[j].FBeginPos := f.ReadInt64;
        param.FFileFinishedInfos[j].FSize := f.ReadInt64;
      end;        
      DoLoadTaskFromFileByOwnData( f, param.FTaskData );
      AddDownTask( @param );
    end;
  finally
    f.Free;
    SetLength( param.FHttpSource, 0 );
    SetLength( param.FFileFinishedInfos, 0 );
  end;
end;

procedure TxdDownTaskManage.LockManage;
begin
  EnterCriticalSection( FLock );
end;

function TxdDownTaskManage.MyTestDown(AHash: string; const AIsFileHash: Boolean): Integer;
var
  param: TDownTaskParam;
begin
//  AHash := '168DA4D77B8A31AD7B1BCFDA2CC3A001';//'9AF6A8052D284C15EC88E7A40A9B22AF';//
  FillChar( param, CtDownTaskParamSize, 0 );
  param.FTaskName := '下载测试';
  param.FFileName := ExtractFilePath(ParamStr(0)) + 'TestDownFiles\' + IntToStr(GetTickCount) + '.rmvb';//'.wmv';// 

  StrToHash( AHash, param.FFileHash );

  param.FWebHash := CtEmptyHash;
//  SetLength(param.FHttpSource, 1);
//  param.FHttpSource[0].FUrl := 'http://203.86.5.87:9090/ktv3/礼光1-87/21/男/X谢霆锋/谢霆锋-大海.RMVB';

  if AddDownTask( @param, True ) then
    Result := param.FTaskID
  else
    Result := -1;  
  
end;

function TxdDownTaskManage.OptDownTask(const ATaskID: Integer; const AOpt: TDownTaskOpt; const ALock: Boolean): Boolean;
var
  i: Integer;
  task: TxdDownTask;
begin
  Result := False;
  if ALock then  
    LockManage;
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      if task.TaskID = ATaskID then
      begin
        case AOpt of
          optStart: task.Active := True;
          optStop:  task.Active := False;
          optDelete:  
          begin
            FTaskList.Delete( i );
            task.Free;
          end;
          optReverse: task.Active := not task.Active;
        end;
        Result := True;
        Break;
      end;
    end;
  finally
    if ALock then
      UnlockManage;
  end;
end;

function TxdDownTaskManage.ReverseTask(const ATaskID: Integer): Boolean;
begin
  Result := OptDownTask( ATaskID, optReverse );
end;

procedure TxdDownTaskManage.SaveToFile;
var
  i, j: Integer;
  FinishedList: TList;
  f: TxdFileStream;
  http: THttpSourceInfo;
  pFinsihed: PFileFinishedInfo;
  task: TxdDownTask;
  md: TxdHash;

  procedure ClearList;
  var
    i: Integer;
  begin
    for i := 0 to FinishedList.Count - 1 do
      Dispose( FinishedList[i] );
    FinishedList.Clear;
  end;
begin
  if FFileName = '' then
    FFileName := ExtractFilePath( (ParamStr(0)) ) + CtDefaultFileName;
  if FTaskList.Count = 0 then 
  begin
    if FileExists(FFileName) then
      DeleteFile( FFileName );
    Exit;
  end;

{
    配置保存方案
        配置版本号(Integer) 下载数量(Integer), 多个以下结构
        begin
          任务ID( Integer )
          任务类型( Byte )
          任务名称（word + string)
          文件名称（word + string)
          文件大小(Int64)
          文件分段大小(Integer)
          文件Hash( 16 )
          Web Hasn( 16 )
          Http服务器数量(Integer)
            URL( string )
            Refer( string )
            cookie( string )
            TotalByteCount(Integer)
          已下载完成信息数量(Integer)
            BeginPos( Int64 )
            Size( Int64 )
        end;
end;}
  f := TxdFileStream.Create( FFileName, fmCreate );
  FinishedList := TList.Create;
  LockManage;
  try
    f.WriteInteger( CtDownTaskManageVersion );
    f.WriteInteger( FTaskList.Count );
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      
      //任务常规信息
      f.WriteInteger( task.TaskID ); //ID
      f.WriteByte( Byte(task.TaskStyle) ); //Style
      f.WriteStringEx( task.TaskName ); //任务名称
      f.WriteStringEx( task.FileName ); //文件名称
      f.WriteInt64( task.FileSize ); //文件大小
      f.WriteInteger( task.FileSegmentSize ); //分段大小
      md := task.FileHash;
      f.WriteLong( md.v[0], CtHashSize ); //文件HASH
      md := task.WebHash;
      f.WriteLong( md.v[0], CtHashSize ); //WEB HASH

      //Http服务器信息
      f.WriteInteger( task.CurHttpSourceCount );
      for j := 0 to task.CurHttpSourceCount - 1 do
      begin
        task.GetHttpSource( j, http );
        f.WriteStringEx( http.FUrl );
        f.WriteStringEx( http.FReferUrl );
        f.WriteStringEx( http.FCookies );
        f.WriteInteger( http.FTotalRecvByteCount );
      end;

      //下载完成信息
      ClearList;
      task.GetFinishedFileInfo( FinishedList );
      f.WriteInteger( FinishedList.Count );
      for j := 0 to FinishedList.Count - 1 do
      begin
        pFinsihed := FinishedList[j];
        f.WriteInt64( pFinsihed^.FBeginPos );
        f.WriteInt64( pFinsihed^.FSize );
      end;

      DoSaveTaskToFileByOwnData( f, task.TaskData );
    end;
  finally
    UnlockManage;
    ClearList;
    FinishedList.Free;
  end;
end;

procedure TxdDownTaskManage.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      ActiveManage
    else
      UnActiveManage;
  end;
end;

procedure TxdDownTaskManage.SetFileName(const Value: string);
begin
  if CompareText(Value, FFileName) <> 0 then
  begin
    if FileExists(FFileName) then
      DeleteFile( FFileName );
    FFileName := Value;
    LoadFromFile;
  end;
end;

procedure TxdDownTaskManage.SetInitRequestMaxBlockCount(const Value: Integer);
begin
  if (Value > 0) and (FTaskInitRequestMaxBlockCount <> Value) then
    FTaskInitRequestMaxBlockCount := Value;
end;

procedure TxdDownTaskManage.SetTaskInitRequestTableCount(const Value: Integer);
begin
  if (Value > 0) and (FTaskInitRequestTableCount <> Value) then
    FTaskInitRequestTableCount := Value;
end;

procedure TxdDownTaskManage.SetTaskMaxP2PSourceCount(const Value: Integer);
begin
  if (FTaskMaxP2PSourceCount <> Value) and (Value > 0) then  
    FTaskMaxP2PSourceCount := Value;
end;

procedure TxdDownTaskManage.SetThreadExcuteSpaceTime(const Value: Cardinal);
begin
  if (Value <> FThreadExcuteSpaceTime) and (Value > 0) then
  begin
    FThreadExcuteSpaceTime := Value;
    if Assigned(FManageThread) then
      FManageThread.SpaceTime := FThreadExcuteSpaceTime;
  end;
end;

procedure TxdDownTaskManage.SetUdp(const Value: TxdUdpCommonClient);
begin
  FUdp := Value;
  if Assigned(FUdp) then
    FUdp.OnDownTaskCmdEvent := DoHandleDownTaskCmdEvent;
end;

function TxdDownTaskManage.StartTask(const ATaskID: Integer): Boolean;
begin
  Result := OptDownTask( ATaskID, optStart );
end;

function TxdDownTaskManage.StopTask(const ATaskID: Integer): Boolean;
begin
  Result := OptDownTask( ATaskID, optStop );
end;

procedure TxdDownTaskManage.UnActiveManage;
var
  i: Integer;
  task: TxdDownTask;
begin
  FActive := False;
  FreeAndNil( FManageThread );

  LockManage;
  try
    for i := 0 to FTaskList.Count - 1 do
    begin
      task := FTaskList[i];
      task.Active := False;
    end;
  finally
    UnlockManage;
  end;
end;

procedure TxdDownTaskManage.UnlockManage;
begin
  LeaveCriticalSection( FLock );
end;

end.
