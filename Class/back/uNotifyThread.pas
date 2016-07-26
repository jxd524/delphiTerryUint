{
单元名称: uNotifyThread
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: 数据结构操作
开始时间: 2010-09-13
修改时间: 2010-09-13(最后修改)

          TNotifyThread 为一次性消费产品，
          当Active为True,开始间隔的调用事件：TThreadNotifyEvnet；
          之后将Active设置为False后，线程将以同步的方式停止，即线程停止。 而且类也会被线程自动释放。
          使用时应该注意此设计方式

          当类创建时，线程开始运行。当Active为True时，调用通知函数. 当Active为False时，释放对象及资源

          CreateNotifyThread 创建一个运行于线程的函数
          EndNotifyThread 停止通知函数的运行

}
unit uNotifyThread;

interface
uses Windows, Classes, SysUtils, Forms;

type
  TThreadNotifyEvnet = function: Boolean of object;  //返回False: 关闭线程
  TNotifyThread = class(TThread)
  private
    FWaitEvent: Cardinal;
    FSpaceTime: Cardinal;
    FNotify: TThreadNotifyEvnet;
    FActive, FRunning, FCloseed: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetSpaceTime(const Value: Cardinal);
  protected
    procedure Execute; override;
    procedure ActiveNotify;
    procedure UnActiveNotify;
  public
    constructor Create(const ANotifyEvent: TThreadNotifyEvnet);
    destructor Destroy; override;
    property SpaceTime: Cardinal read FSpaceTime write SetSpaceTime;
    property Active: Boolean read FActive write SetActive;
  end;

function  CreateNotifyThread(const AEvent: TThreadNotifyEvnet; const ASpaceTime: Cardinal): TNotifyThread;
procedure EndNotifyThread(var AThread: TNotifyThread);

implementation

function  CreateNotifyThread(const AEvent: TThreadNotifyEvnet; const ASpaceTime: Cardinal): TNotifyThread;
begin
  Result := TNotifyThread.Create( AEvent );
  Result.SpaceTime := ASpaceTime;
  Result.Active := True;
end;
procedure EndNotifyThread(var AThread: TNotifyThread);
begin
  if Assigned(AThread) then
  begin
    AThread.Active := False;
    AThread := nil;
  end;
end;

{ TNotifyThread }

procedure TNotifyThread.ActiveNotify;
begin
  FActive := True;
end;

procedure TNotifyThread.UnActiveNotify;
begin
  FActive := False;
  FRunning := False;
  SetEvent( FWaitEvent );
  while not FCloseed do
  begin
    Application.ProcessMessages;
    Sleep(10);
  end;
end;

constructor TNotifyThread.Create(const ANotifyEvent: TThreadNotifyEvnet);
begin
  if not Assigned(ANotifyEvent) then
    raise Exception.Create( '检查线程的 NotifyEvent 不能为nil' );

  FSpaceTime := 500;
  FActive := False;
  FNotify := ANotifyEvent;
  FWaitEvent := CreateEvent( nil, False, False, '' );
  FreeOnTerminate := True;
  FRunning := True;
  FCloseed := False;
  inherited Create( False );
end;

destructor TNotifyThread.Destroy;
begin
  Active := False;
  CloseHandle( FWaitEvent );
  inherited;
end;

procedure TNotifyThread.Execute;
begin
  while FRunning do
  begin
    WaitForSingleObject( FWaitEvent, FSpaceTime );
    if not FRunning then Break;
    if Active and not FNotify then Break;
  end;
  FCloseed := True;
end;

procedure TNotifyThread.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      ActiveNotify
    else
      UnActiveNotify;
  end;
end;

procedure TNotifyThread.SetSpaceTime(const Value: Cardinal);
begin
  if FSpaceTime <> Value then
    FSpaceTime := Value;
end;

end.
