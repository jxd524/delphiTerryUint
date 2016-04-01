unit uJxdThread;

interface

uses
  Windows, Classes;

type
  TOnRun = procedure of object;
  TOnRunEx = procedure(Ap: Pointer) of object;
  TOnRunGlb = procedure(Ap: Pointer);

  TRunOneThread = class(TThread)
  public
    constructor Create(ARun: TOnRun); overload;
    constructor Create(ARun: TOnRunEx; Ap: Pointer); overload;
    constructor Create(ARun: TOnRunGlb; Ap: Pointer); overload;

    destructor Destroy; override;
  protected
    procedure Execute; override;
  private
    FPoint: Pointer;
    FRun: TOnRun;
    FRunEx: TOnRunEx;
    FRunGlb: TOnRunGlb;
  end;

  TThreadCheck = class(TThread)
  public
    SpaceTime: Cardinal;

    procedure ActiveToCheck;
    procedure AutoFreeSelf;

    constructor Create(ARun: TOnRun; ASpaceTime: Cardinal); overload;
    constructor Create(ARun: TOnRunEx; Ap: Pointer; ASpaceTime: Cardinal); overload;

    destructor Destroy; override;
  protected
    procedure InitClass;
    procedure Execute; override;
  private
    FRun: TOnRun;
    FRunEx: TOnRunEx;
    FRunExParam: Pointer;
    FActive, FCloseing: Boolean;
    FWaitEvent: Cardinal;
    FAutoFreeSelf: Boolean;
  public
    property Active: Boolean read FActive;
  end;

procedure RunningByThread(ARun: TOnRun); overload;
procedure RunningByThread(ARun: TOnRunEx; Ap: Pointer); overload;
procedure RunningByThread(ARun: TOnRunGlb; Ap: Pointer); overload;

implementation

procedure RunningByThread(ARun: TOnRun);
begin
  TRunOneThread.Create(ARun);
end;

procedure RunningByThread(ARun: TOnRunEx; Ap: Pointer);
begin
  TRunOneThread.Create(ARun, Ap);
end;

procedure RunningByThread(ARun: TOnRunGlb; Ap: Pointer);
begin
  TRunOneThread.Create(ARun, Ap);
end;

{ TRunOneThread }

constructor TRunOneThread.Create(ARun: TOnRun);
begin
  FRun := ARun;
  FRunEx := nil;
  FRunGlb := nil;
  FreeOnTerminate := True;
  inherited Create(False);
end;

constructor TRunOneThread.Create(ARun: TOnRunEx; Ap: Pointer);
begin
  FRun := nil;
  FRunEx := ARun;
  FRunGlb := nil;
  FPoint := Ap;
  FreeOnTerminate := True;
  inherited Create(False);
end;

constructor TRunOneThread.Create(ARun: TOnRunGlb; Ap: Pointer);
begin
  FRun := nil;
  FRunEx := nil;
  FRunGlb := ARun;
  FPoint := Ap;
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TRunOneThread.Destroy;
begin

  inherited;
end;

procedure TRunOneThread.Execute;
begin
  inherited;
  if Assigned(FRun) then
    FRun
  else if Assigned(FRunEx) then
    FRunEx(FPoint)
  else if Assigned(FRunGlb) then
    FRunGlb(FPoint);
end;

{ TThreadCheck }

constructor TThreadCheck.Create(ARun: TOnRun; ASpaceTime: Cardinal);
begin
  FRun := ARun;
  FRunEx := nil;
  SpaceTime := ASpaceTime;
  InitClass;
  inherited Create(False);
end;

procedure TThreadCheck.ActiveToCheck;
begin
  SetEvent(FWaitEvent);
end;

procedure TThreadCheck.AutoFreeSelf;
begin
  FreeOnTerminate := True;
  FAutoFreeSelf := True;
  SetEvent(FWaitEvent);
end;

constructor TThreadCheck.Create(ARun: TOnRunEx; Ap: Pointer; ASpaceTime: Cardinal);
begin
  FRun := nil;
  FRunEx := ARun;
  FRunExParam := Ap;
  SpaceTime := ASpaceTime;
  InitClass;
  inherited Create(False);
end;

destructor TThreadCheck.Destroy;
begin
  if FActive then
  begin
    FCloseing := True;
    FActive := False;
    SetEvent(FWaitEvent);
    while FCloseing do
      Sleep(10);
  end;
  inherited;
end;

procedure TThreadCheck.Execute;
begin
  FActive := True;
  while FActive do
  begin
    if FAutoFreeSelf then
    begin
      FActive := False;
      Break;
    end;
    
    WaitForSingleObject(FWaitEvent, SpaceTime); 
    
    if FAutoFreeSelf then
    begin
      FActive := False;
      Break;
    end;
    
    try
      if Assigned(FRun) then
        FRun;
      if Assigned(FRunEx) then
        FRunEx(FRunExParam);
    except
    end;
    
  end;
  FCloseing := False;
end;

procedure TThreadCheck.InitClass;
begin
  FActive := False;
  FCloseing := False;
  FAutoFreeSelf := False;
  FWaitEvent := CreateEvent(nil, False, False, nil);
end;

end.

