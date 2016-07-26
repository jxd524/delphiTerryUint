unit uJxdThread;

interface

uses
  Classes;

type
  TOnRun = procedure of object;
  TRunOneThread = class(TThread)
  public
    constructor Create(ARun: TOnRun);
    destructor  Destroy; override;
  protected
    procedure Execute; override;
  private
    FRun: TOnRun;
  end;

procedure RunningByThread(ARun: TOnRun);

implementation

procedure RunningByThread(ARun: TOnRun);
begin
  TRunOneThread.Create( ARun );
end;

{ TRunOneThread }

constructor TRunOneThread.Create(ARun: TOnRun);
begin
  FRun := ARun;
  FreeOnTerminate := True;
  inherited Create( False );
end;

destructor TRunOneThread.Destroy;
begin

  inherited;
end;

procedure TRunOneThread.Execute;
begin
  inherited;
  FRun;
end;

end.
