unit uSimpleLog;

interface
  uses Classes, SysUtils;

{$M+}
type
  TSimpleLog = class(TObject)
  private
    FLog: TStringList;
    FFileName: string;
    procedure SetFileName(const Value: string);
  public
    procedure AddLog(const AMsg: string);
    procedure Flush(AFileName: string = 'Log.txt');
  public
    constructor Create;
    destructor Destroy; override;
  published
    property FileName: string read FFileName write SetFileName;
  end;

procedure Log(const AMsg: string; AFileName: string = 'Log.txt');

implementation

var
  _Log: TSimpleLog;
{ TSimpleLog }

procedure Log(const AMsg: string; AFileName: string);
begin
  _Log.AddLog( AMsg );
  _Log.Flush( AFileName );
end;

procedure TSimpleLog.AddLog(const AMsg: string);
begin
  FLog.Add( FormatdateTime('yyyyƒÍmm‘¬dd»’ hh:nn:ss    ',Now()) + AMsg );
end;

constructor TSimpleLog.Create;
begin
  FLog := TStringList.Create;
  FFileName := 'Log.txt';
end;

destructor TSimpleLog.Destroy;
begin
  FLog.SaveToFile( FFileName );
  inherited;
end;

procedure TSimpleLog.Flush(AFileName: string);
begin
  if AFileName = '' then AFileName := FFileName;
  if AFileName = '' then
  begin
    AFileName := 'Log.txt';
    FFileName := AFileName;
  end;
  FLog.SaveToFile( AFileName );
  FLog.Free;
  FLog := TStringList.Create;
  FLog.LoadFromFile( AFileName );
end;

procedure TSimpleLog.SetFileName(const Value: string);
begin
  if ( FFileName <> Value ) and ( Value <> '' ) then
    FFileName := Value;
end;

initialization
  _Log := TSimpleLog.Create;
finalization
  _Log.Free;

end.
