unit uIniSysOption;

interface

uses
  SysUtils, Classes, IniFiles;

type
  TIniSystemParam = class
  protected
    FRegIniFile: TIniFile;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    procedure LoadAllParam; virtual;
    procedure SaveAllParam; virtual;
  end;

implementation

constructor TIniSystemParam.Create(const AFileName: string);
begin
  FRegIniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + AFileName);
  LoadAllParam;
end;

destructor TIniSystemParam.Destroy;
begin
  SaveAllParam;
  FreeAndNil(FRegIniFile);
  inherited;
end;

procedure TIniSystemParam.LoadAllParam;
begin
end;

procedure TIniSystemParam.SaveAllParam;
begin
end;
end.

