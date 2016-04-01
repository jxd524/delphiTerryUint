unit uAccessDB;

interface
uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, StdCtrls, ComObj,
  DB, ADODB, Forms;

const
  CADOConnectMDBString = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%s;' +
    'Jet OLEDB:Database Password=%s';
  CADOCompactMDBString = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%s;' +
    'Jet OLEDB:Database Password=%s; Jet OLEDB: Engine type = 5';

type
  TAccessBaseDB = class
  private
    FDataBaseName: string;
    FDBPassWord: string;
    FTableList: TStrings;
    function InitDataBase: Boolean;
  protected
    FADOConnection: TADOConnection;

    procedure ExecSQL(ASQL: string; AQuery: TADOQuery = nil);
    function OpenSQL(ASQL: string; AQuery: TADOQuery = nil): TADOQuery;

    function TableExists(const AccessFullName, ATableName, APassword: string): Boolean; overload;
    function TableExists(const ATableName: string): Boolean; overload;

    function CreateAccessTable(ADOConntion: TADOConnection; const AccessFullName, ACreateSQL,  ATableName,
      APassword: string): Boolean;
    function CreateDBTable(ATable: TADOTable; ADOConntion: TADOConnection;
      const ATableName, ACreateSQL, APassword: string; AIsOpenTable: Boolean = False): Boolean;

    procedure InitAllTable; virtual;
  public
    constructor Create(ADOConntion: TADOConnection; const ADataBaseName: string; const APassword: string = '');
    destructor Destroy; override;

  public
    class function CreateAccessDB(const ADBFullName, APassword: string; AForceWrite: Boolean = False): Boolean;
    class function DropAccessTable(const AccessFullName, ATableName, APassword: string): Boolean;
    class function CompactDatabase(const AccessFullName, APassword: string): Boolean;
    class function CreateAccessIndex(ADOConntion: TADOConnection; const ATableName, AIndex, AFields, APassword: string;
      IsUnique, IsPrimary: Boolean): Boolean;
    class procedure FreeDataSet(ADataSet: TDataSet);
  end;

implementation

class function TAccessBaseDB.CompactDatabase(const AccessFullName: string; const
  APassword: string): Boolean;
 //压缩与修复数据库,覆盖源文件
  function GetTempPathFileName: string;
 //取得临时文件名
  var
    SPath, SFile: array[0..254] of char;
  begin
    GetTempPath(254, SPath);
    GetTempFileName(SPath, '~SM', 0, SFile);
    Result := SFile;
    DeleteFile(Result);
  end;
var
  STempFileName: string;
  vJE: OleVariant;
  strS, strT: string;
begin
  STempFileName := GetTempPathFileName;
  try
    vJE := CreateOleObject('Jro.JetEngine');
    if FileExists(STempFileName) then
      DeleteFile(STempFileName);
    strS := Format(CADOConnectMDBString, [AccessFullName, APassword]);
    strT := Format(CADOCompactMDBString, [STempFileName, APassword]);
    vJE.CompactDatabase(strS, strT);
    Result := CopyFile(PChar(STempFileName), PChar(AccessFullName), False);
    DeleteFile(STempFileName);
  except
    Result := False;
  end;
end;


constructor TAccessBaseDB.Create(ADOConntion: TADOConnection; const ADataBaseName:
  string; const APassword: string);
begin
  FDataBaseName := ADataBaseName;
  FDBPassWord := APassword;
  FADOConnection := ADOConntion;
  FTableList := TStringList.Create;
  if InitDataBase then
    InitAllTable
  else
    raise Exception.Create('初始化失败,无法打开数据库');
end;

class function TAccessBaseDB.CreateAccessDB(const ADBFullName, APassword: string; AForceWrite: Boolean): Boolean;
var
  AccessDB: OleVariant;
begin
  Result := True;
  if AForceWrite and FileExists(ADBFullName) then
  begin
    if not DeleteFile(ADBFullName) then
    begin
      Result := False;
      Exit;
    end;
  end;
  if not AForceWrite and FileExists(ADBFullName) then
  begin
    Result := True;
    Exit;
  end;
  //新建数据库
  try
    ForceDirectories(ExtractFileDir(ADBFullName));
    AccessDB := CreateOleObject('ADOX.Catalog');
    AccessDB.Create(Format(CADOConnectMDBString, [ADBFullName, APassword]));
  except
    Result := false;
  end;
end;

class function TAccessBaseDB.CreateAccessIndex(ADOConntion: TADOConnection; const
  ATableName, AIndex, AFields, APassword: string; IsUnique, IsPrimary: Boolean): Boolean;
var
  sqltmp: string;
  AQuery: TADOQuery;
begin
  Result := False;
  if (ADOConntion = nil) or (not ADOConntion.Connected) then
    Exit;
  AQuery := TADOQuery.Create(nil);
  try
    try
      AQuery.Connection := ADOConntion;
      Result := true;
      if IsUnique then sqltmp := 'CREATE UNIQUE INDEX '
      else sqltmp := 'CREATE INDEX ';
      sqltmp := sqltmp + AIndex + ' ON [' + ATableName + '](' + AFields + ')';
      if IsPrimary then sqltmp := sqltmp + ' WITH PRIMARY';
      with AQuery do
      begin
        Close;
        SQL.Text := sqltmp;
        ExecSQL;
      end;
    except
      Result := False;
    end;
  finally
    AQuery.Free;
  end;
end;

function TAccessBaseDB.CreateAccessTable(ADOConntion: TADOConnection; const
  AccessFullName, ACreateSQL, ATableName,
  APassword: string): Boolean;
var
  AQuery: TADOQuery;
begin
  Result := False;
  if TableExists(ATableName) then
  begin
    Result := True;
    Exit;
  end;
  if (ADOConntion = nil) or (not ADOConntion.Connected) then
    Exit;
  AQuery := TADOQuery.Create(nil);
  try
    try
      AQuery.Connection := ADOConntion;
      Result := true;
      with AQuery do
      begin
        Close;
        SQL.Text := ACreateSQL;
        ExecSQL;
      end;
    except
      Result := False;
    end;
  finally
    AQuery.Free;
  end;
end;

function TAccessBaseDB.CreateDBTable(ATable: TADOTable; ADOConntion: TADOConnection;
  const ATableName, ACreateSQL, APassword: string; AIsOpenTable: Boolean): Boolean;
var
  bl: Boolean;
begin
  Result := False;
  bl := False;
  if (ADOConntion = nil) or (not ADOConntion.Connected) then
    Exit;
  if ATable = nil then
  begin
    bl := True;
    ATable := TADOTable.Create(nil);
  end;
  if ATable.Active then
  begin
    Result := True;
    Exit;
  end;
  try
    if not CreateAccessTable(ADOConntion, FDataBaseName, ACreateSQL, ATableName, APassword) then
      Exit;
    try //打开这个表
      ATable.Connection := ADOConntion;
      ATable.TableName := ATableName;
      ATable.Open;
      Result := True;
    except
      ATable.Close;
    end;
    if not AIsOpenTable then
      ATable.Close;
  finally
    if bl then
      Atable.Free;
  end;
end;

procedure TAccessBaseDB.InitAllTable;
begin

end;

function TAccessBaseDB.InitDataBase: Boolean;
begin
  Result := False;
  try
    if not CreateAccessDB(FDataBaseName, FDBPassWord) then Exit;
    CompactDatabase(FDataBaseName, FDBPassWord);
    FADOConnection.LoginPrompt := False;
    FADOConnection.ConnectionString := Format(CADOConnectMDBString,
      [FDataBaseName, FDBPassWord]);
    FADOConnection.Connected := True;
    FADOConnection.GetTableNames(FTableList);
    Result := True;
  except
    if not CreateAccessDB(FDataBaseName, FDBPassWord, True) then
    begin
      Result := False;
      Exit;
    end;

    try
      FADOConnection.LoginPrompt := False;
      FADOConnection.ConnectionString := Format(CADOConnectMDBString,
        [FDataBaseName, FDBPassWord]);
      FADOConnection.Connected := True;
      FADOConnection.GetTableNames(FTableList);
      Result := True;
    except
      Result := False;
    end;
    
  end;
end;

function TAccessBaseDB.OpenSQL(ASQL: string; AQuery: TADOQuery): TADOQuery;
var
  Query: TADOQuery;
begin
  if AQuery = nil then
    Query := TADOQuery.Create(nil)
  else
    Query := AQuery;
  Query.Close;
  Query.Connection := FADOConnection;
  Query.SQL.Text := ASQL;
  try
    Query.Open;
  except
    Query.Free;
    raise;
  end;
  Result := Query;
end;



function TAccessBaseDB.TableExists(const ATableName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FTableList.Count - 1 do
  begin
    if CompareText(FTableList[i], ATableName) = 0 then
    begin
      Result := True;
      Break;
    end;
  end;
end;


destructor TAccessBaseDB.Destroy;
begin
  if Assigned(FADOConnection) then
    FADOConnection.Connected := False;
  FTableList.Free;
  inherited;
end;

class function TAccessBaseDB.DropAccessTable(const AccessFullName, ATableName: string;
  const APassword: string): Boolean;
var
  sqltmp: string;
  AccessCont: TADOConnection;
begin
  try
    AccessCont := TADOConnection.Create(nil);
    try
      AccessCont.LoginPrompt := False;
      AccessCont.ConnectionString := Format(CADOConnectMDBString,
        [AccessFullName, APassword]);
      sqltmp := 'DROP TABLE [' + ATableName + ']';
      AccessCont.Open;
      AccessCont.Execute(sqltmp);
      Result := True;
    finally
      AccessCont.Free;
    end;
  except
    Result := False;
  end;
end;

procedure TAccessBaseDB.ExecSQL(ASQL: string; AQuery: TADOQuery);
var
  Query: TADOQuery;

begin
  if AQuery = nil then
    Query := TADOQuery.Create(nil)
  else
    Query := AQuery;
  try
    Query.Close;
    Query.Connection := FADOConnection;
    Query.ParamCheck := False;
    Query.SQL.Text := ASQL;
    Query.ExecSQL;
  finally
    if AQuery = nil then
      Query.Free;
  end;
end;

class procedure TAccessBaseDB.FreeDataSet(ADataSet: TDataSet);
begin
  ADataSet.Close;
  ADataSet.Free;
end;

function TAccessBaseDB.TableExists(const AccessFullName, ATableName: string;
  const APassword: string): Boolean;
var
  TableList: TStringList;
  AccessCont: TADOConnection;
  i: Integer;
begin
  Result := False;
  AccessCont := TADOConnection.Create(nil);
  TableList := TStringList.Create;
  try
    AccessCont.LoginPrompt := False;
    AccessCont.ConnectionString := Format(CADOConnectMDBString, [AccessFullName,
      APassword]);
    AccessCont.Open;
    AccessCont.GetTableNames(TableList);
    for i := 0 to TableList.Count - 1 do
      if CompareText(TableList[i], ATableName) = 0 then
      begin
        Result := True;
        Break;
      end;
    AccessCont.Close;
  finally
    AccessCont.Free;
    TableList.Free;
  end;
end;

end.

