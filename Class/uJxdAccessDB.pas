{
单元名称: uJxdAccessDB
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: Access数据库基本单元
开始时间: 2011-10-12
修改时间: 2011-10-12(最后修改)

子类提供 TADOConnection 信息
实现 InitAllTable 接口，使用 CreateTable 去创建表，基本自动检查要创建表与已经存在表字段信息
      需要时自动添加字段
      
}
unit uJxdAccessDB;

interface
uses
  Windows, SysUtils, Variants, Classes, ComObj, DB, ADODB, uSysSub, uStringHandle;

const
  CADOConnectMDBString = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%s;' +
    'Jet OLEDB:Database Password=%s';
  CADOCompactMDBString = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%s;' +
    'Jet OLEDB:Database Password=%s; Jet OLEDB: Engine type = 5';

type
  TAccessBaseDB = class
  public
    constructor Create(ADOConntion: TADOConnection; const ADataBaseName: string; const APassword: string = '');
    destructor Destroy; override;
  public
    //创建数据库
    class function CreateAccessDB(const ADBFullName, APassword: string; AForceWrite: Boolean = False): Boolean;
    //删除数据表
    class function DropAccessTable(const AccessFullName, ATableName, APassword: string): Boolean;
    //压缩数据库
    class function CompactDatabase(const AccessFullName, APassword: string): Boolean;
    //创建索引
    class function CreateAccessIndex(ADOConntion: TADOConnection; const ATableName, AIndex, AFields, APassword: string;
      IsUnique, IsPrimary: Boolean): Boolean;
    //释放对象
    class procedure FreeDataSet(ADataSet: TDataSet);
  protected
    FADOConnection: TADOConnection;

    procedure ExecSQL(ASQL: string; AQuery: TADOQuery = nil);
    function  OpenSQL(ASQL: string; AQuery: TADOQuery = nil): TADOQuery;

    function TableExists(const AccessFullName, ATableName, APassword: string): Boolean; overload;
    function TableExists(const ATableName: string): Boolean; overload;
    function CreateTable(const ATableName, ACreateTableSQL: string): Boolean;

    procedure InitAllTable; virtual; abstract;
  private
    FDataBaseName: string;
    FDBPassWord: string;
    FTableList: TStrings;
    function  InitDataBase: Boolean;
    procedure CheckTable(const ATableName, ACreateSQL: string);
  end;

implementation

const
  CtCompactDBMinSize = 1024 * 1024 * 3; //当数据库文件大于CtCompactDBMinSize以后，才会在每次启动时进行压缩

procedure TAccessBaseDB.CheckTable(const ATableName, ACreateSQL: string);
var
  query: TADOQuery;
  lt: TStringList;
  strText, strTemp, strFiledName, strAlterSQL: string;
  i: Integer;
begin
  lt := TStringList.Create;

  strText := ACreateSQL;
  strText := GetRangString( strText, '(', ')', dwNotDelete, swFromLeft, 0, swFromRight, 0, True );

  while True do
  begin
    strTemp := GetRangString( strText, ',' );
    if (Pos('(', strTemp) > 0) and (Pos(')', strTemp) < 0) then
      strTemp := strTemp + ',' + GetRangString( strText, ',' );
    if strTemp <> '' then
    begin
      if SearchSignPositon(strTemp, 'PRIMARY') < 0 then      
        lt.Add( strTemp )
    end
    else
      Break;
  end;
  if (strText <> '') and (SearchSignPositon(strTemp, 'PRIMARY') < 0) then
    lt.Add( strText );

  strAlterSQL := '';  
  query := OpenSQL( 'Select Top 1 * from ' + ATableName );  
  try
    for i := 0 to lt.Count - 1 do
    begin
      strText := Trim( lt[i] );
      strFiledName := GetRangString( strText, ' ', True, swFromLeft, dwNotDelete );
      if strFiledName = '' then Continue;

      if not Assigned(query.Fields.FindField(strFiledName)) then
      begin
        if strAlterSQL <> '' then
          strAlterSQL := strAlterSQL + ',';
        strAlterSQL := strAlterSQL + strText;
      end;
    end;
    if strAlterSQL <> '' then
    begin
      strAlterSQL := 'Alter Table ' + ATableName + ' ADD ' + strAlterSQL;
      ExecSQL( strAlterSQL, query );
    end;
  finally
    query.Free;
    lt.Free;
  end;
end;

class function TAccessBaseDB.CompactDatabase(const AccessFullName: string; const APassword: string): Boolean;
 //压缩与修复数据库,覆盖源文件
var
  STempFileName: string;
  vJE: OleVariant;
  strS, strT: string;
begin
  Result := True;
  if GetFileSizeEx(AccessFullName) < CtCompactDBMinSize then Exit;
  
  STempFileName := GetRandomTempFileName;
  try
    vJE := CreateOleObject('Jro.JetEngine');
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

function TAccessBaseDB.CreateTable(const ATableName, ACreateTableSQL: string): Boolean;
var
  AQuery: TADOQuery;
begin
  Result := False;
  if not Assigned(FADOConnection) or not FADOConnection.Connected then Exit;
  
  //如果已经存在指定的表
  if TableExists(ATableName) then
  begin    
    CheckTable( ATableName, ACreateTableSQL );
    Result := True;
    Exit;
  end;
  //创建新表
  AQuery := TADOQuery.Create(nil);
  try
    try
      with AQuery do
      begin
        Connection := FADOConnection;
        SQL.Text := ACreateTableSQL;
        ExecSQL;
      end;
      Result := true;
    except
      Result := False;
    end;
  finally
    AQuery.Free;
  end;
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
      FADOConnection.ConnectionString := Format(CADOConnectMDBString, [FDataBaseName, FDBPassWord]);
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
    if AQuery = nil then
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
    AccessCont.ConnectionString := Format(CADOConnectMDBString, [AccessFullName, APassword]);
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

