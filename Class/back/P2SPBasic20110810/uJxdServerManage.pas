{
单元名称: uJxdServerManage
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com  jxd524@gmail.com
说    明: 服务器信息管理对象
开始时间: 2011-04-20
修改时间: 2011-04-20 (最后修改时间)
类说明  :
  管理服务器信息, online服务器及客户端需要使用
}

unit uJxdServerManage;

interface

uses
  Windows, Classes, SysUtils, uJxdCmdDefine, uJxdDataStream;

type
  PServerManageInfo = ^TServerManageInfo;
  TServerManageInfo = record
    FServerStyle: TServerStyle;
    FServerID: Cardinal; 
    FServerIP: Cardinal;
    FServerPort: Word;
    FTag: Cardinal;
  end;
  TAryServerInfo = array of TServerManageInfo;
  {$M+}
  TxdServerManage = class
  public
    constructor Create;
    destructor  Destroy; override;
    procedure AddServerInfo(Ap: PServerManageInfo; ATag: Cardinal); //添加服务器信息
    function  GetServerGroup(const AServerStyle: TServerStyle; var AServerInfos: TAryServerInfo): Integer; //本地查找服务器信息
    procedure CopyToStream(ACmdStream: TxdMemoryHandle);
    function  LockManage: TList;
    procedure UnlockManage;
  private
    FServerList: TThreadList;
    function CompareServer(const Ap1, Ap2: PServerManageInfo): Boolean; inline;
    procedure LoadFormFile;
    procedure SaveToFile;
  private
    FFileName: string;
    FLastUpdateTime: Cardinal;
    procedure SetFileName(const Value: string);
  published
    property FileName: string read FFileName write SetFileName;
    property LastUpdateTime: Cardinal read FLastUpdateTime write FLastUpdateTime;
  end;
  {$M-}
implementation

const
  CtServerManageVer = 100;
  CtServerManageInfoSize = SizeOf(TServerManageInfo);

{ TxdServerManage }

procedure TxdServerManage.AddServerInfo(Ap: PServerManageInfo; ATag: Cardinal);
var
  lt: TList;
  i: Integer;
  bFind: Boolean;
  p: PServerManageInfo;
begin
  if not Assigned(Ap) then Exit;
  lt := FServerList.LockList;
  try
    bFind := False;
    for i := 0 to lt.Count - 1 do
    begin
      p := lt[i];
      if CompareServer(p, Ap) then
      begin
        p^.FTag := ATag;
        p^.FServerID := Ap^.FServerID;
        bFind := True;
        Break;
      end;
    end;
    if not bFind then
    begin
      New( p );
      p^.FServerStyle := Ap^.FServerStyle;
      p^.FServerID := Ap^.FServerID;
      p^.FServerIP := Ap^.FServerIP;
      p^.FServerPort := Ap^.FServerPort;
      p^.FTag := ATag;
      lt.Add( p );
    end;
  finally
    FServerList.UnlockList;
  end;
end;

function TxdServerManage.CompareServer(const Ap1, Ap2: PServerManageInfo): Boolean;
begin
  Result := (Ap1^.FServerStyle = Ap2^.FServerStyle) and (Ap1^.FServerIP = Ap2^.FServerIP) and (Ap1^.FServerPort = Ap2^.FServerPort);
end;

procedure TxdServerManage.CopyToStream(ACmdStream: TxdMemoryHandle);
var
  lt: TList;
  i: Integer;
  p: PServerManageInfo;
begin
  lt := FServerList.LockList;
  try
    if lt.Count = 0 then Exit;
    ACmdStream.WriteWord( lt.Count );
    for i := 0 to lt.Count - 1 do
    begin
      p := lt[i];
      ACmdStream.WriteByte( Byte(p^.FServerStyle) );
      ACmdStream.WriteCardinal( p^.FServerIP );
      ACmdStream.WriteWord( p^.FServerPort );
    end;
  finally
    FServerList.UnlockList;
  end;
end;

constructor TxdServerManage.Create;
begin
  FServerList := TThreadList.Create;
  FFileName := 'ServerManage.dat';
  LastUpdateTime := 0;
end;

destructor TxdServerManage.Destroy;
begin
  SaveToFile;
  inherited;
end;

function TxdServerManage.GetServerGroup(const AServerStyle: TServerStyle; var AServerInfos: TAryServerInfo): Integer;
var
  lt: TList;
  i, j, nCount: Integer;
  p: PServerManageInfo;
  bAdd: Boolean;
begin
  nCount := Length( AServerInfos );
  Result := 0;
  lt := FServerList.LockList;
  try
    for i := 0 to lt.Count - 1 do
    begin
      p := lt[i];
      if p^.FServerStyle = AServerStyle then
      begin
        bAdd := True;
        for j := 0 to nCount - 1 do
        begin
          if CompareServer(p, @AServerInfos[j]) then
          begin
            bAdd := False;
            Break;
          end;
        end;
        if bAdd then
        begin
          SetLength( AServerInfos, nCount + Result + 1 );
          Move( p^, AServerInfos[nCount + Result], CtServerManageInfoSize );
          AServerInfos[nCount + Result].FTag := 0;
          Inc( Result );
        end;
      end;
    end;
  finally
    FServerList.UnlockList;
  end;
end;

procedure TxdServerManage.LoadFormFile;
var
  f: TxdFileStream;
  lt: TList;
  i, j, nCount: Integer;
  p: PServerManageInfo;
  bFind, bEmpty: Boolean;
begin
  if not FileExists(FFileName) then Exit;
  f := TxdFileStream.Create( FFileName, fmOpenRead );
  lt := FServerList.LockList;
  try
    if f.Size < 6 then Exit;
    if CtServerManageVer <> f.ReadWord then
    begin
      OutputDebugString( 'TxdServerManage.LoadFormFile; 版本号不正确' );
      Exit;
    end;
    nCount := f.ReadInteger;
    bEmpty := lt.Count = 0;
    for i := 0 to nCount - 1 do
    begin
      New( p );
      try
        p^.FServerStyle := TServerStyle( f.ReadByte );
        p^.FServerID := f.ReadCardinal;
        p^.FServerIP := f.ReadCardinal;
        p^.FServerPort := f.ReadWord;
        p^.FTag := 0;
      except
        Dispose( p );
        Break;
      end;
      if bEmpty then
        lt.Add( p )
      else
      begin
        bFind := False;
        for j := 0 to lt.Count - 1 do
        begin
          if CompareServer(lt[j], p) then
          begin
            bFind := True;
            Break;
          end;
        end;
        if bFind then
          Dispose( p )
        else
          lt.Add( p );
      end;
    end;
  finally
    FServerList.UnlockList;
    f.Free;
  end;
end;

function TxdServerManage.LockManage: TList;
begin
  Result := FServerList.LockList;
end;

procedure TxdServerManage.SaveToFile;
var
  f: TxdFileStream;
  lt: TList;
  i: Integer;
  p: PServerManageInfo;
begin
  if FFileName = '' then
    FFileName := 'ServerManage.dat';
  f := TxdFileStream.Create( FFileName, fmCreate );
  lt := FServerList.LockList;
  try
    f.WriteWord( CtServerManageVer );
    f.WriteInteger( lt.Count );
    for i := 0 to lt.Count - 1 do
    begin
      p := lt[i];
      f.WriteByte( Byte(p^.FServerStyle) );
      f.WriteCardinal( p^.FServerID );
      f.WriteCardinal( p^.FServerIP );
      f.WriteWord( p^.FServerPort );
    end;
  finally
    FServerList.UnlockList;
    f.Free;
  end;
end;

procedure TxdServerManage.SetFileName(const Value: string);
var
  strDir: string;
begin
  strDir := ExtractFilePath(Value);
  if not DirectoryExists(strDir) then
    if not ForceDirectories(strDir) then Exit;
  FFileName := Value;
  if FileExists(FFileName) then
    LoadFormFile;  
end;

procedure TxdServerManage.UnlockManage;
begin
  FServerList.UnlockList;
end;

end.
