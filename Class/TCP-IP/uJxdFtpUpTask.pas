{
单元名称: uJxdFtpUpTask
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: 使用IdFtp对文件进行单线程上传。
开始时间: 2010-08-05
修改时间: 2010-08-06 (最后修改)

上传文件结构：

上传文件名
记录在本地的已上传长度
Host
post
Ftp用户名
Ftp密码
服务器目录
}

unit uJxdFtpUpTask;

interface
uses
  Windows, Messages, SysUtils, Classes, StrUtils, IdFTP, Forms, uJxdThread, IdComponent, Encrypt;

type
  TFtpUpTaskState = (tsNULL, tsInit, tsConnecting, tsTransportData, tsCompletion, tsStop, tsError,
                     tsFileExist, tsCheckHash);
  TOnEncryptBuffer = procedure(Sender: TObject; var Buffer; Count: Integer) of object;
  TOnReUploadFile = procedure(Sender: TObject; var AIsReUpload: Boolean) of object;
  {$M+}
  TxdFtpUpTask = class
  public
    constructor Create;
    destructor  Destroy; override;

    function GetLastErrorText: string;

  protected
    //FTP运行线程
    procedure DoThread_FtpRunning;

    procedure ActiveFtpUpTask;
    procedure UnActiveFtpUpTask;
    function  IsRuning: Boolean;

    procedure CreateFtpObject;
    function  CheckFtpParam: Boolean;
    procedure ChangedUpTaskState(const AState: TFtpUpTaskState);
    procedure DoErrorInfo(const AInfo: string);
    function  ConnectToServer: Boolean;
    procedure DisConnectServer;

    procedure DoBeginTransportFtpData(Sender: TObject; AWorkMode: TWorkMode; const AWorkCountMax: Int64);
    procedure DoEndTransportFtpData(Sender: TObject; AWorkMode: TWorkMode);
    procedure DoTransportFtpData(Sender: TObject; AWorkMode: TWorkMode; const AWorkCount: Int64);
    function  DoReUploadFile: Boolean;
  private
    FThreadRuning: Boolean;
    FLastErrorText: string;
    FServerFileName: string;
    FLocalFileSize: Int64;
    FCurUploadSize: Int64;
    FSaveUploadInfoFileName: string;
    procedure SaveUploadFileInfo;
    procedure LoadUploadFileInfo;
  private
    FFtpObject: TIdFTP;
    FActive: Boolean;
    FUpDirPath: string;
    FCurState: TFtpUpTaskState;
    FUpFileName: string;
    FOnEncryptBuffer: TOnEncryptBuffer;
    FCurServerFileSize: Int64;
    FOnUploadFinished: TNotifyEvent;
    FOnReUploadFile: TOnReUploadFile;
    FOnStateChanged: TNotifyEvent;
    FTaskID: Integer;
    FFreeOnFinished: Boolean;
    FActiveTransDataTime: Cardinal;
    FCurUpFileSize: Int64;
    FOnThreadFreeObject: TNotifyEvent;
    FTag: Integer;
    FTaskOwnerName: string;
    procedure SetActive(const Value: Boolean);
    function  GetUserName: string;
    procedure SetUserName(const Value: string);
    function  GetUserPassWord: string;
    procedure SetUserPassWord(const Value: string);
    procedure SetUpDirPath(const Value: string);
    function  GetHost: string;
    procedure SetHost(const Value: string);
    function  GetPort: Integer;
    procedure SetPort(const Value: Integer);
    procedure SetUpFileName(const Value: string);
    function  GetAverageSpeed: string;
  published
    property Active: Boolean read FActive write SetActive;

    property ActiveTransDataTime: Cardinal read FActiveTransDataTime;
    property CurState: TFtpUpTaskState read FCurState;
    property CurServerFileSize: Int64 read FCurServerFileSize;
    property CurUpFileSize: Int64 read FCurUpFileSize;
    property FileSize: Int64 read FLocalFileSize;
    property AverageSpeed: string read GetAverageSpeed;
    property TaskID: Integer read FTaskID write FTaskID;
    property Tag: Integer read FTag write FTag;
    property TaskOwnerName: string read FTaskOwnerName write FTaskOwnerName;

    property UpFileName: string read FUpFileName write SetUpFileName;
    property UpDirPath: string read FUpDirPath write SetUpDirPath;
    property UserName: string read GetUserName write SetUserName;
    property UserPassWord: string read GetUserPassWord write SetUserPassWord;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
    property FreeOnFinished: Boolean read FFreeOnFinished write FFreeOnFinished;

    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged; 
    property OnReUploadFile: TOnReUploadFile read FOnReUploadFile write FOnReUploadFile; 
    property OnUploadFinished: TNotifyEvent read FOnUploadFinished write FOnUploadFinished;
    property OnEncryptBuffer: TOnEncryptBuffer read FOnEncryptBuffer write FOnEncryptBuffer;
    property OnThreadFreeObject: TNotifyEvent read FOnThreadFreeObject write FOnThreadFreeObject;
  end;
  {$M-}
implementation

type
  TEncryptStream = class(TFileStream)
  public
    Owner: TxdFtpUpTask;
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

const
  CtFtpExe = '.xd';
  CtPassword = '%$xd_terry&(';

function GetFileSizeEx(const AFileName: string): Int64;
var
  SearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(AFileName), faAnyFile, SearchRec) = 0 then
  begin
    Result := SearchRec.Size;
    FindClose(SearchRec);
  end
  else
    Result := 0;
end;

{ TEncryptStream }
function TEncryptStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := inherited Read(Buffer, Count);
  if Assigned(Owner) and Assigned(Owner.OnEncryptBuffer) then
    Owner.OnEncryptBuffer( Owner, Buffer, Count );
end;

{ TxdFtpUpTask }

procedure TxdFtpUpTask.ActiveFtpUpTask;
begin
  if (FCurState <> tsNULL) or (not CheckFtpParam) then Exit;
  FActive := True;
  FCurUpFileSize := 0;
  FActiveTransDataTime := 0;
  RunningByThread( DoThread_FtpRunning );
end;

procedure TxdFtpUpTask.ChangedUpTaskState(const AState: TFtpUpTaskState);
begin
  FCurState := AState;
  case FCurState of
    tsNULL: ;
    tsInit: ;
    tsConnecting: ;
    tsTransportData: ;
    tsCompletion: ;
    tsStop:  
    begin
      try
        DisConnectServer;
        ChangedUpTaskState( tsNULL );
      except
      end;
    end;
    tsError: ;
    tsFileExist: ;
    tsCheckHash: ;
  end;
  if Assigned(OnStateChanged) then
    OnStateChanged( Self );
end;

function TxdFtpUpTask.CheckFtpParam: Boolean;
begin
  if not Assigned(FFtpObject) then
  begin
    Result := False;
    DoErrorInfo( 'must recreate object' );
    Exit;
  end;
  
  if not FileExists(FUpFileName) then
  begin
    Result := False;
    DoErrorInfo( 'not find upload file' );
    Exit;
  end;
  if FLocalFileSize = 0 then
    FLocalFileSize := GetFileSizeEx( FUpFileName );
  if FFtpObject.Host = '' then
  begin
    Result := False;
    DoErrorInfo( 'must set ftp host first' );
    Exit;
  end;
  if FFtpObject.Port = 0 then
  begin
    Result := False;
    DoErrorInfo( 'must set ftp port first' );
    Exit;
  end;
  Result := True;
end;

function TxdFtpUpTask.ConnectToServer: Boolean;
begin
  Result := False;
  if FFtpObject.Connected then
  begin
    Result := True;
    Exit;
  end;
  ChangedUpTaskState( tsConnecting );
  try
    Result := False;
    FFtpObject.Connect( True, 10 * 1000 );
    Result := True;
  except
  end;
  if Result and (FCurState <> tsStop) and (FUpDirPath <> '') then
  begin
    try
      FFtpObject.ChangeDir( FUpDirPath );
    except
      Result := False;
    end;
    if Result then
    begin
      try
        FFtpObject.MakeDir( FUpDirPath );
        FFtpObject.ChangeDir( FUpDirPath );
      except
        Result := False;
      end;
    end;
  end;
end;

constructor TxdFtpUpTask.Create;
begin
  FFtpObject := nil;
  FActive := False;
  FCurState := tsNULL;
  FUpDirPath := '';
  FCurServerFileSize := 0;
  FSaveUploadInfoFileName := '';
  FFreeOnFinished := True;
  FActiveTransDataTime := 0;
  FCurUpFileSize := 0;
  FThreadRuning := False;

  CreateFtpObject;
end;

procedure TxdFtpUpTask.CreateFtpObject;
begin
  FFtpObject := TIdFTP.Create( nil );
  with FFtpObject do
  begin
    Passive := True; //使用被动模式
  end;
end;

destructor TxdFtpUpTask.Destroy;
begin
  SaveUploadFileInfo;
  Active := False;
  inherited;
end;

procedure TxdFtpUpTask.DisConnectServer;
begin
  if Assigned(FFtpObject) and FFtpObject.Connected then
    FFtpObject.Disconnect;
end;

procedure TxdFtpUpTask.DoBeginTransportFtpData(Sender: TObject; AWorkMode: TWorkMode; const AWorkCountMax: Int64);
begin
  
end;

procedure TxdFtpUpTask.DoEndTransportFtpData(Sender: TObject; AWorkMode: TWorkMode);
begin

end;

procedure TxdFtpUpTask.DoErrorInfo(const AInfo: string);
begin
  FLastErrorText := AInfo;  
  OutputDebugString( PChar(AInfo) );
end;

function TxdFtpUpTask.DoReUploadFile: Boolean;
var
  bReUpload: Boolean;
begin
  bReUpload := True;
  if Assigned(OnReUploadFile) then
    OnReUploadFile( Self, bReUpload );
  Result := bReUpload;
end;

procedure TxdFtpUpTask.DoThread_FtpRunning;
var
  stream: TEncryptStream;
  bAppend, bFinished: Boolean;
begin
  //新建线程中运行
  bFinished := False;
  FThreadRuning := True;
  try
    ChangedUpTaskState( tsInit );

    {不对事件进行处理}
    with FFtpObject do
    begin
      OnWorkBegin := nil;
      OnWork := nil;
      OnWorkEnd := nil;
    end;

    {连接服务器，并改变目录，必要时创建目录}
    if not ConnectToServer then
    begin
      DoErrorInfo( 'can not connect to server!' );
      ChangedUpTaskState( tsError );
      Exit;
    end;

    {确定文件上传进度}
    FCurServerFileSize := FFtpObject.Size( FServerFileName );
    if FCurServerFileSize < 0 then
    begin      
      FCurServerFileSize := FFtpObject.Size( ChangeFileExt(FServerFileName, '') );
      //如果此时有大小，则表示此文件已经上传
      if FCurServerFileSize > 0 then
        FCurServerFileSize := FLocalFileSize;
    end;

    if FCurServerFileSize <= 0 then
      FCurServerFileSize := 0
    else if FCurServerFileSize = FLocalFileSize then
    begin
      DoErrorInfo( 'ftp server has already exist the file: ' + FUpFileName );
      ChangedUpTaskState( tsFileExist );
      if not DoReUploadFile then
      begin
        DisConnectServer;
        if FileExists(FSaveUploadInfoFileName) then
          DeleteFile( FSaveUploadInfoFileName );
        if Assigned(OnUploadFinished) then
          OnUploadFinished( Self );
        bFinished := True;
        Exit;
      end
      else
        FCurServerFileSize := 0;
    end;
    FCurUploadSize := 0;
    bAppend := FCurServerFileSize <> 0;

    {接收传输数据时事件触发}
    with FFtpObject do
    begin
      OnWorkBegin := DoBeginTransportFtpData;
      OnWork := DoTransportFtpData;
      OnWorkEnd := DoEndTransportFtpData;
    end;

    {开始优传输数据}
    ChangedUpTaskState( tsTransportData );
    stream := TEncryptStream.Create( UpFileName, fmOpenRead );
    try
      stream.Owner := Self;
      stream.Position := FCurServerFileSize;
      try
        FActiveTransDataTime := GetTickCount;
        FCurUpFileSize := 0;
        FFtpObject.Put( stream, FServerFileName, bAppend );
      except
      end;
    finally
      stream.Free;
    end;

    {数据传输完毕}
    if FLocalFileSize = FFtpObject.Size(FServerFileName) then
    begin
      //上传完成
      OutputDebugString( 'Upload finished' );
      ChangedUpTaskState( tsCompletion );
      DisConnectServer;
      if FileExists(FSaveUploadInfoFileName) then
        DeleteFile( FSaveUploadInfoFileName );
      if Assigned(OnUploadFinished) then
        OnUploadFinished( Self );
      bFinished := True;
    end;
  finally
    FThreadRuning := False;
    FActiveTransDataTime := 0;
    FActive := False;
    if bFinished then
      if FileExists(FSaveUploadInfoFileName) then DeleteFile( FSaveUploadInfoFileName )      
    else
      SaveUploadFileInfo;
    if bFinished and FFreeOnFinished then
    begin
      if Assigned(OnThreadFreeObject) then
        OnThreadFreeObject( Self );
      Free;
    end;
    OutputDebugString( 'thread end' );
  end;
end;

procedure TxdFtpUpTask.DoTransportFtpData(Sender: TObject; AWorkMode: TWorkMode; const AWorkCount: Int64);
begin
  if AWorkMode = wmWrite then
  begin
    FCurServerFileSize := AWorkCount;
    OutputDebugString( PChar('upload file size: ' + IntToStr(FCurServerFileSize)) );
  end;
end;

function TxdFtpUpTask.GetAverageSpeed: string;
var
  dwTime, n: Double;
  bSize: Int64;
const
  CtMB = 1024 * 1024;
  CtK  = 1024;
begin
  if (not Active) or (FActiveTransDataTime = 0) then
  begin
    Result := '';
    Exit;
  end;
  dwTime := (GetTickCount - FActiveTransDataTime) / 1000;
  bSize := FCurUpFileSize;
  try
    n := bSize div CtMB / dwTime;
    if n >= 1.0 then
      Result := Format( '%0.2f MB/S', [n] )
    else
    begin
      n := bSize div CtK / dwTime;
      if n >= 1.0 then
        Result := Format( '%0.2f KB/S', [n] )
      else
        Result := Format( '%0.2f B/S', [bSize / dwTime] );
    end;
  except
    Result := '';
  end;
end;

function TxdFtpUpTask.GetHost: string;
begin
  if Assigned(FFtpObject) then
    Result := FFtpObject.Host
  else
    Result := '';
end;

function TxdFtpUpTask.GetLastErrorText: string;
begin
  Result := FLastErrorText;
end;

function TxdFtpUpTask.GetPort: Integer;
begin
  if Assigned(FFtpObject) then
    Result := FFtpObject.Port
  else
    Result := -1;
end;

function TxdFtpUpTask.GetUserName: string;
begin
  if Assigned(FFtpObject) then
    Result := FFtpObject.Username
  else
    Result := '';
end;

function TxdFtpUpTask.GetUserPassWord: string;
begin
  if Assigned(FFtpObject) then
    Result := FFtpObject.Password
  else
    Result := '';
end;

function TxdFtpUpTask.IsRuning: Boolean;
begin
  Result := Assigned(FFtpObject) and Active;
end;

procedure TxdFtpUpTask.LoadUploadFileInfo;
var
  fs: TFileStream;
  strTemp: string;
  procedure ReadString(var AString: string);
  var
    nLen: Integer;
    pTemp: array[0..1024 * 2] of Char;
  begin
    nLen := Length( AString );
    with fs do
    begin
      ReadBuffer( nLen, SizeOf(nLen) );
      if nLen > 0 then
      begin
        ReadBuffer( pTemp[0], nLen );
        SetLength( AString, nLen );
        pTemp[nLen] := #0;
        AString := pTemp;
      end
      else
        AString := '';
    end;
  end;
var
  nPort: Integer;
begin
  if (FCurServerFileSize < FLocalFileSize) and FileExists(FSaveUploadInfoFileName) then
  begin
    fs := TFileStream.Create( FSaveUploadInfoFileName, fmOpenRead );
    with fs do
    begin
      try
        if Size = 0 then Exit;
        Position := 0;
        ReadString( FUpFileName );
        ReadBuffer( FCurServerFileSize, SizeOf(FCurServerFileSize) );

        ReadString( strTemp );
        Host := strTemp;
        ReadBuffer( nPort, 4 );
        Port := nPort;

        ReadString( strTemp );
        if strTemp <> '' then
          UserName := DecryptStr( strTemp, CtPassword )
        else
          UserName := '';

        ReadString( strTemp );
        if strTemp <> '' then
          UserPassWord := DecryptStr( strTemp, CtPassword )
        else
          UserPassWord := '';

        ReadString( strTemp );
        UpDirPath := strTemp;
      finally
        Free;
      end;
    end;
  end;
end;

procedure TxdFtpUpTask.SaveUploadFileInfo;
var
  fs: TFileStream;
  procedure WriteString(const AString: string);
  var
    nLen: Integer;
  begin
    nLen := Length( AString );
    with fs do
    begin
      WriteBuffer( nLen, SizeOf(nLen) );
      if nLen > 0 then
        WriteBuffer( AString[1], nLen );
    end;
  end;
var
  nPort: Integer;
begin
  if FCurServerFileSize < FLocalFileSize then
  begin
    if FileExists(FSaveUploadInfoFileName) then
      fs := TFileStream.Create(FSaveUploadInfoFileName, fmOpenReadWrite)
    else
      fs := TFileStream.Create(FSaveUploadInfoFileName, fmCreate);
    with fs do
    begin
      try
        Size := 0;
        nPort := Port;
        WriteString( FUpFileName );
        WriteBuffer( FCurServerFileSize, SizeOf(FCurServerFileSize) );
        WriteString( Host );
        WriteBuffer( nPort, 4 );
        WriteString( EncryptStr(Username, CtPassword) );
        WriteString( EncryptStr(UserPassWord, CtPassword) );
        WriteString( UpDirPath );
      finally
        Free;
      end;
    end;
  end;
end;

procedure TxdFtpUpTask.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      ActiveFtpUpTask
    else
      UnActiveFtpUpTask;
  end;
end;

procedure TxdFtpUpTask.SetHost(const Value: string);
begin
  if not IsRuning then
    FFtpObject.Host := Value;
end;

procedure TxdFtpUpTask.SetPort(const Value: Integer);
begin
  if not IsRuning then
    FFtpObject.Port := Value;
end;

procedure TxdFtpUpTask.SetUpDirPath(const Value: string);
begin
  if not IsRuning then
    FUpDirPath := Value;
end;

procedure TxdFtpUpTask.SetUpFileName(const Value: string);
begin
  if (not IsRuning) and FileExists(Value) then
  begin
    FUpFileName := Value;
    FLocalFileSize := GetFileSizeEx( FUpFileName );
  end;
  if FLocalFileSize = 0 then
  begin
    FUpFileName := '';
    FServerFileName := '';
    FSaveUploadInfoFileName := '';
  end
  else
  begin
    FServerFileName := ExtractFileName( FUpFileName );
    FSaveUploadInfoFileName := FUpFileName + CtFtpExe;
    LoadUploadFileInfo;
  end;
end;

procedure TxdFtpUpTask.SetUserName(const Value: string);
begin
  if not IsRuning then
    FFtpObject.Username := Value;
end;

procedure TxdFtpUpTask.SetUserPassWord(const Value: string);
begin
  if not IsRuning then
    FFtpObject.Password := Value;
end;

procedure TxdFtpUpTask.UnActiveFtpUpTask;
var
  nMaxWait: Integer;
begin
  if IsRuning then
    FFtpObject.Abort;
  nMaxWait := 100;
  while Active and (nMaxWait > 0) do
  begin
    Application.ProcessMessages;
    Sleep( 10 );
    Dec( nMaxWait );
  end;
  ChangedUpTaskState( tsStop );
  SaveUploadFileInfo;
end;

end.
