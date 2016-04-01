{
TxdBuildResManage:
  将指定文件编译成资源文件
  编译指令：BuildResManage  
TxdResManage:
  使用文件名称的HASH值去匹配指定的资源文件。
  编译指令：ResManage  
}
unit uJxdGpResManage;

interface

uses
  Windows, SysUtils, Classes, ExtCtrls, uJxdHashCalc, ActiveX, GDIPAPI, GDIPOBJ;

type
  PComplieResInfo = ^TComplieResInfo;
  TComplieResInfo = record
    FFileName: string;
    FHashString: string;
  end;
  PResInfo = ^TResInfo;
  TResInfo = record
    FBmp: TGPBitmap;
    FGlb: HGLOBAL;
    FRefCount: Integer;
    FHashValue: string;    
  end;
  TxdBuildResManage = class
  public
    constructor Create(const AProjectFileName: string);
    destructor  Destroy; override; 
    
    procedure AddToRes(const AFileName: string);
  private
    FProjectFileName: string;
    FComplieResList: TList;
    FTimerToNotifySave: TTimer;
    FResFileName: string;
    procedure DoTimerToSaveRes(Sender: TObject);
    procedure FreeComplieResInfo;
    procedure SetResFilename(const Value: string);
  public
    property ResFileName: string read FResFileName write SetResFilename;
  end;

  TxdResManage = class
  public
    constructor Create;
    destructor  Destroy; override; 

    function  GetRes(const AFileName: string): TGPBitmap;
    procedure RealseBitmap(const ABmp: TGPBitmap);    
  private
    FResList: TList;
    procedure FreeRes(Ap: PResInfo);
  end;
  

{$IFDEF BuildResManage}
var
  GBuildResManage: TxdBuildResManage = nil;
{$ENDIF}

{$IFDEF ResManage}
var
  GResManage: TxdResManage = nil;
{$ENDIF}

implementation

const
  CtResStyle = 'xdResManage';
  CtResFileName = 'xdGpComponents.RES';

{ TxdResManage }

procedure TxdBuildResManage.AddToRes(const AFileName: string);
var
  i: Integer;
  strFileName, strHash: string;
  p: PComplieResInfo;
  bAdd: Boolean;
begin 
  if not FileExists(AFileName) then Exit;  
  
  strFileName := LowerCase(AFileName);
  strHash := HashToStr( (HashString(strFileName)) );
  bAdd := True;
  for i := 0 to FComplieResList.Count - 1 do
  begin
    p := FComplieResList[i];
    if CompareText(p^.FHashString, strHash) = 0 then
    begin
      bAdd := False;
      Break;
    end;
  end;
  if bAdd then
  begin
    New( p );
    p^.FFileName := strFileName;
    p^.FHashString := strHash;
    FComplieResList.Add( p );
  end;

  if not Assigned(FTimerToNotifySave) then
  begin
    FTimerToNotifySave := TTimer.Create( nil );
    FTimerToNotifySave.Interval := 2 * 1000;
    FTimerToNotifySave.OnTimer := DoTimerToSaveRes;
    FTimerToNotifySave.Enabled := True;
  end;
end;

constructor TxdBuildResManage.Create(const AProjectFileName: string);
begin
  FProjectFileName := AProjectFileName;
  FResFileName := ExtractFilePath(AProjectFileName) + CtResFileName;
  FComplieResList := TList.Create;
  FTimerToNotifySave := nil;
end;

destructor TxdBuildResManage.Destroy;
begin
  FreeComplieResInfo;
  inherited;
end;

procedure TxdBuildResManage.DoTimerToSaveRes(Sender: TObject);
var
  nResult: Integer;
  i, nCount: Integer;
  p: PComplieResInfo;
  strText: TStringList;
  strName: string;
  bFind: Boolean;
begin
  FTimerToNotifySave.Enabled := False;
  if FProjectFileName = '' then
    raise Exception.Create('需要设置资源文件名称：在创建TxdBuildResManage对象时设置 ProjectFileName' );

  nResult := MessageBox(0, '是否保存软件所需要的资源文件？' + #13#10 + '(将它编译成RES，添加到工程目录下, 并会修改工程文件)',
                           '资源编译提示', MB_YESNO );
  if nResult = IDYES then
  begin
    nCount := 0;
    strText := TStringList.Create;
    try
      for i := 0 to FComplieResList.Count - 1 do
      begin
        p := FComplieResList[i];
        if FileExists(p^.FFileName) then
        begin
          strText.Add( p^.FHashString + ' ' + CtResStyle + ' "' + p^.FFileName + '"' );
          Inc( nCount );
        end;
      end;
      if nCount > 0 then
      begin
        while FileExists(ResFileName) do
        begin
          DeleteFile( ResFileName );
          Sleep( 100 );
        end;

        Sleep( 500 );
        strName := StringReplace( ResFileName, '.RES', '.rc', [rfReplaceAll, rfIgnoreCase] );
        strText.SaveToFile( strName );
        Sleep( 500 );
        WinExec( PChar('brcc32.exe "' + strName + '"'), SW_SHOWNORMAL );
        Sleep( 500 );
        while FileExists(strName) do
          DeleteFile( strName );
      end;
    finally
      strText.Free;
    end;
    if FileExists(ResFileName) then
    begin
      strText := TStringList.Create;
      strText.LoadFromFile( FProjectFileName );

      if CompareText(ExtractFilePath(FResFileName), ExtractFilePath(FProjectFileName)) = 0 then
        strName := ExtractFileName( FResFileName )
      else
        strName := FResFileName;

      bFind := False;
      for i := 0 to strText.Count - 1 do
      begin
        if Pos(strName, strText[i]) > 0 then
        begin
          bFind := True;
          strText[i] := '{$R ' + strName + '}';
          Break;
        end;
      end;
      if not bFind then
      begin
        for i := 0 to strText.Count - 1 do
        begin
          if CompareText('begin', strText[i]) = 0 then
          begin
            bFind := True;
            strText.Insert(i - 1, '{$R ' + strName + '}' );
            Break;
          end;
        end;
        if not bFind and (strText.Count > 2) then
        begin
          strText.Insert(i - 1, '{$R ' + strName + '}' );
          bFind := True;
        end;
      end;
        
      if bFind then
        strText.SaveToFile( FProjectFileName );
      strText.Free;

      if bFind then         
        MessageBox( 0, '编译成功, 并已修改了工程文件' + #13#10 +
                            '  关闭软件之后再去掉编译条件：BuildResManage之后再重新编译一次', '提示', MB_OK )
      else
        MessageBox( 0, '编译成功, 但无法修改工程文件，请确定设置是正确的', '提示', MB_OK )      
    end;
    
    FreeAndNil( FTimerToNotifySave );
  end
  else
    FTimerToNotifySave.Enabled := True;  
end;

procedure TxdBuildResManage.FreeComplieResInfo;
var
  i: Integer;
begin
  FreeAndNil( FTimerToNotifySave );
  for i := 0 to FComplieResList.Count - 1 do
    Dispose( FComplieResList[i] );
  FComplieResList.Clear;
  FreeAndNil( FComplieResList );
end;

procedure TxdBuildResManage.SetResFilename(const Value: string);
begin
  if Value <> '' then  
    FResFileName := Value;
end;

{ TxdResManage }

constructor TxdResManage.Create;
begin
  FResList := TList.Create;
end;

destructor TxdResManage.Destroy;
var
  i: Integer;
begin
  for i := 0 to FResList.Count - 1 do
    FreeRes( FResList[i] );  
  FResList.Free;
  inherited;
end;

procedure TxdResManage.FreeRes(Ap: PResInfo);
begin
  if Assigned(Ap^.FBmp) then
      Ap^.FBmp.Free;
  GlobalFree( Ap^.FGlb );
  Dispose( Ap );
end;

function TxdResManage.GetRes(const AFileName: string): TGPBitmap;
var
  i: Integer;
  p: PResInfo;
  strHash: string;
  res: TResourceStream;
  pBuf: PByte;
  stream: IStream;
begin
  Result := nil;
  if AFileName = '' then Exit;  
  strHash := HashToStr( (HashString(LowerCase(AFileName))) );
  for i := 0 to FResList.Count - 1 do
  begin
    p := FResList[i];
    if CompareText(strHash, p^.FHashValue) = 0 then
    begin
      Result := p^.FBmp;
      Inc( p^.FRefCount );
      Break;
    end;
  end;
  if not Assigned(Result) then
  begin
    try
      res := TResourceStream.Create( HInstance, strHash, CtResStyle );
    except
      Exit;
    end;

    
    New( p );
    p^.FGlb := GlobalAlloc( GHND, res.Size );
    pBuf := GlobalLock( p^.FGlb );
    res.Read( pBuf^, res.Size );
    GlobalUnlock( p^.FGlb );
    Res.Free;    
    CreateStreamOnHGlobal(p^.FGlb, False, stream );
    p^.FBmp := TGPBitmap.Create( stream );
    p^.FRefCount := 1;
    p^.FHashValue := strHash;

    FResList.Add( p );
    Result := p^.FBmp;
  end;
end;

procedure TxdResManage.RealseBitmap(const ABmp: TGPBitmap);
var
  i: Integer;
  p: PResInfo;
begin
  for i := 0 to FResList.Count - 1 do
  begin
    p := FResList[i];
    if p^.FBmp = ABmp then
    begin
      Dec( p^.FRefCount );
      if p^.FRefCount <= 0 then
      begin
        FreeRes( p );
        FResList.Delete( i );
      end;
      Break;
    end;
  end;
end;

initialization
  {$IFDEF ResManage}
  GResManage := TxdResManage.Create;
  {$ENDIF}
finalization
  {$IFDEF ResManage}
  FreeAndNil( GResManage );
  {$ENDIF}

end.
