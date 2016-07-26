unit uJxdSystemFolder;

interface
uses
  Windows, SysUtils, ShlObj;//, uDebugInfo;

type
  TxdSysFolder = class
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Reset;
    function ParserFile(const AFileName: string): Boolean;
    function Count: Integer;
    function Title(const AIndex: Integer): string;
    function Item(const AIndex: Integer): string; overload;
    function Item(const ATitle: string): string; overload;
  private
    FCurFolder: IShellFolder2;
    FCurPath: WideString;
  end;

implementation

{ TxdSysFolder }

type
  TAttInfo = record
    Index: Integer;
    Title: string;
    Info: string;
  end;
var
  _FileAtt: array[0..39] of TAttInfo = (
     (Index:  0; Title: '名称'; Info: ''),
     (Index:  1; Title: '大小'; Info: ''),
     (Index:  2; Title: '类型'; Info: ''),
     (Index:  3; Title: '修改日期'; Info: ''),
     (Index:  4; Title: '创建日期'; Info: ''),
     (Index:  5; Title: '访问日期'; Info: ''),
     (Index:  6; Title: '属性'; Info: ''),
     (Index:  7; Title: '状态'; Info: ''),
     (Index:  8; Title: '所有者'; Info: ''),
     (Index:  9; Title: '作者'; Info: ''),
     (Index: 10; Title: '标题'; Info: ''),
     (Index: 11; Title: '主题'; Info: ''),
     (Index: 12; Title: '类别'; Info: ''),
     (Index: 13; Title: '页数'; Info: ''),
     (Index: 14; Title: '备注'; Info: ''),
     (Index: 15; Title: '版权'; Info: ''),
     (Index: 16; Title: '艺术家'; Info: ''),
     (Index: 17; Title: '唱片标题'; Info: ''),
     (Index: 18; Title: '发行年'; Info: ''),
     (Index: 19; Title: '曲目号码'; Info: ''),
     (Index: 20; Title: '流派'; Info: ''),
     (Index: 21; Title: '持续时间'; Info: ''),
     (Index: 22; Title: '位速'; Info: ''),
     (Index: 23; Title: '受保护'; Info: ''),
     (Index: 24; Title: '摄影机型号'; Info: ''),
     (Index: 25; Title: '相片拍照日期'; Info: ''),
     (Index: 26; Title: '尺寸'; Info: ''),
     (Index: 27; Title: '宽度'; Info: ''),
     (Index: 28; Title: '高度'; Info: ''),
     (Index: 29; Title: '集名'; Info: ''),
     (Index: 30; Title: '节目描述'; Info: ''),
     (Index: 31; Title: '未知'; Info: ''),
     (Index: 32; Title: '音频采样大小'; Info: ''),
     (Index: 33; Title: '音频采样级别'; Info: ''),
     (Index: 34; Title: '频道'; Info: ''),
     (Index: 35; Title: '公司'; Info: ''),
     (Index: 36; Title: '描述'; Info: ''),
     (Index: 37; Title: '文件版本'; Info: ''),
     (Index: 38; Title: '产品名称'; Info: ''),
     (Index: 39; Title: '产品版本'; Info: ''));

function TxdSysFolder.Count: Integer;
begin
  Result := Length(_FileAtt);
end;

constructor TxdSysFolder.Create;
begin
  Reset;
//  FFileAttInfo := ( [] );
end;

destructor TxdSysFolder.Destroy;
begin
  FCurFolder := nil;
  inherited;
end;

function TxdSysFolder.Item(const ATitle: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(_FileAtt) do
  begin
    if CompareText(ATitle, _FileAtt[i].Title) = 0 then
    begin
      Result := _FileAtt[i].Info;
      Break;
    end;
  end;
end;

function TxdSysFolder.Item(const AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex <= High(_FileAtt)) then
    Result := _FileAtt[AIndex].Info
  else
    Result := '';
end;

function TxdSysFolder.ParserFile(const AFileName: string): Boolean;
var
  strPath, strName: WideString;
  nEatch, nAttri: Cardinal;
  ppIDL: PItemIDList;
  phf: IShellFolder2;
  i: Integer;
  dts: TShellDetails;
begin
   Result := False;
  if not FileExists(AFileName) then Exit;
  strPath := ExtractFilePath( AFileName );
  strName := ExtractFileName( AFileName );
  if CompareText(strPath, FCurPath) <> 0 then
  begin
    Reset;
    if Failed(FCurFolder.ParseDisplayName( 0, nil, PWideChar(strPath), nEatch, ppIDL, nAttri )) then Exit;
    if Failed(FCurFolder.BindToObject( ppidl, nil, IID_IShellFolder2, phf )) then Exit;
    FCurFolder := nil;
    FCurFolder := phf;
    phf := nil;
    FCurPath := strPath;
  end;

  if Failed(FCurFolder.ParseDisplayName( 0, nil, PWideChar(strName), nEatch, ppIDL, nAttri)) then Exit;

  for i := Low(_FileAtt) to High(_FileAtt) do //这个数值未经考证，微软网站的事例好像随便写了个34,我看不够用，就自作主张了。
  begin
    ZeroMemory( @dts, SizeOf(TShellDetails) );

    if Succeeded(FCurFolder.GetDetailsOf(ppidl, i, dts)) then
    begin

      if dts.str.uType = 0 then
        _FileAtt[i].Info := String(dts.str.pOleStr)
      else if dts.str.uType = 1 then
        _FileAtt[i].Info := 'error'
      else if dts.str.uType = 2 then
      begin
        if dts.str.uOffset = 0 then
          _FileAtt[i].Info := ''
        else
          _FileAtt[i].Info := IntToStr(dts.str.uOffset)
      end
      else if dts.str.uType = 3 then
        _FileAtt[i].Info := string(dts.str.cStr)
      else
        _FileAtt[i].Info := 'error';
    end;
  end;
  
//  for i := 0 to 100 do
//  begin
//    ZeroMemory( @psd, SizeOf(TShellDetails) );
//    ZeroMemory( @psd1, SizeOf(TShellDetails) );
//
//    if Succeeded(FCurFolder.GetDetailsOf(ppidl, i, psd)) then
//    begin
//      if Succeeded(FCurFolder.GetDetailsOf(nil, i, psd1)) then
//      begin
//        case psd1.str.uType of
//          0:
//          begin
//            str := psd1.str.pOleStr;
//            if str = '' then
//              str := 'xxxx';
//          end;
//          1: str := 're';
//          2: str := IntToStr(psd1.str.uOffset);
//          3:
//          begin
//            str := psd1.str.cStr[0];
//          end
//          else
//            str := 'Unknow';
//        end;
//      end
//      else
//        str := 'error';
//
//      str := IntToStr( i ) + ' ' + str;
//
//
//      if psd.str.uType = 0 then
//        str := str + ':' + String(psd.str.pOleStr)
//      else if psd.str.uType = 1 then
//        str := 'error'
//      else if psd.str.uType = 2 then
//        str := str + ':' +IntToStr(psd.str.uOffset)
//      else if psd.str.uType = 3 then
//        str := str + ':' +string(psd.str.cStr)
//      else
//        str := 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx';
//
//      _Log( str );
//    end;
//  end;
  Result := True;
end;

procedure TxdSysFolder.Reset;
var
  phf: IShellFolder;
begin
  FCurFolder := nil;
  SHGetDesktopFolder( phf );
  phf.QueryInterface( IID_IShellFolder2, FCurFolder );
  phf := nil;
  FCurPath := '';
end;

function TxdSysFolder.Title(const AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex <= High(_FileAtt)) then
    Result := _FileAtt[AIndex].Title
  else
    Result := '';
end;

end.
