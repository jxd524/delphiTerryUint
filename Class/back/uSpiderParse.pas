unit uSpiderParse;

interface

uses SysUtils, uStringHandle, Classes, uRandomInfo, uSysSub, ComObj, ActiveX;

{$M+}
type

{
格式说明:
fsReplace: [RP]旧值1#27新值#27次数1#8旧值2#27新值2#27次数2
fsDelete:  [Dt]开始位置1#27删除字符1#8开始位置2#27删除字符2
fsJs:      [Js]Js文件路径#27Js函数名#Js语言(Javescript, VBscript)
注:
    次数: -1表示无限
    开始位置1:  -1最后, 其它: 从头
    Js函数格式: function JsHandle(AParam);
        要求返回字符串
}

  TFieldHandleStyle = (fsError, fsNone, fsReplace, fsDelete, fsJs);
  pFieldInfo = ^TFieldInfo;
  TFieldInfo = record
    FFrontdSign: string;
    FBackSign: string;
    FDeleteWay: TDeleteWay;
    FDelEdgeUnVisible: Boolean; //获取字段两边不可见字符是否删除
    FFieldHandleInfo: string;
    FTag: Integer;
  end;

  TOnSpiderParse = procedure(Sender: TObject; AFieldTag: Integer; AFieldContent: string) of object;
  TSpiderParse = class
  private
    FParseFieldInfoList: TList;
    FText: string;
    FMainContentFS, FMainContentBS: string;
    FEnableMainContent: Boolean;
    FTag: Integer;
    FOnSpiderParse: TOnSpiderParse;
    procedure SetText(const Value: string);
  private
    FCaption: string;
    procedure InitVal;
    procedure ClearObj;
    procedure NotifyParse(AFieldTag: Integer; AFieldContent: string);
    class function  GetFieldStyle(AFieldHandleInfo: string): TFieldHandleStyle;
    procedure HandleFieldValue(ATag: Integer; AStrFieldValue, AFieldHandleInfo: string);
    procedure HandleReplace(ATag: Integer; AStrFieldValue, AFieldHandleInfo: string);
    procedure HandleDelete(ATag: Integer; AStrFieldValue, AFieldHandleInfo: string);
    procedure HandleJsWay(ATag: Integer; AStrFieldValue, AFieldHandleInfo: string);
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure MainContentSign(AMainContentFS, AMainContentBS: string);

    function AddParseField(AFieldInfo: TFieldInfo): Boolean;
    function  GetParseField(ATag: Integer): pFieldInfo;
    procedure DeleteParseField(ATag: Integer);

    procedure SpiderRun;
    procedure Clear;

    class procedure MakeReplaceInfo(var AStrResult: string; const AStrOldValue, AStrNewValue: string; AReplaceCount: Integer);
    class procedure MakeDeleteInfo(var AStrResult: string; AIsBegin: Boolean; ADelCount: Integer);
    class procedure MakeJsInfo(var AStrResult: string; AJsFileName, AJsFunction: string; AIsJaveScript: Boolean);
    class procedure ParseHandleInfo(AHandleInfo: string; var AHandleType: TFieldHandleStyle; AInfo: TStringList);
  published
    property Tag: Integer read FTag write FTag;
    property Caption: string read FCaption write FCaption;
    property Text: string read FText write SetText;
    property EnableMainContent: Boolean read FEnableMainContent write FEnableMainContent;

    property OnSpiderParse: TOnSpiderParse read FOnSpiderParse write FOnSpiderParse;
  end;

implementation

{ TSpiderParse }
const
  CtReplace = '[RP]';
  CtDelete = '[DT]';
  CtScript = '[JS]';

  CtSplitSign = #8;
  CtSmallSplitSign = #27;

function HandleFieldByJsFile(AFileName, AFunctionName, AJsLanguage, AParam: string): string;
var
  CodeList: TStringList;
  FScript: Variant;
  strJsHelpCode: string;
begin
  Result := AParam;
  CodeList := TStringList.Create;
  try
    CodeList.LoadFromFile( AFileName );
    strJsHelpCode := CodeList.Text;
  finally
    CodeList.Free;
  end;
  if strJsHelpCode = '' then Exit;

  try
    CoInitialize( nil );
    FScript := CreateOleObject( 'MSScriptControl.ScriptControl' );
    FScript.Language := AJsLanguage;
    FScript.AddCode( strJsHelpCode );
    Result := FScript.Run( AFunctionName, AParam );
  finally
    CoUninitialize;
  end;
end;

function TSpiderParse.AddParseField(AFieldInfo: TFieldInfo): Boolean;
var
  p: pFieldInfo;
begin
  Result := (AFieldInfo.FFrontdSign <> '') and (AFieldInfo.FBackSign <> '') and
            ( GetFieldStyle(AFieldInfo.FFieldHandleInfo) <> fsError);
  if not Result then Exit;
  New(p);
  p^ := AFieldInfo;
  FParseFieldInfoList.Add( p );
end;

procedure TSpiderParse.Clear;
begin
  InitVal;
  ClearObj;
end;

procedure TSpiderParse.ClearObj;
var
  i: Integer;
begin
  for i := FParseFieldInfoList.Count - 1 downto 0 do
  begin
    Dispose( FParseFieldInfoList[i] );
    FParseFieldInfoList.Delete( i );
  end;
end;

constructor TSpiderParse.Create;
begin
  InitVal;
  FParseFieldInfoList := TList.Create;
end;

procedure TSpiderParse.DeleteParseField(ATag: Integer);
var
  i: Integer;
  p: pFieldInfo;
begin
  for i := FParseFieldInfoList.Count - 1 downto 0 do
  begin
    p := FParseFieldInfoList[i];
    if p^.FTag = ATag then
    begin
      Dispose( p );
      FParseFieldInfoList.Delete( i );
    end;
  end;
end;

destructor TSpiderParse.Destroy;
begin
  ClearObj;
  FParseFieldInfoList.Free;
  inherited;
end;

class function TSpiderParse.GetFieldStyle(AFieldHandleInfo: string): TFieldHandleStyle;
var
  strStyle: string;
begin
  if AFieldHandleInfo = '' then
  begin
    Result := fsNone;
    Exit;
  end;
  strStyle := Copy(AFieldHandleInfo, 1, 4);
  if CompareText( strStyle, CtReplace ) = 0 then
    Result := fsReplace
  else if CompareText( strStyle, CtDelete ) = 0 then
    Result := fsDelete
  else if CompareText( strStyle, CtScript ) = 0 then
    Result := fsJs
  else
    Result := fsError;
end;

function TSpiderParse.GetParseField(ATag: Integer): pFieldInfo;
var
  i: Integer;
  p: pFieldInfo;
begin
  Result := nil;
  for i := 0 to FParseFieldInfoList.Count - 1 do
  begin
    p := FParseFieldInfoList[i];
    if p^.FTag = ATag then
    begin
      Result := p;
      Break;
    end;
  end;
end;

procedure TSpiderParse.HandleFieldValue(ATag: Integer; AStrFieldValue, AFieldHandleInfo: string);
var
  HandleStyle: TFieldHandleStyle;
begin
  HandleStyle := GetFieldStyle( AFieldHandleInfo );
  if (HandleStyle = fsError) or (HandleStyle = fsNone) then
  begin
    NotifyParse(  ATag, AStrFieldValue );
    Exit;
  end;
  case HandleStyle of
    fsReplace: HandleReplace(ATag, AStrFieldValue, AFieldHandleInfo);
    fsDelete:  HandleDelete(ATag, AStrFieldValue, AFieldHandleInfo);
    fsJs:      HandleJsWay(ATag, AStrFieldValue, AFieldHandleInfo);
  end;
end;

//fsDelete:  [Dt]开始位置1#27删除字符总数1#8开始位置2#27删除字符总数2
procedure TSpiderParse.HandleDelete(ATag: Integer; AStrFieldValue, AFieldHandleInfo: string);
var
  lt: TStringList;
  i, nBeginPos, nDelCount: Integer;
  strInfo: string;
  Way: TStringDelete;
begin
  AFieldHandleInfo := Copy(AFieldHandleInfo, 5, Length(AFieldHandleInfo) - 4);
  lt := TStringList.Create;
  try
    ParseFormatString( AFieldHandleInfo, #8, lt );
    for i := 0 to lt.Count - 1 do
    begin
      strInfo := lt[i];
      nBeginPos := StrToIntDef( GetRangString( strInfo, #27 ), 0 );
      nDelCount := StrToIntDef( strInfo, 0 );
      if (nBeginPos = 0) and (nDelCount = 0) then
        Continue;
      if nBeginPos = -1 then
        Way := sdRight
      else
        Way := sdLeft;
      AStrFieldValue := DeleteString( AStrFieldValue, nDelCount, Way );
    end;
  finally
    lt.Free;
  end;
  NotifyParse( ATag, AStrFieldValue );
end;

//[Js]Js文件路径#27Js函数名#Js语言(Javescript, VBscript)
//JavaScript VBscript
procedure TSpiderParse.HandleJsWay(ATag: Integer; AStrFieldValue, AFieldHandleInfo: string);
var
  strJsFileName, strJsFunctionName, strJsLang: string;
begin
  AFieldHandleInfo := Copy(AFieldHandleInfo, 5, Length(AFieldHandleInfo) - 4);
  strJsFileName := GetRangString( AFieldHandleInfo, #27 );
  strJsFunctionName := GetRangString( AFieldHandleInfo, #27 );
  strJsLang := AFieldHandleInfo;
  if (not FileExists(strJsFileName)) or (strJsFunctionName = '') or (strJsLang = '') or
     (not IsLetter(strJsFunctionName)) or (not IsLetter(strJsLang)) then
  begin
    NotifyParse( ATag, AStrFieldValue );
    Exit;
  end;
  AStrFieldValue := HandleFieldByJsFile( strJsFunctionName, strJsFunctionName, strJsLang, AStrFieldValue );
  NotifyParse( ATag, AStrFieldValue );
end;

//[RP]旧值1#27新值1#27次数1#8旧值2#27新值2#27次数2
procedure TSpiderParse.HandleReplace(ATag: Integer; AStrFieldValue, AFieldHandleInfo: string);
var
  lt: TStringList;
  i, nReplaceCount: Integer;
  strInfo, strOldValue, strNewValue: string;
begin
  AFieldHandleInfo := Copy(AFieldHandleInfo, 5, Length(AFieldHandleInfo) - 4);
  lt := TStringList.Create;
  try
    ParseFormatString( AFieldHandleInfo, #8, lt );
    for i := 0 to lt.Count - 1 do
    begin
      strInfo := lt[i];
      strOldValue := GetRangString( strInfo, #27 );
      strNewValue := GetRangString( strInfo, #27 );
      nReplaceCount := StrToIntDef( strInfo, -1 );
      StringReplace( AStrFieldValue, strOldValue, strNewValue, swFromLeft, nReplaceCount );
    end;
  finally
    lt.Free;
  end;
  NotifyParse( ATag, AStrFieldValue );
end;

procedure TSpiderParse.InitVal;
begin
  FTag := 0;
  FText := '';
  FMainContentFS := '';
  FMainContentBS := '';
  FEnableMainContent := False;
end;

procedure TSpiderParse.MainContentSign(AMainContentFS, AMainContentBS: string);
begin
  FMainContentFS := AMainContentFS;
  FMainContentBS := AMainContentBS;
end;

class procedure TSpiderParse.MakeDeleteInfo(var AStrResult: string; AIsBegin: Boolean; ADelCount: Integer);
var
  strPos: string;
begin
  if AIsBegin then
    strPos := '1'
  else
    strPos := '-1';

  if AStrResult = '' then
    AStrResult := CtDelete + strPos + CtSmallSplitSign + IntToStr( ADelCount )
  else
    AStrResult := AStrResult + CtSplitSign + strPos + CtSmallSplitSign + IntToStr( ADelCount );
end;

class procedure TSpiderParse.MakeJsInfo(var AStrResult: string; AJsFileName, AJsFunction: string;
  AIsJaveScript: Boolean);
var
  strLang: string;
begin
  if AIsJaveScript then
    strLang := 'JavaScript'
  else
    strLang := 'VBscript';
  if AStrResult = '' then
    AStrResult := CtScript + AJsFileName + CtSmallSplitSign + AJsFunction + CtSmallSplitSign + strLang
  else
    AStrResult := AStrResult + CtSplitSign + AJsFileName + CtSmallSplitSign + AJsFunction + CtSmallSplitSign + strLang;
end;

class procedure TSpiderParse.MakeReplaceInfo(var AStrResult: string; const AStrOldValue, AStrNewValue: string;
  AReplaceCount: Integer);
begin
  if AStrResult = '' then
    AStrResult := CtReplace + AStrOldValue + CtSmallSplitSign + AStrNewValue + CtSmallSplitSign + IntToStr( AReplaceCount )
  else
    AStrResult := AStrResult + CtSplitSign + AStrOldValue + CtSmallSplitSign + AStrNewValue + CtSmallSplitSign + IntToStr( AReplaceCount );
end;

procedure TSpiderParse.NotifyParse(AFieldTag: Integer; AFieldContent: string);
begin
  if Assigned(FOnSpiderParse) then
    FOnSpiderParse( Self, AFieldTag, AFieldContent );
end;

{
AFieldHandleInfo := Copy(AFieldHandleInfo, 5, Length(AFieldHandleInfo) - 4);
  strJsFileName := GetRangString( AFieldHandleInfo, #27 );
  strJsFunctionName := GetRangString( AFieldHandleInfo, #27 );
  strJsLang := AFieldHandleInfo;
  if (not FileExists(strJsFileName)) or (strJsFunctionName = '') or (strJsLang = '') or
     (not IsLetter(strJsFunctionName)) or (not IsLetter(strJsLang)) then
  begin
    NotifyParse( ATag, AStrFieldValue );
    Exit;
  end;
  AStrFieldValue := HandleFieldByJsFile( strJsFunctionName, strJsFunctionName, strJsLang, AStrFieldValue );
  NotifyParse( ATag, AStrFieldValue );
}
class procedure TSpiderParse.ParseHandleInfo(AHandleInfo: string; var AHandleType: TFieldHandleStyle;
  AInfo: TStringList);
var
  lt: TStringList;
  i: Integer;
  strInfo: string;
begin
  AHandleType := GetFieldStyle(AHandleInfo);
  if AHandleType in [fsReplace, fsDelete, fsJs] then
  begin
    AHandleInfo := Copy(AHandleInfo, 5, Length(AHandleInfo) - 4);
    lt := TStringList.Create;
    try
      case AHandleType of
        fsReplace:
        begin
          //旧值, 新值, 次数
          ParseFormatString( AHandleInfo, #8, lt );
          for i := 0 to lt.Count - 1 do
          begin
            strInfo := lt[i];
            AInfo.Add( GetRangString( strInfo, #27 ) );
            AInfo.Add( GetRangString( strInfo, #27 ) );
            AInfo.Add(  strInfo );
          end;
        end;
        fsDelete:
        begin
          //开始位置, 删除字符数
          ParseFormatString( AHandleInfo, #8, lt );
          for i := 0 to lt.Count - 1 do
          begin
            strInfo := lt[i];
            AInfo.Add( GetRangString( strInfo, #27 ) );
            AInfo.Add( strInfo );
          end;
        end;
        fsJs:
        begin
          //文件位置, 函数名称, 语言
          AInfo.Add( GetRangString( AHandleInfo, #27 ) );
          AInfo.Add( GetRangString( AHandleInfo, #27 ) );
          AInfo.Add( AHandleInfo );
        end;
      end;
    finally
      lt.Free;
    end;
  end;
end;

procedure TSpiderParse.SetText(const Value: string);
begin
  FText := Value;
end;

procedure TSpiderParse.SpiderRun;
var
  strText, strFieldValue: string;
  i: Integer;
  p: pFieldInfo;
begin
  if FText = '' then Exit;
  strText := FText;
  if EnableMainContent and (FMainContentFS <> '') and (FMainContentBS <> '') then
    strText := GetRangString( strText, FMainContentFS, FMainContentBS, dwNotDelete);
  for i := 0 to FParseFieldInfoList.Count - 1 do
  begin
    p := FParseFieldInfoList[i];
    strFieldValue := Trim(GetRangString( strText, p^.FFrontdSign, p^.FBackSign, p^.FDeleteWay));
    if p^.FDelEdgeUnVisible then
      DeleteEdgeUnVisible( strFieldValue );
    HandleFieldValue(p^.FTag, strFieldValue, p^.FFieldHandleInfo);
  end;
end;

end.

