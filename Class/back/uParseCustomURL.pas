{
解析自定义的URL
自定义URL格式: http://www.getsoft.com/[1-49, abc,]info?[ako,jskd,aldk]...
其中[]内的内容为要解析的内容. 1-49 解析出: 1, 2, 3...49
                               其它字符串直接解析.最后将所有结果组合成新的URL
解析出:
    http://www.getsoft.com/1info?ako...
    http://www.getsoft.com/2info?ako...
    http://www.getsoft.com/abcinfo?ako...
    http://www.getsoft.com/1info?jskd...
}
unit uParseCustomURL;

interface
  uses SysUtils, Classes, uStringHandle, uSysSub;

type
  {$M+}
  TParseInfoStyle = (psNumber, psLetter, psN2N, psL2L);
  TParseCustomURL = class
  private
    FCustomURL: string;
    FKeepUrlList: TStringList;
    FParseUrlList: TStringList;
    FParsedOkURL: TStringList;

    procedure SetCustomURL(const Value: string);

  private
    procedure InitParseInfo;
    procedure CompartCustomUrl;
    procedure Parse;
  protected
    procedure Combi(AKeepLt: TStringList; Alt: TList);
    function JudageStyle(const ACustomInfo: string): TParseInfoStyle;
    function ParseCustomInfo(const ACustomInfo: string): TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ParsedOkURL: TStringList read FParsedOkURL;
    property CustomURL: string read FCustomURL write SetCustomURL;
  end;
implementation

{ TParseCustomURL }
const
  CtCustomFS = '[';
  CtCustomBS = ']';
  CtSing = '-';
{
http://www.getsoft.com/[1-49, abc]info?[ako,jskd,aldk]/nextpage[a-z]?[0-999][A-Z]
}
procedure TParseCustomURL.Combi(AKeepLt: TStringList; Alt: TList);
  procedure C(AKeepURL: string; AVlt, ASavelt: TStringList);
  var
    i: Integer;
  begin
    for i := 0 to AVlt.Count - 1 do
      ASavelt.Add( AKeepURL + AVlt[i] );
  end;
  
  procedure C2(Alt, AVlt: TStringList);
  var
    i, j: Integer;
    lt: TStringList;
  begin
    lt := TStringList.Create;
    try
      for i := 0 to Alt.Count - 1 do
      begin
        for j := 0 to AVlt.Count - 1 do
          lt.Add( Alt[i] + AVlt[j] );
      end;
      Alt.Clear;
      for i := 0 to lt.Count - 1 do
        Alt.Add( lt[i] );  
    finally
      lt.Free;
    end;

  end;

  procedure A(ASing: string; Alt: TStringList);
  var
    i: Integer;
  begin
    for i := 0 to Alt.Count - 1 do
      Alt[i] := Alt[i] + ASing;
  end;
var
  lt: TStringList;
  i: Integer;
  strUrl: string;
begin
  lt := TStringList.Create;
  try
    i := 0;
    if i >= FKeepUrlList.Count then
      Exit;
    strUrl := FKeepUrlList[i];

    if i >= Alt.Count then
      Exit;
    C( strUrl, Alt[i], lt);

    while True do
    begin
      Inc(i);
      if i >= FKeepUrlList.Count then
        Break;
      A(FKeepUrlList[i], lt);
      
      if i >= Alt.Count then
        Break;
      C2( lt, Alt[i] );
    end;
  finally
    FParsedOkURL.Assign( lt );
    lt.Free;
  end;
end;

procedure TParseCustomURL.CompartCustomUrl;
var
  strParseURL, strTmp: string;
begin
  strParseURL := FCustomURL;
  while strParseURL <> '' do
  begin
    //不变部分
    strTmp := GetRangString(strParseURL, CtCustomFS);
    if strTmp = '' then
    begin
      if Pos(CtCustomBS, strParseURL) > 0 then
        FKeepUrlList.Add( strTmp)
      else
      begin
        FKeepUrlList.Add( strParseURL );
        Break;
      end;
    end
    else
      FKeepUrlList.Add( strTmp );
    //变化部分
    strTmp := GetRangString(strParseURL, CtCustomBS);
    if strTmp = '' then //格式错误
    begin
      FKeepUrlList.Add( FCustomURL );
      FParseUrlList.Clear;
      Break;
    end
    else
      FParseUrlList.Add( strTmp );
  end;
end;

constructor TParseCustomURL.Create;
begin
  FKeepUrlList := TStringList.Create;
  FParseUrlList := TStringList.Create;
  FParsedOkURL := TStringList.Create;
  FCustomURL := '';
  InitParseInfo;
end;

destructor TParseCustomURL.Destroy;
begin
  FKeepUrlList.Free;
  FParseUrlList.Free;
  FParsedOkURL.Free;
  inherited;
end;

procedure TParseCustomURL.InitParseInfo;
begin
  FKeepUrlList.Clear;
  FParseUrlList.Clear;
  FParsedOkURL.Clear;
end;

function TParseCustomURL.JudageStyle(const ACustomInfo: string): TParseInfoStyle;
var
  nPos: Integer;
  str1, str2: string;
begin
  nPos := Pos( CtSing, ACustomInfo );
  if nPos > 0 then
  begin
    str1 := Trim( Copy(ACustomInfo, 1, nPos - 1) );
    str2 := Trim( Copy(ACustomInfo, nPos + 1, Length(ACustomInfo) - nPos) );
    if (Length(str1) = 1) and (Length(str2) = 1) and
       (str1[1] in ['a'..'z', 'A'..'Z']) and (str2[1] in ['a'..'z', 'A'..'Z']) then
      Result := psL2L
    else
    begin
      try
        StrToInt( str1 );
        StrToInt( str2 );
        Result := psN2N;
      except
        Result := psLetter;
      end;
    end;
  end
  else
  begin
    try
      StrToInt( ACustomInfo );
      Result := psNumber;
    except
      Result := psLetter;
    end;
  end;
end;

procedure TParseCustomURL.Parse;
var
  lt: TList;
  i: Integer;
begin
  InitParseInfo;
  CompartCustomUrl;
  if FParseUrlList.Count = 0 then
  begin
    FParsedOkURL.Text := FKeepUrlList.Text;
    Exit;
  end;
  lt := TList.Create;
  try
    for i := 0 to FParseUrlList.Count - 1 do
      lt.Add( ParseCustomInfo(FParseUrlList[i]) );
    Combi( FKeepUrlList, lt );
  finally
    for i := 0 to lt.Count - 1 do
      TStringList( lt[i] ).Free;
    lt.Free;
  end;
end;

function TParseCustomURL.ParseCustomInfo(const ACustomInfo: string): TStringList;

  procedure ParseN2N(AInfo: string; Alt: TStringList);
  var
    i, nNum1, nNum2: Integer;
  begin
    nNum1 := StrToIntDef( GetRangString(AInfo, CtSing), 0 );
    nNum2 := StrToIntDef( AInfo, -1 );
    for i := nNum1 to nNum2 do
      Alt.Add( IntToStr(i) );
  end;

  procedure ParseL2L(AInfo: string; Alt: TStringList);
  var
    str1, str2: string;
    i, nNum1, nNum2: Integer;
  begin
    str1 := GetRangString(AInfo, CtSing);
    if Length(str1) <> 1 then Exit;
    str2 := AInfo;
    if Length(str2) <> 1 then Exit;

    nNum1 := Integer( str1[1] );
    nNum2 := Integer( str2[1] );
    for i := nNum1 to nNum2 do
      Alt.Add( Char(i) );
  end;
  
var
  lt: TStringList;
  i: Integer;
  strParse: string;
  InfoStyle: TParseInfoStyle;
begin
  Result := TStringList.Create;
  lt := TStringList.Create;
  try
    ParseFormatString( ACustomInfo, ',', lt, True, True );
    for i := 0 to lt.Count - 1 do
    begin
      strParse := lt[i];
      InfoStyle := JudageStyle( strParse );
      case InfoStyle of
        psNumber, psLetter: Result.Add( strParse );
        psN2N: ParseN2N( strParse, Result );
        psL2L: ParseL2L( strParse, Result );
      end;
    end;
  finally
    lt.Free;
  end;
end;

procedure TParseCustomURL.SetCustomURL(const Value: string);
begin
  if FCustomURL = Value then Exit;
  FCustomURL := Value;
  Parse;
end;

end.
