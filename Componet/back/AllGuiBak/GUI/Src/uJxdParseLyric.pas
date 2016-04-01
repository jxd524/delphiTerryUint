unit uJxdParseLyric;

interface

uses
  Windows, SysUtils, Classes, Forms;

type
  PLine = ^TLines;
  TLines = record
    LineTime: LongInt;
    LineString: string;
  end;

  PLineInfo = ^TLineInfo;
  TLineInfo = record
    Index: Integer;
    StartTime: LongInt;
    ScrollTime: LongInt;
    ScrollString: string;
  end;
 {$M+}
  TParseLyric = class
    private
      FTimeOffset: Integer;
      FLyricName: string;
      FLyricList: TList;   //存放歌词内容
      FModify: Boolean;   //歌词是否已修改
      FOffsetIndex: Integer;
      FLyricLineCount: Integer;
      FLyricLastTime: Integer;
      //取得方括号内的内容
      function GetSquareBracketsMsg(var AText: string; var Content: string): Boolean;
      //取得分号两边的标识
      procedure GetSemicolon(const AText: string; var AForwardSign, ABackSign: string);
      function GetLineString(ALine: string): string;
      function JudgeIsInteger(AText: string): Boolean;
      procedure SetTimeOffset(const Value: Integer);
    public
      constructor Create;
      destructor Destroy; override;
      function LoadLyricFile(ALyricName: string): Boolean;
      procedure GetLyricMsg(AStrList: TStringList);
      function GetLyricLineString(AIndex: Integer; var ALineString: string): Boolean;
      function TimeToStr(ATime: Integer): string;
      function GetLineInfo(ATime: Integer; var ALineInfo: TLineInfo): Boolean;
      function GetLineInfoByIndex(AIndex: Integer; var ALineInfo: TLineInfo): Boolean;
      procedure SaveLyricFile;
      procedure ClearAllValue;
    published
      property LyricName: string read FLyricName;
      property TimeOffset: Integer read FTimeOffset write SetTimeOffset;
      property Modify: Boolean read FModify;
      property LyricLineCount: Integer read FLyricLineCount;
      property LyricLastTime: Integer read FLyricLastTime;
  end;

  function QuickSortLyricList(AItem1, AItem2: Pointer): Integer;

implementation

{ TParseLyric }

procedure TParseLyric.ClearAllValue;
var
  i: Integer;
begin
  FTimeOffset := 0;
  FOffsetIndex := 0;
  FLyricLineCount := 0;
  FLyricLastTime := 0;
  FLyricName := '';
  FModify := False;
  for i := FLyricList.Count - 1 downto 0 do
    Dispose(PLine(FLyricList.Items[i]));
  FLyricList.Clear;
end;

constructor TParseLyric.Create;
begin
  FLyricList := TList.Create;
  ClearAllValue;
end;

destructor TParseLyric.Destroy;
begin
  ClearAllValue;
  FLyricList.Free;
  inherited;
end;

function TParseLyric.GetLineInfo(ATime: Integer;
  var ALineInfo: TLineInfo): Boolean;
var
  i: Integer;
begin
  ZeroMemory(@ALineInfo, SizeOf(TLineInfo));
  for I := 0 to FLyricList.Count - 1 do
  begin
    if i = FLyricList.Count - 1 then
    begin
      ALineInfo.Index := I;
      ALineInfo.StartTime := PLine(FLyricList.Items[i]).LineTime - FTimeOffset;
      ALineInfo.ScrollTime := 60000;
      ALineInfo.ScrollString := PLine(FLyricList.Items[i]).LineString;
      Break;
    end;
    if (ATime >= PLine(FLyricList.Items[i]).LineTime - FTimeOffset) and
      (ATime < PLine(FLyricList.Items[i + 1]).LineTime - FTimeOffset) then
    begin
      ALineInfo.Index := I;
      ALineInfo.StartTime := PLine(FLyricList.Items[i]).LineTime - FTimeOffset;
      ALineInfo.ScrollTime := PLine(FLyricList.Items[i + 1]).LineTime - PLine(FLyricList.Items[i]).LineTime;
      ALineInfo.ScrollString := PLine(FLyricList.Items[i]).LineString;
      Break;
    end;
  end;
  Result := True;
end;

function TParseLyric.GetLineString(ALine: string): string;
var
  i, nLen: Integer;
begin
  nLen := 0;
  for I := Length(ALine) downto 0 do
    if CompareText(']', ALine[i]) = 0 then
    begin
      nLen := i;
      break;
    end;
  Result := Copy(ALine, nLen + 1, Length(ALine));
end;

function TParseLyric.GetLyricLineString(AIndex: Integer;
  var ALineString: string): Boolean;
begin
  if (AIndex < 0) or (AIndex > FLyricLineCount - 1) then
    Result := False
  else begin
    Result := True;
    ALineString := PLine(FLyricList.Items[AIndex]).LineString;
  end;
end;

procedure TParseLyric.GetLyricMsg(AStrList: TStringList);
var
  i: Integer;
  p: PLine;
begin
  for I := 0 to FLyricList.Count - 1 do
  begin
    p := FLyricList.Items[i];
    AStrList.Add(IntToStr(p.LineTime) + ' | ' + p.LineString);
  end;
end;

function TParseLyric.GetLineInfoByIndex(AIndex: Integer;
  var ALineInfo: TLineInfo): Boolean;
begin
  Result := False;
  if (AIndex < 0) or (AIndex >= FLyricLineCount) then
    Exit;
  ZeroMemory(@ALineInfo, SizeOf(ALineInfo));
  if AIndex = FLyricLineCount - 1 then
  begin
    ALineInfo.Index := AIndex;
    ALineInfo.StartTime := PLine(FLyricList[FLyricLineCount - 1]).LineTime - FTimeOffset;
    ALineInfo.ScrollTime := 60000;
    ALineInfo.ScrollString := PLine(FLyricList[FLyricLineCount - 1]).LineString;
  end
  else begin
    ALineInfo.Index := AIndex;
    ALineInfo.StartTime := PLine(FLyricList[AIndex]).LineTime;
    ALineInfo.ScrollTime := PLine(FLyricList[AIndex + 1]).LineTime - PLine(FLyricList[AIndex]).LineTime;
    ALineInfo.ScrollString := PLine(FLyricList[AIndex]).LineString;
  end;
  Result := True;
end;

procedure TParseLyric.GetSemicolon(const AText: string; var AForwardSign,
  ABackSign: string);
var
  pos1: Integer;
begin
  pos1 := Pos(':', AText);
  AForwardSign := Copy(AText, 1, pos1 - 1);
  ABackSign := Copy(AText, pos1 + 1, Length(AText));
end;

function TParseLyric.GetSquareBracketsMsg(var AText, Content: string): Boolean;
var
  pos1, pos2: Integer;
begin
  pos1 := Pos('[', AText);
  pos2 := Pos(']', AText);
  if (pos1 <= 0) and (pos2 <= 0) then
  begin
    AText := '';
    Content := '';
    Result := False;
    Exit;
  end;
  
  Content := Trim(Copy(AText, pos1 + 1, pos2 - 2));
  AText := Trim(Copy(AText, pos2 + 1, Length(AText)));
  pos1 := Pos('[', AText);
  pos2 := Pos(']', AText);
  if (pos1 > 0) and (pos2 > 0) then
  begin
    Result :=  True;
  end else
    Result := False;
end;

function TParseLyric.JudgeIsInteger(AText: string): Boolean;
begin
  try
    StrToInt(AText);
    Result := True;
  except
    Result := False;
  end;
end;

function TParseLyric.LoadLyricFile(ALyricName: string): Boolean;
var
  nTime: Integer;
  function StrToTime(AStrTime: string): Integer;
  var
    LeftStr, RightStr: string;
  begin
    GetSemicolon(AStrTime, LeftStr, RightStr);
    Result := StrToIntDef(LeftStr, 0) * 60000 + Round(StrToFloatDef(RightStr, 0) * 1000);
  end;

  procedure AddLineNode(ATime: Integer; AStrContent: string);
  var
    P: PLine;
  begin
    if (ATime = 0) and (AStrContent = '') then Exit;
    New(P);
    if ATime = 0 then
    begin
      ATime := nTime;
      Inc(nTime, 100);
    end;
    p.LineTime := ATime;
    p.LineString := AStrContent;
    FLyricList.Add(p);
  end;
var
  LyricContent: TStrings;
  i: Integer;
  tmpMsg, tmpLine: string;
  LineContent: string;
  bNext: Boolean;
  LeftStr, RigthStr: string;
begin
  Result := False;
  nTime := 0;
  if not FileExists(ALyricName) then Exit;
  ClearAllValue;
  FLyricName := ALyricName;
  LyricContent := TStringList.Create;
  try
    LyricContent.LoadFromFile(FLyricName);

    for i := 0 to LyricContent.Count - 1 do
    begin
      tmpMsg := '';
      LineContent := Trim(LyricContent.Strings[i]);
      bNext := GetSquareBracketsMsg(LineContent, tmpLine);
      if (LineContent = '') and (not bNext) then //类似于歌手,编辑等
      begin
        GetSemicolon(tmpLine, LeftStr, RigthStr);
        if (CompareText('offset', LeftStr) = 0) and JudgeIsInteger(RigthStr) then
        begin  //时间偏移量
          FOffSetIndex := i;
          FTimeOffset := StrToInt(RigthStr);
          Continue;
        end;
        if (CompareText(LeftStr, 'ar') = 0) or (CompareText(LeftStr, 'ti') = 0) or
          (CompareText(LeftStr, 'al') = 0) or (CompareText(LeftStr, 'by') = 0) then
          AddLineNode(0, RigthStr)
        else
          AddLineNode(StrToTime(tmpLine), '');
        Continue;
      end else if (LineContent <> '') and (not bNext) then begin//[00:32] 内容
        AddLineNode(StrToTime(tmpLine), LineContent);
        Continue;
      end;

      tmpMsg := GetLineString(LineContent);
      AddLineNode(StrToTime(tmpLine), tmpMsg);
      while bNext do     //多个时间 对于一个内容
      begin
        bNext := GetSquareBracketsMsg(LineContent, tmpLine);
        if tmpLine <> '' then
          AddLineNode(StrToTime(tmpLine), tmpMsg);
      end;
    end;

    FLyricList.Sort(QuickSortLyricList);
  finally
    LyricContent.Free;
    FLyricLineCount := FLyricList.Count;
    FLyricLastTime := PLine(FLyricList.Items[FLyricLineCount - 1]).LineTime;
  end;
  Result := True;
end;


procedure TParseLyric.SaveLyricFile;
var
  FileList: TStrings;
begin
  if not FileExists(FLyricName) then
  begin
    FModify := False;
    Exit;
  end;

  if FModify then
  begin
    FileList := TStringList.Create;
    try
      FileList.LoadFromFile(FLyricName);
      FileList.Strings[FOffSetIndex] := '[offset:' + IntToStr(FTimeOffset) + ']';
      FileList.SaveToFile(FLyricName);
    finally
      FileList.Free;
      FModify := False;
    end;
  end;
end;

procedure TParseLyric.SetTimeOffset(const Value: Integer);
begin
  if Value <> FTimeOffset then
  begin
    FTimeOffset := Value;
    FModify := True;
  end;
end;

function TParseLyric.TimeToStr(ATime: Integer): string;
var
  LeftStr, RightStr: string;

begin
  LeftStr := IntToStr(ATime div 60000);
  RightStr := FloatToStr((ATime - (ATime div 60000) * 60000) / 1000);
  Result := LeftStr + ':' + RightStr;
end;

function QuickSortLyricList(AItem1, AItem2: Pointer): Integer;
begin
  if PLine(AItem1).LineTime > PLine(AItem2).LineTime then
    Result := 1
  else if PLine(AItem1).LineTime < PLine(AItem2).LineTime then
    Result := -1
  else
    Result := 0;
end;

end.
