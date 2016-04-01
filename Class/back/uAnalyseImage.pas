unit uAnalyseImage;

 {配合 uImageIdentify 单元一起使用的}
 /////////By Terry////////////////
 /////////2007-10-9///////////////
 ///作用: 设计图像的标识符,以识别图像//
interface

uses Classes, Windows, Graphics, uImageIdentify, SysUtils;

type
  TAnalyseImage = class(TObject)
  private
    FIdnetifyMsgFile: string;
    FIdentify: array of TIdentify;
    procedure SetIdnetifyMsgText(Value: TStringList);
    procedure SetIdentifyMsg(const Value: string);
    function IsTheString(const AIdentify: TIdentify; AStrSign: string): Boolean;

    procedure ParseIdentify(var AIdentify: TIdentify; AStrText: string);
    function GetCount(AStrText: string): Integer;
    procedure GetIdentify(AStrText: string; var AIdentify: TAryByte);
  public
    class procedure BuildBitmapIdentify(const ABitmapFileName, AIdentifyFileName: string; ASign: array of string); overload;
    class function BuildBitmapIdentify(const ABmp: TBitmap; ASign: array of string): TStringList; overload;
  public
    function GetIdnetifySign(const AIdentify: TIdentify): string;
    function GetQQImageMsg(const ABmp: TBitmap): string;
    function AnalyseBmpNum(const ABmp: TBitmap): string;
    property IdnetifyMsgFileName: string read FIdnetifyMsgFile write SetIdentifyMsg;
    property IdnetifyMsgList: TStringList write SetIdnetifyMsgText;
  end;

function DoEncryptString(AIdentifyMsg: string): string;
function DoDecryptString(AEncryptIdentifyMsg: string): string;

implementation

uses uStringHandle;


const
  CKeyValue = 'make by Terry(jiangXiaode) at 2007-10';

{ TAnalyseImage }

function TAnalyseImage.AnalyseBmpNum(const ABmp: TBitmap): string;
var
  BmpAnalyse: TBitmapIdentify;
  IdentifyMsg: TIdentify;
  X: Integer;
begin
  BmpAnalyse := TBitmapIdentify.Create;
  try
    X := 0;
    Result := '';
    IdentifyMsg := BmpAnalyse.AnalyseBitmap(ABmp, swLeft, X, 0);
    while BmpAnalyse.ScanIsSuccess do
    begin
      Result := Result + GetIdnetifySign(IdentifyMsg);
      X := BmpAnalyse.IdentifyRect.Right + 1;
      IdentifyMsg := BmpAnalyse.AnalyseBitmap(ABmp, swLeft, X, 0);
    end;
  finally
    BmpAnalyse.Free;
  end;
end;

class function TAnalyseImage.BuildBitmapIdentify(const ABmp: TBitmap; ASign: array of string): TStringList;
  procedure InitIdentifyMsg(var IdentifyMsg: TIdentify);
  begin
    IdentifyMsg.FSign := '';
    IdentifyMsg.FCount := 0;
    SetLength(IdentifyMsg.FAryLeftIdentify, 0);
    SetLength(IdentifyMsg.FAryRightIdentify, 0);
  end;
var
  BmpAnalyse: TBitmapIdentify;
  nCount, nXpos, iLoop: Integer;
  IdentifyMsg: TIdentify;
begin
  BmpAnalyse := TBitmapIdentify.Create;
  Result := TStringList.Create;
  Result.Clear;
  try
    nCount := Length(ASign);
    nXpos := 0;
    for iLoop := 0 to nCount - 1 do
    begin
      InitIdentifyMsg(IdentifyMsg);
      IdentifyMsg := BmpAnalyse.AnalyseBitmap(ABmp, swLeft, nXpos, 0);
      IdentifyMsg.FSign := ASign[iLoop];
      Result.Add(TBitmapIdentify.OutputMsg(IdentifyMsg));
      nXpos := BmpAnalyse.IdentifyRect.Right + 1;
    end;
    Result.Text := DoEncryptString(Result.Text);
  finally
    BmpAnalyse.Free;
  end;
end;

class procedure TAnalyseImage.BuildBitmapIdentify(const ABitmapFileName, AIdentifyFileName: string;
  ASign: array of string);
var
  bmp: TBitmap;
  BmpAnalyse: TBitmapIdentify;
  StrIdentify: TStringList;
  nCount, nXpos, iLoop: Integer;
  IdentifyMsg: TIdentify;
begin
  bmp := TBitmap.Create;
  BmpAnalyse := TBitmapIdentify.Create;
  StrIdentify := TStringList.Create;
  try
    bmp.LoadFromFile(ABitmapFileName);
    nCount := Length(ASign);
    nXpos := 0;
    for iLoop := 0 to nCount - 1 do
    begin
      IdentifyMsg := BmpAnalyse.AnalyseBitmap(bmp, swLeft, nXpos, 0);
      IdentifyMsg.FSign := ASign[iLoop];
      StrIdentify.Add(TBitmapIdentify.OutputMsg(IdentifyMsg));
      nXpos := BmpAnalyse.IdentifyRect.Right + 1;
    end;
    StrIdentify.Text := DoEncryptString(StrIdentify.Text);
    StrIdentify.SaveToFile(AIdentifyFileName);
  finally
    bmp.Free;
    BmpAnalyse.Free;
    StrIdentify.Free;
  end;
end;

//解密

function DoDecryptString(AEncryptIdentifyMsg: string): string;
begin
  Result := AEncryptIdentifyMsg;
end;

//加密

function DoEncryptString(AIdentifyMsg: string): string;
begin
  Result := AIdentifyMsg;
end;

function TAnalyseImage.GetCount(AStrText: string): Integer;
var
  nPos: Integer;
begin
  nPos := SearchSignPositon(AStrText, ',', swFromLeft, 0);
  Result := 0;
  while nPos > 0 do
  begin
    Inc(Result);
    inc(nPos);
    nPos := SearchSignPositon(AStrText, ',', swFromLeft, nPos);
  end;
end;

procedure TAnalyseImage.GetIdentify(AStrText: string; var AIdentify: TAryByte);
var
  nPos: Integer;
  nIndex: Integer;
begin
  nPos := pos(',', AStrText);
  nIndex := 0;
  while nPos > 0 do
  begin
    AIdentify[nIndex] := StrToInt(Copy(AStrText, 1, nPos - 1));
    Inc(nIndex);
    Delete(AStrText, 1, nPos);
    nPos := pos(',', AStrText);
  end;
end;

function TAnalyseImage.GetIdnetifySign(const AIdentify: TIdentify): string;
  function JudgeAryIsSame(const ASrcAry, ADecAry: TAryByte): Boolean;
  var
    iLoop: Integer;
  begin
    Result := True;
    for iLoop := Low(ASrcAry) to High(ASrcAry) do
      if ASrcAry[iLoop] <> ADecAry[iLoop] then
      begin
        Result := False;
        Break;
      end;
  end;

var
  iLoop: Integer;
begin
  Result := '-1';
  for iLoop := Low(FIdentify) to High(FIdentify) do
  begin
    if (AIdentify.FCount = FIdentify[iLoop].FCount) then
    begin
      if (Length(AIdentify.FAryLeftIdentify) = Length(FIdentify[iLoop].FAryLeftIdentify)) and
        (JudgeAryIsSame(AIdentify.FAryLeftIdentify, FIdentify[iLoop].FAryLeftIdentify)) then
      begin
        if AIdentify.FCount > 1 then
        begin
          if (Length(AIdentify.FAryRightIdentify) = Length(FIdentify[iLoop].FAryRightIdentify)) and
            (JudgeAryIsSame(AIdentify.FAryRightIdentify, FIdentify[iLoop].FAryRightIdentify)) then
          begin
            Result := FIdentify[iLoop].FSign;
            Break;
          end;
        end
        else
        begin
          Result := FIdentify[iLoop].FSign;
          Break;
        end;
      end;
    end;
  end;
end;

function TAnalyseImage.GetQQImageMsg(const ABmp: TBitmap): string;
var
  AnalyseBmp: TBitmapIdentify;
  nTop: Integer;
  ptRight, ptLeft: TPoint;
  Identify: TIdentify;
  bmp: TBitmap;
begin
  AnalyseBmp := TBitmapIdentify.Create;
  bmp := TBitmap.Create;
  try
    //图片中最后一个 ")"
    nTop := 0;
//    ABmp.SAveToFile('D:\delpih\MyProjects\MyShareSoft\QQCoveyPlane\Bin\yy.bmp');
    while True do
    begin
      Identify := AnalyseBmp.AnalyseBitmap(ABmp, swRight, ABmp.Width, nTop);
      if not AnalyseBmp.ScanIsSuccess then
        Break;
      nTop := AnalyseBmp.IdentifyRect.Top + 1;
      if IsTheString(Identify, ')') then
      begin
        ptRight.X := AnalyseBmp.IdentifyRect.Left - 1;
        ptRight.Y := AnalyseBmp.IdentifyRect.Bottom;
        Break;
      end;
    end;

    // 图片中最后一个 "("
    nTop := 0;
    while True do
    begin
      Identify := AnalyseBmp.AnalyseBitmap(ABmp, swRight, ptRight.X, nTop);
      if not AnalyseBmp.ScanIsSuccess then
        Break;
      nTop := AnalyseBmp.IdentifyRect.Top + 1;
      if IsTheString(Identify, '/') then
      begin
        ptLeft.X := AnalyseBmp.IdentifyRect.Right + 1;
        ptLeft.Y := AnalyseBmp.IdentifyRect.Top;
        Break;
      end;
    end;
    bmp.Width := ptRight.X - ptLeft.X + 1;
    bmp.Height := ptRight.Y - ptLeft.Y;
    BitBlt(bmp.Canvas.Handle, 0, 0, bmp.Width, bmp.Height, ABmp.Canvas.Handle, ptLeft.X, ptLeft.Y, SRCCOPY);
    Result := AnalyseBmpNum(bmp);
  finally
    bmp.Free;
    AnalyseBmp.Free;
  end;
end;

function TAnalyseImage.IsTheString(const AIdentify: TIdentify; AStrSign: string): Boolean;
begin
  Result := GetIdnetifySign(AIdentify) = AStrSign;
end;

procedure TAnalyseImage.ParseIdentify(var AIdentify: TIdentify; AStrText: string);
var
  strTmp: string;
  nCount: Integer;
begin
  strTmp := GetRangString(AStrText, '{', '}');
  AIdentify.FSign := strTmp;
  strTmp := GetRangString(AStrText, '[', ']');
  AIdentify.FCount := StrToInt(strTmp);

  //Left Identify
  strTmp := GetRangString(AStrText, '(', ')') + ',';
  nCount := GetCount(strTmp);
  SetLength(AIdentify.FAryLeftIdentify, nCount);
  GetIdentify(strTmp, AIdentify.FAryLeftIdentify);
  //Right Identify
  if AIdentify.FCount > 1 then
  begin
    strTmp := GetRangString(AStrText, '(', ')', dwNotDelete) + ',';
    if strTmp = ',' then
      Exit;
    nCount := GetCount(strTmp);
    SetLength(AIdentify.FAryRightIdentify, nCount);
    GetIdentify(strTmp, AIdentify.FAryRightIdentify);
  end;
end;

procedure TAnalyseImage.SetIdentifyMsg(const Value: string);
var
  strText: string;
  F: TStringList;
  iLoop: Integer;
begin
  FIdnetifyMsgFile := Value;
  F := TStringList.Create;
  try
    F.LoadFromFile(FIdnetifyMsgFile);
    strText := DoDecryptString(F.Text);
    F.Text := strText;
    SetLength(FIdentify, F.Count);
    for iLoop := 0 to F.Count - 1 do
      ParseIdentify(FIdentify[iLoop], F.Strings[iLoop]);
  finally
    F.Free;
  end;
end;

procedure TAnalyseImage.SetIdnetifyMsgText(Value: TStringList);
var
  strText: string;
  iLoop: Integer;
begin
  strText := DoDecryptString(Value.Text);
  Value.Text := strText;
  SetLength(FIdentify, Value.Count);
  for iLoop := 0 to Value.Count - 1 do
    ParseIdentify(FIdentify[iLoop], Value.Strings[iLoop]);
end;

end.
