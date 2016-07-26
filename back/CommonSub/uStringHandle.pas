{
描述：         字符串操作
作者：         江晓德
}
unit uStringHandle;

interface

uses SysUtils, StrUtils, Classes;

type
  //TDeleteWay: 删除的方向
  //dwFroward: 从前向删除(删除前面)
  //dwBack: 从后向前删除 (删除后面)
  TDeleteWay = (dwFroward, dwBack, dwNotDelete);

  //TSearchWay: 搜索方向
  //swFromLeft: 从前向后搜索(从左向右)
  TSearchWay = (swFromLeft, swFromRight);

  //TStringDelete: 删除字符串
  TStringDelete = (sdLeft, sdRight, sdBoth);


  TCharacterSign = (csNumber, csLowerLetter, csUpperLetter);

   //得到AForwardSign和ABackSign中间的字符串,不包括标识符本身
   //如果删除的话,连同标识符也删除
   function GetRangString(var AText: string; const AForwardSign, ABackSign: string;
                          ADeleteWay: TDeleteWay;
                          AForwardSearchWay: TSearchWay; AForwarBeginPos: Integer;
                          ABackSearchWay: TSearchWay; ABackBeginPos: Integer;
                          AIgnoreCase: Boolean): string;  overload;
   function GetRangString(var AText: string; const AForwardSign, ABackSign: string; ABeginSearchPos: Integer; ADeleteWay: TDeleteWay = dwFroward): string; overload;
   
   function GetRangString(var AText: string; const AForwardSign, ABackSign: string;
                          ADeleteWay: TDeleteWay = dwFroward;
                          AForardSearchWay: TSearchWay = swFromLeft; ABackSearchWay: TSearchWay = swFromLeft;
                          AIgnoreCase: Boolean = True): string;  overload;

   //得到 ASign 左边或右边的字符串.不包括标识符本身
   function GetRangString(var AText: string; const ASign: string; AIsGetLeftStr: Boolean = True;
                          ASearchWay: TSearchWay = swFromLeft; ADeleteWay: TDeleteWay = dwFroward; AIgnoreCase: Boolean = True): string;  overload;

   //在字符串的某一位置加入 ASubText; APos = 1: 最前面, <= 0: 最后面
   function IncludeString(const ASrcText, ASubText: string; APos: Integer = -1; AIsForceAdd: Boolean = False): string;


   //删除字符 左或右的N个字符
   function DeleteString(const ASrcText: string; ADeleteCount: Integer; ADeleteType: TStringDelete = sdLeft): string;

   //循环删除某个子串前面或后值
   function LoopDeleteString(ASrcText, ASignText: string; nLoopCount: Integer = -1; ADeleteSignStr: Boolean = True;
                             ASearchWay: TSearchWay = swFromLeft; ADeleteWay: TDeleteWay = dwFroward): string; overload;
  //循环删除查找到的字符;
   function LoopDeleteString(var AText: string; const AForwardSign, ABackSign: string;
                             ASearchPos: Integer = 0; ALoopCount:  Integer = -1;
                             ASearchWay: TSearchWay = swFromLeft; AIgnoreCase: Boolean = True): string; overload;


   //字符串替换
   procedure StringReplace(var ASrcText: string; const AOldSign, ANewSign: string; AReplaceWay: TSearchWay = swFromLeft;
                           AReplaceCount: Integer = -1);

   //用URL来得到存放的相对地址  网络符号变成本地符号
   function URLToRelativelyAddress(AURL: string): string;


   //搜索特定字符串在源字符串中的位置
   function SearchSignPositon(const ASrcText, ASign: string; ASearchWay: TSearchWay = swFromLeft;
                              ABeginPos: Integer = 0; AIgnoreCase: Boolean = True): Integer;

  //获取email
  procedure GetEmail(const ASrcText: string; AEmailList: TStringList; const AEmailSign: string = '@'); overload;
  function GetEmail(const ASrcText: string; const AEmailSign: string = '@'): string; overload;
  function IsAvailableEmailChar(const AChar: char): Boolean;
  function IsAvailableEmail(const ASrcText: string): Boolean;
  procedure ParseEmail(const ASrcText: string; var AUserName, ADomain: string);

  //判断格式, ASrcText 由 ASign1 和 ASign2 组成
  function JudgeFormat(const ASrcText: string; ASign1: array of TCharacterSign; ASign2: array of Char): Boolean;

  //解析字符串.将ASign分隔的子串保存到AList 中
  function ParseFormatString(const ASrcText, ASign: string; AList: TStrings;
                             const AClearRepeat: Boolean = True; const ATrim: Boolean = False): Integer;
  //删除字符串左右不可见字符, 包含空格也将被删除
  procedure DeleteEdgeUnVisible(var AText: string);

implementation
  uses uSysSub;

function SearchSignPositon(const ASrcText, ASign: string; ASearchWay: TSearchWay; ABeginPos: Integer; AIgnoreCase: Boolean): Integer;
var
  strSrcText, strSign: string;
  
  function GetFromRightPosition: Integer;  //从右向左搜索
  var
    iSrcLoop, nSignPos: Integer;
    tmpStr: string;
  begin
    Result := 0;
    nSignPos := Length(strSign);
    for iSrcLoop := Length(strSrcText) - ABeginPos downto 1 do
    begin
      if strSrcText[iSrcLoop] = strSign[nSignPos] then
      begin
        tmpStr := Copy(strSrcText, iSrcLoop - nSignPos + 1, nSignPos);
        if tmpStr = strSign then
        begin
          Result := iSrcLoop - nSignPos + 1;
          break;
        end;
      end;
    end;
  end;

begin
  if AIgnoreCase then
  begin
    strSrcText := LowerCase(ASrcText);
    strSign := LowerCase(ASign);
  end
  else
  begin
    strSrcText := ASrcText;
    strSign := ASign;
  end;

  if swFromLeft = ASearchWay then //从左向右搜索
  begin
    if ABeginPos = 0 then ABeginPos := 1;    
    Result := PosEx(strSign, strSrcText, ABeginPos)
  end
  else
    Result := GetFromRightPosition;
end;

function GetRangString(var AText: string; const AForwardSign, ABackSign: string;
                          ADeleteWay: TDeleteWay;
                          AForwardSearchWay: TSearchWay; AForwarBeginPos: Integer;
                          ABackSearchWay: TSearchWay; ABackBeginPos: Integer;
                          AIgnoreCase: Boolean): string;
var
  nForwardPos, nBackPos: Integer;
begin
  Result := '';
  nForwardPos := SearchSignPositon(AText, AForwardSign, AForwardSearchWay, AForwarBeginPos, AIgnoreCase);
  if nForwardPos <= 0 then
    Exit;

  if ABackSearchWay = swFromLeft then
    nBackPos := SearchSignPositon(AText, ABackSign, swFromLeft, nForwardPos + Length(AForwardSign), AIgnoreCase)
  else
    nBackPos := SearchSignPositon(AText, ABackSign, swFromRight, ABackBeginPos, AIgnoreCase);

  if nForwardPos < nBackPos then
  begin
    Result := Copy(AText, nForwardPos + Length(AForwardSign), nBackPos - (nForwardPos + Length(AForwardSign)));
    case ADeleteWay of
      dwFroward: Delete(AText, 1, nBackPos + Length(ABackSign) - 1);
      dwBack:    Delete(AText, nForwardPos , Length(AText) - nForwardPos + 1);
      dwNotDelete: ;
    end;
  end;
end;

function GetRangString(var AText: string; const AForwardSign, ABackSign: string; ABeginSearchPos: Integer; ADeleteWay: TDeleteWay): string;
begin
  Result := GetRangString(AText, AForwardSign, ABackSign, ADeleteWay, swFromLeft, ABeginSearchPos, swFromLeft, 0, True);
end;

function GetRangString(var AText: string; const AForwardSign, ABackSign: string; ADeleteWay: TDeleteWay; AForardSearchWay, ABackSearchWay: TSearchWay; AIgnoreCase: Boolean): string;
begin
  Result := GetRangString(AText, AForwardSign, ABackSign, ADeleteWay, AForardSearchWay, 0, ABackSearchWay, 0, AIgnoreCase);
end;

function GetRangString(var AText: string; const ASign: string; AIsGetLeftStr: Boolean;
  ASearchWay: TSearchWay; ADeleteWay: TDeleteWay; AIgnoreCase: Boolean): string;
var
  nPos: Integer;
begin
  Result := '';
  nPos := SearchSignPositon(AText, ASign, ASearchWay, 0, AIgnoreCase);

  if nPos > 0 then
  begin
    if AIsGetLeftStr then
      Result := Copy(AText, 1, nPos - 1)
    else
      Result := Copy(AText, nPos + Length(ASign), Length(AText) - nPos);

    case ADeleteWay of
      dwFroward: Delete(AText, 1, nPos + Length(ASign) - 1);
      dwBack:    Delete(AText, nPos , Length(AText) - nPos + 1);
      dwNotDelete: ;
    end;
  end;
end;
 
function DeleteString(const ASrcText: string; ADeleteCount: Integer; ADeleteType: TStringDelete): string;
var
  nSignPos: Integer;
begin
  Result := '';
  if Length(ASrcText) >= ADeleteCount then
  begin
    Result := ASrcText;
    case ADeleteType of
      sdLeft:
      begin
        while ADeleteCount > 0 do
        begin
          if ByteType(Result, 1) = mbLeadByte then
            Delete(Result, 1, 2)
          else
            Delete(Result, 1, 1);
          Dec(ADeleteCount);
        end;
      end;
      sdRight:
      begin
        while ADeleteCount > 0 do
        begin
          nSignPos := Length(Result);
          if ByteType(Result, nSignPos) = mbTrailByte then
            Delete(Result, nSignPos - 1, 2)
          else
            Delete(Result, nSignPos, 1);
          Dec(ADeleteCount);
        end;
      end;
      sdBoth:
      begin
        Result := DeleteString(Result, ADeleteCount, sdLeft);
        Result := DeleteString(Result, ADeleteCount, sdRight);
      end;
    end;
  end;
end;

function LoopDeleteString(ASrcText, ASignText: string; nLoopCount: Integer; ADeleteSignStr: Boolean;
                             ASearchWay: TSearchWay; ADeleteWay: TDeleteWay): string;
var
  nSignPos: Integer;
begin
  Result := ASrcText;
  if dwNotDelete = ADeleteWay then
    Exit;
  if nLoopCount <= 0 then
    nLoopCount := MaxInt;
  nSignPos := 0;
  while nLoopCount > 0 do
  begin
    nSignPos := SearchSignPositon(ASrcText, ASignText, ASearchWay, nSignPos);
    if (nSignPos <= 0) or ((not ADeleteSignStr) and (nSignPos = 1)) then Break;
    case ADeleteWay of
      dwFroward:
      begin
        if ADeleteSignStr then
          nSignPos := nSignPos + Length(ASignText);
        Result := Copy(ASrcText, nSignPos, Length(ASrcText) - nSignPos + 1);
        Delete(ASrcText, 1, nSignPos - 1);
        nSignPos := Length(ASignText);
      end;
      dwBack:
      begin
        if not ADeleteSignStr then
          nSignPos := nSignPos + Length(ASignText);
        Result := Copy(ASrcText, 1, nSignPos - 1);
        Delete(ASrcText, nSignPos, Length(ASrcText) - nSignPos + 1);
        nSignPos := Length(ASignText);
      end;
      dwNotDelete: Break;
    end;
    Dec(nLoopCount);
  end;
end;

function LoopDeleteString(var AText: string; const AForwardSign, ABackSign: string;
                          ASearchPos: Integer = 0;ALoopCount:  Integer = -1;
                          ASearchWay: TSearchWay = swFromLeft; AIgnoreCase: Boolean = True): string;
var
  nForwardPos, nBackPos: Integer;
begin
  Result := AText;
  if ALoopCount = -1 then
    ALoopCount := MaxInt;
  while ALoopCount > 0 do
  begin
    nForwardPos := SearchSignPositon(AText, AForwardSign, ASearchWay, ASearchPos, AIgnoreCase);
    if nForwardPos <= 0 then
      Break;
    nBackPos := SearchSignPositon(AText, ABackSign, swFromLeft, nForwardPos + Length(AForwardSign) + 1);
    if nBackPos < nForwardPos then
      Break;
    nBackPos := nBackPos + Length(ABackSign);
    Delete(AText, nForwardPos, nBackPos - nForwardPos);
    Result := AText;
    Dec(ALoopCount);
    ASearchPos := 0;
  end;
end;

procedure StringReplace(var ASrcText: string; const AOldSign, ANewSign: string; AReplaceWay: TSearchWay = swFromLeft;
                           AReplaceCount: Integer = -1);
var
  nCompartPos, nLength: Integer;
  strLeft, strRight: string;
begin
  if AReplaceCount <= 0 then
    AReplaceCount := MaxInt;
  nLength := Length(AOldSign);
  while AReplaceCount > 0 do
  begin
    nCompartPos := SearchSignPositon(ASrcText, AOldSign, AReplaceWay);
    if nCompartPos <= 0 then Break;
    strLeft := Copy(ASrcText, 1, nCompartPos - 1);
    strRight := Copy(ASrcText, nCompartPos + nLength, Length(ASrcText) - nCompartPos - nLength + 1);
    ASrcText := strLeft + ANewSign + strRight;
    Dec(AReplaceCount);
  end;
end;

function URLToRelativelyAddress(AURL: string): string;
var
  npos: Integer;
begin
  AUrl := SysUtils.StringReplace(AUrl, 'http://', '', [rfReplaceAll, rfIgnoreCase]);
  nPos := SearchSignPositon(AURL, '/', swFromLeft);
  Result := Copy(AUrl, nPos + 1, Length(AUrl) - nPos);
  Result := SysUtils.StringReplace(Result, '/', '\', [rfReplaceAll]);
  Result := SysUtils.StringReplace(Result, '\\', '\', [rfReplaceAll]);
end;

function IncludeString(const ASrcText, ASubText: string; APos: Integer; AIsForceAdd: Boolean): string;
var
  nSubLen: Integer;
begin
  if (APos <= 0) or (APos >= Length(ASrcText)) then //在最后插入
  begin
    nSubLen := Length(ASubText);
    if Length(ASubText) < Length(ASrcText) then
    begin
      if AIsForceAdd then
        Result := ASrcText + ASubText
      else
      begin
        if (CompareText(Copy(ASrcText, Length(ASrcText) - nSubLen + 1, nSubLen), ASubText) <> 0) then
          Result := ASrcText + ASubText
        else
          Result := ASrcText;
      end;
    end
    else
      Result := ASrcText + ASubText;
  end
  else if APos = 1 then //在最前面插入
  begin
    if AIsForceAdd then
      Result := ASubText + ASrcText
    else
    begin
      nSubLen := Length(ASubText);
      if Length(ASubText) < Length(ASrcText) then
      begin
        if CompareText(Copy(ASrcText, 1, nSubLen), ASubText) <> 0 then
          Result := ASubText + ASrcText
        else
          Result := ASrcText;
      end
      else
        Result := ASubText + ASrcText;
    end;
  end
  else //在中间插入
  begin
    if ByteType(ASrcText, APos) = mbLeadByte then  //如果刚好是汉字
      APos := APos + 1;
    Result := Copy(ASrcText, 1, APos);
    Result := Result + ASubText;
    Result := Result + Copy(ASrcText, Apos + 1, Length(ASrcText) - Apos);
  end;
end;

function GetEmail(const ASrcText: string; const AEmailSign: string): string;
var
  ltEmail: TStringList;
begin
  ltEmail := TStringList.Create;
  try
    GetEmail(ASrcText, ltEmail, AEmailSign);
    if ltEmail.Count > 0 then
      Result :=ltEmail.Strings[0]
    else
      Result := '';
  finally
    ltEmail.Free;
  end;
end;

procedure GetEmail(const ASrcText: string; AEmailList: TStringList; const AEmailSign: string);
var
  nEmailSignPos: Integer;
  nFirstPos, nLastPos, nLen: Integer;
  strTmp: string;
begin
  nEmailSignPos := 0;
  nLen := Length(ASrcText);
  while True do
  begin
    nEmailSignPos := SearchSignPositon(ASrcText, AEmailSign, swFromLeft, nEmailSignPos, False);
    if nEmailSignPos <= 1 then
      Break;
    nFirstPos := nEmailSignPos - 1;
    nLastPos := nEmailSignPos + 1;
    //Email Left
    while nFirstPos > 0 do
    begin
      if IsAvailableEmailChar(ASrcText[nFirstPos]) then
        Dec(nFirstPos)
      else
      begin
        Inc(nFirstPos);
        Break;
      end;
    end;
    if nFirstPos = 0 then
      nFirstPos := 1;
    //Email Right
    while nLastPos <= nLen do
    begin
      if IsAvailableEmailChar(ASrcText[nLastPos]) then
        inc(nLastPos)
      else
      begin
        Dec(nLastPos);
        Break;
      end;
    end;
    if nLastPos > nLen then
      nLastPos := nLen;
    //Judge is email 
    if (nEmailSignPos - nFirstPos > 4) and  (nLastPos - nFirstPos > 6) and
      (IsNumber(ASrcText[nFirstPos]) or IsLetter(ASrcText[nFirstPos])) and
      ( (IsNumber(ASrcText[nLastPos]) ) or ( IsLetter(ASrcText[nLastPos]) )) then
    begin
      strTmp := Copy(ASrcText, nEmailSignPos + 1, nLastPos - nEmailSignPos);
      if Pos('.', strTmp) > 0 then
      begin
        strTmp := Copy(ASrcText, nFirstPos, nLastPos - nFirstPos + 1);
        AEmailList.Add(strTmp);
      end;
    end;
    Inc(nEmailSignPos);
  end;
end;

function IsAvailableEmailChar(const AChar: char): Boolean;
var
  cByte: Byte;
begin
  cByte := Ord(AChar);
  if (cByte = $2E) or (cByte = $40) or (cByte = $2D) or (cByte = $5F) or IsNumber(cByte) or IsLowerLetter(cByte) or IsUpperLetter(cByte) then
    Result := True
  else
    Result := False;
end;

function IsAvailableEmail(const ASrcText: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(ASrcText) do
    if not IsAvailableEmailChar(ASrcText[i]) then   Exit;
  Result := ( pos('@', ASrcText) > 0 ) and ( Pos('.', ASrcText) > 0 ) ;
end;

function JudgeFormat(const ASrcText: string; ASign1: array of TCharacterSign; ASign2: array of Char): Boolean;
var
  iLoop, jLoop: Integer;
  bFirstOk, bSencodOk: Boolean;
begin
  Result := True;
  bSencodOk := True;
  for iLoop := 1 to Length(ASrcText) do
  begin
    if not bSencodOk then
    begin
      Result := False;
      Break;
    end;
    bFirstOk := False;
    for jLoop := 0 to High(ASign1) do
    begin
      case ASign1[jLoop] of
        csNumber: Result := IsNumber(ASrcText[iLoop]);
        csLowerLetter: Result := IsLowerLetter(ASrcText[iLoop]);
        csUpperLetter: Result := IsUpperLetter(ASrcText[iLoop]);
      end;
      if Result then
      begin
        bFirstOk := True;
        Break;
      end;
    end;

    if bFirstOk then
      Continue;

    bSencodOk := False;
    for jLoop := 0 to High(ASign2) do
    begin
      if ASign2[jLoop] = ASrcText[iLoop] then
      begin
        bSencodOk := True;
        Result := True;
        Break;
      end
    end;
  end;
end;

procedure ParseEmail(const ASrcText: string; var AUserName, ADomain: string);
var
  AList: TStringList;
begin
  AList := TStringList.Create;
  try
    ParseFormatString(ASrcText, '@', AList, False, True);
    if AList.Count = 2 then
    begin
      AUserName := AList.Strings[0];
      ADomain := AList.Strings[1];
    end
    else if AList.Count = 1 then
    begin
      AUserName := AList.Strings[0];
      ADomain := '';
    end
    else
    begin
      AUserName := '';
      ADomain := '';
    end;
  finally
    AList.Free;
  end;
end;

function ParseFormatString(const ASrcText, ASign: string; AList: TStrings; const AClearRepeat: Boolean; const ATrim: Boolean): Integer;
  procedure AddMsgToList(const AMsg: string);
  var
    iLoop: Integer;
    bAdd: Boolean;
  begin
    bAdd := True;
    if AClearRepeat then
    begin
      for iLoop := 0 to AList.Count - 1 do
        if CompareText(AMsg, AList[iLoop]) = 0 then
        begin
          bAdd := False;
          Break;
        end;
    end;
    if bAdd then
      AList.Add(AMsg);
  end;
var
  nIndex: Integer;
  strSrc, strTmp: string;
begin
  if not Assigned(AList) then
    AList := TStringList.Create;
    
  nIndex := 0;
  strSrc := IncludeString(ASrcText, ASign);
  while True do
  begin
    strTmp := GetRangString(strSrc, ASign);
    if strTmp = '' then
      Break;
    Inc(nIndex);
    try
      AList.BeginUpdate;
      if ATrim then
        strTmp := Trim(strTmp);
      AddMsgToList(strTmp);
    finally
      AList.EndUpdate;
    end;
  end;
  Result := nIndex;
end;

procedure DeleteEdgeUnVisible(var AText: string);
var
  nPos: Integer;
begin
  nPos := 0;
  while IsVisibleChar(AText[nPos]) do
    Inc(nPos);
  if nPos <> 0 then
    AText := DeleteString(AText, nPos);

  nPos := Length(AText);
  while IsVisibleChar(AText[nPos]) do
    Dec(nPos);
  if nPos <> Length(AText) then
    AText := DeleteString(AText, Length(AText) - nPos, sdRight );
end;

end.
