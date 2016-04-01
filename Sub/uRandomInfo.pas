unit uRandomInfo;

//////////////////////////////////////////////////
//#{30-150} :随机生成数字X: 30<=X<=150
//${3-5}: 随机生成字母n个字母: 3<=n<=5
//注: #{, ${中间不能有空格
//例子: jxdTerry#{4-10}@${3-4}.${2-3}
//       生成类似: jxdTerry8@sic.xo
/////////////////////////////////////////////////
interface
  uses Classes, uStringHandle, SysUtils;

function GetRandomNum(const AMin, AMax: Integer): Integer;
function GetRandomChar(const AIsOnlyLetter: Boolean): Char;
function GetRandomString(const AMin, AMax: Integer; const AOnlyChar: Boolean = True): string;
function RandomFormat(const ASrcText: string): string;

implementation

const
  CtNumberSign = '#{';
  CtLetterSign = '${';
  CtEndSign = '}';
  CtSplitSign = '-';
  CtAllChar = '^E#W0RI&1PCS8-N+Qe*Y()f=|:;''gA_xXJ6[,b>52KD}?\"{MOonBm@Z3c$HGwalj.di<h7Ftrs\uvLTkqU9p4%Vy]z~!';
  CtLetterChar = 'GrQsDLUWxMFqOghNIjtVkolCXzHmYidZeJSpTafvEAwBKnbRuPcy';

function BuildRandom(const ASrcText, ABeginSign, AEndSign, ASplitSign: string; const bRandNum: Boolean): string;
var
  nBeginPos, nEndPos: Integer;
  nBeginSignLen, nEndSignLen: Integer;
  strTmp: string;
  strMin, strMax, strNew: string;
  nMin, nMax: Integer;
begin
  nBeginSignLen := Length( ABeginSign );
  nEndSignLen := Length( AEndSign );
  Result := ASrcText;
  nBeginPos := SearchSignPositon( Result, ABeginSign );
  while nBeginPos > 0 do
  begin
    nEndPos := SearchSignPositon( Result, AEndSign, swFromLeft, nBeginPos + nBeginSignLen + 1 );
    if nEndPos <= 0 then Break;
    strTmp := Copy( Result, nBeginPos, nEndPos + nEndSignLen - nBeginPos );// #{3-23}
    StrMin := Trim( GetRangString(strTmp, ABeginSign, ASplitSign, dwNotDelete) );
    strMax := Trim( GetRangString(strTmp, ASplitSign, AEndSign, dwNotDelete) );
    try
      nMin := StrToInt( StrMin );
      nMax := StrToInt( StrMax );
      if nMin <= nMax then
      begin
        if bRandNum then
          strNew := IntToStr( GetRandomNum( nMin, nMax ) )
        else
          strNew := GetRandomString( nMin, nMax );
        uStringHandle.StringReplace(Result, strTmp, strNew, swFromLeft, 1);
      end;
    except
    end;
    nBeginPos := SearchSignPositon( Result, ABeginSign );
  end;
end;

function RandomFormat(const ASrcText: string): string;
begin
  Result := BuildRandom(ASrcText, CtNumberSign, CtEndSign, CtSplitSign, True);
  Result := BuildRandom(Result, CtLetterSign, CtEndSign, CtSplitSign, False);
end;

function GetRandomNum(const AMin, AMax: Integer): Integer;
begin
  Result := AMin + (Random( MaxInt ) mod (AMax - AMin + 1));
end;

function GetRandomChar(const AIsOnlyLetter: Boolean): Char;
var
  nIndex: Integer;
begin
  if AIsOnlyLetter then
  begin
    nIndex := GetRandomNum( 1, Length(CtLetterChar) - 1 );
    Result := CtLetterChar[nIndex];
  end
  else
  begin
    nIndex := GetRandomNum( 1, Length(CtAllChar) - 1 );
    Result := CtAllChar[nIndex];
  end;
end;

function GetRandomString(const AMin, AMax: Integer; const AOnlyChar: Boolean): string;
var
  nCount: Integer;
  i: Integer;
begin
  nCount := GetRandomNum(AMin, AMax);
  Result := '';
  for i := 0 to nCount - 1 do
  begin
    Result := Result + GetRandomChar( AOnlyChar );
  end;  
end;

Initialization
  Randomize;
end.
