{
单元名称: uJxdParseGradient
单元作者: 江晓德(jxd524@163.com)
说    明: 渐变颜色解析类
开始时间: 2007-10-08
修改时间: 2010-07-07 (最后修改)
}
unit uJxdParseGradient;

interface
uses
  uStringHandle, Graphics, Classes, SysUtils;

{$M+}
type
  PGradient = ^TGradient;
  TGradient = record
    FFromColor: TColor;
    FColorTo:   TColor;
    FPercent:   Double;
  end;
  TGradientWay = (gwUpToDown, gwLeftToRigth);
  TParseGradient = class
  private
    FGradientList: TList;
    FGradientMsgText: string;
    procedure ClearAllListNode;
    procedure SetGradientMsgText(const Value: string);
    procedure ParseMsg(AMsgText: string; AIsAddToList: Boolean);
    procedure OnAddMsgToList(const AFromColor, ColorTo: TColor; const APercent: Double);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property GradientMsg: string read FGradientMsgText write SetGradientMsgText;
    property GradientList: TList read FGradientList;
  end;

implementation

{ TParseGradient }

procedure TParseGradient.ClearAllListNode;
var
  iLoop: Integer;
begin
  for iLoop := 0 to FGradientList.Count - 1 do
    Dispose(FGradientList[iLoop]);
  FGradientList.Clear;
end;

constructor TParseGradient.Create;
begin
  FGradientList := TList.Create;
end;

destructor TParseGradient.Destroy;
begin
  ClearAllListNode;
  FGradientList.Free;
  inherited;
end;

procedure TParseGradient.OnAddMsgToList(const AFromColor, ColorTo: TColor; const APercent: Double);
var
  p: PGradient;
begin
  New(p);
  p^.FFromColor := AFromColor;
  p^.FColorTo := ColorTo;
  p^.FPercent := APercent;
  FGradientList.Add(p);
end;

procedure TParseGradient.ParseMsg(AMsgText: string; AIsAddToList: Boolean);
  function HandleContent(const AText: string): string;
  begin
    Result := AText;
//    Result := StringReplace( Result, ':', '', [rfReplaceAll] );
//    Result := StringReplace( Result, ';', '', [rfReplaceAll] );
//    Result := StringReplace( Result, '：', '', [rfReplaceAll] );
//    Result := StringReplace( Result, '；', '', [rfReplaceAll] );
//    Result := StringReplace( Result, ' ', '', [rfReplaceAll] );
//    Result := StringReplace( Result, #10, '', [rfReplaceAll] );
//    Result := StringReplace( Result, #13, '', [rfReplaceAll] );
    uStringHandle.StringReplace(Result, ':', '');
    uStringHandle.StringReplace(Result, ';', '');
    uStringHandle.StringReplace(Result, '：', '');
    uStringHandle.StringReplace(Result, '；', '');
    uStringHandle.StringReplace(Result, ' ', '');
    uStringHandle.StringReplace(Result, #10, '');
    uStringHandle.StringReplace(Result, #13, '');
  end;
  procedure HandleStrToColor(const AStr: string; var AValue: TColor);
  begin
    try
      AValue := StringToColor(AStr);
    except
      raise Exception.Create('颜色值不正确,请正确填写');
    end;
  end;
  function HandleStrToPercent(const AStr: string): Double;
  var
    strTmp: string;
    nValue: Integer;
  begin
    try
      if Pos('%', AStr) > 0 then
      begin
        strTmp := Astr;
        strTmp := GetRangString(strTmp, '%');
        nValue := StrToInt(strTmp);
        if (nValue > 100) or (nValue <= 0) then
          raise Exception.Create('Percent 的值要在 1% ~ 100%之间');
        Result := nValue / 100.00;
      end
      else
      begin
        Result := StrToFloat(Astr);
        if Result > 1.0 then
          raise Exception.Create('Percent 的值不能超过 1');
      end;
    except
      raise Exception.Create('Percent 值的格式有误');
    end;
  end;

var
  strFromColor, strColorTo, strPercent, strTmpContent: string;
  cFromColor, cColorTo: TColor;
  dPercent: Double;
  nCount: Integer;
begin
  nCount := 0;
  while True do
  begin
    strTmpContent := GetRangString(AMsgText, '#Gradient', '}');
    if strTmpContent = '' then
    begin
      if nCount = 0 then
        raise Exception.Create('输入的信息不正确')
      else
        Break;
    end;
    inc(nCount);
    strFromColor := HandleContent(GetRangString(strTmpContent, 'FromColor', ';'));
    if strFromColor = '' then
       raise Exception.Create('FromColor 不能为空');
    HandleStrToColor(strFromColor, cFromColor);

    strColorTo := HandleContent(GetRangString(strTmpContent, 'ColorTo', ';'));
    if strColorTo = '' then
      raise Exception.Create('ColorTo 不能为空');
    HandleStrToColor(strColorTo, cColorTo);

    strPercent := HandleContent(GetRangString(strTmpContent, 'Percent', False));
    if strPercent = '' then
      raise Exception.Create('Percent 不能为空');
    dPercent := HandleStrToPercent(strPercent);

    if AIsAddToList then
      OnAddMsgToList(cFromColor, cColorTo, dPercent);
  end;
end;

procedure TParseGradient.SetGradientMsgText(const Value: string);
begin
  ParseMsg(Value, False);
  ClearAllListNode;
  FGradientMsgText := Value;
  ParseMsg(FGradientMsgText, True);
end;

end.
