unit uHtml2MIME;

interface
  uses Classes, SysUtils, uStringHandle, uRandomInfo;

type
  //HTML×ÊÔ´
  pHtmlRes = ^THtmlRes;
  THtmlRes = record
    FID: string;
    FFileName: string;
  end;
  {$M+}
  THtml2MIME = class
  private
    FText: string;
    FResList: TList;
    FFileName: string;
    FResIndex: Integer;
    procedure FreeResList;
    procedure SetText(const Value: string);
    procedure SetFileName(const Value: string);
    function GetHtmlText: string;
    function GetSimpleText: string;
    procedure AddHtmlRes(const AID, AFileName: string);
    function NewResID: string;
    function NewCid(const AResID: string): string;
    function GetResCount: Integer;
    function GetHtmlRes(index: Integer): pHtmlRes;
  public
    constructor Create;
    destructor Destroy; override;

    property ResItem[index: Integer]: pHtmlRes read GetHtmlRes;
  published
    property Text: string read FText write SetText;
    property FileName: string read FFileName write SetFileName;

    property SimpleText: string read GetSimpleText;
    property HtmlText: string read GetHtmlText;
    property ResCount: Integer read GetResCount;
  end;
implementation

{ THtml2MIME }
const
  CtImageSign = '<Img';
  CtImageSrc = 'src="';
  CtImgSrcLen = 5;
  CtEndSrc = '"';

procedure THtml2MIME.AddHtmlRes(const AID, AFileName: string);
var
  p: pHtmlRes;
begin
  New(p);
  p^.FID := AID;
  p^.FFileName := AFileName;
  FResList.Add( p );
end;

constructor THtml2MIME.Create;
begin
  Text := '';
  FileName := '';
  FResList := TList.Create;
  FResIndex := GetRandomNum(100, 9999);
end;

destructor THtml2MIME.Destroy;
begin
  FreeResList;
  FResList.Free;
  inherited;
end;

procedure THtml2MIME.FreeResList;
var
  i: integer;
begin
  for i := 0 to FResList.Count - 1 do
    if Assigned(FResList[i]) then
      Dispose(FResList[i]);
  FResList.Clear;
end;

function THtml2MIME.GetHtmlRes(index: Integer): pHtmlRes;
begin
  if (index < 0) or (index > ResCount) then
    Result := nil
  else
    Result := FResList[index];
end;

function THtml2MIME.GetHtmlText: string;
var
  S: string;
  nImagePos, nImgSrcPos, nEndSrcPos: Integer;
  strID, strImagePath: string;
begin
  FreeResList;
  s := Text;
  nImagePos := SearchSignPositon(S, CtImageSign);
  while nImagePos > 0 do
  begin
    nImgSrcPos := SearchSignPositon(S, CtImageSrc, swFromLeft, nImagePos + 4);
    if nImgSrcPos <= 0 then
      Break;
    nImgSrcPos := nImgSrcPos +  CtImgSrcLen;
    nEndSrcPos := SearchSignPositon(S, CtEndSrc, swFromLeft, nImgSrcPos + 1);
    if nEndSrcPos <= 0 then
      Break;
    strImagePath := Copy(S, nImgSrcPos, nEndSrcPos - nImgSrcPos);
    if strImagePath = ''	 then
      Continue;
    strID := NewResID;
    AddHtmlRes( strID, strImagePath );
    StringReplace(S, strImagePath, NewCid(strID), swFromLeft, 1);
    nImagePos := SearchSignPositon(S, CtImageSign, swFromLeft, nImagePos + nEndSrcPos + 1);
  end;
  Result := S;
end;

function THtml2MIME.GetResCount: Integer;
begin
  Result := FResList.Count;
end;

function THtml2MIME.GetSimpleText: string;
begin
  Result := Text;
  Result := LoopDeleteString(Result, '<style', '</style>');
  Result := LoopDeleteString(Result, '<script', '</script>');
  Result := LoopDeleteString(Result, '<', '>');
  StringReplace( Result, '&nbsp;', ' ');
end;

function THtml2MIME.NewCid(const AResID: string): string;
begin
  Result := 'cid: ' + AResID;
end;

function THtml2MIME.NewResID: string;
begin
  Result := GetRandokmString(8, 15) + IntToStr(FResIndex) + GetRandokmString(5, 12);
  Inc(FResIndex);
end;

procedure THtml2MIME.SetFileName(const Value: string);
var
  lt: TStringList;
begin
  if FileExists(Value) and ( CompareText(FFileName, Value) <> 0 ) then
  begin
    FFileName := Value;
    lt := TStringList.Create;
    try
      lt.LoadFromFile( FFileName );
      Text := lt.Text;
    finally
      lt.Free;
    end;
  end;
end;

procedure THtml2MIME.SetText(const Value: string);
begin
  FText := Value;
end;

end.
