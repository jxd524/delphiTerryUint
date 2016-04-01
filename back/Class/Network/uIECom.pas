unit uIECom;

interface
uses
  SysUtils, Windows, ActiveX, Clipbrd, OleCtrls, MSHTML, Graphics, uSysSub;

{$M+}
type
  TIECom = class(TObject)
  private
    FDocument: IHTMLDocument2;
    function CheckDocument: Boolean;
    function FindInputElem(const ANameOrID: string): IHTMLInputElement;
    function FindInputElemByValue(const AValue: string): IHTMLInputElement;
    function FindImage(const AImgIndex: Integer): IHTMLImgElement; overload;
    function FindImage(const AImgSrcSign: string): IHTMLImgElement; overload;
    function CopyImageToBitmap(Aimg: IHTMLImgElement; ABmp: TBitmap): Boolean; overload; 
  public
    {图片处理}
    function GetImageSrcBySign(const AImgSrcSign: string; var AStrUrl: string): Boolean;
    function CopyImageToBitmap(const AImgIndex: Integer; ABmp: TBitmap): Boolean; overload;
    function CopyImageToBitmap(const AImgSrcSign: string; ABmp: TBitmap): Boolean; overload;
    //ANewFileName: 传入路径, 返回全路径
    function CopyImageToFile(const AImgSrcSign: string; var ANewFileName: string): Boolean;
    {输入框}
    function SetInputValue(const ANameOrId: string; const AValue: string): Boolean;
    function GetInputValue(const ANameOrId: string; var AValue: string): Boolean;
    function InputClick(const ANameOrId: string): Boolean;
    function InputClickByValue(const AValue: string): Boolean;
    {所有元素}
    function SetElemAttributeValue(const AID, AAttributeName, AAttributeValue: string): Boolean;
    function GetElemAttributeValue(const AID, AAttributeName: string; var AAttributeValue: string): Boolean;
    {获取接口}
    function GetFrame(const AIndex: Integer; var AFrm: IHTMLWindow2): Boolean;
    function GetLinkElem(const ALinkSign: string; var AElem: IHTMLElement): Boolean;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
  published
    property Document: IHTMLDocument2 read FDocument write FDocument;
  end;

implementation

{ TIECom }

function TIECom.CheckDocument: Boolean;
begin
  Result := FDocument <> nil;
end;

function TIECom.CopyImageToBitmap(const AImgIndex: Integer; ABmp: TBitmap): Boolean;
var
  img:IHTMLImgElement;
begin
  Result := False;
  if (not CheckDocument) or ( not Assigned(ABmp) ) then Exit;
  img := FindImage( AImgIndex );
  if img = nil then Exit;
  Result := CopyImageToBitmap( img, ABmp );
end;


function TIECom.CopyImageToFile(const AImgSrcSign: string; var ANewFileName: string): Boolean;
var
  strImgSrc, strLocaFileName, strNewFileName: string;
begin
  Result := False;
  if not CheckDocument then Exit;
  if not GetImageSrcBySign( AImgSrcSign, strImgSrc ) then Exit;
  if not GetCacheVerifyCodeFile( strImgSrc, strLocaFileName) then Exit;
  strNewFileName := ExtractFileName( strLocaFileName );
  ANewFileName := IncludeTrailingPathDelimiter( ANewFileName );
  ANewFileName := ANewFileName + strNewFileName;
  Result := CopyFile( PChar(strLocaFileName), PChar(ANewFileName), False );
end;

function TIECom.CopyImageToBitmap(const AImgSrcSign: string; ABmp: TBitmap): Boolean;
var
  img:IHTMLImgElement;
begin
  Result := False;
  if (not CheckDocument) or ( not Assigned(ABmp) ) then Exit;
  img := FindImage(AImgSrcSign);
  if img = nil then Exit;
  Result := CopyImageToBitmap( img, ABmp );
end;

function TIECom.CopyImageToBitmap(Aimg: IHTMLImgElement; ABmp: TBitmap): Boolean;
var
  Element2: IHTMLElement2;
  CtrlRange: IHTMLControlRange;
  ce:IHTMLControlElement;
begin
  Result := False;
  try
    Element2 := FDocument.body as IHTMLElement2;
    if not Assigned(Element2) then Exit;
    CtrlRange := Element2.createControlRange as IHTMLControlRange;
    ce:=Aimg as IHTMLControlElement;
    CtrlRange.add( ce );
    CtrlRange.execCommand( 'Copy', False, 0 );
    if Clipboard.HasFormat( CF_BITMAP ) then
    begin
      ABmp.FreeImage;
      ABmp.Assign( Clipboard );
      Result := True;
    end;
  except

  end;
end;

constructor TIECom.Create;
begin
  inherited;
  FDocument := nil;
end;

destructor TIECom.Destroy;
begin

  inherited;
end;

function TIECom.FindImage(const AImgSrcSign: string): IHTMLImgElement;
var
  i: Integer;
  ImgElem: IHTMLImgElement;
begin
  Result := nil;
  for i := 0 to FDocument.images.length - 1 do
  begin
    ImgElem := FDocument.images.item( i, 0 ) as IHTMLImgElement;
    if Pos(AImgSrcSign, ImgElem.src) > 0 then
    begin
      Result := ImgElem;
      Break;
    end;
  end;
end;


function TIECom.FindInputElem(const ANameOrID: string): IHTMLInputElement;
var
  i: Integer;
  IElem: IDispatch;
  InputElem: IHTMLInputElement;
begin
  Result := nil;
  if not CheckDocument then Exit;
  for i := FDocument.all.length - 1 downto 0 do
  begin
    IElem := FDocument.all.item(i, 0);
    if Succeeded( IElem.QueryInterface(IID_IHTMLInputElement, InputElem) ) then
    begin
      if ( CompareText(ANameOrId, InputElem.name) = 0 ) or ( CompareText(ANameOrId, (InputElem as IHTMLElement).id) = 0 ) then
      begin
        Result := InputElem;
        Break;
      end;
    end;
  end;
end;

function TIECom.FindInputElemByValue(const AValue: string): IHTMLInputElement;
var
  i: Integer;
  IElem: IDispatch;
  InputElem: IHTMLInputElement;
begin
  Result := nil;
  if not CheckDocument then Exit;
  for i := FDocument.all.length - 1 downto 0 do
  begin
    IElem := FDocument.all.item(i, 0);
    if Succeeded( IElem.QueryInterface(IID_IHTMLInputElement, InputElem) ) then
    begin
      if ( CompareText(AValue, InputElem.value) = 0 ) then
      begin
        Result := InputElem;
        Break;
      end;
    end;
  end;
end;

function TIECom.GetElemAttributeValue(const AID, AAttributeName: string; var AAttributeValue: string): Boolean;
var
  i: Integer;
  IElem: IDispatch;
  Elem: IHTMLElement;
begin
  Result := False;
  if not CheckDocument then Exit;
  for i := FDocument.all.length - 1 downto 0 do
  begin
    IElem := FDocument.all.item(i, 0);
    if Succeeded( IElem.QueryInterface(IID_IHTMLElement, Elem) ) then
    begin
      if CompareText(AID, Elem.id) = 0  then
      begin
        AAttributeValue := Elem.getAttribute( AAttributeName, 0 );
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TIECom.GetFrame(const AIndex: Integer; var AFrm: IHTMLWindow2): Boolean;
var
  Frms: IHTMLFramesCollection2;
  IFrm: IDispatch;
begin
  Result := False;
  if not CheckDocument then Exit;
  try
    Frms := FDocument.frames;
    IFrm := Frms.item( AIndex );
    Result := Succeeded( IFrm.QueryInterface(IID_IHTMLWindow2, AFrm) );
  except
  end;
end;

function TIECom.GetImageSrcBySign(const AImgSrcSign: string; var AStrUrl: string): Boolean;
var
  img: IHTMLImgElement;
begin
  Result := False;
  img := FindImage( AImgSrcSign );
  if Img = nil then Exit;
  AStrUrl := img.src;
  Result := True;
end;

function TIECom.GetInputValue(const ANameOrId: string; var AValue: string): Boolean;
var
  i: Integer;
  IElem: IDispatch;
  InputElem: IHTMLInputElement;
begin
  Result := False;
  if not CheckDocument then Exit;
  for i := FDocument.all.length - 1 downto 0 do
  begin
    IElem := FDocument.all.item(i, 0);
    if Succeeded( IElem.QueryInterface(IID_IHTMLInputElement, InputElem) ) then
    begin
      if ( CompareText(ANameOrId, InputElem.name) = 0 ) or ( CompareText(ANameOrId, (InputElem as IHTMLElement).id) = 0 ) then
      begin
        AValue := InputElem.value;
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TIECom.GetLinkElem(const ALinkSign: string; var AElem: IHTMLElement): Boolean;
var
  Links: IHTMLElementCollection;
  IElem: IDispatch;
  Elem: IHTMLElement;
  i: Integer;
  strSign: string;
begin
  Result := False;
  if not CheckDocument then Exit;
  strSign := LowerCase( ALinkSign );
  Links := FDocument.links;
  for i := 0 to Links.length - 1 do
  begin
    IElem := Links.item( i, 0 );
    if Succeeded( IElem.QueryInterface( IID_IHTMLElement, Elem) ) then
    begin
      if Pos( strSign, LowerCase(Elem.getAttribute('href', 0)) ) > 0 then
      begin
        AElem := Elem; 
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TIECom.InputClick(const ANameOrId: string): Boolean;
var
  InputElem: IHTMLInputElement;
begin
  Result := False;
  InputElem := FindInputElem( ANameOrId );
  if InputElem <> nil then
  begin
    (InputElem as IHTMLElement).click;
    Result := True;
  end;
end;

function TIECom.InputClickByValue(const AValue: string): Boolean;
var
  InputElem: IHTMLInputElement;
begin
  Result := False;
  InputElem := FindInputElemByValue( AValue );
  if InputElem <> nil then
  begin
    (InputElem as IHTMLElement).click;
    Result := True;
  end;
end;

function TIECom.SetElemAttributeValue(const AID, AAttributeName, AAttributeValue: string): Boolean;
var
  i: Integer;
  IElem: IDispatch;
  Elem: IHTMLElement;
begin
  Result := False;
  if not CheckDocument then Exit;
  for i := FDocument.all.length - 1 downto 0 do
  begin
    IElem := FDocument.all.item(i, 0);
    if Succeeded( IElem.QueryInterface(IID_IHTMLElement, Elem) ) then
    begin
      if CompareText(AID, Elem.id) = 0  then
      begin
        Elem.setAttribute( AAttributeName, AAttributeValue, 0 );
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TIECom.SetInputValue(const ANameOrId, AValue: string): Boolean;
var
  i: Integer;
  IElem: IDispatch;
  InputElem: IHTMLInputElement;
begin
  Result := False;
  if not CheckDocument then Exit;
  for i := FDocument.all.length - 1 downto 0 do
  begin
    IElem := FDocument.all.item(i, 0);
    if Succeeded( IElem.QueryInterface(IID_IHTMLInputElement, InputElem) ) then
    begin
      if ( CompareText(ANameOrId, InputElem.name) = 0 ) or ( CompareText(ANameOrId, (InputElem as IHTMLElement).id) = 0 ) then
      begin
        InputElem.value := AValue;
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TIECom.FindImage(const AImgIndex: Integer): IHTMLImgElement;
begin
  Result := FDocument.images.item( AImgIndex, 0 ) as IHTMLImgElement;
end;

initialization
  CoInitialize( nil );
finalization
  CoUninitialize;
end.
