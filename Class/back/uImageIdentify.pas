{ TBitmapIdentify }

 {简单图像识别功能类}
 /////////By Terry////////////////
 /////////2007-10-9///////////////
  
unit uImageIdentify;

interface
uses Windows, Classes, Graphics, SysUtils;

//0: NULL; left: 1; middle: 2; right: 3
//链表方向: 从左向右
  type
  TAryByte = array of Byte;
  TIdentify = packed record
    FSign:             string; 
    FCount:            Integer;
    FAryLeftIdentify:  TAryByte;
    FAryRightIdentify: TAryByte;
  end;

  TSearchWay = (swLeft, swRight); //查找方向

  PBitmapIdentifyRow = ^TBitmapIdentifyRow;
  TBitmapIdentifyRow = packed record
    FSign:       Byte;
    FPoint:      TPoint;
    FpNextNode:  PBitmapIdentifyRow;
  end;

  PBitmapIdentifyHead = ^TBitmapIdentifyHead;
  TBitmapIdentifyHead = packed record
    FLeftXPos:       Integer;
    FRightXPos:      Integer;
    FYPos:           Integer;
    FLeftCount:      Integer;
    FRightCount:     Integer;
    FCount:          Integer;    //当FCount > 1 时, 方需要用到 FRightIdentify
    FIdentifyRect:   TRect;
    FpLeftIdentify:  PBitmapIdentifyRow;
    FpRightIdentify: PBitmapIdentifyRow;
  end;
  
  TBitmapIdentify = class(TObject)
  private
    FBitmapIdentify: TBitmapIdentifyHead;
    //ScanBitmap operation
    procedure GetScanBitmapHeadMsg(const ABmp: TBitmap; ASearchWay: TSearchWay; const AXBeginPos, AYBeginPos: Integer);
    function  ScanBitmapRowFromLeft(const ABmp: TBitmap; const AXpos, AYpos: Integer; var pIdentifyRow: PBitmapIdentifyRow): Boolean;
    function  ScanBitmapRowFromRight(const ABmp: TBitmap; const AXpos, AYpos: Integer; var pIdentifyRow: PBitmapIdentifyRow): Boolean;
    procedure ScanIdentifyRect(const ABmp: TBitmap; const APoint: TPoint);
    function  IsPixelBlack(const ABmp: TBitmap; const APoint: TPoint): Boolean; overload;
    function  IsPixelBlack(const ABmp: TBitmap; const AxPos, AyPos: Integer): Boolean; overload;
    //List operation
    function BuildIdentifyRow: PBitmapIdentifyRow;
    procedure InitIdentifyHead;
    procedure FreeAllListNode;
    procedure FreeNullNode;
    function GetIdentifyRect: TRect;
    function GetScanSign: Boolean;
  public
    function AnalyseBitmap(const ABmp: TBitmap; ASearchWay: TSearchWay; const AXBeginPos, AYBeginPos: Integer): TIdentify;
    class procedure OutputMsg(const AFileName: string; const AIdentify: TIdentify); overload;
    class function  OutputMsg(const AIdentify: TIdentify): string; overload;

    property IdentifyRect: TRect read GetIdentifyRect;
    property ScanIsSuccess: Boolean read GetScanSign;
  public
    constructor Create;
    destructor Destroy; override;
  end;

const
  CNullIdentify    = 0;
  CLeftIdentify    = 1;
  CMiddleIdentify  = 2;
  CRightIdentify   = 3;
  
implementation

{ TBitmapIdentify }

function TBitmapIdentify.AnalyseBitmap(const ABmp: TBitmap; ASearchWay: TSearchWay; const AXBeginPos, AYBeginPos: Integer): TIdentify;
var
  P: PBitmapIdentifyRow;
  X, Y: Integer;
///////
  procedure ScanIdentifyLeft;
  begin
    FBitmapIdentify.FpLeftIdentify := BuildIdentifyRow;
    Inc(FBitmapIdentify.FLeftCount);
    P := FBitmapIdentify.FpLeftIdentify;
    X := FBitmapIdentify.FLeftXPos;
    Y := FBitmapIdentify.FYPos;
    while ScanBitmapRowFromLeft(ABmp, X, Y, P) do
    begin
      X := P^.FPoint.X;
      Y := P^.FPoint.Y;
      P^.FpNextNode := BuildIdentifyRow;
      P := P^.FpNextNode;
      Inc(FBitmapIdentify.FLeftCount);
    end;
  end;

  procedure ScanIdentifyRight;
  begin
    FBitmapIdentify.FpRightIdentify := BuildIdentifyRow;
    Inc(FBitmapIdentify.FRightCount);
    P := FBitmapIdentify.FpRightIdentify;
    X := FBitmapIdentify.FRightXPos;
    Y := FBitmapIdentify.FYPos;
    while ScanBitmapRowFromRight(ABmp, X, Y, P) do
    begin
      X := P^.FPoint.X;
      Y := P^.FPoint.Y;
      p^.FpNextNode := BuildIdentifyRow;
      P := p^.FpNextNode;
      Inc(FBitmapIdentify.FRightCount);
    end;
  end;

  procedure ScanRect(pHead: PBitmapIdentifyRow);
  begin
    P := pHead;
    while Assigned(P) do
    begin
      ScanIdentifyRect(ABmp, P^.FPoint);
      P := P^.FpNextNode;
    end;
  end;
begin
  InitIdentifyHead;
  GetScanBitmapHeadMsg(ABmp, ASearchWay, AXBeginPos, AYBeginPos);
  if FBitmapIdentify.FCount = 0 then
  begin
    Result.FCount := 0;
    Exit;
  end
  else if FBitmapIdentify.FCount = 1 then
  begin
    ScanIdentifyLeft;
    FreeNullNode;
    ScanRect(FBitmapIdentify.FpLeftIdentify);
  end
  else
  begin
    ScanIdentifyLeft;
    ScanIdentifyRight;
    FreeNullNode;
    ScanRect(FBitmapIdentify.FpLeftIdentify);
    ScanRect(FBitmapIdentify.FpRightIdentify);
  end;

  //Build Result
  Result.FCount := FBitmapIdentify.FCount;
  SetLength(Result.FAryLeftIdentify, FBitmapIdentify.FLeftCount - 1);
  P := FBitmapIdentify.FpLeftIdentify;
  X := 0;
  while (Assigned(P)) and (X < Length(Result.FAryLeftIdentify)) do
  begin
    Result.FAryLeftIdentify[X] := P^.FSign;
    P := P^.FpNextNode;
    Inc(X);
  end;
  
  if (Result.FCount > 1) and (FBitmapIdentify.FRightCount > 1) then
  begin
    SetLength(Result.FAryRightIdentify, FBitmapIdentify.FRightCount - 1);
    P := FBitmapIdentify.FpRightIdentify;
    X := 0;
    while Assigned(P) do
    begin
      Result.FAryRightIdentify[X] := P^.FSign;
      P := P^.FpNextNode;
      Inc(X);
    end;
  end;
end;

function TBitmapIdentify.BuildIdentifyRow: PBitmapIdentifyRow;
begin
  New(Result);
  Result^.FSign := CNullIdentify;
  Result^.FPoint := Point(0, 0);
  Result^.FpNextNode := nil;
end;

constructor TBitmapIdentify.Create;
begin
  InitIdentifyHead;
end;

destructor TBitmapIdentify.Destroy;
begin
  FreeAllListNode;
  inherited;
end;

procedure TBitmapIdentify.FreeAllListNode;
  procedure FreePoint(p: PBitmapIdentifyRow);
  var
    pCur: PBitmapIdentifyRow;
    pNext: PBitmapIdentifyRow;
  begin
    if Assigned(p) then
    begin
      pCur := P;
      while Assigned(pCur) do
      begin
        pNext := pCur.FpNextNode;
        Dispose(pCur);
        pCur := pNext;
      end;
    end;
  end;
begin
  FreePoint(FBitmapIdentify.FpLeftIdentify);
  FreePoint(FBitmapIdentify.FpRightIdentify);
end;

procedure TBitmapIdentify.FreeNullNode;
  procedure FreePoint(pHead: PBitmapIdentifyRow);
  var
    pCur: PBitmapIdentifyRow;
    pNext, pPr: PBitmapIdentifyRow;
  begin
    if Assigned(pHead) then
    begin
      pCur := pHead;
      pPr := pHead;
      while Assigned(pCur) do
      begin
        pNext := pCur^.FpNextNode;
        if pCur^.FSign = CNullIdentify then
        begin
          if pCur <> pHead then
          begin
            Dispose(pCur);
            pPr^.FpNextNode := pNext;
          end;
        end;
        pPr := pCur;
        pCur := pNext;
      end;
    end;
  end;
begin
  FreePoint(FBitmapIdentify.FpLeftIdentify);
  FreePoint(FBitmapIdentify.FpRightIdentify);
end;

function TBitmapIdentify.GetIdentifyRect: TRect;
begin
  Result := FBitmapIdentify.FIdentifyRect;
end;

procedure TBitmapIdentify.GetScanBitmapHeadMsg(const ABmp: TBitmap; ASearchWay: TSearchWay; const AXBeginPos,
  AYBeginPos: Integer);
var
  iXLoop, iYLoop: Integer;
  nWidth, nHeight: Integer;
  bBreak: Boolean;
begin
  nWidth := ABmp.Width;
  nHeight := ABmp.Height;
  bBreak := False;
  if ASearchWay = swLeft then     //从左向右扫描
  begin
    for iYLoop := AYBeginPos to nHeight - 1 do
    begin
      for iXLoop := AXBeginPos to nWidth - 1 do
      begin
        if IsPixelBlack(ABmp, iXLoop, iYLoop) then
        begin
          FBitmapIdentify.FLeftXPos := iXLoop;
          FBitmapIdentify.FRightXPos := iXLoop;
          FBitmapIdentify.FYPos := iYLoop;
          nWidth := iXLoop + 1;
          while IsPixelBlack(ABmp, nWidth, iYLoop) do
          begin
            FBitmapIdentify.FRightXPos := nWidth;
            inc(nWidth);
          end;
          FBitmapIdentify.FCount := FBitmapIdentify.FRightXPos - FBitmapIdentify.FLeftXPos + 1;
          bBreak := True;
          Break;
        end;
      end;
      if bBreak then
        Break;
    end;
  end
  else if ASearchWay = swRight then //从右向左扫描
  begin
    for iYLoop := AYBeginPos to nHeight - 1 do
    begin
      for iXLoop := AXBeginPos downto 0 do
      begin
        if IsPixelBlack(ABmp, iXLoop, iYLoop) then
        begin
          FBitmapIdentify.FLeftXPos := iXLoop;
          FBitmapIdentify.FRightXPos := iXLoop;
          FBitmapIdentify.FYPos := iYLoop;
          nWidth := iXLoop - 1;
          while IsPixelBlack(ABmp, nWidth, iYLoop) do
          begin
            FBitmapIdentify.FLeftXPos := nWidth;
            dec(nWidth);
          end;
          FBitmapIdentify.FCount := FBitmapIdentify.FRightXPos - FBitmapIdentify.FLeftXPos + 1;
          bBreak := True;
          Break;
        end;
      end;
      if bBreak then
        Break;
    end;
  end;

  FBitmapIdentify.FIdentifyRect := Rect(FBitmapIdentify.FLeftXPos, FBitmapIdentify.FYPos,
    FBitmapIdentify.FLeftXPos + FBitmapIdentify.FCount, FBitmapIdentify.FYPos + 1);
end;

function TBitmapIdentify.GetScanSign: Boolean;
begin
  Result := FBitmapIdentify.FCount > 0;
end;

procedure TBitmapIdentify.InitIdentifyHead;
begin
  FreeAllListNode;
  with FBitmapIdentify do
  begin
    FLeftXPos := 0;
    FRightXPos := 0;
    FYPos := 0;
    FCount := 0;
    FLeftCount := 0;
    FRightCount := 0;
    FIdentifyRect := Rect(0, 0, 0, 0);
    FpLeftIdentify := nil;
    FpRightIdentify := nil;
  end;
end;

function TBitmapIdentify.IsPixelBlack(const ABmp: TBitmap; const AxPos, AyPos: Integer): Boolean;
begin
  if ABmp.Canvas.Pixels[AxPos, AyPos] = clBlack then
    Result := True
  else
    Result := False;
end;

class function TBitmapIdentify.OutputMsg(const AIdentify: TIdentify): string;
var
  strText: string;
  iLoop: Integer;
begin
  strText := Format('{%s} [%d] (', [AIdentify.FSign, AIdentify.FCount]);
  for iLoop := Low(AIdentify.FAryLeftIdentify) to High(AIdentify.FAryLeftIdentify) do
    strText := Format('%s%d,', [strText, AIdentify.FAryLeftIdentify[iLoop]]);
  strText[Length(strText)] := ')';
  
  if AIdentify.FCount > 1 then
  begin
    strText := strText + '('; 
    for iLoop := Low(AIdentify.FAryRightIdentify) to High(AIdentify.FAryRightIdentify) do
      strText := Format('%s%d,', [strText, AIdentify.FAryRightIdentify[iLoop]]);
    strText[Length(strText)] := ')';
  end;
  Result := strText;
end;

function TBitmapIdentify.IsPixelBlack(const ABmp: TBitmap; const APoint: TPoint): Boolean;
begin
  Result := IsPixelBlack(ABmp, APoint.X, Apoint.Y);
end;

class procedure TBitmapIdentify.OutputMsg(const AFileName: string; const AIdentify: TIdentify);
var
  IdentifyFile: TStringList;
begin
  IdentifyFile := TStringList.Create;
  try
    IdentifyFile.Text := OutputMsg(AIdentify);
    IdentifyFile.SaveToFile(AFileName);
  finally
    IdentifyFile.Free;
  end;
end;

function TBitmapIdentify.ScanBitmapRowFromLeft(const ABmp: TBitmap; const AXpos, AYpos: Integer;
  var pIdentifyRow: PBitmapIdentifyRow): Boolean;
begin
  Result := True;
  if IsPixelBlack(ABmp, AXpos - 1, AYpos + 1) then
  begin
    pIdentifyRow^.FSign := CLeftIdentify;
    pIdentifyRow^.FPoint := Point(AXpos - 1, AYpos + 1);
  end
  else if IsPixelBlack(ABmp, AXpos, AYpos + 1) then
  begin
    pIdentifyRow^.FSign := CMiddleIdentify;
    pIdentifyRow^.FPoint := Point(AXpos, AYpos + 1);
  end
  else if IsPixelBlack(ABmp, AXpos + 1, AYpos + 1) then
  begin
    pIdentifyRow^.FSign := CRightIdentify;
    pIdentifyRow^.FPoint := Point(AXpos + 1, AYpos + 1);
  end
  else
  begin
    pIdentifyRow^.FSign := CNullIdentify;
    Result := False;
  end;
end;

function TBitmapIdentify.ScanBitmapRowFromRight(const ABmp: TBitmap; const AXpos, AYpos: Integer;
  var pIdentifyRow: PBitmapIdentifyRow): Boolean;
begin
  Result := True;
  if IsPixelBlack(ABmp, AXpos + 1, AYpos + 1) then
  begin
    pIdentifyRow^.FSign := CRightIdentify;
    pIdentifyRow^.FPoint := Point(AXpos + 1, AYpos + 1);
  end
  else if IsPixelBlack(ABmp, AXpos, AYpos + 1) then
  begin
    pIdentifyRow^.FSign := CMiddleIdentify;
    pIdentifyRow^.FPoint := Point(AXpos, AYpos + 1);
  end
  else if IsPixelBlack(ABmp, AXpos - 1, AYpos + 1) then
  begin
    pIdentifyRow^.FSign := CLeftIdentify;
    pIdentifyRow^.FPoint := Point(AXpos - 1, AYpos + 1);
  end
  else
  begin
    pIdentifyRow^.FSign := CNullIdentify;
    Result := False;
  end;
end;

procedure TBitmapIdentify.ScanIdentifyRect(const ABmp: TBitmap; const APoint: TPoint);
var
  nLeft, nRight: Integer;
begin
  //扫描左边
  nLeft := APoint.X - 1;
  while True do
  begin
    if IsPixelBlack(ABmp, nLeft, Apoint.Y) then
      Dec(nLeft)
    else
    begin
      inc(nLeft);
      Break;
    end;
  end;
  //扫描右边
  nRight := APoint.X + 1;
  while True do
  begin
    if IsPixelBlack(ABmp, nRight, Apoint.Y) then    
      Inc(nRight)
    else
    begin
      Dec(nRight);
      Break;
    end;
  end;
  //更新RECT
  if FBitmapIdentify.FIdentifyRect.Left > nLeft then
    FBitmapIdentify.FIdentifyRect.Left := nLeft;
  if FBitmapIdentify.FIdentifyRect.Right < nRight then
    FBitmapIdentify.FIdentifyRect.Right := nRight;
  if FBitmapIdentify.FIdentifyRect.Bottom < APoint.Y then
    FBitmapIdentify.FIdentifyRect.Bottom := APoint.Y;
end;

end.
