{
单元名称: uDataStructure
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: 数据结构操作
开始时间: 2009-2-9
修改时间: 2009-2-9 (最后修改)
注意事项: 非线程安全
主要类  :
}

unit uDataStructure;

interface

uses
  Windows, uQueues;

type
  pNode = ^TNode;
  TNode = record
    FID: Cardinal;
    FData: Pointer;
    FNext: pNode;
  end;
const CtNodeSize = SizeOf(TNode);

type
  TLoopNotify = function(Sender: TObject; p: Pointer; var AIsDel: Boolean): Boolean of object;
  TLoopForUpdate = procedure(Sender: TObject; pData, pInfo: Pointer) of object;
  THashArray = class
  private
    FActive: Boolean;
    FHashTable: array of pNode;
    FHashTableCount: Cardinal;
    FHashNodeMem: TStaticMemoryManager;
    FMaxHashNodeCount: Cardinal;
    FCount: Cardinal;
    FTag: Cardinal;

    procedure ActiveHashArray;
    procedure UnActiveHashArray;
    //1: 正常找到; 2: 没有找到; 3: 添加新节点
    function _Find(const AID: Cardinal; const AForceNew: Boolean; const AIsDel: Boolean; var AFindNode: pNode; var AParentNode: pNode): Word;

    procedure SetHashTableCount(const Value: Cardinal);
    procedure SetMaxHashNodeCount(const Value: Cardinal);
    procedure SetActive(const Value: Boolean);
  protected

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetAllID(var AIDs: array of Cardinal; AMaxCount: Cardinal = 0): Cardinal;
    //AForceAdd: 如果节点已存在, 是否更新
    function Add(const AID: Cardinal; const AData: Pointer; const AForceAdd: Boolean = False): Boolean;
    function Find(const AID: Cardinal; const AIsDel: Boolean = False): Pointer;
    function Delete(const AID: Cardinal): Boolean;

    procedure Loop(ALoopNotify: TLoopNotify); overload;
    procedure Loop(ALoopForUpdate: TLoopForUpdate; pInfo: Pointer); overload;

    property Active: Boolean read FActive write SetActive;
    property Count: Cardinal read FCount;
    property HashTableCount: Cardinal read FHashTableCount write SetHashTableCount;
    property MaxHashNodeCount: Cardinal read FMaxHashNodeCount write SetMaxHashNodeCount;
    property Tag: Cardinal read FTag write FTag;
  end;

  { ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    序号管理类                               TIndexManage
    序号管理. 0 ~ 127

  } ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TByteIndexManage = class
  private
    FByteList: array of Byte;
    FHead: Byte;
    FTail: Byte;
    FCount: Byte;
    FCapacity: Byte;
    FLock: TRTLCriticalSection;
    function  IsCanReclaim(const AIndex: Byte): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function  GetIndex(var AIndex: Byte): Boolean;
    procedure ReclaimIndex(const AIndex: Byte);
    property  Count: Byte read FCount;
  end;

  { ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    序号管理类                               TIndexManage
    序号管理. 0 ~ MAXDOWRD

  } ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  TDwordIndexManage = class
  private
    FCardinalList: array of Cardinal;
    FHead: Cardinal;
    FTail: Cardinal;
    FCount: Cardinal;
    FCapacity: Cardinal;
    FLock: TRTLCriticalSection;
    function  IsCanReclaim(const AIndex: Cardinal): Boolean;
  public
    constructor Create(const AMaxCount: Cardinal; const ABeginPos: Cardinal = 1000);
    destructor Destroy; override;
    function  GetIndex(var AIndex: Cardinal): Boolean;
    procedure ReclaimIndex(const AIndex: Cardinal);
    property  Count: Cardinal read FCount;
  end;
implementation

{ TIndexManage }

constructor TByteIndexManage.Create;
var
  i: Byte;
begin
  FCapacity := 128;
  SetLength( FByteList, FCapacity );
  FHead := 0;
  FTail := 0;
  InitializeCriticalSection( FLock );
  for i := 1 to FCapacity do
    FByteList[i - 1] := i;
  FCount := FCapacity;
end;

destructor TByteIndexManage.Destroy;
begin
  SetLength( FByteList, 0 );
  DeleteCriticalSection( FLock );
  inherited;
end;

function TByteIndexManage.GetIndex(var AIndex: Byte): Boolean;
begin
  Result := False;
  EnterCriticalSection( FLock );
  try
    if FCount = 0 then Exit;
    AIndex := FByteList[ FHead ];
    FHead := ( FHead + 1 ) mod FCapacity;
    Dec( FCount );
    Result := True;
  finally
    LeaveCriticalSection( FLock );
  end;
end;

function TByteIndexManage.IsCanReclaim(const AIndex: Byte): Boolean;
var
  i: Byte;
begin
  Result := False;
  if FCount = FCapacity then Exit;
  i := FTail;
  while i <> FHead do
  begin
    if FByteList[i] = AIndex then
    begin
      Result := True;
      Break;
    end;
    i := (i + 1) mod FCapacity;
  end;
end;

procedure TByteIndexManage.ReclaimIndex(const AIndex: Byte);
begin
  EnterCriticalSection(FLock);
  try
    if FCount >= FCapacity then Exit;
    if IsCanReclaim(AIndex) then
    begin
      FByteList[FTail] := AIndex;
      FTail := (FTail + 1) mod FCapacity;
      Inc(FCount);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;


{ THashArray }

procedure THashArray.ActiveHashArray;
begin
  SetLength( FHashTable, FHashTableCount );
  FHashNodeMem := TStaticMemoryManager.Create( CtNodeSize, FMaxHashNodeCount );
  FCount := 0;
end;

function THashArray.Add(const AID: Cardinal; const AData: Pointer; const AForceAdd: Boolean): Boolean;
var
  pFindNode, pParentNode: pNode;
  nState: Word;
begin
  nState := _Find( AID, True, False, pFindNode, pParentNode );  //1: 正常找到; 2: 没有找到; 3: 添加新节点
  Result := nState in [1, 3];
  if nState = 1 then //正常找到
  begin
    if AForceAdd then
      pFindNode^.FData := AData;
  end
  else if nState = 3 then
    pFindNode^.FData := AData;
end;

constructor THashArray.Create;
begin
  FActive := False;
  FHashTableCount := 8 * 1024;
  FMaxHashNodeCount := 16 * 1024;
end;

destructor THashArray.Destroy;
begin
  Active := False;
  inherited;
end;

function THashArray.Find(const AID: Cardinal; const AIsDel: Boolean): Pointer;
var
  pFindNode, pParentNode: pNode;
begin
  Result := nil;
  pFindNode := nil;
  pParentNode := nil;
  _Find( AID, False, AIsDel, pFindNode, pParentNode );
  if pFindNode <> nil then
    Result := pFindNode^.FData;
  if AIsDel and (pFindNode <> nil) then
  begin
    FillChar( pFindNode^, CtNodeSize, 0 );
    FHashNodeMem.FreeMem( pFindNode );
  end;
end;

function THashArray.GetAllID(var AIDs: array of Cardinal; AMaxCount: Cardinal): Cardinal;
var
  i: Cardinal;
  p: pNode;
begin
  Result := 0;
  if not Active then Exit;
  if FCount = 0 then Exit;

  if AMaxCount = 0 then
    AMaxCount := FCount;
  for i := 0 to FHashTableCount - 1 do
  begin
    p := FHashTable[i];
    while p <> nil do
    begin
      AIDs[Result] := p^.FID;
      Inc( Result );
      Dec( AMaxCount );
      if AMaxCount = 0 then Exit;
      p := p^.FNext;
    end;
  end;
end;

procedure THashArray.Loop(ALoopNotify: TLoopNotify);
var
  i: Cardinal;
  p, pParent: pNode;
  IsDel: Boolean;
  nCount: Cardinal;
begin
  if not Active then Exit;
  if FCount = 0 then Exit;

  nCount := FCount;
  for i := 0 to FHashTableCount - 1 do
  begin
    if nCount <= 0 then Break;
    p := FHashTable[i];
    pParent := nil;
    while (p <> nil) and (nCount > 0) do
    begin
      Dec( nCount );
      IsDel := False;
      if not ALoopNotify( Self, p^.FData, IsDel ) then Exit;
      if IsDel then
      begin
        if nil = pParent then
          FHashTable[i] := p^.FNext
        else
          pParent.FNext := p^.FNext;
        FillChar( p^, CtNodeSize, 0 );
        FHashNodeMem.FreeMem( p );
        Dec( FCount );
        if FCount = 0 then Exit;
        if pParent <> nil then
          p := pParent.FNext
        else
          p := FHashTable[i];
      end
      else
      begin
        pParent := p;
        p := p^.FNext;
      end;
    end;
  end;
end;

procedure THashArray.Loop(ALoopForUpdate: TLoopForUpdate; pInfo: Pointer);
var
  i: Cardinal;
  p: pNode;
begin
  if not Active then Exit;
  if FCount = 0 then Exit;
  for i := 0 to FHashTableCount - 1 do
  begin
    p := FHashTable[i];
    while p <> nil do
    begin
      ALoopForUpdate( Self, p^.FData, pInfo );
      if FCount = 0 then Exit;
      p := p^.FNext;
    end;
  end;
end;

procedure THashArray.SetActive(const Value: Boolean);
begin
  if (FActive <> Value) then
  begin
    if Value then
      ActiveHashArray
    else
      UnActiveHashArray;
    FActive := Value;
  end;
end;

procedure THashArray.SetHashTableCount(const Value: Cardinal);
begin
  if (not Active) and (FHashTableCount <> Value) then
    FHashTableCount := Value;
end;

procedure THashArray.SetMaxHashNodeCount(const Value: Cardinal);
begin
  if (not Active) and (Value <> FMaxHashNodeCount) then
    FMaxHashNodeCount := Value;
end;

procedure THashArray.UnActiveHashArray;
begin
  SetLength( FHashTable, 0 );
  FHashNodeMem.Free;
  FCount := 0;
end;

function THashArray.Delete(const AID: Cardinal): Boolean;
var
  pFindNode, pParentNode: pNode;
begin
  pFindNode := nil;
  pParentNode := nil;
  Result := _Find( AID, False, True, pFindNode, pParentNode ) = 1;
  if Result then
  begin
    FillChar( pFindNode^, CtNodeSize, 0 );
    FHashNodeMem.FreeMem( pFindNode );
  end;
end;

function THashArray._Find(const AID: Cardinal; const AForceNew: Boolean; const AIsDel: Boolean; var AFindNode: pNode; var AParentNode: pNode): Word;
var
  nIndex: Cardinal;
  p, pParent: pNode;
  bFind: Boolean;
begin
//1: 正常找到; 2: 没有找到; 3: 添加新节点
  Result := 2;
  AFindNode := nil;
  nIndex := AID mod (FHashTableCount - 1);
  if nIndex >= FHashTableCount then Exit;

  p := FHashTable[nIndex];
  bFind := False;
  pParent :=  nil;
  while p <> nil do
  begin
    if p^.FID = AID then
    begin
      bFind := True;
      Break;
    end;
    pParent := p;
    p := p^.FNext;
  end;
  if bFind then
  begin
    AFindNode := p;
    AParentNode := pParent;
    if AIsDel then
    begin
      if AParentNode = nil then
        FHashTable[nIndex] := nil
      else
        AParentNode.FNext := p^.FNext;
      Dec( FCount );
    end;
    Result := 1;
  end
  else if AForceNew and FHashNodeMem.GetMem(Pointer(p)) then
  begin
    Inc( FCount );
    Result := 3;
    AParentNode := pParent;
    p^.FID := AID;
    if AParentNode = nil then
      FHashTable[nIndex] := p
    else
      AParentNode.FNext := p;
    AFindNode := p;
  end;
end;

{ TDwordIndexManage }

constructor TDwordIndexManage.Create(const AMaxCount: Cardinal; const ABeginPos: Cardinal);
var
  i: Cardinal;
begin
  FCapacity := AMaxCount;
  SetLength( FCardinalList, FCapacity );
  FHead := 0;
  FTail := 0;
  InitializeCriticalSection( FLock );
  for i := 0 to FCapacity - 1 do
    FCardinalList[i] := ABeginPos + i;
  FCount := FCapacity;
end;

destructor TDwordIndexManage.Destroy;
begin
  SetLength( FCardinalList, 0 );
  DeleteCriticalSection( FLock );
  inherited;
end;

function TDwordIndexManage.GetIndex(var AIndex: Cardinal): Boolean;
begin
  Result := False;
  EnterCriticalSection( FLock );
  try
    if FCount = 0 then Exit;
    AIndex := FCardinalList[ FHead ];
    FHead := ( FHead + 1 ) mod FCapacity;
    Dec( FCount );
    Result := True;
  finally
    LeaveCriticalSection( FLock );
  end;
end;

function TDwordIndexManage.IsCanReclaim(const AIndex: Cardinal): Boolean;
var
  i: Cardinal;
begin
  Result := False;
  if FCount = FCapacity then Exit;
  i := FTail;
  while i <> FHead do
  begin
    if FCardinalList[i] = AIndex then
    begin
      Result := True;
      Break;
    end;
    i := (i + 1) mod FCapacity;
  end;
end;

procedure TDwordIndexManage.ReclaimIndex(const AIndex: Cardinal);
begin
  EnterCriticalSection(FLock);
  try
    if FCount >= FCapacity then Exit;
    if IsCanReclaim(AIndex) then
    begin
      FCardinalList[FTail] := AIndex;
      FTail := (FTail + 1) mod FCapacity;
      Inc(FCount);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

end.
