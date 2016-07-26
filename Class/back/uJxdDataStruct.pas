{
单元名称: uJxdDataStruct
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: 数据结构操作
开始时间: 2009-02-09
修改时间: 2010-09-15 (最后修改)
注意事项: 非线程安全
主要类  :  THashArray

    THashArray
      key1 --> item1 --> item2 --> ...
      key2 --> item1 --> item2 --> ...
       .
       .
       .
UniquelyID： ID是否是唯一的，如果为TRUE，链表中的所有元素ID都是不相同的。

}
unit uJxdDataStruct;

interface
uses
  Windows, SysUtils, uJxdMemoryManage;

type
  pHashNode = ^THashNode;
  THashNode = record
    FID: Cardinal;
    FData: Pointer;
    FNext: pHashNode;
  end;

  TOnFindNode = procedure(Sender: TObject; const AParam, pData: Pointer; var ADel: Boolean; var AFindNext: Boolean) of object;
  TOnLoopNode = procedure(Sender: TObject; const AID: Cardinal; pData: Pointer; var ADel: Boolean; var AFindNext: Boolean) of object;
  {$M+}
  THashArray = class
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    {Add: 添加节点}
    function  Add(const AID: Cardinal; const AData: Pointer): Boolean;
    //UniquelyID = False时；应当调用此Find函数，它回调所有ID为设置值的所有节点;
    //Result表示：调用回调函数的次数; -1说明没有找到指定ID的节点
    function  Find(const AID: Cardinal; const AFindCallBack: TOnFindNode; const AParamPointer: Pointer): Integer; overload;
    //UniquelyID = Ture时；调用以下Find函数即可。否则只返回第一个ID为设置的节点
    function  Find(const AID: Cardinal; var Ap: Pointer; const AIsDel: Boolean): Boolean; overload;
    procedure Loop(const ALoopCallBack: TOnLoopNode);
  private
    FActive: Boolean;
    FHashTable: array of pHashNode;
    FHashTableCount: Cardinal;
    FHashNodeMem: TxdFixedMemoryManager;
    FMaxHashNodeCount: Cardinal;
    FCount: Cardinal;
    FTag: Cardinal;
    FUniquelyID: Boolean;
    FLastErrorCode: Integer;
    FLastErrorText: string;

    procedure ActiveHashArray;
    procedure UnActiveHashArray;
    function  IDToIndex(const AID: Cardinal): Integer;
    procedure DoNoActiveError;
    procedure DoNoGetMem;
    procedure DoExsitsIDError;
    procedure DoNotFindNode;
    
    procedure SetHashTableCount(const Value: Cardinal);
    procedure SetMaxHashNodeCount(const Value: Cardinal);
    procedure SetActive(const Value: Boolean);
    procedure SetAllowClash(const Value: Boolean);

    const CtNodeSize = SizeOf(THashNode);
  published
    property Active: Boolean read FActive write SetActive;
    property UniquelyID: Boolean read FUniquelyID write SetAllowClash;  //ID是否唯一
    property Count: Cardinal read FCount;
    property HashTableCount: Cardinal read FHashTableCount write SetHashTableCount;
    property MaxHashNodeCount: Cardinal read FMaxHashNodeCount write SetMaxHashNodeCount;
    property Tag: Cardinal read FTag write FTag;
    property LastErrorCode: Integer read FLastErrorCode;
    property LastErrorText: string read FLastErrorText;
  end;
  {$M-}

implementation

{ THashArray }

procedure THashArray.ActiveHashArray;
begin
  try
    SetLength( FHashTable, FHashTableCount );
    FHashNodeMem := TxdFixedMemoryManager.Create( CtNodeSize, FMaxHashNodeCount );
    FCount := 0;
    FLastErrorCode := 0;
    FLastErrorText := '';
    FActive := True;
  except
    if FHashTable <> nil then
    begin
      SetLength( FHashTable, 0 );
      FHashTable := nil;
    end;
    if FHashNodeMem <> nil then
      FreeAndNil( FHashNodeMem );
  end;
end;

function THashArray.Add(const AID: Cardinal; const AData: Pointer): Boolean;
var
  pNode, pClashNode: pHashNode;
  nIndex: Integer;
begin
  Result := False;
  if not Active then
  begin
    DoNoActiveError;
    Exit;
  end;
  nIndex := IDToIndex( AID );
  pNode := FHashTable[ nIndex ];
  if pNode = nil then
  begin
    if not FHashNodeMem.GetMem( Pointer(pNode) ) then
    begin
      DoNoGetMem;
      Exit;
    end;
    pNode^.FID := AID;
    pNode^.FData := AData;
    pNode^.FNext := nil;
    FHashTable[ nIndex ] := pNode;
  end
  else
  begin
    if UniquelyID then
    begin
      //ID唯一时，确保要加入的节点不存在设置的ID
      while pNode <> nil do
      begin
        if pNode^.FID = AID then
        begin
          DoExsitsIDError;
          Exit;
        end;
        if pNode^.FNext = nil then
          Break
        else
          pNode := pNode^.FNext;
      end;
    end
    else
    begin
      while pNode^.FNext <> nil do
        pNode := pNode^.FNext;
    end;

    if not FHashNodeMem.GetMem( Pointer(pClashNode) ) then
    begin
      DoNoGetMem;
      Exit;
    end;
    pClashNode^.FID := AID;
    pClashNode^.FData := AData;
    pClashNode^.FNext := nil;
    pNode^.FNext := pClashNode;
  end;
  Result := True;
  Inc( FCount );
end;

constructor THashArray.Create;
begin
  FActive := False;
  FHashTableCount := 8 * 1024;
  FMaxHashNodeCount := 16 * 1024;
  FUniquelyID := True;
  FCount := 0;
  FHashNodeMem := nil;
  FHashTable := nil;
end;

destructor THashArray.Destroy;
begin
  Active := False;
  inherited;
end;

procedure THashArray.DoExsitsIDError;
begin
  FLastErrorCode := 3;
  FLastErrorText := '指定的ID已经存在哈希表了';
end;

procedure THashArray.DoNoActiveError;
begin
  FLastErrorCode := 1;
  FLastErrorText := '需要先激活HashArray对象才能使用( Active 为 False)';
end;

procedure THashArray.DoNoGetMem;
begin
  FLastErrorCode := 2;
  FLastErrorText := '无法申请所需要的内存，当前可用内存块为: ' + IntToStr( FHashNodeMem.Count );
end;

procedure THashArray.DoNotFindNode;
begin
  FLastErrorCode := 4;
  FLastErrorText := '找不到指定ID的节点';
end;

function THashArray.Find(const AID: Cardinal; var Ap: Pointer; const AIsDel: Boolean): Boolean;
var
  pNode, pParent: pHashNode;
  nIndex: Integer;
begin
  Result := False;
  if not Active then
  begin
    DoNoActiveError;
    Exit;
  end;
  nIndex := IDToIndex( AID );
  pNode := FHashTable[ nIndex ];
  pParent := nil;
  while pNode <> nil do
  begin
    if pNode^.FID = AID then
    begin
      Result := True;
      Break;
    end;
    pParent := pNode;
    pNode := pNode^.FNext;
  end;
  if not Result then
  begin
    DoNotFindNode;
    Exit;
  end;

  Ap := pNode^.FData;
  if AIsDel then
  begin
    if pParent = nil then
      FHashTable[ nIndex ] := pNode^.FNext
    else
      pParent^.FNext := pNode^.FNext;
    FHashNodeMem.FreeMem( pNode );
    Dec( FCount );
  end;
end;

function THashArray.Find(const AID: Cardinal; const AFindCallBack: TOnFindNode; const AParamPointer: Pointer): Integer;
var
  pNode, pParent: pHashNode;
  nIndex: Integer;
  bDel, bContinue: Boolean;
begin
  Result := -1;
  if not Active then
  begin
    DoNoActiveError;
    Exit;
  end;
  nIndex := IDToIndex( AID );
  pNode := FHashTable[ nIndex ];
  bContinue := True;
  pParent := nil;
  while bContinue and (pNode <> nil) do
  begin
    bDel := False;
    bContinue := True;

    if pNode^.FID = AID then
    begin
      AFindCallBack( Self, AParamPointer, pNode^.FData, bDel, bContinue );
      Inc( Result );
      if bDel then
      begin
        if pParent = nil then
        begin
          FHashTable[ nIndex ] := pNode^.FNext;
          FHashNodeMem.FreeMem( pNode );
          pNode := FHashTable[ nIndex ];
        end
        else
        begin
          pParent^.FNext := pNode^.FNext;
          FHashNodeMem.FreeMem( pNode );
          pNode := pParent^.FNext;
        end;
        Dec( FCount );
        Continue;
      end;
    end;
    pParent := pNode;
    pNode := pNode^.FNext;
  end;
end;

function THashArray.IDToIndex(const AID: Cardinal): Integer;
begin
  Result := AID mod (FHashTableCount - 1);
end;

procedure THashArray.Loop(const ALoopCallBack: TOnLoopNode);
var
  pNode, pParent: pHashNode;
  i: Integer;
  bDel, bContinue: Boolean;
begin
  if not Active then
  begin
    DoNoActiveError;
    Exit;
  end;

  for i := Low(FHashTable) to High(FHashTable) do
  begin
    bContinue := True;
    pParent := nil;
    pNode := FHashTable[i];
    while (pNode <> nil) and bContinue do
    begin
      bDel := False;
      bContinue := True;
      ALoopCallBack( Self, pNode^.FID, pNode^.FData, bDel, bContinue );
      if bDel then
      begin
        if pParent = nil then
        begin
          FHashTable[ i ] := pNode^.FNext;
          FHashNodeMem.FreeMem( pNode );
          pNode := FHashTable[ i ];
        end
        else
        begin
          pParent^.FNext := pNode^.FNext;
          FHashNodeMem.FreeMem( pNode );
          pNode := pParent^.FNext;
        end;
        Dec( FCount );
      end
      else
      begin
        pParent := pNode;
        pNode := pNode^.FNext;
      end;
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
  end;
end;

procedure THashArray.SetAllowClash(const Value: Boolean);
begin
  if (not Active) and (FUniquelyID <> Value) then
    FUniquelyID := Value;
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
  try
    FActive := False;
    SetLength( FHashTable, 0 );
    FreeAndNil( FHashNodeMem );
    FCount := 0;
  except
  end;
end;

end.
