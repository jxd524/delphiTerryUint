unit uQueues;

interface

uses
  Windows, Classes, SysUtils;

const
  MaxQueueLength = 4096;

type
  EQueueError = Exception;

  TPointerDynArray = array of Pointer;

  TPointerQueue = class //一个指针循环队列
  private
    FList: TPointerDynArray;
    FHead: Integer;
    FTail: Integer;
    FCount: Integer;
    FCapacity: Integer;
    FLock: TRTLCriticalSection;
    function GetPointer(Index: Integer): Pointer;
  public
    function InsertNode(Value: Pointer): Boolean;
    function GetFirstNode(var Value: Pointer): Boolean;
    function LockQueue: TPointerDynArray;
    procedure UnLock;
    procedure Clear;
    constructor Create(ACapacity: Integer = MaxQueueLength);
    destructor Destroy; override;
    property Capacity: Integer read FCapacity;
    property Count: Integer read FCount;
    property Item[Index: Integer]: Pointer read GetPointer;
  end;

  TStaticQueue = class
  private
    FMemory: HGLOBAL;
    FHead: Integer;
    FTail: Integer;
    FCount: Integer;
    FCapacity: Integer;
    FLock: TRTLCriticalSection;
    FNodeSize: Integer;
  public
    procedure LockQueue(ALock: Boolean);
    function InsertNode(AValue: Pointer): Boolean;
    function GetFirstNode(AValue: Pointer): Boolean;
    function Get(AIndex: Integer; AValue: Pointer): Boolean;
    function Put(AIndex: Integer; AValue: Pointer): Boolean;
    procedure Clear;
    constructor Create(ANodeSize: Integer; ACapacity: Integer = MaxQueueLength);
    destructor Destroy; override;
    property Capacity: Integer read FCapacity;
    property Count: Integer read FCount;
    property Memory: HGLOBAL read FMemory;
  end;

  TStaticMemoryManager = class //类似循环队列,包含节点内存与存放节点指针的空间
  private
    FMemory: HGLOBAL;
    FFreeQueue: TPointerQueue;
    FBlockSize: Integer;
    FCapacity: Integer;
    function GetFreeSpace: Integer;
    function GetUsedCount: Integer;
    function GetPointer(index: Integer): Pointer;
  public
    constructor Create(ABlockSize: Integer; ACapacity: Integer = MaxQueueLength);
    destructor Destroy; override;
    function GetMem(var P: Pointer): Boolean;
    function FreeMem(P: Pointer): Boolean;
    property Item[index: Integer]: Pointer read GetPointer; //指定节点内容
    property Capacity: Integer read FCapacity;
    property SpaceCount: Integer read GetFreeSpace; //
    property UsedCount: Integer read GetUsedCount; //已使用数量
  end;

implementation

{ TPointerQueue }

procedure TPointerQueue.Clear;
begin
  EnterCriticalSection(FLock);
  FCount := 0;
  FHead := 0;
  FTail := 0;
  LeaveCriticalSection(FLock);
end;

constructor TPointerQueue.Create(ACapacity: Integer);
begin
  FHead := 0;
  FTail := 0;
  FCount := 0;
  FCapacity := ACapacity;
  SetLength(FList, FCapacity);
  InitializeCriticalSection(FLock);
end;

destructor TPointerQueue.Destroy;
begin
  SetLength(FList, 0);
  DeleteCriticalSection(FLock);
  inherited;
end;

function TPointerQueue.GetFirstNode(var Value: Pointer): Boolean;
begin
  Result := False;
  Value := nil;
  EnterCriticalSection(FLock);
  try
    if FCount <= 0 then
      Exit;
    Value := FList[FHead];
    FHead := (FHead + 1) mod FCapacity;
    Dec(FCount);
    Result := True;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TPointerQueue.GetPointer(Index: Integer): Pointer;
begin
  Result := FList[Index];
end;

function TPointerQueue.InsertNode(Value: Pointer): Boolean;
begin
  Result := False;
  EnterCriticalSection(FLock);
  try
    if FCount >= FCapacity then
      Exit;
    FList[FTail] := Value;
    FTail := (FTail + 1) mod FCapacity;
    Inc(FCount);
    Result := True;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TPointerQueue.LockQueue: TPointerDynArray;
begin
  EnterCriticalSection(FLock);
  Result := FList;
end;

procedure TPointerQueue.UnLock;
begin
  LeaveCriticalSection(FLock);
end;

{ TStaticQueue }

procedure TStaticQueue.Clear;
begin
  LockQueue(True);
  FHead := 0;
  FTail := 0;
  FCount := 0;
  LockQueue(False);
end;

constructor TStaticQueue.Create(ANodeSize, ACapacity: Integer);
begin
  FCount := 0;
  FTail := 0;
  FHead := 0;
  FNodeSize := ANodeSize;
  FCapacity := ACapacity;
  InitializeCriticalSection(FLock); 
  FMemory := GlobalAlloc(GPTR, FNodeSize * FCapacity);
  if FMemory = 0 then
    raise EQueueError.Create('TStaticQueue.Create: Unable to alloc memory');
end;

destructor TStaticQueue.Destroy;
begin
  Clear;
  DeleteCriticalSection(FLock);
  GlobalFree(FMemory);
  inherited;
end;

function TStaticQueue.Get(AIndex: Integer; AValue: Pointer): Boolean;
var
  Source: Pointer;
begin
  Result := (AIndex >= 0) and (AIndex < FCapacity) and (AValue <> nil);
  if Result then
  begin
    Source := Pointer(Integer(FMemory) + FNodeSize * AIndex);
    Move(Source^, AValue^, FNodeSize);
  end;
end;

function TStaticQueue.GetFirstNode(AValue: Pointer): Boolean;
begin
  Result := False;
  if AValue = nil then Exit;
  LockQueue(True); //EnterCriticalSection
  try
    if FCount <= 0 then
      Exit; //关闭  返回False
    Get(FHead, AValue);
    FHead := (FHead + 1) mod FCapacity;
    Dec(FCount);
    Result := True;
  finally
    LockQueue(False); //Leave
  end;
end;

function TStaticQueue.InsertNode(AValue: Pointer): Boolean;
begin
  Result := False;
  if AValue = nil then Exit;
  try
    LockQueue(True);
    if FCount >= FCapacity then
      Exit;
    Put(FTail, AValue);
    Inc(FCount);
    FTail := (FTail + 1) mod FCapacity;
    Result := True;
  finally
    LockQueue(False);
  end;
end;

procedure TStaticQueue.LockQueue(ALock: Boolean);
begin
  if ALock then
    EnterCriticalSection(FLock)
  else
    LeaveCriticalSection(FLock);
end;

function TStaticQueue.Put(AIndex: Integer; AValue: Pointer): Boolean;
var
  Dest: Pointer;
begin
  Result := (AIndex >= 0) and (AIndex < FCapacity) and (AValue <> nil);
  if Result then
  begin
    Dest := Pointer(Integer(FMemory) + FNodeSize * AIndex);
    Move(AValue^, Dest^, FNodeSize);
  end;
end;


{ TStaticMemoryManager }

constructor TStaticMemoryManager.Create(ABlockSize, ACapacity: Integer);
var
  I: Integer;
begin
  FBlockSize := ABlockSize;
  FCapacity := ACapacity;
  FMemory := GlobalAlloc(GPTR, FBlockSize * FCapacity);
  if FMemory = 0 then
    raise EQueueError.Create('TStaticMemoryManager.Create: Unable to alloc memory');
  FFreeQueue := TPointerQueue.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    if not FFreeQueue.InsertNode(Pointer(FMemory + Cardinal(FBlockSize * I))) then
    begin
      FFreeQueue.Free;
      GlobalFree(FMemory);
      raise EQueueError.Create('TStaticMemoryManager.Create: Initialize FreeQueue error');
    end;
end;

destructor TStaticMemoryManager.Destroy;
begin
  FFreeQueue.Free;
  GlobalFree(FMemory);
  inherited;
end;

function TStaticMemoryManager.FreeMem(P: Pointer): Boolean;
begin
  Result := False;
  if P <> nil then
    Result := FFreeQueue.InsertNode(P);
end;

function TStaticMemoryManager.GetFreeSpace: Integer;
begin
  Result := FFreeQueue.Count;
end;

function TStaticMemoryManager.GetMem(var P: Pointer): Boolean;
begin
  Result := FFreeQueue.GetFirstNode(P);
end;

function TStaticMemoryManager.GetPointer(index: Integer): Pointer;
begin
  Result := FFreeQueue.Item[index];
end;

function TStaticMemoryManager.GetUsedCount: Integer;
begin
  Result := FBlockSize - FFreeQueue.Count;
end;

end.


