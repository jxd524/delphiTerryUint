{
单元名称: uMemoryManage
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: 数据结构操作
开始时间: 2010-09-14
修改时间: 2010-09-14(最后修改)
注意事项: 线程安全

    可变内存管理器，先申请一大块，分成N小块，当不够是，重复申请大块，再分小块
    回收时：判断内存地址所属
}
unit uJxdMemoryManage;

interface

uses
  Windows, Classes, SysUtils;

type
  {循环指针队列，用于管理内存块, 非线程安全}
  TPointerDynArray = array of Pointer;
  TPointerQueue = class
  public
    function Put(Value: Pointer): Boolean;
    function Get(var Value: Pointer): Boolean;
    function Count: Integer;
    function Capacity: Integer;

    constructor Create(const ACApacity: Integer);
    destructor Destroy; override;
  private
    FList: TPointerDynArray;
    FHead: Integer;
    FTail: Integer;
    FCount: Integer;
    FCapacity: Integer;
  end;

  TxdFixedMemoryManager = class
  public
    function GetMem(var P: Pointer): Boolean;
    function FreeMem(P: Pointer): Boolean;

    function Count: Cardinal;
    function Capacity: Cardinal;
    function BlockSize: Cardinal;

    constructor Create(const ABlockSize, ACapacity: Cardinal);
    destructor Destroy; override;
  protected
    FMemory: Pointer;
    FFreeQueue: TPointerQueue;
    FBlockSize: Cardinal;
    FCapacity: Cardinal;
  end;

  TxdFixedMemoryManagerEx = class(TxdFixedMemoryManager)
  public
    function Item(const AIndex: Integer): Pointer;
  end;

implementation

{ TPointerQueue }

function TPointerQueue.Capacity: Integer;
begin
  Result := FCapacity;
end;

function TPointerQueue.Count: Integer;
begin
  Result := FCount;
end;

constructor TPointerQueue.Create(const ACApacity: Integer);
begin
  FHead := 0;
  FTail := 0;
  FCount := 0;
  FCapacity := ACapacity;
  SetLength(FList, FCapacity);
end;

destructor TPointerQueue.Destroy;
begin
  SetLength( FList, 0 );
  inherited;
end;

function TPointerQueue.Get(var Value: Pointer): Boolean;
begin
  Result := FCount > 0;
  if Result then
  begin
    Value := FList[FHead];
    FHead := (FHead + 1) mod FCapacity;
    Dec(FCount);
  end;
end;

function TPointerQueue.Put(Value: Pointer): Boolean;
begin
  Result := FCount < FCapacity;
  if Result then
  begin
    FList[FTail] := Value;
    FTail := (FTail + 1) mod FCapacity;
    Inc(FCount);
  end;
end;

{ TxdFixedMemoryManager }

function TxdFixedMemoryManager.BlockSize: Cardinal;
begin
  Result := FBlockSize;
end;

function TxdFixedMemoryManager.Capacity: Cardinal;
begin
  Result := FCapacity;
end;

function TxdFixedMemoryManager.Count: Cardinal;
begin
  Result := FFreeQueue.Count;
end;

constructor TxdFixedMemoryManager.Create(const ABlockSize, ACapacity: Cardinal);
var
  I: Cardinal;
begin
  FBlockSize := ABlockSize;
  FCapacity := ACapacity;

  FMemory := AllocMem( FBlockSize * FCapacity );
  if FMemory = nil then
    raise Exception.Create('TxdFixedMemoryManager.Create: Unable to alloc memory');
  FFreeQueue := TPointerQueue.Create( FCapacity );
  for I := 0 to FCapacity - 1 do
  begin
    if not FFreeQueue.Put(Pointer(Cardinal(FMemory) + Cardinal(FBlockSize * I))) then
    begin
      FFreeQueue.Free;
      FreeMemory( FMemory );
      raise Exception.Create('TxdFixedMemoryManager.Create: Initialize FreeQueue error');
    end;
  end;
end;

destructor TxdFixedMemoryManager.Destroy;
begin
  FFreeQueue.Free;
  FreeMemory( FMemory );
  inherited;
end;

function TxdFixedMemoryManager.FreeMem(P: Pointer): Boolean;
begin
  Result := ( ((Cardinal(p) - Cardinal(FMemory)) mod FBlockSize) = 0 ) and  FFreeQueue.Put( p );
end;

function TxdFixedMemoryManager.GetMem(var P: Pointer): Boolean;
begin
  Result := FFreeQueue.Get( p );
end;

{ TxdFixedMemoryManagerEx }

function TxdFixedMemoryManagerEx.Item(const AIndex: Integer): Pointer;
begin
  if (AIndex >= 0) and (AIndex < Integer(FCapacity)) then
    Result := FFreeQueue.FList[AIndex]
  else
    Result := nil;
end;

end.
