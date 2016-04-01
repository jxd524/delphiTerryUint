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

    TPointerQueue: 一个循环指针队列，管理内存所指向的指针
    TxdFixedMemoryManager：固定内存管理模式，先申请一大块的内存块，把申请到的内存分成N多个固定的小块
      小块指针存在于 TPointerQueue 中
    TxdFixedMemoryManagerEx: 可随意访问指定位置的小块

    TxdBlockingMemoryManage: 实现生产者与消费者模式的内存管理器，缓存使用方式：需要时申请
}
unit uJxdMemoryManage;

interface

uses
  Windows, Classes, SysUtils;

type
  {循环指针队列，用于管理内存块, 线程安全}
  TPointerDynArray = array of Pointer;
  TPointerQueue = class
  public
    function Put(Value: Pointer): Boolean;
    function Get(var Value: Pointer): Boolean;
    function Count: Integer;
    function Capacity: Integer;

    constructor Create(const ACapacity: Integer);
    destructor Destroy; override;
  private
    FList: TPointerDynArray;
    FHead: Integer;
    FTail: Integer;
    FCount: Integer;
    FCapacity: Integer;
    FLock: TRTLCriticalSection;
  end;

  TPointerQueueEx = class(TPointerQueue)
  public
    function GetCurIndex: Integer;
    function Item(const AIndex: Integer): Pointer;
  end;

  TxdFixedMemoryManager = class
  public
    function GetMem(var P: Pointer): Boolean;
    function FreeMem(P: Pointer): Boolean;

    function Count: Cardinal; inline;
    function Capacity: Cardinal; inline;
    function BlockSize: Cardinal; inline;

    constructor Create(const ABlockSize, ACapacity: Cardinal);
    destructor Destroy; override;
  protected
    FMinAddr: Cardinal;
    FMaxAddr: Cardinal;
    FMemory: Pointer;
    FFreeQueue: TPointerQueue;
    FBlockSize: Cardinal;
    FCapacity: Cardinal;
  end;

  TxdFixedMemoryManagerEx = class(TxdFixedMemoryManager)
  public
    function Item(const AIndex: Integer): Pointer;
  end;

  PBlockInfo = ^TBlockInfo;
  TBlockInfo = record
    FBufferLen: Integer;
    FCurPos: Integer;
    FBuffer: PByte;
  end;

  {$M+}
  TxdBlockingMemoryManage = class
  public
    constructor Create(AMaxItem: Integer);
    destructor  Destroy; override;

    function AddBlockingBuffer(const ABuffer: PByte; const ABufferLen: Integer): Boolean;
    function ReadBuffer(const AReadByteLen: Integer; ABuffer: PByte): Boolean; //直接读取指定大小的数据
    //先读取指定长度(1, 2, 4)，读到的数据转成Integer: A; 后面的数据是否足够A长, 才会处理已经读取数据的位置
    function CheckCanRead(const ALen: Integer; ALenBuf: PByte): Boolean;
  protected
    function  NewBlock(const ABufLen: Integer; var Ap: PBlockInfo): Boolean; virtual; //内存申请
    procedure DisposeBlock(var Ap: PBlockInfo); virtual; //内在释放
    procedure Lock;
    procedure UnLock;
  private
    FQueueList: TPointerQueueEx;
    FLock: TRTLCriticalSection;
    FCurByteCount: Integer;
  published
    property CurByteCount: Integer read FCurByteCount;
  end;
  {$M-}
  
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
  InitializeCriticalSection( FLock );
  FHead := 0;
  FTail := 0;
  FCount := 0;
  FCapacity := ACapacity;
  SetLength(FList, FCapacity);
end;

destructor TPointerQueue.Destroy;
begin
  SetLength( FList, 0 );
  DeleteCriticalSection( FLock );
  inherited;
end;

function TPointerQueue.Get(var Value: Pointer): Boolean;
begin
  EnterCriticalSection( FLock );
  try
    Result := FCount > 0;
    if Result then
    begin
      Value := FList[FHead];
      FList[FHead] := nil;
      FHead := (FHead + 1) mod FCapacity;
      Dec(FCount);
    end;
  finally
    LeaveCriticalSection( FLock );
  end;
end;

function TPointerQueue.Put(Value: Pointer): Boolean;
begin
  EnterCriticalSection( FLock );
  try
    Result := FCount < FCapacity;
    if Result then
    begin
      FList[FTail] := Value;
      FTail := (FTail + 1) mod FCapacity;
      Inc(FCount);
    end;
  finally
    LeaveCriticalSection( FLock );
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

//  FMemory := AllocMem( FBlockSize * FCapacity );
  System.GetMem( FMemory, FBlockSize * FCapacity );
  if FMemory = nil then
    raise Exception.Create('TxdFixedMemoryManager.Create: Unable to alloc memory');
  FillChar( FMemory^, FBlockSize * FCapacity, 0 );
  FFreeQueue := TPointerQueue.Create( FCapacity );
  FMinAddr := Cardinal(FMemory);
  FMaxAddr := FMinAddr + FBlockSize * FCapacity;
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
//  FreeMemory( FMemory );
//  OutputDebugString( 'TxdFixedMemoryManager.Destroy' );
  System.FreeMem( FMemory, FBlockSize * FCapacity );
  inherited;
end;

function TxdFixedMemoryManager.FreeMem(P: Pointer): Boolean;
begin
  Result := (Cardinal(p) >= FMinAddr) and (Cardinal(p) < FMaxAddr) and ( ((Cardinal(p) - Cardinal(FMemory)) mod FBlockSize) = 0 ) and  FFreeQueue.Put( p );
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

{ TPointerQueueEx }

{ TPointerQueueEx }

function TPointerQueueEx.GetCurIndex: Integer;
begin
  Result := FHead;
end;

function TPointerQueueEx.Item(const AIndex: Integer): Pointer;
begin
  if (AIndex >= 0) and (AIndex < Integer(FCapacity)) then
    Result := FList[AIndex]
  else
    Result := nil;
end;

{ TxdBlockingMemoryManage }

function TxdBlockingMemoryManage.AddBlockingBuffer(const ABuffer: PByte; const ABufferLen: Integer): Boolean;
var
  p: PBlockInfo;
begin
  Lock;
  try
    Result := NewBlock( ABufferLen, p );
    if not Result then Exit;
    Move( ABuffer^, p^.FBuffer^, ABufferLen );
    Result := FQueueList.Put( p );
    if not Result then
      DisposeBlock( p )
    else
      Inc( FCurByteCount, ABufferLen );
  finally
    UnLock;
  end;
end;

function TxdBlockingMemoryManage.CheckCanRead(const ALen: Integer; ALenBuf: PByte): Boolean;
var
  p: PBlockInfo;
  nReadByte, nPos, nCurNeedReadByteCount, nIndex, nSrcPos, nWantReadLen: Integer;
begin
  if not (ALen in [1, 2, 4]) then
  begin
    Result := False;
    Exit;
  end;
  
  Lock;
  try
    Result := ALen <= CurByteCount;
    if not Result then Exit;
    nPos := 0;
    nCurNeedReadByteCount := ALen;
    nIndex := FQueueList.GetCurIndex;
    //先读取指定长度的数据
    while True do
    begin
      p := FQueueList.Item( nIndex );
      if not Assigned(p) then
      begin
        Result := False;
        Break;
      end;

      nSrcPos := p^.FCurPos;

      if (p^.FBufferLen - nSrcPos) >= nCurNeedReadByteCount then
        nReadByte := nCurNeedReadByteCount
      else
        nReadByte := p^.FBufferLen - nSrcPos;
      Move( PByte(Integer(p^.FBuffer) + nSrcPos)^, PByte(Integer(ALenBuf) + nPos)^, nReadByte );

      Inc( nPos, nReadByte );
      Dec( nCurNeedReadByteCount, nReadByte );

      if nPos = ALen then Break;
      Inc( nIndex );
    end;
    if not Result then Exit;
    nWantReadLen := Integer( ALenBuf^ );
    Result := nWantReadLen <= CurByteCount;
    if Result then
      ReadBuffer( ALen, ALenBuf );   
  finally
    UnLock;
  end;
end;

constructor TxdBlockingMemoryManage.Create(AMaxItem: Integer);
begin
  FQueueList := TPointerQueueEx.Create( AMaxItem );
  InitializeCriticalSection( FLock );
  FCurByteCount := 0;
end;

destructor TxdBlockingMemoryManage.Destroy;
var
  p: PBlockInfo;
begin
  while FQueueList.Get(Pointer(p)) do
    DisposeBlock( p );
  DeleteCriticalSection( FLock );
  inherited;
end;

procedure TxdBlockingMemoryManage.DisposeBlock(var Ap: PBlockInfo);
begin
  if Assigned(Ap) then
  begin
    if Assigned(Ap^.FBuffer) then
      FreeMem( Ap^.FBuffer );
    Dispose( Ap );
    Ap := nil;
    OutputDebugString( 'DisposeBlock: 释放内存成功' );
  end;
end;

procedure TxdBlockingMemoryManage.Lock;
begin
  EnterCriticalSection( FLock );
end;

function TxdBlockingMemoryManage.NewBlock(const ABufLen: Integer; var Ap: PBlockInfo): Boolean;
var
  p: PBlockInfo;
begin
  Result := False;
  if ABufLen <= 0 then Exit;
  try
    New( p );
    p^.FBufferLen := ABufLen;
    p^.FCurPos := 0;
    GetMem( p^.FBuffer, p^.FBufferLen );
    if not Assigned(p^.FBuffer) then
    begin
      DisposeBlock( p );
      Exit;
    end;
    Ap := p;
    Result := True;
  except
    DisposeBlock( p );
  end;
end;

function TxdBlockingMemoryManage.ReadBuffer(const AReadByteLen: Integer; ABuffer: PByte): Boolean;
var
  p: PBlockInfo;
  nReadByte, nPos, nCurNeedReadByteCount: Integer;
begin
  Lock;
  try
    Result := AReadByteLen <= CurByteCount;
    if not Result then Exit;
    nPos := 0;
    nCurNeedReadByteCount := AReadByteLen;
    while True do
    begin
      p := FQueueList.Item( FQueueList.GetCurIndex );
      if (p^.FBufferLen - p^.FCurPos) >= nCurNeedReadByteCount then
        nReadByte := nCurNeedReadByteCount
      else
        nReadByte := p^.FBufferLen - p^.FCurPos;
      Move( PByte(Integer(p^.FBuffer) + p^.FCurPos)^, PByte(Integer(ABuffer) + nPos)^, nReadByte );
      Inc( p^.FCurPos, nReadByte );
      Inc( nPos, nReadByte );
      Dec( FCurByteCount, nReadByte );
      Dec( nCurNeedReadByteCount, nReadByte );
      if p^.FCurPos = p^.FBufferLen then
      begin
        FQueueList.Get( Pointer(p) );
        DisposeBlock( p );
      end;
      if nPos = AReadByteLen then Break;
    end;
  finally
    UnLock;
  end;
end;

procedure TxdBlockingMemoryManage.UnLock;
begin
  LeaveCriticalSection( FLock );
end;

end.
