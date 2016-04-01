unit uShareMemory;

interface

uses
  Windows, SysUtils;

type
  {$M+}
  TShareMemory = class(TObject)
  private
    FMapplingHandle: THandle;
    FShareName: string;
    FCreateProperty: DWORD;
    FViewProperty: DWORD;
    FMapViewPoint: Pointer;
    procedure CheckMapView;
  public
    constructor Create(AHandle: THandle = 0; AShareName: string = ''; AMemorySize: Integer = 0;
                       ACreateProperty: DWORD = PAGE_READWRITE; AViewProperty: DWORD = FILE_MAP_ALL_ACCESS);
    destructor Destroy; override;
    function SetShareMem(AData: PChar; AByteCount, AOffset: Integer): Boolean;
    function GetShareMem(AOffset: Integer): Pointer;
    function MapToMemory(AOffsetBytePos, AMapByteCount: Integer): Pointer;
    property MapViewPoint: Pointer read FMapViewPoint;
  published
    property ShareName: string read FShareName;
    property CreateProperty: DWORD read FCreateProperty;
    property ViewProperty: DWORD read FViewProperty;
  end;
  {$M-}
implementation

{ TShareMemory }

procedure TShareMemory.CheckMapView;
begin
  if FMapViewPoint = nil then
    MapToMemory(0, 0);
end;

constructor TShareMemory.Create(AHandle: THandle; AShareName: string; AMemorySize: Integer;
  ACreateProperty: DWORD; AViewProperty: DWORD);
var
  SecurityAttibutes: SECURITY_ATTRIBUTES;
  SecurityDescriptor: SECURITY_DESCRIPTOR;
  guid: TGUID;
begin
  //安全权限控制,确定一般进程与作为服务之间可共享数据
  InitializeSecurityDescriptor(@SecurityDescriptor, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@SecurityDescriptor, true, nil, false);
  SecurityAttibutes.nLength := sizeof(SecurityAttibutes);
  SecurityAttibutes.lpSecurityDescriptor := @SecurityDescriptor;
  SecurityAttibutes.bInheritHandle := False;

  if (AShareName = '') and ( S_OK = CreateGUID(guid) ) then
    FShareName := GUIDToString( guid )
  else
    FShareName := AShareName;
  FCreateProperty := ACreateProperty;
  FViewProperty := AViewProperty;
  FMapViewPoint := nil;
  if AHandle = 0 then
    AHandle := INVALID_HANDLE_VALUE;
  if AMemorySize <= 0 then
    AMemorySize := 1024;
  FMapplingHandle := CreateFileMapping( AHandle, @SecurityAttibutes, FCreateProperty, 0, AMemorySize, PChar(FShareName) );
  if (FMapplingHandle <> 0) and ( GetLastError = ERROR_ALREADY_EXISTS ) then   //内存文件已存在
    FMapplingHandle := OpenFileMapping( FViewProperty, False, PChar(FShareName) );
  if FMapplingHandle = 0 then
    raise Exception.Create('Can''t Mapping Share Memory~; error: ' + IntToStr(GetLastError) );
  CheckMapView;
end;

destructor TShareMemory.Destroy;
begin
  if FMapViewPoint <> nil then
    UnmapViewOfFile( FMapViewPoint );
  if FMapplingHandle <> 0 then
    CloseHandle( FMapplingHandle );
  inherited;
end;

function TShareMemory.GetShareMem(AOffset: Integer): Pointer;
begin
  CheckMapView;
  Result := Pointer( Integer(FMapViewPoint) + AOffset );
end;

function TShareMemory.MapToMemory(AOffsetBytePos, AMapByteCount: Integer): Pointer;
begin
  if FMapViewPoint <> nil then
    UnmapViewOfFile( FMapViewPoint );
  FMapViewPoint := MapViewOfFile( FMapplingHandle, FViewProperty, 0, AOffsetBytePos, AMapByteCount );
  Result := FMapViewPoint;  
end;

function TShareMemory.SetShareMem(AData: PChar; AByteCount, AOffset: Integer): Boolean;
var
  pAddr: Pointer;
begin
  Result := False;
  try
    pAddr := GetShareMem( AOffset );
    Move( AData^, pAddr^, AByteCount );
//    CopyMemory( pAddr, AData, AByteCount );
  except
    Result := False;
  end;
end;

end.
