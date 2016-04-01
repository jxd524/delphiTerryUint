unit uJxdWndHook;

interface

uses
  Windows, Messages, Classes;

type
  TOnHookProc = procedure(AwParam, AlParam: Integer) of object;
  TAryHookProc = array of TOnHookProc;
  PHookInfo = ^THookInfo;
  THookInfo = record
    FHook: HHOOK;
    FHookID: Cardinal;
    FNotifyEventList: TAryHookProc;
  end;
  
  TxdWndHook = class
  public
    class function  AddHook(const AHookID: Integer; AHookProc: TOnHookProc): Boolean;
    class procedure RemoveHook(const AHookID: Cardinal; AHookProc: TOnHookProc);
  end;

function OnHookCallWndProc(ACode: Integer; AwParam: WPARAM; AlParam: LPARAM): LRESULT stdcall;
function OnHookGetMesProc(ACode: Integer; AwParam: WPARAM; AlParam: LPARAM): LRESULT stdcall;

implementation

type
  THookProcInfo = record
    FHookID: Cardinal;
    FHookProc: TFNHookProc;
  end;

var
  GHookInfoList: TList = nil;

const
  GWndHookEx: array[0..1] of THookProcInfo =
    (
      (FHookID: WH_CALLWNDPROC; FHookProc: OnHookCallWndProc),
      (FHookID: WH_GETMESSAGE; FHookProc: OnHookGetMesProc)
//      (WH_CALLWNDPROCRET, CallWndRetProc),
//      (WH_MSGFILTER, MessageProc),
//      (WH_SYSMSGFILTER, SysMsgProc)
    );

function HookIdToHookProc(const AHookID: Cardinal): TFNHookProc;
var
  i: Integer;
begin
  Result := nil;
  for i := Low(GWndHookEx) to High(GWndHookEx) do
  begin
    if GWndHookEx[i].FHookID = AHookID then
    begin
      Result := GWndHookEx[i].FHookProc;
      Break;
    end;
  end;
end;


function GetHookInfo(const AHookID: Cardinal; const AIsForceCreate: Boolean): PHookInfo;
var
  i: Integer;
  p: PHookInfo;
  proc: TFNHookProc;
begin
  Result := nil;
  for i := 0 to GHookInfoList.Count - 1 do
  begin
    p := GHookInfoList[i];
    if Assigned(p) then
    begin
      if p^.FHookID = AHookID then
      begin
        Result := p;
        Break;
      end;
    end;
  end;
  if (Result = nil) and AIsForceCreate then
  begin
    proc := HookIdToHookProc( AHookID );
    if not Assigned(proc) then Exit;


    New( Result );
    GHookInfoList.Add( Result );
    Result^.FHookID := AHookID;
    SetLength( Result^.FNotifyEventList, 0 );
    Result^.FHook := SetWindowsHookEx( Result^.FHookID, @proc, 0, GetCurrentThreadId );
    if Result^.FHook = 0 then
    begin
      GHookInfoList.Delete( GHookInfoList.IndexOf(Result) );
      Dispose( Result );
      Result := nil;
    end;
  end;
end;

function OnHookProc(const AHookId: Cardinal; ACode: Integer; AwParam: WPARAM; AlParam: LPARAM): LRESULT stdcall;
var
  p: PHookInfo;
  i, nCount: Integer;
begin
  p := GetHookInfo( AHookId, False );
  if ACode = HC_ACTION then
  begin
    nCount := Length( p^.FNotifyEventList );
    for i := 0 to nCount - 1 do
      p^.FNotifyEventList[i]( AwParam, AlParam );
  end;
  Result := CallNextHookEx( p^.FHook, ACode, AwParam, AlParam );
end;

function OnHookCallWndProc(ACode: Integer; AwParam: WPARAM; AlParam: LPARAM): LRESULT stdcall;
begin
  Result := OnHookProc( WH_CALLWNDPROC, ACode, AwParam, AlParam );
end;

function OnHookGetMesProc(ACode: Integer; AwParam: WPARAM; AlParam: LPARAM): LRESULT stdcall;
begin
  Result := OnHookProc( WH_GETMESSAGE, ACode, AwParam, AlParam );
end;

procedure AddNotifyEvent(var ALt: TAryHookProc; const AEvent: TOnHookProc);
var
  i, nCount: Integer;
begin
  nCount := Length( ALt );
  for i := 0 to nCount - 1 do
  begin
    if Integer(@ALt[i]) = Integer(@AEvent) then
      Exit;
  end;
  SetLength( ALt, nCount + 1 );
  ALt[ nCount ] := AEvent ;
end;

{ TxdWndHook }

class function TxdWndHook.AddHook(const AHookID: Integer; AHookProc: TOnHookProc): Boolean;
var
  p: PHookInfo;
begin
  p := GetHookInfo( AHookID, True );
  Result := Assigned( p );
  if Result then
    AddNotifyEvent( p^.FNotifyEventList, AHookProc );
end;

procedure FreeGHookLif;
var
  i: Integer;
  p: PHookInfo;
begin
  for i := 0 to GHookInfoList.Count - 1 do
  begin
    p := GHookInfoList[i];
    if Assigned(p) then
    begin
      if p^.FHook <> 0 then
        UnhookWindowsHookEx( p^.FHook );
      SetLength( p^.FNotifyEventList, 0 );
      Dispose( p );
    end;
  end;
  GHookInfoList.Free;
end;

class procedure TxdWndHook.RemoveHook(const AHookID: Cardinal; AHookProc: TOnHookProc);
var
  i, j, n: Integer;
  p: PHookInfo;
begin
  for i := 0 to GHookInfoList.Count - 1 do
  begin
    p := GHookInfoList[i];
    if Assigned(p) and (p^.FHookID = AHookID) then
    begin
      n := High( p^.FNotifyEventList );
      for j := Low(p^.FNotifyEventList) to n do
      begin
        if Integer(@p^.FNotifyEventList[j]) = Integer(@AHookProc) then
        begin
          if j <> n then
            Move( p^.FNotifyEventList[j + 1], p^.FNotifyEventList[j], (n - j) * SizeOf(TOnHookProc) );
          SetLength( p^.FNotifyEventList, n );
          Break;
        end;
      end;
      if Length(p^.FNotifyEventList) = 0 then
      begin
        UnhookWindowsHookEx( p^.FHook );
        GHookInfoList.Delete( i );
        Dispose( p );
      end;
      Break;
    end;
  end;
end;

initialization
  GHookInfoList := TList.Create;
finalization
  FreeGHookLif;

end.
