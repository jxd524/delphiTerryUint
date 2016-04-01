unit uKeyBoardMouseEvnet;

interface
   uses Windows, Messages, uConversion, uKeyScanCode;

  //点击
  procedure ClickSpecifyPosition(const AHandle: HWND; const xPos, yPos: Integer; bSendMessage: Boolean = False; const AClickSpace: Integer = 10);
  //双击
  procedure DBClickSpecifyPosition(const AHandle: HWND; const xPos, yPos: Integer; bSendMessage: Boolean = False);
  //
  procedure ClickButton(const AHandle: HWND);
  //将滚动条移动到最上方
  procedure MoveScrollToTop(const AHandle: HWND; AIsVerticalScroll: Boolean = True);
  //将滚动条移动到最下方
  procedure MoveScrollToBottom(const AHandle: HWND; AIsVerticalScroll: Boolean = True);
  //将滚动条移下一个位置
  procedure MoveScrollToNextPosition(const AHandle: HWND; AIsVerticalScroll: Boolean = True);
  //将滚动条移下一个位置
  procedure MoveScrollToUpNextPosition(const AHandle: HWND; AIsVerticalScroll: Boolean = True);

  procedure SetValueBit(var nValue: Integer; nCode: Word; ABeginBit: Integer); overload;
  procedure SetValueBit(var nValue: Integer; nCode: Byte; ABeginBit: Integer); overload;
  //编译 SHIFT 键 的LPARAM
  procedure BuildShift(var lParam: Integer; const ALeftShiftButton: Boolean);

  function FocusInput(const AScanCodes: array of Word): Boolean; overload;
  procedure FocusInput(const AScanCode: Word; const AIsNeedShift: Boolean); overload;
  procedure FocusInput(const AStr: string); overload;

  
implementation

procedure ClickSpecifyPosition(const AHandle: HWND; const xPos, yPos: Integer; bSendMessage: Boolean = False; const AClickSpace: Integer = 10);
var
  lParam: Integer;
begin
  lParam := MakeLParam(xPos, yPos);
  if bSendMessage then
  begin
    SendMessage(AHandle, WM_LBUTTONDOWN, MK_LBUTTON, lParam);
    Sleep(AClickSpace);
    SendMessage(AHandle, WM_LBUTTONUP, MK_LBUTTON, lParam);
  end
  else
  begin
    PostMessage(AHandle, WM_LBUTTONDOWN, MK_LBUTTON, lParam);
    Sleep(AClickSpace);
    PostMessage(AHandle, WM_LBUTTONUP, MK_LBUTTON, lParam);
  end;
end;

procedure DBClickSpecifyPosition(const AHandle: HWND; const xPos, yPos: Integer; bSendMessage: Boolean = False);
var
  lParam: Integer;
begin
  ClickSpecifyPosition(AHandle, xPos, yPos, bSendMessage);
  lParam := MakeLparam(xPos, yPos);
  if bSendMessage then
    SendMessage(AHandle, WM_LBUTTONDBLCLK, 0, lParam)
  else
    PostMessage(AHandle, WM_LBUTTONDBLCLK, 0, lParam);
end;

procedure ClickButton(const AHandle: HWND);
var
  R: TRect;
begin
  GetWindowRect(AHandle, R);
  ClickSpecifyPosition(AHandle, (R.Right - R.Left) div 2, (R.Bottom - R.Top) div 2, True);
end;

procedure MoveScrollToBottom(const AHandle: HWND; AIsVerticalScroll: Boolean = True);
begin
  if AIsVerticalScroll then
  begin
    SendMessage(AHandle, WM_VSCROLL, SB_BOTTOM, 0);
    SendMessage(AHandle, WM_VSCROLL, SB_ENDSCROLL, 0);
  end
  else
  begin
    SendMessage(AHandle, WM_HSCROLL, SB_BOTTOM, 0);
    SendMessage(AHandle, WM_HSCROLL, SB_ENDSCROLL, 0);
  end;
end;

procedure MoveScrollToTop(const AHandle: HWND; AIsVerticalScroll: Boolean = True);
begin
  if AIsVerticalScroll then
  begin
    SendMessage(AHandle, WM_VSCROLL, SB_TOP, 0);
    SendMessage(AHandle, WM_VSCROLL, SB_ENDSCROLL, 0);
  end
  else
  begin
    SendMessage(AHandle, WM_HSCROLL, SB_TOP, 0);
    SendMessage(AHandle, WM_HSCROLL, SB_ENDSCROLL, 0);
  end;
end;

procedure MoveScrollToNextPosition(const AHandle: HWND; AIsVerticalScroll: Boolean = True);
begin
  if AIsVerticalScroll then
  begin
    PostMessage(AHandle, WM_VSCROLL, SB_LINEDOWN, 0);
    Sleep(10);
    PostMessage(AHandle, WM_VSCROLL, SB_ENDSCROLL, 0);
  end
  else
  begin
    PostMessage(AHandle, WM_HSCROLL, SB_LINEDOWN, 0);
    Sleep(10);
    PostMessage(AHandle, WM_HSCROLL, SB_ENDSCROLL, 0);
  end;
end;

procedure MoveScrollToUpNextPosition(const AHandle: HWND; AIsVerticalScroll: Boolean = True);
begin
  if AIsVerticalScroll then
  begin
    PostMessage(AHandle, WM_VSCROLL, SB_LINEUP, 0);
    Sleep(10);
    PostMessage(AHandle, WM_VSCROLL, SB_ENDSCROLL, 0);
  end
  else
  begin
    PostMessage(AHandle, WM_HSCROLL, SB_LINEUP, 0);
    Sleep(10);
    PostMessage(AHandle, WM_HSCROLL, SB_ENDSCROLL, 0);
  end;
end;

{
//Left Shift

000
0000
0
00101010
0000000000000001 WM_KEYDOWN nVirtKey:VK_SHIFT cRepeat:1 ScanCode:2A fExtended:0 fAltDown:0 fRepeat:0 fUp:0

0000000000000001 WM_KEYUP nVirtKey:VK_SHIFT cRepeat:1 ScanCode:2A fExtended:0 fAltDown:0 fRepeat:1 fUp:1

//Right Shift
<00003> 000A0830 P WM_KEYDOWN nVirtKey:VK_SHIFT cRepeat:1 ScanCode:36 fExtended:0 fAltDown:0 fRepeat:0 fUp:0
<00004> 000A0830 P WM_KEYUP nVirtKey:VK_SHIFT cRepeat:1 ScanCode:36 fExtended:0 fAltDown:0 fRepeat:1 fUp:1

//Left Ctrl
<00005> 000A0830 P WM_KEYDOWN nVirtKey:VK_CONTROL cRepeat:1 ScanCode:1D fExtended:0 fAltDown:0 fRepeat:0 fUp:0
<00006> 000A0830 P WM_KEYUP nVirtKey:VK_CONTROL cRepeat:1 ScanCode:1D fExtended:0 fAltDown:0 fRepeat:1 fUp:1

//Rifht Ctrl
<00007> 000A0830 P WM_KEYDOWN nVirtKey:VK_CONTROL cRepeat:1 ScanCode:1D fExtended:1 fAltDown:0 fRepeat:0 fUp:0
<00008> 000A0830 P WM_KEYUP nVirtKey:VK_CONTROL cRepeat:1 ScanCode:1D fExtended:1 fAltDown:0 fRepeat:1 fUp:1
}

procedure SetValueBit(var nValue: Integer; nCode: Word; ABeginBit: Integer); overload;
var
  i, iCode: Integer;
  nSize: Integer;
begin
  nSize := sizeof(nCode);
  iCode := 0;
  for i := ABeginBit to ABeginBit + nSize - 1 do
  begin
    SetBit( nValue, i, GetBit(nCode, iCode) );
    Inc(iCode);
  end;
end;

procedure SetValueBit(var nValue: Integer; nCode: Byte; ABeginBit: Integer); overload;
var
  i, iCode: Integer;
  nSize: Integer;
begin
  nSize := sizeof(nCode);
  iCode := 0;
  for i := ABeginBit to ABeginBit + nSize - 1 do
  begin
    SetBit( nValue, i, GetBit(nCode, iCode) );
    Inc(iCode);
  end;
end;

procedure BuildShift(var lParam: Integer; const ALeftShiftButton: Boolean);
var
  nScanCode, cRepeat: Word;
begin
  if ALeftShiftButton then
    nScanCode := $2A
  else
    nScanCode := $36;
  cRepeat := 1;
  lParam := 0;
  SetValueBit( lParam, cRepeat, 0 );
  SetValueBit( lParam, nScanCode, 16 );
end;

function FocusInput(const AScanCodes: array of Word): Boolean;
const 
  KEYEVENTF_SCANCODE = $00000008;
var
  aryIt: array[0..1] of TInput;
  nCount: Integer;
  nIndex: Integer;
begin
  nCount := Length( AScanCodes );                                                                
  nIndex := 0;
  while nIndex < nCount do
  begin
    aryIt[0].Itype := INPUT_KEYBOARD;
    aryIt[0].ki.wScan := AScanCodes[nIndex];
    aryIt[0].ki.dwFlags := KEYEVENTF_SCANCODE;
    aryIt[0].ki.time := 0;
    aryIt[1].Itype := INPUT_KEYBOARD;
    aryIt[1].ki.wScan := AScanCodes[nIndex];
    aryIt[1].ki.dwFlags := KEYEVENTF_SCANCODE or KEYEVENTF_KEYUP;
    aryIt[1].ki.time := 0;
    Inc( nIndex );
    SendInput( 1, aryIt[0], SizeOf(TInPut) );
    Sleep(1);
  end;
  Result := nIndex = nCount;
end;

procedure FocusInput(const AScanCode: Word; const AIsNeedShift: Boolean);
const 
  KEYEVENTF_SCANCODE = $00000008;
var
  aryIt: array[0..1] of TInput;
begin
  if AIsNeedShift then
  begin
    aryIt[0].Itype := INPUT_KEYBOARD;
    aryIt[0].ki.wScan := $2A;
    aryIt[0].ki.dwFlags := KEYEVENTF_SCANCODE;
    aryIt[0].ki.time := 0;
    SendInput( 1, aryIt[0], SizeOf(TInPut) )
  end;
  
  aryIt[0].Itype := INPUT_KEYBOARD;
  aryIt[0].ki.wScan := AScanCode;
  aryIt[0].ki.dwFlags := KEYEVENTF_SCANCODE;
  aryIt[0].ki.time := 0;
  aryIt[1].Itype := INPUT_KEYBOARD;
  aryIt[1].ki.wScan := AScanCode;
  aryIt[1].ki.dwFlags := KEYEVENTF_SCANCODE or KEYEVENTF_KEYUP;
  aryIt[1].ki.time := 0;
  SendInput( 1, aryIt[0], SizeOf(TInPut) );
    
  if AIsNeedShift then
  begin
    aryIt[0].Itype := INPUT_KEYBOARD;
    aryIt[0].ki.wScan := $2A;
    aryIt[0].ki.dwFlags := KEYEVENTF_SCANCODE or KEYEVENTF_KEYUP;
    aryIt[0].ki.time := 0;    
    SendInput( 1, aryIt[0], SizeOf(TInPut) )
  end;
end;

procedure FocusInput(const AStr: string);
var
  i: Integer;
  nScanCode: Word;
  bIsShift: Boolean;
begin
  for i := 1 to Length(AStr) do
  begin
    if CharToScanCode(AStr[i], nScanCode, bIsShift) then
      FocusInput( nScanCode, bIsShift );
  end;
end;

end.
