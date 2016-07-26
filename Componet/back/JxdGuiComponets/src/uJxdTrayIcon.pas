{
单元名称: uJxdTrayIcon
单元作者: 江晓德(jxd524@163.com)
说    明: 直接拖放在主界面就可以使用了.
开始时间: 2007-10-24
修改时间: 2010-06-08 (最后修改) 
}
unit uJxdTrayIcon;

interface

uses
  SysUtils, Windows, Classes, Messages, Forms, ShellAPI, Graphics, Menus, Controls;

const UM_TrayIconMessage = WM_USER + $F4;

type
  TTrayState = (tsMini, tsRestore, tsHitStringChanged);
  TFlyTrayPositionChanged = procedure(Self: TObject; ASelfState: TTrayState) of Object;

  TxdTrayIcon = class(TComponent)
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteTrayIcon;
  protected
    procedure OnApplicationMiniSize(Sender: TObject);
    procedure HandleTrayMsg(var msg: TMessage);
    procedure Loaded; override;
  private
    FIsShowTrayIcon: Boolean;
    FMainHandle: HWND;
    FMainForm: TForm;
    FIcon: TIcon;
    FTrayPopuMenu: TPopupMenu;
    FNotifyIcon: TNotifyIconData;
    FTrayPositionChanged: TFlyTrayPositionChanged;
    FHitText: string;
    FTrayIconMessage: Cardinal;
    FOnClick: TNotifyEvent;
    FOnDClick: TNotifyEvent;
    FAlwaysShowTray: Boolean;
    procedure AddTrayIcon;
    procedure SetTrayHitText(const Value: string);
    procedure AlterTrayIcon;
    procedure InitiaTrayIcon;
    procedure SetIcon(const Value: TIcon);
    procedure PopupAtCursor;
    procedure SetAlwaysShowTray(const Value: Boolean);
  published
    property TrayHitText: string read FHitText write SetTrayHitText stored True;
    property Icon: TIcon read FIcon write SetIcon;
    property PopupMenu: TPopupMenu read FTrayPopuMenu write FTrayPopuMenu;
    property AlwaysShowTray: Boolean read FAlwaysShowTray write SetAlwaysShowTray;

    property OnTrayStateChanged: TFlyTrayPositionChanged read FTrayPositionChanged write FTrayPositionChanged;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDClick: TNotifyEvent read FOnDClick write FOnDClick;
  end;
implementation

{ TFlyTrayIcon }

procedure TxdTrayIcon.AddTrayIcon;
begin
//  if not (csDesigning in ComponentState) and AlwaysShowTray and not FIsShowTrayIcon then
  begin
    if FIsShowTrayIcon then DeleteTrayIcon;
    InitiaTrayIcon;
    if not AlwaysShowTray and IsWindowVisible(Application.Handle) then
      ShowWindow(Application.Handle, SW_HIDE);
    if not AlwaysShowTray and IsWindowVisible(FMainHandle) then
      ShowWindow(FMainHandle, SW_HIDE);
    Shell_NotifyIcon(NIM_ADD, @FNotifyIcon);
    FIsShowTrayIcon := True;
    if Assigned(FTrayPositionChanged) then
      FTrayPositionChanged(Self, tsMini);
  end;
end;

procedure TxdTrayIcon.AlterTrayIcon;
begin
  InitiaTrayIcon;
  if Shell_NotifyIcon(NIM_MODIFY, @FNotifyIcon) and FIsShowTrayIcon then
  begin
    if Assigned(FTrayPositionChanged) then
      FTrayPositionChanged(Self, tsHitStringChanged);
  end;
end;

constructor TxdTrayIcon.Create(Owner: TComponent);
begin
  inherited;
  if not (Owner is TForm) then
    raise Exception.Create('TrayIcon''s parent must be a Form!');
  FTrayIconMessage := RegisterWindowMessage('Terry TrayIcon');
  Application.OnMinimize := OnApplicationMiniSize;
  FMainHandle := (Owner as TForm).Handle;
  FMainForm := Owner as TForm;
  FHitText := '迅达(Terry TrayIcon)';
  FIcon := TIcon.Create;
  FIsShowTrayIcon := False;
  FAlwaysShowTray := False;
end;

procedure TxdTrayIcon.DeleteTrayIcon;
begin
  if Shell_NotifyIcon(NIM_DELETE, @FNotifyIcon) then
  begin
    FIsShowTrayIcon := False;
    if Assigned(FTrayPositionChanged) then
      FTrayPositionChanged(Self, tsRestore);
  end;
end;

destructor TxdTrayIcon.Destroy;
begin
  Shell_NotifyIcon(NIM_DELETE, @FNotifyIcon);
  FIcon.Free;
  inherited;
end;

procedure TxdTrayIcon.HandleTrayMsg(var msg: TMessage);
begin
  if msg.Msg = UM_TrayIconMessage then
  begin
    case msg.LParam of
      WM_LBUTTONDBLCLK: begin
        if not IsWindowVisible(Application.Handle) then
          ShowWindow(Application.Handle, SW_SHOW);
        if not IsWindowVisible(FMainHandle) then
          ShowWindow(FMainHandle, SW_SHOW);
        Application.Minimize;
        Application.Restore;
        if not AlwaysShowTray then
          DeleteTrayIcon;
        SetForegroundWindow(FMainHandle);
        if Assigned(FOnDClick) then
          OnDClick(Self);
      end;
      WM_RBUTTONUP: begin
        SetForeGroundWindow(FMainHandle);
        PopupAtCursor
      end;
      WM_LBUTTONUP: begin
        if Assigned(FOnClick) then
          OnClick(Self);
      end;
    end;
  end else
    Msg.Result := DefWindowProc(FNotifyIcon.Wnd, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TxdTrayIcon.InitiaTrayIcon;
begin
  FNotifyIcon.cbSize := SizeOf(FNotifyIcon);
  FNotifyIcon.Wnd := Classes.AllocateHWnd(HandleTrayMsg);
  FNotifyIcon.uID := FTrayIconMessage;
  FNotifyIcon.uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;
  FNotifyIcon.uCallbackMessage := UM_TrayIconMessage;
  if FIcon.Handle <> 0 then
    FNotifyIcon.hIcon := FIcon.Handle
  else
    FNotifyIcon.hIcon := Application.Icon.Handle;
  SetLength( FHitText, Length(FHitText) + 1 );
  StrPCopy( FNotifyIcon.szTip, FHitText );
end;

procedure TxdTrayIcon.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) and AlwaysShowTray and not FIsShowTrayIcon then
  AddTrayIcon;
end;

procedure TxdTrayIcon.OnApplicationMiniSize(Sender: TObject);
begin
  if not (csDesigning in ComponentState) and not AlwaysShowTray then
    AddTrayIcon;
end;

procedure TxdTrayIcon.PopupAtCursor;
var
  CursorPos: TPoint;
begin
  if Assigned(PopupMenu) and PopupMenu.AutoPopup and GetCursorPos(CursorPos) then
  begin
    Application.ProcessMessages;
    SetForegroundWindow((Owner as TWinControl).Handle);
    PopupMenu.PopupComponent := Self;
    PopupMenu.Popup(CursorPos.X, CursorPos.Y);
    PostMessage((Owner as TWinControl).Handle, WM_NULL, 0, 0);
  end;
end;

procedure TxdTrayIcon.SetAlwaysShowTray(const Value: Boolean);
begin
  if FAlwaysShowTray <> Value then
  begin
    FAlwaysShowTray := Value;
    if FAlwaysShowTray and not FIsShowTrayIcon then
      AddTrayIcon
    else if not FAlwaysShowTray and FIsShowTrayIcon then
      DeleteTrayIcon;
  end;
end;

procedure TxdTrayIcon.SetIcon(const Value: TIcon);
begin
  FIcon.Assign(Value);
  AlterTrayIcon;
end;

procedure TxdTrayIcon.SetTrayHitText(const Value: string);
begin
  FHitText := Value;
  AlterTrayIcon;
end;

end.
