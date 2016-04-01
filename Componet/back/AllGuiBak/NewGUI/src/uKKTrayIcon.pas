{
单元名称: JxdTrayIcon
单元作者: 江晓德(jxd524@163.com)
网    址: www.geysoft.com
说    明: 直接拖放在主界面就可以使用了.
开始时间: 2007-10-24
修改时间: 2007-11-16 (最后修改) 
}
unit uKKTrayIcon;

interface

uses
  SysUtils, Windows, Classes, Messages, Forms, ShellAPI, Graphics, Menus, Controls;

const UM_TrayIconMessage = WM_USER + $F4;

type
  TTrayState = (tsMini, tsRestore, tsHitStringChanged);
  TFlyTrayPositionChanged = procedure(Self: TObject; ASelfState: TTrayState) of Object;
  TKKOnDClick = procedure(Self: TObject; var bHandleMsg: Boolean) of object;
  TKKTrayIcon = class(TComponent)
  private
    { Private declarations }
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
    FOnDClick: TKKOnDClick;
    FShowAlway: Boolean;
    procedure AddTrayIcon;
    procedure SetTrayHitText(const Value: string);
    procedure InitiaTrayIcon;
    procedure SetIcon(const Value: TIcon);
    procedure PopupAtCursor;
    procedure SetShowAlway(const Value: Boolean);
  protected
    { Protected declarations }
    procedure OnApplicationMiniSize(Sender: TObject);
    procedure HandleTrayMsg(var msg: TMessage);
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure AlterTrayIcon;
    procedure DeleteTrayIcon;
    procedure DoubleClick;
  published
    { Published declarations }
    property OnTrayStateChanged: TFlyTrayPositionChanged read FTrayPositionChanged write FTrayPositionChanged;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDClick: TKKOnDClick read FOnDClick write FOnDClick;
    property TrayHitText: string read FHitText write SetTrayHitText stored True;
    property Icon: TIcon read FIcon write SetIcon;
    property PopupMenu: TPopupMenu read FTrayPopuMenu write FTrayPopuMenu;
    property ShowAlway: Boolean read FShowAlway write SetShowAlway; 
  end;
implementation

{ TFlyTrayIcon }

procedure TKKTrayIcon.AddTrayIcon;
begin
  if FIsShowTrayIcon then DeleteTrayIcon;
  InitiaTrayIcon;
  if IsWindowVisible(Application.Handle) then
    ShowWindow(Application.Handle, SW_HIDE);
  if IsWindowVisible(FMainHandle) then
    ShowWindow(FMainHandle, SW_HIDE);
  Shell_NotifyIcon(NIM_ADD, @FNotifyIcon);
  FIsShowTrayIcon := True;
  if Assigned(FTrayPositionChanged) then
    FTrayPositionChanged(Self, tsMini);
end;

procedure TKKTrayIcon.AlterTrayIcon;
begin
  if FIsShowTrayIcon then
  begin
    InitiaTrayIcon;
    if Shell_NotifyIcon(NIM_MODIFY, @FNotifyIcon) then
    begin
      if Assigned(FTrayPositionChanged) then
        FTrayPositionChanged(Self, tsHitStringChanged);
    end;
  end;
end;

constructor TKKTrayIcon.Create(Owner: TComponent);
begin
  inherited;
  if not (Owner is TForm) then
    raise Exception.Create('TrayIcon''s parent must be a Form!');
  FShowAlway := True;
  FTrayIconMessage := RegisterWindowMessage('JxdTrayIcon');
  Application.OnMinimize := OnApplicationMiniSize;
  FMainHandle := (Owner as TForm).Handle;
  FMainForm := Owner as TForm;
  FHitText := '雨缘(JxdTrayIcon)http://www.geysoft.com';
  FIcon := TIcon.Create;
  FIsShowTrayIcon := False;
  if FShowAlway and (not (csDesigning in ComponentState) ) then
    AddTrayIcon;
end;

procedure TKKTrayIcon.DeleteTrayIcon;
begin
  if Shell_NotifyIcon(NIM_DELETE, @FNotifyIcon) then
  begin
    FIsShowTrayIcon := False;
    if Assigned(FTrayPositionChanged) then
      FTrayPositionChanged(Self, tsRestore);
  end;
end;

destructor TKKTrayIcon.Destroy;
begin
  Shell_NotifyIcon(NIM_DELETE, @FNotifyIcon);
  FIcon.Free;
  inherited;
end;

procedure TKKTrayIcon.DoubleClick;
var
  msg: TMessage;
begin
  msg.Msg := UM_TrayIconMessage;
  msg.LParam := WM_LBUTTONDBLCLK;
  HandleTrayMsg(msg);
end;

procedure TKKTrayIcon.HandleTrayMsg(var msg: TMessage);
var
  bHandle: Boolean;
begin
  if msg.Msg = UM_TrayIconMessage then
  begin
    case msg.LParam of
      WM_LBUTTONDBLCLK: begin
        bHandle := True;
        if Assigned(FOnDClick) then
          OnDClick(Self, bHandle);
        if not bHandle then Exit;
        if not IsWindowVisible(Application.Handle) then
          ShowWindow(Application.Handle, SW_SHOW);
        if not IsWindowVisible(FMainHandle) then
        begin
          ShowWindow(FMainHandle, SW_SHOW);
          Application.Minimize;
          Application.Restore;
        end;

        if not FShowAlway then
          DeleteTrayIcon;
        SetForegroundWindow(FMainHandle);
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

procedure TKKTrayIcon.InitiaTrayIcon;
begin
  FNotifyIcon.cbSize := SizeOf(FNotifyIcon);
  if FNotifyIcon.Wnd = 0 then
    FNotifyIcon.Wnd := Classes.AllocateHWnd(HandleTrayMsg);
  FNotifyIcon.uID := FTrayIconMessage;
  FNotifyIcon.uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;
  FNotifyIcon.uCallbackMessage := UM_TrayIconMessage;
  if FIcon.Handle <> 0 then
    FNotifyIcon.hIcon := FIcon.Handle
  else
    FNotifyIcon.hIcon := Application.Icon.Handle;
  SetLength(FHitText, Length(FHitText) + 1);
  StrPCopy(FNotifyIcon.szTip, FHitText);
end;

procedure TKKTrayIcon.OnApplicationMiniSize(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
    AddTrayIcon;
end;

procedure TKKTrayIcon.PopupAtCursor;
var
  CursorPos: TPoint;
begin
  if Assigned(PopupMenu) then
    if PopupMenu.AutoPopup then
      if GetCursorPos(CursorPos) then
      begin
        Application.ProcessMessages;
        SetForegroundWindow((Owner as TWinControl).Handle);
        PopupMenu.PopupComponent := Self;
        PopupMenu.Popup(CursorPos.X, CursorPos.Y);
        PostMessage((Owner as TWinControl).Handle, WM_NULL, 0, 0);
      end;
end;

procedure TKKTrayIcon.SetIcon(const Value: TIcon);
begin
  FIcon.Assign(Value);
  AlterTrayIcon;
end;

procedure TKKTrayIcon.SetShowAlway(const Value: Boolean);
begin
  FShowAlway := Value;
  AlterTrayIcon;
end;

procedure TKKTrayIcon.SetTrayHitText(const Value: string);
begin
  FHitText := Value;
  AlterTrayIcon;
end;

end.
