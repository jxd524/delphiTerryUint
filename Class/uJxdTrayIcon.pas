{
单元名称: uJxdTrayIcon
单元作者: 江晓德(jxd524@163.com)
说    明: 
开始时间: 2011-03-20
修改时间: 2011-03-20 (最后修改)
}
unit uJxdTrayIcon;

interface

uses
  SysUtils, Windows, Classes, Messages, Forms, ShellAPI, Graphics, Menus, Controls;

type
  {$M+}
  TOnMouseEvent = procedure(AButton: TMouseButton) of object;
  TxdTrayIcon = class
  public
    constructor Create;
    destructor  Destroy; override;

    procedure MyTest;
  private
    FNotifyIconData: TNotifyIconData;

    procedure ActiveTrayIcon;
    procedure UnActiveTrayIcon;

    procedure DoHandleTrayMsg(var msg: TMessage);
  private
    FActive: Boolean;
    FIconHandle: HICON;
    FText: string;
    FOnMouseDown: TOnMouseEvent;
    FOnMouseUp: TOnMouseEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetIconHandle(const Value: HICON);
    procedure SetText(const Value: string);
  published
    property IconHandle: HICON read FIconHandle write SetIconHandle;
    property Text: string read FText write SetText;
    property Active: Boolean read FActive write SetActive;
    property OnMouseDown: TOnMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TOnMouseEvent read FOnMouseUp write FOnMouseUp;
  end;
  {$M-}

implementation

const
  CtTrayIconMessage = WM_USER + $F4;
  CtTrayIconID = 524524;

{ TxdTrayIcon }

procedure TxdTrayIcon.ActiveTrayIcon;
begin
  try
    FNotifyIconData.cbSize := SizeOf( TNotifyIconData );
    if FNotifyIconData.Wnd = 0 then
      FNotifyIconData.Wnd := Classes.AllocateHWnd( DoHandleTrayMsg );
    FNotifyIconData.uID := CtTrayIconID;
    FNotifyIconData.uCallbackMessage := CtTrayIconMessage;
    FNotifyIconData.hIcon := FIconHandle;
    StrPCopy( FNotifyIconData.szTip, FText );

    FNotifyIconData.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;

    Shell_NotifyIcon( NIM_ADD, @FNotifyIconData );
    FActive := True;
  except
  end;
end;

constructor TxdTrayIcon.Create;
begin
  FActive := False;
  FIconHandle := Application.Icon.Handle;
  FText := '5yySoft' + #13#10 + 'http://www.5yySoft.com';
  FillChar( FNotifyIconData, SizeOf(TNotifyIconData), 0 );
end;

destructor TxdTrayIcon.Destroy;
begin
  Active := False;
  inherited;
end;

procedure TxdTrayIcon.DoHandleTrayMsg(var msg: TMessage);
var
  bDown, bUp: Boolean;
  mb: TMouseButton;
begin
  case msg.Msg of
    49322:
    begin
      try
        ActiveTrayIcon;
      except
      end;
    end;
    CtTrayIconMessage:
    begin
      if msg.WParam = CtTrayIconID then
      begin
        bDown := False;
        bUp := False;
        mb := mbLeft;
        case msg.LParam of
          WM_LBUTTONDOWN, WM_LBUTTONUP:
          begin
            mb := mbLeft;
            bDown := msg.LParam = WM_LBUTTONDOWN;
            bUp := not bDown;
          end;
          WM_MBUTTONDOWN, WM_MBUTTONUP:
          begin
            mb := mbMiddle;
            bDown := msg.LParam = WM_MBUTTONDOWN;
            bUp := not bDown;
          end;
          WM_RBUTTONDOWN, WM_RBUTTONUP:
          begin
            mb := mbRight;
            bDown := msg.LParam = WM_RBUTTONDOWN;
            bUp := not bDown;
          end;
          WM_LBUTTONDBLCLK:
          begin

          end;
        end;
        //end case
        if bDown then
        begin
          if Assigned(OnMouseDown) then
            OnMouseDown( mb );
        end
        else if bUp then
        begin
          if Assigned(OnMouseUp) then
            OnMouseUp( mb );
        end;
      end;
    end;
  end;
//  OutputDebugString( PChar(Format('MSG: %d; WParam: %d, LParam: %d', [msg.Msg, msg.WParam, msg.LParam])) );
end;

procedure TxdTrayIcon.MyTest;
begin
  if Shell_NotifyIcon( NIM_ADD, @FNotifyIconData ) then
    OutputDebugString( 'ok' )
  else
    OutputDebugString( 'fail' );
end;

procedure TxdTrayIcon.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      ActiveTrayIcon
    else
      UnActiveTrayIcon;
  end;
end;

procedure TxdTrayIcon.SetIconHandle(const Value: HICON);
begin
  if FIconHandle <> Value then
  begin
    FIconHandle := Value;
    if Active then
    begin
      FNotifyIconData.hIcon := FIconHandle;
      FNotifyIconData.uFlags := NIF_ICON;
      Shell_NotifyIcon( NIM_MODIFY, @FNotifyIconData );
    end;
  end;
end;

procedure TxdTrayIcon.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    if Active then
    begin
      StrPCopy( FNotifyIconData.szTip, FText );
      FNotifyIconData.uFlags := NIF_TIP;
      Shell_NotifyIcon( NIM_MODIFY, @FNotifyIconData );
    end;
  end;
end;

procedure TxdTrayIcon.UnActiveTrayIcon;
begin
  try
    if Shell_NotifyIcon(NIM_DELETE, @FNotifyIconData) then
    begin
      if FNotifyIconData.Wnd <> 0 then
        Classes.DeallocateHWnd( FNotifyIconData.Wnd );
      FillChar( FNotifyIconData, SizeOf(TNotifyIconData), 0 );
    end;
  finally
    FActive := False;
  end;
end;

end.
