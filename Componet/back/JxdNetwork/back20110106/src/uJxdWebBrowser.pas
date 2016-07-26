unit uJxdWebBrowser;

interface

uses
  Windows, EmbeddedWB, Classes, SHDocVw_EWB, EwbCore, MSHTML_EWB, Menus, uJxdWebPopupMenu, Dialogs;

type
  TxdWebBroser = class( TEmbeddedWB )
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoPopuItemClick(msStyle: TMenuItemStyle; PopuElemt: IHTMLElement);
  private
    FWebPopupMenu: TWebPopuMenu;
    FIsCanGoBack: Boolean;
    FIsCanForward: Boolean;
    procedure WebCommandStateChange(ASender: TObject; Command: Integer; Enable: WordBool);
    procedure WebShowContextMenu(Sender: TCustomEmbeddedWB; const dwID: Cardinal; const ppt: PPoint;
      const CommandTarget: IInterface; const Context: IDispatch; var Result: HRESULT);
    function GetWebPopupMenu: TPopupMenu;
    procedure SetWebPopupMenu(const Value: TPopupMenu);
  published
    property IsCanGoBack: Boolean read FIsCanGoBack;
    property IsCanForward: Boolean read FIsCanForward;
    property WebPopupMenu: TPopupMenu read GetWebPopupMenu write SetWebPopupMenu;
  end;

implementation

uses
  uJxdWebPageInfo;
{ TxdWebBroser }

constructor TxdWebBroser.Create(Owner: TComponent);
begin
  inherited;
  FWebPopupMenu := TWebPopuMenu.Create( Owner, Self );
  OnCommandStateChange := WebCommandStateChange;
  OnShowContextMenu := WebShowContextMenu;
end;

destructor TxdWebBroser.Destroy;
begin
  FWebPopupMenu.Free;
  inherited;
end;

procedure TxdWebBroser.DoPopuItemClick(msStyle: TMenuItemStyle; PopuElemt: IHTMLElement);
begin
  case msStyle of
    msNULL: ;
    msGoForward:     GoForward;
    msGoBack:        GoBack;
    msRefresh:       Refresh;
    msStop:          Stop;
    msSelAll:        SelectAll;
    msViewHTMLCode:  ViewPageSourceHtml;
    msViewTextCode:  ViewPageSourceText;
    msViewPageInfo:  ShowWebPageInfo( Self );
  end;
end;

function TxdWebBroser.GetWebPopupMenu: TPopupMenu;
begin
  Result := PopupMenu;
end;

procedure TxdWebBroser.SetWebPopupMenu(const Value: TPopupMenu);
begin
  PopupMenu := Value;
  FWebPopupMenu.WebPopupMenu := PopupMenu;
  FWebPopupMenu.OnPopupMenuItemClick := DoPopuItemClick;
end;

procedure TxdWebBroser.WebCommandStateChange(ASender: TObject; Command: Integer; Enable: WordBool);
begin
  case Command of
    CSC_NAVIGATEBACK:    FIsCanGoBack := Enable;
    CSC_NAVIGATEFORWARD: FIsCanForward := Enable;
  end;
end;

procedure TxdWebBroser.WebShowContextMenu(Sender: TCustomEmbeddedWB; const dwID: Cardinal; const ppt: PPoint;
  const CommandTarget: IInterface; const Context: IDispatch; var Result: HRESULT);
var
  pt: TPoint;
  elemt: IHTMLElement;
  ppStyle: TIEPopupMenu;
  X, Y: Integer;
begin
  GetCursorPos( pt );
  X := pt.X;
  Y := pt.Y;
  pt := ScreenToClient( pt );
  elemt := Doc2.elementFromPoint( pt.x, pt.Y );
  ppStyle := TIEPopupMenu( dwID );
  if not FWebPopupMenu.CreatePopupMenu(ppStyle, elemt) then
  begin
    Exit;
  end;
  Result := S_OK;
  if Assigned(PopUpMenu) then // Show assigned TPopupMenu
    PopUpMenu.Popup( X, Y );
end;

end.
