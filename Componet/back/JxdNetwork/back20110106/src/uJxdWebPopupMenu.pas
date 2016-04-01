unit uJxdWebPopupMenu;

interface
uses
  Windows, Menus, Messages, Classes, SysUtils, EmbeddedWB, EwbCore, Dialogs, MSHTML_EWB;

type
  TMenuItemStyle = (msNULL = 0, msGoForward, msGoBack, msRefresh, msStop, msSelAll, msViewHTMLCode, msViewTextCode,
                    msViewPageInfo);
  TMenuItemInfo = record
    FCaption: string;
    FTag: TMenuItemStyle;
  end;
  TOnWebPopuMenuItemClick = procedure(msStyle: TMenuItemStyle; PopuElemt: IHTMLElement) of object;
  {$M+}
  TWebPopuMenu = class
  public
    function CreatePopupMenu(const AStyle: TIEPopupMenu; const ACurElemt: IHTMLElement): Boolean;

    constructor Create(Owner: TComponent; Web: TEmbeddedWB);
    destructor  Destroy; override;
  private
    FWeb: TEmbeddedWB;
    FOwner: TComponent;
    FPopuElemt: IHTMLElement;
    FWebPopupMenu: TPopupMenu;
    FOnPopupMenuItemClick: TOnWebPopuMenuItemClick;

    function CreateDefaultPopupMenu: Boolean;
    function CreateSubPopupMenu(const AAry: array of TMenuItemInfo): Boolean;
    function AddMenuItem(const ACaption: string; const ATag: Integer; const AEnable: Boolean; const AParent: TMenuItem): TMenuItem;

    procedure DoMenuItemClick(Sender: TObject);
    procedure DoPopu(Sender: TObject);
    procedure SetWebPopupMenu(const Value: TPopupMenu);
  published
    property WebPopupMenu: TPopupMenu read FWebPopupMenu write SetWebPopupMenu; 
    property OnPopupMenuItemClick: TOnWebPopuMenuItemClick read FOnPopupMenuItemClick write FOnPopupMenuItemClick;
  end;
  {$M-}
implementation

{ TWebPopuMenu }
uses
  uJxdWebBrowser;

const
  CtDefaultMenuItemsInfo: array[0..8] of TMenuItemInfo =
    (
      (FCaption: '前进'; FTag: msGoForward ),
      (FCaption: '后退'; FTag: msGoBack ),
      (FCaption: '刷新'; FTag: msRefresh ),
      (FCaption: '停止'; FTag: msStop ),
      (FCaption: '-';    FTag: msNULL ),
      (FCaption: '全选'; FTag: msSelAll ),
      (FCaption: '查看页面源码'; FTag: msViewHTMLCode ),
      (FCaption: '查看页面文本'; FTag: msViewTextCode ),
      (FCaption: '查看页面信息'; FTag: msViewPageInfo )
    );


function TWebPopuMenu.AddMenuItem(const ACaption: string; const ATag: Integer; const AEnable: Boolean; const AParent: TMenuItem): TMenuItem;
begin
  Result := TMenuItem.Create( FWebPopupMenu );
  with Result do
  begin
    Tag := ATag;
    Caption := ACaption;
    OnClick := DoMenuItemClick;
  end;
  Result.Enabled := AEnable;
  if AParent <> nil then
    AParent.Add( Result )
  else
    FWebPopupMenu.Items.Add( Result );
end;

constructor TWebPopuMenu.Create(Owner: TComponent; Web: TEmbeddedWB);
begin
  FWeb := Web;
  FOwner := Owner;
end;

function TWebPopuMenu.CreateDefaultPopupMenu: Boolean;
begin
  Result := CreateSubPopupMenu( CtDefaultMenuItemsInfo );
end;

function TWebPopuMenu.CreatePopupMenu(const AStyle: TIEPopupMenu; const ACurElemt: IHTMLElement): Boolean;
begin
  Result := False;
  if FWebPopupMenu = nil then Exit;
  FWebPopupMenu.Items.Clear;
  FPopuElemt := ACurElemt;
  case AStyle of
    rcmDefault:   Result := CreateDefaultPopupMenu;
    rcmImage: ;
    rcmControl: ;
    rcmTable: ;
    rcmSelText: ;
    rcmAnchor: ;
    rcmUnKnown: ;
    rcmImageArt: ;
    rcmImgDynSrc: ;
    rcmDebug: ;
    rcmAll: ;
  end;
end;


function TWebPopuMenu.CreateSubPopupMenu(const AAry: array of TMenuItemInfo): Boolean;
var
  i: Integer;
  item: TMenuItemInfo;
  bEnable: Boolean;
begin
  Result := True;
  for i := Low(AAry) to High(AAry) do
  begin
    item := AAry[i];
    try
      case item.FTag of
        msGoForward: bEnable := (FWeb as TxdWebBroser).IsCanForward;
        msGoBack:    bEnable := (FWeb as TxdWebBroser).IsCanGoBack;
        else
          bEnable := True;
      end;
      AddMenuItem( item.FCaption, Integer(item.FTag), bEnable, nil );
    except
      Result := False;
      Break;
    end;
  end;
end;

destructor TWebPopuMenu.Destroy;
begin
  FWebPopupMenu.Free;
  inherited;
end;

procedure TWebPopuMenu.DoMenuItemClick(Sender: TObject);
var
  msStyle: TMenuItemStyle;
  nTag: Integer;
begin
  if Assigned(OnPopupMenuItemClick) then
  begin
    nTag := (Sender as TMenuItem).Tag;
    if (nTag >= Integer(TMenuItemStyle(Low(msStyle)))) and
       (nTag <= Integer(TMenuItemStyle(High(msStyle)))) then
    begin
      msStyle := TMenuItemStyle( nTag );
      OnPopupMenuItemClick( msStyle, FPopuElemt );
    end;
  end;
end;

procedure TWebPopuMenu.DoPopu(Sender: TObject);
begin

end;

procedure TWebPopuMenu.SetWebPopupMenu(const Value: TPopupMenu);
begin
  FWebPopupMenu := Value;
  if FWebPopupMenu <> nil then
    FWebPopupMenu.OnPopup := DoPopu;
end;

end.
