unit uJxdWebPageInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, uJxdWebBrowser, MSHTML_EWB;

type
  TfrmWebPageInfo = class(TForm)
    pnl1: TPanel;
    lbl2: TLabel;
    edtAddrURL: TEdit;
    edtStyle: TEdit;
    lbl3: TLabel;
    lbl4: TLabel;
    edtEncoding: TEdit;
    edtModiyTime: TEdit;
    lbl5: TLabel;
    lbl6: TLabel;
    edtprotocol: TEdit;
    edtTityle: TEdit;
    lbl1: TLabel;
    edtFileSize: TEdit;
    mmoScripts: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure ShowWebScript;
  public
    OwnerWeb: TxdWebBroser;
    procedure UpdateWebPageInfo;
  end;

procedure ShowWebPageInfo(const AWeb: TxdWebBroser);

implementation

var
  frmWebPageInfo: TfrmWebPageInfo = nil;

{$R *.dfm}
procedure ShowWebPageInfo(const AWeb: TxdWebBroser);
begin
  if frmWebPageInfo = nil then
  begin
    frmWebPageInfo := TfrmWebPageInfo.Create( nil );
    frmWebPageInfo.FormStyle := fsStayOnTop;
  end;
  with frmWebPageInfo do
  begin
    OwnerWeb := AWeb;
    UpdateWebPageInfo;
    Show;
  end;
end;

{ TfrmWebPageInfo }

procedure TfrmWebPageInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  frmWebPageInfo.Free;
  frmWebPageInfo := nil;
end;

procedure TfrmWebPageInfo.ShowWebScript;
var
  i, nIndex: Integer;
  index: OleVariant;
  pDispOption: IDispatch;
  pElemt: IHTMLScriptElement;
  strText, strType: string;
begin
  mmoScripts.Clear;
  nIndex := 1;
  for i := 0 to OwnerWeb.Doc2.scripts.length - 1 do
  begin
    index := i;
    pDispOption := OwnerWeb.Doc2.scripts.item( index, index );
    if pDispOption <> nil then
    begin
      if Succeeded(pDispOption.QueryInterface(IID_IHTMLScriptElement, pElemt)) then
      begin
        strText := pElemt.text;
        if strText <> '' then
        begin
          strType := pElemt.type_;
          if strType = '' then
            strType := 'NULL';
          strText := '第 ' + IntToStr(nIndex) + ' 脚本，类型：' + strType + ' 源码：' + #13#10 +
                     strText + #13#10;
          mmoScripts.Lines.Add( strText );
          pElemt := nil;
          Inc( nIndex );
        end;
      end;
    end;
  end;
end;

procedure TfrmWebPageInfo.UpdateWebPageInfo;
begin
  with OwnerWeb do
  begin
    Caption := '页信息 - ' + LocationURL;
    edtTityle.Text := OwnerWeb.Doc2.title;
    edtAddrURL.Text := LocationURL;
    edtEncoding.Text := UpperCase( CharactersSet );
    edtprotocol.Text := OwnerWeb.Doc2.protocol;
    edtModiyTime.Text := OwnerWeb.Doc2.lastModified;
    edtFileSize.Text := OwnerWeb.Doc2.fileSize + ' 字节';
    ShowWebScript;
  end;
end;

end.
