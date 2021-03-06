unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uJxdGpBasic, uJxdGpCommon, uJxdGpTabSet, StdCtrls, uNetwork, uKeyBoardMouseEvnet,
  uKeyScanCode, uJxdGpForm, uJxdGpButton;

type
  TForm1 = class(TForm)
    xdgptbst1: TxdGpTabSet;
    btn1: TButton;
    btn2: TButton;
    edt1: TEdit;
    btn3: TButton;
    mmo1: TMemo;
    btn4: TxdButton;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure mmo1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure mmo1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure btn3Click(Sender: TObject);
    procedure mmo1DblClick(Sender: TObject);
  private
    FIndex: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  uRandomInfo, SHA1, Encrypt, uStringHandle, MD5;

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  xdgptbst1.AddTabItem( IntToStr(FIndex), 0 );
  Inc( FIndex );
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  xdgptbst1.DeleteTabItem( StrToInt(edt1.Text) );
end;




procedure TForm1.btn3Click(Sender: TObject);
var
  s: string;
begin
  mmo1.Text := URLDecode(mmo1.Text);
//  mmo1.Text := UnicodeTostr( edt1.Text );
//  GetCacheVerifyCodeFile( edt1.Text, s );
//  ShowMessage( s );
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  s1, s2, s3: string;
begin
 s1 :=  MD5Print( 'm_pLblReg->SetClickEvent( &MakeNotifyEvent(this, &CDlgWebLogin::DoLblClick) );' );
//  s1 := Base64( URLEncode('2238914768@qq.com') );
  ShowMessage( s1 );
  Exit;
  s1 := SHA1DigestToStr( SHA1String( 'asd123' ) );
  //ShowMessage( s1 );
  s2 := SHA1DigestToStr( SHA1String( LowerCase(s1) ) );
  s3 := LowerCase(s2) + '1328789911' + 'CP3FHN';
  s3 := LowerCase( SHA1DigestToStr( SHA1String( s3 ) ));
  ShowMessage( s3 );
  DoubleBuffered := True;
  FIndex := 0;
end;

procedure TForm1.mmo1DblClick(Sender: TObject);
begin
  FocusInput( [KEY_SPACE] );
end;

procedure TForm1.mmo1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
//

end;

procedure TForm1.mmo1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
//
end;

end.
