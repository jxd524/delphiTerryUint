unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uJxdFtpUpTask, StdCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    FFtp: TxdFtpUpTask;
    procedure DoEncryptBuffer(Sender: TObject; var Buffer; Count: Integer);
    procedure EncryptBuf(Buf: PAnsiChar; Len: Integer);
    procedure DoFreeObject(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  if Assigned(FFtp) then
    FFtp.Active := False;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  if Assigned(FFtp) then
    ShowMessage( FFtp.UserName );
end;

procedure TForm1.DoEncryptBuffer(Sender: TObject; var Buffer; Count: Integer);
begin
  EncryptBuf( PChar(@Buffer), Count );
end;

procedure TForm1.DoFreeObject(Sender: TObject);
begin
  if Integer(Sender) = Integer(FFtp) then
    FFtp := nil;
end;

procedure TForm1.EncryptBuf(Buf: PAnsiChar; Len: Integer);
var
  I: Integer;
  P: PByte;
begin
  P := PByte(Buf);
  for I := 0 to Len - 1 do
  begin
    P^ := P^ xor 123;
    Inc(P);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FFtp) then
  begin
    FFtp.Active := False;
    FFtp.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFtp := TxdFtpUpTask.Create;
  with FFtp do
  begin
//    UpDirPath := '';
//    UserName := 'ftpusername';
//    UserPassWord := 'ftppassword';;
//    Host := '192.168.2.102';
//    Port := 22;
    UpFileName := 'E:\CompanyWork\MusicT\KBox2.0\bin\record\153112965868239931.wmv.up';
    OnEncryptBuffer := DoEncryptBuffer;
    OnThreadFreeObject := DoFreeObject;
    Active := True;
  end;
end;

end.
