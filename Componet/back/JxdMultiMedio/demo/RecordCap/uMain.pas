unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uJxdRecordCap, ExtCtrls, StdCtrls, DirectShow9, uKBoxEffect;

type
  TForm1 = class(TForm)
    btn1: TButton;
    mmo1: TMemo;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    lbl1: TLabel;
    edtVideoCap: TEdit;
    FCap: TxdRecordCap;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
  private
    Effect: TKBoxEffect;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  //源设置
  FCap.SourceFilterSetting.AudioCapIndex := 0;
  FCap.SourceFilterSetting.AudioCapInPinIndex := 0;
  FCap.SourceFilterSetting.VideoCapIndex := StrToInt(edtVideoCap.Text);
  //输出设置
//  FCap.OutPutFilterSetting.VideoBitRate := 100000;
  FCap.StartRecord;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  FCap.Stop;
end;

procedure TForm1.btn3Click(Sender: TObject);
var
  bmp: TBitmap;
begin
  bmp := nil;
  if Effect.SnapShot(bmp) then
    bmp.SaveToFile( 'a.bmp' );
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
  FCap.SourceFilterSetting.VideoCapIndex := StrToInt(edtVideoCap.Text);
  FCap.StartPreview;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Effect := TKBoxEffect.Create;
  Effect.SetLogo( 10, 10, 'logo.bmp' );
  FCap.SetOverlayEffer( Effect );
  mmo1.Text := GetAudioCapFilters + 'OutPin:' + #13#10 + GetAudioCapPins(0, PINDIR_OUTPUT)
                                  + 'InPin:' + #13#10 + GetAudioCapPins(0, PINDIR_INPUT) + #13#10 +
               GetVideoCapFilters;
end;

end.
