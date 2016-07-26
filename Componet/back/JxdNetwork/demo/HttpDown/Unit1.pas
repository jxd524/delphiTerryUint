unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, uJxdStringGrid, uJxdHttpDown, uJxdHttpDownManage, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    sgPlayList: TxdStringGrid;
    dmHttpManage: TxdHttpDownManage;
    tmr1: TTimer;
    btn1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private

    procedure InitDownStrGridInfo;
    procedure ShowDownInfo;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }
//http://61.144.244.246/songlist.php

procedure TForm1.btn1Click(Sender: TObject);
begin
  dmHttpManage.AllTaskStop;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  dmHttpManage.AllTaskStop;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitDownStrGridInfo;
  dmHttpManage.DownFileListFileName := 'E:\Delphi\TerryUnit\Componet\JxdNetwork\demo\HttpDown\bin\xdDownFileList.dat';
//  dmHttpManage.AddHttpDownTask( 'E:\Delphi\TerryUnit\Componet\JxdNetwork\demo\HttpDown\bin\a1.exe',
//    'http://www.17yyt.com/download/setup.exe', '' );
//  dmHttpManage.AddHttpDownTask( 'E:\Delphi\TerryUnit\Componet\JxdNetwork\demo\HttpDown\bin\a2.mp3',
//    'http://media.share.ovi.com/m1/original/0170/bef20bb3611045238f7a7dcb70357b4a.mp3',
//    'http://202.108.23.172/m?word=mp3,http://www.xjtct.com/music/mp3/Y2JjbWZnaWpjYmVobGNpamloMw$$.mp3,,[%CF%C2%D4%D8]&ct=134217728&tn=baidusg,夏天的风%20%20&si=%CF%C4%CC%EC%B5%C4%B7%E7;;%D4%AA%CE%C0%BE%F5%D0%D1;;30903;;30903&lm=16777216&sgid=1' );
  dmHttpManage.AllTaskStart;
end;

procedure TForm1.InitDownStrGridInfo;
var
  nRowCount: Integer;
begin
  nRowCount := 2;
  with sgPlayList do
  begin
    Options := Options + [goColSizing, goRowSelect] - [goRangeSelect];
    RowCount := nRowCount;
    FixedCols := 0;
    FixedRows := 1;
    ColCount := 8;
    Cells[0, 0] := '状态';
    Cells[1, 0] := '文件名';
    Cells[2, 0] := '文件大小';
    Cells[3, 0] := '进度';
    Cells[4, 0] := '速度';
    Cells[5, 0] := '资源';
    Cells[6, 0] := '剩余时间';
    Cells[7, 0] := '已用时间';
    ColWidths[0] := 30;
    ColWidths[1] := 250;
    ColWidths[2] := 80;
    ColWidths[3] := 110;
    ColWidths[4] := 60;
    ColWidths[5] := 60;
    ColWidths[6] := 75;
    ColWidths[7] := 75;
  end;
end;

procedure TForm1.ShowDownInfo;
  function StateToStr(AState: TJxdHttpDownState): string;
  begin
    case AState of
      dsBeginForCheckFile: Result := '验证';
      dsTransmiting:       Result := '传输';
      dsComplete:          Result := '完成' ;
      dsError:             Result := '出错';
      dsPause:             Result := '暂停' ;
      else                 Result := '停止';
    end;
  end;
var
  i: Integer;
  task: TxdHttpDown;
begin
  with sgPlayList do
  begin
    if dmHttpManage.TaskCount > 0 then
      RowCount := dmHttpManage.TaskCount + 1
    else
    begin
      RowCount := 2;
      Cells[0, 1] := '';
      Cells[1, 1] := '';
      Cells[2, 1] := '';
      Cells[3, 1] := '';
      Cells[4, 1] := '';
      Cells[5, 1] := '';
      Cells[6, 1] := '';
      Cells[7, 1] := '';
      Exit;
    end;
    for i := 0 to dmHttpManage.TaskCount - 1 do
    begin
      task := dmHttpManage.FindHttpDownTask( i );
      if task <> nil then
      begin
        Cells[0, i + 1] := StateToStr( task.CurState );
        Cells[1, i + 1] := task.SaveFileName;
        Cells[2, i + 1] := IntToStr( task.FileSize );
        if task.FileSize = 0 then
          Cells[3, i + 1] := ''
        else
          Cells[3, i + 1] := Format( '%0.2f%%', [task.CurSize / task.FileSize * 100] );
        if task.Active then
        begin
          Cells[4, i + 1] := task.AverageSpeed;
          Cells[5, i + 1] := IntToStr( task.HttpSourceCount );
          Cells[6, i + 1] := StateToStr( task.CurState );
          Cells[7, i + 1] := TimeToStr( task.ActiveTime );
        end
        else
        begin
          Cells[4, i + 1] := '';
          Cells[5, i + 1] := IntToStr( task.HttpSourceCount );
          Cells[6, i + 1] := '';
          Cells[7, i + 1] := '';
        end;
      end;
    end;  
  end;
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  ShowDownInfo;
end;

end.
