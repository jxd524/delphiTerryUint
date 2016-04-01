unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uJxdGraphicBaseClass, uJxdButton, ExtCtrls, uJxdCustomPanel, uJxdPanel, uJxdCustomScrollBar, uJxdScrollBar,
  uJxdProgressBar, StdCtrls, Grids, uJxdStringGrid, ComCtrls;

type
  TForm1 = class(TForm)
    xdButton1: TxdButton;
    xdPanel1: TxdPanel;
    xdScrollBar1: TxdScrollBar;
    xdButton2: TxdButton;
    pbPlayPosition: TxdProgressBar;
    mmo1: TMemo;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    pnl1: TPanel;
    xdStringGrid1: TxdStringGrid;
    xdScrollBar2: TxdScrollBar;
    xdScrollBar3: TxdScrollBar;
    btn4: TButton;
    pbSound: TxdProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure xdButton1Click(Sender: TObject);
    procedure xdButton2Click(Sender: TObject);
    procedure xdScrollBar1PositionChanged(Sender: TObject; const IsChangedBySelf: Boolean);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
  private
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  xdStringGrid1.RowCount := xdStringGrid1.RowCount + 1;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  xdStringGrid1.RowCount := xdStringGrid1.RowCount - 1;
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  mmo1.Lines.Add( IntToStr(xdStringGrid1.TopRow) + ' visibleCount:' + IntToStr(xdStringGrid1.VisibleRowCount) +
                  ' Count: ' + IntToStr(xdStringGrid1.RowCount) );
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i, j: Integer;
begin
  for i := 0 to xdStringGrid1.RowCount - 1 do
  begin
    for j := 0 to xdStringGrid1.ColCount - 1 do
    begin
      xdStringGrid1.Cells[ j, i ] := IntToStr( j + i * xdStringGrid1.RowCount );
    end;
  end;

  xdButton1.ButtonLines.Item[0]^.FColor := $ff00ff;
  xdPanel1.Lines.Item[0]^.FColor := $00FF00;
  xdPanel1.Lines.Item[0]^.FWidth := 2;

  xdStringGrid1.LineDrawInfo.Count := 1;
  xdStringGrid1.LineDrawInfo.Item[0]^.FColor := $ff00ff;
  xdStringGrid1.LineDrawInfo.Item[0]^.FColor := $00FF00;
  xdStringGrid1.LineDrawInfo.Item[0]^.FWidth := 2;
//  xdStringGrid1.LineDrawInfo.Item[0]^.FRect := Rect(0, 0, 100, 100);
//  xdStringGrid1.LineDrawInfo.Visible := True;

  pnl1.DoubleBuffered := True;
  xdStringGrid1.RowCount := 66;
  xdStringGrid1.ScrollBarHorizontal := xdScrollBar2;
//  xdStringGrid1.ScrollBarHorizontalBitmapFileName := '滚动条2.BMP';
end;

procedure TForm1.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

procedure TForm1.xdButton1Click(Sender: TObject);
begin
  xdScrollBar1.Position := xdScrollBar1.Position + 1;
end;

procedure TForm1.xdButton2Click(Sender: TObject);
begin
  xdScrollBar1.Position := xdScrollBar1.Position - 1;
end;

procedure TForm1.xdScrollBar1PositionChanged(Sender: TObject; const IsChangedBySelf: Boolean);
var
  strText: string;
begin
  if IsChangedBySelf then
    strText := '自改变位置 '
  else
    strText := '手工改变位置 ';

  strText := strText + 'CurPos: ' + IntToStr(xdScrollBar1.Position);
  mmo1.Lines.Add( strText );
end;

end.
