unit FrameHtmlEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ComCtrls, ImgList, ToolWin, ExtCtrls, StdCtrls, JxdComboBox, OleCtrls,
  SHDocVw, JxdHtmlEdit, Menus;

type
  TFrmHtmlEdit = class(TFrame)
    ImageListButton: TImageList;
    dlgOpenHtml: TOpenDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    dlgSave: TSaveDialog;
    dlgColor: TColorDialog;
    PopupMenu1: TPopupMenu;
    sdfs1: TMenuItem;
    sfdsfd1: TMenuItem;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    pnlFont: TPanel;
    cbFontName: TJxdComboBox;
    cbFontSize: TJxdComboBox;
    cbReadOnly: TCheckBox;
    Panel2: TPanel;
    htmEdit: TJxdHtmlEdit;
    procedure ToolButton1Click(Sender: TObject);
    procedure cbFontNameChange(Sender: TObject);
    procedure cbFontSizeChange(Sender: TObject);
    procedure cbReadOnlyClick(Sender: TObject);
    procedure htmEditCommandStateChange(ASender: TObject; Command: Integer; Enable: WordBool);
  private
    { Private declarations }
    procedure SetDefaultFontName(const AFontName: string);
    function GetHtmlText: string;
    function GetSimpleText: string;
    procedure SetColor;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SimpleText: string read GetSimpleText;
    property HtmlText: string read GetHtmlText;
  end;

implementation

{$R *.dfm}

{ TFrmHtmlEdit }

procedure TFrmHtmlEdit.cbFontNameChange(Sender: TObject);
begin
  htmEdit.CurFontName := cbFontName.Text;
end;

procedure TFrmHtmlEdit.cbFontSizeChange(Sender: TObject);
begin
  htmEdit.CurFontSize := cbFontSize.ItemIndex + 1;
end;

procedure TFrmHtmlEdit.cbReadOnlyClick(Sender: TObject);
begin
  htmEdit.ReadOnly := not cbReadOnly.Checked;
end;

constructor TFrmHtmlEdit.Create(AOwner: TComponent);
begin
  inherited;
  cbFontName.Items.AddStrings( Screen.Fonts );
  SetDefaultFontName('Times New Roman');
  cbFontSize.ItemIndex := 2;
  if ParentColor then
    SetColor;
end;

function TFrmHtmlEdit.GetHtmlText: string;
begin
  Result := htmEdit.HtmlString;
end;

function TFrmHtmlEdit.GetSimpleText: string;
begin
  Result := htmEdit.SimpleText;
end;

procedure TFrmHtmlEdit.htmEditCommandStateChange(ASender: TObject; Command: Integer;
  Enable: WordBool);
begin
  if Command = -1 then
  begin
    if Assigned(cbReadOnly) then
      cbReadOnly.Checked := not htmEdit.ReadOnly;
  end;
end;

procedure TFrmHtmlEdit.SetColor;
var
  hDCHandle: HDC;
  cBkColor: TColor;
begin
  hDCHandle := GetDC( ParentWindow );
  cBkColor := GetBkColor(hDCHandle);
  ReleaseDC( ParentWindow, hDCHandle );
  ToolBar1.DrawingStyle := TTBDrawingStyle(dsGradient);
  ToolBar1.GradientEndColor := cBkColor;
  ToolBar1.GradientStartColor := cBkColor;
  pnlFont.Color := cBkColor;
end;

procedure TFrmHtmlEdit.SetDefaultFontName(const AFontName: string);
var
  i: Integer;
begin
  for i := cbFontName.Items.Count - 1 downto 0 do
  begin
    if CompareText(cbFontName.Items.Strings[i], AFontName) = 0 then
    begin
      cbFontName.ItemIndex := i;
      Break;
    end;
  end;
end;

procedure TFrmHtmlEdit.ToolButton1Click(Sender: TObject);
var
  nTag: Integer;
begin
  if not (Sender is TToolButton) then Exit;
  nTag := (Sender as TToolButton).Tag;
  case nTag of
    0:  //载入HTML文件
    begin
      if dlgOpenHtml.Execute then
        htmEdit.LoadFromFile( dlgOpenHtml.FileName );
    end;
    1:  //保存HTML文件
    begin
      if dlgSave.Execute then
        htmEdit.SaveToFile(dlgSave.FileName);
    end;
    2: htmEdit.Delete;
    3: htmEdit.Cut;
    4: htmEdit.Copy;
    5: htmEdit.Paste;
    6: htmEdit.CurFontBold := not htmEdit.CurFontBold;
    7: htmEdit.CurFontItalic := not htmEdit.CurFontItalic;
    8: htmEdit.CurUnderLine := not htmEdit.CurUnderLine;
    9:
    begin
      dlgColor.Color := htmEdit.CurFontColor;
      if dlgColor.Execute then
        htmEdit.CurFontColor := dlgColor.Color;
    end;
    10:
    begin
      dlgColor.Color := htmEdit.CurBackColor;
      if dlgColor.Execute then
        htmEdit.CurBackColor := dlgColor.Color;
    end;
    11: htmEdit.CurJustify := tyLeft;
    12: htmEdit.CurJustify := tyCenter;
    13: htmEdit.CurJustify := tyRight;
    14: htmEdit.InsertOrderedList := not htmEdit.InsertOrderedList;
    15: htmEdit.InsertImage;
    16: htmEdit.SetURL;
    17: htmEdit.PrintPreview;
    18: htmEdit.Print(False);
  end;
end;

end.
