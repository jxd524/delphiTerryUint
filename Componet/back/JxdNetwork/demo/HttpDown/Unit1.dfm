object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 408
  ClientWidth = 785
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object sgPlayList: TxdStringGrid
    Left = 0
    Top = 0
    Width = 785
    Height = 161
    Align = alTop
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Ctl3D = False
    DefaultDrawing = False
    FixedCols = 0
    FixedRows = 1
    ParentCtl3D = False
    ScrollBars = ssNone
    TabOrder = 0
    FixedGradientDrawInfo.GradientNormalText = '#Gradient{fromColor: $F8D752; ColorTo: $E09406; percent: 100%}'
    FixedGradientDrawInfo.GradientHoverText = '#Gradient{fromColor: $FFFFFF; ColorTo: $F9F3F0; percent: 100%}'
    FixedGradientDrawInfo.GradientMouseDownText = '#Gradient{fromColor: $EFE2D9; ColorTo: $F9F3F0; percent: 100%}'
    CellGradientDrawInfo.GradientNormalText = '#Gradient{fromColor: $F7F7F7; ColorTo: $F1E5DD; percent: 100%}'
    CellGradientDrawInfo.GradientHoverText = '#Gradient{fromColor: $FF22FF; ColorTo: $F9F3F0; percent: 100%}'
    CellGradientDrawInfo.GradientMouseDownText = '#Gradient{fromColor: $EFE2D9; ColorTo: $00F3F0; percent: 100%}'
    LineDrawInfo.Count = 0
    LineDrawInfo.Visible = False
  end
  object btn1: TButton
    Left = 8
    Top = 375
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 1
    OnClick = btn1Click
  end
  object dmHttpManage: TxdHttpDownManage
    Left = 376
    Top = 16
  end
  object tmr1: TTimer
    Interval = 500
    OnTimer = tmr1Timer
    Left = 384
    Top = 208
  end
end
