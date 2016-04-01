object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 437
  ClientWidth = 454
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 312
    Top = 328
    Width = 45
    Height = 13
    Caption = 'VideoCap'
  end
  object btn1: TButton
    Left = 8
    Top = 404
    Width = 75
    Height = 25
    Caption = #24405#21046
    TabOrder = 0
    OnClick = btn1Click
  end
  object mmo1: TMemo
    Left = 104
    Top = 328
    Width = 185
    Height = 101
    Lines.Strings = (
      'mmo1')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btn2: TButton
    Left = 8
    Top = 373
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = btn2Click
  end
  object btn3: TButton
    Left = 328
    Top = 400
    Width = 75
    Height = 25
    Caption = 'ShapShot'
    TabOrder = 3
    OnClick = btn3Click
  end
  object btn4: TButton
    Left = 8
    Top = 342
    Width = 75
    Height = 25
    Caption = #39044#35272
    TabOrder = 4
    OnClick = btn4Click
  end
  object edtVideoCap: TEdit
    Left = 312
    Top = 347
    Width = 112
    Height = 21
    TabOrder = 5
    Text = '1'
  end
  object FCap: TxdRecordCap
    Left = 0
    Top = 0
    Width = 454
    Height = 289
    Align = alTop
    SourceFilterSetting.VideoCapIndex = -1
    SourceFilterSetting.VideoWidth = 320
    SourceFilterSetting.VideoHeight = 240
    SourceFilterSetting.VideoFrame = 25
    SourceFilterSetting.AudioCapIndex = -1
    SourceFilterSetting.AudioCapInPinIndex = -1
    OutPutFilterSetting.AudioBitRate = -1
    OutPutFilterSetting.AudioChannels = 2
    OutPutFilterSetting.VideoBitRate = -1
    OutPutFilterSetting.VideoWidth = 640
    OutPutFilterSetting.VideoHeight = 480
    OutPutFilterSetting.VideoQuality = -1
    OutPutFilterSetting.VideoMaxKeyFrameSpacing = -1
    ExplicitLeft = 8
  end
end
