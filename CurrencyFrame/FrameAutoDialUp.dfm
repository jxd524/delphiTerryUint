object FrmAutoDialUp: TFrmAutoDialUp
  Left = 0
  Top = 0
  Width = 469
  Height = 111
  TabOrder = 0
  TabStop = True
  object JxdGradientPanel2: TJxdGradientPanel
    Left = 0
    Top = 0
    Width = 469
    Height = 111
    RoundRectRgn = False
    RoundRectWidth = 20
    RoundRectHeight = 20
    GradientText = '#Gradient{fromColor: $FFFFFF; ColorTo: $FFFFFF; percent: 40%}'
    GradientWay = gwUpToDown
    FrameLienVisible = True
    FrameLienColor = 16561022
    FrameLienWidth = 1
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 3
    ExplicitTop = 3
    ExplicitWidth = 515
    ExplicitHeight = 110
    object lbl1: TLabel
      Left = 28
      Top = 35
      Width = 40
      Height = 13
      Caption = #29992#25143#21517':'
      Color = clWhite
      ParentColor = False
    end
    object lbl2: TLabel
      Left = 37
      Top = 61
      Width = 31
      Height = 13
      Caption = #23494#30721': '
      Color = clWhite
      ParentColor = False
    end
    object lbl3: TLabel
      Left = 13
      Top = 87
      Width = 55
      Height = 13
      Caption = #37325#25320#39057#29575': '
      Color = clWhite
      ParentColor = False
    end
    object lbl4: TLabel
      Left = 161
      Top = 88
      Width = 12
      Height = 13
      Caption = #27425
      Color = clWhite
      ParentColor = False
    end
    object lbl5: TLabel
      Left = 182
      Top = 87
      Width = 140
      Height = 13
      Caption = '('#20462#25913#25351#23450#25968#25454#21518#37325#26032#25320#21495')'
      Color = clWhite
      ParentColor = False
    end
    object lbl6: TLabel
      Left = 28
      Top = 10
      Width = 40
      Height = 13
      Caption = #36830#25509#21517':'
      Color = clWhite
      ParentColor = False
    end
    object edtUserName: TJxdEdit
      Left = 79
      Top = 32
      Width = 124
      Height = 20
      FrameColor = 12164478
      GreyColor = 13160660
      FrameBorderWidth = 1
      OnlyNumber = False
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13160660
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = '6'
    end
    object edtUserPass: TJxdEdit
      Left = 79
      Top = 58
      Width = 76
      Height = 20
      FrameColor = 12164478
      GreyColor = 13160660
      FrameBorderWidth = 1
      OnlyNumber = False
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13160660
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = '6'
    end
    object chkNetLogin: TCheckBox
      Left = 233
      Top = 9
      Width = 170
      Height = 17
      Caption = #33258#21160#25320#21495'('#32463#36335#30001#29992#25143#19981#36866#29992')'
      Color = clWhite
      ParentColor = False
      TabOrder = 2
      OnClick = chkNetLoginClick
    end
    object cbConnectionName: TJxdComboBox
      Left = 79
      Top = 4
      Width = 145
      Height = 22
      FrameColor = 12164478
      ScrollBkColor = clWhite
      ScrollActiveBkColor = clWhite
      ScrollDownBKColor = clWhite
      ScrollBitmapTransColor = clFuchsia
      ScrollBitmapTopSpace = 0
      ScrollBitmapLeftSpace = 0
      TrigonColor = 12164478
      TrigonLine = 12164478
      Enabled = False
      ItemHeight = 16
      TabOrder = 3
    end
    object edtFrequency: TJxdEdit
      Left = 79
      Top = 84
      Width = 76
      Height = 20
      FrameColor = 12164478
      GreyColor = 13160660
      FrameBorderWidth = 1
      OnlyNumber = False
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13160660
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      Text = '6'
    end
    object btnTestCon: TJxdGradientButton
      Left = 371
      Top = 45
      Width = 80
      Height = 27
      Caption = #27979#35797#36830#25509
      Enabled = False
      TabOrder = 5
      OnClick = btnTestConClick
      GradientNormalText = '#Gradient{fromColor: $FFFFF5; ColorTo: $F5DCB7; percent: 100%}'
      GradientNormalWay = gwUpToDown
      GradientHoverText = 
        '#Gradient{fromColor: $FFFFFF; ColorTo: $FBEED4; percent: 60%}'#13#10'#' +
        'Gradient{fromColor: $FBEED4; ColorTo: $FFFFFF; percent: 60%}'
      GradientHoverWay = gwUpToDown
      GradientMouseDownText = 
        '#Gradient{fromColor: $FFFFFF; ColorTo: $FFFFFF; percent: 10%}'#13#10'#' +
        'Gradient{fromColor: $FAEAD1; ColorTo: $F9E9CB; percent: 60%}'#13#10'#G' +
        'radient{fromColor: $F9E9CB; ColorTo: $FFFFFF; percent: 85%}'
      GradientMouseDownWay = gwUpToDown
      RoundRectRgn = False
      RoundRectWidth = 4
      RoundRectHeight = 4
      FrameLienVisible = True
      FrameLienColor = clGray
      FrameLienWidth = 1
      GrayLeftSpace = 0
      GrayTopSpace = 0
      GrayTransColor = clFuchsia
      FontLeftSpace = 0
      FontTopSpace = 0
    end
    object btnDisCon: TJxdGradientButton
      Left = 285
      Top = 43
      Width = 80
      Height = 27
      Caption = #26029#24320#36830#25509
      Enabled = False
      TabOrder = 6
      Visible = False
      OnClick = btnDisConClick
      GradientNormalText = '#Gradient{fromColor: $FFFFF5; ColorTo: $F5DCB7; percent: 100%}'
      GradientNormalWay = gwUpToDown
      GradientHoverText = 
        '#Gradient{fromColor: $FFFFFF; ColorTo: $FBEED4; percent: 60%}'#13#10'#' +
        'Gradient{fromColor: $FBEED4; ColorTo: $FFFFFF; percent: 60%}'
      GradientHoverWay = gwUpToDown
      GradientMouseDownText = 
        '#Gradient{fromColor: $FFFFFF; ColorTo: $FFFFFF; percent: 10%}'#13#10'#' +
        'Gradient{fromColor: $FAEAD1; ColorTo: $F9E9CB; percent: 60%}'#13#10'#G' +
        'radient{fromColor: $F9E9CB; ColorTo: $FFFFFF; percent: 85%}'
      GradientMouseDownWay = gwUpToDown
      RoundRectRgn = False
      RoundRectWidth = 4
      RoundRectHeight = 4
      FrameLienVisible = True
      FrameLienColor = clGray
      FrameLienWidth = 1
      GrayLeftSpace = 0
      GrayTopSpace = 0
      GrayTransColor = clFuchsia
      FontLeftSpace = 0
      FontTopSpace = 0
    end
    object btnDialup: TJxdGradientButton
      Left = 371
      Top = 78
      Width = 80
      Height = 27
      Caption = #25320#21495
      Enabled = False
      TabOrder = 7
      OnClick = btnDialupClick
      GradientNormalText = '#Gradient{fromColor: $FFFFF5; ColorTo: $F5DCB7; percent: 100%}'
      GradientNormalWay = gwUpToDown
      GradientHoverText = 
        '#Gradient{fromColor: $FFFFFF; ColorTo: $FBEED4; percent: 60%}'#13#10'#' +
        'Gradient{fromColor: $FBEED4; ColorTo: $FFFFFF; percent: 60%}'
      GradientHoverWay = gwUpToDown
      GradientMouseDownText = 
        '#Gradient{fromColor: $FFFFFF; ColorTo: $FFFFFF; percent: 10%}'#13#10'#' +
        'Gradient{fromColor: $FAEAD1; ColorTo: $F9E9CB; percent: 60%}'#13#10'#G' +
        'radient{fromColor: $F9E9CB; ColorTo: $FFFFFF; percent: 85%}'
      GradientMouseDownWay = gwUpToDown
      RoundRectRgn = False
      RoundRectWidth = 4
      RoundRectHeight = 4
      FrameLienVisible = True
      FrameLienColor = clGray
      FrameLienWidth = 1
      GrayLeftSpace = 0
      GrayTopSpace = 0
      GrayTransColor = clFuchsia
      FontLeftSpace = 0
      FontTopSpace = 0
    end
  end
  object JxdDialUp: TJxdDialUp
    LangStrList.Strings = (
      'Connecting to %s...'
      'Verifying username and password...'
      'An error occured while trying to connect to %s.')
  end
end
