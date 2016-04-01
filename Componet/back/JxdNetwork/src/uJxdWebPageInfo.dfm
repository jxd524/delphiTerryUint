object frmWebPageInfo: TfrmWebPageInfo
  Left = 0
  Top = 0
  Caption = #39029#38754#20449#24687
  ClientHeight = 436
  ClientWidth = 595
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 595
    Height = 436
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 344
    ExplicitTop = 176
    ExplicitWidth = 185
    ExplicitHeight = 41
    DesignSize = (
      595
      436)
    object lbl2: TLabel
      Left = 32
      Top = 72
      Width = 36
      Height = 13
      Caption = #22320#22336#65306
    end
    object lbl3: TLabel
      Left = 32
      Top = 99
      Width = 36
      Height = 13
      Caption = #31867#22411#65306
    end
    object lbl4: TLabel
      Left = 32
      Top = 123
      Width = 36
      Height = 13
      Caption = #32534#30721#65306
    end
    object lbl5: TLabel
      Left = 32
      Top = 196
      Width = 60
      Height = 13
      Caption = #20462#25913#26102#38388#65306
    end
    object lbl6: TLabel
      Left = 32
      Top = 148
      Width = 36
      Height = 13
      Caption = #21327#35758#65306
    end
    object lbl1: TLabel
      Left = 32
      Top = 173
      Width = 36
      Height = 13
      Caption = #22823#23567#65306
    end
    object edtAddrURL: TEdit
      Left = 98
      Top = 71
      Width = 471
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
      Text = 'about:blank'
    end
    object edtStyle: TEdit
      Left = 98
      Top = 98
      Width = 471
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
      Text = 'text/html'
    end
    object edtEncoding: TEdit
      Left = 98
      Top = 123
      Width = 471
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 2
      Text = 'GB2312'
    end
    object edtModiyTime: TEdit
      Left = 98
      Top = 195
      Width = 471
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 3
      Text = '2010'#24180'7'#26376'4'#26085' 10:17:56'
    end
    object edtprotocol: TEdit
      Left = 98
      Top = 148
      Width = 471
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 4
      Text = #25991#26412
    end
    object edtTityle: TEdit
      Left = 18
      Top = 39
      Width = 471
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = True
      ParentFont = False
      ReadOnly = True
      TabOrder = 5
      Text = 'about:blank'
    end
    object edtFileSize: TEdit
      Left = 98
      Top = 173
      Width = 471
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 6
      Text = #25991#26412
    end
    object mmoScripts: TMemo
      Left = 10
      Top = 232
      Width = 572
      Height = 192
      Lines.Strings = (
        'mmoScripts')
      ScrollBars = ssVertical
      TabOrder = 7
    end
  end
end
