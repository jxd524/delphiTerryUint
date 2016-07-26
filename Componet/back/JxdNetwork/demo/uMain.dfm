object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 418
  ClientWidth = 578
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
  object xdWebBroser1: TxdWebBroser
    Left = 8
    Top = 8
    Width = 513
    Height = 345
    PopupMenu = pm1
    TabOrder = 0
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    PrintOptions.Margins.Left = 19.050000000000000000
    PrintOptions.Margins.Right = 19.050000000000000000
    PrintOptions.Margins.Top = 19.050000000000000000
    PrintOptions.Margins.Bottom = 19.050000000000000000
    PrintOptions.Header = '&w&b'#39029#30721#65292'&p/&P'
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Footer = '&u&b&d'
    PrintOptions.Orientation = poPortrait
    WebPopupMenu = pm1
    ControlData = {
      4C00000005350000A82300000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object pm1: TPopupMenu
    Left = 528
    Top = 24
  end
end
