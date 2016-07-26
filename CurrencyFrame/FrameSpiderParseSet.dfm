object FrmSpiderParseSet: TFrmSpiderParseSet
  Left = 0
  Top = 0
  Width = 576
  Height = 550
  TabOrder = 0
  TabStop = True
  object pnlAllLeft: TPanel
    Left = 0
    Top = 0
    Width = 131
    Height = 550
    Align = alLeft
    TabOrder = 0
    DesignSize = (
      131
      550)
    object tvSpiderTask: TTreeView
      Left = 4
      Top = 3
      Width = 124
      Height = 516
      Anchors = [akLeft, akTop, akRight, akBottom]
      Ctl3D = False
      Indent = 19
      ParentCtl3D = False
      RightClickSelect = True
      ShowLines = False
      ShowRoot = False
      TabOrder = 0
      OnChange = tvSpiderTaskChange
      OnCollapsing = tvSpiderTaskCollapsing
      OnEdited = tvSpiderTaskEdited
      OnEditing = tvSpiderTaskEditing
      Items.NodeData = {
        0101000000210000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        0004FB4EA15217526888}
    end
    object btnDelTask: TButton
      Left = 3
      Top = 522
      Width = 61
      Height = 25
      Caption = #21024#38500
      TabOrder = 1
      OnClick = btnDelTaskClick
    end
    object btnAdd: TButton
      Left = 65
      Top = 522
      Width = 63
      Height = 25
      Caption = #28155#21152
      TabOrder = 2
      OnClick = btnAddClick
    end
  end
  object pnlAllClient: TPanel
    Left = 131
    Top = 0
    Width = 445
    Height = 550
    Align = alClient
    TabOrder = 1
    object pnlClient: TPanel
      Left = 1
      Top = 401
      Width = 443
      Height = 148
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitTop = 406
      ExplicitHeight = 143
      object sgFieldInfo: TStringGrid
        Left = 0
        Top = 41
        Width = 443
        Height = 107
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        ColCount = 7
        Ctl3D = False
        DefaultRowHeight = 20
        FixedColor = clInactiveBorder
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
        ParentCtl3D = False
        TabOrder = 1
        OnClick = sgFieldInfoClick
        ExplicitHeight = 102
        ColWidths = (
          65
          72
          63
          62
          62
          61
          49)
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 443
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object btnAddField: TButton
          Left = 333
          Top = 10
          Width = 75
          Height = 25
          Caption = #28155#21152
          TabOrder = 1
          OnClick = btnAddFieldClick
        end
        object btnDel: TButton
          Left = 241
          Top = 10
          Width = 75
          Height = 25
          Caption = #21024#38500
          TabOrder = 0
          OnClick = btnDelClick
        end
      end
    end
    object pnlTop: TPanel
      Left = 1
      Top = 36
      Width = 443
      Height = 365
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 15
        Top = 32
        Width = 60
        Height = 13
        Caption = #33539#22260#21069#26631#35782
      end
      object Label2: TLabel
        Left = 214
        Top = 32
        Width = 60
        Height = 13
        Caption = #33539#22260#21518#26631#35782
      end
      object GroupBox1: TGroupBox
        Left = 3
        Top = 56
        Width = 406
        Height = 129
        Caption = #23383#27573#35774#32622
        TabOrder = 3
        object Label3: TLabel
          Left = 12
          Top = 47
          Width = 36
          Height = 13
          Caption = #21069#26631#35782
        end
        object Label4: TLabel
          Left = 12
          Top = 74
          Width = 36
          Height = 13
          Caption = #21518#26631#35782
        end
        object Label5: TLabel
          Left = 12
          Top = 101
          Width = 48
          Height = 13
          Caption = #21024#38500#26041#21521
        end
        object Label6: TLabel
          Left = 12
          Top = 20
          Width = 48
          Height = 13
          Caption = #23383#27573#21517#31216
        end
        object lbl1: TLabel
          Left = 223
          Top = 20
          Width = 48
          Height = 13
          Caption = #23383#27573#26631#35782
        end
        object edtFS: TEdit
          Left = 67
          Top = 43
          Width = 211
          Height = 21
          TabOrder = 2
        end
        object edtBS: TEdit
          Left = 67
          Top = 70
          Width = 211
          Height = 21
          TabOrder = 4
        end
        object chkEdgeVisible: TCheckBox
          Left = 284
          Top = 50
          Width = 119
          Height = 31
          Caption = #26159#21542#21024#38500#33719#21462#23383#27573#24038#21491#19981#21487#35265#23383#31526
          Checked = True
          State = cbChecked
          TabOrder = 3
          WordWrap = True
        end
        object cbbDelWay: TComboBox
          Left = 66
          Top = 98
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 5
          Text = #21024#38500#21069#38754
          Items.Strings = (
            #21024#38500#21069#38754
            #21024#38500#21518#38754
            #19981#21024#38500)
        end
        object edtFieldName: TEdit
          Left = 66
          Top = 16
          Width = 144
          Height = 21
          TabOrder = 0
        end
        object edtTag: TEdit
          Left = 277
          Top = 16
          Width = 119
          Height = 21
          TabOrder = 1
        end
      end
      object chkBigContent: TCheckBox
        Left = 15
        Top = 3
        Width = 142
        Height = 17
        Caption = #25351#23450#35201#33719#21462#20869#23481#30340#33539#22260
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chkBigContentClick
      end
      object edtBigFS: TEdit
        Tag = 1
        Left = 81
        Top = 29
        Width = 121
        Height = 21
        TabOrder = 1
        OnChange = edtURLChange
      end
      object edtBigBS: TEdit
        Tag = 2
        Left = 280
        Top = 29
        Width = 121
        Height = 21
        TabOrder = 2
        OnChange = edtURLChange
      end
      object gbFieldContent: TGroupBox
        Left = 3
        Top = 191
        Width = 407
        Height = 168
        Caption = #33719#21462#23383#27573#20869#23481#22788#29702
        TabOrder = 4
        DesignSize = (
          407
          168)
        object rbNone: TRadioButton
          Left = 31
          Top = 24
          Width = 85
          Height = 17
          Caption = #19981#38656#35201#22788#29702
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = rbNoneClick
        end
        object rbReplaceString: TRadioButton
          Left = 122
          Top = 24
          Width = 72
          Height = 17
          Caption = #23383#31526#26367#25442
          TabOrder = 1
          OnClick = rbNoneClick
        end
        object rbDelString: TRadioButton
          Left = 200
          Top = 24
          Width = 76
          Height = 17
          Caption = #23383#31526#21024#38500
          TabOrder = 2
          OnClick = rbNoneClick
        end
        object rbJs: TRadioButton
          Left = 282
          Top = 24
          Width = 76
          Height = 17
          Caption = #20351#29992'JS'#22788#29702
          TabOrder = 3
          OnClick = rbNoneClick
        end
        object pnlTotal: TPanel
          Left = 2
          Top = 45
          Width = 403
          Height = 114
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelOuter = bvNone
          TabOrder = 4
          ExplicitHeight = 122
          object pnlReplace: TPanel
            Left = 0
            Top = 0
            Width = 403
            Height = 122
            Align = alTop
            BevelOuter = bvLowered
            TabOrder = 0
            object Label8: TLabel
              Left = 6
              Top = 19
              Width = 24
              Height = 13
              Caption = #21407#20540
            end
            object Label9: TLabel
              Left = 166
              Top = 19
              Width = 24
              Height = 13
              Caption = #26032#20540
            end
            object Label10: TLabel
              Left = 328
              Top = 19
              Width = 24
              Height = 13
              Caption = #27425#25968
            end
            object btnAddReplace: TButton
              Left = 310
              Top = 43
              Width = 75
              Height = 25
              Caption = #28155#21152
              TabOrder = 4
              OnClick = btnAddReplaceClick
            end
            object edtOldValue: TEdit
              Left = 36
              Top = 16
              Width = 118
              Height = 21
              TabOrder = 0
            end
            object edtNewValue: TEdit
              Left = 197
              Top = 16
              Width = 114
              Height = 21
              TabOrder = 1
            end
            object edtReplaceCount: TEdit
              Left = 359
              Top = 16
              Width = 37
              Height = 21
              TabOrder = 2
            end
            object btnClearReplace: TButton
              Left = 229
              Top = 43
              Width = 75
              Height = 25
              Caption = #28165#31354
              TabOrder = 3
              OnClick = btnClearReplaceClick
            end
            object lstReplace: TListBox
              Left = 6
              Top = 71
              Width = 391
              Height = 47
              Ctl3D = False
              ItemHeight = 13
              ParentCtl3D = False
              TabOrder = 5
              OnClick = lstReplaceClick
            end
          end
          object pnlDelString: TPanel
            Left = 0
            Top = 122
            Width = 403
            Height = 0
            Align = alTop
            BevelOuter = bvLowered
            TabOrder = 2
            object Label7: TLabel
              Left = 198
              Top = 10
              Width = 60
              Height = 13
              Caption = #21024#38500#23383#31526#25968
            end
            object Edit4: TEdit
              Left = 264
              Top = 7
              Width = 58
              Height = 21
              TabOrder = 2
              Text = 'Edit4'
            end
            object RadioButton5: TRadioButton
              Left = 17
              Top = 8
              Width = 70
              Height = 17
              Caption = #21024#38500#24320#22836
              TabOrder = 0
            end
            object RadioButton6: TRadioButton
              Left = 93
              Top = 8
              Width = 67
              Height = 17
              Caption = #21024#38500#21518#38754
              TabOrder = 1
            end
            object ListBox2: TListBox
              Left = 6
              Top = 60
              Width = 391
              Height = 58
              Ctl3D = False
              ItemHeight = 13
              Items.Strings = (
                '131323'
                'dfgdfg'
                '123123'
                'dfgdfgdfg')
              ParentCtl3D = False
              TabOrder = 5
            end
            object Button2: TButton
              Left = 310
              Top = 31
              Width = 75
              Height = 25
              Caption = #28155#21152
              TabOrder = 4
            end
            object Button1: TButton
              Left = 229
              Top = 31
              Width = 75
              Height = 25
              Caption = #28155#21152
              TabOrder = 3
            end
          end
          object pnlJs: TPanel
            Left = 0
            Top = 122
            Width = 403
            Height = 0
            Align = alTop
            BevelOuter = bvLowered
            TabOrder = 1
            object Label11: TLabel
              Left = 10
              Top = 16
              Width = 47
              Height = 13
              Caption = 'JS'#25991#20214#21517
            end
            object Label12: TLabel
              Left = 13
              Top = 43
              Width = 47
              Height = 13
              Caption = 'JS'#20989#25968#21517
            end
            object Label13: TLabel
              Left = 13
              Top = 68
              Width = 48
              Height = 13
              Caption = #35821#35328#36873#25321
            end
            object Edit5: TEdit
              Left = 66
              Top = 12
              Width = 287
              Height = 21
              TabOrder = 0
              Text = 'Edit5'
            end
            object Edit6: TEdit
              Left = 69
              Top = 39
              Width = 128
              Height = 21
              TabOrder = 1
              Text = 'Edit5'
            end
            object RadioButton7: TRadioButton
              Left = 74
              Top = 68
              Width = 73
              Height = 17
              Caption = 'JavaScript'
              TabOrder = 2
            end
            object RadioButton8: TRadioButton
              Left = 167
              Top = 68
              Width = 73
              Height = 17
              Caption = 'VBScript'
              TabOrder = 3
            end
          end
        end
      end
    end
    object pnlURL: TPanel
      Left = 1
      Top = 1
      Width = 443
      Height = 35
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label14: TLabel
        Left = 14
        Top = 14
        Width = 48
        Height = 13
        Caption = #22320#22336#35268#21017
      end
      object edtURL: TEdit
        Left = 68
        Top = 11
        Width = 333
        Height = 21
        TabOrder = 0
        OnChange = edtURLChange
      end
    end
  end
end
