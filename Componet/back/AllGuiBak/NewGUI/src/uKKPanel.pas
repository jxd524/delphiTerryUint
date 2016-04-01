unit uKKPanel;

interface
uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Messages, uKKCustomPanel, Graphics, uKKGuiDef, uKKBitmapHandle;

type
  TKKPanelStype = ( psKKMailForm1, psKKSmallForm1, psKKPlayBackgroud1, psKKPgaeBackGroud1, psKKMtvSearchGackGroud1,
                    pskKKListBackGroud1 );
  TKKPanel = class( TKKCustomPanel )
  private
    procedure DrawSysButton(ACanvas: TCanvas; AResOrgX, AResOrgY: Integer; AResBmp: TBitmap);

    procedure DrawKKSmallForm1(ACanvas: TCanvas);
    procedure DrawKKMainForm1(ACanvas: TCanvas);
    procedure DrawPlayBackgroud1(ACanvas: TCanvas);
    procedure DrawKListPageBackGroud1(ACanvas: TCanvas);
    procedure DrawMtvSearch1(ACanvas: TCanvas);
    procedure DrawKKListBackGroud1(ACanvas: TCanvas);
  private
    FResBmp: TBitmap;
    FPanelStype: TKKPanelStype;
    FSysButton: Boolean;
    FSysButtonLength: Integer;
    FBorder: Boolean;
    FSysButtonWidth: Integer;
    FSysButtonTop: Integer;
    FSysLineColor: TColor;
    FChangeColor: TChangeToColor;
    procedure SetPanelStype(const Value: TKKPanelStype);
    procedure LoadResourceBmp;
    procedure FreeResourceBmp;
    procedure SetSysButton(const Value: Boolean);
    procedure SetSysButtonLength(const Value: Integer);
    procedure SetBorder(const Value: Boolean);
    procedure SetSysButtonWidth(const Value: Integer);
    procedure SetSysButtonTop(const Value: Integer);
    procedure SetSysLineColor(const Value: TColor);
  protected
    procedure DrawPanel(ABufBitmap: TBitmap); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property ChangeColor: TChangeToColor read FChangeColor write FChangeColor;
    property PanelStype: TKKPanelStype read FPanelStype write SetPanelStype;
    property SysLineColor: TColor read FSysLineColor write SEtSysLineColor;
    property SysButton: Boolean read FSysButton write SetSysButton default True;
    property SysButtonTop: Integer read FSysButtonTop write SetSysButtonTop default 5;
    property SysButtonWidth: Integer read FSysButtonWidth write SetSysButtonWidth;
    property SysButtonLength: Integer read FSysButtonLength write SetSysButtonLength;
    property Border: Boolean read FBorder write SetBorder default False;
  end;

implementation

{ TKKPanel }
{$R ..\res\KKPanel.RES}

constructor TKKPanel.Create(AOwner: TComponent);
begin
  inherited;
  FResBmp := nil;
  FSysButtonLength := 150;
  FSysButton := True;
  FBorder := False;
  FSysButtonTop := 5;
  FSysButtonWidth := 12;
  FSysLineColor := $005C5C5C;
  FChangeColor := TChangeToColor.Create( Self );
end;

procedure TKKPanel.DrawPanel(ABufBitmap: TBitmap);
begin
  case FPanelStype of
    psKKSmallForm1:     DrawKKSmallForm1( ABufBitmap.Canvas );
    psKKMailForm1:      DrawKKMainForm1( ABufBitmap.Canvas );
    psKKPlayBackgroud1: DrawPlayBackgroud1( ABufBitmap.Canvas );
    psKKPgaeBackGroud1: DrawKListPageBackGroud1( ABufBitmap.Canvas );
    psKKMtvSearchGackGroud1: DrawMtvSearch1( ABufBitmap.Canvas );
    pskKKListBackGroud1: DrawKKListBackGroud1( ABufBitmap.Canvas );
  end;
end;

procedure TKKPanel.DrawPlayBackgroud1(ACanvas: TCanvas);
var
  BmpRect, ResRect: TRect;
begin
  LoadResourceBmp;
  try
    ResRect := Rect( 0, 0, 5, 71 );
    BmpRect := ClientRect;
    DrawRectangle( ACanvas, BmpRect, ResRect, FResBmp );

    ResRect := Rect( 6, 0, 53, 29 );
    BmpRect := Rect( 69, Height - ( ResRect.Bottom - ResRect.Top ) - 3,
                     69 + FSysButtonLength + ( ResRect.Bottom - ResRect.Top ), Height - 3 );
    DrawRectangle( ACanvas, BmpRect, ResRect, FResBmp );
  finally
    FreeResourceBmp;
  end;
end;

destructor TKKPanel.Destroy;
begin
  FChangeColor.Free;
  inherited;
end;

procedure TKKPanel.DrawKKListBackGroud1(ACanvas: TCanvas);
var
  DesR, SrcR: TRect;
begin
  LoadResourceBmp;
  try
    SrcR := Rect(0, 0, 9, 4);
    DesR := ClientRect;
    DesR.Bottom := DesR.Top + HeightOfRect(SrcR);
    DrawRectangle(ACanvas, FResBmp.Canvas, DesR, SrcR, False);

    SrcR := Rect(0, 4, 9, 5);
    DesR.Top := DesR.Bottom;
    DesR.Bottom := Height - 5;
    DrawRectangle(ACanvas, FResBmp.Canvas, DesR, SrcR, False);

    SrcR := Rect(0, 4, 9, 9);
    DesR.Top := DesR.Bottom;
    DesR.Bottom := Height;
    DrawRectangle(ACanvas, FResBmp.Canvas, DesR, SrcR, False);
  finally
    FreeResourceBmp;
  end;
end;

procedure TKKPanel.DrawKKMainForm1(ACanvas: TCanvas);
var
  BmpRect, ResRect: TRect;
begin
  LoadResourceBmp;
  try
    //上
    ResRect := Rect( 0, 0, 28, 37);
    BmpRect := ClientRect;
    BmpRect.Bottom := BmpRect.Top + ( ResRect.Bottom - ResRect.Top );
    DrawRectangle( ACanvas, BmpRect, ResRect, FResBmp );
    DrawSysButton( ACanvas, 28, 0, FResBmp );

    //中
    if not ChangeColor.IsChange then
      ACanvas.Brush.Color := 0
    else
      ACanvas.Brush.Color := ChangeColor.ChangeToColor;
    BmpRect.Top := BmpRect.Bottom;
    BmpRect.Bottom := Height - 18;
    ACanvas.FillRect( BmpRect );

    //下
    BmpRect.Top := BmpRect.Bottom;
    BmpRect.Bottom := Height;
    ResRect := Rect( 0, 37, 29, 55 );
    DrawRectangle( ACanvas, BmpRect, ResRect, FResBmp );

    //两边边框
    ResRect := Rect( 29, 32, 37, 35 );
    BmpRect := Rect( 1, 36, 1 + ResRect.Right - ResRect.Left, Height - 18);
    ACanvas.CopyRect( BmpRect, FResBmp.Canvas, ResRect );
    OffsetRect( BmpRect, Width - ( ResRect.Right - ResRect.Left ) - 2, 0 );
    ACanvas.CopyRect( BmpRect, FResBmp.Canvas, ResRect );

    //分布背景
    ResRect := Rect( 74, 0, 88, 59 );
    BmpRect := Rect( 14, 42, Width - 313, 42 + 59 );
    DrawRectangle( ACanvas, BmpRect, ResRect, FResBmp );
    //
    ResRect := Rect( 96, 0, 143, 65 );
    with BmpRect do
    begin
      Left   := Width - 250 - 11;
      Top    := 38;
      Right  := Left + ResRect.Right - ResRect.Left;
      Bottom := 38 + 64;
    end;
    ACanvas.CopyRect( BmpRect, FResBmp.Canvas, ResRect );

    ResRect.Left := ResRect.Right;
    ResRect.Right := ResRect.Right + 1;
    BmpRect.Left := BmpRect.Right;
    BmpRect.Right := BmpRect.Left + 201;
    ACanvas.CopyRect( BmpRect, FResBmp.Canvas, ResRect );

    ResRect.Left := ResRect.Right;
    ResRect.Right := 147;
    BmpRect.Left := BmpRect.Right;
    BmpRect.Right := BmpRect.Left + 3;
    ACanvas.CopyRect( BmpRect, FResBmp.Canvas, ResRect );
    //线条
    ResRect := Rect( 0, 59, 94, 62);
    BmpRect := Rect( 11, 104, Width - 11, 104 + 3 );
    DrawRectangle( ACanvas, BmpRect, ResRect, FResBmp );
  finally
    FreeResourceBmp;
  end;
end;

procedure TKKPanel.DrawKKSmallForm1(ACanvas: TCanvas);
var
  BmpRect, ResRect: TRect;
begin
  LoadResourceBmp;
  try
    ResRect := Rect( 0, 0, 9, 43);
    BmpRect := ClientRect;
    BmpRect.Bottom := BmpRect.Top + ( ResRect.Bottom - ResRect.Top );
    DrawRectangle( ACanvas, BmpRect, ResRect, FResBmp );
    DrawSysButton( ACanvas, 18, 0, FResBmp );

    if not ChangeColor.IsChange then
      ACanvas.Brush.Color := $001B1B1B
    else
      ACanvas.Brush.Color := ChangeColor.ChangeToColor;
    BmpRect.Top := BmpRect.Bottom;
    BmpRect.Bottom := Height - 43;
    ACanvas.FillRect( BmpRect );

    BmpRect.Top := BmpRect.Bottom;
    BmpRect.Bottom := Height;
    ResRect := Rect( 9, 0, 18, 43 );
    DrawRectangle( ACanvas, BmpRect, ResRect, FResBmp );

    if Border then
    begin
      ResRect := Rect( 64, 0, 72, 24 );
      BmpRect := Rect( 1, 27, 1 + ResRect.Right - ResRect.Left, Height - 8);
      ACanvas.CopyRect( BmpRect, FResBmp.Canvas, ResRect );
      OffsetRect( BmpRect, Width - ( ResRect.Right - ResRect.Left ) - 2, 0 );
      ACanvas.CopyRect( BmpRect, FResBmp.Canvas, ResRect );
    end;
  finally
    FreeResourceBmp;
  end;
end;

procedure TKKPanel.DrawKListPageBackGroud1(ACanvas: TCanvas);
var
  BmpRect, ResRect: TRect;
begin
  LoadResourceBmp;
  try
    ResRect := Rect( 0, 0, 20, 62 );
    BmpRect := ClientRect;
    DrawRectangle( ACanvas, BmpRect, ResRect, FResBmp );
  finally
    FreeResourceBmp;
  end;
end;

procedure TKKPanel.DrawMtvSearch1(ACanvas: TCanvas);
var
  DesR, SrcR: TRect;
begin
  LoadResourceBmp;
  try
    SrcR := Rect( 0, 0, 9, 45 );
    DesR := ClientRect;
    DrawRectangle( ACanvas, FResBmp.Canvas, DesR, SrcR, False );
  finally
    FreeResourceBmp;
  end;
end;

procedure TKKPanel.DrawSysButton(ACanvas: TCanvas; AResOrgX, AResOrgY: Integer; AResBmp: TBitmap);
const
  CtButtonBorderWidth = 23;
  CtButtonBorderHeight = 31;
var
  BmpRect, ResRect: TRect;
  nTmpHeigth, nTmpWidth, nTmp: Integer;
  nLineColor: TColor;
begin
  if FSysButton and (FSysButtonLength > 0) then //拥有系统按钮
  begin
    nTmpWidth := CtButtonBorderWidth * 2 + FSysButtonLength;
    nTmpHeigth := CtButtonBorderHeight;
    with BmpRect do
    begin
      Left   := Width - nTmpWidth - 10;
      Top    := 1;
      Right  := Left + nTmpWidth;
      Bottom := Top + nTmpHeigth;
    end;
    nTmp := BmpRect.Right; //保存
    //按钮背景
    //左边
    ResRect := Rect(AResOrgX, AResOrgY, AResOrgX + CtButtonBorderWidth, CtButtonBorderHeight );
    BmpRect.Right := BmpRect.Left + ResRect.Right - ResRect.Left;
    ACanvas.CopyRect( BmpRect, AResBmp.Canvas, ResRect );
    //中间
    BmpRect.Left := BmpRect.Right;
    BmpRect.Right := BmpRect.Left + FSysButtonLength;
    ResRect := Rect( AResOrgX + CtButtonBorderWidth - 1, AResOrgY, AResOrgX + CtButtonBorderWidth, CtButtonBorderHeight);
    ACanvas.CopyRect( BmpRect, AResBmp.Canvas, ResRect );

    ResRect := BmpRect;
    OffsetRect( ResRect, 5, FSysButtonTop );
    ResRect.Right := ResRect.Right - 9;
    ResRect.Bottom := ResRect.Top + FSysButtonWidth;
    if ChangeColor.IsChange then
      nLineColor := ChangedRGB( FSysLineColor, ChangeColor.ChangeToColor)
    else
      nLineColor := FSysLineColor;
    DrawFrameBorder(ACanvas, nLineColor, 1, ResRect);
    //右边
    BmpRect.Left := BmpRect.Right;
    BmpRect.Right := nTmp;
    ResRect := Rect(AResOrgX + CtButtonBorderWidth, AResOrgY, AResOrgX + CtButtonBorderWidth * 2 + 1, CtButtonBorderHeight);
    ACanvas.CopyRect( BmpRect, AResBmp.Canvas, ResRect );
  end;
end;

procedure TKKPanel.FreeResourceBmp;
begin
  if Assigned(FResBmp) then
  begin
    FResBmp.Free;
    FResBmp := nil;
  end;
end;

procedure TKKPanel.LoadResourceBmp;
begin
  if not Assigned(FResBmp) then
  begin
    FResBmp := TBitmap.Create;
    case FPanelStype of
      psKKMailForm1:       FResBmp.LoadFromResourceName( HInstance, 'KKMainForm1');
      psKKSmallForm1:      FResBmp.LoadFromResourceName( HInstance, 'KKSmallForm1');
      psKKPlayBackgroud1:  FResBmp.LoadFromResourceName( HInstance, 'KkPlay1');
      psKKPgaeBackGroud1:  FResBmp.LoadFromResourceName( HInstance, 'KKPage1');
      psKKMtvSearchGackGroud1: FResBmp.LoadFromResourceName( HInstance, 'KKMtvSearch1');
      pskKKListBackGroud1: FResBmp.LoadFromResourceName( HInstance, 'KKList1');
    end;
    if ChangeColor.IsChange then
      ChangeColor.ChangeBitmap( FResBmp );
  end;
end;

procedure TKKPanel.SetBorder(const Value: Boolean);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Invalidate;
  end;
end;

procedure TKKPanel.SetPanelStype(const Value: TKKPanelStype);
begin
  if FPanelStype <> Value then
  begin
    FPanelStype := Value;
    if FPanelStype = psKKPlayBackgroud1 then
      Height := 72;
    Invalidate;
  end;
end;

procedure TKKPanel.SetSysButton(const Value: Boolean);
begin
  if FSysButton <> Value then
  begin
    FSysButton := Value;
    Invalidate;
  end;
end;

procedure TKKPanel.SetSysButtonLength(const Value: Integer);
begin
  if FSysButtonLength <> Value then
  begin
    FSysButtonLength := Value;
    Invalidate;
  end;
end;

procedure TKKPanel.SetSysButtonTop(const Value: Integer);
begin
  FSysButtonTop := Value;
end;

procedure TKKPanel.SetSysButtonWidth(const Value: Integer);
begin
  FSysButtonWidth := Value;
  Invalidate;
end;

procedure TKKPanel.SetSysLineColor(const Value: TColor);
begin
  FSysLineColor := Value;
  Invalidate;
end;

end.
