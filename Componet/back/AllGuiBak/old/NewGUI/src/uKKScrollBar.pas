unit uKKScrollBar;

interface
uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Graphics, Messages, StdCtrls, uKKCustomScrollBar, uKKGuiDef;

type
  TKKScrollBar = class(TKKCustomScrollBar)
  private
    FResBitmap: TBitmap;
    FSpacePixel: Integer;
    FMinScrollBtnLen: Integer;
    FChangeColor: TChangeToColor;
    procedure DrawUpDownButton(ADecCanvas, ASrcCanvas: TCanvas;  ASrcBackR, ASrcForeR, ADesR: TRect);
    procedure DrawScrollBar(ADesBmp, AResBmp: TBitmap);

    procedure SetSpacePixel(const Value: Integer);
    procedure SetMinScrollBtnLen(const Value: Integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MinScrollButtonLen: Integer read FMinScrollBtnLen write SetMinScrollBtnLen default 15;
    property SpacePixel: Integer read FSpacePixel write SetSpacePixel default 5;
    property ChangeColor: TChangeToColor read FChangeColor write FChangeColor;
  end;

implementation

uses uKKBitmapHandle, Types;

{ TKKScrollBar }
{$R ..\res\KKScrollBar.RES}

constructor TKKScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  FResBitmap := TBitmap.Create;
  FResBitmap.LoadFromResourceName( HInstance, 'KKScroll');
  FSpacePixel := 5;
  FMinScrollBtnLen := 15;
  FChangeColor := TChangeToColor.Create( Self );
end;

destructor TKKScrollBar.Destroy;
begin
  FChangeColor.Free;
  FResBitmap.Free;
  inherited;
end;

procedure TKKScrollBar.DrawScrollBar(ADesBmp, AResBmp: TBitmap);
const
  //资源文件图像位置
  CtScrollNormal: TRect = (Left: 0; Top: 0; Right: 17; Bottom: 9);
  CtScrollActive: TRect = (Left: 0; Top: 9; Right: 17; Bottom: 18);
  CtScrollDown: TRect = (Left: 0; Top: 18; Right: 17; Bottom: 27);
  CtBack: TRect = (Left: 0; Top: 28; Right: 17; Bottom: 29);
  CtBtnNormal: TRect = (Left: 17; Top: 0; Right: 26; Bottom: 5);  //向上
  CtBtnActive: TRect = (Left: 17; Top: 6; Right: 26; Bottom: 11);
  CtBtnDown: TRect = (Left: 17; Top: 12; Right: 26; Bottom: 17);
var
  DesR, ScrollR, BtnR1, BtnR2, R1, R2: TRect;
  nWidth, nHeight: Integer;
  B: TBitmap;

  procedure DrawResToBmp(Bmp: TBitmap; R: TRect);
  begin
    Bmp.Width := WidthOfRect(R);
    Bmp.Height := HeightOfRect(R);
    Bmp.Canvas.CopyRect( Rect(0, 0, Bmp.Width, Bmp.Height), AResBmp.Canvas, R );
  end;

  procedure BmpRotate(Bmp: TBitmap; ARotate: Integer);
  var
    i, iCount: Integer;
  begin
    iCount := ARotate div 90;
    for i := 0 to iCount - 1 do
      ImageRotate90( Bmp );
  end;
  function GetRect: TRect;
  begin
    Result := Rect(0, 0, B.Width, B.Height);
  end;

  procedure DrawBmp(ABmpSrcR, ADesR: TRect; ARotate: Integer);
  var
    bUD: Boolean;
  begin
    DrawResToBmp(B, ABmpSrcR);
    bUD := True;
    if ScrollStyle = ssHorizontal then
    begin
      BmpRotate(B, ARotate);
      bUD := False;
    end;
    DrawRectangle( ADesBmp.Canvas, B.Canvas, ADesR, GetRect, bUD );
  end;

  procedure DrawButton(SrcR, DesR: TRect; AIsFirst: Boolean);
  var
    bUD: Boolean;
    SrcW, DesW, SrcH, DesH: Integer;
    R: TRect;
  begin
    DrawResToBmp( B, SrcR );
    bUD := True;
    if ScrollStyle = ssHorizontal then
    begin
      if AIsFirst then
        BmpRotate(B, 90)
      else
        BmpRotate(B, 270);
      bUD := False;
    end
    else
    begin
      if not AIsFirst then
         BmpRotate(B, 180);
    end;
    SrcR := GetRect;
    SrcW := WidthOfRect( SrcR );
    SrcH := HeightOfRect( SrcR );
    DesW := WidthOfRect( DesR );
    DesH := HeightOfRect( DesR );
    with R do
    begin
      Left := DesR.Left + (DesW - SrcW) div 2;
      Top := DesR.Top + (DesH - SrcH) div 2;
      Right := Left + SrcW;
      Bottom := Top + SrcH;
    end;
    ADesBmp.Canvas.Brush.Style := bsClear;
    ADesBmp.Canvas.BrushCopy( R, B, GetRect, ChangeColor.TransColor );
  end;
begin
  //以垂直方向画
  B := TBitmap.Create;
  try
    //背景
    DrawResToBmp(B, CtBack);
    if ScrollStyle = ssHorizontal then
      BmpRotate(B, 90);
    ADesBmp.Canvas.CopyRect( ClientRect, B.Canvas, GetRect );

    case CurMousePos of
      mpNormal:
      begin
        ScrollR := CtScrollNormal;
        R1 := CtScrollNormal;
        R2 := CtScrollNormal;
        BtnR1 := CtBtnNormal;
        BtnR2 := CtBtnNormal;
      end;
      mpUpButton, mpDownButton:
      begin
        ScrollR := CtScrollNormal;

        if CurMousePos = mpUpButton then
        begin
          R2 := CtScrollNormal;
          BtnR2 := CtBtnNormal;
          if FCurMouseState = msDonw then
          begin
            BtnR1 := CtBtnDown;
            R1 := CtScrollDown;
          end
          else if FCurMouseState = msActive then
          begin
            BtnR1 := CtBtnActive;
            R1 := CtScrollActive
          end
          else
          begin
            BtnR1 := CtBtnNormal;
            R1 := CtScrollNormal;
          end;
        end
        else
        begin
          R1 := CtScrollNormal;
          BtnR1 := CtBtnNormal;
          if FCurMouseState = msDonw then
          begin
            R2 := CtScrollDown;
            BtnR2 := CtBtnDown;
          end
          else if FCurMouseState = msActive then
          begin
            R2 := CtScrollActive;
            BtnR2 := CtBtnActive;
          end
          else
          begin
            R2 := CtScrollNormal;
            BtnR2 := CtBtnNormal;
          end;
        end;
      end;
      mpScrollButton:
      begin
        BtnR1 := CtBtnNormal;
        BtnR2 := CtBtnNormal;
        if FCurMouseState = msDonw then
          ScrollR := CtScrollDown
        else if FCurMouseState = msActive then
          ScrollR := CtScrollActive
        else
          ScrollR := CtScrollNormal;
        R1 := CtScrollNormal;
        R2 := CtScrollNormal;
      end;
    end;
    //滑块
    DrawBmp( ScrollR, FCurScrollRect, 90 );
    //控制按钮
    if UpDownVisiable then
    begin
      //第一个
      DrawBmp( R1, FUpBtnRect, 90 );
      DrawBmp( R2, FDownBtnRect, 90 );

      DrawButton( BtnR1, FUpBtnRect, True );
      DrawButton( BtnR2, FDownBtnRect, False );
    end;
  finally
    B.Free;
  end;
end;

procedure TKKScrollBar.DrawUpDownButton(ADecCanvas, ASrcCanvas: TCanvas; ASrcBackR, ASrcForeR, ADesR: TRect);
var
  SrcR, DesR: TRect;
  w, h: Integer;
begin
  SrcR := ASrcBackR;
  DesR := ADesR;
  DrawRectangle( ADecCanvas, ASrcCanvas, ADesR, SrcR, True );
  SrcR := ASrcForeR;
  w := WidthOfRect(SrcR);
  h := HeightOfRect(SrcR);
  DesR.Top := ADesR.Top + ( HeightOfRect(ADesR) - h ) div 2;
  DesR.Left := ADesR.Left + ( WidthOfRect(ADesR) - w ) div 2;
  DesR.Right := DesR.Left + w;
  DesR.Bottom := DesR.Top + h;
  ADecCanvas.CopyRect( DesR, ASrcCanvas, SrcR );
end;

procedure TKKScrollBar.Paint;
var
  BufBmp, ResBmp: TBitmap;
begin
  BufBmp := TBitmap.Create;
  ResBmp := TBitmap.Create;
  try
    BufBmp.Width := Width;
    BufBmp.Height := Height;
    ResBmp.Assign( FResBitmap );
    if ChangeColor.IsChange then
      ChangeColor.ChangeBitmap( ResBmp );      
    DrawScrollBar(BufBmp, ResBmp);
  finally
    Canvas.Draw(0, 0, BufBmp);
    BufBmp.Free;
  end;
end;

procedure TKKScrollBar.SetMinScrollBtnLen(const Value: Integer);
begin
  FMinScrollBtnLen := Value;
end;

procedure TKKScrollBar.SetSpacePixel(const Value: Integer);
begin
  if FSpacePixel <> Value then
  begin
    FSpacePixel := Value;
    Invalidate;
  end;
end;

end.
