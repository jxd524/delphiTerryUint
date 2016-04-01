unit uKKTrackBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ComCtrls, Graphics, uKKCustomTrackBar;


type
  TTrackStyle = (tsMonitor, tsVol, tsCtrl,tsPlayer);
  TTrigon = array[0..3] of TPoint;

  TKKTrackBar = class(TKKCustomTrackBar)
  private
    FTrackStyle: TTrackStyle;
    FSliderBitmap: TBitmap;
    //0: 刚好; >0: 超出长度; <0: 空白背景
    function  DrawPosition(ADesBmp: TBitmap; DesR: TRect; ASrcBmp: TBitmap; SrcR: TRect; const ADrawWidth, ASpaceWidth: Integer): Integer;
    procedure PaintBackground(ABmp: TBitmap);

    procedure DrawPlay(ACanvas: TCanvas); 
    procedure DrawVol(ACanvas: TCanvas);
    procedure DrawPosCtrl(ACanvas: TCanvas);
    procedure DrawMonitor(ACanvas: TCanvas);

    procedure SetTrackStyle(const Value: TTrackStyle);
    procedure SetBitmap(const Value: TBitmap);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawTrackBar; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TrackStyle: TTrackStyle read FTrackStyle write SetTrackStyle default tsVol;
    property SliderBitmap: TBitmap read FSliderBitmap write SetBitmap;
  end;

implementation

{$R ..\res\KKTrackBar.RES}

uses
  uKKBitmapHandle;

{ TKKTrackBar }

constructor TKKTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSliderBitmap := TBitmap.Create;
end;



destructor TKKTrackBar.Destroy;
begin
  FSliderBitmap.Free;
  inherited;
end;

function TKKTrackBar.DrawPosition(ADesBmp: TBitmap; DesR: TRect; ASrcBmp: TBitmap; SrcR: TRect; const ADrawWidth,
  ASpaceWidth: Integer): Integer;
var
  nSrcWidth: Integer;
  nDrawW, nMaxRight: Integer;
begin
  //0: 刚好; >0: 超出长度; <0: 空白背景
  Result := 0;
  nDrawW := ADrawWidth;
  nSrcWidth := WidthOfRect( SrcR );
  nMaxRight := DesR.Right;
  while nDrawW > 0 do
  begin
    DesR.Right := DesR.Left + nSrcWidth;
    if nDrawW < nSrcWidth then
    begin
      Result := nSrcWidth - nDrawW;
      DesR.Right := DesR.Left + nDrawW;
    end
    else if nDrawW < (nSrcWidth + ASpaceWidth) then
    begin
      Result := nDrawW - (nSrcWidth + ASpaceWidth);
    end;
    if DesR.Right > nMaxRight then
      DesR.Right := nMaxRight;
    ADesBmp.Canvas.CopyRect( DesR, ASrcBmp.Canvas, SrcR );
    DesR.Left := DesR.Left + nSrcWidth + ASpaceWidth;
    Dec( nDrawW, nSrcWidth + ASpaceWidth );
  end;
end;

procedure TKKTrackBar.DrawTrackBar;
begin
  if not SliderBitmap.Empty then
  begin
    FSliderSize.cx := FSliderBitmap.Width;
    FSliderSize.cy := SliderBitmap.Height div 3;
  end;
  case TrackStyle of
    tsMonitor: DrawMonitor( Canvas );
    tsVol:     DrawVol( Canvas );
    tsCtrl:    DrawPosCtrl( Canvas );
    tsPlayer:  DrawPlay( Canvas );
  end;
end;

procedure TKKTrackBar.DrawPlay(ACanvas: TCanvas);
var
  SliderBmp, Bmp, B: TBitmap;
  SelfWidth, SelfHeight, n, nBegin, nEnd: Integer;
  R: TRect;
  procedure DrawPos(AColor, ANearColor: TColor);
  begin
    if ChangeColor.IsChange then
    begin
      AColor := ChangedRGB( AColor, ChangeColor.ChangeToColor );
      ANearColor := ChangedRGB( ANearColor, ChangeColor.ChangeToColor );
    end;
    with Bmp.Canvas do
    begin
      Pen.Color := AColor;
      Pen.Width := 1;
      MoveTo( nBegin, n );
      LineTo( nEnd, n );

      Pen.Color := ANearColor;
      Pen.Width := 2;
      MoveTo( nBegin, n - 1);
      LineTo( nEnd, n - 1 );
      MoveTo( nBegin, n + 2);
      LineTo( nEnd, n + 2 );
    end;
  end;
begin
  SliderBmp := TBitmap.Create;
  Bmp := TBitmap.Create;
  B := TBitmap.Create;
  try
    if Orientation = trHorizontal then
    begin
      SelfWidth := Width;
      SelfHeight := Height;
      Bmp.Width := Width;
      Bmp.Height := Height;
    end
    else
    begin
      SelfWidth := Height;
      SelfHeight := Width;
      Bmp.Width := Height;
      Bmp.Height := Width;
    end;
    B.Assign(Bmp);
    R := Rect(0, 0, SelfWidth, SelfHeight);
    //第一层背景
    Bmp.Canvas.Brush.Color := ChangeColor.TransColor;
    Bmp.Canvas.FillRect( R );

    n := SelfHeight div 2;
    if SelfHeight mod 2 = 0 then
      Inc(n);

    nBegin := 0;
    nEnd := SelfWidth;
    if Position > 0 then
    begin
      nBegin := 0;
      nEnd := Position * SelfWidth div Max;
      DrawPos($27DFB, $20294B);
      nBegin := nEnd;
      nEnd := SelfWidth;
    end;

    if SelEnd > Position then
    begin
      nEnd := SelEnd * SelfWidth div Max;
      DrawPos($139FC, $152640);
      nBegin := nEnd;
      nEnd := SelfWidth;
    end;
    if nEnd > nBegin then
      DrawPos($818181, $323232);

    //滑块
    if not SliderBitmap.Empty then
    begin
      SliderBmp.Assign( SliderBitmap );
      if ChangeColor.IsChange then
        ChangeColor.ChangeBitmap( SliderBmp );
      R := Rect(0, 0, FSliderSize.cx, FSliderSize.cy);
      if FIsDown then
        OffsetRect(R, 0, FSliderSize.cy * 2)
      else if MouseInControl then
         OffsetRect(R, 0, FSliderSize.cy);
      Bmp.Canvas.CopyRect( FSliderRect, SliderBmp.Canvas, R );
    end;
    //画整个进度条
    if Orientation = trVertical then
    begin
      ImageRotate90(Bmp);
      ImageRotate90(B);
    end;
    R := Rect(0, 0, B.Width, B.Height);
    PaintBackground(B);
    B.Canvas.Brush.Style := bsClear;
    B.Canvas.BrushCopy( R, Bmp, R, ChangeColor.TransColor );
    BitBlt(ACanvas.Handle, 0, 0, Width, Height, B.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    SliderBmp.Free;
    Bmp.Free;
    B.Free;
  end;
end;

procedure TKKTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case TrackStyle of
    tsMonitor: ;
    tsVol, tsCtrl, tsPlayer: inherited;
  end;
end;

procedure TKKTrackBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  case TrackStyle of
    tsMonitor: ;
    tsVol, tsCtrl, tsPlayer: inherited;
  end;
end;

procedure TKKTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case TrackStyle of
    tsMonitor: ;
    tsVol, tsCtrl, tsPlayer: inherited;
  end;
end;

procedure TKKTrackBar.PaintBackground(ABmp: TBitmap);
begin
  GetControlBackground( Parent, ABmp, Rect(Left, Top, Left + ABmp.Width, Top + ABmp.Height));
end;

procedure TKKTrackBar.DrawVol(ACanvas: TCanvas);
var
  SliderBmp, Bmp, ResBmp, B, RoteB: TBitmap;
  SelfWidth, SelfHeight, W: Integer;
  R, SrcBmpR: TRect;
begin
  SliderBmp := TBitmap.Create;
  Bmp := TBitmap.Create;
  ResBmp := TBitmap.Create;
  B := TBitmap.Create;
  RoteB := TBitmap.Create;
  try
    if Orientation = trHorizontal then
    begin
      SelfWidth := Width;
      SelfHeight := Height;
      Bmp.Width := Width;
      Bmp.Height := Height;
    end
    else
    begin
      SelfWidth := Height;
      SelfHeight := Width;
      Bmp.Width := Height;
      Bmp.Height := Width;
    end;
    B.Assign(Bmp);
    RoteB.Assign( Bmp );
    SliderBmp.Assign( SliderBitmap );
    ResBmp.LoadFromResourceName(HInstance, 'Vol');
    if ChangeColor.IsChange then
    begin
      ChangeColor.ChangeBitmap( ResBmp );
      ChangeColor.ChangeBitmap( SliderBmp );
    end;
    R := Rect(0, 0, SelfWidth, SelfHeight);
    Bmp.Canvas.Brush.Color := ChangeColor.TransColor;
    Bmp.Canvas.FillRect( R );
    R.Top := R.Top + 4;
    R.Bottom := R.Bottom - 4;
    SrcBmpR := Rect(0, 0, 60, 10);
    //层背景
    Bmp.Canvas.CopyRect( R, ResBmp.Canvas, SrcBmpR );
    //进度
    if Position > 0 then
    begin
      W := Position * Bmp.Width div Max;
      OffsetRect(SrcBmpR, 0, HeightOfRect(SrcBmpR));
      B.Canvas.CopyRect(R, ResBmp.Canvas, SrcBmpR);
      R.Right := W;
      Bmp.Canvas.CopyRect( R, B.Canvas, R );
    end;
    //滑块
    if not SliderBmp.Empty then
    begin
      R := Rect(0, 0, FSliderSize.cx, FSliderSize.cy);
      if FIsDown then
        OffsetRect(R, 0, FSliderSize.cy * 2)
      else if MouseInControl then
         OffsetRect(R, 0, FSliderSize.cy);
      Bmp.Canvas.CopyRect( FSliderRect, SliderBmp.Canvas, R );
    end;
    //画整个进度条
    if Orientation = trVertical then
    begin
      ImageRotate90(Bmp);
      ImageRotate90(RoteB);
    end;
    PaintBackground(RoteB);
    R := Rect(0, 0, RoteB.Width, RoteB.Height);
    RoteB.Canvas.Brush.Style := bsClear;
    RoteB.Canvas.BrushCopy(R, Bmp, R, ChangeColor.TransColor);
    ACanvas.BrushCopy( R, RoteB, R, ChangeColor.TransColor );
  finally
    SliderBmp.Free;
    Bmp.Free;
    RoteB.Free;
    B.Free;
  end;
end;

procedure TKKTrackBar.DrawPosCtrl(ACanvas: TCanvas);
var
  SliderBmp, Bmp, B: TBitmap;
  nColor, nBackColor: TColor;
  SelfWidth, SelfHeight, W: Integer;
  R: TRect;
begin
  SliderBmp := TBitmap.Create;
  Bmp := TBitmap.Create;
  B := TBitmap.Create;
  try
    if Orientation = trHorizontal then
    begin
      SelfWidth := Width;
      SelfHeight := Height;
      Bmp.Width := Width;
      Bmp.Height := Height;
    end
    else
    begin
      SelfWidth := Height;
      SelfHeight := Width;
      Bmp.Width := Height;
      Bmp.Height := Width;
    end;
    Bmp.TransparentColor := clFuchsia;
    bmp.Transparent := True;
    B.Width := Bmp.Width;
    B.Height := Bmp.Height;
    B.TransparentColor := clFuchsia;
    B.Transparent := True;
    SliderBmp.Assign( SliderBitmap );
    nColor := $16EFF;
    nBackColor := $4E4E4E;
    if ChangeColor.IsChange then
    begin
      ChangeColor.ChangeBitmap( SliderBmp );
      nColor := ChangedRGB( nColor, ChangeColor.ChangeHSB );
      nBackColor := ChangedRGB( nBackColor, ChangeColor.ChangeHSB );
    end;
    R := Rect(0, 0, SelfWidth, SelfHeight);
    //第一层背景
    Bmp.Canvas.Brush.Color := ChangeColor.TransColor;
    Bmp.Canvas.FillRect( R );
    //第二层背景
    R.Top := R.Top + 4;
    R.Bottom := R.Bottom - 4;
    Bmp.Canvas.Brush.Color := nBackColor;
    Bmp.Canvas.FillRect( R );
    //进度
    if Position > 0 then
    begin
      W := Position * Bmp.Width div Max;
      R.Right := R.Left + W;
      Bmp.Canvas.Brush.Color := nColor;
      Bmp.Canvas.FillRect( R );
    end;

    //滑块
    if not SliderBmp.Empty then
    begin
      R := Rect(0, 0, FSliderSize.cx, FSliderSize.cy);
      if FIsDown then
        OffsetRect(R, 0, FSliderSize.cy * 2)
      else if MouseInControl then
         OffsetRect(R, 0, FSliderSize.cy);
      Bmp.Canvas.CopyRect( FSliderRect, SliderBmp.Canvas, R );
    end;
    //画整个进度条
    if Orientation = trVertical then
    begin
      ImageRotate90(Bmp);
      ImageRotate90(B);
    end;
    PaintBackground(B);
    B.Canvas.Draw( 0, 0, Bmp );
    BitBlt(ACanvas.Handle, 0, 0, Width, Height, B.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    SliderBmp.Free;
    Bmp.Free;
    B.Free;
  end;
end;
procedure TKKTrackBar.DrawMonitor(ACanvas: TCanvas);
var
  ResBmp, Bmp: TBitmap;
  H:Integer;
  W:Integer;
  nColor, nBackColor: TColor;
  R: TRect;
  SelfWidth, SelfHeight: Integer;
begin
  ResBmp := TBitmap.Create;
  Bmp := TBitmap.Create;
  try
    if Orientation = trHorizontal then
    begin
      SelfWidth := Width;
      SelfHeight := Height;
      Bmp.Width := Width;
      Bmp.Height := Height;
    end
    else
    begin
      SelfWidth := Height;
      SelfHeight := Width;
      Bmp.Width := Height;
      Bmp.Height := Width;
    end;
    ResBmp.Assign( SliderBitmap );
    nColor := $1C1C1C;
    nBackColor := 0;
    if ChangeColor.IsChange then
    begin
      ChangeColor.ChangeBitmap( ResBmp );
      nColor := ChangedRGB( nColor, ChangeColor.ChangeHSB );
      nBackColor := ChangedRGB( nBackColor, ChangeColor.ChangeHSB );
    end;
    R := Rect(0, 0, SelfWidth, SelfHeight);
    Bmp.Canvas.Brush.Color := nBackColor;
    Bmp.Canvas.FillRect( R );
    R.Bottom := R.Bottom - 1;
    DrawFrameBorder( Bmp.Canvas, nColor, 2, R );
    InflateRect( R, -2, -2 );
    R.Bottom := R.Bottom + 1;

    H := ResBmp.Height div 2;
    //背景
    DrawPosition(Bmp, R, ResBmp, Rect(0, H, ResBmp.Width, H*2), SelfWidth - 2, 2);

    //进度
    if Position > 0 then
    begin
      W := Position * (Bmp.Width - 4) div Max;
      R.Right := R.Left + W;
      DrawPosition(Bmp, R, ResBmp, Rect(0, 0, ResBmp.Width, H), W, 2);
    end;
    //画整个进度条
    if Orientation = trVertical then
      ImageRotate90(Bmp);
    BitBlt(ACanvas.Handle, 0, 0, Width, Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    ResBmp.Free;
    Bmp.Free;
  end;
end;

procedure TKKTrackBar.SetBitmap(const Value: TBitmap);
begin
  FSliderBitmap.Assign(Value);
  FSliderSize.cx := FSliderBitmap.Width;
  FSliderSize.cy := SliderBitmap.Height div 3;
  Invalidate;
end;

procedure TKKTrackBar.SetTrackStyle(const Value: TTrackStyle);
begin
  if FTrackStyle <> Value then
  begin
    FTrackStyle := Value;
    Invalidate;
  end;
end;

end.

