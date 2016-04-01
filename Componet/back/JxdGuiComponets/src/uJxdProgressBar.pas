{
BarBitmapÀµ√˜£∫
1£∫±≥æ∞£ª
2£∫“—…Ë÷√Œª÷√±≥æ∞
3£∫ª¨øÈ
4£∫ª¨øÈActive
5£∫ª¨øÈDown
}

unit uJxdProgressBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, Graphics, uJxdGraphicBaseClass, uJxdGuiStyle, uJxdDrawSub;

type
  TOnPositionChanged = procedure(Sender: TObject; const IsMouseChanged: Boolean) of object;
  TProgressBarStyle = (pbsVertical, pbsHorizontal);
  TxdProgressBar = class(TxdGraphicBase)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  MouseToPos(const Apt: TPoint): Int64;    
  protected
    FMouseInPulley: Boolean;
    function  CurDrawPulleyRect: TRect;
    procedure DrawGraphiControl(ABufBmp: TBitmap); override;
    procedure DrawVerticalProgressBar(ADestBmp: TBitmap);
    procedure DoBarBitmapChanged(Sender: TObject);
    procedure DoControlStateChanged(const AOldState, ANewState: TxdComponentState; const ACurPt: TPoint); override;

    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  private
    FMouseDownPt: TPoint;
    FCurPulleyRect: TRect;
    procedure Changed;
    procedure ReCalcPrPixel;
    function  GetPulleyRect(var ARect: TRect): Boolean;
    procedure SetMouseInPulley(AValue: Boolean);
  private
    FPrePixel: Double;
    FMax: Int64;
    FPos: Int64;
    FBarBitmap: TBitmapInfo;
    FOnChanged: TOnPositionChanged;
    FProgressBasStyle: TProgressBarStyle;
    FSpace: Integer;
    FTransColor: TColor;
    FIsTransColor: Boolean;
    FASPulley: Boolean;
    procedure SetMax(const Value: Int64);
    procedure SetPos(const Value: Int64);
    procedure SetBarBitmap(const Value: TBitmapInfo);
    procedure SetProgressBasStyle(const Value: TProgressBarStyle);
    procedure SetSpace(const Value: Integer);
    procedure SetASPulley(const Value: Boolean);
  published
    property ProgressBasStyle: TProgressBarStyle read FProgressBasStyle write SetProgressBasStyle;
    property Max: Int64 read FMax write SetMax;
    property Position: Int64 read FPos write SetPos;
    property Space: Integer read FSpace write SetSpace;
    property BarBitmap: TBitmapInfo read FBarBitmap write SetBarBitmap;
    property AlwayShowPulley: Boolean read FASPulley write SetASPulley;
    property OnChanged: TOnPositionChanged read FOnChanged write FOnChanged;
  end;

implementation

{ TxdProgressBar }

procedure TxdProgressBar.Changed;
begin
  if Assigned(FOnChanged) then
    OnChanged( Self, (FMouseInPulley and (FMouseDownPt.X <> -1) and (FMouseDownPt.Y <> -1)) or
                      (GetCurControlState = csDown) );
end;

constructor TxdProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  FProgressBasStyle := pbsVertical;
  Width := 120;
  Height := 13;
  FMax := 100;
  FSpace := 2;
  FIsTransColor := True;
  FTransColor := clFuchsia;
  FASPulley := True;
  FMouseInPulley := False;
  FMouseDownPt.X := -1;
  FMouseDownPt.Y := -1;

  FBarBitmap := TBitmapInfo.Create;
  FBarBitmap.OnChange := DoBarBitmapChanged;

  ReCalcPrPixel;
end;

function TxdProgressBar.CurDrawPulleyRect: TRect;
begin
  Result := FCurPulleyRect;
  if ProgressBasStyle = pbsHorizontal then
  begin
    Result.Left := Height - FCurPulleyRect.Bottom;
    Result.Top := 0;
    Result.Right := Result.Left + FBarBitmap.Bitmap.Width;
    Result.Bottom := Width;
  end;
end;

destructor TxdProgressBar.Destroy;
begin
  FBarBitmap.Free;
  inherited;
end;

procedure TxdProgressBar.DoBarBitmapChanged(Sender: TObject);
begin
  ReCalcPrPixel;
  Invalidate;
end;

procedure TxdProgressBar.DoControlStateChanged(const AOldState, ANewState: TxdComponentState; const ACurPt: TPoint);
var
  R: TRect;
begin
  if ANewState = csDown then
  begin
    if PtInRect(FCurPulleyRect, ACurPt) then
    begin
      FMouseDownPt := ACurPt;
      R := FCurPulleyRect;
      InvalidateGraphicRect( @R );
    end
    else
      Position := MouseToPos( ACurPt );
  end
  else
  begin
    FMouseDownPt.X := -1;
    FMouseDownPt.Y := -1;
    R := FCurPulleyRect;
//    if PtInRect(FCurPulleyRect, ACurPt) or not AlwayShowPulley then
      InvalidateGraphicRect( @R );
  end;
end;

procedure TxdProgressBar.DrawGraphiControl(ABufBmp: TBitmap);
var
  bmp: TBitmap;
begin
  if ProgressBasStyle = pbsVertical then
    DrawVerticalProgressBar( ABufBmp )
  else
  begin
    bmp := TBitmap.Create;
    bmp.Width := ABufBmp.Height;
    bmp.Height := ABufBmp.Width;
    bmp.Canvas.Brush.Color := TransColor;
    bmp.Canvas.FillRect( Rect(0, 0, bmp.Width, bmp.Height) );
    DrawVerticalProgressBar( bmp );
    ImageRotate90( bmp );
    ABufBmp.Canvas.Draw( 0, 0, bmp );
    bmp.Free;
  end;
end;

procedure TxdProgressBar.DrawVerticalProgressBar(ADestBmp: TBitmap);
var
  SrcR, DestR: TRect;
  nW, nH, nPos: Integer;
  state: TxdComponentState;
begin
  nW := FBarBitmap.Bitmap.Width;
  nH := FBarBitmap.Bitmap.Height;
  if (nW > 0) and (nH > 0) then
  begin
    nH := nH div FBarBitmap.BitmapCount;
    //±≥æ∞
    SrcR := Rect( 0, 0, nW, nH );
    DestR := Rect( 0, 0, ADestBmp.Width, ADestBmp.Height );
    DrawRectangle( FBarBitmap.Bitmap, ADestBmp.Canvas, SrcR, DestR, FBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
    if FBarBitmap.BitmapCount >= 2 then
    begin
      //ª¨∂Ø±≥æ∞
      OffsetRect( SrcR, 0, nH );
      if FPos > 0 then
      begin
        nPos := Round( FPrePixel * FPos );
        DestR := Rect( Space, 0, Space + nPos, ADestBmp.Height );
        DrawRectangle( FBarBitmap.Bitmap, ADestBmp.Canvas, SrcR, DestR, FBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
      end;
      if FBarBitmap.BitmapCount >= 3 then
      begin
        //ª¨øÈ
        OffsetRect( SrcR, 0, nH );
        state := GetCurControlState;

        case state of
          csActive:
          begin
            if FMouseInPulley and (FBarBitmap.BitmapCount >= 4) then
              OffsetRect( SrcR, 0, nH );
          end;
          csDown:
          begin
            if FMouseInPulley then
            begin
              if FBarBitmap.BitmapCount >= 5 then
                OffsetRect( SrcR, 0, nH * 2 )
              else if FBarBitmap.BitmapCount >= 4 then
                OffsetRect( SrcR, 0, nH );
            end;
          end
          else
            if not AlwayShowPulley then Exit;
        end;

        DestR := CurDrawPulleyRect;
        if DestR.Right > 0 then
          DrawRectangle( FBarBitmap.Bitmap, ADestBmp.Canvas, SrcR, DestR, FBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
      end;
    end;
  end;
end;

function TxdProgressBar.GetPulleyRect(var ARect: TRect): Boolean;
var
  nPos, n: Integer;
begin
  Result := False;
  if (FBarBitmap.Bitmap.Width <= 0) or (FBarBitmap.Bitmap.Height <= 0) then Exit;

  nPos := Round( FPrePixel * FPos );
  nPos := Space + nPos;

  if ProgressBasStyle = pbsVertical then
  begin
    n := FBarBitmap.Bitmap.Width div 2;
    ARect := Rect( nPos - n, 0, nPos - n + FBarBitmap.Bitmap.Width, Height );
    if ARect.Right > Width then
      OffsetRect( ARect, Width - ARect.Right, 0 )
    else if ARect.Left < 0 then
      OffsetRect( ARect, -ARect.Left, 0 );
  end
  else
  begin
    n := FBarBitmap.Bitmap.Width div 2;
    ARect := Rect( 0, Height - nPos - n, Width, -1 );
    ARect.Bottom := ARect.Top + FBarBitmap.Bitmap.Width;
    if ARect.Bottom > Height then
      OffsetRect( ARect, 0, Height - ARect.Bottom )
    else if ARect.Top < 0 then
      OffsetRect( ARect, 0, -ARect.Top );
  end;
  Result := True;
end;

function TxdProgressBar.MouseToPos(const Apt: TPoint): Int64;
var
  nLen: Int64;
begin
  if ProgressBasStyle = pbsVertical then
    nLen := Apt.X
  else
    nLen := Height - Apt.Y;
  Inc( nLen, FSpace );
  Result := Round(nLen / FPrePixel);
end;

procedure TxdProgressBar.ReCalcPrPixel;
var
  nLen: Int64;
begin
  if FMax = 0 then Exit;
  if ProgressBasStyle = pbsVertical then
    nLen := Width
  else
    nLen := Height;
  Dec( nLen, FSpace * 2 );
  FPrePixel := nLen / FMax;
  GetPulleyRect( FCurPulleyRect );
end;

procedure TxdProgressBar.SetASPulley(const Value: Boolean);
begin
  if FASPulley <> Value then
  begin
    FASPulley := Value;
    Invalidate;
  end;
end;

procedure TxdProgressBar.SetBarBitmap(const Value: TBitmapInfo);
begin
  FBarBitmap := Value;
end;

procedure TxdProgressBar.SetMax(const Value: Int64);
begin
  if (FMax <> Value) and (Value > 0) then
  begin
    FMax := Value;
    ReCalcPrPixel;
    Invalidate;
  end;
end;

procedure TxdProgressBar.SetMouseInPulley(AValue: Boolean);
var
  R: TRect;
begin
  if FMouseInPulley <> AValue then
  begin
    FMouseInPulley := AValue;
    R := CurDrawPulleyRect;
    InvalidateGraphicRect( @R );
  end;
end;

procedure TxdProgressBar.SetPos(const Value: Int64);
var
  R1, R2: TRect;
begin
  if (FPos <> Value) and (Value < FMax) and (Value >= 0) then
  begin
    FPos := Value;
    R1 := FCurPulleyRect;
    ReCalcPrPixel;
    Changed;    
    R2 := FCurPulleyRect;
    if R1.Left > R2.Left then
      R1.Left := R2.Left;
    if R1.Top > R2.Top then
      R1.Top := R2.Top;
    if R1.Right < R2.Right then
      R1.Right := R2.Right;
    if R1.Bottom < R2.Bottom then
      R1.Bottom := R2.Bottom;
    InvalidateGraphicRect( @R1 );
  end;
end;

procedure TxdProgressBar.SetProgressBasStyle(const Value: TProgressBarStyle);
begin
  if FProgressBasStyle <> Value then
  begin
    FProgressBasStyle := Value;
    ReCalcPrPixel;
    Invalidate;
  end;
end;

procedure TxdProgressBar.SetSpace(const Value: Integer);
begin
  if FSpace <> Value then
  begin
    FSpace := Value;
    ReCalcPrPixel;
    Invalidate;
  end;
end;

procedure TxdProgressBar.WMMouseMove(var Message: TWMMouseMove);
var
  pt: TPoint;
  R: TRect;
begin
  R := FCurPulleyRect;
  if R.Right > 0 then
  begin
    pt.X := Message.XPos;
    pt.Y := Message.YPos;
    SetMouseInPulley( PtInRect(R, pt) );
  end;
  if (FMouseDownPt.X > 0) and (FMouseDownPt.Y > 0) then
  begin
    pt.X := Message.XPos;
    pt.Y := Message.YPos;
    Position := MouseToPos( pt );
  end;
  if Assigned(OnMouseMove) then
    OnMouseMove( Self, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos );
end;

procedure TxdProgressBar.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  ReCalcPrPixel;
  Invalidate;
end;

end.
