unit uJxdScrollBar;

interface
uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Graphics, Messages, StdCtrls, uJxdCustomScrollBar, uJxdDrawSub,
  uJxdGuiStyle;

type
  TxdScrollBar = class(TxdCustomScrollBar)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    procedure DrawGraphiControl(ABufBmp: TBitmap); override;
    procedure DoScrollStyleChanged; override;
  private
    procedure DoBitmapInfoChanged(Sender: TObject);
    procedure DrawHorizontalScrollBar(ABufBmp: TBitmap);
    procedure DrawVericalScrollBar(ABufBmp: TBitmap);
  private
    FScrollBarBitmap: TBitmapInfo;
    FVerSrcBmp: TBitmap;
    procedure SetScrollBarBitmap(const Value: TBitmapInfo);
  published
    property ScrollBarBitmap: TBitmapInfo read FScrollBarBitmap write SetScrollBarBitmap;
  end;

implementation

{ TxdScrollBar }
const
  CtPreBtmCount = 4;

constructor TxdScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  FScrollBarBitmap := TBitmapInfo.Create;
  FScrollBarBitmap.OnChange := DoBitmapInfoChanged;
  FVerSrcBmp := nil;
end;

destructor TxdScrollBar.Destroy;
begin
  FScrollBarBitmap.Free;
  FreeAndNil( FVerSrcBmp );
  inherited;
end;

procedure TxdScrollBar.DoBitmapInfoChanged(Sender: TObject);
var
  nW, nH, nTemp: Integer;
begin
  nW := FScrollBarBitmap.Bitmap.Width;
  nH := FScrollBarBitmap.Bitmap.Height div FScrollBarBitmap.BitmapCount div CtPreBtmCount;
  if ScrollBarStyle = ssVerical then
  begin
    nTemp := nW;
    nW := nH;
    nH := nTemp;
    if FVerSrcBmp = nil then
      FVerSrcBmp := TBitmap.Create;
    FVerSrcBmp.Assign( FScrollBarBitmap.Bitmap );
    ImageRotate90( FVerSrcBmp );
  end;
  SetScrollBarButtonSize( nW, nH );
end;

procedure TxdScrollBar.DoScrollStyleChanged;
begin
  if ScrollBarStyle = ssVerical then
  begin
    if FVerSrcBmp = nil then
      FVerSrcBmp := TBitmap.Create;
    FVerSrcBmp.Assign( FScrollBarBitmap.Bitmap );
    ImageRotate90( FVerSrcBmp );
  end
  else
  begin
    if Assigned(FVerSrcBmp) then
      FreeAndNil( FVerSrcBmp );
  end;
end;

procedure TxdScrollBar.DrawGraphiControl(ABufBmp: TBitmap);
begin
  if (FScrollBarBitmap.Bitmap.Width > 0) and (FScrollBarBitmap.Bitmap.Height > 0) then
  begin
    if ScrollBarStyle = ssHorizontal then
      DrawHorizontalScrollBar( ABufBmp )
    else
      DrawVericalScrollBar( ABufBmp );
  end;
end;

procedure TxdScrollBar.DrawHorizontalScrollBar(ABufBmp: TBitmap);
var
  SrcR, SrcR2, DestR: TRect;
  nW, nH, nSize: Integer;

  procedure CheckSrcRect(AMsSate: TMousePosition);
  var
    mp: TMousePosition;
  begin
    SrcR2 := SrcR;
    mp := GetCurMousePosition;
    case GetCurControlState of
      csActive:
      begin
        if (mp = AMsSate) and (FScrollBarBitmap.BitmapCount >= 2) then
          OffsetRect( SrcR2, 0, nH );
      end;
      csDown:
      begin
        if (mp = AMsSate) and (FScrollBarBitmap.BitmapCount >= 3) then
          OffsetRect( SrcR2, 0, nH * 2 )
        else if (mp = AMsSate) and (FScrollBarBitmap.BitmapCount >= 2) then
          OffsetRect( SrcR2, 0, nH );
      end;
    end;
  end;

begin
  nW := FScrollBarBitmap.Bitmap.Width;
  nH := FScrollBarBitmap.Bitmap.Height;
  if (nW > 0) and (nH > 0) then
  begin
    nH := nH div FScrollBarBitmap.BitmapCount;
    nSize := nH div CtPreBtmCount;
    //±³¾°
    SrcR := Rect( 0, 0, nW, nSize );
    OffsetRect( SrcR, 0, nSize );
    DestR := Rect( 0, 0, ABufBmp.Width, ABufBmp.Height );
    CheckSrcRect( mpNormal );
    DrawRectangle( FScrollBarBitmap.Bitmap, ABufBmp.Canvas, SrcR2, DestR, FScrollBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
    //°´Å¥
    if ScrollButtonVisible then
    begin
      if GetFirstButtonRect(DestR) then
      begin
        OffsetRect( SrcR, 0, -nSize );
        CheckSrcRect( mpFirstButton );
        DrawRectangle( FScrollBarBitmap.Bitmap, ABufBmp.Canvas, SrcR2, DestR, FScrollBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
      end;

      if GetSencondButtonRect(DestR) then
      begin
        OffsetRect( SrcR, 0, nSize * 3 );
        CheckSrcRect( mpSencondButton );
        DrawRectangle( FScrollBarBitmap.Bitmap, ABufBmp.Canvas, SrcR2, DestR, FScrollBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
      end;
    end;

    //»¬¿é
    if GetSlipRect(DestR) then
    begin
      SrcR := Rect(0, nSize * 2, nW, nSize * 3 );
      CheckSrcRect( mpSlipButton );
      DrawRectangle( FScrollBarBitmap.Bitmap, ABufBmp.Canvas, SrcR2, DestR, FScrollBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
    end;
  end;
end;

procedure TxdScrollBar.DrawVericalScrollBar(ABufBmp: TBitmap);
var
  SrcR, SrcR2, DestR: TRect;
  nW, nH, nSize: Integer;

  procedure CheckSrcRect(AMsSate: TMousePosition);
  var
    mp: TMousePosition;
  begin
    SrcR2 := SrcR;
    mp := GetCurMousePosition;
    case GetCurControlState of
      csActive:
      begin
        if (mp = AMsSate) and (FScrollBarBitmap.BitmapCount >= 2) then
          OffsetRect( SrcR2, nW, 0 );
      end;
      csDown:
      begin
        if (mp = AMsSate) and (FScrollBarBitmap.BitmapCount >= 3) then
          OffsetRect( SrcR2, nW * 2, 0 )
        else if (mp = AMsSate) and (FScrollBarBitmap.BitmapCount >= 2) then
          OffsetRect( SrcR2, nW, 0 );
      end;
    end;
  end;

begin
  nW := FVerSrcBmp.Width;
  nH := FVerSrcBmp.Height;
  if (nW > 0) and (nH > 0) then
  begin
    nW := nW div FScrollBarBitmap.BitmapCount;
    nSize := nW div CtPreBtmCount;
    //±³¾°
    SrcR := Rect( 0, 0, nSize, nH );
    OffsetRect( SrcR, nSize, 0 );
    DestR := Rect( 0, 0, ABufBmp.Width, ABufBmp.Height );
    CheckSrcRect( mpNormal );
    DrawRectangle( FVerSrcBmp, ABufBmp.Canvas, SrcR2, DestR, FScrollBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
    //°´Å¥
    if ScrollButtonVisible then
    begin
      if GetFirstButtonRect(DestR) then
      begin
        OffsetRect( SrcR, -nSize, 0 );
        CheckSrcRect( mpFirstButton );
        DrawRectangle( FVerSrcBmp, ABufBmp.Canvas, SrcR2, DestR, FScrollBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
      end;

      if GetSencondButtonRect(DestR) then
      begin
        OffsetRect( SrcR, nSize * 3, 0 );
        CheckSrcRect( mpSencondButton );
        DrawRectangle( FVerSrcBmp, ABufBmp.Canvas, SrcR2, DestR, FScrollBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
      end;
    end;

    //»¬¿é
    if GetSlipRect(DestR) then
    begin
      SrcR := Rect( nSize * 2, 0, nSize * 3, nH );
      CheckSrcRect( mpSlipButton );
      DrawRectangle( FVerSrcBmp, ABufBmp.Canvas, SrcR2, DestR, FScrollBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
    end;
  end;

end;

procedure TxdScrollBar.SetScrollBarBitmap(const Value: TBitmapInfo);
begin
  FScrollBarBitmap := Value;
end;

end.
