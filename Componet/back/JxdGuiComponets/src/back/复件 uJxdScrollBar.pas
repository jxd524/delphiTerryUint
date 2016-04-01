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

    function  GetFirstButtonRect(var ARect: TRect; const ADrawRect: Boolean): Boolean; overload;
    function  GetSencondButtonRect(var ARect: TRect; const ADrawRect: Boolean): Boolean; overload;
    function  GetSlipRect(var ARect: TRect; const ADrawRect: Boolean): Boolean; overload; 
  private
    procedure DoBitmapInfoChanged(Sender: TObject);
    procedure DrawHorizontalScrollBar(ABufBmp: TBitmap);
    procedure DrawVericalScrollBar(ABufBmp: TBitmap);
    function  VerRectToHorizontalRect(const AVerRect: TRect): TRect;
  private
    FScrollBarBitmap: TBitmapInfo;
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
end;

destructor TxdScrollBar.Destroy;
begin
  FScrollBarBitmap.Free;
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
  end;
  SetScrollBarButtonSize( nW, nH );
end;

procedure TxdScrollBar.DrawGraphiControl(ABufBmp: TBitmap);
var
  bmp: TBitmap;
begin
  if (FScrollBarBitmap.Bitmap.Width > 0) and (FScrollBarBitmap.Bitmap.Height > 0) then
  begin
    if ScrollBarStyle = ssHorizontal then
      DrawHorizontalScrollBar( ABufBmp )
    else
    begin
      bmp := TBitmap.Create;
      bmp.Width := ABufBmp.Height;
      bmp.Height := ABufBmp.Width;
      DrawHorizontalScrollBar( bmp );
//      ImageRotate90( bmp );
      ABufBmp.Canvas.Draw( 0, 0, bmp );
    end;
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
      if GetFirstButtonRect(DestR, True) then
      begin
        OffsetRect( SrcR, 0, -nSize );
        CheckSrcRect( mpFirstButton );
        DrawRectangle( FScrollBarBitmap.Bitmap, ABufBmp.Canvas, SrcR2, DestR, FScrollBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
      end;

      if GetSencondButtonRect(DestR, True) then
      begin
        OffsetRect( SrcR, 0, nSize * 3 );
        CheckSrcRect( mpSencondButton );
        DrawRectangle( FScrollBarBitmap.Bitmap, ABufBmp.Canvas, SrcR2, DestR, FScrollBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
      end;
    end;

    //»¬¿é
    if GetSlipRect(DestR, True) then
    begin
      SrcR := Rect(0, nSize * 2, nW, nSize * 3 );
      CheckSrcRect( mpSlipButton );
      DrawRectangle( FScrollBarBitmap.Bitmap, ABufBmp.Canvas, SrcR2, DestR, FScrollBarBitmap.BitmapDrawStyle, IsTransColor, TransColor );
    end;
  end;
end;

procedure TxdScrollBar.DrawVericalScrollBar(ABufBmp: TBitmap);
begin

end;

function TxdScrollBar.GetFirstButtonRect(var ARect: TRect; const ADrawRect: Boolean): Boolean;
begin
  Result := GetFirstButtonRect( ARect );
  if Result and ADrawRect and (ScrollBarStyle = ssVerical) then
    ARect := VerRectToHorizontalRect( ARect );
end;

function TxdScrollBar.GetSencondButtonRect(var ARect: TRect; const ADrawRect: Boolean): Boolean;
begin
  Result := GetSencondButtonRect( ARect );
  if Result and ADrawRect and (ScrollBarStyle = ssVerical) then
    ARect := VerRectToHorizontalRect( ARect );
end;

function TxdScrollBar.GetSlipRect(var ARect: TRect; const ADrawRect: Boolean): Boolean;
begin
  Result := GetSlipRect( ARect );
  if Result and ADrawRect and (ScrollBarStyle = ssVerical) then
    ARect := VerRectToHorizontalRect( ARect );
end;

procedure TxdScrollBar.SetScrollBarBitmap(const Value: TBitmapInfo);
begin
  FScrollBarBitmap := Value;
end;

function TxdScrollBar.VerRectToHorizontalRect(const AVerRect: TRect): TRect;
begin
  Result.Left := AVerRect.Top;
  Result.Top := AVerRect.Left;
  Result.Right := AVerRect.Bottom;
  Result.Bottom := AVerRect.Right;
end;

end.
