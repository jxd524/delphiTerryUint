unit JxdScrollBar;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics;

type
  TJxdScrollBar = class(TGraphicControl)
  private
    FGuiBmp: TBitmap;
    procedure DrawVerticalSrcoll(ACanvas: TCanvas);
  protected
    procedure LoadResource;
    procedure FreeResource;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Visible;
  end;

implementation

uses uBitmapHandle;

{ TJxdProgressBar }

constructor TJxdScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  FGuiBmp := TBitmap.Create;
end;

destructor TJxdScrollBar.Destroy;
begin
  FGuiBmp.Free;
  inherited;
end;

procedure TJxdScrollBar.DrawVerticalSrcoll(ACanvas: TCanvas);
var
  SrcR, DesR: TRect;
begin
  SrcR := Rect(0, 0, 16, 5);
  DesR := ClientRect;
//  DrawRectangle(ACanvas, FGuiBmp.Canvas, DesR, SrcR);
  ACanvas.CopyRect( DesR, FGuiBmp.Canvas, SrcR );
//  ACanvas.Draw( 0, 0, FGuiBmp );
end;

procedure TJxdScrollBar.FreeResource;
begin
  FGuiBmp.LoadFromFile('C:\Documents and Settings\Administrator\×ÀÃæ\Snap1.bmp');
end;

procedure TJxdScrollBar.LoadResource;
begin
  FGuiBmp.FreeImage;
end;

procedure TJxdScrollBar.Paint;
var
  BufBmp: TBitmap;
begin
  BufBmp := TBitmap.Create;
  try
    BufBmp.Width := Width;
    BufBmp.Height := Height;
    LoadResource;
    DrawVerticalSrcoll( BufBmp.Canvas );
  finally
    Canvas.Draw(0, 0, BufBmp);
    FreeResource;
  end;
end;

end.
