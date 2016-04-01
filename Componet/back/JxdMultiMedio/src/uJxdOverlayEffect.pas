unit uJxdOverlayEffect;

interface

uses
  Windows, Graphics, Classes, Forms;

type
  TOverlayEffect = class
  public
    constructor Create;
    destructor  Destroy; override;

    function SnapShot(var Bmp: TBitmap): Boolean;

    procedure DoOverlayEffer(Sender: TObject; MemDC: HDC; const AWidth, AHeight: Integer); virtual;
    procedure OnDrawOverlayEffer(MemCanvas: TCanvas; const AWidth, AHeight: Integer); virtual;
  private
    FMemCanvas: TCanvas;
    FSnapShot: Boolean;
    FSnapBmp: TBitmap;
  end;

implementation

{ TOverlayEffect }

constructor TOverlayEffect.Create;
begin
  FMemCanvas := nil;
  FSnapBmp := nil;
  FSnapShot := False;
end;

destructor TOverlayEffect.Destroy;
begin
  FMemCanvas.Free;
  inherited;
end;

procedure TOverlayEffect.DoOverlayEffer(Sender: TObject; MemDC: HDC; const AWidth, AHeight: Integer);
begin
  if FMemCanvas = nil then
    FMemCanvas := TCanvas.Create;
  FMemCanvas.Handle := MemDC;
  OnDrawOverlayEffer( FMemCanvas, AWidth, AHeight );
  if FSnapShot then
  begin
    FSnapBmp := TBitmap.Create;
    FSnapBmp.Width := AWidth;
    FSnapBmp.Height := AHeight;
    BitBlt( FSnapBmp.Canvas.Handle, 0, 0, AWidth, AHeight, MemDC, 0, 0, SRCCOPY );
    FSnapShot := False;
  end;
end;

procedure TOverlayEffect.OnDrawOverlayEffer(MemCanvas: TCanvas; const AWidth, AHeight: Integer);
begin

end;

function TOverlayEffect.SnapShot(var Bmp: TBitmap): Boolean;
var
  nCount: Integer;
begin
  Result := False;
  if FSnapShot then Exit;
  FSnapShot := True;
  nCount := 0;
  while FSnapShot do
  begin
    Inc( nCount );
    Sleep( 10 );
    Application.ProcessMessages;
    if nCount > 20 then
      Break;
  end;
  Result := not FSnapShot;
  if Result then
  begin
    if Bmp = nil then
      Bmp := TBitmap.Create;
    Bmp.Width := FSnapBmp.Width;
    Bmp.Height := FSnapBmp.Height;
    Bmp.Canvas.Draw( 0, 0, FSnapBmp );
  end;
  if Assigned(FSnapBmp) then
  begin
    FSnapBmp.Free;
    FSnapBmp := nil;
  end;
end;

end.
