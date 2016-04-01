unit uKBoxEffect;

interface
uses
  Windows, uJxdOverlayEffect, Graphics, SysUtils, Classes;

type
  TKBoxEffect = class(TOverlayEffect)
  public
    TransColor: TColor;
    constructor Create;
    destructor  Destroy; override;

    procedure SetLogo(const AXPos, AYPos: Integer; const ALogoBmpFileName: WideString);

    procedure OnDrawOverlayEffer(MemCanvas: TCanvas; const AWidth, AHeight: Integer); override;
  private
    FLogoX, FLogoY: Integer;
    FLogo: TBitmap;
  end;

implementation

{ TKBoxEffect }

constructor TKBoxEffect.Create;
begin
  FLogo := nil;
  TransColor := clFuchsia;
end;

destructor TKBoxEffect.Destroy;
begin
  if Assigned(FLogo) then
    FLogo.Free;
  inherited;
end;

procedure TKBoxEffect.OnDrawOverlayEffer(MemCanvas: TCanvas; const AWidth, AHeight: Integer);
var
  SrcR, DestR: TRect;
begin
  try
    if (FLogo <> nil) and (FLogo.Width > 0) then
    begin
      SrcR := Rect(0, 0, FLogo.Width, FLogo.Height);
      DestR := Rect( FLogoX, FLogoY, FLogo.Width + FLogoX, FLogo.Height + FLogoY );
      MemCanvas.Brush.Style := bsClear;
      MemCanvas.BrushCopy( DestR, FLogo, SrcR, TransColor );
    end;
  except

  end;
end;

procedure TKBoxEffect.SetLogo(const AXPos, AYPos: Integer; const ALogoBmpFileName: WideString);
begin
  if FileExists(ALogoBmpFileName) then
  begin
    if FLogo = nil then
      FLogo := TBitmap.Create;
    FLogo.LoadFromFile( ALogoBmpFileName );
  end
  else if Assigned(FLogo) then
  begin
    FLogoX := AXPos;
    FLogoY := AYPos;
  end;
end;

end.
