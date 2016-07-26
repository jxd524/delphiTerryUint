unit uFlyProgressBar;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics;

type
  TFlyProgressBar = class(TGraphicControl)
  private
    { Private declarations }
    FMax: Integer;
    FShowText: Boolean;
    FPosition: Integer;
    FDawnSolid: Boolean;
    FResBmp: TBitmap;
    FLastPosition: Integer;
    procedure SetDawSolid(const Value: Boolean);
    procedure SetShowText(const Value: Boolean);
    procedure SetPosition(const Value: Integer);
    procedure SetMax(const Value: Integer);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Font;
    property Align;
    property Anchors;
    property Visible;
    property Max: Integer read FMax write SetMax default 100;
    property Position: Integer read FPosition write SetPosition;
    property ShowText: Boolean read FShowText write SetShowText;
    property DawnSolid: Boolean read FDawnSolid write SetDawSolid default False;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TFlyProgressBar]);
end;

{ TFlyProgressBar }
{+R FlyProgress.RES}

constructor TFlyProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  Font.Name := 'ËÎÌå';
  Font.Size := 9;
  Font.Style := [fsBold];
  
  Height := 10;
  FShowText := False;
  FDawnSolid := False;
  FMax := 100;
  FPosition := FMax div 2;
  FLastPosition := FPosition;
  FResBmp := TBitmap.Create;
  FResBmp.LoadFromFile('C:\Documents and Settings\Administrator\×ÀÃæ\Snap3.bmp');
end;

destructor TFlyProgressBar.Destroy;
begin
  FResBmp.Free;
  inherited;
end;

procedure TFlyProgressBar.Paint;
var
  Bmp: TBitmap;
  w, i: Integer;
  R1: TRect;
  Str: string;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Width := Width;
    Bmp.Height := Height;
    w := (Width - 4) * FPosition div FMax;
    R1 := Rect(2, 0, 2 + w, Height);
    with bmp.Canvas do
    begin
      CopyRect(Rect(0, 0, 2, Height), FResBmp.Canvas, Rect(0, 0, 2, 10));
      CopyRect(Rect(Width - 2, 0, Width, Height), FResBmp.Canvas, Rect(3, 0, 5, 10));
      CopyRect(Rect(2, 0, Width - 2, Height), FResBmp.Canvas, Rect(5, 0, 6, 10));

      CopyRect(R1, FResBmp.Canvas, Rect(2, 0, 3, 10));
    end;
    if not FDawnSolid then
    begin
      R1 := Rect(8, 0, 9,Height);
      for i := 0 to w div 7 do
      begin
        Bmp.Canvas.CopyRect(R1, FResBmp.Canvas, Rect(5, 0, 6, 10));
        OffsetRect(R1, 7, 0);
      end;
    end;

    if FShowText then
    begin
      Canvas.Draw(0, 0, Bmp);
      str := Format('%d%%', [(FPosition * 100 + 50) div FMax]);
      Bmp.Canvas.Brush.Style := bsClear;
      Bmp.Canvas.Font := Self.Font;
      R1 := ClientRect;
      DrawText(Bmp.Canvas.Handle, PChar(Str), Length(Str), R1, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    end;

    Bmp.Transparent := True;
    Canvas.Draw(0, 0, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TFlyProgressBar.SetDawSolid(const Value: Boolean);
begin
  if FDawnSolid <> Value then
  begin
    FDawnSolid := Value;
    Invalidate;
  end;
end;

procedure TFlyProgressBar.SetMax(const Value: Integer);
begin
  if (Value > 0) and (Value <> FMax) then
  begin
    FMax := Value;
    Invalidate;
  end;
end;

procedure TFlyProgressBar.SetPosition(const Value: Integer);
begin
  if FPosition <> Value then
  begin
    if Value < 0 then
      FPosition := 0
    else if Value > FMax then
      FPosition := FMax
    else
      FPosition := Value;
    if FLastPosition > FPosition then
    begin
      Invalidate;
    end else
      Paint;
    FLastPosition := FPosition;
  end;
end;

procedure TFlyProgressBar.SetShowText(const Value: Boolean);
begin
  if Value <> FShowText then
  begin
    FShowText := Value;
    Invalidate;
  end;
end;

end.
