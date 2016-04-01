unit uJxdProgressBar;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics;

type
  TJxdProgressBar = class(TGraphicControl)
  private
    { Private declarations }
    FMax: Integer;
    FShowText: Boolean;
    FPosition: Integer;
    FDawnSolid: Boolean;
    FResBmp: TBitmap;
    FLastPosition: Integer;
    FAutoDrawLineColor: TColor;
    FAutoDrawLine: Boolean;
    FAutoDrawLineWidth: Integer;
    procedure SetDawSolid(const Value: Boolean);
    procedure SetShowText(const Value: Boolean);
    procedure SetPosition(const Value: Integer);
    procedure SetMax(const Value: Integer);
    procedure SetAutoDrawLine(const Value: Boolean);
    procedure SetAutoDrawLineColor(const Value: TColor);
    procedure SetAutoDrawLineWidth(const Value: Integer);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure DrawLine(var ABmp: TBitmap);
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
    property AutoDrawLine: Boolean read FAutoDrawLine write SetAutoDrawLine default False;
    property AutoDrawLineColor: TColor read FAutoDrawLineColor write SetAutoDrawLineColor default 0;
    property AutoDrawLineWidth: Integer read FAutoDrawLineWidth write SetAutoDrawLineWidth default 2;
  end;

implementation
{$R ..\Resource\JxdResource.RES}

{ TFlyProgressBar }

constructor TJxdProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  Font.Name := 'ËÎÌו';
  Font.Size := 9;

  Height := 20;
  Width := 150;
  FShowText := False;
  FDawnSolid := False;
  FMax := 100;
  FAutoDrawLine := False;
  FPosition := FMax div 2;
  FLastPosition := FPosition;
  FResBmp := TBitmap.Create;
  FAutoDrawLineWidth := 2;
  FResBmp.LoadFromResourceName(HInstance, 'JxdProgress');
end;

destructor TJxdProgressBar.Destroy;
begin
  FResBmp.Free;
  inherited;
end;

procedure TJxdProgressBar.DrawLine(var ABmp: TBitmap);
var
  OldPenColor: TColor;
  OldPenWidth: Integer;
begin
  OldPenColor := ABmp.Canvas.Pen.Color;
  OldPenWidth := ABmp.Canvas.Pen.Width;
  
  ABmp.Canvas.Pen.Color := FAutoDrawLineColor;
  ABmp.Canvas.Pen.Width := 1;
  ABmp.Canvas.MoveTo(0, 1);
  ABmp.Canvas.LineTo(0, ABmp.Height - 1);
  ABmp.Canvas.MoveTo(1, ABmp.Height);
  ABmp.Canvas.LineTo(1, 0);

  ABmp.Canvas.Pixels[ABmp.Width - 2, 1] := FAutoDrawLineColor;
  ABmp.Canvas.Pixels[ABmp.Width - 2, ABmp.Height - 2] := FAutoDrawLineColor;
  ABmp.Canvas.MoveTo(ABmp.Width - 1, 1);
  ABmp.Canvas.LineTo(ABmp.Width - 1, ABmp.Height - 1);

  ABmp.Canvas.Pen.Width := FAutoDrawLineWidth;
  ABmp.Canvas.MoveTo(1, FAutoDrawLineWidth div 2);
  ABmp.Canvas.LineTo(ABmp.Width - 1, FAutoDrawLineWidth div 2); //
  ABmp.Canvas.MoveTo(ABmp.Width - 2, ABmp.Height - 1);
  ABmp.Canvas.LineTo(1, ABmp.Height - 1); //
  ABmp.Canvas.Pen.Color := OldPenColor;
  ABmp.Canvas.Pen.Width := OldPenWidth;
end;

procedure TJxdProgressBar.Paint;
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

    if FAutoDrawLine then
      DrawLine(Bmp);

    Bmp.Transparent := True;
    Canvas.Draw(0, 0, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TJxdProgressBar.SetAutoDrawLine(const Value: Boolean);
begin
  FAutoDrawLine := Value;
  Invalidate;
end;

procedure TJxdProgressBar.SetAutoDrawLineColor(const Value: TColor);
begin
  FAutoDrawLineColor := Value;
  if FAutoDrawLine then
    Invalidate;
end;

procedure TJxdProgressBar.SetAutoDrawLineWidth(const Value: Integer);
begin
  FAutoDrawLineWidth := Value;
  if FAutoDrawLine then
    Invalidate;
end;

procedure TJxdProgressBar.SetDawSolid(const Value: Boolean);
begin
  if FDawnSolid <> Value then
  begin
    FDawnSolid := Value;
    Invalidate;
  end;
end;

procedure TJxdProgressBar.SetMax(const Value: Integer);
begin
  if (Value > 0) and (Value <> FMax) then
  begin
    FMax := Value;
    Invalidate;
  end;
end;

procedure TJxdProgressBar.SetPosition(const Value: Integer);
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

procedure TJxdProgressBar.SetShowText(const Value: Boolean);
begin
  if Value <> FShowText then
  begin
    FShowText := Value;
    Invalidate;
  end;
end;

end.
