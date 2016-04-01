unit uKKSplitter;

interface
uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Graphics, uKKCustomButton, uKKCustomSplitter, uKKGuiDef;

type
  TKKSplitter = class(TKKCustomSplitter)
  private
    FChangeColor: TChangeToColor;
    procedure DrawVerticalSplitter(ACanvas: TCanvas; AResBmp: TBitmap);
    procedure DrawHorizontalSplitter(ACanvas: TCanvas; AResBmp: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DrawSplitter(ACanvas: TCanvas); override;
  published
    property ChangeColor: TChangeToColor read FChangeColor write FChangeColor;
  end;

implementation

uses uKKBitmapHandle;

{ TKKButton }

{$R ..\res\KKSplitter1.RES}

constructor TKKSplitter.Create(AOwner: TComponent);
begin
  inherited;
  SplitterWay := swLeft;
  SplitterStyle := ssVertical;
  Width := 8;
  FChangeColor := TChangeToColor.Create( Self );
end;

destructor TKKSplitter.Destroy;
begin
  FChangeColor.Free;
  inherited;
end;

procedure TKKSplitter.DrawHorizontalSplitter(ACanvas: TCanvas; AResBmp: TBitmap);
var
  DesR, SrcR, SrcR1: TRect;
begin
  //±³¾°
  DesR := ClientRect;
  SrcR := Rect(0, 0, 1, 8);
  ACanvas.CopyRect(DesR, AResBmp.Canvas, SrcR);
  //°´¼ü
  if (FMouseState in [kcsActive, kcsDown]) and CheckMouseInButton then
  begin
    SrcR := Rect(8, 0, 13, 8);
    if SplitterWay in [swLeft, swUp] then
      SrcR1 := Rect(26, 0, 37, 8)
    else
      SrcR1 := Rect(50, 0, 61, 8);
  end
  else
  begin
    SrcR := Rect(2, 0, 7, 8);
    if SplitterWay in [swLeft, swUp] then
      SrcR1 := Rect(14, 0, 25, 8)
    else
      SrcR1 := Rect(38, 0, 49, 8);
  end;
  DrawRectangle( ACanvas, AResBmp.Canvas, FButtonRect, SrcR, True );
  DesR.Left := FButtonRect.Left + ( WidthOfRect(FButtonRect) - WidthOfRect(SrcR1) ) div 2;
  DesR.Right := DesR.Left + WidthOfRect(SrcR1);
  ACanvas.CopyRect(DesR, AResBmp.Canvas, SrcR1);
end;

procedure TKKSplitter.DrawSplitter(ACanvas: TCanvas);
var
  ResBmp: TBitmap;
begin
  ResBmp := TBitmap.Create;
  try
    case SplitterStyle of
      ssHorizontal:
      begin
        ResBmp.LoadFromResourceName(HInstance, 'HSplitter1');
        if FChangeColor.IsChange then
          FChangeColor.ChangeBitmap( ResBmp );
        DrawHorizontalSplitter(ACanvas, ResBmp);
      end;
      ssVertical:
      begin
        ResBmp.LoadFromResourceName(HInstance, 'VSplitter1');
        if FChangeColor.IsChange then
          FChangeColor.ChangeBitmap( ResBmp );
        DrawVerticalSplitter(ACanvas, ResBmp);
      end;
    end;
  finally
    ResBmp.Free;
  end;
end;

procedure TKKSplitter.DrawVerticalSplitter(ACanvas: TCanvas; AResBmp: TBitmap);
var
  DesR, SrcR, SrcR1: TRect;
begin
  //±³¾°
  DesR := ClientRect;
  SrcR := Rect(0, 0, 8, 1);
  ACanvas.CopyRect(DesR, AResBmp.Canvas, SrcR);
  //°´¼ü
  if (FMouseState in [kcsActive, kcsDown]) and CheckMouseInButton then
  begin
    SrcR := Rect(0, 8, 8, 13);
    if SplitterWay in [swLeft, swUp] then
      SrcR1 := Rect(16, 0, 24, 11)
    else
      SrcR1 := Rect(32, 0, 40, 11);
  end
  else
  begin
    SrcR := Rect(0, 2, 8, 7);
    if SplitterWay in [swLeft, swUp] then
      SrcR1 := Rect(8, 0, 17, 11)
    else
      SrcR1 := Rect(24, 0, 33, 11);
  end;
  DrawRectangle( ACanvas, AResBmp.Canvas, FButtonRect, SrcR, True );
  DesR.Top := FButtonRect.Top + ( HeightOfRect(FButtonRect) - HeightOfRect(SrcR1) ) div 2;
  DesR.Bottom := DesR.Top + HeightOfRect(SrcR1);
  ACanvas.CopyRect(DesR, AResBmp.Canvas, SrcR1);
end;

end.
