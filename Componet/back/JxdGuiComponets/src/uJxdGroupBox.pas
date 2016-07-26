unit uJxdGroupBox;

interface
uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Messages, uJxdCustomPanel, Graphics, uJxdGuiDef;

{$M+}
type
  TJxdGroupBox = class( TJxdCustomPanel )
  private
    FChangeColor: TChangeToColor;
    FLineColor: TColor;
    FCaptionLeftSpace: Integer;
    FCaptionCerten: Boolean;
    FLineMargin: Integer;
    procedure PaintBackground(ABmp: TBitmap);
    procedure SetChangeColor(const Value: TChangeToColor);
    procedure SetLineColor(const Value: TColor);
    procedure SetCaptionLeftSpace(const Value: Integer);
    procedure SetCaptionCerten(const Value: Boolean);
    procedure SetLineMargin(const Value: Integer);
  protected
    procedure DrawPanel(ABufBitmap: TBitmap); override;
    function DrawCaption(ABufBitmap: TBitmap): TRect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ChangeColor: TChangeToColor read FChangeColor write SetChangeColor;
    property LineColor: TColor read FLineColor write SetLineColor;
    property CaptionLeftSpace: Integer read FCaptionLeftSpace write SetCaptionLeftSpace;
    property CaptionCerten: Boolean read FCaptionCerten write SetCaptionCerten;
    property LineMargin: Integer read FLineMargin write SetLineMargin;
  end;

implementation

uses uBitmapHandle;

{ TJxdGroupBox }

constructor TJxdGroupBox.Create(AOwner: TComponent);
begin
  inherited;
  FLineMargin := 3;
  FLineColor := $FFFFFF;
  FCaptionLeftSpace := 10;
  FCaptionCerten := True;
end;

destructor TJxdGroupBox.Destroy;
begin

  inherited;
end;

function TJxdGroupBox.DrawCaption(ABufBitmap: TBitmap): TRect;
var
  nWidth, nHeight: Integer;
  strCaption: string;
begin
  strCaption := Caption;
  nWidth := Canvas.TextWidth( strCaption );
  nHeight := Canvas.TextHeight( strCaption );
  if FCaptionCerten then
    Result.Left := (Width - nWidth) div 2
  else
    Result.Left := CaptionLeftSpace;
  Result.Top := LineMargin;
  Result.Right := Result.Left + nWidth;
  Result.Bottom := Result.Top + nHeight;
  ABufBitmap.Canvas.Brush.Style := bsClear;
  ABufBitmap.Canvas.Font.Assign( Font );
  DrawText( ABufBitmap.Canvas.Handle, PChar(Caption), Length(Caption), Result, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
end;

procedure TJxdGroupBox.DrawPanel(ABufBitmap: TBitmap);
var
  CaptionR, R: TRect;
begin
  inherited;
  PaintBackground( ABufBitmap );
  CaptionR := DrawCaption( ABufBitmap );

  R.Left := LineMargin;
  R.Top := CaptionR.Bottom div 2;
  R.Bottom := Height - LineMargin + 1;
  R.Right := Width - LineMargin + 1;

  ABufBitmap.Canvas.Pen.Color := LineColor;
  ABufBitmap.Canvas.Pen.Width := 1;
  with ABufBitmap.Canvas do
  begin
    MoveTo( CaptionR.Left, R.Top );
    LineTo( R.Left, R.Top );
    LineTo( R.Left, R.Bottom );
    LineTo( R.Right, R.Bottom );
    LineTo( R.Right, R.Top );
    LineTo( CaptionR.Right, R.Top );
  end;
end;

procedure TJxdGroupBox.PaintBackground(ABmp: TBitmap);
begin
  GetControlBackground( Parent, ABmp, Rect(Left, Top, Left + ABmp.Width, Top + ABmp.Height));
end;

procedure TJxdGroupBox.SetCaptionCerten(const Value: Boolean);
begin
  if FCaptionCerten <> Value then
  begin
    FCaptionCerten := Value;
    Invalidate;
  end;
end;

procedure TJxdGroupBox.SetCaptionLeftSpace(const Value: Integer);
begin
  if FCaptionLeftSpace <> Value then
  begin
    FCaptionLeftSpace := Value;
    Invalidate;
  end;
end;

procedure TJxdGroupBox.SetChangeColor(const Value: TChangeToColor);
begin
  FChangeColor.Assign( Value );
end;

procedure TJxdGroupBox.SetLineColor(const Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Invalidate;
  end;
end;

procedure TJxdGroupBox.SetLineMargin(const Value: Integer);
begin
  if FLineMargin <> Value then
  begin
    FLineMargin := Value;
    Invalidate;
  end;
end;

end.
