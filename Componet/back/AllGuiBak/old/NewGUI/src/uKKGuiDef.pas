unit uKKGuiDef;

interface
uses
  Windows, Classes, Controls, Graphics, uKKBitmapHandle;

type
  TWideCaption = type WideString;
  TChangeToColor = class(TPersistent)
  private
    FOwner: TControl;
    FChangeToColor: TColor;
    FChangeHSB: THSBColor;
    FTransColor: TColor;
    FOnColorChanged: TNotifyEvent;
    procedure SetChangeHSB(const Value: THSBColor);
    procedure SetChangeToColor(const Value: TColor);
    function GetIsChange: Boolean;
    procedure SetTransColor(const Value: TColor);
  protected
    procedure OnChanged; virtual;
  public
    constructor Create(Control: TControl); virtual;
    procedure ChangeBitmap(Bmp: TBitmap);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property IsChange: Boolean read GetIsChange;
    property TransColor: TColor read FTransColor write SetTransColor default clFuchsia;
    property ChangeToColor: TColor read FChangeToColor write SetChangeToColor;
    property ChangeHSB: THSBColor read FChangeHSB write SetChangeHSB;
    property OnColorChanged: TNotifyEvent read FOnColorChanged write FOnColorChanged;
  end;

implementation

{ TChangeToColor }

procedure TChangeToColor.Assign(Source: TPersistent);
var
  src: TChangeToColor;
begin
  if Source is TChangeToColor then
  begin
    src := Source as TChangeToColor;
    TransColor := src.TransColor;
    ChangeToColor := src.ChangeToColor;
  end;
end;

procedure TChangeToColor.AssignTo(Dest: TPersistent);
var
  dt: TChangeToColor;
begin
  if Dest is TChangeToColor then
  begin
    dt := Dest as TChangeToColor;
    dt.TransColor := TransColor;
    dt.ChangeToColor := ChangeToColor;
  end;
end;

procedure TChangeToColor.ChangeBitmap(Bmp: TBitmap);
begin
  if IsChange then
    SetBitmapHSB( Bmp.Canvas, Bmp.Width, Bmp.Height, ChangeHSB, FTransColor);
end;

constructor TChangeToColor.Create(Control: TControl);
begin
  FOwner := Control;
  FChangeToColor := -1;
  FTransColor := clFuchsia;
end;

function TChangeToColor.GetIsChange: Boolean;
begin
  Result := FChangeToColor <> -1;
end;

procedure TChangeToColor.OnChanged;
begin
  if Assigned(FOnColorChanged) then
    FOnColorChanged(Self);
end;

procedure TChangeToColor.SetChangeHSB(const Value: THSBColor);
var
  R: TRGBColor;
begin
  R := HSB2RGB( Value );
  FChangeToColor := RGB(R.Red, R.Green, R.Blue);
  FChangeHSB := Value;
  OnChanged;
  FOwner.Invalidate;
end;

procedure TChangeToColor.SetChangeToColor(const Value: TColor);
var
  R: TRGBColor;
begin
  if FChangeToColor <> Value then
  begin
    FChangeToColor := Value;
    R.Red := GetRValue( FChangeToColor );
    R.Green := GetGValue( FChangeToColor );
    R.Blue := GetBValue( FChangeToColor );
    FChangeHSB := RGB2HSB( R );
    OnChanged;
    FOwner.Invalidate;
  end;
end;

procedure TChangeToColor.SetTransColor(const Value: TColor);
begin
  FTransColor := Value;
end;

end.
