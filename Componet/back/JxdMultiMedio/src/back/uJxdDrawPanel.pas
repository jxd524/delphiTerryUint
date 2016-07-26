unit uJxdDrawPanel;

interface
uses
  Windows, Classes, ExtCtrls, Controls, Messages, Graphics;

type
  TxdDrawPanel = class(TCustomPanel)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    procedure Loaded; override;
    procedure Paint; override;
  private
    FBackBitmap: TBitmap;
    FStretchBmp: Boolean;
    procedure SetBackBitmap(const Value: TBitmap);
    procedure SetStretchBmp(const Value: Boolean);
  published
    property Align;
    property Alignment;
    property Anchors;
    property Color;
    property TabStop;
    property OnClick;
    property OnDblClick;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnCanResize;

    property StretchBmp: Boolean read FStretchBmp write SetStretchBmp;
    property BackBitmap: TBitmap read FBackBitmap write SetBackBitmap;
  end;

implementation

{ TxdDrawPanel }

constructor TxdDrawPanel.Create(AOwner: TComponent);
begin
  inherited;
  Caption := '';
  BevelOuter := bvNone;
  BevelInner := bvNone;
  FBackBitmap := TBitmap.Create;
end;

destructor TxdDrawPanel.Destroy;
begin
  FBackBitmap.Free;
  inherited;
end;

procedure TxdDrawPanel.Loaded;
begin
  inherited;
  Caption := '';
  BevelOuter := bvNone;
  BevelInner := bvNone;
end;

procedure TxdDrawPanel.Paint;
var
  R: TRect;
begin
  if Assigned(FBackBitmap) and (FBackBitmap.Width > 0) and (FBackBitmap.Height > 0) then
  begin
    if not DoubleBuffered then DoubleBuffered := True;
    if FStretchBmp then
      Canvas.CopyRect( Rect(0, 0, Width, Height), FBackBitmap.Canvas, Rect(0, 0, FBackBitmap.Width, FBackBitmap.Height) )
    else
    begin
      R := Rect( 0, 0, FBackBitmap.Width, FBackBitmap.Height );
      if R.Right < Width then
        OffsetRect( R, (Width - R.Right) div 2, 0 );
      if R.Bottom < Height then
        OffsetRect( R, 0, (Height - R.Bottom) div 2 );
      BitBlt( Canvas.Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, FBackBitmap.Canvas.Handle, 0, 0, SRCCOPY );
    end;
  end;
  inherited;
end;

procedure TxdDrawPanel.SetBackBitmap(const Value: TBitmap);
begin
  FBackBitmap.Assign( Value );
  Invalidate;
end;

procedure TxdDrawPanel.SetStretchBmp(const Value: Boolean);
begin
  if FStretchBmp <> Value then
  begin
    FStretchBmp := Value;
    Invalidate;
  end;
end;

end.
