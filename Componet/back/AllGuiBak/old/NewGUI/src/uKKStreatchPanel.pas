unit uKKStreatchPanel;

interface
uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Messages, uKKCustomPanel, Graphics, uKKGuiDef;

{$M+}
type
  TKKStreatchPanel = class( TKKCustomPanel )
  private
    FChangeColor: TChangeToColor;
    FOrgBmp: TBitmap;
    FOrgR: TRect;
    FUpToDown: Boolean;
    procedure SetOrgBmp(const Value: TBitmap);
    procedure SetUpToDown(const Value: Boolean);
    procedure SetChangeColor(const Value: TChangeToColor);
  protected
    procedure DrawPanel(ABufBitmap: TBitmap); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ChangeColor: TChangeToColor read FChangeColor write SetChangeColor;
    property StreatchBitmap: TBitmap read FOrgBmp write SetOrgBmp;
    property UpToDown: Boolean read FUpToDown write SetUpToDown;
  end;
implementation

{ TKKStreatchPanel }
uses
  uKKBitmapHandle;

constructor TKKStreatchPanel.Create(AOwner: TComponent);
begin
  inherited;
  FChangeColor := TChangeToColor.Create( Self );
  FOrgBmp := TBitmap.Create;
end;

destructor TKKStreatchPanel.Destroy;
begin
  FChangeColor.Free;
  FOrgBmp.Free;
  inherited;
end;

procedure TKKStreatchPanel.DrawPanel(ABufBitmap: TBitmap);
var
  Bmp: TBitmap;
begin
  if not FOrgBmp.Empty then
  begin
    FOrgR := Rect( 0, 0, FOrgBmp.Width, FOrgBmp.Height );
    if not ChangeColor.IsChange then
      DrawRectangle( ABufBitmap.Canvas, FOrgBmp.Canvas,  ClientRect, FOrgR, FUpToDown )
    else
    begin
      Bmp := TBitmap.Create;
      try
        Bmp.Assign( FOrgBmp );
        ChangeColor.ChangeBitmap( Bmp );
        DrawRectangle( ABufBitmap.Canvas, Bmp.Canvas,  ClientRect, FOrgR, FUpToDown );
      finally
        Bmp.Free;
      end;
    end;
  end;
end;

procedure TKKStreatchPanel.SetChangeColor(const Value: TChangeToColor);
begin
  FChangeColor.Assign( Value );
end;

procedure TKKStreatchPanel.SetOrgBmp(const Value: TBitmap);
begin
  FOrgBmp.Assign( Value );
  if not FOrgBmp.Empty then
    FOrgR := Rect( 0, 0, FOrgBmp.Width, FOrgBmp.Height );
  Invalidate;
end;

procedure TKKStreatchPanel.SetUpToDown(const Value: Boolean);
begin
  FUpToDown := Value;
  Invalidate;
end;

end.
