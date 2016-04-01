unit uJxdPanel;

interface
uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Messages, Forms, Graphics, uJxdCustomPanel,
  uJxdDrawSub, uJxdGuiStyle, uJxdParseGradient;

type
  TxdPanel = class(TxdCustomPanel)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    procedure DrawPanel(ABufBitmap: TBitmap); override;
    procedure OnDrawInfoChanged(Sender: TObject);
  private
    FTransColor: TColor;
    FIsTransColor: Boolean;
    FBackBmpInfo: TBitmapInfo;
    FGradient: TGradientDrawInfo;
    FLines: TLineDrawInfo;
    procedure SetIsTransColor(const Value: Boolean);
    procedure SetTransColor(const Value: TColor);
    procedure SetBackBmpInfo(const Value: TBitmapInfo);
    procedure SetGradient(const Value: TGradientDrawInfo);
    procedure SetLines(const Value: TLineDrawInfo);
  published
    property TransColor: TColor read FTransColor write SetTransColor default clFuchsia;
    property IsTransColor: Boolean read FIsTransColor write SetIsTransColor default True;
    property BackBitmap: TBitmapInfo read FBackBmpInfo write SetBackBmpInfo;
    property Gradient: TGradientDrawInfo read FGradient write SetGradient;
    property Lines: TLineDrawInfo read FLines write SetLines;
  end;
  
implementation

{ TxdPanel }

constructor TxdPanel.Create(AOwner: TComponent);
begin
  inherited;
  FTransColor := clFuchsia;
  FIsTransColor := True;
  FBackBmpInfo := TBitmapInfo.Create;
  FBackBmpInfo.OnChange := OnDrawInfoChanged;
  FGradient := TGradientDrawInfo.Create;
  FGradient.OnChange := OnDrawInfoChanged;
  FLines := TLineDrawInfo.Create;
  FLines.OnChange := OnDrawInfoChanged;
end;

destructor TxdPanel.Destroy;
begin
  FBackBmpInfo.Free;
  FGradient.Free;
  FLines.Free;
  inherited;
end;

procedure TxdPanel.DrawPanel(ABufBitmap: TBitmap);
var
  SrcR, DestR: TRect;
begin
  if (FBackBmpInfo.Bitmap.Width > 0) and (FBackBmpInfo.Bitmap.Height > 0) then
  begin
    SrcR := Rect( 0, 0, FBackBmpInfo.Bitmap.Width, FBackBmpInfo.Bitmap.Height );
    DestR := Rect( 0, 0, ABufBitmap.Width, ABufBitmap.Height );
    DrawRectangle( FBackBmpInfo.Bitmap, ABufBitmap.Canvas, SrcR, DestR, FBackBmpInfo.BitmapDrawStyle, IsTransColor, TransColor );
  end
  else
  begin
    DrawGradientInfo( ABufBitmap.Canvas, FGradient.ParseGradientNormal, FGradient.GradientWay = gwLeftToRigth, 0, 0, Width, Height );
  end;
  DrawLinesInfo( ABufBitmap.Canvas, Lines, Width, Height );
  if not Enabled then
    GrapCanvas( ABufBitmap.Canvas, ABufBitmap.Width, ABufBitmap.Height, TransColor );
end;

procedure TxdPanel.OnDrawInfoChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TxdPanel.SetBackBmpInfo(const Value: TBitmapInfo);
begin
  FBackBmpInfo.Assign(Value);
end;

procedure TxdPanel.SetGradient(const Value: TGradientDrawInfo);
begin
  FGradient.Assign( Value );
end;

procedure TxdPanel.SetIsTransColor(const Value: Boolean);
begin
  if FIsTransColor <> Value then
  begin
    FIsTransColor := Value;
    Invalidate;
  end;
end;

procedure TxdPanel.SetLines(const Value: TLineDrawInfo);
begin
  FLines.Assign( Value );
end;

procedure TxdPanel.SetTransColor(const Value: TColor);
begin
  if FTransColor <> Value then
  begin
    FTransColor := Value;
    Invalidate;
  end;
end;

end.
