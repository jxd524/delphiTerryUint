unit uKKLabel;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Windows, uKKGuiDef, Graphics;

type
  TKKLabel = class(TLabel)
  private
    FCaption: TWideCaption;
    procedure SetWideCaption(const Value: TWideCaption);
    { Private declarations }
  protected
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
  public
    { Public declarations }
  published
    property Caption: TWideCaption read FCaption write SetWideCaption;
  end;

implementation

{ TKKLabel }

procedure TKKLabel.DoDrawText(var Rect: TRect; Flags: Integer);
begin
  DrawTextW(Canvas.Handle, PWideChar(FCaption), Length(FCaption), Rect, 0);
end;

procedure TKKLabel.SetWideCaption(const Value: TWideCaption);
begin
  FCaption := Value;
  Invalidate;
end;

end.
