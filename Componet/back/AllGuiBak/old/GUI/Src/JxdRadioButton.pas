unit JxdRadioButton;

interface

uses
  SysUtils, Classes, Controls, Windows, Messages, StdCtrls, Graphics;

type
  TJxdRadioButton = class(TRadioButton)
  private
    { Private declarations }
    FCanvas: TCanvas;
    FCaption: string;
    procedure setCaption(const Value: string);
  protected
    { Protected declarations }
    procedure CmTextChanged(var Message: TMessage); message WM_SETTEXT;
    procedure CmTest(var Message: TMessage); message WM_NCPaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Caption: string read FCaption write setCaption;
  end;

implementation

{ TRadioButton1 }

procedure TJxdRadioButton.CmTest(var Message: TMessage);
var
  R: TRect;
begin
//  //inherited;
//  R := GetClientRect;
//  FCanvas.Brush.Style := bsClear;
//  DrawText(FCanvas.Handle, PChar(Caption), Length(Caption), R, 0);
//  TextOut(FCanvas.Handle, 0, 0, PChar(Caption), Length(Caption));
end;

procedure TJxdRadioButton.CmTextChanged(var Message: TMessage);
begin
//  inherited;

//  TextOut(FCanvas.Handle, 0, 0, PChar('xxffffffffffx'), Length('xddddddddddddddddxx'));
end;

constructor TJxdRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TJxdRadioButton.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TJxdRadioButton.setCaption(const Value: string);
begin
  FCaption := Value;
  Invalidate;
end;

end.
