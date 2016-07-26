unit uJxdStringGrid;

interface

uses
  SysUtils, Classes, Controls, Grids, BaseGrid, AdvGrid, Messages;

type
  TJxdStringGrid = class(TAdvStringGrid)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure WM_MouseWheel(var Msg:TMessage); message WM_MOUSEWHEEL;
    procedure WMKeyDown(var Msg:TWMKeydown); message WM_KEYDOWN;
  public
    { Public declarations }
  published
    { Published declarations }
  end;

implementation
{ TAdvStringGrid1 }

procedure TJxdStringGrid.WMKeyDown(var Msg: TWMKeydown);
begin
  if msg.CharCode = 1 then
  ;
end;

procedure TJxdStringGrid.WM_MouseWheel(var Msg: TMessage);
begin

end;

end.
