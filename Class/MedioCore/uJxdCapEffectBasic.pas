unit uJxdCapEffectBasic;

interface

uses
  Windows, Graphics, Classes, Forms;

type
  TxdCapEffectBasic = class
  public
    procedure DoOverlayEffer(Sender: TObject; MemDC: HDC; const AWidth, AHeight: Integer); virtual; abstract;
  end;

implementation


end.
