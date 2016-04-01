unit uJxdPlayerConsts;

interface

{$DEFINE Debug}

uses
  Windows, SysUtils
  {$IFDEF Debug}
  , uDebugInfo
  {$ENDIF};

type
  

const
  

function  GetSegmentSize(const AFileName: string): Cardinal;
function  GetFileMusicStyle(const AFileName: string): TMusicStyle;
procedure Dbg(const AInfo: string); overload;
procedure Dbg(const AInfo: string; const Args: array of const); overload;

implementation

{$IFDEF Debug}
var
  _Index: Integer = 1;
{$ENDIF}




end.
