unit uJxdPlayerConsts;

interface

{$DEFINE Debug}

uses
  Windows, SysUtils
  {$IFDEF Debug}
  , uDebugInfo
  {$ENDIF};

type
  TMusicStyle = (msWMV, msMPEG, msRMVB, msFLV, msMP4, msHelp_ffdShow, msNULL);

const
  CtFileBlockExtName = '.fb';
  CtDownConfigExtName = '.dc';
  CtDownTempSingName = '_-_TEMP';
  CtDefaultSegmentSize = 1024 * 4;
  CtWMVSegmentSize = 1024 * 8;
  CtMpegSegmentSize = 1024 * 64;

function  GetSegmentSize(const AFileName: string): Cardinal;
function  GetFileMusicStyle(const AFileName: string): TMusicStyle;
procedure Dbg(const AInfo: string); overload;
procedure Dbg(const AInfo: string; const Args: array of const); overload;

implementation

{$IFDEF Debug}
var
  _Index: Integer = 1;
{$ENDIF}

function  GetSegmentSize(const AFileName: string): Cardinal;
var
  mStyle: TMusicStyle;
begin
  mStyle := GetFileMusicStyle( AFileName );
  case mStyle of
    msWMV:  Result := CtWMVSegmentSize;
    msMPEG: Result := CtWMVSegmentSize;
    msRMVB: Result := CtMpegSegmentSize;
    msFLV:  Result := CtWMVSegmentSize;
    msMP4:  Result := CtWMVSegmentSize;
    else    Result := CtDefaultSegmentSize;
  end;
end;

function  GetFileMusicStyle(const AFileName: string): TMusicStyle;
var
  ext: string;
  mStyle: TMusicStyle;
  function CheckFileStyle(const AStyle: TMusicStyle; const AExts: array of string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := Low(AExts) to High(AExts) do
    begin
      if CompareText(ext, AExts[i]) = 0 then
      begin
        Result := True;
        mStyle := AStyle;
        Break;
      end;
    end;
  end;
begin
  //此函数决定在线播放的能力
  mStyle := msNULL;
  ext := ExtractFileExt(AFileName);
  ext := StringReplace( ext, CtDownTempSingName, '', [rfReplaceAll] );

  if not CheckFileStyle(msWMV, ['.wmv']) then
    if not CheckFileStyle(msMPEG, ['.mpg', '.mpeg']) then
      if not CheckFileStyle(msRMVB, ['.rm', '.rmvb']) then
        if not CheckFileStyle(msFLV, ['.flv', '.f4v']) then
          if not CheckFileStyle(msMP4, ['.mp4']) then ;

  Result := mStyle;
end;


procedure Dbg(const AInfo: string);
begin
  {$IFDEF Debug}
  _Log( IntToStr(_Index) + '  ' + AInfo, 'P2SP_Player_Debug.txt' );
  InterlockedIncrement( _Index );
  {$ENDIF}
  OutputDebugString( PChar(AInfo) );
end;

procedure Dbg(const AInfo: string; const Args: array of const);
begin
  Dbg( Format(AInfo, Args) );
end;


end.
