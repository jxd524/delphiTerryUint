unit uDebugInfo;

interface
uses
  Windows, Classes, SysUtils;

procedure _Log(const AInfo: string; const AFileName: string = 'log.txt'); overload;
procedure _Log(const AInfo: string; const Args: array of const; const AFileName: string = 'log.txt'); overload;
procedure _Debug(const AInfo: string); overload;
procedure _Debug(const AInfo: string; const Args: array of const); overload;
function  DataToHex(pBuf: PChar; nBufLen: Integer; bSpace: Boolean = False): string;

implementation

function  DataToHex(pBuf: PChar; nBufLen: Integer; bSpace: Boolean): string;
var
  pText: PChar;
  i: Integer;
begin
  GetMem( pText, nBufLen * 3 );
  try
    FillChar( pText^, nBufLen * 3, 0 );
    BinToHex(pBuf, pText, nBufLen);
    if bSpace then
    begin
      nBufLen := nBufLen * 2;
      Result := '';
      for i := 0 to nBufLen - 1 do
      begin
        Result := Result + pText[i];
        if ((i + 1) mod 2) = 0 then
          Result := Result + ' ';
      end
    end
    else
      Result := pText;
  finally
    FreeMem( pText );
  end;
end;

procedure _Debug(const AInfo: string);
begin
  OutputDebugString( PChar(AInfo) );
end;

procedure _Debug(const AInfo: string; const Args: array of const); overload;
begin
  OutputDebugString( PChar(Format(AInfo, Args)) );
end;

procedure _Log(const AInfo: string; const AFileName: string);
const
  CtCRTL = #13#10;
  CtSize = Length(CtCRTL);
var
  f: HWND;
  FileStream: TFileStream;
  strData: string;
begin
  f := CreateFile( PAnsiChar(AFileName),
                   GENERIC_READ	or GENERIC_WRITE,
                   FILE_SHARE_READ or FILE_SHARE_WRITE,
                   nil,
                   OPEN_ALWAYS,
                   FILE_ATTRIBUTE_ARCHIVE,
                   0	);
  if f = 0 then Exit;
  strData := FormatdateTime('yyyyÄêmmÔÂddÈÕ hh:nn:ss    ',Now());
  FileStream := TFileStream.Create( f );
  try
    FileStream.Position := FileStream.Size;
    if FileStream.Size > 0 then
      FileStream.WriteBuffer( CtCRTL[1], CTSize );
    FileStream.WriteBuffer( strData[1], Length(strData) );
    FileStream.WriteBuffer( AInfo[1], Length(AInfo) );
  finally
    FileStream.Free;
  end;
end;

procedure _Log(const AInfo: string; const Args: array of const; const AFileName: string); overload;
begin
  _Log( Format(AInfo, Args), AFileName );
end;


end.
