unit JxdTypes;

interface
uses
  Windows, SysUtils, Classes, WinSock2,Types;
  
type
  TPPGUID = record
    case Integer of
      0: (
        UserID: DWORD;
        SerialID: DWORD;
        );
      1: (
        GUID: LONGLONG);
  end;

  TReplyData = record
    Buffer: PChar;
    BufferLength: Integer;
  end;
  
implementation

end.
