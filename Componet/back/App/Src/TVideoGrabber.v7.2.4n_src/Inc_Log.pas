{………………………………………………………………………………………………………………………………………………………………………………………………………………}
//                               Log functions
{………………………………………………………………………………………………………………………………………………………………………………………………………………}

const
   //_____ error report levels
   LTRACE = 0;
   LINFO = 1;
   LWARNING = 2;
   LERROR  = 3;

type
   TLog = class (TObject)
   public
      Data: TStringList;
      Time: Dword;
      FirstLog: Boolean;
      cs: TRtlCriticalSection;

      constructor Create;
      destructor Destroy; override;
   end;

var
   FLog: TLog = nil;
   Log_DoLogToFile: Boolean = false;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
constructor TLog.Create;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   Data := TStringList.Create;
   Time := 0;
   FirstLog := True;
   InitializeCriticalSection (cs);
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
destructor TLog.Destroy;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   Data.Free;
   DeleteCriticalSection (cs);
   inherited Destroy;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function WriteLogToFile: Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   i: LongInt;
begin
   Result := false;
   try
      i := FLog.Data.Add ('}');
      FLog.Data.SaveToFile ('vidgrablog.rtf');
      FLog.Data.Delete (i);
      Result := true;
   except
     on E: Exception do;
   end;
end;

const
   sPar = '\par';
   sBlack = '\cf0 ';
   sGrey = '\cf1 ';
   sBlue = '\cf2 ';
   sGreen = '\cf3 ';
   sRed = '\cf4 ';
   sPurple = '\cf5 ';
   sYellow = '\cf6 ';

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
procedure WriteLog (SeparatorChar: pChar; sColor: String; s: string);
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
// writes the log string to the DEBUGFILE file
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   DifTime: Dword;
   //LogPath: string;
begin
      if not Log_DoLogToFile then Exit;

      if not assigned (FLog) then begin
         FLog := TLog.Create;
         FLog.Data.Add ('{\rtf1\ansi\ansicpg1252\deff0\deflang1036{\fonttbl{\f0\fmodern\fprq1\fcharset0 Courier New;}}');
         FLog.Data.Add ('{\colortbl ;\red128\green128\blue128;\red0\green0\blue255;\red0\green128\blue0;\red255\green0\blue0;\red128\green0\blue128;\red255\green255\blue0;}');
         FLog.Data.Add ('\viewkind4\uc1\pard\b\f0\fs20');
         WriteLog ('=', sBlack, 'BEGIN TVideoGrabber v' + TVideoGrabberVersion + ' process log');
      end;
      EnterCriticalSection (FLog.cs);
      DifTime := FLog.Time;
      FLog.Time := GetTickCount;
      if DifTime <> 0 then begin
         s:= FormatFloat ('0.000', ((GetTickCount - DifTime) / 1000)) + ' ' + s;
      end;
      if assigned (SeparatorChar) then begin
         FLog.Data.Add (sPar);
         FLog.Data.Add (sYellow + stringOfChar (SeparatorChar^, 70) + sPar);
         FLog.Data.Add (sColor + s + sPar);
         FLog.Data.Add (sYellow + stringOfChar (SeparatorChar^, 70) + sPar);
         FLog.Data.Add ('\par');
      end
      else begin
         FLog.Data.Add (sColor + s + sPar);
      end;
      WriteLogToFile; // stops writing upon failure
      LeaveCriticalSection (FLog.cs);
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function WriteLogBool (Yes: Boolean; sColor: String; Text: string): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   if Yes then begin
      WriteLog (nil, sColor, Text + ': YES');
   end
   else begin
      WriteLog (nil, sColor, Text + ': no');
   end;
   Result := Yes;
end;


