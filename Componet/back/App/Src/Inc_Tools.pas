{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function CheckRegKeyId: string;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   OkLeft, OkRight: Boolean;
begin
   if length(RegKeyId) > 2 then begin
      OkLeft := RegKeyId[1] = '\';
      OkRight := RegKeyId[length(RegKeyId)] = '\';
      if OkLeft and OkRight then begin
         Result := RegKeyId;
      end
      else begin
         Result := RegKeyId;
         if not OkLeft then begin
            Result := '\' + Result;
         end;
         if not OkRight then begin
            Result := Result + '\';
         end;
      end;
   end
   else begin
      Result := '\Software\' + APPNAME + '\';
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function DeviceRegKeyId (Devices: TDevices; Index: LongInt): string;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
      Result := '';
      if Index < 0 then Exit;
      if Index >= Devices.FDevices.Count then Exit;
      Result := CheckRegKeyId + 'Devices\' + TCapDeviceInfo(Devices.FDevices.Objects[Index]).RegistryStringId;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function GlobalRegKeyId: string;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
      Result := CheckRegKeyId + 'Global\';
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function FilterRegKeyId (var Filter: IBaseFilter): string;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   FilterInfo: TFilterInfo;
begin
      Result := '';
      if not assigned (Filter) then Exit;
      if Filter.QueryFilterInfo (FilterInfo) <> S_OK then Exit;
      FilterInfo.pGraph := nil;
      Result := CheckRegKeyId + 'Filters\' + FilterInfo.achName;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function DataTypeToRegData(Value: Integer): TRegDataType;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
  if Value = REG_SZ then Result := rdstring
  else if Value = REG_EXPAND_SZ then Result := rdExpandstring
  else if Value = REG_DWORD then Result := rdInteger
  else if Value = REG_BINARY then Result := rdBinary
  else Result := rdUnknown;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
{                              TREGISTRY2 CLASS
{………………………………………………………………………………………………………………………………………………………………………………………………………………}

var
   RegKeyWriteAllowed: Boolean = false;
   RegKeyWriteTested: Boolean = false;

type

TRegistry_ = class (TRegistry)
end;

   TRegistry2 = class (TObject)
   private
      Registry: TRegistry_;
   public
      constructor Create (MustUseRootKey: Boolean);
      destructor Destroy; override;
      function CreateKey (const Key: string): Boolean;
      function DeleteKey(const Key: string): Boolean;
      function DeleteValue(const Name: string): Boolean;
      function OpenKey(const Key: String; Cancreate: boolean): Boolean;
      function GetData(const Name: string; Buffer: Pointer; BufSize: LongWord; var RegData: TRegDataType; var SizeRead: Integer): Boolean;
      function GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean;
      function KeyExists(const Key: string): Boolean;
      function Readstring(const Name: string; var stringRead: string): Boolean;
      function ReadInteger(const Name: string; var IntRead: Integer): Boolean;
      function ReadBool(const Name: string; var BoolRead: Boolean): Boolean;
      function ReadDouble (const Name: string; var DoubleRead: Double): Boolean;
      function ReadBinaryDataFixedSize (const Name: string; var Buffer; SizeToRead: Integer): Boolean;
      function ReadBinaryData (const Name: string; var Buffer; SizeToRead: Integer; var SizeRead: Integer): Boolean;
      function ValueExists(const Name: string): Boolean;
      procedure WriteString(const Name, Value: string);
      procedure WriteInteger(const Name: string; Value: Integer);
      procedure WriteBool(const Name: string; Value: Boolean);
      procedure WriteDouble (const Name: string; Value: Double);
      procedure WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
   end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
procedure TestRegKeyWrite;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   Reg: TRegistry;
   TestKey: String;
begin
   if RegRootKey = RR_HKEY_LOCAL_MACHINE then begin
      Reg := TRegistry.Create;
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Randomize;
      TestKey := RegKeyId + IntToStr (Random(10000));
      try
         RegKeyWriteAllowed := False;
         if Reg.OpenKey (TestKey, true) then begin
            RegKeyWriteAllowed := True;
         end;
      finally
         if RegKeyWriteAllowed then begin
            Reg.DeleteKey (TestKey);
         end;
         Reg.Free;
      end;
   end
   else begin
      RegKeyWriteAllowed := True;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
constructor TRegistry2.Create (MustUseRootKey: Boolean);
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   Registry := TRegistry_.Create;
   if (RegRootKey = RR_HKEY_LOCAL_MACHINE) or MustUseRootKey then begin
      Registry.RootKey := HKEY_LOCAL_MACHINE;
   end;
   if not RegKeyWriteTested then begin
      RegKeyWriteTested := True;
      TestRegKeyWrite;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
destructor TRegistry2.Destroy;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   Registry.Free;
   inherited Destroy;
end;


{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.OpenKey(const Key: String; Cancreate: boolean): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   if RegKeyWriteAllowed then begin
      Result := Registry.OpenKey (Key, CanCreate);
   end
   else begin
      Result := Registry.OpenKeyReadOnly (Key);
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.CreateKey (const Key: string): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   if RegKeyWriteAllowed then begin
      Result := Registry.CreateKey (Key);
   end
   else begin
      Result := True;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.DeleteValue(const Name: string): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   if RegKeyWriteAllowed then begin
      Result := Registry.DeleteValue (Name);
   end
   else begin
      Result := True;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.DeleteKey (const Key: string): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   if RegKeyWriteAllowed then begin
      Result := Registry.DeleteKey (Key);
   end
   else begin
      Result := True;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
procedure TRegistry2.WriteString (const Name, Value: string);
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   if RegKeyWriteAllowed then begin
      Registry.WriteString (Name, Value);
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
procedure TRegistry2.WriteInteger (const Name: string; Value: Integer);
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   if RegKeyWriteAllowed then begin
      Registry.WriteInteger (Name, Value);
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
procedure TRegistry2.WriteBool (const Name: string; Value: Boolean);
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   if RegKeyWriteAllowed then begin
      Registry.WriteBool (Name, Value);
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
procedure TRegistry2.WriteDouble (const Name: string; Value: Double);
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   if RegKeyWriteAllowed then begin
      Registry.WriteFloat (Name, Value); // WriteFloat writes a double value
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
procedure TRegistry2.WriteBinaryData (const Name: string; var Buffer; BufSize: Integer);
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   if RegKeyWriteAllowed then begin
      Registry.WriteBinaryData (Name, Buffer, BufSize);
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.ValueExists(const Name: string): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   Result := Registry.ValueExists (Name);
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.KeyExists(const Key: string): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   Result := Registry.KeyExists (Key);
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.GetData (const Name: string; Buffer: Pointer; BufSize: LongWord; var RegData: TRegDataType; var SizeRead: Integer): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
  DataType: LongWord;
begin
  Result := false;
  DataType := REG_NONE;
  SizeRead := 0;
  If RegQueryValueEx(Registry.CurrentKey, PChar(Name), nil, @DataType, PByte(Buffer), @BufSize) = ERROR_SUCCESS then begin
     SizeRead := BufSize;
     RegData := DataTypeToRegData(DataType);
     Result := true;
  end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   Result := Registry.GetDataInfo (ValueName, Value);
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.Readstring (const Name: string; var stringRead: string): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   Len: Integer;
   RegData: TRegDataType;
   SizeRead: Integer;
begin
   Result := false;
   Len := Registry.GetDataSize(Name);
   if Len > 0 then begin
      Setstring(stringRead, nil, Len);
      GetData (Name, PChar(stringRead), Len, RegData, SizeRead);
      if (RegData = rdstring) or (RegData = rdExpandstring) then begin
         SetLength(stringRead, StrLen(PChar(stringRead)));
         Result := true;
      end;
   end;
   if not Result then begin
      stringRead := '';
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.ReadInteger (const Name: string; var IntRead: Integer): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   RegData: TRegDataType;
   SizeRead: Integer;
begin
   GetData (Name, @IntRead, SizeOf(Integer), RegData, SizeRead);
   Result := RegData = rdInteger;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.ReadDouble (const Name: string; var DoubleRead: Double): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   RegData: TRegDataType;
   SizeRead: Integer;
begin
   if not GetData (Name, @DoubleRead, SizeOf(Double), RegData, SizeRead) then begin
      Result := false;
   end
   else begin
      Result := ((RegData = rdBinary) and (SizeRead = SizeOf(Double)));
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.ReadBool(const Name: string; var BoolRead: Boolean): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   Value: Integer;
begin
  if ReadInteger (Name, Value) then begin
     BoolRead := Value <> 0;
     Result := true;
  end
  else begin
     Result := false;
  end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.ReadBinaryData (const Name: string; var Buffer; SizeToRead: Integer; var SizeRead: Integer): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   RegData: TRegDataType;
   Info: TRegDataInfo;
begin
   Result := false;
   SizeRead := 0;
   if Registry.GetDataInfo(Name, Info) then begin
      SizeRead := Info.DataSize;
      RegData := Info.RegData;
      if ((RegData = rdBinary) or (RegData = rdUnknown)) and (SizeRead = SizeToRead) then begin
          Registry.GetData (Name, @Buffer, SizeRead, RegData);
          Result := true;
      end;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TRegistry2.ReadBinaryDataFixedSize(const Name: string; var Buffer; SizeToRead: Integer): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   SizeRead: Integer;
begin
   Result := ReadBinaryData (Name, Buffer, SizeToRead, SizeRead);
   if Result then begin
      Result := SizeToRead = SizeRead;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function SaveFilterState (Filter: IUnknown; FilterRegKeyId: string): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   AMVfwCompressDialogs: IAMVfwCompressDialogs;
   pData: pByte;
   pSize: Integer;
   RegKey: TRegistry2;
   PersistStream: IPersistStream;
   Stream: IStream;
   StreamSize: LargeInt;
   StreamSizeInt: LongInt;
   NewPosition: LargeInt;
   BytesRead: Integer;
begin
   Result := false;
   if not assigned (Filter) then Exit;
   if FilterRegKeyId = '' then Exit;
   RegKey := TRegistry2.Create (False);
   if RegKey.OpenKey (FilterRegKeyId, true) then begin
      if Filter.QueryInterface(IPersistStream, PersistStream) = S_OK then begin
         if CreateStreamOnHGlobal (0, true, Stream) = S_OK then begin
            if PersistStream.Save(Stream, true) = S_OK then begin
               if Stream.Seek (0, STREAM_SEEK_END, StreamSize) = S_OK then begin
                  {$IFNDEF DELPHI_OR_BCB_3}
                  StreamSizeInt := StreamSize;
                  GetMem (pData, StreamSizeInt);
                  {$ELSE}
                  StreamSizeInt := Round (StreamSize);
                  GetMem (pData, StreamSizeInt);
                  {$ENDIF DELPHI_OR_BCB_3}
                  if Stream.Seek (0, STREAM_SEEK_SET, newPosition) = S_OK then begin
                     if Stream.Read (pData, StreamSizeInt, @BytesRead) = S_OK then begin
                        RegKey.WriteBinaryData ('WDMState', pData^, StreamSizeInt);
                        RegKey.WriteInteger ('WDMStateSize', StreamSizeInt);
                        Result := true;
                     end;
                  end;
                  FreeMem (pData);
               end;
            end;
            Stream := nil;
         end;
         PersistStream := nil;
      end;
      if Filter.QueryInterface (IAMVfwCompressDialogs, AMVfwCompressDialogs) = S_OK then begin
         pSize := AMVfwCompressDialogs.SendDriverMessage (ICM__GETSTATE, 0, 0);
         GetMem (pData, pSize);
         AMVfwCompressDialogs.SendDriverMessage (ICM__GETSTATE, DWORD (pData), pSize);
         RegKey.WriteBinaryData ('VfwState', pData^, pSize);
         FreeMem (pData);
         Result := true;
      end;
   end;
   RegKey.Free;
   if assigned (AMVfwCompressDialogs) then AMVfwCompressDialogs := nil;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function RestoreFilterState (Filter: IUnknown; FilterRegKeyId: string): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   AMVfwCompressDialogs: IAMVfwCompressDialogs;
   pData: pByte;
   pSize: Integer;
   RegKey: TRegistry2;
   PersistStream: IPersistStream;
   Stream: IStream;
   StreamSize: Integer;
   NewPosition: LargeInt;
   BytesWritten: Integer;
begin
   Result := false;
   if not assigned (Filter) then Exit;
   if FilterRegKeyId = '' then Exit;
   RegKey := TRegistry2.Create (False);
   if RegKey.OpenKey (FilterRegKeyId, false) then begin
      if Filter.QueryInterface(IPersistStream, PersistStream) = S_OK then begin
         if RegKey.ReadInteger ('WDMStateSize', StreamSize) then begin
            GetMem (pData, StreamSize);
            if RegKey.ReadBinaryDataFixedSize ('WDMState', pData^, StreamSize) then begin
               if CreateStreamOnHGlobal (0, true, Stream) = S_OK then begin
                  if Stream.Write (pData, StreamSize, @BytesWritten) = S_OK then begin
                     if Stream.Seek (0, STREAM_SEEK_SET, newPosition) = S_OK then begin
                        if PersistStream.Load (Stream) = S_OK then begin
                           Result := true;
                        end;
                     end;
                  end;
                  Stream := nil;
               end;
            end;
            FreeMem (pData);
         end;
         PersistStream := nil;
      end;
      if Filter.QueryInterface (IAMVfwCompressDialogs, AMVfwCompressDialogs) = S_OK then begin
         pSize := AMVfwCompressDialogs.SendDriverMessage (ICM__GETSTATE, 0, 0);
         GetMem (pData, pSize);
         if RegKey.ReadBinaryDataFixedSize ('VfwState', pData^, pSize) then begin
            AMVfwCompressDialogs.SendDriverMessage (ICM__SETSTATE, DWORD (pData), pSize);
            Result := true;
         end;
         FreeMem (pData);
      end;
   end;
   RegKey.Free;
   if assigned (AMVfwCompressDialogs) then AMVfwCompressDialogs := nil;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
{                                   XML CLASS
{………………………………………………………………………………………………………………………………………………………………………………………………………………}

type
   TXMLState = (csNone, csWaitIdentifier, csIdentifier, csWaitProperty, csProperty, csWaitValue, csValue, csCloseIdentifier);

TXML = class (TObject)
   FLevel: Integer;
   FLevelList: TStringList;
   FXMLList: TStringList;
   FCurrentXMLIndex: Integer;
   FXMLState: TXMLState;
   FGeneratedXML: TStringList;
   constructor Create;
   destructor Destroy; override;
   procedure ProcessCurWord (var CodeList: TStringList; var CurWord: String; XMLState: TXMLState);
   function ParseXML (XMLString: String): String;
   procedure ReInitXML; virtual;
   procedure DoXML (Id: String; Value: String); overload;
   procedure DoXML (Id: String; Value: Integer); overload;
   procedure UpdateValue (Id: String; Value: String);
   function  GenerateXML: Boolean;
   function GetGeneratedXML: String;
end;

TASFXMLDataType = (xdt_None, xdt_VideoStream, xdt_AudioStream);

TASFXMLData = class (TObject)
   ASFXMLDataType: TASFXMLDataType;
end;

TASFXML = class (TXML)
   procedure ReInitXML; override;
   function FilterStreams (UseVideo: Boolean; UseAudio: Boolean): Boolean;
   procedure UpdateASFValue (Id: String; Value: String; ASFXMLDataType: TASFXMLDataType);
end;

constructor TXML.Create;
begin
   FLevelList := TStringList.Create;
   FXMLList := TStringList.Create;
   FGeneratedXML := TStringList.Create;
   ReinitXML;
end;

destructor TXML.Destroy;
begin
   ReinitXML;
   FLevelList.Free;
   FXMLList.Free;
   FGeneratedXML.Free;
   inherited Destroy;
end;

procedure TXML.ProcessCurWord (var CodeList: TStringList; var CurWord: String; XMLState: TXMLState);
const
   STATEMENT = '   XML.DoXML (''';
var
   FinalWord: String;
begin
   FinalWord := trim (CurWord);

   case XMLState of
      csIDentifier: begin
         inc (FLevel);
         FLevelList.Add (FinalWord);
         FCurrentXMLIndex := FXMLList.Add ('<' + FLevelList[FLevel - 1]);
         CodeList.Add (STATEMENT + FXMLList[FCurrentXMLIndex] + ''', '''');');
      end;
      csCloseIDentifier: begin
         FCurrentXMLIndex := FXMLList.Add ('>' + FLevelList[FLevel - 1]);
         CodeList.Add (STATEMENT + FXMLList[FCurrentXMLIndex] + ''', '''');');
         if FLevel > 0 then begin
            dec (FLevel);
            if FLevelList.Count > FLevel then begin
               FLevelList.Delete (FLevel);
            end;
         end;
      end;
      csProperty: begin
         FCurrentXMLIndex := FXMLList.Add (FinalWord);
      end;
      csValue: begin
         CodeList.Add (STATEMENT + FXMLList[FCurrentXMLIndex] + ''', ''' + FinalWord + ''');');
         FXMLList[FCurrentXMLIndex] := FXMLList[FCurrentXMLIndex] + '=' + FinalWord;
         //CodeList.Add (FXMLList[FCurrentXMLIndex]);
      end;
   end;
   CurWord := '';
end;

function TXML.ParseXML (XMLString: String): String;
var
   Done: Boolean;
   i: integer;
   s: String;
   c, Oldc: Char;
   CurWord: String;
   NullVar: String;
   OutList: TStringList;
begin
   ReInitXML;
   OutList := TStringList.Create;
   s := XMLString;
   Done := False;
   i := 1;
   c := ' ';
   FXMLState := csNone;
   CurWord:= '';
   NullVar := ' ';
   while not Done do begin
      if i > length (s) then begin
         Done := True;
      end
      else begin
         OldC := c;
         c := s[i];
         if c = '"' then begin
            if FXMLState = csWaitValue then begin
               FXMLState := csValue;
               Curword := '';
            end
            else begin
               if FXMLState = csValue then begin
                  ProcessCurWord (OutList, CurWord, csValue);
                  FXMLState := csWaitProperty;
               end;
            end;
         end
         else begin
            if FXMLState = csValue then begin
               CurWord := CurWord + c;
            end
            else begin
               if (c in ['a'..'z']) or (c in ['A'..'Z']) or (c in ['0'..'9']) then begin
                  if FXMLState = csWaitIdentifier then begin
                     FXMLState := csIdentifier;
                     Curword := c;
                  end
                  else if FXMLState = csWaitValue then begin
                     FXMLState := csValue;
                     Curword := c;
                  end
                  else if FXMLState = csWaitProperty then begin
                     FXMLState := csProperty;
                     Curword := c;
                  end
                  else begin
                     Curword := Curword + c;
                  end;
               end
               else if ((Oldc = '/') and (c = '>')) then begin
                  ProcessCurWord (OutList, NullVar, csCloseIdentifier);
                  FXMLState := csNone;
               end
               else if ((Oldc = '<') and (c = '/')) then begin
                  ProcessCurWord (OutList, NullVar, csCloseIdentifier);
                  FXMLState := csNone;
               end
               else if c = '>' then begin
                  if CurWord <> '' then begin
                     ProcessCurWord (OutList, CurWord, FXMLState);
                     FXMLState := csNone;
                  end;
               end
               else if c = '<' then begin
                  FXMLState := csWaitIdentifier;
                  Curword := '';
               end
               else begin
                  if CurWord <> '' then begin
                     if FXMLState = csIdentifier then begin
                        ProcessCurword (OutList, CurWord, csIdentifier);
                        FXMLState := csWaitProperty;
                     end;
                     if FXMLState = csProperty then begin
                        ProcessCurword (OutList, CurWord, csProperty);
                        FXMLState := csWaitValue;
                     end;
                  end;
               end;
            end;
         end;
      end;
      inc (i);
   end;
   Result := OutList.Text;
   OutList.Free;
end;

function FindLastChar (s: string; c: Char): Integer;
var
   i: Integer;
   Done: Boolean;
begin
   Done := False;
   Result := 0;
   i := length (s);
   while not Done do begin
      if i < 1 then begin
         Done := True;
      end
      else begin
         if s[i] = c then begin
            Result := i;
            Done := True;
         end
         else begin
            dec (i);
         end;
      end;
   end;
end;

function TXML.GenerateXML: Boolean;
var
   i: Integer;
   s: string;
   OutString: String;
   p: Integer;
   Level: Integer;

   function Tab: String;
   begin
      Result := StringOfChar (' ', 5 * Level);
   end;
begin
   FGeneratedXML.Clear;
   OutString := '';
   Level := 0;
   for i := 0 to FXMLList.Count - 1 do begin
      s := FXMLList[i];
      if s[1] = '<' then begin
         if OutString <> '' then begin
            FGeneratedXML.Add (OutString + '>');
            OutString := '';
         end;
         OutString := Tab + s + ' ';
         inc (Level);
      end
      else if s[1] = '>' then begin
         if OutString <> '' then begin
            FGeneratedXML.Add (OutString + '>');
            OutString := '';
         end;
         Dec (Level);
         FGeneratedXML.Add (Tab + '</' + Copy (s, 2, MAXINT) + '>');
      end
      else begin
         p := pos ('=', s);
         if p > 0 then begin
            if OutString <> '' then begin
               FGeneratedXML.Add (OutString);
               OutString := '';
            end;
            OutString := Tab + Copy (s, 1, p) + '"' + Copy (s, p + 1, MAXINT) + '"';
         end;
      end;
   end;
   Result := FGeneratedXML.Count > 0;
end;

procedure TXML.UpdateValue (Id: String; Value: String);
var
   i: Integer;
   Done: Boolean;
   sId: String;
   sXML: String;
   lId: LongInt;
   p: LongInt;
begin
   sId := LowerCase (Id);
   lId := length(sId);
   Done := False;
   i := 0;
   while not Done do begin
      if i >= FXMLList.Count then begin
         Done := True;
      end
      else begin
         sXML := FXMLList[i];
         if copy (sXML, 1, lId) = sId then begin
            p := pos ('=', sXML);
            if p > 0 then begin
               FXMLList[i] := sId + '=' + Value;
            end;
         end;
         inc (i);
      end;
   end;
end;

procedure TXML.ReInitXML;
begin
   FXMLList.Clear;
   FLevelList.Clear;
   FLevel := 0;
   FXMLState := csNone;
   FGeneratedXML.Clear;
end;

procedure TXML.DoXML (Id: String; Value: Integer);
begin
   DoXML (Id, IntToStr (Value));
end;

procedure TXML.DoXML (Id: String; Value: String);
begin
   if length(Id) > 0 then begin
      if Id[1] in ['<','>'] then begin
         FXMLList.Add (Id);
      end
      else begin
         FXMLList.Add (Id + '=' + Value);
      end;
   end;
end;

function TXML.GetGeneratedXML: String;
begin
   Result := FGeneratedXML.Text;
end;

procedure TASFXML.ReInitXML;
var
   i: Integer;
begin
   for i := 0 to FXMLList.Count - 1 do begin
      if assigned (FXMLList.Objects[i]) then begin
         TASFXMLData (FXMLList.Objects[i]).Free;
      end;
   end;
   inherited;
end;

procedure TASFXML.UpdateASFValue (Id: String; Value: String; ASFXMLDataType: TASFXMLDataType);
var
   i: Integer;
   Done: Boolean;
   sId: String;
   sXML: String;
   lId: LongInt;
   p: LongInt;
begin
   sId := LowerCase (Id);
   lId := length(sId);
   Done := False;
   i := 0;
   while not Done do begin
      if i >= FXMLList.Count then begin
         Done := True;
      end
      else begin
         sXML := LowerCase (FXMLList[i]);
         if TASFXMLData(FXMLList.Objects[i]).ASFXMLDataType = ASFXMLDataType then begin
            if copy (sXML, 1, lId) = sId then begin
               p := pos ('=', sXML);
               if p > 0 then begin
                  FXMLList[i] := sId + '=' + Value;
               end;
            end;
         end;
         inc (i);
      end;
   end;
end;

function TASFXML.FilterStreams (UseVideo: Boolean; UseAudio: Boolean): Boolean;
var
   Done: Boolean;
   VideoStreamIndex: Integer;
   AudioStreamIndex: Integer;
   i: Integer;
   InStreamConfig: Boolean;
   s: String;
   StreamIndex: Integer;
   DeletingStream: Boolean;
   MustDecreaseStreamNumber: Boolean;
   NewStreamNumber: Integer;
   ASFXMLDataType: TASFXMLDataType;
begin
   Result := False;
   if FXMLList.Count = 0 then Exit;
   VideoStreamIndex:= -1;
   AudioStreamIndex:= -1;
   InStreamConfig := False;

   StreamIndex := 0;
   ASFXMLDataType := xdt_None;

   for i := 0 to FXMLList.Count - 1 do begin
      s := lowercase (FXMLList[i]);
      if Copy (s, 1, 13) = '<streamconfig' then begin
         InStreamConfig := True;
         inc (StreamIndex);
      end
      else if Copy (s, 1, 13) = '>streamconfig' then begin
         InStreamConfig := False;
         ASFXMLDataType := xdt_None;
      end
      else begin
         if InStreamConfig then begin
            if (pos ('audio', s) > 0) or (pos ('<waveformatex', s) > 0) then begin
               if AudioStreamIndex = -1 then begin
                  AudioStreamIndex := StreamIndex;
               end;
               ASFXMLDataType := xdt_AudioStream;
            end
            else if (pos ('video', s) > 0) or (pos ('<videoinfoheader', s) > 0) then begin
               if VideoStreamIndex = -1 then begin
                  VideoStreamIndex := StreamIndex;
               end;
               ASFXMLDataType := xdt_VideoStream;
            end
         end;
      end;
      if not assigned (FXMLList.Objects[i]) then begin
         FXMLList.Objects[i] := TASFXMLData.Create;
      end;
      TASFXMLData(FXMLList.Objects[i]).ASFXMLDataType := ASFXMLDataType;
   end;

   if (VideoStreamIndex = -1) and (AudioStreamIndex = -1) then Exit;

   MustDecreaseStreamNumber := False;
   if (AudioStreamIndex <> -1) and (VideoStreamIndex <> -1) then begin
      MustDecreaseStreamNumber := ((not UseAudio) and (AudioStreamIndex > VideoStreamIndex)) or ((not UseVideo) and (VideoStreamIndex > AudioStreamIndex));
   end;

   DeletingStream := False;
   StreamIndex := 0;
   for i := 0 to FXMLList.Count - 1 do begin
      s := FXMLList[i];
      if Copy (s, 1, 13) = '<streamconfig' then begin
         InStreamConfig := True;
         inc (StreamIndex);
         if ((not UseAudio) and (StreamIndex = AudioStreamIndex))
          or ((not UseVideo) and (StreamIndex = VideoStreamIndex)) then begin
            DeletingStream := True;
            FXMLList[i] := 'ZZZ';
         end;
      end
      else if Copy (s, 1, 13) = '>streamconfig' then begin
         InStreamConfig := False;
         if DeletingStream then begin
            FXMLList[i] := 'ZZZ';
            DeletingStream := False;
         end;
      end
      else begin
         if InStreamConfig then begin
            if MustDecreaseStreamNumber then begin
               if copy (s, 1, 12) = 'streamnumber' then begin
                  NewStreamNumber := StrToIntDef (copy (s, 14, MAXINT), -1);
                  if NewStreamNumber > 1 then begin
                     FXMLList[i] := 'streamnumber=' + IntToStr (NewStreamNumber - 1);
                  end;
               end;
            end;
         end;
         if DeletingStream then begin
            FXMLList[i] := 'ZZZ';
         end;
      end;
   end;

   Done := False;
   i := 0;
   while not Done do begin
      if i >= FXMLList.Count then begin
         Done := True;
      end
      else begin
         if FXMLList[i] = 'ZZZ' then begin
            if assigned (FXMLList.Objects[i]) then begin
               TASFXMLData (FXMLList.Objects[i]).Free;
            end;
            FXMLList.Delete (i);
         end
         else begin
            inc (i);
         end;
      end;
   end;
   Result := True;
end;


