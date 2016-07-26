unit uJxdAsfXml;

interface
uses
  Classes, SysUtils;

type
  TXMLState = (csNone, csWaitIdentifier, csIdentifier, csWaitProperty, csProperty, csWaitValue, csValue, csCloseIdentifier);

  TXML = class(TObject)
  private
    FLevel: Integer;
    FLevelList: TStringList;
    FXMLList: TStringList;
    FCurrentXMLIndex: Integer;
    FXMLState: TXMLState;
    FGeneratedXML: TStringList;
  public
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

  TASFXMLData = class(TObject)
    ASFXMLDataType: TASFXMLDataType;
  end;

  TASFXML = class(TXML)
  public
    procedure ReInitXML; override;
    function FilterStreams (UseVideo: Boolean; UseAudio: Boolean): Boolean;
    procedure UpdateASFValue (Id: String; Value: String; ASFXMLDataType: TASFXMLDataType);
  end;

implementation

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

procedure TXML.ProcessCurWord(var CodeList: TStringList; var CurWord: String; XMLState: TXMLState);
const
  STATEMENT = '   XML.DoXML (''';
var
  FinalWord: String;
begin
  FinalWord := Trim( CurWord );

  case XMLState of
  csIDentifier:
  begin
    inc( FLevel );
    FLevelList.Add( FinalWord );
    FCurrentXMLIndex := FXMLList.Add( '<' + FLevelList[FLevel - 1] );
    CodeList.Add( STATEMENT + FXMLList[FCurrentXMLIndex] + ''', '''');' );
  end;
  csCloseIDentifier:
  begin
    FCurrentXMLIndex := FXMLList.Add( '>' + FLevelList[FLevel - 1] );
    CodeList.Add( STATEMENT + FXMLList[FCurrentXMLIndex] + ''', '''');' );
    if FLevel > 0 then
    begin
      dec( FLevel );
      if FLevelList.Count > FLevel then
      begin
        FLevelList.Delete( FLevel );
      end;
    end;
  end;
  csProperty:
  begin
    FCurrentXMLIndex := FXMLList.Add( FinalWord );
  end;
  csValue:
  begin
    CodeList.Add( STATEMENT + FXMLList[FCurrentXMLIndex] + ''', ''' + FinalWord + ''');' );
    FXMLList[FCurrentXMLIndex] := FXMLList[FCurrentXMLIndex] + '=' + FinalWord;
    //CodeList.Add (FXMLList[FCurrentXMLIndex]);
  end;
  end;
  CurWord := '';
end;

function TXML.ParseXML(XMLString: String): String;
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


end.
