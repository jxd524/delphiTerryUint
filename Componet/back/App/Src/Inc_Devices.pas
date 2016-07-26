const
   ExcludeVideoCompressorList = 'ENCODER DMO!XLDECOMPRESS';
   //ExcludeVideoCompressorList = 'ENCODER DMO!XLDECOMPRESS!MICROSOFT H.261!MICROSOFT H.263!PCLEPIM1';
   ExcludeAudioCompressorList = 'ENCODER DMO';
   //ExcludeAudioCompressorList = 'ENCODER DMO!IAC2!ACELP.NET!WINDOWS MEDIA AUDIO V1!MPEG LAYER-3!VOXWARE';

type

   TCapDeviceInfo = class (TObject)
      RegistryStringId: String;
      FriendlyName: String;
      Manufacturer: String;
      Model: String;
      WDM: Boolean;
      WasConnected: Boolean;
      IgnoreConnected: Boolean;
      IsDVDevice: Boolean;
      Connected: Boolean;
      VfwDriverNo: Integer;
      InUse: Boolean;
      ItemNumber: Integer;
      ClsidDeviceClass: TGUID;
      isDummy: Boolean;
      DummyGUID: TGUID;
   end;

   TDeviceClass = (dc_VideoDevices, dc_AudioDevices, dc_VideoCompressors, dc_AudioCompressors);

   TDevices = class (TObject)
   private
      FCanDisableFromIDE: Boolean;
      FDeviceClass: TDeviceClass;
      FDeviceClassName: string;
      FclsidDeviceClass: TGUID;
      FDevices: TStringList;
      FVfwClsid: string;
      FWDMClsid: string;
      FAddWDMInfo: Boolean;
      FEnumeratedCount: Integer;
      FPreviousEnumeratedCount: Integer;
      FExclusionList: string;
      FVfwDriversList: TStringList;
      FLockRebuild: TRTLCriticalSection;
      FEnumFlags: DWORD;
      FAlternateDevices: TDevices;

      function GetList: string;
      function Count: Integer;
      function Listed (RegistryStringId: string): Boolean;
      procedure MakeSorts (List: TStringList);
  public
      constructor Create (DeviceClass: TDeviceClass; DeviceClassName: string; const clsidDeviceClass: TGUID; Flags: Dword; ExclusionList: string; CanDisableFromIDE: Boolean; VfwVideoDrivers: string; AddWDMInfo: Boolean; AlternateDevices: TDevices);
      destructor Destroy; override;
      property List: string read GetList;
      function DeviceName (Index: integer): String;
      function FindCompressorByName (Value: String): Integer;
      function NextItem (Name: String): Integer;
      procedure RebuildDeviceList;
      procedure AddDummyDevice (Name: String; GUID: pGUID);
      function ProcessCustomEncoder (RegId: String; GUID: pGUID): Boolean;
   end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
{                                TDEVICES class
{ stores VideoDevices, AudioDevices, VideoCompressors, AudioCompressors
{………………………………………………………………………………………………………………………………………………………………………………………………………………}

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function FormattedKeystring (DisplayName, FriendlyName: string): string;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   c: Char;
   i: Integer;
   Temp: string;
   Value: string;
begin
   Value := DisplayName + FriendlyName;
   Temp := '';
   for i := 1 to Length (Value) do begin
      c:= Value[i];
      (*if c in ['\'] then begin
         c := ' ';
      end;*)
      if (c in [' ','-','{','}'])
       or (c in ['a'..'z'])
       or (c in ['A'..'Z'])
       or (c in ['0'..'9']) then begin
         Temp := Temp + c;
      end;
   end;
   Result := Temp;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function ExtractItem (var i: Integer; ExclusionList: string): string;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
{ extract an item from a string (items are separated by "!")
{ e.g. "ONE!TWO!THREE" returns "ONE", then "TWO", then "THREE"...
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   Done: Boolean;
begin
   Result := '';
   Done := false;
   while not Done do begin
      if i > length (ExclusionList) then begin
         Done := true;
      end
      else begin
         if ExclusionList[i] <> '!' then begin
            Result := Result + ExclusionList[i];
         end
         else begin
            Done := true;
         end;
         inc (i);
      end;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function CheckExclusionList (Name: string; ExclusionList: string): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
{ checks if one of the items of ExclusionList is not contained in Name
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   Item: string;
   i: Integer;
   Done: Boolean;
begin
   Result := true;
   if ExclusionList <> '' then begin
      Done := false;
      i:= 1;
      Repeat
         Item := ExtractItem (i, ExclusionList);
         if Item <> '' then begin
            if pos (Item, Uppercase (Name)) > 0 then begin
               Result := false;
               Done := true;
            end;
         end;
      until (Item = '') or Done;
   end;
end;

type

TMonikerNameId = (mk_FullString, mk_SubString, mk_AllWords, mk_UseFormat);

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function RetrieveMoniker (pClsidDeviceClass: pGUID; Name: string; MonikerNameId: TMonikerNameId; var Moniker: IMoniker; var FriendlyName: String): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
  SysDevEnum: ICreateDevEnum;
  EnumCat: IEnumMoniker;
  cFetched: Integer;
  Done: Boolean;
  DoneWords: Boolean;
  iWords: Integer;
  Words: string;
  AllWordsFound: Boolean;
  sName: POleStr;
  PropBag: IPropertyBag;
  DisplayName: string;
  varName: OleVariant;
begin
   try
      Result := False;
      if not CreateInstance (@CLSID_SystemDeviceEnum, ICreateDevEnum, SysDevEnum, 'Sys dev enum') then Exit;
      if SysDevEnum.CreateClassEnumerator (pClsidDeviceClass^, EnumCat, 0) <> S_OK then Exit;
      EnumCat.Reset;
      Done := False;
      while not Done do begin
         if EnumCat.Next(1, Moniker, @cFetched) = S_OK then begin
            if assigned (Moniker) then begin
                  if Moniker.GetDisplayName (nil, nil, sName) = S_OK then begin
                     DisplayName := sName;
                     CoTaskMemFree (sName);
                     if Moniker.BindToStorage(nil, nil, IPropertyBag, PropBag) = S_OK then begin
                        VariantInit (varName);
                        if PropBag.Read('FriendlyName', varName, nil) = S_OK then begin
                           FriendlyName := varName;
                           VarClear (varName);
                           case MonikerNameId of
                              mk_UseFormat: begin
                                 if FormattedKeystring (DisplayName, FriendlyName) = Name then begin
                                    Result := True;
                                    Done := True;
                                 end;
                              end;
                              mk_SubString: begin
                                 if pos (Name, FriendlyName) > 0 then begin
                                    Result := True;
                                    Done := True;
                                 end;
                              end;
                              mk_FullString: begin
                                 if Name = FriendlyName then begin
                                    Result := True;
                                    Done := True;
                                 end;
                              end;
                              mk_AllWords: begin
                                 DoneWords := False;
                                 Words := '';
                                 iWords := 1;
                                 AllWordsFound := True;
                                 while not DoneWords do begin
                                    if iWords <= Length(Name) then begin
                                       if Name[iWords] <> '!' then begin
                                          Words := Words + Name[iWords];
                                       end
                                       else begin
                                          if Words <> '' then begin
                                             if pos (Words, FriendlyName) < 1 then begin
                                                AllWordsFound := False;
                                                DoneWords := True;
                                             end;
                                             Words := '';
                                          end;
                                       end;
                                    end
                                    else begin
                                       if Words <> '' then begin
                                          if pos (Words, FriendlyName) < 1 then begin
                                             AllWordsFound := False;
                                          end;
                                       end;
                                       DoneWords := True;
                                    end;
                                    inc (iWords);
                                 end;
                                 if AllWordsFound then begin
                                    Result := True;
                                    Done := True;
                                 end;
                              end;
                           end;
                        end;
                        PropBag := nil;
                     end;
                  end;
                  if not Result then begin
                     Moniker := nil;
                  end;
            end;
         end
         else begin
            Done := True;
         end;
      end;
   finally
      if assigned (EnumCat) then EnumCat := nil;
      if assigned (SysDevEnum) then SysDevEnum := nil;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
constructor TDevices.Create (DeviceClass: TDeviceClass; DeviceClassName: string; const clsidDeviceClass: TGUID; Flags: DWord; ExclusionList: string; CanDisableFromIDE: Boolean; VfwVideoDrivers: string; AddWDMInfo: Boolean; AlternateDevices: TDevices);
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   Temp: POleStr;
begin
   InitializeCriticalSection (FLockRebuild);
   FDevices := TStringList.Create;
   FCanDisableFromIDE := CanDisableFromIDE;
   FDeviceClass := DeviceClass;
   FDeviceClassName := DeviceClassName;
   FExclusionList := ExclusionList;
   stringFromCLSID (CLSID_VfwCapture, Temp);
   FVfwClsid := Temp;
   CoTaskMemFree (Temp);
   stringFromCLSID (CLSID_Proxy, Temp);
   FWDMClsid := Temp;
   CoTaskMemFree (Temp);
   FclsidDeviceClass := clsidDeviceClass;
   FVfwDriversList := TStringList.Create;
   FVfwDriversList.Text := VfwVideoDrivers;
   FAddWDMInfo := AddWDMInfo;
   FEnumFlags := Flags;
   FAlternateDevices := AlternateDevices;

   FEnumeratedCount:= 0;
   FPreviousEnumeratedCount:= -1;

   RebuildDeviceList;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
destructor TDevices.Destroy;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
{ the FDevices dynamic array must be freed by assigning nil
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   DeleteCriticalSection (FLockRebuild);
   FVfwDriversList.Free;
   while FDevices.Count > 0 do begin
      FDevices.Objects[0].Free;
      FDevices.Delete (0);
   end;
   FDevices.Free;
   inherited Destroy;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TDevices.GetList: string;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
{ returns the devices (or compressors) list.
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   Result := FDevices.Text;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TDevices.Listed (RegistryStringId: string): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
{ returns true if the MkName device is already listed
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   Done, Found: Boolean;
   i: Integer;
begin
   Done := false;
   Found := false;
   i := 0;
   while not Done do begin
      if i = FDevices.Count then begin
         Done := true;
      end
      else begin
         if RegistryStringId = TCapDeviceInfo (FDevices.Objects[i]).RegistryStringId then begin
            TCapDeviceInfo (FDevices.Objects[i]).Connected := True;
            Found := true;
            Done := true;
         end;
         inc (i);
      end;
   end;
   Result := Found;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TDevices.DeviceName (Index: integer): String;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   Result := '';
   if Index < 0 then Exit;
   if Index >= FDevices.Count then Exit;
   Result := FDevices[Index];
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TDevices.FindCompressorByName (Value: String): Integer;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   i: integer;
   Done: Boolean;
begin
   Result := -1;
   i := 0;
   Done := False;
   while not Done do  begin
      if i >= FDevices.Count then begin
         Done := True;
      end
      else begin
         if FDevices[i] = Value then begin
            Result := i;
            Done := True;
         end
         else begin
            inc (i);
         end;
      end;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function ExtractIdString (id: String; var Manufacturer: String; var Model: String): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   Temp, Temp2: String;
   p: Integer;
   p2: Integer;
begin
   Result := False;
   Manufacturer := '';
   Model := '';
   p := pos ('\\?\avc#', id);
   if p > 0 then begin
      Temp := Copy (id, p + 8, MAXINT);
      p2 := pos ('#', Temp);
      if p2 > 1 then begin
         Temp2 := copy (Temp, 1, p2 - 1);
         if length(Temp2) > 0 then begin
            p := pos ('&', Temp2);
            while p > 0 do begin
               Temp2[p] := ' ';
               p := pos ('&', Temp2);
            end;
            Result := True;
            p := pos ('-', Temp2);
            if p > 1 then begin
               Manufacturer := Copy (Temp2, 1, p - 1);
               Model := Copy (Temp2, p + 1, MAXINT);
            end
            else begin
               Manufacturer := Temp2;
               Model := Temp2;
            end;
         end;
      end;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TDevices.NextItem (Name: String): Integer;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   i: integer;
begin
   Result := 1;
   for i := 0 to FDevices.Count - 1 do begin
      if TCapDeviceInfo (FDevices.Objects[i]).FriendlyName = Name then begin
         if Result <= TCapDeviceInfo (FDevices.Objects[i]).ItemNumber then begin
            Result := TCapDeviceInfo (FDevices.Objects[i]).ItemNumber + 1;
         end;
      end;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
procedure SwapToTopIfSubString (l: TStringList; var MinTop: Integer; SubString: String; ExcludedSubString: String);
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   HSubString: String;
   HExcludedSubString: String;
   i: Integer;
   HList: String;
begin
   HSubString := UpperCase (SubString);
   HExcludedSubString := UpperCase (ExcludedSubString);
   for i := MinTop to l.Count - 1 do begin
      HList := UpperCase (l[i]);
      if pos (HSubString, HList) > 0 then begin
         if (ExcludedSubString = '') or ((ExcludedSubString <> '') and (pos (ExcludedSubString, HList) = 0)) then begin
            l.Exchange (MinTop, i);
            inc (MinTop);
         end;
      end;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
procedure TDevices.MakeSorts (List: TStringList);
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   MinTop: Integer;
begin
   MinTop := 0;
   case FDeviceClass of
      dc_VideoCompressors: begin
         SwapToTopIfSubString (List, MinTop, 'MPEG-4', '');
         SwapToTopIfSubString (List, MinTop, 'MPEG', '');
         SwapToTopIfSubString (List, MinTop, 'DIVX', '');
         SwapToTopIfSubString (List, MinTop, 'XVID', '');
         SwapToTopIfSubString (List, MinTop, 'MJPEG', '');
      end;
      dc_AudioCompressors: begin
         SwapToTopIfSubString (List, MinTop, 'Windows Media Audio V2', '');
         SwapToTopIfSubString (List, MinTop, 'PCM', 'ADPCM');
         SwapToTopIfSubString (List, MinTop, 'MPEG', '');
      end;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
procedure TDevices.AddDummyDevice (Name: String; GUID: pGUID);
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   NewDevice: TCapDeviceInfo;
   FinalDeviceName: String;
   i: Integer;
   RegistryStringId: String;
begin
   RegistryStringId := FormattedKeystring (Name + '°', '');
   if not Listed (RegistryStringId) then begin

      NewDevice := TCapDeviceInfo.Create;
      NewDevice.IsDummy := True;
      NewDevice.RegistryStringId := RegistryStringId;
      NewDevice.FriendlyName := Name;
      NewDevice.WDM := True;
      NewDevice.Connected := True;
      NewDevice.WasConnected := False;
      NewDevice.VfwDriverNo := -1;
      NewDevice.InUse := False;
      NewDevice.ItemNumber := NextItem (Name);
      NewDevice.IsDVDevice := False;
      NewDevice.Manufacturer := '';
      NewDevice.Model := '';
      CopyGUID (@NewDevice.DummyGUID, GUID);

      FinalDeviceName := Name;

      if NewDevice.ItemNumber > 1 then begin
         FinalDeviceName := FinalDeviceName + ' #' + IntToStr (NewDevice.ItemNumber);
      end;

      NewDevice.IgnoreConnected := False;
      CopyGUID (@NewDevice.ClsidDeviceClass, @FClsidDeviceClass);
      i := FDevices.Add (FinalDeviceName);
      FDevices.Objects[i] := NewDevice;
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
procedure TDevices.ReBuildDeviceList;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
{ rescans all the devices available, builds the list of names, detects if
{ the device is WDM compliant. Adjusts the FDevices array size.
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
type
  TDriver = (dv_None, dv_WDM, dv_vfw);
var
  Driver: TDriver;
  SysDevEnum: ICreateDevEnum;
  EnumCat: IEnumMoniker;
  Moniker: IMoniker;
  cFetched: Integer;
  PropBag: IPropertyBag;
  varName: OleVariant;
  varCLSID: OleVariant;
  FriendlyName: string;
  OkToAdd: Boolean;
  i: Integer;
  sName: POleStr;
  VfwDriverNo: Integer;
  RegistryStringId: string;
  DisplayName: string;
  NewDevice: TCapDeviceInfo;
  MustRebuildAlternateDevices: Boolean;
  FinalDeviceName: String;
begin
   MustRebuildAlternateDevices := False;

   try

      WriteLog ('v', sBlue, 'rebuilding ' + FDeviceClassName);

      for i := 0 to FDevices.Count - 1 do begin
         if not TCapDeviceInfo (FDevices.Objects[i]).IgnoreConnected then begin
            TCapDeviceInfo (FDevices.Objects[i]).WasConnected := TCapDeviceInfo (FDevices.Objects[i]).Connected;
            TCapDeviceInfo (FDevices.Objects[i]).Connected := False;
         end;
      end;

      if (FCanDisableFromIDE and CodecsDisabledFromIDE) then begin
         if FindWindow('TAppBuilder', nil) <> 0 then begin
            WriteLog ('v', sPurple, FDeviceClassName + 'rebuild disabled from IDE');
            Exit;
         end;
      end;

      EnterCriticalSection (FLockRebuild);

      if FPreviousEnumeratedCount <> -1 then begin
         FPreviousEnumeratedCount := FEnumeratedCount;
      end;
      FEnumeratedCount := 0;

      if not CreateInstance(@CLSID_SystemDeviceEnum, ICreateDevEnum, SysDevEnum, 'create sys dev enum') then Exit;
      if SysDevEnum.CreateClassEnumerator(FclsidDeviceClass, EnumCat, FEnumFlags) <> S_OK then Exit;

      EnumCat.Reset;
      while EnumCat.Next(1, Moniker, @cFetched) = S_OK do begin
         if assigned (Moniker) then begin
               VfwDriverNo := -1;
               Driver := dv_None;
               inc (FEnumeratedCount);
               if Moniker.GetDisplayName (nil, nil, sName) = S_OK then begin
                  DisplayName := sName;
                  CoTaskMemFree (sName);
               end
               else begin
                  DisplayName := '';
               end;

               if Moniker.BindToStorage(nil, nil, IPropertyBag, PropBag) = S_OK then begin

                  if PropBag.Read('FriendlyName', varName, nil) = S_OK then begin
                     FriendlyName := VarName;
                     VarClear (varName);

                     RegistryStringId := FormattedKeystring (DisplayName, FriendlyName);

                     OkToAdd := CheckExclusionList (FriendlyName, FExclusionList);

                     if OkToAdd then begin
                        OkToAdd := not Listed (RegistryStringId);
                     end;

                     if OkToAdd then begin
                        if PropBag.Read('CLSID', varCLSID, nil) = S_OK then begin
                           if varCLSID = FVfwClsid then begin
                              VfwDriverNo := FVfwDriversList.IndexOf (FriendlyName);
                              Driver := dv_vfw;
                           end
                           else begin
                              Driver := dv_WDM;
                           end;
                           VarClear (varCLSID);
                        end;
                        if driver = dv_vfw then begin
                           OkToAdd := VfwDriversEnabled;
                        end;
                     end;

                     if OkToAdd then begin

                        i := -1;

                        NewDevice := TCapDeviceInfo.Create;
                        NewDevice.IsDummy := False;
                        NewDevice.RegistryStringId:= RegistryStringId;
                        NewDevice.FriendlyName := FriendlyName;
                        NewDevice.WDM := Driver = dv_WDM;
                        NewDevice.Connected := True;
                        NewDevice.WasConnected := False;
                        NewDevice.VfwDriverNo := VfwDriverNo;
                        NewDevice.InUse := False;
                        NewDevice.ItemNumber := NextItem (FriendlyName);
                        NewDevice.IsDVDevice := pos ('Microsoft DV', FriendlyName) > 0;
                        ExtractIdString (DisplayName, NewDevice.Manufacturer, NewDevice.Model);

                        FinalDeviceName := FriendlyName;

                        if NewDevice.ItemNumber > 1 then begin
                           FinalDeviceName := FinalDeviceName + ' #' + IntToStr (NewDevice.ItemNumber);
                        end;

                        if FAddWDMInfo then begin
                           if driver = dv_WDM then begin
                              FinalDeviceName := FinalDeviceName + ' (WDM)';
                           end
                           else begin
                              FinalDeviceName := FinalDeviceName + ' (vfw)';
                           end;
                        end;

                        if FDeviceClass = dc_VideoDevices then begin
                           if assigned (FAlternateDevices) then begin
                              if pos ('AUDIO', uppercase (FinalDeviceName)) > 0 then begin
                                 NewDevice.IgnoreConnected := True;
                                 CopyGUID (@NewDevice.ClsidDeviceClass, @CLSID_VideoInputDeviceCategory);
                                 i := FAlternateDevices.FDevices.Add (FinalDeviceName);
                                 FAlternateDevices.FDevices.Objects[i] := NewDevice;
                                 MustRebuildAlternateDevices := True;
                              end;
                           end;
                        end;

                        if i = -1 then begin
                           NewDevice.IgnoreConnected := False;
                           CopyGUID (@NewDevice.ClsidDeviceClass, @FClsidDeviceClass);
                           i := FDevices.Add (FinalDeviceName);
                           FDevices.Objects[i] := NewDevice;
                        end;

                        WriteLog (nil, sBlue, 'l= ' + inttostr (FDevices.Count) + ',' + ' adding ' + FinalDeviceName);
                     end;
                  end;
               end;
               PropBag:= nil;
               Moniker:= nil;
         end;
      end;
   finally
      MakeSorts (FDevices);

      if assigned (SysDevEnum) then SysDevEnum := nil;
      if assigned (EnumCat) then EnumCat := nil;

      if FPreviousEnumeratedCount = -1 then begin
         FPreviousEnumeratedCount := FEnumeratedCount;
      end;
      LeaveCriticalSection (FLockRebuild);
      if MustRebuildAlternateDevices then begin
         FAlternateDevices.RebuildDeviceList;
      end;

      if assigned (CustomEnClsid) then begin
         if FDeviceClass = dc_VideoCompressors then begin
           if not ProcessCustomEncoder ('\CLSID\' + GuidToString (CustomEnClsid^), CustomEnClsid)  then begin
           end;
         end;
      end;

      WriteLog ('v', sBlue, 'rebuilded ' + FDeviceClassName);
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TDevices.ProcessCustomEncoder (RegId: String; GUID: pGUID): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   RegKey: TRegistry;
   EncoderId: String;
begin
   RegKey := nil;
   EncoderId := '';
   try
      Result := False;
      RegKey := TRegistry.Create;
      RegKey.RootKey := HKEY_CLASSES_ROOT;
      if RegKey.KeyExists (RegId) then begin
         if RegKey.OpenKeyReadOnly (RegId) then begin
            EncoderId := Regkey.Readstring ('');
         end;
      end;
    finally
      if assigned (RegKey) then RegKey.Free;
      if EncoderId <> '' then begin
         AddDummyDevice (EncoderId, GUID);
         Result := True;
      end;
    end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function TDevices.Count: Integer;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   Result:= FDevices.Count;
end;

