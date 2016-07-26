{Copyright:      Hagen Reddmann  mailto:HaReddmann@AOL.COM
 Author:         Hagen Reddmann
 Remarks:        freeware, but this Copyright must be included
 known Problems: none
 Version:        3.0,  Delphi Encryption Compendium
                 Delphi 2-4, designed and testet under D3 & D4
 Remarks:        remove Cipher1 from the Uses Clausel to
                 use only a Selection from Ciphers (Cipher.pas)
}
unit DECReg;

{$DEFINE Part_II}
{$DEFINE Part_III}

interface

uses
  DECConst, DECUtil, Hash, Cipher, HCMngr, SysUtils, Classes, DsgnIntf, Cipher1, RFC2289
{$IFDEF Part_II}
{$ENDIF}
{$IFDEF Part_III}
{$ENDIF};


type
// List all registered Hash Names
  TAlgorithmHashProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
// List only Checksums
  TAlgorithmChecksumProperty = class(TAlgorithmHashProperty)
    procedure GetValues(Proc: TGetStrProc); override;
  end;
// List only Secure Hashs (excludes Checksum)
  TAlgorithmSecureHashProperty = class(TAlgorithmHashProperty)
    procedure GetValues(Proc: TGetStrProc); override;
  end;
// List all registered Ciphers
  TAlgorithmCipherProperty = class(TAlgorithmHashProperty)
    procedure GetValues(Proc: TGetStrProc); override;
  end;
// List all valid Idents ("otp-", "s/key")
  TOTPIdentProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
// List all registered Stringformats
  TStringFormatProperty = class(TIntegerProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

procedure Register;

implementation

{$R *.RES}

function TAlgorithmHashProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly, paValueList, paMultiSelect];
end;

procedure TAlgorithmHashProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  with HashList do
    for I := 0 to Count-1 do Proc(Names[I]);
end;

procedure TAlgorithmChecksumProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  with HashList do
    for I := 0 to Count-1 do
      if THashClass(Objects[I]).InheritsFrom(TChecksum) then Proc(Names[I]);
end;

procedure TAlgorithmSecureHashProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  with HashList do
    for I := 0 to Count-1 do
      if not THashClass(Objects[I]).InheritsFrom(TChecksum) then Proc(Names[I]);
end;

procedure TAlgorithmCipherProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  with CipherList do
    for I := 0 to Count-1 do Proc(Names[I]);
end;

function TOTPIdentProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TOTPIdentProperty.GetValues(Proc: TGetStrProc);
begin
  Proc(sOTPIdent);
  Proc(sSKeyIdent);
end;

function TStringFormatProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TStringFormatProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  S: TStringList;
begin
  S := TStringList.Create;
  try
    GetStringFormats(S);
    for I := 0 to S.Count-1 do Proc(S[I]);
  finally
    S.Free;
  end;
end;

function TStringFormatProperty.GetValue: String;
var
  Fmt: TStringFormatClass;
begin
  Fmt := StringFormat(GetOrdValue);   
  if Fmt <> nil then Result := Fmt.Name else Result := '(unknown)';
end;

procedure TStringFormatProperty.SetValue(const Value: String);
var
  I: Integer;
  S: TStringList;
begin
  S := TStringList.Create;
  try
    GetStringFormats(S);
    I := S.IndexOf(Value);
    if I < 0 then SetOrdValue(StrToIntDef(Value, DefaultStringFormat))
      else SetOrdValue(TStringFormatClass(S.Objects[I]).Format);
  finally
    S.Free;
  end;
end;

procedure Register;
begin
  RegisterComponents('DEC', [THashManager, TCipherManager, TOneTimePassword]);
  RegisterPropertyEditor(TypeInfo(String), TOneTimePassword, 'Algorithm', TAlgorithmSecureHashProperty);
  RegisterPropertyEditor(TypeInfo(String), TOneTimePassword, 'Ident', TOTPIdentProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TOneTimePassword, 'Format', TStringFormatProperty);
  RegisterPropertyEditor(TypeInfo(String), THashManager, 'Algorithm', TAlgorithmHashProperty);
  RegisterPropertyEditor(TypeInfo(String), TCipherManager, 'Algorithm', TAlgorithmCipherProperty);
{$IFDEF Part_II}
{$ENDIF}
{$IFDEF Part_III}
{$ENDIF}
end;

end.
