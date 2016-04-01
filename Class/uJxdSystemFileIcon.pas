unit uJxdSystemFileIcon;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, ShellAPI;

type
  TxdSystemFileIcon = class
  public
    constructor Create(const ASmallIcon: Boolean);
    destructor Destroy; override;
    procedure GetSysFileIcon(const AFileName: string; var AIcon: TIcon);
  private
    FIconImage: TImageList;
    FIndexList: TList;
    FFlags: Cardinal;
  end;

implementation

type
  PIconIndexInfo = ^TIconIndexInfo;
  TIconIndexInfo = record
    FExt: string;
    FIndex: Integer;
  end;

{ TxdSystemFileIcon }

constructor TxdSystemFileIcon.Create(const ASmallIcon: Boolean);
var
  nSize: Integer;
  SHFileInfo: TSHFileInfo;
begin
  if not ASmallIcon then
  begin
    nSize := 32;
    FFlags := SHGFI_SYSICONINDEX or SHGFI_LARGEICON;
  end
  else
  begin
    nSize := 16;
    FFlags := SHGFI_SYSICONINDEX or SHGFI_SMALLICON;
  end;
  FIconImage := TImageList.CreateSize(nSize, nSize);
  FIconImage.ShareImages := True;
  FIconImage.Handle := ShGetFileInfo('', 0, SHFileInfo, SizeOf(TSHFileInfo), FFlags);
  FIndexList := TList.Create;
end;

destructor TxdSystemFileIcon.Destroy;
var
  i: Integer;
begin
  for i := 0 to FIndexList.Count - 1 do
    Dispose(FIndexList[i]);
  FIndexList.Free;
  FIconImage.Free;
  inherited;
end;

procedure TxdSystemFileIcon.GetSysFileIcon(const AFileName: string; var AIcon: TIcon);
var
  i, nIndex: Integer;
  strExt, strTempFile: string;
  p: PIconIndexInfo;
  f: Integer;
  bCreate: Boolean;
  SHFileInfo: TSHFileInfo;
begin
  strExt := ExtractFileExt(AFileName);
  nIndex := -1;
  for i := 0 to FIndexList.Count - 1 do
  begin
    p := FIndexList[i];
    if strExt = p^.FExt then
    begin
      nIndex := p^.FIndex;
      Break;
    end;
  end;
  if nIndex = -1 then
  begin
    bCreate := not FileExists(AFileName);
    if bCreate then
    begin
      strTempFile := ExtractFilePath(ParamStr(0)) + '_temp_icon_file_del' + strExt;
      f := FileCreate(strTempFile);
      FileClose(f);
    end
    else
      strTempFile := AFileName;

    ShGetFileInfo(PChar(strTempFile), 0, SHFileInfo, SizeOf(SHFileInfo), FFlags);
    if bCreate then
      DeleteFile(strTempFile);

    New(P);
    P^.FExt := strExt;
    P^.FIndex := SHFileInfo.iIcon;
    FIndexList.Add(p);
    nIndex := p^.FIndex;
  end;
  if not Assigned(AIcon) then
    AIcon := TIcon.Create;

//  for i := 0 to FIconImage.Count - 1 do
//  begin
//    FIconImage.GetIcon( i, AIcon );
//    AIcon.SaveToFile( 'E:\Delphi\MyProject\MySoftTool\XdPlayer\bin\icon\' + IntToStr(i) + '.ico' );
//  end;
  FIconImage.GetIcon(nIndex, AIcon);
end;

end.
