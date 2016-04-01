unit uVCLZipFunc;

interface
uses
  Classes, Windows, SysUtils, Forms, VCLUnZip, VCLZip, kpZipObj;

function ZipFileFolder(const AZipPath: string; const AZipFileName: string): Boolean;
function UnZipFileFolder(const AUnZipPath: string; const AOriginFile: string): Boolean;

implementation

function FileNameComparison(const Item1, Item2: string): Integer;
begin
  Result := CompareText(Item1, Item2);
end;

procedure FindAllFile(Path: string; var FileResult: Tstrings);
var
  fpath: string;
  fs: TsearchRec;
begin
  fpath := path + '\*.*';
  if findfirst(fpath, faAnyFile, fs) = 0 then
  begin
    if (fs.Name <> '.') and (fs.Name <> '..') then
      if (fs.Attr and faDirectory) = faDirectory then
        FindAllFile(path + '\' + fs.Name, fileresult)
      else
      begin
        FileResult.Add(strpas(pchar(path)) + '\' + strpas(
          pchar(fs.Name)));
      end;
    while findnext(fs) = 0 do
    begin
      if (fs.Name <> '.') and (fs.Name <> '..') then
        if (fs.Attr and faDirectory) = faDirectory then
          FindAllFile(path + '\' + fs.Name, fileresult)
        else
          FileResult.Add(strpas(pchar(path)) + '\' +
            strpas(pchar(fs.Name)));
    end;
  end;
  findclose(fs);
end;


function ZipFileFolder(const AZipPath: string; const AZipFileName: string): Boolean;
var
  AVCLZip: TVCLZip;
  aFileList: TStrings;
  SavePath: string;
begin
  Result := False;
  try
    SavePath := ExtractFilePath(AZipFileName);
    if not DirectoryExists(SavePath) then
      ForceDirectories(SavePath);
    AVCLZip := TVCLZip.Create(nil);
    aFileList := TStringList.Create;
    try
      FindAllFile(AZipPath, aFileList);
      if aFileList.Count =0 then
        Exit;

      AVCLZip.FilesList.Assign(aFileList);
      AVCLZip.ZipName := AZipFileName;
      AVCLZip.RootDir := AZipPath;
      AVCLZip.SortMode := ByFileName;
      AVCLZip.IncludeSysFiles := True;
      AVCLZip.IncludeArchiveFiles := True;
      AVCLZip.IncludeReadOnlyFiles := True;
      AVCLZip.Recurse := True;
      AVCLZip.RelativePaths := True;
      AVCLZip.StorePaths := True;
      Application.ProcessMessages;
      Result := AVCLZip.zip > 0;
    finally
      aFileList.Free;
      AVCLZip.Free;
    end;
  except
    Result := False;
  end;
end;

function UnZipFileFolder(const AUnZipPath: string; const AOriginFile: string): Boolean;
var
  AVCLZip: TVCLZip;
begin
  try
    if not DirectoryExists(AUnZipPath) then
      ForceDirectories(AUnZipPath);
    AVCLZip := TVCLZip.Create(nil);
    try
      AVCLZip.ZipName := AOriginFile;
      AVCLZip.DoAll := True;
      AVCLZip.DestDir := AUnZipPath;
      AVCLZip.RootDir := AUnZipPath;
      AVCLZip.OverwriteMode := Always;
      AVCLZip.RecreateDirs := True;
      AVCLZip.RetainAttributes := True;
      Result := AVCLZip.UnZip > 0;
    finally
      AVCLZip.Free;
    end;
  except
    Result := False;
  end;
end;

end.

