unit UParserMediaInfo;

interface

uses
  Windows, Classes, SysUtils, Graphics, DirectDraw, uMediaReader;


type
  //library local/remote(browse)
  PRecord_MediaFile_Info = ^Record_MediaFile_Info;
  Record_MediaFile_Info = record
    Ext: string; //扩展名
    Title: string; //标题
    Album: string; //专辑
    Artist: string; //艺人,作者
    Category: string; //类别 /种类
    Format: string; //格式
    Vidinfo: string; //
    Year: string; //年份
    FileSize: int64; //文件大小
    DurationString: string; //媒体长度，字符串表示
    Quanlity: Integer; //品质
    Comment: string; //备注
    Language: string; //语言
    FileDate: TDatetime; //文件时间
    FilePath: string; //文件位置
    Url: string;

    BitRate: Integer; //bit rate
    SampleRate: Integer; //sample rate
    Duration: Integer; //duration
    DetailInfo: Boolean;
  end;
  
  TMediaInfoReader = class
  private
    Mp3: TMPEGaudio;
    Ogg: TOggVorbis;
    Wma: TWMAfile;
    Wav: Twavfile;
    Flac: TFLACfile;
    Ape: Tmonkey;
    Aac: Taacfile;
//    Immagine: Tdcimageinfo;
    Vqf: TTwinVQ;
    Mpc: TMPCFile;
    raudio: precord_audioinfo;
    info_video: TDSMediaInfo;
  protected
    function FileCreateTime(AFileName: string): TDateTime;
    function IsSkipType(AExt: string): Boolean;
    function FormatTimeToStr(MiliSec: Cardinal): string;
    function JAPEncode(const AContent: string): string;
    function JudgeString(AStr: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    //
    function ReadMediaInfo(AFileName: string; PMediaInfo: PRecord_MediaFile_Info; bReadDetail: Boolean): Boolean;
  end;

function ricava_dati_mov(nomefile: widestring): record_audioinfo;
function ricava_dati_avi(nomefile: string): record_audioinfo;




implementation

uses
  uSysSub;

const
  FilterJapWord:  Array[0..55] of Word=(92, 304, 305, 430, 431, 437, 438, 12460, 12461, 12462, 12463, 12464, 12465, 12466, 12467, 12468, 12469, 12470, 12471, 12472, 12473, 12474, 12475, 12476, 12477, 12478, 12479, 12480, 12481, 12482, 12483, 12485, 12486, 12487, 12488, 12489, 12490, 12496, 12497, 12498, 12499, 12500, 12501, 12502, 12503, 12504, 12505, 12506, 12507, 12508, 12509, 12510, 12521, 12532, 12533, 65340);

function ricava_dati_mov(nomefile: widestring): record_audioinfo;
var
  stream: thandlestream;
  buffer: array[0..88] of byte;
  wres, hres, posizione: integer;
  count: longint;
  timescale: longint;
begin
  result.duration := 0;
  result.bitrate := 0;
  result.frequency := 0;
  result.codec := '';

  stream := MyFileOpen(nomefile);
  if stream = nil then exit;

  try
        // cerchiamo tkhd trovato il punto che equivale alla pos 1 proseguiamo con l'header

 // cerchiamo timescale
    count := 0;
    posizione := 0;
    while count <> -1 do begin
      if posizione >= 1024 then begin

        FreeHandleStream(Stream);
        exit; // errore :)
      end;
      stream.seek(posizione, sofrombeginning);
      count := stream.Read(buffer, 4); //mvhd
      if ((buffer[0] = 109) and (buffer[1] = 118) and (buffer[2] = 104) and (buffer[3] = 100)) then break else inc(posizione);
    end; // fine while

    if posizione = -1 then begin

      FreeHandleStream(Stream);
      exit;
    end;

    stream.seek(posizione + 16, sofrombeginning);
    stream.Read(buffer, 8);
    timescale := buffer[0];
    timescale := timescale shl 8;
    timescale := timescale + buffer[1];
    timescale := timescale shl 8;
    timescale := timescale + buffer[2];
    timescale := timescale shl 8;
    timescale := timescale + buffer[3];
    count := buffer[4];
    count := count shl 8;
    count := count + buffer[5];
    count := count shl 8;
    count := count + buffer[6];
    count := count shl 8;
    count := count + buffer[7];
    result.duration := count div timescale;
    // ora cerchiamo trkheader
    count := 0;
    posizione := 88; // saltiamo in toto il movie header
    while count <> -1 do begin
      if posizione >= 1024 then begin
        FreeHandleStream(Stream);
        exit; // errore :)
      end;

      stream.seek(posizione, sofrombeginning);
      count := stream.Read(buffer, 4); //tkhd
      if ((buffer[0] = 116) and (buffer[1] = 107) and (buffer[2] = 104) and (buffer[3] = 100)) then break else
        inc(posizione);
    end; // fine while

    if posizione = -1 then begin
      FreeHandleStream(Stream);
      exit;
    end;

    stream.seek(posizione, sofrombeginning);
    stream.Read(buffer, 88);

    FreeHandleStream(Stream);


    wres := buffer[78];
    wres := wres shl 8;
    wres := wres + buffer[79];
    wres := wres shl 8;
    wres := wres + buffer[80];
    wres := wres shl 8;
    result.bitrate := wres + buffer[81];

    hres := buffer[82];
    hres := hres shl 8;
    hres := hres + buffer[83];
    hres := hres shl 8;
    hres := hres + buffer[84];
    hres := hres shl 8;
    result.frequency := hres + buffer[85];

    result.codec := '';


  except
    FreeHandleStream(Stream);
  end;

end;

function ricava_dati_avi(nomefile: string): record_audioinfo;
var
  stream: thandlestream;
  buffer: array[0..116] of byte;
  framerate, wres, hres: integer;
  codec: string;
  count: longint;
begin
  result.duration := 0;
  result.bitrate := 0;
  result.frequency := 0;
  result.codec := '';

  stream := MyFileOpen(nomefile);
  if stream = nil then exit;


  try
    count := stream.Read(buffer, 116);

    if count <> 116 then exit;

    count := buffer[35];
    count := count shl 8;
    count := count + buffer[34];
    count := count shl 8;
    count := count + buffer[33];
    count := count shl 8;
    count := count + buffer[32];
    if count > 0 then framerate := 1000000 div count else framerate := 0; // 24000 fotogrammi al millesimo di secondo
    if framerate = 0 then begin
      exit;
    end;
    count := buffer[51];
    count := count shl 8;
    count := count + buffer[50];
    count := count shl 8;
    count := count + buffer[49];
    count := count shl 8;
    count := count + buffer[48];
    count := count * 1000; // perch?non ho mollato il framerate
    result.duration := (count div (framerate)) div 1000;

    wres := buffer[67];
    wres := wres shl 8;
    wres := wres + buffer[66];
    wres := wres shl 8;
    wres := wres + buffer[65];
    wres := wres shl 8;
    result.bitrate := wres + buffer[64];

    hres := buffer[71];
    hres := hres shl 8;
    hres := hres + buffer[70];
    hres := hres shl 8;
    hres := hres + buffer[69];
    hres := hres shl 8;
    result.frequency := hres + buffer[68];
    codec := '';
    codec := codec + chr(ord(buffer[112]));
    codec := codec + chr(ord(buffer[113]));
    codec := codec + chr(ord(buffer[114]));
    result.codec := codec + chr(ord(buffer[115]));
  finally
    if Assigned(Stream) then
      FreeHandleStream(Stream);
  end;
end;


constructor TMediaInfoReader.Create;
begin
  mp3 := TMPEGaudio.create;
  wav := Twavfile.create;
  ogg := TOggVorbis.create;
  wma := TWMAfile.create;
  flac := TFLACfile.create;
  ape := Tmonkey.create;
  vqf := TTwinVQ.create;
  aac := Taacfile.create;
  mpc := TMPCFile.create;
//  immagine := tdcimageinfo.create;

  New( raudio );
end;

destructor TMediaInfoReader.Destroy;
begin
  try
    if mp3 <> nil then mp3.free;
    if wav <> nil then wav.free;
    if ogg <> nil then ogg.free;
    if wma <> nil then wma.free;
    if flac <> nil then flac.free;
    if ape <> nil then ape.free;
    if vqf <> nil then vqf.free;
    if aac <> nil then aac.free;
    if mpc <> nil then mpc.free;
//    if immagine <> nil then immagine.free;
    Dispose( raudio );
  except
  end;
  inherited;
end;

function TMediaInfoReader.FileCreateTime(AFileName: string): TDateTime;
  function CovFileDate(Fd:_FileTime):TDateTime;
  { 转换文件的时间格式 }
  var
  Tct:_SystemTime;
  Temp:_FileTime;
  begin
  FileTimeToLocalFileTime(Fd,Temp);
  FileTimeToSystemTime(Temp,Tct);
  CovFileDate:=SystemTimeToDateTime(Tct);
  end;
var
  RefHandle: THandle;
  tmpCreationTime: TFileTime;
begin
  Result := Now;
  //初始化时间,2004-08-08  04:00
  tmpCreationTime.dwLowDateTime := 515989504;
  tmpCreationTime.dwHighDateTime := 29654201;
  //
  RefHandle := CreateFile(PChar(AFileName), GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if RefHandle <> INVALID_HANDLE_VALUE then
  begin
    GetFileTime(RefHandle, PFileTime(@tmpCreationTime), nil, nil);
    CloseHandle(RefHandle);
    result := CovFileDate(tmpCreationTime);
  end;
end;

function TMediaInfoReader.FormatTimeToStr(MiliSec: Cardinal): string;
var
  sec: Cardinal;
  H, M, S: Cardinal;
begin
  Result := '';
//  sec := MiliSec div 1000;
  sec := MiliSec;
  //
  H := Sec div 3600;
  if H > 0 then
  begin
    if H < 10 then
      Result := '0' + IntToStr(H) + ':'
    else
      Result := IntToStr(H) + ':';
  end;

  M := (Sec mod 3600) div 60;
  if M < 10 then
    Result := Result + '0' + IntToStr(M) + ':'
  else
    Result := Result + IntToStr(M) + ':';

  S := Sec mod 60;
  if S < 10 then
    Result := Result + '0' + IntToStr(S)
  else
    Result := Result + IntToStr(S);
end;


function TMediaInfoReader.IsSkipType(AExt: string): Boolean;
const
  SReadFileExt = '.mp3.mp4.aac.wma.wav.avi.';
begin
  result := not (Pos(AExt, SReadFileExt)>0);
end;

function TMediaInfoReader.JAPEncode(const AContent: string): string;
var
  i, j, len: integer;
  tmpContent: WideString;
  tmpWord: Word;
begin
  Result := Trim(AContent);
  if Result='' then Exit;
  tmpContent := StringToOleStr(AContent);
  len := Length(tmpContent);
  for I := 1 to len do
  begin
    tmpWord := Ord(tmpContent[i]);
    for j := Low(FilterJapWord) to High(FilterJapWord) do
    begin
      if ((tmpWord>=$30A0) and (tmpWord<=$30FF)) or ((tmpWord>=$3040) and (tmpWord<=$309F)) then
      begin
        tmpContent[i] := ' ';
        Break;
      end;
    end;
  end;
  Result := tmpContent;
end;

function TMediaInfoReader.JudgeString(AStr: string): Boolean;
var
  I: Integer;
  PreMBT: TMbcsByteType;
  MBT: TMbcsByteType; // TMbcsByteType = (mbSingleByte, mbLeadByte, mbTrailByte);
begin
  Result := True;
  PreMBT := mbSingleByte;
  for I := 1 to Length(AStr) do
  begin
    MBT := ByteType(AStr, I);
    //判断是否是可显字符
    if (MBT = mbSingleByte) and ( not (AStr[I] in [Chr($21)..Chr($7e)])) then
    begin
      Result := False;
      Exit;
    end;

    case PreMBT of
      mbSingleByte:
        begin
          if MBT = mbTrailByte then
          begin
            Result := False;
            Exit;
          end
          else
          begin
            PreMBT := MBT; 
          end;
        end;
      mbLeadByte:
        begin
          if MBT <> mbTrailByte then
          begin
            Result := False;
            Exit;
          end
          else
            PreMBT := MBT;
        end;
      mbTrailByte:
        begin
          if MBT = mbTrailByte then
          begin
            Result := False;
            Exit;
          end
          else
            PreMBT := MBT;
        end;
    end;
  end;

  if PreMBT = mbLeadByte  then
  begin
    Result := False;
    Exit;
  end;
end;

function TMediaInfoReader.ReadMediaInfo(AFileName: string; PMediaInfo: PRecord_MediaFile_Info;
   bReadDetail: Boolean): Boolean;
var
  oldTitle: string;
begin
  Result := False;
  try
//   OutputDebugString(PChar(AFileName));
    ZeroMemory(PMediaInfo, SizeOf(Record_MediaFile_Info));
    PMediaInfo^.Ext := LowerCase(ExtractFileExt(AFileName));
    PMediaInfo^.Format := PMediaInfo^.Ext;
    oldTitle := ExtractFileName(ChangeFileExt(AFileName, ''));
    PMediaInfo^.Title := oldTitle;
    PMediaInfo^.FileDate := Now;
    PMediaInfo^.FilePath := AFileName;
    PMediaInfo^.FileSize := GetFileSizeEx(AFileName);
    PMediaInfo^.Year := '';
    PMediaInfo^.DetailInfo := bReadDetail;
    PMediaInfo^.Artist := '';
    PMediaInfo^.album := '';
    PMediaInfo^.category := '';

//    if (not bReadDetail) or (IsSkipType(PMediaInfo^.Ext)) then
//    begin
//      Result := True;
//      Exit;
//    end;


    if PMediaInfo^.Ext = '.mp3' then
    begin // mp3
      try
        if not mp3.ReadFromFile(AFileName) then exit;
      except
        Exit;
      end;
      if not mp3.Valid then exit;
      PMediaInfo^.FileSize := mp3.FileSize;
      PMediaInfo^.BitRate := mp3.BitRate;
      PMediaInfo^.Duration := trunc(mp3.Duration);
      PMediaInfo^.SampleRate := mp3.SampleRate;
      if mp3.id3v2.exists then
      begin
        PMediaInfo^.title := mp3.id3v2.Title;
        PMediaInfo^.artist := mp3.id3v2.artist;
        PMediaInfo^.album := mp3.id3v2.Album;
        PMediaInfo^.category := mp3.id3v2.Genre;
        PMediaInfo^.comment := '';
        PMediaInfo^.year := mp3.id3v2.Year;
        if mp3.id3v2.comment <> mp3.id3v2.Link then PMediaInfo^.url := mp3.id3v2.Link;
      end else
      if mp3.ID3v1.Exists then
      begin
        PMediaInfo^.title := mp3.id3v1.Title;
        PMediaInfo^.artist := mp3.id3v1.artist;
        PMediaInfo^.album := mp3.id3v1.Album;
        PMediaInfo^.category := mp3.id3v1.Genre;
        PMediaInfo^.comment := '';
        PMediaInfo^.year := mp3.id3v1.Year;
      end;
    end else
    if ((PMediaInfo^.ext = '.aac') or (PMediaInfo^.ext = '.mp4')) then
    begin
      try
        if not aac.ReadFromFile(AFileName) then exit;
      except
        exit;
      end;
      if not aac.Valid then exit;

      PMediaInfo^.BitRate := aac.BitRate;
      PMediaInfo^.Duration := trunc(aac.Duration);
      PMediaInfo^.SampleRate := aac.SampleRate;
      if aac.id3v2.exists then
      begin
        PMediaInfo^.title := aac.id3v2.Title;
        PMediaInfo^.artist := aac.id3v2.artist;
        PMediaInfo^.album := aac.id3v2.Album;
        PMediaInfo^.category := aac.id3v2.Genre;
        PMediaInfo^.comment := aac.id3v2.Comment;
        PMediaInfo^.year := aac.id3v2.Year;
        PMediaInfo^.url := aac.id3v2.Link;
      end else
      if aac.ID3v1.Exists then
      begin
        PMediaInfo^.title := aac.id3v1.Title;
        PMediaInfo^.artist := aac.id3v1.artist;
        PMediaInfo^.album := aac.id3v1.Album;
        PMediaInfo^.category := aac.id3v1.Genre;
        PMediaInfo^.comment := aac.id3v1.Comment;
        PMediaInfo^.year := aac.id3v1.Year;
      end;
    end else
    if PMediaInfo^.ext = '.ogg' then
    begin
      try
        if not ogg.ReadFromFile(AFileName) then exit;
      except
        exit;
      end;
      if not ogg.Valid then exit;
      PMediaInfo^.BitRate := ogg.BitRateNominal;
      PMediaInfo^.SampleRate := ogg.SampleRate;
      PMediaInfo^.Duration := trunc(ogg.duration);

      PMediaInfo^.title := ogg.Title;
      PMediaInfo^.artist := ogg.Artist;
      PMediaInfo^.album := ogg.Album;
      PMediaInfo^.year := ogg.Date;
      PMediaInfo^.comment := ogg.Comment;
      PMediaInfo^.category := ogg.Genre;
    end else
    if PMediaInfo^.ext = '.wma'  then
    begin
      try
        if not wma.ReadFromFile(AFileName) then exit;
      except
        exit;
      end;
      if not wma.Valid then exit;
      PMediaInfo^.BitRate := wma.BitRate;
      PMediaInfo^.SampleRate := wma.SampleRate;
      PMediaInfo^.Duration := trunc(wma.duration);
      PMediaInfo^.title := wma.Title;
      PMediaInfo^.artist := wma.Artist;
      PMediaInfo^.album := wma.album;
      PMediaInfo^.category := wma.genre;
      PMediaInfo^.comment := wma.comment;
      PMediaInfo^.year := wma.year;
    end else
    if PMediaInfo^.ext = '.wav' then
    begin
      try
        if not wav.ReadFromFile(AFileName) then exit;
      except
        exit;
      end;
      if not wav.Valid then exit;
      PMediaInfo^.BitRate := wav.BitsPerSample;
      PMediaInfo^.SampleRate := wav.SampleRate;
      PMediaInfo^.Duration := Trunc(wav.duration);
    end else
    if PMediaInfo^.ext = '.avi' then
    begin
      try
        raudio^ := ricava_dati_avi( StringTOOLeStr(AFileName) );
        PMediaInfo^.BitRate := raudio^.bitrate;
        PMediaInfo^.SampleRate := raudio^.frequency;
        PMediaInfo^.Duration := raudio^.duration;
        PMediaInfo^.format := 'AVI ' + uppercase(raudio^.codec);
        if PMediaInfo^.BitRate = 0 then
        begin
          PMediaInfo^.format := 'AVI';
          try
            info_video := getmediainfo(StringTOOLeStr(AFileName));
            if info_video.width < 4000 then
            begin
              PMediaInfo^.BitRate := info_video.Width;
              PMediaInfo^.SampleRate := info_video.height;
              PMediaInfo^.Duration := info_video.medialength div 10000000;
            end else
            begin
              PMediaInfo^.BitRate := 0;
              PMediaInfo^.SampleRate := 0;
              PMediaInfo^.Duration := 0;
            end;
          except
            PMediaInfo^.BitRate := 0;
            PMediaInfo^.SampleRate := 0;
            PMediaInfo^.Duration := 0;
          end;
        end;
      except
        PMediaInfo^.format := 'AVI';
        try
          info_video := getmediainfo(StringTOOLeStr(AFileName));
          if info_video.width < 4000 then begin
            PMediaInfo^.BitRate := info_video.Width;
            PMediaInfo^.SampleRate := info_video.height;
            PMediaInfo^.Duration := info_video.medialength div 10000000;
          end else begin
            PMediaInfo^.BitRate := 0;
            PMediaInfo^.SampleRate := 0;
            PMediaInfo^.Duration := 0;
          end;
        except
          PMediaInfo^.BitRate := 0;
          PMediaInfo^.SampleRate := 0;
          PMediaInfo^.Duration := 0;
        end;
      end;
      if ((PMediaInfo^.BitRate = 0) or (PMediaInfo^.SampleRate = 0) or (PMediaInfo^.Duration = 0)) then
      begin
        PMediaInfo^.BitRate := 0;
        PMediaInfo^.SampleRate := 0;
        PMediaInfo^.Duration := 0;
      end;
      if PMediaInfo^.BitRate = 0 then exit;
    end else
    if ((PMediaInfo^.ext = '.mpe') or (PMediaInfo^.ext = '.mpg') or (PMediaInfo^.ext = '.mpa') or (PMediaInfo^.ext = '.mpeg')) then
    begin
      try
        info_video := getmediainfo(StringTOOLeStr(AFileName));
        PMediaInfo^.BitRate := info_video.Width;
        PMediaInfo^.SampleRate := info_video.height;
        PMediaInfo^.Duration := info_video.medialength div 10000000;
      except
        PMediaInfo^.BitRate := 0;
        PMediaInfo^.SampleRate := 0;
        PMediaInfo^.Duration := 0;
      end;
      if ((PMediaInfo^.BitRate = 0) or (PMediaInfo^.SampleRate = 0) or (PMediaInfo^.Duration = 0)) then
      begin
        PMediaInfo^.BitRate := 0;
        PMediaInfo^.SampleRate := 0;
        PMediaInfo^.Duration := 0;
      end;
      PMediaInfo^.format := 'MPEG';
      if PMediaInfo^.BitRate = 0 then exit;
    end else
    begin

    end;

   //替换日文中几个特别字符，避免导致access出错的问题
    PMediaInfo^.Title := JAPEncode(PMediaInfo^.Title);
    PMediaInfo^.Artist := JAPEncode(PMediaInfo^.Artist);
    PMediaInfo^.Category := JAPEncode(PMediaInfo^.Category);
    PMediaInfo^.Format := JAPEncode(PMediaInfo^.Format);
    PMediaInfo^.Vidinfo := JAPEncode(PMediaInfo^.Vidinfo);
    PMediaInfo^.Comment := JAPEncode(PMediaInfo^.Comment);
    PMediaInfo^.Language := JAPEncode(PMediaInfo^.Language);

    if Trim(PMediaInfo^.Title)='' then
      PMediaInfo^.Title := oldTitle;
    PMediaInfo^.Quanlity := PMediaInfo.BitRate;
    PMediaInfo^.DurationString := FormatTimeToStr(PMediaInfo^.Duration);
    Result := True;
  except
    Result := False;
  end;
end;

end.

