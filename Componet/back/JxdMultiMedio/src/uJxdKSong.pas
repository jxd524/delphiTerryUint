unit uJxdKSong;

interface

uses
  Windows, Classes, ExtCtrls, Controls, uJxdPlayer, uJxdRecordCap, uJxdAudioFilter, uJxdOverlayEffect, DirectShow9, Graphics;

type
  TShowMode = (smPlayer, smRecord, smOlnyPlayer, smOlnyRecord);
  TKSongState = (ssPlayer, ssPlayerAndRecord, ssRecord, ssNULL);
  TOnGetFileName = function(Sender: TObject; const ANext: Boolean; var AFileName: WideString): Boolean of object;
  TOnPlayerStateChanaged = procedure(Sender: TObject; AState: TJxdPlayerMode) of object;
  TxdKSong = class(TCustomPanel)
  public
    procedure Stop;

    //播放器控制
    function  Play: Boolean;
    function  PlayNext: Boolean;
    function  PlayPre: Boolean;
    procedure Pause;
    procedure RepaintPlayer;

    //录制控制
    procedure SetOverlayEffect(AEffect: TOverlayEffect);
    function  StartRecord(const AFileName: WideString): Boolean;

    //播放并录制
    function PlayAndRecord(const APlayFileName, ARecordOutFileName: WideString): Boolean;

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    procedure Loaded; override;
  private
    FKSongState: TKSongState;
    FPlayer: TxdPlayer;
    FRecord: TxdRecordCap;
    procedure ShowPlayerMode;
    procedure ShowRecordMode;
    procedure ShowOlnyPlayerMode;
    procedure ShowOlnyRecordMode;

    procedure NotifyPalyerChanged(const AState: TJxdPlayerMode);
    procedure DoPlayerPause(Sender: TObject);
    procedure DoPlayerComplete(Sender: TObject);
    procedure DoPlayerPlaying(Sender: TObject);
  private
    FSmallWindowPos: TRect;
    FShowMode: TShowMode;
    FSmallAnchorLeftTop: Boolean;
    FAudioCapIndex: Integer;
    FVideoCapIndex: Integer;
    FAudioCapInPinIndex: Integer;
    FOnGetPlayFileName: TOnGetFileName;
    FOnPlayerStateChanged: TOnPlayerStateChanaged;
    procedure SetSmallWindowPos(const Value: TRect);
    procedure SetShowMode(const Value: TShowMode);
    procedure SetSmallAnchorLeftTop(const Value: Boolean);
    function  GetBalance: integer;
    function  GetCurPosition: Integer;
    function  GetDuration: Int64;
    function  GetRate: Double;
    function  GetVolume: integer;
    procedure SetBalance(const Value: integer);
    procedure SetRate(const Value: Double);
    procedure SetVolume(const Value: integer);
    function  GetPlayBackBitmap: TBitmap;
    procedure SetPlayBackBitmap(const Value: TBitmap);
    function  GetRecordBackBitmap: TBitmap;
    procedure SetRecordBackBitmap(const Value: TBitmap);
    function  GetAudioState: TAudioState;
    procedure SetAudioState(const Value: TAudioState);
    procedure SetCurPosition(const Value: Integer);
  published
    property Align;
    property Alignment;
    property Anchors;
    property Color;
    property TabStop;
    property OnClick;
    property OnDblClick;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnCanResize;

    //整体显示模式
    property ShowMode: TShowMode read FShowMode write SetShowMode;
    property SmallAnchorLeftTop: Boolean read FSmallAnchorLeftTop write SetSmallAnchorLeftTop;
    property SmallWindowPos: TRect read FSmallWindowPos write SetSmallWindowPos;

    //录制输入设置
    property RecordBackBitmap: TBitmap read GetRecordBackBitmap write SetRecordBackBitmap;
    property VideoCapIndex: Integer read FVideoCapIndex write FVideoCapIndex;
    property AudioCapIndex: Integer read FAudioCapIndex write FAudioCapIndex;
    property AudioCapInPinIndex: Integer read FAudioCapInPinIndex write FAudioCapInPinIndex;

    //播放信息相关属性
    property PlayBackBitmap: TBitmap read GetPlayBackBitmap write SetPlayBackBitmap;
    property PlayCurPosition: Integer read GetCurPosition write SetCurPosition;
    property PlayDuration: Int64 read GetDuration;
    property PlayRate: Double read GetRate write SetRate;
    property PlayBalance: integer read GetBalance write SetBalance;
    property PlayVolume: integer read GetVolume write SetVolume;
    property PlayAudioState: TAudioState read GetAudioState write SetAudioState;

    //播放器事件
    property OnGetPlayFileName: TOnGetFileName read FOnGetPlayFileName write FOnGetPlayFileName;
    property OnPlayerStateChanged: TOnPlayerStateChanaged read FOnPlayerStateChanged write FOnPlayerStateChanged; 
  end;

implementation

{ TxdKSong }

constructor TxdKSong.Create(AOwner: TComponent);
begin
  inherited;
  Caption := '';
  BevelInner := bvNone;
  BevelKind := bkNone;
  BevelOuter := bvNone;
  
  FSmallAnchorLeftTop := True;
  FKSongState := ssNULL;

  FPlayer := TxdPlayer.Create( Self );
  with FPlayer do
  begin
    Parent  := Self;
    OnPlay  := DoPlayerPlaying;
    OnStop  := DoPlayerComplete;
    OnPause := DoPlayerPause;
  end;

  FRecord := TxdRecordCap.Create( Self );
  FRecord.Parent := Self;

  FSmallWindowPos := Rect( 0, 0, 320, 240 );
  ShowMode := smPlayer;
  FAudioCapIndex := 0;
  FVideoCapIndex := 0;
  FAudioCapInPinIndex := 0;
end;

destructor TxdKSong.Destroy;
begin
  FPlayer.Free;
  FRecord.Free;
  inherited;
end;

procedure TxdKSong.DoPlayerComplete(Sender: TObject);
begin
  NotifyPalyerChanged( pmStoped );
end;

procedure TxdKSong.DoPlayerPause(Sender: TObject);
begin
  NotifyPalyerChanged( pmPaused );
end;

procedure TxdKSong.DoPlayerPlaying(Sender: TObject);
begin
  NotifyPalyerChanged( pmPlaying );
end;

function TxdKSong.GetAudioState: TAudioState;
begin
  Result := FPlayer.AudioState;
end;

function TxdKSong.GetBalance: integer;
begin
  Result := 0;
//  Result := FPlayer.Balance;
end;

function TxdKSong.GetCurPosition: Integer;
begin
  Result := FPlayer.CurPlayPos;
end;

function TxdKSong.GetDuration: Int64;
begin
  Result := FPlayer.Duration;
end;


function TxdKSong.GetPlayBackBitmap: TBitmap;
begin
  Result := FPlayer.BackBitmap;
end;

function TxdKSong.GetRate: Double;
begin
  Result := 0;
//  Result := FPlayer.Rate;
end;

function TxdKSong.GetRecordBackBitmap: TBitmap;
begin
  Result := FRecord.BackBitmap;
end;

function TxdKSong.GetVolume: integer;
begin
  Result := 0;
//  Result := FPlayer.Volume;
end;

procedure TxdKSong.Loaded;
begin
  inherited;
  Caption := '';
  BevelInner := bvNone;
  BevelKind := bkNone;
  BevelOuter := bvNone;
  ShowMode := ShowMode;
end;

procedure TxdKSong.NotifyPalyerChanged(const AState: TJxdPlayerMode);
begin
  if Assigned(OnPlayerStateChanged) then
    OnPlayerStateChanged( Self, AState );
end;

procedure TxdKSong.Pause;
begin
  FPlayer.Pause;
end;

function TxdKSong.Play: Boolean;
var
  strFileName: WideString;
begin
  ShowMode := ShowMode;
  case FPlayer.CurPlayerMode of
    pmNoToOpened,
    pmStoped:
    begin
      Result := Assigned(OnGetPlayFileName) and OnGetPlayFileName(Self, True, strFileName);
      if Result then
        Result := FPlayer.Open( strFileName ) and FPlayer.Play;
    end;
    pmOpened,
    pmPaused:    Result := FPlayer.Play;
    else
      Result := True;
  end;
end;

function TxdKSong.PlayAndRecord(const APlayFileName, ARecordOutFileName: WideString): Boolean;
begin
//  Result := StartRecord( ARecordOutFileName ) and Play( APlayFileName );
end;

function TxdKSong.PlayNext: Boolean;
begin
  Stop;
  Result := Play;
end;

function TxdKSong.PlayPre: Boolean;
var
  strFileName: WideString;
begin
  ShowMode := ShowMode;
  Stop;
  case FPlayer.CurPlayerMode of
    pmNoToOpened,
    pmStoped:
    begin
      Result := Assigned(OnGetPlayFileName) and OnGetPlayFileName(Self, False, strFileName);
      if Result then
        Result := FPlayer.Open( strFileName ) and FPlayer.Play;
    end;
    pmOpened,
    pmPaused:    Result := FPlayer.Play;
    else
      Result := True;
  end;
end;

procedure TxdKSong.RepaintPlayer;
begin
  FPlayer.RepaintVideo;
end;

procedure TxdKSong.SetAudioState(const Value: TAudioState);
begin
  FPlayer.AudioState := Value;
end;

procedure TxdKSong.SetBalance(const Value: integer);
begin
//  FPlayer.Balance := Value;
end;

procedure TxdKSong.SetCurPosition(const Value: Integer);
begin
  FPlayer.CurPlayPos := Value;
end;

procedure TxdKSong.SetOverlayEffect(AEffect: TOverlayEffect);
begin
  FRecord.SetOverlayEffer( AEffect );
end;


procedure TxdKSong.SetPlayBackBitmap(const Value: TBitmap);
begin
  FPlayer.BackBitmap := Value;
end;

procedure TxdKSong.SetRate(const Value: Double);
begin
//  FPlayer.Rate := Value;
end;

procedure TxdKSong.SetRecordBackBitmap(const Value: TBitmap);
begin
  FRecord.BackBitmap := Value;
end;

procedure TxdKSong.SetShowMode(const Value: TShowMode);
begin
  FShowMode := Value;
  case FShowMode of
    smPlayer:     ShowPlayerMode;
    smRecord:     ShowRecordMode;
    smOlnyPlayer: ShowOlnyPlayerMode;
    smOlnyRecord: ShowOlnyRecordMode;
  end;
end;

procedure TxdKSong.SetSmallAnchorLeftTop(const Value: Boolean);
begin
  FSmallAnchorLeftTop := Value;
end;

procedure TxdKSong.SetSmallWindowPos(const Value: TRect);
begin
  FSmallWindowPos := Value;
  ShowMode := ShowMode;
end;

procedure TxdKSong.SetVolume(const Value: integer);
begin
//  FPlayer.Volume := Value;
end;

procedure TxdKSong.ShowOlnyPlayerMode;
begin
  FRecord.Visible := False;
  with FPlayer do
  begin
    Left := 0;
    Top := 0;
    Width := Self.Width;
    Height := Self.Height;
    Anchors := [ akLeft, akTop, akRight, akBottom ];
    Visible := True;
  end;
end;

procedure TxdKSong.ShowOlnyRecordMode;
begin
  FPlayer.Visible := False;
  with FRecord do
  begin
    Left := 0;
    Top := 0;
    Width := Self.Width;
    Height := Self.Height;
    Anchors := [ akLeft, akTop, akRight, akBottom ];
    Visible := True;
  end;
end;

procedure TxdKSong.ShowPlayerMode;
begin
  with FPlayer do
  begin
    Left := 0;
    Top := 0;
    Width := Self.Width;
    Height := Self.Height;
    Anchors := [ akLeft, akTop, akRight, akBottom ];
    Visible := True;
  end;
  with FRecord do
  begin
    Left := FSmallWindowPos.Left;
    Top := FSmallWindowPos.Top;
    Width := FSmallWindowPos.Right - FSmallWindowPos.Left;
    Height := FSmallWindowPos.Bottom - FSmallWindowPos.Top;
    if SmallAnchorLeftTop then
      Anchors := [ akLeft, akTop ]
    else
      Anchors := [ akRight, akBottom ];
    Visible := True;
    BringToFront;
  end;
end;

procedure TxdKSong.ShowRecordMode;
begin
  with FRecord do
  begin
    Left := 0;
    Top := 0;
    Width := Self.Width;
    Height := Self.Height;
    Anchors := [ akLeft, akTop, akRight, akBottom ];
    Visible := True;
  end;
  with FPlayer do
  begin
    Left := FSmallWindowPos.Left;
    Top := FSmallWindowPos.Top;
    Width := FSmallWindowPos.Right - FSmallWindowPos.Left;
    Height := FSmallWindowPos.Bottom - FSmallWindowPos.Top;
    if SmallAnchorLeftTop then
      Anchors := [ akLeft, akTop ]
    else
      Anchors := [ akRight, akBottom ];
    Visible := True;
    BringToFront;
  end;
end;

function TxdKSong.StartRecord(const AFileName: WideString): Boolean;
begin
  Result := False;
  if FRecord.RecordStyle = rsRecord then Exit;
  with FRecord.OutPutFilterSetting do
  begin
    AudioBitRate := -1;
    AudioChannels := 2;
    VideoBitRate := -1;
    VideoWidth := 320;
    VideoHeight := 240;
    VideoQuality := -1;
    VideoMaxKeyFrameSpacing := -1;
    OutPutFileName := AFileName;
  end;
  with FRecord.SourceFilterSetting do
  begin
    AudioCapIndex := FAudioCapIndex;
    AudioCapInPinIndex := FAudioCapInPinIndex;
    VideoCapIndex := FVideoCapIndex;
    VideoWidth := 320;
    VideoHeight := 240;
    VideoFrame := 28;
  end;
  Result := FRecord.StartRecord;
end;

procedure TxdKSong.Stop;
begin
  FPlayer.Stop;
  FRecord.Stop;
end;

end.
