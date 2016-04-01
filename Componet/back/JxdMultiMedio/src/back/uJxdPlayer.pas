unit uJxdPlayer;

interface
uses
  Windows, Classes, ExtCtrls, Controls, Messages, DSPack, DirectShow9, uJxdAudioFilter, DSUtil, uJxdDrawPanel, Forms;

type
  TxdVideoWindow = class(TVideoWindow)
  private
    FFullScreen: Boolean;
    procedure SetFullScreen(const Value: Boolean);
  published//(TDSVideoWindowEx2)
  published
//    property FullScreen: Boolean read FFullScreen write SetFullScreen;
  end;
  TPlayerState = (psNULL, psPlaying, psPause);
  {$M+}
  TxdPlayer = class(TxdDrawPanel)
  public
    procedure Stop;
    function  Play(const AFileName: WideString): Boolean; overload;
    function  Pause: Boolean;

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    function  Play: Boolean; overload;
    function  CreatePlayerFilter: Boolean;
    procedure Loaded; override;
    //事件处理
    procedure DoVideoDblClick(Sender: TObject);
    procedure DoVideoKeyPress(Sender: TObject; var Key: Char);

    procedure DoFreePlayer(Sender: TObject);
    procedure DoGraphComplete(sender: TObject; Result: HRESULT; Renderer: IBaseFilter);
  private
    FPlayerState: TPlayerState;
    FCurPlayFileName: WideString;
    //DirectShow 对象
    FPlayerGraph: TFilterGraph;
    FVideoWindow: TxdVideoWindow;
    FAudioStateFilter: TAudioFilter;
    FAudioState: TAudioState;

    FOnEndPlayer: TNotifyEvent;
    procedure SetAudioState(const Value: TAudioState);
    function GetBalance: integer;
    function GetDuration: Integer;
    function GetRate: Double;
    function GetVolume: integer;
    procedure SetBalance(const Value: integer);
    procedure SetRate(const Value: Double);
    procedure SetVolume(const Value: integer);
    function GetCurPosition: Integer;

  published
    property CurPosition: Integer read GetCurPosition;
    property Duration: Integer read GetDuration;
    property Rate: Double read GetRate write SetRate;
    property Balance: integer read GetBalance write SetBalance;
    property Volume: integer read GetVolume write SetVolume;

    property PlayerState: TPlayerState read FPlayerState;
    property CurPlayFileName: WideString read FCurPlayFileName;
    property AudioState: TAudioState read FAudioState write SetAudioState;
    property OnEndPlayer: TNotifyEvent read FOnEndPlayer write FOnEndPlayer;
  end;
  {$M-}
implementation

{ TxdPlayer }

constructor TxdPlayer.Create(AOwner: TComponent);
begin
  inherited;
  FAudioState := asAll;
end;

function TxdPlayer.CreatePlayerFilter: Boolean;
begin
  FPlayerGraph := TFilterGraph.Create( Self );
  FPlayerGraph.Mode := gmNormal;
  FPlayerGraph.Active := True;
  FPlayerGraph.OnGraphComplete := DoGraphComplete;

  FVideoWindow := TxdVideoWindow.Create( Self );
  FVideoWindow.Parent := Self;
  FVideoWindow.Align := alClient;
  FVideoWindow.Visible := True;
  FVideoWindow.FilterGraph := FPlayerGraph;
  FVideoWindow.OnDblClick := DoVideoDblClick;
  FVideoWindow.OnKeyPress := DoVideoKeyPress;

  FAudioStateFilter := TAudioFilter.Create;
  (FPlayerGraph as IGraphBuilder).AddFilter( FAudioStateFilter, 'Audio State Filter' );
  FAudioStateFilter.put_State( FAudioState );
  
  Result := True;
end;

destructor TxdPlayer.Destroy;
begin

  inherited;
end;

procedure TxdPlayer.DoVideoDblClick(Sender: TObject);
begin
  if FPlayerState = psPlaying then
  begin
    with Application.MainForm do
    begin
      FormStyle := fsStayOnTop;
      Left := 0;
      Top := -20;
      Height := 1000;
      Width := 1280;
    end;
//    Width := 1280;
//    Height := 800;
//    FVideoWindow.FullScreen := not FVideoWindow.FullScreen;
//    Application.MainForm.Visible := not FVideoWindow.FullScreen;
  end;
end;

procedure TxdPlayer.DoVideoKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) and Assigned(FVideoWindow) and FVideoWindow.FullScreen then
    FVideoWindow.FullScreen := False;
end;

procedure TxdPlayer.DoFreePlayer(Sender: TObject);
begin
  Stop;
  if Sender is TTimer then
    (Sender as TTimer).Free;
end;

procedure TxdPlayer.DoGraphComplete(sender: TObject; Result: HRESULT; Renderer: IBaseFilter);
begin
  if Assigned(OnEndPlayer) then
    OnEndPlayer( Self );
  with TTimer.Create(nil) do
  begin
    Interval := 50;
    OnTimer := DoFreePlayer;
    Enabled := True;
  end;
end;

function TxdPlayer.GetBalance: integer;
begin
  Result := 0;
  if Assigned(FPlayerGraph) then
    Result := FPlayerGraph.Balance;
end;

function TxdPlayer.GetCurPosition: Integer;
var
  MediaSeeking: IMediaSeeking;
  RefTime: int64;
begin
  if Assigned(FPlayerGraph) and Succeeded(FPlayerGraph.QueryInterface(IMediaSeeking, MediaSeeking)) then
  begin
    MediaSeeking.GetCurrentPosition(RefTime);
    Result := RefTimeToMiliSec(RefTime);
    MediaSeeking := nil;
  end
  else
    Result := 0;
end;

function TxdPlayer.GetDuration: Integer;
begin
  Result := 0;
  if Assigned(FPlayerGraph) then
    Result := FPlayerGraph.Duration;
end;

function TxdPlayer.GetRate: Double;
begin
  Result := 0;
  if Assigned(FPlayerGraph) then
    Result := FPlayerGraph.Rate;
end;

function TxdPlayer.GetVolume: integer;
begin
  Result := 0;
  if Assigned(FPlayerGraph) then
    Result := FPlayerGraph.Volume;
end;

procedure TxdPlayer.Loaded;
begin
  inherited;
end;

function TxdPlayer.Pause: Boolean;
begin
  Result := (FPlayerState = psPlaying) and Assigned(FPlayerGraph) and FPlayerGraph.Active and FPlayerGraph.Pause;
  if Result then
    FPlayerState := psPause;
end;

function TxdPlayer.Play: Boolean;
begin
  Result := (FPlayerState <> psPlaying) and Assigned(FPlayerGraph) and FPlayerGraph.Active and FPlayerGraph.Play;
  if Result then
    FPlayerState := psPlaying;
end;

function TxdPlayer.Play(const AFileName: WideString): Boolean;
//var
//  w: IVideoWindow;
begin
  Result := False;
  try
    if FPlayerState = psPause then
    begin
      Result := Play;
      Exit;
    end;
    if FPlayerState = psPlaying then Exit;
    if not CreatePlayerFilter then Exit;
    Result := Succeeded( FPlayerGraph.RenderFile(AFileName) ) and Play;
    if not Result then
      Stop
    else
    begin
//      if Succeeded(FPlayerGraph.QueryInterface(IID_IVMRVideoStreamControl, w )) then
//        Stop;
    end;
  except
    Stop;
  end;
end;

procedure TxdPlayer.SetAudioState(const Value: TAudioState);
begin
  if (FPlayerState <> psNULL) and (FAudioState <> Value) then
  begin
    FAudioState := Value;
    if Assigned(FAudioStateFilter) then
      FAudioStateFilter.put_State( FAudioState );
  end;
end;

procedure TxdPlayer.SetBalance(const Value: integer);
begin
  if Assigned(FPlayerGraph) then
    FPlayerGraph.Balance := Value;
end;

procedure TxdPlayer.SetRate(const Value: Double);
begin
  if Assigned(FPlayerGraph) then
    FPlayerGraph.Rate := Value;
end;

procedure TxdPlayer.SetVolume(const Value: integer);
begin
  if Assigned(FPlayerGraph) then
    FPlayerGraph.Volume := Value;
end;

procedure TxdPlayer.Stop;
begin
  if FPlayerGraph <> nil then
  begin
    FPlayerGraph.Stop;
    FPlayerGraph.Active := False;
    FAudioStateFilter := nil;
    FreeAndNil( FVideoWindow );
    FreeAndNil( FPlayerGraph );
  end;
  FPlayerState := psNULL;
end;

{ TxdVideoWindow }

procedure TxdVideoWindow.SetFullScreen(const Value: Boolean);
var
  VideoWindow: IVideoWindow;
  FWindowStyle, StyleEX: Cardinal;
begin
  FWindowStyle := 0;
  if Succeeded(QueryInterface(IID_IVideoWindow, VideoWindow)) then
  begin
    try
      if Value then
      begin
        CheckDSError(VideoWindow.put_Owner(0));
        CheckDSError(VideoWindow.put_WindowStyle(FWindowStyle and not(WS_BORDER or WS_CAPTION or WS_THICKFRAME)));
//        StyleEX := FWindowStyleEx and not(WS_EX_CLIENTEDGE or WS_EX_STATICEDGE
//          or WS_EX_WINDOWEDGE or WS_EX_DLGMODALFRAME);
//        if FTopMost then StyleEX := StyleEX or WS_EX_TOPMOST;
//        CheckDSError(VideoWindow.put_WindowStyleEx(StyleEX));
        CheckDSError(VideoWindow.SetWindowPosition(0, 0, Screen.Width, Screen.Height));
//        FIsFullScreen := True;
      end;
    finally
      VideoWindow := nil;
    end;
  end;
  FFullScreen := Value;
end;

end.
