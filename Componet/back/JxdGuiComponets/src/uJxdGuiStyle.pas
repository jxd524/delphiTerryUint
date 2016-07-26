{
单元名称: uJxdGuiStyle
单元作者: 江晓德(jxd524@163.com)
说    明: 组件包使用类型定义
开始时间: 2010-06-08
修改时间: 2010-07-07 (最后修改)
}
unit uJxdGuiStyle;

interface
uses Windows, SysUtils, Classes, Graphics, uJxdParseGradient;

type
  TDrawStyle = (dsPaste, dsStretchyAll, dsStretchyUpToDown, dsStretchyLeftToRight);
  TxdComponentChanged = procedure(Sender: TObject; const IsChangedBySelf: Boolean) of object;
  TxdComponentState = (csNormal, csActive, csDown);
  TxdGuiComponetsError = class(Exception);

  //自画渐变信息
  TGradientDrawInfo = class(TPersistent)
  public
    ParseGradientNormal: TParseGradient;
    ParseGradientHover: TParseGradient;
    ParseGradientMouseDown: TParseGradient;
    
    constructor Create;
    destructor  Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  protected
    procedure Changed; dynamic;
    procedure CheckGradientObject(const AFree: Boolean);
  private
    FOnChange: TNotifyEvent;
    FGradientMouseDownText: string;
    FGradientWay: TGradientWay;
    FGradientHoverText: string;
    FGradientNormalText: string;
    procedure SetGradientHoverText(const Value: string);
    procedure SetGradientMouseDownText(const Value: string);
    procedure SetGradientNormalText(const Value: string);
    procedure SetGradientWay(const Value: TGradientWay);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property GradientWay: TGradientWay read FGradientWay write SetGradientWay default gwUpToDown;
    property GradientNormalText: string read FGradientNormalText write SetGradientNormalText;
    property GradientHoverText: string read FGradientHoverText write SetGradientHoverText;
    property GradientMouseDownText: string read FGradientMouseDownText write SetGradientMouseDownText;
  end;

  //绘制线条信息
  PLineInfo = ^TLineInfo;
  TLineInfo = record
    FColor: TColor;
    FWidth: Integer;
    FRect: TRect;
  end;
  TLineDrawInfo = class(TPersistent)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  protected
    procedure Changed; dynamic;
  private
    FOnChange: TNotifyEvent;
    FCount: Integer;
    FVisible: Boolean;
    FLinesInfo: array of TLineInfo;
    FDefaultColor: TColor;
    procedure SetCount(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    function  GetItem(index: Integer): PLineInfo;
    procedure SetItem(index: Integer; const Value: PLineInfo);
    procedure SetDefaultColor(const Value: TColor);
  public
    property Item[index: Integer]: PLineInfo read GetItem write SetItem;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Count: Integer read FCount write SetCount;
    property Visible: Boolean read FVisible write SetVisible;
    property DefaultColor: TColor read FDefaultColor write SetDefaultColor;
  end;

  //Bitmap图像信息
  TBitmapInfo = class(TPersistent)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  protected
    procedure Changed; dynamic;
    procedure DoBitmapChanged(Sender: TObject);
  private
    FBitmap: TBitmap;
    FBitmapCount: Integer;
    FOnChange: TNotifyEvent;
    FBitmapDrawStyle: TDrawStyle;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetBitmapCount(const Value: Integer);
    procedure SetBitmapStyle(const Value: TDrawStyle);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property BitmapCount: Integer read FBitmapCount write SetBitmapCount default 3;
    property BitmapDrawStyle: TDrawStyle read FBitmapDrawStyle write SetBitmapStyle default dsPaste;
  end;

  //小图标信息
  TGrayInfo = class(TPersistent)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  protected
    procedure Changed; dynamic;
  private
    FGray: TBitmap;
    FLeftSpace, FTopSpace: Integer;
    FOnChange: TNotifyEvent;
    FGrayCount: Integer;
    FGrayDrawStyle: TDrawStyle;
    procedure SetGray(const Value: TBitmap);
    procedure SetLeftSpace(const Value: Integer);
    procedure SetTopSpace(const Value: Integer);
    procedure SetGrayCount(const Value: Integer);
    procedure SetGrayDrawStyle(const Value: TDrawStyle);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Gray: TBitmap read FGray write SetGray;
    property GrayCount: Integer read FGrayCount write SetGrayCount default 1;
    property GrayDrawStyle: TDrawStyle read FGrayDrawStyle write SetGrayDrawStyle default dsPaste;
    property LeftSpace: Integer read FLeftSpace write SetLeftSpace default 3;
    property TopSpace: Integer read FTopSpace write SetTopSpace default 3;
  end;

procedure DrawGradientInfo(ADestCanvas: TCanvas; AParseGradient: TParseGradient; const ADriection: Boolean; const ALeft, ATop, AWidth, AHeight: Integer);
procedure DrawLinesInfo(ADestCanvas: TCanvas; const ALinesInfo: TLineDrawInfo; const AWidth, AHeight: Integer);

implementation

uses
  uJxdDrawSub;

procedure DrawLinesInfo(ADestCanvas: TCanvas; const ALinesInfo: TLineDrawInfo; const AWidth, AHeight: Integer);
var
  i: Integer;
  p: PLineInfo;
  R: TRect;
begin
  if ALinesInfo.Visible and (ALinesInfo.Count > 0) then
  begin
    with ALinesInfo do
    begin
      for i := 0 to Count - 1 do
      begin
        p := Item[i];
        R := p^.FRect;
        if p^.FWidth <= 0   then p^.FWidth := 1;

        if R.Left < 0   then R.Left := -R.Left - 1;
        if R.Top < 0    then R.Top := -R.Top - 1;
        if R.Right < 0  then R.Right := AWidth + R.Right + 1;
        if R.Bottom < 0 then R.Bottom := AHeight + R.Bottom + 1;

        DrawFrameBorder( ADestCanvas, p^.FColor, p^.FWidth, R );
      end;
    end;
  end;
end;

procedure DrawGradientInfo(ADestCanvas: TCanvas; AParseGradient: TParseGradient; const ADriection: Boolean; const ALeft, ATop, AWidth, AHeight: Integer);
var
  R: TRect;
  iLoop, nStep, nOldHeight, nHeight, nOldWidth, nWidth: Integer;
  p: PGradient;
begin
  if ADriection then
  begin
    nOldWidth := ALeft;
    for iLoop := 0 to AParseGradient.GradientList.Count - 1 do
    begin
      p := AParseGradient.GradientList[iLoop];
      if iLoop = AParseGradient.GradientList.Count - 1 then
        nWidth := AWidth + ALeft - nOldWidth
      else
        nWidth := Round(AWidth * p^.FPercent);
      nStep := nWidth div 2 + 1;
      R := Rect(nOldWidth, ATop, nOldWidth + nWidth, ATop + AHeight);
      DrawGradient( ADestCanvas, p^.FFromColor, p^.FColorTo, nStep, R, ADriection );
      nOldWidth := nOldWidth + nWidth;
    end;
  end
  else
  begin
    nOldHeight := ATop;
    for iLoop := 0 to AParseGradient.GradientList.Count - 1 do
    begin
      p := AParseGradient.GradientList[iLoop];
      if iLoop = AParseGradient.GradientList.Count - 1 then
        nHeight := AHeight + ATop - nOldHeight
      else
        nHeight := Round(AHeight * p^.FPercent);
      nStep := nHeight div 2 + 1;
      R := Rect(ALeft, nOldHeight, ALeft + AWidth, nHeight + nOldHeight);
      DrawGradient( ADestCanvas, p^.FFromColor, p^.FColorTo, nStep, R, ADriection );
      nOldHeight := nOldHeight + nHeight;
    end;
  end;
end;


{ TBitmapInfo }

procedure TBitmapInfo.Assign(Source: TPersistent);
begin
  if Source is TBitmapInfo then
  begin
    BitmapDrawStyle := TBitmapInfo(Source).BitmapDrawStyle;
    Bitmap := TBitmapInfo(Source).Bitmap;
    BitmapCount := TBitmapInfo(Source).BitmapCount;
  end;
end;

procedure TBitmapInfo.AssignTo(Dest: TPersistent);
begin
  if Dest is TBitmapInfo then
  begin
    TBitmapInfo(Dest).Bitmap := Bitmap;
    TBitmapInfo(Dest).BitmapCount := BitmapCount;
  end;
end;

procedure TBitmapInfo.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange( Self );
end;

constructor TBitmapInfo.Create;
begin
  inherited;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := DoBitmapChanged;
  FBitmapCount := 3;
end;

destructor TBitmapInfo.Destroy;
begin
  FreeAndNil( FBitmap );
  inherited;
end;

procedure TBitmapInfo.DoBitmapChanged(Sender: TObject);
begin
  Changed;
end;

procedure TBitmapInfo.SetBitmap(const Value: TBitmap);
begin
  if Assigned(Value) then
  begin
    FBitmap.Width := Value.Width;
    FBitmap.Height := Value.Height;
  end;
  FBitmap.Assign( Value );
  Changed;
end;

procedure TBitmapInfo.SetBitmapCount(const Value: Integer);
begin
  if (FBitmapCount <> Value) and (Value > 0) then
  begin
    FBitmapCount := Value;
    Changed;
  end;
end;

procedure TBitmapInfo.SetBitmapStyle(const Value: TDrawStyle);
begin
  if FBitmapDrawStyle <> Value then
  begin
    FBitmapDrawStyle := Value;
    Changed;
  end;
end;

{ TGrayInfo }

procedure TGrayInfo.Assign(Source: TPersistent);
begin
  if Source is TGrayInfo then
  begin
    Gray := TGrayInfo(Source).Gray;
    LeftSpace := TGrayInfo(Source).LeftSpace;
    TopSpace := TGrayInfo(Source).TopSpace;
  end;
end;

procedure TGrayInfo.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TGrayInfo then
  begin
    TGrayInfo(Dest).Gray := Gray;
    TGrayInfo(Dest).LeftSpace := LeftSpace;
    TGrayInfo(Dest).TopSpace := TopSpace;
  end;
end;

procedure TGrayInfo.Changed;
begin
  if Assigned(FOnChange) and (FGray.Width > 0) and (FGray.Height > 0) then
    FOnChange( Self );  
end;

constructor TGrayInfo.Create;
begin
  inherited;
  FGray := TBitmap.Create;
  FGrayCount := 1;
  FLeftSpace := 3;
  FTopSpace := 3;
end;

destructor TGrayInfo.Destroy;
begin
  FreeAndNil( FGray );
  inherited;
end;

procedure TGrayInfo.SetGray(const Value: TBitmap);
begin
  FGray.Assign( Value );
  if FGray.Empty then
  begin
    if Assigned(FOnChange) then
      OnChange( Self );
  end
  else
    Changed;
end;

procedure TGrayInfo.SetGrayCount(const Value: Integer);
begin
  if (FGrayCount <> Value) and (Value > 0) and (Value < 4) then
  begin
    FGrayCount := Value;
    Changed;
  end;
end;

procedure TGrayInfo.SetGrayDrawStyle(const Value: TDrawStyle);
begin
  if FGrayDrawStyle <> Value then
  begin
    FGrayDrawStyle := Value;
    Changed;
  end;
end;

procedure TGrayInfo.SetLeftSpace(const Value: Integer);
begin
  if FLeftSpace <> Value then
  begin
    FLeftSpace := Value;
    Changed;
  end;
end;

procedure TGrayInfo.SetTopSpace(const Value: Integer);
begin
  if TopSpace <> Value then
  begin
    FTopSpace := Value;
    Changed;
  end;
end;

{ TGradientDrawInfo }

procedure TGradientDrawInfo.Assign(Source: TPersistent);
begin
  if Source is TGradientDrawInfo then
  begin
    with Source as TGradientDrawInfo do
    begin
      GradientWay := Self.GradientWay;
      GradientNormalText := Self.GradientNormalText;
      GradientHoverText := Self.GradientHoverText;
      GradientMouseDownText := Self.GradientMouseDownText;
    end;
  end;
end;

procedure TGradientDrawInfo.AssignTo(Dest: TPersistent);
begin
  if Dest is TGradientDrawInfo then
  begin
    with Dest as TGradientDrawInfo do
    begin
      Self.GradientWay := GradientWay;
      Self.GradientNormalText := GradientNormalText;
      Self.GradientHoverText := GradientHoverText;
      Self.GradientMouseDownText := GradientMouseDownText;
    end;
  end;
end;

procedure TGradientDrawInfo.Changed;
begin
  if Assigned(FOnChange) then OnChange( Self );
end;

procedure TGradientDrawInfo.CheckGradientObject(const AFree: Boolean);
begin
  if AFree then
  begin
    FreeAndNil( ParseGradientNormal );
    FreeAndNil( ParseGradientHover );
    FreeAndNil( ParseGradientMouseDown );
    Exit;
  end;
  
  if not Assigned(ParseGradientNormal) then
  begin
    ParseGradientNormal := TParseGradient.Create;
    ParseGradientNormal.GradientMsg := GradientNormalText;
  end;
  if not Assigned(ParseGradientHover) then
  begin
    ParseGradientHover := TParseGradient.Create;
    ParseGradientHover.GradientMsg := GradientHoverText;
  end;
  if not Assigned(ParseGradientMouseDown) then
  begin
    ParseGradientMouseDown := TParseGradient.Create;
    ParseGradientMouseDown.GradientMsg := GradientMouseDownText;
  end;
end;

constructor TGradientDrawInfo.Create;
begin
  FGradientNormalText :=    '#Gradient{fromColor: $F7F7F7; ColorTo: $F1E5DD; percent: 100%}';
  FGradientHoverText :=     '#Gradient{fromColor: $FFFFFF; ColorTo: $F9F3F0; percent: 100%}';
  FGradientMouseDownText := '#Gradient{fromColor: $EFE2D9; ColorTo: $F9F3F0; percent: 100%}';
  CheckGradientObject( False );
end;

destructor TGradientDrawInfo.Destroy;
begin
  CheckGradientObject( True );
  inherited;
end;

procedure TGradientDrawInfo.SetGradientHoverText(const Value: string);
begin
  if FGradientHoverText <> Value then
  begin
    FGradientHoverText := Value;
    ParseGradientHover.GradientMsg := GradientHoverText;
    Changed;
  end;
end;

procedure TGradientDrawInfo.SetGradientMouseDownText(const Value: string);
begin
  if FGradientMouseDownText <> Value then
  begin
    FGradientMouseDownText := Value;
    ParseGradientMouseDown.GradientMsg := GradientMouseDownText;
    Changed;
  end;
end;

procedure TGradientDrawInfo.SetGradientNormalText(const Value: string);
begin
  if FGradientNormalText <> Value then
  begin
    FGradientNormalText := Value;
    ParseGradientNormal.GradientMsg := GradientNormalText;
    Changed;
  end;
end;

procedure TGradientDrawInfo.SetGradientWay(const Value: TGradientWay);
begin
  if FGradientWay <> Value then
  begin
    FGradientWay := Value;
    Changed;
  end;
end;

{ TLineDrawInfo }

procedure TLineDrawInfo.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Source is TLineDrawInfo then
  begin
    with Source as TLineDrawInfo do
    begin
      Self.Count := Count;
      Self.Visible := Visible;
      for i := 0 to Count - 1 do
        Self.Item[i] := Item[i];
      Changed;
    end;
  end;
end;

procedure TLineDrawInfo.AssignTo(Dest: TPersistent);
var
  i: Integer;
begin
  if Dest is TLineDrawInfo then
  begin
    with Dest as TLineDrawInfo do
    begin
      Count := Self.Count;
      Visible := Self.Visible;
      for i := 0 to Count - 1 do
        Item[i] := Self.Item[i];
      Changed;
    end;
  end;
end;


procedure TLineDrawInfo.Changed;
begin
  if Assigned(FOnChange) then
    OnChange( Self );
end;

constructor TLineDrawInfo.Create;
begin
  FCount := 0;
  FDefaultColor := $00D58F1B;
end;

destructor TLineDrawInfo.Destroy;
begin
  SetLength( FLinesInfo, 0 );
  inherited;
end;

function TLineDrawInfo.GetItem(index: Integer): PLineInfo;
begin
  Result := nil;
  if (index >= 0) and (index < FCount) then
    Result := @FLinesInfo[index];
end;

procedure TLineDrawInfo.SetCount(const Value: Integer);
var
  i, nBegin: Integer;
begin
  if FCount <> Value then
  begin
    FCount := Value;
    nBegin := High( FLinesInfo ) + 1;
    SetLength( FLinesInfo, FCount );
    for i := nBegin to High(FLinesInfo) do
    begin
      FLinesInfo[i].FColor := FDefaultColor;
      FLinesInfo[i].FWidth := 1;
      FLinesInfo[i].FRect := Rect(-1 - i, -1 - i, -1 - i, -1 - i);
    end;
    Changed;
  end;
end;

procedure TLineDrawInfo.SetDefaultColor(const Value: TColor);
begin
  if FDefaultColor <> Value then
  begin
    FDefaultColor := Value;
    Changed;
  end;
end;

procedure TLineDrawInfo.SetItem(index: Integer; const Value: PLineInfo);
begin
  if (index >= 0) and (index < FCount) then
  begin
    FLinesInfo[index].FColor := Value^.FColor;
    FLinesInfo[index].FWidth := Value^.FWidth;
    FLinesInfo[index].FRect := Value^.FRect;
  end;
end;

procedure TLineDrawInfo.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

end.
