unit uJxdForm;

interface

uses
  Windows, Messages, Classes, Forms, ExtCtrls, SysUtils;

type
  PRoundRectRgn = ^TRoundRectRgn;
  TRoundRectRgn = record
    FLeftRect, FTopRect: Integer;
    FRightRect, FBottomRect: Integer;
    FWidthEllipse, FHeightEllipse: Integer;
  end;
  TxdForm = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    FRoundRectRgn: PRoundRectRgn;
    procedure Resize; override;
//    procedure DoHide; override;
//    procedure DoShow; override;
  private
//    FAnimalForm: Boolean;
//    FAnimalTimer, FAlphaBlendTimer: TTimer;
//    FAnimalSize, FTempSize: TRect;
//    FCurSpeed: Integer;
    procedure CheckFormRect;
    procedure SetRoundRectRgn(const Value: PRoundRectRgn);
//    procedure DoTimerToAnimalForm(Sender: TObject);
//    procedure DoTimerToAlphaBlend(Sender: TObject);
  public
//    property AnimalForm: Boolean read FAnimalForm write FAnimalForm;
    property RoundRectRgn: PRoundRectRgn read FRoundRectRgn write SetRoundRectRgn;
  end;

implementation


{ TxdForm }

procedure TxdForm.CheckFormRect;
var
  rgn: HRGN;
begin
  if Assigned(FRoundRectRgn) then
  begin
    if WindowState = wsMaximized then
      rgn := CreateRoundRectRgn(0, 0, Width, Height, 0, 0 )
    else
      rgn := CreateRoundRectRgn( FRoundRectRgn^.FLeftRect, FRoundRectRgn^.FTopRect,
          Width - FRoundRectRgn^.FRightRect, Height - FRoundRectRgn^.FBottomRect,
          FRoundRectRgn^.FWidthEllipse, FRoundRectRgn^.FHeightEllipse );
    SetWindowRgn(Handle, rgn, True);
  end;
end;

constructor TxdForm.Create(AOwner: TComponent);
begin
  inherited;
  RoundRectRgn := nil;
//  AnimalForm := False;
end;

destructor TxdForm.Destroy;
begin
  if Assigned(RoundRectRgn) then
  begin
    Dispose( FRoundRectRgn );
    FRoundRectRgn := nil;
  end;
  inherited;
end;

//procedure TxdForm.DoHide;
//var
//  nTemp: Integer;
//begin
//  if AnimalForm then
//  begin
//    GetWindowRect( Handle, FAnimalSize );
//    FTempSize := FAnimalSize;
//
//    nTemp := 40;
//    FTempSize.Left := FTempSize.Left + nTemp;
//    FTempSize.Top := FTempSize.Top + nTemp;
//    FTempSize.Right := FTempSize.Right - nTemp;
//    FTempSize.Bottom := FTempSize.Bottom - nTemp;
//
//    {AlphaBlend Setting}
//    AlphaBlend := True;
//    AlphaBlendValue := 0;
//    if not Assigned(FAlphaBlendTimer) then
//      FAlphaBlendTimer := TTimer.Create( Self );
//    FAlphaBlendTimer.Interval := 50;
//    FAlphaBlendTimer.Tag := -20;
//    FAlphaBlendTimer.OnTimer := DoTimerToAlphaBlend;
//    FAlphaBlendTimer.Enabled := True;
//  end;
//  inherited;
//end;

//procedure TxdForm.DoShow;
//var
//  nTemp: Integer;
//begin
//  if AnimalForm then
//  begin
//    GetWindowRect( Handle, FAnimalSize );
//    FTempSize := FAnimalSize;
//
//    nTemp := 40;
//    FTempSize.Left := FTempSize.Left + nTemp;
//    FTempSize.Top := FTempSize.Top + nTemp;
//    FTempSize.Right := FTempSize.Right - nTemp;
//    FTempSize.Bottom := FTempSize.Bottom - nTemp;
//
//    {AlphaBlend Setting}
//    AlphaBlend := True;
//    AlphaBlendValue := 0;
//    FAlphaBlendTimer := TTimer.Create( Self );
//    FAlphaBlendTimer.Interval := 50;
//    FAlphaBlendTimer.Tag := 20;
//    FAlphaBlendTimer.OnTimer := DoTimerToAlphaBlend;
//    FAlphaBlendTimer.Enabled := True;
//
////    FAnimalTimer := TTimer.Create( nil );
////    FCurSpeed := 30;
////    FAnimalTimer.Interval := 20;
////    FAnimalTimer.OnTimer := DoTimerToAnimalForm;
////    FAnimalTimer.Enabled := True;
//  end;
//  inherited;
//end;

//procedure TxdForm.DoTimerToAlphaBlend(Sender: TObject);
//var
//  nTemp, nSpeed: Integer;
//begin
//  nSpeed := (Sender as TTimer).Tag;
//  nTemp := AlphaBlendValue + nSpeed;
//  if nTemp > 255 then
//    nTemp := 255
//  else if nTemp <= 0 then
//    nTemp := 0;
//    
//  AlphaBlendValue := nTemp;
//  if (AlphaBlendValue = 255) or (AlphaBlendValue = 0) then
//  begin
//    AlphaBlend := False;
//    FreeAndNil( FAlphaBlendTimer );
//  end;
//end;
//
//procedure TxdForm.DoTimerToAnimalForm(Sender: TObject);
//var
//  nTemp: Integer;
//begin
//  nTemp := AlphaBlendValue + FCurSpeed;
//  if nTemp > 255 then
//    nTemp := 255;
//  AlphaBlendValue := nTemp;
//
//  nTemp := 3;
//
//  FTempSize.Left := FTempSize.Left - nTemp;
//  FTempSize.Top := FTempSize.Top - nTemp;
//  FTempSize.Right := FTempSize.Right + nTemp;
//  FTempSize.Bottom := FTempSize.Bottom + nTemp;
//  SetWindowPos( Handle, HWND_TOP, FTempSize.Left, FTempSize.Top,
//    FTempSize.Right - FTempSize.Left, FTempSize.Bottom - FTempSize.Top, SWP_SHOWWINDOW );
//
//  if AlphaBlendValue = 255 then
//  begin
//    AlphaBlend := False;
//    FreeAndNil( FAnimalTimer );
//
////    SetWindowPos( Handle, HWND_TOP, FAnimalSize.Left, FAnimalSize.Top,
////      FAnimalSize.Right - FAnimalSize.Left, FAnimalSize.Bottom - FAnimalSize.Top, SWP_SHOWWINDOW );
//  end;
//end;

procedure TxdForm.Resize;
begin
  inherited;
  CheckFormRect;
end;

procedure TxdForm.SetRoundRectRgn(const Value: PRoundRectRgn);
begin
  if Assigned(Value) then
  begin
    if not Assigned(FRoundRectRgn) then
      New( FRoundRectRgn );
    FRoundRectRgn^.FLeftRect := Value^.FLeftRect;
    FRoundRectRgn^.FTopRect := Value^.FTopRect;
    FRoundRectRgn^.FRightRect := Value^.FRightRect;
    FRoundRectRgn^.FBottomRect := Value^.FBottomRect;
    FRoundRectRgn^.FHeightEllipse := Value^.FHeightEllipse;
    FRoundRectRgn^.FWidthEllipse := Value^.FWidthEllipse;

    SetWindowLong(handle, GWL_STYLE, GetWindowLong(handle, GWL_STYLE) and not WS_DLGFRAME );
  end
  else
  begin
    if Assigned(FRoundRectRgn) then
    begin
      Dispose( FRoundRectRgn );
      FRoundRectRgn := nil;
      SetWindowRgn(Handle, 0, True);
    end;
  end;
  CheckFormRect;
end;

end.
