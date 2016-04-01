unit VistaSysButton;

interface

uses Windows, Messages, Classes, Controls, Forms, Graphics, StdCtrls,
  ExtCtrls, CommCtrl, Buttons, CustomGCButton;

type
  TSysButtonType = (sbtNone, sbtMenu, sbtMin, sbtMax, sbtClose);
  TSysButtonClickEvent=procedure(Sender:TObject; var Done:Boolean; ButtonType:TSysButtonType) of object;
  TVistaSysButton = class(TCustomGCButton)
  private
    FForm: TForm;
    FCurrentButton: TSysButtonType;
    FOnSysButtonClick: TSysButtonClickEvent;
    function GetButtonType(X, Y: Integer): TSysButtonType;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoSysButtonClick(var Done:Boolean; ButtonType:TSysButtonType);
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
  published
    property Form: TForm read FForm write FForm;
    property OnSysButtonClick:TSysButtonClickEvent read FOnSysButtonClick write FOnSysButtonClick;
  end;

implementation

{$R SysButton.RES}

{ TVistaSysButton }

procedure TVistaSysButton.Click;
var
  CmdType: Word;
  Done:Boolean;
begin
  if (FForm = nil) or (FCurrentButton = sbtNone) then
    Exit
  else begin
    Done:=True;
    DoSysButtonClick(Done,FCurrentButton);
    if not Done then
      Exit;
    case FCurrentButton of
      sbtMin: CmdType := SC_MINIMIZE;
      sbtMax:
        if IsZoomed(FForm.Handle) then
          CmdType := SC_RESTORE
        else
          CmdType := SC_MAXIMIZE;
      else
       CmdType := SC_CLOSE;
    end;
    PostMessage(FForm.Handle, WM_SYSCOMMAND, CmdType, 0);
  end;
end;

constructor TVistaSysButton.Create(AOwner: TComponent);
var
  Control: TWinControl;
begin
  inherited;
  Width := 149;
  Height := 21;
  Control := TWinControl(AOwner);
  while (Control <> nil) and not (Control is TForm) do
    Control := Control.Parent;
  if (Control <> nil) and (Control is TForm) then
    FForm := TForm(Control);
end;

procedure TVistaSysButton.DoSysButtonClick(var Done: Boolean;
  ButtonType: TSysButtonType);
begin
  if Assigned(FOnSysButtonClick) then
    FOnSysButtonClick(Self,Done,ButtonType);  
end;

function TVistaSysButton.GetButtonType(X, Y: Integer): TSysButtonType;
begin
  Result := sbtNone;
  if (X >= 3) and (X < 146) and (Y < 17) then
  begin
    if X < 52 then
      Result := sbtMenu
    else if X < 77 then
      Result := sbtMin
    else if X < 102 then
      Result := sbtMax
    else
      Result := sbtClose;
  end;
end;

procedure TVistaSysButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FCurrentButton := GetButtonType(X, Y);
  inherited;
end;

procedure TVistaSysButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  OldButton: TSysButtonType;
begin
  OldButton := FCurrentButton;
  FCurrentButton := GetButtonType(X, Y);
  if OldButton <> FCurrentButton then
    Invalidate;
  inherited;
end;

procedure TVistaSysButton.Paint;
var
  ResBmp: TBitmap;
  Index: Integer;
  R: TRect;
begin
  Index := 0;
  ResBmp := TBitmap.Create;
  ResBmp.LoadFromResourceName(HInstance, 'SysButton');
  if (FCurrentButton = sbtNone) or not MouseInControl then
    Index := 0
  else begin
    if FState = bsDown then
    begin
      case FCurrentButton of
        sbtMenu: Index := 2;
        sbtMin: Index := 4;
        sbtMax: Index := 6;
        sbtClose: Index := 8;
      end;
    end
    else begin
      case FCurrentButton of
        sbtMenu: Index := 1;
        sbtMin: Index := 3;
        sbtMax: Index := 5;
        sbtClose: Index := 7;
      end;
    end;
  end;
  R := Rect(0, Index * 21, ResBmp.Width, (Index + 1) * 21);
  Canvas.CopyRect(ClientRect, ResBmp.Canvas, R);
  ResBmp.Free;
end;

end.

