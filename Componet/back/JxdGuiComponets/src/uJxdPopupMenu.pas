unit uJxdPopupMenu;

interface

uses
  Windows, Classes, Menus, Graphics, ExtCtrls, Controls, Messages, SysUtils, uJxdDrawSub, Forms, uJxdGuiStyle,
  uJxdParseGradient;

type
  PMenuItemInfo = ^TMenuItemInfo;
  TMenuItemInfo = record
  private
    FID: Integer;
    FCaption: string;
    FRect: TRect;
    FEnable: Boolean;
    FCheck: Boolean;
    FIsLineCaption: Boolean;
  public
    property ID: Integer read FID;
    property Caption: string read FCaption;
    property Enable: Boolean read FEnable write FEnable;
    property Check: Boolean read FCheck write FCheck;
  end;

  TOnMenuItemInfo = procedure(const Ap: PMenuItemInfo) of object;
  TxdPopupMenu = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  AddItem(const ACaption: string): Integer;
    procedure Popup(X, Y: Integer); overload;
    procedure Popup; overload;
  protected
    FCurPopupMenu: TCustomControl;
    FOldPopupWnd: TWndMethod;
    procedure CheckSize;
    procedure DoPopupWndProc(var Message: TMessage);
    procedure DoPopupWindowPaint(Sender: TObject);
    procedure DoClick(Sender: TObject);
    procedure HandleMouseMove(var Msg: TWMMouseMove);
  private
    FCurSelectIndex: Integer;
    FCurRect: TRect;
    FItems: TList;
    FBackGradient: TGradientDrawInfo;
    FBackLines: TLineDrawInfo;
    FLeftSpace: Integer;
    FMargins: TMargins;
    FFontNormal: TFont;
    FFontSelected: TFont;
    FBackBitmap: TBitmapInfo;
    function  IsCurCaptionLine: Boolean;
    procedure SetBackGradient(const Value: TGradientDrawInfo);
    procedure OnDrawInfoChanged(Sender: TObject);
    procedure SetBackLines(const Value: TLineDrawInfo);
    procedure SetLeftSpace(const Value: Integer);
    procedure SetFontSelected(const Value: TFont);
    procedure SetNormalFont(const Value: TFont);
    procedure SetBackBitmap(const Value: TBitmapInfo);
  published
    property BackBitmap: TBitmapInfo read FBackBitmap write SetBackBitmap;
    property BackGradient: TGradientDrawInfo read FBackGradient write SetBackGradient;
    property BackLines: TLineDrawInfo read FBackLines write SetBackLines;
    property LeftSpace: Integer read FLeftSpace write SetLeftSpace;
    property Margins: TMargins read FMargins write FMargins;
    property FontNormal: TFont read FFontNormal write SetNormalFont;
    property FontSelected: TFont read FFontSelected write SetFontSelected;
  end;

implementation

{ TxdPopupMenu }
const
  CtLineCaption = '-';
var
  _ID: Integer = 0;

function TxdPopupMenu.AddItem(const ACaption: string): Integer;
var
  p: PMenuItemInfo;
begin
  New( p );
  p^.FID := _ID;
  Inc( _ID );
  p^.FCaption := ACaption;
  p^.FEnable := True;
  p^.FCheck := False;
  p^.FIsLineCaption := ACaption = CtLineCaption;
  FItems.Add( p );
  Result := p^.FID;
end;

procedure TxdPopupMenu.CheckSize;
var
  i: Integer;
  p: PMenuItemInfo;
  nTop, nCapW, nCapH: Integer;
  nMaxWidth: Integer;
  sNormal, sSelected: TSize;
  Cvs: TCanvas;
  dc: HDC;
const
  LineCaptionHeight = 2;
begin
  nMaxWidth := Margins.Left + Margins.Right;
  nCapW := nMaxWidth;
  nCapH := Margins.Top + Margins.Bottom;

  dc := GetDC( FCurPopupMenu.Handle );
  Cvs := TCanvas.Create;
  Cvs.Handle := dc;
  for i := 0 to FItems.Count - 1 do
  begin
    p := FItems[i];
    if not p^.FIsLineCaption then
    begin
      Cvs.Font := FFontNormal;
      sNormal := Cvs.TextExtent( p^.FCaption );
      Cvs.Font := FFontSelected;
      sSelected := Cvs.TextExtent( p^.FCaption );

      if sNormal.cx > nCapW then
        nCapW := sNormal.cx;
      if sSelected.cx > nCapW then
        nCapW := sSelected.cx;
      if sNormal.cy > nCapH then
        nCapH := sNormal.cy;
      if sSelected.cy > nCapH then
        nCapH := sSelected.cy;
    end
  end;
  nMaxWidth := Margins.Left + LeftSpace + nCapW + Margins.Right;

  nTop := Margins.Top;
  for i := 0 to FItems.Count - 1 do
  begin
    p := FItems[i];
    p^.FRect.Left := Margins.Left;
    p^.FRect.Top := nTop;
    p^.FRect.Right := p^.FRect.Left + nCapW + LeftSpace;
    if not p^.FIsLineCaption then
    begin
      p^.FRect.Bottom := p^.FRect.Top + nCapH;
      Inc( nTop, nCapH + Margins.Bottom );
      p^.FRect.Bottom := nTop;
    end
    else
    begin
      p^.FRect.Bottom := nTop + LineCaptionHeight;
      Inc( nTop, LineCaptionHeight + Margins.Bottom );
      p^.FRect.Bottom := nTop;
    end;
  end;
  FCurPopupMenu.Width := nMaxWidth;
  FCurPopupMenu.Height := nTop + Margins.Bottom;
end;

constructor TxdPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TList.Create;
  FCurPopupMenu := nil;
  FBackGradient := TGradientDrawInfo.Create;
  FBackGradient.GradientWay := gwLeftToRigth;
  FBackGradient.OnChange := OnDrawInfoChanged;
  FBackLines := TLineDrawInfo.Create;
  FBackLines.OnChange := OnDrawInfoChanged;
  FMargins := TMargins.Create( nil );
  FFontNormal := TFont.Create;
  FBackBitmap := TBitmapInfo.Create;
  FBackBitmap.OnChange := OnDrawInfoChanged;

  with FFontNormal do
  begin
    Color := 0;//RGB(255, 0, 255);
    Size := 12;
    Name := '宋体';
  end;
  FFontSelected := TFont.Create;
  with FFontSelected do
  begin
    Color := $FFFFFF;
    Size := 12;
    Name := '宋体';
  end;
  BackLines.Count := 1;
  BackLines.Visible := True;
  LeftSpace := 30;
  Margins.Left := 3;
  Margins.Right := 3;
  Margins.Top := 3;
  Margins.Bottom := 3;
end;

destructor TxdPopupMenu.Destroy;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    Dispose( FItems[i] );
  FItems.Free;
  FreeAndNil( FCurPopupMenu );
  FreeAndNil( FBackGradient );
  FreeAndNil( FBackLines );
  FreeAndNil( FMargins );
  FreeAndNil( FFontNormal );
  FreeAndNil( FFontSelected );
  FreeAndNil( FBackBitmap );
  inherited;
end;

procedure TxdPopupMenu.OnDrawInfoChanged(Sender: TObject);
begin
  if Assigned(FCurPopupMenu) then
    FCurPopupMenu.Invalidate;
end;

procedure TxdPopupMenu.Popup;
var
  pt: TPoint;
begin
  GetCursorPos( pt );
  Popup( pt.X, pt.Y );
end;

procedure TxdPopupMenu.DoPopupWndProc(var Message: TMessage);
const
  CtFreeMessage = WM_USER + 1234;
  procedure FreePopupMenu;  
  begin
    if Assigned(FCurPopupMenu) then
      PostMessage( FCurPopupMenu.Handle, CtFreeMessage, 0, 0);
  end;
begin
  FOldPopupWnd( Message );
  case Message.Msg of
    CtFreeMessage: FreeAndNil( FCurPopupMenu );
    WM_ACTIVATE:
      with TWMActivate(Message) do
      begin
        if Active = WA_INACTIVE then
          FreePopupMenu;
      end;
    WM_KEYDOWN:
      with TWMKeyDown(Message) do
        if CharCode = 27 then
          FreePopupMenu;
    WM_LBUTTONUP, WM_RBUTTONDOWN:
      if not IsCurCaptionLine then FreePopupMenu;
    WM_MOUSEMOVE: HandleMouseMove( TWMMouseMove(message) );      
  end;
end;

procedure TxdPopupMenu.HandleMouseMove(var Msg: TWMMouseMove);
var
  i: Integer;
  p: PMenuItemInfo;
  pt: TPoint;
begin
  pt := Point( Msg.XPos, Msg.YPos );
  if FCurSelectIndex <> -1 then
  begin
    if PtInRect( FCurRect, pt) then Exit;
    InvalidateRect( FCurPopupMenu.Handle, @FCurRect, False );
  end;

  for i := 0 to FItems.Count - 1 do
  begin
    p := FItems[i];
    if PtInRect(p^.FRect, pt) then
    begin
      FCurSelectIndex := i;
      FCurRect := p^.FRect;
      InvalidateRect( FCurPopupMenu.Handle, @FCurRect, False );
      Break;
    end;
  end;
end;

function TxdPopupMenu.IsCurCaptionLine: Boolean;
begin
  Result := (FCurSelectIndex >= 0) and (FCurSelectIndex <FItems.Count - 1) and
            PMenuItemInfo(FItems[FCurSelectIndex])^.FIsLineCaption;
end;

procedure TxdPopupMenu.DoClick(Sender: TObject);
begin
  if not IsCurCaptionLine then
  begin
    OutputDebugString( PChar('点击项: ' + IntToStr(FCurSelectIndex)) );
  end;
end;

procedure TxdPopupMenu.DoPopupWindowPaint(Sender: TObject);
var
  Form: TForm;
  i, h: Integer;
  p: PMenuItemInfo;
  R: TRect;
  cr: TColor;
begin
  Form := Sender as TForm;
  with Form do
  begin
    DrawGradientInfo( Canvas, BackGradient.ParseGradientNormal, BackGradient.GradientWay = gwLeftToRigth, 0, 0, Width, Height );
    DrawLinesInfo( Canvas, BackLines, Width, Height );
    for i := 0 to FItems.Count - 1 do
    begin
      p := FItems[i];
      if p^.FIsLineCaption then
      begin
        h := p^.FRect.Top + (p^.FRect.Bottom - p^.FRect.Top) div 2;
        cr := $CED3D6;
        Canvas.Pen.Color := cr;
        Canvas.Pen.Width := 1;
        Canvas.MoveTo( p^.FRect.Left + LeftSpace + Margins.Left, h );
        Canvas.LineTo( p^.FRect.Right - Margins.Right, h );
        Continue;
      end;

      if p^.FCaption <> '' then
      begin
        if i = FCurSelectIndex then
        begin
          R := p^.FRect;
          cr := $EFD3C6;
          Canvas.Brush.Color := cr;
          Canvas.FillRect( R );
          cr := $C66931;
          DrawFrameBorder( Canvas, cr, 1, R );
          Canvas.Font := FontSelected
        end
        else
          Canvas.Font := FontNormal;
        Canvas.Brush.Style := bsClear;
        R := p^.FRect;
        R.Left := R.Left + LeftSpace;
        DrawText(Canvas.Handle, PChar(p^.FCaption), Length(p^.FCaption), R, DT_VCENTER or DT_SINGLELINE);
      end;
    end;
  end;
end;

procedure TxdPopupMenu.Popup(X, Y: Integer);
var
  sx, sy: Integer;
  W, H: Integer;
begin
  if not Assigned(FCurPopupMenu) then
  begin
    FCurPopupMenu := TCustomControl.Create( nil );
    FCurPopupMenu.DoubleBuffered := True;
    FOldPopupWnd := FCurPopupMenu.WindowProc;
    FCurPopupMenu.WindowProc := DoPopupWndProc;
  end;

  with FCurPopupMenu do
  begin
//    OnPaint := DoPopupWindowPaint;
//    OnClick := DoClick;
//    BorderStyle := bsNone;
//    FormStyle := fsStayOnTop;
    ParentWindow := Application.Handle;
    CheckSize;
    sx := GetSystemMetrics( SM_CXSCREEN );
    sy := GetSystemMetrics( SM_CYSCREEN );
    W := FCurPopupMenu.Width;
    H := FCurPopupMenu.Height;

//    ActivateHint( Rect(x, y, x + Width, y + Height), 'ajdskfjsdlf' );

    if X + W > sx then
      Dec( X, W )
    else if X + W < 0 then
      X := 0;

    if Y + H > sy then
      Dec( Y, H );
    FCurSelectIndex := -1;
    FCurRect := Rect( 0, 0, 0, 0 );
//
    SetWindowPos(Handle, HWND_TOPMOST, x, y, Width, Height,
      SWP_NOACTIVATE);
    ShowWindow(Handle, SW_SHOWNOACTIVATE);
  end;
end;

procedure TxdPopupMenu.SetBackBitmap(const Value: TBitmapInfo);
begin
  FBackBitmap.Assign( Value );
end;

procedure TxdPopupMenu.SetBackGradient(const Value: TGradientDrawInfo);
begin
  FBackGradient.Assign( Value );
end;

procedure TxdPopupMenu.SetBackLines(const Value: TLineDrawInfo);
begin
  FBackLines.Assign( Value );
end;

procedure TxdPopupMenu.SetFontSelected(const Value: TFont);
begin
  FFontSelected.Assign( Value );
end;

procedure TxdPopupMenu.SetLeftSpace(const Value: Integer);
begin
  if FLeftSpace <> Value then
    FLeftSpace := Value;
end;

procedure TxdPopupMenu.SetNormalFont(const Value: TFont);
begin
  FFontNormal.Assign( Value );
end;

initialization
  _ID := Integer( GetTickCount );

end.
