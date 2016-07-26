unit uJxdGpForm;

interface

uses
  Windows, Messages, Classes, Controls, Forms, ExtCtrls, SysUtils, uJxdGpStyle, uJxdGPSub, GDIPAPI, GDIPOBJ;

type
  TTieStyle = (tsNULL, tsLeft, tsTop, tsRight, tsBottom);
  TxdForm = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;  
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure DoShow; override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {窗口过程}
    procedure WndProc(var msg: TMessage); override;
  private
    FAnimateTimer: TTimer;
    FAnimateClose: Boolean;
    FAnimateSpeed, FAnimateVale: Integer;
    FAnimateAlphaValueSpeed: Integer;
    FIsCreateRgnOK: Boolean; 
    procedure CreateFormStyle(const ABmp: TGPBitmap);
    procedure CheckSize;
    function  CheckToAnimateHide: Boolean;    
    procedure DoTimerToAnimateShow(Sender: TObject); 
    procedure DoTimerToAnimateHide(Sender: TObject);    

    {绘制使用方法}
    procedure DoDrawObjectChagned(Sender: TObject);
    function  DoGetDrawState(const Ap: PDrawInfo): TxdGpUIState; 
    function  DoIsDrawSubItem(const Ap: PDrawInfo): Boolean;
    procedure DoDrawSubItemText(const AGh: TGPGraphics; const AText: string; const AR: TGPRectF; const AItemState: TxdGpUIState);
  private
    {自动依靠信息}
    FCurTieStyle: TTieStyle;
    FCurTieWnd: TxdForm;
    FWinRect: TRect;
    FTieSubWndList: TList;
    
    procedure AddTieWnd(AForm: TxdForm);
    procedure DelTieWnd(AForm: TxdForm);
    procedure CheckCurTieWnd;
    function  CheckTieStyle(AForm: TxdForm): TTieStyle;
    procedure DoMoveSubWndPos(const AOldR, ACurR: TRect);
    procedure DoHandleWM_Moving(var pCurRect: PRect);
  private
    {自动生成信息}
    FImageInfo: TImageInfo;
    FImgDrawMethod: TDrawMethod;
    FCreateFormStyleByImage: Boolean;
    FAutoSizeByImage: Boolean;
    FAnimate: Boolean;
    FAutoTie: Boolean;
    FAutoTieSpace: Integer;
    FMoveForm: Boolean;
    procedure SetImageInfo(const Value: TImageInfo);
    procedure SetImgDrawMethod(const Value: TDrawMethod);
    procedure SetCreateFormStyleByImage(const Value: Boolean);
    procedure SetAutoSizeByImage(const Value: Boolean);
    procedure SetAnimate(const Value: Boolean);
    procedure SetAutoTie(const Value: Boolean);
    procedure SetAutoTieSpace(const Value: Integer);
  published
    //自动移动窗口
    property AutoMoveForm: Boolean read FMoveForm write FMoveForm default True;
    {自动停靠}
    property AutoTie: Boolean read FAutoTie write SetAutoTie;
    property AutoTieSpace: Integer read FAutoTieSpace write SetAutoTieSpace;
    {显示或隐藏动态效果}
    property Animate: Boolean read FAnimate write SetAnimate; //窗口显示或隐藏时，是否以动画的效果来表现
    {窗口背景与形状}
    property AutoSizeByImage: Boolean read FAutoSizeByImage write SetAutoSizeByImage; //使用图像的大小来设置窗口的大小
    property CreateFormStyleByImage: Boolean read FCreateFormStyleByImage write SetCreateFormStyleByImage; //根据图像的透明来设置窗口的样式
    property ImageInfo: TImageInfo read FImageInfo write SetImageInfo; //图像信息
    property ImageDrawMethod: TDrawMethod read FImgDrawMethod write SetImgDrawMethod; //绘制方式
  end;  

implementation

const
  CtAnimateSpaceTime = 20; //动画窗口间隔时间
  CtAnimateExcuteCount = 5; //动画窗口次数

var
  _TieFormManage: TList = nil;

procedure AddTieWndToManage(f: TxdForm);
var
  i: Integer;
  bAdd: Boolean;
begin
  if not Assigned(_TieFormManage) then
  begin
    _TieFormManage := TList.Create;
    _TieFormManage.Add( f );
  end
  else
  begin
    bAdd := True;
    for i := 0 to _TieFormManage.Count - 1 do
    begin
      if _TieFormManage[i] = f then
      begin
        bAdd := False;
        Break;
      end;
    end;
    if bAdd then
      _TieFormManage.Add( f );
  end;
end;

{ TxdForm }

procedure TxdForm.AddTieWnd(AForm: TxdForm);
var
  i: Integer;
  bAdd: Boolean;
begin
  bAdd := True;
  for i := 0 to FTieSubWndList.Count - 1 do
  begin
    if FTieSubWndList[i] = AForm then
    begin
      bAdd := False;
      Break;
    end;
  end;
  if bAdd then
    FTieSubWndList.Add( AForm );
end;

procedure TxdForm.CheckCurTieWnd;
  function IsSubTieForm(AForm: TxdForm): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to FTieSubWndList.Count - 1 do
    begin
      if FTieSubWndList[i] = AForm then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
var
  i: Integer;
  f: TxdForm;
begin
  if Assigned(FCurTieWnd) and IsWindowVisible(FCurTieWnd.Handle) then
    FCurTieStyle := CheckTieStyle( FCurTieWnd )
  else
    FCurTieStyle := tsNULL;

  if FCurTieStyle = tsNULL then
  begin
    for i := 0 to _TieFormManage.Count - 1 do
    begin
      f := _TieFormManage[i];
      if IsWindowVisible(f.Handle) and (f <> Self) and (f <> FCurTieWnd) and not IsSubTieForm(f) then
      begin
        FCurTieStyle := CheckTieStyle( f );
        if FCurTieStyle <> tsNULL then
        begin
          FCurTieWnd := f;
          FCurTieWnd.AddTieWnd( Self );
          Break;
        end;
      end;
    end;
  end;
  
  if FCurTieStyle = tsNULL then
  begin
    if Assigned(FCurTieWnd) then
    begin
      FCurTieWnd.DelTieWnd( Self );
      FCurTieWnd := nil;
    end;
  end
  else
  begin
    for i := 0 to FTieSubWndList.Count - 1 do
    begin
      if FTieSubWndList[i] = FCurTieWnd then
      begin
        FTieSubWndList.Delete( i );
        Break;
      end;
    end;
  end;
end;

procedure TxdForm.CheckSize;
begin
  if FCreateFormStyleByImage and Assigned(FImageInfo.Image) and AutoSizeByImage then
    SetWindowPos( Handle, 0, 0, 0, FImageInfo.Image.GetWidth, FImageInfo.Image.GetHeight, SWP_NOMOVE );
end;

function TxdForm.CheckTieStyle(AForm: TxdForm): TTieStyle;
  function CheckCombin(A1, B1, A2, B2: Integer): Boolean;
  begin
    Result := (((A1 >= A2) and (A1 <= B2)) or ((B1 >= A2) and (B1 <= B2))) or
              (((A2 >= A1) and (A2 <= B1)) or ((B2 >= A1) and (B2 <= B1)));
  end;
var
  R1, R2: TRect;
begin
  {
    A  B
    R1 R2
    口 口
    
    以B为参照点，tsLeft 表示 A在B的左边，差值在范围之内； tsTop 表示 A在B的上边，差值在范围之内；
  }
  R1 := FWinRect;
  R2 := AForm.FWinRect;
  
  Result := tsNULL;
  if CheckCombin(R1.Top, R1.Bottom, R2.Top, R2.Bottom) then
  begin
    if Abs(R1.Right - R2.Left) <= AutoTieSpace then
      Result := tsLeft
    else if Abs(R1.Left - R2.Right) <= AutoTieSpace then
      Result := tsRight
    else if Abs(R1.Top - R2.Bottom) <= AutoTieSpace then
      Result := tsBottom
    else if Abs(R1.Bottom - R2.Top) <= AutoTieSpace then
      Result := tsTop
  end
  else if CheckCombin(R1.Left, R1.Right, R2.Left, R2.Right) then
  begin
    if Abs(R1.Top - R2.Bottom) <= AutoTieSpace then
      Result := tsBottom
    else if Abs(R1.Bottom - R2.Top) <= AutoTieSpace then
      Result := tsTop
    else if Abs(R1.Right - R2.Left) <= AutoTieSpace then
      Result := tsLeft
    else if Abs(R1.Left - R2.Right) <= AutoTieSpace then
      Result := tsRight
  end;
        
end;

function TxdForm.CheckToAnimateHide: Boolean;
begin
  Result := False;
  if Animate and not FAnimateClose and not Assigned(FAnimateTimer) then
  begin    
    Result := True;
    FAnimateClose := True;
    
    FAnimateTimer := TTimer.Create( Self );
    FAnimateTimer.Interval := CtAnimateSpaceTime;
    FAnimateTimer.OnTimer := DoTimerToAnimateHide;
    
    FAnimateSpeed := 6;    
    FAnimateVale := -FAnimateSpeed * CtAnimateExcuteCount;

    FAnimateAlphaValueSpeed := -20;
    AlphaBlendValue := 255;
    AlphaBlend := True;    
    
    FAnimateTimer.Enabled := True;
  end;  
end;

constructor TxdForm.Create(AOwner: TComponent);
begin
  FAutoTie := False;
  FIsCreateRgnOK := False;
  FAutoSizeByImage := False;
  FAnimate := False;
  FAnimateTimer := nil;
  FTieSubWndList := nil;
  FCurTieWnd := nil;
  FCurTieStyle := tsNULL;
  FAnimateClose := False;
  FAutoTieSpace := 5;
  FMoveForm := True;
  
  FImageInfo := TImageInfo.Create;
  FImgDrawMethod := TDrawMethod.Create;

  FImageInfo.OnChange := DoDrawObjectChagned;
  FImgDrawMethod.OnChange := DoDrawObjectChagned;
  inherited;
end;

procedure TxdForm.CreateFormStyle(const ABmp: TGPBitmap);
var
  h, tempH: HRGN;  
  bmpData: TBitmapData;
  P: PCardinal;
  x, y: Integer;
type
  TArgbInfo = packed record
    FBlue, 
    FGreen, 
    FRed, 
    FAlpha: Byte;
  end;
begin
  if not Assigned(ABmp) or FIsCreateRgnOK then Exit;
  
  FIsCreateRgnOK := True;
  h := CreateRectRgn(0, 0, Width, Height );
  ABmp.LockBits( MakeRect(0, 0, Width, Height), ImageLockModeRead, PixelFormat32bppARGB, bmpData);
  try
    P := bmpData.Scan0;
    for y := 0 to bmpData.Height - 1 do
    begin
      for x := 0 to bmpData.Width - 1 do
      begin
        with TArgbInfo(P^) do
        begin
          if FAlpha = 0 then
          begin
            tempH := CreateRectRgn(x, y, x+1, y+1);
            CombineRgn(h, h, tempH,  RGN_XOR);
            DeleteObject( tempH );
          end;
        end;
        Inc(P);
      end;
    end;
  finally
    ABmp.UnlockBits( bmpData );
  end;

  SetWindowRgn( Handle, h, True );
  DeleteObject( h );
end;

procedure TxdForm.DelTieWnd(AForm: TxdForm);
var
  i: Integer;
begin
  for i := 0 to FTieSubWndList.Count - 1 do
  begin
    if FTieSubWndList[i] = AForm then
    begin
      FTieSubWndList.Delete( i );
      Break;
    end;
  end;
end;

destructor TxdForm.Destroy;
begin
  inherited;
  FreeAndNil( FImageInfo );
  FreeAndNil( FImgDrawMethod );
  FreeAndNil( FAnimateTimer );
  FreeAndNil( FTieSubWndList );
end;

procedure TxdForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  if (Action = caHide) and CheckToAnimateHide then
    Action := caNone;
end;

procedure TxdForm.DoDrawObjectChagned(Sender: TObject);
begin
  CheckSize;
  Invalidate;
end;
                
procedure TxdForm.DoDrawSubItemText(const AGh: TGPGraphics; const AText: string; const AR: TGPRectF;
  const AItemState: TxdGpUIState);
begin

end;

function TxdForm.DoGetDrawState(const Ap: PDrawInfo): TxdGpUIState;
begin
  Result := uiNormal;
end;

procedure TxdForm.DoHandleWM_Moving(var pCurRect: PRect);
var
  nW, nH: Integer;
  old: TRect;
begin
  old := FWinRect;
  FWinRect := pCurRect^;
  CheckCurTieWnd;
  
  if Assigned(FCurTieWnd) then
  begin    
    case FCurTieStyle of
      tsLeft:   
      begin
        nW := pCurRect^.Right - pCurRect^.Left;
        pCurRect^.Left := FCurTieWnd.FWinRect.Left - nW;
        pCurRect^.Right := pCurRect^.Left + nW;
      end;
      tsTop:    
      begin
        nH := pCurRect^.Bottom - pCurRect^.Top;
        pCurRect^.Top := FCurTieWnd.FWinRect.Top - nH;
        pCurRect^.Bottom := pCurRect^.Top + nH;
      end;
      tsRight:  
      begin
        nW := pCurRect^.Right - pCurRect^.Left;
        pCurRect^.Left := FCurTieWnd.FWinRect.Right;
        pCurRect^.Right := pCurRect^.Left + nW;
      end;
      tsBottom: 
      begin
        nH := pCurRect^.Bottom - pCurRect^.Top;
        pCurRect^.Top := FCurTieWnd.FWinRect.Bottom;
        pCurRect^.Bottom := pCurRect^.Top + nH;
      end;
    end;
  end;
  DoMoveSubWndPos( old, pCurRect^ );  
  FWinRect := pCurRect^;
end;

function TxdForm.DoIsDrawSubItem(const Ap: PDrawInfo): Boolean;
begin
  Result := True;
end;

procedure TxdForm.DoMoveSubWndPos(const AOldR, ACurR: TRect);
var
  i: Integer;
  X, Y: Integer;
  f: TxdForm;
begin
  if Assigned(FTieSubWndList) then
  begin
    X := ACurR.Left - AOldR.Left;
    Y := ACurR.Top - AOldR.Top;
    for i := 0 to FTieSubWndList.Count - 1 do
    begin
      f := FTieSubWndList[i];
      SetWindowPos( f.Handle, 0, f.FWinRect.Left + X, f.FWinRect.Top + Y, 0, 0, SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE );
    end;
  end;
end;

procedure TxdForm.DoShow;
begin        
  if Animate and not Assigned(FAnimateTimer) then
  begin
    FAnimateTimer := TTimer.Create( Self );
    FAnimateTimer.Interval := CtAnimateSpaceTime;
    FAnimateTimer.OnTimer := DoTimerToAnimateShow;
    
    FAnimateSpeed := 6;    
    FAnimateVale := FAnimateSpeed * CtAnimateExcuteCount;

    FAnimateAlphaValueSpeed := 20;
    AlphaBlend := True;
    AlphaBlendValue := 10;
    
    FAnimateTimer.Enabled := True;
  end; 
  inherited DoShow;
  FAnimateClose := False;
end;

procedure TxdForm.DoTimerToAnimateHide(Sender: TObject);
var
  rgn: HRGN;
begin
  if FAnimateVale <= 0 then
  begin
    rgn := CreateRectRgn( -FAnimateVale, -FAnimateVale, Width + FAnimateVale, Height + FAnimateVale );
    SetWindowRgn( Handle, rgn, False );
    DeleteObject( rgn );
    
    if FAnimateAlphaValueSpeed + AlphaBlendValue <= 255 then    
      AlphaBlendValue := FAnimateAlphaValueSpeed + AlphaBlendValue;
    Inc(FAnimateVale, FAnimateSpeed);
  end
  else
  begin   
    FAnimateClose := True; 
    Close;    
    FreeAndNil( FAnimateTimer );
  end;
end;

procedure TxdForm.DoTimerToAnimateShow(Sender: TObject);
var
  rgn: HRGN;
begin
  if FAnimateVale >= 0 then
  begin
    rgn := CreateRectRgn( FAnimateVale, FAnimateVale, Width - FAnimateVale, Height - FAnimateVale );
    SetWindowRgn( Handle, rgn, False );
    DeleteObject( rgn );
    
    if FAnimateAlphaValueSpeed + AlphaBlendValue <= 255 then    
      AlphaBlendValue := FAnimateAlphaValueSpeed + AlphaBlendValue;
    Dec(FAnimateVale, FAnimateSpeed);
  end
  else
  begin
    AlphaBlendValue := 255;
    AlphaBlend := False;
    FreeAndNil( FAnimateTimer );
  end;
end;

procedure TxdForm.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  
  if AutoMoveForm then
  begin
    ReleaseCapture;
    PostMessage(Handle, WM_SYSCOMMAND, 61458, 0);
  end;
end;

procedure TxdForm.Paint;
var
  G, bmpG: TGPGraphics;
  bmp: TGPBitmap;
begin
  if Assigned(FImageInfo.Image) then
  begin    
    G := TGPGraphics.Create( Canvas.Handle );
    bmp := TGPBitmap.Create( Width, Height );
    bmpG := TGPGraphics.Create( bmp );
    try
      DrawImageCommon( bmpG, MakeRect(0, 0, Width, Height), FImageInfo, FImgDrawMethod, 
        DoGetDrawState, DoIsDrawSubItem, DoDrawSubItemText );
      
      if CreateFormStyleByImage then
        CreateFormStyle(bmp);
        
      G.DrawImage( bmp, 0, 0, Width, Height );
    finally
      G.Free;
      bmp.Free;
      bmpG.Free;
    end;    
  end
  else
    inherited;  
end;

procedure TxdForm.Resize;
begin  
  FIsCreateRgnOK := False;
  inherited;    
  Invalidate;
  GetWindowRect( Handle, FWinRect );
end;

procedure TxdForm.SetAnimate(const Value: Boolean);
begin
  FAnimate := Value;
end;

procedure TxdForm.SetAutoSizeByImage(const Value: Boolean);
begin
  if FAutoSizeByImage <> Value then
  begin
    FAutoSizeByImage := Value;
    if FAutoSizeByImage then
      CheckSize;
  end;
end;

procedure TxdForm.SetAutoTie(const Value: Boolean);
begin
  if FAutoTie <> Value then
  begin
    FAutoTie := Value;
    if FAutoTie then
    begin
      if not Assigned(FTieSubWndList) then
        FTieSubWndList := TList.Create;
      AddTieWndToManage( Self );
      GetWindowRect( Handle, FWinRect );
      CheckCurTieWnd;
    end;
  end;
end;

procedure TxdForm.SetAutoTieSpace(const Value: Integer);
begin
  FAutoTieSpace := Value;
end;

procedure TxdForm.SetCreateFormStyleByImage(const Value: Boolean);
begin
  if FCreateFormStyleByImage <> Value then
  begin
    FCreateFormStyleByImage := Value;
    FIsCreateRgnOK := False;
    CheckSize;    
    Invalidate;
  end;
end;

procedure TxdForm.SetImageInfo(const Value: TImageInfo);
begin
  FImageInfo.Assign( Value );
end;

procedure TxdForm.SetImgDrawMethod(const Value: TDrawMethod);
begin
  FImgDrawMethod.Assign( Value );
end;

procedure TxdForm.WndProc(var msg: TMessage);
var
  R: TRect;
begin
  inherited;  
  case msg.Msg of
    WM_MOVING: 
    begin
      if Assigned(_TieFormManage) then      
        DoHandleWM_Moving( PRect(msg.LParam) );
    end;
    WM_MOVE:       
    begin                             
      if Assigned(_TieFormManage) then
      begin
        GetWindowRect( Handle, R );
        DoMoveSubWndPos( FWinRect, R );
        FWinRect := R;
      end;
    end;
  end;
end;

end.
