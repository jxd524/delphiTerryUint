unit uJxdGpScrollBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, GDIPAPI, GDIPOBJ, uJxdGpBasic, uJxdGpStyle, uJxdGpCommon;

type
  TxdScrollBar = class(TxdGpCommon)  
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    procedure Resize; override;
    procedure DoObjectChanged(Sender: TObject); override;
    procedure DoControlStateChanged(const AOldState, ANewState: TxdGpUIState; const ACurPt: TPoint); override;
    procedure DoSubItemMouseDown(const Ap: PDrawInfo); override;
    procedure DoSubItemMouseUp(const Ap: PDrawInfo); override;

    procedure DoTimerToMovePos(Sender: TObject); 

    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TMessage); message WM_LBUTTONUP;
  private
    FBuildingDrawInfo: Boolean; //是否进行生成绘制信息
    FBufferBmp: TGPBitmap;   //缓存图像
    FSliderInfo: PDrawInfo; //滑块信息指针
    FSliderMaxRunSize: Integer; //可被滑块运动的大小
    FPrePosPiexl: Double; //每一个位置占用几个像素点
    FMouseDownPt: TPoint; 
    FChangeStyle: TxdChangedStyle;
    FTimeToMovePos: TTimer;
    
    procedure ReBuildDrawInfo;  //生成需要绘制信息
    procedure FindSliderItem;   //查找滑块信息
    procedure ReCalcSliderInfo; //计算滑块的位置和大小

    procedure DoNotifyPosChanged;
  private
    {属性自动生成}
    FScrollBarStyle: TxdScrollStyle;
    FMax: Integer;
    FMin: Integer;
    FPos: Integer;
    FSliderMax: Integer;
    FSliderMin: Integer;
    FOnPosChanged: TOnChangedNotify;
    procedure SetScrollBarStyle(const Value: TxdScrollStyle);
    procedure SetMax(const Value: Integer);
    procedure SetPos(const Value: Integer);
    procedure SetSliderMax(const Value: Integer);
  published
    property ScrollBarStyle: TxdScrollStyle read FScrollBarStyle write SetScrollBarStyle;
    property Min: Integer read FMin;
    property Max: Integer read FMax write SetMax;
    property SliderMin: Integer read FSliderMin; //滑块最小大小
    property SliderMax: Integer read FSliderMax write SetSliderMax; //滑块最大大小
    property Position: Integer read FPos write SetPos; //当前位置

    property OnPosChanged: TOnChangedNotify read FOnPosChanged write FOnPosChanged;
  end;  

implementation

{ TxdScrollBar }
const
  CtClickID_DecButton = 0; //左边或上边的按钮
  CtClickID_IncButton = 1; //右边或下边的按钮
  CtClickID_SliderButton = 2; //滑块按钮
  CtClickID_Other = 3; //其它地方
  CtLayoutBk = 0;
  CtLayoutButton = 1;
  CtMinPerSlipPiexl = 2;  //最小滚动像素点
  
constructor TxdScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  FMin := 0;
  FMax := 100;
  FPos := 50;
  FSliderMin := 20;
  FSliderMax := 80;
  FBufferBmp := nil;
  FSliderMaxRunSize := 0;
  FScrollBarStyle := ssVerical;
  ImageDrawMethod.DrawStyle := dsDrawByInfo;
  ImageInfo.ImageCount := 4;
  Caption := '';
  FChangeStyle := csNull;
  FBuildingDrawInfo := False;
end;

destructor TxdScrollBar.Destroy;
begin
  FreeAndNil( FBufferBmp );
  FreeAndNil( FTimeToMovePos );
  inherited;  
end;

procedure TxdScrollBar.DoControlStateChanged(const AOldState, ANewState: TxdGpUIState; const ACurPt: TPoint);
begin
  inherited;
  if ANewState = uiDown then
    FMouseDownPt := ACurPt
  else
    FMouseDownPt := Point(0, 0);
end;

procedure TxdScrollBar.DoNotifyPosChanged;
begin
  if Assigned(FOnPosChanged) then
    FOnPosChanged( Self, FChangeStyle );
  FChangeStyle := csNull;
end;

procedure TxdScrollBar.DoObjectChanged(Sender: TObject);
begin
  if not ((Sender is TDrawMethod) or (Sender is TImageInfo)) then 
  begin
    Inherited;
    Exit;
  end;
  if not FBuildingDrawInfo then  
  begin
    FBuildingDrawInfo := True;
    try
      ReBuildDrawInfo;
      ReCalcSliderInfo;
    finally
      FBuildingDrawInfo := False;
    end;
  end;
  inherited;

  if Assigned(OnPosChanged) then
    OnPosChanged( Self, csMouse );
end;

procedure TxdScrollBar.DoSubItemMouseDown(const Ap: PDrawInfo);
var
  nTag: Integer;
  pt: TPoint;
begin
  nTag := 0;
  case Ap^.FClickID of
    CtClickID_DecButton: nTag := -1;
    CtClickID_IncButton: nTag := 1;
    CtClickID_Other:
    begin
      if Assigned(FSliderInfo) then
      begin
        GetCursorPos(pt);
        pt := ScreenToClient(pt);
        if FScrollBarStyle = ssVerical then
        begin
          if pt.X < FSliderInfo^.FDestRect.X then
            nTag := -5
          else if (pt.X > FSliderInfo^.FDestRect.x + FSliderInfo^.FDestRect.Width) then
            nTag := 5;
        end
        else
        begin
          if pt.Y < FSliderInfo^.FDestRect.Y then
            nTag := -5
          else if (pt.Y > FSliderInfo^.FDestRect.Y + FSliderInfo^.FDestRect.Height) then
            nTag := 5;
        end;
      end;
    end;
  end;
  
  FreeAndNil( FTimeToMovePos );
  if nTag <> 0 then
  begin    
    FTimeToMovePos := TTimer.Create( Self );
    FTimeToMovePos.Interval := 100;
    FTimeToMovePos.Tag := nTag;
    FTimeToMovePos.OnTimer := DoTimerToMovePos;
    FTimeToMovePos.Enabled := True;
  end;
end;

procedure TxdScrollBar.DoSubItemMouseUp(const Ap: PDrawInfo);
begin
  inherited;  
end;

procedure TxdScrollBar.DoTimerToMovePos(Sender: TObject);
begin
  FChangeStyle := csMouse;
  Position := Position + FTimeToMovePos.Tag;
end;

procedure TxdScrollBar.FindSliderItem;
var
  i: Integer;
  p: PDrawInfo;
begin
  FSliderInfo := nil;
  for i := 0 to ImageDrawMethod.CurDrawInfoCount - 1 do
  begin
    p := ImageDrawMethod.GetDrawInfo( i );
    if Assigned(p) and (p^.FClickID = CtClickID_SliderButton) then
    begin
      FSliderInfo := p;
      Break;
    end;
  end;
end;

procedure TxdScrollBar.ReBuildDrawInfo;
var
  nBmpW, nH, nBmpH, nW, nBmpSpace: Integer;
  info: TDrawInfo;
  bmp: TGPBitmap;
  G: TGPGraphics;
  destPt: array[0..2] of TGPPoint;
begin                                  
  ImageDrawMethod.ClearDrawInfo;
  if not Assigned(ImageInfo.Image) then Exit;

  nBmpW := ImageInfo.Image.GetWidth;
  nBmpH := ImageInfo.Image.GetHeight;
  
  if FScrollBarStyle = ssVerical then
  begin
    //水平方式
    if nBmpH > nBmpW then
    begin
      //将垂直图片修改成水平方式
      bmp := TGPBitmap.Create( nBmpH, nBmpW, ImageInfo.Image.GetPixelFormat );
      G := TGPGraphics.Create( bmp );
      destPt[0] := MakePoint(0, Integer(bmp.GetHeight) );
      destPt[1] := MakePoint(0, 0);
      destPt[2] := MakePoint( Integer(bmp.GetWidth), Integer(bmp.GetHeight) );
      G.DrawImage( ImageInfo.Image, PGpPoint(@destPt), 3, 0, 0, nBmpW, nBmpH, UnitPixel );      
      FreeAndNil( FBufferBmp );
      G.Free;
      
      FBufferBmp := bmp;
      ImageInfo.OnChange := nil;
      ImageInfo.Image := FBufferBmp;
      ImageInfo.OnChange := DoObjectChanged;
    end;
    nBmpW := ImageInfo.Image.GetWidth div 3; //三种状态
    nBmpH := ImageInfo.Image.GetHeight;
    nW := nBmpW div 4; //四种图形
    nH := nBmpH;
    FSliderMaxRunSize := Width - nW * 2;
    FSliderMin := nW;
    nBmpSpace := nW * 3;
    
    //左边按钮
    info.FText := '';
    info.FClickID := CtClickID_DecButton;
    info.FLayoutIndex := CtLayoutButton;
    info.FDrawStyle := dsPaste;
    info.FDestRect := MakeRect(0, 0, nW, nH);
    info.FNormalSrcRect := MakeRect(0, 0, nW, nH);
    info.FActiveSrcRect := info.FNormalSrcRect;
    info.FActiveSrcRect.X := info.FActiveSrcRect.X + nBmpW;
    info.FDownSrcRect := info.FActiveSrcRect;
    info.FDownSrcRect.X := info.FDownSrcRect.X + nBmpW;
    ImageDrawMethod.AddDrawInfo( @info );
    
    //右边按键   
    info.FClickID := CtClickID_IncButton;
    info.FLayoutIndex := CtLayoutButton;
    Inc( info.FDestRect.X, FSliderMaxRunSize + nW );
    Inc( info.FNormalSrcRect.X, nBmpSpace );
    Inc( info.FActiveSrcRect.X, nBmpSpace );
    Inc( Info.FDownSrcRect.X, nBmpSpace );
    ImageDrawMethod.AddDrawInfo( @info ); 

    //背景
    info.FClickID := CtClickID_Other;
    info.FLayoutIndex := CtLayoutBk;
    info.FDrawStyle := dsStretchByVH;
    info.FDestRect := MakeRect(nW, 0, FSliderMaxRunSize, nH);
    info.FNormalSrcRect := MakeRect(nW, 0, nW, nH);
    info.FActiveSrcRect := info.FNormalSrcRect;
    Inc( info.FActiveSrcRect.X, nBmpW ); 
    info.FDownSrcRect := info.FActiveSrcRect;
    Inc( info.FDownSrcRect.X, nBmpW );
    ImageDrawMethod.AddDrawInfo( @info );

    //滑块
    info.FClickID := CtClickID_SliderButton;
    info.FLayoutIndex := CtLayoutButton;
    info.FDrawStyle := dsStretchByVH;
    info.FDestRect := MakeRect(nW, 0, nW, nH);
    info.FNormalSrcRect := MakeRect(nW * 2, 0, nW, nH);
    info.FActiveSrcRect := info.FNormalSrcRect;
    Inc( info.FActiveSrcRect.X, nBmpW );
    info.FDownSrcRect := info.FActiveSrcRect;
    Inc( info.FDownSrcRect.X, nBmpW );
    ImageDrawMethod.AddDrawInfo( @info );
  end
  else
  begin
    //垂直方式
    if nBmpH < nBmpW then
    begin
      //将水平图片修改成垂直方式
      bmp := TGPBitmap.Create( nBmpH, nBmpW, ImageInfo.Image.GetPixelFormat );
      G := TGPGraphics.Create( bmp );
      destPt[0] := MakePoint( Integer(bmp.GetWidth), 0 );
      destPt[1] := MakePoint( Integer(bmp.GetWidth), Integer(bmp.GetHeight) );
      destPt[2] := MakePoint( 0, 0 );
      G.DrawImage( ImageInfo.Image, PGpPoint(@destPt), 3, 0, 0, nBmpW, nBmpH, UnitPixel );      
      FreeAndNil( FBufferBmp );
      G.Free;
      
      FBufferBmp := bmp;
      ImageInfo.OnChange := nil;
      ImageInfo.Image := FBufferBmp;
      ImageInfo.OnChange := DoObjectChanged;
    end;
    nBmpW := ImageInfo.Image.GetWidth;
    nBmpH := ImageInfo.Image.GetHeight div 3;
    nW := nBmpW;
    nH := nBmpH div 4; //四种图形
    FSliderMaxRunSize := Height - nH * 2 + Height mod 2;
    FSliderMin := nH;
    nBmpSpace := nH * 3;
    Width := nW;
    
    //上边按钮
    info.FText := '';
    info.FClickID := CtClickID_DecButton;
    info.FLayoutIndex := CtLayoutButton;
    info.FDrawStyle := dsPaste;
    info.FDestRect := MakeRect(0, 0, nW, nH);
    info.FNormalSrcRect := MakeRect(0, 0, nW, nH);
    info.FActiveSrcRect := info.FNormalSrcRect;
    Inc( info.FActiveSrcRect.Y, nBmpH );
    info.FDownSrcRect := info.FActiveSrcRect;
    Inc( info.FDownSrcRect.Y, nBmpH );
    ImageDrawMethod.AddDrawInfo( @info );
    
    //下边按键   
    info.FClickID := CtClickID_IncButton;
    info.FLayoutIndex := CtLayoutButton;
    Inc( info.FDestRect.Y, FSliderMaxRunSize + nH );
    Inc( info.FNormalSrcRect.Y, nBmpSpace );
    Inc( info.FActiveSrcRect.Y, nBmpSpace );
    Inc( Info.FDownSrcRect.Y, nBmpSpace );
    ImageDrawMethod.AddDrawInfo( @info ); 

    //背景
    info.FClickID := CtClickID_Other;
    info.FLayoutIndex := CtLayoutBk;
    info.FDrawStyle := dsStretchByVH;
    info.FDestRect := MakeRect(0, nH, nW, FSliderMaxRunSize);
    info.FNormalSrcRect := MakeRect(0, nH, nW, nH);
    info.FActiveSrcRect := info.FNormalSrcRect;
    Inc( info.FActiveSrcRect.Y, nBmpH );
    info.FDownSrcRect := info.FActiveSrcRect;
    Inc( info.FDownSrcRect.Y, nBmpH );
    ImageDrawMethod.AddDrawInfo( @info );

    //滑块
    info.FClickID := CtClickID_SliderButton;
    info.FLayoutIndex := CtLayoutButton;
    info.FDrawStyle := dsStretchByVH;
    info.FDestRect := MakeRect(0, nH, nW, nH);
    info.FNormalSrcRect := MakeRect(0, nH * 2, nW, nH);
    info.FActiveSrcRect := info.FNormalSrcRect;
    Inc( info.FActiveSrcRect.Y, nBmpH );
    info.FDownSrcRect := info.FActiveSrcRect;
    Inc( info.FDownSrcRect.Y, nBmpH );
    ImageDrawMethod.AddDrawInfo( @info );
  end; 

  FindSliderItem; 
end;

procedure TxdScrollBar.ReCalcSliderInfo;
var
  nSliderSize: Integer;
begin
  if not Assigned(FSliderInfo) or (FSliderMaxRunSize <= 0) or (Max <= 0) then Exit;

  nSliderSize := (Max - 1) * CtMinPerSlipPiexl;
  if nSliderSize + CtMinPerSlipPiexl > FSliderMaxRunSize then
  begin
    FPrePosPiexl := ( FSliderMaxRunSize - FSliderMin ) / Max;
    nSliderSize := FSliderMaxRunSize - Round( Max * FPrePosPiexl );
  end
  else
  begin
    FPrePosPiexl := CtMinPerSlipPiexl;
    nSliderSize := FSliderMaxRunSize - Max * CtMinPerSlipPiexl;
  end;
  if nSliderSize > SliderMax then
  begin
    nSliderSize := SliderMax;
    FPrePosPiexl := (FSliderMaxRunSize - nSliderSize) / Max;
  end;

  if FScrollBarStyle = ssVerical then
  begin
    FSliderInfo^.FDestRect.X := FSliderMin + Round( FPrePosPiexl * FPos );
    FSliderInfo^.FDestRect.Width := nSliderSize;
  end
  else
  begin
    FSliderInfo^.FDestRect.Y := FSliderMin + Round( FPrePosPiexl * FPos );
    FSliderInfo^.FDestRect.Height := nSliderSize;
  end;
end;

procedure TxdScrollBar.Resize;
begin
  inherited;
  ReBuildDrawInfo;
  ReCalcSliderInfo;
end;

procedure TxdScrollBar.SetMax(const Value: Integer);
begin
  if (FMax <> Value) and (Value > 0) and (Value >= Position) then
  begin
    FMax := Value;
    ReCalcSliderInfo;
    Invalidate;
  end;
end;

procedure TxdScrollBar.SetPos(const Value: Integer);
var
  R1, R2: TGPRect;
begin
  if (FPos <> Value) and (Value >= 0) and (Value <= FMax) then
  begin
    FPos := Value;
    if Assigned(FSliderInfo) then
      R1 := FSliderInfo^.FDestRect;
    ReCalcSliderInfo;
    if Assigned(FSliderInfo) then
    begin
      R2 := FSliderInfo^.FDestRect;
      case FScrollBarStyle of
        ssVerical: 
        begin
          if R1.X > R2.X then
          begin
            R1.Width := R1.X + R1.Width - R2.X + 1;
            R1.X := R2.X;            
          end
          else
            R1.Width := R2.X + R2.Width - R1.X;
        end;        
        ssHorizontal: 
        begin
          if R1.Y > R2.Y then
          begin
            R1.Height := R1.Y + R1.Height - R2.Y + 1;
            R1.Y := R2.Y;
          end
          else
            R1.Height := R2.Y + R2.Height - R1.Y;
        end;
      end;
      InvalidateRect( R1 );
    end;
    DoNotifyPosChanged;
  end;
  FChangeStyle := csNull;
end;

procedure TxdScrollBar.SetScrollBarStyle(const Value: TxdScrollStyle);
var
  nTemp: Integer;
begin
  if FScrollBarStyle <> Value then
  begin
    FScrollBarStyle := Value;
    if FScrollBarStyle = ssVerical then
    begin
      if (csDesigning in ComponentState) and (Height > Width) then
      begin
        nTemp := Height;
        Height := Width;
        Width := nTemp;
      end;           
    end
    else
    begin
      if (csDesigning in ComponentState) and (Height < Width) then
      begin
        nTemp := Height;
        Height := Width;
        Width := nTemp;
      end;
    end;    
    ReBuildDrawInfo; 
    ReCalcSliderInfo;   
  end;
end;

procedure TxdScrollBar.SetSliderMax(const Value: Integer);
begin
  if (FSliderMax <> Value) and (Value > 50) then
  begin
    FSliderMax := Value;
    ReBuildDrawInfo;
    ReCalcSliderInfo;
  end;
end;

procedure TxdScrollBar.WMLButtonUp(var Message: TMessage);
begin
  inherited;
  FreeAndNil( FTimeToMovePos );
end;

procedure TxdScrollBar.WMMouseMove(var Message: TWMMouseMove);
var
  nLen, nPos: Integer;
  p: PDrawInfo;
begin  
  inherited;
  p := CurActiveSubItem;
  if Assigned(p) then
  begin
    if GetCurControlState = uiDown then
    begin
      case p^.FClickID of
        CtClickID_SliderButton:
        begin
          if FScrollBarStyle = ssVerical then
            nLen := Message.XPos - FMouseDownPt.X
          else
            nLen := Message.YPos - FMouseDownPt.Y;
          if abs(nLen) < Round(FPrePosPiexl) then Exit;          
          nPos := Round( nLen / FPrePosPiexl );
          if abs(nPos) > 0 then
          begin
            FMouseDownPt.X := Message.XPos;
            FMouseDownPt.Y := Message.YPos;
            FChangeStyle := csMouse;
            Position := Position + nPos;
          end;
        end;
      end;   
    end;
  end;
end;

end.
