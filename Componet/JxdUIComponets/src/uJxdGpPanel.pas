{
单元名称: uJxdCustomPanel
单元作者: 江晓德(jxd524@163.com)
说    明: 容器类组件父类
开始时间: 2011-09-29
修改时间: 2011-09-29 (最后修改)
}
unit uJxdGpPanel;

interface

uses
  SysUtils, Classes, Windows, Controls, ExtCtrls, Messages, Forms, uJxdGpStyle, uJxdGpSub, GDIPAPI, GDIPOBJ;

type
  TxdPanel = class(TCustomPanel)
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    FParentForm: TForm;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure DoDrawObjectChanged(Sender: TObject);  
  private
    {绘制相关变量}
    FBmpDC: HDC;
    FBufBmp: HBITMAP;
    FCurBmpWidth, FCurBmpHeight: Integer;
    FDelDrawObjectTimer: TTimer;

    procedure ClearBkRes;
    procedure DoTimerToDeleteDrawObject(Sender: TObject);

    procedure MoveMainWindow;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED; //不处理字体
  private
    FMoveForm: Boolean;
    FImageInfo: TImageInfo;
    FImgDrawMethod: TDrawMethod;    
    procedure SetMoveForm(const Value: Boolean);
    procedure SetImageInfo(const Value: TImageInfo);
    procedure SetImgDrawMethod(const Value: TDrawMethod);
  published
    property ImageInfo: TImageInfo read FImageInfo write SetImageInfo; //图像属性
    property ImageDrawMethod: TDrawMethod read FImgDrawMethod write SetImgDrawMethod; //图像绘制方式
    property AutoMoveForm: Boolean read FMoveForm write SetMoveForm default True;
    property MoveForm: TForm read FParentForm write FParentForm;
    property DockManager;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property VerticalAlignment;
    property Visible;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

{ TKKPanel }

procedure TxdPanel.ClearBkRes;
begin
  FCurBmpWidth := 0;
  FCurBmpHeight := 0;
  if FBufBmp <> 0 then
  begin
    DeleteObject( FBufBmp );
    FBufBmp := 0;
  end;
  if FBmpDC <> 0 then
  begin
    DeleteDC( FBmpDC );
    FBmpDC := 0;
  end;
end;

procedure TxdPanel.CMTextChanged(var Message: TMessage);
begin
  Message.Result := 1;
end;

constructor TxdPanel.Create(AOwner: TComponent);
begin
  inherited;
  if (AOwner <> nil) and (AOwner is TForm) then
    FParentForm := (AOwner as TForm)
  else
    FParentForm := nil;
  FBmpDC := 0;
  FBufBmp := 0;
  FCurBmpWidth := 0;
  FCurBmpHeight := 0;
  FDelDrawObjectTimer := nil;
  FMoveForm := True;
  FImageInfo := TImageInfo.Create;
  FImgDrawMethod := TDrawMethod.Create;

  FImageInfo.OnChange := DoDrawObjectChanged;
  FImgDrawMethod.OnChange := DoDrawObjectChanged;

  BevelOuter := bvNone;
end;

procedure TxdPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

destructor TxdPanel.Destroy;
begin
  FreeAndNil( FImageInfo );
  FreeAndNil( FImgDrawMethod );
  ClearBkRes;
  inherited;
end;

procedure TxdPanel.DoDrawObjectChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TxdPanel.DoTimerToDeleteDrawObject(Sender: TObject);
begin
  ClearBkRes;
  FreeAndNil( FDelDrawObjectTimer );
end;

procedure TxdPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FMoveForm then
    MoveMainWindow;
end;

procedure TxdPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TxdPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TxdPanel.MoveMainWindow;
begin
  if (FParentForm <> nil) and ( FParentForm.WindowState <> wsMaximized) then
  begin
    ReleaseCapture;
    PostMessage(FParentForm.Handle, WM_SYSCOMMAND, 61458, 0);
  end;
end;

procedure TxdPanel.SetImageInfo(const Value: TImageInfo);
begin
  FImageInfo.Assign( Value );
end;

procedure TxdPanel.SetImgDrawMethod(const Value: TDrawMethod);
begin
  FImgDrawMethod.Assign( Value );
end;

procedure TxdPanel.SetMoveForm(const Value: Boolean);
begin
  FMoveForm := Value;
end;

procedure TxdPanel.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

procedure TxdPanel.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PS: TPaintStruct;
  bmp: TGPBitmap;
  bmpG: TGPGraphics;
  bAllReDraw: Boolean;
  
  MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
begin
//  OutputDebugString( Pchar(IntToStr(Message.DC)));
  if (Message.DC = 0) and Assigned(FImageInfo.Image) then
  begin    
    DC := BeginPaint(Handle, PS);
    try 
      bAllReDraw := False;
      if (FCurBmpWidth <> Width) or (FCurBmpHeight <> Height) or 
         ((PS.rcPaint.Right - PS.rcPaint.Left = Width) and (PS.rcPaint.Bottom - PS.rcPaint.Top = Height)) then
      begin
        if FBufBmp <> 0 then
          DeleteObject( FBufBmp );
        if FBmpDC <> 0 then
          DeleteDC( FBmpDC );  
                
        FCurBmpWidth := Width;
        FCurBmpHeight := Height;
        bAllReDraw := True;
        
        bmp := TGPBitmap.Create( FCurBmpWidth, FCurBmpHeight );
        bmpG := TGPGraphics.Create( bmp );

        //绘制背景到临时图片
        DrawImageCommon( bmpG, MakeRect(0, 0, Width, Height), FImageInfo, FImgDrawMethod, nil, nil, nil, nil );
        
        bmp.GetHBITMAP( 0, FBufBmp );
        bmpG.Free;        
        bmp.Free;
        
        FBmpDC := CreateCompatibleDC( DC );
        SelectObject( FBmpDC, FBufBmp );        
      end;

      if ControlCount = 0 then
      begin
        if bAllReDraw then
          BitBlt( DC, 0, 0, Width, Height, FBmpDC, 0, 0, SRCCOPY )
        else
        begin
          BitBlt( DC, PS.rcPaint.Left, PS.rcPaint.Top, 
            PS.rcPaint.Right - PS.rcPaint.Left, PS.rcPaint.Bottom - PS.rcPaint.Top,
            FBmpDC, PS.rcPaint.Left, PS.rcPaint.Top, SRCCOPY );
        end;
      end
      else
      begin
        //绘制子控件
        MemDC := CreateCompatibleDC(DC);
        MemBitmap := CreateCompatibleBitmap(DC, PS.rcPaint.Right - PS.rcPaint.Left,
          PS.rcPaint.Bottom - PS.rcPaint.Top);
        OldBitmap := SelectObject(MemDC, MemBitmap);
        try
          SetWindowOrgEx(MemDC, PS.rcPaint.Left, PS.rcPaint.Top, nil);
          BitBlt( MemDC, 0, 0, Width, Height, FBmpDC, 0, 0, SRCCOPY );
          Message.DC := MemDC;
          WMPaint(Message);
          Message.DC := 0;
          BitBlt(DC, PS.rcPaint.Left, PS.rcPaint.Top,
            PS.rcPaint.Right - PS.rcPaint.Left, PS.rcPaint.Bottom - PS.rcPaint.Top,
            MemDC, PS.rcPaint.Left, PS.rcPaint.Top, SRCCOPY);
        finally
          SelectObject(MemDC, OldBitmap);
          DeleteDC(MemDC);
          DeleteObject(MemBitmap);
        end;
      end;
    finally
      EndPaint(Handle, PS);
    end;

    if not (csDesigning in ComponentState) then
    begin
      if not Assigned(FDelDrawObjectTimer) then
      begin
        FDelDrawObjectTimer := TTimer.Create( Self );
        FDelDrawObjectTimer.OnTimer := DoTimerToDeleteDrawObject;
        FDelDrawObjectTimer.Interval := 1000;
        FDelDrawObjectTimer.Enabled := True;
      end
      else
      begin
        FDelDrawObjectTimer.Enabled := False;
        FDelDrawObjectTimer.Enabled := True;
      end;
    end;
  end
  else
    inherited;  
end;

end.
