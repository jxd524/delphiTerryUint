{
单元名称: JxdCustomButton
单元作者: 江晓德(jxd524@163.com)
网    址: www.geysoft.com
说    明: 自绘按钮父类
开始时间: 2007-10-24
修改时间: 2007-11-3 (最后修改) 
}
unit JxdCustomButton;

interface

uses
  Windows,SysUtils, Classes, Controls, StdCtrls,Graphics,Messages;

type
  TJxdCustomButton = class(TButton)
  private
    FIsFocused: Boolean;
    FCanvas: TCanvas;
    FIsMouseInButton: Boolean;
    function GetIsMouseDown: Boolean;

    {消息处理}
    procedure CMFontChanged(var message: TMessage); message CM_FONTCHANGED;
    procedure CMEnablChanged(var message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMEraseBkGnd(var message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CNMeasurcItem(var message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMLButtonDBlclk(var message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMMouceEnter(var message: TMessage); message CM_MOUSEENTER;
    procedure CMMouceLeave(var message: TMessage); message CM_MOUSELEAVE;
  protected
    property Canvas: TCanvas read FCanvas;

    procedure Paint; virtual;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct); virtual;
    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    property IsFocused: Boolean read FIsFocused;
    property IsMouseInButton: Boolean read FIsMouseInButton;
    property IsDown: Boolean read GetIsMouseDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;
implementation

procedure TJxdCustomButton.CMEnablChanged(var message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJxdCustomButton.CMFontChanged(var message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJxdCustomButton.CMMouceEnter(var message: TMessage);
begin
  inherited;
  if not FIsMouseInButton then
  begin
    FIsMouseInButton := True;
    Invalidate;
  end;
end;

procedure TJxdCustomButton.CMMouceLeave(var message: TMessage);
begin
  inherited;
  ControlState := ControlState - [csLButtonDown];
  if FIsMouseInButton then
  begin
    FIsMouseInButton := False;
    Invalidate;
  end;
  
end;

procedure TJxdCustomButton.CNDrawItem(var message: TWMDrawItem);
begin
  DrawItem(message.DrawItemStruct^);
end;

procedure TJxdCustomButton.CNMeasurcItem(var message: TWMMeasureItem);
begin
  message.MeasureItemStruct^.itemWidth := Width;
  message.MeasureItemStruct^.itemHeight := Height;
end;

constructor TJxdCustomButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

procedure TJxdCustomButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BS_OWNERDRAW;
end;

destructor TJxdCustomButton.Destroy;
begin
  inherited;
end;

procedure TJxdCustomButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
begin
  Paint;
end;

function TJxdCustomButton.GetIsMouseDown: Boolean;
begin
  if csLButtonDown in ControlState then
    Result := True
  else
    Result := False;
end;

procedure TJxdCustomButton.Paint;
begin

end;

procedure TJxdCustomButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> FIsFocused then
  begin
    FIsFocused := ADefault;
    Repaint;
  end;
end;

procedure TJxdCustomButton.WMEraseBkGnd(var message: TWMEraseBkgnd);
begin
  message.Result := 0;
end;

procedure TJxdCustomButton.WMLButtonDBlclk(var message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, message.Keys, LongInt(message.Pos));
end;

end.
