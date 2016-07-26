unit uJxdGpButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, GDIPAPI, GDIPOBJ, uJxdGpStyle, uJxdGpCommon;

type
{$M+}
  TxdButton = class(TxdGpCommon)  
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  protected
    function  DoGetDrawState(const Ap: PDrawInfo): TxdGpUIState; override;
    procedure DoChangedSrcBmpRect(const AState: TxdGpUIState; var ASrcBmpRect: TGPRect); override;
  private
    FSelected: Boolean;
    procedure SetSelected(const Value: Boolean);
  published
    property Selected: Boolean read FSelected write SetSelected;
  end;

  TxdGraphicsPanel = class(TxdGpCommon)
  protected
    function  DoGetDrawState(const Ap: PDrawInfo): TxdGpUIState; override;
    procedure DoControlStateChanged(const AOldState, ANewState: TxdGpUIState; const ACurPt: TPoint); override;
  end;

implementation

{ TKgUICoreCommon }

constructor TxdButton.Create(AOwner: TComponent);
begin
  FSelected := False;
  inherited;  
end;

destructor TxdButton.Destroy;
begin

  inherited;
end;


procedure TxdButton.DoChangedSrcBmpRect(const AState: TxdGpUIState; var ASrcBmpRect: TGPRect);
begin
  if FSelected then
    ASrcBmpRect := MakeRect(0, ASrcBmpRect.Height * (ImageInfo.ImageCount - 1), 
      ASrcBmpRect.Width, ASrcBmpRect.Height);
end;

function TxdButton.DoGetDrawState(const Ap: PDrawInfo): TxdGpUIState;
begin
  if FSelected then
    Result := uiDown
  else
    Result := inherited DoGetDrawState(Ap);
end;

procedure TxdButton.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    Invalidate;
  end;
end;

{ TxdGraphicsPanel }

procedure TxdGraphicsPanel.DoControlStateChanged(const AOldState, ANewState: TxdGpUIState; const ACurPt: TPoint);
begin
//不需要做任务处理
//  inherited;
end;

function TxdGraphicsPanel.DoGetDrawState(const Ap: PDrawInfo): TxdGpUIState;
begin
  Result := uiNormal;
end;

end.
