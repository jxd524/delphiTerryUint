unit uKKStringGrid;

interface

uses
  Messages, SysUtils, Classes, StdCtrls, Controls, Grids, uKKCustomScrollBar, uKKScrollBar, Graphics,
  Types, Forms, uKKBitmapHandle, TntGrids, Windows;

type
  TKKStringGrid = class(TTntStringGrid)
  private
    FKKScrollBar: TKKScrollBar;
    procedure SetKKScrollBar(const Value: TKKScrollBar);
    procedure DoScrollBarPosChanged(Sender: TObject);
    procedure SynScrollBarPos(const nOldRow, nCurRow, nDrawFirstRow, nDrawLastRow: Integer);
    procedure SynScrollBarCount;
    function GetKKRowCountEx: Integer;
    procedure SetKKRowCountEx(const Value: Integer);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RowCount: Integer read GetKKRowCountEx write SetKKRowCountEx default 10;
    property KKScrollBar: TKKScrollBar read FKKScrollBar write SetKKScrollBar;
  end;

implementation
{ TAdvStringGrid1 }
{ TKKStringGrid }

constructor TKKStringGrid.Create(AOwner: TComponent);
begin
  inherited;
  FKKScrollBar := nil;
  FixedCols := 0;
  FixedRows := 1;
  ScrollBars := ssNone;
  Ctl3D := False;
end;

destructor TKKStringGrid.Destroy;
begin

  inherited;
end;

function TKKStringGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  nOldRow, nCurRow, nFirstRow, nLastRow: Integer;
  DrawInfo: TGridDrawInfo;
begin
  if not Assigned(FKKScrollBar) then
  begin
    Result := inherited DoMouseWheelDown(Shift, MousePos);
    Exit;
  end;
  nOldRow := Row;
  CalcDrawInfo(DrawInfo);
  nFirstRow := DrawInfo.Vert.FirstGridCell;
  nLastRow := DrawInfo.Vert.LastFullVisibleCell;
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  nCurRow := Row;
  SynScrollBarPos( nOldRow, nCurRow, nFirstRow, nLastRow );
end;

function TKKStringGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  nOldRow, nCurRow, nFirstRow, nLastRow: Integer;
  DrawInfo: TGridDrawInfo;
begin
  if not Assigned(FKKScrollBar) then
  begin
    Result := inherited DoMouseWheelUp(Shift, MousePos);
    Exit;
  end;
  nOldRow := Row;
  CalcDrawInfo(DrawInfo);
  nFirstRow := DrawInfo.Vert.FirstGridCell;
  nLastRow := DrawInfo.Vert.LastFullVisibleCell;
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  nCurRow := Row;
  SynScrollBarPos( nOldRow, nCurRow, nFirstRow, nLastRow );
end;

procedure TKKStringGrid.DoScrollBarPosChanged(Sender: TObject);
var
  nPos1, nPos2: Integer;
  b: Boolean;
begin
  Exit;
  nPos1 := GetScrollPos(Handle, SB_VERT);
  nPos2 := KKScrollBar.Position;
  SetScrollPos( Handle, SB_VERT, nPos2, False);
  if nPos1 > nPos2 then
  begin
    PostMessage(Handle, WM_VSCROLL, SB_LINEUP, 0);
    Sleep(10);
    PostMessage(Handle, WM_VSCROLL, SB_ENDSCROLL, 0);
  end
  else if nPos1 < nPos2 then
  begin
    PostMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
    Sleep(10);
    PostMessage(Handle, WM_VSCROLL, SB_ENDSCROLL, 0);
  end;
end;

function TKKStringGrid.GetKKRowCountEx: Integer;
begin
  Result := inherited RowCount;
end;

procedure TKKStringGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  nOldRow, nCurRow, nFirstRow, nLastRow: Integer;
  DrawInfo: TGridDrawInfo;
begin
  if not Assigned(FKKScrollBar) then
  begin
    inherited KeyDown(Key, Shift);
    Exit;
  end;
  nOldRow := Row;
  CalcDrawInfo(DrawInfo);
  nFirstRow := DrawInfo.Vert.FirstGridCell;
  nLastRow := DrawInfo.Vert.LastFullVisibleCell;
  inherited KeyDown(Key, Shift);
  nCurRow := Row;
  SynScrollBarPos( nOldRow, nCurRow, nFirstRow, nLastRow );
end;

procedure TKKStringGrid.SetKKRowCountEx(const Value: Integer);
begin
  inherited RowCount := Value;
  SynScrollBarCount;
end;

procedure TKKStringGrid.SetKKScrollBar(const Value: TKKScrollBar);
begin
  FKKScrollBar := Value;
  if FKKScrollBar <> nil then
  begin
    with FKKScrollBar do
    begin
      Min := 0;
      Position := 0;
      OnPosChange := DoScrollBarPosChanged;
    end;
    SynScrollBarCount;
  end;
end;

procedure TKKStringGrid.SynScrollBarPos(const nOldRow, nCurRow, nDrawFirstRow, nDrawLastRow: Integer);
var
  DrawInfo: TGridDrawInfo;
  nCurFirstRow, nCurLastRow: Integer;
begin
  CalcDrawInfo(DrawInfo);
  nCurFirstRow := DrawInfo.Vert.FirstGridCell;
  nCurLastRow := DrawInfo.Vert.LastFullVisibleCell;

  if not Assigned(FKKScrollBar) then Exit;
  if nOldRow < nCurRow then //向下滚动
  begin
    if nCurRow > nDrawLastRow then
    begin
      FKKScrollBar.Position := FKKScrollBar.Position + (nCurRow - nDrawLastRow);
    end;
  end
  else if nOldRow > nCurRow then //向上滚动
  begin
    if nCurRow < nDrawFirstRow then
      FKKScrollBar.Position := FKKScrollBar.Position - (nDrawFirstRow - nCurRow);
  end;
end;

procedure TKKStringGrid.SynScrollBarCount;
var
  nCount: Integer;
begin
  if not Assigned(FKKScrollBar) then Exit;
  nCount := RowCount - VisibleRowCount - FixedRows;
  if nCount > 1 then
  begin
    FKKScrollBar.Max := nCount;
    SetScrollRange( Handle, SB_VERT, FKKScrollBar.Min, FKKScrollBar.Max, False );
    ShowScrollBar( Handle, SB_VERT, False );
    if not FKKScrollBar.Visible then
      FKKScrollBar.Visible := True;
  end
  else
    FKKScrollBar.Visible := False;
end;

end.
