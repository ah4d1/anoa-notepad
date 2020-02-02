unit rz_an_cmp_linenumber;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, Graphics, StdCtrls, Dialogs, RichMemo, LCLIntf, LCLType;

type

  TRZANCustomLineNumber = class(TStringGrid)
  public
    constructor Create (AOwner : TComponent); override;
    procedure RZAllLineNumber (AEditor : TRichMemo; ARowHeight : Integer);
  end;

  TRZANLineNumber = class(TRZANCustomLineNumber)
  end;

implementation

constructor TRZANCustomLineNumber.Create (AOwner : TComponent);
var
  LTextStyle : TTextStyle;
begin
  inherited Create(AOwner);
  {Component Properties}
  Self.FixedRows := 0;
  Self.Width := 70;
  Self.ColWidths[0] := Self.Width;
  Self.Color := $00EFEFEF;
  Self.Font.Size := 8;
  Self.ScrollBars := ssNone;
  Self.GridLineWidth := 0;
  {Set Default Text Style}
  LTextStyle := Self.DefaultTextStyle;
  LTextStyle.Alignment := taCenter;
  Self.DefaultTextStyle := LTextStyle;
end;

procedure TRZANCustomLineNumber.RZAllLineNumber (AEditor : TRichMemo; ARowHeight : Integer);
var
  i : Integer;
begin
  Self.RowCount := AEditor.Lines.Count + 1;
  if AEditor.Lines.Count > 0 then
  begin
    for i := 1 to Self.RowCount-1 do
    begin
      Self.RowHeights[i-1] := ARowHeight;
      Self.Cells[0,i-1] := IntToStr(i);
    end;
    Self.Cells[0,i] := 'EOF';
  end;
end;

end.

