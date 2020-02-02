// This file is part of Anoa-Notepad project
// Copyright (C)2019 Ahadi Aprianto <ahadi.aprianto@gmail.com>
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either
// version 2 of the License, or (at your option) any later version.
//
// Note that the GPL places important restrictions on "derived works", yet
// it does not provide a detailed definition of that term.  To avoid
// misunderstandings, we consider an application to constitute a
// "derivative work" for the purpose of this license if it does any of the
// following:
// 1. Integrates source code from Anoa-Notepad.
// 2. Integrates/includes/aggregates Anoa-Notepad into a proprietary executable
//    installer, such as those produced by InstallShield.
// 3. Links to a library or executes a program that does any of the above.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

unit rz_an_pas_tabsheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Controls, Dialogs, Graphics, ExtCtrls, Grids, StdCtrls, Messages,
  LCLIntf, rz_an_cmp_richmemo, rz_an_cmp_statusbar, rz_an_pas_opendialog, rz_an_pas_savedialog,
  rz_an_pas_reserved_word, rz_an_pas_language, rz_an_cmp_linenumber, rz_an_pas_var;

type
  TRZANCustomTabSheet = class(TTabSheet)
  private
    FRZFileName : TFileName;
    FRZLineNumberPanel : TRZANLineNumber;
  protected
    procedure SetRZFileName (const AValue : TFileName);
    property RZFileName : TFileName read FRZFileName write SetRZFileName;
    property RZLineNumberPanel : TRZANLineNumber read FRZLineNumberPanel write FRZLineNumberPanel;
  public
    RZEditor : TRZANRichMemo;
    constructor Create (AOwner : TComponent); override;
    procedure DoShow; override;
  end;

  TRZANTabSheet = class(TRZANCustomTabSheet)
  public
    property RZFileName;
  end;

implementation

{Methods}

constructor TRZANCustomTabSheet.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  {Create Line Number Panel}
  Self.RZLineNumberPanel := TRZANLineNumber.Create(Self);
  Self.RZLineNumberPanel.Parent := Self;
  Self.RZLineNumberPanel.Align := alLeft;
  {Create Editor}
  Self.RZEditor := TRZANRichMemo.Create(Self);
  Self.RZEditor.Parent := Self;
  Self.RZEditor.Align := alClient;
  Self.RZEditor.RZANLineNumber := Self.RZLineNumberPanel;
  Self.RZEditor.Lines.Clear;
  Self.RZEditor.OnScroll := @Self.RZEditor.DoScroll;
end;

{DoShow is inherited method, needed to set focus on the editor while select a tabsheet}

procedure TRZANCustomTabSheet.DoShow;
begin
  Self.RZEditor.SetFocus;
  inherited;
end;

{FileName is needed to set TabSheet Caption}

procedure TRZANCustomTabSheet.SetRZFileName (const AValue : TFileName);
begin
  Self.FRZFileName := AValue;
  if Trim(Self.RZFileName) <> '' then Self.Caption := ExtractFileName(Self.RZFileName);
end;

end.

