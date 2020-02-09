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

unit rz_an_cmp_synedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, LCLType, SynEdit, rz_an_cmp_statusbar,
  rz_an_pas_var, SynHighlighterPas, SynHighlighterJava, SynHighlighterPython;

type

  {1.1.4}
  TRZANCustomSynEdit = class(TSynEdit)
  private
    {5. Editor Format}
    {5.20}
    FRZLanguage : rz_an_type_Language;
    FRZLangPascal : TSynPasSyn;
    FRZLangJava : TSynJavaSyn;
    FRZLangPython : TSynPythonSyn;
    {7. Caret}
    {7.1}
    FRZCaretPosX : Integer;
    {7.2}
    FRZCaretPosY : Integer;
  protected
    {5. Editor Format}
    {5.20}
    procedure SetRZLanguage (const AValue: rz_an_type_Language);
    {7. Caret}
    {7.1}
    procedure SetRZCaretPosX (const AValue: Integer);
    {7.2}
    procedure SetRZCaretPosY (const AValue: Integer);
    {8. Events}
    {8.1}
    procedure RZDoChange (Sender : TObject);
    {8.2}
    procedure RZDoEnter (Sender : TObject);
    {8.3}
    procedure KeyDown (var Key: Word; Shift: TShiftState); override;
    {8.4}
    procedure KeyPress (var Key: char); override;
    {8.5}
    procedure KeyUp (var Key: Word; Shift: TShiftState); override;
    {8.8}
    procedure RZDoClick (Sender : TObject);
    {8.90}
    procedure RZOnEventUpdate;
    {5. Editor Format}
    {5.20}
    property RZLanguage : rz_an_type_Language read FRZLanguage write SetRZLanguage;
    property RZLangJava : TSynJavaSyn read FRZLangJava write FRZLangJava;
    property RZLangPascal : TSynPasSyn read FRZLangPascal write FRZLangPascal;
    property RZLangPython : TSynPythonSyn read FRZLangPython write FRZLangPython;
    {7. Caret}
    {7.1}
    property RZCaretPosX : Integer read FRZCaretPosX write SetRZCaretPosX;
    {7.2}
    property RZCaretPosY : Integer read FRZCaretPosY write SetRZCaretPosY;
  public
    {1. Application}
    {1.1.4}
    constructor Create (AOwner : TComponent); override;
    {2. File Operation}
    {2.3}
    procedure RZOpen (AFileName : TFileName);
    {2.4}
    procedure RZSave (AFileName : TFileName);
    {3. Text Editing}
    {3.1}
    procedure RZUndo;
    {3.2}
    procedure RZRedo;
    {3.3}
    procedure RZCopy;
    {3.4}
    procedure RZCut;
    {3.5}
    procedure RZPaste;
    {3.6}
    procedure RZSelectAll;
  end;

  {1.1.4}
  TRZANSynEdit = class(TRZANCustomSynEdit)
  public
    {5. Editor Format}
    {5.20}
    property RZLanguage;
    {7. Caret}
    {7.1}
    property RZCaretPosX;
    {7.2}
    property RZCaretPosY;
  end;

implementation

uses
  rz_an_cmp_tabsheet;

{1. Application}

{1.1.4}
constructor TRZANCustomSynEdit.Create (AOwner : TComponent);
begin
  {1. Application}
  {1.1}
  inherited Create(AOwner);
  Self.Font.Name := 'Courier New';
  Self.Font.Size := 10;
  Self.Font.Color := clBlack;
  Self.Font.Quality := fqDraft;
  {5. Editor Format}
  {5.20}
  Self.RZLangJava := TSynJavaSyn.Create(Self);
  Self.RZLangPascal := TSynPasSyn.Create(Self);
  Self.RZLangPython := TSynPythonSyn.Create(Self);
  {8. Events}
  {8.1}
  Self.OnChange := @Self.RZDoChange;
  {8.2}
  Self.OnEnter := @Self.RZDoEnter;
  {8.8}
  Self.OnClick := @Self.RZDoClick;
end;

{2. File Operation}

{2.3}
procedure TRZANCustomSynEdit.RZOpen (AFileName : TFileName);
begin
  Self.Lines.LoadFromFile(AFileName);
end;

{2.4}
procedure TRZANCustomSynEdit.RZSave (AFileName : TFileName);
begin
  Self.Lines.SaveToFile(AFileName);
end;

{3. Text Editing}

{3.1}
procedure TRZANCustomSynEdit.RZUndo;
begin
  Self.Undo;
end;

{3.2}
procedure TRZANCustomSynEdit.RZRedo;
begin
  Self.Redo;
end;

{3.3}
procedure TRZANCustomSynEdit.RZCopy;
begin
  Self.CopyToClipboard;
end;

{3.4}
procedure TRZANCustomSynEdit.RZCut;
begin
  Self.CutToClipboard;
end;

{3.5}
procedure TRZANCustomSynEdit.RZPaste;
begin
  Self.PasteFromClipboard;
end;

{3.6}
procedure TRZANCustomSynEdit.RZSelectAll;
begin
  Self.SelectAll;
end;

{5. Editor Format}

{5.20}
procedure TRZANCustomSynEdit.SetRZLanguage (const AValue: rz_an_type_Language);
begin
  Self.FRZLanguage := AValue;
  case Self.RZLanguage of
    rz_an_type_lang_Java : Self.Highlighter := Self.RZLangJava;
    rz_an_type_lang_Pascal : Self.Highlighter := Self.RZLangPascal;
    rz_an_type_lang_Python : Self.Highlighter := Self.RZLangPython;
  end;
end;

{7. Caret}

{7.1}
procedure TRZANCustomSynEdit.SetRZCaretPosX (const AValue : Integer);
begin
  Self.FRZCaretPosX := AValue;
  if (Self.Parent <> nil) then (Self.Parent as TRZANTabSheet).RZCaretPosX := Self.RZCaretPosX - 1;
end;

{7.2}
procedure TRZANCustomSynEdit.SetRZCaretPosY (const AValue : Integer);
begin
  Self.FRZCaretPosY := AValue;
  if (Self.Parent <> nil) then (Self.Parent as TRZANTabSheet).RZCaretPosY := Self.RZCaretPosY;
end;

{8. Events}

{8.1}
procedure TRZANCustomSynEdit.RZDoChange (Sender : TObject);
begin
  Self.RZOnEventUpdate;
  (Self.Parent as TRZANTabSheet).RZStatus := rz_an_type_status_Modified;
end;

{8.2}
procedure TRZANCustomSynEdit.RZDoEnter (Sender : TObject);
begin
  Self.RZOnEventUpdate;
end;

{8.3}
procedure TRZANCustomSynEdit.KeyDown (var Key: Word; Shift: TShiftState);
begin
  Self.RZOnEventUpdate;
  if Key = VK_TAB then
  begin
    // Key:= 0;
    Key := VK_A; //VK_SHIFT + VK_TAB;
  end;
  inherited;
end;

{8.4}
procedure TRZANCustomSynEdit.KeyPress (var Key: char);
begin
  Self.RZOnEventUpdate;
  inherited;
end;

{8.5}
procedure TRZANCustomSynEdit.KeyUp (var Key: Word; Shift: TShiftState);
begin
  Self.RZOnEventUpdate;
  inherited;
end;

{8.8}
procedure TRZANCustomSynEdit.RZDoClick (Sender : TObject);
begin
  Self.RZOnEventUpdate;
end;

{8.90}
procedure TRZANCustomSynEdit.RZOnEventUpdate;
begin
  Self.RZCaretPosX := Self.CaretX;
  Self.RZCaretPosY := Self.CaretY;
end;

end.

