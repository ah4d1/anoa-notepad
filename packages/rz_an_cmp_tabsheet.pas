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

unit rz_an_cmp_tabsheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Controls, Dialogs, Graphics, ExtCtrls, Grids, StdCtrls, Messages,
  LCLIntf, rz_an_cmp_richmemo, rz_an_cmp_synedit, rz_an_cmp_statusbar, rz_an_cmp_opendialog,
  rz_an_cmp_savedialog, rz_an_pas_var;

type

  TRZANCustomTabSheet = class(TTabSheet)
  private
    {1. Application}
    {1.1.5}
    FRZANStatusBar : TRZANStatusBar;
    {2. File Operation}
    {2.20}
    FRZTabId : Byte;
    {2.30}
    FRZFileName : TFileName;
    {2.31}
    FRZFileExt : string;
    {3. Text Editing}
    {3.80}
    FRZStatus : rz_an_type_Status;
    {5. Editor Format}
    {5.20}
    FRZLanguage : rz_an_type_Language;
    {5.30}
    FRZEditorType : rz_an_type_Editor;
    {7. Caret}
    {7.1}
    FRZCaretPosX : Integer;
    {7.2}
    FRZCaretPosY : Integer;
  protected
    {2. File Operation}
    {2.20}
    procedure SetRZTabId (AValue : Byte);
    {2.30}
    procedure SetRZFileName (const AValue : TFileName);
    {2.31}
    procedure SetRZFileExt (const AValue : string);
    {3. Text Editing}
    {3.80}
    procedure SetRZStatus (const AValue: rz_an_type_Status);
    {5. Editor Format}
    {5.20}
    procedure SetRZLanguage (AValue : rz_an_type_Language);
    {5.30}
    procedure SetRZEditorType (AValue : rz_an_type_Editor);
    {7. Caret}
    {7.1}
    procedure SetRZCaretPosX (const AValue: Integer);
    {7.2}
    procedure SetRZCaretPosY (const AValue: Integer);
    {1. Application}
    {1.1.5}
    property RZANStatusBar : TRZANStatusBar read FRZANStatusBar write FRZANStatusBar;
    {2. File Operation}
    {2.20}
    property RZTabId : Byte read FRZTabId write SetRZTabId;
    {2.30}
    property RZFileName : TFileName read FRZFileName write SetRZFileName;
    {2.31}
    property RZFileExt : string read FRZFileExt write SetRZFileExt;
    {3. Text Editing}
    {3.80}
    property RZStatus : rz_an_type_Status read FRZStatus write SetRZStatus;
    {5. Editor Format}
    {5.20}
    property RZLanguage : rz_an_type_Language read FRZLanguage write SetRZLanguage;
    {5.30}
    property RZEditorType : rz_an_type_Editor read FRZEditorType write SetRZEditorType;
    {7. Caret}
    {7.1}
    property RZCaretPosX : Integer read FRZCaretPosX write SetRZCaretPosX;
    {7.2}
    property RZCaretPosY : Integer read FRZCaretPosY write SetRZCaretPosY;
  public
    {1. Application}
    {1.1.3}
    RZTextEditor : TRZANRichMemo;
    {1.1.4}
    RZSynEditor : TRZANSynEdit;
    {1. Application}
    {1.1.2}
    constructor Create (AOwner : TComponent); override;
    {2. File Operation}
    {2.1} {2.7}
    procedure DoShow; override;
    {2.2}
    function RZClose : Boolean;
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
    {5. Editor Format}
    {5.20}
    procedure RZSetNewLanguage (ALanguage : rz_an_type_Language);
    {6. Editor Style}
    {6.0}
    procedure RZSetNewStyle (AStyle : rz_an_type_Style);
  end;

  TRZANTabSheet = class(TRZANCustomTabSheet)
  public
    {1. Application}
    {1.1.5}
    property RZANStatusBar;
    {2. File Operation}
    {2.20}
    property RZTabId;
    {2.30}
    property RZFileName;
    {3. Text Editing}
    {3.80}
    property RZStatus;
    {5. Editor Format}
    {5.20}
    property RZLanguage;
    {6. Editor Style}
    {6.0}
    property RZEditorType;
    {7. Caret}
    {7.1}
    property RZCaretPosX;
    {7.2}
    property RZCaretPosY;
  end;

implementation

{1. Application}

{1.1.2}
constructor TRZANCustomTabSheet.Create (AOwner : TComponent);
begin
  {1. Application}
  {1.1}
  inherited Create(AOwner);
  Self.ImageIndex := rz_an_var_ImageIndex_Saved;
  {1.1.3}
  Self.RZTextEditor := TRZANRichMemo.Create(Self);
  Self.RZTextEditor.Parent := Self;
  Self.RZTextEditor.Align := alClient;
  Self.RZTextEditor.Visible := True;
  Self.RZTextEditor.Lines.Clear;
  {1.1.4}
  Self.RZSynEditor := TRZANSynEdit.Create(Self);
  Self.RZSynEditor.Parent := Self;
  Self.RZSynEditor.Align := alClient;
  Self.RZSynEditor.Visible := False;
  {5. Editor Format}
  {5.20}
  Self.RZLanguage := rz_an_type_lang_Text;
end;

{2. File Operation}

{2.1} {2.7}
procedure TRZANCustomTabSheet.DoShow;
begin
  inherited;
  if Self.RZEditorType = rz_an_type_editor_text then Self.RZTextEditor.SetFocus
    else if Self.RZEditorType = rz_an_type_editor_syntax then Self.RZSynEditor.SetFocus
  ;
end;

{2.2}
function TRZANCustomTabSheet.RZClose : Boolean;
begin
  if (Self.RZStatus = rz_an_type_status_Ready) or (Self.RZStatus = rz_an_type_status_Saved) then
  begin
    Self.Free;
    Result := True;
  end
  else if (Self.RZStatus = rz_an_type_status_Modified) then
  begin
    if MessageDlg('Closing Confirmation','Do you want to close before save?',
        mtConfirmation,[mbYes,mbNo],0) = mrYes then
    begin
      Self.Free;
      Result := True;
    end
    else
      Result:= False;
    ;
  end;
end;

{2.3}
procedure TRZANCustomTabSheet.RZOpen (AFileName : TFileName);
begin
  Self.RZFileName := AFileName;
  if (Trim(AFileName) <> '') then
  begin
    if Self.RZEditorType = rz_an_type_editor_text then
      Self.RZTextEditor.RZOpen(Self.RZFileName)
    else if Self.RZEditorType = rz_an_type_editor_syntax then
      Self.RZSynEditor.RZOpen(Self.RZFileName)
    ;
  end;
  Self.RZCaretPosX := 0;
  Self.RZCaretPosY := 1;
  Self.RZStatus := rz_an_type_status_Ready;
end;

{2.4}
procedure TRZANCustomTabSheet.RZSave (AFileName : TFileName);
begin
  if Self.RZEditorType = rz_an_type_editor_text then
    Self.RZTextEditor.RZSave(AFileName)
  else if Self.RZEditorType = rz_an_type_editor_syntax then
    Self.RZSynEditor.RZSave(AFileName)
  ;
  Self.RZStatus := rz_an_type_status_Saved;
  Self.RZFileName := AFileName;
end;

{2.20}
procedure TRZANCustomTabSheet.SetRZTabId (AValue : Byte);
begin
  Self.FRZTabId := AValue;
  if Self.FRZTabId = 0 then Self.Caption := 'Note'
    else Self.Caption := 'Note' + IntToStr(Self.FRZTabId)
  ;
end;

{2.30}
procedure TRZANCustomTabSheet.SetRZFileName (const AValue : TFileName);
begin
  Self.FRZFileName := AValue;
  if AValue <> '' then
  begin
    Self.RZFileExt := ExtractFileExt(Self.RZFileName);
    Self.Caption := ExtractFileName(Self.RZFileName);
  end;
  Self.RZANStatusBar.RZFileName := Self.RZFileName;
end;

{2.31}
procedure TRZANCustomTabSheet.SetRZFileExt (const AValue : string);
begin
  Self.FRZFileExt := AValue;
  if Self.RZFileExt = '.java' then
  begin
    Self.RZLanguage := rz_an_type_lang_Java;
    Self.RZEditorType := rz_an_type_editor_syntax;
  end
  else if Self.RZFileExt = '.pas' then
  begin
    Self.RZLanguage := rz_an_type_lang_Pascal;
    Self.RZEditorType := rz_an_type_editor_syntax;
  end
  else if Self.RZFileExt = '.py' then
  begin
    Self.RZLanguage := rz_an_type_lang_Python;
    Self.RZEditorType := rz_an_type_editor_syntax;
  end
  else
    Self.RZEditorType := rz_an_type_editor_text
  ;
end;

{3. Text Editing}

{3.1}
procedure TRZANCustomTabSheet.RZUndo;
begin
  if Self.RZEditorType = rz_an_type_editor_text then Self.RZTextEditor.RZUndo
    else if Self.RZEditorType = rz_an_type_editor_syntax then Self.RZSynEditor.RZUndo
  ;
  Self.RZStatus := rz_an_type_status_Modified;
end;

{3.2}
procedure TRZANCustomTabSheet.RZRedo;
begin
  if Self.RZEditorType = rz_an_type_editor_text then Self.RZTextEditor.RZRedo
    else if Self.RZEditorType = rz_an_type_editor_syntax then Self.RZSynEditor.RZRedo
  ;
  Self.RZStatus := rz_an_type_status_Modified;
end;

{3.3}
procedure TRZANCustomTabSheet.RZCopy;
begin
  if Self.RZEditorType = rz_an_type_editor_text then Self.RZTextEditor.RZCopy
    else if Self.RZEditorType = rz_an_type_editor_syntax then Self.RZSynEditor.RZCopy
  ;
end;

{3.4}
procedure TRZANCustomTabSheet.RZCut;
begin
  if Self.RZEditorType = rz_an_type_editor_text then Self.RZTextEditor.RZCut
    else if Self.RZEditorType = rz_an_type_editor_syntax then Self.RZSynEditor.RZCut
  ;
  Self.RZStatus := rz_an_type_status_Modified;
end;

{3.5}
procedure TRZANCustomTabSheet.RZPaste;
begin
  if Self.RZEditorType = rz_an_type_editor_text then Self.RZTextEditor.RZPaste
    else if Self.RZEditorType = rz_an_type_editor_syntax then Self.RZSynEditor.RZPaste
  ;
  Self.RZStatus := rz_an_type_status_Modified;
end;

{3.6}
procedure TRZANCustomTabSheet.RZSelectAll;
begin
  if Self.RZEditorType = rz_an_type_editor_text then Self.RZTextEditor.RZSelectAll
    else if Self.RZEditorType = rz_an_type_editor_syntax then Self.RZSynEditor.RZSelectAll
  ;
end;

{3.80}
procedure TRZANCustomTabSheet.SetRZStatus (const AValue: rz_an_type_Status);
begin
  Self.FRZStatus := AValue;
  if (Self.RZStatus = rz_an_type_status_Ready) or (Self.RZStatus = rz_an_type_status_Saved) then
    Self.ImageIndex := rz_an_var_ImageIndex_Saved
  else
    Self.ImageIndex := rz_an_var_ImageIndex_Modified
  ;
  Self.RZANStatusBar.RZStatus := Self.RZStatus;
end;

{5. Editor Format}

{5.20}
procedure TRZANCustomTabSheet.SetRZLanguage (AValue : rz_an_type_Language);
begin
  Self.FRZLanguage := AValue;
  if Self.RZANStatusBar <> nil then Self.RZANStatusBar.RZLanguage := Self.RZLanguage;
end;

{5.30}
procedure TRZANCustomTabSheet.RZSetNewLanguage (ALanguage : rz_an_type_Language);
var
  i : Word;
begin
  if ALanguage = rz_an_type_lang_Text then
  begin
    if Self.RZEditorType = rz_an_type_editor_syntax then
    begin
      Self.RZTextEditor.Lines := Self.RZSynEditor.Lines;
      Self.RZSynEditor.Lines.Clear;
      Self.RZEditorType := rz_an_type_editor_text;
    end;
  end
  else
  begin
    if Self.RZEditorType = rz_an_type_editor_text then
    begin
      for i := 0 to Self.RZTextEditor.Lines.Count - 1 do
        Self.RZSynEditor.Lines.Add(Self.RZTextEditor.Lines[i])
      ;
      Self.RZTextEditor.Lines.Clear;
      Self.RZEditorType := rz_an_type_editor_syntax;
      Self.RZSynEditor.RZLanguage := ALanguage;
    end
    else if Self.RZEditorType = rz_an_type_editor_syntax then
    begin
      Self.RZSynEditor.RZLanguage := ALanguage;
    end;
  end;
  Self.RZLanguage := ALanguage;
end;

{5.30}
procedure TRZANCustomTabSheet.SetRZEditorType (AValue : rz_an_type_Editor);
begin
  Self.FRZEditorType := AValue;
  if Self.RZEditorType = rz_an_type_editor_text then
  begin
    Self.RZTextEditor.Visible := True;
    Self.RZSynEditor.Visible := False;
  end
  else if Self.RZEditorType = rz_an_type_editor_syntax then
  begin
    Self.RZSynEditor.Visible := True;
    Self.RZTextEditor.Visible := False;
  end;
end;

{6. Editor Style}

{6.0}
procedure TRZANCustomTabSheet.RZSetNewStyle (AStyle : rz_an_type_Style);
begin
  if AStyle = rz_an_type_style_Normal then
  begin
    Self.RZTextEditor.Color := clWhite;
    Self.RZTextEditor.Font.Color := clBlack;
    Self.RZSynEditor.Color := clWhite;
    Self.RZSynEditor.Font.Color := clBlack;
  end
  else if AStyle = rz_an_type_style_Dark then
  begin
    Self.RZTextEditor.Color := clBlack;
    Self.RZTextEditor.Font.Color := clWhite;
    Self.RZSynEditor.Color := clBlack;
    Self.RZSynEditor.Font.Color := clWhite;
  end;
end;

{7. Caret}

{7.1}
procedure TRZANCustomTabSheet.SetRZCaretPosX (const AValue : Integer);
begin
  Self.FRZCaretPosX := AValue;
  if Self.RZANStatusBar <> nil then Self.RZANStatusBar.RZCaretPosX := Self.RZCaretPosX;
end;

{7.2}
procedure TRZANCustomTabSheet.SetRZCaretPosY (const AValue : Integer);
begin
  Self.FRZCaretPosY := AValue;
  if Self.RZANStatusBar <> nil then Self.RZANStatusBar.RZCaretPosY := Self.RZCaretPosY;
end;

end.

