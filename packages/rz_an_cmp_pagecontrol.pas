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

unit rz_an_cmp_pagecontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Controls, Dialogs, Graphics, rz_an_cmp_statusbar,
  rz_an_pas_opendialog, rz_an_pas_savedialog, rz_an_pas_reserved_word, rz_an_pas_language,
  rz_an_pas_var, rz_an_pas_tabsheet, rz_an_pas_tools, rz_an_cmp_richmemo;

type

  TRZANCustomPageControl = class(TPageControl)
  private
    {Properties}
    FRZTabInit : Boolean;
    FRZTabId : Byte;
    FRZDoParsing :Boolean;
    {Sub-Components}
    FRZEditor : TRZANRichMemo;
    FRZStatusBar : TRZANStatusBar;
  protected
    procedure SetRZDoParsing (AValue : Boolean);
    {Properties}
    property RZTabInit : Boolean read FRZTabInit write FRZTabInit;
    property RZTabId : Byte read FRZTabId write FRZTabId;
    property RZDoParsing : Boolean read FRZDoParsing write SetRZDoParsing;
    {Sub-Components}
    property RZEditor : TRZANRichMemo read FRZEditor write FRZEditor;
    property RZANStatusBar : TRZANStatusBar read FRZStatusBar write FRZStatusBar;
  public
    {Sub-Components}
    RZANOpenDialog : TRZANOpenDialog;
    RZANSaveDialog : TRZANSaveDialog;
    {Create}
    constructor Create (AOwner : TComponent); override;
    {Add & Close Sheet}
    procedure RZAddSheet (AFileName : TFileName);
    procedure RZANCloseSheet;
    {Open and Save File}
    procedure RZOpenFile;
    procedure RZSaveFile;
    procedure RZSaveAsFile;
    {Text Editing}
    procedure RZTextEditing (AType : string);
    procedure RZUndo;
    procedure RZRedo;
    procedure RZCopy;
    procedure RZCut;
    procedure RZPaste;
    procedure RZSelectAll;
    {Language}
    procedure RZSetNewLanguage (ALanguage : rz_an_type_Language);
    {Style}
    procedure RZSetNewStyle (AStyle : rz_an_type_Style);
    {Events}
    procedure Change; override;
    {Methods}
    procedure RZGetRZEditor (APage : TTabSheet);
  end;

  TRZANPageControl = class(TRZANCustomPageControl)
  public
    property RZTabId;
    property RZEditor;
  published
    property RZANStatusBar;
  end;

implementation

{Create}

constructor TRZANCustomPageControl.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  {Sub-Components}
  Self.RZANOpenDialog := TRZANOpenDialog.Create(Self);
  Self.RZANSaveDialog := TRZANSaveDialog.Create(Self);
  {Properties}
  Self.ImagesWidth := 24;
  {RZANTabInit will be used to load file to existing editor at first app start}
  Self.RZTabInit := True;
  {RZANTabId will be used to set tab sheet number when added}
  Self.RZTabId := 0;
end;

{Get RZEditor}

procedure TRZANCustomPageControl.RZGetRZEditor (APage : TTabSheet);
var
  i : Byte;
begin
  // The result will be stored at RZEditor property
  for i := 0 to APage.ControlCount - 1 do
  begin
    if (APage.Controls[i].ClassName) = 'TRZANRichMemo' then
      Self.RZEditor := Self.Pages[Self.PageIndex].Controls[i] as TRZANRichMemo
    ;
  end;
end;

{Add & Close Sheet}

procedure TRZANCustomPageControl.RZAddSheet (AFileName : TFileName);
var
  LRZTabSheet : TRZANTabSheet;
begin
  LRZTabSheet := TRZANTabSheet.Create(Self);
  LRZTabSheet.Parent := Self;
  if Self.RZTabId = 0 then LRZTabSheet.Caption := 'Note'
    else LRZTabSheet.Caption := 'Note' + IntToStr(Self.RZTabId)
  ;
  LRZTabSheet.ImageIndex := 8;
  LRZTabSheet.RZEditor.RZANStatusBar := Self.RZANStatusBar;
  if Trim(AFileName) <> '' then
  begin
    LRZTabSheet.RZFileName := AFileName;
    LRZTabSheet.RZEditor.RZOpen(AFileName);
  end
  else
  begin
    LRZTabSheet.RZFileName := '';
  end;
  LRZTabSheet.RZEditor.RZStatus := rz_an_type_status_Ready;
  LRZTabSheet.Show;
  Self.RZTabId := Self.RZTabId + 1;
end;

procedure TRZANCustomPageControl.RZANCloseSheet;
begin
  if Self.PageCount > 1 then Self.Pages[Self.ActivePageIndex].Free;
end;

{Open and Save File}

procedure TRZANCustomPageControl.RZOpenFile;
var
  LFileName : TFileName;
begin
  Self.RZGetRZEditor(Self.Pages[Self.PageIndex]);
  Self.RZANOpenDialog.FilterIndex := Ord(Self.RZEditor.RZLanguage) + 1; // Filter Index begin at 1
  if Self.RZANOpenDialog.Execute then
  begin
    LFileName := Self.RZANOpenDialog.FileName;
    if (Self.RZTabInit) and (Self.RZEditor.RZStatus = rz_an_type_status_Ready) then
    begin
      Self.RZEditor.RZOpen(LFileName);
      Self.Pages[Self.ActivePageIndex].Caption := ExtractFileName(LFileName);
      Self.RZTabInit := False;
    end
    else
      Self.RZAddSheet(LFileName)
    ;
  end;
  Self.RZEditor.RZStatus := rz_an_type_status_Ready;
end;

procedure TRZANCustomPageControl.RZSaveFile;
begin
  Self.RZGetRZEditor(Self.Pages[Self.PageIndex]);
  if Trim(Self.FRZEditor.RZFileName) = '' then
  begin
    Self.RZSaveAsFile
  end
  else
  begin
    Self.RZGetRZEditor(Self.Pages[Self.PageIndex]);
    Self.FRZEditor.RZSave(Self.FRZEditor.RZFileName);
    Self.RZANStatusBar.RZStatus := rz_an_type_status_Saved;
  end;
end;

procedure TRZANCustomPageControl.RZSaveAsFile;
var
  LFileName : TFileName;
begin
  Self.RZGetRZEditor(Self.Pages[Self.PageIndex]);
  Self.RZANSaveDialog.FilterIndex := Ord(Self.FRZEditor.RZLanguage) + 1; // Filter Index begin at 1
  if Self.RZANSaveDialog.Execute then
  begin
    LFileName := Self.RZANSaveDialog.FileName;
    Self.RZGetRZEditor(Self.Pages[Self.PageIndex]);
    Self.RZEditor.RZSave(LFileName);
    Self.Pages[Self.PageIndex].Caption := ExtractFileName(LFileName);
  end;
end;

{Text Editing}

procedure TRZANCustomPageControl.SetRZDoParsing (AValue : Boolean);
begin
  Self.FRZDoParsing := AValue;
  Self.RZGetRZEditor(Self.Pages[Self.PageIndex]);
  Self.RZEditor.RZDoParsing := Self.RZDoParsing;
end;

procedure TRZANCustomPageControl.RZTextEditing (AType : string);
begin
  Self.RZDoParsing := False;
  Self.RZGetRZEditor(Self.Pages[Self.PageIndex]);
  if AType = 'UNDO' then Self.RZEditor.RZUndo
  else if AType = 'REDO' then Self.RZEditor.RZRedo
  else if AType = 'COPY' then Self.RZEditor.RZCopy
  else if AType = 'CUT' then Self.RZEditor.RZCut
  else if AType = 'PASTE' then Self.RZEditor.RZPaste
  else if AType = 'SELECTALL' then Self.RZEditor.RZSelectAll;
  ;
end;

procedure TRZANCustomPageControl.RZUndo;
begin
  Self.RZTextEditing('UNDO');
end;

procedure TRZANCustomPageControl.RZRedo;
begin
  Self.RZTextEditing('REDO');
end;

procedure TRZANCustomPageControl.RZCopy;
begin
  Self.RZTextEditing('COPY');
end;

procedure TRZANCustomPageControl.RZCut;
begin
  Self.RZTextEditing('CUT');
end;

procedure TRZANCustomPageControl.RZPaste;
begin
  Self.RZTextEditing('PASTE');
end;

procedure TRZANCustomPageControl.RZSelectAll;
begin
  Self.RZTextEditing('SELECTALL');
end;

{Language}

procedure TRZANCustomPageControl.RZSetNewLanguage (ALanguage : rz_an_type_Language);
begin
  Self.RZDoParsing := True;
  Self.RZGetRZEditor(Self.Pages[Self.PageIndex]);
  Self.RZEditor.RZLanguage := ALanguage;
end;

{Style}

procedure TRZANCustomPageControl.RZSetNewStyle (AStyle : rz_an_type_Style);
begin
  Self.RZDoParsing := True;
  Self.RZGetRZEditor(Self.Pages[Self.PageIndex]);
  Self.RZEditor.RZStyle := AStyle;
end;

{Events}

procedure TRZANCustomPageControl.Change;
begin
  Self.RZGetRZEditor(Self.Pages[Self.PageIndex]);
  Self.RZANStatusBar.RZStatus := Self.RZEditor.RZStatus;
  Self.RZANStatusBar.RZLanguage := Self.RZEditor.RZLanguage;
  Self.RZANStatusBar.RZCaretPosX := Self.RZEditor.RZCaretPosX;
  Self.RZANStatusBar.RZCaretPosY := Self.RZEditor.RZCaretPosY;
  Self.RZANStatusBar.RZFileName := Self.RZEditor.RZFileName;
  Self.RZDoParsing := False;
  inherited;
end;

end.

