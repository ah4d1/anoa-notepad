{********************************************************************************

This file is part of Anoa-Notepad project.

Anoa-Notepad is a free and open source text and code editor for programmers,
software developers, software engineers, and common users.

Copyright(C)2019-2020 Ahadi Aprianto (ahadi.aprianto@gmail.com)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA

********************************************************************************}

unit rz_an_cmp_pagecontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Controls, Dialogs, Graphics, Forms, Menus, RichMemo,
  SynEdit, rz_an_cmp_statusbar, rz_an_cmp_opendialog, rz_an_cmp_savedialog, rz_an_pas_var,
  rz_an_cmp_tabsheet, rz_an_cmp_richmemo, rz_an_cmp_synedit;

type

  TRZANCustomPageControl = class(TPageControl)
  private
    {1. Application}
    FRZPopupMenu : TPopupMenu;
    {1.1.5}
    FRZStatusBar : TRZANStatusBar;
    {2. File Operation}
    {2.20}
    FRZTabId : Byte;
    {2.21}
    FRZTabInit : Boolean;
    {2.6}
    FRZActiveTabSheet : TRZANTabSheet;
  protected
    {2. File Operation}
    {2.6}
    function GetRZActiveTabSheet : TRZANTabSheet;
    {1. Application}
    property RZPopupMenu : TPopupMenu read FRZPopupMenu write FRZPopupMenu;
    property RZANStatusBar : TRZANStatusBar read FRZStatusBar write FRZStatusBar;
    {2. File Operation}
    {2.20}
    property RZTabId : Byte read FRZTabId write FRZTabId;
    {2.21}
    property RZTabInit : Boolean read FRZTabInit write FRZTabInit;
    {2.6}
    property RZActiveTabSheet : TRZANTabSheet read GetRZActiveTabSheet;
  public
    {1. Application}
    {1.3}
    RZANOpenDialog : TRZANOpenDialog;
    {1.4}
    RZANSaveDialog : TRZANSaveDialog;
    {1. Application}
    {1.1}
    constructor Create (AOwner : TComponent); override;
    {2. File Operation}
    {2.1}
    procedure RZAddSheet; overload;
    procedure RZAddSheet (AFileName : TFileName); overload;
    {2.2}
    procedure RZCloseSheet;
    {2.3}
    procedure RZOpenFile;
    {2.4}
    procedure RZSaveFile;
    procedure RZSaveAsFile;
    {2.5}
    procedure RZPrint (APrintParameters: TPrintParams);
    {2.7}
    procedure Change; override;
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
    procedure RZSetNewEditorFormat (AEditorFormat : rz_an_type_EditorFormat);
    {6. Editor Style}
    {6.0}
    procedure RZSetNewStyle (AStyle : rz_an_type_Style);
    {*}
    function RZSpecialCharactersShow : Boolean;
    function RZSpecialCharactersHide : Boolean;
  end;

  TRZANPageControl = class(TRZANCustomPageControl)
  published
    {1. Application}
    property RZPopupMenu;
    {1.1.5}
    property RZANStatusBar;
  end;

implementation

{1. Application}

{1.1.1}
constructor TRZANCustomPageControl.Create (AOwner : TComponent);
begin
  {1. Application}
  {1.1}
  inherited Create(AOwner);
  Self.ImagesWidth := 24;
  {1.3}
  Self.RZANOpenDialog := TRZANOpenDialog.Create(Self);
  {1.4}
  Self.RZANSaveDialog := TRZANSaveDialog.Create(Self);
  {2. File Operation}
  {2.20}
  Self.RZTabId := 0;
  {2.21}
  Self.RZTabInit := True;
end;

{2. File Operation}

{2.1}
procedure TRZANCustomPageControl.RZAddSheet;
begin
  Self.RZAddSheet('');
end;

{2.1}
procedure TRZANCustomPageControl.RZAddSheet (AFileName : TFileName);
begin
  with TRZANTabSheet.Create(Self) do
  begin
    Parent := Self;
    RZTabId := Self.RZTabId;
    RZPopupMenu := Self.RZPopupMenu;
    RZANStatusBar := Self.RZANStatusBar;
    RZOpen(AFileName);
    Show;
    Self.RZTabId := Self.RZTabId + 1;
  end;
end;

{2.2}
procedure TRZANCustomPageControl.RZCloseSheet;
begin
  if Self.PageCount > 1 then
    Self.RZActiveTabSheet.RZClose
  else
  begin
    if Self.RZActiveTabSheet.RZClose then Self.RZAddSheet;
  end;
end;

{2.3}
procedure TRZANCustomPageControl.RZOpenFile;
begin
  Self.RZANOpenDialog.FilterIndex := Ord(Self.RZActiveTabSheet.RZEditorFormat) + 1; // Filter Index begin at 1
  if Self.RZANOpenDialog.Execute then
  begin
    if Self.RZTabInit then
      Self.RZActiveTabSheet.RZOpen(Self.RZANOpenDialog.FileName)
    else
      Self.RZAddSheet(Self.RZANOpenDialog.FileName);
    ;
    Self.RZTabInit := False;
  end;
end;

{2.4}
procedure TRZANCustomPageControl.RZSaveFile;
begin
  Self.RZANSaveDialog.FilterIndex := Ord(Self.RZActiveTabSheet.RZEditorFormat) + 1; // Filter Index begin at 1
  if Trim(Self.RZActiveTabSheet.RZFileName) <> '' then
    Self.RZActiveTabSheet.RZSave(Self.RZActiveTabSheet.RZFileName)
  else
  begin
    if Self.RZANSaveDialog.Execute then
    begin
      Self.RZActiveTabSheet.RZSave(Self.RZANSaveDialog.FileName);
    end;
  end;
end;

{2.4}
procedure TRZANCustomPageControl.RZSaveAsFile;
begin
  Self.RZANSaveDialog.FilterIndex := Ord(Self.RZActiveTabSheet.RZEditorFormat) + 1; // Filter Index begin at 1
  if Self.RZANSaveDialog.Execute then
  begin
    Self.RZActiveTabSheet.RZSave(Self.RZANSaveDialog.FileName);
  end;
end;

{2.5}
procedure TRZANCustomPageControl.RZPrint (APrintParameters: TPrintParams);
begin
  if Self.RZActiveTabSheet.RZEditorType = rz_an_type_editortype_text then
    Self.RZActiveTabSheet.RZTextEditor.Print(APrintParameters)
  else
    MessageDlg('Information','Change Editor Format to Text then Print',mtInformation,[mbOK],0)
  ;
end;

{2.6}
function TRZANCustomPageControl.GetRZActiveTabSheet : TRZANTabSheet;
begin
  Self.FRZActiveTabSheet := (Self.Pages[Self.PageIndex] as TRZANTabSheet);
  Result := Self.FRZActiveTabSheet;
end;

{2.7}
procedure TRZANCustomPageControl.Change;
begin
  if Self.RZActiveTabSheet.RZEditorType = rz_an_type_editortype_text then
  begin
    Self.RZActiveTabSheet.RZANStatusBar.RZStatus := Self.RZActiveTabSheet.RZStatus;
    Self.RZActiveTabSheet.RZANStatusBar.RZEditorFormat := Self.RZActiveTabSheet.RZEditorFormat;
    Self.RZActiveTabSheet.RZANStatusBar.RZCaretPosX := Self.RZActiveTabSheet.RZTextEditor.RZCaretPosX;
    Self.RZActiveTabSheet.RZANStatusBar.RZCaretPosY := Self.RZActiveTabSheet.RZTextEditor.RZCaretPosY;
    Self.RZActiveTabSheet.RZANStatusBar.RZFileName := Self.RZActiveTabSheet.RZFileName;
  end
  else if Self.RZActiveTabSheet.RZEditorType = rz_an_type_editortype_syntax then
  begin
    Self.RZActiveTabSheet.RZANStatusBar.RZStatus := Self.RZActiveTabSheet.RZStatus;
    Self.RZActiveTabSheet.RZANStatusBar.RZEditorFormat := Self.RZActiveTabSheet.RZEditorFormat;
    Self.RZActiveTabSheet.RZANStatusBar.RZCaretPosX := Self.RZActiveTabSheet.RZSynEditor.RZCaretPosX;
    Self.RZActiveTabSheet.RZANStatusBar.RZCaretPosY := Self.RZActiveTabSheet.RZSynEditor.RZCaretPosY;
    Self.RZActiveTabSheet.RZANStatusBar.RZFileName := Self.RZActiveTabSheet.RZFileName;
  end;
  inherited;
end;

{3. Text Editing}

{3.1}
procedure TRZANCustomPageControl.RZUndo;
begin
  Self.RZActiveTabSheet.RZUndo;
end;

{3.2}
procedure TRZANCustomPageControl.RZRedo;
begin
  Self.RZActiveTabSheet.RZRedo;
end;

{3.3}
procedure TRZANCustomPageControl.RZCopy;
begin
  Self.RZActiveTabSheet.RZCopy;
end;

{3.4}
procedure TRZANCustomPageControl.RZCut;
begin
  Self.RZActiveTabSheet.RZCut;
end;

{3.5}
procedure TRZANCustomPageControl.RZPaste;
begin
  Self.RZActiveTabSheet.RZPaste;
end;

{3.6}
procedure TRZANCustomPageControl.RZSelectAll;
begin
  Self.RZActiveTabSheet.RZSelectAll;
end;

{5. Editor Format}

{5.20}
procedure TRZANCustomPageControl.RZSetNewEditorFormat (AEditorFormat : rz_an_type_EditorFormat);
begin
  Self.RZActiveTabSheet.RZChangeEditorFormat(AEditorFormat);
end;

{6. Editor Style}

{6.0}
procedure TRZANCustomPageControl.RZSetNewStyle (AStyle : rz_an_type_Style);
begin
  Self.RZActiveTabSheet.RZEditorStyle := AStyle;
end;

{*}

function TRZANCustomPageControl.RZSpecialCharactersShow : Boolean;
begin
  if Self.RZActiveTabSheet.RZEditorType = rz_an_type_editortype_syntax then
  begin
    Self.RZActiveTabSheet.RZSynEditor.Options := Self.RZActiveTabSheet.RZSynEditor.Options
      + [eoShowSpecialChars];
    Result := True;
  end
  else
  begin
    MessageDlg('Ooops!!','Sorry, this function is not available for "Text" Editor Format',
      mtInformation,[mbOK],0);
    Result := False;
  end;
end;

function TRZANCustomPageControl.RZSpecialCharactersHide : Boolean;
begin
  if Self.RZActiveTabSheet.RZEditorType = rz_an_type_editortype_syntax then
  begin
    Self.RZActiveTabSheet.RZSynEditor.Options := Self.RZActiveTabSheet.RZSynEditor.Options
      - [eoShowSpecialChars];
    Result := True;
  end
  else
  begin
    MessageDlg('Ooops!!','Sorry, this function is not available for "Text" Editor Format',
      mtInformation,[mbOK],0);
    Result := False;
  end;
end;


end.

