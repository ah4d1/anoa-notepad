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
  rz_an_pas_opendialog, rz_an_pas_savedialog, rz_an_pas_reserved_word, rz_an_pas_language, rz_an_pas_var,
  rz_an_pas_tabsheet;

type

  TRZANCustomPageControl = class(TPageControl)
  private
    {Properties - StatusBar}
    FActiveStatus : rz_an_type_Status;
    FActiveLanguage : rz_an_type_Language;
    FActiveCaretPosX : Integer;
    FActiveCaretPosY : Integer;
    FActiveFileName : TFileName;
    FActiveFileNameOnly : string;
    FActiveFileNameExt : string;
    {Properties - Misc}
    FPageInit : Boolean;
    FPageIndex : Byte;
    FPageCount : Byte;
    FDoParsing :Boolean;
    {Sub-Components}
    FStatusBar : TRZANStatusBar;
  protected
    {Events - StatusBar}
    function GetActiveStatus : rz_an_type_Status;
    procedure SetActiveStatus (const AValue: rz_an_type_Status);
    function GetActiveCaretPosX : Integer;
    procedure SetActiveCaretPosX (const AValue: Integer);
    function GetActiveCaretPosY : Integer;
    procedure SetActiveCaretPosY (const AValue: Integer);
    function GetActiveLanguage : rz_an_type_Language;
    procedure SetActiveLanguage (const AValue: rz_an_type_Language);
    function GetActiveFileName : TFileName;
    procedure SetActiveFileName (const AValue: TFileName);
    function GetActiveFileNameOnly : string;
    procedure SetActiveFileNameOnly (const AValue: string);
    function GetActiveFileNameExt : string;
    procedure SetActiveFileNameExt (const AValue: string);
    {Events - Misc}
    function GetPageIndex : Byte;
    procedure SetPageIndex (const AValue: Byte);
    function GetDoParsing : Boolean;
    procedure SetDoParsing (const AValue: Boolean);
    {Properties - StatusBar}
    property ActiveStatus : rz_an_type_Status read GetActiveStatus write SetActiveStatus;
    property ActiveCaretPosX : Integer read GetActiveCaretPosX write SetActiveCaretPosX;
    property ActiveCaretPosY : Integer read GetActiveCaretPosY write SetActiveCaretPosY;
    property ActiveLanguage : rz_an_type_Language read GetActiveLanguage write SetActiveLanguage;
    property ActiveFileName : TFileName read GetActiveFileName write SetActiveFileName;
    property ActiveFileNameOnly : string read GetActiveFileNameOnly write SetActiveFileNameOnly;
    property ActiveFileNameExt : string read GetActiveFileNameExt write SetActiveFileNameExt;
    {Properties - Misc}
    property PageInit : Boolean read FPageInit write FPageInit;
    property PageIndex : Byte read FPageIndex write FPageIndex;
    property PageCount : Byte read FPageCount write FPageCount;
    property DoParsing : Boolean read GetDoParsing write SetDoParsing;
    {Sub-Components}
    property StatusBar : TRZANStatusBar read FStatusBar write FStatusBar;
  public
    {Sub-Components}
    OpenDialog : TRZANOpenDialog;
    SaveDialog : TRZANSaveDialog;
    TabSheet : array[0..100] of TRZANTabSheet;
    {Methods}
    constructor Create (AOwner : TComponent); override;
    procedure AddSheet;
    procedure CloseActiveSheet;
    procedure OpenFile;
    procedure SaveFile;
    procedure SaveAsFile;
    procedure SetNewLanguage (ALanguage : rz_an_type_Language);
    procedure SubComponentUpdate;
    procedure Undo;
    procedure Redo;
    procedure Copy;
    procedure Cut;
    procedure Paste;
    procedure SelectAll;
    {Events}
    procedure Change; override;
  end;

  TRZANPageControl = class(TRZANCustomPageControl)
  published
    property StatusBar;
  end;

implementation

{Methods}

constructor TRZANCustomPageControl.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  {Sub-Components}
  Self.OpenDialog := TRZANOpenDialog.Create(Self);
  Self.SaveDialog := TRZANSaveDialog.Create(Self);
  {Properties}
  Self.ImagesWidth := 24;
  Self.PageInit := True;
  Self.PageIndex := -1;
  Self.PageCount := 0;
end;

procedure TRZANCustomPageControl.AddSheet;
begin
  {Initialize}
  Self.PageIndex := Self.PageIndex + 1;
  {Create New Tab}
  Self.TabSheet[Self.PageIndex] := TRZANTabSheet.Create(Self);
  Self.TabSheet[Self.PageIndex].Parent := Self;
  if Self.PageIndex > 0 then
    TabSheet[Self.PageIndex].Caption := 'Note' + IntToStr(Self.PageIndex)
  else
    TabSheet[Self.PageIndex].Caption := 'Note'
  ;
  Self.TabSheet[Self.PageIndex].ImageIndex := 8;
  Self.TabSheet[Self.PageIndex].Name := 'TabSheet' + IntToStr(Self.PageIndex);
  {Sub-Components}
  Self.TabSheet[Self.PageIndex].StatusBar := Self.StatusBar;
  {Finalize}
  Self.ActiveStatus := rz_an_type_status_Ready;
  Self.ActivePage := Self.TabSheet[Self.PageIndex];
  Self.PageCount := Self.PageCount + 1;
end;

procedure TRZANCustomPageControl.CloseActiveSheet;
begin
  if Self.PageCount > 1 then
  begin
    Self.Pages[Self.ActivePageIndex].Free;
    Self.PageCount := Self.PageCount - 1;
  end;
end;

procedure TRZANCustomPageControl.OpenFile;
begin
  if Self.OpenDialog.Execute then
  begin
    if not(Self.PageInit) then Self.AddSheet;
    Self.ActiveFileName := Self.OpenDialog.FileName;
    Self.TabSheet[Self.TabIndex].OpenFile(Self.ActiveFileName);
    Self.ActiveStatus := rz_an_type_status_Ready;
    Self.PageInit := False;
  end;
end;

procedure TRZANCustomPageControl.SaveFile;
begin
  if Trim(Self.ActiveFileName) = '' then
  begin
    Self.SaveAsFile
  end
  else
  begin
    Self.TabSheet[Self.TabIndex].SaveFile(Self.ActiveFileName);
    Self.ActiveStatus := rz_an_type_status_Saved;
  end;
end;

procedure TRZANCustomPageControl.SaveAsFile;
begin
  if Self.SaveDialog.Execute then
  begin
    Self.ActiveFileName := Self.SaveDialog.FileName;
    Self.TabSheet[Self.TabIndex].SaveFile(Self.ActiveFileName);
    Self.ActiveStatus := rz_an_type_status_Saved;
  end;
end;

procedure TRZANCustomPageControl.SetNewLanguage (ALanguage : rz_an_type_Language);
begin
  Self.DoParsing := True;
  Self.ActiveLanguage := ALanguage;
end;

procedure TRZANCustomPageControl.SubComponentUpdate;
begin
  Self.TabSheet[Self.TabIndex].SubComponentUpdate;
  Self.ActiveStatus := Self.TabSheet[Self.TabIndex].Status;
  Self.ActiveCaretPosX := Self.TabSheet[Self.TabIndex].CaretPosX;
  Self.ActiveCaretPosY := Self.TabSheet[Self.TabIndex].CaretPosY;
end;

{Properties - StatusBar}

function TRZANCustomPageControl.GetActiveStatus : rz_an_type_Status;
begin
  Result := Self.FActiveStatus;
end;

procedure TRZANCustomPageControl.SetActiveStatus (const AValue : rz_an_type_Status);
begin
  Self.FActiveStatus := AValue;
  if Self.ActiveStatus = rz_an_type_status_Modified then Self.PageInit := False;
  Self.TabSheet[Self.TabIndex].Status := Self.ActiveStatus;
end;

function TRZANCustomPageControl.GetActiveCaretPosX : Integer;
begin
  Result := Self.FActiveCaretPosX;
end;

procedure TRZANCustomPageControl.SetActiveCaretPosX (const AValue : Integer);
begin
  Self.FActiveCaretPosX := AValue;
  Self.TabSheet[Self.TabIndex].CaretPosX := Self.ActiveCaretPosX;
end;

function TRZANCustomPageControl.GetActiveCaretPosY : Integer;
begin
  Result := Self.FActiveCaretPosY;
end;

procedure TRZANCustomPageControl.SetActiveCaretPosY (const AValue : Integer);
begin
  Self.FActiveCaretPosY := AValue;
  Self.TabSheet[Self.TabIndex].CaretPosY := Self.ActiveCaretPosY;
end;

function TRZANCustomPageControl.GetActiveLanguage : rz_an_type_Language;
begin
  Result := Self.FActiveLanguage;
end;

procedure TRZANCustomPageControl.SetActiveLanguage (const AValue: rz_an_type_Language);
begin
  Self.FActiveLanguage := AValue;
  Self.TabSheet[Self.TabIndex].Language := Self.ActiveLanguage;
end;

function TRZANCustomPageControl.GetActiveFileName : TFileName;
begin
  Result := Self.FActiveFileName;
end;

procedure TRZANCustomPageControl.SetActiveFileName (const AValue: TFileName);
begin
  Self.FActiveFileName := AValue;
  Self.ActiveFileNameOnly := ExtractFileName(Self.ActiveFileName);
  Self.ActiveFileNameExt := ExtractFileExt(Self.ActiveFileName);
  // TEST : DELETE ?
  Self.TabSheet[Self.TabIndex].FileName := Self.ActiveFileName;
end;

function TRZANCustomPageControl.GetActiveFileNameOnly : string;
begin
  Result := Self.FActiveFileNameOnly;
end;

procedure TRZANCustomPageControl.SetActiveFileNameOnly (const AValue: string);
begin
  Self.FActiveFileNameOnly := AValue;
end;

function TRZANCustomPageControl.GetActiveFileNameExt : string;
begin
  Result := Self.FActiveFileNameExt;
end;

procedure TRZANCustomPageControl.SetActiveFileNameExt (const AValue: string);
begin
  Self.FActiveFileNameExt := AValue;
  if Self.ActiveFileNameExt = '.java' then
  begin
    Self.ActiveLanguage := rz_an_type_lang_Java;
    Self.OpenDialog.FilterIndex := rz_an_var_FileFilterIndex_Java;
    Self.SaveDialog.FilterIndex := rz_an_var_FileFilterIndex_Java;
  end
  else if Self.ActiveFileNameExt = '.pas' then
  begin
    Self.ActiveLanguage := rz_an_type_lang_Pascal;
    Self.OpenDialog.FilterIndex := rz_an_var_FileFilterIndex_Pascal;
    Self.SaveDialog.FilterIndex := rz_an_var_FileFilterIndex_Pascal;
  end
  else if Self.ActiveFileNameExt = '.py' then
  begin
    Self.ActiveLanguage := rz_an_type_lang_Python;
    Self.OpenDialog.FilterIndex := rz_an_var_FileFilterIndex_Python;
    Self.SaveDialog.FilterIndex := rz_an_var_FileFilterIndex_Python;
  end
  else
  begin
    Self.ActiveLanguage := rz_an_type_lang_Text;
    Self.OpenDialog.FilterIndex := rz_an_var_FileFilterIndex_Text;
    Self.SaveDialog.FilterIndex := rz_an_var_FileFilterIndex_Text;
  end;
end;

{Properties - Misc}

function TRZANCustomPageControl.GetPageIndex : Byte;
begin
  Result := Self.FPageIndex;
end;

procedure TRZANCustomPageControl.SetPageIndex (const AValue: Byte);
begin
  Self.FPageIndex := AValue;
end;

function TRZANCustomPageControl.GetDoParsing : Boolean;
begin
  Result := Self.FDoParsing;
end;

procedure TRZANCustomPageControl.SetDoParsing (const AValue: Boolean);
begin
  Self.FDoParsing := AValue;
  Self.TabSheet[Self.TabIndex].DoParsing := Self.DoParsing;
end;

procedure TRZANCustomPageControl.Undo;
begin
  Self.DoParsing := False;
  Self.TabSheet[Self.TabIndex].Undo;
end;

procedure TRZANCustomPageControl.Redo;
begin
  Self.DoParsing := False;
  Self.TabSheet[Self.TabIndex].Redo;
end;

procedure TRZANCustomPageControl.Copy;
begin
  Self.DoParsing := False;
  Self.TabSheet[Self.TabIndex].Copy;
end;

procedure TRZANCustomPageControl.Cut;
begin
  Self.DoParsing := False;
  Self.TabSheet[Self.TabIndex].Cut;
end;

procedure TRZANCustomPageControl.Paste;
begin
  Self.DoParsing := False;
  Self.TabSheet[Self.TabIndex].Paste;
end;

procedure TRZANCustomPageControl.SelectAll;
begin
  Self.DoParsing := False;
  Self.TabSheet[Self.TabIndex].SelectAll;
end;

{Events}

procedure TRZANCustomPageControl.Change;
begin
  Self.ActiveStatus := Self.TabSheet[Self.TabIndex].Status;
  Self.ActiveLanguage := Self.TabSheet[Self.TabIndex].Language;
  Self.ActiveCaretPosX := Self.TabSheet[Self.TabIndex].CaretPosX;
  Self.ActiveCaretPosY := Self.TabSheet[Self.TabIndex].CaretPosY;
  Self.ActiveFileName := Self.TabSheet[Self.TabIndex].FileName;
  Self.DoParsing := False;
  inherited;
end;

end.

