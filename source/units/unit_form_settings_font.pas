// This file is part of Anoa Notepad project
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
// 1. Integrates source code from Anoa Notepad.
// 2. Integrates/includes/aggregates Anoa Notepad into a proprietary executable
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

unit unit_form_settings_font;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, rz_an_cmp_pagecontrol,
  rz_an_cmp_tabsheet;

type

  { TFormSettingsFont }

  TFormSettingsFont = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ComboBoxName: TComboBox;
    ComboBoxSize: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private

  public

  end;

var
  FormSettingsFont: TFormSettingsFont;

implementation

{$R *.lfm}

uses
  unit_form_main;

{ TFormSettingsFont }

procedure TFormSettingsFont.FormCreate(Sender: TObject);
var
  LActiveTabSheet : TRZANTabSheet;
  LActiveFontName : string;
  LActiveFontSize : Byte;
begin
  LActiveTabSheet := (FormMain.RZANPageControlMain.Pages[FormMain.RZANPageControlMain.PageIndex] as TRZANTabSheet);
  if LActiveTabSheet.RZTextEditor.Visible then
  begin
    LActiveFontName := LActiveTabSheet.RZTextEditor.Font.Name;
    LActiveFontSize := LActiveTabSheet.RZTextEditor.Font.Size;
  end
  else if LActiveTabSheet.RZSynEditor.Visible then
  begin
    LActiveFontName := LActiveTabSheet.RZSynEditor.Font.Name;
    LActiveFontSize := LActiveTabSheet.RZSynEditor.Font.Size;
  end;
  Self.ComboBoxName.Items.Assign(Screen.Fonts);
  Self.ComboBoxName.Text := LActiveFontName;
  Self.ComboBoxSize.Text := IntToStr(LActiveFontSize);
end;

procedure TFormSettingsFont.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FormMain.Enabled := True;
  Self.Release;
end;

procedure TFormSettingsFont.ButtonOKClick(Sender: TObject);
var
  LActiveTabSheet : TRZANTabSheet;
  LSelectedFontName : string;
  LSelectedFontSize : Byte;
begin
  LActiveTabSheet := (FormMain.RZANPageControlMain.Pages[FormMain.RZANPageControlMain.PageIndex] as TRZANTabSheet);
  LSelectedFontName := Self.ComboBoxName.Text;
  LSelectedFontSize := StrToInt(Self.ComboBoxSize.Text);
  if LActiveTabSheet.RZTextEditor.Visible then
  begin
    LActiveTabSheet.RZTextEditor.Font.Name := LSelectedFontName;
    LActiveTabSheet.RZTextEditor.Font.Size := LSelectedFontSize;
  end
  else if LActiveTabSheet.RZSynEditor.Visible then
  begin
    LActiveTabSheet.RZSynEditor.Font.Name := LSelectedFontName;
    LActiveTabSheet.RZSynEditor.Font.Size := LSelectedFontSize;
  end;
  FormMain.Enabled := True;
  Self.Release;
end;

procedure TFormSettingsFont.ButtonCancelClick(Sender: TObject);
begin
  FormMain.Enabled := True;
  Self.Release;
end;

end.

