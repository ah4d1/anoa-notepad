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

unit unit_form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  Menus,
  ComCtrls,
  PrintersDlgs,
  LCLType,
  ExtCtrls,
  Grids,
  FileUtil,
  RichMemo,
  rz_anoa_notepad,
  rz_an_cmp_pagecontrol,
  rz_an_cmp_richmemo,
  rz_an_cmp_statusbar
, Types
  , Messages;

type

  { TFormMain }

  TFormMain = class(TForm)
    ImageListMain: TImageList;
    MainMenuMemo: TMainMenu;
    MenuItemPopupDeleteTab: TMenuItem;
    MenuItemPopupAddTab: TMenuItem;
    MenuItemHelpGPL: TMenuItem;
    MenuItemHelpLicense: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemSettingsStyle: TMenuItem;
    MenuItemMemoSettingsEditorStyleNormal: TMenuItem;
    MenuItemMemoSettingsEditorStyleDark: TMenuItem;
    MenuItemEditSelectAll: TMenuItem;
    MenuItemEditPaste: TMenuItem;
    MenuItemEditCut: TMenuItem;
    MenuItemEditCopy: TMenuItem;
    MenuItemSettingsLanguageText: TMenuItem;
    MenuItemMemoLanguagePython: TMenuItem;
    MenuItemFileExit: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemMemoSettings: TMenuItem;
    MenuItemSettingsLanguage: TMenuItem;
    MenuItemSettingsLanguagePascal: TMenuItem;
    MenuItemSettingsLanguageJava: TMenuItem;
    MenuItemFilePrint: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItemEditRedo: TMenuItem;
    MenuItemEditUndo: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemFileSaveAs: TMenuItem;
    MenuItemFileSave: TMenuItem;
    MenuItemSettingsFont: TMenuItem;
    MenuItemFileNew: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemHelpAbout: TMenuItem;
    PopupMenuPageControl: TPopupMenu;
    PrinterSetupDialogMain: TPrinterSetupDialog;
    RZANPageControlMain: TRZANPageControl;
    RZANStatusBarMain: TRZANStatusBar;
    TimerFormCreate: TTimer;
    ToolBarMemo: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    {Application}
    procedure FormCreate(Sender: TObject);
    procedure TimerFormCreateTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    {MainMenu}
    {MainMenu - File}
    procedure MenuItemFileNewClick(Sender: TObject);
    procedure MenuItemFileOpenClick(Sender: TObject);
    procedure MenuItemFileSaveClick(Sender: TObject);
    procedure MenuItemFileSaveAsClick(Sender: TObject);
    procedure MenuItemFilePrintClick(Sender: TObject);
    procedure MenuItemFileExitClick(Sender: TObject);
    {MainMenu - Edit}
    procedure MenuItemEditUndoClick(Sender: TObject);
    procedure MenuItemEditRedoClick(Sender: TObject);
    procedure MenuItemEditCopyClick(Sender: TObject);
    procedure MenuItemEditCutClick(Sender: TObject);
    procedure MenuItemEditPasteClick(Sender: TObject);
    procedure MenuItemEditSelectAllClick(Sender: TObject);
    {MainMenu - Settings}
    procedure MenuItemSettingsFontClick(Sender: TObject);
    procedure MenuItemSettingsLanguageTextClick(Sender: TObject);
    procedure MenuItemSettingsLanguageJavaClick(Sender: TObject);
    procedure MenuItemSettingsLanguagePascalClick(Sender: TObject);
    procedure MenuItemMemoLanguagePythonClick(Sender: TObject);
    procedure MenuItemMemoSettingsEditorStyleNormalClick(Sender: TObject);
    procedure MenuItemMemoSettingsEditorStyleDarkClick(Sender: TObject);
    {MainMenu - Help}
    procedure MenuItemHelpAboutClick(Sender: TObject);
    procedure MenuItemHelpLicenseClick(Sender: TObject);
    procedure MenuItemHelpGPLClick(Sender: TObject);
    {PopupMenu}
    procedure MenuItemPopupAddTabClick(Sender: TObject);
    procedure MenuItemPopupDeleteTabClick(Sender: TObject);
  private
    {Private Declaration}
  public
    {Public Declaration}
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  unit_form_settings_font
  , unit_form_readme
  , rz_an_pas_var
;

{ TFormMain }

{Application}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Self.TimerFormCreate.Enabled := True;
end;

procedure TFormMain.TimerFormCreateTimer(Sender: TObject);
begin
  Self.RZANPageControlMain.RZAddSheet('');
  Self.TimerFormCreate.Enabled := False;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.Terminate;
end;

{MainMenu - File}

procedure TFormMain.MenuItemFileNewClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZAddSheet('');
end;

procedure TFormMain.MenuItemFileOpenClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZOpenFile;
end;

procedure TFormMain.MenuItemFileSaveClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSaveFile;
end;

procedure TFormMain.MenuItemFileSaveAsClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSaveAsFile;
end;

procedure TFormMain.MenuItemFilePrintClick(Sender: TObject);
var
  LPrintParameters: TPrintParams;
begin
  if not Self.PrinterSetupDialogMain.Execute then Exit;
  LPrintParameters.JobTitle := 'Anoa-Notepad Printing Job';
  LPrintParameters.Margins.Top := 30;
  LPrintParameters.Margins.Bottom := 30;
  LPrintParameters.Margins.Left := 30;
  LPrintParameters.Margins.Right := 30;
  // Self.RZANPageControlMain.RZANTabSheet[FormMain.RZANPageControlMain.RZANTabId].Editor.Print(LPrintParameters);
end;

procedure TFormMain.MenuItemFileExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

{MainMenu - Edit}

procedure TFormMain.MenuItemEditUndoClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZUndo;
end;

procedure TFormMain.MenuItemEditRedoClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZRedo;
end;

procedure TFormMain.MenuItemEditCopyClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZCopy;
end;

procedure TFormMain.MenuItemEditCutClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZCut;
end;

procedure TFormMain.MenuItemEditPasteClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZPaste;
end;

procedure TFormMain.MenuItemEditSelectAllClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSelectAll;
end;

{MainMenu - Settings}

procedure TFormMain.MenuItemSettingsFontClick(Sender: TObject);
begin
  FormSettingsFont := TFormSettingsFont.Create(Self);
  FormSettingsFont.Show;
  Self.Enabled := False;
end;

procedure TFormMain.MenuItemSettingsLanguageTextClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewLanguage(rz_an_type_lang_Text);
end;

procedure TFormMain.MenuItemSettingsLanguageJavaClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewLanguage(rz_an_type_lang_Java);
end;

procedure TFormMain.MenuItemSettingsLanguagePascalClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewLanguage(rz_an_type_lang_Pascal);
end;

procedure TFormMain.MenuItemMemoLanguagePythonClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewLanguage(rz_an_type_lang_Python);
end;

procedure TFormMain.MenuItemMemoSettingsEditorStyleNormalClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewStyle(rz_an_type_style_Normal);
end;

procedure TFormMain.MenuItemMemoSettingsEditorStyleDarkClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewStyle(rz_an_type_style_Dark);
end;

{MainMenu - Help}

procedure TFormMain.MenuItemHelpAboutClick(Sender: TObject);
begin
  FormReadme := TFormReadme.Create(Self);
  FormReadme.Caption := 'About';
  FormReadme.MemoMain.Lines := FormReadme.MemoAbout.Lines;
  FormReadme.Show;
end;

procedure TFormMain.MenuItemHelpLicenseClick(Sender: TObject);
begin
  FormReadme := TFormReadme.Create(Self);
  FormReadme.Caption := 'License';
  FormReadme.MemoMain.Lines := FormReadme.MemoLicense.Lines;
  FormReadme.Show;
end;

procedure TFormMain.MenuItemHelpGPLClick(Sender: TObject);
begin
  FormReadme := TFormReadme.Create(Self);
  FormReadme.Caption := 'GPL';
  FormReadme.MemoMain.Lines := FormReadme.MemoGPL.Lines;
  FormReadme.Show;
end;

{Popup}

procedure TFormMain.MenuItemPopupAddTabClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZAddSheet('');
end;

procedure TFormMain.MenuItemPopupDeleteTabClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZANCloseSheet;
end;

end.

