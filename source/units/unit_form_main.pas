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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ComCtrls, PrintersDlgs, LCLType, ExtCtrls, FileUtil, RichMemo,
  rz_an_cmp_pagecontrol, rz_an_cmp_statusbar;

type

  { TFormMain }

  TFormMain = class(TForm)
    {1. Application}
    {1.1}
    MainMenuMemo: TMainMenu;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem7: TMenuItem;
    PanelToolBarAndCloseTab: TPanel;
    PanelToolBar: TPanel;
    PanelCloseTab: TPanel;
    ToolBarMemo: TToolBar;
    ToolButtonDivider1: TToolButton;
    ToolButtonDivider2: TToolButton;
    ToolButtonDivider3: TToolButton;
    ImageListMain: TImageList;
    PopupMenuPageControl: TPopupMenu;
    {1.1.1}
    RZANPageControlMain: TRZANPageControl;
    {1.1.5}
    RZANStatusBarMain: TRZANStatusBar;
    {1.2}
    MenuItemFileExit: TMenuItem;
    {2. File Operation}
    MenuItemFile: TMenuItem;
    {2.1}
    MenuItemFileNew: TMenuItem;
    ToolButtonFileNew: TToolButton;
    MenuItemPopupAddTab: TMenuItem;
    {2.2}
    LabelCloseTab: TLabel;
    MenuItemPopupCloseTab: TMenuItem;
    {2.3}
    MenuItemFileOpen: TMenuItem;
    ToolButtonFileOpen: TToolButton;
    {2.4}
    MenuItemFileSave: TMenuItem;
    MenuItemFileSaveAs: TMenuItem;
    ToolButtonFileSave: TToolButton;
    {2.5}
    MenuItemFilePrint: TMenuItem;
    ToolButtonFilePrint: TToolButton;
    PrinterSetupDialogMain: TPrinterSetupDialog;
    {3. Text Editing}
    MenuItemEdit: TMenuItem;
    {3.1}
    MenuItemEditUndo: TMenuItem;
    ToolButtonEditUndo: TToolButton;
    {3.2}
    MenuItemEditRedo: TMenuItem;
    ToolButtonEditRedo: TToolButton;
    {3.3}
    MenuItemEditCopy: TMenuItem;
    ToolButtonEditCopy: TToolButton;
    {3.4}
    MenuItemEditCut: TMenuItem;
    ToolButtonEditCut: TToolButton;
    {3.5}
    MenuItemEditPaste: TMenuItem;
    ToolButtonEditPaste: TToolButton;
    {3.6}
    MenuItemEditSelectAll: TMenuItem;
    {4. Font Settings}
    {4.1}
    MenuItemSettings: TMenuItem;
    MenuItemSettingsFont: TMenuItem;
    ToolButtonSettingsFont: TToolButton;
    {5. Editor Format}
    MenuItemSettingsLanguage: TMenuItem;
    {5.1}
    MenuItemSettingsLanguageText: TMenuItem;
    {5.2}
    MenuItemSettingsLanguagePython: TMenuItem;
    MenuItemSettingsLanguagePascal: TMenuItem;
    MenuItemSettingsLanguageJava: TMenuItem;
    {6. Editor Style}
    MenuItemSettingsStyle: TMenuItem;
    {6.1}
    MenuItemMemoSettingsEditorStyleNormal: TMenuItem;
    {6.2}
    MenuItemMemoSettingsEditorStyleDark: TMenuItem;
    {90. Help}
    MenuItemHelp: TMenuItem;
    {90.1}
    MenuItemHelpAbout: TMenuItem;
    ToolButtonHepAbout: TToolButton;
    {90.2}
    MenuItemHelpLicense: TMenuItem;
    {90.3}
    MenuItemHelpGPL: TMenuItem;
    {1. Application}
    {1.1}
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    {1.2}
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MenuItemFileExitClick(Sender: TObject);
    {2. File Operation}
    {2.1}
    procedure MenuItemFileNewClick(Sender: TObject);
    procedure MenuItemPopupAddTabClick(Sender: TObject);
    {2.2}
    procedure LabelCloseTabClick(Sender: TObject);
    procedure MenuItemPopupCloseTabClick(Sender: TObject);
    {2.3}
    procedure MenuItemFileOpenClick(Sender: TObject);
    {2.4}
    procedure MenuItemFileSaveClick(Sender: TObject);
    procedure MenuItemFileSaveAsClick(Sender: TObject);
    {2.5}
    procedure MenuItemFilePrintClick(Sender: TObject);
    {3. Text Editing}
    {3.1}
    procedure MenuItemEditUndoClick(Sender: TObject);
    {3.2}
    procedure MenuItemEditRedoClick(Sender: TObject);
    {3.3}
    procedure MenuItemEditCopyClick(Sender: TObject);
    {3.4}
    procedure MenuItemEditCutClick(Sender: TObject);
    {3.5}
    procedure MenuItemEditPasteClick(Sender: TObject);
    {3.6}
    procedure MenuItemEditSelectAllClick(Sender: TObject);
    {4. Font Settings}
    {4.1}
    procedure MenuItemSettingsFontClick(Sender: TObject);
    {5. Editor Format}
    {5.1}
    procedure MenuItemSettingsLanguageTextClick(Sender: TObject);
    {5.2}
    procedure MenuItemSettingsLanguageJavaClick(Sender: TObject);
    procedure MenuItemSettingsLanguagePascalClick(Sender: TObject);
    procedure MenuItemSettingsLanguagePythonClick(Sender: TObject);
    {6. Editor Style}
    {6.1}
    procedure MenuItemMemoSettingsEditorStyleNormalClick(Sender: TObject);
    {6.2}
    procedure MenuItemMemoSettingsEditorStyleDarkClick(Sender: TObject);
    {90. Help}
    {90.1}
    procedure MenuItemHelpAboutClick(Sender: TObject);
    {90.2}
    procedure MenuItemHelpLicenseClick(Sender: TObject);
    {90.3}
    procedure MenuItemHelpGPLClick(Sender: TObject);
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
  unit_form_settings_font, unit_form_readme, rz_an_pas_var;

{ TFormMain }

{1. Application}

{1.1}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  {Reserved Procedures}
end;

{1.1}
procedure TFormMain.FormShow(Sender: TObject);
begin
  Self.RZANPageControlMain.RZAddSheet;
end;

{1.2}
procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.Terminate;
end;

{1.2}
procedure TFormMain.MenuItemFileExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

{2. File Operation}

{2.1}
procedure TFormMain.MenuItemFileNewClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZAddSheet;
end;

{2.1}
procedure TFormMain.MenuItemPopupAddTabClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZAddSheet;
end;

{2.2}
procedure TFormMain.LabelCloseTabClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZCloseSheet;
end;

{2.2}
procedure TFormMain.MenuItemPopupCloseTabClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZCloseSheet;
end;

{2.3}
procedure TFormMain.MenuItemFileOpenClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZOpenFile;
end;

{2.4}
procedure TFormMain.MenuItemFileSaveClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSaveFile;
end;

{2.4}
procedure TFormMain.MenuItemFileSaveAsClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSaveAsFile;
end;

{2.5}
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

{3. Text Editing}

{3.1}
procedure TFormMain.MenuItemEditUndoClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZUndo;
end;

{3.2}
procedure TFormMain.MenuItemEditRedoClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZRedo;
end;

{3.3}
procedure TFormMain.MenuItemEditCopyClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZCopy;
end;

{3.4}
procedure TFormMain.MenuItemEditCutClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZCut;
end;

{3.5}
procedure TFormMain.MenuItemEditPasteClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZPaste;
end;

{3.6}
procedure TFormMain.MenuItemEditSelectAllClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSelectAll;
end;

{4. Font Settings}

{4.1}
procedure TFormMain.MenuItemSettingsFontClick(Sender: TObject);
begin
  FormSettingsFont := TFormSettingsFont.Create(Self);
  FormSettingsFont.Show;
  Self.Enabled := False;
end;

{5. Editor Format}

{5.1}
procedure TFormMain.MenuItemSettingsLanguageTextClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewLanguage(rz_an_type_lang_Text);
end;

{5.2}
procedure TFormMain.MenuItemSettingsLanguageJavaClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewLanguage(rz_an_type_lang_Java);
end;

{5.2}
procedure TFormMain.MenuItemSettingsLanguagePascalClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewLanguage(rz_an_type_lang_Pascal);
end;

{5.2}
procedure TFormMain.MenuItemSettingsLanguagePythonClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewLanguage(rz_an_type_lang_Python);
end;

{6. Editor Style}

{6.1}
procedure TFormMain.MenuItemMemoSettingsEditorStyleNormalClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewStyle(rz_an_type_style_Normal);
end;

{6.2}
procedure TFormMain.MenuItemMemoSettingsEditorStyleDarkClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewStyle(rz_an_type_style_Dark);
end;

{90. Help}

{90.1}
procedure TFormMain.MenuItemHelpAboutClick(Sender: TObject);
begin
  FormReadme := TFormReadme.Create(Self);
  FormReadme.Caption := 'About';
  FormReadme.MemoMain.Lines := FormReadme.MemoAbout.Lines;
  FormReadme.Show;
end;

{90.2}
procedure TFormMain.MenuItemHelpLicenseClick(Sender: TObject);
begin
  FormReadme := TFormReadme.Create(Self);
  FormReadme.Caption := 'License';
  FormReadme.MemoMain.Lines := FormReadme.MemoLicense.Lines;
  FormReadme.Show;
end;

{90.3}
procedure TFormMain.MenuItemHelpGPLClick(Sender: TObject);
begin
  FormReadme := TFormReadme.Create(Self);
  FormReadme.Caption := 'GPL';
  FormReadme.MemoMain.Lines := FormReadme.MemoGPL.Lines;
  FormReadme.Show;
end;

end.

