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

unit unit_form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ComCtrls, PrintersDlgs, LCLType, ExtCtrls, FileUtil, lclintf, SynEdit, RichMemo,
  rz_an_cmp_pagecontrol, rz_an_cmp_statusbar;

type

  { TFormMain }

  TFormMain = class(TForm)
    {1. Application}
    {1.1}
    MainMenuMemo: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemHelpOnlineHelp: TMenuItem;
    MenuItemSettingsEditorFormatDataframe: TMenuItem;
    MenuItemSettingsSpecialCharactersHide: TMenuItem;
    MenuItemSettingsSpecialCharactersShow: TMenuItem;
    MenuItemSettingsSpecialCharacters: TMenuItem;
    MenuItemSettingsContextMenuWindowsExplorerAdd: TMenuItem;
    MenuItemSettingsContextMenuWindowsExplorer: TMenuItem;
    MenuItemSettingsContextMenu: TMenuItem;
    N1: TMenuItem;
    MenuItem7: TMenuItem;
    PanelToolBarAndCloseTab: TPanel;
    PanelToolBar: TPanel;
    PanelCloseTab: TPanel;
    PopupMenuEditor: TPopupMenu;
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
    MenuItemPopupEditorCopy: TMenuItem;
    {3.4}
    MenuItemEditCut: TMenuItem;
    ToolButtonEditCut: TToolButton;
    MenuItemPopupEditorCut: TMenuItem;
    {3.5}
    MenuItemEditPaste: TMenuItem;
    ToolButtonEditPaste: TToolButton;
    MenuItemPopupEditorPaste: TMenuItem;
    {3.6}
    MenuItemEditSelectAll: TMenuItem;
    MenuItemPopupEditorSelectAll: TMenuItem;
    {4. Font Settings}
    {4.1}
    MenuItemSettings: TMenuItem;
    MenuItemSettingsFont: TMenuItem;
    ToolButtonSettingsFont: TToolButton;
    {5. Editor Format}
    MenuItemSettingsEditorFormat: TMenuItem;
    {5.1}
    MenuItemSettingsEditorFormatText: TMenuItem;
    {5.2}
    MenuItemSettingsEditorFormatBasic: TMenuItem;
    MenuItemSettingsEditorFormatCpp: TMenuItem;
    MenuItemSettingsEditorFormatCSS: TMenuItem;
    MenuItemSettingsEditorFormatHTML: TMenuItem;
    MenuItemSettingsEditorFormatJava: TMenuItem;
    MenuItemSettingsEditorFormatJavascript: TMenuItem;
    MenuItemSettingsEditorFormatPascal: TMenuItem;
    MenuItemSettingsEditorFormatPHP: TMenuItem;
    MenuItemSettingsEditorFormatPython: TMenuItem;
    MenuItemSettingsEditorFormatSQL: TMenuItem;
    MenuItemSettingsEditorFormatXML: TMenuItem;
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
    procedure MenuItemHelpOnlineHelpClick(Sender: TObject);
    {1.30}
    procedure MenuItemSettingsContextMenuWindowsExplorerAddClick(Sender: TObject);
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
    procedure MenuItemPopupEditorCopyClick(Sender: TObject);
    procedure MenuItemPopupEditorCutClick(Sender: TObject);
    procedure MenuItemPopupEditorPasteClick(Sender: TObject);
    procedure MenuItemPopupEditorSelectAllClick(Sender: TObject);
    {4. Font Settings}
    {4.1}
    procedure MenuItemSettingsFontClick(Sender: TObject);
    {5. Editor Format}
    {5.1}
    procedure MenuItemSettingsEditorFormatTextClick(Sender: TObject);
    {5.2}
    procedure MenuItemSettingsEditorFormatBasicClick(Sender: TObject);
    procedure MenuItemSettingsEditorFormatCppClick(Sender: TObject);
    procedure MenuItemSettingsEditorFormatCSSClick(Sender: TObject);
    procedure MenuItemSettingsEditorFormatHTMLClick(Sender: TObject);
    procedure MenuItemSettingsEditorFormatJavaClick(Sender: TObject);
    procedure MenuItemSettingsEditorFormatJavascriptClick(Sender: TObject);
    procedure MenuItemSettingsEditorFormatPascalClick(Sender: TObject);
    procedure MenuItemSettingsEditorFormatPHPClick(Sender: TObject);
    procedure MenuItemSettingsEditorFormatPythonClick(Sender: TObject);
    procedure MenuItemSettingsEditorFormatSQLClick(Sender: TObject);
    procedure MenuItemSettingsEditorFormatXMLClick(Sender: TObject);
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
    procedure MenuItemSettingsSpecialCharactersShowClick(Sender: TObject);
    procedure MenuItemSettingsSpecialCharactersHideClick(Sender: TObject);
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
  unit_form_settings_font, unit_form_readme, rz_an_pas_var, rz_an_pas_tools;

{ TFormMain }

{1. Application}

{1.1}
procedure TFormMain.FormCreate(Sender: TObject);
begin
  VRZANVar.Init;
  VRZANVar.RZVarOS := VRZANVar.GetRZOS;
  VRZANVar.RZVarAppExeName := Application.ExeName;
  VRZANVar.RZVarCallFileOnAppCreate := ParamStr(1);
end;

{1.1}
procedure TFormMain.FormShow(Sender: TObject);
begin
  Self.RZANPageControlMain.RZAddSheet(VRZANVar.RZVarCallFileOnAppCreate,
    Ord(rz_an_var_EditorFormat_Default)
  );
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

{1.30}
procedure TFormMain.MenuItemSettingsContextMenuWindowsExplorerAddClick(Sender: TObject);
begin
  if VRZANVar.RZVarOS = rz_an_type_os_windows then
  begin
    MessageDlg('Information','Make sure that you "Run As Administrator"',mtInformation,[mbOK],0);
    try
      VRZANTools.CreateRegistry(VRZANVar.RZVarAppExeName);
      MessageDlg('Information','"Edit with Anoa-Notepad" has been added as a context menu at Windows Explorer',mtInformation,[mbOK],0);
    except
      MessageDlg('Error','Please make sure that you "Run As Administrator"',mtError,[mbOK],0);
    end;
  end
  else
    MessageDlg('Error','For Windows Only',mtError,[mbOK],0);
  ;
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
  Self.RZANPageControlMain.RZPrint(LPrintParameters);
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

{3.3}
procedure TFormMain.MenuItemPopupEditorCopyClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZCopy;
end;

{3.4}
procedure TFormMain.MenuItemEditCutClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZCut;
end;

procedure TFormMain.MenuItemPopupEditorCutClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZCut;
end;

{3.5}
procedure TFormMain.MenuItemEditPasteClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZPaste;
end;

procedure TFormMain.MenuItemPopupEditorPasteClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZPaste;
end;

{3.6}
procedure TFormMain.MenuItemEditSelectAllClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSelectAll;
end;

procedure TFormMain.MenuItemPopupEditorSelectAllClick(Sender: TObject);
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
procedure TFormMain.MenuItemSettingsEditorFormatTextClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewEditorFormat(rz_an_type_editorformat_Text);
end;

{5.2}
procedure TFormMain.MenuItemSettingsEditorFormatBasicClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewEditorFormat(rz_an_type_editorformat_Basic);
end;

{5.2}
procedure TFormMain.MenuItemSettingsEditorFormatCppClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewEditorFormat(rz_an_type_editorformat_Cpp);
end;

{5.2}
procedure TFormMain.MenuItemSettingsEditorFormatCSSClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewEditorFormat(rz_an_type_editorformat_CSS);
end;

{5.2}
procedure TFormMain.MenuItemSettingsEditorFormatHTMLClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewEditorFormat(rz_an_type_editorformat_HTML);
end;

{5.2}
procedure TFormMain.MenuItemSettingsEditorFormatJavaClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewEditorFormat(rz_an_type_editorformat_Java);
end;

{5.2}
procedure TFormMain.MenuItemSettingsEditorFormatJavascriptClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewEditorFormat(rz_an_type_editorformat_Javascript);
end;

{5.2}
procedure TFormMain.MenuItemSettingsEditorFormatPascalClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewEditorFormat(rz_an_type_editorformat_Pascal);
end;

{5.2}
procedure TFormMain.MenuItemSettingsEditorFormatPHPClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewEditorFormat(rz_an_type_editorformat_PHP);
end;

{5.2}
procedure TFormMain.MenuItemSettingsEditorFormatPythonClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewEditorFormat(rz_an_type_editorformat_Python);
end;

{5.2}
procedure TFormMain.MenuItemSettingsEditorFormatSQLClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewEditorFormat(rz_an_type_editorformat_SQL);
end;

{5.2}
procedure TFormMain.MenuItemSettingsEditorFormatXMLClick(Sender: TObject);
begin
  Self.RZANPageControlMain.RZSetNewEditorFormat(rz_an_type_editorformat_XML);
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
  FormReadme.RichMemoAbout.Align := alClient;
  FormReadme.RichMemoAbout.Visible := True;
  FormReadme.RichMemoLicense.Visible := False;
  FormReadme.RichMemoGPL.Visible := False;
  FormReadme.ShowModal;
end;

{90.2}
procedure TFormMain.MenuItemHelpLicenseClick(Sender: TObject);
begin
  FormReadme := TFormReadme.Create(Self);
  FormReadme.Caption := 'License';
  FormReadme.RichMemoLicense.Align := alClient;
  FormReadme.RichMemoLicense.Visible := True;
  FormReadme.RichMemoAbout.Visible := False;
  FormReadme.RichMemoGPL.Visible := False;
  FormReadme.ShowModal;
end;

{90.3}
procedure TFormMain.MenuItemHelpGPLClick(Sender: TObject);
begin
  FormReadme := TFormReadme.Create(Self);
  FormReadme.Caption := 'GPL';
  FormReadme.RichMemoGPL.Align := alClient;
  FormReadme.RichMemoGPL.Visible := True;
  FormReadme.RichMemoLicense.Visible := False;
  FormReadme.RichMemoAbout.Visible := False;
  FormReadme.ShowModal;
end;

procedure TFormMain.MenuItemHelpOnlineHelpClick(Sender: TObject);
begin
  OpenURL('http://anoa-projects.com/anoa-notepad/');
end;

procedure TFormMain.MenuItemSettingsSpecialCharactersShowClick(Sender: TObject);
begin
  if Self.RZANPageControlMain.RZSpecialCharactersShow then
  begin
    Self.MenuItemSettingsSpecialCharactersShow.Checked := True;
    Self.MenuItemSettingsSpecialCharactersHide.Checked := False;
  end;
end;

procedure TFormMain.MenuItemSettingsSpecialCharactersHideClick(Sender: TObject);
begin
  if Self.RZANPageControlMain.RZSpecialCharactersHide then
  begin
    Self.MenuItemSettingsSpecialCharactersShow.Checked := False;
    Self.MenuItemSettingsSpecialCharactersHide.Checked := True;
  end;
end;

end.

