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

unit rz_an_cmp_synedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, LCLType, SynEdit, Controls, rz_an_cmp_statusbar,
  rz_an_pas_var, SynHighlighterCpp, SynHighlighterCss, SynHighlighterHTML, SynHighlighterJava,
  SynHighlighterJScript, SynHighlighterPas, SynHighlighterPHP, SynHighlighterPython,
  SynHighlighterSQL, SynHighlighterVB, SynHighlighterXML;

type

  {1.1.4}
  TRZANCustomSynEdit = class(TSynEdit)
  private
    {2. FileOperation}
    {2.30}
    FRZFileName : TFileName;
    {2.31}
    FRZFileExt : string;
    {5. Editor Format}
    {5.20}
    FRZEditorFormat : rz_an_type_EditorFormat;
    FRZEditorFormatBasic : TSynVBSyn;
    FRZEditorFormatCpp : TSynCppSyn;
    FRZEditorFormatCSS : TSynCssSyn;
    FRZEditorFormatHTML: TSynHTMLSyn;
    FRZEditorFormatJava : TSynJavaSyn;
    FRZEditorFormatJavascript : TSynJScriptSyn;
    FRZEditorFormatPascal : TSynPasSyn;
    FRZEditorFormatPHP : TSynPHPSyn;
    FRZEditorFormatPython : TSynPythonSyn;
    FRZEditorFormatSQL : TSynSQLSyn;
    FRZEditorFormatXML : TSynXMLSyn;
    {6. Editor Style}
    {6.0}
    FRZStyle : rz_an_type_Style;
    {7. Caret}
    {7.1}
    FRZCaretPosX : Integer;
    {7.2}
    FRZCaretPosY : Integer;
  protected
    {2. File Operation}
    {2.30}
    procedure SetRZFileName (const AValue : TFileName);
    {2.31}
    procedure SetRZFileExt (const AValue : string);
    {5. Editor Format}
    {5.20}
    procedure SetRZEditorFormat (const AValue: rz_an_type_EditorFormat);
    {6. Editor Style}
    {6.0}
    procedure SetRZStyle (AValue : rz_an_type_Style);
    procedure SetRZEditorFormatColors (AEditorStyle : rz_an_type_Style);
    procedure SetRZEditorFormatColorLanguages (AEditorStyle : rz_an_type_Style);
    procedure SetRZEditorFormatColorLanguage (AEditorFormat : TSynVBSyn;
      ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style); overload;
    procedure SetRZEditorFormatColorLanguage (AEditorFormat : TSynCppSyn;
      ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style); overload;
    procedure SetRZEditorFormatColorLanguage (AEditorFormat : TSynCSSSyn;
      ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style); overload;
    procedure SetRZEditorFormatColorLanguage (AEditorFormat : TSynHTMLSyn;
      ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style); overload;
    procedure SetRZEditorFormatColorLanguage (AEditorFormat : TSynJavaSyn;
      ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style); overload;
    procedure SetRZEditorFormatColorLanguage (AEditorFormat : TSynJScriptSyn;
      ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style); overload;
    procedure SetRZEditorFormatColorLanguage (AEditorFormat : TSynPasSyn;
      ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style); overload;
    procedure SetRZEditorFormatColorLanguage (AEditorFormat : TSynPHPSyn;
      ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style); overload;
    procedure SetRZEditorFormatColorLanguage (AEditorFormat : TSynPythonSyn;
      ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style); overload;
    procedure SetRZEditorFormatColorLanguage (AEditorFormat : TSynSQLSyn;
      ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style); overload;
    procedure SetRZEditorFormatColorLanguage (AEditorFormat : TSynXMLSyn;
      ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style); overload;
    procedure SetRZEditorFormatColorAttribute (ALanguageColor : TRZANLanguageColor;
      AEditorStyle : rz_an_type_Style; var AReservedWord,AComment,AString : TColor);
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
    {8.6}
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {8.8}
    procedure RZDoClick (Sender : TObject);
    {8.90}
    procedure RZOnEventUpdate;
    {2. File Operation}
    {2.30}
    property RZFileName : TFileName read FRZFileName write SetRZFileName;
    {2.31}
    property RZFileExt : string read FRZFileExt write SetRZFileExt;
    {5. Editor Format}
    {5.20}
    property RZEditorFormat : rz_an_type_EditorFormat read FRZEditorFormat write SetRZEditorFormat;
    property RZEditorFormatBasic : TSynVBSyn read FRZEditorFormatBasic write FRZEditorFormatBasic;
    property RZEditorFormatCpp : TSynCppSyn read FRZEditorFormatCpp write FRZEditorFormatCpp;
    property RZEditorFormatCSS : TSynCssSyn read FRZEditorFormatCSS write FRZEditorFormatCSS;
    property RZEditorFormatHTML : TSynHTMLSyn read FRZEditorFormatHTML write FRZEditorFormatHTML;
    property RZEditorFormatJava : TSynJavaSyn read FRZEditorFormatJava write FRZEditorFormatJava;
    property RZEditorFormatJavascript : TSynJScriptSyn read FRZEditorFormatJavascript write FRZEditorFormatJavascript;
    property RZEditorFormatPascal : TSynPasSyn read FRZEditorFormatPascal write FRZEditorFormatPascal;
    property RZEditorFormatPHP : TSynPHPSyn read FRZEditorFormatPHP write FRZEditorFormatPHP;
    property RZEditorFormatPython : TSynPythonSyn read FRZEditorFormatPython write FRZEditorFormatPython;
    property RZEditorFormatSQL : TSynSQLSyn read FRZEditorFormatSQL write FRZEditorFormatSQL;
    property RZEditorFormatXML : TSynXMLSyn read FRZEditorFormatXML write FRZEditorFormatXML;
    {6. Editor Style}
    {6.0}
    property RZStyle : rz_an_type_Style read FRZStyle write SetRZStyle;
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
    property RZEditorFormat;
    {6. Editor Style}
    {6.0}
    property RZStyle;
    {7. Caret}
    {7.1}
    property RZCaretPosX;
    {7.2}
    property RZCaretPosY;
  end;

implementation

uses
  rz_an_cmp_tabsheet, rz_an_pas_tools;

{1. Application}

{1.1.4}
constructor TRZANCustomSynEdit.Create (AOwner : TComponent);
begin
  {1. Application}
  {1.1}
  inherited Create(AOwner);
  Self.Font.Name := 'Courier New';
  Self.Font.Size := 10;
  Self.Font.Quality := fqDraft;
  Self.SetRZEditorFormatColors(Self.FRZStyle);
  {5. Editor Format}
  {5.20}
  Self.RZEditorFormatBasic := TSynVBSyn.Create(Self);
  Self.RZEditorFormatCpp := TSynCppSyn.Create(Self);
  Self.RZEditorFormatCSS := TSynCssSyn.Create(Self);
  Self.RZEditorFormatHTML := TSynHTMLSyn.Create(Self);
  Self.RZEditorFormatJava := TSynJavaSyn.Create(Self);
  Self.RZEditorFormatJavascript := TSynJScriptSyn.Create(Self);
  Self.RZEditorFormatPascal := TSynPasSyn.Create(Self);
  Self.RZEditorFormatPHP := TSynPHPSyn.Create(Self);
  Self.RZEditorFormatPython := TSynPythonSyn.Create(Self);
  Self.RZEditorFormatSQL := TSynSQLSyn.Create(Self);
  Self.RZEditorFormatXML := TSynXMLSyn.Create(Self);
  {Colors}
  Self.SetRZEditorFormatColorLanguages(rz_an_type_style_Normal);
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
  Self.RZFileName := AFileName;
end;

{2.4}
procedure TRZANCustomSynEdit.RZSave (AFileName : TFileName);
begin
  Self.Lines.SaveToFile(AFileName);
  Self.RZFileName := AFileName;
end;

{2.30}
procedure TRZANCustomSynEdit.SetRZFileName (const AValue : TFileName);
begin
  Self.FRZFileName := AValue;
  if AValue <> '' then Self.RZFileExt := ExtractFileExt(Self.RZFileName);
end;

{2.31}
procedure TRZANCustomSynEdit.SetRZFileExt (const AValue : string);
begin
  Self.FRZFileExt := AValue;
  Self.RZEditorFormat := VRZANVar.GetRZEditorFormat(Self.FRZFileExt);
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
procedure TRZANCustomSynEdit.SetRZEditorFormat (const AValue: rz_an_type_EditorFormat);
begin
  Self.FRZEditorFormat := AValue;
  case Self.FRZEditorFormat of
    rz_an_type_editorformat_Basic : Self.Highlighter := Self.RZEditorFormatBasic;
    rz_an_type_editorformat_Cpp : Self.Highlighter := Self.RZEditorFormatCpp;
    rz_an_type_editorformat_CSS : Self.Highlighter := Self.RZEditorFormatCSS;
    rz_an_type_editorformat_HTML : Self.Highlighter := Self.RZEditorFormatHTML;
    rz_an_type_editorformat_Java : Self.Highlighter := Self.RZEditorFormatJava;
    rz_an_type_editorformat_Javascript : Self.Highlighter := Self.RZEditorFormatJavascript;
    rz_an_type_editorformat_Pascal : Self.Highlighter := Self.RZEditorFormatPascal;
    rz_an_type_editorformat_PHP : Self.Highlighter := Self.RZEditorFormatPHP;
    rz_an_type_editorformat_Python : Self.Highlighter := Self.RZEditorFormatPython;
    rz_an_type_editorformat_SQL : Self.Highlighter := Self.RZEditorFormatSQL;
    rz_an_type_editorformat_XML : Self.Highlighter := Self.RZEditorFormatXML;
  end;
end;

{6. Editor Style}

{6.0}
procedure TRZANCustomSynEdit.SetRZStyle (AValue : rz_an_type_Style);
begin
  Self.FRZStyle := AValue;
  Self.SetRZEditorFormatColors(Self.FRZStyle);
  Self.SetRZEditorFormatColorLanguages(Self.FRZStyle);
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColors (AEditorStyle : rz_an_type_Style);
begin
  if AEditorStyle = rz_an_type_style_Normal then
  begin
    Self.Color := VRZANVar.RZMainColor.RZBackground;
    Self.Gutter.Color := VRZANVar.RZMainColor.RZGutter;
    Self.Gutter.Parts[1].MarkupInfo.Background := VRZANVar.RZMainColor.RZGutter;
    Self.Gutter.Parts[3].MarkupInfo.Background := VRZANVar.RZMainColor.RZGutter;
    Self.LineHighlightColor.Background := VRZANVar.RZMainColor.RZLineHighlight;
    Self.Font.Color := VRZANVar.RZMainColor.RZText;
  end
  else if AEditorStyle = rz_an_type_style_Dark then
  begin
    Self.Color := VRZANTools.ComplementaryColor(VRZANVar.RZMainColor.RZBackground);
    Self.Gutter.Color := VRZANTools.ComplementaryColor(VRZANVar.RZMainColor.RZGutter);
    Self.Gutter.Parts[1].MarkupInfo.Background := VRZANTools.ComplementaryColor(VRZANVar.RZMainColor.RZGutter);
    Self.Gutter.Parts[3].MarkupInfo.Background := VRZANTools.ComplementaryColor(VRZANVar.RZMainColor.RZGutter);
    Self.LineHighlightColor.Background := VRZANTools.ComplementaryColor(VRZANVar.RZMainColor.RZLineHighlight);
    Self.Font.Color := VRZANTools.ComplementaryColor(VRZANVar.RZMainColor.RZText);
  end;
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColorLanguages (AEditorStyle : rz_an_type_Style);
begin
  Self.SetRZEditorFormatColorLanguage(Self.RZEditorFormatBasic,VRZANVar.RZMainColor.RZBasic,AEditorStyle);
  Self.SetRZEditorFormatColorLanguage(Self.RZEditorFormatCpp,VRZANVar.RZMainColor.RZCpp,AEditorStyle);
  Self.SetRZEditorFormatColorLanguage(Self.RZEditorFormatCSS,VRZANVar.RZMainColor.RZCSS,AEditorStyle);
  Self.SetRZEditorFormatColorLanguage(Self.RZEditorFormatHTML,VRZANVar.RZMainColor.RZHTML,AEditorStyle);
  Self.SetRZEditorFormatColorLanguage(Self.RZEditorFormatJava,VRZANVar.RZMainColor.RZJava,AEditorStyle);
  Self.SetRZEditorFormatColorLanguage(Self.RZEditorFormatJavaScript,VRZANVar.RZMainColor.RZJavaScript,AEditorStyle);
  Self.SetRZEditorFormatColorLanguage(Self.RZEditorFormatPascal,VRZANVar.RZMainColor.RZPascal,AEditorStyle);
  Self.SetRZEditorFormatColorLanguage(Self.RZEditorFormatPHP,VRZANVar.RZMainColor.RZPHP,AEditorStyle);
  Self.SetRZEditorFormatColorLanguage(Self.RZEditorFormatPython,VRZANVar.RZMainColor.RZPython,AEditorStyle);
  Self.SetRZEditorFormatColorLanguage(Self.RZEditorFormatSQL,VRZANVar.RZMainColor.RZSQL,AEditorStyle);
  Self.SetRZEditorFormatColorLanguage(Self.RZEditorFormatXML,VRZANVar.RZMainColor.RZXML,AEditorStyle);
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColorLanguage (AEditorFormat : TSynVBSyn;
  ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style);
var
  LReservedWord,LComment,LString : TColor;
begin
  Self.SetRZEditorFormatColorAttribute(ALanguageColor,AEditorStyle,LReservedWord,LComment,LString);
  AEditorFormat.KeyAttri.Foreground := LReservedWord;
  AEditorFormat.CommentAttri.Foreground := LComment;
  AEditorFormat.StringAttri.Foreground := LString;
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColorLanguage (AEditorFormat : TSynCppSyn;
  ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style);
var
  LReservedWord,LComment,LString : TColor;
begin
  Self.SetRZEditorFormatColorAttribute(ALanguageColor,AEditorStyle,LReservedWord,LComment,LString);
  AEditorFormat.KeyAttri.Foreground := LReservedWord;
  AEditorFormat.CommentAttri.Foreground := LComment;
  AEditorFormat.StringAttri.Foreground := LString;
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColorLanguage (AEditorFormat : TSynCSSSyn;
  ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style);
var
  LReservedWord,LComment,LString : TColor;
begin
  Self.SetRZEditorFormatColorAttribute(ALanguageColor,AEditorStyle,LReservedWord,LComment,LString);
  AEditorFormat.KeyAttri.Foreground := LReservedWord;
  AEditorFormat.CommentAttri.Foreground := LComment;
  AEditorFormat.StringAttri.Foreground := LString;
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColorLanguage (AEditorFormat : TSynHTMLSyn;
  ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style);
var
  LReservedWord,LComment,LString : TColor;
begin
  Self.SetRZEditorFormatColorAttribute(ALanguageColor,AEditorStyle,LReservedWord,LComment,LString);
  AEditorFormat.KeyAttri.Foreground := LReservedWord;
  AEditorFormat.CommentAttri.Foreground := LComment;
  // AEditorFormat.StringAttri.Foreground := LString;
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColorLanguage (AEditorFormat : TSynJavaSyn;
  ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style);
var
  LReservedWord,LComment,LString : TColor;
begin
  Self.SetRZEditorFormatColorAttribute(ALanguageColor,AEditorStyle,LReservedWord,LComment,LString);
  AEditorFormat.KeyAttri.Foreground := LReservedWord;
  AEditorFormat.CommentAttri.Foreground := LComment;
  AEditorFormat.StringAttri.Foreground := LString;
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColorLanguage (AEditorFormat : TSynJScriptSyn;
  ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style);
var
  LReservedWord,LComment,LString : TColor;
begin
  Self.SetRZEditorFormatColorAttribute(ALanguageColor,AEditorStyle,LReservedWord,LComment,LString);
  AEditorFormat.KeyAttri.Foreground := LReservedWord;
  AEditorFormat.CommentAttri.Foreground := LComment;
  AEditorFormat.StringAttri.Foreground := LString;
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColorLanguage (AEditorFormat : TSynPasSyn;
  ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style);
var
  LReservedWord,LComment,LString : TColor;
begin
  Self.SetRZEditorFormatColorAttribute(ALanguageColor,AEditorStyle,LReservedWord,LComment,LString);
  AEditorFormat.KeyAttri.Foreground := LReservedWord;
  AEditorFormat.CommentAttri.Foreground := LComment;
  AEditorFormat.StringAttri.Foreground := LString;
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColorLanguage (AEditorFormat : TSynPHPSyn;
  ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style);
var
  LReservedWord,LComment,LString : TColor;
begin
  Self.SetRZEditorFormatColorAttribute(ALanguageColor,AEditorStyle,LReservedWord,LComment,LString);
  AEditorFormat.KeyAttri.Foreground := LReservedWord;
  AEditorFormat.CommentAttri.Foreground := LComment;
  AEditorFormat.StringAttri.Foreground := LString;
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColorLanguage (AEditorFormat : TSynPythonSyn;
  ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style);
var
  LReservedWord,LComment,LString : TColor;
begin
  Self.SetRZEditorFormatColorAttribute(ALanguageColor,AEditorStyle,LReservedWord,LComment,LString);
  AEditorFormat.KeyAttri.Foreground := LReservedWord;
  AEditorFormat.CommentAttri.Foreground := LComment;
  AEditorFormat.StringAttri.Foreground := LString;
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColorLanguage (AEditorFormat : TSynSQLSyn;
  ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style);
var
  LReservedWord,LComment,LString : TColor;
begin
  Self.SetRZEditorFormatColorAttribute(ALanguageColor,AEditorStyle,LReservedWord,LComment,LString);
  AEditorFormat.KeyAttri.Foreground := LReservedWord;
  AEditorFormat.CommentAttri.Foreground := LComment;
  AEditorFormat.StringAttri.Foreground := LString;
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColorLanguage (AEditorFormat : TSynXMLSyn;
  ALanguageColor : TRZANLanguageColor; AEditorStyle : rz_an_type_Style);
var
  LReservedWord,LComment,LString : TColor;
begin
  Self.SetRZEditorFormatColorAttribute(ALanguageColor,AEditorStyle,LReservedWord,LComment,LString);
  // AEditorFormat.KeyAttri.Foreground := LReservedWord;
  AEditorFormat.CommentAttri.Foreground := LComment;
  // AEditorFormat.StringAttri.Foreground := LString;
end;

{6.0}
procedure TRZANCustomSynEdit.SetRZEditorFormatColorAttribute (ALanguageColor : TRZANLanguageColor;
  AEditorStyle : rz_an_type_Style; var AReservedWord,AComment,AString : TColor);
begin
  if AEditorStyle = rz_an_type_style_Normal then
  begin
    AReservedWord := ALanguageColor.RZReservedWord;
    AComment := ALanguageColor.RZComment;
    AString := ALanguageColor.RZString;
  end
  else if AEditorStyle = rz_an_type_style_Dark then
  begin
    AReservedWord := VRZANTools.ComplementaryColor(ALanguageColor.RZReservedWord);
    AComment := VRZANTools.ComplementaryColor(ALanguageColor.RZComment);
    AString := VRZANTools.ComplementaryColor(ALanguageColor.RZString);
  end
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
  Self.PopupMenu := (Self.Parent as TRZANTabSheet).RZPopupMenu;
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

{8.6}
procedure TRZANCustomSynEdit.MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Self.PopupMenu := (Self.Parent as TRZANTabSheet).RZPopupMenu;
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

