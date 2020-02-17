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

unit rz_an_pas_var;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, typinfo;

type
  rz_an_type_OS = (rz_an_type_os_windows,rz_an_type_os_linux);
  rz_an_type_EditorType = (rz_an_type_editortype_text,rz_an_type_editortype_syntax);
  rz_an_type_EditorFormat = (
    rz_an_type_editorformat_Basic,
    rz_an_type_editorformat_Cpp,
    rz_an_type_editorformat_CSS,
    rz_an_type_editorformat_HTML,
    rz_an_type_editorformat_Java,
    rz_an_type_editorformat_Javascript,
    rz_an_type_editorformat_Pascal,
    rz_an_type_editorformat_PHP,
    rz_an_type_editorformat_Python,
    rz_an_type_editorformat_SQL,
    rz_an_type_editorformat_XML,
    rz_an_type_editorformat_Text
  );
  rz_an_type_EditorFormatLanguage = rz_an_type_editorformat_Basic..rz_an_type_editorformat_XML;
  // rz_an_type_EditorFormatText = rz_an_type_editorformat_Text;
  rz_an_type_Status = (rz_an_type_status_Ready,rz_an_type_status_Modified,rz_an_type_status_Saved);
  rz_an_type_Style = (rz_an_type_style_Normal,rz_an_type_style_Dark);

  TRZANLanguageColor = class
  private
    FRZNormalReservedWord : TColor;
    FRZNormalComment : TColor;
    FRZNormalString : TColor;
  public
    RZReservedWord : TColor;
    RZComment : TColor;
    RZString : TColor;
    constructor Create (AEditorStyle : rz_an_type_Style);
  end;

  TRZANMainColor = object
    RZBackground : TColor;
    RZText : TColor;
    RZLineHighlight : TColor;
    RZGutter : TColor;
    RZBasic : TRZANLanguageColor;
    RZCpp : TRZANLanguageColor;
    RZCSS : TRZANLanguageColor;
    RZHTML : TRZANLanguageColor;
    RZJava : TRZANLanguageColor;
    RZJavascript : TRZANLanguageColor;
    RZPascal : TRZANLanguageColor;
    RZPHP : TRZANLanguageColor;
    RZPython : TRZANLanguageColor;
    RZSQL : TRZANLanguageColor;
    RZXML : TRZANLanguageColor;
  end;

  TRZANVar = object
  public
    RZVarOS : rz_an_type_OS;
    RZVarAppExeName : TFileName;
    RZVarCallFileOnAppCreate : TFileName;
    RZMainColor : TRZANMainColor;
    procedure Init;
    function GetRZOS : rz_an_type_OS;
    function GetRZEditorFormat (AFileExt : string) : rz_an_type_EditorFormat;
    function IsRZEditorFormatLanguage (AEditorFormat : rz_an_type_EditorFormat) : Boolean;
    function GetRZEditorType (AFileExt : string) : rz_an_type_EditorType;
  end;

const
  {Image Index}
  rz_an_var_ImageIndex_Saved = 8;
  rz_an_var_ImageIndex_Modified = 14;

 var
   VRZANVar : TRZANVar;

implementation

uses
  rz_an_pas_tools;

constructor TRZANLanguageColor.Create (AEditorStyle : rz_an_type_Style);
begin
  Self.FRZNormalReservedWord := clMaroon;
  Self.FRZNormalComment := clRed;
  Self.FRZNormalString := clNavy;
  if AEditorStyle = rz_an_type_style_Normal then
  begin
    Self.RZReservedWord := Self.FRZNormalReservedWord;
    Self.RZComment := Self.FRZNormalComment;
    Self.RZString := Self.FRZNormalString;
  end
  else if AEditorStyle = rz_an_type_style_Dark then
  begin
    Self.RZReservedWord := VRZANTools.ComplementaryColor(Self.FRZNormalReservedWord);
    Self.RZComment := VRZANTools.ComplementaryColor(Self.FRZNormalComment);
    Self.RZString := VRZANTools.ComplementaryColor(Self.FRZNormalString);
  end;
end;

procedure TRZANVar.Init;
begin
  Self.RZMainColor.RZBackground := clWhite;
  Self.RZMainColor.RZText := clBlack;
  Self.RZMainColor.RZLineHighlight := $FFCDCD; // $00D4D4D4; // $00FAFAFA;
  Self.RZMainColor.RZGutter := $00E3E3E3;
  {Languages}
  Self.RZMainColor.RZBasic := TRZANLanguageColor.Create(rz_an_type_style_Normal);
  Self.RZMainColor.RZCpp := TRZANLanguageColor.Create(rz_an_type_style_Normal);
  Self.RZMainColor.RZCSS := TRZANLanguageColor.Create(rz_an_type_style_Normal);
  Self.RZMainColor.RZHTML := TRZANLanguageColor.Create(rz_an_type_style_Normal);
  Self.RZMainColor.RZJava := TRZANLanguageColor.Create(rz_an_type_style_Normal);
  Self.RZMainColor.RZJavaScript := TRZANLanguageColor.Create(rz_an_type_style_Normal);
  Self.RZMainColor.RZPascal := TRZANLanguageColor.Create(rz_an_type_style_Normal);
  Self.RZMainColor.RZPHP := TRZANLanguageColor.Create(rz_an_type_style_Normal);
  Self.RZMainColor.RZPython := TRZANLanguageColor.Create(rz_an_type_style_Normal);
  Self.RZMainColor.RZSQL := TRZANLanguageColor.Create(rz_an_type_style_Normal);
  Self.RZMainColor.RZXML := TRZANLanguageColor.Create(rz_an_type_style_Normal);
end;

function TRZANVar.GetRZOS : rz_an_type_OS;
begin
  {$IFDEF WINDOWS}
    Result := rz_an_type_os_windows;
  {$ENDIF}
  {$IFDEF UNIX}
    Result := rz_an_type_os_linux;
  {$ENDIF}
end;

function TRZANVar.GetRZEditorFormat (AFileExt : string) : rz_an_type_EditorFormat;
begin
  if (AFileExt = '.bas') then Result := rz_an_type_editorformat_Basic
    else if (AFileExt = '.cpp') then Result := rz_an_type_editorformat_Cpp
    else if (AFileExt = '.css') then Result := rz_an_type_editorformat_CSS
    else if (AFileExt = '.htm') or (AFileExt = '.html') then Result := rz_an_type_editorformat_HTML
    else if (AFileExt = '.java') or (AFileExt = '.class') then Result := rz_an_type_editorformat_Java
    else if (AFileExt = '.js') then Result := rz_an_type_editorformat_Javascript
    else if (AFileExt = '.pas') then Result := rz_an_type_editorformat_Pascal
    else if (AFileExt = '.php') then Result := rz_an_type_editorformat_PHP
    else if (AFileExt = '.py') then Result := rz_an_type_editorformat_Python
    else if (AFileExt = '.sql') then Result := rz_an_type_editorformat_SQL
    else if (AFileExt = '.xml') then Result := rz_an_type_editorformat_XML
    else Result := rz_an_type_editorformat_Text
  ;
end;

function TRZANVar.IsRZEditorFormatLanguage (AEditorFormat : rz_an_type_EditorFormat) : Boolean;
var
  i : Byte;
  LResult : Boolean;
  LEditorFormatName : string;
begin
  LResult := False;
  for i := Ord(Low(rz_an_type_EditorFormatLanguage)) to Ord(High(rz_an_type_EditorFormatLanguage)) do
  begin
    LEditorFormatName := GetEnumName(TypeInfo(rz_an_type_EditorFormatLanguage), Ord(i));
    if GetEnumName(TypeInfo(rz_an_type_EditorFormatLanguage),Ord(rz_an_type_EditorFormatLanguage(AEditorFormat))) = LEditorFormatName then
    begin
      LResult := True;
      Break;
    end;
  end;
  Result := LResult;
end;

function TRZANVar.GetRZEditorType (AFileExt : string) : rz_an_type_EditorType;
var
  LRZEditorFormat : rz_an_type_EditorFormat;
begin
  LRZEditorFormat := Self.GetRZEditorFormat(AFileExt);
  if Self.IsRZEditorFormatLanguage(LRZEditorFormat) then Result := rz_an_type_editortype_syntax
  else Result := rz_an_type_editortype_text;
end;

end.

