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

unit rz_an_cmp_statusbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Controls, Dialogs, rz_an_pas_var;

type

  {9. Status Bar}
  {9.1 Index}
  TRZANStatusBarIndex = class(TComponent)
  private
    {9. Status Bar}
    {9.1.1}
    FRZStatus : Byte;
    {9.1.2}
    FRZEditorFormat : Byte;
    {9.1.3}
    FRZCaret : Byte;
    {9.1.4}
    FRZFileName : Byte;
  public
    {9.1.1}
    property RZStatus : Byte read FRZStatus write FRZStatus;
    {9.1.2}
    property RZEditorFormat : Byte read FRZEditorFormat write FRZEditorFormat;
    {9.1.3}
    property RZCaret : Byte read FRZCaret write FRZCaret;
    {9.1.4}
    property RZFileName : Byte read FRZFileName write FRZFileName;
    {9.1.0}
    constructor Create (AOwner : TComponent); override;
  end;

  {1.1.5}
  TRZANCustomStatusBar = class(TStatusBar)
  private
    {9. Status Bar}
    {9.1}
    FRZIndex : TRZANStatusBarIndex;
    {9.1.1}
    FRZStatus : rz_an_type_Status;
    {9.1.2}
    FRZEditorFormat : rz_an_type_EditorFormat;
    {9.1.3}
    FRZCaretPosX : Integer;
    FRZCaretPosY : Integer;
    {9.1.4}
    FRZFileName : TFileName;
    {9.2}
    procedure FRZPanelAdd (AAlignment : TAlignment; AText : TCaption; AWidth : Integer);
    procedure FRZPanelsAdd;
  protected
    {9. Status Bar}
    {9.1.1}
    procedure SetRZStatus (const AValue: rz_an_type_Status);
    {9.1.2}
    procedure SetRZEditorFormat (const AValue: rz_an_type_EditorFormat);
    {9.1.3}
    procedure SetRZCaretPosX (const AValue: Integer);
    procedure SetRZCaretPosY (const AValue: Integer);
    {9.1.4}
    procedure SetRZFileName (const AValue: TFileName);
    {9.1}
    property RZIndex : TRZANStatusBarIndex read FRZIndex write FRZIndex;
    {9.1.1}
    property RZStatus : rz_an_type_Status read FRZStatus write SetRZStatus;
    {9.1.2}
    property RZEditorFormat : rz_an_type_EditorFormat read FRZEditorFormat write SetRZEditorFormat;
    {9.1.3}
    property RZCaretPosX : Integer read FRZCaretPosX write SetRZCaretPosX;
    property RZCaretPosY : Integer read FRZCaretPosY write SetRZCaretPosY;
    {9.1.4}
    property RZFileName : TFileName read FRZFileName write SetRZFileName;
  public
    {1. Application}
    {1.1.5}
    constructor Create (AOwner : TComponent); override;
  end;

  TRZANStatusBar = class(TRZANCustomStatusBar)
  public
    {9. Status Bar}
    {9.1}
    property RZIndex;
    {9.1.1}
    property RZStatus;
    {9.1.2}
    property RZEditorFormat;
    {9.1.3}
    property RZCaretPosX;
    property RZCaretPosY;
    {9.1.4}
    property RZFileName;
  end;

implementation

{9. Status Bar}

{9.1}
constructor TRZANStatusBarIndex.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  {9.1.1}
  Self.RZStatus := 0;
  {9.1.2}
  Self.RZEditorFormat := 1;
  {9.1.3}
  Self.RZCaret := 2;
  {9.1.4}
  Self.RZFileName := 3;
end;

{1. Application}

{1.1.5}
constructor TRZANCustomStatusBar.Create (AOwner : TComponent);
begin
  {1. Application}
  {1.1}
  inherited Create(AOwner);
  {9.2}
  Self.FRZPanelsAdd;
  {9.1}
  Self.RZIndex := TRZANStatusBarIndex.Create(Self);
end;

{9. Status Bar}

{9.1.1}
procedure TRZANCustomStatusBar.SetRZStatus (const AValue : rz_an_type_Status);
begin
  Self.FRZStatus := AValue;
  if Self.RZStatus = rz_an_type_status_Ready then Self.Panels[Self.RZIndex.RZStatus].Text := 'Ready'
    else if Self.RZStatus = rz_an_type_status_Modified then Self.Panels[Self.RZIndex.RZStatus].Text := 'Modified'
    else if Self.RZStatus = rz_an_type_status_Saved then Self.Panels[Self.RZIndex.RZStatus].Text := 'Saved'
  ;
end;

{9.1.2}
procedure TRZANCustomStatusBar.SetRZEditorFormat (const AValue : rz_an_type_EditorFormat);
begin
  Self.FRZEditorFormat := AValue;
  if Self.RZEditorFormat = rz_an_type_editorformat_Basic then Self.Panels[Self.RZIndex.RZEditorFormat].Text := 'Basic'
    else if Self.RZEditorFormat = rz_an_type_editorformat_Cpp then Self.Panels[Self.RZIndex.RZEditorFormat].Text := 'C++'
    else if Self.RZEditorFormat = rz_an_type_editorformat_CSS then Self.Panels[Self.RZIndex.RZEditorFormat].Text := 'CSS'
    else if Self.RZEditorFormat = rz_an_type_editorformat_HTML then Self.Panels[Self.RZIndex.RZEditorFormat].Text := 'HTML'
    else if Self.RZEditorFormat = rz_an_type_editorformat_Java then Self.Panels[Self.RZIndex.RZEditorFormat].Text := 'Java'
    else if Self.RZEditorFormat = rz_an_type_editorformat_Javascript then Self.Panels[Self.RZIndex.RZEditorFormat].Text := 'Javascript'
    else if Self.RZEditorFormat = rz_an_type_editorformat_PHP then Self.Panels[Self.RZIndex.RZEditorFormat].Text := 'PHP'
    else if Self.RZEditorFormat = rz_an_type_editorformat_Pascal then Self.Panels[Self.RZIndex.RZEditorFormat].Text := 'Pascal'
    else if Self.RZEditorFormat = rz_an_type_editorformat_Python then Self.Panels[Self.RZIndex.RZEditorFormat].Text := 'Python'
    else if Self.RZEditorFormat = rz_an_type_editorformat_SQL then Self.Panels[Self.RZIndex.RZEditorFormat].Text := 'SQL'
    else if Self.RZEditorFormat = rz_an_type_editorformat_XML then Self.Panels[Self.RZIndex.RZEditorFormat].Text := 'XML'
    else if Self.RZEditorFormat = rz_an_type_editorformat_Dataframe then Self.Panels[Self.RZIndex.RZEditorFormat].Text := 'Dataframe'
    else if Self.RZEditorFormat = rz_an_type_editorformat_Text then Self.Panels[Self.RZIndex.RZEditorFormat].Text := 'Text'
    else if Self.RZEditorFormat = rz_an_type_editorformat_All then Self.Panels[Self.RZIndex.RZEditorFormat].Text := '*'
  ;
end;

{9.1.3}
procedure TRZANCustomStatusBar.SetRZCaretPosX (const AValue : Integer);
begin
  Self.FRZCaretPosX := AValue;
  Self.Panels[Self.RZIndex.RZCaret].Text := IntToStr(Self.RZCaretPosY) + ':'
    + IntToStr(Self.RZCaretPosX)
  ;
end;

{9.1.3}
procedure TRZANCustomStatusBar.SetRZCaretPosY (const AValue : Integer);
begin
  Self.FRZCaretPosY := AValue;
  Self.Panels[Self.RZIndex.RZCaret].Text := IntToStr(Self.RZCaretPosY) + ':'
    + IntToStr(Self.RZCaretPosX)
  ;
end;

{9.1.4}
procedure TRZANCustomStatusBar.SetRZFileName (const AValue : TFileName);
begin
  Self.FRZFileName := AValue;
  Self.Panels[Self.RZIndex.RZFileName].Text := Self.FRZFileName;
end;

{9.2}
procedure TRZANCustomStatusBar.FRZPanelAdd (AAlignment : TAlignment; AText : TCaption; AWidth : Integer);
begin
  with Self.Panels.Add do
  begin
    Alignment := AAlignment;
    Text := AText;
    Width := AWidth;
  end;
end;

{9.2}
procedure TRZANCustomStatusBar.FRZPanelsAdd;
begin
  Self.FRZPanelAdd(taCenter,'Ready',100);
  Self.FRZPanelAdd(taCenter,'Text',100);
  Self.FRZPanelAdd(taCenter,'1:0',150);
  Self.FRZPanelAdd(taLeftJustify,'',50);
end;

end.

