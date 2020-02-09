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
    FRZLanguage : Byte;
    {9.1.3}
    FRZCaret : Byte;
    {9.1.4}
    FRZFileName : Byte;
  public
    {9.1.1}
    property RZStatus : Byte read FRZStatus write FRZStatus;
    {9.1.2}
    property RZLanguage : Byte read FRZLanguage write FRZLanguage;
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
    FRZLanguage : rz_an_type_Language;
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
    procedure SetRZLanguage (const AValue: rz_an_type_Language);
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
    property RZLanguage : rz_an_type_Language read FRZLanguage write SetRZLanguage;
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
    property RZLanguage;
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
  Self.RZLanguage := 1;
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
procedure TRZANCustomStatusBar.SetRZLanguage (const AValue : rz_an_type_Language);
begin
  Self.FRZLanguage := AValue;
  if Self.RZLanguage = rz_an_type_lang_Java then Self.Panels[Self.RZIndex.RZLanguage].Text := 'Java'
    else if Self.RZLanguage = rz_an_type_lang_Pascal then Self.Panels[Self.RZIndex.RZLanguage].Text := 'Pascal'
    else if Self.RZLanguage = rz_an_type_lang_Python then Self.Panels[Self.RZIndex.RZLanguage].Text := 'Python'
    else if Self.RZLanguage = rz_an_type_lang_Text then Self.Panels[Self.RZIndex.RZLanguage].Text := 'Text'
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

