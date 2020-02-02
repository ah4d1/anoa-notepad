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

  TRZANStatusBarIndex = class(TComponent)
  private
    FRZStatus : Byte;
    FRZLanguage : Byte;
    FRZCaret : Byte;
    FRZFileName : Byte;
  public
    property RZStatus : Byte read FRZStatus write FRZStatus;
    property RZLanguage : Byte read FRZLanguage write FRZLanguage;
    property RZCaret : Byte read FRZCaret write FRZCaret;
    property RZFileName : Byte read FRZFileName write FRZFileName;
    constructor Create (AOwner : TComponent); override;
  end;

  TRZANCustomStatusBar = class(TStatusBar)
  private
    FRZIndex : TRZANStatusBarIndex;
    FRZStatus : rz_an_type_Status;
    FRZLanguage : rz_an_type_Language;
    FRZCaretPosX : Integer;
    FRZCaretPosY : Integer;
    FRZFileName : TFileName;
    procedure FRZPanelAdd (AAlignment : TAlignment; AText : TCaption; AWidth : Integer);
    procedure FRZPanelsAdd;
  protected
    {Events of Properties}
    procedure SetRZStatus (const AValue: rz_an_type_Status);
    procedure SetRZLanguage (const AValue: rz_an_type_Language);
    procedure SetRZCaretPosX (const AValue: Integer);
    procedure SetRZCaretPosY (const AValue: Integer);
    procedure SetRZFileName (const AValue: TFileName);
    {Properties}
    property RZIndex : TRZANStatusBarIndex read FRZIndex write FRZIndex;
    property RZStatus : rz_an_type_Status read FRZStatus write SetRZStatus;
    property RZLanguage : rz_an_type_Language read FRZLanguage write SetRZLanguage;
    property RZCaretPosX : Integer read FRZCaretPosX write SetRZCaretPosX;
    property RZCaretPosY : Integer read FRZCaretPosY write SetRZCaretPosY;
    property RZFileName : TFileName read FRZFileName write SetRZFileName;
  public
    constructor Create (AOwner : TComponent); override;
  end;

  TRZANStatusBar = class(TRZANCustomStatusBar)
  public
    property RZIndex;
    property RZStatus;
    property RZLanguage;
    property RZCaretPosX;
    property RZCaretPosY;
    property RZFileName;
  end;

implementation

{Status Bar Index}

constructor TRZANStatusBarIndex.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  Self.RZStatus := 0;
  Self.RZLanguage := 1;
  Self.RZCaret := 2;
  Self.RZFileName := 3;
end;

{Status Bar}

constructor TRZANCustomStatusBar.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  Self.FRZPanelsAdd;
  Self.RZIndex := TRZANStatusBarIndex.Create(Self);
end;

procedure TRZANCustomStatusBar.FRZPanelAdd (AAlignment : TAlignment; AText : TCaption; AWidth : Integer);
begin
  with Self.Panels.Add do
  begin
    Alignment := AAlignment;
    Text := AText;
    Width := AWidth;
  end;
end;

procedure TRZANCustomStatusBar.FRZPanelsAdd;
begin
  Self.FRZPanelAdd(taCenter,'Ready',100);
  Self.FRZPanelAdd(taCenter,'Text',100);
  Self.FRZPanelAdd(taCenter,'1:0',150);
  Self.FRZPanelAdd(taLeftJustify,'',50);
end;

procedure TRZANCustomStatusBar.SetRZStatus (const AValue : rz_an_type_Status);
begin
  Self.FRZStatus := AValue;
  if Self.RZStatus = rz_an_type_status_Ready then Self.Panels[Self.RZIndex.RZStatus].Text := 'Ready'
    else if Self.RZStatus = rz_an_type_status_Modified then Self.Panels[Self.RZIndex.RZStatus].Text := 'Modified'
    else if Self.RZStatus = rz_an_type_status_Saved then Self.Panels[Self.RZIndex.RZStatus].Text := 'Saved'
  ;
end;

procedure TRZANCustomStatusBar.SetRZLanguage (const AValue : rz_an_type_Language);
begin
  Self.FRZLanguage := AValue;
  if Self.RZLanguage = rz_an_type_lang_Java then Self.Panels[Self.RZIndex.RZLanguage].Text := 'Java'
    else if Self.RZLanguage = rz_an_type_lang_Pascal then Self.Panels[Self.RZIndex.RZLanguage].Text := 'Pascal'
    else if Self.RZLanguage = rz_an_type_lang_Python then Self.Panels[Self.RZIndex.RZLanguage].Text := 'Python'
    else if Self.RZLanguage = rz_an_type_lang_Text then Self.Panels[Self.RZIndex.RZLanguage].Text := 'Text'
  ;
end;

procedure TRZANCustomStatusBar.SetRZCaretPosX (const AValue : Integer);
begin
  Self.FRZCaretPosX := AValue;
  Self.Panels[Self.RZIndex.RZCaret].Text := IntToStr(Self.RZCaretPosY + 1) + ':'
    + IntToStr(Self.RZCaretPosX)
  ;
end;

procedure TRZANCustomStatusBar.SetRZCaretPosY (const AValue : Integer);
begin
  Self.FRZCaretPosY := AValue;
  Self.Panels[Self.RZIndex.RZCaret].Text := IntToStr(Self.RZCaretPosY + 1) + ':'
    + IntToStr(Self.RZCaretPosX)
  ;
end;

procedure TRZANCustomStatusBar.SetRZFileName (const AValue : TFileName);
begin
  Self.FRZFileName := AValue;
  Self.Panels[Self.RZIndex.RZFileName].Text := Self.RZFileName;
end;

end.

