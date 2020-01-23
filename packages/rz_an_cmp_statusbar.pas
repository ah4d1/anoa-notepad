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

unit rz_an_cmp_statusbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Controls, Dialogs, rz_an_pas_var;

type

  TRZANStatusBarIndex = class(TComponent)
  private
    FStatus : Byte;
    FLanguage : Byte;
    FCaret : Byte;
    FFileName : Byte;
  public
    property Status : Byte read FStatus write FStatus;
    property Language : Byte read FLanguage write FLanguage;
    property Caret : Byte read FCaret write FCaret;
    property FileName : Byte read FFileName write FFileName;
    constructor Create (AOwner : TComponent); override;
  end;

  TRZANCustomStatusBar = class(TStatusBar)
  private
    FCaretPosX : Integer;
    FCaretPosY : Integer;
    FFileName : TFileName;
    FIndex : TRZANStatusBarIndex;
    FLanguage : rz_an_type_Language;
    FStatus : rz_an_type_Status;
    procedure _PanelAdd(AAlignment : TAlignment; AText : TCaption; AWidth : Integer);
    procedure _PanelsAdd;
  protected
    function GetCaretPosX : Integer;
    procedure SetCaretPosX (const AValue: Integer);
    function GetCaretPosY : Integer;
    procedure SetCaretPosY (const AValue: Integer);
    function GetFileName : TFileName;
    procedure SetFileName (const AValue: TFileName);
    function GetLanguage : rz_an_type_Language;
    procedure SetLanguage (const AValue: rz_an_type_Language);
    function GetStatus : rz_an_type_Status;
    procedure SetStatus (const AValue: rz_an_type_Status);
    property CaretPosX : Integer read GetCaretPosX write SetCaretPosX;
    property CaretPosY : Integer read GetCaretPosY write SetCaretPosY;
    property FileName : TFileName read GetFileName write SetFileName;
    property Index : TRZANStatusBarIndex read FIndex write FIndex;
    property Language : rz_an_type_Language read GetLanguage write SetLanguage;
    property Status : rz_an_type_Status read GetStatus write SetStatus;
  public
    constructor Create (AOwner : TComponent); override;
  end;

  TRZANStatusBar = class(TRZANCustomStatusBar)
  public
    property CaretPosX;
    property CaretPosY;
    property FileName;
    property Index;
    property Language;
    property Status;
  end;

implementation

{Status Bar Index}

constructor TRZANStatusBarIndex.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  Self.Status := 0;
  Self.Language := 1;
  Self.Caret := 2;
  Self.FileName := 3;
end;

{Status Bar}

constructor TRZANCustomStatusBar.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  Self._PanelsAdd;
  Self.Index := TRZANStatusBarIndex.Create(Self);
end;

procedure TRZANCustomStatusBar._PanelAdd(AAlignment : TAlignment; AText : TCaption; AWidth : Integer);
begin
  with Self.Panels.Add do
  begin
    Alignment := AAlignment;
    Text := AText;
    Width := AWidth;
  end;
end;

procedure TRZANCustomStatusBar._PanelsAdd;
begin
  Self._PanelAdd(taCenter,'Ready',100);
  Self._PanelAdd(taCenter,'Text',100);
  Self._PanelAdd(taCenter,'1:0',150);
  Self._PanelAdd(taLeftJustify,'',50);
end;

function TRZANCustomStatusBar.GetStatus : rz_an_type_Status;
begin
  Result := Self.FStatus;
end;

procedure TRZANCustomStatusBar.SetStatus (const AValue : rz_an_type_Status);
begin
  Self.FStatus := AValue;
  if Self.Status = rz_an_type_status_Ready then Self.Panels[Self.Index.Status].Text := 'Ready'
    else if Self.Status = rz_an_type_status_Modified then Self.Panels[Self.Index.Status].Text := 'Modified'
    else if Self.Status = rz_an_type_status_Saved then Self.Panels[Self.Index.Status].Text := 'Saved'
  ;
end;

function TRZANCustomStatusBar.GetLanguage : rz_an_type_Language;
begin
  Result := Self.FLanguage;
end;

procedure TRZANCustomStatusBar.SetLanguage (const AValue : rz_an_type_Language);
begin
  Self.FLanguage := AValue;
  if Self.Language = rz_an_type_lang_Java then Self.Panels[Self.Index.Language].Text := 'Java'
    else if Self.Language = rz_an_type_lang_Pascal then Self.Panels[Self.Index.Language].Text := 'Pascal'
    else if Self.Language = rz_an_type_lang_Python then Self.Panels[Self.Index.Language].Text := 'Python'
    else if Self.Language = rz_an_type_lang_Text then Self.Panels[Self.Index.Language].Text := 'Text'
  ;
end;

function TRZANCustomStatusBar.GetCaretPosX : Integer;
begin
  Result := Self.FCaretPosX;
end;

procedure TRZANCustomStatusBar.SetCaretPosX (const AValue : Integer);
begin
  Self.FCaretPosX := AValue;
  Self.Panels[Self.Index.Caret].Text := IntToStr(Self.CaretPosY + 1) + ':' + IntToStr(Self.CaretPosX);
end;

function TRZANCustomStatusBar.GetCaretPosY : Integer;
begin
  Result := Self.FCaretPosY;
end;

procedure TRZANCustomStatusBar.SetCaretPosY (const AValue : Integer);
begin
  Self.FCaretPosY := AValue;
  Self.Panels[Self.Index.Caret].Text := IntToStr(Self.CaretPosY + 1) + ':' + IntToStr(Self.CaretPosX);
end;

function TRZANCustomStatusBar.GetFileName : TFileName;
begin
  Result := Self.FFileName;
end;

procedure TRZANCustomStatusBar.SetFileName (const AValue : TFileName);
begin
  Self.FFileName := AValue;
  Self.Panels[Self.Index.FileName].Text := Self.FileName;
end;

end.

