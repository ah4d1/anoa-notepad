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

unit rz_an_pas_tabsheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Controls, Dialogs, Graphics, rz_an_cmp_richmemo, rz_an_cmp_statusbar,
  rz_an_pas_opendialog, rz_an_pas_savedialog, rz_an_pas_reserved_word, rz_an_pas_language, rz_an_pas_var;

type
  TRZANCustomTabSheet = class(TTabSheet)
  private
    {Properties - StatusBar}
    FStatus : rz_an_type_Status;
    FCaretPosX : Integer;
    FCaretPosY : Integer;
    FLanguage : rz_an_type_Language;
    FLanguageProc : TRZANLanguage;
    FReservedWord : TStrings;
    FReservedWordDict : TRZANReservedWord;
    FFileName : TFileName;
    FFileNameOnly : string;
    FFileNameExt : string;
    {Properties - Misc}
    FDoParsing : Boolean;
    {Sub-Components}
    FStatusBar : TRZANStatusBar;
  protected
    {Methods - StatusBar}
    function GetStatus : rz_an_type_Status;
    procedure SetStatus (const AValue: rz_an_type_Status);
    function GetCaretPosX : Integer;
    procedure SetCaretPosX (const AValue: Integer);
    function GetCaretPosY : Integer;
    procedure SetCaretPosY (const AValue: Integer);
    function GetLanguage : rz_an_type_Language;
    procedure SetLanguage (const AValue: rz_an_type_Language);
    function GetFileName : TFileName;
    procedure SetFileName (const AValue : TFileName);
    function GetFileNameOnly : string;
    procedure SetFileNameOnly (const AValue: string);
    function GetFileNameExt : string;
    procedure SetFileNameExt (const AValue: string);
    {Methods - Misc}
    function GetDoParsing : Boolean;
    procedure SetDoParsing (const AValue: Boolean);
    function GetStatusBar : TRZANStatusBar;
    procedure SetStatusBar (const AValue: TRZANStatusBar);
    {Properties - StatusBar}
    property Status : rz_an_type_Status read GetStatus write SetStatus;
    property CaretPosX : Integer read GetCaretPosX write SetCaretPosX;
    property CaretPosY : Integer read GetCaretPosY write SetCaretPosY;
    property Language : rz_an_type_Language read GetLanguage write SetLanguage;
    property LanguageProc : TRZANLanguage read FLanguageProc write FLanguageProc;
    property ReservedWord : TStrings read FReservedWord write FReservedWord;
    property ReservedWordDict : TRZANReservedWord read FReservedWordDict write FReservedWordDict;
    property FileName : TFileName read GetFileName write SetFileName;
    property FileNameOnly : string read GetFileNameOnly write SetFileNameOnly;
    property FileNameExt : string read GetFileNameExt write SetFileNameExt;
    {Properties - Misc}
    property DoParsing : Boolean read GetDoParsing write SetDoParsing;
    {Sub-Components}
    property StatusBar : TRZANStatusBar read GetStatusBar write SetStatusBar;
  public
    {Sub-Components}
    Editor : TRZANRichMemo;
    {Methods}
    constructor Create(AOwner : TComponent); override;
    procedure OpenFile (AFileName : TFileName);
    procedure SaveFile (AFileName : TFileName);
    procedure SubComponentUpdate;
    procedure Undo;
    procedure Redo;
    procedure Copy;
    procedure Cut;
    procedure Paste;
    procedure SelectAll;
    {Events}
    procedure DoShow; override;
  end;

  TRZANTabSheet = class(TRZANCustomTabSheet)
  public
    {Properties}
    property Status;
    property Language;
    property CaretPosX;
    property CaretPosY;
    property FileName;
    property DoParsing;
    {Sub-Components}
    property StatusBar;
  end;

implementation

{Methods}

constructor TRZANCustomTabSheet.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  {Properties}
  Self.LanguageProc := TRZANLanguage.Create(Self);
  Self.ReservedWord := TStrings.Create;
  Self.ReservedWordDict := TRZANReservedWord.Create(Self);
  {Sub-Components}
  Self.Editor := TRZANRichMemo.Create(Self);
  Self.Editor.Parent := Self;
  Self.Editor.Align := alClient;
end;

procedure TRZANCustomTabSheet.DoShow;
begin
  Self.Editor.SetFocus;
  inherited;
end;

procedure TRZANCustomTabSheet.OpenFile (AFileName : TFileName);
begin
  Self.DoParsing := True;
  Self.FileName := AFileName;
  Self.Editor.Open(Self.FileName);
end;

procedure TRZANCustomTabSheet.SaveFile (AFileName : TFileName);
begin
  Self.FileName := AFileName;
  Self.Editor.Save(Self.FileName);
end;

procedure TRZANCustomTabSheet.SubComponentUpdate;
begin
  Self.Status := Self.Editor.Status;
  Self.CaretPosX := Self.Editor.CaretPosX;
  Self.CaretPosY := Self.Editor.CaretPosY;
end;

procedure TRZANCustomTabSheet.Undo;
begin
  Self.Editor.DoUndo;
end;

procedure TRZANCustomTabSheet.Redo;
begin
  Self.Editor.DoRedo;
end;

procedure TRZANCustomTabSheet.Copy;
begin
  Self.Editor.DoCopy;
end;

procedure TRZANCustomTabSheet.Cut;
begin
  Self.Editor.DoCut;
end;

procedure TRZANCustomTabSheet.Paste;
begin
  Self.Editor.DoPaste;
end;

procedure TRZANCustomTabSheet.SelectAll;
begin
  Self.Editor.DoSelectAll;
end;

{Properties}

function TRZANCustomTabSheet.GetCaretPosX : Integer;
begin
  Result := Self.FCaretPosX;
end;

procedure TRZANCustomTabSheet.SetCaretPosX (const AValue : Integer);
begin
  Self.FCaretPosX := AValue;
  Self.Editor.CaretPosX := Self.CaretPosX;
end;

function TRZANCustomTabSheet.GetCaretPosY : Integer;
begin
  Result := Self.FCaretPosY;
end;

procedure TRZANCustomTabSheet.SetCaretPosY (const AValue : Integer);
begin
  Self.FCaretPosY := AValue;
  Self.Editor.CaretPosY := Self.CaretPosY;
end;

function TRZANCustomTabSheet.GetDoParsing : Boolean;
begin
  Result := Self.FDoParsing;
end;

procedure TRZANCustomTabSheet.SetDoParsing (const AValue: Boolean);
begin
  Self.FDoParsing := AValue;
  Self.Editor.DoParsing := Self.DoParsing;
end;

function TRZANCustomTabSheet.GetFileName : TFileName;
begin
  Result := Self.FFileName;
end;

procedure TRZANCustomTabSheet.SetFileName (const AValue : TFileName);
begin
  Self.FFileName := AValue;
  Self.FileNameOnly := ExtractFileName(Self.FileName);
  Self.FileNameExt := ExtractFileExt(Self.FileName);
  Self.Editor.FileName := Self.FileName;
end;

function TRZANCustomTabSheet.GetFileNameOnly : string;
begin
  Result := Self.FFileNameOnly;
end;

procedure TRZANCustomTabSheet.SetFileNameOnly (const AValue: string);
begin
  Self.FFileNameOnly := AValue;
  if Trim(Self.FileNameOnly) <> '' then Self.Caption := Self.FileNameOnly;
end;

function TRZANCustomTabSheet.GetFileNameExt : string;
begin
  Result := Self.FFileNameExt;
end;

procedure TRZANCustomTabSheet.SetFileNameExt (const AValue: string);
begin
  Self.FFileNameExt := AValue;
  Self.Editor.FileNameExt := Self.FileNameExt;
end;

function TRZANCustomTabSheet.GetLanguage : rz_an_type_Language;
begin
  Result := Self.FLanguage;
end;

procedure TRZANCustomTabSheet.SetLanguage (const AValue: rz_an_type_Language);
begin
  Self.FLanguage := AValue;
  Self.Editor.Language := Self.Language;
end;

function TRZANCustomTabSheet.GetStatus : rz_an_type_Status;
begin
  Result := Self.FStatus;
end;

procedure TRZANCustomTabSheet.SetStatus (const AValue: rz_an_type_Status);
begin
  Self.FStatus := AValue;
  Self.Editor.Status := Self.Status;
end;

function TRZANCustomTabSheet.GetStatusBar : TRZANStatusBar;
begin
  Result := Self.FStatusBar;
end;

procedure TRZANCustomTabSheet.SetStatusBar (const AValue: TRZANStatusBar);
begin
  Self.FStatusBar := AValue;
  Self.Editor.StatusBar := Self.StatusBar;
end;

end.

