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

unit rz_an_cmp_richmemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Graph, ComCtrls, Dialogs,
  StdCtrls, RichMemo, LCLType, LCLIntf, Messages, Clipbrd, rz_an_pas_var;

type

  TRZANCustomRichMemo = class(TRichMemo)
  private
    {3. Text Editing}
    {3.90}
    FRZClipboard : TClipboard;
    {7. Caret}
    {7.1}
    FRZCaretPosX : Integer;
    {7.2}
    FRZCaretPosY : Integer;
  protected
    {3. Text Editing}
    {3.90}
    property RZClipboard : TClipboard read FRZClipboard write FRZClipboard;
    {7. Caret}
    {7.1}
    procedure SetRZCaretPosX (const AValue: Integer);
    {7.2}
    procedure SetRZCaretPosY (const AValue: Integer);
    {7. Caret}
    {7.1}
    property RZCaretPosX : Integer read FRZCaretPosX write SetRZCaretPosX;
    {7.2}
    property RZCaretPosY : Integer read FRZCaretPosY write SetRZCaretPosY;
    {8. Events}
    {8.1}
    procedure Change; override;
    {8.2}
    procedure DoEnter; override;
    {8.3}
    procedure KeyPress (var Key: char); override;
    {8.4}
    procedure KeyDown (var Key: Word; Shift: TShiftState); override;
    {8.5}
    procedure KeyUp (var Key: Word; Shift: TShiftState); override;
    {8.6}
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {8.7}
    procedure MouseWheel (Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    {8.90}
    procedure RZOnEventUpdate;
  public
    {1. Application}
    {1.1.3}
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

  TRZANRichMemo = class(TRZANCustomRichMemo)
  public
    {7. Caret}
    {7.1}
    property RZCaretPosX;
    {7.2}
    property RZCaretPosY;
  end;

implementation

uses
  rz_an_cmp_tabsheet;

{1.1.3}
constructor TRZANCustomRichMemo.Create (AOwner : TComponent);
begin
  {1. Application}
  {1.1}
  inherited Create(AOwner);
  Self.Color := clWhite;
  Self.Font.Name := 'Courier New';
  Self.Font.Size := 10;
  Self.Font.Color := clBlack;
  Self.Font.Quality := fqDraft;
  Self.ScrollBars := ssBoth;
  {3. Text Editing}
  {3.90}
  Self.RZClipboard := TClipboard.Create;
end;

{2. File Operation}

{2.3}
procedure TRZANCustomRichMemo.RZOpen (AFileName : TFileName);
begin
  Self.Lines.LoadFromFile(AFileName);
end;

{2.4}
procedure TRZANCustomRichMemo.RZSave (AFileName : TFileName);
begin
  Self.Lines.SaveToFile(AFileName);
end;

{3. Text Editing}

{3.1}
procedure TRZANCustomRichMemo.RZUndo;
begin
  Self.Undo;
end;

{3.2}
procedure TRZANCustomRichMemo.RZRedo;
begin
  Self.Redo;
end;

{3.3}
procedure TRZANCustomRichMemo.RZCopy;
begin
  Self.RZClipboard.Clear;
  Self.RZClipboard.AsText := Self.SelText;
end;

{3.4}
procedure TRZANCustomRichMemo.RZCut;
begin
  Self.RZClipboard.AsText := Self.SelText;
  Self.SelText := '';
end;

{3.5}
procedure TRZANCustomRichMemo.RZPaste;
begin
  Self.SelText := Self.RZClipboard.AsText;
end;

{3.6}
procedure TRZANCustomRichMemo.RZSelectAll;
begin
  Self.SelectAll;
end;

{7. Caret}

{7.1}
procedure TRZANCustomRichMemo.SetRZCaretPosX (const AValue : Integer);
begin
  Self.FRZCaretPosX := AValue;
  if (Self.Parent <> nil) then (Self.Parent as TRZANTabSheet).RZCaretPosX := Self.RZCaretPosX;
end;

{7.2}
procedure TRZANCustomRichMemo.SetRZCaretPosY (const AValue : Integer);
begin
  Self.FRZCaretPosY := AValue;
  if (Self.Parent <> nil) then (Self.Parent as TRZANTabSheet).RZCaretPosY := Self.RZCaretPosY + 1;
end;

{8. Events}

{8.1}
procedure TRZANCustomRichMemo.Change;
begin
  Self.RZOnEventUpdate;
  (Self.Parent as TRZANTabSheet).RZStatus := rz_an_type_status_Modified;
  inherited;
end;

{8.2}
procedure TRZANCustomRichMemo.DoEnter;
begin
  Self.RZOnEventUpdate;
  inherited;
end;

{8.3}
procedure TRZANCustomRichMemo.KeyDown (var Key: Word; Shift: TShiftState);
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
procedure TRZANCustomRichMemo.KeyPress (var Key: char);
begin
  Self.RZOnEventUpdate;
  inherited;
end;

{8.5}
procedure TRZANCustomRichMemo.KeyUp (var Key: Word; Shift: TShiftState);
begin
  Self.RZOnEventUpdate;
  inherited;
end;

{8.6}
procedure TRZANCustomRichMemo.MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Self.RZOnEventUpdate;
  inherited;
end;

{8.7}
procedure TRZANCustomRichMemo.MouseWheel (Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Self.RZOnEventUpdate;
  inherited;
end;

{8.7}
procedure TRZANCustomRichMemo.RZOnEventUpdate;
begin
  Self.RZCaretPosX := Self.CaretPos.X;
  Self.RZCaretPosY := Self.CaretPos.Y;
end;

end.

