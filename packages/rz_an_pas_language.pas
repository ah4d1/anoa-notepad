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

unit rz_an_pas_language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, RichMemo, rz_an_pas_reserved_word;

type
  TRZANLanguage = class(TComponent)
  public
    procedure AllParsing (AMemo : TRichMemo; AReservedWord : TStrings);
    procedure LineParsing (AMemo : TRichMemo; AReservedWord : TStrings);
    procedure Reset (AMemo : TRichMemo);
  end;

implementation

procedure TRZANLanguage.AllParsing (AMemo : TRichMemo; AReservedWord : TStrings);
var
  i : LongInt;
  //
  LPosDelimiter : LongInt;
  LWord : string;
  LWordRest : string;
  //
  j : Word;
  LReservedWord : string;
  LFont : TFont;
  LAbsolutePos : LongInt;
begin
  if AMemo.Lines.Count <= 0 then Exit;
  if AReservedWord.Count <= 0 then Exit;

  Self.Reset(AMemo);

  LFont := TFont.Create;
  LFont.Name := AMemo.Font.Name;
  LFont.Quality := AMemo.Font.Quality;
  LFont.Size := AMemo.Font.Size;
  LFont.Color := AMemo.Font.Color;

  LFont.Style := [];
  AMemo.SelectAll;
  AMemo.SetTextAttributes(AMemo.SelStart,AMemo.SelLength,LFont);
  AMemo.SelStart := 0;

  if AReservedWord.Count < 1 then Exit;

  LAbsolutePos := 0;
  AMemo.Font.Style := [];
  for i := 0 to AMemo.Lines.Count - 1 do
  begin
    LWord := '';
    LWordRest := AMemo.Lines[i];
    repeat
      LPosDelimiter := Pos(' ',LWordRest);
      if LPosDelimiter > 0 then
      begin
        LWord := Copy(LWordRest,0,LPosDelimiter-1);
        LWordRest := Copy(LWordRest,LPosDelimiter+1,Length(LWordRest));
        LAbsolutePos := LAbsolutePos + Length(LWord);
        for j := 0 to AReservedWord.Count - 1 do
        begin
          LReservedWord := AReservedWord.Strings[j];
          if (LWord = LReservedWord) then
          begin
            LFont.Style := [fsBold];
            AMemo.SetTextAttributes(LAbsolutePos-Length(LWord),Length(LWord),LFont);
          end;
        end;
        LAbsolutePos := LAbsolutePos + 1; // new word
      end
      else
      begin
        LWord := LWordRest;
        LWordRest := '';
        LAbsolutePos := LAbsolutePos + Length(LWord);
        for j := 0 to AReservedWord.Count - 1 do
        begin
          LReservedWord := AReservedWord.Strings[j];
          if (LWord = LReservedWord) then
          begin
            LFont.Style := [fsBold];
            AMemo.SetTextAttributes(LAbsolutePos-Length(LWord),Length(LWord),LFont);
          end;
        end;
      end;
    until Trim(LWordRest) = '';
    LAbsolutePos := LAbsolutePos + 1; // new line
  end;
  LFont.Free;
end;

procedure TRZANLanguage.LineParsing (AMemo : TRichMemo; AReservedWord : TStrings);
var
  i : LongInt;
  //
  LPosDelimiter : LongInt;
  LWord : string;
  LWordRest : string;
  //
  j : Word;
  LReservedWord : string;
  LFont : TFont;
  LAbsolutePos : LongInt;
begin
  if AMemo.Lines.Count <= 0 then Exit;
  if AReservedWord.Count <= 0 then Exit;

  LFont := TFont.Create;
  LFont.Name := AMemo.Font.Name;
  LFont.Quality := AMemo.Font.Quality;
  LFont.Size := AMemo.Font.Size;
  LFont.Color := AMemo.Font.Color;

  LAbsolutePos := AMemo.SelStart - AMemo.CaretPos.X;
  LFont.Style := [];
  AMemo.SetTextAttributes(LAbsolutePos,Length(AMemo.Lines[AMemo.CaretPos.Y]),LFont);

  LWord := '';
  LWordRest := AMemo.Lines[AMemo.CaretPos.Y];
  repeat
    LPosDelimiter := Pos(' ',LWordRest);
    if LPosDelimiter > 0 then
    begin
      LWord := Copy(LWordRest,0,LPosDelimiter-1);
      LWordRest := Copy(LWordRest,LPosDelimiter+1,Length(LWordRest));
      LAbsolutePos := LAbsolutePos + Length(LWord);
      for j := 0 to AReservedWord.Count - 1 do
      begin
        LReservedWord := AReservedWord.Strings[j];
        if (LWord = LReservedWord) then
        begin
          LFont.Style := [fsBold];
          AMemo.SetTextAttributes(LAbsolutePos-Length(LWord),Length(LWord),LFont);
        end;
      end;
      LAbsolutePos := LAbsolutePos + 1; // new word
    end
    else
    begin
      LWord := LWordRest;
      LWordRest := '';
      LAbsolutePos := LAbsolutePos + Length(LWord);
      for j := 0 to AReservedWord.Count - 1 do
      begin
        LReservedWord := AReservedWord.Strings[j];
        if (LWord = LReservedWord) then
        begin
          LFont.Style := [fsBold];
          AMemo.SetTextAttributes(LAbsolutePos-Length(LWord),Length(LWord),LFont);
        end;
      end;
    end;
  until Trim(LWordRest) = '';
  LFont.Free;
end;

procedure TRZANLanguage.Reset (AMemo : TRichMemo);
var
  LFont : TFont;
begin
  LFont := TFont.Create;
  LFont.Name := AMemo.Font.Name;
  LFont.Quality := AMemo.Font.Quality;
  LFont.Size := AMemo.Font.Size;
  LFont.Style := [];
  AMemo.SelectAll;
  AMemo.SetTextAttributes(AMemo.SelStart,AMemo.SelLength,LFont);
  AMemo.SelStart := 0;
end;

end.

