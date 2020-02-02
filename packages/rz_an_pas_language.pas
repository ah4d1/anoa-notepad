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

unit rz_an_pas_language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, RichMemo, rz_an_pas_reserved_word, rz_an_pas_tools;

type
  TRZANLanguage = class(TComponent)
  private
    function FRZDelimiterPos (AText : WideString) : TStringList;
  public
    procedure RZReset (AMemo : TRichMemo);
    procedure RZLineParsing (AMemo : TRichMemo; AReservedWord : TStrings;
      ALineText : WideString; AAbsolutePos : LongInt);
    procedure RZAllParsing (AMemo : TRichMemo; AReservedWord : TStrings);
  end;

implementation

function TRZANLanguage.FRZDelimiterPos (AText : WideString) : TStringList;
var
  LText : WideString;
  i : Integer;
  LPosArr : TStringList;
begin
  LText := AText;
  LText := StringReplace(LText,';',' ',[rfReplaceAll]);
  LText := StringReplace(LText,'.',' ',[rfReplaceAll]);
  LText := StringReplace(LText,Chr(9),' ',[rfReplaceAll]); // tab
  LText := StringReplace(LText,Chr(10),' ',[rfReplaceAll]); // line feed
  LText := StringReplace(LText,Chr(13),' ',[rfReplaceAll]); // carriage return
  LText := StringReplace(LText,#13#10,' ',[rfReplaceAll]);
  LPosArr := TStringList.Create;
  LPosArr.Add(IntToStr(0));
  for i := 1 to Length(LText) do
  begin
    if LText[i] = ' ' then LPosArr.Add(IntToStr(i));
  end;
  LPosArr.Add(IntToStr(Length(LText)+1));
  Result := LPosArr;
end;

procedure TRZANLanguage.RZReset (AMemo : TRichMemo);
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
  LFont.Free;
end;

procedure TRZANLanguage.RZLineParsing (AMemo : TRichMemo; AReservedWord : TStrings;
  ALineText : WideString; AAbsolutePos : LongInt);
var
  LAbsolutePos : LongInt;
  LFont : TFont;
  LLineText : WideString;
  LDelimiterPos : TStringList;
  i,j : Integer;
  LWord : string;
  LReservedWord : string;
begin
  LAbsolutePos := AAbsolutePos;
  LLineText := ALineText;
  {Initial Font Setup}
  LFont := TFont.Create;
  LFont.Name := AMemo.Font.Name;
  LFont.Quality := AMemo.Font.Quality;
  LFont.Size := AMemo.Font.Size;
  LFont.Color := AMemo.Font.Color;
  {Initial Text Attributes}
  AMemo.SetTextAttributes(LAbsolutePos,Length(LLineText),LFont);
  {Parsing}
  LDelimiterPos := Self.FRZDelimiterPos(LLineText);
  for i := 1 to LDelimiterPos.Count-1 do
  begin
    LWord := Copy(LLineText,StrToInt(LDelimiterPos[i-1])+1,
      StrToInt(LDelimiterPos[i])-StrToInt(LDelimiterPos[i-1])-1)
    ;
    for j := 0 to AReservedWord.Count - 1 do
    begin
      LReservedWord := AReservedWord.Strings[j];
      if (LWord = LReservedWord) then
      begin
        LFont.Style := [fsBold];
        AMemo.SetTextAttributes(LAbsolutePos,Length(LWord),LFont);
      end;
    end;
    LAbsolutePos := LAbsolutePos + Length(LWord) + 1; // + 1 for delimiter
  end;
  {Free Memory}
  LFont.Free;
end;

procedure TRZANLanguage.RZAllParsing (AMemo : TRichMemo; AReservedWord : TStrings);
var
  i : LongInt;
  LLineText : WideString;
  LAbsolutePos : LongInt;
begin
  if AMemo.Lines.Count <= 0 then Exit;
  if AReservedWord.Count <= 0 then Exit;
  Self.RZReset(AMemo);
  LAbsolutePos := 0;
  for i := 0 to AMemo.Lines.Count-1 do
  begin
    AMemo.CaretPos := Point(0,i);
    LLineText := AMemo.Lines[i];
    LAbsolutePos := AMemo.SelStart;
    Self.RZLineParsing(AMemo,AReservedWord,LLineText,LAbsolutePos);
  end;
  AMemo.CaretPos := Point(0,0);
end;

end.

