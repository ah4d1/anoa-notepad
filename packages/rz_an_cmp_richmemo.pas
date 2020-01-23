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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, ComCtrls, Dialogs, StdCtrls, RichMemo,
  LCLType, Messages, rz_an_pas_var, rz_an_pas_opendialog, rz_an_pas_savedialog, rz_an_cmp_statusbar,
  rz_an_pas_reserved_word, rz_an_pas_language;

type
  TRZANCustomRichMemo = class(TRichMemo)
  private
    FStatus : rz_an_type_Status;
    FLanguage : rz_an_type_Language;
    FLanguageProc : TRZANLanguage;
    FCaretPosX : Integer;
    FCaretPosY : Integer;
    FFileName : TFileName;
    FFileNameOnly : string;
    FFileNameExt : string;
    FDoParsing : Boolean;
    FReservedWord : TStrings;
    FReservedWordDict : TRZANReservedWord;
    FStatusBar : TRZANStatusBar;
    FStyle : rz_an_type_Style;
    FTabSheet : TTabSheet;
    procedure _SetTabSet;
    procedure _SetStatusBarCaret;
    procedure _SetStyle (AStyle : rz_an_type_Style);
  protected
    {Functions of Properties}
    function GetStatus : rz_an_type_Status;
    procedure SetStatus (const AValue: rz_an_type_Status);
    function GetLanguage : rz_an_type_Language;
    procedure SetLanguage (const AValue: rz_an_type_Language);
    function GetCaretPosX : Integer;
    procedure SetCaretPosX (const AValue: Integer);
    function GetCaretPosY : Integer;
    procedure SetCaretPosY (const AValue: Integer);
    function GetFileName : TFileName;
    procedure SetFileName (const AValue: TFileName);
    function GetFileNameOnly : string;
    procedure SetFileNameOnly (const AValue: string);
    function GetFileNameExt : string;
    procedure SetFileNameExt (const AValue: string);
    function GetStyle : rz_an_type_Style;
    procedure SetStyle (const AValue: rz_an_type_Style);
    function GetTabSheet : TTabSheet;
    procedure SetTabSheet (const AValue: TTabSheet);
    {Properties}
    property Status : rz_an_type_Status read GetStatus write SetStatus;
    property Language : rz_an_type_Language read GetLanguage write SetLanguage;
    property LanguageProc : TRZANLanguage read FLanguageProc write FLanguageProc;
    property CaretPosX : Integer read GetCaretPosX write SetCaretPosX;
    property CaretPosY : Integer read GetCaretPosY write SetCaretPosY;
    property FileName : TFileName read GetFileName write SetFileName;
    property FileNameOnly : string read GetFileNameOnly write SetFileNameOnly;
    property FileNameExt : string read GetFileNameExt write SetFileNameExt;
    property DoParsing : Boolean read FDoParsing write FDoParsing;
    property ReservedWord : TStrings read FReservedWord write FReservedWord;
    property ReservedWordDict : TRZANReservedWord read FReservedWordDict write FReservedWordDict;
    property StatusBar : TRZANStatusBar read FStatusBar write FStatusBar;
    property Style : rz_an_type_Style read GetStyle write SetStyle;
    property TabSheet : TTabSheet read GetTabSheet write SetTabSheet;
    {Events}
    procedure Change; override;
    procedure DoEnter; override;
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress (var Key: char); override;
    procedure KeyDown (var Key: Word; Shift: TShiftState); override;
    procedure KeyUp (var Key: Word; Shift: TShiftState); override;
  public
    constructor Create (AOwner : TComponent); override;
    procedure Open (AFileName : TFileName);
    procedure Save (AFileName : TFileName);
    procedure DoUndo;
    procedure DoRedo;
    procedure DoCopy;
    procedure DoCut;
    procedure DoPaste;
    procedure DoSelectAll;
  end;

  TRZANRichMemo = class(TRZANCustomRichMemo)
  public
    property CaretPosX;
    property CaretPosY;
    property FileName;
    property FileNameOnly;
    property FileNameExt;
    property ReservedWord;
    property DoParsing;
  published
    property Status;
    property Language;
    property StatusBar;
    property Style;
    property TabSheet;
  end;

implementation

constructor TRZANCustomRichMemo.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  Self.LanguageProc := TRZANLanguage.Create(Self);
  Self.ReservedWord := TStrings.Create;
  Self.ReservedWordDict := TRZANReservedWord.Create(Self);
  Self.StatusBar := TRZANStatusBar.Create(Self);
  Self.DoParsing := True;
  Self.Language := rz_an_type_lang_Text;
  Self.Status := rz_an_type_status_Ready;
  // Self.Style := rz_an_type_style_Normal; // ERROR
  Self.Font.Name := 'Courier New';
  Self.Font.Size := 10;
  Self.Font.Color := clBlack;
  Self.Font.Quality := fqDraft;
  Self.Color := clWhite;
  Self.ScrollBars := ssBoth;
  Self._SetTabSet;
end;

{Privates}

procedure TRZANCustomRichMemo._SetTabSet; // not working
var
  LStopList: TTabStopList;
  LChrW: Integer;
  LTabStopsPt: array[0..99] of double;
  i : Byte;
begin
  LChrW := 5; // FormMain.Canvas.TextWidth('W');
  for i := 1 to 100 do LTabStopsPt[i-1] := i * LChrW;
  InitTabStopList(LStopList,LTabStopsPt);
  Self.SetParaTabs(1, 9999, LStopList);
end;

procedure TRZANCustomRichMemo._SetStatusBarCaret;
begin
  Self.StatusBar.CaretPosX := Self.CaretPos.X;
  Self.StatusBar.CaretPosY := Self.CaretPos.Y;
  Self.CaretPosX := Self.CaretPos.X;
  Self.CaretPosY := Self.CaretPos.Y;
end;

procedure TRZANCustomRichMemo._SetStyle (AStyle : rz_an_type_Style);
var
  LFont : TFont;
begin
  if (AStyle = rz_an_type_style_Normal) then
  begin
    Self.Color := rz_an_var_Style_Color_Normal;
    Self.Font.Color := rz_an_var_Style_FontColor_Normal;
  end
  else if (AStyle = rz_an_type_style_Dark) then
  begin
    Self.Color := rz_an_var_Style_Color_Dark;
    Self.Font.Color := rz_an_var_Style_FontColor_Dark;
  end;
  Self.SelectAll;
  LFont := TFont.Create;
  LFont.Color := Self.Font.Color;
  Self.SetTextAttributes(0,Self.SelLength,LFont);
  LFont.Free;
  Self.Language := Self.Language;
  Self.SelStart := 0;
end;

{Editor Format}

function TRZANCustomRichMemo.GetCaretPosX : Integer;
begin
  Result := Self.FCaretPosX;
end;

procedure TRZANCustomRichMemo.SetCaretPosX (const AValue : Integer);
begin
  Self.FCaretPosX := AValue;
  Self.StatusBar.CaretPosX := Self.CaretPosX;
end;

function TRZANCustomRichMemo.GetCaretPosY : Integer;
begin
  Result := Self.FCaretPosY;
end;

procedure TRZANCustomRichMemo.SetCaretPosY (const AValue : Integer);
begin
  Self.FCaretPosY := AValue;
  Self.StatusBar.CaretPosY := Self.CaretPosY;
end;

function TRZANCustomRichMemo.GetLanguage : rz_an_type_Language;
begin
  Result := Self.FLanguage;
end;

procedure TRZANCustomRichMemo.SetLanguage (const AValue: rz_an_type_Language);
begin
  Self.FLanguage := AValue;
  if Self.Language = rz_an_type_lang_Java then Self.ReservedWord := Self.ReservedWordDict.LangJava
    else if Self.Language = rz_an_type_lang_Pascal then Self.ReservedWord := Self.ReservedWordDict.LangPascal
    else if Self.Language = rz_an_type_lang_Python then Self.ReservedWord := Self.ReservedWordDict.LangPython
    else Self.ReservedWord := Self.ReservedWordDict.LangText
  ;
  if Self.DoParsing then Self.LanguageProc.AllParsing(Self,Self.ReservedWord);
  Self.StatusBar.Language := Self.Language;
  Self.DoParsing := False;
end;

{File Name}

function TRZANCustomRichMemo.GetFileName : TFileName;
begin
  Result := Self.FFileName;
end;

procedure TRZANCustomRichMemo.SetFileName (const AValue: TFileName);
begin
  Self.FFileName := AValue;
  Self.FileNameOnly := ExtractFileName(Self.FileName);
  Self.FileNameExt := ExtractFileExt(Self.FileName);
  Self.StatusBar.FileName := Self.FileName;
end;

function TRZANCustomRichMemo.GetFileNameOnly : string;
begin
  Result := Self.FFileNameOnly;
end;

procedure TRZANCustomRichMemo.SetFileNameOnly (const AValue: string);
begin
  Self.FFileNameOnly := AValue;
end;

function TRZANCustomRichMemo.GetFileNameExt : string;
begin
  Result := Self.FFileNameExt;
end;

procedure TRZANCustomRichMemo.SetFileNameExt (const AValue: string);
begin
  Self.FFileNameExt := AValue;
  if Self.FileNameExt = '.java' then Self.Language := rz_an_type_lang_Java
  else if Self.FileNameExt = '.pas' then Self.Language := rz_an_type_lang_Pascal
  else if Self.FileNameExt = '.py' then Self.Language := rz_an_type_lang_Python
  else Self.Language := rz_an_type_lang_Text;
end;

{Status}

function TRZANCustomRichMemo.GetStatus : rz_an_type_Status;
begin
  Result := Self.FStatus;
end;

procedure TRZANCustomRichMemo.SetStatus (const AValue: rz_an_type_Status);
begin
  Self.FStatus := AValue;
  Self.StatusBar.Status := Self.Status;
end;

{Style}

function TRZANCustomRichMemo.GetStyle : rz_an_type_Style;
begin
  Result := Self.FStyle;
end;

procedure TRZANCustomRichMemo.SetStyle (const AValue: rz_an_type_Style);
begin
  Self.FStyle := AValue;
  Self._SetStyle(AValue);
end;

{Tab Sheet}

function TRZANCustomRichMemo.GetTabSheet : TTabSheet;
begin
  Result := Self.FTabSheet;
end;

procedure TRZANCustomRichMemo.SetTabSheet (const AValue: TTabSheet);
begin
  Self.FTabSheet := AValue;
end;

{Common Function}

procedure TRZANCustomRichMemo.Open (AFileName : TFileName);
begin
  Self.FileName := AFileName;
  Self.Lines.LoadFromFile(Self.FileName);
  Self.LanguageProc.AllParsing(Self,Self.ReservedWord);
  Self.StatusBar.FileName := Self.FileName;
end;

procedure TRZANCustomRichMemo.Save (AFileName : TFileName);
begin
  Self.FileName := AFileName;
  Self.Lines.SaveToFile(Self.FileName);
  Self.Status := rz_an_type_status_Saved;
end;

procedure TRZANCustomRichMemo.DoUndo;
begin
  Self.Undo;
end;

procedure TRZANCustomRichMemo.DoRedo;
begin
  Self.Redo;
end;

procedure TRZANCustomRichMemo.DoCopy;
begin
  Self.CopyToClipboard;
end;

procedure TRZANCustomRichMemo.DoCut;
begin
  Self.CutToClipboard;
end;

procedure TRZANCustomRichMemo.DoPaste;
begin
  Self.PasteFromClipboard;
end;

procedure TRZANCustomRichMemo.DoSelectAll;
begin
  Self.SelectAll;
end;

{Events}

procedure TRZANCustomRichMemo.Change;
begin
  Self.Status := rz_an_type_status_Modified;
  if Self.DoParsing then Self.LanguageProc.LineParsing(Self,Self.ReservedWord);
  inherited;
end;

procedure TRZANCustomRichMemo.DoEnter;
begin
  Self._SetStatusBarCaret;
  inherited;
end;

procedure TRZANCustomRichMemo.MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Self._SetStatusBarCaret;
  inherited;
end;

procedure TRZANCustomRichMemo.KeyDown (var Key: Word; Shift: TShiftState);
begin
  Self._SetStatusBarCaret;
  if Key = VK_TAB then
  begin
    // Key:= 0;
    Key := VK_A; //VK_SHIFT + VK_TAB;
  end;
  inherited;
end;

procedure TRZANCustomRichMemo.KeyPress (var Key: char);
begin
  Self._SetStatusBarCaret;
  inherited;
end;

procedure TRZANCustomRichMemo.KeyUp (var Key: Word; Shift: TShiftState);
begin
  Self._SetStatusBarCaret;
  inherited;
end;

end.

