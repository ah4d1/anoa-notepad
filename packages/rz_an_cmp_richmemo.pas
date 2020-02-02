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
  Classes, SysUtils, LResources, Forms, Grids, Controls, Graphics, Graph, ComCtrls, Dialogs, StdCtrls, RichMemo,
  LCLType, LCLIntf, Messages, Clipbrd, rz_an_pas_var, rz_an_pas_opendialog,
  rz_an_pas_savedialog, rz_an_cmp_statusbar, rz_an_pas_reserved_word, rz_an_pas_language,
  rz_an_cmp_linenumber;

type

  TRZScrollEvent = procedure(Sender: TObject; Message: TMessage) of object;
  TRZANCustomRichMemo = class(TRichMemo)
  private
    {Properties}
    FRZStatus : rz_an_type_Status;
    FRZLanguage : rz_an_type_Language;
    FRZCaretPosX : Integer;
    FRZCaretPosY : Integer;
    FRZFileName : TFileName;
    FRZFileNameExt : string;
    FRZDoParsing : Boolean;
    FRZStyle : rz_an_type_Style;
    FRZClipboard : TClipboard;
    FOnScroll : TRZScrollEvent;
    {Sub-Components}
    FRZANLanguage : TRZANLanguage;
    FRZReservedWords : TStrings;
    FRZANReservedWord : TRZANReservedWord;
    FRZANLineNumber : TRZANLineNumber;
    FRZANStatusBar : TRZANStatusBar;
    procedure _SetTabSet;
  protected
    {Functions of Properties}
    procedure SetRZStatus (const AValue: rz_an_type_Status);
    procedure SetRZLanguage (const AValue: rz_an_type_Language);
    procedure SetRZCaretPosX (const AValue: Integer);
    procedure SetRZCaretPosY (const AValue: Integer);
    procedure SetRZFileName (const AValue: TFileName);
    procedure SetFileNameExt (const AValue: string);
    procedure SetRZStyle (const AValue: rz_an_type_Style);
    {Properties}
    property RZStatus : rz_an_type_Status read FRZStatus write SetRZStatus;
    property RZLanguage : rz_an_type_Language read FRZLanguage write SetRZLanguage;
    property RZCaretPosX : Integer read FRZCaretPosX write SetRZCaretPosX;
    property RZCaretPosY : Integer read FRZCaretPosY write SetRZCaretPosY;
    property RZFileName : TFileName read FRZFileName write SetRZFileName;
    property RZFileNameExt : string read FRZFileNameExt write SetFileNameExt;
    property RZStyle : rz_an_type_Style read FRZStyle write SetRZStyle;
    {***}
    property RZDoParsing : Boolean read FRZDoParsing write FRZDoParsing;
    property RZClipboard : TClipboard read FRZClipboard write FRZClipboard;
    {Sub-Components}
    property RZANLanguage : TRZANLanguage read FRZANLanguage write FRZANLanguage;
    property RZReservedWords : TStrings read FRZReservedWords write FRZReservedWords;
    property RZANReservedWord : TRZANReservedWord read FRZANReservedWord write FRZANReservedWord;
    property RZANLineNumber : TRZANLineNumber read FRZANLineNumber write FRZANLineNumber;
    property RZANStatusBar : TRZANStatusBar read FRZANStatusBar write FRZANStatusBar;
    {Events}
    function RZFirstVisibleLine :Integer;
    procedure RZInfoUpdate;
    procedure Change; override;
    procedure DoEnter; override;
    procedure KeyPress (var Key: char); override;
    procedure KeyDown (var Key: Word; Shift: TShiftState); override;
    procedure KeyUp (var Key: Word; Shift: TShiftState); override;
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseWheel (Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    {Scroll}
    procedure WndProc(var Message: TMessage); override;
  public
    {Create}
    constructor Create (AOwner : TComponent); override;
    {Open and Save}
    procedure RZOpen (AFileName : TFileName);
    procedure RZSave (AFileName : TFileName);
    {Text Editing}
    procedure RZUndo;
    procedure RZRedo;
    procedure RZCopy;
    procedure RZCut;
    procedure RZPaste;
    procedure RZSelectAll;
    {Line Numbering}
    procedure RZAllLineNumber;
    {Scroll}
    procedure DoScroll (Sender: TObject; AMessage: TMessage);
    property OnScroll : TRZScrollEvent read FOnScroll write FOnScroll;
    {Language}
    procedure RZLanguageRefresh;
  end;

  TRZANRichMemo = class(TRZANCustomRichMemo)
  public
    {Properties}
    property RZCaretPosX;
    property RZCaretPosY;
    property RZFileName;
    property RZFileNameExt;
    property RZDoParsing;
    {Sub-Components}
    property RZReservedWords;
    property RZANLineNumber;
  published
    property RZStatus;
    property RZLanguage;
    property RZStyle;
    property RZANStatusBar;
  end;

implementation

{Create}

constructor TRZANCustomRichMemo.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  {Sub-Components}
  Self.RZANLanguage := TRZANLanguage.Create(Self);
  Self.RZReservedWords := TStrings.Create;
  Self.RZANReservedWord := TRZANReservedWord.Create(Self);
  Self.RZANLineNumber := TRZANLineNumber.Create(Self);
  Self.RZANStatusBar := TRZANStatusBar.Create(Self);
  Self.RZClipboard := TClipboard.Create;
  {Properties}
  Self.Color := clWhite;
  Self.VertScrollBar.Smooth := False;
  Self.RZStatus := rz_an_type_status_Ready;
  Self.RZLanguage := rz_an_type_lang_Text;
  Self.RZDoParsing := True;
  // Self.Style := rz_an_type_style_Normal; // ERROR
  {Font Setting}
  Self.Font.Name := 'Courier New';
  Self.Font.Size := 10;
  Self.Font.Color := clBlack;
  Self.Font.Quality := fqDraft;
  {Misc Settings}
  Self.ScrollBars := ssBoth;
  // Self._SetTabSet;
  {Scroll}
  Self.OnScroll := @Self.DoScroll;
end;

{Open and Save}

procedure TRZANCustomRichMemo.RZOpen (AFileName : TFileName);
begin
  Self.RZFileName := AFileName;
  Self.Lines.LoadFromFile(Self.RZFileName);
  Self.RZANLanguage.RZAllParsing(Self,Self.RZReservedWords);
  Self.RZAllLineNumber;
end;

procedure TRZANCustomRichMemo.RZSave (AFileName : TFileName);
begin
  Self.RZFileName := AFileName;
  Self.Lines.SaveToFile(Self.RZFileName);
  Self.RZStatus := rz_an_type_status_Saved;
end;

{Text Editing}

procedure TRZANCustomRichMemo.RZUndo;
begin
  Self.RZDoParsing := False;
  Self.Undo;
  Self.RZDoParsing := True;
end;

procedure TRZANCustomRichMemo.RZRedo;
begin
  Self.RZDoParsing := False;
  Self.Redo;
  Self.RZDoParsing := True;
end;

procedure TRZANCustomRichMemo.RZCopy;
begin
  Self.RZClipboard.Clear;
  Self.RZClipboard.AsText := Self.SelText;
end;

procedure TRZANCustomRichMemo.RZCut;
begin
  Self.RZClipboard.AsText := Self.SelText;
  Self.SelText := '';
end;

procedure TRZANCustomRichMemo.RZPaste;
begin
  Self.SelText := Self.RZClipboard.AsText;
  Self.RZANLanguage.RZAllParsing(Self,Self.RZReservedWords);
end;

procedure TRZANCustomRichMemo.RZSelectAll;
begin
  Self.SelectAll;
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

{Status}

procedure TRZANCustomRichMemo.SetRZStatus (const AValue: rz_an_type_Status);
begin
  Self.FRZStatus := AValue;
  Self.RZANStatusBar.RZStatus := Self.RZStatus;
end;

{Language}

procedure TRZANCustomRichMemo.SetRZLanguage (const AValue: rz_an_type_Language);
begin
  Self.FRZLanguage := AValue;
  if Self.RZLanguage = rz_an_type_lang_Java then Self.RZReservedWords := Self.RZANReservedWord.RZLangJava
    else if Self.RZLanguage = rz_an_type_lang_Pascal then Self.RZReservedWords := Self.RZANReservedWord.RZLangPascal
    else if Self.RZLanguage = rz_an_type_lang_Python then Self.RZReservedWords := Self.RZANReservedWord.RZLangPython
    else Self.RZReservedWords := Self.RZANReservedWord.RZLangText
  ;
  if Self.RZDoParsing then Self.RZANLanguage.RZAllParsing(Self,Self.RZReservedWords);
  Self.RZANStatusBar.RZLanguage := Self.RZLanguage;
  Self.RZDoParsing := False;
end;

{Carets}

procedure TRZANCustomRichMemo.SetRZCaretPosX (const AValue : Integer);
begin
  Self.FRZCaretPosX := AValue;
  Self.RZANStatusBar.RZCaretPosX := Self.RZCaretPosX;
end;

procedure TRZANCustomRichMemo.SetRZCaretPosY (const AValue : Integer);
begin
  Self.FRZCaretPosY := AValue;
  Self.RZANStatusBar.RZCaretPosY := Self.RZCaretPosY;
end;

{FileName}

procedure TRZANCustomRichMemo.SetRZFileName (const AValue: TFileName);
begin
  Self.FRZFileName := AValue;
  Self.RZFileNameExt := ExtractFileExt(Self.RZFileName);
  Self.RZANStatusBar.RZFileName := Self.RZFileName;
end;

procedure TRZANCustomRichMemo.SetFileNameExt (const AValue: string);
begin
  Self.FRZFileNameExt := AValue;
  if Self.RZFileNameExt = '.java' then Self.RZLanguage := rz_an_type_lang_Java
  else if Self.RZFileNameExt = '.pas' then Self.RZLanguage := rz_an_type_lang_Pascal
  else if Self.RZFileNameExt = '.py' then Self.RZLanguage := rz_an_type_lang_Python
  else Self.RZLanguage := rz_an_type_lang_Text;
end;

{Style}

procedure TRZANCustomRichMemo.SetRZStyle (const AValue: rz_an_type_Style);
var
  LFont : TFont;
begin
  Self.FRZStyle := AValue;
  if (AValue = rz_an_type_style_Normal) then
  begin
    Self.Color := rz_an_var_Style_Color_Normal;
    Self.Font.Color := rz_an_var_Style_FontColor_Normal;
  end
  else if (AValue = rz_an_type_style_Dark) then
  begin
    Self.Color := rz_an_var_Style_Color_Dark;
    Self.Font.Color := rz_an_var_Style_FontColor_Dark;
  end;
  Self.SelectAll;
  LFont := TFont.Create;
  LFont.Color := Self.Font.Color;
  Self.SetTextAttributes(0,Self.SelLength,LFont);
  LFont.Free;
  Self.RZLanguage := Self.RZLanguage;
  Self.SelStart := 0;
  Self.RZANLineNumber.TopRow := 0;
end;

{Line Number}

procedure TRZANCustomRichMemo.RZAllLineNumber;
begin
  if Self.Lines.Count >= 1 then
  begin
    {OK for write lines from zero, but why 130?}
    Self.RZANLineNumber.RZAllLineNumber(Self,Abs(Self.Font.Height*Screen.PixelsPerInch div 130));
    {OK for Self.VertScrollBar.Range > Self.Height}
    // Self.RZANLineNumber.RZAllLineNumber(Self,Round(Self.VertScrollBar.Range/Self.Lines.Count));
  end;
end;

{Events}

function TRZANCustomRichMemo.RZFirstVisibleLine :Integer;
const
  EM_GETFIRSTVISIBLELINE = 206;
begin
  Result := SendMessage(Self.Handle,EM_GETFIRSTVISIBLELINE,0,0);
end;

procedure TRZANCustomRichMemo.RZInfoUpdate;
var
  i : Integer;
begin
  {Caret}
  Self.RZANStatusBar.RZCaretPosX := Self.CaretPos.X;
  Self.RZANStatusBar.RZCaretPosY := Self.CaretPos.Y;
  Self.RZCaretPosX := Self.CaretPos.X;
  Self.RZCaretPosY := Self.CaretPos.Y;
  {Line Number}
  Self.RZANLineNumber.RowCount := Self.Lines.Count + 1;
  if Self.Lines.Count >= 1 then
  begin
    for i := 1 to Self.Lines.Count do
    begin
      Self.RZANLineNumber.RowHeights[i-1] := Round(Self.VertScrollBar.Range/Self.Lines.Count);
    end;
  end;
  Self.RZANLineNumber.TopRow := Self.RZFirstVisibleLine;
  Self.RZAllLineNumber;
end;

procedure TRZANCustomRichMemo.RZLanguageRefresh;
begin
  Self.RZLanguage := Self.RZLanguage;
end;

procedure TRZANCustomRichMemo.Change;
var
  LLineText : WideString;
  LAbsolutePos : LongInt;
begin
  Self.RZStatus := rz_an_type_status_Modified;
  LLineText := Self.Lines[Self.CaretPos.Y];
  LAbsolutePos := Self.SelStart - Self.CaretPos.X;
  if Self.RZDoParsing then Self.RZANLanguage.RZLineParsing(Self,Self.RZReservedWords,LLineText,LAbsolutePos);
  Self.RZInfoUpdate;
  Self.RZDoParsing := True;
  inherited;
end;

procedure TRZANCustomRichMemo.DoEnter;
begin
  Self.RZInfoUpdate;
  inherited;
end;

procedure TRZANCustomRichMemo.KeyDown (var Key: Word; Shift: TShiftState);
begin
  Self.RZInfoUpdate;
  if Key = VK_TAB then
  begin
    // Key:= 0;
    Key := VK_A; //VK_SHIFT + VK_TAB;
  end;
  inherited;
end;

procedure TRZANCustomRichMemo.KeyPress (var Key: char);
begin
  Self.RZInfoUpdate;
  inherited;
end;

procedure TRZANCustomRichMemo.KeyUp (var Key: Word; Shift: TShiftState);
begin
  Self.RZInfoUpdate;
  inherited;
end;

procedure TRZANCustomRichMemo.MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Self.RZInfoUpdate;
  inherited;
end;

procedure TRZANCustomRichMemo.MouseWheel (Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Self.RZInfoUpdate;
  inherited;
end;

{Scroll}

procedure TRZANCustomRichMemo.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = WM_VSCROLL then
    if Assigned(FOnScroll) then FOnScroll(Self,Message);
end;

procedure TRZANCustomRichMemo.DoScroll (Sender: TObject; AMessage: TMessage);
begin
  Self.RZInfoUpdate;
end;

end.

