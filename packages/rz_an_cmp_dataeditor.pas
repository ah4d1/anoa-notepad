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

unit rz_an_cmp_dataeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Grids, Graphics, Dialogs, Menus, rz_an_pas_var, rz_an_pas_tools,
  rz_an_cmp_imagelist;

type

  TRZANCustomDataEditor = class(TStringGrid)
  private
    {6. Editor Style}
    {6.0}
    FRZStyle : rz_an_type_Style;
    {7. Caret}
    {7.1}
    FRZCaretPosX : Integer;
    {7.2}
    FRZCaretPosY : Integer;
  protected
    {6. Editor Style}
    {6.0}
    procedure SetRZStyle (AValue : rz_an_type_Style);
    {7. Caret}
    {7.1}
    procedure SetRZCaretPosX (const AValue: Integer);
    {7.2}
    procedure SetRZCaretPosY (const AValue: Integer);
    {6. Editor Style}
    {6.0}
    property RZStyle : rz_an_type_Style read FRZStyle write SetRZStyle;
    {7. Caret}
    {7.1}
    property RZCaretPosX : Integer read FRZCaretPosX write SetRZCaretPosX;
    {7.2}
    property RZCaretPosY : Integer read FRZCaretPosY write SetRZCaretPosY;
    {8. Events}
    {8.3}
    procedure KeyDown (var Key: Word; Shift: TShiftState); override;
    procedure Click; override;
    {8.90}
    procedure RZOnEventUpdate;
  public
    RZImageList : TRZANImageList;
    RZPopupMenu : TPopupMenu;
    constructor Create (AOwner : TComponent); override;
    procedure RZOpen (AFileName : TFileName);
    procedure RZMenuItemAdd (APopupMenu : TPopupMenu; ACaption : string;
      AEvent : TNotifyEvent; AImageIndex : Integer);
    procedure RZExportToMySQL (Sender : TObject);
    procedure RZShowProperties (Sender : TObject);
  end;

  TRZANDataEditor = class(TRZANCustomDataEditor)
  public
    {6. Editor Style}
    {6.0}
    property RZStyle;
    {7. Caret}
    {7.1}
    property RZCaretPosX;
    {7.2}
    property RZCaretPosY;
  end;

implementation

uses
  rz_an_cmp_tabsheet, rz_an_frm_dataeditor_exporttomysql, rz_an_frm_dataeditor_properties;

constructor TRZANCustomDataEditor.Create (AOwner : TComponent);
var
  LPopupMenu : TPopupMenu;
  LMenuItem : TMenuItem;
begin
  inherited Create(AOwner);
  Self.FixedCols := 0;
  Self.RZStyle := rz_an_type_style_Normal;
  Self.FixedColor := VRZANVar.RZMainColor.RZHeader;
  Self.SelectedColor := VRZANVar.RZMainColor.RZRowHighlight;
  Self.TitleFont.Color := VRZANVar.RZMainColor.RZText;
  Self.TitleFont.Style := [fsBold];
  Self.AutoSizeColumns;
  Self.Options := Self.Options + [goColMoving,goColSizing,goDblClickAutoSize,goEditing,goRowSelect];
  {Popup Menu}
  RZImageList := TRZANImageList.Create(Self);
  LPopupMenu := TPopupMenu.Create(Self);
  LPopupMenu.Images := RZImageList;
  LPopupMenu.ImagesWidth := 24;
  Self.RZMenuItemAdd(LPopupMenu,'Export to MySQL ...',@Self.RZExportToMySQL,13);
  Self.RZMenuItemAdd(LPopupMenu,'Properties ...',@Self.RZShowProperties,16);
  Self.PopupMenu := LPopupMenu;
end;

procedure TRZANCustomDataEditor.RZMenuItemAdd (APopupMenu : TPopupMenu; ACaption : string;
  AEvent : TNotifyEvent; AImageIndex : Integer);
var
  LMenuItem : TMenuItem;
begin
  LMenuItem := TMenuItem.Create(APopupMenu);
  LMenuItem.Caption := ACaption;
  LMenuItem.ImageIndex := AImageIndex;
  LMenuItem.OnClick := AEvent;
  APopupMenu.Items.Add(LMenuItem);
end;

procedure TRZANCustomDataEditor.RZExportToMySQL (Sender : TObject);
begin
  FormDataEditorExportToMySQL := TFormDataEditorExportToMySQL.Create(Self);
  FormDataEditorExportToMySQL.RZDataEditor := Self;
  FormDataEditorExportToMySQL.ShowModal;
end;

procedure TRZANCustomDataEditor.RZShowProperties (Sender : TObject);
begin
  FormDataEditorProperties := TFormDataEditorProperties.Create(Self);
  FormDataEditorProperties.RZRecordCount := Self.RowCount - 1;
  FormDataEditorProperties.RZColumnCount := Self.ColCount;
  FormDataEditorProperties.ShowModal;
end;

{2. File Operation}

{2.3}
procedure TRZANCustomDataEditor.RZOpen (AFileName : TFileName);
var
  LDelimiter : string;
begin
  Self.Visible := False;
  LDelimiter := ',';
  if InputQuery('Defining Delimiter','Please type a delimiter',LDelimiter) then
  begin
    Self.LoadFromCSVFile(AFileName,LDelimiter[1]);
    Self.AutoSizeColumns;
  end;
  Self.Visible := True;
end;

{6. Editor Style}

{6.0}
procedure TRZANCustomDataEditor.SetRZStyle (AValue : rz_an_type_Style);
begin
  Self.FRZStyle := AValue;
  if Self.FRZStyle = rz_an_type_style_Normal then
  begin
    Self.Color := VRZANVar.RZMainColor.RZBackground;
    Self.Font.Color := VRZANVar.RZMainColor.RZText;
    Self.FixedColor := VRZANVar.RZMainColor.RZHeader;
    Self.TitleFont.Color := VRZANVar.RZMainColor.RZText;
    Self.SelectedColor := VRZANVar.RZMainColor.RZRowHighlight;
  end
  else if Self.FRZStyle = rz_an_type_style_Dark then
  begin
    Self.Color := VRZANTools.ComplementaryColor(VRZANVar.RZMainColor.RZBackground);
    Self.Font.Color := VRZANTools.ComplementaryColor(VRZANVar.RZMainColor.RZText);
    Self.FixedColor := VRZANVar.RZMainColor.RZHeader;
    Self.TitleFont.Color := VRZANVar.RZMainColor.RZText;
    Self.SelectedColor := VRZANVar.RZMainColor.RZRowHighlight;
  end;
end;

{7. Caret}

{7.1}
procedure TRZANCustomDataEditor.SetRZCaretPosX (const AValue : Integer);
begin
  Self.FRZCaretPosX := AValue;
  if (Self.Parent <> nil) then (Self.Parent as TRZANTabSheet).RZCaretPosX := Self.RZCaretPosX;
end;

{7.2}
procedure TRZANCustomDataEditor.SetRZCaretPosY (const AValue : Integer);
begin
  Self.FRZCaretPosY := AValue;
  if (Self.Parent <> nil) then (Self.Parent as TRZANTabSheet).RZCaretPosY := Self.RZCaretPosY + 1;
end;

{8. Event}

{8.3}
procedure TRZANCustomDataEditor.KeyDown (var Key: Word; Shift: TShiftState);
begin
  inherited;
  Self.RZOnEventUpdate;
end;

procedure TRZANCustomDataEditor.Click;
begin
  inherited Click;
  Self.RZOnEventUpdate;
end;

{8.7}
procedure TRZANCustomDataEditor.RZOnEventUpdate;
begin
  Self.RZCaretPosX := Self.Col + 1;
  Self.RZCaretPosY := Self.Row - 1;
end;

end.

