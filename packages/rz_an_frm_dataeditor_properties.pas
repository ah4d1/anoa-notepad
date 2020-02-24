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

unit rz_an_frm_dataeditor_properties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ValEdit,
  Buttons, rz_an_cmp_imagelist;

type

  { TFormDataEditorProperties }

  TFormDataEditorProperties = class(TForm)
    BitBtnClose: TBitBtn;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    LabelColumnCount: TLabel;
    LabelRecordCount: TLabel;
    procedure BitBtnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FRZImageList : TRZANImageList;
  public
    RZRecordCount : Integer;
    RZColumnCount : Integer;
  end;

var
  FormDataEditorProperties: TFormDataEditorProperties;

implementation

{$R *.lfm}

{ TFormDataEditorProperties }

procedure TFormDataEditorProperties.BitBtnCloseClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormDataEditorProperties.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormDataEditorProperties.FormCreate(Sender: TObject);
begin
  Self.FRZImageList := TRZANImageList.Create(Self);
  Self.BitBtnClose.Images := Self.FRZImageList;
  Self.BitBtnClose.ImageIndex := 1;
end;

procedure TFormDataEditorProperties.FormShow(Sender: TObject);
begin
  Self.LabelRecordCount.Caption := IntToStr(RZRecordCount);
  Self.LabelColumnCount.Caption := IntToStr(RZColumnCount);
end;

end.

