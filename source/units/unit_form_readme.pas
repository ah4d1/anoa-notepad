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

unit unit_form_readme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormReadme }

  TFormReadme = class(TForm)
    MemoAbout: TMemo;
    MemoLicense: TMemo;
    MemoGPL: TMemo;
    MemoMain: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MemoMainChange(Sender: TObject);
  private

  public

  end;

var
  FormReadme: TFormReadme;

implementation

{$R *.lfm}

{ TFormReadme }

uses
  unit_form_main;

procedure TFormReadme.FormCreate(Sender: TObject);
begin
  FormMain.Enabled := False;
end;

procedure TFormReadme.MemoMainChange(Sender: TObject);
begin

end;

procedure TFormReadme.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FormMain.Enabled := True;
end;

end.

