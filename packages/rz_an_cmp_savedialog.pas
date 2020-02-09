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

unit rz_an_cmp_savedialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, rz_an_pas_var;

type

  TRZANSaveDialog = class(TSaveDialog)
  public
    constructor Create (AOwner : TComponent); override;
    function Execute : Boolean; override;
  end;

implementation

{Open Dialog}

constructor TRZANSaveDialog.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  // see rz_an_type_Language type
  Self.Filter := ''
  + 'Java Files (*.java)|*.java'
  + '|Pascal Files (*.pas)|*.pas'
  + '|Python Files (*.py)|*.py'
  + '|Text Files (*.txt)|*.txt'
  + '|All Files (*.*)|*.*'
  ;
end;

function TRZANSaveDialog.Execute : Boolean;
begin
  Self.FileName := '';
  inherited;
end;

end.

