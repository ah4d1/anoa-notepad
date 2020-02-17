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
  Self.Filter := ''
    + 'Basic Files (*.bas)|*.bas'
    + '|C++ Files (*.cpp)|*.cpp'
    + '|CSS Files (*.css)|*.css'
    + '|HTML Files (*.htm;*.html)|*.css;*.html'
    + '|Java Files (*.java;*.class)|*.java;*.class'
    + '|Javascript Files (*.js)|*.js'
    + '|Pascal Files (*.pas)|*.pas'
    + '|PHP Files (*.php)|*.php'
    + '|Python Files (*.py)|*.py'
    + '|SQL Files (*.sql)|*.sql'
    + '|XML Files (*.xml)|*.xml'
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

