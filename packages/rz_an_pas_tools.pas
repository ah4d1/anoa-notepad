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

unit rz_an_pas_tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Registry, Graphics;

type
  TRZANTools = object
  public
    function IsContextMenuExists : Boolean;
    procedure CreateRegistry (AAppExeName : TFileName);
    function ComplementaryColor (AColor: TColor): TColor;
  end;

var
  VRZANTools : TRZANTools;

implementation

function TRZANTools.IsContextMenuExists : Boolean;
var
  LRegistry : TRegistry;
begin
  LRegistry := TRegistry.Create(KEY_READ);
  LRegistry.RootKey := HKEY_CLASSES_ROOT;
  if not(LRegistry.KeyExists('*\shell\Edit with Anoa-Notepad\')) then
    Result := False
  else
    Result := True
  ;
  LRegistry.CloseKey();
  LRegistry.Free;
end;

procedure TRZANTools.CreateRegistry (AAppExeName : TFileName);
var
  LRegistry : TRegistry;
begin
  LRegistry := TRegistry.Create();
  LRegistry.RootKey := HKEY_CLASSES_ROOT;
  LRegistry.OpenKey('*\shell\Edit with Anoa-Notepad\',True);
  LRegistry.CloseKey();
  LRegistry.OpenKey('*\shell\Edit with Anoa-Notepad\command\',True);
  LRegistry.WriteString('','"' + AAppExeName + '" %1');
  LRegistry.CloseKey();
  LRegistry.Free;
end;

function TRZANTools.ComplementaryColor (AColor: TColor): TColor;
begin
  Result := clWhite - ColorToRGB(AColor);
    // OR : RGBToColor(255 - Red(AColor), 255- Green(AColor), 255 - Blue(AColor));
end;

end.

