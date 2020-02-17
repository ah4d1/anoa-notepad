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

unit RZ_Anoa_Notepad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, rz_an_cmp_pagecontrol, rz_an_cmp_statusbar;

procedure Register;

implementation

procedure Register;
begin
  {$I rz_anoa_notepad_icon.lrs}
  RegisterComponents('rzAnoaNotepad',[TRZANPageControl,TRZANStatusBar]);
end;

end.
