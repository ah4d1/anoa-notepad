// This file is part of Anoa Notepad project
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
// 1. Integrates source code from Anoa Notepad.
// 2. Integrates/includes/aggregates Anoa Notepad into a proprietary executable
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

unit rz_an_pas_var;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Graphics
;

type
  rz_an_type_Language = (rz_an_type_lang_Java,rz_an_type_lang_Pascal,rz_an_type_lang_Python,rz_an_type_lang_Text);
  rz_an_type_Status = (rz_an_type_status_Ready,rz_an_type_status_Modified,rz_an_type_status_Saved);
  rz_an_type_Style = (rz_an_type_style_Normal,rz_an_type_style_Dark);

const
  rz_an_var_FileFilterIndex_All = 1; // begin at 1
  rz_an_var_FileFilterIndex_Java = 2;
  rz_an_var_FileFilterIndex_Pascal = 3;
  rz_an_var_FileFilterIndex_Python = 4;
  rz_an_var_FileFilterIndex_Text = 5;
  rz_an_var_FileFilterIndex_Default = 5;
  {Style}
  rz_an_var_Style_Color_Normal = clWhite;
  rz_an_var_Style_FontColor_Normal = clBlack;
  rz_an_var_Style_Color_Dark = clBlack;
  rz_an_var_Style_FontColor_Dark = clWhite;


implementation

end.

