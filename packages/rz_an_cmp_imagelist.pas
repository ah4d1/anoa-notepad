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

unit rz_an_cmp_imagelist;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, rz_an_pas_var;

type

  TRZANCustomImageList = class(TImageList)
  public
    constructor Create (AOwner : TComponent); override;
    procedure RZAdd (AFileName : TFileName);
  end;

  TRZANImageList = class(TRZANCustomImageList)

  end;

implementation

constructor TRZANCustomImageList.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  Self.RZAdd('_IMAGES/folder.png');      // 0
  Self.RZAdd('_IMAGES/cross.png');       // 1
  Self.RZAdd('_IMAGES/add-file.png');    // 2
  Self.RZAdd('_IMAGES/typography.png');  // 3
  Self.RZAdd('_IMAGES/save.png');        // 4
  Self.RZAdd('_IMAGES/undo.png');        // 5
  Self.RZAdd('_IMAGES/redo.png');        // 6
  Self.RZAdd('_IMAGES/printer.png');     // 7
  Self.RZAdd('_IMAGES/edit.png');        // 8
  Self.RZAdd('_IMAGES/copy.png');        // 9
  Self.RZAdd('_IMAGES/cut.png');         // 10
  Self.RZAdd('_IMAGES/paste.png');       // 11
  Self.RZAdd('_IMAGES/info.png');        // 12
  Self.RZAdd('_IMAGES/data-table.png');  // 13
  Self.RZAdd('_IMAGES/edit-red.png');    // 14
  Self.RZAdd('_IMAGES/export.png');      // 15
  Self.RZAdd('_IMAGES/clipboard.png');   // 16
end;

procedure TRZANCustomImageList.RZAdd (AFileName : TFileName);
var
  LPicture : TPicture;
  LBitmap : TBitmap;
begin
  LPicture := TPicture.Create;
  LBitmap := TBitmap.Create;
  try
    LPicture.LoadFromFile(rz_an_var_CmpPath + AFileName);
    LBitmap.Assign(LPicture.Graphic);
    Self.Add(LBitmap,nil);
  finally
    LPicture.Free;
    LBitmap.Free;
  end;
end;

end.

