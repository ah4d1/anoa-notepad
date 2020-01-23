{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rz_anoa_notepad_package;

{$warn 5023 off : no warning about unused units}
interface

uses
  RZ_Anoa_Notepad, rz_an_cmp_richmemo, rz_an_cmp_statusbar, 
  rz_an_pas_opendialog, rz_an_pas_var, rz_an_pas_savedialog, 
  rz_an_pas_reserved_word, rz_an_pas_tools, rz_an_pas_language, 
  rz_an_cmp_pagecontrol, rz_an_pas_tabsheet, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RZ_Anoa_Notepad', @RZ_Anoa_Notepad.Register);
end;

initialization
  RegisterPackage('rz_anoa_notepad_package', @Register);
end.
