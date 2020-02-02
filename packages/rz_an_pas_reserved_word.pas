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

unit rz_an_pas_reserved_word;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, rz_an_pas_tools;

type

  TRZANCustomReservedWord = class(TComponent)
  private
    FRZStringJava : Widestring;
    FRZLangJava : TStrings;
    FRZStringPascal : Widestring;
    FRZLangPascal : TStrings;
    FRZStringPython : Widestring;
    FRZLangPython : TStrings;
    FRZStringText : Widestring;
    FRZLangText : TStrings;
  protected
    property RZStringJava : Widestring read FRZStringJava write FRZStringJava;
    property RZLangJava : TStrings read FRZLangJava write FRZLangJava;
    property RZStringPascal : Widestring read FRZStringPascal write FRZStringPascal;
    property RZLangPascal : TStrings read FRZLangPascal write FRZLangPascal;
    property RZStringPython : Widestring read FRZStringPython write FRZStringPython;
    property RZLangPython : TStrings read FRZLangPython write FRZLangPython;
    property RZStringText : Widestring read FRZStringText write FRZStringText;
    property RZLangText : TStrings read FRZLangText write FRZLangText;
  public
    constructor Create (AOwner : TComponent); override;
  end;

  TRZANReservedWord = class(TRZANCustomReservedWord)
  public
    property RZLangJava;
    property RZLangPascal;
    property RZLangPython;
    property RZLangText;
  end;

implementation

constructor TRZANCustomReservedWord.Create (AOwner : TComponent);
begin
  RZStringJava := ''
    + 'abstract,assert,boolean,break,byte,case,catch,char,class,const,default,do'
    + ',double,else,enum,extends,false,final,finally,float,for,goto,if,implements'
    + ',import,instanceof,int,interface,long,native,new,null,package,private,protected'
    + ',public,return,short,static,strictfp,super,switch,synchronized,this,throw'
    + ',throws,transient,true,try,void,volatile,while,continue'
  ;
  RZLangJava := VRZANTools.RZStringExplode(RZStringJava,',');
  RZStringPascal := ''
    + 'and,array,as,asm,begin,break,case,class,const,constructor,continue,destructor,dispose'
    + ',div,do,downto,else,end,except,exit,exports,false,file,finalization,finally,for,function'
    + ',goto,if,implementation,in,inherited,initialization,inline,interface,is,label,library'
    + ',mod,new,nil,not,object,of,on,on,operator,or,out,packed,procedure,program,property,raise'
    + ',record,repeat,self,set,shl,shr,string,then,threadvar,to,true,try,type,unit,until,uses,var'
    + ',while,with,xor'
  ;
  RZLangPascal := VRZANTools.RZStringExplode(RZStringPascal,',');
  RZStringPython := ''
    + 'and,as,assert,break,class,continue,def,del,elif,else,except,finally,false'
    + ',for,from,global,if,import,in,is,lambda,nonlocal,none,not,or,pass,raise,return'
    + ',true,try,with,while,yield'
  ;
  RZLangPython := VRZANTools.RZStringExplode(RZStringPython,',');
  RZStringText := ''
    + '%%%%%%$#$%$$#%%%%%%$#$%$$#%%%%%%$#$%$$#%%%%%%$#$%$$#' // need only for dummy
  ;
  RZLangText := VRZANTools.RZStringExplode(RZStringText,',');
end;

end.

