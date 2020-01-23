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

unit rz_an_pas_reserved_word;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, rz_an_pas_tools;

type

  TRZANCustomReservedWord = class(TComponent)
  private
    FStringJava : Widestring;
    FLangJava : TStrings;
    FStringPascal : Widestring;
    FLangPascal : TStrings;
    FStringPython : Widestring;
    FLangPython : TStrings;
    FStringText : Widestring;
    FLangText : TStrings;
  protected
    property StringJava : Widestring read FStringJava write FStringJava;
    property LangJava : TStrings read FLangJava write FLangJava;
    property StringPascal : Widestring read FStringPascal write FStringPascal;
    property LangPascal : TStrings read FLangPascal write FLangPascal;
    property StringPython : Widestring read FStringPython write FStringPython;
    property LangPython : TStrings read FLangPython write FLangPython;
    property StringText : Widestring read FStringText write FStringText;
    property LangText : TStrings read FLangText write FLangText;
  public
    constructor Create (AOwner : TComponent); override;
  end;

  TRZANReservedWord = class(TRZANCustomReservedWord)
  public
    property LangJava;
    property LangPascal;
    property LangPython;
    property LangText;
  end;

implementation

constructor TRZANCustomReservedWord.Create (AOwner : TComponent);
begin
  StringJava := ''
    + 'abstract,assert,boolean,break,byte,case,catch,char,class,const,default,do'
    + ',double,else,enum,extends,false,final,finally,float,for,goto,if,implements'
    + ',import,instanceof,int,interface,long,native,new,null,package,private,protected'
    + ',public,return,short,static,strictfp,super,switch,synchronized,this,throw'
    + ',throws,transient,true,try,void,volatile,while,continue'
  ;
  LangJava := VRZANTools.StringExplode(StringJava,',');
  StringPascal := ''
    + 'and,array,as,asm,begin,break,case,class,const,constructor,continue,destructor,dispose'
    + ',div,do,downto,else,end,except,exit,exports,false,file,finalization,finally,for,function'
    + ',goto,if,implementation,in,inherited,initialization,inline,interface,is,label,library'
    + ',mod,new,nil,not,object,of,on,on,operator,or,out,packed,procedure,program,property,raise'
    + ',record,repeat,self,set,shl,shr,string,then,threadvar,to,true,try,type,unit,until,uses,var'
    + ',while,with,xor'
  ;
  LangPascal := VRZANTools.StringExplode(StringPascal,',');
  StringPython := ''
    + 'and,as,assert,break,class,continue,def,del,elif,else,except,finally,false'
    + ',for,from,global,if,import,in,is,lambda,nonlocal,none,not,or,pass,raise,return'
    + ',true,try,with,while,yield'
  ;
  LangPython := VRZANTools.StringExplode(StringPython,',');
  StringText := ''
    + '%%%%%%$#$%$$#%%%%%%$#$%$$#%%%%%%$#$%$$#%%%%%%$#$%$$#' // need for dummy only
  ;
  LangText := VRZANTools.StringExplode(StringText,',');
end;

end.

