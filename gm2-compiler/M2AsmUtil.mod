(* Copyright (C) 2001 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)
IMPLEMENTATION MODULE M2AsmUtil ;


FROM SFIO IMPORT WriteS ;
FROM FIO IMPORT StdOut ;
FROM Strings IMPORT String, string, ConCat, KillString, InitString, Mark, InitStringCharStar, ConCatChar ;
FROM StdIO IMPORT Write ;
FROM StrIO IMPORT WriteString ;
FROM NameKey IMPORT WriteKey, GetKey, MakeKey, makekey, KeyToCharStar ;

FROM SymbolTable IMPORT NulSym,
                        GetSymName,
                        GetScopeAuthor,
                        GetBaseModule,
                        IsInnerModule,
                        IsVar,
                        IsProcedure,
                        IsModule,
                        IsDefImp,
                        IsExportQualified,
                        IsExported ;

FROM M2Error IMPORT InternalError ;
FROM M2Configure IMPORT UseUnderscoreForC, UseDotForJumpLabels ;


(* %%%FORWARD%%%
PROCEDURE GetModulePrefix (Name: String; Sym, ModSym: CARDINAL) : String ; FORWARD ;
PROCEDURE WriteModulePrefix (Sym, ModSym: CARDINAL) ; FORWARD ;
PROCEDURE GetFullScopePrefix (Name: String; Sym: CARDINAL) : String ; FORWARD ;
   %%%FORWARD%%% *)


(*
   StringToKey - returns a Name, from a string and destroys the string.
*)

PROCEDURE StringToKey (s: String) : Name ;
VAR
   k: Name ;
BEGIN
   k := makekey(string(s)) ;
   s := KillString(s) ;
   RETURN( k )
END StringToKey ;


(*
   DotifyLabel - place a dot infront of the label if necessary.
                 The string, s, should no longer be used after
                 this function.
*)

PROCEDURE DotifyLabel (s: String) : String ;
BEGIN
   IF UseDotForJumpLabels
   THEN
      RETURN( ConCat(InitString('.'), Mark(s)) )
   ELSE
      RETURN( s )
   END
END DotifyLabel ;


(*
   GetFullScopeAsmName - returns the fully qualified name for the symbol.
                         This will take the format
                         [DefImpModule|Module]_{InnerModule}_{Procedure}_SymbolName
*)

PROCEDURE GetFullScopeAsmName (Sym: CARDINAL) : Name ;
VAR
   Module: String ;
   Father: CARDINAL ;
BEGIN
   Father := GetScopeAuthor(Sym) ;
   IF UseUnderscoreForC
   THEN
      Module := InitString('_')
   ELSE
      Module := InitString('')
   END ;
   Module := ConCat(GetFullScopePrefix(Module, Father),
                    InitStringCharStar(KeyToCharStar(GetSymName(Sym)))) ;
   RETURN( StringToKey(Module) )
END GetFullScopeAsmName ;


(*
   GetAsmName - returns the NameKey for the assembler string of a symbol.
*)

PROCEDURE GetAsmName (Sym: CARDINAL) : Name ;
VAR
   Module: String ;
   Father: CARDINAL ;
BEGIN
   Father := GetScopeAuthor(Sym) ;
   IF UseUnderscoreForC
   THEN
      Module := InitString('_')
   ELSE
      Module := InitString('')
   END ;
   Module := ConCat(GetModulePrefix(Module, Sym, Father),
                    InitStringCharStar(KeyToCharStar(GetSymName(Sym)))) ;
   RETURN( StringToKey(Module) )
END GetAsmName ;


(*
   GetFullSymName - returns the NameKey for the symbol name (which also
                    may contain the module name). This is the same as
                    GetAsmName except that it does not have the leading _
*)

PROCEDURE GetFullSymName (Sym: CARDINAL) : Name ;
VAR
   Module  : String ;
   k       : Name ;
   Father  : CARDINAL ;
BEGIN
   Father := GetScopeAuthor(Sym) ;
   Module := GetModulePrefix(InitString(''), Sym, Father) ;
   RETURN( StringToKey(ConCat(Module, InitStringCharStar(KeyToCharStar(GetSymName(Sym))))) )
END GetFullSymName ;


(*
   WriteAsmName - displays the symbol, Sym, name using module prefixes
                  if it is EXPORT QUALIFIED.
*)

PROCEDURE WriteAsmName (Sym: CARDINAL) ;
VAR
   Father: CARDINAL ;
BEGIN
   IF UseUnderscoreForC
   THEN
      Write('_')
   END ;
   Father := GetScopeAuthor(Sym) ;
   WriteModulePrefix(Sym, Father) ;
   WriteKey(GetSymName(Sym))
END WriteAsmName ;


(*
   WriteName - displays the symbol, Sym, name using module prefixes
               if it is EXPORT QUALIFIED.
               This procedure differs from the above procedure because
               it does not generate any _ prefix.
*)

PROCEDURE WriteName (Sym: CARDINAL) ;
VAR
   Father: CARDINAL ;
BEGIN
   IF IsVar(Sym) OR IsProcedure(Sym)
   THEN
      Father := GetScopeAuthor(Sym) ;
      WriteModulePrefix(Sym, Father) ;
      WriteKey(GetSymName(Sym))
   ELSE
      InternalError('Expecting Var or Procedure symbol', __FILE__, __LINE__)
   END
END WriteName ;


(*
   GetModulePrefix - returns a String containing the module prefix
                     for module, ModSym, providing symbol, Sym, is exported.
                     Name is marked if it is appended onto the new string.
*)

PROCEDURE GetModulePrefix (Name: String; Sym, ModSym: CARDINAL) : String ;
BEGIN
   IF (ModSym#NulSym) AND (ModSym#GetBaseModule())
   THEN
      IF IsInnerModule(Sym) OR IsInnerModule(ModSym)
      THEN
         RETURN( ConCat(ConCatChar(InitStringCharStar(KeyToCharStar(GetSymName(ModSym))), '_'),
                        GetModulePrefix(Name, ModSym, GetScopeAuthor(ModSym))) )
      ELSIF IsDefImp(ModSym) AND IsExportQualified(Sym)
      THEN
         RETURN( ConCatChar(ConCat(InitStringCharStar(KeyToCharStar(GetSymName(ModSym))), Mark(Name)), '_') )
      END
   END ;
   RETURN( Name )
END GetModulePrefix ;


(*
   GetFullScopePrefix - returns a String containing the full scope prefix
                        for symbol, Sym.
                        Name is marked if it is appended onto the new string.
*)

PROCEDURE GetFullScopePrefix (Name: String; Sym: CARDINAL) : String ;
BEGIN
   IF Sym#NulSym
   THEN
      IF IsInnerModule(Sym)
      THEN
         RETURN( ConCat(ConCatChar(InitStringCharStar(KeyToCharStar(GetSymName(Sym))), '_'),
                        GetFullScopePrefix(Name, GetScopeAuthor(Sym))) )
      ELSIF IsDefImp(Sym) OR IsProcedure(Sym)
      THEN
         RETURN( ConCatChar(ConCat(InitStringCharStar(KeyToCharStar(GetSymName(Sym))), Mark(Name)), '_') )
      END
   END ;
   RETURN( Name )
END GetFullScopePrefix ;


(*
   WriteModulePrefix - writes the module prefix for module, ModSym,
                       providing symbol, Sym, is exported.
*)

PROCEDURE WriteModulePrefix (Sym, ModSym: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := GetModulePrefix(InitString(''), Sym, ModSym) ;
   s := KillString(WriteS(StdOut, s))
END WriteModulePrefix ;


(*
   UnderScoreString - emits a string with a leading underscore if the C compiler
                      uses _ prefixes. The string without the underscore is returned.
*)

PROCEDURE UnderScoreString (s: String) : String ;
BEGIN
   IF UseUnderscoreForC
   THEN
      Write('_')
   END ;
   RETURN( WriteS(StdOut, s) )
END UnderScoreString ;


(*
   GetModuleInitName - returns the name of the initialization section of a module.
*)

PROCEDURE GetModuleInitName (Sym: CARDINAL) : Name ;
VAR
   s: String ;
BEGIN
   s := ConCat(ConCat(InitString('_M2_'), Mark(GetModulePrefix(InitStringCharStar(KeyToCharStar(GetSymName(Sym))),
                                                               Sym, GetScopeAuthor(Sym)))),
               Mark(InitString('_init'))) ;
   IF UseUnderscoreForC
   THEN
      s := ConCat(InitString('_'), Mark(s))
   END ;
   RETURN( StringToKey(s) )
END GetModuleInitName ;


END M2AsmUtil.
