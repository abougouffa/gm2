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
IMPLEMENTATION MODULE SymbolConversion ;

FROM NameKey IMPORT Name ;
FROM SymbolKey IMPORT SymbolTree, GetSymKey, PutSymKey, DelSymKey, InitTree, NulKey ;
FROM M2Error IMPORT InternalError ;
FROM SymbolTable IMPORT IsConst, PopValue, IsValueSolved ;
FROM M2ALU IMPORT PushIntegerTree ;
FROM gccgm2 IMPORT GetErrorNode, RememberConstant ;
FROM Storage IMPORT ALLOCATE ;
FROM SYSTEM IMPORT ADDRESS ;

CONST
   USEPOISON = TRUE ;
   GGCPOISON = 0A5A5A5A5H ;   (* poisoned memory contains this code *)

TYPE
   PtrToInteger = POINTER TO INTEGER ;

VAR
   mod2gcc       : SymbolTree ;
   PoisonedSymbol: ADDRESS ;   


PROCEDURE mystop2 ; BEGIN END mystop2 ;


(*
   Mod2Gcc - given a modula-2 symbol, sym, return the gcc equivalent.
*)

PROCEDURE Mod2Gcc (sym: CARDINAL) : Tree ;
VAR
   t : PtrToInteger ;
   tr: Tree ;
BEGIN
   IF USEPOISON
   THEN
      t := PtrToInteger(GetSymKey(mod2gcc, Name(sym))) ;
      IF (t#NIL) AND (t^=GGCPOISON)
      THEN
         InternalError('gcc symbol has been poisoned', __FILE__, __LINE__)
      END
   END ;
   tr := Tree(GetSymKey(mod2gcc, Name(sym))) ;
   IF tr=PoisonedSymbol
   THEN
      InternalError('attempting to use a gcc symbol which is no longer in scope', __FILE__, __LINE__)
   END ;
   RETURN( tr )
END Mod2Gcc ;


(*
   AddModGcc - adds the tuple [ sym, gcc ] into the database.
*)

PROCEDURE AddModGcc (sym: CARDINAL; gcc: Tree) ;
VAR
   old: Tree ;
   t  : PtrToInteger ;
BEGIN
   IF gcc=GetErrorNode()
   THEN
      InternalError('error node generated during symbol conversion', __FILE__, __LINE__)
   END ;

   IF USEPOISON
   THEN
      t := PtrToInteger(gcc) ;
      IF (gcc#Tree(NIL)) AND (t^=GGCPOISON)
      THEN
         InternalError('gcc symbol has been poisoned', __FILE__, __LINE__)
      END
   END ;
   IF sym=149
   THEN
      mystop2
   END ;

   old := Mod2Gcc(sym) ;
   IF old=Tree(NIL)
   THEN
      (* absent - add it *)
      PutSymKey(mod2gcc, Name(sym), CARDINAL(gcc)) ;
      gcc := RememberConstant(gcc)
   ELSIF old=gcc
   THEN
      (* do nothing, as it is already stored *)
   ELSIF old=GetErrorNode()
   THEN
      InternalError('replacing a temporary symbol (currently unexpected)', __FILE__, __LINE__)
   ELSE
(*
      DelSymKey(mod2gcc, sym) ;
      PutSymKey(mod2gcc, sym, CARDINAL(gcc)) ;
      InternalError('should not be replacing a symbol', __FILE__, __LINE__)
   ELSE
*)
      InternalError('should not be replacing a symbol', __FILE__, __LINE__)
   END ;

   IF IsConst(sym) AND (NOT IsValueSolved(sym))
   THEN
      PushIntegerTree(gcc) ;
      PopValue(sym)
   END
END AddModGcc ;


(*
   GccKnowsAbout - returns TRUE if gcc knows about the symbol, sym.
*)

PROCEDURE GccKnowsAbout (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( GetSymKey(mod2gcc, Name(sym))#NulKey )
END GccKnowsAbout ;


(*
   AddTemporaryKnown - adds a temporary gcc symbol against the modula-2 sym.
*)

PROCEDURE AddTemporaryKnown (sym: CARDINAL) ;
BEGIN
   (* we add the error node against symbol, sym. We expect it to be retacted later.. *)
   PutSymKey(mod2gcc, Name(sym), CARDINAL(GetErrorNode()))
END AddTemporaryKnown ;


(*
   RemoveTemporaryKnown - removes the temporary symbol.
*)

PROCEDURE RemoveTemporaryKnown (sym: CARDINAL) ;
BEGIN
   IF Mod2Gcc(sym)=GetErrorNode()
   THEN
      DelSymKey(mod2gcc, Name(sym))
   ELSE
      InternalError('attempting to remove a symbol which is not present in the tree', __FILE__, __LINE__)
   END
END RemoveTemporaryKnown ;


(*
   Poison - poisons a symbol.
*)

PROCEDURE Poison (sym: WORD) ;
VAR
   a: ADDRESS ;
BEGIN
   a := Mod2Gcc(sym) ;
   IF a#NIL
   THEN
      DelSymKey(mod2gcc, Name(sym)) ;
      PutSymKey(mod2gcc, Name(sym), CARDINAL(PoisonedSymbol))
   END
END Poison ;


(*
   Init - create both binary trees.
*)

PROCEDURE Init ;
BEGIN
   InitTree(mod2gcc) ;
   ALLOCATE(PoisonedSymbol, 1)
END Init ;


BEGIN
   Init
END SymbolConversion.
