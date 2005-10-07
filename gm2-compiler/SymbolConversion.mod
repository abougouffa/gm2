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

FROM Indexing IMPORT Index, InitIndex, PutIndice, GetIndice, InBounds,
                     DebugIndex ;

FROM M2Error IMPORT InternalError ;
FROM SymbolTable IMPORT IsConst, PopValue, IsValueSolved, GetSymName ;
FROM M2ALU IMPORT PushIntegerTree ;
FROM gccgm2 IMPORT GetErrorNode, RememberConstant, GarbageCollect ;
FROM M2Printf IMPORT printf1 ;
FROM Storage IMPORT ALLOCATE ;
FROM SYSTEM IMPORT ADDRESS ;

CONST
   USEPOISON = TRUE ;
   GGCPOISON = 0A5A5A5A5H ;   (* poisoned memory contains this code *)

TYPE
   PtrToInteger = POINTER TO INTEGER ;

VAR
   mod2gcc       : Index ;
   PoisonedSymbol: ADDRESS ;   


PROCEDURE mystop2 ; BEGIN END mystop2 ;


(*
   Mod2Gcc - given a modula-2 symbol, sym, return the gcc equivalent.
*)

PROCEDURE Mod2Gcc (sym: CARDINAL) : Tree ;
VAR
   n : Name ;
   t : PtrToInteger ;
   tr: Tree ;
BEGIN
   IF USEPOISON
   THEN
      IF InBounds(mod2gcc, sym)
      THEN
         t := PtrToInteger(GetIndice(mod2gcc, sym)) ;
         IF (t#NIL) AND (t^=GGCPOISON)
         THEN
            InternalError('gcc symbol has been poisoned', __FILE__, __LINE__)
         END
      END
   END ;
   IF InBounds(mod2gcc, sym)
   THEN
      tr := Tree(GetIndice(mod2gcc, sym)) ;
      IF tr=PoisonedSymbol
      THEN
         n := GetSymName(sym) ;
         printf1('name of poisoned symbol was (%a)\n', n) ;
         InternalError('attempting to use a gcc symbol which is no longer in scope', __FILE__, __LINE__)
      END ;
      RETURN( tr )
   ELSE
      RETURN( NIL )
   END
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

   IF sym=840
   THEN
      mystop2
   END ;

   IF USEPOISON
   THEN
      t := PtrToInteger(gcc) ;
      IF (gcc#Tree(NIL)) AND (t^=GGCPOISON)
      THEN
         InternalError('gcc symbol has been poisoned', __FILE__, __LINE__)
      END
   END ;

   old := Mod2Gcc(sym) ;
   IF old=Tree(NIL)
   THEN
      (* absent - add it *)
      PutIndice(mod2gcc, sym, gcc) ;
      IF GetIndice(mod2gcc, sym)#gcc
      THEN
         InternalError('failed to add gcc <-> mod2 symbol', __FILE__, __LINE__)
      END ;
      gcc := RememberConstant(gcc)
   ELSIF old=gcc
   THEN
      (* do nothing, as it is already stored *)
   ELSIF old=GetErrorNode()
   THEN
      InternalError('replacing a temporary symbol (currently unexpected)', __FILE__, __LINE__)
   ELSE
      InternalError('should not be replacing a symbol', __FILE__, __LINE__)
   END ;

   IF IsConst(sym) AND (NOT IsValueSolved(sym))
   THEN
      PushIntegerTree(gcc) ;
      PopValue(sym)
   END
END AddModGcc ;


(*
   RemoveMod2Gcc - removes the gcc symbol from the lookup table.
*)

PROCEDURE RemoveMod2Gcc (sym: CARDINAL) ;
BEGIN
   PutIndice(mod2gcc, sym, NIL)   
END RemoveMod2Gcc ;


(*
   GccKnowsAbout - returns TRUE if gcc knows about the symbol, sym.
*)

PROCEDURE GccKnowsAbout (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( InBounds(mod2gcc, sym) AND (GetIndice(mod2gcc, sym)#NIL) )
END GccKnowsAbout ;


(*
   AddTemporaryKnown - adds a temporary gcc symbol against the modula-2 sym.
*)

PROCEDURE AddTemporaryKnown (sym: CARDINAL) ;
BEGIN
   (* we add the error node against symbol, sym. We expect it to be retacted later.. *)
   PutIndice(mod2gcc, sym, GetErrorNode())
END AddTemporaryKnown ;


(*
   RemoveTemporaryKnown - removes the temporary symbol.
*)

PROCEDURE RemoveTemporaryKnown (sym: CARDINAL) ;
BEGIN
   IF Mod2Gcc(sym)=GetErrorNode()
   THEN
      PutIndice(mod2gcc, sym, NIL)
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
      PutIndice(mod2gcc, sym, PoisonedSymbol)
   END
END Poison ;


(*
   Init - create both binary trees.
*)

PROCEDURE Init ;
BEGIN
   mod2gcc := InitIndex(1) ;
   ALLOCATE(PoisonedSymbol, 1)
END Init ;


BEGIN
   Init
END SymbolConversion.
