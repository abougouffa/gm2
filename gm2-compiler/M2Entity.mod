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
IMPLEMENTATION MODULE M2Entity ;


FROM SymbolTable IMPORT NulSym, IsConst ;
FROM M2Constants IMPORT IsSame ;
FROM M2Lexical IMPORT InternalError ;


TYPE

   (*
      the version of the variable might vary during a basic block as
      the variable might be altered, consider: a := b + c ; b := d ;
      We set the IndexNo to the NodeIndex for the SymId record and
      set the Node.IndexNo to the maximum operand IndexNo+1. Follow
      the rules outlined by Trembley & Sorenson P.624 of The Theory and
      Practice of Compiler Writing
   *)

   EntityId = RECORD
                 Symbol     : CARDINAL ;  (* symbol table id              *)
                 IsClean    : BOOLEAN ;   (* have we cleaned this entity? *)
                 IndexNo    : CARDINAL ;  (* which version of variable    *)
                 IsLeftValue: BOOLEAN ;   (* is entity really &Sym ?      *)
              END ;

VAR
   NoOfEntities: CARDINAL ;
   EntityList  : ARRAY [1..MaxEntity] OF EntityId ;


(*
   InitEntities - destroy all entities.
*)

PROCEDURE InitEntities ;
BEGIN
   NoOfEntities := 0 ;   
END InitEntities ;


(*
   GetNoOfEntities - returns the number of entities.
*)

PROCEDURE GetNoOfEntities () : CARDINAL ;
BEGIN
   RETURN( NoOfEntities )
END GetNoOfEntities ;


(*
   FindLatestEntity - returns the latest entity, possibly NulEntity.
*)

PROCEDURE FindLatestEntity (Sym: CARDINAL; IsLeft: BOOLEAN) : Entities ;
VAR
   i, highest,
   latest    : CARDINAL ;
BEGIN
   i := GetNoOfEntities() ;
   highest := 0 ;
   latest := NulEntity ;
   WHILE i>0 DO
      WITH EntityList[i] DO
         IF IsSame(Symbol, Sym) AND (IndexNo>=highest) AND (IsLeft=IsLeftValue)
         THEN
            highest := IndexNo ;
            latest := i
         END
      END ;
      DEC(i)
   END ;
   RETURN( latest )
END FindLatestEntity ;


(*
   GetLatestEntity - returns the latest entity of a variable.
                     It will create one if necessary.
*)

PROCEDURE GetLatestEntity (Sym: CARDINAL; IsLeft: BOOLEAN; Index: CARDINAL) : Entities ;
VAR
   latest: CARDINAL ;
BEGIN
   latest := FindLatestEntity(Sym, IsLeft) ;
   IF latest=0
   THEN
      RETURN( MakeNewEntity(Sym, IsLeft, Index) )
   ELSE
      RETURN( latest )
   END
END GetLatestEntity ;


(*
   IsEntityClean - returns TRUE if the entity, e, is clean.
*)

PROCEDURE IsEntityClean (e: Entities) : BOOLEAN ;
BEGIN
   RETURN( EntityList[e].IsClean )
END IsEntityClean ;


(*
   IsLValue - returns TRUE if entity, e, is an LValue
*)

PROCEDURE IsLValue (e: Entities) : BOOLEAN ;
BEGIN
   RETURN( EntityList[e].IsLeftValue )
END IsLValue ;


(*
   MakeEntityDirty - 
*)

PROCEDURE MakeEntityDirty (e: Entities) ;
BEGIN
   EntityList[e].IsClean := FALSE
END MakeEntityDirty ;


(*
   MakeEntityClean - 
*)

PROCEDURE MakeEntityClean (e: Entities) ;
BEGIN
   EntityList[e].IsClean := TRUE
END MakeEntityClean ;


(*
   GiveEntityIndex - 
*)

PROCEDURE GiveEntityIndex (e: Entities; index: CARDINAL) ;
BEGIN
   EntityList[e].IndexNo := index
END GiveEntityIndex ;


(*
   GetEntityIndex - 
*)

PROCEDURE GetEntityIndex (e: Entities) : CARDINAL ;
BEGIN
   RETURN( EntityList[e].IndexNo )
END GetEntityIndex ;


(*
   GetEntitySym - 
*)

PROCEDURE GetEntitySym (e: Entities) : CARDINAL ;
BEGIN
   IF e=NulEntity
   THEN
      RETURN( NulSym )
   ELSE
      RETURN( EntityList[e].Symbol )
   END
END GetEntitySym ;


(*
   CheckEntityConsistancy - 
*)

PROCEDURE CheckEntityConsistancy ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE i<NoOfEntities DO
      IF (EntityList[i].Symbol=EntityList[NoOfEntities].Symbol) AND
         (EntityList[i].IndexNo=EntityList[NoOfEntities].IndexNo) AND
         (EntityList[i].IsLeftValue=EntityList[NoOfEntities].IsLeftValue)
      THEN
         InternalError('the same entity is declared twice', __FILE__, __LINE__)
      END ;
      INC(i)
   END
END CheckEntityConsistancy ;


(*
   MakeNewEntity - makes a new entity for symbol, Sym.
*)

PROCEDURE MakeNewEntity (Sym: CARDINAL; IsLeft: BOOLEAN; Index: CARDINAL) : Entities ;
VAR
   latest: Entities ;
BEGIN
   IF NoOfEntities=MaxEntity
   THEN
      InternalError('increase MaxEntity', __FILE__, __LINE__)
   ELSE
      latest := FindLatestEntity(Sym, IsLeft) ;
      IF (latest#NulEntity) AND (GetEntityIndex(latest)=Index)
      THEN
         RETURN( latest )
      ELSE
         INC(NoOfEntities) ;
         WITH EntityList[NoOfEntities] DO
            IsClean     := IsConst(Sym) ;
            IndexNo     := Index ;
            Symbol      := Sym ;
            IsLeftValue := IsLeft
         END ;
         CheckEntityConsistancy ;
         RETURN( NoOfEntities )
      END
   END
END MakeNewEntity ;


END M2Entity.
