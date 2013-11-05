(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                 2010
                 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)
IMPLEMENTATION MODULE M2Entity ;


FROM SYSTEM IMPORT ADDRESS ;
FROM SymbolTable IMPORT NulSym, IsConst ;
FROM M2Constants IMPORT IsSame ;
FROM M2Error IMPORT InternalError ;
FROM Indexing IMPORT Index, InitIndex, KillIndex, IncludeIndiceIntoIndex,
                     ForeachIndiceInIndexDo, GetIndice ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;

TYPE

   (*
      This module handles variable entities where an entity is
      a different value of a variable. Here we store two entities
      for variable, b.
      Consider: a := b + c ; b := d ;
      It follows the rules outlined by Trembley & Sorenson p.624 of
      The Theory and Practice of Compiler Writing
   *)

   Entity = POINTER TO entity ;
   entity = RECORD
               Symbol     : CARDINAL ;  (* symbol table id              *)
               IsClean    : BOOLEAN ;   (* have we cleaned this entity? *)
               IndexNo    : CARDINAL ;  (* which version of variable    *)
               IsLeftValue: BOOLEAN ;   (* is entity really &Sym ?      *)
               TokenNo    : CARDINAL ;  (* the associated token to sym  *)
            END ;

VAR
   NoOfEntities: CARDINAL ;
   EntityList  : Index ;


(*
   DeallocateEntity - 
*)

PROCEDURE DeallocateEntity (e: ADDRESS) ;
VAR
   p: Entity ;
BEGIN
   p := e ;
   DISPOSE(p)
END DeallocateEntity ;


(*
   InitEntities - destroy all entities.
*)

PROCEDURE InitEntities ;
BEGIN
   NoOfEntities := 0 ;
   ForeachIndiceInIndexDo(EntityList, DeallocateEntity) ;
   EntityList := KillIndex(EntityList) ;
   EntityList := InitIndex(1)
END InitEntities ;


(*
   GetNoOfEntities - returns the number of entities.
*)

PROCEDURE GetNoOfEntities () : CARDINAL ;
BEGIN
   RETURN( NoOfEntities )
END GetNoOfEntities ;


(*
   FindLatestEntity - returns the latest entity, possibly NIL.
*)

PROCEDURE FindLatestEntity (Sym: CARDINAL; IsLeft: BOOLEAN) : Entity ;
VAR
   i, highest: CARDINAL ;
   e,
   latest    : Entity ;
BEGIN
   i := GetNoOfEntities() ;
   highest := 0 ;
   latest := NIL ;
   WHILE i>0 DO
      e := GetIndice(EntityList, i) ;
      WITH e^ DO
         IF IsSame(TokenNo, Symbol, Sym) AND (IndexNo>=highest) AND (IsLeft=IsLeftValue)
         THEN
            highest := IndexNo ;
            latest := e
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

PROCEDURE GetLatestEntity (tokenno: CARDINAL; Sym: CARDINAL; IsLeft: BOOLEAN; Index: CARDINAL) : Entity ;
VAR
   latest: Entity ;
BEGIN
   latest := FindLatestEntity(Sym, IsLeft) ;
   IF latest=NIL
   THEN
      RETURN( MakeNewEntity(tokenno, Sym, IsLeft, Index) )
   ELSE
      RETURN( latest )
   END
END GetLatestEntity ;


(*
   IsEntityClean - returns TRUE if the entity, e, is clean.
*)

PROCEDURE IsEntityClean (e: Entity) : BOOLEAN ;
BEGIN
   RETURN( e^.IsClean )
END IsEntityClean ;


(*
   IsLValue - returns TRUE if entity, e, is an LValue
*)

PROCEDURE IsLValue (e: Entity) : BOOLEAN ;
BEGIN
   RETURN( e^.IsLeftValue )
END IsLValue ;


(*
   MakeEntityDirty - 
*)

PROCEDURE MakeEntityDirty (e: Entity) ;
BEGIN
   e^.IsClean := FALSE
END MakeEntityDirty ;


(*
   MakeEntityClean - 
*)

PROCEDURE MakeEntityClean (e: Entity) ;
BEGIN
   e^.IsClean := TRUE
END MakeEntityClean ;


(*
   GiveEntityIndex - 
*)

PROCEDURE GiveEntityIndex (e: Entity; index: CARDINAL) ;
BEGIN
   e^.IndexNo := index
END GiveEntityIndex ;


(*
   GetEntityIndex - 
*)

PROCEDURE GetEntityIndex (e: Entity) : CARDINAL ;
BEGIN
   RETURN( e^.IndexNo )
END GetEntityIndex ;


(*
   GetEntitySym - 
*)

PROCEDURE GetEntitySym (e: Entity) : CARDINAL ;
BEGIN
   IF e=NIL
   THEN
      RETURN( NulSym )
   ELSE
      RETURN( e^.Symbol )
   END
END GetEntitySym ;


(*
   CheckEntityConsistancy - 
*)

PROCEDURE CheckEntityConsistancy ;
VAR
   i   : CARDINAL ;
   e, f: Entity ;
BEGIN
   f := GetIndice(EntityList, NoOfEntities) ;  (* last in the list *)
   i := 1 ;
   WHILE i<NoOfEntities DO
      e := GetIndice(EntityList, i) ;
      IF (e^.Symbol=f^.Symbol) AND (e^.IndexNo=f^.IndexNo) AND
         (e^.IsLeftValue=f^.IsLeftValue)
      THEN
         InternalError('the same entity is declared twice', __FILE__, __LINE__)
      END ;
      INC(i)
   END
END CheckEntityConsistancy ;


(*
   MakeNewEntity - makes a new entity for symbol, Sym.
*)

PROCEDURE MakeNewEntity (tokenno: CARDINAL; Sym: CARDINAL; IsLeft: BOOLEAN; Index: CARDINAL) : Entity ;
VAR
   latest: Entity ;
BEGIN
   latest := FindLatestEntity(Sym, IsLeft) ;
   IF (latest#NIL) AND (GetEntityIndex(latest)=Index)
   THEN
      RETURN( latest )
   ELSE
      INC(NoOfEntities) ;
      NEW(latest) ;
      IF latest=NIL
      THEN
         InternalError('out of memory error', __FILE__, __LINE__)
      END ;
      IncludeIndiceIntoIndex(EntityList, latest) ;
      WITH latest^ DO
         IsClean     := IsConst(Sym) ;
         IndexNo     := Index ;
         Symbol      := Sym ;
         IsLeftValue := IsLeft ;
         TokenNo     := tokenno
      END ;
      CheckEntityConsistancy ;
      RETURN( latest )
   END
END MakeNewEntity ;


(*
   HasLValue - returns TRUE if symbol, Sym, has an LValue
*)

PROCEDURE HasLValue (Sym: CARDINAL) : BOOLEAN ;
VAR
   i: CARDINAL ;
   e: Entity ;
BEGIN
   i := GetNoOfEntities() ;
   WHILE i>0 DO
      e := GetIndice(EntityList, i) ;
      IF (GetEntitySym(e)=Sym) AND IsLValue(e)
      THEN
         RETURN( TRUE )
      END ;
      DEC(i)
   END ;
   RETURN( FALSE )
END HasLValue ;


END M2Entity.
