(* Copyright (C) 2003 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE M2Scope ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM SymbolTable IMPORT IsProcedure, IsDefImp, GetProcedureQuads ;
FROM M2Quads IMPORT QuadOperator, Head, GetNextQuad, GetQuad ;


TYPE
   ScopeBlock = POINTER TO scopeblock ;
   scopeblock = RECORD
                   low, high: CARDINAL ;
                   next     : ScopeBlock ;
                END ;

VAR
   FreeList: ScopeBlock ;


(*
   New - 
*)

PROCEDURE New (VAR sb: ScopeBlock) ;
BEGIN
   IF FreeList=NIL
   THEN
      NEW(sb)
   ELSE
      sb := FreeList ;
      FreeList := FreeList^.next
   END
END New ;


(*
   Dispose - 
*)

PROCEDURE Dispose (VAR sb: ScopeBlock) ;
BEGIN
   sb^.next := FreeList ;
   FreeList := sb ;
   sb := NIL
END Dispose ;


(*
   GetGlobalQuads - 
*)

PROCEDURE GetGlobalQuads (sb: ScopeBlock) ;
VAR
   i            : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
   First,
   InProcedure  : BOOLEAN ;
BEGIN
   InProcedure := FALSE ;
   First := FALSE ;
   i := Head ;
   sb^.low := 0 ;
   sb^.high := 0 ;
   WHILE i#0 DO
      GetQuad(i, op, op1, op2, op3) ;
      IF op=NewLocalVarOp
      THEN
         InProcedure := TRUE
      ELSIF op=ReturnOp
      THEN
         InProcedure := FALSE ;
         First := TRUE
      ELSE
         IF NOT InProcedure
         THEN
            IF First
            THEN
               First := FALSE ;
               sb^.next := InitScopeBlock(0) ;
               sb := sb^.next
            END ;
            IF sb^.low=0
            THEN
               sb^.low := i
            END ;
            sb^.high := i
         END
      END ;
      i := GetNextQuad(i)
   END
END GetGlobalQuads ;


(*
   InitScopeBlock - 
*)

PROCEDURE InitScopeBlock (scope: CARDINAL) : ScopeBlock ;
VAR
   sb: ScopeBlock ;
BEGIN
   New(sb) ;
   WITH sb^ DO
      next := NIL ;
      IF scope=0
      THEN
         low := 0 ;
         high := 0
      ELSE
         IF IsProcedure(scope)
         THEN
            GetProcedureQuads(scope, low, high)
         ELSE
            GetGlobalQuads(sb)
         END
      END
   END ;
   RETURN( sb )
END InitScopeBlock ;


(*
   KillScopeBlock - 
*)

PROCEDURE KillScopeBlock (sb: ScopeBlock) : ScopeBlock ;
VAR
   t: ScopeBlock ;
BEGIN
   t := sb ;
   WHILE t#NIL DO
      sb := t ;
      t := t^.next ;
      Dispose(sb) ;
   END ;
   RETURN( NIL )
END KillScopeBlock ;


(*
   ForeachScopeBlockDo - 
*)

PROCEDURE ForeachScopeBlockDo (sb: ScopeBlock; p: ScopeProcedure) ;
BEGIN
   WHILE sb#NIL DO
      WITH sb^ DO
         IF (low#0) AND (high#0)
         THEN
            p(low, high)
         END
      END ;
      sb := sb^.next
   END
END ForeachScopeBlockDo ;


(*
   Init - initializes the global variables for this module.
*)

PROCEDURE Init ;
BEGIN
   FreeList := NIL
END Init ;


BEGIN
   Init
END M2Scope.
