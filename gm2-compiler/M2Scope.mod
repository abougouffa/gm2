(* Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE M2Scope ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;

FROM SymbolTable IMPORT IsProcedure, IsDefImp, GetProcedureQuads, GetScope,
                        GetProcedureScope, IsModule, IsModuleWithinProcedure,
                        NulSym ;

FROM M2Quads IMPORT QuadOperator, GetFirstQuad, GetNextQuad, GetQuad ;
FROM M2StackWord IMPORT StackOfWord, InitStackWord, KillStackWord,
                        PopWord, PushWord, PeepWord ;


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
   AddToRange - returns a ScopeBlock pointer to the last block. The,
                quad, will be added to the end of sb or a later block
                if First is TRUE.
*)

PROCEDURE AddToRange (sb: ScopeBlock;
                      First: BOOLEAN; quad: CARDINAL) : ScopeBlock ;
BEGIN
   IF First
   THEN
      IF sb^.high=0
      THEN
         sb^.high := sb^.low
      END ;
      sb^.next := InitScopeBlock(0) ;
      sb := sb^.next
   END ;
   IF sb^.low=0
   THEN
      sb^.low := quad
   END ;
   sb^.high := quad ;
   RETURN( sb )
END AddToRange ;


(*
   GetGlobalQuads - 
*)

PROCEDURE GetGlobalQuads (sb: ScopeBlock; scope: CARDINAL) : ScopeBlock ;
VAR
   nb           : ScopeBlock ;
   NestedLevel,
   i            : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
   First        : BOOLEAN ;
   start, end   : CARDINAL ;
BEGIN
   NestedLevel := 0 ;
   First := FALSE ;
   IF (GetScope(scope)#NulSym) AND
      (IsProcedure(GetScope(scope)) OR
       (IsModule(scope) AND IsModuleWithinProcedure(scope)))
   THEN
      GetProcedureQuads(GetProcedureScope(scope), i, start, end) ;
      GetQuad(i, op, op1, op2, op3) ;
      WHILE (op#ModuleScopeOp) OR (op3#scope) DO
         i := GetNextQuad(i) ;
         GetQuad(i, op, op1, op2, op3)
      END ;
      end := i ;
      GetQuad(end, op, op1, op2, op3) ;
      WHILE (op#FinallyEndOp) OR (op3#scope) DO
         end := GetNextQuad(end) ;
         GetQuad(end, op, op1, op2, op3)
      END
   ELSE
      i := GetFirstQuad() ;
      end := 0
   END ;
   nb := sb ;
   sb^.low := 0 ;
   sb^.high := 0 ;
   LOOP
      IF i=0
      THEN
         RETURN( sb )
      END ;
      GetQuad(i, op, op1, op2, op3) ;
      IF op=ProcedureScopeOp
      THEN
         INC(NestedLevel)
      ELSIF op=ReturnOp
      THEN
         IF NestedLevel>0
         THEN
            DEC(NestedLevel)
         END ;
         IF NestedLevel=0
         THEN
            First := TRUE
         END
      ELSE
         IF NestedLevel=0
         THEN
            nb := AddToRange(nb, First, i) ;
            First := FALSE
         END
      END ;
      IF i=end
      THEN
         RETURN( sb )
      END ;
      i := GetNextQuad(i)
   END
END GetGlobalQuads ;


(*
   GetProcQuads - 
*)

PROCEDURE GetProcQuads (sb: ScopeBlock;
                        proc: CARDINAL) : ScopeBlock ;
VAR
   nb           : ScopeBlock ;
   scope, start,
   end, i, last : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
   First        : BOOLEAN ;
   s            : StackOfWord ;
BEGIN
   s := InitStackWord() ;
   GetProcedureQuads(proc, scope, start, end) ;
   PushWord(s, 0) ;
   First := FALSE ;
   i := scope ;
   last := scope ;
   nb := sb ;
   sb^.low := scope ;
   sb^.high := 0 ;
   WHILE (i<=end) AND (start#0) DO
      GetQuad(i, op, op1, op2, op3) ;
      IF (op=ProcedureScopeOp) OR (op=ModuleScopeOp)
      THEN
         IF (PeepWord(s, 1)=proc) AND (op3=proc)
         THEN
            nb := AddToRange(nb, First, last) ;
            First := FALSE
         END ;
         PushWord(s,  op3)
      ELSIF (op=ReturnOp) OR (op=FinallyEndOp)
      THEN
         op3 := PopWord(s) ;
         IF PeepWord(s, 1)=proc
         THEN
            First := TRUE
         END
      ELSE
         IF PeepWord(s, 1)=proc
         THEN
            nb := AddToRange(nb, First, i) ;
            First := FALSE
         END
      END ;
      last := i ;
      i := GetNextQuad(i)
   END ;
   IF start<=nb^.high
   THEN
      nb^.high := end
   ELSE
      nb^.next := InitScopeBlock(0) ;
      nb := nb^.next ;
      WITH nb^ DO
         low := start ;
         high := end
      END
   END ;
   s := KillStackWord(s) ;
   RETURN( sb )
END GetProcQuads ;


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
            sb := GetProcQuads(sb, scope)
         ELSE
            sb := GetGlobalQuads(sb, scope)
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
