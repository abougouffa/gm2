(* Copyright (C) 2009 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE M2CaseList ;


FROM M2Debug IMPORT Assert ;
FROM M2GCCDeclare IMPORT TryDeclareConstant ;
FROM M2MetaError IMPORT MetaError2, MetaErrorT1, MetaError1, MetaErrorT2, MetaErrorT3, MetaErrorT4 ;
FROM M2Error IMPORT InternalError ;
FROM M2Range IMPORT OverlapsRange ;
FROM Indexing IMPORT Index, InitIndex, PutIndice, GetIndice, ForeachIndiceInIndexDo, HighIndice ;
FROM SymbolTable IMPORT NulSym, IsConst, IsFieldVarient, IsRecord ;
FROM SymbolConversion IMPORT GccKnowsAbout, Mod2Gcc ;
FROM gccgm2 IMPORT Tree ;
FROM Storage IMPORT ALLOCATE ;

TYPE
   RangePair = POINTER TO rangePair ;
   rangePair = RECORD
                  low, high: CARDINAL ;
                  tokenno  : CARDINAL ;
               END ;

   ConflictingPair = POINTER TO conflictingPair ;
   conflictingPair = RECORD
                        a, b: RangePair ;
                     END ;

   CaseList = POINTER TO caseList ;
   caseList = RECORD
                 maxRangeId  : CARDINAL ;
                 rangeArray  : Index ;
                 currentRange: RangePair ;
                 varient     : CARDINAL ;
              END ;

   CaseDescriptor = POINTER TO caseDescriptor ;
   caseDescriptor = RECORD
                       elseClause   : BOOLEAN ;
                       record       : CARDINAL ;
                       maxCaseId    : CARDINAL ;
                       caseListArray: Index ;
                       currentCase  : CaseList ;
                       next         : CaseDescriptor ;
                    END ;

VAR
   caseStack    : CaseDescriptor ;
   caseId       : CARDINAL ;
   caseArray    : Index ;
   conflictArray: Index ;


(*
   PushCase - create a case entity and push it to an internal stack.
              r, is NulSym is this is a CASE statement and a record
              if it is to indicate a varient record.
              Return the case id.
*)

PROCEDURE PushCase (r: CARDINAL) : CARDINAL ;
VAR
   c: CaseDescriptor ;
BEGIN
   INC(caseId) ;
   NEW(c) ;
   IF c=NIL
   THEN
      InternalError('out of memory error', __FILE__, __LINE__)
   ELSE
      WITH c^ DO
         elseClause := FALSE ;
         record := r ;
         maxCaseId := 0 ;
         caseListArray := InitIndex(1) ;
         next := caseStack ;
         currentCase := NIL
      END ;
      caseStack := c ;
      PutIndice(caseArray, caseId, c)
   END ;
   RETURN( caseId )
END PushCase ;


(*
   PopCase - pop the top element of the case entity from the internal
             stack.
*)

PROCEDURE PopCase ;
BEGIN
   IF caseStack=NIL
   THEN
      InternalError('case stack is empty', __FILE__, __LINE__)
   END ;
   caseStack := caseStack^.next
END PopCase ;


(*
   ElseCase - indicates that this case varient does have an else clause.
*)

PROCEDURE ElseCase ;
BEGIN
   caseStack^.elseClause := TRUE
END ElseCase ;


(*
   BeginCaseList - create a new label list.
*)

PROCEDURE BeginCaseList (v: CARDINAL) ;
VAR
   l: CaseList ;
BEGIN
   NEW(l) ;
   IF l=NIL
   THEN
      InternalError('out of memory error', __FILE__, __LINE__)
   END ;
   WITH l^ DO
      maxRangeId   := 0 ;
      rangeArray   := InitIndex(1) ;
      currentRange := NIL ;
      varient      := v
   END ;
   WITH caseStack^ DO
      INC(maxCaseId) ;
      PutIndice(caseListArray, maxCaseId, l) ;
      currentCase := l
   END
END BeginCaseList ;


(*
   EndCaseList - terminate the current label list.
*)

PROCEDURE EndCaseList ;
BEGIN
   caseStack^.currentCase := NIL
END EndCaseList ;


(*
   AddRange - add a range to the current label list.
*)

PROCEDURE AddRange (r1, r2: CARDINAL; tok: CARDINAL) ;
VAR
   r: RangePair ;
BEGIN
   NEW(r) ;
   IF r=NIL
   THEN
      InternalError('out of memory error', __FILE__, __LINE__)
   ELSE
      WITH r^ DO
         low := r1 ;
         high := r2 ;
         tokenno := tok
      END ;
      WITH caseStack^.currentCase^ DO
         INC(maxRangeId) ;
         PutIndice(rangeArray, maxRangeId, r) ;
         currentRange := r
      END
   END
END AddRange ;


(*
   CaseBoundsResolved - returns TRUE if all constants in the case list, c,
                        are known to GCC.
*)

PROCEDURE CaseBoundsResolved (tokenno: CARDINAL; c: CARDINAL) : BOOLEAN ;
VAR
   p   : CaseDescriptor ;
   q   : CaseList ;
   r   : RangePair ;
   i, j: CARDINAL ;
BEGIN
   p := GetIndice(caseArray, c) ;
   WITH p^ DO
      i := 1 ;
      WHILE i<=maxCaseId DO
         q := GetIndice(caseListArray, i) ;
         j := 1 ;
         WHILE j<=q^.maxRangeId DO
            r := GetIndice(q^.rangeArray, j) ;
            IF r^.low#NulSym
            THEN
               IF IsConst(r^.low)
               THEN
                  TryDeclareConstant(tokenno, r^.low) ;
                  IF NOT GccKnowsAbout(r^.low)
                  THEN
                     RETURN( FALSE )
                  END
               ELSE
                  IF r^.high=NulSym
                  THEN
                     MetaError1('the CASE statement variant must be defined by a constant {%1Da:is a {%1d}}', r^.low)
                  ELSE
                     MetaError1('the CASE statement variant low value in a range must be defined by a constant {%1Da:is a {%1d}}',
                                r^.low)
                  END
               END
            END ;
            IF r^.high#NulSym
            THEN
               IF IsConst(r^.high)
               THEN
                  TryDeclareConstant(tokenno, r^.high) ;
                  IF NOT GccKnowsAbout(r^.high)
                  THEN
                     RETURN( FALSE )
                  END
               ELSE
                  MetaError1('the CASE statement variant high value in a range must be defined by a constant {%1Da:is a {%1d}}',
                             r^.high)
               END
            END ;
            INC(j)
         END ;
         INC(i)
      END
   END ;
   RETURN( TRUE )
END CaseBoundsResolved ;


(*
   IsSame - return TRUE if r, s, are in, e.
*)

PROCEDURE IsSame (e: ConflictingPair; r, s: RangePair) : BOOLEAN ;
BEGIN
   WITH e^ DO
      RETURN( ((a=r) AND (b=s)) OR ((a=s) AND (b=r)) )
   END
END IsSame ;


(*
   SeenBefore - 
*)

PROCEDURE SeenBefore (r, s: RangePair) : BOOLEAN ;
VAR
   i, h: CARDINAL ;
   e   : ConflictingPair ;
BEGIN
   h := HighIndice(conflictArray) ;
   i := 1 ;
   WHILE i<=h DO
      e := GetIndice(conflictArray, i) ;
      IF IsSame(e, r, s)
      THEN
         RETURN( TRUE )
      END ;
      INC(i)
   END ;
   NEW(e) ;
   WITH e^ DO
      a := r ;
      b := s
   END ;
   PutIndice(conflictArray, h+1, e) ;
   RETURN( FALSE )
END SeenBefore ;


(*
   Overlaps - 
*)

PROCEDURE Overlaps (tokenno: CARDINAL; r, s: RangePair) : BOOLEAN ;
VAR
   a, b, c, d: CARDINAL ;
BEGIN
   a := r^.low ;
   c := s^.low ;
   IF r^.high=NulSym
   THEN
      b := a ;
      IF s^.high=NulSym
      THEN
         d := c ;
         IF OverlapsRange(Mod2Gcc(a), Mod2Gcc(b), Mod2Gcc(c), Mod2Gcc(d))
         THEN
            IF NOT SeenBefore(r, s)
            THEN
               MetaErrorT2(r^.tokenno, 'case label {%1ad} is a duplicate with {%2ad}', a, c) ;
               MetaErrorT2(s^.tokenno, 'case label {%1ad} is a duplicate with {%2ad}', c, a)
            END ;
            RETURN( TRUE )
         END
      ELSE
         d := s^.high ;
         IF OverlapsRange(Mod2Gcc(a), Mod2Gcc(b), Mod2Gcc(c), Mod2Gcc(d))
         THEN
            IF NOT SeenBefore(r, s)
            THEN
               MetaErrorT3(r^.tokenno, 'case label {%1ad} is a duplicate in the range {%2ad}..{%3ad}', a, c, d) ;
               MetaErrorT3(s^.tokenno, 'case range {%2ad}..{%3ad} is a duplicate of case label {%1ad}', c, d, a)
            END ;
            RETURN( TRUE )
         END
      END
   ELSE
      b := r^.high ; 
      IF s^.high=NulSym
      THEN
         d := c ;
         IF OverlapsRange(Mod2Gcc(a), Mod2Gcc(b), Mod2Gcc(c), Mod2Gcc(d))
         THEN
            IF NOT SeenBefore(r, s)
            THEN
               MetaErrorT3(r^.tokenno, 'case range {%1ad}..{%2ad} is a duplicate with case label {%3ad}', a, b, c) ;
               MetaErrorT3(s^.tokenno, 'case label {%1ad} is a duplicate with case range %{2ad}..{%3ad}', c, a, b)
            END ;
            RETURN( TRUE )
         END
      ELSE
         d := s^.high ; 
         IF OverlapsRange(Mod2Gcc(a), Mod2Gcc(b), Mod2Gcc(c), Mod2Gcc(d))
         THEN
            IF NOT SeenBefore(r, s)
            THEN
               MetaErrorT4(r^.tokenno, 'case range {%1ad}..{%2ad} overlaps case range {%3ad}..{%4ad}', a, b, c, d) ;
               MetaErrorT4(s^.tokenno, 'case range {%1ad}..{%2ad} overlaps case range {%3ad}..{%4ad}', c, d, a, b)
            END ;
            RETURN( TRUE )
         END
      END
   END ;
   RETURN( FALSE )
END Overlaps ;


(*
   OverlappingCaseBound - returns TRUE if, r, overlaps any case bound in the
                          case statement, c.
*)

PROCEDURE OverlappingCaseBound (tokenno: CARDINAL; r: RangePair; c: CARDINAL) : BOOLEAN ;
VAR
   p      : CaseDescriptor ;
   q      : CaseList ;
   s      : RangePair ;
   i, j   : CARDINAL ;
   overlap: BOOLEAN ;
BEGIN
   p := GetIndice(caseArray, c) ;
   overlap := FALSE ;
   WITH p^ DO
      i := 1 ;
      WHILE i<=maxCaseId DO
         q := GetIndice(caseListArray, i) ;
         j := 1 ;
         WHILE j<=q^.maxRangeId DO
            s := GetIndice(q^.rangeArray, j) ;
            IF (s#r) AND Overlaps(tokenno, r, s)
            THEN
               overlap := TRUE
            END ;
            INC(j)
         END ;
         INC(i)
      END
   END ;
   RETURN( overlap )
END OverlappingCaseBound ;


(*
   OverlappingCaseBounds - returns TRUE if there were any overlapping bounds
                           in the case list, c.  It will generate an error
                           messages for each overlapping bound found.
*)

PROCEDURE OverlappingCaseBounds (tokenno: CARDINAL; c: CARDINAL) : BOOLEAN ;
VAR
   p      : CaseDescriptor ;
   q      : CaseList ;
   r      : RangePair ;
   i, j   : CARDINAL ;
   overlap: BOOLEAN ;
BEGIN
   p := GetIndice(caseArray, c) ;
   overlap := FALSE ;
   WITH p^ DO
      i := 1 ;
      WHILE i<=maxCaseId DO
         q := GetIndice(caseListArray, i) ;
         j := 1 ;
         WHILE j<=q^.maxRangeId DO
            r := GetIndice(q^.rangeArray, j) ;
            IF OverlappingCaseBound(tokenno, r, c)
            THEN
               overlap := TRUE
            END ;
            INC(j)
         END ;
         INC(i)
      END
   END ;
   RETURN( overlap )
END OverlappingCaseBounds ;


(*
   WriteCase - 
*)

PROCEDURE WriteCase (c: CARDINAL) ;
BEGIN
   
END WriteCase ;


(*
   InRangeList - returns TRUE if the value, tag, is defined in the case list.
*)

PROCEDURE InRangeList (cl: CaseList; tag: CARDINAL) : BOOLEAN ;
VAR
   i, h: CARDINAL ;
   r   : RangePair ;
   a   : Tree ;
BEGIN
   WITH cl^ DO
      i := 1 ;
      h := HighIndice(rangeArray) ;
      WHILE i<=h DO
         r := GetIndice(rangeArray, i) ;
         WITH r^ DO
            IF high=NulSym
            THEN
               a := Mod2Gcc(low)
            ELSE
               a := Mod2Gcc(high)
            END ;
            IF OverlapsRange(Mod2Gcc(low), a, Mod2Gcc(tag), Mod2Gcc(tag))
            THEN
               RETURN( TRUE )
            END
         END ;
         INC(i)
      END
   END ;
   RETURN( FALSE )
END InRangeList ;


(*
   FindVarientField - returns the appropriate varient field associated with, tag,
                      in, c.
*)

PROCEDURE FindVarientField (c: CaseDescriptor; tag: CARDINAL) : CARDINAL ;
VAR
   i, h: CARDINAL ;
   cl  : CaseList ;
   v   : CARDINAL ;
BEGIN
   WITH c^ DO
      i := 1 ;
      h := HighIndice(caseListArray) ;
      WHILE i<=h DO
         cl := GetIndice(caseListArray, i) ;
         IF InRangeList(cl, tag)
         THEN
            RETURN( cl^.varient )
         END ;
         INC(i)
      END ;
      IF NOT elseClause
      THEN
         MetaError2('tag value {%1an} does not correspond with any varient field value in record {%2ad}',
                    tag, c^.record) ;
      END
   END ;
   RETURN( NulSym )
END FindVarientField ;


(*
   FindVarient - returns a varient field of, record, given the value, tag.
*)

PROCEDURE FindVarient (tag, record: CARDINAL) : CARDINAL ;
VAR
   c   : CaseDescriptor ;
   i, h: CARDINAL ;
BEGIN
   IF IsRecord(record)
   THEN
      i := 1 ;
      h := HighIndice(caseArray) ;
      WHILE i<=h DO
         c := GetIndice(caseArray, i) ;
         IF c^.record=record
         THEN
            RETURN( FindVarientField(c, tag) )
         END ;
         INC(i)
      END
   ELSE
      MetaError1('expecting the first parameter of TSIZE to be a record type, rather than {%1d}', record) ;
      RETURN( NulSym )
   END
END FindVarient ;


BEGIN
   caseStack := NIL ;
   caseId := 0 ;
   caseArray := InitIndex(1) ;
   conflictArray := InitIndex(1)
END M2CaseList.
