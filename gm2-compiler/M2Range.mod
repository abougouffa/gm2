(* Copyright (C) 2008 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE M2Range ;



FROM SymbolTable IMPORT NulSym, GetLowestType, PutReadQuad, RemoveReadQuad,
                        IsVar, IsConst, PushValue, GetSubrange, GetType,
                        IsSubrange, GetSymName, IsTemporary,
                        IsRecord, IsPointer, IsArray, IsProcType, IsConstLit,
                        IsAModula2Type, IsUnbounded, IsEnumeration, GetMode,
                        ModeOfAddr ;

FROM gccgm2 IMPORT Tree, GetMinFrom, GetMaxFrom, BuildSub,
                   BuildAdd, BuildConvert, BuildProcedureCallTree,
                   BuildIfThenElseEnd, BuildIfThenDoEnd, IsTrue, IsFalse,
                   GetIntegerZero, GetIntegerOne, GetIntegerType, GetTreeType,
                   CompareTrees, BuildIndirect, BuildParam, BuildStringConstant,
                   BuildIntegerConstant, AddStatement, BuildGreaterThan, BuildLessThan,
                   BuildNegate, BuildEqualTo, GetPointerType, GetPointerZero, BuildAddr ;

FROM M2Debug IMPORT Assert ;
FROM Indexing IMPORT Index, InitIndex, InBounds, PutIndice, GetIndice ;
FROM Storage IMPORT ALLOCATE ;
FROM M2ALU IMPORT Equ, PushIntegerTree, Gre, Less ;
FROM M2Error IMPORT Error, InternalError, NewWarning, NewError, ErrorFormat0, ErrorFormat1, ErrorFormat2, ErrorString ;
FROM M2LexBuf IMPORT GetTokenNo, FindFileNameFromToken, TokenToLineNo, TokenToColumnNo ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM M2GCCDeclare IMPORT DeclareConstant ;
FROM M2Quads IMPORT QuadOperator, PutQuad, SubQuad, WriteOperand ;
FROM SymbolConversion IMPORT GccKnowsAbout, Mod2Gcc ;
FROM Lists IMPORT List ;
FROM NameKey IMPORT Name ;
FROM StdIO IMPORT Write ;
FROM DynamicStrings IMPORT String, string, Length, InitString, ConCat, ConCatChar, Mark ;
FROM M2GenGCC IMPORT GetHighFromUnbounded ;
FROM M2System IMPORT Address ;

FROM M2Base IMPORT Nil, IsRealType, GetBaseTypeMinMax,
                   Cardinal,
                   ExceptionAssign, ExceptionInc, ExceptionDec,
                   ExceptionStaticArray, ExceptionDynamicArray,
                   ExceptionForLoopBegin, ExceptionForLoopTo, ExceptionForLoopEnd,
                   ExceptionPointerNil, ExceptionNoReturn, ExceptionCase,
                   ExceptionNo ;


TYPE
   TypeOfRange = (assignment, subrangeassignment,
                  inc, dec, staticarraysubscript,
                  dynamicarraysubscript,
                  forloopbegin, forloopto, forloopend,
                  pointernil, noreturn, noelse,
                  none) ;

   Range = POINTER TO range ;   (* to help p2c *)
   range =            RECORD
                         type          : TypeOfRange ;
                         des,
                         expr,
                         desLowestType,
                         exprLowestType: CARDINAL ;
                         isLeftValue   : BOOLEAN ;  (* is des an LValue,
                                                       only used in pointernil *)
                         tokenNo       : CARDINAL ;
                      END ;


VAR
   TopOfRange            : CARDINAL ;
   RangeIndex            : Index ;


(*
   OverlapsRange - returns TRUE if a1..a2 overlaps with b1..b2.
*)

PROCEDURE OverlapsRange (a1, a2, b1, b2: Tree) : BOOLEAN ;
BEGIN
   (* RETURN( ((a1<=b2) AND (a2>=b1)) ) *)
   RETURN( (CompareTrees(a1, b2)<=0) AND (CompareTrees(a2, b1)>=0) )
END OverlapsRange ;


(*
   IsGreater - returns TRUE if a>b.
*)

PROCEDURE IsGreater (a, b: Tree) : BOOLEAN ;
BEGIN
   RETURN( CompareTrees(a, b)>0 )
END IsGreater ;


(*
   lookupExceptionHandler - 
*)

PROCEDURE lookupExceptionHandler (type: TypeOfRange) : CARDINAL ;
BEGIN
   CASE type OF

   assignment           : RETURN( ExceptionAssign ) |
   subrangeassignment   : InternalError('not expecting this case value', __FILE__, __LINE__) |
   inc                  : RETURN( ExceptionInc ) |
   dec                  : RETURN( ExceptionDec ) |
   staticarraysubscript : RETURN( ExceptionStaticArray ) |
   dynamicarraysubscript: RETURN( ExceptionDynamicArray ) |
   forloopbegin         : RETURN( ExceptionForLoopBegin ) |
   forloopto            : RETURN( ExceptionForLoopTo ) |
   forloopend           : RETURN( ExceptionForLoopEnd ) |
   pointernil           : RETURN( ExceptionPointerNil ) |
   noreturn             : RETURN( ExceptionNoReturn ) |
   noelse               : RETURN( ExceptionCase ) |
   none                 : RETURN( ExceptionNo )

   ELSE
      InternalError('enumeration value unknown', __FILE__, __LINE__)
   END
END lookupExceptionHandler ;


(*
   InitRange - returns a new range item.
*)

PROCEDURE InitRange () : CARDINAL ;
VAR
   r: CARDINAL ;
   p: Range ;
BEGIN
   INC(TopOfRange) ;
   r := TopOfRange ;
   NEW(p) ;
   IF p=NIL
   THEN
      InternalError('out of memory error', __FILE__, __LINE__)
   ELSE
      WITH p^ DO
         type           := none ;
         des            := NulSym ;
         expr           := NulSym ;
         desLowestType  := NulSym ;
         exprLowestType := NulSym ;
         isLeftValue    := FALSE ;   (* ignored in all cases other *)
         tokenNo        := 0         (* than pointernil            *)
      END ;
      PutIndice(RangeIndex, r, p)
   END ;
   RETURN( r )
END InitRange ;


(*
   PutRange - initializes contents of, p, to
              d, e and their lowest types.
              It also fills in the current token no
              and returns, p.
*)

PROCEDURE PutRange (p: Range; t: TypeOfRange; d, e: CARDINAL) : Range ;
BEGIN
   WITH p^ DO
      type           := t ;
      des            := d ;
      expr           := e ;
      desLowestType  := GetLowestType(d) ;
      exprLowestType := GetLowestType(e) ;
      tokenNo        := GetTokenNo()
   END ;
   RETURN( p )
END PutRange ;


(*
   PutRangePointer - initializes contents of, p, to
                     d, isLeft and their lowest types.
                     It also fills in the current token no
                     and returns, p.
*)

PROCEDURE PutRangePointer (p: Range; d: CARDINAL; isLeft: BOOLEAN) : Range ;
BEGIN
   WITH p^ DO
      type           := pointernil ;
      des            := d ;
      expr           := NulSym ;
      desLowestType  := GetLowestType(GetType(d)) ;
      exprLowestType := NulSym ;
      isLeftValue    := isLeft ;
      tokenNo        := GetTokenNo()
   END ;
   RETURN( p )
END PutRangePointer ;


(*
   PutRangeNoEval - initializes contents of, p, to a non evaluation
                    runtime check such as a no else clause or
                    no return found in function call.
*)

PROCEDURE PutRangeNoEval (p: Range; t: TypeOfRange) : Range ;
BEGIN
   WITH p^ DO
      type    := t ;
      tokenNo := GetTokenNo()
   END ;
   RETURN( p )
END PutRangeNoEval ;


(*
   InitAssignmentRangeCheck - returns a range check node which
                              remembers the information necessary
                              so that a range check for d := e
                              can be generated later on.
*)

PROCEDURE InitAssignmentRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   p: Range ;
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   p := PutRange(GetIndice(RangeIndex, r), assignment, d, e) ;
   RETURN( r )
END InitAssignmentRangeCheck ;


(*
   InitSubrangeRangeCheck - returns a range check node which
                            remembers the information necessary
                            so that a range check for d := e
                            can be generated later on.
*)

PROCEDURE InitSubrangeRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   p: Range ;
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   p := PutRange(GetIndice(RangeIndex, r), subrangeassignment, d, e) ;
   RETURN( r )
END InitSubrangeRangeCheck ;


(*
   InitStaticArraySubscriptRangeCheck - returns a range check node which
                                        remembers the information necessary
                                        so that a range check for d[e]
                                        can be generated later on.
*)

PROCEDURE InitStaticArraySubscriptRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   p: Range ;
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   p := PutRange(GetIndice(RangeIndex, r), staticarraysubscript, d, e) ;
   RETURN( r )
END InitStaticArraySubscriptRangeCheck ;


(*
   InitDynamicArraySubscriptRangeCheck - returns a range check node which
                                         remembers the information necessary
                                         so that a range check for d[e]
                                         can be generated later on.
*)

PROCEDURE InitDynamicArraySubscriptRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   p: Range ;
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   p := PutRange(GetIndice(RangeIndex, r), dynamicarraysubscript, d, e) ;
   RETURN( r )
END InitDynamicArraySubscriptRangeCheck ;


(*
   InitIncRangeCheck - returns a range check node which
                       remembers the information necessary
                       so that a range check for INC(d, e)
                       can be generated later on.
*)

PROCEDURE InitIncRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   p: Range ;
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   p := PutRange(GetIndice(RangeIndex, r), inc, d, e) ;
   RETURN( r )
END InitIncRangeCheck ;


(*
   InitDecRangeCheck - returns a range check node which
                       remembers the information necessary
                       so that a range check for DEC(d, e)
                       can be generated later on.
*)

PROCEDURE InitDecRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   p: Range ;
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   p := PutRange(GetIndice(RangeIndex, r), inc, d, e) ;
   RETURN( r )
END InitDecRangeCheck ;


(*
   InitForLoopBeginRangeCheck - returns a range check node which
                                remembers the information necessary
                                so that a range check for FOR d := e TO .. DO
                                can be generated later on.
*)

PROCEDURE InitForLoopBeginRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   p: Range ;
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   p := PutRange(GetIndice(RangeIndex, r), forloopbegin, d, e) ;
   RETURN( r )
END InitForLoopBeginRangeCheck ;


(*
   InitForLoopToRangeCheck - returns a range check node which
                             remembers the information necessary
                             so that a range check for the final value
                             implied by ... e1 TO e2 BY e3 DO
                             can be generated later on.
*)

PROCEDURE InitForLoopToRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   p: Range ;
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   p := PutRange(GetIndice(RangeIndex, r), forloopto, d, e) ;
   RETURN( r )
END InitForLoopToRangeCheck ;


(*
   InitForLoopEndRangeCheck - returns a range check node which
                              remembers the information necessary
                              so that a range check for
                              INC or DEC(d, e)
                              can be generated later on.
*)

PROCEDURE InitForLoopEndRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   p: Range ;
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   p := PutRange(GetIndice(RangeIndex, r), forloopend, d, e) ;
   RETURN( r )
END InitForLoopEndRangeCheck ;


(*
   InitPointerRangeCheck - creates a pointer # NIL check.
*)

PROCEDURE InitPointerRangeCheck (d: CARDINAL; isLeft: BOOLEAN) : CARDINAL ;
VAR
   p: Range ;
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   p := PutRangePointer(GetIndice(RangeIndex, r), d, isLeft) ;
   RETURN( r )
END InitPointerRangeCheck ;


(*
   InitNoReturnRangeCheck - creates a check held in the function
                            to detect the absence of a RETURN
                            statement at runtime.
*)

PROCEDURE InitNoReturnRangeCheck () : CARDINAL ;
VAR
   p: Range ;
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   p := PutRangeNoEval(GetIndice(RangeIndex, r), noreturn) ;
   RETURN( r )
END InitNoReturnRangeCheck ;


(*
   InitNoElseRangeCheck - creates a check held at the end of
                          a CASE statement without an ELSE
                          clause to detect its absence
                          at runtime.
*)

PROCEDURE InitNoElseRangeCheck () : CARDINAL ;
VAR
   p: Range ;
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   p := PutRangeNoEval(GetIndice(RangeIndex, r), noelse) ;
   RETURN( r )
END InitNoElseRangeCheck ;


(*
   FoldNil - attempts to fold the pointer against nil comparison.
*)

PROCEDURE FoldNil (tokenno: CARDINAL; l: List; q: CARDINAL; r: CARDINAL) ;
VAR
   p: Range ;
   e: Error ;
   n: Name ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenno, des) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF GccKnowsAbout(des) AND IsConst(des)
      THEN
         PushValue(des) ;
         PushValue(Nil) ;
         IF Equ(tokenno)
         THEN
            e := NewWarning(tokenNo) ;  (* use range tokenNo for warning *)
            n := GetSymName(des) ;
            ErrorFormat1(e, 'attempting to dereference a pointer (%a) whose value will be NIL',
                         n) ;
            PutQuad(q, ErrorOp, NulSym, NulSym, r)
         ELSE
            SubQuad(q)
         END
      END
   END
END FoldNil ;


(*
   FoldNoReturn - folds the no return range check.
*)

PROCEDURE FoldNoReturn (tokenno: CARDINAL; l: List; q: CARDINAL; r: CARDINAL) ;
VAR
   p: Range ;
   e: Error ;
   n: Name ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenno, des) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF GccKnowsAbout(des) AND IsConst(des)
      THEN
         PushValue(des) ;
         PushValue(Nil) ;
         IF Equ(tokenno)
         THEN
            e := NewWarning(tokenNo) ;  (* use range tokenNo for warning *)
            n := GetSymName(des) ;
            ErrorFormat1(e, 'attempting to dereference a pointer (%a) whose value will be NIL',
                         n) ;
            PutQuad(q, ErrorOp, NulSym, NulSym, r)
         ELSE
            SubQuad(q)
         END
      END
   END
END FoldNoReturn ;


(*
   GetMinMax - returns TRUE if we know the max and min of a type, t.
*)

PROCEDURE GetMinMax (t: CARDINAL; VAR min, max: Tree) : BOOLEAN ;
VAR
   minC, maxC: CARDINAL ;
BEGIN
   Assert(IsAModula2Type(t)) ;
   IF GccKnowsAbout(t) AND (NOT IsPointer(t)) AND
      (NOT IsArray(t)) AND (NOT IsRecord(t)) AND
      (NOT IsRecord(t)) AND (NOT IsUnbounded(t)) AND
      (NOT IsProcType(t)) AND (NOT IsRealType(t)) AND
      (t#Address)
   THEN
      IF IsSubrange(t)
      THEN
         GetSubrange(t, maxC, minC) ;
         max := Mod2Gcc(maxC) ;
         min := Mod2Gcc(minC)
      ELSIF IsEnumeration(t)
      THEN
         GetBaseTypeMinMax(t, minC, maxC) ;
         max := Mod2Gcc(maxC) ;
         min := Mod2Gcc(minC)
      ELSE
         max := GetMaxFrom(Mod2Gcc(t)) ;
         min := GetMinFrom(Mod2Gcc(t))
      END ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END GetMinMax ;


(*
   OutOfRange - returns TRUE if expr lies outside min..max.
*)

PROCEDURE OutOfRange (tokenno: CARDINAL; min, expr, max: Tree) : BOOLEAN ;
BEGIN
   PushIntegerTree(expr) ;
   PushIntegerTree(min) ;
   IF Less(tokenno)
   THEN
      RETURN( TRUE )
   END ;
   PushIntegerTree(expr) ;
   PushIntegerTree(max) ;
   IF Gre(tokenno)
   THEN
      RETURN( TRUE )
   END ;
   RETURN( FALSE )
END OutOfRange ;


(*
   HaveHandler - 
*)

PROCEDURE HaveHandler (r: CARDINAL) : BOOLEAN ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      CASE type OF

      assignment           : RETURN( ExceptionAssign#NulSym ) |
      subrangeassignment   : InternalError('not expecting this case value', __FILE__, __LINE__) |
      inc                  : RETURN( ExceptionInc#NulSym ) |
      dec                  : RETURN( ExceptionDec#NulSym ) |
      staticarraysubscript : RETURN( ExceptionStaticArray#NulSym ) |
      dynamicarraysubscript: RETURN( ExceptionDynamicArray#NulSym ) |
      forloopbegin         : RETURN( ExceptionForLoopBegin#NulSym ) |
      forloopto            : RETURN( ExceptionForLoopTo#NulSym ) |
      forloopend           : RETURN( ExceptionForLoopEnd#NulSym ) |
      pointernil           : RETURN( ExceptionPointerNil#NulSym ) |
      noreturn             : RETURN( ExceptionNoReturn#NulSym ) |
      noelse               : RETURN( ExceptionCase#NulSym ) |
      none                 : RETURN( FALSE )

      ELSE
         InternalError('enumeration value unknown', __FILE__, __LINE__)
      END
   END
END HaveHandler ;


(*
   FoldAssignment - 
*)

PROCEDURE FoldAssignment (tokenno: CARDINAL; l: List; q: CARDINAL; r: CARDINAL) ;
VAR
   p       : Range ;
   e       : Error ;
   n1, n2  : Name ;
   min, max: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr) AND
            GetMinMax(desLowestType, min, max)
         THEN
            IF OutOfRange(tokenno, min, Mod2Gcc(expr), max)
            THEN
               e := NewWarning(tokenNo) ;  (* use range tokenNo for warning *)
               n1 := GetSymName(GetType(des)) ;
               n2 := GetSymName(expr) ;
               IF IsConstLit(expr) OR (IsVar(expr) AND (NOT IsTemporary(expr)))
               THEN
                  ErrorFormat2(e, 'attempting to assign a value (%a) to a designator which will exceed the range of type (%a)',
                               n2, n1) ;
               ELSE
                  ErrorFormat1(e, 'attempting to assign a value to a designator which will exceed the range of type (%a)',
                               n1) ;
               END ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSE
               SubQuad(q)
            END
         END
      END
   END
END FoldAssignment ;


(*
   FoldInc - 
*)

PROCEDURE FoldInc (tokenno: CARDINAL; l: List; q: CARDINAL; r: CARDINAL) ;
VAR
   p          : Range ;
   e          : Error ;
   n1, n2     : Name ;
   t, min, max: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      DeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr) AND
            GetMinMax(desLowestType, min, max)
         THEN
            IF OutOfRange(tokenno, min, Mod2Gcc(expr), max)
            THEN
               e := NewWarning(tokenNo) ;  (* use range tokenNo for warning *)
               n1 := GetSymName(GetType(des)) ;
               n2 := GetSymName(expr) ;
               ErrorFormat1(e, 'operand to INC exceeds the range of type (%a)', n1) ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSIF GccKnowsAbout(des) AND IsConst(des) AND GccKnowsAbout(desLowestType)
            THEN
               t := BuildSub(max,
                             BuildConvert(Mod2Gcc(desLowestType), Mod2Gcc(expr), FALSE),
                             FALSE) ;
               PushIntegerTree(Mod2Gcc(des)) ;
               PushIntegerTree(t) ;
               IF Gre(tokenNo)
               THEN
                  e := NewWarning(tokenNo) ;  (* use range tokenNo for warning *)
                  ErrorFormat0(e, 'INC will cause a range error') ;
                  PutQuad(q, ErrorOp, NulSym, NulSym, r)
               ELSE
                  (* range check is unnecessary *)
                  SubQuad(q)
               END
            END
         END
      END
   END
END FoldInc ;


(*
   FoldDec - 
*)

PROCEDURE FoldDec (tokenno: CARDINAL; l: List; q: CARDINAL; r: CARDINAL) ;
VAR
   p          : Range ;
   e          : Error ;
   n1, n2     : Name ;
   t, min, max: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      DeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr) AND
            GetMinMax(desLowestType, min, max)
         THEN
            IF OutOfRange(tokenno, min, Mod2Gcc(expr), max)
            THEN
               e := NewWarning(tokenNo) ;  (* use range tokenNo for warning *)
               n1 := GetSymName(GetType(des)) ;
               n2 := GetSymName(expr) ;
               ErrorFormat1(e, 'operand to DEC exceeds the range of type (%a)', n1) ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSIF GccKnowsAbout(des) AND IsConst(des) AND GccKnowsAbout(desLowestType)
            THEN
               t := BuildSub(BuildConvert(Mod2Gcc(desLowestType), Mod2Gcc(expr), FALSE),
                             min,
                             FALSE) ;
               PushIntegerTree(Mod2Gcc(des)) ;
               PushIntegerTree(t) ;
               IF Less(tokenNo)
               THEN
                  e := NewWarning(tokenNo) ;  (* use range tokenNo for warning *)
                  ErrorFormat0(e, 'DEC will cause a range error') ;
                  PutQuad(q, ErrorOp, NulSym, NulSym, r)
               ELSE
                  (* range check is unnecessary *)
                  SubQuad(q)
               END
            END
         END
      END
   END
END FoldDec ;


(*
   FoldForLoopBegin - 
*)

PROCEDURE FoldForLoopBegin (tokenno: CARDINAL; l: List; q: CARDINAL; r: CARDINAL) ;
VAR
   p       : Range ;
   e       : Error ;
   n1, n2  : Name ;
   min, max: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr) AND
            GetMinMax(desLowestType, min, max)
         THEN
            IF OutOfRange(tokenno, min, Mod2Gcc(expr), max)
            THEN
               e := NewWarning(tokenNo) ;  (* use range tokenNo for warning *)
               n1 := GetSymName(GetType(des)) ;
               n2 := GetSymName(expr) ;
               IF IsConstLit(expr) OR (IsVar(expr) AND (NOT IsTemporary(expr)))
               THEN
                  ErrorFormat2(e, 'attempting to assign a value (%a) to a FOR loop designator which will exceed the range of type (%a)',
                               n2, n1) ;
               ELSE
                  ErrorFormat1(e, 'attempting to assign a value to a FOR loop designator which will exceed the range of type (%a)',
                               n1) ;
               END ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSE
               SubQuad(q)
            END
         END
      END
   END
END FoldForLoopBegin ;


(*
   FoldForLoopTo - 
*)

PROCEDURE FoldForLoopTo (tokenno: CARDINAL; l: List; q: CARDINAL; r: CARDINAL) ;
VAR
   p       : Range ;
   e       : Error ;
   n1, n2  : Name ;
   min, max: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr) AND
            GetMinMax(desLowestType, min, max)
         THEN
            IF OutOfRange(tokenno, min, Mod2Gcc(expr), max)
            THEN
               e := NewWarning(tokenNo) ;  (* use range tokenNo for warning *)
               n1 := GetSymName(GetType(des)) ;
               n2 := GetSymName(expr) ;
               IF IsConstLit(expr) OR (IsVar(expr) AND (NOT IsTemporary(expr)))
               THEN
                  ErrorFormat2(e, 'final value in FOR loop will exceed type range (%a) of designator (%a)',
                               n2, n1) ;
               ELSE
                  ErrorFormat1(e, 'final value in FOR loop iterator will exceed type range of designator (%a)',
                               n1) ;
               END ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSE
               SubQuad(q)
            END
         END
      END
   END
END FoldForLoopTo ;


(*
   FoldStaticArraySubscript - 
*)

PROCEDURE FoldStaticArraySubscript (tokenno: CARDINAL; l: List; q: CARDINAL; r: CARDINAL) ;
VAR
   p          : Range ;
   e          : Error ;
   n1, n2     : Name ;
   t, min, max: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      DeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr) AND
            GetMinMax(desLowestType, min, max)
         THEN
            IF OutOfRange(tokenno, min, Mod2Gcc(expr), max)
            THEN
               e := NewWarning(tokenNo) ;  (* use range tokenNo for warning *)
               n1 := GetSymName(GetType(des)) ;
               n2 := GetSymName(expr) ;
               ErrorFormat0(e, 'index out of range found while attempting to access an element of a static array') ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSE
               (* range check is unnecessary *)
               SubQuad(q)
            END
         END
      END
   END
END FoldStaticArraySubscript ;


(*
   FoldDynamicArraySubscript - 
*)

PROCEDURE FoldDynamicArraySubscript (tokenno: CARDINAL; l: List; q: CARDINAL; r: CARDINAL) ;
VAR
   p          : Range ;
   e          : Error ;
   n1, n2     : Name ;
   t, min, max: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr)
         THEN
            IF IsGreater(GetIntegerZero(), BuildConvert(GetIntegerType(), Mod2Gcc(expr), FALSE))
            THEN
               e := NewWarning(tokenNo) ;  (* use range tokenNo for warning *)
               n1 := GetSymName(GetType(des)) ;
               n2 := GetSymName(expr) ;
               ErrorFormat0(e, 'index out of range found while attempting to access an element of a dynamic array') ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSE
               (* cannot fold high bounds, so leave that for the runtime *)
            END
         END
      END
   END
END FoldDynamicArraySubscript ;


(*
   FoldRangeCheck - attempts to resolve the range check, r.
                    If it evaluates to true then
                       it is replaced by an ErrorOp
                    elsif it evaluates to false then
                       it is removed
                    else
                       it is left alone
*)

PROCEDURE FoldRangeCheck (tokenno: CARDINAL; l: List; q: CARDINAL; r: CARDINAL) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      CASE type OF

      assignment           :  FoldAssignment(tokenno, l, q, r) |
(*      subrangeassignment   :  |  unused currently *)
      inc                  :  FoldInc(tokenno, l, q, r) |
      dec                  :  FoldDec(tokenno, l, q, r) |
      staticarraysubscript :  FoldStaticArraySubscript(tokenno, l, q, r) |
      dynamicarraysubscript:  FoldDynamicArraySubscript(tokenno, l, q, r) |
      forloopbegin         :  FoldForLoopBegin(tokenno, l, q, r) |
      forloopto            :  FoldForLoopTo(tokenno, l, q, r) |
      forloopend           :  RETURN (* unable to fold anything at this point, des, will be variable *) |
      pointernil           :  FoldNil(tokenno, l, q, r) |
      noreturn             :  RETURN (* nothing to fold *) |
      noelse               :  RETURN (* nothing to fold *) |
      none                 :  SubQuad(q)

      ELSE
         InternalError('unexpected case', __FILE__, __LINE__)
      END
   END
END FoldRangeCheck ;


(*
   DeReferenceLValue - returns a Tree which is either ModGcc(expr)
                       or Mod2Gcc( *expr) depending whether, expr,
                       is an LValue.
*)

PROCEDURE DeReferenceLValue (expr: CARDINAL) : Tree ;
VAR
   e: Tree ;
BEGIN
   e := Mod2Gcc(expr) ;
   IF GetMode(expr)=LeftValue
   THEN
      e := BuildIndirect(e, Mod2Gcc(GetType(expr)))
   END ;
   RETURN( e )
END DeReferenceLValue ;


(*
   BuildStringParam - builds a C style string parameter which will be passed
                      as an ADDRESS type.
*)

PROCEDURE BuildStringParam (s: String) ;
BEGIN
   BuildParam(BuildConvert(Mod2Gcc(Address),
                           BuildAddr(BuildStringConstant(string(s), Length(s)),
                                     FALSE), FALSE))
END BuildStringParam ;


(*
   CodeErrorCheck - returns a Tree calling the approprate exception handler.
*)

PROCEDURE CodeErrorCheck (r: CARDINAL; scopeDesc: String) : Tree ;
VAR
   filename: String ;
   line,
   column  : CARDINAL ;
   p       : Range ;
   f       : Tree ;
BEGIN
   IF HaveHandler(r)
   THEN
      p := GetIndice(RangeIndex, r) ;
      WITH p^ DO
         filename := FindFileNameFromToken(tokenNo, 0) ;
         line := TokenToLineNo(tokenNo, 0) ;
         column := TokenToColumnNo(tokenNo, 0) ;
         f := Mod2Gcc(lookupExceptionHandler(type)) ;
         BuildStringParam(scopeDesc) ;
         BuildParam(BuildIntegerConstant(column)) ;
         BuildParam(BuildIntegerConstant(line)) ;
         BuildStringParam(filename) ;
         RETURN( BuildProcedureCallTree(f, NIL) )
      END
   ELSE
      RETURN( NIL )
   END
END CodeErrorCheck ;


(*
   IssueWarning - 
*)

PROCEDURE IssueWarning (scopeDesc: String; r: CARDINAL) ;
VAR
   p: Range ;
   s: String ;
   e: Error ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      CASE type OF

      assignment           : s := InitString('if the assignment is ever executed it will be out of bounds') |
      subrangeassignment   : InternalError('not expecting this case value', __FILE__, __LINE__) |
      inc                  : s := InitString('if the INC is ever executed it will cause an overflow error') |
      dec                  : s := InitString('if the DEC is ever executed it will cause an underflow error') |
      staticarraysubscript : s := InitString('if the static array access is ever made the index will be out of bounds') |
      dynamicarraysubscript: s := InitString('if the dynamic array access is ever made the index will be out of bounds') |
      forloopbegin         : s := InitString('if the assignment in this FOR loop is ever executed it will be out of bounds') |
      forloopto            : s := InitString('the final value in this FOR loop will be out of bounds if ever executed') |
      forloopend           : s := InitString('the increment/decrement at the end of this FOR loop will be out of bounds if ever executed') |
      pointernil           : s := InitString('the pointer value is NIL, if it is ever dereferenced it will cause an exception') |
      noreturn             : s := InitString('this function will exit without executing a RETURN statement') |
      noelse               : s := InitString('this CASE statement does not have an ELSE statement') |
      none                 : InternalError('unexpected value', __FILE__, __LINE__)

      ELSE
         InternalError('enumeration value unknown', __FILE__, __LINE__)
      END ;
      e := NewWarning(tokenNo) ;
      s := ConCat(ConCatChar(scopeDesc, ':'), Mark(s)) ;
      ErrorString(e, s)
   END
END IssueWarning ;


(*
   BuildIfCallHandler - 
*)

PROCEDURE BuildIfCallHandler (condition: Tree; r: CARDINAL;
                              scopeDesc: String; warning: BOOLEAN) : Tree ;
BEGIN
   IF warning AND IsTrue(condition)
   THEN
      IssueWarning(scopeDesc, r)
   END ;
   RETURN( BuildIfThenDoEnd(condition, CodeErrorCheck(r, scopeDesc)) )
END BuildIfCallHandler ;


(*
   DoCodeAssignmentExprType - 
*)

PROCEDURE DoCodeAssignmentExprType (p: Range;
                                    r: CARDINAL; scopeDesc: String;
                                    message: ARRAY OF CHAR) ;
VAR
   condition,
   desMin, desMax,
   exprMin, exprMax: Tree ;
BEGIN
   WITH p^ DO
      IF GccKnowsAbout(desLowestType) AND
         GccKnowsAbout(exprLowestType)
      THEN
         IF GetMinMax(exprLowestType, exprMin, exprMax) AND
            GetMinMax(desLowestType, desMin, desMax)
         THEN
            IF OverlapsRange(desMin, desMax, exprMin, exprMax)
            THEN
               IF IsGreater(desMin, exprMin)
               THEN
                  condition := BuildLessThan(DeReferenceLValue(expr), BuildConvert(Mod2Gcc(exprLowestType), desMin, FALSE)) ;
                  AddStatement(BuildIfCallHandler(condition, r, scopeDesc, TRUE))
               END ;
               IF IsGreater(exprMax, desMax)
               THEN
                  condition := BuildGreaterThan(DeReferenceLValue(expr), BuildConvert(Mod2Gcc(exprLowestType), desMax, FALSE)) ;
                  AddStatement(BuildIfCallHandler(condition, r, scopeDesc, TRUE))
               END
            ELSE
               ErrorFormat0(NewError(tokenNo), message)
            END
         END
      ELSE
         InternalError('should have resolved these types', __FILE__, __LINE__)
      END
   END
END DoCodeAssignmentExprType ;


(*
   DoCodeAssignmentWithoutExprType - 
*)

PROCEDURE DoCodeAssignmentWithoutExprType (p: Range;
                                           r: CARDINAL; scopeDesc: String;
                                           message: ARRAY OF CHAR) ;
VAR
   condition,
   desMin, desMax,
   exprMin, exprMax: Tree ;
BEGIN
   WITH p^ DO
      IF GccKnowsAbout(desLowestType)
      THEN
         IF GetMinMax(desLowestType, desMin, desMax)
         THEN
            condition := BuildLessThan(BuildConvert(Mod2Gcc(desLowestType), DeReferenceLValue(expr), FALSE), desMin) ;
            AddStatement(BuildIfCallHandler(condition, r, scopeDesc, TRUE)) ;
            condition := BuildGreaterThan(BuildConvert(Mod2Gcc(desLowestType), DeReferenceLValue(expr), FALSE), desMax) ;
            AddStatement(BuildIfCallHandler(condition, r, scopeDesc, TRUE))
         END
      ELSE
         InternalError('should have resolved this type', __FILE__, __LINE__)
      END
   END
END DoCodeAssignmentWithoutExprType ;


(*
   DoCodeAssignment - 
*)

PROCEDURE DoCodeAssignment (tokenno: CARDINAL;
                            r: CARDINAL;
                            scopeDesc: String;
                            message: ARRAY OF CHAR) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenNo, des) ;
      DeclareConstant(tokenNo, expr) ;
      IF desLowestType#NulSym
      THEN
         Assert(GccKnowsAbout(expr)) ;
         IF exprLowestType=NulSym
         THEN
            DoCodeAssignmentWithoutExprType(p, r, scopeDesc, message)
         ELSE
            DoCodeAssignmentExprType(p, r, scopeDesc, message)
         END
      END
   END
END DoCodeAssignment ;


(*
   CodeAssignment - 
*)

PROCEDURE CodeAssignment (tokenno: CARDINAL;
                          r: CARDINAL; scopeDesc: String) ;
BEGIN
   DoCodeAssignment(tokenno, r, scopeDesc,
                    'assignment will cause a range error, as the two type ranges do not overlap')
END CodeAssignment ;


(*
   IfOutsideLimitsDo - 
*)

PROCEDURE IfOutsideLimitsDo (min, expr, max: Tree; r: CARDINAL; scopeDesc: String) ;
VAR
   condition: Tree ;
BEGIN
   condition := BuildGreaterThan(min, expr) ;
   AddStatement(BuildIfThenDoEnd(condition, CodeErrorCheck(r, scopeDesc))) ;
   condition := BuildLessThan(max, expr) ;
   AddStatement(BuildIfThenDoEnd(condition, CodeErrorCheck(r, scopeDesc)))
END IfOutsideLimitsDo ;


(*
   CodeInc -
*)

PROCEDURE CodeInc (tokenno: CARDINAL;
                   r: CARDINAL; scopeDesc: String) ;
VAR
   p             : Range ;
   t, condition,
   e,
   desMin, desMax: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenNo, des) ;
      DeclareConstant(tokenNo, expr) ;
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND GccKnowsAbout(desLowestType)
         THEN
            IF GetMinMax(desLowestType, desMin, desMax)
            THEN
               e := BuildConvert(GetTreeType(desMin), DeReferenceLValue(expr), FALSE) ;
               IfOutsideLimitsDo(desMin, e, desMax, r, scopeDesc) ;
               t := BuildSub(desMax,
                             BuildConvert(Mod2Gcc(desLowestType), e, FALSE),
                             FALSE) ;
               condition := BuildGreaterThan(Mod2Gcc(des), t) ;
               AddStatement(BuildIfThenDoEnd(condition, CodeErrorCheck(r, scopeDesc)))
            END
         ELSE
            InternalError('should have resolved these types', __FILE__, __LINE__)
         END
      END
   END
END CodeInc ;


(*
   CodeDec -
*)

PROCEDURE CodeDec (tokenno: CARDINAL;
                   r: CARDINAL; scopeDesc: String) ;
VAR
   p             : Range ;
   t, condition,
   e,
   desMin, desMax: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenNo, des) ;
      DeclareConstant(tokenNo, expr) ;
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND GccKnowsAbout(desLowestType)
         THEN
            IF GetMinMax(desLowestType, desMin, desMax)
            THEN
               e := BuildConvert(GetTreeType(desMin), DeReferenceLValue(expr), FALSE) ;
               IfOutsideLimitsDo(desMin, e, desMax, r, scopeDesc) ;
               t := BuildSub(BuildConvert(Mod2Gcc(desLowestType), e, FALSE),
                             desMin,
                             FALSE) ;
               condition := BuildLessThan(Mod2Gcc(des), t) ;
               AddStatement(BuildIfThenDoEnd(condition, CodeErrorCheck(r, scopeDesc)))
            END
         ELSE
            InternalError('should have resolved these types', __FILE__, __LINE__)
         END
      END
   END
END CodeDec ;


(*
   CodeStaticArraySubscript -
*)

PROCEDURE CodeStaticArraySubscript (tokenno: CARDINAL;
                                    r: CARDINAL; scopeDesc: String) ;
VAR
   p             : Range ;
   desMin, desMax: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenNo, expr) ;
      Assert(IsSubrange(desLowestType)) ;
      IF GccKnowsAbout(expr) AND GccKnowsAbout(desLowestType)
      THEN
         IF GetMinMax(desLowestType, desMin, desMax)
         THEN
            IfOutsideLimitsDo(desMin,
                              BuildConvert(GetTreeType(desMin), DeReferenceLValue(expr), FALSE),
                              desMax, r, scopeDesc)
         ELSE
            InternalError('should have resolved the bounds of the static array', __FILE__, __LINE__)
         END
      ELSE
         InternalError('should have resolved these types', __FILE__, __LINE__)
      END
   END
END CodeStaticArraySubscript ;


(*
   CodeDynamicArraySubscript -
*)

PROCEDURE CodeDynamicArraySubscript (tokenno: CARDINAL;
                                     r: CARDINAL; scopeDesc: String) ;
VAR
   UnboundedType: CARDINAL ;
   p            : Range ;
   condition,
   high, e      : Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenNo, expr) ;
      Assert(IsVar(des)) ;
      IF GccKnowsAbout(expr) AND GccKnowsAbout(des)
      THEN
         UnboundedType := GetType(des) ;
         Assert(IsUnbounded(UnboundedType)) ;
         high := GetHighFromUnbounded(des) ;
         e := BuildConvert(GetIntegerType(), DeReferenceLValue(expr), FALSE) ;
         IfOutsideLimitsDo(GetIntegerZero(), e, high, r, scopeDesc)
      ELSE
         InternalError('should have resolved these types', __FILE__, __LINE__)
      END
   END
END CodeDynamicArraySubscript ;


(*
   CodeForLoopBegin - 
*)

PROCEDURE CodeForLoopBegin (tokenno: CARDINAL;
                            r: CARDINAL; scopeDesc: String) ;
BEGIN
   DoCodeAssignment(tokenno, r, scopeDesc,
                    'the initial assignment of the FOR loop will cause a range error, as the two type ranges do not overlap')
END CodeForLoopBegin ;


(*
   CodeForLoopTo - 
*)

PROCEDURE CodeForLoopTo (tokenno: CARDINAL;
                         r: CARDINAL; scopeDesc: String) ;
BEGIN
   DoCodeAssignment(tokenno, r, scopeDesc,
                    'the final TO value of the FOR loop will cause a range error with the iterator variable')
END CodeForLoopTo ;


(*
   CodeForLoopEnd - 
*)

PROCEDURE CodeForLoopEnd (tokenno: CARDINAL;
                          r: CARDINAL; scopeDesc: String) ;
VAR
   p             : Range ;
   falseStatement,
   trueStatement,
   condition, c,
   min, max, e   : Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      DeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         Assert(GccKnowsAbout(expr)) ;
         IF GetMinMax(desLowestType, min, max) AND
            GccKnowsAbout(desLowestType)
         THEN
            e := BuildConvert(GetIntegerType(), DeReferenceLValue(expr), FALSE) ;
            condition := BuildGreaterThan(e, GetIntegerZero()) ;

            (* check des has room to add, expr, without overflowing *)
            c := BuildGreaterThan(e, BuildSub(max, Mod2Gcc(des), FALSE)) ;
            trueStatement := BuildIfCallHandler(c, r, scopeDesc, IsTrue(condition)) ;

            (* check des has room to subtract, expr, without underflowing *)
            c := BuildLessThan(BuildSub(e, min, FALSE),
                               BuildNegate(e, FALSE)) ;
            falseStatement := BuildIfCallHandler(c, r, scopeDesc, IsFalse(condition)) ;

            AddStatement(BuildIfThenElseEnd(condition, trueStatement, falseStatement))
         END
      END
   END
END CodeForLoopEnd ;


(*
   CodeNil - 
*)

PROCEDURE CodeNil (tokenno: CARDINAL;
                   r: CARDINAL; scopeDesc: String) ;
VAR
   p           : Range ;
   condition, t: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      DeclareConstant(tokenNo, des) ;
(*
      IF GetMode(des)=LeftValue
      THEN
         (* t := BuildIndirect(Mod2Gcc(des), Mod2Gcc(GetType(des))) *)
      ELSE
         t := Mod2Gcc(des)
      END ;
*)
      t := Mod2Gcc(des) ;
      condition := BuildEqualTo(BuildConvert(GetPointerType(), t, FALSE), GetPointerZero()) ;
      AddStatement(BuildIfCallHandler(condition, r, scopeDesc, TRUE))
   END
END CodeNil ;


(*
   CodeRangeCheck - returns a Tree representing the code for a
                    range test defined by, r.
*)

PROCEDURE CodeRangeCheck (r: CARDINAL; scopeDesc: String) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      CASE type OF

      assignment           :  CodeAssignment(tokenNo, r, scopeDesc) |
      subrangeassignment   :  InternalError('unexpected case', __FILE__, __LINE__) |
      inc                  :  CodeInc(tokenNo, r, scopeDesc) |
      dec                  :  CodeDec(tokenNo, r, scopeDesc) |
      staticarraysubscript :  CodeStaticArraySubscript(tokenNo, r, scopeDesc) |
      dynamicarraysubscript:  CodeDynamicArraySubscript(tokenNo, r, scopeDesc) |
      forloopbegin         :  CodeForLoopBegin(tokenNo, r, scopeDesc) |
      forloopto            :  CodeForLoopTo(tokenNo, r, scopeDesc) |
      forloopend           :  CodeForLoopEnd(tokenNo, r, scopeDesc) |
      pointernil           :  CodeNil(tokenNo, r, scopeDesc) |
      noreturn             :  AddStatement(CodeErrorCheck(r, scopeDesc)) |
      noelse               :  AddStatement(CodeErrorCheck(r, scopeDesc)) |
      none                 :

      ELSE
         InternalError('unexpected case', __FILE__, __LINE__)
      END
   END
END CodeRangeCheck ;


(*
   AddVarRead - checks to see whether symbol, Sym, is
                a variable or a parameter and if so it
                then adds this quadruple to the variable
                list.
*)

PROCEDURE AddVarRead (sym: CARDINAL; quadNo: CARDINAL) ;
BEGIN
   IF (sym#NulSym) AND IsVar(sym)
   THEN
      PutReadQuad(sym, quadNo)
   END
END AddVarRead ;


(*
   SubVarRead - checks to see whether symbol, Sym, is
                a variable or a parameter and if so it
                then removes this quadruple from the
                variable list.
*)

PROCEDURE SubVarRead (sym: CARDINAL; quadNo: CARDINAL) ;
BEGIN
   IF (sym#NulSym) AND IsVar(sym)
   THEN
      RemoveReadQuad(sym, quadNo)
   END
END SubVarRead ;


(*
   CheckRangeAddVariableRead - ensures that any references to reading
                               variables used by this range check, r,
                               at this, quadNo, are recorded in the
                               symbol table.
*)

PROCEDURE CheckRangeAddVariableRead (r: CARDINAL; quadNo: CARDINAL) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      AddVarRead(des, quadNo) ;
      AddVarRead(expr, quadNo)
   END
END CheckRangeAddVariableRead ;


(*
   CheckRangeRemoveVariableRead - ensures that any references to reading
                                  variable at this quadNo are removed from
                                  the symbol table.
*)

PROCEDURE CheckRangeRemoveVariableRead (r: CARDINAL; quadNo: CARDINAL) ; 
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      SubVarRead(des, quadNo) ;
      SubVarRead(expr, quadNo)
   END
END CheckRangeRemoveVariableRead ;


(*
   WriteRangeCheck - displays debugging information about range, r.
*)

PROCEDURE WriteRangeCheck (r: CARDINAL) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      CASE type OF

      assignment           :  WriteString('assignment (') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      subrangeassignment   :  WriteString('subrangeassignment(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      inc                  :  WriteString('inc(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      dec                  :  WriteString('dec(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      staticarraysubscript :  WriteString('staticarraysubscript(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      dynamicarraysubscript:  WriteString('dynamicarraysubscript(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      forloopbegin         :  WriteString('forloopbegin(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      forloopto            :  WriteString('forloopto(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      forloopend           :  WriteString('forloopend(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      pointernil           :  WriteString('pointernil(') ; WriteOperand(des) |
      noreturn             :  WriteString('noreturn(') |
      noelse               :  WriteString('noelse(') |
      none                 :  WriteString('none(') |
      
      ELSE
         InternalError('unknown case', __FILE__, __LINE__)
      END ;
      Write(')')
   END
END WriteRangeCheck ;


(*
   Init - initializes the modules global variables.
*)

PROCEDURE Init ;
BEGIN
   TopOfRange := 0 ;
   RangeIndex := InitIndex(1)
END Init ;


BEGIN
   Init
END M2Range.
