(* Copyright (C) 2005 Free Software Foundation, Inc. *)
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
                        IsAModula2Type ;

FROM gccgm2 IMPORT Tree, GetMinFrom, GetMaxFrom, BuildSub,
                   BuildAdd, BuildConvert ;

FROM M2Debug IMPORT Assert ;
FROM Indexing IMPORT Index, InitIndex, InBounds, PutIndice, GetIndice ;
FROM Storage IMPORT ALLOCATE ;
FROM M2ALU IMPORT Equ, PushIntegerTree, Gre, Less ;
FROM M2Error IMPORT Error, InternalError, NewWarning, ErrorFormat0, ErrorFormat1, ErrorFormat2 ;
FROM M2LexBuf IMPORT GetTokenNo ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM M2Quads IMPORT QuadOperator, PutQuad, SubQuad, WriteOperand ;
FROM M2Base IMPORT Nil, IsRealType ;
FROM M2GCCDeclare IMPORT DeclareConstant ;
FROM SymbolConversion IMPORT GccKnowsAbout, Mod2Gcc ;
FROM Lists IMPORT List ;
FROM NameKey IMPORT Name ;
FROM StdIO IMPORT Write ;


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
   TopOfRange: CARDINAL ;
   RangeIndex: Index ;


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
   CodeRangeCheck - returns a Tree representing the code for a
                    range test defined by, r.
*)

PROCEDURE CodeRangeCheck (r: CARDINAL; scopeName: Name) : Tree ;
BEGIN
   (* loads of code goes here --fixme-- *)
   RETURN NIL
END CodeRangeCheck ;


(*
   CodeErrorCheck - returns a Tree calling the approprate exception handler.
*)

PROCEDURE CodeErrorCheck (op3: CARDINAL; scopeName: Name) : Tree ;
BEGIN
   (* loads of code goes here --fixme-- *)
   RETURN NIL
END CodeErrorCheck ;


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
      (NOT IsProcType(t)) AND (NOT IsRealType(t))
   THEN
      IF IsSubrange(t)
      THEN
         GetSubrange(t, maxC, minC) ;
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
      dec                  :  (* FoldDec(tokenno, l, q, r) *) |
      staticarraysubscript :  (* FoldStaticArraySubscript(tokenno, l, q, r) *) |
      dynamicarraysubscript:  (* FoldDynamicArraySubscript(tokenno, l, q, r) *) |
      forloopbegin         :  (* FoldForLoopBegin(tokenno, l, q, r) *) |
      forloopto            :  (* FoldForLoopTo(tokenno, l, q, r) *) |
      forloopend           :  (* FoldForLoopEnd(tokenno, l, q, r) *) |
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
