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
IMPLEMENTATION MODULE M2ALU ;

(*
    Title      : M2ALU.mod
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Mon Jul 10 12:04:50 2000
    Description: gcc implementation of the M2ALU module, this module provides an interface
                 between some of the Modula-2 front end optimization routines and tree
                 construction required so that efficient trees can be passed to gcc's
                 backend. M2ALU allows constant expressions to be calculated.
*)

FROM ASCII IMPORT nul ;
FROM SYSTEM IMPORT WORD ;
FROM NameKey IMPORT KeyToCharStar ;
FROM M2Error IMPORT InternalError, WriteFormat0, ErrorStringAt, FlushErrors ;
FROM M2Debug IMPORT Assert ;
FROM Storage IMPORT ALLOCATE ;
FROM StringConvert IMPORT ostoi, bstoi, stoi, hstoi ;
FROM M2GCCDeclare IMPORT GetTypeMin, GetTypeMax, CompletelyResolved, DeclareConstant ;
FROM M2Bitset IMPORT Bitset ;
FROM SymbolConversion IMPORT Mod2Gcc ;
FROM M2Printf IMPORT printf2 ;
FROM M2Base IMPORT MixTypes ;
FROM DynamicStrings IMPORT InitString ;
FROM M2Constants IMPORT MakeNewConstFromValue ;

FROM SymbolTable IMPORT NulSym, IsEnumeration, IsSubrange, IsValueSolved, PushValue,
                        ForeachFieldEnumerationDo, MakeTemporary, PutVar, PopValue, GetType,
                        IsSet, SkipType,
                        GetSubrange, GetSymName,
                        ModeOfAddr ;

IMPORT DynamicStrings ;

FROM gccgm2 IMPORT Tree, BuildIntegerConstant,
                   CompareTrees, ConvertConstantAndCheck, GetIntegerType, GetLongRealType,
                   GetIntegerOne, GetIntegerZero,
                   GetWordOne, ToWord,
                   AreConstantsEqual, GetBitsPerBitset,
                   BuildAdd, BuildSub, BuildMult, BuildDiv, BuildMod,
                   BuildLSL, BuildLSR,
                   BuildLogicalOr, BuildLogicalAnd, BuildSymmetricDifference,
                   BuildIfIn,
                   BuildStartRecord, BuildFieldRecord, ChainOn, BuildEndRecord,
                   RealToTree, RememberConstant, BuildConstLiteralNumber,
                   BuildStartSetConstructor, BuildSetConstructorElement,
                   BuildEndSetConstructor,
                   FoldAndStrip, TreeOverflow,
                   DebugTree ;



(* %%%FORWARD%%%
PROCEDURE DupConst (tokenno: CARDINAL; sym: CARDINAL; offset: INTEGER) : CARDINAL ; FORWARD ;
PROCEDURE DupConstAndAdd (tokenno: CARDINAL;
                          sym: CARDINAL;
                          extra: Tree) : CARDINAL ; FORWARD ;
PROCEDURE DupConstAndAddMod (tokenno: CARDINAL;
                             sym: CARDINAL; extra: Tree;
                             l, h: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE Eval (tokenno: CARDINAL; v: PtrToValue) ; FORWARD ;
PROCEDURE Push (v: PtrToValue) ; FORWARD ;
PROCEDURE Pop () : PtrToValue ; FORWARD ;
PROCEDURE RealSub (Op1, Op2: PtrToValue) ; FORWARD ;
PROCEDURE RealAdd (Op1, Op2: PtrToValue) ; FORWARD ;
PROCEDURE RealMult (Op1, Op2: PtrToValue) ; FORWARD ;
PROCEDURE RealDiv (Op1, Op2: PtrToValue) ; FORWARD ;
PROCEDURE RealMod (Op1, Op2: PtrToValue) ; FORWARD ;
PROCEDURE BuildBitset (tokenno: CARDINAL; v: PtrToValue; low, high: Tree) : Tree ; FORWARD ;
PROCEDURE IsSuperset (tokenno: CARDINAL; s1, s2: PtrToValue) : BOOLEAN ; FORWARD ;
PROCEDURE IsSubset (tokenno: CARDINAL; s1, s2: PtrToValue) : BOOLEAN ; FORWARD ;
PROCEDURE Val (tokenno: CARDINAL; type: CARDINAL; value: Tree) : CARDINAL ; FORWARD ;
   %%%FORWARD%%% *)

CONST
   Debugging    = FALSE ;
   DebugGarbage = TRUE ;

TYPE
   cellType   = (none, integer, real, set) ;

   listOfRange = POINTER TO rList ;
   rList       = RECORD
                    low, high: CARDINAL ;  (* symbol table *)
                    next     : listOfRange ;
                 END ;

   PtrToValue = POINTER TO cell ;
   cell       = RECORD
                   solved : BOOLEAN ;
                   setType: CARDINAL ;
                   next   : PtrToValue ;

                   CASE type: cellType OF

                   none,
                   integer, real: numberValue: Tree |
                   set          : setValue   : listOfRange

                   END
                END ;

   DoSetProcedure = PROCEDURE (CARDINAL, listOfRange, listOfRange) : listOfRange ;

(* %%%FORWARD%%%
PROCEDURE SortElements (tokenno: CARDINAL; h: listOfRange) ; FORWARD ;
PROCEDURE CombineElements (tokenno: CARDINAL; r: listOfRange) ; FORWARD ;
   %%%FORWARD%%% *)

VAR
   RangeFreeList   : listOfRange ;
   FreeList,
   TopOfStack      : PtrToValue ;
   EnumerationValue: Tree ;
   EnumerationField: CARDINAL ;
   CurrentTokenNo  : CARDINAL ;


(*
   New - allocate a PtrToValue. Firstly check the FreeList, if empty call upon New.
*)

PROCEDURE New () : PtrToValue ;
VAR
   v: PtrToValue ;
BEGIN
   IF FreeList=NIL
   THEN
      NEW(v)
   ELSE
      v := FreeList ;
      FreeList := FreeList^.next
   END ;
   RETURN( v )
END New ;


(*
   DisposeRange - adds the list, v, to the free list.
*)

PROCEDURE DisposeRange (VAR v: listOfRange) ;
VAR
   r: listOfRange ;
BEGIN
   r := RangeFreeList ;
   WHILE (r#NIL) AND (r^.next#NIL) DO
      r := r^.next
   END ;
   IF r#NIL
   THEN
      r^.next := RangeFreeList
   END ;
   RangeFreeList := v ;
   v := NIL
END DisposeRange ;

PROCEDURE stop ; BEGIN END stop ;


(*
   CheckNotAlreadyOnFreeList - checks to see whether, v, is already on the free list
                               and aborts if this is the case.
*)

PROCEDURE CheckNotAlreadyOnFreeList (v: PtrToValue) ;
VAR
   l: PtrToValue ;
BEGIN
   IF DebugGarbage
   THEN
      l := FreeList ;
      WHILE l#NIL DO
         IF l=v
         THEN
            InternalError('value is already on the free list', __FILE__, __LINE__)
         END ;
         l := l^.next
      END
   END
END CheckNotAlreadyOnFreeList ;


(*
   CheckNotOnStack - checks to see whether, v, is already on the stack
                     and aborts if this is the case.
*)

PROCEDURE CheckNotOnStack (v: PtrToValue) ;
VAR
   l: PtrToValue ;
BEGIN
   IF DebugGarbage
   THEN
      l := TopOfStack ;
      WHILE l#NIL DO
         IF l=v
         THEN
            InternalError('value is already on the stack', __FILE__, __LINE__)
         END ;
         l := l^.next
      END
   END
END CheckNotOnStack ;


(*
   Dispose - place, v, onto the FreeList.
*)

PROCEDURE Dispose (v: PtrToValue) ;
BEGIN
   CheckNotAlreadyOnFreeList(v) ;
   CheckNotOnStack(v) ;
   IF v^.type=set
   THEN
      DisposeRange(v^.setValue)
   END ;
   v^.next := FreeList ;
   FreeList := v
END Dispose ;


(*
   AddRange - returns a ListOfRange which is prepended to the front of the current list.
*)

PROCEDURE AddRange (head: listOfRange; l, h: CARDINAL) : listOfRange ;
VAR
   r: listOfRange ;
BEGIN
   NEW(r) ;
   IF r=NIL
   THEN
      InternalError('out of memory', __FILE__, __LINE__)
   END ;
   WITH r^ DO
      low  := l ;
      high := h ;
      next := head
   END ;
   RETURN( r )
END AddRange ;


(*
   DupRange - duplicates and returns the list, t.
*)

PROCEDURE DupRange (r: listOfRange) : listOfRange ;
VAR
   s: listOfRange ;
BEGIN
   s := NIL ;
   WHILE r#NIL DO
      s := AddRange(s, r^.low, r^.high) ;
      r := r^.next
   END ;
   RETURN( s )
END DupRange ;


(*
   InitValue - initializes and returns a memory cell.
*)

PROCEDURE InitValue () : PtrToValue ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   IF v=NIL
   THEN
      InternalError('out of memory error', __FILE__, __LINE__)
   ELSE
      WITH v^ DO
         type    := none ;
         solved  := FALSE ;
         next    := NIL ;
         setType := NulSym
      END ;
      RETURN( v )
   END
END InitValue ;


(*
   IsValueTypeNone - returns TRUE if the value on the top stack has no value.
*)

PROCEDURE IsValueTypeNone () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=none
      THEN
         Push(v) ;
         RETURN( TRUE )
      ELSE
         Push(v) ;
         RETURN( FALSE )
      END
   END
END IsValueTypeNone ;


(*
   IsValueTypeInteger - returns TRUE if the value on the top stack is an integer.
*)

PROCEDURE IsValueTypeInteger () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=integer
      THEN
         Push(v) ;
         RETURN( TRUE )
      ELSE
         Push(v) ;
         RETURN( FALSE )
      END
   END
END IsValueTypeInteger ;


(*
   IsValueTypeReal - returns TRUE if the value on the top stack is a real.
*)

PROCEDURE IsValueTypeReal () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=real
      THEN
         Push(v) ;
         RETURN( TRUE )
      ELSE
         Push(v) ;
         RETURN( FALSE )
      END
   END
END IsValueTypeReal ;


(*
   IsValueTypeSet - returns TRUE if the value on the top stack is a set.
*)

PROCEDURE IsValueTypeSet () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=set
      THEN
         Push(v) ;
         RETURN( TRUE )
      ELSE
         Push(v) ;
         RETURN( FALSE )
      END
   END
END IsValueTypeSet ;


(*
   GetSetValueType - returns the set type on top of the ALU stack.
*)

PROCEDURE GetSetValueType () : CARDINAL ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   Push(v) ;
   WITH v^ DO
      IF type=set
      THEN
         RETURN( setType )
      ELSE
         InternalError('expecting set type', __FILE__, __LINE__)
      END
   END
END GetSetValueType ;


(*
   PushIntegerTree - pushes a gcc tree value onto the ALU stack.
*)

PROCEDURE PushIntegerTree (t: Tree) ;
VAR
   v: PtrToValue ;
BEGIN
   v := InitValue() ;
   WITH v^ DO
      type        := integer ;
      numberValue := t ;
      solved      := TRUE
   END ;
   Push(v)
END PushIntegerTree ;


(*
   PopIntegerTree - pops a gcc tree value from the ALU stack.
*)

PROCEDURE PopIntegerTree () : Tree ;
VAR
   v: PtrToValue ;
   t: Tree ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=integer
      THEN
         t := numberValue
      ELSE
         InternalError('expecting type of constant to be a whole number', __FILE__, __LINE__)
      END
   END ;
   Dispose(v) ;
   RETURN( t )
END PopIntegerTree ;


(*
   PushRealTree - pushes a gcc tree value onto the ALU stack.
*)

PROCEDURE PushRealTree (t: Tree) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      type        := real ;
      numberValue := t ;
      solved      := TRUE
   END ;
   Push(v)
END PushRealTree ;


(*
   PopRealTree - pops a gcc tree value from the ALU stack.
*)

PROCEDURE PopRealTree () : Tree ;
VAR
   v: PtrToValue ;
   t: Tree ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=real
      THEN
         t := numberValue
      ELSE
         InternalError('expecting type of constant to be a real number', __FILE__, __LINE__)
      END
   END ;
   Dispose(v) ;
   RETURN( t )
END PopRealTree ;


(*
   PushSetTree - pushes a gcc tree onto the ALU stack.
                 The tree, t, is expected to contain a
                 word value. It is converted into a set
                 type (sym). Bit 0 maps onto MIN(sym).
*)

PROCEDURE PushSetTree (tokenno: CARDINAL;
                       t: Tree; sym: CARDINAL) ;
VAR
   v: PtrToValue ;
   c,
   i: CARDINAL ;
   r: listOfRange ;
BEGIN
   r := NIL ;
   i := 0 ;
   WHILE (i<GetBitsPerBitset()) AND
         (CompareTrees(GetIntegerZero(), t)#0) DO
      IF CompareTrees(GetIntegerOne(),
                      BuildLogicalAnd(t, GetIntegerOne(), FALSE))=0
      THEN
         PushCard(i) ;
         c := Val(tokenno, SkipType(sym), PopIntegerTree()) ;
         DeclareConstant(tokenno, c) ;
         r := AddRange(r, c, c)
      END ;
      t := BuildLSR(t, GetIntegerOne(), FALSE) ;
      INC(i)
   END ;
   SortElements(tokenno, r) ;
   CombineElements(tokenno, r) ;
   v := New() ;
   WITH v^ DO
      type := set ;
      setType := sym ;
      setValue := r
   END ;
   Eval(tokenno, v) ;
   Push(v)
END PushSetTree ;


(*
   PopSetTree - pops a gcc tree from the ALU stack.
*)

PROCEDURE PopSetTree (tokenno: CARDINAL) : Tree ;
VAR
   v: PtrToValue ;
   t: Tree ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=set
      THEN
         Eval(tokenno, v) ;
         IF NOT v^.solved
         THEN
            InternalError('the set has not been resolved', __FILE__, __LINE__)
         END ;
         t := ConstructSetConstant(tokenno, v)
      ELSE
         InternalError('expecting type of constant to be a set', __FILE__, __LINE__)
      END
   END ;
   Dispose(v) ;
   RETURN( t )
END PopSetTree ;


(*
   Pop - pops and returns top element from the stack.
*)

PROCEDURE Pop () : PtrToValue ;
VAR
   v: PtrToValue ;
BEGIN
   IF TopOfStack=NIL
   THEN
      InternalError('stack underflow error', __FILE__, __LINE__)
   ELSE
      v          := TopOfStack ;
      TopOfStack := TopOfStack^.next
   END ;
   CheckNotAlreadyOnFreeList(v) ;
   RETURN( v )
END Pop ;


(*
   Push - pushes the value onto the stack.
*)

PROCEDURE Push (v: PtrToValue) ;
BEGIN
   CheckNotAlreadyOnFreeList(v) ;
   CheckNotOnStack(v) ;
   v^.next    := TopOfStack ;
   TopOfStack := v
END Push ;


(*
   PrintValue - debugging procedure to display the value on the top of the stack.
*)

PROCEDURE PrintValue ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=integer
      THEN
         DebugTree(numberValue)
      END
   END ;
   Push(v)
END PrintValue ;


(*
   PushFrom - pushes a copy of the contents of, v, onto stack.
*)

PROCEDURE PushFrom (v: PtrToValue) ;
VAR
   t: PtrToValue ;
BEGIN
   CheckNotAlreadyOnFreeList(v) ;
   t := New() ;     (* as it is a copy *)
   t^ := v^ ;
   IF v^.type=set
   THEN
      t^.setValue := DupRange(v^.setValue)
   END ;
   Push(t)
END PushFrom ;


(*
   PopInto - pops the top element from the stack and places it into, v.
*)

PROCEDURE PopInto (v: PtrToValue) ;
VAR
   t: PtrToValue ;
BEGIN
   t := Pop() ;
   v^ := t^ ;
   IF v^.type=set
   THEN
      t^.setValue := NIL
   ELSE
      v^.numberValue := RememberConstant(FoldAndStrip(t^.numberValue))
   END ;
   Dispose(t)
END PopInto ;


(*
   PushCard - pushes a cardinal onto the stack.
*)
 
PROCEDURE PushCard (c: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      type        := integer ;
      numberValue := BuildIntegerConstant(INTEGER(c)) ;
      solved      := TRUE
   END ;
   Push(v)
END PushCard ;

 
(*
   PushInt - pushes an integer onto the stack.
*)

PROCEDURE PushInt (i: INTEGER) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      type        := integer ;
      numberValue := BuildIntegerConstant(i) ;
      solved      := TRUE
   END ;
   Push(v)
END PushInt ;

 
(*
   PushChar - pushes a char onto the stack.
*)
 
PROCEDURE PushChar (c: CHAR) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      type        := integer ;
      numberValue := BuildIntegerConstant(ORD(c)) ;
      solved      := TRUE
   END ;
   Push(v)
END PushChar ;
 
 
(*
   IsReal - returns TRUE if a is a REAL number.
*)

PROCEDURE IsReal (a: DynamicStrings.String) : BOOLEAN ;
BEGIN
   RETURN( DynamicStrings.Index(a, '.', 0)#-1 )
END IsReal ;


(*
   PushString - pushes the numerical value of the string onto the stack.
*)
 
PROCEDURE PushString (s: Name) ;
VAR
   ch    : CHAR ;
   a, b  : DynamicStrings.String ;
   length: CARDINAL ;
BEGIN
   a := DynamicStrings.InitStringCharStar(KeyToCharStar(s)) ;
   b := NIL ;
   length := DynamicStrings.Length(a) ;
   IF length>0
   THEN
      DEC(length) ;
      ch := DynamicStrings.char(a, length) ;
      CASE ch OF

      'H': (* hexadecimal *)
           b := DynamicStrings.Slice(a, 0, -1) ;
           PushIntegerTree(BuildConstLiteralNumber(DynamicStrings.string(b),
                                                   16)) |
      'A': (* binary *)
           b := DynamicStrings.Slice(a, 0, -1) ;
           PushIntegerTree(BuildConstLiteralNumber(DynamicStrings.string(b),
                                                   2)) |
      'C', (* --fixme-- question:
              should we type this as a char rather than an int? *)
      'B': (* octal *)
           b := DynamicStrings.Slice(a, 0, -1) ;
           PushIntegerTree(BuildConstLiteralNumber(DynamicStrings.string(b),
                                                   8))

      ELSE
         IF IsReal(a)
         THEN
            PushRealTree(RealToTree(KeyToCharStar(s)))
         ELSE
            PushIntegerTree(BuildConstLiteralNumber(KeyToCharStar(s), 10))
         END
      END
   ELSE
      InternalError('expecting constant literal', __FILE__, __LINE__)
   END ;
   a := DynamicStrings.KillString(a) ;
   b := DynamicStrings.KillString(b)
END PushString ;


(*
   CoerseLongRealToCard - performs a coersion between a REAL to a CARDINAL
*)

PROCEDURE CoerseLongRealToCard ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=real
      THEN
         numberValue := ConvertConstantAndCheck(GetIntegerType(), numberValue) ;
         type        := integer ;
         solved      := TRUE
      ELSE
         InternalError('expecting a REAL number', __FILE__, __LINE__)
      END
   END ;
   Push(v)
END CoerseLongRealToCard ;


(*
   ConvertRealToInt - converts a REAL into an INTEGER
*)

PROCEDURE ConvertRealToInt ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=real
      THEN
         numberValue := ConvertConstantAndCheck(GetIntegerType(), numberValue) ;
         type        := integer ;
         solved      := TRUE
      ELSE
         InternalError('expecting a REAL number', __FILE__, __LINE__)
      END
   END ;
   Push(v)
END ConvertRealToInt ;


(*
   ConvertIntToReal - converts a INTEGER into a LONGREAL
*)

PROCEDURE ConvertIntToReal ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=integer
      THEN
         numberValue := ConvertConstantAndCheck(GetLongRealType(), numberValue) ;
         type        := real ;
         solved      := TRUE
      ELSE
         InternalError('expecting an INTEGER number', __FILE__, __LINE__)
      END
   END ;
   Push(v)
END ConvertIntToReal ;


(*
   ConvertToInt - converts the value into an INTEGER. This should be used
                  if we are computing the number of elements in a char set to
                  avoid an overflow.
*)

PROCEDURE ConvertToInt ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=integer
      THEN
         numberValue := ConvertConstantAndCheck(GetIntegerType(), numberValue) ;
         solved      := TRUE
      ELSE
         InternalError('expecting an INTEGER number', __FILE__, __LINE__)
      END
   END ;
   Push(v)
END ConvertToInt ;


(*
   IsSolved - returns true if the memory cell indicated by v
              has a set value.
*)
 
PROCEDURE IsSolved (v: PtrToValue) : BOOLEAN ;
BEGIN
   IF v=NIL
   THEN
      InternalError('uninitialized value', __FILE__, __LINE__)
   ELSE
      RETURN( v^.solved )
   END
END IsSolved ;


(*
   EitherReal - returns TRUE if either, Op1, or, Op2, are Real.
*)

PROCEDURE EitherReal (Op1, Op2: PtrToValue) : BOOLEAN ;
BEGIN
   RETURN( (Op1^.type=real) OR (Op2^.type=real) )
END EitherReal ;


(*
   Add - adds the top two elements on the stack.
 
         The Stack:
 
         Entry             Exit
    
  Ptr ->
         +------------+
         | Op1        |                   <- Ptr
         |------------|    +------------+
         | Op2        |    | Op2 + Op1  |
         |------------|    |------------|
*)
 
PROCEDURE Addn ;
VAR
   Temp,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF EitherReal(Op1, Op2)
   THEN
      RealAdd(Op1, Op2)
   ELSE
      Temp := New() ;    (* as it is a temp *)
      WITH Temp^ DO
         type   := integer ;
         numberValue  := BuildAdd(Op1^.numberValue, Op2^.numberValue, FALSE) ;
         solved       := TRUE
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END Addn ;


(*
   RealAdd - adds two numbers. One of which is a Real.
*)

PROCEDURE RealAdd (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF Op1^.type=integer
   THEN
      Push(Op1) ;
      ConvertIntToReal ;
      Op1 := Pop()
   END ;
   IF Op2^.type=integer
   THEN
      Push(Op2) ;
      ConvertIntToReal ;
      Op2 := Pop()
   END ;
   Temp := New() ;
   WITH Temp^ DO
      numberValue := BuildAdd(Op1^.numberValue, Op2^.numberValue, FALSE) ;
      type        := real ;
      solved      := TRUE
   END ;
   Push(Temp)
END RealAdd ;


(*
   Sub - subtracts the top two elements on the stack.
 
         The Stack:
 
         Entry             Exit
 
  Ptr ->
         +------------+
         | Op1        |                   <- Ptr
         |------------|    +------------+
         | Op2        |    | Op2 - Op1  |
         |------------|    |------------|
*)
 
PROCEDURE Sub ;
VAR
   Temp,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF EitherReal(Op1, Op2)
   THEN
      RealSub(Op1, Op2)
   ELSE
      Temp := New() ;      (* as it is a temp *)
      WITH Temp^ DO
         type        := integer ;
         numberValue := BuildSub(Op2^.numberValue, Op1^.numberValue, TRUE) ;
         solved      := TRUE
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END Sub ;


(*
   RealSub - subtracts two numbers. One of which is a Real.
*)

PROCEDURE RealSub (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF Op1^.type=integer
   THEN
      Push(Op1) ;
      ConvertIntToReal ;
      Op1 := Pop()
   END ;
   IF Op2^.type=integer
   THEN
      Push(Op2) ;
      ConvertIntToReal ;
      Op2 := Pop()
   END ;
   Temp := New() ;
   WITH Temp^ DO
      numberValue := BuildSub(Op2^.numberValue, Op1^.numberValue, FALSE) ;
      type        := real ;
      solved      := TRUE
   END ;
   Push(Temp)
END RealSub ;


(*
   Mult - multiplies the top two elements on the stack.
 
          The Stack:
 
          Entry             Exit
 
   Ptr ->
          +------------+
          | Op1        |                   <- Ptr
          |------------|    +------------+
          | Op2        |    | Op2 * Op1  |
          |------------|    |------------|
*)

PROCEDURE Multn ;
VAR
   Temp,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF EitherReal(Op1, Op2)
   THEN
      RealMult(Op1, Op2)
   ELSE
      Temp := New() ;     (* as it is a temp *)
      WITH Temp^ DO
         type        := integer ;
         numberValue := BuildMult(Op2^.numberValue, Op1^.numberValue, FALSE) ;
         solved      := TRUE
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END Multn ;


(*
   RealMult - multiplies two numbers. One of which is a Real.
*)

PROCEDURE RealMult (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF Op1^.type=integer
   THEN
      Push(Op1) ;
      ConvertIntToReal ;
      Op1 := Pop()
   END ;
   IF Op2^.type=integer
   THEN
      Push(Op2) ;
      ConvertIntToReal ;
      Op2 := Pop()
   END ;
   Temp := New() ;     (* as it is a temp *)
   WITH Temp^ DO
      numberValue := BuildMult(Op2^.numberValue, Op1^.numberValue, FALSE) ;
      type        := real ;
      solved      := TRUE
   END ;
   Push(Temp)
END RealMult ;


(*
   Div - divides the top two elements on the stack.

         The Stack:

         Entry             Exit

  Ptr ->
         +------------+
         | Op1        |                     <- Ptr
         |------------|    +--------------+
         | Op2        |    | Op2 DIV Op1  |
         |------------|    |--------------|
*)

PROCEDURE Div ;
VAR
   Temp,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF EitherReal(Op1, Op2)
   THEN
      RealDiv(Op1, Op2)
   ELSE
      Temp := New() ;     (* as it is a temp *)
      WITH Temp^ DO
         type  := integer ;
         numberValue := BuildDiv(Op2^.numberValue, Op1^.numberValue, FALSE) ;
         solved      := TRUE
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END Div ;


(*
   RealDiv - divides two numbers. One of which is a Real.
*)

PROCEDURE RealDiv (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF Op1^.type=integer
   THEN
      Push(Op1) ;
      ConvertIntToReal ;
      Op1 := Pop()
   END ;
   IF Op2^.type=integer
   THEN
      Push(Op2) ;
      ConvertIntToReal ;
      Op2 := Pop()
   END ;
   Temp := New() ;     (* as it is a temp *)
   WITH Temp^ DO
      numberValue := BuildDiv(Op2^.numberValue, Op1^.numberValue, FALSE) ;
      type        := real ;
      solved      := TRUE
   END ;
   Push(Temp)
END RealDiv ;


(*
   Mod - modulus of the top two elements on the stack.

         The Stack:

         Entry             Exit

  Ptr ->
         +------------+
         | Op1        |                     <- Ptr
         |------------|    +--------------+
         | Op2        |    | Op2 MOD Op1  |
         |------------|    |--------------|
*)

PROCEDURE Mod ;
VAR
   Temp,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF EitherReal(Op1, Op2)
   THEN
      WriteFormat0('cannot perform MOD on REALs')
   ELSE
      Temp := New() ;     (* as it is a temp *)
      WITH Temp^ DO
         type        := integer ;
         numberValue := BuildMod(Op2^.numberValue, Op1^.numberValue, FALSE) ;
         solved      := TRUE
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END Mod ;


(*
   AreSetsEqual - returns TRUE if sets, op1, and, op2, contain the same
                  members.
*)

PROCEDURE AreSetsEqual (tokenno: CARDINAL; op1, op2: PtrToValue) : BOOLEAN ;
VAR
   low1, low2,
   high1, high2: CARDINAL ;
   i           : CARDINAL ;
BEGIN
   i := 1 ;
   Eval(tokenno, op1) ;
   Eval(tokenno, op2) ;
   IF NOT (op1^.solved AND op2^.solved)
   THEN
      InternalError('can only compare set values when they are known', __FILE__, __LINE__)
   END ;
   LOOP
      IF GetRange(op1, i, low1, high1)
      THEN
         IF GetRange(op2, i, low2, high2)
         THEN
            PushValue(low1) ;
            PushValue(low2) ;
            IF NotEqu(tokenno)
            THEN
               RETURN( FALSE )
            END ;
            PushValue(high1) ;
            PushValue(high2) ;
            IF NotEqu(tokenno)
            THEN
               RETURN( FALSE )
            END ;
            INC(i)
         ELSE
            (* op2 is out of ranges, but op1 still has >= 1 range left *)
            RETURN( FALSE )
         END
      ELSE
         IF GetRange(op2, i, low2, high2)
         THEN
            (* op1 is out of ranges, but op2 still has >= 1 range left *)
            RETURN( FALSE )
         ELSE
            (* both out of ranges and they were the same *)
            RETURN( TRUE )
         END
      END
   END
END AreSetsEqual ;


(*
   Equ - returns true if the top two elements on the stack
         are identical.

         The Stack:

         Entry             Exit

  Ptr ->
         +------------+
         | Op1        |  
         |------------|
         | Op2        |  
         |------------|    Empty
 
         RETURN( Op2 = Op1 )
*)
 
PROCEDURE Equ (tokenno: CARDINAL) : BOOLEAN ;
VAR
   Op1, Op2: PtrToValue ;
   result  : BOOLEAN ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF (Op1^.type=set) AND (Op2^.type=set)
   THEN
      result := AreSetsEqual(tokenno, Op1, Op2)
   ELSIF (Op1^.type=set) OR (Op2^.type=set)
   THEN
      ErrorStringAt(InitString('cannot perform a comparison between a number and a set'), tokenno) ;
      result := FALSE
   ELSE
      result := AreConstantsEqual(Op1^.numberValue, Op2^.numberValue)
   END ;
   Dispose(Op1) ;
   Dispose(Op2) ;
   RETURN( result )
END Equ ;


(*
   NotEqu - returns true if the top two elements on the stack
            are not identical.
 
            The Stack:
 
            Entry             Exit
 
     Ptr ->
            +------------+
            | Op1        |
            |------------|
            | Op2        |
            |------------|    Empty
 
            RETURN( Op2 # Op1 )
*)

PROCEDURE NotEqu (tokenno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( NOT Equ(tokenno) )
END NotEqu ;


(*
   Less - returns true if Op2 < Op1
 
          The Stack:
 
          Entry             Exit
 
   Ptr ->
          +------------+
          | Op1        | 
          |------------|
          | Op2        | 
          |------------|    Empty
 
          RETURN( Op2 < Op1 )
*)
 
PROCEDURE Less (tokenno: CARDINAL) : BOOLEAN ;
VAR
   v1, v2: PtrToValue ;
   result: BOOLEAN ;
   res   : INTEGER ;
BEGIN
   v1 := Pop() ;
   v2 := Pop() ;
   IF (v1^.type=set) AND (v2^.type=set)
   THEN
      result := NOT IsSuperset(tokenno, v2, v1)
   ELSIF (v1^.type=set) OR (v2^.type=set)
   THEN
      ErrorStringAt(InitString('cannot perform a comparison between a number and a set'), tokenno) ;
      result := FALSE
   ELSE
      res := CompareTrees(v2^.numberValue, v1^.numberValue) ;
      IF res=-1
      THEN
         result := TRUE
      ELSE
         result := FALSE
      END ;
      (* result := (CompareTrees(v2^.numberValue, v1^.numberValue)=-1) *)
   END ;
   Dispose(v1) ;
   Dispose(v2) ;
   RETURN( result )
END Less ;


(*
   Gre - returns true if Op2 > Op1

         The Stack:

         Entry             Exit

  Ptr ->
         +------------+
         | Op1        |  
         |------------|
         | Op2        |  
         |------------|    Empty

         RETURN( Op2 > Op1 )
*)

PROCEDURE Gre (tokenno: CARDINAL) : BOOLEAN ;
VAR
   v1, v2: PtrToValue ;
   result: BOOLEAN ;
BEGIN
   v1 := Pop() ;
   v2 := Pop() ;
   IF (v1^.type=set) AND (v2^.type=set)
   THEN
      result := NOT IsSubset(tokenno, v2, v1)
   ELSIF (v1^.type=set) OR (v2^.type=set)
   THEN
      ErrorStringAt(InitString('cannot perform a comparison between a number and a set'), tokenno) ;
      result := FALSE
   ELSE
      result := (CompareTrees(v2^.numberValue, v1^.numberValue)=1)
   END ;
   Dispose(v1) ;
   Dispose(v2) ;
   RETURN( result )
END Gre ;


(*
   IsSubset - returns TRUE if the set as defined by, s1, is a subset of set, s2.
*)

PROCEDURE IsSubset (tokenno: CARDINAL; s1, s2: PtrToValue) : BOOLEAN ;
BEGIN
   Push(s1) ;
   Push(s2) ;
   SetAnd(tokenno) ;
   Push(s1) ;
   RETURN( Equ(tokenno) )
END IsSubset ;


(*
   LessEqu - returns true if Op2<Op1

             The Stack:

             Entry             Exit

      Ptr ->
             +------------+
             | Op1        |
             |------------|
             | Op2        |
             |------------|    Empty

             RETURN( Op2 <= Op1 )
*)

PROCEDURE LessEqu (tokenno: CARDINAL) : BOOLEAN ;
VAR
   v1, v2: PtrToValue ;
   result: BOOLEAN ;
BEGIN
   v1 := Pop() ;
   v2 := Pop() ;
   IF (v1^.type=set) AND (v2^.type=set)
   THEN
      result := IsSubset(tokenno, v2, v1)
   ELSIF (v1^.type=set) OR (v2^.type=set)
   THEN
      ErrorStringAt(InitString('cannot perform a comparison between a number and a set'), tokenno) ;
      result := FALSE
   ELSE
      result := (CompareTrees(v2^.numberValue, v1^.numberValue)<=0)
   END ;
   Dispose(v1) ;
   Dispose(v2) ;
   RETURN( result )
END LessEqu ;


(*
   IsSuperset - returns TRUE if the set as defined by, s1, is a superset of set, s2.
*)

PROCEDURE IsSuperset (tokenno: CARDINAL; s1, s2: PtrToValue) : BOOLEAN ;
BEGIN
   PushFrom(s1) ;
   PushFrom(s2) ;
   SetAnd(tokenno) ;
   PushFrom(s2) ;
   RETURN( Equ(tokenno) )
END IsSuperset ;


(*
   GreEqu - returns true if Op2 >= Op1
            are not identical.
 
            The Stack:
 
            Entry             Exit
 
     Ptr ->
            +------------+
            | Op1        |
            |------------|
            | Op2        |
            |------------|    Empty
 
            RETURN( Op2 >= Op1 )
*)

PROCEDURE GreEqu (tokenno: CARDINAL) : BOOLEAN ;
VAR
   v1, v2: PtrToValue ;
   result: BOOLEAN ;
BEGIN
   v1 := Pop() ;
   v2 := Pop() ;
   IF (v1^.type=set) AND (v2^.type=set)
   THEN
      result := IsSuperset(tokenno, v2, v1)
   ELSIF (v1^.type=set) OR (v2^.type=set)
   THEN
      ErrorStringAt(InitString('cannot perform a comparison between a number and a set'), tokenno) ;
      result := FALSE
   ELSE
      result := (CompareTrees(v2^.numberValue, v1^.numberValue)>=0)
   END ;
   Dispose(v1) ;
   Dispose(v2) ;
   RETURN( result )
END GreEqu ;


(*
   IsNulSet - returns TRUE if the top element is the nul set constant, {}.
*)

PROCEDURE IsNulSet () : BOOLEAN ;
VAR
   v: PtrToValue ;
   r: BOOLEAN ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      r := (type=set) AND (setValue=NIL)
   END ;
   Push(v) ;
   RETURN( r )
END IsNulSet ;


(*
   IsGenericNulSet - returns TRUE if the top element is the generic nul set constant, {}.
*)

PROCEDURE IsGenericNulSet () : BOOLEAN ;
VAR
   v: PtrToValue ;
   r: BOOLEAN ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      r := (type=set) AND (setValue=NIL) AND (setType=NulSym)
   END ;
   Push(v) ;
   RETURN( r )
END IsGenericNulSet ;


(*
   PushNulSet - pushes an empty set {} onto the ALU stack. The subrange type used
                to construct the set is defined by, setType. If this is NulSym then
                the set is generic and compatible with all sets.

                The Stack:

                Entry             Exit

                                                 <- Ptr
                                  +------------+
                                  | {}         |
                Ptr ->            |------------|
                                  
*)

PROCEDURE PushNulSet (settype: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := InitValue() ;
   WITH v^ DO
      type     := set ;
      setType  := settype ;
      solved   := CompletelyResolved(settype) ;
      setValue := NIL ;
      next     := NIL ;
   END ;
   Push(v)
END PushNulSet ;


(*
   SetNegate - negates the top set on the stack.

               Ptr ->                                               <- Ptr
                      +-----------+                  +------------+
                      | Set       |                  | Set        |
                      |-----------|                  |------------|
*)

PROCEDURE SetNegate (tokenno: CARDINAL) ;
VAR
   min,
   max : CARDINAL ;
   r, s: listOfRange ;
   v   : PtrToValue ;
   i, j: CARDINAL ;
BEGIN
   v := Pop() ;
   Eval(tokenno, v) ;
   IF v^.setType=NulSym
   THEN
      WriteFormat0('cannot negate a generic set, set should be prefixed by a simple type')
   END ;
   r := NIL ;
   min := GetTypeMin(GetType(v^.setType)) ;
   max := GetTypeMax(GetType(v^.setType)) ;
   i := min ;
   s := v^.setValue ;
   WHILE s#NIL DO
      PushValue(s^.low) ;
      PushValue(min) ;
      IF Gre(tokenno)
      THEN
         PushValue(i) ;
         PushValue(max) ;
         IF LessEqu(tokenno)
         THEN
            r := AddRange(r, i, DupConst(tokenno, s^.low, -1))
         END
      END ;
      i := DupConst(tokenno, s^.high, 1) ;
      s := s^.next
   END ;
   DisposeRange(v^.setValue) ;
   PushValue(i) ;
   PushValue(max) ;
   IF LessEqu(tokenno)
   THEN
      r := AddRange(r, i, max)
   END ;
   WITH v^ DO
      solved := FALSE ;
      setValue := r ;
   END ;
   Eval(tokenno, v) ;
   Push(v)
END SetNegate ;


(*
   AddBitRange - adds the range op1..op2 to the underlying set.

                 Ptr ->
                                                           <- Ptr
                        +------------+      +------------+
                        | Set        |      | Set        |
                        |------------|      |------------|

*)

PROCEDURE AddBitRange (op1, op2: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   IF v^.type=set
   THEN
      WITH v^ DO
         setValue := AddRange(setValue, op1, op2) ;
         solved   := solved AND IsValueSolved(op1) AND IsValueSolved(op2)
      END
   ELSE
      InternalError('expecting set type constant', __FILE__, __LINE__)
   END ;
   Push(v)
END AddBitRange ;


(*
   AddBit - adds the bit op1 to the underlying set. INCL(Set, op1)

            Ptr ->
                                                      <- Ptr
                   +------------+      +------------+
                   | Set        |      | Set        |
                   |------------|      |------------|
*)

PROCEDURE AddBit (op1: CARDINAL) ;
BEGIN
   AddBitRange(op1, op1)
END AddBit ;


(*
   ElementsSolved - returns TRUE if all ranges in the set have been solved.
*)

PROCEDURE ElementsSolved (r: listOfRange) : BOOLEAN ;
BEGIN
   WHILE r#NIL DO
      WITH r^ DO
         IF NOT (IsValueSolved(low) AND IsValueSolved(high))
         THEN
            RETURN( FALSE )
         END
      END ;
      r := r^.next
   END ;
   RETURN( TRUE )
END ElementsSolved ;


(*
   Swap - swaps the contents of, i, and, j.
*)

PROCEDURE Swap (i, j: listOfRange) ;
VAR
   t: CARDINAL ;
BEGIN
   t := i^.low ;
   i^.low := j^.low ;
   j^.low := t ;

   t := i^.high ;
   i^.high := j^.high ;
   j^.high := t
END Swap ;


(*
   DisplayElements - 
*)

PROCEDURE DisplayElements (i: listOfRange) ;
VAR
   t: Tree ;
BEGIN
   WHILE i#NIL DO
      PushValue(i^.low) ;
      PrintValue ;
      t := PopIntegerTree() ;
      PushValue(i^.high) ;
      PrintValue ;
      t := PopIntegerTree() ;
      i := i^.next
   END
END DisplayElements ;


(*
   SortElements - sorts the list as defined by, h, into ascending range order.
                  The low element is the sort key.
*)

PROCEDURE SortElements (tokenno: CARDINAL; h: listOfRange) ;
VAR
   i, j, k: listOfRange ;
BEGIN
   i := h ;
   WHILE i#NIL DO
      j := i ;
      k := i^.next ;
      WHILE k#NIL DO
         PushValue(k^.low) ;
         ConvertToInt ;
         PushValue(j^.low) ;
         ConvertToInt ;
         IF Less(tokenno)
         THEN
            j := k ;
         END ;
         k := k^.next
      END ;
      Swap(i, j) ;
      i := i^.next
   END
END SortElements ;


(*
   CombineElements - given a sorted list determine whether there is any
                     overlap in the low..high bounds. If overlap exists
                     then remove it.
*)

PROCEDURE CombineElements (tokenno: CARDINAL; r: listOfRange) ;
VAR
   t, j: listOfRange ;
BEGIN
   WHILE r#NIL DO
      j := r^.next ;
      WHILE j#NIL DO
         PushValue(r^.high) ;
         ConvertToInt ;
         PushCard(1) ;
         Addn ;
         PushValue(j^.low) ;
         ConvertToInt ;
         IF GreEqu(tokenno)
         THEN
            r^.high := j^.high ;
            t := j^.next ;
            r^.next := j^.next ;
            j^.next := NIL ;
            DisposeRange(j) ;
            j := t
         ELSE
            j := NIL
         END
      END ;
      r := r^.next
   END
END CombineElements ;


(*
   EvalElements - returns TRUE if all elements in this set have been resolved.
*)

PROCEDURE EvalElements (tokenno: CARDINAL; r: listOfRange) : BOOLEAN ;
BEGIN
   IF ElementsSolved(r)
   THEN
      SortElements(tokenno, r) ;
      CombineElements(tokenno, r) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END EvalElements ;


(*
   Eval - attempts to solve a set type.
*)

PROCEDURE Eval (tokenno: CARDINAL; v: PtrToValue) ;
BEGIN
   CheckNotAlreadyOnFreeList(v) ;
   WITH v^ DO
      IF type=set
      THEN
         Assert((setType=NulSym) OR IsSet(setType)) ;
         solved := CompletelyResolved(setType) AND EvalElements(tokenno, setValue)
      END
   END
END Eval ;


(*
   CollectSetDependants - traverse the set, sym, declaring all constants which define the set value.
                          TRUE is returned if all constants and the set type has been declared to GCC.
*)

PROCEDURE CollectSetDependants (tokenno: CARDINAL; sym: CARDINAL) : BOOLEAN ;
VAR
   v       : PtrToValue ;
   r       : listOfRange ;
   resolved: BOOLEAN ;
BEGIN
   resolved := TRUE ;
   PushValue(sym) ;
   v := Pop() ;
   WITH v^ DO
      IF (type=set) AND (NOT solved)
      THEN
         resolved := CompletelyResolved(setType) ;
         r := setValue ;
         WHILE r#NIL DO
            WITH r^ DO
               DeclareConstant(tokenno, low) ;
               DeclareConstant(tokenno, high) ;
               resolved := resolved AND CompletelyResolved(low) AND CompletelyResolved(high)
            END ;
            r := r^.next
         END ;
         solved := resolved
      END
   END ;
   Push(v) ;
   PopValue(sym) ;
   RETURN( resolved )
END CollectSetDependants ;


(*
   FindValueEnum - 
*)

PROCEDURE FindValueEnum (field: WORD) ;
BEGIN
   PushValue(field) ;
   PushIntegerTree(EnumerationValue) ;
   IF Equ(CurrentTokenNo)
   THEN
      EnumerationField := field
   END
END FindValueEnum ;


(*
   Val - returns a GCC symbol enumeration or a GCC constant which has, value, and which is
         of type, type.
*)

PROCEDURE Val (tokenno: CARDINAL; type: CARDINAL; value: Tree) : CARDINAL ;
VAR
   sym: CARDINAL ;
BEGIN
   IF IsEnumeration(type)
   THEN
      EnumerationField := NulSym ;
      EnumerationValue := value ;
      CurrentTokenNo := tokenno ;
      ForeachFieldEnumerationDo(type, FindValueEnum) ;
      IF EnumerationField=NulSym
      THEN
         InternalError('enumeration value exceeds range', __FILE__, __LINE__)
      END ;
      RETURN( EnumerationField )
   ELSE
      sym := MakeTemporary(ImmediateValue) ;
      PutVar(sym, type) ;
      PushIntegerTree(value) ;
      PopValue(sym) ;
      RETURN( sym )
   END
END Val ;


(*
   DupConst - duplicates and returns a constant, sym, but adds, offset to its value.
*)

PROCEDURE DupConst (tokenno: CARDINAL; sym: CARDINAL; offset: INTEGER) : CARDINAL ;
BEGIN
   PushValue(sym) ;
   PushInt(offset) ;
   Addn ;
   RETURN( Val(tokenno, GetType(sym), PopIntegerTree()) )
END DupConst ;


(*
   DupConstAndAdd - duplicates and returns a constant, sym,
                    but adds the symbol, extra.
*)

PROCEDURE DupConstAndAdd (tokenno: CARDINAL;
                          sym: CARDINAL; extra: Tree) : CARDINAL ;
BEGIN
   PushValue(sym) ;
   PushIntegerTree(extra) ;
   Addn ;
   RETURN( Val(tokenno, GetType(sym), PopIntegerTree()) )
END DupConstAndAdd ;


(*
   DupConstAndAddMod - duplicates and returns a constant, sym,
                       but adds the symbol, extra, and ensures that
                       the result in within limits: min..max using
                       modulo arithmetic.
*)

PROCEDURE DupConstAndAddMod (tokenno: CARDINAL;
                             sym: CARDINAL; extra: Tree;
                             l, h: CARDINAL) : CARDINAL ;
BEGIN
   (* result := (((sym-l) + extra) MOD (h-l)) + l) *)
   PushValue(sym) ;
   PushValue(l) ;
   Sub ;
   PushIntegerTree(extra) ;
   Addn ;
   PushValue(h) ;
   PushValue(l) ;
   Sub ;
   Mod ;
   PushValue(l) ;
   Addn ;
   RETURN( Val(tokenno, GetType(sym), PopIntegerTree()) )
END DupConstAndAddMod ;


(*
   Remove - removes, v, from list, h.
*)

PROCEDURE Remove (VAR h: listOfRange; v: listOfRange) ;
VAR
   i: listOfRange ;
BEGIN
   IF h=v
   THEN
      h := h^.next
   ELSE
      i := h ;
      WHILE (i#NIL) AND (i^.next#v) DO
         i := i^.next
      END ;
      IF i=NIL
      THEN
         InternalError('expecting v to be on the list', __FILE__, __LINE__)
      ELSE
         i := v^.next
      END
   END ;
   v^.next := NIL ;
   DisposeRange(v)
END Remove ;


(*
   RemoveBit - remove bit, op1, from range, v, on list, h.
*)

PROCEDURE RemoveBit (tokenno: CARDINAL; VAR h: listOfRange; v: listOfRange; op1: CARDINAL) ;
BEGIN
   WITH v^ DO
      PushValue(low) ;
      PushValue(high) ;
      IF Equ(tokenno)
      THEN
         (* single bit in this range *)
         PushValue(low) ;
         PushValue(op1) ;
         IF Equ(tokenno)
         THEN
            (* remove entry *)
            Remove(h, v) ;
            RETURN
         END
      ELSE
         (* is op1 equal to low? *)
         PushValue(op1) ;
         PushValue(low) ;
         IF Equ(tokenno)
         THEN
            low := DupConst(tokenno, low, 1)
         ELSE
            PushValue(op1) ;
            PushValue(high) ;
            IF Equ(tokenno)
            THEN
               high := DupConst(tokenno, high, -1)
            ELSE
               high := DupConst(tokenno, op1, -1) ;
               h := AddRange(h, DupConst(tokenno, op1, 1), high) ;
               SortElements(tokenno, h)
            END
         END
      END
   END
END RemoveBit ;


(*
   PerformSubBit - 
*)

PROCEDURE PerformSubBit (tokenno: CARDINAL; VAR h: listOfRange; op1: CARDINAL) ;
VAR
   v: listOfRange ;
BEGIN
   v := h ;
   WHILE v#NIL DO
      WITH v^ DO
         PushValue(low) ;
         PushValue(op1) ;
         IF LessEqu(tokenno)
         THEN
            PushValue(op1) ;
            PushValue(high) ;
            IF LessEqu(tokenno)
            THEN
               RemoveBit(tokenno, h, v, op1) ;
               RETURN
            END
         END
      END ;
      v := v^.next
   END
END PerformSubBit ;


(*
   SubBit - removes a bit op1 from the underlying set. EXCL(Set, Op1)

            Ptr ->
                                                      <- Ptr
                   +------------+      +------------+
                   | Set        |      | Set        |
                   |------------|      |------------|
*)

PROCEDURE SubBit (tokenno: CARDINAL; op1: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   IF v^.type=set
   THEN
      Eval(tokenno, v) ;
      WITH v^ DO
         IF solved
         THEN
            IF IsValueSolved(op1)
            THEN
               PerformSubBit(tokenno, setValue, op1) ;
               solved := FALSE
            ELSE
               InternalError('can only subtract a bit from a set when the bit value is known', __FILE__, __LINE__)
            END
         ELSE
            InternalError('can only subtract a bit from a set when the set value is known', __FILE__, __LINE__)
         END
      END ;
      Eval(tokenno, v)
   ELSE
      InternalError('expecting set type constant', __FILE__, __LINE__)
   END ;
   Push(v)
END SubBit ;


(*
   PerformSetIn - returns TRUE if op1 is in set.
*)

PROCEDURE PerformSetIn (tokenno: CARDINAL; op1: CARDINAL; h: listOfRange) : BOOLEAN ;
VAR
   v: listOfRange ;
BEGIN
   WHILE h#NIL DO
      WITH h^ DO
         PushValue(op1) ;
         ConvertToInt ;
         PushValue(low) ;
         ConvertToInt ;
         IF GreEqu(tokenno)
         THEN
            PushValue(op1) ;
            PushValue(high) ;
            IF LessEqu(tokenno)
            THEN
               RETURN( TRUE )
            END
         ELSE
            (* op1 is smaller than this and all subsequent ranges *)
            RETURN( FALSE )
         END
      END ;
      h := h^.next
   END ;
   RETURN( FALSE )
END PerformSetIn ;


(*
   SetIn - returns true if Op2 IN Op1

           The Stack:

           Entry             Exit

    Ptr ->
           +------------+
           | Set        |
           |------------|    Empty

           RETURN( Op1 IN Set )
*)

PROCEDURE SetIn (tokenno: CARDINAL; Op1: CARDINAL) : BOOLEAN ;
VAR
   Set   : PtrToValue ;
   result: BOOLEAN ;
BEGIN
   Set := Pop() ;
   IF Set^.type#set
   THEN
      InternalError('expecting ALU operand to be a set', __FILE__, __LINE__)
   END ;
   Eval(tokenno, Set) ;
   IF IsValueSolved(Op1) AND Set^.solved
   THEN
      result := PerformSetIn(tokenno, Op1, Set^.setValue)
   ELSE
      InternalError('one or more operands have not been resolved', __FILE__, __LINE__)
   END ;
   Dispose(Set) ;
   RETURN( result )
END SetIn ;


(*
   SetOp - perform the function doOp on the top two elements of the stack.
*)

PROCEDURE SetOp (tokenno: CARDINAL; doOp: DoSetProcedure) ;
VAR
   Result,
   Set1, Set2: PtrToValue ;
BEGIN
   Set1 := Pop() ;
   Set2 := Pop() ;
   Eval(tokenno, Set1) ;
   Eval(tokenno, Set2) ;
   IF NOT (Set1^.solved AND Set2^.solved)
   THEN
      InternalError('one or more operands have not been resolved', __FILE__, __LINE__)
   END ;
   IF Set1^.type#set
   THEN
      InternalError('expecting type of constant to be a set', __FILE__, __LINE__)
   END ;
   IF Set2^.type#set
   THEN
      InternalError('expecting type of constant to be a set', __FILE__, __LINE__)
   END ;
   Result := New() ;
   WITH Result^ DO
      type     := set ;
      setValue := doOp(tokenno, Set1^.setValue, Set2^.setValue) ;
      setType  := MixTypes(Set1^.setType, Set2^.setType, tokenno) ;
      solved   := FALSE
   END ;
   (* Set1 and Set2 have given their range lists to the Result *)
   Set1^.setValue := NIL ;
   Set2^.setValue := NIL ;
   Eval(tokenno, Result) ;
   Push(Result) ;
   Dispose(Set1) ;
   Dispose(Set2)
END SetOp ;


(*
   PerformOr - performs a logical OR between the two ranges.
               The ranges, r1, r2, are destroyed.
*)

PROCEDURE PerformOr (tokenno: CARDINAL; r1, r2: listOfRange) : listOfRange ;
VAR
   i: listOfRange ;
BEGIN
   i := r1 ;
   WHILE (i#NIL) AND (i^.next#NIL) DO
      i := i^.next
   END ;
   IF i=NIL
   THEN
      r1 := r2
   ELSE
      i^.next := r2
   END ;
   SortElements(tokenno, r1) ;
   CombineElements(tokenno, r1) ;
   RETURN( r1 )
END PerformOr ;


(*
   SetOr -  performs an inclusive OR of the top two elements on the stack.

            The Stack:

            Entry             Exit

     Ptr ->
            +------------+
            | Set1       |                   <- Ptr
            |------------|    +------------+
            | Set2       |    | Set1 + Set2|
            |------------|    |------------|

*)

PROCEDURE SetOr (tokenno: CARDINAL) ;
BEGIN
   SetOp(tokenno, PerformOr)
END SetOr ;


(*
   Min - returns the symbol which has the least value.
*)

PROCEDURE Min (tokenno: CARDINAL; a, b: CARDINAL) : CARDINAL ;
BEGIN
   PushValue(a) ;
   ConvertToInt ;
   PushValue(b) ;
   ConvertToInt ;
   IF Less(tokenno)
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


(*
   Max - returns the symbol which has the greatest value.
*)

PROCEDURE Max (tokenno: CARDINAL; a, b: CARDINAL) : CARDINAL ;
BEGIN
   PushValue(a) ;
   ConvertToInt ;
   PushValue(b) ;
   ConvertToInt ;
   IF Gre(tokenno)
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   IsRangeIntersection - returns TRUE if ranges, r1, and, r2, intersect.
*)

PROCEDURE IsRangeIntersection (tokenno: CARDINAL; r1, r2: listOfRange) : BOOLEAN ;
BEGIN
   IF (r1=NIL) OR (r2=NIL)
   THEN
      RETURN( FALSE )
   ELSE
      (* easier to prove NOT outside limits *)
      PushValue(r1^.low) ;
      ConvertToInt ;
      PushValue(r2^.high) ;
      ConvertToInt ;
      IF Gre(tokenno)
      THEN
         RETURN( FALSE )
      ELSE
         PushValue(r1^.high) ;
         ConvertToInt ;
         PushValue(r2^.low) ;
         ConvertToInt ;
         IF Less(tokenno)
         THEN
            RETURN( FALSE )
         ELSE
            RETURN( TRUE )
         END
      END
   END
END IsRangeIntersection ;


(*
   IsRangeLess - returns TRUE if r1^.low is < r2^.low
*)

PROCEDURE IsRangeLess (tokenno: CARDINAL; r1, r2: listOfRange) : BOOLEAN ;
BEGIN
   IF (r1=NIL) OR (r2=NIL)
   THEN
      InternalError('not expecting NIL ranges', __FILE__, __LINE__)
   END ;
   PushValue(r1^.high) ;
   ConvertToInt ;
   PushValue(r2^.low) ;
   ConvertToInt ;
   RETURN( Less(tokenno) )
END IsRangeLess ;


(*
   MinTree - returns the tree symbol which has the least value.
*)

PROCEDURE MinTree (tokenno: CARDINAL; a, b: Tree) : Tree ;
BEGIN
   PushIntegerTree(a) ;
   ConvertToInt ;
   PushIntegerTree(b) ;
   ConvertToInt ;
   IF Less(tokenno)
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END MinTree ;


(*
   MaxTree - returns the symbol which has the greatest value.
*)

PROCEDURE MaxTree (tokenno: CARDINAL; a, b: Tree) : Tree ;
BEGIN
   PushIntegerTree(a) ;
   ConvertToInt ;
   PushIntegerTree(b) ;
   ConvertToInt ;
   IF Gre(tokenno)
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END MaxTree ;


(*
   IsIntersectionTree - returns TRUE if ranges, a..b, and, c..d, intersect.
*)

PROCEDURE IsIntersectionTree (tokenno: CARDINAL; a, b, c, d: Tree) : BOOLEAN ;
BEGIN
   (* easier to prove NOT outside limits *)
   PushIntegerTree(a) ;
   ConvertToInt ;
   PushIntegerTree(d) ;
   ConvertToInt ;
   IF Gre(tokenno)
   THEN
      RETURN( FALSE )
   ELSE
      PushIntegerTree(b) ;
      ConvertToInt ;
      PushIntegerTree(c) ;
      ConvertToInt ;
      IF Less(tokenno)
      THEN
         RETURN( FALSE )
      ELSE
         RETURN( TRUE )
      END
   END
END IsIntersectionTree ;


(*
   SubTree - returns the tree value containing (a-b)
*)

PROCEDURE SubTree (a, b: Tree) : Tree ;
BEGIN
   PushIntegerTree(a) ;
   PushIntegerTree(b) ;
   Sub ;
   RETURN( PopIntegerTree() )
END SubTree ;


(*
   PerformAnd - performs a logical AND between the two ranges.
                The ranges, r1, r2, are unaltered.
*)

PROCEDURE PerformAnd (tokenno: CARDINAL; r1, r2: listOfRange) : listOfRange ;
VAR
   r: listOfRange ;
BEGIN
   r := NIL ;
   WHILE (r1#NIL) AND (r2#NIL) DO
      IF IsRangeIntersection(tokenno, r1, r2)
      THEN
         r := AddRange(r, Max(tokenno, r1^.low, r2^.low), Min(tokenno, r1^.high, r2^.high)) ;
         IF r^.high=r1^.high
         THEN
            r1 := r1^.next
         ELSE
            r2 := r2^.next
         END
      ELSIF IsRangeLess(tokenno, r1, r2)
      THEN
         (* move r1 onto the next range *)
         r1 := r1^.next
      ELSE
         (* move r2 onto the next range *)
         r2 := r2^.next
      END
   END ;
   RETURN( r )
END PerformAnd ;


(*
   SetAnd - performs a set AND the top two elements on the stack.

            The Stack:

            Entry             Exit

     Ptr ->
            +------------+
            | Op1        |                   <- Ptr
            |------------|    +------------+
            | Op2        |    | Op2 * Op1  |
            |------------|    |------------|
*)

PROCEDURE SetAnd (tokenno: CARDINAL) ;
BEGIN
   SetOp(tokenno, PerformAnd)
END SetAnd ;


(*
   SetDifference - performs a set difference of the top two elements on the stack.
                   For each member in the set
                      if member in Op2 and not member in Op1

                   The Stack:

                   Entry             Exit

            Ptr ->
                   +------------+
                   | Op1        |                   <- Ptr
                   |------------|    +-------------------+
                   | Op2        |    | Op2 and (not Op1) |
                   |------------|    |-------------------|
*)

PROCEDURE SetDifference (tokenno: CARDINAL) ;
VAR
   Set1, Set2: PtrToValue ;
BEGIN
   Set1 := Pop() ;
   Set2 := Pop() ;
   Eval(tokenno, Set1) ;
   Eval(tokenno, Set2) ;
   IF NOT (Set1^.solved AND Set2^.solved)
   THEN
      InternalError('one or more operands have not been resolved', __FILE__, __LINE__)
   END ;
   IF Set1^.setValue=NIL
   THEN
      (* null set, return Set2 *)
      Push(Set1) ;
   ELSE
      Push(Set1) ;
      SetNegate(tokenno) ;
      Push(Set2) ;
      SetAnd(tokenno)
   END
END SetDifference ;


(*
   SetSymmetricDifference - performs a set difference of the top two elements on the stack.

                            The Stack:

                            Entry             Exit

                     Ptr ->
                            +------------+
                            | Op1        |                    <- Ptr
                            |------------|    +-------------+
                            | Op2        |    | Op2 xor Op1 |
                            |------------|    |-------------|
*)

PROCEDURE SetSymmetricDifference (tokenno: CARDINAL) ;
VAR
   Set1, Set2: PtrToValue ;
BEGIN
   Set1 := Pop() ;
   Set2 := Pop() ;
   Eval(tokenno, Set1) ;
   Eval(tokenno, Set2) ;
   IF NOT (Set1^.solved AND Set2^.solved)
   THEN
      InternalError('one or more operands have not been resolved', __FILE__, __LINE__)
   END ;
   IF Set1^.setValue=NIL
   THEN
      Dispose(Set1) ;
      Push(Set2)
   ELSIF Set2^.setValue=NIL
   THEN
      Dispose(Set2) ;
      Push(Set1)
   ELSE
      (* Set1 or Set2 and (not (Set1 and Set2)) *)
      PushFrom(Set1) ;
      PushFrom(Set2) ;
      SetAnd(tokenno) ;
      SetNegate(tokenno) ;
      Push(Set1) ;
      Push(Set2) ;
      SetOr(tokenno) ;
      SetAnd(tokenno)
   END
END SetSymmetricDifference ;


(*
   SetShift - if op1 is positive
              then
                 result := op2 << op1
              else
                 result := op2 >> op1
              fi


              The Stack:

                     Entry             Exit

              Ptr ->
                     +------------+
                     | Op1        |                   <- Ptr
                     |------------|    +------------+
                     | Op2        |    | result     |
                     |------------|    |------------|

*)

PROCEDURE SetShift (tokenno: CARDINAL) ;
VAR
   res,
   Shift,
   Set    : PtrToValue ;
   n      : CARDINAL ;
   r1, r2 : CARDINAL ;
BEGIN
   IF NOT IsValueTypeInteger()
   THEN
      InternalError('expecting integer type', __FILE__, __LINE__)
   END ;
   Shift := Pop() ;
   IF NOT IsValueTypeSet()
   THEN
      InternalError('expecting set type', __FILE__, __LINE__)
   END ;
   Set := Pop() ;
   Eval(tokenno, Set) ;
   IF NOT Set^.solved
   THEN
      InternalError('set has not been resolved', __FILE__, __LINE__)
   END ;
   IF Set^.setValue=NIL
   THEN
      Push(Set)
   ELSE
      res := New() ;
      res^ := Set^ ;
      WITH res^ DO
         setValue := NIL ;
         n := 1 ;
         WHILE GetRange(Set^.setValue, n, r1, r2) DO
            setValue := AddRange(setValue,
                                 DupConstAndAdd(tokenno, r1, Shift),
                                 DupConstAndAdd(tokenno, r2, Shift)) ;
            INC(n)
         END ;
         Push(res) ;
         IF setType#NulSym
         THEN
            PushNulSet(setType) ;
            SetNegate(tokenno) ;
            SetAnd(tokenno)
         END
      END ;
      Dispose(set)
   END
END SetShift ;


(*
   SetRotate - if op1 is positive
               then
                  result := ROTATERIGHT(op2, op1)
               else
                  result := ROTATELEFT(op2, op1)
               fi


               The Stack:

                      Entry             Exit

               Ptr ->
                      +------------+
                      | Op1        |                   <- Ptr
                      |------------|    +------------+
                      | Op2        |    | result     |
                      |------------|    |------------|
*)

PROCEDURE SetRotate (tokenno: CARDINAL) ;
VAR
   res,
   Rotate,
   Set    : PtrToValue ;
   n      : CARDINAL ;
   l, h,
   type,
   r1, r2 : CARDINAL ;
BEGIN
   IF NOT IsValueTypeInteger()
   THEN
      InternalError('expecting integer type', __FILE__, __LINE__)
   END ;
   Rotate := Pop() ;
   IF NOT IsValueTypeSet()
   THEN
      InternalError('expecting set type', __FILE__, __LINE__)
   END ;
   Set := Pop() ;
   Eval(tokenno, Set) ;
   IF NOT Set^.solved
   THEN
      InternalError('set has not been resolved', __FILE__, __LINE__)
   END ;
   IF Set^.setValue=NIL
   THEN
      Push(Set)
   ELSE
      type := Set^.setType ;
      IF type=NulSym
      THEN
         ErrorStringAt(InitString('cannot perform a ROTATE on a generic set'), tokenno) ;
         Push(Set) ;
         RETURN
      END ;
      l := GetTypeMin(type) ;
      h := GetTypeMax(type) ;
      res := New() ;
      res^ := Set^ ;
      WITH res^ DO
         setValue := NIL ;
         n := 1 ;
         WHILE GetRange(Set^.setValue, n, r1, r2) DO
            setValue := AddRange(setValue,
                                 DupConstAndAddMod(tokenno, r1, Rotate, l, h),
                                 DupConstAndAddMod(tokenno, r2, Rotate, l, h)) ;
            INC(n)
         END
      END ;
      Push(res) ;
      Dispose(set)
   END
END SetRotate ;


(*
   GetValue - returns and pops the value from the top of stack.
*)

PROCEDURE GetValue (tokenno: CARDINAL) : PtrToValue ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   Eval(tokenno, v) ;
   RETURN( v )
END GetValue ;


(*
   GetRange - returns TRUE if range number, n, exists in the value, v.
              A non empty set is defined by having 1..N ranges
*)

PROCEDURE GetRange (v: PtrToValue; n: CARDINAL; VAR low, high: CARDINAL) : BOOLEAN ;
VAR
   l: listOfRange ;
BEGIN
   WITH v^ DO
      IF type#set
      THEN
         InternalError('expecting set constant', __FILE__, __LINE__)
      END ;
      l := setValue ;
      WHILE n>1 DO
         IF l=NIL
         THEN
            RETURN( FALSE )
         END ;
         l := l^.next ;
         DEC(n)
      END ;
      IF l=NIL
      THEN
         RETURN( FALSE )
      END ;
      low := l^.low ;
      high := l^.high
   END ;
   RETURN( TRUE )
END GetRange ;


(*
   BuildStructBitset - v is the PtrToValue.
                       low and high are the limits of the subrange.
*)

PROCEDURE BuildStructBitset (tokenno: CARDINAL; v: PtrToValue; low, high: Tree) : Tree ;
VAR
   BitsInSet,
   GccField  : Tree ;
   bpw       : CARDINAL ;
BEGIN
   PushIntegerTree(low) ;
   ConvertToInt ;
   low := PopIntegerTree() ;
   PushIntegerTree(high) ;
   ConvertToInt ;
   high := PopIntegerTree() ;
   bpw  := GetBitsPerBitset() ;

   PushIntegerTree(high) ;
   PushIntegerTree(low) ;
   Sub ;
   PushCard(1) ;
   Addn ;
   BitsInSet := PopIntegerTree() ;

   BuildStartSetConstructor(Mod2Gcc(v^.setType)) ;

   PushIntegerTree(BitsInSet) ;
   PushCard(0) ;
   WHILE Gre(tokenno) DO
      PushIntegerTree(BitsInSet) ;
      PushCard(bpw-1) ;
      IF GreEqu(tokenno)
      THEN
         PushIntegerTree(low) ;
         PushCard(bpw-1) ;
         Addn ;

         BuildSetConstructorElement(BuildBitset(tokenno, v, low, PopIntegerTree())) ;

         PushIntegerTree(low) ;
         PushCard(bpw) ;
         Addn ;
         low := PopIntegerTree() ;
         PushIntegerTree(BitsInSet) ;
         PushCard(bpw) ;
         Sub ;
         BitsInSet := PopIntegerTree()
      ELSE
         (* printf2('range is %a..%a\n', GetSymName(low), GetSymName(high)) ; *)

         BuildSetConstructorElement(BuildBitset(tokenno, v, low, high)) ;

         PushCard(0) ;
         BitsInSet := PopIntegerTree()
      END ;
      PushIntegerTree(BitsInSet) ;
      PushCard(0)
   END ;
   RETURN( BuildEndSetConstructor() )
END BuildStructBitset ;


(*
   ConstructLargeOrSmallSet - generates a constant representing the set value of the symbol, sym.
                              We manufacture the constant by using a initialization
                              structure of integers.

                              { (int), (int) etc }
*)

PROCEDURE ConstructLargeOrSmallSet (tokenno: CARDINAL; v: PtrToValue; low, high: CARDINAL) : Tree ;
BEGIN
   PushValue(high) ;
   ConvertToInt ;
   PushValue(low) ;
   ConvertToInt ;
   Sub ;
   PushCard(GetBitsPerBitset()) ;
   IF Less(tokenno)
   THEN
      (* small set *)
      RETURN( BuildBitset(tokenno, v, Mod2Gcc(low), Mod2Gcc(high)) )
   ELSE
      (* large set *)
      RETURN( BuildStructBitset(tokenno, v, Mod2Gcc(low), Mod2Gcc(high)) )
   END
END ConstructLargeOrSmallSet ;


(*
   ConstructSetConstant - builds a struct of integers which represents the
                          set const as defined by, v.
*)

PROCEDURE ConstructSetConstant (tokenno: CARDINAL; v: PtrToValue) : Tree ;
VAR
   n1, n2   : Name ;
   gccsym   : Tree ;
   baseType,
   high, low: CARDINAL ;
BEGIN
   WITH v^ DO
      IF setType=NulSym
      THEN
         InternalError('set type must be known in order to generate a constant', __FILE__, __LINE__)
      ELSE
         baseType := GetType(setType) ;
         IF Debugging
         THEN
            n1 := GetSymName(setType) ;
            n2 := GetSymName(baseType) ;
            printf2('ConstructSetConstant of type %a and baseType %a\n', n1, n2)
         END ;
         IF IsSubrange(baseType)
         THEN
            GetSubrange(baseType, high, low) ;
            gccsym := ConstructLargeOrSmallSet(tokenno, v, low, high)
         ELSE
            gccsym := ConstructLargeOrSmallSet(tokenno, v, GetTypeMin(baseType), GetTypeMax(baseType))
         END ;
         RETURN( gccsym )
      END
   END
END ConstructSetConstant ;


(*
   BuildRange - returns a integer sized constant which represents the
                value  {e1..e2}.
*)

PROCEDURE BuildRange (tokenno: CARDINAL; e1, e2: Tree) : Tree ;
VAR
   c, i, t: Tree ;
BEGIN
   PushIntegerTree(e1) ;
   PushIntegerTree(e2) ;
   IF Gre(tokenno)
   THEN
      c := e1 ;
      e1 := e2 ;
      e2 := c
   END ;
   t := Tree(NIL) ;
   PushIntegerTree(e1) ;
   i := PopIntegerTree() ;
   REPEAT
      IF t=Tree(NIL)
      THEN
         t := BuildLSL(GetWordOne(), ToWord(i), FALSE)
      ELSE
         t := BuildLogicalOr(t, BuildLSL(GetWordOne(), ToWord(i), FALSE), FALSE)
      END ;
      PushIntegerTree(i) ;
      PushIntegerTree(GetIntegerOne()) ;
      Addn ;
      i := PopIntegerTree() ;
      PushIntegerTree(i) ;
      PushIntegerTree(e2) ;
   UNTIL Gre(tokenno) ;
   RETURN( t )
END BuildRange ;


(*
   BuildBitset - given a set, v, construct the bitmask for its constant value which
                 lie in the range low..high.
*)

PROCEDURE BuildBitset (tokenno: CARDINAL; v: PtrToValue; low, high: Tree) : Tree ;
VAR
   tl, th,
   t      : Tree ;
   n      : CARDINAL ;
   r1, r2 : CARDINAL ;
BEGIN
   n := 1 ;
   t := GetIntegerZero() ;
   WHILE GetRange(v, n, r1, r2) DO
      PushValue(r1) ;
      tl := PopIntegerTree() ;
      PushValue(r2) ;
      th := PopIntegerTree() ;
      IF IsIntersectionTree(tokenno, tl, th, low, high)
      THEN
         tl := SubTree(MaxTree(tokenno, tl, low), low) ;
         th := SubTree(MinTree(tokenno, th, high), low) ;
         t := BuildLogicalOr(t, BuildRange(tokenno, tl, th), FALSE)
      END ;
      INC(n)
   END ;
   RETURN( t )
END BuildBitset ;


(*
   IsValueAndTreeKnown - returns TRUE if the value is known and the gcc tree
                         is defined.

                         The Stack:

                                Entry             Exit

                         Ptr ->
                                +------------+
                                | Op1        |                   <- Ptr
                                |------------|    +------------+
*)

PROCEDURE IsValueAndTreeKnown () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   IF v#NIL
   THEN
      WITH v^ DO
         IF solved
         THEN
            CASE type OF

            integer, real:  IF numberValue=NIL
                            THEN
                               Dispose(v) ;
                               RETURN( FALSE )
                            END
            ELSE
            END
         ELSE
            Dispose(v) ;
            RETURN( FALSE )
         END
      END ;
      Dispose(v)
   END ;
   RETURN( TRUE )
END IsValueAndTreeKnown ;


(*
   CheckOverflow - tests to see whether the tree, t, has caused
                   an overflow error and if so it generates an
                   error message.
*)

PROCEDURE CheckOverflow (tokenno: CARDINAL; t: Tree) ;
BEGIN
   IF TreeOverflow(t)
   THEN
      ErrorStringAt(InitString('constant overflow error'), tokenno) ;
      FlushErrors
   END
END CheckOverflow ;


(*
   Init - initialises the stack and the free list.
*)

PROCEDURE Init ;
BEGIN
   FreeList      := NIL ;
   TopOfStack    := NIL ;
   RangeFreeList := NIL ;
END Init ;


BEGIN
   Init
END M2ALU.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I.:../gm2-libs:../gm2-libs-ch:../gm2-libiberty/ M2ALU.mod"
 * End:
 *)