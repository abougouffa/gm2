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
    Last edit  : Mon Jul 10 12:04:50 2000
    Description: gcc implementation of the M2ALU module, this module provides an interface
                 between some of the Modula-2 front end optimization routines and basic
                 DAG construction, needed so that efficient trees can be passed to gcc's
                 backend. M2ALU allows constant expressions to be calculated.
*)

FROM ASCII IMPORT nul ;
FROM StrLib IMPORT StrLen ;
FROM NameKey IMPORT GetKey, KeyToCharStar ;
FROM FpuIO IMPORT StrToLongReal ;
FROM M2Lexical IMPORT WriteError, InternalError ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM M2Debug IMPORT Assert ;
FROM Storage IMPORT ALLOCATE ;

FROM NumberIO IMPORT StrToInt, StrToCard, StrToHex, StrToOct, StrToBin, WriteCard, BinToStr,
                     StrToBinInt, StrToOctInt, StrToHexInt ;

FROM gccgm2 IMPORT Tree, BuildIntegerConstant,
                   DetermineSign, ConvertConstantAndCheck, GetIntegerType, GetLongRealType,
                   GetIntegerOne, GetIntegerZero,
                   AreConstantsEqual, GetBitsPerWord,
                   BuildAdd, BuildSub, BuildMult, BuildDiv, BuildMod, BuildLSL,
                   BuildLogicalOr, BuildLogicalAnd, BuildSymmetricDifference,
                   BuildIfIn,
                   RealToTree, RememberConstant ;

VAR
   FreeList,
   TopOfStack : PtrToValue ;


(* %%%FORWARD%%%
PROCEDURE Push (v: PtrToValue) ; FORWARD ;
PROCEDURE Pop () : PtrToValue ; FORWARD ;
PROCEDURE IsReal (a: ARRAY OF CHAR) : BOOLEAN ; FORWARD ;
PROCEDURE RealSub (Op1, Op2: PtrToValue) ; FORWARD ;
PROCEDURE RealAdd (Op1, Op2: PtrToValue) ; FORWARD ;
PROCEDURE RealMult (Op1, Op2: PtrToValue) ; FORWARD ;
PROCEDURE RealDiv (Op1, Op2: PtrToValue) ; FORWARD ;
PROCEDURE RealMod (Op1, Op2: PtrToValue) ; FORWARD ;
   %%%FORWARD%%% *)


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
      FreeList := FreeList^.Next
   END ;
   RETURN( v )
END New ;


(*
   Dispose - place, v, onto the FreeList.
*)

PROCEDURE Dispose (v: PtrToValue) ;
BEGIN
   v^.Next := FreeList ;
   FreeList := v
END Dispose ;


(*
   InitValue - initializes a memory cell. v is set to the index
               of the initialized cell.
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
         Solved := FALSE
      END ;
      RETURN( v )
   END
END InitValue ;


(*
   PushIntegerTree - pushes a gcc tree value onto the ALU stack.
*)

PROCEDURE PushIntegerTree (t: Tree) ;
VAR
   v: PtrToValue ;
BEGIN
   v := InitValue() ;
   WITH v^ DO
      Type   := integer ;
      Value  := t
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
      IF Type=integer
      THEN
         t := Value
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
      Type   := real ;
      Value  := t
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
      IF Type=real
      THEN
         t := Value
      ELSE
         InternalError('expecting type of constant to be a real number', __FILE__, __LINE__)
      END
   END ;
   Dispose(v) ;
   RETURN( t )
END PopRealTree ;


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
      v^.Solved  := TRUE ;
      TopOfStack := TopOfStack^.Next
   END ;
   RETURN( v )
END Pop ;


(*
   Push - pushes the value onto the stack.
*)

PROCEDURE Push (v: PtrToValue) ;
BEGIN
   v^.Next    := TopOfStack ;
   TopOfStack := v
END Push ;


(*
   PushFrom - pushes a copy of the contents of, v, onto stack.
*)

PROCEDURE PushFrom (v: PtrToValue) ;
VAR
   t: PtrToValue ;
BEGIN
   t := New() ;     (* as it is a copy *)
   t^ := v^ ;
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
   v^.Value := RememberConstant(t^.Value) ;
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
      Type   := integer ;
      Value  := BuildIntegerConstant(INTEGER(c))
   END ;
   Push(v)
END PushCard ;

 
(*
   PopCard - attempts to return the top element from the stack as
             a cardinal.
             If the top element is negative then an error messsage
             is emitted.
*)

PROCEDURE PopCard () : CARDINAL ;
BEGIN
   InternalError('why are we calling PopCard()?', __FILE__, __LINE__) ;
   RETURN( 0 )  (* to keep gm2 happy *)
END PopCard ;
 

(*
   PushInt - pushes an integer onto the stack.
*)

PROCEDURE PushInt (i: INTEGER) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      Type   := integer ;
      Value  := BuildIntegerConstant(i)
   END ;
   Push(v)
END PushInt ;

 
(*
   PopInt - attempts to return the top element from the stack as
            an integer.
*)
 
PROCEDURE PopInt () : INTEGER ;
BEGIN
   InternalError('why are we calling PopInt()?', __FILE__, __LINE__) ;
   RETURN( 0 )  (* to keep gm2 happy *)
END PopInt ;
 
 
(*
   PushChar - pushes a char onto the stack.
*)
 
PROCEDURE PushChar (c: CHAR) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      Type   := integer ;
      Value  := BuildIntegerConstant(ORD(c))
   END ;
   Push(v)
END PushChar ;
 
 
(*
   PopChar - attempts to return the top element from the stack as
             a char.
*)

PROCEDURE PopChar () : CHAR ;
BEGIN
   InternalError('why are we calling PopChar()?', __FILE__, __LINE__) ;
   RETURN( nul )  (* to keep gm2 happy *)
END PopChar ;
 

(*
   PushReal - pushes a REAL, r, onto the stack.
*)

PROCEDURE PushReal (r: REAL) ;
BEGIN
   InternalError('why are we calling PushReal?', __FILE__, __LINE__) ;
END PushReal ;


(*
   PushLongReal - pushes a LONGREAL, r, onto the stack.
*)

PROCEDURE PushLongReal (r: LONGREAL) ;
BEGIN
   InternalError('why are we calling PushLongReal?', __FILE__, __LINE__) ;
END PushLongReal ;


(*
   PopReal - attempts to return the top element from the stack as
             a REAL.
*)

PROCEDURE PopReal () : REAL ;
BEGIN
   InternalError('why are we calling PopReal()?', __FILE__, __LINE__) ;
   RETURN( 0.0 )  (* to keep gm2 happy *)
END PopReal ;


(*
   PopLongReal - attempts to return the top element from the stack as
                 a LONGREAL.
*)

PROCEDURE PopLongReal () : LONGREAL ;
BEGIN
   InternalError('why are we calling PopLongReal()?', __FILE__, __LINE__) ;
   RETURN( 0.0 )  (* to keep gm2 happy *)
END PopLongReal ;


(*
   PushString - pushes the numerical human readable value of the string
                onto the stack.
*)
 
PROCEDURE PushString (s: CARDINAL) ;
CONST
   Max = 64 ;
VAR
   a      : ARRAY [0..Max] OF CHAR ;
   r      : LONGREAL ;
   i, v   : INTEGER ;
   high   : CARDINAL ;
BEGIN
   GetKey(s, a) ;
   high := StrLen(a) ;
   IF a[high-1]='H'
   THEN
      a[high-1] := nul ;
      StrToHexInt(a, v) ;
      PushInt(v)
   ELSIF a[high-1]='A'
   THEN
      a[high-1] := nul ;
      StrToBinInt(a, v) ;
      PushInt(v)
   ELSIF a[high-1]='B'
   THEN
      a[high-1] := nul ;
      StrToOctInt(a, v) ;
      PushInt(v)
   ELSIF a[high-1]='C'
   THEN
      a[high-1] := nul ;
      StrToOctInt(a, v) ;
      PushInt(v)
   ELSIF IsReal(a)
   THEN
      PushRealTree(RealToTree(KeyToCharStar(s)))
   ELSE
      StrToInt(a, i) ;
      PushInt(i)
   END
END PushString ;


(*
   IsReal - returns TRUE if a is a REAL number.
*)

PROCEDURE IsReal (a: ARRAY OF CHAR) : BOOLEAN ;
VAR
   high,
   i   : CARDINAL ;
BEGIN
   high := StrLen(a) ;
   i := 0 ;
   WHILE i<high DO
      IF a[i]='.'
      THEN
         RETURN( TRUE )
      ELSE
         INC(i)
      END
   END ;
   RETURN( FALSE )
END IsReal ;


(*
   CoerseLongRealToCard - performs a coersion between a REAL to a CARDINAL
*)

PROCEDURE CoerseLongRealToCard ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF Type=real
      THEN
         Value := ConvertConstantAndCheck(GetIntegerType(), Value) ;
         Type  := integer
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
      IF Type=real
      THEN
         Value := ConvertConstantAndCheck(GetIntegerType(), Value) ;
         Type  := integer
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
      IF Type=integer
      THEN
         Value := ConvertConstantAndCheck(GetLongRealType(), Value) ;
         Type  := real
      ELSE
         InternalError('expecting an INTEGER number', __FILE__, __LINE__)
      END
   END ;
   Push(v)
END ConvertIntToReal ;


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
      RETURN( v^.Solved )
   END
END IsSolved ;


(*
   EitherReal - returns TRUE if either, Op1, or, Op2, are Real.
*)

PROCEDURE EitherReal (Op1, Op2: PtrToValue) : BOOLEAN ;
BEGIN
   RETURN( (Op1^.Type=real) OR (Op2^.Type=real) )
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
 
PROCEDURE Add ;
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
         Type   := integer ;
         Value  := BuildAdd(Op1^.Value, Op2^.Value, FALSE)
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END Add ;


(*
   RealAdd - adds two numbers. One of which is a Real.
*)

PROCEDURE RealAdd (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF Op1^.Type=integer
   THEN
      Push(Op1) ;
      ConvertIntToReal ;
      Op1 := Pop()
   END ;
   IF Op2^.Type=integer
   THEN
      Push(Op2) ;
      ConvertIntToReal ;
      Op2 := Pop()
   END ;
   Temp := New() ;
   WITH Temp^ DO
      Value := BuildAdd(Op1^.Value, Op2^.Value, FALSE) ;
      Type  := real
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
         Type   := integer ;
         Value  := BuildSub(Op2^.Value, Op1^.Value, FALSE) ;
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
   IF Op1^.Type=integer
   THEN
      Push(Op1) ;
      ConvertIntToReal ;
      Op1 := Pop()
   END ;
   IF Op2^.Type=integer
   THEN
      Push(Op2) ;
      ConvertIntToReal ;
      Op2 := Pop()
   END ;
   Temp := New() ;
   WITH Temp^ DO
      Value := BuildSub(Op2^.Value, Op1^.Value, FALSE) ;
      Type  := real
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

PROCEDURE Mult ;
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
         Type  := integer ;
         Value := BuildMult(Op2^.Value, Op1^.Value, FALSE)
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END Mult ;


(*
   RealMult - multiplies two numbers. One of which is a Real.
*)

PROCEDURE RealMult (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF Op1^.Type=integer
   THEN
      Push(Op1) ;
      ConvertIntToReal ;
      Op1 := Pop()
   END ;
   IF Op2^.Type=integer
   THEN
      Push(Op2) ;
      ConvertIntToReal ;
      Op2 := Pop()
   END ;
   Temp := New() ;     (* as it is a temp *)
   WITH Temp^ DO
      Value := BuildMult(Op2^.Value, Op1^.Value, FALSE) ;
      Type  := real
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
         Type  := integer ;
         Value := BuildDiv(Op2^.Value, Op1^.Value, FALSE)
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
   IF Op1^.Type=integer
   THEN
      Push(Op1) ;
      ConvertIntToReal ;
      Op1 := Pop()
   END ;
   IF Op2^.Type=integer
   THEN
      Push(Op2) ;
      ConvertIntToReal ;
      Op2 := Pop()
   END ;
   Temp := New() ;     (* as it is a temp *)
   WITH Temp^ DO
      Value := BuildDiv(Op2^.Value, Op1^.Value, FALSE) ;
      Type  := real
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
      WriteError('cannot yet perform MOD on REALs') ;
   ELSE
      Temp := New() ;     (* as it is a temp *)
      WITH Temp^ DO
         Type  := integer ;
         Value := BuildMod(Op2^.Value, Op1^.Value, FALSE)
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END Mod ;


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
 
PROCEDURE Equ () : BOOLEAN ;
VAR
   Op1, Op2: PtrToValue ;
   result  : BOOLEAN ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   result := AreConstantsEqual(Op1^.Value, Op2^.Value) ;
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

PROCEDURE NotEqu () : BOOLEAN ;
BEGIN
   RETURN( NOT Equ() )
END NotEqu ;


(*
   Less - returns true if Op2 < Op1
          are not identical.
 
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
 
PROCEDURE Less () : BOOLEAN ;
VAR
   res   : PtrToValue ;
   result: BOOLEAN ;
BEGIN
   Sub ;
   res := Pop() ;
   result := DetermineSign(res^.Value)=-1 ;
   Dispose(res) ;
   RETURN( result )
END Less ;


(*
   Gre - returns true if Op2 > Op1
         are not identical.

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

PROCEDURE Gre () : BOOLEAN ;
VAR
   res   : PtrToValue ;
   result: BOOLEAN ;
BEGIN
   Sub ;
   res := Pop() ;
   result := DetermineSign(res^.Value)=1 ;
   Dispose(res) ;
   RETURN( result )
END Gre ;


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

PROCEDURE LessEqu () : BOOLEAN ;
VAR
   res   : PtrToValue ;
   result: BOOLEAN ;
BEGIN
   Sub ;
   res := Pop() ;
   result := DetermineSign(res^.Value) <= 0 ;
   Dispose(res) ;
   RETURN( result )
END LessEqu ;


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

PROCEDURE GreEqu () : BOOLEAN ;
VAR
   res   : PtrToValue ;
   result: BOOLEAN ;
BEGIN
   Sub ;
   res := Pop() ;
   result := DetermineSign(res^.Value) >= 0 ;
   Dispose(res) ;
   RETURN( result )
END GreEqu ;


(*
   Bit -  performs 1 shifted right by the value of the number on the stack.

                 The Stack:

                 Entry             Exit

          Ptr ->                                 <- Ptr

                 +------------+    +------------+
                 | Op1        |    | 1 << Op1   |
                 |------------|    |------------|

*)

PROCEDURE Bit ;
VAR
   Op1: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   IF Op1^.Type=integer
   THEN
      Op1^.Value := BuildLSL(GetIntegerOne(), Op1^.Value, FALSE)
   ELSE
      (* this should be trapped earlier on I suspect, if not then convert this to InternalError *)
      InternalError('cannot perform left shift bit operations on a REAL', __FILE__, __LINE__)
   END ;
   Push(Op1)
END Bit ;


(*
   BitRange -  evaluates the mask defined by Op1..Op2

                    The Stack:

                    Entry             Exit

               Ptr ->
                      +------------+
                      | Op1        |                   <- Ptr
                      |------------|    +------------+
                      | Op2        |    | Op1..Op2   |
                      |------------|    |------------|
*)

PROCEDURE BitRange ;
VAR
   i  : CARDINAL ;
   Op1,
   Op2: PtrToValue ;
BEGIN
   Op1 := InitValue() ;
   Op2 := InitValue() ;
   PopInto(Op1) ;
   PopInto(Op2) ;
   PushFrom(Op2) ;
   PushFrom(Op1) ;
   IF Gre()
   THEN
      PushFrom(Op2) ;
      PushFrom(Op1) ;
      PopInto(Op2) ;
      PopInto(Op1)
   END ;
   PushFrom(Op2) ;
   PushCard(GetBitsPerWord()) ;
   IF Less()
   THEN
      (*
         we encode the following using our stack primitives
         FOR i := Op1 TO Op2 DO
            PushCard(i) ;
            Bit ;
            Add
         END
      *)
      (* in correct bitset range *)
      PushCard(0) ;  (* running total *)
      PushFrom(Op1) ;
      PushFrom(Op1) ;
      PushFrom(Op2) ;
      WHILE LessEqu() DO
         PopInto(Op1) ;
         PushFrom(Op1) ;
         Bit ;
         Add ;
         (* now increment Op1 *)
         PushFrom(Op1) ;
         PushCard(1) ;
         Add ;
         PopInto(Op1) ;
         PushFrom(Op1) ;
         PushFrom(Op1) ;
         PushFrom(Op2)
      END ;
      Dispose(Pop()) ;
      (* now we are left with the running total *)
   ELSE
      WriteError('bitrange exceeds wordlength')
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END BitRange ;


(*
   SetIn - returns true if the Op2 IN Op1

           The Stack:

           Entry             Exit

    Ptr ->
           +------------+
           | Op1        |
           |------------|
           | Op2        |
           |------------|    Empty

           RETURN( Op2 IN Op1 )
*)

PROCEDURE SetIn () : BOOLEAN ;
VAR
   Op1, Op2: PtrToValue ;
   result  : BOOLEAN ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   result := NOT AreConstantsEqual(BuildIfIn(Op2^.Value, Op1^.Value), GetIntegerZero()) ;
   Dispose(Op1) ;
   Dispose(Op2) ;
   RETURN( result )
END SetIn ;


(*
   SetOr -  performs an inclusive OR of the top two elements on the stack.

            The Stack:

            Entry             Exit

     Ptr ->
            +------------+
            | Op1        |                   <- Ptr
            |------------|    +------------+
            | Op2        |    | Op2 + Op1  |
            |------------|    |------------|

*)

PROCEDURE SetOr ;
VAR
   Result,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   Result := New() ;
   WITH Result^ DO
      Value := BuildLogicalOr(Op2^.Value, Op1^.Value, FALSE) ;
      Type  := integer ;
   END ;
   Push(Result) ;
   Dispose(Op1) ;
   Dispose(Op2)
END SetOr ;


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

PROCEDURE SetAnd ;
VAR
   Result,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   Result := New() ;
   WITH Result^ DO
      Value := BuildLogicalAnd(Op2^.Value, Op1^.Value, FALSE) ;
      Type  := integer ;
   END ;
   Push(Result) ;
   Dispose(Op1) ;
   Dispose(Op2)
END SetAnd ;


(*
   SetDifference - performs a set difference of the top two elements on the stack.

                   The Stack:

                   Entry             Exit

            Ptr ->
                   +------------+
                   | Op1        |                   <- Ptr
                   |------------|    +------------+
                   | Op2        |    | Op2 - Op1  |
                   |------------|    |------------|
*)

PROCEDURE SetDifference ;
VAR
   Result,
   Op1, Op2: PtrToValue ;
BEGIN
   (* firstly flip all bits of the top operand *)
   PushInt(-1) ;
   SetSymmetricDifference ;
   Op1 := Pop() ;
   Op2 := Pop() ;
   Result := New() ;
   WITH Result^ DO
      Value := BuildLogicalAnd(Op2^.Value, Op1^.Value, FALSE) ;
      Type  := integer ;
   END ;
   Push(Result) ;
   Dispose(Op1) ;
   Dispose(Op2)
END SetDifference ;


(*
   SetSymmetricDifference - performs a set difference of the top two elements on the stack.

                            The Stack:

                            Entry             Exit

                     Ptr ->
                            +------------+
                            | Op1        |                   <- Ptr
                            |------------|    +------------+
                            | Op2        |    | Op2 - Op1  |
                            |------------|    |------------|
*)

PROCEDURE SetSymmetricDifference ;
VAR
   Result,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   Result := New() ;
   WITH Result^ DO
      Value := BuildSymmetricDifference(Op2^.Value, Op1^.Value, FALSE) ;
      Type  := integer ;
   END ;
   Push(Result) ;
   Dispose(Op1) ;
   Dispose(Op2)
END SetSymmetricDifference ;


(*
   Init - initialises the stack and the free list.
*)

PROCEDURE Init ;
BEGIN
   FreeList   := NIL ;
   TopOfStack := NIL
END Init ;


BEGIN
   Init
END M2ALU.
