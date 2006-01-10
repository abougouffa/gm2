(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
IMPLEMENTATION MODULE M2Constants ;

FROM M2ALU IMPORT Equ ;

FROM SymbolTable IMPORT MakeConstLit, PushValue, IsConst, GetType, IsValueSolved, MakeConstVar, PopValue,
                        IsConstString, GetStringLength, IsConstSet, PutConstSet, PutConst ;

FROM M2ALU IMPORT IsValueTypeSet, GetSetValueType ;
FROM NameKey IMPORT MakeKey, makekey ;
FROM NumberIO IMPORT CardToStr ;
FROM DynamicStrings IMPORT String, string, InitString, KillString, Mark ;
FROM FormatStrings IMPORT Sprintf1 ;

VAR
   ConstNo           : CARDINAL ;  (* count of generated constants *)
   ZeroCard, ZeroReal,
   OneCard, OneReal,
   TwoCard, TwoReal  : CARDINAL ;


(*
   FindCommonValue - examines the constant, Sym, if it is really 0, 1 or 2 then these
                     predefined constants are returned. This is only really a debugging aid
                     as these constants are common and can be read rather than the _constXXX
                     names that we must otherwise generate.
*)

PROCEDURE FindCommonValue (tokenno: CARDINAL; Sym: CARDINAL) : CARDINAL ;
BEGIN
   IF NOT IsConstSet(Sym)
   THEN
      IF IsZero(tokenno, Sym)
      THEN
         RETURN( ZeroCard )
      ELSIF IsOne(tokenno, Sym)
      THEN
         RETURN( OneCard )
      ELSIF IsTwo(tokenno, Sym)
      THEN
         RETURN( TwoCard )
      END
   END ;
   RETURN( Sym )   (* we don't know about this value *)
END FindCommonValue ;


(*
   MakeNewConstFromValue - makes a new constant given a value on the M2ALU stack.
*)

PROCEDURE MakeNewConstFromValue (tokenno: CARDINAL) : CARDINAL ;
VAR
   Sym : CARDINAL ;
   Name: String ;
BEGIN
   Name := Sprintf1(Mark(InitString('_const%d')), ConstNo) ;
   INC(ConstNo) ;
   Sym := MakeConstVar(makekey(string(Name))) ;
   IF IsValueTypeSet()
   THEN
      PutConstSet(Sym) ;
      PutConst(Sym, GetSetValueType())
   END ;
   PopValue(Sym) ;
   Name := KillString(Name) ;
   RETURN( FindCommonValue(tokenno, Sym) )
END MakeNewConstFromValue ;


(*
   IsSame - returns TRUE if the symbols, s1, and, s2, are the same or
            have the same constant value and same type.
*)

PROCEDURE IsSame (tokenno: CARDINAL; s1, s2: CARDINAL) : BOOLEAN ;
BEGIN
   IF s1=s2
   THEN
      RETURN( TRUE )   (* easy decision.. *)
   ELSE
      IF IsConst(s1) AND IsConst(s2) AND (GetType(s1)=GetType(s2)) AND
         IsValueSolved(s1) AND IsValueSolved(s2) AND
         ((NOT IsConstString(s1)) OR (GetStringLength(s1)=1)) AND
         ((NOT IsConstString(s2)) OR (GetStringLength(s2)=1))
      THEN
         PushValue(s1) ;
         PushValue(s2) ;
         RETURN( Equ(tokenno) )
      ELSE
         RETURN( FALSE )    (* not constant or a different type therefore we do not know *)
      END
   END
END IsSame ;


(*
   IsZero - returns TRUE if symbol, s, represents the value zero.
*)

PROCEDURE IsZero (tokenno: CARDINAL; s: CARDINAL) : BOOLEAN ;
BEGIN
   IF (s=ZeroCard) OR (s=ZeroReal)
   THEN
      RETURN( TRUE )
   ELSIF IsConst(s)
   THEN
      PushValue(s) ;
      PushValue(ZeroCard) ;
      RETURN( Equ(tokenno) )
   ELSE
      RETURN( FALSE )
   END
END IsZero ;


(*
   IsOne - returns TRUE if symbol, s, represents the value one.
*)

PROCEDURE IsOne (tokenno: CARDINAL; s: CARDINAL) : BOOLEAN ;
BEGIN
   IF (s=OneCard) OR (s=OneReal)
   THEN
      RETURN( TRUE )
   ELSIF IsConst(s)
   THEN
      PushValue(s) ;
      PushValue(OneCard) ;
      RETURN( Equ(tokenno) )
   ELSE
      RETURN( FALSE )
   END
END IsOne ;


(*
   IsTwo - returns TRUE if symbol, s, represents the value two.
*)

PROCEDURE IsTwo (tokenno: CARDINAL; s: CARDINAL) : BOOLEAN ;
BEGIN
   IF (s=TwoCard) OR (s=TwoReal)
   THEN
      RETURN( TRUE )
   ELSIF IsConst(s)
   THEN
      PushValue(s) ;
      PushValue(TwoCard) ;
      RETURN( Equ(tokenno) )
   ELSE
      RETURN( FALSE )
   END
END IsTwo ;


(*
   Init - initialize the constants.
*)

PROCEDURE Init ;
BEGIN
   ConstNo  := 0 ;
   ZeroCard := MakeConstLit(MakeKey('0')) ;
   OneCard  := MakeConstLit(MakeKey('1')) ;
   TwoCard  := MakeConstLit(MakeKey('2')) ;
   ZeroReal := MakeConstLit(MakeKey('0.0')) ;
   OneReal  := MakeConstLit(MakeKey('1.0')) ;
   TwoReal  := MakeConstLit(MakeKey('2.0'))
END Init ;


BEGIN
   Init
END M2Constants.
