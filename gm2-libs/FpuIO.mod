(* Copyright (C) 2001 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA *)
IMPLEMENTATION MODULE FpuIO ;


FROM StrIO IMPORT ReadString, WriteString, WriteLn ;
FROM StrLib IMPORT StrLen, StrRemoveWhitePrefix ;
FROM ASCII IMPORT nul ;


(* %%%FORWARD%%%


   %%%FORWARD%%% *)


CONST
   MaxLineLength = 100 ;
   MaxDigits     = 100 ;


(*
   Max - 
*)

PROCEDURE Max (i, j: INTEGER) : INTEGER ;
BEGIN
   IF i>j
   THEN
      RETURN( i )
   ELSE
      RETURN( j )
   END
END Max ;


(*
   ToThePower10 - returns a LONGREAL containing the value of v * 10^power.
*)

PROCEDURE ToThePower10 (v: LONGREAL; power: CARDINAL) : LONGREAL;
VAR 
   i: CARDINAL;
BEGIN
   i := 0 ;
   WHILE i<power DO
      v := v * 10.0 ;
      INC(i)
   END ;
   RETURN( VAL(REAL, v) )
END ToThePower10 ;


(*
   DetermineSafeTruncation - we wish to use TRUNC when converting REAL/LONGREAL into a string for
                             the non fractional component. However we need a simple method to
                             determine the maximum safe truncation value.
*)

PROCEDURE DetermineSafeTruncation () : CARDINAL ;
VAR
   MaxPowerOfTen: REAL ;
   LogPower     : CARDINAL ;
BEGIN
   MaxPowerOfTen := 1.0 ;
   LogPower      := 0 ;
   WHILE MaxPowerOfTen*10.0<FLOAT(MAX(INTEGER) DIV 10) DO
      MaxPowerOfTen := MaxPowerOfTen * 10.0 ;
      INC(LogPower)
   END ;
   RETURN( LogPower )
END DetermineSafeTruncation ;



PROCEDURE ReadReal (VAR x: REAL) ;
VAR
   a: ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   ReadString(a) ;
   StrToReal(a, x)
END ReadReal ;


(*
   WriteReal - converts a REAL number, x, which has a, TotalWidth, and
               FractionWidth into, string, a.
*)

PROCEDURE WriteReal (x: REAL; TotalWidth, FractionWidth: CARDINAL) ;
VAR
   a: ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   RealToStr(x, TotalWidth, FractionWidth, a) ;
   WriteString(a)
END WriteReal ;


PROCEDURE StrToReal (a: ARRAY OF CHAR ; VAR x: REAL) ;
VAR
   lr: LONGREAL ;
BEGIN
   StrToLongReal(a, lr) ;  (* let StrToLongReal do the work and we convert the result back to REAL *)
   x := lr
END StrToReal ;


PROCEDURE ReadLongReal (VAR x: LONGREAL) ;
VAR
   a: ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   ReadString( a ) ;
   StrToLongReal( a, x )
END ReadLongReal ;


(*
   WriteLongReal - converts a LONGREAL number, x, which has a, TotalWidth, and
                   FractionWidth into a string.
*)

PROCEDURE WriteLongReal (x: LONGREAL; TotalWidth, FractionWidth: CARDINAL) ;
VAR
   a: ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   LongRealToStr(x, TotalWidth, FractionWidth, a) ;
   WriteString(a)
END WriteLongReal ;


PROCEDURE StrToLongReal (a: ARRAY OF CHAR ; VAR x: LONGREAL) ;
VAR
   i, high   : CARDINAL ;
   IsNegative: BOOLEAN ;
   Fraction  : LONGREAL ;
   Exponent  : LONGREAL ;
BEGIN
   StrRemoveWhitePrefix(a, a) ;
   high := StrLen(a) ;
   i := 0 ;
   IF (i<high) AND (a[i]='-')
   THEN
      IsNegative := TRUE ;
      INC(i)
   ELSE
      IsNegative := FALSE
   END ;
   (* firstly deal with the non fractional component, just like integer conversion *)
   x := 0.0 ;
   WHILE (i<high) AND (a[i]>='0') AND (a[i]<='9') DO
      x := x*10.0+FLOAT(ORD(a[i])-ORD('0')) ;
      INC(i)
   END ;
   IF i<high
   THEN
      (* test for fractional component *)
      IF a[i]='.'
      THEN
         (* yes fractional component exists *)
         Exponent := 10.0 ;
         Fraction := 0.0 ;
         INC(i) ;  (* move over '.' *)
         WHILE (i<high) AND (a[i]>='0') AND (a[i]<='9') DO
            Fraction := Fraction+FLOAT(ORD(a[i])-ORD('0'))/Exponent ;
            Exponent := Exponent*10.0 ;
            INC(i)
         END ;
         (* and combine both components *)
         x := x+Fraction
      END
   END ;
   (* restore sign *)
   IF IsNegative
   THEN
      x := -x
   END
END StrToLongReal ;


(*
   Add - places, ch, into, a, at index, i, providing, i<High
*)

PROCEDURE Add (VAR a: ARRAY OF CHAR; High: CARDINAL; i: CARDINAL; ch: CHAR) ;
BEGIN
   IF i<High
   THEN
      a[i] := ch
   END
END Add ;


(*
   IntegerToStr - converts an INTEGER, i, into a string, a, at position, index.
                  It returns the number of characters added.
                  It assumes that index<=HIGH(a) at entry.
*)

PROCEDURE IntegerToStr (i: INTEGER; VAR a: ARRAY OF CHAR; index: CARDINAL) : CARDINAL ;
VAR
   start,
   high,
   added: CARDINAL ;
   ch   : CHAR ;
BEGIN
   added := 0 ;
   high  := HIGH(a) ;
   start := index ;
   REPEAT
      a[index] := CHR(CARDINAL(i MOD 10)+ORD('0')) ;
      i := i DIV 10 ;
      INC(index) ;
      INC(added)
   UNTIL (i=0) OR (index>high) ;
   (* now reverse to the correct way *)
   DEC(index) ;
   WHILE start<index DO
      ch       := a[start] ;
      a[start] := a[index] ;
      a[index] := ch ;
      DEC(index) ;
      INC(start)
   END ;
   RETURN( added )
END IntegerToStr ;


(*
   RealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
               FractionWidth into a string.
*)

PROCEDURE RealToStr (x: REAL; TotalWidth, FractionWidth: CARDINAL; VAR a: ARRAY OF CHAR) ;
VAR
   lr: LONGREAL ;
BEGIN
   lr := x ;
   LongRealToStr(lr, TotalWidth, FractionWidth, a)
END RealToStr ;


(*
   LongRealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
                   FractionWidth into a string.
*)

PROCEDURE LongRealToStr (x: LONGREAL; TotalWidth, FractionWidth: CARDINAL; VAR a: ARRAY OF CHAR) ;
VAR
   TruncedX        : INTEGER;
   NonTruncedDigits: CARDINAL ;

   i,
   aIndex,
   BufIndex        : CARDINAL ;
   Buffer          : ARRAY [0..MaxDigits] OF CHAR;
   IsNegative      : BOOLEAN;
   IntegerWidth    : CARDINAL ;
   SignWidth       : CARDINAL ;

   LogPower        : CARDINAL ;
   MaxPower        : LONGREAL ;
   High            : CARDINAL ;
BEGIN
   High     := HIGH(a) ;
   LogPower := DetermineSafeTruncation() ;
   MaxPower := ToThePower10(1.0, LogPower) ;

   IF x<0.0
   THEN
      x          := -x ;
      SignWidth  := 1;
      IsNegative := TRUE
   ELSE
      SignWidth  := 0;
      IsNegative := FALSE
   END ;

   BufIndex := 0 ;
   REPEAT
      (* keep dividing x by 10.0 until we can safely use the TRUNC operator *)
      NonTruncedDigits := 0 ;
      WHILE x/ToThePower10(1.0, NonTruncedDigits) >= FLOAT(MAX(INTEGER) DIV 10) DO
         INC(NonTruncedDigits)
      END ;
      IF NonTruncedDigits>0
      THEN
         (* only divide x if we need, avoid rounding *)
         x := x / ToThePower10(1.0, NonTruncedDigits)
      END ;

      (*  Now store the non-fractional digits *)
      TruncedX := TRUNC(x) ;
      x := x - VAL(LONGREAL, TruncedX) ;

      INC(BufIndex, IntegerToStr(TruncedX, Buffer, BufIndex)) ;
      IF NonTruncedDigits>0
      THEN
         (* now restore x to its original magnitude *)
         x := ToThePower10(x, NonTruncedDigits)
      END
   UNTIL NonTruncedDigits = 0 ;

   IntegerWidth := Max(INTEGER(BufIndex), INTEGER(TotalWidth)-INTEGER(FractionWidth)) ;

   (* add leading spaces *)
   aIndex := 0 ;
   WHILE aIndex<IntegerWidth-BufIndex-SignWidth DO
      Add(a, High, aIndex, ' ') ;
      INC(aIndex)
   END ;

   (* now add the sign *)
   IF IsNegative
   THEN
      Add(a, High, aIndex, '-') ;
      INC(aIndex)
   END ;

   (* now add integer component of x *)
   i := 0 ;
   WHILE i<BufIndex DO
      Add(a, High, aIndex, Buffer[i]) ;
      INC(i) ;
      INC(aIndex)
   END ;

   IF IntegerWidth<TotalWidth
   THEN
      Add(a, High, aIndex, '.') ;
      INC(aIndex) ;
      WHILE aIndex<=TotalWidth DO
         x := x * MaxPower ;
         TruncedX := TRUNC(x) ;
         x := x - VAL(LONGREAL, TruncedX) ;

         BufIndex := IntegerToStr(TruncedX, Buffer, 0) ;

         (* add leading zero's *)
         i := BufIndex ;
         WHILE (i<LogPower) AND (aIndex<High) DO
            Add(a, High, aIndex, '0') ;
            INC(aIndex) ;
            INC(i)
         END ;
         (* and add contents of the buffer *)
         i := 0 ;
         REPEAT
            Add(a, High, aIndex, Buffer[i]) ;
            INC(i) ;
            INC(aIndex)
         UNTIL i=BufIndex
      END
   END ;
   Add(a, High, aIndex, nul)
END LongRealToStr ;


PROCEDURE ReadLongInt (VAR x: LONGINT) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   ReadString( a ) ;
   StrToLongInt( a, x )
END ReadLongInt ;


PROCEDURE WriteLongInt (x: LONGINT; n: CARDINAL) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   LongIntToStr( x, n, a ) ;
   WriteString( a )
END WriteLongInt ;


PROCEDURE LongIntToStr (x: LONGINT; n: CARDINAL ; VAR a: ARRAY OF CHAR) ;
VAR
   i, j,
   Higha    : CARDINAL ;
   buf      : ARRAY [1..MaxDigits] OF CARDINAL ;
   Negative : BOOLEAN ;
BEGIN
   IF x<0
   THEN
      Negative := TRUE ;
      IF n>0
      THEN
         DEC(n)
      END ;
      x := -x
   ELSE
      Negative := FALSE ;
   END ;
   i := 0 ;
   REPEAT
      INC(i) ;
      IF i>MaxDigits
      THEN
         WriteString('increase MaxDigits in FpuIO') ; WriteLn ;
         HALT
      END ;
      buf[i] := VAL(CARDINAL, x MOD 10) ;
      x := x DIV 10 ;
   UNTIL x=0 ;
   j := 0 ;
   Higha := HIGH(a) ;
   WHILE (n>i) AND (j<=Higha) DO
      a[j] := ' ' ;
      INC(j) ;
      DEC(n)
   END ;
   IF Negative
   THEN
      a[j] := '-' ;
      INC(j)
   END ;
   WHILE (i#0) AND (j<=Higha) DO
      a[j] := CHR( buf[i] + ORD('0') ) ;
      INC(j) ;
      DEC(i)
   END ;
   IF j<=Higha
   THEN
      a[j] := nul
   END
END LongIntToStr ;


PROCEDURE StrToLongInt (a: ARRAY OF CHAR ; VAR x: LONGINT) ;
VAR
   i        : CARDINAL ;
   finished,
   Negative : BOOLEAN ;
   higha    : CARDINAL ;
BEGIN
   StrRemoveWhitePrefix(a, a) ;
   higha := StrLen(a) ;
   i := 0 ;
   Negative := FALSE ;
   finished := FALSE ;
   REPEAT
      IF i<higha
      THEN
         IF a[i]='-'
         THEN
            INC(i) ;
            Negative := NOT Negative
         ELSIF (a[i]<'0') OR (a[i]>'9')
         THEN
            INC(i)
         END
      ELSE
         finished := TRUE
      END
   UNTIL finished ;
   x := 0 ;
   IF i<=higha
   THEN
      finished := FALSE ;
      REPEAT
         IF Negative
         THEN
            x := 10*x - INTEGER(ORD(a[i])-ORD('0'))
         ELSE
            x := 10*x + INTEGER(ORD(a[i])-ORD('0'))
         END ;
         IF i<higha
         THEN
            INC(i) ;
            IF (a[i]<'0') OR (a[i]>'9')
            THEN
               finished := TRUE
            END
         ELSE
            finished := TRUE
         END
      UNTIL finished
   END
END StrToLongInt ;


END FpuIO.
