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

IMPLEMENTATION MODULE StringConvert ;


FROM DynamicStrings IMPORT String, InitString, InitStringChar, Mark, ConCat,
                           Slice, Index, char, Assign, Length, Mult,
                           RemoveWhitePrefix, KillString ;


(*
   Max - 
*)

PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   Min - 
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


(*
   IsDecimalDigitValid - returns the TRUE if, ch, is a base legal decimal digit.
                         If legal then the value is appended numerically onto, c.
*)

PROCEDURE IsDecimalDigitValid (ch: CHAR; base: CARDINAL; VAR c: CARDINAL) : BOOLEAN ;
BEGIN
   IF (ch>='0') AND (ch<='9') AND (ORD(ch)-ORD('0')<base)
   THEN
      c := c*base + (ORD(ch)-ORD('0')) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsDecimalDigitValid ;


(*
   IsHexidecimalDigitValid - returns the TRUE if, ch, is a base legal hexidecimal digit.
                             If legal then the value is appended numerically onto, c.
*)

PROCEDURE IsHexidecimalDigitValid (ch: CHAR; base: CARDINAL; VAR c: CARDINAL) : BOOLEAN ;
BEGIN
   IF (ch>='a') AND (ch<='f') AND (ORD(ch)-ORD('a')+10<base)
   THEN
      c := c*base + (ORD(ch)-ORD('a')+10) ;
      RETURN( TRUE )
   ELSIF (ch>='A') AND (ch<='F') AND (ORD(ch)-ORD('F')+10<base)
   THEN
      c := c*base + (ORD(ch)-ORD('F')+10) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsHexidecimalDigitValid ;


(*
   IntegerToString - converts INTEGER, i, into a String. The field with can be specified
                     if non zero. Leading characters are defined by padding and this
                     function will prepend a + if sign is set to TRUE.
                     The base allows the caller to generate binary, octal, decimal, hexidecimal
                     numbers. The value of lower is only used when hexidecimal numbers are
                     generated and if TRUE then digits abcdef are used, and if FALSE then ABCDEF
                     are used.
*)

PROCEDURE IntegerToString (i: INTEGER; width: CARDINAL; padding: CHAR; sign: BOOLEAN;
                           base: CARDINAL; lower: BOOLEAN) : String ;
VAR
   s: String ;
BEGIN
   IF i<0
   THEN
      IF i=MIN(INTEGER)
      THEN
         IF width>0
         THEN
            RETURN( ConCat(IntegerToString(i DIV 10, width-1, padding, sign, base, lower),
                           IntegerToString(i MOD 10, 0, ' ', FALSE, base, lower)) )
         ELSE
            RETURN( ConCat(IntegerToString(i DIV 10, 0, padding, sign, base, lower),
                           IntegerToString(i MOD 10, 0, ' ', FALSE, base, lower)) )
         END
      ELSE
         s := InitString('-')
      END ;
      i := -i
   ELSE
      IF sign
      THEN
         s := InitString('+')
      ELSE
         s := InitString('')
      END
   END ;
   IF i>base-1
   THEN
      s := ConCat(ConCat(s, IntegerToString(i DIV 10, 0, ' ', FALSE, base, lower)),
                  IntegerToString(i MOD 10, 0, ' ', FALSE, base, lower))
   ELSE
      IF i<=9
      THEN
         s := ConCat(s, InitStringChar(CHR(i+ORD('0'))))
      ELSE
         IF lower
         THEN
            s := ConCat(s, InitStringChar(CHR(i+ORD('a')-10)))
         ELSE
            s := ConCat(s, InitStringChar(CHR(i+ORD('A')-10)))
         END
      END
   END ;
   IF width>Length(s)
   THEN
      RETURN( ConCat(Mult(InitStringChar(padding), width-Length(s)), s) )
   END ;
   RETURN( s )
END IntegerToString ;


(*
   CardinalToString - converts CARDINAL, c, into a String. The field with can be specified
                      if non zero. Leading characters are defined by padding.
                      The base allows the caller to generate binary, octal, decimal, hexidecimal
                      numbers. The value of lower is only used when hexidecimal numbers are
                      generated and if TRUE then digits abcdef are used, and if FALSE then ABCDEF
                      are used.
*)

PROCEDURE CardinalToString (c: CARDINAL; width: CARDINAL; padding: CHAR;
                            base: CARDINAL; lower: BOOLEAN) : String ;
VAR
   s: String ;
BEGIN
   s := InitString('') ;
   IF c>base-1
   THEN
      s := ConCat(ConCat(s, CardinalToString(c DIV 10, 0, ' ', base, lower)),
                  CardinalToString(c MOD 10, 0, ' ', base, lower))
   ELSE
      IF c<=9
      THEN
         s := ConCat(s, InitStringChar(CHR(c+ORD('0'))))
      ELSE
         IF lower
         THEN
            s := ConCat(s, InitStringChar(CHR(c+ORD('a')-10)))
         ELSE
            s := ConCat(s, InitStringChar(CHR(c+ORD('A')-10)))
         END
      END
   END ;
   IF width>Length(s)
   THEN
      RETURN( ConCat(Mult(InitStringChar(padding), width-Length(s)), s) )
   END ;
   RETURN( s )
END CardinalToString ;


(*
   StringToInteger - converts a string, s, of, base, into an INTEGER.
                     Leading white space is ignored. It stops converting
                     when either the string is exhausted or if an illegal
                     numeral is found.
                     The parameter found is set TRUE if a number was found.
*)

PROCEDURE StringToInteger (s: String; base: CARDINAL; VAR found: BOOLEAN) : INTEGER ;
VAR
   n, l    : CARDINAL ;
   c       : CARDINAL ;
   negative: BOOLEAN ;
BEGIN
   s := RemoveWhitePrefix(s) ;    (* returns a new string, s *)
   l := Length(s) ;
   c := 0 ;
   n := 0 ;
   negative := FALSE ;
   IF n<l
   THEN
      (* parse leading + and - *)
      WHILE (char(s, n)='-') OR (char(s, n)='+') DO
         IF char(s, n)='-'
         THEN
            negative := NOT negative
         END ;
         INC(n)
      END ;
      WHILE (n<l) AND (IsDecimalDigitValid(char(s, n), base, c) OR
                       IsHexidecimalDigitValid(char(s, n), base, c)) DO
         found := TRUE ;
         INC(n)
      END
   END ;
   s := KillString(s) ;
   IF negative
   THEN
      RETURN( VAL(INTEGER, -Min(-MIN(INTEGER), c)) )
   ELSE
      RETURN( VAL(INTEGER, Min(MAX(INTEGER), c)) )
   END
END StringToInteger ;


(*
   StringToCardinal - converts a string, s, of, base, into an CARDINAL.
                      Leading white space is ignored. It stops converting
                      when either the string is exhausted or if an illegal
                      numeral is found.
                      The parameter found is set TRUE if a number was found.
*)

PROCEDURE StringToCardinal (s: String; base: CARDINAL; VAR found: BOOLEAN) : INTEGER ;
VAR
   n, l: CARDINAL ;
   c   : CARDINAL ;
BEGIN
   s := RemoveWhitePrefix(s) ;    (* returns a new string, s *)
   l := Length(s) ;
   c := 0 ;
   n := 0 ;
   IF n<l
   THEN
      (* parse leading + *)
      WHILE (char(s, n)='+') DO
         INC(n)
      END ;
      WHILE (n<l) AND (IsDecimalDigitValid(char(s, n), base, c) OR
                       IsHexidecimalDigitValid(char(s, n), base, c)) DO
         found := TRUE ;
         INC(n)
      END
   END ;
   s := KillString(s) ;
   RETURN( c )
END StringToCardinal ;


(*
   stoi - decimal string to INTEGER
*)

PROCEDURE stoi (s: String) : INTEGER ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToInteger(s, 10, found) )
END stoi ;


(*
   itos - integer to decimal string.
*)

PROCEDURE itos (i: INTEGER; width: CARDINAL; padding: CHAR; sign: BOOLEAN) : String ;
BEGIN
   RETURN( IntegerToString(i, width, padding, sign, 10, FALSE) )
END itos ;


(*
   ctos - cardinal to decimal string.
*)

PROCEDURE ctos (c: CARDINAL; width: CARDINAL; padding: CHAR) : String ;
BEGIN
   RETURN( CardinalToString(c, width, padding, 10, FALSE) )
END ctos ;


(*
   stoc - decimal string to CARDINAL
*)

PROCEDURE stoc (s: String) : CARDINAL ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToCardinal(s, 10, found) )
END stoc ;


(*
   hstoi - hexidecimal string to INTEGER
*)

PROCEDURE hstoi (s: String) : INTEGER ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToInteger(s, 16, found) )
END hstoi ;


(*
   ostoi - octal string to INTEGER
*)

PROCEDURE ostoi (s: String) : INTEGER ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToInteger(s, 8, found) )
END ostoi ;


(*
   bstoi - binary string to INTEGER
*)

PROCEDURE bstoi (s: String) : INTEGER ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToInteger(s, 2, found) )
END bstoi ;


(*
   hstoc - hexidecimal string to CARDINAL
*)

PROCEDURE hstoc (s: String) : CARDINAL ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToCardinal(s, 16, found) )
END hstoc ;


(*
   ostoc - octal string to CARDINAL
*)

PROCEDURE ostoc (s: String) : CARDINAL ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToCardinal(s, 8, found) )
END ostoc ;


(*
   bstoc - binary string to CARDINAL
*)

PROCEDURE bstoc (s: String) : CARDINAL ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToCardinal(s, 2, found) )
END bstoc ;


(* **********************************************************************
   R e a l    a n d    L o n g R e a l    c o n v e r s i o n
   ********************************************************************** *)


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


(*******************
(*
   LongrealToString - converts a LONGREAL number, Real, which has, TotalWidth, and
                      FractionWidth into a string.
*)

PROCEDURE LongRealToStr (x: LONGREAL; TotalWidth, FractionWidth: CARDINAL) : String ;
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
**********************)

(*
   StringToLongreal - returns a LONGREAL and sets found to TRUE if a legal number is seen.
*)

PROCEDURE StringToLongreal (s: String; VAR found: BOOLEAN) : LONGREAL ;
VAR
   i, l    : CARDINAL ;
   negative: BOOLEAN ;
   x,
   Fraction,
   Exponent: LONGREAL ;
BEGIN
   s := RemoveWhitePrefix(s) ;   (* new string is created *)
   l := Length(s) ;
   i := 0 ;
   negative := FALSE ;
   x := 0.0 ;
   IF i<l
   THEN
      (* parse leading + and - *)
      WHILE (char(s, i)='-') OR (char(s, i)='+') DO
         IF char(s, i)='-'
         THEN
            negative := NOT negative
         END ;
         INC(i)
      END ;

      (* firstly deal with the non fractional component, just like integer conversion *)
      WHILE (i<l) AND (char(s, i)>='0') AND (char(s, i)<='9') DO
         x := x*10.0+FLOAT(ORD(char(s, i))-ORD('0')) ;
         found := TRUE ;
         INC(i)
      END ;
      (* test for fractional component *)
      IF (i<l) AND (char(s, i)='.')
      THEN
         (* yes fractional component exists *)
         Exponent := 10.0 ;
         Fraction := 0.0 ;
         INC(i) ;  (* move over '.' *)
         WHILE (i<l) AND (char(s, i)>='0') AND (char(s, i)<='9') DO
            Fraction := Fraction+FLOAT(ORD(char(s, i))-ORD('0'))/Exponent ;
            found := TRUE ;
            Exponent := Exponent*10.0 ;
            INC(i)
         END ;
         (* and combine both components *)
         x := x+Fraction
      END
   END ;
   s := KillString(s) ;
   (* restore sign *)
   IF negative
   THEN
      RETURN( -x )
   ELSE
      RETURN( x )
   END
END StringToLongreal ;


(*
   rtos - 
*)

PROCEDURE rtos (r: REAL; TotalWidth, FractionWidth: CARDINAL) : String ;
BEGIN
   HALT ;
   RETURN ( NIL )
END rtos ;


(*
   stor - returns a REAL given a string.
*)

PROCEDURE stor (s: String) : REAL ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( VAL(REAL, StringToLongreal(s, found)) )
END stor ;


(*
   lrtos - 
*)

PROCEDURE lrtos (r: LONGREAL; TotalWidth, FractionWidth: CARDINAL) : String ;
BEGIN
   HALT ;
   RETURN ( NIL )
END lrtos ;


(*
   stolr - returns a LONGREAL given a string.
*)

PROCEDURE stolr (s: String) : LONGREAL ;
VAR
   found: BOOLEAN ;
BEGIN
   RETURN( StringToLongreal(s, found) )
END stolr ;


END StringConvert.
