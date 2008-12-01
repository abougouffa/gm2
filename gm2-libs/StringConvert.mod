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

IMPLEMENTATION MODULE StringConvert ;


FROM DynamicStrings IMPORT String, InitString, InitStringChar, Mark, ConCat,
                           Slice, Index, char, Assign, Length, Mult,
                           RemoveWhitePrefix, ConCatChar, KillString ;


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
   LongMin - returns the smallest LONGCARD
*)

PROCEDURE LongMin (a, b: LONGCARD) : LONGCARD ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END LongMin ;


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
      c := c*base + (ORD(ch)-ORD('A')+10) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsHexidecimalDigitValid ;


(*
   IsDecimalDigitValidLong - returns the TRUE if, ch, is a base legal decimal digit.
                             If legal then the value is appended numerically onto, c.
*)

PROCEDURE IsDecimalDigitValidLong (ch: CHAR; base: CARDINAL;
                                   VAR c: LONGCARD) : BOOLEAN ;
BEGIN
   IF (ch>='0') AND (ch<='9') AND (ORD(ch)-ORD('0')<base)
   THEN
      c := c * VAL(LONGCARD, base + (ORD(ch)-ORD('0'))) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsDecimalDigitValidLong ;


(*
   IsHexidecimalDigitValidLong - returns the TRUE if, ch, is a base legal hexidecimal digit.
                                 If legal then the value is appended numerically onto, c.
*)

PROCEDURE IsHexidecimalDigitValidLong (ch: CHAR; base:  CARDINAL; VAR c: LONGCARD) : BOOLEAN ;
BEGIN
   IF (ch>='a') AND (ch<='f') AND (ORD(ch)-ORD('a')+10<base)
   THEN
      c := c * VAL(LONGCARD, base + (ORD(ch)-ORD('a')+10)) ;
      RETURN( TRUE )
   ELSIF (ch>='A') AND (ch<='F') AND (ORD(ch)-ORD('F')+10<base)
   THEN
      c := c * VAL(LONGCARD, base + (ORD(ch)-ORD('A')+10)) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsHexidecimalDigitValidLong ;


(*
   IsDecimalDigitValidShort - returns the TRUE if, ch, is a base legal decimal digit.
                              If legal then the value is appended numerically onto, c.
*)

PROCEDURE IsDecimalDigitValidShort (ch: CHAR; base: CARDINAL; VAR c: SHORTCARD) : BOOLEAN ;
BEGIN
   IF (ch>='0') AND (ch<='9') AND (ORD(ch)-ORD('0')<base)
   THEN
      c := c * VAL(SHORTCARD, base + (ORD(ch)-ORD('0'))) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsDecimalDigitValidShort ;


(*
   IsHexidecimalDigitValidShort - returns the TRUE if, ch, is a base legal hexidecimal digit.
                                  If legal then the value is appended numerically onto, c.
*)

PROCEDURE IsHexidecimalDigitValidShort (ch: CHAR; base: CARDINAL; VAR c: SHORTCARD) : BOOLEAN ;
BEGIN
   IF (ch>='a') AND (ch<='f') AND (ORD(ch)-ORD('a')+10<base)
   THEN
      c := c * VAL(SHORTCARD, base + (ORD(ch)-ORD('a')+10)) ;
      RETURN( TRUE )
   ELSIF (ch>='A') AND (ch<='F') AND (ORD(ch)-ORD('F')+10<base)
   THEN
      c := c * VAL(SHORTCARD, base + (ORD(ch)-ORD('A')+10)) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsHexidecimalDigitValidShort ;


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
   c: CARDINAL ;
BEGIN
   IF i<0
   THEN
      IF i=MIN(INTEGER)
      THEN
         (* remember that -15 MOD 4 = 1 in Modula-2 *)
         c := VAL(CARDINAL, ABS(i+1))+1 ;
         IF width>0
         THEN
            RETURN( ConCat(IntegerToString(-VAL(INTEGER, c DIV 10),
                                           width-1, padding, sign, base, lower),
                           IntegerToString(c MOD 10, 0, ' ', FALSE, base, lower)) )
         ELSE
            RETURN( ConCat(IntegerToString(-VAL(INTEGER, c DIV 10),
                                           0, padding, sign, base, lower),
                           IntegerToString(c MOD 10, 0, ' ', FALSE, base, lower)) )
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
   IF i>VAL(INTEGER, base)-1
   THEN
      s := ConCat(ConCat(s, IntegerToString(i DIV 10, 0, ' ', FALSE, base, lower)),
                  IntegerToString(i MOD 10, 0, ' ', FALSE, base, lower))
   ELSE
      IF i<=9
      THEN
         s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, i)+ORD('0'))))
      ELSE
         IF lower
         THEN
            s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, i)+ORD('a')-10)))
         ELSE
            s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, i)+ORD('A')-10)))
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
   LongIntegerToString - converts LONGINT, i, into a String. The field with
                         can be specified if non zero. Leading characters
                         are defined by padding and this function will
                         prepend a + if sign is set to TRUE.
                         The base allows the caller to generate binary,
                         octal, decimal, hexidecimal numbers.
                         The value of lower is only used when hexidecimal
                         numbers are generated and if TRUE then digits
                         abcdef are used, and if FALSE then ABCDEF are used.
*)

PROCEDURE LongIntegerToString (i: LONGINT; width: CARDINAL; padding: CHAR;
                               sign: BOOLEAN; base: CARDINAL; lower: BOOLEAN) : String ;

VAR
   s: String ;
   c: LONGCARD ;
BEGIN
   IF i<0
   THEN
      IF i=MIN(LONGINT)
      THEN
         (* remember that -15 MOD 4 is 1 in Modula-2, and although ABS(MIN(LONGINT)+1)
            is very likely MAX(LONGINT), it is safer not to assume this is the case *)
         c := VAL(LONGCARD, ABS(i+1))+1 ;
         IF width>0
         THEN
            RETURN( ConCat(LongIntegerToString(-VAL(LONGINT, c DIV 10),
                                               width-1, padding, sign, base, lower),
                           LongIntegerToString(c MOD 10, 0, ' ', FALSE, base, lower)) )
         ELSE
            RETURN( ConCat(LongIntegerToString(-VAL(LONGINT, c DIV 10),
                                               0, padding, sign, base, lower),
                           LongIntegerToString(c MOD 10, 0, ' ', FALSE, base, lower)) )
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
   IF i>VAL(LONGINT, base-1)
   THEN
      s := ConCat(ConCat(s, LongIntegerToString(i DIV 10, 0, ' ', FALSE, base, lower)),
                  LongIntegerToString(i MOD 10, 0, ' ', FALSE, base, lower))
   ELSE
      IF i<=9
      THEN
         s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, i)+ORD('0'))))
      ELSE
         IF lower
         THEN
            s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, i)+ORD('a')-10)))
         ELSE
            s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, i)+ORD('A')-10)))
         END
      END
   END ;
   IF width>Length(s)
   THEN
      RETURN( ConCat(Mult(InitStringChar(padding), width-Length(s)), s) )
   END ;
   RETURN( s )
END LongIntegerToString ;


(*
   StringToLongInteger - converts a string, s, of, base, into an LONGINT.
                         Leading white space is ignored. It stops converting
                         when either the string is exhausted or if an illegal
                         numeral is found.
                         The parameter found is set TRUE if a number was found.
*)

PROCEDURE StringToLongInteger (s: String; base: CARDINAL; VAR found: BOOLEAN) : LONGINT ;
VAR
   n, l    : CARDINAL ;
   c       : LONGCARD ;
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
      WHILE (n<l) AND (IsDecimalDigitValidLong(char(s, n), base, c) OR
                       IsHexidecimalDigitValidLong(char(s, n), base, c)) DO
         found := TRUE ;
         INC(n)
      END
   END ;
   s := KillString(s) ;
   IF negative
   THEN
      RETURN( -VAL(LONGINT, LongMin(VAL(LONGCARD, MAX(LONGINT))+1, c)) )
   ELSE
      RETURN( VAL(LONGINT, LongMin(MAX(LONGINT), c)) )
   END
END StringToLongInteger ;


(*
   StringToInteger - converts a string, s, of, base, into an INTEGER.
                     Leading white space is ignored. It stops converting
                     when either the string is exhausted or if an illegal
                     numeral is found.
                     The parameter found is set TRUE if a number was found.
*)

PROCEDURE StringToInteger (s: String; base: CARDINAL;
                           VAR found: BOOLEAN) : INTEGER ;
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
      RETURN( -VAL(INTEGER, Min(VAL(CARDINAL, MAX(INTEGER))+1, c)) )
   ELSE
      RETURN( VAL(INTEGER, Min(MAX(INTEGER), c)) )
   END
END StringToInteger ;


(*
   StringToCardinal - converts a string, s, of, base, into a CARDINAL.
                      Leading white space is ignored. It stops converting
                      when either the string is exhausted or if an illegal
                      numeral is found.
                      The parameter found is set TRUE if a number was found.
*)

PROCEDURE StringToCardinal (s: String; base: CARDINAL;
                            VAR found: BOOLEAN) : CARDINAL ;
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

PROCEDURE ToThePower10 (v: LONGREAL; power: INTEGER) : LONGREAL;
VAR 
   i: INTEGER ;
BEGIN
   i := 0 ;
   IF power>0
   THEN
      WHILE i<power DO
         v := v * 10.0 ;
         INC(i)
      END
   ELSE
      WHILE i>power DO
         v := v / 10.0 ;
         DEC(i)
      END
   END ;
   RETURN( v )
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


(*
   LongrealToString - converts a LONGREAL number, Real, which has,
                      TotalWidth, and FractionWidth into a string.
*)

PROCEDURE LongrealToString (x: LONGREAL;
                            TotalWidth, FractionWidth: CARDINAL) : String ;
VAR
   TruncedX        : INTEGER;
   NonTruncedDigits: CARDINAL ;

   s, Result       : String ;
   IsNegative      : BOOLEAN;
   IntegerWidth    : CARDINAL ;
   SignWidth       : CARDINAL ;

   LogPower        : CARDINAL ;
   MaxPower        : LONGREAL ;
BEGIN
   Result   := InitString('') ;
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

   REPEAT
      (* keep dividing x by 10.0 until we can safely use the TRUNC operator *)
      NonTruncedDigits := 0 ;
      WHILE x/ToThePower10(1.0, NonTruncedDigits) >= VAL(LONGREAL, MAX(INTEGER) DIV 10) DO
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

      Result := ConCat(Result, Mark(IntegerToString(TruncedX, 0, ' ',
                                                    FALSE, 10, FALSE))) ;
      IF NonTruncedDigits>0
      THEN
         (* now restore x to its original magnitude *)
         x := ToThePower10(x, NonTruncedDigits)
      END
   UNTIL NonTruncedDigits = 0 ;

   IntegerWidth := Max(Length(Result),
                       INTEGER(TotalWidth)-INTEGER(FractionWidth)) ;

   (* now add the sign *)
   IF IsNegative
   THEN
      Result := ConCat(InitString('-'), Mark(Result))
   END ;

   (* and add leading spaces *)
   IF IntegerWidth>Length(Result)
   THEN
      Result := ConCat(Mult(InitString(' '),
                            IntegerWidth-Length(Result)),
                       Mark(Result))
   END ;

   IF IntegerWidth<TotalWidth
   THEN
      Result := ConCatChar(Result, '.') ;
      WHILE Length(Result)<=TotalWidth DO
         x := x * MaxPower ;
         TruncedX := TRUNC(x) ;
         x := x - VAL(LONGREAL, TruncedX) ;

         s := IntegerToString(TruncedX, 0, ' ', FALSE, 10, FALSE) ;

         (* add leading zero's *)
         IF Length(s)<LogPower
         THEN
            s := ConCat(Mult(InitString('0'), LogPower-Length(s)), Mark(s))
         END ;
         Result := ConCat(Result, Mark(s))
      END
   END ;
   RETURN( Result )
END LongrealToString ;


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
   exponent: INTEGER ;
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
         x := x*10.0+VAL(LONGREAL, ORD(char(s, i))-ORD('0')) ;
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
            Fraction := Fraction+VAL(LONGREAL, ORD(char(s, i))-ORD('0'))/Exponent ;
            found := TRUE ;
            Exponent := Exponent*10.0 ;
            INC(i)
         END ;
         (* and combine both components *)
         x := x+Fraction
      END ;
      IF (char(s, i)='E') OR (char(s, i)='e')
      THEN
         INC(i) ;
         s := Slice(Mark(s), i, -1) ;
         exponent := StringToInteger(s, 10, found) ;
         IF found
         THEN
            x := ToThePower10(x, exponent)
         END
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


(*
   LongCardinalToString - converts LONGCARD, c, into a String. The field
                          width can be specified if non zero. Leading
                          characters are defined by padding.
                          The base allows the caller to generate binary,
                          octal, decimal, hexidecimal numbers.
                          The value of lower is only used when hexidecimal
                          numbers are generated and if TRUE then digits
                          abcdef are used, and if FALSE then ABCDEF are used.
*)

PROCEDURE LongCardinalToString (c: LONGCARD; width: CARDINAL; padding: CHAR;
                                base: CARDINAL; lower: BOOLEAN) : String ;
VAR
   s: String ;
BEGIN
   s := InitString('') ;
   IF c>VAL(LONGCARD, base-1)
   THEN
      s := ConCat(ConCat(s, LongCardinalToString(c DIV 10, 0, ' ', base, lower)),
                  LongCardinalToString(c MOD 10, 0, ' ', base, lower))
   ELSE
      IF c<=9
      THEN
         s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, c)+ORD('0'))))
      ELSE
         IF lower
         THEN
            s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, c)+ORD('a')-10)))
         ELSE
            s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, c)+ORD('A')-10)))
         END
      END
   END ;
   IF width>Length(s)
   THEN
      RETURN( ConCat(Mult(InitStringChar(padding), width-Length(s)), s) )
   END ;
   RETURN( s )
END LongCardinalToString ;


(*
   StringToLongCardinal - converts a string, s, of, base, into a LONGCARD.
                          Leading white space is ignored. It stops converting
                          when either the string is exhausted or if an illegal
                          numeral is found.
                          The parameter found is set TRUE if a number was found.
*)

PROCEDURE StringToLongCardinal (s: String; base: CARDINAL; VAR found: BOOLEAN) : LONGCARD ;
VAR
   n, l: CARDINAL ;
   c   : LONGCARD ;
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
      WHILE (n<l) AND (IsDecimalDigitValidLong(char(s, n), base, c) OR
                       IsHexidecimalDigitValidLong(char(s, n), base, c)) DO
         found := TRUE ;
         INC(n)
      END
   END ;
   s := KillString(s) ;
   RETURN( c )
END StringToLongCardinal ;


(*
   ShortCardinalToString - converts SHORTCARD, c, into a String. The field
                          width can be specified if non zero. Leading
                          characters are defined by padding.
                          The base allows the caller to generate binary,
                          octal, decimal, hexidecimal numbers.
                          The value of lower is only used when hexidecimal
                          numbers are generated and if TRUE then digits
                          abcdef are used, and if FALSE then ABCDEF are used.
*)

PROCEDURE ShortCardinalToString (c: SHORTCARD; width: CARDINAL; padding: CHAR;
                                base: CARDINAL; lower: BOOLEAN) : String ;
VAR
   s: String ;
BEGIN
   s := InitString('') ;
   IF VAL(CARDINAL, c)>base-1
   THEN
      s := ConCat(ConCat(s, ShortCardinalToString(c DIV 10, 0, ' ', base, lower)),
                  ShortCardinalToString(c MOD 10, 0, ' ', base, lower))
   ELSE
      IF c<=9
      THEN
         s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, c)+ORD('0'))))
      ELSE
         IF lower
         THEN
            s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, c)+ORD('a')-10)))
         ELSE
            s := ConCat(s, InitStringChar(CHR(VAL(CARDINAL, c)+ORD('A')-10)))
         END
      END
   END ;
   IF width>Length(s)
   THEN
      RETURN( ConCat(Mult(InitStringChar(padding), width-Length(s)), s) )
   END ;
   RETURN( s )
END ShortCardinalToString ;


(*
   StringToShortCardinal - converts a string, s, of, base, into a SHORTCARD.
                           Leading white space is ignored. It stops converting
                           when either the string is exhausted or if an illegal
                           numeral is found.
                           The parameter found is set TRUE if a number was found.
*)

PROCEDURE StringToShortCardinal (s: String; base: CARDINAL;
                                 VAR found: BOOLEAN) : SHORTCARD ;
VAR
   n, l: CARDINAL ;
   c   : SHORTCARD ;
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
      WHILE (n<l) AND (IsDecimalDigitValidShort(char(s, n), base, c) OR
                       IsHexidecimalDigitValidShort(char(s, n), base, c)) DO
         found := TRUE ;
         INC(n)
      END
   END ;
   s := KillString(s) ;
   RETURN( c )
END StringToShortCardinal ;


END StringConvert.
