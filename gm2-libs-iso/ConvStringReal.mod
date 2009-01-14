(* Copyright (C) 2009 Free Software Foundation, Inc. *)
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA *)

IMPLEMENTATION MODULE ConvStringReal ;

FROM DynamicStrings IMPORT InitString, KillString, ConCat, ConCatChar,
                           Slice, Length, Mult, Mark, InitStringCharStar,
                           InitStringChar ;
FROM StringConvert IMPORT IntegerToString, ToSigFig ;
FROM dtoa IMPORT dtoa, Mode ;
FROM SYSTEM IMPORT ADDRESS ;


(*
   RealToFloatString - converts a real with, sigFigs, into a string
                       and returns the result as a string.
*)

PROCEDURE RealToFloatString (real: REAL; sigFigs: CARDINAL) : String ;
VAR
   point, l,
   powerOfTen: INTEGER ;
   s         : String ;
   r         : ADDRESS ;
   sign      : BOOLEAN ;
BEGIN
   r := dtoa(real, maxsignificant, sigFigs, point, sign) ;
   s := InitStringCharStar(r) ;
   IF sigFigs>0
   THEN
      (* free(r) ; *)
      l := Length(s) ;
      IF VAL(INTEGER, sigFigs)<l
      THEN
         s := Slice(Mark(s), 0, sigFigs) ;
         l := Length(s)
      END ;
      IF point<0
      THEN
         s := ConCat(ConCat(InitString('0.'), Mult(InitStringChar('0'), -point)), s)
      ELSIF point<l
      THEN
         s := ConCat(ConCatChar(Slice(s, 0, point), '.'),
                     Slice(s, point, 0))
      END ;
      s := ConCat(ConCatChar(s, 'E'),
                  IntegerToString(point, 0, ' ', TRUE, 10, FALSE)) ;
      IF sign
      THEN
         s := ConCat(InitStringChar('-'), Mark(s))
      END
   END ;
   RETURN( s )
END RealToFloatString ;


(*
   RealToEngString - converts the value of real to floating-point
                     string form, with sigFigs significant figures.
                     The number is scaled with one to three digits
                     in the whole number part and with an exponent
                     that is a multiple of three.
*)

PROCEDURE RealToEngString (real: REAL; sigFigs: CARDINAL) : String ;
VAR
   point,
   powerOfTen: INTEGER ;
   s         : String ;
   r         : ADDRESS ;
   l         : CARDINAL ;
   sign      : BOOLEAN ;
BEGIN
   r := dtoa(real, maxsignificant, sigFigs, point, sign) ;
   s := InitStringCharStar(r) ;
   IF sigFigs>0
   THEN
      (* free(r) ; *)
      l := Length(s) ;
      IF sigFigs<l
      THEN
         s := Slice(Mark(s), 0, sigFigs) ;
         l := Length(s)
      END ;
      IF point>=0
      THEN
         CASE point MOD 3 OF

         0:  powerOfTen := point |
         1:  powerOfTen := point-1 |
         2:  powerOfTen := point-2

         END
      ELSE
         CASE ABS(point) MOD 3 OF

         0:  powerOfTen := point |
         1:  powerOfTen := point+1 |
         2:  powerOfTen := point+2

         END
      END ;

      IF powerOfTen<0
      THEN
         s := ConCat(ConCat(InitString('0.'), Mult(InitStringChar('0'), -powerOfTen)), s)
      ELSIF powerOfTen<VAL(INTEGER, l)
      THEN
         s := ConCat(ConCatChar(Slice(s, 0, powerOfTen), '.'),
                     Slice(s, powerOfTen, 0))
      END ;
      s := ConCat(ConCatChar(s, 'E'),
                  IntegerToString(powerOfTen, 0, ' ', TRUE, 10, FALSE)) ;
      IF sign
      THEN
         s := ConCat(InitStringChar('-'), Mark(s))
      END
   END ;
   RETURN( s )
END RealToEngString ;


(*
   FixedRealToString - returns the number of characters in the fixed-point
                       string representation of real rounded to the given
                       place relative to the decimal point.
*)

PROCEDURE FixedRealToString (real: REAL; place: INTEGER) : String ;
VAR
   l,
   point: INTEGER ;
   sign : BOOLEAN ;
   r    : ADDRESS ;
   s, t : String ;
BEGIN
   r := dtoa(real, maxsignificant, 100, point, sign) ;
   (* free(r) ; *)
   t := InitStringCharStar(r) ;
   (* free(r) ; *)
   l := Length(t) ;
   IF l-point+place>0
   THEN
      s := ToSigFig(t, l-point+place)
   ELSE
      s := InitString('0')
   END ;
   t := KillString(t) ;
   IF sign
   THEN
      s := ConCat(InitStringChar('-'), Mark(s))
   END ;
   RETURN( s )
END FixedRealToString ;


END ConvStringReal.
