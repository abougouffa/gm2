(* Copyright (C) 2005 Free Software Foundation, Inc. *)
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA *)

IMPLEMENTATION MODULE RealConversions ;

FROM DynamicStrings IMPORT String, InitString, KillString, CopyOut, Length,
                           ConCat, ConCatChar, Mark, RemoveWhitePrefix,
                           InitStringChar, Mult, Slice, Index, string ;

FROM StringConvert IMPORT LongrealToString, StringToLongreal,
                          StringToLongreal, StringToInteger, itos ;

FROM ASCII IMPORT nul ;
FROM Builtins IMPORT logl ;
FROM libc IMPORT printf ;


CONST
   Debugging = FALSE ;


(*
   logl10 - 
*)

PROCEDURE logl10 (r: LONGREAL) : LONGREAL ;
BEGIN
   RETURN logl(r)/logl(10.0)
END logl10 ;


(*
   logi10 - 
*)

PROCEDURE logi10 (i: INTEGER) : INTEGER ;
VAR
   j: INTEGER ;
BEGIN
   j := 0 ;
   IF i<0
   THEN
      WHILE i<-9 DO
         DEC(j) ;
         i := i DIV 10
      END
   ELSE
      WHILE i>9 DO
         INC(j) ;
         i := i DIV 10
      END
   END ;
   RETURN j
END logi10 ;


(*
   powl10 - 
*)

PROCEDURE powl10 (i: INTEGER) : LONGREAL ;
VAR
   r: LONGREAL ;
BEGIN
   r := 1.0 ;
   IF i<0
   THEN
      WHILE i<0 DO
         r := r/10.0 ;
         INC(i)
      END
   ELSE
      WHILE i>0 DO
         r := r*10.0 ;
         DEC(i)
      END
   END ;
   RETURN r
END powl10 ;


(*
   RealToString - converts a real, r, into a right justified string, str.
                  The number of digits to the right of the decimal point
                  is given in, digits. The value, width, represents the
                  maximum number of characters to be used in the string,
                  str.

                  If digits is negative then exponental notation is used
                  whereas if digits is positive then fixed point notation
                  is used.

                  If, r, is less than 0.0 then a '-' preceeds the value,
                  str. However, if, r, is >= 0.0 a '+' is not added.

                  If the conversion of, r, to a string requires more
                  than, width, characters then the string, str, is set
                  to a nul string and, ok is assigned FALSE.

                  For fixed point notation the minimum width required is
                  ABS(width)+8

                  For exponent notation the minimum width required is
                  ABS(width)+2+ the number of digits to the left of the
                  decimal point.
*)

PROCEDURE RealToString (r: REAL; digits: INTEGER; width: CARDINAL;
                        VAR str: ARRAY OF CHAR; VAR ok: BOOLEAN) ;
VAR
   l: LONGREAL ;
BEGIN
   l := VAL(LONGREAL, r) ;
   LongRealToString(l, digits, width, str, ok)
END RealToString ;


(*
   LongRealToString - converts a real, r, into a right justified string, str.
                      The number of digits to the right of the decimal point
                      is given in, digits. The value, width, represents the
                      maximum number of characters to be used in the string,
                      str.

                      If digits is negative then exponent notation is used
                      whereas if digits is positive then fixed point notation
                      is used.

                      If, r, is less than 0.0 then a '-' preceeds the value,
                      str. However, if, r, is >= 0.0 a '+' is not added.

                      If the conversion of, r, to a string requires more
                      than, width, characters then the string, str, is set
                      to a nul string and, ok is assigned FALSE.

                      For fixed point notation the minimum width required is
                      ABS(width)+8

                      For exponent notation the minimum width required is
                      ABS(width)+2+ the number of digits to the left of the
                      decimal point.
*)

PROCEDURE LongRealToString (r: LONGREAL; digits: INTEGER; width : CARDINAL;
                            VAR str: ARRAY OF CHAR; VAR ok: BOOLEAN) ;
VAR
   s, e      : String ;
   p, k, j,
   powerOfTen: INTEGER ;
   i         : INTEGER ;
BEGIN
   IF digits>0
   THEN
      s := Slice(Mark(LongrealToString(r, width, digits)), 0, width)
   ELSE
      digits := ABS(digits) ;
      IF r>=0.0
      THEN
         powerOfTen := TRUNC(logl10(r)) ;
         s := InitString('') ;
         j := 0
      ELSE
         powerOfTen := TRUNC(logl10(r)) ;
         s := InitString('-') ;
         j := 1
      END ;
      r := r*powl10(-powerOfTen) ;
      IF width>=Length(s)+2
      THEN
         s := ConCat(s, Mark(RemoveWhitePrefix(Mark(LongrealToString(r, width+1, digits))))) ;
         IF Debugging
         THEN
            i := printf('value returned was %s\n', string(s))
         END ;
         p := Index(s, '.', 0) ;
         IF p>=0
         THEN
            (* remove the '.' *)
            s := ConCat(Slice(Mark(s), 0, p), Mark(Slice(Mark(s), p+1, 0))) ;
            s := Slice(Mark(s), 0, width) ;
            IF Debugging
            THEN
               i := printf('value returned was %s\n', string(s))
            END ;
            p := powerOfTen ;
            k := Length(s) ;
            IF Debugging
            THEN
               i := printf('p = %d, powerOfTen = %d, k = %d,  k-p = %d\n',
                           p, powerOfTen, k, k-p)
            END ;
            WHILE (p<k) AND (k-p>digits) AND (width>3+j+p+logi10(powerOfTen-p+1)+1) DO
               INC(p) ;
               IF Debugging
               THEN
                  i := printf('p = %d, powerOfTen = %d, k = %d,  k-p = %d\n',
                              p, powerOfTen, k, k-p)
               END
            END ;
            s := ConCat(Slice(s, 0, p),
                        Mark(ConCat(Mark(ConCat(InitStringChar('.'),
                                                Slice(Mark(s), p, 0))),
                                    Mark(ConCat(InitStringChar('E'),
                                                Mark(itos(powerOfTen-p+1, 0, ' ', TRUE))))))) ;
            IF Debugging
            THEN
               i := printf('value returned was %s\n', string(s))
            END
         ELSE
            s := ConCat(s, Mark(e))
         END
      ELSE
         s := KillString(s) ;
         ok := FALSE ;
         str[0] := nul ;
         RETURN
      END ;
   END ;
   IF Length(s)<=width
   THEN
      s := ConCat(Mult(InitStringChar(' '), width-Length(s)), Mark(s)) ;
      IF Debugging
      THEN
         i := printf('value returned was %s\n', string(s))
      END ;
      CopyOut(str, s) ;
      ok := TRUE
   ELSE
      str[0] := nul ;
      ok := FALSE
   END ;
   s := KillString(s)
END LongRealToString ;


(*
   StringToLongReal - converts, str, into a LONGREAL, r. The parameter, ok, is
                      set to TRUE if the conversion was successful.
*)

PROCEDURE StringToLongReal (str: ARRAY OF CHAR; VAR r: LONGREAL; VAR ok: BOOLEAN) ;
VAR
   s: String ;
BEGIN
   s := InitString(str) ;
   r := StringToLongreal(s, ok) ;
   s := KillString(s)
END StringToLongReal ;


(*
   StringToReal - converts, str, into a REAL, r. The parameter, ok, is
                  set to TRUE if the conversion was successful.
*)

PROCEDURE StringToReal (str: ARRAY OF CHAR; VAR r: REAL; VAR ok: BOOLEAN) ;
VAR
   l: LONGREAL ;
BEGIN
   StringToLongReal(str, l, ok) ;
   IF ok
   THEN
      r := VAL(REAL, l)
   END
END StringToReal ;


END RealConversions.
