(* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
                 Free Software Foundation, Inc. *)
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA *)

IMPLEMENTATION MODULE FormatStrings ;

FROM DynamicStrings IMPORT String, InitString, InitStringChar, Mark,
                           ConCat, Slice, Index, char,
                           Assign, Length, Mult, Dup, ConCatChar,
                           PushAllocation, PopAllocationExemption,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;

FROM StringConvert IMPORT IntegerToString, CardinalToString ;
FROM ASCII IMPORT nul, nl, tab ;
FROM SYSTEM IMPORT ADDRESS ;

(*
#define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#define Dup(X) DupDB(X, __FILE__, __LINE__)
#define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
*)


(*
   doDSdbEnter - 
*)

PROCEDURE doDSdbEnter ;
BEGIN
   PushAllocation
END doDSdbEnter ;


(*
   doDSdbExit - 
*)

PROCEDURE doDSdbExit (s: String) ;
BEGIN
   s := PopAllocationExemption(TRUE, s)
END doDSdbExit ;


(*
   DSdbEnter - 
*)

PROCEDURE DSdbEnter ;
BEGIN
END DSdbEnter ;


(*
   DSdbExit - 
*)

PROCEDURE DSdbExit (s: String) ;
BEGIN
END DSdbExit ;


(*
#define DBsbEnter doDBsbEnter
#define DBsbExit  doDBsbExit
*)


(*
   IsDigit - returns TRUE if ch lies in the range: 0..9
*)

PROCEDURE IsDigit (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch>='0') AND (ch<='9') )
END IsDigit ;


(*
   Cast - casts a := b
*)

PROCEDURE Cast (VAR a: ARRAY OF BYTE; b: ARRAY OF BYTE) ;
VAR
   i: CARDINAL ;
BEGIN
   IF HIGH(a)=HIGH(b)
   THEN
      FOR i := 0 TO HIGH(a) DO
         a[i] := b[i]
      END
   ELSE
      HALT
   END
END Cast ;


(*
   HandleEscape - translates \n and \t into their respective ascii codes.
*)

PROCEDURE HandleEscape (s: String) : String ;
VAR
   d   : String ;
   i, j: INTEGER ;
   ch  : CHAR ;
BEGIN
   DSdbEnter ;
   d := InitString('') ;
   i := Index(s, '\', 0) ;
   j := 0 ;
   WHILE i>=0 DO
      IF i>0
      THEN
         (* initially i might be zero which means the end of the string, which is not what we want *)
         d := ConCat(d, Slice(s, j, i))
      END ;
      ch := char(s, i+1) ;
      IF ch='n'
      THEN
         (* requires a newline *)
         d := ConCat(d, Mark(InitStringChar(nl)))
      ELSIF ch='t'
      THEN
         (* requires a tab (yuck) *)
         d := ConCat(d, Mark(InitStringChar(tab)))
      ELSE
         (* copy escaped character *)
         d := ConCat(d, Mark(InitStringChar(ch)))
      END ;
      INC(i, 2) ;
      j := i ;
      i := Index(s, '\', CARDINAL(i))
   END ;
   (*   s := Assign(s, Mark(ConCat(d, Mark(Slice(s, j, 0))))) ;   (* dont Mark(s) in the Slice as we Assign contents *) *)
   s := ConCat(d, Mark(Slice(Mark(s), j, 0))) ;
   DSdbExit(s) ;
   RETURN( s )
END HandleEscape ;

      
(*
   FormatString - returns a String containing, s, together with encapsulated
                  entity, w. It only formats the first %s or %d or %u with n.
                  A new string is returned.
*)

PROCEDURE FormatString (s: String; w: ARRAY OF BYTE) : String ;
VAR
   left   : BOOLEAN ;
   u      : CARDINAL ;
   c,
   width,
   i, j, k: INTEGER ;
   leader,
   ch, ch2: CHAR ;
   p      : String ;
BEGIN
   DSdbEnter ;
   i := 0 ;
   j := Index(s, '%', 0) ;
   IF j=0
   THEN
      k := -Length(s)
   ELSE
      k := j
   END ;
   IF j>=0
   THEN
      IF char(s, j+1)='-'
      THEN
         left := TRUE ;
         INC(j)
      ELSE
         left := FALSE
      END ;
      ch := char(s, j+1) ;
      IF ch='0'
      THEN
         leader := '0'
      ELSE
         leader := ' '
      END ;
      width := 0 ;
      WHILE IsDigit(ch) DO
         width := (width*10)+VAL(INTEGER, ORD(ch)-ORD('0')) ;
         INC(j) ;
         ch := char(s, j+1)
      END ;
      IF (ch='c') OR (ch='s')
      THEN
         IF (ch='c')
         THEN
            ch2 := w[0] ;
            p := ConCatChar(InitString(''), ch2)
         ELSE
            Cast(p, w) ;
            p := Dup(p)
         END ;
         IF (width>0) AND (VAL(INTEGER, Length(p))<width)
         THEN
            IF left
            THEN
               (* place trailing spaces after, p *)
               p := ConCat(p,
                           Mark(Mult(Mark(InitString(' ')), width-VAL(INTEGER, Length(p)))))
            ELSE
               (* padd string, p, with leading spaces *)
               p := ConCat(Mult(Mark(InitString(' ')), width-VAL(INTEGER, Length(p))),
                           Mark(p))
            END
         END ;
         (* include string, p, into s *)
         s := ConCat(ConCat(Slice(s, i, k), Mark(p)), Mark(Slice(s, j+2, 0)))
      ELSIF ch='d'
      THEN
         Cast(c, w) ;
         s := ConCat(ConCat(Slice(s, i, k), IntegerToString(c, width, leader, FALSE, 10, FALSE)),
                     Mark(Slice(s, j+2, 0)))
      ELSIF ch='u'
      THEN
         Cast(u, w) ;
         s := ConCat(ConCat(Slice(s, i, k), CardinalToString(u, width, leader, 10, FALSE)),
                     Mark(Slice(s, j+2, 0)))
      ELSE
         s := ConCat(ConCat(Slice(s, i, k), Mark(InitStringChar(ch))), Mark(Slice(s, j+1, 0)))
      END
   ELSE
      s := Dup(s)
   END ;
   DSdbExit(s) ;
   RETURN( s )
END FormatString ;


(*
   Sprintf0 - returns a String containing, s, after it has had its
              escape sequences translated.
*)

PROCEDURE Sprintf0 (s: String) : String ;
BEGIN
   DSdbEnter ;
   s := HandleEscape(s) ;
   DSdbExit(s) ;
   RETURN( s )
END Sprintf0 ;


(*
   Sprintf1 - returns a String containing, s, together with encapsulated
              entity, w. It only formats the first %s or %d with n.
*)

PROCEDURE Sprintf1 (s: String; w: ARRAY OF BYTE) : String ;
BEGIN
   DSdbEnter ;
   s := FormatString(HandleEscape(s), w) ;
   DSdbExit(s) ;
   RETURN( s )
END Sprintf1 ;


(*
   Sprintf2 - returns a string, s, which has been formatted.
*)

PROCEDURE Sprintf2 (s: String; w1, w2: ARRAY OF BYTE) : String ;
BEGIN
   DSdbEnter ;
   s := FormatString(FormatString(HandleEscape(s), w1), w2) ;
   DSdbExit(s) ;
   RETURN( s )
END Sprintf2 ;


(*
   Sprintf3 - returns a string, s, which has been formatted.
*)

PROCEDURE Sprintf3 (s: String; w1, w2, w3: ARRAY OF BYTE) : String ;
BEGIN
   DSdbEnter ;
   s := FormatString(FormatString(FormatString(HandleEscape(s), w1), w2), w3) ;
   DSdbExit(s) ;
   RETURN( s )
END Sprintf3 ;


(*
   Sprintf4 - returns a string, s, which has been formatted.
*)

PROCEDURE Sprintf4 (s: String; w1, w2, w3, w4: ARRAY OF BYTE) : String ;
BEGIN
   DSdbEnter ;
   s := FormatString(FormatString(FormatString(FormatString(HandleEscape(s), w1), w2), w3), w4) ;
   DSdbExit(s) ;
   RETURN( s )
END Sprintf4 ;


END FormatStrings.
