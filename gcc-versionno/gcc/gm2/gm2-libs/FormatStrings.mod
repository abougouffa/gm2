(* FormatStrings.mod provides a pseudo printf capability.

Copyright (C) 2005-2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE FormatStrings ;

FROM DynamicStrings IMPORT String, InitString, InitStringChar, Mark,
                           ConCat, Slice, Index, char, string,
                           Assign, Length, Mult, Dup, ConCatChar,
                           PushAllocation, PopAllocationExemption,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB,
                           KillString, ConCatChar ;

FROM StringConvert IMPORT IntegerToString, CardinalToString ;
FROM ASCII IMPORT nul, nl, tab ;
FROM SYSTEM IMPORT ADDRESS ;


(*
#undef GM2_DEBUG_FORMATSTRINGS
#if defined(GM2_DEBUG_FORMATSTRINGS)
#  define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#  define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#  define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#  define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#  define Dup(X) DupDB(X, __FILE__, __LINE__)
#  define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
#endif
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
   s := PopAllocationExemption (TRUE, s)
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
#if defined(GM2_DEBUG_FORMATSTRINGS)
#  define DBsbEnter doDBsbEnter
#  define DBsbExit  doDBsbExit
#endif
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
   d := InitString ('') ;
   i := Index (s, '\', 0) ;
   j := 0 ;
   WHILE i>=0 DO
      IF i>0
      THEN
         (* initially i might be zero which means the end of the string, which is not what we want.  *)
         d := ConCat (d, Slice (s, j, i))
      END ;
      ch := char (s, i+1) ;
      IF ch='n'
      THEN
         (* requires a newline.  *)
         d := ConCat (d, Mark (InitStringChar (nl)))
      ELSIF ch='t'
      THEN
         (* requires a tab.  *)
         d := ConCat (d, Mark (InitStringChar (tab)))
      ELSE
         (* copy escaped character.  *)
         d := ConCat (d, Mark (InitStringChar (ch)))
      END ;
      INC (i, 2) ;
      j := i ;
      i := Index (s, '\', CARDINAL (i))
   END ;
   (*   s := Assign(s, Mark(ConCat(d, Mark(Slice(s, j, 0))))) ;   (* dont Mark(s) in the Slice as we Assign contents *) *)
   s := ConCat (d, Mark (Slice (Mark (s), j, 0))) ;
   DSdbExit (s) ;
   RETURN s
END HandleEscape ;


(*
   FormatString - returns a String containing, s, together with encapsulated
                  entity, w. It only formats the first %s or %d or %u with n.
                  A new string is returned.
*)

PROCEDURE FormatString (fmt: String; VAR startpos: INTEGER; in: String; w: ARRAY OF BYTE) : String ;
VAR
   s: String ;
BEGIN
   DSdbEnter ;
   IF startpos >= 0
   THEN
      s := PerformFormatString (fmt, startpos, in, w)
   ELSE
      s := Dup (in)
   END ;
   DSdbExit (s) ;
   RETURN s
END FormatString ;


PROCEDURE PerformFormatString (fmt: String; VAR startpos: INTEGER; in: String; w: ARRAY OF BYTE) : String ;
VAR
   left     : BOOLEAN ;
   u        : CARDINAL ;
   c,
   width,
   nextperc,
   afterperc,
   endpos   : INTEGER ;
   leader,
   ch, ch2  : CHAR ;
   p        : String ;
BEGIN
   WHILE startpos >= 0 DO
      nextperc := Index (fmt, '%', startpos) ;
      afterperc := nextperc ;
      IF nextperc >= 0
      THEN
         INC (afterperc) ;
         IF char (fmt, afterperc)='-'
         THEN
            left := TRUE ;
            INC (afterperc)
         ELSE
            left := FALSE
         END ;
         ch := char (fmt, afterperc) ;
         IF ch = '0'
         THEN
            leader := '0'
         ELSE
            leader := ' '
         END ;
         width := 0 ;
         WHILE IsDigit (ch) DO
            width := (width*10)+VAL (INTEGER, ORD (ch) - ORD ('0')) ;
            INC (afterperc) ;
            ch := char (fmt, afterperc)
         END ;
         IF (ch='c') OR (ch='s')
         THEN
            INC (afterperc) ;
            IF (ch='c')
            THEN
               ch2 := w[0] ;
               p := ConCatChar (InitString (''), ch2)
            ELSE
               Cast (p, w) ;
               p := Dup (p)
            END ;
            IF (width>0) AND (VAL (INTEGER, Length (p)) < width)
            THEN
               IF left
               THEN
                  (* place trailing spaces after, p.  *)
                  p := ConCat(p,
                              Mark(Mult(Mark(InitString(' ')), width-VAL(INTEGER, Length(p)))))
               ELSE
                  (* padd string, p, with leading spaces.  *)
                  p := ConCat(Mult(Mark(InitString(' ')), width-VAL(INTEGER, Length(p))),
                              Mark(p))
               END
            END ;
            (* include string, p, into, in.  *)
            IF nextperc > 0
            THEN
               in := ConCat (in, Slice (fmt, startpos, nextperc))
            END ;
            in := ConCat (in, p) ;
            startpos := afterperc ;
            DSdbExit (NIL) ;
            RETURN in
         ELSIF ch='d'
         THEN
            INC (afterperc) ;
            Cast (c, w) ;
            in := Copy (fmt, in, startpos, nextperc) ;
            in := ConCat (in, IntegerToString (c, width, leader, FALSE, 10, FALSE)) ;
            startpos := afterperc ;
            DSdbExit (NIL) ;
            RETURN in
         ELSIF ch='x'
         THEN
            INC (afterperc) ;
            Cast (u, w) ;
            in := ConCat (in, Slice (fmt, startpos, nextperc)) ;
            in := ConCat (in, CardinalToString (u, width, leader, 16, TRUE)) ;
            startpos := afterperc ;
            DSdbExit (NIL) ;
            RETURN in
         ELSIF ch='u'
         THEN
            INC (afterperc) ;
            Cast (u, w) ;
            in := ConCat (in, Slice (fmt, startpos, nextperc)) ;
            in := ConCat (in, CardinalToString (u, width, leader, 10, FALSE)) ;
            startpos := afterperc ;
            DSdbExit (NIL) ;
            RETURN in
         ELSE
            INC (afterperc) ;
            (* copy format string.  *)
            IF nextperc > 0
            THEN
               in := ConCat (in, Slice (fmt, startpos, nextperc))
            END ;
            (* and the character after the %.  *)
            in := ConCat (in, Mark (InitStringChar (ch)))
         END ;
         startpos := afterperc
      ELSE
         (* nothing to do.  *)
         DSdbExit (NIL) ;
         RETURN in
      END
   END ;
   DSdbExit (NIL) ;
   RETURN in
END PerformFormatString ;


(*
   Copy - copies, fmt[start:end] -> in and returns in.  Providing that start >= 0.
*)

PROCEDURE Copy (fmt, in: String; start, end: INTEGER) : String ;
BEGIN
   IF start >= 0
   THEN
      IF end > 0
      THEN
         in := ConCat (in, Mark (Slice (fmt, start, end)))
      ELSIF end < 0
      THEN
         in := ConCat (in, Mark (Slice (fmt, start, 0)))
      END
   END ;
   RETURN in
END Copy ;


(*
   HandlePercent - pre-condition:  s, is a string.
                   Post-condition:  a new string is returned which is a copy of,
                   s, except %% is transformed into %.
*)

PROCEDURE HandlePercent (fmt, s: String; startpos: INTEGER) : String ;
VAR
   prevpos: INTEGER ;
   result : String ;
BEGIN
   IF (startpos = Length (fmt)) OR (startpos < 0)
   THEN
      RETURN s
   ELSE
      prevpos := startpos ;
      WHILE (startpos >= 0) AND (prevpos < INTEGER (Length (fmt))) DO
         startpos := Index (fmt, '%', startpos) ;
         IF startpos >= prevpos
         THEN
            IF startpos > 0
            THEN
               s := ConCat (s, Mark (Slice (fmt, prevpos, startpos)))
            END ;
            INC (startpos) ;
            IF char (fmt, startpos) = '%'
            THEN
               s := ConCatChar (s, '%') ;
               INC (startpos)
            END ;
            prevpos := startpos
         END
      END ;
      IF (prevpos < INTEGER (Length (fmt)))
      THEN
         s := ConCat (s, Mark (Slice (fmt, prevpos, 0)))
      END ;
      RETURN s
   END
END HandlePercent ;


(*
   Sprintf0 - returns a String containing, s, after it has had its
              escape sequences translated.
*)

PROCEDURE Sprintf0 (fmt: String) : String ;
VAR
   s: String ;
BEGIN
   DSdbEnter ;
   fmt := HandleEscape (fmt) ;
   s := HandlePercent (fmt, InitString (''), 0) ;
   DSdbExit (s) ;
   RETURN s
END Sprintf0 ;


(*
   Sprintf1 - returns a String containing, s, together with encapsulated
              entity, w. It only formats the first %s or %d with n.
*)

PROCEDURE Sprintf1 (fmt: String; w: ARRAY OF BYTE) : String ;
VAR
   i: INTEGER ;
   s: String ;
BEGIN
   DSdbEnter ;
   fmt := HandleEscape (fmt) ;
   i := 0 ;
   s := FormatString (fmt, i, InitString (''), w) ;
   s := HandlePercent (fmt, s, i) ;
   DSdbExit (s) ;
   RETURN s
END Sprintf1 ;


(*
   Sprintf2 - returns a string, s, which has been formatted.
*)

PROCEDURE Sprintf2 (fmt: String; w1, w2: ARRAY OF BYTE) : String ;
VAR
   i: INTEGER ;
   s: String ;
BEGIN
   DSdbEnter ;
   fmt := HandleEscape (fmt) ;
   i := 0 ;
   s := FormatString (fmt, i, InitString (''), w1) ;
   s := FormatString (fmt, i, s, w2) ;
   s := HandlePercent (fmt, s, i) ;
   DSdbExit (s) ;
   RETURN s
END Sprintf2 ;


(*
   Sprintf3 - returns a string, s, which has been formatted.
*)

PROCEDURE Sprintf3 (fmt: String; w1, w2, w3: ARRAY OF BYTE) : String ;
VAR
   i: INTEGER ;
   s: String ;
BEGIN
   DSdbEnter ;
   fmt := HandleEscape (fmt) ;
   i := 0 ;
   s := FormatString (fmt, i, InitString (''), w1) ;
   s := FormatString (fmt, i, s, w2) ;
   s := FormatString (fmt, i, s, w3) ;
   s := HandlePercent (fmt, s, i) ;
   DSdbExit (s) ;
   RETURN s
END Sprintf3 ;


(*
   Sprintf4 - returns a string, s, which has been formatted.
*)

PROCEDURE Sprintf4 (fmt: String; w1, w2, w3, w4: ARRAY OF BYTE) : String ;
VAR
   i: INTEGER ;
   s: String ;
BEGIN
   DSdbEnter ;
   fmt := HandleEscape (fmt) ;
   i := 0 ;
   s := FormatString (fmt, i, InitString (''), w1) ;
   s := FormatString (fmt, i, s, w2) ;
   s := FormatString (fmt, i, s, w3) ;
   s := FormatString (fmt, i, s, w4) ;
   s := HandlePercent (fmt, s, i) ;
   DSdbExit (s) ;
   RETURN s
END Sprintf4 ;


END FormatStrings.
