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

IMPLEMENTATION MODULE M2Printf ;

FROM SFIO IMPORT WriteS ;
FROM FIO IMPORT StdOut ;
FROM DynamicStrings IMPORT string, InitString, KillString, InitStringCharStar, Mark ;
FROM StrLib IMPORT StrLen ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3, Sprintf4 ;
FROM NameKey IMPORT KeyToCharStar ;


(*
   IsDigit - returns TRUE if, ch, is a character 0..9
*)

PROCEDURE IsDigit (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch>='0') AND (ch<='9') )
END IsDigit ;


(*
   TranslateNameToString - takes a format specification string, a, and
                           if they consist of of %a then this is translated
                           into a String and %a is replaced by %s.
*)

PROCEDURE TranslateNameToCharStar (VAR a: ARRAY OF CHAR; n: CARDINAL;
                                   VAR w1, w2, w3, w4: WORD) ;
VAR
   argno,
   i, h : CARDINAL ;
BEGIN
   argno := 1 ;
   i := 0 ;
   h := StrLen(a) ;
   WHILE i<h DO
      IF a[i]='%'
      THEN
         INC(i) ;
         WHILE (i<h) AND IsDigit(a[i]) DO
            INC(i)
         END ;
         IF (i<h) AND (a[i]='a')
         THEN
            (* translate the NameKey into a String *)
            a[i] := 's' ;
            IF argno=1
            THEN
               w1 := Mark(InitStringCharStar(KeyToCharStar(w1)))
            ELSIF argno=2
            THEN
               w2 := Mark(InitStringCharStar(KeyToCharStar(w2)))
            ELSIF argno=3
            THEN
               w3 := Mark(InitStringCharStar(KeyToCharStar(w3)))
            ELSE
               w4 := Mark(InitStringCharStar(KeyToCharStar(w4)))
            END
         END ;
         INC(argno) ;
         IF argno>n
         THEN
            (* all done *)
            RETURN
         END
      END ;
      INC(i)
   END
END TranslateNameToCharStar ;


(*
   fprintf0 - writes out an array to, file, after the escape sequences have been
              translated.
*)

PROCEDURE fprintf0 (file: File; a: ARRAY OF CHAR) ;
BEGIN
   IF KillString(WriteS(file, Sprintf0(InitString(a))))=NIL
   THEN
   END
END fprintf0 ;


PROCEDURE fprintf1 (file: File; a: ARRAY OF CHAR; w: WORD) ;
VAR
   n: WORD ;
BEGIN
   TranslateNameToCharStar(a, 1, w, n, n, n) ;
   IF KillString(WriteS(file, Sprintf1(InitString(a), w)))=NIL
   THEN
   END
END fprintf1 ;


PROCEDURE fprintf2 (file: File; a: ARRAY OF CHAR; w1, w2: WORD) ;
VAR
   n: WORD ;
BEGIN
   TranslateNameToCharStar(a, 2, w1, w2, n, n) ;
   IF KillString(WriteS(file, Sprintf2(InitString(a), w1, w2)))=NIL
   THEN
   END
END fprintf2 ;


PROCEDURE fprintf3 (file: File; a: ARRAY OF CHAR; w1, w2, w3: WORD) ;
VAR
   n: WORD ;
BEGIN
   TranslateNameToCharStar(a, 3, w1, w2, w3, n) ;
   IF KillString(WriteS(file, Sprintf3(InitString(a), w1, w2, w3)))=NIL
   THEN
   END
END fprintf3 ;


PROCEDURE fprintf4 (file: File; a: ARRAY OF CHAR; w1, w2, w3, w4: WORD) ;
BEGIN
   TranslateNameToCharStar(a, 4, w1, w2, w3, w4) ;
   IF KillString(WriteS(file, Sprintf4(InitString(a), w1, w2, w3, w4)))=NIL
   THEN
   END
END fprintf4 ;


(*
   printf0 - writes out an array to, StdOut, after the escape sequences have been
             translated.
*)

PROCEDURE printf0 (a: ARRAY OF CHAR) ;
BEGIN
   fprintf0(StdOut, a)
END printf0 ;


PROCEDURE printf1 (a: ARRAY OF CHAR; w: WORD) ;
BEGIN
   fprintf1(StdOut, a, w)
END printf1 ;


PROCEDURE printf2 (a: ARRAY OF CHAR; w1, w2: WORD) ;
BEGIN
   fprintf2(StdOut, a, w1, w2)
END printf2 ;


PROCEDURE printf3 (a: ARRAY OF CHAR; w1, w2, w3: WORD) ;
BEGIN
   fprintf3(StdOut, a, w1, w2, w3)
END printf3 ;


PROCEDURE printf4 (a: ARRAY OF CHAR; w1, w2, w3, w4: WORD) ;
BEGIN
   fprintf4(StdOut, a, w1, w2, w3, w4)
END printf4 ;


END M2Printf.
