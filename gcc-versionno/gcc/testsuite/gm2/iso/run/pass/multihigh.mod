(* Copyright (C) 2018 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. *)

MODULE multihigh ;


FROM libc IMPORT exit, printf ;


PROCEDURE assert (b: BOOLEAN; line: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      printf ("%s:%d:assert failed\n", __FILE__, line)
   END
END assert ;


PROCEDURE sumArray (a: ARRAY OF CARDINAL) ;
BEGIN
   assert (HIGH (a) = 9, __LINE__) ;
   printf ("in sumArray, HIGH (a) = %d\n", HIGH (a))
END sumArray ;


PROCEDURE sumMatrix (a: ARRAY OF ARRAY OF CARDINAL) : CARDINAL ;
VAR
   i, c, s: CARDINAL ;
BEGIN
   printf ("in sumMatrix, HIGH (a) = %d\n", HIGH (a)) ;
   assert (HIGH (a) = 2, __LINE__) ;
   FOR i := 0 TO HIGH (a) DO
      printf ("in sumMatix, HIGH (a[i]) = %d\n", HIGH (a[i])) ;
      sumArray (a[i]) ;
      assert (HIGH (a[i])=9, __LINE__)
   END ;
   RETURN 1
END sumMatrix ;


PROCEDURE test ;
VAR
   v, s, i, j: CARDINAL ;
   m         : ARRAY [0..2] OF ARRAY [0..9] OF CARDINAL ;
BEGIN
   v := 1 ;
   FOR i := 0 TO 2 DO
      FOR j := 0 TO 2 DO
         m[i, j] := v ;
         INC (v)
      END
   END ;
   s := sumMatrix (m)
END test ;


BEGIN
   test
END multihigh.
