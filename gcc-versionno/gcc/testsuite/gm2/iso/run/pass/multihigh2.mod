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

MODULE multihigh2 ;


FROM libc IMPORT exit, printf ;


PROCEDURE assert (b: BOOLEAN; line: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      printf ("%s:%d:assert failed\n", __FILE__, line)
   END
END assert ;


PROCEDURE sum1D (a: ARRAY OF CARDINAL) ;
BEGIN
   assert (HIGH (a) = 5, __LINE__) ;
   printf ("in sum1D, HIGH (a) = %d\n", HIGH (a))
END sum1D ;


PROCEDURE sum2D (a: ARRAY OF ARRAY OF CARDINAL) ;
BEGIN
   assert (HIGH (a) = 9, __LINE__) ;
   printf ("in sum2D, HIGH (a) = %d\n", HIGH (a)) ;
   assert (HIGH (a[0]) = 5, __LINE__) ;
   printf ("in sum2D, HIGH (a[0]) = %d\n", HIGH (a[0])) ;
   sum1D (a[0])
END sum2D ;


PROCEDURE sumMatrix (a: ARRAY OF ARRAY OF ARRAY OF CARDINAL) : CARDINAL ;
VAR
   i, j, c, s: CARDINAL ;
BEGIN
   printf ("in sumMatrix, HIGH (a) = %d\n", HIGH (a)) ;
   assert (HIGH (a) = 2, __LINE__) ;
   FOR i := 0 TO HIGH (a) DO
      printf ("in sumMatix, HIGH (a[i]) = %d\n", HIGH (a[i])) ;
      sum2D (a[i]) ;
      assert (HIGH (a[i])=9, __LINE__) ;
      FOR j := 0 TO HIGH (a[i]) DO
         sum1D (a[i, j]) ;
         sum1D (a[i][j])
      END
   END ;
   RETURN 1
END sumMatrix ;


PROCEDURE test ;
VAR
   v, s, i, j, k: CARDINAL ;
   m            : ARRAY [0..2] OF ARRAY [0..9] OF ARRAY [0..5] OF CARDINAL ;
BEGIN
   v := 1 ;
   FOR i := 0 TO 2 DO
      FOR j := 0 TO HIGH (m[i]) DO
         FOR k := 0 TO HIGH (m[i,j]) DO
            m[i, j, k] := v
         END ;
         INC (v)
      END
   END ;
   s := sumMatrix (m)
END test ;


BEGIN
   test
END multihigh2.
