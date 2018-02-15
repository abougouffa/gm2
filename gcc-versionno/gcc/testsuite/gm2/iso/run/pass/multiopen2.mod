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

IMPLEMENTATION MODULE multiopen2 ;


FROM libc IMPORT exit, printf ;


PROCEDURE assert (b: BOOLEAN; line: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      printf ("%s:%d:assert failed\n", __FILE__, line)
   END
END assert ;


PROCEDURE sumArray (a: ARRAY OF CARDINAL) : CARDINAL ;
VAR
   s,
   i: CARDINAL ;
BEGIN
   s := 0 ;
   FOR i := 0 TO 2 DO
      printf ("  array[%d] = %d\n", i, a[i]) ;
      INC (s, a[i])
   END ;
   RETURN s
END sumArray ;


PROCEDURE sumMatrix (a: ARRAY OF ARRAY OF CARDINAL) : CARDINAL ;
VAR
   i, c, s: CARDINAL ;
BEGIN
   s := 0 ;
   FOR i := 0 TO 2 DO
	printf ("column %d\n", i) ;
	c := sumArray (a[i]) ;
	printf ("column sum = %d\n", c) ;
	INC (s, c)
   END ;
   RETURN s
END sumMatrix ;


PROCEDURE test ;
VAR
   v, s, i, j: CARDINAL ;
   m         : ARRAY [0..2] OF ARRAY [0..2] OF CARDINAL ;
BEGIN
     v := 1 ;
     FOR i := 0 TO 2 DO
	  FOR j := 0 TO 2 DO
	       m[i, j] := v ;
	       INC (v)
	  END
     END ;
     s := sumMatrix (m) ;
     printf ("sum of complete array is: %d\n", s) ;
     assert (s = 45, __LINE__)
END test ;


BEGIN
   test
END multiopen2.
