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

MODULE multiopen ;

FROM libc IMPORT exit, printf ;


PROCEDURE assert (b: BOOLEAN; line: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      printf ("%s:%d:assert failed\n", __FILE__, line)
   END
END assert ;


(*
   foo -
*)

PROCEDURE foo (a: ARRAY OF CARDINAL) : CARDINAL ;
VAR
   s, i: CARDINAL ;
BEGIN
   s := 0 ;
   FOR i := 0 TO HIGH (a) DO
      INC (s, a[i])
   END ;
   RETURN s
END foo ;


(*
   foo2 -
*)

PROCEDURE foo2 (a: ARRAY OF ARRAY OF CARDINAL) : CARDINAL ;
VAR
   i, j, s: CARDINAL ;
BEGIN
   s := foo (a[1]) ;
   s := 0 ;
   FOR i := 0 TO HIGH (a) DO
      FOR j := 0 TO HIGH (a[0]) DO
         INC (s, a[i, j])
      END
   END ;
   RETURN s
END foo2 ;


TYPE
   list = ARRAY [0..2] OF CARDINAL ;

VAR
   m: list ;
   s: CARDINAL ;
   m2: ARRAY [0..2] OF list ;
BEGIN
   m := list { 1, 2, 3 } ;
   s := foo (m) ;
   assert (s = 6, __LINE__) ;
   m2[0] := list { 1, 2, 3 } ;
   m2[1] := list { 4, 5, 6 } ;
   m2[2] := list { 7, 8, 9 } ;
   s := foo (m2[1]) ;
   assert (s = 15, __LINE__) ;
   s := foo2 (m2) ;
   assert (s = 45, __LINE__) ;
END multiopen.
