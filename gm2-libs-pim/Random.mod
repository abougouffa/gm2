(* Copyright (C) 2005, 2006 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE Random ;

FROM libc IMPORT rand, srand ;
FROM Selective IMPORT Timeval, InitTime, KillTime, GetTime, GetTimeOfDay ;
FROM FloatingUtilities IMPORT Frac, Fracl ;


(*
   Randomize - initialize the random number generator with a seed
               based on the microseconds.
*)

PROCEDURE Randomize ;
VAR
   t        : Timeval ;
   sec, usec: CARDINAL ;
BEGIN
   t := InitTime(0, 0) ;
   IF GetTimeOfDay(t)=0
   THEN
   END ;
   GetTime(t, sec, usec) ;
   RandomInit(usec) ;
   t := KillTime(t)
END Randomize ;


(*
   RandomInit - initialize the random number generator with value, seed.
*)

PROCEDURE RandomInit (seed: CARDINAL) ;
BEGIN
   srand(seed)
END RandomInit ;


(*
   RandomBytes - fills in an array with random values.
*)

PROCEDURE RandomBytes (a: ARRAY OF BYTE) ;
VAR
   i, h: CARDINAL ;
BEGIN
   h := HIGH(a) ;
   i := 0 ;
   WHILE i<=h DO
      a[i] := VAL(BYTE, rand()) ;
      INC(i)
   END
END RandomBytes ;


(*
   RandomInt - return an INTEGER in the range 0..bound-1
*)

PROCEDURE RandomInt (bound: INTEGER) : INTEGER ;
BEGIN
   IF bound=0
   THEN
      RETURN rand()
   ELSE
      RETURN rand() MOD bound
   END
END RandomInt ;


(*
   RandomCard - return a CARDINAL in the range 0..bound-1
*)

PROCEDURE RandomCard (bound: CARDINAL) : CARDINAL ;
VAR
   c: CARDINAL ;
BEGIN
   RandomBytes(c) ;
   RETURN c MOD bound
END RandomCard ;


(*
   RandomReal - return a REAL number in the range 0.0..1.0
*)

PROCEDURE RandomReal () : REAL ;
VAR
   r: REAL ;
BEGIN
   RandomBytes(r) ;
   RETURN Frac(r)
END RandomReal ;


(*
   RandomLongReal - return a LONGREAL number in the range 0.0..1.0
*)

PROCEDURE RandomLongReal () : LONGREAL ;
VAR
   l: LONGREAL ;
BEGIN
   RandomBytes(l) ;
   RETURN Fracl(l)
END RandomLongReal ;


BEGIN
   Randomize
END Random.