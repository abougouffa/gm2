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
IMPLEMENTATION MODULE gcd ;


FROM StrIO IMPORT WriteLn ;
FROM NumberIO IMPORT WriteCard ;


PROCEDURE GreatestCommonDivisor (x, y: CARDINAL) : CARDINAL ;
VAR
   u, v: CARDINAL ;
BEGIN
   u := x ;
   v := y ;
   WHILE x#y DO
      IF x>y
      THEN
         DEC(x, y) ;
         INC(u, v)
      ELSE
         DEC(y, x) ;
         INC(v, u)
      END
   END ;
   RETURN( u+v )
END GreatestCommonDivisor ;


VAR
   d1, d2,
   answer: CARDINAL ;
BEGIN
(*
   d1 := 24 ;
   d2 := 60 ;
*)
   answer := GreatestCommonDivisor(24, 60) ;
   WriteCard(answer, 6) ; WriteLn
END gcd.
