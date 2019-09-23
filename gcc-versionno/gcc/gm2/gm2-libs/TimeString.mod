(* TimeString.mod provides time related string manipulation procedures.

Copyright (C) 2001-2019 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE TimeString ;


FROM wrapc IMPORT strtime ;
FROM ASCII IMPORT nul ;
FROM SYSTEM IMPORT ADDRESS ;


(*
   GetTimeString - places the time in ascii format into array, a.

*)

PROCEDURE GetTimeString (VAR a: ARRAY OF CHAR) ;
VAR
   Addr: POINTER TO CHAR ;
   i   : CARDINAL ;
BEGIN
   Addr := strtime() ;
   i := 0 ;
   IF Addr#NIL
   THEN
      WHILE (i<HIGH(a)) AND (Addr^#nul) DO
         a[i] := Addr^ ;
         INC(i) ;
         INC(Addr)
      END
   END ;
   IF i<HIGH(a)
   THEN
      a[i] := nul
   END
END GetTimeString ;


END TimeString.
