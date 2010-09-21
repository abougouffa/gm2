(* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc. *)
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
