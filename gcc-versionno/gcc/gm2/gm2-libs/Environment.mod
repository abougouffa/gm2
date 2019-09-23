(* Environment.mod provides access to the environment settings of a process.

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

IMPLEMENTATION MODULE Environment ;


FROM SYSTEM IMPORT ADR ;
FROM libc IMPORT getenv ;
FROM ASCII IMPORT nul ;
FROM StrLib IMPORT StrCopy ;


(*
   GetEnvironment - gets the environment variable, Env, and places
      	       	    a copy of its value into string, a.
*)

PROCEDURE GetEnvironment (Env: ARRAY OF CHAR; VAR a: ARRAY OF CHAR) : BOOLEAN ;
VAR
   High,
   i   : CARDINAL ;
   Addr: POINTER TO CHAR ;
BEGIN
   i := 0 ;
   High := HIGH(a) ;
   Addr := getenv(ADR(Env)) ;
   WHILE (i<High) AND (Addr#NIL) AND (Addr^#nul) DO
      a[i] := Addr^ ;
      INC(Addr) ;
      INC(i)
   END ;
   IF i<High
   THEN
      a[i] := nul
   END ;
   RETURN( Addr#NIL )
END GetEnvironment ;


END Environment.
