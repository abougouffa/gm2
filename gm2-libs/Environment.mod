(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc. *)
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
