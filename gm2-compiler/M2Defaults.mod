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
IMPLEMENTATION MODULE M2Defaults;

FROM DynamicStrings IMPORT InitString, KillString ;
FROM SEnvironment IMPORT GetEnvironment ;


(*
   GetSearchPath - sets string, a, to the environment variable
      	           M2PATH.
*)

PROCEDURE GetSearchPath () : String ;
VAR
   s, p: String ;
BEGIN
   s := InitString('M2PATH') ;
   IF GetEnvironment(s, p)
   THEN
   END ;
   s := KillString(s) ;
   RETURN( p )
END GetSearchPath ;


(*
   GetOptions - returns a string, which is a copy of the environment variable
      	        M2OPTIONS
*)

PROCEDURE GetOptions () : String ;
VAR
   s, p: String ;
BEGIN
   s := InitString('M2OPTIONS') ;
   IF GetEnvironment(s, p)
   THEN
   END ;
   s := KillString(s) ;
   RETURN( p )
END GetOptions ;


END M2Defaults.
