(* SEnvironment.mod provides access to the environment of a process.

Copyright (C) 2001-2020 Free Software Foundation, Inc.
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
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE SEnvironment ;

FROM DynamicStrings IMPORT string, InitStringCharStar,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;

FROM libc IMPORT getenv ;

(*
#undef GM2_DEBUG_SENVIRONMENT
if defined(GM2_DEBUG_SENVIRONMENT)
#  define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#  define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#  define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#  define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#  define Dup(X) DupDB(X, __FILE__, __LINE__)
#  define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
#endif
*)


(*
   GetEnvironment - gets the environment variable, Env, and places
      	       	    a copy of its value into string, a.
*)

PROCEDURE GetEnvironment (env: String; VAR s: String) : BOOLEAN ;
VAR
   Addr: POINTER TO CHAR ;
BEGIN
   IF env=NIL
   THEN
      s := NIL ;
      RETURN( FALSE )
   ELSE
      Addr := getenv(string(env)) ;
      IF Addr=NIL
      THEN
         s := NIL ;
         RETURN( FALSE )
      ELSE
         s := InitStringCharStar(Addr) ;
         RETURN( TRUE )
      END
   END
END GetEnvironment ;


END SEnvironment.
