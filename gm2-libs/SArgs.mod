(* Copyright (C) 2001 Free Software Foundation, Inc. *)
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
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA *)
IMPLEMENTATION MODULE SArgs ;


FROM SYSTEM IMPORT TSIZE, ADDRESS ;
FROM UnixArgs IMPORT ArgC, ArgV ;
FROM Strings IMPORT InitStringCharStar ;


TYPE
   PtrToChar      = POINTER TO CHAR ;
   PtrToPtrToChar = POINTER TO PtrToChar ;


(*
   GetArg - returns the nth argument from the command line.
            The success of the operation is returned.
            If TRUE is returned then the string, s, contains a
            new string, otherwise s is set to NIL.
*)

PROCEDURE GetArg (VAR s: String; i: CARDINAL) : BOOLEAN ;
VAR
   ppc: PtrToPtrToChar ;
BEGIN
   IF i<ArgC
   THEN
      ppc := ADDRESS(CARDINAL(ArgV) + (i*TSIZE(PtrToChar))) ;
      s   := InitStringCharStar(ppc^) ;

      RETURN( TRUE )
   ELSE
      s := NIL ;
      RETURN( FALSE )
   END ;
END GetArg ;


(*
   Narg - returns the number of arguments available from
          command line.
*)

PROCEDURE Narg () : CARDINAL ;
BEGIN
   RETURN( ArgC )
END Narg ;


END SArgs.
