(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE Args ;


FROM UnixArgs IMPORT ArgC, ArgV ;
FROM ASCII IMPORT nul ;

(* %%%FORWARD%%%
PROCEDURE GetArg (VAR a: ARRAY OF CHAR ; i: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE Narg () : CARDINAL ; FORWARD ;
   %%%FORWARD%%% *)

CONST
   MaxArgs   =  255 ;
   MaxString = 4096 ;


(*
   Source allows us to examine the ArgV contents.
*)

VAR
   Source: POINTER TO ARRAY [0..MaxArgs] OF
           POINTER TO ARRAY [0..MaxString] OF CHAR ;


(*
   GetArg - returns the nth argument from the command line.
            The success of the operation is returned.
*)

PROCEDURE GetArg (VAR a: ARRAY OF CHAR; i: CARDINAL) : BOOLEAN ;
VAR
   High,
   j   : CARDINAL ;
BEGIN
   j := 0 ;
   High := HIGH(a) ;
   IF i<ArgC
   THEN
      Source := ArgV ;
      WHILE (Source^[i]^[j]#nul) AND (j<High) DO
         a[j] := Source^[i]^[j] ;
         INC(j)
      END
   END ;
   IF j<=High
   THEN
      a[j] := nul
   END ;
   RETURN( i<ArgC )
END GetArg ;


(*
   Narg - returns the number of arguments available from
          command line.
*)

PROCEDURE Narg () : CARDINAL ;
BEGIN
   RETURN( ArgC )
END Narg ;


END Args.
