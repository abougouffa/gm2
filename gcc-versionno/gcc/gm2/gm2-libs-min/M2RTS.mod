(* M2RTS.mod provides dummy routines to enable GNU Modula-2.

Copyright (C) 2001-2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with GNU Modula-2.  If not, see <https://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2RTS ;

IMPORT libc, SYSTEM ;
(* we reference these to ensure they are dragged in to the link *)


(*
   all these procedures do nothing except satisfy the linker.
*)

PROCEDURE ExecuteTerminationProcedures ;
BEGIN
END ExecuteTerminationProcedures ;


PROCEDURE ExecuteInitialProcedures ;
BEGIN
END ExecuteInitialProcedures ;


PROCEDURE HALT ;
BEGIN
END HALT ;


PROCEDURE NoException (filename: ADDRESS;
                       line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
END NoException ;


END M2RTS.
