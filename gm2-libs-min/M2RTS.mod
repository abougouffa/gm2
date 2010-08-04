(* Copyright (C) 2006 Free Software Foundation, Inc. *)
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
