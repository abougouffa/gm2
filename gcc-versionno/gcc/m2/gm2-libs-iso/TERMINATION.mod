(* TERMINATION.mod implement the ISO TERMINATION specification.

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

IMPLEMENTATION MODULE TERMINATION ;

IMPORT M2RTS ;

(* Provides facilities for enquiries concerning the occurrence of termination events. *)

VAR
   terminating: BOOLEAN ;


(* Returns true if any coroutine has started  program termination and false otherwise. *)

PROCEDURE IsTerminating (): BOOLEAN ;
BEGIN
   RETURN M2RTS.IsTerminating()
END IsTerminating ;


(* Returns true if a call to HALT has been made and false otherwise. *)

PROCEDURE HasHalted (): BOOLEAN ;
BEGIN
   RETURN M2RTS.HasHalted()
END HasHalted ;


END TERMINATION.
