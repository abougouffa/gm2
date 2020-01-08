(* ErrorCode.mod provides a Logitech compatible module.

Copyright (C) 2004-2020 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE ErrorCode ;

FROM M2RTS IMPORT ExitOnHalt ;

VAR
   exitCode: INTEGER ;


(*
   SetErrorCode - sets the exit value which will be used if
                  the application terminates normally.
*)

PROCEDURE SetErrorCode (value: INTEGER) ;
BEGIN
   exitCode := value ;
   ExitOnHalt(value)
END SetErrorCode ;


(*
   GetErrorCode - returns the current value to be used upon
                  application termination.
*)

PROCEDURE GetErrorCode (VAR value: INTEGER) ;
BEGIN
   value := exitCode
END GetErrorCode ;


(*
   ExitToOS - terminate the application and exit returning
              the last value set by SetErrorCode to the OS.
*)

PROCEDURE ExitToOS ;
BEGIN
   HALT
END ExitToOS ;


BEGIN
   exitCode := 0
END ErrorCode.
