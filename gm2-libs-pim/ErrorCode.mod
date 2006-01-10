(* Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA *)

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
