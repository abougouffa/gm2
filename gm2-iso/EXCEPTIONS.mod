(* Copyright (C) 2003 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE EXCEPTIONS ;

TYPE
  ExceptionSource = POINTER TO RECORD

                    END ;
                           (* values of this type are used within library modules to
                              identify the source of raised exceptions *)

PROCEDURE AllocateSource(VAR newSource: ExceptionSource);
  (* Allocates a unique value of type ExceptionSource *)
BEGIN
   
END AllocateSource ;


PROCEDURE RAISE (source: ExceptionSource; number: ExceptionNumber; message: ARRAY OF CHAR);
  (* Associates the given values of source, number and message with the current context
     and raises an exception.
  *)
BEGIN
   
END RAISE ;


PROCEDURE CurrentNumber (source: ExceptionSource): ExceptionNumber;
  (* If the current coroutine is in the exceptional execution state because of the raising
     of an exception from source, returns the corresponding number, and otherwise
     raises an exception.
  *)
BEGIN
   
END CurrentNumber ;


PROCEDURE GetMessage (VAR text: ARRAY OF CHAR);
  (* If the current coroutine is in the exceptional execution state, returns the possibly
     truncated string associated with the current context.
     Otherwise, in normal execution state, returns the empty string.
  *)
BEGIN
   
END GetMessage ;


PROCEDURE IsCurrentSource (source: ExceptionSource): BOOLEAN;
  (* If the current coroutine is in the exceptional execution state because of the raising
     of an exception from source, returns TRUE, and otherwise returns FALSE.
  *)
BEGIN
   
END IsCurrentSource ;


PROCEDURE IsExceptionalExecution (): BOOLEAN;
  (* If the current coroutine is in the exceptional execution state because of the raising
     of an exception, returns TRUE, and otherwise returns FALSE.
  *)
BEGIN
   
END IsExceptionalExecution ;


END EXCEPTIONS.
