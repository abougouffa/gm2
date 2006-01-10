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

IMPLEMENTATION MODULE Termbase ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2RTS IMPORT Halt ;
IMPORT Display, Keyboard ;

TYPE
   ReadMethods = POINTER TO RECORD
                               r   : ReadProcedure ;
                               s   : StatusProcedure ;
                               next: ReadMethods ;
                            END ;

   WriteMethod = POINTER TO RECORD
                               w   : WriteProcedure ;
                               next: WriteMethod ;
                            END ;

VAR
   rStack: ReadMethods ;
   wStack: WriteMethod ;


(*
   AssignRead - assigns a read procedure and status procedure for terminal
                input. Done is set to TRUE if successful. Subsequent
                Read and KeyPressed calls are mapped onto the user supplied
                procedures. The previous read and status procedures are
                uncovered and reused after UnAssignRead is called.
*)

PROCEDURE AssignRead (rp: ReadProcedure; sp: StatusProcedure;
                      VAR Done: BOOLEAN) ;
VAR
   t: ReadMethods ;
BEGIN
   t := wStack ;
   NEW(rStack) ;
   IF rStack=NIL
   THEN
      Done := FALSE
   ELSE
      WITH rStack^ DO
         r := rp ;
         s := sp ;
         next := t
      END ;
      Done := TRUE
   END
END AssignRead ;


(*
   UnAssignRead - undo the last call to AssignRead and set Done to TRUE
                  on success.
*)

PROCEDURE UnAssignRead (VAR Done: BOOLEAN) ;
VAR
   t: ReadMethods ;
BEGIN
   IF rStack=NIL
   THEN
      Done := FALSE
   ELSE
      Done := TRUE
   END ;
   t := rStack ;
   rStack := rStack^.next ;
   DISPOSE(t)
END UnAssignRead ;


(*
   Read - reads a single character using the currently active read
          procedure.
*)

PROCEDURE Read (VAR ch: CHAR) ;
BEGIN
   IF rStack=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'no active read procedure')
   ELSE
      rStack^.r(ch)
   END
END Read ;


(*
   KeyPressed - returns TRUE if a character is available to be read.
*)

PROCEDURE KeyPressed () : BOOLEAN ;
BEGIN
   IF rStack=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'no active status procedure')
   ELSE
      RETURN( rStack^.s() )
   END
END KeyPressed ;


(*
   AssignWrite - assigns a write procedure for terminal output.
                 Done is set to TRUE if successful. Subsequent
                 Write calls are mapped onto the user supplied
                 procedure. The previous write procedure is
                 uncovered and reused after UnAssignWrite is called.
*)

PROCEDURE AssignWrite (wp: WriteProcedure; VAR Done: BOOLEAN) ;
VAR
   t: WriteMethod ;
BEGIN
   t := wStack ;
   NEW(wStack) ;
   IF wStack=NIL
   THEN
      Done := FALSE
   ELSE
      WITH wStack^ DO
         w := wp ;
         next := t
      END ;
      Done := TRUE
   END
END AssignWrite ;


(*
   UnAssignWrite - undo the last call to AssignWrite and set Done to TRUE
                   on success.
*)

PROCEDURE UnAssignWrite (VAR Done: BOOLEAN) ;
VAR
   t: WriteMethod ;
BEGIN
   IF wStack=NIL
   THEN
      Done := FALSE
   ELSE
      Done := TRUE
   END ;
   t := wStack ;
   wStack := wStack^.next ;
   DISPOSE(t)
END UnAssignWrite ;


(*
   Write - writes a single character using the currently active write
           procedure.
*)

PROCEDURE Write (VAR ch: CHAR) ;
BEGIN
   IF wStack=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'no active write procedure')
   ELSE
      wStack^.w(ch)
   END
END Write ;


(*
   Init - 
*)

PROCEDURE Init ;
VAR
   Done: BOOLEAN ;
BEGIN
   rStack := NIL ;
   wStack := NIL ;
   AssignRead(Keyboard.Read, Keyboard.KeyPressed, Done) ;
   IF NOT Done
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'failed to assign read routines from module Keyboard')
   END ;
   AssignWrite(Display.Write, Done) ;
   IF NOT Done
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'failed to assign write routine from module Display')
   END
END Init ;


BEGIN
   Init
END Termbase.
