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

IMPLEMENTATION MODULE Keyboard ;

FROM Selective IMPORT SetOfFd, InitSet, KillSet, MaxFdsPlusOne, ReadCharRaw,
                      Timeval, InitTime, KillTime, FdIsSet, FdZero, FdSet,
                      Select ;


CONST
   stdin = 0 ;


(*
   Read - reads a character from StdIn. If necessary it will wait
          for a key to become present on StdIn.
*)

PROCEDURE Read (VAR ch: CHAR) ;
BEGIN
   ch := ReadCharRaw(stdin)
END Read ;


(*
   KeyPressed - returns TRUE if a character can be read from StdIn
                without blocking the caller.
*)

PROCEDURE KeyPressed () : BOOLEAN ;
VAR
   s      : SetOfFd ;
   t      : Timeval ;
   r      : INTEGER ;
   Pressed: BOOLEAN ;
BEGIN
   t := InitTime(0, 0) ;
   s := InitSet() ;
   FdZero(s) ;
   FdSet(stdin, s) ;
   r := Select(MaxFdsPlusOne(stdin, stdin),
               s, NIL, NIL, t) ;
   Pressed := FdIsSet(stdin, s) ;
   s := KillSet(s) ;
   t := KillTime(t) ;
   RETURN( Pressed )
END KeyPressed ;


END Keyboard.
