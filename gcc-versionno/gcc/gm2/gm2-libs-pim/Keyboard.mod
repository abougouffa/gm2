(* Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA *)

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
