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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE Delay ;

FROM Selective IMPORT Timeval, InitTime, KillTime, Select ;


PROCEDURE Delay (milliSec: INTEGER) ;
VAR
   t: Timeval ;
   r: INTEGER ;
BEGIN
   t := InitTime(0, milliSec*1000) ;
   r := Select(0, NIL, NIL, NIL, t) ;
   t := KillTime(t)
END Delay ;


END Delay.