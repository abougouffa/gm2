(* Copyright (C) 2001 Free Software Foundation, Inc. *)
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
MODULE testscn ;


CONST
   Width     = 80 ;
   Height    = 25 ;

TYPE
   DisplayUnit = RECORD
                    char  : CHAR ;
                    attrib: CHAR ;
                 END ;

   Line        = ARRAY [0..Width-1] OF DisplayUnit ;
   Screen      = ARRAY [0..Height-1] OF Line ;

VAR
   screen    : POINTER TO Screen ;


PROCEDURE ScrollUp ;
VAR
   j: CARDINAL ;
BEGIN
   FOR j := 0 TO Height-2 DO
      screen^[j] := screen^[j+1]
   END
END ScrollUp ;


BEGIN
   ScrollUp
END testscn.