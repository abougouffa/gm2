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
MODULE testloop2 ;


FROM SYSTEM IMPORT ADR ;

PROCEDURE foo ;
VAR
   p: POINTER TO CARDINAL ;
BEGIN
   i := 1 ;
   p := ADR(i) ;
   WHILE i#0 DO
      IF TRUE
      THEN
         dec(i)
      END ;
      IF i>0
      THEN
      ELSE
      END ;
      WHILE i=0 DO
      END ;
      REPEAT
      UNTIL i=0
   END
END foo ;

PROCEDURE dec (VAR i: CARDINAL) ;
BEGIN
END dec ;

PROCEDURE bar ;
BEGIN
END bar ;

VAR
   i: CARDINAL ;
BEGIN
   foo
END testloop2.
