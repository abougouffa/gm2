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
MODULE testscope ;


PROCEDURE ToThePower (i, n: CARDINAL) : CARDINAL ;
VAR
   bar: CARDINAL ;
BEGIN
   IF n=0
   THEN
      RETURN( 1 )
   ELSE
      bar := n ;
      WHILE bar>1 DO
         i := i*i ;
         DEC(bar)
      END ;
      RETURN( bar )
   END
END ToThePower ;


VAR
   bar: CARDINAL ;
BEGIN
   bar := ToThePower(2, 3)
END testscope.
