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

MODULE testmod2 ;


MODULE m1 ;

EXPORT M1 ;
TYPE
   colour = (red, blue, green) ;
   M1 = colour ;
END m1 ;

MODULE m2 ;

EXPORT M2 ;
IMPORT M1 ;

TYPE
   M2 = M1 ;
END m2 ;

TYPE
   
CONST
   c = M2.red ;

END testmod2.
