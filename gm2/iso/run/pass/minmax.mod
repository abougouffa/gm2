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

MODULE minmax ;


TYPE
   color = (red, blue, green) ;
   colourset = SET OF color ;

VAR
   c1, c2: color ;
   b1, b2: CARDINAL ;
BEGIN
   c1 := MIN(colourset) ;
   c2 := MAX(colourset) ;
(*
   Assert(c1=red) ;
   Assert(c2=green) ;
*)
   b1 := MIN(BITSET) ;
   b2 := MAX(BITSET)
END minmax.