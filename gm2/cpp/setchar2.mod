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

MODULE setchar2 ;

TYPE
   smallchar = SET OF ['A'..'Z'] ;

VAR
   s : smallchar ;
   ch: CHAR ;
BEGIN
#if 1
   s := smallchar {} ;
   s := smallchar {'B', 'D', 'G'} ;
   s := smallchar {'H', 'I'} ;
#endif
   s := smallchar {'A', 'Z'} ;
#if 1
   s := s + smallchar {'H', 'I'} ;
   s := smallchar {'H', 'I'} ;
#endif
END setchar2.
