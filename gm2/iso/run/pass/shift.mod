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

MODULE shift ;

FROM libc IMPORT exit ;
FROM SYSTEM IMPORT SHIFT ;

VAR
   b: BITSET ;
BEGIN
   b := BITSET{1, 2, 3} ;
   b := SHIFT(b, 1) ;
   IF b=BITSET{2, 3, 4}
   THEN
      exit(0)
   ELSE
      exit(1)
   END
END shift.
