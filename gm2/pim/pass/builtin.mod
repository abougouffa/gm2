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

IMPLEMENTATION MODULE builtin ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_alloca)) alloca (i: CARDINAL) : ADDRESS ;
BEGIN
   RETURN NIL
END alloca ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_memcpy)) memcopy (dest, src: ADDRESS; n: CARDINAL) : ADDRESS ;
BEGIN
   RETURN dest
END memcopy ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sinf)) sinf (x: REAL) : REAL ;
BEGIN
   RETURN 0.0
END sinf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sinl)) sinl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN 0.0
END sinl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_memset)) memset (s: ADDRESS; c: INTEGER; n: CARDINAL) : ADDRESS ;
BEGIN
   RETURN s
END memset ;

END builtin.