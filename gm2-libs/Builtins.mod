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

IMPLEMENTATION MODULE Builtins ;

IMPORT cbuiltin ;


PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((alloca)) alloca (i: CARDINAL) : ADDRESS ;
BEGIN
   (* hopefully the compiler will choose to use the __builtin_alloca function within GCC.
      This call is here just in case it cannot. Ie if the user sets a procedure variable to
      alloca, then clearly the compiler cannot inline such a call and thus it will
      be forced into calling this function.
   *)
   RETURN cbuiltin.__builtin_alloca (i)
END alloca ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((memcpy)) memcpy (dest, src: ADDRESS; n: CARDINAL) : ADDRESS ;
BEGIN
   RETURN cbuiltin.memcpy (dest, src, n)
END memcpy ;


END Builtins.
