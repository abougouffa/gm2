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
Foundation, 59 Temple Place - Suite 330, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE Builtins ;

IMPORT cbuiltin ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_alloca)) alloca (i: CARDINAL) : ADDRESS ;
BEGIN
   HALT ;  (* not allowed to call alloca yet, it can only be expanded as a built-in *)
   RETURN NIL
END alloca ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_memcpy)) memcpy (dest, src: ADDRESS; n: CARDINAL) : ADDRESS ;
BEGIN
   (* hopefully the compiler will choose to use the __builtin_memcpy function within GCC.
      This call is here just in case it cannot. Ie if the user sets a procedure variable to
      memcpy, then clearly the compiler cannot inline such a call and thus it will
      be forced into calling this function.
   *)
   RETURN cbuiltin.memcpy (dest, src, n)
END memcpy ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sin)) sin (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.sin (x)
END sin ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sinf)) sinf (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.sinf (x)
END sinf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sinl)) sinl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.sinl (x)
END sinl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cos)) cos (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.cos (x)
END cos ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cosf)) cosf (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.cosf (x)
END cosf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cosl)) cosl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.cosl (x)
END cosl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sqrt)) sqrt (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.sqrt (x)
END sqrt ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sqrtf)) sqrtf (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.sqrtf (x)
END sqrtf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sqrtl)) sqrtl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.sqrtl (x)
END sqrtl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_fabs)) fabs (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.fabs (x)
END fabs ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_fabsf)) fabsf (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.fabsf (x)
END fabsf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_fabsl)) fabsl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.fabsl (x)
END fabsl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_index)) index (s: ADDRESS; c: INTEGER) : ADDRESS ;
BEGIN
   RETURN cbuiltin.index (s, c)
END index ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_rindex)) rindex (s: ADDRESS; c: INTEGER) : ADDRESS ;
BEGIN
   RETURN cbuiltin.rindex (s, c)
END rindex ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_memcmp)) memcmp (s1, s2: ADDRESS; n: CARDINAL) : INTEGER ;
BEGIN
   RETURN cbuiltin.memcmp (s1, s2, n)
END memcmp ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_memset)) memset (s: ADDRESS; c: INTEGER; n: CARDINAL) : ADDRESS ;
BEGIN
   RETURN cbuiltin.memset (s, c, n)
END memset ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strcat)) strcat (dest, src: ADDRESS) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strcat (dest, src)
END strcat ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strncat)) strncat (dest, src: ADDRESS; n: CARDINAL) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strncat (dest, src, n)
END strncat ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strcpy)) strcpy (dest, src: ADDRESS) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strcpy (dest, src)
END strcpy ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strncpy)) strncpy (dest, src: ADDRESS; n: CARDINAL) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strncpy (dest, src, n)
END strncpy ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strcmp)) strcmp (s1, s2: ADDRESS) : INTEGER ;
BEGIN
   RETURN cbuiltin.strcmp (s1, s2)
END strcmp ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strncmp)) strncmp (s1, s2: ADDRESS; n: CARDINAL) : INTEGER ;
BEGIN
   RETURN cbuiltin.strncmp (s1, s2, n)
END strncmp ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strlen)) strlen (s: ADDRESS) : INTEGER ;
BEGIN
   RETURN cbuiltin.strlen (s)
END strlen ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strstr)) strstr (haystack, needle: ADDRESS) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strstr (haystack, needle)
END strstr ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strpbrk)) strpbrk (s, accept: ADDRESS) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strpbrk (s, accept)
END strpbrk ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strspn)) strspn (s, accept: ADDRESS) : CARDINAL ;
BEGIN
   RETURN cbuiltin.strspn (s, accept)
END strspn ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strcspn)) strcspn (s, accept: ADDRESS) : CARDINAL ;
BEGIN
   RETURN cbuiltin.strcspn (s, accept)
END strcspn ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strchr)) strchr (s: ADDRESS; c: INTEGER) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strchr (s, c)
END strchr ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strrchr)) strrchr (s: ADDRESS; c: INTEGER) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strrchr (s, c)
END strrchr ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_huge_val)) huge_val (r: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.huge_val (r)
END huge_val ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_huge_vall)) huge_vall (l: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.huge_vall (l)
END huge_vall ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_huge_valf)) huge_valf (s: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.huge_valf (s)
END huge_valf ;

END Builtins.
