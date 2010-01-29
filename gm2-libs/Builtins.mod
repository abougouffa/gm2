(* Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc.  *)
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

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

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_log)) log (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.log (x)
END log ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_logf)) logf (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.logf (x)
END logf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_logl)) logl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.logl (x)
END logl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_exp)) exp (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.exp (x)
END exp ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_expf)) expf (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.expf (x)
END expf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_expl)) expl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.expl (x)
END expl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_log10)) log10 (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.log10 (x)
END log10 ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_log10f)) log10f (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.log10f (x)
END log10f ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_log10l)) log10l (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.log10l (x)
END log10l ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_exp10)) exp10 (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.exp10 (x)
END exp10 ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_exp10f)) exp10f (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.exp10f (x)
END exp10f ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_exp10l)) exp10l (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.exp10l (x)
END exp10l ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_ilogb)) ilogb (x: REAL) : INTEGER ;
BEGIN
   RETURN cbuiltin.ilogb (x)
END ilogb ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_ilogbf)) ilogbf (x: SHORTREAL) : INTEGER ;
BEGIN
   RETURN cbuiltin.ilogbf (x)
END ilogbf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_ilogbl)) ilogbl (x: LONGREAL) : INTEGER ;
BEGIN
   RETURN cbuiltin.ilogbl (x)
END ilogbl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cabsf)) cabsf (z: SHORTCOMPLEX) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.cabsf(z)
END cabsf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cabs)) cabs (z: COMPLEX) : REAL ;
BEGIN
   RETURN cbuiltin.cabs(z)
END cabs ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cabsl)) cabsl (z: LONGCOMPLEX) : LONGREAL ;
BEGIN
   RETURN cbuiltin.cabsl(z)
END cabsl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cargf)) cargf (z: SHORTCOMPLEX) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.cargf(z)
END cargf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_carg)) carg (z: COMPLEX) : REAL ;
BEGIN
   RETURN cbuiltin.carg(z)
END carg ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cargl)) cargl (z: LONGCOMPLEX) : LONGREAL ;
BEGIN
   RETURN cbuiltin.cargl(z)
END cargl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_conjf)) conjf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.conjf(z)
END conjf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_conj)) conj (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.conj(z)
END conj ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_conjl)) conjl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.conjl(z)
END conjl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cpowerf)) cpowerf (base: SHORTCOMPLEX; exp: SHORTREAL) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.cpowerf(base, exp)
END cpowerf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cpower)) cpower (base: COMPLEX; exp: REAL) : COMPLEX ;
BEGIN
   RETURN cbuiltin.cpower(base, exp)
END cpower ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cpowerl)) cpowerl (base: LONGCOMPLEX; exp: LONGREAL) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.cpowerl(base, exp)
END cpowerl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csqrtf)) csqrtf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.csqrtf(z)   
END csqrtf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csqrt)) csqrt (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.csqrt(z)
END csqrt ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csqrtl)) csqrtl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.csqrtl(z)
END csqrtl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cexpf)) cexpf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.cexpf(z)   
END cexpf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cexp)) cexp (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.cexp(z)
END cexp ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cexpl)) cexpl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.cexpl(z)
END cexpl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_clnf)) clnf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.clnf(z)   
END clnf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cln)) cln (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.cln(z)
END cln ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_clnl)) clnl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.clnl(z)
END clnl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csinf)) csinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.csinf(z)   
END csinf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csin)) csin (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.csin(z)
END csin ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csinl)) csinl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.csinl(z)
END csinl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ccosf)) ccosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.ccosf(z)   
END ccosf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ccos)) ccos (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.ccos(z)
END ccos ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ccosl)) ccosl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.ccosl(z)
END ccosl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ctanf)) ctanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.ctanf(z)   
END ctanf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ctan)) ctan (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.ctan(z)
END ctan ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ctanl)) ctanl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.ctanl(z)
END ctanl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carcsinf)) carcsinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.carcsinf(z)   
END carcsinf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carcsin)) carcsin (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.carcsin(z)
END carcsin ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carcsinl)) carcsinl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.carcsinl(z)
END carcsinl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carccosf)) carccosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.carccosf(z)   
END carccosf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carccos)) carccos (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.carccos(z)
END carccos ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carccosl)) carccosl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.carccosl(z)
END carccosl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carctanf)) carctanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.carctanf(z)   
END carctanf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carctan)) carctan (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.carctan(z)
END carctan ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carctanl)) carctanl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.carctanl(z)
END carctanl ;

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

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_memmove)) memmove (s1, s2: ADDRESS; n: CARDINAL) : ADDRESS ;
BEGIN
   RETURN cbuiltin.memmove (s1, s2, n)
END memmove ;

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
   RETURN -1.0
END huge_val ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_huge_vall)) huge_vall (l: LONGREAL) : LONGREAL ;
BEGIN
   RETURN -1.0
END huge_vall ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_huge_valf)) huge_valf (s: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN -1.0
END huge_valf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_longjmp)) longjmp (env: ADDRESS; val: INTEGER) ;
BEGIN
   (* empty, replaced internally by gcc *)
END longjmp ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_setjmp)) setjmp (env: ADDRESS) : INTEGER ;
BEGIN
   (* empty, replaced internally by gcc *)
   RETURN 0   (* keeps gm2 happy *)
END setjmp ;


(*
   frame_address - returns the address of the frame.
                   The current frame is obtained if level is 0,
                   the next level up is level is 1 etc.
*)

PROCEDURE __ATTRIBUTE__ __BUILTIN__
         ((__builtin_frame_address))
         frame_address (level: CARDINAL) : ADDRESS ;
BEGIN
   RETURN NIL
END frame_address ;


(*
   return_address - returns the return address of function.
                    The current function return address is
                    obtained if level is 0,
                    the next level up is level is 1 etc.
*)

PROCEDURE __ATTRIBUTE__ __BUILTIN__
         ((__builtin_return_address))
         return_address (level: CARDINAL) : ADDRESS ;
BEGIN
   RETURN NIL
END return_address ;


END Builtins.
