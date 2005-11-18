(* Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE MathLib0 ;

IMPORT cbuiltin, libm ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sqrt)) sqrt (x: REAL): REAL;
BEGIN
   RETURN cbuiltin.sqrt (x)
END sqrt ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sqrtl)) sqrtl (x: LONGREAL): LONGREAL;
BEGIN
   RETURN cbuiltin.sqrtl (x)
END sqrtl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sqrts)) sqrts (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.sqrtf (x)
END sqrts ;

PROCEDURE exp (x: REAL) : REAL ;
BEGIN
   RETURN libm.exp (x)   
END exp ;

PROCEDURE exps (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN VAL(SHORTREAL, libm.exp (VAL(REAL, x)))
END exps ;

(*
                log (b)
   log (b)  =      c
      a         ------
                log (a)
                   c
*)

PROCEDURE ln (x: REAL) : REAL ;
BEGIN
   RETURN libm.log (x) / libm.log (exp1)
END ln ;

PROCEDURE lns (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN VAL(SHORTREAL, libm.log (VAL(REAL, x)) / libm.log (exp1))
END lns ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sin)) sin (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.sin (x)
END sin ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sinl)) sinl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.sinl (x)
END sinl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sinf)) sins (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.sinf (x)
END sins ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cos)) cos (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.cos (x)
END cos ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cosf)) coss (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.cosf (x)
END coss ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cosl)) cosl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.cosl (x)
END cosl ;

PROCEDURE tan (x: REAL) : REAL ;
BEGIN
   RETURN libm.tan (x)
END tan ;

PROCEDURE tanl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN sinl (x) / cosl (x)
END tanl ;

PROCEDURE tans (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN VAL(SHORTREAL, libm.tan (VAL (REAL, x)))
END tans ;

PROCEDURE arctan (x: REAL) : REAL ;
BEGIN
   RETURN libm.atan (x)
END arctan ;

PROCEDURE arctans (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN VAL(SHORTREAL, libm.atan (VAL (REAL, x)))
END arctans ;

PROCEDURE entier (x: REAL) : INTEGER ;
BEGIN
   RETURN TRUNC (libm.floor (x))
END entier ;

PROCEDURE entiers (x: SHORTREAL) : INTEGER ;
BEGIN
   RETURN TRUNC (libm.floor (VAL(REAL, x)))
END entiers ;


END MathLib0.
