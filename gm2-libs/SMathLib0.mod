(* Copyright (C) 2009 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA *)

IMPLEMENTATION MODULE SMathLib0 ;

IMPORT cbuiltin, libm ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sqrts)) sqrt (x: SHORTREAL): SHORTREAL;
BEGIN
   RETURN cbuiltin.sqrts (x)
END sqrt ;

PROCEDURE exp (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN libm.exps (x)
END exp ;


(*
                log (b)
   log (b)  =      c
      a         ------
                log (a)
                   c
*)

PROCEDURE ln (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN libm.logs (x) / libm.logs (exp1)
END ln ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sins)) sin (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.sins (x)
END sin ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_coss)) cos (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.coss (x)
END cos ;

PROCEDURE tan (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN libm.tans (x)
END tan ;

PROCEDURE arctan (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN libm.atans (x)
END arctan ;

PROCEDURE entier (x: SHORTREAL) : INTEGER ;
BEGIN
   RETURN TRUNC (libm.floors (x))
END entier ;


END SMathLib0.
