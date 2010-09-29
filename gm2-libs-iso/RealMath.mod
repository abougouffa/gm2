(* Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE RealMath ;

IMPORT libm ;
IMPORT cbuiltin ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sqrt)) sqrt (x: REAL): REAL;
  (* Returns the positive square root of x *)
BEGIN
   RETURN cbuiltin.sqrt(x)
END sqrt ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_exp)) exp (x: REAL): REAL;
  (* Returns the exponential of x *)
BEGIN
   RETURN cbuiltin.exp(x)
END exp ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_log)) ln (x: REAL): REAL;
  (* Returns the natural logarithm of x *)
BEGIN
   RETURN cbuiltin.log(x)
END ln ;

  (* The angle in all trigonometric functions is measured in radians *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sin)) sin (x: REAL): REAL;
  (* Returns the sine of x *)
BEGIN
   RETURN cbuiltin.sin(x)
END sin ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cos)) cos (x: REAL): REAL;
  (* Returns the cosine of x *)
BEGIN
   RETURN cbuiltin.cos(x)
END cos ;

PROCEDURE tan (x: REAL): REAL;
  (* Returns the tangent of x *)
BEGIN
   RETURN libm.tan(x)
END tan ;

PROCEDURE arcsin (x: REAL): REAL;
  (* Returns the arcsine of x *)
BEGIN
   RETURN libm.asin(x)
END arcsin ;

PROCEDURE arccos (x: REAL): REAL;
  (* Returns the arccosine of x *)
BEGIN
   RETURN libm.acos(x)
END arccos ;

PROCEDURE arctan (x: REAL): REAL;
  (* Returns the arctangent of x *)
BEGIN
   RETURN libm.atan(x)
END arctan ;

PROCEDURE power (base, exponent: REAL): REAL;
  (* Returns the value of the number base raised to the power exponent *)
BEGIN
   RETURN libm.pow(base, exponent)
END power ;

PROCEDURE round (x: REAL): INTEGER;
  (* Returns the value of x rounded to the nearest integer *)
BEGIN
   RETURN TRUNC(x)  (* hmm we could provide access to the GNU Modula-2 built-in *)
END round ;

PROCEDURE IsRMathException (): BOOLEAN;
  (* Returns TRUE if the current coroutine is in the exceptional execution state
     because of the raising of an exception in a routine from this module; otherwise
     returns FALSE.
  *)
BEGIN
   RETURN FALSE
END IsRMathException ;

END RealMath.
