(* Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

It originated from p2c-1.20 another GNU Project "p2c" file: src/system.m2
*)

(* "p2c", a Pascal to C translator, version 1.20.
   Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
                 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation.
   Author: Dave Gillespie.
   Author's address: daveg@csvax.caltech.edu; 256-80 Caltech/Pasadena CA 91125.
*)

(* Declarations for Modula-2 built-in objects *)

(* Note: All functions with unusual syntaxes are not included here *)



DEFINITION MODULE SYSTEM;   (*PERMANENT*)

TYPE
   ADDRESS = POINTER TO WORD;

FUNCTION  CAP(c : CHAR) : CHAR;
FUNCTION  CHR(i : INTEGER) : CHAR;
FUNCTION  ODD(i : INTEGER) : BOOLEAN;
FUNCTION  ROUND(x : REAL) : INTEGER;
FUNCTION  TRUNC(x : REAL) : INTEGER;
(* Other things are defined internally to p2c *)

END;



DEFINITION MODULE InOut;



END;



DEFINITION MODULE MathLib0;

FUNCTION  arctan(x : REAL) : REAL;
FUNCTION  cos(x : REAL) : REAL;
FUNCTION  entier(x : REAL) : INTEGER;
FUNCTION  exp(x : REAL) : REAL;
FUNCTION  ln(x : REAL) : REAL;
FUNCTION  real(i : INTEGER) : REAL;
FUNCTION  sin(x : REAL) : REAL;
FUNCTION  sqrt(x : REAL) : REAL;

END.


