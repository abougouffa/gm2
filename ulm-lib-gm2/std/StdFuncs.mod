(* Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc. *)
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

This file was originally part of the University of Ulm library
*)


(* Ulm's Modula-2 Library
   Copyright (C) 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991,
   1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003, 2004, 2005
   by University of Ulm, SAI, D-89069 Ulm, Germany
*)

IMPLEMENTATION MODULE StdFuncs;

   FROM Functions IMPORT Real,
      InstallStdFunc1, InstallStdFunc2, InstallStdConst;
   FROM MathLib IMPORT arctan, exp, ln, sin, cos, sqrt;

   PROCEDURE Power(x, y: Real) : Real;
   BEGIN
      RETURN exp(y*ln(x))
   END Power;

   PROCEDURE Log(base, x: Real) : Real;
   BEGIN
      RETURN ln(x)/ln(base)
   END Log;

   PROCEDURE Log10(x: Real) : Real;
   BEGIN
      RETURN Log(10.0, x)
   END Log10;

   PROCEDURE Tan(x: Real) : Real;
   BEGIN
      RETURN sin(x)/cos(x)
   END Tan;

BEGIN
   InstallStdFunc1("arctan", arctan);
   InstallStdFunc1("exp", exp);
   InstallStdFunc1("ln", ln);
   InstallStdFunc1("sin", sin);
   InstallStdFunc1("cos", cos);
   InstallStdFunc1("sqrt", sqrt);
   InstallStdFunc1("log10", Log10);
   InstallStdFunc1("tan", Tan);

   InstallStdFunc2("log", Log);
   InstallStdFunc2("pow", Power);

   InstallStdConst("pi", 4.0*arctan(1.0));
   InstallStdConst("e", exp(1.0));
END StdFuncs.
