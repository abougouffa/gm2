(* StdFuncs.mod.

Copyright (C) 2004-2019 Free Software Foundation, Inc.
Contributed by University of Ulm.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)


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
