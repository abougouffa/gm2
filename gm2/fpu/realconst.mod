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
MODULE realconst;
(*A program to convert an angle in degrees to an angle in radians*)

FROM StrIO IMPORT WriteString, WriteLn;		 
FROM MATH IMPORT pi;
FROM FpuIO IMPORT WriteReal, ReadReal;

VAR
   Degrees, Radians: REAL;


PROCEDURE ConvertAngle (Angle:REAL): REAL;
(*Convert an angle from degrees to radians*)
VAR
   ConversionFactor : REAL;
BEGIN
   ConversionFactor:= pi / 180.0;
   WriteReal(ConversionFactor, 20, 15 );
   RETURN ConversionFactor * Angle
END ConvertAngle;

BEGIN (*convert*)
   WriteString("Enter an angle in degrees:  ");
   ReadReal(Degrees); WriteLn;
   Radians := ConvertAngle (Degrees);
   WriteString("The angle in radians is:  ");
   WriteReal(Radians, 20, 5 ); WriteLn;
END realconst.
