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
MODULE prog31ex;
FROM StrIO IMPORT WriteLn,WriteString;
FROM FpuIO IMPORT ReadReal, WriteReal;

VAR
   fahrenheit      : REAL;
   celsius         : REAL;
BEGIN
   celsius := -10.0;
   fahrenheit := 0.0;
   WHILE celsius <= 100.0 DO
      WriteReal(celsius,6,2);
      fahrenheit := ((celsius * 9.0) / 5.0) + 32.0;
      WriteString(" degrees Celsius goes to ") ;
      WriteReal(fahrenheit,6,2);
      WriteString(" degrees Fahrenheit");
      WriteLn;
      celsius := celsius + 5.0;
   END
END prog31ex.