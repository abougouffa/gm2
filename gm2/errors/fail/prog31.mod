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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)
(*
 * Program written by iptatton
 * Created on Tue Nov 11 09:06:16 GMT 1997
 * sdp course
*)

(* to Convert Celsius to Fahrenheit *)

MODULE prog31;
FROM StrIO IMPORT WriteLn,WriteString;
FROM FpuIO IMPORT ReadReal, WriteReal;
FROM NumberIO IMPORT WriteInt;

celsius : REAL;
end_of_range : INTEGER;

BEGIN
    celsius :=  -10.0;
    fahrenheit := 000000.00;
    end_of_range := 100;

    (*WriteString('Enter the celsius value :');*)
    (*ReadReal(celsius);*)
    (*WriteLn;*)


    WHILE (celsius < 100.0) DO
       fahrenheit := ( (9.00 * celsius ) / 5.00 ) + 32.00;
       WriteReal(celsius,6,2);
       WriteString(" degrees Celsius goes to ") ;
       WriteReal(fahrenheit,6,2);
       WriteString(" degrees Fahrenheit");
       WriteLn;
       celsius := (celsius + 5.0);
    END;
END prog31.
