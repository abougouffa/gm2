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
MODULE prog110;

FROM StrIO IMPORT WriteString, WriteLn;
FROM NumberIO IMPORT ReadInt, WriteInt;

VAR
    n : INTEGER ;      (* Number to divide *)
    d : INTEGER ;      (* Divisor *)
    result : INTEGER;  (* Result from divide *)
    r : INTEGER ;      (* Remainder *)
    c : CHAR ;         (* Answer from user to continue y/n *)

BEGIN
   c := "Y";          (* set this to Y so it does the loop the first
time *)
   
   WHILE c = "y" OR c = "Y" DO
      WriteString "Input Number to divide (n)";
      ReadInt (n);
      WriteString "Input Number to divide by n";
      ReadInt (d);
      
      R := 0;
      result := N DIV D;

   END
END prog110.
