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
MODULE testfp ;

FROM MATH IMPORT atan, sin ;
FROM FpuIO IMPORT WriteLongReal ;
FROM StrIO IMPORT WriteString, WriteLn ;

VAR
   x: LONGREAL ;
BEGIN
   x := atan(0.4) ;
   WriteLongReal(x, 20, 10) ; WriteLn ;
   x := sin(0.4) ;
   WriteLongReal(x, 20, 10) ; WriteLn
END testfp.
