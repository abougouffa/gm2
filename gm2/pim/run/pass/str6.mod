(* Copyright (C) 2005, 2006 Free Software Foundation, Inc. *)
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
MODULE str6 ;
 
FROM StrIO IMPORT WriteString, WriteLn;
FROM StrLib IMPORT StrEqual ;
FROM M2RTS IMPORT Terminate ;
FROM libc IMPORT exit ;
 
TYPE
   String = ARRAY [0..255] OF CHAR;
 
VAR
   Str : String;
BEGIN
   Str := 'abcdefghij';
   WriteString(Str);
   WriteLn;
   Str := '1234';
   WriteString(Str);
   WriteLn ;
   IF NOT StrEqual(Str, '1234')
   THEN
      Terminate ;
      exit(1)
   END
END str6.