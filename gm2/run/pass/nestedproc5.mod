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
MODULE nestedproc5 ;

FROM StrIO IMPORT WriteString, WriteLn ;
FROM StrLib IMPORT StrCopy, StrLen ;

PROCEDURE outer ;
VAR
   a: ARRAY [0..80] OF CHAR ;

   PROCEDURE flip (i, j: CARDINAL) ;
   VAR
      t: CHAR ;
   BEGIN
      t := a[i] ;
      a[i] := a[j] ;
      a[j] := t
   END flip ;

   PROCEDURE inner ;
   VAR
      h, l, k: CARDINAL ;
   BEGIN
      h := HIGH(a) ;  (* test it.. *)
      IF h#80
      THEN
         HALT
      END ;
      k := 0 ;
      l := StrLen(a)-1 ;
      flip(3, 8)
   END inner ;
BEGIN
   StrCopy('0128456739', a) ;
   inner ;
   WriteString(a) ; WriteLn
END outer ;


BEGIN
   outer
END nestedproc5.
