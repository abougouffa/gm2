(* Copyright (C) 2003 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE RealInOut ;

FROM DynamicStrings IMPORT String, InitString, KillString ;
FROM StringConvert IMPORT StringToLongreal, LongrealToString ;
FROM SYSTEM IMPORT ADR, BYTE ;
IMPORT InOut ;


PROCEDURE ReadReal (VAR x: REAL) ;
VAR
   s: String ;
BEGIN
   s := InOut.ReadS() ;
   IF InOut.Done
   THEN
      x := StringToLongreal(s, Done)
   ELSE
      Done := FALSE
   END ;
   s := KillString(s)
END ReadReal ;


(*
   WriteReal - 
*)

PROCEDURE WriteReal (x: REAL; n: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := KillString(InOut.WriteS(LongrealToString(x, n, n))) ;
   Done := TRUE
END WriteReal ;


(*
   WriteRealOct - writes the real to terminal in octal words.
*)

PROCEDURE WriteRealOct (x: REAL) ;
VAR
   p: POINTER TO BYTE ;
   i: CARDINAL ;
BEGIN
   p := ADR(x) ;
   i := 0 ;
   WHILE i<SIZE(x) DO
      InOut.WriteOct(VAL(CARDINAL, p^), 3) ;
      INC(p) ;
      INC(i)
   END
END WriteRealOct ;


END RealInOut.
