(* Copyright (C) 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA 02110-1301, USA *)

IMPLEMENTATION MODULE RealInOut ;

FROM DynamicStrings IMPORT String, InitString, KillString, RemoveWhitePrefix ;
FROM StringConvert IMPORT StringToLongreal, LongrealToString ;
FROM SYSTEM IMPORT ADR, BYTE ;
IMPORT InOut ;

CONST
   NoMinfieldWidth = -1 ;

VAR
   DecimalPlacesLength: INTEGER ;


(*
   SetNoOfDecimalPlaces - number of decimal places WriteReal and WriteLongReal
                          should emit.  If this value is set to -1 then
                          the number of decimal places is the minimum field
                          width (the default).
*)

PROCEDURE SetNoOfDecimalPlaces (places: INTEGER) ;
BEGIN
   DecimalPlacesLength := places
END SetNoOfDecimalPlaces ;


(*
   ReadReal - reads a real number, legal syntaxes include:
              100, 100.0, 100e0, 100E0, 100E-1, E2, +1E+2, 1e+2
*)

PROCEDURE ReadReal (VAR x: REAL) ;
VAR
   s: String ;
BEGIN
   s := RemoveWhitePrefix(InOut.ReadS()) ;
   IF InOut.Done
   THEN
      x := VAL(REAL, StringToLongreal(s, Done))
   ELSE
      Done := FALSE
   END ;
   s := KillString(s)
END ReadReal ;


(*
   WriteReal - writes a real to the terminal. The real number
               is right justified and, n, is the minimum field
               width.
*)

PROCEDURE WriteReal (x: REAL; n: CARDINAL) ;
VAR
   s: String ;
BEGIN
   IF DecimalPlacesLength=NoMinfieldWidth
   THEN
      s := KillString(InOut.WriteS(LongrealToString(VAL(LONGREAL, x), n, n)))
   ELSE
      s := KillString(InOut.WriteS(LongrealToString(VAL(LONGREAL, x), n, DecimalPlacesLength)))
   END ;
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


(*
   ReadLongReal - reads a LONGLONGREAL number, legal syntaxes include:
                  100, 100.0, 100e0, 100E0, 100E-1, E2, +1E+2, 1e+2
*)

PROCEDURE ReadLongReal (VAR x: LONGREAL) ;
VAR
   s: String ;
BEGIN
   s := RemoveWhitePrefix(InOut.ReadS()) ;
   IF InOut.Done
   THEN
      x := StringToLongreal(s, Done)
   ELSE
      Done := FALSE
   END ;
   s := KillString(s)
END ReadLongReal ;


(*
   WriteLongReal - writes a real to the terminal. The real number
                   is right justified and, n, is the minimum field
                   width.
*)

PROCEDURE WriteLongReal (x: LONGREAL; n: CARDINAL) ;
VAR
   s: String ;
BEGIN
   IF DecimalPlacesLength=NoMinfieldWidth
   THEN
      s := KillString(InOut.WriteS(LongrealToString(x, n, n)))
   ELSE
      s := KillString(InOut.WriteS(LongrealToString(x, n, DecimalPlacesLength)))
   END ;
   Done := TRUE
END WriteLongReal ;


(*
   WriteLongRealOct - writes the real to terminal in octal words.
*)

PROCEDURE WriteLongRealOct (x: LONGREAL) ;
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
END WriteLongRealOct ;


(*
   ReadShortReal - reads a SHORTREAL number, legal syntaxes include:
                   100, 100.0, 100e0, 100E0, 100E-1, E2, +1E+2, 1e+2
*)

PROCEDURE ReadShortReal (VAR x: SHORTREAL) ;
VAR
   s: String ;
BEGIN
   s := RemoveWhitePrefix(InOut.ReadS()) ;
   IF InOut.Done
   THEN
      x := VAL(SHORTREAL, StringToLongreal(s, Done))
   ELSE
      Done := FALSE
   END ;
   s := KillString(s)
END ReadShortReal ;


(*
   WriteShortReal - writes a real to the terminal. The real number
                    is right justified and, n, is the minimum field
                    width.
*)

PROCEDURE WriteShortReal (x: SHORTREAL; n: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := KillString(InOut.WriteS(LongrealToString(VAL(LONGREAL, x), n, n))) ;
   Done := TRUE
END WriteShortReal ;


(*
   WriteShortRealOct - writes the real to terminal in octal words.
*)

PROCEDURE WriteShortRealOct (x: SHORTREAL) ;
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
END WriteShortRealOct ;


BEGIN
   DecimalPlacesLength := NoMinfieldWidth
END RealInOut.
