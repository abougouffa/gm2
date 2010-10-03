(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                 2010
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA *)

IMPLEMENTATION MODULE FpuIO ;

FROM StrIO IMPORT ReadString, WriteString, WriteLn ;
FROM StrLib IMPORT StrLen, StrRemoveWhitePrefix ;
FROM ASCII IMPORT nul ;
FROM DynamicStrings IMPORT String, InitString, KillString, CopyOut ;

FROM StringConvert IMPORT StringToLongreal, LongrealToString, 
                          LongIntegerToString, StringToLongInteger ;


CONST
   MaxLineLength = 100 ;


PROCEDURE ReadReal (VAR x: REAL) ;
VAR
   a: ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   ReadString(a) ;
   StrToReal(a, x)
END ReadReal ;


(*
   WriteReal - converts a REAL number, x, which has a, TotalWidth, and
               FractionWidth into, string, a.
*)

PROCEDURE WriteReal (x: REAL; TotalWidth, FractionWidth: CARDINAL) ;
VAR
   a: ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   RealToStr(x, TotalWidth, FractionWidth, a) ;
   WriteString(a)
END WriteReal ;


PROCEDURE StrToReal (a: ARRAY OF CHAR ; VAR x: REAL) ;
VAR
   lr: LONGREAL ;
BEGIN
   StrToLongReal(a, lr) ;  (* let StrToLongReal do the work and we convert the result back to REAL *)
   x := VAL(REAL, lr)
END StrToReal ;


PROCEDURE ReadLongReal (VAR x: LONGREAL) ;
VAR
   a: ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   ReadString(a) ;
   StrToLongReal(a, x)
END ReadLongReal ;


(*
   WriteLongReal - converts a LONGREAL number, x, which has a, TotalWidth, and
                   FractionWidth into a string.
*)

PROCEDURE WriteLongReal (x: LONGREAL; TotalWidth, FractionWidth: CARDINAL) ;
VAR
   a: ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   LongRealToStr(x, TotalWidth, FractionWidth, a) ;
   WriteString(a)
END WriteLongReal ;


PROCEDURE StrToLongReal (a: ARRAY OF CHAR ; VAR x: LONGREAL) ;
VAR
   found: BOOLEAN ;
   s    : String ;
BEGIN
   s := InitString(a) ;
   x := StringToLongreal(s, found) ;
   s := KillString(s)
END StrToLongReal ;


(*
   RealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
               FractionWidth into a string.
*)

PROCEDURE RealToStr (x: REAL; TotalWidth, FractionWidth: CARDINAL; VAR a: ARRAY OF CHAR) ;
VAR
   lr: LONGREAL ;
BEGIN
   lr := VAL(LONGREAL, x) ;
   LongRealToStr(lr, TotalWidth, FractionWidth, a)
END RealToStr ;


(*
   LongRealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
                   FractionWidth into a string.
*)

PROCEDURE LongRealToStr (x: LONGREAL; TotalWidth, FractionWidth: CARDINAL; VAR a: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   s := LongrealToString(x, TotalWidth, FractionWidth) ;
   CopyOut(a, s) ;
   s := KillString(s)
END LongRealToStr ;


PROCEDURE ReadLongInt (VAR x: LONGINT) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   ReadString( a ) ;
   StrToLongInt(a, x)
END ReadLongInt ;


PROCEDURE WriteLongInt (x: LONGINT; n: CARDINAL) ;
VAR
   a : ARRAY [0..MaxLineLength] OF CHAR ;
BEGIN
   LongIntToStr(x, n, a) ;
   WriteString(a)
END WriteLongInt ;


PROCEDURE LongIntToStr (x: LONGINT; n: CARDINAL ; VAR a: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   s := LongIntegerToString(x, n, ' ', FALSE, 10, TRUE) ;
   CopyOut(a, s) ;
   s := KillString(s)
END LongIntToStr ;


PROCEDURE StrToLongInt (a: ARRAY OF CHAR ; VAR x: LONGINT) ;
VAR
   s    : String ;
   found: BOOLEAN ;
BEGIN
   s := InitString(a) ;
   x := StringToLongInteger(s, 10, found) ;
   s := KillString(s)
END StrToLongInt ;


END FpuIO.
