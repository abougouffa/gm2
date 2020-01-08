(* Conversions.mod provides a Logitech-3.0 compatible library.

Copyright (C) 2004-2020 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE Conversions ;


FROM DynamicStrings IMPORT String, InitString, KillString, CopyOut ;
FROM StringConvert IMPORT IntegerToString, StringToInteger,
                          StringToLongInteger, LongIntegerToString,
                          StringToCardinal, CardinalToString ;


(*
   ConvertOctal - converts a CARDINAL, num, into an octal string
                  and right justifies the string. It adds
                  spaces rather than '0' to pad out the string
                  to len characters.

                  If the length of str is < num then the number is
                  truncated on the right.
*)

PROCEDURE ConvertOctal (num, len: CARDINAL; VAR str: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   s := CardinalToString(num, len, ' ', 8, FALSE) ;
   CopyOut(str, s) ;
   s := KillString(s)
END ConvertOctal ;


PROCEDURE ConvertHex (num, len: CARDINAL; VAR str: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   s := CardinalToString(num, len, ' ', 16, TRUE) ;
   CopyOut(str, s) ;
   s := KillString(s)
END ConvertHex ;


PROCEDURE ConvertCardinal (num, len: CARDINAL; VAR str: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   s := CardinalToString(num, len, ' ', 10, FALSE) ;
   CopyOut(str, s) ;
   s := KillString(s)
END ConvertCardinal ;


(*
   The INTEGER counterparts will add a '-' if, num, is <0
*)

PROCEDURE ConvertInteger (num: INTEGER; len: CARDINAL;
                          VAR str: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   IF num<0
   THEN
      s := IntegerToString(num, len, ' ', TRUE, 10, FALSE)
   ELSE
      s := IntegerToString(num, len, ' ', FALSE, 10, FALSE)
   END ;
   CopyOut(str, s) ;
   s := KillString(s)
END ConvertInteger ;


PROCEDURE ConvertLongInt (num: LONGINT; len: CARDINAL; VAR str: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   IF num<0
   THEN
      s := LongIntegerToString(num, len, ' ', TRUE, 10, FALSE)
   ELSE
      s := LongIntegerToString(num, len, ' ', FALSE, 10, FALSE)
   END ;
   CopyOut(str, s) ;
   s := KillString(s)
END ConvertLongInt ;


PROCEDURE ConvertShortInt (num: SHORTINT; len: CARDINAL; VAR str: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   IF num<0
   THEN
      s := IntegerToString(VAL(INTEGER, num), len, ' ', TRUE, 10, FALSE)
   ELSE
      s := IntegerToString(VAL(INTEGER, num), len, ' ', FALSE, 10, FALSE)
   END ;
   CopyOut(str, s) ;
   s := KillString(s)
END ConvertShortInt ;


END Conversions.
