(* LongIO.mod provides a Logitech-3.0 compatible library for GNU Modula-2.

Copyright (C) 2004-2019 Free Software Foundation, Inc.
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
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE LongIO ;

FROM DynamicStrings IMPORT String, InitString, KillString, RemoveWhitePrefix ;
FROM StringConvert IMPORT StringToLongInteger, LongIntegerToString ;

IMPORT InOut ;



PROCEDURE ReadLongInt (VAR i: LONGINT) ;
VAR
   s: String ;
BEGIN
   s := RemoveWhitePrefix(InOut.ReadS()) ;
   IF InOut.Done
   THEN
      i := StringToLongInteger(s, 10, Done)
   ELSE
      Done := FALSE
   END ;
   s := KillString(s)
END ReadLongInt ;


PROCEDURE WriteLongInt (i: LONGINT; n: CARDINAL) ;
VAR
   s: String ;
BEGIN
   IF i<0
   THEN
      s := KillString(InOut.WriteS(LongIntegerToString(i, n, ' ', TRUE, 10, FALSE)))
   ELSE
      s := KillString(InOut.WriteS(LongIntegerToString(i, n, ' ', FALSE, 10, FALSE)))
   END ;
   Done := TRUE
END WriteLongInt ;


END LongIO.
