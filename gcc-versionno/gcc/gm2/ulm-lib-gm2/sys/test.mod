(* test.mod.

Copyright (C) 2001-2019 Free Software Foundation, Inc.
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

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

MODULE test ;

FROM SysWrite IMPORT Write ;
FROM SYSTEM IMPORT ADR ;
FROM ASCII IMPORT nl ;

VAR
   a: ARRAY [0..20] OF CHAR ;
   c: CARDINAL ;
BEGIN
   a := "hello world" ;
   c := 11 ;
   IF Write(1, ADR(a), c)
   THEN
   END ;
   a[0] := nl ;
   c := 1 ;
   IF Write(1, ADR(a), c)
   THEN
   END
END test.
