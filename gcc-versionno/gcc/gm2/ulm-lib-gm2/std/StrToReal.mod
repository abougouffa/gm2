(* StrToReal.mod.

Copyright (C) 2004-2019 Free Software Foundation, Inc.
Contributed by University of Ulm.

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


(* Ulm's Modula-2 Library
   Copyright (C) 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991,
   1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003, 2004, 2005
   by University of Ulm, SAI, D-89069 Ulm, Germany
*)

IMPLEMENTATION MODULE StrToReal;

   FROM RealConv IMPORT Done, ReadReal;
   FROM Strings IMPORT StrCpy;

   CONST
      buflen = 512;
   VAR
      readbuf: ARRAY [0..buflen-1] OF CHAR;
      index: [0..buflen];

   PROCEDURE Read(VAR ch: CHAR);
      (* return characters from read buffer *)
   BEGIN
      IF index <= HIGH(readbuf) THEN
	 ch := readbuf[index]; INC(index);
      ELSE
	 ch := 0C;
      END;
   END Read;

   PROCEDURE StrToReal(str: ARRAY OF CHAR; VAR real: REAL) : BOOLEAN;
      (* converts str to the REAL real, leading white space is
	 ignored, returns FALSE if str does not conform to following
	 syntax:
	    ["+" | "-"] digit { digit } ["." digit { digit } ]
	    ["E" ["+" | "-"] digit [digit] ]
      *)
   BEGIN
      StrCpy(readbuf, str); index := 0; (* setup read buffer *)
      ReadReal(Read, real);
      RETURN Done
   END StrToReal;

END StrToReal.
