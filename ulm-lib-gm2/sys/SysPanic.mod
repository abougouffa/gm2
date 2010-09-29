(* Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

This file was originally part of the University of Ulm library
*)


(* Ulm's Modula-2 Library
   Copyright (C) 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991,
   1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003, 2004, 2005
   by University of Ulm, SAI, D-89069 Ulm, Germany
*)

IMPLEMENTATION MODULE SysPanic;

   (* don't use the StdIO or FtdIO (else reference cycle problems) *)

   FROM SysWrite IMPORT Write;
   FROM SysExit IMPORT Exit;
   FROM SYSTEM IMPORT ADR;

   PROCEDURE Panic(text: ARRAY OF CHAR);
      CONST
         stderr = 2;
      VAR
         index: CARDINAL;
         count: CARDINAL;
   BEGIN
      count := 1;
      index := 0;
      WHILE (count = 1) AND (index <= HIGH(text)) AND (text[index] <> 0C)
            AND Write(stderr, ADR(text[index]), count) DO
         INC(index);
      END;
      IF count = 1 THEN
         text[0] := 12C; (* nl *)
         IF Write(stderr, ADR(text), count) THEN END;
      END;
      Exit(255);
   END Panic;

END SysPanic.
