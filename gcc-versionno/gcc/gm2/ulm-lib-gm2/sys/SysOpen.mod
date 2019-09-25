(* SysOpen.mod.

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

IMPLEMENTATION MODULE SysOpen;

   FROM SYSTEM IMPORT UNIXCALL, ADR, WORD;
   FROM Sys IMPORT open;
   FROM Errno IMPORT errno;
   FROM UnixString IMPORT Copy, Buffer;

   (* mode = 0 : read, 1 : write, 2 : both *)

   PROCEDURE Open(VAR fd: CARDINAL; filename: ARRAY OF CHAR;
                  oflag: WORD) : BOOLEAN;
   BEGIN
      RETURN OpenCreat(fd, filename, oflag, 0);
   END Open;

   PROCEDURE OpenCreat(VAR fd: CARDINAL; filename: ARRAY OF CHAR;
		       oflag: WORD; mode: CARDINAL) : BOOLEAN;
      VAR r0, r1: INTEGER;
          Buf: Buffer;
   BEGIN
      Copy(Buf, filename);
      IF UNIXCALL(open, r0, r1, ADR(Buf), oflag, mode) THEN
         fd := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END OpenCreat;

END SysOpen.
