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
