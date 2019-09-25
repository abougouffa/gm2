(* SysExit.mod.

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

IMPLEMENTATION MODULE SysExit;

   FROM SYSTEM IMPORT UNIXCALL;
   FROM Sys IMPORT exit;

   CONST
      MaxCleanup = 32;
   VAR
      CleanupProcs: ARRAY[0..MaxCleanup-1] OF PROC;
      index: CARDINAL; (* of CleanupProcs *)
      cleanup: BOOLEAN;

   PROCEDURE Exit(exitCode: CARDINAL);
      VAR r0, r1: INTEGER; i: CARDINAL;
   BEGIN
      IF NOT cleanup AND (index > 0) THEN
         cleanup := TRUE;
         FOR i := index-1 TO 0 BY -1 DO
            CleanupProcs[i];
         END;
      END;
      IF UNIXCALL(exit, r0, r1, exitCode) THEN
	 (* can this ever happen ??? *)
      END;
      (* NOTREACHED *)
   END Exit;

   PROCEDURE EnterCleanup(p: PROC);
   BEGIN
      IF index < MaxCleanup THEN
         CleanupProcs[index] := p;
         INC(index);
      END;
   END EnterCleanup;

BEGIN
   index := 0;
   cleanup := FALSE;
END SysExit.
