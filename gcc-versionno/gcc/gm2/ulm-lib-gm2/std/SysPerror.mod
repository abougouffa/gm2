(* SysPerror.mod.

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

IMPLEMENTATION MODULE SysPerror; (* AFB 2/84 *)

   FROM Errno IMPORT errno, message, maxerror;
   FROM FtdIO IMPORT FwriteString, FwriteInt, FwriteLn;
   FROM StdIO IMPORT stderr;
   FROM Strings IMPORT StrCpy;

   CONST
      unknownError = "unknown error code";

   PROCEDURE Perror(str: ARRAY OF CHAR);
   BEGIN
      FwriteString(stderr, str);
      FwriteString(stderr, ": ");
      IF (errno <= maxerror) & (message[errno][0] # 0C) THEN
	 FwriteString(stderr, message[errno]);
      ELSE
	 FwriteString(stderr, unknownError);
	 FwriteString(stderr, " (");
	 FwriteInt(stderr, errno, 1);
	 FwriteString(stderr, ")");
      END;
      FwriteLn(stderr);
   END Perror;

   PROCEDURE GetErrorString(errno: CARDINAL; VAR str: ARRAY OF CHAR);
   BEGIN
      IF (errno <= maxerror) & (message[errno][0] # 0C) THEN
	 StrCpy(str, message[errno]);
      ELSE
	 StrCpy(str, unknownError);
      END;
   END GetErrorString;

END SysPerror.
