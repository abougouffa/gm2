(* Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
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
