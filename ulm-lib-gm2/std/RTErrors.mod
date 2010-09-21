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

IMPLEMENTATION MODULE RTErrors;

   FROM FtdIO IMPORT FwriteString, FwriteLn, FwriteInt, FwriteCard;
   FROM StdIO IMPORT stderr;

   VAR
      handler: Handler;

   PROCEDURE StandardHandler(error: Error);
      TYPE
	 String = POINTER TO ARRAY [0..31] OF CHAR;
      VAR
	 modname: String;
	 msg: ARRAY [0..79] OF CHAR;
   BEGIN
      WITH error^ DO
	 FwriteLn(stderr); (* don't start in the middle of a line *)
	 IF (module # NIL) & (line # 0) THEN
	    FwriteString(stderr, "runtime error in module ");
	    modname := module; FwriteString(stderr, modname^);
	    FwriteString(stderr, " at line "); FwriteCard(stderr, line, 1);
	    FwriteString(stderr, ":"); FwriteLn(stderr);
	 END;
	 IF kind = range THEN
	    CASE rtype OF
	    | unsigned: FwriteString(stderr, "CARDINAL value ");
	                FwriteCard(stderr, value, 1);
			FwriteString(stderr, " out of [");
			FwriteCard(stderr, min, 1);
			FwriteString(stderr, "..");
			FwriteCard(stderr, max, 1);
			FwriteString(stderr, "]");
	    | signed:   FwriteString(stderr, "INTEGER value ");
	                FwriteInt(stderr, ivalue, 1);
			FwriteString(stderr, " out of [");
			FwriteInt(stderr, imin, 1);
			FwriteString(stderr, "..");
			FwriteInt(stderr, imax, 1);
			FwriteString(stderr, "]");
	    | sign:     FwriteString(stderr,
			   "CARDINAL/INTEGER conversion error");
	    | dyn:      FwriteString(stderr, "index ");
			FwriteCard(stderr, value, 1);
	                FwriteString(stderr, " out of bounds of ");
	                FwriteString(stderr, "dynamic array [0..");
			FwriteCard(stderr, max, 1);
			FwriteString(stderr, "]");
	    END;
	 ELSE
	    CASE kind OF
	    | halt:  msg := "call of procedure HALT";
	    | case:  msg := "no case label";
	    | stack: msg := "stack overflow";
	    | crend: msg := "RETURN of coroutine body";
	    | prio:  msg := "priority of module is lower than current priority";
	    | fret:  msg := "function does not return any value";
	    END;
	    FwriteString(stderr, msg);
	 END;
	 FwriteLn(stderr);
      END;
   END StandardHandler;

   PROCEDURE Notify(error: Error);
      (* called by runtime system *)
   BEGIN
      handler(error);
   END Notify;

   PROCEDURE SetHandler(newHandler: Handler);
      (* define alternative handler of runtime errors *)
   BEGIN
      handler := newHandler;
   END SetHandler;

BEGIN
   handler := StandardHandler;
END RTErrors.
