(* Ulm's Modula-2 Library
   Copyright (C) 1984-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either version
   2 of the License, or (at your option) any later version.

   Ulm's Modula-2 Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: gm2@glam.ac.uk
   ----------------------------------------------------------------------------
   $Id: RTErrors.mod,v 1.2 2004/06/29 08:51:42 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: RTErrors.mod,v $
   Revision 1.2  2004/06/29 08:51:42  gaius
   * made flex lexical analysers ignore carriage return
   * fixed bug in M2Quads.mod checking parameter of
     a const var before value was known.
   * fixed local MODULEs so that they can FROM mod IMPORT
   * tidied up some ulm implementation modules in ulm-lib-gm2/std

   Revision 1.1  2003/12/27 00:16:05  gaius
   added ulm libraries into the gm2 tree. Currently these
   are only used when regression testing, but later they
   will be accessible by users of gm2.

   Revision 0.2  1997/02/28  15:50:23  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:31  borchert
   Initial revision

   ----------------------------------------------------------------------------
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
	 IF (module # 0) & (line # 0) THEN
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
