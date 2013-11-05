(* Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
                 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE EtcGroup;

   FROM StdIO	IMPORT FILE, MODE, Fopen, Fclose;
   FROM Strings	IMPORT StrCmp;
   FROM ScanPwfile IMPORT fieldsep, linesep, GetText, GetNumber, ReRead;

   CONST
      grfilename  = "/etc/group";

   VAR
      grfile: FILE;
      opened: BOOLEAN;

   PROCEDURE GetGrent(VAR grent: Grent): BOOLEAN;		(* EXPORTED *)
      VAR
	 buf: ARRAY [0..1023] OF CHAR;
   BEGIN
      IF ~opened THEN RETURN FALSE END;
      WITH grent DO
	 members := NIL;		(* members not yet implemented *)
	 RETURN
	    GetText(grfile, grname,fieldsep) &
	    GetText(grfile, password,fieldsep) &
	    GetNumber(grfile, gid,fieldsep) &
	    GetText(grfile, buf,linesep)
      END;
   END GetGrent;

   PROCEDURE OpenGr(filename: ARRAY OF CHAR): BOOLEAN;		(* EXPORTED *)
   BEGIN
      IF opened THEN
	 IF ~Fclose(grfile) THEN END;
      END;
      opened := Fopen(grfile, filename, read, (*buff'd*) TRUE);
      RETURN opened
   END OpenGr;

   PROCEDURE CloseGr(): BOOLEAN;				(* EXPORTED *)
   BEGIN
      IF ~opened THEN
	 RETURN FALSE
      END;
      IF ~Fclose(grfile) THEN END;
      opened := FALSE;
      RETURN TRUE
   END CloseGr;

   PROCEDURE ReopenGr(): BOOLEAN;				(* EXPORTED *)
   BEGIN
      RETURN opened & ReRead(grfile)
   END ReopenGr;

   PROCEDURE GetGrgid(gid: CARDINAL; VAR grent: Grent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      WHILE GetGrent(grent) DO
	 IF grent.gid = gid THEN
	    RETURN TRUE
	 END;
      END;
      RETURN FALSE
   END GetGrgid;

   PROCEDURE GetGrnam(grn: ARRAY OF CHAR; VAR grent: Grent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      WHILE GetGrent(grent) DO
	 IF StrCmp(grent.grname,grn) = 0 THEN
	    RETURN TRUE
	 END;
      END;
      RETURN FALSE
   END GetGrnam;

   PROCEDURE FetchGrgid(gid: CARDINAL; VAR grent: Grent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      RETURN
	 OpenGr(grfilename) &
	 GetGrgid(gid, grent) &
	 CloseGr()
   END FetchGrgid;

   PROCEDURE FetchGrnam(grn: ARRAY OF CHAR; VAR grent: Grent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      RETURN
	 OpenGr(grfilename) &
	 GetGrnam(grn, grent) &
	 CloseGr()
   END FetchGrnam;

END EtcGroup.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I../sys:. EtcGroup.mod"
 * End:
 *)
