(* Copyright (C) 2004, 2005 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE Passwd;

   FROM StdIO	IMPORT FILE, MODE, Fopen, Fclose;
   FROM Strings	IMPORT StrCmp;
   FROM ScanPwfile IMPORT fieldsep, linesep, ReRead, GetText, GetNumber;

(* TYPE Pwent = RECORD ... END; *)

   CONST
      pwfilename  = "/etc/passwd";

   VAR
      pwfile: FILE;
      opened: BOOLEAN;

   PROCEDURE GetPwent(VAR pwent: Pwent): BOOLEAN;		(* EXPORTED *)
   BEGIN
      IF ~opened THEN RETURN FALSE END;
      WITH pwent DO
	 RETURN
	    GetText(pwfile, logname,fieldsep) &
	    GetText(pwfile, password,fieldsep) &
	    GetNumber(pwfile, uid,fieldsep) &
	    GetNumber(pwfile, gid,fieldsep) &
	    GetText(pwfile, fullname,fieldsep) &
	    GetText(pwfile, dir,fieldsep) &
	    GetText(pwfile, shell,linesep)
      END;
   END GetPwent;

   PROCEDURE OpenPw(filename: ARRAY OF CHAR): BOOLEAN;		(* EXPORTED *)
   BEGIN
      IF opened THEN
	 IF ~Fclose(pwfile) THEN END;
      END;
      opened := Fopen(pwfile, filename, read, (*buff'd*) TRUE);
      RETURN opened
   END OpenPw;

   PROCEDURE ClosePw(): BOOLEAN;				(* EXPORTED *)
   BEGIN
      IF ~opened THEN
	 RETURN FALSE
      END;
      IF ~Fclose(pwfile) THEN END;
      opened := FALSE;
      RETURN TRUE
   END ClosePw;

   PROCEDURE ReopenPw(): BOOLEAN;				(* EXPORTED *)
   BEGIN
      RETURN
	 opened & ReRead(pwfile)
   END ReopenPw;

   PROCEDURE GetPwuid(uid: CARDINAL; VAR pwent: Pwent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      WHILE GetPwent(pwent) DO
	 IF pwent.uid = uid THEN
	    RETURN TRUE
	 END;
      END;
      RETURN FALSE
   END GetPwuid;

   PROCEDURE GetPwnam(logn: ARRAY OF CHAR; VAR pwent: Pwent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      WHILE GetPwent(pwent) DO
	 IF StrCmp(pwent.logname,logn) = 0 THEN
	    RETURN TRUE
	 END;
      END;
      RETURN FALSE
   END GetPwnam;

   PROCEDURE FetchPwuid(uid: CARDINAL; VAR pwent: Pwent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      RETURN
	 OpenPw(pwfilename) &
	 GetPwuid(uid, pwent) &
	 ClosePw()
   END FetchPwuid;

   PROCEDURE FetchPwnam(logn: ARRAY OF CHAR; VAR pwent: Pwent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      RETURN
	 OpenPw(pwfilename) &
	 GetPwnam(logn, pwent) &
	 ClosePw()
   END FetchPwnam;

END Passwd.
