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

IMPLEMENTATION MODULE ScanPwfile;

   FROM StdIO	IMPORT FILE, Fseek, Fgetc;

   PROCEDURE GetText(pwfile: FILE;
		     VAR text: ARRAY OF CHAR; sepchar: CHAR): BOOLEAN;
   (* EXPORTED *)
      VAR
	 ch: CHAR;
	 pos: CARDINAL;
   BEGIN
      pos := 0;
      LOOP
	 IF ~Fgetc(ch,pwfile) THEN
	    RETURN FALSE
	 END;
	 IF ch = sepchar THEN EXIT END;
	 IF pos <= HIGH(text) THEN
	    text[pos] := ch;
	    INC(pos);
	 END;
      END; (*LOOP*)
      IF pos <= HIGH(text) THEN
	 text[pos] := 0C;
      END;
      RETURN TRUE;
   END GetText;

   PROCEDURE GetNumber(pwfile: FILE;
			VAR number: CARDINAL; sepchar: CHAR): BOOLEAN;
   (* EXPORTED *)

      VAR
	 ch: CHAR;
	 numerical: BOOLEAN;
   BEGIN
      number := 0;
      numerical := TRUE;
      LOOP
	 IF ~Fgetc(ch,pwfile) THEN
	    RETURN FALSE
	 END;
	 IF ch = sepchar THEN
	    RETURN TRUE
	 END;
	 IF numerical THEN
	    IF (ch >= "0") & (ch <= "9") THEN
	       number := number * 10 + (ORD(ch) - ORD("0"));
	    ELSE
	       numerical := FALSE;
	    END;
	 END;
      END; (*LOOP*)
   END GetNumber;

   PROCEDURE ReRead(pwfile: FILE): BOOLEAN;		(* EXPORTED *)
   BEGIN
      RETURN Fseek(pwfile, (*offset*)0, (*whence=beginning*)0 )
   END ReRead;

END ScanPwfile.
