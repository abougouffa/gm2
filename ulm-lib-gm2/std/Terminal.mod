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

IMPLEMENTATION MODULE Terminal; (* AFB 8/84 *)

  (* version using StdIO *)

   FROM SYSTEM IMPORT ADR, SIZE;
   FROM StdIO IMPORT stdin, stdout, Fputc, Fgetc, Fungetc, Fwrite;

   VAR
      (* Done: BOOLEAN; *)
      oldch: CHAR;
      readAgain: BOOLEAN;

   PROCEDURE Read(VAR ch: CHAR);
   BEGIN
      IF NOT Fgetc(ch, stdin) THEN
         Done := FALSE;
         ch := 0C;
      ELSE
         Done := TRUE;
      END;
      readAgain := FALSE;
      oldch := ch;
   END Read;

   PROCEDURE ReadAgain;
   BEGIN
      IF readAgain THEN
         Done := FALSE;
      ELSE
         Done := Fungetc(oldch, stdin);
         readAgain := TRUE;
      END;
   END ReadAgain;

   PROCEDURE Write(ch: CHAR);
   BEGIN
      Done := Fputc(ch, stdout);
   END Write;

   PROCEDURE WriteLn;
      CONST nl = 12C;
   BEGIN
      Write(nl);
   END WriteLn;

   PROCEDURE WriteString(s: ARRAY OF CHAR);
      VAR i: CARDINAL;
   BEGIN
      i := 0;
      WHILE (i <= HIGH(s)) AND (s[i] <> 0C) DO
         INC(i);
      END;
      (* use Fwrite for efficiency *)
      Done := (i = 0) OR Fwrite(ADR(s), SIZE(CHAR), i, stdout);
   END WriteString;

BEGIN
   readAgain := FALSE;
   Done := TRUE;
END Terminal.
