(* Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE InOut; (* stripped version: AFB 4/84 *)

   FROM Conversions IMPORT ConvertInteger, ConvertCardinal,
      ConvertOctal, ConvertHex;
   FROM ReadIntCard IMPORT int, card;
   IMPORT ReadIntCard;
   IMPORT Terminal;

   (* (* from definition module *)
   CONST
      EOL = 12C;
   VAR
      Done: BOOLEAN; (* on eof true *)
      termCH: CHAR; (* set in ReadString and numeric input procs *)

   *)

   CONST
      tab = 11C;

   PROCEDURE Read(VAR ch: CHAR);
   BEGIN
      Terminal.Read(ch);
      Done := Terminal.Done;
   END Read;

   PROCEDURE ReadString(VAR str: ARRAY OF CHAR);
      VAR ch: CHAR;
	  index: CARDINAL;
   BEGIN
      REPEAT
         Terminal.Read(ch);
         IF NOT Terminal.Done THEN str[0] := 0C; Done := FALSE; RETURN; END;
      UNTIL (ch <> ' ') AND (ch <> tab);
      index := 0;
      LOOP
         IF ch <= ' ' THEN
	    str[index] := 0C;
	    termCH := ch;
            Done := TRUE;
	    EXIT
	 END;
	 str[index] := ch;
	 INC(index);
	 IF index > HIGH(str) THEN
	    Terminal.Read(termCH);
            Done := Terminal.Done;
	    EXIT
	 END;
	 Terminal.Read(ch);
         IF NOT Terminal.Done THEN str[index] := 0C; Done := FALSE; RETURN; END;
      END; (* LOOP *)
   END ReadString;

   PROCEDURE ReadChar(VAR ch: CHAR);
   BEGIN
      Terminal.Read(ch);
      IF NOT Terminal.Done THEN ch := 0C END;
      termCH := ch;
   END ReadChar;

   PROCEDURE ReadCard(VAR arg: CARDINAL);
   BEGIN
      ReadIntCard.Read(arg, card, ReadChar);
      Done := ReadIntCard.Done;
   END ReadCard;

   PROCEDURE ReadInt(VAR arg: INTEGER);
   BEGIN
      ReadIntCard.Read(arg, int, ReadChar);
      Done := ReadIntCard.Done;
   END ReadInt;

   PROCEDURE Write(ch: CHAR);
   BEGIN
      Terminal.Write(ch);
      Done := Terminal.Done;
   END Write;

   PROCEDURE WriteLn;
   BEGIN
      Terminal.WriteLn;
      Done := Terminal.Done;
   END WriteLn;

   PROCEDURE WriteString(s: ARRAY OF CHAR);
   BEGIN
      Terminal.WriteString(s);
      Done := Terminal.Done;
   END WriteString;

   (* n: minimum field width *)

   PROCEDURE Blanks(VAR n: CARDINAL; min: CARDINAL; VAR ok: BOOLEAN);
      (* if n > min then write n-min blanks *)
   BEGIN
      ok := TRUE;
      WHILE (n > min) AND ok DO
         Terminal.Write(" ");
         ok := Terminal.Done;
         DEC(n);
      END;
   END Blanks;

   PROCEDURE WriteInt(x: INTEGER; n: CARDINAL);
      VAR
         buf: ARRAY[0..10] OF CHAR;
         ok: BOOLEAN;
   BEGIN
      Blanks(n, HIGH(buf)+1, ok);
      IF NOT ok THEN Done := FALSE; RETURN END;
      ConvertInteger(x, n, buf);
      Terminal.WriteString(buf);
      Done := Terminal.Done;
   END WriteInt;

   PROCEDURE WriteCard(x: CARDINAL; n: CARDINAL);
      VAR
         buf: ARRAY[0..10] OF CHAR;
         ok: BOOLEAN;
   BEGIN
      Blanks(n, HIGH(buf)+1, ok);
      IF NOT ok THEN Done := FALSE; RETURN END;
      ConvertCardinal(x, n, buf);
      Terminal.WriteString(buf);
      Done := Terminal.Done;
   END WriteCard;

   PROCEDURE WriteOct(x: CARDINAL; n: CARDINAL);
      VAR
         buf: ARRAY[0..10] OF CHAR;
         ok: BOOLEAN;
   BEGIN
      Blanks(n, HIGH(buf)+1, ok);
      IF NOT ok THEN Done := FALSE; RETURN END;
      ConvertOctal(x, n, buf);
      Terminal.WriteString(buf);
      Done := Terminal.Done;
   END WriteOct;

   PROCEDURE WriteHex(x: CARDINAL; n: CARDINAL);
      VAR
         buf: ARRAY[0..8] OF CHAR;
         ok: BOOLEAN;
   BEGIN
      Blanks(n, HIGH(buf)+1, ok);
      IF NOT ok THEN Done := FALSE; RETURN END;
      ConvertHex(x, n, buf);
      Terminal.WriteString(buf);
      Done := Terminal.Done;
   END WriteHex;

END InOut.
