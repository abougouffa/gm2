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

IMPLEMENTATION MODULE Strings; (* AFB 7/84 *)

   PROCEDURE StrLen(s: ARRAY OF CHAR) : CARDINAL;
      VAR len: CARDINAL;
   BEGIN
      len := 0;
      WHILE (len <= HIGH(s)) AND (s[len] <> 0C) DO
         INC(len);
      END;
      RETURN len;
   END StrLen;

   PROCEDURE StrCat(VAR s: ARRAY OF CHAR; s1: ARRAY OF CHAR);
      VAR index1, index2: CARDINAL;
   BEGIN
      index1 := StrLen(s);
      index2 := 0;
      WHILE (index1 <= HIGH(s)) AND (index2 <= HIGH(s1)) AND
            (s1[index2] <> 0C) DO
         s[index1] := s1[index2];
         INC(index1);
         INC(index2);
      END;
      IF index1 <= HIGH(s) THEN
         s[index1] := 0C;
      END;
   END StrCat;

   PROCEDURE StrCmp(s1, s2: ARRAY OF CHAR) : INTEGER;
      VAR index: CARDINAL;
          min: CARDINAL;
   BEGIN
      IF HIGH(s1) < HIGH(s2) THEN min := HIGH(s1) ELSE min := HIGH(s2) END;
      FOR index := 0 TO min DO
         IF s1[index] <> s2[index] THEN
            RETURN ORD(s1[index]) - ORD(s2[index]);
         ELSIF s1[index] = 0C THEN
            RETURN 0;
         END;
      END;
      IF HIGH(s1) = HIGH(s2) THEN
         RETURN 0
      ELSIF (HIGH(s1) > min) AND (s1[min+1] <> 0C) THEN
         RETURN 1;
      ELSIF (HIGH(s2) > min) AND (s2[min+1] <> 0C) THEN
         RETURN -1;
      ELSE
         RETURN 0;
      END;
   END StrCmp;

   PROCEDURE StrCpy(VAR s: ARRAY OF CHAR; s1: ARRAY OF CHAR);
      VAR index: CARDINAL;
          min: CARDINAL;
   BEGIN
      index := 0;
      IF HIGH(s) < HIGH(s1) THEN min := HIGH(s) ELSE min := HIGH(s1) END;
      FOR index := 0 TO min DO
         s[index] := s1[index];
         IF s[index] = 0C THEN
            RETURN;
         END;
      END;
      index := min+1;
      IF index <= HIGH(s) THEN
         s[index] := 0C;
      END;
   END StrCpy;

END Strings.
