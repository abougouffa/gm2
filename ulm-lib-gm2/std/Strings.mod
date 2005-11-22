(* Ulm's Modula-2 Library
   Copyright (C) 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992,
   1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
   2004, 2005
   by University of Ulm, SAI, D-89069 Ulm, Germany
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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
   ----------------------------------------------------------------------------
   E-mail contact: gm2@glam.ac.uk
   ----------------------------------------------------------------------------
   $Id: Strings.mod,v 1.5 2005/11/22 15:13:21 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: Strings.mod,v $
   Revision 1.5  2005/11/22 15:13:21  gaius
   fixed Copyright dates

   Revision 1.4  2005/11/21 12:09:59  gaius
   updated Copyright notices and dates

   Revision 1.3  2004/06/29 08:51:42  gaius
   * made flex lexical analysers ignore carriage return
   * fixed bug in M2Quads.mod checking parameter of
     a const var before value was known.
   * fixed local MODULEs so that they can FROM mod IMPORT
   * tidied up some ulm implementation modules in ulm-lib-gm2/std

   Revision 1.2  2004/05/05 21:34:58  gaius
   * added SHIFT and ROTATE into ISO SYSTEM and
     made the compiler shift and rotate word and multi-word
     set types. Multi-word set rotate and shifts are implemented
     by calling ISO SYSTEM runtime procedures. Word sized sets or
     smaller are implemented inline using shift/rotate instructions.
     Currently not yet debugged, but mostly complete code.

   * fixed bug report by Paul Whittington <pwhittington@nitrodata.com>
     (see testsuite/gm2/link/pim/fail/import.mod).

   * updated gm2.texi to reflect new options and changes to the
     run-time system.

   * introduced -Wunbounded-by-reference option which will make a
     reference to non VAR unbounded data providing it is not written to
     within the callee procedure.
   * introduced -Wverbose-unbounded option which displays names of
     unbounded parameters which the compiler will implement as
     references even though they were specified as non VAR parameters.

   * introduced -Wcase, -Wnil runtime checks
   * introduced -Wcheck-all to enable all runtime flags
   * updated documentation to refect new options

   Revision 1.1  2003/12/27 00:16:06  gaius
   added ulm libraries into the gm2 tree. Currently these
   are only used when regression testing, but later they
   will be accessible by users of gm2.

   Revision 0.2  1997/02/28  15:50:42  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:38  borchert
   Initial revision

   ----------------------------------------------------------------------------
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
