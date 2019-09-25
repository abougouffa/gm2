(* Plot.mod.

Copyright (C) 2004-2019 Free Software Foundation, Inc.
Contributed by University of Ulm.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)


(* Ulm's Modula-2 Library
   Copyright (C) 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991,
   1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003, 2004, 2005
   by University of Ulm, SAI, D-89069 Ulm, Germany
*)

IMPLEMENTATION MODULE Plot;

   FROM StdIO IMPORT FILE, stdout, stderr, Fputc, Fflush;
   FROM SysPerror IMPORT Perror;
   FROM FtdIO IMPORT FwriteString, FwriteLn;

   CONST
      nl = 12C;
   VAR
      fp: FILE;

   (* device independent plotter interface; see plot(3) and plot(5) *)

   (* local routines *)

   PROCEDURE Error;
   BEGIN
      Perror("Plot");
      HALT;
   END Error;

   PROCEDURE PutChar(ch: CHAR);
   BEGIN
      IF NOT Fputc(ch, fp) THEN Error END;
   END PutChar;

   PROCEDURE PutInt(value: INTEGER);
      CONST
         MaxShortInt = 77777B;
      VAR
         cvalue: CARDINAL;
         minus: BOOLEAN;
   BEGIN
      IF value < 0 THEN minus := TRUE; value := -value;
      ELSE minus := FALSE END;
      cvalue := CARDINAL(value);
      IF cvalue > MaxShortInt THEN
         FwriteString(stderr, "Plot: value out of bounds");
         FwriteLn(stderr);
         HALT;
      ELSE
         IF minus THEN
            cvalue := CARDINAL(BITSET(-value) - { 0..15 } + { 16 });
         END;
	 (* nice greetings from PDP11: bytes must be swapped *)
         PutChar(CHR(cvalue MOD 400B));
         PutChar(CHR(cvalue DIV 400B));
      END;
   END PutInt;

   (* visible routines *)

   PROCEDURE OpenPlot(f: FILE);
   BEGIN
      fp := f;
   END OpenPlot;

   PROCEDURE ClosePlot;
   BEGIN
      IF Fflush(fp) THEN (* ignore result *) END;
   END ClosePlot;

   PROCEDURE Move(xto, yto: INTEGER);
   BEGIN
      PutChar("m"); PutInt(xto); PutInt(yto);
   END Move;

   PROCEDURE Cont(xto, yto: INTEGER);
   BEGIN
      PutChar("n"); PutInt(xto); PutInt(yto);
   END Cont;

   PROCEDURE Point(xpoint, ypoint: INTEGER);
   BEGIN
      PutChar("p"); PutInt(xpoint); PutInt(ypoint);
   END Point;

   PROCEDURE Line(xfrom, yfrom, xto, yto: INTEGER);
   BEGIN
      PutChar("l"); PutInt(xfrom); PutInt(yfrom); PutInt(xto); PutInt(yto);
   END Line;

   PROCEDURE String(str: ARRAY OF CHAR);
      VAR ix: CARDINAL;
   BEGIN
      PutChar("t");
      ix := 0;
      WHILE (ix <= HIGH(str)) AND (str[ix] <> nl) AND (str[ix] <> 0C) DO
         PutChar(str[ix]);
         INC(ix);
      END;
      PutChar(nl);
   END String;

   PROCEDURE Arc(xcenter, ycenter, xstart, ystart, xend, yend: INTEGER);
   BEGIN
      PutChar("a"); PutInt(xcenter); PutInt(ycenter);
      PutInt(xstart); PutInt(ystart); PutInt(xend); PutInt(yend);
   END Arc;

   PROCEDURE Circle(xcenter, ycenter, radius: INTEGER);
   BEGIN
      PutChar("c"); PutInt(xcenter); PutInt(ycenter); PutInt(radius);
   END Circle;

   PROCEDURE Erase;
   BEGIN
      PutChar("e");
   END Erase;

   PROCEDURE LineMod(style: ARRAY OF CHAR);
      VAR ix: CARDINAL;
   BEGIN
      PutChar("f");
      ix := 0;
      WHILE (ix <= HIGH(style)) AND (style[ix] <> nl) AND (style[ix] <> 0C) DO
         PutChar(style[ix]);
         INC(ix);
      END;
      PutChar(nl); PutChar(0C);
   END LineMod;

   PROCEDURE Space(xupleft, yupleft, xlowright, ylowright: INTEGER);
   BEGIN
      PutChar("s"); PutInt(xupleft); PutInt(yupleft);
      PutInt(xlowright); PutInt(ylowright);
   END Space;

   PROCEDURE Reverse(xupleft, yupleft, xlowright, ylowright: INTEGER);
   BEGIN
      PutChar("R");
      PutInt(xupleft); PutInt(yupleft); PutInt(xlowright); PutInt(ylowright);
   END Reverse;

   PROCEDURE Polygon(xcenter, ycenter, xstart, ystart, edges: INTEGER);
   BEGIN
      PutChar("P");
      PutInt(xcenter); PutInt(ycenter); PutInt(xstart); PutInt(ystart);
      PutInt(edges);
   END Polygon;

   PROCEDURE CharMod(plotchar: CHAR);
   BEGIN
      PutChar("C"); PutChar(plotchar);
   END CharMod;

BEGIN
   fp := stdout;
END Plot.
