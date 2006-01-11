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

IMPLEMENTATION MODULE SysTermIO;

   FROM SYSTEM IMPORT UNIXCALL, ADR, BYTE;
   FROM Sys IMPORT ioctl;
   FROM Errno IMPORT errno;

   (* (* exported from definition module *)
   TYPE
      ControlChars = (vintr, vquit, verase, vkill, veof, veol, veol2, vswtch);
      ControlCharsRange = [MIN(ControlChars)..MAX(ControlChars)];
      InputModes = BITSET;
      OutputModes = BITSET;
      ControlModes = BITSET;
      LineModes = BITSET;
      TermIO =
	 RECORD
	    inputmodes: InputModes;
	    outputmodes: OutputModes;
	    controlmodes: ControlModes;
	    linemodes: LineModes;
	    linedisc: CHAR;
	    cc: ARRAY ControlCharsRange OF CHAR;
	 END;
      Winsize =
	 RECORD
	    rows, cols: CARDINAL;
	    xpixels, ypixels: CARDINAL;
	 END;
   *)

   TYPE
      CTermIO =
	 RECORD
	    iflag1, iflag2: CHAR;
	    oflag1, oflag2: CHAR;
	    cflag1, cflag2: CHAR;
	    lflag1, lflag2: CHAR;
	    line: CHAR;
	    c1, c2, c3, c4, c5, c6, c7, c8: CHAR;
	 END;
      CWinSize =
	 RECORD
	    row1, row2: CHAR;
	    col1, col2: CHAR;
	    xpixel1, xpixel2: CHAR;
	    ypixel1, ypixel2: CHAR;
	 END;

   TYPE
      RequestType = CARDINAL;
   CONST
      tcgeta  = 00005401H;
      tcseta  = 00005402H;
      tcsetaw = 00005403H;
      tcsbrk  = 00005405H;
      tcxonc  = 00005406H;
      tcflsh  = 00005407H;
      tiocgwinsz = 00005468H;
      tiocswinsz = 00005467H;

   PROCEDURE Ioctl(fd: CARDINAL; request: RequestType;
                   VAR argp: ARRAY OF BYTE) : BOOLEAN;
      VAR
	 r0, r1: CARDINAL;
   BEGIN
      IF NOT UNIXCALL(ioctl, r0, r1, fd, request, ADR(argp)) THEN
         errno := r0;
         RETURN FALSE;
      ELSE
         RETURN TRUE;
      END;
   END Ioctl;

   PROCEDURE SetTermIO(fd: CARDINAL; termio: TermIO) : BOOLEAN;
      VAR
	 ctermio: CTermIO;

      PROCEDURE Convert(VAR flag1, flag2: CHAR; bs: BITSET);
      BEGIN
	 flag1 := CHR(CARDINAL(bs) DIV 100H);
	 flag2 := CHR(CARDINAL(bs) MOD 100H);
      END Convert;

   BEGIN
      WITH termio DO
	 WITH ctermio DO
	    Convert(iflag1, iflag2, inputmodes);
	    Convert(oflag1, oflag2, outputmodes);
	    Convert(cflag1, cflag2, controlmodes);
	    Convert(lflag1, lflag2, linemodes);
	    line := linedisc;
	    c1 := cc[vintr];
	    c2 := cc[vquit];
	    c3 := cc[verase];
	    c4 := cc[vkill];
	    c5 := cc[veof];
	    c6 := cc[veol];
	    c7 := cc[veol2];
	    c8 := cc[vswtch];
	 END;
      END;
      RETURN Ioctl(fd, tcseta, ctermio);
   END SetTermIO;

   PROCEDURE GetTermIO(fd: CARDINAL; VAR termio: TermIO) : BOOLEAN;
      VAR
	 ctermio: CTermIO;

      PROCEDURE Convert(flag1, flag2: CHAR; VAR bs: BITSET);
      BEGIN
	 bs := BITSET( ORD(flag1)*100H + ORD(flag2) );
      END Convert;

   BEGIN
      IF NOT Ioctl(fd, tcgeta, ctermio) THEN RETURN FALSE END;
      WITH termio DO 
         WITH ctermio DO
	    Convert(iflag1, iflag2, inputmodes);
	    Convert(oflag1, oflag2, outputmodes);
	    Convert(cflag1, cflag2, controlmodes);
	    Convert(lflag1, lflag2, linemodes);
	    linedisc := line;
	    cc[vintr] := c1;
	    cc[vquit] := c2;
	    cc[verase] := c3;
	    cc[vkill] := c4;
	    cc[veof] := c5;
	    cc[veol] := c6;
	    cc[veol2] := c7;
	    cc[vswtch] := c8;
	 END;
      END;
      RETURN TRUE
   END GetTermIO;

   PROCEDURE GetWinsize(fd: CARDINAL; VAR winbuf: Winsize) : BOOLEAN;
      VAR
	 crec: CWinSize;

      PROCEDURE Convert(byte1, byte2: CHAR; VAR cardinal: CARDINAL);
      BEGIN
	 cardinal := ORD(byte1) * 100H + ORD(byte2);
      END Convert;

   BEGIN
      IF NOT Ioctl(fd, tiocgwinsz, crec) THEN RETURN FALSE END;
      WITH crec DO
	 WITH winbuf DO
	    Convert(row1, row2, rows);
	    Convert(col1, col2, cols);
	    Convert(xpixel1, xpixel2, xpixels);
	    Convert(ypixel1, ypixel2, ypixels);
	 END;
      END;
      RETURN TRUE
   END GetWinsize;

   PROCEDURE Baudrate(termio: TermIO) : CARDINAL;
      VAR
	 baudrate: ControlModes;
   BEGIN
      WITH termio DO
	 baudrate := controlmodes * cbaud;
	 IF    baudrate = b0     THEN RETURN     0
	 ELSIF baudrate = b50    THEN RETURN    50
	 ELSIF baudrate = b75    THEN RETURN    75
	 ELSIF baudrate = b110   THEN RETURN   110
	 ELSIF baudrate = b134   THEN RETURN   134
	 ELSIF baudrate = b150   THEN RETURN   150
	 ELSIF baudrate = b200   THEN RETURN   200
	 ELSIF baudrate = b300   THEN RETURN   300
	 ELSIF baudrate = b600   THEN RETURN   600
	 ELSIF baudrate = b1200  THEN RETURN  1200
	 ELSIF baudrate = b1800  THEN RETURN  1800
	 ELSIF baudrate = b2400  THEN RETURN  2400
	 ELSIF baudrate = b4800  THEN RETURN  4800
	 ELSIF baudrate = b9600  THEN RETURN  9600
	 ELSIF baudrate = b19200 THEN RETURN 19200
	 ELSIF baudrate = b38400 THEN RETURN 38400
	 ELSE
	    RETURN 0
	 END;
      END;
   END Baudrate;

   PROCEDURE Isatty(fd: CARDINAL) : BOOLEAN;
      VAR
	 termio: TermIO;
   BEGIN
      RETURN GetTermIO(fd, termio)
   END Isatty;

END SysTermIO.
