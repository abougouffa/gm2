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
   2002, 2003, 2004, 2005, 2006
   by University of Ulm, SAI, D-89069 Ulm, Germany
*)

IMPLEMENTATION MODULE SysTermIO;

IMPORT termios ;


PROCEDURE SetTermIO (fd: CARDINAL; termio: TermIO) : BOOLEAN ;
VAR
   t: termios.TERMIOS ;
   b: BOOLEAN ;
   r: INTEGER ;
   c: ControlChar ;
   f: termios.Flag ;
BEGIN
   t := termios.InitTermios() ;
   WITH termio DO
      FOR f := MIN(termios.Flag) TO MAX(termios.Flag) DO
         b := termios.SetFlag(t, f, f IN modes)
      END ;

      (* clear all baud bits *)
      b := termios.SetFlag(t, termios.b0, FALSE) ;
      b := termios.SetFlag(t, termios.b50, FALSE) ;
      b := termios.SetFlag(t, termios.b75, FALSE) ;
      b := termios.SetFlag(t, termios.b110, FALSE) ;
      b := termios.SetFlag(t, termios.b135, FALSE) ;
      b := termios.SetFlag(t, termios.b150, FALSE) ;
      b := termios.SetFlag(t, termios.b200, FALSE) ;
      b := termios.SetFlag(t, termios.b300, FALSE) ;
      b := termios.SetFlag(t, termios.b600, FALSE) ;
      b := termios.SetFlag(t, termios.b1200, FALSE) ;
      b := termios.SetFlag(t, termios.b1800, FALSE) ;
      b := termios.SetFlag(t, termios.b2400, FALSE) ;
      b := termios.SetFlag(t, termios.b4800, FALSE) ;
      b := termios.SetFlag(t, termios.b9600, FALSE) ;
      b := termios.SetFlag(t, termios.b19200, FALSE) ;
      b := termios.SetFlag(t, termios.b38400, FALSE) ;
      b := termios.SetFlag(t, termios.b57600, FALSE) ;
      b := termios.SetFlag(t, termios.b115200, FALSE) ;
      b := termios.SetFlag(t, termios.b240400, FALSE) ;
      b := termios.SetFlag(t, termios.b460800, FALSE) ;
      b := termios.SetFlag(t, termios.b500000, FALSE) ;
      b := termios.SetFlag(t, termios.b576000, FALSE) ;
      b := termios.SetFlag(t, termios.b921600, FALSE) ;
      b := termios.SetFlag(t, termios.b1000000, FALSE) ;
      b := termios.SetFlag(t, termios.b1152000, FALSE) ;
      b := termios.SetFlag(t, termios.b1500000, FALSE) ;
      b := termios.SetFlag(t, termios.b2000000, FALSE) ;
      b := termios.SetFlag(t, termios.b2500000, FALSE) ;
      b := termios.SetFlag(t, termios.b3000000, FALSE) ;
      b := termios.SetFlag(t, termios.b3500000, FALSE) ;
      b := termios.SetFlag(t, termios.b4000000, FALSE) ;
      b := termios.SetFlag(t, termios.maxbaud, FALSE) ;

      (* now set the single bit *)
     
      CASE baud OF

      0:  b := termios.SetFlag(t, termios.b0, TRUE) |
      50: b := termios.SetFlag(t, termios.b50, TRUE) |
      75: b := termios.SetFlag(t, termios.b75, TRUE) |
      110: b := termios.SetFlag(t, termios.b110, TRUE) |
      135: b := termios.SetFlag(t, termios.b135, TRUE) |
      150: b := termios.SetFlag(t, termios.b150, TRUE) |
      200: b := termios.SetFlag(t, termios.b200, TRUE) |
      300: b := termios.SetFlag(t, termios.b300, TRUE) |
      600: b := termios.SetFlag(t, termios.b600, TRUE) |
      1200: b := termios.SetFlag(t, termios.b1200, TRUE) |
      1800: b := termios.SetFlag(t, termios.b1800, TRUE) |
      2400: b := termios.SetFlag(t, termios.b2400, TRUE) |
      4800: b := termios.SetFlag(t, termios.b4800, TRUE) |
      9600: b := termios.SetFlag(t, termios.b9600, TRUE) |
      19200: b := termios.SetFlag(t, termios.b19200, TRUE) |
      38400: b := termios.SetFlag(t, termios.b38400, TRUE) |
      57600: b := termios.SetFlag(t, termios.b57600, TRUE) |
      115200: b := termios.SetFlag(t, termios.b115200, TRUE) |
      240400: b := termios.SetFlag(t, termios.b240400, TRUE) |
      460800: b := termios.SetFlag(t, termios.b460800, TRUE) |
      500000: b := termios.SetFlag(t, termios.b500000, TRUE) |
      576000: b := termios.SetFlag(t, termios.b576000, TRUE) |
      921600: b := termios.SetFlag(t, termios.b921600, TRUE) |
      1000000: b := termios.SetFlag(t, termios.b1000000, TRUE) |
      1152000: b := termios.SetFlag(t, termios.b1152000, TRUE) |
      1500000: b := termios.SetFlag(t, termios.b1500000, TRUE) |
      2000000: b := termios.SetFlag(t, termios.b2000000, TRUE) |
      2500000: b := termios.SetFlag(t, termios.b2500000, TRUE) |
      3000000: b := termios.SetFlag(t, termios.b3000000, TRUE) |
      3500000: b := termios.SetFlag(t, termios.b3500000, TRUE) |
      4000000: b := termios.SetFlag(t, termios.b4000000, TRUE) |
      maxbaud: b := termios.SetFlag(t, termios.maxbaud, TRUE)

      ELSE
         t := termios.KillTermios(t) ;
         RETURN FALSE
      END ;

      FOR c := MIN(ControlChar) TO MAX(ControlChar) DO
         b := termios.SetChar(t, c, cc[c])
      END
   END ;

   r := termios.tcsetattr(fd, termios.tcsnow(), t) ;
   t := termios.KillTermios(t) ;
   RETURN r=0
END SetTermIO;


PROCEDURE doFlag (VAR m: Modes; f: Flag; v: BOOLEAN) ;
BEGIN
   IF v
   THEN
      INCL(m, f)
   ELSE
      EXCL(m, f)
   END
END doFlag ;


PROCEDURE GetTermIO (fd: CARDINAL; VAR termio: TermIO) : BOOLEAN ;
VAR
   t: termios.TERMIOS ;
   b, v: BOOLEAN ;
   r: INTEGER ;
   c: ControlChar ;
   f: termios.Flag ;
BEGIN
   t := termios.InitTermios() ;
   r := termios.tcgetattr(fd, t) ;
   IF r#0
   THEN
      RETURN FALSE
   END ;
   WITH termio DO
      modes := Modes{} ;
      FOR f := MIN(termios.Flag) TO MAX(termios.Flag) DO
         b := termios.GetFlag(t, f, v) ;
         doFlag(modes, f, v)
      END ;

      (* now find the baud bit and set baud *)

      baud := 0 ;

      b := termios.GetFlag(t, termios.b0, v) ;
      IF v
      THEN
         baud := 0
      END ;
      b := termios.GetFlag(t, termios.b50, v) ;
      IF v
      THEN
         baud := 50
      END ;
      b := termios.GetFlag(t, termios.b75, v) ;
      IF v
      THEN
         baud := 75
      END ;
      b := termios.GetFlag(t, termios.b110, v) ;
      IF v
      THEN
         baud := 110
      END ;
      b := termios.GetFlag(t, termios.b135, v) ;
      IF v
      THEN
         baud := 135
      END ;
      b := termios.GetFlag(t, termios.b150, v) ;
      IF v
      THEN
         baud := 150
      END ;
      b := termios.GetFlag(t, termios.b200, v) ;
      IF v
      THEN
         baud := 200
      END ;
      b := termios.GetFlag(t, termios.b300, v) ;
      IF v
      THEN
         baud := 300
      END ;
      b := termios.GetFlag(t, termios.b600, v) ;
      IF v
      THEN
         baud := 600
      END ;
      b := termios.GetFlag(t, termios.b1200, v) ;
      IF v
      THEN
         baud := 1200
      END ;
      b := termios.GetFlag(t, termios.b1800, v) ;
      IF v
      THEN
         baud := 1800
      END ;
      b := termios.GetFlag(t, termios.b2400, v) ;
      IF v
      THEN
         baud := 2400
      END ;
      b := termios.GetFlag(t, termios.b4800, v) ;
      IF v
      THEN
         baud := 4800
      END ;
      b := termios.GetFlag(t, termios.b9600, v) ;
      IF v
      THEN
         baud := 9600
      END ;
      b := termios.GetFlag(t, termios.b19200, v) ;
      IF v
      THEN
         baud := 19200
      END ;
      b := termios.GetFlag(t, termios.b38400, v) ;
      IF v
      THEN
         baud := 38400
      END ;
      b := termios.GetFlag(t, termios.b57600, v) ;
      IF v
      THEN
         baud := 57600
      END ;
      b := termios.GetFlag(t, termios.b115200, v) ;
      IF v
      THEN
         baud := 115200
      END ;
      b := termios.GetFlag(t, termios.b240400, v) ;
      IF v
      THEN
         baud := 240400
      END ;
      b := termios.GetFlag(t, termios.b460800, v) ;
      IF v
      THEN
         baud := 460800
      END ;
      b := termios.GetFlag(t, termios.b500000, v) ;
      IF v
      THEN
         baud := 500000
      END ;
      b := termios.GetFlag(t, termios.b576000, v) ;
      IF v
      THEN
         baud := 576000
      END ;
      b := termios.GetFlag(t, termios.b921600, v) ;
      IF v
      THEN
         baud := 921600
      END ;
      b := termios.GetFlag(t, termios.b1000000, v) ;
      IF v
      THEN
         baud := 1000000
      END ;
      b := termios.GetFlag(t, termios.b1152000, v) ;
      IF v
      THEN
         baud := 1152000
      END ;
      b := termios.GetFlag(t, termios.b1500000, v) ;
      IF v
      THEN
         baud := 1500000
      END ;
      b := termios.GetFlag(t, termios.b2000000, v) ;
      IF v
      THEN
         baud := 2000000
      END ;
      b := termios.GetFlag(t, termios.b2500000, v) ;
      IF v
      THEN
         baud := 2500000
      END ;
      b := termios.GetFlag(t, termios.b3000000, v) ;
      IF v
      THEN
         baud := 3000000
      END ;
      b := termios.GetFlag(t, termios.b3500000, v) ;
      IF v
      THEN
         baud := 3500000
      END ;
      b := termios.GetFlag(t, termios.b4000000, v) ;
      IF v
      THEN
         baud := 4000000
      END ;
      b := termios.GetFlag(t, termios.maxbaud, v) ;
      IF v
      THEN
         baud := 4000000
      END ;
      
      FOR c := MIN(ControlChar) TO MAX(ControlChar) DO
         b := termios.SetChar(t, c, cc[c])
      END
   END ;

   t := termios.KillTermios(t) ;
   RETURN r=0
END GetTermIO;


PROCEDURE Baudrate (termio: TermIO) : CARDINAL ;
BEGIN
   RETURN termio.baud
END Baudrate;


PROCEDURE Isatty (fd: CARDINAL) : BOOLEAN ;
VAR
   termio: TermIO;
BEGIN
   RETURN GetTermIO(fd, termio)
END Isatty;


END SysTermIO.
