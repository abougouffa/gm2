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

IMPLEMENTATION MODULE Clock;

   FROM SYSTEM IMPORT ADR, UNIXCALL;
   FROM Sys IMPORT times, gettimeofday;
   FROM SysTime IMPORT Time;
   FROM SystemTypes IMPORT TIME;

   TYPE
      TimesRec =
         RECORD
            utime: TIME;        (* CPU time while in user mode *)
            stime: TIME;        (* CPU time while in system mode *)
            cutime: TIME;       (* user time of all children *)
            cstime: TIME;       (* system time of all children *)
         END;
      TimeVal =
         RECORD
            tvsec: TIME;
            tvusec: TIME;
         END;

   VAR
      real: TimeVal;
      cpu: TIME;

   PROCEDURE GetProcessTimes(VAR timebuf: TimesRec);
      VAR
         d0, d1: INTEGER;

   BEGIN
      IF ~UNIXCALL(times, d0, d1, ADR(timebuf)) THEN
	 WITH timebuf DO
	    utime := 0; stime := 0; cutime := 0; cstime := 0;
	 END;
      END;
   END GetProcessTimes;

   PROCEDURE GetTimeVal(VAR timeval: TimeVal);
      VAR
         d0, d1: INTEGER;
   BEGIN
      IF ~UNIXCALL(gettimeofday, d0, d1, ADR(timeval), 0) THEN
         timeval.tvsec := 0;
         timeval.tvusec := 0;
      END;
   END GetTimeVal;

   PROCEDURE RealTime(reset: BOOLEAN) : TIME;
      (* return elapsed real time in units elapsed since the
         start of the process or since the last call with
         argument TRUE
      *)
      VAR
         result: TIME;
         buf   : TimeVal;

      PROCEDURE DiffInUnits(tval1, tval2: TimeVal) : TIME;
         VAR
            diff: TIME;
      BEGIN
         RETURN (tval2.tvsec - tval1.tvsec) * UnitsPerSecond + (tval2.tvusec
            - tval1.tvusec) DIV 100 * UnitsPerSecond DIV 10000;
      END DiffInUnits;

   BEGIN
      GetTimeVal(buf);
      result := DiffInUnits(real, buf);
      IF reset THEN
         real := buf;
      END;
      RETURN result
   END RealTime;

   PROCEDURE CPUTime (reset: BOOLEAN): TIME;
      VAR
         result: TIME;
         buf   : TimesRec;
   BEGIN
      GetProcessTimes(buf);
      result := buf.utime + buf.stime + buf.cutime + buf.cstime - cpu;
      IF reset THEN
         INC(cpu, result);
      END;
      RETURN result
   END CPUTime;

BEGIN
   GetTimeVal(real);
   cpu := 0;
END Clock.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I../sys:. Clock.mod"
 * End:
 *)
