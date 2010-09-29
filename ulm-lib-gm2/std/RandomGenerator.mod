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

IMPLEMENTATION MODULE RandomGenerator;

   FROM SYSTEM IMPORT ADR, UNIXCALL;
   FROM Sys IMPORT getpid, gettimeofday;
   FROM SystemTypes IMPORT TIME;
   FROM SysTime IMPORT Time;

   (* implementation uses subtractive method taken from
      Knuth, Seminumerical Algorithms, 3.2.2 and 3.6
      X[n] = (X[n-k] - X[n-l]) MOD m
      pairs of (l, k) which yields long periods mod 2 are
      found in Table 1 in 3.2.2
      examples:
      (24,55), (7, 57), (19, 58),
      (4, 81), (16, 81), (35, 81), (27, 98)
   *)

   CONST
      (* subscript pair: l < k *)
      l = 24; k = 55; values = k;

   TYPE
      State =
	 RECORD
	    value: ARRAY [0..values-1] OF REAL;
	    index: INTEGER;
	    seed: REAL;
	 END;
   VAR
      state: State;
	 (* currently, we just maintain a global state -- but the
	    sources may be easily changed to support more than one
	    sequence of pseudo-random numbers
	 *)

   (* === private procedures ============================================ *)

   PROCEDURE NextValues(VAR state: State);
      VAR
         i: INTEGER;
         val: REAL;
   BEGIN
      WITH state DO
         i := 0;
         WHILE i < l DO
	    val := value[i] - value[i+k-l];
	    IF val < 0.0 THEN
	       val := val + 1.0;
	    END;
	    value[i] := val;
	    INC(i);
         END;
         WHILE i < k DO
	    val := value[i] - value[i-l];
	    IF val < 0.0 THEN
	       val := val + 1.0;
	    END;
	    value[i] := val;
	    INC(i);
         END;
         index := 0;
      END;
   END NextValues;

   PROCEDURE Warmup(VAR state: State);
      VAR
	 i, j: INTEGER;
	 val1, val2: REAL;
   BEGIN
      WITH state DO
	 i := 0;
	 val1 := seed;
	 val2 := 0.731;
	 WHILE i < values DO
	    j := 21 * i MOD values;
	    value[j] := val1;
	    val1 := val2 - val1;
	    IF val1 < 0.0 THEN
	       val1 := val1 + 1.0;
	    END;
	    val2 := value[j];
	    INC(i);
         END;

         (* "warm up" the generator *)
         i := 0;
         WHILE i < values DO
	    NextValues(state); 
	    INC(i);
         END;
      END;
   END Warmup;

   PROCEDURE Entier(val: REAL) : INTEGER;
      (* round towards minus infinity, TRUNC rounds towards 0 *)
   BEGIN
      IF val >= 0.0 THEN
	 RETURN TRUNC(val)
      ELSE
	 RETURN - TRUNC(-val)
      END;
   END Entier;

   PROCEDURE GetSeed() : INTEGER;
      VAR
	 timeseed: TIME;

      PROCEDURE Times(): INTEGER;
	 TYPE
	    Timeval = RECORD sec,usec: CARDINAL END;
	    Timezone = RECORD minwest, dsttime: CARDINAL END;
	 VAR
	    val: Timeval;
	    zone: Timezone;
	    r0, r1: INTEGER;
      BEGIN
	 IF ~UNIXCALL(gettimeofday, r0, r1, ADR(val), ADR(zone)) THEN END;
	 RETURN val.usec
      END Times;

      PROCEDURE GetPid(): INTEGER;
	 VAR r0, r1: INTEGER;
      BEGIN
	 IF ~UNIXCALL(getpid, r0, r1) THEN END;
	 RETURN r0 + r1
      END GetPid;

   BEGIN 
      IF ~Time(timeseed) THEN
	 RETURN Times();
      ELSE
	 RETURN VAL(INTEGER, timeseed) * GetPid() + Times();
      END;
   END GetSeed;

   (* === exported procedures =========================================== *)
   
   PROCEDURE RealVal() : REAL;
   BEGIN
      WITH state DO
	 IF index >= values THEN
	    NextValues(state);
	 END;
         INC(index);
         RETURN value[index-1] 
      END;
   END RealVal;

   PROCEDURE IntVal() : INTEGER;
      (* get random 32-bit value *)
      VAR
	 real: REAL;
   BEGIN
      real := RealVal();
      RETURN Entier((1.0 - real - real) * FLOAT(MIN(INTEGER)))
   END IntVal;

   PROCEDURE Random(low, high: INTEGER): INTEGER;
      (* get a uniformly distributed integer in [low..high] *)
   BEGIN
      RETURN Entier(FLOAT(low) +
                    RealVal() * (1.0 + FLOAT(high) - FLOAT(low)))
   END Random;

   PROCEDURE Flip() : BOOLEAN;
      (* return TRUE or FALSE *)
   BEGIN
      RETURN IntVal() >= 0
   END Flip;

   PROCEDURE Init(seed: INTEGER);
      (* initializes the random generator;
	 the sequence of random numbers depends on seed
      *)

      VAR
         seedval: REAL;
   BEGIN
      IF seed = MIN(INTEGER) THEN
         seed := MAX(INTEGER);
      ELSIF seed < 0 THEN
         seed := ABS(seed);
      END;
      IF seed > 1 THEN
         seedval := 1.0 / FLOAT(seed);
      ELSE
         seedval := 0.172;
      END;
      (* seedval now in [0..1) *)

      state.seed := seedval;
      Warmup(state);
   END Init;

BEGIN
   Init(GetSeed());
END RandomGenerator. 
