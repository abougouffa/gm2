(* Ulm's Modula-2 Library
   Copyright (C) 1984-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
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
   $Id: RandomGenerator.mod,v 1.3 2005/11/21 12:09:59 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: RandomGenerator.mod,v $
   Revision 1.3  2005/11/21 12:09:59  gaius
   updated Copyright notices and dates

   Revision 1.2  2004/06/29 08:51:42  gaius
   * made flex lexical analysers ignore carriage return
   * fixed bug in M2Quads.mod checking parameter of
     a const var before value was known.
   * fixed local MODULEs so that they can FROM mod IMPORT
   * tidied up some ulm implementation modules in ulm-lib-gm2/std

   Revision 1.1  2004/05/05 21:34:58  gaius
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

   Revision 1.1  2003/12/27 00:16:05  gaius
   added ulm libraries into the gm2 tree. Currently these
   are only used when regression testing, but later they
   will be accessible by users of gm2.

   Revision 0.2  1997/02/28  15:50:25  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:31  borchert
   Initial revision

   ----------------------------------------------------------------------------
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
