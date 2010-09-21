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

IMPLEMENTATION MODULE Processes;

   (* see N. Wirth: Programming in Modula-2, pp. 128 *)

   FROM SYSTEM IMPORT ADDRESS, NEWPROCESS, TRANSFER, TSIZE;
   FROM Storage IMPORT ALLOCATE;
   FROM SysPanic IMPORT Panic;

   TYPE
      SIGNAL = POINTER TO ProcessDescriptor;
      ProcessDescriptor =
         RECORD
            next: SIGNAL; (* ring *)
            queue: SIGNAL; (* queue of waiting processes *)
            cor: ADDRESS;
            ready: BOOLEAN;
         END;

   VAR
      cp: SIGNAL; (* current process *)

   PROCEDURE StartProcess(P: PROC; n: CARDINAL);
      VAR s0: SIGNAL; wsp: ADDRESS;
   BEGIN
      s0 := cp; ALLOCATE(wsp, n);
      ALLOCATE(cp, TSIZE(ProcessDescriptor));
      WITH cp^ DO
         next := s0^.next;
         s0^.next := cp;
         ready := TRUE;
         queue := NIL;
      END;
      NEWPROCESS(P, wsp, n, cp^.cor);
      TRANSFER(s0^.cor, cp^.cor);
   END StartProcess;

   PROCEDURE SEND(VAR s: SIGNAL);
      VAR s0: SIGNAL;
   BEGIN
      IF s <> NIL THEN
         s0 := cp;
         cp := s;
         WITH cp^ DO
            s := queue;
            ready := TRUE;
            queue := NIL;
         END;
         TRANSFER(s0^.cor, cp^.cor);
      END;
   END SEND;

   PROCEDURE WAIT(VAR s: SIGNAL);
      VAR s0, s1: SIGNAL;
   BEGIN
      (* insert cp in queue s *)
      IF s = NIL THEN
         s := cp;
      ELSE
         s0 := s;
         s1 := s0^.queue;
         WHILE s1 <> NIL DO
            s0 := s1;
            s1 := s0^.queue;
         END;
         s0^.queue := cp;
      END;
      s0 := cp;
      REPEAT
         cp := cp^.next;
      UNTIL cp^.ready;
      IF cp = s0 THEN
         (* deadlock *)
         Panic("Deadlock.");
      END;
      s0^.ready := FALSE;
      TRANSFER(s0^.cor, cp^.cor);
   END WAIT;

   PROCEDURE Awaited(s: SIGNAL) : BOOLEAN;
   BEGIN
      RETURN s <> NIL;
   END Awaited;

   PROCEDURE Init(VAR s: SIGNAL);
   BEGIN
      s := NIL;
   END Init;

BEGIN
   ALLOCATE(cp, TSIZE(ProcessDescriptor));
   WITH cp^ DO
      next := cp;
      ready := TRUE;
      queue := NIL;
   END;
END Processes.
