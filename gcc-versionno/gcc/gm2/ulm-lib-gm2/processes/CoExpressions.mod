(* CoExpressions.mod.

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

IMPLEMENTATION MODULE CoExpressions;

   FROM SYSTEM IMPORT ADDRESS, NEWPROCESS, TRANSFER, WORD;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;

   (* (* from definition module *)
   TYPE
      CoRoutine;
   VAR
      main, source: CoRoutine;
   *)

   CONST
      StackSize = 8192;

   TYPE
      ResultType = WORD;
      CoRoutine = POINTER TO Process;

      Process =
         RECORD
            pc: ADDRESS; (* coroutine *)
            stack: ADDRESS; (* pointer to stack of pc *)
            caller: CoRoutine; (* return to caller on failure *)
         END;
   VAR
      cp: CoRoutine;
      failed: BOOLEAN;
      SuspendValue: ResultType;

   PROCEDURE Send(cr: CoRoutine; value: WORD);
   BEGIN
      IF cr = NIL THEN RETURN END;
      source := cp;
      SuspendValue := value;
      failed := FALSE;
      cp := cr;
      TRANSFER(source^.pc, cp^.pc);
   END Send;

   PROCEDURE SendChar(cr: CoRoutine; ch: CHAR);
   BEGIN
      Send(cr, ORD(ch));
   END SendChar;

   PROCEDURE Fail;
      VAR src: CoRoutine;
   BEGIN
      source := cp;
      cp := cp^.caller;
      failed := TRUE;
      TRANSFER(source^.pc, cp^.pc);
   END Fail;

   PROCEDURE Create(VAR cr: CoRoutine; proc: PROC);
   BEGIN
      NEW(cr);
      WITH cr^ DO
         ALLOCATE(stack, StackSize);
         NEWPROCESS(proc, stack, StackSize, pc);
      END;
   END Create;

   PROCEDURE Receive(cr: CoRoutine; VAR value: WORD) : BOOLEAN;
   BEGIN
      WITH cr^ DO
         cr^.caller := cp;
         source := cp;
         cp := cr;
         TRANSFER(source^.pc, cp^.pc);
         IF failed THEN
            DEALLOCATE(stack, StackSize);
            DISPOSE(cr);
            source := NIL;
            RETURN FALSE;
         ELSE
            value := SuspendValue;
            RETURN TRUE;
         END;
      END;
   END Receive;

   PROCEDURE ReceiveChar(cr: CoRoutine; VAR ch: CHAR) : BOOLEAN;
      VAR value: ResultType; returnValue: BOOLEAN;
   BEGIN
      returnValue := Receive(cr, value);
      IF returnValue THEN
         ch := CHR(CARDINAL(value));
      END;
      RETURN returnValue;
   END ReceiveChar;

   PROCEDURE Call(VAR cr: CoRoutine);
      VAR dummy: ResultType;
   BEGIN
      IF NOT Receive(cr, dummy) THEN
         cr := NIL;
      END;
   END Call;

BEGIN
   source := NIL;
   NEW(cp);
   main := cp;
END CoExpressions.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I../sys:. CoExpressions.mod"
 * End:
 *)
