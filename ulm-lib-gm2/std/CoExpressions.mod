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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: CoExpressions.mod,v 1.2 2004/06/22 18:14:01 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: CoExpressions.mod,v $
   Revision 1.2  2004/06/22 18:14:01  gaius
   fixed parameters in ulm directory

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

   Revision 0.2  1997/02/28  15:49:55  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:21  borchert
   Initial revision

   ----------------------------------------------------------------------------
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
