(* Copyright (C) 2003 Free Software Foundation, Inc. *)
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
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

MODULE testexecutive ;


FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT PushOutput, Write ;
FROM SYSTEM IMPORT ADR, TurnInterrupts, OnOrOff ;
FROM libc IMPORT write, read ;
FROM ASCII IMPORT nl ;
FROM SysVec IMPORT InitInputVector, InitOutputVector ;

FROM Executive IMPORT DESCRIPTOR, InitProcess, Resume,
                      Suspend, GetCurrentProcess, Ps,
                      SEMAPHORE, InitSemaphore, Wait, Signal,
                      WaitForIO ;


PROCEDURE LocalWrite (ch: CHAR) ;
VAR
   r: INTEGER ;
   v: CARDINAL ;
BEGIN
(*
   v := InitOutputVector(1) ;
   WaitForIO(v) ;
*)
   r := write(1, ADR(ch), 1)
END LocalWrite ;


PROCEDURE LocalRead (VAR ch: CHAR) ;
VAR
   r: INTEGER ;
   v: CARDINAL ;
BEGIN
   v := InitInputVector(0) ;
   WaitForIO(v) ;
   r := read(0, ADR(ch), 1)
END LocalRead ;


(*
   ProcessA - 
*)

PROCEDURE ProcessA ;
VAR
   InterruptState: OnOrOff ;
BEGIN
   InterruptState := TurnInterrupts(On) ;
   LOOP
      Wait(FromB) ;
      WriteString('A: is this going to work? ') ;
      Signal(FromA)
   END
END ProcessA ;


(*
   ProcessB - 
*)

PROCEDURE ProcessB ;
VAR
   InterruptState: OnOrOff ;
BEGIN
   InterruptState := TurnInterrupts(On) ;
   LOOP
      Wait(FromA) ;
      WriteString('B: is this going to work? ') ;
      Signal(FromB)
   END
END ProcessB ;


CONST
   StackSize = 0100000H ;

VAR
   ProcA, ProcB: DESCRIPTOR ;
   FromA, FromB: SEMAPHORE ;
   ch          : CHAR ;
BEGIN
   WriteString('got to OS\n') ;

   ProcA := NIL ;
   ProcB := NIL ;

   PushOutput(LocalWrite) ;

   FromA := InitSemaphore(0, 'FromA') ;
   FromB := InitSemaphore(1, 'FromB') ;

   WriteString('lots of text to be displayed\n') ;
   WriteString('now to create a process...\n') ;

   ProcA := Resume(InitProcess(ProcessA, StackSize, 'Process1')) ;
   ProcB := Resume(InitProcess(ProcessB, StackSize, 'Process2')) ;

   LOOP
      LocalRead(ch) ;
(*
      Write('>') ; Write(' ') ;
      Write(ch) ;
      Write(nl) ;
*)
      IF ch='p'
      THEN
         Ps
      END
   END
END testexecutive.