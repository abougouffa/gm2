(* testtime.mod create processes which test the TimerHandler.

Copyright (C) 2001-2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

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

MODULE testtime ;


FROM Debug IMPORT Halt ;
FROM StdIO IMPORT PushOutput ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM TimerHandler IMPORT EVENT, TicksPerSecond, Sleep, ArmEvent,
                         Cancel, WaitOn, ReArmEvent ;
FROM SYSTEM IMPORT TurnInterrupts ;
FROM COROUTINES IMPORT PROTECTION ;
FROM Executive IMPORT DESCRIPTOR, InitProcess, Resume, Ps ;
FROM SYSTEM IMPORT ADR ;
FROM libc IMPORT write ;


(*
   LocalWrite -
*)

PROCEDURE LocalWrite (ch: CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   r := write(1, ADR(ch), 1)
END LocalWrite ;


(*
   TenSeconds -
*)

PROCEDURE TenSeconds ;
BEGIN
   OldInts := TurnInterrupts(MIN(PROTECTION)) ;
   WriteString('.') ;
   LOOP
      Sleep(10*TicksPerSecond) ;
      WriteString('..10..') ;
   END
END TenSeconds ;


(*
   FifteenSeconds -
*)

PROCEDURE FifteenSeconds ;
BEGIN
   OldInts := TurnInterrupts(MIN(PROTECTION)) ;
   WriteString('.') ;
   LOOP
      Sleep(15*TicksPerSecond) ;
      WriteString('..15..') ;
   END
END FifteenSeconds ;


(*
   SixtySeconds -
*)

PROCEDURE SixtySeconds ;
BEGIN
   OldInts := TurnInterrupts(MAX(PROTECTION)) ;
   WriteString('.') ;
   LOOP
      Timeout := ArmEvent(60*TicksPerSecond) ;
      IF WaitOn(Timeout)
      THEN
         WriteString('...someone cancelled it...')
      ELSE
         WriteString('..60 seconds Alarm..')
      END ;
      WriteLn
   END
END SixtySeconds ;


CONST
   StackSize = 0100000H ;

VAR
   p10, p15,
   p60     : DESCRIPTOR ;
   OldInts : PROTECTION ;
   Timeout : EVENT ;
   ch      : CHAR ;
BEGIN
   OldInts := TurnInterrupts(MIN(PROTECTION)) ;
   PushOutput(LocalWrite) ;
   WriteString('got to OS\n') ;

   WriteString('now to create three processes...') ;  WriteLn ;

   p10 := Resume(InitProcess(TenSeconds    , StackSize, '10')) ;
   p15 := Resume(InitProcess(FifteenSeconds, StackSize, '15')) ;
   p60 := Resume(InitProcess(SixtySeconds  , StackSize, '60')) ;

   Sleep(120*TicksPerSecond) ;
   Halt(__FILE__, __LINE__, __FUNCTION__, '\nsuccessfully completed\n')
END testtime.
