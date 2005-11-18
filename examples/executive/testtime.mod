(* Copyright (C) 2005 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA *)
MODULE testtime ;


FROM Debug IMPORT Halt ;
FROM StdIO IMPORT PushOutput ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM TimerHandler IMPORT EVENT, TicksPerSecond, Sleep, ArmEvent,
                         Cancel, WaitOn, ReArmEvent ;
FROM SYSTEM IMPORT PRIORITY, TurnInterrupts ;
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
   OldInts := TurnInterrupts(MIN(PRIORITY)) ;
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
   OldInts := TurnInterrupts(MIN(PRIORITY)) ;
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
   OldInts := TurnInterrupts(MAX(PRIORITY)) ;
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
   OldInts : PRIORITY ;
   Timeout : EVENT ;
   ch      : CHAR ;
BEGIN
   OldInts := TurnInterrupts(MIN(PRIORITY)) ;
   PushOutput(LocalWrite) ;
   WriteString('got to OS\n') ;

   WriteString('now to create three processes...') ;  WriteLn ;

   p10 := Resume(InitProcess(TenSeconds    , StackSize, '10')) ;
   p15 := Resume(InitProcess(FifteenSeconds, StackSize, '15')) ;
   p60 := Resume(InitProcess(SixtySeconds  , StackSize, '60')) ;

   Sleep(120*TicksPerSecond) ;
   Halt(__FILE__, __LINE__, __FUNCTION__, '\nsuccessfully completed\n')
END testtime.
