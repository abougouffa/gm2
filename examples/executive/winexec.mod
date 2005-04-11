MODULE winexec ;


IMPORT Debug ;
IMPORT WindowDevice ;

FROM WindowDevice IMPORT Window, InitWindow, SetWindow, TitleWindow,
                         WriteChar, PutOnTop ;

FROM SysVec IMPORT InitInputVector, InitOutputVector ;
FROM SYSTEM IMPORT TurnInterrupts, PRIORITY, ADR ;
FROM ncurses IMPORT Blue, Red, Magenta, White, Green, Yellow ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT PushOutput, Write ;
FROM libc IMPORT write, read ;
FROM Executive IMPORT DESCRIPTOR, InitProcess, Resume,
                      Suspend, GetCurrentProcess, Ps,
                      SEMAPHORE, InitSemaphore, Wait, Signal, WaitForIO ;

VAR
   First,
   Second   : Window ;
   Debugging: Window ;


(*
   SetupWindows - sets up three windows, First, Second and Debugging.
                  After this procedure has been called all StdIO
                  writes will go through LocalWrite.
*)

PROCEDURE SetupWindows ;
BEGIN
   WriteString('\nBefore SetWindow') ;

   (* first process window *)
   First := SetWindow(InitWindow(), Blue, White, 38, 9, 1, 1, TRUE) ;
   WriteString('\nBefore TitleWindow') ;
   TitleWindow(First, 'Initial process') ;

   (* second process window *)
   Second := SetWindow(InitWindow(), Magenta, White, 36, 9, 42, 1, TRUE) ;
   TitleWindow(Second, 'Second process') ;

   (* debugging window at the bottom *)
   Debugging := SetWindow(InitWindow(), Red, White, 77, 11, 1, 12, TRUE) ;
   TitleWindow(Debugging, 'Debugging output') ;
   PutOnTop(Debugging) ;

   PushOutput(LocalWrite) ;
   Debug.PushOutput(LocalWrite)
END SetupWindows ;


(*
   LocalWrite - 
*)

PROCEDURE LocalWrite (ch: CHAR) ;
BEGIN
   IF GetCurrentProcess()=ProcA
   THEN
      WindowDevice.WriteChar(First, ch)
   ELSIF GetCurrentProcess()=ProcB
   THEN
      WindowDevice.WriteChar(Second, ch)
   ELSE
      WindowDevice.WriteChar(Debugging, ch)
   END
END LocalWrite ;


PROCEDURE LocalRead (VAR ch: CHAR) ;
VAR
   r: INTEGER ;
   v: CARDINAL ;
BEGIN
   WriteString('inside LocalRead (before WaitForIO)\n') ;
   v := InitInputVector(0, MAX(PRIORITY)) ;
   WaitForIO(v) ;
   WriteString('before read\n') ;
   r := read(0, ADR(ch), 1) ;
   WriteString('after read\n')
END LocalRead ;


(*
   ProcessA - 
*)

PROCEDURE ProcessA ;
BEGIN
   OldInts := TurnInterrupts(MIN(PRIORITY)) ;
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
BEGIN
   OldInts := TurnInterrupts(MIN(PRIORITY)) ;
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
   OldInts     : PRIORITY ;
   ch          : CHAR ;
BEGIN
   WriteString('got to OS\n') ;

   ProcA := NIL ;
   ProcB := NIL ;
   SetupWindows ;

   FromA := InitSemaphore(0, 'FromA') ;
   FromB := InitSemaphore(1, 'FromB') ;

   WriteString('lots of text to be displayed\n') ;
   WriteString('now to create a process...\n') ;

   ProcA := Resume(InitProcess(ProcessA, StackSize, 'Process1')) ;
   ProcB := Resume(InitProcess(ProcessB, StackSize, 'Process2')) ;

   LOOP
      LocalRead(ch) ;
      Write(ch) ;
      IF ch='p'
      THEN
         Ps
      END
   END
END winexec.

