IMPLEMENTATION MODULE Debug ;


FROM ASCII IMPORT nul, lf ;
FROM NumberIO IMPORT CardToStr ;
FROM libc IMPORT printf, exit ;
FROM SYSTEM IMPORT TurnInterrupts, OnOrOff ;


(*
   Write - writes out a single character without blocking
*)

PROCEDURE Write (ch: CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   r := printf('%c', ch)
END Write ;


(*
   Halt - writes a message in the format:
          File:Line:Function:Message

          to the debugging device.
*)

PROCEDURE Halt (File    : ARRAY OF CHAR;
                LineNo  : CARDINAL;
                Function: ARRAY OF CHAR;
                Message : ARRAY OF CHAR) ;
CONST
   MaxNoOfDigits = 12 ;  (* should be large enough for most source files.. *)
VAR
   StrNo            : ARRAY [0..MaxNoOfDigits] OF CHAR ;
   OldInterruptState: OnOrOff ;
BEGIN
   OldInterruptState := TurnInterrupts(Off) ;
   DebugString(File) ;
   CardToStr(LineNo, 0, StrNo) ;
   DebugString(':') ;
   DebugString(StrNo) ;
   DebugString(':') ;
   DebugString(Function) ;
   DebugString(':') ;
   DebugString(Message) ;
   DebugString('\n') ;
   exit(1)
END Halt ;


(*
   DebugString - writes a string to the debugging device (Scn.Write).
                 It interprets \n as carriage return, linefeed.
*)

PROCEDURE DebugString (a: ARRAY OF CHAR) ;
VAR
   n, high          : CARDINAL ;
   OldInterruptState: OnOrOff ;
BEGIN
   OldInterruptState := TurnInterrupts(Off) ;
   high := HIGH( a ) ;
   n := 0 ;

   (* your code needs to go here *)

   WHILE (n <= high) AND (a[n] # nul) DO     (* remove for student *)
      IF a[n]='\'                            (* remove for student *)
      THEN                                   (* remove for student *)
         IF n+1<=high                        (* remove for student *)
         THEN                                (* remove for student *)
            IF a[n+1]='n'                    (* remove for student *)
            THEN                             (* remove for student *)
               WriteLn ;                     (* remove for student *)
               INC(n)                        (* remove for student *)
            ELSIF a[n+1]='\'                 (* remove for student *)
            THEN                             (* remove for student *)
               Write('\') ;                  (* remove for student *)
               INC(n)                        (* remove for student *)
            END                              (* remove for student *)
         END                                 (* remove for student *)
      ELSE                                   (* remove for student *)
         Write( a[n] )                       (* remove for student *)
      END ;                                  (* remove for student *)
      INC( n )                               (* remove for student *)
   END ;                                     (* remove for student *)
   OldInterruptState := TurnInterrupts(OldInterruptState)
END DebugString ;


(*
   WriteLn - writes a carriage return and a newline
             character.
*)

PROCEDURE WriteLn ;
BEGIN
   Write(lf)
END WriteLn ;


END Debug.
