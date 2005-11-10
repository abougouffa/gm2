(* Copyright (C) 2001 Free Software Foundation, Inc. *)
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA *)
IMPLEMENTATION MODULE M2RTS ;


FROM libc IMPORT abort, exit, write ;
FROM NumberIO IMPORT CardToStr ;
FROM StrLib IMPORT StrCopy, StrLen, StrEqual ;
FROM SYSTEM IMPORT ADDRESS, ADR, BITSET ;
FROM ASCII IMPORT nl, nul ;


CONST
   MaxProcedures =   20 ;
   MaxLength     = 4096 ;


VAR
   Ptr      : CARDINAL ;
   List     : ARRAY [0..MaxProcedures] OF PROC ;
   ExitValue: INTEGER ;
   CallExit : BOOLEAN ;


(*
   Terminate - calls each installed termination procedure in reverse order.
*)

PROCEDURE Terminate ;
VAR
   i: CARDINAL ;
BEGIN
   i := Ptr ;
   WHILE i>0 DO
      DEC(i) ;
      List[i]
   END
END Terminate ;


(*
   HALT - terminate the current program.
          The procedure Terminate is called before the program is
          stopped.
*)

PROCEDURE HALT ;
BEGIN
   Terminate ;
   IF CallExit
   THEN
      exit(ExitValue)
   ELSE
      abort
   END
END HALT ;


(*
   ErrorString - writes a string to stderr.
*)

PROCEDURE ErrorString (a: ARRAY OF CHAR) ;
VAR
   buf: ARRAY [0..MaxLength] OF CHAR ;
   n  : INTEGER ;
BEGIN
   StrCopy(a, buf) ;
   n := write(2, ADR(buf), StrLen(buf))
END ErrorString ;


(*
   ErrorMessage - emits an error message to the stderr
*)

PROCEDURE ErrorMessage (message: ARRAY OF CHAR;
                        file: ARRAY OF CHAR;
                        line: CARDINAL;
                        function: ARRAY OF CHAR) ;
VAR
   LineNo: ARRAY [0..10] OF CHAR ;
BEGIN
   ErrorString(file) ; ErrorString(':') ;
   CardToStr(line, 0, LineNo) ;
   ErrorString(LineNo) ; ErrorString(':') ;
   IF NOT StrEqual(function, '')
   THEN
      ErrorString('in ') ;
      ErrorString(function) ;
      ErrorString(' has caused ') ;
   END ;
   ErrorString(message) ;
   LineNo[0] := nl ; LineNo[1] := nul ;
   ErrorString(LineNo) ;
   exit(1)
END ErrorMessage ;


(*
   Halt - provides a more user friendly version of HALT, which takes
          four parameters to aid debugging.
*)

PROCEDURE Halt (file: ARRAY OF CHAR; line: CARDINAL;
                function: ARRAY OF CHAR; description: ARRAY OF CHAR) ;
BEGIN
   ErrorMessage(description, file, line, function) ;
   HALT
END Halt ;


(*
   SubrangeAssignmentError - part of the runtime checking, called if a
                             subrange variable is just about to be assigned an illegal value.
*)

PROCEDURE SubrangeAssignmentError (file: ARRAY OF CHAR;
                                   line: CARDINAL;
                                   function: ARRAY OF CHAR) ;
BEGIN
   ErrorMessage('variable exceeds subrange', file, line, function)
END SubrangeAssignmentError ;


(*
   ArraySubscriptError -  part of the runtime checking, called if an
                          array indice is out of range.
*)

PROCEDURE ArraySubscriptError (file: ARRAY OF CHAR;
                               line: CARDINAL;
                               function: ARRAY OF CHAR) ;
BEGIN
   ErrorMessage('array index out of bounds', file, line, function)
END ArraySubscriptError ;


(*
   FunctionReturnError -  part of the runtime checking, called if a
                          function exits without a RETURN statement.
*)

PROCEDURE FunctionReturnError (file: ARRAY OF CHAR;
                               line: CARDINAL;
                               function: ARRAY OF CHAR) ;
BEGIN
   ErrorMessage('function is attempting to exit without a formal RETURN statement', file, line, function)
END FunctionReturnError ;


(*
   NilPointerError -  part of the runtime checking, called if a
                      the code is about to dereference NIL.
*)

PROCEDURE NilPointerError (file: ARRAY OF CHAR;
                           line: CARDINAL;
                           function: ARRAY OF CHAR) ;
BEGIN
   ErrorMessage('attempted to dereference a value through a NIL pointer', file, line, function)
END NilPointerError ;


(*
   CaseElseError - part of the runtime checking, called if a
                   CASE statement falls into an ELSE statement
                   (which was not declared by the programmer).
*)

PROCEDURE CaseElseError (file: ARRAY OF CHAR;
                         line: CARDINAL;
                         function: ARRAY OF CHAR) ;
BEGIN
   ErrorMessage('needs an ELSE clause to this CASE statement as none of the selection clauses match the expression', file, line, function)
END CaseElseError ;


(*
   ExitOnHalt - if HALT is executed then call exit with the exit code, e.
*)

PROCEDURE ExitOnHalt (e: INTEGER) ;
BEGIN
   ExitValue := e ;
   CallExit := TRUE
END ExitOnHalt ;


(*
   InstallTerminationProcedure - installs a procedure, p, which will
                                 be called when the procedure Terminate
                                 is invoked.
*)

PROCEDURE InstallTerminationProcedure (p: PROC) ;
BEGIN
   IF Ptr=MaxProcedures
   THEN
      ErrorMessage('maximum number of termination procedures have been set', __FILE__, __LINE__, __FUNCTION__)
   ELSE
      List[Ptr] := p ;
      INC(Ptr)
   END
END InstallTerminationProcedure ;


(*
   Length - returns the length of a string, a. This is called whenever
            the user calls LENGTH and the parameter cannot be calculated
            at compile time.
*)

PROCEDURE Length (a: ARRAY OF CHAR) : CARDINAL ;
VAR
   l, h: CARDINAL ;
BEGIN
   l := 0 ;
   h := HIGH(a) ;
   WHILE (l<=h) AND (a[l]#nul) DO
      INC(l)
   END ;
   RETURN( l )
END Length ;


BEGIN
   Ptr := 0 ;
   ExitValue := 0 ;
   CallExit := FALSE   (* default by calling abort *)
END M2RTS.
