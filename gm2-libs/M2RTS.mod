(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM ASCII IMPORT nl, nul ;
FROM RTExceptions IMPORT Raise ;

IMPORT M2EXCEPTION ;



CONST
   MaxProcedures = 1024 ;


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
   n: INTEGER ;
BEGIN
   n := write(2, ADR(a), StrLen(a))
END ErrorString ;


(*
   ErrorMessage - emits an error message to stderr
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
   The following are the runtime exception handler routines.
*)

PROCEDURE AssignmentException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(M2EXCEPTION.rangeException,
         filename, line, column, scope,
         ADR("variable exceeds range during assignment"))
END AssignmentException ;


PROCEDURE IncException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(M2EXCEPTION.rangeException,
         filename, line, column, scope,
         ADR("variable exceeds range during INC statement"))
END IncException ;


PROCEDURE DecException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(M2EXCEPTION.rangeException,
         filename, line, column, scope,
         ADR("variable exceeds range during DEC statement"))
END DecException ;


PROCEDURE StaticArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(M2EXCEPTION.indexException,
         filename, line, column, scope,
         ADR("array index out of bounds during static array access"))
END StaticArraySubscriptException ;


PROCEDURE DynamicArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(M2EXCEPTION.indexException,
         filename, line, column, scope,
         ADR("array index out of bounds during dynamic array access"))
END DynamicArraySubscriptException ;


PROCEDURE ForLoopBeginException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(M2EXCEPTION.rangeException,
         filename, line, column, scope,
         ADR("iterator variable exceeds range during FOR loop initial assignment"))
END ForLoopBeginException ;


PROCEDURE ForLoopToException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(M2EXCEPTION.rangeException,
         filename, line, column, scope,
         ADR("iterator variable exceeds range when calculating final value in FOR loop"))
END ForLoopToException ;


PROCEDURE ForLoopEndException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(M2EXCEPTION.rangeException,
         filename, line, column, scope,
         ADR("iterator variable exceeds range during increment at the end of a FOR loop"))
END ForLoopEndException ;


PROCEDURE PointerNilException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(M2EXCEPTION.invalidLocation,
         filename, line, column, scope,
         ADR("attempting to dereference a NIL valued pointer"))
END PointerNilException ;


PROCEDURE NoReturnException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(M2EXCEPTION.functionException,
         filename, line, column, scope,
         ADR("about to finish a PROCEDURE without executing a RETURN statement"))
END NoReturnException ;


PROCEDURE CaseException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(M2EXCEPTION.caseSelectException,
         filename, line, column, scope,
         ADR("the expression in the CASE statement cannot be selected"))
END CaseException ;


PROCEDURE NoException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(M2EXCEPTION.exException,
         filename, line, column, scope,
         ADR("M2Expection was called when no there was no outstanding exception to be returned"))
END NoException ;


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
