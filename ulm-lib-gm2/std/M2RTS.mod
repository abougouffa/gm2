(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                 2010
                 Free Software Foundation, Inc. *)
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
FROM Conversions IMPORT ConvertCardinal ;
FROM Strings IMPORT StrCpy, StrLen, StrCmp ;
FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM ASCII IMPORT nl, nul ;

(* RTExceptions is not used in this module - but it is a
   runtime module and it is required during linking *)
IMPORT RTExceptions ;


CONST
   MaxProcedures = 1024 ;
   MaxLength     = 4096 ;


VAR
   iPtr, tPtr   : CARDINAL ;
   InitialProc,
   TerminateProc: ARRAY [0..MaxProcedures] OF PROC ;
   ExitValue    : INTEGER ;
   CallExit     : BOOLEAN ;


(*
   ExecuteTerminationProcedures - calls each installed termination procedure
                                  in reverse order.
*)

PROCEDURE ExecuteTerminationProcedures ;
VAR
   i: CARDINAL ;
BEGIN
   i := tPtr ;
   WHILE i>0 DO
      DEC(i) ;
      TerminateProc[i]
   END
END ExecuteTerminationProcedures ;


(*
   InstallTerminationProcedure - installs a procedure, p, which will
                                 be called when the procedure
                                 ExecuteTerminationProcedures
                                 is invoked.  It returns TRUE is the
                                 procedure is installed.
*)

PROCEDURE InstallTerminationProcedure (p: PROC) : BOOLEAN ;
BEGIN
   IF tPtr>MaxProcedures
   THEN
      RETURN( FALSE )
   ELSE
      TerminateProc[tPtr] := p ;
      INC(tPtr) ;
      RETURN( TRUE )
   END
END InstallTerminationProcedure ;


(*
   ExecuteInitialProcedures - executes the initial procedures installed by
                              InstallInitialProcedure.
*)

PROCEDURE ExecuteInitialProcedures ;
VAR
   i: CARDINAL ;
BEGIN
   i := iPtr ;
   WHILE i>0 DO
      DEC(i) ;
      InitialProc[i]
   END
END ExecuteInitialProcedures ;


(*
   InstallInitialProcedure - installs a procedure to be executed just before the
                             BEGIN code section of the main program module.
*)

PROCEDURE InstallInitialProcedure (p: PROC) : BOOLEAN ;
BEGIN
   IF iPtr>MaxProcedures
   THEN
      RETURN( FALSE )
   ELSE
      InitialProc[iPtr] := p ;
      INC(iPtr) ;
      RETURN( TRUE )
   END
END InstallInitialProcedure ;


(*
   HALT - terminate the current program.  The procedure
          ExecuteTerminationProcedures is called before the
          program is stopped.  The parameter exitcode is optional.
          If the parameter is not supplied HALT will call libc 'abort',
          otherwise it will exit with the code supplied.  Supplying a
          parameter to HALT has the same effect as calling ExitOnHalt
          with the same code and then calling HALT with no parameter.
*)

PROCEDURE HALT ([exitcode: INTEGER = -1]) ;
BEGIN
   IF exitcode#-1
   THEN
      CallExit := TRUE ;
      ExitValue := exitcode
   END ;
   ExecuteTerminationProcedures ;
   IF CallExit
   THEN
      exit(ExitValue)
   ELSE
      abort
   END
END HALT ;


(*
   Terminate - provides compatibility for pim.  It call exit with
               the exitcode provided in a prior call to ExitOnHalt
               (or zero if ExitOnHalt was never called).  It does
               not call ExecuteTerminationProcedures.
*)

PROCEDURE Terminate ;
BEGIN
   exit(ExitValue)
END Terminate ;


(*
   ErrorString - writes a string to stderr.
*)

PROCEDURE ErrorString (a: ARRAY OF CHAR) ;
VAR
   buf: ARRAY [0..MaxLength] OF CHAR ;
   n  : INTEGER ;
BEGIN
   StrCpy(buf, a) ;
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
   ConvertCardinal(line, 0, LineNo) ;
   ErrorString(LineNo) ; ErrorString(':') ;
   IF StrCmp(function, '')#0
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
   ErrorCharStar - 
*)

PROCEDURE ErrorCharStar (a: ADDRESS) ;
VAR
   p: POINTER TO CHAR ;
   n: INTEGER ;
BEGIN
   p := a ;
   n := 0 ;
   WHILE (p#NIL) AND (p^#nul) DO
      INC(n) ;
      INC(p)
   END ;
   IF n>0
   THEN
      n := write(2, a, n)
   END
END ErrorCharStar ;


(*
   ErrorMessageColumn - emits an error message to the stderr
*)

PROCEDURE ErrorMessageColumn (filename, scope, message: ADDRESS;
                              line, column: CARDINAL) ;
VAR
   LineNo: ARRAY [0..10] OF CHAR ;
BEGIN
   ErrorCharStar(filename) ; ErrorString(':') ;
   ConvertCardinal(line, 0, LineNo) ;
   ErrorString(LineNo) ; ErrorString(':') ;
   ConvertCardinal(column, 0, LineNo) ;
   ErrorString(LineNo) ; ErrorString(':') ;
   ErrorCharStar(scope) ; ErrorString(':') ;
   ErrorCharStar(message) ;
   LineNo[0] := nl ; LineNo[1] := nul ;
   ErrorString(LineNo) ;
   exit(1)
END ErrorMessageColumn ;


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
   ErrorMessageColumn(filename, scope,
                      ADR("variable exceeds range during assignment"),
                      line, column)
END AssignmentException ;


PROCEDURE IncException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("variable exceeds range during INC statement"),
                      line, column)
END IncException ;


PROCEDURE DecException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("variable exceeds range during DEC statement"),
                      line, column)
END DecException ;


PROCEDURE InclException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("bit exceeds set range during INCL statement"),
                      line, column)
END InclException ;


PROCEDURE ExclException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("bit exceeds set range during EXCL statement"),
                      line, column)
END ExclException ;


PROCEDURE ShiftException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("bit exceeds set range during SHIFT statement"),
                      line, column)
END ShiftException ;


PROCEDURE RotateException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("bit exceeds set range during ROTATE statement"),
                      line, column)
END RotateException ;


PROCEDURE StaticArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("array index out of bounds during static array access"),
                      line, column)
END StaticArraySubscriptException ;


PROCEDURE DynamicArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("array index out of bounds during dynamic array access"),
                      line, column)
END DynamicArraySubscriptException ;


PROCEDURE ForLoopBeginException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("iterator variable exceeds range during FOR loop initial assignment"),
                      line, column)
END ForLoopBeginException ;


PROCEDURE ForLoopToException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("iterator variable exceeds range when calculating final value in FOR loop"),
                      line, column)
END ForLoopToException ;


PROCEDURE ForLoopEndException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("iterator variable exceeds range during increment at the end of a FOR loop"),
                      line, column)
END ForLoopEndException ;


PROCEDURE PointerNilException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("attempting to dereference a NIL valued pointer"),
                      line, column)
END PointerNilException ;


PROCEDURE NoReturnException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("about to finish a PROCEDURE without executing a RETURN statement"),
                      line, column)
END NoReturnException ;


PROCEDURE CaseException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("the expression in the CASE statement cannot be selected"),
                      line, column)
END CaseException ;


PROCEDURE WholeNonPosDivException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("the division expression has a divisor which is less than or equal to zero"),
                      line, column)
END WholeNonPosDivException ;


PROCEDURE WholeNonPosModException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("the modulus expression has a divisor which is less than or equal to zero"),
                      line, column)
END WholeNonPosModException ;


PROCEDURE WholeZeroDivException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("the division expression has a divisor which is equal to zero"),
                      line, column)
END WholeZeroDivException ;


PROCEDURE WholeZeroRemException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("the remainder expression has a divisor which is equal to zero"),
                      line, column)
END WholeZeroRemException ;


PROCEDURE NoException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   ErrorMessageColumn(filename, scope,
                      ADR("M2Expection was called when no there was no outstanding exception to be returned"),
                      line, column)
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
   iPtr := 0 ;
   tPtr := 0 ;
   ExitValue := 0 ;
   CallExit := FALSE   (* default by calling abort *)
END M2RTS.
