(* Copyright (C) 2001 Free Software Foundation, Inc. *)
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
IMPLEMENTATION MODULE M2Options ;


IMPORT CmdArgs ;
FROM SArgs IMPORT GetArg, Narg ;
FROM Strings IMPORT String, InitString, Mark, Slice, EqualArray ;
FROM M2Search IMPORT PrependSearchPath ;
FROM M2Version IMPORT WriteVersion ;
FROM M2Printf IMPORT printf0, printf1 ;
FROM libc IMPORT exit ;
FROM Debug IMPORT Halt ;


(*
   DisplayVersion - displays the version of the compiler.
*)

PROCEDURE DisplayVersion ;
BEGIN
   printf0('gm2  [') ;
   WriteVersion ;
   printf1('] [%s]\n', Mark(InitString(__DATE__)))
END DisplayVersion ;


(*
   ParseOptions - parses the options and sets the option flags
                  accordingly.
*)

PROCEDURE ParseOptions ;
VAR
   s   : String ;
   i, n: CARDINAL ;
BEGIN
   n := Narg() ;
   IF n>1
   THEN
      i := 1 ;
      REPEAT
         IF GetArg(s, i)
         THEN
            IF EqualArray(Mark(Slice(s, 0, 1)), '-I')
            THEN
               PrependSearchPath(Slice(s, 2, -1))
            ELSIF EqualArray(s, '-version')
            THEN
               DisplayVersion
            ELSIF NOT IsAnOption(s)
            THEN
               (*
                  do nothing - note that we may have set some flags by
                  calling IsAnOption.
               *)
            END
         END ;
         INC(i)
      UNTIL i>n
   END
END ParseOptions ;


(*
   ScanForInitialOptions - parses the options and sets any initial
                           options immediately.
*)

PROCEDURE ScanForInitialOptions ;
VAR
   s: String ;
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE GetArg(s, i) DO
      IF IsAnOption(s)
      THEN
      END ;
      INC(i)
   END
END ScanForInitialOptions ;


(*
   IsAnOption - returns TRUE if the option, s, was legal for gm2.
*)

PROCEDURE IsAnOption (s: String) : BOOLEAN ;
VAR
   Legal: BOOLEAN ;
BEGIN
   IF EqualArray(s, '-g')
   THEN
      GenerateDebugging := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wd')
   THEN
      CompilerDebugging := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-O') OR EqualArray(s, '-O1') OR EqualArray(s, '-O2') OR
         EqualArray(s, '-O3')
   THEN
      Optimizing := TRUE ;
      OptimizeUncalledProcedures := TRUE ;
      OptimizeBasicBlock := TRUE ;
      OptimizeCommonSubExpressions := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Obb')
   THEN
      OptimizeBasicBlock := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Ouncalled')
   THEN
      OptimizeUncalledProcedures := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Ocse')
   THEN
      OptimizeCommonSubExpressions := FALSE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wbounds')
   THEN
      BoundsChecking := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wcpp')
   THEN
      Legal := TRUE ;
      LineDirectives := TRUE
   ELSIF EqualArray(s, '-Wreturn')
   THEN
      ReturnChecking := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wstatistics')
   THEN
      Statistics := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wstudents')
   THEN
      StudentChecking := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wpedantic')
   THEN
      Pedantic := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-verbose')
   THEN
      Verbose := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-quiet')
   THEN
      Quiet := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wq')
   THEN
      DisplayQuadruples := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-dumpbase')
   THEN
      (* gcc passes this to us, we ignore it *)
      Legal := TRUE
   ELSE
      Legal := FALSE
   END ;
   RETURN( Legal )
END IsAnOption ;


BEGIN
   Statistics                   := FALSE ;
   StudentChecking              := FALSE ;
   CompilerDebugging            := FALSE ;
   GenerateDebugging            := FALSE ;
   Optimizing                   := FALSE ;
   Pedantic                     := FALSE ;
   Verbose                      := FALSE ;
   Quiet                        := FALSE ;
   Profiling                    := FALSE ;
   DisplayQuadruples            := FALSE ;
   OptimizeBasicBlock           := FALSE ;
   OptimizeUncalledProcedures   := FALSE ;
   OptimizeCommonSubExpressions := FALSE ;
   BoundsChecking               := FALSE ;
   ReturnChecking               := FALSE ;
   LineDirectives               := FALSE ;
   ScanForInitialOptions
END M2Options.
