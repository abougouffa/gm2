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
FROM DynamicStrings IMPORT String, InitString, Mark, Slice, EqualArray, ConCatChar, ConCat ;
FROM M2Search IMPORT PrependSearchPath ;
FROM M2Version IMPORT WriteVersion ;
FROM M2Printf IMPORT printf0, printf1 ;
FROM libc IMPORT exit ;
FROM Debug IMPORT Halt ;


VAR
   CppAndArgs : String ;
   SeenSources: BOOLEAN ;


(*
   DisplayVersion - displays the version of the compiler.
*)

PROCEDURE DisplayVersion ;
VAR
   s: String ;
BEGIN
   printf0('gm2  [') ;
   WriteVersion ;
   s := Mark(InitString(__DATE__)) ;
   printf1('] [%s]\n', s)
END DisplayVersion ;


(*
   ScanCppArgs - scans the cpp arguments and builds up the cpp command line.
*)

PROCEDURE ScanCppArgs (i: CARDINAL) : CARDINAL ;
VAR
   s: String ;
BEGIN
   IF GetArg(s, i) AND EqualArray(s, '-Wcppbegin')
   THEN
      INC(i) ;
      WHILE GetArg(s, i) DO
         IF EqualArray(s, '-Wcppend')
         THEN
            RETURN( i )
         ELSIF NOT EqualArray(CppAndArgs, '')
         THEN
            CppAndArgs := ConCatChar(CppAndArgs, ' ')
         END ;
         CppAndArgs := ConCat(CppAndArgs, s) ;
         INC(i)
      END
   END ;
   RETURN( i )
END ScanCppArgs ;


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
            IF EqualArray(Mark(Slice(s, 0, 2)), '-I')
            THEN
               PrependSearchPath(Slice(s, 2, 0))
            ELSIF EqualArray(s, '-Wcppbegin')
            THEN
               i := ScanCppArgs(i)
            ELSIF EqualArray(s, '-version')
            THEN
               DisplayVersion
            ELSIF IsAnOptionAndArg(s)
            THEN
               INC(i)
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
   IF EqualArray(Mark(Slice(s, 0, 2)), '-f')
   THEN
      Legal := TRUE
   ELSIF EqualArray(Mark(Slice(s, 0, 2)), '-g')
   THEN
      GenerateDebugging := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wiso')
   THEN
      Iso := TRUE ;
      Pim := FALSE ;
      Pim2 := FALSE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wpim')
   THEN
      Pim := TRUE ;
      Iso := FALSE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wpim2')
   THEN
      Pim := TRUE ;
      Pim2 := TRUE ;
      Iso := FALSE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wulm')
   THEN
      (* at present this switch just modifies the default library
         search path *)
      Pim := TRUE ;
      Iso := FALSE ;
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
   ELSIF EqualArray(s, '-funbounded-by-reference')
   THEN
      UnboundedByReference := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wverbose-unbounded')
   THEN
      VerboseUnbounded := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wpedantic-param-names')
   THEN
      PedanticParamNames := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wcheck-all')
   THEN
      BoundsChecking := TRUE ;
      ReturnChecking := TRUE ;
      NilChecking := TRUE ;
      CaseElseChecking := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wbounds')
   THEN
      BoundsChecking := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wnil')
   THEN
      NilChecking := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wcase')
   THEN
      CaseElseChecking := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wcpp') OR EqualArray(s, '-Wcppbegin')
   THEN
      Legal := TRUE ;
      CPreProcessor := TRUE ;
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
   ELSIF EqualArray(s, '-Wextended-opaque')
   THEN
      ExtendedOpaque := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-verbose') OR EqualArray(s, '-v')
   THEN
      Verbose := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wsources')
   THEN
      Quiet := FALSE ;
      SeenSources := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-quiet')
   THEN
      IF NOT SeenSources
      THEN
         Quiet := TRUE    (* Quiet is automatically set by the front end *)
      END ;
      Legal := TRUE
   ELSIF EqualArray(Mark(Slice(s, 0, 12)), '-Wtarget-ar=')
   THEN
      Legal := TRUE
   ELSIF EqualArray(s, '-Wq')
   THEN
      DisplayQuadruples := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(Mark(Slice(s, 0, 2)), '-I')
   THEN
      Legal := TRUE
   ELSIF EqualArray(s, '-dumpbase')
   THEN
      (* gcc passes this to us, we ignore it *)
      Legal := TRUE
   ELSIF EqualArray(Mark(Slice(s, 0, 2)), '-m')
   THEN
      (* if gcc passes architectural flags to us, we ignore it *)
      Legal := TRUE
   ELSIF EqualArray(s, '-Wmakeall') OR EqualArray(s, '-Wmakeall0') OR
         EqualArray(Mark(Slice(s, 0, 9)), '-Wmake-I=')
   THEN
      Legal := TRUE
   ELSE
      Legal := FALSE
   END ;
   RETURN( Legal )
END IsAnOption ;


(*
   IsAnOptionAndArg - returns TRUE if argument, s, implies that the next argument
                      is associated with this argument.
*)

PROCEDURE IsAnOptionAndArg (s: String) : BOOLEAN ;
BEGIN
   RETURN(
          EqualArray(s, '-auxbase-strip') OR
          EqualArray(s, '-auxbase')
         )
END IsAnOptionAndArg ;


(*
   CppCommandLine - returns the Cpp command line and all arguments.
*)

PROCEDURE CppCommandLine () : String ;
BEGIN
   RETURN( CppAndArgs )
END CppCommandLine ;


BEGIN
   CppAndArgs                   := InitString('') ;
   Pim                          := TRUE ;
   Pim2                         := FALSE ;
   Iso                          := FALSE ;
   SeenSources                  := FALSE ;
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
   CPreProcessor                := FALSE ;
   LineDirectives               := FALSE ;
   ExtendedOpaque               := FALSE ;
   UnboundedByReference         := FALSE ;
   VerboseUnbounded             := FALSE ;
   PedanticParamNames           := FALSE ;
   ScanForInitialOptions
END M2Options.