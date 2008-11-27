(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE M2Options ;


IMPORT CmdArgs ;
FROM SArgs IMPORT GetArg, Narg ;
FROM M2Search IMPORT PrependSearchPath, SetDefExtension, SetModExtension ;
FROM M2Version IMPORT GetGM2Version, GetGM2Date, GetGCCVersion, GetYear ;
FROM M2Printf IMPORT printf0, printf1 ;
FROM libc IMPORT exit ;
FROM Debug IMPORT Halt ;

FROM DynamicStrings IMPORT String, Length, InitString, Mark, Slice, EqualArray,
                           InitStringCharStar, ConCatChar, ConCat ;


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
   s := Mark(GetGM2Version()) ;
   printf1('GNU Modula-2  %s', s) ;
   s := Mark(GetGM2Date()) ;
   printf1('  (%s)\n', s) ;
   s := Mark(GetGCCVersion()) ;
   printf1('  grafted onto GCC %s\n', s) ;
   s := Mark(GetYear()) ;
   printf1('Copyright (C) %s Free Software Foundation, Inc.\n', s) ;
   printf0('License GPLv2: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>\n') ;
   printf0('This is free software: you are free to change and redistribute it.\n') ;
   printf0('There is NO WARRANTY, to the extent permitted by law.\n') ;
   exit(0)
END DisplayVersion ;


(*
   ScanCppArgs - scans the cpp arguments and builds up the cpp command line.
*)

PROCEDURE ScanCppArgs (i: CARDINAL) : CARDINAL ;
VAR
   s: String ;
BEGIN
   IF GetArg(s, i) AND EqualArray(s, '-fcppbegin')
   THEN
      INC(i) ;
      WHILE GetArg(s, i) DO
         IF EqualArray(s, '-fcppend')
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
            ELSIF EqualArray(Mark(Slice(s, 0, 6)), '-fdef=')
            THEN
               SetDefExtension(Slice(s, 6, 0))
            ELSIF EqualArray(Mark(Slice(s, 0, 6)), '-fmod=')
            THEN
               SetModExtension(Slice(s, 6, 0))
            ELSIF EqualArray(s, '-fcppbegin')
            THEN
               i := ScanCppArgs(i)
            ELSIF EqualArray(s, '-fversion') OR EqualArray(s, '--version') OR
                  EqualArray(s, '-fgm2-version')
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
   SetReturnCheck -
*)

PROCEDURE SetReturnCheck (value: BOOLEAN) : BOOLEAN ;
BEGIN
   ReturnChecking := value ;
   RETURN( TRUE )
END SetReturnCheck ;


(*
   SetNilCheck -
*)

PROCEDURE SetNilCheck (value: BOOLEAN) : BOOLEAN ;
BEGIN
   NilChecking := value ;
   RETURN( TRUE )
END SetNilCheck ;


(*
   SetCaseCheck - set else case checking to, value.
*)

PROCEDURE SetCaseCheck (value: BOOLEAN) : BOOLEAN ;
BEGIN
   CaseElseChecking := value ;
   RETURN( TRUE )
END SetCaseCheck ;


(*
   SetCheckAll - set all runtime checking to, value.
*)

PROCEDURE SetCheckAll (value: BOOLEAN) : BOOLEAN ;
BEGIN
   BoundsChecking := value ;
   ReturnChecking := value ;
   NilChecking := value ;
   CaseElseChecking := value ;
   DivModRemChecking := value ;
   RETURN( TRUE )
END SetCheckAll ;


(*
   SetVerboseUnbounded - sets the VerboseUnbounded flag to, value.
*)

PROCEDURE SetVerboseUnbounded (value: BOOLEAN) : BOOLEAN ;
BEGIN
   VerboseUnbounded := value ;
   RETURN( TRUE )
END SetVerboseUnbounded ;


(*
   SetQuiet - sets the quiet flag to, value.
*)

PROCEDURE SetQuiet (value: BOOLEAN) : BOOLEAN ;
BEGIN
   Quiet := value ;
   RETURN( TRUE )
END SetQuiet ;


(*
   SetCpp -
*)

PROCEDURE SetCpp (value: BOOLEAN) : BOOLEAN ;
BEGIN
   CPreProcessor := value ;
   LineDirectives := value ;
   RETURN( TRUE )
END SetCpp ;


(*
   SetMakeall -
*)

PROCEDURE SetMakeall (value: BOOLEAN) : BOOLEAN ;
BEGIN
   (* value is unused *)
   RETURN( TRUE )
END SetMakeall ;


(*
   SetMakeall0 -
*)

PROCEDURE SetMakeall0 (value: BOOLEAN) : BOOLEAN ;
BEGIN
   (* value is unused *)
   RETURN( TRUE )
END SetMakeall0 ;


(*
   SetIncludePath -
*)

PROCEDURE SetIncludePath (arg: ADDRESS) : BOOLEAN ;
BEGIN
   RETURN( TRUE )
END SetIncludePath ;


(*
   SetUnboundedByReference -
*)

PROCEDURE SetUnboundedByReference (value: BOOLEAN) : BOOLEAN ;
BEGIN
   UnboundedByReference := value ;
   RETURN( TRUE )
END SetUnboundedByReference ;


(*
   IsAnOption - returns TRUE if the option, s, was legal for gm2.
*)

PROCEDURE IsAnOption (s: String) : BOOLEAN ;
VAR
   Legal: BOOLEAN ;
BEGIN
   IF EqualArray(Mark(Slice(s, 0, 2)), '-g')
   THEN
      GenerateDebugging := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(Mark(Slice(s, 0, 2)), '-p')
   THEN
      (* profiling *)
      Legal := TRUE
   ELSIF EqualArray(s, '-fiso')
   THEN
      Iso := TRUE ;
      Pim := FALSE ;
      Pim2 := FALSE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fpim')
   THEN
      Pim := TRUE ;
      Iso := FALSE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fpim2')
   THEN
      Pim := TRUE ;
      Pim2 := TRUE ;
      Iso := FALSE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fpim3')
   THEN
      Pim := TRUE ;
      Pim3 := TRUE ;
      Iso := FALSE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fpim4')
   THEN
      Pim := TRUE ;
      Pim4 := TRUE ;
      Iso := FALSE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fpositive-mod-floor-div')
   THEN
      PositiveModFloorDiv := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(Mark(Slice(s, 0, 7)), '-flibs=')
   THEN
      (* at present this switch just modifies the default library
         search path *)
      Legal := TRUE
   ELSIF EqualArray(s, '-fd')
   THEN
      CompilerDebugging := TRUE ;
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
   ELSIF EqualArray(Mark(Slice(s, 0, 2)), '-O')
   THEN
      Optimizing := TRUE ;
      OptimizeUncalledProcedures := TRUE ;
      OptimizeBasicBlock := TRUE ;
      OptimizeCommonSubExpressions := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-funbounded-by-reference')
   THEN
      Legal := SetUnboundedByReference(TRUE)
   ELSIF EqualArray(s, '-Wverbose-unbounded')
   THEN
      Legal := SetVerboseUnbounded(TRUE)
   ELSIF EqualArray(s, '-Wpedantic-param-names')
   THEN
      PedanticParamNames := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wpedantic-cast')
   THEN
      PedanticCast := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fcheck-all')
   THEN
      Legal := SetCheckAll(TRUE)
   ELSIF EqualArray(s, '-fbounds')
   THEN
      BoundsChecking := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fnil')
   THEN
      NilChecking := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fcase')
   THEN
      CaseElseChecking := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fdiv-mod-rem')
   THEN
      DivModRemChecking := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fcpp') OR EqualArray(s, '-fcppbegin')
   THEN
      Legal := SetCpp(TRUE)
   ELSIF EqualArray(s, '-freturn')
   THEN
      ReturnChecking := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fstatistics')
   THEN
      Statistics := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fxcode')
   THEN
      Xcode := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fextended-opaque')
   THEN
      ExtendedOpaque := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fdump-system-exports')
   THEN
      DumpSystemExports := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fswig')
   THEN
      GenerateSwig := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-verbose') OR EqualArray(s, '-v')
   THEN
      Verbose := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-fsources')
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
   ELSIF EqualArray(Mark(Slice(s, 0, 12)), '-ftarget-ar=')
   THEN
      Legal := TRUE
   ELSIF EqualArray(Mark(Slice(s, 0, 16)), '-ftarget-ranlib=')
   THEN
      Legal := TRUE
   ELSIF EqualArray(s, '-fq')
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
   ELSIF EqualArray(s, '-fmakeall') OR EqualArray(s, '-fmakeall0') OR
         EqualArray(Mark(Slice(s, 0, 9)), '-fmake-I=')
   THEN
      Legal := TRUE
   ELSIF EqualArray(Mark(Slice(s, 0, 6)), '-fdef=') OR
         EqualArray(Mark(Slice(s, 0, 6)), '-fmod=')
   THEN
      Legal := TRUE
   ELSIF EqualArray(s, '-Wstudents')
   THEN
      StudentChecking := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(s, '-Wpedantic')
   THEN
      Pedantic := TRUE ;
      Legal := TRUE
   ELSIF EqualArray(Mark(Slice(s, 0, 2)), '-m')
   THEN
      (* if gcc passes architectural flags to us, we ignore it *)
      Legal := TRUE
   ELSIF EqualArray(Mark(Slice(s, 0, 2)), '-d')
   THEN
      (* if gcc passes -d flags to us, we ignore them *)
      Legal := TRUE
   ELSIF EqualArray(Mark(Slice(s, 0, 2)), '-f')
   THEN
      (* if gcc passes architectural flags to us, we ignore it *)
      Legal := TRUE
   ELSIF EqualArray(s, '--version')
   THEN
      (* gcc passes this to us, we handle it *)
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
   Pim                          :=  TRUE ;
   Pim2                         := FALSE ;
   Pim3                         := FALSE ;
   Pim4                         :=  TRUE ;
   PositiveModFloorDiv          := FALSE ;
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
   DivModRemChecking            := FALSE ;
   CPreProcessor                := FALSE ;
   LineDirectives               := FALSE ;
   ExtendedOpaque               := FALSE ;
   UnboundedByReference         := FALSE ;
   VerboseUnbounded             := FALSE ;
   PedanticParamNames           := FALSE ;
   PedanticCast                 := FALSE ;
   Xcode                        := FALSE ;
   DumpSystemExports            := FALSE ;
   GenerateSwig                 := FALSE ;
   ScanForInitialOptions
END M2Options.
