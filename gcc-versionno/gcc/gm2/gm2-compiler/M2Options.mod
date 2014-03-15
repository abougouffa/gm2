(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                 2010, 2011
                 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
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
FROM m2linemap IMPORT location_t ;

FROM DynamicStrings IMPORT String, Length, InitString, Mark, Slice, EqualArray,
                           InitStringCharStar, ConCatChar, ConCat, KillString,
                           Dup,
                           PushAllocation, PopAllocationExemption,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;

(*
#define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#define Dup(X) DupDB(X, __FILE__, __LINE__)
#define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
*)

CONST
   Debugging = FALSE ;

VAR
   CppProgram,
   CppArgs            : String ;
   CC1Quiet,
   SeenSources        : BOOLEAN ;
   ForcedLocationValue: location_t ;


(*
   doDSdbEnter - 
*)

PROCEDURE doDSdbEnter ;
BEGIN
   PushAllocation
END doDSdbEnter ;


(*
   doDSdbExit - 
*)

PROCEDURE doDSdbExit (s: String) ;
BEGIN
   s := PopAllocationExemption(TRUE, s)
END doDSdbExit ;


(*
   DSdbEnter - 
*)

PROCEDURE DSdbEnter ;
BEGIN
END DSdbEnter ;


(*
   DSdbExit - 
*)

PROCEDURE DSdbExit (s: String) ;
BEGIN
END DSdbExit ;


(*
#define DSdbEnter doDSdbEnter
#define DSdbExit  doDSdbExit
*)


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
   CppCommandLine - returns the Cpp command line and all arguments.
*)

PROCEDURE CppCommandLine () : String ;
VAR
   s: String ;
BEGIN
   IF EqualArray(CppProgram, '')
   THEN
      RETURN( NIL )
   ELSE
      s := Dup(CppProgram) ;
      s := ConCat(ConCatChar(s, ' '), CppArgs) ;
      IF CC1Quiet
      THEN
         s := ConCat(ConCatChar(s, ' '), Mark(InitString('-quiet')))
      END ;
      RETURN( s )
   END
END CppCommandLine ;


(*
   CppProg - sets the cpp program to be, program.
*)

PROCEDURE CppProg (program: ADDRESS) ;
BEGIN
   CppProgram := KillString(CppProgram) ;
   CppProgram := InitStringCharStar(program)
END CppProg ;


(*
   CppArg - sets the option and arg in the cpp command line.
*)

PROCEDURE CppArg (opt, arg: ADDRESS; joined: BOOLEAN) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar(opt) ;
   IF EqualArray(s, '-fcppbegin') OR EqualArray(s, '-fcppend')
   THEN
      (* do nothing *)
   ELSIF EqualArray(s, '-fcppprog=')
   THEN
      CppProgram := KillString(CppProgram) ;
      CppProgram := InitStringCharStar(arg)
   ELSE
      IF NOT EqualArray(CppArgs, '')
      THEN
         CppArgs := ConCatChar(CppArgs, ' ')
      END ;
      CppArgs := ConCat(CppArgs, Mark(s)) ;
      IF arg#NIL
      THEN
         s := InitStringCharStar(arg) ;
         IF NOT joined
         THEN
            CppArgs := ConCatChar(CppArgs, ' ')
         END ;
         CppArgs := ConCat(CppArgs, s)
      END
   END
END CppArg ;


(*
   FinaliseOptions - once all options have been parsed we set any inferred
                     values.
*)

PROCEDURE FinaliseOptions ;
BEGIN
   (* currently only one value, this could be make an option in the future *)
   VariantValueChecking := Iso
END FinaliseOptions ;


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
   NilChecking := value ;
   WholeDivChecking := value ;
   WholeValueChecking := value ;
   IndexChecking := value ;
   RangeChecking := value ;
   ReturnChecking := value ;
   NilChecking := value ;
   CaseElseChecking := value ;
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
   SetDebugging - sets the debugging flag to, v.
*)

PROCEDURE SetDebugging (value: BOOLEAN) ;
BEGIN
   GenerateDebugging := value
END SetDebugging ;


(*
   SetProfiling - dummy procedure, as profiling is implemented in the gcc backend.
*)

PROCEDURE SetProfiling (value: BOOLEAN) ;
BEGIN
   (* nothing to do *)
END SetProfiling ;


(*
   SetISO - 
*)

PROCEDURE SetISO (value: BOOLEAN) ;
BEGIN
   Iso := value ;
   Pim := NOT value ;
   Pim2 := NOT value
END SetISO ;


(*
   SetPIM - 
*)

PROCEDURE SetPIM (value: BOOLEAN) ;
BEGIN
   Pim := value ;
   Iso := NOT value
END SetPIM ;


(*
   SetPIM2 - 
*)

PROCEDURE SetPIM2 (value: BOOLEAN) ;
BEGIN
   Pim := value ;
   Pim2 := value ;
   Iso := NOT value
END SetPIM2 ;


(*
   SetPIM3 - 
*)

PROCEDURE SetPIM3 (value: BOOLEAN) ;
BEGIN
   Pim := value ;
   Pim3 := value ;
   Iso := NOT value
END SetPIM3 ;


(*
   SetPIM4 - 
*)

PROCEDURE SetPIM4 (value: BOOLEAN) ;
BEGIN
   Pim := value ;
   Pim4 := value ;
   Iso := NOT value
END SetPIM4 ;


(*
   SetPositiveModFloor - sets the positive mod floor option.
*)

PROCEDURE SetPositiveModFloor (value: BOOLEAN) ;
BEGIN
   PositiveModFloorDiv := value
END SetPositiveModFloor ;


(*
   SetWholeDiv - sets the whole division flag.
*)

PROCEDURE SetWholeDiv (value: BOOLEAN) ;
BEGIN
   WholeDivChecking := value
END SetWholeDiv ;


(*
   SetIndex - sets the runtime array index checking flag.
*)

PROCEDURE SetIndex (value: BOOLEAN) ;
BEGIN
   IndexChecking := value
END SetIndex ;


(*
   SetRange -  sets the runtime range checking flag.
*)

PROCEDURE SetRange (value: BOOLEAN) ;
BEGIN
   RangeChecking := value
END SetRange ;


(*
   SetExceptions - sets the enable runtime exceptions flag.
*)

PROCEDURE SetExceptions (value: BOOLEAN) ;
BEGIN
   Exceptions := value
END SetExceptions ;


(*
   SetStudents - 
*)

PROCEDURE SetStudents (value: BOOLEAN) ;
BEGIN
   StudentChecking := value
END SetStudents ;


(*
   SetPedantic - 
*)

PROCEDURE SetPedantic (value: BOOLEAN) ;
BEGIN
   Pedantic := value
END SetPedantic ;


(*
   SetPedanticParamNames - sets the pedantic parameter name flag.
*)

PROCEDURE SetPedanticParamNames (value: BOOLEAN) ;
BEGIN
   PedanticParamNames := value
END SetPedanticParamNames ;


(*
   SetPedanticCast - sets the pedantic cast flag.
*)

PROCEDURE SetPedanticCast (value: BOOLEAN) ;
BEGIN
   PedanticCast := value
END SetPedanticCast ;


(*
   SetExtendedOpaque - sets the ExtendedOpaque flag.
*)

PROCEDURE SetExtendedOpaque (value: BOOLEAN) ;
BEGIN
   ExtendedOpaque := value
END SetExtendedOpaque ;


(*
   SetXCode - sets the xcode flag.
*)

PROCEDURE SetXCode (value: BOOLEAN) ;
BEGIN
   Xcode := value
END SetXCode ;


(*
   SetSwig - 
*)

PROCEDURE SetSwig (value: BOOLEAN) ;
BEGIN
   GenerateSwig := value
END SetSwig ;


(*
   SetQuadDebugging - display the quadruples (internal debugging).
*)

PROCEDURE SetQuadDebugging (value: BOOLEAN) ;
BEGIN
   DisplayQuadruples := value
END SetQuadDebugging ;


(*
   SetCompilerDebugging - turn on internal compiler debugging.
*)

PROCEDURE SetCompilerDebugging (value: BOOLEAN) ;
BEGIN
   CompilerDebugging := value
END SetCompilerDebugging ;


(*
   SetSources - 
*)

PROCEDURE SetSources (value: BOOLEAN) ;
BEGIN
   Quiet := NOT value ;
   SeenSources := value
END SetSources ;


(*
   SetDumpSystemExports - 
*)

PROCEDURE SetDumpSystemExports (value: BOOLEAN) ;
BEGIN
   DumpSystemExports := value
END SetDumpSystemExports ;


(*
   SetSearchPath -
*)

PROCEDURE SetSearchPath (arg: ADDRESS) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar(arg) ;
   IF Debugging
   THEN
      printf1("setting search path to: %s\n", s)
   END ;
   PrependSearchPath(s) ;
   s := KillString(s)
END SetSearchPath ;


(*
   setdefextension - 
*)

PROCEDURE setdefextension (arg: ADDRESS) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar(arg) ;
   SetDefExtension(s) ;
   s := KillString(s)
END setdefextension ;


(*
   setmodextension - 
*)

PROCEDURE setmodextension (arg: ADDRESS) ;
VAR
   s: String ;
BEGIN
   s := InitStringCharStar(arg) ;
   SetModExtension(s) ;
   s := KillString(s)
END setmodextension ;


(*
   SetOptimizing - 
*)

PROCEDURE SetOptimizing (value: CARDINAL) ;
BEGIN
   IF value>0
   THEN
      Optimizing := TRUE ;
      OptimizeBasicBlock := TRUE ;
      OptimizeUncalledProcedures := TRUE ;
      OptimizeCommonSubExpressions := TRUE
   ELSE
      Optimizing := FALSE ;
      OptimizeBasicBlock := FALSE ;
      OptimizeUncalledProcedures := FALSE ;
      OptimizeCommonSubExpressions := FALSE
   END
END SetOptimizing ;


(*
   SetForcedLocation - sets the location for the lifetime of this compile to, location.
                       This is primarily an internal debugging switch.
*)

PROCEDURE SetForcedLocation (location: location_t) ;
BEGIN
   ForcedLocation := TRUE ;
   ForcedLocationValue := location
END SetForcedLocation ;


(*
   SetCC1Quiet - sets the cc1quiet flag to, value.
*)

PROCEDURE SetCC1Quiet (value: BOOLEAN) ;
BEGIN
   CC1Quiet := value
END SetCC1Quiet ;


(*
   OverrideLocation - possibly override the location value, depending upon
                      whether the -flocation= option was used.
*)

PROCEDURE OverrideLocation (location: location_t) : location_t ;
BEGIN
   IF ForcedLocation
   THEN
      RETURN( ForcedLocationValue )
   ELSE
      RETURN( location )
   END
END OverrideLocation ;


BEGIN
   CppArgs                      := InitString('') ;
   CppProgram                   := InitString('') ;
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
   Quiet                        :=  TRUE ;
   CC1Quiet                     :=  TRUE ;
   Profiling                    := FALSE ;
   DisplayQuadruples            := FALSE ;
   OptimizeBasicBlock           := FALSE ;
   OptimizeUncalledProcedures   := FALSE ;
   OptimizeCommonSubExpressions := FALSE ;
   NilChecking                  := FALSE ;
   WholeDivChecking             := FALSE ;
   WholeValueChecking           := FALSE ;
   IndexChecking                := FALSE ;
   RangeChecking                := FALSE ;
   ReturnChecking               := FALSE ;
   CaseElseChecking             := FALSE ;
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
   Exceptions                   :=  TRUE ;
   DebugBuiltins                := FALSE ;
   ForcedLocation               := FALSE
END M2Options.
