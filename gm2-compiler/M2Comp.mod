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

IMPLEMENTATION MODULE M2Comp ;


FROM SYSTEM IMPORT ADDRESS ;
FROM M2Options IMPORT Statistics, Quiet ;

FROM M2Pass IMPORT SetPassToPass1, SetPassToPass2, SetPassToPassC, SetPassToPass3,
                   SetPassToNoPass, SetPassToPassHidden ;

FROM M2Reserved IMPORT toktype ;
FROM M2Search IMPORT FindSourceDefFile, FindSourceModFile ;
FROM M2Code IMPORT Code ;
FROM M2LexBuf IMPORT OpenSource, CloseSource, ResetForNewPass, currenttoken, GetToken, ReInitialize, currentstring ;
FROM M2FileName IMPORT CalculateFileName ;
FROM M2Preprocess IMPORT PreprocessModule ;
FROM libc IMPORT exit ;

FROM M2Error IMPORT ErrorStringAt, ErrorStringAt2, ErrorStringsAt2, WriteFormat0, FlushErrors, FlushWarnings ;
FROM FormatStrings IMPORT Sprintf1 ;

IMPORT m2flex ;
IMPORT P1SyntaxCheck ;
IMPORT P2Build ;
IMPORT PCBuild ;
IMPORT P3Build ;
IMPORT PHBuild ;
IMPORT P2SymBuild ;

FROM M2Batch IMPORT GetSource, GetModuleNo, GetDefinitionModuleFile, GetModuleFile,
                    AssociateModule, AssociateDefinition, MakeImplementationSource,
                    MakeProgramSource ;

FROM SymbolTable IMPORT GetSymName, IsDefImp, NulSym,
                        IsHiddenTypeDeclared, GetFirstUsed, GetMainModule, SetMainModule,
                        ResolveConstructorTypes ;

FROM FIO IMPORT StdErr ;
FROM NameKey IMPORT Name, GetKey, KeyToCharStar, makekey ;
FROM M2Printf IMPORT fprintf1 ;
FROM M2Quiet IMPORT qprintf0, qprintf1, qprintf2 ;
FROM DynamicStrings IMPORT String, InitString, KillString, InitStringCharStar, Dup, Mark, string ;

CONST
   Debugging = FALSE ;

VAR
   ModuleType : (None, Definition, Implementation, Program) ;


(* %%%FORWARD%%%
PROCEDURE DoPass1 (s: String) ; FORWARD ;
PROCEDURE DoPass2 ; FORWARD ;
PROCEDURE DoPass3 ; FORWARD ;
PROCEDURE DoPassC ; FORWARD ;
   %%%FORWARD%%% *)

(*
   CompilingDefinitionModule - returns true if the current module being
                               compiled is a definition module.
*)

PROCEDURE CompilingDefinitionModule() : BOOLEAN ;
BEGIN
   RETURN( ModuleType=Definition )
END CompilingDefinitionModule ;


(*
   CompilingImplementationModule - returns true if the current module being
                                   compiled is an implementation module.
*)

PROCEDURE CompilingImplementationModule() : BOOLEAN ;
BEGIN
   RETURN( ModuleType=Implementation )
END CompilingImplementationModule ;


(*
   CompilingProgramModule - returns true if the current module being
                            compiled is a program module.
*)

PROCEDURE CompilingProgramModule() : BOOLEAN ;
BEGIN
   RETURN( ModuleType=Program )
END CompilingProgramModule ;


(*
   Compile - compile file, s, using a 4 pass technique.
*)

PROCEDURE Compile (s: String) ;
BEGIN
   DoPass1(s) ;
   FlushWarnings ; FlushErrors ; 
   qprintf0('Pass 2\n') ;
   ResetForNewPass ;
   DoPass2 ;
   FlushWarnings ; FlushErrors ; 
   qprintf0('Pass C\n') ;
   ResetForNewPass ;
   DoPassC ;
   FlushWarnings ; FlushErrors ; 
   qprintf0('Pass 3\n') ;
   ResetForNewPass ;
   DoPass3 ;
   FlushWarnings ; FlushErrors ; 
   qprintf0('Pass 4\n') ;
   Code ;
   FlushWarnings ; FlushErrors
END Compile ;


(*
   ExamineCompilationUnit - opens the source file to obtain the module name and kind of module.
*)

PROCEDURE ExamineCompilationUnit (VAR name: ADDRESS; VAR isdefimp: BOOLEAN) ;
BEGIN
   isdefimp := FALSE ;   (* default to program module *)
   (* stop if we see eof, ';' or '[' *)
   WHILE (currenttoken#eoftok) AND (currenttoken#semicolontok) AND (currenttoken#lsbratok) DO
      IF (currenttoken=implementationtok) OR (currenttoken=definitiontok)
      THEN
         isdefimp := TRUE ;
         GetToken
      END ;
      IF currenttoken=identtok
      THEN
         name := currentstring ;
         RETURN
      END ;
      GetToken
   END ;
   m2flex.M2Error(string(InitString('failed to find module name'))) ;
   exit(1)
END ExamineCompilationUnit ;


(*
   PeepInto - peeps into source, s, and initializes a definition/implementation or
              program module accordingly.
*)

PROCEDURE PeepInto (s: String) ;
VAR
   name    : ADDRESS ;
   isdefimp: BOOLEAN ;
BEGIN
   IF OpenSource(PreprocessModule(s))
   THEN
      ExamineCompilationUnit(name, isdefimp) ;
      IF isdefimp
      THEN
         SetMainModule(MakeImplementationSource(makekey(name)))
      ELSE
         SetMainModule(MakeProgramSource(makekey(name)))
      END ;
      CloseSource ;
      ReInitialize
   ELSE
      fprintf1(StdErr, 'failed to open %s\n', s) ;
      exit(1)
   END
END PeepInto ;


(*
   DoPass1 - 
*)

PROCEDURE DoPass1 (s: String) ;
VAR
   Main,
   Sym     : CARDINAL ;
   i       : CARDINAL ;
   ModName,
   SymName,
   FileName: String ;
BEGIN
   SetPassToPass1 ;
   PeepInto(s) ;
   Main := GetMainModule() ;
   i := 1 ;
   Sym := GetModuleNo(i) ;
   qprintf1('Compiling: %s\n', s) ;
   WHILE Sym#NulSym DO
      SymName := InitStringCharStar(KeyToCharStar(GetSymName(Sym))) ;
      IF IsDefImp(Sym)
      THEN
         IF FindSourceDefFile(SymName, FileName)
         THEN
            qprintf2('   Module %-20s : %s\n', SymName, FileName) ;
            ModuleType := Definition ;
            IF OpenSource(AssociateDefinition(PreprocessModule(FileName), Sym))
            THEN
               IF NOT P1SyntaxCheck.CompilationUnit()
               THEN
                  WriteFormat0('compilation failed') ;
                  CloseSource ;
                  RETURN
               END ;
               CloseSource
            ELSE
               fprintf1(StdErr, 'failed to open %s\n', FileName) ;
               exit(1)
            END
         ELSE
            fprintf1(StdErr, 'failed to find definition module %s.def\n', SymName) ;
            exit(1)
         END ;
         ModuleType := Implementation
      ELSE
         ModuleType := Program
      END ;
      IF (Main=Sym) OR (IsDefImp(Sym) AND IsHiddenTypeDeclared(Sym))
      THEN
         (* only need to read implementation module if hidden types are declared or it is the main module *)
         IF Main=Sym
         THEN
            FileName := Dup(s)
         ELSE
            IF FindSourceModFile(SymName, FileName)
            THEN
            END
         END ;
         qprintf2('   Module %-20s : %s\n', SymName, FileName) ;
         IF OpenSource(AssociateModule(PreprocessModule(FileName), Sym))
         THEN
            IF NOT P1SyntaxCheck.CompilationUnit()
            THEN
               WriteFormat0('compilation failed') ;
               CloseSource ;
               RETURN
            END ;
            CloseSource
         ELSE
            ErrorStringAt(Sprintf1(InitString('file %s cannot be found'), FileName), GetFirstUsed(Sym)) ;
            fprintf1(StdErr, 'file %s cannot be opened\n', FileName)
         END
      END ;
      SymName := KillString(SymName) ;
      FileName := KillString(FileName) ;
      INC(i) ;
      Sym := GetModuleNo(i)
   END ;
   SetPassToNoPass
END DoPass1 ;


(*
   DoPass2 - parses the sources of all modules necessary to compile
             the required module, Main.
*)

PROCEDURE DoPass2 ;
VAR
   name    : Name ;
   Sym     : CARDINAL ;
   i,
   n       : CARDINAL ;
   FileName: String ;
BEGIN
   SetPassToPass2 ;
   i := 1 ;
   Sym := GetModuleNo(i) ;
   WHILE Sym#NulSym DO
      FileName := GetDefinitionModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF Debugging
         THEN
            name := GetSymName(Sym) ;
            qprintf1('   Module %a\n', name)
         END ;
         IF OpenSource(FileName)
         THEN
            ModuleType := Definition ;
            IF NOT P2Build.CompilationUnit()
            THEN
               WriteFormat0('compilation failed') ;
               CloseSource ;
               RETURN
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END ;
         ModuleType := Implementation
      ELSE
         ModuleType := Program
      END ;
      FileName := GetModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF Debugging
         THEN
            name := GetSymName(Sym) ;
            qprintf1('   Module %a\n', name)
         END ;
         IF OpenSource(FileName)
         THEN
            IF NOT P2Build.CompilationUnit()
            THEN
               WriteFormat0('compilation failed') ;
               CloseSource ;
               RETURN
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END
      END ;
      INC(i) ;
      Sym := GetModuleNo(i)
   END ;
   SetPassToNoPass
END DoPass2 ;


(*
   DoPassC - parses the sources of all modules necessary to compile
             the required module, Main.
*)

PROCEDURE DoPassC ;
VAR
   name    : Name ;
   Sym     : CARDINAL ;
   i,
   n       : CARDINAL ;
   FileName: String ;
BEGIN
   SetPassToPassC ;
   i := 1 ;
   Sym := GetModuleNo(i) ;
   WHILE Sym#NulSym DO
      FileName := GetDefinitionModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF Debugging
         THEN
            name := GetSymName(Sym) ;
            qprintf1('   Module %a\n', name)
         END ;
         IF OpenSource(FileName)
         THEN
            ModuleType := Definition ;
            IF NOT PCBuild.CompilationUnit()
            THEN
               WriteFormat0('compilation failed') ;
               CloseSource ;
               RETURN
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END ;
         ModuleType := Implementation
      ELSE
         ModuleType := Program
      END ;
      FileName := GetModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF Debugging
         THEN
            name := GetSymName(Sym) ;
            qprintf1('   Module %a\n', name)
         END ;
         IF OpenSource(FileName)
         THEN
            IF NOT PCBuild.CompilationUnit()
            THEN
               WriteFormat0('compilation failed') ;
               CloseSource ;
               RETURN
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END
      END ;
      INC(i) ;
      Sym := GetModuleNo(i)
   END ;
   P2SymBuild.ResolveConstTypes ;
   ResolveConstructorTypes ;
   SetPassToNoPass
END DoPassC ;


(*
   DoPass3 - parses the sources of all modules necessary to compile
             the required module, Main.
*)

PROCEDURE DoPass3 ;
VAR
   Main,
   Sym : CARDINAL ;
   i,
   n   : CARDINAL ;
   Name,
   FileName: String ;
BEGIN
   SetPassToPass3 ;
   Main := GetMainModule() ;
   i := 1 ;
   Sym := GetModuleNo(i) ;
   WHILE Sym#NulSym DO
      FileName := GetDefinitionModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF OpenSource(FileName)
         THEN
            ModuleType := Definition ;
            IF NOT P3Build.CompilationUnit()
            THEN
               WriteFormat0('compilation failed') ;
               CloseSource ;
               RETURN
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END ;
         ModuleType := Implementation
      ELSE
         ModuleType := Program
      END ;
      FileName := GetModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF OpenSource(FileName)
         THEN
            IF Main=Sym
            THEN
               IF NOT P3Build.CompilationUnit()
               THEN
                  WriteFormat0('compilation failed') ;
                  CloseSource ;
                  RETURN
               END
            ELSE
               (*
                  not the main module .mod therefore must be implementing
                  a hidden type - we dont want to generate any
                  StatementSequence quadrupes but we do want to build TYPEs
                  and ConstExpressions.
               *)
               SetPassToNoPass ;
               SetPassToPassHidden ;
               IF NOT PHBuild.CompilationUnit()
               THEN
                  WriteFormat0('compilation failed') ;
                  CloseSource ;
                  RETURN
               END ;
               SetPassToNoPass ;
               SetPassToPass3
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END
      END ;
      INC(i) ;
      Sym := GetModuleNo(i)
   END ;
   SetPassToNoPass
END DoPass3 ;


BEGIN
   ModuleType := None
END M2Comp.
