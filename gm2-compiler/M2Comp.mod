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
IMPLEMENTATION MODULE M2Comp ;


FROM M2Options IMPORT Statistics,
                      Recovery1, Recovery2, Recovery3, Quiet ;

FROM M2Pass IMPORT SetPassToPass1, SetPassToPass2, SetPassToPass3,
                   SetPassToNoPass, SetPassToPassHidden ;

FROM M2Search IMPORT FindSourceFile ;
FROM M2Code IMPORT Code ;
FROM M2Lexical IMPORT OpenSource, CloseSource, WriteError, WriteErrorFormat1, NearToken ;
FROM M2FileName IMPORT CalculateFileName ;
FROM libc IMPORT exit ;

IMPORT P1SyntaxCheck ;
IMPORT P2Build ;
IMPORT P3Build ;
IMPORT PHBuild ;
IMPORT P1Compile ;
IMPORT P2Compile ;
IMPORT P3Compile ;
IMPORT PHCompile ;

FROM M2Batch IMPORT GetSource, GetModuleNo ;

FROM SymbolTable IMPORT GetSymName, IsDefImp, NulSym,
                        IsHiddenTypeDeclared, GetFirstUsed ;

FROM NameKey IMPORT GetKey ;
FROM NumberIO IMPORT CardToStr ;
FROM StrLib IMPORT StrConCat ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT Write ;


CONST
   MaxStringLen = 8192 ;


VAR
   ModuleType : (None, Definition, Implementation, Program) ;


(* %%%FORWARD%%%
PROCEDURE DoPass1 (Main: CARDINAL; FullPath: ARRAY OF CHAR) ; FORWARD ;
PROCEDURE DoPass2 (Main: CARDINAL; FullPath: ARRAY OF CHAR) ; FORWARD ;
PROCEDURE DoPass3 (Main: CARDINAL; FullPath: ARRAY OF CHAR) ; FORWARD ;
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
   Compile - compiles the required modules by using a 3 pass compilation
             technique.
*)

PROCEDURE Compile (Sym: CARDINAL; FullPath: String) ;
BEGIN
   qprintf0('Pass 1\n') ;
   DoPass1(Sym, FullPath) ;
   qprintf0('Pass 2\n') ;
   DoPass2(Sym, FullPath) ;
   qprintf0('Pass 3\n') ;
   DoPass3(Sym, FullPath) ;
   qprintf0('Pass 4\n') ;
   Code
END Compile ;


(*
   OpenSourceOrIntemediate - 
*)

PROCEDURE OpenSourceOrIntemediate (Main, Sym: CARDINAL; Announce: BOOLEAN;
                                   Name, FullName: ARRAY OF CHAR) : BOOLEAN ;
VAR
   FileName: ARRAY [0..MaxStringLen] OF CHAR ;
BEGIN
   IF Main=Sym
   THEN
      (* potentially reading from a temporary file *)
      IF OpenSource(FullName, GetSymName(Sym))
      THEN
         IF Announce
         THEN
            QuietString('   Module ') ; QuietName(Name) ; QuietString(' : ') ;
            QuietString(Name) ; QuietLn
         END ;
         IF ModuleType=Definition
         THEN
            ModuleType := Implementation
         ELSE
            ModuleType := Program
         END ;
         RETURN( TRUE )
      ELSE
         RETURN( FALSE )
      END
   ELSE
      (* straight forward .mod or .def file (no preprocessing should have occurred in this file) *)
      CalculateFileName(Name, 'mod', FileName) ;
      IF OpenSource(FileName, GetSymName(Sym))
      THEN
         IF Announce
         THEN
            QuietString('   Module ') ; QuietName(Name) ; QuietString(' : ') ;
            IF FindSourceFile(FileName, FileName)
            THEN
               QuietString(FileName) ; QuietLn
            END
         END ;
         IF ModuleType=Definition
         THEN
            ModuleType := Implementation
         ELSE
            ModuleType := Program
         END ;
         RETURN( TRUE )
      ELSE
         RETURN( FALSE )
      END
   END
END OpenSourceOrIntemediate ;


(*
   DoPass1 - parses the sources of all modules necessary to compile
             the required module, Main.
*)

PROCEDURE DoPass1 (Main: CARDINAL; FullPath: ARRAY OF CHAR) ;
VAR
   Sym : CARDINAL ;
   n, i: CARDINAL ;
   Name,
   FileName: ARRAY [0..MaxStringLen] OF CHAR ;
BEGIN
   SetPassToPass1 ;
   i := 1 ;
   Sym := GetModuleNo(i) ;
   WHILE Sym#NulSym DO
      n := GetSymName(Sym) ;
      GetKey(n, Name) ;
      (*
         Attempt to open <Name>.def, cannot test IsDefImp because it may not
         be set yet.
      *)
      CalculateFileName(Name, 'def', FileName) ;
      IF OpenSource(FileName, n)
      THEN
         QuietString('   Module ') ; QuietName(Name) ; QuietString(' : ') ;
      	 IF FindSourceFile(FileName, FileName)
      	 THEN
            QuietString(FileName) ; QuietLn
      	 ELSE
      	    WriteError('inconsistancy between opening file and finding file')
      	 END ;
         ModuleType := Definition ;
         IF Recovery1
         THEN
            IF NOT P1SyntaxCheck.CompilationUnit()
            THEN
               WriteError('compilation failed')
            END
         ELSE
            IF NOT P1Compile.CompilationUnit()
            THEN
               WriteError('compilation failed')
            END
         END ;
         CloseSource
      ELSIF Sym#Main
      THEN
         NearToken('', GetFirstUsed(Sym)) ;
         WriteErrorFormat1('cannot find definition module %s.def', n)
      ELSE
         ModuleType := Program
      END ;
      IF (Main=Sym) OR (IsDefImp(Sym) AND IsHiddenTypeDeclared(Sym))
      THEN
         IF OpenSourceOrIntemediate(Main, Sym, TRUE, Name, FullPath)
         THEN
            IF Recovery1
            THEN
               IF NOT P1SyntaxCheck.CompilationUnit()
               THEN
                  WriteError('compilation failed')
               END
            ELSE
               IF NOT P1Compile.CompilationUnit()
               THEN
                  WriteError('compilation failed')
               END
            END ;
            CloseSource
         ELSE
            NearToken('', GetFirstUsed(Sym)) ;
            WriteErrorFormat1('module %s.mod cannot be found', n)
         END
      END ;
      INC(i) ;
      Sym := GetModuleNo(i)
   END ;
   SetPassToNoPass
END DoPass1 ;


(*
   DoPass2 - parses the sources of all modules necessary to compile
             the required module, Main.
*)

PROCEDURE DoPass2 (Main: CARDINAL; FullPath: ARRAY OF CHAR) ;
VAR
   Sym : CARDINAL ;
   i,
   n   : CARDINAL ;
   Name,
   FileName: ARRAY [0..MaxStringLen] OF CHAR ;
BEGIN
   SetPassToPass2 ;
   i := 1 ;
   Sym := GetModuleNo(i) ;
   WHILE Sym#NulSym DO
      n := GetSymName(Sym) ;
      GetKey(n, Name) ;
      IF IsDefImp(Sym)
      THEN
      	 CalculateFileName(Name, 'def', FileName) ;
         IF OpenSource(FileName, n)
         THEN
            ModuleType := Definition ;
            IF Recovery2
            THEN
               IF NOT P2Build.CompilationUnit()
               THEN
                  WriteError('compilation failed')
               END
            ELSE
               IF NOT P2Compile.CompilationUnit()
               THEN
                  WriteError('compilation failed')
               END
            END ;
            CloseSource
         ELSE
            NearToken('', GetFirstUsed(Sym)) ;
            WriteErrorFormat1('module %s.def cannot be found', n)
         END
      ELSE
         ModuleType := Program
      END ;
      IF (Main=Sym) OR (IsDefImp(Sym) AND IsHiddenTypeDeclared(Sym))
      THEN
         IF OpenSourceOrIntemediate(Main, Sym, FALSE, Name, FullPath)
         THEN
            IF Recovery2
            THEN
               IF NOT P2Build.CompilationUnit()
               THEN
                  WriteError('compilation failed')
               END
            ELSE
               IF NOT P2Compile.CompilationUnit()
               THEN
                  WriteError('compilation failed')
               END
            END ;
            CloseSource
         ELSE
            NearToken('', GetFirstUsed(Sym)) ;
            WriteErrorFormat1('module %s.mod cannot be found', n)
         END
      END ;
      INC(i) ;
      Sym := GetModuleNo(i)
   END ;
   SetPassToNoPass
END DoPass2 ;


(*
   DoPass3 - parses the sources of all modules necessary to compile
             the required module, Main.
*)

PROCEDURE DoPass3 (Main: CARDINAL; FullPath: ARRAY OF CHAR) ;
VAR
   Sym : CARDINAL ;
   i,
   n   : CARDINAL ;
   Name,
   FileName: ARRAY [0..MaxStringLen] OF CHAR ;
BEGIN
   SetPassToPass3 ;
   i := 1 ;
   Sym := GetModuleNo(i) ;
   WHILE Sym#NulSym DO
      n := GetSymName(Sym) ;
      GetKey(n, Name) ;
      IF IsDefImp(Sym)
      THEN
      	 CalculateFileName(Name, 'def', FileName) ;
         IF OpenSource(FileName, n)
         THEN
            ModuleType := Definition ;
            IF Recovery3
            THEN
               IF NOT P3Build.CompilationUnit()
               THEN
                  WriteError('compilation failed')
               END
            ELSE
               IF NOT P3Compile.CompilationUnit()
               THEN
                  WriteError('compilation failed')
               END
            END ;
            CloseSource
         ELSE
            NearToken('', GetFirstUsed(Sym)) ;
            WriteErrorFormat1('module %s.def cannot be found', n)
         END
      ELSE
         ModuleType := Program
      END ;
      IF (Main=Sym) OR (IsDefImp(Sym) AND IsHiddenTypeDeclared(Sym))
      THEN
         IF OpenSourceOrIntemediate(Main, Sym, FALSE, Name, FullPath)
         THEN
            IF Main=Sym
            THEN
               IF Recovery3
               THEN
                  IF NOT P3Build.CompilationUnit()
                  THEN
                     WriteError('compilation failed')
                  END
               ELSE
                  IF NOT P3Compile.CompilationUnit()
                  THEN
                     WriteError('compilation failed')
                  END
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
               IF Recovery3
               THEN
                  IF NOT PHBuild.CompilationUnit()
                  THEN
                     WriteError('compilation failed')
                  END
               ELSE
                  IF NOT PHCompile.CompilationUnit()
                  THEN
                     WriteError('compilation failed')
                  END
               END ;
               SetPassToNoPass ;
               SetPassToPass3
            END ;
            CloseSource
         ELSE
            NearToken('', GetFirstUsed(Sym)) ;
            WriteErrorFormat1('module %s.mod cannot be found', n)
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
