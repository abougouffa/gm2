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
IMPLEMENTATION MODULE P1SymBuild ;


FROM ASCII IMPORT nul ;
FROM StrLib IMPORT StrLen, StrEqual ;
FROM NameKey IMPORT WriteKey, MakeKey, NulName ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM M2Debug IMPORT Assert, WriteDebug ;
FROM M2Lexical IMPORT WriteError, GetFileName, WriteErrorFormat1, WriteErrorFormat2 ;
FROM M2Error IMPORT BeginError, EndError ;
FROM M2Reserved IMPORT ImportTok, ExportTok, QualifiedTok, UnQualifiedTok,
                       NulTok, VarTok, ArrayTok ;

FROM FifoQueue IMPORT PutIntoFifoQueue ;

FROM SymbolTable IMPORT NulSym,
                        ModeOfAddr,
                        StartScope, EndScope, PseudoScope,
                        SetCurrentModule, SetFileModule,
                        MakeInnerModule,
                        MakeConstLit,
                        MakeConstLitString,
                        MakeEnumeration, MakeSubrange,
                        MakeVar, MakeType, PutType,
                        MakeHiddenType,
                        PutMode,
                        PutFieldEnumeration, PutSubrange, PutVar,
                        IsDefImp, IsType,
                        GetSym, RequestSym, IsUnknown, RenameSym,
                        GetFromOuterModule,
                        GetExported,
                        PutImported,
                        PutExported, PutExportQualified, PutExportUnQualified,
                        MakeProcedure,
                        PutFunction, PutParam, PutVarParam,
                        GetNthParam,
                        IsProcedure,
                        MakePointer, PutPointer,
                        MakeRecord, PutFieldRecord,
                        MakeArray, PutFieldArray,
                        MakeSubscript, PutSubscript,
                        PutArray, GetType, IsArray,
                        IsProcType, MakeProcType,
                        PutProcTypeVarParam, PutProcTypeParam,
                        MakeConstVar,
                        MakeUnbounded, PutUnbounded,
                        GetSymName ;

FROM M2Batch IMPORT MakeDefinitionSource,
                    MakeImplementationSource,
                    MakeProgramSource ;

FROM M2Quads IMPORT PushT, PopT,
                    PushTF, PopTF,
                    Operand, GetPtr, PutPtr ;

FROM M2Comp IMPORT CompilingDefinitionModule,
                   CompilingImplementationModule,
                   CompilingProgramModule ;

FROM M2Base IMPORT MixTypes ;


CONST
   MaxFileName = 4096 ;

VAR
   CheckProcedure: BOOLEAN ;  (* Set if currently implementing a defined *)
                              (* procedure.                              *)


(*
   ReduceToModule - creates the module name from the file name.
*)

PROCEDURE ReduceToModule (File: ARRAY OF CHAR; VAR Module: ARRAY OF CHAR) ;
VAR
   fi, mi,
   high  : CARDINAL ;
BEGIN
   fi := 0 ;
   mi := 0 ;
   high := StrLen(File) ;
   IF (high>3) AND (File[high-4]='.')
   THEN
      File[high-4] := nul
   END ;
   fi := StrLen(File)-1 ;
   WHILE (fi>0) AND (File[fi]#'/') DO
      DEC(fi)
   END ;
   IF File[fi]='/'
   THEN
      INC(fi)
   END ;
   high := StrLen(File) ;
   WHILE (fi<high) AND (mi<=HIGH(Module)) DO
      Module[mi] := File[fi] ;
      INC(mi) ;
      INC(fi)
   END ;
   IF mi<=HIGH(Module)
   THEN
      Module[mi] := nul
   END
END ReduceToModule ;


(*
   CheckName - checks to see that the module name matches the file name.
*)

PROCEDURE CheckFileName (Name: CARDINAL; ModuleType: ARRAY OF CHAR) ;
VAR
   FileName: ARRAY [0..MaxFileName] OF CHAR ;
BEGIN
   GetFileName(FileName) ;
   ReduceToModule(FileName, FileName) ;
   IF MakeKey(FileName)#Name
   THEN
      BeginError ;
      WriteString(ModuleType) ;
      WriteString(' module name does not match file name') ;
      WriteLn ;
      WriteErrorFormat1('inconsistant module and file name (%s)', Name) ;
      EndError
   END
END CheckFileName ;


(*
   StartBuildDefinitionModule - Creates a definition module and starts
                                a new scope.

                                he Stack is expected:

                                Entry                 Exit

                         Ptr ->                                     <- Ptr
                                +------------+        +-----------+
                                | NameStart  |        | NameStart |
                                |------------|        |-----------|

*)

PROCEDURE P1StartBuildDefinitionModule ;
VAR
   Name     : CARDINAL ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(Name) ;
   CheckFileName(Name, 'definition') ;
   ModuleSym := MakeDefinitionSource(Name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(IsDefImp(ModuleSym)) ;
   Assert(CompilingDefinitionModule()) ;
   PushT(Name)
END P1StartBuildDefinitionModule ;


(*
   EndBuildDefinitionModule - Destroys the definition module scope and
                              checks for correct name.

                              The Stack is expected:

                              Entry                 Exit

                       Ptr ->
                              +------------+        +-----------+
                              | NameEnd    |        |           |
                              |------------|        |-----------|
                              | NameStart  |        |           | <- Ptr
                              |------------|        |-----------|
*)

PROCEDURE P1EndBuildDefinitionModule ;
VAR
   NameStart,
   NameEnd  : CARDINAL ;
BEGIN                                 
   Assert(CompilingDefinitionModule()) ;
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF NameStart#NameEnd
   THEN
      WriteErrorFormat1('inconsistant definition module name %s', NameStart)
   END
END P1EndBuildDefinitionModule ;


(*
   StartBuildImplementationModule - Creates an implementation module and starts
                                    a new scope.

                                    The Stack is expected:

                                    Entry                 Exit

                             Ptr ->                                     <- Ptr
                                    +------------+        +-----------+
                                    | NameStart  |        | NameStart |
                                    |------------|        |-----------|

*)

PROCEDURE P1StartBuildImplementationModule ;
VAR
   Name     : CARDINAL ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(Name) ;
   CheckFileName(Name, 'implementation') ;
   ModuleSym := MakeImplementationSource(Name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   IF NOT IsDefImp(ModuleSym)
   THEN
      WriteErrorFormat1('cannot find corresponding definition module to %s', GetSymName(ModuleSym))
   END ;
   Assert(CompilingImplementationModule()) ;
   PushT(Name)                        
END P1StartBuildImplementationModule ;


(*
   EndBuildImplementationModule - Destroys the implementation module scope and
                                  checks for correct name.

                                  The Stack is expected:

                                  Entry                 Exit

                           Ptr ->
                                  +------------+        +-----------+
                                  | NameEnd    |        |           |
                                  |------------|        |-----------|
                                  | NameStart  |        |           | <- Ptr
                                  |------------|        |-----------|
*)

PROCEDURE P1EndBuildImplementationModule ;
VAR
   NameStart,
   NameEnd  : CARDINAL ;
BEGIN
   Assert(CompilingImplementationModule()) ;
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF NameStart#NameEnd
   THEN
      WriteErrorFormat1('inconsistant implementation module name %s', NameStart)
   END
END P1EndBuildImplementationModule ;


(*
   StartBuildProgramModule - Creates a program module and starts
                             a new scope.

                             The Stack is expected:

                             Entry                 Exit

                      Ptr ->                                     <- Ptr
                             +------------+        +-----------+
                             | NameStart  |        | NameStart |
                             |------------|        |-----------|

*)

PROCEDURE P1StartBuildProgramModule ;
VAR
   Name     : CARDINAL ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(Name) ;
   CheckFileName(Name, 'main') ;
   ModuleSym := MakeProgramSource(Name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(CompilingProgramModule()) ;
   Assert(NOT IsDefImp(ModuleSym)) ;
   PushT(Name)
END P1StartBuildProgramModule ;


(*
   EndBuildProgramModule - Destroys the program module scope and
                           checks for correct name.

                           The Stack is expected:

                           Entry                 Exit

                    Ptr ->
                           +------------+        +-----------+
                           | NameEnd    |        |           |
                           |------------|        |-----------|
                           | NameStart  |        |           | <- Ptr
                           |------------|        |-----------|
*)

PROCEDURE P1EndBuildProgramModule ;
VAR
   NameStart,
   NameEnd  : CARDINAL ;
BEGIN
   Assert(CompilingProgramModule()) ;
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF NameStart#NameEnd
   THEN
      WriteErrorFormat1('inconsistant program module name %s', NameStart)
   END
END P1EndBuildProgramModule ;


(*
   StartBuildInnerModule - Creates an Inner module and starts
                           a new scope.

                           The Stack is expected:

                           Entry                 Exit

                    Ptr ->                                     <- Ptr
                           +------------+        +-----------+
                           | NameStart  |        | NameStart |
                           |------------|        |-----------|

*)

PROCEDURE StartBuildInnerModule ;
VAR
   Name     : CARDINAL ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(Name) ;
   ModuleSym := MakeInnerModule(Name) ;
   StartScope(ModuleSym) ;
   Assert(NOT IsDefImp(ModuleSym)) ;
   PushT(Name)
END StartBuildInnerModule ;


(*
   EndBuildInnerModule - Destroys the Inner module scope and
                         checks for correct name.

                         The Stack is expected:

                         Entry                 Exit

                  Ptr ->
                         +------------+        +-----------+
                         | NameEnd    |        |           |
                         |------------|        |-----------|
                         | NameStart  |        |           | <- Ptr
                         |------------|        |-----------|
*)

PROCEDURE EndBuildInnerModule ;
VAR
   NameStart,
   NameEnd  : CARDINAL ;
BEGIN
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF NameStart#NameEnd
   THEN
      WriteErrorFormat1('inconsistant inner module name %s', NameStart)
   END
END EndBuildInnerModule ;


(*
   BuildImportOuterModule - Builds imported identifiers into an outer module
                            from a definition module.

                            The Stack is expected:

                            Entry           OR    Entry

                     Ptr ->                Ptr ->
                            +------------+        +-----------+
                            | #          |        | #         |
                            |------------|        |-----------|
                            | Id1        |        | Id1       |
                            |------------|        |-----------|
                            .            .        .           .
                            .            .        .           .
                            .            .        .           .
                            |------------|        |-----------|
                            | Id#        |        | Id#       |
                            |------------|        |-----------|
                            | ImportTok  |        | Ident     |
                            |------------|        |-----------|

                            IMPORT Id1, .. Id# ;  FROM Ident IMPORT Id1 .. Id# ;


                            Exit

                            All above stack discarded
*)

PROCEDURE BuildImportOuterModule ;
VAR
   Ptr, Top,
   Sym, ModSym,
   i, j       : CARDINAL ;
BEGIN
   GetPtr(Ptr) ;
   Top := Ptr-2 ;  (* Top = Id1                 *)
   PopT(i) ;       (* i   = # of the Ident List *)
   j := Top-i+1 ;  (* j   = Id#                 *)
   DEC(Ptr,i+2) ;  (* Ptr = ImportTok           *)
   IF Operand(Ptr)=ImportTok
   THEN
      (* Ident list contains Module Names *)
      WHILE j<=Top DO
         ModSym := MakeDefinitionSource(Operand(j)) ;
         PutImported(ModSym) ;
         INC(j)
      END
   ELSE
      (* Ident List contains list of objects *)
      ModSym := MakeDefinitionSource(Operand(Ptr)) ;
      WHILE j<=Top DO
(*
         WriteString('Importing ') ; WriteKey(Operand(j)) ; WriteString(' from ') ; WriteKey(GetSymName(ModSym)) ; WriteLn ;
*)
         Sym := GetExported(ModSym, Operand(j)) ;
         PutImported(Sym) ;
         INC(j)
      END
   END ;
   PutPtr(Ptr)   (* Clear Stack *)
END BuildImportOuterModule ;


(*
   BuildExportOuterModule - Builds exported identifiers from an outer module
                            to the outside world of library modules.

                            The Stack is expected:

                            Entry           OR    Entry

                     Ptr ->                Ptr ->
                            +------------+        +--------------+
                            | #          |        | #            |
                            |------------|        |--------------|
                            | Id1        |        | Id1          |
                            |------------|        |--------------|
                            .            .        .              .
                            .            .        .              .
                            .            .        .              .
                            |------------|        |--------------|
                            | Id#        |        | Id#          |
                            |------------|        |--------------|
                            | ExportTok  |        | QualifiedTok |
                            |------------|        |--------------|

                            EXPORT Id1, .. Id# ;  EXPORT QUALIFIED Id1 .. Id# ;

                            Error Condition


                            Exit

                            All above stack discarded
*)

PROCEDURE BuildExportOuterModule ;
VAR
   Ptr, Top,
   ModSym,
   i, j    : CARDINAL ;
BEGIN
   GetPtr(Ptr) ;
   Top := Ptr-2 ;  (* Top = Id1                 *)
   PopT(i) ;       (* i   = # of the Ident List *)
   j := Top-i+1 ;  (* j   = Id#                 *)
   DEC(Ptr,i+2) ;  (* Ptr = ExportTok           *)
   IF (Operand(Ptr)=QualifiedTok) AND CompilingDefinitionModule()
   THEN
      (* Ident List contains list of objects *)
      WHILE j<=Top DO
         PutExportQualified(Operand(j)) ;
         INC(j)
      END
   ELSIF (Operand(Ptr)=UnQualifiedTok) AND CompilingDefinitionModule()
   THEN
      (* Ident List contains list of objects *)
      WHILE j<=Top DO
         PutExportUnQualified(Operand(j)) ;
         INC(j)
      END
   ELSIF CompilingDefinitionModule()
   THEN
      WriteError('QUALIFIED export only allowed in a definition module')
   ELSE
      WriteError('only allowed inter module exports in definition module')
   END ;
   PutPtr(Ptr)   (* Clear Stack *)
END BuildExportOuterModule ;


(*
   BuildImportInnerModule - Builds imported identifiers into an inner module
                            from the last level of module.

                            The Stack is expected:

                            Entry           OR    Entry

                     Ptr ->                Ptr ->
                            +------------+        +-----------+
                            | #          |        | #         |
                            |------------|        |-----------|
                            | Id1        |        | Id1       |
                            |------------|        |-----------|
                            .            .        .           .
                            .            .        .           .
                            .            .        .           .
                            |------------|        |-----------|
                            | Id#        |        | Id#       |
                            |------------|        |-----------|
                            | ImportTok  |        | Ident     |
                            |------------|        |-----------|

                            IMPORT Id1, .. Id# ;  FROM Ident IMPORT Id1 .. Id# ;


                                                  Error Condition
                            Exit

                            All above stack discarded
*)

PROCEDURE BuildImportInnerModule ;
VAR
   Ptr, Top,
   Sym, ModSym,
   i, j       : CARDINAL ;
BEGIN
   GetPtr(Ptr) ;
   Top := Ptr-2 ;  (* Top = Id1                 *)
   PopT(i) ;       (* i   = # of the Ident List *)
   j := Top-i+1 ;  (* j   = Id#                 *)
   DEC(Ptr,i+2) ;  (* Ptr = ImportTok           *)
   IF Operand(Ptr)=ImportTok
   THEN
      (* Ident List contains list of objects *)
      WHILE j<=Top DO
         Sym := GetFromOuterModule(Operand(j)) ;
         PutImported(Sym) ;
         INC(j)
      END
   ELSE
      WriteError('not allowed FROM in an inner module')
   END ;
   PutPtr(Ptr)   (* Clear Stack *)
END BuildImportInnerModule ;


(*
   BuildExportInnerModule - Builds exported identifiers from an inner module
                            to the next layer module.

                            The Stack is expected:

                            Entry           OR    Entry

                     Ptr ->                Ptr ->
                            +------------+        +--------------+
                            | #          |        | #            |
                            |------------|        |--------------|
                            | Id1        |        | Id1          |
                            |------------|        |--------------|
                            .            .        .              .
                            .            .        .              .
                            .            .        .              .
                            |------------|        |--------------|
                            | Id#        |        | Id#          |
                            |------------|        |--------------|
                            | ExportTok  |        | QualifiedTok |
                            |------------|        |--------------|

                            EXPORT Id1, .. Id# ;  EXPORT QUALIFIED Id1 .. Id# ;


                            Exit

                            All above stack discarded
*)

PROCEDURE BuildExportInnerModule ;
VAR
   Ptr, Top,
   Sym, ModSym,
   i, j       : CARDINAL ;
BEGIN
   GetPtr(Ptr) ;
   Top := Ptr-2 ;  (* Top = Id1                 *)
   PopT(i) ;       (* i   = # of the Ident List *)
   j := Top-i+1 ;  (* j   = Id#                 *)
   DEC(Ptr,i+2) ;  (* Ptr = ExportTok           *)
   IF Operand(Ptr)=ExportTok
   THEN
      (* Ident List contains list of objects *)
      WHILE j<=Top DO
         Sym := RequestSym(Operand(j)) ;          (* NulSym - dependant sym *)
                                                  (* in case an unknown is  *)
                                                  (* used.                  *)
         PutExported(Sym) ;
         INC(j)
      END
   ELSE
      WriteError('QUALIFIED not allowed in an Inner Module')
   END ;
   PutPtr(Ptr)   (* Clear Stack *)
END BuildExportInnerModule ;


(*
   BuildEnumeration - Builds an Enumeration type Type.


                      Stack

                      Entry                 Exit

               Ptr ->
                      +------------+
                      | #          |
                      |------------|
                      | en 1       |
                      |------------|
                      | en 2       |
                      |------------|
                      .            .
                      .            .
                      .            .                       <- Ptr
                      |------------|        +------------+
                      | en #       |        | Type       |
                      |------------|        |------------|
                      | Name       |        | Name       |
                      |------------|        |------------|
*)

PROCEDURE BuildEnumeration ;
VAR
   No,
   Ptr, Top,
   i,
   Name, Type: CARDINAL ;
BEGIN
   PopT(No) ;      (* No := # *)
   GetPtr(Ptr) ;
   Top := Ptr-1 ;  (* Top -> en 1 *)
   i := Ptr-No ;   (* i   -> en # *)
   PutPtr(i) ;
   PopT(Name) ;
   Type := MakeEnumeration(Name) ;
   WHILE i<=Top DO
      PutFieldEnumeration(Type, Operand(i)) ;
      INC(i)
   END ;
   PutIntoFifoQueue(Type) ;  (* Store enumeration away for Pass 2 *)
   PushT(Name) ;
   PushT(Type)
END BuildEnumeration ;


(*
   BuildType - Builds a Type.


               Stack

               Entry                 Exit

        Ptr ->
               +------------+
               | Type       |                          <- Ptr
               |------------|        +---------------+
               | Name       |        | Type  | Name  |
               |------------|        |---------------|

                                     Empty
*)

PROCEDURE BuildType ;
VAR
   Sym,
   Type,
   Name: CARDINAL ;
BEGIN
   (*
      Two cases

      - the type name the same as Name, or the name is nul. - do nothing.
      - when type with a name that is different to Name. In which case
        we create a new type.
   *)
   PopT(Type) ;
   PopT(Name) ;
   IF (Name=NulName) OR (GetSymName(Type)=Name)
   THEN
      (*
         Typically the declaration that causes this case is:

         VAR
            a: RECORD
                  etc
               END ;
             ^
             |
             +---- type has no name.
      *)
      PushTF(Type, Name)
   ELSE
      (* E.G   TYPE a = CARDINAL *)
      Sym := MakeType(Name) ;
      PutType(Sym, Type) ;
      PushTF(Sym, Name)
   END
END BuildType ;


(*
   BuildHiddenType - Builds a Hidden Type.


                     Stack

                     Entry                 Exit

              Ptr ->
                     +------------+
                     | Name       |                          <- Ptr
                     |------------|        Empty
*)

PROCEDURE BuildHiddenType ;
VAR
   Name,
   Sym : CARDINAL ;
BEGIN
   PopT(Name) ;
   (* WriteString('Hidden type enocuntered: ') ; *)
   (* WriteKey(Name) ; WriteLn ; *)
   Sym := MakeHiddenType(Name)
END BuildHiddenType ;


(*
   StartBuildProcedure - Builds a Procedure.

                         The Stack:

                         Entry                 Exit

                                                              <- Ptr
                                               +------------+
                  Ptr ->                       | ProcSym    |
                         +------------+        |------------|
                         | Name       |        | Name       |
                         |------------|        |------------|
*)

PROCEDURE StartBuildProcedure ;
VAR 
   Name,
   ProcSym : CARDINAL ;
BEGIN
   PopT(Name) ;
   PushT(Name) ;  (* Name saved for the EndBuildProcedure name check *)
   ProcSym := RequestSym(Name) ;
   IF IsUnknown(ProcSym)
   THEN
      (*
         May have been compiled in DEF or IMP module, remember that IMP maybe
         compiled before corresponding DEF module.
      *)
      ProcSym := MakeProcedure(Name)
   ELSE
      Assert(IsProcedure(ProcSym))
   END ;
   PushT(ProcSym) ;
   StartScope(ProcSym)
END StartBuildProcedure ;


(*
   EndBuildProcedure - Ends building a Procedure.
                       It checks the start procedure name matches the end
                       procedure name.

                       The Stack:

                       (Procedure Not Defined in definition module)

                       Entry                 Exit

                Ptr ->
                       +------------+
                       | NameEnd    |
                       |------------|
                       | ProcSym    |
                       |------------|
                       | NameStart  |
                       |------------|
                                             Empty
*)

PROCEDURE EndBuildProcedure ;
VAR
   NameEnd,
   ProcSym,
   NameStart: CARDINAL ;
BEGIN
   PopT(NameEnd) ;
   PopT(ProcSym) ;
   PopT(NameStart) ;
   IF NameEnd#NameStart
   THEN
      WriteErrorFormat2('procedure name at end (%s) does not match name at beginning (%s)', NameEnd, NameStart)
   END ;
   EndScope
END EndBuildProcedure ;


(*
   BuildProcedureHeading - Builds a procedure heading for the definition
                           module procedures.

                           Operation only performed if compiling a
                           definition module.

                           The Stack:

                           Entry                       Exit

                    Ptr ->
                           +------------+
                           | ProcSym    |
                           |------------|
                           | NameStart  |
                           |------------|
                                                       Empty

*)

PROCEDURE BuildProcedureHeading ;
VAR
   ProcSym,
   NameStart: CARDINAL ;
BEGIN
   IF CompilingDefinitionModule()
   THEN
      PopT(ProcSym) ;
      PopT(NameStart) ;
      EndScope
   END
END BuildProcedureHeading ;


(*
   BuildNulName - Pushes a NulName onto the top of the stack.
                  The Stack:


                  Entry                    Exit

                                                          <- Ptr
                  Empty                    +------------+
                                           | NulName    |
                                           |------------|
*)

PROCEDURE BuildNulName ;
BEGIN
   PushT(NulName)
END BuildNulName ;


(*
   BuildTypeEnd - Pops the type Type and Name.
                  The Stack:


                  Entry                    Exit


           Ptr ->
                  +-------------+
                  | Type | Name |          Empty
                  |-------------|
*)

PROCEDURE BuildTypeEnd ;
VAR
   Type, Name: CARDINAL ;
BEGIN
   PopTF(Type, Name)
END BuildTypeEnd ;


END P1SymBuild.
