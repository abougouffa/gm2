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

IMPLEMENTATION MODULE P1SymBuild ;


FROM ASCII IMPORT nul ;
FROM NameKey IMPORT Name, WriteKey, MakeKey, KeyToCharStar, NulName ;
FROM M2Debug IMPORT Assert, WriteDebug ;
FROM M2LexBuf IMPORT GetFileName ;
FROM M2Error IMPORT WriteFormat0, WriteFormat1, WriteFormat2, WriteFormat3 ;
FROM DynamicStrings IMPORT String, Slice, InitString, KillString, EqualCharStar, RIndex, Mark ;
FROM M2Printf IMPORT printf0, printf1, printf2 ;
FROM M2Options IMPORT Iso ;

FROM M2Reserved IMPORT ImportTok, ExportTok, QualifiedTok, UnQualifiedTok,
                       NulTok, VarTok, ArrayTok, BuiltinTok, InlineTok ;

FROM FifoQueue IMPORT PutEnumerationIntoFifoQueue ;

FROM SymbolTable IMPORT NulSym,
                        ModeOfAddr,
                        StartScope, EndScope, PseudoScope,
                        GetScope, GetCurrentScope,
                        IsDeclaredIn,
                        SetCurrentModule, SetFileModule,
                        MakeInnerModule,
                        MakeConstLit,
                        MakeConstLitString,
                        MakeEnumeration, MakeSubrange,
                        MakeVar, MakeType, PutType,
                        MakeHiddenType,
                        PutMode,
                        PutFieldEnumeration, PutSubrange, PutVar,
                        IsDefImp, IsModule, IsType,
                        GetCurrentModule,
                        AddSymToModuleScope,
                        AddNameToImportList,
                        GetSym, RequestSym, IsUnknown, RenameSym,
                        GetFromOuterModule,
                        GetExported, IsExported,
                        GetLocalSym,
                        PutImported,
                        PutExported, PutExportQualified, PutExportUnQualified,
                        TryMoveUndeclaredSymToInnerModule,
                        PutDefinitionForC,
                        IsDefinitionForC,
                        PutDoesNeedExportList, PutDoesNotNeedExportList,
                        DoesNotNeedExportList,
                        MakeProcedure,
                        PutFunction, PutParam, PutVarParam,
                        GetNthParam,
                        IsProcedure, IsConstString,
                        MakePointer, PutPointer,
                        MakeRecord, PutFieldRecord,
                        MakeArray,
                        MakeSubscript, PutSubscript,
                        PutArray, GetType, IsArray,
                        IsProcType, MakeProcType,
                        PutProcTypeVarParam, PutProcTypeParam,
                        PutProcedureBuiltin, PutProcedureInline,
                        GetSymName,
                        ResolveImports,
                        DisplayTrees ;

FROM M2Batch IMPORT MakeDefinitionSource,
                    MakeImplementationSource,
                    MakeProgramSource ;

FROM M2Quads IMPORT PushT, PopT, PushTF, PopTF, OperandT, PopN ;

FROM M2Comp IMPORT CompilingDefinitionModule,
                   CompilingImplementationModule,
                   CompilingProgramModule ;

FROM M2Base IMPORT MixTypes ;


CONST
   Debugging = FALSE ;

VAR
   CheckProcedure: BOOLEAN ;  (* Set if currently implementing a defined *)
                              (* procedure.                              *)


(*
   CheckName - checks to see that the module name matches the file name.
*)

PROCEDURE CheckFileName (name: Name; ModuleType: ARRAY OF CHAR) ;
VAR
   ext,
   basename: CARDINAL ;
   s,
   FileName: String ;
BEGIN
   FileName := GetFileName() ;
   basename := RIndex(FileName, '/', 0) ;
   IF basename=-1
   THEN
      basename := 0
   END ;
   ext := RIndex(FileName, '.', 0) ;
   IF ext=-1
   THEN
      ext := 0
   END ;
   FileName := Slice(FileName, basename, ext) ;
   IF EqualCharStar(FileName, KeyToCharStar(name))
   THEN
      FileName := KillString(FileName)
   ELSE
      s := Mark(InitString(ModuleType)) ;
      WriteFormat3('%s module name (%a) is inconsistant with the filename (%s)',
                   s, name, FileName)
   END
END CheckFileName ;


(*
   StartBuildDefinitionModule - Creates a definition module and starts
                                a new scope.

                                he Stack is expected:

                                Entry                 Exit

                         Ptr ->               
                                +------------+
                                | NameStart  |                       <- Ptr
                                |------------|        +------------+
                                | NulName/"C"|        | NameStart  |
                                |------------|        |------------|
*)

PROCEDURE P1StartBuildDefinitionModule ;
VAR
   n, name  : Name ;
   language,
   ModuleSym: CARDINAL ;
BEGIN
   PopT(name) ;
   (* CheckFileName(name, 'definition') ; *)
   ModuleSym := MakeDefinitionSource(name) ;
   PutDoesNotNeedExportList(ModuleSym) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(IsDefImp(ModuleSym)) ;
   Assert(CompilingDefinitionModule()) ;
   PopT(language) ;
   IF (language#NulSym) AND IsConstString(language)
   THEN
      IF GetSymName(language)=MakeKey('C')
      THEN
         PutDefinitionForC(ModuleSym)
      ELSIF GetSymName(language)=NulName
      THEN
         WriteFormat0('currently a non modula-2 definition module can only be declared as DEFINITION FOR "C"')
      ELSE
         n := GetSymName(language) ;
         WriteFormat1('unknown definition module language (%a), currently a non modula-2 definition module can only be declared as DEFINITION FOR "C"', n)
      END
   END ;
   PushT(name)
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
   NameEnd  : Name ;
BEGIN                                 
   Assert(CompilingDefinitionModule()) ;
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF Debugging
   THEN
      printf0('pass 1: ') ;
      DisplayTrees(GetCurrentModule())
   END ;
   IF NameStart#NameEnd
   THEN
      WriteFormat1('inconsistant definition module name %a', NameStart)
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
   name, n  : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(name) ;
   (* CheckFileName(name, 'implementation') ; *)
   ModuleSym := MakeImplementationSource(name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   IF NOT IsDefImp(ModuleSym)
   THEN
      n := GetSymName(ModuleSym) ;
      WriteFormat1('cannot find corresponding definition module to %a', n)
   END ;
   Assert(CompilingImplementationModule()) ;
   PushT(name)
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
   NameEnd  : Name ;
BEGIN
   ResolveImports ;
   Assert(CompilingImplementationModule()) ;
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF NameStart#NameEnd
   THEN
      WriteFormat1('inconsistant implementation module name %a', NameStart)
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
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(name) ;
   (* CheckFileName(name, 'main') ; *)
   ModuleSym := MakeProgramSource(name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   IF (NOT CompilingProgramModule()) OR IsDefImp(ModuleSym)
   THEN
      WriteFormat1('module %a has a corresponding DEFINITION MODULE but no IMPLEMENTATION keyword in the main module', name)
   END ;
   PushT(name)
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
   NameEnd  : Name ;
BEGIN
   ResolveImports ;
   Assert(CompilingProgramModule()) ;
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF Debugging
   THEN
      printf0('pass 1: ') ;
      DisplayTrees(GetCurrentModule())
   END ;
   IF NameStart#NameEnd
   THEN
      WriteFormat1('inconsistant program module name %a', NameStart)
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
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(name) ;
   ModuleSym := MakeInnerModule(name) ;
   StartScope(ModuleSym) ;
   Assert(NOT IsDefImp(ModuleSym)) ;
   PushT(name)
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
   NameEnd  : Name ;
BEGIN
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF NameStart#NameEnd
   THEN
      WriteFormat1('inconsistant inner module name %a', NameStart)
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
   Sym, ModSym,
   i, n       : CARDINAL ;
BEGIN
   PopT(n) ;       (* n   = # of the Ident List *)
   IF OperandT(n+1)=ImportTok
   THEN
      (* Ident list contains Module Names *)
      i := 1 ;
      WHILE i<=n DO
         ModSym := MakeDefinitionSource(OperandT(n+1-i)) ;
         PutImported(ModSym) ;
         INC(i)
      END
   ELSE
      (* Ident List contains list of objects *)
      ModSym := MakeDefinitionSource(OperandT(n+1)) ;
      i := 1 ;
      WHILE i<=n DO
(*
         WriteString('Importing ') ; WriteKey(Operand(j)) ; WriteString(' from ') ; WriteKey(GetSymName(ModSym)) ; WriteLn ;
*)
         Sym := GetExported(ModSym, OperandT(n+1-i)) ;
         PutImported(Sym) ;
         INC(i)
      END
   END ;
   PopN(n+1)   (* clear stack *)
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
   i, n: CARDINAL ;
BEGIN
   PopT(n) ;       (* n   = # of the Ident List *)
   IF (OperandT(n+1)=QualifiedTok) AND CompilingDefinitionModule()
   THEN
      PutDoesNeedExportList(GetCurrentModule()) ;
      (* Ident List contains list of export qualified objects *)
      i := 1 ;
      WHILE i<=n DO
         PutExportQualified(OperandT(i)) ;
         INC(i)
      END
   ELSIF (OperandT(n+1)=UnQualifiedTok) AND CompilingDefinitionModule()
   THEN
      PutDoesNeedExportList(GetCurrentModule()) ;
      (* Ident List contains list of export unqualified objects *)
      i := 1 ;
      WHILE i<=n DO
         PutExportUnQualified(OperandT(i)) ;
         INC(i)
      END
   ELSIF CompilingDefinitionModule()
   THEN
      WriteFormat0('the export must be either QUALIFIED or UNQUALIFIED in a definition module')
   ELSE
      WriteFormat0('only allowed inter module exports in definition module')
   END ;
   PopN(n+1)  (* clear stack *)
END BuildExportOuterModule ;


(*
   CheckExplicitExported - checks to see whether we are compiling
                           a definition module and whether the ident
                           is implicitly export qualified or unqualified.


                                  The Stack is expected:

                                  Entry                 Exit

                           Ptr ->                Ptr ->
                                  +------------+        +-----------+
                                  | Identname  |        | Identname |
                                  |------------|        |-----------|
 
*)

PROCEDURE CheckExplicitExported ;
BEGIN
   IF CompilingDefinitionModule() AND DoesNotNeedExportList(GetCurrentModule())
   THEN
      (* printf1('exporting identifier %a\n', OperandT(1)) ; *)
      IF IsDefinitionForC(GetCurrentModule())
      THEN
         PutExportUnQualified(OperandT(1))
      ELSE
         PutExportQualified(OperandT(1))
      END
   END
END CheckExplicitExported ;


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

                            Exit

                            All above stack discarded
*)

PROCEDURE BuildImportInnerModule ;
VAR
   Sym, ModSym,
   i, n       : CARDINAL ;
BEGIN
   PopT(n) ;       (* n   = # of the Ident List *)
   IF OperandT(n+1)=ImportTok
   THEN
      (* Ident List contains list of objects *)
      i := 1 ;
      WHILE i<=n DO
         AddNameToImportList(OperandT(i)) ;
         INC(i)
      END
   ELSE
      (* Ident List contains list of objects *)
      ModSym := MakeDefinitionSource(OperandT(n+1)) ;
      i := 1 ;
      WHILE i<=n DO
         Sym := GetExported(ModSym, OperandT(n+1-i)) ;
         PutImported(Sym) ;
         INC(i)
      END
   END ;
   PopN(n+1)    (* clear stack *)
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
   PrevMod,
   Sym, ModSym,
   i, n       : CARDINAL ;
BEGIN
   (* --fixme-- *)
   PopT(n) ;       (* n   = # of the Ident List *)
   IF OperandT(n+1)=ExportTok
   THEN
      (* Ident List contains list of objects *)
      i := 1 ;
      PrevMod := GetScope(GetCurrentScope()) ;
      WHILE i<=n DO
         IF (PrevMod#NulSym) AND (IsModule(PrevMod) OR IsDefImp(PrevMod))
         THEN
            Sym := GetLocalSym(PrevMod, OperandT(i)) ;
            IF Sym=NulSym
            THEN
               Sym := TryMoveUndeclaredSymToInnerModule(PrevMod, GetCurrentScope(), OperandT(i)) ;
               IF Sym=NulSym
               THEN
                  Sym := RequestSym(OperandT(i)) ;
                  PutExported(Sym)
               END
            ELSE
               (* use Sym which has already been created in outer scope *)
               AddSymToModuleScope(GetCurrentScope(), Sym)
            END
         ELSE
            Sym := RequestSym(OperandT(i)) ;
            PutExported(Sym)
         END ;
         INC(i)
      END
   ELSE
      WriteFormat0('QUALIFIED not allowed in an Inner Module')
   END ;
   PopN(n+1)    (* clear stack *)
END BuildExportInnerModule ;


(*
   StartBuildEnumeration - Builds an Enumeration type Type.


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

PROCEDURE StartBuildEnumeration ;
VAR
   name: Name ;
   n, i,
   Type: CARDINAL ;
BEGIN
   PopT(n) ;      (* No := # *)
   name := OperandT(n+1) ;
   Type := MakeEnumeration(name) ;
   i := 1 ;
   WHILE i<=n DO
      PutFieldEnumeration(Type, OperandT(n-i+1)) ;
      INC(i)
   END ;
   PutEnumerationIntoFifoQueue(Type) ;  (* store enumeration away for pass 2 *)
   PopN(n+1) ;
   PushT(name) ;
   PushT(Type)
END StartBuildEnumeration ;


(*
   EndBuildEnumeration - completes the construction of the enumeration type.


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

PROCEDURE EndBuildEnumeration ;
VAR
   Sym,
   Type  : CARDINAL ;
   n1, n2,
   name  : Name ;
BEGIN
   (*
      Two cases

      - the type name the same as Name, or the name is nul. - do nothing.
      - when type with a name that is different to Name. In which case
        we create a new type.
   *)
   PopT(Type) ;
   PopT(name) ;

   IF Debugging
   THEN
      n1 := GetSymName(GetCurrentModule()) ;
      printf2('inside module %a declaring type name %a\n',
              n1, name) ;
      IF (NOT IsUnknown(Type))
      THEN
         n1 := GetSymName(GetScope(Type)) ;
         n2 := GetSymName(Type) ;
         printf2('type was created inside scope %a as name %a\n',
                 n1, n2)
      END
   END ;
   IF (name=NulName) OR (GetSymName(Type)=name)
   THEN
      (*
         Typically the declaration that causes this case is:

         VAR
            a: (blue, green, red) ;
             ^
             |
             +---- type has no name.

         in which case the constructed from StartBuildEnumeration is complete
      *)
      PushTF(Type, name)
   ELSE
      (* in this case we are seeing:

         TYPE
            name = (blue, green, red)

         so we construct the type name and define it to have the previously
         created enumeration type
      *)
      Sym := MakeType(name) ;
      PutType(Sym, Type) ;
      PushTF(Sym, name)
   END
END EndBuildEnumeration ;


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
   name: Name ;
   Sym : CARDINAL ;
BEGIN
   PopT(name) ;
   (* WriteString('Hidden type enocuntered: ') ; *)
   (* WriteKey(Name) ; WriteLn ; *)
   Sym := MakeHiddenType(name)
END BuildHiddenType ;


(*
   StartBuildProcedure - Builds a Procedure.

                         The Stack:

                         Entry                 Exit

                  Ptr ->                                      <- Ptr
                         +------------+        +------------+
                         | Name       |        | ProcSym    |
                         |------------|        |------------|
                         | inlinetok  |        |            |
                         | or         |        |            |
                         | builtintok |        |            |
                         | or name or |        | Name       |
                         | NulTok     |        |            |
                         |------------|        |------------|
*)

PROCEDURE StartBuildProcedure ;
VAR
   builtin,
   name    : Name ;
   ProcSym : CARDINAL ;
   n       : Name ;
BEGIN
   PopT(name) ;
   PopT(builtin) ; (* was this procedure defined as a builtin?        *)
   PushT(name) ;   (* Name saved for the EndBuildProcedure name check *)
   ProcSym := RequestSym(name) ;
   IF IsUnknown(ProcSym)
   THEN
      (*
         May have been compiled in DEF or IMP module, remember that IMP maybe
         compiled before corresponding DEF module.
      *)
      ProcSym := MakeProcedure(name)
   ELSIF NOT IsProcedure(ProcSym)
   THEN
      n := GetSymName(ProcSym) ;
      WriteFormat1('expecting a procedure name and symbol (%a) has been declared otherwise', n) ;
      PushT(ProcSym) ;
      RETURN
   END ;
   IF builtin#NulTok
   THEN
      IF builtin=BuiltinTok
      THEN
         PutProcedureBuiltin(ProcSym, name)
      ELSIF builtin=InlineTok
      THEN
         PutProcedureInline(ProcSym)
      ELSE
         PutProcedureBuiltin(ProcSym, builtin)
      END
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
   ProcSym  : CARDINAL ;
   NameEnd,
   NameStart: Name ;
BEGIN
   PopT(NameEnd) ;
   PopT(ProcSym) ;
   PopT(NameStart) ;
   IF NameEnd#NameStart
   THEN
      IF NameEnd=NulName
      THEN
         WriteFormat1('procedure name at end does not match name at beginning (%a)', NameStart)
      ELSIF NameStart=NulName
      THEN
         WriteFormat1('procedure name at end (%a) does not match name at beginning', NameEnd)
      ELSE
         WriteFormat2('procedure name at end (%a) does not match name at beginning (%a)', NameEnd, NameStart)
      END
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
   ProcSym  : CARDINAL ;
   NameStart: Name ;
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
   Type: CARDINAL ;
   name: Name ;
BEGIN
   PopTF(Type, name)
END BuildTypeEnd ;


END P1SymBuild.
