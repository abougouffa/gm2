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
IMPLEMENTATION MODULE P2SymBuild ;


FROM NameKey IMPORT WriteKey, MakeKey, NulName ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard, CardToStr ;
FROM StrLib IMPORT StrEqual ;
FROM M2Debug IMPORT Assert, WriteDebug ;
FROM M2Lexical IMPORT WriteError, NearTokens, GetTokenNo, WriteErrorFormat1, WriteErrorFormat2, InternalError ;
FROM M2Error IMPORT BeginError, EndError ;
FROM M2Base IMPORT Char ;

FROM M2Reserved IMPORT ImportTok, ExportTok, QualifiedTok, UnQualifiedTok,
                       NulTok, VarTok, ArrayTok ;

FROM FifoQueue IMPORT GetFromFifoQueue ;

FROM SymbolTable IMPORT NulSym,
                        ModeOfAddr,
                        StartScope, EndScope, PseudoScope,
                        SetCurrentModule, SetFileModule,
                        GetCurrentModule,
                        MakeConstLit,
                        MakeConstLitString,
                        MakeEnumeration, MakeSubrange,
                        MakeVar, MakeType, PutType,
                        PutMode,
                        PutFieldEnumeration, PutSubrange, PutVar, PutConst,
                        PutConstSet,
                        IsDefImp, IsType,
                        IsSubrange, IsEnumeration, IsConstString,
                        GetSym, RequestSym, IsUnknown, RenameSym,
                        GetLocalSym, Father, IsRecord,
                        GetFromOuterModule,
                        GetExported,
                        PutImported,
                        PutExported, PutExportQualified, PutExportUnQualified,
                        PutExportUnImplemented,
                        CheckForEnumerationInCurrentModule,
                        CheckForExportedImplementation,
                        MakeProcedure,
                        PutFunction, PutParam, PutVarParam,
                        GetNthParam,
                        IsProcedure,
                        NoOfElements,
                        MakePointer, PutPointer,
      	          	MakeSet, PutSet,
                        MakeRecord, PutFieldRecord,
                        MakeVarient, MakeFieldVarient,
                        MakeArray, PutFieldArray,
                        MakeSubscript, PutSubscript,
                        PutConstString, GetString,
                        PutArray, IsArray,
                        GetType,
                        IsProcType, MakeProcType,
                        PutProcTypeVarParam, PutProcTypeParam,
                        MakeConstVar,
                        MakeUnbounded, PutUnbounded,
                        NoOfParam,
                        GetParam,
                        AreParametersDefinedInDefinition,
                        AreParametersDefinedInImplementation,
                        AreProcedureParametersDefined,
                        ParametersDefinedInDefinition,
                        ParametersDefinedInImplementation,
                        ProcedureParametersDefined,
                        CheckForUnImplementedExports,
                        CheckForUndeclaredExports,
                        IsUnboundedParam,
                        IsVarParam,
                        GetSymName,
                        GetDeclared ;

FROM M2Batch IMPORT MakeDefinitionSource,
                    MakeImplementationSource,
                    MakeProgramSource ;

FROM M2Quads IMPORT PushT, PopT,
                    PushTF, PopTF, SetWatch,
                    Operand, GetPtr, PutPtr, DisplayStack ;

FROM M2Comp IMPORT CompilingDefinitionModule,
                   CompilingImplementationModule,
                   CompilingProgramModule ;

FROM M2Base IMPORT MixTypes ;


FROM FifoQueue IMPORT PutIntoFifoQueue ;


(* %%%FORWARD%%%
PROCEDURE BuildFormalParameterSection ; FORWARD ;
PROCEDURE BuildNulParam ; FORWARD ;
PROCEDURE CheckFormalParameterSection ; FORWARD ;
PROCEDURE FailParameter (CurrentState : ARRAY OF CHAR;
                         PreviousState: ARRAY OF CHAR;
                         Expecting    : CARDINAL ;
                         ParameterNo  : CARDINAL;
                         ProcedureSym : CARDINAL) ; FORWARD ;
   %%%FORWARD%%% *)

VAR
   IsBuildingConstDeclaration: BOOLEAN ;


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

PROCEDURE P2StartBuildDefModule ;
VAR
   Name     : CARDINAL ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(Name) ;
   ModuleSym := MakeDefinitionSource(Name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(IsDefImp(ModuleSym)) ;
   Assert(CompilingDefinitionModule()) ;
   PushT(Name)
END P2StartBuildDefModule ;


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

PROCEDURE P2EndBuildDefModule ;
VAR
   NameStart,
   NameEnd  : CARDINAL ;
BEGIN                                 
   Assert(CompilingDefinitionModule()) ;
   CheckForUndeclaredExports(GetCurrentModule()) ;
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF NameStart#NameEnd
   THEN
      WriteErrorFormat1('inconsistant definition module name %s', NameStart)
   END
END P2EndBuildDefModule ;


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

PROCEDURE P2StartBuildImplementationModule ;
VAR
   Name     : CARDINAL ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(Name) ;
   ModuleSym := MakeImplementationSource(Name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(IsDefImp(ModuleSym)) ;
   Assert(CompilingImplementationModule()) ;
   PushT(Name)                        
END P2StartBuildImplementationModule ;


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

PROCEDURE P2EndBuildImplementationModule ;
VAR
   NameStart,
   NameEnd  : CARDINAL ;
BEGIN
   Assert(CompilingImplementationModule()) ;
   CheckForUnImplementedExports ;
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF NameStart#NameEnd
   THEN
      WriteErrorFormat1('inconsistant implementation module name %s', NameStart)
   END
END P2EndBuildImplementationModule ;


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

PROCEDURE P2StartBuildProgramModule ;
VAR
   Name     : CARDINAL ;
   ModuleSym: CARDINAL ;
BEGIN
   (* WriteString('StartBuildProgramModule') ; WriteLn ; *)
   PopT(Name) ;
   ModuleSym := MakeProgramSource(Name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   (* WriteString('MODULE - ') ; WriteKey(GetSymName(ModuleSym)) ; WriteLn ; *)
   StartScope(ModuleSym) ;
   Assert(CompilingProgramModule()) ;
   Assert(NOT IsDefImp(ModuleSym)) ;
   PushT(Name) ;
(*
   WriteString('P2StartBuildProgramModule') ; WriteLn ;
   DisplayStack
*)
END P2StartBuildProgramModule ;


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

PROCEDURE P2EndBuildProgramModule ;
VAR
   NameStart,
   NameEnd  : CARDINAL ;
BEGIN
   Assert(CompilingProgramModule()) ;
   CheckForUndeclaredExports(GetCurrentModule()) ;  (* Not really allowed exports here though! *)
   EndScope ;
(*
   WriteString('P2EndBuildProgramModule') ; WriteLn ;
   DisplayStack ;
*)
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF NameStart#NameEnd
   THEN
      WriteErrorFormat1('inconsistant program module name %s', NameStart)
   END
END P2EndBuildProgramModule ;


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
   ModuleSym := RequestSym(Name) ;
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
   CheckForUndeclaredExports(GetCurrentModule()) ;
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
   IF Operand(Ptr)#ImportTok
   THEN
      (* Ident List contains list of objects *)
      ModSym := MakeDefinitionSource(Operand(Ptr)) ;
      WHILE j<=Top DO
         Sym := GetExported(ModSym, Operand(j)) ;
         CheckForEnumerationInCurrentModule(Sym) ;
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
   (* WriteString('Inside BuildExportOuterModule') ; WriteLn ; *)
   GetPtr(Ptr) ;
   Top := Ptr-2 ;  (* Top = Id1                 *)
   PopT(i) ;       (* i   = # of the Ident List *)
   j := Top-i+1 ;  (* j   = Id#                 *)
   DEC(Ptr,i+2) ;  (* Ptr = ExportTok           *)
   PutPtr(Ptr)     (* Clear Stack *)
 (* ;  WriteString('Exit BuildExportOuterModule') ; WriteLn ; *)
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
         CheckForEnumerationInCurrentModule(Sym) ;
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
   PutPtr(Ptr)     (* Clear Stack               *)
END BuildExportInnerModule ;


(*
   BuildNumber - Converts a number into a symbol.


                 Stack

                 Entry                 Exit

          Ptr ->                                      <- Ptr
                 +------------+        +------------+
                 | Name       |        | Sym        |
                 |------------+        |------------|
*)

PROCEDURE BuildNumber ;
VAR
   Name,
   Sym : CARDINAL ;
BEGIN
   PopT(Name) ;
   Sym := MakeConstLit(Name) ;
   (* WriteString('Number symbol is') ; WriteCard(Sym, 6) ; WriteLn ; *)
   PushTF(Sym, GetType(Sym))
END BuildNumber ;


(*
   BuildString - Converts a string into a symbol.


                 Stack

                 Entry                 Exit

          Ptr ->                                      <- Ptr
                 +------------+        +------------+
                 | Name       |        | Sym        |
                 |------------+        |------------|
*)

PROCEDURE BuildString ;
VAR
   Name,
   Sym : CARDINAL ;
BEGIN
   PopT(Name) ;
   Sym := MakeConstLitString(Name) ;
   PushTF(Sym, NulSym)
END BuildString ;


(*
   BuildConst - builds a constant.
                Stack

                Entry                 Exit

         Ptr ->
                +------------+
                | Name       |
                |------------+                       <- Ptr
*)

PROCEDURE BuildConst ;
VAR
   Name,
   Sym : CARDINAL ;
BEGIN
   PopT(Name) ;
   Sym := MakeConstVar(Name) ;
   PushT(Sym)
END BuildConst ;


(*
   BuildConstTypeFromAssignment - assigns a string to the constant name.
                                  But it only does this if we in a CONST statement.
                                  In both cases the string on top of the stack is removed.

                                  Stack

                                  Entry           Exit

                           Ptr ->
                                  +------------+
                                  | Expr       |
                                  |------------|
                                  | Sym        |            <- Ptr
                                  |------------|
*)

PROCEDURE BuildConstTypeFromAssignment ;
VAR
   Expr,
   Sym : CARDINAL ;
BEGIN
   (* we might get called via a type declaration, consider
      TYPE  CharSet = SET OF ['0'..'9'] ;
   *)
   IF IsBuildingConstDeclaration
   THEN
      PopT(Expr) ;
      PopT(Sym) ;
      IF IsConstString(Expr)
      THEN
         PutConstString(Sym, GetString(Expr))
      END ;
      PushT(Sym)
   ELSE
      PopT(Expr)    (* remove the string *)
   END
END BuildConstTypeFromAssignment ;


(*
   BuildConstSetType - assigns the const var symbol on top of the stack
                       as being a set constant. The stack is unchanged.

                                  Entry           Exit

                       Ptr ->                                    <- Ptr
                              +------------+      +------------+
                              | Sym        |      | Sym        |
                              |------------|      |------------|
*)

PROCEDURE BuildConstSetType ;
VAR
   Expr,
   Sym : CARDINAL ;
BEGIN
   IF IsBuildingConstDeclaration
   THEN
      PopT(Expr) ;
      PutConstSet(Expr) ;
      PushT(Expr)
(*
      PopT(Expr) ;
      PopT(Sym) ;
      PutConstSet(Sym) ;  (* Sym = Expr is being parsed *)
      PushT(Sym) ;
      PushT(Expr)
*)
   ELSE
      PopT(Expr) ;
      PutConstSet(Expr) ;
      PushT(Expr)
   END
END BuildConstSetType ;


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
   GetFromFifoQueue(Type) ;
   CheckForExportedImplementation(Type) ;   (* May be an exported hidden type *)
   PushT(Name) ;
   PushT(Type)
END BuildEnumeration ;


(*
   BuildSubrange - Builds a Subrange type Symbol.


                      Stack

                      Entry                 Exit


                                                           <- Ptr
                                            +------------+
               Ptr ->                       | Type       |
                      +------------+        |------------|
                      | Name       |        | Name       |
                      |------------|        |------------| 
*)

PROCEDURE BuildSubrange ;
VAR
   Type,
   Name: CARDINAL ;
BEGIN
   PopT(Name) ;
   (* WriteString('Subrange type name is: ') ; WriteKey(Name) ; WriteLn ; *)
   Type := MakeSubrange(Name) ;
   PutIntoFifoQueue(Type) ;   (* Store Subrange away so that we can fill in *)
                              (* its bounds during pass 3.                  *)
   CheckForExportedImplementation(Type) ;   (* May be an exported hidden type *)
   PushT(Name) ;
   PushT(Type)
END BuildSubrange ;


(*
   BuildVariable - Builds variables listed in an IdentList with a Type.

                   Stack

                   Entry                 Exit

            Ptr ->
                   +------------+        +------------+
                   | Type | Name|        |            |
                   |------------|        |------------|
                   | #          |        |            |
                   |------------|        |------------|
                   | Ident 1    |        |            |
                   |------------|        |------------|
                   | Ident 2    |        |            |
                   |------------|        |------------|
                   .            .        .            .
                   .            .        .            .
                   .            .        .            .
                   |------------|        |------------|
                   | Ident #    |        |            | <- Ptr
                   |------------|        |------------|

                                           Empty
*)

PROCEDURE BuildVariable ;
VAR
   Type, Name,
   Var,
   Ptr, Top ,
   i, No    : CARDINAL ;
BEGIN
   PopTF(Type, Name) ;
   (* WriteString('Name of variable type is ') ; WriteKey(Name) ; WriteLn ; *)
   PopT(No) ;
   GetPtr(Ptr) ;
   Top := Ptr-1 ;  (* Ident 1 *)
   i := Ptr-No ;   (* Ident # *)
   PutPtr(i) ;     (* Chuck all stack *)
   WHILE i<=Top DO
      Var := MakeVar(Operand(i)) ;
      PutVar(Var, Type) ;
(*
      ; WriteString('Variable ') ; WriteKey(GetSymName(Var)) ;
      ; WriteString(' has a type ') ; WriteKey(GetSymName(Type)) ; WriteLn ;
*)
      INC(i)
   END
END BuildVariable ;


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
      (* WriteString('Blank name type') ; WriteLn ; *)
      PushTF(Type, Name)
   ELSE
(*
      WriteString('Creating an intemediate type with name ') ;
      WriteKey(Name) ; WriteLn ;
*)
      (* E.G   TYPE a = CARDINAL *)
      Sym := MakeType(Name) ;
      PutType(Sym, Type) ;
      CheckForExportedImplementation(Sym) ;   (* May be an exported hidden type *)
      PushTF(Sym, Name)
   END
END BuildType ;


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
         - no defs should always be compilied before implementation modules.
      *)
      ProcSym := MakeProcedure(Name)
   ELSE
      IF NOT IsProcedure(ProcSym)
      THEN
         NearTokens('', GetTokenNo(), GetDeclared(ProcSym)) ;
         WriteErrorFormat1('procedure name (%s) has been declared as another object elsewhere',
                           Name)
      END
   END ;
   IF CompilingDefinitionModule()
   THEN
      PutExportUnImplemented(ProcSym)    (* Defined but not yet implemented *)
   ELSE
      CheckForExportedImplementation(ProcSym)   (* May be exported procedure *)
   END ;
   PushT(ProcSym) ;
(*
   WriteString('PROCEDURE ') ; WriteKey(Name) ; WriteString(' Built - symbol:') ; WriteCard(ProcSym, 4) ; WriteLn ;
*)
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
      WriteErrorFormat1('procedure name does not match beginning %s', NameStart)
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
   BuildFPSection - Builds a Formal Parameter in a procedure.

                    The Stack:

                    Entry                 Exit

             Ptr ->
                    +------------+
                    | ParamTotal |
                    |------------|
                    | TypeSym    |
                    |------------|
                    | Array/Nul  |
                    |------------|
                    | NoOfIds    |
                    |------------|
                    | Id 1       |
                    |------------|
                    .            .
                    .            .
                    .            .
                    |------------|
                    | Id n       |                       <- Ptr
                    |------------|        +------------+
                    | Var / Nul  |        | ParamTotal |
                    |------------|        |------------|
                    | ProcSym    |        | ProcSym    |
                    |------------|        |------------|
*)

PROCEDURE BuildFPSection ;
VAR
   ParamTotal,
   TypeSym,
   UnBoundedSym,
   Array,
   NoOfIds,
   Ptr,
   ProcSym,
   i, Var    : CARDINAL ;
BEGIN
   GetPtr(Ptr) ;
   ParamTotal := Operand(Ptr-1) ;
   DEC(Ptr, 4) ;
   ProcSym := Operand(Ptr-Operand(Ptr)-2) ;
   Assert(IsProcedure(ProcSym)) ;
   IF CompilingDefinitionModule()
   THEN
      IF AreParametersDefinedInDefinition(ProcSym) AND (ParamTotal=0)
      THEN
         WriteErrorFormat1('cannot declare procedure %s twice in definition module', GetSymName(ProcSym))
      ELSIF AreParametersDefinedInImplementation(ProcSym)
      THEN
         CheckFormalParameterSection
      ELSE
         BuildFormalParameterSection ;
         IF ParamTotal=0
         THEN
            ParametersDefinedInDefinition(ProcSym) ;
            ProcedureParametersDefined(ProcSym)
         END
      END
   ELSIF CompilingImplementationModule()
   THEN
      IF AreParametersDefinedInImplementation(ProcSym) AND (ParamTotal=0)
      THEN
         WriteErrorFormat1('cannot declare procedure %s twice in implementation module', GetSymName(ProcSym))
      ELSIF AreParametersDefinedInDefinition(ProcSym)
      THEN
         CheckFormalParameterSection
      ELSE
         BuildFormalParameterSection ;
         IF ParamTotal=0
         THEN
            ParametersDefinedInImplementation(ProcSym) ;
            ProcedureParametersDefined(ProcSym)
         END
      END
   ELSIF CompilingProgramModule()
   THEN
      IF AreProcedureParametersDefined(ProcSym) AND (ParamTotal=0)
      THEN
         WriteErrorFormat1('procedure %s parameters already declared in program module', GetSymName(ProcSym))
      ELSE
         BuildFormalParameterSection ;
         IF ParamTotal=0
         THEN
            ProcedureParametersDefined(ProcSym)
         END
      END
   ELSE
      InternalError('should never reach this point', __FILE__, __LINE__)
   END
END BuildFPSection ;


(*
   BuildFormalParameterSection - Builds a Formal Parameter in a procedure.

                                 The Stack:

                                 Entry                 Exit

                          Ptr ->
                                 +------------+
                                 | ParamTotal |
                                 |------------|
                                 | TypeSym    |
                                 |------------|
                                 | Array/Nul  |
                                 |------------|
                                 | NoOfIds    |
                                 |------------|
                                 | Id 1       |
                                 |------------|
                                 .            .
                                 .            .
                                 .            .
                                 |------------|
                                 | Id n       |                       <- Ptr
                                 |------------|        +------------+
                                 | Var / Nul  |        | ParamTotal |
                                 |------------|        |------------|
                                 | ProcSym    |        | ProcSym    |
                                 |------------|        |------------|
*)

PROCEDURE BuildFormalParameterSection ;
VAR
   ParamTotal,
   TypeSym,
   UnBoundedSym,
   Array,
   NoOfIds,
   Ptr,
   ProcSym,
   i, Var    : CARDINAL ;
BEGIN
   PopT(ParamTotal) ;
   PopT(TypeSym) ;
   PopT(Array) ;
   Assert( (Array=ArrayTok) OR (Array=NulTok) ) ;
   PopT(NoOfIds) ;
   GetPtr(Ptr) ;
   DEC(Ptr, NoOfIds+2) ;  (* Ptr points to ProcSym *)
   PutPtr(Ptr) ;
   ProcSym := Operand(Ptr) ;
   Assert(IsProcedure(ProcSym)) ;
   Var := Operand(Ptr+1) ;
   Assert( (Var=VarTok) OR (Var=NulTok) ) ;
   IF Array=ArrayTok
   THEN
      UnBoundedSym := MakeUnbounded() ;
      PutUnbounded(UnBoundedSym, TypeSym) ;
      TypeSym := UnBoundedSym
   END ;
   i := 1 ;
(*
   WriteString('Procedure ') ; WriteKey(GetSymName(ProcSym)) ;
   WriteString(' adding No. of identifiers:') ; WriteCard(NoOfIds, 4) ; WriteLn ;
*)
   WHILE i<=NoOfIds DO
      IF Var=VarTok
      THEN
         (* VAR pamarater *)
         IF NOT PutVarParam(ProcSym, ParamTotal+i, Operand(Ptr+i+1), TypeSym)
         THEN
            InternalError('problems adding a VarParameter - wrong param #?', __FILE__, __LINE__)
         END
      ELSE
         (* Non VAR parameter *)
         IF NOT PutParam(ProcSym, ParamTotal+i, Operand(Ptr+i+1), TypeSym)
         THEN
            InternalError('problems adding a Parameter - wrong param #?', __FILE__, __LINE__)
         END
      END ;
(*
      WriteString(' parameter') ; WriteCard(ParamTotal+i, 4) ; WriteLn ;
      WriteKey(Operand(Ptr+i+1)) ; WriteString(' is a parameter with type ') ;
      WriteKey(GetSymName(TypeSym)) ; WriteLn ;
*)
      INC(i)
   END ;
   PushT(ProcSym) ;
   PushT(ParamTotal+NoOfIds)
END BuildFormalParameterSection ;


(*
   CheckFormalParameterSection - Checks a Formal Parameter in a procedure.

                                 The Stack:

                                 Entry                 Exit

                          Ptr ->
                                 +------------+
                                 | ParamTotal |
                                 |------------|
                                 | TypeSym    |
                                 |------------|
                                 | Array/Nul  |
                                 |------------|
                                 | NoOfIds    |
                                 |------------|
                                 | Id 1       |
                                 |------------|
                                 .            .
                                 .            .
                                 .            .
                                 |------------|
                                 | Id n       |                       <- Ptr
                                 |------------|        +------------+
                                 | Var / Nul  |        | ParamTotal |
                                 |------------|        |------------|
                                 | ProcSym    |        | ProcSym    |
                                 |------------|        |------------|
*)

PROCEDURE CheckFormalParameterSection ;
VAR
   Unbounded   : BOOLEAN ;
   ParamI,
   ParamIType,
   ParamTotal,
   TypeSym,
   Array,
   NoOfIds,
   Ptr,
   ProcSym,
   i, Var      : CARDINAL ;
BEGIN
   PopT(ParamTotal) ;
   PopT(TypeSym) ;
   PopT(Array) ;
   Assert( (Array=ArrayTok) OR (Array=NulTok) ) ;
   PopT(NoOfIds) ;
   GetPtr(Ptr) ;
   DEC(Ptr, NoOfIds+2) ;  (* Ptr points to ProcSym *)
   PutPtr(Ptr) ;
   ProcSym := Operand(Ptr) ;
   Assert(IsProcedure(ProcSym)) ;
   Var := Operand(Ptr+1) ;
   Assert( (Var=VarTok) OR (Var=NulTok) ) ;
   Unbounded := (Array=ArrayTok) ;  (* ARRAY OF Type, parameter *)
   i := 1 ;
(*
   WriteString('No. of identifiers:') ; WriteCard(NoOfIds, 4) ; WriteLn ;
*)
   WHILE i<=NoOfIds DO
      IF ParamTotal+i<=NoOfParam(ProcSym)
      THEN
         IF Unbounded AND (NOT IsUnboundedParam(ProcSym, ParamTotal+i))
         THEN
            FailParameter('the parameter was declared as an ARRAY OF CHAR',
                          'the parameter was not declared as an ARRAY OF CHAR',
                          NulName, ParamTotal+i, ProcSym)
         ELSIF (NOT Unbounded) AND IsUnboundedParam(ProcSym, ParamTotal+i)
         THEN
            FailParameter('the parameter was not declared as an ARRAY OF CHAR',
                          'the parameter was declared as an ARRAY OF CHAR',
                          NulName, ParamTotal+i, ProcSym)
         END ;
         IF (Var=VarTok) AND (NOT IsVarParam(ProcSym, ParamTotal+i))
         THEN
            (* expecting non VAR pamarater *)
            FailParameter('the parameter has been declared as a VAR parameter',
                          'the parameter was not declared as a VAR parameter',
                          NulName, ParamTotal+i, ProcSym)
         ELSIF (Var=NulTok) AND IsVarParam(ProcSym, ParamTotal+i)
         THEN
            (* expecting VAR pamarater *)
            FailParameter('the parameter was not declared as a VAR parameter',
                          'the parameter has been declared as a VAR parameter',
                          NulName, ParamTotal+i, ProcSym)
         END ;
         ParamI := GetParam(ProcSym, ParamTotal+i) ;
         IF GetSymName(ParamI)#Operand(Ptr+i+1)
         THEN
            (* different parameter names *)
            FailParameter('',
                          'the parameter has been declared with a different name',
                          GetSymName(ParamI), ParamTotal+i, ProcSym)
         END ;
         IF Unbounded
         THEN
            (* GetType(ParamI) yields an UnboundedSym *)
            ParamIType := GetType(GetType(ParamI))
         ELSE
            ParamIType := GetType(ParamI)
         END ;
         IF ParamIType#TypeSym
         THEN
            (* different parameter types *)
            FailParameter('',
                          'the parameter has been declared with a different type',
                          GetSymName(ParamIType), ParamTotal+i, ProcSym)
         END ;
(*
         WriteKey(Operand(Ptr+i+1)) ; WriteString(' is a parameter with type ') ;
         WriteKey(GetSymName(TypeSym)) ; WriteLn
*)
      ELSE
         FailParameter('too many parameters',
                       'fewer parameters were declared',
                       NulName, ParamTotal+i, ProcSym)
      END ;
      INC(i)
   END ;
   PushT(ProcSym) ;
   PushT(ParamTotal+NoOfIds)
END CheckFormalParameterSection ;


(*
   FailParameter - generates an error message indicating that a parameter
                   declaration has failed.

                   The parameters are:

                   CurrentState  - string describing the current failing state.
                   PreviousState - string describing the old defined state.
                   Expecting     - token or identifier that was expected.
                   ParameterNo   - parameter number that has failed.
                   ProcedureSym  - procedure symbol where parameter has failed.

                   If any parameter is Nul then it is ignored.
*)

PROCEDURE FailParameter (CurrentState : ARRAY OF CHAR;
                         PreviousState: ARRAY OF CHAR;
                         Expecting    : CARDINAL ;
                         ParameterNo  : CARDINAL;
                         ProcedureSym : CARDINAL) ;
VAR
   First,
   Second: CARDINAL ;
BEGIN
   BeginError ;
   IF NoOfParam(ProcedureSym)>=ParameterNo
   THEN
      First := GetDeclared(GetNthParam(ProcedureSym, ParameterNo))
   ELSE
      (* ParameterNo does not exist - which is probably the reason why this routine was called.. *)
      First := GetDeclared(ProcedureSym)
   END ;
   IF CompilingDefinitionModule()
   THEN
      WriteString('Error found while compiling the definition module') ; WriteLn ;
      WriteString('PROCEDURE ') ; WriteKey(GetSymName(ProcedureSym)) ;
      WriteString(' : parameter') ; WriteCard(ParameterNo, 4) ;
      IF NOT StrEqual(CurrentState, '')
      THEN
         WriteString(' - ') ; WriteString(CurrentState)
      END ;
      WriteLn ;
      IF NOT StrEqual(PreviousState, '')
      THEN
         WriteString('whereas in the implementation module ') ;
         WriteString(PreviousState) ;
         WriteLn
      END
   ELSIF CompilingImplementationModule()
   THEN
      WriteString('Error found while compiling the implementation module') ; WriteLn ;
      WriteString('PROCEDURE ') ; WriteKey(GetSymName(ProcedureSym)) ;
      WriteString(' : parameter') ; WriteCard(ParameterNo, 4) ;
      IF NOT StrEqual(CurrentState, '')
      THEN
         WriteString(' - ') ; WriteString(CurrentState)
      END ;
      WriteLn ;
      IF NOT StrEqual(PreviousState, '')
      THEN
         WriteString('Whereas in the definition module ') ;
         WriteString(PreviousState) ;
         WriteLn
      END
   ELSIF CompilingProgramModule()
   THEN
      WriteString('Error found while compiling the program module') ; WriteLn ;
      WriteString('PROCEDURE ') ; WriteKey(GetSymName(ProcedureSym)) ;
      WriteString(' : parameter') ; WriteCard(ParameterNo, 4) ;
      IF NOT StrEqual(CurrentState, '')
      THEN
         WriteString(' - ') ; WriteString(CurrentState)
      END ;
      WriteLn ;
      IF NOT StrEqual(PreviousState, '')
      THEN
         WriteString('whereas previously ') ;
         WriteString(PreviousState) ;
         WriteLn
      END
   END ;
   IF Expecting#NulName
   THEN
      WriteString('Compiler expected ') ; WriteKey(Expecting)
   END ;
   WriteLn ;
   NearTokens('parameter mismatch', First, GetTokenNo()) ;
   EndError
END FailParameter ;


(*
   StartBuildFormalParameters - Initialises the quadruple stack for
                                Formal Parameters.

                                The Stack:

                                Entry                Exit

                                                                    <- Ptr
                                                     +------------+
                                Empty                | 0          |
                                                     |------------|
*)

PROCEDURE StartBuildFormalParameters ;
BEGIN
   PushT(0)
END StartBuildFormalParameters ;


(*
   EndBuildFormalParameters - Resets the quadruple stack after building
                              Formal Parameters.

                              The Stack:

                              Entry                    Exit

                       Ptr ->
                              +------------+
                              | NoOfParam  |                          <- Ptr
                              |------------|           +------------+
                              | ProcSym    |           | ProcSym    |
                              |------------|           |------------|
*)

PROCEDURE EndBuildFormalParameters ;
VAR
   NoOfPar  : CARDINAL ;
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfPar) ;
   PopT(ProcSym) ;
   PushT(ProcSym) ;
   Assert(IsProcedure(ProcSym)) ;
   IF NoOfParam(ProcSym)#NoOfPar
   THEN
      IF CompilingDefinitionModule()
      THEN
         WriteErrorFormat1('smaller number of procedure (%s) parameters in the DEFINITION MODULE', GetSymName(ProcSym))
      ELSE
         WriteErrorFormat1('larger number of procedure (%s) parameters in the DEFINIION MODULE', GetSymName(ProcSym))
      END
   END
END EndBuildFormalParameters ;


(*
   BuildFunction - Builds a procedures return type.
                   Procedure becomes a function.

                    The Stack:

                    Entry                 Exit

             Ptr ->
                    +------------+
                    | TypeSym    |                       <- Ptr
                    |------------|        +------------+
                    | ProcSym    |        | ProcSym    |
                    |------------|        |------------|
*)

PROCEDURE BuildFunction ;
VAR
   TypeSym,
   ProcSym : CARDINAL ;
BEGIN
   PopT(TypeSym) ;
   PopT(ProcSym) ;
   PutFunction(ProcSym, TypeSym) ;
(*
   WriteString('Procedure ') ; WriteKey(GetSymName(ProcSym)) ;
   WriteString(' has a return argument ') ;
   WriteKey(GetSymName(TypeSym)) ;
   WriteString(' checking ') ; WriteKey(GetSymName(GetType(ProcSym))) ;
   WriteLn ;
*)
   PushT(ProcSym)
END BuildFunction ;


(*
   BuildNulParam - Builds a nul parameter on the stack.
                   The Stack:

                   Entry             Exit

                                                    <- Ptr
                   Empty             +------------+
                                     | 0          |
                                     |------------|
*)

PROCEDURE BuildNulParam ;
BEGIN
   PushT(0)
END BuildNulParam ;


(*
   BuildPointerType - builds a pointer type.
                      The Stack:

                      Entry                       Exit
                      =====                       ====


               Ptr ->                                             <- Ptr
                      +------------+              +-------------+
                      | Type       |              | PointerType |
                      |------------|              |-------------|
                      | Name       |              | Name        |
                      |------------|              |-------------|
*)

PROCEDURE BuildPointerType ;
VAR
   Type,
   Name,
   PtrToType: CARDINAL ;
BEGIN
   PopT(Type) ;
   PopT(Name) ;
   PtrToType := MakePointer(Name) ;
   PutPointer(PtrToType, Type) ;
   CheckForExportedImplementation(PtrToType) ;   (* May be an exported hidden type *)
   PushT(Name) ;
   PushT(PtrToType)
(*
 ; WriteKey(Name) ; WriteString(' Pointer made') ; WriteLn
*)
END BuildPointerType ;


(*
   BuildSetType - builds a set type.
                  The Stack:

                  Entry                       Exit
                  =====                       ====


           Ptr ->                                             <- Ptr
                  +------------+              +-------------+
                  | Type       |              | SetType     |
                  |------------|              |-------------|
                  | Name       |              | Name        |
                  |------------|              |-------------|
*)

PROCEDURE BuildSetType ;
VAR
   Type,
   Name,
   SetType: CARDINAL ;
BEGIN
   PopT(Type) ;
   PopT(Name) ;
   SetType := MakeSet(Name) ;
   CheckForExportedImplementation(SetType) ;   (* May be an exported hidden type *)
   PutSet(SetType, Type) ;
   PushT(Name) ;
   PushT(SetType)
END BuildSetType ;


(*
   BuildRecord - Builds a record type.
                 The Stack:

                 Entry                        Exit
                 =====                        ====

                                                            <- Ptr
                                              +-----------+
          Ptr ->                              | RecordSym |
                 +------------+               |-----------|
                 | Name       |               | Name      |
                 |------------|               |-----------|
*)

PROCEDURE BuildRecord ;
VAR
   Name      : CARDINAL ;
   RecordType: CARDINAL ;
BEGIN
   PopT(Name) ;
   PushT(Name) ;
   RecordType := MakeRecord(Name) ;
   CheckForExportedImplementation(RecordType) ;   (* May be an exported hidden type *)
   PushT(RecordType)
(* ; WriteKey(Name) ; WriteString(' RECORD made') ; WriteLn *)
END BuildRecord ;


(*
   BuildFieldRecord - Builds a field into a record sym.
                      The Stack:


                      Entry                     Exit
                      =====                     ====

               Ptr ->
                      +-------------+
                      | Type | Name |
                      |-------------|
                      | n           |
                      |-------------|
                      | Id 1        |
                      |-------------|
                      .             .
                      .             .
                      .             .
                      |-------------|
                      | Id n        |                           <- Ptr
                      |-------------|           +-------------+
                      | RecordSym   |           | RecordSym   |
                      |-------------|           |-------------|
                      | RecordName  |           | RecordName  |
                      |-------------|           |-------------|
*)

PROCEDURE BuildFieldRecord ;
VAR
   Parent,
   Type, Name,
   NoOfFields,
   Record,
   Ptr, i    : CARDINAL ;
BEGIN
   PopTF(Type, Name) ;
   PopT(NoOfFields) ;
   GetPtr(Ptr) ;
   DEC(Ptr, NoOfFields+1) ;  (* Ptr points to the record sym *)
   Record := Operand(Ptr) ;
   IF IsRecord(Record)
   THEN
      Parent := Record
   ELSE
      (* Record maybe VarientRecord *)
      Parent := Father(Record)
   END ;
   i := 1 ;
   WHILE i<=NoOfFields DO
(*
      WriteKey(Operand(Ptr+i)) ; WriteString(' is a Field with type ') ;
      WriteKey(GetSymName(Type)) ; WriteLn ;
*)
      IF GetLocalSym(Parent, Operand(Ptr+i))=NulSym
      THEN
         PutFieldRecord(Record, Operand(Ptr+i), Type)
      ELSE
         IF GetSymName(Parent)=NulName
         THEN
            WriteErrorFormat1('field %s is already present inside record', Operand(Ptr+i))
         ELSE
            WriteErrorFormat2('field %s is already present inside record %s', Operand(Ptr+i), GetSymName(Parent))
         END
      END ;
      INC(i)
   END ;
   PutPtr(Ptr) ;
   PushT(Record)
(*  ; WriteString('Field Placed in record') ; WriteLn *)
END BuildFieldRecord ;


(*
   BuildVarientSelector - Builds a field into a record sym.
                          The Stack:


                          Entry                     Exit
                          =====                     ====

                   Ptr ->
                          +-------------+
                          | Qualident   |
                          |-------------|
                          | Ident       |                           <- Ptr
                          |-------------|           +-------------+
                          | RecordSym   |           | RecordSym   |
                          |-------------|           |-------------|
*)

PROCEDURE BuildVarientSelector ;
VAR
   Qualident,
   Ident    : CARDINAL ;
BEGIN
   PopT(Qualident) ;
   PushT(1) ;  (* Number of Idents *)
   PushTF(Qualident, GetSymName(Qualident)) ;
   BuildFieldRecord
END BuildVarientSelector ;


(*
   StartBuildVarientFieldRecord - Builds a varient field into a varient sym.
                                  The Stack:


                                  Entry                     Exit
                                  =====                     ====

                                                                       <- Ptr
                                                       +-------------+
                      Ptr ->                           | VarientField|
                             +-------------+           |-------------|
                             | VarientSym  |           | RecordSym   |
                             |-------------|           |-------------|
*)

PROCEDURE StartBuildVarientFieldRecord ;
VAR
   FieldSym,
   VarientSym: CARDINAL ;
BEGIN
   PopT(VarientSym) ;
   FieldSym := MakeFieldVarient(VarientSym) ;
   PushT(VarientSym) ;
   PushT(FieldSym)
END StartBuildVarientFieldRecord ;


(*
   EndBuildVarientFieldRecord - Removes a varient field from the stack.
                                The Stack:


                                Entry                     Exit
                                =====                     ====

                         Ptr ->
                                +-------------+
                                | VarientField|                           <- Ptr
                                |-------------|           +-------------+
                                | VarientSym  |           | VarientSym  |
                                |-------------|           |-------------|
*)

PROCEDURE EndBuildVarientFieldRecord ;
VAR
   FieldSym: CARDINAL ;
BEGIN
   PopT(FieldSym)
END EndBuildVarientFieldRecord ;


(*
   StartBuildVarient - Builds a varient symbol on top of a record sym.
                       The Stack:


                       Entry                     Exit
                       =====                     ====

                                                                 <- Ptr
                                                 +-------------+
                Ptr ->                           | VarientSym  |
                       +-------------+           |-------------|
                       | RecordSym   |           | RecordSym   |
                       |-------------|           |-------------|
                       | RecordName  |           | RecordName  |
                       |-------------|           |-------------|
*)

PROCEDURE StartBuildVarient ;
VAR
   RecordSym,
   Sym      : CARDINAL ;
BEGIN
   PopT(RecordSym) ;
   Sym := MakeVarient(RecordSym) ;
   PushT(RecordSym) ;
   PushT(Sym)
END StartBuildVarient ;



(*
   EndBuildVarient - Removes the varient symbol from the stack.
                     The Stack:

                     Entry                     Exit
                     =====                     ====

              Ptr ->
                     +-------------+
                     | VarientSym  |                           <- Ptr
                     |-------------|           +-------------+
                     | RecordSym   |           | RecordSym   |
                     |-------------|           |-------------|
                     | RecordName  |           | RecordName  |
                     |-------------|           |-------------|
*)

PROCEDURE EndBuildVarient ;
VAR
   Sym: CARDINAL ;
BEGIN
   PopT(Sym)
END EndBuildVarient ;


(*
   BuildNulName - Pushes a NulName onto the top of the stack.
                  The Stack:


                  Entry                    Exit

                                                          <- Ptr
                  Empty                    +------------+
                                           | NulName   |
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


(*
   StartBuildArray - Builds an array type.
                     The Stack:

                     Entry                        Exit
                     =====                        ====

                                                                <- Ptr
                                                  +-----------+
              Ptr ->                              | ArraySym  |
                     +------------+               |-----------|
                     | Name       |               | Name      |
                     |------------|               |-----------|
*)

PROCEDURE StartBuildArray ;
VAR
   Name     : CARDINAL ;
   ArrayType: CARDINAL ;
BEGIN
   PopT(Name) ;
   PushT(Name) ;
   ArrayType := MakeArray(Name) ;
   CheckForExportedImplementation(ArrayType) ;   (* May be an exported hidden type *)
   PushT(ArrayType)
(* ; WriteKey(Name) ; WriteString(' ARRAY made') ; WriteLn *)
END StartBuildArray ;


(*
   EndBuildArray - Builds an array type.
                   The Stack:

                   Entry                        Exit
                   =====                        ====

            Ptr ->
                   +------------+
                   | TypeSym    |                              <- Ptr
                   |------------|               +------------+
                   | ArraySym   |               | ArraySym   |
                   |------------|               |------------|
                   | Name       |               | Name       |
                   |------------|               |------------|
*)

PROCEDURE EndBuildArray ;
VAR
   TypeSym,
   ArraySym: CARDINAL ;
BEGIN
   PopT(TypeSym) ;
   PopT(ArraySym) ;
   Assert(IsArray(ArraySym)) ;
   PutArray(ArraySym, TypeSym) ;
   PushT(ArraySym)
END EndBuildArray ;


(*
   BuildFieldArray - Builds a field into an array sym.
                     The Stack:


                     Entry                     Exit
                     =====                     ====

              Ptr ->
                     +-------------+
                     | Type | Name |                           <- Ptr
                     |-------------|           +-------------+
                     | ArraySym    |           | ArraySym    |
                     |-------------|           |-------------|
                     | ArrayName   |           | ArrayName   |
                     |-------------|           |-------------|
*)

PROCEDURE BuildFieldArray ;
VAR
   Str       : ARRAY [0..12] OF CHAR ;
   Subrange,
   Subscript,
   Type, Name,
   Array     : CARDINAL ;
BEGIN
   PopTF(Type, Name) ;
   PopT(Array) ;
   Assert(IsArray(Array)) ;
   Subscript := MakeSubscript() ;
   IF IsEnumeration(Type)
   THEN
      (* We must now create a subrange type based upon the enumeration type *)
      Subrange := MakeSubrange(NulName) ;
      IF NoOfElements(Type)=0
      THEN
         WriteError('cannot create an array with 0 elements')
      ELSE
         CardToStr(NoOfElements(Type)-1, 0, Str) ;
         PutSubrange(Subrange,
                     MakeConstLit(MakeKey('0')), MakeConstLit(MakeKey(Str)),
                     Type) ;
         PutSubscript(Subscript, Subrange)
      END
   ELSE
      (*
         Would like to Assert(IsSubrange(Type)) - but unfortunately
         the subrange type might be declared later on in the file.
         Hence we take it in 'faith' at this point - check later in
         the M2SymBuild and M2CodeGen pass.
      *)
      PutSubscript(Subscript, Type)
   END ;
   PutFieldArray(Array, Subscript) ;
   PushT(Array)
(* ; WriteString('Field Placed in Array') ; WriteLn *)
END BuildFieldArray ;



(*
   BuildProcedureType - builds a procedure type symbol.
                        The Stack:


                                                               <- Ptr
                                               +-------------+
                 Ptr ->                        | ProcTypeSym |
                        +-------------+        |-------------|
                        | Name        |        | Name        |
                        |-------------|        |-------------|
*)

PROCEDURE BuildProcedureType ;
VAR
   Name,
   ProcTypeSym: CARDINAL ;
BEGIN
   PopT(Name) ;
   ProcTypeSym := MakeProcType(Name) ;
   PushT(Name) ;
   PushT(ProcTypeSym)
END BuildProcedureType ;


(*
   BuildFormalType - Builds a Formal Parameter in a procedure type.

                     The Stack:

                     Entry                 Exit

              Ptr ->
                     +------------+
                     | TypeSym    |
                     |------------|
                     | Array/Nul  |
                     |------------|
                     | Var / Nul  |                         <- Ptr
                     |------------|        +--------------+
                     | ProcTypeSym|        | ProcTypeSym  |
                     |------------|        |--------------|
*)

PROCEDURE BuildFormalType ;
VAR
   TypeSym,
   UnboundedSym,
   Array,
   Ptr,
   ProcTypeSym,
   Var        : CARDINAL ;
BEGIN
   PopT(TypeSym) ;
   PopT(Array) ;
   PopT(Var) ;
   PopT(ProcTypeSym) ;

   Assert( (Array=ArrayTok) OR (Array=NulTok) ) ;
   Assert(IsProcType(ProcTypeSym)) ;
   Assert( (Var=VarTok) OR (Var=NulTok) ) ;

   IF Array=ArrayTok
   THEN
      UnboundedSym := MakeUnbounded() ;
      PutUnbounded(UnboundedSym, TypeSym) ;
      TypeSym := UnboundedSym
   END ;
   IF Var=VarTok
   THEN
      (* VAR pamarater *)
      PutProcTypeVarParam(ProcTypeSym, TypeSym)
   ELSE
      (* Non VAR parameter *)
      PutProcTypeParam(ProcTypeSym, TypeSym)
   END ;
   PushT(ProcTypeSym)
END BuildFormalType ;


(*
   BuildPriority - give a module a constant priority.

                   The Stack:

                   Entry                 Exit

            Ptr ->
                   +------------+
                   | ConstSym   |                         <- Ptr
                   |------------|        +--------------+
                   | ModuleName |        | ModuleName   |
                   |------------|        |--------------|
*)

PROCEDURE BuildPriority ;
VAR
   ConstSym,
   ModuleName: CARDINAL ;
BEGIN
   PopT(ConstSym) ;
   PopT(ModuleName) ;
   PushT(ModuleName) ;
   InternalError('not implemented yet - to implement place a PutModulePriority and GetModulePriority into SymbolTable', __FILE__, __LINE__)
END BuildPriority ;


(*
   StartBuildingConstDeclaration - 
*)

PROCEDURE StartBuildingConstDeclaration ;
BEGIN
   IsBuildingConstDeclaration := TRUE
END StartBuildingConstDeclaration ;


(*
   EndBuildingConstDeclaration - 
*)

PROCEDURE EndBuildingConstDeclaration ;
BEGIN
   IsBuildingConstDeclaration := FALSE
END EndBuildingConstDeclaration ;


BEGIN
   IsBuildingConstDeclaration := FALSE
END P2SymBuild.
