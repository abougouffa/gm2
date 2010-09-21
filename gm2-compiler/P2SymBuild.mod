(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                 2010
   Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE P2SymBuild ;


FROM libc IMPORT strlen ;
FROM NameKey IMPORT Name, MakeKey, makekey, KeyToCharStar, NulName, LengthKey, WriteKey ;
FROM StrLib IMPORT StrEqual ;
FROM M2Debug IMPORT Assert, WriteDebug ;
FROM M2LexBuf IMPORT GetTokenNo ;
FROM M2Error IMPORT InternalError, WriteFormat1, WriteFormat2, WriteFormat0, ErrorStringAt2, WarnStringAt, ErrorStringAt ;
FROM M2MetaError IMPORT MetaError1, MetaError2, MetaErrorsT2, MetaErrors1, MetaErrors2 ;
FROM DynamicStrings IMPORT String, InitString, InitStringCharStar, Mark, Slice, ConCat, KillString, string ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf4 ;
FROM M2Printf IMPORT printf0, printf1, printf2 ;
FROM M2StackWord IMPORT StackOfWord, InitStackWord, PushWord, PopWord ;
FROM M2Options IMPORT PedanticParamNames, ExtendedOpaque ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM M2Base IMPORT ZType ;
FROM Storage IMPORT ALLOCATE ;

FROM M2Reserved IMPORT ImportTok, ExportTok, QualifiedTok, UnQualifiedTok,
                       NulTok, VarTok, ArrayTok ;

FROM FifoQueue IMPORT GetEnumerationFromFifoQueue, PutSubrangeIntoFifoQueue,
                      PutConstructorIntoFifoQueue, PutConstIntoFifoQueue ;

FROM SymbolTable IMPORT NulSym,
                        ModeOfAddr,
                        StartScope, EndScope, PseudoScope,
                        GetCurrentScope, GetScope,
                        IsDeclaredIn,
                        SetCurrentModule, SetFileModule,
                        GetCurrentModule, GetMainModule,
                        MakeTemporary, CheckAnonymous, IsNameAnonymous,
                        MakeConstLit,
                        MakeConstLitString,
                        MakeEnumeration, MakeSubrange,
                        MakeVar, MakeType, PutType,
                        PutMode,
                        PutFieldEnumeration, PutSubrange, PutVar, PutConst,
                        PutConstSet, PutConstructor,
                        IsDefImp, IsType, IsRecord, IsRecordField, IsPointer,
                        IsSubrange, IsEnumeration, IsConstString,
                        IsError, IsAModula2Type,
                        GetSym, GetDeclareSym, IsUnknown, RenameSym,
                        GetLocalSym, GetParent, IsRecord, GetRecord,
                        GetFromOuterModule,
                        GetExported,
                        PutExported, PutExportQualified, PutExportUnQualified,
                        PutExportUnImplemented,
                        PutFieldVarient, GCFieldVarient, PutVarientTag,
                        IsFieldVarient, IsVarient,
                        CheckForEnumerationInCurrentModule,
                        CheckForExportedImplementation,
                        MakeProcedure,
                        PutFunction, PutOptFunction,
                        PutParam, PutVarParam,
                        GetNthParam,
                        IsProcedure,
                        NoOfElements,
                        MakePointer, PutPointer,
      	          	MakeSet, PutSet,
                        MakeRecord, PutFieldRecord,
                        MakeVarient, MakeFieldVarient,
                        MakeArray, PutArraySubscript,
                        MakeSubscript, PutSubscript,
                        PutConstString, GetString,
                        PutArray, IsArray,
                        GetType, SkipType,
                        IsProcType, MakeProcType,
                        PutProcTypeVarParam, PutProcTypeParam,
                        MakeConstVar,
                        PutVariableAtAddress, IsVariableAtAddress,
                        GetAlignment, PutAlignment,
                        MakeUnbounded, IsUnbounded,
                        NoOfParam,
                        PutParamName,
                        GetParam, GetDimension,
                        AreParametersDefinedInDefinition,
                        AreParametersDefinedInImplementation,
                        AreProcedureParametersDefined,
                        ParametersDefinedInDefinition,
                        ParametersDefinedInImplementation,
                        ProcedureParametersDefined,
                        CheckForUnImplementedExports,
                        CheckForUndeclaredExports,
                        IsHiddenTypeDeclared,
                        IsUnboundedParam,
                        IsVarParam,
                        PutUseVarArgs,
                        UsesVarArgs,
                        PutUseOptArg,
                        UsesOptArg,
                        IsDefinitionForC,
                        GetSymName,
                        GetDeclared,
                        RequestSym,
                        DisplayTrees ;

FROM M2Batch IMPORT MakeDefinitionSource,
                    MakeImplementationSource,
                    MakeProgramSource ;

FROM M2Quads IMPORT PushT, PopT,
                    PushTF, PopTF,
                    OperandT, OperandF, OperandA, PopN, DisplayStack,
                    AddVarientFieldToList ;

FROM M2Comp IMPORT CompilingDefinitionModule,
                   CompilingImplementationModule,
                   CompilingProgramModule ;



(* %%%FORWARD%%%
PROCEDURE BuildFormalParameterSection ; FORWARD ;
PROCEDURE BuildNulParam ; FORWARD ;
PROCEDURE CheckFormalParameterSection ; FORWARD ;
PROCEDURE FailParameter (CurrentState : ARRAY OF CHAR;
                         PreviousState: ARRAY OF CHAR;
                         Given        : Name ;
                         ParameterNo  : CARDINAL;
                         ProcedureSym : CARDINAL) ; FORWARD ;
PROCEDURE findConstMeta (sym: CARDINAL) : constType ; FORWARD ;
PROCEDURE findConstType (sym: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE fixupConstCast (sym: CARDINAL; castType: CARDINAL) ; FORWARD ;
PROCEDURE fixupConstMeta (sym: CARDINAL; meta: constType) ; FORWARD ;
PROCEDURE addToConstList (sym: CARDINAL) ; FORWARD ;
   %%%FORWARD%%% *)

CONST
   Debugging   = FALSE ;
   DebugConsts = FALSE ;

TYPE
   constType = (unknown, set, str, constructor, array, cast) ;

   constList = POINTER TO cList ;
   cList     = RECORD
                  constsym : CARDINAL ;
                  constmeta: constType ;
                  expr     : CARDINAL ;
                  type     : CARDINAL ;
                  next     : constList ;
               END ;

VAR
   alignTypeNo       : CARDINAL ;
   castType          : CARDINAL ;
   type              : constType ;
   RememberedConstant: CARDINAL ;
   RememberStack,
   TypeStack         : StackOfWord ;
   headOfConsts      : constList ;


PROCEDURE stop ; BEGIN END stop ;

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
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(name) ;
   ModuleSym := MakeDefinitionSource(name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(IsDefImp(ModuleSym)) ;
   Assert(CompilingDefinitionModule()) ;
   PushT(name)
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
   NameEnd  : Name ;
BEGIN                                 
   Assert(CompilingDefinitionModule()) ;
   CheckForUndeclaredExports(GetCurrentModule()) ;
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF Debugging
   THEN
      printf0('pass 2: ') ;
      DisplayTrees(GetCurrentModule())
   END ;
   IF NameStart#NameEnd
   THEN
      WriteFormat2('inconsistant definition module name, module began as (%a) and ended with (%a)', NameStart, NameEnd)
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
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(name) ;
   ModuleSym := MakeImplementationSource(name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(IsDefImp(ModuleSym)) ;
   Assert(CompilingImplementationModule()) ;
   PushT(name)
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
   NameEnd  : Name ;
BEGIN
   Assert(CompilingImplementationModule()) ;
   CheckForUnImplementedExports ;
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF NameStart#NameEnd
   THEN
      WriteFormat1('inconsistant implementation module name %a', NameStart)
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
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(name) ;
   ModuleSym := MakeProgramSource(name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(CompilingProgramModule()) ;
   Assert(NOT IsDefImp(ModuleSym)) ;
   PushT(name) ;
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
   NameEnd  : Name ;
BEGIN
   Assert(CompilingProgramModule()) ;
   CheckForUndeclaredExports(GetCurrentModule()) ;  (* Not really allowed exports here though! *)
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF Debugging
   THEN
      printf0('pass 2: ') ;
      DisplayTrees(GetCurrentModule())
   END ;
   IF NameStart#NameEnd
   THEN
      WriteFormat2('inconsistant program module name %a does not match %a', NameStart, NameEnd)
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
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(name) ;
   ModuleSym := GetDeclareSym(name) ;
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
   CheckForUndeclaredExports(GetCurrentModule()) ;
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF NameStart#NameEnd
   THEN
      WriteFormat2('inconsistant inner module name %a does not match %a',
                   NameStart, NameEnd)
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
   IF OperandT(n+1)#ImportTok
   THEN
      (* Ident List contains list of objects *)
      ModSym := MakeDefinitionSource(OperandT(n+1)) ;
      i := 1 ;
      WHILE i<=n DO
         Sym := GetExported(ModSym, OperandT(i)) ;
         CheckForEnumerationInCurrentModule(Sym) ;
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
   n: CARDINAL ;
BEGIN
   PopT(n) ;       (* n   = # of the Ident List *)
   PopN(n+1)
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

                            Exit

                            All above stack discarded
*)

PROCEDURE BuildImportInnerModule ;
VAR
   Sym, ModSym,
   n, i       : CARDINAL ;
BEGIN
   PopT(n) ;       (* i   = # of the Ident List *)
   IF OperandT(n+1)=ImportTok
   THEN
      (* Ident List contains list of objects *)
      i := 1 ;
      WHILE i<=n DO
         Sym := GetFromOuterModule(OperandT(i)) ;
         CheckForEnumerationInCurrentModule(Sym) ;
         INC(i)
      END
   ELSE
      (* Ident List contains list of objects *)
      ModSym := MakeDefinitionSource(OperandT(n+1)) ;
      i := 1 ;
      WHILE i<=n DO
         Sym := GetExported(ModSym, OperandT(i)) ;
         CheckForEnumerationInCurrentModule(Sym) ;
         INC(i)
      END
   END ;
   PopN(n+1)   (* Clear Stack *)
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
   n: CARDINAL ;
BEGIN
   PopT(n) ;
   PopN(n+1)   (* clear stack               *)
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
   name: Name ;
   Sym : CARDINAL ;
BEGIN
   PopT(name) ;
   Sym := MakeConstLit(name) ;
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
   name: Name ;
   Sym : CARDINAL ;
BEGIN
   PopT(name) ;
   (* slice off the leading and trailing quotes *)
   Sym := MakeConstLitString(makekey(string(Mark(Slice(Mark(InitStringCharStar(KeyToCharStar(name))), 1, -1))))) ;
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
   name: Name ;
   sym : CARDINAL ;
BEGIN
   PopT(name) ;
   sym := MakeConstVar(name) ;
   PushT(sym) ;
   RememberConstant(sym) ;
   addToConstList(sym)
END BuildConst ;


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
   PopT(n) ;       (* n := # *)
   name := OperandT(n+1) ;
   GetEnumerationFromFifoQueue(Type) ;
   CheckForExportedImplementation(Type) ;   (* May be an exported hidden type *)
   PopN(n) ;
   PushT(Type)
END StartBuildEnumeration ;


(*
   BuildSubrange - Builds a Subrange type Symbol, the base type can also be
                   supplied if known.

                      Stack

                      Entry                 Exit


                                                           <- Ptr
                                            +------------+
               Ptr ->                       | Type       |
                      +------------+        |------------|
                      | Name       |        | Name       |
                      |------------|        |------------| 
*)

PROCEDURE BuildSubrange (Base: CARDINAL) ;
VAR
   name: Name ;
   Type: CARDINAL ;
BEGIN
   PopT(name) ;
   Type := MakeSubrange(name) ;
   PutSubrangeIntoFifoQueue(Type) ;   (* Store Subrange away so that we can fill in *)
                                      (* its bounds during pass 3.                  *)
   PutSubrangeIntoFifoQueue(Base) ;   (* store Base type of subrange away as well.  *)
   CheckForExportedImplementation(Type) ; (* May be an exported hidden type *)
   PushT(name) ;
   PushT(Type)
END BuildSubrange ;


(*
   BuildAligned - builds an alignment constant symbol which is placed onto
                  the stack.  It expects the ident ALIGNED to be on the
                  stack.

                         Stack

                           Entry                 Exit


                  Ptr ->                                            <- Ptr
                         +------------+        +-----------------+
                         | ALIGNED    |        | AlignmentConst  |
                         +------------+        |-----------------|
*)

PROCEDURE BuildAligned ;
VAR
   name : Name ;
   align: CARDINAL ;
BEGIN
   PopT(name) ;
   IF name#MakeKey('ALIGNED')
   THEN
      WriteFormat1('expecting ALIGNED identifier, rather than %a', name)
   END ;
   align := MakeTemporary(ImmediateValue) ;
   PutConst(align, ZType) ;
   PutConstIntoFifoQueue(align) ;     (* Store align away so that we can fill in its  *)
   PushT(align)                       (* value during pass 3.                         *)
END BuildAligned ;


(*
   BuildTypeAlignment - the AlignmentConst is either a temporary or NulSym.
                        In the case of NulSym it is popped from the stack
                        and the procedure returns.  Otherwise the temporary
                        is popped and recorded as the alignment value for this
                        type.  A type may only have one alignment value and
                        error checking is performed.

                        Stack

                            Entry                 Exit


                    Ptr ->
                           +-----------------+
                           | AlignmentConst  |
                           |-----------------|
                           | Type            |    Empty
                           |-----------------|
*)

PROCEDURE BuildTypeAlignment ;
VAR
   type,
   align: CARDINAL ;
BEGIN
   PopT(align) ;
   PopT(type) ;
   IF align#NulSym
   THEN
      IF IsRecord(type) OR IsRecordField(type) OR IsType(type) OR IsArray(type) OR IsPointer(type)
      THEN
         PutAlignment(type, align)
      ELSE
         MetaError1('not allowed to add an alignment attribute to type {%1ad}', type)
      END
   END
END BuildTypeAlignment ;


(*
   BuildVarAlignment - the AlignmentConst is either a temporary or NulSym.
                       A type may only have one alignment value and
                       error checking is performed.

                       Stack

                           Entry                 Exit


                   Ptr ->
                          +-----------------+
                          | AlignmentConst  |                             <- Ptr
                          |-----------------|        +------------------+
                          | Type            |        | Type  | TypeName |
                          |-----------------|        |------------------|
*)

PROCEDURE BuildVarAlignment ;
VAR
   name,
   newname: Name ;
   new,
   type,
   align  : CARDINAL ;
   s      : String ;
BEGIN
   PopT(align) ;
   IF align#NulSym
   THEN
      PopT(type) ;
      IF IsRecord(type) OR IsRecordField(type) OR IsType(type) OR IsArray(type) OR IsPointer(type)
      THEN
         stop ;
         IF IsNameAnonymous(type)
         THEN
            PutAlignment(type, align) ;
            PushTF(type, GetSymName(type))
         ELSE
            (* create a pseudonym *)
            s := Sprintf1(Mark(InitString('_$A%d')), alignTypeNo) ;
            INC(alignTypeNo) ;
            newname := makekey(string(s)) ;
            IF IsPointer(type)
            THEN
               new := MakePointer(newname)
            ELSE
               new := MakeType(newname)
            END ;
            s := KillString(s) ;
            PutType(new, type) ;
            PutAlignment(new, align) ;
            PushTF(new, GetSymName(new))
         END
      ELSE
         MetaError1('not allowed to add an alignment attribute to type {%1ad}', type) ;
         PushTF(type, GetSymName(type))
      END
   END
END BuildVarAlignment ;


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
   name     : Name ;
   AtAddress,
   Type,
   Var,
   i, n     : CARDINAL ;
BEGIN
   PopTF(Type, name) ;
   PopT(n) ;
   i := 1 ;
   WHILE i<=n DO
      Var := MakeVar(OperandT(n+1-i)) ;
      AtAddress := OperandA(n+1-i) ;
      IF AtAddress#NulSym
      THEN
         PutVariableAtAddress(Var, NulSym) ;
         PutMode(Var, LeftValue)
      END ;
      PutVar(Var, Type) ;
      INC(i)
   END ;
   PopN(n)
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
   isunknown: BOOLEAN ;
   n1, n2   : Name ;
   Sym,
   Type     : CARDINAL ;
   name     : Name ;
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
   IF name=NulName
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
      PushTF(Type, name)
   ELSIF IsError(Type)
   THEN
      PushTF(Sym, name)
   ELSIF GetSymName(Type)=name
   THEN
      isunknown := IsUnknown(Type) ; 
      IF isunknown OR
         (NOT IsDeclaredIn(GetCurrentScope(), Type))
      THEN
         Sym := MakeType(name) ;
         IF NOT IsError(Sym)
         THEN
            IF Sym=Type
            THEN
               IF isunknown
               THEN
                  MetaError2('attempting to declare a type {%1ad} to a type which is itself unknown {%2ad}',
                             Sym, Type)
               ELSE
                  MetaError1('attempting to declare a type {%1ad} as itself', Sym)
               END
            ELSE
               PutType(Sym, Type) ;
               CheckForExportedImplementation(Sym) ;    (* May be an exported hidden type *)
               (* if Type is an enumerated type then add its contents to the pseudo scope *)
               CheckForEnumerationInCurrentModule(Type)
            END
         END ;
         PushTF(Sym, name)
      ELSE
         PushTF(Type, name)
      END
   ELSE
      (* example   TYPE a = CARDINAL *)
      Sym := MakeType(name) ;
      PutType(Sym, Type) ;
      CheckForExportedImplementation(Sym) ;   (* May be an exported hidden type *)
      PushTF(Sym, name)
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
   name   : Name ;
   ProcSym: CARDINAL ;
BEGIN
   PopT(name) ;
   PushT(name) ;  (* name saved for the EndBuildProcedure name check *)
   IF name=323
   THEN
      stop
   END ;
   ProcSym := GetDeclareSym(name) ;
   IF ProcSym=411
   THEN
      stop
   END ;
   IF IsUnknown(ProcSym)
   THEN
      (*
         May have been compiled in the definition or implementation module,
         remember that implementation maybe compiled before corresponding
         definition module.
         - no definition should always be compilied before implementation modules.
      *)
      ProcSym := MakeProcedure(name)
   ELSE
      IF NOT IsProcedure(ProcSym)
      THEN
         ErrorStringAt2(Sprintf1(Mark(InitString('procedure name (%a) has been declared as another object elsewhere')),
                                 name), GetTokenNo(), GetDeclared(ProcSym))
      END
   END ;
   IF CompilingDefinitionModule()
   THEN
      PutExportUnImplemented(ProcSym)    (* Defined but not yet implemented *)
   ELSE
      CheckForExportedImplementation(ProcSym)   (* May be exported procedure *)
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
   NameStart: Name ;
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NameEnd) ;
   PopT(ProcSym) ;
   Assert(IsProcedure(ProcSym)) ;
   PopT(NameStart) ;
   IF NameEnd#NameStart
   THEN
      WriteFormat2('end procedure name does not match beginning %a name %a', NameStart, NameEnd)
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
      Assert(IsProcedure(ProcSym)) ;
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
   n         : Name ;
   ProcSym,
   ParamTotal: CARDINAL ;
BEGIN
   PopT(ParamTotal) ;
   ProcSym := CARDINAL(OperandT(3+CARDINAL(OperandT(3))+2)) ;
   PushT(ParamTotal) ;
   Assert(IsProcedure(ProcSym)) ;
   IF CompilingDefinitionModule()
   THEN
      IF AreParametersDefinedInDefinition(ProcSym) AND (ParamTotal=0)
      THEN
         n := GetSymName(ProcSym) ;
         WriteFormat1('cannot declare procedure %a twice in the definition module', n)
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
         n := GetSymName(ProcSym) ;
         WriteFormat1('cannot declare procedure %a twice in the implementation module', n)
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
         n := GetSymName(ProcSym) ;
         WriteFormat1('procedure %a parameters already declared in program module', n)
      ELSE
         BuildFormalParameterSection ;
         IF ParamTotal=0
         THEN
            ProcedureParametersDefined(ProcSym)
         END
      END
   ELSE
      InternalError('should never reach this point', __FILE__, __LINE__)
   END ;
   Assert(IsProcedure(OperandT(2)))
END BuildFPSection ;


(*
   BuildVarArgs - indicates that the ProcSym takes varargs
                  after ParamTotal.
                                                         <- Ptr
                    +------------+        +------------+
                    | ParamTotal |        | ParamTotal |
                    |------------|        |------------|
                    | ProcSym    |        | ProcSym    |
                    |------------|        |------------|

*)

PROCEDURE BuildVarArgs ;
VAR
   ProcSym,
   ParamTotal: CARDINAL ;
BEGIN
   PopT(ParamTotal) ;
   PopT(ProcSym) ;
   IF UsesOptArg(ProcSym)
   THEN
      WriteFormat0('procedure can use either a single optional argument or a single vararg section ... at the end of the formal parameter list')
   END ;
   IF UsesVarArgs(ProcSym)
   THEN
      WriteFormat0('procedure can only have one vararg section ... at the end of the formal parameter list')
   END ;
   PutUseVarArgs(ProcSym) ;
   IF IsDefImp(GetCurrentModule())
   THEN
      IF NOT IsDefinitionForC(GetCurrentModule())
      THEN
         WriteFormat0('the definition module must be declared as DEFINITION MODULE FOR "C" if varargs are to be used')
      END
   ELSE
      WriteFormat0('varargs can only be used in the module declared as DEFINITION MODULE FOR "C"')
   END ;
   PushT(ProcSym) ;
   PushT(ParamTotal)
END BuildVarArgs ;


(*
   BuildOptArg - indicates that the ProcSym takes a single optarg
                 after ParamTotal.

                                                         <- Ptr
                 +------------+        +------------+
                 | ParamTotal |        | ParamTotal |
                 |------------|        |------------|
                 | ProcSym    |        | ProcSym    |
                 |------------|        |------------|
*)

PROCEDURE BuildOptArg ;
VAR
   ProcSym,
   ParamTotal: CARDINAL ;
BEGIN
   PopT(ParamTotal) ;
   PopT(ProcSym) ;
   IF UsesVarArgs(ProcSym)
   THEN
      WriteFormat0('procedure can not use an optional argument after a vararg ...')
   END ;
   PutUseOptArg(ProcSym) ;
   PushT(ProcSym) ;
   PushT(ParamTotal)
END BuildOptArg ;


(*
   BuildFormalVarArgs - indicates that the procedure type takes varargs.

                                                             <- Ptr
                        +------------+        +------------+
                        | ProcSym    |        | ProcSym    |
                        |------------|        |------------|

*)

PROCEDURE BuildFormalVarArgs ;
VAR
   ProcSym: CARDINAL ;
BEGIN
   PopT(ProcSym) ;
   IF UsesVarArgs(ProcSym)
   THEN
      WriteFormat0('procedure type can only have one vararg section ... at the end of the formal parameter list')
   END ;
   PutUseVarArgs(ProcSym) ;
   IF IsDefImp(GetCurrentModule())
   THEN
      IF NOT IsDefinitionForC(GetCurrentModule())
      THEN
         WriteFormat0('the definition module must be declared as DEFINITION MODULE FOR "C" if varargs are to be used')
      END
   ELSE
      WriteFormat0('varargs can only be used in the module declared as DEFINITION MODULE FOR "C"')
   END ;
   PushT(ProcSym)
END BuildFormalVarArgs ;


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
   ParamName,
   Var,
   Array     : Name ;
   ParamTotal,
   TypeSym,
   UnBoundedSym,
   NoOfIds,
   ProcSym,
   i, ndim   : CARDINAL ;
BEGIN
   PopT(ParamTotal) ;
   PopT(TypeSym) ;
   PopTF(Array, ndim) ;
   Assert( (Array=ArrayTok) OR (Array=NulTok) ) ;
   PopT(NoOfIds) ;
   ProcSym := OperandT(NoOfIds+2) ;
   Assert(IsProcedure(ProcSym)) ;
   Var := OperandT(NoOfIds+1) ;
   Assert( (Var=VarTok) OR (Var=NulTok) ) ;
   IF Array=ArrayTok
   THEN
      UnBoundedSym := MakeUnbounded(TypeSym, ndim) ;
      TypeSym := UnBoundedSym
   END ;
   i := 1 ;
(*
   WriteString('Procedure ') ; WriteKey(GetSymName(ProcSym)) ;
   WriteString(' adding No. of identifiers:') ; WriteCard(NoOfIds, 4) ; WriteLn ;
*)
   WHILE i<=NoOfIds DO
      IF CompilingDefinitionModule() AND (NOT PedanticParamNames) AND
         (* we will see the parameters in the implementation module *)
         ((GetMainModule()=GetCurrentModule()) OR
          (IsHiddenTypeDeclared(GetCurrentModule()) AND ExtendedOpaque))
      THEN
         ParamName := NulName
      ELSE
         ParamName := OperandT(NoOfIds+1-i)
      END ;
      IF Var=VarTok
      THEN
         (* VAR parameter *)
         IF NOT PutVarParam(ProcSym, ParamTotal+i, ParamName, TypeSym, Array=ArrayTok)
         THEN
            InternalError('problems adding a VarParameter - wrong param #?', __FILE__, __LINE__)
         END
      ELSE
         (* Non VAR parameter *)
         IF NOT PutParam(ProcSym, ParamTotal+i, ParamName, TypeSym, Array=ArrayTok)
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
   PopN(NoOfIds+1) ;
   PushT(ParamTotal+NoOfIds) ;
   Assert(IsProcedure(OperandT(2)))
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
   Array, Var: Name ;
   Unbounded : BOOLEAN ;
   ParamI,
   ParamIType,
   ParamTotal,
   TypeSym,
   NoOfIds,
   ProcSym,
   pi, i, ndim: CARDINAL ;
BEGIN
   PopT(ParamTotal) ;
   PopT(TypeSym) ;
   PopTF(Array, ndim) ;
   Assert( (Array=ArrayTok) OR (Array=NulTok) ) ;
   PopT(NoOfIds) ;
   ProcSym := OperandT(NoOfIds+2) ;
   Assert(IsProcedure(ProcSym)) ;
   Var := OperandT(NoOfIds+1) ;
   Assert( (Var=VarTok) OR (Var=NulTok) ) ;
   Unbounded := (Array=ArrayTok) ;  (* ARRAY OF Type, parameter *)
   i := 1 ;
   pi := NoOfIds ;     (* stack index referencing stacked parameter, i *)
(*
   WriteString('No. of identifiers:') ; WriteCard(NoOfIds, 4) ; WriteLn ;
*)
   WHILE i<=NoOfIds DO
      IF ParamTotal+i<=NoOfParam(ProcSym)
      THEN
         IF Unbounded AND (NOT IsUnboundedParam(ProcSym, ParamTotal+i))
         THEN
            FailParameter('the parameter was declared as an ARRAY OF type',
                          'the parameter was not declared as an ARRAY OF type',
                          NulName, ParamTotal+i, ProcSym)
         ELSIF (NOT Unbounded) AND IsUnboundedParam(ProcSym, ParamTotal+i)
         THEN
            FailParameter('the parameter was not declared as an ARRAY OF type',
                          'the parameter was declared as an ARRAY OF type',
                          NulName, ParamTotal+i, ProcSym)
         END ;
         IF Unbounded
         THEN
            IF GetDimension(GetNthParam(ProcSym, ParamTotal+1))#ndim
            THEN
               FailParameter('', 'the dynamic array parameter was declared with different number of dimensions',
                             NulName, ParamTotal+i, ProcSym)
            END
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
         IF PedanticParamNames
         THEN
            IF GetSymName(ParamI)#OperandT(pi)
            THEN
               (* different parameter names *)
               FailParameter('',
                             'the parameter has been declared with a different name',
                             OperandT(pi), ParamTotal+i, ProcSym)
            END
         ELSE
            IF GetSymName(ParamI)=NulName
            THEN
               PutParamName(ProcSym, ParamTotal+i, OperandT(pi))
            END
         END ;
         IF Unbounded
         THEN
            (* GetType(ParamI) yields an UnboundedSym or a PartialUnboundedSym,
               depending whether it has been resolved.. *)
            ParamIType := GetType(GetType(ParamI))
         ELSE
            ParamIType := GetType(ParamI)
         END ;
         IF ((SkipType(ParamIType)#SkipType(TypeSym)) OR
             (PedanticParamNames AND (ParamIType#TypeSym))) AND
            (NOT IsUnknown(SkipType(TypeSym))) AND
            (NOT IsUnknown(SkipType(ParamIType)))
         THEN
            (* different parameter types *)
            FailParameter('',
                          'the parameter has been declared with a different type',
                          OperandT(pi), ParamTotal+i, ProcSym)
         END
      ELSE
         FailParameter('too many parameters',
                       'fewer parameters were declared',
                       NulName, ParamTotal+i, ProcSym)
      END ;
      INC(i) ;
      DEC(pi)
   END ;
   PopN(NoOfIds+1) ;   (* +1 for the Var/Nul *)
   PushT(ParamTotal+NoOfIds) ;
   Assert(IsProcedure(OperandT(2)))
END CheckFormalParameterSection ;


(*
   FailParameter - generates an error message indicating that a parameter
                   declaration has failed.

                   The parameters are:

                   CurrentState  - string describing the current failing state.
                   PreviousState - string describing the old defined state.
                   Given         - token or identifier that was given.
                   ParameterNo   - parameter number that has failed.
                   ProcedureSym  - procedure symbol where parameter has failed.

                   If any parameter is Nul then it is ignored.
*)

PROCEDURE FailParameter (CurrentState : ARRAY OF CHAR;
                         PreviousState: ARRAY OF CHAR;
                         Given        : Name ;
                         ParameterNo  : CARDINAL;
                         ProcedureSym : CARDINAL) ;
VAR
   First,
   Second      : CARDINAL ;
   FirstModule,
   SecondModule,
   s1, s2, s3  : String ;
BEGIN
   IF NoOfParam(ProcedureSym)>=ParameterNo
   THEN
      First := GetDeclared(GetNthParam(ProcedureSym, ParameterNo))
   ELSE
      (* ParameterNo does not exist - which is probably the reason why this routine was called.. *)
      First := GetDeclared(ProcedureSym)
   END ;
   IF CompilingDefinitionModule()
   THEN
      FirstModule := InitString('definition module') ;
      SecondModule := InitString('implementation module')
   ELSIF CompilingImplementationModule()
   THEN
      FirstModule := InitString('implementation module') ;
      SecondModule := InitString('definition module')
   ELSIF CompilingProgramModule()
   THEN
      FirstModule := InitString('program module') ;
      SecondModule := InitString('definition module')
   END ;
   s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(ProcedureSym)))) ;
   s3 := Mark(FirstModule) ;
   s1 := Sprintf4(Mark(InitString('declaration of procedure %s in the %s differs from the %s, problem with parameter number %d')),
                  s2, s3,
                  SecondModule,
                  ParameterNo) ;
   IF NoOfParam(ProcedureSym)>=ParameterNo
   THEN
      s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetNthParam(ProcedureSym, ParameterNo))))) ;
      s1 := ConCat(s1, Mark(Sprintf1(Mark(InitString(' (%s)')), s2)))
   END ;
   IF NOT StrEqual(CurrentState, '')
   THEN
      s2 := Mark(InitString(CurrentState)) ;
      s1 := ConCat(s1, Mark(Sprintf1(Mark(InitString(', %s')), s2)))
   END ;
   IF NOT StrEqual(PreviousState, '')
   THEN
      s2 := Mark(SecondModule) ;
      s3 := Mark(InitString(PreviousState)) ;
      s1 := ConCat(s1, Mark(Sprintf2(Mark(InitString(' in the %s %s')), s2, s3)))
   END ;
   IF Given#NulName
   THEN
      s2 := Mark(InitStringCharStar(KeyToCharStar(Given))) ;
      s1 := ConCat(s1, Mark(Sprintf1(Mark(InitString(' (%s)')), s2)))
   END ;
   s1 := ConCat(s1, Mark(Sprintf0(Mark(InitString('\n'))))) ;
   ErrorStringAt2(s1, GetTokenNo(), First)
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
   n      : Name ;
   NoOfPar: CARDINAL ;
   ProcSym: CARDINAL ;
BEGIN
   PopT(NoOfPar) ;
   PopT(ProcSym) ;
   PushT(ProcSym) ;
   Assert(IsProcedure(ProcSym)) ;
   IF NoOfParam(ProcSym)#NoOfPar
   THEN
      n := GetSymName(ProcSym) ;
      IF CompilingDefinitionModule()
      THEN
         WriteFormat1('procedure (%a) was declared with fewer parameters in the DEFINITION MODULE', n)
      ELSE
         WriteFormat1('procedure (%a) was declared with more parameters in the DEFINITION MODULE', n)
      END
   END ;
   Assert(IsProcedure(OperandT(1)))
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
   PrevSym,
   TypeSym,
   ProcSym : CARDINAL ;
BEGIN
   PopT(TypeSym) ;
   PopT(ProcSym) ;
   IF IsProcedure(ProcSym) AND AreProcedureParametersDefined(ProcSym)
   THEN
      PrevSym := GetType(ProcSym) ;
      IF (PrevSym#NulSym) AND (PrevSym#TypeSym)
      THEN
         IF CompilingDefinitionModule()
         THEN
            MetaErrorsT2(GetDeclared(ProcSym),
                         'the return type for procedure {%1a} is defined differently in the definition module as {%1tad} and the implementation module as {%2ad}',
                         'the return type for procedure {%1a} is defined differently in the definition module as {%1tad} and the implementation module as {%2ad}',
                         ProcSym, TypeSym)
         ELSE
            MetaErrorsT2(GetDeclared(ProcSym),
                         'the return type for procedure {%1a} is defined differently in the definition module as {%2ad} and the implementation module as {%1tad}',
                         'the return type for procedure {%1a} is defined differently in the definition module as {%2ad} and the implementation module as {%1tad}',
                         ProcSym, TypeSym)
         END
      END
   END ;
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
   BuildOptFunction - Builds a procedures optional return type.
                      Procedure becomes a function and the user
                      can either call it as a function or a procedure.

                      The Stack:

                      Entry                 Exit

               Ptr ->
                      +------------+
                      | TypeSym    |                       <- Ptr
                      |------------|        +------------+
                      | ProcSym    |        | ProcSym    |
                      |------------|        |------------|
*)

PROCEDURE BuildOptFunction ;
VAR
   TypeSym,
   ProcSym : CARDINAL ;
BEGIN
   PopT(TypeSym) ;
   PopT(ProcSym) ;
   PutOptFunction(ProcSym, TypeSym) ;
(*
   WriteString('Procedure ') ; WriteKey(GetSymName(ProcSym)) ;
   WriteString(' has a return argument ') ;
   WriteKey(GetSymName(TypeSym)) ;
   WriteString(' checking ') ; WriteKey(GetSymName(GetType(ProcSym))) ;
   WriteLn ;
*)
   PushT(ProcSym)
END BuildOptFunction ;


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
   name     : Name ;
   NewType,
   Type,
   PtrToType: CARDINAL ;
BEGIN
   PopT(Type) ;
   PopT(name) ;
   name := CheckAnonymous(name) ;
(*
   PtrToType := MakePointer(NulName) ;
   PutPointer(PtrToType, Type) ;
   NewType := MakeType(name) ;
   PutType(NewType, PtrToType) ;
   CheckForExportedImplementation(NewType) ;   (* May be an exported hidden type *)
   PushT(name) ;
   PushT(NewType) ;
*)

   PtrToType := MakePointer(name) ;
   PutPointer(PtrToType, Type) ;
   CheckForExportedImplementation(PtrToType) ;   (* May be an exported hidden type *)
   PushT(name) ;
   PushT(PtrToType)

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
   name   : Name ;
   Type,
   SetType: CARDINAL ;
BEGIN
   PopT(Type) ;
   PopT(name) ;
   SetType := MakeSet(name) ;
   CheckForExportedImplementation(SetType) ;   (* May be an exported hidden type *)
   PutSet(SetType, Type) ;
   PushT(name) ;
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
   name      : Name ;
   RecordType: CARDINAL ;
BEGIN
   PopT(name) ;
   PushT(name) ;
   name := CheckAnonymous(name) ;
   RecordType := MakeRecord(name) ;
   CheckForExportedImplementation(RecordType) ;   (* May be an exported hidden type *)
   PushT(RecordType)
(* ; WriteKey(name) ; WriteString(' RECORD made') ; WriteLn *)
END BuildRecord ;


(*
   BuildFieldRecord - Builds a field into a record sym.
                      The Stack:


                      Entry                     Exit
                      =====                     ====

               Ptr ->
                      +-------------+
                      | Alignment   |
                      |-------------|
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
   name,
   n1, n2    : Name ;
   align,
   fsym,
   Field,
   Varient,
   Parent,
   Type,
   NoOfFields,
   Record,
   Ptr, i    : CARDINAL ;
BEGIN
   PopT(align) ;
   PopTF(Type, name) ;
   PopT(NoOfFields) ;
   Record := OperandT(NoOfFields+1) ;
   IF IsRecord(Record)
   THEN
      Parent := Record ;
      Varient := NulSym
   ELSE
      (* Record maybe FieldVarient *)
      Parent := GetRecord(GetParent(Record)) ;
      Assert(IsFieldVarient(Record)) ;
      Varient := OperandT(NoOfFields+2) ;
      Assert(IsVarient(Varient)) ;
      PutFieldVarient(Record, Varient) ;
      IF Debugging
      THEN
         n1 := GetSymName(Record) ;
         WriteString('Record ') ;
         WriteKey(n1) ;
         WriteString(' has varient ') ;
         n1 := GetSymName(Varient) ;
         WriteKey(n1) ; WriteLn
      END
   END ;
   i := 1 ;
   WHILE i<=NoOfFields DO
      IF Debugging
      THEN
         n1 := GetSymName(Record) ;
         WriteString('Record ') ;
         WriteKey(n1) ;
         WriteString('  ') ;
         WriteKey(OperandT(NoOfFields+1-i)) ; WriteString(' is a Field with type ') ;
         WriteKey(GetSymName(Type)) ; WriteLn ;
      END ;
      fsym := GetLocalSym(Parent, OperandT(NoOfFields+1-i)) ;
      IF fsym=NulSym
      THEN
         Field := PutFieldRecord(Record, OperandT(NoOfFields+1-i), Type, Varient) ;
         IF align#NulSym
         THEN
            PutAlignment(Field, align)
         END
      ELSE
         MetaErrors2('record field {%1ad} has already been declared inside a {%2Dd} {%2a}',
                     'attempting to declare a duplicate record field', fsym, Parent)
      END ;
      INC(i)
   END ;
   PopN(NoOfFields+1) ;
   PushT(Record)
END BuildFieldRecord ;


(*
   BuildVarientSelector - Builds a field into a record sym.
                          The Stack:


                          Entry                     Exit
                          =====                     ====

                   Ptr ->
                          +-------------+
                          | Type        |
                          |-------------|
                          | Tag         |                           <- Ptr
                          |-------------|           +-------------+
                          | RecordSym   |           | RecordSym   |
                          |-------------|           |-------------|
*)

PROCEDURE BuildVarientSelector ;
VAR
   tag       : Name ;
   Field,
   Type,
   Varient,
   Parent,
   VarField,
   NoOfFields,
   Record    : CARDINAL ;
BEGIN
   PopT(Type) ;
   PopT(tag) ;
   PopT(Record) ;
   PushT(Record) ;  (* and restore stack ready for exiting *)
   IF IsRecord(Record)
   THEN
      Parent := Record ;
      Varient := NulSym ;
      InternalError('not expecting a record symbol', __FILE__, __LINE__)   (* is this really true.. *)
   ELSIF IsVarient(Record)
   THEN
      Varient := Record ;
      VarField := GetParent(Varient) ;
      IF (Type=NulSym) AND (tag=NulName)
      THEN
         MetaError1('expecting a tag field in the declaration of a varient record {%1Ua}', Record)
      ELSIF Type=NulSym
      THEN
         PutVarientTag(Varient, RequestSym(tag))
      ELSE
         Field := PutFieldRecord(VarField, tag, Type, Varient) ;
         PutVarientTag(Varient, Field) ;
         IF Debugging
         THEN
            WriteString('varient field ') ; WriteKey(GetSymName(VarField)) ;
            WriteString('varient ') ; WriteKey(GetSymName(Varient)) ; WriteLn
         END
      END
   ELSE
      (* Record maybe FieldVarient *)
      Parent := GetParent(Record) ;
      Assert(IsFieldVarient(Record)) ;
      Varient := OperandT(1+2) ;
      Assert(IsVarient(Varient)) ;
      PutFieldVarient(Record, Varient) ;
      IF Debugging
      THEN
         WriteString('record ') ; WriteKey(GetSymName(Record)) ;
         WriteString('varient ') ; WriteKey(GetSymName(Varient)) ; WriteLn
      END ;
      IF (Type=NulSym) AND (tag=NulName)
      THEN
         MetaError1('expecting a tag field in the declaration of a varient record {%1Ua}', Record)
      ELSIF Type=NulSym
      THEN
         PutVarientTag(Varient, RequestSym(tag))
      ELSE
         Field := PutFieldRecord(Record, tag, Type, Varient) ;
         PutVarientTag(Varient, Field) ;
         IF Debugging
         THEN
            WriteString('record ') ; WriteKey(GetSymName(Record)) ;
            WriteString('varient ') ; WriteKey(GetSymName(Varient)) ; WriteLn
         END
      END
   END
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
                             | VarientSym  |           | VarientSym  |
                             |-------------|           |-------------|
*)

PROCEDURE StartBuildVarientFieldRecord ;
VAR
   VarientSym,
   FieldSym  : CARDINAL ;
BEGIN
   PopT(VarientSym) ;
   FieldSym := MakeFieldVarient(CheckAnonymous(NulName), VarientSym) ;
   PushT(VarientSym) ;
   PushT(FieldSym) ;
   Assert(IsFieldVarient(FieldSym)) ;
   PutFieldVarient(FieldSym, VarientSym) ;
   AddVarientFieldToList(FieldSym)
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
   PopT(FieldSym) ;
   (* GCFieldVarient(FieldSym) *)
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
   Type: CARDINAL ;
   name: Name ;
BEGIN
   PopTF(Type, name)
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
   name     : Name ;
   ArrayType: CARDINAL ;
BEGIN
   PopT(name) ;
   PushT(name) ;
   ArrayType := MakeArray(name) ;
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
   d         : CARDINAL ;
   s         : String ;
   Subrange,
   Subscript,
   Type,
   Array     : CARDINAL ;
   name      : Name ;
BEGIN
   PopTF(Type, name) ;
   PopT(Array) ;
   Assert(IsArray(Array)) ;
   Subscript := MakeSubscript() ;
   (*
      We cannot Assert(IsSubrange(Type)) as the subrange type might be
      declared later on in the file.
      We also note it could be an ordinal type or enumerated type.
      Therefore we must save this information and deal with the
      different cases in M2GCCDeclare.mod and M2GenGCC.mod.
      However this works to our advantage as it preserves the
      actual declaration as specified by the source file.
   *)
   PutSubscript(Subscript, Type) ;
   PutArraySubscript(Array, Subscript) ;
   PushT(Array)
(* ; WriteString('Field Placed in Array') ; WriteLn *)
END BuildFieldArray ;


(*
   BuildArrayComma - converts ARRAY [..], [..] OF into ARRAY [..] OF ARRAY [..]


              Ptr ->                                           <- Ptr
                     +-------------+           +-------------+
                     | ArraySym1   |           | ArraySym2   |
                     |-------------|           |-------------|
                     | ArrayName   |           | ArrayName   |
                     |-------------|           |-------------|
*)

PROCEDURE BuildArrayComma ;
VAR
   Nothing,
   ArraySym1,
   ArraySym2: CARDINAL ;
BEGIN
   PushT(NulName) ;
   StartBuildArray ;
   PopT(ArraySym2) ;
   PopT(Nothing) ;
   PushT(ArraySym2) ;
   EndBuildArray ;
   PopT(ArraySym1) ;
   PushT(ArraySym2)
END BuildArrayComma ;


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
   name       : Name ;
   ProcTypeSym: CARDINAL ;
BEGIN
   PopT(name) ;
   ProcTypeSym := MakeProcType(name) ;
   PushT(name) ;
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
   Array, Var : Name ;
   TypeSym,
   UnboundedSym,
   ProcTypeSym: CARDINAL ;
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
      UnboundedSym := MakeUnbounded(TypeSym, 1) ;
      TypeSym := UnboundedSym
   END ;
   IF Var=VarTok
   THEN
      (* VAR parameter *)
      PutProcTypeVarParam(ProcTypeSym, TypeSym, IsUnbounded(TypeSym))
   ELSE
      (* Non VAR parameter *)
      PutProcTypeParam(ProcTypeSym, TypeSym, IsUnbounded(TypeSym))
   END ;
   PushT(ProcTypeSym)
END BuildFormalType ;


(*
   SaveRememberedConstructor - 
*)

PROCEDURE SaveRememberedConstructor ;
BEGIN
(*
   IF RememberedConstant=NulSym
   THEN
      RememberedConstant := MakeTemporary(ImmediateValue)
   END ;
   PutConstructorIntoFifoQueue(RememberedConstant)
*)

END SaveRememberedConstructor ;


(*
   GetSeenString - returns a string corresponding to, s.
*)

PROCEDURE GetSeenString (s: constType) : String ;
BEGIN
   CASE s OF

   unknown    : RETURN( InitString('unknown') ) |
   set        : RETURN( InitString('SET') ) |
   str        : RETURN( InitString('string') ) |
   constructor: RETURN( InitString('constructor') ) |
   array      : RETURN( InitString('ARRAY') ) |
   cast       : RETURN( InitStringCharStar(KeyToCharStar(GetSymName(castType))) ) |

   ELSE
      InternalError('unexpected value of type', __FILE__, __LINE__)
   END ;
   RETURN( NIL )
END GetSeenString ;


(*
   SetTypeTo - attempts to set, type, to, s.
*)

PROCEDURE SetTypeTo (s: constType) ;
VAR
   s1, s2, s3: String ;
BEGIN
   IF type=unknown
   THEN
      type := s
   ELSIF (type=constructor) AND (s#str)
   THEN
      type := s
   ELSIF (s=constructor) AND ((type=array) OR (type=set))
   THEN
      (* leave it alone *)
   ELSIF type#s
   THEN
      s1 := GetSeenString(type) ;
      s2 := GetSeenString(s) ;
      s3 := Sprintf2(InitString('cannot create a %s constant together with a %s constant'), s1, s2) ;
      ErrorStringAt(s3, GetTokenNo()) ;
      s1 := KillString(s1) ;
      s2 := KillString(s2)
   END
END SetTypeTo ;


(*
   SeenUnknown - sets the operand type to unknown.
*)

PROCEDURE SeenUnknown ;
BEGIN
   type := unknown
END SeenUnknown ;


(*
   SeenCast - sets the operand type to cast.
*)

PROCEDURE SeenCast (sym: CARDINAL) ;
BEGIN
   type := cast ;
   castType := sym ;
   Assert(IsAModula2Type(sym)) ;
END SeenCast ;


(*
   SeenSet - sets the operand type to set.
*)

PROCEDURE SeenSet ;
BEGIN
   SetTypeTo(set) ;
   SaveRememberedConstructor
END SeenSet ;


(*
   SeenArray - sets the operand type to array.
*)

PROCEDURE SeenArray ;
BEGIN
   SetTypeTo(array)
END SeenArray ;


(*
   SeenConstructor - sets the operand type to constructor.
*)

PROCEDURE SeenConstructor ;
BEGIN
   SetTypeTo(constructor) ;
   SaveRememberedConstructor
END SeenConstructor ;


(*
   SeenString - sets the operand type to string.
*)

PROCEDURE SeenString ;
BEGIN
   SetTypeTo(str)
END SeenString ;


(*
   DetermineType - assigns the top of stack symbol with the type of
                   constant expression, if known.
*)

PROCEDURE DetermineType ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := OperandT(1) ;
   fixupConstMeta(Sym, type) ;
   CASE type OF

   set        :  PutConstSet(Sym) |
   str        :  PutConstString(Sym, MakeKey('')) |
   array,
   constructor:  PutConstructor(Sym) |
   cast       :  PutConst(Sym, castType) ; fixupConstCast(Sym, castType) |
   unknown    :  

   ELSE
   END
END DetermineType ;


(*
   PushType - 
*)

PROCEDURE PushType ;
BEGIN
   PushWord(TypeStack, type)
END PushType ;


(*
   PopType - 
*)

PROCEDURE PopType ;
BEGIN
   type := PopWord(TypeStack)
END PopType ;


(*
   PushRememberConstant - 
*)

PROCEDURE PushRememberConstant ;
BEGIN
   PushWord(RememberStack, RememberedConstant) ;
   RememberConstant(NulSym)
END PushRememberConstant ;


(*
   PopRememberConstant - 
*)

PROCEDURE PopRememberConstant ;
BEGIN
   RememberedConstant := PopWord(RememberStack)
END PopRememberConstant ;


(*
   RememberConstant - 
*)

PROCEDURE RememberConstant (sym: CARDINAL) ;
BEGIN
   RememberedConstant := sym
END RememberConstant ;


(*
   addToConstList - add a constant, sym, to the head of the constants list.
*)

PROCEDURE addToConstList (sym: CARDINAL) ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      IF h^.constsym=sym
      THEN
         InternalError('should never see the same symbol id declared twice', __FILE__, __LINE__)
      END ;
      h := h^.next
   END ;
   NEW(h) ;
   WITH h^ DO
      constsym  := sym ;
      constmeta := unknown ;
      expr      := NulSym ;
      type      := NulSym ;
      next      := headOfConsts
   END ;
   headOfConsts := h
END addToConstList ;


(*
   FixupConstAsString - fixes up a constant, sym, which will have the string type.
*)

PROCEDURE FixupConstAsString (sym: CARDINAL) ;
BEGIN
   fixupConstMeta(sym, str)
END FixupConstAsString ;


(*
   FixupConstType - fixes up a constant, sym, which will have the type, consttype.
*)

PROCEDURE FixupConstType (sym: CARDINAL; consttype: CARDINAL) ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF constsym=sym
         THEN
            IF constmeta=str
            THEN
               InternalError('cannot fix up a constant to have a type if it is already known as a string', __FILE__, __LINE__)
            END ;
            type := consttype ;
            PutConst(sym, consttype) ;
            RETURN
         END
      END ;
      h := h^.next
   END
END FixupConstType ;


(*
   FixupConstExpr - fixes up a constant, sym, which will be equivalent to e.
*)

PROCEDURE FixupConstExpr (sym: CARDINAL; e: CARDINAL) ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF constsym=sym
         THEN
            expr := e ;
            RETURN
         END
      END ;
      h := h^.next
   END
END FixupConstExpr ;


(*
   fixupConstMeta - 
*)

PROCEDURE fixupConstMeta (sym: CARDINAL; meta: constType) ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF constsym=sym
         THEN
            constmeta := meta ;
            RETURN
         END
      END ;
      h := h^.next
   END
END fixupConstMeta ;


(*
   fixupConstCast - 
*)

PROCEDURE fixupConstCast (sym: CARDINAL; castType: CARDINAL) ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF constsym=sym
         THEN
            type := castType ;
            RETURN
         END
      END ;
      h := h^.next
   END
END fixupConstCast ;


(*
   findConstType - 
*)

PROCEDURE findConstType (sym: CARDINAL) : CARDINAL ;
VAR
   h: constList ;
   t: CARDINAL ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF constsym=sym
         THEN
            t := GetType(sym) ;
            IF t=NulSym
            THEN
               RETURN( type )
            ELSE
               RETURN( t )
            END
         END
      END ;
      h := h^.next
   END ;
   RETURN( NulSym )
END findConstType ;


(*
   findConstMeta - 
*)

PROCEDURE findConstMeta (sym: CARDINAL) : constType ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF constsym=sym
         THEN
            RETURN( constmeta )
         END
      END ;
      h := h^.next
   END ;
   RETURN( unknown )
END findConstMeta ;


(*
   ReportUnresolvedConstTypes - emits an error message for any unresolved constant type.
*)

PROCEDURE ReportUnresolvedConstTypes ;
VAR
   h: constList ;
BEGIN
   h := headOfConsts ;
   WHILE h#NIL DO
      WITH h^ DO
         IF (constmeta#unknown) AND (constmeta#str) AND (type=NulSym)
         THEN
            MetaError1('unable to resolve the type of the constant {%1Dad}', h^.constsym)
         END
      END ;
      h := h^.next
   END
END ReportUnresolvedConstTypes ;


(*
   ResolveConstTypes - resolves the types of all aggegrate constants.
*)

PROCEDURE ResolveConstTypes ;
VAR
   h      : constList ;
   changed: BOOLEAN ;
   n      : Name ;
BEGIN
   REPEAT
      changed := FALSE ;
      h := headOfConsts ;
      WHILE h#NIL DO
         WITH h^ DO
            IF (constmeta=unknown) OR (constmeta=str)
            THEN
               (* do nothing *)
            ELSE
               IF DebugConsts
               THEN
                  n := GetSymName(constsym) ;
                  printf1('constant %a ', n) ;
                  IF type=NulSym
                  THEN
                     printf0('type is unknown\n') ;
                  ELSE
                     printf0('type is known\n') ;
                  END
               END ;
               IF type=NulSym
               THEN
                  IF expr#NulSym
                  THEN
                     IF findConstMeta(expr)=str
                     THEN
                        changed := TRUE ;
                        PutConstString(constsym, MakeKey('')) ;
                        IF DebugConsts
                        THEN
                           n := GetSymName(constsym) ;
                           printf1('resolved constant %a as a string\n', n)
                        END
                     ELSE
                        type := findConstType(expr) ;
                        IF type#NulSym
                        THEN
                           changed := TRUE ;
                           PutConst(constsym, type) ;
                           IF DebugConsts
                           THEN
                              n := GetSymName(constsym) ;
                              printf1('resolved type of constant %a\n', n)
                           END
                        END
                     END
                  END
               END
            END
         END ;
         h := h^.next
      END
   UNTIL NOT changed ;
   ReportUnresolvedConstTypes
END ResolveConstTypes ;


(*
   SkipConst - returns the symbol which is a pseudonum of, sym.
*)

PROCEDURE SkipConst (sym: CARDINAL) : CARDINAL ;
VAR
   init: CARDINAL ;
   h   : constList ;
BEGIN
   init := sym ;
   h := headOfConsts ;
   WHILE h#NIL DO
      IF (h^.constsym=sym) AND (h^.expr#NulSym)
      THEN
         sym := h^.expr ;
         IF sym=init
         THEN
            (* circular definition found *)
            RETURN( sym )
         END ;
         h := headOfConsts
      ELSE
         h := h^.next
      END
   END ;
   RETURN( sym )
END SkipConst ;


BEGIN
   TypeStack := InitStackWord() ;
   RememberStack := InitStackWord() ;
   castType := NulSym ;
   alignTypeNo := 0 ;
   headOfConsts := NIL
END P2SymBuild.
