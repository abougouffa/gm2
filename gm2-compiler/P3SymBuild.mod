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
IMPLEMENTATION MODULE P3SymBuild ;


FROM NameKey IMPORT Name, WriteKey, NulName ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM M2Debug IMPORT Assert, WriteDebug ;
FROM M2Error IMPORT WriteFormat0, WriteFormat1, WriteFormat2, FlushErrors ;

FROM SymbolTable IMPORT NulSym,
                        StartScope, EndScope, GetScope,
                        SetCurrentModule, GetCurrentModule, SetFileModule,
                        GetExported,
                        IsDefImp, IsModule,
                        RequestSym,
                        IsProcedure,
                        CheckForUnknownInModule,
                        GetFromOuterModule,
                        CheckForEnumerationInCurrentModule,
                        PutSubrange,
                        GetSymName ;

FROM M2Batch IMPORT MakeDefinitionSource,
                    MakeImplementationSource,
                    MakeProgramSource ;

FROM M2Quads IMPORT PushT, PopT, OperandT, PopN ;

FROM M2Comp IMPORT CompilingDefinitionModule,
                   CompilingImplementationModule,
                   CompilingProgramModule ;

FROM FifoQueue IMPORT GetFromFifoQueue ;
FROM M2Reserved IMPORT ImportTok ;


(*
   StartBuildDefinitionModule - Creates a definition module and starts
                                a new scope.

                                The Stack is expected:

                                Entry                 Exit

                         Ptr ->                                     <- Ptr
                                +------------+        +-----------+
                                | NameStart  |        | NameStart |
                                |------------|        |-----------|

*)

PROCEDURE P3StartBuildDefModule ;
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
END P3StartBuildDefModule ;


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

PROCEDURE P3EndBuildDefModule ;
VAR
   NameStart,
   NameEnd  : CARDINAL ;
BEGIN                                 
   Assert(CompilingDefinitionModule()) ;
   CheckForUnknownInModule ;
   EndScope ;
   PopT(NameEnd) ;
   PopT(NameStart) ;
   IF NameStart#NameEnd
   THEN
      WriteFormat2('inconsistant definition module was named (%a) and concluded as (%a)', NameStart, NameEnd)
   END
END P3EndBuildDefModule ;


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

PROCEDURE P3StartBuildImpModule ;
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
END P3StartBuildImpModule ;


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

PROCEDURE P3EndBuildImpModule ;
VAR
   NameStart,
   NameEnd  : Name ;
BEGIN
   Assert(CompilingImplementationModule()) ;
   CheckForUnknownInModule ;
   EndScope ;
   PopT(NameEnd) ;
   PopT(NameStart) ;
   IF NameStart#NameEnd
   THEN
      (* we dont issue an error based around incorrect module names as this is done in P1 and P2.
         If we get here then something has gone wrong with our error recovery in P3, so we bail out.
      *)
      WriteFormat0('too many errors in pass 3') ;
      FlushErrors
   END
END P3EndBuildImpModule ;


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

PROCEDURE P3StartBuildProgModule ;
VAR
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   (* WriteString('StartBuildProgramModule') ; WriteLn ; *)
   PopT(name) ;
   ModuleSym := MakeProgramSource(name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   (* WriteString('MODULE - ') ; WriteKey(GetSymName(ModuleSym)) ; WriteLn ; *)
   StartScope(ModuleSym) ;
   Assert(CompilingProgramModule()) ;
   Assert(NOT IsDefImp(ModuleSym)) ;
   PushT(name)
END P3StartBuildProgModule ;


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

PROCEDURE P3EndBuildProgModule ;
VAR
   NameStart,
   NameEnd  : Name ;
BEGIN
   Assert(CompilingProgramModule()) ;
   CheckForUnknownInModule ;
   EndScope ;
   PopT(NameEnd) ;
   PopT(NameStart) ;
   IF NameStart#NameEnd
   THEN
      (* we dont issue an error based around incorrect module names this would be done in P1 and P2.
         If we get here then something has gone wrong with our error recovery in P3, so we bail out.
      *)
      WriteFormat0('too many errors in pass 3') ;
      FlushErrors
   END
END P3EndBuildProgModule ;


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
   ModuleSym := RequestSym(name) ;
   Assert(IsModule(ModuleSym)) ;
   StartScope(ModuleSym) ;
   Assert(NOT IsDefImp(ModuleSym)) ;
   SetCurrentModule(ModuleSym) ;
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
   CheckForUnknownInModule ;
   EndScope ;
   PopT(NameEnd) ;
   PopT(NameStart) ;
   IF NameStart#NameEnd
   THEN
      (* we dont issue an error based around incorrect module names this would be done in P1 and P2.
         If we get here then something has gone wrong with our error recovery in P3, so we bail out.
      *)
      WriteFormat0('too many errors in pass 3') ;
      FlushErrors
   END ;
   SetCurrentModule(GetScope(GetCurrentModule()))
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
      WriteFormat0('not allowed to import using FROM in an inner module')
   END ;
   PopN(n+1)   (* Clear Stack *)
END BuildImportInnerModule ;


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
   name    : Name ;
   ProcSym : CARDINAL ;
BEGIN
   PopT(name) ;
   PushT(name) ;  (* Name saved for the EndBuildProcedure name check *)
   ProcSym := RequestSym(name) ;
   Assert(IsProcedure(ProcSym)) ;
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
      (* we dont issue an error based around incorrect module names this would be done in P1 and P2.
         If we get here then something has gone wrong with our error recovery in P3, so we bail out.
      *)
      WriteFormat0('too many errors in pass 3') ;
      FlushErrors
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
   BuildSubrange - Builds a Subrange type Symbol.


                      Stack

                      Entry                 Exit

               Ptr ->
                      +------------+
                      | High       |
                      |------------|
                      | Low        |                       <- Ptr
                      |------------|
*)

PROCEDURE BuildSubrange ;
VAR
   Type,
   Low,
   High: CARDINAL ;
BEGIN
   PopT(High) ;
   PopT(Low) ;
   GetFromFifoQueue(Type) ;  (* Collect subrange type from pass 2 and fill in *)
                             (* bounds.                                       *)
(*
   WriteString('Subrange type name is: ') ; WriteKey(GetSymName(Type)) ; WriteLn ;
   WriteString('Subrange High is: ') ; WriteKey(GetSymName(High)) ;
   WriteString(' Low is: ') ; WriteKey(GetSymName(Low)) ; WriteLn ;
*)
   PutSubrange(Type, Low, High, NulSym)   (* Base is worked out in M2EvalSym *)
END BuildSubrange ;


(*
   BuildNulName - Pushes a NulKey onto the top of the stack.
                  The Stack:


                  Entry                    Exit

                                                          <- Ptr
                  Empty                    +------------+
                                           | NulKey     |
                                           |------------|
*)

PROCEDURE BuildNulName ;
BEGIN
   PushT(NulName)
END BuildNulName ;


(*
   BuildConst - builds a constant.
                Stack

                Entry                 Exit

         Ptr ->                                      <- Ptr
                +------------+        +------------+
                | Name       |        | Sym        |
                |------------+        |------------|
*)

PROCEDURE BuildConst ;
VAR
   name: Name ;
   Sym : CARDINAL ;
BEGIN
   PopT(name) ;
   Sym := RequestSym(name) ;
   PushT(Sym)
END BuildConst ;


END P3SymBuild.
