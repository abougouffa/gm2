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


FROM NameKey IMPORT WriteKey, NulName ;

FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;

FROM M2Debug IMPORT Assert, WriteDebug ;

FROM M2Lexical IMPORT WriteError, WriteErrorFormat1, WriteErrorFormat2 ;

FROM SymbolTable IMPORT NulSym,
                        StartScope, EndScope, GetScopeAuthor,
                        SetCurrentModule, GetCurrentModule, SetFileModule,
                        IsDefImp, IsModule,
                        RequestSym,
                        IsProcedure,
                        CheckForUnknownInModule,
                        PutSubrange,
                        GetSymName ;

FROM M2Batch IMPORT MakeDefinitionSource,
                    MakeImplementationSource,
                    MakeProgramSource ;

FROM M2Quads IMPORT PushT, PopT ;

FROM M2Comp IMPORT CompilingDefinitionModule,
                   CompilingImplementationModule,
                   CompilingProgramModule ;

FROM FifoQueue IMPORT GetFromFifoQueue ;


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
      WriteErrorFormat2('inconsistant definition module was named (%s) and concluded as (%s)', NameStart, NameEnd)
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
   NameEnd  : CARDINAL ;
BEGIN
   Assert(CompilingImplementationModule()) ;
   CheckForUnknownInModule ;
   EndScope ;
   PopT(NameEnd) ;
   PopT(NameStart) ;
   IF NameStart#NameEnd
   THEN
      WriteErrorFormat2('inconsistant implementation module was named (%s) and concluded as (%s)', NameStart, NameEnd)
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
   PushT(Name)
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
   NameEnd  : CARDINAL ;
BEGIN
   Assert(CompilingProgramModule()) ;
   CheckForUnknownInModule ;
   EndScope ;
   PopT(NameEnd) ;
   PopT(NameStart) ;
   IF NameStart#NameEnd
   THEN
      WriteErrorFormat2('inconsistant program module was named (%s) and concluded as (%s)', NameStart, NameEnd)
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
   Name     : CARDINAL ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(Name) ;
   ModuleSym := RequestSym(Name) ;
   Assert(IsModule(ModuleSym)) ;
   StartScope(ModuleSym) ;
   Assert(NOT IsDefImp(ModuleSym)) ;
   SetCurrentModule(ModuleSym) ;   (* --gaius--  16/10/97 *)
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
   CheckForUnknownInModule ;
   EndScope ;
   PopT(NameEnd) ;
   PopT(NameStart) ;
   IF NameStart#NameEnd
   THEN
      WriteErrorFormat2('inconsistant inner module was named (%s) and concluded as (%s)', NameStart, NameEnd)
   END ;
   (* --gaius--  16/10/97 *)
   SetCurrentModule(GetScopeAuthor(GetCurrentModule()))
END EndBuildInnerModule ;


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
   NameEnd,
   ProcSym,
   NameStart: CARDINAL ;
BEGIN
   PopT(NameEnd) ;
   PopT(ProcSym) ;
   PopT(NameStart) ;
   IF NameEnd#NameStart
   THEN
      WriteErrorFormat2('procedure name (%s) does not match end name (%s)', NameStart, NameEnd)
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
   Name,
   Sym : CARDINAL ;
BEGIN
   PopT(Name) ;
   Sym := RequestSym(Name) ;
   PushT(Sym)
END BuildConst ;


END P3SymBuild.
