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
IMPLEMENTATION MODULE P1Compile ;


FROM M2Debug IMPORT WriteDebug, Assert ;

FROM M2Quads IMPORT PushT, PopT,
                    StartBuildInit,
                    EndBuildInit,
                    BuildProcedureStart,
                    BuildProcedureEnd,
                    BuildAssignment,
                    GetPtr ;

FROM M2Lexical IMPORT TokenIs, WriteError ;
FROM M2Atom IMPORT Ident, PushAutoOff, PushAutoOn, PopAuto ;
FROM M2Reference IMPORT Qualident, IdentList ;
FROM P1Expression IMPORT ConstExpression ;
FROM P1Statement IMPORT StatementSequence ;
FROM P1Type IMPORT Type ;

FROM M2Reserved IMPORT EqualTok, SemiColonTok, ColonTok, CommaTok,
                       LParaTok, RParaTok, LSBraTok, RSBraTok,
                       BarTok, PeriodTok, PeriodPeriodTok,
                       CaseTok, OfTok, EndTok, ElseTok,
                       ModuleTok, DefinitionTok, ImplementationTok,
                       ProcedureTok, BeginTok, ConstTok, TypeTok,
                       VarTok, ArrayTok, ExportTok, QualifiedTok,
                       UnQualifiedTok,
                       FromTok, ImportTok,
                       NulTok ;

FROM P1SymBuild IMPORT P1StartBuildProgramModule,
                       P1EndBuildProgramModule,
                       P1StartBuildDefinitionModule,
                       P1EndBuildDefinitionModule,
                       P1StartBuildImplementationModule,
                       P1EndBuildImplementationModule,
                       StartBuildInnerModule,
                       EndBuildInnerModule,

                       BuildImportOuterModule,
                       BuildImportInnerModule,
                       BuildExportOuterModule,
                       BuildExportInnerModule,

                       BuildHiddenType,
                       BuildTypeEnd,
                       BuildNulName,

                       BuildProcedureHeading,
                       StartBuildProcedure,
                       EndBuildProcedure ;


(* %%%FORWARD%%%
PROCEDURE ProcedureBlock () : BOOLEAN ; FORWARD ;
PROCEDURE Block() : BOOLEAN ; FORWARD ;
PROCEDURE ConstantDeclaration() : BOOLEAN ; FORWARD ;
PROCEDURE Declaration() : BOOLEAN ; FORWARD ;
PROCEDURE Definition() : BOOLEAN ; FORWARD ;
PROCEDURE DefinitionModule() : BOOLEAN ; FORWARD ;
PROCEDURE Export() : BOOLEAN ; FORWARD ;
PROCEDURE FPSection() : BOOLEAN ; FORWARD ;
PROCEDURE FormalParameters() : BOOLEAN ; FORWARD ;
PROCEDURE FormalType() : BOOLEAN ; FORWARD ;
PROCEDURE ImplementationModule() : BOOLEAN ; FORWARD ;
PROCEDURE Import() : BOOLEAN ; FORWARD ;
PROCEDURE Module() : BOOLEAN ; FORWARD ;
PROCEDURE ModuleDeclaration() : BOOLEAN ; FORWARD ;
PROCEDURE NonVarFPSection() : BOOLEAN ; FORWARD ;
PROCEDURE Priority() : BOOLEAN ; FORWARD ;
PROCEDURE ProcedureDeclaration() : BOOLEAN ; FORWARD ;
PROCEDURE ProcedureHeading() : BOOLEAN ; FORWARD ;
PROCEDURE TypeDeclaration() : BOOLEAN ; FORWARD ;
PROCEDURE VarFPSection() : BOOLEAN ; FORWARD ;
PROCEDURE VariableDeclaration() : BOOLEAN ; FORWARD ;
   %%%FORWARD%%% *)


PROCEDURE ConstantDeclaration () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF Ident()
   THEN
      IF TokenIs(EqualTok)
      THEN
         IF ConstExpression()
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('ConstExpression - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('= - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ConstantDeclaration ;

PROCEDURE TypeDeclaration () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
   c1, c2: CARDINAL ;
BEGIN
   GetPtr(c1) ;
   PushAutoOn ;  (* ******** *)
   IF Ident()
   THEN
      IF TokenIs(EqualTok)
      THEN
         IF Type()
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('Type - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('= - expected')
      END
   ELSE
      Success := FALSE
   END ;
   PopAuto ;  (* ******** *)
   GetPtr(c2) ;
   Assert(c1=c2) ;
   RETURN( Success )
END TypeDeclaration ;

PROCEDURE VariableDeclaration () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
   Ptr, PtrC: CARDINAL ;
BEGIN
   GetPtr(Ptr) ;
   IF IdentList()
   THEN
      IF TokenIs(ColonTok)
      THEN
         BuildNulName ;  (* ******** *)
         IF Type()
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('Type - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError(': - expected')
      END
   ELSE
      Success := FALSE
   END ;
   GetPtr(PtrC) ;
   Assert(Ptr=PtrC) ;
   RETURN( Success )
END VariableDeclaration ;

PROCEDURE ProcedureDeclaration () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF ProcedureHeading()
   THEN
      IF TokenIs(SemiColonTok)
      THEN
         IF ProcedureBlock()
         THEN
            PushAutoOn ;  (* ******** *)
            IF Ident()
            THEN
               EndBuildProcedure ;  (* Symbol Table Generation ******** *)
               Success := TRUE
            ELSE
               Success := FALSE ;
               WriteError('Ident - expected')
            END ;
            PopAuto  (* ******** *)
         ELSE
            Success := FALSE ;
            WriteError('Block - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('; - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ProcedureDeclaration ;

PROCEDURE ProcedureHeading () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(ProcedureTok)
   THEN
      PushAutoOn ;  (* ******** *)
      IF Ident()
      THEN
         StartBuildProcedure ;  (* Symbol Table Generation ******** *)
         PushAutoOff ;  (* ******** *)
         IF FormalParameters()
         THEN
         END ;
         PopAuto ;  (* ******** *)
         BuildProcedureHeading ;  (* Symbol Table Generation ******** *)
         Success := TRUE
      ELSE
         Success := FALSE ;
         WriteError('Ident - expected')
      END ;
      PopAuto  (* ******** *)
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ProcedureHeading ;


(*
   ProcedureBlock - introduced to catch programming errors.
                    Specifically the Block as defined by N. Wirth
                    allows BEGIN to be optional. This is fine for
                    mature programmers, but causes confusion when
                    students fail to use BEGIN when implementing a
                    PROCEDURE. The simplist way around this is to
                    have two kinds of Blocks, one for modules and
                    one for procedures.
*)

PROCEDURE ProcedureBlock () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   WHILE Declaration() DO
   END ;
   IF TokenIs(BeginTok)
   THEN
      IF StatementSequence()
      THEN
      ELSE
         WriteError('StatementSequence - expected')
      END ;
      IF TokenIs(EndTok)
      THEN
         Success := TRUE
      ELSE
         Success := FALSE ;
         WriteError('END - expected, or possibly a misformed block')
      END
   ELSE
      WriteError('BEGIN - expected, or possibly a misformed block')
   END ;
   RETURN( Success )
END ProcedureBlock ;

PROCEDURE Block () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   WHILE Declaration() DO
   END ;
   IF TokenIs(BeginTok)
   THEN
      IF StatementSequence()
      THEN
      ELSE
         WriteError('StatementSequence - expected')
      END
   END ;
   IF TokenIs(EndTok)
   THEN
      Success := TRUE
   ELSE
      Success := FALSE ;
      WriteError('END - expected, or possibly a misformed block')
   END ;
   RETURN( Success )
END Block ;

PROCEDURE Declaration () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(ConstTok)
   THEN
      WHILE ConstantDeclaration() DO
         IF TokenIs(SemiColonTok)
         THEN
         ELSE
            WriteError('; - expected')
         END
      END ;
      Success := TRUE
   ELSIF TokenIs(TypeTok)
   THEN
      WHILE TypeDeclaration() DO
         IF TokenIs(SemiColonTok)
         THEN
         ELSE
            WriteError('; - expected')
         END
      END ;
      Success := TRUE
   ELSIF TokenIs(VarTok)
   THEN
      WHILE VariableDeclaration() DO
         IF TokenIs(SemiColonTok)
         THEN
         ELSE
            WriteError('; - expected')
         END
      END ;
      Success := TRUE
   ELSIF ProcedureDeclaration()
   THEN
      IF TokenIs(SemiColonTok)
      THEN
         Success := TRUE
      ELSE
         Success := FALSE ;
         WriteError('; - expected')
      END
   ELSIF ModuleDeclaration()
   THEN
      IF TokenIs(SemiColonTok)
      THEN
         Success := TRUE
      ELSE
         Success := FALSE ;
         WriteError('; - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Declaration ;

PROCEDURE FormalParameters () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(LParaTok)
   THEN
      IF FPSection()
      THEN
         WHILE TokenIs(SemiColonTok) DO
            IF FPSection()
            THEN
            ELSE
               IF TokenIs(RParaTok)
               THEN
                  WriteError('remove ; before ) to make it comply with the formal parameter syntax')
               ELSE
                  WriteError('FPSection - expected')
               END
            END
         END
      END ;
      IF TokenIs(RParaTok)
      THEN
         IF TokenIs(ColonTok)
         THEN
            IF Qualident()
            THEN
            ELSE
               WriteError('Qualident - expected')
            END
         END ;
         Success := TRUE
      ELSE
         Success := FALSE ;
         WriteError(') - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END FormalParameters ;

PROCEDURE FPSection () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF NonVarFPSection()
   THEN
      Success := TRUE
   ELSIF VarFPSection()
   THEN
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END FPSection ;

PROCEDURE VarFPSection () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(VarTok)
   THEN
      IF IdentList()
      THEN
         IF TokenIs(ColonTok)
         THEN
            IF FormalType()
            THEN
               Success := TRUE
            ELSE
               Success := FALSE ;
               WriteError('FormalType - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError(': - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('IdentList - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END VarFPSection ;

PROCEDURE NonVarFPSection () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF IdentList()
   THEN
      IF TokenIs(ColonTok)
      THEN
         IF FormalType()
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('FormalType - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError(': - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END NonVarFPSection ;

(*
PROCEDURE FPSection () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(VarTok)
   THEN
   ELSE
   END ;
   IF IdentList()
   THEN
      IF TokenIs(ColonTok)
      THEN
         IF FormalType()
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('FormalType - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError(': - expected')
      END
   ELSE
      Success := FALSE ;
      WriteError('IdentList - expected')
   END ;
   RETURN( Success )
END FPSection ;
*)

PROCEDURE FormalType () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(ArrayTok)
   THEN
      IF TokenIs(OfTok)
      THEN
      ELSE
         WriteError('OF - expected')
      END
   END ;
   IF Qualident()
   THEN
      Success := TRUE
   ELSE
      Success := FALSE ;
      WriteError('Qualident - expected')
   END ;
   RETURN( Success )
END FormalType ;

PROCEDURE ModuleDeclaration () : BOOLEAN ;
VAR
   Success  : BOOLEAN ;
BEGIN
   IF TokenIs(ModuleTok)
   THEN
      PushAutoOn ;  (* ******** *)
      IF Ident()
      THEN
         StartBuildInnerModule ; (* ******** *)
         PushAutoOff ;  (* ******** *)
         IF Priority()
         THEN
         END ;
         IF TokenIs(SemiColonTok)
         THEN
            PushAutoOn ;  (* ******** *)
            WHILE Import() DO
               BuildImportInnerModule  (* ******** *)
            END ;
            IF Export()
            THEN
               BuildExportInnerModule  (* ******** *)
            END ;
            PopAuto ;  (* ******** *)
            IF Block()
            THEN
               PushAutoOn ;  (* ********** *)
               IF Ident()
               THEN
                  EndBuildInnerModule ;  (* ******** *)
                  Success := TRUE
               ELSE
                  Success := FALSE ;
                  WriteError('Ident - expected')
               END ;
               PopAuto  (* ********** *)
            ELSE
               Success := FALSE ;
               WriteError('Block - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError('; - expected')
         END ;
         PopAuto ;  (* ******** *)
      ELSE
         Success := FALSE ;
         WriteError('Ident - expected')
      END ;
      PopAuto  (* ******** *)
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ModuleDeclaration ;

PROCEDURE Priority () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(LSBraTok)
   THEN
      IF ConstExpression()
      THEN
         IF TokenIs(RSBraTok)
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('] - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('ConstExpression - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Priority ;


(*
   Export - stacks exported identifiers.
            The Stack:


            Entry

            Empty


            Exit                      Exit

            +--------------+          +--------------+
            | #            |          | #            |
            |--------------|          |--------------|
            | Ident 1      |          | Ident 1      |
            |--------------|          |--------------|
            | Ident 2      |          | Ident 2      |
            |--------------|          |--------------|
            .              .          .              .
            .              .          .              .
            .              .          .              .
            |--------------|          |--------------|
            | Ident #      |          | Ident #      |
            |--------------|          |--------------|
            | QualifiedTok |          | ExportTok    |
            |--------------|          |--------------|
*)

PROCEDURE Export () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(ExportTok)
   THEN
      IF TokenIs(QualifiedTok)
      THEN
         PushT(QualifiedTok)
      ELSIF TokenIs(UnQualifiedTok)
      THEN
         PushT(UnQualifiedTok)
      ELSE
         PushT(ExportTok)
      END ;
      IF IdentList()
      THEN
         IF TokenIs(SemiColonTok)
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('; - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('IdentList - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Export ;


(*
   Import - Stacks the imported identifiers.

            Entry          Exit                   Exit

            Empty          +-----------+          +-----------+
                           | #         |          | #         |
                           |-----------|          |-----------|
                           | Ident 1   |          | Ident 1   |
                           |-----------|          |-----------|
                           | Ident 2   |          | Ident 2   |
                           |-----------|          |-----------|
                           .           .          .           .
                           .           .          .           .
                           .           .          .           .
                           |-----------|          |-----------|
                           | Ident #   |          | Ident #   |
                           |-----------|          |-----------|
                           | ImportTok |          | Name      |
                           |-----------|          |-----------|

                           IMPORT Ident1, .. ;    FROM Name IMPORT Ident1, ..
*)

PROCEDURE Import () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(FromTok)
   THEN
      IF Ident()
      THEN
         IF TokenIs(ImportTok)
         THEN
            IF IdentList()
            THEN
               IF TokenIs(SemiColonTok)
               THEN
                  Success := TRUE
               ELSE
                  Success := FALSE ;
                  WriteError('; - expected')
               END
            ELSE
               Success := FALSE ;
               WriteError('IdentList - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError('IMPORT - expected')
         END
      ELSE
         WriteError('Ident - expected')
      END
   ELSIF TokenIs(ImportTok)
   THEN
      PushT(ImportTok) ; (* Determines whether Ident or Module *)
      IF IdentList()
      THEN
         IF TokenIs(SemiColonTok)
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('; - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('IdentList - expected')
      END
   ELSE
      Success := FALSE ;
      (* WriteError('IMPORT - expected') *)
   END ;
   RETURN( Success )
END Import ;

PROCEDURE DefinitionModule () : BOOLEAN ;
VAR
   Success  : BOOLEAN ;
BEGIN
   IF TokenIs(DefinitionTok)
   THEN
      IF TokenIs(ModuleTok)
      THEN
         PushAutoOn ;  (* ******** *)
         IF Ident()
         THEN
            P1StartBuildDefinitionModule ;  (* ******** *)
            IF TokenIs(SemiColonTok)
            THEN
               WHILE Import() DO
                  BuildImportOuterModule  (* ******** *)
               END ;
               IF Export()
               THEN
                  BuildExportOuterModule  (* ******** *)
               END ;
               PushAutoOff ;  (* ******** *)
               WHILE Definition() DO
               END ;
               PopAuto ;  (* ******** *)
               IF TokenIs(EndTok)
               THEN
                  IF Ident()
                  THEN
                     P1EndBuildDefinitionModule ;  (* ******** *)
                     IF TokenIs(PeriodTok)
                     THEN
                        Success := TRUE
                     ELSE
                        Success := FALSE ;
                        WriteError('. - expected')
                     END
                  ELSE
                     Success := FALSE ;
                     WriteError('Ident - expected')
                  END
               ELSE
                  Success := FALSE ;
                  WriteError('END - expected')
               END
            ELSE
               Success := FALSE ;
               WriteError('; - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError('Ident - expected')
         END ;
         PopAuto  (* ******** *)
      ELSE
         Success := FALSE ;
         WriteError('MODULE - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END DefinitionModule ;

PROCEDURE Definition () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(ConstTok)
   THEN
      WHILE ConstantDeclaration() DO
         IF TokenIs(SemiColonTok)
         THEN
         ELSE
            WriteError('; - expected')
         END
      END ;
      Success := TRUE
   ELSIF TokenIs(TypeTok)
   THEN
      PushAutoOn ;  (* ********** *)
      WHILE Ident() DO
         IF TokenIs(EqualTok)
         THEN
            IF Type()
            THEN
            ELSE
               WriteError('Type - expected')
            END
         ELSE
            BuildHiddenType   (* ********** *)
         END ;
         IF TokenIs(SemiColonTok)
         THEN
         ELSE
            WriteError('; - expected')
         END
      END ;
      PopAuto ;  (* ********** *)
      Success := TRUE
   ELSIF TokenIs(VarTok)
   THEN
      WHILE VariableDeclaration() DO
         IF TokenIs(SemiColonTok)
         THEN
         ELSE
            WriteError('; - expected')
         END
      END ;
      Success := TRUE
   ELSIF ProcedureHeading()
   THEN
      IF TokenIs(SemiColonTok)
      THEN
         Success := TRUE
      ELSE
         Success := FALSE ;
         WriteError('; - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Definition ;

PROCEDURE Module () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
   Ptr, PtrC: CARDINAL ;
BEGIN
   IF TokenIs(ModuleTok)
   THEN
      PushAutoOn ;  (* ******** *)
      IF Ident()
      THEN
         GetPtr(Ptr) ;
         P1StartBuildProgramModule ;  (* ******** *)
         PushAutoOff ;  (* ******** *)
         GetPtr(PtrC) ; Assert(Ptr=PtrC) ;
         IF Priority()
         THEN
         END ;
         GetPtr(PtrC) ; Assert(Ptr=PtrC) ;
         IF TokenIs(SemiColonTok)
         THEN
            PushAutoOn ;  (* ******** *)
            WHILE Import() DO
               BuildImportOuterModule  (* ******** *)
            END ;
            PopAuto ;  (* ******** *)
            GetPtr(PtrC) ; Assert(Ptr=PtrC) ;
            IF Block()
            THEN
               GetPtr(PtrC) ; Assert(Ptr=PtrC) ;
               PushAutoOn ;  (* ******** *)
               IF Ident()
               THEN
                  P1EndBuildProgramModule ;  (* ******** *)
                  IF TokenIs(PeriodTok)
                  THEN
                     Success := TRUE
                  ELSE
                     Success := FALSE ;
                     WriteError('. - expected')
                  END
               ELSE
                  Success := FALSE ;
                  WriteError('Ident - expected')
               END ;
               PopAuto ;  (* ******** *)
            ELSE
               Success := FALSE ;
               WriteError('Block - expected')
            END ;
            (* EndBuildInit ;   (* ******** Generate Quads *) *)
         ELSE
            Success := FALSE ;
            WriteError('; - expected')
         END ;
         PopAuto ;  (* ******** *)
      ELSE
         Success := FALSE ;
         WriteError('Ident - expected')
      END ;
      PopAuto  (* ******** *)
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Module ;

PROCEDURE ImplementationModule () : BOOLEAN ;
VAR
   Success  : BOOLEAN ;
BEGIN
   IF TokenIs(ImplementationTok)
   THEN
      IF TokenIs(ModuleTok)
      THEN
         PushAutoOn ;  (* ******** *)
         IF Ident()
         THEN
            P1StartBuildImplementationModule ;  (* ******** *)
            PushAutoOff ;  (* ******** *)
            IF Priority()
            THEN
            END ;
            IF TokenIs(SemiColonTok)
            THEN
               PushAutoOn ;  (* ******** *)
               WHILE Import() DO
                  BuildImportOuterModule  (* ******** *)
               END ;
               PopAuto ;  (* ******** *)
               IF Block()
               THEN
                  PushAutoOn ;  (* ******** *)
                  IF Ident()
                  THEN
                     P1EndBuildImplementationModule ;  (* ******** *)
                     IF TokenIs(PeriodTok)
                     THEN
                        Success := TRUE
                     ELSE
                        Success := FALSE ;
                        WriteError('. - expected')
                     END
                  ELSE
                     Success := FALSE ;
                     WriteError('Ident - expected')
                  END ;
                  PopAuto   (* ******** *)
               ELSE
                  Success := FALSE ;
                  WriteError('Block - expected')
               END
            ELSE
               Success := FALSE ;
               WriteError('; - expected')
            END ;
            PopAuto ;  (* ******** *)
         ELSE
            Success := FALSE ;
            WriteError('Ident - expected')
         END ;
         PopAuto  (* ******** *)
      ELSE
         WriteError('IMPLEMENTATION - expected') ;
         Success := FALSE
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ImplementationModule ;

PROCEDURE CompilationUnit () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   PushAutoOff ;    (* ********** *)
   IF DefinitionModule()
   THEN
      Success := TRUE
   ELSIF ImplementationModule()
   THEN
      Success := TRUE
   ELSIF Module()
   THEN
      Success := TRUE
   ELSE
      Success := FALSE ;
      WriteError('Module failed to compile')
   END ;
   PopAuto ;    (* ********** *)
   RETURN( Success )
END CompilationUnit ;


END P1Compile.
