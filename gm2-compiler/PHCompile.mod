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
IMPLEMENTATION MODULE PHCompile ;


FROM M2Debug IMPORT WriteDebug, Assert ;

FROM M2Quads IMPORT PushT, PopT,
                    StartBuildDefFile, StartBuildModFile,
                    EndBuildFile,
                    StartBuildInit,
                    EndBuildInit,
                    BuildProcedureStart,
                    BuildProcedureEnd,
                    BuildAssignment ;

FROM M2Lexical IMPORT TokenIs, WriteError ;

FROM M2Atom IMPORT Ident, PushAutoOn, PushAutoOff, PopAuto ;

FROM M2Reference IMPORT Qualident, IdentList ;

FROM P3Expression IMPORT ConstExpression ;

(*
   Note that in PHCompile we do not generate quadruples for StatementSequence
*)

FROM P2Statement IMPORT StatementSequence ;

FROM P3Type IMPORT Type ;

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

FROM P3SymBuild IMPORT (* StartBuildProgramModule, *)  P3StartBuildProgModule,
                       (* EndBuildProgramModule, *)  P3EndBuildProgModule,

                       (* StartBuildDefinitionModule, *)  P3StartBuildDefModule,
                       (* EndBuildDefinitionModule, *)  P3EndBuildDefModule,

                       (* StartBuildImplementationModule, *)  P3StartBuildImpModule,
                       (* EndBuildImplementationModule, *)  P3EndBuildImpModule,

                       StartBuildInnerModule,
                       EndBuildInnerModule,

(*
                       StartBuildProcedure,
                       BuildProcedureHeading,
                       EndBuildProcedure,
*)
                       BuildConst,
                       BuildNulName ;


(* %%%FORWARD%%%
PROCEDURE Block() : BOOLEAN ; FORWARD ;
PROCEDURE ConstantDeclaration() : BOOLEAN ; FORWARD ;
PROCEDURE Declaration() : BOOLEAN ; FORWARD ;
PROCEDURE Definition() : BOOLEAN ; FORWARD ;
PROCEDURE DefinitionModule() : BOOLEAN ; FORWARD ;
PROCEDURE Export() : BOOLEAN ; FORWARD ;
PROCEDURE FPSection() : BOOLEAN ; FORWARD ;
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


PROCEDURE ConstantDeclaration() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   PushAutoOn ;  (* ********** *)
   IF Ident()
   THEN
      IF TokenIs(EqualTok)
      THEN
         BuildConst ;  (* ******** Symbol Table *)
         IF ConstExpression()
         THEN
            BuildAssignment ;  (* ******** Generate Quads *)
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
   PopAuto ;  (* ********** *)
   RETURN( Success )
END ConstantDeclaration ;

PROCEDURE TypeDeclaration() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
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
         END ;
      ELSE
         Success := FALSE ;
         WriteError('= - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END TypeDeclaration ;

PROCEDURE VariableDeclaration() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF IdentList()
   THEN
      IF TokenIs(ColonTok)
      THEN
         (* BuildNulName ;  (* ******** *) *)
         IF Type()
         THEN
            (* BuildVariable ;  (* ******** *) *)
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
   RETURN( Success )
END VariableDeclaration ;

PROCEDURE ProcedureDeclaration() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF ProcedureHeading()
   THEN
      IF TokenIs(SemiColonTok)
      THEN
         (* BuildProcedureStart ;  (* Generate Quads ******** *) *)
         IF Block()
         THEN
            (* BuildProcedureEnd ;  (* Generate Quads ******** *) *)
            (* PushAutoOn ;   (* ********** *) *)
            IF Ident()
            THEN
               (* EndBuildProcedure ;  (* Symbol Table Generation ******** *) *)
               Success := TRUE
            ELSE
               Success := FALSE ;
               WriteError('Ident - expected')
            END ;
            (* PopAuto   (* ********** *) *)
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

PROCEDURE ProcedureHeading() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(ProcedureTok)
   THEN
      (* PushAutoOn ;   (* ********** *) *)
      IF Ident()
      THEN
         (* StartBuildProcedure ;  (* Symbol Table Generation ******** *) *)
         (* PushAutoOff ;   (* ********** *) *)
         IF FormalParameters()
         THEN
         END ;
         (* BuildProcedureHeading ;  (* Symbol Table Generation ******** *) *)
         (* PopAuto ;   (* ********** *) *)
         Success := TRUE
      ELSE
         Success := FALSE ;
         WriteError('Ident - expected')
      END ;
      (* PopAuto     (* ********** *) *)
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ProcedureHeading ;

PROCEDURE Block() : BOOLEAN ;
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
      WriteError('END - expected')
   END ;
   RETURN( Success )
END Block ;

PROCEDURE Declaration() : BOOLEAN ;
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

PROCEDURE FormalParameters() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(LParaTok)
   THEN
      (* StartBuildFormalParameters ;  (* ******** *) *)
      IF FPSection()
      THEN
         WHILE TokenIs(SemiColonTok) DO
            IF FPSection()
            THEN
            ELSE
               WriteError('FPSection - expected')
            END
         END
      END ;
      IF TokenIs(RParaTok)
      THEN
         (* EndBuildFormalParameters ;  (* ******** *) *)
         IF TokenIs(ColonTok)
         THEN
            IF Qualident()
            THEN
               (* BuildFunction  (* ******** *) *)
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

PROCEDURE FPSection() : BOOLEAN ;
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

PROCEDURE VarFPSection() : BOOLEAN ;
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

PROCEDURE NonVarFPSection() : BOOLEAN ;
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
PROCEDURE FPSection() : BOOLEAN ;
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
            Success := TRUE ;
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
PROCEDURE FormalType() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(ArrayTok)
   THEN
      IF TokenIs(OfTok)
      THEN
         (* PushT(ArrayTok)  (* ******** *) *)
      ELSE
         WriteError('OF - expected')
      END
   ELSE
      (* PushT(NulTok)  (* ******** *) *)
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

PROCEDURE ModuleDeclaration() : BOOLEAN ;
VAR
   Success  : BOOLEAN ;
BEGIN
   IF TokenIs(ModuleTok)
   THEN
      PushAutoOn ;   (* ********** *)
      IF Ident()
      THEN
         StartBuildInnerModule ; (* ******** *)
         PushAutoOff ;   (* ********** *)
         IF Priority()
         THEN
            (* BuildPriority   (* ********** *) *)
         END ;
         IF TokenIs(SemiColonTok)
         THEN
            WHILE Import() DO
            END ;
            IF Export()
            THEN
            END ;
            IF Block()
            THEN
               PushAutoOn ;   (* ********** *)
               IF Ident()
               THEN
                  EndBuildInnerModule ;  (* ******** *)
                  Success := TRUE
               ELSE
                  Success := FALSE ;
                  WriteError('Ident - expected')
               END ;
               PopAuto     (* ********** *)
            ELSE
               Success := FALSE ;
               WriteError('Block - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError('; - expected')
         END ;
         PopAuto   (* ********** *)
      ELSE
         Success := FALSE ;
         WriteError('Ident - expected')
      END ;
      PopAuto   (* ********** *)
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ModuleDeclaration ;

PROCEDURE Priority() : BOOLEAN ;
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

PROCEDURE Export() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(ExportTok)
   THEN
      IF TokenIs(QualifiedTok)
      THEN
         (* PushT(QualifiedTok) *)
      ELSIF TokenIs(UnQualifiedTok)
      THEN
         (* PushT(UnQualifiedTok) *)
      ELSE
         (* PushT(ExportTok) *)
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

PROCEDURE Import() : BOOLEAN ;
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
      (* PushT(ImportTok) ; (* Determines whether Ident or Module *) *)
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

PROCEDURE DefinitionModule() : BOOLEAN ;
VAR
   Success  : BOOLEAN ;
BEGIN
   IF TokenIs(DefinitionTok)
   THEN
      IF TokenIs(ModuleTok)
      THEN
         PushAutoOn ;   (* ********** *)
         IF Ident()
         THEN
            StartBuildDefFile ;   (* ********** Generate Quads *)
            P3StartBuildDefModule ;  (* ******** *)
            PushAutoOff ;     (* ********** *)
            IF TokenIs(SemiColonTok)
            THEN
               WHILE Import() DO
                  (* BuildImportOuterModule  (* ******** *) *)
               END ;
               IF Export()
               THEN
                  (* BuildExportOuterModule  (* ******** *) *)
               END ;
               WHILE Definition() DO
               END ;
               IF TokenIs(EndTok)
               THEN
                  PushAutoOn ;     (* ********** *)
                  IF Ident()
                  THEN
                     EndBuildFile ;   (* ********** Generate Quads *)
                     P3EndBuildDefModule ;  (* ******** *)
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
                  PopAuto       (* ********** *)
               ELSE
                  Success := FALSE ;
                  WriteError('END - expected')
               END
            ELSE
               Success := FALSE ;
               WriteError('; - expected')
            END ;
            PopAuto     (* ********** *)
         ELSE
            Success := FALSE ;
            WriteError('Ident - expected')
         END ;
         PopAuto     (* ********** *)
      ELSE
         Success := FALSE ;
         WriteError('MODULE - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END DefinitionModule ;

PROCEDURE Definition() : BOOLEAN ;
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
      WHILE Ident() DO
         IF TokenIs(EqualTok)
         THEN
            IF Type()
            THEN
            ELSE
               WriteError('Type - expected')
            END
         END ;
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

PROCEDURE Module() : BOOLEAN ;
VAR
   Success  : BOOLEAN ;
BEGIN
   IF TokenIs(ModuleTok)
   THEN
      PushAutoOn ;   (* ********** *)
      IF Ident()
      THEN
         StartBuildModFile ;   (* ********** Generate Quads *)
         P3StartBuildProgModule ;  (* ******** *)
         PushAutoOff ;   (* ********** *)
         IF Priority()
         THEN
            (* BuildPriority   (* ********** *) *)
         END ;
         IF TokenIs(SemiColonTok)
         THEN
            WHILE Import() DO
               (* BuildImportOuterModule  (* ******** *) *)
            END ;
            StartBuildInit ;   (* ******** Generate Quads *)
            IF Block()
            THEN
               PushAutoOn ;     (* ********** *)
               IF Ident()
               THEN
                  EndBuildFile ;   (* ********** Generate Quads *)
                  P3EndBuildProgModule ;  (* ******** *)
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
               PopAuto     (* ********** *)
            ELSE
               Success := FALSE ;
               WriteError('Block - expected')
            END ;
            EndBuildInit ;   (* ******** Generate Quads *)
         ELSE
            Success := FALSE ;
            WriteError('; - expected')
         END ;
         PopAuto     (* ********** *)
      ELSE
         Success := FALSE ;
         WriteError('Ident - expected')
      END ;
      (* EndBuildModuleSuit *)    (* ******** Generate Quads *)
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Module ;

PROCEDURE ImplementationModule() : BOOLEAN ;
VAR
   Success  : BOOLEAN ;
BEGIN
   IF TokenIs(ImplementationTok)
   THEN
      IF TokenIs(ModuleTok)
      THEN
         PushAutoOn ;   (* ********** *)
         IF Ident()
         THEN
            StartBuildModFile ;   (* ********** Generate Quads *)
            P3StartBuildImpModule ;  (* ******** *)
            PushAutoOff ;   (* ********** *)
            IF Priority()
            THEN
               (* BuildPriority   (* ********** *) *)
            END ;
            IF TokenIs(SemiColonTok)
            THEN
               WHILE Import() DO
                  (* BuildImportOuterModule  (* ******** *) *)
               END ;
               StartBuildInit ;  (* ******** Generate Quads *)
               IF Block()
               THEN
                  PushAutoOn ;   (* ********** *)
                  IF Ident()
                  THEN
                     EndBuildFile ;   (* ********** Generate Quads *)
                     P3EndBuildImpModule ;  (* ******** *)
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
                  PopAuto     (* ********** *)
               ELSE
                  Success := FALSE ;
                  WriteError('Block - expected')
               END ;
               EndBuildInit ;  (* ******** Generate Quads *)
            ELSE
               Success := FALSE ;
               WriteError('; - expected')
            END ;
            PopAuto     (* ********** *)
         ELSE
            Success := FALSE ;
            WriteError('Ident - expected')
         END ;
         PopAuto     (* ********** *)
      ELSE
         WriteError('IMPLEMENTATION - expected') ;
         Success := FALSE
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ImplementationModule ;

PROCEDURE CompilationUnit() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   PushAutoOff ;   (* ********** *)
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
   PopAuto ;   (* ********** *)
   RETURN( Success )
END CompilationUnit ;


END PHCompile.
