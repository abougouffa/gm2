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
IMPLEMENTATION MODULE P3Statement ;


FROM M2Debug IMPORT WriteDebug, Assert ;

FROM M2Lexical IMPORT GetToken, PutToken,
                      CurrentToken, TokenType, TypeOfToken,
                      TokenIs, IsToken, WriteError,
                      CardToType, TypeToCard ;

FROM M2Reserved IMPORT BarTok, CommaTok, SemiColonTok, ColonTok,
                       PeriodPeriodTok, BecomesTok,
                       SingleQuoteTok, DoubleQuotesTok,
                       IfTok, ThenTok, ElseTok, ElsifTok, EndTok,
                       DoTok, WhileTok, RepeatTok,
                       ForTok, ByTok, ToTok,
                       CaseTok, WithTok, ExitTok, ReturnTok,
                       OfTok, UntilTok, LoopTok, AsmTok,
                       OrTok ;

FROM M2Atom IMPORT Ident, Found, PushAutoOn, PopAuto ;

FROM P3Expression IMPORT ConstExpression, Expression, ActualParameters ;

FROM M2Reference IMPORT Designator ;

FROM M2Inline IMPORT AsmStatement ;

FROM M2Quads IMPORT PushTF, PopTF,
                    BuildAssignment,
                    BuildRepeat, BuildUntil,
                    BuildWhile, BuildDoWhile, BuildEndWhile,
                    BuildLoop, BuildExit, BuildEndLoop,
                    BuildThenIf, BuildElse, BuildEndIf,
                    BuildForToByDo, BuildPseudoBy, BuildEndFor,
                    BuildElsif1, BuildElsif2,
                    BuildProcedureCall, BuildReturn, BuildNulExpression,
                    StartBuildWith, EndBuildWith,
                    BuildInline,
                    BuildCaseStart,
                    BuildCaseOr,
                    BuildCaseElse,
                    BuildCaseEnd,
                    BuildCaseStartStatementSequence,
                    BuildCaseEndStatementSequence,
                    BuildCaseList,
                    BuildCaseRange, BuildCaseEquality,
                    RecordOp ;

FROM M2Quads IMPORT BuildNulParam,
                    BuildLineNo ;


(* %%%FORWARD%%%
PROCEDURE Assignment() : BOOLEAN ; FORWARD ;
PROCEDURE Case() : BOOLEAN ; FORWARD ;
PROCEDURE CaseLabelList() : BOOLEAN ; FORWARD ;
PROCEDURE CaseLabels() : BOOLEAN ; FORWARD ;
PROCEDURE CaseStatement() : BOOLEAN ; FORWARD ;
PROCEDURE ForStatement() : BOOLEAN ; FORWARD ;
PROCEDURE IfStatement() : BOOLEAN ; FORWARD ;
PROCEDURE LoopStatement() : BOOLEAN ; FORWARD ;
PROCEDURE ProcedureCall() : BOOLEAN ; FORWARD ;
PROCEDURE RepeatStatement() : BOOLEAN ; FORWARD ;
PROCEDURE Statement() : BOOLEAN ; FORWARD ;
PROCEDURE WhileStatement() : BOOLEAN ; FORWARD ;
PROCEDURE WithStatement() : BOOLEAN ; FORWARD ;
   %%%FORWARD%%% *)


PROCEDURE Statement() : BOOLEAN ;
BEGIN
   PushAutoOn ;
   BuildLineNo ;  (* ******** *)
   IF AsmStatement()
   THEN
   ELSIF IfStatement()
   THEN
   ELSIF CaseStatement()
   THEN
   ELSIF WhileStatement()
   THEN
   ELSIF RepeatStatement()
   THEN
   ELSIF LoopStatement()
   THEN
   ELSIF ForStatement()
   THEN
   ELSIF WithStatement()
   THEN
   ELSIF TokenIs(ExitTok)
   THEN
      BuildExit  (* ******** *)
   ELSIF TokenIs(ReturnTok)
   THEN
      BuildLineNo ;  (* ******** *)
      IF Expression()
      THEN
      ELSE
         BuildNulExpression  (* ******** *)
      END ;
      BuildReturn  (* ********** *)
   ELSIF Assignment()
   THEN
   ELSIF ProcedureCall()
   THEN
   END ;
   PopAuto ;
   RETURN( TRUE )
END Statement ;

PROCEDURE Assignment() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   Assert(NOT IsToken(BecomesTok)) ;
   (* Must check to see if the next symbol is a :=             *)
   (* otherwise the statement sequence may be a procedure call *)
   BuildLineNo ;  (* ******** *)
   IF Found(BecomesTok)
   THEN
      IF Designator()
      THEN
         IF TokenIs(BecomesTok)
         THEN
            IF Expression()
            THEN
               BuildAssignment ;  (* ******** *)
               Success := TRUE
            ELSE
               Success := FALSE ;
               WriteError('Expression - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError(':= - expected')
         END
      ELSE
         Success := FALSE
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Assignment ;

PROCEDURE ProcedureCall() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   BuildLineNo ;  (* ******** *)
   IF Designator()
   THEN
      BuildLineNo ;  (* ******** *)
      IF ActualParameters()
      THEN
      ELSE
         BuildNulParam  (* ******** *)
      END ;
      BuildProcedureCall ;  (* ******** *)
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ProcedureCall ;

PROCEDURE StatementSequence () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   BuildLineNo ;  (* ******** *)
   IF Statement()
   THEN
      BuildLineNo ;  (* ******** *)
      WHILE TokenIs(SemiColonTok) DO
         BuildLineNo ;  (* ******** *)
         IF Statement()
         THEN
         ELSE
            WriteError('Statement - expected')
         END
      END ;
      BuildLineNo ;  (* ******** *)
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END StatementSequence ;

PROCEDURE IfStatement() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(IfTok)
   THEN
      BuildLineNo ;  (* ******** *)
      IF Expression()
      THEN
         IF TokenIs(ThenTok)
         THEN
            BuildLineNo ;  (* ******** *)
            BuildThenIf ;  (* ******** *)
            IF StatementSequence()
            THEN
               WHILE TokenIs(ElsifTok) DO
                  BuildLineNo ;  (* ******** *)
                  BuildElsif1 ;  (* ******** *)
                  IF Expression()
                  THEN
                     IF TokenIs(ThenTok)
                     THEN
                        BuildLineNo ;  (* ******** *)
                        BuildThenIf ;  (* ******** *)
                        IF StatementSequence()
                        THEN
                           BuildElsif2  (* ******** *)
                        ELSE
                           WriteError('StatementSequence - expected')
                        END
                     ELSE
                        WriteError('THEN - expected')
                     END
                  ELSE
                     WriteError('Expression - expected')
                  END
               END ;
               IF TokenIs(ElseTok)
               THEN
                  BuildLineNo ;  (* ******** *)
                  BuildElse ;  (* ******** *)
                  IF StatementSequence()
                  THEN
                  ELSE
                     WriteError('StatementSequence - expected')
                  END
               END ;
               IF TokenIs(EndTok)
               THEN
                  BuildLineNo ;  (* ******** *)
                  BuildEndIf ;  (* ******** *)
                  Success := TRUE
               ELSE
                  Success := FALSE ;
                  WriteError('END or ; expected, or possibly misformed statement')
               END
            ELSE
               Success := FALSE ;
               WriteError('StatementSequence - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError('THEN - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('Expression - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END IfStatement ;

PROCEDURE CaseStatement() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(CaseTok)
   THEN
      BuildLineNo ;  (* ******** *)
      IF Expression()
      THEN
         BuildCaseStart ;  (* ******** *)
         IF TokenIs(OfTok)
         THEN
            IF Case()
            THEN
               WHILE TokenIs(BarTok) DO
                  IF Case()
                  THEN
                  ELSE
                     WriteError('Case - expected')
                  END
               END ;
               IF TokenIs(ElseTok)
               THEN
                  BuildLineNo ;  (* ******** *)
                  BuildCaseElse ;  (* ********** *)
                  IF StatementSequence()
                  THEN
                  ELSE
                     WriteError('StatementSequence - expected')
                  END
               END ;
               IF TokenIs(EndTok)
               THEN
                  BuildLineNo ;  (* ******** *)
                  BuildCaseEnd ;  (* ********** *)
                  Success := TRUE
               ELSE
                  Success := FALSE ;
                  WriteError('END or ; expected, or possibly misformed statement')
               END
            ELSE
               Success := FALSE ;
               WriteError('Case - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError('OF - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('Expression - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END CaseStatement ;

PROCEDURE Case() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF CaseLabelList()
   THEN
      IF TokenIs(ColonTok)
      THEN
         BuildCaseStartStatementSequence ;  (* ********** *)
         IF StatementSequence()
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('StatementSequence - expected')
         END ;
         BuildCaseEndStatementSequence  (* ********** *)
      ELSE
         Success := FALSE ;
         WriteError(': - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Case ;

PROCEDURE CaseLabelList() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF CaseLabels()
   THEN
      WHILE TokenIs(CommaTok) DO
         BuildCaseOr ;    (* ********** *)
         IF CaseLabels()
         THEN
         ELSE
            WriteError('CaseLabels - expected')
         END
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END CaseLabelList ;

PROCEDURE CaseLabels() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF ConstExpression()
   THEN
      IF TokenIs(PeriodPeriodTok)
      THEN
         IF ConstExpression()
         THEN
            BuildCaseRange  (* ********** *)
            ; BuildCaseList ;  (* ********** *)
         ELSE
            WriteError('ConstExpression - expected')
         END
      ELSE
         BuildCaseEquality  (* ********** *)
         ; BuildCaseList ;  (* ********** *)
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END CaseLabels ;

PROCEDURE WhileStatement() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(WhileTok)
   THEN
      BuildLineNo ;  (* ******** *)
      BuildWhile ;  (* ******** *)
      IF Expression()
      THEN
         IF TokenIs(DoTok)
         THEN
            BuildDoWhile ;  (* ******** *)
            IF StatementSequence()
            THEN
               IF TokenIs(EndTok)
               THEN
                  BuildEndWhile ;  (* ******** *)
                  Success := TRUE
               ELSE
                  Success := FALSE ;
                  WriteError('END or ; expected, or possibly misformed statement')
               END
            ELSE
               Success := FALSE ;
               WriteError('StatementSquence - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError('DO - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('Expression - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END WhileStatement ;

PROCEDURE RepeatStatement() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(RepeatTok)
   THEN
      BuildLineNo ;  (* ******** *)
      BuildRepeat ;  (* ******** *)
      IF StatementSequence()
      THEN
         IF TokenIs(UntilTok)
         THEN
            BuildLineNo ;  (* ******** *)
            IF Expression()
            THEN
               BuildUntil ; (* ******** *)
               BuildLineNo ;  (* ******** *)
               Success := TRUE
            ELSE
               Success := FALSE ;
               WriteError('Expression - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError('UNTIL - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('StatementSequence - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END RepeatStatement ;

PROCEDURE ForStatement() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(ForTok)
   THEN
      BuildLineNo ;  (* ******** *)
      IF Ident()
      THEN
         IF TokenIs(BecomesTok)
         THEN
            IF Expression()
            THEN
               IF TokenIs(ToTok)
               THEN
                  IF Expression()
                  THEN
                     IF TokenIs(ByTok)
                     THEN
                        IF ConstExpression()
                        THEN
                        ELSE
                           WriteError('ConstExpression - expected')
                        END
                     ELSE
                        BuildPseudoBy  (* ******** *)
                     END ;
                     IF TokenIs(DoTok)
                     THEN
                        BuildForToByDo ;  (* ******** *)
                        IF StatementSequence()
                        THEN
                           IF TokenIs(EndTok)
                           THEN
                              BuildEndFor ;  (* ******** *)
                              Success := TRUE
                           ELSE
                              Success := FALSE ;
                              WriteError('END or ; expected, or possibly misformed statement')
                           END
                        ELSE
                           Success := FALSE ;
                           WriteError('StatementSequence - expected')
                        END
                     ELSE
                        Success := FALSE ;
                        WriteError('DO - expected')
                     END
                  ELSE
                     Success := FALSE ;
                     WriteError('Expression - expected')
                  END
               ELSE
                  Success := FALSE ;
                  WriteError('TO - expected')
               END
            ELSE
               Success := FALSE ;
               WriteError('Expression - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError(':= - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('Ident - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ForStatement ;

PROCEDURE LoopStatement() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(LoopTok)
   THEN
      BuildLineNo ;  (* ******** *)
      BuildLoop ;  (* ******** *)
      IF StatementSequence()
      THEN
         IF TokenIs(EndTok)
         THEN
            BuildEndLoop ;  (* ******** *)
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('END or ; expected, or possibly misformed statement')
         END
      ELSE
         Success := FALSE ;
         WriteError('StatementSequence - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END LoopStatement ;

PROCEDURE WithStatement() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(WithTok)
   THEN
      BuildLineNo ;  (* ******** *)
      IF Designator()
      THEN
         IF TokenIs(DoTok)
         THEN
            BuildLineNo ;  (* ******** *)
            StartBuildWith ;  (* ******** *)
            IF StatementSequence()
            THEN
               IF TokenIs(EndTok)
               THEN
                  BuildLineNo ;  (* ******** *)
                  EndBuildWith ;  (* ******** *)
                  Success := TRUE
               ELSE
                  Success := FALSE ;
                  WriteError('END or ; expected, or possibly misformed statement')
               END
            ELSE
               Success := FALSE ;
               WriteError('StatementSequence - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError('DO - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('Designator - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END WithStatement ;


END P3Statement.
