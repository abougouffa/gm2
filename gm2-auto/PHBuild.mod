(* it is advisable not to edit this file as it was automatically generated from the grammer file bnf/m2-h.bnf *)
# 32 "bnf/m2-h.bnf"

(* Copyright (C) 2001 Free Software Foundation, Inc.
   This file is part of GNU Modula-2.

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
IMPLEMENTATION MODULE PHBuild ;

FROM M2LexBuf IMPORT currentstring, currenttoken, GetToken, InsertToken, InsertTokenAndRewind, GetTokenNo ;
FROM M2Error IMPORT ErrorStringAt ;
FROM NameKey IMPORT NulName, Name ;
FROM M2Reserved IMPORT tokToTok, toktype ;
FROM Strings IMPORT String, InitString, KillString, Mark, ConCat, ConCatChar ;
FROM M2Printf IMPORT printf0 ;
FROM M2Debug IMPORT Assert ;
FROM P2SymBuild IMPORT BuildString, BuildNumber ;

FROM M2Quads IMPORT PushT, PopT, PushTF, PopTF, PopNothing,
                    StartBuildDefFile, StartBuildModFile,
                    EndBuildFile,
                    StartBuildInit,
                    EndBuildInit,
                    BuildProcedureStart,
                    BuildProcedureEnd,
                    BuildAssignment,
      	       	    BuildLineNo,
                    BuildFunctionCall,
                    BuildBinaryOp, BuildUnaryOp, BuildRelOp, BuildNot,
      	       	    BuildLogicalOr, BuildSetRange, BuildEmptySet,
                    BuildElement, BuildBitsetStart, BuildBitsetEnd,
                    BuildLineNo, BuildSizeCheckStart,
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
                    RecordOp,
                    BuildNulParam,
                    BuildDesignatorRecord,
                    BuildDesignatorArray,
                    BuildDesignatorPointer,
                    CheckWithReference,
                    CheckOuterScopeProcedureVariable,
                    IsAutoPushOn, PushAutoOff, PushAutoOn, PopAuto ;

FROM P3SymBuild IMPORT P3StartBuildProgModule,
                       P3EndBuildProgModule,

                       P3StartBuildDefModule,
                       P3EndBuildDefModule,

                       P3StartBuildImpModule,
                       P3EndBuildImpModule,

                       StartBuildInnerModule,
                       EndBuildInnerModule,

                       StartBuildProcedure,
                       BuildProcedureHeading,
                       EndBuildProcedure,
                       BuildConst,
                       BuildSubrange,
                       BuildNulName ;

FROM SymbolTable IMPORT MakeGnuAsm, PutGnuAsmVolatile, PutGnuAsm, PutGnuAsmInput,
                        PutGnuAsmOutput, PutGnuAsmTrash, PutGnuAsmVolatile,
                        MakeRegInterface,
                        PutRegInterface, GetRegInterface,
                        GetSymName, GetType,
                        NulSym,
                        StartScope, EndScope,
                        PutIncluded,
                        IsVarParam, IsProcedure, IsDefImp, IsModule,
                        IsRecord,
                        RequestSym,
                        GetSym, GetLocalSym,
                        RequestSym ;

FROM M2Batch IMPORT IsModuleKnown ;

FROM M2Reserved IMPORT NulTok, ImportTok, ExportTok, QualifiedTok, UnQualifiedTok,
                       EqualTok, HashTok, LessGreaterTok, LessTok, LessEqualTok,
                       GreaterTok, GreaterEqualTok, InTok, PlusTok, MinusTok,
                       OrTok, TimesTok, DivTok, ModTok, AndTok, AmbersandTok ;


CONST
   Debugging = FALSE ;
   Pass1     = FALSE ;          (* permanently disabled for the time being *)
   Pass2     = FALSE ;          (* permanently disabled for the time being *)
   Pass3     = FALSE ;

VAR
   WasNoError: BOOLEAN ;


PROCEDURE ErrorString (s: String) ;
BEGIN
   ErrorStringAt(s, GetTokenNo()) ;
   WasNoError := FALSE
END ErrorString ;


PROCEDURE ErrorArray (a: ARRAY OF CHAR) ;
BEGIN
   ErrorString(InitString(a))
END ErrorArray ;


(*
   expecting token set defined as an enumerated type
   (eoftok, plustok, minustok, timestok, dividetok, becomestok, ambersandtok, periodtok, commatok, semicolontok, lparatok, rparatok, lsbratok, rsbratok, lcbratok, rcbratok, uparrowtok, singlequotetok, equaltok, hashtok, lesstok, greatertok, lessgreatertok, lessequaltok, greaterequaltok, periodperiodtok, colontok, doublequotestok, bartok, andtok, arraytok, begintok, bytok, casetok, consttok, definitiontok, divtok, dotok, elsetok, elsiftok, endtok, exittok, exporttok, fortok, fromtok, iftok, implementationtok, importtok, intok, looptok, modtok, moduletok, nottok, oftok, ortok, pointertok, proceduretok, qualifiedtok, unqualifiedtok, recordtok, repeattok, returntok, settok, thentok, totok, typetok, untiltok, vartok, whiletok, withtok, asmtok, volatiletok, periodperiodperiodtok, datetok, linetok, filetok, integertok, identtok, realtok, stringtok) ;
*)
TYPE
   SetOfStop0 = SET OF [eoftok..begintok] ;
   SetOfStop1 = SET OF [bytok..thentok] ;
   SetOfStop2 = SET OF [totok..stringtok] ;
   

(* %%%FORWARD%%%
PROCEDURE Ident (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Integer (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Real (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE string (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE FileUnit (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ProgramModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ImplementationModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ImplementationOrProgramModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Number (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Qualident (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ConstantDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ConstExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Relation (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SimpleConstExpr (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE UnaryOrConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE AddOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE MulOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ConstFactor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ConstString (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ConstQualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE QualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Element (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE TypeDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Type (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SimpleType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Enumeration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE IdentList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SubrangeType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ArrayType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE RecordType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE FieldListSequence (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE FieldListStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE FieldList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Varient (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentCaseLabelList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentCaseLabels (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentConstExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentRelation (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentSimpleConstExpr (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentUnaryOrConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentAddOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentMulOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentConstFactor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentConstString (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentConstQualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentSimpleSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SilentElement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SetType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE PointerType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ProcedureType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE FormalTypeList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE FormalReturn (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ProcedureParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ProcedureParameter (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE VariableDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Designator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SubDesignator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ExpList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Expression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SimpleExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE UnaryOrTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Term (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Factor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SimpleSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SetOrDesignatorOrFunction (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE SimpleDes (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ActualParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Statement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE AssignmentOrProcedureCall (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE StatementSequence (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE IfStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE CaseStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Case (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE CaseLabelList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE CaseLabels (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE WhileStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE RepeatStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ForStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE LoopStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE WithStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ProcedureDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ProcedureHeading (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ProcedureBlock (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Block (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Declaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE FormalParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE FPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE VarFPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE NonVarFPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE FormalType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE ModuleDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Priority (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Export (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Import (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE DefinitionModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE Definition (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE AsmStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE AsmOperands (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE AsmList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE AsmElement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE TrashList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
   %%%FORWARD%%% *)

(*
   DescribeStop - issues a message explaining what tokens were expected
*)

PROCEDURE DescribeStop (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) : String ;
VAR
   n      : CARDINAL ;
   str,
   message: String ;
BEGIN
   n := 0 ;
   message := InitString('') ;
   IF stringtok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`string'"))) ; INC(n)
   END ;
   IF realtok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`real number'"))) ; INC(n)
   END ;
   IF identtok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`identifier'"))) ; INC(n)
   END ;
   IF integertok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`integer number'"))) ; INC(n)
   END ;
   IF filetok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`__FILE__'"))) ; INC(n)
   END ;
   IF linetok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`__LINE__'"))) ; INC(n)
   END ;
   IF datetok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`__DATE__'"))) ; INC(n)
   END ;
   IF periodperiodperiodtok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`...'"))) ; INC(n)
   END ;
   IF volatiletok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`VOLATILE'"))) ; INC(n)
   END ;
   IF asmtok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`ASM'"))) ; INC(n)
   END ;
   IF withtok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`WITH'"))) ; INC(n)
   END ;
   IF whiletok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`WHILE'"))) ; INC(n)
   END ;
   IF vartok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`VAR'"))) ; INC(n)
   END ;
   IF untiltok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`UNTIL'"))) ; INC(n)
   END ;
   IF typetok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`TYPE'"))) ; INC(n)
   END ;
   IF totok IN stopset2
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`TO'"))) ; INC(n)
   END ;
   IF thentok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`THEN'"))) ; INC(n)
   END ;
   IF settok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`SET'"))) ; INC(n)
   END ;
   IF returntok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`RETURN'"))) ; INC(n)
   END ;
   IF repeattok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`REPEAT'"))) ; INC(n)
   END ;
   IF recordtok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`RECORD'"))) ; INC(n)
   END ;
   IF unqualifiedtok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`UNQUALIFIED'"))) ; INC(n)
   END ;
   IF qualifiedtok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`QUALIFIED'"))) ; INC(n)
   END ;
   IF proceduretok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`PROCEDURE'"))) ; INC(n)
   END ;
   IF pointertok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`POINTER'"))) ; INC(n)
   END ;
   IF ortok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`OR'"))) ; INC(n)
   END ;
   IF oftok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`OF'"))) ; INC(n)
   END ;
   IF nottok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`NOT'"))) ; INC(n)
   END ;
   IF moduletok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`MODULE'"))) ; INC(n)
   END ;
   IF modtok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`MOD'"))) ; INC(n)
   END ;
   IF looptok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`LOOP'"))) ; INC(n)
   END ;
   IF intok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`IN'"))) ; INC(n)
   END ;
   IF importtok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`IMPORT'"))) ; INC(n)
   END ;
   IF implementationtok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`IMPLEMENTATION'"))) ; INC(n)
   END ;
   IF iftok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`IF'"))) ; INC(n)
   END ;
   IF fromtok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`FROM'"))) ; INC(n)
   END ;
   IF fortok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`FOR'"))) ; INC(n)
   END ;
   IF exporttok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`EXPORT'"))) ; INC(n)
   END ;
   IF exittok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`EXIT'"))) ; INC(n)
   END ;
   IF endtok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`END'"))) ; INC(n)
   END ;
   IF elsiftok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`ELSIF'"))) ; INC(n)
   END ;
   IF elsetok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`ELSE'"))) ; INC(n)
   END ;
   IF dotok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`DO'"))) ; INC(n)
   END ;
   IF divtok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`DIV'"))) ; INC(n)
   END ;
   IF definitiontok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`DEFINITION'"))) ; INC(n)
   END ;
   IF consttok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`CONST'"))) ; INC(n)
   END ;
   IF casetok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`CASE'"))) ; INC(n)
   END ;
   IF bytok IN stopset1
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`BY'"))) ; INC(n)
   END ;
   IF begintok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`BEGIN'"))) ; INC(n)
   END ;
   IF arraytok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`ARRAY'"))) ; INC(n)
   END ;
   IF andtok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`AND'"))) ; INC(n)
   END ;
   IF colontok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`:'"))) ; INC(n)
   END ;
   IF periodperiodtok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`..'"))) ; INC(n)
   END ;
   IF greaterequaltok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`>='"))) ; INC(n)
   END ;
   IF lessequaltok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`<='"))) ; INC(n)
   END ;
   IF lessgreatertok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`<>'"))) ; INC(n)
   END ;
   IF hashtok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`#'"))) ; INC(n)
   END ;
   IF equaltok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`='"))) ; INC(n)
   END ;
   IF uparrowtok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`^'"))) ; INC(n)
   END ;
   IF semicolontok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`;'"))) ; INC(n)
   END ;
   IF commatok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`,'"))) ; INC(n)
   END ;
   IF periodtok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`.'"))) ; INC(n)
   END ;
   IF ambersandtok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`&'"))) ; INC(n)
   END ;
   IF dividetok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`/'"))) ; INC(n)
   END ;
   IF timestok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`*'"))) ; INC(n)
   END ;
   IF minustok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`-'"))) ; INC(n)
   END ;
   IF plustok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`+'"))) ; INC(n)
   END ;
   IF doublequotestok IN stopset0
   THEN
      message := ConCatChar(ConCatChar(ConCatChar(ConCatChar(ConCatChar(message, " "), "`"), '"'), "'"), ",") ; INC(n) ; 
   END ;
   IF singlequotetok IN stopset0
   THEN
      message := ConCatChar(ConCatChar(ConCatChar(ConCatChar(ConCatChar(message, ' '), '"'), "'"), '"'), ',') ; INC(n) ; 
   END ;
   IF greatertok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`>'"))) ; INC(n)
   END ;
   IF lesstok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`<'"))) ; INC(n)
   END ;
   IF rparatok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`)'"))) ; INC(n)
   END ;
   IF lparatok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`('"))) ; INC(n)
   END ;
   IF rcbratok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`}'"))) ; INC(n)
   END ;
   IF lcbratok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`{'"))) ; INC(n)
   END ;
   IF rsbratok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`]'"))) ; INC(n)
   END ;
   IF lsbratok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`['"))) ; INC(n)
   END ;
   IF bartok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`|'"))) ; INC(n)
   END ;
   IF becomestok IN stopset0
   THEN
      message := ConCat(ConCatChar(message, ' '), Mark(InitString("`:='"))) ; INC(n)
   END ;
   IF eoftok IN stopset0
   THEN
      (* eoftok has no token name (needed to generate error messages) *)
   END ;

   IF n=0
   THEN
      str := InitString(' syntax error') ; 
      message := KillString(message) ; 
   ELSIF n=1
   THEN
      str := ConCat(message, Mark(InitString(' missing '))) ;
   ELSE
      str := ConCat(InitString(' expecting one of'), message) ;
      message := KillString(message) ;
   END ;
   RETURN( str )
END DescribeStop ;


(*
   DescribeError - issues a message explaining what tokens were expected
*)

PROCEDURE DescribeError (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
   str: String ;
BEGIN
   str := InitString('') ;
   CASE currenttoken OF
   
   stringtok: str := ConCat(InitString("syntax error, found `string'"), Mark(str)) |
   realtok: str := ConCat(InitString("syntax error, found `real number'"), Mark(str)) |
   identtok: str := ConCat(InitString("syntax error, found `identifier'"), Mark(str)) |
   integertok: str := ConCat(InitString("syntax error, found `integer number'"), Mark(str)) |
   filetok: str := ConCat(InitString("syntax error, found `__FILE__'"), Mark(str)) |
   linetok: str := ConCat(InitString("syntax error, found `__LINE__'"), Mark(str)) |
   datetok: str := ConCat(InitString("syntax error, found `__DATE__'"), Mark(str)) |
   periodperiodperiodtok: str := ConCat(InitString("syntax error, found `...'"), Mark(str)) |
   volatiletok: str := ConCat(InitString("syntax error, found `VOLATILE'"), Mark(str)) |
   asmtok: str := ConCat(InitString("syntax error, found `ASM'"), Mark(str)) |
   withtok: str := ConCat(InitString("syntax error, found `WITH'"), Mark(str)) |
   whiletok: str := ConCat(InitString("syntax error, found `WHILE'"), Mark(str)) |
   vartok: str := ConCat(InitString("syntax error, found `VAR'"), Mark(str)) |
   untiltok: str := ConCat(InitString("syntax error, found `UNTIL'"), Mark(str)) |
   typetok: str := ConCat(InitString("syntax error, found `TYPE'"), Mark(str)) |
   totok: str := ConCat(InitString("syntax error, found `TO'"), Mark(str)) |
   thentok: str := ConCat(InitString("syntax error, found `THEN'"), Mark(str)) |
   settok: str := ConCat(InitString("syntax error, found `SET'"), Mark(str)) |
   returntok: str := ConCat(InitString("syntax error, found `RETURN'"), Mark(str)) |
   repeattok: str := ConCat(InitString("syntax error, found `REPEAT'"), Mark(str)) |
   recordtok: str := ConCat(InitString("syntax error, found `RECORD'"), Mark(str)) |
   unqualifiedtok: str := ConCat(InitString("syntax error, found `UNQUALIFIED'"), Mark(str)) |
   qualifiedtok: str := ConCat(InitString("syntax error, found `QUALIFIED'"), Mark(str)) |
   proceduretok: str := ConCat(InitString("syntax error, found `PROCEDURE'"), Mark(str)) |
   pointertok: str := ConCat(InitString("syntax error, found `POINTER'"), Mark(str)) |
   ortok: str := ConCat(InitString("syntax error, found `OR'"), Mark(str)) |
   oftok: str := ConCat(InitString("syntax error, found `OF'"), Mark(str)) |
   nottok: str := ConCat(InitString("syntax error, found `NOT'"), Mark(str)) |
   moduletok: str := ConCat(InitString("syntax error, found `MODULE'"), Mark(str)) |
   modtok: str := ConCat(InitString("syntax error, found `MOD'"), Mark(str)) |
   looptok: str := ConCat(InitString("syntax error, found `LOOP'"), Mark(str)) |
   intok: str := ConCat(InitString("syntax error, found `IN'"), Mark(str)) |
   importtok: str := ConCat(InitString("syntax error, found `IMPORT'"), Mark(str)) |
   implementationtok: str := ConCat(InitString("syntax error, found `IMPLEMENTATION'"), Mark(str)) |
   iftok: str := ConCat(InitString("syntax error, found `IF'"), Mark(str)) |
   fromtok: str := ConCat(InitString("syntax error, found `FROM'"), Mark(str)) |
   fortok: str := ConCat(InitString("syntax error, found `FOR'"), Mark(str)) |
   exporttok: str := ConCat(InitString("syntax error, found `EXPORT'"), Mark(str)) |
   exittok: str := ConCat(InitString("syntax error, found `EXIT'"), Mark(str)) |
   endtok: str := ConCat(InitString("syntax error, found `END'"), Mark(str)) |
   elsiftok: str := ConCat(InitString("syntax error, found `ELSIF'"), Mark(str)) |
   elsetok: str := ConCat(InitString("syntax error, found `ELSE'"), Mark(str)) |
   dotok: str := ConCat(InitString("syntax error, found `DO'"), Mark(str)) |
   divtok: str := ConCat(InitString("syntax error, found `DIV'"), Mark(str)) |
   definitiontok: str := ConCat(InitString("syntax error, found `DEFINITION'"), Mark(str)) |
   consttok: str := ConCat(InitString("syntax error, found `CONST'"), Mark(str)) |
   casetok: str := ConCat(InitString("syntax error, found `CASE'"), Mark(str)) |
   bytok: str := ConCat(InitString("syntax error, found `BY'"), Mark(str)) |
   begintok: str := ConCat(InitString("syntax error, found `BEGIN'"), Mark(str)) |
   arraytok: str := ConCat(InitString("syntax error, found `ARRAY'"), Mark(str)) |
   andtok: str := ConCat(InitString("syntax error, found `AND'"), Mark(str)) |
   colontok: str := ConCat(InitString("syntax error, found `:'"), Mark(str)) |
   periodperiodtok: str := ConCat(InitString("syntax error, found `..'"), Mark(str)) |
   greaterequaltok: str := ConCat(InitString("syntax error, found `>='"), Mark(str)) |
   lessequaltok: str := ConCat(InitString("syntax error, found `<='"), Mark(str)) |
   lessgreatertok: str := ConCat(InitString("syntax error, found `<>'"), Mark(str)) |
   hashtok: str := ConCat(InitString("syntax error, found `#'"), Mark(str)) |
   equaltok: str := ConCat(InitString("syntax error, found `='"), Mark(str)) |
   uparrowtok: str := ConCat(InitString("syntax error, found `^'"), Mark(str)) |
   semicolontok: str := ConCat(InitString("syntax error, found `;'"), Mark(str)) |
   commatok: str := ConCat(InitString("syntax error, found `,'"), Mark(str)) |
   periodtok: str := ConCat(InitString("syntax error, found `.'"), Mark(str)) |
   ambersandtok: str := ConCat(InitString("syntax error, found `&'"), Mark(str)) |
   dividetok: str := ConCat(InitString("syntax error, found `/'"), Mark(str)) |
   timestok: str := ConCat(InitString("syntax error, found `*'"), Mark(str)) |
   minustok: str := ConCat(InitString("syntax error, found `-'"), Mark(str)) |
   plustok: str := ConCat(InitString("syntax error, found `+'"), Mark(str)) |
   doublequotestok: str := ConCat(ConCatChar(ConCatChar(InitString("syntax error, found '"), '"'), "'"), Mark(str)) |
   singlequotetok: str := ConCat(ConCatChar(ConCatChar(InitString('syntax error, found "'), "'"), '"'), Mark(str)) |
   greatertok: str := ConCat(InitString("syntax error, found `>'"), Mark(str)) |
   lesstok: str := ConCat(InitString("syntax error, found `<'"), Mark(str)) |
   rparatok: str := ConCat(InitString("syntax error, found `)'"), Mark(str)) |
   lparatok: str := ConCat(InitString("syntax error, found `('"), Mark(str)) |
   rcbratok: str := ConCat(InitString("syntax error, found `}'"), Mark(str)) |
   lcbratok: str := ConCat(InitString("syntax error, found `{'"), Mark(str)) |
   rsbratok: str := ConCat(InitString("syntax error, found `]'"), Mark(str)) |
   lsbratok: str := ConCat(InitString("syntax error, found `['"), Mark(str)) |
   bartok: str := ConCat(InitString("syntax error, found `|'"), Mark(str)) |
   becomestok: str := ConCat(InitString("syntax error, found `:='"), Mark(str)) |
   eoftok: str := ConCat(InitString("syntax error, found `'"), Mark(str))
   ELSE
   END ;
   ErrorString(str) ;
END DescribeError ;
# 165 "bnf/m2-h.bnf"



(*
   SyntaxError - after a syntax error we skip all tokens up until we reach
                 a stop symbol.
*)

PROCEDURE SyntaxError (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   DescribeError(stopset0, stopset1, stopset2) ;
   IF Debugging
   THEN
      printf0('\nskipping token *** ')
   END ;
   (*
      yes the ORD(currenttoken) looks ugly, but it is *much* safer than
      using currenttoken<sometok as a change to the ordering of the
      token declarations below would cause this to break. Using ORD() we are
      immune from such changes
   *)
   WHILE NOT (((ORD(currenttoken)<32)  AND (currenttoken IN stopset0)) OR
              ((ORD(currenttoken)>=32) AND (ORD(currenttoken)<64) AND (currenttoken IN stopset1)) OR
              ((ORD(currenttoken)>=64) AND (currenttoken IN stopset2)))
   DO
      GetToken
   END ;
   IF Debugging
   THEN
      printf0(' ***\n')
   END
END SyntaxError ;


(*
   SyntaxCheck - 
*)

PROCEDURE SyntaxCheck (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   (* and again (see above re: ORD)
    *)
   IF NOT (((ORD(currenttoken)<32)  AND (currenttoken IN stopset0)) OR
     	   ((ORD(currenttoken)>=32) AND (ORD(currenttoken)<64) AND (currenttoken IN stopset1)) OR
           ((ORD(currenttoken)>=64) AND (currenttoken IN stopset2)))
   THEN
      SyntaxError(stopset0, stopset1, stopset2)
   END
END SyntaxCheck ;


(*
   WarnMissingToken - generates a warning message about a missing token, t.
*)

PROCEDURE WarnMissingToken (t: toktype) ;
VAR
   s0 : SetOfStop0 ;
   s1 : SetOfStop1 ;
   s2 : SetOfStop2 ;
   str: String ;
BEGIN
   s0 := SetOfStop0{} ;
   s1 := SetOfStop1{} ;
   s2 := SetOfStop2{} ;
   IF ORD(t)<32
   THEN
      s0 := SetOfStop0{t}
   ELSIF ORD(t)<64
   THEN
      s1 := SetOfStop1{t}
   ELSE
      s2 := SetOfStop2{t}
   END ;
   str := DescribeStop(s0, s1, s2) ;
   
   str := ConCat(InitString('syntax error,'), Mark(str)) ;
   ErrorStringAt(str, GetTokenNo())
END WarnMissingToken ;


(*
   MissingToken - generates a warning message about a missing token, t.
*)

PROCEDURE MissingToken (t: toktype) ;
BEGIN
   WarnMissingToken(t) ;
   IF (t#identtok) AND (t#integertok) AND (t#realtok) AND (t#stringtok)
   THEN
      IF Debugging
      THEN
         printf0('inserting token\n')
      END ;
      InsertToken(t)
   END
END MissingToken ;


(*
   CheckAndInsert - 
*)

PROCEDURE CheckAndInsert (t: toktype; stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) : BOOLEAN ;
BEGIN
   IF ((ORD(t)<32) AND (t IN stopset0)) OR
      ((ORD(t)>=32) AND (ORD(t)<64) AND (t IN stopset1)) OR
      ((ORD(t)>=64) AND (t IN stopset2))
   THEN
      WarnMissingToken(t) ;
      InsertTokenAndRewind(t) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END CheckAndInsert ;


(*
   InStopSet 
*)

PROCEDURE InStopSet (t: toktype; stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) : BOOLEAN ;
BEGIN
   IF ((ORD(t)<32) AND (t IN stopset0)) OR
      ((ORD(t)>=32) AND (ORD(t)<64) AND (t IN stopset1)) OR
      ((ORD(t)>=64) AND (t IN stopset2))
   THEN
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END InStopSet ;


(*
   PeepToken - peep token checks to see whether the stopset is satisfied by currenttoken
               If it is not then it will insert a token providing the token
               is one of ; ] ) } . OF END ,

               if the stopset contains <identtok> then we do not insert a token
*)

PROCEDURE PeepToken (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   (* and again (see above re: ORD)
    *)
   IF (NOT (((ORD(currenttoken)<32)  AND (currenttoken IN stopset0)) OR
     	    ((ORD(currenttoken)>=32) AND (ORD(currenttoken)<64) AND (currenttoken IN stopset1)) OR
            ((ORD(currenttoken)>=64) AND (currenttoken IN stopset2)))) AND
      (NOT InStopSet(identtok, stopset0, stopset1, stopset2))
   THEN
      (* SyntaxCheck would fail since currentoken is not part of the stopset
         we check to see whether any of currenttoken might be a commonly omitted token *)
      IF CheckAndInsert(semicolontok, stopset0, stopset1, stopset2) OR
         CheckAndInsert(rsbratok, stopset0, stopset1, stopset2) OR
         CheckAndInsert(rparatok, stopset0, stopset1, stopset2) OR
         CheckAndInsert(rcbratok, stopset0, stopset1, stopset2) OR
         CheckAndInsert(periodtok, stopset0, stopset1, stopset2) OR
         CheckAndInsert(oftok, stopset0, stopset1, stopset2) OR
         CheckAndInsert(endtok, stopset0, stopset1, stopset2) OR
         CheckAndInsert(commatok, stopset0, stopset1, stopset2)
      THEN
      END
   END
END PeepToken ;


(*
   Expect - 
*)

PROCEDURE Expect (t: toktype; stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=t
   THEN
      GetToken ;
      IF Pass1
      THEN
         PeepToken(stopset0, stopset1, stopset2)
      END
   ELSE
      MissingToken(t)
   END ;
   SyntaxCheck(stopset0, stopset1, stopset2)
END Expect ;


(*
   CompilationUnit - returns TRUE if the input was correct enough to parse
                     in future passes.
*)

PROCEDURE CompilationUnit () : BOOLEAN ;
BEGIN
   WasNoError := TRUE ;
   FileUnit({eoftok}, {}, {}) ;
   RETURN( WasNoError )
END CompilationUnit ;


(*
   Ident - error checking varient of Ident
*)

PROCEDURE Ident (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF IsAutoPushOn()
   THEN
      PushTF(currentstring, identtok)
   END ;
   Expect(identtok, stopset0, stopset1, stopset2)
END Ident ;


(*
   string -
*)

PROCEDURE string (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF IsAutoPushOn()
   THEN
      PushTF(currentstring, stringtok) ;
      BuildString
   END ;
   Expect(stringtok, stopset0, stopset1, stopset2)
END string ;


(*
   Integer -
*)

PROCEDURE Integer (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF IsAutoPushOn()
   THEN
      PushTF(currentstring, integertok) ;
      BuildNumber
   END ;
   Expect(integertok, stopset0, stopset1, stopset2)
END Integer ;


(*
   Real -
*)

PROCEDURE Real (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF IsAutoPushOn()
   THEN
      PushTF(currentstring, realtok) ;
      BuildNumber
   END ;
   Expect(realtok, stopset0, stopset1, stopset2)
END Real ;

(*
   Ident := 

   first  symbols:identtok
   
   cannot reachend
*)
(*
   Integer := 

   first  symbols:integertok
   
   cannot reachend
*)
(*
   Real := 

   first  symbols:realtok
   
   cannot reachend
*)
(*
   string := 

   first  symbols:stringtok
   
   cannot reachend
*)
(*
   FileUnit := 
               % PushAutoOff  %
               ( DefinitionModule  | ImplementationOrProgramModule  )
               
               % PopAuto  %
               

   first  symbols:moduletok, implementationtok, definitiontok
   
   cannot reachend
*)

# 528 "bnf/m2-h.bnf"
PROCEDURE FileUnit (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 528 "bnf/m2-h.bnf"
BEGIN
# 528 "bnf/m2-h.bnf"
   PushAutoOff  ;
# 529 "bnf/m2-h.bnf"
   IF currenttoken=definitiontok
   THEN
      DefinitionModule(stopset0, stopset1, stopset2) ;
# 530 "bnf/m2-h.bnf"
   ELSIF ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, implementationtok}))
   THEN
      ImplementationOrProgramModule(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: MODULE IMPLEMENTATION DEFINITION')
   END ;
# 530 "bnf/m2-h.bnf"
   PopAuto  ;
END FileUnit ;


(*
   ProgramModule := 'MODULE' 
                    % PushAutoOn  %
                    Ident 
                    % P3StartBuildProgModule  %
                    
                    % PushAutoOff  %
                    [ Priority  ]';' { Import  }
                    % StartBuildInit  %
                    Block 
                    % PushAutoOn  %
                    Ident 
                    % EndBuildFile  %
                    
                    % P3EndBuildProgModule  %
                    '.' 
                    % PopAuto ;
                      EndBuildInit ;
                      PopAuto  %
                    

   first  symbols:moduletok
   
   cannot reachend
*)

# 533 "bnf/m2-h.bnf"
PROCEDURE ProgramModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 533 "bnf/m2-h.bnf"
BEGIN
# 533 "bnf/m2-h.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 533 "bnf/m2-h.bnf"
   PushAutoOn  ;
# 534 "bnf/m2-h.bnf"
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
# 534 "bnf/m2-h.bnf"
   P3StartBuildProgModule  ;
# 535 "bnf/m2-h.bnf"
   PushAutoOff  ;
# 536 "bnf/m2-h.bnf"
# 537 "bnf/m2-h.bnf"
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
# 538 "bnf/m2-h.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 539 "bnf/m2-h.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 540 "bnf/m2-h.bnf"
   StartBuildInit  ;
# 541 "bnf/m2-h.bnf"
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 541 "bnf/m2-h.bnf"
   PushAutoOn  ;
# 542 "bnf/m2-h.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 542 "bnf/m2-h.bnf"
   EndBuildFile  ;
# 543 "bnf/m2-h.bnf"
   P3EndBuildProgModule  ;
# 544 "bnf/m2-h.bnf"
   Expect(periodtok, stopset0, stopset1, stopset2) ;
# 544 "bnf/m2-h.bnf"
# 546 "bnf/m2-h.bnf"
   PopAuto ;
   EndBuildInit ;
   PopAuto  ;
END ProgramModule ;


(*
   ImplementationModule := 'IMPLEMENTATION' 'MODULE' 
                           % PushAutoOn  %
                           Ident 
                           % StartBuildModFile  %
                           
                           % P3StartBuildImpModule  %
                           
                           % PushAutoOff  %
                           [ Priority  ]';' { Import  }
                           % StartBuildInit  %
                           Block 
                           % PushAutoOn  %
                           Ident 
                           % EndBuildFile  %
                           
                           % P3EndBuildImpModule  %
                           '.' 
                           % PopAuto ;
                             EndBuildInit ;
                             PopAuto ;
                             PopAuto  %
                           

   first  symbols:implementationtok
   
   cannot reachend
*)

# 549 "bnf/m2-h.bnf"
PROCEDURE ImplementationModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 549 "bnf/m2-h.bnf"
BEGIN
# 549 "bnf/m2-h.bnf"
   Expect(implementationtok, stopset0, stopset1 + SetOfStop1{moduletok}, stopset2) ;
# 549 "bnf/m2-h.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 549 "bnf/m2-h.bnf"
   PushAutoOn  ;
# 550 "bnf/m2-h.bnf"
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
# 550 "bnf/m2-h.bnf"
   StartBuildModFile  ;
# 551 "bnf/m2-h.bnf"
   P3StartBuildImpModule  ;
# 552 "bnf/m2-h.bnf"
   PushAutoOff  ;
# 553 "bnf/m2-h.bnf"
# 554 "bnf/m2-h.bnf"
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
# 554 "bnf/m2-h.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 555 "bnf/m2-h.bnf"
# 556 "bnf/m2-h.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 556 "bnf/m2-h.bnf"
   StartBuildInit  ;
# 557 "bnf/m2-h.bnf"
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 557 "bnf/m2-h.bnf"
   PushAutoOn  ;
# 559 "bnf/m2-h.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 559 "bnf/m2-h.bnf"
   EndBuildFile  ;
# 560 "bnf/m2-h.bnf"
   P3EndBuildImpModule  ;
# 561 "bnf/m2-h.bnf"
   Expect(periodtok, stopset0, stopset1, stopset2) ;
# 561 "bnf/m2-h.bnf"
# 564 "bnf/m2-h.bnf"
   PopAuto ;
   EndBuildInit ;
   PopAuto ;
   PopAuto  ;
END ImplementationModule ;


(*
   ImplementationOrProgramModule := 
                                    % PushAutoOff  %
                                    ( ImplementationModule  | 
                                      ProgramModule  )
                                    % PopAuto  %
                                    

   first  symbols:moduletok, implementationtok
   
   cannot reachend
*)

# 567 "bnf/m2-h.bnf"
PROCEDURE ImplementationOrProgramModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 567 "bnf/m2-h.bnf"
BEGIN
# 567 "bnf/m2-h.bnf"
   PushAutoOff  ;
# 568 "bnf/m2-h.bnf"
   IF currenttoken=implementationtok
   THEN
      ImplementationModule(stopset0, stopset1, stopset2) ;
# 568 "bnf/m2-h.bnf"
   ELSIF currenttoken=moduletok
   THEN
      ProgramModule(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: MODULE IMPLEMENTATION')
   END ;
# 568 "bnf/m2-h.bnf"
   PopAuto  ;
END ImplementationOrProgramModule ;


(*
   Number := Integer  | Real 

   first  symbols:realtok, integertok
   
   cannot reachend
*)

# 571 "bnf/m2-h.bnf"
PROCEDURE Number (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 571 "bnf/m2-h.bnf"
BEGIN
# 571 "bnf/m2-h.bnf"
   IF currenttoken=integertok
   THEN
      Integer(stopset0, stopset1, stopset2) ;
# 571 "bnf/m2-h.bnf"
   ELSIF currenttoken=realtok
   THEN
      Real(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: real number integer number')
   END ;
END Number ;


(*
   Qualident := 
                % VAR name: Name;
                      Type, Sym: CARDINAL ;  %
                Ident 
                % IF IsAutoPushOn()
                  THEN
                     PopT(name) ;
                     Sym := RequestSym(name) ;
                     IF IsDefImp(Sym) OR IsModule(Sym)
                     THEN
                        Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
                        StartScope(Sym) ;
                        Qualident(stopset0, stopset1, stopset2) ;
                        (* should we test for lack of ident? *)
                        PopTF(Sym, Type) ;
                        PushTF(Sym, Type) ;
                        PutIncluded(Sym) ;
                        EndScope
                     ELSE
                        PushTF(Sym, GetType(Sym))
                     END
                  ELSE (* just parse qualident *)  %
                { '.' Ident  }
                % END  %
                

   first  symbols:identtok
   
   cannot reachend
*)

# 573 "bnf/m2-h.bnf"
PROCEDURE Qualident (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
 name: Name;
                                                                                 Type, Sym: CARDINAL ; 
# 573 "bnf/m2-h.bnf"
BEGIN
# 573 "bnf/m2-h.bnf"
# 576 "bnf/m2-h.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 576 "bnf/m2-h.bnf"
# 593 "bnf/m2-h.bnf"
   IF IsAutoPushOn()
   THEN
      PopT(name) ;
      Sym := RequestSym(name) ;
      IF IsDefImp(Sym) OR IsModule(Sym)
      THEN
         Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
         StartScope(Sym) ;
         Qualident(stopset0, stopset1, stopset2) ;
         (* should we test for lack of ident? *)
         PopTF(Sym, Type) ;
         PushTF(Sym, Type) ;
         PutIncluded(Sym) ;
         EndScope
      ELSE
         PushTF(Sym, GetType(Sym))
      END
   ELSE (* just parse qualident *)  ;
# 594 "bnf/m2-h.bnf"
   WHILE currenttoken=periodtok DO
      Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 594 "bnf/m2-h.bnf"
      Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
   END (* while *) ;
# 594 "bnf/m2-h.bnf"
   END  ;
END Qualident ;


(*
   ConstantDeclaration := 
                          % PushAutoOn  %
                          ( Ident '=' 
                            % BuildConst  %
                            ConstExpression  )
                          % BuildAssignment  %
                          
                          % PopAuto  %
                          

   first  symbols:identtok
   
   cannot reachend
*)

# 597 "bnf/m2-h.bnf"
PROCEDURE ConstantDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 597 "bnf/m2-h.bnf"
BEGIN
# 597 "bnf/m2-h.bnf"
   PushAutoOn  ;
# 598 "bnf/m2-h.bnf"
   Ident(stopset0 + SetOfStop0{equaltok}, stopset1, stopset2) ;
# 598 "bnf/m2-h.bnf"
   Expect(equaltok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 598 "bnf/m2-h.bnf"
   BuildConst  ;
# 599 "bnf/m2-h.bnf"
   ConstExpression(stopset0, stopset1, stopset2) ;
# 599 "bnf/m2-h.bnf"
   BuildAssignment  ;
# 600 "bnf/m2-h.bnf"
   PopAuto  ;
END ConstantDeclaration ;


(*
   ConstExpression := 
                      % PushAutoOn  %
                      SimpleConstExpr [ Relation SimpleConstExpr 
                                        
                                        % BuildRelOp  %
                                         ]
                      % PopAuto  %
                      

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 603 "bnf/m2-h.bnf"
PROCEDURE ConstExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 603 "bnf/m2-h.bnf"
BEGIN
# 603 "bnf/m2-h.bnf"
   PushAutoOn  ;
# 604 "bnf/m2-h.bnf"
   SimpleConstExpr(stopset0 + SetOfStop0{equaltok, hashtok, lessgreatertok, lesstok, lessequaltok, greatertok, greaterequaltok}, stopset1 + SetOfStop1{intok}, stopset2) ;
# 604 "bnf/m2-h.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok})) OR
      (currenttoken=intok)
   THEN
      Relation(stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 604 "bnf/m2-h.bnf"
      SimpleConstExpr(stopset0, stopset1, stopset2) ;
# 604 "bnf/m2-h.bnf"
      BuildRelOp  ;
   END ;
# 605 "bnf/m2-h.bnf"
   PopAuto  ;
END ConstExpression ;


(*
   Relation := '=' 
               % PushT(EqualTok)  %
                | '#' 
               % PushT(HashTok)  %
                | '<>' 
               % PushT(LessGreaterTok)  %
                | '<' 
               % PushT(LessTok)  %
                | '<=' 
               % PushT(LessEqualTok)  %
                | '>' 
               % PushT(GreaterTok)  %
                | '>=' 
               % PushT(GreaterEqualTok)  %
                | 'IN' 
               % PushT(InTok)  %
               

   first  symbols:intok, greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok
   
   cannot reachend
*)

# 608 "bnf/m2-h.bnf"
PROCEDURE Relation (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 608 "bnf/m2-h.bnf"
BEGIN
# 608 "bnf/m2-h.bnf"
   IF currenttoken=equaltok
   THEN
      Expect(equaltok, stopset0, stopset1, stopset2) ;
# 608 "bnf/m2-h.bnf"
      PushT(EqualTok)  ;
# 609 "bnf/m2-h.bnf"
   ELSIF currenttoken=hashtok
   THEN
      Expect(hashtok, stopset0, stopset1, stopset2) ;
# 609 "bnf/m2-h.bnf"
      PushT(HashTok)  ;
# 610 "bnf/m2-h.bnf"
   ELSIF currenttoken=lessgreatertok
   THEN
      Expect(lessgreatertok, stopset0, stopset1, stopset2) ;
# 610 "bnf/m2-h.bnf"
      PushT(LessGreaterTok)  ;
# 611 "bnf/m2-h.bnf"
   ELSIF currenttoken=lesstok
   THEN
      Expect(lesstok, stopset0, stopset1, stopset2) ;
# 611 "bnf/m2-h.bnf"
      PushT(LessTok)  ;
# 612 "bnf/m2-h.bnf"
   ELSIF currenttoken=lessequaltok
   THEN
      Expect(lessequaltok, stopset0, stopset1, stopset2) ;
# 612 "bnf/m2-h.bnf"
      PushT(LessEqualTok)  ;
# 613 "bnf/m2-h.bnf"
   ELSIF currenttoken=greatertok
   THEN
      Expect(greatertok, stopset0, stopset1, stopset2) ;
# 613 "bnf/m2-h.bnf"
      PushT(GreaterTok)  ;
# 614 "bnf/m2-h.bnf"
   ELSIF currenttoken=greaterequaltok
   THEN
      Expect(greaterequaltok, stopset0, stopset1, stopset2) ;
# 614 "bnf/m2-h.bnf"
      PushT(GreaterEqualTok)  ;
# 615 "bnf/m2-h.bnf"
   ELSIF currenttoken=intok
   THEN
      Expect(intok, stopset0, stopset1, stopset2) ;
# 615 "bnf/m2-h.bnf"
      PushT(InTok)  ;
   ELSE
      ErrorArray('expecting one of: IN >= > <= < <> # =')
   END ;
END Relation ;


(*
   SimpleConstExpr := UnaryOrConstTerm { AddOperator ConstTerm 
                                         
                                         % BuildBinaryOp  %
                                          }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 618 "bnf/m2-h.bnf"
PROCEDURE SimpleConstExpr (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 618 "bnf/m2-h.bnf"
BEGIN
# 618 "bnf/m2-h.bnf"
   UnaryOrConstTerm(stopset0 + SetOfStop0{plustok, minustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
# 618 "bnf/m2-h.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok})) OR
         (currenttoken=ortok) DO
      AddOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 618 "bnf/m2-h.bnf"
      ConstTerm(stopset0 + SetOfStop0{minustok, plustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
# 618 "bnf/m2-h.bnf"
      BuildBinaryOp  ;
   END (* while *) ;
END SimpleConstExpr ;


(*
   UnaryOrConstTerm := '+' 
                       % PushT(PlusTok)  %
                       ConstTerm 
                       % BuildUnaryOp  %
                        | '-' 
                       % PushT(MinusTok)  %
                       ConstTerm 
                       % BuildUnaryOp  %
                        | ConstTerm 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 621 "bnf/m2-h.bnf"
PROCEDURE UnaryOrConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 621 "bnf/m2-h.bnf"
BEGIN
# 621 "bnf/m2-h.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 621 "bnf/m2-h.bnf"
      PushT(PlusTok)  ;
# 622 "bnf/m2-h.bnf"
      ConstTerm(stopset0, stopset1, stopset2) ;
# 622 "bnf/m2-h.bnf"
      BuildUnaryOp  ;
# 624 "bnf/m2-h.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 624 "bnf/m2-h.bnf"
      PushT(MinusTok)  ;
# 625 "bnf/m2-h.bnf"
      ConstTerm(stopset0, stopset1, stopset2) ;
# 625 "bnf/m2-h.bnf"
      BuildUnaryOp  ;
# 627 "bnf/m2-h.bnf"
   ELSIF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok})) OR
         (currenttoken=nottok) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {stringtok, realtok, integertok, identtok}))
   THEN
      ConstTerm(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: NOT ( string real number integer number identifier { - +')
   END ;
END UnaryOrConstTerm ;


(*
   AddOperator := '+' 
                  % PushT(PlusTok) ;
                    RecordOp  %
                   | '-' 
                  % PushT(MinusTok) ;
                    RecordOp  %
                   | 'OR' 
                  % PushT(OrTok) ;
                    RecordOp  %
                  

   first  symbols:ortok, minustok, plustok
   
   cannot reachend
*)

# 629 "bnf/m2-h.bnf"
PROCEDURE AddOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 629 "bnf/m2-h.bnf"
BEGIN
# 629 "bnf/m2-h.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0, stopset1, stopset2) ;
# 629 "bnf/m2-h.bnf"
# 630 "bnf/m2-h.bnf"
      PushT(PlusTok) ;
      RecordOp  ;
# 631 "bnf/m2-h.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0, stopset1, stopset2) ;
# 631 "bnf/m2-h.bnf"
# 632 "bnf/m2-h.bnf"
      PushT(MinusTok) ;
      RecordOp  ;
# 633 "bnf/m2-h.bnf"
   ELSIF currenttoken=ortok
   THEN
      Expect(ortok, stopset0, stopset1, stopset2) ;
# 633 "bnf/m2-h.bnf"
# 634 "bnf/m2-h.bnf"
      PushT(OrTok) ;
      RecordOp  ;
   ELSE
      ErrorArray('expecting one of: OR - +')
   END ;
END AddOperator ;


(*
   ConstTerm := ConstFactor { MulOperator ConstFactor 
                              % BuildBinaryOp  %
                               }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok
   
   cannot reachend
*)

# 637 "bnf/m2-h.bnf"
PROCEDURE ConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 637 "bnf/m2-h.bnf"
BEGIN
# 637 "bnf/m2-h.bnf"
   ConstFactor(stopset0 + SetOfStop0{timestok, dividetok, andtok, ambersandtok}, stopset1 + SetOfStop1{divtok, modtok}, stopset2) ;
# 637 "bnf/m2-h.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {ambersandtok, andtok, dividetok, timestok})) OR
         ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {modtok, divtok})) DO
      MulOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 637 "bnf/m2-h.bnf"
      ConstFactor(stopset0 + SetOfStop0{ambersandtok, andtok, dividetok, timestok}, stopset1 + SetOfStop1{modtok, divtok}, stopset2) ;
# 637 "bnf/m2-h.bnf"
      BuildBinaryOp  ;
   END (* while *) ;
END ConstTerm ;


(*
   MulOperator := '*' 
                  % PushT(TimesTok) ;
                    RecordOp  %
                   | '/' 
                  % PushT(DivTok) ;
                    RecordOp  %
                   | 'DIV' 
                  % PushT(DivTok) ;
                    RecordOp  %
                   | 'MOD' 
                  % PushT(ModTok) ;
                    RecordOp  %
                   | 'AND' 
                  % PushT(AndTok) ;
                    RecordOp  %
                   | '&' 
                  % PushT(AmbersandTok) ;
                    RecordOp  %
                  

   first  symbols:ambersandtok, andtok, modtok, divtok, dividetok, timestok
   
   cannot reachend
*)

# 640 "bnf/m2-h.bnf"
PROCEDURE MulOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 640 "bnf/m2-h.bnf"
BEGIN
# 640 "bnf/m2-h.bnf"
   IF currenttoken=timestok
   THEN
      Expect(timestok, stopset0, stopset1, stopset2) ;
# 640 "bnf/m2-h.bnf"
# 641 "bnf/m2-h.bnf"
      PushT(TimesTok) ;
      RecordOp  ;
# 642 "bnf/m2-h.bnf"
   ELSIF currenttoken=dividetok
   THEN
      Expect(dividetok, stopset0, stopset1, stopset2) ;
# 642 "bnf/m2-h.bnf"
# 643 "bnf/m2-h.bnf"
      PushT(DivTok) ;
      RecordOp  ;
# 644 "bnf/m2-h.bnf"
   ELSIF currenttoken=divtok
   THEN
      Expect(divtok, stopset0, stopset1, stopset2) ;
# 644 "bnf/m2-h.bnf"
# 645 "bnf/m2-h.bnf"
      PushT(DivTok) ;
      RecordOp  ;
# 646 "bnf/m2-h.bnf"
   ELSIF currenttoken=modtok
   THEN
      Expect(modtok, stopset0, stopset1, stopset2) ;
# 646 "bnf/m2-h.bnf"
# 647 "bnf/m2-h.bnf"
      PushT(ModTok) ;
      RecordOp  ;
# 648 "bnf/m2-h.bnf"
   ELSIF currenttoken=andtok
   THEN
      Expect(andtok, stopset0, stopset1, stopset2) ;
# 648 "bnf/m2-h.bnf"
# 649 "bnf/m2-h.bnf"
      PushT(AndTok) ;
      RecordOp  ;
# 650 "bnf/m2-h.bnf"
   ELSIF currenttoken=ambersandtok
   THEN
      Expect(ambersandtok, stopset0, stopset1, stopset2) ;
# 650 "bnf/m2-h.bnf"
# 651 "bnf/m2-h.bnf"
      PushT(AmbersandTok) ;
      RecordOp  ;
   ELSE
      ErrorArray('expecting one of: & AND MOD DIV / *')
   END ;
END MulOperator ;


(*
   ConstFactor := ConstQualidentOrSet  | 
                  Number  | ConstString  | '(' ConstExpression 
                  ')'  | ( 'NOT' ConstFactor 
                           % BuildNot  %
                            )

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok
   
   cannot reachend
*)

# 654 "bnf/m2-h.bnf"
PROCEDURE ConstFactor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 654 "bnf/m2-h.bnf"
BEGIN
# 654 "bnf/m2-h.bnf"
   IF (currenttoken=lcbratok) OR
      (currenttoken=identtok)
   THEN
      ConstQualidentOrSet(stopset0, stopset1, stopset2) ;
# 654 "bnf/m2-h.bnf"
   ELSIF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {realtok, integertok}))
   THEN
      Number(stopset0, stopset1, stopset2) ;
# 655 "bnf/m2-h.bnf"
   ELSIF currenttoken=stringtok
   THEN
      ConstString(stopset0, stopset1, stopset2) ;
# 656 "bnf/m2-h.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 656 "bnf/m2-h.bnf"
      ConstExpression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 656 "bnf/m2-h.bnf"
      Expect(rparatok, stopset0, stopset1, stopset2) ;
# 656 "bnf/m2-h.bnf"
   Expect(nottok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 656 "bnf/m2-h.bnf"
   ConstFactor(stopset0, stopset1, stopset2) ;
# 656 "bnf/m2-h.bnf"
   BuildNot  ;
   ELSE
      ErrorArray('expecting one of: NOT ( string real number integer number identifier {')
   END ;
END ConstFactor ;


(*
   ConstString := string 

   first  symbols:stringtok
   
   cannot reachend
*)

# 662 "bnf/m2-h.bnf"
PROCEDURE ConstString (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 662 "bnf/m2-h.bnf"
BEGIN
# 662 "bnf/m2-h.bnf"
# 663 "bnf/m2-h.bnf"
   string(stopset0, stopset1, stopset2) ;
END ConstString ;


(*
   ConstQualidentOrSet := SimpleSet 
                          % BuildBitsetStart  %
                           | Qualident [ SimpleSet  ]

   first  symbols:identtok, lcbratok
   
   cannot reachend
*)

# 665 "bnf/m2-h.bnf"
PROCEDURE ConstQualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 665 "bnf/m2-h.bnf"
BEGIN
# 665 "bnf/m2-h.bnf"
   IF currenttoken=lcbratok
   THEN
      SimpleSet(stopset0, stopset1, stopset2) ;
# 665 "bnf/m2-h.bnf"
      BuildBitsetStart  ;
# 666 "bnf/m2-h.bnf"
   ELSIF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok}, stopset1, stopset2) ;
# 666 "bnf/m2-h.bnf"
      IF currenttoken=lcbratok
      THEN
         SimpleSet(stopset0, stopset1, stopset2) ;
      END ;
   ELSE
      ErrorArray('expecting one of: identifier {')
   END ;
END ConstQualidentOrSet ;


(*
   QualidentOrSet := SimpleSet  | Qualident [ SimpleSet  ]

   first  symbols:identtok, lcbratok
   
   cannot reachend
*)

# 669 "bnf/m2-h.bnf"
PROCEDURE QualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 669 "bnf/m2-h.bnf"
BEGIN
# 669 "bnf/m2-h.bnf"
# 670 "bnf/m2-h.bnf"
   IF currenttoken=lcbratok
   THEN
      SimpleSet(stopset0, stopset1, stopset2) ;
# 670 "bnf/m2-h.bnf"
   ELSIF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok}, stopset1, stopset2) ;
# 670 "bnf/m2-h.bnf"
      IF currenttoken=lcbratok
      THEN
         SimpleSet(stopset0, stopset1, stopset2) ;
      END ;
   ELSE
      ErrorArray('expecting one of: identifier {')
   END ;
END QualidentOrSet ;


(*
   Element := ConstExpression ( '..' ConstExpression 
                                % BuildSetRange  %
                                 | 
                                % BuildElement (* epsilon *)  %
                                 )

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   reachend
*)

# 674 "bnf/m2-h.bnf"
PROCEDURE Element (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 674 "bnf/m2-h.bnf"
BEGIN
# 674 "bnf/m2-h.bnf"
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 674 "bnf/m2-h.bnf"
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 674 "bnf/m2-h.bnf"
      ConstExpression(stopset0, stopset1, stopset2) ;
# 674 "bnf/m2-h.bnf"
      BuildSetRange  ;
# 675 "bnf/m2-h.bnf"
   ELSE
# 675 "bnf/m2-h.bnf"
      BuildElement (* epsilon *)  ;
   END ;
END Element ;


(*
   TypeDeclaration := Ident '=' Type 

   first  symbols:identtok
   
   cannot reachend
*)

# 678 "bnf/m2-h.bnf"
PROCEDURE TypeDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 678 "bnf/m2-h.bnf"
BEGIN
# 678 "bnf/m2-h.bnf"
   Ident(stopset0 + SetOfStop0{equaltok}, stopset1, stopset2) ;
# 678 "bnf/m2-h.bnf"
   Expect(equaltok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 679 "bnf/m2-h.bnf"
   Type(stopset0, stopset1, stopset2) ;
END TypeDeclaration ;


(*
   Type := 
           % PushAutoOff  %
           ( SimpleType  | ArrayType  | RecordType  | 
             SetType  | PointerType  | ProcedureType  )
           % PopAuto  %
           

   first  symbols:proceduretok, pointertok, settok, recordtok, arraytok, lsbratok, lparatok, identtok
   
   cannot reachend
*)

# 681 "bnf/m2-h.bnf"
PROCEDURE Type (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 681 "bnf/m2-h.bnf"
BEGIN
# 681 "bnf/m2-h.bnf"
# 682 "bnf/m2-h.bnf"
   PushAutoOff  ;
# 683 "bnf/m2-h.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lsbratok, lparatok})) OR
      (currenttoken=identtok)
   THEN
      SimpleType(stopset0, stopset1, stopset2) ;
# 684 "bnf/m2-h.bnf"
   ELSIF currenttoken=arraytok
   THEN
      ArrayType(stopset0, stopset1, stopset2) ;
# 684 "bnf/m2-h.bnf"
   ELSIF currenttoken=recordtok
   THEN
      RecordType(stopset0, stopset1, stopset2) ;
# 685 "bnf/m2-h.bnf"
   ELSIF currenttoken=settok
   THEN
      SetType(stopset0, stopset1, stopset2) ;
# 686 "bnf/m2-h.bnf"
   ELSIF currenttoken=pointertok
   THEN
      PointerType(stopset0, stopset1, stopset2) ;
# 687 "bnf/m2-h.bnf"
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureType(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: PROCEDURE POINTER SET RECORD ARRAY [ ( identifier')
   END ;
# 687 "bnf/m2-h.bnf"
   PopAuto  ;
END Type ;


(*
   SimpleType := Qualident  | Enumeration  | SubrangeType 

   first  symbols:lsbratok, lparatok, identtok
   
   cannot reachend
*)

# 690 "bnf/m2-h.bnf"
PROCEDURE SimpleType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 690 "bnf/m2-h.bnf"
BEGIN
# 690 "bnf/m2-h.bnf"
   IF currenttoken=identtok
   THEN
      Qualident(stopset0, stopset1, stopset2) ;
# 690 "bnf/m2-h.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Enumeration(stopset0, stopset1, stopset2) ;
# 690 "bnf/m2-h.bnf"
   ELSIF currenttoken=lsbratok
   THEN
      SubrangeType(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: [ ( identifier')
   END ;
END SimpleType ;


(*
   Enumeration := '(' ( IdentList  )')' 

   first  symbols:lparatok
   
   cannot reachend
*)

# 692 "bnf/m2-h.bnf"
PROCEDURE Enumeration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 692 "bnf/m2-h.bnf"
BEGIN
# 692 "bnf/m2-h.bnf"
   Expect(lparatok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 693 "bnf/m2-h.bnf"
   IdentList(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 695 "bnf/m2-h.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END Enumeration ;


(*
   IdentList := Ident 
                % VAR
                                                                                                  on: BOOLEAN ;
                                                                                                  n : CARDINAL ;  %
                
                % on := IsAutoPushOn() ;
                  IF on
                  THEN
                     n := 1
                  END  %
                { ',' Ident 
                  % IF on
                    THEN
                       INC(n)
                    END  %
                   }
                % IF on
                  THEN
                     PushT(n)
                  END  %
                

   first  symbols:identtok
   
   cannot reachend
*)

# 698 "bnf/m2-h.bnf"
PROCEDURE IdentList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR

                                                                                on: BOOLEAN ;
                                                                                n : CARDINAL ; 
# 698 "bnf/m2-h.bnf"
BEGIN
# 698 "bnf/m2-h.bnf"
   Ident(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 698 "bnf/m2-h.bnf"
# 701 "bnf/m2-h.bnf"
# 705 "bnf/m2-h.bnf"
   on := IsAutoPushOn() ;
   IF on
   THEN
      n := 1
   END  ;
# 706 "bnf/m2-h.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 706 "bnf/m2-h.bnf"
      Ident(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 706 "bnf/m2-h.bnf"
# 709 "bnf/m2-h.bnf"
      IF on
      THEN
         INC(n)
      END  ;
   END (* while *) ;
# 710 "bnf/m2-h.bnf"
# 713 "bnf/m2-h.bnf"
   IF on
   THEN
      PushT(n)
   END  ;
END IdentList ;


(*
   SubrangeType := '[' ConstExpression '..' ConstExpression ']' 
                   
                   % BuildSubrange ;  %
                   

   first  symbols:lsbratok
   
   cannot reachend
*)

# 716 "bnf/m2-h.bnf"
PROCEDURE SubrangeType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 716 "bnf/m2-h.bnf"
BEGIN
# 716 "bnf/m2-h.bnf"
   Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 716 "bnf/m2-h.bnf"
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 716 "bnf/m2-h.bnf"
   Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 716 "bnf/m2-h.bnf"
   ConstExpression(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 716 "bnf/m2-h.bnf"
   Expect(rsbratok, stopset0, stopset1, stopset2) ;
# 716 "bnf/m2-h.bnf"
   BuildSubrange ;  ;
END SubrangeType ;


(*
   ArrayType := 'ARRAY' SimpleType { ',' SimpleType  }'OF' Type 

   first  symbols:arraytok
   
   cannot reachend
*)

# 719 "bnf/m2-h.bnf"
PROCEDURE ArrayType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 719 "bnf/m2-h.bnf"
BEGIN
# 719 "bnf/m2-h.bnf"
   Expect(arraytok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 722 "bnf/m2-h.bnf"
   SimpleType(stopset0 + SetOfStop0{commatok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 722 "bnf/m2-h.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 724 "bnf/m2-h.bnf"
      SimpleType(stopset0 + SetOfStop0{commatok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
   END (* while *) ;
# 724 "bnf/m2-h.bnf"
   Expect(oftok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 726 "bnf/m2-h.bnf"
   Type(stopset0, stopset1, stopset2) ;
END ArrayType ;


(*
   RecordType := 'RECORD' FieldListSequence 'END' 

   first  symbols:recordtok
   
   cannot reachend
*)

# 728 "bnf/m2-h.bnf"
PROCEDURE RecordType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 728 "bnf/m2-h.bnf"
BEGIN
# 728 "bnf/m2-h.bnf"
   Expect(recordtok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok, endtok}, stopset2 + SetOfStop2{identtok}) ;
# 729 "bnf/m2-h.bnf"
   FieldListSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 729 "bnf/m2-h.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END RecordType ;


(*
   FieldListSequence := FieldListStatement { ';' FieldListStatement  }

   first  symbols:semicolontok, casetok, identtok
   
   reachend
*)

# 732 "bnf/m2-h.bnf"
PROCEDURE FieldListSequence (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 732 "bnf/m2-h.bnf"
BEGIN
# 732 "bnf/m2-h.bnf"
   FieldListStatement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 732 "bnf/m2-h.bnf"
   WHILE currenttoken=semicolontok DO
      Expect(semicolontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok}, stopset2 + SetOfStop2{identtok}) ;
# 732 "bnf/m2-h.bnf"
      FieldListStatement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END (* while *) ;
END FieldListSequence ;


(*
   FieldListStatement := [ FieldList  ]

   first  symbols:casetok, identtok
   
   reachend
*)

# 735 "bnf/m2-h.bnf"
PROCEDURE FieldListStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 735 "bnf/m2-h.bnf"
BEGIN
# 735 "bnf/m2-h.bnf"
   IF (currenttoken=casetok) OR
      (currenttoken=identtok)
   THEN
      FieldList(stopset0, stopset1, stopset2) ;
   END ;
END FieldListStatement ;


(*
   FieldList := IdentList ':' Type  | 'CASE' Ident [ ':' Qualident  | 
                                                     '.' Qualident  ]
                'OF' Varient { '|' Varient  }[ 'ELSE' FieldListSequence  ]
                'END' 

   first  symbols:casetok, identtok
   
   cannot reachend
*)

# 746 "bnf/m2-h.bnf"
PROCEDURE FieldList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 746 "bnf/m2-h.bnf"
BEGIN
# 746 "bnf/m2-h.bnf"
   IF currenttoken=identtok
   THEN
      IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 746 "bnf/m2-h.bnf"
      Expect(colontok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 748 "bnf/m2-h.bnf"
      Type(stopset0, stopset1, stopset2) ;
# 749 "bnf/m2-h.bnf"
   ELSIF currenttoken=casetok
   THEN
      Expect(casetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 749 "bnf/m2-h.bnf"
      Ident(stopset0 + SetOfStop0{colontok, periodtok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 749 "bnf/m2-h.bnf"
      IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {periodtok, colontok}))
      THEN
         (* seen optional [ | ] expression *)
# 749 "bnf/m2-h.bnf"
         IF currenttoken=colontok
         THEN
            Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 750 "bnf/m2-h.bnf"
            Qualident(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 751 "bnf/m2-h.bnf"
         ELSIF currenttoken=periodtok
         THEN
            Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 753 "bnf/m2-h.bnf"
            Qualident(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
         ELSE
            ErrorArray('expecting one of: . :')
         END ;
         (* end of optional [ | ] expression *)
      END ;
# 753 "bnf/m2-h.bnf"
      Expect(oftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 757 "bnf/m2-h.bnf"
      Varient(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{elsetok, endtok}, stopset2) ;
# 757 "bnf/m2-h.bnf"
      WHILE currenttoken=bartok DO
         Expect(bartok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 759 "bnf/m2-h.bnf"
         Varient(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{endtok, elsetok}, stopset2) ;
      END (* while *) ;
# 760 "bnf/m2-h.bnf"
      IF currenttoken=elsetok
      THEN
         Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok, endtok}, stopset2 + SetOfStop2{identtok}) ;
# 762 "bnf/m2-h.bnf"
         FieldListSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
      END ;
# 762 "bnf/m2-h.bnf"
      Expect(endtok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: CASE identifier')
   END ;
END FieldList ;


(*
   Varient := SilentCaseLabelList ':' FieldListSequence 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 765 "bnf/m2-h.bnf"
PROCEDURE Varient (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 765 "bnf/m2-h.bnf"
BEGIN
# 765 "bnf/m2-h.bnf"
   SilentCaseLabelList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 765 "bnf/m2-h.bnf"
   Expect(colontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok}, stopset2 + SetOfStop2{identtok}) ;
# 765 "bnf/m2-h.bnf"
   FieldListSequence(stopset0, stopset1, stopset2) ;
END Varient ;


(*
   SilentCaseLabelList := SilentCaseLabels { ',' SilentCaseLabels  }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 767 "bnf/m2-h.bnf"
PROCEDURE SilentCaseLabelList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 767 "bnf/m2-h.bnf"
BEGIN
# 767 "bnf/m2-h.bnf"
   SilentCaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 767 "bnf/m2-h.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 767 "bnf/m2-h.bnf"
      SilentCaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END SilentCaseLabelList ;


(*
   SilentCaseLabels := SilentConstExpression [ '..' SilentConstExpression  ]

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 769 "bnf/m2-h.bnf"
PROCEDURE SilentCaseLabels (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 769 "bnf/m2-h.bnf"
BEGIN
# 769 "bnf/m2-h.bnf"
   SilentConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 769 "bnf/m2-h.bnf"
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 769 "bnf/m2-h.bnf"
      SilentConstExpression(stopset0, stopset1, stopset2) ;
   END ;
END SilentCaseLabels ;


(*
   SilentConstExpression := 
                            % PushAutoOff  %
                            SilentSimpleConstExpr [ SilentRelation 
                                                    SilentSimpleConstExpr  ]
                            
                            % PopAuto  %
                            

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 779 "bnf/m2-h.bnf"
PROCEDURE SilentConstExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 779 "bnf/m2-h.bnf"
BEGIN
# 779 "bnf/m2-h.bnf"
   PushAutoOff  ;
# 781 "bnf/m2-h.bnf"
   SilentSimpleConstExpr(stopset0 + SetOfStop0{equaltok, hashtok, lessgreatertok, lesstok, lessequaltok, greatertok, greaterequaltok}, stopset1 + SetOfStop1{intok}, stopset2) ;
# 781 "bnf/m2-h.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok})) OR
      (currenttoken=intok)
   THEN
      SilentRelation(stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 781 "bnf/m2-h.bnf"
      SilentSimpleConstExpr(stopset0, stopset1, stopset2) ;
   END ;
# 781 "bnf/m2-h.bnf"
   PopAuto  ;
END SilentConstExpression ;


(*
   SilentRelation := '='  | '#'  | '<>'  | '<'  | '<='  | 
                     '>'  | '>='  | 'IN' 

   first  symbols:intok, greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok
   
   cannot reachend
*)

# 784 "bnf/m2-h.bnf"
PROCEDURE SilentRelation (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 784 "bnf/m2-h.bnf"
BEGIN
# 784 "bnf/m2-h.bnf"
   IF currenttoken=equaltok
   THEN
      Expect(equaltok, stopset0, stopset1, stopset2) ;
# 784 "bnf/m2-h.bnf"
   ELSIF currenttoken=hashtok
   THEN
      Expect(hashtok, stopset0, stopset1, stopset2) ;
# 784 "bnf/m2-h.bnf"
   ELSIF currenttoken=lessgreatertok
   THEN
      Expect(lessgreatertok, stopset0, stopset1, stopset2) ;
# 784 "bnf/m2-h.bnf"
   ELSIF currenttoken=lesstok
   THEN
      Expect(lesstok, stopset0, stopset1, stopset2) ;
# 784 "bnf/m2-h.bnf"
   ELSIF currenttoken=lessequaltok
   THEN
      Expect(lessequaltok, stopset0, stopset1, stopset2) ;
# 784 "bnf/m2-h.bnf"
   ELSIF currenttoken=greatertok
   THEN
      Expect(greatertok, stopset0, stopset1, stopset2) ;
# 784 "bnf/m2-h.bnf"
   ELSIF currenttoken=greaterequaltok
   THEN
      Expect(greaterequaltok, stopset0, stopset1, stopset2) ;
# 784 "bnf/m2-h.bnf"
   ELSIF currenttoken=intok
   THEN
      Expect(intok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: IN >= > <= < <> # =')
   END ;
END SilentRelation ;


(*
   SilentSimpleConstExpr := SilentUnaryOrConstTerm { SilentAddOperator 
                                                     SilentConstTerm  }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 786 "bnf/m2-h.bnf"
PROCEDURE SilentSimpleConstExpr (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 786 "bnf/m2-h.bnf"
BEGIN
# 786 "bnf/m2-h.bnf"
   SilentUnaryOrConstTerm(stopset0 + SetOfStop0{plustok, minustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
# 786 "bnf/m2-h.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok})) OR
         (currenttoken=ortok) DO
      SilentAddOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 786 "bnf/m2-h.bnf"
      SilentConstTerm(stopset0 + SetOfStop0{minustok, plustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
   END (* while *) ;
END SilentSimpleConstExpr ;


(*
   SilentUnaryOrConstTerm := '+' SilentConstTerm  | 
                             '-' SilentConstTerm  | 
                             SilentConstTerm 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 788 "bnf/m2-h.bnf"
PROCEDURE SilentUnaryOrConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 788 "bnf/m2-h.bnf"
BEGIN
# 788 "bnf/m2-h.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 788 "bnf/m2-h.bnf"
      SilentConstTerm(stopset0, stopset1, stopset2) ;
# 788 "bnf/m2-h.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 788 "bnf/m2-h.bnf"
      SilentConstTerm(stopset0, stopset1, stopset2) ;
# 788 "bnf/m2-h.bnf"
   ELSIF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok})) OR
         (currenttoken=nottok) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {stringtok, realtok, integertok, identtok}))
   THEN
      SilentConstTerm(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: NOT ( string real number integer number identifier { - +')
   END ;
END SilentUnaryOrConstTerm ;


(*
   SilentAddOperator := '+'  | '-'  | 'OR' 

   first  symbols:ortok, minustok, plustok
   
   cannot reachend
*)

# 790 "bnf/m2-h.bnf"
PROCEDURE SilentAddOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 790 "bnf/m2-h.bnf"
BEGIN
# 790 "bnf/m2-h.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0, stopset1, stopset2) ;
# 790 "bnf/m2-h.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0, stopset1, stopset2) ;
# 790 "bnf/m2-h.bnf"
   ELSIF currenttoken=ortok
   THEN
      Expect(ortok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: OR - +')
   END ;
END SilentAddOperator ;


(*
   SilentConstTerm := SilentConstFactor { SilentMulOperator SilentConstFactor  }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok
   
   cannot reachend
*)

# 792 "bnf/m2-h.bnf"
PROCEDURE SilentConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 792 "bnf/m2-h.bnf"
BEGIN
# 792 "bnf/m2-h.bnf"
   SilentConstFactor(stopset0 + SetOfStop0{timestok, dividetok, andtok, ambersandtok}, stopset1 + SetOfStop1{divtok, modtok}, stopset2) ;
# 792 "bnf/m2-h.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {ambersandtok, andtok, dividetok, timestok})) OR
         ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {modtok, divtok})) DO
      SilentMulOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 792 "bnf/m2-h.bnf"
      SilentConstFactor(stopset0 + SetOfStop0{ambersandtok, andtok, dividetok, timestok}, stopset1 + SetOfStop1{modtok, divtok}, stopset2) ;
   END (* while *) ;
END SilentConstTerm ;


(*
   SilentMulOperator := '*'  | '/'  | 'DIV'  | 'MOD'  | 
                        'AND'  | '&' 

   first  symbols:ambersandtok, andtok, modtok, divtok, dividetok, timestok
   
   cannot reachend
*)

# 794 "bnf/m2-h.bnf"
PROCEDURE SilentMulOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 794 "bnf/m2-h.bnf"
BEGIN
# 794 "bnf/m2-h.bnf"
   IF currenttoken=timestok
   THEN
      Expect(timestok, stopset0, stopset1, stopset2) ;
# 794 "bnf/m2-h.bnf"
   ELSIF currenttoken=dividetok
   THEN
      Expect(dividetok, stopset0, stopset1, stopset2) ;
# 794 "bnf/m2-h.bnf"
   ELSIF currenttoken=divtok
   THEN
      Expect(divtok, stopset0, stopset1, stopset2) ;
# 794 "bnf/m2-h.bnf"
   ELSIF currenttoken=modtok
   THEN
      Expect(modtok, stopset0, stopset1, stopset2) ;
# 794 "bnf/m2-h.bnf"
   ELSIF currenttoken=andtok
   THEN
      Expect(andtok, stopset0, stopset1, stopset2) ;
# 794 "bnf/m2-h.bnf"
   ELSIF currenttoken=ambersandtok
   THEN
      Expect(ambersandtok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: & AND MOD DIV / *')
   END ;
END SilentMulOperator ;


(*
   SilentConstFactor := SilentConstQualidentOrSet  | 
                        Number  | SilentConstString  | 
                        '(' SilentConstExpression ')'  | 
                        'NOT' SilentConstFactor 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok
   
   cannot reachend
*)

# 796 "bnf/m2-h.bnf"
PROCEDURE SilentConstFactor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 796 "bnf/m2-h.bnf"
BEGIN
# 796 "bnf/m2-h.bnf"
   IF (currenttoken=lcbratok) OR
      (currenttoken=identtok)
   THEN
      SilentConstQualidentOrSet(stopset0, stopset1, stopset2) ;
# 796 "bnf/m2-h.bnf"
   ELSIF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {realtok, integertok}))
   THEN
      Number(stopset0, stopset1, stopset2) ;
# 797 "bnf/m2-h.bnf"
   ELSIF currenttoken=stringtok
   THEN
      SilentConstString(stopset0, stopset1, stopset2) ;
# 798 "bnf/m2-h.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 798 "bnf/m2-h.bnf"
      SilentConstExpression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 798 "bnf/m2-h.bnf"
      Expect(rparatok, stopset0, stopset1, stopset2) ;
# 798 "bnf/m2-h.bnf"
   ELSIF currenttoken=nottok
   THEN
      Expect(nottok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 798 "bnf/m2-h.bnf"
      SilentConstFactor(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: NOT ( string real number integer number identifier {')
   END ;
END SilentConstFactor ;


(*
   SilentConstString := string 

   first  symbols:stringtok
   
   cannot reachend
*)

# 800 "bnf/m2-h.bnf"
PROCEDURE SilentConstString (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 800 "bnf/m2-h.bnf"
BEGIN
# 800 "bnf/m2-h.bnf"
   string(stopset0, stopset1, stopset2) ;
END SilentConstString ;


(*
   SilentConstQualidentOrSet := SilentSimpleSet  | 
                                Qualident [ SilentSimpleSet  ]

   first  symbols:identtok, lcbratok
   
   cannot reachend
*)

# 802 "bnf/m2-h.bnf"
PROCEDURE SilentConstQualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 802 "bnf/m2-h.bnf"
BEGIN
# 802 "bnf/m2-h.bnf"
   IF currenttoken=lcbratok
   THEN
      SilentSimpleSet(stopset0, stopset1, stopset2) ;
# 802 "bnf/m2-h.bnf"
   ELSIF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok}, stopset1, stopset2) ;
# 802 "bnf/m2-h.bnf"
      IF currenttoken=lcbratok
      THEN
         SilentSimpleSet(stopset0, stopset1, stopset2) ;
      END ;
   ELSE
      ErrorArray('expecting one of: identifier {')
   END ;
END SilentConstQualidentOrSet ;


(*
   SilentSimpleSet := '{' [ SilentElement { ',' SilentElement  } ]
                      '}' 

   first  symbols:lcbratok
   
   cannot reachend
*)

# 804 "bnf/m2-h.bnf"
PROCEDURE SilentSimpleSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 804 "bnf/m2-h.bnf"
BEGIN
# 804 "bnf/m2-h.bnf"
   Expect(lcbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 804 "bnf/m2-h.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {commatok, lparatok, lcbratok, minustok, plustok})) OR
      (currenttoken=nottok) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {stringtok, realtok, integertok, identtok}))
   THEN
      SilentElement(stopset0 + SetOfStop0{commatok, rcbratok}, stopset1, stopset2) ;
# 804 "bnf/m2-h.bnf"
      WHILE currenttoken=commatok DO
         Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok, commatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 804 "bnf/m2-h.bnf"
         SilentElement(stopset0 + SetOfStop0{rcbratok, commatok}, stopset1, stopset2) ;
      END (* while *) ;
   END ;
# 804 "bnf/m2-h.bnf"
   Expect(rcbratok, stopset0, stopset1, stopset2) ;
END SilentSimpleSet ;


(*
   SilentElement := SilentConstExpression [ '..' SilentConstExpression  ]

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   reachend
*)

# 806 "bnf/m2-h.bnf"
PROCEDURE SilentElement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 806 "bnf/m2-h.bnf"
BEGIN
# 806 "bnf/m2-h.bnf"
   SilentConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 806 "bnf/m2-h.bnf"
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 806 "bnf/m2-h.bnf"
      SilentConstExpression(stopset0, stopset1, stopset2) ;
   END ;
END SilentElement ;


(*
   SetType := 'SET' 'OF' SimpleType 

   first  symbols:settok
   
   cannot reachend
*)

# 810 "bnf/m2-h.bnf"
PROCEDURE SetType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 810 "bnf/m2-h.bnf"
BEGIN
# 810 "bnf/m2-h.bnf"
   Expect(settok, stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 810 "bnf/m2-h.bnf"
   Expect(oftok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 810 "bnf/m2-h.bnf"
   SimpleType(stopset0, stopset1, stopset2) ;
END SetType ;


(*
   PointerType := 'POINTER' 'TO' Type 

   first  symbols:pointertok
   
   cannot reachend
*)

# 812 "bnf/m2-h.bnf"
PROCEDURE PointerType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 812 "bnf/m2-h.bnf"
BEGIN
# 812 "bnf/m2-h.bnf"
   Expect(pointertok, stopset0, stopset1, stopset2 + SetOfStop2{totok}) ;
# 812 "bnf/m2-h.bnf"
   Expect(totok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 814 "bnf/m2-h.bnf"
   Type(stopset0, stopset1, stopset2) ;
END PointerType ;


(*
   ProcedureType := 'PROCEDURE' [ FormalTypeList  ]

   first  symbols:proceduretok
   
   cannot reachend
*)

# 816 "bnf/m2-h.bnf"
PROCEDURE ProcedureType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 816 "bnf/m2-h.bnf"
BEGIN
# 816 "bnf/m2-h.bnf"
   Expect(proceduretok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 817 "bnf/m2-h.bnf"
   IF currenttoken=lparatok
   THEN
      FormalTypeList(stopset0, stopset1, stopset2) ;
   END ;
END ProcedureType ;


(*
   FormalTypeList := '(' ( ')' FormalReturn  | 
                           ProcedureParameters ')' FormalReturn  )

   first  symbols:lparatok
   
   cannot reachend
*)

# 819 "bnf/m2-h.bnf"
PROCEDURE FormalTypeList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 819 "bnf/m2-h.bnf"
BEGIN
# 819 "bnf/m2-h.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{rparatok, arraytok}, stopset1, stopset2 + SetOfStop2{periodperiodperiodtok, vartok, identtok}) ;
# 819 "bnf/m2-h.bnf"
   IF currenttoken=rparatok
   THEN
      Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 819 "bnf/m2-h.bnf"
      FormalReturn(stopset0, stopset1, stopset2) ;
# 820 "bnf/m2-h.bnf"
   ELSIF (currenttoken=arraytok) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, vartok, periodperiodperiodtok}))
   THEN
      ProcedureParameters(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 820 "bnf/m2-h.bnf"
      Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 820 "bnf/m2-h.bnf"
      FormalReturn(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: identifier ARRAY VAR ... )')
   END ;
END FormalTypeList ;


(*
   FormalReturn := [ ':' Qualident  ]

   first  symbols:colontok
   
   reachend
*)

# 822 "bnf/m2-h.bnf"
PROCEDURE FormalReturn (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 822 "bnf/m2-h.bnf"
BEGIN
# 822 "bnf/m2-h.bnf"
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 823 "bnf/m2-h.bnf"
      Qualident(stopset0, stopset1, stopset2) ;
   END ;
END FormalReturn ;


(*
   ProcedureParameters := ProcedureParameter { ',' ProcedureParameter  }

   first  symbols:identtok, arraytok, vartok, periodperiodperiodtok
   
   cannot reachend
*)

# 825 "bnf/m2-h.bnf"
PROCEDURE ProcedureParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 825 "bnf/m2-h.bnf"
BEGIN
# 825 "bnf/m2-h.bnf"
# 826 "bnf/m2-h.bnf"
   ProcedureParameter(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 826 "bnf/m2-h.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{periodperiodperiodtok, vartok, identtok}) ;
# 826 "bnf/m2-h.bnf"
      ProcedureParameter(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END ProcedureParameters ;


(*
   ProcedureParameter := '...'  | 'VAR' FormalType  | 
                         FormalType 

   first  symbols:identtok, arraytok, vartok, periodperiodperiodtok
   
   cannot reachend
*)

# 828 "bnf/m2-h.bnf"
PROCEDURE ProcedureParameter (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 828 "bnf/m2-h.bnf"
BEGIN
# 828 "bnf/m2-h.bnf"
   IF currenttoken=periodperiodperiodtok
   THEN
      Expect(periodperiodperiodtok, stopset0, stopset1, stopset2) ;
# 828 "bnf/m2-h.bnf"
   ELSIF currenttoken=vartok
   THEN
      Expect(vartok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 828 "bnf/m2-h.bnf"
      FormalType(stopset0, stopset1, stopset2) ;
# 828 "bnf/m2-h.bnf"
   ELSIF (currenttoken=arraytok) OR
         (currenttoken=identtok)
   THEN
      FormalType(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: identifier ARRAY VAR ...')
   END ;
END ProcedureParameter ;


(*
   VariableDeclaration := IdentList ':' Type 

   first  symbols:identtok
   
   cannot reachend
*)

# 830 "bnf/m2-h.bnf"
PROCEDURE VariableDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 830 "bnf/m2-h.bnf"
BEGIN
# 830 "bnf/m2-h.bnf"
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 830 "bnf/m2-h.bnf"
   Expect(colontok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 832 "bnf/m2-h.bnf"
   Type(stopset0, stopset1, stopset2) ;
END VariableDeclaration ;


(*
   Designator := Qualident { SubDesignator  }

   first  symbols:identtok
   
   cannot reachend
*)

# 834 "bnf/m2-h.bnf"
PROCEDURE Designator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 834 "bnf/m2-h.bnf"
BEGIN
# 834 "bnf/m2-h.bnf"
# 835 "bnf/m2-h.bnf"
   Qualident(stopset0 + SetOfStop0{periodtok, lsbratok, uparrowtok}, stopset1, stopset2) ;
# 835 "bnf/m2-h.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {uparrowtok, lsbratok, periodtok})) DO
      SubDesignator(stopset0 + SetOfStop0{uparrowtok, lsbratok, periodtok}, stopset1, stopset2) ;
   END (* while *) ;
END Designator ;


(*
   SubDesignator := '.' Ident  | '[' ExpList ']'  | 
                    '^' 

   first  symbols:uparrowtok, lsbratok, periodtok
   
   cannot reachend
*)

# 837 "bnf/m2-h.bnf"
PROCEDURE SubDesignator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 837 "bnf/m2-h.bnf"
BEGIN
# 837 "bnf/m2-h.bnf"
   IF currenttoken=periodtok
   THEN
      Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 839 "bnf/m2-h.bnf"
      Ident(stopset0, stopset1, stopset2) ;
# 839 "bnf/m2-h.bnf"
   ELSIF currenttoken=lsbratok
   THEN
      Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 840 "bnf/m2-h.bnf"
      ExpList(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 840 "bnf/m2-h.bnf"
      Expect(rsbratok, stopset0, stopset1, stopset2) ;
# 841 "bnf/m2-h.bnf"
   ELSIF currenttoken=uparrowtok
   THEN
      Expect(uparrowtok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: ^ [ .')
   END ;
END SubDesignator ;


(*
   ExpList := Expression { ',' Expression  }

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

# 845 "bnf/m2-h.bnf"
PROCEDURE ExpList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 845 "bnf/m2-h.bnf"
BEGIN
# 845 "bnf/m2-h.bnf"
# 846 "bnf/m2-h.bnf"
   Expression(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 846 "bnf/m2-h.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 848 "bnf/m2-h.bnf"
      Expression(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END ExpList ;


(*
   Expression := SimpleExpression [ SilentRelation SimpleExpression  ]

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

# 852 "bnf/m2-h.bnf"
PROCEDURE Expression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 852 "bnf/m2-h.bnf"
BEGIN
# 852 "bnf/m2-h.bnf"
   SimpleExpression(stopset0 + SetOfStop0{equaltok, hashtok, lessgreatertok, lesstok, lessequaltok, greatertok, greaterequaltok}, stopset1 + SetOfStop1{intok}, stopset2) ;
# 852 "bnf/m2-h.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok})) OR
      (currenttoken=intok)
   THEN
      SilentRelation(stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 853 "bnf/m2-h.bnf"
      SimpleExpression(stopset0, stopset1, stopset2) ;
   END ;
END Expression ;


(*
   SimpleExpression := UnaryOrTerm { SilentAddOperator Term  }

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

# 856 "bnf/m2-h.bnf"
PROCEDURE SimpleExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 856 "bnf/m2-h.bnf"
BEGIN
# 856 "bnf/m2-h.bnf"
   UnaryOrTerm(stopset0 + SetOfStop0{plustok, minustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
# 856 "bnf/m2-h.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok})) OR
         (currenttoken=ortok) DO
      SilentAddOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 857 "bnf/m2-h.bnf"
      Term(stopset0 + SetOfStop0{minustok, plustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
   END (* while *) ;
END SimpleExpression ;


(*
   UnaryOrTerm := '+' Term  | '-' Term  | Term 

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

# 859 "bnf/m2-h.bnf"
PROCEDURE UnaryOrTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 859 "bnf/m2-h.bnf"
BEGIN
# 859 "bnf/m2-h.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 861 "bnf/m2-h.bnf"
      Term(stopset0, stopset1, stopset2) ;
# 861 "bnf/m2-h.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 863 "bnf/m2-h.bnf"
      Term(stopset0, stopset1, stopset2) ;
# 863 "bnf/m2-h.bnf"
   ELSIF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok})) OR
         (currenttoken=nottok) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, stringtok, realtok, integertok}))
   THEN
      Term(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: NOT ( { identifier string real number integer number - +')
   END ;
END UnaryOrTerm ;


(*
   Term := Factor { SilentMulOperator Factor  }

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok
   
   cannot reachend
*)

# 865 "bnf/m2-h.bnf"
PROCEDURE Term (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 865 "bnf/m2-h.bnf"
BEGIN
# 865 "bnf/m2-h.bnf"
   Factor(stopset0 + SetOfStop0{timestok, dividetok, andtok, ambersandtok}, stopset1 + SetOfStop1{divtok, modtok}, stopset2) ;
# 865 "bnf/m2-h.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {ambersandtok, andtok, dividetok, timestok})) OR
         ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {modtok, divtok})) DO
      SilentMulOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 866 "bnf/m2-h.bnf"
      Factor(stopset0 + SetOfStop0{ambersandtok, andtok, dividetok, timestok}, stopset1 + SetOfStop1{modtok, divtok}, stopset2) ;
   END (* while *) ;
END Term ;


(*
   Factor := Number  | string  | SetOrDesignatorOrFunction  | 
             '(' Expression ')'  | 'NOT' ( Factor  )

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok
   
   cannot reachend
*)

# 869 "bnf/m2-h.bnf"
PROCEDURE Factor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 869 "bnf/m2-h.bnf"
BEGIN
# 869 "bnf/m2-h.bnf"
   IF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {realtok, integertok}))
   THEN
      Number(stopset0, stopset1, stopset2) ;
# 869 "bnf/m2-h.bnf"
   ELSIF currenttoken=stringtok
   THEN
      string(stopset0, stopset1, stopset2) ;
# 869 "bnf/m2-h.bnf"
   ELSIF (currenttoken=lcbratok) OR
         (currenttoken=identtok)
   THEN
      SetOrDesignatorOrFunction(stopset0, stopset1, stopset2) ;
# 870 "bnf/m2-h.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 870 "bnf/m2-h.bnf"
      Expression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 870 "bnf/m2-h.bnf"
      Expect(rparatok, stopset0, stopset1, stopset2) ;
# 870 "bnf/m2-h.bnf"
   ELSIF currenttoken=nottok
   THEN
      Expect(nottok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 870 "bnf/m2-h.bnf"
# 871 "bnf/m2-h.bnf"
      Factor(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: NOT ( { identifier string real number integer number')
   END ;
END Factor ;


(*
   SimpleSet := '{' [ Element { ',' Element  } ]'}' 

   first  symbols:lcbratok
   
   cannot reachend
*)

# 875 "bnf/m2-h.bnf"
PROCEDURE SimpleSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 875 "bnf/m2-h.bnf"
BEGIN
# 875 "bnf/m2-h.bnf"
   Expect(lcbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 876 "bnf/m2-h.bnf"
# 877 "bnf/m2-h.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {commatok, lparatok, lcbratok, minustok, plustok})) OR
      (currenttoken=nottok) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {stringtok, realtok, integertok, identtok}))
   THEN
      Element(stopset0 + SetOfStop0{commatok, rcbratok}, stopset1, stopset2) ;
# 877 "bnf/m2-h.bnf"
      WHILE currenttoken=commatok DO
         Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok, commatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 878 "bnf/m2-h.bnf"
         Element(stopset0 + SetOfStop0{rcbratok, commatok}, stopset1, stopset2) ;
      END (* while *) ;
   END ;
# 878 "bnf/m2-h.bnf"
   Expect(rcbratok, stopset0, stopset1, stopset2) ;
END SimpleSet ;


(*
   SetOrDesignatorOrFunction := ( Qualident [ SimpleSet  | 
                                              SimpleDes [ ActualParameters  ] ] | 
                                  SimpleSet  )

   first  symbols:lcbratok, identtok
   
   cannot reachend
*)

# 881 "bnf/m2-h.bnf"
PROCEDURE SetOrDesignatorOrFunction (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 881 "bnf/m2-h.bnf"
BEGIN
# 881 "bnf/m2-h.bnf"
# 882 "bnf/m2-h.bnf"
   IF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok, periodtok, lsbratok, uparrowtok, lparatok}, stopset1, stopset2) ;
# 882 "bnf/m2-h.bnf"
      IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, uparrowtok, lsbratok, periodtok, lcbratok}))
      THEN
         (* seen optional [ | ] expression *)
# 882 "bnf/m2-h.bnf"
         IF currenttoken=lcbratok
         THEN
            SimpleSet(stopset0, stopset1, stopset2) ;
# 883 "bnf/m2-h.bnf"
         ELSIF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, uparrowtok, lsbratok, periodtok}))
         THEN
            SimpleDes(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 883 "bnf/m2-h.bnf"
# 884 "bnf/m2-h.bnf"
            IF currenttoken=lparatok
            THEN
               ActualParameters(stopset0, stopset1, stopset2) ;
            END ;
         ELSE
            ErrorArray('expecting one of: ( ^ [ . {')
         END ;
         (* end of optional [ | ] expression *)
      END ;
# 886 "bnf/m2-h.bnf"
   ELSIF currenttoken=lcbratok
   THEN
      SimpleSet(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: { identifier')
   END ;
END SetOrDesignatorOrFunction ;


(*
   SimpleDes := { SubDesignator  }

   first  symbols:uparrowtok, lsbratok, periodtok
   
   reachend
*)

# 890 "bnf/m2-h.bnf"
PROCEDURE SimpleDes (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 890 "bnf/m2-h.bnf"
BEGIN
# 890 "bnf/m2-h.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {uparrowtok, lsbratok, periodtok})) DO
      SubDesignator(stopset0 + SetOfStop0{uparrowtok, lsbratok, periodtok}, stopset1, stopset2) ;
   END (* while *) ;
END SimpleDes ;


(*
   ActualParameters := '(' ( ExpList  | 
                             % (* epsilon *)  %
                              )')' 

   first  symbols:lparatok
   
   cannot reachend
*)

# 892 "bnf/m2-h.bnf"
PROCEDURE ActualParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 892 "bnf/m2-h.bnf"
BEGIN
# 892 "bnf/m2-h.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 893 "bnf/m2-h.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok, minustok, plustok})) OR
      (currenttoken=nottok) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, stringtok, realtok, integertok}))
   THEN
      ExpList(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 893 "bnf/m2-h.bnf"
   ELSE
# 893 "bnf/m2-h.bnf"
      (* epsilon *)  ;
   END ;
# 894 "bnf/m2-h.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END ActualParameters ;


(*
   Statement := [ AssignmentOrProcedureCall  | 
                  IfStatement  | CaseStatement  | 
                  WhileStatement  | RepeatStatement  | 
                  LoopStatement  | ForStatement  | 
                  WithStatement  | AsmStatement  | 
                  'EXIT'  | 'RETURN' ( Expression  | 
                                       
                                       % (* in epsilon *)  %
                                        ) ]

   first  symbols:returntok, exittok, asmtok, withtok, fortok, looptok, repeattok, whiletok, casetok, iftok, identtok
   
   reachend
*)

# 896 "bnf/m2-h.bnf"
PROCEDURE Statement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 896 "bnf/m2-h.bnf"
BEGIN
# 896 "bnf/m2-h.bnf"
# 897 "bnf/m2-h.bnf"
   IF ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {returntok, exittok, fortok, looptok, repeattok, casetok, iftok})) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {asmtok, withtok, whiletok, identtok}))
   THEN
      (* seen optional [ | ] expression *)
# 897 "bnf/m2-h.bnf"
      IF currenttoken=identtok
      THEN
         AssignmentOrProcedureCall(stopset0, stopset1, stopset2) ;
# 897 "bnf/m2-h.bnf"
      ELSIF currenttoken=iftok
      THEN
         IfStatement(stopset0, stopset1, stopset2) ;
# 897 "bnf/m2-h.bnf"
      ELSIF currenttoken=casetok
      THEN
         CaseStatement(stopset0, stopset1, stopset2) ;
# 898 "bnf/m2-h.bnf"
      ELSIF currenttoken=whiletok
      THEN
         WhileStatement(stopset0, stopset1, stopset2) ;
# 898 "bnf/m2-h.bnf"
      ELSIF currenttoken=repeattok
      THEN
         RepeatStatement(stopset0, stopset1, stopset2) ;
# 898 "bnf/m2-h.bnf"
      ELSIF currenttoken=looptok
      THEN
         LoopStatement(stopset0, stopset1, stopset2) ;
# 899 "bnf/m2-h.bnf"
      ELSIF currenttoken=fortok
      THEN
         ForStatement(stopset0, stopset1, stopset2) ;
# 899 "bnf/m2-h.bnf"
      ELSIF currenttoken=withtok
      THEN
         WithStatement(stopset0, stopset1, stopset2) ;
# 899 "bnf/m2-h.bnf"
      ELSIF currenttoken=asmtok
      THEN
         AsmStatement(stopset0, stopset1, stopset2) ;
# 900 "bnf/m2-h.bnf"
      ELSIF currenttoken=exittok
      THEN
         Expect(exittok, stopset0, stopset1, stopset2) ;
# 901 "bnf/m2-h.bnf"
      ELSIF currenttoken=returntok
      THEN
         Expect(returntok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 902 "bnf/m2-h.bnf"
         IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok, minustok, plustok})) OR
            (currenttoken=nottok) OR
            ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, stringtok, realtok, integertok}))
         THEN
            Expression(stopset0, stopset1, stopset2) ;
# 902 "bnf/m2-h.bnf"
         ELSE
# 902 "bnf/m2-h.bnf"
            (* in epsilon *)  ;
         END ;
      ELSE
         ErrorArray('expecting one of: RETURN EXIT ASM WITH FOR LOOP REPEAT WHILE CASE IF identifier')
      END ;
      (* end of optional [ | ] expression *)
   END ;
END Statement ;


(*
   AssignmentOrProcedureCall := Designator ( ':=' Expression  | 
                                             ActualParameters  | 
                                             
                                             % (* in epsilon *)  %
                                              )

   first  symbols:identtok
   
   cannot reachend
*)

# 907 "bnf/m2-h.bnf"
PROCEDURE AssignmentOrProcedureCall (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 907 "bnf/m2-h.bnf"
BEGIN
# 907 "bnf/m2-h.bnf"
   Designator(stopset0 + SetOfStop0{becomestok, lparatok}, stopset1, stopset2) ;
# 907 "bnf/m2-h.bnf"
   IF currenttoken=becomestok
   THEN
      Expect(becomestok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 907 "bnf/m2-h.bnf"
      Expression(stopset0, stopset1, stopset2) ;
# 908 "bnf/m2-h.bnf"
   ELSIF currenttoken=lparatok
   THEN
      ActualParameters(stopset0, stopset1, stopset2) ;
# 908 "bnf/m2-h.bnf"
   ELSE
# 908 "bnf/m2-h.bnf"
      (* in epsilon *)  ;
   END ;
END AssignmentOrProcedureCall ;


(*
   StatementSequence := Statement { ';' Statement  }

   first  symbols:semicolontok, returntok, exittok, asmtok, withtok, fortok, looptok, repeattok, whiletok, casetok, iftok, identtok
   
   reachend
*)

# 916 "bnf/m2-h.bnf"
PROCEDURE StatementSequence (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 916 "bnf/m2-h.bnf"
BEGIN
# 916 "bnf/m2-h.bnf"
# 917 "bnf/m2-h.bnf"
   Statement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 917 "bnf/m2-h.bnf"
   WHILE currenttoken=semicolontok DO
      Expect(semicolontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 918 "bnf/m2-h.bnf"
      Statement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END (* while *) ;
END StatementSequence ;


(*
   IfStatement := 'IF' Expression 'THEN' StatementSequence { 
   'ELSIF' Expression 'THEN' StatementSequence  }[ 'ELSE' StatementSequence  ]
                  'END' 

   first  symbols:iftok
   
   cannot reachend
*)

# 922 "bnf/m2-h.bnf"
PROCEDURE IfStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 922 "bnf/m2-h.bnf"
BEGIN
# 922 "bnf/m2-h.bnf"
   Expect(iftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 923 "bnf/m2-h.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{thentok}, stopset2) ;
# 923 "bnf/m2-h.bnf"
   Expect(thentok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, elsiftok, elsetok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 925 "bnf/m2-h.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{elsiftok, elsetok, endtok}, stopset2) ;
# 925 "bnf/m2-h.bnf"
   WHILE currenttoken=elsiftok DO
      Expect(elsiftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 926 "bnf/m2-h.bnf"
      Expression(stopset0, stopset1 + SetOfStop1{thentok}, stopset2) ;
# 926 "bnf/m2-h.bnf"
      Expect(thentok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok, elsetok, elsiftok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 928 "bnf/m2-h.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok, elsetok, elsiftok}, stopset2) ;
   END (* while *) ;
# 929 "bnf/m2-h.bnf"
   IF currenttoken=elsetok
   THEN
      Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 930 "bnf/m2-h.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   END ;
# 930 "bnf/m2-h.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END IfStatement ;


(*
   CaseStatement := 'CASE' Expression 'OF' Case { '|' Case  }
                    [ 'ELSE' StatementSequence  ]'END' 

   first  symbols:casetok
   
   cannot reachend
*)

# 933 "bnf/m2-h.bnf"
PROCEDURE CaseStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 933 "bnf/m2-h.bnf"
BEGIN
# 933 "bnf/m2-h.bnf"
   Expect(casetok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 935 "bnf/m2-h.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 935 "bnf/m2-h.bnf"
   Expect(oftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 935 "bnf/m2-h.bnf"
   Case(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{elsetok, endtok}, stopset2) ;
# 935 "bnf/m2-h.bnf"
   WHILE currenttoken=bartok DO
      Expect(bartok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 935 "bnf/m2-h.bnf"
      Case(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{endtok, elsetok}, stopset2) ;
   END (* while *) ;
# 936 "bnf/m2-h.bnf"
   IF currenttoken=elsetok
   THEN
      Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 937 "bnf/m2-h.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   END ;
# 937 "bnf/m2-h.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END CaseStatement ;


(*
   Case := CaseLabelList ':' StatementSequence 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 940 "bnf/m2-h.bnf"
PROCEDURE Case (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 940 "bnf/m2-h.bnf"
BEGIN
# 940 "bnf/m2-h.bnf"
   CaseLabelList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 940 "bnf/m2-h.bnf"
   Expect(colontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 942 "bnf/m2-h.bnf"
   StatementSequence(stopset0, stopset1, stopset2) ;
END Case ;


(*
   CaseLabelList := CaseLabels { ',' CaseLabels  }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 944 "bnf/m2-h.bnf"
PROCEDURE CaseLabelList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 944 "bnf/m2-h.bnf"
BEGIN
# 944 "bnf/m2-h.bnf"
   CaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 944 "bnf/m2-h.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 945 "bnf/m2-h.bnf"
      CaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END CaseLabelList ;


(*
   CaseLabels := ConstExpression ( '..' ConstExpression  | 
                                   
                                   % (* epsilon *)  %
                                    )

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 947 "bnf/m2-h.bnf"
PROCEDURE CaseLabels (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 947 "bnf/m2-h.bnf"
BEGIN
# 947 "bnf/m2-h.bnf"
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 947 "bnf/m2-h.bnf"
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 948 "bnf/m2-h.bnf"
      ConstExpression(stopset0, stopset1, stopset2) ;
# 948 "bnf/m2-h.bnf"
   ELSE
# 948 "bnf/m2-h.bnf"
      (* epsilon *)  ;
   END ;
END CaseLabels ;


(*
   WhileStatement := 'WHILE' Expression 'DO' StatementSequence 
                     'END' 

   first  symbols:whiletok
   
   cannot reachend
*)

# 951 "bnf/m2-h.bnf"
PROCEDURE WhileStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 951 "bnf/m2-h.bnf"
BEGIN
# 951 "bnf/m2-h.bnf"
   Expect(whiletok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 952 "bnf/m2-h.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
# 952 "bnf/m2-h.bnf"
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 953 "bnf/m2-h.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 953 "bnf/m2-h.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END WhileStatement ;


(*
   RepeatStatement := 'REPEAT' StatementSequence 'UNTIL' Expression 

   first  symbols:repeattok
   
   cannot reachend
*)

# 956 "bnf/m2-h.bnf"
PROCEDURE RepeatStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 956 "bnf/m2-h.bnf"
BEGIN
# 956 "bnf/m2-h.bnf"
   Expect(repeattok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok, untiltok}) ;
# 957 "bnf/m2-h.bnf"
   StatementSequence(stopset0, stopset1, stopset2 + SetOfStop2{untiltok}) ;
# 957 "bnf/m2-h.bnf"
   Expect(untiltok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 959 "bnf/m2-h.bnf"
   Expression(stopset0, stopset1, stopset2) ;
END RepeatStatement ;


(*
   ForStatement := 'FOR' Ident ':=' Expression 'TO' Expression 
                   ( 'BY' ConstExpression  | 
                     
                     % (* epsilon *)  %
                      )'DO' StatementSequence 'END' 

   first  symbols:fortok
   
   cannot reachend
*)

# 961 "bnf/m2-h.bnf"
PROCEDURE ForStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 961 "bnf/m2-h.bnf"
BEGIN
# 961 "bnf/m2-h.bnf"
   Expect(fortok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 962 "bnf/m2-h.bnf"
   Ident(stopset0 + SetOfStop0{becomestok}, stopset1, stopset2) ;
# 962 "bnf/m2-h.bnf"
   Expect(becomestok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 962 "bnf/m2-h.bnf"
   Expression(stopset0, stopset1, stopset2 + SetOfStop2{totok}) ;
# 962 "bnf/m2-h.bnf"
   Expect(totok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 963 "bnf/m2-h.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{bytok, dotok}, stopset2) ;
# 963 "bnf/m2-h.bnf"
   IF currenttoken=bytok
   THEN
      Expect(bytok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 963 "bnf/m2-h.bnf"
      ConstExpression(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
# 963 "bnf/m2-h.bnf"
   ELSE
# 963 "bnf/m2-h.bnf"
      (* epsilon *)  ;
   END ;
# 964 "bnf/m2-h.bnf"
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 965 "bnf/m2-h.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 965 "bnf/m2-h.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END ForStatement ;


(*
   LoopStatement := 'LOOP' StatementSequence 'END' 

   first  symbols:looptok
   
   cannot reachend
*)

# 968 "bnf/m2-h.bnf"
PROCEDURE LoopStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 968 "bnf/m2-h.bnf"
BEGIN
# 968 "bnf/m2-h.bnf"
   Expect(looptok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 969 "bnf/m2-h.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 969 "bnf/m2-h.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END LoopStatement ;


(*
   WithStatement := 'WITH' Designator 'DO' StatementSequence 
                    'END' 

   first  symbols:withtok
   
   cannot reachend
*)

# 972 "bnf/m2-h.bnf"
PROCEDURE WithStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 972 "bnf/m2-h.bnf"
BEGIN
# 972 "bnf/m2-h.bnf"
   Expect(withtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 973 "bnf/m2-h.bnf"
   Designator(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
# 973 "bnf/m2-h.bnf"
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 975 "bnf/m2-h.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 975 "bnf/m2-h.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END WithStatement ;


(*
   ProcedureDeclaration := ProcedureHeading ';' ProcedureBlock 
                           Ident 

   first  symbols:proceduretok
   
   cannot reachend
*)

# 978 "bnf/m2-h.bnf"
PROCEDURE ProcedureDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 978 "bnf/m2-h.bnf"
BEGIN
# 978 "bnf/m2-h.bnf"
   ProcedureHeading(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 978 "bnf/m2-h.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{consttok, proceduretok, moduletok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 979 "bnf/m2-h.bnf"
   ProcedureBlock(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 980 "bnf/m2-h.bnf"
   Ident(stopset0, stopset1, stopset2) ;
END ProcedureDeclaration ;


(*
   ProcedureHeading := 'PROCEDURE' ( Ident [ FormalParameters  ] )

   first  symbols:proceduretok
   
   cannot reachend
*)

# 982 "bnf/m2-h.bnf"
PROCEDURE ProcedureHeading (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 982 "bnf/m2-h.bnf"
BEGIN
# 982 "bnf/m2-h.bnf"
   Expect(proceduretok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 983 "bnf/m2-h.bnf"
# 984 "bnf/m2-h.bnf"
   Ident(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 984 "bnf/m2-h.bnf"
   IF currenttoken=lparatok
   THEN
      FormalParameters(stopset0, stopset1, stopset2) ;
   END ;
END ProcedureHeading ;


(*
   ProcedureBlock := { Declaration  }'BEGIN' StatementSequence 
                     'END' 

   first  symbols:begintok, moduletok, proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

# 991 "bnf/m2-h.bnf"
PROCEDURE ProcedureBlock (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 991 "bnf/m2-h.bnf"
BEGIN
# 991 "bnf/m2-h.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Declaration(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 991 "bnf/m2-h.bnf"
   Expect(begintok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 992 "bnf/m2-h.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 992 "bnf/m2-h.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END ProcedureBlock ;


(*
   Block := { Declaration  }( 'BEGIN' StatementSequence 'END'  | 
                              'END'  )

   first  symbols:endtok, begintok, moduletok, proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

# 995 "bnf/m2-h.bnf"
PROCEDURE Block (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 995 "bnf/m2-h.bnf"
BEGIN
# 995 "bnf/m2-h.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Declaration(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 995 "bnf/m2-h.bnf"
   IF currenttoken=begintok
   THEN
      Expect(begintok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 996 "bnf/m2-h.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 996 "bnf/m2-h.bnf"
      Expect(endtok, stopset0, stopset1, stopset2) ;
# 997 "bnf/m2-h.bnf"
   ELSIF currenttoken=endtok
   THEN
      Expect(endtok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: END BEGIN')
   END ;
END Block ;


(*
   Declaration := 'CONST' { ConstantDeclaration ';'  } | 
                  'TYPE' { TypeDeclaration ';'  } | 
                  'VAR' { VariableDeclaration ';'  } | 
                  ProcedureDeclaration ';'  | 
                  ModuleDeclaration ';' 

   first  symbols:moduletok, proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

# 1001 "bnf/m2-h.bnf"
PROCEDURE Declaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1001 "bnf/m2-h.bnf"
BEGIN
# 1001 "bnf/m2-h.bnf"
   IF currenttoken=consttok
   THEN
      Expect(consttok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1001 "bnf/m2-h.bnf"
      WHILE currenttoken=identtok DO
         ConstantDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1001 "bnf/m2-h.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 1002 "bnf/m2-h.bnf"
   ELSIF currenttoken=typetok
   THEN
      Expect(typetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1002 "bnf/m2-h.bnf"
      WHILE currenttoken=identtok DO
         TypeDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1002 "bnf/m2-h.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 1003 "bnf/m2-h.bnf"
   ELSIF currenttoken=vartok
   THEN
      Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1003 "bnf/m2-h.bnf"
      WHILE currenttoken=identtok DO
         VariableDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1003 "bnf/m2-h.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 1004 "bnf/m2-h.bnf"
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1004 "bnf/m2-h.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
# 1005 "bnf/m2-h.bnf"
   ELSIF currenttoken=moduletok
   THEN
      ModuleDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1005 "bnf/m2-h.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: MODULE PROCEDURE VAR TYPE CONST')
   END ;
END Declaration ;


(*
   FormalParameters := '(' [ FPSection { ';' FPSection  } ]')' 
                       [ ':' Qualident  ]

   first  symbols:lparatok
   
   cannot reachend
*)

# 1007 "bnf/m2-h.bnf"
PROCEDURE FormalParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1007 "bnf/m2-h.bnf"
BEGIN
# 1007 "bnf/m2-h.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{rparatok}, stopset1, stopset2 + SetOfStop2{identtok, vartok, periodperiodperiodtok}) ;
# 1007 "bnf/m2-h.bnf"
   IF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {periodperiodperiodtok, vartok, identtok}))
   THEN
      FPSection(stopset0 + SetOfStop0{semicolontok, rparatok}, stopset1, stopset2) ;
# 1007 "bnf/m2-h.bnf"
      WHILE currenttoken=semicolontok DO
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok, vartok, periodperiodperiodtok}) ;
# 1007 "bnf/m2-h.bnf"
         FPSection(stopset0 + SetOfStop0{rparatok, semicolontok}, stopset1, stopset2) ;
      END (* while *) ;
   END ;
# 1007 "bnf/m2-h.bnf"
   Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 1008 "bnf/m2-h.bnf"
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1009 "bnf/m2-h.bnf"
      Qualident(stopset0, stopset1, stopset2) ;
   END ;
END FormalParameters ;


(*
   FPSection := NonVarFPSection  | VarFPSection  | 
                '...' 

   first  symbols:periodperiodperiodtok, vartok, identtok
   
   cannot reachend
*)

# 1011 "bnf/m2-h.bnf"
PROCEDURE FPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1011 "bnf/m2-h.bnf"
BEGIN
# 1011 "bnf/m2-h.bnf"
   IF currenttoken=identtok
   THEN
      NonVarFPSection(stopset0, stopset1, stopset2) ;
# 1011 "bnf/m2-h.bnf"
   ELSIF currenttoken=vartok
   THEN
      VarFPSection(stopset0, stopset1, stopset2) ;
# 1011 "bnf/m2-h.bnf"
   ELSIF currenttoken=periodperiodperiodtok
   THEN
      Expect(periodperiodperiodtok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: ... VAR identifier')
   END ;
END FPSection ;


(*
   VarFPSection := 'VAR' IdentList ':' FormalType 

   first  symbols:vartok
   
   cannot reachend
*)

# 1013 "bnf/m2-h.bnf"
PROCEDURE VarFPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1013 "bnf/m2-h.bnf"
BEGIN
# 1013 "bnf/m2-h.bnf"
   Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1013 "bnf/m2-h.bnf"
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 1013 "bnf/m2-h.bnf"
   Expect(colontok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1013 "bnf/m2-h.bnf"
   FormalType(stopset0, stopset1, stopset2) ;
END VarFPSection ;


(*
   NonVarFPSection := IdentList ':' FormalType 

   first  symbols:identtok
   
   cannot reachend
*)

# 1015 "bnf/m2-h.bnf"
PROCEDURE NonVarFPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1015 "bnf/m2-h.bnf"
BEGIN
# 1015 "bnf/m2-h.bnf"
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 1015 "bnf/m2-h.bnf"
   Expect(colontok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1015 "bnf/m2-h.bnf"
   FormalType(stopset0, stopset1, stopset2) ;
END NonVarFPSection ;


(*
   FormalType := [ 'ARRAY' 'OF'  ]Qualident 

   first  symbols:identtok, arraytok
   
   cannot reachend
*)

# 1017 "bnf/m2-h.bnf"
PROCEDURE FormalType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1017 "bnf/m2-h.bnf"
BEGIN
# 1017 "bnf/m2-h.bnf"
   IF currenttoken=arraytok
   THEN
      Expect(arraytok, stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 1017 "bnf/m2-h.bnf"
      Expect(oftok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   END ;
# 1017 "bnf/m2-h.bnf"
   Qualident(stopset0, stopset1, stopset2) ;
END FormalType ;


(*
   ModuleDeclaration := 'MODULE' 
                        % PushAutoOn  %
                        Ident 
                        % StartBuildInnerModule ;
                          PushAutoOff  %
                        [ Priority  ]';' { Import  }[ Export  ]
                        Block 
                        % PushAutoOn  %
                        Ident 
                        % EndBuildInnerModule  %
                        
                        % PopAuto ; PopAuto ; PopAuto  %
                        

   first  symbols:moduletok
   
   cannot reachend
*)

# 1019 "bnf/m2-h.bnf"
PROCEDURE ModuleDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1019 "bnf/m2-h.bnf"
BEGIN
# 1019 "bnf/m2-h.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1019 "bnf/m2-h.bnf"
   PushAutoOn  ;
# 1020 "bnf/m2-h.bnf"
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
# 1020 "bnf/m2-h.bnf"
# 1021 "bnf/m2-h.bnf"
   StartBuildInnerModule ;
   PushAutoOff  ;
# 1022 "bnf/m2-h.bnf"
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
# 1022 "bnf/m2-h.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, exporttok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 1023 "bnf/m2-h.bnf"
# 1024 "bnf/m2-h.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, exporttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 1024 "bnf/m2-h.bnf"
# 1025 "bnf/m2-h.bnf"
   IF currenttoken=exporttok
   THEN
      Export(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END ;
# 1026 "bnf/m2-h.bnf"
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1026 "bnf/m2-h.bnf"
   PushAutoOn  ;
# 1027 "bnf/m2-h.bnf"
   Ident(stopset0, stopset1, stopset2) ;
# 1027 "bnf/m2-h.bnf"
   EndBuildInnerModule  ;
# 1028 "bnf/m2-h.bnf"
   PopAuto ; PopAuto ; PopAuto  ;
END ModuleDeclaration ;


(*
   Priority := '[' ConstExpression ']' 

   first  symbols:lsbratok
   
   cannot reachend
*)

# 1031 "bnf/m2-h.bnf"
PROCEDURE Priority (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1031 "bnf/m2-h.bnf"
BEGIN
# 1031 "bnf/m2-h.bnf"
   Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 1031 "bnf/m2-h.bnf"
   ConstExpression(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 1031 "bnf/m2-h.bnf"
   Expect(rsbratok, stopset0, stopset1, stopset2) ;
END Priority ;


(*
   Export := 'EXPORT' ( 'QUALIFIED' IdentList  | 
                        'UNQUALIFIED' IdentList  | 
                        IdentList  )';' 

   first  symbols:exporttok
   
   cannot reachend
*)

# 1033 "bnf/m2-h.bnf"
PROCEDURE Export (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1033 "bnf/m2-h.bnf"
BEGIN
# 1033 "bnf/m2-h.bnf"
   Expect(exporttok, stopset0, stopset1 + SetOfStop1{qualifiedtok, unqualifiedtok}, stopset2 + SetOfStop2{identtok}) ;
# 1033 "bnf/m2-h.bnf"
   IF currenttoken=qualifiedtok
   THEN
      Expect(qualifiedtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1034 "bnf/m2-h.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1035 "bnf/m2-h.bnf"
   ELSIF currenttoken=unqualifiedtok
   THEN
      Expect(unqualifiedtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1036 "bnf/m2-h.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1037 "bnf/m2-h.bnf"
   ELSIF currenttoken=identtok
   THEN
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: identifier UNQUALIFIED QUALIFIED')
   END ;
# 1037 "bnf/m2-h.bnf"
   Expect(semicolontok, stopset0, stopset1, stopset2) ;
END Export ;


(*
   Import := 'FROM' Ident 'IMPORT' IdentList ';'  | 
             'IMPORT' IdentList ';' 

   first  symbols:importtok, fromtok
   
   cannot reachend
*)

# 1039 "bnf/m2-h.bnf"
PROCEDURE Import (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1039 "bnf/m2-h.bnf"
BEGIN
# 1039 "bnf/m2-h.bnf"
   IF currenttoken=fromtok
   THEN
      Expect(fromtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1039 "bnf/m2-h.bnf"
      Ident(stopset0, stopset1 + SetOfStop1{importtok}, stopset2) ;
# 1039 "bnf/m2-h.bnf"
      Expect(importtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1039 "bnf/m2-h.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1039 "bnf/m2-h.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
# 1040 "bnf/m2-h.bnf"
   ELSIF currenttoken=importtok
   THEN
      Expect(importtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1041 "bnf/m2-h.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1041 "bnf/m2-h.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: IMPORT FROM')
   END ;
END Import ;


(*
   DefinitionModule := 'DEFINITION' 'MODULE' 
                       % PushAutoOn  %
                       [ 'FOR' string  ]Ident 
                       % StartBuildDefFile ;
                         P3StartBuildDefModule ;
                         PushAutoOff  %
                       ';' { Import  }[ Export  ]{ Definition  }
                       'END' 
                       % PushAutoOn  %
                       Ident 
                       % EndBuildFile ;
                         P3EndBuildDefModule  %
                       '.' 
                       % PopAuto ; PopAuto ; PopAuto  %
                       

   first  symbols:definitiontok
   
   cannot reachend
*)

# 1043 "bnf/m2-h.bnf"
PROCEDURE DefinitionModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1043 "bnf/m2-h.bnf"
BEGIN
# 1043 "bnf/m2-h.bnf"
   Expect(definitiontok, stopset0, stopset1 + SetOfStop1{moduletok}, stopset2) ;
# 1043 "bnf/m2-h.bnf"
   Expect(moduletok, stopset0, stopset1 + SetOfStop1{fortok}, stopset2 + SetOfStop2{identtok}) ;
# 1043 "bnf/m2-h.bnf"
   PushAutoOn  ;
# 1044 "bnf/m2-h.bnf"
   IF currenttoken=fortok
   THEN
      Expect(fortok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 1044 "bnf/m2-h.bnf"
      string(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   END ;
# 1045 "bnf/m2-h.bnf"
   Ident(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1045 "bnf/m2-h.bnf"
# 1047 "bnf/m2-h.bnf"
   StartBuildDefFile ;
   P3StartBuildDefModule ;
   PushAutoOff  ;
# 1048 "bnf/m2-h.bnf"
   Expect(semicolontok, stopset0, stopset1 + SetOfStop1{fromtok, importtok, exporttok, consttok, proceduretok, endtok}, stopset2 + SetOfStop2{typetok, vartok}) ;
# 1049 "bnf/m2-h.bnf"
# 1050 "bnf/m2-h.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok, exporttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 1050 "bnf/m2-h.bnf"
# 1051 "bnf/m2-h.bnf"
   IF currenttoken=exporttok
   THEN
      Export(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END ;
# 1052 "bnf/m2-h.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Definition(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 1053 "bnf/m2-h.bnf"
   Expect(endtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1053 "bnf/m2-h.bnf"
   PushAutoOn  ;
# 1054 "bnf/m2-h.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 1054 "bnf/m2-h.bnf"
# 1055 "bnf/m2-h.bnf"
   EndBuildFile ;
   P3EndBuildDefModule  ;
# 1056 "bnf/m2-h.bnf"
   Expect(periodtok, stopset0, stopset1, stopset2) ;
# 1056 "bnf/m2-h.bnf"
   PopAuto ; PopAuto ; PopAuto  ;
END DefinitionModule ;


(*
   Definition := 'CONST' { ConstantDeclaration ';'  } | 
                 'TYPE' { Ident ( ';'  | '=' Type ';'  ) } | 
                 'VAR' { VariableDeclaration ';'  } | 
                 ProcedureHeading ';' 

   first  symbols:proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

# 1059 "bnf/m2-h.bnf"
PROCEDURE Definition (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1059 "bnf/m2-h.bnf"
BEGIN
# 1059 "bnf/m2-h.bnf"
   IF currenttoken=consttok
   THEN
      Expect(consttok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1059 "bnf/m2-h.bnf"
      WHILE currenttoken=identtok DO
         ConstantDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1059 "bnf/m2-h.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 1060 "bnf/m2-h.bnf"
   ELSIF currenttoken=typetok
   THEN
      Expect(typetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1061 "bnf/m2-h.bnf"
      WHILE currenttoken=identtok DO
         Ident(stopset0 + SetOfStop0{semicolontok, equaltok}, stopset1, stopset2) ;
# 1061 "bnf/m2-h.bnf"
         IF currenttoken=semicolontok
         THEN
            Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1062 "bnf/m2-h.bnf"
         ELSIF currenttoken=equaltok
         THEN
            Expect(equaltok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 1062 "bnf/m2-h.bnf"
            Type(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1062 "bnf/m2-h.bnf"
            Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
         ELSE
            ErrorArray('expecting one of: = ;')
         END ;
      END (* while *) ;
# 1065 "bnf/m2-h.bnf"
   ELSIF currenttoken=vartok
   THEN
      Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1065 "bnf/m2-h.bnf"
      WHILE currenttoken=identtok DO
         VariableDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1065 "bnf/m2-h.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 1066 "bnf/m2-h.bnf"
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureHeading(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1066 "bnf/m2-h.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: PROCEDURE VAR TYPE CONST')
   END ;
END Definition ;


(*
   AsmStatement := 'ASM' [ 'VOLATILE'  ]'(' AsmOperands ')' 

   first  symbols:asmtok
   
   cannot reachend
*)

# 1068 "bnf/m2-h.bnf"
PROCEDURE AsmStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1068 "bnf/m2-h.bnf"
BEGIN
# 1068 "bnf/m2-h.bnf"
   Expect(asmtok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2 + SetOfStop2{volatiletok}) ;
# 1068 "bnf/m2-h.bnf"
   IF currenttoken=volatiletok
   THEN
      Expect(volatiletok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
   END ;
# 1068 "bnf/m2-h.bnf"
   Expect(lparatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 1068 "bnf/m2-h.bnf"
   AsmOperands(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 1068 "bnf/m2-h.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END AsmStatement ;


(*
   AsmOperands := string [ ':' AsmList [ ':' AsmList [ ':' TrashList  ] ] ]

   first  symbols:stringtok
   
   cannot reachend
*)

# 1070 "bnf/m2-h.bnf"
PROCEDURE AsmOperands (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1070 "bnf/m2-h.bnf"
BEGIN
# 1070 "bnf/m2-h.bnf"
   string(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 1070 "bnf/m2-h.bnf"
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0 + SetOfStop0{commatok, colontok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 1070 "bnf/m2-h.bnf"
      AsmList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 1070 "bnf/m2-h.bnf"
      IF currenttoken=colontok
      THEN
         Expect(colontok, stopset0 + SetOfStop0{commatok, colontok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 1070 "bnf/m2-h.bnf"
         AsmList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 1070 "bnf/m2-h.bnf"
         IF currenttoken=colontok
         THEN
            Expect(colontok, stopset0 + SetOfStop0{commatok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 1070 "bnf/m2-h.bnf"
            TrashList(stopset0, stopset1, stopset2) ;
         END ;
      END ;
   END ;
END AsmOperands ;


(*
   AsmList := [ AsmElement  ]{ ',' AsmElement  }

   first  symbols:commatok, stringtok
   
   reachend
*)

# 1073 "bnf/m2-h.bnf"
PROCEDURE AsmList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1073 "bnf/m2-h.bnf"
BEGIN
# 1073 "bnf/m2-h.bnf"
   IF currenttoken=stringtok
   THEN
      AsmElement(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END ;
# 1073 "bnf/m2-h.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 1073 "bnf/m2-h.bnf"
      AsmElement(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END AsmList ;


(*
   AsmElement := string '(' Expression ')' 

   first  symbols:stringtok
   
   cannot reachend
*)

# 1075 "bnf/m2-h.bnf"
PROCEDURE AsmElement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1075 "bnf/m2-h.bnf"
BEGIN
# 1075 "bnf/m2-h.bnf"
   string(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 1075 "bnf/m2-h.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 1075 "bnf/m2-h.bnf"
   Expression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 1075 "bnf/m2-h.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END AsmElement ;


(*
   TrashList := [ string  ]{ ',' string  }

   first  symbols:commatok, stringtok
   
   reachend
*)

# 1078 "bnf/m2-h.bnf"
PROCEDURE TrashList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1078 "bnf/m2-h.bnf"
BEGIN
# 1078 "bnf/m2-h.bnf"
   IF currenttoken=stringtok
   THEN
      string(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END ;
# 1078 "bnf/m2-h.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 1078 "bnf/m2-h.bnf"
      string(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END TrashList ;


# 424 "bnf/m2-h.bnf"

END PHBuild.
