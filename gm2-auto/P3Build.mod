(* it is advisable not to edit this file as it was automatically generated from the grammer file bnf/m2-3.bnf *)

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
IMPLEMENTATION MODULE P3Build ;

FROM M2LexBuf IMPORT currentstring, currenttoken, GetToken, InsertToken, InsertTokenAndRewind, GetTokenNo ;
FROM M2Error IMPORT ErrorStringAt, WriteFormat1, WriteFormat2 ;
FROM NameKey IMPORT NulName, Name ;
FROM Strings IMPORT String, InitString, KillString, Mark, ConCat, ConCatChar ;
FROM M2Printf IMPORT printf0 ;
FROM M2Debug IMPORT Assert ;
FROM P2SymBuild IMPORT BuildString, BuildNumber ;

FROM M2Reserved IMPORT tokToTok, toktype,
                       NulTok, ImportTok, ExportTok, QualifiedTok, UnQualifiedTok,
                       EqualTok, HashTok, LessGreaterTok, LessTok, LessEqualTok,
                       GreaterTok, GreaterEqualTok, InTok, PlusTok, MinusTok,
                       OrTok, TimesTok, DivTok, ModTok, AndTok, AmbersandTok ;

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
                    DumpStack,
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
                        PutRegInterface,
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


CONST
   Debugging = FALSE ;
   Pass1     = FALSE ;          (* permanently disabled for the time being *)
   Pass2     =  TRUE ;
   Pass3     = FALSE ;          (* permanently disabled for the time being *)

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

# 525 "bnf/m2-3.bnf"
PROCEDURE FileUnit (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 525 "bnf/m2-3.bnf"
BEGIN
# 525 "bnf/m2-3.bnf"
   PushAutoOff  ;
# 526 "bnf/m2-3.bnf"
   IF currenttoken=definitiontok
   THEN
      DefinitionModule(stopset0, stopset1, stopset2) ;
# 527 "bnf/m2-3.bnf"
   ELSIF ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, implementationtok}))
   THEN
      ImplementationOrProgramModule(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: MODULE IMPLEMENTATION DEFINITION')
   END ;
# 527 "bnf/m2-3.bnf"
   PopAuto  ;
END FileUnit ;


(*
   ProgramModule := 'MODULE' 
                    % PushAutoOn  %
                    Ident 
                    % P3StartBuildProgModule  %
                    
                    % PushAutoOff  %
                    [ Priority  ]';' { Import  }Block 
                    % PushAutoOn  %
                    Ident 
                    % EndBuildFile  %
                    
                    % P3EndBuildProgModule  %
                    '.' 
                    % PopAuto ; PopAuto  %
                    

   first  symbols:moduletok
   
   cannot reachend
*)

# 530 "bnf/m2-3.bnf"
PROCEDURE ProgramModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 530 "bnf/m2-3.bnf"
BEGIN
# 530 "bnf/m2-3.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 530 "bnf/m2-3.bnf"
   PushAutoOn  ;
# 531 "bnf/m2-3.bnf"
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
# 531 "bnf/m2-3.bnf"
   P3StartBuildProgModule  ;
# 532 "bnf/m2-3.bnf"
   PushAutoOff  ;
# 533 "bnf/m2-3.bnf"
# 534 "bnf/m2-3.bnf"
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
# 535 "bnf/m2-3.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 536 "bnf/m2-3.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 538 "bnf/m2-3.bnf"
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 538 "bnf/m2-3.bnf"
   PushAutoOn  ;
# 539 "bnf/m2-3.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 539 "bnf/m2-3.bnf"
   EndBuildFile  ;
# 540 "bnf/m2-3.bnf"
   P3EndBuildProgModule  ;
# 541 "bnf/m2-3.bnf"
   Expect(periodtok, stopset0, stopset1, stopset2) ;
# 541 "bnf/m2-3.bnf"
   PopAuto ; PopAuto  ;
END ProgramModule ;


(*
   ImplementationModule := 'IMPLEMENTATION' 'MODULE' 
                           % PushAutoOn  %
                           Ident 
                           % StartBuildModFile  %
                           
                           % P3StartBuildImpModule  %
                           
                           % PushAutoOff  %
                           [ Priority  ]';' { Import  }Block 
                           
                           % PushAutoOn  %
                           Ident 
                           % EndBuildFile  %
                           
                           % P3EndBuildImpModule  %
                           '.' 
                           % PopAuto ; PopAuto ; PopAuto  %
                           

   first  symbols:implementationtok
   
   cannot reachend
*)

# 544 "bnf/m2-3.bnf"
PROCEDURE ImplementationModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 544 "bnf/m2-3.bnf"
BEGIN
# 544 "bnf/m2-3.bnf"
   Expect(implementationtok, stopset0, stopset1 + SetOfStop1{moduletok}, stopset2) ;
# 544 "bnf/m2-3.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 544 "bnf/m2-3.bnf"
   PushAutoOn  ;
# 545 "bnf/m2-3.bnf"
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
# 545 "bnf/m2-3.bnf"
   StartBuildModFile  ;
# 546 "bnf/m2-3.bnf"
   P3StartBuildImpModule  ;
# 547 "bnf/m2-3.bnf"
   PushAutoOff  ;
# 548 "bnf/m2-3.bnf"
# 549 "bnf/m2-3.bnf"
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
# 549 "bnf/m2-3.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 550 "bnf/m2-3.bnf"
# 551 "bnf/m2-3.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 552 "bnf/m2-3.bnf"
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 552 "bnf/m2-3.bnf"
   PushAutoOn  ;
# 554 "bnf/m2-3.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 554 "bnf/m2-3.bnf"
   EndBuildFile  ;
# 555 "bnf/m2-3.bnf"
   P3EndBuildImpModule  ;
# 556 "bnf/m2-3.bnf"
   Expect(periodtok, stopset0, stopset1, stopset2) ;
# 556 "bnf/m2-3.bnf"
   PopAuto ; PopAuto ; PopAuto  ;
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

# 559 "bnf/m2-3.bnf"
PROCEDURE ImplementationOrProgramModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 559 "bnf/m2-3.bnf"
BEGIN
# 559 "bnf/m2-3.bnf"
   PushAutoOff  ;
# 560 "bnf/m2-3.bnf"
   IF currenttoken=implementationtok
   THEN
      ImplementationModule(stopset0, stopset1, stopset2) ;
# 560 "bnf/m2-3.bnf"
   ELSIF currenttoken=moduletok
   THEN
      ProgramModule(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: MODULE IMPLEMENTATION')
   END ;
# 560 "bnf/m2-3.bnf"
   PopAuto  ;
END ImplementationOrProgramModule ;


(*
   Number := Integer  | Real 

   first  symbols:realtok, integertok
   
   cannot reachend
*)

# 563 "bnf/m2-3.bnf"
PROCEDURE Number (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 563 "bnf/m2-3.bnf"
BEGIN
# 563 "bnf/m2-3.bnf"
   IF currenttoken=integertok
   THEN
      Integer(stopset0, stopset1, stopset2) ;
# 563 "bnf/m2-3.bnf"
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

# 565 "bnf/m2-3.bnf"
PROCEDURE Qualident (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
 name: Name;
                                                                                 Type, Sym: CARDINAL ; 
# 565 "bnf/m2-3.bnf"
BEGIN
# 565 "bnf/m2-3.bnf"
# 568 "bnf/m2-3.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 568 "bnf/m2-3.bnf"
# 585 "bnf/m2-3.bnf"
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
# 586 "bnf/m2-3.bnf"
   WHILE currenttoken=periodtok DO
      Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 586 "bnf/m2-3.bnf"
      Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
   END (* while *) ;
# 586 "bnf/m2-3.bnf"
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

# 589 "bnf/m2-3.bnf"
PROCEDURE ConstantDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 589 "bnf/m2-3.bnf"
BEGIN
# 589 "bnf/m2-3.bnf"
   PushAutoOn  ;
# 590 "bnf/m2-3.bnf"
   Ident(stopset0 + SetOfStop0{equaltok}, stopset1, stopset2) ;
# 590 "bnf/m2-3.bnf"
   Expect(equaltok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 590 "bnf/m2-3.bnf"
   BuildConst  ;
# 591 "bnf/m2-3.bnf"
   ConstExpression(stopset0, stopset1, stopset2) ;
# 591 "bnf/m2-3.bnf"
   BuildAssignment  ;
# 592 "bnf/m2-3.bnf"
   PopAuto  ;
END ConstantDeclaration ;


(*
   ConstExpression := 
                      % PushAutoOn  %
                      SimpleConstExpr [ Relation SimpleConstExpr 
                                        
                                        % BuildRelOp  %
                                         ]
                      % PopAuto  %
                      

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, lcbratok, identtok, minustok, plustok
   
   cannot reachend
*)

# 595 "bnf/m2-3.bnf"
PROCEDURE ConstExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 595 "bnf/m2-3.bnf"
BEGIN
# 595 "bnf/m2-3.bnf"
   PushAutoOn  ;
# 596 "bnf/m2-3.bnf"
   SimpleConstExpr(stopset0 + SetOfStop0{equaltok, hashtok, lessgreatertok, lesstok, lessequaltok, greatertok, greaterequaltok}, stopset1 + SetOfStop1{intok}, stopset2) ;
# 596 "bnf/m2-3.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok})) OR
      (currenttoken=intok)
   THEN
      Relation(stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 596 "bnf/m2-3.bnf"
      SimpleConstExpr(stopset0, stopset1, stopset2) ;
# 596 "bnf/m2-3.bnf"
      BuildRelOp  ;
   END ;
# 597 "bnf/m2-3.bnf"
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

# 600 "bnf/m2-3.bnf"
PROCEDURE Relation (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 600 "bnf/m2-3.bnf"
BEGIN
# 600 "bnf/m2-3.bnf"
   IF currenttoken=equaltok
   THEN
      Expect(equaltok, stopset0, stopset1, stopset2) ;
# 600 "bnf/m2-3.bnf"
      PushT(EqualTok)  ;
# 601 "bnf/m2-3.bnf"
   ELSIF currenttoken=hashtok
   THEN
      Expect(hashtok, stopset0, stopset1, stopset2) ;
# 601 "bnf/m2-3.bnf"
      PushT(HashTok)  ;
# 602 "bnf/m2-3.bnf"
   ELSIF currenttoken=lessgreatertok
   THEN
      Expect(lessgreatertok, stopset0, stopset1, stopset2) ;
# 602 "bnf/m2-3.bnf"
      PushT(LessGreaterTok)  ;
# 603 "bnf/m2-3.bnf"
   ELSIF currenttoken=lesstok
   THEN
      Expect(lesstok, stopset0, stopset1, stopset2) ;
# 603 "bnf/m2-3.bnf"
      PushT(LessTok)  ;
# 604 "bnf/m2-3.bnf"
   ELSIF currenttoken=lessequaltok
   THEN
      Expect(lessequaltok, stopset0, stopset1, stopset2) ;
# 604 "bnf/m2-3.bnf"
      PushT(LessEqualTok)  ;
# 605 "bnf/m2-3.bnf"
   ELSIF currenttoken=greatertok
   THEN
      Expect(greatertok, stopset0, stopset1, stopset2) ;
# 605 "bnf/m2-3.bnf"
      PushT(GreaterTok)  ;
# 606 "bnf/m2-3.bnf"
   ELSIF currenttoken=greaterequaltok
   THEN
      Expect(greaterequaltok, stopset0, stopset1, stopset2) ;
# 606 "bnf/m2-3.bnf"
      PushT(GreaterEqualTok)  ;
# 607 "bnf/m2-3.bnf"
   ELSIF currenttoken=intok
   THEN
      Expect(intok, stopset0, stopset1, stopset2) ;
# 607 "bnf/m2-3.bnf"
      PushT(InTok)  ;
   ELSE
      ErrorArray('expecting one of: IN >= > <= < <> # =')
   END ;
END Relation ;


(*
   SimpleConstExpr := UnaryOrConstTerm { AddOperator ConstTerm 
                                         
                                         % BuildBinaryOp  %
                                          }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, lcbratok, identtok, minustok, plustok
   
   cannot reachend
*)

# 610 "bnf/m2-3.bnf"
PROCEDURE SimpleConstExpr (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 610 "bnf/m2-3.bnf"
BEGIN
# 610 "bnf/m2-3.bnf"
   UnaryOrConstTerm(stopset0 + SetOfStop0{plustok, minustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
# 610 "bnf/m2-3.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok})) OR
         (currenttoken=ortok) DO
      AddOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 610 "bnf/m2-3.bnf"
      ConstTerm(stopset0 + SetOfStop0{minustok, plustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
# 610 "bnf/m2-3.bnf"
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

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, lcbratok, identtok, minustok, plustok
   
   cannot reachend
*)

# 613 "bnf/m2-3.bnf"
PROCEDURE UnaryOrConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 613 "bnf/m2-3.bnf"
BEGIN
# 613 "bnf/m2-3.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 613 "bnf/m2-3.bnf"
      PushT(PlusTok)  ;
# 614 "bnf/m2-3.bnf"
      ConstTerm(stopset0, stopset1, stopset2) ;
# 614 "bnf/m2-3.bnf"
      BuildUnaryOp  ;
# 616 "bnf/m2-3.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 616 "bnf/m2-3.bnf"
      PushT(MinusTok)  ;
# 617 "bnf/m2-3.bnf"
      ConstTerm(stopset0, stopset1, stopset2) ;
# 617 "bnf/m2-3.bnf"
      BuildUnaryOp  ;
# 619 "bnf/m2-3.bnf"
   ELSIF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok})) OR
         (currenttoken=nottok) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {stringtok, realtok, integertok, identtok}))
   THEN
      ConstTerm(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: NOT ( string real number integer number { identifier - +')
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

# 621 "bnf/m2-3.bnf"
PROCEDURE AddOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 621 "bnf/m2-3.bnf"
BEGIN
# 621 "bnf/m2-3.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0, stopset1, stopset2) ;
# 621 "bnf/m2-3.bnf"
# 622 "bnf/m2-3.bnf"
      PushT(PlusTok) ;
      RecordOp  ;
# 623 "bnf/m2-3.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0, stopset1, stopset2) ;
# 623 "bnf/m2-3.bnf"
# 624 "bnf/m2-3.bnf"
      PushT(MinusTok) ;
      RecordOp  ;
# 625 "bnf/m2-3.bnf"
   ELSIF currenttoken=ortok
   THEN
      Expect(ortok, stopset0, stopset1, stopset2) ;
# 625 "bnf/m2-3.bnf"
# 626 "bnf/m2-3.bnf"
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

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, lcbratok, identtok
   
   cannot reachend
*)

# 629 "bnf/m2-3.bnf"
PROCEDURE ConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 629 "bnf/m2-3.bnf"
BEGIN
# 629 "bnf/m2-3.bnf"
   ConstFactor(stopset0 + SetOfStop0{timestok, dividetok, andtok, ambersandtok}, stopset1 + SetOfStop1{divtok, modtok}, stopset2) ;
# 629 "bnf/m2-3.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {ambersandtok, andtok, dividetok, timestok})) OR
         ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {modtok, divtok})) DO
      MulOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 629 "bnf/m2-3.bnf"
      ConstFactor(stopset0 + SetOfStop0{ambersandtok, andtok, dividetok, timestok}, stopset1 + SetOfStop1{modtok, divtok}, stopset2) ;
# 629 "bnf/m2-3.bnf"
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

# 632 "bnf/m2-3.bnf"
PROCEDURE MulOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 632 "bnf/m2-3.bnf"
BEGIN
# 632 "bnf/m2-3.bnf"
   IF currenttoken=timestok
   THEN
      Expect(timestok, stopset0, stopset1, stopset2) ;
# 632 "bnf/m2-3.bnf"
# 633 "bnf/m2-3.bnf"
      PushT(TimesTok) ;
      RecordOp  ;
# 634 "bnf/m2-3.bnf"
   ELSIF currenttoken=dividetok
   THEN
      Expect(dividetok, stopset0, stopset1, stopset2) ;
# 634 "bnf/m2-3.bnf"
# 635 "bnf/m2-3.bnf"
      PushT(DivTok) ;
      RecordOp  ;
# 636 "bnf/m2-3.bnf"
   ELSIF currenttoken=divtok
   THEN
      Expect(divtok, stopset0, stopset1, stopset2) ;
# 636 "bnf/m2-3.bnf"
# 637 "bnf/m2-3.bnf"
      PushT(DivTok) ;
      RecordOp  ;
# 638 "bnf/m2-3.bnf"
   ELSIF currenttoken=modtok
   THEN
      Expect(modtok, stopset0, stopset1, stopset2) ;
# 638 "bnf/m2-3.bnf"
# 639 "bnf/m2-3.bnf"
      PushT(ModTok) ;
      RecordOp  ;
# 640 "bnf/m2-3.bnf"
   ELSIF currenttoken=andtok
   THEN
      Expect(andtok, stopset0, stopset1, stopset2) ;
# 640 "bnf/m2-3.bnf"
# 641 "bnf/m2-3.bnf"
      PushT(AndTok) ;
      RecordOp  ;
# 642 "bnf/m2-3.bnf"
   ELSIF currenttoken=ambersandtok
   THEN
      Expect(ambersandtok, stopset0, stopset1, stopset2) ;
# 642 "bnf/m2-3.bnf"
# 643 "bnf/m2-3.bnf"
      PushT(AmbersandTok) ;
      RecordOp  ;
   ELSE
      ErrorArray('expecting one of: & AND MOD DIV / *')
   END ;
END MulOperator ;


(*
   ConstFactor := ConstQualidentOrSet  | 
                  Number  | ConstString  | '(' ConstExpression 
                  ')'  | 'NOT' ConstFactor 
                  % BuildNot  %
                  

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, lcbratok, identtok
   
   cannot reachend
*)

# 646 "bnf/m2-3.bnf"
PROCEDURE ConstFactor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 646 "bnf/m2-3.bnf"
BEGIN
# 646 "bnf/m2-3.bnf"
   IF (currenttoken=lcbratok) OR
      (currenttoken=identtok)
   THEN
      ConstQualidentOrSet(stopset0, stopset1, stopset2) ;
# 646 "bnf/m2-3.bnf"
   ELSIF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {realtok, integertok}))
   THEN
      Number(stopset0, stopset1, stopset2) ;
# 647 "bnf/m2-3.bnf"
   ELSIF currenttoken=stringtok
   THEN
      ConstString(stopset0, stopset1, stopset2) ;
# 648 "bnf/m2-3.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 648 "bnf/m2-3.bnf"
      ConstExpression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 648 "bnf/m2-3.bnf"
      Expect(rparatok, stopset0, stopset1, stopset2) ;
# 648 "bnf/m2-3.bnf"
   ELSIF currenttoken=nottok
   THEN
      Expect(nottok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 648 "bnf/m2-3.bnf"
      ConstFactor(stopset0, stopset1, stopset2) ;
# 648 "bnf/m2-3.bnf"
      BuildNot  ;
   ELSE
      ErrorArray('expecting one of: NOT ( string real number integer number { identifier')
   END ;
END ConstFactor ;


(*
   ConstString := string 

   first  symbols:stringtok
   
   cannot reachend
*)

# 654 "bnf/m2-3.bnf"
PROCEDURE ConstString (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 654 "bnf/m2-3.bnf"
BEGIN
# 654 "bnf/m2-3.bnf"
# 655 "bnf/m2-3.bnf"
   string(stopset0, stopset1, stopset2) ;
END ConstString ;


(*
   ConstQualidentOrSet := Qualident [ SimpleSet  ] | 
                          
                          % BuildBitsetStart  %
                          SimpleSet 

   first  symbols:lcbratok, identtok
   
   cannot reachend
*)

# 657 "bnf/m2-3.bnf"
PROCEDURE ConstQualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 657 "bnf/m2-3.bnf"
BEGIN
# 657 "bnf/m2-3.bnf"
   IF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok}, stopset1, stopset2) ;
# 657 "bnf/m2-3.bnf"
      IF currenttoken=lcbratok
      THEN
         SimpleSet(stopset0, stopset1, stopset2) ;
      END ;
# 658 "bnf/m2-3.bnf"
   ELSE
# 658 "bnf/m2-3.bnf"
      BuildBitsetStart  ;
# 660 "bnf/m2-3.bnf"
      SimpleSet(stopset0, stopset1, stopset2) ;
   END ;
END ConstQualidentOrSet ;


(*
   QualidentOrSet := SimpleSet 
                     % BuildBitsetStart  %
                      | Qualident [ SimpleSet  ]

   first  symbols:identtok, lcbratok
   
   cannot reachend
*)

# 662 "bnf/m2-3.bnf"
PROCEDURE QualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 662 "bnf/m2-3.bnf"
BEGIN
# 662 "bnf/m2-3.bnf"
   IF currenttoken=lcbratok
   THEN
      SimpleSet(stopset0, stopset1, stopset2) ;
# 662 "bnf/m2-3.bnf"
      BuildBitsetStart  ;
# 663 "bnf/m2-3.bnf"
   ELSIF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok}, stopset1, stopset2) ;
# 663 "bnf/m2-3.bnf"
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

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, lcbratok, identtok, minustok, plustok
   
   reachend
*)

# 667 "bnf/m2-3.bnf"
PROCEDURE Element (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 667 "bnf/m2-3.bnf"
BEGIN
# 667 "bnf/m2-3.bnf"
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 667 "bnf/m2-3.bnf"
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 667 "bnf/m2-3.bnf"
      ConstExpression(stopset0, stopset1, stopset2) ;
# 667 "bnf/m2-3.bnf"
      BuildSetRange  ;
# 668 "bnf/m2-3.bnf"
   ELSE
# 668 "bnf/m2-3.bnf"
      BuildElement (* epsilon *)  ;
   END ;
END Element ;


(*
   TypeDeclaration := Ident '=' Type 

   first  symbols:identtok
   
   cannot reachend
*)

# 671 "bnf/m2-3.bnf"
PROCEDURE TypeDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 671 "bnf/m2-3.bnf"
BEGIN
# 671 "bnf/m2-3.bnf"
   Ident(stopset0 + SetOfStop0{equaltok}, stopset1, stopset2) ;
# 671 "bnf/m2-3.bnf"
   Expect(equaltok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 672 "bnf/m2-3.bnf"
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

# 674 "bnf/m2-3.bnf"
PROCEDURE Type (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 674 "bnf/m2-3.bnf"
BEGIN
# 674 "bnf/m2-3.bnf"
# 675 "bnf/m2-3.bnf"
   PushAutoOff  ;
# 676 "bnf/m2-3.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lsbratok, lparatok})) OR
      (currenttoken=identtok)
   THEN
      SimpleType(stopset0, stopset1, stopset2) ;
# 677 "bnf/m2-3.bnf"
   ELSIF currenttoken=arraytok
   THEN
      ArrayType(stopset0, stopset1, stopset2) ;
# 677 "bnf/m2-3.bnf"
   ELSIF currenttoken=recordtok
   THEN
      RecordType(stopset0, stopset1, stopset2) ;
# 678 "bnf/m2-3.bnf"
   ELSIF currenttoken=settok
   THEN
      SetType(stopset0, stopset1, stopset2) ;
# 679 "bnf/m2-3.bnf"
   ELSIF currenttoken=pointertok
   THEN
      PointerType(stopset0, stopset1, stopset2) ;
# 680 "bnf/m2-3.bnf"
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureType(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: PROCEDURE POINTER SET RECORD ARRAY [ ( identifier')
   END ;
# 680 "bnf/m2-3.bnf"
   PopAuto  ;
END Type ;


(*
   SimpleType := Qualident  | Enumeration  | SubrangeType 

   first  symbols:lsbratok, lparatok, identtok
   
   cannot reachend
*)

# 683 "bnf/m2-3.bnf"
PROCEDURE SimpleType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 683 "bnf/m2-3.bnf"
BEGIN
# 683 "bnf/m2-3.bnf"
   IF currenttoken=identtok
   THEN
      Qualident(stopset0, stopset1, stopset2) ;
# 683 "bnf/m2-3.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Enumeration(stopset0, stopset1, stopset2) ;
# 683 "bnf/m2-3.bnf"
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

# 685 "bnf/m2-3.bnf"
PROCEDURE Enumeration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 685 "bnf/m2-3.bnf"
BEGIN
# 685 "bnf/m2-3.bnf"
   Expect(lparatok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 686 "bnf/m2-3.bnf"
   IdentList(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 688 "bnf/m2-3.bnf"
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

# 691 "bnf/m2-3.bnf"
PROCEDURE IdentList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR

                                                                                on: BOOLEAN ;
                                                                                n : CARDINAL ; 
# 691 "bnf/m2-3.bnf"
BEGIN
# 691 "bnf/m2-3.bnf"
   Ident(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 691 "bnf/m2-3.bnf"
# 694 "bnf/m2-3.bnf"
# 698 "bnf/m2-3.bnf"
   on := IsAutoPushOn() ;
   IF on
   THEN
      n := 1
   END  ;
# 699 "bnf/m2-3.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 699 "bnf/m2-3.bnf"
      Ident(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 699 "bnf/m2-3.bnf"
# 702 "bnf/m2-3.bnf"
      IF on
      THEN
         INC(n)
      END  ;
   END (* while *) ;
# 703 "bnf/m2-3.bnf"
# 706 "bnf/m2-3.bnf"
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

# 709 "bnf/m2-3.bnf"
PROCEDURE SubrangeType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 709 "bnf/m2-3.bnf"
BEGIN
# 709 "bnf/m2-3.bnf"
   Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 709 "bnf/m2-3.bnf"
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 709 "bnf/m2-3.bnf"
   Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 709 "bnf/m2-3.bnf"
   ConstExpression(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 709 "bnf/m2-3.bnf"
   Expect(rsbratok, stopset0, stopset1, stopset2) ;
# 709 "bnf/m2-3.bnf"
   BuildSubrange ;  ;
END SubrangeType ;


(*
   ArrayType := 'ARRAY' SimpleType { ',' SimpleType  }'OF' Type 

   first  symbols:arraytok
   
   cannot reachend
*)

# 712 "bnf/m2-3.bnf"
PROCEDURE ArrayType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 712 "bnf/m2-3.bnf"
BEGIN
# 712 "bnf/m2-3.bnf"
   Expect(arraytok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 715 "bnf/m2-3.bnf"
   SimpleType(stopset0 + SetOfStop0{commatok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 715 "bnf/m2-3.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 717 "bnf/m2-3.bnf"
      SimpleType(stopset0 + SetOfStop0{commatok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
   END (* while *) ;
# 717 "bnf/m2-3.bnf"
   Expect(oftok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 719 "bnf/m2-3.bnf"
   Type(stopset0, stopset1, stopset2) ;
END ArrayType ;


(*
   RecordType := 'RECORD' FieldListSequence 'END' 

   first  symbols:recordtok
   
   cannot reachend
*)

# 721 "bnf/m2-3.bnf"
PROCEDURE RecordType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 721 "bnf/m2-3.bnf"
BEGIN
# 721 "bnf/m2-3.bnf"
   Expect(recordtok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok, endtok}, stopset2 + SetOfStop2{identtok}) ;
# 722 "bnf/m2-3.bnf"
   FieldListSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 722 "bnf/m2-3.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END RecordType ;


(*
   FieldListSequence := FieldListStatement { ';' FieldListStatement  }

   first  symbols:semicolontok, casetok, identtok
   
   reachend
*)

# 725 "bnf/m2-3.bnf"
PROCEDURE FieldListSequence (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 725 "bnf/m2-3.bnf"
BEGIN
# 725 "bnf/m2-3.bnf"
   FieldListStatement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 725 "bnf/m2-3.bnf"
   WHILE currenttoken=semicolontok DO
      Expect(semicolontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok}, stopset2 + SetOfStop2{identtok}) ;
# 725 "bnf/m2-3.bnf"
      FieldListStatement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END (* while *) ;
END FieldListSequence ;


(*
   FieldListStatement := [ FieldList  ]

   first  symbols:casetok, identtok
   
   reachend
*)

# 728 "bnf/m2-3.bnf"
PROCEDURE FieldListStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 728 "bnf/m2-3.bnf"
BEGIN
# 728 "bnf/m2-3.bnf"
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

# 739 "bnf/m2-3.bnf"
PROCEDURE FieldList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 739 "bnf/m2-3.bnf"
BEGIN
# 739 "bnf/m2-3.bnf"
   IF currenttoken=identtok
   THEN
      IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 739 "bnf/m2-3.bnf"
      Expect(colontok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 741 "bnf/m2-3.bnf"
      Type(stopset0, stopset1, stopset2) ;
# 742 "bnf/m2-3.bnf"
   ELSIF currenttoken=casetok
   THEN
      Expect(casetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 742 "bnf/m2-3.bnf"
      Ident(stopset0 + SetOfStop0{colontok, periodtok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 742 "bnf/m2-3.bnf"
      IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {periodtok, colontok}))
      THEN
         (* seen optional [ | ] expression *)
# 742 "bnf/m2-3.bnf"
         IF currenttoken=colontok
         THEN
            Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 743 "bnf/m2-3.bnf"
            Qualident(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 744 "bnf/m2-3.bnf"
         ELSIF currenttoken=periodtok
         THEN
            Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 746 "bnf/m2-3.bnf"
            Qualident(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
         ELSE
            ErrorArray('expecting one of: . :')
         END ;
         (* end of optional [ | ] expression *)
      END ;
# 746 "bnf/m2-3.bnf"
      Expect(oftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 750 "bnf/m2-3.bnf"
      Varient(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{elsetok, endtok}, stopset2) ;
# 750 "bnf/m2-3.bnf"
      WHILE currenttoken=bartok DO
         Expect(bartok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 752 "bnf/m2-3.bnf"
         Varient(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{endtok, elsetok}, stopset2) ;
      END (* while *) ;
# 753 "bnf/m2-3.bnf"
      IF currenttoken=elsetok
      THEN
         Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok, endtok}, stopset2 + SetOfStop2{identtok}) ;
# 755 "bnf/m2-3.bnf"
         FieldListSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
      END ;
# 755 "bnf/m2-3.bnf"
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

# 758 "bnf/m2-3.bnf"
PROCEDURE Varient (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 758 "bnf/m2-3.bnf"
BEGIN
# 758 "bnf/m2-3.bnf"
   SilentCaseLabelList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 758 "bnf/m2-3.bnf"
   Expect(colontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok}, stopset2 + SetOfStop2{identtok}) ;
# 758 "bnf/m2-3.bnf"
   FieldListSequence(stopset0, stopset1, stopset2) ;
END Varient ;


(*
   SilentCaseLabelList := SilentCaseLabels { ',' SilentCaseLabels  }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 760 "bnf/m2-3.bnf"
PROCEDURE SilentCaseLabelList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 760 "bnf/m2-3.bnf"
BEGIN
# 760 "bnf/m2-3.bnf"
   SilentCaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 760 "bnf/m2-3.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 760 "bnf/m2-3.bnf"
      SilentCaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END SilentCaseLabelList ;


(*
   SilentCaseLabels := SilentConstExpression [ '..' SilentConstExpression  ]

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 762 "bnf/m2-3.bnf"
PROCEDURE SilentCaseLabels (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 762 "bnf/m2-3.bnf"
BEGIN
# 762 "bnf/m2-3.bnf"
   SilentConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 762 "bnf/m2-3.bnf"
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 762 "bnf/m2-3.bnf"
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

# 772 "bnf/m2-3.bnf"
PROCEDURE SilentConstExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 772 "bnf/m2-3.bnf"
BEGIN
# 772 "bnf/m2-3.bnf"
   PushAutoOff  ;
# 774 "bnf/m2-3.bnf"
   SilentSimpleConstExpr(stopset0 + SetOfStop0{equaltok, hashtok, lessgreatertok, lesstok, lessequaltok, greatertok, greaterequaltok}, stopset1 + SetOfStop1{intok}, stopset2) ;
# 774 "bnf/m2-3.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok})) OR
      (currenttoken=intok)
   THEN
      SilentRelation(stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 774 "bnf/m2-3.bnf"
      SilentSimpleConstExpr(stopset0, stopset1, stopset2) ;
   END ;
# 774 "bnf/m2-3.bnf"
   PopAuto  ;
END SilentConstExpression ;


(*
   SilentRelation := '='  | '#'  | '<>'  | '<'  | '<='  | 
                     '>'  | '>='  | 'IN' 

   first  symbols:intok, greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok
   
   cannot reachend
*)

# 777 "bnf/m2-3.bnf"
PROCEDURE SilentRelation (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 777 "bnf/m2-3.bnf"
BEGIN
# 777 "bnf/m2-3.bnf"
   IF currenttoken=equaltok
   THEN
      Expect(equaltok, stopset0, stopset1, stopset2) ;
# 777 "bnf/m2-3.bnf"
   ELSIF currenttoken=hashtok
   THEN
      Expect(hashtok, stopset0, stopset1, stopset2) ;
# 777 "bnf/m2-3.bnf"
   ELSIF currenttoken=lessgreatertok
   THEN
      Expect(lessgreatertok, stopset0, stopset1, stopset2) ;
# 777 "bnf/m2-3.bnf"
   ELSIF currenttoken=lesstok
   THEN
      Expect(lesstok, stopset0, stopset1, stopset2) ;
# 777 "bnf/m2-3.bnf"
   ELSIF currenttoken=lessequaltok
   THEN
      Expect(lessequaltok, stopset0, stopset1, stopset2) ;
# 777 "bnf/m2-3.bnf"
   ELSIF currenttoken=greatertok
   THEN
      Expect(greatertok, stopset0, stopset1, stopset2) ;
# 777 "bnf/m2-3.bnf"
   ELSIF currenttoken=greaterequaltok
   THEN
      Expect(greaterequaltok, stopset0, stopset1, stopset2) ;
# 777 "bnf/m2-3.bnf"
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

# 779 "bnf/m2-3.bnf"
PROCEDURE SilentSimpleConstExpr (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 779 "bnf/m2-3.bnf"
BEGIN
# 779 "bnf/m2-3.bnf"
   SilentUnaryOrConstTerm(stopset0 + SetOfStop0{plustok, minustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
# 779 "bnf/m2-3.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok})) OR
         (currenttoken=ortok) DO
      SilentAddOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 779 "bnf/m2-3.bnf"
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

# 781 "bnf/m2-3.bnf"
PROCEDURE SilentUnaryOrConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 781 "bnf/m2-3.bnf"
BEGIN
# 781 "bnf/m2-3.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 781 "bnf/m2-3.bnf"
      SilentConstTerm(stopset0, stopset1, stopset2) ;
# 781 "bnf/m2-3.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 781 "bnf/m2-3.bnf"
      SilentConstTerm(stopset0, stopset1, stopset2) ;
# 781 "bnf/m2-3.bnf"
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

# 783 "bnf/m2-3.bnf"
PROCEDURE SilentAddOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 783 "bnf/m2-3.bnf"
BEGIN
# 783 "bnf/m2-3.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0, stopset1, stopset2) ;
# 783 "bnf/m2-3.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0, stopset1, stopset2) ;
# 783 "bnf/m2-3.bnf"
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

# 785 "bnf/m2-3.bnf"
PROCEDURE SilentConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 785 "bnf/m2-3.bnf"
BEGIN
# 785 "bnf/m2-3.bnf"
   SilentConstFactor(stopset0 + SetOfStop0{timestok, dividetok, andtok, ambersandtok}, stopset1 + SetOfStop1{divtok, modtok}, stopset2) ;
# 785 "bnf/m2-3.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {ambersandtok, andtok, dividetok, timestok})) OR
         ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {modtok, divtok})) DO
      SilentMulOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 785 "bnf/m2-3.bnf"
      SilentConstFactor(stopset0 + SetOfStop0{ambersandtok, andtok, dividetok, timestok}, stopset1 + SetOfStop1{modtok, divtok}, stopset2) ;
   END (* while *) ;
END SilentConstTerm ;


(*
   SilentMulOperator := '*'  | '/'  | 'DIV'  | 'MOD'  | 
                        'AND'  | '&' 

   first  symbols:ambersandtok, andtok, modtok, divtok, dividetok, timestok
   
   cannot reachend
*)

# 787 "bnf/m2-3.bnf"
PROCEDURE SilentMulOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 787 "bnf/m2-3.bnf"
BEGIN
# 787 "bnf/m2-3.bnf"
   IF currenttoken=timestok
   THEN
      Expect(timestok, stopset0, stopset1, stopset2) ;
# 787 "bnf/m2-3.bnf"
   ELSIF currenttoken=dividetok
   THEN
      Expect(dividetok, stopset0, stopset1, stopset2) ;
# 787 "bnf/m2-3.bnf"
   ELSIF currenttoken=divtok
   THEN
      Expect(divtok, stopset0, stopset1, stopset2) ;
# 787 "bnf/m2-3.bnf"
   ELSIF currenttoken=modtok
   THEN
      Expect(modtok, stopset0, stopset1, stopset2) ;
# 787 "bnf/m2-3.bnf"
   ELSIF currenttoken=andtok
   THEN
      Expect(andtok, stopset0, stopset1, stopset2) ;
# 787 "bnf/m2-3.bnf"
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

# 789 "bnf/m2-3.bnf"
PROCEDURE SilentConstFactor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 789 "bnf/m2-3.bnf"
BEGIN
# 789 "bnf/m2-3.bnf"
   IF (currenttoken=lcbratok) OR
      (currenttoken=identtok)
   THEN
      SilentConstQualidentOrSet(stopset0, stopset1, stopset2) ;
# 789 "bnf/m2-3.bnf"
   ELSIF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {realtok, integertok}))
   THEN
      Number(stopset0, stopset1, stopset2) ;
# 790 "bnf/m2-3.bnf"
   ELSIF currenttoken=stringtok
   THEN
      SilentConstString(stopset0, stopset1, stopset2) ;
# 791 "bnf/m2-3.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 791 "bnf/m2-3.bnf"
      SilentConstExpression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 791 "bnf/m2-3.bnf"
      Expect(rparatok, stopset0, stopset1, stopset2) ;
# 791 "bnf/m2-3.bnf"
   ELSIF currenttoken=nottok
   THEN
      Expect(nottok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 791 "bnf/m2-3.bnf"
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

# 793 "bnf/m2-3.bnf"
PROCEDURE SilentConstString (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 793 "bnf/m2-3.bnf"
BEGIN
# 793 "bnf/m2-3.bnf"
   string(stopset0, stopset1, stopset2) ;
END SilentConstString ;


(*
   SilentConstQualidentOrSet := SilentSimpleSet  | 
                                Qualident [ SilentSimpleSet  ]

   first  symbols:identtok, lcbratok
   
   cannot reachend
*)

# 795 "bnf/m2-3.bnf"
PROCEDURE SilentConstQualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 795 "bnf/m2-3.bnf"
BEGIN
# 795 "bnf/m2-3.bnf"
   IF currenttoken=lcbratok
   THEN
      SilentSimpleSet(stopset0, stopset1, stopset2) ;
# 795 "bnf/m2-3.bnf"
   ELSIF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok}, stopset1, stopset2) ;
# 795 "bnf/m2-3.bnf"
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

# 797 "bnf/m2-3.bnf"
PROCEDURE SilentSimpleSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 797 "bnf/m2-3.bnf"
BEGIN
# 797 "bnf/m2-3.bnf"
   Expect(lcbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 797 "bnf/m2-3.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {commatok, lparatok, lcbratok, minustok, plustok})) OR
      (currenttoken=nottok) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {stringtok, realtok, integertok, identtok}))
   THEN
      SilentElement(stopset0 + SetOfStop0{commatok, rcbratok}, stopset1, stopset2) ;
# 797 "bnf/m2-3.bnf"
      WHILE currenttoken=commatok DO
         Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok, commatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 797 "bnf/m2-3.bnf"
         SilentElement(stopset0 + SetOfStop0{rcbratok, commatok}, stopset1, stopset2) ;
      END (* while *) ;
   END ;
# 797 "bnf/m2-3.bnf"
   Expect(rcbratok, stopset0, stopset1, stopset2) ;
END SilentSimpleSet ;


(*
   SilentElement := SilentConstExpression [ '..' SilentConstExpression  ]

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   reachend
*)

# 799 "bnf/m2-3.bnf"
PROCEDURE SilentElement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 799 "bnf/m2-3.bnf"
BEGIN
# 799 "bnf/m2-3.bnf"
   SilentConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 799 "bnf/m2-3.bnf"
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 799 "bnf/m2-3.bnf"
      SilentConstExpression(stopset0, stopset1, stopset2) ;
   END ;
END SilentElement ;


(*
   SetType := 'SET' 'OF' SimpleType 

   first  symbols:settok
   
   cannot reachend
*)

# 803 "bnf/m2-3.bnf"
PROCEDURE SetType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 803 "bnf/m2-3.bnf"
BEGIN
# 803 "bnf/m2-3.bnf"
   Expect(settok, stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 803 "bnf/m2-3.bnf"
   Expect(oftok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 803 "bnf/m2-3.bnf"
   SimpleType(stopset0, stopset1, stopset2) ;
END SetType ;


(*
   PointerType := 'POINTER' 'TO' Type 

   first  symbols:pointertok
   
   cannot reachend
*)

# 805 "bnf/m2-3.bnf"
PROCEDURE PointerType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 805 "bnf/m2-3.bnf"
BEGIN
# 805 "bnf/m2-3.bnf"
   Expect(pointertok, stopset0, stopset1, stopset2 + SetOfStop2{totok}) ;
# 805 "bnf/m2-3.bnf"
   Expect(totok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 807 "bnf/m2-3.bnf"
   Type(stopset0, stopset1, stopset2) ;
END PointerType ;


(*
   ProcedureType := 'PROCEDURE' [ FormalTypeList  ]

   first  symbols:proceduretok
   
   cannot reachend
*)

# 809 "bnf/m2-3.bnf"
PROCEDURE ProcedureType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 809 "bnf/m2-3.bnf"
BEGIN
# 809 "bnf/m2-3.bnf"
   Expect(proceduretok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 810 "bnf/m2-3.bnf"
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

# 812 "bnf/m2-3.bnf"
PROCEDURE FormalTypeList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 812 "bnf/m2-3.bnf"
BEGIN
# 812 "bnf/m2-3.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{rparatok, arraytok}, stopset1, stopset2 + SetOfStop2{vartok, identtok}) ;
# 812 "bnf/m2-3.bnf"
   IF currenttoken=rparatok
   THEN
      Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 812 "bnf/m2-3.bnf"
      FormalReturn(stopset0, stopset1, stopset2) ;
# 813 "bnf/m2-3.bnf"
   ELSIF (currenttoken=arraytok) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, vartok}))
   THEN
      ProcedureParameters(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 813 "bnf/m2-3.bnf"
      Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 813 "bnf/m2-3.bnf"
      FormalReturn(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: identifier ARRAY VAR )')
   END ;
END FormalTypeList ;


(*
   FormalReturn := [ ':' Qualident  ]

   first  symbols:colontok
   
   reachend
*)

# 815 "bnf/m2-3.bnf"
PROCEDURE FormalReturn (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 815 "bnf/m2-3.bnf"
BEGIN
# 815 "bnf/m2-3.bnf"
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 816 "bnf/m2-3.bnf"
      Qualident(stopset0, stopset1, stopset2) ;
   END ;
END FormalReturn ;


(*
   ProcedureParameters := ProcedureParameter { ',' ProcedureParameter  }

   first  symbols:identtok, arraytok, vartok
   
   cannot reachend
*)

# 818 "bnf/m2-3.bnf"
PROCEDURE ProcedureParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 818 "bnf/m2-3.bnf"
BEGIN
# 818 "bnf/m2-3.bnf"
# 819 "bnf/m2-3.bnf"
   ProcedureParameter(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 819 "bnf/m2-3.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{vartok, identtok}) ;
# 819 "bnf/m2-3.bnf"
      ProcedureParameter(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END ProcedureParameters ;


(*
   ProcedureParameter := 'VAR' FormalType  | 
                         FormalType 

   first  symbols:identtok, arraytok, vartok
   
   cannot reachend
*)

# 821 "bnf/m2-3.bnf"
PROCEDURE ProcedureParameter (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 821 "bnf/m2-3.bnf"
BEGIN
# 821 "bnf/m2-3.bnf"
   IF currenttoken=vartok
   THEN
      Expect(vartok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 823 "bnf/m2-3.bnf"
      FormalType(stopset0, stopset1, stopset2) ;
# 825 "bnf/m2-3.bnf"
   ELSIF (currenttoken=arraytok) OR
         (currenttoken=identtok)
   THEN
      FormalType(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: identifier ARRAY VAR')
   END ;
END ProcedureParameter ;


(*
   VariableDeclaration := IdentList ':' Type 

   first  symbols:identtok
   
   cannot reachend
*)

# 827 "bnf/m2-3.bnf"
PROCEDURE VariableDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 827 "bnf/m2-3.bnf"
BEGIN
# 827 "bnf/m2-3.bnf"
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 827 "bnf/m2-3.bnf"
   Expect(colontok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 829 "bnf/m2-3.bnf"
   Type(stopset0, stopset1, stopset2) ;
END VariableDeclaration ;


(*
   Designator := Qualident 
                 % CheckWithReference  %
                 
                 % CheckOuterScopeProcedureVariable  %
                 { SubDesignator  }

   first  symbols:identtok
   
   cannot reachend
*)

# 831 "bnf/m2-3.bnf"
PROCEDURE Designator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 831 "bnf/m2-3.bnf"
BEGIN
# 831 "bnf/m2-3.bnf"
   Qualident(stopset0 + SetOfStop0{periodtok, lsbratok, uparrowtok}, stopset1, stopset2) ;
# 831 "bnf/m2-3.bnf"
   CheckWithReference  ;
# 832 "bnf/m2-3.bnf"
   CheckOuterScopeProcedureVariable  ;
# 833 "bnf/m2-3.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {uparrowtok, lsbratok, periodtok})) DO
      SubDesignator(stopset0 + SetOfStop0{uparrowtok, lsbratok, periodtok}, stopset1, stopset2) ;
   END (* while *) ;
END Designator ;


(*
   SubDesignator := '.' 
                    % VAR Sym, Type,
                          n        : CARDINAL ;
                          name     : Name ;  %
                    
                    % PopTF(Sym, Type) ;
                      PushTF(Sym, Type) ;
                      IF Type=NulSym
                      THEN
                         IF IsModuleKnown(GetSymName(Sym))
                         THEN
                            WriteFormat2('%a looks like a module which has not been globally imported (eg. suggest that you IMPORT %a ;)',
                            GetSymName(Sym), GetSymName(Sym))
                         ELSE
                            WriteFormat1('%a is not a record variable', GetSymName(Sym))
                         END
                      ELSIF NOT IsRecord(Type)
                      THEN
                         WriteFormat1('%a is not a record type', GetSymName(Type))
                      END ;
                      StartScope(Type)  %
                    Ident 
                    % PopT(name) ;
                      Sym := GetLocalSym(Type, name) ;
                      IF Sym=NulSym
                      THEN
                         WriteFormat2('field %a does not exist within record %a', name, GetSymName(Type))
                      END ;
                      Type := GetType(Sym) ;
                      PushTF(Sym, Type) ;
                      EndScope ;
                      PushT(1) ;
                      BuildDesignatorRecord  %
                     | '[' ExpList 
                    % BuildDesignatorArray  %
                    ']'  | '^' 
                    % BuildDesignatorPointer  %
                    

   first  symbols:uparrowtok, lsbratok, periodtok
   
   cannot reachend
*)

# 835 "bnf/m2-3.bnf"
PROCEDURE SubDesignator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
 Sym, Type,
                                                                                 n        : CARDINAL ;
                                                                                 name     : Name ; 
# 835 "bnf/m2-3.bnf"
BEGIN
# 835 "bnf/m2-3.bnf"
   IF currenttoken=periodtok
   THEN
      Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 835 "bnf/m2-3.bnf"
# 838 "bnf/m2-3.bnf"
# 853 "bnf/m2-3.bnf"
      PopTF(Sym, Type) ;
      PushTF(Sym, Type) ;
      IF Type=NulSym
      THEN
         IF IsModuleKnown(GetSymName(Sym))
         THEN
            WriteFormat2('%a looks like a module which has not been globally imported (eg. suggest that you IMPORT %a ;)',
            GetSymName(Sym), GetSymName(Sym))
         ELSE
            WriteFormat1('%a is not a record variable', GetSymName(Sym))
         END
      ELSIF NOT IsRecord(Type)
      THEN
         WriteFormat1('%a is not a record type', GetSymName(Type))
      END ;
      StartScope(Type)  ;
# 855 "bnf/m2-3.bnf"
      Ident(stopset0, stopset1, stopset2) ;
# 855 "bnf/m2-3.bnf"
# 865 "bnf/m2-3.bnf"
      PopT(name) ;
      Sym := GetLocalSym(Type, name) ;
      IF Sym=NulSym
      THEN
         WriteFormat2('field %a does not exist within record %a', name, GetSymName(Type))
      END ;
      Type := GetType(Sym) ;
      PushTF(Sym, Type) ;
      EndScope ;
      PushT(1) ;
      BuildDesignatorRecord  ;
# 866 "bnf/m2-3.bnf"
   ELSIF currenttoken=lsbratok
   THEN
      Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 866 "bnf/m2-3.bnf"
      ExpList(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 866 "bnf/m2-3.bnf"
      BuildDesignatorArray  ;
# 867 "bnf/m2-3.bnf"
      Expect(rsbratok, stopset0, stopset1, stopset2) ;
# 868 "bnf/m2-3.bnf"
   ELSIF currenttoken=uparrowtok
   THEN
      Expect(uparrowtok, stopset0, stopset1, stopset2) ;
# 868 "bnf/m2-3.bnf"
      BuildDesignatorPointer  ;
   ELSE
      ErrorArray('expecting one of: ^ [ .')
   END ;
END SubDesignator ;


(*
   ExpList := 
              % VAR n: CARDINAL ;  %
              Expression 
              % n := 1  %
              { ',' Expression 
                % INC(n)  %
                 }
              % PushT(n)  %
              

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

# 871 "bnf/m2-3.bnf"
PROCEDURE ExpList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
 n: CARDINAL ; 
# 871 "bnf/m2-3.bnf"
BEGIN
# 871 "bnf/m2-3.bnf"
# 873 "bnf/m2-3.bnf"
   Expression(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 873 "bnf/m2-3.bnf"
   n := 1  ;
# 874 "bnf/m2-3.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 875 "bnf/m2-3.bnf"
      Expression(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 875 "bnf/m2-3.bnf"
      INC(n)  ;
   END (* while *) ;
# 877 "bnf/m2-3.bnf"
   PushT(n)  ;
END ExpList ;


(*
   Expression := 
                 % PushAutoOn  %
                 SimpleExpression [ Relation SimpleExpression 
                                    
                                    % BuildRelOp  %
                                     ]
                 % PopAuto  %
                 

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

# 880 "bnf/m2-3.bnf"
PROCEDURE Expression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 880 "bnf/m2-3.bnf"
BEGIN
# 880 "bnf/m2-3.bnf"
   PushAutoOn  ;
# 881 "bnf/m2-3.bnf"
   SimpleExpression(stopset0 + SetOfStop0{equaltok, hashtok, lessgreatertok, lesstok, lessequaltok, greatertok, greaterequaltok}, stopset1 + SetOfStop1{intok}, stopset2) ;
# 881 "bnf/m2-3.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok})) OR
      (currenttoken=intok)
   THEN
      Relation(stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 881 "bnf/m2-3.bnf"
      SimpleExpression(stopset0, stopset1, stopset2) ;
# 881 "bnf/m2-3.bnf"
      BuildRelOp  ;
   END ;
# 882 "bnf/m2-3.bnf"
   PopAuto  ;
END Expression ;


(*
   SimpleExpression := UnaryOrTerm { AddOperator Term 
                                     % BuildBinaryOp  %
                                      }

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

# 885 "bnf/m2-3.bnf"
PROCEDURE SimpleExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 885 "bnf/m2-3.bnf"
BEGIN
# 885 "bnf/m2-3.bnf"
   UnaryOrTerm(stopset0 + SetOfStop0{plustok, minustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
# 885 "bnf/m2-3.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok})) OR
         (currenttoken=ortok) DO
      AddOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 885 "bnf/m2-3.bnf"
      Term(stopset0 + SetOfStop0{minustok, plustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
# 885 "bnf/m2-3.bnf"
      BuildBinaryOp  ;
   END (* while *) ;
END SimpleExpression ;


(*
   UnaryOrTerm := '+' 
                  % PushT(PlusTok)  %
                  Term 
                  % BuildUnaryOp  %
                   | '-' 
                  % PushT(MinusTok)  %
                  Term 
                  % BuildUnaryOp  %
                   | Term 

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

# 888 "bnf/m2-3.bnf"
PROCEDURE UnaryOrTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 888 "bnf/m2-3.bnf"
BEGIN
# 888 "bnf/m2-3.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 888 "bnf/m2-3.bnf"
      PushT(PlusTok)  ;
# 889 "bnf/m2-3.bnf"
      Term(stopset0, stopset1, stopset2) ;
# 889 "bnf/m2-3.bnf"
      BuildUnaryOp  ;
# 890 "bnf/m2-3.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 890 "bnf/m2-3.bnf"
      PushT(MinusTok)  ;
# 891 "bnf/m2-3.bnf"
      Term(stopset0, stopset1, stopset2) ;
# 891 "bnf/m2-3.bnf"
      BuildUnaryOp  ;
# 892 "bnf/m2-3.bnf"
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
   Term := Factor { MulOperator Factor 
                    % BuildBinaryOp  %
                     }

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok
   
   cannot reachend
*)

# 894 "bnf/m2-3.bnf"
PROCEDURE Term (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 894 "bnf/m2-3.bnf"
BEGIN
# 894 "bnf/m2-3.bnf"
   Factor(stopset0 + SetOfStop0{timestok, dividetok, andtok, ambersandtok}, stopset1 + SetOfStop1{divtok, modtok}, stopset2) ;
# 894 "bnf/m2-3.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {ambersandtok, andtok, dividetok, timestok})) OR
         ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {modtok, divtok})) DO
      MulOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 894 "bnf/m2-3.bnf"
      Factor(stopset0 + SetOfStop0{ambersandtok, andtok, dividetok, timestok}, stopset1 + SetOfStop1{modtok, divtok}, stopset2) ;
# 894 "bnf/m2-3.bnf"
      BuildBinaryOp  ;
   END (* while *) ;
END Term ;


(*
   Factor := 
             % BuildLineNo  %
             Number  | string  | SetOrDesignatorOrFunction  | 
             '(' Expression ')'  | 'NOT' ( Factor 
                                           % BuildNot  %
                                            )

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok
   
   cannot reachend
*)

# 897 "bnf/m2-3.bnf"
PROCEDURE Factor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 897 "bnf/m2-3.bnf"
BEGIN
# 897 "bnf/m2-3.bnf"
   BuildLineNo  ;
# 898 "bnf/m2-3.bnf"
   IF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {realtok, integertok}))
   THEN
      Number(stopset0, stopset1, stopset2) ;
# 898 "bnf/m2-3.bnf"
   ELSIF currenttoken=stringtok
   THEN
      string(stopset0, stopset1, stopset2) ;
# 898 "bnf/m2-3.bnf"
   ELSIF (currenttoken=lcbratok) OR
         (currenttoken=identtok)
   THEN
      SetOrDesignatorOrFunction(stopset0, stopset1, stopset2) ;
# 899 "bnf/m2-3.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 899 "bnf/m2-3.bnf"
      Expression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 899 "bnf/m2-3.bnf"
      Expect(rparatok, stopset0, stopset1, stopset2) ;
# 899 "bnf/m2-3.bnf"
   ELSIF currenttoken=nottok
   THEN
      Expect(nottok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 899 "bnf/m2-3.bnf"
      Factor(stopset0, stopset1, stopset2) ;
# 899 "bnf/m2-3.bnf"
      BuildNot  ;
   ELSE
      ErrorArray('expecting one of: NOT ( { identifier string real number integer number')
   END ;
END Factor ;


(*
   SimpleSet := '{' 
                % BuildEmptySet  %
                [ Element 
                  % BuildLogicalOr  %
                  { ',' Element 
                    % BuildLogicalOr  %
                     } ]'}' 
                % BuildBitsetEnd  %
                

   first  symbols:lcbratok
   
   cannot reachend
*)

# 904 "bnf/m2-3.bnf"
PROCEDURE SimpleSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 904 "bnf/m2-3.bnf"
BEGIN
# 904 "bnf/m2-3.bnf"
   Expect(lcbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 904 "bnf/m2-3.bnf"
   BuildEmptySet  ;
# 905 "bnf/m2-3.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {commatok, lparatok, lcbratok, minustok, plustok})) OR
      (currenttoken=nottok) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {stringtok, realtok, integertok, identtok}))
   THEN
      Element(stopset0 + SetOfStop0{commatok, rcbratok}, stopset1, stopset2) ;
# 905 "bnf/m2-3.bnf"
      BuildLogicalOr  ;
# 906 "bnf/m2-3.bnf"
      WHILE currenttoken=commatok DO
         Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok, commatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 906 "bnf/m2-3.bnf"
         Element(stopset0 + SetOfStop0{rcbratok, commatok}, stopset1, stopset2) ;
# 906 "bnf/m2-3.bnf"
         BuildLogicalOr  ;
      END (* while *) ;
   END ;
# 907 "bnf/m2-3.bnf"
   Expect(rcbratok, stopset0, stopset1, stopset2) ;
# 907 "bnf/m2-3.bnf"
   BuildBitsetEnd  ;
END SimpleSet ;


(*
   SetOrDesignatorOrFunction := Qualident 
                                % CheckWithReference  %
                                
                                % CheckOuterScopeProcedureVariable  %
                                [ SimpleSet  | SimpleDes [ ActualParameters 
                                                           
                                                           % BuildFunctionCall  %
                                                            ] ] | 
                                
                                % BuildBitsetStart  %
                                SimpleSet 

   first  symbols:lcbratok, identtok
   
   cannot reachend
*)

# 910 "bnf/m2-3.bnf"
PROCEDURE SetOrDesignatorOrFunction (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 910 "bnf/m2-3.bnf"
BEGIN
# 910 "bnf/m2-3.bnf"
   IF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok, periodtok, lsbratok, uparrowtok, lparatok}, stopset1, stopset2) ;
# 910 "bnf/m2-3.bnf"
      CheckWithReference  ;
# 911 "bnf/m2-3.bnf"
      CheckOuterScopeProcedureVariable  ;
# 912 "bnf/m2-3.bnf"
      IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, uparrowtok, lsbratok, periodtok, lcbratok}))
      THEN
         (* seen optional [ | ] expression *)
# 912 "bnf/m2-3.bnf"
         IF currenttoken=lcbratok
         THEN
            SimpleSet(stopset0, stopset1, stopset2) ;
# 913 "bnf/m2-3.bnf"
         ELSIF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, uparrowtok, lsbratok, periodtok}))
         THEN
            SimpleDes(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 913 "bnf/m2-3.bnf"
            IF currenttoken=lparatok
            THEN
               ActualParameters(stopset0, stopset1, stopset2) ;
# 913 "bnf/m2-3.bnf"
               BuildFunctionCall  ;
            END ;
         ELSE
            ErrorArray('expecting one of: ( ^ [ . {')
         END ;
         (* end of optional [ | ] expression *)
      END ;
# 916 "bnf/m2-3.bnf"
   ELSE
# 916 "bnf/m2-3.bnf"
      BuildBitsetStart  ;
# 918 "bnf/m2-3.bnf"
      SimpleSet(stopset0, stopset1, stopset2) ;
   END ;
END SetOrDesignatorOrFunction ;


(*
   SimpleDes := { SubDesignator  }

   first  symbols:uparrowtok, lsbratok, periodtok
   
   reachend
*)

# 921 "bnf/m2-3.bnf"
PROCEDURE SimpleDes (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 921 "bnf/m2-3.bnf"
BEGIN
# 921 "bnf/m2-3.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {uparrowtok, lsbratok, periodtok})) DO
      SubDesignator(stopset0 + SetOfStop0{uparrowtok, lsbratok, periodtok}, stopset1, stopset2) ;
   END (* while *) ;
END SimpleDes ;


(*
   ActualParameters := '(' 
                       % BuildSizeCheckStart  %
                       ( ExpList  | 
                         % BuildNulParam  %
                          )')' 

   first  symbols:lparatok
   
   cannot reachend
*)

# 923 "bnf/m2-3.bnf"
PROCEDURE ActualParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 923 "bnf/m2-3.bnf"
BEGIN
# 923 "bnf/m2-3.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 923 "bnf/m2-3.bnf"
   BuildSizeCheckStart  ;
# 924 "bnf/m2-3.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok, minustok, plustok})) OR
      (currenttoken=nottok) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, stringtok, realtok, integertok}))
   THEN
      ExpList(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 924 "bnf/m2-3.bnf"
   ELSE
# 924 "bnf/m2-3.bnf"
      BuildNulParam  ;
   END ;
# 925 "bnf/m2-3.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END ActualParameters ;


(*
   Statement := 
                % PushAutoOn ; DumpStack ;
                  BuildLineNo  %
                [ AssignmentOrProcedureCall  | 
                  IfStatement  | CaseStatement  | 
                  WhileStatement  | RepeatStatement  | 
                  LoopStatement  | ForStatement  | 
                  WithStatement  | AsmStatement  | 
                  'EXIT' 
                  % BuildExit  %
                   | 'RETURN' 
                  % BuildLineNo  %
                  ( Expression  | 
                    % BuildNulExpression (* in epsilon *)  %
                     )
                  % BuildReturn  %
                   ]
                % PopAuto ;  %
                

   first  symbols:returntok, exittok, asmtok, withtok, fortok, looptok, repeattok, whiletok, casetok, iftok, identtok
   
   reachend
*)

# 927 "bnf/m2-3.bnf"
PROCEDURE Statement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 927 "bnf/m2-3.bnf"
BEGIN
# 927 "bnf/m2-3.bnf"
# 928 "bnf/m2-3.bnf"
   PushAutoOn ; DumpStack ;
   BuildLineNo  ;
# 929 "bnf/m2-3.bnf"
   IF ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {returntok, exittok, fortok, looptok, repeattok, casetok, iftok})) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {asmtok, withtok, whiletok, identtok}))
   THEN
      (* seen optional [ | ] expression *)
# 929 "bnf/m2-3.bnf"
      IF currenttoken=identtok
      THEN
         AssignmentOrProcedureCall(stopset0, stopset1, stopset2) ;
# 929 "bnf/m2-3.bnf"
      ELSIF currenttoken=iftok
      THEN
         IfStatement(stopset0, stopset1, stopset2) ;
# 929 "bnf/m2-3.bnf"
      ELSIF currenttoken=casetok
      THEN
         CaseStatement(stopset0, stopset1, stopset2) ;
# 930 "bnf/m2-3.bnf"
      ELSIF currenttoken=whiletok
      THEN
         WhileStatement(stopset0, stopset1, stopset2) ;
# 930 "bnf/m2-3.bnf"
      ELSIF currenttoken=repeattok
      THEN
         RepeatStatement(stopset0, stopset1, stopset2) ;
# 930 "bnf/m2-3.bnf"
      ELSIF currenttoken=looptok
      THEN
         LoopStatement(stopset0, stopset1, stopset2) ;
# 931 "bnf/m2-3.bnf"
      ELSIF currenttoken=fortok
      THEN
         ForStatement(stopset0, stopset1, stopset2) ;
# 931 "bnf/m2-3.bnf"
      ELSIF currenttoken=withtok
      THEN
         WithStatement(stopset0, stopset1, stopset2) ;
# 931 "bnf/m2-3.bnf"
      ELSIF currenttoken=asmtok
      THEN
         AsmStatement(stopset0, stopset1, stopset2) ;
# 932 "bnf/m2-3.bnf"
      ELSIF currenttoken=exittok
      THEN
         Expect(exittok, stopset0, stopset1, stopset2) ;
# 932 "bnf/m2-3.bnf"
         BuildExit  ;
# 933 "bnf/m2-3.bnf"
      ELSIF currenttoken=returntok
      THEN
         Expect(returntok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 933 "bnf/m2-3.bnf"
         BuildLineNo  ;
# 934 "bnf/m2-3.bnf"
         IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok, minustok, plustok})) OR
            (currenttoken=nottok) OR
            ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, stringtok, realtok, integertok}))
         THEN
            Expression(stopset0, stopset1, stopset2) ;
# 934 "bnf/m2-3.bnf"
         ELSE
# 934 "bnf/m2-3.bnf"
            BuildNulExpression (* in epsilon *)  ;
         END ;
# 935 "bnf/m2-3.bnf"
         BuildReturn  ;
      ELSE
         ErrorArray('expecting one of: RETURN EXIT ASM WITH FOR LOOP REPEAT WHILE CASE IF identifier')
      END ;
      (* end of optional [ | ] expression *)
   END ;
# 936 "bnf/m2-3.bnf"
   PopAuto ;  ;
END Statement ;


(*
   AssignmentOrProcedureCall := 
                                % BuildLineNo ; DumpStack  %
                                Designator ( ':=' Expression 
                                             
                                             % BuildAssignment  %
                                              | 
                                             
                                             % BuildLineNo  %
                                             ( ActualParameters  | 
                                               
                                               % BuildNulParam (* in epsilon *)  %
                                                )
                                             % BuildProcedureCall  %
                                              )
                                % DumpStack  %
                                

   first  symbols:identtok
   
   cannot reachend
*)

# 939 "bnf/m2-3.bnf"
PROCEDURE AssignmentOrProcedureCall (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 939 "bnf/m2-3.bnf"
BEGIN
# 939 "bnf/m2-3.bnf"
   BuildLineNo ; DumpStack  ;
# 941 "bnf/m2-3.bnf"
   Designator(stopset0 + SetOfStop0{becomestok, lparatok}, stopset1, stopset2) ;
# 941 "bnf/m2-3.bnf"
   IF currenttoken=becomestok
   THEN
      Expect(becomestok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 941 "bnf/m2-3.bnf"
      Expression(stopset0, stopset1, stopset2) ;
# 941 "bnf/m2-3.bnf"
      BuildAssignment  ;
# 942 "bnf/m2-3.bnf"
   ELSE
# 942 "bnf/m2-3.bnf"
      BuildLineNo  ;
# 943 "bnf/m2-3.bnf"
      IF currenttoken=lparatok
      THEN
         ActualParameters(stopset0, stopset1, stopset2) ;
# 943 "bnf/m2-3.bnf"
      ELSE
# 943 "bnf/m2-3.bnf"
         BuildNulParam (* in epsilon *)  ;
      END ;
# 944 "bnf/m2-3.bnf"
      BuildProcedureCall  ;
   END ;
# 945 "bnf/m2-3.bnf"
   DumpStack  ;
END AssignmentOrProcedureCall ;


(*
   StatementSequence := 
                        % BuildLineNo  %
                        Statement 
                        % BuildLineNo  %
                        { ';' 
                          % BuildLineNo  %
                          Statement  }
                        % BuildLineNo  %
                        

   first  symbols:semicolontok, returntok, exittok, asmtok, withtok, fortok, looptok, repeattok, whiletok, casetok, iftok, identtok
   
   reachend
*)

# 952 "bnf/m2-3.bnf"
PROCEDURE StatementSequence (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 952 "bnf/m2-3.bnf"
BEGIN
# 952 "bnf/m2-3.bnf"
   BuildLineNo  ;
# 953 "bnf/m2-3.bnf"
   Statement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 953 "bnf/m2-3.bnf"
   BuildLineNo  ;
# 954 "bnf/m2-3.bnf"
   WHILE currenttoken=semicolontok DO
      Expect(semicolontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 954 "bnf/m2-3.bnf"
      BuildLineNo  ;
# 955 "bnf/m2-3.bnf"
      Statement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END (* while *) ;
# 955 "bnf/m2-3.bnf"
   BuildLineNo  ;
END StatementSequence ;


(*
   IfStatement := 
                  % BuildLineNo  %
                  'IF' Expression 'THEN' 
                  % BuildLineNo ;
                    BuildThenIf  %
                  StatementSequence { 'ELSIF' 
                                      % BuildLineNo ;
                                        BuildElsif1  %
                                      Expression 'THEN' 
                                      % BuildLineNo ;
                                        BuildThenIf  %
                                      StatementSequence 
                                      % BuildElsif2  %
                                       }[ 'ELSE' 
                                          % BuildLineNo ;
                                            BuildElse  %
                                          StatementSequence  ]
                  'END' 
                  % BuildLineNo ;
                    BuildEndIf  %
                  

   first  symbols:iftok
   
   cannot reachend
*)

# 958 "bnf/m2-3.bnf"
PROCEDURE IfStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 958 "bnf/m2-3.bnf"
BEGIN
# 958 "bnf/m2-3.bnf"
   BuildLineNo  ;
# 959 "bnf/m2-3.bnf"
   Expect(iftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 960 "bnf/m2-3.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{thentok}, stopset2) ;
# 960 "bnf/m2-3.bnf"
   Expect(thentok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, elsiftok, elsetok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 960 "bnf/m2-3.bnf"
# 961 "bnf/m2-3.bnf"
   BuildLineNo ;
   BuildThenIf  ;
# 963 "bnf/m2-3.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{elsiftok, elsetok, endtok}, stopset2) ;
# 963 "bnf/m2-3.bnf"
   WHILE currenttoken=elsiftok DO
      Expect(elsiftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 963 "bnf/m2-3.bnf"
# 964 "bnf/m2-3.bnf"
      BuildLineNo ;
      BuildElsif1  ;
# 965 "bnf/m2-3.bnf"
      Expression(stopset0, stopset1 + SetOfStop1{thentok}, stopset2) ;
# 965 "bnf/m2-3.bnf"
      Expect(thentok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok, elsetok, elsiftok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 965 "bnf/m2-3.bnf"
# 966 "bnf/m2-3.bnf"
      BuildLineNo ;
      BuildThenIf  ;
# 967 "bnf/m2-3.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok, elsetok, elsiftok}, stopset2) ;
# 967 "bnf/m2-3.bnf"
      BuildElsif2  ;
   END (* while *) ;
# 969 "bnf/m2-3.bnf"
   IF currenttoken=elsetok
   THEN
      Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 969 "bnf/m2-3.bnf"
# 970 "bnf/m2-3.bnf"
      BuildLineNo ;
      BuildElse  ;
# 971 "bnf/m2-3.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   END ;
# 971 "bnf/m2-3.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
# 971 "bnf/m2-3.bnf"
# 972 "bnf/m2-3.bnf"
   BuildLineNo ;
   BuildEndIf  ;
END IfStatement ;


(*
   CaseStatement := 'CASE' 
                    % BuildLineNo  %
                    Expression 
                    % BuildCaseStart  %
                    'OF' Case { '|' Case  }[ 'ELSE' 
                                             % BuildLineNo ;
                                               BuildCaseElse  %
                                             StatementSequence  ]
                    'END' 
                    % BuildLineNo ;
                      BuildCaseEnd  %
                    

   first  symbols:casetok
   
   cannot reachend
*)

# 975 "bnf/m2-3.bnf"
PROCEDURE CaseStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 975 "bnf/m2-3.bnf"
BEGIN
# 975 "bnf/m2-3.bnf"
   Expect(casetok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 975 "bnf/m2-3.bnf"
   BuildLineNo  ;
# 976 "bnf/m2-3.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 976 "bnf/m2-3.bnf"
   BuildCaseStart  ;
# 977 "bnf/m2-3.bnf"
   Expect(oftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 977 "bnf/m2-3.bnf"
   Case(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{elsetok, endtok}, stopset2) ;
# 977 "bnf/m2-3.bnf"
   WHILE currenttoken=bartok DO
      Expect(bartok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 977 "bnf/m2-3.bnf"
      Case(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{endtok, elsetok}, stopset2) ;
   END (* while *) ;
# 978 "bnf/m2-3.bnf"
   IF currenttoken=elsetok
   THEN
      Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 978 "bnf/m2-3.bnf"
# 979 "bnf/m2-3.bnf"
      BuildLineNo ;
      BuildCaseElse  ;
# 980 "bnf/m2-3.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   END ;
# 980 "bnf/m2-3.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
# 980 "bnf/m2-3.bnf"
# 981 "bnf/m2-3.bnf"
   BuildLineNo ;
   BuildCaseEnd  ;
END CaseStatement ;


(*
   Case := CaseLabelList ':' 
           % BuildCaseStartStatementSequence  %
           StatementSequence 
           % BuildCaseEndStatementSequence  %
           

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, lcbratok, identtok, minustok, plustok
   
   cannot reachend
*)

# 984 "bnf/m2-3.bnf"
PROCEDURE Case (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 984 "bnf/m2-3.bnf"
BEGIN
# 984 "bnf/m2-3.bnf"
   CaseLabelList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 984 "bnf/m2-3.bnf"
   Expect(colontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 984 "bnf/m2-3.bnf"
   BuildCaseStartStatementSequence  ;
# 985 "bnf/m2-3.bnf"
   StatementSequence(stopset0, stopset1, stopset2) ;
# 985 "bnf/m2-3.bnf"
   BuildCaseEndStatementSequence  ;
END Case ;


(*
   CaseLabelList := CaseLabels { ',' 
                                 % BuildCaseOr  %
                                 CaseLabels  }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, lcbratok, identtok, minustok, plustok
   
   cannot reachend
*)

# 988 "bnf/m2-3.bnf"
PROCEDURE CaseLabelList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 988 "bnf/m2-3.bnf"
BEGIN
# 988 "bnf/m2-3.bnf"
   CaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 988 "bnf/m2-3.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 988 "bnf/m2-3.bnf"
      BuildCaseOr  ;
# 989 "bnf/m2-3.bnf"
      CaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END CaseLabelList ;


(*
   CaseLabels := ConstExpression ( '..' ConstExpression 
                                   % BuildCaseRange ;
                                     BuildCaseList  %
                                    | 
                                   % BuildCaseEquality ;  (* epsilon *)
                                     BuildCaseList  %
                                    )

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, lcbratok, identtok, minustok, plustok
   
   cannot reachend
*)

# 991 "bnf/m2-3.bnf"
PROCEDURE CaseLabels (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 991 "bnf/m2-3.bnf"
BEGIN
# 991 "bnf/m2-3.bnf"
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 991 "bnf/m2-3.bnf"
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 991 "bnf/m2-3.bnf"
      ConstExpression(stopset0, stopset1, stopset2) ;
# 991 "bnf/m2-3.bnf"
# 992 "bnf/m2-3.bnf"
      BuildCaseRange ;
      BuildCaseList  ;
# 993 "bnf/m2-3.bnf"
   ELSE
# 993 "bnf/m2-3.bnf"
# 994 "bnf/m2-3.bnf"
      BuildCaseEquality ;  (* epsilon *)
      BuildCaseList  ;
   END ;
END CaseLabels ;


(*
   WhileStatement := 'WHILE' 
                     % BuildLineNo ;
                       BuildWhile ; DumpStack  %
                     Expression 'DO' 
                     % BuildDoWhile  %
                     StatementSequence 'END' 
                     % DumpStack ; BuildEndWhile  %
                     

   first  symbols:whiletok
   
   cannot reachend
*)

# 997 "bnf/m2-3.bnf"
PROCEDURE WhileStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 997 "bnf/m2-3.bnf"
BEGIN
# 997 "bnf/m2-3.bnf"
   Expect(whiletok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 997 "bnf/m2-3.bnf"
# 998 "bnf/m2-3.bnf"
   BuildLineNo ;
   BuildWhile ; DumpStack  ;
# 999 "bnf/m2-3.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
# 999 "bnf/m2-3.bnf"
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 999 "bnf/m2-3.bnf"
   BuildDoWhile  ;
# 1000 "bnf/m2-3.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 1000 "bnf/m2-3.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
# 1000 "bnf/m2-3.bnf"
   DumpStack ; BuildEndWhile  ;
END WhileStatement ;


(*
   RepeatStatement := 'REPEAT' 
                      % BuildLineNo ;
                        BuildRepeat  %
                      StatementSequence 'UNTIL' 
                      % BuildLineNo  %
                      Expression 
                      % BuildUntil ;
                        BuildLineNo  %
                      

   first  symbols:repeattok
   
   cannot reachend
*)

# 1003 "bnf/m2-3.bnf"
PROCEDURE RepeatStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1003 "bnf/m2-3.bnf"
BEGIN
# 1003 "bnf/m2-3.bnf"
   Expect(repeattok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok, untiltok}) ;
# 1003 "bnf/m2-3.bnf"
# 1004 "bnf/m2-3.bnf"
   BuildLineNo ;
   BuildRepeat  ;
# 1005 "bnf/m2-3.bnf"
   StatementSequence(stopset0, stopset1, stopset2 + SetOfStop2{untiltok}) ;
# 1005 "bnf/m2-3.bnf"
   Expect(untiltok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 1005 "bnf/m2-3.bnf"
   BuildLineNo  ;
# 1006 "bnf/m2-3.bnf"
   Expression(stopset0, stopset1, stopset2) ;
# 1006 "bnf/m2-3.bnf"
# 1007 "bnf/m2-3.bnf"
   BuildUntil ;
   BuildLineNo  ;
END RepeatStatement ;


(*
   ForStatement := 'FOR' 
                   % BuildLineNo  %
                   Ident ':=' Expression 'TO' Expression ( 'BY' 
                                                           ConstExpression  | 
                                                           
                                                           % BuildPseudoBy  (* epsilon *)  %
                                                            )
                   'DO' 
                   % BuildForToByDo  %
                   StatementSequence 'END' 
                   % BuildEndFor  %
                   

   first  symbols:fortok
   
   cannot reachend
*)

# 1010 "bnf/m2-3.bnf"
PROCEDURE ForStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1010 "bnf/m2-3.bnf"
BEGIN
# 1010 "bnf/m2-3.bnf"
   Expect(fortok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1010 "bnf/m2-3.bnf"
   BuildLineNo  ;
# 1011 "bnf/m2-3.bnf"
   Ident(stopset0 + SetOfStop0{becomestok}, stopset1, stopset2) ;
# 1011 "bnf/m2-3.bnf"
   Expect(becomestok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 1011 "bnf/m2-3.bnf"
   Expression(stopset0, stopset1, stopset2 + SetOfStop2{totok}) ;
# 1011 "bnf/m2-3.bnf"
   Expect(totok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 1012 "bnf/m2-3.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{bytok, dotok}, stopset2) ;
# 1012 "bnf/m2-3.bnf"
   IF currenttoken=bytok
   THEN
      Expect(bytok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 1012 "bnf/m2-3.bnf"
      ConstExpression(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
# 1012 "bnf/m2-3.bnf"
   ELSE
# 1012 "bnf/m2-3.bnf"
      BuildPseudoBy  (* epsilon *)  ;
   END ;
# 1013 "bnf/m2-3.bnf"
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 1013 "bnf/m2-3.bnf"
   BuildForToByDo  ;
# 1014 "bnf/m2-3.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 1014 "bnf/m2-3.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
# 1014 "bnf/m2-3.bnf"
   BuildEndFor  ;
END ForStatement ;


(*
   LoopStatement := 'LOOP' 
                    % BuildLineNo ;
                      BuildLoop  %
                    StatementSequence 'END' 
                    % BuildEndLoop  %
                    

   first  symbols:looptok
   
   cannot reachend
*)

# 1017 "bnf/m2-3.bnf"
PROCEDURE LoopStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1017 "bnf/m2-3.bnf"
BEGIN
# 1017 "bnf/m2-3.bnf"
   Expect(looptok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 1017 "bnf/m2-3.bnf"
# 1018 "bnf/m2-3.bnf"
   BuildLineNo ;
   BuildLoop  ;
# 1019 "bnf/m2-3.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 1019 "bnf/m2-3.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
# 1019 "bnf/m2-3.bnf"
   BuildEndLoop  ;
END LoopStatement ;


(*
   WithStatement := 'WITH' 
                    % DumpStack ; BuildLineNo  %
                    Designator 'DO' 
                    % BuildLineNo ;
                      StartBuildWith  %
                    StatementSequence 'END' 
                    % BuildLineNo ;
                      EndBuildWith ; DumpStack  %
                    

   first  symbols:withtok
   
   cannot reachend
*)

# 1022 "bnf/m2-3.bnf"
PROCEDURE WithStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1022 "bnf/m2-3.bnf"
BEGIN
# 1022 "bnf/m2-3.bnf"
   Expect(withtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1022 "bnf/m2-3.bnf"
   DumpStack ; BuildLineNo  ;
# 1023 "bnf/m2-3.bnf"
   Designator(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
# 1023 "bnf/m2-3.bnf"
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 1023 "bnf/m2-3.bnf"
# 1024 "bnf/m2-3.bnf"
   BuildLineNo ;
   StartBuildWith  ;
# 1026 "bnf/m2-3.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 1026 "bnf/m2-3.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
# 1026 "bnf/m2-3.bnf"
# 1027 "bnf/m2-3.bnf"
   BuildLineNo ;
   EndBuildWith ; DumpStack  ;
END WithStatement ;


(*
   ProcedureDeclaration := ProcedureHeading ';' ProcedureBlock 
                           
                           % BuildProcedureEnd ;
                             PushAutoOn  %
                           Ident 
                           % EndBuildProcedure ;
                             PopAuto  %
                           

   first  symbols:proceduretok
   
   cannot reachend
*)

# 1030 "bnf/m2-3.bnf"
PROCEDURE ProcedureDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1030 "bnf/m2-3.bnf"
BEGIN
# 1030 "bnf/m2-3.bnf"
   ProcedureHeading(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1030 "bnf/m2-3.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{consttok, proceduretok, moduletok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 1030 "bnf/m2-3.bnf"
   ProcedureBlock(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1030 "bnf/m2-3.bnf"
# 1031 "bnf/m2-3.bnf"
   BuildProcedureEnd ;
   PushAutoOn  ;
# 1033 "bnf/m2-3.bnf"
   Ident(stopset0, stopset1, stopset2) ;
# 1033 "bnf/m2-3.bnf"
# 1034 "bnf/m2-3.bnf"
   EndBuildProcedure ;
   PopAuto  ;
END ProcedureDeclaration ;


(*
   ProcedureHeading := 'PROCEDURE' 
                       % PushAutoOn  %
                       ( Ident 
                         % StartBuildProcedure ;
                           PushAutoOff  %
                         [ FormalParameters  ]
                         % BuildProcedureHeading ;
                           PopAuto  %
                          )
                       % PopAuto  %
                       

   first  symbols:proceduretok
   
   cannot reachend
*)

# 1037 "bnf/m2-3.bnf"
PROCEDURE ProcedureHeading (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1037 "bnf/m2-3.bnf"
BEGIN
# 1037 "bnf/m2-3.bnf"
   Expect(proceduretok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1037 "bnf/m2-3.bnf"
   PushAutoOn  ;
# 1038 "bnf/m2-3.bnf"
# 1039 "bnf/m2-3.bnf"
   Ident(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 1039 "bnf/m2-3.bnf"
# 1040 "bnf/m2-3.bnf"
   StartBuildProcedure ;
   PushAutoOff  ;
# 1041 "bnf/m2-3.bnf"
   IF currenttoken=lparatok
   THEN
      FormalParameters(stopset0, stopset1, stopset2) ;
   END ;
# 1042 "bnf/m2-3.bnf"
# 1043 "bnf/m2-3.bnf"
   BuildProcedureHeading ;
   PopAuto  ;
# 1044 "bnf/m2-3.bnf"
   PopAuto  ;
END ProcedureHeading ;


(*
   ProcedureBlock := { Declaration  }'BEGIN' 
                     % BuildProcedureStart ;
                       BuildLineNo  %
                     StatementSequence 'END' 
                     % BuildLineNo  %
                     

   first  symbols:begintok, moduletok, proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

# 1050 "bnf/m2-3.bnf"
PROCEDURE ProcedureBlock (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1050 "bnf/m2-3.bnf"
BEGIN
# 1050 "bnf/m2-3.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Declaration(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 1050 "bnf/m2-3.bnf"
   Expect(begintok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 1050 "bnf/m2-3.bnf"
# 1051 "bnf/m2-3.bnf"
   BuildProcedureStart ;
   BuildLineNo  ;
# 1052 "bnf/m2-3.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 1052 "bnf/m2-3.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
# 1052 "bnf/m2-3.bnf"
   BuildLineNo  ;
END ProcedureBlock ;


(*
   Block := { Declaration  }( 'BEGIN' 
                              % StartBuildModFile ;
                                StartBuildInit ;
                                BuildLineNo  %
                              StatementSequence 'END' 
                              % BuildLineNo ;
                                EndBuildInit  %
                               | 
                              'END' 
                              % StartBuildModFile ;
                                StartBuildInit ;
                                BuildLineNo ;
                                EndBuildInit  %
                               )

   first  symbols:endtok, begintok, moduletok, proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

# 1055 "bnf/m2-3.bnf"
PROCEDURE Block (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1055 "bnf/m2-3.bnf"
BEGIN
# 1055 "bnf/m2-3.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Declaration(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 1055 "bnf/m2-3.bnf"
   IF currenttoken=begintok
   THEN
      Expect(begintok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 1055 "bnf/m2-3.bnf"
# 1057 "bnf/m2-3.bnf"
      StartBuildModFile ;
      StartBuildInit ;
      BuildLineNo  ;
# 1058 "bnf/m2-3.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 1058 "bnf/m2-3.bnf"
      Expect(endtok, stopset0, stopset1, stopset2) ;
# 1058 "bnf/m2-3.bnf"
# 1059 "bnf/m2-3.bnf"
      BuildLineNo ;
      EndBuildInit  ;
# 1060 "bnf/m2-3.bnf"
   ELSIF currenttoken=endtok
   THEN
      Expect(endtok, stopset0, stopset1, stopset2) ;
# 1060 "bnf/m2-3.bnf"
# 1063 "bnf/m2-3.bnf"
      StartBuildModFile ;
      StartBuildInit ;
      BuildLineNo ;
      EndBuildInit  ;
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

# 1067 "bnf/m2-3.bnf"
PROCEDURE Declaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1067 "bnf/m2-3.bnf"
BEGIN
# 1067 "bnf/m2-3.bnf"
   IF currenttoken=consttok
   THEN
      Expect(consttok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1067 "bnf/m2-3.bnf"
      WHILE currenttoken=identtok DO
         ConstantDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1067 "bnf/m2-3.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 1068 "bnf/m2-3.bnf"
   ELSIF currenttoken=typetok
   THEN
      Expect(typetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1068 "bnf/m2-3.bnf"
      WHILE currenttoken=identtok DO
         TypeDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1068 "bnf/m2-3.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 1069 "bnf/m2-3.bnf"
   ELSIF currenttoken=vartok
   THEN
      Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1069 "bnf/m2-3.bnf"
      WHILE currenttoken=identtok DO
         VariableDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1069 "bnf/m2-3.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 1070 "bnf/m2-3.bnf"
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1070 "bnf/m2-3.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
# 1071 "bnf/m2-3.bnf"
   ELSIF currenttoken=moduletok
   THEN
      ModuleDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1071 "bnf/m2-3.bnf"
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

# 1073 "bnf/m2-3.bnf"
PROCEDURE FormalParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1073 "bnf/m2-3.bnf"
BEGIN
# 1073 "bnf/m2-3.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{rparatok}, stopset1, stopset2 + SetOfStop2{identtok, vartok}) ;
# 1074 "bnf/m2-3.bnf"
   IF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, identtok}))
   THEN
      FPSection(stopset0 + SetOfStop0{semicolontok, rparatok}, stopset1, stopset2) ;
# 1074 "bnf/m2-3.bnf"
      WHILE currenttoken=semicolontok DO
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok, vartok}) ;
# 1074 "bnf/m2-3.bnf"
         FPSection(stopset0 + SetOfStop0{rparatok, semicolontok}, stopset1, stopset2) ;
      END (* while *) ;
   END ;
# 1074 "bnf/m2-3.bnf"
   Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 1075 "bnf/m2-3.bnf"
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1076 "bnf/m2-3.bnf"
      Qualident(stopset0, stopset1, stopset2) ;
   END ;
END FormalParameters ;


(*
   FPSection := NonVarFPSection  | VarFPSection 

   first  symbols:vartok, identtok
   
   cannot reachend
*)

# 1078 "bnf/m2-3.bnf"
PROCEDURE FPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1078 "bnf/m2-3.bnf"
BEGIN
# 1078 "bnf/m2-3.bnf"
   IF currenttoken=identtok
   THEN
      NonVarFPSection(stopset0, stopset1, stopset2) ;
# 1078 "bnf/m2-3.bnf"
   ELSIF currenttoken=vartok
   THEN
      VarFPSection(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: VAR identifier')
   END ;
END FPSection ;


(*
   VarFPSection := 'VAR' IdentList ':' FormalType 

   first  symbols:vartok
   
   cannot reachend
*)

# 1080 "bnf/m2-3.bnf"
PROCEDURE VarFPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1080 "bnf/m2-3.bnf"
BEGIN
# 1080 "bnf/m2-3.bnf"
   Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1080 "bnf/m2-3.bnf"
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 1080 "bnf/m2-3.bnf"
   Expect(colontok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1080 "bnf/m2-3.bnf"
   FormalType(stopset0, stopset1, stopset2) ;
END VarFPSection ;


(*
   NonVarFPSection := IdentList ':' FormalType 

   first  symbols:identtok
   
   cannot reachend
*)

# 1082 "bnf/m2-3.bnf"
PROCEDURE NonVarFPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1082 "bnf/m2-3.bnf"
BEGIN
# 1082 "bnf/m2-3.bnf"
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 1082 "bnf/m2-3.bnf"
   Expect(colontok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1082 "bnf/m2-3.bnf"
   FormalType(stopset0, stopset1, stopset2) ;
END NonVarFPSection ;


(*
   FormalType := [ 'ARRAY' 'OF'  ]Qualident 

   first  symbols:identtok, arraytok
   
   cannot reachend
*)

# 1084 "bnf/m2-3.bnf"
PROCEDURE FormalType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1084 "bnf/m2-3.bnf"
BEGIN
# 1084 "bnf/m2-3.bnf"
   IF currenttoken=arraytok
   THEN
      Expect(arraytok, stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 1084 "bnf/m2-3.bnf"
      Expect(oftok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   END ;
# 1084 "bnf/m2-3.bnf"
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

# 1086 "bnf/m2-3.bnf"
PROCEDURE ModuleDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1086 "bnf/m2-3.bnf"
BEGIN
# 1086 "bnf/m2-3.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1086 "bnf/m2-3.bnf"
   PushAutoOn  ;
# 1087 "bnf/m2-3.bnf"
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
# 1087 "bnf/m2-3.bnf"
# 1088 "bnf/m2-3.bnf"
   StartBuildInnerModule ;
   PushAutoOff  ;
# 1089 "bnf/m2-3.bnf"
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
# 1089 "bnf/m2-3.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, exporttok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 1090 "bnf/m2-3.bnf"
# 1091 "bnf/m2-3.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, exporttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 1091 "bnf/m2-3.bnf"
# 1092 "bnf/m2-3.bnf"
   IF currenttoken=exporttok
   THEN
      Export(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END ;
# 1093 "bnf/m2-3.bnf"
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1093 "bnf/m2-3.bnf"
   PushAutoOn  ;
# 1094 "bnf/m2-3.bnf"
   Ident(stopset0, stopset1, stopset2) ;
# 1094 "bnf/m2-3.bnf"
   EndBuildInnerModule  ;
# 1095 "bnf/m2-3.bnf"
   PopAuto ; PopAuto ; PopAuto  ;
END ModuleDeclaration ;


(*
   Priority := '[' ConstExpression ']' 

   first  symbols:lsbratok
   
   cannot reachend
*)

# 1098 "bnf/m2-3.bnf"
PROCEDURE Priority (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1098 "bnf/m2-3.bnf"
BEGIN
# 1098 "bnf/m2-3.bnf"
   Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 1098 "bnf/m2-3.bnf"
   ConstExpression(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 1098 "bnf/m2-3.bnf"
   Expect(rsbratok, stopset0, stopset1, stopset2) ;
END Priority ;


(*
   Export := 'EXPORT' ( 'QUALIFIED' IdentList  | 
                        'UNQUALIFIED' IdentList  | 
                        IdentList  )';' 

   first  symbols:exporttok
   
   cannot reachend
*)

# 1100 "bnf/m2-3.bnf"
PROCEDURE Export (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1100 "bnf/m2-3.bnf"
BEGIN
# 1100 "bnf/m2-3.bnf"
   Expect(exporttok, stopset0, stopset1 + SetOfStop1{qualifiedtok, unqualifiedtok}, stopset2 + SetOfStop2{identtok}) ;
# 1100 "bnf/m2-3.bnf"
   IF currenttoken=qualifiedtok
   THEN
      Expect(qualifiedtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1101 "bnf/m2-3.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1102 "bnf/m2-3.bnf"
   ELSIF currenttoken=unqualifiedtok
   THEN
      Expect(unqualifiedtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1103 "bnf/m2-3.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1104 "bnf/m2-3.bnf"
   ELSIF currenttoken=identtok
   THEN
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: identifier UNQUALIFIED QUALIFIED')
   END ;
# 1104 "bnf/m2-3.bnf"
   Expect(semicolontok, stopset0, stopset1, stopset2) ;
END Export ;


(*
   Import := 'FROM' Ident 'IMPORT' IdentList ';'  | 
             'IMPORT' IdentList ';' 

   first  symbols:importtok, fromtok
   
   cannot reachend
*)

# 1106 "bnf/m2-3.bnf"
PROCEDURE Import (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1106 "bnf/m2-3.bnf"
BEGIN
# 1106 "bnf/m2-3.bnf"
   IF currenttoken=fromtok
   THEN
      Expect(fromtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1106 "bnf/m2-3.bnf"
      Ident(stopset0, stopset1 + SetOfStop1{importtok}, stopset2) ;
# 1106 "bnf/m2-3.bnf"
      Expect(importtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1106 "bnf/m2-3.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1106 "bnf/m2-3.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
# 1107 "bnf/m2-3.bnf"
   ELSIF currenttoken=importtok
   THEN
      Expect(importtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1108 "bnf/m2-3.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1108 "bnf/m2-3.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: IMPORT FROM')
   END ;
END Import ;


(*
   DefinitionModule := 'DEFINITION' 'MODULE' 
                       % PushAutoOn  %
                       Ident 
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

# 1110 "bnf/m2-3.bnf"
PROCEDURE DefinitionModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1110 "bnf/m2-3.bnf"
BEGIN
# 1110 "bnf/m2-3.bnf"
   Expect(definitiontok, stopset0, stopset1 + SetOfStop1{moduletok}, stopset2) ;
# 1110 "bnf/m2-3.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1110 "bnf/m2-3.bnf"
   PushAutoOn  ;
# 1111 "bnf/m2-3.bnf"
   Ident(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1111 "bnf/m2-3.bnf"
# 1113 "bnf/m2-3.bnf"
   StartBuildDefFile ;
   P3StartBuildDefModule ;
   PushAutoOff  ;
# 1114 "bnf/m2-3.bnf"
   Expect(semicolontok, stopset0, stopset1 + SetOfStop1{fromtok, importtok, exporttok, consttok, proceduretok, endtok}, stopset2 + SetOfStop2{typetok, vartok}) ;
# 1115 "bnf/m2-3.bnf"
# 1116 "bnf/m2-3.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok, exporttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 1116 "bnf/m2-3.bnf"
# 1117 "bnf/m2-3.bnf"
   IF currenttoken=exporttok
   THEN
      Export(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END ;
# 1118 "bnf/m2-3.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Definition(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 1119 "bnf/m2-3.bnf"
   Expect(endtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1119 "bnf/m2-3.bnf"
   PushAutoOn  ;
# 1120 "bnf/m2-3.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 1120 "bnf/m2-3.bnf"
# 1121 "bnf/m2-3.bnf"
   EndBuildFile ;
   P3EndBuildDefModule  ;
# 1122 "bnf/m2-3.bnf"
   Expect(periodtok, stopset0, stopset1, stopset2) ;
# 1122 "bnf/m2-3.bnf"
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

# 1125 "bnf/m2-3.bnf"
PROCEDURE Definition (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1125 "bnf/m2-3.bnf"
BEGIN
# 1125 "bnf/m2-3.bnf"
   IF currenttoken=consttok
   THEN
      Expect(consttok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1125 "bnf/m2-3.bnf"
      WHILE currenttoken=identtok DO
         ConstantDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1125 "bnf/m2-3.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 1126 "bnf/m2-3.bnf"
   ELSIF currenttoken=typetok
   THEN
      Expect(typetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1127 "bnf/m2-3.bnf"
      WHILE currenttoken=identtok DO
         Ident(stopset0 + SetOfStop0{semicolontok, equaltok}, stopset1, stopset2) ;
# 1127 "bnf/m2-3.bnf"
         IF currenttoken=semicolontok
         THEN
            Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1128 "bnf/m2-3.bnf"
         ELSIF currenttoken=equaltok
         THEN
            Expect(equaltok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 1128 "bnf/m2-3.bnf"
            Type(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1128 "bnf/m2-3.bnf"
            Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
         ELSE
            ErrorArray('expecting one of: = ;')
         END ;
      END (* while *) ;
# 1131 "bnf/m2-3.bnf"
   ELSIF currenttoken=vartok
   THEN
      Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 1131 "bnf/m2-3.bnf"
      WHILE currenttoken=identtok DO
         VariableDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1131 "bnf/m2-3.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 1132 "bnf/m2-3.bnf"
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureHeading(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 1132 "bnf/m2-3.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: PROCEDURE VAR TYPE CONST')
   END ;
END Definition ;


(*
   AsmStatement := 
                   % VAR CurrentAsm: CARDINAL ;  %
                   'ASM' 
                   % IF Pass3
                     THEN
                        PushAutoOn ;
                        PushT(MakeGnuAsm())
                     END
                      %
                   [ 'VOLATILE' 
                     % IF Pass3
                       THEN
                          PopT(CurrentAsm) ;
                          PutGnuAsmVolatile(CurrentAsm) ;
                          PushT(CurrentAsm)
                       END
                        %
                      ]'(' AsmOperands 
                   % IF Pass3
                     THEN
                        BuildInline ;
                        PopAuto
                     END
                      %
                   ')' 

   first  symbols:asmtok
   
   cannot reachend
*)

# 1134 "bnf/m2-3.bnf"
PROCEDURE AsmStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
 CurrentAsm: CARDINAL ; 
# 1134 "bnf/m2-3.bnf"
BEGIN
# 1134 "bnf/m2-3.bnf"
# 1135 "bnf/m2-3.bnf"
   Expect(asmtok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2 + SetOfStop2{volatiletok}) ;
# 1135 "bnf/m2-3.bnf"
# 1140 "bnf/m2-3.bnf"
   IF Pass3
   THEN
      PushAutoOn ;
      PushT(MakeGnuAsm())
   END
    ;
# 1141 "bnf/m2-3.bnf"
   IF currenttoken=volatiletok
   THEN
      Expect(volatiletok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 1141 "bnf/m2-3.bnf"
# 1147 "bnf/m2-3.bnf"
      IF Pass3
      THEN
         PopT(CurrentAsm) ;
         PutGnuAsmVolatile(CurrentAsm) ;
         PushT(CurrentAsm)
      END
       ;
   END ;
# 1148 "bnf/m2-3.bnf"
   Expect(lparatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 1148 "bnf/m2-3.bnf"
   AsmOperands(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 1148 "bnf/m2-3.bnf"
# 1153 "bnf/m2-3.bnf"
   IF Pass3
   THEN
      BuildInline ;
      PopAuto
   END
    ;
# 1154 "bnf/m2-3.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END AsmStatement ;


(*
   AsmOperands := 
                  % VAR CurrentAsm, outputs, inputs, trash: CARDINAL ;
                        str: CARDINAL ;
                     %
                  string 
                  % IF Pass3
                    THEN
                       PopT(str) ;
                       PopT(CurrentAsm) ;
                       (* adds the name/instruction for this asm *)
                       PutGnuAsm(CurrentAsm, str) ;
                       PushT(CurrentAsm)
                    END
                     %
                  [ ':' AsmList 
                    % IF Pass3
                      THEN
                         PopT(outputs) ;
                         PopT(CurrentAsm) ;
                         PutGnuAsmOutput(CurrentAsm, outputs) ;
                         PushT(CurrentAsm)
                      END
                       %
                    [ ':' AsmList 
                      % IF Pass3
                        THEN
                           PopT(inputs) ;
                           PopT(CurrentAsm) ;
                           PutGnuAsmInput(CurrentAsm, inputs) ;
                           PushT(CurrentAsm)
                        END
                         %
                      [ ':' TrashList 
                        % IF Pass3
                          THEN
                             PopT(trash) ;
                             PopT(CurrentAsm) ;
                             PutGnuAsmTrash(CurrentAsm, trash) ;
                             PushT(CurrentAsm)
                          END
                           %
                         ] ] ]

   first  symbols:stringtok
   
   cannot reachend
*)

# 1156 "bnf/m2-3.bnf"
PROCEDURE AsmOperands (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
 CurrentAsm, outputs, inputs, trash: CARDINAL ;
                                                                                 str: CARDINAL ;
                                                                           
# 1156 "bnf/m2-3.bnf"
BEGIN
# 1156 "bnf/m2-3.bnf"
# 1159 "bnf/m2-3.bnf"
   string(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 1159 "bnf/m2-3.bnf"
# 1167 "bnf/m2-3.bnf"
   IF Pass3
   THEN
      PopT(str) ;
      PopT(CurrentAsm) ;
      (* adds the name/instruction for this asm *)
      PutGnuAsm(CurrentAsm, str) ;
      PushT(CurrentAsm)
   END
    ;
# 1168 "bnf/m2-3.bnf"
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0 + SetOfStop0{commatok, colontok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 1168 "bnf/m2-3.bnf"
      AsmList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 1168 "bnf/m2-3.bnf"
# 1175 "bnf/m2-3.bnf"
      IF Pass3
      THEN
         PopT(outputs) ;
         PopT(CurrentAsm) ;
         PutGnuAsmOutput(CurrentAsm, outputs) ;
         PushT(CurrentAsm)
      END
       ;
# 1176 "bnf/m2-3.bnf"
      IF currenttoken=colontok
      THEN
         Expect(colontok, stopset0 + SetOfStop0{commatok, colontok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 1176 "bnf/m2-3.bnf"
         AsmList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 1176 "bnf/m2-3.bnf"
# 1183 "bnf/m2-3.bnf"
         IF Pass3
         THEN
            PopT(inputs) ;
            PopT(CurrentAsm) ;
            PutGnuAsmInput(CurrentAsm, inputs) ;
            PushT(CurrentAsm)
         END
          ;
# 1184 "bnf/m2-3.bnf"
         IF currenttoken=colontok
         THEN
            Expect(colontok, stopset0 + SetOfStop0{commatok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 1184 "bnf/m2-3.bnf"
            TrashList(stopset0, stopset1, stopset2) ;
# 1184 "bnf/m2-3.bnf"
# 1191 "bnf/m2-3.bnf"
            IF Pass3
            THEN
               PopT(trash) ;
               PopT(CurrentAsm) ;
               PutGnuAsmTrash(CurrentAsm, trash) ;
               PushT(CurrentAsm)
            END
             ;
         END ;
      END ;
   END ;
END AsmOperands ;


(*
   AsmList := 
              % IF Pass3
                THEN
                   PushT(NulSym)
                END
                 %
              [ AsmElement  ]{ ',' AsmElement  }

   first  symbols:commatok, stringtok
   
   reachend
*)

# 1195 "bnf/m2-3.bnf"
PROCEDURE AsmList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 1195 "bnf/m2-3.bnf"
BEGIN
# 1195 "bnf/m2-3.bnf"
# 1199 "bnf/m2-3.bnf"
   IF Pass3
   THEN
      PushT(NulSym)
   END
    ;
# 1200 "bnf/m2-3.bnf"
   IF currenttoken=stringtok
   THEN
      AsmElement(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END ;
# 1200 "bnf/m2-3.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 1200 "bnf/m2-3.bnf"
      AsmElement(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END AsmList ;


(*
   AsmElement := 
                 % VAR str,
                       expr, CurrentInterface: CARDINAL ;  %
                 string '(' Expression 
                 % IF Pass3
                   THEN
                      PopT(expr) ;
                      PopT(str) ;
                      PopT(CurrentInterface) ;
                      IF CurrentInterface=NulSym
                      THEN
                         CurrentInterface := MakeRegInterface()
                      END ;
                      PutRegInterface(CurrentInterface, str, expr) ;
                      PushT(CurrentInterface)
                   END
                    %
                 ')' 

   first  symbols:stringtok
   
   cannot reachend
*)

# 1202 "bnf/m2-3.bnf"
PROCEDURE AsmElement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
 str,
                                                                                 expr, CurrentInterface: CARDINAL ; 
# 1202 "bnf/m2-3.bnf"
BEGIN
# 1202 "bnf/m2-3.bnf"
# 1204 "bnf/m2-3.bnf"
   string(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 1204 "bnf/m2-3.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 1204 "bnf/m2-3.bnf"
   Expression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 1204 "bnf/m2-3.bnf"
# 1216 "bnf/m2-3.bnf"
   IF Pass3
   THEN
      PopT(expr) ;
      PopT(str) ;
      PopT(CurrentInterface) ;
      IF CurrentInterface=NulSym
      THEN
         CurrentInterface := MakeRegInterface()
      END ;
      PutRegInterface(CurrentInterface, str, expr) ;
      PushT(CurrentInterface)
   END
    ;
# 1217 "bnf/m2-3.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END AsmElement ;


(*
   TrashList := 
                % VAR CurrentInterface: CARDINAL ;
                      str: CARDINAL ;  %
                [ string 
                  % IF Pass3
                    THEN
                       PopT(str) ;
                       CurrentInterface := MakeRegInterface() ;
                       PutRegInterface(CurrentInterface, str, NulSym) ;
                       PushT(CurrentInterface)
                    END
                     %
                   ]{ ',' string 
                      % IF Pass3
                        THEN
                           PopT(str) ;
                           PopT(CurrentInterface) ;
                           PutRegInterface(CurrentInterface, str, NulSym) ;
                           PushT(CurrentInterface)
                        END
                         %
                       }

   first  symbols:commatok, stringtok
   
   reachend
*)

# 1220 "bnf/m2-3.bnf"
PROCEDURE TrashList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
 CurrentInterface: CARDINAL ;
                                                                                 str: CARDINAL ; 
# 1220 "bnf/m2-3.bnf"
BEGIN
# 1220 "bnf/m2-3.bnf"
# 1222 "bnf/m2-3.bnf"
   IF currenttoken=stringtok
   THEN
      string(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 1222 "bnf/m2-3.bnf"
# 1229 "bnf/m2-3.bnf"
      IF Pass3
      THEN
         PopT(str) ;
         CurrentInterface := MakeRegInterface() ;
         PutRegInterface(CurrentInterface, str, NulSym) ;
         PushT(CurrentInterface)
      END
       ;
   END ;
# 1230 "bnf/m2-3.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 1230 "bnf/m2-3.bnf"
      string(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 1230 "bnf/m2-3.bnf"
# 1237 "bnf/m2-3.bnf"
      IF Pass3
      THEN
         PopT(str) ;
         PopT(CurrentInterface) ;
         PutRegInterface(CurrentInterface, str, NulSym) ;
         PushT(CurrentInterface)
      END
       ;
   END (* while *) ;
END TrashList ;



END P3Build.
