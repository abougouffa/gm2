(* it is advisable not to edit this file as it was automatically generated from the grammer file bnf/m2-2.bnf *)

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
IMPLEMENTATION MODULE P2Build ;

FROM M2LexBuf IMPORT currentstring, currenttoken, GetToken, InsertToken, InsertTokenAndRewind, GetTokenNo ;
FROM M2Error IMPORT ErrorStringAt ;
FROM NameKey IMPORT NulName, Name ;
FROM M2Reserved IMPORT tokToTok, toktype, NulTok, ImportTok, ExportTok, QualifiedTok, UnQualifiedTok ;
FROM Strings IMPORT String, InitString, KillString, Mark, ConCat, ConCatChar ;
FROM M2Printf IMPORT printf0 ;
FROM M2Debug IMPORT Assert ;

FROM M2Quads IMPORT PushT, PopT, PushTF, PopTF, PopNothing, OperandT,
                    StartBuildInit,
                    EndBuildInit,
                    BuildProcedureStart,
                    BuildProcedureEnd,
                    BuildAssignment,
                    BuildInline,
                    PushT, PushTF, IsAutoPushOn, PushAutoOff, PushAutoOn, PopAuto ;

FROM P2SymBuild IMPORT P2StartBuildProgramModule,
                       P2EndBuildProgramModule,
                       P2StartBuildDefModule,
                       P2EndBuildDefModule,
                       P2StartBuildImplementationModule,
                       P2EndBuildImplementationModule,
                       StartBuildInnerModule,
                       EndBuildInnerModule,

                       BuildImportOuterModule,
                       BuildImportInnerModule,
                       BuildExportOuterModule,
                       BuildExportInnerModule,

                       BuildString, BuildNumber,
                       BuildConst,
                       BuildVariable,
                       BuildTypeEnd,
                       BuildNulName,
                       BuildConstTypeFromAssignment,
                       BuildConstSetType,
                       BuildType,
                       BuildEnumeration,

                       StartBuildFormalParameters,
                       EndBuildFormalParameters,
                       BuildFPSection,
                       BuildProcedureHeading,
                       StartBuildProcedure,
                       EndBuildProcedure,
                       BuildFunction,
                       BuildPriority,

                       BuildPointerType,
                       BuildRecord, BuildFieldRecord,
                       StartBuildVarient, EndBuildVarient,
                       BuildVarientSelector,
                       StartBuildVarientFieldRecord,
                       EndBuildVarientFieldRecord,
                       BuildEnumeration,
                       BuildType,
                       BuildNulName,
                       StartBuildArray,
                       EndBuildArray,
                       BuildFieldArray,
                       BuildSubrange,
                       BuildSetType,
                       BuildFormalType, BuildFunction, BuildProcedureType,

                       StartBuildingConstDeclaration,
                       EndBuildingConstDeclaration ;

FROM M2Reserved IMPORT ArrayTok, VarTok ;

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
                        RequestSym ;

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
PROCEDURE CaseLabelList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
PROCEDURE CaseLabels (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
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
               % PushAutoOn  %
               ( DefinitionModule  | ImplementationOrProgramModule  )
               
               % PopAuto  %
               

   first  symbols:moduletok, implementationtok, definitiontok
   
   cannot reachend
*)

# 513 "bnf/m2-2.bnf"
PROCEDURE FileUnit (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 513 "bnf/m2-2.bnf"
BEGIN
# 513 "bnf/m2-2.bnf"
   PushAutoOn  ;
# 514 "bnf/m2-2.bnf"
   IF currenttoken=definitiontok
   THEN
      DefinitionModule(stopset0, stopset1, stopset2) ;
# 515 "bnf/m2-2.bnf"
   ELSIF ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, implementationtok}))
   THEN
      ImplementationOrProgramModule(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: MODULE IMPLEMENTATION DEFINITION')
   END ;
# 515 "bnf/m2-2.bnf"
   PopAuto  ;
END FileUnit ;


(*
   ProgramModule := 'MODULE' Ident 
                    % P2StartBuildProgramModule ;  %
                    [ Priority 
                      % BuildPriority ;  %
                       ]';' { Import 
                              % BuildImportOuterModule ;  %
                               }Block Ident 
                    % P2EndBuildProgramModule ;  %
                    '.' 

   first  symbols:moduletok
   
   cannot reachend
*)

# 518 "bnf/m2-2.bnf"
PROCEDURE ProgramModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 518 "bnf/m2-2.bnf"
BEGIN
# 518 "bnf/m2-2.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 519 "bnf/m2-2.bnf"
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
# 519 "bnf/m2-2.bnf"
   P2StartBuildProgramModule ;  ;
# 524 "bnf/m2-2.bnf"
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 524 "bnf/m2-2.bnf"
      BuildPriority ;  ;
   END ;
# 526 "bnf/m2-2.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 528 "bnf/m2-2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
# 528 "bnf/m2-2.bnf"
      BuildImportOuterModule ;  ;
   END (* while *) ;
# 531 "bnf/m2-2.bnf"
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 533 "bnf/m2-2.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 533 "bnf/m2-2.bnf"
   P2EndBuildProgramModule ;  ;
# 534 "bnf/m2-2.bnf"
   Expect(periodtok, stopset0, stopset1, stopset2) ;
END ProgramModule ;


(*
   ImplementationModule := 'IMPLEMENTATION' 'MODULE' Ident 
                           % P2StartBuildImplementationModule ;  %
                           [ Priority 
                             % BuildPriority ;  %
                              ]';' { Import 
                                     % BuildImportOuterModule ;  %
                                      }Block Ident 
                           % P2EndBuildImplementationModule ;  %
                           '.' 

   first  symbols:implementationtok
   
   cannot reachend
*)

# 537 "bnf/m2-2.bnf"
PROCEDURE ImplementationModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 537 "bnf/m2-2.bnf"
BEGIN
# 537 "bnf/m2-2.bnf"
   Expect(implementationtok, stopset0, stopset1 + SetOfStop1{moduletok}, stopset2) ;
# 537 "bnf/m2-2.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 538 "bnf/m2-2.bnf"
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
# 538 "bnf/m2-2.bnf"
   P2StartBuildImplementationModule ;  ;
# 540 "bnf/m2-2.bnf"
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 540 "bnf/m2-2.bnf"
      BuildPriority ;  ;
   END ;
# 541 "bnf/m2-2.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 542 "bnf/m2-2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
# 542 "bnf/m2-2.bnf"
      BuildImportOuterModule ;  ;
   END (* while *) ;
# 544 "bnf/m2-2.bnf"
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 546 "bnf/m2-2.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 546 "bnf/m2-2.bnf"
   P2EndBuildImplementationModule ;  ;
# 547 "bnf/m2-2.bnf"
   Expect(periodtok, stopset0, stopset1, stopset2) ;
END ImplementationModule ;


(*
   ImplementationOrProgramModule := ImplementationModule  | 
                                    ProgramModule 

   first  symbols:moduletok, implementationtok
   
   cannot reachend
*)

# 549 "bnf/m2-2.bnf"
PROCEDURE ImplementationOrProgramModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 549 "bnf/m2-2.bnf"
BEGIN
# 549 "bnf/m2-2.bnf"
   IF currenttoken=implementationtok
   THEN
      ImplementationModule(stopset0, stopset1, stopset2) ;
# 549 "bnf/m2-2.bnf"
   ELSIF currenttoken=moduletok
   THEN
      ProgramModule(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: MODULE IMPLEMENTATION')
   END ;
END ImplementationOrProgramModule ;


(*
   Number := Integer  | Real 

   first  symbols:realtok, integertok
   
   cannot reachend
*)

# 551 "bnf/m2-2.bnf"
PROCEDURE Number (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 551 "bnf/m2-2.bnf"
BEGIN
# 551 "bnf/m2-2.bnf"
   IF currenttoken=integertok
   THEN
      Integer(stopset0, stopset1, stopset2) ;
# 551 "bnf/m2-2.bnf"
   ELSIF currenttoken=realtok
   THEN
      Real(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: real number integer number')
   END ;
END Number ;


(*
   Qualident := 
                % VAR name: Name ;
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

# 553 "bnf/m2-2.bnf"
PROCEDURE Qualident (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
 name: Name ;
                                                                                 Type, Sym: CARDINAL ; 
# 553 "bnf/m2-2.bnf"
BEGIN
# 553 "bnf/m2-2.bnf"
# 556 "bnf/m2-2.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 556 "bnf/m2-2.bnf"
# 573 "bnf/m2-2.bnf"
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
# 574 "bnf/m2-2.bnf"
   WHILE currenttoken=periodtok DO
      Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 574 "bnf/m2-2.bnf"
      Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
   END (* while *) ;
# 574 "bnf/m2-2.bnf"
   END  ;
END Qualident ;


(*
   ConstantDeclaration := Ident '=' 
                          % BuildConst ;
                            StartBuildingConstDeclaration  %
                          ConstExpression 
                          % EndBuildingConstDeclaration ;
                            PopNothing  %
                          

   first  symbols:identtok
   
   cannot reachend
*)

# 577 "bnf/m2-2.bnf"
PROCEDURE ConstantDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 577 "bnf/m2-2.bnf"
BEGIN
# 577 "bnf/m2-2.bnf"
   Ident(stopset0 + SetOfStop0{equaltok}, stopset1, stopset2) ;
# 577 "bnf/m2-2.bnf"
   Expect(equaltok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 577 "bnf/m2-2.bnf"
# 578 "bnf/m2-2.bnf"
   BuildConst ;
   StartBuildingConstDeclaration  ;
# 579 "bnf/m2-2.bnf"
   ConstExpression(stopset0, stopset1, stopset2) ;
# 579 "bnf/m2-2.bnf"
# 580 "bnf/m2-2.bnf"
   EndBuildingConstDeclaration ;
   PopNothing  ;
END ConstantDeclaration ;


(*
   ConstExpression := 
                      % PushAutoOff  %
                      SimpleConstExpr [ Relation SimpleConstExpr  ]
                      
                      % PopAuto  %
                      

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 583 "bnf/m2-2.bnf"
PROCEDURE ConstExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 583 "bnf/m2-2.bnf"
BEGIN
# 583 "bnf/m2-2.bnf"
   PushAutoOff  ;
# 584 "bnf/m2-2.bnf"
   SimpleConstExpr(stopset0 + SetOfStop0{equaltok, hashtok, lessgreatertok, lesstok, lessequaltok, greatertok, greaterequaltok}, stopset1 + SetOfStop1{intok}, stopset2) ;
# 584 "bnf/m2-2.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok})) OR
      (currenttoken=intok)
   THEN
      Relation(stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 584 "bnf/m2-2.bnf"
      SimpleConstExpr(stopset0, stopset1, stopset2) ;
   END ;
# 584 "bnf/m2-2.bnf"
   PopAuto  ;
END ConstExpression ;


(*
   Relation := '='  | '#'  | '<>'  | '<'  | '<='  | '>'  | 
               '>='  | 'IN' 

   first  symbols:intok, greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok
   
   cannot reachend
*)

# 587 "bnf/m2-2.bnf"
PROCEDURE Relation (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 587 "bnf/m2-2.bnf"
BEGIN
# 587 "bnf/m2-2.bnf"
   IF currenttoken=equaltok
   THEN
      Expect(equaltok, stopset0, stopset1, stopset2) ;
# 587 "bnf/m2-2.bnf"
   ELSIF currenttoken=hashtok
   THEN
      Expect(hashtok, stopset0, stopset1, stopset2) ;
# 587 "bnf/m2-2.bnf"
   ELSIF currenttoken=lessgreatertok
   THEN
      Expect(lessgreatertok, stopset0, stopset1, stopset2) ;
# 587 "bnf/m2-2.bnf"
   ELSIF currenttoken=lesstok
   THEN
      Expect(lesstok, stopset0, stopset1, stopset2) ;
# 587 "bnf/m2-2.bnf"
   ELSIF currenttoken=lessequaltok
   THEN
      Expect(lessequaltok, stopset0, stopset1, stopset2) ;
# 587 "bnf/m2-2.bnf"
   ELSIF currenttoken=greatertok
   THEN
      Expect(greatertok, stopset0, stopset1, stopset2) ;
# 587 "bnf/m2-2.bnf"
   ELSIF currenttoken=greaterequaltok
   THEN
      Expect(greaterequaltok, stopset0, stopset1, stopset2) ;
# 587 "bnf/m2-2.bnf"
   ELSIF currenttoken=intok
   THEN
      Expect(intok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: IN >= > <= < <> # =')
   END ;
END Relation ;


(*
   SimpleConstExpr := UnaryOrConstTerm { AddOperator ConstTerm  }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 589 "bnf/m2-2.bnf"
PROCEDURE SimpleConstExpr (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 589 "bnf/m2-2.bnf"
BEGIN
# 589 "bnf/m2-2.bnf"
   UnaryOrConstTerm(stopset0 + SetOfStop0{plustok, minustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
# 589 "bnf/m2-2.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok})) OR
         (currenttoken=ortok) DO
      AddOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 589 "bnf/m2-2.bnf"
      ConstTerm(stopset0 + SetOfStop0{minustok, plustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
   END (* while *) ;
END SimpleConstExpr ;


(*
   UnaryOrConstTerm := '+' ConstTerm  | '-' ConstTerm  | 
                       ConstTerm 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 591 "bnf/m2-2.bnf"
PROCEDURE UnaryOrConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 591 "bnf/m2-2.bnf"
BEGIN
# 591 "bnf/m2-2.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 591 "bnf/m2-2.bnf"
      ConstTerm(stopset0, stopset1, stopset2) ;
# 591 "bnf/m2-2.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 591 "bnf/m2-2.bnf"
      ConstTerm(stopset0, stopset1, stopset2) ;
# 591 "bnf/m2-2.bnf"
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
   AddOperator := '+'  | '-'  | 'OR' 

   first  symbols:ortok, minustok, plustok
   
   cannot reachend
*)

# 593 "bnf/m2-2.bnf"
PROCEDURE AddOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 593 "bnf/m2-2.bnf"
BEGIN
# 593 "bnf/m2-2.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0, stopset1, stopset2) ;
# 593 "bnf/m2-2.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0, stopset1, stopset2) ;
# 593 "bnf/m2-2.bnf"
   ELSIF currenttoken=ortok
   THEN
      Expect(ortok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: OR - +')
   END ;
END AddOperator ;


(*
   ConstTerm := ConstFactor { MulOperator ConstFactor  }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok
   
   cannot reachend
*)

# 595 "bnf/m2-2.bnf"
PROCEDURE ConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 595 "bnf/m2-2.bnf"
BEGIN
# 595 "bnf/m2-2.bnf"
   ConstFactor(stopset0 + SetOfStop0{timestok, dividetok, andtok, ambersandtok}, stopset1 + SetOfStop1{divtok, modtok}, stopset2) ;
# 595 "bnf/m2-2.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {ambersandtok, andtok, dividetok, timestok})) OR
         ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {modtok, divtok})) DO
      MulOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 595 "bnf/m2-2.bnf"
      ConstFactor(stopset0 + SetOfStop0{ambersandtok, andtok, dividetok, timestok}, stopset1 + SetOfStop1{modtok, divtok}, stopset2) ;
   END (* while *) ;
END ConstTerm ;


(*
   MulOperator := '*'  | '/'  | 'DIV'  | 'MOD'  | 'AND'  | 
                  '&' 

   first  symbols:ambersandtok, andtok, modtok, divtok, dividetok, timestok
   
   cannot reachend
*)

# 597 "bnf/m2-2.bnf"
PROCEDURE MulOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 597 "bnf/m2-2.bnf"
BEGIN
# 597 "bnf/m2-2.bnf"
   IF currenttoken=timestok
   THEN
      Expect(timestok, stopset0, stopset1, stopset2) ;
# 597 "bnf/m2-2.bnf"
   ELSIF currenttoken=dividetok
   THEN
      Expect(dividetok, stopset0, stopset1, stopset2) ;
# 597 "bnf/m2-2.bnf"
   ELSIF currenttoken=divtok
   THEN
      Expect(divtok, stopset0, stopset1, stopset2) ;
# 597 "bnf/m2-2.bnf"
   ELSIF currenttoken=modtok
   THEN
      Expect(modtok, stopset0, stopset1, stopset2) ;
# 597 "bnf/m2-2.bnf"
   ELSIF currenttoken=andtok
   THEN
      Expect(andtok, stopset0, stopset1, stopset2) ;
# 597 "bnf/m2-2.bnf"
   ELSIF currenttoken=ambersandtok
   THEN
      Expect(ambersandtok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: & AND MOD DIV / *')
   END ;
END MulOperator ;


(*
   ConstFactor := ConstQualidentOrSet  | 
                  Number  | ConstString  | '(' ConstExpression 
                  ')'  | 'NOT' ConstFactor 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok
   
   cannot reachend
*)

# 599 "bnf/m2-2.bnf"
PROCEDURE ConstFactor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 599 "bnf/m2-2.bnf"
BEGIN
# 599 "bnf/m2-2.bnf"
   IF (currenttoken=lcbratok) OR
      (currenttoken=identtok)
   THEN
      ConstQualidentOrSet(stopset0, stopset1, stopset2) ;
# 599 "bnf/m2-2.bnf"
   ELSIF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {realtok, integertok}))
   THEN
      Number(stopset0, stopset1, stopset2) ;
# 600 "bnf/m2-2.bnf"
   ELSIF currenttoken=stringtok
   THEN
      ConstString(stopset0, stopset1, stopset2) ;
# 601 "bnf/m2-2.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 601 "bnf/m2-2.bnf"
      ConstExpression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 601 "bnf/m2-2.bnf"
      Expect(rparatok, stopset0, stopset1, stopset2) ;
# 601 "bnf/m2-2.bnf"
   ELSIF currenttoken=nottok
   THEN
      Expect(nottok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 601 "bnf/m2-2.bnf"
      ConstFactor(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: NOT ( string real number integer number identifier {')
   END ;
END ConstFactor ;


(*
   ConstString := 
                  % PushAutoOn ;  %
                  string 
                  % PopAuto ;  %
                  
                  % BuildConstTypeFromAssignment ;  %
                  

   first  symbols:stringtok
   
   cannot reachend
*)

# 605 "bnf/m2-2.bnf"
PROCEDURE ConstString (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 605 "bnf/m2-2.bnf"
BEGIN
# 605 "bnf/m2-2.bnf"
   PushAutoOn ;  ;
# 606 "bnf/m2-2.bnf"
   string(stopset0, stopset1, stopset2) ;
# 606 "bnf/m2-2.bnf"
   PopAuto ;  ;
# 607 "bnf/m2-2.bnf"
   BuildConstTypeFromAssignment ;  ;
END ConstString ;


(*
   ConstQualidentOrSet := SimpleSet 
                          % BuildConstSetType ;  %
                           | Qualident [ SimpleSet 
                                         % BuildConstSetType ;  %
                                          ]

   first  symbols:identtok, lcbratok
   
   cannot reachend
*)

# 610 "bnf/m2-2.bnf"
PROCEDURE ConstQualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 610 "bnf/m2-2.bnf"
BEGIN
# 610 "bnf/m2-2.bnf"
   IF currenttoken=lcbratok
   THEN
      SimpleSet(stopset0, stopset1, stopset2) ;
# 610 "bnf/m2-2.bnf"
      BuildConstSetType ;  ;
# 611 "bnf/m2-2.bnf"
   ELSIF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok}, stopset1, stopset2) ;
# 611 "bnf/m2-2.bnf"
      IF currenttoken=lcbratok
      THEN
         SimpleSet(stopset0, stopset1, stopset2) ;
# 611 "bnf/m2-2.bnf"
         BuildConstSetType ;  ;
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

# 615 "bnf/m2-2.bnf"
PROCEDURE QualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 615 "bnf/m2-2.bnf"
BEGIN
# 615 "bnf/m2-2.bnf"
   IF currenttoken=lcbratok
   THEN
      SimpleSet(stopset0, stopset1, stopset2) ;
# 615 "bnf/m2-2.bnf"
   ELSIF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok}, stopset1, stopset2) ;
# 615 "bnf/m2-2.bnf"
      IF currenttoken=lcbratok
      THEN
         SimpleSet(stopset0, stopset1, stopset2) ;
      END ;
   ELSE
      ErrorArray('expecting one of: identifier {')
   END ;
END QualidentOrSet ;


(*
   Element := ConstExpression [ '..' ConstExpression  ]

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   reachend
*)

# 619 "bnf/m2-2.bnf"
PROCEDURE Element (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 619 "bnf/m2-2.bnf"
BEGIN
# 619 "bnf/m2-2.bnf"
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 619 "bnf/m2-2.bnf"
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 619 "bnf/m2-2.bnf"
      ConstExpression(stopset0, stopset1, stopset2) ;
   END ;
END Element ;


(*
   TypeDeclaration := Ident '=' Type 
                      % BuildTypeEnd ;  %
                      

   first  symbols:identtok
   
   cannot reachend
*)

# 621 "bnf/m2-2.bnf"
PROCEDURE TypeDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 621 "bnf/m2-2.bnf"
BEGIN
# 621 "bnf/m2-2.bnf"
   Ident(stopset0 + SetOfStop0{equaltok}, stopset1, stopset2) ;
# 621 "bnf/m2-2.bnf"
   Expect(equaltok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 621 "bnf/m2-2.bnf"
   Type(stopset0, stopset1, stopset2) ;
# 621 "bnf/m2-2.bnf"
   BuildTypeEnd ;  ;
END TypeDeclaration ;


(*
   Type := 
           % PushAutoOn ;  %
           ( SimpleType  | ArrayType 
             % BuildType ;  %
              | RecordType 
             % BuildType ;  %
              | SetType 
             % BuildType ;  %
              | PointerType 
             % BuildType ;  %
              | ProcedureType 
             % BuildType ;  %
              )
           % PopAuto ;  %
           

   first  symbols:proceduretok, pointertok, settok, recordtok, arraytok, lsbratok, lparatok, identtok
   
   cannot reachend
*)

# 624 "bnf/m2-2.bnf"
PROCEDURE Type (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 624 "bnf/m2-2.bnf"
BEGIN
# 624 "bnf/m2-2.bnf"
# 625 "bnf/m2-2.bnf"
   PushAutoOn ;  ;
# 626 "bnf/m2-2.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lsbratok, lparatok})) OR
      (currenttoken=identtok)
   THEN
      SimpleType(stopset0, stopset1, stopset2) ;
# 626 "bnf/m2-2.bnf"
   ELSIF currenttoken=arraytok
   THEN
      ArrayType(stopset0, stopset1, stopset2) ;
# 626 "bnf/m2-2.bnf"
      BuildType ;  ;
# 627 "bnf/m2-2.bnf"
   ELSIF currenttoken=recordtok
   THEN
      RecordType(stopset0, stopset1, stopset2) ;
# 627 "bnf/m2-2.bnf"
      BuildType ;  ;
# 628 "bnf/m2-2.bnf"
   ELSIF currenttoken=settok
   THEN
      SetType(stopset0, stopset1, stopset2) ;
# 628 "bnf/m2-2.bnf"
      BuildType ;  ;
# 629 "bnf/m2-2.bnf"
   ELSIF currenttoken=pointertok
   THEN
      PointerType(stopset0, stopset1, stopset2) ;
# 629 "bnf/m2-2.bnf"
      BuildType ;  ;
# 630 "bnf/m2-2.bnf"
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureType(stopset0, stopset1, stopset2) ;
# 630 "bnf/m2-2.bnf"
      BuildType ;  ;
   ELSE
      ErrorArray('expecting one of: PROCEDURE POINTER SET RECORD ARRAY [ ( identifier')
   END ;
# 631 "bnf/m2-2.bnf"
   PopAuto ;  ;
END Type ;


(*
   SimpleType := ( Qualident 
                   % BuildType ;  %
                    | Enumeration 
                   % BuildType ;  %
                    | SubrangeType 
                   % BuildType ;  %
                    )

   first  symbols:lsbratok, lparatok, identtok
   
   cannot reachend
*)

# 634 "bnf/m2-2.bnf"
PROCEDURE SimpleType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 634 "bnf/m2-2.bnf"
BEGIN
# 634 "bnf/m2-2.bnf"
# 635 "bnf/m2-2.bnf"
   IF currenttoken=identtok
   THEN
      Qualident(stopset0, stopset1, stopset2) ;
# 635 "bnf/m2-2.bnf"
      BuildType ;  ;
# 636 "bnf/m2-2.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Enumeration(stopset0, stopset1, stopset2) ;
# 636 "bnf/m2-2.bnf"
      BuildType ;  ;
# 637 "bnf/m2-2.bnf"
   ELSIF currenttoken=lsbratok
   THEN
      SubrangeType(stopset0, stopset1, stopset2) ;
# 637 "bnf/m2-2.bnf"
      BuildType ;  ;
   ELSE
      ErrorArray('expecting one of: [ ( identifier')
   END ;
END SimpleType ;


(*
   Enumeration := '(' ( IdentList  )')' 
                  % BuildEnumeration ;  %
                  

   first  symbols:lparatok
   
   cannot reachend
*)

# 641 "bnf/m2-2.bnf"
PROCEDURE Enumeration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 641 "bnf/m2-2.bnf"
BEGIN
# 641 "bnf/m2-2.bnf"
   Expect(lparatok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 642 "bnf/m2-2.bnf"
   IdentList(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 644 "bnf/m2-2.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
# 644 "bnf/m2-2.bnf"
   BuildEnumeration ;  ;
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

# 647 "bnf/m2-2.bnf"
PROCEDURE IdentList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR

                                                                                on: BOOLEAN ;
                                                                                n : CARDINAL ; 
# 647 "bnf/m2-2.bnf"
BEGIN
# 647 "bnf/m2-2.bnf"
   Ident(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 647 "bnf/m2-2.bnf"
# 650 "bnf/m2-2.bnf"
# 654 "bnf/m2-2.bnf"
   on := IsAutoPushOn() ;
   IF on
   THEN
      n := 1
   END  ;
# 655 "bnf/m2-2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 655 "bnf/m2-2.bnf"
      Ident(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 655 "bnf/m2-2.bnf"
# 658 "bnf/m2-2.bnf"
      IF on
      THEN
         INC(n)
      END  ;
   END (* while *) ;
# 659 "bnf/m2-2.bnf"
# 662 "bnf/m2-2.bnf"
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

# 665 "bnf/m2-2.bnf"
PROCEDURE SubrangeType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 665 "bnf/m2-2.bnf"
BEGIN
# 665 "bnf/m2-2.bnf"
   Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 665 "bnf/m2-2.bnf"
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 665 "bnf/m2-2.bnf"
   Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 665 "bnf/m2-2.bnf"
   ConstExpression(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 665 "bnf/m2-2.bnf"
   Expect(rsbratok, stopset0, stopset1, stopset2) ;
# 665 "bnf/m2-2.bnf"
   BuildSubrange ;  ;
END SubrangeType ;


(*
   ArrayType := 'ARRAY' 
                % StartBuildArray ;
                  BuildNulName ;  %
                SimpleType 
                % BuildFieldArray ;  %
                { ',' 
                  % BuildNulName ;  %
                  SimpleType 
                  % BuildFieldArray ;  %
                   }'OF' 
                % BuildNulName ;  %
                Type 
                % EndBuildArray ;  %
                

   first  symbols:arraytok
   
   cannot reachend
*)

# 668 "bnf/m2-2.bnf"
PROCEDURE ArrayType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 668 "bnf/m2-2.bnf"
BEGIN
# 668 "bnf/m2-2.bnf"
   Expect(arraytok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 668 "bnf/m2-2.bnf"
# 669 "bnf/m2-2.bnf"
   StartBuildArray ;
   BuildNulName ;  ;
# 670 "bnf/m2-2.bnf"
   SimpleType(stopset0 + SetOfStop0{commatok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 670 "bnf/m2-2.bnf"
   BuildFieldArray ;  ;
# 671 "bnf/m2-2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 671 "bnf/m2-2.bnf"
      BuildNulName ;  ;
# 672 "bnf/m2-2.bnf"
      SimpleType(stopset0 + SetOfStop0{commatok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 672 "bnf/m2-2.bnf"
      BuildFieldArray ;  ;
   END (* while *) ;
# 673 "bnf/m2-2.bnf"
   Expect(oftok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 673 "bnf/m2-2.bnf"
   BuildNulName ;  ;
# 674 "bnf/m2-2.bnf"
   Type(stopset0, stopset1, stopset2) ;
# 674 "bnf/m2-2.bnf"
   EndBuildArray ;  ;
END ArrayType ;


(*
   RecordType := 'RECORD' 
                 % BuildRecord ;  %
                 FieldListSequence 'END' 

   first  symbols:recordtok
   
   cannot reachend
*)

# 677 "bnf/m2-2.bnf"
PROCEDURE RecordType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 677 "bnf/m2-2.bnf"
BEGIN
# 677 "bnf/m2-2.bnf"
   Expect(recordtok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok, endtok}, stopset2 + SetOfStop2{identtok}) ;
# 677 "bnf/m2-2.bnf"
   BuildRecord ;  ;
# 678 "bnf/m2-2.bnf"
   FieldListSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 678 "bnf/m2-2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END RecordType ;


(*
   FieldListSequence := FieldListStatement { ';' FieldListStatement  }

   first  symbols:semicolontok, casetok, identtok
   
   reachend
*)

# 681 "bnf/m2-2.bnf"
PROCEDURE FieldListSequence (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 681 "bnf/m2-2.bnf"
BEGIN
# 681 "bnf/m2-2.bnf"
   FieldListStatement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 681 "bnf/m2-2.bnf"
   WHILE currenttoken=semicolontok DO
      Expect(semicolontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok}, stopset2 + SetOfStop2{identtok}) ;
# 681 "bnf/m2-2.bnf"
      FieldListStatement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END (* while *) ;
END FieldListSequence ;


(*
   FieldListStatement := [ FieldList  ]

   first  symbols:casetok, identtok
   
   reachend
*)

# 684 "bnf/m2-2.bnf"
PROCEDURE FieldListStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 684 "bnf/m2-2.bnf"
BEGIN
# 684 "bnf/m2-2.bnf"
   IF (currenttoken=casetok) OR
      (currenttoken=identtok)
   THEN
      FieldList(stopset0, stopset1, stopset2) ;
   END ;
END FieldListStatement ;


(*
   FieldList := IdentList ':' 
                % BuildNulName ;  %
                Type 
                % BuildFieldRecord ;  %
                 | 'CASE' Ident [ ':' Qualident 
                                  % BuildVarientSelector ;  %
                                  
                                  % BuildNulName ;  %
                                   | '.' 
                                  % PushAutoOff ;  %
                                  Qualident 
                                  % PopAuto ;  %
                                   ]'OF' 
                % PopNothing ;  %
                
                % StartBuildVarient ;  %
                
                % StartBuildVarientFieldRecord ;  %
                Varient 
                % EndBuildVarientFieldRecord ;  %
                { '|' 
                  % StartBuildVarientFieldRecord ;  %
                  Varient 
                  % EndBuildVarientFieldRecord ;  %
                   }[ 'ELSE' 
                      % StartBuildVarientFieldRecord ;  %
                      FieldListSequence 
                      % EndBuildVarientFieldRecord ;  %
                       ]'END' 
                % EndBuildVarient ;  %
                

   first  symbols:casetok, identtok
   
   cannot reachend
*)

# 695 "bnf/m2-2.bnf"
PROCEDURE FieldList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 695 "bnf/m2-2.bnf"
BEGIN
# 695 "bnf/m2-2.bnf"
   IF currenttoken=identtok
   THEN
      IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 695 "bnf/m2-2.bnf"
      Expect(colontok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 695 "bnf/m2-2.bnf"
      BuildNulName ;  ;
# 696 "bnf/m2-2.bnf"
      Type(stopset0, stopset1, stopset2) ;
# 696 "bnf/m2-2.bnf"
      BuildFieldRecord ;  ;
# 698 "bnf/m2-2.bnf"
   ELSIF currenttoken=casetok
   THEN
      Expect(casetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 698 "bnf/m2-2.bnf"
      Ident(stopset0 + SetOfStop0{colontok, periodtok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 698 "bnf/m2-2.bnf"
      IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {periodtok, colontok}))
      THEN
         (* seen optional [ | ] expression *)
# 698 "bnf/m2-2.bnf"
         IF currenttoken=colontok
         THEN
            Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 698 "bnf/m2-2.bnf"
            Qualident(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 698 "bnf/m2-2.bnf"
            BuildVarientSelector ;  ;
# 699 "bnf/m2-2.bnf"
            BuildNulName ;  ;
# 700 "bnf/m2-2.bnf"
         ELSIF currenttoken=periodtok
         THEN
            Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 700 "bnf/m2-2.bnf"
            PushAutoOff ;  ;
# 701 "bnf/m2-2.bnf"
            Qualident(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 701 "bnf/m2-2.bnf"
            PopAuto ;  ;
         ELSE
            ErrorArray('expecting one of: . :')
         END ;
         (* end of optional [ | ] expression *)
      END ;
# 702 "bnf/m2-2.bnf"
      Expect(oftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 702 "bnf/m2-2.bnf"
      PopNothing ;  ;
# 703 "bnf/m2-2.bnf"
      StartBuildVarient ;  ;
# 704 "bnf/m2-2.bnf"
      StartBuildVarientFieldRecord ;  ;
# 705 "bnf/m2-2.bnf"
      Varient(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{elsetok, endtok}, stopset2) ;
# 705 "bnf/m2-2.bnf"
      EndBuildVarientFieldRecord ;  ;
# 706 "bnf/m2-2.bnf"
      WHILE currenttoken=bartok DO
         Expect(bartok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 706 "bnf/m2-2.bnf"
         StartBuildVarientFieldRecord ;  ;
# 707 "bnf/m2-2.bnf"
         Varient(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{endtok, elsetok}, stopset2) ;
# 707 "bnf/m2-2.bnf"
         EndBuildVarientFieldRecord ;  ;
      END (* while *) ;
# 709 "bnf/m2-2.bnf"
      IF currenttoken=elsetok
      THEN
         Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok, endtok}, stopset2 + SetOfStop2{identtok}) ;
# 709 "bnf/m2-2.bnf"
         StartBuildVarientFieldRecord ;  ;
# 710 "bnf/m2-2.bnf"
         FieldListSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 710 "bnf/m2-2.bnf"
         EndBuildVarientFieldRecord ;  ;
      END ;
# 711 "bnf/m2-2.bnf"
      Expect(endtok, stopset0, stopset1, stopset2) ;
# 711 "bnf/m2-2.bnf"
      EndBuildVarient ;  ;
   ELSE
      ErrorArray('expecting one of: CASE identifier')
   END ;
END FieldList ;


(*
   Varient := CaseLabelList ':' FieldListSequence 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 714 "bnf/m2-2.bnf"
PROCEDURE Varient (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 714 "bnf/m2-2.bnf"
BEGIN
# 714 "bnf/m2-2.bnf"
   CaseLabelList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 714 "bnf/m2-2.bnf"
   Expect(colontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok}, stopset2 + SetOfStop2{identtok}) ;
# 714 "bnf/m2-2.bnf"
   FieldListSequence(stopset0, stopset1, stopset2) ;
END Varient ;


(*
   CaseLabelList := CaseLabels { ',' CaseLabels  }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 716 "bnf/m2-2.bnf"
PROCEDURE CaseLabelList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 716 "bnf/m2-2.bnf"
BEGIN
# 716 "bnf/m2-2.bnf"
   CaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 716 "bnf/m2-2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 716 "bnf/m2-2.bnf"
      CaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END CaseLabelList ;


(*
   CaseLabels := ConstExpression [ '..' ConstExpression  ]

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 718 "bnf/m2-2.bnf"
PROCEDURE CaseLabels (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 718 "bnf/m2-2.bnf"
BEGIN
# 718 "bnf/m2-2.bnf"
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 718 "bnf/m2-2.bnf"
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 718 "bnf/m2-2.bnf"
      ConstExpression(stopset0, stopset1, stopset2) ;
   END ;
END CaseLabels ;


(*
   SetType := 'SET' 'OF' 
              % BuildNulName ;  %
              SimpleType 
              % BuildSetType ;  %
              

   first  symbols:settok
   
   cannot reachend
*)

# 720 "bnf/m2-2.bnf"
PROCEDURE SetType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 720 "bnf/m2-2.bnf"
BEGIN
# 720 "bnf/m2-2.bnf"
   Expect(settok, stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 720 "bnf/m2-2.bnf"
   Expect(oftok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 720 "bnf/m2-2.bnf"
   BuildNulName ;  ;
# 721 "bnf/m2-2.bnf"
   SimpleType(stopset0, stopset1, stopset2) ;
# 721 "bnf/m2-2.bnf"
   BuildSetType ;  ;
END SetType ;


(*
   PointerType := 'POINTER' 'TO' 
                  % BuildNulName ;  %
                  Type 
                  % BuildPointerType ;  %
                  

   first  symbols:pointertok
   
   cannot reachend
*)

# 724 "bnf/m2-2.bnf"
PROCEDURE PointerType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 724 "bnf/m2-2.bnf"
BEGIN
# 724 "bnf/m2-2.bnf"
   Expect(pointertok, stopset0, stopset1, stopset2 + SetOfStop2{totok}) ;
# 724 "bnf/m2-2.bnf"
   Expect(totok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 724 "bnf/m2-2.bnf"
   BuildNulName ;  ;
# 725 "bnf/m2-2.bnf"
   Type(stopset0, stopset1, stopset2) ;
# 725 "bnf/m2-2.bnf"
   BuildPointerType ;  ;
END PointerType ;


(*
   ProcedureType := 'PROCEDURE' 
                    % BuildProcedureType ;  %
                    [ FormalTypeList  ]

   first  symbols:proceduretok
   
   cannot reachend
*)

# 728 "bnf/m2-2.bnf"
PROCEDURE ProcedureType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 728 "bnf/m2-2.bnf"
BEGIN
# 728 "bnf/m2-2.bnf"
   Expect(proceduretok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 728 "bnf/m2-2.bnf"
   BuildProcedureType ;  ;
# 729 "bnf/m2-2.bnf"
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

# 731 "bnf/m2-2.bnf"
PROCEDURE FormalTypeList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 731 "bnf/m2-2.bnf"
BEGIN
# 731 "bnf/m2-2.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{rparatok, arraytok}, stopset1, stopset2 + SetOfStop2{vartok, identtok}) ;
# 731 "bnf/m2-2.bnf"
   IF currenttoken=rparatok
   THEN
      Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 731 "bnf/m2-2.bnf"
      FormalReturn(stopset0, stopset1, stopset2) ;
# 732 "bnf/m2-2.bnf"
   ELSIF (currenttoken=arraytok) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, vartok}))
   THEN
      ProcedureParameters(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 732 "bnf/m2-2.bnf"
      Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 732 "bnf/m2-2.bnf"
      FormalReturn(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: identifier ARRAY VAR )')
   END ;
END FormalTypeList ;


(*
   FormalReturn := [ ':' Qualident 
                     % BuildFunction ;  %
                      ]

   first  symbols:colontok
   
   reachend
*)

# 734 "bnf/m2-2.bnf"
PROCEDURE FormalReturn (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 734 "bnf/m2-2.bnf"
BEGIN
# 734 "bnf/m2-2.bnf"
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 734 "bnf/m2-2.bnf"
      Qualident(stopset0, stopset1, stopset2) ;
# 734 "bnf/m2-2.bnf"
      BuildFunction ;  ;
   END ;
END FormalReturn ;


(*
   ProcedureParameters := ProcedureParameter { ',' ProcedureParameter  }

   first  symbols:identtok, arraytok, vartok
   
   cannot reachend
*)

# 737 "bnf/m2-2.bnf"
PROCEDURE ProcedureParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 737 "bnf/m2-2.bnf"
BEGIN
# 737 "bnf/m2-2.bnf"
# 738 "bnf/m2-2.bnf"
   ProcedureParameter(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 738 "bnf/m2-2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{vartok, identtok}) ;
# 738 "bnf/m2-2.bnf"
      ProcedureParameter(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END ProcedureParameters ;


(*
   ProcedureParameter := 'VAR' 
                         % PushT(VarTok) ;  %
                         FormalType 
                         % BuildFormalType ;  %
                          | 
                         % PushT(NulTok) ;  %
                         FormalType 
                         % BuildFormalType ;  %
                         

   first  symbols:identtok, arraytok, vartok
   
   cannot reachend
*)

# 740 "bnf/m2-2.bnf"
PROCEDURE ProcedureParameter (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 740 "bnf/m2-2.bnf"
BEGIN
# 740 "bnf/m2-2.bnf"
   IF currenttoken=vartok
   THEN
      Expect(vartok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 740 "bnf/m2-2.bnf"
      PushT(VarTok) ;  ;
# 741 "bnf/m2-2.bnf"
      FormalType(stopset0, stopset1, stopset2) ;
# 741 "bnf/m2-2.bnf"
      BuildFormalType ;  ;
# 742 "bnf/m2-2.bnf"
   ELSE
# 742 "bnf/m2-2.bnf"
      PushT(NulTok) ;  ;
# 743 "bnf/m2-2.bnf"
      FormalType(stopset0, stopset1, stopset2) ;
# 743 "bnf/m2-2.bnf"
      BuildFormalType ;  ;
   END ;
END ProcedureParameter ;


(*
   VariableDeclaration := IdentList ':' 
                          % BuildNulName  %
                          Type 
                          % BuildVariable  %
                          

   first  symbols:identtok
   
   cannot reachend
*)

# 746 "bnf/m2-2.bnf"
PROCEDURE VariableDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 746 "bnf/m2-2.bnf"
BEGIN
# 746 "bnf/m2-2.bnf"
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 746 "bnf/m2-2.bnf"
   Expect(colontok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 746 "bnf/m2-2.bnf"
   BuildNulName  ;
# 747 "bnf/m2-2.bnf"
   Type(stopset0, stopset1, stopset2) ;
# 747 "bnf/m2-2.bnf"
   BuildVariable  ;
END VariableDeclaration ;


(*
   Designator := Qualident { SubDesignator  }

   first  symbols:identtok
   
   cannot reachend
*)

# 750 "bnf/m2-2.bnf"
PROCEDURE Designator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 750 "bnf/m2-2.bnf"
BEGIN
# 750 "bnf/m2-2.bnf"
   Qualident(stopset0 + SetOfStop0{periodtok, lsbratok, uparrowtok}, stopset1, stopset2) ;
# 750 "bnf/m2-2.bnf"
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

# 752 "bnf/m2-2.bnf"
PROCEDURE SubDesignator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 752 "bnf/m2-2.bnf"
BEGIN
# 752 "bnf/m2-2.bnf"
   IF currenttoken=periodtok
   THEN
      Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 752 "bnf/m2-2.bnf"
      Ident(stopset0, stopset1, stopset2) ;
# 752 "bnf/m2-2.bnf"
   ELSIF currenttoken=lsbratok
   THEN
      Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 752 "bnf/m2-2.bnf"
      ExpList(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 752 "bnf/m2-2.bnf"
      Expect(rsbratok, stopset0, stopset1, stopset2) ;
# 752 "bnf/m2-2.bnf"
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

# 754 "bnf/m2-2.bnf"
PROCEDURE ExpList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 754 "bnf/m2-2.bnf"
BEGIN
# 754 "bnf/m2-2.bnf"
   Expression(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 754 "bnf/m2-2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 754 "bnf/m2-2.bnf"
      Expression(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END ExpList ;


(*
   Expression := 
                 % PushAutoOff  %
                 SimpleExpression [ Relation SimpleExpression  ]
                 
                 % PopAuto  %
                 

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

# 756 "bnf/m2-2.bnf"
PROCEDURE Expression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 756 "bnf/m2-2.bnf"
BEGIN
# 756 "bnf/m2-2.bnf"
   PushAutoOff  ;
# 757 "bnf/m2-2.bnf"
   SimpleExpression(stopset0 + SetOfStop0{equaltok, hashtok, lessgreatertok, lesstok, lessequaltok, greatertok, greaterequaltok}, stopset1 + SetOfStop1{intok}, stopset2) ;
# 757 "bnf/m2-2.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok})) OR
      (currenttoken=intok)
   THEN
      Relation(stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 757 "bnf/m2-2.bnf"
      SimpleExpression(stopset0, stopset1, stopset2) ;
   END ;
# 757 "bnf/m2-2.bnf"
   PopAuto  ;
END Expression ;


(*
   SimpleExpression := UnaryOrTerm { AddOperator Term  }

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

# 760 "bnf/m2-2.bnf"
PROCEDURE SimpleExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 760 "bnf/m2-2.bnf"
BEGIN
# 760 "bnf/m2-2.bnf"
   UnaryOrTerm(stopset0 + SetOfStop0{plustok, minustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
# 760 "bnf/m2-2.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok})) OR
         (currenttoken=ortok) DO
      AddOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 760 "bnf/m2-2.bnf"
      Term(stopset0 + SetOfStop0{minustok, plustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
   END (* while *) ;
END SimpleExpression ;


(*
   UnaryOrTerm := '+' Term  | '-' Term  | Term 

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

# 762 "bnf/m2-2.bnf"
PROCEDURE UnaryOrTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 762 "bnf/m2-2.bnf"
BEGIN
# 762 "bnf/m2-2.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 762 "bnf/m2-2.bnf"
      Term(stopset0, stopset1, stopset2) ;
# 762 "bnf/m2-2.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 762 "bnf/m2-2.bnf"
      Term(stopset0, stopset1, stopset2) ;
# 762 "bnf/m2-2.bnf"
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
   Term := Factor { MulOperator Factor  }

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok
   
   cannot reachend
*)

# 764 "bnf/m2-2.bnf"
PROCEDURE Term (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 764 "bnf/m2-2.bnf"
BEGIN
# 764 "bnf/m2-2.bnf"
   Factor(stopset0 + SetOfStop0{timestok, dividetok, andtok, ambersandtok}, stopset1 + SetOfStop1{divtok, modtok}, stopset2) ;
# 764 "bnf/m2-2.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {ambersandtok, andtok, dividetok, timestok})) OR
         ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {modtok, divtok})) DO
      MulOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 764 "bnf/m2-2.bnf"
      Factor(stopset0 + SetOfStop0{ambersandtok, andtok, dividetok, timestok}, stopset1 + SetOfStop1{modtok, divtok}, stopset2) ;
   END (* while *) ;
END Term ;


(*
   Factor := Number  | string  | SetOrDesignatorOrFunction  | 
             '(' Expression ')'  | 'NOT' Factor 

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok
   
   cannot reachend
*)

# 766 "bnf/m2-2.bnf"
PROCEDURE Factor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 766 "bnf/m2-2.bnf"
BEGIN
# 766 "bnf/m2-2.bnf"
   IF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {realtok, integertok}))
   THEN
      Number(stopset0, stopset1, stopset2) ;
# 766 "bnf/m2-2.bnf"
   ELSIF currenttoken=stringtok
   THEN
      string(stopset0, stopset1, stopset2) ;
# 766 "bnf/m2-2.bnf"
   ELSIF (currenttoken=lcbratok) OR
         (currenttoken=identtok)
   THEN
      SetOrDesignatorOrFunction(stopset0, stopset1, stopset2) ;
# 767 "bnf/m2-2.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 767 "bnf/m2-2.bnf"
      Expression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 767 "bnf/m2-2.bnf"
      Expect(rparatok, stopset0, stopset1, stopset2) ;
# 767 "bnf/m2-2.bnf"
   ELSIF currenttoken=nottok
   THEN
      Expect(nottok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 767 "bnf/m2-2.bnf"
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

# 771 "bnf/m2-2.bnf"
PROCEDURE SimpleSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 771 "bnf/m2-2.bnf"
BEGIN
# 771 "bnf/m2-2.bnf"
   Expect(lcbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 771 "bnf/m2-2.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {commatok, lparatok, lcbratok, minustok, plustok})) OR
      (currenttoken=nottok) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {stringtok, realtok, integertok, identtok}))
   THEN
      Element(stopset0 + SetOfStop0{commatok, rcbratok}, stopset1, stopset2) ;
# 771 "bnf/m2-2.bnf"
      WHILE currenttoken=commatok DO
         Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok, commatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 771 "bnf/m2-2.bnf"
         Element(stopset0 + SetOfStop0{rcbratok, commatok}, stopset1, stopset2) ;
      END (* while *) ;
   END ;
# 771 "bnf/m2-2.bnf"
   Expect(rcbratok, stopset0, stopset1, stopset2) ;
END SimpleSet ;


(*
   SetOrDesignatorOrFunction := ( Qualident [ SimpleSet  | 
                                              SimpleDes [ ActualParameters  ] ] | 
                                  SimpleSet  )

   first  symbols:lcbratok, identtok
   
   cannot reachend
*)

# 773 "bnf/m2-2.bnf"
PROCEDURE SetOrDesignatorOrFunction (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 773 "bnf/m2-2.bnf"
BEGIN
# 773 "bnf/m2-2.bnf"
   IF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok, periodtok, lsbratok, uparrowtok, lparatok}, stopset1, stopset2) ;
# 773 "bnf/m2-2.bnf"
      IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, uparrowtok, lsbratok, periodtok, lcbratok}))
      THEN
         (* seen optional [ | ] expression *)
# 773 "bnf/m2-2.bnf"
         IF currenttoken=lcbratok
         THEN
            SimpleSet(stopset0, stopset1, stopset2) ;
# 774 "bnf/m2-2.bnf"
         ELSIF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, uparrowtok, lsbratok, periodtok}))
         THEN
            SimpleDes(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 774 "bnf/m2-2.bnf"
            IF currenttoken=lparatok
            THEN
               ActualParameters(stopset0, stopset1, stopset2) ;
            END ;
         ELSE
            ErrorArray('expecting one of: ( ^ [ . {')
         END ;
         (* end of optional [ | ] expression *)
      END ;
# 776 "bnf/m2-2.bnf"
   ELSIF currenttoken=lcbratok
   THEN
      SimpleSet(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: { identifier')
   END ;
END SetOrDesignatorOrFunction ;


(*
   SimpleDes := { '.' Ident  | '[' ExpList ']'  | 
                  '^'  }

   first  symbols:uparrowtok, lsbratok, periodtok
   
   reachend
*)

# 779 "bnf/m2-2.bnf"
PROCEDURE SimpleDes (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 779 "bnf/m2-2.bnf"
BEGIN
# 779 "bnf/m2-2.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {uparrowtok, lsbratok, periodtok}))
   THEN
      (* seen optional { | } expression *)
      WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {uparrowtok, lsbratok, periodtok})) DO
# 779 "bnf/m2-2.bnf"
         IF currenttoken=periodtok
         THEN
            Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 779 "bnf/m2-2.bnf"
            Ident(stopset0 + SetOfStop0{uparrowtok, lsbratok, periodtok}, stopset1, stopset2) ;
# 779 "bnf/m2-2.bnf"
         ELSIF currenttoken=lsbratok
         THEN
            Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 779 "bnf/m2-2.bnf"
            ExpList(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 779 "bnf/m2-2.bnf"
            Expect(rsbratok, stopset0 + SetOfStop0{uparrowtok, lsbratok, periodtok}, stopset1, stopset2) ;
# 779 "bnf/m2-2.bnf"
         ELSIF currenttoken=uparrowtok
         THEN
            Expect(uparrowtok, stopset0 + SetOfStop0{uparrowtok, lsbratok, periodtok}, stopset1, stopset2) ;
         END ;
         (* end of optional { | } expression *)
      END ;
   END ;
END SimpleDes ;


(*
   ActualParameters := '(' [ ExpList  ]')' 

   first  symbols:lparatok
   
   cannot reachend
*)

# 781 "bnf/m2-2.bnf"
PROCEDURE ActualParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 781 "bnf/m2-2.bnf"
BEGIN
# 781 "bnf/m2-2.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 781 "bnf/m2-2.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok, minustok, plustok})) OR
      (currenttoken=nottok) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, stringtok, realtok, integertok}))
   THEN
      ExpList(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
   END ;
# 781 "bnf/m2-2.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END ActualParameters ;


(*
   Statement := 
                % PushAutoOff ;  %
                [ AssignmentOrProcedureCall  | 
                  IfStatement  | CaseStatement  | 
                  WhileStatement  | RepeatStatement  | 
                  LoopStatement  | ForStatement  | 
                  WithStatement  | AsmStatement  | 
                  'EXIT'  | 'RETURN' [ Expression  ] ]
                % PopAuto ;  %
                

   first  symbols:returntok, exittok, asmtok, withtok, fortok, looptok, repeattok, whiletok, casetok, iftok, identtok
   
   reachend
*)

# 783 "bnf/m2-2.bnf"
PROCEDURE Statement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 783 "bnf/m2-2.bnf"
BEGIN
# 783 "bnf/m2-2.bnf"
   PushAutoOff ;  ;
# 784 "bnf/m2-2.bnf"
   IF ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {returntok, exittok, fortok, looptok, repeattok, casetok, iftok})) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {asmtok, withtok, whiletok, identtok}))
   THEN
      (* seen optional [ | ] expression *)
# 784 "bnf/m2-2.bnf"
      IF currenttoken=identtok
      THEN
         AssignmentOrProcedureCall(stopset0, stopset1, stopset2) ;
# 784 "bnf/m2-2.bnf"
      ELSIF currenttoken=iftok
      THEN
         IfStatement(stopset0, stopset1, stopset2) ;
# 784 "bnf/m2-2.bnf"
      ELSIF currenttoken=casetok
      THEN
         CaseStatement(stopset0, stopset1, stopset2) ;
# 785 "bnf/m2-2.bnf"
      ELSIF currenttoken=whiletok
      THEN
         WhileStatement(stopset0, stopset1, stopset2) ;
# 785 "bnf/m2-2.bnf"
      ELSIF currenttoken=repeattok
      THEN
         RepeatStatement(stopset0, stopset1, stopset2) ;
# 785 "bnf/m2-2.bnf"
      ELSIF currenttoken=looptok
      THEN
         LoopStatement(stopset0, stopset1, stopset2) ;
# 786 "bnf/m2-2.bnf"
      ELSIF currenttoken=fortok
      THEN
         ForStatement(stopset0, stopset1, stopset2) ;
# 786 "bnf/m2-2.bnf"
      ELSIF currenttoken=withtok
      THEN
         WithStatement(stopset0, stopset1, stopset2) ;
# 786 "bnf/m2-2.bnf"
      ELSIF currenttoken=asmtok
      THEN
         AsmStatement(stopset0, stopset1, stopset2) ;
# 787 "bnf/m2-2.bnf"
      ELSIF currenttoken=exittok
      THEN
         Expect(exittok, stopset0, stopset1, stopset2) ;
# 787 "bnf/m2-2.bnf"
      ELSIF currenttoken=returntok
      THEN
         Expect(returntok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 787 "bnf/m2-2.bnf"
         IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok, minustok, plustok})) OR
            (currenttoken=nottok) OR
            ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, stringtok, realtok, integertok}))
         THEN
            Expression(stopset0, stopset1, stopset2) ;
         END ;
      ELSE
         ErrorArray('expecting one of: RETURN EXIT ASM WITH FOR LOOP REPEAT WHILE CASE IF identifier')
      END ;
      (* end of optional [ | ] expression *)
   END ;
# 787 "bnf/m2-2.bnf"
   PopAuto ;  ;
END Statement ;


(*
   AssignmentOrProcedureCall := Designator ( ':=' Expression  | 
                                             ActualParameters  | 
                                             
                                             % (* epsilon *)  %
                                              )

   first  symbols:identtok
   
   cannot reachend
*)

# 790 "bnf/m2-2.bnf"
PROCEDURE AssignmentOrProcedureCall (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 790 "bnf/m2-2.bnf"
BEGIN
# 790 "bnf/m2-2.bnf"
   Designator(stopset0 + SetOfStop0{becomestok, lparatok}, stopset1, stopset2) ;
# 790 "bnf/m2-2.bnf"
   IF currenttoken=becomestok
   THEN
      Expect(becomestok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 790 "bnf/m2-2.bnf"
      Expression(stopset0, stopset1, stopset2) ;
# 791 "bnf/m2-2.bnf"
   ELSIF currenttoken=lparatok
   THEN
      ActualParameters(stopset0, stopset1, stopset2) ;
# 791 "bnf/m2-2.bnf"
   ELSE
# 791 "bnf/m2-2.bnf"
      (* epsilon *)  ;
   END ;
END AssignmentOrProcedureCall ;


(*
   StatementSequence := Statement { ';' Statement  }

   first  symbols:semicolontok, returntok, exittok, asmtok, withtok, fortok, looptok, repeattok, whiletok, casetok, iftok, identtok
   
   reachend
*)

# 798 "bnf/m2-2.bnf"
PROCEDURE StatementSequence (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 798 "bnf/m2-2.bnf"
BEGIN
# 798 "bnf/m2-2.bnf"
   Statement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 798 "bnf/m2-2.bnf"
   WHILE currenttoken=semicolontok DO
      Expect(semicolontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 798 "bnf/m2-2.bnf"
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

# 800 "bnf/m2-2.bnf"
PROCEDURE IfStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 800 "bnf/m2-2.bnf"
BEGIN
# 800 "bnf/m2-2.bnf"
   Expect(iftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 800 "bnf/m2-2.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{thentok}, stopset2) ;
# 800 "bnf/m2-2.bnf"
   Expect(thentok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, elsiftok, elsetok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 801 "bnf/m2-2.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{elsiftok, elsetok, endtok}, stopset2) ;
# 801 "bnf/m2-2.bnf"
   WHILE currenttoken=elsiftok DO
      Expect(elsiftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 801 "bnf/m2-2.bnf"
      Expression(stopset0, stopset1 + SetOfStop1{thentok}, stopset2) ;
# 801 "bnf/m2-2.bnf"
      Expect(thentok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok, elsetok, elsiftok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 801 "bnf/m2-2.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok, elsetok, elsiftok}, stopset2) ;
   END (* while *) ;
# 802 "bnf/m2-2.bnf"
   IF currenttoken=elsetok
   THEN
      Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 802 "bnf/m2-2.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   END ;
# 802 "bnf/m2-2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END IfStatement ;


(*
   CaseStatement := 'CASE' Expression 'OF' Case { '|' Case  }
                    [ 'ELSE' StatementSequence  ]'END' 

   first  symbols:casetok
   
   cannot reachend
*)

# 804 "bnf/m2-2.bnf"
PROCEDURE CaseStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 804 "bnf/m2-2.bnf"
BEGIN
# 804 "bnf/m2-2.bnf"
   Expect(casetok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 804 "bnf/m2-2.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 804 "bnf/m2-2.bnf"
   Expect(oftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 804 "bnf/m2-2.bnf"
   Case(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{elsetok, endtok}, stopset2) ;
# 804 "bnf/m2-2.bnf"
   WHILE currenttoken=bartok DO
      Expect(bartok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 804 "bnf/m2-2.bnf"
      Case(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{endtok, elsetok}, stopset2) ;
   END (* while *) ;
# 805 "bnf/m2-2.bnf"
   IF currenttoken=elsetok
   THEN
      Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 805 "bnf/m2-2.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   END ;
# 805 "bnf/m2-2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END CaseStatement ;


(*
   Case := CaseLabelList ':' StatementSequence 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 807 "bnf/m2-2.bnf"
PROCEDURE Case (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 807 "bnf/m2-2.bnf"
BEGIN
# 807 "bnf/m2-2.bnf"
   CaseLabelList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 807 "bnf/m2-2.bnf"
   Expect(colontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 807 "bnf/m2-2.bnf"
   StatementSequence(stopset0, stopset1, stopset2) ;
END Case ;


(*
   WhileStatement := 'WHILE' Expression 'DO' StatementSequence 
                     'END' 

   first  symbols:whiletok
   
   cannot reachend
*)

# 809 "bnf/m2-2.bnf"
PROCEDURE WhileStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 809 "bnf/m2-2.bnf"
BEGIN
# 809 "bnf/m2-2.bnf"
   Expect(whiletok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 809 "bnf/m2-2.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
# 809 "bnf/m2-2.bnf"
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 809 "bnf/m2-2.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 809 "bnf/m2-2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END WhileStatement ;


(*
   RepeatStatement := 'REPEAT' StatementSequence 'UNTIL' Expression 

   first  symbols:repeattok
   
   cannot reachend
*)

# 811 "bnf/m2-2.bnf"
PROCEDURE RepeatStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 811 "bnf/m2-2.bnf"
BEGIN
# 811 "bnf/m2-2.bnf"
   Expect(repeattok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok, untiltok}) ;
# 811 "bnf/m2-2.bnf"
   StatementSequence(stopset0, stopset1, stopset2 + SetOfStop2{untiltok}) ;
# 811 "bnf/m2-2.bnf"
   Expect(untiltok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 811 "bnf/m2-2.bnf"
   Expression(stopset0, stopset1, stopset2) ;
END RepeatStatement ;


(*
   ForStatement := 'FOR' Ident ':=' Expression 'TO' Expression 
                   [ 'BY' ConstExpression  ]'DO' StatementSequence 
                   'END' 

   first  symbols:fortok
   
   cannot reachend
*)

# 813 "bnf/m2-2.bnf"
PROCEDURE ForStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 813 "bnf/m2-2.bnf"
BEGIN
# 813 "bnf/m2-2.bnf"
   Expect(fortok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 813 "bnf/m2-2.bnf"
   Ident(stopset0 + SetOfStop0{becomestok}, stopset1, stopset2) ;
# 813 "bnf/m2-2.bnf"
   Expect(becomestok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 813 "bnf/m2-2.bnf"
   Expression(stopset0, stopset1, stopset2 + SetOfStop2{totok}) ;
# 813 "bnf/m2-2.bnf"
   Expect(totok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 814 "bnf/m2-2.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{bytok, dotok}, stopset2) ;
# 814 "bnf/m2-2.bnf"
   IF currenttoken=bytok
   THEN
      Expect(bytok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 814 "bnf/m2-2.bnf"
      ConstExpression(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
   END ;
# 814 "bnf/m2-2.bnf"
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 815 "bnf/m2-2.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 815 "bnf/m2-2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END ForStatement ;


(*
   LoopStatement := 'LOOP' StatementSequence 'END' 

   first  symbols:looptok
   
   cannot reachend
*)

# 817 "bnf/m2-2.bnf"
PROCEDURE LoopStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 817 "bnf/m2-2.bnf"
BEGIN
# 817 "bnf/m2-2.bnf"
   Expect(looptok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 817 "bnf/m2-2.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 817 "bnf/m2-2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END LoopStatement ;


(*
   WithStatement := 'WITH' Designator 'DO' StatementSequence 
                    'END' 

   first  symbols:withtok
   
   cannot reachend
*)

# 819 "bnf/m2-2.bnf"
PROCEDURE WithStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 819 "bnf/m2-2.bnf"
BEGIN
# 819 "bnf/m2-2.bnf"
   Expect(withtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 819 "bnf/m2-2.bnf"
   Designator(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
# 819 "bnf/m2-2.bnf"
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 819 "bnf/m2-2.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 819 "bnf/m2-2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END WithStatement ;


(*
   ProcedureDeclaration := ProcedureHeading 
                           % Assert(IsProcedure(OperandT(1)))  %
                           ';' ( ProcedureBlock 
                                 % Assert(IsProcedure(OperandT(1)))  %
                                 Ident  )
                           % EndBuildProcedure  %
                           

   first  symbols:proceduretok
   
   cannot reachend
*)

# 821 "bnf/m2-2.bnf"
PROCEDURE ProcedureDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 821 "bnf/m2-2.bnf"
BEGIN
# 821 "bnf/m2-2.bnf"
   ProcedureHeading(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 821 "bnf/m2-2.bnf"
   Assert(IsProcedure(OperandT(1)))  ;
# 822 "bnf/m2-2.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{consttok, proceduretok, moduletok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 822 "bnf/m2-2.bnf"
# 823 "bnf/m2-2.bnf"
   ProcedureBlock(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 823 "bnf/m2-2.bnf"
   Assert(IsProcedure(OperandT(1)))  ;
# 824 "bnf/m2-2.bnf"
   Ident(stopset0, stopset1, stopset2) ;
# 825 "bnf/m2-2.bnf"
   EndBuildProcedure  ;
END ProcedureDeclaration ;


(*
   ProcedureHeading := 'PROCEDURE' ( Ident 
                                     % StartBuildProcedure  %
                                     
                                     % Assert(IsProcedure(OperandT(1)))  %
                                     [ FormalParameters  ]
                                     % BuildProcedureHeading  %
                                      )

   first  symbols:proceduretok
   
   cannot reachend
*)

# 829 "bnf/m2-2.bnf"
PROCEDURE ProcedureHeading (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 829 "bnf/m2-2.bnf"
BEGIN
# 829 "bnf/m2-2.bnf"
   Expect(proceduretok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 830 "bnf/m2-2.bnf"
# 831 "bnf/m2-2.bnf"
   Ident(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 831 "bnf/m2-2.bnf"
   StartBuildProcedure  ;
# 832 "bnf/m2-2.bnf"
   Assert(IsProcedure(OperandT(1)))  ;
# 833 "bnf/m2-2.bnf"
   IF currenttoken=lparatok
   THEN
      FormalParameters(stopset0, stopset1, stopset2) ;
   END ;
# 834 "bnf/m2-2.bnf"
   BuildProcedureHeading  ;
END ProcedureHeading ;


(*
   ProcedureBlock := 
                     % Assert(IsProcedure(OperandT(1)))  %
                     { 
                       % Assert(IsProcedure(OperandT(1)))  %
                       Declaration 
                       % Assert(IsProcedure(OperandT(1)))  %
                        }'BEGIN' StatementSequence 'END' 
                     % Assert(IsProcedure(OperandT(1)))  %
                     

   first  symbols:begintok, moduletok, proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

# 841 "bnf/m2-2.bnf"
PROCEDURE ProcedureBlock (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 841 "bnf/m2-2.bnf"
BEGIN
# 841 "bnf/m2-2.bnf"
   Assert(IsProcedure(OperandT(1)))  ;
# 842 "bnf/m2-2.bnf"
   Assert(IsProcedure(OperandT(1)))  ;
# 843 "bnf/m2-2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Declaration(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
# 843 "bnf/m2-2.bnf"
      Assert(IsProcedure(OperandT(1)))  ;
   END (* while *) ;
# 844 "bnf/m2-2.bnf"
   Expect(begintok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 844 "bnf/m2-2.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 844 "bnf/m2-2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
# 844 "bnf/m2-2.bnf"
   Assert(IsProcedure(OperandT(1)))  ;
END ProcedureBlock ;


(*
   Block := { Declaration  }[ 'BEGIN' StatementSequence  ]'END' 

   first  symbols:endtok, begintok, moduletok, proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

# 847 "bnf/m2-2.bnf"
PROCEDURE Block (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 847 "bnf/m2-2.bnf"
BEGIN
# 847 "bnf/m2-2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Declaration(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 847 "bnf/m2-2.bnf"
   IF currenttoken=begintok
   THEN
      Expect(begintok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 847 "bnf/m2-2.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   END ;
# 847 "bnf/m2-2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
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

# 849 "bnf/m2-2.bnf"
PROCEDURE Declaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 849 "bnf/m2-2.bnf"
BEGIN
# 849 "bnf/m2-2.bnf"
   IF currenttoken=consttok
   THEN
      Expect(consttok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 849 "bnf/m2-2.bnf"
      WHILE currenttoken=identtok DO
         ConstantDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 849 "bnf/m2-2.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 850 "bnf/m2-2.bnf"
   ELSIF currenttoken=typetok
   THEN
      Expect(typetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 850 "bnf/m2-2.bnf"
      WHILE currenttoken=identtok DO
         TypeDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 850 "bnf/m2-2.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 851 "bnf/m2-2.bnf"
   ELSIF currenttoken=vartok
   THEN
      Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 851 "bnf/m2-2.bnf"
      WHILE currenttoken=identtok DO
         VariableDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 851 "bnf/m2-2.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 852 "bnf/m2-2.bnf"
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 852 "bnf/m2-2.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
# 853 "bnf/m2-2.bnf"
   ELSIF currenttoken=moduletok
   THEN
      ModuleDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 853 "bnf/m2-2.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: MODULE PROCEDURE VAR TYPE CONST')
   END ;
END Declaration ;


(*
   FormalParameters := '(' 
                       % StartBuildFormalParameters   %
                       [ FPSection { ';' FPSection  } ]')' 
                       % EndBuildFormalParameters  %
                       [ ':' Qualident 
                         % BuildFunction  %
                          ]

   first  symbols:lparatok
   
   cannot reachend
*)

# 855 "bnf/m2-2.bnf"
PROCEDURE FormalParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 855 "bnf/m2-2.bnf"
BEGIN
# 855 "bnf/m2-2.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{rparatok}, stopset1, stopset2 + SetOfStop2{identtok, vartok}) ;
# 855 "bnf/m2-2.bnf"
   StartBuildFormalParameters   ;
# 856 "bnf/m2-2.bnf"
   IF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, identtok}))
   THEN
      FPSection(stopset0 + SetOfStop0{semicolontok, rparatok}, stopset1, stopset2) ;
# 856 "bnf/m2-2.bnf"
      WHILE currenttoken=semicolontok DO
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok, vartok}) ;
# 856 "bnf/m2-2.bnf"
         FPSection(stopset0 + SetOfStop0{rparatok, semicolontok}, stopset1, stopset2) ;
      END (* while *) ;
   END ;
# 856 "bnf/m2-2.bnf"
   Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 856 "bnf/m2-2.bnf"
   EndBuildFormalParameters  ;
# 857 "bnf/m2-2.bnf"
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 857 "bnf/m2-2.bnf"
      Qualident(stopset0, stopset1, stopset2) ;
# 857 "bnf/m2-2.bnf"
      BuildFunction  ;
   END ;
END FormalParameters ;


(*
   FPSection := NonVarFPSection  | VarFPSection 

   first  symbols:vartok, identtok
   
   cannot reachend
*)

# 860 "bnf/m2-2.bnf"
PROCEDURE FPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 860 "bnf/m2-2.bnf"
BEGIN
# 860 "bnf/m2-2.bnf"
   IF currenttoken=identtok
   THEN
      NonVarFPSection(stopset0, stopset1, stopset2) ;
# 860 "bnf/m2-2.bnf"
   ELSIF currenttoken=vartok
   THEN
      VarFPSection(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: VAR identifier')
   END ;
END FPSection ;


(*
   VarFPSection := 'VAR' 
                   % VAR n: CARDINAL ;  %
                   
                   % PopT(n) ;  %
                   
                   % PushT(VarTok) ;  %
                   IdentList ':' FormalType 
                   % PushT(n)  %
                   
                   % BuildFPSection  %
                   

   first  symbols:vartok
   
   cannot reachend
*)

# 862 "bnf/m2-2.bnf"
PROCEDURE VarFPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
 n: CARDINAL ; 
# 862 "bnf/m2-2.bnf"
BEGIN
# 862 "bnf/m2-2.bnf"
   Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 862 "bnf/m2-2.bnf"
# 863 "bnf/m2-2.bnf"
   PopT(n) ;  ;
# 864 "bnf/m2-2.bnf"
   PushT(VarTok) ;  ;
# 865 "bnf/m2-2.bnf"
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 865 "bnf/m2-2.bnf"
   Expect(colontok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 865 "bnf/m2-2.bnf"
   FormalType(stopset0, stopset1, stopset2) ;
# 865 "bnf/m2-2.bnf"
   PushT(n)  ;
# 866 "bnf/m2-2.bnf"
   BuildFPSection  ;
END VarFPSection ;


(*
   NonVarFPSection := 
                      % VAR n: CARDINAL ;  %
                      
                      % PopT(n)  %
                      
                      % PushT(NulTok)  %
                      IdentList ':' FormalType 
                      % PushT(n)  %
                      
                      % BuildFPSection  %
                      

   first  symbols:identtok
   
   cannot reachend
*)

# 869 "bnf/m2-2.bnf"
PROCEDURE NonVarFPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
 n: CARDINAL ; 
# 869 "bnf/m2-2.bnf"
BEGIN
# 869 "bnf/m2-2.bnf"
# 870 "bnf/m2-2.bnf"
   PopT(n)  ;
# 871 "bnf/m2-2.bnf"
   PushT(NulTok)  ;
# 872 "bnf/m2-2.bnf"
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 872 "bnf/m2-2.bnf"
   Expect(colontok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 872 "bnf/m2-2.bnf"
   FormalType(stopset0, stopset1, stopset2) ;
# 872 "bnf/m2-2.bnf"
   PushT(n)  ;
# 873 "bnf/m2-2.bnf"
   BuildFPSection  ;
END NonVarFPSection ;


(*
   FormalType := 'ARRAY' 'OF' 
                 % PushT(ArrayTok)  %
                 Qualident  | 
                 % VAR Sym, Type: CARDINAL ;  %
                 Qualident 
                 % PopTF(Sym, Type) ;
                   PushT(NulTok) ;
                   PushTF(Sym, Type)  %
                 

   first  symbols:identtok, arraytok
   
   cannot reachend
*)

# 876 "bnf/m2-2.bnf"
PROCEDURE FormalType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
 Sym, Type: CARDINAL ; 
# 876 "bnf/m2-2.bnf"
BEGIN
# 876 "bnf/m2-2.bnf"
   IF currenttoken=arraytok
   THEN
      Expect(arraytok, stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 876 "bnf/m2-2.bnf"
      Expect(oftok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 876 "bnf/m2-2.bnf"
      PushT(ArrayTok)  ;
# 878 "bnf/m2-2.bnf"
      Qualident(stopset0, stopset1, stopset2) ;
# 878 "bnf/m2-2.bnf"
   ELSE
# 878 "bnf/m2-2.bnf"
# 880 "bnf/m2-2.bnf"
      Qualident(stopset0, stopset1, stopset2) ;
# 880 "bnf/m2-2.bnf"
# 882 "bnf/m2-2.bnf"
      PopTF(Sym, Type) ;
      PushT(NulTok) ;
      PushTF(Sym, Type)  ;
   END ;
END FormalType ;


(*
   ModuleDeclaration := 'MODULE' Ident 
                        % StartBuildInnerModule  %
                        [ Priority 
                          % BuildPriority  %
                           ]';' { Import 
                                  % BuildImportInnerModule  %
                                   }[ Export 
                                      % BuildExportInnerModule  %
                                       ]Block Ident 
                        % EndBuildInnerModule  %
                        

   first  symbols:moduletok
   
   cannot reachend
*)

# 885 "bnf/m2-2.bnf"
PROCEDURE ModuleDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 885 "bnf/m2-2.bnf"
BEGIN
# 885 "bnf/m2-2.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 886 "bnf/m2-2.bnf"
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
# 886 "bnf/m2-2.bnf"
   StartBuildInnerModule  ;
# 887 "bnf/m2-2.bnf"
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 887 "bnf/m2-2.bnf"
      BuildPriority  ;
   END ;
# 888 "bnf/m2-2.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, exporttok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 889 "bnf/m2-2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, exporttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
# 889 "bnf/m2-2.bnf"
      BuildImportInnerModule  ;
   END (* while *) ;
# 890 "bnf/m2-2.bnf"
   IF currenttoken=exporttok
   THEN
      Export(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
# 890 "bnf/m2-2.bnf"
      BuildExportInnerModule  ;
   END ;
# 893 "bnf/m2-2.bnf"
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 893 "bnf/m2-2.bnf"
   Ident(stopset0, stopset1, stopset2) ;
# 893 "bnf/m2-2.bnf"
   EndBuildInnerModule  ;
END ModuleDeclaration ;


(*
   Priority := '[' ConstExpression ']' 

   first  symbols:lsbratok
   
   cannot reachend
*)

# 896 "bnf/m2-2.bnf"
PROCEDURE Priority (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 896 "bnf/m2-2.bnf"
BEGIN
# 896 "bnf/m2-2.bnf"
   Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 896 "bnf/m2-2.bnf"
   ConstExpression(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 896 "bnf/m2-2.bnf"
   Expect(rsbratok, stopset0, stopset1, stopset2) ;
END Priority ;


(*
   Export := 'EXPORT' ( 'QUALIFIED' 
                        % PushT(QualifiedTok)  %
                        IdentList  | 'UNQUALIFIED' 
                        % PushT(UnQualifiedTok)  %
                        IdentList  | 
                        
                        % PushT(ExportTok)  %
                        IdentList  )';' 

   first  symbols:exporttok
   
   cannot reachend
*)

# 898 "bnf/m2-2.bnf"
PROCEDURE Export (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 898 "bnf/m2-2.bnf"
BEGIN
# 898 "bnf/m2-2.bnf"
   Expect(exporttok, stopset0, stopset1 + SetOfStop1{qualifiedtok, unqualifiedtok}, stopset2 + SetOfStop2{identtok}) ;
# 898 "bnf/m2-2.bnf"
   IF currenttoken=qualifiedtok
   THEN
      Expect(qualifiedtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 898 "bnf/m2-2.bnf"
      PushT(QualifiedTok)  ;
# 899 "bnf/m2-2.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 900 "bnf/m2-2.bnf"
   ELSIF currenttoken=unqualifiedtok
   THEN
      Expect(unqualifiedtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 900 "bnf/m2-2.bnf"
      PushT(UnQualifiedTok)  ;
# 901 "bnf/m2-2.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 901 "bnf/m2-2.bnf"
   ELSE
# 901 "bnf/m2-2.bnf"
      PushT(ExportTok)  ;
# 902 "bnf/m2-2.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
# 902 "bnf/m2-2.bnf"
   Expect(semicolontok, stopset0, stopset1, stopset2) ;
END Export ;


(*
   Import := 'FROM' Ident 'IMPORT' IdentList ';'  | 
             'IMPORT' 
             % PushT(ImportTok)
                                                                                            (* determines whether Ident or Module *)  %
             IdentList ';' 

   first  symbols:importtok, fromtok
   
   cannot reachend
*)

# 904 "bnf/m2-2.bnf"
PROCEDURE Import (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 904 "bnf/m2-2.bnf"
BEGIN
# 904 "bnf/m2-2.bnf"
   IF currenttoken=fromtok
   THEN
      Expect(fromtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 904 "bnf/m2-2.bnf"
      Ident(stopset0, stopset1 + SetOfStop1{importtok}, stopset2) ;
# 904 "bnf/m2-2.bnf"
      Expect(importtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 904 "bnf/m2-2.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 904 "bnf/m2-2.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
# 905 "bnf/m2-2.bnf"
   ELSIF currenttoken=importtok
   THEN
      Expect(importtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 905 "bnf/m2-2.bnf"
# 906 "bnf/m2-2.bnf"
      PushT(ImportTok)
                                                                                   (* determines whether Ident or Module *)  ;
# 907 "bnf/m2-2.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 907 "bnf/m2-2.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: IMPORT FROM')
   END ;
END Import ;


(*
   DefinitionModule := 'DEFINITION' 'MODULE' Ident 
                       % P2StartBuildDefModule  %
                       ';' { Import 
                             % BuildImportOuterModule  %
                              }[ Export 
                                 % BuildExportOuterModule  %
                                  ]{ Definition  }'END' Ident 
                       
                       % P2EndBuildDefModule  %
                       '.' 

   first  symbols:definitiontok
   
   cannot reachend
*)

# 909 "bnf/m2-2.bnf"
PROCEDURE DefinitionModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 909 "bnf/m2-2.bnf"
BEGIN
# 909 "bnf/m2-2.bnf"
   Expect(definitiontok, stopset0, stopset1 + SetOfStop1{moduletok}, stopset2) ;
# 909 "bnf/m2-2.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 910 "bnf/m2-2.bnf"
   Ident(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 910 "bnf/m2-2.bnf"
   P2StartBuildDefModule  ;
# 911 "bnf/m2-2.bnf"
   Expect(semicolontok, stopset0, stopset1 + SetOfStop1{fromtok, importtok, exporttok, consttok, proceduretok, endtok}, stopset2 + SetOfStop2{typetok, vartok}) ;
# 912 "bnf/m2-2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok, exporttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
# 912 "bnf/m2-2.bnf"
      BuildImportOuterModule  ;
   END (* while *) ;
# 913 "bnf/m2-2.bnf"
   IF currenttoken=exporttok
   THEN
      Export(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
# 913 "bnf/m2-2.bnf"
      BuildExportOuterModule  ;
   END ;
# 915 "bnf/m2-2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Definition(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 916 "bnf/m2-2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 916 "bnf/m2-2.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 916 "bnf/m2-2.bnf"
   P2EndBuildDefModule  ;
# 917 "bnf/m2-2.bnf"
   Expect(periodtok, stopset0, stopset1, stopset2) ;
END DefinitionModule ;


(*
   Definition := 'CONST' { ConstantDeclaration ';'  } | 
                 'TYPE' { Ident ( ';'  | '=' Type ';'  )
                          % BuildTypeEnd  %
                           } | 
                 'VAR' { VariableDeclaration ';'  } | 
                 ProcedureHeading ';' 

   first  symbols:proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

# 920 "bnf/m2-2.bnf"
PROCEDURE Definition (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 920 "bnf/m2-2.bnf"
BEGIN
# 920 "bnf/m2-2.bnf"
   IF currenttoken=consttok
   THEN
      Expect(consttok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 920 "bnf/m2-2.bnf"
      WHILE currenttoken=identtok DO
         ConstantDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 920 "bnf/m2-2.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 921 "bnf/m2-2.bnf"
   ELSIF currenttoken=typetok
   THEN
      Expect(typetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 922 "bnf/m2-2.bnf"
      WHILE currenttoken=identtok DO
         Ident(stopset0 + SetOfStop0{semicolontok, equaltok}, stopset1, stopset2) ;
# 922 "bnf/m2-2.bnf"
         IF currenttoken=semicolontok
         THEN
            Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 923 "bnf/m2-2.bnf"
         ELSIF currenttoken=equaltok
         THEN
            Expect(equaltok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 923 "bnf/m2-2.bnf"
            Type(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 923 "bnf/m2-2.bnf"
            Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
         ELSE
            ErrorArray('expecting one of: = ;')
         END ;
# 923 "bnf/m2-2.bnf"
         BuildTypeEnd  ;
      END (* while *) ;
# 926 "bnf/m2-2.bnf"
   ELSIF currenttoken=vartok
   THEN
      Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 926 "bnf/m2-2.bnf"
      WHILE currenttoken=identtok DO
         VariableDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 926 "bnf/m2-2.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 927 "bnf/m2-2.bnf"
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureHeading(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 927 "bnf/m2-2.bnf"
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

# 929 "bnf/m2-2.bnf"
PROCEDURE AsmStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 929 "bnf/m2-2.bnf"
BEGIN
# 929 "bnf/m2-2.bnf"
   Expect(asmtok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2 + SetOfStop2{volatiletok}) ;
# 929 "bnf/m2-2.bnf"
   IF currenttoken=volatiletok
   THEN
      Expect(volatiletok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
   END ;
# 929 "bnf/m2-2.bnf"
   Expect(lparatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 929 "bnf/m2-2.bnf"
   AsmOperands(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 929 "bnf/m2-2.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END AsmStatement ;


(*
   AsmOperands := string [ ':' AsmList [ ':' AsmList [ ':' TrashList  ] ] ]

   first  symbols:stringtok
   
   cannot reachend
*)

# 931 "bnf/m2-2.bnf"
PROCEDURE AsmOperands (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 931 "bnf/m2-2.bnf"
BEGIN
# 931 "bnf/m2-2.bnf"
   string(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 931 "bnf/m2-2.bnf"
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0 + SetOfStop0{commatok, colontok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 931 "bnf/m2-2.bnf"
      AsmList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 931 "bnf/m2-2.bnf"
      IF currenttoken=colontok
      THEN
         Expect(colontok, stopset0 + SetOfStop0{commatok, colontok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 931 "bnf/m2-2.bnf"
         AsmList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 931 "bnf/m2-2.bnf"
         IF currenttoken=colontok
         THEN
            Expect(colontok, stopset0 + SetOfStop0{commatok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 931 "bnf/m2-2.bnf"
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

# 934 "bnf/m2-2.bnf"
PROCEDURE AsmList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 934 "bnf/m2-2.bnf"
BEGIN
# 934 "bnf/m2-2.bnf"
   IF currenttoken=stringtok
   THEN
      AsmElement(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END ;
# 934 "bnf/m2-2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 934 "bnf/m2-2.bnf"
      AsmElement(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END AsmList ;


(*
   AsmElement := string '(' Expression ')' 

   first  symbols:stringtok
   
   cannot reachend
*)

# 936 "bnf/m2-2.bnf"
PROCEDURE AsmElement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 936 "bnf/m2-2.bnf"
BEGIN
# 936 "bnf/m2-2.bnf"
   string(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 936 "bnf/m2-2.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 936 "bnf/m2-2.bnf"
   Expression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 936 "bnf/m2-2.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END AsmElement ;


(*
   TrashList := [ string  ]{ ',' string  }

   first  symbols:commatok, stringtok
   
   reachend
*)

# 939 "bnf/m2-2.bnf"
PROCEDURE TrashList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 939 "bnf/m2-2.bnf"
BEGIN
# 939 "bnf/m2-2.bnf"
   IF currenttoken=stringtok
   THEN
      string(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END ;
# 939 "bnf/m2-2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 939 "bnf/m2-2.bnf"
      string(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END TrashList ;



END P2Build.
