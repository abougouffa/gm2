(* it is advisable not to edit this file as it was automatically generated from the grammer file bnf/m2.bnf *)
# 21 "bnf/m2.bnf"

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
IMPLEMENTATION MODULE P1SyntaxCheck ;

FROM M2LexBuf IMPORT currentstring, currenttoken, GetToken, InsertToken, InsertTokenAndRewind, GetTokenNo ;
FROM M2Error IMPORT WriteFormat0, WriteFormat1, ErrorStringAt ;
FROM M2Quads IMPORT PushT, PushTF, IsAutoPushOn, PushAutoOff, PushAutoOn, PopAuto ;
FROM M2Reserved IMPORT tokToTok, toktype, NulTok, ImportTok, ExportTok, QualifiedTok, UnQualifiedTok ;
FROM NameKey IMPORT NulName ;
FROM StrLib IMPORT StrCopy, StrConCat, StrEqual ;
FROM P2SymBuild IMPORT BuildString, BuildNumber ;
FROM Strings IMPORT String, InitString, KillString, Mark, ConCat, ConCatChar ;
FROM M2Debug IMPORT Assert ;
FROM M2Printf IMPORT printf0 ;


(* imports for Pass1 *)
FROM M2Quads IMPORT PushT, PopT,
                    StartBuildInit,
                    EndBuildInit,
                    BuildProcedureStart,
                    BuildProcedureEnd,
                    BuildAssignment,
                    BuildInline ;

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

                       BuildEnumeration, BuildType,

                       BuildProcedureHeading,
                       StartBuildProcedure,
                       EndBuildProcedure ;

FROM SymbolTable IMPORT MakeGnuAsm, PutGnuAsmVolatile, PutGnuAsm, PutGnuAsmInput,
                        PutGnuAsmOutput, PutGnuAsmTrash, PutGnuAsmVolatile,
                        MakeRegInterface,
                        PutRegInterface, GetRegInterface,
                        GetSymName,
                        NulSym ;

CONST
   Debugging = FALSE ;
   Pass1     = TRUE ;
   Pass2     = FALSE ;          (* permanently disabled for the time being *)
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
# 115 "bnf/m2.bnf"


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
(*
      WriteFormat2('token number %d token was %a', GetTokenNo(), currentstring) ;
      FlushErrors ;
*)
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

# 481 "bnf/m2.bnf"
PROCEDURE FileUnit (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 481 "bnf/m2.bnf"
BEGIN
# 481 "bnf/m2.bnf"
   PushAutoOff  ;
# 482 "bnf/m2.bnf"
   IF currenttoken=definitiontok
   THEN
      DefinitionModule(stopset0, stopset1, stopset2) ;
# 483 "bnf/m2.bnf"
   ELSIF ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, implementationtok}))
   THEN
      ImplementationOrProgramModule(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: MODULE IMPLEMENTATION DEFINITION')
   END ;
# 483 "bnf/m2.bnf"
   PopAuto  ;
END FileUnit ;


(*
   ProgramModule := 'MODULE' 
                    % PushAutoOn ;  %
                    Ident 
                    % P1StartBuildProgramModule ;  %
                    
                    % PushAutoOff ;  %
                    [ Priority  ]';' 
                    % PushAutoOn ;  %
                    { Import 
                      % BuildImportOuterModule  %
                       }
                    % PopAuto  %
                    Block 
                    % PushAutoOn  %
                    Ident 
                    % P1EndBuildProgramModule  %
                    '.' 
                    % PopAuto ; PopAuto ; PopAuto  %
                    

   first  symbols:moduletok
   
   cannot reachend
*)

# 486 "bnf/m2.bnf"
PROCEDURE ProgramModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 486 "bnf/m2.bnf"
BEGIN
# 486 "bnf/m2.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 486 "bnf/m2.bnf"
   PushAutoOn ;  ;
# 487 "bnf/m2.bnf"
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
# 487 "bnf/m2.bnf"
   P1StartBuildProgramModule ;  ;
# 488 "bnf/m2.bnf"
   PushAutoOff ;  ;
# 489 "bnf/m2.bnf"
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
# 490 "bnf/m2.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 491 "bnf/m2.bnf"
   PushAutoOn ;  ;
# 492 "bnf/m2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
# 492 "bnf/m2.bnf"
      BuildImportOuterModule  ;
   END (* while *) ;
# 493 "bnf/m2.bnf"
   PopAuto  ;
# 495 "bnf/m2.bnf"
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 495 "bnf/m2.bnf"
   PushAutoOn  ;
# 496 "bnf/m2.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 496 "bnf/m2.bnf"
   P1EndBuildProgramModule  ;
# 497 "bnf/m2.bnf"
   Expect(periodtok, stopset0, stopset1, stopset2) ;
# 497 "bnf/m2.bnf"
   PopAuto ; PopAuto ; PopAuto  ;
END ProgramModule ;


(*
   ImplementationModule := 'IMPLEMENTATION' 'MODULE' 
                           % PushAutoOn ;  %
                           Ident 
                           % P1StartBuildImplementationModule ;  %
                           
                           % PushAutoOff ;  %
                           [ Priority  ]';' 
                           % PushAutoOn ;  %
                           { Import 
                             % BuildImportOuterModule  %
                              }
                           % PopAuto ;  %
                           Block 
                           % PushAutoOn ;  %
                           Ident 
                           % P1EndBuildImplementationModule  %
                           
                           % PopAuto ; PopAuto ; PopAuto ;  %
                           '.' 

   first  symbols:implementationtok
   
   cannot reachend
*)

# 500 "bnf/m2.bnf"
PROCEDURE ImplementationModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 500 "bnf/m2.bnf"
BEGIN
# 500 "bnf/m2.bnf"
   Expect(implementationtok, stopset0, stopset1 + SetOfStop1{moduletok}, stopset2) ;
# 500 "bnf/m2.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 500 "bnf/m2.bnf"
   PushAutoOn ;  ;
# 501 "bnf/m2.bnf"
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
# 501 "bnf/m2.bnf"
   P1StartBuildImplementationModule ;  ;
# 502 "bnf/m2.bnf"
   PushAutoOff ;  ;
# 503 "bnf/m2.bnf"
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
# 503 "bnf/m2.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 503 "bnf/m2.bnf"
   PushAutoOn ;  ;
# 504 "bnf/m2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
# 504 "bnf/m2.bnf"
      BuildImportOuterModule  ;
   END (* while *) ;
# 505 "bnf/m2.bnf"
   PopAuto ;  ;
# 506 "bnf/m2.bnf"
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 506 "bnf/m2.bnf"
   PushAutoOn ;  ;
# 508 "bnf/m2.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 508 "bnf/m2.bnf"
   P1EndBuildImplementationModule  ;
# 509 "bnf/m2.bnf"
   PopAuto ; PopAuto ; PopAuto ;  ;
# 510 "bnf/m2.bnf"
   Expect(periodtok, stopset0, stopset1, stopset2) ;
END ImplementationModule ;


(*
   ImplementationOrProgramModule := ImplementationModule  | 
                                    ProgramModule 

   first  symbols:moduletok, implementationtok
   
   cannot reachend
*)

# 512 "bnf/m2.bnf"
PROCEDURE ImplementationOrProgramModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 512 "bnf/m2.bnf"
BEGIN
# 512 "bnf/m2.bnf"
   IF currenttoken=implementationtok
   THEN
      ImplementationModule(stopset0, stopset1, stopset2) ;
# 512 "bnf/m2.bnf"
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

# 514 "bnf/m2.bnf"
PROCEDURE Number (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 514 "bnf/m2.bnf"
BEGIN
# 514 "bnf/m2.bnf"
   IF currenttoken=integertok
   THEN
      Integer(stopset0, stopset1, stopset2) ;
# 514 "bnf/m2.bnf"
   ELSIF currenttoken=realtok
   THEN
      Real(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: real number integer number')
   END ;
END Number ;


(*
   Qualident := Ident { '.' Ident  }

   first  symbols:identtok
   
   cannot reachend
*)

# 516 "bnf/m2.bnf"
PROCEDURE Qualident (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 516 "bnf/m2.bnf"
BEGIN
# 516 "bnf/m2.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 516 "bnf/m2.bnf"
   WHILE currenttoken=periodtok DO
      Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 516 "bnf/m2.bnf"
      Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
   END (* while *) ;
END Qualident ;


(*
   ConstantDeclaration := Ident '=' ConstExpression 

   first  symbols:identtok
   
   cannot reachend
*)

# 518 "bnf/m2.bnf"
PROCEDURE ConstantDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 518 "bnf/m2.bnf"
BEGIN
# 518 "bnf/m2.bnf"
   Ident(stopset0 + SetOfStop0{equaltok}, stopset1, stopset2) ;
# 518 "bnf/m2.bnf"
   Expect(equaltok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 518 "bnf/m2.bnf"
   ConstExpression(stopset0, stopset1, stopset2) ;
END ConstantDeclaration ;


(*
   ConstExpression := 
                      % PushAutoOff  %
                      SimpleConstExpr [ Relation SimpleConstExpr  ]
                      
                      % PopAuto  %
                      

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 520 "bnf/m2.bnf"
PROCEDURE ConstExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 520 "bnf/m2.bnf"
BEGIN
# 520 "bnf/m2.bnf"
   PushAutoOff  ;
# 521 "bnf/m2.bnf"
   SimpleConstExpr(stopset0 + SetOfStop0{equaltok, hashtok, lessgreatertok, lesstok, lessequaltok, greatertok, greaterequaltok}, stopset1 + SetOfStop1{intok}, stopset2) ;
# 521 "bnf/m2.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok})) OR
      (currenttoken=intok)
   THEN
      Relation(stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 521 "bnf/m2.bnf"
      SimpleConstExpr(stopset0, stopset1, stopset2) ;
   END ;
# 521 "bnf/m2.bnf"
   PopAuto  ;
END ConstExpression ;


(*
   Relation := '='  | '#'  | '<>'  | '<'  | '<='  | '>'  | 
               '>='  | 'IN' 

   first  symbols:intok, greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok
   
   cannot reachend
*)

# 524 "bnf/m2.bnf"
PROCEDURE Relation (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 524 "bnf/m2.bnf"
BEGIN
# 524 "bnf/m2.bnf"
   IF currenttoken=equaltok
   THEN
      Expect(equaltok, stopset0, stopset1, stopset2) ;
# 524 "bnf/m2.bnf"
   ELSIF currenttoken=hashtok
   THEN
      Expect(hashtok, stopset0, stopset1, stopset2) ;
# 524 "bnf/m2.bnf"
   ELSIF currenttoken=lessgreatertok
   THEN
      Expect(lessgreatertok, stopset0, stopset1, stopset2) ;
# 524 "bnf/m2.bnf"
   ELSIF currenttoken=lesstok
   THEN
      Expect(lesstok, stopset0, stopset1, stopset2) ;
# 524 "bnf/m2.bnf"
   ELSIF currenttoken=lessequaltok
   THEN
      Expect(lessequaltok, stopset0, stopset1, stopset2) ;
# 524 "bnf/m2.bnf"
   ELSIF currenttoken=greatertok
   THEN
      Expect(greatertok, stopset0, stopset1, stopset2) ;
# 524 "bnf/m2.bnf"
   ELSIF currenttoken=greaterequaltok
   THEN
      Expect(greaterequaltok, stopset0, stopset1, stopset2) ;
# 524 "bnf/m2.bnf"
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

# 526 "bnf/m2.bnf"
PROCEDURE SimpleConstExpr (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 526 "bnf/m2.bnf"
BEGIN
# 526 "bnf/m2.bnf"
   UnaryOrConstTerm(stopset0 + SetOfStop0{plustok, minustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
# 526 "bnf/m2.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok})) OR
         (currenttoken=ortok) DO
      AddOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 526 "bnf/m2.bnf"
      ConstTerm(stopset0 + SetOfStop0{minustok, plustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
   END (* while *) ;
END SimpleConstExpr ;


(*
   UnaryOrConstTerm := '+' ConstTerm  | '-' ConstTerm  | 
                       ConstTerm 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 528 "bnf/m2.bnf"
PROCEDURE UnaryOrConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 528 "bnf/m2.bnf"
BEGIN
# 528 "bnf/m2.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 528 "bnf/m2.bnf"
      ConstTerm(stopset0, stopset1, stopset2) ;
# 528 "bnf/m2.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 528 "bnf/m2.bnf"
      ConstTerm(stopset0, stopset1, stopset2) ;
# 528 "bnf/m2.bnf"
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

# 530 "bnf/m2.bnf"
PROCEDURE AddOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 530 "bnf/m2.bnf"
BEGIN
# 530 "bnf/m2.bnf"
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0, stopset1, stopset2) ;
# 530 "bnf/m2.bnf"
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0, stopset1, stopset2) ;
# 530 "bnf/m2.bnf"
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

# 532 "bnf/m2.bnf"
PROCEDURE ConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 532 "bnf/m2.bnf"
BEGIN
# 532 "bnf/m2.bnf"
   ConstFactor(stopset0 + SetOfStop0{timestok, dividetok, andtok, ambersandtok}, stopset1 + SetOfStop1{divtok, modtok}, stopset2) ;
# 532 "bnf/m2.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {ambersandtok, andtok, dividetok, timestok})) OR
         ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {modtok, divtok})) DO
      MulOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 532 "bnf/m2.bnf"
      ConstFactor(stopset0 + SetOfStop0{ambersandtok, andtok, dividetok, timestok}, stopset1 + SetOfStop1{modtok, divtok}, stopset2) ;
   END (* while *) ;
END ConstTerm ;


(*
   MulOperator := '*'  | '/'  | 'DIV'  | 'MOD'  | 'AND'  | 
                  '&' 

   first  symbols:ambersandtok, andtok, modtok, divtok, dividetok, timestok
   
   cannot reachend
*)

# 534 "bnf/m2.bnf"
PROCEDURE MulOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 534 "bnf/m2.bnf"
BEGIN
# 534 "bnf/m2.bnf"
   IF currenttoken=timestok
   THEN
      Expect(timestok, stopset0, stopset1, stopset2) ;
# 534 "bnf/m2.bnf"
   ELSIF currenttoken=dividetok
   THEN
      Expect(dividetok, stopset0, stopset1, stopset2) ;
# 534 "bnf/m2.bnf"
   ELSIF currenttoken=divtok
   THEN
      Expect(divtok, stopset0, stopset1, stopset2) ;
# 534 "bnf/m2.bnf"
   ELSIF currenttoken=modtok
   THEN
      Expect(modtok, stopset0, stopset1, stopset2) ;
# 534 "bnf/m2.bnf"
   ELSIF currenttoken=andtok
   THEN
      Expect(andtok, stopset0, stopset1, stopset2) ;
# 534 "bnf/m2.bnf"
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

# 536 "bnf/m2.bnf"
PROCEDURE ConstFactor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 536 "bnf/m2.bnf"
BEGIN
# 536 "bnf/m2.bnf"
   IF (currenttoken=lcbratok) OR
      (currenttoken=identtok)
   THEN
      ConstQualidentOrSet(stopset0, stopset1, stopset2) ;
# 536 "bnf/m2.bnf"
   ELSIF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {realtok, integertok}))
   THEN
      Number(stopset0, stopset1, stopset2) ;
# 536 "bnf/m2.bnf"
   ELSIF currenttoken=stringtok
   THEN
      ConstString(stopset0, stopset1, stopset2) ;
# 537 "bnf/m2.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 537 "bnf/m2.bnf"
      ConstExpression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 537 "bnf/m2.bnf"
      Expect(rparatok, stopset0, stopset1, stopset2) ;
# 537 "bnf/m2.bnf"
   ELSIF currenttoken=nottok
   THEN
      Expect(nottok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 537 "bnf/m2.bnf"
      ConstFactor(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: NOT ( string real number integer number identifier {')
   END ;
END ConstFactor ;


(*
   ConstString := string 

   first  symbols:stringtok
   
   cannot reachend
*)

# 541 "bnf/m2.bnf"
PROCEDURE ConstString (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 541 "bnf/m2.bnf"
BEGIN
# 541 "bnf/m2.bnf"
   string(stopset0, stopset1, stopset2) ;
END ConstString ;


(*
   ConstQualidentOrSet := SimpleSet  | Qualident [ SimpleSet  ]

   first  symbols:identtok, lcbratok
   
   cannot reachend
*)

# 543 "bnf/m2.bnf"
PROCEDURE ConstQualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 543 "bnf/m2.bnf"
BEGIN
# 543 "bnf/m2.bnf"
   IF currenttoken=lcbratok
   THEN
      SimpleSet(stopset0, stopset1, stopset2) ;
# 543 "bnf/m2.bnf"
   ELSIF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok}, stopset1, stopset2) ;
# 543 "bnf/m2.bnf"
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

# 545 "bnf/m2.bnf"
PROCEDURE QualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 545 "bnf/m2.bnf"
BEGIN
# 545 "bnf/m2.bnf"
   IF currenttoken=lcbratok
   THEN
      SimpleSet(stopset0, stopset1, stopset2) ;
# 545 "bnf/m2.bnf"
   ELSIF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok}, stopset1, stopset2) ;
# 545 "bnf/m2.bnf"
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

# 549 "bnf/m2.bnf"
PROCEDURE Element (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 549 "bnf/m2.bnf"
BEGIN
# 549 "bnf/m2.bnf"
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 549 "bnf/m2.bnf"
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 549 "bnf/m2.bnf"
      ConstExpression(stopset0, stopset1, stopset2) ;
   END ;
END Element ;


(*
   TypeDeclaration := 
                      % PushAutoOn  %
                      ( Ident '=' Type  )
                      % PopAuto     %
                      

   first  symbols:identtok
   
   cannot reachend
*)

# 552 "bnf/m2.bnf"
PROCEDURE TypeDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 552 "bnf/m2.bnf"
BEGIN
# 552 "bnf/m2.bnf"
   PushAutoOn  ;
# 553 "bnf/m2.bnf"
   Ident(stopset0 + SetOfStop0{equaltok}, stopset1, stopset2) ;
# 553 "bnf/m2.bnf"
   Expect(equaltok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 553 "bnf/m2.bnf"
   Type(stopset0, stopset1, stopset2) ;
# 553 "bnf/m2.bnf"
   PopAuto     ;
END TypeDeclaration ;


(*
   Type := 
           % VAR Name: CARDINAL ;  %
           
           % PushAutoOff  %
           ( SimpleType  | ArrayType  | RecordType  | 
             SetType  | PointerType  | ProcedureType  )
           % PopAuto  %
           
           % PopT(Name)  (* remove TYPE name from stack *)  %
           

   first  symbols:proceduretok, pointertok, settok, recordtok, arraytok, lsbratok, lparatok, identtok
   
   cannot reachend
*)

# 556 "bnf/m2.bnf"
PROCEDURE Type (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
 Name: CARDINAL ; 
# 556 "bnf/m2.bnf"
BEGIN
# 556 "bnf/m2.bnf"
# 557 "bnf/m2.bnf"
   PushAutoOff  ;
# 558 "bnf/m2.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lsbratok, lparatok})) OR
      (currenttoken=identtok)
   THEN
      SimpleType(stopset0, stopset1, stopset2) ;
# 558 "bnf/m2.bnf"
   ELSIF currenttoken=arraytok
   THEN
      ArrayType(stopset0, stopset1, stopset2) ;
# 558 "bnf/m2.bnf"
   ELSIF currenttoken=recordtok
   THEN
      RecordType(stopset0, stopset1, stopset2) ;
# 558 "bnf/m2.bnf"
   ELSIF currenttoken=settok
   THEN
      SetType(stopset0, stopset1, stopset2) ;
# 559 "bnf/m2.bnf"
   ELSIF currenttoken=pointertok
   THEN
      PointerType(stopset0, stopset1, stopset2) ;
# 559 "bnf/m2.bnf"
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureType(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: PROCEDURE POINTER SET RECORD ARRAY [ ( identifier')
   END ;
# 559 "bnf/m2.bnf"
   PopAuto  ;
# 560 "bnf/m2.bnf"
   PopT(Name)  (* remove TYPE name from stack *)  ;
END Type ;


(*
   SimpleType := ( Qualident  | Enumeration 
                   % BuildType  %
                    | SubrangeType  )

   first  symbols:lsbratok, lparatok, identtok
   
   cannot reachend
*)

# 564 "bnf/m2.bnf"
PROCEDURE SimpleType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 564 "bnf/m2.bnf"
BEGIN
# 564 "bnf/m2.bnf"
   IF currenttoken=identtok
   THEN
      Qualident(stopset0, stopset1, stopset2) ;
# 564 "bnf/m2.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Enumeration(stopset0, stopset1, stopset2) ;
# 564 "bnf/m2.bnf"
      BuildType  ;
# 565 "bnf/m2.bnf"
   ELSIF currenttoken=lsbratok
   THEN
      SubrangeType(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: [ ( identifier')
   END ;
END SimpleType ;


(*
   Enumeration := '(' 
                  % PushAutoOn  %
                  ( IdentList 
                    % BuildEnumeration  %
                     )
                  % PopAuto  %
                  ')' 

   first  symbols:lparatok
   
   cannot reachend
*)

# 568 "bnf/m2.bnf"
PROCEDURE Enumeration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 568 "bnf/m2.bnf"
BEGIN
# 568 "bnf/m2.bnf"
   Expect(lparatok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 568 "bnf/m2.bnf"
   PushAutoOn  ;
# 569 "bnf/m2.bnf"
   IdentList(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 569 "bnf/m2.bnf"
   BuildEnumeration  ;
# 570 "bnf/m2.bnf"
   PopAuto  ;
# 571 "bnf/m2.bnf"
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

# 573 "bnf/m2.bnf"
PROCEDURE IdentList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR

                                                                                on: BOOLEAN ;
                                                                                n : CARDINAL ; 
# 573 "bnf/m2.bnf"
BEGIN
# 573 "bnf/m2.bnf"
   Ident(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 573 "bnf/m2.bnf"
# 576 "bnf/m2.bnf"
# 580 "bnf/m2.bnf"
   on := IsAutoPushOn() ;
   IF on
   THEN
      n := 1
   END  ;
# 581 "bnf/m2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 581 "bnf/m2.bnf"
      Ident(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 581 "bnf/m2.bnf"
# 584 "bnf/m2.bnf"
      IF on
      THEN
         INC(n)
      END  ;
   END (* while *) ;
# 585 "bnf/m2.bnf"
# 588 "bnf/m2.bnf"
   IF on
   THEN
      PushT(n)
   END  ;
END IdentList ;


(*
   SubrangeType := '[' ConstExpression '..' ConstExpression ']' 

   first  symbols:lsbratok
   
   cannot reachend
*)

# 591 "bnf/m2.bnf"
PROCEDURE SubrangeType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 591 "bnf/m2.bnf"
BEGIN
# 591 "bnf/m2.bnf"
   Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 591 "bnf/m2.bnf"
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 591 "bnf/m2.bnf"
   Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 591 "bnf/m2.bnf"
   ConstExpression(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 591 "bnf/m2.bnf"
   Expect(rsbratok, stopset0, stopset1, stopset2) ;
END SubrangeType ;


(*
   ArrayType := 'ARRAY' SimpleType { ',' SimpleType  }'OF' 
                % BuildNulName  %
                Type 

   first  symbols:arraytok
   
   cannot reachend
*)

# 593 "bnf/m2.bnf"
PROCEDURE ArrayType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 593 "bnf/m2.bnf"
BEGIN
# 593 "bnf/m2.bnf"
   Expect(arraytok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 593 "bnf/m2.bnf"
   SimpleType(stopset0 + SetOfStop0{commatok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 593 "bnf/m2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 593 "bnf/m2.bnf"
      SimpleType(stopset0 + SetOfStop0{commatok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
   END (* while *) ;
# 593 "bnf/m2.bnf"
   Expect(oftok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 593 "bnf/m2.bnf"
   BuildNulName  ;
# 594 "bnf/m2.bnf"
   Type(stopset0, stopset1, stopset2) ;
END ArrayType ;


(*
   RecordType := 'RECORD' FieldListSequence 'END' 

   first  symbols:recordtok
   
   cannot reachend
*)

# 596 "bnf/m2.bnf"
PROCEDURE RecordType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 596 "bnf/m2.bnf"
BEGIN
# 596 "bnf/m2.bnf"
   Expect(recordtok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok, endtok}, stopset2 + SetOfStop2{identtok}) ;
# 596 "bnf/m2.bnf"
   FieldListSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 596 "bnf/m2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END RecordType ;


(*
   FieldListSequence := FieldListStatement { ';' FieldListStatement  }

   first  symbols:semicolontok, casetok, identtok
   
   reachend
*)

# 598 "bnf/m2.bnf"
PROCEDURE FieldListSequence (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 598 "bnf/m2.bnf"
BEGIN
# 598 "bnf/m2.bnf"
   FieldListStatement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 598 "bnf/m2.bnf"
   WHILE currenttoken=semicolontok DO
      Expect(semicolontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok}, stopset2 + SetOfStop2{identtok}) ;
# 598 "bnf/m2.bnf"
      FieldListStatement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END (* while *) ;
END FieldListSequence ;


(*
   FieldListStatement := [ FieldList  ]

   first  symbols:casetok, identtok
   
   reachend
*)

# 600 "bnf/m2.bnf"
PROCEDURE FieldListStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 600 "bnf/m2.bnf"
BEGIN
# 600 "bnf/m2.bnf"
   IF (currenttoken=casetok) OR
      (currenttoken=identtok)
   THEN
      FieldList(stopset0, stopset1, stopset2) ;
   END ;
END FieldListStatement ;


(*
   FieldList := IdentList ':' 
                % BuildNulName  %
                Type  | 'CASE' Ident [ ':' Qualident  | 
                                       '.' Qualident  ]'OF' Varient 
                { '|' Varient  }[ 'ELSE' FieldListSequence  ]
                'END' 

   first  symbols:casetok, identtok
   
   cannot reachend
*)

# 610 "bnf/m2.bnf"
PROCEDURE FieldList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 610 "bnf/m2.bnf"
BEGIN
# 610 "bnf/m2.bnf"
   IF currenttoken=identtok
   THEN
      IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 610 "bnf/m2.bnf"
      Expect(colontok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 610 "bnf/m2.bnf"
      BuildNulName  ;
# 611 "bnf/m2.bnf"
      Type(stopset0, stopset1, stopset2) ;
# 612 "bnf/m2.bnf"
   ELSIF currenttoken=casetok
   THEN
      Expect(casetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 612 "bnf/m2.bnf"
      Ident(stopset0 + SetOfStop0{colontok, periodtok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 612 "bnf/m2.bnf"
      IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {periodtok, colontok}))
      THEN
         (* seen optional [ | ] expression *)
# 612 "bnf/m2.bnf"
         IF currenttoken=colontok
         THEN
            Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 612 "bnf/m2.bnf"
            Qualident(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 612 "bnf/m2.bnf"
         ELSIF currenttoken=periodtok
         THEN
            Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 612 "bnf/m2.bnf"
            Qualident(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
         ELSE
            ErrorArray('expecting one of: . :')
         END ;
         (* end of optional [ | ] expression *)
      END ;
# 612 "bnf/m2.bnf"
      Expect(oftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 613 "bnf/m2.bnf"
      Varient(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{elsetok, endtok}, stopset2) ;
# 613 "bnf/m2.bnf"
      WHILE currenttoken=bartok DO
         Expect(bartok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 613 "bnf/m2.bnf"
         Varient(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{endtok, elsetok}, stopset2) ;
      END (* while *) ;
# 614 "bnf/m2.bnf"
      IF currenttoken=elsetok
      THEN
         Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok, endtok}, stopset2 + SetOfStop2{identtok}) ;
# 614 "bnf/m2.bnf"
         FieldListSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
      END ;
# 614 "bnf/m2.bnf"
      Expect(endtok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: CASE identifier')
   END ;
END FieldList ;


(*
   Varient := CaseLabelList ':' FieldListSequence 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 616 "bnf/m2.bnf"
PROCEDURE Varient (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 616 "bnf/m2.bnf"
BEGIN
# 616 "bnf/m2.bnf"
   CaseLabelList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 616 "bnf/m2.bnf"
   Expect(colontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok}, stopset2 + SetOfStop2{identtok}) ;
# 616 "bnf/m2.bnf"
   FieldListSequence(stopset0, stopset1, stopset2) ;
END Varient ;


(*
   CaseLabelList := CaseLabels { ',' CaseLabels  }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 618 "bnf/m2.bnf"
PROCEDURE CaseLabelList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 618 "bnf/m2.bnf"
BEGIN
# 618 "bnf/m2.bnf"
   CaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 618 "bnf/m2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 618 "bnf/m2.bnf"
      CaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END CaseLabelList ;


(*
   CaseLabels := ConstExpression [ '..' ConstExpression  ]

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 620 "bnf/m2.bnf"
PROCEDURE CaseLabels (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 620 "bnf/m2.bnf"
BEGIN
# 620 "bnf/m2.bnf"
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
# 620 "bnf/m2.bnf"
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 620 "bnf/m2.bnf"
      ConstExpression(stopset0, stopset1, stopset2) ;
   END ;
END CaseLabels ;


(*
   SetType := 'SET' 'OF' SimpleType 

   first  symbols:settok
   
   cannot reachend
*)

# 622 "bnf/m2.bnf"
PROCEDURE SetType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 622 "bnf/m2.bnf"
BEGIN
# 622 "bnf/m2.bnf"
   Expect(settok, stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 622 "bnf/m2.bnf"
   Expect(oftok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 622 "bnf/m2.bnf"
   SimpleType(stopset0, stopset1, stopset2) ;
END SetType ;


(*
   PointerType := 'POINTER' 'TO' 
                  % BuildNulName  %
                  Type 

   first  symbols:pointertok
   
   cannot reachend
*)

# 624 "bnf/m2.bnf"
PROCEDURE PointerType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 624 "bnf/m2.bnf"
BEGIN
# 624 "bnf/m2.bnf"
   Expect(pointertok, stopset0, stopset1, stopset2 + SetOfStop2{totok}) ;
# 624 "bnf/m2.bnf"
   Expect(totok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 624 "bnf/m2.bnf"
   BuildNulName  ;
# 625 "bnf/m2.bnf"
   Type(stopset0, stopset1, stopset2) ;
END PointerType ;


(*
   ProcedureType := 'PROCEDURE' [ FormalTypeList  ]

   first  symbols:proceduretok
   
   cannot reachend
*)

# 627 "bnf/m2.bnf"
PROCEDURE ProcedureType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 627 "bnf/m2.bnf"
BEGIN
# 627 "bnf/m2.bnf"
   Expect(proceduretok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 627 "bnf/m2.bnf"
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

# 629 "bnf/m2.bnf"
PROCEDURE FormalTypeList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 629 "bnf/m2.bnf"
BEGIN
# 629 "bnf/m2.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{rparatok, arraytok}, stopset1, stopset2 + SetOfStop2{vartok, identtok}) ;
# 629 "bnf/m2.bnf"
   IF currenttoken=rparatok
   THEN
      Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 629 "bnf/m2.bnf"
      FormalReturn(stopset0, stopset1, stopset2) ;
# 630 "bnf/m2.bnf"
   ELSIF (currenttoken=arraytok) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, vartok}))
   THEN
      ProcedureParameters(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 630 "bnf/m2.bnf"
      Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 630 "bnf/m2.bnf"
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

# 632 "bnf/m2.bnf"
PROCEDURE FormalReturn (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 632 "bnf/m2.bnf"
BEGIN
# 632 "bnf/m2.bnf"
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 632 "bnf/m2.bnf"
      Qualident(stopset0, stopset1, stopset2) ;
   END ;
END FormalReturn ;


(*
   ProcedureParameters := ProcedureParameter { ',' ProcedureParameter  }

   first  symbols:identtok, arraytok, vartok
   
   cannot reachend
*)

# 634 "bnf/m2.bnf"
PROCEDURE ProcedureParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 634 "bnf/m2.bnf"
BEGIN
# 634 "bnf/m2.bnf"
# 635 "bnf/m2.bnf"
   ProcedureParameter(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 635 "bnf/m2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{vartok, identtok}) ;
# 635 "bnf/m2.bnf"
      ProcedureParameter(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END ProcedureParameters ;


(*
   ProcedureParameter := 'VAR' FormalType  | 
                         FormalType 

   first  symbols:identtok, arraytok, vartok
   
   cannot reachend
*)

# 637 "bnf/m2.bnf"
PROCEDURE ProcedureParameter (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 637 "bnf/m2.bnf"
BEGIN
# 637 "bnf/m2.bnf"
   IF currenttoken=vartok
   THEN
      Expect(vartok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 637 "bnf/m2.bnf"
      FormalType(stopset0, stopset1, stopset2) ;
# 637 "bnf/m2.bnf"
   ELSIF (currenttoken=arraytok) OR
         (currenttoken=identtok)
   THEN
      FormalType(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: identifier ARRAY VAR')
   END ;
END ProcedureParameter ;


(*
   VariableDeclaration := ( IdentList ':' 
                            % BuildNulName  %
                            Type  )

   first  symbols:identtok
   
   cannot reachend
*)

# 640 "bnf/m2.bnf"
PROCEDURE VariableDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 640 "bnf/m2.bnf"
BEGIN
# 640 "bnf/m2.bnf"
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 640 "bnf/m2.bnf"
   Expect(colontok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 640 "bnf/m2.bnf"
   BuildNulName  ;
# 641 "bnf/m2.bnf"
   Type(stopset0, stopset1, stopset2) ;
END VariableDeclaration ;


(*
   Designator := Qualident { SubDesignator  }

   first  symbols:identtok
   
   cannot reachend
*)

# 644 "bnf/m2.bnf"
PROCEDURE Designator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 644 "bnf/m2.bnf"
BEGIN
# 644 "bnf/m2.bnf"
   Qualident(stopset0 + SetOfStop0{periodtok, lsbratok, uparrowtok}, stopset1, stopset2) ;
# 644 "bnf/m2.bnf"
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

# 646 "bnf/m2.bnf"
PROCEDURE SubDesignator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 646 "bnf/m2.bnf"
BEGIN
# 646 "bnf/m2.bnf"
   IF currenttoken=periodtok
   THEN
      Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 646 "bnf/m2.bnf"
      Ident(stopset0, stopset1, stopset2) ;
# 646 "bnf/m2.bnf"
   ELSIF currenttoken=lsbratok
   THEN
      Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rsbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 646 "bnf/m2.bnf"
      ExpList(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 646 "bnf/m2.bnf"
      Expect(rsbratok, stopset0, stopset1, stopset2) ;
# 646 "bnf/m2.bnf"
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

# 648 "bnf/m2.bnf"
PROCEDURE ExpList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 648 "bnf/m2.bnf"
BEGIN
# 648 "bnf/m2.bnf"
   Expression(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
# 648 "bnf/m2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 648 "bnf/m2.bnf"
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

# 650 "bnf/m2.bnf"
PROCEDURE Expression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 650 "bnf/m2.bnf"
BEGIN
# 650 "bnf/m2.bnf"
   PushAutoOff  ;
# 651 "bnf/m2.bnf"
   SimpleExpression(stopset0 + SetOfStop0{equaltok, hashtok, lessgreatertok, lesstok, lessequaltok, greatertok, greaterequaltok}, stopset1 + SetOfStop1{intok}, stopset2) ;
# 651 "bnf/m2.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok})) OR
      (currenttoken=intok)
   THEN
      Relation(stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 651 "bnf/m2.bnf"
      SimpleExpression(stopset0, stopset1, stopset2) ;
   END ;
# 651 "bnf/m2.bnf"
   PopAuto  ;
END Expression ;


(*
   SimpleExpression := [ '+'  | '-'  ]Term { AddOperator Term  }

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

# 654 "bnf/m2.bnf"
PROCEDURE SimpleExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 654 "bnf/m2.bnf"
BEGIN
# 654 "bnf/m2.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok}))
   THEN
      (* seen optional [ | ] expression *)
# 654 "bnf/m2.bnf"
      IF currenttoken=plustok
      THEN
         Expect(plustok, stopset0 + SetOfStop0{lparatok, lcbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, stringtok, realtok, integertok}) ;
# 654 "bnf/m2.bnf"
      ELSIF currenttoken=minustok
      THEN
         Expect(minustok, stopset0 + SetOfStop0{lparatok, lcbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, stringtok, realtok, integertok}) ;
      ELSE
         ErrorArray('expecting one of: - +')
      END ;
      (* end of optional [ | ] expression *)
   END ;
# 654 "bnf/m2.bnf"
   Term(stopset0 + SetOfStop0{plustok, minustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
# 654 "bnf/m2.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok})) OR
         (currenttoken=ortok) DO
      AddOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 654 "bnf/m2.bnf"
      Term(stopset0 + SetOfStop0{minustok, plustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
   END (* while *) ;
END SimpleExpression ;


(*
   Term := Factor { MulOperator Factor  }

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok
   
   cannot reachend
*)

# 656 "bnf/m2.bnf"
PROCEDURE Term (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 656 "bnf/m2.bnf"
BEGIN
# 656 "bnf/m2.bnf"
   Factor(stopset0 + SetOfStop0{timestok, dividetok, andtok, ambersandtok}, stopset1 + SetOfStop1{divtok, modtok}, stopset2) ;
# 656 "bnf/m2.bnf"
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {ambersandtok, andtok, dividetok, timestok})) OR
         ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {modtok, divtok})) DO
      MulOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 656 "bnf/m2.bnf"
      Factor(stopset0 + SetOfStop0{ambersandtok, andtok, dividetok, timestok}, stopset1 + SetOfStop1{modtok, divtok}, stopset2) ;
   END (* while *) ;
END Term ;


(*
   Factor := Number  | string  | SetOrDesignatorOrFunction  | 
             '(' Expression ')'  | 'NOT' Factor 

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok
   
   cannot reachend
*)

# 658 "bnf/m2.bnf"
PROCEDURE Factor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 658 "bnf/m2.bnf"
BEGIN
# 658 "bnf/m2.bnf"
   IF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {realtok, integertok}))
   THEN
      Number(stopset0, stopset1, stopset2) ;
# 658 "bnf/m2.bnf"
   ELSIF currenttoken=stringtok
   THEN
      string(stopset0, stopset1, stopset2) ;
# 658 "bnf/m2.bnf"
   ELSIF (currenttoken=lcbratok) OR
         (currenttoken=identtok)
   THEN
      SetOrDesignatorOrFunction(stopset0, stopset1, stopset2) ;
# 659 "bnf/m2.bnf"
   ELSIF currenttoken=lparatok
   THEN
      Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 659 "bnf/m2.bnf"
      Expression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 659 "bnf/m2.bnf"
      Expect(rparatok, stopset0, stopset1, stopset2) ;
# 659 "bnf/m2.bnf"
   ELSIF currenttoken=nottok
   THEN
      Expect(nottok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 659 "bnf/m2.bnf"
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

# 663 "bnf/m2.bnf"
PROCEDURE SimpleSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 663 "bnf/m2.bnf"
BEGIN
# 663 "bnf/m2.bnf"
   Expect(lcbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 663 "bnf/m2.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {commatok, lparatok, lcbratok, minustok, plustok})) OR
      (currenttoken=nottok) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {stringtok, realtok, integertok, identtok}))
   THEN
      Element(stopset0 + SetOfStop0{commatok, rcbratok}, stopset1, stopset2) ;
# 663 "bnf/m2.bnf"
      WHILE currenttoken=commatok DO
         Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok, commatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 663 "bnf/m2.bnf"
         Element(stopset0 + SetOfStop0{rcbratok, commatok}, stopset1, stopset2) ;
      END (* while *) ;
   END ;
# 663 "bnf/m2.bnf"
   Expect(rcbratok, stopset0, stopset1, stopset2) ;
END SimpleSet ;


(*
   SetOrDesignatorOrFunction := ( Qualident [ SimpleSet  | 
                                              SimpleDes [ ActualParameters  ] ] | 
                                  SimpleSet  )

   first  symbols:lcbratok, identtok
   
   cannot reachend
*)

# 665 "bnf/m2.bnf"
PROCEDURE SetOrDesignatorOrFunction (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 665 "bnf/m2.bnf"
BEGIN
# 665 "bnf/m2.bnf"
   IF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok, periodtok, lsbratok, uparrowtok, lparatok}, stopset1, stopset2) ;
# 665 "bnf/m2.bnf"
      IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, uparrowtok, lsbratok, periodtok, lcbratok}))
      THEN
         (* seen optional [ | ] expression *)
# 665 "bnf/m2.bnf"
         IF currenttoken=lcbratok
         THEN
            SimpleSet(stopset0, stopset1, stopset2) ;
# 666 "bnf/m2.bnf"
         ELSIF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, uparrowtok, lsbratok, periodtok}))
         THEN
            SimpleDes(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 666 "bnf/m2.bnf"
            IF currenttoken=lparatok
            THEN
               ActualParameters(stopset0, stopset1, stopset2) ;
            END ;
         ELSE
            ErrorArray('expecting one of: ( ^ [ . {')
         END ;
         (* end of optional [ | ] expression *)
      END ;
# 668 "bnf/m2.bnf"
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

# 671 "bnf/m2.bnf"
PROCEDURE SimpleDes (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 671 "bnf/m2.bnf"
BEGIN
# 671 "bnf/m2.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {uparrowtok, lsbratok, periodtok}))
   THEN
      (* seen optional { | } expression *)
      WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {uparrowtok, lsbratok, periodtok})) DO
# 671 "bnf/m2.bnf"
         IF currenttoken=periodtok
         THEN
            Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 671 "bnf/m2.bnf"
            Ident(stopset0 + SetOfStop0{uparrowtok, lsbratok, periodtok}, stopset1, stopset2) ;
# 671 "bnf/m2.bnf"
         ELSIF currenttoken=lsbratok
         THEN
            Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rsbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 671 "bnf/m2.bnf"
            ExpList(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 671 "bnf/m2.bnf"
            Expect(rsbratok, stopset0 + SetOfStop0{uparrowtok, lsbratok, periodtok}, stopset1, stopset2) ;
# 671 "bnf/m2.bnf"
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

# 673 "bnf/m2.bnf"
PROCEDURE ActualParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 673 "bnf/m2.bnf"
BEGIN
# 673 "bnf/m2.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 673 "bnf/m2.bnf"
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok, minustok, plustok})) OR
      (currenttoken=nottok) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, stringtok, realtok, integertok}))
   THEN
      ExpList(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
   END ;
# 673 "bnf/m2.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END ActualParameters ;


(*
   Statement := [ AssignmentOrProcedureCall  | 
                  IfStatement  | CaseStatement  | 
                  WhileStatement  | RepeatStatement  | 
                  LoopStatement  | ForStatement  | 
                  WithStatement  | AsmStatement  | 
                  'EXIT'  | 'RETURN' [ Expression  ] ]

   first  symbols:returntok, exittok, asmtok, withtok, fortok, looptok, repeattok, whiletok, casetok, iftok, identtok
   
   reachend
*)

# 675 "bnf/m2.bnf"
PROCEDURE Statement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 675 "bnf/m2.bnf"
BEGIN
# 675 "bnf/m2.bnf"
   IF ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {returntok, exittok, fortok, looptok, repeattok, casetok, iftok})) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {asmtok, withtok, whiletok, identtok}))
   THEN
      (* seen optional [ | ] expression *)
# 675 "bnf/m2.bnf"
      IF currenttoken=identtok
      THEN
         AssignmentOrProcedureCall(stopset0, stopset1, stopset2) ;
# 675 "bnf/m2.bnf"
      ELSIF currenttoken=iftok
      THEN
         IfStatement(stopset0, stopset1, stopset2) ;
# 675 "bnf/m2.bnf"
      ELSIF currenttoken=casetok
      THEN
         CaseStatement(stopset0, stopset1, stopset2) ;
# 676 "bnf/m2.bnf"
      ELSIF currenttoken=whiletok
      THEN
         WhileStatement(stopset0, stopset1, stopset2) ;
# 676 "bnf/m2.bnf"
      ELSIF currenttoken=repeattok
      THEN
         RepeatStatement(stopset0, stopset1, stopset2) ;
# 676 "bnf/m2.bnf"
      ELSIF currenttoken=looptok
      THEN
         LoopStatement(stopset0, stopset1, stopset2) ;
# 677 "bnf/m2.bnf"
      ELSIF currenttoken=fortok
      THEN
         ForStatement(stopset0, stopset1, stopset2) ;
# 677 "bnf/m2.bnf"
      ELSIF currenttoken=withtok
      THEN
         WithStatement(stopset0, stopset1, stopset2) ;
# 677 "bnf/m2.bnf"
      ELSIF currenttoken=asmtok
      THEN
         AsmStatement(stopset0, stopset1, stopset2) ;
# 678 "bnf/m2.bnf"
      ELSIF currenttoken=exittok
      THEN
         Expect(exittok, stopset0, stopset1, stopset2) ;
# 678 "bnf/m2.bnf"
      ELSIF currenttoken=returntok
      THEN
         Expect(returntok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 678 "bnf/m2.bnf"
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
END Statement ;


(*
   AssignmentOrProcedureCall := Designator ( ':=' Expression  | 
                                             ActualParameters  | 
                                             
                                             % (* epsilon *)  %
                                              )

   first  symbols:identtok
   
   cannot reachend
*)

# 680 "bnf/m2.bnf"
PROCEDURE AssignmentOrProcedureCall (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 680 "bnf/m2.bnf"
BEGIN
# 680 "bnf/m2.bnf"
   Designator(stopset0 + SetOfStop0{becomestok, lparatok}, stopset1, stopset2) ;
# 680 "bnf/m2.bnf"
   IF currenttoken=becomestok
   THEN
      Expect(becomestok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 680 "bnf/m2.bnf"
      Expression(stopset0, stopset1, stopset2) ;
# 681 "bnf/m2.bnf"
   ELSIF currenttoken=lparatok
   THEN
      ActualParameters(stopset0, stopset1, stopset2) ;
# 681 "bnf/m2.bnf"
   ELSE
# 681 "bnf/m2.bnf"
      (* epsilon *)  ;
   END ;
END AssignmentOrProcedureCall ;


(*
   StatementSequence := Statement { ';' Statement  }

   first  symbols:semicolontok, returntok, exittok, asmtok, withtok, fortok, looptok, repeattok, whiletok, casetok, iftok, identtok
   
   reachend
*)

# 688 "bnf/m2.bnf"
PROCEDURE StatementSequence (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 688 "bnf/m2.bnf"
BEGIN
# 688 "bnf/m2.bnf"
   Statement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 688 "bnf/m2.bnf"
   WHILE currenttoken=semicolontok DO
      Expect(semicolontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 688 "bnf/m2.bnf"
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

# 690 "bnf/m2.bnf"
PROCEDURE IfStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 690 "bnf/m2.bnf"
BEGIN
# 690 "bnf/m2.bnf"
   Expect(iftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok, thentok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 690 "bnf/m2.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{thentok}, stopset2) ;
# 690 "bnf/m2.bnf"
   Expect(thentok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, elsiftok, elsetok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 691 "bnf/m2.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{elsiftok, elsetok, endtok}, stopset2) ;
# 691 "bnf/m2.bnf"
   WHILE currenttoken=elsiftok DO
      Expect(elsiftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok, thentok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 691 "bnf/m2.bnf"
      Expression(stopset0, stopset1 + SetOfStop1{thentok}, stopset2) ;
# 691 "bnf/m2.bnf"
      Expect(thentok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok, elsetok, elsiftok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 691 "bnf/m2.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok, elsetok, elsiftok}, stopset2) ;
   END (* while *) ;
# 692 "bnf/m2.bnf"
   IF currenttoken=elsetok
   THEN
      Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 692 "bnf/m2.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   END ;
# 692 "bnf/m2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END IfStatement ;


(*
   CaseStatement := 'CASE' Expression 'OF' Case { '|' Case  }
                    [ 'ELSE' StatementSequence  ]'END' 

   first  symbols:casetok
   
   cannot reachend
*)

# 694 "bnf/m2.bnf"
PROCEDURE CaseStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 694 "bnf/m2.bnf"
BEGIN
# 694 "bnf/m2.bnf"
   Expect(casetok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok, oftok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 694 "bnf/m2.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 694 "bnf/m2.bnf"
   Expect(oftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 694 "bnf/m2.bnf"
   Case(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{elsetok, endtok}, stopset2) ;
# 694 "bnf/m2.bnf"
   WHILE currenttoken=bartok DO
      Expect(bartok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 694 "bnf/m2.bnf"
      Case(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{endtok, elsetok}, stopset2) ;
   END (* while *) ;
# 695 "bnf/m2.bnf"
   IF currenttoken=elsetok
   THEN
      Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 695 "bnf/m2.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   END ;
# 695 "bnf/m2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END CaseStatement ;


(*
   Case := CaseLabelList ':' StatementSequence 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

# 697 "bnf/m2.bnf"
PROCEDURE Case (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 697 "bnf/m2.bnf"
BEGIN
# 697 "bnf/m2.bnf"
   CaseLabelList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 697 "bnf/m2.bnf"
   Expect(colontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 697 "bnf/m2.bnf"
   StatementSequence(stopset0, stopset1, stopset2) ;
END Case ;


(*
   WhileStatement := 'WHILE' Expression 'DO' StatementSequence 
                     'END' 

   first  symbols:whiletok
   
   cannot reachend
*)

# 699 "bnf/m2.bnf"
PROCEDURE WhileStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 699 "bnf/m2.bnf"
BEGIN
# 699 "bnf/m2.bnf"
   Expect(whiletok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok, dotok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 699 "bnf/m2.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
# 699 "bnf/m2.bnf"
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 699 "bnf/m2.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 699 "bnf/m2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END WhileStatement ;


(*
   RepeatStatement := 'REPEAT' StatementSequence 'UNTIL' Expression 

   first  symbols:repeattok
   
   cannot reachend
*)

# 701 "bnf/m2.bnf"
PROCEDURE RepeatStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 701 "bnf/m2.bnf"
BEGIN
# 701 "bnf/m2.bnf"
   Expect(repeattok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok, untiltok}) ;
# 701 "bnf/m2.bnf"
   StatementSequence(stopset0, stopset1, stopset2 + SetOfStop2{untiltok}) ;
# 701 "bnf/m2.bnf"
   Expect(untiltok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 701 "bnf/m2.bnf"
   Expression(stopset0, stopset1, stopset2) ;
END RepeatStatement ;


(*
   ForStatement := 'FOR' Ident ':=' Expression 'TO' Expression 
                   [ 'BY' ConstExpression  ]'DO' StatementSequence 
                   'END' 

   first  symbols:fortok
   
   cannot reachend
*)

# 703 "bnf/m2.bnf"
PROCEDURE ForStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 703 "bnf/m2.bnf"
BEGIN
# 703 "bnf/m2.bnf"
   Expect(fortok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 703 "bnf/m2.bnf"
   Ident(stopset0 + SetOfStop0{becomestok}, stopset1, stopset2) ;
# 703 "bnf/m2.bnf"
   Expect(becomestok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok, totok}) ;
# 703 "bnf/m2.bnf"
   Expression(stopset0, stopset1, stopset2 + SetOfStop2{totok}) ;
# 703 "bnf/m2.bnf"
   Expect(totok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok, bytok, dotok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 704 "bnf/m2.bnf"
   Expression(stopset0, stopset1 + SetOfStop1{bytok, dotok}, stopset2) ;
# 704 "bnf/m2.bnf"
   IF currenttoken=bytok
   THEN
      Expect(bytok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 704 "bnf/m2.bnf"
      ConstExpression(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
   END ;
# 704 "bnf/m2.bnf"
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 705 "bnf/m2.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 705 "bnf/m2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END ForStatement ;


(*
   LoopStatement := 'LOOP' StatementSequence 'END' 

   first  symbols:looptok
   
   cannot reachend
*)

# 707 "bnf/m2.bnf"
PROCEDURE LoopStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 707 "bnf/m2.bnf"
BEGIN
# 707 "bnf/m2.bnf"
   Expect(looptok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 707 "bnf/m2.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 707 "bnf/m2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END LoopStatement ;


(*
   WithStatement := 'WITH' Designator 'DO' StatementSequence 
                    'END' 

   first  symbols:withtok
   
   cannot reachend
*)

# 709 "bnf/m2.bnf"
PROCEDURE WithStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 709 "bnf/m2.bnf"
BEGIN
# 709 "bnf/m2.bnf"
   Expect(withtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 709 "bnf/m2.bnf"
   Designator(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
# 709 "bnf/m2.bnf"
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 709 "bnf/m2.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 709 "bnf/m2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END WithStatement ;


(*
   ProcedureDeclaration := ProcedureHeading ';' ( ProcedureBlock 
                                                  
                                                  % PushAutoOn  %
                                                  Ident  )
                           % EndBuildProcedure  %
                           
                           % PopAuto  %
                           

   first  symbols:proceduretok
   
   cannot reachend
*)

# 711 "bnf/m2.bnf"
PROCEDURE ProcedureDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 711 "bnf/m2.bnf"
BEGIN
# 711 "bnf/m2.bnf"
   ProcedureHeading(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 711 "bnf/m2.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{consttok, proceduretok, moduletok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 711 "bnf/m2.bnf"
   ProcedureBlock(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 711 "bnf/m2.bnf"
   PushAutoOn  ;
# 712 "bnf/m2.bnf"
   Ident(stopset0, stopset1, stopset2) ;
# 712 "bnf/m2.bnf"
   EndBuildProcedure  ;
# 713 "bnf/m2.bnf"
   PopAuto  ;
END ProcedureDeclaration ;


(*
   ProcedureHeading := 'PROCEDURE' 
                       % PushAutoOn  %
                       ( Ident 
                         % StartBuildProcedure  %
                         
                         % PushAutoOff  %
                         [ FormalParameters  ]
                         % PopAuto  %
                         
                         % BuildProcedureHeading  %
                          )
                       % PopAuto  %
                       

   first  symbols:proceduretok
   
   cannot reachend
*)

# 716 "bnf/m2.bnf"
PROCEDURE ProcedureHeading (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 716 "bnf/m2.bnf"
BEGIN
# 716 "bnf/m2.bnf"
   Expect(proceduretok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 716 "bnf/m2.bnf"
   PushAutoOn  ;
# 717 "bnf/m2.bnf"
# 718 "bnf/m2.bnf"
   Ident(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 718 "bnf/m2.bnf"
   StartBuildProcedure  ;
# 719 "bnf/m2.bnf"
   PushAutoOff  ;
# 720 "bnf/m2.bnf"
   IF currenttoken=lparatok
   THEN
      FormalParameters(stopset0, stopset1, stopset2) ;
   END ;
# 721 "bnf/m2.bnf"
   PopAuto  ;
# 722 "bnf/m2.bnf"
   BuildProcedureHeading  ;
# 723 "bnf/m2.bnf"
   PopAuto  ;
END ProcedureHeading ;


(*
   ProcedureBlock := { Declaration  }'BEGIN' StatementSequence 
                     'END' 

   first  symbols:begintok, moduletok, proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

# 729 "bnf/m2.bnf"
PROCEDURE ProcedureBlock (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 729 "bnf/m2.bnf"
BEGIN
# 729 "bnf/m2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Declaration(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 729 "bnf/m2.bnf"
   Expect(begintok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 729 "bnf/m2.bnf"
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
# 729 "bnf/m2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2) ;
END ProcedureBlock ;


(*
   Block := { Declaration  }[ 'BEGIN' StatementSequence  ]'END' 

   first  symbols:endtok, begintok, moduletok, proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

# 731 "bnf/m2.bnf"
PROCEDURE Block (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 731 "bnf/m2.bnf"
BEGIN
# 731 "bnf/m2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Declaration(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 731 "bnf/m2.bnf"
   IF currenttoken=begintok
   THEN
      Expect(begintok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
# 731 "bnf/m2.bnf"
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   END ;
# 731 "bnf/m2.bnf"
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

# 733 "bnf/m2.bnf"
PROCEDURE Declaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 733 "bnf/m2.bnf"
BEGIN
# 733 "bnf/m2.bnf"
   IF currenttoken=consttok
   THEN
      Expect(consttok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 733 "bnf/m2.bnf"
      WHILE currenttoken=identtok DO
         ConstantDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 733 "bnf/m2.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 734 "bnf/m2.bnf"
   ELSIF currenttoken=typetok
   THEN
      Expect(typetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 734 "bnf/m2.bnf"
      WHILE currenttoken=identtok DO
         TypeDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 734 "bnf/m2.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 735 "bnf/m2.bnf"
   ELSIF currenttoken=vartok
   THEN
      Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 735 "bnf/m2.bnf"
      WHILE currenttoken=identtok DO
         VariableDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 735 "bnf/m2.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 736 "bnf/m2.bnf"
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 736 "bnf/m2.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
# 737 "bnf/m2.bnf"
   ELSIF currenttoken=moduletok
   THEN
      ModuleDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 737 "bnf/m2.bnf"
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

# 739 "bnf/m2.bnf"
PROCEDURE FormalParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 739 "bnf/m2.bnf"
BEGIN
# 739 "bnf/m2.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{rparatok}, stopset1, stopset2 + SetOfStop2{identtok, vartok}) ;
# 739 "bnf/m2.bnf"
   IF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, identtok}))
   THEN
      FPSection(stopset0 + SetOfStop0{semicolontok, rparatok}, stopset1, stopset2) ;
# 739 "bnf/m2.bnf"
      WHILE currenttoken=semicolontok DO
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok, vartok}) ;
# 739 "bnf/m2.bnf"
         FPSection(stopset0 + SetOfStop0{rparatok, semicolontok}, stopset1, stopset2) ;
      END (* while *) ;
   END ;
# 739 "bnf/m2.bnf"
   Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 740 "bnf/m2.bnf"
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 740 "bnf/m2.bnf"
      Qualident(stopset0, stopset1, stopset2) ;
   END ;
END FormalParameters ;


(*
   FPSection := NonVarFPSection  | VarFPSection 

   first  symbols:vartok, identtok
   
   cannot reachend
*)

# 742 "bnf/m2.bnf"
PROCEDURE FPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 742 "bnf/m2.bnf"
BEGIN
# 742 "bnf/m2.bnf"
   IF currenttoken=identtok
   THEN
      NonVarFPSection(stopset0, stopset1, stopset2) ;
# 742 "bnf/m2.bnf"
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

# 744 "bnf/m2.bnf"
PROCEDURE VarFPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 744 "bnf/m2.bnf"
BEGIN
# 744 "bnf/m2.bnf"
   Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 744 "bnf/m2.bnf"
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 744 "bnf/m2.bnf"
   Expect(colontok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 744 "bnf/m2.bnf"
   FormalType(stopset0, stopset1, stopset2) ;
END VarFPSection ;


(*
   NonVarFPSection := IdentList ':' FormalType 

   first  symbols:identtok
   
   cannot reachend
*)

# 746 "bnf/m2.bnf"
PROCEDURE NonVarFPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 746 "bnf/m2.bnf"
BEGIN
# 746 "bnf/m2.bnf"
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 746 "bnf/m2.bnf"
   Expect(colontok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 746 "bnf/m2.bnf"
   FormalType(stopset0, stopset1, stopset2) ;
END NonVarFPSection ;


(*
   FormalType := [ 'ARRAY' 'OF'  ]Qualident 

   first  symbols:identtok, arraytok
   
   cannot reachend
*)

# 748 "bnf/m2.bnf"
PROCEDURE FormalType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 748 "bnf/m2.bnf"
BEGIN
# 748 "bnf/m2.bnf"
   IF currenttoken=arraytok
   THEN
      Expect(arraytok, stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
# 748 "bnf/m2.bnf"
      Expect(oftok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   END ;
# 748 "bnf/m2.bnf"
   Qualident(stopset0, stopset1, stopset2) ;
END FormalType ;


(*
   ModuleDeclaration := 'MODULE' 
                        % PushAutoOn  %
                        Ident 
                        % StartBuildInnerModule  %
                        
                        % PushAutoOff  %
                        [ Priority  ]';' 
                        % PushAutoOn  %
                        { Import 
                          % BuildImportInnerModule  %
                           }[ Export 
                              % BuildExportInnerModule  %
                               ]
                        % PopAuto  %
                        Block 
                        % PushAutoOn  %
                        Ident 
                        % EndBuildInnerModule  %
                        
                        % PopAuto ; PopAuto ; PopAuto  %
                        

   first  symbols:moduletok
   
   cannot reachend
*)

# 750 "bnf/m2.bnf"
PROCEDURE ModuleDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 750 "bnf/m2.bnf"
BEGIN
# 750 "bnf/m2.bnf"
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 750 "bnf/m2.bnf"
   PushAutoOn  ;
# 751 "bnf/m2.bnf"
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
# 751 "bnf/m2.bnf"
   StartBuildInnerModule  ;
# 752 "bnf/m2.bnf"
   PushAutoOff  ;
# 753 "bnf/m2.bnf"
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
# 753 "bnf/m2.bnf"
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, exporttok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
# 753 "bnf/m2.bnf"
   PushAutoOn  ;
# 754 "bnf/m2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, exporttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
# 754 "bnf/m2.bnf"
      BuildImportInnerModule  ;
   END (* while *) ;
# 755 "bnf/m2.bnf"
   IF currenttoken=exporttok
   THEN
      Export(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
# 755 "bnf/m2.bnf"
      BuildExportInnerModule  ;
   END ;
# 756 "bnf/m2.bnf"
   PopAuto  ;
# 757 "bnf/m2.bnf"
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 757 "bnf/m2.bnf"
   PushAutoOn  ;
# 758 "bnf/m2.bnf"
   Ident(stopset0, stopset1, stopset2) ;
# 758 "bnf/m2.bnf"
   EndBuildInnerModule  ;
# 759 "bnf/m2.bnf"
   PopAuto ; PopAuto ; PopAuto  ;
END ModuleDeclaration ;


(*
   Priority := '[' ConstExpression ']' 

   first  symbols:lsbratok
   
   cannot reachend
*)

# 762 "bnf/m2.bnf"
PROCEDURE Priority (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 762 "bnf/m2.bnf"
BEGIN
# 762 "bnf/m2.bnf"
   Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
# 762 "bnf/m2.bnf"
   ConstExpression(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
# 762 "bnf/m2.bnf"
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

# 764 "bnf/m2.bnf"
PROCEDURE Export (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 764 "bnf/m2.bnf"
BEGIN
# 764 "bnf/m2.bnf"
   Expect(exporttok, stopset0, stopset1 + SetOfStop1{qualifiedtok, unqualifiedtok}, stopset2 + SetOfStop2{identtok}) ;
# 764 "bnf/m2.bnf"
   IF currenttoken=qualifiedtok
   THEN
      Expect(qualifiedtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 764 "bnf/m2.bnf"
      PushT(QualifiedTok)  ;
# 765 "bnf/m2.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 766 "bnf/m2.bnf"
   ELSIF currenttoken=unqualifiedtok
   THEN
      Expect(unqualifiedtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 766 "bnf/m2.bnf"
      PushT(UnQualifiedTok)  ;
# 767 "bnf/m2.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 767 "bnf/m2.bnf"
   ELSE
# 767 "bnf/m2.bnf"
      PushT(ExportTok)  ;
# 768 "bnf/m2.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
# 768 "bnf/m2.bnf"
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

# 770 "bnf/m2.bnf"
PROCEDURE Import (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 770 "bnf/m2.bnf"
BEGIN
# 770 "bnf/m2.bnf"
   IF currenttoken=fromtok
   THEN
      Expect(fromtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 770 "bnf/m2.bnf"
      Ident(stopset0, stopset1 + SetOfStop1{importtok}, stopset2) ;
# 770 "bnf/m2.bnf"
      Expect(importtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 770 "bnf/m2.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 770 "bnf/m2.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
# 771 "bnf/m2.bnf"
   ELSIF currenttoken=importtok
   THEN
      Expect(importtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 771 "bnf/m2.bnf"
# 772 "bnf/m2.bnf"
      PushT(ImportTok)
                                                                                   (* determines whether Ident or Module *)  ;
# 773 "bnf/m2.bnf"
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 773 "bnf/m2.bnf"
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorArray('expecting one of: IMPORT FROM')
   END ;
END Import ;


(*
   DefinitionModule := 'DEFINITION' 'MODULE' 
                       % PushAutoOn  %
                       [ 'FOR' string  ]Ident 
                       % P1StartBuildDefinitionModule  %
                       ';' { Import 
                             % BuildImportOuterModule  %
                              }[ Export 
                                 % BuildExportOuterModule  %
                                  ]
                       % PushAutoOff  %
                       { Definition  }
                       % PopAuto  %
                       'END' Ident 
                       % P1EndBuildDefinitionModule  %
                       '.' 
                       % PopAuto  %
                       

   first  symbols:definitiontok
   
   cannot reachend
*)

# 775 "bnf/m2.bnf"
PROCEDURE DefinitionModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 775 "bnf/m2.bnf"
BEGIN
# 775 "bnf/m2.bnf"
   Expect(definitiontok, stopset0, stopset1 + SetOfStop1{moduletok}, stopset2) ;
# 775 "bnf/m2.bnf"
   Expect(moduletok, stopset0, stopset1 + SetOfStop1{fortok}, stopset2 + SetOfStop2{identtok}) ;
# 775 "bnf/m2.bnf"
   PushAutoOn  ;
# 776 "bnf/m2.bnf"
   IF currenttoken=fortok
   THEN
      Expect(fortok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 776 "bnf/m2.bnf"
      string(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   END ;
# 777 "bnf/m2.bnf"
   Ident(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 777 "bnf/m2.bnf"
   P1StartBuildDefinitionModule  ;
# 778 "bnf/m2.bnf"
   Expect(semicolontok, stopset0, stopset1 + SetOfStop1{fromtok, importtok, exporttok, consttok, proceduretok, endtok}, stopset2 + SetOfStop2{typetok, vartok}) ;
# 779 "bnf/m2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok, exporttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
# 779 "bnf/m2.bnf"
      BuildImportOuterModule  ;
   END (* while *) ;
# 780 "bnf/m2.bnf"
   IF currenttoken=exporttok
   THEN
      Export(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
# 780 "bnf/m2.bnf"
      BuildExportOuterModule  ;
   END ;
# 781 "bnf/m2.bnf"
   PushAutoOff  ;
# 782 "bnf/m2.bnf"
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Definition(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
# 782 "bnf/m2.bnf"
   PopAuto  ;
# 783 "bnf/m2.bnf"
   Expect(endtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 783 "bnf/m2.bnf"
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
# 783 "bnf/m2.bnf"
   P1EndBuildDefinitionModule  ;
# 784 "bnf/m2.bnf"
   Expect(periodtok, stopset0, stopset1, stopset2) ;
# 784 "bnf/m2.bnf"
   PopAuto  ;
END DefinitionModule ;


(*
   Definition := 'CONST' { ConstantDeclaration ';'  } | 
                 'TYPE' 
                 % PushAutoOn  %
                 { Ident ( ';' 
                           % BuildHiddenType  %
                            | '=' Type ';'  ) }
                 % PopAuto  %
                  | 'VAR' { VariableDeclaration ';'  } | 
                 ProcedureHeading ';' 

   first  symbols:proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

# 787 "bnf/m2.bnf"
PROCEDURE Definition (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 787 "bnf/m2.bnf"
BEGIN
# 787 "bnf/m2.bnf"
   IF currenttoken=consttok
   THEN
      Expect(consttok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 787 "bnf/m2.bnf"
      WHILE currenttoken=identtok DO
         ConstantDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 787 "bnf/m2.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 788 "bnf/m2.bnf"
   ELSIF currenttoken=typetok
   THEN
      Expect(typetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 788 "bnf/m2.bnf"
      PushAutoOn  ;
# 789 "bnf/m2.bnf"
      WHILE currenttoken=identtok DO
         Ident(stopset0 + SetOfStop0{semicolontok, equaltok}, stopset1, stopset2) ;
# 789 "bnf/m2.bnf"
         IF currenttoken=semicolontok
         THEN
            Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 789 "bnf/m2.bnf"
            BuildHiddenType  ;
# 790 "bnf/m2.bnf"
         ELSIF currenttoken=equaltok
         THEN
            Expect(equaltok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
# 790 "bnf/m2.bnf"
            Type(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 790 "bnf/m2.bnf"
            Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
         ELSE
            ErrorArray('expecting one of: = ;')
         END ;
      END (* while *) ;
# 790 "bnf/m2.bnf"
      PopAuto  ;
# 792 "bnf/m2.bnf"
   ELSIF currenttoken=vartok
   THEN
      Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
# 792 "bnf/m2.bnf"
      WHILE currenttoken=identtok DO
         VariableDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 792 "bnf/m2.bnf"
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
# 793 "bnf/m2.bnf"
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureHeading(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
# 793 "bnf/m2.bnf"
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

# 795 "bnf/m2.bnf"
PROCEDURE AsmStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 795 "bnf/m2.bnf"
BEGIN
# 795 "bnf/m2.bnf"
   Expect(asmtok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2 + SetOfStop2{volatiletok}) ;
# 795 "bnf/m2.bnf"
   IF currenttoken=volatiletok
   THEN
      Expect(volatiletok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
   END ;
# 795 "bnf/m2.bnf"
   Expect(lparatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 795 "bnf/m2.bnf"
   AsmOperands(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 795 "bnf/m2.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END AsmStatement ;


(*
   AsmOperands := string [ ':' AsmList [ ':' AsmList [ ':' TrashList  ] ] ]

   first  symbols:stringtok
   
   cannot reachend
*)

# 797 "bnf/m2.bnf"
PROCEDURE AsmOperands (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 797 "bnf/m2.bnf"
BEGIN
# 797 "bnf/m2.bnf"
   string(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 797 "bnf/m2.bnf"
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0 + SetOfStop0{commatok, colontok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 797 "bnf/m2.bnf"
      AsmList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 797 "bnf/m2.bnf"
      IF currenttoken=colontok
      THEN
         Expect(colontok, stopset0 + SetOfStop0{commatok, colontok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 797 "bnf/m2.bnf"
         AsmList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
# 797 "bnf/m2.bnf"
         IF currenttoken=colontok
         THEN
            Expect(colontok, stopset0 + SetOfStop0{commatok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 797 "bnf/m2.bnf"
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

# 800 "bnf/m2.bnf"
PROCEDURE AsmList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 800 "bnf/m2.bnf"
BEGIN
# 800 "bnf/m2.bnf"
   IF currenttoken=stringtok
   THEN
      AsmElement(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END ;
# 800 "bnf/m2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 800 "bnf/m2.bnf"
      AsmElement(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END AsmList ;


(*
   AsmElement := string '(' Expression ')' 

   first  symbols:stringtok
   
   cannot reachend
*)

# 802 "bnf/m2.bnf"
PROCEDURE AsmElement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 802 "bnf/m2.bnf"
BEGIN
# 802 "bnf/m2.bnf"
   string(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
# 802 "bnf/m2.bnf"
   Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
# 802 "bnf/m2.bnf"
   Expression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
# 802 "bnf/m2.bnf"
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END AsmElement ;


(*
   TrashList := [ string  ]{ ',' string  }

   first  symbols:commatok, stringtok
   
   reachend
*)

# 805 "bnf/m2.bnf"
PROCEDURE TrashList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
# 805 "bnf/m2.bnf"
BEGIN
# 805 "bnf/m2.bnf"
   IF currenttoken=stringtok
   THEN
      string(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END ;
# 805 "bnf/m2.bnf"
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
# 805 "bnf/m2.bnf"
      string(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END TrashList ;


# 377 "bnf/m2.bnf"

END P1SyntaxCheck.
