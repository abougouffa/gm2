(* it is advisable not to edit this file as it was automatically generated from the grammer file bnf/m2.bnf *)

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

FROM M2LexBuf IMPORT currentstring, currenttoken, GetToken, InsertToken, InsertTokenAndRewind ;
FROM M2Error IMPORT WriteFormat0 ;
FROM M2Quads IMPORT PushT, PushTF, IsAutoPushOn, PushAutoOff, PushAutoOn, PopAuto ;
FROM M2Reserved IMPORT tokToTok, toktype, NulTok, ImportTok, ExportTok, QualifiedTok, UnQualifiedTok ;
FROM NameKey IMPORT NulName ;
FROM StrLib IMPORT StrCopy, StrConCat, StrEqual ;
FROM P2SymBuild IMPORT BuildString, BuildNumber ;
FROM M2Debug IMPORT Assert ;
FROM M2Printf IMPORT printf0 ;


(* imports for Pass1 *)
FROM M2Quads IMPORT PushT, PopT,
                    StartBuildInit,
                    EndBuildInit,
                    BuildProcedureStart,
                    BuildProcedureEnd,
                    BuildAssignment,
                    BuildInline,
                    GetPtr ;

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


PROCEDURE ErrorMessage (a: ARRAY OF CHAR) ;
BEGIN
   WriteFormat0(a) ;
   WasNoError := FALSE
END ErrorMessage ;


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
PROCEDURE String (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ; FORWARD ;
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

PROCEDURE DescribeStop (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2; VAR str: ARRAY OF CHAR) ;
VAR
   n      : CARDINAL ;
   message: ARRAY [0..8192] OF CHAR ;
BEGIN
   n := 0 ;
   StrCopy('', message) ;
   IF stringtok IN stopset2
   THEN
      StrConCat(message, ' string,', message) ; INC(n)
   END ;
   IF realtok IN stopset2
   THEN
      StrConCat(message, ' real number,', message) ; INC(n)
   END ;
   IF identtok IN stopset2
   THEN
      StrConCat(message, ' identifier,', message) ; INC(n)
   END ;
   IF integertok IN stopset2
   THEN
      StrConCat(message, ' integer number,', message) ; INC(n)
   END ;
   IF filetok IN stopset2
   THEN
      StrConCat(message, ' __FILE__', message) ; INC(n)
   END ;
   IF linetok IN stopset2
   THEN
      StrConCat(message, ' __LINE__', message) ; INC(n)
   END ;
   IF datetok IN stopset2
   THEN
      StrConCat(message, ' __DATE__', message) ; INC(n)
   END ;
   IF periodperiodperiodtok IN stopset2
   THEN
      StrConCat(message, ' ...', message) ; INC(n)
   END ;
   IF volatiletok IN stopset2
   THEN
      StrConCat(message, ' VOLATILE', message) ; INC(n)
   END ;
   IF asmtok IN stopset2
   THEN
      StrConCat(message, ' ASM', message) ; INC(n)
   END ;
   IF withtok IN stopset2
   THEN
      StrConCat(message, ' WITH', message) ; INC(n)
   END ;
   IF whiletok IN stopset2
   THEN
      StrConCat(message, ' WHILE', message) ; INC(n)
   END ;
   IF vartok IN stopset2
   THEN
      StrConCat(message, ' VAR', message) ; INC(n)
   END ;
   IF untiltok IN stopset2
   THEN
      StrConCat(message, ' UNTIL', message) ; INC(n)
   END ;
   IF typetok IN stopset2
   THEN
      StrConCat(message, ' TYPE', message) ; INC(n)
   END ;
   IF totok IN stopset2
   THEN
      StrConCat(message, ' TO', message) ; INC(n)
   END ;
   IF thentok IN stopset1
   THEN
      StrConCat(message, ' THEN', message) ; INC(n)
   END ;
   IF settok IN stopset1
   THEN
      StrConCat(message, ' SET', message) ; INC(n)
   END ;
   IF returntok IN stopset1
   THEN
      StrConCat(message, ' RETURN', message) ; INC(n)
   END ;
   IF repeattok IN stopset1
   THEN
      StrConCat(message, ' REPEAT', message) ; INC(n)
   END ;
   IF recordtok IN stopset1
   THEN
      StrConCat(message, ' RECORD', message) ; INC(n)
   END ;
   IF unqualifiedtok IN stopset1
   THEN
      StrConCat(message, ' UNQUALIFIED', message) ; INC(n)
   END ;
   IF qualifiedtok IN stopset1
   THEN
      StrConCat(message, ' QUALIFIED', message) ; INC(n)
   END ;
   IF proceduretok IN stopset1
   THEN
      StrConCat(message, ' PROCEDURE', message) ; INC(n)
   END ;
   IF pointertok IN stopset1
   THEN
      StrConCat(message, ' POINTER', message) ; INC(n)
   END ;
   IF ortok IN stopset1
   THEN
      StrConCat(message, ' OR', message) ; INC(n)
   END ;
   IF oftok IN stopset1
   THEN
      StrConCat(message, ' OF', message) ; INC(n)
   END ;
   IF nottok IN stopset1
   THEN
      StrConCat(message, ' NOT', message) ; INC(n)
   END ;
   IF moduletok IN stopset1
   THEN
      StrConCat(message, ' MODULE', message) ; INC(n)
   END ;
   IF modtok IN stopset1
   THEN
      StrConCat(message, ' MOD', message) ; INC(n)
   END ;
   IF looptok IN stopset1
   THEN
      StrConCat(message, ' LOOP', message) ; INC(n)
   END ;
   IF intok IN stopset1
   THEN
      StrConCat(message, ' IN', message) ; INC(n)
   END ;
   IF importtok IN stopset1
   THEN
      StrConCat(message, ' IMPORT', message) ; INC(n)
   END ;
   IF implementationtok IN stopset1
   THEN
      StrConCat(message, ' IMPLEMENTATION', message) ; INC(n)
   END ;
   IF iftok IN stopset1
   THEN
      StrConCat(message, ' IF', message) ; INC(n)
   END ;
   IF fromtok IN stopset1
   THEN
      StrConCat(message, ' FROM', message) ; INC(n)
   END ;
   IF fortok IN stopset1
   THEN
      StrConCat(message, ' FOR', message) ; INC(n)
   END ;
   IF exporttok IN stopset1
   THEN
      StrConCat(message, ' EXPORT', message) ; INC(n)
   END ;
   IF exittok IN stopset1
   THEN
      StrConCat(message, ' EXIT', message) ; INC(n)
   END ;
   IF endtok IN stopset1
   THEN
      StrConCat(message, ' END', message) ; INC(n)
   END ;
   IF elsiftok IN stopset1
   THEN
      StrConCat(message, ' ELSIF', message) ; INC(n)
   END ;
   IF elsetok IN stopset1
   THEN
      StrConCat(message, ' ELSE', message) ; INC(n)
   END ;
   IF dotok IN stopset1
   THEN
      StrConCat(message, ' DO', message) ; INC(n)
   END ;
   IF divtok IN stopset1
   THEN
      StrConCat(message, ' DIV', message) ; INC(n)
   END ;
   IF definitiontok IN stopset1
   THEN
      StrConCat(message, ' DEFINITION', message) ; INC(n)
   END ;
   IF consttok IN stopset1
   THEN
      StrConCat(message, ' CONST', message) ; INC(n)
   END ;
   IF casetok IN stopset1
   THEN
      StrConCat(message, ' CASE', message) ; INC(n)
   END ;
   IF bytok IN stopset1
   THEN
      StrConCat(message, ' BY', message) ; INC(n)
   END ;
   IF begintok IN stopset0
   THEN
      StrConCat(message, ' BEGIN', message) ; INC(n)
   END ;
   IF arraytok IN stopset0
   THEN
      StrConCat(message, ' ARRAY', message) ; INC(n)
   END ;
   IF andtok IN stopset0
   THEN
      StrConCat(message, ' AND', message) ; INC(n)
   END ;
   IF colontok IN stopset0
   THEN
      StrConCat(message, ' :', message) ; INC(n)
   END ;
   IF periodperiodtok IN stopset0
   THEN
      StrConCat(message, ' ..', message) ; INC(n)
   END ;
   IF greaterequaltok IN stopset0
   THEN
      StrConCat(message, ' >=', message) ; INC(n)
   END ;
   IF lessequaltok IN stopset0
   THEN
      StrConCat(message, ' <=', message) ; INC(n)
   END ;
   IF lessgreatertok IN stopset0
   THEN
      StrConCat(message, ' <>', message) ; INC(n)
   END ;
   IF hashtok IN stopset0
   THEN
      StrConCat(message, ' #', message) ; INC(n)
   END ;
   IF equaltok IN stopset0
   THEN
      StrConCat(message, ' =', message) ; INC(n)
   END ;
   IF uparrowtok IN stopset0
   THEN
      StrConCat(message, ' ^', message) ; INC(n)
   END ;
   IF semicolontok IN stopset0
   THEN
      StrConCat(message, ' ;', message) ; INC(n)
   END ;
   IF commatok IN stopset0
   THEN
      StrConCat(message, ' ,', message) ; INC(n)
   END ;
   IF periodtok IN stopset0
   THEN
      StrConCat(message, ' .', message) ; INC(n)
   END ;
   IF ambersandtok IN stopset0
   THEN
      StrConCat(message, ' &', message) ; INC(n)
   END ;
   IF dividetok IN stopset0
   THEN
      StrConCat(message, ' /', message) ; INC(n)
   END ;
   IF timestok IN stopset0
   THEN
      StrConCat(message, ' *', message) ; INC(n)
   END ;
   IF minustok IN stopset0
   THEN
      StrConCat(message, ' -', message) ; INC(n)
   END ;
   IF plustok IN stopset0
   THEN
      StrConCat(message, ' +', message) ; INC(n)
   END ;
   IF doublequotestok IN stopset0
   THEN
      StrConCat(message, ' "', message) ; INC(n)
   END ;
   IF singlequotetok IN stopset0
   THEN
      StrConCat(message, " '", message) ; INC(n)
   END ;
   IF greatertok IN stopset0
   THEN
      StrConCat(message, ' >', message) ; INC(n)
   END ;
   IF lesstok IN stopset0
   THEN
      StrConCat(message, ' <', message) ; INC(n)
   END ;
   IF rparatok IN stopset0
   THEN
      StrConCat(message, ' )', message) ; INC(n)
   END ;
   IF lparatok IN stopset0
   THEN
      StrConCat(message, ' (', message) ; INC(n)
   END ;
   IF rcbratok IN stopset0
   THEN
      StrConCat(message, ' }', message) ; INC(n)
   END ;
   IF lcbratok IN stopset0
   THEN
      StrConCat(message, ' {', message) ; INC(n)
   END ;
   IF rsbratok IN stopset0
   THEN
      StrConCat(message, ' ]', message) ; INC(n)
   END ;
   IF lsbratok IN stopset0
   THEN
      StrConCat(message, ' [', message) ; INC(n)
   END ;
   IF bartok IN stopset0
   THEN
      StrConCat(message, ' |', message) ; INC(n)
   END ;
   IF becomestok IN stopset0
   THEN
      StrConCat(message, ' :=', message) ; INC(n)
   END ;
   IF eoftok IN stopset0
   THEN
      StrConCat(message, ' ', message) ; INC(n)
   END ;
   IF n=0
   THEN
      StrCopy(' syntax error', str) ;
   ELSIF n=1
   THEN
      StrCopy(' missing ', str) ;
      StrConCat(str, message, str)
   ELSE
      StrCopy(' expecting one of', str) ;
      StrConCat(str, message, str)
   END
END DescribeStop ;


(*
   DescribeError - issues a message explaining what tokens were expected
*)

PROCEDURE DescribeError (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
   str: ARRAY [0..8192] OF CHAR ;
BEGIN
   DescribeStop(stopset0, stopset1, stopset2, str) ;
   CASE currenttoken OF
   
   stringtok: StrConCat('syntax error, found string,', str, str) |
   realtok: StrConCat('syntax error, found real number,', str, str) |
   identtok: StrConCat('syntax error, found identifier,', str, str) |
   integertok: StrConCat('syntax error, found integer number,', str, str) |
   filetok: StrConCat('syntax error, found __FILE__', str, str) |
   linetok: StrConCat('syntax error, found __LINE__', str, str) |
   datetok: StrConCat('syntax error, found __DATE__', str, str) |
   periodperiodperiodtok: StrConCat('syntax error, found ...', str, str) |
   volatiletok: StrConCat('syntax error, found VOLATILE', str, str) |
   asmtok: StrConCat('syntax error, found ASM', str, str) |
   withtok: StrConCat('syntax error, found WITH', str, str) |
   whiletok: StrConCat('syntax error, found WHILE', str, str) |
   vartok: StrConCat('syntax error, found VAR', str, str) |
   untiltok: StrConCat('syntax error, found UNTIL', str, str) |
   typetok: StrConCat('syntax error, found TYPE', str, str) |
   totok: StrConCat('syntax error, found TO', str, str) |
   thentok: StrConCat('syntax error, found THEN', str, str) |
   settok: StrConCat('syntax error, found SET', str, str) |
   returntok: StrConCat('syntax error, found RETURN', str, str) |
   repeattok: StrConCat('syntax error, found REPEAT', str, str) |
   recordtok: StrConCat('syntax error, found RECORD', str, str) |
   unqualifiedtok: StrConCat('syntax error, found UNQUALIFIED', str, str) |
   qualifiedtok: StrConCat('syntax error, found QUALIFIED', str, str) |
   proceduretok: StrConCat('syntax error, found PROCEDURE', str, str) |
   pointertok: StrConCat('syntax error, found POINTER', str, str) |
   ortok: StrConCat('syntax error, found OR', str, str) |
   oftok: StrConCat('syntax error, found OF', str, str) |
   nottok: StrConCat('syntax error, found NOT', str, str) |
   moduletok: StrConCat('syntax error, found MODULE', str, str) |
   modtok: StrConCat('syntax error, found MOD', str, str) |
   looptok: StrConCat('syntax error, found LOOP', str, str) |
   intok: StrConCat('syntax error, found IN', str, str) |
   importtok: StrConCat('syntax error, found IMPORT', str, str) |
   implementationtok: StrConCat('syntax error, found IMPLEMENTATION', str, str) |
   iftok: StrConCat('syntax error, found IF', str, str) |
   fromtok: StrConCat('syntax error, found FROM', str, str) |
   fortok: StrConCat('syntax error, found FOR', str, str) |
   exporttok: StrConCat('syntax error, found EXPORT', str, str) |
   exittok: StrConCat('syntax error, found EXIT', str, str) |
   endtok: StrConCat('syntax error, found END', str, str) |
   elsiftok: StrConCat('syntax error, found ELSIF', str, str) |
   elsetok: StrConCat('syntax error, found ELSE', str, str) |
   dotok: StrConCat('syntax error, found DO', str, str) |
   divtok: StrConCat('syntax error, found DIV', str, str) |
   definitiontok: StrConCat('syntax error, found DEFINITION', str, str) |
   consttok: StrConCat('syntax error, found CONST', str, str) |
   casetok: StrConCat('syntax error, found CASE', str, str) |
   bytok: StrConCat('syntax error, found BY', str, str) |
   begintok: StrConCat('syntax error, found BEGIN', str, str) |
   arraytok: StrConCat('syntax error, found ARRAY', str, str) |
   andtok: StrConCat('syntax error, found AND', str, str) |
   colontok: StrConCat('syntax error, found :', str, str) |
   periodperiodtok: StrConCat('syntax error, found ..', str, str) |
   greaterequaltok: StrConCat('syntax error, found >=', str, str) |
   lessequaltok: StrConCat('syntax error, found <=', str, str) |
   lessgreatertok: StrConCat('syntax error, found <>', str, str) |
   hashtok: StrConCat('syntax error, found #', str, str) |
   equaltok: StrConCat('syntax error, found =', str, str) |
   uparrowtok: StrConCat('syntax error, found ^', str, str) |
   semicolontok: StrConCat('syntax error, found ;', str, str) |
   commatok: StrConCat('syntax error, found ,', str, str) |
   periodtok: StrConCat('syntax error, found .', str, str) |
   ambersandtok: StrConCat('syntax error, found &', str, str) |
   dividetok: StrConCat('syntax error, found /', str, str) |
   timestok: StrConCat('syntax error, found *', str, str) |
   minustok: StrConCat('syntax error, found -', str, str) |
   plustok: StrConCat('syntax error, found +', str, str) |
   doublequotestok: StrConCat('syntax error, found "', str, str) |
   singlequotetok: StrConCat("syntax error, found '", str, str) |
   greatertok: StrConCat('syntax error, found >', str, str) |
   lesstok: StrConCat('syntax error, found <', str, str) |
   rparatok: StrConCat('syntax error, found )', str, str) |
   lparatok: StrConCat('syntax error, found (', str, str) |
   rcbratok: StrConCat('syntax error, found }', str, str) |
   lcbratok: StrConCat('syntax error, found {', str, str) |
   rsbratok: StrConCat('syntax error, found ]', str, str) |
   lsbratok: StrConCat('syntax error, found [', str, str) |
   bartok: StrConCat('syntax error, found |', str, str) |
   becomestok: StrConCat('syntax error, found :=', str, str) |
   eoftok: StrConCat('syntax error, found ', str, str)

   ELSE
   END ;
   ErrorMessage(str) ;
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
   str: ARRAY [0..8192] OF CHAR ;
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
   DescribeStop(s0, s1, s2, str) ;
   StrConCat('syntax error,', str, str) ;
   WriteFormat0(str)
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
   String -
*)

PROCEDURE String (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF IsAutoPushOn()
   THEN
      PushTF(currentstring, stringtok) ;
      BuildString
   END ;
   Expect(stringtok, stopset0, stopset1, stopset2)
END String ;


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
   String := 

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

PROCEDURE FileUnit (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   PushAutoOff  ;
   
   IF currenttoken=definitiontok
   THEN
      DefinitionModule(stopset0, stopset1, stopset2) ;
   ELSIF ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, implementationtok}))
   THEN
      ImplementationOrProgramModule(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: MODULE IMPLEMENTATION DEFINITION')
   END ;
   PopAuto  ;
END FileUnit ;


(*
   ProgramModule := 'MODULE' 
                    % PushAutoOn ;  %
                    Ident 
                    % VAR Ptr, PtrC: CARDINAL ;  %
                    
                    % GetPtr(Ptr) ;  %
                    
                    % P1StartBuildProgramModule ;  %
                    
                    % PushAutoOff ;  %
                    
                    % GetPtr(PtrC) ; Assert(Ptr=PtrC) ;  %
                    [ Priority  ]
                    % GetPtr(PtrC) ; Assert(Ptr=PtrC) ;  %
                    ';' 
                    % PushAutoOn ;  %
                    { Import 
                      % BuildImportOuterModule  %
                       }
                    % PopAuto  %
                    
                    % GetPtr(PtrC) ; Assert(Ptr=PtrC) ;  %
                    Block 
                    % GetPtr(PtrC) ; Assert(Ptr=PtrC) ;  %
                    
                    % PushAutoOn  %
                    Ident 
                    % P1EndBuildProgramModule  %
                    '.' 
                    % PopAuto ; PopAuto ; PopAuto  %
                    

   first  symbols:moduletok
   
   cannot reachend
*)

PROCEDURE ProgramModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
    Ptr, PtrC: CARDINAL ; 
BEGIN
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   PushAutoOn ;  ;
   
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
   
   
   GetPtr(Ptr) ;  ;
   
   P1StartBuildProgramModule ;  ;
   
   PushAutoOff ;  ;
   
   GetPtr(PtrC) ; Assert(Ptr=PtrC) ;  ;
   
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
   GetPtr(PtrC) ; Assert(Ptr=PtrC) ;  ;
   
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
   PushAutoOn ;  ;
   
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
      BuildImportOuterModule  ;
   END (* while *) ;
   PopAuto  ;
   
   GetPtr(PtrC) ; Assert(Ptr=PtrC) ;  ;
   
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   GetPtr(PtrC) ; Assert(Ptr=PtrC) ;  ;
   
   PushAutoOn  ;
   
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
   P1EndBuildProgramModule  ;
   
   Expect(periodtok, stopset0, stopset1, stopset2) ;
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

PROCEDURE ImplementationModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(implementationtok, stopset0, stopset1 + SetOfStop1{moduletok}, stopset2) ;
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   PushAutoOn ;  ;
   
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
   P1StartBuildImplementationModule ;  ;
   
   PushAutoOff ;  ;
   
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
   PushAutoOn ;  ;
   
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
      BuildImportOuterModule  ;
   END (* while *) ;
   PopAuto ;  ;
   
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   PushAutoOn ;  ;
   
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
   P1EndBuildImplementationModule  ;
   
   PopAuto ; PopAuto ; PopAuto ;  ;
   
   Expect(periodtok, stopset0, stopset1, stopset2) ;
END ImplementationModule ;


(*
   ImplementationOrProgramModule := ImplementationModule  | 
                                    ProgramModule 

   first  symbols:moduletok, implementationtok
   
   cannot reachend
*)

PROCEDURE ImplementationOrProgramModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=implementationtok
   THEN
      ImplementationModule(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=moduletok
   THEN
      ProgramModule(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: MODULE IMPLEMENTATION')
   END ;
END ImplementationOrProgramModule ;


(*
   Number := Integer  | Real 

   first  symbols:realtok, integertok
   
   cannot reachend
*)

PROCEDURE Number (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=integertok
   THEN
      Integer(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=realtok
   THEN
      Real(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: real number, integer number,')
   END ;
END Number ;


(*
   Qualident := Ident { '.' Ident  }

   first  symbols:identtok
   
   cannot reachend
*)

PROCEDURE Qualident (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
   WHILE currenttoken=periodtok DO
      Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
   END (* while *) ;
END Qualident ;


(*
   ConstantDeclaration := Ident '=' ConstExpression 

   first  symbols:identtok
   
   cannot reachend
*)

PROCEDURE ConstantDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Ident(stopset0 + SetOfStop0{equaltok}, stopset1, stopset2) ;
   Expect(equaltok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
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

PROCEDURE ConstExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   PushAutoOff  ;
   
   SimpleConstExpr(stopset0 + SetOfStop0{equaltok, hashtok, lessgreatertok, lesstok, lessequaltok, greatertok, greaterequaltok}, stopset1 + SetOfStop1{intok}, stopset2) ;
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok})) OR
      (currenttoken=intok)
   THEN
      Relation(stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
      SimpleConstExpr(stopset0, stopset1, stopset2) ;
   END ;
   PopAuto  ;
END ConstExpression ;


(*
   Relation := '='  | '#'  | '<>'  | '<'  | '<='  | '>'  | 
               '>='  | 'IN' 

   first  symbols:intok, greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok
   
   cannot reachend
*)

PROCEDURE Relation (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=equaltok
   THEN
      Expect(equaltok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=hashtok
   THEN
      Expect(hashtok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=lessgreatertok
   THEN
      Expect(lessgreatertok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=lesstok
   THEN
      Expect(lesstok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=lessequaltok
   THEN
      Expect(lessequaltok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=greatertok
   THEN
      Expect(greatertok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=greaterequaltok
   THEN
      Expect(greaterequaltok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=intok
   THEN
      Expect(intok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: IN >= > <= < <> # =')
   END ;
END Relation ;


(*
   SimpleConstExpr := UnaryOrConstTerm { AddOperator ConstTerm  }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

PROCEDURE SimpleConstExpr (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   UnaryOrConstTerm(stopset0 + SetOfStop0{plustok, minustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok})) OR
         (currenttoken=ortok) DO
      AddOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
      ConstTerm(stopset0 + SetOfStop0{minustok, plustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
   END (* while *) ;
END SimpleConstExpr ;


(*
   UnaryOrConstTerm := '+' ConstTerm  | '-' ConstTerm  | 
                       ConstTerm 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

PROCEDURE UnaryOrConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
      ConstTerm(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
      ConstTerm(stopset0, stopset1, stopset2) ;
   ELSIF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok})) OR
         (currenttoken=nottok) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {stringtok, realtok, integertok, identtok}))
   THEN
      ConstTerm(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: NOT ( string, real number, integer number, identifier, { - +')
   END ;
END UnaryOrConstTerm ;


(*
   AddOperator := '+'  | '-'  | 'OR' 

   first  symbols:ortok, minustok, plustok
   
   cannot reachend
*)

PROCEDURE AddOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=plustok
   THEN
      Expect(plustok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=minustok
   THEN
      Expect(minustok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=ortok
   THEN
      Expect(ortok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: OR - +')
   END ;
END AddOperator ;


(*
   ConstTerm := ConstFactor { MulOperator ConstFactor  }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok
   
   cannot reachend
*)

PROCEDURE ConstTerm (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   ConstFactor(stopset0 + SetOfStop0{timestok, dividetok, andtok, ambersandtok}, stopset1 + SetOfStop1{divtok, modtok}, stopset2) ;
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {ambersandtok, andtok, dividetok, timestok})) OR
         ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {modtok, divtok})) DO
      MulOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
      ConstFactor(stopset0 + SetOfStop0{ambersandtok, andtok, dividetok, timestok}, stopset1 + SetOfStop1{modtok, divtok}, stopset2) ;
   END (* while *) ;
END ConstTerm ;


(*
   MulOperator := '*'  | '/'  | 'DIV'  | 'MOD'  | 'AND'  | 
                  '&' 

   first  symbols:ambersandtok, andtok, modtok, divtok, dividetok, timestok
   
   cannot reachend
*)

PROCEDURE MulOperator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=timestok
   THEN
      Expect(timestok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=dividetok
   THEN
      Expect(dividetok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=divtok
   THEN
      Expect(divtok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=modtok
   THEN
      Expect(modtok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=andtok
   THEN
      Expect(andtok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=ambersandtok
   THEN
      Expect(ambersandtok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: & AND MOD DIV / *')
   END ;
END MulOperator ;


(*
   ConstFactor := ConstQualidentOrSet  | 
                  Number  | ConstString  | '(' ConstExpression 
                  ')'  | 'NOT' ConstFactor 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok
   
   cannot reachend
*)

PROCEDURE ConstFactor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF (currenttoken=lcbratok) OR
      (currenttoken=identtok)
   THEN
      ConstQualidentOrSet(stopset0, stopset1, stopset2) ;
   ELSIF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {realtok, integertok}))
   THEN
      Number(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=stringtok
   THEN
      ConstString(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=lparatok
   THEN
      Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
      ConstExpression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
      Expect(rparatok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=nottok
   THEN
      Expect(nottok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
      ConstFactor(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: NOT ( string, real number, integer number, identifier, {')
   END ;
END ConstFactor ;


(*
   ConstString := String 

   first  symbols:stringtok
   
   cannot reachend
*)

PROCEDURE ConstString (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   String(stopset0, stopset1, stopset2) ;
END ConstString ;


(*
   ConstQualidentOrSet := SimpleSet  | Qualident [ SimpleSet  ]

   first  symbols:identtok, lcbratok
   
   cannot reachend
*)

PROCEDURE ConstQualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=lcbratok
   THEN
      SimpleSet(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok}, stopset1, stopset2) ;
      IF currenttoken=lcbratok
      THEN
         SimpleSet(stopset0, stopset1, stopset2) ;
      END ;
   ELSE
      ErrorMessage('expecting one of: identifier, {')
   END ;
END ConstQualidentOrSet ;


(*
   QualidentOrSet := SimpleSet  | Qualident [ SimpleSet  ]

   first  symbols:identtok, lcbratok
   
   cannot reachend
*)

PROCEDURE QualidentOrSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=lcbratok
   THEN
      SimpleSet(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok}, stopset1, stopset2) ;
      IF currenttoken=lcbratok
      THEN
         SimpleSet(stopset0, stopset1, stopset2) ;
      END ;
   ELSE
      ErrorMessage('expecting one of: identifier, {')
   END ;
END QualidentOrSet ;


(*
   Element := ConstExpression [ '..' ConstExpression  ]

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   reachend
*)

PROCEDURE Element (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
      ConstExpression(stopset0, stopset1, stopset2) ;
   END ;
END Element ;


(*
   TypeDeclaration := 
                      % VAR c1, c2: CARDINAL ;  %
                      
                      % GetPtr(c1)  %
                      
                      % PushAutoOn  %
                      ( Ident '=' Type  )
                      % PopAuto     %
                      
                      % GetPtr(c2)  %
                      
                      % Assert(c1=c2)  %
                      

   first  symbols:identtok
   
   cannot reachend
*)

PROCEDURE TypeDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
    c1, c2: CARDINAL ; 
BEGIN
   
   
   GetPtr(c1)  ;
   
   PushAutoOn  ;
   
   Ident(stopset0 + SetOfStop0{equaltok}, stopset1, stopset2) ;
   Expect(equaltok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
   Type(stopset0, stopset1, stopset2) ;
   PopAuto     ;
   
   GetPtr(c2)  ;
   
   Assert(c1=c2)  ;
END TypeDeclaration ;


(*
   Type := 
           % VAR Name: CARDINAL ;  %
           
           % VAR Ptr, PtrC: CARDINAL ;  %
           
           % GetPtr(Ptr) ; PushAutoOff  %
           ( SimpleType  | ArrayType  | RecordType  | 
             SetType  | PointerType  | ProcedureType  )
           % PopAuto ; GetPtr(PtrC) ; Assert(Ptr=PtrC)  %
           
           % PopT(Name)  (* remove TYPE name from stack *)  %
           

   first  symbols:proceduretok, pointertok, settok, recordtok, arraytok, lsbratok, lparatok, identtok
   
   cannot reachend
*)

PROCEDURE Type (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
    Name: CARDINAL ;  Ptr, PtrC: CARDINAL ; 
BEGIN
   
   
   
   
   GetPtr(Ptr) ; PushAutoOff  ;
   
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lsbratok, lparatok})) OR
      (currenttoken=identtok)
   THEN
      SimpleType(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=arraytok
   THEN
      ArrayType(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=recordtok
   THEN
      RecordType(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=settok
   THEN
      SetType(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=pointertok
   THEN
      PointerType(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureType(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: PROCEDURE POINTER SET RECORD ARRAY [ ( identifier,')
   END ;
   PopAuto ; GetPtr(PtrC) ; Assert(Ptr=PtrC)  ;
   
   PopT(Name)  (* remove TYPE name from stack *)  ;
END Type ;


(*
   SimpleType := 
                 % VAR Ptr, PtrC: CARDINAL ;  %
                 
                 % GetPtr(Ptr)  %
                 ( Qualident  | Enumeration 
                   % BuildType  %
                    | SubrangeType  )
                 % GetPtr(PtrC) ; Assert(Ptr=PtrC)  %
                 

   first  symbols:lsbratok, lparatok, identtok
   
   cannot reachend
*)

PROCEDURE SimpleType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
    Ptr, PtrC: CARDINAL ; 
BEGIN
   
   
   GetPtr(Ptr)  ;
   
   IF currenttoken=identtok
   THEN
      Qualident(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=lparatok
   THEN
      Enumeration(stopset0, stopset1, stopset2) ;
      BuildType  ;
   ELSIF currenttoken=lsbratok
   THEN
      SubrangeType(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: [ ( identifier,')
   END ;
   GetPtr(PtrC) ; Assert(Ptr=PtrC)  ;
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

PROCEDURE Enumeration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(lparatok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   PushAutoOn  ;
   
   IdentList(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
   BuildEnumeration  ;
   PopAuto  ;
   
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

PROCEDURE IdentList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
   
                                                                                on: BOOLEAN ;
                                                                                n : CARDINAL ; 
BEGIN
   Ident(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   
   
   on := IsAutoPushOn() ;
   IF on
   THEN
      n := 1
   END  ;
   
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      Ident(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
      IF on
      THEN
         INC(n)
      END  ;
   END (* while *) ;
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

PROCEDURE SubrangeType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
   Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
   ConstExpression(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
   Expect(rsbratok, stopset0, stopset1, stopset2) ;
END SubrangeType ;


(*
   ArrayType := 'ARRAY' SimpleType { ',' SimpleType  }'OF' 
                % BuildNulName  %
                Type 

   first  symbols:arraytok
   
   cannot reachend
*)

PROCEDURE ArrayType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(arraytok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
   SimpleType(stopset0 + SetOfStop0{commatok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
      SimpleType(stopset0 + SetOfStop0{commatok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
   END (* while *) ;
   Expect(oftok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
   BuildNulName  ;
   
   Type(stopset0, stopset1, stopset2) ;
END ArrayType ;


(*
   RecordType := 'RECORD' FieldListSequence 'END' 

   first  symbols:recordtok
   
   cannot reachend
*)

PROCEDURE RecordType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(recordtok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok, endtok}, stopset2 + SetOfStop2{identtok}) ;
   FieldListSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   Expect(endtok, stopset0, stopset1, stopset2) ;
END RecordType ;


(*
   FieldListSequence := FieldListStatement { ';' FieldListStatement  }

   first  symbols:semicolontok, casetok, identtok
   
   reachend
*)

PROCEDURE FieldListSequence (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   FieldListStatement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   WHILE currenttoken=semicolontok DO
      Expect(semicolontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok}, stopset2 + SetOfStop2{identtok}) ;
      FieldListStatement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END (* while *) ;
END FieldListSequence ;


(*
   FieldListStatement := [ FieldList  ]

   first  symbols:casetok, identtok
   
   reachend
*)

PROCEDURE FieldListStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
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

PROCEDURE FieldList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=identtok
   THEN
      IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
      Expect(colontok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
      BuildNulName  ;
      
      Type(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=casetok
   THEN
      Expect(casetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      Ident(stopset0 + SetOfStop0{colontok, periodtok}, stopset1 + SetOfStop1{oftok}, stopset2) ;
      
      IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {periodtok, colontok}))
      THEN
         (* seen optional [ | ] expression *)
         IF currenttoken=colontok
         THEN
            Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
            Qualident(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
         ELSIF currenttoken=periodtok
         THEN
            Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
            Qualident(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
         ELSE
            ErrorMessage('expecting one of: . :')
         END ;
         (* end of optional [ | ] expression *)
      END ;
      Expect(oftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
      Varient(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{elsetok, endtok}, stopset2) ;
      WHILE currenttoken=bartok DO
         Expect(bartok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
         Varient(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{endtok, elsetok}, stopset2) ;
      END (* while *) ;
      IF currenttoken=elsetok
      THEN
         Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok, endtok}, stopset2 + SetOfStop2{identtok}) ;
         FieldListSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
      END ;
      Expect(endtok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: CASE identifier,')
   END ;
END FieldList ;


(*
   Varient := CaseLabelList ':' FieldListSequence 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

PROCEDURE Varient (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   CaseLabelList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
   Expect(colontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{casetok}, stopset2 + SetOfStop2{identtok}) ;
   FieldListSequence(stopset0, stopset1, stopset2) ;
END Varient ;


(*
   CaseLabelList := CaseLabels { ',' CaseLabels  }

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

PROCEDURE CaseLabelList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   CaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
      CaseLabels(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END CaseLabelList ;


(*
   CaseLabels := ConstExpression [ '..' ConstExpression  ]

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

PROCEDURE CaseLabels (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   ConstExpression(stopset0 + SetOfStop0{periodperiodtok}, stopset1, stopset2) ;
   IF currenttoken=periodperiodtok
   THEN
      Expect(periodperiodtok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
      ConstExpression(stopset0, stopset1, stopset2) ;
   END ;
END CaseLabels ;


(*
   SetType := 'SET' 'OF' SimpleType 

   first  symbols:settok
   
   cannot reachend
*)

PROCEDURE SetType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(settok, stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
   Expect(oftok, stopset0 + SetOfStop0{lparatok, lsbratok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
   SimpleType(stopset0, stopset1, stopset2) ;
END SetType ;


(*
   PointerType := 'POINTER' 'TO' 
                  % BuildNulName  %
                  Type 

   first  symbols:pointertok
   
   cannot reachend
*)

PROCEDURE PointerType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(pointertok, stopset0, stopset1, stopset2 + SetOfStop2{totok}) ;
   Expect(totok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
   BuildNulName  ;
   
   Type(stopset0, stopset1, stopset2) ;
END PointerType ;


(*
   ProcedureType := 'PROCEDURE' [ FormalTypeList  ]

   first  symbols:proceduretok
   
   cannot reachend
*)

PROCEDURE ProcedureType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(proceduretok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
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

PROCEDURE FormalTypeList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(lparatok, stopset0 + SetOfStop0{rparatok, arraytok}, stopset1, stopset2 + SetOfStop2{vartok, identtok}) ;
   IF currenttoken=rparatok
   THEN
      Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
      FormalReturn(stopset0, stopset1, stopset2) ;
   ELSIF (currenttoken=arraytok) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, vartok}))
   THEN
      ProcedureParameters(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
      Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
      FormalReturn(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: identifier, ARRAY VAR )')
   END ;
END FormalTypeList ;


(*
   FormalReturn := [ ':' Qualident  ]

   first  symbols:colontok
   
   reachend
*)

PROCEDURE FormalReturn (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      Qualident(stopset0, stopset1, stopset2) ;
   END ;
END FormalReturn ;


(*
   ProcedureParameters := ProcedureParameter { ',' ProcedureParameter  }

   first  symbols:identtok, arraytok, vartok
   
   cannot reachend
*)

PROCEDURE ProcedureParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   ProcedureParameter(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{vartok, identtok}) ;
      ProcedureParameter(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END ProcedureParameters ;


(*
   ProcedureParameter := 'VAR' FormalType  | 
                         FormalType 

   first  symbols:identtok, arraytok, vartok
   
   cannot reachend
*)

PROCEDURE ProcedureParameter (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=vartok
   THEN
      Expect(vartok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
      FormalType(stopset0, stopset1, stopset2) ;
   ELSIF (currenttoken=arraytok) OR
         (currenttoken=identtok)
   THEN
      FormalType(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: identifier, ARRAY VAR')
   END ;
END ProcedureParameter ;


(*
   VariableDeclaration := 
                          % VAR Ptr, PtrC: CARDINAL ;  %
                          
                          % GetPtr(Ptr)  %
                          ( IdentList ':' 
                            % BuildNulName  %
                            Type  )
                          % GetPtr(PtrC)  %
                          
                          % Assert(Ptr=PtrC)  %
                          

   first  symbols:identtok
   
   cannot reachend
*)

PROCEDURE VariableDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
    Ptr, PtrC: CARDINAL ; 
BEGIN
   
   
   GetPtr(Ptr)  ;
   
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
   Expect(colontok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
   BuildNulName  ;
   
   Type(stopset0, stopset1, stopset2) ;
   GetPtr(PtrC)  ;
   
   Assert(Ptr=PtrC)  ;
END VariableDeclaration ;


(*
   Designator := Qualident { SubDesignator  }

   first  symbols:identtok
   
   cannot reachend
*)

PROCEDURE Designator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Qualident(stopset0 + SetOfStop0{periodtok, lsbratok, uparrowtok}, stopset1, stopset2) ;
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

PROCEDURE SubDesignator (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=periodtok
   THEN
      Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      Ident(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=lsbratok
   THEN
      Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rsbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
      ExpList(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
      Expect(rsbratok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=uparrowtok
   THEN
      Expect(uparrowtok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: ^ [ .')
   END ;
END SubDesignator ;


(*
   ExpList := Expression { ',' Expression  }

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

PROCEDURE ExpList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expression(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
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

PROCEDURE Expression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   PushAutoOff  ;
   
   SimpleExpression(stopset0 + SetOfStop0{equaltok, hashtok, lessgreatertok, lesstok, lessequaltok, greatertok, greaterequaltok}, stopset1 + SetOfStop1{intok}, stopset2) ;
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {greaterequaltok, greatertok, lessequaltok, lesstok, lessgreatertok, hashtok, equaltok})) OR
      (currenttoken=intok)
   THEN
      Relation(stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
      SimpleExpression(stopset0, stopset1, stopset2) ;
   END ;
   PopAuto  ;
END Expression ;


(*
   SimpleExpression := [ '+'  | '-'  ]Term { AddOperator Term  }

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok, minustok, plustok
   
   cannot reachend
*)

PROCEDURE SimpleExpression (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok}))
   THEN
      (* seen optional [ | ] expression *)
      IF currenttoken=plustok
      THEN
         Expect(plustok, stopset0 + SetOfStop0{lparatok, lcbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, stringtok, realtok, integertok}) ;
      ELSIF currenttoken=minustok
      THEN
         Expect(minustok, stopset0 + SetOfStop0{lparatok, lcbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, stringtok, realtok, integertok}) ;
      ELSE
         ErrorMessage('expecting one of: - +')
      END ;
      (* end of optional [ | ] expression *)
   END ;
   Term(stopset0 + SetOfStop0{plustok, minustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {minustok, plustok})) OR
         (currenttoken=ortok) DO
      AddOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
      Term(stopset0 + SetOfStop0{minustok, plustok}, stopset1 + SetOfStop1{ortok}, stopset2) ;
   END (* while *) ;
END SimpleExpression ;


(*
   Term := Factor { MulOperator Factor  }

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok
   
   cannot reachend
*)

PROCEDURE Term (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Factor(stopset0 + SetOfStop0{timestok, dividetok, andtok, ambersandtok}, stopset1 + SetOfStop1{divtok, modtok}, stopset2) ;
   WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {ambersandtok, andtok, dividetok, timestok})) OR
         ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {modtok, divtok})) DO
      MulOperator(stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
      Factor(stopset0 + SetOfStop0{ambersandtok, andtok, dividetok, timestok}, stopset1 + SetOfStop1{modtok, divtok}, stopset2) ;
   END (* while *) ;
END Term ;


(*
   Factor := Number  | String  | SetOrDesignatorOrFunction  | 
             '(' Expression ')'  | 'NOT' Factor 

   first  symbols:nottok, lparatok, lcbratok, identtok, stringtok, realtok, integertok
   
   cannot reachend
*)

PROCEDURE Factor (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {realtok, integertok}))
   THEN
      Number(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=stringtok
   THEN
      String(stopset0, stopset1, stopset2) ;
   ELSIF (currenttoken=lcbratok) OR
         (currenttoken=identtok)
   THEN
      SetOrDesignatorOrFunction(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=lparatok
   THEN
      Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
      Expression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
      Expect(rparatok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=nottok
   THEN
      Expect(nottok, stopset0 + SetOfStop0{lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
      Factor(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: NOT ( { identifier, string, real number, integer number,')
   END ;
END Factor ;


(*
   SimpleSet := '{' [ Element { ',' Element  } ]'}' 

   first  symbols:lcbratok
   
   cannot reachend
*)

PROCEDURE SimpleSet (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(lcbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {commatok, lparatok, lcbratok, minustok, plustok})) OR
      (currenttoken=nottok) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {stringtok, realtok, integertok, identtok}))
   THEN
      Element(stopset0 + SetOfStop0{commatok, rcbratok}, stopset1, stopset2) ;
      WHILE currenttoken=commatok DO
         Expect(commatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rcbratok, commatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
         Element(stopset0 + SetOfStop0{rcbratok, commatok}, stopset1, stopset2) ;
      END (* while *) ;
   END ;
   Expect(rcbratok, stopset0, stopset1, stopset2) ;
END SimpleSet ;


(*
   SetOrDesignatorOrFunction := ( Qualident [ SimpleSet  | 
                                              SimpleDes [ ActualParameters  ] ] | 
                                  SimpleSet  )

   first  symbols:lcbratok, identtok
   
   cannot reachend
*)

PROCEDURE SetOrDesignatorOrFunction (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=identtok
   THEN
      Qualident(stopset0 + SetOfStop0{lcbratok, periodtok, lsbratok, uparrowtok, lparatok}, stopset1, stopset2) ;
      
      IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, uparrowtok, lsbratok, periodtok, lcbratok}))
      THEN
         (* seen optional [ | ] expression *)
         IF currenttoken=lcbratok
         THEN
            SimpleSet(stopset0, stopset1, stopset2) ;
         ELSIF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, uparrowtok, lsbratok, periodtok}))
         THEN
            SimpleDes(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
            IF currenttoken=lparatok
            THEN
               ActualParameters(stopset0, stopset1, stopset2) ;
            END ;
         ELSE
            ErrorMessage('expecting one of: ( ^ [ . {')
         END ;
         (* end of optional [ | ] expression *)
      END ;
   ELSIF currenttoken=lcbratok
   THEN
      SimpleSet(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: { identifier,')
   END ;
END SetOrDesignatorOrFunction ;


(*
   SimpleDes := { '.' Ident  | '[' ExpList ']'  | 
                  '^'  }

   first  symbols:uparrowtok, lsbratok, periodtok
   
   reachend
*)

PROCEDURE SimpleDes (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {uparrowtok, lsbratok, periodtok}))
   THEN
      (* seen optional { | } expression *)
      WHILE ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {uparrowtok, lsbratok, periodtok})) DO
         IF currenttoken=periodtok
         THEN
            Expect(periodtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
            Ident(stopset0 + SetOfStop0{uparrowtok, lsbratok, periodtok}, stopset1, stopset2) ;
         ELSIF currenttoken=lsbratok
         THEN
            Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rsbratok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
            ExpList(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
            Expect(rsbratok, stopset0 + SetOfStop0{uparrowtok, lsbratok, periodtok}, stopset1, stopset2) ;
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

PROCEDURE ActualParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
   IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok, minustok, plustok})) OR
      (currenttoken=nottok) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, stringtok, realtok, integertok}))
   THEN
      ExpList(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
   END ;
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

PROCEDURE Statement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   
   IF ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {returntok, exittok, fortok, looptok, repeattok, casetok, iftok})) OR
      ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {asmtok, withtok, whiletok, identtok}))
   THEN
      (* seen optional [ | ] expression *)
      IF currenttoken=identtok
      THEN
         AssignmentOrProcedureCall(stopset0, stopset1, stopset2) ;
      ELSIF currenttoken=iftok
      THEN
         IfStatement(stopset0, stopset1, stopset2) ;
      ELSIF currenttoken=casetok
      THEN
         CaseStatement(stopset0, stopset1, stopset2) ;
      ELSIF currenttoken=whiletok
      THEN
         WhileStatement(stopset0, stopset1, stopset2) ;
      ELSIF currenttoken=repeattok
      THEN
         RepeatStatement(stopset0, stopset1, stopset2) ;
      ELSIF currenttoken=looptok
      THEN
         LoopStatement(stopset0, stopset1, stopset2) ;
      ELSIF currenttoken=fortok
      THEN
         ForStatement(stopset0, stopset1, stopset2) ;
      ELSIF currenttoken=withtok
      THEN
         WithStatement(stopset0, stopset1, stopset2) ;
      ELSIF currenttoken=asmtok
      THEN
         AsmStatement(stopset0, stopset1, stopset2) ;
      ELSIF currenttoken=exittok
      THEN
         Expect(exittok, stopset0, stopset1, stopset2) ;
      ELSIF currenttoken=returntok
      THEN
         Expect(returntok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
         IF ((currenttoken<bytok) AND (currenttoken IN SetOfStop0 {lparatok, lcbratok, minustok, plustok})) OR
            (currenttoken=nottok) OR
            ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {identtok, stringtok, realtok, integertok}))
         THEN
            Expression(stopset0, stopset1, stopset2) ;
         END ;
      ELSE
         ErrorMessage('expecting one of: RETURN EXIT ASM WITH FOR LOOP REPEAT WHILE CASE IF identifier,')
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

PROCEDURE AssignmentOrProcedureCall (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Designator(stopset0 + SetOfStop0{becomestok, lparatok}, stopset1, stopset2) ;
   IF currenttoken=becomestok
   THEN
      Expect(becomestok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
      Expression(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=lparatok
   THEN
      ActualParameters(stopset0, stopset1, stopset2) ;
   ELSE
      (* epsilon *)  ;
   END ;
END AssignmentOrProcedureCall ;


(*
   StatementSequence := Statement { ';' Statement  }

   first  symbols:semicolontok, returntok, exittok, asmtok, withtok, fortok, looptok, repeattok, whiletok, casetok, iftok, identtok
   
   reachend
*)

PROCEDURE StatementSequence (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Statement(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   WHILE currenttoken=semicolontok DO
      Expect(semicolontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
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

PROCEDURE IfStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(iftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok, thentok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
   Expression(stopset0, stopset1 + SetOfStop1{thentok}, stopset2) ;
   Expect(thentok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, elsiftok, elsetok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
   StatementSequence(stopset0, stopset1 + SetOfStop1{elsiftok, elsetok, endtok}, stopset2) ;
   WHILE currenttoken=elsiftok DO
      Expect(elsiftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok, thentok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
      Expression(stopset0, stopset1 + SetOfStop1{thentok}, stopset2) ;
      Expect(thentok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok, elsetok, elsiftok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok, elsetok, elsiftok}, stopset2) ;
   END (* while *) ;
   IF currenttoken=elsetok
   THEN
      Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   END ;
   Expect(endtok, stopset0, stopset1, stopset2) ;
END IfStatement ;


(*
   CaseStatement := 'CASE' Expression 'OF' Case { '|' Case  }
                    [ 'ELSE' StatementSequence  ]'END' 

   first  symbols:casetok
   
   cannot reachend
*)

PROCEDURE CaseStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(casetok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok, oftok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
   Expression(stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
   Expect(oftok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
   Case(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{elsetok, endtok}, stopset2) ;
   WHILE currenttoken=bartok DO
      Expect(bartok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
      Case(stopset0 + SetOfStop0{bartok}, stopset1 + SetOfStop1{endtok, elsetok}, stopset2) ;
   END (* while *) ;
   IF currenttoken=elsetok
   THEN
      Expect(elsetok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   END ;
   Expect(endtok, stopset0, stopset1, stopset2) ;
END CaseStatement ;


(*
   Case := CaseLabelList ':' StatementSequence 

   first  symbols:nottok, lparatok, stringtok, realtok, integertok, identtok, lcbratok, minustok, plustok
   
   cannot reachend
*)

PROCEDURE Case (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   CaseLabelList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
   Expect(colontok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
   StatementSequence(stopset0, stopset1, stopset2) ;
END Case ;


(*
   WhileStatement := 'WHILE' Expression 'DO' StatementSequence 
                     'END' 

   first  symbols:whiletok
   
   cannot reachend
*)

PROCEDURE WhileStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(whiletok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok, dotok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
   Expression(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   Expect(endtok, stopset0, stopset1, stopset2) ;
END WhileStatement ;


(*
   RepeatStatement := 'REPEAT' StatementSequence 'UNTIL' Expression 

   first  symbols:repeattok
   
   cannot reachend
*)

PROCEDURE RepeatStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(repeattok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok, untiltok}) ;
   StatementSequence(stopset0, stopset1, stopset2 + SetOfStop2{untiltok}) ;
   Expect(untiltok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
   Expression(stopset0, stopset1, stopset2) ;
END RepeatStatement ;


(*
   ForStatement := 'FOR' Ident ':=' Expression 'TO' Expression 
                   [ 'BY' ConstExpression  ]'DO' StatementSequence 
                   'END' 

   first  symbols:fortok
   
   cannot reachend
*)

PROCEDURE ForStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(fortok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   Ident(stopset0 + SetOfStop0{becomestok}, stopset1, stopset2) ;
   Expect(becomestok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok, totok}) ;
   Expression(stopset0, stopset1, stopset2 + SetOfStop2{totok}) ;
   Expect(totok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok, bytok, dotok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
   Expression(stopset0, stopset1 + SetOfStop1{bytok, dotok}, stopset2) ;
   IF currenttoken=bytok
   THEN
      Expect(bytok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
      ConstExpression(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
   END ;
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   Expect(endtok, stopset0, stopset1, stopset2) ;
END ForStatement ;


(*
   LoopStatement := 'LOOP' StatementSequence 'END' 

   first  symbols:looptok
   
   cannot reachend
*)

PROCEDURE LoopStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(looptok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   Expect(endtok, stopset0, stopset1, stopset2) ;
END LoopStatement ;


(*
   WithStatement := 'WITH' Designator 'DO' StatementSequence 
                    'END' 

   first  symbols:withtok
   
   cannot reachend
*)

PROCEDURE WithStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(withtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   Designator(stopset0, stopset1 + SetOfStop1{dotok}, stopset2) ;
   Expect(dotok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
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

PROCEDURE ProcedureDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   ProcedureHeading(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{consttok, proceduretok, moduletok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
   ProcedureBlock(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   PushAutoOn  ;
   
   Ident(stopset0, stopset1, stopset2) ;
   EndBuildProcedure  ;
   
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

PROCEDURE ProcedureHeading (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(proceduretok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   PushAutoOn  ;
   
   Ident(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
   StartBuildProcedure  ;
   
   PushAutoOff  ;
   
   IF currenttoken=lparatok
   THEN
      FormalParameters(stopset0, stopset1, stopset2) ;
   END ;
   PopAuto  ;
   
   BuildProcedureHeading  ;
   PopAuto  ;
END ProcedureHeading ;


(*
   ProcedureBlock := { Declaration  }'BEGIN' StatementSequence 
                     'END' 

   first  symbols:begintok, moduletok, proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

PROCEDURE ProcedureBlock (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Declaration(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
   Expect(begintok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
   StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   Expect(endtok, stopset0, stopset1, stopset2) ;
END ProcedureBlock ;


(*
   Block := { Declaration  }[ 'BEGIN' StatementSequence  ]'END' 

   first  symbols:endtok, begintok, moduletok, proceduretok, vartok, typetok, consttok
   
   cannot reachend
*)

PROCEDURE Block (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {moduletok, proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Declaration(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
   IF currenttoken=begintok
   THEN
      Expect(begintok, stopset0 + SetOfStop0{semicolontok}, stopset1 + SetOfStop1{iftok, casetok, repeattok, looptok, fortok, exittok, returntok, endtok}, stopset2 + SetOfStop2{identtok, whiletok, withtok, asmtok}) ;
      StatementSequence(stopset0, stopset1 + SetOfStop1{endtok}, stopset2) ;
   END ;
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

PROCEDURE Declaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=consttok
   THEN
      Expect(consttok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      WHILE currenttoken=identtok DO
         ConstantDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
   ELSIF currenttoken=typetok
   THEN
      Expect(typetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      WHILE currenttoken=identtok DO
         TypeDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
   ELSIF currenttoken=vartok
   THEN
      Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      WHILE currenttoken=identtok DO
         VariableDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=moduletok
   THEN
      ModuleDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: MODULE PROCEDURE VAR TYPE CONST')
   END ;
END Declaration ;


(*
   FormalParameters := '(' [ FPSection { ';' FPSection  } ]')' 
                       [ ':' Qualident  ]

   first  symbols:lparatok
   
   cannot reachend
*)

PROCEDURE FormalParameters (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(lparatok, stopset0 + SetOfStop0{rparatok}, stopset1, stopset2 + SetOfStop2{identtok, vartok}) ;
   IF ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, identtok}))
   THEN
      FPSection(stopset0 + SetOfStop0{semicolontok, rparatok}, stopset1, stopset2) ;
      WHILE currenttoken=semicolontok DO
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok, vartok}) ;
         FPSection(stopset0 + SetOfStop0{rparatok, semicolontok}, stopset1, stopset2) ;
      END (* while *) ;
   END ;
   Expect(rparatok, stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      Qualident(stopset0, stopset1, stopset2) ;
   END ;
END FormalParameters ;


(*
   FPSection := NonVarFPSection  | VarFPSection 

   first  symbols:vartok, identtok
   
   cannot reachend
*)

PROCEDURE FPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=identtok
   THEN
      NonVarFPSection(stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=vartok
   THEN
      VarFPSection(stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: VAR identifier,')
   END ;
END FPSection ;


(*
   VarFPSection := 'VAR' IdentList ':' FormalType 

   first  symbols:vartok
   
   cannot reachend
*)

PROCEDURE VarFPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
   Expect(colontok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
   FormalType(stopset0, stopset1, stopset2) ;
END VarFPSection ;


(*
   NonVarFPSection := IdentList ':' FormalType 

   first  symbols:identtok
   
   cannot reachend
*)

PROCEDURE NonVarFPSection (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IdentList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
   Expect(colontok, stopset0 + SetOfStop0{arraytok}, stopset1, stopset2 + SetOfStop2{identtok}) ;
   FormalType(stopset0, stopset1, stopset2) ;
END NonVarFPSection ;


(*
   FormalType := [ 'ARRAY' 'OF'  ]Qualident 

   first  symbols:identtok, arraytok
   
   cannot reachend
*)

PROCEDURE FormalType (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=arraytok
   THEN
      Expect(arraytok, stopset0, stopset1 + SetOfStop1{oftok}, stopset2) ;
      Expect(oftok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   END ;
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

PROCEDURE ModuleDeclaration (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   PushAutoOn  ;
   
   Ident(stopset0 + SetOfStop0{lsbratok, semicolontok}, stopset1, stopset2) ;
   StartBuildInnerModule  ;
   
   PushAutoOff  ;
   
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{fromtok, importtok, exporttok, consttok, proceduretok, moduletok, endtok}, stopset2 + SetOfStop2{typetok, vartok, identtok}) ;
   PushAutoOn  ;
   
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok, exporttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
      BuildImportInnerModule  ;
   END (* while *) ;
   IF currenttoken=exporttok
   THEN
      Export(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, moduletok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
      BuildExportInnerModule  ;
   END ;
   PopAuto  ;
   
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   PushAutoOn  ;
   
   Ident(stopset0, stopset1, stopset2) ;
   EndBuildInnerModule  ;
   
   PopAuto ; PopAuto ; PopAuto  ;
END ModuleDeclaration ;


(*
   Priority := '[' ConstExpression ']' 

   first  symbols:lsbratok
   
   cannot reachend
*)

PROCEDURE Priority (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(lsbratok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{identtok, integertok, realtok, stringtok}) ;
   ConstExpression(stopset0 + SetOfStop0{rsbratok}, stopset1, stopset2) ;
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

PROCEDURE Export (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(exporttok, stopset0, stopset1 + SetOfStop1{qualifiedtok, unqualifiedtok}, stopset2 + SetOfStop2{identtok}) ;
   IF currenttoken=qualifiedtok
   THEN
      Expect(qualifiedtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      PushT(QualifiedTok)  ;
      
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   ELSIF currenttoken=unqualifiedtok
   THEN
      Expect(unqualifiedtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      PushT(UnQualifiedTok)  ;
      
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   ELSE
      PushT(ExportTok)  ;
      
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
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

PROCEDURE Import (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=fromtok
   THEN
      Expect(fromtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      Ident(stopset0, stopset1 + SetOfStop1{importtok}, stopset2) ;
      Expect(importtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
   ELSIF currenttoken=importtok
   THEN
      Expect(importtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      PushT(ImportTok)
                                                                                   (* determines whether Ident or Module *)  ;
      
      IdentList(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: IMPORT FROM')
   END ;
END Import ;


(*
   DefinitionModule := 'DEFINITION' 'MODULE' 
                       % PushAutoOn  %
                       Ident 
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

PROCEDURE DefinitionModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(definitiontok, stopset0, stopset1 + SetOfStop1{moduletok}, stopset2) ;
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   PushAutoOn  ;
   
   Ident(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   P1StartBuildDefinitionModule  ;
   
   Expect(semicolontok, stopset0, stopset1 + SetOfStop1{fromtok, importtok, exporttok, consttok, proceduretok, endtok}, stopset2 + SetOfStop2{typetok, vartok}) ;
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {importtok, fromtok})) DO
      Import(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok, exporttok, importtok, fromtok}, stopset2 + SetOfStop2{vartok, typetok}) ;
      BuildImportOuterModule  ;
   END (* while *) ;
   IF currenttoken=exporttok
   THEN
      Export(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
      BuildExportOuterModule  ;
   END ;
   PushAutoOff  ;
   
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {proceduretok, consttok})) OR
         ((currenttoken>=totok) AND (currenttoken IN SetOfStop2 {vartok, typetok})) DO
      Definition(stopset0, stopset1 + SetOfStop1{endtok, proceduretok, consttok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
   PopAuto  ;
   
   Expect(endtok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
   P1EndBuildDefinitionModule  ;
   
   Expect(periodtok, stopset0, stopset1, stopset2) ;
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

PROCEDURE Definition (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=consttok
   THEN
      Expect(consttok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      WHILE currenttoken=identtok DO
         ConstantDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
   ELSIF currenttoken=typetok
   THEN
      Expect(typetok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      PushAutoOn  ;
      
      WHILE currenttoken=identtok DO
         Ident(stopset0 + SetOfStop0{semicolontok, equaltok}, stopset1, stopset2) ;
         IF currenttoken=semicolontok
         THEN
            Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
            BuildHiddenType  ;
         ELSIF currenttoken=equaltok
         THEN
            Expect(equaltok, stopset0 + SetOfStop0{lparatok, lsbratok, arraytok}, stopset1 + SetOfStop1{recordtok, settok, pointertok, proceduretok}, stopset2 + SetOfStop2{identtok}) ;
            Type(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
            Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
         ELSE
            ErrorMessage('expecting one of: = ;')
         END ;
      END (* while *) ;
      PopAuto  ;
   ELSIF currenttoken=vartok
   THEN
      Expect(vartok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      WHILE currenttoken=identtok DO
         VariableDeclaration(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
         Expect(semicolontok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
      END (* while *) ;
   ELSIF currenttoken=proceduretok
   THEN
      ProcedureHeading(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
      Expect(semicolontok, stopset0, stopset1, stopset2) ;
   ELSE
      ErrorMessage('expecting one of: PROCEDURE VAR TYPE CONST')
   END ;
END Definition ;


(*
   AsmStatement := 'ASM' [ 'VOLATILE'  ]'(' AsmOperands ')' 

   first  symbols:asmtok
   
   cannot reachend
*)

PROCEDURE AsmStatement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(asmtok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2 + SetOfStop2{volatiletok}) ;
   IF currenttoken=volatiletok
   THEN
      Expect(volatiletok, stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
   END ;
   Expect(lparatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
   AsmOperands(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END AsmStatement ;


(*
   AsmOperands := String [ ':' AsmList [ ':' AsmList [ ':' TrashList  ] ] ]

   first  symbols:stringtok
   
   cannot reachend
*)

PROCEDURE AsmOperands (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   String(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
   IF currenttoken=colontok
   THEN
      Expect(colontok, stopset0 + SetOfStop0{commatok, colontok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
      AsmList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
      IF currenttoken=colontok
      THEN
         Expect(colontok, stopset0 + SetOfStop0{commatok, colontok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
         AsmList(stopset0 + SetOfStop0{colontok}, stopset1, stopset2) ;
         IF currenttoken=colontok
         THEN
            Expect(colontok, stopset0 + SetOfStop0{commatok}, stopset1, stopset2 + SetOfStop2{stringtok}) ;
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

PROCEDURE AsmList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=stringtok
   THEN
      AsmElement(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END ;
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
      AsmElement(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END AsmList ;


(*
   AsmElement := String '(' Expression ')' 

   first  symbols:stringtok
   
   cannot reachend
*)

PROCEDURE AsmElement (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   String(stopset0 + SetOfStop0{lparatok}, stopset1, stopset2) ;
   Expect(lparatok, stopset0 + SetOfStop0{plustok, minustok, lcbratok, lparatok, rparatok}, stopset1 + SetOfStop1{nottok}, stopset2 + SetOfStop2{integertok, realtok, stringtok, identtok}) ;
   Expression(stopset0 + SetOfStop0{rparatok}, stopset1, stopset2) ;
   Expect(rparatok, stopset0, stopset1, stopset2) ;
END AsmElement ;


(*
   TrashList := [ String  ]{ ',' String  }

   first  symbols:commatok, stringtok
   
   reachend
*)

PROCEDURE TrashList (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   IF currenttoken=stringtok
   THEN
      String(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END ;
   WHILE currenttoken=commatok DO
      Expect(commatok, stopset0, stopset1, stopset2 + SetOfStop2{stringtok}) ;
      String(stopset0 + SetOfStop0{commatok}, stopset1, stopset2) ;
   END (* while *) ;
END TrashList ;



END P1SyntaxCheck.
