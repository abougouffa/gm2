(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                 2010
                 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE M2Quads ;


FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Debug IMPORT Assert, WriteDebug ;
FROM NameKey IMPORT Name, NulName, MakeKey, GetKey, makekey, KeyToCharStar ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3 ;

FROM M2MetaError IMPORT MetaError1, MetaError2, MetaError3,
                        MetaErrors1, MetaErrors2, MetaErrors3,
                        MetaErrorString1, MetaErrorString2 ;

FROM DynamicStrings IMPORT String, string, InitString, KillString, 
                           ConCat, InitStringCharStar, Dup, Mark,
                           PushAllocation, PopAllocationExemption,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;

FROM SymbolTable IMPORT ModeOfAddr, GetMode, PutMode, GetSymName, IsUnknown,
                        MakeTemporary,
                        MakeTemporaryFromExpression,
                        MakeTemporaryFromExpressions,
                        MakeConstLit, MakeConstLitString,
                        Make2Tuple,
                        RequestSym, MakePointer, PutPointer,
                        GetType, GetLowestType, SkipType,
                        GetScope, GetCurrentScope,
                        GetSubrange, SkipTypeAndSubrange,
                        GetModule, GetMainModule,
                        GetCurrentModule, GetFileModule, GetLocalSym,
                        GetStringLength, GetString,
                        GetArraySubscript, GetDimension,
                        GetParam,
                        GetNth, GetNthParam,
                        GetFirstUsed, GetDeclared,
                        GetQuads, GetReadQuads, GetWriteQuads,
                        GetWriteLimitQuads, GetReadLimitQuads,
                        GetVarScope,
                        GetModuleQuads, GetProcedureQuads,
                        MakeProcedure,
                        PutConstString,
                        PutModuleStartQuad, PutModuleEndQuad,
                        PutModuleFinallyStartQuad, PutModuleFinallyEndQuad,
                        PutProcedureStartQuad, PutProcedureEndQuad,
                        PutProcedureScopeQuad,
                        PutVar, PutConstSet,
                        GetVarPointerCheck, PutVarPointerCheck,
                        PutVarWritten,
                        PutReadQuad, RemoveReadQuad,
                        PutWriteQuad, RemoveWriteQuad,
                        PutPriority, GetPriority,
                        IsVarParam, IsProcedure, IsPointer, IsParameter,
                        IsUnboundedParam, IsEnumeration, IsDefinitionForC,
                        IsVarAParam, IsVarient, IsLegal,
                        UsesVarArgs, UsesOptArg,
                        GetOptArgInit,
                        IsReturnOptional,
                        NoOfElements,
                        NoOfParam,
                        StartScope, EndScope,
                        HasExceptionBlock, PutExceptionBlock,
                        HasExceptionFinally, PutExceptionFinally,
                        GetParent, GetRecord, IsRecordField, IsFieldVarient, IsRecord,
                        IsVar, IsProcType, IsType, IsSubrange, IsExported,
                        IsConst, IsConstString, IsModule, IsDefImp,
                        IsArray, IsUnbounded, IsProcedureNested,
                        IsPartialUnbounded, IsProcedureBuiltin,
                        IsSet, IsConstSet, IsConstructor, PutConst,
                        PutConstructor, PutConstructorFrom,
                        IsSubscript,
                        IsTemporary,
                        IsAModula2Type,
                        PutLeftValueFrontBackType,
                        PushSize, PushValue, PopValue,
                        GetVariableAtAddress, IsVariableAtAddress,

                        GetUnboundedRecordType,
                        GetUnboundedAddressOffset,
                        GetUnboundedHighOffset,
                        
                        ForeachFieldEnumerationDo, ForeachLocalSymDo,
                        GetExported, PutImported, GetSym,
                        NulSym ;

FROM M2Configure IMPORT PushParametersLeftToRight, UsingGCCBackend ;
FROM M2Batch IMPORT MakeDefinitionSource ;
FROM M2GCCDeclare IMPORT PutToBeSolvedByQuads ;

FROM FifoQueue IMPORT GetConstFromFifoQueue,
                      PutConstructorIntoFifoQueue, GetConstructorFromFifoQueue ;

FROM M2Comp IMPORT CompilingImplementationModule,
                   CompilingProgramModule ;

FROM M2LexBuf IMPORT currenttoken,
                     GetToken,
                     GetFileName,
                     GetTokenNo, GetLineNo, GetPreviousTokenLineNo ;

FROM M2Error IMPORT Error,
                    InternalError,
                    WriteFormat0, WriteFormat1, WriteFormat2, WriteFormat3,
                    NewError, NewWarning, ErrorFormat0, ErrorFormat1,
                    ErrorFormat2, ErrorFormat3, FlushErrors, ChainError,
                    ErrorString,
                    ErrorStringAt, ErrorStringAt2, ErrorStringsAt2,
                    WarnStringAt, WarnStringAt2, WarnStringsAt2 ;

FROM M2Printf IMPORT printf0, printf1, printf2, printf3, printf4 ;

FROM M2Reserved IMPORT PlusTok, MinusTok, TimesTok, DivTok, ModTok,
                       DivideTok, RemTok,
                       OrTok, AndTok, AmbersandTok,
                       EqualTok, LessEqualTok, GreaterEqualTok,
                       LessTok, GreaterTok, HashTok, LessGreaterTok,
                       InTok,
                       UpArrowTok, RParaTok, LParaTok, CommaTok,
                       NulTok, ByTok,
                       SemiColonTok, toktype ;

FROM M2Base IMPORT True, False, Boolean, Cardinal, Integer, Char,
                   Real, LongReal, ShortReal, Nil,
                   ZType, RType, CType,
                   Re, Im, Cmplx,
                   NegateType, ComplexToScalar, GetCmplxReturnType,
                   IsAssignmentCompatible, IsExpressionCompatible,
                   IsParameterCompatible,
                   AssignmentRequiresWarning,
                   CannotCheckTypeInPass3, ScalarToComplex, MixTypes,
                   CheckAssignmentCompatible, CheckExpressionCompatible,
                   High, LengthS, New, Dispose, Inc, Dec, Incl, Excl,
                   Cap, Abs, Odd,
                   IsOrd, Chr, Convert, Val, IsFloat, IsTrunc,
                   IsInt, Min, Max,
                   IsPseudoBaseProcedure, IsPseudoBaseFunction,
                   IsMathType, IsOrdinalType, IsRealType,
                   IsBaseType, GetBaseTypeMinMax, ActivationPointer ;

FROM M2System IMPORT IsPseudoSystemFunction, IsPseudoSystemProcedure,
                     IsSystemType, GetSystemTypeMinMax,
                     IsPseudoSystemFunctionConstExpression,
                     IsGenericSystemType,
                     Adr, TSize, AddAdr, SubAdr, DifAdr, Cast, Shift, Rotate,
                     MakeAdr, Address, Byte, Word, Loc, Throw ;

FROM M2Size IMPORT Size ;
FROM M2Bitset IMPORT Bitset ;

FROM M2ALU IMPORT PushInt, Gre, Less, PushNulSet, AddBitRange, AddBit,
                  IsGenericNulSet, IsValueAndTreeKnown, AddField,
                  AddElements, ChangeToConstructor ;

FROM Lists IMPORT List, InitList, GetItemFromList, NoOfItemsInList, PutItemIntoList,
                  IsItemInList, KillList, IncludeItemIntoList ;

FROM M2Constants IMPORT MakeNewConstFromValue ;

FROM M2Options IMPORT NilChecking,
                      WholeDivChecking, WholeValueChecking,
                      IndexChecking, RangeChecking,
                      CaseElseChecking, ReturnChecking,
                      Iso, Pim, Pim2, Pim3, Pim4, PositiveModFloorDiv,
                      Pedantic, CompilerDebugging, GenerateDebugging,
                      GenerateLineDebug, Exceptions,
                      Profiling, Coding, Optimizing ;

FROM M2Pass IMPORT IsPassCodeGeneration, IsNoPass ;

FROM M2StackAddress IMPORT StackOfAddress, InitStackAddress, KillStackAddress,
                           PushAddress, PopAddress, PeepAddress,
                           IsEmptyAddress, NoOfItemsInStackAddress ;

FROM M2StackWord IMPORT StackOfWord, InitStackWord, KillStackWord,
                        PushWord, PopWord, PeepWord,
                        IsEmptyWord, NoOfItemsInStackWord ;

FROM Indexing IMPORT Index, InitIndex, GetIndice, PutIndice, InBounds ;

FROM M2Range IMPORT InitAssignmentRangeCheck,
                    InitSubrangeRangeCheck,
                    InitStaticArraySubscriptRangeCheck,
                    InitDynamicArraySubscriptRangeCheck,
                    InitIncRangeCheck,
                    InitDecRangeCheck,
                    InitInclCheck,
                    InitExclCheck,
                    InitRotateCheck,
                    InitShiftCheck,
                    InitTypesAssignmentCheck,
                    InitTypesExpressionCheck,
                    InitTypesParameterCheck,
                    InitForLoopBeginRangeCheck,
                    InitForLoopToRangeCheck,
                    InitForLoopEndRangeCheck,
                    InitPointerRangeCheck,
                    InitNoReturnRangeCheck,
                    InitNoElseRangeCheck,
                    InitCaseBounds,
                    InitWholeZeroDivisionCheck,
                    InitWholeZeroDivisionCheck,
                    InitWholeZeroRemainderCheck,
                    CheckRangeAddVariableRead,
                    CheckRangeRemoveVariableRead,
                    WriteRangeCheck ;

FROM M2CaseList IMPORT PushCase, PopCase, AddRange, BeginCaseList, EndCaseList, ElseCase ;
FROM PCSymBuild IMPORT SkipConst ;

FROM gm2builtins IMPORT GetBuiltinTypeInfoType ;


CONST
   DebugStack = FALSE ;
   DebugVarients = FALSE ;
   BreakAtQuad = 37 ;

TYPE
   ConstructorFrame = POINTER TO constructorFrame ;
   constructorFrame = RECORD
                         type : CARDINAL ;
                         index: CARDINAL ;
                      END ;

   BoolFrame = POINTER TO boolFrame ;  (* using intemediate type helps p2c *)
   boolFrame =            RECORD
                             TrueExit : CARDINAL ;
                             FalseExit: CARDINAL ;
                             Unbounded: CARDINAL ;
                             BooleanOp: BOOLEAN ;
                             Dimension: CARDINAL ;
                             ReadWrite: CARDINAL ;
                             name     : CARDINAL ;
                          END ;

   QuadFrame = POINTER TO quadFrame ;  (* again we help p2c *)
   quadFrame =            RECORD
                             Operator           : QuadOperator ;
                             Operand1           : CARDINAL ;
                             Operand2           : CARDINAL ;
                             Operand3           : CARDINAL ;
                             Next               : CARDINAL ;     (* Next quadruple                 *)
                             LineNo             : CARDINAL ;     (* Line No of source text         *)
                             TokenNo            : CARDINAL ;     (* Token No of source text        *)
                             NoOfTimesReferenced: CARDINAL ;     (* No of times quad is referenced *)
                             CheckOverflow      : BOOLEAN ;      (* should backend check overflow  *)
                          END ;

   WithFrame = POINTER TO withFrame ;  (* again we help p2c *)
   withFrame =            RECORD
                             PtrSym   : CARDINAL ;
                             RecordSym: CARDINAL ;
                             rw       : CARDINAL ;  (* the record variable *)
                          END ;

   ForLoopInfo = RECORD
                    IncrementQuad,
                    StartOfForLoop,                              (* we keep a list of all for      *)
                    EndOfForLoop,                                (* loops so we can check index    *)
                    ForLoopIndex  : List ;                       (* variables are not abused       *)
                 END ;

   LineNote  = POINTER TO lineFrame ;  (* again we help p2c *)
   lineFrame =            RECORD
                             Line: CARDINAL ;
                             File: Name ;
                             Next: LineNote ;
                          END ;
VAR
   ConstructorStack,
   LineStack,
   BoolStack,
   WithStack            : StackOfAddress ;
   TryStack,
   CatchStack,
   ExceptStack,
   ConstStack,
   AutoStack,
   RepeatStack,
   WhileStack,
   ForStack,
   ExitStack,
   ReturnStack          : StackOfWord ;   (* Return quadruple of the procedure.      *)
   PriorityStack        : StackOfWord ;   (* temporary variable holding old priority *)
   SuppressWith         : BOOLEAN ;
   QuadArray            : Index ;
   NextQuad             : CARDINAL ;  (* Next quadruple number to be created.    *)
   FreeList             : CARDINAL ;  (* FreeList of quadruples.                 *)
   CurrentProc          : CARDINAL ;  (* Current procedure being compiled, used  *)
                                      (* to determine which procedure a RETURN   *)
                                      (* ReturnValueOp must have as its 3rd op.  *)
   InitQuad             : CARDINAL ;  (* Initial Quad BackPatch that starts the  *)
                                      (* suit of Modules.                        *)
   LastQuadNo           : CARDINAL ;  (* Last Quadruple accessed by GetQuad.     *)
   LogicalOrTok,                      (* Internal _LOR token.                    *)
   LogicalAndTok,                     (* Internal _LAND token.                   *)
   LogicalXorTok,                     (* Internal _LXOR token.                   *)
   LogicalDifferenceTok : Name ;      (* Internal _LDIFF token.                  *)
   InConstExpression,
   IsAutoOn,                          (* should parser automatically push idents *)
   MustNotCheckBounds   : BOOLEAN ;
   ForInfo              : ForLoopInfo ;  (* start and end of all FOR loops       *)
   GrowInitialization   : CARDINAL ;  (* upper limit of where the initialized    *)
                                      (* quadruples.                             *)
   BuildingHigh,
   BuildingSize,
   QuadrupleGeneration  : BOOLEAN ;      (* should we be generating quadruples?  *)
   FreeLineList         : LineNote ;  (* free list of line notes                 *)
   VarientFields        : List ;      (* the list of all varient fields created  *)
   VarientFieldNo       : CARDINAL ;  (* used to retrieve the VarientFields      *)
                                      (* in order.                               *)
   NoOfQuads            : CARDINAL ;  (* Number of used quadruples.              *)
   Head                 : CARDINAL ;  (* Head of the list of quadruples *)


(*
   Rules for file and initialization quadruples:

   StartModFileOp  - indicates that this file (module) has produced the
                     following code
   StartDefFileOp  - indicates that this definition module has produced
                     this code.
   EndFileOp       - indicates that a module has finished
   InitStartOp     - the start of the initialization code of a module
   InitEndOp       - the end of the above
   FinallyStartOp  - the start of the finalization code of a module
   FinallyEndOp    - the end of the above
*)

(* %%%FORWARD%%%
PROCEDURE doIndrX (des, exp: CARDINAL) ; FORWARD ;
PROCEDURE doConvert (type: CARDINAL; sym: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE PushTrw (True: WORD; rw: WORD) ; FORWARD ;
PROCEDURE PopTFrw (VAR True, False, rw: WORD) ; FORWARD ;
PROCEDURE PopTrw (VAR True, rw: WORD) ; FORWARD ;
PROCEDURE CheckConst (sym: CARDINAL) ; FORWARD ;
PROCEDURE doBuildAssignment (checkTypes, checkOverflow: BOOLEAN) ; FORWARD ;
PROCEDURE doBuildBinaryOp (checkTypes, checkOverflow: BOOLEAN) ; FORWARD ;
PROCEDURE DereferenceLValue (operand: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE BuildError (r: CARDINAL) ; FORWARD ;
PROCEDURE PushLineNote (l: LineNote) ; FORWARD ;
PROCEDURE PopLineNo () : LineNote ; FORWARD ;
PROCEDURE UseLineNote (l: LineNote) ; FORWARD ;
PROCEDURE BuildRealFuncProcCall (IsFunc, IsForC: BOOLEAN) ; FORWARD ;
PROCEDURE CheckForIndex (Start, End, Omit: CARDINAL; IndexSym: CARDINAL) ; FORWARD ;
PROCEDURE BuildMaxFunction ; FORWARD ;
PROCEDURE BuildMinFunction ; FORWARD ;
PROCEDURE BuildAddAdrFunction ; FORWARD ;
PROCEDURE BuildSubAdrFunction ; FORWARD ;
PROCEDURE BuildDifAdrFunction ; FORWARD ;
PROCEDURE BuildCastFunction ; FORWARD ;
PROCEDURE BuildShiftFunction ; FORWARD ;
PROCEDURE BuildRotateFunction ; FORWARD ;
PROCEDURE BuildMakeAdrFunction ; FORWARD ;
PROCEDURE CheckVariablesInBlock (BlockSym: CARDINAL) ; FORWARD ;
PROCEDURE CheckRemoveVariableRead (Sym: CARDINAL; canDereference: BOOLEAN; Quad: CARDINAL) ; FORWARD ;
PROCEDURE CheckRemoveVariableWrite (Sym: CARDINAL; canDereference: BOOLEAN; Quad: CARDINAL) ; FORWARD ;
PROCEDURE CheckFunctionReturn (ProcSym: CARDINAL) ; FORWARD ;
PROCEDURE CheckAddVariableWrite (Sym: CARDINAL; canDereference: BOOLEAN; Quad: CARDINAL) ; FORWARD ;
PROCEDURE CheckAddVariableRead (Sym: CARDINAL; canDereference: BOOLEAN; Quad: CARDINAL) ; FORWARD ;
PROCEDURE ConvertBooleanToVariable (i: CARDINAL) ; FORWARD ;
PROCEDURE BuildFloatFunction (Sym: CARDINAL) ; FORWARD ;
PROCEDURE BuildTruncFunction (Sym: CARDINAL) ; FORWARD ;
PROCEDURE CheckAssignCompatible (Des, Exp: CARDINAL) ; FORWARD ;
PROCEDURE CheckForLogicalOperator (Tok: Name; e1, t1, e2, t2: CARDINAL) : Name ; FORWARD ;
PROCEDURE DisplayType (Sym: CARDINAL) ; FORWARD ;
PROCEDURE CheckProcedureParameters (IsForC: BOOLEAN) ; FORWARD ;
PROCEDURE CheckParameter (Actual, Formal, ProcSym: CARDINAL; i: CARDINAL; TypeList: List) ; FORWARD ;
PROCEDURE FailParameter (CurrentState : ARRAY OF CHAR;
                         Given        : CARDINAL;
                         Expecting    : CARDINAL;
                         ProcedureSym : CARDINAL;
                         ParameterNo  : CARDINAL) ; FORWARD ;
PROCEDURE WarnParameter (CurrentState : ARRAY OF CHAR;
                         Given        : CARDINAL;
                         Expecting    : CARDINAL;
                         ProcedureSym : CARDINAL;
                         ParameterNo  : CARDINAL) ; FORWARD ;
PROCEDURE DisplayType (Sym: CARDINAL) ; FORWARD ;
PROCEDURE AlterReference (Head, OldQuad, NewQuad: CARDINAL) ; FORWARD ;
PROCEDURE RemoveReference (q: CARDINAL) ; FORWARD ;
PROCEDURE ManipulateReference (q: CARDINAL; to: CARDINAL) ; FORWARD ;
PROCEDURE AreConstant (b: BOOLEAN) : ModeOfAddr ; FORWARD ;
PROCEDURE AssignUnboundedNonVar (Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ; FORWARD ;
PROCEDURE AssignUnboundedVar (Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ; FORWARD ;
PROCEDURE BackPatch (QuadNo, Value: CARDINAL) ; FORWARD ;
PROCEDURE BuildAccessWithField ; FORWARD ;
PROCEDURE BuildAdrFunction ; FORWARD ;
PROCEDURE BuildChrFunction ; FORWARD ;
PROCEDURE BuildConvertFunction ; FORWARD ;
PROCEDURE BuildOddFunction ; FORWARD ;
PROCEDURE BuildAbsFunction ; FORWARD ;
PROCEDURE BuildCapFunction ; FORWARD ;
PROCEDURE BuildDecProcedure ; FORWARD ;
PROCEDURE BuildDisposeProcedure ; FORWARD ;
PROCEDURE BuildDynamicArray ; FORWARD ;
PROCEDURE BuildHighFromChar ; FORWARD ;
PROCEDURE BuildHighFromArray ; FORWARD ;
PROCEDURE BuildHighFromString ; FORWARD ;
PROCEDURE BuildHighFromUnbounded ; FORWARD ;
PROCEDURE BuildHighFunction ; FORWARD ;
PROCEDURE BuildLengthFunction ; FORWARD ;
PROCEDURE BuildIncProcedure ; FORWARD ;
PROCEDURE BuildNewProcedure ; FORWARD ;
PROCEDURE BuildInclProcedure ; FORWARD ;
PROCEDURE BuildExclProcedure ; FORWARD ;
PROCEDURE BuildOrdFunction (Sym: CARDINAL) ; FORWARD ;
PROCEDURE ManipulatePseudoCallParameters ; FORWARD ;
PROCEDURE BuildPseudoFunctionCall ; FORWARD ;
PROCEDURE BuildPseudoProcedureCall ; FORWARD ;
PROCEDURE BuildRealFunctionCall ; FORWARD ;
PROCEDURE BuildRealProcedureCall ; FORWARD ;
PROCEDURE BuildSizeFunction ; FORWARD ;
PROCEDURE BuildStaticArray ; FORWARD ;
PROCEDURE BuildTSizeFunction ; FORWARD ;
PROCEDURE BuildTypeCoercion ; FORWARD ;
PROCEDURE BuildValFunction ; FORWARD ;
PROCEDURE CheckBooleanId ; FORWARD ;
PROCEDURE DisplayQuad (QuadNo: CARDINAL) ; FORWARD ;
PROCEDURE DisposeQuad (QuadNo: CARDINAL) ; FORWARD ;
PROCEDURE GenQuad (Operation: QuadOperator;
                   Op1, Op2, Op3: CARDINAL) ; FORWARD ;
PROCEDURE GenQuadO (Operation: QuadOperator;
                    Op1, Op2, Op3: CARDINAL; overflow: BOOLEAN) ; FORWARD ;
PROCEDURE GetItemPointedTo (Sym: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE Init ; FORWARD ;
PROCEDURE InitQuads ; FORWARD ;
PROCEDURE IsBoolean (pos: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsReallyPointer (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE MakeOp (t: Name) : QuadOperator ; FORWARD ;
PROCEDURE ManipulateParameters (IsForC: BOOLEAN) ; FORWARD ;
PROCEDURE Merge (QuadList1, QuadList2: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE NewQuad (VAR QuadNo: CARDINAL) ; FORWARD ;
PROCEDURE PopBool (VAR True, False: CARDINAL) ; FORWARD ;
PROCEDURE PopExit() : CARDINAL ; FORWARD ;
PROCEDURE PopInit (VAR q: CARDINAL) ; FORWARD ;
PROCEDURE PopWith ; FORWARD ;
PROCEDURE PushBool (True, False: CARDINAL) ; FORWARD ;
PROCEDURE PushExit (Exit: CARDINAL) ; FORWARD ;
PROCEDURE PushWith (Sym, Type, RecordVar: CARDINAL) ; FORWARD ;
PROCEDURE PushFor (Exit: CARDINAL) ; FORWARD ;
PROCEDURE PopFor () : CARDINAL ; FORWARD ;
PROCEDURE UnboundedNonVarLinkToArray (Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ; FORWARD ;
PROCEDURE UnboundedVarLinkToArray (Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ; FORWARD ;
PROCEDURE WriteMode (Mode: ModeOfAddr) ; FORWARD ;
PROCEDURE WriteOperand (Sym: CARDINAL) ; FORWARD ;
PROCEDURE WriteOperator (Operator: QuadOperator) ; FORWARD ;
PROCEDURE WriteQuad (BufferQuad: CARDINAL) ; FORWARD ;
PROCEDURE IsBoolean (pos: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE OperandT (pos: CARDINAL) : WORD ; FORWARD ;
PROCEDURE OperandF (pos: CARDINAL) : WORD ; FORWARD ;
PROCEDURE OperandA (pos: CARDINAL) : WORD ; FORWARD ;
PROCEDURE OperandD (pos: CARDINAL) : WORD ; FORWARD ;
PROCEDURE OperandRW (pos: CARDINAL) : WORD ; FORWARD ;
PROCEDURE OperandMergeRW (pos: CARDINAL) : WORD ; FORWARD ;
PROCEDURE PushTFrw (True, False: WORD; rw: CARDINAL) ; FORWARD ;
PROCEDURE PopN (n: CARDINAL) ; FORWARD ;
PROCEDURE PushTFAD (True, False, Array, Dim: WORD) ; FORWARD ;
PROCEDURE PushTFADrw (True, False, Array, Dim, rw: WORD) ; FORWARD ;
PROCEDURE PushTFD (True, False, Dim: WORD) ; FORWARD ;
PROCEDURE PushTFDrw (True, False, Dim, rw: WORD) ; FORWARD ;
PROCEDURE GetQualidentImport (n, module: Name) : CARDINAL ; FORWARD ;
PROCEDURE CheckNeedPriorityBegin (scope, module: CARDINAL) ; FORWARD ;
PROCEDURE CheckNeedPriorityEnd (scope, module: CARDINAL) ; FORWARD ;
PROCEDURE CheckVariablesAt (scope: CARDINAL) ; FORWARD ;
PROCEDURE CheckVariableAt (sym: CARDINAL) ; FORWARD ;
PROCEDURE CheckAddVariableReadLeftValue (sym: CARDINAL; q: CARDINAL) ; FORWARD ;
PROCEDURE CheckRemoveVariableReadLeftValue (sym: CARDINAL; q: CARDINAL) ; FORWARD ;
PROCEDURE BuildThrowProcedure ; FORWARD ;
PROCEDURE BuildRTExceptEnter ; FORWARD ;
PROCEDURE BuildRTExceptLeave (destroy: BOOLEAN) ; FORWARD ;
PROCEDURE BuildReFunction ; FORWARD ;
PROCEDURE BuildImFunction ; FORWARD ;
PROCEDURE BuildCmplxFunction ; FORWARD ;
PROCEDURE BuildConstHighFromSym ; FORWARD ;
PROCEDURE IncOperandD (pos: CARDINAL) ; FORWARD ;
PROCEDURE calculateMultipicand (arraySym, arrayType: CARDINAL; dim: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE BuildIntFunction (Sym: CARDINAL) ; FORWARD ;
PROCEDURE PushTFD (True, False, Dim: WORD) ; FORWARD ;
PROCEDURE PopTFD (VAR True, False, Dim: WORD) ; FORWARD ;
   %%%FORWARD%%% *)


(*
#define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#define Dup(X) DupDB(X, __FILE__, __LINE__)
#define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
*)


(*
   doDSdbEnter - 
*)

PROCEDURE doDSdbEnter ;
BEGIN
   PushAllocation
END doDSdbEnter ;


(*
   doDSdbExit - 
*)

PROCEDURE doDSdbExit (s: String) ;
BEGIN
   s := PopAllocationExemption(TRUE, s)
END doDSdbExit ;


(*
   DSdbEnter - 
*)

PROCEDURE DSdbEnter ;
BEGIN
END DSdbEnter ;


(*
   DSdbExit - 
*)

PROCEDURE DSdbExit (s: String) ;
BEGIN
END DSdbExit ;


(*
#define DBsbEnter doDBsbEnter
#define DBsbExit  doDBsbExit
*)


(*
   SetOptionProfiling - builds a profile quadruple if the profiling
                        option was given to the compiler.
*)

PROCEDURE SetOptionProfiling (b: BOOLEAN) ;
BEGIN
   IF b#Profiling
   THEN
      IF b
      THEN
         BuildProfileOn
      ELSE
         BuildProfileOff
      END ;
      Profiling := b
   END
END SetOptionProfiling ;


(*
   SetOptionCoding - builds a code quadruple if the profiling
                        option was given to the compiler.
*)

PROCEDURE SetOptionCoding (b: BOOLEAN) ;
BEGIN
   IF b#Coding
   THEN
      IF b
      THEN
         BuildCodeOn
      ELSE
         BuildCodeOff
      END ;
      Coding := b
   END
END SetOptionCoding ;


(*
   SetOptionOptimizing - builds a quadruple to say that the optimization option
                         has been found in a comment.
*)

PROCEDURE SetOptionOptimizing (b: BOOLEAN) ;
BEGIN
   IF b
   THEN
      BuildOptimizeOn
   ELSE
      BuildOptimizeOff
   END
END SetOptionOptimizing ;


(*
   GetQF - returns the QuadFrame associated with, q.
*)

PROCEDURE GetQF (q: CARDINAL) : QuadFrame ;
BEGIN
   RETURN( GetIndice(QuadArray, q) )
END GetQF ;


(*
   Opposite - returns the opposite comparison operator.
*)

PROCEDURE Opposite (Operator: QuadOperator) : QuadOperator ;
VAR
   Op: QuadOperator ;
BEGIN
   CASE Operator OF

   IfNotEquOp : Op := IfEquOp |
   IfEquOp    : Op := IfNotEquOp |
   IfLessEquOp: Op := IfGreOp |
   IfGreOp    : Op := IfLessEquOp |
   IfGreEquOp : Op := IfLessOp |
   IfLessOp   : Op := IfGreEquOp |
   IfInOp     : Op := IfNotInOp |
   IfNotInOp  : Op := IfInOp

   ELSE
      InternalError('unexpected operator', __FILE__, __LINE__)
   END ;
   RETURN( Op )
END Opposite ;


(*
   IsReferenced - returns true if QuadNo is referenced by another quadruple.
*)

PROCEDURE IsReferenced (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      RETURN( (Operator=ProcedureScopeOp) OR (Operator=NewLocalVarOp) OR
              (NoOfTimesReferenced>0) )
   END
END IsReferenced ;


(*
   IsBackReference - returns TRUE if quadruple, q, is referenced from a quad further on.
*)

PROCEDURE IsBackReference (q: CARDINAL) : BOOLEAN ;
VAR
   i            : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   i := q ;
   WHILE i#0 DO
      GetQuad(i, op, op1, op2, op3) ;
      CASE op OF

      NewLocalVarOp,
      KillLocalVarOp,
      FinallyStartOp,
      FinallyEndOp,
      InitEndOp,
      InitStartOp,
      EndFileOp,
      StartDefFileOp,
      StartModFileOp:  RETURN( FALSE ) |       (* run into end of procedure or module *)

      GotoOp,
      IfEquOp,
      IfLessEquOp,
      IfGreEquOp,
      IfGreOp,
      IfLessOp,
      IfNotEquOp,
      IfInOp,
      IfNotInOp     :  IF op3=q
                       THEN
                          RETURN( TRUE )
                       END

      END ;
      i := GetNextQuad(i)
   END ;
   InternalError('fix this for the sake of efficiency..', __FILE__, __LINE__)
END IsBackReference ;


(*
   IsUnConditional - returns true if QuadNo is an unconditional jump.
*)

PROCEDURE IsUnConditional (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      CASE Operator OF

      ThrowOp,
      RetryOp,
      CallOp,
      ReturnOp,
      GotoOp  : RETURN( TRUE )

      ELSE
         RETURN( FALSE )
      END
   END
END IsUnConditional ;


(*
   IsConditional - returns true if QuadNo is a conditional jump.
*)

PROCEDURE IsConditional (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      CASE Operator OF

      IfInOp,
      IfNotInOp,
      IfEquOp,
      IfNotEquOp,
      IfLessOp,
      IfLessEquOp,
      IfGreOp,
      IfGreEquOp : RETURN( TRUE )

      ELSE
         RETURN( FALSE )
      END ;
   END
END IsConditional ;


(*
   IsBackReferenceConditional - returns TRUE if quadruple, q, is referenced from
                                a conditional quad further on.
*)

PROCEDURE IsBackReferenceConditional (q: CARDINAL) : BOOLEAN ;
VAR
   i            : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   i := q ;
   WHILE i#0 DO
      GetQuad(i, op, op1, op2, op3) ;
      CASE op OF

      NewLocalVarOp,
      KillLocalVarOp,
      FinallyStartOp,
      FinallyEndOp,
      InitEndOp,
      InitStartOp,
      EndFileOp,
      StartDefFileOp,
      StartModFileOp:  RETURN( FALSE ) |       (* run into end of procedure or module *)

      TryOp,
      RetryOp,
      GotoOp,
      IfEquOp,
      IfLessEquOp,
      IfGreEquOp,
      IfGreOp,
      IfLessOp,
      IfNotEquOp,
      IfInOp,
      IfNotInOp     :  IF (op3=q) AND IsConditional(q)
                       THEN
                          RETURN( TRUE )
                       END

      END ;
      i := GetNextQuad(i)
   END ;
   InternalError('fix this for the sake of efficiency..', __FILE__, __LINE__)
END IsBackReferenceConditional ;


(*
   IsQuadA - returns true if QuadNo is a op.
*)

PROCEDURE IsQuadA (QuadNo: CARDINAL; op: QuadOperator) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      RETURN( Operator=op )
   END
END IsQuadA ;


(*
   IsCall - returns true if QuadNo is a call operation.
*)

PROCEDURE IsCall (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, CallOp) )
END IsCall ;


(*
   IsReturn - returns true if QuadNo is a return operation.
*)

PROCEDURE IsReturn (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, ReturnOp) )
END IsReturn ;


(* 
   IsNewLocalVar - returns true if QuadNo is a NewLocalVar operation.
*) 
 
PROCEDURE IsNewLocalVar (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, NewLocalVarOp) )
END IsNewLocalVar ;


(* 
   IsKillLocalVar - returns true if QuadNo is a KillLocalVar operation.
*) 
 
PROCEDURE IsKillLocalVar (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, KillLocalVarOp) )
END IsKillLocalVar ;


(* 
   IsProcedureScope - returns true if QuadNo is a ProcedureScope operation.
*) 
 
PROCEDURE IsProcedureScope (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, ProcedureScopeOp) )
END IsProcedureScope ;


(* 
   IsCatchBegin - returns true if QuadNo is a catch begin quad.
*) 
 
PROCEDURE IsCatchBegin (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, CatchBeginOp) )
END IsCatchBegin ;


(* 
   IsCatchEnd - returns true if QuadNo is a catch end quad.
*) 
 
PROCEDURE IsCatchEnd (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, CatchEndOp) )
END IsCatchEnd ;


(* 
   IsInitStart - returns true if QuadNo is a init start quad.
*) 
 
PROCEDURE IsInitStart (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, InitStartOp) )
END IsInitStart ;


(* 
   IsInitEnd - returns true if QuadNo is a init end quad.
*) 
 
PROCEDURE IsInitEnd (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, InitEndOp) )
END IsInitEnd ;


(* 
   IsFinallyStart - returns true if QuadNo is a finally start quad.
*) 
 
PROCEDURE IsFinallyStart (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, FinallyStartOp) )
END IsFinallyStart ;


(* 
   IsFinallyEnd - returns true if QuadNo is a finally end quad.
*) 
 
PROCEDURE IsFinallyEnd (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, FinallyEndOp) )
END IsFinallyEnd ;


(*
   IsInitialisingConst - returns TRUE if the quadruple is setting
                         a const (op1) with a value.
*)

PROCEDURE IsInitialisingConst (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   GetQuad(QuadNo, op, op1, op2, op3) ;
   CASE op OF

   InclOp,
   ExclOp,
   UnboundedOp,
   FunctValueOp,
   NegateOp,
   BecomesOp,
   HighOp,
   SizeOp,
   AddrOp,
   OffsetOp,
   ArrayOp,
   LogicalShiftOp,
   LogicalRotateOp,
   LogicalOrOp,
   LogicalAndOp,
   LogicalXorOp,
   CoerceOp,
   ConvertOp,
   CastOp,
   AddOp,
   SubOp,
   MultOp,
   ModFloorOp,
   DivFloorOp,
   ModTruncOp,
   DivTruncOp,
   XIndrOp,
   IndrXOp,
   SaveExceptionOp,
   RestoreExceptionOp:  RETURN( IsConst(op1) )

   ELSE
      RETURN( FALSE )
   END
END IsInitialisingConst ;


(*
   IsOptimizeOn - returns true if the Optimize flag was true at QuadNo.
*)

PROCEDURE IsOptimizeOn (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f : QuadFrame ;
   n,
   q : CARDINAL ;
   On: BOOLEAN ;
BEGIN
   On := Optimizing ;
   q := Head ;
   WHILE (q#0) AND (q#QuadNo) DO
      f := GetQF(q) ;
      WITH f^ DO
         IF Operator=OptimizeOnOp
         THEN
            On := TRUE
         ELSIF Operator=OptimizeOffOp
         THEN
            On := FALSE
         END ;
         n := Next
      END ;
      q := n
   END ;
   RETURN( On )
END IsOptimizeOn ;


(*
   IsProfileOn - returns true if the Profile flag was true at QuadNo.
*)

PROCEDURE IsProfileOn (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f : QuadFrame ;
   n,
   q : CARDINAL ;
   On: BOOLEAN ;
BEGIN
   On := Profiling ;
   q := Head ;
   WHILE (q#0) AND (q#QuadNo) DO
      f := GetQF(q) ;
      WITH f^ DO
         IF Operator=ProfileOnOp
         THEN
            On := TRUE
         ELSIF Operator=ProfileOffOp
         THEN
            On := FALSE
         END ;
         n := Next
      END ;
      q := n
   END ;
   RETURN( On )
END IsProfileOn ;


(*
   IsCodeOn - returns true if the Code flag was true at QuadNo.
*)

PROCEDURE IsCodeOn (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f : QuadFrame ;
   n,
   q : CARDINAL ;
   On: BOOLEAN ;
BEGIN
   On := Coding ;
   q := Head ;
   WHILE (q#0) AND (q#QuadNo) DO
      f := GetQF(q) ;
      WITH f^ DO
         IF Operator=CodeOnOp
         THEN
            On := TRUE
         ELSIF Operator=CodeOffOp
         THEN
            On := FALSE
         END ;
         n := Next
      END ;
      q := n
   END ;
   RETURN( On )
END IsCodeOn ;


(*
   IsDefOrModFile - returns TRUE if QuadNo is a start of Module or Def file
                    directive.
*)

PROCEDURE IsDefOrModFile (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      RETURN( (Operator=StartDefFileOp) OR (Operator=StartModFileOp) )
   END
END IsDefOrModFile ;


(*
   IsPseudoQuad - returns true if QuadNo is a compiler directive.
                  ie code, profile and optimize.
                     StartFile, EndFile,
*)

PROCEDURE IsPseudoQuad (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      RETURN( (Operator=CodeOnOp) OR (Operator=CodeOffOp) OR
              (Operator=ProfileOnOp) OR (Operator=ProfileOffOp) OR
              (Operator=OptimizeOnOp) OR (Operator=OptimizeOffOp) OR
              (Operator=EndFileOp) OR
              (Operator=StartDefFileOp) OR (Operator=StartModFileOp)
            )
   END
END IsPseudoQuad ;


(*
   GetLastFileQuad - returns the Quadruple number of the last StartDefFile or
                     StartModFile quadruple.
*)

PROCEDURE GetLastFileQuad (QuadNo: CARDINAL) : CARDINAL ;
VAR
   f       : QuadFrame ;
   q, i,
   FileQuad: CARDINAL ;
BEGIN
   q := Head ;
   FileQuad := 0 ;
   REPEAT
      f := GetQF(q) ;
      WITH f^ DO
         IF (Operator=StartModFileOp) OR (Operator=StartDefFileOp)
         THEN
            FileQuad := q
         END ;
         i := Next
      END ;
      q := i
   UNTIL (i=QuadNo) OR (i=0) ;
   Assert(i#0) ;
   Assert(FileQuad#0) ;
   RETURN( FileQuad )
END GetLastFileQuad ;


(*
   GetLastQuadNo - returns the last quadruple number referenced
                   by a GetQuad.
*)

PROCEDURE GetLastQuadNo () : CARDINAL ;
BEGIN
   RETURN( LastQuadNo )
END GetLastQuadNo ;


(*
   QuadToLineNo - Converts a QuadNo into the approprate line number of the
                  source file, the line number is returned.

                  This may be used to yield an idea where abouts in the
                  source file the code generetion is
                  processing.
*)

PROCEDURE QuadToLineNo (QuadNo: CARDINAL) : CARDINAL ;
VAR
   f: QuadFrame ;
BEGIN
   IF (LastQuadNo=0) AND (NOT IsNoPass()) AND (NOT IsPassCodeGeneration())
   THEN
      RETURN( 0 )
   ELSE
      f := GetQF(QuadNo) ;
      RETURN( f^.LineNo )
   END
END QuadToLineNo ;


(*
   QuadToTokenNo - Converts a QuadNo into the approprate token number of the
                   source file, the line number is returned.

                   This may be used to yield an idea where abouts in the
                   source file the code generetion is
                   processing.
*)

PROCEDURE QuadToTokenNo (QuadNo: CARDINAL) : CARDINAL ;
VAR
   f: QuadFrame ;
BEGIN
   IF (LastQuadNo=0) AND (NOT IsNoPass()) AND (NOT IsPassCodeGeneration())
   THEN
      RETURN( 0 )
   ELSE
      f := GetQF(QuadNo) ;
      RETURN( f^.TokenNo )
   END
END QuadToTokenNo ;


(*
   GetQuad - returns the Quadruple QuadNo.
*)

PROCEDURE GetQuad (QuadNo: CARDINAL;
                   VAR Op: QuadOperator;
                   VAR Oper1, Oper2, Oper3: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   LastQuadNo := QuadNo ;
   WITH f^ DO
      Op := Operator ;
      Oper1 := Operand1 ;
      Oper2 := Operand2 ;
      Oper3 := Operand3
   END
END GetQuad ;


(*
   AddQuadInformation - adds variable analysis and jump analysis to the new quadruple.
*)

PROCEDURE AddQuadInformation (QuadNo: CARDINAL;
                              Op: QuadOperator;
                              Oper1, Oper2, Oper3: CARDINAL) ;
BEGIN
   CASE Op OF

   IfInOp,
   IfNotInOp,
   IfEquOp,
   IfNotEquOp,
   IfLessOp,
   IfLessEquOp,
   IfGreOp,
   IfGreEquOp : ManipulateReference(QuadNo, Oper3) ;
                CheckAddVariableRead(Oper1, FALSE, QuadNo) ;
                CheckAddVariableRead(Oper2, FALSE, QuadNo) |

   TryOp,
   RetryOp,
   GotoOp     : ManipulateReference(QuadNo, Oper3) |

   (* variable references *)

   InclOp,
   ExclOp            : CheckConst(Oper1) ;
                       CheckAddVariableRead(Oper3, FALSE, QuadNo) ;
                       CheckAddVariableWrite(Oper1, TRUE, QuadNo) |
   UnboundedOp,
   FunctValueOp,
   NegateOp,
   BecomesOp,
   HighOp,
   SizeOp            : CheckConst(Oper1) ;
                       CheckAddVariableWrite(Oper1, FALSE, QuadNo) ;
                       CheckAddVariableRead(Oper3, FALSE, QuadNo) |
   AddrOp            : CheckConst(Oper1) ;
                       CheckAddVariableWrite(Oper1, FALSE, QuadNo) ;
                       (* CheckAddVariableReadLeftValue(Oper3, QuadNo) *)
                       (* the next line is a kludge and assumes we _will_
                          write to the variable as we have taken its address *)
                       CheckRemoveVariableWrite(Oper1, TRUE, QuadNo) |
   ReturnValueOp     : CheckAddVariableRead(Oper1, FALSE, QuadNo) |
   ReturnOp,
   NewLocalVarOp,
   KillLocalVarOp    : |
   CallOp            : CheckAddVariableRead(Oper3, TRUE, QuadNo) |

   ParamOp           : CheckAddVariableRead(Oper2, FALSE, QuadNo) ;
                       CheckAddVariableRead(Oper3, FALSE, QuadNo) ;
                       IF (Oper1>0) AND (Oper1<=NoOfParam(Oper2)) AND
                          IsVarParam(Oper2, Oper1)
                       THEN
                          (* _may_ also write to a var parameter, although we dont know *)
                          CheckAddVariableWrite(Oper3, TRUE, QuadNo)
                       END |
   OffsetOp,
   ArrayOp,
   LogicalShiftOp,
   LogicalRotateOp,
   LogicalOrOp,
   LogicalAndOp,
   LogicalXorOp,
   CoerceOp,
   ConvertOp,
   CastOp,
   AddOp,
   SubOp,
   MultOp,
   ModFloorOp,
   DivFloorOp,
   ModTruncOp,
   DivTruncOp        : CheckConst(Oper1) ;
                       CheckAddVariableWrite(Oper1, FALSE, QuadNo) ;
                       CheckAddVariableRead(Oper2, FALSE, QuadNo) ;
                       CheckAddVariableRead(Oper3, FALSE, QuadNo) |

   XIndrOp           : CheckConst(Oper1) ;
                       CheckAddVariableWrite(Oper1, TRUE, QuadNo) ;
                       CheckAddVariableRead(Oper3, FALSE, QuadNo) |

   IndrXOp           : CheckConst(Oper1) ;
                       CheckAddVariableWrite(Oper1, FALSE, QuadNo) ;
                       CheckAddVariableRead(Oper3, TRUE, QuadNo) |

   RangeCheckOp      : CheckRangeAddVariableRead(Oper3, QuadNo) |
   SaveExceptionOp   : CheckConst(Oper1) ;
                       CheckAddVariableWrite(Oper1, FALSE, QuadNo) |
   RestoreExceptionOp: CheckAddVariableRead(Oper1, FALSE, QuadNo)

   ELSE
   END
END AddQuadInformation ;


PROCEDURE stop ; BEGIN END stop ;


(*
   PutQuadO - alters a quadruple QuadNo with Op, Oper1, Oper2, Oper3, and
              sets a boolean to determinine whether overflow should be checked.
*)

PROCEDURE PutQuadO (QuadNo: CARDINAL;
                    Op: QuadOperator;
                    Oper1, Oper2, Oper3: CARDINAL;
                    overflow: BOOLEAN) ;
VAR
   f: QuadFrame ;
BEGIN
   IF QuadNo=BreakAtQuad
   THEN
      stop
   END ;
   IF QuadrupleGeneration
   THEN
      EraseQuad(QuadNo) ;
      AddQuadInformation(QuadNo, Op, Oper1, Oper2, Oper3) ;
      f := GetQF(QuadNo) ;
      WITH f^ DO
         Operator      := Op ;
         Operand1      := Oper1 ;
         Operand2      := Oper2 ;
         Operand3      := Oper3 ;
         CheckOverflow := overflow
      END
   END
END PutQuadO ;


(*
   PutQuad - overwrites a quadruple QuadNo with Op, Oper1, Oper2, Oper3
*)

PROCEDURE PutQuad (QuadNo: CARDINAL;
                   Op: QuadOperator;
                   Oper1, Oper2, Oper3: CARDINAL) ;
BEGIN
   PutQuadO(QuadNo, Op, Oper1, Oper2, Oper3, TRUE)
END PutQuad ;


(*
   UndoReadWriteInfo - 
*)

PROCEDURE UndoReadWriteInfo (QuadNo: CARDINAL;
                             Op: QuadOperator;
                             Oper1, Oper2, Oper3: CARDINAL) ;
BEGIN
   CASE Op OF

   (* jumps, calls and branches *)
   IfInOp,
   IfNotInOp,
   IfEquOp,
   IfNotEquOp,
   IfLessOp,
   IfLessEquOp,
   IfGreOp,
   IfGreEquOp        : RemoveReference(QuadNo) ;
                       CheckRemoveVariableRead(Oper1, FALSE, QuadNo) ;
                       CheckRemoveVariableRead(Oper2, FALSE, QuadNo) |

   TryOp,
   RetryOp,
   GotoOp            : RemoveReference(QuadNo) |

   (* variable references *)

   InclOp,
   ExclOp            : CheckRemoveVariableRead(Oper1, FALSE, QuadNo) ;
                       CheckRemoveVariableWrite(Oper1, TRUE, QuadNo) |

   UnboundedOp,
   FunctValueOp,
   NegateOp,
   BecomesOp,
   HighOp,
   SizeOp            : CheckRemoveVariableWrite(Oper1, FALSE, QuadNo) ;
                       CheckRemoveVariableRead(Oper3, FALSE, QuadNo) |
   AddrOp            : CheckRemoveVariableWrite(Oper1, FALSE, QuadNo) ;
                       (* CheckRemoveVariableReadLeftValue(Oper3, QuadNo) ; *)
                       (* the next line is a kludge and assumes we _will_
                          write to the variable as we have taken its address *)
                       CheckRemoveVariableWrite(Oper1, TRUE, QuadNo) |
   ReturnValueOp     : CheckRemoveVariableRead(Oper1, FALSE, QuadNo) |
   ReturnOp,
   CallOp,
   NewLocalVarOp,
   KillLocalVarOp    : |
   ParamOp           : CheckRemoveVariableRead(Oper2, FALSE, QuadNo) ;
                       CheckRemoveVariableRead(Oper3, FALSE, QuadNo) ;
                       IF (Oper1>0) AND (Oper1<=NoOfParam(Oper2)) AND
                          IsVarParam(Oper2, Oper1)
                       THEN
                          (* _may_ also write to a var parameter, although we dont know *)
                          CheckRemoveVariableWrite(Oper3, TRUE, QuadNo)
                       END |
   OffsetOp,
   ArrayOp,
   LogicalShiftOp,
   LogicalRotateOp,
   LogicalOrOp,
   LogicalAndOp,
   LogicalXorOp,
   CoerceOp,
   ConvertOp,
   CastOp,
   AddOp,
   SubOp,
   MultOp,
   ModFloorOp,
   DivFloorOp,
   ModTruncOp,
   DivTruncOp        : CheckRemoveVariableWrite(Oper1, FALSE, QuadNo) ;
                       CheckRemoveVariableRead(Oper2, FALSE, QuadNo) ;
                       CheckRemoveVariableRead(Oper3, FALSE, QuadNo) |

   XIndrOp           : CheckRemoveVariableWrite(Oper1, TRUE, QuadNo) ;
                       CheckRemoveVariableRead(Oper3, FALSE, QuadNo) |

   IndrXOp           : CheckRemoveVariableWrite(Oper1, FALSE, QuadNo) ;
                       CheckRemoveVariableRead(Oper3, TRUE, QuadNo) |

   RangeCheckOp      : CheckRangeRemoveVariableRead(Oper3, QuadNo) |
   SaveExceptionOp   : CheckRemoveVariableWrite(Oper1, FALSE, QuadNo) |
   RestoreExceptionOp: CheckRemoveVariableRead(Oper1, FALSE, QuadNo)

   ELSE
   END
END UndoReadWriteInfo ;


(*
   EraseQuad - erases a quadruple QuadNo, the quadruple is still in the list
               but wiped clean.
*)

PROCEDURE EraseQuad (QuadNo: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      UndoReadWriteInfo(QuadNo, Operator, Operand1, Operand2, Operand3) ;
      Operator := DummyOp ;   (* finally blank it out *)
      Operand1 := 0 ;
      Operand2 := 0 ;
      Operand3 := 0
   END
END EraseQuad ;


(*
   CheckAddVariableReadLeftValue - 
*)

PROCEDURE CheckAddVariableReadLeftValue (sym: CARDINAL; q: CARDINAL) ;
BEGIN
   IF IsVar(sym)
   THEN
      PutReadQuad(sym, LeftValue, q)
   END
END CheckAddVariableReadLeftValue ;


(*
   CheckRemoveVariableReadLeftValue - 
*)

PROCEDURE CheckRemoveVariableReadLeftValue (sym: CARDINAL; q: CARDINAL) ;
BEGIN
   IF IsVar(sym)
   THEN
      RemoveReadQuad(sym, LeftValue, q)
   END
END CheckRemoveVariableReadLeftValue ;


(*
   CheckAddVariableRead - checks to see whether symbol, Sym, is a variable or
                          a parameter and if so it then adds this quadruple
                          to the variable list.
*)

PROCEDURE CheckAddVariableRead (Sym: CARDINAL; canDereference: BOOLEAN; Quad: CARDINAL) ;
BEGIN
   IF IsVar(Sym)
   THEN
      PutReadQuad(Sym, GetMode(Sym), Quad) ;
      IF (GetMode(Sym)=LeftValue) AND canDereference
      THEN
         PutReadQuad(Sym, RightValue, Quad)
      END
   END
END CheckAddVariableRead ;


(*
   CheckRemoveVariableRead - checks to see whether, Sym, is a variable or
                             a parameter and if so then it removes the
                             quadruple from the variable list.
*)

PROCEDURE CheckRemoveVariableRead (Sym: CARDINAL; canDereference: BOOLEAN; Quad: CARDINAL) ;
BEGIN
   IF IsVar(Sym)
   THEN
      RemoveReadQuad(Sym, GetMode(Sym), Quad) ;
      IF (GetMode(Sym)=LeftValue) AND canDereference
      THEN
         RemoveReadQuad(Sym, RightValue, Quad)
      END
   END
END CheckRemoveVariableRead ;


(*
   CheckAddVariableWrite - checks to see whether symbol, Sym, is a variable and
                           if so it then adds this quadruple to the variable list.
*)

PROCEDURE CheckAddVariableWrite (Sym: CARDINAL; canDereference: BOOLEAN; Quad: CARDINAL) ;
BEGIN
   IF IsVar(Sym)
   THEN
      IF (GetMode(Sym)=LeftValue) AND canDereference
      THEN
         PutReadQuad(Sym, LeftValue, Quad) ;
         PutWriteQuad(Sym, RightValue, Quad)
      ELSE
         PutWriteQuad(Sym, GetMode(Sym), Quad)
      END
   END
END CheckAddVariableWrite ;


(*
   CheckRemoveVariableWrite - checks to see whether, Sym, is a variable and
                              if so then it removes the quadruple from the
                              variable list.
*)

PROCEDURE CheckRemoveVariableWrite (Sym: CARDINAL; canDereference: BOOLEAN; Quad: CARDINAL) ;
BEGIN
   IF IsVar(Sym)
   THEN
      IF (GetMode(Sym)=LeftValue) AND canDereference
      THEN
         RemoveReadQuad(Sym, LeftValue, Quad) ;
         RemoveWriteQuad(Sym, RightValue, Quad)
      ELSE
         RemoveWriteQuad(Sym, GetMode(Sym), Quad)
      END
   END
END CheckRemoveVariableWrite ;


(*
   CheckConst - 
*)

PROCEDURE CheckConst (sym: CARDINAL) ;
BEGIN
   IF IsConst(sym)
   THEN
      PutToBeSolvedByQuads(sym)
   END
END CheckConst ;


(*
   GetFirstQuad - returns the first quadruple.
*)

PROCEDURE GetFirstQuad () : CARDINAL ;
BEGIN
   RETURN( Head )
END GetFirstQuad ;


(*
   GetNextQuad - returns the Quadruple number following QuadNo.
*)

PROCEDURE GetNextQuad (QuadNo: CARDINAL) : CARDINAL ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   RETURN( f^.Next )
END GetNextQuad ;


(*
   SubQuad - subtracts a quadruple QuadNo from a list Head.
*)

PROCEDURE SubQuad (QuadNo: CARDINAL) ;
VAR
   i   : CARDINAL ;
   f, g: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      AlterReference(Head, QuadNo, f^.Next) ;
      UndoReadWriteInfo(QuadNo, Operator, Operand1, Operand2, Operand3)
   END ;
   IF Head=QuadNo
   THEN
      Head := f^.Next
   ELSE
      i := Head ;
      g := GetQF(i) ;
      WHILE g^.Next#QuadNo DO
         i := g^.Next ;
         g := GetQF(i)
      END ;
      g^.Next := f^.Next
   END ;
   f^.Operator := DummyOp ;
   DEC(NoOfQuads)
END SubQuad ;


(*
   GetRealQuad - returns the Quadruple number of the real quadruple
                 at QuadNo or beyond.
*)

PROCEDURE GetRealQuad (QuadNo: CARDINAL) : CARDINAL ;
VAR
   f: QuadFrame ;
BEGIN
   WHILE QuadNo#0 DO
      IF InBounds(QuadArray, QuadNo)
      THEN
         f := GetQF(QuadNo) ;
         WITH f^ DO
            IF (NOT IsPseudoQuad(QuadNo)) AND
               (Operator#DummyOp) AND (Operator#LineNumberOp)
            THEN
               RETURN( QuadNo )
            END
         END ;
         INC(QuadNo)
      ELSE
         RETURN( 0 )
      END
   END ;
   RETURN( 0 )
END GetRealQuad ;


(*
   AlterReference - alters all references from OldQuad, to NewQuad in a
                    quadruple list Head.
*)

PROCEDURE AlterReference (Head, OldQuad, NewQuad: CARDINAL) ;
VAR
   f, g       : QuadFrame ;
   OldOperand3,
   i          : CARDINAL ;
BEGIN
   f := GetQF(OldQuad) ;
   WHILE (f^.NoOfTimesReferenced>0) AND (Head#0) DO
      g := GetQF(Head) ;
      WITH g^ DO
         CASE Operator OF

         IfInOp,
         IfNotInOp,
         IfEquOp,
         IfNotEquOp,
         IfLessOp,
         IfLessEquOp,
         IfGreOp,
         IfGreEquOp,
         TryOp,
         RetryOp,
         GotoOp     : IF Operand3=OldQuad
                      THEN
                         ManipulateReference(Head, NewQuad)
                      END

         ELSE
         END ;
         i := Next
      END ;
      Head := i
   END
END AlterReference ;


(*
   GrowQuads - grows the list of quadruples to the quadruple, to.
*)

PROCEDURE GrowQuads (to: CARDINAL) ;
VAR
   i: CARDINAL ;
   f: QuadFrame ;
BEGIN
   IF (to#0) AND (to>GrowInitialization)
   THEN
      i := GrowInitialization+1 ;
      WHILE i<=to DO
         IF InBounds(QuadArray, i)
         THEN
            Assert(GetIndice(QuadArray, i)#NIL)
         ELSE
            NEW(f) ;
            IF f=NIL
            THEN
               InternalError('out of memory error when trying to allocate a quadruple', __FILE__, __LINE__)
            END ;
            PutIndice(QuadArray, i, f) ;
            f^.NoOfTimesReferenced := 0
         END ;
         INC(i)
      END ;
      GrowInitialization := to
   END
END GrowQuads ;


(*
   ManipulateReference - manipulates the quadruple, q, so that it now points to quad, to.
*)

PROCEDURE ManipulateReference (q: CARDINAL; to: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   Assert((GrowInitialization>=q) OR (to=0)) ;
   GrowQuads(to) ;
   RemoveReference(q) ;
   f := GetQF(q) ;
   f^.Operand3 := to ;
   IF to#0
   THEN
      f := GetQF(to) ;
      INC(f^.NoOfTimesReferenced)
   END
END ManipulateReference ;
   

(*
   RemoveReference - remove the reference by quadruple, q, to wherever
                     it was pointing to.
*)

PROCEDURE RemoveReference (q: CARDINAL) ;
VAR
   f, g: QuadFrame ;
BEGIN
   f := GetQF(q) ;
   IF (f^.Operand3#0) AND (f^.Operand3<NextQuad)
   THEN
      g := GetQF(f^.Operand3) ;
      Assert(g^.NoOfTimesReferenced#0) ;
      DEC(g^.NoOfTimesReferenced)
   END
END RemoveReference ;
   
   
(*
   CountQuads - returns the number of quadruples.
*)

PROCEDURE CountQuads () : CARDINAL ;
BEGIN
   RETURN( NoOfQuads )
END CountQuads ;


(*
   NewQuad - sets QuadNo to a new quadruple.
*)

PROCEDURE NewQuad (VAR QuadNo: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   QuadNo := FreeList ;
   IF InBounds(QuadArray, QuadNo) AND (GetIndice(QuadArray, QuadNo)#NIL)
   THEN
      f := GetIndice(QuadArray, QuadNo)
   ELSE
      NEW(f) ;
      IF f=NIL
      THEN
         InternalError('out of memory error trying to allocate a quadruple', __FILE__, __LINE__)
      ELSE
         INC(NoOfQuads) ;
         PutIndice(QuadArray, QuadNo, f) ;
         f^.NoOfTimesReferenced := 0
      END
   END ;
   WITH f^ DO
      Operator := DummyOp ;
      Operand3 := 0 ;
      Next := 0
   END ;
   INC(FreeList) ;
   IF GrowInitialization<FreeList
   THEN
      GrowInitialization := FreeList
   END
END NewQuad ;


(*
   CheckVariableAt - checks to see whether, sym, was declared at a particular address.
*)

PROCEDURE CheckVariableAt (sym: CARDINAL) ;
BEGIN
   IF IsVar(sym) AND IsVariableAtAddress(sym)
   THEN
      IF GetMode(sym)=LeftValue
      THEN
         GenQuad(BecomesOp, sym, Address, GetVariableAtAddress(sym))
      ELSE
         InternalError('expecting lvalue for this variable which is declared at an explicit address',
                       __FILE__, __LINE__)
      END
   END
END CheckVariableAt ;


(*
   CheckVariablesAt - checks to see whether we need to initialize any pointers
                      which point to variable declared at addresses.
*)

PROCEDURE CheckVariablesAt (scope: CARDINAL) ;
BEGIN
   ForeachLocalSymDo(scope, CheckVariableAt)
END CheckVariablesAt ;


(*
   GetTurnInterrupts - returns the TurnInterrupts procedure function.
*)

PROCEDURE GetTurnInterrupts () : CARDINAL ;
BEGIN
   IF Iso
   THEN
      RETURN( GetQualidentImport(MakeKey('TurnInterrupts'), MakeKey('COROUTINES')) )
   ELSE
      RETURN( GetQualidentImport(MakeKey('TurnInterrupts'), MakeKey('SYSTEM')) )
   END
END GetTurnInterrupts ;


(*
   CheckNeedPriorityBegin - checks to see whether we need to save the old
                            module priority and change to another module
                            priority.
                            The current module initialization or procedure
                            being built is defined by, scope. The module whose
                            priority will be used is defined by, module.
*)

PROCEDURE CheckNeedPriorityBegin (scope, module: CARDINAL) ;
VAR
   ProcSym, old, return: CARDINAL ;
BEGIN
   IF GetPriority(module)#NulSym
   THEN
      (* module has been given a priority *)
      ProcSym := GetTurnInterrupts() ;
      IF ProcSym#NulSym
      THEN
         old := MakeTemporary(RightValue) ;
         PutVar(old, Cardinal) ;

         GenQuad(SavePriorityOp, old, scope, ProcSym) ;
         PushWord(PriorityStack, old)
      END
   END
END CheckNeedPriorityBegin ;


(*
   CheckNeedPriorityEnd - checks to see whether we need to restore the old
                          module priority.
                          The current module initialization or procedure
                          being built is defined by, scope.
*)

PROCEDURE CheckNeedPriorityEnd (scope, module: CARDINAL) ;
VAR
   ProcSym, old: CARDINAL ;
BEGIN
   IF GetPriority(module)#NulSym
   THEN
      (* module has been given a priority *)
      ProcSym := GetTurnInterrupts() ;
      IF ProcSym#NulSym
      THEN
         old := PopWord(PriorityStack) ;
         GenQuad(RestorePriorityOp, old, scope, ProcSym)
      END
   END
END CheckNeedPriorityEnd ;


(*
   StartBuildDefFile - generates a StartFileDefOp quadruple indicating the file
                       that has produced the subsequent quadruples.
                       The code generator uses the StartDefFileOp quadruples
                       to relate any error to the appropriate file.


                       Entry                   Exit
                       =====                   ====


                Ptr ->                                        <- Ptr
                       +------------+          +------------+
                       | ModuleName |          | ModuleName | 
                       |------------|          |------------|


                       Quadruples Produced

                       q     StartDefFileOp  _  _  ModuleSym
*)

PROCEDURE StartBuildDefFile ;
VAR
   ModuleName: Name ;
BEGIN
   PopT(ModuleName) ;
   PushT(ModuleName) ;
   GenQuad(StartDefFileOp, GetPreviousTokenLineNo(), NulSym, GetModule(ModuleName))
END StartBuildDefFile ;


(*
   StartBuildModFile - generates a StartModFileOp quadruple indicating the file
                       that has produced the subsequent quadruples.
                       The code generator uses the StartModFileOp quadruples
                       to relate any error to the appropriate file.


                       Entry                   Exit
                       =====                   ====


                Ptr ->                                        <- Ptr
                       +------------+          +------------+
                       | ModuleName |          | ModuleName | 
                       |------------|          |------------|


                       Quadruples Produced

                       q     StartModFileOp  lineno  filename  ModuleSym
*)

PROCEDURE StartBuildModFile ;
BEGIN
   GenQuad(StartModFileOp, GetPreviousTokenLineNo(),
           WORD(makekey(string(GetFileName()))), GetFileModule())
END StartBuildModFile ;


(*
   EndBuildFile - generates an EndFileOp quadruple indicating the file
                  that has produced the previous quadruples has ended.

                  Entry                   Exit
                  =====                   ====


           Ptr ->                                        <- Ptr
                  +------------+          +------------+
                  | ModuleName |          | ModuleName | 
                  |------------|          |------------|


                  Quadruples Produced

                  q     EndFileOp  _  _  ModuleSym
*)

PROCEDURE EndBuildFile ;
VAR
   ModuleName: Name ;
BEGIN
   PopT(ModuleName) ;
   PushT(ModuleName) ;
   GenQuad(EndFileOp, NulSym, NulSym, GetModule(ModuleName))
END EndBuildFile ;


(*
   StartBuildInit - Sets the start of initialization code of the
                    current module to the next quadruple.
*)
 
PROCEDURE StartBuildInit ;
VAR
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(name) ;
   ModuleSym := GetCurrentModule() ;
   Assert(IsModule(ModuleSym) OR IsDefImp(ModuleSym)) ;
   Assert(GetSymName(ModuleSym)=name) ;
   PutModuleStartQuad(ModuleSym, NextQuad) ;
   GenQuad(InitStartOp, GetPreviousTokenLineNo(), GetFileModule(), ModuleSym) ;
   PushWord(ReturnStack, 0) ;
   PushT(name) ;
   CheckVariablesAt(ModuleSym) ;
   CheckNeedPriorityBegin(ModuleSym, ModuleSym) ;
   PushWord(TryStack, NextQuad) ;
   PushWord(CatchStack, 0) ;
   IF HasExceptionBlock(ModuleSym)
   THEN
      GenQuad(TryOp, NulSym, NulSym, 0)
   END
END StartBuildInit ;
 
 
(*
   EndBuildInit - Sets the end initialization code of a module.
*)
 
PROCEDURE EndBuildInit ;
BEGIN
   IF HasExceptionBlock(GetCurrentModule())
   THEN
      BuildRTExceptLeave(TRUE) ;
      GenQuad(CatchEndOp, NulSym, NulSym, NulSym)
   END ;
   BackPatch(PopWord(ReturnStack), NextQuad) ;
   CheckNeedPriorityEnd(GetCurrentModule(), GetCurrentModule()) ;
   PutModuleEndQuad(GetCurrentModule(), NextQuad) ;
   CheckVariablesInBlock(GetCurrentModule()) ;
   GenQuad(InitEndOp, GetPreviousTokenLineNo(), GetFileModule(),
           GetCurrentModule())
END EndBuildInit ;


(*
   StartBuildFinally - Sets the start of finalization code of the
                       current module to the next quadruple.
*)
 
PROCEDURE StartBuildFinally ;
VAR
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(name) ;
   ModuleSym := GetCurrentModule() ;
   Assert(IsModule(ModuleSym) OR IsDefImp(ModuleSym)) ;
   Assert(GetSymName(ModuleSym)=name) ;
   PutModuleFinallyStartQuad(ModuleSym, NextQuad) ;
   GenQuad(FinallyStartOp, GetPreviousTokenLineNo(), GetFileModule(), ModuleSym) ;
   PushWord(ReturnStack, 0) ;
   PushT(name) ;
   (* CheckVariablesAt(ModuleSym) ; *)
   CheckNeedPriorityBegin(ModuleSym, ModuleSym) ;
   PushWord(TryStack, NextQuad) ;
   PushWord(CatchStack, 0) ;
   IF HasExceptionFinally(ModuleSym)
   THEN
      GenQuad(TryOp, NulSym, NulSym, 0)
   END
END StartBuildFinally ;
 
 
(*
   EndBuildFinally - Sets the end finalization code of a module.
*)
 
PROCEDURE EndBuildFinally ;
BEGIN
   IF HasExceptionFinally(GetCurrentModule())
   THEN
      BuildRTExceptLeave(TRUE) ;
      GenQuad(CatchEndOp, NulSym, NulSym, NulSym)
   END ;
   BackPatch(PopWord(ReturnStack), NextQuad) ;
   CheckNeedPriorityEnd(GetCurrentModule(), GetCurrentModule()) ;
   PutModuleFinallyEndQuad(GetCurrentModule(), NextQuad) ;
   CheckVariablesInBlock(GetCurrentModule()) ;
   GenQuad(FinallyEndOp, GetPreviousTokenLineNo(), GetFileModule(),
           GetCurrentModule())
END EndBuildFinally ;


(*
   BuildRTExceptEnter - informs RTExceptions that we are about to enter the except state.
*)

PROCEDURE BuildRTExceptEnter ;
VAR
   old,
   ProcSym: CARDINAL ;
BEGIN
   IF Exceptions
   THEN
      (* now inform the Modula-2 runtime we are in the exception state *)
      ProcSym := GetQualidentImport(MakeKey('SetExceptionState'), MakeKey('RTExceptions')) ;
      IF ProcSym=NulSym
      THEN
         ErrorString(NewWarning(GetTokenNo()),
                     InitString('no procedure SetExceptionState found in RTExceptions which is needed to implement exception handling'))
      ELSE
         old := MakeTemporary(RightValue) ;
         PutVar(old, Boolean) ;
         GenQuad(SaveExceptionOp, old, NulSym, ProcSym) ;
         PushWord(ExceptStack, old)
      END
   ELSE
      ErrorFormat0(NewError(GetTokenNo()),
                   'cannot use EXCEPT blocks with the -fno-exceptions flag')
   END
END BuildRTExceptEnter ;


(*
   BuildRTExceptLeave - informs RTExceptions that we are about to leave the except state.
                        If, destroy, is TRUE then pop the ExceptStack.
*)

PROCEDURE BuildRTExceptLeave (destroy: BOOLEAN) ;
VAR
   old,
   ProcSym: CARDINAL ;
BEGIN
   IF Exceptions
   THEN
      (* now inform the Modula-2 runtime we are in the exception state *)
      ProcSym := GetQualidentImport(MakeKey('SetExceptionState'), MakeKey('RTExceptions')) ;
      IF ProcSym#NulSym
      THEN
         IF destroy
         THEN
            old := PopWord(ExceptStack)
         ELSE
            old := PeepWord(ExceptStack, 1)
         END ;
         GenQuad(RestoreExceptionOp, old, NulSym, ProcSym)
      END
   ELSE
      (* no need for an error message here as it will be generated in the Enter procedure above *)
   END
END BuildRTExceptLeave ;


(*
   BuildExceptInitial - adds an CatchBeginOp, CatchEndOp quadruple
                        in the current block.
*)
 
PROCEDURE BuildExceptInitial ;
VAR
   previous: CARDINAL ;
BEGIN
   (* we have finished the 'try' block, so now goto the return
      section which will tidy up (any) priorities before returning.
   *)
   GenQuad(GotoOp, NulSym, NulSym, PopWord(ReturnStack)) ;
   PushWord(ReturnStack, NextQuad-1) ;
   (*
      this is the 'catch' block.
   *)
   BackPatch(PeepWord(TryStack, 1), NextQuad) ;
   GenQuad(CatchBeginOp, NulSym, NulSym, NulSym) ;
   previous := PopWord(CatchStack) ;
   IF previous#0
   THEN
      ErrorFormat0(NewError(GetTokenNo()),
                   'only allowed one EXCEPT statement in a procedure or module')
   END ;
   PushWord(CatchStack, NextQuad-1) ;
   BuildRTExceptEnter
END BuildExceptInitial ;


(*
   BuildExceptFinally - adds an ExceptOp quadruple in a modules
                        finally block.
*)
 
PROCEDURE BuildExceptFinally ;
BEGIN
   BuildExceptInitial
END BuildExceptFinally ;


(*
   BuildExceptProcedure - adds an ExceptOp quadruple in a procedure
                          block.
*)
 
PROCEDURE BuildExceptProcedure ;
BEGIN
   BuildExceptInitial
END BuildExceptProcedure ;


(*
   BuildRetry - adds an RetryOp quadruple.
*)
 
PROCEDURE BuildRetry ;
BEGIN
   IF PeepWord(CatchStack, 1)=0
   THEN
      ErrorFormat0(NewError(GetTokenNo()),
                   'RETRY statement must occur after an EXCEPT statement in the same module or procedure block')
   ELSE
      BuildRTExceptLeave(FALSE) ;
      GenQuad(RetryOp, NulSym, NulSym, PeepWord(TryStack, 1))
   END
END BuildRetry ;


(*
   BuildModuleStart - starts current module scope.
*)

PROCEDURE BuildModuleStart ;
BEGIN
   GenQuad(ModuleScopeOp, GetPreviousTokenLineNo(),
           WORD(makekey(string(GetFileName()))), GetCurrentModule())
END BuildModuleStart ;


(*
   StartBuildInnerInit - Sets the start of initialization code of the
                         inner module to the next quadruple.
*)
 
PROCEDURE StartBuildInnerInit ;
BEGIN
   PutModuleStartQuad(GetCurrentModule(), NextQuad) ;
   GenQuad(InitStartOp, GetPreviousTokenLineNo(), NulSym, GetCurrentModule()) ;
   PushWord(ReturnStack, 0) ;
   CheckNeedPriorityBegin(GetCurrentModule(), GetCurrentModule()) ;
   PushWord(TryStack, NextQuad) ;
   PushWord(CatchStack, 0) ;
   IF HasExceptionFinally(GetCurrentModule())
   THEN
      GenQuad(TryOp, NulSym, NulSym, 0)
   END
END StartBuildInnerInit ;
 
 
(*
   EndBuildInnerInit - Sets the end initialization code of a module.
*)
 
PROCEDURE EndBuildInnerInit ;
BEGIN
   IF HasExceptionBlock(GetCurrentModule())
   THEN
      BuildRTExceptLeave(TRUE) ;
      GenQuad(CatchEndOp, NulSym, NulSym, NulSym)
   END ;
   PutModuleEndQuad(GetCurrentModule(), NextQuad) ;
   CheckVariablesInBlock(GetCurrentModule()) ;
   BackPatch(PopWord(ReturnStack), NextQuad) ;
   CheckNeedPriorityEnd(GetCurrentModule(), GetCurrentModule()) ;
   GenQuad(InitEndOp, GetPreviousTokenLineNo(), NulSym, GetCurrentModule())
END EndBuildInnerInit ;


(*
   BuildModulePriority - assigns the current module with a priority
                         from the top of stack.

                         Entry                   Exit
                         =====                   ====


                  Ptr ->                         Empty
                         +------------+
                         | Priority   |
                         |------------|
*)

PROCEDURE BuildModulePriority ;
VAR
   Priority: CARDINAL ;
BEGIN
   PopT(Priority) ;
   PutPriority(GetCurrentModule(), Priority)
END BuildModulePriority ;


(*
   ForLoopAnalysis - checks all the FOR loops for index variable manipulation
                     and dangerous usage outside the loop.
*)

PROCEDURE ForLoopAnalysis ;
VAR
   i, n: CARDINAL ;
BEGIN
   IF Pedantic
   THEN
      WITH ForInfo DO
         n := NoOfItemsInList(IncrementQuad) ;
         i := 1 ;
         WHILE i<=n DO
            CheckForIndex(GetItemFromList(StartOfForLoop, i),
                          GetItemFromList(EndOfForLoop, i),
                          GetItemFromList(IncrementQuad, i),
                          GetItemFromList(ForLoopIndex, i)) ;
            INC(i)
         END
      END
   END
END ForLoopAnalysis ;


(*
   AddForInfo - adds the description of the FOR loop into the record list.
                This is used if -pedantic is turned on to check index variable
                usage.
*)

PROCEDURE AddForInfo (Start, End, IncQuad: CARDINAL; Sym: CARDINAL) ;
BEGIN
   IF Pedantic
   THEN
      WITH ForInfo DO
         PutItemIntoList(IncrementQuad, IncQuad) ;
         PutItemIntoList(StartOfForLoop, Start) ;
         PutItemIntoList(EndOfForLoop, End) ;
         PutItemIntoList(ForLoopIndex, Sym)
      END
   END
END AddForInfo ;


(*
   CheckForIndex - checks the quadruples: Start..End to see whether a
                   for loop index is manipulated by the programmer.
                   It generates a warning if this is the case.
                   It also checks to see whether the IndexSym is read
                   immediately outside the loop in which case a warning
                   is issued.
*)

PROCEDURE CheckForIndex (Start, End, Omit: CARDINAL; IndexSym: CARDINAL) ;
VAR
   ReadStart, ReadEnd,
   WriteStart, WriteEnd: CARDINAL ;
   s                   : String ;
BEGIN
   GetWriteLimitQuads(IndexSym, RightValue, Start, End, WriteStart, WriteEnd) ;
   IF (WriteStart<Omit) AND (WriteStart>Start)
   THEN
      s := Mark(InitStringCharStar(KeyToCharStar(GetSymName(IndexSym)))) ;
      WarnStringAt(Sprintf1(Mark(InitString('FOR loop index variable (%s) is being manipulated inside the loop, this is considered bad practice and may cause unknown program behaviour')),
                            s),
                   QuadToTokenNo(WriteStart))
   END ;
   GetWriteLimitQuads(IndexSym, RightValue, End, 0, WriteStart, WriteEnd) ;
   GetReadLimitQuads(IndexSym, RightValue, End, 0, ReadStart, ReadEnd) ;
   IF (ReadStart#0) AND ((ReadStart<WriteStart) OR (WriteStart=0))
   THEN
      s := Mark(InitStringCharStar(KeyToCharStar(GetSymName(IndexSym)))) ;
      WarnStringAt(Sprintf1(Mark(InitString('FOR loop index variable (%s) is being read outside the FOR loop (without being reset first), this is considered extremely bad practice and may cause unknown program behaviour')),
                            s),
                   QuadToTokenNo(ReadStart))
   END
END CheckForIndex ;


(*
   GetCurrentFunctionName - returns the name for the current __FUNCTION__
*)

PROCEDURE GetCurrentFunctionName () : Name ;
VAR
   s: String ;
   n: Name ;
BEGIN
   IF CurrentProc=NulSym
   THEN
      s := InitStringCharStar(KeyToCharStar(GetSymName(GetCurrentModule()))) ;
      s := Sprintf1(Mark(InitString('module %s initialization')), s) ;
      n := makekey(string(s)) ;
      s := KillString(s) ;
      RETURN( n )
   ELSE
      RETURN( GetSymName(CurrentProc) )
   END
END GetCurrentFunctionName ;


(*
   BuildRange - generates a RangeCheckOp quad with, r, as its operand.
*)

PROCEDURE BuildRange (r: CARDINAL) ;
BEGIN
   GenQuad(RangeCheckOp, WORD(GetLineNo()), NulSym, r)
END BuildRange ;


(*
   BuildError - generates a ErrorOp quad, indicating that if this
                quadruple is reachable, then a runtime error would
                occur.
*)

PROCEDURE BuildError (r: CARDINAL) ;
BEGIN
   GenQuad(ErrorOp, WORD(GetLineNo()), NulSym, r)
END BuildError ;


(*
   CheckPointerThroughNil - builds a range quadruple, providing, sym, is
                            a candidate for checking against NIL.
                            This range quadruple is only expanded into
                            code during the code generation phase
                            thus allowing limited compile time checking.
*)

PROCEDURE CheckPointerThroughNil (sym: CARDINAL) ;
BEGIN
   IF IsVar(sym) AND GetVarPointerCheck(sym)
   THEN
      (* PutVarPointerCheck(sym, FALSE) ;  (* so we do not detect this again *) *)
      BuildRange(InitPointerRangeCheck(sym, GetMode(sym)=LeftValue))
   END
END CheckPointerThroughNil ;


(*
   CollectLow - returns the low of the subrange value.
*)

PROCEDURE CollectLow (sym: CARDINAL) : CARDINAL ;
VAR
   low, high: CARDINAL ;
BEGIN
   IF IsSubrange(sym)
   THEN
      GetSubrange(sym, high, low) ;
      RETURN( low )
   ELSE
      InternalError('expecting Subrange symbol', __FILE__, __LINE__)
   END
END CollectLow ;


(*
   CollectHigh - returns the high of the subrange value, sym.
*)

PROCEDURE CollectHigh (sym: CARDINAL) : CARDINAL ;
VAR
   low, high: CARDINAL ;
BEGIN
   IF IsSubrange(sym)
   THEN
      GetSubrange(sym, high, low) ;
      RETURN( high )
   ELSE
      InternalError('expecting Subrange symbol', __FILE__, __LINE__)
   END
END CollectHigh ;


(*
   BackPatchSubrangesAndOptParam - runs through all the quadruples and finds SubrangeLow or SubrangeHigh
                                   quadruples and replaces it by an assignment to the Low or High component
                                   of the subrange type.

                                   Input:
                                   SubrangeLow    op1     op3         (* op3 is a subrange *)

                                   Output:
                                   Becomes        op1     low

                                   Input:
                                   SubrangeHigh   op1     op3         (* op3 is a subrange *)

                                   Output:
                                   Becomes        op1     high

                                   Input:
                                   OptParam       op1     op2    op3

                                   Output:
                                   Param          op1     op2    GetOptArgInit(op3)
*)

PROCEDURE BackPatchSubrangesAndOptParam ;
VAR
   f: QuadFrame ;
   q: CARDINAL ;
BEGIN
   q := GetFirstQuad() ;
   IF q#0
   THEN
      REPEAT
         f := GetQF(q) ;
         WITH f^ DO
            CASE Operator OF

            SubrangeLowOp :  Operand3 := CollectLow(Operand3) ;
                             Operator := BecomesOp |
            SubrangeHighOp:  Operand3 := CollectHigh(Operand3) ;
                             Operator := BecomesOp |
            OptParamOp    :  Operand3 := GetOptArgInit(Operand3) ;
                             Operator := ParamOp

            ELSE
            END ;
            q := Next
         END
      UNTIL q=0
   END
END BackPatchSubrangesAndOptParam ;


(*
   CheckCompatibleWithBecomes - checks to see that symbol, sym, is
                                compatible with the := operator.
*)

PROCEDURE CheckCompatibleWithBecomes (sym: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   IF IsType(sym) OR IsProcedure(sym)
   THEN
      n := GetSymName(sym) ;
      WriteFormat1('illegal designator (%a) it must be a variable',
                   n)
   END
END CheckCompatibleWithBecomes ;


(*
   BuildAssignmentWithoutBounds - calls BuildAssignment but makes sure we do not
                                  check bounds.
*)

PROCEDURE BuildAssignmentWithoutBounds (checkTypes, checkOverflow: BOOLEAN) ;
VAR
   old: BOOLEAN ;
BEGIN
   old := MustNotCheckBounds ;
   MustNotCheckBounds := TRUE ;
   doBuildAssignment(checkTypes, checkOverflow) ;
   MustNotCheckBounds := old
END BuildAssignmentWithoutBounds ;


(*
   MarkArrayWritten - marks, Array, as being written.
*)

PROCEDURE MarkArrayWritten (Array: CARDINAL) ;
BEGIN
   IF (Array#NulSym) AND IsVarAParam(Array)
   THEN
      PutVarWritten(Array, TRUE)
   END
END MarkArrayWritten ;


(*
   MarkAsReadWrite - marks the variable or parameter as being
                     read/write.
*)

PROCEDURE MarkAsReadWrite (sym: CARDINAL) ;
BEGIN
   IF (sym#NulSym) AND IsVar(sym)
   THEN
      PutReadQuad(sym, RightValue, NextQuad) ;
      PutWriteQuad(sym, RightValue, NextQuad)
   END
END MarkAsReadWrite ;


(*
   MarkAsRead - marks the variable or parameter as being read.
*)

PROCEDURE MarkAsRead (sym: CARDINAL) ;
BEGIN
   IF (sym#NulSym) AND IsVar(sym)
   THEN
      PutReadQuad(sym, RightValue, NextQuad) ;
      PutWriteQuad(sym, RightValue, NextQuad)
   END
END MarkAsRead ;


(*
   MarkAsWrite - marks the variable or parameter as being written.
*)

PROCEDURE MarkAsWrite (sym: CARDINAL) ;
BEGIN
   IF (sym#NulSym) AND IsVar(sym)
   THEN
      PutWriteQuad(sym, RightValue, NextQuad)
   END
END MarkAsWrite ;


(*
   doVal - return an expression which is VAL(type, expr).  If
           expr is a constant then return expr.
*)

PROCEDURE doVal (type, expr: CARDINAL) : CARDINAL ;
BEGIN
   IF (NOT IsConst(expr)) AND (SkipType(type)#SkipType(GetType(expr)))
   THEN
      PushTF(Convert, NulSym) ;
      PushT(SkipType(type)) ;
      PushT(expr) ;
      PushT(2) ;          (* Two parameters *)
      BuildConvertFunction ;
      PopT(expr)
   END ;
   RETURN( expr )
END doVal ;


(*
   MoveWithMode - 
*)

PROCEDURE MoveWithMode (Des, Exp, Array: CARDINAL) ;
VAR
   t: CARDINAL ;
BEGIN
   IF IsConstString(Exp) AND IsConst(Des)
   THEN
      GenQuad(BecomesOp, Des, NulSym, Exp) ;
      PutConstString(Des, GetString(Exp))
   ELSE
      IF GetMode(Des)=RightValue
      THEN
         IF GetMode(Exp)=LeftValue
         THEN
            CheckPointerThroughNil(Exp) ;  (*    Des = *Exp    *)
            doIndrX(Des, Exp)
         ELSE
            GenQuad(BecomesOp, Des, NulSym, Exp)
         END
      ELSIF GetMode(Des)=LeftValue
      THEN
         MarkArrayWritten(Array) ;
         IF GetMode(Exp)=LeftValue
         THEN
            t := MakeTemporary(RightValue) ;
            PutVar(t, GetType(Exp)) ;
            CheckPointerThroughNil(Exp) ;
            doIndrX(t, Exp) ;
            CheckPointerThroughNil(Des) ;  (*    *Des = Exp    *)
            GenQuad(XIndrOp, Des, GetType(Des), doVal(GetType(Des), t))
         ELSE
            CheckPointerThroughNil(Des) ;  (*    *Des = Exp    *)
            GenQuad(XIndrOp, Des, GetType(Des), doVal(GetType(Des), Exp))
         END
      ELSE
         GenQuad(BecomesOp, Des, NulSym, Exp)
      END
   END
END MoveWithMode ;


(*
   BuildBuiltinConst - makes reference to a builtin constant within gm2.

                              Entry                 Exit

                       Ptr ->
                              +------------+        +------------+
                              | Ident      |        | Sym        |
                              |------------|        |------------|

                       Quadruple produced:

                       q    Sym  BuiltinConstOp  Ident
*)

PROCEDURE BuildBuiltinConst ;
VAR
   Id : CARDINAL ;
   Sym: CARDINAL ;
BEGIN
   PopT(Id) ;
   Sym := MakeTemporary(ImmediateValue) ;
   PutVar(Sym, Integer) ;
(*
   CASE GetBuiltinConstType(KeyToCharStar(Name(Id))) OF

   0:  ErrorFormat1(NewError(GetTokenNo()),
                    '%a unrecognised builtin constant', Id) |
   1:  PutVar(Sym, Integer) |
   2:  PutVar(Sym, Real)

   ELSE
      InternalError('unrecognised value', __FILE__, __LINE__)
   END ;
*)
   GenQuad(BuiltinConstOp, Sym, NulSym, Id) ;
   PushT(Sym)
END BuildBuiltinConst ;


(*
   BuildBuiltinTypeInfo - make reference to a builtin typeinfo function
                          within gm2.

                                 Entry                 Exit

                          Ptr ->
                                 +-------------+
                                 | Type        |
                                 |-------------|       +------------+
                                 | Ident       |       | Sym        |
                                 |-------------|       |------------|

                          Quadruple produced:

                          q    Sym  BuiltinTypeInfoOp  Type Ident
*)

PROCEDURE BuildBuiltinTypeInfo ;
VAR
   Ident,
   Type,
   Sym  : CARDINAL ;
BEGIN
   PopT(Ident) ;
   PopT(Type) ;
   Sym := MakeTemporary(ImmediateValue) ;
   CASE GetBuiltinTypeInfoType(KeyToCharStar(Name(Ident))) OF

   0:  ErrorFormat1(NewError(GetTokenNo()),
                    '%a unrecognised builtin constant', Ident) |
   1:  PutVar(Sym, Boolean) |
   2:  PutVar(Sym, ZType) |
   3:  PutVar(Sym, RType)

   ELSE
      InternalError('unrecognised value', __FILE__, __LINE__)
   END ;
   GenQuad(BuiltinTypeInfoOp, Sym, Type, Ident) ;
   PushT(Sym)
END BuildBuiltinTypeInfo ;


(*
   CheckNotConstAndVar - checks to make sure that we are not
                         assigning a variable to a constant.
*)

PROCEDURE CheckNotConstAndVar (Des, Exp: CARDINAL) ;
BEGIN
   IF IsConst(Des) AND IsVar(Exp)
   THEN
      MetaErrors2('error in assignment, cannot assign a variable {%2a} to a constant {%1a}',
                  'designator {%1Da} is declared as a CONST', Des, Exp)
   END
END CheckNotConstAndVar ;


(*
   BuildAssignment - Builds an assignment from the values given on the
                     quad stack. Either an assignment to an
                     arithmetic expression or an assignment to a
                     boolean expression.
                     The Stack is expected to contain:


       Either

                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | Expression |
                     |------------|
                     | Designator |
                     |------------|          +------------+
                     |            |          |            |  <- Ptr
                     |------------|          |------------|


                     Quadruples Produced depend of GetMode of Designator and Expression


                     Designator = RightValue         Expression = RightValue
                     q     BecomesOp  Designator  _  Expression

                     Designator = RightValue         Expression = LeftValue
                     q     IndrX      Designator     Expression

                     Designator = LeftValue          Expression = RightValue
                     q     XIndr      Designator     Expression

                     Designator = LeftValue          Expression = LeftValue
                     q     IndrX      t              Expression
                     q+1   XIndr      Designator     t


       OR

                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | True |False|
                     |------------|
                     | Designator |
                     |------------|          +------------+
                     |            |          |            |  <- Ptr
                     |------------|          |------------|


                     Quadruples Produced depend of GetMode of Designator

                     Designator = RightValue
                     q     BecomesOp  Designator  _  TRUE
                     q+1   GotoOp                    q+3
                     q+2   BecomesOp  Designator  _  FALSE


                     Designator = LeftValue
                     q     XIndr      Designator  _  TRUE
                     q+1   GotoOp                    q+3
                     q+2   XIndr      Designator  _  FALSE
*)

PROCEDURE BuildAssignment ;
BEGIN
   doBuildAssignment(TRUE, TRUE)
END BuildAssignment ;


(*
   doBuildAssignment - subsiduary procedure of BuildAssignment.
                       It builds the assignment and optionally
                       checks the types are compatible.
*)

PROCEDURE doBuildAssignment (checkTypes, checkOverflow: BOOLEAN) ;
VAR
   r, w,
   t, f,
   Array,
   Des, Exp: CARDINAL ;
BEGIN
   DumpStack ;
   IF IsBoolean(1)
   THEN
      PopBool(t, f) ;
      PopT(Des) ;
      (* Conditional Boolean Assignment *)
      BackPatch(t, NextQuad) ;
      IF GetMode(Des)=RightValue
      THEN
         GenQuadO(BecomesOp, Des, NulSym, True, checkOverflow)
      ELSE
         CheckPointerThroughNil(Des) ;
         GenQuad(XIndrOp, Des, Boolean, True)
      END ;
      GenQuad(GotoOp, NulSym, NulSym, NextQuad+2) ;
      BackPatch(f, NextQuad) ;
      IF GetMode(Des)=RightValue
      THEN
         GenQuadO(BecomesOp, Des, NulSym, False, checkOverflow)
      ELSE
         CheckPointerThroughNil(Des) ;
         GenQuad(XIndrOp, Des, Boolean, False)
      END
   ELSE
      PopTrw(Exp, r) ;
      MarkAsRead(r) ;
      IF Exp=NulSym
      THEN
         WriteFormat0('unknown expression found during assignment') ;
         FlushErrors
      END ;
      Array := OperandA(1) ;
      PopTrw(Des, w) ;
      MarkAsWrite(w) ;
      CheckCompatibleWithBecomes(Des) ;
      IF (GetType(Des)#NulSym) AND (NOT IsSet(SkipType(GetType(Des))))
      THEN
         (* tell code generator to test runtime values of assignment so ensure we
            catch overflow and underflow *)
         BuildRange(InitAssignmentRangeCheck(Des, Exp))
      END ;
      CheckNotConstAndVar(Des, Exp) ;
      (* Traditional Assignment *)
      MoveWithMode(Des, Exp, Array) ;
      IF checkTypes
      THEN
         IF (CannotCheckTypeInPass3(Des) OR CannotCheckTypeInPass3(Exp))
         THEN
            (* we must do this after the assignment to allow the Designator to be
               resolved (if it is a constant) before the type checking is done *)
            (* prompt post pass 3 to check the assignment once all types are resolved *)
            BuildRange(InitTypesAssignmentCheck(Des, Exp))
         END ;
         CheckAssignCompatible(Des, Exp)
      END
   END
 ; DumpStack ;
END doBuildAssignment ;


(*
   CheckAssignCompatible - checks to see that an assignment is compatible.
                           It performs limited checking - thorough checking
                           is done in pass 3.  But we do what we can here
                           given knowledge so far.
*)

PROCEDURE CheckAssignCompatible (Des, Exp: CARDINAL) ;
VAR
   DesT, ExpT, DesL: CARDINAL ;
   n               : Name ;
BEGIN
   DesT := GetType(Des) ;
   ExpT := GetType(Exp) ;
   DesL := GetLowestType(Des) ;
   IF IsProcedure(Exp) AND
      ((DesT#NulSym) AND (NOT IsProcType(DesT))) AND
      ((DesL#NulSym) AND (NOT IsProcType(DesL)))
   THEN
      n := GetSymName(Des) ;
      WriteFormat1('incorrectly assigning a procedure to a variable %a (variable is not a procedure type)', n)
   ELSIF IsProcedure(Exp) AND IsProcedureNested(Exp)
   THEN
      n := GetSymName(Exp) ;
      WriteFormat1('cannot call nested procedure, %a, indirectly as the outer scope will not be known',
                   n)
   ELSIF IsConstString(Exp)
   THEN
   ELSIF (DesT#NulSym) AND (IsUnbounded(DesT))
   THEN
   ELSIF (ExpT#NulSym) AND (IsUnbounded(ExpT))
   THEN
   ELSIF (DesL#NulSym) AND IsArray(DesL)
   THEN
   ELSIF IsConstructor(Exp)
   THEN
      IF ExpT=NulSym
      THEN
         (* ignore type checking *)
      ELSIF (DesT=NulSym) AND IsConst(Des) AND (IsConstructor(Des) OR IsConstSet(Des))
      THEN
         PutConst(Des, ExpT)
      ELSIF NOT IsAssignmentCompatible(DesT, ExpT)
      THEN
         WriteFormat0('constructor is not compatible during assignment')
      END
   ELSIF IsConst(Exp) AND (ExpT#Address) AND (NOT IsConst(Des)) AND
         (DesL#NulSym) AND ((DesL=Cardinal) OR (NOT IsSubrange(DesL))) AND
         (NOT IsEnumeration(DesL))
   THEN
      IF (IsBaseType(DesL) OR IsSystemType(DesL))
      THEN
         CheckAssignmentCompatible(ExpT, DesT)
      ELSE
         n := GetSymName(Des) ;
         WriteFormat1('assignment of a constant (%a) can only be made to a variable whose type is equivalent to a Modula-2 base type',
                      n)
      END
   ELSIF (DesT#NulSym) AND IsSet(DesT) AND IsConst(Exp)
   THEN
      (* We ignore checking of these types in pass 3 - but we do check them thoroughly post pass 3 *)
   ELSE
      IF (DesT#NulSym) AND IsProcType(DesT) AND IsProcedure(Exp)
      THEN
         DesT := GetType(DesT) ; (* we can at least check RETURN values of procedure variables *)
         (* remember that thorough assignment checking is done post pass 3 *)
         CheckAssignmentCompatible(ExpT, DesT)
      END
   END
END CheckAssignCompatible ;


(*
   CheckBooleanId - Checks to see if the top operand is a boolean.
                    If the operand is not a boolean then it is tested
                    with true and a boolean is generated.
                    The Stack:


                    Entry                     Exit
             Ptr ->                                          <- Ptr
                    +------------+            +------------+
                    | Sym        |            | t   | f    |
                    |------------|            |------------|

                        Quadruples

                        q   If=      Sym   True   _
                        q+1 GotoOp   _     _      _
*)

PROCEDURE CheckBooleanId ;
VAR
   n: Name ;
BEGIN
   IF NOT IsBoolean(1)
   THEN
      IF IsVar(OperandT(1))
      THEN
         IF GetType(OperandT(1))#Boolean
         THEN
            MetaError1('{%1Ua:is not a boolean expression}' +
                       '{!%1Ua:boolean expression expected}', OperandT(1))
         END
      END ;
      PushT(EqualTok) ;
      PushT(True) ;
      BuildRelOp
   END
END CheckBooleanId ;


(*
   BuildAlignment - builds an assignment to an alignment constant.

                    The Stack is expected to contain:


                            Entry                   Exit
                            =====                   ====

                    Ptr ->
                            +------------+
                            | Expression |
                            |------------|
                            | ALIGNED    |
                            |------------|          empty
*)

PROCEDURE BuildAlignment ;
VAR
   name : Name ;
   expr,
   align: CARDINAL ;
BEGIN
   PopT(expr) ;
   PopT(name) ;
   IF name#MakeKey('ALIGNED')
   THEN
      WriteFormat1('expecting ALIGNED identifier, rather than %a', name)
   END ;
   GetConstFromFifoQueue(align) ;
   PushT(align) ;
   PushT(expr) ;
   BuildAssignment
END BuildAlignment ;


(*
   BuildRepeat - Builds the repeat statement from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====


                 Empty
                                                        <- Ptr
                                         +------------+
                                         | RepeatQuad |
                                         |------------|

*)

PROCEDURE BuildRepeat ;
BEGIN
   PushT(NextQuad)
END BuildRepeat ;


(*
   BuildUntil - Builds the until part of the repeat statement
                from the quad stack.
                The Stack is expected to contain:


                Entry                   Exit
                =====                   ====

        Ptr ->
                +------------+
                | t   | f    |
                |------------|
                | RepeatQuad |          Empty
                |------------|
*)

PROCEDURE BuildUntil ;
VAR
   t, f,
   Repeat: CARDINAL ;
BEGIN
   CheckBooleanId ;
   PopBool(t, f) ;
   PopT(Repeat) ;
   BackPatch(f, Repeat) ;          (* If False then keep on repeating *)
   BackPatch(t, NextQuad) ;        (* If True then exit repeat        *)
END BuildUntil ;


(*
   BuildWhile - Builds the While part of the While statement
                from the quad stack.
                The Stack is expected to contain:


                Entry                   Exit
                =====                   ====

                                                       <- Ptr
                                        |------------|
                Empty                   | WhileQuad  |
                                        |------------|
*)

PROCEDURE BuildWhile ;
BEGIN
   PushT(NextQuad)
END BuildWhile ;


(*
   BuildDoWhile - Builds the Do part of the while statement
                  from the quad stack.
                  The Stack is expected to contain:


                  Entry                   Exit
                  =====                   ====

          Ptr ->
                  +------------+          +------------+
                  | t   | f    |          | 0    | f   |
                  |------------|          |------------|
                  | WhileQuad  |          | WhileQuad  |
                  |------------|          |------------|

                  Quadruples

                  BackPatch t exit to the NextQuad
*)

PROCEDURE BuildDoWhile ;
VAR
   t, f: CARDINAL ;
BEGIN
   CheckBooleanId ;
   PopBool(t, f) ;
   BackPatch(t, NextQuad) ;
   PushBool(0, f)
END BuildDoWhile ;


(*
   BuildEndWhile - Builds the end part of the while statement
                   from the quad stack.
                   The Stack is expected to contain:


                   Entry                   Exit
                   =====                   ====

           Ptr ->
                   +------------+
                   | t   | f    |
                   |------------|
                   | WhileQuad  |          Empty
                   |------------|

                   Quadruples

                   q    GotoOp  WhileQuad
                   False exit is backpatched with q+1
*)

PROCEDURE BuildEndWhile ;
VAR
   While,
   t, f : CARDINAL ;
BEGIN
   PopBool(t, f) ;
   Assert(t=0) ;
   PopT(While) ;
   GenQuad(GotoOp, NulSym, NulSym, While) ;
   BackPatch(f, NextQuad)
END BuildEndWhile ;


(*
   BuildLoop - Builds the Loop part of the Loop statement
               from the quad stack.
               The Stack is expected to contain:


               Entry                   Exit
               =====                   ====

                                                      <- Ptr
               Empty                   +------------+
                                       | LoopQuad   |
                                       |------------|
*)

PROCEDURE BuildLoop ;
BEGIN
   PushT(NextQuad) ;
   PushExit(0)         (* Seperate Exit Stack for loop end *)
END BuildLoop ;


(*
   BuildExit - Builds the Exit part of the Loop statement.
*)

PROCEDURE BuildExit ;
BEGIN
   IF IsEmptyWord(ExitStack)
   THEN
      WriteFormat0('EXIT is only allowed in a LOOP statement')
   ELSE
      GenQuad(GotoOp, NulSym, NulSym, 0) ;
      PushExit(Merge(PopExit(), NextQuad-1))
   END
END BuildExit ;


(*
   BuildEndLoop - Builds the End part of the Loop statement
                  from the quad stack.
                  The Stack is expected to contain:


                  Entry                   Exit
                  =====                   ====

          Ptr ->
                  +------------+
                  | LoopQuad   |          Empty
                  |------------|

                  Quadruples

                  Goto  _  _  LoopQuad
*)

PROCEDURE BuildEndLoop ;
VAR
   Loop: CARDINAL ;
BEGIN
   PopT(Loop) ;
   GenQuad(GotoOp, NulSym, NulSym, Loop) ;
   BackPatch(PopExit(), NextQuad)
END BuildEndLoop ;


(*
   BuildThenIf - Builds the Then part of the If statement
                 from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

         Ptr ->                                          <- Ptr
                 +------------+          +------------+
                 | t   | f    |          | 0    | f   |
                 |------------|          |------------|

                 Quadruples

                 The true exit is BackPatched to point to
                 the NextQuad.
*)

PROCEDURE BuildThenIf ;
VAR
   t, f: CARDINAL ;
BEGIN
   CheckBooleanId ;
   PopBool(t, f) ;
   BackPatch(t, NextQuad) ;
   PushBool(0, f)
END BuildThenIf ;


(*
   BuildElse - Builds the Else part of the If statement
               from the quad stack.
               The Stack is expected to contain:


               Entry                   Exit
               =====                   ====

       Ptr ->
               +------------+          +------------+
               | t   | f    |          | t+q  | 0   |
               |------------|          |------------|

               Quadruples

               q    GotoOp  _  _  0
               q+1  <- BackPatched from f
*)

PROCEDURE BuildElse ;
VAR
   t, f: CARDINAL ;
BEGIN
   GenQuad(GotoOp, NulSym, NulSym, 0) ;
   PopBool(t, f) ;
   BackPatch(f, NextQuad) ;
   PushBool(Merge(t, NextQuad-1), 0)   (* NextQuad-1 = Goto Quad *)
END BuildElse ;


(*
   BuildEndIf - Builds the End part of the If statement
                from the quad stack.
                The Stack is expected to contain:


                Entry                   Exit
                =====                   ====

        Ptr ->
                +------------+
                | t   | f    |          Empty
                |------------|

                Quadruples

                Both t and f are backpatched to point to the NextQuad
*)

PROCEDURE BuildEndIf ;
VAR
   t, f: CARDINAL ;
BEGIN
   PopBool(t, f) ;
   BackPatch(t, NextQuad) ;
   BackPatch(f, NextQuad)
END BuildEndIf ;


(*
   BuildElsif1 - Builds the Elsif part of the If statement
                 from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

         Ptr ->
                 +------------+          +------------+
                 | t   | f    |          | t+q  | 0   |
                 |------------|          |------------|

                 Quadruples

                 q    GotoOp  _  _  0
                 q+1  <- BackPatched from f
*)

PROCEDURE BuildElsif1 ;
VAR
   t, f: CARDINAL ;
BEGIN
   GenQuad(GotoOp, NulSym, NulSym, 0) ;
   PopBool(t, f) ;
   BackPatch(f, NextQuad) ;
   PushBool(Merge(t, NextQuad-1), 0)   (* NextQuad-1 = Goto Quad *)
END BuildElsif1 ;


(*
   BuildElsif2 - Builds the Elsif until part of the If statement
                 from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

          Ptr ->
                 +--------------+
                 | 0    | f1    |                            <- Ptr
                 |--------------|          +---------------+
                 | t2   | f2    |          | t2    | f1+f2 |
                 |--------------|          |---------------|
*)

PROCEDURE BuildElsif2 ;
VAR
   t1, f1,
   t2, f2: CARDINAL ;
BEGIN
   PopBool(t1, f1) ;
   Assert(t1=0) ;
   PopBool(t2, f2) ;
   PushBool(t2, Merge(f1, f2))
END BuildElsif2 ;


(*
   BuildPseudoBy - Builds the Non existant part of the By
                   clause of the For statement
                   from the quad stack.
                   The Stack is expected to contain:


                   Entry                   Exit
                   =====                   ====

                                                           <- Ptr
                                           +------------+
            Ptr ->                         | BySym | t  |
                   +------------+          |------------|
                   | e    | t   |          | e     | t  |
                   |------------|          |------------|
*)

PROCEDURE BuildPseudoBy ;
VAR
   e, t: CARDINAL ;
BEGIN
   PopTF(e, t) ;
   PushTF(e, t) ;
   IF t=NulSym
   THEN
      t := GetType(e)
   END ;
   IF t=NulSym
   THEN
      PushTF(MakeConstLit(MakeKey('1')), t)
   ELSE
      PushTF(Convert, NulSym) ;
      PushT(t) ;
      PushT(MakeConstLit(MakeKey('1'))) ;
      PushT(2) ;          (* Two parameters *)
      BuildConvertFunction
   END
END BuildPseudoBy ;


(*
   BuildForLoopToRangeCheck - builds the range check to ensure that the id
                              does not exceed the limits of its type.
*)

PROCEDURE BuildForLoopToRangeCheck ;
VAR
   d, dt,
   e, et: CARDINAL ;
BEGIN
   PopTF(e, et) ;
   PopTF(d, dt) ;
   BuildRange(InitForLoopToRangeCheck(d, e)) ;
   PushTF(d, dt) ;
   PushTF(e, et)
END BuildForLoopToRangeCheck ;


(*
   BuildForToByDo - Builds the For To By Do part of the For statement
                    from the quad stack.
                    The Stack is expected to contain:


                    Entry                   Exit
                    =====                   ====

                    
             Ptr ->                                           <- Ptr
                    +----------------+      |----------------|
                    | BySym | ByType |      | ForQuad        |
                    |----------------|      |----------------|
                    | e2             |      | LastValue      |
                    |----------------|      |----------------|
                    | e1             |      | BySym | ByType |
                    |----------------|      |----------------|
                    | Ident          |      | IdentSym       |
                    |----------------|      |----------------|


                    x := e1 ;
                    LASTVALUE := ((e2-e1) DIV BySym) * BySym + e1
                    IF BySym<0
                    THEN
                       IF e1<e2
                       THEN
                          goto exit
                       END
                    ELSE
                       IF e1>e2
                       THEN
                          goto exit
                       END
                    END ;
                    LOOP
                       body
                       IF x=LASTVALUE
                       THEN
                          goto exit
                       END ;
                       INC(x, BySym)
                    END

                    Quadruples:

                    q     BecomesOp  IdentSym  _  e1
                    q+    LastValue  := ((e1-e2) DIV by) * by + e1
                    q+1   if >=      by        0  q+..2
                    q+2   GotoOp                  q+3
                    q+3   If >=      e1  e2       q+5
                    q+4   GotoOp                  exit
                    q+5   ..
                    q+..1 Goto                    q+..5
                    q+..2 If >=      e2  e1       q+..4
                    q+..3 GotoOp                  exit
                    q+..4 ..

                    The For Loop is regarded:

                    For ident := e1 To e2 By by Do

                    End
*)

PROCEDURE BuildForToByDo ;
VAR
   l1, l2    : LineNote ;
   e1, e2,
   Id        : Name ;
   FinalValue,
   exit1,
   IdSym,
   BySym,
   ByType,
   ForLoop,
   t, f      : CARDINAL ;
   etype,
   t1, f1    : CARDINAL ;
BEGIN
   l2 := PopLineNo() ;
   l1 := PopLineNo() ;
   UseLineNote(l1) ;
   PushFor(0) ;
   PopTF(BySym, ByType) ;
   PopT(e2) ;
   PopT(e1) ;
   PopT(Id) ;
   IdSym := RequestSym(Id) ;
   IF NOT IsExpressionCompatible(GetType(e1), GetType(e2))
   THEN
      WriteFormat0('incompatible types found in FOR loop header') ;
      CheckExpressionCompatible(GetType(e1), GetType(e2))
   END ;
   IF NOT IsExpressionCompatible(GetType(e1), ByType)
   THEN
      WriteFormat0('incompatible types found in FOR loop header') ;
      CheckExpressionCompatible(GetType(e1), ByType)
   ELSIF NOT IsExpressionCompatible(GetType(e2), ByType)
   THEN
      WriteFormat0('incompatible types found in FOR loop header') ;
      CheckExpressionCompatible(GetType(e2), ByType)
   END ;
   BuildRange(InitForLoopBeginRangeCheck(IdSym, e1)) ;
   PushT(IdSym) ;
   PushT(e1) ;
   BuildAssignmentWithoutBounds(TRUE, TRUE) ;

   UseLineNote(l2) ;
   FinalValue := MakeTemporary(AreConstant(IsConst(e1) AND IsConst(e2) AND
                                           IsConst(BySym))) ;
   PutVar(FinalValue, GetType(IdSym)) ;
   etype := MixTypes(GetType(e1), GetType(e2), GetTokenNo()) ;
   e1 := doConvert(etype, e1) ;
   e2 := doConvert(etype, e2) ;

   PushTF(FinalValue, GetType(FinalValue)) ;
   PushTF(e2, GetType(e2)) ;  (* FinalValue := ((e1-e2) DIV By) * By + e1 *)
   PushT(MinusTok) ;
   PushTF(e1, GetType(e1)) ;
   doBuildBinaryOp(TRUE, FALSE) ;
   PushT(DivideTok) ;
   PushTF(BySym, ByType) ;
   doBuildBinaryOp(FALSE, FALSE) ;
   PushT(TimesTok) ;
   PushTF(BySym, ByType) ;
   doBuildBinaryOp(FALSE, FALSE) ;
   PushT(PlusTok) ;
   PushTF(e1, GetType(e1)) ;
   doBuildBinaryOp(FALSE, FALSE) ;
   BuildForLoopToRangeCheck ;
   BuildAssignmentWithoutBounds(FALSE, FALSE) ;

   (* q+1 if >=      by        0  q+..2 *)
   (* q+2 GotoOp                  q+3   *)
   PushTF(BySym, ByType) ;  (* BuildRelOp  1st parameter *)
   PushT(GreaterEqualTok) ; (*             2nd parameter *)
                                        (* 3rd parameter *)
   PushTF(MakeConstLit(MakeKey('0')), ByType) ;

   BuildRelOp ;
   PopBool(t, f) ;
   BackPatch(f, NextQuad) ;
   (* q+3 If >=       e1  e2      q+5  *)
   (* q+4 GotoOp                  Exit *)
   PushTF(e1, GetType(e1)) ; (* BuildRelOp  1st parameter *)
   PushT(GreaterEqualTok) ;  (*             2nd parameter *)
   PushTF(e2, GetType(e2)) ; (*             3rd parameter *)
   BuildRelOp ;
   PopBool(t1, exit1) ;
   BackPatch(t1, NextQuad) ;
   PushFor(Merge(PopFor(), exit1)) ;       (* merge exit1 *)

   GenQuad(GotoOp, NulSym, NulSym, 0) ;
   ForLoop := NextQuad-1 ;

   (* ELSE *)

   BackPatch(t, NextQuad) ;
   PushTF(e2, GetType(e2)) ; (* BuildRelOp  1st parameter *)
   PushT(GreaterEqualTok) ;  (*             2nd parameter *)
   PushTF(e1, GetType(e1)) ; (*             3rd parameter *)
   BuildRelOp ;
   PopBool(t1, exit1) ;
   BackPatch(t1, NextQuad) ;
   PushFor(Merge(PopFor(), exit1)) ;       (* merge exit1 *)

   BackPatch(ForLoop, NextQuad) ; (* fixes the start of the for loop *)
   ForLoop := NextQuad ;

   (* and set up the stack *)

   PushTF(IdSym, GetSym(IdSym)) ;
   PushTF(BySym, ByType) ;
   PushTF(FinalValue, GetType(FinalValue)) ;
   PushT(ForLoop)
END BuildForToByDo ;


(*
   BuildEndFor - Builds the End part of the For statement
                 from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

         Ptr ->
                 +----------------+
                 | ForQuad        |
                 |----------------|
                 | LastValue      |
                 |----------------|
                 | BySym | ByType |
                 |----------------|
                 | IdSym          |      Empty
                 |----------------|
*)

PROCEDURE BuildEndFor ;
VAR
   t, f,
   tsym,
   IncQuad,
   ForQuad: CARDINAL ;
   LastSym,
   ByType,
   BySym,
   IdSym  : CARDINAL ;
BEGIN
   PopT(ForQuad) ;
   PopT(LastSym) ;
   PopTF(BySym, ByType) ;
   PopT(IdSym) ;

   (* IF IdSym=LastSym THEN exit END *)
   PushTF(IdSym, GetType(IdSym)) ;
   PushT(EqualTok) ;
   PushTF(LastSym, GetType(LastSym)) ;
   BuildRelOp ;
   PopBool(t, f) ;
   
   BackPatch(t, NextQuad) ;
   GenQuad(GotoOp, NulSym, NulSym, 0) ;
   PushFor(Merge(PopFor(), NextQuad-1)) ;
   BackPatch(f, NextQuad) ;
   IF GetMode(IdSym)=LeftValue
   THEN
      (* index variable is a LeftValue, therefore we must dereference it *)
      tsym := MakeTemporary(RightValue) ;
      PutVar(tsym, GetType(IdSym)) ;
      CheckPointerThroughNil(IdSym) ;
      doIndrX(tsym, IdSym) ;
      BuildRange(InitForLoopEndRangeCheck(tsym, BySym)) ;
      IncQuad := NextQuad ;
      GenQuad(AddOp, tsym, tsym, BySym) ;
      CheckPointerThroughNil(IdSym) ;
      GenQuad(XIndrOp, IdSym, GetType(IdSym), tsym)
   ELSE
      BuildRange(InitForLoopEndRangeCheck(IdSym, BySym)) ;
      IncQuad := NextQuad ;
      GenQuad(AddOp, IdSym, IdSym, BySym)
   END ;
   GenQuad(GotoOp, NulSym, NulSym, ForQuad) ;
   BackPatch(PopFor(), NextQuad) ;
   AddForInfo(ForQuad, NextQuad-1, IncQuad, IdSym)
END BuildEndFor ;


(*
   BuildCaseStart - starts the case statement.
                    It initializes a backpatch list on the compile
                    time stack, the list is used to contain all
                    case break points. The list is later backpatched
                    and contains all positions of the case statement
                    which jump to the end of the case statement.
                    The stack also contains room for a boolean
                    expression, this is needed to allow , operator
                    in the CaseField alternatives.

                    The Stack is expected to contain:


                    Entry                   Exit
                    =====                   ====

                                                           <- Ptr
                                            +------------+
                    Empty                   | 0    | 0   |
                                            |------------|
                                            | 0    | 0   |
                                            |------------|
*)

PROCEDURE BuildCaseStart ;
BEGIN
   BuildRange(InitCaseBounds(PushCase(NulSym, NulSym))) ;
   PushBool(0, 0) ;  (* BackPatch list initialized *)
   PushBool(0, 0)    (* Room for a boolean expression *)
END BuildCaseStart ;


(*
   BuildCaseStartStatementSequence - starts the statement sequence
                                     inside a case clause.
                                     BackPatches the true exit to the
                                     NextQuad.
                                     The Stack:

                                     Entry             Exit

                              Ptr ->                                  <- Ptr
                                     +-----------+     +------------+
                                     | t   | f   |     | 0   | f    |
                                     |-----------|     |------------|
*)

PROCEDURE BuildCaseStartStatementSequence ;
VAR
   t, f: CARDINAL ;
BEGIN
   PopBool(t, f) ;
   BackPatch(t, NextQuad) ;
   PushBool(0, f)
END BuildCaseStartStatementSequence ;


(*
   BuildCaseEndStatementSequence - ends the statement sequence
                                   inside a case clause.
                                   BackPatches the false exit f1 to the
                                   NextQuad.
                                   Asserts that t1 and f2 is 0
                                   Pushes t2+q and 0

                                   Quadruples:

                                   q  GotoOp  _  _  0

                                   The Stack:

                                   Entry             Exit

                            Ptr ->                                  <- Ptr
                                   +-----------+     +------------+
                                   | t1  | f1  |     | 0    | 0   |
                                   |-----------|     |------------|
                                   | t2  | f2  |     | t2+q | 0   |
                                   |-----------|     |------------|
*)

PROCEDURE BuildCaseEndStatementSequence ;
VAR
   t1, f1,
   t2, f2: CARDINAL ;
BEGIN
   GenQuad(GotoOp, NulSym, NulSym, 0) ;
   PopBool(t1, f1) ;
   PopBool(t2, f2) ;          (* t2 contains the break list for the case *)
   BackPatch(f1, NextQuad) ;  (* f1 no longer needed *)
   Assert(t1=0) ;
   Assert(f2=0) ;
   PushBool(Merge(t2, NextQuad-1), 0) ;  (* NextQuad-1 = Goto Quad *)
   PushBool(0, 0)             (* Room for boolean expression *)
END BuildCaseEndStatementSequence ;


(*
   BuildCaseRange - builds the range testing quaruples for
                    a case clause.

                    IF (e1>=ce1) AND (e1<=ce2)
                    THEN

                    ELS..

                    The Stack:

                    Entry             Exit

             Ptr ->
                    +-----------+
                    | ce2       |                   <- Ptr
                    |-----------|     +-----------+
                    | ce1       |     | t   | f   |
                    |-----------|     |-----------|
                    | t1  | f1  |     | t1  | f1  |
                    |-----------|     |-----------|
                    | t2  | f2  |     | t2  | f2  |
                    |-----------|     |-----------|
                    | e1        |     | e1        |
                    |-----------|     |-----------|
*)

PROCEDURE BuildCaseRange ;
VAR
   ce1, ce2,
   e1,
   t2, f2,
   t1, f1  : CARDINAL ;
BEGIN
   PopT(ce2) ;
   PopT(ce1) ;
   AddRange(ce1, ce2, GetTokenNo()) ;
   PopBool(t1, f1) ;
   PopBool(t2, f2) ;
   PopT(e1) ;
   PushT(e1) ;         (* leave e1 on bottom of stack when exit procedure *)
   PushBool(t2, f2) ;
   PushBool(t1, f1) ;  (* also leave t1 and f1 on the bottom of the stack *)
   PushT(e1) ;
   PushT(GreaterEqualTok) ;
   PushT(ce1) ;
   BuildRelOp ;
   PushT(AndTok) ;
   RecordOp ;
   PushT(e1) ;
   PushT(LessEqualTok) ;
   PushT(ce2) ;
   BuildRelOp ;
   BuildBinaryOp
END BuildCaseRange ;


(*
   BuildCaseEquality - builds the range testing quadruples for
                       a case clause.

                       IF e1=ce1
                       THEN

                       ELS..

                       The Stack:

                       Entry             Exit

                Ptr ->
                       +-----------+     +-----------+
                       | ce1       |     | t   | f   |
                       |-----------|     |-----------|
                       | t1  | f1  |     | t1  | f1  |
                       |-----------|     |-----------|
                       | t2  | f2  |     | t2  | f2  |
                       |-----------|     |-----------|
                       | e1        |     | e1        |
                       |-----------|     |-----------|
*)
 
PROCEDURE BuildCaseEquality ;
VAR
   ce1, e1,
   t2, f2,
   t1, f1 : CARDINAL ;
BEGIN
   PopT(ce1) ;
   AddRange(ce1, NulSym, GetTokenNo()) ;
   PopBool(t1, f1) ;
   PopBool(t2, f2) ;
   PopT(e1) ;
   PushT(e1) ;        (* leave e1 on bottom of stack when exit procedure *)
   PushBool(t2, f2) ; (* also leave t2 and f2 on the bottom of the stack *)
   PushBool(t1, f1) ;
   PushT(e1) ;
   PushT(EqualTok) ;
   PushT(ce1) ;
   BuildRelOp
END BuildCaseEquality ;


(*
   BuildCaseList - merges two case tests into one

                   The Stack:
 
                   Entry             Exit
 
            Ptr ->
                   +-----------+
                   | t2  | f2  |
                   |-----------|     +-------------+
                   | t1  | f1  |     | t1+t2| f1+f2|
                   |-----------|     |-------------|
*)

PROCEDURE BuildCaseList ; 
VAR
   t2, f2,
   t1, f1: CARDINAL ;
BEGIN
   PopBool(t2, f2) ;
   PopBool(t1, f1) ;
   PushBool(Merge(t1, t2), Merge(f1, f2))
END BuildCaseList ;


(*
   BuildCaseOr - builds the , in the case clause.
 
                 The Stack:
 
                 Entry             Exit
 
          Ptr ->                                  <- Ptr
                 +-----------+     +------------+
                 | t   | f   |     | t    | 0   |
                 |-----------|     |------------|
*)
 
PROCEDURE BuildCaseOr ;
VAR 
   t, f: CARDINAL ;
BEGIN 
   PopBool(t, f) ; 
   BackPatch(f, NextQuad) ; 
   PushBool(t, 0)
END BuildCaseOr ; 


(*
   BuildCaseElse - builds the else of case clause.
 
                  The Stack:
 
                  Entry             Exit
 
           Ptr ->                                  <- Ptr
                  +-----------+     +------------+
                  | t   | f   |     | t    | 0   |
                  |-----------|     |------------|
*)
 
PROCEDURE BuildCaseElse ;
VAR 
   t, f: CARDINAL ;
BEGIN 
   PopBool(t, f) ; 
   BackPatch(f, NextQuad) ; 
   PushBool(t, 0)
END BuildCaseElse ; 


(* 
   BuildCaseEnd - builds the end of case clause.

                  The Stack:

                  Entry             Exit

           Ptr ->
                  +-----------+
                  | t1  | f1  |
                  |-----------|
                  | t2  | f2  |
                  |-----------|
                  | e1        |
                  |-----------|     Empty
*)

PROCEDURE BuildCaseEnd ;
VAR
   e1,
   t, f: CARDINAL ;
BEGIN
   PopBool(t, f) ;
   BackPatch(f, NextQuad) ;
   BackPatch(t, NextQuad) ;
   PopBool(t, f) ;
   BackPatch(f, NextQuad) ;
   BackPatch(t, NextQuad) ;
   PopT(e1) ;
   PopCase
END BuildCaseEnd ;


(*
   BuildCaseCheck - builds the case checking code to ensure that
                    the program does not need an else clause at runtime.
                    The stack is unaltered.
*)

PROCEDURE BuildCaseCheck ;
BEGIN
   BuildError(InitNoElseRangeCheck())
END BuildCaseCheck ;


(*
   BuildNulParam - Builds a nul parameter on the stack.
                   The Stack:

                   Entry             Exit

                                                    <- Ptr
                   Empty             +------------+
                                     | 0          |
                                     |------------|
*)

PROCEDURE BuildNulParam ;
BEGIN
   PushT(0)
END BuildNulParam ;


(*
   BuildSizeCheckStart - switches off all quadruple generation if the function SIZE or HIGH
                         is being "called".  This should be done as SIZE only requires the
                         actual type of the expression, not its value.  Consider the problem of
                         SIZE(UninitializedPointer^) which is quite legal and it must
                         also be safe!
                         ISO Modula-2 also allows HIGH(a[0]) for a two dimensional array
                         and there is no need to compute a[0], we just need to follow the
                         type and count dimensions.  However if SIZE(a) or HIGH(a) occurs
                         and, a, is an unbounded array then we turn on quadruple generation.

                         The Stack is expected to contain:


                         Entry                       Exit
                         =====                       ====

                 Ptr ->                                                 <- Ptr
                        +----------------+          +-----------------+
                        | ProcSym | Type |          | ProcSym | Type  |
                        |----------------|          |-----------------|
*)

PROCEDURE BuildSizeCheckStart ;
VAR
   ProcSym, Type: CARDINAL ;
BEGIN
   PopTF(ProcSym, Type) ;
   IF ProcSym=Size
   THEN
      QuadrupleGeneration := FALSE ;
      BuildingSize := TRUE ;
   ELSIF ProcSym=High
   THEN
      QuadrupleGeneration := FALSE ;
      BuildingHigh := TRUE
   END ;
   PushTF(ProcSym, Type)
END BuildSizeCheckStart ;


(*
   BuildSizeCheckEnd - checks to see whether the function "called" was in fact SIZE.
                       If so then we restore quadruple generation.
*)

PROCEDURE BuildSizeCheckEnd (ProcSym: CARDINAL) ;
BEGIN
   IF ProcSym=Size
   THEN
      QuadrupleGeneration := TRUE ;
      BuildingSize := FALSE ;
   ELSIF ProcSym=High
   THEN
      QuadrupleGeneration := TRUE ;
      BuildingHigh := FALSE
   END ;
END BuildSizeCheckEnd ;


(*
   BuildProcedureCall - builds a procedure call.
                        Although this procedure does not directly
                        destroy the procedure parameters, it calls
                        routine which will manipulate the stack and
                        so the entry and exit states of the stack are shown.

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildProcedureCall ;
VAR
   NoOfParam,
   ProcSym  : CARDINAL ;
   n        : Name ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   PushT(NoOfParam) ;  (* Compile time stack restored to entry state *)
   IF IsPseudoBaseProcedure(ProcSym) OR IsPseudoSystemProcedure(ProcSym)
   THEN
      DumpStack ;
      ManipulatePseudoCallParameters ;
      DumpStack ;
      BuildPseudoProcedureCall ;
      DumpStack ;
   ELSIF IsUnknown(ProcSym)
   THEN
      MetaError1('{%1Ua} is not recognised as a procedure, check declaration or import', ProcSym) ;
      PopN(NoOfParam + 2)
   ELSE
      DumpStack ;
      BuildRealProcedureCall ;
      DumpStack ;
   END
END BuildProcedureCall ;


(*
   BuildRealProcedureCall - builds a real procedure call.
                            The Stack:


                            Entry                      Exit

                     Ptr ->
                            +----------------+
                            | NoOfParam      |
                            |----------------|
                            | Param 1        |
                            |----------------|
                            | Param 2        |
                            |----------------|
                            .                .
                            .                .
                            .                .
                            |----------------|
                            | Param #        |
                            |----------------|
                            | ProcSym | Type |         Empty
                            |----------------|
*)

PROCEDURE BuildRealProcedureCall ;
VAR
   NoOfParam: CARDINAL ;
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   PushT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+2) ;
   ProcSym := SkipConst(ProcSym) ;
   IF IsVar(ProcSym)
   THEN
      (* Procedure Variable ? *)
      ProcSym := SkipType(OperandF(NoOfParam+2))
   END ;
   IF IsDefImp(GetScope(ProcSym)) AND IsDefinitionForC(GetScope(ProcSym))
   THEN
      BuildRealFuncProcCall(FALSE, TRUE)
   ELSE
      BuildRealFuncProcCall(FALSE, FALSE)
   END
END BuildRealProcedureCall ;


(*
   BuildRealProcFuncCall - builds a real procedure or function call.
                           The Stack:


                            Entry                      Exit

                     Ptr ->
                            +----------------+
                            | NoOfParam      |
                            |----------------|
                            | Param 1        |
                            |----------------|
                            | Param 2        |
                            |----------------|
                            .                .
                            .                .
                            .                .
                            |----------------|
                            | Param #        |
                            |----------------|
                            | ProcSym | Type |         Empty
                            |----------------|
*)

PROCEDURE BuildRealFuncProcCall (IsFunc, IsForC: BOOLEAN) ;
VAR
   e             : Error ;
   n             : Name ;
   ForcedFunc,
   ParamConstant : BOOLEAN ;
   NoOfParameters,
   i, pi,
   ReturnVar,
   ProcSym,
   Proc          : CARDINAL ;
BEGIN
   CheckProcedureParameters(IsForC) ;
   PopT(NoOfParameters) ;
   PushT(NoOfParameters) ;  (* Restore stack to original state *)
   ProcSym := OperandT(NoOfParameters+2) ;
   ProcSym := SkipConst(ProcSym) ;
   ForcedFunc := FALSE ;
   IF IsVar(ProcSym)
   THEN
      (* Procedure Variable ? *)
      Proc := SkipType(OperandF(NoOfParameters+2)) ;
      ParamConstant := FALSE
   ELSE
      Proc := ProcSym ;
      ParamConstant := IsProcedureBuiltin(Proc)
   END ;
   IF IsFunc
   THEN
      IF GetType(Proc)=NulSym
      THEN
         MetaErrors1('procedure {%1a} cannot be used as a function',
                     'procedure {%1Da} does not have a return type',
                     Proc)
      END
   ELSE
      (* is being called as a procedure *)
      IF GetType(Proc)#NulSym
      THEN
         (* however it was declared as a procedure function *)
         IF NOT IsReturnOptional(Proc)
         THEN
            MetaErrors1('function {%1a} is being called but its return value is ignored',
                        'function {%1Da} return a type {%1ta:of {%1ta}}',
                        Proc)
         END ;
         IsFunc := TRUE ;
         ForcedFunc := TRUE
      END
   END ;
   ManipulateParameters(IsForC) ;
   PopT(NoOfParameters) ;
   IF IsFunc
   THEN
      GenQuad(ParamOp, 0, Proc, ProcSym)  (* Space for return value *)
   END ;
   IF PushParametersLeftToRight
   THEN
      IF (NoOfParameters+1=NoOfParam(Proc)) AND UsesOptArg(Proc)
      THEN
         GenQuad(OptParamOp, NoOfParam(Proc), Proc, Proc)
      END ;
      i := NoOfParameters ;
      pi := 1 ;     (* stack index referencing stacked parameter, i *)
      WHILE i>0 DO
         GenQuad(ParamOp, i, Proc, OperandT(pi)) ;
         IF NOT IsConst(OperandT(pi))
         THEN
            ParamConstant := FALSE
         END ;
         DEC(i) ;
         INC(pi)
      END
   ELSE
      i := 1 ;
      pi := NoOfParameters ;   (* stack index referencing stacked parameter, i *)
      WHILE i<=NoOfParameters DO
         GenQuad(ParamOp, i, Proc, OperandT(pi)) ;
         IF NOT IsConst(OperandT(pi))
         THEN
            ParamConstant := FALSE
         END ;
         INC(i) ;
         DEC(pi)
      END ;
      IF (NoOfParameters+1=NoOfParam(Proc)) AND UsesOptArg(Proc)
      THEN
         GenQuad(OptParamOp, NoOfParam(Proc), Proc, Proc)
      END
   END ;
   GenQuad(CallOp, NulSym, NulSym, ProcSym) ;
   PopN(NoOfParameters+1) ; (* Destroy arguments and procedure call *)
   IF IsFunc
   THEN
      (* ReturnVar - will have the type of the procedure *)
      (* it would be neat to allow constant parameters
         to builtins to always be evaluated at compile
         time --fixme--
      *)
      ReturnVar := MakeTemporary(AreConstant(ParamConstant)) ;
      IF ReturnVar=2873
      THEN
         stop
      END ;
      (* ReturnVar := MakeTemporary(RightValue) ; *)
      PutVar(ReturnVar, GetType(Proc)) ;
      GenQuad(FunctValueOp, ReturnVar, NulSym, Proc) ;
      IF NOT ForcedFunc
      THEN
         PushTF(ReturnVar, GetType(Proc))
      END
   END
END BuildRealFuncProcCall ;


(*
   CheckProcedureParameters - Checks the parameters which are being passed to
                              procedure ProcSym.

                              The Stack:
   
   
                              Entry                      Exit
   
                       Ptr ->                                               <- Ptr
                              +----------------+         +----------------+
                              | NoOfParam      |         | NoOfParam      |
                              |----------------|         |----------------|
                              | Param 1        |         | Param 1        |
                              |----------------|         |----------------|
                              | Param 2        |         | Param 2        |
                              |----------------|         |----------------|
                              .                .         .                .
                              .                .         .                .
                              .                .         .                .
                              |----------------|         |----------------|
                              | Param #        |         | Param #        |
                              |----------------|         |----------------|
                              | ProcSym | Type |         | ProcSym | Type |
                              |----------------|         |----------------|

*)

PROCEDURE CheckProcedureParameters (IsForC: BOOLEAN) ;
VAR
   n1, n2      : Name ;
   e           : Error ;
   Unbounded   : BOOLEAN ;
   Actual,
   FormalI,
   FormalIType,
   ParamTotal,
   TypeSym,
   pi,
   Proc,
   ProcSym,
   i, Var      : CARDINAL ;
BEGIN
   PopT(ParamTotal) ;
   PushT(ParamTotal) ;  (* Restore stack to origional state *)
   ProcSym := OperandT(ParamTotal+1+1) ;
   IF IsVar(ProcSym) AND IsProcType(SkipType(GetType(ProcSym)))
   THEN
      (* Procedure Variable ? *)
      Proc := SkipType(OperandF(ParamTotal+1+1))
   ELSE
      Proc := SkipConst(ProcSym)
   END ;
   IF NOT (IsProcedure(Proc) OR IsProcType(Proc))
   THEN
      IF IsUnknown(Proc)
      THEN
         MetaError1('{%1Ua} is not recognised as a procedure, check declaration or import', Proc)
      ELSE
         MetaErrors1('{%1a} is not recognised as a procedure, check declaration or import',
                     '{%1Ua} is not recognised as a procedure, check declaration or import',
                     Proc)
      END
   END ;
   IF CompilerDebugging
   THEN
      n1 := GetSymName(Proc) ;
      printf1('  %a ( ', n1)
   END ;
   i := 1 ;
   pi := ParamTotal+1 ;   (* stack index referencing stacked parameter, i *)
   WHILE i<=ParamTotal DO
      IF i<=NoOfParam(Proc)
      THEN
         FormalI := GetParam(Proc, i) ;
         IF CompilerDebugging
         THEN
            n1 := GetSymName(FormalI) ;
            n2 := GetSymName(GetType(FormalI)) ;
            printf2('%a: %a', n1, n2)
         END ;
         Actual := OperandT(pi) ;
         BuildRange(InitTypesParameterCheck(Proc, i, FormalI, Actual)) ;
         IF IsConst(Actual)
         THEN
            IF IsVarParam(Proc, i)
            THEN
               FailParameter('trying to pass a constant to a VAR parameter',
                             Actual, FormalI, Proc, i)
            ELSIF IsConstString(Actual)
            THEN
               IF (GetStringLength(Actual) = 0)   (* if = 0 then it maybe unknown at this time *)
               THEN
                  (* dont check this yet *)
               ELSIF IsArray(SkipType(GetType(FormalI))) AND (GetType(SkipType(GetType(FormalI)))=Char)
               THEN
                  (* allow string literals to be passed to ARRAY [0..n] OF CHAR *)
               ELSIF (GetStringLength(Actual) = 1)   (* if = 1 then it maybe treated as a char *)
               THEN
                  CheckParameter(Actual, FormalI, Proc, i, NIL)
               ELSIF NOT IsUnboundedParam(Proc, i)
               THEN
                  IF IsForC AND (GetType(FormalI)=Address)
                  THEN
                     FailParameter('a string constant can either be passed to an ADDRESS parameter or an ARRAY OF CHAR',
                                   Actual, FormalI, Proc, i)
                  ELSE
                     FailParameter('cannot pass a string constant to a non unbounded array parameter',
                                   Actual, FormalI, Proc, i)
                  END
               END
            END
         ELSE
            CheckParameter(Actual, FormalI, Proc, i, NIL)
         END
      ELSE
         IF IsForC AND UsesVarArgs(Proc)
         THEN
            (* these are varargs, therefore we don't check them *)
            i := ParamTotal
         ELSE
            MetaError2('too many parameters {%2n} passed to {%1a} ', Proc, i)
         END
      END ;
      INC(i) ;
      DEC(pi) ;
      IF CompilerDebugging
      THEN
         IF i<=ParamTotal
         THEN
            printf0('; ')
         ELSE
            printf0(' ) ; \n')
         END
      END
   END
END CheckProcedureParameters ;


(*
   CheckProcTypeAndProcedure - checks the ProcType with the call.
*)

PROCEDURE CheckProcTypeAndProcedure (ProcType: CARDINAL; call: CARDINAL; TypeList: List) ;
VAR
   n1, n2          : Name ;
   i, n, t         : CARDINAL ;
   CheckedProcedure: CARDINAL ;
   e               : Error ;
   s               : String ;
BEGIN
   n := NoOfParam(ProcType) ;
   IF IsVar(call) OR IsTemporary(call) OR IsParameter(call)
   THEN
      CheckedProcedure := SkipType(GetType(call))
   ELSE
      CheckedProcedure := call
   END ;
   IF n#NoOfParam(CheckedProcedure)
   THEN
      e := NewError(GetDeclared(ProcType)) ;
      n1 := GetSymName(call) ;
      n2 := GetSymName(ProcType) ;
      ErrorFormat2(e, 'procedure (%a) is a parameter being passed as variable (%a) but they are declared with different number of parameters',
                   n1, n2) ;
      e := ChainError(GetDeclared(call), e) ;
      t := NoOfParam(CheckedProcedure) ;
      IF n<2
      THEN
         ErrorFormat3(e, 'procedure (%a) is being called incorrectly with (%d) parameter, declared with (%d)',
                      n1, n, t)
      ELSE
         ErrorFormat3(e, 'procedure (%a) is being called incorrectly with (%d) parameters, declared with (%d)',
                      n1, n, t)
      END
   ELSE
      i := 1 ;
      WHILE i<=n DO
         IF IsVarParam(ProcType, i) # IsVarParam(CheckedProcedure, i)
         THEN
            MetaError3('parameter {%3n} in {%1dD} causes a mismatch it was declared as a {%2d}', ProcType, GetNth(ProcType, i), i) ;
            MetaError3('parameter {%3n} in {%1dD} causes a mismatch it was declared as a {%2d}', call, GetNth(call, i), i)
         END ;
         BuildRange(InitTypesParameterCheck(CheckedProcedure, i,
                                            GetParam(CheckedProcedure, i),
                                            GetParam(ProcType, i))) ;
         CheckParameter(GetParam(CheckedProcedure, i), GetParam(ProcType, i), call, i, TypeList) ;
         INC(i)
      END
   END
END CheckProcTypeAndProcedure ;


(*
   IsReallyPointer - returns TRUE is sym is a pointer, address or a type declared
                     as a pointer or address.
*)

PROCEDURE IsReallyPointer (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsVar(Sym)
   THEN
      Sym := GetType(Sym)
   END ;
   Sym := SkipType(Sym) ;
   RETURN( IsPointer(Sym) OR (Sym=Address) )
END IsReallyPointer ;


(*
   LegalUnboundedParam - returns TRUE if the parameter, Actual, can legally be
                         passed to ProcSym, i, the, Formal, parameter.
*)

PROCEDURE LegalUnboundedParam (ProcSym, i, ActualType, Actual, Formal: CARDINAL) : BOOLEAN ;
VAR
   FormalType: CARDINAL ;
   n, m      : CARDINAL ;
BEGIN
   ActualType := SkipType(ActualType) ;
   FormalType := SkipType(GetType(Formal)) ;
   FormalType := GetType(FormalType) ;   (* type of the unbounded ARRAY *)
   IF IsArray(ActualType)
   THEN
      m := GetDimension(Formal) ;
      n := 0 ;
      WHILE IsArray(ActualType) DO
         INC(n) ;
         ActualType := SkipType(GetType(ActualType)) ;
         IF (m=n) AND (ActualType=FormalType)
         THEN
            RETURN( TRUE )
         END
      END ;
      IF n=m
      THEN
         (* now we fall though and test ActualType against FormalType *)
      ELSE
         IF IsGenericSystemType(FormalType)
         THEN
            RETURN( TRUE )
         ELSE
            FailParameter('attempting to pass an array with the incorrect number dimenisons to an unbounded formal parameter of different dimensions',
                          Actual, Formal, ProcSym, i) ;
            RETURN( FALSE )
         END
      END
   ELSIF IsUnbounded(ActualType)
   THEN
      IF GetDimension(Formal)=GetDimension(Actual)
      THEN
         (* now we fall though and test ActualType against FormalType *)
         ActualType := GetType(ActualType)
      ELSE
         IF IsGenericSystemType(FormalType)
         THEN
            RETURN( TRUE )
         ELSE
            FailParameter('attempting to pass an unbounded array with the incorrect number dimenisons to an unbounded formal parameter of different dimensions',
                          Actual, Formal, ProcSym, i) ;
            RETURN( FALSE )
         END
      END
   END ;
   IF ((FormalType=Word) OR (ActualType=Word)) OR
      ((FormalType=Byte) OR (ActualType=Byte)) OR
      ((FormalType=Loc)  OR (ActualType=Loc))  OR
      IsParameterCompatible(FormalType, ActualType)
   THEN
      (* we think it is legal, but we ask post pass 3 to check as
         not all types are known at this point *)
      RETURN( TRUE )
   ELSE
      FailParameter('identifier with an incompatible type is being passed to this procedure',
                    Actual, Formal, ProcSym, i) ;
      RETURN( FALSE )
   END
END LegalUnboundedParam ;


(*
   CheckParameter - checks that types ActualType and FormalType are compatible for parameter
                    passing. ProcSym is the procedure and i is the parameter number.

                    We obey the following rules:

                    (1)  we allow WORD, BYTE, LOC to be compitable with any like sized
                         type.
                    (2)  we allow ADDRESS to be compatible with any pointer type.
                    (3)  we relax INTEGER and CARDINAL checking for Temporary variables.

                    Note that type sizes are checked during the code generation pass.
*)

PROCEDURE CheckParameter (Actual, Formal, ProcSym: CARDINAL; i: CARDINAL; TypeList: List) ;
VAR
   n                  : Name ;
   NewList            : BOOLEAN ;
   ActualType, FormalType: CARDINAL ;
BEGIN
   FormalType := GetType(Formal) ;
   IF IsConstString(Actual) AND (GetStringLength(Actual) = 1)   (* if = 1 then it maybe treated as a char *)
   THEN
      ActualType := Char
   ELSIF Actual=Boolean
   THEN
      ActualType := Actual
   ELSE
      ActualType := GetType(Actual)
   END ;
   IF TypeList=NIL
   THEN
      NewList := TRUE ;
      InitList(TypeList)
   ELSE
      NewList := FALSE
   END ;
   IF IsItemInList(TypeList, ActualType)
   THEN
      (* no need to check *)
      RETURN
   END ;
   IncludeItemIntoList(TypeList, ActualType) ;
   IF IsProcType(FormalType)
   THEN
      IF (NOT IsProcedure(Actual)) AND ((ActualType=NulSym) OR (NOT IsProcType(SkipType(ActualType))))
      THEN
         FailParameter('expecting a procedure or procedure variable as a parameter',
                       Actual, Formal, ProcSym, i) ;
         RETURN
      END ;
      IF IsProcedure(Actual) AND IsProcedureNested(Actual)
      THEN
         n := GetSymName(Actual) ;
         WriteFormat2('cannot pass a nested procedure, %a, as parameter number %d as the outer scope will be unknown at runtime',
                      n, i)
      END ;
      (* we can check the return type of both proc types *)
      IF (ActualType#NulSym) AND IsProcType(ActualType)
      THEN
         IF ((GetType(ActualType)#NulSym) AND (GetType(FormalType)=NulSym))
         THEN
            FailParameter('the item being passed is a function whereas the formal procedure parameter is a procedure',
                          Actual, Formal, ProcSym, i) ;
            RETURN
         ELSIF ((GetType(ActualType)=NulSym) AND (GetType(FormalType)#NulSym))
         THEN
            FailParameter('the item being passed is a procedure whereas the formal procedure parameter is a function',
                          Actual, Formal, ProcSym, i) ;
            RETURN
         ELSIF AssignmentRequiresWarning(GetType(ActualType), GetType(FormalType))
         THEN
            WarnParameter('the return result of the procedure variable parameter may not be compatible on other targets with the return result of the item being passed',
                          Actual, Formal, ProcSym, i) ;
            RETURN
         ELSIF NOT IsParameterCompatible(GetType(ActualType), GetType(FormalType))
         THEN
            FailParameter('the return result of the procedure variable parameter is not compatible with the return result of the item being passed',
                          Actual, Formal, ProcSym, i) ;
            RETURN
         END
      END ;
      (* now to check each parameter of the proc type *)
      CheckProcTypeAndProcedure(FormalType, Actual, TypeList)
   ELSIF (ActualType#FormalType) AND (ActualType#NulSym)
   THEN
      IF IsUnknown(FormalType)
      THEN
         FailParameter('procedure parameter type is undeclared',
                       Actual, Formal, ProcSym, i) ;
         RETURN
      END ;
      IF IsUnbounded(ActualType) AND (NOT IsUnboundedParam(ProcSym, i))
      THEN
         FailParameter('attempting to pass an unbounded array to a NON unbounded parameter',
                       Actual, Formal, ProcSym, i) ;
         RETURN
      ELSIF IsUnboundedParam(ProcSym, i)
      THEN
         IF NOT LegalUnboundedParam(ProcSym, i, ActualType, Actual, Formal)
         THEN
            RETURN
         END
      ELSIF ActualType#FormalType
      THEN
         IF AssignmentRequiresWarning(FormalType, ActualType)
         THEN
            WarnParameter('identifier being passed to this procedure may contain a possibly incompatible type when compiling for a different target',
                          Actual, Formal, ProcSym, i)
         ELSIF IsParameterCompatible(FormalType, ActualType)
         THEN
            (* we think it is legal, but we ask post pass 3 to check as
               not all types are known at this point *)
         ELSE
            FailParameter('identifier with an incompatible type is being passed to this procedure',
                          Actual, Formal, ProcSym, i)
         END
      END
   END ;
   IF NewList
   THEN
      KillList(TypeList)
   END
END CheckParameter ;


(*
   DescribeType - returns a String describing a symbol, Sym, name and its type.
*)

PROCEDURE DescribeType (Sym: CARDINAL) : String ;
VAR
   n1, n2   : Name ;
   s, s1, s2: String ;
   i,
   Low, High,
   Subrange,
   Subscript,
   Type     : CARDINAL ;
BEGIN
   s := NIL ;
   IF IsConstString(Sym)
   THEN
      IF (GetStringLength(Sym) = 1)   (* if = 1 then it maybe treated as a char *)
      THEN
         s := InitString('(constant string) or CHAR')
      ELSE
         s := InitString('(constant string)')
      END
   ELSIF IsConst(Sym)
   THEN
      s := InitString('(constant)')
   ELSIF IsUnknown(Sym)
   THEN
      s := InitString('(unknown)')
   ELSE
      Type := GetType(Sym) ;
      IF Type=NulSym
      THEN
         s := InitString('(unknown)')
      ELSIF IsUnbounded(Type)
      THEN
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(Type))))) ;
         s := Sprintf1(Mark(InitString('ARRAY OF %s')), s1)
      ELSIF IsArray(Type)
      THEN
         s := InitString('ARRAY [') ;
         Subscript := GetArraySubscript(Type) ;
         IF Subscript#NulSym
         THEN
            Assert(IsSubscript(Subscript)) ;
            Subrange := GetType(Subscript) ;
            IF NOT IsSubrange(Subrange)
            THEN
               n1 := GetSymName(Sym) ;
               n2 := GetSymName(Subrange) ;
               WriteFormat3('error in definition of array (%a) in subscript (%d) which has no subrange, instead type given is (%a)',
                            n1, i, n2)
            END ;
            Assert(IsSubrange(Subrange)) ;
            GetSubrange(Subrange, High, Low) ;
            s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Low)))) ;
            s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(High)))) ;
            s := ConCat(s, Mark(Sprintf2(Mark(InitString('%s..%s')),
                                         s1, s2)))
         END ;
         s1 := Mark(DescribeType(Type)) ;
         s := ConCat(ConCat(s, Mark(InitString('] OF '))), s1)
      ELSE
         IF IsUnknown(Type)
         THEN
            s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Type)))) ;
            s := Sprintf1(Mark(InitString('%s (currently unknown, check declaration or import)')),
                          s1)
         ELSE
            s := InitStringCharStar(KeyToCharStar(GetSymName(Type)))
         END
      END
   END ;
   RETURN( s )
END DescribeType ;


(*
   FailParameter - generates an error message indicating that a parameter
                   declaration has failed.

                   The parameters are:

                   CurrentState  - string describing the current failing state.
                   Given         - the token that the source code provided.
                   Expecting     - token or identifier that was expected.
                   ParameterNo   - parameter number that has failed.
                   ProcedureSym  - procedure symbol where parameter has failed.

                   If any parameter is Nul then it is ignored.
*)

PROCEDURE FailParameter (CurrentState : ARRAY OF CHAR;
                         Given        : CARDINAL;
                         Expecting    : CARDINAL;
                         ProcedureSym : CARDINAL;
                         ParameterNo  : CARDINAL) ;
VAR
   First,
   ExpectType,
   ReturnType: CARDINAL ;
   e         : Error ;
   s, s1, s2 : String ;
BEGIN
   s := InitString('') ;
   IF CompilingImplementationModule()
   THEN
      s := ConCat(s, Sprintf0(Mark(InitString('error found while compiling the implementation module\n'))))
   ELSIF CompilingProgramModule()
   THEN
      s := ConCat(s, Sprintf0(Mark(InitString('error found while compiling the program module\n'))))
   END ;
   s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(ProcedureSym)))) ;
   s := ConCat(s, Mark(Sprintf2(Mark(InitString('problem in parameter %d, PROCEDURE %s (')),
                                ParameterNo, s1)));
   IF NoOfParam(ProcedureSym)>=ParameterNo
   THEN
      IF ParameterNo>1
      THEN
         s := ConCat(s, Mark(InitString('.., ')))
      END ;
      IF IsVarParam(ProcedureSym, ParameterNo)
      THEN
         s := ConCat(s, Mark(InitString('VAR ')))
      END ;

      First := GetDeclared(GetNthParam(ProcedureSym, ParameterNo)) ;
      ExpectType := GetType(Expecting) ;
      IF IsUnboundedParam(ProcedureSym, ParameterNo)
      THEN
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Expecting)))) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(ExpectType))))) ;
         s := ConCat(s, Mark(Sprintf2(Mark(InitString('%s: ARRAY OF %s')),
                                      s1, s2)))
      ELSE
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Expecting)))) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(ExpectType)))) ;
         s := ConCat(s, Mark(Sprintf2(Mark(InitString('%s: %s')), s1, s2)))
      END ;
      IF ParameterNo<NoOfParam(ProcedureSym)
      THEN
         s := ConCat(s, Mark(InitString('; ... ')))
      END
   ELSE
      First := GetDeclared(ProcedureSym) ;
      IF NoOfParam(ProcedureSym)>0
      THEN
         s := ConCat(s, Mark(InitString('..')))
      END
   END ;
   ReturnType := GetType(ProcedureSym) ;
   IF ReturnType=NulSym
   THEN
      s := ConCat(s, Sprintf0(Mark(InitString(') ;\n'))))
   ELSE
      s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(ReturnType)))) ;
      s := ConCat(s, Mark(Sprintf1(Mark(InitString(') : %s ;\n')), s1)))
   END ;
   IF IsConstString(Given)
   THEN
      s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Given)))) ;
      s := ConCat(s, Mark(Sprintf1(Mark(InitString("item being passed is '%s'")),
                                   s1)))
   ELSIF IsTemporary(Given)
   THEN
      s := ConCat(s, Mark(InitString("item being passed has type")))
   ELSE
      s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Given)))) ;
      s := ConCat(s, Mark(Sprintf1(Mark(InitString("item being passed is '%s'")),
                                   s1)))
   END ;
   s1 := DescribeType(Given) ;
   s2 := Mark(InitString(CurrentState)) ;
   s := ConCat(s, Mark(Sprintf2(Mark(InitString(': %s\nparameter mismatch: %s')),
                                s1, s2))) ;
   ErrorStringAt2(s, First, GetTokenNo())
END FailParameter ;


(*
   WarnParameter - generates a warning message indicating that a parameter
                   use might cause problems on another target.

                   The parameters are:

                   CurrentState  - string describing the current failing state.
                   Given         - the token that the source code provided.
                   Expecting     - token or identifier that was expected.
                   ParameterNo   - parameter number that has failed.
                   ProcedureSym  - procedure symbol where parameter has failed.

                   If any parameter is Nul then it is ignored.
*)

PROCEDURE WarnParameter (CurrentState : ARRAY OF CHAR;
                         Given        : CARDINAL;
                         Expecting    : CARDINAL;
                         ProcedureSym : CARDINAL;
                         ParameterNo  : CARDINAL) ;
VAR
   First,
   ExpectType,
   ReturnType: CARDINAL ;
   e         : Error ;
   s, s1, s2 : String ;
BEGIN
   s := InitString('') ;
   IF CompilingImplementationModule()
   THEN
      s := ConCat(s, Sprintf0(Mark(InitString('warning issued while compiling the implementation module\n'))))
   ELSIF CompilingProgramModule()
   THEN
      s := ConCat(s, Sprintf0(Mark(InitString('warning issued while compiling the program module\n'))))
   END ;
   s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(ProcedureSym)))) ;
   s := ConCat(s, Mark(Sprintf2(Mark(InitString('problem in parameter %d, PROCEDURE %s (')),
                                ParameterNo,
                                s1))) ;
   IF NoOfParam(ProcedureSym)>=ParameterNo
   THEN
      IF ParameterNo>1
      THEN
         s := ConCat(s, Mark(InitString('.., ')))
      END ;
      IF IsVarParam(ProcedureSym, ParameterNo)
      THEN
         s := ConCat(s, Mark(InitString('VAR ')))
      END ;

      First := GetDeclared(GetNthParam(ProcedureSym, ParameterNo)) ;
      ExpectType := GetType(Expecting) ;
      IF IsUnboundedParam(ProcedureSym, ParameterNo)
      THEN
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Expecting)))) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(ExpectType))))) ;
         s := ConCat(s, Mark(Sprintf2(Mark(InitString('%s: ARRAY OF %s')),
                                      s1, s2)))
      ELSE
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Expecting)))) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(ExpectType)))) ;
         s := ConCat(s, Mark(Sprintf2(Mark(InitString('%s: %s')), s1, s2)))
      END ;
      IF ParameterNo<NoOfParam(ProcedureSym)
      THEN
         s := ConCat(s, Mark(InitString('; ... ')))
      END
   ELSE
      First := GetDeclared(ProcedureSym) ;
      IF NoOfParam(ProcedureSym)>0
      THEN
         s := ConCat(s, Mark(InitString('..')))
      END
   END ;
   ReturnType := GetType(ProcedureSym) ;
   IF ReturnType=NulSym
   THEN
      s := ConCat(s, Sprintf0(Mark(InitString(') ;\n'))))
   ELSE
      s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(ReturnType)))) ;
      s := ConCat(s, Mark(Sprintf1(Mark(InitString(') : %s ;\n')), s1)))
   END ;
   IF IsConstString(Given)
   THEN
      s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Given)))) ;
      s := ConCat(s, Mark(Sprintf1(Mark(InitString("item being passed is '%s'")),
                                   s1)))
   ELSIF IsTemporary(Given)
   THEN
      s := ConCat(s, Mark(InitString("item being passed has type")))
   ELSE
      s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Given)))) ;
      s := ConCat(s, Mark(Sprintf1(Mark(InitString("item being passed is '%s'")),
                                   s1)))
   END ;
   s1 := DescribeType(Given) ;
   s2 := Mark(InitString(CurrentState)) ;
   s := ConCat(s, Mark(Sprintf2(Mark(InitString(': %s\nparameter mismatch: %s')),
                                s1, s2))) ;
   ErrorString(NewWarning(GetTokenNo()), Dup(s)) ;
   ErrorString(NewWarning(First), s)
END WarnParameter ;


(*
   ExpectVariable - checks to see whether, sym, is declared as a variable.
                    If not then it generates an error message.
*)

PROCEDURE ExpectVariable (a: ARRAY OF CHAR; sym: CARDINAL) ;
VAR
   e         : Error ;
   s1, s2, s3: String ;
BEGIN
   IF NOT IsVar(sym)
   THEN
      e := NewError(GetTokenNo()) ;
      IF IsUnknown(sym)
      THEN
         s1 := Mark(InitString(a)) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(sym)))) ;
         ErrorString(e, Sprintf2(Mark(InitString('%s but was given an undeclared symbol %s')), s1, s2))
      ELSE
         s1 := Mark(InitString(a)) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(sym)))) ;
         s3 := Mark(DescribeType(sym)) ;
         ErrorString(e, Sprintf3(Mark(InitString('%s but was given %s: %s')),
                                 s1, s2, s3))
      END
   END
END ExpectVariable ;


(*
   doIndrX - perform des = *exp with a conversion if necessary.
*)

PROCEDURE doIndrX (des, exp: CARDINAL) ;
VAR
   t: CARDINAL ;
BEGIN
   IF SkipType(GetType(des))=SkipType(GetType(exp))
   THEN
      GenQuad(IndrXOp, des, GetType(des), exp)
   ELSE
      t := MakeTemporary(RightValue) ;
      PutVar(t, GetType(exp)) ;
      GenQuad(IndrXOp, t, GetType(exp), exp) ;
      GenQuad(BecomesOp, des, NulSym, doVal(GetType(des), t))
   END
END doIndrX ;


(*
   MakeRightValue - returns a temporary which will have the RightValue of symbol, Sym.
                    If Sym is a right value and has type, type, then no quadruples are
                    generated and Sym is returned. Otherwise a new temporary is created
                    and an IndrX quadruple is generated.
*)

PROCEDURE MakeRightValue (Sym: CARDINAL; type: CARDINAL) : CARDINAL ;
VAR
   t: CARDINAL ;
BEGIN
   IF GetMode(Sym)=RightValue
   THEN
      IF GetType(Sym)=type
      THEN
         RETURN( Sym )   (* already a RightValue with desired type *)
      ELSE
         (*
            type change or mode change, type changes are a pain, but I've
            left them here as it is perhaps easier to remove them later
            inside M2SubExp (ideally I'd like to identify a fix here and
            in the type checking routines, but it is likely to become too
            complex.  So currently we are stuck with sometimes creating
            temporaries just to change type)
         *)
         t := MakeTemporary(RightValue) ;
         PutVar(t, type) ;
         GenQuad(BecomesOp, t, NulSym, doVal(type, Sym)) ;
         RETURN( t )
      END
   ELSE
      t := MakeTemporary(RightValue) ;
      PutVar(t, type) ;
      CheckPointerThroughNil(Sym) ;
      doIndrX(t, Sym) ;
      RETURN( t )
   END
END MakeRightValue ;


(*
   MakeLeftValue - returns a temporary coresponding to the LeftValue of
                   symbol, Sym. No quadruple is generated if Sym is already
                   a LeftValue and has the same type.
*)

PROCEDURE MakeLeftValue (Sym: CARDINAL; with: ModeOfAddr; type: CARDINAL) : CARDINAL ;
VAR
   t: CARDINAL ;
BEGIN
   IF GetMode(Sym)=LeftValue
   THEN
      IF GetType(Sym)=type
      THEN
         RETURN( Sym )
      ELSE
         (*
            type change or mode change, type changes are a pain, but I've
            left them here as it is perhaps easier to remove them later
            inside M2SubExp (ideally I'd like to identify a fix here and
            in the type checking routines, but it is likely to become too
            complex.  So currently we are stuck with sometimes creating
            temporaries just to change type)
         *)
         t := MakeTemporary(with) ;
         PutVar(t, type) ;
         GenQuad(BecomesOp, t, NulSym, Sym) ;
         RETURN( t )
      END
   ELSE
      t := MakeTemporary(with) ;
      PutVar(t, type) ;
      GenQuad(AddrOp, t, NulSym, Sym) ;
      RETURN( t )
   END
END MakeLeftValue ;


(*
   ManipulatePseudoCallParameters - manipulates the parameters to a pseudo function or
                                    procedure. It dereferences all LeftValue parameters
                                    and Boolean parameters.
                                    The Stack:


                                    Entry                      Exit

                             Ptr ->                            exactly the same
                                    +----------------+
                                    | NoOfParameters |
                                    |----------------|
                                    | Param 1        |
                                    |----------------|
                                    | Param 2        |
                                    |----------------|
                                    .                .
                                    .                .
                                    .                .
                                    |----------------|
                                    | Param #        |
                                    |----------------|
                                    | ProcSym | Type |
                                    |----------------|

*)

PROCEDURE ManipulatePseudoCallParameters ;
VAR
   NoOfParameters,
   ProcSym, Proc,
   i, pi         : CARDINAL ;
   f             : BoolFrame ;
BEGIN
   PopT(NoOfParameters) ;
   PushT(NoOfParameters) ;    (* restored to original state *)
   (* Ptr points to the ProcSym *)
   ProcSym := OperandT(NoOfParameters+1+1) ;
   IF IsVar(ProcSym)
   THEN
      InternalError('expecting a pseudo procedure or a type', __FILE__, __LINE__) ;
   ELSE
      Proc := ProcSym
   END ;
   i := 1 ;
   pi := NoOfParameters+1 ;
   WHILE i<=NoOfParameters DO
      IF (GetMode(OperandT(pi))=LeftValue) AND
         (Proc#Adr) AND (Proc#Size) AND (Proc#TSize) AND (Proc#High) AND
         (* procedures which have first parameter as a VAR param *)
         (((Proc#Inc) AND (Proc#Incl) AND (Proc#Dec) AND (Proc#Excl) AND (Proc#New) AND (Proc#Dispose)) OR (i>1))
      THEN
         (* must dereference LeftValue *)
         f := PeepAddress(BoolStack, pi) ;
         f^.TrueExit := MakeRightValue(OperandT(pi), GetType(OperandT(pi)))
      END ;
      INC(i) ;
      DEC(pi)
   END
END ManipulatePseudoCallParameters ;


(*
   ConvertStringToC - creates a new ConstString symbol with a C style string.
*)

PROCEDURE ConvertStringToC (sym: CARDINAL) : CARDINAL ;
VAR
   s: String ;
   n: CARDINAL ;
BEGIN
   s := Sprintf0(InitStringCharStar(KeyToCharStar(GetString(sym)))) ;
   n := MakeConstLitString(makekey(string(s))) ;
   s := KillString(s) ;
   RETURN( n )
END ConvertStringToC ;


(*
   ManipulateParameters - manipulates the procedure parameters in
                          preparation for a procedure call.
                          Prepares Boolean, Unbounded and VAR parameters.
                          The Stack:


                          Entry                      Exit

                   Ptr ->                            exactly the same
                          +----------------+
                          | NoOfParameters |
                          |----------------|
                          | Param 1        |
                          |----------------|
                          | Param 2        |
                          |----------------|
                          .                .
                          .                .
                          .                .
                          |----------------|
                          | Param #        |
                          |----------------|
                          | ProcSym | Type |
                          |----------------|
*)

PROCEDURE ManipulateParameters (IsForC: BOOLEAN) ;
VAR
   np           : CARDINAL ;
   n            : Name ;
   s            : String ;
   ArraySym,
   UnboundedType,
   ParamType,
   NoOfParameters,
   i, pi,
   ProcSym, rw,
   Proc,
   t,
   true, false,
   Des          : CARDINAL ;
   f            : BoolFrame ;
BEGIN
   PopT(NoOfParameters) ;
   ProcSym := OperandT(NoOfParameters+1) ;
   IF IsVar(ProcSym)
   THEN
      (* Procedure Variable ? *)
      Proc := SkipType(OperandF(NoOfParameters+1))
   ELSE
      Proc := SkipConst(ProcSym)
   END ;

   IF IsForC AND UsesVarArgs(Proc)
   THEN
      IF NoOfParameters<NoOfParam(Proc)
      THEN
         s := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Proc)))) ;
         np := NoOfParam(Proc) ;
         ErrorStringAt2(Sprintf3(Mark(InitString('attempting to pass (%d) parameters to procedure (%s) which was declared with varargs but contains at least (%d) parameters')),
                                 NoOfParameters, s, np),
                        GetTokenNo(), GetTokenNo())
      END
   ELSIF UsesOptArg(Proc)
   THEN
      IF NOT ((NoOfParameters=NoOfParam(Proc)) OR (NoOfParameters+1=NoOfParam(Proc)))
      THEN
         s := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Proc)))) ;
         np := NoOfParam(Proc) ;
         ErrorStringAt2(Sprintf3(Mark(InitString('attempting to pass (%d) parameters to procedure (%s) which was declared with an optarg with a maximum of (%d) parameters')),
                                 NoOfParameters, s, np),
                        GetTokenNo(), GetTokenNo())
      END
   ELSIF NoOfParameters#NoOfParam(Proc)
   THEN
      s := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Proc)))) ;
      np := NoOfParam(Proc) ;
      ErrorStringAt2(Sprintf3(Mark(InitString('attempting to pass (%d) parameters to procedure (%s) which was declared with (%d) parameters')),
                              NoOfParameters, s, np),
                     GetTokenNo(), GetTokenNo())
   END ;
   i := 1 ;
   pi := NoOfParameters ;
   WHILE i<=NoOfParameters DO
      f := PeepAddress(BoolStack, pi) ;
      rw := OperandMergeRW(pi) ;
      Assert(IsLegal(rw)) ;
      IF i>NoOfParam(Proc)
      THEN
         IF IsForC AND UsesVarArgs(Proc)
         THEN
            IF (GetType(OperandT(pi))#NulSym) AND IsArray(SkipType(GetType(OperandT(pi))))
            THEN
               f^.TrueExit := MakeLeftValue(OperandT(pi), RightValue, Address) ;
               MarkAsReadWrite(rw)
            ELSIF IsConstString(OperandT(pi))
            THEN
               f^.TrueExit := MakeLeftValue(ConvertStringToC(OperandT(pi)), RightValue, Address) ;
               MarkAsReadWrite(rw)
            ELSIF (GetType(OperandT(pi))#NulSym) AND IsUnbounded(GetType(OperandT(pi)))
            THEN
               MarkAsReadWrite(rw) ;
               (* pass the address field of an unbounded variable *)
               PushTF(Adr, Address) ;
               PushT(f^.TrueExit) ;
               PushT(1) ;
               BuildAdrFunction ;
               PopT(f^.TrueExit)
            ELSIF GetMode(OperandT(pi))=LeftValue
            THEN
               MarkAsReadWrite(rw) ;
               (* must dereference LeftValue (even if we are passing variable as a vararg) *)
               t := MakeTemporary(RightValue) ;
               PutVar(t, GetType(OperandT(pi))) ;
               CheckPointerThroughNil(OperandT(pi)) ;
               doIndrX(t, OperandT(pi)) ;
               f^.TrueExit := t
            END
         ELSE
            n := GetSymName(Proc) ;
            WriteFormat1('parameter not expected for procedure %a', n)
         END
      ELSIF IsForC AND IsUnboundedParam(Proc, i) AND
            (GetType(OperandT(pi))#NulSym) AND IsArray(SkipType(GetType(OperandT(pi))))
      THEN
         f^.TrueExit := MakeLeftValue(OperandT(pi), RightValue, Address) ;
         MarkAsReadWrite(rw)
      ELSIF IsForC AND IsUnboundedParam(Proc, i) AND
            (GetType(OperandT(pi))#NulSym) AND IsUnbounded(SkipType(GetType(OperandT(pi))))
      THEN
         MarkAsReadWrite(rw) ;
         (* pass the address field of an unbounded variable *)
         PushTF(Adr, Address) ;
         PushT(f^.TrueExit) ;
         PushT(1) ;
         BuildAdrFunction ;
         PopT(f^.TrueExit)
      ELSIF IsForC AND IsConstString(OperandT(pi))
      THEN
         f^.TrueExit := MakeLeftValue(ConvertStringToC(OperandT(pi)), RightValue, Address) ;
         MarkAsReadWrite(rw)
      ELSIF IsUnboundedParam(Proc, i)
      THEN
         t := MakeTemporary(RightValue) ;
         UnboundedType := GetType(GetParam(Proc, i)) ;
         PutVar(t, UnboundedType) ;
         ParamType := GetType(UnboundedType) ;
         IF OperandD(pi)=0
         THEN
            ArraySym := OperandT(pi)
         ELSE
            ArraySym := OperandA(pi)
         END ;
         IF IsVarParam(Proc, i)
         THEN
            MarkArrayWritten(OperandT(pi)) ;
            MarkArrayWritten(OperandA(pi)) ;
            MarkAsReadWrite(rw) ;
            AssignUnboundedVar(OperandT(pi), ArraySym, t, ParamType, OperandD(pi))
         ELSE
            MarkAsRead(rw) ;
            AssignUnboundedNonVar(OperandT(pi), ArraySym, t, ParamType, OperandD(pi))
         END ;
         f^.TrueExit := t
      ELSIF IsVarParam(Proc, i)
      THEN
         (*
            Ok must reference by address
            - but we contain the type of the referenced entity
            - we will actually use a RightValue here as we know we are about to call
              a procedure, we are NEVER going to actually use, t, other than in a ParamOp
              which ignores the distinction between Left and Right.
         *)
         MarkArrayWritten(OperandT(pi)) ;
         MarkArrayWritten(OperandA(pi)) ;
         MarkAsReadWrite(rw) ;
         f^.TrueExit := MakeLeftValue(OperandT(pi), RightValue, Address)
      ELSIF (NOT IsVarParam(Proc, i)) AND (GetMode(OperandT(pi))=LeftValue)
      THEN
         (* must dereference LeftValue *)
         t := MakeTemporary(RightValue) ;
         PutVar(t, GetType(OperandT(pi))) ;
         CheckPointerThroughNil(OperandT(pi)) ;
         doIndrX(t, OperandT(pi)) ;
         f^.TrueExit := t ;
         MarkAsRead(rw)
      ELSE
         MarkAsRead(rw)
      END ;
      INC(i) ;
      DEC(pi)
   END ;
   PushT(NoOfParameters)
END ManipulateParameters ;


(*
   IsSameUnbounded - returns TRUE if unbounded types, t1, and, t2,
                     are compatible.
*)

PROCEDURE IsSameUnbounded (t1, t2: CARDINAL) : BOOLEAN ;
BEGIN
   Assert(IsUnbounded(t1)) ;
   Assert(IsUnbounded(t2)) ;
   RETURN( SkipType(GetType(t1))=SkipType(GetType(t2)) )
END IsSameUnbounded ;


(*
   AssignUnboundedVar - assigns an Unbounded symbol fields,
                        ArrayAddress and ArrayHigh, from an array symbol.
                        UnboundedSym is not a VAR parameter and therefore
                        this procedure can complete both of the fields.
                        Sym can be a Variable with type Unbounded.
                        Sym can be a Variable with type Array.
                        Sym can be a String Constant.

                        ParamType is the TYPE of the parameter
*)

PROCEDURE AssignUnboundedVar (Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ;
VAR
   Type: CARDINAL ;
BEGIN
   IF IsConst(Sym)
   THEN
      MetaError1('{%1ad} cannot be passed to a VAR formal parameter', Sym)
   ELSIF IsVar(Sym)
   THEN
      Type := SkipType(GetType(Sym)) ;
      IF IsUnbounded(Type)
      THEN
         IF Type=GetType(UnboundedSym)
         THEN
            (* Copy Unbounded Symbol ie. UnboundedSym := Sym *)
            PushT(UnboundedSym) ;
            PushT(Sym) ;
            BuildAssignmentWithoutBounds(FALSE, TRUE)
         ELSIF IsSameUnbounded(Type, GetType(UnboundedSym)) OR
               IsGenericSystemType(ParamType)
         THEN
            UnboundedVarLinkToArray(Sym, ArraySym, UnboundedSym, ParamType, dim)
         ELSE
            MetaError1('{%1ad} cannot be passed to a VAR formal parameter', Sym)
         END
      ELSIF IsArray(Type) OR IsGenericSystemType(ParamType)
      THEN
         UnboundedVarLinkToArray(Sym, ArraySym, UnboundedSym, ParamType, dim)
      ELSE
         MetaError1('{%1ad} cannot be passed to a VAR formal parameter', Sym)
      END
   ELSE
      MetaError1('{%1ad} cannot be passed to a VAR formal parameter', Sym)
   END
END AssignUnboundedVar ;


(*
   AssignUnboundedNonVar - assigns an Unbounded symbol fields,
                           The difference between this procedure and
                           AssignUnboundedVar is that this procedure cannot
                           set the Unbounded.Address since the data from
                           Sym will be copied because parameter is NOT a VAR
                           parameter.
                           UnboundedSym is not a VAR parameter and therefore
                           this procedure can only complete the HIGH field
                           and not the ADDRESS field.
                           Sym can be a Variable with type Unbounded.
                           Sym can be a Variable with type Array.
                           Sym can be a String Constant.

                           ParamType is the TYPE of the paramater
*)

PROCEDURE AssignUnboundedNonVar (Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ;
VAR
   n           : Name ;
   Field,
   AddressField,
   Type        : CARDINAL ;
BEGIN
   IF IsConst(Sym)  (* was IsConstString(Sym) *)
   THEN
      UnboundedNonVarLinkToArray(Sym, ArraySym, UnboundedSym, ParamType, dim)
   ELSIF IsVar(Sym)
   THEN
      Type := SkipType(GetType(Sym)) ;
      IF IsUnbounded(Type)
      THEN
         UnboundedNonVarLinkToArray(Sym, ArraySym, UnboundedSym, ParamType, dim)
      ELSIF IsArray(Type) OR IsGenericSystemType(ParamType)
      THEN
         UnboundedNonVarLinkToArray(Sym, ArraySym, UnboundedSym, ParamType, dim)
      ELSE
         n := GetSymName(Sym) ;
         WriteFormat1('illegal type parameter %a: expecting Array or Dynamic Array', n)
      END
   ELSE
      n := GetSymName(Sym) ;
      WriteFormat1('illegal parameter %a which cannot be passed as: VAR ARRAY OF Type', n)
   END
END AssignUnboundedNonVar ;


(*
   GenHigh - generates a HighOp but it checks if op3 is a
             L value and if so it dereferences it.  This
             is inefficient, however it is clean and we let the gcc
             backend detect these as common subexpressions.
             It will also detect that a R value -> L value -> R value
             via indirection and eleminate these.
*)

PROCEDURE GenHigh (op1, op2, op3: CARDINAL) ;
VAR
   sym: CARDINAL ;
BEGIN
   IF (GetMode(op3)=LeftValue) AND IsUnbounded(GetType(op3))
   THEN
      sym := MakeTemporary(RightValue) ;
      PutVar(sym, GetType(op3)) ;
      doIndrX(sym, op3) ;
      GenQuad(HighOp, op1, op2, sym)
   ELSE
      GenQuad(HighOp, op1, op2, op3)
   END
END GenHigh ;


(*
   AssignHighField - 
*)

PROCEDURE AssignHighField (Sym, ArraySym, UnboundedSym, ParamType: CARDINAL;
                           actuali, formali: CARDINAL) ;
VAR
   ReturnVar,
   ArrayType,
   Field    : CARDINAL ;
BEGIN
   (* Unbounded.ArrayHigh := HIGH(ArraySym) *)
   PushTF(UnboundedSym, GetType(UnboundedSym)) ;
   Field := GetUnboundedHighOffset(GetType(UnboundedSym), formali) ;
   PushTF(Field, GetType(Field)) ;
   PushT(1) ;
   BuildDesignatorRecord ;
   IF IsGenericSystemType(ParamType)
   THEN
      ArrayType := GetType(Sym) ;
      IF IsUnbounded(ArrayType)
      THEN
         (*
          *  SIZE(parameter) DIV TSIZE(ParamType)
          *  however in this case parameter
          *  is an unbounded symbol and therefore we must use
          *  (HIGH(parameter)+1)*SIZE(unbounded type) DIV TSIZE(ParamType)
          *
          *  we call upon the function SIZE(ArraySym)
          *  remember SIZE doubles as
          *  (HIGH(a)+1) * SIZE(ArrayType) for unbounded symbols
          *)
         PushTF(calculateMultipicand(ArraySym, ArrayType, actuali-1), Cardinal) ;
         PushT(DivideTok) ;        (* Divide by                    *)
         PushTF(TSize, Cardinal) ; (* TSIZE(ParamType)             *)
         PushT(ParamType) ;
         PushT(1) ;                (* 1 parameter for TSIZE()      *)
         BuildFunctionCall ;
         BuildBinaryOp
      ELSE
         (* SIZE(parameter) DIV TSIZE(ParamType)                   *)
         PushTF(TSize, Cardinal) ;  (* TSIZE(ArrayType)            *)
         PushT(ArrayType) ;
         PushT(1) ;                (* 1 parameter for TSIZE()      *)
         BuildFunctionCall ;
         PushT(DivideTok) ;        (* Divide by                    *)
         PushTF(TSize, Cardinal) ; (* TSIZE(ParamType)             *)
         PushT(ParamType) ;
         PushT(1) ;                (* 1 parameter for TSIZE()      *)
         BuildFunctionCall ;
         BuildBinaryOp
      END ;
      (* now convert from no of elements into HIGH by subtracting 1 *)
      PushT(MinusTok) ;            (* -1                           *)
      PushT(MakeConstLit(MakeKey('1'))) ;
      BuildBinaryOp
   ELSE
      ReturnVar := MakeTemporary(RightValue) ;
      PutVar(ReturnVar, Cardinal) ;
      GenHigh(ReturnVar, formali, Sym) ;
      PushTF(ReturnVar, GetType(ReturnVar))
   END ;
   BuildAssignmentWithoutBounds(FALSE, TRUE)
END AssignHighField ;


(*
   AssignHighFields - 
*)

PROCEDURE AssignHighFields (Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ;
VAR
   type            : CARDINAL ;
   actuali, formali,
   actualn, formaln: CARDINAL ;
BEGIN
   type := SkipType(GetType(Sym)) ;
   actualn := 1 ;
   IF (type#NulSym) AND (IsUnbounded(type) OR IsArray(type))
   THEN
      actualn := GetDimension(type)
   END ;
   actuali := dim+1 ;
   formali := 1 ;
   formaln := GetDimension(SkipType(GetType(UnboundedSym))) ;
   WHILE (actuali<actualn) AND (formali<formaln) DO
      AssignHighField(Sym, ArraySym, UnboundedSym, NulSym, actuali, formali) ;
      INC(actuali) ;
      INC(formali)
   END ;
   AssignHighField(Sym, ArraySym, UnboundedSym, ParamType, actuali, formali)
END AssignHighFields ;


(*
   UnboundedNonVarLinkToArray - links an array, ArraySym, to an unbounded
                                array, UnboundedSym. The parameter is a
                                NON VAR variety.
*)

PROCEDURE UnboundedNonVarLinkToArray (Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ;
VAR
   ArrayType,
   t, f,
   AddressField,
   ArrayAdr,
   Field       : CARDINAL ;
BEGIN
   (* Unbounded.ArrayAddress := ??? runtime *)
   PushTF(UnboundedSym, GetType(UnboundedSym)) ;

   Field := GetUnboundedAddressOffset(GetType(UnboundedSym)) ;
   PushTF(Field, GetType(Field)) ;
   PushT(1) ;
   BuildDesignatorRecord ;
   PopT(AddressField) ;

   (* caller saves non var unbounded array contents *)
   GenQuad(UnboundedOp, AddressField, NulSym, Sym) ;

   AssignHighFields(Sym, ArraySym, UnboundedSym, ParamType, dim)
END UnboundedNonVarLinkToArray ;


(*
   UnboundedVarLinkToArray - links an array, ArraySym, to an unbounded array,
                             UnboundedSym. The parameter is a VAR variety.
*)

PROCEDURE UnboundedVarLinkToArray (Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ;
VAR
   ArrayType,
   Field    : CARDINAL ;
BEGIN
   (* Unbounded.ArrayAddress := ADR(Sym) *)
   PushTF(UnboundedSym, GetType(UnboundedSym)) ;
   Field := GetUnboundedAddressOffset(GetType(UnboundedSym)) ;
   PushTF(Field, GetType(Field)) ;
   PushT(1) ;
   BuildDesignatorRecord ;
   PushTF(Adr, Address) ;   (* ADR(Sym)                     *)
   PushT(Sym) ;
   PushT(1) ;               (* 1 parameter for ADR()        *)
   BuildFunctionCall ;
   BuildAssignmentWithoutBounds(FALSE, TRUE) ;

   AssignHighFields(Sym, ArraySym, UnboundedSym, ParamType, dim)
END UnboundedVarLinkToArray ;


(*
   BuildPseudoProcedureCall - builds a pseudo procedure call.
                              This procedure does not directly alter the
                              stack, but by calling routines the stack
                              will change in the following way when this
                              procedure returns.

                              The Stack:


                              Entry                      Exit

                       Ptr ->
                              +----------------+
                              | NoOfParam      |
                              |----------------|
                              | Param 1        |
                              |----------------|
                              | Param 2        |
                              |----------------|
                              .                .
                              .                .
                              .                .
                              |----------------|
                              | Param #        |
                              |----------------|
                              | ProcSym | Type |         Empty
                              |----------------|
*)

PROCEDURE BuildPseudoProcedureCall ;
VAR
   NoOfParam,
   ProcSym,
   Ptr      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   PushT(NoOfParam) ;
   (* Compile time stack restored to entry state *)
   IF ProcSym=New
   THEN
      BuildNewProcedure
   ELSIF ProcSym=Dispose
   THEN
      BuildDisposeProcedure
   ELSIF ProcSym=Inc
   THEN
      BuildIncProcedure
   ELSIF ProcSym=Dec
   THEN
      BuildDecProcedure
   ELSIF ProcSym=Incl
   THEN
      BuildInclProcedure
   ELSIF ProcSym=Excl
   THEN
      BuildExclProcedure
   ELSIF ProcSym=Throw
   THEN
      BuildThrowProcedure
   ELSE
      InternalError('pseudo procedure not implemented yet', __FILE__, __LINE__)
   END
END BuildPseudoProcedureCall ;


(*
   GetItemPointedTo - returns the symbol type that is being pointed to
                      by Sym.
*)

PROCEDURE GetItemPointedTo (Sym: CARDINAL) : CARDINAL ;
BEGIN
   IF IsPointer(Sym)
   THEN
      RETURN( GetType(Sym) )
   ELSIF IsVar(Sym) OR IsType(Sym)
   THEN
      RETURN( GetItemPointedTo(GetType(Sym)) )
   END
END GetItemPointedTo ;


(*
   BuildThrowProcedure - builds the pseudo procedure call M2RTS.Throw.
                         The Stack:


                         Entry                      Exit

                Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |
                         |----------------|
                         | Param 2        |
                         |----------------|
                         .                .
                         .                .
                         .                .
                         |----------------|
                         | Param #        |
                         |----------------|
                         | ProcSym | Type |         Empty
                         |----------------|
*)
   
PROCEDURE BuildThrowProcedure ;
VAR
   op       : CARDINAL ;
   NoOfParam: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      op := OperandT(NoOfParam) ;
      GenQuad(ThrowOp, NulSym, NulSym, op)
   ELSE
      WriteFormat0('the pseudo procedure Throw takes one INTEGER parameter')
   END ;
   PopN(NoOfParam+1)
END BuildThrowProcedure ;


(*
   BuildReThrow - creates a ThrowOp _ _ NulSym, indicating that
                  the exception needs to be rethrown.  The stack
                  is unaltered.
*)

PROCEDURE BuildReThrow ;
BEGIN
   GenQuad(ThrowOp, NulSym, NulSym, NulSym)
END BuildReThrow ;


(*
   BuildNewProcedure - builds the pseudo procedure call NEW.
                       This procedure is traditionally a "macro" for
                       NEW(x, ...) --> ALLOCATE(x, TSIZE(x^, ...))
                       One method of implementation is to emulate a "macro"
                       processor by pushing the relevant input tokens
                       back onto the input stack.
                       However this causes two problems:

                       (i)  Unnecessary code is produced for x^
                       (ii) SIZE must be imported from SYSTEM
                       Therefore we chose an alternative method of
                       implementation;
                       generate quadruples for ALLOCATE(x, TSIZE(x^, ...))
                       this, although slightly more efficient,
                       is more complex and circumvents problems (i) and (ii).

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |
                       |----------------|
                       | ProcSym | Type |         Empty
                       |----------------|
*)
   
PROCEDURE BuildNewProcedure ;
VAR
   NoOfParam,
   SizeSym,
   PtrSym,
   ProcSym,
   Ptr      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam>=1
   THEN
      ProcSym := RequestSym(MakeKey('ALLOCATE')) ;
      IF (ProcSym#NulSym) AND IsProcedure(ProcSym)
      THEN
         PtrSym := OperandT(NoOfParam) ;
         IF IsReallyPointer(PtrSym)
         THEN
            (*
               Build macro: ALLOCATE( PtrSym, SIZE(PtrSym^) )
            *)
            PushTF(TSize, Cardinal) ;(* Procedure      *)
                                     (* x^             *)
            PushT(GetItemPointedTo(PtrSym)) ;
            PushT(1) ;               (* One parameter  *)
            BuildFunctionCall ;
            PopT(SizeSym) ;

            PushT(ProcSym) ;         (* ALLOCATE       *)
            PushT(PtrSym) ;          (* x              *)
            PushT(SizeSym) ;         (* TSIZE(x^)      *)
            PushT(2) ;               (* Two parameters *)
            BuildProcedureCall

         ELSE
            WriteFormat0('argument to NEW must be a pointer')
         END
      ELSE
         WriteFormat0('ALLOCATE procedure not found for NEW substitution')
      END
   ELSE
      WriteFormat0('the pseudo procedure NEW has one or more parameters')
   END ;
   PopN(NoOfParam+1)
END BuildNewProcedure ;


(*
   BuildDisposeProcedure - builds the pseudo procedure call DISPOSE.
                           This procedure is traditionally a "macro" for
                           DISPOSE(x) --> DEALLOCATE(x, TSIZE(x^))
                           One method of implementation is to emulate a "macro"
                           processor by pushing the relevant input tokens
                           back onto the input stack.
                           However this causes two problems:

                           (i)  Unnecessary code is produced for x^
                           (ii) TSIZE must be imported from SYSTEM
                           Therefore we chose an alternative method of
                           implementation;
                           generate quadruples for DEALLOCATE(x, TSIZE(x^))
                           this, although slightly more efficient,
                           is more complex and circumvents problems (i)
                           and (ii).

                           The Stack:


                           Entry                      Exit

                    Ptr ->
                           +----------------+
                           | NoOfParam      |
                           |----------------|
                           | Param 1        |
                           |----------------|
                           | Param 2        |
                           |----------------|
                           .                .
                           .                .
                           .                .
                           |----------------|
                           | Param #        |
                           |----------------|
                           | ProcSym | Type |         Empty
                           |----------------|
*)

PROCEDURE BuildDisposeProcedure ;
VAR
   NoOfParam,
   SizeSym,
   PtrSym,
   ProcSym,
   Ptr      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam>=1
   THEN
      ProcSym := RequestSym(MakeKey('DEALLOCATE')) ;
      IF (ProcSym#NulSym) AND IsProcedure(ProcSym)
      THEN
         PtrSym := OperandT(NoOfParam) ;
         IF IsReallyPointer(PtrSym)
         THEN
            (*
               Build macro: DEALLOCATE( PtrSym, TSIZE(PtrSym^) )
            *)
            PushTF(TSize, Cardinal) ;(* Procedure      *)
                                     (* x^             *)
            PushT(GetItemPointedTo(PtrSym)) ;
            PushT(1) ;               (* One parameter  *)
            BuildFunctionCall ;
            PopT(SizeSym) ;

            PushT(ProcSym) ;         (* DEALLOCATE     *)
            PushT(PtrSym) ;          (* x              *)
            PushT(SizeSym) ;         (* TSIZE(x^)      *)
            PushT(2) ;               (* Two parameters *)
            BuildProcedureCall

         ELSE
            WriteFormat0('argument to DISPOSE must be a pointer')
         END
      ELSE
         WriteFormat0('DEALLOCATE procedure not found for DISPOSE substitution')
      END
   ELSE
      WriteFormat0('the pseudo procedure DISPOSE has one or more parameters')
   END ;
   PopN(NoOfParam+1)
END BuildDisposeProcedure ;


(*
   CheckRangeIncDec - performs des := des <tok> expr
                      with range checking (if enabled).

                               Stack
                      Entry              Exit

                                     +------------+
                      empty          | des + expr |
                                     |------------|
*)

PROCEDURE CheckRangeIncDec (des, expr: CARDINAL; tok: Name) ;
VAR
   num         : String ;
   line,
   t, f,
   dtype, etype,
   dlow, dhigh : CARDINAL ;
BEGIN
   dtype := SkipType(GetType(des)) ;
   etype := SkipType(GetType(expr)) ;
   IF WholeValueChecking AND (NOT MustNotCheckBounds)
   THEN
      IF tok=PlusTok
      THEN
         BuildRange(InitIncRangeCheck(des, expr))
      ELSE
         BuildRange(InitDecRangeCheck(des, expr))
      END
   END ;

   IF IsExpressionCompatible(dtype, etype)
   THEN
      (* the easy case simulate a straightforward macro *)
      PushTF(des, dtype) ;
      PushT(tok) ;
      PushTF(expr, etype) ;
      doBuildBinaryOp(FALSE, TRUE)
   ELSE
      IF (IsOrdinalType(dtype) OR (dtype=Address) OR IsPointer(dtype)) AND
         (IsOrdinalType(etype) OR (etype=Address) OR IsPointer(etype))
      THEN
         PushTF(des, dtype) ;
         PushT(tok) ;
         PushTF(Convert, NulSym) ;
         PushT(dtype) ;
         PushT(expr) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction ;
         doBuildBinaryOp(FALSE, TRUE)
      ELSE
         IF tok=PlusTok
         THEN
            WriteFormat0('cannot perform INC using non ordinal types')
         ELSE
            WriteFormat0('cannot perform DEC using non ordinal types')
         END ;
         PushTF(MakeConstLit(MakeKey('0')), NulSym)
      END
   END
END CheckRangeIncDec ;


(*
   BuildIncProcedure - builds the pseudo procedure call INC.
                       INC is a procedure which increments a variable.
                       It takes one or two parameters:
                       INC(a, b)  or  INC(a)
                       a := a+b   or  a := a+1

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |
                       |----------------|
                       | ProcSym | Type |         Empty
                       |----------------|
*)

PROCEDURE BuildIncProcedure ;
VAR
   NoOfParam,
   OperandSym,
   VarSym,
   TempSym,
   ProcSym   : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF (NoOfParam=1) OR (NoOfParam=2)
   THEN
      VarSym := OperandT(NoOfParam) ;  (* bottom/first parameter *)
      IF IsVar(VarSym)
      THEN
         IF NoOfParam=2
         THEN
            OperandSym := DereferenceLValue(OperandT(1))
         ELSE
            OperandSym := MakeConstLit(MakeKey('1'))
         END ;

         PushT(VarSym) ;
         TempSym := DereferenceLValue(VarSym) ;
         CheckRangeIncDec(TempSym, OperandSym, PlusTok) ;  (* TempSym + OperandSym *)
         BuildAssignmentWithoutBounds(FALSE, TRUE)   (* VarSym := TempSym + OperandSym *)
      ELSE
         ExpectVariable('base procedure INC expects a variable as a parameter',
                        VarSym)
      END
   ELSE
      WriteFormat0('base procedure INC expects 1 or 2 parameters')
   END ;
   PopN(NoOfParam+1)
END BuildIncProcedure ;


(*
   BuildDecProcedure - builds the pseudo procedure call DEC.
                       DEC is a procedure which decrements a variable.
                       It takes one or two parameters:
                       DEC(a, b)  or  DEC(a)
                       a := a-b   or  a := a-1

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |
                       |----------------|
                       | ProcSym | Type |         Empty
                       |----------------|
*)

PROCEDURE BuildDecProcedure ;
VAR
   NoOfParam,
   OperandSym,
   VarSym,
   TempSym,
   ProcSym   : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF (NoOfParam=1) OR (NoOfParam=2)
   THEN
      VarSym := OperandT(NoOfParam) ;  (* bottom/first parameter *)
      IF IsVar(VarSym)
      THEN
         IF NoOfParam=2
         THEN
            OperandSym := DereferenceLValue(OperandT(1))
         ELSE
            OperandSym := MakeConstLit(MakeKey('1'))
         END ;

         PushT(VarSym) ;
         TempSym := DereferenceLValue(VarSym) ;
         CheckRangeIncDec(TempSym, OperandSym, MinusTok) ;  (* TempSym - OperandSym *)
         BuildAssignmentWithoutBounds(FALSE, TRUE)   (* VarSym := TempSym - OperandSym *)
      ELSE
         ExpectVariable('base procedure DEC expects a variable as a parameter',
                        VarSym)
      END
   ELSE
      WriteFormat0('base procedure DEC expects 1 or 2 parameters')
   END ;
   PopN(NoOfParam+1)
END BuildDecProcedure ;


(*
   DereferenceLValue - checks to see whether, operand, is declare as an LValue
                       and if so it dereferences it.
*)

PROCEDURE DereferenceLValue (operand: CARDINAL) : CARDINAL ;
VAR
   sym: CARDINAL ;
BEGIN
   IF GetMode(operand)=LeftValue
   THEN
      (* dereference the pointer *)
      sym := MakeTemporary(AreConstant(IsConst(operand))) ;
      PutVar(sym, GetType(operand)) ;

      PushT(sym) ;
      PushT(operand) ;
      BuildAssignmentWithoutBounds(FALSE, TRUE) ;
      RETURN( sym )
   ELSE
      RETURN( operand )
   END
END DereferenceLValue ;


(*
   BuildInclProcedure - builds the pseudo procedure call INCL.
                        INCL is a procedure which adds bit b into a BITSET a.
                        It takes two parameters:
                        INCL(a, b)

                        a := a + {b}

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildInclProcedure ;
VAR
   NoOfParam,
   DerefSym,
   OperandSym,
   VarSym,
   ProcSym   : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT(2) ;
      MarkArrayWritten(OperandA(2)) ;
      OperandSym := OperandT(1) ;
      IF IsVar(VarSym)
      THEN
         IF IsSet(SkipType(GetType(VarSym)))
         THEN
            DerefSym := DereferenceLValue(OperandSym) ;
            BuildRange(InitInclCheck(VarSym, DerefSym)) ;
            GenQuad(InclOp, VarSym, NulSym, DerefSym)
         ELSE
            ExpectVariable('the first parameter to INCL must be a SET variable',
                           VarSym)
         END
      ELSE
         WriteFormat0('base procedure INCL expects a variable as a parameter')
      END
   ELSE
      WriteFormat0('base procedure INCL expects 2 parameters')
   END ;
   PopN(NoOfParam+1)
END BuildInclProcedure ;


(*
   BuildExclProcedure - builds the pseudo procedure call EXCL.
                        INCL is a procedure which removes bit b from SET a.
                        It takes two parameters:
                        EXCL(a, b)

                        a := a - {b}

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildExclProcedure ;
VAR
   NoOfParam,
   DerefSym,
   OperandSym,
   VarSym,
   ProcSym   : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT(2) ;
      MarkArrayWritten(OperandA(2)) ;
      OperandSym := OperandT(1) ;
      IF IsVar(VarSym)
      THEN
         IF IsSet(SkipType(GetType(VarSym)))
         THEN
            DerefSym := DereferenceLValue(OperandSym) ;
            BuildRange(InitExclCheck(VarSym, DerefSym)) ;
            GenQuad(ExclOp, VarSym, NulSym, DerefSym)
         ELSE
            ExpectVariable('the first parameter to EXCL must be a SET variable',
                           VarSym)
         END
      ELSE
         WriteFormat0('base procedure EXCL expects a variable as a parameter')
      END
   ELSE
      WriteFormat0('base procedure EXCL expects 2 parameters')
   END ;
   PopN(NoOfParam+1)
END BuildExclProcedure ;


(*
   CheckBuildFunction - checks to see whether ProcSym is a function
                        and if so it adds a TempSym value which will
                        hold the return value once the function finishes.
                        This procedure also generates an error message
                        if the user is calling a function and ignoring
                        the return result.  The additional TempSym
                        is not created if ProcSym is a procedure
                        and the stack is unaltered.

                        The Stack:


                       Entry                      Exit

                Ptr ->

                                                  +----------------+
                                                  | ProcSym | Type |
                       +----------------+         |----------------|
                       | ProcSym | Type |         | TempSym | Type |
                       |----------------|         |----------------|
*)

PROCEDURE CheckBuildFunction () : BOOLEAN ;
VAR
   n            : Name ;
   TempSym,
   ProcSym, Type: CARDINAL ;
BEGIN
   PopTF(ProcSym, Type) ;
   IF IsVar(ProcSym) AND IsProcType(Type)
   THEN
      IF GetType(Type)#NulSym
      THEN
         TempSym := MakeTemporary(RightValue) ;
         PutVar(TempSym, GetType(Type)) ;
         PushTF(TempSym, GetType(Type)) ;
         PushTF(ProcSym, Type) ;
         IF NOT IsReturnOptional(Type)
         THEN
            IF IsTemporary(ProcSym)
            THEN
               ErrorFormat0(NewError(GetTokenNo()),
                            'function is being called but its return value is ignored')
            ELSE
               n := GetSymName(ProcSym) ;
               ErrorFormat1(NewError(GetTokenNo()),
                            'function (%a) is being called but its return value is ignored', n)
            END
         END ;
         RETURN( TRUE )
      END
   ELSIF IsProcedure(ProcSym) AND (Type#NulSym)
   THEN
      TempSym := MakeTemporary(RightValue) ;
      PutVar(TempSym, Type) ;
      PushTF(TempSym, Type) ;
      PushTF(ProcSym, Type) ;
      IF NOT IsReturnOptional(ProcSym)
      THEN
         n := GetSymName(ProcSym) ;
         ErrorFormat1(NewError(GetTokenNo()),
                      'function (%a) is being called but its return value is ignored', n)
      END ;
      RETURN( TRUE )
   END ;
   PushTF(ProcSym, Type) ;
   RETURN( FALSE )
END CheckBuildFunction ;


(*
   BuildFunctionCall - builds a function call.
                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |                        <- Ptr
                       |----------------|         +------------+
                       | ProcSym | Type |         | ReturnVar  |
                       |----------------|         |------------|
*)

PROCEDURE BuildFunctionCall ;
VAR
   n        : Name ;
   NoOfParam,
   ProcSym,
   Ptr      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   PushT(NoOfParam) ;
   (* Compile time stack restored to entry state *)
   IF IsUnknown(ProcSym)
   THEN
      n := GetSymName(ProcSym) ;
      WriteFormat1('function %a is undefined', n) ;
      PopN(NoOfParam+2) ;
      PushT(MakeConstLit(MakeKey('0')))   (* fake return value to continue compiling *)
   ELSIF IsAModula2Type(ProcSym)
   THEN
      ManipulatePseudoCallParameters ;
      BuildTypeCoercion
   ELSIF IsPseudoSystemFunction(ProcSym) OR
         IsPseudoBaseFunction(ProcSym)
   THEN
      ManipulatePseudoCallParameters ;
      BuildPseudoFunctionCall
   ELSE
      BuildRealFunctionCall
   END
END BuildFunctionCall ;


(*
   BuildConstFunctionCall - builds a function call and checks that this function can be
                            called inside a ConstExpression.

                            The Stack:


                            Entry                      Exit

                     Ptr ->
                            +----------------+
                            | NoOfParam      |
                            |----------------|
                            | Param 1        |
                            |----------------|
                            | Param 2        |
                            |----------------|
                            .                .
                            .                .
                            .                .
                            |----------------|
                            | Param #        |                        <- Ptr
                            |----------------|         +------------+
                            | ProcSym | Type |         | ReturnVar  |
                            |----------------|         |------------|

*)

PROCEDURE BuildConstFunctionCall ;
VAR
   ConstExpression,
   NoOfParam,
   ProcSym        : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   PushT(NoOfParam) ;
   IF (ProcSym#Convert) AND
      (IsPseudoBaseFunction(ProcSym) OR
       IsPseudoSystemFunctionConstExpression(ProcSym) OR
       (IsProcedure(ProcSym) AND IsProcedureBuiltin(ProcSym)))
   THEN
      BuildFunctionCall
   ELSE
      IF IsAModula2Type(ProcSym)
      THEN
         (* type conversion *)
         IF NoOfParam=1
         THEN
            ConstExpression := OperandT(NoOfParam+1) ;
            PopN(NoOfParam+2) ;
            (*
               Build macro: CONVERT( ProcSym, ConstExpression )
            *)
            PushTF(Convert, NulSym) ;
            PushT(ProcSym) ;
            PushT(ConstExpression) ;
            PushT(2) ;          (* Two parameters *)
            BuildConvertFunction
         ELSE
            WriteFormat0('a constant type conversion can only have one argument')
         END
      ELSE
         (* error issue message and fake return stack *)
         IF Iso
         THEN
            WriteFormat0('the only functions permissible in a constant expression are: CAP, CHR, CMPLX, FLOAT, HIGH, IM, LENGTH, MAX, MIN, ODD, ORD, RE, SIZE, TSIZE, TRUNC, VAL and gcc builtins')
         ELSE
            WriteFormat0('the only functions permissible in a constant expression are: CAP, CHR, FLOAT, HIGH, MAX, MIN, ODD, ORD, SIZE, TSIZE, TRUNC, VAL and gcc builtins')
         END ;
         PopN(NoOfParam+2) ;
         PushT(MakeConstLit(MakeKey('0')))   (* fake return value to continue compiling *)
      END
   END
END BuildConstFunctionCall ;


(*
   BuildTypeCoercion - builds the type coersion.
                       MODULA-2 allows types to be coersed with no runtime
                       penility.
                       It insists that the TSIZE(t1)=TSIZE(t2) where
                       t2 variable := t2(variable of type t1).
                       The ReturnVar on the stack is of type t2.

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |  
                       |----------------|
                       | Param 1        |  
                       |----------------|
                       | Param 2        |  
                       |----------------|
                       .                .  
                       .                .  
                       .                .  
                       |----------------|
                       | Param #        |                        <- Ptr
                       |----------------|         +------------+
                       | ProcSym | Type |         | ReturnVar  |
                       |----------------|         |------------|

                       Quadruples:

                       CoerceOp  ReturnVar  Type  Param1

                       A type coercion will only be legal if the different
                       types have exactly the same size.
                       Since we can only decide this after M2Eval has processed
                       the symbol table then we create a quadruple explaining
                       the coercion taking place, the code generator can test
                       this assertion and report an error if the type sizes
                       differ.
*)

PROCEDURE BuildTypeCoercion ;
VAR
   s1, s2   : String ;
   NoOfParam,
   ReturnVar,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   IF NOT IsAModula2Type(ProcSym)
   THEN
      WriteFormat0('coersion expecting a type')
   END ;
   ReturnVar := MakeTemporary(RightValue) ;
   PutVar(ReturnVar, ProcSym) ;  (* Set ReturnVar's TYPE *)
   IF NoOfParam=1
   THEN
      IF IsConst(OperandT(1)) OR IsVar(OperandT(1))
      THEN
         GenQuad(CoerceOp, ReturnVar, ProcSym, OperandT(1)) ;
         PopN(NoOfParam+1) ;
         PushTF(ReturnVar, ProcSym)
      ELSE
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(OperandT(1))))) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(ProcSym)))) ;
         ErrorStringAt2(Sprintf2(Mark(InitString('trying to coerse (%s) which is not a variable or constant into (%s)')),
                                 s1, s2),
                        GetTokenNo(), GetDeclared(OperandT(1))) ;
         PopN(NoOfParam+1)
      END
   ELSE
      WriteFormat0('only one parameter expected in a TYPE coersion')
   END
END BuildTypeCoercion ;


(*
   BuildRealFunctionCall - builds a function call.
                           The Stack:


                           Entry                      Exit

                    Ptr ->
                           +----------------+
                           | NoOfParam      |
                           |----------------|
                           | Param 1        |
                           |----------------|
                           | Param 2        |
                           |----------------|
                           .                .
                           .                .
                           .                .
                           |----------------|
                           | Param #        |                        <- Ptr
                           |----------------|         +------------+
                           | ProcSym | Type |         | ReturnVar  |
                           |----------------|         |------------|
*)

PROCEDURE BuildRealFunctionCall ;
VAR
   NoOfParam,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   PushT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+2) ;
   IF IsVar(ProcSym)
   THEN
      (* Procedure Variable ? *)
      ProcSym := SkipType(OperandF(NoOfParam+2))
   END ;
   IF IsDefImp(GetScope(ProcSym)) AND IsDefinitionForC(GetScope(ProcSym))
   THEN
      BuildRealFuncProcCall(TRUE, TRUE)
   ELSE
      BuildRealFuncProcCall(TRUE, FALSE)
   END
END BuildRealFunctionCall ;


(*
   BuildPseudoFunctionCall - builds the pseudo function
                             The Stack:


                             Entry                      Exit

                      Ptr ->
                             +----------------+
                             | NoOfParam      |  
                             |----------------|
                             | Param 1        |  
                             |----------------|
                             | Param 2        |  
                             |----------------|
                             .                .  
                             .                .  
                             .                .  
                             |----------------|
                             | Param #        |                        <- Ptr
                             |----------------|         +------------+
                             | ProcSym | Type |         | ReturnVar  |
                             |----------------|         |------------|

*)

PROCEDURE BuildPseudoFunctionCall ;
VAR
   NoOfParam,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   PushT(NoOfParam) ;
   (* Compile time stack restored to entry state *)
   IF ProcSym=High
   THEN
      BuildHighFunction
   ELSIF ProcSym=LengthS
   THEN
      BuildLengthFunction
   ELSIF ProcSym=Adr
   THEN
      BuildAdrFunction
   ELSIF ProcSym=Size
   THEN
      BuildSizeFunction
   ELSIF ProcSym=TSize
   THEN
      BuildTSizeFunction
   ELSIF ProcSym=Convert
   THEN
      BuildConvertFunction
   ELSIF ProcSym=Odd
   THEN
      BuildOddFunction
   ELSIF ProcSym=Abs
   THEN
      BuildAbsFunction
   ELSIF ProcSym=Cap
   THEN
      BuildCapFunction
   ELSIF ProcSym=Val
   THEN
      BuildValFunction
   ELSIF ProcSym=Chr
   THEN
      BuildChrFunction
   ELSIF IsOrd(ProcSym)
   THEN
      BuildOrdFunction(ProcSym)
   ELSIF IsInt(ProcSym)
   THEN
      BuildIntFunction(ProcSym)
   ELSIF IsTrunc(ProcSym)
   THEN
      BuildTruncFunction(ProcSym)
   ELSIF IsFloat(ProcSym)
   THEN
      BuildFloatFunction(ProcSym)
   ELSIF ProcSym=Min
   THEN
      BuildMinFunction
   ELSIF ProcSym=Max
   THEN
      BuildMaxFunction
   ELSIF ProcSym=AddAdr
   THEN
      BuildAddAdrFunction
   ELSIF ProcSym=SubAdr
   THEN
      BuildSubAdrFunction
   ELSIF ProcSym=DifAdr
   THEN
      BuildDifAdrFunction
   ELSIF ProcSym=Cast
   THEN
      BuildCastFunction
   ELSIF ProcSym=Shift
   THEN
      BuildShiftFunction
   ELSIF ProcSym=Rotate
   THEN
      BuildRotateFunction
   ELSIF ProcSym=MakeAdr
   THEN
      BuildMakeAdrFunction
   ELSIF ProcSym=Re
   THEN
      BuildReFunction
   ELSIF ProcSym=Im
   THEN
      BuildImFunction
   ELSIF ProcSym=Cmplx
   THEN
      BuildCmplxFunction
   ELSE
      InternalError('pseudo function not implemented yet', __FILE__, __LINE__)
   END
END BuildPseudoFunctionCall ;


(*
   BuildAddAdrFunction - builds the pseudo procedure call ADDADR.

                         PROCEDURE ADDADR (addr: ADDRESS; offset: CARDINAL): ADDRESS ;

                         Which returns address given by (addr + offset),
                         [ the standard says that it _may_
                           "raise an exception if this address is not valid."
                           currently we do not generate any exception code ]

                         The Stack:

                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |
                         |----------------|
                         | Param 2        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|
*)

PROCEDURE BuildAddAdrFunction ;
VAR
   ReturnVar,
   NoOfParam,
   OperandSym,
   VarSym    : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT(2) ;
      OperandSym := OperandT(1) ;
      PopN(NoOfParam+1) ;
      IF IsVar(VarSym)
      THEN
         IF IsReallyPointer(VarSym) OR (GetType(VarSym)=Address)
         THEN
            ReturnVar := MakeTemporary(RightValue) ;
            PutVar(ReturnVar, Address) ;
            GenQuad(AddOp, ReturnVar, VarSym, DereferenceLValue(OperandSym)) ;
            PushTF(ReturnVar, Address)
         ELSE
            ExpectVariable('the first parameter to ADDADR must be a variable of type ADDRESS or a POINTER',
                           VarSym) ;
            PushTF(MakeConstLit(MakeKey('0')), Address)
         END
      ELSE
         WriteFormat0('SYSTEM procedure ADDADR expects a variable which has a type of ADDRESS or is a POINTER as its first parameter') ;
         PushTF(MakeConstLit(MakeKey('0')), Address)
      END
   ELSE
      WriteFormat0('SYSTEM procedure ADDADR expects 2 parameters') ;
      PopN(NoOfParam+1) ;
      PushTF(MakeConstLit(MakeKey('0')), Address)
   END
END BuildAddAdrFunction ;


(*
   BuildSubAdrFunction - builds the pseudo procedure call ADDADR.

                         PROCEDURE SUBADR (addr: ADDRESS; offset: CARDINAL): ADDRESS ;

                         Which returns address given by (addr - offset),
                         [ the standard says that it _may_
                           "raise an exception if this address is not valid."
                           currently we do not generate any exception code ]

                         The Stack:

                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |
                         |----------------|
                         | Param 2        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|
*)

PROCEDURE BuildSubAdrFunction ;
VAR
   ReturnVar,
   NoOfParam,
   OperandSym,
   VarSym    : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT(2) ;
      OperandSym := OperandT(1) ;
      PopN(NoOfParam+1) ;
      IF IsVar(VarSym)
      THEN
         IF IsReallyPointer(VarSym) OR (GetType(VarSym)=Address)
         THEN
            ReturnVar := MakeTemporary(RightValue) ;
            PutVar(ReturnVar, Address) ;
            GenQuad(SubOp, ReturnVar, VarSym, DereferenceLValue(OperandSym)) ;
            PushTF(ReturnVar, Address)
         ELSE
            ExpectVariable('the first parameter to SUBADR must be a variable of type ADDRESS or a POINTER',
                           VarSym) ;
            PushTF(MakeConstLit(MakeKey('0')), Address)
         END
      ELSE
         WriteFormat0('SYSTEM procedure SUBADR expects a variable which has a type of ADDRESS or is a POINTER as its first parameter') ;
         PushTF(MakeConstLit(MakeKey('0')), Address)
      END
   ELSE
      WriteFormat0('SYSTEM procedure SUBADR expects 2 parameters') ;
      PopN(NoOfParam+1) ;
      PushTF(MakeConstLit(MakeKey('0')), Address)
   END
END BuildSubAdrFunction ;


(*
   BuildDifAdrFunction - builds the pseudo procedure call DIFADR.

                         PROCEDURE DIFADR (addr1, addr2: ADDRESS): INTEGER ;

                         Which returns address given by (addr1 - addr2),
                         [ the standard says that it _may_
                           "raise an exception if this address is invalid or
                            address space is non-contiguous."
                           currently we do not generate any exception code ]

                         The Stack:

                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |
                         |----------------|
                         | Param 2        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|
*)

PROCEDURE BuildDifAdrFunction ;
VAR
   TempVar,
   NoOfParam,
   OperandSym,
   VarSym    : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT(2) ;
      OperandSym := OperandT(1) ;
      PopN(NoOfParam+1) ;
      IF IsVar(VarSym)
      THEN
         IF IsReallyPointer(VarSym) OR (GetType(VarSym)=Address)
         THEN
            IF IsReallyPointer(OperandSym) OR (GetType(OperandSym)=Address)
            THEN
               TempVar := MakeTemporary(RightValue) ;
               PutVar(TempVar, Address) ;
               GenQuad(SubOp, TempVar, VarSym, DereferenceLValue(OperandSym)) ;
               (*
                  Build macro: CONVERT( INTEGER, TempVar )
               *)
               PushTF(Convert, NulSym) ;
               PushT(Integer) ;
               PushT(TempVar) ;
               PushT(2) ;          (* Two parameters *)
               BuildConvertFunction
            ELSE
               ExpectVariable('the second parameter to ADDADR must be a variable of type ADDRESS or a POINTER',
                              OperandSym) ;
               PushTF(MakeConstLit(MakeKey('0')), Integer)
            END
         ELSE
            ExpectVariable('the first parameter to ADDADR must be a variable of type ADDRESS or a POINTER',
                           VarSym) ;
            PushTF(MakeConstLit(MakeKey('0')), Integer)
         END
      ELSE
         WriteFormat0('SYSTEM procedure ADDADR expects a variable which has a type of ADDRESS or is a POINTER as its first parameter') ;
         PushTF(MakeConstLit(MakeKey('0')), Integer)
      END
   ELSE
      WriteFormat0('SYSTEM procedure ADDADR expects 2 parameters') ;
      PopN(NoOfParam+1) ;
      PushTF(MakeConstLit(MakeKey('0')), Integer)
   END
END BuildDifAdrFunction ;


(*
   BuildHighFunction - checks the stack in preparation for generating
                       quadruples which perform HIGH.
                       This procedure does not alter the stack but
                       determines whether, a, in HIGH(a) is an ArraySym
                       or UnboundedSym.
                       Both cases are different and appropriate quadruple
                       generating routines are called.

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |  
                       |----------------|
                       | Param 1        |  
                       |----------------|
                       | Param 2        |  
                       |----------------|
                       .                .  
                       .                .  
                       .                .  
                       |----------------|
                       | Param #        |                        <- Ptr
                       |----------------|         +------------+
                       | ProcSym | Type |         | ReturnVar  |
                       |----------------|         |------------|

*)

PROCEDURE BuildHighFunction ;
VAR
   ProcSym,
   Type,
   NoOfParam,
   Param,
   ReturnVar: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   BuildSizeCheckEnd(ProcSym) ;   (* quadruple generation now on *)
   IF NoOfParam=1
   THEN
      Param := OperandT(1) ;
      Type := SkipType(GetType(Param)) ;
      (* Restore stack to original form *)
      PushT(NoOfParam) ;
      IF (NOT IsVar(Param)) AND (NOT IsConstString(Param)) AND (NOT IsConst(Param))
      THEN
         (* we cannot test for IsConst(Param) AND (GetType(Param)=Char)  as the type might not be assigned yet *)
         WriteFormat0('base procedure HIGH expects a variable or string constant as its parameter')
      ELSIF IsUnbounded(Type)
      THEN
         BuildHighFromUnbounded
      ELSE
         BuildConstHighFromSym
      END
   ELSE
      ErrorFormat0(NewError(GetTokenNo()),
                   'base procedure function HIGH has one parameter') ;
      PopN(2) ;
      PushTF(MakeConstLit(MakeKey('0')), Cardinal)
   END
END BuildHighFunction ;


(*
   BuildConstHighFromSym - builds the pseudo function HIGH from an Sym.
                           Sym is a constant or an array which has constant bounds
                           and therefore it can be calculated at compile time.

                           The Stack:


                           Entry                      Exit

                   Ptr ->
                           +----------------+
                           | NoOfParam      |  
                           |----------------|
                           | Param 1        |  
                           |----------------|
                           | Param 2        |  
                           |----------------|
                           .                .  
                           .                .  
                           .                .  
                           |----------------|
                           | Param #        |                        <- Ptr
                           |----------------|         +------------+
                           | ProcSym | Type |         | ReturnVar  |
                           |----------------|         |------------|
*)

PROCEDURE BuildConstHighFromSym ;
VAR
   Dim,
   NoOfParam,
   ReturnVar: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ReturnVar := MakeTemporary(ImmediateValue) ;
   Dim := OperandD(1) ;
   INC(Dim) ;
   GenHigh(ReturnVar, Dim, OperandT(1)) ;
   PopN(NoOfParam+1) ;
   PushT(ReturnVar)
END BuildConstHighFromSym ;


(*
   BuildHighFromUnbounded - builds the pseudo function HIGH from an
                            UnboundedSym.

                            The Stack:


                            Entry                      Exit

                     Ptr ->
                            +----------------+
                            | NoOfParam      |  
                            |----------------|
                            | Param #        |                        <- Ptr
                            |----------------|         +------------+
                            | ProcSym | Type |         | ReturnVar  |
                            |----------------|         |------------|

*)

PROCEDURE BuildHighFromUnbounded ;
VAR
   Dim,
   NoOfParam,
   ReturnVar: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   Assert(NoOfParam=1) ;
   ReturnVar := MakeTemporary(RightValue) ;
   PutVar(ReturnVar, Cardinal) ;
   Dim := OperandD(1) ;
   INC(Dim) ;
   IF Dim>1
   THEN
      GenHigh(ReturnVar, Dim, OperandA(1))
   ELSE
      GenHigh(ReturnVar, Dim, OperandT(1))
   END ;
   PopN(2) ;
   PushTF(ReturnVar, GetType(ReturnVar))
END BuildHighFromUnbounded ;


(*
   GetQualidentImport - returns the symbol as if it were qualified from, module.n.
                        This is used to reference runtime support procedures and an
                        error is generated if the symbol cannot be obtained.
*)

PROCEDURE GetQualidentImport (n: Name; module: Name) : CARDINAL ;
VAR
   ProcSym,
   ModSym : CARDINAL ;
BEGIN
   ModSym := MakeDefinitionSource(module) ;
   IF ModSym=NulSym
   THEN
      WriteFormat2('module %a cannot be found and is needed to import %a', module, n) ;
      FlushErrors ;
      RETURN( NulSym )
   END ;
   Assert(IsDefImp(ModSym)) ;
   IF (GetExported(ModSym, n)=NulSym) OR IsUnknown(GetExported(ModSym, n))
   THEN
      WriteFormat2('module %a does not export procedure %a which is a necessary component of the runtime system, hint check the path and library/language variant', module, n) ;
      FlushErrors ;
      RETURN( NulSym )
   END ;
   RETURN( GetExported(MakeDefinitionSource(module), n) )
END GetQualidentImport ;


(*
   BuildLengthFunction - builds the inline standard function LENGTH.

                         The Stack:


                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |  
                         |----------------|
                         | Param 1        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|

*)

PROCEDURE BuildLengthFunction ;
VAR
   s        : String ;
   l,
   ProcSym,
   Type,
   NoOfParam,
   Param,
   ReturnVar: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   Param := OperandT(1) ;
   (* Restore stack to origional form *)
   PushT(NoOfParam) ;
   Type  := GetType(Param) ;  (* get the type from the symbol, not the stack *)
   IF NoOfParam#1
   THEN
      WriteFormat0('base procedure LENGTH expects 1 parameter')
   END ;
   IF NoOfParam>=1
   THEN
      IF IsConst(Param) AND (GetType(Param)=Char)
      THEN
         PopT(NoOfParam) ;
         PopN(NoOfParam+1) ;
         ReturnVar := MakeConstLit(MakeKey('1')) ;
         PushT(ReturnVar)
      ELSIF IsConstString(Param)
      THEN
         PopT(NoOfParam) ;
         l := GetStringLength(OperandT(1)) ;
         s := Sprintf1(Mark(InitString("%d")), l) ;
         ReturnVar := MakeConstLit(makekey(string(s))) ;
         s := KillString(s) ;
         PopN(NoOfParam+1) ;
         PushT(ReturnVar)
      ELSE
         ProcSym := GetQualidentImport(MakeKey('Length'), MakeKey('M2RTS')) ;
         IF (ProcSym#NulSym) AND IsProcedure(ProcSym)
         THEN
            PopT(NoOfParam) ;
            ReturnVar := MakeTemporary(AreConstant(IsConst(OperandT(1)))) ;
            PutVar(ReturnVar, Cardinal) ;
            GenQuad(StandardFunctionOp, ReturnVar, ProcSym, OperandT(1)) ;
            PopN(NoOfParam+1) ;
            PushT(ReturnVar)
         ELSE
            PopT(NoOfParam) ;
            PopN(NoOfParam+1) ;
            PushT(MakeConstLit(MakeKey('0'))) ;
            WriteFormat0('no procedure Length found for substitution to the standard function LENGTH which is required to calculate non constant string lengths')
         END
      END
   ELSE
      (* NoOfParam is _very_ wrong, we flush all outstanding errors *)
      FlushErrors
   END
END BuildLengthFunction ;


(*
   BuildOddFunction - builds the pseudo procedure call ODD.
                      This procedure is actually a "macro" for
                      ORD(x) --> VAL(BOOLEAN, x MOD 2)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildOddFunction ;
VAR
   NoOfParam,
   Res, Var : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         PopN(NoOfParam+1) ;
         (*
            Build macro: VAL(BOOLEAN, (x MOD 2))
         *)

         (* compute (x MOD 2) *)
         PushTF(Var, GetType(Var)) ;
         PushT(ModTok) ;
         PushTF(MakeConstLit(MakeKey('2')), ZType) ;
         BuildBinaryOp ;
         PopT(Res) ;

         (* compute IF ...=0 *)
         PushT(Res) ;
         PushT(EqualTok) ;
         PushT(MakeConstLit(MakeKey('0'))) ;
         BuildRelOp ;
         BuildThenIf ;
         
         Res := MakeTemporary(RightValue) ;
         PutVar(Res, Boolean) ;

         PushT(Res) ;
         PushT(False) ;
         BuildAssignment ;
         BuildElse ;
         PushT(Res) ;
         PushT(True) ;
         BuildAssignment ;
         BuildEndIf ;

         PushT(Res)
      ELSE
         WriteFormat0('argument to ODD must be a variable or constant')
      END
   ELSE
      WriteFormat0('the pseudo procedure ODD only has one parameter')
   END
END BuildOddFunction ;


(*
   BuildAbsFunction - builds a call to the standard function ABS.

                      We cannot implement it as a macro or inline an
                      IF THEN statement as the IF THEN ELSE requires
                      we write the value to the same variable (or constant)
                      twice. The macro implementation will fail as
                      the compiler maybe building a function
                      call and expecting a ReturnVar on the stack.
                      The only method to implement this is to pass it to the
                      gcc backend.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildAbsFunction ;
VAR
   NoOfParam,
   ProcSym,
   Res, Var : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         ProcSym := OperandT(NoOfParam+1) ;
         PopN(NoOfParam+1) ;

         Res := MakeTemporary(AreConstant(IsConst(Var))) ;
         PutVar(Res, GetType(Var)) ;

         GenQuad(StandardFunctionOp, Res, ProcSym, Var) ;
         PushTF(Res, GetType(Var))
      ELSE
         WriteFormat0('argument to ABS must be a variable or constant')
      END
   ELSE
      WriteFormat0('the pseudo procedure ABS only has one parameter')
   END
END BuildAbsFunction ;


(*
   BuildCapFunction - builds the pseudo procedure call CAP.
                      We generate a the following quad:


                      StandardFunctionOp  ReturnVal  Cap  Param1

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam = 1  |
                      |----------------|
                      | Param 1        |
                      |----------------|         +-------------+
                      | ProcSym | Type |         | ReturnVal   |
                      |----------------|         |-------------|
*)

PROCEDURE BuildCapFunction ;
VAR
   NoOfParam,
   ProcSym,
   Res, Var : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         ProcSym := OperandT(NoOfParam+1) ;
         PopN(NoOfParam+1) ;

         Res := MakeTemporary(AreConstant(IsConst(Var))) ;
         PutVar(Res, Char) ;
         GenQuad(StandardFunctionOp, Res, ProcSym, Var) ;
         PushTF(Res, Char)
      ELSE
         WriteFormat0('argument to CAP must be a variable or constant')
      END
   ELSE
      WriteFormat0('the pseudo procedure CAP only has one parameter')
   END
END BuildCapFunction ;


(*
   BuildChrFunction - builds the pseudo procedure call CHR.
                      This procedure is actually a "macro" for
                      CHR(x) --> CONVERT(CHAR, x)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildChrFunction ;
VAR
   NoOfParam,
   Var      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         PopN(NoOfParam+1) ;
         (*
            Build macro: CONVERT( CHAR, Var )
         *)
         PushTF(Convert, NulSym) ;
         PushT(Char) ;
         PushT(Var) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction
      ELSE
         WriteFormat0('argument to CHR must be a variable or constant')
      END
   ELSE
      WriteFormat0('the pseudo procedure CHR only has one parameter')
   END
END BuildChrFunction ;


(*
   BuildOrdFunction - builds the pseudo procedure call ORD.
                      This procedure is actually a "macro" for
                      ORD(x) --> CONVERT(GetType(sym), x)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildOrdFunction (Sym: CARDINAL) ;
VAR
   n        : Name ;
   NoOfParam,
   Type, Var: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         Type := GetType(Sym) ;
         PopN(NoOfParam+1) ;
         (*
            Build macro: CONVERT( CARDINAL, Var )
         *)
         PushTF(Convert, NulSym) ;
         PushT(Type) ;
         PushT(Var) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction
      ELSE
         n := GetSymName(Sym) ;
         WriteFormat1('argument to %a must be a variable or constant', n)
      END
   ELSE
      n := GetSymName(Sym) ;
      WriteFormat1('the pseudo procedure %a only has one parameter', n)
   END
END BuildOrdFunction ;


(*
   BuildIntFunction - builds the pseudo procedure call INT.
                      This procedure is actually a "macro" for
                      INT(x) --> CONVERT(INTEGER, x)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildIntFunction (Sym: CARDINAL) ;
VAR
   n        : Name ;
   NoOfParam,
   Type, Var: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         Type := GetType(Sym) ;  (* return type of function *)
         PopN(NoOfParam+1) ;
         (*
            Build macro: CONVERT( CARDINAL, Var )
         *)
         PushTF(Convert, NulSym) ;
         PushT(Type) ;
         PushT(Var) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction
      ELSE
         n := GetSymName(Sym) ;
         WriteFormat1('argument to %a must be a variable or constant', n)
      END
   ELSE
      n := GetSymName(Sym) ;
      WriteFormat1('the pseudo procedure %a only has one parameter', n)
   END
END BuildIntFunction ;


(*
   BuildMakeAdrFunction - builds the pseudo procedure call MAKEADR.

                          The Stack:


                          Entry                      Exit

                   Ptr ->
                          +----------------+
                          | NoOfParam      |
                          |----------------|
                          | Param 1        |
                          |----------------|
                          | Param 2        |
                          |----------------|
                          .                .
                          .                .
                          .                .
                          |----------------|
                          | Param #        |
                          |----------------|
                          | ProcSym | Type |         Empty
                          |----------------|
*)

PROCEDURE BuildMakeAdrFunction ;
VAR
   AreConst      : BOOLEAN ;
   i, pi,
   NoOfParameters: CARDINAL ;
   ReturnVar     : CARDINAL ;
BEGIN
   PopT(NoOfParameters) ;
   IF NoOfParameters>0
   THEN
      GenQuad(ParamOp, 0, MakeAdr, MakeAdr) ;
      IF PushParametersLeftToRight
      THEN
         i := NoOfParameters ;
         (* stack index referencing stacked parameter, i *)
         pi := 1 ;
         WHILE i>0 DO
            GenQuad(ParamOp, i, MakeAdr, OperandT(pi)) ;
            DEC(i) ;
            INC(pi)
         END
      ELSE
         i := 1 ;
         (* stack index referencing stacked parameter, i *)
         pi := NoOfParameters ;
         WHILE i<=NoOfParameters DO
            GenQuad(ParamOp, i, MakeAdr, OperandT(pi)) ;
            INC(i) ;
            DEC(pi)
         END
      END ;
      AreConst := TRUE ;
      i := 1 ;
      WHILE i<=NoOfParameters DO
         IF IsVar(OperandT(i))
         THEN
            AreConst := FALSE ;
         ELSIF NOT IsConst(OperandT(i))
         THEN
            WriteFormat1('problem in argument (%d) for MAKEADR, all arguments to MAKEADR must be either variables or constants', i)
         END ;
         INC(i)
      END ;
      (* ReturnVar - will have the type of the procedure *)
      ReturnVar := MakeTemporary(AreConstant(AreConst)) ;
      PutVar(ReturnVar, GetType(MakeAdr)) ;
      GenQuad(FunctValueOp, ReturnVar, NulSym, MakeAdr) ;
      PopN(NoOfParameters+1) ;
      PushTF(ReturnVar, GetType(MakeAdr))
   ELSE
      WriteFormat0('the pseudo procedure MAKEADR requires at least one parameter') ;
      PopN(1)
   END
END BuildMakeAdrFunction ;


(*
   BuildShiftFunction - builds the pseudo procedure call SHIFT.

                        PROCEDURE SHIFT (val: <any type>;
                                         num: INTEGER): <any type> ;

                       "Returns a bit sequence obtained from val by
                        shifting up or down (left or right) by the
                        absolute value of num, introducing
                        zeros as necessary.  The direction is down if
                        the sign of num is negative, otherwise the
                        direction is up."

                        The Stack:

                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |                        <- Ptr
                        |----------------|         +------------+
                        | ProcSym | Type |         | ReturnVar  |
                        |----------------|         |------------|
*)

PROCEDURE BuildShiftFunction ;
VAR
   ReturnVar,
   NoOfParam,
   OperandSym,
   TempSym,
   VarSym    : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT(2) ;
      OperandSym := OperandT(1) ;
      PopN(NoOfParam+1) ;
      IF (GetType(VarSym)#NulSym) AND IsSet(SkipType(GetType(VarSym)))
      THEN
         TempSym := DereferenceLValue(OperandSym) ;
         BuildRange(InitShiftCheck(VarSym, TempSym)) ;
         ReturnVar := MakeTemporary(RightValue) ;
         PutVar(ReturnVar, GetType(VarSym)) ;
         GenQuad(LogicalShiftOp, ReturnVar, VarSym, TempSym) ;
         PushTF(ReturnVar, GetType(VarSym))
      ELSE
         WriteFormat0('SYSTEM procedure SHIFT expects a constant or variable which has a type of SET as its first parameter') ;
         PushTF(MakeConstLit(MakeKey('0')), Cardinal)
      END
   ELSE
      WriteFormat0('SYSTEM procedure SHIFT expects 2 parameters') ;
      PopN(NoOfParam+1) ;
      PushTF(MakeConstLit(MakeKey('0')), Cardinal)
   END
END BuildShiftFunction ;


(*
   BuildRotateFunction - builds the pseudo procedure call ROTATE.

                         PROCEDURE ROTATE (val: <any type>;
                                           num: INTEGER): <any type> ;

                        "Returns a bit sequence obtained from val
                         by rotating up or down (left or right) by
                         the absolute value of num.  The direction is
                         down if the sign of num is negative, otherwise
                         the direction is up."

                         The Stack:

                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |
                         |----------------|
                         | Param 2        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|
*)

PROCEDURE BuildRotateFunction ;
VAR
   ReturnVar,
   NoOfParam,
   TempSym,
   OperandSym,
   VarSym    : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT(2) ;
      OperandSym := OperandT(1) ;
      PopN(NoOfParam+1) ;
      IF (GetType(VarSym)#NulSym) AND IsSet(SkipType(GetType(VarSym)))
      THEN
         TempSym := DereferenceLValue(OperandSym) ;
         BuildRange(InitRotateCheck(VarSym, TempSym)) ;
         ReturnVar := MakeTemporary(RightValue) ;
         PutVar(ReturnVar, GetType(VarSym)) ;
         GenQuad(LogicalRotateOp, ReturnVar, VarSym, TempSym) ;
         PushTF(ReturnVar, GetType(VarSym))
      ELSE
         WriteFormat0('SYSTEM procedure ROTATE expects a constant or variable which has a type of SET as its first parameter') ;
         PushTF(MakeConstLit(MakeKey('0')), Cardinal)
      END
   ELSE
      WriteFormat0('SYSTEM procedure ROTATE expects 2 parameters') ;
      PopN(NoOfParam+1) ;
      PushTF(MakeConstLit(MakeKey('0')), Cardinal)
   END
END BuildRotateFunction ;


(*
   BuildValFunction - builds the pseudo procedure call VAL.
                      This procedure is actually a "macro" for
                      VAL(Type, x) --> CONVERT(Type, x)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildValFunction ;
VAR
   n        : Name ;
   NoOfParam,
   Var, Type: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      Type := OperandT(2) ;
      Var := OperandT(1) ;
      IF IsUnknown(Type)
      THEN
         n := GetSymName(Type) ;
         WriteFormat1('undeclared type found in VAL (%a)', n)
      ELSIF (IsSet(Type) OR IsEnumeration(Type) OR IsSubrange(Type) OR
             IsType(Type) OR IsPointer(Type) OR IsProcType(Type)) AND
              (IsVar(Var) OR IsConst(Var) OR IsProcedure(Var))
      THEN
         PopN(NoOfParam+1) ;
         (*
            Build macro: CONVERT( Type, Var )
         *)
         PushTF(Convert, NulSym) ;
         PushT(Type) ;
         PushT(Var) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction
      ELSE
         WriteFormat0('the builtin type conversion procedure function VAL has the following formal parameter declaration VAL(type, variable or procedure or constant)')
      END
   ELSE
      WriteFormat0('the pseudo procedure VAL has 2 parameters, a Type and Variable')
   END
END BuildValFunction ;


(*
   BuildCastFunction - builds the pseudo procedure call CAST.
                       This procedure is actually a "macro" for
                       CAST(Type, x) --> Type(x)
                       However we cannot push tokens back onto the input stack
                       because the compiler is currently building a function
                       call and expecting a ReturnVar on the stack.
                       Hence we manipulate the stack and call
                       BuildConvertFunction.

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |
                       |----------------|
                       | ProcSym | Type |         Empty
                       |----------------|
*)

PROCEDURE BuildCastFunction ;
VAR
   n        : Name ;
   ReturnVar,
   NoOfParam,
   Var, Type: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      Type := OperandT(2) ;
      Var := OperandT(1) ;
      IF IsUnknown(Type)
      THEN
         n := GetSymName(Type) ;
         WriteFormat1('undeclared type found in CAST (%a)', n)
      ELSIF IsSet(Type) OR IsEnumeration(Type) OR IsSubrange(Type) OR IsType(Type) OR
            IsPointer(Type) OR IsArray(Type) OR IsProcType(Type)
      THEN
         IF IsConst(Var)
         THEN
            PopN(NoOfParam+1) ;
            (*
               Build macro: Type( Var )
            *)
            PushTF(Type, NulSym) ;
            PushT(Var) ;
            PushT(1) ;          (* one parameter *)
            BuildTypeCoercion
         ELSIF IsVar(Var) OR IsProcedure(Var)
         THEN
            PopN(NoOfParam+1) ;
            ReturnVar := MakeTemporary(RightValue) ;
            PutVar(ReturnVar, Type) ;
            GenQuad(CastOp, ReturnVar, Type, Var) ;
            PushTF(ReturnVar, Type)
         ELSE
            WriteFormat0('second parameter to CAST must either be a variable or procedure or a constant, the formal parameters to cast are CAST(type, variable or constant or procedure)')
         END
      ELSE
         WriteFormat0('formal paramater declaration of the builtin procedure function CAST is CAST(type, variable or procedure or constant)')
      END
   ELSE
      WriteFormat0('the pseudo procedure CAST has 2 parameters, a type and expression')
   END
END BuildCastFunction ;


(*
   BuildConvertFunction - builds the pseudo function CONVERT.
                          CONVERT( Type, Variable ) ;

                          The Stack:


                          Entry                      Exit

                   Ptr ->
                          +----------------+
                          | NoOfParam      |  
                          |----------------|
                          | Param 1        |  
                          |----------------|
                          | Param 2        |  
                          |----------------|
                          .                .  
                          .                .  
                          .                .  
                          |----------------|
                          | Param #        |                                 <- Ptr
                          |----------------|         +---------------------+
                          | ProcSym | Type |         | ReturnVar | Param1  |
                          |----------------|         |---------------------|

                          Quadruples:

                          ConvertOp  ReturnVar  Param1  Param2

                          Converts variable Param2 into a variable Param1
                          with a type Param1.
*)

PROCEDURE BuildConvertFunction ;
VAR
   n         : Name ;
   t,
   Var, Type,
   NoOfParam,
   ProcSym,
   IntegerVar,
   ReturnVar : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   IF NoOfParam#2
   THEN
      WriteFormat0('builtin procedure function CONVERT expects 2 parameters')
   ELSE
      Type := OperandT(2) ;
      Var := OperandT(1) ;
      IF IsUnknown(Type)
      THEN
         n := GetSymName(Type) ;
         WriteFormat1('undeclared type found in CONVERT (%a)', n)
      ELSIF IsUnknown(Var)
      THEN
         n := GetSymName(Var) ;
         WriteFormat1('undeclared variable found in CONVERT (%a)', n)
      ELSIF (IsSet(Type) OR IsEnumeration(Type) OR IsSubrange(Type) OR
             IsType(Type) OR IsPointer(Type) OR IsProcType(Type)) AND
            (IsVar(Var) OR IsConst(Var) OR IsProcedure(Var))
      THEN
         PopN(NoOfParam+1) ;    (* destroy arguments to this function *)
         (* firstly dereference Var *)
         IF GetMode(Var)=LeftValue
         THEN
            t := MakeTemporary(RightValue) ;
            PutVar(t, GetType(Var)) ;
            CheckPointerThroughNil(Var) ;
            doIndrX(t, Var) ;
            Var := t
         END ;

         ReturnVar := MakeTemporary(AreConstant(IsConst(Var))) ;
         PutVar(ReturnVar, Type) ;
         GenQuad(ConvertOp, ReturnVar, Type, Var) ;
         PushTF(ReturnVar, Type)
      ELSE
         WriteFormat0('builtin procedure function CONVERT is declared with the following formal parameters CONVERT(type, constant or procedure or variable)')
      END
   END
END BuildConvertFunction ;


(*
   CheckBaseTypeValue - checks to see whether the value, min, really exists.
*)

PROCEDURE CheckBaseTypeValue (type: CARDINAL;
                              min: CARDINAL;
                              n: ARRAY OF CHAR) : CARDINAL ;
VAR
   n1, n2: Name ;
BEGIN
   IF (type=Real) OR (type=LongReal) OR (type=ShortReal)
   THEN
      PushValue(min) ;
      IF NOT IsValueAndTreeKnown()
      THEN
         n1 := MakeKey(n) ;
         n2 := GetSymName(type) ;
         WriteFormat2('%a(%a) cannot be calculated at compile time for the target architecture', n1, n2) ;
         RETURN( MakeConstLit(MakeKey('1.0')) )
      END
   END ;
   RETURN( min )
END CheckBaseTypeValue ;


(*
   GetTypeMin - 
*)

PROCEDURE GetTypeMin (type: CARDINAL) : CARDINAL ;
VAR
   n       : Name ;
   min, max: CARDINAL ;
BEGIN
   IF IsSubrange(type)
   THEN
      min := MakeTemporary(ImmediateValue) ;
      PutVar(min, type) ;
      GenQuad(SubrangeLowOp, min, NulSym, type) ;
      RETURN( min )
   ELSIF IsSet(SkipType(type))
   THEN
      RETURN( GetTypeMin(GetType(SkipType(type))) )
   ELSIF IsBaseType(type) OR IsEnumeration(type)
   THEN
      GetBaseTypeMinMax(type, min, max) ;
      min := CheckBaseTypeValue(type, min, 'MIN') ;
      RETURN( min )
   ELSIF IsSystemType(type)
   THEN
      GetSystemTypeMinMax(type, min, max) ;
      RETURN( min )
   ELSIF GetType(type)=NulSym
   THEN
      n := GetSymName(type) ;
      WriteFormat1('unable to obtain the MIN value for type %a', n)
   ELSE
      RETURN( GetTypeMin(GetType(type)) )
   END
END GetTypeMin ;


(*
   GetTypeMax - 
*)

PROCEDURE GetTypeMax (type: CARDINAL) : CARDINAL ;
VAR
   n       : Name ;
   min, max: CARDINAL ;
BEGIN
   IF IsSubrange(type)
   THEN
      max := MakeTemporary(ImmediateValue) ;
      PutVar(max, type) ;
      GenQuad(SubrangeHighOp, max, NulSym, type) ;
      RETURN( max )
   ELSIF IsSet(SkipType(type))
   THEN
      RETURN( GetTypeMax(GetType(SkipType(type))) )
   ELSIF IsBaseType(type) OR IsEnumeration(type)
   THEN
      GetBaseTypeMinMax(type, min, max) ;
      max := CheckBaseTypeValue(type, max, 'MAX') ;
      RETURN( max )
   ELSIF IsSystemType(type)
   THEN
      GetSystemTypeMinMax(type, min, max) ;
      RETURN( max )
   ELSIF GetType(type)=NulSym
   THEN
      n := GetSymName(type) ;
      WriteFormat1('unable to obtain the MAX value for type %a', n)
   ELSE
      RETURN( GetTypeMax(GetType(type)) )
   END
END GetTypeMax ;


(*
   BuildMinFunction - builds the pseudo function call Min.

                      The Stack:

                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam=1    |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildMinFunction ;
VAR
   min,
   NoOfParam,
   Var,
   ProcSym,
   Ptr      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      PopN(NoOfParam+1) ;    (* destroy arguments to this function *)
      IF IsAModula2Type(Var)
      THEN
         min := GetTypeMin(Var) ;
         PushTF(min, GetType(min))
      ELSIF IsVar(Var)
      THEN
         min := GetTypeMin(GetType(Var)) ;
         PushTF(min, GetType(Var))
      ELSE
         WriteFormat0('parameter to MIN must be a type or a variable') ;
         PushT(MakeConstLit(MakeKey('0')))   (* put a legal value on the stack *)
      END
   ELSE
      WriteFormat0('the pseudo function MIN only has one parameter') ;
      PushT(MakeConstLit(MakeKey('0')))   (* put a legal value on the stack *)
   END
END BuildMinFunction ;


(*
   BuildMaxFunction - builds the pseudo function call Max.

                      The Stack:

                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam=1    |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildMaxFunction ;
VAR
   max,
   NoOfParam,
   Var,
   ProcSym,
   Ptr      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      PopN(NoOfParam+1) ;    (* destroy arguments to this function *)
      IF IsAModula2Type(Var)
      THEN
         max := GetTypeMax(Var) ;
         PushTF(max, GetType(max))
      ELSIF IsVar(Var)
      THEN
         max := GetTypeMax(GetType(Var)) ;
         PushTF(max, GetType(Var))
      ELSE
         WriteFormat0('parameter to MAX must be a type or a variable') ;
         PushT(MakeConstLit(MakeKey('0')))   (* put a legal value on the stack *)
      END
   ELSE
      WriteFormat0('the pseudo function MAX only has one parameter') ;
      PushT(MakeConstLit(MakeKey('0')))   (* put a legal value on the stack *)
   END
END BuildMaxFunction ;


(*
   BuildTruncFunction - builds the pseudo procedure call TRUNC.
                        This procedure is actually a "macro" for
                        TRUNC(x) --> CONVERT(INTEGER, x)
                        However we cannot push tokens back onto the input stack
                        because the compiler is currently building a function
                        call and expecting a ReturnVar on the stack.
                        Hence we manipulate the stack and call
                        BuildConvertFunction.

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildTruncFunction (Sym: CARDINAL) ;
VAR
   NoOfParam: CARDINAL ;
   Type,
   Var,
   ReturnVar,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   Assert(IsTrunc(OperandT(NoOfParam+1))) ;
   IF NoOfParam=1
   THEN
      ProcSym := RequestSym(MakeKey('CONVERT')) ;
      IF (ProcSym#NulSym) AND IsProcedure(ProcSym)
      THEN
         Var := OperandT(1) ;
         Type := GetType(Sym) ;
         PopN(NoOfParam+1) ;    (* destroy arguments to this function *)
         IF IsVar(Var) OR IsConst(Var)
         THEN
            IF IsRealType(GetType(Var))
            THEN
               (*
                  Build macro: CONVERT( INTEGER, Var )
               *)
               PushTF(ProcSym, NulSym) ;
               PushT(Type) ;
               PushT(Var) ;
               PushT(2) ;          (* Two parameters *)
               BuildConvertFunction
            ELSE
               WriteFormat0('argument to TRUNC must be a float point type') ;
               PushTF(MakeConstLit(MakeKey('0')), Type)
            END
         ELSE
            WriteFormat0('argument to TRUNC must be a variable or constant') ;
            PushTF(MakeConstLit(MakeKey('0')), Type)
         END
      ELSE
         InternalError('CONVERT procedure not found for TRUNC substitution', __FILE__, __LINE__)
      END
   ELSE
      WriteFormat0('the pseudo procedure TRUNC only has one parameter')
   END
END BuildTruncFunction ;


(*
   BuildFloatFunction - builds the pseudo procedure call FLOAT.
                        This procedure is actually a "macro" for
                        FLOAT(x) --> CONVERT(REAL, x)
                        However we cannot push tokens back onto the input stack
                        because the compiler is currently building a function
                        call and expecting a ReturnVar on the stack.
                        Hence we manipulate the stack and call
                        BuildConvertFunction.

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildFloatFunction (Sym: CARDINAL) ;
VAR
   NoOfParam: CARDINAL ;
   Type,
   Var,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      ProcSym := RequestSym(MakeKey('CONVERT')) ;
      IF (ProcSym#NulSym) AND IsProcedure(ProcSym)
      THEN
         Var := OperandT(1) ;
         IF IsVar(Var) OR IsConst(Var)
         THEN
            Type := GetType(Sym) ;
            PopN(NoOfParam+1) ;    (* destroy arguments to this function *)
            (*
               Build macro: CONVERT( REAL, Var )
            *)
            PushTF(ProcSym, NulSym) ;
            PushT(Type) ;
            PushT(Var) ;
            PushT(2) ;          (* Two parameters *)
            BuildConvertFunction
         ELSE
            WriteFormat0('argument to FLOAT must be a variable or constant')
         END
      ELSE
         InternalError('CONVERT procedure not found for FLOAT substitution', __FILE__, __LINE__)
      END
   ELSE
      WriteFormat0('the pseudo procedure FLOAT only has one parameter')
   END
END BuildFloatFunction ;


(*
   BuildReFunction - builds the pseudo procedure call RE.

                     The Stack:


                         Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildReFunction ;
VAR
   NoOfParam: CARDINAL ;
   Type,
   ReturnVar,
   Var,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         ReturnVar := MakeTemporary(AreConstant(IsConst(Var))) ;
         PutVar(ReturnVar, ComplexToScalar(GetType(Var))) ;
         GenQuad(StandardFunctionOp, ReturnVar, Re, Var) ;
         PopN(NoOfParam+1) ;  (* destroy arguments to this function *)
         PushTF(ReturnVar, GetType(ReturnVar))
      ELSE
         PopN(NoOfParam+1) ;  (* destroy arguments to this function *)
         PushTF(MakeConstLit(MakeKey('1.0')), RType)
      END
   ELSE
      WriteFormat0('the pseudo procedure RE only has one parameter')
   END
END BuildReFunction ;


(*
   BuildImFunction - builds the pseudo procedure call IM.

                     The Stack:


                         Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildImFunction ;
VAR
   NoOfParam: CARDINAL ;
   Type,
   ReturnVar,
   Var,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         ReturnVar := MakeTemporary(AreConstant(IsConst(Var))) ;
         PutVar(ReturnVar, ComplexToScalar(GetType(Var))) ;
         GenQuad(StandardFunctionOp, ReturnVar, Im, Var) ;
         PopN(NoOfParam+1) ;  (* destroy arguments to this function *)
         PushTF(ReturnVar, GetType(ReturnVar))
      ELSE
         PopN(NoOfParam+1) ;  (* destroy arguments to this function *)
         PushTF( MakeConstLit(MakeKey('1.0')), RType )
      END
   ELSE
      WriteFormat0('the pseudo procedure IM only has one parameter')
   END
END BuildImFunction ;


(*
   BuildCmplxFunction - builds the pseudo procedure call CMPLX.

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildCmplxFunction ;
VAR
   NoOfParam: CARDINAL ;
   ReturnVar,
   Type,
   l, r,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      l := OperandT(2) ;
      r := OperandT(1) ;
      IF (IsVar(l) OR IsConst(l)) AND
         (IsVar(r) OR IsConst(r))
      THEN
         CheckExpressionCompatible(GetType(l), GetType(r)) ;
         ReturnVar := MakeTemporary(AreConstant(IsConst(l) AND IsConst(r))) ;
         PutVar(ReturnVar, GetCmplxReturnType(SkipType(GetType(l)), SkipType(GetType(r)))) ;
         GenQuad(StandardFunctionOp, ReturnVar, Cmplx, Make2Tuple(l, r)) ;
         PopN(NoOfParam+1) ;   (* destroy arguments to this function *)
         PushTF(ReturnVar, GetType(ReturnVar))
      ELSE
         WriteFormat0('the pseudo procedure CMPLX requires two parameters') ;
         PopN(NoOfParam+1) ;  (* destroy arguments to this function *)
         PushTF( MakeConstLit(MakeKey('1.0')), RType )
      END
   ELSE
      WriteFormat0('the pseudo procedure CMPLX requires two parameters')
   END
END BuildCmplxFunction ;


(*
   BuildAdrFunction - builds the pseudo function ADR
                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |  
                      |----------------|
                      | Param 1        |  
                      |----------------|
                      | Param 2        |  
                      |----------------|
                      .                .  
                      .                .  
                      .                .  
                      |----------------|
                      | Param #        |                        <- Ptr
                      |----------------|         +------------+
                      | ProcSym | Type |         | ReturnVar  |
                      |----------------|         |------------|

*)

PROCEDURE BuildAdrFunction ;
VAR
   t,
   Array,
   UnboundedSym,
   Field,
   NoOfParam,
   ProcSym,
   ReturnVar,
   Type, rw    : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   IF NoOfParam#1
   THEN
      WriteFormat0('SYSTEM procedure ADR expects 1 parameter') ;
      PopN(NoOfParam+1) ;    (* destroy the arguments and function *)
      PushTF(Nil, Address)
   ELSIF IsConstString(OperandT(1))
   THEN
      ReturnVar := MakeLeftValue(OperandT(1), RightValue, GetType(ProcSym)) ;
      PopN(NoOfParam+1) ;    (* destroy the arguments and function *)
      PushTF(ReturnVar, GetType(ReturnVar))
   ELSIF (NOT IsVar(OperandT(1))) AND (NOT IsProcedure(OperandT(1)))
   THEN
      WriteFormat0('SYSTEM procedure ADR expects a variable, procedure or a constant string as its parameter') ;
      PopN(NoOfParam+1) ;    (* destroy the arguments and function *)
      PushTF(Nil, Address)
   ELSIF IsProcedure(OperandT(1))
   THEN
      ReturnVar := MakeLeftValue(OperandT(1), RightValue, GetType(ProcSym)) ;
      PopN(NoOfParam+1) ;    (* destroy the arguments and function *)
      PushTF(ReturnVar, GetType(ReturnVar))
   ELSE
      Type := GetType(OperandT(1)) ;
      MarkArrayWritten(OperandT(1)) ;
      MarkArrayWritten(OperandA(1)) ;
      IF IsUnbounded(Type)
      THEN
         (* we will reference the address field of the unbounded structure *)
         UnboundedSym := OperandT(1) ;
         rw := OperandRW(1) ;
         PushTFrw(UnboundedSym, GetType(UnboundedSym), rw) ;
         Field := GetUnboundedAddressOffset(GetType(UnboundedSym)) ;
         PushTF(Field, GetType(Field)) ;
         PushT(1) ;
         BuildDesignatorRecord ;
         PopTrw(ReturnVar, rw) ;
         Assert(GetMode(ReturnVar)=LeftValue) ;
         t := MakeTemporary(RightValue) ;
         PutVar(t, GetType(ProcSym)) ;
         doIndrX(t, ReturnVar) ;  (* was       GenQuad(IndrXOp, t, GetType(ProcSym), ReturnVar) ; *)
         ReturnVar := t
      ELSE
         ReturnVar := MakeTemporary(RightValue) ;
         PutVar(ReturnVar, GetType(ProcSym)) ;
         IF GetMode(OperandT(1))=LeftValue
         THEN
            PutVar(ReturnVar, GetType(ProcSym)) ;
            GenQuad(ConvertOp, ReturnVar, GetType(ProcSym), OperandT(1))
         ELSE
            GenQuad(AddrOp, ReturnVar, NulSym, OperandT(1))
         END ;
         rw := OperandMergeRW(1) ;
         Assert(IsLegal(rw))
      END ;
      PopN(NoOfParam+1) ;    (* destroy the arguments and function *)
      PushTFrw(ReturnVar, GetType(ReturnVar), rw)
   END
END BuildAdrFunction ;


(*
   BuildSizeFunction - builds the pseudo function SIZE
                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |  
                       |----------------|
                       | Param 1        |  
                       |----------------|
                       | Param 2        |  
                       |----------------|
                       .                .  
                       .                .  
                       .                .  
                       |----------------|
                       | Param #        |                        <- Ptr
                       |----------------|         +------------+
                       | ProcSym | Type |         | ReturnVar  |
                       |----------------|         |------------|
*)

PROCEDURE BuildSizeFunction ;
VAR
   n           : Name ;
   dim         : CARDINAL ;
   Type,
   UnboundedSym,
   Field,
   NoOfParam,
   ProcSym, ti,
   ReturnVar   : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   IF NoOfParam#1
   THEN
      WriteFormat0('SYSTEM procedure SIZE expects 1 parameter') ;
      ReturnVar := MakeConstLit(MakeKey('0'))
   ELSIF IsAModula2Type(OperandT(1))
   THEN
      BuildSizeCheckEnd(ProcSym) ;   (* quadruple generation now on *)
      ReturnVar := MakeTemporary(ImmediateValue) ;
      GenQuad(SizeOp, ReturnVar, NulSym, OperandT(1))
   ELSIF IsVar(OperandT(1))
   THEN
      BuildSizeCheckEnd(ProcSym) ;   (* quadruple generation now on *)
      Type := GetType(OperandT(1)) ;
      IF IsUnbounded(Type)
      THEN
         (* eg. SIZE(a)  ; where a is unbounded dereference HIGH and multiply by the TYPE *)
         dim := OperandD(1) ;
         IF dim=0
         THEN
            ReturnVar := calculateMultipicand(OperandT(1), Type, dim)
         ELSE
            ReturnVar := calculateMultipicand(OperandA(1), Type, dim)
         END
      ELSE
         ReturnVar := MakeTemporary(ImmediateValue) ;
         IF Type=NulSym
         THEN
            n := GetSymName(OperandT(1)) ;
            WriteFormat1('cannot get the type and size of variable (%a)', n)
         END ;
         GenQuad(SizeOp, ReturnVar, NulSym, Type)
      END
   ELSE
      WriteFormat0('SYSTEM procedure SIZE expects a variable as its parameter') ;
      ReturnVar := MakeConstLit(MakeKey('0'))
   END ;
   PopN(NoOfParam+1) ;       (* destroy the arguments and function *)
   PushTF(ReturnVar, GetType(ProcSym))
END BuildSizeFunction ;


(*
   BuildTSizeFunction - builds the pseudo function TSIZE
                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |  
                        |----------------|
                        | Param 1        |  
                        |----------------|
                        | Param 2        |  
                        |----------------|
                        .                .  
                        .                .  
                        .                .  
                        |----------------|
                        | Param #        |                        <- Ptr
                        |----------------|         +------------+
                        | ProcSym | Type |         | ReturnVar  |
                        |----------------|         |------------|

*)

PROCEDURE BuildTSizeFunction ;
VAR
   t, f        : CARDINAL ;
   i, NoOfParam: CARDINAL ;
   ti, tj,
   ProcSym,
   Record,
   ReturnVar   : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   IF NoOfParam=1
   THEN
      IF IsAModula2Type(OperandT(1))
      THEN
         ReturnVar := MakeTemporary(ImmediateValue) ;
         GenQuad(SizeOp, ReturnVar, NulSym, OperandT(1))
      ELSIF IsVar(OperandT(1))
      THEN
         ReturnVar := MakeTemporary(ImmediateValue) ;
         GenQuad(SizeOp, ReturnVar, NulSym, GetType(OperandT(1)))
      ELSE
         WriteFormat0('SYSTEM procedure TSIZE expects the first parameter to be a type or variable') ;
         ReturnVar := MakeConstLit(MakeKey('0'))
      END
   ELSE
      Record := OperandT(NoOfParam) ;
      IF IsRecord(Record)
      THEN
         ReturnVar := MakeTemporary(ImmediateValue) ;
         GenQuad(SizeOp, ReturnVar, NulSym, Record)
(*
         IF NoOfParam=2
         THEN
            ReturnVar := MakeTemporary(ImmediateValue) ;
            GenQuad(SizeOp, ReturnVar, OperandT(1), Record)
         ELSIF NoOfParam>2
         THEN
            i := 2 ;
            ReturnVar := MakeTemporary(RightValue) ;
            PutVar(ReturnVar, Cardinal) ;
            GenQuad(SizeOp, ReturnVar, OperandT(1), Record) ;
            REPEAT
               ti := MakeTemporary(ImmediateValue) ;
               GenQuad(SizeOp, ti, OperandT(i), Record) ;
               PushTF(ti, Cardinal) ;
               PushT(GreaterTok) ;
               PushTF(ReturnVar, Cardinal) ;
               BuildRelOp ;
               PopBool(t, f) ;
               BackPatch(t, NextQuad) ;
               GenQuad(BecomesOp, ReturnVar, NulSym, ti) ;
               BackPatch(f, NextQuad) ;
               INC(i)
            UNTIL i>=NoOfParam
         END
*)
      ELSE
         WriteFormat0('SYSTEM procedure TSIZE expects the first parameter to be a record type') ;
         ReturnVar := MakeConstLit(MakeKey('0'))
      END
   END ;
   PopN(NoOfParam+1) ;       (* destroy the arguments and function *)
   PushTF(ReturnVar, GetType(ProcSym))
END BuildTSizeFunction ;


(*
   ExpectingParameterType - 
*)

PROCEDURE ExpectingParameterType (BlockSym, Type: CARDINAL) ;
VAR
   s1, s2: String ;
BEGIN
   IF NOT IsAModula2Type(Type)
   THEN
      IF Type=NulSym
      THEN
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(BlockSym)))) ;
         ErrorStringAt2(Sprintf1(Mark(InitString('the type used in the formal parameter in procedure (%s) is unknown')),
                                 s2),
                        GetDeclared(BlockSym), GetDeclared(Type))
      ELSIF IsPartialUnbounded(Type) OR IsUnknown(Type)
      THEN
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Type)))) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(BlockSym)))) ;
         ErrorStringAt2(Sprintf2(Mark(InitString('the type in the formal parameter is unknown (%s) in procedure (%s)')),
                                 s1, s2),
                        GetDeclared(BlockSym), GetDeclared(Type))
      ELSE
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Type)))) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(BlockSym)))) ;
         ErrorStringAt2(Sprintf2(Mark(InitString('the type (%s) specified as the formal parameter in procedure (%s) was not declared as a type')),
                                 s1, s2),
                        GetDeclared(BlockSym), GetDeclared(Type))
      END
   END
END ExpectingParameterType ;


(*
   ExpectingVariableType - 
*)

PROCEDURE ExpectingVariableType (BlockSym, Type: CARDINAL) ;
VAR
   s1, s2: String ;
BEGIN
   IF NOT IsAModula2Type(Type)
   THEN
      IF Type=NulSym
      THEN
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(BlockSym)))) ;
         ErrorStringAt2(Sprintf1(Mark(InitString('the type used during the variable declaration section in procedure (%s) is unknown')),
                                 s2),
                        GetDeclared(BlockSym), GetDeclared(Type))
      ELSIF IsPartialUnbounded(Type) OR IsUnknown(Type)
      THEN
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Type)))) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(BlockSym)))) ;
         ErrorStringAt2(Sprintf2(Mark(InitString('the type (%s) used during variable declaration section in procedure (%s) is unknown')),
                                 s1, s2),
                        GetDeclared(BlockSym), GetDeclared(Type))
      ELSE
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Type)))) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(BlockSym)))) ;
         ErrorStringAt2(Sprintf2(Mark(InitString('the symbol (%s) is not a type and therefore cannot be used to declare a variable in procedure (%s)')),
                                 s1, s2),
                        GetDeclared(BlockSym), GetDeclared(Type))
      END
   END
END ExpectingVariableType ;


(*
   CheckVariablesAndParameterTypesInBlock - checks to make sure that block, BlockSym, has
                                            parameters types and variable types which are legal.
*)

PROCEDURE CheckVariablesAndParameterTypesInBlock (BlockSym: CARDINAL) ;
VAR
   i, n,
   ParamNo: CARDINAL ;
BEGIN
   IF IsProcedure(BlockSym)
   THEN
      ParamNo := NoOfParam(BlockSym)
   ELSE
      ParamNo := 0
   END ;
   i := 1 ;
   REPEAT
      n := GetNth(BlockSym, i) ;
      IF (n#NulSym) AND (NOT IsTemporary(n)) AND
         (IsProcedure(BlockSym) OR ((IsDefImp(BlockSym) AND (GetMainModule()=BlockSym)) OR IsModule(BlockSym)))
      THEN
         IF i<=ParamNo
         THEN
            (* n is a parameter *)
            ExpectingParameterType(BlockSym, GetType(n))
         ELSE
            (* n is a local variable *)
            ExpectingVariableType(BlockSym, GetType(n))
         END
      END ;
      INC(i)
   UNTIL n=NulSym ;
END CheckVariablesAndParameterTypesInBlock ;


(*
   BuildProcedureStart - Builds start of the procedure. Generates a
                         quadruple which indicated the start of
                         this procedure declarations scope.
                         The Stack is expected to contain:


                         Entry                   Exit
                         =====                   ====

                 Ptr ->                                       <- Ptr
                        +------------+          +-----------+
                        | ProcSym    |          | ProcSym   |
                        |------------|          |-----------|
                        | Name       |          | Name      |
                        |------------|          |-----------|


                        Quadruples:

                        q   ProcedureScopeOp  Line#  Scope  ProcSym
*)

PROCEDURE BuildProcedureStart ;
VAR
   ProcSym: CARDINAL ;
BEGIN
   BuildLineNo ;
   PopT(ProcSym) ;
   Assert(IsProcedure(ProcSym)) ;
   PutProcedureScopeQuad(ProcSym, NextQuad) ;
   GenQuad(ProcedureScopeOp, GetPreviousTokenLineNo(), GetScope(ProcSym), ProcSym) ;
   PushT(ProcSym)
END BuildProcedureStart ;


(*
   BuildProcedureBegin - determines the start of the BEGIN END block of
                         the procedure.
                         The Stack is expected to contain:


                         Entry                   Exit
                         =====                   ====

                 Ptr ->                                       <- Ptr
                        +------------+          +-----------+
                        | ProcSym    |          | ProcSym   |
                        |------------|          |-----------|
                        | Name       |          | Name      |
                        |------------|          |-----------|


                        Quadruples:

                        q   NewLocalVarOp  _  _  ProcSym
*)

PROCEDURE BuildProcedureBegin ;
VAR
   ProcSym: CARDINAL ;
BEGIN
   BuildLineNo ;
   PopT(ProcSym) ;
   Assert(IsProcedure(ProcSym)) ;
   PutProcedureStartQuad(ProcSym, NextQuad) ;
   GenQuad(NewLocalVarOp, GetPreviousTokenLineNo(), GetScope(ProcSym), ProcSym) ;
   CurrentProc := ProcSym ;
   PushWord(ReturnStack, 0) ;
   PushT(ProcSym) ;
   CheckVariablesAt(ProcSym) ;
   CheckNeedPriorityBegin(ProcSym, GetCurrentModule()) ;
   PushWord(TryStack, NextQuad) ;
   PushWord(CatchStack, 0) ;
   IF HasExceptionBlock(ProcSym)
   THEN
      GenQuad(TryOp, NulSym, NulSym, 0)
   END
END BuildProcedureBegin ;


(*
   BuildProcedureEnd - Builds end of the procedure. Destroys space for
                       the local variables.
                       The Stack is expected to contain:


                       Entry                   Exit
                       =====                   ====

                Ptr ->                                       <- Ptr
                       +------------+          +-----------+
                       | ProcSym    |          | ProcSym   |
                       |------------|          |-----------|
                       | Name       |          | Name      |
                       |------------|          |-----------|


                       Quadruples:

                       q   KillLocalVarOp  _  _  ProcSym
*)

PROCEDURE BuildProcedureEnd ;
VAR
   t,
   ProcSym: CARDINAL ;
BEGIN
   PopT(ProcSym) ;
   IF HasExceptionBlock(ProcSym)
   THEN
      BuildRTExceptLeave(TRUE) ;
      GenQuad(CatchEndOp, NulSym, NulSym, NulSym)
   END ;
   IF GetType(ProcSym)#NulSym
   THEN
      BuildError(InitNoReturnRangeCheck())
   END ;
   BackPatch(PopWord(ReturnStack), NextQuad) ;
   CheckNeedPriorityEnd(ProcSym, GetCurrentModule()) ;
   CurrentProc := NulSym ;
   GenQuad(KillLocalVarOp, GetPreviousTokenLineNo(), NulSym, ProcSym) ;
   PutProcedureEndQuad(ProcSym, NextQuad) ;
   GenQuad(ReturnOp, NulSym, NulSym, ProcSym) ;
   CheckFunctionReturn(ProcSym) ;
   CheckVariablesInBlock(ProcSym) ;
   t := PopWord(CatchStack) ;
   t := PopWord(TryStack) ;
   PushT(ProcSym)
END BuildProcedureEnd ;


(*
   CheckReadBeforeInitialized - 
*)

PROCEDURE CheckReadBeforeInitialized (ProcSym: CARDINAL; Start, End: CARDINAL) ;
VAR
   s1, s2              : String ;
   i, n, ParamNo,
   ReadStart, ReadEnd,
   WriteStart, WriteEnd: CARDINAL ;
BEGIN
   ParamNo := NoOfParam(ProcSym) ;
   i := 1 ;
   REPEAT
      n := GetNth(ProcSym, i) ;
      IF (n#NulSym) AND (NOT IsTemporary(n))
      THEN
         GetReadQuads(n, RightValue, ReadStart, ReadEnd) ;
         GetWriteQuads(n, RightValue, WriteStart, WriteEnd) ;
         IF i>ParamNo
         THEN
            (* n is a not a parameter thus we can check *)
            IF (ReadStart>0) AND (ReadStart<End)
            THEN
               (* it is read in the first basic block *)
               IF ReadStart<WriteStart
               THEN
                  (* read before written, this is a problem which must be fixed *)
                  s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(n)))) ;
                  s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(ProcSym)))) ;
                  ErrorStringAt2(Sprintf2(Mark(InitString('reading from a variable (%s) before it is initialized in procedure (%s)')),
                                          s1, s2),
                                 GetDeclared(n), GetDeclared(n))
               END
            END
         END
      END ;
      INC(i)
   UNTIL n=NulSym
END CheckReadBeforeInitialized ;


(*
   VariableAnalysis - checks to see whether a variable is:

                      read before it has been initialized
*)

PROCEDURE VariableAnalysis (Start, End: CARDINAL) ;
VAR
   Op           : QuadOperator ;
   Op1, Op2, Op3: CARDINAL ;
BEGIN
   IF Pedantic
   THEN
      GetQuad(Start, Op, Op1, Op2, Op3) ;
      CASE Op OF

      NewLocalVarOp:  CheckReadBeforeInitialized(Op3, Start, End)

      ELSE
      END
   END
END VariableAnalysis ;


(*
   IsNeverAltered - returns TRUE if variable, sym, is never altered
                    between quadruples: Start..End
*)

PROCEDURE IsNeverAltered (sym: CARDINAL; Start, End: CARDINAL) : BOOLEAN ;
VAR
   WriteStart, WriteEnd: CARDINAL ;
BEGIN
   GetWriteLimitQuads(sym, GetMode(sym), Start, End, WriteStart, WriteEnd) ;
   RETURN( (WriteStart=0) AND (WriteEnd=0) )
END IsNeverAltered ;


(*
   IsConditionVariable - returns TRUE if the condition at quadruple, q, is variable.
*)

PROCEDURE IsConditionVariable (q: CARDINAL; Start, End: CARDINAL) : BOOLEAN ;
VAR
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
   LeftFixed,
   RightFixed   : BOOLEAN ;
BEGIN
   GetQuad(q, op, op1, op2, op3) ;
   IF op=GotoOp
   THEN
      RETURN( FALSE )
   ELSE
      LeftFixed  := IsConst(op1) ;
      RightFixed := IsConst(op2) ;
      IF NOT LeftFixed
      THEN
         LeftFixed := IsNeverAltered(op1, Start, End)
      END ;
      IF NOT RightFixed
      THEN
         RightFixed := IsNeverAltered(op2, Start, End)
      END ;
      RETURN( NOT (LeftFixed AND RightFixed) )
   END
END IsConditionVariable ;


(*
   IsInfiniteLoop - returns TRUE if an infinite loop is found.
                    Given a backwards jump at, End, it returns a BOOLEAN which depends on
                    whether a jump is found to jump beyond, End. If a conditonal jump is found
                    to pass over, End, the condition is tested for global variables, procedure variables and
                    constants.

                         constant        - ignored
                         variables       - tested to see whether they are altered inside the loop
                         global variable - the procedure tests to see whether it is altered as above
                                           but will also test to see whether this loop calls a procedure
                                           in which case it believes the loop NOT to be infinite
                                           (as this procedure call might alter the global variable)

                    Note that this procedure can easily be fooled by the user altering variables
                    with pointers.
*)

PROCEDURE IsInfiniteLoop (End: CARDINAL) : BOOLEAN ;
VAR
   SeenCall,
   IsGlobal     : BOOLEAN ;
   Current,
   Start        : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   SeenCall := FALSE ;
   IsGlobal := FALSE ;
   GetQuad(End, op, op1, op2, Start) ;
   Current := Start ;
   WHILE Current#End DO
      GetQuad(Current, op, op1, op2, op3) ;
      (* remember that this function is only called once we have optimized the redundant gotos and conditionals *)
      IF IsConditional(Current) AND (NOT IsGlobal)
      THEN
         IsGlobal := (IsVar(op1) AND (NOT IsProcedure(GetVarScope(op1)))) OR
                     (IsVar(op2) AND (NOT IsProcedure(GetVarScope(op2))))
      END ;
      IF op=CallOp
      THEN
         SeenCall := TRUE
      END ;
      IF (op=GotoOp) OR (IsConditional(Current) AND IsConditionVariable(Current, Start, End))
      THEN
         IF (op3>End) OR (op3<Start)
         THEN
            RETURN( FALSE )    (* may jump out of this loop, good *)
         END
      END ;
      Current := GetNextQuad(Current)
   END ;
   GetQuad(End, op, op1, op2, op3) ;
   IF IsConditional(End)
   THEN
      IF IsConditionVariable(End, Start, End)
      THEN
         RETURN( FALSE )
      ELSE
         IF NOT IsGlobal
         THEN
            IsGlobal := (IsVar(op1) AND (NOT IsProcedure(GetVarScope(op1)))) OR
                        (IsVar(op2) AND (NOT IsProcedure(GetVarScope(op2))))
         END
      END
   END ;
   (* we have found a likely infinite loop if no conditional uses a global and no procedure call was seen *)
   RETURN( NOT (IsGlobal AND SeenCall) )
END IsInfiniteLoop ;


(*
   LoopAnalysis - checks whether an infinite loop exists.
*)

PROCEDURE LoopAnalysis (Current, End: CARDINAL) ;
VAR
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   IF Pedantic
   THEN
      WHILE (Current<=End) AND (Current#0) DO
         GetQuad(Current, op, op1, op2, op3) ;
         IF (op=GotoOp) OR IsConditional(Current)
         THEN
            IF op3<=Current
            THEN
               (* found a loop - ie a branch which goes back in quadruple numbers *)
               IF IsInfiniteLoop(Current)
               THEN
                  WarnStringAt(InitString('it is very likely (although not absolutely certain) that the top of an infinite loop is here'),
                               QuadToTokenNo(op3)) ;
                  WarnStringAt(InitString('and the bottom of the infinite loop is ends here or alternatively a component of this loop is never executed'),
                               QuadToTokenNo(Current))
               END
            END
         END ;
         Current := GetNextQuad(Current)
      END
   END
END LoopAnalysis ;


(*
   CheckUninitializedVariablesAreUsed - checks to see whether uninitialized variables are used.
*)

PROCEDURE CheckUninitializedVariablesAreUsed (BlockSym: CARDINAL) ;
VAR
   i, n,
   ParamNo   : CARDINAL ;
   ReadStart,
   ReadEnd,
   WriteStart,
   WriteEnd  : CARDINAL ;
BEGIN
   IF IsProcedure(BlockSym)
   THEN
      ParamNo := NoOfParam(BlockSym)
   ELSE
      ParamNo := 0
   END ;
   i := 1 ;
   REPEAT
      n := GetNth(BlockSym, i) ;
      IF (n#NulSym) AND (NOT IsTemporary(n)) AND
         (IsProcedure(BlockSym) OR (((IsDefImp(BlockSym) AND (GetMainModule()=BlockSym)) OR IsModule(BlockSym)) AND
                                    (NOT IsExported(BlockSym, n))))
      THEN
         GetReadQuads(n, RightValue, ReadStart, ReadEnd) ;
         GetWriteQuads(n, RightValue, WriteStart, WriteEnd) ;
         IF i<=ParamNo
         THEN
            (* n is a parameter *)
            IF ReadStart=0
            THEN
               IF WriteStart=0
               THEN
                  MetaError2('unused parameter {%1Wad} in procedure {%2Wad}', n, BlockSym)
               ELSE
                  IF NOT IsVarParam(BlockSym, i)
                  THEN
                     MetaError2('writing to a non var parameter {%1Wad} and never reading from it in procedure {%2Wad}',
                                n, BlockSym)
                  END
               END
            END
         ELSE
            (* n is a local variable *)
            IF ReadStart=0
            THEN
               IF WriteStart=0
               THEN
                  MetaError2('unused variable {%1Wad} in {%2Wad}', n, BlockSym)
               ELSE
                  MetaError2('writing to a variable {%1Wad} and never reading from it in {%2Wad}', n, BlockSym)
               END
            ELSE
               IF WriteStart=0
               THEN
                  MetaError2('variable {%1Wad} is being used but it is never initialized in {%2Wad}', n, BlockSym)
               END
            END
         END
      END ;
      INC(i)
   UNTIL n=NulSym
END CheckUninitializedVariablesAreUsed ;


(*
   IsInlineWithinBlock - returns TRUE if an InlineOp is found
                         within start..end.
*)

PROCEDURE IsInlineWithinBlock (start, end: CARDINAL) : BOOLEAN ;
VAR
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   WHILE (start#end) AND (start#0) DO
      GetQuad(start, op, op1, op2, op3) ;
      IF op=InlineOp
      THEN
         RETURN( TRUE )
      END ;
      start := GetNextQuad(start)
   END ;
   RETURN( FALSE )
END IsInlineWithinBlock ;


(*
   AsmStatementsInBlock - returns TRUE if an ASM statement is found within a block, BlockSym.
*)

PROCEDURE AsmStatementsInBlock (BlockSym: CARDINAL) : BOOLEAN ;
VAR
   Scope,
   StartInit,
   EndInit,
   StartFinish,
   EndFinish    : CARDINAL ;
BEGIN
   IF IsProcedure(BlockSym)
   THEN
      GetProcedureQuads(BlockSym, Scope, StartInit, EndInit) ;
      RETURN( IsInlineWithinBlock(StartInit, EndInit) )
   ELSE
      GetModuleQuads(BlockSym, StartInit, EndInit, StartFinish, EndFinish) ;
      RETURN( IsInlineWithinBlock(StartInit, EndInit) OR
              IsInlineWithinBlock(StartFinish, EndFinish) )
   END
END AsmStatementsInBlock ;


(*
   CheckVariablesInBlock - given a block, BlockSym, check whether all variables are used.
*)

PROCEDURE CheckVariablesInBlock (BlockSym: CARDINAL) ;
BEGIN
   CheckVariablesAndParameterTypesInBlock(BlockSym) ;
   IF Pedantic
   THEN
      IF (NOT AsmStatementsInBlock(BlockSym))
      THEN
         CheckUninitializedVariablesAreUsed(BlockSym)
      END
   END
END CheckVariablesInBlock ;


(*
   CheckFunctionReturn - checks to see that a RETURN statement was present in a function.
*)

PROCEDURE CheckFunctionReturn (ProcSym: CARDINAL) ;
VAR
   n            : Name ;
   Op           : QuadOperator ;
   Op1, Op2, Op3,
   Scope,
   Start, End   : CARDINAL ;
BEGIN
   IF GetType(ProcSym)#NulSym
   THEN
      (* yes it is a function *)
      GetProcedureQuads(ProcSym, Scope, Start, End) ;
      GetQuad(Start, Op, Op1, Op2, Op3) ;
      IF Start=0
      THEN
         n := GetSymName(ProcSym) ;
         WriteFormat1('error in function %a', n) ;
         InternalError('incorrect start quad', __FILE__, __LINE__)
      END ;
      WHILE (Start#End) AND (Op#ReturnValueOp) AND (Op#InlineOp) DO
         Start := GetNextQuad(Start) ;
         GetQuad(Start, Op, Op1, Op2, Op3)
      END ;
      IF (Op#ReturnValueOp) AND (Op#InlineOp)
      THEN
         (* an InlineOp can always be used to emulate a RETURN *)
         n := GetSymName(ProcSym) ;
         WriteFormat1('function %a does not RETURN a value', n)
      END
   END
END CheckFunctionReturn ;


(*
   CheckReturnType - checks to see that the return type from currentProc is
                     assignment compatible with actualType.
*)

PROCEDURE CheckReturnType (currentProc, actualVal, actualType: CARDINAL) ;
VAR
   procType: CARDINAL ;
   s1, s2  : String ;
   n1, n2  : Name ;
BEGIN
   procType := GetType(currentProc) ;
   IF procType=NulSym
   THEN
      n1 := GetSymName(currentProc) ;
      WriteFormat1('attempting to RETURN a value from a procedure (%a) and not a function', n1)

   ELSIF AssignmentRequiresWarning(actualType, GetType(currentProc))
   THEN
      s1 := InitStringCharStar(KeyToCharStar(GetSymName(actualType))) ;
      s2 := InitStringCharStar(KeyToCharStar(GetSymName(procType))) ;
      ErrorString(NewWarning(GetTokenNo()),
                  Sprintf2(Mark(InitString('attempting to RETURN a value with a (possibly on other targets) incompatible type (%s) from a function which returns (%s)')),
                           s1, s2))
   ELSIF (NOT IsAssignmentCompatible(actualType, procType))
   THEN
      n1 := GetSymName(actualType) ;
      n2 := GetSymName(procType) ;
      WriteFormat2('attempting to RETURN a value with an incompatible type (%a) from a function which returns (%a)',
                   n1, n2)
   ELSIF IsProcedure(actualVal) AND (NOT IsAssignmentCompatible(actualVal, procType))
   THEN
(*
      MetaWarnings2('attempting to RETURN a value with an incompatible type {%1ad} from function {%2a} which returns {%2ta}',
                    actualVal, currentProc)

      --fixme--  introduce MetaWarning, MetaWarning2, MetaWarning3 into M2MetaError
*)
      s1 := InitStringCharStar(KeyToCharStar(GetSymName(actualVal))) ;
      s2 := InitStringCharStar(KeyToCharStar(GetSymName(procType))) ;
      ErrorString(NewWarning(GetTokenNo()),
                  Sprintf2(Mark(InitString('attempting to RETURN a value with a (possibly on other targets) incompatible type (%s) from a function which returns (%s)')),
                           s1, s2))
   ELSIF IsProcedure(actualVal) AND (NOT IsAssignmentCompatible(actualVal, GetType(CurrentProc)))
   THEN
      n1 := GetSymName(actualVal) ;
      n2 := GetSymName(GetType(currentProc)) ;
      WriteFormat2('attempting to RETURN a value with an incompatible type (%a) from a function which returns (%a)',
                   n1, n2)
   ELSE
      BuildRange(InitTypesAssignmentCheck(currentProc, actualVal))
   END  
END CheckReturnType ;


(*
   BuildReturn - Builds the Return part of the procedure.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

         Ptr ->
                 +------------+
                 | e1         |          Empty
                 |------------|
*)

PROCEDURE BuildReturn ;
VAR
   e2, t2,
   e1, t1,
   t, f,
   Des   : CARDINAL ;
BEGIN
   IF IsBoolean(1)
   THEN
      PopBool(t, f) ;
      (* Des will be a boolean type *)
      Des := MakeTemporary(RightValue) ;
      PutVar(Des, Boolean) ;
      PushTF(Des, Boolean) ;
      PushBool(t, f) ;
      BuildAssignmentWithoutBounds(FALSE, TRUE) ;
      PushTF(Des, Boolean)
   END ;
   PopTF(e1, t1) ;
   IF e1#NulSym
   THEN
      CheckReturnType(CurrentProc, e1, t1) ;
      (* dereference LeftValue if necessary *)
      IF GetMode(e1)=LeftValue
      THEN
         t2 := GetType(CurrentProc) ;
         e2 := MakeTemporary(RightValue) ;
         PutVar(e2, t2) ;
         CheckPointerThroughNil(e1) ;
         doIndrX(e2, e1) ;
         GenQuad(ReturnValueOp, e2, NulSym, CurrentProc)
      ELSE
         GenQuad(ReturnValueOp, e1, NulSym, CurrentProc)
      END
   END ;
   GenQuad(GotoOp, NulSym, NulSym, PopWord(ReturnStack)) ;
   PushWord(ReturnStack, NextQuad-1)
END BuildReturn ;


(*
   BuildDesignatorRecord - Builds the record referencing.
                           The Stack is expected to contain:


                           Entry                   Exit
                           =====                   ====

                   Ptr ->
                           +--------------+
                           | n            |
                           |--------------|
                           | fld1 | type1 |
                           |--------------|
                           .              .
                           .              .
                           .              .
                           |--------------|
                           | fldn | typen |                        <- Ptr
                           |--------------|        +-------------+
                           | Sym  | Type  |        | S    | type1|
                           |--------------|        |-------------|
*)

PROCEDURE BuildDesignatorRecord ;
VAR
   n, i, rw,
   PrevType,
   Sym, Type,
   adr, Res,
   t1, t2, t3: CARDINAL ;
BEGIN
   PopT(n) ;
   Sym  := OperandT(n+1) ;
   Type := SkipType(OperandF(n+1)) ;
   rw   := OperandMergeRW(n+1) ;
   Assert(IsLegal(rw)) ;
   (* adr will be Address type *)
   adr := MakeLeftValue(Sym, RightValue, Address) ;
   (* No type for t1 since constant *)
   t1 := MakeTemporary(ImmediateValue) ;
   Sym  := OperandT(n) ;
   Type := SkipType(OperandF(n)) ;
   PrevType := GetRecord(GetParent(Sym)) ;
   GenQuad(OffsetOp, t1, PrevType, Sym) ;
   IF n>1
   THEN
      InternalError('not expecting to see n>1', __FILE__, __LINE__)
   END ;
   (* Res will be an Address since it is LeftValue mode *)
   Res := MakeTemporary(LeftValue) ;
   (*
      Ok must reference by address
      - but we contain the type of the referenced entity
   *)
   PutLeftValueFrontBackType(Res, Type, Address) ;
   GenQuad(AddOp, Res, adr, t1) ;
   PopN(n+1) ;
   PushTFrw(Res, Type, rw)
END BuildDesignatorRecord ;


(*
   BuildDesignatorArray - Builds the array referencing.
                          The purpose of this procedure is to work out
                          whether the DesignatorArray is a static or
                          dynamic array and to call the appropriate
                          BuildRoutine.

                          The Stack is expected to contain:


                          Entry                   Exit
                          =====                   ====

                  Ptr ->
                          +--------------+
                          | e            |                        <- Ptr
                          |--------------|        +------------+
                          | Sym  | Type  |        | S    | T   |
                          |--------------|        |------------|
*)

PROCEDURE BuildDesignatorArray ;
VAR
   s       : String ;
   e, t, d,
   Sym, n,
   Type, rw: CARDINAL ;
BEGIN
   IF IsConst(OperandT(2)) AND IsConstructor(OperandT(2))
   THEN
      t := SkipType(GetType(OperandT(2))) ;
      IF t=NulSym
      THEN
         InternalError('constructor type should have been resolved', __FILE__, __LINE__)
      ELSIF IsArray(t)
      THEN
         PopT(e) ;
         PopTFD(Sym, Type, d) ;
         t := MakeTemporary(RightValue) ;
         PutVar(t, Type) ;
         PushTF(t, GetType(t)) ;
         PushT(Sym) ;
         BuildAssignment ;
         PushTFD(t, SkipType(GetType(t)), d) ;
         PushT(e)
      END
   END ;
   IF (NOT IsVar(OperandT(2))) AND (NOT IsTemporary(OperandT(2)))
   THEN
      ErrorStringAt2(Mark(InitString('can only access arrays using variables or formal parameters')),
                     GetDeclared(OperandT(2)), GetTokenNo())
   END ;
   Sym := SkipType(GetType(OperandT(2))) ;
   IF Sym=NulSym
   THEN
      IF GetSymName(Sym)=NulName
      THEN
         ErrorStringAt(Mark(InitString('type of array is undefined (no such array declared)')), GetTokenNo())
      ELSE
         s := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Sym)))) ;
         ErrorStringAt(Sprintf1(Mark(InitString('type of array is undefined (%s)')),
                                s),
                       GetTokenNo())
      END
   END ;
   IF IsUnbounded(Sym)
   THEN
      BuildDynamicArray
   ELSIF IsArray(Sym)
   THEN
      BuildStaticArray
   ELSE
      WriteFormat0('can only index static or dynamic arrays')
   END
END BuildDesignatorArray ;


(*
   BuildStaticArray - Builds the array referencing for static arrays.
                      The Stack is expected to contain:


                      Entry                   Exit
                      =====                   ====

              Ptr ->
                      +--------------+
                      | e            |                       <- Ptr
                      |--------------|        +------------+
                      | Sym  | Type  |        | S    | T   |
                      |--------------|        |------------|
*)

PROCEDURE BuildStaticArray ;
VAR
   rw,
   Dim,
   Array,
   Index,
   BackEndType,
   Type, Adr  : CARDINAL ;
BEGIN
   Index := OperandT(1) ;
   Array  := OperandT(2) ;
   Type := SkipType(OperandF(2)) ;
   rw := OperandMergeRW(2) ;
   Assert(IsLegal(rw)) ;
   Dim := OperandD(2) ;
   INC(Dim) ;
   IF GetMode(Index)=LeftValue
   THEN
      Index := MakeRightValue(Index, GetType(Index))
   END ;
   BuildRange(InitStaticArraySubscriptRangeCheck(GetArraySubscript(Type), Index, Dim)) ;

   (* now make Adr point to the address of the indexed element *)
   Adr := MakeTemporary(LeftValue) ;
   (*
      Ok in the future must reference the array element by its lvalue
      - but we contain the type of the referenced entity
   *)

   BackEndType := MakePointer(NulName) ;
   PutPointer(BackEndType, SkipType(GetType(Type))) ;
   (* PutVar(Adr, BackEndType) ; *)
   PutLeftValueFrontBackType(Adr, SkipType(GetType(Type)), BackEndType) ;

   GenQuad(ArrayOp, Adr, Index, Array) ;
   PopN(2) ;   (* remove all parameters to this procedure *)
   PushTFDrw(Adr, GetType(Adr), Dim, rw)
END BuildStaticArray ;


(*
   calculateMultipicand - generates quadruples which calculate the
                          multiplicand for the array at dimension, dim.
*)

PROCEDURE calculateMultipicand (arraySym, arrayType: CARDINAL; dim: CARDINAL) : CARDINAL ;
VAR
   ti, tj, tk, tl: CARDINAL ;
BEGIN
   IF dim=GetDimension(arrayType)
   THEN
      (* ti has no type since constant *)
      ti := MakeTemporary(ImmediateValue) ;
      GenQuad(ElementSizeOp, ti, arrayType, 1)
   ELSE
      INC(dim) ;
      tk := MakeTemporary(RightValue) ;
      PutVar(tk, Cardinal) ;
      GenHigh(tk, dim, arraySym) ;
      tl := MakeTemporary(RightValue) ;
      PutVar(tl, Cardinal) ;
      GenQuad(AddOp, tl, tk, MakeConstLit(MakeKey('1'))) ;
      tj := calculateMultipicand(arraySym, arrayType, dim) ;
      ti := MakeTemporary(RightValue) ;
      PutVar(ti, Cardinal) ;
      GenQuad(MultOp, ti, tj, tl)
   END ;
   RETURN( ti )
END calculateMultipicand ;


(*
   BuildDynamicArray - Builds the array referencing for dynamic arrays.
                       The Stack is expected to contain:


                       Entry                          Exit
                       =====                          ====

               Ptr ->
                       +-----------------------+
                       | Index                 |                                    <- Ptr
                       |-----------------------|      +---------------------------+
                       | ArraySym | Type | Dim |      | S  | T | ArraySym | Dim+1 |
                       |-----------------------|      |---------------------------|


   if Dim=1
   then
      S := base of ArraySym + TSIZE(Type)*Index
   else
      S := S + TSIZE(Type)*Index
   fi
*)

PROCEDURE BuildDynamicArray ;
VAR
   Sym, idx,
   Type, Adr,
   ArraySym,
   BackEndType,
   UnboundedType,
   PtrToBase,
   Base,
   Dim, i, rw,
   ti, tj, tk   : CARDINAL ;
BEGIN
   DumpStack ;
   Sym  := OperandT(2) ;
   Type := SkipType(OperandF(2)) ;
   Dim := OperandD(2) ;
   rw := OperandMergeRW(2) ;
   Assert(IsLegal(rw)) ;
   INC(Dim) ;
   IF Dim=1
   THEN
      (*
         Base has type address because
         BuildDesignatorRecord references by address.

         Build a record for retrieving the address of dynamic array.
         BuildDesignatorRecord will generate the required quadruples,
         therefore build set up the stack for BuildDesignatorRecord
         to generate the record access.

         Build above current current info needed for array.
         Note that, n, has gone by now.
      *)
      ArraySym := Sym ;
      UnboundedType := GetUnboundedRecordType(GetType(Sym)) ;
      PushTFrw(Sym, UnboundedType, rw) ;
      PushTF(GetUnboundedAddressOffset(GetType(Sym)),
             GetType(GetUnboundedAddressOffset(GetType(Sym)))) ;
      PushT(1) ;  (* One record field to dereference *)
      BuildDesignatorRecord ;
      PopT(PtrToBase) ;
      DumpStack ;
      (* Now actually copy Unbounded.ArrayAddress into base *)
      IF GetMode(PtrToBase)=LeftValue
      THEN
         Base := MakeTemporary(RightValue) ;
         PutVar(Base, Address) ;           (* has type ADDRESS *)
         CheckPointerThroughNil(PtrToBase) ;
         GenQuad(IndrXOp, Base, Address, PtrToBase)           (* Base = *PtrToBase *)
      ELSE
         Assert(GetMode(PtrToBase)#ImmediateValue) ;
         Base := PtrToBase
      END
   ELSE
      (* Base already calculated previously and pushed to stack *)
      UnboundedType := SkipType(OperandF(2)) ;
      Base := Sym ;
      ArraySym := OperandA(2)
   END ;
   Assert(GetType(Sym)=Type) ;
   ti := calculateMultipicand(Sym, Type, Dim) ;
   idx := OperandT(1) ;
   IF IsConst(idx)
   THEN
      (* tj has no type since constant *)
      tj := MakeTemporary(ImmediateValue) ;
      tk := MakeTemporary(ImmediateValue)
   ELSE
      (* tj has Cardinal type since we have multiplied array indices *)
      tj := MakeTemporary(RightValue) ;
      IF GetType(idx)#Cardinal
      THEN
         PushTF(RequestSym(MakeKey('CONVERT')), NulSym) ;
         PushT(Cardinal) ;
         PushT(idx) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction ;
         PopT(idx)
      END ;
      PutVar(tj, Cardinal) ;
      tk := MakeTemporary(RightValue) ;
      PutVar(tk, Cardinal)
   END ;
   BuildRange(InitDynamicArraySubscriptRangeCheck(ArraySym, idx, Dim)) ;

   PushT(tj) ;
   PushT(idx) ;
   BuildAssignmentWithoutBounds(FALSE, TRUE) ;

   GenQuad(MultOp, tk, ti, tj) ;
   Adr := MakeTemporary(LeftValue) ;
   (*
      Ok must reference by address
      - but we contain the type of the referenced entity
   *)
   BackEndType := MakePointer(NulName) ;
   PutPointer(BackEndType, GetType(Type)) ;

   IF Dim=GetDimension(Type)
   THEN
      PutLeftValueFrontBackType(Adr, GetType(Type), BackEndType) ;
      
      GenQuad(AddOp, Adr, Base, tk) ;
      PopN(2) ;
      PushTFADrw(Adr, GetType(Adr), ArraySym, Dim, rw)
   ELSE
      (* more to index *)
      PutLeftValueFrontBackType(Adr, Type, BackEndType) ;
      
      GenQuad(AddOp, Adr, Base, tk) ;
      PopN(2) ;
      PushTFADrw(Adr, GetType(Adr), ArraySym, Dim, rw)
   END
END BuildDynamicArray ;


(*
   BuildDesignatorPointer - Builds a pointer reference.
                            The Stack is expected to contain:


                            Entry                   Exit
                            =====                   ====

                    Ptr ->                                           <- Ptr
                            +--------------+        +--------------+
                            | Sym1  | Type1|        | Sym2  | Type2|
                            |--------------|        |--------------|
*)

PROCEDURE BuildDesignatorPointer ;
VAR
   n1, n2     : Name ;
   rw,
   BackEndType,
   Sym1, Type1,
   Sym2, Type2: CARDINAL ;
BEGIN
   PopTFrw(Sym1, Type1, rw) ;
   Type1 := SkipType(Type1) ;
   IF IsUnknown(Sym1)
   THEN
      n1 := GetSymName(Sym1) ;
      n2 := GetSymName(Sym1) ;
      WriteFormat2('symbol (%a) is undefined and thus cannot resolve (%a^)',
                   n1, n2)
   ELSIF IsPointer(Type1)
   THEN
      Type2 := GetType(Type1) ;
      Sym2 := MakeTemporary(LeftValue) ;
      (*
         Ok must reference by address
         - but we contain the type of the referenced entity
      *)
      MarkAsRead(rw) ;
      IF GetMode(Sym1)=LeftValue
      THEN
         rw := NulSym ;
         CheckPointerThroughNil(Sym1) ;
         PutLeftValueFrontBackType(Sym2, Type2, Type1) ;
         GenQuad(IndrXOp, Sym2, Type1, Sym1)            (* Sym2 := *Sym1 *)
      ELSE
         PutLeftValueFrontBackType(Sym2, Type2, NulSym) ;
         GenQuad(BecomesOp, Sym2, NulSym, Sym1)         (* Sym2 :=  Sym1 *)
      END ;

      PutVarPointerCheck(Sym2, TRUE) ;       (* we should check this for *)
                                             (* pointer via NIL          *)
      PushTFrw(Sym2, Type2, rw)
   ELSE
      MetaError2('{%1ad} is not a pointer type but a {%2d}', Sym1, Type1)
   END
END BuildDesignatorPointer ;


(*
   StartBuildWith - performs the with statement.
                    The Stack:

                    Entry                    Exit

                    +------------+
                    | Sym | Type |           Empty
                    |------------|
*)

PROCEDURE StartBuildWith ;
VAR
   n        : Name ;
   Sym, Type,
   adr      : CARDINAL ;
BEGIN
   DumpStack ;
   PopTF(Sym, Type) ;
   Type := SkipType(Type) ;
   adr := MakeTemporary(LeftValue) ;
   PutLeftValueFrontBackType(adr, Type, NulSym) ;

   IF GetMode(Sym)=LeftValue
   THEN
      (* copy LeftValue *)
      GenQuad(BecomesOp, adr, NulSym, Sym)
   ELSE
      (* calculate the address of Sym *)
      GenQuad(AddrOp, adr, NulSym, Sym)
   END ;
   PushWith(adr, Type, Sym) ;
   IF Type=NulSym
   THEN
      IF IsTemporary(Sym)
      THEN
         WriteFormat0('unknown record variable specified in WITH statement')
      ELSE
         n := GetSymName(Sym) ;
         WriteFormat1('symbol (%a) is unknown, it should be a variable of a record type',
                           n)
      END
   ELSIF NOT IsRecord(Type)
   THEN
      IF GetSymName(Type)=NulName
      THEN
         IF GetSymName(Sym)=NulSym
         THEN
            WriteFormat0('expression in the WITH statement does must be a record type')
         ELSE
            n := GetSymName(Sym) ;
            WriteFormat1('the type being used in this WITH statement is not a record type, variable (%a)',
                         n)
         END
      ELSE
         n := GetSymName(Type) ;
         WriteFormat1('the type being used in this WITH statement is not a record type (%a)',
                           n)
      END
   END ;
   StartScope(Type)
 ; DumpStack ;
END StartBuildWith ;


(*
   EndBuildWith - terminates the innermost with scope.
*)

PROCEDURE EndBuildWith ;
BEGIN
   DumpStack ;
   EndScope ;
   PopWith
 ; DumpStack ;
END EndBuildWith ;


(*
   PushWith - pushes sym and type onto the with stack. It checks for
              previous declaration of this record type.
*)

PROCEDURE PushWith (Sym, Type, RecordVar: CARDINAL) ;
VAR
   i, n: CARDINAL ;
   f   : WithFrame ;
BEGIN
   IF Pedantic
   THEN
      n := NoOfItemsInStackAddress(WithStack) ;
      i := 1 ;  (* top of the stack *)
      WHILE i<=n DO
         (* Search for other declarations of the with using Type *)
         f := PeepAddress(WithStack, i) ;
         IF f^.RecordSym=Type
         THEN
            WriteFormat0('cannot have nested WITH statements referencing the same RECORD')
         END ;
         INC(i)
      END
   END ;
   NEW(f) ;
   WITH f^ DO
      PtrSym    := Sym ;
      RecordSym := Type ;
      rw        := RecordVar
   END ;
   PushAddress(WithStack, f)
END PushWith ;


PROCEDURE PopWith ;
VAR
   f: WithFrame ;
BEGIN
   f := PopAddress(WithStack) ;
   DISPOSE(f)
END PopWith ;


(*
   CheckWithReference - performs the with statement.
                        The Stack:

                        Entry                    Exit

                        +------------+           +------------+
                        | Sym | Type |           | Sym | Type |
                        |------------|           |------------|
*)

PROCEDURE CheckWithReference ;
VAR
   f        : WithFrame ;
   i, n, rw,
   Sym, Type: CARDINAL ;
BEGIN
   n := NoOfItemsInStackAddress(WithStack) ;
   IF (n>0) AND (NOT SuppressWith)
   THEN
      PopTFrw(Sym, Type, rw) ;
      (* inner WITH always has precidence *)
      i := 1 ;  (* top of stack *)
      WHILE i<=n DO
         (* WriteString('Checking for a with') ; *)
         f := PeepAddress(WithStack, i) ;
         WITH f^ DO
            IF IsRecordField(Sym) AND (GetRecord(GetParent(Sym))=RecordSym)
            THEN
               (* Fake a RecordSym.op *)
               PushTFrw(PtrSym, RecordSym, rw) ;
               PushTF(Sym, Type) ;
               BuildAccessWithField ;
               PopTFrw(Sym, Type, rw) ;
               i := n+1
            ELSE
               INC(i)
            END
         END
      END ;
      PushTFrw(Sym, Type, rw)
   END
END CheckWithReference ;


(*
   BuildAccessWithField - similar to BuildDesignatorRecord except it
                          does not perform the address operation.
                          The address will have been computed at the
                          beginning of the WITH statement.
                          It also stops the GenQuad procedure from examining the
                          with stack.

                          The Stack

                          Entry

                   Ptr ->
                          +--------------+
                          | Field | Type1|                          <- Ptr
                          |-------|------|          +-------------+
                          | Adr   | Type2|          | Sym  | Type1|
                          |--------------|          |-------------|
*)

PROCEDURE BuildAccessWithField ;
VAR
   OldSuppressWith: BOOLEAN ;
   rw,
   Field, Type1,
   Adr, Type2,
   Res,
   t1, t2         : CARDINAL ;
BEGIN
   OldSuppressWith := SuppressWith ;
   SuppressWith := TRUE ;
   (*
      now the WITH cannot look at the stack of outstanding WITH records.
   *)
   PopTF(Field, Type1) ;
   PopTFrw(Adr, Type2, rw) ;
   t1 := MakeTemporary(ImmediateValue) ;
   (* No type since t1 is constant *)
   GenQuad(OffsetOp, t1, GetRecord(GetParent(Field)), Field) ;
   Res := MakeTemporary(LeftValue) ;
   (*
      Ok must reference by address
      - but we contain the type of the referenced entity
   *)
   PutLeftValueFrontBackType(Res, Type1, NulSym) ;
   GenQuad(AddOp, Res, Adr, t1) ;
   PushTFrw(Res, Type1, rw) ;
   SuppressWith := OldSuppressWith
END BuildAccessWithField ;


(*
   CheckOuterScopeProcedureVariable - checks whether the symbol
                                      on top of the stack is a procedure
                                      symbol from a previous scope.

                                      Entry                    Exit

                                      +------------+           +------------+
                                      | Sym | Type |           | Sym | Type |
                                      |------------|           |------------|
*)

PROCEDURE CheckOuterScopeProcedureVariable ;
VAR
   n1, n2: Name ;
   t2, t1,
   t, c,
   ScopeSym,
   DeclaredScopeSym,
   Sym, Type       : CARDINAL ;
BEGIN
   ScopeSym := GetCurrentScope() ;
   (*
    *  note that we do not need to check for nested procedure variables when
    *  using the GCC backend as GCC is intelligent to work this out for itself.
    *  So we simply use the variables and not calculate the position using the
    *  activation record pointer.
    *)
   IF (NOT UsingGCCBackend) AND IsProcedure(ScopeSym)
   THEN
      (* currently inside a procedure *)
      PopTF(Sym, Type) ;
      IF IsVar(Sym)
      THEN
         (* note we must not try to GetScope if Sym is not a variable *)
         DeclaredScopeSym := GetScope(Sym) ;

         IF CompilerDebugging
         THEN
            n1 := GetSymName(Sym) ;
            n2 := GetSymName(GetScope(Sym)) ;
            printf2('need to reference symbol, %a, defined in scope %a\n',
                     n1, n2)
         END ;

         IF IsProcedure(DeclaredScopeSym) AND (DeclaredScopeSym#ScopeSym)
         THEN
            (*
               so now we know that Sym is a procedure variable which was not declared in
               the current procedure but in an outer procedure.
            *)
            t := MakeTemporary(RightValue) ;
            PutVar(t, Address) ;
            GenQuad(BecomesOp, t, NulSym, ActivationPointer) ;
            ScopeSym := GetScope(ScopeSym) ;
            WHILE IsProcedure(DeclaredScopeSym) AND (ScopeSym#DeclaredScopeSym) DO
               c := MakeTemporary(ImmediateValue) ;
               (* must continue to chain backwards to find the scope where Sym was declared *)
               GenQuad(OffsetOp, c, GetRecord(GetParent(ActivationPointer)), ActivationPointer) ;
               (* need to look indirectly to find next activation record *)
               t1 := MakeTemporary(RightValue) ;   (* we use a different variable to help the optimizer *)
               PutVar(t1, Address) ;
               GenQuad(AddOp, t1, t, c) ;
               t2 := MakeTemporary(RightValue) ;   (* we use a different variable to help the optimizer *)
               PutVar(t2, Address) ;
               GenQuad(IndrXOp, t2, Address, t1) ; (* next chained frame is now in t2 *)
               t := t2 ;
               ScopeSym := GetScope(ScopeSym)
            END ;
            IF ScopeSym=DeclaredScopeSym
            THEN
               (* finished chaining backwards, found sym in scope ScopeSym *)
               c := MakeTemporary(ImmediateValue) ;
               GenQuad(OffsetOp, c, GetRecord(GetParent(Sym)), Sym) ;
               (* calculate address of sym *)
               t1 := MakeTemporary(RightValue) ;   (* we use a different variable to help the optimizer *)
               PutVar(t1, Address) ;
               GenQuad(AddOp, t1, t, c) ;

               (* now we create a new variable, sym, which will contain the address of the variable *)
               Sym := MakeTemporary(LeftValue) ;
               PutLeftValueFrontBackType(Sym, Type, NulSym) ;
               GenQuad(BecomesOp, Sym, NulSym, t1)
            ELSE
               InternalError('we should have found symbol in a procedure scope', __FILE__, __LINE__)
            END
         END
      END ;
      PushTF(Sym, Type)
   END
END CheckOuterScopeProcedureVariable ;


(*
   BuildNulExpression - Builds a nul expression on the stack.
                        The Stack:

                        Entry             Exit

                                                         <- Ptr
                        Empty             +------------+
                                          | NulSym     |
                                          |------------|
*)

PROCEDURE BuildNulExpression ;
BEGIN
   PushT(NulSym)
END BuildNulExpression ;


(*
   BuildTypeForConstructor - pushes the type implied by the current constructor.
                             If no constructor is currently being built then
                             it Pushes a Bitset type.
*)

PROCEDURE BuildTypeForConstructor ;
VAR
   c: ConstructorFrame ;
BEGIN
   IF NoOfItemsInStackAddress(ConstructorStack)=0
   THEN
      PushT(Bitset)
   ELSE
      c := PeepAddress(ConstructorStack, 1) ;
      WITH c^ DO
         IF IsArray(type) OR IsSet(type)
         THEN
            PushT(GetType(type))
         ELSIF IsRecord(type)
         THEN
            PushT(GetType(GetNth(type, index)))
         ELSE
            MetaError1('{%1ad} is not a set, record or array type which is expected when constructing an aggregate entity',
                       type)
         END
      END ;
      stop
   END
END BuildTypeForConstructor ;


(*
   BuildSetStart - Pushes a Bitset type on the stack.

                      The Stack:

                      Entry             Exit

               Ptr ->                                        <- Ptr

                      Empty             +--------------+
                                        | Bitset       |
                                        |--------------|
*)

PROCEDURE BuildSetStart ;
BEGIN
   PushT(Bitset)
END BuildSetStart ;


(*
   BuildSetEnd - pops the set value and type from the stack
                 and pushes the value,type pair.

                    Entry                   Exit

             Ptr ->
                    +--------------+
                    | Set Value    |                         <- Ptr
                    |--------------|        +--------------+
                    | Set Type     |        | Value | Type |
                    |--------------|        |--------------|
*)

PROCEDURE BuildSetEnd ;
VAR
   v, t: CARDINAL ;
BEGIN
   PopT(v) ;
   PopT(t) ;
   PushTF(v, t) ;
   Assert(IsSet(t))
END BuildSetEnd ;


(*
   BuildEmptySet - Builds an empty set on the stack.
                   The Stack:

                   Entry             Exit

                                                     <- Ptr
                                     +-------------+
            Ptr ->                   | Value       |
                   +-----------+     |-------------|
      	       	   | SetType   |     | SetType     |
                   |-----------|     |-------------|

*)

PROCEDURE BuildEmptySet ;
VAR
   n     : Name ;
   Type  : CARDINAL ;
   NulSet: CARDINAL ;
BEGIN
   PopT(Type) ;  (* type of set we are building *)
   IF (Type=NulSym) AND Pim
   THEN
      (* allowed generic {} in PIM Modula-2 *)
   ELSIF IsUnknown(Type)
   THEN
      n := GetSymName(Type) ;
      WriteFormat1('set type %a is undefined', n) ;
      Type := Bitset
   ELSIF NOT IsSet(SkipType(Type))
   THEN
      n := GetSymName(Type) ;
      WriteFormat1('expecting a set type %a', n) ;
      Type := Bitset
   ELSE
      Type := SkipType(Type) ;
      Assert((Type#NulSym))
   END ;
   NulSet := MakeTemporary(ImmediateValue) ;
   PutVar(NulSet, Type) ;
   PutConstSet(NulSet) ;
   IF CompilerDebugging
   THEN
      n := GetSymName(Type) ;
      printf1('set type = %a\n', n)
   END ;
   PushNulSet(Type) ;   (* onto the ALU stack  *)
   PopValue(NulSet) ;   (* ALU -> symbol table *)

   (* and now construct the M2Quads stack as defined by the comments above *)
   PushT(Type) ;
   PushT(NulSet) ;
   IF CompilerDebugging
   THEN
      n := GetSymName(Type) ;
      printf2('Type = %a  (%d)  built empty set\n', n, Type) ;
      DisplayStack    (* Debugging info *)
   END
END BuildEmptySet ;


(*
   BuildInclRange - includes a set range with a set.


                          Entry                   Exit
                          =====                   ====


                   Ptr ->
                          +------------+
                          | El2        |
                          |------------|
                          | El1        |                                 <- Ptr
                          |------------|           +-------------------+
                          | Set Value  |           | Value + {El1..El2}|
                          |------------|           |-------------------|

                   No quadruples produced as the range info is contained within
                   the set value.
*)

PROCEDURE BuildInclRange ;
VAR
   n       : Name ;
   el1, el2,
   value   : CARDINAL ;
BEGIN
   PopT(el2) ;
   PopT(el1) ;
   PopT(value) ;
   IF NOT IsConstSet(value)
   THEN
      n := GetSymName(el1) ;
      WriteFormat1('can only add bit ranges to a constant set, %a is not a constant set', n)
   END ;
   IF IsConst(el1) AND IsConst(el2)
   THEN
      PushValue(value) ;  (* onto ALU stack *)
      AddBitRange(GetTokenNo(), el1, el2) ;
      PopValue(value)     (* ALU -> symboltable *)
   ELSE
      IF NOT IsConst(el1)
      THEN
         n := GetSymName(el1) ;
         WriteFormat1('must use constants as ranges when defining a set constant, problem with the low value %a', n)
      END ;
      IF NOT IsConst(el2)
      THEN
         n := GetSymName(el2) ;
         WriteFormat1('must use constants as ranges when defining a set constant, problem with the high value %a', n)
      END
   END ;
   PushT(value)
END BuildInclRange ;


(*
   BuildInclBit - includes a bit into the set.

                         Entry                   Exit
                         =====                   ====


                  Ptr ->
                         +------------+
                         | Element    |                         <- Ptr
                         |------------|          +------------+
                         | Value      |          | Value      |
                         |------------|          |------------|

*)

PROCEDURE BuildInclBit ;
VAR
   e           : Error ;
   el, value, t: CARDINAL ;
BEGIN
   PopT(el) ;
   PopT(value) ;
   IF IsConst(el)
   THEN
      PushValue(value) ;  (* onto ALU stack *)
      AddBit(GetTokenNo(), el) ;
      PopValue(value)    (* ALU -> symboltable *)
   ELSE
      IF GetMode(el)=LeftValue
      THEN
         t := MakeTemporary(RightValue) ;
         PutVar(t, GetType(el)) ;
         CheckPointerThroughNil(el) ;
         doIndrX(t, el) ;
         el := t
      END ;
      IF IsConst(value)
      THEN
         (* move constant into a variable to achieve the include *)
         t := MakeTemporary(RightValue) ;
         PutVar(t, GetType(value)) ;
         GenQuad(BecomesOp, t, NulSym, value) ;
         value := t
      END ;
      GenQuad(InclOp, value, NulSym, el)
   END ;
   PushT(value)
END BuildInclBit ;


(*
   PushConstructor - 
*)

PROCEDURE PushConstructor (sym: CARDINAL) ;
VAR
   c: ConstructorFrame ;
BEGIN
   NEW(c) ;
   WITH c^ DO
      type := SkipType(sym) ;
      index := 1
   END ;
   PushAddress(ConstructorStack, c)
END PushConstructor ;


(*
   PopConstructor - removes the top constructor from the top of stack.
*)

PROCEDURE PopConstructor ;
VAR
   c: ConstructorFrame ;
BEGIN
   c := PopAddress(ConstructorStack) ;
   DISPOSE(c)
END PopConstructor ;


(*
   NextConstructorField - increments the top of constructor stacks index by one.
*)

PROCEDURE NextConstructorField ;
VAR
   c: ConstructorFrame ;
BEGIN
   c := PeepAddress(ConstructorStack, 1) ;
   INC(c^.index)
END NextConstructorField ;


(*
   SilentBuildConstructor - places NulSym into the constructor fifo queue.
*)

PROCEDURE SilentBuildConstructor ;
BEGIN
   PutConstructorIntoFifoQueue(NulSym)
END SilentBuildConstructor ;


(*
   BuildConstructor - builds a constructor.
                      Stack

                      Entry                 Exit

               Ptr ->
                      +------------+
                      | Type       |                <- Ptr
                      |------------+
*)

PROCEDURE BuildConstructor ;
VAR
   name      : Name ;
   constValue,
   type,
   const     : CARDINAL ;
BEGIN
   PopT(type) ;
   constValue := MakeTemporary(ImmediateValue) ;
   PutVar(constValue, type) ;
   PutConstructor(constValue) ;
   PushValue(constValue) ;
   ChangeToConstructor(GetTokenNo(), type) ;
   PutConstructorFrom(constValue, type) ;
   PopValue(constValue) ;
   PutConstructorIntoFifoQueue(constValue) ;
   PushConstructor(type)
END BuildConstructor ;


(*
   SilentBuildConstructorStart - removes an entry from the constructor fifo queue.
*)

PROCEDURE SilentBuildConstructorStart ;
VAR
   constValue: CARDINAL ;
BEGIN
   GetConstructorFromFifoQueue(constValue)
END SilentBuildConstructorStart ;


(*
   BuildConstructorStart - builds a constructor.
                           Stack

                           Entry                 Exit

                    Ptr ->                                          <- Ptr
                           +------------+        +----------------+
                           | Type       |        | ConstructorSym |
                           |------------+        |----------------|
*)

PROCEDURE BuildConstructorStart ;
VAR
   constValue,
   type      : CARDINAL ;
BEGIN
   PopT(type) ;   (* we ignore the type as we already have the constructor symbol from pass C *)
   GetConstructorFromFifoQueue(constValue) ;
   Assert(type=GetType(constValue)) ;
   PushT(constValue) ;
   PushConstructor(type)
END BuildConstructorStart ;


(*
   BuildConstructorEnd - removes the current constructor frame from the
                         constructor stack (it does not effect the quad
                         stack)

                         Entry                 Exit

                  Ptr ->                                      <- Ptr
                         +------------+        +------------+
                         | const      |        | const      |
                         |------------|        |------------|
*)

PROCEDURE BuildConstructorEnd ;
BEGIN
   PopConstructor
END BuildConstructorEnd ;


(*
   AddFieldTo - adds field, e, to, value.
*)

PROCEDURE AddFieldTo (value, e: CARDINAL) : CARDINAL ;
BEGIN
   IF IsSet(SkipType(GetType(value)))
   THEN
      PutConstSet(value) ;
      PushT(value) ;
      PushT(e) ;
      BuildInclBit ;
      PopT(value)
   ELSE
      PushValue(value) ;
      AddField(GetTokenNo(), e) ;
      PopValue(value)
   END ;
   RETURN( value )
END AddFieldTo ;


(*
   BuildComponentValue -  builds a component value.

                          Entry                 Exit

                   Ptr ->                                      <- Ptr


                          +------------+        +------------+
                          | const      |        | const      |
                          |------------|        |------------|
*)

PROCEDURE BuildComponentValue ;
VAR
   const,
   e1, e2   : CARDINAL ;
   nuldotdot,
   nulby    : Name ;
BEGIN
   PopT(nulby) ;
   IF nulby=NulTok
   THEN
      PopT(nuldotdot) ;
      IF nuldotdot=NulTok
      THEN
         PopT(e1) ;
         PopT(const) ;
         PushT(AddFieldTo(const, e1))
      ELSE
         PopT(e2) ;
         PopT(e1) ;
         PopT(const) ;
         PushValue(const) ;
         AddBitRange(GetTokenNo(), e1, e2) ;
         PopValue(const) ;
         PushT(const)
      END
   ELSE
      PopT(e1) ;
      PopT(nuldotdot) ;
      IF nuldotdot=NulTok
      THEN
         PopT(e2) ;
         PopT(const) ;
         PushValue(const) ;
         AddElements(GetTokenNo(), e2, e1) ;
         PopValue(const) ;
         PushT(const)
      ELSE
         PopT(e2) ;
         PopT(e1) ;
         PopT(const) ;
         WriteFormat0('either the constant must be an array constructor or a set constructor but not both') ;
         PushT(const)
      END
   END
END BuildComponentValue ;


(*
   RecordOp - Records the operator passed on the stack.
              Checks for AND operator or OR operator
              if either of these operators are found then BackPatching
              takes place.
              The Expected Stack:

              Entry                        Exit

       Ptr ->                                               <- Ptr
              +-------------+               +-------------+
              | OperatorTok |               | OperatorTok |
              |-------------|               |-------------|
              | t    | f    |               | t    | f    |
              |-------------|               |-------------|


              If OperatorTok=AndTok
              Then
                 BackPatch(f, NextQuad)
              Elsif OperatorTok=OrTok
              Then
                 BackPatch(t, NextQuad)
              End
*)

PROCEDURE RecordOp ;
VAR
   Op  : Name ;
   t, f: CARDINAL ;
BEGIN
   PopT(Op) ;
   IF (Op=AndTok) OR (Op=AmbersandTok)
   THEN
      CheckBooleanId ;
      PopBool(t, f) ;
      BackPatch(t, NextQuad) ;
      PushBool(0, f)
   ELSIF Op=OrTok
   THEN
      CheckBooleanId ;
      PopBool(t, f) ;
      BackPatch(f, NextQuad) ;
      PushBool(t, 0)
   END ;
   PushT(Op)
END RecordOp ;


(*
   CheckForLogicalOperator - returns a logical operator if the operands imply
                             a logical operation should be performed.
*)

PROCEDURE CheckForLogicalOperator (Tok: Name; e1, t1, e2, t2: CARDINAL) : Name ;
BEGIN
   IF (Tok=PlusTok) OR (Tok=TimesTok) OR (Tok=DivideTok) OR (Tok=MinusTok)
   THEN
      (* --fixme-- when we add complex arithmetic, we must check constructor is not a complex constant *)
      IF ((t2#NulSym) AND IsSet(SkipType(t2))) OR
         IsConstSet(e2) OR IsConstructor(e2)
      THEN
         IF Tok=PlusTok
         THEN
            RETURN( LogicalOrTok )
         ELSIF Tok=DivideTok
         THEN
            RETURN( LogicalXorTok )
         ELSIF Tok=TimesTok
         THEN
            RETURN( LogicalAndTok )
         ELSIF Tok=MinusTok
         THEN
            RETURN( LogicalDifferenceTok )
         END
      END
   END ;
   RETURN( Tok )
END CheckForLogicalOperator ;


(*
   CheckGenericNulSet - checks to see whether e1 is a generic nul set and if so it alters it
                        to the nul set of t2.
*)

PROCEDURE CheckGenericNulSet (e1: CARDINAL; VAR t1: CARDINAL; t2: CARDINAL) ;
VAR
   n1, n2: Name ;
BEGIN
   IF IsConstSet(e1)
   THEN
      IF NOT IsSet(t2)
      THEN
         n1 := GetSymName(t1) ;
         n2 := GetSymName(t2) ;
         WriteFormat2('incompatibility between a set constant of type (%a) and an object of type (%a)',
                      n1, n2)
      END ;
      PushValue(e1) ;
      IF IsGenericNulSet()
      THEN
         PopValue(e1) ;
         PushNulSet(t2) ;
         t1 := t2
      END ;
      PopValue(e1)
   END
END CheckGenericNulSet ;


(*
   CheckForGenericNulSet - if e1 or e2 is the generic nul set then
                           alter it to the nul set of the other operands type.
*)

PROCEDURE CheckForGenericNulSet (e1, e2: CARDINAL; VAR t1, t2: CARDINAL) ;
BEGIN
   IF t1#t2
   THEN
      CheckGenericNulSet(e1, t1, t2) ;
      CheckGenericNulSet(e2, t2, t1)
   END
END CheckForGenericNulSet ;


(*
   CheckDivModRem - initiates calls to check the divisor for DIV, MOD, REM
                    expressions.
*)

PROCEDURE CheckDivModRem (tok: Name; d, e: CARDINAL) ;
BEGIN
   IF tok=DivTok
   THEN
      BuildRange(InitWholeZeroDivisionCheck(d, e))
   ELSIF tok=ModTok
   THEN
      BuildRange(InitWholeZeroDivisionCheck(d, e))
   ELSIF tok=RemTok
   THEN
      BuildRange(InitWholeZeroRemainderCheck(d, e))
   END
END CheckDivModRem ;


(*
   doConvert - convert, sym, to a new symbol with, type.
               Return the new symbol.
*)

PROCEDURE doConvert (type: CARDINAL; sym: CARDINAL) : CARDINAL ;
BEGIN
   IF GetType(sym)#type
   THEN
      PushTF(Convert, NulSym) ;
      PushT(type) ;
      PushT(sym) ;
      PushT(2) ;          (* Two parameters *)
      BuildConvertFunction ;
      PopT(sym)
   END ;
   RETURN( sym )
END doConvert ;


(*
   BuildBinaryOp   - Builds a binary operation from the quad stack.
                     Be aware that this procedure will check for
                     the overloading of the bitset operators + - \ *.
                     So do NOT call this procedure if you are building
                     a reference to an array which has a bitset type or
                     the address arithmetic will be wrongly coersed into
                     logical ORs.
                     
                     The Stack is expected to contain:


                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | Sym1       |
                     |------------|
                     | Operator   |                          <- Ptr
                     |------------|          +------------+
                     | Sym2       |          | Temporary  |
                     |------------|          |------------|


                     Quadruples Produced

                     q     Operator  Temporary  Sym1  Sym2


                OR


                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | T1   | F1  |
                     |------------|
                     | OrTok      |                          <- Ptr
                     |------------|          +------------+
                     | T2   | F2  |          | T1+T2| F1  |
                     |------------|          |------------|


                     Quadruples Produced

*)

PROCEDURE BuildBinaryOp ;
BEGIN
   doBuildBinaryOp(TRUE, TRUE)
END BuildBinaryOp ;


(*
   doBuildBinaryOp - build the binary op, with or without type
                     checking.
*)

PROCEDURE doBuildBinaryOp (checkTypes, checkOverflow: BOOLEAN) ;
VAR
   s     : String ;
   NewTok,
   Tok   : Name ;
   r1, r2,
   t1, f1,
   t2, f2,
   e1, e2,
   t,
   ta    : CARDINAL ;
BEGIN
   Tok := OperandT(2) ;
   IF Tok=OrTok
   THEN
      CheckBooleanId ;
      PopBool(t1, f1) ;
      PopT(Tok) ;
      PopBool(t2, f2) ;
      Assert(f2=0) ;
      PushBool(Merge(t1, t2), f1)
   ELSIF (Tok=AndTok) OR (Tok=AmbersandTok)
   THEN
      CheckBooleanId ;
      PopBool(t1, f1) ;
      PopT(Tok) ;
      PopBool(t2, f2) ;
      Assert(t2=0) ;
      PushBool(t1, Merge(f1, f2))
   ELSE
      PopTFrw(e1, t1, r1) ;
      PopT(Tok) ;
      PopTFrw(e2, t2, r2) ;
      MarkAsRead(r1) ;
      MarkAsRead(r2) ;
      NewTok := CheckForLogicalOperator(Tok, e1, t1, e2, t2) ;
      IF NewTok=Tok
      THEN
         (*
            BinaryOps and UnaryOps only work with immediate and
            offset addressing.  This is fine for calculating
            array and record offsets but we need to get the real
            values to perform normal arithmetic. Not address
            arithmetic.

            However the set operators will dereference LValues
            (to optimize large set arithemetic)
         *)
         IF GetMode(e1)=LeftValue
         THEN
            t := MakeTemporary(RightValue) ;
            PutVar(t, t1) ;
            CheckPointerThroughNil(e1) ;
            doIndrX(t, e1) ;
            e1 := t
         END ;
         IF GetMode(e2)=LeftValue
         THEN
            t := MakeTemporary(RightValue) ;
            PutVar(t, t2) ;
            CheckPointerThroughNil(e2) ;
            doIndrX(t, e2) ;
            e2 := t
         END
      ELSE
         (* CheckForGenericNulSet(e1, e2, t1, t2) *)
      END ;
      IF (Tok=PlusTok) AND IsConstString(e1) AND IsConstString(e2)
      THEN
         (* handle special addition for constant strings *)
         s := InitStringCharStar(KeyToCharStar(GetString(e2))) ;
         s := ConCat(s, Mark(InitStringCharStar(KeyToCharStar(GetString(e1))))) ;
         t := MakeConstLitString(makekey(string(s))) ;
         s := KillString(s)
      ELSE
         IF checkTypes
         THEN
            CheckExpressionCompatible(t1, t2) ;
            IF CannotCheckTypeInPass3(e1) OR CannotCheckTypeInPass3(e2)
            THEN
               BuildRange(InitTypesExpressionCheck(e1, e2))
            END
         END ;
         t := MakeTemporaryFromExpressions(e1, e2, GetTokenNo(),
                                           AreConstant(IsConst(e1) AND IsConst(e2))) ;
         CheckDivModRem(NewTok, t, e1) ;
         GenQuadO(MakeOp(NewTok), t, e2, e1, checkOverflow)
      END ;
      PushTF(t, GetType(t))
   END
END doBuildBinaryOp ;


(*
   BuildUnaryOp   - Builds a unary operation from the quad stack.
                    The Stack is expected to contain:


                    Entry                   Exit
                    =====                   ====

             Ptr ->
                    +------------+
                    | Sym        |
                    |------------|          +------------+
                    | Operator   |          | Temporary  | <- Ptr
                    |------------|          |------------|


                    Quadruples Produced

                    q     Operator  Temporary  _ Sym

*)

PROCEDURE BuildUnaryOp ;
VAR
   Tok       : Name ;
   type,
   Sym,
   SymT, r, t: CARDINAL ;
BEGIN
   PopTrw(Sym, r) ;
   PopT(Tok) ;
   IF Tok=MinusTok
   THEN
      MarkAsRead(r) ;
      type := NegateType(GetType(Sym), GetTokenNo()) ;
      t := MakeTemporary(AreConstant(IsConst(Sym))) ;
      PutVar(t, type) ;

      (*
         variables must have a type and REAL/LONGREAL constants must
         be typed
      *)

      IF NOT IsConst(Sym)
      THEN
         IF (type#NulSym) AND IsSet(SkipType(type))
         THEN
            (* do not dereference set variables *)
         ELSIF GetMode(Sym)=LeftValue
         THEN
            (* dereference symbols which are not sets and which are variables *)

            SymT := MakeTemporary(RightValue) ;
            PutVar(SymT, GetType(Sym)) ;
            CheckPointerThroughNil(Sym) ;
            doIndrX(SymT, Sym) ;
            Sym := SymT
         END
      END ;
      GenQuad(NegateOp, t, NulSym, Sym) ;
      PushT(t)
   ELSIF Tok=PlusTok
   THEN
      PushTrw(Sym, r)
   ELSE
      WriteFormat1('not expecting this kind of unary operator (%a)', Tok)
   END
END BuildUnaryOp ;


(*
   AreConstant - returns immediate addressing mode if b is true else
                 offset mode is returned. b determines whether the
                 operands are all constant - in which case we can use
                 a constant temporary variable.
*)

PROCEDURE AreConstant (b: BOOLEAN) : ModeOfAddr ;
BEGIN
   IF b
   THEN
      RETURN( ImmediateValue )
   ELSE
      RETURN( RightValue )
   END
END AreConstant ;


(*
   ConvertBooleanToVariable - converts a BoolStack(i) from a Boolean True|False
                              exit pair into a variable containing the value TRUE or
                              FALSE. The parameter, i, is relative to the top
                              of the stack.
*)

PROCEDURE ConvertBooleanToVariable (i: CARDINAL) ;
VAR
   Des: CARDINAL ;
   f  : BoolFrame ;
BEGIN
   Assert(IsBoolean(i)) ;
   (*
      need to convert it to a variable containing the result.
      Des will be a boolean type
   *)
   Des := MakeTemporary(RightValue) ;
   PutVar(Des, Boolean) ;
   PushT(Des) ;   (* we have just increased the stack so we must use i+1 *)
   f := PeepAddress(BoolStack, i+1) ;
   PushBool(f^.TrueExit, f^.FalseExit) ;
   BuildAssignmentWithoutBounds(FALSE, TRUE) ;  (* restored stack *)
   f := PeepAddress(BoolStack, i) ;
   WITH f^ DO
      TrueExit := Des ;  (* alter Stack(i) to contain the variable *)
      FalseExit := Boolean ;
      BooleanOp := FALSE ; (* no longer a Boolean True|False pair    *)
      Unbounded := NulSym ;
      Dimension := 0 ;
      ReadWrite := NulSym
   END
END ConvertBooleanToVariable ;


(*
   BuildBooleanVariable - tests to see whether top of stack is a boolean
                          conditional and if so it converts it into a boolean
                          variable.
*)

PROCEDURE BuildBooleanVariable ;
BEGIN
   IF IsBoolean(1)
   THEN
      ConvertBooleanToVariable(1)
   END
END BuildBooleanVariable ;


(*
   BuildRelOpFromBoolean - builds a relational operator sequence of quadruples
                           instead of using a temporary boolean variable.
                           This function can only be used when we perform
                           the following translation:

                           (a=b) # (c=d)  alternatively   (a=b) = (c=d)
                                 ^                              ^

                           it only allows # = to be used as >= <= > < all
                           assume a particular value for TRUE and FALSE.
                           (In which case the user should specify ORD)


                           before

                           q    if r1      op1     op2     t2
                           q+1  Goto                       f2
                           q+2  if r2      op3     op4     t1
                           q+3  Goto                       f1

                           after (in case of =)

                           q    if r1      op1     op2     q+2
                           q+1  Goto                       q+4
                           q+2  if r2      op3     op4     t
                           q+3  Goto                       f
                           q+4  if r2      op3     op4     f
                           q+5  Goto                       t

                           after (in case of #)

                           q    if r1      op1     op2     q+2
                           q+1  Goto                       q+4
                           q+2  if r2      op3     op4     f
                           q+3  Goto                       t
                           q+4  if r2      op3     op4     t
                           q+5  Goto                       f

                           The Stack is expected to contain:


                           Entry                   Exit
                           =====                   ====

                    Ptr ->
                           +------------+
                           | t1 | f1    |
                           |------------|
                           | Operator   |                          <- Ptr
                           |------------|          +------------+
                           | t2 | f2    |          | t    | f   |
                           |------------|          |------------|


*)

PROCEDURE BuildRelOpFromBoolean ;
VAR
   Tok,
   t1, f1,
   t2, f2: CARDINAL ;
   f     : QuadFrame ;
BEGIN
   Assert(IsBoolean(1) AND IsBoolean(3)) ;
   IF OperandT(2)=EqualTok
   THEN
      (* are the two boolean expressions the same? *)
      PopBool(t1, f1) ;
      PopT(Tok) ;
      PopBool(t2, f2) ;
      (* give the false exit a second chance *)
      BackPatch(t2, t1) ;        (* q    if   _     _    q+2 *)
      BackPatch(f2, NextQuad) ;  (* q+1  if   _     _    q+4 *)
      Assert(NextQuad=f1+1) ;
      f := GetQF(t1) ;
      WITH f^ DO
         GenQuad(Operator, Operand1, Operand2, 0)
      END ;
      GenQuad(GotoOp, NulSym, NulSym, 0) ;
      PushBool(Merge(NextQuad-1, t1), Merge(NextQuad-2, f1))
   ELSIF (OperandT(2)=HashTok) OR (OperandT(2)=LessGreaterTok)
   THEN
      (* are the two boolean expressions the different? *)
      PopBool(t1, f1) ;
      PopT(Tok) ;
      PopBool(t2, f2) ;
      (* give the false exit a second chance *)
      BackPatch(t2, t1) ;        (* q    if   _     _    q+2 *)
      BackPatch(f2, NextQuad) ;  (* q+1  if   _     _    q+4 *)
      Assert(NextQuad=f1+1) ;
      f := GetQF(t1) ;
      WITH f^ DO
         GenQuad(Operator, Operand1, Operand2, 0)
      END ;
      GenQuad(GotoOp, NulSym, NulSym, 0) ;
      PushBool(Merge(NextQuad-2, f1), Merge(NextQuad-1, t1))
   ELSE
      WriteFormat0('only allowed to use relation operators = # on BOOLEAN expressions as these do not imply a value for TRUE or FALSE')
   END
END BuildRelOpFromBoolean ;


(*
   CheckVariableOrConstantOrProcedure - checks to make sure sym is a variable, constant or procedure.
*)

PROCEDURE CheckVariableOrConstantOrProcedure (sym: CARDINAL) ;
VAR
   s   : String ;
   type: CARDINAL ;
BEGIN
   type := GetType(sym) ;
   IF IsUnknown(sym)
   THEN
      MetaError1('{%1Uad} has not been declared', sym)
   ELSIF (NOT IsConst(sym)) AND (NOT IsVar(sym)) AND
         (NOT IsProcedure(sym)) AND
         (NOT IsTemporary(sym)) AND (NOT MustNotCheckBounds)
   THEN
      MetaErrors1('{%1ad} expected a variable, procedure, constant or expression',
                  'and it was declared as a {%1Dd}', sym) ;
   ELSIF (type#NulSym) AND IsArray(type)
   THEN
      MetaErrors1('{%1U} not expecting an array variable as an operand for either comparison or binary operation',
                 'it was declared as a {%1Dd}', sym)
   ELSIF IsConstString(sym) AND (GetStringLength(sym)>1)
   THEN
      MetaError1('{%1U} not expecting a string constant as an operand for either comparison or binary operation',
                 sym)
   END
END CheckVariableOrConstantOrProcedure ;


(*
   CheckInCompatible - checks to see that t1 IN t2 is type legal.
*)

PROCEDURE CheckInCompatible (Op: Name; t1, t2: CARDINAL) : CARDINAL ;
VAR
   s: String ;
BEGIN
   IF Op=InTok
   THEN
      t2 := SkipType(t2) ;
      IF IsSet(t2)
      THEN
         RETURN( GetType(t2) )
      ELSE
         s := Mark(InitStringCharStar(KeyToCharStar(GetSymName(t2)))) ;
         ErrorStringAt2(Sprintf1(Mark(InitString('expect a set type as the right hand operand to the IN operator, type name is (%s)')),
                                 s),
                        GetTokenNo(), GetDeclared(t2)) ;
         RETURN( t1 )
      END
   ELSE
      RETURN( t2 )
   END
END CheckInCompatible ;


(*
   BuildRelOp   - Builds a relative operation from the quad stack.
                  The Stack is expected to contain:


                  Entry                   Exit
                  =====                   ====

           Ptr ->
                  +------------+
                  | e1         |
                  |------------|                          <- Ptr
                  | Operator   |
                  |------------|          +------------+
                  | e2         |          | t    | f   |
                  |------------|          |------------|


                    Quadruples Produced

                    q     IFOperator  e2  e1  TrueExit    ; e2  e1 since
                    q+1   GotoOp              FalseExit   ; relation > etc
                                                          ; requires order.
*)

PROCEDURE BuildRelOp ;
VAR
   Op    : Name ;
   t,
   t1, t2,
   e1, e2: CARDINAL ;
BEGIN
   IF CompilerDebugging
   THEN
      DisplayStack    (* Debugging info *)
   END ;
   IF IsBoolean(1) AND IsBoolean(3)
   THEN
      (*
         we allow # and = to be used with Boolean expressions.
         we do not allow >  <  >=  <=  though
      *)
      BuildRelOpFromBoolean
   ELSE
      IF IsBoolean(1)
      THEN
         ConvertBooleanToVariable(1)
      END ;
      IF IsBoolean(3)
      THEN
         ConvertBooleanToVariable(3)
      END ;
      PopTF(e1, t1) ;
      PopT(Op) ;
      PopTF(e2, t2) ;

      CheckVariableOrConstantOrProcedure(e1) ;
      CheckVariableOrConstantOrProcedure(e2) ;

      IF (Op=EqualTok) OR (Op=HashTok) OR (Op=LessGreaterTok)
      THEN
         CheckAssignmentCompatible(t1, t2)
      ELSE
         IF IsConstructor(e1) OR IsConstSet(e1)
         THEN
            (* ignore type checking for now *)
         ELSE
            t1 := CheckInCompatible(Op, t2, t1) ;
            CheckExpressionCompatible(t1, t2)
         END
      END ;

      (* must dereference LeftValue operands *)
      IF GetMode(e1)=LeftValue
      THEN
         t := MakeTemporary(RightValue) ;
         PutVar(t, GetType(e1)) ;
         CheckPointerThroughNil(e1) ;
         doIndrX(t, e1) ;
         e1 := t
      END ;
      IF GetMode(e2)=LeftValue
      THEN
         t := MakeTemporary(RightValue) ;
         PutVar(t, GetType(e2)) ;
         CheckPointerThroughNil(e2) ;
         doIndrX(t, e2) ;
         e2 := t
      END ;
      GenQuad(MakeOp(Op), e2, e1, 0) ;      (* True  Exit *)
      GenQuad(GotoOp, NulSym, NulSym, 0) ;  (* False Exit *)
      PushBool(NextQuad-2, NextQuad-1)
   END
END BuildRelOp ;


(*
   BuildNot   - Builds a NOT operation from the quad stack.
                The Stack is expected to contain:


                  Entry                   Exit
                  =====                   ====

           Ptr ->                                        <- Ptr
                  +------------+          +------------+
                  | t    | f   |          | f    | t   |
                  |------------|          |------------|
*)

PROCEDURE BuildNot ;
VAR
   t, f: CARDINAL ;
BEGIN
   CheckBooleanId ;
   PopBool(t, f) ;
   PushBool(f, t)
END BuildNot ;


(*
   MakeOp - returns the equalent quadruple operator to a token, t.
*)

PROCEDURE MakeOp (t: Name) : QuadOperator ;
BEGIN
   IF t=PlusTok
   THEN
      RETURN( AddOp )
   ELSIF t=MinusTok
   THEN
      RETURN( SubOp )
   ELSIF t=DivTok
   THEN
      IF (Pim2 OR Pim3) AND (NOT PositiveModFloorDiv)
      THEN
         RETURN( DivTruncOp )
      ELSE
         RETURN( DivFloorOp )
      END
   ELSIF t=DivideTok
   THEN
      RETURN( DivTruncOp )
   ELSIF t=RemTok
   THEN
      RETURN( ModTruncOp )
   ELSIF t=ModTok
   THEN
      IF (Pim2 OR Pim3) AND (NOT PositiveModFloorDiv)
      THEN
         RETURN( ModTruncOp )
      ELSE
         RETURN( ModFloorOp )
      END
   ELSIF t=TimesTok
   THEN
      RETURN( MultOp )
   ELSIF t=HashTok
   THEN
      RETURN( IfNotEquOp )
   ELSIF t=LessGreaterTok
   THEN
      RETURN( IfNotEquOp )
   ELSIF t=GreaterEqualTok
   THEN
      RETURN( IfGreEquOp )
   ELSIF t=LessEqualTok
   THEN
      RETURN( IfLessEquOp )
   ELSIF t=EqualTok
   THEN
      RETURN( IfEquOp )
   ELSIF t=LessTok
   THEN
      RETURN( IfLessOp )
   ELSIF t=GreaterTok
   THEN
      RETURN( IfGreOp )
   ELSIF t=InTok
   THEN
      RETURN( IfInOp )
   ELSIF t=LogicalOrTok
   THEN
      RETURN( LogicalOrOp )
   ELSIF t=LogicalAndTok
   THEN
      RETURN( LogicalAndOp )
   ELSIF t=LogicalXorTok
   THEN
      RETURN( LogicalXorOp )
   ELSIF t=LogicalDifferenceTok
   THEN
      RETURN( LogicalDiffOp )
   ELSE
      InternalError('binary operation not implemented yet',
                    __FILE__, __LINE__)
   END
END MakeOp ;


(*
   GenQuadO - generate a quadruple with Operation, Op1, Op2, Op3, overflow.
*)

PROCEDURE GenQuadO (Operation: QuadOperator;
                    Op1, Op2, Op3: CARDINAL; overflow: BOOLEAN) ;
VAR
   f: QuadFrame ;
BEGIN
   (* WriteString('Potential Quad: ') ; *)
   IF QuadrupleGeneration
   THEN
      IF NextQuad#Head
      THEN
         f := GetQF(NextQuad-1) ;
         f^.Next := NextQuad
      END ;
      PutQuadO(NextQuad, Operation, Op1, Op2, Op3, overflow) ;
      f := GetQF(NextQuad) ;
      WITH f^ DO
         Next := 0 ;
         LineNo := GetLineNo() ;
         TokenNo := GetTokenNo()
      END ;
      IF NextQuad=BreakAtQuad
      THEN
         stop
      END ;
      (* DisplayQuad(NextQuad) ; *)
      NewQuad(NextQuad)
   END
END GenQuadO ;


(*
   GenQuad - Generate a quadruple with Operation, Op1, Op2, Op3.
*)

PROCEDURE GenQuad (Operation: QuadOperator;
                   Op1, Op2, Op3: CARDINAL) ;
BEGIN
   GenQuadO(Operation, Op1, Op2, Op3, TRUE)
END GenQuad ;


(*
   DisplayQuadList - displays all quads.
*)

PROCEDURE DisplayQuadList ;
VAR
   i: CARDINAL ;
   f: QuadFrame ;
BEGIN
   printf0('Quadruples:\n') ;
   i := Head ;
   WHILE i#0 DO
      DisplayQuad(i) ;
      f := GetQF(i) ;
      i := f^.Next
   END
END DisplayQuadList ;


(*
   DisplayQuadRange - displays all quads in list range, start..end.
*)

PROCEDURE DisplayQuadRange (start, end: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   printf0('Quadruples:\n') ;
   WHILE (start<=end) AND (start#0) DO
      DisplayQuad(start) ;
      f := GetQF(start) ;
      start := f^.Next
   END
END DisplayQuadRange ;


(*
   BackPatch - Makes each of the quadruples on the list pointed to by
               StartQuad, take quadruple Value as a target.
*)

PROCEDURE BackPatch (QuadNo, Value: CARDINAL) ;
VAR
   i: CARDINAL ;
   f: QuadFrame ;
BEGIN
   IF QuadrupleGeneration
   THEN
      WHILE QuadNo#0 DO
         f := GetQF(QuadNo) ;
         WITH f^ DO
            i := Operand3 ;                       (* Next Link along the BackPatch *)
            ManipulateReference(QuadNo, Value)    (* Filling in the BackPatch.     *)
         END ;
         QuadNo := i
      END
   END
END BackPatch ;


(*
   Merge - joins two quad lists, QuadList2 to the end of QuadList1.
           A QuadList of value zero is a nul list.
*)

PROCEDURE Merge (QuadList1, QuadList2: CARDINAL) : CARDINAL ;
VAR
   i, j: CARDINAL ;
   f   : QuadFrame ;
BEGIN
   IF QuadList1=0
   THEN
      RETURN( QuadList2 )
   ELSIF QuadList2=0
   THEN
      RETURN( QuadList1 )
   ELSE
      i := QuadList1 ;
      REPEAT
         j := i ;
         f := GetQF(i) ;
         i := f^.Operand3
      UNTIL i=0 ;
      ManipulateReference(j, QuadList2) ;
      RETURN( QuadList1 )
   END
END Merge ;


(*
   DisplayStack - displays the compile time symbol stack.
*)

PROCEDURE DisplayStack ;
VAR
   i, n: CARDINAL ;
   f   : BoolFrame ;
BEGIN
   printf0('+-------------------+\n') ;
   n := NoOfItemsInStackAddress(BoolStack) ;
   i := 1 ;
   WHILE i<=n DO
      f := PeepAddress(BoolStack, i) ;
      WITH f^ DO
         IF BooleanOp
         THEN
            printf2('| Q%6d | Q%6d |\n', TrueExit, FalseExit) ;
            printf0('|---------|---------|\n')
         ELSE
            printf1('| %6d            |\n', TrueExit) ;
            printf0('|-------------------|\n')
         END
      END ;
      INC(i)
   END
END DisplayStack ;


(*
   DisplayQuad - displays a quadruple, QuadNo.
*)

PROCEDURE DisplayQuad (QuadNo: CARDINAL) ;
BEGIN
   DSdbEnter ;
   printf1('%4d  ', QuadNo) ; WriteQuad(QuadNo) ; printf0('\n') ;
   DSdbExit(NIL)
END DisplayQuad ;


(*
   WriteQuad - Writes out the Quad BufferQuad.
*)

PROCEDURE WriteQuad (BufferQuad: CARDINAL) ;
VAR
   n1, n2: Name ;
   f     : QuadFrame ;
BEGIN
   f := GetQF(BufferQuad) ;
   WITH f^ DO
      WriteOperator(Operator) ;
      printf1('  [%d]    ', NoOfTimesReferenced) ;
      CASE Operator OF

      HighOp           : WriteOperand(Operand1) ;
                         printf1('  %4d  ', Operand2) ;
                         WriteOperand(Operand3) |
      SavePriorityOp,
      RestorePriorityOp,
      SubrangeLowOp,
      SubrangeHighOp,
      BecomesOp,
      InclOp,
      ExclOp,
      UnboundedOp,
      ReturnValueOp,
      FunctValueOp,
      NegateOp,
      AddrOp            : WriteOperand(Operand1) ;
                          printf0('  ') ;
                          WriteOperand(Operand3) |
      ElementSizeOp,
      IfInOp,
      IfNotInOp,
      IfNotEquOp,
      IfEquOp,
      IfLessOp,
      IfGreOp,
      IfLessEquOp,
      IfGreEquOp        : WriteOperand(Operand1) ;
                          printf0('  ') ;
                          WriteOperand(Operand2) ;
                          printf1('  %4d', Operand3) |

      InlineOp,
      RetryOp,
      TryOp,
      GotoOp            : printf1('%4d', Operand3) |

      LineNumberOp      : printf2('%a:%d', Operand1, Operand3) |

      EndFileOp         : n1 := GetSymName(Operand3) ;
                          printf1('%a', n1) |

      ThrowOp,
      ReturnOp,
      CallOp,
      KillLocalVarOp    : WriteOperand(Operand3) |

      ProcedureScopeOp,
      NewLocalVarOp,
      FinallyStartOp,
      FinallyEndOp,
      InitEndOp,
      InitStartOp       : n1 := GetSymName(Operand2) ;
                          n2 := GetSymName(Operand3) ;
                          printf3('  %4d  %a  %a', Operand1, n1, n2) |

      ModuleScopeOp,
      StartModFileOp    : n1 := GetSymName(Operand3) ;
                          printf4('%a:%d  %a(%d)', Operand2, Operand1, n1, Operand3) |

      StartDefFileOp    : n1 := GetSymName(Operand3) ;
                          printf2('  %4d  %a', Operand1, n1) |

      OptParamOp,
      ParamOp           : printf1('%4d  ', Operand1) ;
                          WriteOperand(Operand2) ;
                          printf0('  ') ;
                          WriteOperand(Operand3) |
      SizeOp,
      OffsetOp,
      IndrXOp,
      XIndrOp,
      ArrayOp,
      LogicalShiftOp,
      LogicalRotateOp,
      LogicalOrOp,
      LogicalAndOp,
      LogicalXorOp,
      LogicalDiffOp,
      CoerceOp,
      ConvertOp,
      CastOp,
      AddOp,
      SubOp,
      MultOp,
      ModFloorOp,
      DivFloorOp,
      ModTruncOp,
      DivTruncOp        : WriteOperand(Operand1) ;
                          printf0('  ') ;
                          WriteOperand(Operand2) ;
                          printf0('  ') ;
                          WriteOperand(Operand3) |
      DummyOp,
      CodeOnOp,
      CodeOffOp,
      ProfileOnOp,
      ProfileOffOp,
      OptimizeOnOp,
      OptimizeOffOp     : |
      BuiltinConstOp    : WriteOperand(Operand1) ;
                          printf1('   %a', Operand3) |
      BuiltinTypeInfoOp : WriteOperand(Operand1) ;
                          printf1('   %a', Operand2) ;
                          printf1('   %a', Operand3) |
      StandardFunctionOp: WriteOperand(Operand1) ;
                          printf0('  ') ;
                          WriteOperand(Operand2) ;
                          printf0('  ') ;
                          WriteOperand(Operand3) |
      CatchBeginOp,
      CatchEndOp        : |

      RangeCheckOp,
      ErrorOp           : WriteRangeCheck(Operand3) |
      SaveExceptionOp,
      RestoreExceptionOp: WriteOperand(Operand1) ;
                          printf0('  ') ;
                          WriteOperand(Operand3)

      ELSE
         InternalError('quadruple not recognised', __FILE__, __LINE__)
      END
   END
END WriteQuad ;


(*
   WriteOperator - writes the name of the quadruple operator.
*)

PROCEDURE WriteOperator (Operator: QuadOperator) ;
BEGIN
   CASE Operator OF

   LogicalOrOp              : printf0('Or                ') |
   LogicalAndOp             : printf0('And               ') |
   LogicalXorOp             : printf0('Xor               ') |
   LogicalDiffOp            : printf0('Ldiff             ') |
   LogicalShiftOp           : printf0('Shift             ') |
   LogicalRotateOp          : printf0('Rotate            ') |
   BecomesOp                : printf0('Becomes           ') |
   IndrXOp                  : printf0('IndrX             ') |
   XIndrOp                  : printf0('XIndr             ') |
   ArrayOp                  : printf0('Array             ') |
   ElementSizeOp            : printf0('ElementSize       ') |
   AddrOp                   : printf0('Addr              ') |
   SizeOp                   : printf0('Size              ') |
   OffsetOp                 : printf0('Offset            ') |
   IfInOp                   : printf0('If IN             ') |
   IfNotInOp                : printf0('If NOT IN         ') |
   IfNotEquOp               : printf0('If <>             ') |
   IfEquOp                  : printf0('If =              ') |
   IfLessEquOp              : printf0('If <=             ') |
   IfGreEquOp               : printf0('If >=             ') |
   IfGreOp                  : printf0('If >              ') |
   IfLessOp                 : printf0('If <              ') |
   GotoOp                   : printf0('Goto              ') |
   DummyOp                  : printf0('Dummy             ') |
   ModuleScopeOp            : printf0('ModuleScopeOp     ') |
   StartDefFileOp           : printf0('StartDefFile      ') |
   StartModFileOp           : printf0('StartModFile      ') |
   EndFileOp                : printf0('EndFileOp         ') |
   InitStartOp              : printf0('InitStart         ') |
   InitEndOp                : printf0('InitEnd           ') |
   FinallyStartOp           : printf0('FinallyStart      ') |
   FinallyEndOp             : printf0('FinallyEnd        ') |
   RetryOp                  : printf0('Retry             ') |
   TryOp                    : printf0('Try               ') |
   ThrowOp                  : printf0('Throw             ') |
   CatchBeginOp             : printf0('CatchBegin        ') |
   CatchEndOp               : printf0('CatchEnd          ') |
   AddOp                    : printf0('+                 ') |
   SubOp                    : printf0('-                 ') |
   DivFloorOp               : printf0('DIV floor         ') |
   ModFloorOp               : printf0('MOD floor         ') |
   DivTruncOp               : printf0('DIV trunc         ') |
   ModTruncOp               : printf0('MOD trunc         ') |
   MultOp                   : printf0('*                 ') |
   NegateOp                 : printf0('Negate            ') |
   InclOp                   : printf0('Incl              ') |
   ExclOp                   : printf0('Excl              ') |
   ReturnOp                 : printf0('Return            ') |
   ReturnValueOp            : printf0('ReturnValue       ') |
   FunctValueOp             : printf0('FunctValue        ') |
   CallOp                   : printf0('Call              ') |
   ParamOp                  : printf0('Param             ') |
   OptParamOp               : printf0('OptParam          ') |
   NewLocalVarOp            : printf0('NewLocalVar       ') |
   KillLocalVarOp           : printf0('KillLocalVar      ') |
   ProcedureScopeOp         : printf0('ProcedureScope    ') |
   UnboundedOp              : printf0('Unbounded         ') |
   CoerceOp                 : printf0('Coerce            ') |
   ConvertOp                : printf0('Convert           ') |
   CastOp                   : printf0('Cast              ') |
   HighOp                   : printf0('High              ') |
   CodeOnOp                 : printf0('CodeOn            ') |
   CodeOffOp                : printf0('CodeOff           ') |
   ProfileOnOp              : printf0('ProfileOn         ') |
   ProfileOffOp             : printf0('ProfileOff        ') |
   OptimizeOnOp             : printf0('OptimizeOn        ') |
   OptimizeOffOp            : printf0('OptimizeOff       ') |
   InlineOp                 : printf0('Inline            ') |
   LineNumberOp             : printf0('LineNumber        ') |
   BuiltinConstOp           : printf0('BuiltinConst      ') |
   BuiltinTypeInfoOp        : printf0('BuiltinTypeInfo   ') |
   StandardFunctionOp       : printf0('StandardFunction  ') |
   SavePriorityOp           : printf0('SavePriority      ') |
   RestorePriorityOp        : printf0('RestorePriority   ') |
   RangeCheckOp             : printf0('RangeCheck        ') |
   ErrorOp                  : printf0('Error             ') |
   SaveExceptionOp          : printf0('SaveException     ') |
   RestoreExceptionOp       : printf0('RestoreException  ')

   ELSE
      InternalError('operator not expected', __FILE__, __LINE__)
   END
END WriteOperator ;


(*
   WriteOperand - displays the operands name, symbol id and mode of addressing.
*)

PROCEDURE WriteOperand (Sym: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   IF Sym=NulSym
   THEN
      printf0('<nulsym>')
   ELSE
      n := GetSymName(Sym) ;
      printf1('%a', n) ;
      IF IsVar(Sym) OR IsConst(Sym)
      THEN
         printf0('[') ; WriteMode(GetMode(Sym)) ; printf0(']')
      END ;
      printf1('(%d)', Sym)
   END
END WriteOperand ;


PROCEDURE WriteMode (Mode: ModeOfAddr) ;
BEGIN
   CASE Mode OF

   ImmediateValue: printf0('i') |
   NoValue       : printf0('n') |
   RightValue    : printf0('r') |
   LeftValue     : printf0('l')

   ELSE
      InternalError('unrecognised mode', __FILE__, __LINE__)
   END
END WriteMode ;


(*
   PushExit - pushes the exit value onto the EXIT stack.
*)

PROCEDURE PushExit (Exit: CARDINAL) ;
BEGIN
   PushWord(ExitStack, Exit)
END PushExit ;


(*
   PopExit - pops the exit value from the EXIT stack.
*)

PROCEDURE PopExit() : WORD ;
BEGIN
   RETURN( PopWord(ExitStack) )
END PopExit ;


(*
   PushFor - pushes the exit value onto the FOR stack.
*)

PROCEDURE PushFor (Exit: CARDINAL) ;
BEGIN
   PushWord(ForStack, Exit)
END PushFor ;


(*
   PopFor - pops the exit value from the FOR stack.
*)

PROCEDURE PopFor() : WORD ;
BEGIN
   RETURN( PopWord(ForStack) )
END PopFor ;


(*
   DumpStack - display the expression stack.
*)

PROCEDURE DumpStack ;
VAR
   o1, o2 : WORD ;
   i, n, v: CARDINAL ;
BEGIN
   IF DebugStack
   THEN
      n := NoOfItemsInStackAddress(BoolStack) ;
      i := n ;
      printf0('quad stack: ') ;
      WHILE i>0 DO
         v := n-i+1 ;
         IF IsBoolean(i)
         THEN
            printf1('%d {boolean}  ', v)
         ELSE
            o1 := OperandT(i) ;
            o2 := OperandF(i) ;
            printf3('%d [%d, %d]  ', v, o1, o2)
         END ;
         DEC(i)
      END ;
      printf0('\n')
   END
END DumpStack ;


(*
   PopBool - Pops a True and a False exit quad number from the True/False
             stack.
*)

PROCEDURE PopBool (VAR True, False: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      Assert(BooleanOp)
   END ;
   DISPOSE(f)
END PopBool ;


(*
   PushBool - Push a True and a False exit quad numbers onto the
              True/False stack.
*)

PROCEDURE PushBool (True, False: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(True<=NextQuad) ;
   Assert(False<=NextQuad) ;
   NEW(f) ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      BooleanOp := TRUE
   END ;
   PushAddress(BoolStack, f)
END PushBool ;


(*
   IsBoolean - returns true is the Stack position pos contains a Boolean
               Exit. False is returned if an Ident is stored.
*)

PROCEDURE IsBoolean (pos: CARDINAL) : BOOLEAN ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   f := PeepAddress(BoolStack, pos) ;
   RETURN( f^.BooleanOp )
END IsBoolean ;


(*
   OperandD - returns possible array dimension associated with the ident
              operand stored on the boolean stack.
*)

PROCEDURE OperandD (pos: CARDINAL) : WORD ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   Assert(NOT IsBoolean(pos)) ;
   f := PeepAddress(BoolStack, pos) ;
   RETURN( f^.Dimension )
END OperandD ;


(*
   OperandA - returns possible array symbol associated with the ident
              operand stored on the boolean stack.
*)

PROCEDURE OperandA (pos: CARDINAL) : WORD ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   Assert(NOT IsBoolean(pos)) ;
   f := PeepAddress(BoolStack, pos) ;
   RETURN( f^.Unbounded )
END OperandA ;


(*
   OperandT - returns the ident operand stored in the true position on the boolean stack.
*)

PROCEDURE OperandT (pos: CARDINAL) : WORD ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   Assert(NOT IsBoolean(pos)) ;
   f := PeepAddress(BoolStack, pos) ;
   RETURN( f^.TrueExit )
END OperandT ;


(*
   OperandF - returns the ident operand stored in the false position on the boolean stack.
*)

PROCEDURE OperandF (pos: CARDINAL) : WORD ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   Assert(NOT IsBoolean(pos)) ;
   f := PeepAddress(BoolStack, pos) ;
   RETURN( f^.FalseExit )
END OperandF ;


(*
   OperandRW - returns the rw operand stored on the boolean stack.
*)

PROCEDURE OperandRW (pos: CARDINAL) : WORD ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   Assert(NOT IsBoolean(pos)) ;
   f := PeepAddress(BoolStack, pos) ;
   RETURN( f^.ReadWrite )
END OperandRW ;


(*
   OperandMergeRW - returns the rw operand if not NulSym else it
                    returns True.
*)

PROCEDURE OperandMergeRW (pos: CARDINAL) : WORD ;
BEGIN
   IF OperandRW(pos)=NulSym
   THEN
      RETURN( OperandT(pos) )
   ELSE
      RETURN( OperandRW(pos) )
   END
END OperandMergeRW ;


(*
   BuildCodeOn - generates a quadruple declaring that code should be
                 emmitted from henceforth.

                 The Stack is unnaffected.
*)

PROCEDURE BuildCodeOn ;
BEGIN
   GenQuad(CodeOnOp, NulSym, NulSym, NulSym)
END BuildCodeOn ;


(*
   BuildCodeOff - generates a quadruple declaring that code should not be
                  emmitted from henceforth.

                  The Stack is unnaffected.
*)

PROCEDURE BuildCodeOff ;
BEGIN
   GenQuad(CodeOffOp, NulSym, NulSym, NulSym)
END BuildCodeOff ;


(*
   BuildProfileOn - generates a quadruple declaring that profile timings
                    should be emmitted from henceforth.

                    The Stack is unnaffected.
*)

PROCEDURE BuildProfileOn ;
BEGIN
   GenQuad(ProfileOnOp, NulSym, NulSym, NulSym)
END BuildProfileOn ;


(*
   BuildProfileOn - generates a quadruple declaring that profile timings
                    should be emmitted from henceforth.

                    The Stack is unnaffected.
*)

PROCEDURE BuildProfileOff ;
BEGIN
   GenQuad(ProfileOffOp, NulSym, NulSym, NulSym)
END BuildProfileOff ;


(*
   BuildOptimizeOn - generates a quadruple declaring that optimization
                     should occur from henceforth.

                     The Stack is unnaffected.
*)

PROCEDURE BuildOptimizeOn ;
BEGIN
   GenQuad(OptimizeOnOp, NulSym, NulSym, NulSym)
END BuildOptimizeOn ;


(*
   BuildOptimizeOff - generates a quadruple declaring that optimization
                      should not occur from henceforth.

                      The Stack is unnaffected.
*)

PROCEDURE BuildOptimizeOff ;
BEGIN
   GenQuad(OptimizeOffOp, NulSym, NulSym, NulSym)
END BuildOptimizeOff ;


(*
   BuildInline - builds an Inline pseudo quadruple operator.
                 The inline interface, Sym, is stored as the operand
                 to the operator InlineOp.

                 The stack is expected to contain:


                        Entry                   Exit
                        =====                   ====
  
                 Ptr ->
                        +--------------+
                        | Sym          |        Empty
                        |--------------|
*)

PROCEDURE BuildInline ;
VAR
   Sym: CARDINAL ;
BEGIN
   PopT(Sym) ;
   GenQuad(InlineOp, NulSym, NulSym, Sym)
END BuildInline ;


(*
   BuildLineNo - builds a LineNumberOp pseudo quadruple operator.
                 This quadruple indicates which source line has been
                 processed, these quadruples are only generated if we
                 are producing runtime debugging information.

                 The stack is not affected, read or altered in any way.


                        Entry                   Exit
                        =====                   ====
  
                 Ptr ->                              <- Ptr
*)

PROCEDURE BuildLineNo ;
VAR
   filename: Name ;
   f       : QuadFrame ;
BEGIN
   IF (NextQuad#Head) AND (GenerateLineDebug OR GenerateDebugging)
   THEN
      filename := makekey(string(GetFileName())) ;
      f := GetQF(NextQuad-1) ;
      IF (f^.Operator=LineNumberOp) AND (f^.Operand1=WORD(filename))
      THEN
         (* do nothing *)
      ELSE
         GenQuad(LineNumberOp, WORD(filename), NulSym, WORD(GetLineNo()))
      END
   END
END BuildLineNo ;


(*
   UseLineNote - uses the line note and returns it to the free list.
*)

PROCEDURE UseLineNote (l: LineNote) ;
VAR
   f: QuadFrame ;
BEGIN
   WITH l^ DO
      f := GetQF(NextQuad-1) ;
      IF (f^.Operator=LineNumberOp) AND (f^.Operand1=WORD(File))
      THEN
         (* do nothing *)
      ELSE
         GenQuad(LineNumberOp, WORD(File), NulSym, WORD(Line))
      END ;
      Next := FreeLineList
   END ;
   FreeLineList := l
END UseLineNote ;


(*
   PopLineNo - pops a line note from the line stack.
*)

PROCEDURE PopLineNo () : LineNote ;
VAR
   l: LineNote ;
BEGIN
   l := PopAddress(LineStack) ;
   IF l=NIL
   THEN
      InternalError('no line note available', __FILE__, __LINE__)
   END ;
   RETURN( l  )
END PopLineNo ;


(*
   InitLineNote - creates a line note and initializes it to
                  contain, file, line.
*)

PROCEDURE InitLineNote (file: Name; line: CARDINAL) : LineNote ;
VAR
   l: LineNote ;
BEGIN
   IF FreeLineList=NIL
   THEN
      NEW(l)
   ELSE
      l := FreeLineList ;
      FreeLineList := FreeLineList^.Next
   END ;
   WITH l^ DO
      File := file ;
      Line := line
   END ;
   RETURN( l )
END InitLineNote ;


(*
   PushLineNote - 
*)

PROCEDURE PushLineNote (l: LineNote) ;
BEGIN
   PushAddress(LineStack, l)   
END PushLineNote ;


(*
   PushLineNo - pushes the current file and line number to the stack.
*)

PROCEDURE PushLineNo ;
BEGIN
   PushLineNote(InitLineNote(makekey(string(GetFileName())), GetLineNo()))
END PushLineNo ;


(*
   AddRecordToList - adds the record held on the top of stack to the
                     list of records and varient fields.
*)

PROCEDURE AddRecordToList ;
VAR
   r: CARDINAL ;
   n: CARDINAL ;
BEGIN
   r := OperandT(1) ;
   Assert(IsRecord(r) OR IsFieldVarient(r)) ;
   (*
      r might be a field varient if the declaration consists of nested
      varients.  However ISO TSIZE can only utilise record types, we store
      a varient field anyway as the next pass would not know whether to
      ignore a varient field.
   *)
   PutItemIntoList(VarientFields, r) ;
   IF DebugVarients
   THEN
      n := NoOfItemsInList(VarientFields) ;
      IF IsRecord(r)
      THEN
         printf2('in list: record %d is %d\n', n, r)
      ELSE
         printf2('in list: varient field %d is %d\n', n, r)
      END
   END
END AddRecordToList ;


(*
   AddVarientToList - adds varient held on the top of stack to the list.
*)

PROCEDURE AddVarientToList ;
VAR
   v, n: CARDINAL ;
BEGIN
   v := OperandT(1) ;
   Assert(IsVarient(v)) ;
   PutItemIntoList(VarientFields, v) ;
   IF DebugVarients
   THEN
      n := NoOfItemsInList(VarientFields) ;
      printf2('in list: varient %d is %d\n', n, v)
   END
END AddVarientToList ;


(*
   AddVarientFieldToList - adds varient field, f, to the list of all varient
                           fields created.
*)

PROCEDURE AddVarientFieldToList (f: CARDINAL) ;
VAR
   n: CARDINAL ;
BEGIN
   Assert(IsFieldVarient(f)) ;
   PutItemIntoList(VarientFields, f) ;
   IF DebugVarients
   THEN
      n := NoOfItemsInList(VarientFields) ;
      printf2('in list: varient field %d is %d\n', n, f)
   END
END AddVarientFieldToList ;


(*
   GetRecordOrField - 
*)

PROCEDURE GetRecordOrField () : CARDINAL ;
VAR
   f: CARDINAL ;
BEGIN
   INC(VarientFieldNo) ;
   f := GetItemFromList(VarientFields, VarientFieldNo) ;
   IF DebugVarients
   THEN
      IF IsRecord(f)
      THEN
         printf2('out list: record %d is %d\n', VarientFieldNo, f)
      ELSE
         printf2('out list: varient field %d is %d\n', VarientFieldNo, f)
      END
   END ;
   RETURN( f )
END GetRecordOrField ;


(*
   BeginVarient - begin a varient record.
*)

PROCEDURE BeginVarient ;
VAR
   r, v: CARDINAL ;
BEGIN
   r := GetRecordOrField() ;
   Assert(IsRecord(r) OR IsFieldVarient(r)) ;
   v := GetRecordOrField() ;
   Assert(IsVarient(v)) ;
   BuildRange(InitCaseBounds(PushCase(r, v)))
END BeginVarient ;


(*
   EndVarient - end a varient record.
*)

PROCEDURE EndVarient ;
BEGIN
   PopCase
END EndVarient ;


(*
   ElseVarient - associate an ELSE clause with a varient record.
*)

PROCEDURE ElseVarient ;
VAR
   f: CARDINAL ;
BEGIN
   f := GetRecordOrField() ;
   Assert(IsFieldVarient(f)) ;
   ElseCase(f)
END ElseVarient ;



(*
   BeginVarientList - begin an ident list containing ranges belonging to a
                      varient list.
*)

PROCEDURE BeginVarientList ;
VAR
   f: CARDINAL ;
BEGIN
   f := GetRecordOrField() ;
   Assert(IsFieldVarient(f)) ;
   BeginCaseList(f)
END BeginVarientList ;


(*
   EndVarientList - end a range list for a varient field.
*)

PROCEDURE EndVarientList ;
BEGIN
   EndCaseList
END EndVarientList ;


(*
   AddVarientRange - creates a range from the top two contant expressions
                     on the stack which are recorded with the current
                     varient field.  The stack is unaltered.
*)

PROCEDURE AddVarientRange ;
VAR
   r1, r2: CARDINAL ;
BEGIN
   PopT(r1) ;
   PopT(r2) ;
   AddRange(r1, r2, GetTokenNo())
END AddVarientRange ;


(*
   AddVarientEquality - adds the contant expression on the top of the stack
                        to the current varient field being recorded.
                        The stack is unaltered.
*)

PROCEDURE AddVarientEquality ;
VAR
   r1: CARDINAL ;
BEGIN
   PopT(r1) ;
   AddRange(r1, NulSym, GetTokenNo())
END AddVarientEquality ;


(*
   IncOperandD - increment the dimension number associated with symbol
                 at, pos, on the boolean stack.
*)

PROCEDURE IncOperandD (pos: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PeepAddress(BoolStack, pos) ;
   INC(f^.Dimension)
END IncOperandD ;


(*
   PushTFA - Push True, False, Array, numbers onto the
             True/False stack.  True and False are assumed to
             contain Symbols or Ident etc.
*)

PROCEDURE PushTFA (True, False, Array: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Unbounded := Array ;
      BooleanOp := FALSE ;
      Dimension := 0 ;
      ReadWrite := NulSym ;
      name      := NulSym
   END ;
   PushAddress(BoolStack, f)
END PushTFA ;


(*
   PushTFAD - Push True, False, Array, Dim, numbers onto the
              True/False stack.  True and False are assumed to
              contain Symbols or Ident etc.
*)

PROCEDURE PushTFAD (True, False, Array, Dim: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Unbounded := Array ;
      BooleanOp := FALSE ;
      Dimension := Dim ;
      ReadWrite := NulSym ;
      name      := NulSym
   END ;
   PushAddress(BoolStack, f)
END PushTFAD ;


(*
   PushTFADrw - Push True, False, Array, Dim, rw, numbers onto the
                True/False stack.  True and False are assumed to
                contain Symbols or Ident etc.
*)

PROCEDURE PushTFADrw (True, False, Array, Dim, rw: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Unbounded := Array ;
      BooleanOp := FALSE ;
      Dimension := Dim ;
      ReadWrite := rw ;
      name      := NulSym
   END ;
   PushAddress(BoolStack, f)
END PushTFADrw ;


(*
   PushTFD - Push True, False, Dim, numbers onto the
             True/False stack.  True and False are assumed to
             contain Symbols or Ident etc.
*)

PROCEDURE PushTFD (True, False, Dim: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Unbounded := NulSym ;
      BooleanOp := FALSE ;
      Dimension := Dim ;
      ReadWrite := NulSym ;
      name      := NulSym
   END ;
   PushAddress(BoolStack, f)
END PushTFD ;


(*
   PopTFD - Pop a True, False, Dim number from the True/False stack.
            True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PopTFD (VAR True, False, Dim: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      Dim := Dimension ;
      Assert(NOT BooleanOp)
   END ;
   DISPOSE(f)
END PopTFD ;


(*
   PushTFDrw - Push True, False, Dim, numbers onto the
               True/False stack.  True and False are assumed to
               contain Symbols or Ident etc.
*)

PROCEDURE PushTFDrw (True, False, Dim, rw: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Unbounded := NulSym ;
      BooleanOp := FALSE ;
      Dimension := Dim ;
      ReadWrite := rw ;
      name      := NulSym
   END ;
   PushAddress(BoolStack, f)
END PushTFDrw ;


(*
   PushTFrw - Push a True and False numbers onto the True/False stack.
              True and False are assumed to contain Symbols or Ident etc.
              It also pushes the higher level symbol which is associated
              with the True symbol.  Eg record variable or array variable.
*)

PROCEDURE PushTFrw (True, False: WORD; rw: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Unbounded := NulSym ;
      BooleanOp := FALSE ;
      Dimension := 0 ;
      ReadWrite := rw ;
      name      := NulSym
   END ;
   PushAddress(BoolStack, f)
END PushTFrw ;


(*
   PopTFrw - Pop a True and False number from the True/False stack.
             True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PopTFrw (VAR True, False, rw: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      Assert(NOT BooleanOp) ;
      rw := ReadWrite
   END ;
   DISPOSE(f)
END PopTFrw ;


(*
   PushTF - Push a True and False numbers onto the True/False stack.
            True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PushTF (True, False: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Unbounded := NulSym ;
      BooleanOp := FALSE ;
      Dimension := 0 ;
      ReadWrite := NulSym ;
      name      := NulSym
   END ;
   PushAddress(BoolStack, f)
END PushTF ;


(*
   PopTF - Pop a True and False number from the True/False stack.
           True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PopTF (VAR True, False: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      Assert(NOT BooleanOp)
   END ;
   DISPOSE(f)
END PopTF ;


(*
   PushT - Push an item onto the True/False stack. The False value will be zero.
*)

PROCEDURE PushT (True: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := 0 ;
      Unbounded := NulSym ;
      BooleanOp := FALSE ;
      Dimension := 0 ;
      ReadWrite := NulSym ;
      name      := NulSym
   END ;
   PushAddress(BoolStack, f)
END PushT ;


(*
   PopT - Pops an item from the True/False stack. The False value is ignored.
*)

PROCEDURE PopT (VAR True: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      Assert(NOT BooleanOp)
   END ;
   DISPOSE(f)
END PopT ;


(*
   PushTrw - Push an item onto the True/False stack. The False value will be zero.
*)

PROCEDURE PushTrw (True: WORD; rw: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := 0 ;
      Unbounded := NulSym ;
      BooleanOp := FALSE ;
      Dimension := 0 ;
      ReadWrite := rw ;
      name      := NulSym
   END ;
   PushAddress(BoolStack, f)
END PushTrw ;


(*
   PopTrw - Pop a True field and rw symbol from the stack.
*)

PROCEDURE PopTrw (VAR True, rw: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      Assert(NOT BooleanOp) ;
      rw := ReadWrite
   END ;
   DISPOSE(f)
END PopTrw ;


(*
   PushTFn - Push a True and False numbers onto the True/False stack.
             True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PushTFn (True, False, n: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      TrueExit  := True ;
      FalseExit := False ;
      Unbounded := NulSym ;
      BooleanOp := FALSE ;
      Dimension := 0 ;
      ReadWrite := NulSym ;
      name      := n
   END ;
   PushAddress(BoolStack, f)
END PushTFn ;


(*
   PopTFn - Pop a True and False number from the True/False stack.
            True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PopTFn (VAR True, False, n: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      n := name ;
      Assert(NOT BooleanOp)
   END ;
   DISPOSE(f)
END PopTFn ;


(*
   PopNothing - pops the top element on the boolean stack.
*)

PROCEDURE PopNothing ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   DISPOSE(f)
END PopNothing ;


(*
   PopN - pops multiple elements from the BoolStack.
*)

PROCEDURE PopN (n: CARDINAL) ;
BEGIN
   WHILE n>0 DO
      PopNothing ;
      DEC(n)
   END
END PopN ;


(*
   Top - returns the no of items held in the stack.
*)

PROCEDURE Top () : CARDINAL ;
BEGIN
   RETURN( NoOfItemsInStackAddress(BoolStack) )
END Top ;


(*
   PushAutoOn - push the auto flag and then set it to TRUE.
                Any call to ident in the parser will result in the token being pushed.
*)

PROCEDURE PushAutoOn ;
BEGIN
   PushWord(AutoStack, IsAutoOn) ;
   IsAutoOn := TRUE
END PushAutoOn ;


(*
   PushAutoOff - push the auto flag and then set it to FALSE.
*)

PROCEDURE PushAutoOff ;
BEGIN
   PushWord(AutoStack, IsAutoOn) ;
   IsAutoOn := FALSE
END PushAutoOff ;


(*
   IsAutoPushOn - returns the value of the current Auto ident push flag.
*)

PROCEDURE IsAutoPushOn () : BOOLEAN ;
BEGIN
   RETURN( IsAutoOn )
END IsAutoPushOn ;


(*
   PopAuto - restores the previous value of the Auto flag.
*)

PROCEDURE PopAuto ;
BEGIN
   IsAutoOn := PopWord(AutoStack)
END PopAuto ;


(*
   PushInConstExpression - push the InConstExpression flag and then set it to TRUE.
*)

PROCEDURE PushInConstExpression ;
BEGIN
   PushWord(ConstStack, InConstExpression) ;
   InConstExpression := TRUE
END PushInConstExpression ;


(*
   PopInConstExpression - restores the previous value of the InConstExpression.
*)

PROCEDURE PopInConstExpression ;
BEGIN
   InConstExpression := PopWord(ConstStack)
END PopInConstExpression ;


(*
   IsInConstExpression - returns the value of the InConstExpression.
*)

PROCEDURE IsInConstExpression () : BOOLEAN ;
BEGIN
   RETURN( InConstExpression )
END IsInConstExpression ;


(*
   MustCheckOverflow - returns TRUE if the quadruple should test for overflow.
*)

PROCEDURE MustCheckOverflow (q: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(q) ;
   RETURN( f^.CheckOverflow )
END MustCheckOverflow ;


(*
   StressStack - 
*)

PROCEDURE StressStack ;
CONST
   Maxtries = 1000 ;
VAR
   n, i, j: CARDINAL ;
BEGIN
   PushT(1) ;
   PopT(i) ;
   Assert(i=1) ;
   FOR n := 1 TO Maxtries DO
      FOR i := n TO 1 BY -1 DO
         PushT(i)
      END ;
      FOR i := n TO 1 BY -1 DO
         Assert(OperandT(i)=i)
      END ;
      FOR i := 1 TO n DO
         Assert(OperandT(i)=i)
      END ;
      FOR i := 1 TO n BY 10 DO
         Assert(OperandT(i)=i)
      END ;
      IF (n>1) AND (n MOD 2 = 0)
      THEN
         FOR i := 1 TO n DIV 2 DO
            PopT(j) ;
            Assert(j=i)
         END ;
         FOR i := n DIV 2 TO 1 BY -1 DO
            PushT(i)
         END
      END ;
      FOR i := 1 TO n DO
         PopT(j) ;
         Assert(j=i)
      END
   END
END StressStack ;


(*
   Init - initialize the M2Quads module, all the stacks, all the lists 
          and the quads list.
*)

PROCEDURE Init ;
BEGIN
   LogicalOrTok := MakeKey('_LOR') ;
   LogicalAndTok := MakeKey('_LAND') ;
   LogicalXorTok := MakeKey('_LXOR') ;
   LogicalDifferenceTok := MakeKey('_LDIFF') ;
   QuadArray := InitIndex(1) ;
   FreeList := 1 ;
   NewQuad(NextQuad) ;
   Assert(NextQuad=1) ;
   BoolStack := InitStackAddress() ;
   ExitStack := InitStackWord() ;
   RepeatStack := InitStackWord() ;
   WhileStack := InitStackWord() ;
   ForStack := InitStackWord() ;
   WithStack := InitStackAddress() ;
   ReturnStack := InitStackWord() ;
   LineStack := InitStackAddress() ;
   PriorityStack := InitStackWord() ;
   TryStack := InitStackWord() ;
   CatchStack := InitStackWord() ;
   ExceptStack := InitStackWord() ;
   ConstructorStack := InitStackAddress() ;
   ConstStack := InitStackWord() ;
   (* StressStack ; *)
   SuppressWith := FALSE ;
   Head := 1 ;
   LastQuadNo := 0 ;
   MustNotCheckBounds := FALSE ;
   InitQuad := 0 ;
   GrowInitialization := 0 ;
   WITH ForInfo DO
      InitList(IncrementQuad) ;
      InitList(StartOfForLoop) ;
      InitList(EndOfForLoop) ;
      InitList(ForLoopIndex)
   END ;
   QuadrupleGeneration := TRUE ;
   BuildingHigh := FALSE ;
   BuildingSize := FALSE ;
   AutoStack := InitStackWord() ;
   IsAutoOn := TRUE ;
   InConstExpression := FALSE ;
   FreeLineList := NIL ;
   InitList(VarientFields) ;
   VarientFieldNo := 0 ;
   NoOfQuads := 0
END Init ;


BEGIN
   Init
END M2Quads.
