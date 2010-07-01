(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE M2GenGCC ;

FROM SYSTEM IMPORT ADDRESS, WORD ;

FROM SymbolTable IMPORT PushSize, PopSize, PushValue, PopValue,
                        PushVarSize,
                        PushSumOfLocalVarSize,
                        PushSumOfParamSize,
                        MakeConstLit,
                        RequestSym, FromModuleGetSym,
                        StartScope, EndScope,
                        GetMainModule, GetModuleScope,
                        GetSymName, ModeOfAddr, GetMode,
                        GetGnuAsm, IsGnuAsmVolatile,
                        GetGnuAsmInput, GetGnuAsmOutput, GetGnuAsmTrash,
                        GetLowestType,
                        GetModuleFinallyFunction, PutModuleFinallyFunction,
                        GetLocalSym, GetVarWritten,
                        GetVarient, GetVarBackEndType,
                        NoOfParam, GetParent, GetDimension,
                        IsModule, IsDefImp, IsType, IsModuleWithinProcedure,
                        IsConstString, GetString, GetStringLength,
                        IsConst, IsConstSet, IsProcedure, IsProcType,
                        IsVar, IsVarParam, IsTemporary,
                        IsEnumeration,
                        IsUnbounded, IsArray, IsSet, IsConstructor,
                        IsProcedureVariable,
                        IsUnboundedParam,
                        IsRecordField, IsFieldVarient, IsVarient, IsRecord,
                        IsExportQualified,
                        IsExported,
                        IsSubrange, IsPointer,
                        IsProcedureBuiltin, IsProcedureInline,
                        IsParameter,
                        IsValueSolved, IsSizeSolved,
                        IsProcedureNested, IsInnerModule,
                        IsComposite,
                        ForeachExportedDo,
                        ForeachImportedDo,
                        ForeachProcedureDo,
                        ForeachInnerModuleDo,
                        GetType, GetNth, GetNthParam,
                        SkipType, SkipTypeAndSubrange,
                        GetUnboundedHighOffset,
                        GetUnboundedAddressOffset,
                        GetSubrange, NoOfElements, GetArraySubscript,
                        GetFirstUsed, GetDeclared,
                        GetRegInterface,
                        GetProcedureQuads,
                        GetProcedureBuiltin,
                        GetPriority, GetNeedSavePriority,
                        PutConstString,
                        PutConst, PutConstSet, PutConstructor,
                        NulSym ;

FROM M2LexBuf IMPORT FindFileNameFromToken, TokenToLineNo ;
FROM M2Code IMPORT CodeBlock ;
FROM M2Debug IMPORT Assert ;
FROM M2Error IMPORT InternalError, WriteFormat0, WriteFormat1, WriteFormat2, ErrorStringAt, WarnStringAt ;
FROM M2MetaError IMPORT MetaErrorT1, MetaErrorT2, MetaError1 ;

FROM M2Options IMPORT DisplayQuadruples, UnboundedByReference, PedanticCast,
                      VerboseUnbounded, Iso, Pim ;

FROM M2Printf IMPORT printf0, printf1, printf2, printf4 ;

FROM M2Base IMPORT MixTypes, NegateType, ActivationPointer, IsMathType,
                   IsRealType, IsAComplexType,
                   IsOrdinalType,
                   Cardinal, Char, Integer, IsTrunc,
                   Boolean, True,
                   Im, Re, Cmplx, GetCmplxReturnType, GetBaseTypeMinMax,
                   CheckAssignmentCompatible, IsAssignmentCompatible ;

FROM M2Bitset IMPORT Bitset ;
FROM NameKey IMPORT Name, MakeKey, KeyToCharStar, makekey, NulName ;
FROM DynamicStrings IMPORT string, InitString, KillString, String, InitStringCharStar, Mark, Slice, ConCat ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3, Sprintf4 ;
FROM M2System IMPORT Address, Word, System, MakeAdr, IsSystemType, IsGenericSystemType, IsRealN, IsComplexN, IsSetN ;
FROM M2FileName IMPORT CalculateFileName ;
FROM M2AsmUtil IMPORT GetModuleInitName, GetModuleFinallyName ;
FROM SymbolConversion IMPORT AddModGcc, Mod2Gcc, GccKnowsAbout ;

FROM M2StackWord IMPORT InitStackWord, StackOfWord, PeepWord, ReduceWord,
                        PushWord, PopWord, IsEmptyWord ;

FROM Lists IMPORT List, InitList, KillList,
                  PutItemIntoList,
                  RemoveItemFromList, IncludeItemIntoList,
                  NoOfItemsInList, GetItemFromList ;

FROM M2ALU IMPORT PtrToValue,
                  IsValueTypeReal, IsValueTypeSet,
                  IsValueTypeConstructor, IsValueTypeArray,
                  IsValueTypeRecord, IsValueTypeComplex,
                  PushIntegerTree, PopIntegerTree,
                  PushSetTree, PopSetTree,
                  PopRealTree, PushCard,
                  PushRealTree,
                  PopComplexTree,
                  Gre, Sub, Equ, NotEqu, LessEqu,
                  BuildRange, SetOr, SetAnd, SetNegate,
                  SetSymmetricDifference, SetDifference,
                  SetShift, SetRotate,
                  AddBit, SubBit, Less, Addn, GreEqu, SetIn,
                  CheckOrResetOverflow, GetRange, GetValue ;

FROM M2GCCDeclare IMPORT WalkAction,
                         DeclareConstant, TryDeclareConstant,
                         DeclareConstructor, TryDeclareConstructor,
                         StartDeclareScope, EndDeclareScope,
                         DeclareLocalVariables, PromoteToString,
                         CompletelyResolved,
                         PoisonSymbols, GetTypeMin, GetTypeMax,
                         IsProcedureGccNested, DeclareParameters,
                         ConstantKnownAndUsed ;

FROM M2Range IMPORT CodeRangeCheck, FoldRangeCheck, CodeErrorCheck ;

FROM gm2builtins IMPORT BuiltInMemCopy, BuiltInAlloca,
                        GetBuiltinConst, GetBuiltinTypeInfo,
                        BuiltinExists, BuildBuiltinTree ;

FROM gccgm2 IMPORT Tree, GetIntegerZero, GetIntegerOne, GetIntegerType,
                   BuildStartFunctionDeclaration, BuildEndFunctionDeclaration,
                   BuildVariableArrayAndDeclare, BuildCharConstant,
                   BuildBinProcedure, BuildUnaryProcedure,
                   BuildSetProcedure,
                   ChainOnParamValue, AddStringToTreeList,
                   SetFileNameAndLineNo, EmitLineNote, BuildStart, BuildEnd,
                   BuildCallInner,
                   BuildStartFunctionCode, BuildEndFunctionCode,
                   BuildReturnValueCode, SetLastFunction,
                   BuildAssignmentTree, DeclareKnownConstant,
                   BuildAdd, BuildSub, BuildMult, BuildLSL,
                   BuildDivTrunc, BuildModTrunc, BuildDivFloor, BuildModFloor,
                   BuildLogicalOrAddress,
                   BuildLogicalOr, BuildLogicalAnd, BuildSymmetricDifference,
                   BuildLogicalDifference,
                   BuildLogicalShift, BuildLogicalRotate,
                   BuildNegate, BuildAddr, BuildSize, BuildOffset, BuildOffset1,
                   BuildGoto, DeclareLabel, StringLength,
                   BuildLessThan, BuildGreaterThan,
                   BuildLessThanOrEqual, BuildGreaterThanOrEqual,
                   BuildEqualTo, BuildNotEqualTo,
                   BuildIsSuperset, BuildIsNotSuperset,
                   BuildIsSubset, BuildIsNotSubset,
                   BuildIfIn, BuildIfNotIn,
                   BuildIndirect, BuildArray,
                   BuildConvert, BuildTrunc, BuildCoerce,
                   BuildBinaryForeachWordDo,
                   BuildUnaryForeachWordDo,
                   BuildBinarySetDo,
                   BuildExcludeVarConst, BuildIncludeVarConst,
                   BuildExcludeVarVar, BuildIncludeVarVar,
                   BuildIfConstInVar, BuildIfNotConstInVar,
                   BuildIfVarInVar, BuildIfNotVarInVar,
                   BuildIfInRangeGoto, BuildIfNotInRangeGoto,
                   BuildForeachWordInSetDoIfExpr,
                   ConvertConstantAndCheck,
                   BuildArrayStringConstructor,
                   AreConstantsEqual, CompareTrees,
                   DoJump,
                   BuildProcedureCallTree, BuildIndirectProcedureCallTree,
                   BuildParam, BuildFunctValue,
                   BuildAsm, DebugTree,
                   BuildSetNegate,
                   BuildPushFunctionContext, BuildPopFunctionContext,
                   BuildCap, BuildAbs, BuildRe, BuildIm, BuildCmplx,
                   AddStatement, ConvertString,
                   GetPointerType, GetPointerZero,
                   GetWordType, GetM2ZType, GetM2RType, GetM2CType,
                   GetBitsPerBitset, GetSizeOfInBits, GetMaxFrom,
                   BuildIntegerConstant, BuildStringConstant,
                   RememberConstant, FoldAndStrip, RemoveOverflow ;

FROM gm2except IMPORT BuildThrow, BuildTryBegin, BuildTryEnd,
                      BuildCatchBegin, BuildCatchEnd ;

FROM M2Quads IMPORT QuadOperator, GetQuad, IsReferenced, GetNextQuad,
                    SubQuad, PutQuad, MustCheckOverflow,
                    QuadToTokenNo,
                    DisplayQuadList ;


CONST
   Debugging         = FALSE ;
   PriorityDebugging = FALSE ;
   CascadedDebugging = FALSE ;

TYPE
   DoProcedure      = PROCEDURE (CARDINAL) ;
   DoUnaryProcedure = PROCEDURE (CARDINAL) ;

VAR
   UnboundedLabelNo         : CARDINAL ;
   CurrentQuadToken         : CARDINAL ;
   LastLine                 : CARDINAL ;(* The Last Line number emitted with the  *)
                                        (* generated code.                        *)
   LastOperator             : QuadOperator ; (* The last operator processed.      *)
   ModuleName,
   FileName                 : String ;
   CompilingMainModuleStack : StackOfWord ; (* Determines whether the main module     *)
                                            (* quadrules are being processed.         *)
   ScopeStack               : StackOfWord ; (* keeps track of the current scope       *)
                                            (* under translation.                     *)
   NoChange                 : BOOLEAN ;     (* has any constant been resolved?        *)


(*
   Rules for Quadruples
   ====================

   Rules
   =====

   All program declared variables are given the mode, Offset.
   All constants have mode, Immediate.

   Operators
   =========

------------------------------------------------------------------------------
   Array Operators
------------------------------------------------------------------------------
   Sym<I>   Base   a            Delivers a constant result if a is a
                                Global variable. If a is a local variable
                                then the Frame pointer needs to be added.
                                Base yields the effective location in memory
                                of, a, array [0,0, .. ,0] address.
   Sym<I>   ElementSize 1       Always delivers a constant. The number
                                indicates which specified element is chosen.
                                ElementSize is the TypeSize for that element.
   Unbounded  Op1 Op3           Initializes the op1 StartAddress of the array
                                op3. Op3 can be a normal array or unbounded array.
                                op1 (is the Unbounded.ArrayAddress) := ADR(op3).
                                In GNU Modula-2 the callee saves non var unbounded
                                arrays. This is direct contrast to the M2F native
                                code generators.
------------------------------------------------------------------------------
   := Operator
------------------------------------------------------------------------------
   Sym1<I> := Sym3<I>           := produces a constant
   Sym1<O> := Sym3<O>           := has the effect Mem[Sym1<I>] := Mem[Sym3<I>]
------------------------------------------------------------------------------
   Addr Operator  - contains the address of a variable - may need to add
------------------------------------------------------------------------------
   Yields the address of a variable - need to add the frame pointer if
   a variable is local to a procedure.

   Sym1<O>   Addr   Sym2<O>     meaning     Mem[Sym1<I>] := Sym2<I>
   Sym1<V>   Addr   Sym2<O>     meaning     Mem[Sym1<I>] := Sym2<I>
   Sym1<O>   Addr   Sym2<V>     meaning     Mem[Sym1<I>] := Mem[Sym2<I>]
   Sym1<V>   Addr   Sym2<V>     meaning     Mem[Sym1<I>] := Mem[Sym2<I>]
------------------------------------------------------------------------------
   Xindr Operator  ( *a = b)
------------------------------------------------------------------------------
   Sym1<O>   Copy   Sym2<I>     Meaning     Mem[Sym1<I>] := constant
   Sym1<V>   Copy   Sym2<I>     Meaning     Mem[Sym1<I>] := constant
   Sym1<O>   Copy   Sym2<O>     meaning     Mem[Sym1<I>] := Mem[Sym2<I>]
   Sym1<V>   Copy   Sym2<O>     meaning     Mem[Sym1<I>] := Mem[Sym2<I>]
   Sym1<O>   Copy   Sym2<V>     meaning     Mem[Sym1<I>] := Mem[Mem[Sym2<I>]]
   Sym1<V>   Copy   Sym2<V>     meaning     Mem[Sym1<I>] := Mem[Mem[Sym2<I>]]
------------------------------------------------------------------------------
   IndrX Operator  (a = *b)   where <X> means any value
------------------------------------------------------------------------------
   Sym1<X>   IndrX  Sym2<I>     meaning     Mem[Sym1<I>] := Mem[constant]
   Sym1<X>   IndrX  Sym2<I>     meaning     Mem[Sym1<I>] := Mem[constant]

   Sym1<X>   IndrX  Sym2<X>     meaning     Mem[Sym1<I>] := Mem[Mem[Sym2<I>]]
   Sym1<X>   IndrX  Sym2<X>     meaning     Mem[Sym1<I>] := Mem[Mem[Sym2<I>]]
------------------------------------------------------------------------------
   + - / * Operators
------------------------------------------------------------------------------
   Sym1<I>   +      Sym2<I>  Sym3<I>  meaning Sym1<I> := Sym2<I> + Sym3<I>
   Sym1<O>   +      Sym2<O>  Sym3<I>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Sym3<I>
   Sym1<O>   +      Sym2<O>  Sym3<O>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Mem[Sym3<I>]
   Sym1<O>   +      Sym2<O>  Sym3<V>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Mem[Sym3<I>]
   Sym1<V>   +      Sym2<O>  Sym3<V>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Mem[Sym3<I>]
   Sym1<V>   +      Sym2<V>  Sym3<V>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Mem[Sym3<I>]
------------------------------------------------------------------------------
   Base Operator
------------------------------------------------------------------------------
   Sym1<O>   Base  Sym2   Sym3<O>     meaning     Mem[Sym1<I>] := Sym3<I>
   Sym1<V>   Base  Sym2   Sym3<O>     meaning     Should Never Occur But If it did..
                                                  Mem[Mem[Sym1<I>]] := Sym3<I>
   Sym1<O>   Base  Sym2   Sym3<V>     meaning     Mem[Sym1<I>] := Mem[Sym3<I>]
   Sym1<V>   Base  Sym2   Sym3<V>     meaning     Should Never Occur But If it did..
                                                  Mem[Mem[Sym1<I>]] := Mem[Sym3<I>]
                   Sym2 is the array type
------------------------------------------------------------------------------
*)

(* To keep p2c happy we declare forward procedures *)

(* %%%FORWARD%%%
PROCEDURE FoldBuiltinTypeInfo (tokenno: CARDINAL; p: WalkAction;
                               quad: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CheckStop (q: CARDINAL) ; FORWARD ;
PROCEDURE stop ; FORWARD ;
PROCEDURE CodeSaveException (quad: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeRestoreException (quad: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeError (quad: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeRange (quad: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE FoldRange (tokenno: CARDINAL; p: WalkAction;
                     q: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE StringToChar (t: Tree; type, str: CARDINAL) : Tree ; FORWARD ;
PROCEDURE LValueToGenericPtrOrConvert (sym: CARDINAL; type: Tree) : Tree ; FORWARD ;
PROCEDURE SafeConvert (sym, with: CARDINAL) : Tree ; FORWARD ;
PROCEDURE CodeInitStart (q: CARDINAL; op1, op2, op3: CARDINAL; CompilingMainModule: BOOLEAN); FORWARD ;
PROCEDURE CodeInitEnd (q: CARDINAL; op1, op2, op3: CARDINAL; CompilingMainModule: BOOLEAN); FORWARD ;
PROCEDURE CodeStartModFile (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeStartDefFile (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeEndFile (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeStatement (q: CARDINAL) ; FORWARD ;
PROCEDURE CodeLineNumber (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeNewLocalVar (quad: CARDINAL; LineNo, PreviousScope, CurrentProcedure: CARDINAL); FORWARD ;
PROCEDURE CodeKillLocalVar (quad: CARDINAL; LineNo, op2, CurrentProcedure: CARDINAL); FORWARD ;
PROCEDURE CodeReturnValue (quad: CARDINAL; res, op2, Procedure: CARDINAL); FORWARD ;
PROCEDURE CodeReturn (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeProcedureScope (quad: CARDINAL; LineNo, PreviousScope, CurrentProcedure: CARDINAL); FORWARD ;
PROCEDURE CodeTry (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeCatchBegin (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeCatchEnd (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeRetry (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldBecomes (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeBecomes (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldAdd (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeAdd (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldSub (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeSub (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldMult (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeMult (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldDivTrunc (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeDivTrunc (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldModTrunc (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeModTrunc (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldDivFloor (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeDivFloor (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldModFloor (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeModFloor (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldBitRange (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE FoldBit (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeBit (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeGoto (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CheckReferenced (q: CARDINAL; op: QuadOperator) ; FORWARD ;
PROCEDURE FoldSetOr (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE FoldSetAnd (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeSymmetricDifference (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldSymmetricDifference (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE FoldNegate (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeNegate (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldSize (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeSize (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeAddr (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldStandardFunction (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeStandardFunction (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeIfLess (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeIfLessEqu (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeIfGre (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeIfGreEqu (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeIfEqu (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeIfNotEqu (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeIfNotIn (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeIfIn (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeIndrX (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeXIndr (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeCall (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeDirectCall (procedure: CARDINAL); FORWARD ;
PROCEDURE CodeIndirectCall (ProcVar: CARDINAL); FORWARD ;
PROCEDURE CodeParam (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeFunctValue (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeInline (quad: CARDINAL; op1, op2, GnuAsm: CARDINAL); FORWARD ;
PROCEDURE CodeOffset (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldOffset (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeHigh (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldHigh (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL; op1, dim, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeUnbounded (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeArray (quad: CARDINAL; res, index, array: CARDINAL) ; FORWARD ;
PROCEDURE FoldElementSize (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeElementSize (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldCoerce (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeCoerce (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldCast (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeCast (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldConvert (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeConvert (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeMath (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeSetShift (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeSetRotate (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeSetLogicalDifference (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeSetOr (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeSetAnd (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeSetSymmetricDifference (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeIncl (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE CodeExcl (quad: CARDINAL; op1, op2, op3: CARDINAL); FORWARD ;
PROCEDURE FoldIncl (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE FoldExcl (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE FoldIfIn (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE FoldIfNotIn (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE FoldBuiltinConst (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL;op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE ResolveHigh (quad: CARDINAL; dim, operand: CARDINAL) : Tree ; FORWARD ;
PROCEDURE MakeCopyAndUse (proc, param, i: CARDINAL) ; FORWARD ;
PROCEDURE FoldIfLess (tokenno: CARDINAL; p: WalkAction;
                      quad: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE FoldSetShift (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE FoldSetRotate (tokenno: CARDINAL; p: WalkAction;
                         quad: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE FoldBuiltinFunction (tokenno: CARDINAL; p: WalkAction;
                               q: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE FoldMakeAdr (tokenno: CARDINAL; p: WalkAction;
                       q: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeBuiltinFunction (q: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeMakeAdr (q: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeModuleScope (quad: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeSavePriority (quad: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CodeRestorePriority (quad: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE CreateLabelProcedureN (proc: CARDINAL; leader: ARRAY OF CHAR; unboundedCount, n: CARDINAL) : String ; FORWARD ;
PROCEDURE CreateLabelName (q: CARDINAL) : String ; FORWARD ;
PROCEDURE CodeFinallyStart (quad: CARDINAL; op1, op2, op3: CARDINAL; CompilingMainModule: BOOLEAN) ; FORWARD ;
PROCEDURE CodeFinallyEnd (quad: CARDINAL; op1, op2, op3: CARDINAL; CompilingMainModule: BOOLEAN) ; FORWARD ;
PROCEDURE CodeThrow (quad: CARDINAL; op1, op2, op3: CARDINAL) ; FORWARD ;
   %%%FORWARD%%% *)


(*
   ConvertQuadsToTree - runs through the quadruple list and converts it into
                        the GCC tree structure.
*)

PROCEDURE ConvertQuadsToTree (Start, End: CARDINAL) ;
VAR
   Prev: CARDINAL ;
BEGIN
   REPEAT
      CodeStatement(Start) ;
      Prev := Start ;
      Start := GetNextQuad(Start)
   UNTIL (Start>End) OR (Start=0)
END ConvertQuadsToTree ;


(*
   IsCompilingMainModule - 
*)

PROCEDURE IsCompilingMainModule (sym: CARDINAL) : BOOLEAN ;
BEGIN
   WHILE (sym#NulSym) AND (GetMainModule()#sym) DO
      sym := GetModuleScope(sym)
   END ;
   RETURN( sym#NulSym )
END IsCompilingMainModule ;


(*
   CodeStatement - A multi-way decision call depending on the current
                   quadruple.
*)

PROCEDURE CodeStatement (q: CARDINAL) ;
VAR
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   GetQuad(q, op, op1, op2, op3) ;
   CurrentQuadToken := QuadToTokenNo(q) ;
   CheckReferenced(q, op) ;

   CASE op OF

   StartDefFileOp     : CodeStartDefFile(q, op1, op2, op3) |
   StartModFileOp     : CodeStartModFile(q, op1, op2, op3) |
   ModuleScopeOp      : CodeModuleScope(q, op1, op2, op3) |
   EndFileOp          : CodeEndFile(q, op1, op2, op3) |
   InitStartOp        : CodeInitStart(q, op1, op2, op3, IsCompilingMainModule(op3)) |
   InitEndOp          : CodeInitEnd(q, op1, op2, op3, IsCompilingMainModule(op3)) |
   FinallyStartOp     : CodeFinallyStart(q, op1, op2, op3, IsCompilingMainModule(op3)) |
   FinallyEndOp       : CodeFinallyEnd(q, op1, op2, op3, IsCompilingMainModule(op3)) |
   NewLocalVarOp      : CodeNewLocalVar(q, op1, op2, op3) |
   KillLocalVarOp     : CodeKillLocalVar(q, op1, op2, op3) |
   ProcedureScopeOp   : CodeProcedureScope(q, op1, op2, op3) |
   ReturnOp           : CodeReturn(q, op1, op2, op3) |
   ReturnValueOp      : CodeReturnValue(q, op1, op2, op3) |
   TryOp              : CodeTry(q, op1, op2, op3) |
   ThrowOp            : CodeThrow(q, op1, op2, op3) |
   CatchBeginOp       : CodeCatchBegin(q, op1, op2, op3) |
   CatchEndOp         : CodeCatchEnd(q, op1, op2, op3) |
   RetryOp            : CodeRetry(q, op1, op2, op3) |
   DummyOp            : |
   BecomesOp          : CodeBecomes(q, op1, op2, op3) |
   AddOp              : CodeAdd(q, op1, op2, op3) |
   SubOp              : CodeSub(q, op1, op2, op3) |
   MultOp             : CodeMult(q, op1, op2, op3) |
   DivTruncOp         : CodeDivTrunc(q, op1, op2, op3) |
   ModTruncOp         : CodeModTrunc(q, op1, op2, op3) |
   DivFloorOp         : CodeDivFloor(q, op1, op2, op3) |
   ModFloorOp         : CodeModFloor(q, op1, op2, op3) |
   GotoOp             : CodeGoto(q, op1, op2, op3) |
   InclOp             : CodeIncl(q, op1, op2, op3) |
   ExclOp             : CodeExcl(q, op1, op2, op3) |
   NegateOp           : CodeNegate(q, op1, op2, op3) |
   LogicalShiftOp     : CodeSetShift(q, op1, op2, op3) |
   LogicalRotateOp    : CodeSetRotate(q, op1, op2, op3) |
   LogicalOrOp        : CodeSetOr(q, op1, op2, op3) |
   LogicalAndOp       : CodeSetAnd(q, op1, op2, op3) |
   LogicalXorOp       : CodeSetSymmetricDifference(q, op1, op2, op3) |
   LogicalDiffOp      : CodeSetLogicalDifference(q, op1, op2, op3) |
   IfLessOp           : CodeIfLess(q, op1, op2, op3) |
   IfEquOp            : CodeIfEqu(q, op1, op2, op3) |
   IfNotEquOp         : CodeIfNotEqu(q, op1, op2, op3) |
   IfGreEquOp         : CodeIfGreEqu(q, op1, op2, op3) |
   IfLessEquOp        : CodeIfLessEqu(q, op1, op2, op3) |
   IfGreOp            : CodeIfGre(q, op1, op2, op3) |
   IfInOp             : CodeIfIn(q, op1, op2, op3) |
   IfNotInOp          : CodeIfNotIn(q, op1, op2, op3) |
   IndrXOp            : CodeIndrX(q, op1, op2, op3) |
   XIndrOp            : CodeXIndr(q, op1, op2, op3) |
   CallOp             : CodeCall(q, op1, op2, op3) |
   ParamOp            : CodeParam(q, op1, op2, op3) |
   FunctValueOp       : CodeFunctValue(q, op1, op2, op3) |
   AddrOp             : CodeAddr(q, op1, op2, op3) |
   SizeOp             : CodeSize(q, op1, op2, op3) |
   UnboundedOp        : CodeUnbounded(q, op1, op2, op3) |
   OffsetOp           : CodeOffset(q, op1, op2, op3) |
   HighOp             : CodeHigh(q, op1, op2, op3) |
   ArrayOp            : CodeArray(q, op1, op2, op3) |
   ElementSizeOp      : CodeElementSize(q, op1, op2, op3) |
   ConvertOp          : CodeConvert(q, op1, op2, op3) |
   CoerceOp           : CodeCoerce(q, op1, op2, op3) |
   CastOp             : CodeCast(q, op1, op2, op3) |
   StandardFunctionOp : CodeStandardFunction(q, op1, op2, op3) |
   SavePriorityOp     : CodeSavePriority(q, op1, op2, op3) |
   RestorePriorityOp  : CodeRestorePriority(q, op1, op2, op3) |

   InlineOp           : CodeInline(q, op1, op2, op3) |
   LineNumberOp       : CodeLineNumber(q, op1, op2, op3) |
   CodeOnOp           : |           (* the following make no sense with gcc *)
   CodeOffOp          : |
   ProfileOnOp        : |
   ProfileOffOp       : |
   OptimizeOnOp       : |
   OptimizeOffOp      : |
   RangeCheckOp       : CodeRange(q, op1, op2, op3) |
   ErrorOp            : CodeError(q, op1, op2, op3) |
   SaveExceptionOp    : CodeSaveException(q, op1, op2, op3) |
   RestoreExceptionOp : CodeRestoreException(q, op1, op2, op3)

   ELSE
      WriteFormat1('quadruple %d not yet implemented', q) ;
      InternalError('quadruple not implemented yet', __FILE__, __LINE__)
   END ;
   LastOperator := op
END CodeStatement ;


(*
   ResolveConstantExpressions - resolves constant expressions from the quadruple list.
                                It returns TRUE if one or more constants were folded.
                                When a constant symbol value is solved, the call back
                                p(sym) is invoked.
*)

PROCEDURE ResolveConstantExpressions (p: WalkAction; start, end: CARDINAL) : BOOLEAN ;
VAR
   tokenno: CARDINAL ;
   quad   : CARDINAL ;
   op     : QuadOperator ;
   op1,
   op2,
   op3    : CARDINAL ;
   Changed: BOOLEAN ;
BEGIN
   Changed  := FALSE ;
   REPEAT
      NoChange := TRUE ;
      quad := start ;
      WHILE (quad<=end) AND (quad#0) DO
         GetQuad(quad, op, op1, op2, op3) ;
         tokenno := QuadToTokenNo(quad) ;

         (* CheckStop(quad) ; *)

         CASE op OF

         StandardFunctionOp : FoldStandardFunction(tokenno, p, quad, op1, op2, op3) |
         BuiltinConstOp     : FoldBuiltinConst(tokenno, p, quad, op1, op2, op3) |
         BuiltinTypeInfoOp  : FoldBuiltinTypeInfo(tokenno, p, quad, op1, op2, op3) |
         LogicalOrOp        : FoldSetOr(tokenno, p, quad, op1, op2, op3) |
         LogicalAndOp       : FoldSetAnd(tokenno, p, quad, op1, op2, op3) |
         LogicalXorOp       : FoldSymmetricDifference(tokenno, p, quad, op1, op2, op3) |
         BecomesOp          : FoldBecomes(tokenno, p, quad, op1, op2, op3) |
         AddOp              : FoldAdd(tokenno, p, quad, op1, op2, op3) |
         SubOp              : FoldSub(tokenno, p, quad, op1, op2, op3) |
         MultOp             : FoldMult(tokenno, p, quad, op1, op2, op3) |
         DivTruncOp         : FoldDivTrunc(tokenno, p, quad, op1, op2, op3) |
         ModTruncOp         : FoldModTrunc(tokenno, p, quad, op1, op2, op3) |
         DivFloorOp         : FoldDivFloor(tokenno, p, quad, op1, op2, op3) |
         ModFloorOp         : FoldModFloor(tokenno, p, quad, op1, op2, op3) |
         NegateOp           : FoldNegate(tokenno, p, quad, op1, op2, op3) |
         SizeOp             : FoldSize(tokenno, p, quad, op1, op2, op3) |
         OffsetOp           : FoldOffset(tokenno, p, quad, op1, op2, op3) |
         HighOp             : FoldHigh(tokenno, p, quad, op1, op2, op3) |
         ElementSizeOp      : FoldElementSize(tokenno, p, quad, op1, op2, op3) |
         ConvertOp          : FoldConvert(tokenno, p, quad, op1, op2, op3) |
         CoerceOp           : FoldCoerce(tokenno, p, quad, op1, op2, op3) |
         CastOp             : FoldCast(tokenno, p, quad, op1, op2, op3) |
         InclOp             : FoldIncl(tokenno, p, quad, op1, op2, op3) |
         ExclOp             : FoldExcl(tokenno, p, quad, op1, op2, op3) |
         IfLessOp           : FoldIfLess(tokenno, p, quad, op1, op2, op3) |
         IfInOp             : FoldIfIn(tokenno, p, quad, op1, op2, op3) |
         IfNotInOp          : FoldIfNotIn(tokenno, p, quad, op1, op2, op3) |
         LogicalShiftOp     : FoldSetShift(tokenno, p, quad, op1, op2, op3) |
         LogicalRotateOp    : FoldSetRotate(tokenno, p, quad, op1, op2, op3) |
         ParamOp            : FoldBuiltinFunction(tokenno, p, quad, op1, op2, op3) |
         RangeCheckOp       : FoldRange(tokenno, p, quad, op1, op2, op3)

         ELSE
            (* ignore quadruple as it is not associated with a constant expression *)
         END ;
         quad := GetNextQuad(quad)
      END ;
      IF NOT NoChange
      THEN
         Changed := TRUE
      END
   UNTIL NoChange ;
   IF Debugging AND DisplayQuadruples AND FALSE
   THEN
      printf0('after resolving expressions with gcc\n') ;
      DisplayQuadList
   END ;
   RETURN( Changed )
END ResolveConstantExpressions ;


(*
   FindSize - given a Modula-2 symbol, sym, return the GCC Tree
              (constant) representing the storage size in bytes.
*)

PROCEDURE FindSize (sym: CARDINAL) : Tree ;
BEGIN
   IF IsConstString(sym)
   THEN
      PushCard(GetStringLength(sym)) ;
      RETURN( PopIntegerTree() )
   ELSIF IsSizeSolved(sym)
   THEN
      PushSize(sym) ;
      RETURN( PopIntegerTree() )
   ELSE
      IF GccKnowsAbout(sym)
      THEN
         PushIntegerTree(BuildSize(Mod2Gcc(sym), FALSE)) ;
         PopSize(sym) ;
         PushSize(sym) ;
         RETURN( PopIntegerTree() )
      ELSIF IsVar(sym) AND GccKnowsAbout(GetType(sym))
      THEN
         PushIntegerTree(BuildSize(Mod2Gcc(GetType(sym)), FALSE)) ;
         RETURN( PopIntegerTree() )
      ELSE
         InternalError('expecting gcc to already know about this symbol', __FILE__, __LINE__)
      END
   END
END FindSize ;


(*
   FindType - returns the type of, Sym, if Sym is a TYPE then return Sym otherwise return GetType(Sym)
*)

PROCEDURE FindType (Sym: CARDINAL) : CARDINAL ;
BEGIN
   IF IsType(Sym)
   THEN
      RETURN( Sym )
   ELSE
      RETURN( GetType(Sym) )
   END
END FindType ;


(*
   BuildTreeFromInterface - generates a GCC tree from an interface definition.
*)

PROCEDURE BuildTreeFromInterface (sym: CARDINAL) : Tree ;
VAR
   n   : CARDINAL ;
   str,
   obj : CARDINAL ;
   tree: Tree ;
BEGIN
   tree := Tree(NIL) ;
   IF sym#NulSym
   THEN
      n := 1 ;
      REPEAT
         GetRegInterface(sym, n, str, obj) ;
         IF str#NulSym
         THEN
            IF IsConstString(str)
            THEN
               DeclareConstant(GetDeclared(obj), obj) ;
               tree := ChainOnParamValue(tree, PromoteToString(GetDeclared(str), str), Mod2Gcc(obj))
            ELSE
               WriteFormat0('a constraint to the GNU ASM statement must be a constant string')
            END
         END ;
         INC(n)
      UNTIL (str=NulSym) AND (obj=NulSym) ;
   END ;
   RETURN( tree )
END BuildTreeFromInterface ;


(*
   BuildTrashTreeFromInterface - generates a GCC string tree from an interface definition.
*)

PROCEDURE BuildTrashTreeFromInterface (sym: CARDINAL) : Tree ;
VAR
   n   : CARDINAL ;
   str,
   obj : CARDINAL ;
   tree: Tree ;
BEGIN
   tree := Tree(NIL) ;
   IF sym#NulSym
   THEN
      n := 1 ;
      REPEAT
         GetRegInterface(sym, n, str, obj) ;
         IF str#NulSym
         THEN
            IF IsConstString(str)
            THEN
               tree := AddStringToTreeList(tree, PromoteToString(GetDeclared(str), str))
            ELSE
               WriteFormat0('a constraint to the GNU ASM statement must be a constant string')
            END
         END ;
         IF obj#NulSym
         THEN
            InternalError('not expecting the object to be non null in the trash list', __FILE__, __LINE__)
         END ;
         INC(n)
      UNTIL (str=NulSym) AND (obj=NulSym)
   END ;
   RETURN( tree )
END BuildTrashTreeFromInterface ;


(*
   CodeInline - InlineOp is a quadruple which has the following format:

                InlineOp   NulSym  NulSym  Sym

                The inline asm statement, Sym, is written to standard output.
*)

PROCEDURE CodeInline (q: CARDINAL; op1, op2, GnuAsm: CARDINAL) ;
VAR
   string  : CARDINAL ;
   inputs,
   outputs,
   trash   : Tree ;
BEGIN
   (*
      no need to explicity flush the outstanding instructions as
      per M2GenDyn486 and M2GenAPU. The GNU ASM statements in GCC
      can handle the register dependency providing the user
      specifies VOLATILE and input/output/trash sets correctly.
   *)
   inputs  := BuildTreeFromInterface(GetGnuAsmInput(GnuAsm)) ;
   outputs := BuildTreeFromInterface(GetGnuAsmOutput(GnuAsm)) ;
   trash   := BuildTrashTreeFromInterface(GetGnuAsmTrash(GnuAsm)) ;
   string  := GetGnuAsm(GnuAsm) ;
   DeclareConstant(CurrentQuadToken, string) ;
   BuildAsm(Mod2Gcc(string), IsGnuAsmVolatile(GnuAsm), inputs, outputs, trash)
END CodeInline ;


(*
   CodeLineNumber - LineNumberOp is a quadruple which has the following format:

                    LineNumberOp   NulSym  NulSym  LineNo

                    The line number, LineNo, is ommited if we are producing
                    runtime debugging information.
*)

PROCEDURE CodeLineNumber (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF LastOperator#LineNumberOp
   THEN
      FileName := KillString(FileName) ;
      FileName := InitStringCharStar(KeyToCharStar(Name(op1))) ;
      SetFileNameAndLineNo(string(FileName), op3) ;
      EmitLineNote(string(FileName), op3)
   END
END CodeLineNumber ;


(*
   FoldRange - attempts to fold the range test.
               --fixme-- complete this
*)

PROCEDURE FoldRange (tokenno: CARDINAL; p: WalkAction;
                     q: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldRangeCheck(tokenno, q, op3)
END FoldRange ;


(*
   CodeSaveException - op1 := op3(TRUE)
*)

PROCEDURE CodeSaveException (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t: Tree ;
BEGIN
   BuildParam(Mod2Gcc(True)) ;
   t := BuildProcedureCallTree(Mod2Gcc(op3), Mod2Gcc(GetType(op3))) ;
   BuildFunctValue(Mod2Gcc(op1))
END CodeSaveException ;


(*
   CodeRestoreException - op1 := op3(op1)
*)

PROCEDURE CodeRestoreException (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t: Tree ;
BEGIN
   BuildParam(Mod2Gcc(op1)) ;
   t := BuildProcedureCallTree(Mod2Gcc(op3), Mod2Gcc(GetType(op3))) ;
   BuildFunctValue(Mod2Gcc(op1))
END CodeRestoreException ;


(*
   PushScope - 
*)

PROCEDURE PushScope (sym: CARDINAL) ;
BEGIN
   PushWord(ScopeStack, sym)
END PushScope ;


(*
   PopScope - 
*)

PROCEDURE PopScope ;
VAR
   sym: CARDINAL ;
BEGIN
   sym := PopWord(ScopeStack)
END PopScope ;


(*
   GetCurrentScopeDescription - returns a description of the current scope.
*)

PROCEDURE GetCurrentScopeDescription () : String ;
VAR
   sym : CARDINAL ;
   s, n: String ;
BEGIN
   IF IsEmptyWord(ScopeStack)
   THEN
      InternalError('not expecting scope stack to be empty', __FILE__, __LINE__)
   ELSE
      sym := PeepWord(ScopeStack, 1) ;
      n := Mark(InitStringCharStar(KeyToCharStar(GetSymName(sym)))) ;
      IF IsDefImp(sym)
      THEN
         RETURN( Sprintf1(Mark(InitString('implementation module %s')), n) )
      ELSIF IsModule(sym)
      THEN
         IF IsInnerModule(sym)
         THEN
            RETURN( Sprintf1(Mark(InitString('inner module %s')), n) )
         ELSE
            RETURN( Sprintf1(Mark(InitString('program module %s')), n) )
         END
      ELSIF IsProcedure(sym)
      THEN
         IF IsProcedureNested(sym)
         THEN
            RETURN( Sprintf1(Mark(InitString('nested procedure %s')), n) )
         ELSE
            RETURN( Sprintf1(Mark(InitString('procedure %s')), n) )
         END
      ELSE
         InternalError('unexpected scope symbol', __FILE__, __LINE__)
      END
   END
END GetCurrentScopeDescription ;


(*
   CodeRange - encode the range test associated with op3.
*)

PROCEDURE CodeRange (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeRangeCheck(op3, GetCurrentScopeDescription())
END CodeRange ;


(*
   CodeError - encode the error test associated with op3.
*)

PROCEDURE CodeError (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   (* would like to test whether this position is in the same basicblock
      as any known entry point.  If so we could emit an error message.
   *)
   AddStatement(CodeErrorCheck(op3, GetCurrentScopeDescription()))
END CodeError ;


(*
   CodeModuleScope - ModuleScopeOp is a quadruple which has the following
                     format:

                     ModuleScopeOp  _  _  ModuleSym

                     Its function is to reset the source file to another
                     file, hence all line numbers emitted with the
                     generated code will be relative to this source file.
*)

PROCEDURE CodeModuleScope (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   PushScope(op3) ;
   ModuleName := KillString(ModuleName) ;
   ModuleName := InitStringCharStar(KeyToCharStar(GetSymName(op3))) ;
   SetFileNameAndLineNo(KeyToCharStar(Name(op2)), op1) ;
   EmitLineNote(KeyToCharStar(Name(op2)), op1)
END CodeModuleScope ;


(*
   CodeStartModFile - StartModFileOp is a quadruple which has the following
                      format:

                      StartModFileOp  _  _  ModuleSym

                      Its function is to reset the source file to another
                      file, hence all line numbers emitted with the
                      generated code will be relative to this source file.
*)

PROCEDURE CodeStartModFile (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   LastLine := 1 ;
   PushScope(op3) ;
   PushWord(CompilingMainModuleStack, GetMainModule()=op3) ;
   ModuleName := KillString(ModuleName) ;
   ModuleName := InitStringCharStar(KeyToCharStar(GetSymName(op3))) ;
   IF PeepWord(CompilingMainModuleStack, 1)=TRUE
   THEN
      SetFileNameAndLineNo(KeyToCharStar(Name(op2)), op1) ;
      EmitLineNote(KeyToCharStar(Name(op2)), op1)
   END
END CodeStartModFile ;


(*
   CodeStartDefFile - StartDefFileOp is a quadruple which has the following
                      format:

                      StartDefFileOp  _  _  ModuleSym

                      Its function is to reset the source file to another
                      file, hence all line numbers emitted with the
                      generated code will be relative to this source file.
*)

PROCEDURE CodeStartDefFile (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   PushScope(op3) ;
   LastLine := 1 ;
   PushWord(CompilingMainModuleStack, FALSE) ;
   ModuleName := KillString(ModuleName) ;
   ModuleName := InitStringCharStar(KeyToCharStar(GetSymName(op3)))
END CodeStartDefFile ;


(*
   CodeEndFile - FileOp is a quadruple which has the following format:

                 EndFileOp  _  _  ModuleSym

                 Its function is to reset the source file to another
                 file, hence all line numbers emitted with the
                 generated code will be relative to this source file.
*)

PROCEDURE CodeEndFile (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   ReduceWord(CompilingMainModuleStack, 1) ;
(*   PopScope *)
END CodeEndFile ;


(*
   CallInnerInit - produce a call to inner module initialization routine.
*)

PROCEDURE CallInnerInit (Sym: WORD) ;
BEGIN
   BuildCallInner(Mod2Gcc(Sym))
END CallInnerInit ;


(*
   CallInnerFinally - produce a call to inner module finalization routine.
*)

PROCEDURE CallInnerFinally (Sym: WORD) ;
BEGIN
   BuildCallInner(GetModuleFinallyFunction(Sym))
END CallInnerFinally ;


(*
   CodeInitStart - emits starting code before the main BEGIN END of the
                   current module.
*)

PROCEDURE CodeInitStart (quad: CARDINAL; op1, op2, op3: CARDINAL;
                         CompilingMainModule: BOOLEAN) ;
VAR
   CurrentModuleInitFunction: Tree ;
BEGIN
   IF CompilingMainModule
   THEN
      SetFileNameAndLineNo(string(FileName), op1) ;
      IF IsModuleWithinProcedure(op3)
      THEN
         CurrentModuleInitFunction := Mod2Gcc(op3) ;
         BuildStartFunctionCode(CurrentModuleInitFunction, FALSE, FALSE)
      ELSE
         CurrentModuleInitFunction := BuildStart(KeyToCharStar(GetModuleInitName(op3)), op1, op2#op3) ;
         AddModGcc(op3, CurrentModuleInitFunction)
      END ;
      EmitLineNote(string(FileName), op1) ;
      ForeachInnerModuleDo(op3, CallInnerInit)
   END
END CodeInitStart ;


(*
   BuildTerminationCall - generates a call to the termination handler.
                          After checking that, module, is a MODULE and
                          is also the main module.
*)

(*
PROCEDURE BuildTerminationCall (module: CARDINAL) ;
BEGIN
   IF (GetMainModule()=module) AND IsModule(module)
   THEN
      IF Pim
      THEN
         CodeDirectCall(FromModuleGetSym(MakeKey('Terminate'),
                                         GetModule(MakeKey('M2RTS'))))
      END
   END
END BuildTerminationCall ;
*)

(*
   CodeInitEnd - emits terminating code after the main BEGIN END of the
                 current module.
*)

PROCEDURE CodeInitEnd (quad: CARDINAL; op1, op2, op3: CARDINAL;
                       CompilingMainModule: BOOLEAN) ;
BEGIN
   IF CompilingMainModule
   THEN
      SetFileNameAndLineNo(string(FileName), op1) ;
      EmitLineNote(string(FileName), op1) ;
      IF IsModuleWithinProcedure(op3)
      THEN
         BuildEndFunctionCode(Mod2Gcc(op3), TRUE)
      ELSE
         BuildEnd(Mod2Gcc(op3), FALSE)
      END
   END
END CodeInitEnd ;


(*
   CodeFinallyStart - emits starting code before the main BEGIN END of the
                      current module.
*)

PROCEDURE CodeFinallyStart (quad: CARDINAL; op1, op2, op3: CARDINAL;
                            CompilingMainModule: BOOLEAN) ;
VAR
   CurrentModuleFinallyFunction: Tree ;
BEGIN
   IF CompilingMainModule
   THEN
      SetFileNameAndLineNo(string(FileName), op1) ;
      IF IsModuleWithinProcedure(op3)
      THEN
         CurrentModuleFinallyFunction := GetModuleFinallyFunction(op3) ;
         BuildStartFunctionCode(CurrentModuleFinallyFunction, FALSE, FALSE)
      ELSE
         CurrentModuleFinallyFunction := BuildStart(KeyToCharStar(GetModuleFinallyName(op3)), op1, op2#op3) ;
         PutModuleFinallyFunction(op3, CurrentModuleFinallyFunction)
      END ;
      EmitLineNote(string(FileName), op1) ;
      ForeachInnerModuleDo(op3, CallInnerFinally)
   END
END CodeFinallyStart ;


(*
   CodeFinallyEnd - emits terminating code after the main BEGIN END of the
                    current module.
*)

PROCEDURE CodeFinallyEnd (quad: CARDINAL; op1, op2, op3: CARDINAL;
                          CompilingMainModule: BOOLEAN) ;
BEGIN
   IF CompilingMainModule
   THEN
      SetFileNameAndLineNo(string(FileName), op1) ;
      EmitLineNote(string(FileName), op1) ;
      IF IsModuleWithinProcedure(op3)
      THEN
         BuildEndFunctionCode(GetModuleFinallyFunction(op3), TRUE)
      ELSE
         BuildEnd(GetModuleFinallyFunction(op3), FALSE)
      END
   END
END CodeFinallyEnd ;


(*
   GetAddressOfUnbounded - returns the address of the unbounded array contents.
*)

PROCEDURE GetAddressOfUnbounded (param: CARDINAL) : Tree ;
VAR
   UnboundedType: CARDINAL ;
BEGIN
   UnboundedType := GetType(param) ;
   Assert(IsUnbounded(UnboundedType)) ;

   RETURN( BuildIndirect(BuildAdd(BuildAddr(Mod2Gcc(param), FALSE),
                                  BuildOffset1(Mod2Gcc(GetUnboundedAddressOffset(UnboundedType)),
                                               FALSE),
                                  FALSE),
                         GetPointerType()) )
END GetAddressOfUnbounded ;


(*
   GetHighFromUnbounded - returns a Tree containing the value of
                          param.HIGH.
*)

PROCEDURE GetHighFromUnbounded (dim, param: CARDINAL) : Tree ;
VAR
   UnboundedType,
   ArrayType    : CARDINAL ;
BEGIN
   UnboundedType := GetType(param) ;
   Assert(IsUnbounded(UnboundedType)) ;
   ArrayType := GetType(UnboundedType) ;

   RETURN( BuildIndirect(BuildAdd(BuildAddr(Mod2Gcc(param), FALSE),
                                  BuildOffset1(Mod2Gcc(GetUnboundedHighOffset(UnboundedType, dim)), FALSE),
                                  FALSE),
                         GetIntegerType()) )
END GetHighFromUnbounded ;


(*
   GetSizeOfHighFromUnbounded - returns a Tree containing the value of
                                param.HIGH * sizeof(unboundedType).
                                The number of legal bytes this array
                                occupies.
*)

PROCEDURE GetSizeOfHighFromUnbounded (param: CARDINAL) : Tree ;
VAR
   t            : Tree ;
   UnboundedType,
   ArrayType    : CARDINAL ;
   i, n         : CARDINAL ;
BEGIN
   UnboundedType := GetType(param) ;
   Assert(IsUnbounded(UnboundedType)) ;
   ArrayType := GetType(UnboundedType) ;

   i := 1 ;
   n := GetDimension(UnboundedType) ;
   t := GetIntegerZero() ;
   WHILE i<=n DO
      t := BuildAdd(BuildAdd(GetHighFromUnbounded(i, param),
                             GetIntegerOne(),
                             FALSE),
                    t, FALSE) ;
      (* remember we must add one as HIGH(a) means we can legally reference a[HIGH(a)] *)      
      INC(i)
   END ;
   RETURN( BuildConvert(GetIntegerType (),
                        BuildMult(t, FindSize(ArrayType), FALSE),
                        FALSE) )
END GetSizeOfHighFromUnbounded ;


(*
   MakeCopyAndUse - make a copy of the unbounded array and alter all references
                    from the old unbounded array to the new unbounded array.
                    The parameter, param, contains a RECORD
                                                        ArrayAddress: ADDRESS ;
                                                        ArrayHigh   : CARDINAL ;
                                                     END
                    we simply declare a new array of size, ArrayHigh
                    and set ArrayAddress to the address of the copy.

                    Remember ArrayHigh == sizeof(Array)-sizeof(typeof(array))
                             so we add 1 for the size and add 1 for a possible <nul>
*)

PROCEDURE MakeCopyAndUse (proc, param, i: CARDINAL) ;
VAR
   UnboundedType,
   ArrayType    : CARDINAL ;
   t,
   Addr,
   High,
   GccIndex,
   GccArray,
   NewArray,
   Type         : Tree ;
BEGIN
   UnboundedType := GetType(param) ;
   Assert(IsUnbounded(UnboundedType)) ;

   High := GetSizeOfHighFromUnbounded(param) ;
   Addr := GetAddressOfUnbounded(param) ;
   Type := Mod2Gcc(GetType(param)) ;
   
   NewArray := BuiltInAlloca(High) ;
   NewArray := BuiltInMemCopy(NewArray, Addr, High) ;

   (* now assign  param.Addr := ADR(NewArray) *)

   t := BuildAssignmentTree(BuildIndirect(BuildAdd(BuildAddr(Mod2Gcc(param), FALSE),
                                                   BuildOffset1(Mod2Gcc(GetUnboundedAddressOffset(UnboundedType)), FALSE),
                                                   FALSE),
                                          GetPointerType()), NewArray)

END MakeCopyAndUse ;


(*
   GetParamAddress - returns the address of parameter, param.
*)

PROCEDURE GetParamAddress (proc, param: CARDINAL) : Tree ;
VAR
   sym,
   type: CARDINAL ;
BEGIN
   IF IsParameter(param)
   THEN
      type := GetType(param) ;
      sym := GetLocalSym(proc, GetSymName(param)) ;
      IF IsUnbounded(type)
      THEN
         RETURN( GetAddressOfUnbounded(sym) )
      ELSE
         Assert(GetMode(sym)=LeftValue) ;
         RETURN( Mod2Gcc(sym) )
      END
   ELSE
      Assert(IsVar(param)) ;
      Assert(GetMode(param)=LeftValue) ;
      RETURN( Mod2Gcc(param) )
   END
END GetParamAddress ;


(*
   IsUnboundedWrittenTo - returns TRUE if the unbounded parameter
                          might be written to, or if -funbounded-by-reference
                          was _not_ specified.
*)

PROCEDURE IsUnboundedWrittenTo (proc, param: CARDINAL) : BOOLEAN ;
VAR
   f     : String ;
   l     : CARDINAL ;
   sym   : CARDINAL ;
   n1, n2: Name ;
BEGIN
   sym := GetLocalSym(proc, GetSymName(param)) ;
   IF sym=NulSym
   THEN
      InternalError('should find symbol in table', __FILE__, __LINE__)
   ELSE
      IF UnboundedByReference
      THEN
         IF (NOT GetVarWritten(sym)) AND VerboseUnbounded
         THEN
            n1 := GetSymName(sym) ;
            n2 := GetSymName(proc) ;
            f := FindFileNameFromToken(GetDeclared(sym), 0) ;
            l := TokenToLineNo(GetDeclared(sym), 0) ;
            printf4('%s:%d:non VAR unbounded parameter %a in procedure %a does not need to be copied\n',
                    f, l, n1, n2)
         END ;
         RETURN( GetVarWritten(sym) )
      ELSE
         RETURN( TRUE )
      END
   END
END IsUnboundedWrittenTo ;


(*

PROCEDURE BuildCascadedIfThenElsif (mustCheck: List;
                                    proc, param, i: CARDINAL) ;
VAR
   tl, tr: Tree ;
   n, j  : CARDINAL ;
   s     : String ;
BEGIN
   n := NoOfItemsInList(mustCheck) ;
   (* want a sequence of if then elsif statements *)
   IF n>0
   THEN
      INC(UnboundedLabelNo) ;
      j := 1 ;
      tr := GetAddressOfUnbounded(param) ;
      WHILE j<=n DO
         IF j>1
         THEN
            s := CreateLabelProcedureN(proc, UnboundedLabelNo, j) ;
            IF CascadedDebugging
            THEN
               printf1('label %s\n', s)
            END ;
            DeclareLabel(string(s))
         END ;
         tl := GetParamAddress(proc, GetItemFromList(mustCheck, j)) ;
         s := InitStringCharStar(KeyToCharStar(GetSymName(GetItemFromList(mustCheck, j)))) ;
         IF CascadedDebugging
         THEN
            printf1('if %s # ', s)
         END ;
         s := InitStringCharStar(KeyToCharStar(GetSymName(param))) ;
         IF CascadedDebugging
         THEN
            printf1('%s', s)
         END ;
         s := CreateLabelProcedureN(proc, UnboundedLabelNo, j+1) ;
         IF CascadedDebugging
         THEN
            printf1(' then goto %s\n', s)
         END ;
         DoJump(BuildNotEqualTo(tl, tr), NIL, string(s)) ;
         s := InitStringCharStar(KeyToCharStar(GetSymName(param))) ;
         IF CascadedDebugging
         THEN
            printf1('make copy of %s\n', s)
         END ;
         MakeCopyAndUse(proc, param, i) ;
         IF j<n
         THEN
            s := CreateLabelProcedureN(proc, UnboundedLabelNo, n+1) ;
            IF CascadedDebugging
            THEN
               printf1('goto exit %s\n', s)
            END ;
            BuildGoto(string(s))
         END ;
         INC(j)
      END ;
      s := CreateLabelProcedureN(proc, UnboundedLabelNo, n+1) ;
      IF CascadedDebugging
      THEN
         printf1('label %s\n', s)
      END ;
      DeclareLabel(string(s)) ;
   END
END BuildCascadedIfThenElsif ;
*)


(*
   GetParamSize - returns the size in bytes of, param.
*)

PROCEDURE GetParamSize (param: CARDINAL) : Tree ;
BEGIN
   Assert(IsVar(param) OR IsParameter(param)) ;
   IF IsUnbounded(param)
   THEN
      RETURN GetSizeOfHighFromUnbounded(param)
   ELSE
      RETURN BuildSize(Mod2Gcc(GetType(param)), FALSE)
   END
END GetParamSize ;


(*
   DoIsIntersection - jumps to, tLabel, if the ranges i1..i2  j1..j2 overlap
                      else jump to, fLabel.
*)

PROCEDURE DoIsIntersection(ta, tb, tc, td: Tree; tLabel, fLabel: String) ;
BEGIN
   (*
     if (ta>td) OR (tb<tc)
     then
        goto fLabel
     else
        goto tLabel
     fi
   *)
   DoJump(BuildGreaterThan(ta, td), NIL, string(fLabel)) ;
   DoJump(BuildLessThan(tb, tc), NIL, string(fLabel)) ;
   BuildGoto(string(tLabel))
END DoIsIntersection ;


(*
   BuildCascadedIfThenElsif - mustCheck contains a list of variables which
                              must be checked against the address of (proc, param, i).
                              If the address matches we make a copy of the unbounded
                              parameter (proc, param, i) and quit further checking.
*)

PROCEDURE BuildCascadedIfThenElsif (mustCheck: List;
                                    proc, param, i: CARDINAL) ;
VAR
   ta, tb,
   tc, td: Tree ;
   n, j  : CARDINAL ;
   s,
   tLabel,
   fLabel,
   nLabel: String ;
BEGIN
   n := NoOfItemsInList(mustCheck) ;
   (* want a sequence of if then elsif statements *)
   IF n>0
   THEN
      INC(UnboundedLabelNo) ;
      j := 1 ;
      ta := GetAddressOfUnbounded(param) ;
      tb := BuildConvert(GetPointerType(),
                         BuildAdd(ta, GetSizeOfHighFromUnbounded(param), FALSE),
                         FALSE) ;
      WHILE j<=n DO
         IF j>1
         THEN
            nLabel := CreateLabelProcedureN(proc, "n", UnboundedLabelNo, j) ;
            IF CascadedDebugging
            THEN
               printf1('label %s\n', nLabel)
            END ;
            DeclareLabel(string(nLabel))
         END ;
         tc := GetParamAddress(proc, GetItemFromList(mustCheck, j)) ;
         td := BuildConvert(GetPointerType(),
                            BuildAdd(tc, GetParamSize(param), FALSE),
                            FALSE) ;
         tLabel := CreateLabelProcedureN(proc, "t", UnboundedLabelNo, j+1) ;
         fLabel := CreateLabelProcedureN(proc, "f", UnboundedLabelNo, j+1) ;
         DoIsIntersection(ta, tb, tc, td, tLabel, fLabel) ;
         DeclareLabel(string(tLabel)) ;
         MakeCopyAndUse(proc, param, i) ;
         IF j<n
         THEN
            nLabel := CreateLabelProcedureN(proc, "n", UnboundedLabelNo, n+1) ;
            BuildGoto(string(nLabel))
         END ;
         DeclareLabel(string(fLabel)) ;
         INC(j)
      END ;
      nLabel := CreateLabelProcedureN(proc, "fin", UnboundedLabelNo, n+1) ;
      DeclareLabel(string(nLabel))
   END
END BuildCascadedIfThenElsif ;


(*
   CheckUnboundedNonVarParameter - if non var unbounded parameter is written to
                                   then
                                      make a copy of the contents of this parameter
                                      and use the copy
                                   else if param
                                      is type compatible with any parameter, symv
                                      and at runtime its address matches symv
                                   then
                                      make a copy of the contents of this parameter
                                      and use the copy
                                   fi
*)

PROCEDURE CheckUnboundedNonVarParameter (trashed: List;
                                         proc, param, i: CARDINAL) ;
VAR
   mustCheck   : List ;
   paramTrashed,
   n, j        : CARDINAL ;
   f           : String ;
   l           : CARDINAL ;
   n1, n2      : Name ;
BEGIN
   IF IsUnboundedWrittenTo(proc, param)
   THEN
      MakeCopyAndUse(proc, param, i)
   ELSE
      InitList(mustCheck) ;
      n := NoOfItemsInList(trashed) ;
      j := 1 ;
      WHILE j<=n DO
         paramTrashed := GetItemFromList(trashed, j) ;
         IF IsAssignmentCompatible(GetLowestType(param), GetLowestType(paramTrashed))
         THEN
            (* we must check whether this unbounded parameter has the same
               address as the trashed parameter *)
            IF VerboseUnbounded
            THEN
               n1 := GetSymName(paramTrashed) ;
               n2 := GetSymName(proc) ;
               f := FindFileNameFromToken(GetDeclared(paramTrashed), 0) ;
               l := TokenToLineNo(GetDeclared(paramTrashed), 0) ;
               printf4('%s:%d:must check at runtime the address of parameter, %a, in procedure, %a, whose contents will be trashed\n',
                       f, l, n1, n2) ;
               n1 := GetSymName(param) ;
               n2 := GetSymName(paramTrashed) ;
               printf4('%s:%d:against address of parameter, %a, possibly resulting in a copy of parameter, %a\n',
                       f, l, n1, n2)
            END ;
            PutItemIntoList(mustCheck, paramTrashed)
         END ;
         INC(j)
      END ;
      (* now we build a sequence of if then { elsif then } end to check addresses *)
      BuildCascadedIfThenElsif(mustCheck, proc, param, i) ;
      KillList(mustCheck)
   END
END CheckUnboundedNonVarParameter ;


(*
   IsParameterWritten - returns TRUE if a parameter, sym, is written to.
*)

PROCEDURE IsParameterWritten (proc: CARDINAL; sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsParameter(sym)
   THEN
      sym := GetLocalSym(proc, GetSymName(sym))
   END ;
   IF IsVar(sym)
   THEN
      (* unbounded arrays will appear as vars *)
      RETURN GetVarWritten(sym)
   END ;
   InternalError('expecting IsVar to return TRUE', __FILE__, __LINE__)
END IsParameterWritten ;


(*
   SaveNonVarUnboundedParameters - for each var parameter, symv, do
                                      (* not just unbounded var parameters, but _all_
                                         parameters *)
                                      if symv is written to
                                      then
                                         add symv to a compile list
                                      fi
                                   done

                                   for each parameter of procedure, symu, do
                                      if non var unbounded parameter is written to
                                      then
                                         make a copy of the contents of this parameter
                                         and use the copy
                                      else if
                                         symu is type compatible with any parameter, symv
                                         and at runtime its address matches symv
                                      then
                                         make a copy of the contents of this parameter
                                         and use the copy
                                      fi
                                   done
*)

PROCEDURE SaveNonVarUnboundedParameters (proc: CARDINAL) ;
VAR
   i, p   : CARDINAL ;
   trashed: List ;
   f      : String ;
   sym    : CARDINAL ;
   l      : CARDINAL ;
   n1, n2 : Name ;
BEGIN
   InitList(trashed) ;
   i := 1 ;
   p := NoOfParam(proc) ;
   WHILE i<=p DO
      sym := GetNthParam(proc, i) ;
      IF IsParameterWritten(proc, sym)
      THEN
         IF VerboseUnbounded
         THEN
            n1 := GetSymName(sym) ;
            n2 := GetSymName(proc) ;
            f := FindFileNameFromToken(GetDeclared(sym), 0) ;
            l := TokenToLineNo(GetDeclared(sym), 0) ;
            printf4('%s:%d:parameter, %a, in procedure, %a, is trashed\n',
                    f, l, n1, n2)
         END ;
         PutItemIntoList(trashed, sym)
      END ;
      INC(i)
   END ;
   (* now see whether we need to copy any unbounded array parameters *)
   i := 1 ;
   p := NoOfParam(proc) ;
   WHILE i<=p DO
      IF IsUnboundedParam(proc, i) AND (NOT IsVarParam(proc, i))
      THEN
         CheckUnboundedNonVarParameter(trashed, proc, GetNth(proc, i), i)
      END ;
      INC(i)
   END ;
   KillList(trashed)
END SaveNonVarUnboundedParameters ;


(*
   CodeNewLocalVar - Builds a new frame on the stack to contain the procedure
                     local variables.
*)

PROCEDURE CodeNewLocalVar (quad: CARDINAL;
                           LineNo, PreviousScope, CurrentProcedure: CARDINAL) ;
BEGIN
   (* callee saves non var unbounded parameter contents *)
   SaveNonVarUnboundedParameters(CurrentProcedure) ;
   EmitLineNote(string(FileName), LineNo) ;
   BuildPushFunctionContext ;
   ForeachProcedureDo(CurrentProcedure, CodeBlock) ;
   ForeachInnerModuleDo(CurrentProcedure, CodeBlock) ;
   BuildPopFunctionContext ;
   ForeachInnerModuleDo(CurrentProcedure, CallInnerInit)
END CodeNewLocalVar ;


(*
   CodeKillLocalVar - removes local variables and returns to previous scope.
*)

PROCEDURE CodeKillLocalVar (quad: CARDINAL;
                            LineNo, op2, CurrentProcedure: CARDINAL) ;
BEGIN
   SetFileNameAndLineNo(string(FileName), LineNo) ;
   BuildEndFunctionCode(Mod2Gcc(CurrentProcedure),
                        IsProcedureGccNested(CurrentProcedure)) ;
   PoisonSymbols(CurrentProcedure)
END CodeKillLocalVar ;


(*
   CodeProcedureScope -
*)

PROCEDURE CodeProcedureScope (quad: CARDINAL;
                              LineNo, PreviousScope, CurrentProcedure: CARDINAL) ;
BEGIN
   ModuleName := KillString(ModuleName) ;
   ModuleName := InitStringCharStar(KeyToCharStar(GetSymName(GetMainModule()))) ;
   (* SetFileNameAndLineNo(string(FileName), LineNo) ; *)
   BuildStartFunctionCode(Mod2Gcc(CurrentProcedure),
                          IsExported(GetMainModule(), CurrentProcedure),
                          IsProcedureInline(CurrentProcedure)) ;
   StartDeclareScope(CurrentProcedure) ;
   (* DeclareParameters(CurrentProcedure) *)
END CodeProcedureScope ;


(*
   CodeReturn - does nothing, as the return is done by KillLocalVar.
*)

PROCEDURE CodeReturn (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
END CodeReturn ;


(*
   CodeReturnValue - places the operand into the return value space
                     allocated by the function call.
*)

PROCEDURE CodeReturnValue (quad: CARDINAL; res, op2, Procedure: CARDINAL) ;
VAR
   t, op3t: Tree ;
BEGIN
   TryDeclareConstant(CurrentQuadToken, res) ;  (* checks to see whether it is a constant and declares it *)
   TryDeclareConstructor(quad, res) ;
   IF IsConstString(res) AND (SkipTypeAndSubrange(GetType(Procedure))#Char)
   THEN
      DoCopyString(t, op3t, GetType(Procedure), res) ;
      t := BuildArrayStringConstructor(Mod2Gcc(GetType(Procedure)), op3t, t)
   ELSE
      t := Mod2Gcc(res)
   END ;
   BuildReturnValueCode(Mod2Gcc(Procedure), t)
END CodeReturnValue ;


(*
   CodeCall - determines whether the procedure call is a direct call
              or an indirect procedure call.
*)

PROCEDURE CodeCall (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   (*
      op  : CallOp
      op3 : Procedure
   *)
   IF IsProcedure(op3)
   THEN
      DeclareParameters(op3) ;
      CodeDirectCall(op3)
   ELSIF IsProcType(GetType(op3))
   THEN
      DeclareParameters(GetType(op3)) ;
      CodeIndirectCall(op3)
   ELSE
      InternalError('Expecting Procedure or ProcType', __FILE__, __LINE__)
   END
END CodeCall ;


(*
   CanUseBuiltin - returns TRUE if the procedure, Sym, can be
                   inlined via a builtin function.
*)

PROCEDURE CanUseBuiltin (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( BuiltinExists(KeyToCharStar(GetProcedureBuiltin(Sym))) OR
           BuiltinExists(KeyToCharStar(GetSymName(Sym))) )
END CanUseBuiltin ;


(*
   UseBuiltin - returns a Tree containing the builtin function
                and parameters. It should only be called if
                CanUseBuiltin returns TRUE.
*)

PROCEDURE UseBuiltin (Sym: CARDINAL) : Tree ;
BEGIN
   IF BuiltinExists(KeyToCharStar(GetProcedureBuiltin(Sym)))
   THEN
      RETURN( BuildBuiltinTree(KeyToCharStar(GetProcedureBuiltin(Sym))) )
   ELSE
      RETURN( BuildBuiltinTree(KeyToCharStar(GetSymName(Sym))) )
   END
END UseBuiltin ;


(*
   CodeDirectCall - saves all volitiles and jumps to a subroutine.
*)

PROCEDURE CodeDirectCall (procedure: CARDINAL) ;
VAR
   tree: Tree ;
BEGIN
   IF IsProcedureBuiltin(procedure) AND CanUseBuiltin(procedure)
   THEN
      tree := UseBuiltin(procedure)
   ELSE
      IF GetType(procedure)=NulSym
      THEN
         tree := BuildProcedureCallTree(Mod2Gcc(procedure), NIL)
      ELSE
         tree := BuildProcedureCallTree(Mod2Gcc(procedure), Mod2Gcc(GetType(procedure)))
      END
   END ;
   IF GetType(procedure)=NulSym
   THEN
      AddStatement(tree)
   ELSE
      (* leave tree alone - as it will be picked up when processing FunctValue *)
   END
END CodeDirectCall ;


(*
   CodeIndirectCall - saves all volitiles and jumps to a subroutine.
*)

PROCEDURE CodeIndirectCall (ProcVar: CARDINAL) ;
VAR
   tree,
   ReturnType: Tree ;
   proc      : CARDINAL ;
BEGIN
   proc := GetType(ProcVar) ;
   IF GetType(proc)=NulSym
   THEN
      ReturnType := Tree(NIL)
   ELSE
      ReturnType := Tree(Mod2Gcc(GetType(proc)))
   END ;

   (* now we dereference the lvalue if necessary *)

   IF GetMode(ProcVar)=LeftValue
   THEN
      tree := BuildIndirectProcedureCallTree(BuildIndirect(Mod2Gcc(ProcVar), Mod2Gcc(proc)),
                                             ReturnType)
   ELSE
      tree := BuildIndirectProcedureCallTree(Mod2Gcc(ProcVar), ReturnType)
   END
END CodeIndirectCall ;


(*
   StringToChar - if type=Char and str is a string (of size <= 1)
                  then convert the string into a character constant.
*)

PROCEDURE StringToChar (t: Tree; type, str: CARDINAL) : Tree ;
VAR
   s: String ;
   n: Name ;
BEGIN
   type := SkipType(type) ;
   IF (type=Char) AND IsConstString(str)
   THEN
      IF GetStringLength(str)=0
      THEN
         s := InitString('') ;
         t := BuildCharConstant(s) ;
         s := KillString(s) ;
      ELSIF GetStringLength(str)>1
      THEN
         n := GetSymName(str) ;
         WriteFormat1("type incompatibility, attempting to use a string ('%a') when a CHAR is expected", n) ;
         s := InitString('') ;  (* do something safe *)
         t := BuildCharConstant(s)
      END ;
      s := InitStringCharStar(KeyToCharStar(GetString(str))) ;
      s := Slice(s, 0, 1) ;
      t := BuildCharConstant(string(s)) ;
      s := KillString(s) ;
   END ;
   RETURN( t )
END StringToChar ;


(*
   SafeConvert - converts, sym, into a tree which is type compatible with, with.
*)

PROCEDURE SafeConvert (sym, with: CARDINAL) : Tree ;
VAR
   t: Tree ;
BEGIN
   t := StringToChar(NIL, GetType(with), sym) ;
   IF t=NIL
   THEN
      RETURN( ZConstToTypedConst(LValueToGenericPtr(sym), sym, with) )
   ELSE
      RETURN( t )
   END
END SafeConvert ;


(*
   IsCoerceableParameter - returns TRUE if symbol, sym, is a
                           coerceable parameter.
*)

PROCEDURE IsCoerceableParameter (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          IsSet(sym) OR
          (IsOrdinalType(sym) AND (sym#Boolean) AND (NOT IsEnumeration(sym))) OR
          IsAComplexType(sym) OR IsRealType(sym) OR
          IsComplexN(sym) OR IsRealN(sym) OR IsSetN(sym)
         )
END IsCoerceableParameter ;


(*
   CheckConvertCoerceParameter - 
*)

PROCEDURE CheckConvertCoerceParameter (op1, op2, op3: CARDINAL) : Tree ;
VAR
   OperandType,
   ParamType  : CARDINAL ;
BEGIN
   IF GetNthParam(op2, op1)=NulSym
   THEN
      (*
       * We reach here if the argument is being passed to a C vararg function.
       *)
      RETURN( Mod2Gcc(op3) )
   ELSE
      OperandType := SkipType(GetType(op3)) ;
      ParamType := SkipType(GetType(GetNthParam(op2, op1)))
   END ;
   IF IsProcType(ParamType)
   THEN
      IF IsProcedure(op3)
      THEN
         RETURN( Mod2Gcc(op3) )
      ELSE
         RETURN( BuildConvert(Mod2Gcc(ParamType), Mod2Gcc(op3), FALSE) )
      END
   ELSIF IsRealType(OperandType) AND IsRealType(ParamType) AND
      (ParamType#OperandType)
   THEN
      (* SHORTREAL, LONGREAL and REAL conversion during parameter passing *)
      RETURN( BuildConvert(Mod2Gcc(ParamType),
                           Mod2Gcc(op3), FALSE) )
   ELSIF (OperandType#NulSym) AND IsSet(OperandType) AND IsConst(op3)
   THEN
      RETURN( DeclareKnownConstant(Mod2Gcc(ParamType),
                                   Mod2Gcc(op3)) )
   ELSIF IsConst(op3) AND
         (IsOrdinalType(ParamType) OR IsSystemType(ParamType))
   THEN
      RETURN( BuildConvert(Mod2Gcc(ParamType),
                           StringToChar(Mod2Gcc(op3), ParamType, op3),
                           FALSE) )
   ELSIF (OperandType#NulSym) AND IsCoerceableParameter(OperandType) AND (OperandType#ParamType)
   THEN
      RETURN( BuildConvert(Mod2Gcc(ParamType), Mod2Gcc(op3), FALSE) )
   ELSE
      RETURN( Mod2Gcc(op3) )
   END
END CheckConvertCoerceParameter ;


(*
   CheckConstant - checks to see whether we should declare the constant.
*)

PROCEDURE CheckConstant (des, expr: CARDINAL) : Tree ;
BEGIN
   IF IsProcedure(expr)
   THEN
      RETURN( Mod2Gcc(expr) )
   ELSE
      RETURN( DeclareKnownConstant(Mod2Gcc(GetType(des)), Mod2Gcc(expr)) )
   END
END CheckConstant ;


(*
   CodeMakeAdr - code the function MAKEADR.
*)

PROCEDURE CodeMakeAdr (q: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   r   : CARDINAL ;
   n   : CARDINAL ;
   type: CARDINAL ;
   op  : QuadOperator ;
   bits,
   max     : CARDINAL ;
   tmp,
   res,
   val : Tree ;
BEGIN
   n := q ;
   REPEAT
      IF op1>0
      THEN
         DeclareConstant(QuadToTokenNo(n), op3)
      END ;
      n := GetNextQuad(n) ;
      GetQuad(n, op, r, op2, op3)
   UNTIL op=FunctValueOp ;

   n := q ;
   GetQuad(n, op, op1, op2, op3) ;
   res := Mod2Gcc(r) ;
   max := GetSizeOfInBits(Mod2Gcc(Address)) ;
   bits := 0 ;
   val := GetPointerZero() ;
   REPEAT
      IF (op=ParamOp) AND (op1>0)
      THEN
         IF GetType(op3)=NulSym
         THEN
            WriteFormat0('must supply typed constants to MAKEADR')
         ELSE
            type := GetType(op3) ;
            tmp := BuildConvert(GetPointerType(), Mod2Gcc(op3), FALSE) ;
            IF bits>0
            THEN
               tmp := BuildLSL(tmp, BuildIntegerConstant(bits), FALSE)
            END ;
            INC(bits, GetSizeOfInBits(Mod2Gcc(type))) ;
            val := BuildLogicalOrAddress(val, tmp, FALSE)
         END
      END ;
      SubQuad(n) ;
      n := GetNextQuad(n) ;
      GetQuad(n, op, op1, op2, op3)
   UNTIL op=FunctValueOp ;
   IF bits>max
   THEN
      ErrorStringAt(InitString('total number of bit specified as parameters to MAKEADR exceeds address width'),
                    QuadToTokenNo(q))
   END ;
   SubQuad(n) ;
   res := BuildAssignmentTree(res, val)
END CodeMakeAdr ;


(*
   CodeBuiltinFunction - attempts to inline a function. Currently it only
                         inlines the SYSTEM function MAKEADR.
*)

PROCEDURE CodeBuiltinFunction (q: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF (op1=0) AND (op3=MakeAdr)
   THEN
      CodeMakeAdr(q, op1, op2, op3)
   END
END CodeBuiltinFunction ;


(*
   FoldMakeAdr - attempts to fold the function MAKEADR.
*)

PROCEDURE FoldMakeAdr (tokenno: CARDINAL; p: WalkAction;
                       q: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   resolved: BOOLEAN ;
   r       : CARDINAL ;
   n       : CARDINAL ;
   op      : QuadOperator ;
   type    : CARDINAL ;
   bits,
   max     : CARDINAL ;
   tmp,
   val,
   res     : Tree ;
BEGIN
   resolved := TRUE ;
   n := q ;
   r := op1 ;
   REPEAT
      IF r>0
      THEN
         TryDeclareConstant(QuadToTokenNo(n), op3) ;
         IF NOT GccKnowsAbout(op3)
         THEN
            resolved := FALSE
         END
      END ;
      n := GetNextQuad(n) ;
      GetQuad(n, op, r, op2, op3)
   UNTIL op=FunctValueOp ;

   IF resolved AND IsConst(r)
   THEN
      n := q ;
      GetQuad(n, op, op1, op2, op3) ;
      max := GetSizeOfInBits(Mod2Gcc(Address)) ;
      bits := 0 ;
      val := GetPointerZero() ;
      REPEAT
         IF (op=ParamOp) AND (op1>0)
         THEN
            IF GetType(op3)=NulSym
            THEN
               WriteFormat0('must supply typed constants to MAKEADR')
            ELSE
               type := GetType(op3) ;
               tmp := BuildConvert(GetPointerType(), Mod2Gcc(op3), FALSE) ;
               IF bits>0
               THEN
                  tmp := BuildLSL(tmp, BuildIntegerConstant(bits), FALSE)
               END ;
               INC(bits, GetSizeOfInBits(Mod2Gcc(type))) ;
               val := BuildLogicalOrAddress(val, tmp, FALSE)
            END
         END ;
         SubQuad(n) ;
         n := GetNextQuad(n) ;
         GetQuad(n, op, op1, op2, op3)
      UNTIL op=FunctValueOp ;
      IF bits>max
      THEN
         ErrorStringAt(InitString('total number of bit specified as parameters to MAKEADR exceeds address width'),
                       QuadToTokenNo(q))
      END ;
      PutConst(r, Address) ;
      AddModGcc(r, DeclareKnownConstant(Mod2Gcc(Address), val)) ;
      p(r) ;
      NoChange := FALSE ;
      SubQuad(n)
   END
END FoldMakeAdr ;


(*
   doParam - builds the parameter, op3, which is to be passed to
             procedure, op2.  The number of the parameter is op1.
*)

PROCEDURE doParam (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   DeclareConstant(CurrentQuadToken, op3) ;
   DeclareConstructor(quad, op3) ;
   BuildParam(CheckConvertCoerceParameter(op1, op2, op3))
END doParam ;


(*
   FoldBuiltin - attempts to fold the gcc builtin function.
*)

PROCEDURE FoldBuiltin (tokenno: CARDINAL; p: WalkAction;
                       q: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   resolved  : BOOLEAN ;
   procedure,
   r         : CARDINAL ;
   n         : CARDINAL ;
   op        : QuadOperator ;
   val       : Tree ;
BEGIN
   resolved := TRUE ;
   n := q ;
   r := op1 ;
   REPEAT
      IF r>0
      THEN
         TryDeclareConstant(QuadToTokenNo(n), op3) ;
         IF NOT GccKnowsAbout(op3)
         THEN
            resolved := FALSE
         END
      END ;
      IF (op=CallOp) AND (NOT IsProcedure(op3))
      THEN
         (* cannot fold an indirect procedure function call *)
         resolved := FALSE
      END ;
      n := GetNextQuad(n) ;
      GetQuad(n, op, r, op2, op3)
   UNTIL op=FunctValueOp ;

   IF resolved AND IsConst(r)
   THEN
      n := q ;
      GetQuad(n, op, op1, op2, op3) ;
      REPEAT
         IF (op=ParamOp) AND (op1>0)
         THEN
            doParam(n, op1, op2, op3)
         ELSIF op=CallOp
         THEN
            procedure := op3
         END ;
         SubQuad(n) ;
         n := GetNextQuad(n) ;
         GetQuad(n, op, op1, op2, op3)
      UNTIL op=FunctValueOp ;

      IF IsProcedureBuiltin(procedure) AND CanUseBuiltin(procedure)
      THEN
         val := FoldAndStrip(UseBuiltin(procedure)) ;
         PutConst(r, GetType(procedure)) ;
         AddModGcc(r, DeclareKnownConstant(Mod2Gcc(GetType(procedure)), val)) ;
         p(r) ;
         SetLastFunction(NIL)
      ELSE
         MetaErrorT1(QuadToTokenNo(n), 'gcc builtin procedure {%1ad} cannot be used in a constant expression', procedure) ;
      END ;
      NoChange := FALSE ;
      SubQuad(n)
   END
END FoldBuiltin ;


(*
   FoldBuiltinFunction - attempts to inline a function. Currently it only
                         inlines the SYSTEM function MAKEADR.
*)

PROCEDURE FoldBuiltinFunction (tokenno: CARDINAL; p: WalkAction;
                               q: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF op1=0
   THEN
      (* must be a function as op1 is the return parameter *)
      IF op3=MakeAdr
      THEN
         FoldMakeAdr(tokenno, p, q, op1, op2, op3)
      ELSIF IsProcedure(op3) AND IsProcedureBuiltin(op3) AND CanUseBuiltin(op3)
      THEN
         FoldBuiltin(tokenno, p, q, op1, op2, op3)
      END
   END
END FoldBuiltinFunction ;


(*
   CodeParam - builds a parameter list.

               NOTE that we almost can treat VAR and NON VAR parameters the same, expect for
                    some types:

                    procedure parameters
                    unbounded parameters

                    these require special attention and thus it is easier to test individually
                    for VAR and NON VAR parameters.

               NOTE that we CAN ignore ModeOfAddr though
*)

PROCEDURE CodeParam (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   IF op1=0
   THEN
      CodeBuiltinFunction(quad, op1, op2, op3)
   ELSE
      IF (op1<=NoOfParam(op2)) AND
         IsVarParam(op2, op1) AND IsConst(op3)
      THEN
         MetaErrorT1(CurrentQuadToken, 'cannot pass a constant {%1ad} as a VAR parameter', op3)
      ELSE
         doParam(quad, op1, op2, op3)
      END
   END
END CodeParam ;


(*
   CodeFunctValue - retrieves the function return value and assigns it
                    into a variable.
*)

PROCEDURE CodeFunctValue (q: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   (*
      operator : FunctValueOp
      op1 : The Returned Variable
      op2 : The Function Returning this Variable
   *)
   BuildFunctValue(Mod2Gcc(op1))
END CodeFunctValue ;


(*
   Addr Operator  - contains the address of a variable.

   Yields the address of a variable - need to add the frame pointer if
   a variable is local to a procedure.

   Sym1<X>   Addr   Sym2<X>     meaning     Mem[Sym1<I>] := Sym2<I>
*)

PROCEDURE CodeAddr (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t: Tree ;
   s: String ;
BEGIN
   IF IsConst(op3) AND (NOT IsConstString(op3))
   THEN
      MetaErrorT1(CurrentQuadToken, 'error in expression, trying to find the address of a constant {%1ad}', op3)
   ELSE
      DeclareConstant(CurrentQuadToken, op3) ;  (* we might be asked to find the address of a constant string *)
      DeclareConstructor(quad, op3) ;
      t := BuildAssignmentTree(Mod2Gcc(op1),
                               BuildConvert(GetPointerType(),
                                            BuildAddr(Mod2Gcc(op3), FALSE), FALSE))
   END
END CodeAddr ;


PROCEDURE stop ; BEGIN END stop ;

PROCEDURE CheckStop (q: CARDINAL) ;
BEGIN
   IF q=16
   THEN
      stop
   END
END CheckStop ;

(*
------------------------------------------------------------------------------
   := Operator
------------------------------------------------------------------------------
   Sym1<I> := Sym3<I>           := produces a constant
*)

PROCEDURE FoldBecomes (tokenno: CARDINAL; p: WalkAction; quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   s: String ;
   t: Tree ;
BEGIN
   TryDeclareConstant(tokenno, op3) ;  (* checks to see whether it is a constant literal and declares it *)
   TryDeclareConstructor(quad, op3) ;
   IF IsConst(op1) AND IsConst(op3)
   THEN
      CheckStop(quad) ;

      (* constant folding taking place, but have we resolved op3 yet? *)
      IF GccKnowsAbout(op3)
      THEN
         (* great, now we can tell gcc about the relationship between, op1 and op3 *)
         IF GccKnowsAbout(op1)
         THEN
            MetaErrorT1(tokenno, 'constant {%1ad} should not be reassigned', op1)
         ELSE
            IF IsConstString(op3)
            THEN
               PutConstString(op1, GetString(op3)) ;
            ELSIF GetType(op1)=NulSym
            THEN
               Assert(GetType(op3)#NulSym) ;
               PutConst(op1, GetType(op3))
            END ;
            IF GetType(op3)=NulSym
            THEN
               CheckOrResetOverflow(tokenno, Mod2Gcc(op3), MustCheckOverflow(quad)) ;
               AddModGcc(op1, Mod2Gcc(op3))
            ELSE
               IF IsValueSolved(op3)
               THEN
                  PushValue(op3) ;
                  IF IsValueTypeReal()
                  THEN
                     CheckOrResetOverflow(tokenno, PopRealTree(), MustCheckOverflow(quad)) ;
                     PushValue(op3) ;
                     AddModGcc(op1, PopRealTree())
                  ELSIF IsValueTypeSet()
                  THEN
                     PopValue(op1) ;
                     PutConstSet(op1)
                  ELSIF IsValueTypeConstructor() OR IsValueTypeArray() OR IsValueTypeRecord()
                  THEN
                     PopValue(op1) ;
                     PutConstructor(op1)
                  ELSIF IsValueTypeComplex()
                  THEN
                     CheckOrResetOverflow(tokenno, PopComplexTree(), MustCheckOverflow(quad)) ;
                     PushValue(op3) ;
                     PopValue(op1)
                  ELSE
                     CheckOrResetOverflow(tokenno, PopIntegerTree(), MustCheckOverflow(quad)) ;
                     PushValue(op3) ;
                     AddModGcc(op1, PopIntegerTree())
                  END
               ELSE
                  CheckOrResetOverflow(tokenno, Mod2Gcc(op3), MustCheckOverflow(quad)) ;
                  AddModGcc(op1,
                            DeclareKnownConstant(Mod2Gcc(GetType(op3)),
                                                 Mod2Gcc(op3)))
               END
            END ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad) ;
            t := RememberConstant(Mod2Gcc(op1))
         END
      ELSE
         (* not to worry, we must wait until op3 is known *)
      END
   END
END FoldBecomes ;

VAR
   tryBlock: Tree ;    (* this must be placed into gccgm2 and it must follow the
                          current function scope - ie it needs work with nested procedures *)
   handlerBlock: Tree ;


(*
   CodeTry - starts building a GCC 'try' node.
*)

PROCEDURE CodeTry (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   handlerBlock := NIL ;
   tryBlock := BuildTryBegin()
END CodeTry ;


(*
   CodeThrow - builds a GCC 'throw' node.
*)

PROCEDURE CodeThrow (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF op3=NulSym
   THEN
      AddStatement(BuildThrow(Tree(NIL)))
   ELSE
      DeclareConstant(CurrentQuadToken, op3) ;  (* checks to see whether it is a constant and declares it *)
      AddStatement(BuildThrow(BuildConvert(GetIntegerType(),
                                           Mod2Gcc(op3), FALSE)))
   END
END CodeThrow ;


PROCEDURE CodeRetry (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   BuildGoto(string(CreateLabelName(op3)))
END CodeRetry ;


PROCEDURE CodeCatchBegin (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   BuildTryEnd(tryBlock) ;
   handlerBlock := BuildCatchBegin()
END CodeCatchBegin ;


PROCEDURE CodeCatchEnd (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   tryBlock := BuildCatchEnd(handlerBlock, tryBlock) ;
   AddStatement(tryBlock)
END CodeCatchEnd ;


(*
   DescribeTypeError - 
*)

PROCEDURE DescribeTypeError (token: CARDINAL;
                             op1, op2: CARDINAL) ;
VAR
   s1, s2, s3: String ;
BEGIN
   s1 := NIL ;
   s2 := NIL ;
   IF IsTemporary(op1)
   THEN
      IF (GetType(op1)#NulSym) AND (GetSymName(GetType(op1))#NulName)
      THEN
         s3 := InitStringCharStar(KeyToCharStar(GetSymName(GetType(op1)))) ;
         s1 := Mark(Sprintf1(Mark(InitString('symbol of type %s')), s3))
      END
   ELSE
      s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(op1)))))
   END ;

   IF IsTemporary(op2)
   THEN
      IF (GetType(op2)#NulSym) AND (GetSymName(GetType(op2))#NulName)
      THEN
         s3 := InitStringCharStar(KeyToCharStar(GetSymName(GetType(op2)))) ;
         s2 := Mark(Sprintf1(Mark(InitString('symbol of type %s')), s3))
      END
   ELSE
      s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(op2)))))
   END ;
   IF (s1#NIL) AND (s2#NIL)
   THEN
      ErrorStringAt(Sprintf2(Mark(InitString('incompatible set types in assignment, assignment between (%s) and (%s)')),
                             s1, s2),
                    token)
   ELSE
      ErrorStringAt(InitString('incompatible set types in assignment'),
                    token)
   END
END DescribeTypeError ;


(*
   DefaultConvertGM2 - provides a simple mapping between
                       front end data types and GCC equivalents.
                       This is only used to aid assignment of
                       typed constants.
*)

PROCEDURE DefaultConvertGM2 (sym: CARDINAL) : Tree ;
BEGIN
   sym := SkipType(sym) ;
   IF sym=Bitset
   THEN
      RETURN( GetWordType() )
   ELSE
      RETURN( Mod2Gcc(sym) )
   END
END DefaultConvertGM2 ;


(*
   GetTypeMode - 
*)

PROCEDURE GetTypeMode (sym: CARDINAL) : CARDINAL ;
BEGIN
   IF GetMode(sym)=LeftValue
   THEN
      RETURN( Address )
   ELSE
      RETURN( GetType(sym) )
   END
END GetTypeMode ;


(*
   FoldConstBecomes - returns a Tree containing op3.
                      The tree will have been folded and
                      type converted if necessary.
*)

PROCEDURE FoldConstBecomes (tokenno: CARDINAL;
                            op1, op3: CARDINAL) : Tree ;
VAR
   t: Tree ;
BEGIN
   IF IsConstSet(op3) OR ((SkipType(GetType(op3))#NulSym) AND
                          IsSet(SkipType(GetType(op3))))
   THEN
      (* we have not checked set compatibility in
         M2Quads.mod:BuildAssignmentTree so we do it here.
      *)
(*
      IF (Iso AND (SkipType(GetType(op1))#SkipType(GetType(op3)))) OR
         (Pim AND ((SkipType(GetType(op1))#SkipType(GetType(op3))) AND
                   (SkipType(GetType(op1))#Bitset) AND
                   (SkipType(GetType(op3))#Bitset)))
*)
      IF SkipType(GetTypeMode(op1))#SkipType(GetTypeMode(op3))
      THEN
         DescribeTypeError(CurrentQuadToken, op1, op3) ;
         RETURN( Mod2Gcc(op1) ) (* we might crash if we execute the BuildAssignmentTree with op3 *)
      END
   END ;
   TryDeclareConstant(tokenno, op3) ;
   t := Mod2Gcc(op3) ;
   Assert(t#NIL) ;
   IF IsConst(op3)
   THEN
      IF (NOT IsConstString(op3)) AND (NOT IsConstSet(op3)) AND
         (SkipType(GetType(op3))#SkipType(GetType(op1)))
      THEN
         t := ConvertConstantAndCheck(DefaultConvertGM2(GetType(op1)), t)
      ELSIF GetType(op1)#NulSym
      THEN
         t := StringToChar(Mod2Gcc(op3), GetType(op1), op3)
      END
   END ;
   RETURN( t )
END FoldConstBecomes ;


(*
   DoCopyString - returns trees:
                  t    number of bytes to be copied (including the nul)
                  op3t the string with the extra nul character
                       providing it fits.
*)

PROCEDURE DoCopyString (VAR t, op3t: Tree; op1t, op3: CARDINAL) ;
BEGIN
   Assert(IsArray(SkipType(op1t))) ;
   (* handle string assignments:
      VAR
         str: ARRAY [0..10] OF CHAR ;
         ch : CHAR ;

         str := 'abcde' but not ch := 'a'
   *)
   IF GetType(op3)=Char
   THEN
      (*
       *  create string from char and add nul to the end, nul is
       *  added by BuildStringConstant
       *)
      op3t := BuildStringConstant(KeyToCharStar(GetString(op3)), 1)
   ELSE
      op3t := Mod2Gcc(op3)
   END ;
   op3t := ConvertString(Mod2Gcc(op1t), op3t) ;

   PushIntegerTree(FindSize(op3)) ;
   PushIntegerTree(FindSize(op1t)) ;
   IF Less(CurrentQuadToken)
   THEN
      (* there is room for the extra <nul> character *)
      t := BuildAdd(FindSize(op3), GetIntegerOne(), FALSE)
   ELSE
      PushIntegerTree(FindSize(op3)) ;
      PushIntegerTree(FindSize(op1t)) ;
      IF Gre(CurrentQuadToken)
      THEN
         WarnStringAt(InitString('string constant is too large to be assigned to the array'),
                      CurrentQuadToken) ;
         t := FindSize(op1t)
      ELSE
         (* equal so return max characters in the array *)
         t := FindSize(op1t)
      END
   END
END DoCopyString ;


(*
------------------------------------------------------------------------------
   := Operator
------------------------------------------------------------------------------
   Sym1<I> := Sym3<I>           := produces a constant
   Sym1<O> := Sym3<O>           := has the effect Mem[Sym1<I>] := Mem[Sym3<I>]
*)

PROCEDURE CodeBecomes (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   op3t, t: Tree ;
BEGIN
   DeclareConstant(CurrentQuadToken, op3) ;  (* checks to see whether it is a constant and declares it *)
   DeclareConstructor(quad, op3) ;
   CheckStop(quad) ;
   IF IsConst(op1) AND (NOT GccKnowsAbout(op1))
   THEN
      ConstantKnownAndUsed(op1, CheckConstant(op1, op3))
   ELSIF IsConstString(op3) AND (SkipTypeAndSubrange(GetType(op1))#Char)
   THEN
      DoCopyString(t, op3t, SkipType(GetType(op1)), op3) ;
      AddStatement(BuiltInMemCopy(BuildAddr(Mod2Gcc(op1), FALSE),
                                  BuildAddr(op3t, FALSE),
                                  t))
   ELSE
      IF ((IsGenericSystemType(SkipType(GetType(op1))) #
           IsGenericSystemType(SkipType(GetType(op3)))) OR
          (IsUnbounded(SkipType(GetType(op1))) AND
           IsUnbounded(SkipType(GetType(op3))) AND
           (IsGenericSystemType(SkipType(GetType(GetType(op1)))) #
            IsGenericSystemType(SkipType(GetType(GetType(op3))))))) AND
         (NOT IsConst(op3))
      THEN
         AddStatement(BuiltInMemCopy(BuildAddr(Mod2Gcc(op1), FALSE),
                                     BuildAddr(Mod2Gcc(op3), FALSE),
                                     BuildSize(Mod2Gcc(op1), FALSE)))
      ELSIF (SkipType(GetType(op1))=Word) AND Iso AND
            (SkipType(GetType(op3))#SkipType(GetType(op1)))
      THEN
         (* in ISO the WORD type is defined as an ARRAY of BYTEs *)
         t := BuildAssignmentTree(BuildIndirect(BuildAddr(Mod2Gcc(op1), FALSE),
                                                GetWordType()),
                                  BuildConvert(GetWordType(), Mod2Gcc(op3), FALSE)) ;
      ELSE
         t := BuildAssignmentTree(Mod2Gcc(op1),
                                  FoldConstBecomes(QuadToTokenNo(quad), op1, op3))
      END
   END
END CodeBecomes ;


(*
   LValueToGenericPtr - returns a Tree representing symbol, sym.
                        It coerces a lvalue into an internal pointer type
*)

PROCEDURE LValueToGenericPtr (sym: CARDINAL) : Tree ;
VAR
   t: Tree ;
BEGIN
   t := Mod2Gcc(sym) ;
   IF t=NIL
   THEN
      InternalError('expecting symbol to be resolved', __FILE__, __LINE__)
   END ;
   IF GetMode(sym)=LeftValue
   THEN
      t := BuildConvert(GetPointerType(), t, FALSE)
   END ;
   RETURN( t )
END LValueToGenericPtr ;


(*
   LValueToGenericPtrOrConvert - if sym is an lvalue then convert to pointer type
                                 else convert to type, type. Return the converted tree.
*)

PROCEDURE LValueToGenericPtrOrConvert (sym: CARDINAL; type: Tree) : Tree ;
VAR
   n: Tree ;
BEGIN
   n := Mod2Gcc(sym) ;
   IF n=NIL
   THEN
      InternalError('expecting symbol to be resolved', __FILE__, __LINE__)
   END ;
   IF GetMode(sym)=LeftValue
   THEN
      n := BuildConvert(GetPointerType(), n, FALSE)
   ELSE
      n := BuildConvert(type, n, FALSE)
   END ;
   RETURN( n )
END LValueToGenericPtrOrConvert ;


(*
   ZConstToTypedConst - checks whether op1 and op2 are constants and
                        coerces, t, appropriately.
*)

PROCEDURE ZConstToTypedConst (t: Tree; op1, op2: CARDINAL) : Tree ;
BEGIN
   IF IsConst(op1) AND IsConst(op2)
   THEN
      (* leave, Z type, alone *)
      RETURN( t )
   ELSIF IsConst(op1)
   THEN
      IF GetMode(op2)=LeftValue
      THEN
         (* convert, Z type const into type of non constant operand *)
         RETURN( BuildConvert(GetPointerType(), t, FALSE) )
      ELSE
         (* convert, Z type const into type of non constant operand *)
         RETURN( BuildConvert(Mod2Gcc(FindType(op2)), t, FALSE) )
      END
   ELSIF IsConst(op2)
   THEN
      IF GetMode(op1)=LeftValue
      THEN
         (* convert, Z type const into type of non constant operand *)
         RETURN( BuildConvert(GetPointerType(), t, FALSE) )
      ELSE
         (* convert, Z type const into type of non constant operand *)
         RETURN( BuildConvert(Mod2Gcc(FindType(op1)), t, FALSE) )
      END
   ELSE
      (* neither operands are constants, leave alone *)
      RETURN( t )
   END
END ZConstToTypedConst ;


(*
   FoldBinary - check whether we can fold the binop operation.
*)

PROCEDURE FoldBinary (tokenno: CARDINAL; p: WalkAction; binop: BuildBinProcedure;
                      quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   tl, tr, tv: Tree ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   TryDeclareConstant(tokenno, op3) ;
   TryDeclareConstant(tokenno, op2) ;
   IF IsConst(op2) AND IsConst(op3)
   THEN
      IF GccKnowsAbout(op2) AND GccKnowsAbout(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            Assert(MixTypes(FindType(op3), FindType(op2), tokenno)#NulSym) ;
            PutConst(op1, MixTypes(FindType(op3), FindType(op2), tokenno)) ;
            tl := LValueToGenericPtr(op2) ;
            tr := LValueToGenericPtr(op3) ;
            tv := binop(tl, tr, TRUE) ;
            CheckOrResetOverflow(tokenno, tv, MustCheckOverflow(quad)) ;

            IF (GetType(op1)=NulSym) OR IsOrdinalType(GetType(op1))
            THEN
               AddModGcc(op1, DeclareKnownConstant(GetM2ZType(), tv))
            ELSE
               AddModGcc(op1, DeclareKnownConstant(Mod2Gcc(GetType(op1)), tv))
            END ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         ELSE
            (* we can still fold the expression, but not the assignment,
               however, we will not do this here but in CodeBinary
             *)
         END
      END
   END
END FoldBinary ;


(*
   CodeBinary - encode a binary arithmetic operation.
*)

PROCEDURE CodeBinary (binop: BuildBinProcedure;
                      q: CARDINAL;
                      op1, op2, op3: CARDINAL) ;
VAR
   t, tv,
   tl, tr: Tree ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, op3) ;
   DeclareConstant(CurrentQuadToken, op2) ;
   tl := ZConstToTypedConst(LValueToGenericPtr(op2), op2, op3) ;
   tr := ZConstToTypedConst(LValueToGenericPtr(op3), op2, op3) ;
   
   tv := binop(tl, tr, TRUE) ;
   CheckOrResetOverflow(CurrentQuadToken, tv, MustCheckOverflow(q)) ;
   IF IsConst(op1)
   THEN
      (* still have a constant which was not resolved, pass it to gcc *)
      Assert(MixTypes(FindType(op3), FindType(op2), CurrentQuadToken)#NulSym) ;

      PutConst(op1, MixTypes(FindType(op3), FindType(op2), CurrentQuadToken)) ;
      ConstantKnownAndUsed(op1, DeclareKnownConstant(Mod2Gcc(GetType(op3)), tv))
   ELSE
      t := BuildAssignmentTree(Mod2Gcc(op1), tv)
   END
END CodeBinary ;


(*
   CodeBinarySet - encode a binary set arithmetic operation.
                   Set operands may be longer than a word.
*)

PROCEDURE CodeBinarySet (binop: BuildBinProcedure; doOp: DoProcedure;
                         q: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t: CARDINAL ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, op3) ;
   DeclareConstant(CurrentQuadToken, op2) ;
   DeclareConstructor(q, op3) ;
   DeclareConstructor(q, op2) ;
   IF IsConst(op1)
   THEN
      IF IsValueSolved(op2) AND IsValueSolved(op3)
      THEN
         Assert(MixTypes(FindType(op3), FindType(op2), CurrentQuadToken)#NulSym) ;
         PutConst(op1, FindType(op3)) ;
         PushValue(op2) ;
         PushValue(op3) ;
         doOp(CurrentQuadToken) ;
         PopValue(op1) ;
         PutConstSet(op1) ;
      ELSE
         ErrorStringAt(InitString('constant expression cannot be evaluated'),
                       CurrentQuadToken)
      END
   ELSE
      BuildBinaryForeachWordDo(Mod2Gcc(SkipType(GetType(op1))),
                               Mod2Gcc(op1), Mod2Gcc(op2), Mod2Gcc(op3), binop,
                               GetMode(op1)=LeftValue,
                               GetMode(op2)=LeftValue,
                               GetMode(op3)=LeftValue,
                               IsConst(op1),
                               IsConst(op2),
                               IsConst(op3))
   END
END CodeBinarySet ;


(*
   FoldAdd - check addition for constant folding.
*)

PROCEDURE FoldAdd (tokenno: CARDINAL; p: WalkAction;
                   quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   s: String ;
BEGIN
   IF IsConst(op2) AND IsConst(op3) AND IsConst(op3) AND
      IsConstString(op2) AND IsConstString(op3)
   THEN
      (* handle special addition for constant strings *)
      s := InitStringCharStar(KeyToCharStar(GetString(op2))) ;
      s := ConCat(s, Mark(InitStringCharStar(KeyToCharStar(GetString(op3))))) ;
      PutConstString(op1, makekey(string(s))) ;
      TryDeclareConstant(tokenno, op1) ;
      p(op1) ;
      NoChange := FALSE ;
      SubQuad(quad) ;
      s := KillString(s)
   ELSE
      FoldBinary(tokenno, p, BuildAdd, quad, op1, op2, op3)
   END
END FoldAdd ;


(*
   CodeAdd - encode addition.
*)

PROCEDURE CodeAdd (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinary(BuildAdd, quad, op1, op2, op3)
END CodeAdd ;


(*
   FoldSub - check subtraction for constant folding.
*)

PROCEDURE FoldSub (tokenno: CARDINAL; p: WalkAction;
                   quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinary(tokenno, p, BuildSub, quad, op1, op2, op3)
END FoldSub ;


(*
   CodeSub - encode subtraction.
*)

PROCEDURE CodeSub (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinary(BuildSub, quad, op1, op2, op3)
END CodeSub ;


(*
   FoldMult - check multiplication for constant folding.
*)

PROCEDURE FoldMult (tokenno: CARDINAL; p: WalkAction;
                   quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinary(tokenno, p, BuildMult, quad, op1, op2, op3)
END FoldMult ;


(*
   CodeMult - encode multiplication.
*)

PROCEDURE CodeMult (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinary(BuildMult, quad, op1, op2, op3)
END CodeMult ;


(*
   FoldDivTrunc - check division for constant folding.
*)

PROCEDURE FoldDivTrunc (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinary(tokenno, p, BuildDivTrunc, quad, op1, op2, op3)
END FoldDivTrunc ;


(*
   CodeDivTrunc - encode multiplication.
*)

PROCEDURE CodeDivTrunc (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinary(BuildDivTrunc, quad, op1, op2, op3)
END CodeDivTrunc ;


(*
   FoldModTrunc - check modulus for constant folding.
*)

PROCEDURE FoldModTrunc (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinary(tokenno, p, BuildModTrunc, quad, op1, op2, op3)
END FoldModTrunc ;


(*
   CodeModTrunc - encode modulus.
*)

PROCEDURE CodeModTrunc (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinary(BuildModTrunc, quad, op1, op2, op3)
END CodeModTrunc ;


(*
   FoldDivFloor - check division for constant folding.
*)

PROCEDURE FoldDivFloor (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinary(tokenno, p, BuildDivFloor, quad, op1, op2, op3)
END FoldDivFloor ;


(*
   CodeDivFloor - encode multiplication.
*)

PROCEDURE CodeDivFloor (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinary(BuildDivFloor, quad, op1, op2, op3)
END CodeDivFloor ;


(*
   FoldModFloor - check modulus for constant folding.
*)

PROCEDURE FoldModFloor (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinary(tokenno, p, BuildModFloor, quad, op1, op2, op3)
END FoldModFloor ;


(*
   CodeModFloor - encode modulus.
*)

PROCEDURE CodeModFloor (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinary(BuildModFloor, quad, op1, op2, op3)
END CodeModFloor ;


(*
   FoldBuiltinConst - 
*)

PROCEDURE FoldBuiltinConst (tokenno: CARDINAL; p: WalkAction;
                            quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t: Tree ;
   s: String ;
BEGIN
   t := GetBuiltinConst(KeyToCharStar(Name(op3))) ;
   IF t=NIL
   THEN
      MetaErrorT1(tokenno, 'unknown built in constant {%1ad}', op3)
   ELSE
      AddModGcc(op1, t) ;
      p(op1) ;
      NoChange := FALSE ;
      SubQuad(quad)
   END
END FoldBuiltinConst ;


(*
   FoldBuiltinTypeInfo - attempts to fold a builtin attribute value on type op2.
*)

PROCEDURE FoldBuiltinTypeInfo (tokenno: CARDINAL; p: WalkAction;
                               quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t: Tree ;
   s: String ;
BEGIN
   IF GccKnowsAbout(op2) AND CompletelyResolved(op2)
   THEN
      t := GetBuiltinTypeInfo(Mod2Gcc(op2), KeyToCharStar(Name(op3))) ;
      IF t=NIL
      THEN
         MetaErrorT2(tokenno, 'unknown built in constant {%1ad} attribute for type {%2ad}', op3, op2)
      ELSE
         AddModGcc(op1, t) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad)
      END
   END
END FoldBuiltinTypeInfo ;


(*
   FoldStandardFunction - attempts to fold a standard function.
*)

PROCEDURE FoldStandardFunction (tokenno: CARDINAL; p: WalkAction;
                                quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t     : Tree ;
   s     : String ;
   type,
   d,
   result: CARDINAL ;
BEGIN
   IF GetSymName(op2)=MakeKey('Length')
   THEN
      TryDeclareConstant(tokenno, op3) ;
      IF IsConst(op3) AND GccKnowsAbout(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            IF IsConstString(op3)
            THEN
               AddModGcc(op1, FindSize(op3)) ;
               p(op1) ;
               NoChange := FALSE ;
               SubQuad(quad)
            ELSE
               MetaErrorT1(tokenno, 'parameter to LENGTH must be a string {%1ad}', op3)
            END
         ELSE
            (* rewrite the quad to use becomes *)
            d := GetStringLength(op3) ;
            s := Sprintf1(Mark(InitString("%d")), d) ;
            result := MakeConstLit(makekey(string(s))) ;
            s := KillString(s) ;
            TryDeclareConstant(tokenno, result) ;
            PutQuad(quad, BecomesOp, op1, NulSym, result)
         END
      END
   ELSIF GetSymName(op2)=MakeKey('CAP')
   THEN
      TryDeclareConstant(tokenno, op3) ;
      IF IsConst(op3) AND GccKnowsAbout(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            IF (IsConstString(op3) AND (GetStringLength(op3)=1)) OR
               (GetType(op3)=Char)
            THEN
               AddModGcc(op1, BuildCap(Mod2Gcc(op3))) ;
               p(op1) ;
               NoChange := FALSE ;
               SubQuad(quad)
            ELSE
               MetaErrorT1(tokenno, 'parameter to CAP must be a single character {%1ad}', op3)
            END
         END
      END
   ELSIF GetSymName(op2)=MakeKey('ABS')
   THEN
      TryDeclareConstant(tokenno, op3) ;
      IF IsConst(op3) AND GccKnowsAbout(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            AddModGcc(op1, BuildAbs(Mod2Gcc(op3))) ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         END
      END
   ELSIF op2=Im
   THEN
      TryDeclareConstant(tokenno, op3) ;
      IF IsConst(op3) AND GccKnowsAbout(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            AddModGcc(op1, BuildIm(Mod2Gcc(op3))) ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         END
      END
   ELSIF op2=Re
   THEN
      TryDeclareConstant(tokenno, op3) ;
      IF IsConst(op3) AND GccKnowsAbout(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            AddModGcc(op1, BuildRe(Mod2Gcc(op3))) ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         END
      END
   ELSIF op2=Cmplx
   THEN
      TryDeclareConstant(tokenno, GetNth(op3, 1)) ;
      TryDeclareConstant(tokenno, GetNth(op3, 2)) ;
      IF IsConst(GetNth(op3, 1)) AND GccKnowsAbout(GetNth(op3, 1)) AND
         IsConst(GetNth(op3, 2)) AND GccKnowsAbout(GetNth(op3, 2))
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            type := GetCmplxReturnType(GetType(GetNth(op3, 1)), GetType(GetNth(op3, 2))) ;
            IF type=NulSym
            THEN
               MetaErrorT2(tokenno, 'real {%1atd} and imaginary {%2atd} types are incompatible',
                           GetNth(op3, 1), GetNth(op3, 2))
            ELSE
               AddModGcc(op1, BuildCmplx(Mod2Gcc(type),
                                         Mod2Gcc(GetNth(op3, 1)),
                                         Mod2Gcc(GetNth(op3, 2)))) ;
               p(op1) ;
               NoChange := FALSE ;
               SubQuad(quad)
            END
         END
      END
   ELSE
      InternalError('only expecting LENGTH, CAP, ABS, IM, RE or CMPLX as a standard function',
                    __FILE__, __LINE__)
   END
END FoldStandardFunction ;


(*
   CodeStandardFunction - 
*)

PROCEDURE CodeStandardFunction (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   type: CARDINAL ;
   t   : Tree ;
BEGIN
   DeclareConstant(CurrentQuadToken, op3) ;
   DeclareConstructor(quad, op3) ;
   IF (op2#NulSym) AND (GetSymName(op2)=MakeKey('Length'))
   THEN
      IF IsProcedure(op2)
      THEN
         IF IsConstString(op3)
         THEN
            InternalError('the LENGTH of a constant string should already be known', __FILE__, __LINE__)
         ELSIF IsUnbounded(GetType(op3))
         THEN
            BuildParam(Mod2Gcc(op3)) ;
            t := BuildProcedureCallTree(Mod2Gcc(op2), Mod2Gcc(GetType(op2))) ;
            BuildFunctValue(Mod2Gcc(op1))
         ELSIF GetMode(op3)=RightValue
         THEN
            BuildParam(ResolveHigh(quad, 1, op3)) ;
            BuildParam(BuildAddr(Mod2Gcc(op3), FALSE)) ;
            t := BuildProcedureCallTree(Mod2Gcc(op2), Mod2Gcc(GetType(op2))) ;
            BuildFunctValue(Mod2Gcc(op1))
         ELSE
            BuildParam(ResolveHigh(quad, 1, op3)) ;
            BuildParam(Mod2Gcc(op3)) ;
            t := BuildProcedureCallTree(Mod2Gcc(op2), Mod2Gcc(GetType(op2))) ;
            BuildFunctValue(Mod2Gcc(op1))
         END
      ELSE
         ErrorStringAt(Sprintf0(Mark(InitString('no procedure Length found for substitution to the standard function LENGTH which is required to calculate non constant string lengths'))),
                       CurrentQuadToken)
      END
   ELSIF (op2#NulSym) AND (GetSymName(op2)=MakeKey('CAP'))
   THEN
      IF IsConst(op1)
      THEN
         InternalError('CAP function should already have been folded',
                       __FILE__, __LINE__)
      ELSE
         t := BuildAssignmentTree(Mod2Gcc(op1), BuildCap(Mod2Gcc(op3)))
      END
   ELSIF (op2#NulSym) AND (GetSymName(op2)=MakeKey('ABS'))
   THEN
      IF IsConst(op1)
      THEN
         InternalError('ABS function should already have been folded',
                       __FILE__, __LINE__)
      ELSE
         t := BuildAssignmentTree(Mod2Gcc(op1), BuildAbs(Mod2Gcc(op3)))
      END
   ELSIF op2=Im
   THEN
      IF IsConst(op1)
      THEN
         InternalError('IM function should already have been folded',
                       __FILE__, __LINE__)
      ELSE
         t := BuildAssignmentTree(Mod2Gcc(op1), BuildIm(Mod2Gcc(op3)))
      END
   ELSIF op2=Re
   THEN
      IF IsConst(op1)
      THEN
         InternalError('RE function should already have been folded',
                       __FILE__, __LINE__)
      ELSE
         t := BuildAssignmentTree(Mod2Gcc(op1), BuildRe(Mod2Gcc(op3)))
      END
   ELSIF op2=Cmplx
   THEN
      IF IsConst(op1)
      THEN
         InternalError('CMPLX function should already have been folded',
                       __FILE__, __LINE__)
      ELSE
         type := GetCmplxReturnType(GetType(GetNth(op3, 1)), GetType(GetNth(op3, 2))) ;
         IF type=NulSym
         THEN
            MetaErrorT2(QuadToTokenNo(quad),
                        'real {%1atd} and imaginary {%2atd} types are incompatible',
                        GetNth(op3, 1), GetNth(op3, 2))
         ELSE
            t := BuildAssignmentTree(Mod2Gcc(op1), BuildCmplx(Mod2Gcc(type),
                                                              Mod2Gcc(GetNth(op3, 1)),
                                                              Mod2Gcc(GetNth(op3, 2))))
         END
      END
   ELSE
      InternalError('expecting LENGTH, CAP, ABS, IM, RE or CMPLX as a standard function',
                    __FILE__, __LINE__)
   END
END CodeStandardFunction ;


(*
   CodeSavePriority - checks to see whether op2 is reachable and is directly accessible
                      externally. If so then it saves the current interrupt priority
                      in op1 and sets the current priority to that determined by
                      appropriate module.

                      op1 := op3(GetModuleScope(op2))
*)

PROCEDURE CodeSavePriority (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t  : Tree ;
   mod: CARDINAL ;
   n  : Name ;
BEGIN
   IF IsModule(op2) OR IsDefImp(op2) OR
      (IsProcedure(op2) AND GetNeedSavePriority(op2))
   THEN
      IF IsProcedure(op2)
      THEN
         mod := GetModuleScope(op2) ;
      ELSE
         Assert(IsModule(op2) OR IsDefImp(op2)) ;
         mod := op2
      END ;
      IF GetPriority(mod)#NulSym
      THEN
         IF PriorityDebugging
         THEN
            n := GetSymName(op2) ;
            printf1('procedure <%a> needs to save interrupts\n', n)
         END ;
         DeclareConstant(CurrentQuadToken, GetPriority(mod)) ;
         BuildParam(Mod2Gcc(GetPriority(mod))) ;
         t := BuildProcedureCallTree(Mod2Gcc(op3), Mod2Gcc(GetType(op3))) ;
         BuildFunctValue(Mod2Gcc(op1))
      END
   END
END CodeSavePriority ;


(*
   CodeRestorePriority - checks to see whether op2 is reachable and is directly accessible
                         externally. If so then it restores the previous interrupt priority
                         held in op1.

                         op1 := op3(op1)
*)

PROCEDURE CodeRestorePriority (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t  : Tree ;
   mod: CARDINAL ;
   n  : Name ;
BEGIN
   IF IsModule(op2) OR IsDefImp(op2) OR
      (IsProcedure(op2) AND GetNeedSavePriority(op2))
   THEN
      IF IsProcedure(op2)
      THEN
         mod := GetModuleScope(op2) ;
      ELSE
         Assert(IsModule(op2) OR IsDefImp(op2)) ;
         mod := op2
      END ;
      IF GetPriority(mod)#NulSym
      THEN
         IF PriorityDebugging
         THEN
            n := GetSymName(op2) ;
            printf1('procedure <%a> needs to restore interrupts\n', n)
         END ;
         BuildParam(Mod2Gcc(op1)) ;
         t := BuildProcedureCallTree(Mod2Gcc(op3), Mod2Gcc(GetType(op3))) ;
         BuildFunctValue(Mod2Gcc(op1))
      END
   END
END CodeRestorePriority ;


(*
   FoldBinarySet - attempts to fold set arithmetic it removes the quad if successful.
*)

PROCEDURE FoldBinarySet (tokenno: CARDINAL; p: WalkAction; op: DoProcedure;
                         quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   (* firstly try and ensure that constants are declared *)
   TryDeclareConstant(tokenno, op2) ;
   TryDeclareConstant(tokenno, op3) ;
   IF IsConst(op2) AND IsConstSet(op2) AND
      IsConst(op3) AND IsConstSet(op3) AND
      IsConst(op1)
   THEN
      IF IsValueSolved(op2) AND IsValueSolved(op3)
      THEN
         Assert(MixTypes(FindType(op3), FindType(op2), tokenno)#NulSym) ;
         PutConst(op1, MixTypes(FindType(op3), FindType(op2), tokenno)) ;
         PushValue(op2) ;
         PushValue(op3) ;
         op(tokenno) ;
         PopValue(op1) ;
         PushValue(op1) ;
         PutConstSet(op1) ;
         AddModGcc(op1,
                   DeclareKnownConstant(Mod2Gcc(GetType(op3)),
                                        PopSetTree(tokenno))) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad)
      END
   END
END FoldBinarySet ;


(*
   FoldSetOr - check whether we can fold a set arithmetic or.
*)

PROCEDURE FoldSetOr (tokenno: CARDINAL; p: WalkAction;
                     quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinarySet(tokenno, p, SetOr, quad, op1, op2, op3)
END FoldSetOr ;


(*
   CodeSetOr - encode set arithmetic or.
*)

PROCEDURE CodeSetOr (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinarySet(BuildLogicalOr, SetOr, quad, op1, op2, op3)
END CodeSetOr ;


(*
   FoldSetAnd - check whether we can fold a logical and.
*)

PROCEDURE FoldSetAnd (tokenno: CARDINAL; p: WalkAction;
                      quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinarySet(tokenno, p, SetAnd, quad, op1, op2, op3)
END FoldSetAnd ;


(*
   CodeSetAnd - encode set arithmetic and.
*)

PROCEDURE CodeSetAnd (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinarySet(BuildLogicalAnd, SetAnd, quad, op1, op2, op3)
END CodeSetAnd ;


(*
   CodeBinarySetShift - encode a binary set arithmetic operation.
                        The set maybe larger than a machine word
                        and the value of one word may effect the
                        values of another - ie shift and rotate.
                        Set sizes of a word or less are evaluated
                        with binop, whereas multiword sets are
                        evaluated by M2RTS.
*)

PROCEDURE CodeBinarySetShift (binop: BuildSetProcedure;
                              doOp : DoProcedure;
                              var, left, right: Name;
                              quad: CARDINAL;
                              op1, op2, op3: CARDINAL) ;
VAR
   nBits,
   unbounded,
   leftproc,
   rightproc,
   varproc  : Tree ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, op3) ;
   DeclareConstant(CurrentQuadToken, op2) ;
   DeclareConstructor(quad, op3) ;
   DeclareConstructor(quad, op2) ;
   IF IsConst(op1)
   THEN
      IF IsValueSolved(op2) AND IsValueSolved(op3)
      THEN
         Assert(MixTypes(FindType(op3),
                         FindType(op2), CurrentQuadToken)#NulSym) ;
         PutConst(op1, FindType(op3)) ;
         PushValue(op2) ;
         PushValue(op3) ;
         doOp(CurrentQuadToken) ;
         PopValue(op1) ;
         PutConstSet(op1)
      ELSE
         ErrorStringAt(InitString('constant expression cannot be evaluated'),
                       CurrentQuadToken)
      END
   ELSE
      varproc := Mod2Gcc(FromModuleGetSym(var, System)) ;
      leftproc := Mod2Gcc(FromModuleGetSym(left, System)) ;
      rightproc := Mod2Gcc(FromModuleGetSym(right, System)) ;
      unbounded := Mod2Gcc(GetType(GetNthParam(FromModuleGetSym(var, System), 1))) ;
      PushValue(GetTypeMax(SkipType(GetType(op1)))) ;
      PushValue(GetTypeMin(SkipType(GetType(op1)))) ;
      Sub ;
      PushCard(1) ;
      Addn ;
      nBits := PopIntegerTree() ;
      BuildBinarySetDo(Mod2Gcc(SkipType(GetType(op1))),
                       Mod2Gcc(op1),
                       Mod2Gcc(op2),
                       Mod2Gcc(op3),
                       binop,
                       GetMode(op1)=LeftValue,
                       GetMode(op2)=LeftValue,
                       GetMode(op3)=LeftValue,
                       nBits,
                       unbounded,
                       varproc, leftproc, rightproc)
   END
END CodeBinarySetShift ;


(*
   FoldSetShift - check whether we can fold a logical shift.
*)

PROCEDURE FoldSetShift (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinarySet(tokenno, p, SetShift, quad, op1, op2, op3)
END FoldSetShift ;


(*
   CodeSetShift - encode set arithmetic shift.
*)

PROCEDURE CodeSetShift (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinarySetShift(BuildLogicalShift,
                      SetShift,
                      MakeKey('ShiftVal'),
                      MakeKey('ShiftLeft'),
                      MakeKey('ShiftRight'),
                      quad, op1, op2, op3)
END CodeSetShift ;


(*
   FoldSetRotate - check whether we can fold a logical rotate.
*)

PROCEDURE FoldSetRotate (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinarySet(tokenno, p, SetRotate, quad, op1, op2, op3)
END FoldSetRotate ;


(*
   CodeSetRotate - encode set arithmetic rotate.
*)

PROCEDURE CodeSetRotate (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinarySetShift(BuildLogicalRotate,
                      SetRotate,
                      MakeKey('RotateVal'),
                      MakeKey('RotateLeft'),
                      MakeKey('RotateRight'),
                      quad, op1, op2, op3)
END CodeSetRotate ;


(*
   FoldSetLogicalDifference - check whether we can fold a logical difference.
*)

PROCEDURE FoldSetLogicalDifference (tokenno: CARDINAL; p: WalkAction;
                                    quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinarySet(tokenno, p, SetDifference, quad, op1, op2, op3)
END FoldSetLogicalDifference ;


(*
   CodeSetLogicalDifference - encode set arithmetic logical difference.
*)

PROCEDURE CodeSetLogicalDifference (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinarySet(BuildLogicalDifference, SetDifference,
                 quad, op1, op2, op3)
END CodeSetLogicalDifference ;


(*
   FoldSymmetricDifference - check whether we can fold a logical difference.
*)

PROCEDURE FoldSymmetricDifference (tokenno: CARDINAL; p: WalkAction;
                                   quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinarySet(tokenno, p, SetSymmetricDifference, quad, op1, op2, op3)
END FoldSymmetricDifference ;


(*
   CodeSetSymmetricDifference - code set difference.
*)

PROCEDURE CodeSetSymmetricDifference (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinarySet(BuildSymmetricDifference, SetSymmetricDifference,
                 quad, op1, op2, op3)
END CodeSetSymmetricDifference ;


(*
   CodeUnarySet - encode a unary set arithmetic operation.
                  Set operands may be longer than a word.
*)

PROCEDURE CodeUnarySet (unop: BuildUnaryProcedure; doOp: DoUnaryProcedure;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, op3) ;
   DeclareConstructor(quad, op3) ;
   IF IsConst(op1)
   THEN
      IF IsValueSolved(op3)
      THEN
         Assert(FindType(op3)#NulSym) ;
         PutConst(op1, FindType(op3)) ;
         PushValue(op3) ;
         doOp(CurrentQuadToken) ;
         PopValue(op1) ;
         PushValue(op1) ;
         PutConstSet(op1) ;
         ConstantKnownAndUsed(op1,
                              DeclareKnownConstant(Mod2Gcc(GetType(op3)),
                                                   PopSetTree(CurrentQuadToken)))
      ELSE
         ErrorStringAt(InitString('constant expression cannot be evaluated'), CurrentQuadToken)
      END
   ELSE
      BuildUnaryForeachWordDo(Mod2Gcc(GetType(op1)), Mod2Gcc(op1), Mod2Gcc(op3), unop,
                              GetMode(op1)=LeftValue, GetMode(op3)=LeftValue,
                              IsConst(op1), IsConst(op3))
   END
END CodeUnarySet ;


(*
   FoldIncl - check whether we can fold the InclOp.
              op1 := op1 + (1 << op3)
*)

PROCEDURE FoldIncl (tokenno: CARDINAL; p: WalkAction;
                    quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   TryDeclareConstant(tokenno, op3) ;
   IF IsConst(op1) AND IsConst(op3)
   THEN
      IF GccKnowsAbout(op3) AND IsValueSolved(op1)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         PushValue(op1) ;
         AddBit(tokenno, op3) ;
         AddModGcc(op1, PopSetTree(tokenno)) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad)
      END
   END
END FoldIncl ;


(*
   FoldIfIn - check whether we can fold the IfInOp
              if op1 in op2 then goto op3
*)

PROCEDURE FoldIfLess (tokenno: CARDINAL; p: WalkAction;
                      quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   TryDeclareConstant(tokenno, op1) ;
   TryDeclareConstant(tokenno, op2) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      IF IsValueSolved(op1) AND IsValueSolved(op2)
      THEN
         (* fine, we can take advantage of this and evaluate the condition *)
         PushValue(op1) ;
         PushValue(op2) ;
         IF Less(tokenno)
         THEN
            PutQuad(quad, GotoOp, NulSym, NulSym, op3)
         ELSE
            SubQuad(quad)
         END
      END
   END
END FoldIfLess ;


(*
   FoldIfIn - check whether we can fold the IfInOp
              if op1 in op2 then goto op3
*)

PROCEDURE FoldIfIn (tokenno: CARDINAL; p: WalkAction;
                    quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   TryDeclareConstant(tokenno, op1) ;
   TryDeclareConstant(tokenno, op2) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      IF IsValueSolved(op1) AND IsValueSolved(op2)
      THEN
         (* fine, we can take advantage of this and evaluate the condition *)
         PushValue(op2) ;
         IF SetIn(tokenno, op1)
         THEN
            PutQuad(quad, GotoOp, NulSym, NulSym, op3)
         ELSE
            SubQuad(quad)
         END
      END
   END
END FoldIfIn ;


(*
   FoldIfNotIn - check whether we can fold the IfNotInOp
                 if not (op1 in op2) then goto op3
*)

PROCEDURE FoldIfNotIn (tokenno: CARDINAL; p: WalkAction;
                       quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   TryDeclareConstant(tokenno, op1) ;
   TryDeclareConstant(tokenno, op2) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      IF IsValueSolved(op1) AND IsValueSolved(op2)
      THEN
         (* fine, we can take advantage of this and evaluate the condition *)
         PushValue(op2) ;
         IF NOT SetIn(tokenno, op1)
         THEN
            PutQuad(quad, GotoOp, NulSym, NulSym, op3)
         ELSE
            SubQuad(quad)
         END
      END
   END
END FoldIfNotIn ;


(*
   GetSetLimits - assigns low and high to the limits of the declared, set.
*)

PROCEDURE GetSetLimits (set: CARDINAL; VAR low, high: CARDINAL) ;
VAR
   type: CARDINAL ;
BEGIN
   type := GetType(set) ;
   IF IsSubrange(type)
   THEN
      GetSubrange(type, high, low) ;
   ELSE
      low := GetTypeMin(type) ;
      high := GetTypeMax(type)
   END
END GetSetLimits ;


(*
   GetFieldNo - returns the field number in the, set, which contains, element.
*)

PROCEDURE GetFieldNo (tokenno: CARDINAL; element: CARDINAL; set: CARDINAL; VAR offset: Tree) : INTEGER ;
VAR
   type, low, high, bpw, c: CARDINAL ;
BEGIN
   bpw := GetBitsPerBitset() ;
   GetSetLimits(set, low, high) ;

   (* check element is legal *)

   PushValue(element) ;
   PushValue(low) ;
   IF Less(tokenno)
   THEN
      (* out of range *)
      RETURN( -1 )
   ELSE
      PushValue(element) ;
      PushValue(high) ;
      IF Gre(tokenno)
      THEN
         RETURN( -1 )
      END
   END ;

   (* all legal *)

   PushValue(low) ;
   offset := PopIntegerTree() ;
   c := 0 ;
   PushValue(element) ;
   PushValue(low) ;
   PushCard(bpw) ;
   Addn ;
   WHILE GreEqu(tokenno) DO
      INC(c) ;   (* move onto next field *)
      PushValue(element) ;
      PushCard((c+1)*bpw) ;
      PushValue(low) ;
      Addn ;
      PushIntegerTree(offset) ;
      PushCard(bpw) ;
      Addn ;
      offset := PopIntegerTree() ;
   END ;
   RETURN( VAL(INTEGER, c) )
END GetFieldNo ;


(*
   CodeIncl - encode an InclOp:
              op1 := op1 + (1 << op3)
*)

PROCEDURE CodeIncl (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   s      : String ;
   low,
   high   : CARDINAL ;
   offset : Tree ;
   fieldno: INTEGER ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, op3) ;
   IF IsConst(op1)
   THEN
      IF IsConst(op3)
      THEN
         InternalError('this quadruple should have been removed by FoldIncl', __FILE__, __LINE__)
      ELSE
         InternalError('should not get to here (why are we generating <incl const, var> ?)', __FILE__, __LINE__)
      END
   ELSE
      IF IsConst(op3)
      THEN
         fieldno := GetFieldNo(CurrentQuadToken, op3, GetType(op1), offset) ;
         IF fieldno>=0
         THEN
            PushValue(op3) ;
            PushIntegerTree(offset) ;
            Sub ;
            BuildIncludeVarConst(Mod2Gcc(GetType(op1)),
                                 Mod2Gcc(op1), PopIntegerTree(),
                                 GetMode(op1)=LeftValue, fieldno)
         ELSE
            MetaErrorT1(CurrentQuadToken, 'bit exceeded the range of set {%1atd}', op1)
         END
      ELSE
         GetSetLimits(GetType(op1), low, high) ;
         BuildIncludeVarVar(Mod2Gcc(GetType(op1)),
                            Mod2Gcc(op1), Mod2Gcc(op3), GetMode(op1)=LeftValue, Mod2Gcc(low))
      END
   END
END CodeIncl ;


(*
   FoldExcl - check whether we can fold the InclOp.
              op1 := op1 - (1 << op3)
*)

PROCEDURE FoldExcl (tokenno: CARDINAL; p: WalkAction;
                    quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   TryDeclareConstant(tokenno, op3) ;
   IF IsConst(op1) AND IsConst(op3)
   THEN
      IF GccKnowsAbout(op3) AND IsValueSolved(op1)
      THEN
         PushValue(op1) ;
         SubBit(tokenno, op3) ;
         AddModGcc(op1, PopSetTree(tokenno)) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad)
      END
   END
END FoldExcl ;


(*
   CodeExcl - encode an ExclOp:
                op1 := op1 - (1 << op3)
*)

PROCEDURE CodeExcl (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   s       : String ;
   low,
   high    : CARDINAL ;
   offset  : Tree ;
   fieldno : INTEGER ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, op3) ;
   IF IsConst(op1)
   THEN
      InternalError('should not get to here (if we do we should consider calling FoldInclOp)', __FILE__, __LINE__)
   ELSE
      IF IsConst(op3)
      THEN
         fieldno := GetFieldNo(CurrentQuadToken, op3, GetType(op1), offset) ;
         IF fieldno>=0
         THEN
            PushValue(op3) ;
            PushIntegerTree(offset) ;
            Sub ;
            BuildExcludeVarConst(Mod2Gcc(GetType(op1)),
                                 Mod2Gcc(op1), PopIntegerTree(),
                                 GetMode(op1)=LeftValue, fieldno)
         ELSE
            MetaErrorT1(CurrentQuadToken, 'bit exceeded the range of set {%1atd}', op1)
         END
      ELSE
         GetSetLimits(GetType(op1), low, high) ;
         BuildExcludeVarVar(Mod2Gcc(GetType(op1)),
                            Mod2Gcc(op1), Mod2Gcc(op3), GetMode(op1)=LeftValue, Mod2Gcc(low))
      END
   END
END CodeExcl ;


(*
   FoldUnary - check whether we can fold the unop operation.
*)

PROCEDURE FoldUnary (tokenno: CARDINAL; p: WalkAction;
                     unop: BuildUnaryProcedure; ZConstToTypedConst: Tree;
                     quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   tv: Tree ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   TryDeclareConstant(tokenno, op3) ;
   IF IsConst(op3)
   THEN
      IF GccKnowsAbout(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            IF ZConstToTypedConst=Tree(NIL)
            THEN
               IF (GetType(op3)=NulSym) OR IsOrdinalType(SkipType(GetType(op3)))
               THEN
                  ZConstToTypedConst := GetM2ZType()
               ELSIF IsRealType(SkipType(GetType(op3))) OR IsRealN(SkipType(GetType(op3)))
               THEN
                  ZConstToTypedConst := GetM2RType()
               ELSIF IsAComplexType(SkipType(GetType(op3))) OR
                     IsComplexN(SkipType(GetType(op3)))
               THEN
                  ZConstToTypedConst := GetM2CType()
               END
            END ;
            IF GetType(op1)=NulSym
            THEN
               PutConst(op1, NegateType(GetType(op3), tokenno))
            END ;
            tv := unop(LValueToGenericPtrOrConvert(op3, ZConstToTypedConst), FALSE) ;
            CheckOrResetOverflow(tokenno, tv, MustCheckOverflow(quad)) ;

            AddModGcc(op1, DeclareKnownConstant(ZConstToTypedConst, tv)) ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         ELSE
            (* we can still fold the expression, but not the assignment, however, we will
               not do this here but in CodeUnary
             *)
         END
      END
   END
END FoldUnary ;


(*
   FoldUnarySet - check whether we can fold the doOp operation.
*)

PROCEDURE FoldUnarySet (tokenno: CARDINAL; p: WalkAction; doOp: DoUnaryProcedure;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   (* firstly try and ensure that constants are declared *)
   TryDeclareConstant(tokenno, op3) ;
   IF IsConst(op3) AND IsConstSet(op3) AND
      IsConst(op1)
   THEN
      IF IsValueSolved(op3) AND (GetType(op3)#NulSym)
      THEN
         PutConst(op1, FindType(op3)) ;
         PushValue(op3) ;
         doOp(tokenno) ;
         PopValue(op1) ;
         PushValue(op1) ;
         PutConstSet(op1) ;
         AddModGcc(op1,
                   DeclareKnownConstant(Mod2Gcc(GetType(op3)),
                                        PopSetTree(QuadToTokenNo(quad)))) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad)
      END
   END
END FoldUnarySet ;


(*
   CodeUnary - encode a unary arithmetic operation.
*)

PROCEDURE CodeUnary (unop: BuildUnaryProcedure; ZConstToTypedConst: Tree;
                     quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t, tv: Tree ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, op3) ;
   DeclareConstructor(quad, op3) ;
   tv := unop(LValueToGenericPtr(op3), FALSE) ;
   CheckOrResetOverflow(CurrentQuadToken, tv, MustCheckOverflow(quad)) ;
   IF IsConst(op1)
   THEN
      IF ZConstToTypedConst=Tree(NIL)
      THEN
         ZConstToTypedConst := Tree(Mod2Gcc(GetType(op3)))
      END ;
      (* still have a constant which was not resolved, pass it to gcc *)
      PutConst(op1, FindType(op3)) ;
      ConstantKnownAndUsed(op1, DeclareKnownConstant(ZConstToTypedConst, tv))
   ELSE
      t := BuildAssignmentTree(Mod2Gcc(op1), tv)
   END
END CodeUnary ;


(*
   FoldNegate - check unary negate for constant folding.
*)

PROCEDURE FoldNegate (tokenno: CARDINAL; p: WalkAction;
                      quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF IsConstSet(op3)
   THEN
      FoldUnarySet(tokenno, p, SetNegate, quad, op1, op2, op3)
   ELSE
      FoldUnary(tokenno, p, BuildNegate, NIL, quad, op1, op2, op3)
   END
END FoldNegate ;


(*
   CodeNegate - encode unary negate.
*)

PROCEDURE CodeNegate (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF IsConstSet(op3) OR IsSet(GetType(op3))
   THEN
      CodeUnarySet(BuildSetNegate, SetNegate, quad, op1, op2, op3)
   ELSE
      CodeUnary(BuildNegate, NIL, quad, op1, op2, op3)
   END
END CodeNegate ;


(*
   FoldSize - check unary SIZE for constant folding.
*)

PROCEDURE FoldSize (tokenno: CARDINAL; p: WalkAction;
                    quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t: Tree ;
   l: List ;
BEGIN
   IF IsConst(op1) AND CompletelyResolved(op3)
   THEN
      IF op2=NulSym
      THEN
         t := BuildSize(Mod2Gcc(op3), FALSE) ;
         PushIntegerTree(t) ;
         PopValue(op1) ;
         PutConst(op1, Cardinal) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad) ;
         t := RememberConstant(t)
      ELSIF GccKnowsAbout(op2)
      THEN
         (* ignore the chosen varients as we implement it as a C union *)
         t := BuildSize(Mod2Gcc(op3), FALSE) ;
         PushIntegerTree(t) ;
         PopValue(op1) ;
         PutConst(op1, Cardinal) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad) ;
         t := RememberConstant(t)
      END
   END
END FoldSize ;


(*
   CodeSize - encode the inbuilt SIZE function.
*)

PROCEDURE CodeSize (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t: Tree ;
   s: CARDINAL ;
   l: List ;
BEGIN
   IF op2=NulSym
   THEN
      PushIntegerTree(BuildSize(Mod2Gcc(op3), FALSE))
   ELSE
      (* ignore the chosen varients as we implement it as a C union *)
      t := BuildSize(Mod2Gcc(op3), FALSE)
   END ;
   IF IsConst(op1)
   THEN
      PopValue(op1) ;
      PutConst(op1, Cardinal) ;
      PushValue(op1) ;
      ConstantKnownAndUsed(op1,
                           DeclareKnownConstant(GetIntegerType(),
                                                PopIntegerTree()))
   ELSE
      t := BuildAssignmentTree(Mod2Gcc(op1), PopIntegerTree())
   END
END CodeSize ;


(*
   DetermineFieldOf - is sadly complicated by the way varient records are encoded in the front end
                      symbol table. The symbol, sym, is a RecordField which is either in the structure:

                      RecordSym
                         RecordField: Type ;
                         RecordField: Type ;
                      End

                      or alternatively:

                      RecordSym
                         Varient:  VarientField: RecordField: Type ;
                                                 RecordField: Type ;
                                   VarientField: RecordField: Type ;
                                                 RecordField: Type ;
                         Varient:  VarientField: RecordField: Type ;
                                                 RecordField: Type ;
                                   VarientField: RecordField: Type ;
                                                 RecordField: Type ;
                      End

                      Thus when we are asked to calculate Offset RecordField
                      we need to know which of the two alternatives we are dealing with.
                      The GCC BuildOffset calculates the offset between RecordField its
                      Varient parent. We need to add the offset between varient parent and
                      the RecordSym. This code is bridging the difference in symbol table
                      construction between the front end and GCC.

                      We return the Varient symbol if sym was declared in the second method.
*)

PROCEDURE DetermineFieldOf (parent, sym: CARDINAL) : CARDINAL ;
VAR
   varient: CARDINAL ;
BEGIN
   Assert(IsRecordField(sym)) ;
   varient := GetVarient(sym) ;
   IF (varient=NulSym) OR IsRecord(varient)
   THEN
      RETURN( NulSym )
   ELSE
      sym := NulSym ;
      WHILE (varient#NulSym) AND (IsVarient(varient) OR IsFieldVarient(varient)) DO
         sym := varient ;
         varient := GetVarient(varient)
      END ;
      RETURN( sym )
   END
END DetermineFieldOf ;


(*
   FoldOffset - check whether we can fold an OffsetOp quadruple.
                Very similar to FoldUnary, except that we need to hard code
                a few parameters to the gcc backend.
*)

PROCEDURE FoldOffset (tokenno: CARDINAL; p: WalkAction;
                      quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   field: CARDINAL ;
   t    : Tree ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   TryDeclareConstant(tokenno, op3) ;
   IF IsRecordField(op3) OR IsFieldVarient(op3)
   THEN
      IF GccKnowsAbout(op2) AND GccKnowsAbout(op3) AND
         CompletelyResolved(op2) AND CompletelyResolved(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            t := BuildOffset(Mod2Gcc(op2), Mod2Gcc(op3), FALSE) ;
            IF NOT IsValueSolved(op1)
            THEN
               PushIntegerTree(t) ;
               PopValue(op1)
            END ;
            PutConst(op1, Address) ;
            AddModGcc(op1,
                      DeclareKnownConstant(GetPointerType(),
                                           t)) ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         ELSE
            (* we can still fold the expression, but not the assignment, however, we will
               not do this here but in CodeOffset
             *)
         END
      END
   END
END FoldOffset ;


(*
   CodeOffset - encode an OffsetOp quadruple. Very similar to CodeUnary, except that
                we need to hard code a few parameters to the gcc backend.
                op1 is set to contain the offset (in bytes) of field op3
                from its parent record.
*)

PROCEDURE CodeOffset (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   field: CARDINAL ;
   t    : Tree ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   IF IsRecordField(op3) OR IsFieldVarient(op3)
   THEN
      IF GccKnowsAbout(op2) AND GccKnowsAbout(op3) AND
         CompletelyResolved(op2) AND CompletelyResolved(op3)
      THEN
         t := BuildOffset(Mod2Gcc(op2), Mod2Gcc(op3), FALSE) ;
         IF IsConst(op1)
         THEN
            (* fine, we can take advantage of this and fold constants *)
            IF NOT IsValueSolved(op1)
            THEN
               PushIntegerTree(t) ;
               PopValue(op1)
            END ;
            PutConst(op1, Address) ;
            ConstantKnownAndUsed(op1,
                                 DeclareKnownConstant(GetPointerType(),
                                                      t))
         ELSE
            (* ok, use assignment *)
            t := BuildAssignmentTree(Mod2Gcc(op1), t)
         END
      ELSE
         InternalError('symbol type should have been declared by now..', __FILE__, __LINE__)
      END
   ELSE
      InternalError('not expecting this type of symbol', __FILE__, __LINE__)
   END
END CodeOffset ;


(*
   BuildHighFromChar - 
*)

PROCEDURE BuildHighFromChar (operand: CARDINAL) : Tree ;
BEGIN
   RETURN( GetIntegerZero() )   
END BuildHighFromChar ;


(*
   SkipToArray - 
*)

PROCEDURE SkipToArray (operand, dim: CARDINAL) : CARDINAL ;
VAR
   type: CARDINAL ;
BEGIN
   WHILE dim>1 DO
      type := SkipType(GetType(operand)) ;
      IF IsArray(type)
      THEN
         operand := type
      END ;
      DEC(dim)
   END ;
   RETURN( operand )
END SkipToArray ;


(*
   BuildHighFromArray - 
*)

PROCEDURE BuildHighFromArray (dim, operand: CARDINAL) : Tree ;
VAR
   Type,
   High, Low: CARDINAL ;
   Subscript,
   Subrange : CARDINAL ;
BEGIN
   Type := SkipType(GetType(SkipToArray(operand, dim))) ;
   Subscript := GetArraySubscript(Type) ;
   Subrange := SkipType(GetType(Subscript)) ;
   IF IsEnumeration(Subrange)
   THEN
      GetBaseTypeMinMax(Subrange, Low, High) ;
      IF GccKnowsAbout(High)
      THEN
         RETURN( Tree(Mod2Gcc(High)) )
      END
   ELSIF IsSubrange(Subrange)
   THEN
      GetSubrange(Subrange, High, Low) ;
      IF GccKnowsAbout(Low) AND GccKnowsAbout(High)
      THEN
         RETURN( BuildSub(Mod2Gcc(High), Mod2Gcc(Low), FALSE) )
      END
   ELSE
      MetaError1('array subscript {%1Dad:for} must be a subrange or enumeration type', Type) ;
      RETURN( Tree(NIL) )
   END ;
   IF GccKnowsAbout(High)
   THEN
      RETURN( Tree(Mod2Gcc(High)) )
   ELSE
      RETURN( Tree(NIL) )
   END
END BuildHighFromArray ;


(*
   BuildHighFromString - 
*)

PROCEDURE BuildHighFromString (operand: CARDINAL) : Tree ;
BEGIN
   IF GccKnowsAbout(operand) AND (StringLength(Mod2Gcc(operand))>0)
   THEN
      RETURN( BuildIntegerConstant(StringLength(Mod2Gcc(operand))-1) )
   ELSE
      RETURN( GetIntegerZero() )
   END
END BuildHighFromString ;


(*
   ResolveHigh - given an Modula-2 operand, it resolves the HIGH(operand)
                 and returns a GCC constant symbol containing the value of
                 HIGH(operand).
*)

PROCEDURE ResolveHigh (quad: CARDINAL; dim, operand: CARDINAL) : Tree ;
VAR
   Type: CARDINAL ;
BEGIN
   Type := SkipType(GetType(operand)) ;
   IF (Type=Char) AND (dim=1)
   THEN
      RETURN( BuildHighFromChar(operand) )
   ELSIF IsConstString(operand) AND (dim=1)
   THEN
      RETURN( BuildHighFromString(operand) )
   ELSIF IsArray(Type)
   THEN
      RETURN( BuildHighFromArray(dim, operand) )
   ELSIF IsUnbounded(Type)
   THEN
      RETURN( GetHighFromUnbounded(dim, operand) )
   ELSE
      MetaErrorT1(QuadToTokenNo(quad),
                  'base procedure HIGH expects a variable of type array or a constant string or CHAR as its parameter, rather than {%1tad}',
                  operand) ;
      RETURN( GetIntegerZero() )
   END
END ResolveHigh ;


(*
   FoldHigh - if the array is not dynamic then we should be able to
              remove the HighOp quadruple and assign op1 with
              the known compile time HIGH(op3).
*)

PROCEDURE FoldHigh (tokenno: CARDINAL; p: WalkAction;
                    quad: CARDINAL; op1, dim, op3: CARDINAL) ;
VAR
   t       : Tree ;
   high,
   low,
   type    : CARDINAL ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   TryDeclareConstant(tokenno, op3) ;
   IF GccKnowsAbout(op3) AND CompletelyResolved(op3)
   THEN
      t := ResolveHigh(quad, dim, op3) ;
      (* fine, we can take advantage of this and fold constants *)
      IF IsConst(op1) AND (t#Tree(NIL))
      THEN
         PutConst(op1, Cardinal) ;
         AddModGcc(op1,
                   DeclareKnownConstant(GetIntegerType(),
                                        t)) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad)
      ELSE
         (* we can still fold the expression, but not the assignment, however, we will
            not do this here but in CodeHigh
         *)
      END
   END
END FoldHigh ;


(*
   CodeHigh - encode a unary arithmetic operation.
*)

PROCEDURE CodeHigh (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t: Tree ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, op3) ;
   IF IsConst(op1)
   THEN
      (* still have a constant which was not resolved, pass it to gcc *)
      ConstantKnownAndUsed(op1,
                           DeclareKnownConstant(GetM2ZType(),
                                                ResolveHigh(quad, op2, op3)))
   ELSE
      t := BuildAssignmentTree(Mod2Gcc(op1),
                               ResolveHigh(quad, op2, op3))
   END
END CodeHigh ;


(*
   CodeUnbounded - codes the creation of an unbounded parameter variable.
                   places the address of op3 into *op1
*)

PROCEDURE CodeUnbounded (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   Addr,
   t   : Tree ;
BEGIN
   DeclareConstant(CurrentQuadToken, op3) ;
   IF IsConstString(op3)
   THEN
      t := BuildAssignmentTree(BuildIndirect(Mod2Gcc(op1), GetPointerType()),
                               BuildAddr(PromoteToString(CurrentQuadToken, op3),
                                         FALSE))
   ELSIF IsConstructor(op3)
   THEN
      t := BuildAssignmentTree(BuildIndirect(Mod2Gcc(op1), GetPointerType()),
                               BuildAddr(Mod2Gcc(op3), TRUE))
   ELSIF IsUnbounded(GetType(op3))
   THEN
      Addr := BuildIndirect(BuildAdd(BuildAddr(Mod2Gcc(op3), FALSE),
                                     BuildOffset1(Mod2Gcc(GetUnboundedAddressOffset(GetType(op3))), FALSE),
                                     FALSE),
                            GetPointerType()) ;
      t := BuildAssignmentTree(BuildIndirect(Mod2Gcc(op1), GetPointerType()), Addr)
   ELSIF GetMode(op3)=RightValue
   THEN
      t := BuildAssignmentTree(BuildIndirect(Mod2Gcc(op1), GetPointerType()), BuildAddr(Mod2Gcc(op3), FALSE))
   ELSE
      t := BuildAssignmentTree(BuildIndirect(Mod2Gcc(op1), GetPointerType()), Mod2Gcc(op3))
   END
END CodeUnbounded ;


(*
   AreSubrangesKnown - returns TRUE if the subranges values used within, array, are known.
*)

PROCEDURE AreSubrangesKnown (array: CARDINAL) : BOOLEAN ;
VAR
   type,
   subscript,
   low, high: CARDINAL ;
BEGIN
   IF GccKnowsAbout(array)
   THEN
      subscript := GetArraySubscript(array) ;
      IF subscript=NulSym
      THEN
         InternalError('not expecting a NulSym as a subscript', __FILE__, __LINE__)
      ELSE
         type := SkipType(GetType(subscript)) ;
         low  := GetTypeMin(type) ;
         high := GetTypeMax(type) ;
         RETURN( GccKnowsAbout(low) AND GccKnowsAbout(high) )
      END
   ELSE
      RETURN( FALSE )
   END
END AreSubrangesKnown ;


(*
   CalculateBase - works out the constant &array[declaration] - &array[0,0,0,0,etc]
*)

PROCEDURE CalculateBase (array: CARDINAL) : Tree ;
VAR
   offset,
   size    : Tree ;
   High,
   Low     : CARDINAL ;
   Subscript,
   type    : CARDINAL ;
BEGIN
   offset    := GetPointerZero() ;
   Subscript := GetArraySubscript(array) ;
   IF Subscript#NulSym
   THEN
      size     := BuildSize(Mod2Gcc(Subscript), FALSE) ;  (* Size for element *)
      type     := SkipType(GetType(Subscript)) ;
      Low      := GetTypeMin(type) ;
      High     := GetTypeMax(type) ;
      offset   := BuildSub(BuildConvert(GetM2ZType(), offset, FALSE),
                           BuildConvert(GetM2ZType(),
                                        BuildMult(size,
                                                  BuildConvert(GetM2ZType(),
                                                               Mod2Gcc(Low),
                                                               FALSE),
                                                  FALSE),
                                        FALSE),
                           FALSE) ;
      offset   := RemoveOverflow(BuildConvert(GetPointerType(), offset, FALSE))
   END ;
   RETURN( offset )
END CalculateBase ;


(*
   CodeArray - res is an lvalue which will point to the array element.
*)

PROCEDURE CodeArray (quad: CARDINAL; res, index, array: CARDINAL) ;
VAR
   resType,
   arrayDecl,
   type, low,
   subscript  : CARDINAL ;
   elementSize,
   t, a, ta   : Tree ;
BEGIN
   arrayDecl := SkipType(GetType(array)) ;
   IF AreSubrangesKnown(arrayDecl)
   THEN
      subscript := GetArraySubscript(arrayDecl) ;
      type := SkipType(GetType(subscript)) ;
      low  := GetTypeMin(type) ;
      resType := GetVarBackEndType(res) ;
      IF resType=NulSym
      THEN
         resType := GetType(res)
      END ;
      elementSize := BuildSize(Mod2Gcc(GetType(arrayDecl)), FALSE) ;
      ta := Mod2Gcc(SkipType(GetType(arrayDecl))) ;
      IF GetMode(array)=LeftValue
      THEN
         a := BuildIndirect(Mod2Gcc(array), Mod2Gcc(SkipType(GetType(array))))
      ELSE
         a := Mod2Gcc(array)
      END ;
      t := BuildAssignmentTree(Mod2Gcc(res),
                               BuildConvert(Mod2Gcc(resType),
                                            BuildAddr(BuildArray(ta, a,
                                                                 Mod2Gcc(index),
                                                                 Mod2Gcc(low),
                                                                 elementSize),
                                                      FALSE),
                                            FALSE)) ;
      AddStatement(t)
   ELSE
      InternalError('subranges not yet resolved', __FILE__, __LINE__)
   END
END CodeArray ;


(*
   FoldElementSizeForArray - attempts to calculate the Subscript
                             multiplier for the index op3.
*)

PROCEDURE FoldElementSizeForArray (quad: CARDINAL; p: WalkAction;
                                   op1, type, op3: CARDINAL) ;
VAR
   Subscript: CARDINAL ;
BEGIN
   IF IsConst(op1) AND (NOT GccKnowsAbout(op1))
   THEN
      Subscript := GetArraySubscript(type) ;
      IF IsSizeSolved(Subscript)
      THEN
         PutConst(op1, Integer) ;
         PushSize(Subscript) ;
         AddModGcc(op1,
                   DeclareKnownConstant(GetIntegerType(),
                                        PopIntegerTree())) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad)
      END
   END
END FoldElementSizeForArray ;


(*
   FoldElementSizeForUnbounded - Unbounded arrays only have one index,
                                 therefore element size will be the
                                 TSIZE(Type) where Type is defined as:
                                 ARRAY OF Type.
*)

PROCEDURE FoldElementSizeForUnbounded (quad: CARDINAL; p: WalkAction;
                                       op1, ArrayType, op3: CARDINAL) ;
VAR
   Type: CARDINAL ;
BEGIN
   IF IsConst(op1)
   THEN
      IF GccKnowsAbout(op1)
      THEN
         InternalError('cannot assign a value twice to a constant', __FILE__, __LINE__)
      ELSE
         Assert(IsUnbounded(ArrayType)) ;
         Type := GetType(ArrayType) ;
         IF GccKnowsAbout(Type)
         THEN
            PutConst(op1, Cardinal) ;
            AddModGcc(op1,
                      DeclareKnownConstant(GetIntegerType(),
                                           FindSize(Type))) ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         END
      END
   END
END FoldElementSizeForUnbounded ;


(*
   FoldElementSize - folds the element size for an ArraySym or UnboundedSym.
                     ElementSize returns a constant which defines the
                     multiplier to be multiplied by this element index.
*)

PROCEDURE FoldElementSize (tokenno: CARDINAL; p: WalkAction;
                           quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF IsUnbounded(op2)
   THEN
      FoldElementSizeForUnbounded(quad, p, op1, op2, op3)
   ELSIF IsArray(op2)
   THEN
      FoldElementSizeForArray(quad, p, op1, op2, op3)
   ELSE
      InternalError('expecting UnboundedSym or ArraySym', __FILE__, __LINE__)
   END
END FoldElementSize ;


(*

*)

PROCEDURE CodeElementSize (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   InternalError('strange - expected ElementSizeOp to be folded via constant evaluation', __FILE__, __LINE__)
END CodeElementSize ;


(*
   FoldConvert - attempts to fold op3 to type op2 placing the result into
                 op1, providing that op1 and op3 are constants.
                 Convert will, if need be, alter the machine representation
                 of op3 to comply with TYPE op2.
*)

PROCEDURE FoldConvert (tokenno: CARDINAL; p: WalkAction;
                       quad: CARDINAL; op1, op2, op3: CARDINAL) ;

VAR
   tl, tr: Tree ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   TryDeclareConstant(tokenno, op3) ;
   IF IsConst(op3)
   THEN
      IF GccKnowsAbout(op2) AND IsValueSolved(op3) AND
         GccKnowsAbout(SkipType(op2))
      THEN
         (* fine, we can take advantage of this and fold constant *)
         IF IsConst(op1)
         THEN
            PutConst(op1, op2) ;
            tl := Mod2Gcc(SkipType(op2)) ;
            PushValue(op3) ;
            IF IsConstSet(op3)
            THEN
               IF IsSet(SkipType(op2))
               THEN
                  WriteFormat0('cannot convert values between sets')
               ELSE
                  PushIntegerTree(FoldAndStrip(BuildConvert(tl, PopSetTree(tokenno), TRUE))) ;
                  PopValue(op1) ;
                  PushValue(op1) ;
                  AddModGcc(op1, PopIntegerTree())
               END
            ELSE
               IF IsSet(SkipType(op2))
               THEN
                  PushSetTree(tokenno,
                              FoldAndStrip(BuildConvert(tl, PopIntegerTree(),
                                                        TRUE)), SkipType(op2)) ;
                  PopValue(op1) ;
                  PutConstSet(op1) ;
                  PushValue(op1) ;
                  AddModGcc(op1, PopSetTree(tokenno))
               ELSIF IsRealType(SkipType(op2))
               THEN
                  PushRealTree(FoldAndStrip(BuildConvert(tl, PopIntegerTree(),
                                                         TRUE))) ;
                  PopValue(op1) ;
                  PushValue(op1) ;
                  AddModGcc(op1, PopRealTree())
               ELSE
                  (* we let CheckOverflow catch a potential overflow rather than BuildConvert *)
                  PushIntegerTree(FoldAndStrip(BuildConvert(tl,
                                                            PopIntegerTree(),
                                                            FALSE))) ;
                  PopValue(op1) ;
                  PushValue(op1) ;
                  CheckOrResetOverflow(tokenno, PopIntegerTree(), MustCheckOverflow(quad)) ;
                  PushValue(op1) ;
                  AddModGcc(op1, PopIntegerTree())
               END
            END ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         END
      END
   END
END FoldConvert ;


(*
   CodeConvert - Converts op3 to type op2 placing the result into
                 op1.
                 Convert will, if need be, alter the machine representation
                 of op3 to comply with TYPE op2.
*)

PROCEDURE CodeConvert (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t,
   tl, tr: Tree ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, op3) ;
   DeclareConstructor(quad, op3) ;
   tl := LValueToGenericPtr(op2) ;
   IF IsProcedure(op3)
   THEN
      tr := BuildAddr(Mod2Gcc(op3), FALSE)
   ELSE
      tr := LValueToGenericPtr(op3)
   END ;
   IF IsConst(op1)
   THEN
      (* fine, we can take advantage of this and fold constant *)
      PutConst(op1, op2) ;
      tl := Mod2Gcc(SkipType(op2)) ;
      ConstantKnownAndUsed(op1,
                           BuildConvert(tl, Mod2Gcc(op3), TRUE))
   ELSE
      t := BuildAssignmentTree(Mod2Gcc(op1), BuildConvert(tl, tr, TRUE))
   END
END CodeConvert ;


(*
   CodeCoerce - Coerce op3 to type op2 placing the result into
                op1.
                Coerce will NOT alter the machine representation
                of op3 to comply with TYPE op2.
                Therefore it _insists_ that under all circumstances that the
                type sizes of op1 and op3 are the same.
                CONVERT will perform machine manipulation to change variable
                types, coerce does no such thing.
*)

PROCEDURE CodeCoerce (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t: Tree ;
BEGIN
   DeclareConstant(CurrentQuadToken, op3) ;  (* checks to see whether it is a constant literal and declares it *)
   DeclareConstructor(quad, op3) ;
   IF IsProcedure(op3)
   THEN
      IF AreConstantsEqual(FindSize(op1), FindSize(Address))
      THEN
         IF IsConst(op1)
         THEN
            ConstantKnownAndUsed(op1, CheckConstant(op1, op3))
         ELSE
            t := BuildAssignmentTree(Mod2Gcc(op1), Mod2Gcc(op3))
         END
      ELSE
         ErrorStringAt(InitString('procedure address can only be stored in an address sized operand'),
                       CurrentQuadToken)
      END
   ELSIF IsConst(op3) OR AreConstantsEqual(FindSize(op1), FindSize(op3))
   THEN
      IF IsConst(op1)
      THEN
         ConstantKnownAndUsed(op1,
                              DeclareKnownConstant(Mod2Gcc(GetType(op1)),
                                                   Mod2Gcc(op3)))
      ELSE
         Assert(GccKnowsAbout(op2)) ;
         IF IsConst(op3)
         THEN
            t := BuildAssignmentTree(Mod2Gcc(op1), Mod2Gcc(op3))
         ELSE
            (* does not work t := BuildCoerce(Mod2Gcc(op1), Mod2Gcc(op2), Mod2Gcc(op3)) *)
            AddStatement(BuiltInMemCopy(BuildAddr(Mod2Gcc(op1), FALSE),
                                        BuildAddr(Mod2Gcc(op3), FALSE),
                                        FindSize(op2)))
         END
      END
   ELSE
      ErrorStringAt(InitString('can only CAST objects of the same size'),
                    QuadToTokenNo(quad))
   END
END CodeCoerce ;


(*
   FoldCoerce -
*)

PROCEDURE FoldCoerce (tokenno: CARDINAL; p: WalkAction;
                      quad, op1, op2, op3: CARDINAL) ;
BEGIN
   TryDeclareConstant(tokenno, op3) ;  (* checks to see whether it is a constant literal and declares it *)
   IF GccKnowsAbout(op2) AND GccKnowsAbout(op3)
   THEN
      IF IsProcedure(op3)
      THEN
         IF AreConstantsEqual(FindSize(op1), FindSize(Address))
         THEN
            IF IsConst(op1)
            THEN
               AddModGcc(op1,
                         DeclareKnownConstant(Mod2Gcc(GetType(op1)),
                                              Mod2Gcc(op3))) ;
               p(op1) ;
               NoChange := FALSE ;
               SubQuad(quad)
            END
         ELSE
            ErrorStringAt(InitString('procedure address can only be stored in a address sized operand'), QuadToTokenNo(quad))
         END
      ELSIF IsConst(op3)
      THEN
         IF IsConst(op1)
         THEN
            AddModGcc(op1,
                      DeclareKnownConstant(Mod2Gcc(GetType(op1)),
                                           Mod2Gcc(op3))) ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         END
      END
   END
END FoldCoerce ;


(*
   CodeCast - Cast op3 to type op2 placing the result into op1.
              Cast will NOT alter the machine representation
              of op3 to comply with TYPE op2 as long as SIZE(op3)=SIZE(op2).
              If the sizes differ then Convert is called.
*)

PROCEDURE CodeCast (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t: Tree ;
BEGIN
   DeclareConstant(CurrentQuadToken, op3) ;  (* checks to see whether it is a constant literal and declares it *)
   DeclareConstructor(quad, op3) ;
   IF IsProcedure(op3)
   THEN
      IF AreConstantsEqual(FindSize(op1), FindSize(Address))
      THEN
         IF IsConst(op1)
         THEN
            ConstantKnownAndUsed(op1, CheckConstant(op1, op3))
         ELSE
            t := BuildAssignmentTree(Mod2Gcc(op1), Mod2Gcc(op3))
         END
      ELSE
         ErrorStringAt(InitString('procedure address can only be stored in an address sized operand'),
                       CurrentQuadToken)
      END
   ELSIF IsConst(op3) OR AreConstantsEqual(FindSize(op1), FindSize(op3))
   THEN
      CodeCoerce(quad, op1, op2, op3)
   ELSE
      IF PedanticCast
      THEN
         WarnStringAt(InitString('CAST is converting a variable to a different sized type'),
                      QuadToTokenNo(quad))
      END ;
      CodeConvert(quad, op1, op2, op3)
   END
END CodeCast ;


(*
   FoldCoerce -
*)

PROCEDURE FoldCast (tokenno: CARDINAL; p: WalkAction;
                    quad, op1, op2, op3: CARDINAL) ;
BEGIN
   TryDeclareConstant(tokenno, op3) ;  (* checks to see whether it is a constant literal and declares it *)
   IF GccKnowsAbout(op2) AND GccKnowsAbout(op3)
   THEN
      IF IsProcedure(op3)
      THEN
         IF AreConstantsEqual(FindSize(op1), FindSize(Address))
         THEN
            FoldCoerce(tokenno, p, quad, op1, op2, op3)
         ELSE
            ErrorStringAt(InitString('procedure address can only be stored in an address sized operand'), QuadToTokenNo(quad))
         END
      ELSIF IsConst(op3)
      THEN
         FoldCoerce(tokenno, p, quad, op1, op2, op3)
      END
   END
END FoldCast ;


(*
   CodeMath - translates the MathOp into a GCC tree structure.
              Op2 := Op1(Op3)

              where:

              Op1    function
              Op2    return variable
              Op3    parameter
*)

PROCEDURE CodeMath (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t: Tree ;
BEGIN
   DeclareConstant(CurrentQuadToken, op3) ;  (* checks to see whether it is a constant literal and declares it *)
   DeclareConstructor(quad, op3) ;
   IF IsTrunc(op1)
   THEN
      t := BuildAssignmentTree(Mod2Gcc(op2), BuildTrunc(Mod2Gcc(op3)))
   ELSE
      InternalError('unknown math operator', __FILE__, __LINE__)
   END ;
END CodeMath ;


(*
   CreateLabelProcedureN - creates a label using procedure name and
                           an integer.
*)

PROCEDURE CreateLabelProcedureN (proc: CARDINAL; leader: ARRAY OF CHAR;
                                 unboundedCount, n: CARDINAL) : String ;
VAR
   n1, n2: String ;
BEGIN
   n1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(proc)))) ;
   n2 := Mark(InitString(leader)) ;
   (* prefixed by .L unboundedCount and n to ensure that no Modula-2 identifiers clash *)
   RETURN( Sprintf4(Mark(InitString('.L%d.%d.unbounded.%s.%s')), unboundedCount, n, n1, n2) )
END CreateLabelProcedureN ;


(*
   CreateLabelName - creates a namekey from quadruple, q.
*)

PROCEDURE CreateLabelName (q: CARDINAL) : String ;
BEGIN
   (* prefixed by . to ensure that no Modula-2 identifiers clash *)
   RETURN( Sprintf1(Mark(InitString('.L%d')), q) )
END CreateLabelName ;


(*
   CodeGoto - creates a jump to a labeled quadruple.
*)

PROCEDURE CodeGoto (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   BuildGoto(string(CreateLabelName(op3)))
END CodeGoto ;


(*
   CheckReferenced - checks to see whether this quadruple requires a label.
*)

PROCEDURE CheckReferenced (q: CARDINAL; op: QuadOperator) ;
BEGIN
   (* we do not create labels for procedure entries *)
   IF (op#ProcedureScopeOp) AND (op#NewLocalVarOp) AND IsReferenced(q)
   THEN
      DeclareLabel(string(CreateLabelName(q)))
   END
END CheckReferenced ;


(*
   CodeIfSetLess - 
*)

PROCEDURE CodeIfSetLess (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
BEGIN
   IF IsConst(op1) AND IsConst(op2)
   THEN
      InternalError('this should have been folded in the calling procedure',
                    __FILE__, __LINE__)
   ELSIF IsConst(op1)
   THEN
      settype := SkipType(GetType(op2))
   ELSE
      settype := SkipType(GetType(op1))
   END ;
   IF CompareTrees(FindSize(settype), FindSize(Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(BuildIsNotSuperset(BuildConvert(GetWordType(), Mod2Gcc(op1), FALSE),
                                BuildConvert(GetWordType(), Mod2Gcc(op2), FALSE)),
             NIL, string(CreateLabelName(op3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(Mod2Gcc(settype),
                                    Mod2Gcc(op1), Mod2Gcc(op2),
                                    GetMode(op1)=LeftValue,
                                    GetMode(op2)=LeftValue,
                                    IsConst(op1), IsConst(op2),
                                    BuildIsSuperset,
                                    falselabel) ;

      BuildGoto(string(CreateLabelName(op3))) ;
      DeclareLabel(falselabel)
   END
END CodeIfSetLess ;


(*
   CodeIfLess - codes the quadruple if op1 < op2 then goto op3
*)

PROCEDURE CodeIfLess (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   tl, tr: Tree ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, op1) ;
   DeclareConstant(CurrentQuadToken, op2) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      PushValue(op1) ;
      PushValue(op2) ;
      IF Less(CurrentQuadToken)
      THEN
         BuildGoto(string(CreateLabelName(op3)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(op1) OR (IsVar(op1) AND IsSet(SkipType(GetType(op1)))) OR
         IsConstSet(op2) OR (IsVar(op2) AND IsSet(SkipType(GetType(op2))))
   THEN
      CodeIfSetLess(quad, op1, op2, op3)
   ELSE
      IF IsComposite(GetType(op1)) OR IsComposite(GetType(op2))
      THEN
         MetaErrorT2(QuadToTokenNo(quad),
                     'comparison tests between are composite types not allowed {%1atd} and {%2atd}',
                     op1, op2)
      ELSE
         tl := SafeConvert(op1, op2) ;
         tr := SafeConvert(op2, op1) ;
         DoJump(BuildLessThan(tl, tr), NIL, string(CreateLabelName(op3)))
      END
   END
END CodeIfLess ;


(*
   CodeIfSetGre - 
*)

PROCEDURE CodeIfSetGre (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
BEGIN
   IF IsConst(op1) AND IsConst(op2)
   THEN
      InternalError('this should have been folded in the calling procedure',
                    __FILE__, __LINE__)
   ELSIF IsConst(op1)
   THEN
      settype := SkipType(GetType(op2))
   ELSE
      settype := SkipType(GetType(op1))
   END ;
   IF CompareTrees(FindSize(settype), FindSize(Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(BuildIsNotSubset(BuildConvert(GetWordType(), Mod2Gcc(op1), FALSE),
                              BuildConvert(GetWordType(), Mod2Gcc(op2), FALSE)),
             NIL, string(CreateLabelName(op3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(Mod2Gcc(settype),
                                    Mod2Gcc(op1), Mod2Gcc(op2),
                                    GetMode(op1)=LeftValue,
                                    GetMode(op2)=LeftValue,
                                    IsConst(op1), IsConst(op2),
                                    BuildIsSubset,
                                    falselabel) ;

      BuildGoto(string(CreateLabelName(op3))) ;
      DeclareLabel(falselabel)
   END
END CodeIfSetGre ;


(*
   CodeIfGre - codes the quadruple if op1 > op2 then goto op3
*)

PROCEDURE CodeIfGre (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   tl, tr: Tree ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, op1) ;
   DeclareConstant(CurrentQuadToken, op2) ;
   DeclareConstructor(quad, op1) ;
   DeclareConstructor(quad, op2) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      PushValue(op1) ;
      PushValue(op2) ;
      IF Gre(CurrentQuadToken)
      THEN
         BuildGoto(string(CreateLabelName(op3)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(op1) OR (IsVar(op1) AND IsSet(SkipType(GetType(op1)))) OR
         IsConstSet(op2) OR (IsVar(op2) AND IsSet(SkipType(GetType(op2))))
   THEN
      CodeIfSetGre(quad, op1, op2, op3)
   ELSE
      IF IsComposite(GetType(op1)) OR IsComposite(GetType(op2))
      THEN
         MetaErrorT2(QuadToTokenNo(quad),
                     'comparison tests between are composite types not allowed {%1atd} and {%2atd}',
                     op1, op2)
      ELSE
         tl := SafeConvert(op1, op2) ;
         tr := SafeConvert(op2, op1) ;
         DoJump(BuildGreaterThan(tl, tr), NIL, string(CreateLabelName(op3)))
      END
   END
END CodeIfGre ;


(*
   CodeIfSetLessEqu - 
*)

PROCEDURE CodeIfSetLessEqu (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
BEGIN
   IF IsConst(op1) AND IsConst(op2)
   THEN
      InternalError('this should have been folded in the calling procedure',
                    __FILE__, __LINE__)
   ELSIF IsConst(op1)
   THEN
      settype := SkipType(GetType(op2))
   ELSE
      settype := SkipType(GetType(op1))
   END ;
   IF CompareTrees(FindSize(settype), FindSize(Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(BuildIsSubset(BuildConvert(GetWordType(), Mod2Gcc(op1), FALSE),
                           BuildConvert(GetWordType(), Mod2Gcc(op2), FALSE)),
             NIL, string(CreateLabelName(op3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(Mod2Gcc(settype),
                                    Mod2Gcc(op1), Mod2Gcc(op2),
                                    GetMode(op1)=LeftValue,
                                    GetMode(op2)=LeftValue,
                                    IsConst(op1), IsConst(op2),
                                    BuildIsNotSubset,
                                    falselabel) ;

      BuildGoto(string(CreateLabelName(op3))) ;
      DeclareLabel(falselabel)
   END
END CodeIfSetLessEqu ;


(*
   CodeIfLessEqu - codes the quadruple if op1 <= op2 then goto op3
*)

PROCEDURE CodeIfLessEqu (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   tl, tr: Tree ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, op1) ;
   DeclareConstant(CurrentQuadToken, op2) ;
   DeclareConstructor(quad, op1) ;
   DeclareConstructor(quad, op2) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      PushValue(op1) ;
      PushValue(op2) ;
      IF LessEqu(CurrentQuadToken)
      THEN
         BuildGoto(string(CreateLabelName(op3)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(op1) OR (IsVar(op1) AND IsSet(SkipType(GetType(op1)))) OR
         IsConstSet(op2) OR (IsVar(op2) AND IsSet(SkipType(GetType(op2))))
   THEN
      CodeIfSetLessEqu(quad, op1, op2, op3)
   ELSE
      IF IsComposite(GetType(op1)) OR IsComposite(GetType(op2))
      THEN
         MetaErrorT2(QuadToTokenNo(quad),
                     'comparison tests between are composite types not allowed {%1atd} and {%2atd}',
                     op1, op2)
      ELSE
         tl := SafeConvert(op1, op2) ;
         tr := SafeConvert(op2, op1) ;
         DoJump(BuildLessThanOrEqual(tl, tr), NIL, string(CreateLabelName(op3)))
      END
   END
END CodeIfLessEqu ;


(*
   CodeIfSetGreEqu - 
*)

PROCEDURE CodeIfSetGreEqu (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
BEGIN
   IF IsConst(op1) AND IsConst(op2)
   THEN
      InternalError('this should have been folded in the calling procedure',
                    __FILE__, __LINE__)
   ELSIF IsConst(op1)
   THEN
      settype := SkipType(GetType(op2))
   ELSE
      settype := SkipType(GetType(op1))
   END ;
   IF CompareTrees(FindSize(settype), FindSize(Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(BuildIsSuperset(BuildConvert(GetWordType(), Mod2Gcc(op1), FALSE),
                             BuildConvert(GetWordType(), Mod2Gcc(op2), FALSE)),
             NIL, string(CreateLabelName(op3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(Mod2Gcc(settype),
                                    Mod2Gcc(op1), Mod2Gcc(op2),
                                    GetMode(op1)=LeftValue,
                                    GetMode(op2)=LeftValue,
                                    IsConst(op1), IsConst(op2),
                                    BuildIsNotSuperset,
                                    falselabel) ;

      BuildGoto(string(CreateLabelName(op3))) ;
      DeclareLabel(falselabel)
   END
END CodeIfSetGreEqu ;


(*
   CodeIfGreEqu - codes the quadruple if op1 >= op2 then goto op3
*)

PROCEDURE CodeIfGreEqu (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   tl, tr: Tree ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, op1) ;
   DeclareConstant(CurrentQuadToken, op2) ;
   DeclareConstructor(quad, op1) ;
   DeclareConstructor(quad, op2) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      PushValue(op1) ;
      PushValue(op2) ;
      IF GreEqu(CurrentQuadToken)
      THEN
         BuildGoto(string(CreateLabelName(op3)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(op1) OR (IsVar(op1) AND IsSet(SkipType(GetType(op1)))) OR
         IsConstSet(op2) OR (IsVar(op2) AND IsSet(SkipType(GetType(op2))))
   THEN
      CodeIfSetGreEqu(quad, op1, op2, op3)
   ELSE
      IF IsComposite(GetType(op1)) OR IsComposite(GetType(op2))
      THEN
         MetaErrorT2(QuadToTokenNo(quad),
                     'comparison tests between are composite types not allowed {%1atd} and {%2atd}',
                     op1, op2)
      ELSE
         tl := SafeConvert(op1, op2) ;
         tr := SafeConvert(op2, op1) ;
         DoJump(BuildGreaterThanOrEqual(tl, tr), NIL, string(CreateLabelName(op3)))
      END
   END
END CodeIfGreEqu ;


(*
   CodeIfSetEqu - codes if op1 = op2 then goto op3
                  Note that if op1 and op2 are not both constants
                  since this will have been evaluated in CodeIfEqu.
*)

PROCEDURE CodeIfSetEqu (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
BEGIN
   IF IsConst(op1) AND IsConst(op2)
   THEN
      InternalError('this should have been folded in the calling procedure',
                    __FILE__, __LINE__)
   ELSIF IsConst(op1)
   THEN
      settype := SkipType(GetType(op2))
   ELSE
      settype := SkipType(GetType(op1))
   END ;
   IF CompareTrees(FindSize(settype), FindSize(Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(BuildEqualTo(BuildConvert(GetWordType(), Mod2Gcc(op1), FALSE),
                          BuildConvert(GetWordType(), Mod2Gcc(op2), FALSE)),
             NIL, string(CreateLabelName(op3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;
      
      BuildForeachWordInSetDoIfExpr(Mod2Gcc(settype),
                                    Mod2Gcc(op1), Mod2Gcc(op2),
                                    GetMode(op1)=LeftValue,
                                    GetMode(op2)=LeftValue,
                                    IsConst(op1), IsConst(op2),
                                    BuildNotEqualTo,
                                    falselabel) ;

      BuildGoto(string(CreateLabelName(op3))) ;
      DeclareLabel(falselabel)
   END
END CodeIfSetEqu ;


(*
   CodeIfSetNotEqu - codes if op1 # op2 then goto op3
                     Note that if op1 and op2 are not both constants
                     since this will have been evaluated in CodeIfNotEqu.
*)

PROCEDURE CodeIfSetNotEqu (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t        : Tree ;
   settype  : CARDINAL ;
   truelabel: ADDRESS ;
BEGIN
   IF IsConst(op1) AND IsConst(op2)
   THEN
      InternalError('this should have been folded in the calling procedure', __FILE__, __LINE__)
   ELSIF IsConst(op1)
   THEN
      settype := SkipType(GetType(op2))
   ELSE
      settype := SkipType(GetType(op1))
   END ;
   IF CompareTrees(FindSize(settype), FindSize(Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(BuildNotEqualTo(BuildConvert(GetWordType(), Mod2Gcc(op1), FALSE),
                             BuildConvert(GetWordType(), Mod2Gcc(op2), FALSE)),
             NIL, string(CreateLabelName(op3)))
   ELSE
      truelabel := string(CreateLabelName(op3)) ;

      BuildForeachWordInSetDoIfExpr(Mod2Gcc(settype),
                                    Mod2Gcc(op1), Mod2Gcc(op2),
                                    GetMode(op1)=LeftValue,
                                    GetMode(op2)=LeftValue,
                                    IsConst(op1), IsConst(op2),
                                    BuildNotEqualTo,
                                    truelabel)
   END
END CodeIfSetNotEqu ;


(*
   CodeIfEqu - codes the quadruple if op1 = op2 then goto op3
*)

PROCEDURE CodeIfEqu (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   tl, tr: Tree ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, op1) ;
   DeclareConstant(CurrentQuadToken, op2) ;
   DeclareConstructor(quad, op1) ;
   DeclareConstructor(quad, op2) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      PushValue(op1) ;
      PushValue(op2) ;
      IF Equ(CurrentQuadToken)
      THEN
         BuildGoto(string(CreateLabelName(op3)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(op1) OR (IsVar(op1) AND IsSet(SkipType(GetType(op1)))) OR
         IsConstSet(op2) OR (IsVar(op2) AND IsSet(SkipType(GetType(op2))))
   THEN
      CodeIfSetEqu(quad, op1, op2, op3)
   ELSE
      IF IsComposite(GetType(op1)) OR IsComposite(GetType(op2))
      THEN
         MetaErrorT2(QuadToTokenNo(quad),
                     'equality tests between are composite types not allowed {%1atd} and {%2atd}',
                     op1, op2)
      ELSE
         tl := SafeConvert(op1, op2) ;
         tr := SafeConvert(op2, op1) ;
         DoJump(BuildEqualTo(tl, tr), NIL, string(CreateLabelName(op3)))
      END
   END
END CodeIfEqu ;


(*
   CodeIfNotEqu - codes the quadruple if op1 # op2 then goto op3
*)

PROCEDURE CodeIfNotEqu (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   tl, tr: Tree ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, op1) ;
   DeclareConstant(CurrentQuadToken, op2) ;
   DeclareConstructor(quad, op1) ;
   DeclareConstructor(quad, op2) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      PushValue(op1) ;
      PushValue(op2) ;
      IF NotEqu(CurrentQuadToken)
      THEN
         BuildGoto(string(CreateLabelName(op3)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(op1) OR (IsVar(op1) AND IsSet(SkipType(GetType(op1)))) OR
         IsConstSet(op2) OR (IsVar(op2) AND IsSet(SkipType(GetType(op2))))
   THEN
      CodeIfSetNotEqu(quad, op1, op2, op3)
   ELSE
      IF IsComposite(op1) OR IsComposite(op2)
      THEN
         MetaErrorT2(QuadToTokenNo(quad),
                     'inequality tests between are composite types not allowed {%1atd} and {%2atd}',
                     op1, op2)
      ELSE
         tl := SafeConvert(op1, op2) ;
         tr := SafeConvert(op2, op1) ;
         DoJump(BuildNotEqualTo(tl, tr), NIL, string(CreateLabelName(op3)))
      END
   END
END CodeIfNotEqu ;


(*
   BuildIfVarInConstValue - if var in constsetvalue then goto trueexit
*)

PROCEDURE BuildIfVarInConstValue (constsetvalue: PtrToValue; var, trueexit: CARDINAL) ;
VAR
   low, high, n: CARDINAL ;
   truelabel   : String ;
BEGIN
   n := 1 ;
   truelabel := string(CreateLabelName(trueexit)) ;
   WHILE GetRange(constsetvalue, n, low, high) DO
      BuildIfInRangeGoto(Mod2Gcc(var), Mod2Gcc(low), Mod2Gcc(high), truelabel) ;
      INC(n)
   END
END BuildIfVarInConstValue ;


(*
   BuildIfNotVarInConstValue - if not (var in constsetvalue) then goto trueexit
*)

PROCEDURE BuildIfNotVarInConstValue (quad: CARDINAL; constsetvalue: PtrToValue; var, trueexit: CARDINAL) ;
VAR
   low, high, n: CARDINAL ;
   falselabel,
   truelabel   : String ;
BEGIN
   truelabel := string(CreateLabelName(trueexit)) ;
   n := 1 ;
   WHILE GetRange(constsetvalue, n, low, high) DO
      INC(n)
   END ;
   IF n=2
   THEN
      (* actually only one set range, so we invert it *)
      BuildIfNotInRangeGoto(Mod2Gcc(var), Mod2Gcc(low), Mod2Gcc(high), truelabel) ;
   ELSE
      n := 1 ;
      falselabel := string(Sprintf1(Mark(InitString('.Lset%d')), quad)) ;
      WHILE GetRange(constsetvalue, n, low, high) DO
         BuildIfInRangeGoto(Mod2Gcc(var), Mod2Gcc(low), Mod2Gcc(high), falselabel) ;
         INC(n)
      END ;
      BuildGoto(truelabel) ;
      DeclareLabel(falselabel)
   END
END BuildIfNotVarInConstValue ;


(*
   CodeIfIn - code the quadruple: if op1 in op2 then goto op3
*)

PROCEDURE CodeIfIn (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   s       : String ;
   low,
   high    : CARDINAL ;
   lowtree,
   hightree,
   offset  : Tree ;
   fieldno : INTEGER ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, op1) ;
   DeclareConstant(CurrentQuadToken, op2) ;
   DeclareConstructor(quad, op1) ;
   DeclareConstructor(quad, op2) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      InternalError('should not get to here (if we do we should consider calling FoldIfIn)', __FILE__, __LINE__)
   ELSE
      IF IsConst(op1)
      THEN
         fieldno := GetFieldNo(CurrentQuadToken, op1, GetType(op2), offset) ;
         IF fieldno>=0
         THEN
            PushValue(op1) ;
            PushIntegerTree(offset) ;
            Sub ;
            BuildIfConstInVar(Mod2Gcc(SkipType(GetType(op2))),
                              Mod2Gcc(op2), PopIntegerTree(),
                              GetMode(op2)=LeftValue, fieldno,
                              string(CreateLabelName(op3)))
         ELSE
            MetaErrorT1(CurrentQuadToken, 'bit exceeded the range of set {%1atd}', op1)
         END
      ELSIF IsConst(op2)
      THEN
         (* builds a cascaded list of if statements *)
         PushValue(op2) ;
         BuildIfVarInConstValue(GetValue(CurrentQuadToken), op1, op3)
      ELSE
         GetSetLimits(SkipType(GetType(op2)), low, high) ;

         PushValue(low) ;
         lowtree := PopIntegerTree() ;
         PushValue(high) ;
         hightree := PopIntegerTree() ;

         BuildIfVarInVar(Mod2Gcc(SkipType(GetType(op2))),
                         Mod2Gcc(op2), Mod2Gcc(op1),
                         GetMode(op2)=LeftValue,
                         lowtree, hightree,
                         string(CreateLabelName(op3)))
      END
   END
END CodeIfIn ;


(*
   CodeIfNotIn - code the quadruple: if not (op1 in op2) then goto op3
*)

PROCEDURE CodeIfNotIn (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   s       : String ;
   operator: QuadOperator ;
   low,
   high    : CARDINAL ;
   lowtree,
   hightree,
   offset  : Tree ;
   fieldno : INTEGER ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, op1) ;
   DeclareConstant(CurrentQuadToken, op2) ;
   DeclareConstructor(quad, op1) ;
   DeclareConstructor(quad, op2) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      InternalError('should not get to here (if we do we should consider calling FoldIfIn)', __FILE__, __LINE__)
   ELSE
      IF IsConst(op1)
      THEN
         fieldno := GetFieldNo(CurrentQuadToken, op1, SkipType(GetType(op2)), offset) ;
         IF fieldno>=0
         THEN
            PushValue(op1) ;
            PushIntegerTree(offset) ;
            Sub ;
            BuildIfNotConstInVar(Mod2Gcc(SkipType(GetType(op2))),
                                 Mod2Gcc(op2), PopIntegerTree(),
                                 GetMode(op2)=LeftValue, fieldno,
                                 string(CreateLabelName(op3)))
         ELSE
            MetaErrorT1(CurrentQuadToken, 'bit exceeded the range of set {%1atd}', op2)
         END
      ELSIF IsConst(op2)
      THEN
         (* builds a cascaded list of if statements *)
         PushValue(op2) ;
         BuildIfNotVarInConstValue(quad, GetValue(CurrentQuadToken), op1, op3)
      ELSE
         GetSetLimits(SkipType(GetType(op2)), low, high) ;

         PushValue(low) ;
         lowtree := PopIntegerTree() ;
         PushValue(high) ;
         hightree := PopIntegerTree() ;

         BuildIfNotVarInVar(Mod2Gcc(SkipType(GetType(op2))),
                            Mod2Gcc(op2), Mod2Gcc(op1),
                            GetMode(op2)=LeftValue,
                            lowtree, hightree,
                            string(CreateLabelName(op3)))
      END
   END
END CodeIfNotIn ;


(*
------------------------------------------------------------------------------
   IndrX Operator           a = *b
------------------------------------------------------------------------------
   Sym1<X>   IndrX   Sym2<I>     Meaning     Mem[Sym1<I>] := Mem[constant]
   Sym1<X>   IndrX   Sym2<X>     Meaning     Mem[Sym1<I>] := Mem[Mem[Sym3<I>]]

   (op2 is the type of the data being indirectly copied)
*)

PROCEDURE CodeIndrX (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t: Tree ;
BEGIN
   (*
      Follow the Quadruple rules:
   *)
   DeclareConstant(CurrentQuadToken, op3) ;  (* checks to see whether it is a constant and declares it *)
   DeclareConstructor(quad, op3) ;
   IF IsConstString(op3)
   THEN
      InternalError('not expecting to index through a constant string', __FILE__, __LINE__)
   ELSE
      (*
         Mem[op1] := Mem[Mem[op3]]
      *)
      t := BuildAssignmentTree(Mod2Gcc(op1), BuildIndirect(Mod2Gcc(op3), Mod2Gcc(op2)))
   END
END CodeIndrX ;


(*
------------------------------------------------------------------------------
   XIndr Operator           *a = b
------------------------------------------------------------------------------
   Sym1<I>   XIndr   Sym2<X>     Meaning     Mem[constant]     := Mem[Sym3<I>]
   Sym1<X>   XIndr   Sym2<X>     Meaning     Mem[Mem[Sym1<I>]] := Mem[Sym3<I>]

   (op2 is the type of the data being indirectly copied)
*)

PROCEDURE CodeXIndr (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   op3t, t: Tree ;
BEGIN
   DeclareConstant(CurrentQuadToken, op3) ;
   DeclareConstructor(quad, op3) ;
   (*
      Follow the Quadruple rule:

      Mem[Mem[Op1]] := Mem[Op3]
   *)
   IF IsProcType(SkipType(op2))
   THEN
      t := BuildAssignmentTree(BuildIndirect(Mod2Gcc(op1), GetPointerType()), Mod2Gcc(op3))
   ELSIF IsConstString(op3) AND (GetStringLength(op3)=0) AND (GetMode(op1)=LeftValue)
   THEN
      (*
         no need to check for type errors,
         but we handle nul string as a special case as back end
         complains if we pass through a "" and ask it to copy the
         contents.
      *)
      t := BuildAssignmentTree(BuildIndirect(LValueToGenericPtr(op1), Mod2Gcc(Char)),
                               StringToChar(Mod2Gcc(op3), Char, op3))
   ELSIF IsConstString(op3) AND (SkipTypeAndSubrange(GetType(op1))#Char)
   THEN
      DoCopyString(t, op3t, op2, op3) ;
      AddStatement(BuiltInMemCopy(Mod2Gcc(op1), BuildAddr(op3t, FALSE), t))
   ELSE
      t := BuildAssignmentTree(BuildIndirect(Mod2Gcc(op1), Mod2Gcc(op2)),
                               StringToChar(Mod2Gcc(op3), op2, op3))
   END
END CodeXIndr ;


BEGIN
   ModuleName := NIL ;
   FileName := NIL ;
   UnboundedLabelNo := 0 ;
   CompilingMainModuleStack := InitStackWord() ;
   ScopeStack := InitStackWord()
END M2GenGCC.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I.:../gm2-libs:../gm2-libs-ch:../gm2-libiberty/ M2GenGCC.mod"
 * End:
 *)
