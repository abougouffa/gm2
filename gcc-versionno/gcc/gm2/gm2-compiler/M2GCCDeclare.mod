(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                 2010, 2011, 2012
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

IMPLEMENTATION MODULE M2GCCDeclare ;

(*
    Title      : M2GCCDeclare
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Fri Jul 16 20:10:55 1999
    Description: declares Modula-2 types to GCC, it attempts
                 to only declare a type once all subcomponents are known.
*)

FROM SYSTEM IMPORT ADDRESS, ADR, WORD ;
FROM ASCII IMPORT nul ;
FROM M2Debug IMPORT Assert ;
FROM M2Quads IMPORT DisplayQuadRange, QuadToTokenNo ;

IMPORT FIO ;

FROM M2Options IMPORT DisplayQuadruples,
                      GenerateDebugging, GenerateLineDebug, Iso, Optimizing ;

FROM M2AsmUtil IMPORT WriteAsmName, WriteName, GetAsmName, GetFullSymName,
                      UnderScoreString, GetModuleInitName, GetModuleFinallyName,
                      GetFullScopeAsmName ;

FROM NameKey IMPORT Name, MakeKey, NulName, KeyToCharStar, makekey ;
FROM M2FileName IMPORT CalculateFileName ;
FROM M2Configure IMPORT PushParametersLeftToRight ;
FROM DynamicStrings IMPORT String, string, InitString, KillString, InitStringCharStar, Mark ;
FROM FormatStrings IMPORT Sprintf1 ;
FROM M2LexBuf IMPORT TokenToLineNo, FindFileNameFromToken ;
FROM M2MetaError IMPORT MetaError1 ;
FROM M2Error IMPORT FlushErrors, InternalError ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3 ;

FROM Indexing IMPORT Index, InitIndex, PutIndice, GetIndice, InBounds,
                     DebugIndex ;

FROM Lists IMPORT List, InitList, IncludeItemIntoList,
                  PutItemIntoList, GetItemFromList,
                  RemoveItemFromList, ForeachItemInListDo,
      	       	  IsItemInList, NoOfItemsInList, KillList ;

FROM Sets IMPORT Set, InitSet, KillSet,
                 IncludeElementIntoSet, ExcludeElementFromSet,
                 NoOfElementsInSet, IsElementInSet, ForeachElementInSetDo ;

FROM SymbolTable IMPORT NulSym,
                        ModeOfAddr,
                        GetMode,
                        GetScope,
                        GetNth, GetType, SkipType, GetVarBackEndType,
                        MakeType, PutType, MakeConstLit,
      	       	     	GetSubrange, PutSubrange, GetArraySubscript,
      	       	     	NoOfParam, GetNthParam,
                        PushValue, PopValue, PopSize,
                        IsTemporary, IsUnbounded, IsPartialUnbounded,
                        IsEnumeration, IsVar,
      	       	     	IsSubrange, IsPointer, IsRecord, IsArray,
                        IsFieldEnumeration,
                        IsProcedure, IsProcedureNested, IsModule,
                        IsDefImp,
      	       	     	IsSubscript, IsVarient, IsFieldVarient,
      	       	     	IsType, IsProcType, IsSet,
                        IsConst, IsConstSet, IsConstructor,
                        IsFieldEnumeration,
                        IsExported, IsImported,
                        IsVarParam, IsRecordField, IsUnboundedParam,
                        IsValueSolved,
                        IsDefinitionForC, IsHiddenTypeDeclared,
                        IsInnerModule, IsUnknown,
                        IsProcedureReachable, IsParameter, IsConstLit,
                        IsDummy, IsVarAParam, IsProcedureVariable,
                        IsGnuAsm, IsGnuAsmVolatile, IsObject, IsTuple,
                        IsError, IsHiddenType,
                        IsDefinitionForC, IsHiddenTypeDeclared,
                        IsComponent,
      	       	     	GetMainModule, GetBaseModule, GetModule,
                        PutModuleFinallyFunction,
                        GetProcedureScope, GetProcedureQuads,
                        IsRecordFieldAVarientTag, IsEmptyFieldVarient,
                        GetVarient, GetUnbounded,
                        IsAModula2Type, UsesVarArgs,
                        GetSymName, GetParent,
                        GetDeclared, GetVarBackEndType,
                        GetString, GetStringLength, IsConstString,
                        GetAlignment, IsDeclaredPacked, PutDeclaredPacked,
                        GetDefaultRecordFieldAlignment, IsDeclaredPackedResolved,
                        GetPackedEquivalent, 
                        GetParameterShadowVar,
                        GetUnboundedRecordType,
                        ForeachOAFamily, GetOAFamily,
                        IsModuleWithinProcedure,
                        IsVariableAtAddress, IsConstructorConstant,
                        ForeachLocalSymDo, ForeachFieldEnumerationDo,
      	       	     	ForeachProcedureDo, ForeachModuleDo,
                        ForeachInnerModuleDo, ForeachImportedDo,
                        ForeachExportedDo ;

FROM M2Base IMPORT IsPseudoBaseProcedure, IsPseudoBaseFunction,
                   GetBaseTypeMinMax, MixTypes,
                   Cardinal, Char, Proc, Integer,
                   LongInt, LongCard, ShortCard, ShortInt,
                   Real, LongReal, ShortReal, ZType, RType,
                   CType, Complex, LongComplex, ShortComplex,
                   Boolean, True, False, Nil,
                   IsRealType, IsNeededAtRunTime, IsAComplexType ;

FROM M2System IMPORT IsPseudoSystemFunction, IsSystemType,
                     GetSystemTypeMinMax, Address, Word, Byte, Loc,
                     System, IntegerN, CardinalN, WordN, RealN, SetN, ComplexN ;

FROM M2Bitset IMPORT Bitset, Bitnum ;
FROM SymbolConversion IMPORT AddModGcc, Mod2Gcc, GccKnowsAbout, Poison, RemoveMod2Gcc ;
FROM M2GenGCC IMPORT ResolveConstantExpressions ;
FROM M2Scope IMPORT ScopeBlock, InitScopeBlock, KillScopeBlock, ForeachScopeBlockDo ;

FROM M2ALU IMPORT Addn, Sub, Equ, GreEqu, Gre, Less, PushInt, PushCard,
                  PushIntegerTree, PopIntegerTree, PopRealTree, ConvertToInt, PopSetTree,
                  IsConstructorDependants, WalkConstructorDependants,
                  PopConstructorTree, PopComplexTree, PutConstructorSolved,
                  ChangeToConstructor, EvaluateValue, TryEvaluateValue ;

FROM gccgm2 IMPORT Tree, Constructor,
                   SetFileNameAndLineNo,
                   DeclareKnownType, DeclareKnownVariable,
                   GetDefaultType, GetCopyOfType,
                   GetIntegerType, GetCharType, GetM2CharType, GetM2ZType, GetM2RType,
                   GetVoidType, GetIntegerZero, GetIntegerOne, GetCurrentFunction,
                   GetPointerType, GetM2LongIntType, GetM2LongCardType,
                   GetM2ShortIntType, GetM2ShortCardType,
                   GetM2RealType, GetM2ShortRealType, GetM2LongRealType,
                   GetProcType, GetCardinalType, GetWordType, GetByteType,
                   GetBitsetType, GetBitnumType, GetMinFrom, GetMaxFrom,
                   GetBitsPerInt, GetBitsPerBitset,
                   GetM2IntegerType, GetM2CardinalType, GetPointerZero,
                   GetISOLocType, GetISOByteType, GetISOWordType,
                   BuildStartEnumeration, BuildEndEnumeration, BuildEnumerator,
                   BuildIntegerConstant, BuildStringConstant, BuildCharConstant,
                   BuildSubrangeType,
                   BuildStartRecord, BuildEndRecord,
                   BuildStartVarient, BuildEndVarient,
                   BuildStartFieldVarient, BuildEndFieldVarient,
                   BuildFieldRecord,
                   BuildArrayIndexType, BuildStartArrayType, BuildEndArrayType, BuildSetType,
                   BuildAdd, BuildSub, BuildLSL, BuildDivTrunc, BuildModTrunc,
                   DebugTree, GetDeclContext,
                   ChainOn,
                   BuildPointerType, BuildConstPointerType, BuildConvert, SetAlignment,
                   SetDeclPacked, SetTypePacked, SetRecordFieldOffset, BuildSmallestTypeRange,
                   BuildStartFunctionType, BuildEndFunctionType,
                   InitFunctionTypeParameters,
                   BuildParameterDeclaration,
                   BuildStartFunctionDeclaration, BuildEndFunctionDeclaration,
                   BuildStartMainModule, BuildEndMainModule,
                   BuildStartType, BuildEndType, PutArrayType,
                   RememberType, BuildTypeDeclaration, RememberConstant,
                   GetBooleanType, GetBooleanFalse, GetBooleanTrue,
                   GetPackedBooleanType,
                   BuildSize, MarkFunctionReferenced, BuildTBitSize,
                   GetM2Integer8, GetM2Integer16, GetM2Integer32, GetM2Integer64,
                   GetM2Cardinal8, GetM2Cardinal16, GetM2Cardinal32, GetM2Cardinal64,
                   GetM2Bitset8, GetM2Bitset16, GetM2Bitset32,
                   GetM2Word16, GetM2Word32, GetM2Word64,
                   GetM2Real32, GetM2Real64, GetM2Real96, GetM2Real128,
                   GetM2Complex32, GetM2Complex64, GetM2Complex96, GetM2Complex128,
                   GetM2ComplexType, GetM2LongComplexType, GetM2ShortComplexType,
                   GetM2CType ;

TYPE
   StartProcedure = PROCEDURE (ADDRESS) : Tree ;
   ListType       = (fullydeclared, partiallydeclared, niltypedarrays,
                     heldbyalignment, finishedalignment, todolist, tobesolvedbyquads) ;
   doDeclareProcedure = PROCEDURE (CARDINAL, CARDINAL) ;


(* %%%FORWARD%%%
PROCEDURE WalkComponentDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE IsVarDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE DeclareFieldVarient (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE WalkProcedureDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE IsProcedureDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE DeclareRecordField (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE WriteRule ; FORWARD ;
PROCEDURE RecordNotPacked (sym: CARDINAL) ; FORWARD ;
PROCEDURE MaybeAlignField (field: CARDINAL; VAR byteOffset, bitOffset: Tree) : Tree ; FORWARD ;
PROCEDURE WalkRecordDependants2 (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE DetermineIfRecordPacked (sym: CARDINAL) ; FORWARD ;
PROCEDURE DeclareProcedureToGcc (Sym: CARDINAL) ; FORWARD ;
PROCEDURE AlignDeclarationWithSource (sym: CARDINAL) ; FORWARD ;
PROCEDURE PrintTerse (sym: CARDINAL) ; FORWARD ;
PROCEDURE DeclareFileName ; FORWARD ;
PROCEDURE DeclareImportedVariables (sym: WORD) ; FORWARD ;
PROCEDURE DeclareSet (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclarePointer (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclareLocalVariables (sym: CARDINAL; i: CARDINAL) ; FORWARD ;
PROCEDURE DeclareDefaultTypes ; FORWARD ;
PROCEDURE DeclareGlobalVariables (ModSym: WORD) ; FORWARD ;
PROCEDURE DeclareModuleVariables (sym: CARDINAL) ; FORWARD ;
PROCEDURE DeclareType (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE IsUnboundedDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE IsEnumerationDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE IsSubrangeDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE IsPointerDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE IsRecordDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE IsRecordFieldDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE IsVarientDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE IsArrayDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE IsSetDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE IsTypeDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE IsProcTypeDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE DeclareEnumeration (sym: WORD) : Tree ; FORWARD ;
PROCEDURE AllDependantsFullyDeclared (sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE DeclareVarient (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE PostDeclareVarient (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE IsEffectivelyImported (ModSym, sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE DeclareUnboundedProcedureParameters (sym: WORD) ; FORWARD ;
PROCEDURE PreAddModGcc (sym: CARDINAL; t: Tree) ; FORWARD ;
PROCEDURE DeclareAssociatedUnbounded (sym: CARDINAL) ; FORWARD ;
PROCEDURE DeclareUnbounded (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclareTypesConstantsProceduresInRange (start, end: CARDINAL) ; FORWARD ;
PROCEDURE IsTypeQ (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE IsFullyDeclared (sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE TypeConstFullyDeclared (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE CompleteDeclarationOf (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE TraverseDependants (sym: WORD) ; FORWARD ;
PROCEDURE WalkEnumerationDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE WalkSubrangeDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE IsSubrangeDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE WalkPointerDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE IsPointerDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE WalkRecordDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE WalkVarientDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE WalkArrayDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE WalkSetDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE WalkProcTypeDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE WalkUnboundedDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE WalkTypeDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE DeclareConst (tokenno: CARDINAL; sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE IsConstDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE WalkDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE DeclareRecord (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclareProcType (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclareTypeConstFully (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclareArray (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE WalkVarDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE BuildIndex (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE IsPartiallyOrFullyDeclared (sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsNilTypedArrays (sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE WalkRecordFieldDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE IsVarientFieldDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE WalkVarientFieldDependants (sym: CARDINAL; p: WalkAction) ; FORWARD ;
PROCEDURE TryDeclareConst (tokenno: CARDINAL; sym: CARDINAL) ; FORWARD ;
PROCEDURE PossiblyPacked (sym: CARDINAL; isPacked: BOOLEAN) : Tree ; FORWARD ;
   %%%FORWARD%%% *)

CONST
   Debugging = FALSE ;
   Progress  = FALSE ;

VAR
   ToBeSolvedByQuads,               (* constants which must be solved *)
                                    (* by processing the quadruples.  *)
   NilTypedArrays,                  (* arrays which have NIL as their *)
                                    (* type.                          *)
   FullyDeclared,                   (* those symbols which have been  *)
                                    (* fully declared.                *)
   PartiallyDeclared,               (* those types which have need to *)
                                    (* be finished (but already       *)
                                    (* started: records, function,    *)
                                    (* and array type).               *)
   HeldByAlignment,                 (* types which have a user        *)
                                    (* specified alignment constant.  *)
   FinishedAlignment,               (* records for which we know      *)
                                    (* their alignment value.         *)
   VisitedList,
   ChainedList,
   ToDoList            : Set ;      (* Contains a set of all          *)
                                    (* outstanding types that need to *)
                                    (* be declared to GCC once        *)
                                    (* its dependants have            *)
                                    (* been written.                  *)
   AnotherType         : CARDINAL ; (* The number of AnotherTypes     *)
                                    (* that have been produced.       *)
   HaveInitDefaultTypes: BOOLEAN ;  (* have we initialized them yet?  *)
   WatchList           : Set ;      (* Set of symbols being watched   *)
   EnumerationIndex    : Index ;
   action              : IsAction ;
   enumDeps            : BOOLEAN ;


PROCEDURE mystop ; BEGIN END mystop ;


(*
   PrintSym - 
*)

PROCEDURE PrintSym (sym: WORD) ;
BEGIN
   printf1('%d, ', sym)
END PrintSym ;


(*
   DebugSet - 
*)

PROCEDURE DebugSet (a: ARRAY OF CHAR; l: Set) ;
BEGIN
   printf0(a) ;
   printf0(' {') ;
   ForeachElementInSetDo(l, PrintSym) ;
   printf0('}\n')
END DebugSet ;


(*
   DebugSets - 
*)

PROCEDURE DebugSets ;
BEGIN
   DebugSet('ToDoList', ToDoList) ;
   DebugSet('HeldByAlignment', HeldByAlignment) ;
   DebugSet('FinishedAlignment', FinishedAlignment) ;
   DebugSet('PartiallyDeclared', PartiallyDeclared) ;
   DebugSet('FullyDeclared', FullyDeclared) ;
   DebugSet('NilTypedArrays', NilTypedArrays) ;
   DebugSet('ToBeSolvedByQuads', ToBeSolvedByQuads)
END DebugSets ;


(*
   DebugNumber - 
*)

PROCEDURE DebugNumber (a: ARRAY OF CHAR; s: Set) ;
VAR
   n: CARDINAL ;
BEGIN
   n := NoOfElementsInSet(s) ;
   printf1(a, n) ;
   FIO.FlushBuffer(FIO.StdOut)
END DebugNumber ;


(*
   FindSetNumbers - 
*)

PROCEDURE FindSetNumbers (VAR t, a, p, f, n, b: CARDINAL) : BOOLEAN ;
VAR
   t1, p1, f1, n1, b1, a1: CARDINAL ;
   same                  : BOOLEAN ;
BEGIN
   t1 := NoOfElementsInSet(ToDoList) ;
   a1 := NoOfElementsInSet(HeldByAlignment) ;
   p1 := NoOfElementsInSet(PartiallyDeclared) ;
   f1 := NoOfElementsInSet(FullyDeclared) ;
   n1 := NoOfElementsInSet(NilTypedArrays) ;
   b1 := NoOfElementsInSet(ToBeSolvedByQuads) ;
   same := ((t=t1) AND (a=a1) AND (p=p1) AND (f=f1) AND (n=n1) AND (b=b1)) ;
   t := t1 ;
   a := a1 ;
   p := p1 ;
   f := f1 ;
   n := n1 ;
   b := b1 ;
   RETURN( same )
END FindSetNumbers ;


(*
   DebugSets - 
*)

PROCEDURE DebugSetNumbers ;
BEGIN
   DebugNumber('ToDoList : %d\n', ToDoList) ;
   DebugNumber('HeldByAlignment : %d\n', HeldByAlignment) ;
   DebugNumber('PartiallyDeclared : %d\n', PartiallyDeclared) ;
   DebugNumber('FullyDeclared : %d\n', FullyDeclared) ;
   DebugNumber('NilTypedArrays : %d\n', NilTypedArrays) ;
   DebugNumber('ToBeSolvedByQuads : %d\n', ToBeSolvedByQuads)
END DebugSetNumbers ;


(*
   AddSymToWatch - adds symbol, sym, to the list of symbols
                   to watch and annotate their movement between
                   lists.
*)

PROCEDURE AddSymToWatch (sym: WORD) ;
BEGIN
   IF NOT IsElementInSet(WatchList, sym)
   THEN
      IncludeElementIntoSet(WatchList, sym) ;
      WalkDependants(sym, AddSymToWatch) ;
      printf1("watching symbol %d\n", sym) ;
      FIO.FlushBuffer(FIO.StdOut)
   END
END AddSymToWatch ;


(*
   doInclude - 
*)

PROCEDURE doInclude (l: Set; a: ARRAY OF CHAR; sym: CARDINAL) ;
BEGIN
   IF NOT IsElementInSet(l, sym)
   THEN
      printf0('rule: ') ;
      WriteRule ;
      printf0('  ') ;
      printf1(a, sym) ;
      FIO.FlushBuffer(FIO.StdOut) ;
      IncludeElementIntoSet(l, sym)
   END
END doInclude ;


(*
   WatchIncludeList - include a symbol onto the set first checking
                      whether it is already on the set and
                      displaying a debug message if the set is
                      changed.
*)

PROCEDURE WatchIncludeList (sym: CARDINAL; lt: ListType) ;
BEGIN
   IF IsElementInSet(WatchList, sym)
   THEN
      CASE lt OF

      tobesolvedbyquads :  doInclude(ToBeSolvedByQuads, "symbol %d -> ToBeSolvedByQuads\n", sym) |
      fullydeclared     :  doInclude(FullyDeclared, "symbol %d -> FullyDeclared\n", sym) |
      partiallydeclared :  doInclude(PartiallyDeclared, "symbol %d -> PartiallyDeclared\n", sym) |
      heldbyalignment   :  doInclude(HeldByAlignment, "symbol %d -> HeldByAlignment\n", sym) |
      finishedalignment :  doInclude(FinishedAlignment, "symbol %d -> FinishedAlignment\n", sym) |
      todolist          :  doInclude(ToDoList, "symbol %d -> ToDoList\n", sym) |
      niltypedarrays    :  doInclude(NilTypedArrays, "symbol %d -> NilTypedArrays\n", sym)

      ELSE
         InternalError('unknown list', __FILE__, __LINE__)
      END
   ELSE
      CASE lt OF

      tobesolvedbyquads :  IncludeElementIntoSet(ToBeSolvedByQuads, sym) |
      fullydeclared     :  IncludeElementIntoSet(FullyDeclared, sym) |
      partiallydeclared :  IncludeElementIntoSet(PartiallyDeclared, sym) |
      heldbyalignment   :  IncludeElementIntoSet(HeldByAlignment, sym) |
      finishedalignment :  IncludeElementIntoSet(FinishedAlignment, sym) |
      todolist          :  IncludeElementIntoSet(ToDoList, sym) |
      niltypedarrays    :  IncludeElementIntoSet(NilTypedArrays, sym)

      ELSE
         InternalError('unknown list', __FILE__, __LINE__)
      END
   END
END WatchIncludeList ;


(*
   doExclude - 
*)

PROCEDURE doExclude (l: Set; a: ARRAY OF CHAR; sym: CARDINAL) ;
BEGIN
   IF IsElementInSet(l, sym)
   THEN
      printf0('rule: ') ;
      WriteRule ;
      printf0('  ') ;
      printf1(a, sym) ;
      FIO.FlushBuffer(FIO.StdOut) ;
      ExcludeElementFromSet(l, sym)
   END
END doExclude ;


(*
   WatchRemoveList - remove a symbol onto the list first checking
                     whether it is already on the list and
                     displaying a debug message if the list is
                     changed.
*)

PROCEDURE WatchRemoveList (sym: CARDINAL; lt: ListType) ;
BEGIN
   IF IsElementInSet(WatchList, sym)
   THEN
      CASE lt OF

      tobesolvedbyquads :  doExclude(ToBeSolvedByQuads, "symbol %d off ToBeSolvedByQuads\n", sym) |
      fullydeclared     :  doExclude(FullyDeclared, "symbol %d off FullyDeclared\n", sym) |
      partiallydeclared :  doExclude(PartiallyDeclared, "symbol %d off PartiallyDeclared\n", sym) |
      heldbyalignment   :  doExclude(HeldByAlignment, "symbol %d -> HeldByAlignment\n", sym) |
      finishedalignment :  doExclude(FinishedAlignment, "symbol %d -> FinishedAlignment\n", sym) |
      todolist          :  doExclude(ToDoList, "symbol %d off ToDoList\n", sym) ;
                           IF sym=865
                           THEN
                              mystop
                           END |
      niltypedarrays    :  doExclude(NilTypedArrays, "symbol %d off NilTypedArrays\n", sym)

      ELSE
         InternalError('unknown list', __FILE__, __LINE__)
      END
   ELSE
      CASE lt OF

      tobesolvedbyquads :  ExcludeElementFromSet(ToBeSolvedByQuads, sym) |
      fullydeclared     :  ExcludeElementFromSet(FullyDeclared, sym) |
      partiallydeclared :  ExcludeElementFromSet(PartiallyDeclared, sym) |
      heldbyalignment   :  ExcludeElementFromSet(HeldByAlignment, sym) |
      finishedalignment :  ExcludeElementFromSet(FinishedAlignment, sym) |
      todolist          :  ExcludeElementFromSet(ToDoList, sym) |
      niltypedarrays    :  ExcludeElementFromSet(NilTypedArrays, sym)

      ELSE
         InternalError('unknown list', __FILE__, __LINE__)
      END
   END
END WatchRemoveList ;


(*
   GetEnumList - 
*)

PROCEDURE GetEnumList (sym: CARDINAL) : Tree ;
BEGIN
   IF InBounds(EnumerationIndex, sym)
   THEN
      RETURN( GetIndice(EnumerationIndex, sym) )
   ELSE
      RETURN( NIL )
   END
END GetEnumList ;


(*
   PutEnumList - 
*)

PROCEDURE PutEnumList (sym: CARDINAL; enumlist: Tree) ;
BEGIN
   PutIndice(EnumerationIndex, sym, enumlist)
END PutEnumList ;


(*
   MarkExported - tell GCC to mark all exported procedures in module sym.
*)

PROCEDURE MarkExported (sym: CARDINAL) ;
BEGIN
   IF Optimizing
   THEN
      MarkFunctionReferenced(Mod2Gcc(sym)) ;
      IF IsDefImp(sym) OR IsModule(sym)
      THEN
         ForeachExportedDo(sym, MarkExported)
      END
   END
END MarkExported ;


(*
   Chained - checks to see that, sym, has not already been placed on a chain.
             It returns the symbol, sym.
*)

PROCEDURE Chained (sym: CARDINAL) : CARDINAL ;
BEGIN
   IF IsElementInSet(ChainedList, sym)
   THEN
      InternalError('symbol has already been chained onto a previous list',
                    __FILE__, __LINE__)
   END ;
   IncludeElementIntoSet(ChainedList, sym) ;
   RETURN( sym )
END Chained ;


(*
   DoStartDeclaration - returns a tree representing a symbol which has
                        not yet been finished. (Useful when declaring
                        recursive types).
*)

PROCEDURE DoStartDeclaration (sym: CARDINAL; p: StartProcedure) : Tree ;
BEGIN
   IF NOT GccKnowsAbout(sym)
   THEN
      PreAddModGcc(sym, p(KeyToCharStar(GetFullSymName(sym))))
   END ;
   RETURN( Mod2Gcc(sym) )
END DoStartDeclaration ;


(*
   ArrayComponentsDeclared - returns TRUE if array, sym,
                             subscripts and type are known.
*)

PROCEDURE ArrayComponentsDeclared (sym: CARDINAL) : BOOLEAN ;
VAR
   Subscript      : CARDINAL ;
   Type, High, Low: CARDINAL ;
BEGIN
   Subscript := GetArraySubscript(sym) ;
   Assert(IsSubscript(Subscript)) ;
   Type := SkipType(GetType(Subscript)) ;
   Low := GetTypeMin(Type) ;
   High := GetTypeMax(Type) ;
   RETURN( IsFullyDeclared(Type) AND
           IsFullyDeclared(Low) AND
           IsFullyDeclared(High) )
END ArrayComponentsDeclared ;


(*
   GetRecordOfVarient - 
*)

PROCEDURE GetRecordOfVarient (sym: CARDINAL) : CARDINAL ;
BEGIN
   IF IsVarient(sym) OR IsFieldVarient(sym)
   THEN
      REPEAT
         sym := GetParent(sym)
      UNTIL IsRecord(sym)
   END ;
   RETURN( sym )
END GetRecordOfVarient ;


(*
   CanDeclareRecordKind - 
*)

PROCEDURE CanDeclareRecordKind (sym: CARDINAL) : BOOLEAN ;
BEGIN
   sym := GetRecordOfVarient(sym) ;
   RETURN( IsRecord(sym) AND
           ((GetDefaultRecordFieldAlignment(sym)=NulSym) OR
            IsFullyDeclared(GetDefaultRecordFieldAlignment(sym))) )
END CanDeclareRecordKind ;


(*
   DeclareRecordKind - works out whether record, sym, is packed or not.
*)

PROCEDURE DeclareRecordKind (sym: CARDINAL) ;
BEGIN
   IF IsRecord(sym)
   THEN
      DetermineIfRecordPacked(sym)
   END ;
   WatchIncludeList(sym, todolist) ;
   WatchRemoveList(sym, heldbyalignment) ;
   WatchIncludeList(sym, finishedalignment) ;
   IF AllDependantsFullyDeclared(sym)
   THEN
      (* ready to be solved.. *)
   END
END DeclareRecordKind ;


(*
   CanDeclareRecord - 
*)

PROCEDURE CanDeclareRecord (sym: CARDINAL) : BOOLEAN ;
BEGIN
   TraverseDependants(sym) ;
   IF AllDependantsFullyDeclared(sym)
   THEN
      RETURN TRUE
   ELSE
      WatchIncludeList(sym, finishedalignment) ;
      RETURN FALSE
   END
END CanDeclareRecord ;


(*
   FinishDeclareRecord - 
*)

PROCEDURE FinishDeclareRecord (sym: CARDINAL) ;
BEGIN
   DeclareTypeConstFully(sym) ;
   WatchRemoveList(sym, heldbyalignment) ;
   WatchRemoveList(sym, finishedalignment) ;
   WatchRemoveList(sym, todolist) ;
   WatchIncludeList(sym, fullydeclared)
END FinishDeclareRecord ;


(*
   CanDeclareTypePartially - return TRUE if we are able to make a
                             gcc partially created type.
*)

PROCEDURE CanDeclareTypePartially (sym: CARDINAL) : BOOLEAN ;
VAR
   type: CARDINAL ;
BEGIN
   IF IsElementInSet(PartiallyDeclared, sym)
   THEN
      RETURN( FALSE )
   ELSIF IsProcType(sym) OR IsRecord(sym) OR IsVarient(sym) OR IsFieldVarient(sym)
   THEN
      RETURN( TRUE )
   ELSIF IsType(sym)
   THEN
      type := GetType(sym) ;
      IF (type#NulSym) AND IsNilTypedArrays(type)
      THEN
         RETURN( TRUE )
      END
   END ;
   RETURN( FALSE )
END CanDeclareTypePartially ;


(*
   DeclareTypePartially - create the gcc partial type symbol from, sym.
*)

PROCEDURE DeclareTypePartially (sym: CARDINAL) ;
VAR
   t: Tree ;
BEGIN
   (* check to see if we have already partially declared the symbol *)
   IF NOT IsElementInSet(PartiallyDeclared, sym)
   THEN
      IF IsRecord(sym)
      THEN
         Assert(NOT IsElementInSet(HeldByAlignment, sym)) ;
         t := DoStartDeclaration(sym, BuildStartRecord) ;
         WatchIncludeList(sym, heldbyalignment)
      ELSIF IsVarient(sym)
      THEN
         Assert(NOT IsElementInSet(HeldByAlignment, sym)) ;
         t := DoStartDeclaration(sym, BuildStartVarient) ;
         WatchIncludeList(sym, heldbyalignment)
      ELSIF IsFieldVarient(sym)
      THEN
         Assert(NOT IsElementInSet(HeldByAlignment, sym)) ;
         t := DoStartDeclaration(sym, BuildStartFieldVarient) ;
         WatchIncludeList(sym, heldbyalignment)
      ELSIF IsProcType(sym)
      THEN
         t := DoStartDeclaration(sym, BuildStartFunctionType)
      ELSIF IsType(sym)
      THEN
         IF NOT GccKnowsAbout(sym)
         THEN
            PreAddModGcc(sym, BuildStartType(KeyToCharStar(GetFullSymName(sym)),
                                             Mod2Gcc(GetType(sym))))
         END
      ELSE
         InternalError('do not know how to create a partial type from this symbol',
                       __FILE__, __LINE__)
      END ;
      WatchIncludeList(sym, partiallydeclared) ;
      TraverseDependants(sym)
   END
END DeclareTypePartially ;


(*
   CanDeclareArrayAsNil - 
*)

PROCEDURE CanDeclareArrayAsNil (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsArray(sym) AND ArrayComponentsDeclared(sym) )
END CanDeclareArrayAsNil ;


(*
   DeclareArrayAsNil - 
*)

PROCEDURE DeclareArrayAsNil (sym: CARDINAL) ;
BEGIN
   PreAddModGcc(sym, BuildStartArrayType(BuildIndex(sym), NIL, SkipType(GetType(sym)))) ;
   WatchIncludeList(sym, niltypedarrays)
END DeclareArrayAsNil ;


(*
   CanDeclareArrayPartially - 
*)

PROCEDURE CanDeclareArrayPartially (sym: CARDINAL) : BOOLEAN ;
VAR
   type: CARDINAL ;
BEGIN
   IF IsArray(sym)
   THEN
      type := GetType(sym) ;
      IF IsPartiallyOrFullyDeclared(type) OR
         (IsPointer(type) AND IsNilTypedArrays(type))
      THEN
         RETURN( TRUE )
      END
   END ;
   RETURN( FALSE )
END CanDeclareArrayPartially ;


(*
   DeclareArrayPartially - 
*)

PROCEDURE DeclareArrayPartially (sym: CARDINAL) ;
BEGIN
   Assert(IsArray(sym) AND GccKnowsAbout(sym)) ;
   PutArrayType(Mod2Gcc(sym), Mod2Gcc(GetType(sym))) ;
   WatchIncludeList(sym, partiallydeclared)
END DeclareArrayPartially ;


(*
   CanDeclarePointerToNilArray - 
*)

PROCEDURE CanDeclarePointerToNilArray (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsPointer(sym) AND IsNilTypedArrays(GetType(sym)) )
END CanDeclarePointerToNilArray ;


(*
   DeclarePointerToNilArray - 
*)

PROCEDURE DeclarePointerToNilArray (sym: CARDINAL) ;
BEGIN
   PreAddModGcc(sym, BuildPointerType(Mod2Gcc(GetType(sym)))) ;
   WatchIncludeList(sym, niltypedarrays)
END DeclarePointerToNilArray ;


(*
   CanPromotePointerFully - 
*)

PROCEDURE CanPromotePointerFully (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsPointer(sym) AND IsPartiallyOrFullyDeclared(GetType(sym)) )
END CanPromotePointerFully ;


(*
   PromotePointerFully - 
*)

PROCEDURE PromotePointerFully (sym: CARDINAL) ;
BEGIN
   WatchIncludeList(sym, fullydeclared)
END PromotePointerFully ;


(*
   CompletelyResolved - returns TRUE if a symbols has been completely resolved
                        and is not partically declared (such as a record).
*)

PROCEDURE CompletelyResolved (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsElementInSet(FullyDeclared, sym) )
END CompletelyResolved ;


(*
   IsTypeQ - returns TRUE if all q(dependants) of, sym,
             return TRUE.
*)

PROCEDURE IsTypeQ (sym: CARDINAL; q: IsAction) : BOOLEAN ;
BEGIN
   IF IsVar(sym)
   THEN
      RETURN( IsVarDependants(sym, q) )
   ELSIF IsEnumeration(sym)
   THEN
      RETURN( IsEnumerationDependants(sym, q) )
   ELSIF IsFieldEnumeration(sym)
   THEN
      RETURN( TRUE )
   ELSIF IsSubrange(sym)
   THEN
      RETURN( IsSubrangeDependants(sym, q) )
   ELSIF IsPointer(sym)
   THEN
      RETURN( IsPointerDependants(sym, q) )
   ELSIF IsRecord(sym)
   THEN
      RETURN( IsRecordDependants(sym, q) )
   ELSIF IsRecordField(sym)
   THEN
      RETURN( IsRecordFieldDependants(sym, q) )
   ELSIF IsVarient(sym)
   THEN
      RETURN( IsVarientDependants(sym, q) )
   ELSIF IsFieldVarient(sym)
   THEN
      RETURN( IsVarientFieldDependants(sym, q) )
   ELSIF IsArray(sym)
   THEN
      RETURN( IsArrayDependants(sym, q) )
   ELSIF IsProcType(sym)
   THEN
      RETURN( IsProcTypeDependants(sym, q) )
   ELSIF IsUnbounded(sym)
   THEN
      RETURN( IsUnboundedDependants(sym, q) )
   ELSIF IsPartialUnbounded(sym)
   THEN
      InternalError('should not be declaring a partial unbounded symbol',
                    __FILE__, __LINE__)
   ELSIF IsSet(sym)
   THEN
      RETURN( IsSetDependants(sym, q) )
   ELSIF IsType(sym)
   THEN
      RETURN( IsTypeDependants(sym, q) )
   ELSIF IsConst(sym)
   THEN
      RETURN( IsConstDependants(sym, q) )
   ELSIF IsConstructor(sym) OR IsConstSet(sym)
   THEN
      (* sym can be a constructor, but at present we have not resolved whether
         all dependants are constants.
       *)
      RETURN( IsConstructorDependants(sym, q) )
   ELSIF IsProcedure(sym)
   THEN
      RETURN( IsProcedureDependants(sym, q) )
   ELSE
      RETURN( TRUE )
   END
END IsTypeQ ;


(*
   IsNilTypedArrays - returns TRUE if, sym, is dependant upon a NIL typed array
*)

PROCEDURE IsNilTypedArrays (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsElementInSet(NilTypedArrays, sym) )
END IsNilTypedArrays ;


(*
   IsFullyDeclared - returns TRUE if, sym, is fully declared.
*)

PROCEDURE IsFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsElementInSet(FullyDeclared, sym) )
END IsFullyDeclared ;


(*
   AllDependantsFullyDeclared - returns TRUE if all dependants of,
                                sym, are declared.
*)

PROCEDURE AllDependantsFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsTypeQ(sym, IsFullyDeclared) )
END AllDependantsFullyDeclared ;


(*
   NotAllDependantsFullyDeclared - returns TRUE if any dependants of,
                                   sym, are not declared.
*)

PROCEDURE NotAllDependantsFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( NOT IsTypeQ(sym, IsFullyDeclared) )
END NotAllDependantsFullyDeclared ;


(*
   IsPartiallyDeclared - returns TRUE if, sym, is partially declared.
*)

PROCEDURE IsPartiallyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsElementInSet(PartiallyDeclared, sym) )
END IsPartiallyDeclared ;


(*
   AllDependantsPartiallyDeclared - returns TRUE if all dependants of,
                                    sym, are partially declared.
*)

PROCEDURE AllDependantsPartiallyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsTypeQ(sym, IsPartiallyDeclared) )
END AllDependantsPartiallyDeclared ;


(*
   NotAllDependantsPartiallyDeclared - returns TRUE if any dependants of,
                                       sym, are not partially declared.
*)

PROCEDURE NotAllDependantsPartiallyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( NOT IsTypeQ(sym, IsPartiallyDeclared) )
END NotAllDependantsPartiallyDeclared ;


(*
   IsPartiallyOrFullyDeclared - returns TRUE if, sym, is partially or fully declared.
*)

PROCEDURE IsPartiallyOrFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsElementInSet(PartiallyDeclared, sym) OR
           IsElementInSet(FullyDeclared, sym) )
END IsPartiallyOrFullyDeclared ;


(*
   AllDependantsPartiallyOrFullyDeclared - returns TRUE if all dependants of,
                                           sym, are partially or fully declared.
*)

PROCEDURE AllDependantsPartiallyOrFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsTypeQ(sym, IsPartiallyOrFullyDeclared) )
END AllDependantsPartiallyOrFullyDeclared ;


(*
   NotAllDependantsPartiallyOrFullyDeclared - returns TRUE if all dependants of,
                                              sym, are not partially and not fully
                                              declared.
*)

PROCEDURE NotAllDependantsPartiallyOrFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsTypeQ(sym, IsPartiallyOrFullyDeclared) )
END NotAllDependantsPartiallyOrFullyDeclared ;


(*
   TypeConstDependantsFullyDeclared - returns TRUE if sym is a constant or
                                      type and its dependants are fully
                                      declared.
*)

PROCEDURE TypeConstDependantsFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (NOT IsVar(sym)) AND
           (NOT IsRecord(sym)) AND
           (NOT IsParameter(sym)) AND
           AllDependantsFullyDeclared(sym) )
END TypeConstDependantsFullyDeclared ;


(*
   CanBeDeclaredViaPartialDependants - returns TRUE if this symbol
                                       can be declared by partial
                                       dependants.  Such a symbol must
                                       be a record, proctype or
                                       an array.
*)

PROCEDURE CanBeDeclaredViaPartialDependants (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (IsPointer(sym) OR IsProcType(sym)) AND
           AllDependantsPartiallyOrFullyDeclared(sym) )
END CanBeDeclaredViaPartialDependants ;


(*
   DeclareConstFully - will add, sym, to the fully declared list and
                       also remove it from the to do list.  This is
                       called indirectly from M2GenGCC as it calculates
                       constants during quadruple processing.
*)

PROCEDURE DeclareConstFully (sym: CARDINAL) ;
BEGIN
   WatchIncludeList(sym, fullydeclared) ;
   WatchRemoveList(sym, todolist) ;
   WatchRemoveList(sym, partiallydeclared) ;
   WatchRemoveList(sym, tobesolvedbyquads)
END DeclareConstFully ;


(*
   PutToBeSolvedByQuads - places, sym, to this list and returns,
                          sym.
*)

PROCEDURE PutToBeSolvedByQuads (sym: CARDINAL) ;
BEGIN
   WatchIncludeList(sym, tobesolvedbyquads)
END PutToBeSolvedByQuads ;


(*
   DeclareTypeConstFully - declare the GCC type and add the double
                           book keeping entry.
*)

PROCEDURE DeclareTypeConstFully (sym: CARDINAL) ;
VAR
   t: Tree ;
BEGIN
   IF NOT IsElementInSet(ToBeSolvedByQuads, sym)
   THEN
      IF IsModule(sym) OR IsDefImp(sym)
      THEN
         WatchIncludeList(sym, fullydeclared) ;
         WatchRemoveList(sym, partiallydeclared) ;
         WatchRemoveList(sym, todolist)
      ELSIF IsProcedure(sym)
      THEN
         DeclareProcedureToGcc(sym) ;
         WatchIncludeList(sym, fullydeclared) ;
         WatchRemoveList(sym, partiallydeclared) ;
         WatchRemoveList(sym, todolist)
      ELSE
         t := TypeConstFullyDeclared(sym) ;
         IF t#NIL
         THEN
            (* add relationship between gccsym and sym *)
            PreAddModGcc(sym, t) ;
            WatchIncludeList(sym, fullydeclared) ;
            WatchRemoveList(sym, partiallydeclared) ;
            WatchRemoveList(sym, heldbyalignment) ;
            WatchRemoveList(sym, finishedalignment) ;
            WatchRemoveList(sym, todolist)
         END
      END
   END
END DeclareTypeConstFully ;


(*
   DeclareTypeFromPartial - declare the full GCC type from a partial type
                            and add the double book keeping entry.
*)

PROCEDURE DeclareTypeFromPartial (sym: CARDINAL) ;
VAR
   t: Tree ;
BEGIN
   t := CompleteDeclarationOf(sym) ;
   IF t=NIL
   THEN
      InternalError('expecting to be able to create a gcc type',
                    __FILE__, __LINE__)
   ELSE
      AddModGcc(sym, t) ;
      WatchIncludeList(sym, fullydeclared) ;
      WatchRemoveList(sym, partiallydeclared)
   END
END DeclareTypeFromPartial ;


(*
   DeclarePointerTypeFully - if, sym, is a pointer type then
                             declare it.
*)

PROCEDURE DeclarePointerTypeFully (sym: CARDINAL) ;
BEGIN
   IF IsPointer(sym)
   THEN
      WatchIncludeList(sym, fullydeclared) ;
      WatchRemoveList(sym, partiallydeclared) ;
      WatchRemoveList(sym, todolist) ;
      PreAddModGcc(sym, DeclarePointer(sym))
   ELSE
      (* place sym and all dependants on the todolist
         providing they are not already on the FullyDeclared list
      *)
      TraverseDependants(sym)
   END
END DeclarePointerTypeFully ;


(*
   CanBeDeclaredPartiallyViaPartialDependants - returns TRUE if, sym,
                                                can be partially declared via
                                                another partially declared type.
*)

PROCEDURE CanBeDeclaredPartiallyViaPartialDependants (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsType(sym) AND AllDependantsPartiallyDeclared(sym) )
END CanBeDeclaredPartiallyViaPartialDependants ;


(*
   EmitCircularDependancyError - issue a dependancy error.
*)

PROCEDURE EmitCircularDependancyError (sym: CARDINAL) ;
BEGIN
   MetaError1('circular dependancy error found when trying to resolve {%1Uad}',
              sym)
END EmitCircularDependancyError ;


TYPE
   Rule = (none, partialtype, arraynil, pointernilarray, arraypartial,
           pointerfully, recordkind, recordfully, typeconstfully,
           pointerfrompartial, typefrompartial, partialfrompartial,
           partialtofully, circulartodo, circularpartial, circularniltyped) ;

VAR
   bodyp          : WalkAction ;
   bodyq          : IsAction ;
   bodyt          : ListType ;
   bodyl          : Set ;
   bodyr          : Rule ;
   recursionCaught,
   oneResolved,
   noMoreWritten  : BOOLEAN ;


(*
   WriteRule - writes out the name of the rule.
*)

PROCEDURE WriteRule ;
BEGIN
   IF Debugging
   THEN
      CASE bodyr OF

      none              :  printf0('none') |
      partialtype       :  printf0('partialtype') |
      arraynil          :  printf0('arraynil') |
      pointernilarray   :  printf0('pointernilarray') |
      arraypartial      :  printf0('arraypartial') |
      pointerfully      :  printf0('pointerfully') |
      recordkind        :  printf0('recordkind') |
      recordfully       :  printf0('recordfully') |
      typeconstfully    :  printf0('typeconstfully') |
      pointerfrompartial:  printf0('pointerfrompartial') |
      typefrompartial   :  printf0('typefrompartial') |
      partialfrompartial:  printf0('partialfrompartial') |
      partialtofully    :  printf0('partialtofully') |
      circulartodo      :  printf0('circulartodo') |
      circularpartial   :  printf0('circularpartial') |
      circularniltyped  :  printf0('circularniltyped')

      ELSE
         InternalError('unknown rule', __FILE__, __LINE__)
      END
   END
END WriteRule ;


(*
   Body - 
*)

PROCEDURE Body (sym: CARDINAL) ;
BEGIN
   IF bodyq(sym)
   THEN
      WatchRemoveList(sym, bodyt) ;
      bodyp(sym) ;
      (* bodyp(sym) might have replaced sym into the set *)
      IF NOT IsElementInSet(bodyl, sym)
      THEN
         noMoreWritten := FALSE ;
         oneResolved := TRUE
      END
   END
END Body ;


(*
   ForeachTryDeclare - while q(of one sym in l) is true
                          for each symbol in, l,
                          if q(sym)
                          then
                             p(sym)
                          end
                       end
*)

PROCEDURE ForeachTryDeclare (start, end: CARDINAL;
                             t: ListType; l: Set; r: Rule;
                             q: IsAction; p: WalkAction) : BOOLEAN ;
VAR
   sym : CARDINAL ;
   n, i: CARDINAL ;
BEGIN
   IF recursionCaught
   THEN
      InternalError('caught recursive cycle in ForeachTryDeclare', __FILE__, __LINE__)
   END ;
   bodyt := t ;
   bodyq := q ;
   bodyp := p ;
   bodyl := l ;
   bodyr := r ;
   recursionCaught := TRUE ;
   oneResolved := FALSE ;
   REPEAT
      noMoreWritten := TRUE ;
      ForeachElementInSetDo(l, Body)
   UNTIL noMoreWritten ;
   bodyr := none ;
   recursionCaught := FALSE ;
   RETURN( oneResolved )
END ForeachTryDeclare ;


(*
   testThis - 
*)

PROCEDURE testThis ;
VAR
   t      : Tree ;
   type,
   pointer,
   array  : CARDINAL ;
BEGIN
   array := 628 ;
   IF NOT GccKnowsAbout(array)
   THEN
      PreAddModGcc(array, BuildStartArrayType(BuildIndex(array), NIL, SkipType(GetType(array))))
   END ;
   pointer := 626 ;
   IF NOT GccKnowsAbout(pointer)
   THEN
      PreAddModGcc(pointer, BuildPointerType(Mod2Gcc(array)))
   END ;
   type := 627 ;
   IF NOT GccKnowsAbout(type)
   THEN
      PreAddModGcc(type, BuildStartType(KeyToCharStar(GetFullSymName(type)),
                                        Mod2Gcc(pointer))) ;
      PutArrayType(Mod2Gcc(array), Mod2Gcc(type)) ;
      t := BuildEndType(Mod2Gcc(type)) ;
      WatchRemoveList(type, todolist) ;
      WatchIncludeList(type, fullydeclared) ;
      WatchRemoveList(pointer, todolist) ;
      WatchIncludeList(pointer, fullydeclared) ;
      t := DeclareArray(array) ;
      WatchIncludeList(array, fullydeclared) ;
      WatchRemoveList(array, todolist)
   END
END testThis ;


(*
   DeclaredOutandingTypes - writes out any types that have their
                            dependants solved.  It returns TRUE if
                            all outstanding types have been written.
*)

PROCEDURE DeclaredOutstandingTypes (MustHaveCompleted: BOOLEAN;
                                    start, end: CARDINAL) : BOOLEAN ;
VAR
   finished        : BOOLEAN ;  (* p2c cannot handle LOOP EXIT END *)
   d, a, p, f, n, b: CARDINAL ;
BEGIN
   d := 0 ;
   a := 0 ;
   p := 0 ;
   f := 0 ;
   n := 0 ;
   b := 0 ;
   finished := FALSE ;
   REPEAT
      IF FindSetNumbers(d, a, p, f, n, b) OR Progress
      THEN
         DebugSetNumbers
      END ;
      IF ForeachTryDeclare(start, end,
                           todolist, ToDoList,
                           partialtype,
                           CanDeclareTypePartially,
                           DeclareTypePartially)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              todolist, ToDoList,
                              arraynil,
                              CanDeclareArrayAsNil,
                              DeclareArrayAsNil)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              todolist, ToDoList,
                              pointernilarray,
                              CanDeclarePointerToNilArray,
                              DeclarePointerToNilArray)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              niltypedarrays, NilTypedArrays,
                              arraypartial,
                              CanDeclareArrayPartially,
                              DeclareArrayPartially)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              niltypedarrays, NilTypedArrays,
                              pointerfully,
                              CanPromotePointerFully,
                              PromotePointerFully)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              heldbyalignment, HeldByAlignment,
                              recordkind,
                              CanDeclareRecordKind,
                              DeclareRecordKind)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              finishedalignment, FinishedAlignment,
                              recordfully,
                              CanDeclareRecord,
                              FinishDeclareRecord)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              todolist, ToDoList,
                              typeconstfully,
                              TypeConstDependantsFullyDeclared,
                              DeclareTypeConstFully)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              todolist, ToDoList,
                              (* partiallydeclared, PartiallyDeclared, *)
                              typefrompartial,
                              CanBeDeclaredViaPartialDependants,
                              DeclareTypeFromPartial)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              partiallydeclared, PartiallyDeclared,
                              partialfrompartial,
                              CanBeDeclaredPartiallyViaPartialDependants,
                              DeclareTypePartially)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              partiallydeclared, PartiallyDeclared,
                              partialtofully,
                              TypeConstDependantsFullyDeclared,
                              DeclareTypeConstFully)
      THEN
         (* continue looping *)
      ELSE
         (* nothing left to do (and constants are resolved elsewhere) *)
         finished := TRUE
      END
   UNTIL finished ;
   IF MustHaveCompleted
   THEN
      IF ForeachTryDeclare(start, end,
                           todolist, ToDoList,
                           circulartodo,
                           NotAllDependantsFullyDeclared,
                           EmitCircularDependancyError)
      THEN
      ELSIF ForeachTryDeclare(start, end,
                              partiallydeclared, PartiallyDeclared,
                              circularpartial,
                              NotAllDependantsPartiallyDeclared,
                              EmitCircularDependancyError)
      THEN
      ELSIF ForeachTryDeclare(start, end,
                              niltypedarrays, NilTypedArrays,
                              circularniltyped,
                              NotAllDependantsPartiallyDeclared,
                              EmitCircularDependancyError)
      THEN
      END
   END ;
   RETURN( NoOfElementsInSet(ToDoList)=0 )
END DeclaredOutstandingTypes ;


(*
   CompleteDeclarationOf - returns the GCC Tree for, sym, if it can
                           be created from partially or fully declared
                           dependents.
*)

PROCEDURE CompleteDeclarationOf (sym: CARDINAL) : Tree ;
BEGIN
   IF IsArray(sym)
   THEN
      RETURN( DeclareArray(sym) )
   ELSIF IsProcType(sym)
   THEN
      RETURN( DeclareProcType(sym) )
   ELSIF IsRecordField(sym)
   THEN
      RETURN( DeclareRecordField(sym) )
   ELSIF IsPointer(sym)
   THEN
      RETURN( DeclarePointer(sym) )
   ELSE
      RETURN( NIL )
   END
END CompleteDeclarationOf ;


(*
   DeclareType - here a type has been created via TYPE foo = bar,
                 we must tell GCC about it.
*)

PROCEDURE DeclareType (sym: CARDINAL) : Tree ;
VAR
   t: Tree ;
BEGIN
   IF GetType(sym)=NulSym
   THEN
      MetaError1('base type {%1Ua} not understood', sym) ;
      InternalError('base type should have been declared', __FILE__, __LINE__)
   ELSE
      IF GetSymName(sym)=NulName
      THEN
         RETURN( Tree(Mod2Gcc(GetType(sym))) )
      ELSE
         IF GccKnowsAbout(sym)
         THEN
            t := Mod2Gcc(sym)
         ELSE
            (* not partially declared therefore start it *)
            t := BuildStartType(KeyToCharStar(GetFullSymName(sym)), Mod2Gcc(GetType(sym)))
         END ;
         t := BuildEndType(t) ;  (* now finish it *)
         RETURN( t )
      END
   END
END DeclareType ;


(*
   DeclareIntegerConstant - declares an integer constant.
*)

PROCEDURE DeclareIntegerConstant (sym: CARDINAL; value: INTEGER) ;
BEGIN
   PreAddModGcc(sym, BuildIntegerConstant(value)) ;
   WatchRemoveList(sym, todolist) ;
   WatchIncludeList(sym, fullydeclared)
END DeclareIntegerConstant ;


(*
   DeclareIntegerFromTree - declares an integer constant from a Tree, value.
*)

PROCEDURE DeclareConstantFromTree (sym: CARDINAL; value: Tree) ;
BEGIN
   PreAddModGcc(sym, value) ;
   WatchRemoveList(sym, todolist) ;
   WatchIncludeList(sym, fullydeclared)
END DeclareConstantFromTree ;


(*
   DeclareCharConstant - declares a character constant.
*)

PROCEDURE DeclareCharConstant (sym: CARDINAL) ;
BEGIN
   PreAddModGcc(sym, BuildCharConstant(KeyToCharStar(GetString(sym)))) ;
   WatchRemoveList(sym, todolist) ;
   WatchIncludeList(sym, fullydeclared)
END DeclareCharConstant ;


(*
   DeclareStringConstant - declares a string constant.
*)

PROCEDURE DeclareStringConstant (sym: CARDINAL) ;
BEGIN
   PreAddModGcc(sym, BuildStringConstant(KeyToCharStar(GetString(sym)),
                                         GetStringLength(sym))) ;
   WatchRemoveList(sym, todolist) ;
   WatchIncludeList(sym, fullydeclared)
END DeclareStringConstant ;


(*
   PromoteToString - declare, sym, and then promote it to a string.
                     Note that if sym is a single character we do
                          *not* record it as a string
                          but as a char however we always
                          return a string constant.
*)

PROCEDURE PromoteToString (tokenno: CARDINAL; sym: CARDINAL) : Tree ;
VAR
   size: CARDINAL ;
BEGIN
   DeclareConstant(tokenno, sym) ;
   size := GetStringLength(sym) ;
   IF size>1
   THEN
      (* will be a string anyway *)
      RETURN( Tree(Mod2Gcc(sym)) )
   ELSE
      RETURN(
             BuildStringConstant(KeyToCharStar(GetString(sym)),
                                 GetStringLength(sym))
            )
   END
END PromoteToString ;


(*
   WalkConstructor - walks all dependants of, sym.
*)

PROCEDURE WalkConstructor (sym: CARDINAL; p: WalkAction) ;
VAR
   type: CARDINAL ;
BEGIN
   type := GetType(sym) ;
   IF type#NulSym
   THEN
      WalkDependants(type, p) ;
      WalkConstructorDependants(sym, p)
   END
END WalkConstructor ;


(*
   DeclareConstructor - declares a constructor.
*)

PROCEDURE DeclareConstructor (quad: CARDINAL; sym: CARDINAL) ;
VAR
   tokenno: CARDINAL ;
BEGIN
   IF sym=NulSym
   THEN
      InternalError('trying to declare the NulSym', __FILE__, __LINE__)
   END ;
   IF IsConstructor(sym) AND (NOT GccKnowsAbout(sym))
   THEN
      IF quad=0
      THEN
         tokenno := GetDeclared(sym)
      ELSE
         tokenno := QuadToTokenNo(quad)
      END ;
      WalkConstructor(sym, TraverseDependants) ;
      DeclareTypesConstantsProceduresInRange(quad, quad) ;
      Assert(IsConstructorDependants(sym, IsFullyDeclared)) ;
      PushValue(sym) ;
      DeclareConstantFromTree(sym, PopConstructorTree(tokenno))
   END
END DeclareConstructor ;


(*
   TryDeclareConstructor - try and declare a constructor.  If, sym, is a
                           constructor try and declare it, if we cannot
                           then enter it into the to do list.
*)

PROCEDURE TryDeclareConstructor (quad: CARDINAL; sym: CARDINAL) ;
VAR
   tokenno: CARDINAL ;
BEGIN
   IF sym=NulSym
   THEN
      InternalError('trying to declare the NulSym', __FILE__, __LINE__)
   END ;
   IF IsConstructor(sym) AND (NOT GccKnowsAbout(sym))
   THEN
      IF quad=0
      THEN
         tokenno := GetDeclared(sym)
      ELSE
         tokenno := QuadToTokenNo(quad)
      END ;
      WalkConstructor(sym, TraverseDependants) ;
      IF NOT IsElementInSet(ToBeSolvedByQuads, sym)
      THEN
         TryEvaluateValue(sym) ;
         IF IsConstructorDependants(sym, IsFullyDeclared)
         THEN
            PushValue(sym) ;
            DeclareConstantFromTree(sym, PopConstructorTree(tokenno))
         END
      END
   END
END TryDeclareConstructor ;


(*
   WalkConst - walks all dependants of, sym.
*)

PROCEDURE WalkConst (sym: CARDINAL; p: WalkAction) ;
VAR
   type: CARDINAL ;
BEGIN
   Assert(IsConst(sym)) ;
   type := GetType(sym) ;
   IF type#NulSym
   THEN
      p(type)
   END ;
   IF IsConstSet(sym) OR IsConstructor(sym)
   THEN
      WalkConstructor(sym, p)
   END
END WalkConst ;


(*
   IsConstDependants - returns TRUE if the symbol, sym,
                       q(dependants) all return TRUE.
*)

PROCEDURE IsConstDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   type: CARDINAL ;
BEGIN
   Assert(IsConst(sym)) ;
   type := GetType(sym) ;
   IF type#NulSym
   THEN
      IF NOT q(type)
      THEN
         RETURN( FALSE )
      END
   END ;
   IF IsConstSet(sym) OR IsConstructor(sym)
   THEN
      RETURN( IsConstructorDependants(sym, q) )
   END ;
   RETURN( IsValueSolved(sym) )
END IsConstDependants ;


(*
   TryDeclareConstant - try and declare a constant.  If, sym, is a
                        constant try and declare it, if we cannot
                        then enter it into the to do list.
*)

PROCEDURE TryDeclareConstant (tokenno: CARDINAL; sym: CARDINAL) ;
VAR
   type: CARDINAL ;
BEGIN
   TryDeclareConstructor(0, sym) ;
   IF IsConst(sym)
   THEN
      TraverseDependants(sym) ;
      type := GetType(sym) ;
      IF (type#NulSym) AND (NOT CompletelyResolved(type))
      THEN
         TraverseDependants(sym) ;
(*
         WatchIncludeList(sym, todolist) ;
         WatchIncludeList(type, todolist) ;
*)
         RETURN
      END ;
      IF IsConstructor(sym) AND (NOT IsConstructorConstant(sym))
      THEN
         TraverseDependants(sym) ;
(*
         WatchIncludeList(sym, todolist) ;
*)
         RETURN
      END ;
      IF (IsConstructor(sym) OR IsConstSet(sym)) AND (type=NulSym)
      THEN
(*
         WatchIncludeList(sym, todolist) ;
*)
         TraverseDependants(sym) ;
         RETURN
      END ;
      IF IsElementInSet(ToBeSolvedByQuads, sym)
      THEN
         (* we allow the above rules to be executed even if it is fully declared
            so to ensure that types of compiler builtin constants (BitsetSize
            etc) are fully declared.

            However at this point if, sym, is fully declared we return
         *)
         IF IsFullyDeclared(sym)
         THEN
            RETURN
         END ;
         TraverseDependants(sym) ;
(*
         WatchIncludeList(sym, todolist)
*)
      ELSE
         TryDeclareConst(tokenno, sym)
      END
   END 
END TryDeclareConstant ;


(*
   DeclareConstant - checks to see whether, sym, is a constant and
                     declares the constant to gcc.
*)

PROCEDURE DeclareConstant (tokenno: CARDINAL; sym: CARDINAL) ;
VAR
   type: CARDINAL ;
   t   : Tree ;
BEGIN
   IF IsConst(sym)
   THEN
      TraverseDependants(sym) ;
      type := GetType(sym) ;
      Assert((type=NulSym) OR CompletelyResolved(type)) ;
      Assert((NOT IsConstructor(sym)) OR IsConstructorConstant(sym)) ;
      Assert((type#NulSym) OR (NOT (IsConstructor(sym) OR IsConstSet(sym)))) ;
      t := DeclareConst(tokenno, sym) ;
      Assert(t#NIL)
   END 
END DeclareConstant ;


(*
   TryDeclareConst - try to declare a const to gcc.  If it cannot
                     declare the symbol it places it into the
                     todolist.
*)

PROCEDURE TryDeclareConst (tokenno: CARDINAL; sym: CARDINAL) ;
VAR
   size: CARDINAL ;
BEGIN
   IF NOT GccKnowsAbout(sym)
   THEN
      IF IsConstructor(sym) OR IsConstSet(sym)
      THEN
         WalkConstructorDependants(sym, TraverseDependants) ;
         TryEvaluateValue(sym) ;
         IF NOT IsConstructorDependants(sym, IsFullyDeclared)
         THEN
(*
            WatchIncludeList(sym, todolist) ;
*)
            TraverseDependants(sym) ;
            RETURN
         END ;
         IF NOT IsConstructorConstant(sym)
         THEN
            RETURN
         END
      END ;
      IF IsConstString(sym)
      THEN
         size := GetStringLength(sym) ;
         IF size=1
         THEN
            DeclareCharConstant(sym)
         ELSE
            DeclareStringConstant(sym)
         END
      ELSIF IsValueSolved(sym)
      THEN
         PushValue(sym) ;
         IF IsConstSet(sym)
         THEN
            DeclareConstantFromTree(sym, PopSetTree(tokenno))
         ELSIF IsConstructor(sym)
         THEN
            DeclareConstantFromTree(sym, PopConstructorTree(tokenno))
         ELSIF IsRealType(SkipType(GetType(sym)))
         THEN
            DeclareConstantFromTree(sym, PopRealTree())
         ELSIF IsAComplexType(SkipType(GetType(sym)))
         THEN
            DeclareConstantFromTree(sym, PopComplexTree())
         ELSE
            DeclareConstantFromTree(sym, PopIntegerTree())
         END
      ELSE
         TraverseDependants(sym) ;
(*
         WatchIncludeList(sym, todolist)
*)
      END
   END
END TryDeclareConst ;


(*
   DeclareConst - declares a const to gcc and returns a Tree.
*)

PROCEDURE DeclareConst (tokenno: CARDINAL; sym: CARDINAL) : Tree ;
VAR
   type: CARDINAL ;
   size: CARDINAL ;
BEGIN
   IF GccKnowsAbout(sym)
   THEN
      RETURN( Mod2Gcc(sym) )
   END ;
   IF IsConstructor(sym) OR IsConstSet(sym)
   THEN
      EvaluateValue(sym)
   END ;
   IF IsConstString(sym)
   THEN
      size := GetStringLength(sym) ;
      IF size=1
      THEN
         DeclareCharConstant(sym)
      ELSE
         DeclareStringConstant(sym)
      END
   ELSIF IsValueSolved(sym)
   THEN
      PushValue(sym) ;
      IF IsConstSet(sym)
      THEN
         DeclareConstantFromTree(sym, PopSetTree(tokenno))
      ELSIF IsConstructor(sym)
      THEN
         DeclareConstantFromTree(sym, PopConstructorTree(tokenno))
      ELSIF IsRealType(SkipType(GetType(sym)))
      THEN
         DeclareConstantFromTree(sym, PopRealTree())
      ELSIF IsAComplexType(SkipType(GetType(sym)))
      THEN
         DeclareConstantFromTree(sym, PopComplexTree())
      ELSE
         type := GetType(sym) ;
         IF type=NulSym
         THEN
            DeclareConstantFromTree(sym, PopIntegerTree())
         ELSE
            DeclareConstantFromTree(sym,
                                    BuildConvert(Mod2Gcc(type), PopIntegerTree(), FALSE))
         END
      END
   END ;
   IF GccKnowsAbout(sym)
   THEN
      RETURN( Mod2Gcc(sym) )
   ELSE
      RETURN( NIL )
   END
END DeclareConst ;


(*
   DeclareParameters -
*)

PROCEDURE DeclareParameters (sym: CARDINAL) ;
BEGIN
   DeclareUnboundedProcedureParameters(sym)
END DeclareParameters ;


VAR
   unboundedp: WalkAction ;


(*
   WalkFamilyOfUnbounded - 
*)

PROCEDURE WalkFamilyOfUnbounded (oaf: CARDINAL; dim: CARDINAL; unbounded: CARDINAL) ;
BEGIN
   unboundedp(unbounded)
END WalkFamilyOfUnbounded ;


(*
   WalkAssociatedUnbounded - 
*)

PROCEDURE WalkAssociatedUnbounded (sym: CARDINAL; p: WalkAction) ;
VAR
   oaf: CARDINAL ;
   o  : WalkAction ;
BEGIN
   oaf := GetOAFamily(sym) ;
   o := unboundedp ;
   unboundedp := p ;
   ForeachOAFamily(oaf, WalkFamilyOfUnbounded) ;
   unboundedp := o
END WalkAssociatedUnbounded ;


(*
   WalkProcedureParameterDependants - 
*)

PROCEDURE WalkProcedureParameterDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   son,
   type,
   n, i: CARDINAL ;
BEGIN
   IF IsProcedure(sym)
   THEN
      n := NoOfParam(sym) ;
      i := n ;
      WHILE i>0 DO
         IF IsUnboundedParam(sym, i)
         THEN
            son := GetNthParam(sym, i)
         ELSE
            son := GetNth(sym, i) ;
         END ;
         type := GetType(son) ;
         p(type) ;
         WalkDependants(type, p) ;
         DEC(i)
      END
   END
END WalkProcedureParameterDependants ;


(*
   WalkDependants - walks through all dependants of, Sym,
                    calling, p, for each dependant.
*)

PROCEDURE WalkDependants (sym: CARDINAL; p: WalkAction) ;
BEGIN
   WalkAssociatedUnbounded(sym, p) ;
   IF IsComponent(sym)
   THEN
      WalkComponentDependants(sym, p)
   ELSIF IsEnumeration(sym)
   THEN
      WalkEnumerationDependants(sym, p)
   ELSIF IsSubrange(sym)
   THEN
      WalkSubrangeDependants(sym, p)
   ELSIF IsPointer(sym)
   THEN
      WalkPointerDependants(sym, p)
   ELSIF IsRecord(sym)
   THEN
      WalkRecordDependants(sym, p)
   ELSIF IsVarient(sym)
   THEN
      WalkVarientDependants(sym, p)
   ELSIF IsRecordField(sym)
   THEN
      WalkRecordFieldDependants(sym, p)
   ELSIF IsFieldVarient(sym)
   THEN
      WalkVarientFieldDependants(sym, p)
   ELSIF IsArray(sym)
   THEN
      WalkArrayDependants(sym, p)
   ELSIF IsProcType(sym)
   THEN
      WalkProcTypeDependants(sym, p)
   ELSIF IsUnbounded(sym)
   THEN
      WalkUnboundedDependants(sym, p)
   ELSIF IsSet(sym)
   THEN
      WalkSetDependants(sym, p)
   ELSIF IsType(sym)
   THEN
      WalkTypeDependants(sym, p)
   ELSIF IsConst(sym)
   THEN
      WalkConst(sym, p)
   ELSIF IsVar(sym)
   THEN
      WalkVarDependants(sym, p)
   ELSIF IsProcedure(sym)
   THEN
      WalkProcedureDependants(sym, p)
   END
END WalkDependants ;


(*
   TraverseDependantsInner - 
*)

PROCEDURE TraverseDependantsInner (sym: WORD) ;
BEGIN
   IF (NOT IsElementInSet(FullyDeclared, sym)) AND
      (NOT IsElementInSet(ToDoList, sym))
   THEN
      WatchIncludeList(sym, todolist)
   END ;
   IF NOT IsElementInSet(VisitedList, sym)
   THEN
      IncludeElementIntoSet(VisitedList, sym) ;
      WalkDependants(sym, TraverseDependantsInner)
   END
END TraverseDependantsInner ;


(*
   TraverseDependants - walks, sym, dependants.  But it checks
                        to see that, sym, is not on the
                        FullyDeclared and not on the ToDoList.
*)

PROCEDURE TraverseDependants (sym: WORD) ;
BEGIN
   IF VisitedList=NIL
   THEN
      VisitedList := InitSet(1) ;
      TraverseDependantsInner(sym) ;
      VisitedList := KillSet(VisitedList)
   ELSE
      InternalError('recursive call to TraverseDependants caught', __FILE__, __LINE__)
   END
END TraverseDependants ;


(*
   WalkTypeInfo - walks type, sym, and its dependants.
*)

PROCEDURE WalkTypeInfo (sym: WORD) ;
BEGIN
   IF IsVarient(sym)
   THEN
      InternalError('why have we reached here?', __FILE__, __LINE__)
   ELSIF IsVar(sym)
   THEN
      WalkTypeInfo(GetType(sym)) ;
      IF GetVarBackEndType(sym)#NulSym
      THEN
         WalkTypeInfo(GetVarBackEndType(sym))
      END
   ELSIF IsAModula2Type(sym)
   THEN
      TraverseDependants(sym)
   END
END WalkTypeInfo ;


(*
   DeclareUnboundedProcedureParameters - 
*)

PROCEDURE DeclareUnboundedProcedureParameters (sym: WORD) ;
VAR
   son,
   type,
   p, i: CARDINAL ;
BEGIN
   IF IsProcedure(sym)
   THEN
      p := NoOfParam(sym) ;
      i := p ;
      WHILE i>0 DO
         IF IsUnboundedParam(sym, i)
         THEN
            son := GetNthParam(sym, i) ;
            type := GetType(son) ;
            TraverseDependants(type) ;
            IF GccKnowsAbout(type)
            THEN
               BuildTypeDeclaration(Mod2Gcc(type))
            END
         ELSE
            son := GetNth(sym, i) ;
            type := GetType(son) ;
            TraverseDependants(type)
         END ;
         DEC(i)
      END
   END
END DeclareUnboundedProcedureParameters ;


(*
   WalkUnboundedProcedureParameters - 
*)

PROCEDURE WalkUnboundedProcedureParameters (sym: WORD) ;
VAR
   son,
   type,
   p, i: CARDINAL ;
BEGIN
   IF IsProcedure(sym)
   THEN
      p := NoOfParam(sym) ;
      i := p ;
      WHILE i>0 DO
         IF IsUnboundedParam(sym, i)
         THEN
            son := GetNthParam(sym, i) ;
            type := GetType(son) ;
            WalkTypeInfo(type) ;
(*
            type := GetUnboundedRecordType(type) ;
            Assert(IsRecord(type)) ;
            RecordNotPacked(type)      (* which is never packed.                   *)
*)
         ELSE
            son := GetNth(sym, i) ;
            type := GetType(son) ;
            WalkTypeInfo(type)
         END ;
         DEC(i)
      END
   END
END WalkUnboundedProcedureParameters ;


(*
   WalkTypesInProcedure - walk all types in procedure, Sym.
*)

PROCEDURE WalkTypesInProcedure (sym: WORD) ;
BEGIN
   ForeachLocalSymDo(sym, TraverseDependants)
END WalkTypesInProcedure ;


(*
   WalkTypesInModule - declare all types in module, Sym, to GCC.
*)

PROCEDURE WalkTypesInModule (sym: WORD) ;
VAR
   n: Name ;
BEGIN
   IF Debugging
   THEN
      n := GetSymName(sym) ;
      printf1('Declaring types in MODULE %a\n', n)
   END ;
   ForeachLocalSymDo(sym, WalkTypeInfo) ;
   ForeachLocalSymDo(sym, WalkUnboundedProcedureParameters) ;
   ForeachInnerModuleDo(sym, WalkTypesInModule)
END WalkTypesInModule ;


(*
   IsRecordFieldDependants - returns TRUE if the record field
                             symbol, sym, p(dependants) all return TRUE.
*)

PROCEDURE IsRecordFieldDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   v    : CARDINAL ;
   final: BOOLEAN ;
BEGIN
   final := TRUE ;
   IF NOT q(GetType(sym))
   THEN
      final := FALSE
   END ;
   RETURN( final )
END IsRecordFieldDependants ;


(*
   GetModuleWhereDeclared - returns the module where, Sym, was created.
*)

PROCEDURE GetModuleWhereDeclared (sym: CARDINAL) : CARDINAL ;
VAR
   s: CARDINAL ;
BEGIN
   s := GetScope(sym) ;
   IF (s=NulSym) OR IsDefImp(s) OR
      (IsModule(s) AND (GetScope(s)=NulSym))
   THEN
      RETURN( s )
   ELSE
      RETURN( GetModuleWhereDeclared(s) )
   END
END GetModuleWhereDeclared ;


(*
   IsPseudoProcFunc - returns TRUE if Sym is a pseudo function or procedure.
*)

PROCEDURE IsPseudoProcFunc (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          IsPseudoBaseProcedure(Sym) OR IsPseudoBaseFunction(Sym) OR
          IsPseudoSystemFunction(Sym)
         )
END IsPseudoProcFunc ;


(*
   IsProcedureGccNested - returns TRUE if procedure, sym, will be considered
                          as nested by GCC.
                          This will occur if either its outer defining scope
                          is a procedure or is a module which is inside a
                          procedure.
*)

PROCEDURE IsProcedureGccNested (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          IsProcedureNested(sym) OR
          (IsModule(GetScope(sym)) AND IsModuleWithinProcedure(GetScope(sym)))
         )
END IsProcedureGccNested ;


(*
   AlignProcedureWithSource - tells the gcc backend the source file and line
                              number associated with the start of the procedure.
*)

PROCEDURE AlignProcedureWithSource (sym: CARDINAL) ;
VAR
   scope,
   start,
   end  : CARDINAL ;
   file : String ;
   t,
   line : CARDINAL ;
BEGIN
   IF GenerateDebugging
   THEN
      GetProcedureQuads(sym, scope, start, end) ;
      IF start>0
      THEN
         t := QuadToTokenNo(start) ;
         line := TokenToLineNo(t, 0) ;
         file := FindFileNameFromToken(t, 0) ;
         SetFileNameAndLineNo(string(file), line)
      END
   END
END AlignProcedureWithSource ;


(*
   DeclareProcedureToGcc - traverses all parameters and interfaces to gm2gcc.
*)

PROCEDURE DeclareProcedureToGcc (Sym: CARDINAL) ;
VAR
   GccParam: Tree ;
   Son,
   p, i    : CARDINAL ;
BEGIN
   IF (NOT GccKnowsAbout(Sym)) AND (NOT IsPseudoProcFunc(Sym)) AND
      (IsEffectivelyImported(GetMainModule(), Sym) OR
       (GetModuleWhereDeclared(Sym)=GetMainModule()) OR
       IsNeededAtRunTime(Sym) OR
       IsImported(GetBaseModule(), Sym) OR
       IsExported(GetModuleWhereDeclared(Sym), Sym))
   THEN
      Assert(PushParametersLeftToRight) ;
      AlignProcedureWithSource(Sym) ;
      BuildStartFunctionDeclaration(UsesVarArgs(Sym)) ;
      p := NoOfParam(Sym) ;
      i := p ;
      WHILE i>0 DO
         (* note we dont use GetNthParam as we want the parameter that is seen by the procedure block
            remember that this is treated exactly the same as a variable, just its position on
            the activation record is special (ie a parameter)
         *)
         Son := GetNth(Sym, i) ;
         IF IsUnboundedParam(Sym, i)
         THEN
            GccParam := BuildParameterDeclaration(KeyToCharStar(GetSymName(Son)),
                                                  Mod2Gcc(GetType(Son)),
                                                  FALSE)
         ELSE
            GccParam := BuildParameterDeclaration(KeyToCharStar(GetSymName(Son)),
                                                  Mod2Gcc(GetType(Son)),
                                                  IsVarParam(Sym, i))
         END ;
         PreAddModGcc(Son, GccParam) ;
         WatchRemoveList(Son, todolist) ;
         WatchIncludeList(Son, fullydeclared) ;
         DEC(i)
      END ;
      IF GetType(Sym)=NulSym
      THEN
         PreAddModGcc(Sym, BuildEndFunctionDeclaration(KeyToCharStar(GetFullSymName(Sym)),
                                                       NIL,
                                                       IsEffectivelyImported(GetMainModule(), Sym) AND
                                                       (GetModuleWhereDeclared(Sym)#GetMainModule()),
                                                       IsProcedureGccNested(Sym),
                                                       IsExported(GetModuleWhereDeclared(Sym), Sym)))
      ELSE
         PreAddModGcc(Sym, BuildEndFunctionDeclaration(KeyToCharStar(GetFullSymName(Sym)),
                                                       Mod2Gcc(GetType(Sym)),
                                                       IsEffectivelyImported(GetMainModule(), Sym) AND
                                                       (GetModuleWhereDeclared(Sym)#GetMainModule()),
                                                       IsProcedureGccNested(Sym),
                                                       IsExported(GetModuleWhereDeclared(Sym), Sym)))
      END ;
      WatchRemoveList(Sym, todolist) ;
      WatchIncludeList(Sym, fullydeclared)
   END
END DeclareProcedureToGcc ;


(*
   DeclareProcedure - declares procedure, sym, or all procedures inside
                      module sym.
*)

PROCEDURE DeclareProcedure (sym: WORD) ;
BEGIN
   IF IsProcedure(sym)
   THEN
      DeclareProcedureToGcc(sym)
   ELSIF IsModule(sym) OR IsDefImp(sym)
   THEN
      ForeachProcedureDo(sym, DeclareProcedure)
   ELSE
      InternalError('expecting procedure, module or defimp symbol',
                    __FILE__, __LINE__)
   END
END DeclareProcedure ;


(*
   FoldConstants - a wrapper for ResolveConstantExpressions.
*)

PROCEDURE FoldConstants (start, end: CARDINAL) ;
BEGIN
   IF ResolveConstantExpressions(DeclareConstFully, start, end)
   THEN
   END
END FoldConstants ;


(*
   DeclareTypesConstantsProceduresInRange - 
*)

PROCEDURE DeclareTypesConstantsProceduresInRange (start, end: CARDINAL) ;
VAR
   n, m: CARDINAL ;
BEGIN
   IF DisplayQuadruples
   THEN
      DisplayQuadRange(start, end)
   END ;
   REPEAT
      n := NoOfElementsInSet(ToDoList) ;
      WHILE ResolveConstantExpressions(DeclareConstFully, start, end) DO
      END ;
      (* we need to evaluate some constant expressions to resolve these types *)
      IF DeclaredOutstandingTypes(FALSE, start, end)
      THEN
      END ;
      m := NoOfElementsInSet(ToDoList)
   UNTIL (NOT ResolveConstantExpressions(DeclareConstFully, start, end)) AND
         (n=m)
END DeclareTypesConstantsProceduresInRange ;


(*
   DeclareTypesConstantsProcedures - 
*)

PROCEDURE DeclareTypesConstantsProcedures (scope: CARDINAL) ;
VAR
   s, t: CARDINAL ;
   sb  : ScopeBlock ;
BEGIN
   sb := InitScopeBlock(scope) ;
   REPEAT
      s := NoOfElementsInSet(ToDoList) ;
      (* ForeachLocalSymDo(scope, DeclareTypeInfo) ; *)
      ForeachScopeBlockDo(sb, DeclareTypesConstantsProceduresInRange) ;
      t := NoOfElementsInSet(ToDoList) ;
   UNTIL s=t ;
   sb := KillScopeBlock(sb)
END DeclareTypesConstantsProcedures ;


(*
   AssertAllTypesDeclared - asserts that all types for variables are declared in, scope.
*)

PROCEDURE AssertAllTypesDeclared (scope: CARDINAL) ;
VAR
   o, s,
   n, Var: CARDINAL ;
BEGIN
   n := 1 ;
   Var := GetNth(scope, n) ;
   o := 0 ;
   WHILE Var#NulSym DO
      IF NOT AllDependantsFullyDeclared(GetType(Var))
      THEN
         mystop
      END ;
      Assert(AllDependantsFullyDeclared(GetType(Var))) ;
      INC(n) ;
      Var := GetNth(scope, n)
   END
END AssertAllTypesDeclared ;


(*
   DeclareModuleInit - declared the initialization `function' within
                       a module.
*)

PROCEDURE DeclareModuleInit (sym: WORD) ;
VAR
   t: Tree ;
BEGIN
   IF IsModuleWithinProcedure(sym)
   THEN
      BuildStartFunctionDeclaration(FALSE) ;
      t := BuildEndFunctionDeclaration(KeyToCharStar(GetModuleInitName(sym)),
                                       NIL, FALSE, TRUE, FALSE) ;
      PreAddModGcc(sym, t) ;
      BuildStartFunctionDeclaration(FALSE) ;
      t := BuildEndFunctionDeclaration(KeyToCharStar(GetModuleFinallyName(sym)),
                                       NIL, FALSE, TRUE, FALSE) ;
      PutModuleFinallyFunction(sym, t)
   END
END DeclareModuleInit ;


(*
   StartDeclareScope - declares types, variables associated with this scope.
*)

PROCEDURE StartDeclareScope (scope: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   (* 
   AddSymToWatch(2125) ;  (* watch goes here *)
   DebugSets ;
    *)
   (*
   AddSymToWatch(2125) ;  (* watch goes here *)
   *)
   (*
   IncludeElementIntoSet(WatchList, 369) ;
   IncludeElementIntoSet(WatchList, 709) ;
   *)
   (*
   IncludeElementIntoSet(WatchList, 1006) ;
    *)
   (* AddSymToWatch(8) ; *)
   (* IncludeElementIntoSet(WatchList, 4188) ; *)
   (* AddSymToWatch(1420) ; *)
   (* AddSymToWatch(5889) ; *)
   (* IncludeElementIntoSet(WatchList, 717) ; *)
   (* IncludeElementIntoSet(WatchList, 829) ; *)
   (* IncludeElementIntoSet(WatchList, 2714) ; *)
   (* IncludeElementIntoSet(WatchList, 23222) ; *)
   (* IncludeElementIntoSet(WatchList, 1104) ; *)
   (* IncludeElementIntoSet(WatchList, 859) ; *)
   (* IncludeElementIntoSet(WatchList, 858) ; *)

   (* IncludeElementIntoSet(WatchList, 720) ; *)
   (* IncludeElementIntoSet(WatchList, 706) ; *)
   (* IncludeElementIntoSet(WatchList, 1948) ; *)
   (* IncludeElementIntoSet(WatchList, 865) ; *)

   IF Debugging
   THEN
      n := GetSymName(scope) ;
      printf1('declaring symbols in BLOCK %a\n', n)
   END ;
   IF IsProcedure(scope)
   THEN
      WalkTypesInProcedure(scope) ;
      DeclareProcedure(scope) ;
      ForeachInnerModuleDo(scope, WalkTypesInModule) ;
      DeclareTypesConstantsProcedures(scope) ;
      ForeachInnerModuleDo(scope, DeclareTypesConstantsProcedures) ;
      DeclareLocalVariables(scope) ;
      ForeachInnerModuleDo(scope, DeclareModuleVariables) ;
      AssertAllTypesDeclared(scope) ;
      ForeachProcedureDo(scope, DeclareProcedure) ;
      ForeachInnerModuleDo(scope, StartDeclareScope)
   ELSIF scope#GetMainModule()
   THEN
      DeclareTypesConstantsProcedures(scope) ;
      AssertAllTypesDeclared(scope) ;
      ForeachProcedureDo(scope, DeclareProcedure) ;
      DeclareModuleInit(scope) ;
      ForeachInnerModuleDo(scope, StartDeclareScope)
   ELSE
      ForeachModuleDo(WalkTypesInModule) ;     (* will populate the TYPE and CONST ToDo list  *)
      DeclareTypesConstantsProcedures(scope) ; (* will resolved TYPEs and CONSTs on the ToDo  *)
                                               (* lists.                                      *)
      ForeachModuleDo(DeclareProcedure) ;
      (*
         now that all types have been resolved it is safe to declare
         variables
      *)
(*
      IF scope=148
      THEN
         testThis
      END ;
*)
      AssertAllTypesDeclared(scope) ;
      DeclareGlobalVariables(scope) ;
      ForeachImportedDo(scope, DeclareImportedVariables) ;
      (* now it is safe to declare all procedures *)
      ForeachProcedureDo(scope, DeclareProcedure) ;
      (* --testing-- *)
      ForeachInnerModuleDo(scope, WalkTypesInModule) ;
      ForeachInnerModuleDo(scope, DeclareTypesConstantsProcedures) ;
      (* --end of test-- *)
      ForeachInnerModuleDo(scope, StartDeclareScope)
   END ;
   IF Debugging
   THEN
      n := GetSymName(scope) ;
      printf1('\nEND declaring symbols in BLOCK %a\n', n)
   END
END StartDeclareScope ;


(*
   EndDeclareScope -
*)

PROCEDURE EndDeclareScope ;
BEGIN
   (* no need to do anything *)
END EndDeclareScope ;


(*
   PreAddModGcc - adds a relationship between sym and t.
                  It also determines whether an unbounded
                  for sym is required and if so this is also
                  created.
*)

PROCEDURE PreAddModGcc (sym: CARDINAL; t: Tree) ;
BEGIN
   AddModGcc(sym, t)
END PreAddModGcc ;


(*
   DeclareDefaultType - declares a default type, sym, with, name.
*)

PROCEDURE DeclareDefaultType (sym: CARDINAL; name: ARRAY OF CHAR; gcctype: Tree) ;
VAR
   n        : Name ;
   t        : Tree ;
   high, low: CARDINAL ;
BEGIN
   (* DeclareDefaultType will declare a new identifier as a type of, gcctype, if it has not already been
      declared by gccgm2.c *)
   t := GetDefaultType(KeyToCharStar(MakeKey(name)), gcctype) ;
   AddModGcc(sym, t) ;
   IncludeElementIntoSet(FullyDeclared, sym) ;
   WalkAssociatedUnbounded(sym, TraverseDependants) ;
   (*
      this is very simplistic and assumes that the caller only uses Subranges, Sets and GCC types.
      We need to declare any constants with the types so that AllDependantsFullyDeclared works.
   *)
   IF IsSubrange(sym)
   THEN
      GetSubrange(sym, high, low) ;
      DeclareConstant(GetDeclared(sym), high) ;
      DeclareConstant(GetDeclared(sym), low)
   ELSIF IsSet(sym)
   THEN
      IF IsSubrange(GetType(sym))
      THEN
         IF NOT GccKnowsAbout(GetType(sym))
         THEN
            (* only true for internal types of course *)
            InternalError('subrange type within the set type must be declared before the set type', __FILE__, __LINE__)
         END ;
         GetSubrange(GetType(sym), high, low) ;
         DeclareConstant(GetDeclared(sym), high) ;
         DeclareConstant(GetDeclared(sym), low)
      ELSIF IsEnumeration(GetType(sym))
      THEN
         IF NOT GccKnowsAbout(GetType(sym))
         THEN
            (* only true for internal types of course *)
            InternalError('enumeration type within the set type must be declared before the set type', __FILE__, __LINE__)
         END
      END
   END
END DeclareDefaultType ;


(*
   DeclareBoolean - declares the Boolean type together with true and false.
*)

PROCEDURE DeclareBoolean ;
BEGIN
   AddModGcc(Boolean, GetBooleanType()) ;
   AddModGcc(True, GetBooleanTrue()) ;
   AddModGcc(False, GetBooleanFalse()) ;
   IncludeElementIntoSet(FullyDeclared, Boolean) ;
   IncludeElementIntoSet(FullyDeclared, True) ;
   IncludeElementIntoSet(FullyDeclared, False) ;
   WalkAssociatedUnbounded(Boolean, TraverseDependants)
END DeclareBoolean ;


(*
   DeclareFileName - declares the filename to the back end.
*)

PROCEDURE DeclareFileName ;
VAR
   ModuleName,
   FileName  : String ;
BEGIN
   ModuleName := InitStringCharStar(KeyToCharStar(GetSymName(GetMainModule()))) ;
   FileName   := CalculateFileName(ModuleName, Mark(InitString('mod'))) ;
   SetFileNameAndLineNo(string(FileName), 1) ;
   ModuleName := KillString(ModuleName) ;
   FileName   := KillString(FileName)
END DeclareFileName ;


(*
   DeclareFixedSizedType - declares the GNU Modula-2 fixed types
                           (if the back end support such a type).
*)

PROCEDURE DeclareFixedSizedType (name: ARRAY OF CHAR; type: CARDINAL; t: Tree) ;
VAR
   typetype,
   low, high: CARDINAL ;
BEGIN
   IF type#NulSym
   THEN
      IF IsSet(type) AND (NOT GccKnowsAbout(GetType(type)))
      THEN
         typetype := GetType(type) ;
         GetSubrange(typetype, high, low) ;
         DeclareConstant(GetDeclared(type), high) ;
         DeclareConstant(GetDeclared(type), low) ;
         PreAddModGcc(typetype, BuildSubrangeType(KeyToCharStar(GetFullSymName(typetype)),
                                                  Mod2Gcc(GetType(typetype)),
                                                  Mod2Gcc(low), Mod2Gcc(high))) ;
         IncludeElementIntoSet(FullyDeclared, typetype) ;
         WalkAssociatedUnbounded(typetype, TraverseDependants)
      END ;
      (* gcc back end supports, type *)
      DeclareDefaultType(type, name, t)
   END
END DeclareFixedSizedType ;


(*
   DeclareDefaultSimpleTypes - declares the simple types.
*)

PROCEDURE DeclareDefaultSimpleTypes ;
BEGIN
   AddModGcc(ZType, GetM2ZType()) ;
   AddModGcc(RType, GetM2RType()) ;
   AddModGcc(CType, GetM2CType()) ;
   IncludeElementIntoSet(FullyDeclared, ZType) ;
   IncludeElementIntoSet(FullyDeclared, RType) ;
   IncludeElementIntoSet(FullyDeclared, CType) ;

   DeclareDefaultType(Cardinal    , "CARDINAL"    , GetM2CardinalType()) ;
   DeclareDefaultType(Integer     , "INTEGER"     , GetM2IntegerType()) ;
   DeclareDefaultType(Char        , "CHAR"        , GetM2CharType()) ;
   DeclareDefaultType(Loc         , "LOC"         , GetISOLocType()) ;

   IF Iso
   THEN
      DeclareDefaultType(Byte     , "BYTE"        , GetISOByteType()) ;
      DeclareDefaultType(Word     , "WORD"        , GetISOWordType())
   ELSE
      DeclareDefaultType(Byte     , "BYTE"        , GetByteType()) ;
      DeclareDefaultType(Word     , "WORD"        , GetWordType())
   END ;
   
   DeclareDefaultType(Proc        , "PROC"        , GetProcType()) ;
   DeclareDefaultType(Address     , "ADDRESS"     , GetPointerType()) ;
   DeclareDefaultType(LongInt     , "LONGINT"     , GetM2LongIntType()) ;
   DeclareDefaultType(LongCard    , "LONGCARD"    , GetM2LongCardType()) ;
   DeclareDefaultType(ShortInt    , "SHORTINT"    , GetM2ShortIntType()) ;
   DeclareDefaultType(ShortCard   , "SHORTCARD"   , GetM2ShortCardType()) ;
   DeclareDefaultType(ShortReal   , "SHORTREAL"   , GetM2ShortRealType()) ;
   DeclareDefaultType(Real        , "REAL"        , GetM2RealType()) ;
   DeclareDefaultType(LongReal    , "LONGREAL"    , GetM2LongRealType()) ;
   DeclareDefaultType(Bitnum      , "BITNUM"      , GetBitnumType()) ;
   DeclareDefaultType(Bitset      , "BITSET"      , GetBitsetType()) ;
   DeclareDefaultType(Complex     , "COMPLEX"     , GetM2ComplexType()) ;
   DeclareDefaultType(LongComplex , "LONGCOMPLEX" , GetM2LongComplexType()) ;
   DeclareDefaultType(ShortComplex, "SHORTCOMPLEX", GetM2ShortComplexType()) ;

   DeclareBoolean ;

   DeclareFixedSizedType("INTEGER8"  , IntegerN(8)  , GetM2Integer8()) ;
   DeclareFixedSizedType("INTEGER16" , IntegerN(16) , GetM2Integer16()) ;
   DeclareFixedSizedType("INTEGER32" , IntegerN(32) , GetM2Integer32()) ;
   DeclareFixedSizedType("INTEGER64" , IntegerN(64) , GetM2Integer64()) ;
   DeclareFixedSizedType("CARDINAL8" , CardinalN(8) , GetM2Cardinal8()) ;
   DeclareFixedSizedType("CARDINAL16", CardinalN(16), GetM2Cardinal16()) ;
   DeclareFixedSizedType("CARDINAL32", CardinalN(32), GetM2Cardinal32()) ;
   DeclareFixedSizedType("CARDINAL64", CardinalN(64), GetM2Cardinal64()) ;
   DeclareFixedSizedType("WORD16"    , WordN(16)    , GetM2Word16()) ;
   DeclareFixedSizedType("WORD32"    , WordN(32)    , GetM2Word32()) ;
   DeclareFixedSizedType("WORD64"    , WordN(64)    , GetM2Word64()) ;
   DeclareFixedSizedType("BITSET8"   , SetN(8)      , GetM2Bitset8()) ;
   DeclareFixedSizedType("BITSET16"  , SetN(16)     , GetM2Bitset16()) ;
   DeclareFixedSizedType("BITSET32"  , SetN(32)     , GetM2Bitset32()) ;
   DeclareFixedSizedType("REAL32"    , RealN(32)    , GetM2Real32()) ;
   DeclareFixedSizedType("REAL64"    , RealN(64)    , GetM2Real64()) ;
   DeclareFixedSizedType("REAL96"    , RealN(96)    , GetM2Real96()) ;
   DeclareFixedSizedType("REAL128"   , RealN(128)   , GetM2Real128()) ;
   DeclareFixedSizedType("COMPLEX32" , ComplexN(32) , GetM2Complex32()) ;
   DeclareFixedSizedType("COMPLEX64" , ComplexN(64) , GetM2Complex64()) ;
   DeclareFixedSizedType("COMPLEX96" , ComplexN(96) , GetM2Complex96()) ;
   DeclareFixedSizedType("COMPLEX128", ComplexN(128), GetM2Complex128())
END DeclareDefaultSimpleTypes ;


(*
   DeclarePackedBoolean - 
*)

PROCEDURE DeclarePackedBoolean ;
VAR
   e: CARDINAL ;
BEGIN
   e := GetPackedEquivalent(Boolean) ;
   AddModGcc(e, GetPackedBooleanType()) ;
   IncludeElementIntoSet(FullyDeclared, e)
END DeclarePackedBoolean ;


(*
   DeclarePackedDefaultSimpleTypes - 
*)

PROCEDURE DeclarePackedDefaultSimpleTypes ;
BEGIN
   DeclarePackedBoolean
END DeclarePackedDefaultSimpleTypes ;


(*
   DeclareDefaultTypes - makes default types known to GCC
*)

PROCEDURE DeclareDefaultTypes ;
BEGIN
   IF NOT HaveInitDefaultTypes
   THEN
      HaveInitDefaultTypes := TRUE ;
      DeclareDefaultSimpleTypes ;
      DeclarePackedDefaultSimpleTypes
   END
END DeclareDefaultTypes ;


(*
   DeclareDefaultConstants - make default constants known to GCC
*)

PROCEDURE DeclareDefaultConstants ;
BEGIN
   AddModGcc(Nil, GetPointerZero()) ;
   IncludeElementIntoSet(FullyDeclared, Nil)
END DeclareDefaultConstants ;


(*
   AlignDeclarationWithSource - given a symbol, sym, set the source file and line
                                number with the declaration position of sym.
*)

PROCEDURE AlignDeclarationWithSource (sym: CARDINAL) ;
VAR
   s: String ;
   t: CARDINAL ;
BEGIN
   t := GetDeclared(sym) ;
   s := FindFileNameFromToken(t, 0) ;
   SetFileNameAndLineNo(string(s), TokenToLineNo(t, 0))
END AlignDeclarationWithSource ;


(*
   FindTreeScope - returns the scope where the symbol
                   should be created.

                   Symbols created in a module will
                   return NIL trees, but symbols created
                   in a module which is declared inside
                   a procedure will return the procedure Tree.
*)

PROCEDURE FindTreeScope (sym: CARDINAL) : Tree ;
BEGIN
   sym := GetProcedureScope(sym) ;
   IF sym=NulSym
   THEN
      RETURN( NIL )
   ELSE
      RETURN( Mod2Gcc(sym) )
   END
END FindTreeScope ;


(*
   IsEffectivelyImported - returns TRUE if symbol, Sym, was
                           effectively imported into ModSym.
*)

PROCEDURE IsEffectivelyImported (ModSym, sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          IsImported(ModSym, sym) OR
          (IsImported(ModSym, GetModuleWhereDeclared(sym)) AND
           IsExported(GetModuleWhereDeclared(sym), sym))
         )
END IsEffectivelyImported ;


(*
   FindOuterModule - returns the out most module where, sym,
                     was declared.  It returns NulSym if the
                     symbol or the module was declared inside
                     a procedure.
*)

PROCEDURE FindOuterModule (sym: CARDINAL) : CARDINAL ;
BEGIN
   sym := GetScope(sym) ;
   WHILE (NOT IsDefImp(sym)) DO
      IF IsModule(sym)
      THEN
         IF GetScope(sym)=NulSym
         THEN
            RETURN( sym )
         ELSE
            sym := GetScope(sym)
         END
      ELSIF IsProcedure(sym)
      THEN
         sym := GetScope(sym)
      END
   END ;
   RETURN( sym )
END FindOuterModule ;


(*
   DoVariableDeclaration - 
*)

PROCEDURE DoVariableDeclaration (var: CARDINAL; name: ADDRESS;
                                 isImported, isExported,
                                 isTemporary, isGlobal: BOOLEAN;
                                 scope: Tree) ;
VAR
   type   : Tree ;
   varType: CARDINAL ;
BEGIN
   IF IsComponent(var)
   THEN
      RETURN
   END ;
   IF GetMode(var)=LeftValue
   THEN
      (*
        There are two issues to deal with:

        (i)   LeftValue is really a pointer to GetType(Son)
        (ii)  Front end might have specified the back end use a particular
              data type - in which case we might tell gcc exactly this.
              We do not add an extra pointer if this is the case.
      *)
      varType := GetVarBackEndType(var) ;
      IF varType=NulSym
      THEN
         (* we have not explicity told back end the type, so build it *)
         varType := GetType(var) ;
         IF IsVariableAtAddress(var)
         THEN
            type := BuildConstPointerType(Mod2Gcc(varType))
         ELSE
            type := BuildPointerType(Mod2Gcc(varType))
         END
      ELSE
         type := Mod2Gcc(varType)
      END ;
      Assert(AllDependantsFullyDeclared(varType))
   ELSE
      type := Mod2Gcc(GetType(var))
   END ;
   PreAddModGcc(var, DeclareKnownVariable(name, type,
                                          isExported, isImported, isTemporary,
                                          isGlobal, scope)) ;
   WatchRemoveList(var, todolist) ;
   WatchIncludeList(var, fullydeclared)
END DoVariableDeclaration ;


(*
   DeclareVariable - declares a global variable to GCC.
*)

PROCEDURE DeclareVariable (ModSym, Son: CARDINAL) ;
VAR
   scope: Tree ;
   decl : CARDINAL ;
BEGIN
   IF NOT GccKnowsAbout(Son)
   THEN
      AlignDeclarationWithSource(Son) ;
      scope := FindTreeScope(ModSym) ;
      decl := FindOuterModule(Son) ;
      Assert(AllDependantsFullyDeclared(GetType(Son))) ;
      DoVariableDeclaration(Son,
                            KeyToCharStar(GetFullSymName(Son)),
                            (* in Modula-2 we are allowed to import from ourselves, but we do not present this to GCC *)
                            IsEffectivelyImported(ModSym, Son) AND (GetMainModule()#decl),
                            IsExported(ModSym, Son),
                            IsTemporary(Son),
                            GetMainModule()=decl,
                            scope)
   END
END DeclareVariable ;


(*
   DeclareGlobalVariables - lists the Global variables for
                            Module ModSym together with their offset.
*)

PROCEDURE DeclareGlobalVariables (ModSym: CARDINAL) ;
VAR
   o, s,
   n, Son: CARDINAL ;
BEGIN
   n := 1 ;
   Son := GetNth(ModSym, n) ;
   o := 0 ;
   WHILE Son#NulSym DO
      DeclareVariable(ModSym, Son) ;
      INC(n) ;
      Son := GetNth(ModSym, n)
   END ;
   ForeachInnerModuleDo(ModSym, DeclareGlobalVariables)
END DeclareGlobalVariables ;


(*
   DeclareImportedVariables - declares all imported variables to GM2.
*)

PROCEDURE DeclareImportedVariables (sym: WORD) ;
BEGIN
   IF IsVar(sym)
   THEN
      DeclareVariable(GetMainModule(), sym)
   ELSIF IsDefImp(sym)
   THEN
      ForeachExportedDo(sym, DeclareImportedVariables)
   END
END DeclareImportedVariables ;


(*
   DeclareLocalVariables - declares Local variables for procedure Sym.
*)

PROCEDURE DeclareLocalVariables (sym: CARDINAL) ;
VAR
   i, Var: CARDINAL ;
BEGIN
   i := NoOfParam(sym)+1 ;
   Var := GetNth(sym, i) ;
   WHILE Var#NulSym DO
      AlignDeclarationWithSource(Var) ;
      Assert(AllDependantsFullyDeclared(Var)) ;
      DoVariableDeclaration(Var,
                            KeyToCharStar(GetFullSymName(Var)),
                            FALSE,  (* local variables cannot be imported *)
                            FALSE,  (* or exported *)
                            IsTemporary(Var),
                            FALSE,  (* and are not global *)
                            Mod2Gcc(sym)) ;
      INC(i) ;
      Var := GetNth(sym, i)
   END
END DeclareLocalVariables ;


(*
   DeclareModuleVariables - declares Module variables for a module
                            which was declared inside a procedure.
*)

PROCEDURE DeclareModuleVariables (sym: CARDINAL) ;
VAR
   scope : Tree ;
   i, Var: CARDINAL ;
BEGIN
   i := 1 ;
   scope := Mod2Gcc(GetProcedureScope(sym)) ;
   Var := GetNth(sym, i) ;
   WHILE Var#NulSym DO
      AlignDeclarationWithSource(Var) ;
      Assert(AllDependantsFullyDeclared(GetType(Var))) ;
      DoVariableDeclaration(Var,
                            KeyToCharStar(GetFullSymName(Var)),
                            FALSE,   (* inner module variables cannot be imported *)
                            FALSE,   (* or exported (as far as GCC is concerned)  *)
                            IsTemporary(Var),
                            FALSE,   (* and are not global *)
                            scope) ;
      INC(i) ;
      Var := GetNth(sym, i)
   END
END DeclareModuleVariables ;


(*
   DeclareFieldValue - 
*)

PROCEDURE DeclareFieldValue (sym: CARDINAL; value: Tree; VAR list: Tree) : Tree ;
BEGIN
   IF (GetModuleWhereDeclared(sym)=NulSym) OR
      (GetModuleWhereDeclared(sym)=GetMainModule())
   THEN
      RETURN( BuildEnumerator(KeyToCharStar(GetSymName(sym)), value, list) )
   ELSE
      RETURN( BuildEnumerator(KeyToCharStar(GetFullScopeAsmName(sym)), value, list) )
   END
END DeclareFieldValue ;


(*
   DeclareFieldEnumeration - declares an enumerator within the current enumeration type.
*)

PROCEDURE DeclareFieldEnumeration (sym: WORD) : Tree ;
VAR
   type    : CARDINAL ;
   field,
   enumlist: Tree ;
BEGIN
   (* add relationship between gccSym and sym *)
   type := GetType(sym) ;
   enumlist := GetEnumList(type) ;
   PushValue(sym) ;
   field := DeclareFieldValue(sym, PopIntegerTree(), enumlist) ;
   PutEnumList(type, enumlist) ;
   RETURN( field )
END DeclareFieldEnumeration ;


(*
   DeclareEnumeration - declare an enumerated type.
*)

PROCEDURE DeclareEnumeration (sym: WORD) : Tree ;
VAR
   enumlist,
   gccenum : Tree ;
BEGIN
   gccenum := BuildStartEnumeration(KeyToCharStar(GetFullSymName(sym)), FALSE) ;
   enumlist := GetEnumList(sym) ;
   RETURN( BuildEndEnumeration(gccenum, enumlist) )
END DeclareEnumeration ;


(*
   DeclareSubrange - declare a subrange type.
*)

PROCEDURE DeclareSubrange (sym: CARDINAL) : Tree ;
VAR
   gccsym   : Tree ;
   high, low: CARDINAL ;
BEGIN
   GetSubrange(sym, high, low) ;
   gccsym := BuildSubrangeType(KeyToCharStar(GetFullSymName(sym)),
                               Mod2Gcc(GetType(sym)), Mod2Gcc(low), Mod2Gcc(high)) ;
   RETURN( gccsym )
END DeclareSubrange ;


(*
   IncludeGetNth - 
*)

PROCEDURE IncludeGetNth (l: List; sym: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   printf0(' ListOfSons [') ;
   i := 1 ;
   WHILE GetNth(sym, i)#NulSym DO
      IF i>1
      THEN
         printf0(', ') ;
      END ;
      IncludeItemIntoList(l, GetNth(sym, i)) ;
      PrintTerse(GetNth(sym, i)) ;
      INC(i)
   END ;
   printf0(']')
END IncludeGetNth ;


(*
   IncludeType - 
*)

PROCEDURE IncludeType (l: List; sym: CARDINAL) ;
VAR
   t: CARDINAL ;
BEGIN
   t := GetType(sym) ;
   IF t#NulSym
   THEN
      printf0(' type [') ;
      PrintTerse(t) ;
      IncludeItemIntoList(l, t) ;
      printf0(']') ;
      t := GetVarBackEndType(sym) ;
      IF t#NulSym
      THEN
         printf0(' gcc type [') ;
         PrintTerse(t) ;
         IncludeItemIntoList(l, t) ;
         printf0(']')
      END
   END
END IncludeType ;


(*
   IncludeSubscript - 
*)

PROCEDURE IncludeSubscript (l: List; sym: CARDINAL) ;
VAR
   t: CARDINAL ;
BEGIN
   t := GetArraySubscript(sym) ;
   IF t#NulSym
   THEN
      printf0(' subrange [') ;
      PrintTerse(t) ;
      IncludeItemIntoList(l, t) ;
      printf0(']') ;
   END
END IncludeSubscript ;


(*
   PrintLocalSymbol - 
*)

PROCEDURE PrintLocalSymbol (sym: CARDINAL) ;
BEGIN
   PrintTerse(sym) ; printf0(', ')
END PrintLocalSymbol ;


(*
   PrintLocalSymbols - 
*)

PROCEDURE PrintLocalSymbols (sym: CARDINAL) ;
BEGIN
   printf0('Local Symbols {') ;
   ForeachLocalSymDo(sym, PrintLocalSymbol) ;
   printf0('}')
END PrintLocalSymbols ;


(*
   IncludeGetVarient - 
*)

PROCEDURE IncludeGetVarient (l: List; sym: CARDINAL) ;
BEGIN
   IF GetVarient(sym)#NulSym
   THEN
      printf0(' Varient [') ;
      PrintTerse(GetVarient(sym)) ;
      printf0(']') ;
      IncludeItemIntoList(l, GetVarient(sym))
   END
END IncludeGetVarient ;


(*
   IncludeUnbounded - includes the record component of an unbounded type.
*)

PROCEDURE IncludeUnbounded (l: List; sym: CARDINAL) ;
BEGIN
   IF GetUnboundedRecordType(sym)#NulSym
   THEN
      IncludeItemIntoList(l, GetUnboundedRecordType(sym))
   END
END IncludeUnbounded ;


(*
   IncludePartialUnbounded - includes the type component of a partial unbounded symbol.
*)

PROCEDURE IncludePartialUnbounded (l: List; sym: CARDINAL) ;
BEGIN
   IF GetType(sym)#NulSym
   THEN
      IncludeItemIntoList(l, GetType(sym))
   END
END IncludePartialUnbounded ;


(*
   PrintDeclared - prints out where, sym, was declared.
*)

PROCEDURE PrintDeclared (sym: CARDINAL) ;
VAR
   filename: String ;
   lineno,
   tokenno : CARDINAL ;
BEGIN
   tokenno := GetDeclared(sym) ;
   filename := FindFileNameFromToken(tokenno, 0) ;
   lineno := TokenToLineNo(tokenno, 0) ;
   printf2(" declared in %s:%d", filename, lineno)
END PrintDeclared ;


(*
   PrintAlignment - 
*)

PROCEDURE PrintAlignment (sym: CARDINAL) ;
VAR
   align: CARDINAL ;
BEGIN
   IF IsRecord(sym) OR IsType(sym) OR IsRecordField(sym) OR IsPointer(sym) OR IsArray(sym)
   THEN
      align := GetAlignment(sym) ;
      IF align#NulSym
      THEN
         printf1(" aligned [%d]", align)
      END
   END
END PrintAlignment ;


(*
   IncludeGetParent - 
*)

PROCEDURE IncludeGetParent (l: List; sym: CARDINAL) ;
BEGIN
   printf0(' Parent [') ;
   IncludeItemIntoList(l, GetParent(sym)) ;
   PrintTerse(GetParent(sym)) ;
   printf0(']')
END IncludeGetParent ;


(*
   PrintDecl - 
*)

PROCEDURE PrintDecl (sym: CARDINAL) ;
BEGIN
   IF IsDeclaredPackedResolved(sym)
   THEN
      IF IsDeclaredPacked(sym)
      THEN
         printf0(' packed')
      ELSE
         printf0(' unpacked')
      END
   ELSE
      printf0(' unknown if packed')
   END
END PrintDecl ;


(*
   PrintVerboseFromList - prints the, i, th element in the list, l.
*)

PROCEDURE PrintVerboseFromList (l: List; i: CARDINAL) ;
VAR
   type,
   low,
   high,
   sym  : CARDINAL ;
   n, n2: Name ;
BEGIN
   sym := GetItemFromList(l, i) ;
   n := GetSymName(sym) ;
   IF IsError(sym)
   THEN
      printf2('sym %d IsError (%a)', sym, n)
   ELSIF IsDefImp(sym)
   THEN
      printf2('sym %d IsDefImp (%a)', sym, n) ;
      IF IsDefinitionForC(sym)
      THEN
         printf0('and IsDefinitionForC')
      END ;
      IF IsHiddenTypeDeclared(sym)
      THEN
         printf0(' IsHiddenTypeDeclared')
      END
   ELSIF IsModule(sym)
   THEN
      printf2('sym %d IsModule (%a)', sym, n) ;
      IF IsModuleWithinProcedure(sym)
      THEN
         printf0(' and IsModuleWithinProcedure')
      END
   ELSIF IsInnerModule(sym)
   THEN
      printf2('sym %d IsInnerModule (%a)', sym, n)
   ELSIF IsUnknown(sym)
   THEN
      printf2('sym %d IsUnknown (%a)', sym, n)
   ELSIF IsType(sym)
   THEN
      printf2('sym %d IsType (%a)', sym, n) ;
      IncludeType(l, sym) ;
      PrintAlignment(sym)
   ELSIF IsProcedure(sym)
   THEN
      printf2('sym %d IsProcedure (%a)', sym, n);
      IF IsProcedureReachable(sym)
      THEN
         printf0(' and IsProcedureReachable')
      END ;
      PrintDeclared(sym)
   ELSIF IsParameter(sym)
   THEN
      printf2('sym %d IsParameter (%a)', sym, n) ;
      IF GetParameterShadowVar(sym)=NulSym
      THEN
         printf0(' no shadow local variable')
      ELSE
         printf0(' shadow ') ;
         IncludeType(l, GetParameterShadowVar(sym))
         (* PrintVerboseFromList(l, GetParameterShadowVar(sym)) *)
      END ;
      IncludeType(l, sym)
   ELSIF IsPointer(sym)
   THEN
      printf2('sym %d IsPointer (%a)', sym, n) ;
      IncludeType(l, sym) ;
      PrintAlignment(sym)
   ELSIF IsRecord(sym)
   THEN
      printf2('sym %d IsRecord (%a)', sym, n) ;
      PrintLocalSymbols(sym) ;
      IncludeGetNth(l, sym) ;
      PrintAlignment(sym) ;
      PrintDecl(sym)
   ELSIF IsVarient(sym)
   THEN
      printf2('sym %d IsVarient (%a)', sym, n) ;
      PrintDecl(sym) ;
      IncludeGetNth(l, sym) ;
      IncludeGetVarient(l, sym) ;
      IncludeGetParent(l, sym)
   ELSIF IsFieldVarient(sym)
   THEN
      printf2('sym %d IsFieldVarient (%a)', sym, n) ;
      PrintDecl(sym) ;
      IncludeGetNth(l, sym) ;
      IncludeGetVarient(l, sym) ;
      IncludeGetParent(l, sym)
   ELSIF IsFieldEnumeration(sym)
   THEN
      printf2('sym %d IsFieldEnumeration (%a)', sym, n)
   ELSIF IsArray(sym)
   THEN
      printf2('sym %d IsArray (%a)', sym, n) ;
      IncludeSubscript(l, sym) ;
      IncludeType(l, sym) ;
      PrintAlignment(sym)
   ELSIF IsEnumeration(sym)
   THEN
      printf2('sym %d IsEnumeration (%a)', sym, n)
   ELSIF IsSet(sym)
   THEN
      printf2('sym %d IsSet (%a)', sym, n) ;
      IncludeType(l, sym)
   ELSIF IsUnbounded(sym)
   THEN
      printf2('sym %d IsUnbounded (%a)', sym, n) ;
      IncludeUnbounded(l, sym)
   ELSIF IsPartialUnbounded(sym)
   THEN
      printf2('sym %d IsPartialUnbounded (%a)', sym, n) ;
      IncludePartialUnbounded(l, sym)
   ELSIF IsRecordField(sym)
   THEN
      printf2('sym %d IsRecordField (%a)', sym, n) ;
      IF IsRecordFieldAVarientTag(sym)
      THEN
         printf0(' variant tag')
      END ;
      IncludeType(l, sym) ;
      IncludeGetVarient(l, sym) ;
      IncludeGetParent(l, sym) ;
      PrintAlignment(sym) ;
      PrintDecl(sym)
   ELSIF IsProcType(sym)
   THEN
      printf2('sym %d IsProcType (%a)', sym, n)
   ELSIF IsVar(sym)
   THEN
      n2 := GetSymName(GetScope(sym)) ;
      printf3('sym %d IsVar (%a) declared in scope %a mode ', sym, n, n2) ;
      CASE GetMode(sym) OF

      LeftValue     : printf0('l ') |
      RightValue    : printf0('r ') |
      ImmediateValue: printf0('i ') |
      NoValue       : printf0('n ')

      END ;
      IF IsTemporary(sym)
      THEN
         printf0('temporary ')
      END ;
      IF IsComponent(sym)
      THEN
         printf0('component ')
      END ;
      IncludeType(l, sym)
   ELSIF IsConst(sym)
   THEN
      printf2('sym %d IsConst (%a)', sym, n) ;
      IF IsConstructor(sym)
      THEN
         printf0(' constant constructor ')
      ELSIF IsConstSet(sym)
      THEN
         printf0(' constant constructor set ')
      END ;
      IncludeType(l, sym)
   ELSIF IsConstructor(sym)
   THEN
      printf2('sym %d IsConstructor (non constant) (%a)', sym, n) ;
      IncludeType(l, sym)
   ELSIF IsConstString(sym)
   THEN
      printf2('sym %d IsConstString (%a)', sym, n)
   ELSIF IsConstLit(sym)
   THEN
      printf2('sym %d IsConstLit (%a)', sym, n)
   ELSIF IsDummy(sym)
   THEN
      printf2('sym %d IsDummy (%a)', sym, n)
   ELSIF IsTemporary(sym)
   THEN
      printf2('sym %d IsTemporary (%a)', sym, n)
   ELSIF IsVarAParam(sym)
   THEN
      printf2('sym %d IsVarAParam (%a)', sym, n)
   ELSIF IsSubscript(sym)
   THEN
      printf2('sym %d IsSubscript (%a)', sym, n)
   ELSIF IsSubrange(sym)
   THEN
      GetSubrange(sym, high, low) ;
      printf2('sym %d IsSubrange (%a)', sym, n) ;
      IF (low#NulSym) AND (high#NulSym)
      THEN
         type := GetType(sym) ;
         IF type#NulSym
         THEN
            IncludeType(l, sym) ;
            n := GetSymName(type) ;
            printf1(' %a', n)
         END ;
         n := GetSymName(low) ;
         n2 := GetSymName(high) ;
         printf2('[%a..%a]', n, n2)
      END
   ELSIF IsProcedureVariable(sym)
   THEN
      printf2('sym %d IsProcedureVariable (%a)', sym, n)
   ELSIF IsProcedureNested(sym)
   THEN
      printf2('sym %d IsProcedureNested (%a)', sym, n)
   ELSIF IsAModula2Type(sym)
   THEN
      printf2('sym %d IsAModula2Type (%a)', sym, n)
   ELSIF IsObject(sym)
   THEN
      printf2('sym %d IsObject (%a)', sym, n)
   ELSIF IsTuple(sym)
   THEN
      printf2('sym %d IsTuple (%a)', sym, n) ;
      low := GetNth(sym, 1) ;
      high := GetNth(sym, 2) ;
      printf2('%d, %d\n', low, high)      
   ELSIF IsGnuAsm(sym)
   THEN
      IF IsGnuAsmVolatile(sym)
      THEN
         printf2('sym %d IsGnuAsmVolatile (%a)', sym, n)
      ELSE
         printf2('sym %d IsGnuAsm (%a)', sym, n)
      END
   ELSIF IsComponent(sym)
   THEN
      printf2('sym %d IsComponent (%a) ', sym, n) ;
      i := 1 ;
      REPEAT
         type := GetNth(sym, i) ;
         IF type#NulSym
         THEN
            IncludeItemIntoList(l, type) ;
            n := GetSymName(type) ;
            printf2("[%a %d] ", n, type) ;
            INC(i)
         END ;
      UNTIL type=NulSym
   END ;

   IF IsHiddenType(sym)
   THEN
      printf0(' IsHiddenType')
   END ;
   printf0('\n')
END PrintVerboseFromList ;


(*
   PrintVerbose - prints limited information about a symbol.
*)

PROCEDURE PrintVerbose (sym: CARDINAL) ;
VAR
   l: List ;
   i: CARDINAL ;
BEGIN
   InitList(l) ;
   IncludeItemIntoList(l, sym) ;
   i := 1 ;
   WHILE i<=NoOfItemsInList(l) DO
      PrintVerboseFromList(l, i) ;
      INC(i)
   END ;
   KillList(l)
END PrintVerbose ;


(*
   PrintSymbol - prints limited information about a symbol.
*)

PROCEDURE PrintSymbol (sym: CARDINAL) ;
BEGIN
   PrintTerse(sym) ;
   printf0('\n')
END PrintSymbol ;


(*
   PrintTerse - 
*)

PROCEDURE PrintTerse (sym: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   n := GetSymName(sym) ;
   IF IsError(sym)
   THEN
      printf2('sym %d IsError (%a)', sym, n)
   ELSIF IsDefImp(sym)
   THEN
      printf2('sym %d IsDefImp (%a)', sym, n) ;
      IF IsDefinitionForC(sym)
      THEN
         printf0('and IsDefinitionForC')
      END ;
      IF IsHiddenTypeDeclared(sym)
      THEN
         printf0(' IsHiddenTypeDeclared')
      END
   ELSIF IsModule(sym)
   THEN
      printf2('sym %d IsModule (%a)', sym, n) ;
      IF IsModuleWithinProcedure(sym)
      THEN
         printf0(' and IsModuleWithinProcedure')
      END
   ELSIF IsInnerModule(sym)
   THEN
      printf2('sym %d IsInnerModule (%a)', sym, n)
   ELSIF IsUnknown(sym)
   THEN
      printf2('sym %d IsUnknown (%a)', sym, n)
   ELSIF IsType(sym)
   THEN
      printf2('sym %d IsType (%a)', sym, n)
   ELSIF IsProcedure(sym)
   THEN
      printf2('sym %d IsProcedure (%a)', sym, n);
      IF IsProcedureReachable(sym)
      THEN
         printf0(' and IsProcedureReachable')
      END
   ELSIF IsParameter(sym)
   THEN
      printf2('sym %d IsParameter (%a)', sym, n)
   ELSIF IsPointer(sym)
   THEN
      printf2('sym %d IsPointer (%a)', sym, n)
   ELSIF IsRecord(sym)
   THEN
      printf2('sym %d IsRecord (%a)', sym, n)
   ELSIF IsVarient(sym)
   THEN
      printf2('sym %d IsVarient (%a)', sym, n)
   ELSIF IsFieldVarient(sym)
   THEN
      printf2('sym %d IsFieldVarient (%a)', sym, n)
   ELSIF IsFieldEnumeration(sym)
   THEN
      printf2('sym %d IsFieldEnumeration (%a)', sym, n)
   ELSIF IsArray(sym)
   THEN
      printf2('sym %d IsArray (%a)', sym, n)
   ELSIF IsEnumeration(sym)
   THEN
      printf2('sym %d IsEnumeration (%a)', sym, n)
   ELSIF IsSet(sym)
   THEN
      printf2('sym %d IsSet (%a)', sym, n)
   ELSIF IsUnbounded(sym)
   THEN
      printf2('sym %d IsUnbounded (%a)', sym, n)
   ELSIF IsRecordField(sym)
   THEN
      printf2('sym %d IsRecordField (%a)', sym, n)
   ELSIF IsProcType(sym)
   THEN
      printf2('sym %d IsProcType (%a)', sym, n)
   ELSIF IsVar(sym)
   THEN
      printf2('sym %d IsVar (%a)', sym, n)
   ELSIF IsConst(sym)
   THEN
      printf2('sym %d IsConst (%a)', sym, n)
   ELSIF IsConstString(sym)
   THEN
      printf2('sym %d IsConstString (%a)', sym, n)
   ELSIF IsConstLit(sym)
   THEN
      printf2('sym %d IsConstLit (%a)', sym, n)
   ELSIF IsDummy(sym)
   THEN
      printf2('sym %d IsDummy (%a)', sym, n)
   ELSIF IsTemporary(sym)
   THEN
      printf2('sym %d IsTemporary (%a)', sym, n)
   ELSIF IsVarAParam(sym)
   THEN
      printf2('sym %d IsVarAParam (%a)', sym, n)
   ELSIF IsSubscript(sym)
   THEN
      printf2('sym %d IsSubscript (%a)', sym, n)
   ELSIF IsSubrange(sym)
   THEN
      printf2('sym %d IsSubrange (%a)', sym, n)
   ELSIF IsProcedureVariable(sym)
   THEN
      printf2('sym %d IsProcedureVariable (%a)', sym, n)
   ELSIF IsProcedureNested(sym)
   THEN
      printf2('sym %d IsProcedureNested (%a)', sym, n)
   ELSIF IsAModula2Type(sym)
   THEN
      printf2('sym %d IsAModula2Type (%a)', sym, n)
   ELSIF IsGnuAsmVolatile(sym)
   THEN
      printf2('sym %d IsGnuAsmVolatile (%a)', sym, n)
   END ;

   IF IsHiddenType(sym)
   THEN
      printf0(' IsHiddenType')
   END
END PrintTerse ;


(*
   CheckAlignment - 
*)

PROCEDURE CheckAlignment (type: Tree; sym: CARDINAL) : Tree ;
VAR
   align: CARDINAL ;
BEGIN
   align := GetAlignment(sym) ;
   IF align#NulSym
   THEN
      PushInt(0) ;
      PushValue(align) ;
      IF NOT Equ(GetDeclared(sym))
      THEN
         RETURN( SetAlignment(type, Mod2Gcc(GetAlignment(sym))) )
      END
   END ;
   RETURN( type )
END CheckAlignment ;


(*
   CheckPragma - 
*)

PROCEDURE CheckPragma (type: Tree; sym: CARDINAL) : Tree ;
VAR
   align: CARDINAL ;
BEGIN
   align := GetAlignment(sym) ;
   IF IsDeclaredPacked(sym)
   THEN
      IF IsRecordField(sym) OR IsFieldVarient(sym)
      THEN
         type := SetDeclPacked(type)
      ELSIF IsRecord(sym) OR IsVarient(sym)
      THEN
         type := SetTypePacked(type)
      END
   END ;
   RETURN( CheckAlignment(type, sym) )
END CheckPragma ;


(*
   IsZero - returns TRUE if symbol, sym, is zero.
*)

PROCEDURE IsZero (sym: CARDINAL) : BOOLEAN ;
BEGIN
   PushIntegerTree(Mod2Gcc(sym)) ;
   PushInt(0) ;
   RETURN( Equ(GetDeclared(sym)) )
END IsZero ;


(*
   SetFieldPacked - sets Varient, VarientField and RecordField symbols
                    as packed.
*)

PROCEDURE SetFieldPacked (field: CARDINAL) ;
BEGIN
   IF IsVarient(field) OR IsFieldVarient(field) OR IsRecordField(field)
   THEN
      PutDeclaredPacked(field, TRUE)
   END
END SetFieldPacked ;


(*
   RecordPacked - indicates that record, sym, and its fields
                  are all packed.
*)

PROCEDURE RecordPacked (sym: CARDINAL) ;
BEGIN
   PutDeclaredPacked(sym, TRUE) ;
   WalkRecordDependants(sym, SetFieldPacked)
END RecordPacked ;


(*
   SetFieldNotPacked - sets Varient, VarientField and RecordField symbols
                       as not packed.
*)

PROCEDURE SetFieldNotPacked (field: CARDINAL) ;
BEGIN
   IF IsVarient(field) OR IsFieldVarient(field) OR IsRecordField(field)
   THEN
      PutDeclaredPacked(field, FALSE)
   END
END SetFieldNotPacked ;


(*
   RecordNotPacked - indicates that record, sym, and its fields
                     are all not packed.
*)

PROCEDURE RecordNotPacked (sym: CARDINAL) ;
BEGIN
   PutDeclaredPacked(sym, FALSE) ;
   WalkRecordDependants(sym, SetFieldNotPacked)
END RecordNotPacked ;


(*
   DetermineIfRecordPacked - 
*)

PROCEDURE DetermineIfRecordPacked (sym: CARDINAL) ;
VAR
   defaultAlignment: CARDINAL ;
BEGIN
   defaultAlignment := GetDefaultRecordFieldAlignment(sym) ;
   IF (defaultAlignment#NulSym) AND IsZero(defaultAlignment)
   THEN
      RecordPacked(sym)
   ELSE
      RecordNotPacked(sym)
   END
END DetermineIfRecordPacked ;


(*
   DeclarePackedSubrange - 
*)

PROCEDURE DeclarePackedSubrange (equiv, sym: CARDINAL) ;
VAR
   type,
   gccsym   : Tree ;
   high, low: CARDINAL ;
BEGIN
   GetSubrange(sym, high, low) ;
   type := BuildSmallestTypeRange(Mod2Gcc(low), Mod2Gcc(high)) ;
   gccsym := BuildSubrangeType(KeyToCharStar(GetFullSymName(sym)),
                               type, Mod2Gcc(low), Mod2Gcc(high)) ;
   AddModGcc(equiv, gccsym)
END DeclarePackedSubrange ;


(*
   DeclarePackedSet - 
*)

PROCEDURE DeclarePackedSet (equiv, sym: CARDINAL) ;
VAR
   highLimit,
   range,
   gccsym   : Tree ;
   type,
   high, low: CARDINAL ;
BEGIN
   Assert(IsSet(sym)) ;
   type := SkipType(GetType(sym)) ;
   low := GetTypeMin(type) ;
   high := GetTypeMax(type) ;
   highLimit := BuildSub(Mod2Gcc(high), Mod2Gcc(low), FALSE) ;
   (* --fixme-- we need to check that low <= WORDLENGTH *)
   highLimit := BuildLSL(GetIntegerOne(), highLimit, FALSE) ;
   range := BuildSmallestTypeRange(GetIntegerZero(), highLimit) ;
   gccsym := BuildSubrangeType(KeyToCharStar(GetFullSymName(sym)),
                               range, GetIntegerZero(), highLimit) ;
   AddModGcc(equiv, gccsym)
END DeclarePackedSet ;


(*
   DeclareFieldEnumeration - declares an enumerator within the current enumeration type.
*)

PROCEDURE DeclarePackedFieldEnumeration (sym: WORD) ;
VAR
   equiv,
   type    : CARDINAL ;
   field,
   enumlist: Tree ;
BEGIN
   (* add relationship between gccSym and sym *)
   type := GetType(sym) ;
   equiv := GetPackedEquivalent(type) ;
   enumlist := GetEnumList(equiv) ;
   PushValue(sym) ;
   field := DeclareFieldValue(sym, PopIntegerTree(), enumlist) ;
   PutEnumList(equiv, enumlist)
END DeclarePackedFieldEnumeration ;


(*
   DeclarePackedEnumeration - 
*)

PROCEDURE DeclarePackedEnumeration (equiv, sym: CARDINAL) ;
VAR
   enumlist,
   gccenum : Tree ;
BEGIN
   gccenum := BuildStartEnumeration(KeyToCharStar(GetFullSymName(sym)), TRUE) ;
   ForeachLocalSymDo(sym, DeclarePackedFieldEnumeration) ;
   enumlist := GetEnumList(equiv) ;
   gccenum := BuildEndEnumeration(gccenum, enumlist) ;
   AddModGcc(equiv, gccenum)
END DeclarePackedEnumeration ;


(*
   DeclarePackedType - 
*)

PROCEDURE DeclarePackedType (equiv, sym: CARDINAL) ;
VAR
   type: CARDINAL ;
BEGIN
   type := GetType(sym) ;
   IF type=NulSym
   THEN
      IF sym=Boolean
      THEN
         AddModGcc(equiv, GetPackedBooleanType())
      ELSE
         AddModGcc(equiv, Mod2Gcc(sym))
      END
   ELSE
      DeclarePackedType(GetPackedEquivalent(type), type) ;
      AddModGcc(equiv, Mod2Gcc(GetPackedEquivalent(type)))
   END
END DeclarePackedType ;


(*
   doDeclareEquivalent -
*)

PROCEDURE doDeclareEquivalent (sym: CARDINAL; p: doDeclareProcedure) : Tree ;
VAR
   type,
   equiv: CARDINAL ;
BEGIN
   equiv := GetPackedEquivalent(sym) ;
   IF NOT GccKnowsAbout(equiv)
   THEN
      p(equiv, sym) ;
      IncludeElementIntoSet(FullyDeclared, equiv)
   END ;
   RETURN( Mod2Gcc(equiv) )
END doDeclareEquivalent ;


(*
   PossiblyPacked - 
*)

PROCEDURE PossiblyPacked (sym: CARDINAL; isPacked: BOOLEAN) : Tree ;
BEGIN
   IF isPacked
   THEN
      IF IsSubrange(sym)
      THEN
         RETURN( doDeclareEquivalent(sym, DeclarePackedSubrange) )
      ELSIF IsType(sym)
      THEN
         RETURN( doDeclareEquivalent(sym, DeclarePackedType) )
      ELSIF IsEnumeration(sym)
      THEN
         RETURN( doDeclareEquivalent(sym, DeclarePackedEnumeration) )
      ELSIF IsSet(sym)
      THEN
         RETURN( doDeclareEquivalent(sym, DeclarePackedSet) )
      END
   END ;
   RETURN( Mod2Gcc(sym) )
END PossiblyPacked ;


(*
   GetPackedType - returns a possibly packed type for field.
*)

PROCEDURE GetPackedType (sym: CARDINAL) : Tree ;
BEGIN
   IF IsSubrange(sym)
   THEN
      RETURN( doDeclareEquivalent(sym, DeclarePackedSubrange) )
   ELSIF IsType(sym)
   THEN
      RETURN( doDeclareEquivalent(sym, DeclarePackedType) )
   ELSIF IsEnumeration(sym)
   THEN
      RETURN( doDeclareEquivalent(sym, DeclarePackedEnumeration) )
   END ;
   RETURN( Mod2Gcc(sym) )
END GetPackedType ;


(*
   MaybeAlignField - checks to see whether, field, is packed or aligned and it updates
                     the offsets if appropriate.
*)

PROCEDURE MaybeAlignField (field: CARDINAL; VAR byteOffset, bitOffset: Tree) : Tree ;
VAR
   f, ftype, nbits: Tree ;
BEGIN
   f := Mod2Gcc(field) ;
   IF IsDeclaredPacked(field)
   THEN
      f := SetDeclPacked(f) ;
      ftype := GetPackedType(GetType(field)) ;
      nbits := BuildTBitSize(ftype) ;
      f := SetRecordFieldOffset(f, byteOffset, bitOffset, ftype, nbits) ;
      bitOffset := BuildAdd(bitOffset, nbits, FALSE) ;
      RETURN( f )
   ELSE
      RETURN( CheckAlignment(f, field) )
   END
END MaybeAlignField ;


(*
   DeclareRecord - declares a record and its fields to gcc.
                   The final gcc record type is returned.
*)

PROCEDURE DeclareRecord (Sym: CARDINAL) : Tree ;
VAR
   Field       : CARDINAL ;
   i           : CARDINAL ;
   nbits,
   ftype,
   field,
   byteOffset,
   bitOffset,
   FieldList,
   RecordType  : Tree ;
BEGIN
   i := 1 ;
   FieldList := Tree(NIL) ;
   RecordType := DoStartDeclaration(Sym, BuildStartRecord) ;
   byteOffset := GetIntegerZero() ;
   bitOffset := GetIntegerZero() ;
   REPEAT
      Field := GetNth(Sym, i) ;
      IF Field#NulSym
      THEN
         IF IsRecordField(Field) AND IsRecordFieldAVarientTag(Field) AND (GetSymName(Field)=NulName)
         THEN
            (* do not include a nameless tag into the C struct *)
         ELSIF IsVarient(Field)
         THEN
            Field := Chained(Field) ;
            field := Mod2Gcc(Field) ;
            IF IsDeclaredPacked(Field)
            THEN
               field := SetDeclPacked(field) ;
               ftype := GetPackedType(GetType(Field)) ;
               nbits := BuildTBitSize(ftype) ;
               field := SetRecordFieldOffset(field, byteOffset, bitOffset, ftype, nbits) ;
               bitOffset := BuildAdd(bitOffset, nbits, FALSE) ;
               byteOffset := BuildAdd(byteOffset,
                                      BuildDivTrunc(bitOffset, BuildIntegerConstant(8), FALSE),
                                      FALSE) ;
               bitOffset := BuildModTrunc(bitOffset, BuildIntegerConstant(8), FALSE)
            END ;
            FieldList := ChainOn(FieldList, field)
         ELSE
            IF Debugging
            THEN
               printf0('chaining ') ; PrintTerse(Field) ; printf0('\n')
            END ;
            FieldList := ChainOn(FieldList, MaybeAlignField(Chained(Field), byteOffset, bitOffset))
         END
      END ;
      INC(i)
   UNTIL Field=NulSym ;
   WatchRemoveList(Sym, partiallydeclared) ;
   WatchRemoveList(Sym, heldbyalignment) ;
   WatchRemoveList(Sym, finishedalignment) ;
   RETURN( BuildEndRecord(RecordType, FieldList, IsDeclaredPacked(Sym)) )
END DeclareRecord ;


(*
   DeclareRecordField - 
*)

PROCEDURE DeclareRecordField (sym: CARDINAL) : Tree ;
VAR
   field,
   GccFieldType: Tree ;
BEGIN
   GccFieldType := PossiblyPacked(GetType(sym), IsDeclaredPacked(sym)) ;
   field := BuildFieldRecord(KeyToCharStar(GetFullSymName(sym)), GccFieldType) ;
   RETURN( field )
END DeclareRecordField ;


(*
   DeclareVarient - declares a record and its fields to gcc.
                    The final gcc record type is returned.
*)

PROCEDURE DeclareVarient (sym: CARDINAL) : Tree ;
VAR
   Field       : CARDINAL ;
   i           : CARDINAL ;
   byteOffset,
   bitOffset,
   FieldList,
   field,
   VarientType : Tree ;
BEGIN
   i := 1 ;
   FieldList := Tree(NIL) ;
   VarientType := DoStartDeclaration(sym, BuildStartVarient) ;
   byteOffset := GetIntegerZero() ;
   bitOffset := GetIntegerZero() ;
   WHILE GetNth(sym, i)#NulSym DO
      Field := GetNth(sym, i) ;
      IF IsRecordField(Field) AND IsRecordFieldAVarientTag(Field) AND (GetSymName(Field)=NulName)
      THEN
         (* do not include a nameless tag into the C struct *)
      ELSE
         IF Debugging
         THEN
            printf0('chaining ') ; PrintTerse(Field) ; printf0('\n')
         END ;
         FieldList := ChainOn(FieldList, MaybeAlignField(Chained(Field), byteOffset, bitOffset))
      END ;
      INC(i)
   END ;
   WatchRemoveList(sym, partiallydeclared) ;
   WatchRemoveList(sym, heldbyalignment) ;
   WatchRemoveList(sym, finishedalignment) ;
   VarientType := BuildEndVarient(VarientType, FieldList, IsDeclaredPacked(sym)) ;
   RETURN( VarientType )
END DeclareVarient ;


(*
   DeclareFieldVarient - 
*)

PROCEDURE DeclareFieldVarient (sym: CARDINAL) : Tree ;
VAR
   i, f        : CARDINAL ;
   VarientList,
   VarientType,
   byteOffset,
   bitOffset,
   field,
   GccFieldType: Tree ;
BEGIN
   i := 1 ;
   VarientList := Tree(NIL) ;
   VarientType := DoStartDeclaration(sym, BuildStartFieldVarient) ;
   (* no need to store the [sym, RecordType] tuple as it is stored by DeclareRecord which calls us *)
   byteOffset := GetIntegerZero() ;
   bitOffset := GetIntegerZero() ;
   WHILE GetNth(sym, i)#NulSym DO
      f := GetNth(sym, i) ;
      IF IsFieldVarient(f) AND IsEmptyFieldVarient(f)
      THEN
         (* do not include empty varient fields (created via 'else end' in variant records *)
      ELSE
         IF Debugging
         THEN
            printf0('chaining ') ; PrintTerse(f) ; printf0('\n')
         END ;
         VarientList := ChainOn(VarientList, MaybeAlignField(Chained(f), byteOffset, bitOffset))
      END ;
      INC(i)
   END ;
   WatchRemoveList(sym, partiallydeclared) ;
   GccFieldType := BuildEndFieldVarient(VarientType, VarientList, IsDeclaredPacked(sym)) ;
   RETURN( GccFieldType )
END DeclareFieldVarient ;


(*
   DeclarePointer - declares a pointer type to gcc and returns the Tree.
*)

PROCEDURE DeclarePointer (sym: CARDINAL) : Tree ;
BEGIN
   RETURN( BuildPointerType(Mod2Gcc(GetType(sym))) )
END DeclarePointer ;


(*
   DeclareUnbounded - builds an unbounded type and returns the gcc tree.
*)

PROCEDURE DeclareUnbounded (sym: CARDINAL) : Tree ;
VAR
   record: CARDINAL ;
BEGIN
   Assert(IsUnbounded(sym)) ;
   IF GccKnowsAbout(sym)
   THEN
      RETURN( Mod2Gcc(sym) )
   ELSE
      record := GetUnboundedRecordType(sym) ;
      Assert(IsRecord(record)) ;
      Assert(AllDependantsFullyDeclared(record)) ;
      IF (NOT GccKnowsAbout(record))
      THEN
         DeclareTypeConstFully(record) ;
         WatchRemoveList(record, todolist)
      END ;
      RETURN( Mod2Gcc(record) )
   END
END DeclareUnbounded ;


(*
   BuildIndex - 
*)

PROCEDURE BuildIndex (sym: CARDINAL) : Tree ;
VAR
   Subscript: CARDINAL ;
   Type,
   High, Low: CARDINAL ;
BEGIN
   Subscript := GetArraySubscript(sym) ;
   Assert(IsSubscript(Subscript)) ;
   Type := SkipType(GetType(Subscript)) ;
   Low := GetTypeMin(Type) ;
   High := GetTypeMax(Type) ;
   RETURN( BuildArrayIndexType(Mod2Gcc(Low), Mod2Gcc(High)) )
END BuildIndex ;


(*
   DeclareArray - declares an array to gcc and returns the gcc tree.
*)

PROCEDURE DeclareArray (Sym: CARDINAL) : Tree ;
VAR
   typeOfArray: CARDINAL ;
   ArrayType,
   GccArray,
   GccIndex   : Tree ;
   Subscript  : CARDINAL ;
BEGIN
   Assert(IsArray(Sym)) ;

   Subscript := GetArraySubscript(Sym) ;
   typeOfArray := SkipType(GetType(Sym)) ;
   GccArray := Mod2Gcc(typeOfArray) ;
   GccIndex := BuildIndex(Sym) ;

   IF GccKnowsAbout(Sym)
   THEN
      ArrayType := Mod2Gcc(Sym)
   ELSE
      ArrayType := BuildStartArrayType(GccIndex, GccArray, typeOfArray) ;
      PreAddModGcc(Sym, ArrayType)
   END ;

   PreAddModGcc(Subscript, GccArray) ;       (* we save the type of this array as the subscript *)
   PushIntegerTree(BuildSize(GccArray, FALSE)) ;  (* and the size of this array so far *)
   PopSize(Subscript) ;

   GccArray := BuildEndArrayType(ArrayType, GccArray, GccIndex, typeOfArray) ;
   Assert(GccArray=ArrayType) ;

   RETURN( GccArray )
END DeclareArray ;


(*
   DeclareProcType - declares a procedure type to gcc and returns the gcc type tree.
*)

PROCEDURE DeclareProcType (Sym: CARDINAL) : Tree ;
VAR
   i, p, Son,
   ReturnType: CARDINAL ;
   func,
   GccParam  : Tree ;
BEGIN
   ReturnType := GetType(Sym) ;
   func := DoStartDeclaration(Sym, BuildStartFunctionType) ;
   InitFunctionTypeParameters(UsesVarArgs(Sym)) ;
   p := NoOfParam(Sym) ;
   i := p ;
   Assert(PushParametersLeftToRight) ;
   WHILE i>0 DO
      Son := GetNthParam(Sym, i) ;
      GccParam := BuildParameterDeclaration(NIL, Mod2Gcc(GetType(Son)), IsVarParam(Sym, i)) ;
      PreAddModGcc(Son, GccParam) ;
      DEC(i)
   END ;
   IF ReturnType=NulSym
   THEN
      RETURN( BuildEndFunctionType(func, NIL) )
   ELSE
      RETURN( BuildEndFunctionType(func, Mod2Gcc(ReturnType)) )
   END
END DeclareProcType ;


VAR
   MaxEnumerationField,
   MinEnumerationField: CARDINAL ;


(*
   FindMinMaxEnum - finds the minimum and maximum enumeration fields.
*)

PROCEDURE FindMinMaxEnum (field: WORD) ;
VAR
   i: CARDINAL ;
BEGIN
   IF MaxEnumerationField=NulSym
   THEN
      MaxEnumerationField := field
   ELSE
      PushValue(field) ;
      PushValue(MaxEnumerationField) ;
      IF Gre(GetDeclared(field))
      THEN
         MaxEnumerationField := field
      END
   END ;
   IF MinEnumerationField=NulSym
   THEN
      MinEnumerationField := field
   ELSE
      PushValue(field) ;
      PushValue(MinEnumerationField) ;
      IF Less(GetDeclared(field))
      THEN
         MinEnumerationField := field
      END
   END
END FindMinMaxEnum ;


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
      GetSubrange(type, max, min) ;
      RETURN( min )
   ELSIF IsSet(type)
   THEN
      RETURN( GetTypeMin(GetType(type)) )
   ELSIF IsEnumeration(type)
   THEN
      MinEnumerationField := NulSym ;
      MaxEnumerationField := NulSym ;
      ForeachFieldEnumerationDo(type, FindMinMaxEnum) ;
      RETURN( MinEnumerationField )
   ELSIF IsBaseType(type)
   THEN
      GetBaseTypeMinMax(type, min, max) ;
      RETURN( min )
   ELSIF IsSystemType(type)
   THEN
      GetSystemTypeMinMax(type, min, max) ;
      RETURN( min )
   ELSIF GetType(type)=NulSym
   THEN
      MetaError1('unable to obtain the MIN value for type {%1as}', type)
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
      GetSubrange(type, max, min) ;
      RETURN( max )
   ELSIF IsSet(type)
   THEN
      RETURN( GetTypeMax(GetType(type)) )
   ELSIF IsEnumeration(type)
   THEN
      MinEnumerationField := NulSym ;
      MaxEnumerationField := NulSym ;
      ForeachFieldEnumerationDo(type, FindMinMaxEnum) ;
      RETURN( MaxEnumerationField )
   ELSIF IsBaseType(type)
   THEN
      GetBaseTypeMinMax(type, min, max) ;
      RETURN( max )
   ELSIF IsSystemType(type)
   THEN
      GetSystemTypeMinMax(type, min, max) ;
      RETURN( max )
   ELSIF GetType(type)=NulSym
   THEN
      MetaError1('unable to obtain the MAX value for type {%1as}', type)
   ELSE
      RETURN( GetTypeMax(GetType(type)) )
   END
END GetTypeMax ;


(*
   DeclareLargeSet - n is the name of the set.
                     type is the subrange type (or simple type)
                     low and high are the limits of the subrange.
*)

PROCEDURE DeclareLargeSet (n: Name; type: CARDINAL; low, high: CARDINAL) : Tree ;
VAR
   lowtree,
   hightree,
   BitsInSet,
   RecordType,
   GccField,
   FieldList : Tree ;
   bpw       : CARDINAL ;
BEGIN
   bpw        := GetBitsPerBitset() ;
   PushValue(low) ;
   lowtree    := PopIntegerTree() ;
   PushValue(high) ;
   hightree   := PopIntegerTree() ;
   FieldList  := Tree(NIL) ;
   RecordType := BuildStartRecord(KeyToCharStar(n)) ;  (* no problem with recursive types here *)
   PushValue(high) ;
   ConvertToInt ;
   PushValue(low) ;
   ConvertToInt ;
   Sub ;
   PushCard(1) ;
   Addn ;
   BitsInSet := PopIntegerTree() ;
   PushIntegerTree(BitsInSet) ;
   PushCard(0) ;
   WHILE Gre(GetDeclared(type)) DO
      PushIntegerTree(BitsInSet) ;
      PushCard(bpw-1) ;
      IF GreEqu(GetDeclared(type))
      THEN
         PushIntegerTree(lowtree) ;
         PushCard(bpw-1) ;
         Addn ;
         GccField := BuildFieldRecord(NIL, BuildSetType(NIL, Mod2Gcc(type), lowtree, PopIntegerTree())) ;
         PushIntegerTree(lowtree) ;
         PushCard(bpw) ;
         Addn ;
         lowtree := PopIntegerTree() ;
         PushIntegerTree(BitsInSet) ;
         PushCard(bpw) ;
         Sub ;
         BitsInSet := PopIntegerTree()
      ELSE
         (* printf2('range is %a..%a\n', GetSymName(low), GetSymName(high)) ; *)
         GccField := BuildFieldRecord(NIL, BuildSetType(NIL, Mod2Gcc(type), lowtree, hightree)) ;
         PushCard(0) ;
         BitsInSet := PopIntegerTree()
      END ;
      FieldList := ChainOn(FieldList, GccField) ;
      PushIntegerTree(BitsInSet) ;
      PushCard(0)
   END ;
   RETURN( BuildEndRecord(RecordType, FieldList, FALSE) )
END DeclareLargeSet ;


(*
   DeclareLargeOrSmallSet - works out whether the set will exceed TSIZE(WORD). If it does
                            we manufacture a set using:

                            settype = RECORD
                                         w1: SET OF [...]
                                         w2: SET OF [...]
                                      END

                            We do this as GCC and GDB (stabs) only knows about WORD sized sets.
                            If the set will fit into a WORD then we call gccgm2 directly.
*)

PROCEDURE DeclareLargeOrSmallSet (sym: CARDINAL;
                                  n: Name; type: CARDINAL; low, high: CARDINAL) : Tree ;
BEGIN
   PushValue(high) ;
   ConvertToInt ;
   PushValue(low) ;
   ConvertToInt ;
   Sub ;
   PushCard(GetBitsPerBitset()) ;
   IF Less(GetDeclared(type))
   THEN
      (* small set *)
      (* PutSetSmall(sym) ; *)
      RETURN( BuildSetType(KeyToCharStar(n), Mod2Gcc(type), Mod2Gcc(low), Mod2Gcc(high)) )
   ELSE
      (* PutSetLarge(sym) ; *)
      RETURN( DeclareLargeSet(n, type, low, high) )
   END
END DeclareLargeOrSmallSet ;


(*
   DeclareSet - declares a set type to gcc and returns a Tree.
*)

PROCEDURE DeclareSet (sym: CARDINAL) : Tree ;
VAR
   gccsym   : Tree ;
   type,
   high, low: CARDINAL ;
BEGIN
   type := SkipType(GetType(sym)) ;
   IF IsSubrange(type)
   THEN
      GetSubrange(type, high, low) ;
      gccsym := DeclareLargeOrSmallSet(sym, GetFullSymName(sym), GetType(type), low, high)
   ELSE
      gccsym := DeclareLargeOrSmallSet(sym, GetFullSymName(sym), type, GetTypeMin(type), GetTypeMax(type))
   END ;
   RETURN( gccsym )
END DeclareSet ;


(*
   CheckResolveSubrange - checks to see whether we can determine
                          the subrange type.  We are able to do
                          this once low, high and the type are known.
*)

PROCEDURE CheckResolveSubrange (sym: CARDINAL) ;
VAR
   n                    : Name ;
   size, high, low, type: CARDINAL ;
BEGIN
   GetSubrange(sym, high, low) ;
   type := GetType(sym) ;
   IF type=NulSym
   THEN
      IF GccKnowsAbout(low) AND GccKnowsAbout(high)
      THEN
         IF IsConstString(low)
         THEN
            size := GetStringLength(low) ;
            IF size=1
            THEN
               PutSubrange(sym, low, high, Char)
            ELSE
               MetaError1('cannot have a subrange of a string type {%1Uad}',
                          sym)
            END
         ELSIF IsFieldEnumeration(low)
         THEN
            IF GetType(low)=GetType(high)
            THEN
               PutSubrange(sym, low, high, GetType(low))
            ELSE
               MetaError1('subrange limits must be of the same type {%1Uad}', sym)
            END
         ELSIF IsValueSolved(low)
         THEN
            IF GetType(low)=LongReal
            THEN
               MetaError1('cannot have a subrange of a SHORTREAL, REAL or LONGREAL type {%1Uad}', sym)
            ELSE
               PutSubrange(sym, low, high, MixTypes(GetType(low), GetType(high), GetDeclared(sym)))
            END
         END
      END
   END
END CheckResolveSubrange ;


(*
   TypeConstFullyDeclared - all, sym, dependents are declared, so create and
                            return the GCC Tree equivalent.
*)

PROCEDURE TypeConstFullyDeclared (sym: CARDINAL) : Tree ;
VAR
   t: Tree ;
   n: Name ;
BEGIN
   IF IsEnumeration(sym)
   THEN
      t := DeclareEnumeration(sym)
   ELSIF IsFieldEnumeration(sym)
   THEN
      t := DeclareFieldEnumeration(sym)
   ELSIF IsSubrange(sym)
   THEN
      t := DeclareSubrange(sym)
   ELSIF IsRecord(sym)
   THEN
      t := CheckPragma(DeclareRecord(sym), sym)
   ELSIF IsRecordField(sym)
   THEN
      t := CheckPragma(DeclareRecordField(sym), sym)
   ELSIF IsFieldVarient(sym)
   THEN
      t := DeclareFieldVarient(sym)
   ELSIF IsVarient(sym)
   THEN
      t := DeclareVarient(sym)
   ELSIF IsPointer(sym)
   THEN
      t := CheckAlignment(DeclarePointer(sym), sym)
   ELSIF IsUnbounded(sym)
   THEN
      t := DeclareUnbounded(sym)
   ELSIF IsArray(sym)
   THEN
      t := CheckAlignment(DeclareArray(sym), sym)
   ELSIF IsProcType(sym)
   THEN
      t := DeclareProcType(sym)
   ELSIF IsSet(sym)
   THEN
      t := DeclareSet(sym)
   ELSIF IsConst(sym)
   THEN
      IF IsConstructor(sym)
      THEN
         PushValue(sym) ;
         ChangeToConstructor(GetDeclared(sym), GetType(sym)) ;
         PopValue(sym) ;
         EvaluateValue(sym) ;
         PutConstructorSolved(sym) ;
      ELSIF IsConstSet(sym)
      THEN
         EvaluateValue(sym)
      END ;
      IF NOT IsValueSolved(sym)
      THEN
         RETURN( NIL )
      END ;
      t := DeclareConst(GetDeclared(sym), sym) ;
      Assert(t#NIL)
   ELSIF IsConstructor(sym)
   THEN
      (* not yet known as a constant *)
      RETURN( NIL )
   ELSE
      t := DeclareType(sym) ;
      IF IsType(sym)
      THEN
         t := CheckAlignment(t, sym)
      END
   END ;
   IF GetSymName(sym)#NulName
   THEN
      IF Debugging
      THEN
         n := GetSymName(sym) ;
         printf1('declaring type %a\n', n)
      END ;
      t := RememberType(t)
   END ;
   RETURN( t )
END TypeConstFullyDeclared ;


(*
   IsBaseType - returns true if a type, Sym, is a base type and
                we use predefined GDB information to represent this
                type.
*)

PROCEDURE IsBaseType (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (Sym=Cardinal) OR (Sym=Integer) OR
           (Sym=Char) OR (Sym=Proc) )
END IsBaseType ;


(*
   IsFieldEnumerationDependants - sets enumDeps to FALSE if action(Sym)
                                  is also FALSE.
*)

PROCEDURE IsFieldEnumerationDependants (Sym: WORD) ;
BEGIN
   IF NOT action(Sym)
   THEN
      enumDeps := FALSE
   END
END IsFieldEnumerationDependants ;


(*
   IsEnumerationDependants - returns true if the enumeration
                             p(dependants) all return true.
*)

PROCEDURE IsEnumerationDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
BEGIN
   action := q ;
   enumDeps := TRUE ;
   ForeachFieldEnumerationDo(sym, IsFieldEnumerationDependants) ;
   RETURN( enumDeps )
END IsEnumerationDependants ;


(*
   WalkEnumerationDependants - returns walks all dependants of Sym.
*)

PROCEDURE WalkEnumerationDependants (sym: CARDINAL; p: WalkAction) ;
BEGIN
   ForeachFieldEnumerationDo(sym, p)
END WalkEnumerationDependants ;


(*
   WalkSubrangeDependants - calls p(dependants) for each dependant of, sym.
*)

PROCEDURE WalkSubrangeDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   type,
   high, low: CARDINAL ;
BEGIN
   GetSubrange(sym, high, low) ;
   CheckResolveSubrange(sym) ;
   type := GetType(sym) ;
   IF type#NulSym
   THEN
      p(type)
   END ;
   (* low and high are not types but constants and they are resolved by M2GenGCC *)
   p(low) ;
   p(high)
END WalkSubrangeDependants ;


(*
   IsSubrangeDependants - returns TRUE if the subrange
                          q(dependants) all return TRUE.
*)

PROCEDURE IsSubrangeDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   result   : BOOLEAN ;
   type,
   high, low: CARDINAL ;
BEGIN
   GetSubrange(sym, high, low) ;
   (* low and high are not types but constants and they are resolved by M2GenGCC *)
   CheckResolveSubrange(sym) ;
   result := TRUE ;
   type := GetType(sym) ;
   IF (type=NulSym) OR (NOT q(type))
   THEN
      result := FALSE
   END ;
   IF NOT q(low)
   THEN
      result := FALSE
   END ;
   IF NOT q(high)
   THEN
      result := FALSE
   END ;
   RETURN( result )
END IsSubrangeDependants ;


(*
   WalkComponentDependants - 
*)

PROCEDURE WalkComponentDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   i   : CARDINAL ;
   type: CARDINAL ;
BEGIN
   (* need to walk record and field *)
   i := 1 ;
   REPEAT
      type := GetNth(sym, i) ;
      IF type#NulSym
      THEN
         IF IsVar(type)
         THEN
            p(GetType(type))
         ELSE
            p(type)
         END ;
         INC(i)
      END
   UNTIL type=NulSym
END WalkComponentDependants ;


(*
   IsComponentDependants - 
*)

PROCEDURE IsComponentDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   type  : CARDINAL ;
   i     : CARDINAL ;
   result: BOOLEAN ;
BEGIN
   (* need to check record is completely resolved *)
   result := TRUE ;
   i := 1 ;
   REPEAT
      type := GetNth(sym, i) ;
      IF type#NulSym
      THEN
         IF IsVar(type)
         THEN
            type := GetType(type)
         END ;
         IF NOT q(type)
         THEN
            result := FALSE
         END ;
         INC(i)
      END
   UNTIL type=NulSym ;
   RETURN( result )
END IsComponentDependants ;


(*
   WalkVarDependants - walks all dependants of sym.
*)

PROCEDURE WalkVarDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   type: CARDINAL ;
BEGIN
   p(GetType(sym)) ;
   IF IsComponent(sym)
   THEN
      WalkComponentDependants(sym, p)
   END ;
   type := GetVarBackEndType(sym) ;
   IF type#NulSym
   THEN
      p(type)
   END
END WalkVarDependants ;


(*
   IsVarDependants - returns TRUE if the pointer symbol, sym,
                     p(dependants) all return TRUE.
*)

PROCEDURE IsVarDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   type  : CARDINAL ;
   result: BOOLEAN ;
BEGIN
   result := TRUE ;
   IF NOT q(GetType(sym))
   THEN
      result := FALSE
   END ;
   IF IsComponent(sym)
   THEN
      IF NOT IsComponentDependants(sym, q)
      THEN
         result := FALSE
      END
   END ;
   type := GetVarBackEndType(sym) ;
   IF type#NulSym
   THEN
      IF NOT q(type)
      THEN
         result := FALSE
      END
   END ;
   RETURN( result )
END IsVarDependants ;


(*
   WalkPointerDependants - walks all dependants of sym.
*)

PROCEDURE WalkPointerDependants (sym: CARDINAL; p: WalkAction) ;
BEGIN
   p(GetType(sym))
END WalkPointerDependants ;


(*
   IsPointerDependants - returns TRUE if the pointer symbol, sym,
      	       	         p(dependants) all return TRUE.
*)

PROCEDURE IsPointerDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
BEGIN
   RETURN( q(GetType(sym)) )
END IsPointerDependants ;


(*
   IsRecordAlignment -
*)

PROCEDURE IsRecordAlignment (sym: CARDINAL; q: IsAction) : BOOLEAN ;
BEGIN
   IF GetDefaultRecordFieldAlignment(sym)#NulSym
   THEN
      IF NOT q(GetDefaultRecordFieldAlignment(sym))
      THEN
         RETURN( FALSE )
      END
   END ;
   RETURN( TRUE )
END IsRecordAlignment ;


(*
   IsRecordDependants - returns TRUE if the symbol, sym,
                        q(dependants) all return TRUE.
*)

PROCEDURE IsRecordDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   result: BOOLEAN ;
   i     : CARDINAL ;
   field : CARDINAL ;
BEGIN
   result := IsRecordAlignment(sym, q) ;
   i := 1 ;
   REPEAT
      field := GetNth(sym, i) ;
      IF field#NulSym
      THEN
         IF IsRecordField(field)
         THEN
            IF (NOT IsRecordFieldAVarientTag(field)) OR (GetSymName(field)#NulName)
            THEN
               IF NOT q(field)
               THEN
                  result := FALSE
               END
            END
         ELSIF IsVarient(field)
         THEN
            IF NOT q(field)
            THEN
               result := FALSE
      	    END
      	 ELSIF IsFieldVarient(field)
      	 THEN
            InternalError('should not see a field varient', __FILE__, __LINE__)
         ELSE
            InternalError('unknown symbol in record', __FILE__, __LINE__)
      	 END
      END ;
      INC(i)
   UNTIL field=NulSym ;
   RETURN( result )
END IsRecordDependants ;


(*
   WalkRecordAlignment - walks the alignment constant associated with
                         record, sym.
*)

PROCEDURE WalkRecordAlignment (sym: CARDINAL; p: WalkAction) ;
BEGIN
   IF GetDefaultRecordFieldAlignment(sym)#NulSym
   THEN
      p(GetDefaultRecordFieldAlignment(sym))
   END
END WalkRecordAlignment ;


(*
   WalkRecordDependants - walks symbol, sym, dependants.  It only
                          walks the fields if the alignment is
                          unused or fully declared.
*)

PROCEDURE WalkRecordDependants (sym: CARDINAL; p: WalkAction) ;
BEGIN
   WalkRecordAlignment(sym, p) ;
   WalkRecordDependants2(sym, p)
END WalkRecordDependants ;


(*
   WalkRecordFieldDependants - 
*)

PROCEDURE WalkRecordFieldDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   v: CARDINAL ;
BEGIN
   Assert(IsRecordField(sym)) ;
   p(GetType(sym)) ;
   v := GetVarient(sym) ;
   IF v#NulSym
   THEN
      p(v)
   END
END WalkRecordFieldDependants ;


(*
   WalkVarient - 
*)

PROCEDURE WalkVarient (sym: CARDINAL; p: WalkAction) ;
VAR
   v: CARDINAL ;
BEGIN
   p(sym) ;
   v := GetVarient(sym) ;
   IF v#NulSym
   THEN
      p(v)
   END
END WalkVarient ;


(*
   WalkRecordDependants2 - walks the fields of record, sym, calling
                           p on every dependant.
*)

PROCEDURE WalkRecordDependants2 (sym: CARDINAL; p: WalkAction) ;
VAR
   i    : CARDINAL ;
   Field: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE GetNth(sym, i)#NulSym DO
      Field := GetNth(sym, i) ;
      p(Field) ;
      IF IsRecordField(Field)
      THEN
         WalkRecordFieldDependants(Field, p)
      ELSIF IsVarient(Field)
      THEN
         WalkVarientDependants(Field, p)
      ELSIF IsFieldVarient(Field)
      THEN
         InternalError('should not see a field varient', __FILE__, __LINE__)
      ELSE
         InternalError('unknown symbol in record', __FILE__, __LINE__)
      END ;
      INC(i)
   END
END WalkRecordDependants2 ;


(*
   IsVarientAlignment - 
*)

PROCEDURE IsVarientAlignment (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   align: CARDINAL ;
BEGIN
   sym := GetRecordOfVarient(sym) ;
   align := GetDefaultRecordFieldAlignment(sym) ;
   IF (align#NulSym) AND (NOT q(align))
   THEN
      RETURN( FALSE )
   END ;
   RETURN( TRUE )
END IsVarientAlignment ;


(*
   IsVarientDependants - returns TRUE if the symbol, sym,
                         q(dependants) all return TRUE.
*)

PROCEDURE IsVarientDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   result: BOOLEAN ;
   i     : CARDINAL ;
   Field : CARDINAL ;
BEGIN
   result := IsVarientAlignment(sym, q) ;
   i := 1 ;
   WHILE GetNth(sym, i)#NulSym DO
      Field := GetNth(sym, i) ;
      Assert(IsFieldVarient(Field)) ;
      IF NOT q(Field)
      THEN
         result := FALSE
      END ;
      INC(i)
   END ;
   RETURN( result )
END IsVarientDependants ;


(*
   WalkVarientAlignment - 
*)

PROCEDURE WalkVarientAlignment (sym: CARDINAL; p: WalkAction) ;
VAR
   align: CARDINAL ;
BEGIN
   sym := GetRecordOfVarient(sym) ;
   align := GetDefaultRecordFieldAlignment(sym) ;
   IF align#NulSym
   THEN
      p(align)
   END
END WalkVarientAlignment ;


(*
   WalkVarientDependants - walks symbol, sym, dependants.
*)

PROCEDURE WalkVarientDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   i    : CARDINAL ;
   v,
   Field: CARDINAL ;
BEGIN
   WalkVarientAlignment(sym, p) ;
   IF GetType(sym)#NulSym
   THEN
      p(GetType(sym))
   END ;
   v := GetVarient(sym) ;
   IF v#NulSym
   THEN
      p(v)
   END ;
   i := 1 ;
   WHILE GetNth(sym, i)#NulSym DO
      Field := GetNth(sym, i) ;
      Assert(IsFieldVarient(Field)) ;  (* field varients do _not_ have a type *)
      p(Field) ;
      WalkVarientFieldDependants(Field, p) ;
      INC(i)
   END
END WalkVarientDependants ;


(*
   IsVarientFieldDependants - returns TRUE if the symbol, sym,
                              q(dependants) all return TRUE.
*)

PROCEDURE IsVarientFieldDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   i     : CARDINAL ;
   type,
   Field : CARDINAL ;
   result: BOOLEAN ;
BEGIN
   i := 1 ;
   result := IsVarientAlignment(sym, q) ;
   WHILE GetNth(sym, i)#NulSym DO
      Field := GetNth(sym, i) ;
      IF NOT q(Field)
      THEN
         result := FALSE
      END ;
      type := GetType(Field) ;
      IF type#NulSym
      THEN
         IF NOT q(type)
         THEN
            result := FALSE
         END
      END ;
      INC(i)
   END ;
   RETURN( result )
END IsVarientFieldDependants ;


(*
   WalkVarientFieldDependants - 
*)

PROCEDURE WalkVarientFieldDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   i    : CARDINAL ;
   type,
   Field: CARDINAL ;
BEGIN
   WalkVarientAlignment(sym, p) ;
   i := 1 ;
   WHILE GetNth(sym, i)#NulSym DO
      Field := GetNth(sym, i) ;
      p(Field) ;
      type := GetType(Field) ;
      IF type#NulSym
      THEN
         p(type)
      END ;
      INC(i)
   END
END WalkVarientFieldDependants ;


(*
   IsArrayDependants - returns TRUE if the symbol, sym,
      	       	       q(dependants) all return TRUE.

*)

PROCEDURE IsArrayDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   result   : BOOLEAN ;
   subscript: CARDINAL ;
   high, low: CARDINAL ;
   type     : CARDINAL ;
BEGIN
   result := TRUE ;
   Assert(IsArray(sym)) ;
   result := TRUE ;
   type := GetType(sym) ;

   IF NOT q(type)
   THEN
      result := FALSE
   END ;
   subscript := GetArraySubscript(sym) ;
   IF subscript#NulSym
   THEN
      Assert(IsSubscript(subscript)) ;
      type := GetType(subscript) ;
      IF NOT q(type)
      THEN
         result := FALSE
      END ;
      type := SkipType(type) ;
      (* the array might be declared as ARRAY type OF foo *)
      low  := GetTypeMin(type) ;
      high := GetTypeMax(type) ;
      IF NOT q(low)
      THEN
         result := FALSE
      END ;
      IF NOT q(high)
      THEN
         result := FALSE
      END
   END ;
   RETURN( result )
END IsArrayDependants ;


(*
   WalkArrayDependants - walks symbol, sym, dependants.
*)

PROCEDURE WalkArrayDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   subscript: CARDINAL ;
   high, low: CARDINAL ;
   type     : CARDINAL ;
BEGIN
   Assert(IsArray(sym)) ;
   type := GetType(sym) ;
   p(type) ;
   subscript := GetArraySubscript(sym) ;
   IF subscript#NulSym
   THEN
      Assert(IsSubscript(subscript)) ;
      type := GetType(subscript) ;
      p(type) ;
      type := SkipType(type) ;
      (* the array might be declared as ARRAY type OF foo *)
      low  := GetTypeMin(type) ;
      high := GetTypeMax(type) ;
      p(low) ;
      p(high)
   END
END WalkArrayDependants ;


(*
   IsSetDependants - returns TRUE if the symbol, sym,
                     q(dependants) all return TRUE.
*)

PROCEDURE IsSetDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   result         : BOOLEAN ;
   type, low, high: CARDINAL ;
BEGIN
   result := TRUE ;
   Assert(IsSet(sym)) ;

   type := SkipType(GetType(sym)) ;
   IF NOT q(type)
   THEN
      result := FALSE
   END ;
   low  := GetTypeMin(type) ;
   high := GetTypeMax(type) ;
   IF NOT q(low)
   THEN
      result := FALSE
   END ;
   IF NOT q(high)
   THEN
      result := FALSE
   END ;
   RETURN( result )
END IsSetDependants ;


(*
   WalkSetDependants - walks dependants, sym.
*)

PROCEDURE WalkSetDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   type, low, high: CARDINAL ;
BEGIN
   Assert(IsSet(sym)) ;

   type := SkipType(GetType(sym)) ;
   p(type) ;
   low  := GetTypeMin(type) ;
   p(low) ;
   high := GetTypeMax(type) ;
   p(high)
END WalkSetDependants ;


(*
   IsProcTypeDependants - 
*)

PROCEDURE IsProcTypeDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   i, p, son : CARDINAL ;
   ParamType,
   ReturnType: CARDINAL ;
   result    : BOOLEAN ;
BEGIN
   result := TRUE ;
   Assert(IsProcType(sym)) ;
   i := 1 ;
   ReturnType := GetType(sym) ;
   p := NoOfParam(sym) ;
   WHILE i<=p DO
      son := GetNthParam(sym, i) ;
      ParamType := GetType(son) ;
      IF NOT q(ParamType)
      THEN
         result := FALSE
      END ;
      INC(i)
   END ;
   IF (ReturnType=NulSym) OR q(ReturnType)
   THEN
      RETURN( result )
   ELSE
      RETURN( FALSE )
   END
END IsProcTypeDependants ;


(*
   WalkProcTypeDependants - walks dependants, sym.
*)

PROCEDURE WalkProcTypeDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   i, n, son : CARDINAL ;
   ParamType,
   ReturnType: CARDINAL ;
BEGIN
   Assert(IsProcType(sym)) ;
   i := 1 ;
   ReturnType := GetType(sym) ;
   n := NoOfParam(sym) ;
   WHILE i<=n DO
      son := GetNthParam(sym, i) ;
      ParamType := GetType(son) ;
      p(ParamType) ;
      INC(i)
   END ;
   IF ReturnType#NulSym
   THEN
      p(ReturnType)
   END
END WalkProcTypeDependants ;


(*
   IsProcedureDependants - 
*)

PROCEDURE IsProcedureDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   i, son    : CARDINAL ;
   type,
   ReturnType: CARDINAL ;
   result    : BOOLEAN ;
BEGIN
   result := TRUE ;
   Assert(IsProcedure(sym)) ;
   i := 1 ;
   ReturnType := GetType(sym) ;
   WHILE GetNth(sym, i)#NulSym DO
      son := GetNth(sym, i) ;
      type := GetType(son) ;
      IF NOT q(type)
      THEN
         result := FALSE
      END ;
      INC(i)
   END ;
   IF (ReturnType=NulSym) OR q(ReturnType)
   THEN
      RETURN( result )
   ELSE
      RETURN( FALSE )
   END
END IsProcedureDependants ;


(*
   WalkProcedureDependants - walks dependants, sym.
*)

PROCEDURE WalkProcedureDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   i, son    : CARDINAL ;
   type,
   ReturnType: CARDINAL ;
BEGIN
   Assert(IsProcedure(sym)) ;
   i := 1 ;
   ReturnType := GetType(sym) ;
   WHILE GetNth(sym, i)#NulSym DO
      son := GetNth(sym, i) ;
      type := GetType(son) ;
      p(type) ;
      INC(i)
   END ;
   IF ReturnType#NulSym
   THEN
      p(ReturnType)
   END
END WalkProcedureDependants ;


(*
   IsUnboundedDependants - returns TRUE if the symbol, sym,
                           q(dependants) all return TRUE.
*)

PROCEDURE IsUnboundedDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   result: BOOLEAN ;
BEGIN
   result := TRUE ;
   IF NOT q(GetUnboundedRecordType(sym))
   THEN
      result := FALSE
   END ;
   IF NOT q(Cardinal)
   THEN
      result := FALSE
   END ;
   IF NOT q(GetType(sym))
   THEN
      result := FALSE
   END ;
   RETURN( result )
END IsUnboundedDependants ;


(*
   WalkUnboundedDependants - walks the dependants of, sym.
*)

PROCEDURE WalkUnboundedDependants (sym: CARDINAL; p: WalkAction) ;
BEGIN
   p(GetUnboundedRecordType(sym)) ;
   p(Cardinal) ;
   p(GetType(sym))
END WalkUnboundedDependants ;


(*
   IsTypeDependants - returns TRUE if all q(dependants) return
                      TRUE.
*)

PROCEDURE IsTypeDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   type  : CARDINAL ;
   result: BOOLEAN ;
BEGIN
   type := GetType(sym) ;
   result := TRUE ;
   IF (type#NulSym) AND (NOT q(type))
   THEN
      result := FALSE
   END ;
   RETURN( result )
END IsTypeDependants ;


(*
   WalkTypeDependants - walks all dependants of, sym.
*)

PROCEDURE WalkTypeDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   type: CARDINAL ;
BEGIN
   type := GetType(sym) ;
   IF type#NulSym
   THEN
      p(type)
   END
END WalkTypeDependants ;


(*
   PoisonSymbols - poisons all gcc symbols from procedure, sym.
                   A debugging aid.
*)

PROCEDURE PoisonSymbols (sym: CARDINAL) ;
BEGIN
   IF IsProcedure(sym)
   THEN
      ForeachLocalSymDo(sym, Poison)
   END
END PoisonSymbols ;


(*
   ConstantKnownAndUsed - 
*)

PROCEDURE ConstantKnownAndUsed (sym: CARDINAL; t: Tree) ;
BEGIN
   DeclareConstantFromTree(sym, RememberConstant(t))
END ConstantKnownAndUsed ;


(*
   InitDeclarations - initializes default types and the source filename.
*)

PROCEDURE InitDeclarations ;
BEGIN
   DeclareFileName ;
   DeclareDefaultTypes ;
   DeclareDefaultConstants
END InitDeclarations ;


BEGIN
   ToDoList := InitSet(1) ;
   FullyDeclared := InitSet(1) ;
   PartiallyDeclared := InitSet(1) ;
   NilTypedArrays := InitSet(1) ;
   HeldByAlignment := InitSet(1) ;
   FinishedAlignment := InitSet(1) ;
   ToBeSolvedByQuads := InitSet(1) ;
   ChainedList := InitSet(1) ;
   WatchList := InitSet(1) ;
   VisitedList := NIL ;
   EnumerationIndex := InitIndex(1) ;
   IncludeElementIntoSet(WatchList, 8) ;
   HaveInitDefaultTypes := FALSE ;
   recursionCaught := FALSE
END M2GCCDeclare.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I.:../gm2-libs:../gm2-libs-ch:../gm2-libiberty/ M2GCCDeclare.mod"
 * End:
 *)