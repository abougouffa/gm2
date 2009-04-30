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

FROM SymbolTable IMPORT NulSym,
                        ModeOfAddr,
                        GetMode,
                        GetScope,
                        GetNth, GetType, SkipType, GetVarBackEndType,
                        MakeType, PutType, MakeConstLit,
      	       	     	GetSubrange, PutSubrange, GetArraySubscript,
      	       	     	NoOfParam, GetNthParam,
                        PushValue, PopValue, PopSize,
                        IsTemporary, IsUnbounded, IsEnumeration, IsVar,
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
                        IsGnuAsmVolatile, IsObject, IsTuple,
                        IsError, IsHiddenType,
                        IsDefinitionForC, IsHiddenTypeDeclared,
      	       	     	GetMainModule, GetBaseModule, GetModule,
                        PutModuleFinallyFunction,
                        GetProcedureScope, GetProcedureQuads,
                        GetVarient, GetUnbounded,
                        IsAModula2Type, UsesVarArgs,
                        GetSymName,
                        GetDeclared, GetVarBackEndType,
                        GetString, GetStringLength, IsConstString,
                        GetParameterShadowVar,
                        GetUnboundedAddressOffset, GetUnboundedHighOffset,
                        GetUnboundedRecordType,
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
                   Boolean, True, False,
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
                  ChangeToConstructor, EvaluateValue ;

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
                   GetBitsetType, GetBitnumType, GetMinFrom, GetMaxFrom, GetBitsPerInt, GetBitsPerBitset,
                   GetM2IntegerType, GetM2CardinalType,
                   GetISOLocType, GetISOByteType, GetISOWordType,
                   BuildStartEnumeration, BuildEndEnumeration, BuildEnumerator,
                   BuildIntegerConstant, BuildStringConstant, BuildCharConstant,
                   BuildSubrangeType,
                   BuildStartRecord, BuildEndRecord, BuildStartVarientRecord, BuildFieldRecord,
                   BuildArrayIndexType, BuildStartArrayType, BuildEndArrayType, BuildSetType,
                   DebugTree, GetDeclContext,
                   ChainOn,
                   BuildPointerType, BuildConstPointerType,
                   BuildStartFunctionType, BuildEndFunctionType,
                   InitFunctionTypeParameters,
                   BuildParameterDeclaration,
                   BuildStartFunctionDeclaration, BuildEndFunctionDeclaration,
                   BuildStartMainModule, BuildEndMainModule,
                   BuildStartType, BuildEndType, PutArrayType,
                   RememberType, BuildTypeDeclaration, RememberConstant,
                   GetBooleanType, GetBooleanFalse, GetBooleanTrue,
                   BuildSize, MarkFunctionReferenced,
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
                     todolist, tobesolvedbyquads) ;


(* %%%FORWARD%%%
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
PROCEDURE DeclareTypesAndConstantsInRange (start, end: CARDINAL) ; FORWARD ;
PROCEDURE IsTypeQ (sym: CARDINAL; q: IsAction) : BOOLEAN ; FORWARD ;
PROCEDURE IsFullyDeclared (sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE TypeConstFullyDeclared (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE CanCompleteDeclarationOf (sym: CARDINAL) : Tree ; FORWARD ;
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
   %%%FORWARD%%% *)

CONST
   Debugging = FALSE ;

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
   ToDoList            : List ;     (* Contains a list of all         *)
                                    (* outstanding types that need to *)
                                    (* be declared to GCC once        *)
                                    (* its dependants have            *)
                                    (* been written.                  *)
   AnotherType         : CARDINAL ; (* The number of AnotherTypes     *)
                                    (* that have been produced.       *)
   HaveInitDefaultTypes: BOOLEAN ;  (* have we initialized them yet?  *)
   WatchList           : List ;     (* List of symbols being watched  *)
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
   DebugList - 
*)

PROCEDURE DebugList (a: ARRAY OF CHAR; l: List) ;
BEGIN
   printf0(a) ;
   printf0(' {') ;
   ForeachItemInListDo(l, PrintSym) ;
   printf0('}\n')
END DebugList ;


(*
   DebugLists - 
*)

PROCEDURE DebugLists ;
BEGIN
   DebugList('ToDoList', ToDoList) ;
   DebugList('PartiallyDeclared', PartiallyDeclared) ;
   DebugList('FullyDeclared', FullyDeclared) ;
   DebugList('NilTypedArrays', NilTypedArrays)
END DebugLists ;


(*
   AddSymToWatch - adds symbol, sym, to the list of symbols
                   to watch and annotate their movement between
                   lists.
*)

PROCEDURE AddSymToWatch (sym: WORD) ;
BEGIN
   IF NOT IsItemInList(WatchList, sym)
   THEN
      IncludeItemIntoList(WatchList, sym) ;
      WalkDependants(sym, AddSymToWatch) ;
      printf1("watching symbol %d\n", sym) ;
      FIO.FlushBuffer(FIO.StdOut)
   END
END AddSymToWatch ;


(*
   WatchIncludeList - include a symbol onto the list first checking
                      whether it is already on the list and
                      displaying a debug message if the list is
                      changed.
*)

PROCEDURE WatchIncludeList (sym: CARDINAL; lt: ListType) ;
BEGIN
   IF IsItemInList(WatchList, sym)
   THEN
      CASE lt OF

      tobesolvedbyquads :  IF NOT IsItemInList(ToBeSolvedByQuads, sym)
                           THEN
                              printf1("symbol %d -> ToBeSolvedByQuads\n", sym) ;
                              FIO.FlushBuffer(FIO.StdOut) ;
                              IncludeItemIntoList(ToBeSolvedByQuads, sym)
                           END |
      fullydeclared     :  IF NOT IsItemInList(FullyDeclared, sym)
                           THEN
                              printf1("symbol %d -> FullyDeclared\n", sym) ;
                              FIO.FlushBuffer(FIO.StdOut) ;
                              IncludeItemIntoList(FullyDeclared, sym)
                              ; IF sym=644
                              THEN
                                 mystop
                              END
                           END |
      partiallydeclared :  IF NOT IsItemInList(PartiallyDeclared, sym)
                           THEN
                              printf1("symbol %d -> PartiallyDeclared\n", sym) ;
                              FIO.FlushBuffer(FIO.StdOut) ;
                              IncludeItemIntoList(PartiallyDeclared, sym)
                           END |
      todolist          :  IF NOT IsItemInList(ToDoList, sym)
                           THEN
                              printf1("symbol %d -> ToDoList\n", sym) ;
                              FIO.FlushBuffer(FIO.StdOut) ;
                              IncludeItemIntoList(ToDoList, sym)
                           END |
      niltypedarrays    :  IF NOT IsItemInList(NilTypedArrays, sym)
                           THEN
                              printf1("symbol %d -> NilTypedArrays\n", sym) ;
                              FIO.FlushBuffer(FIO.StdOut) ;
                              IncludeItemIntoList(NilTypedArrays, sym)
                           END

      ELSE
         InternalError('unknown list', __FILE__, __LINE__)
      END
   ELSE
      CASE lt OF

      tobesolvedbyquads :  IncludeItemIntoList(ToBeSolvedByQuads, sym) |
      fullydeclared     :  IncludeItemIntoList(FullyDeclared, sym) |
      partiallydeclared :  IncludeItemIntoList(PartiallyDeclared, sym) |
      todolist          :  IncludeItemIntoList(ToDoList, sym) |
      niltypedarrays    :  IncludeItemIntoList(NilTypedArrays, sym)

      ELSE
         InternalError('unknown list', __FILE__, __LINE__)
      END
   END
END WatchIncludeList ;


(*
   WatchRemoveList - remove a symbol onto the list first checking
                     whether it is already on the list and
                     displaying a debug message if the list is
                     changed.
*)

PROCEDURE WatchRemoveList (sym: CARDINAL; lt: ListType) ;
BEGIN
   IF IsItemInList(WatchList, sym)
   THEN
      CASE lt OF

      tobesolvedbyquads :  IF IsItemInList(ToBeSolvedByQuads, sym)
                           THEN
                              printf1("symbol %d off ToBeSolvedByQuads\n", sym) ;
                              FIO.FlushBuffer(FIO.StdOut) ;
                              RemoveItemFromList(ToBeSolvedByQuads, sym)
                           END |
      fullydeclared     :  IF IsItemInList(FullyDeclared, sym)
                           THEN
                              printf1("symbol %d off FullyDeclared\n", sym) ;
                              FIO.FlushBuffer(FIO.StdOut) ;
                              RemoveItemFromList(FullyDeclared, sym)
                           END |
      partiallydeclared :  IF IsItemInList(PartiallyDeclared, sym)
                           THEN
                              printf1("symbol %d off PartiallyDeclared\n", sym) ;
                              FIO.FlushBuffer(FIO.StdOut) ;
                              RemoveItemFromList(PartiallyDeclared, sym)
                           END |
      todolist          :  IF IsItemInList(ToDoList, sym)
                           THEN
                              printf1("symbol %d off ToDoList\n", sym) ;
                              FIO.FlushBuffer(FIO.StdOut) ;
                              RemoveItemFromList(ToDoList, sym)
                           END |
      niltypedarrays    :  IF IsItemInList(NilTypedArrays, sym)
                           THEN
                              printf1("symbol %d off NilTypedArrays\n", sym) ;
                              FIO.FlushBuffer(FIO.StdOut) ;
                              RemoveItemFromList(NilTypedArrays, sym)
                           END

      ELSE
         InternalError('unknown list', __FILE__, __LINE__)
      END
   ELSE
      CASE lt OF

      tobesolvedbyquads :  RemoveItemFromList(ToBeSolvedByQuads, sym) |
      fullydeclared     :  RemoveItemFromList(FullyDeclared, sym) |
      partiallydeclared :  RemoveItemFromList(PartiallyDeclared, sym) |
      todolist          :  RemoveItemFromList(ToDoList, sym) |
      niltypedarrays    :  RemoveItemFromList(NilTypedArrays, sym)

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
   CanDeclareTypePartially - return TRUE if we are able to make a
                             gcc partially created type.
*)

PROCEDURE CanDeclareTypePartially (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsProcType(sym) OR IsRecord(sym) OR
           (IsType(sym) AND IsNilTypedArrays(GetType(sym))) )
END CanDeclareTypePartially ;


(*
   DeclareTypePartially - create the gcc partial type symbol from, sym.
*)

PROCEDURE DeclareTypePartially (sym: CARDINAL) ;
VAR
   t: Tree ;
BEGIN
   IF IsRecord(sym)
   THEN
      t := DoStartDeclaration(sym, BuildStartRecord)
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
   WatchIncludeList(sym, todolist)
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
   PreAddModGcc(sym, BuildStartArrayType(BuildIndex(sym), NIL)) ;
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
   RETURN( IsItemInList(FullyDeclared, sym) )
END CompletelyResolved ;


(*
   IsTypeQ - returns TRUE if all q(dependants) of, sym,
             return TRUE.
*)

PROCEDURE IsTypeQ (sym: CARDINAL; q: IsAction) : BOOLEAN ;
BEGIN
   IF IsEnumeration(sym)
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
   ELSE
      RETURN( TRUE )
   END
END IsTypeQ ;


(*
   IsNilTypedArrays - returns TRUE if, sym, is dependant upon a NIL typed array
*)

PROCEDURE IsNilTypedArrays (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsItemInList(NilTypedArrays, sym) )
END IsNilTypedArrays ;


(*
   IsFullyDeclared - returns TRUE if, sym, is fully declared.
*)

PROCEDURE IsFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsItemInList(FullyDeclared, sym) )
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
   RETURN( IsItemInList(PartiallyDeclared, sym) )
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
   RETURN( IsItemInList(PartiallyDeclared, sym) OR
           IsItemInList(FullyDeclared, sym) )
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
   IF NOT IsItemInList(ToBeSolvedByQuads, sym)
   THEN
      IF IsProcedure(sym) OR IsModule(sym) OR IsDefImp(sym)
      THEN
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
   t := CanCompleteDeclarationOf(sym) ;
   IF t=NIL
   THEN
      InternalError('expecting to be able to create a gcc type',
                    __FILE__, __LINE__)
   ELSE
      AddModGcc(sym, t) ;
      WatchIncludeList(sym, fullydeclared) ;
      WatchRemoveList(sym, partiallydeclared) ;
      DeclareAssociatedUnbounded(sym)
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
      WatchIncludeList(sym, todolist)
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
                             t: ListType; l: List;
                             q: IsAction ; p: WalkAction) : BOOLEAN ;
VAR
   oneResolved,
   noMoreWritten: BOOLEAN ;
   sym          : CARDINAL ;
   n, i         : CARDINAL ;
BEGIN
   oneResolved := FALSE ;
   REPEAT
      noMoreWritten := TRUE ;
      n := NoOfItemsInList(l) ;
      i := 1 ;
      WHILE i<=n DO
      	 sym := GetItemFromList(l, i) ;
         IF q(sym)
         THEN
            WatchRemoveList(sym, t) ;
            p(sym) ;
            (* p(sym) might have replaced sym into the list *)
            IF NOT IsItemInList(l, sym)
            THEN
               i := 0 ;
               noMoreWritten := FALSE ;
               oneResolved := TRUE ;
               DEC(n)
            END
         END ;
         INC(i)
      END
   UNTIL noMoreWritten ;
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
      PreAddModGcc(array, BuildStartArrayType(BuildIndex(array), NIL))
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
   finished: BOOLEAN ;  (* p2c cannot handle LOOP EXIT END *)
BEGIN
   finished := FALSE ;
   REPEAT
      IF ForeachTryDeclare(start, end,
                           todolist, ToDoList,
                           CanDeclareTypePartially,
                           DeclareTypePartially)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              todolist, ToDoList,
                              CanDeclareArrayAsNil,
                              DeclareArrayAsNil)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              todolist, ToDoList,
                              CanDeclarePointerToNilArray,
                              DeclarePointerToNilArray)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              niltypedarrays, NilTypedArrays,
                              CanDeclareArrayPartially,
                              DeclareArrayPartially)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              niltypedarrays, NilTypedArrays,
                              CanPromotePointerFully,
                              PromotePointerFully)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              todolist, ToDoList,
                              TypeConstDependantsFullyDeclared,
                              DeclareTypeConstFully)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              todolist, ToDoList,
                              AllDependantsPartiallyOrFullyDeclared,
                              DeclarePointerTypeFully)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              partiallydeclared, PartiallyDeclared,
                              CanBeDeclaredViaPartialDependants,
                              DeclareTypeFromPartial)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              partiallydeclared, PartiallyDeclared,
                              CanBeDeclaredPartiallyViaPartialDependants,
                              DeclareTypePartially)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare(start, end,
                              partiallydeclared, PartiallyDeclared,
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
                           NotAllDependantsFullyDeclared,
                           EmitCircularDependancyError)
      THEN
      ELSIF ForeachTryDeclare(start, end,
                              partiallydeclared, PartiallyDeclared,
                              NotAllDependantsPartiallyDeclared,
                              EmitCircularDependancyError)
      THEN
      ELSIF ForeachTryDeclare(start, end,
                              niltypedarrays, NilTypedArrays,
                              NotAllDependantsPartiallyDeclared,
                              EmitCircularDependancyError)
      THEN
      END
   END ;
   RETURN( NoOfItemsInList(ToDoList)=0 )
END DeclaredOutstandingTypes ;


(*
   CanCompleteDeclarationOf - returns the GCC Tree for, sym, if it can
                              be created from partially or fully declared
                              dependents.
*)

PROCEDURE CanCompleteDeclarationOf (sym: CARDINAL) : Tree ;
BEGIN
   IF IsRecord(sym)
   THEN
      RETURN( DeclareRecord(sym) )
   ELSIF IsArray(sym)
   THEN
      RETURN( DeclareArray(sym) )
   ELSIF IsProcType(sym)
   THEN
      RETURN( DeclareProcType(sym) )
   ELSE
      RETURN( NIL )
   END
END CanCompleteDeclarationOf ;


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
      DeclareTypesAndConstantsInRange(quad, quad) ;
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
      IF NOT IsItemInList(ToBeSolvedByQuads, sym)
      THEN
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
   size: CARDINAL ;
   t   : Tree ;
BEGIN
   IF IsConst(sym)
   THEN
      TraverseDependants(sym) ;
      type := GetType(sym) ;
      IF (type#NulSym) AND (NOT CompletelyResolved(type))
      THEN
         WatchIncludeList(sym, todolist) ;
         WatchIncludeList(type, todolist) ;
         RETURN
      END ;
      IF IsConstructor(sym) AND (NOT IsConstructorConstant(sym))
      THEN
         WatchIncludeList(sym, todolist) ;
         RETURN
      END ;
      IF (IsConstructor(sym) OR IsConstSet(sym)) AND (type=NulSym)
      THEN
         WatchIncludeList(sym, todolist) ;
         RETURN
      END ;
      IF IsItemInList(ToBeSolvedByQuads, sym)
      THEN
         t := NIL
      ELSE
         t := DeclareConst(tokenno, sym)
      END ;
      IF t=NIL
      THEN
         WatchIncludeList(sym, todolist)
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
   DeclareConst - declares a const to gcc and returns a Tree.
*)

PROCEDURE DeclareConst (tokenno: CARDINAL; sym: CARDINAL) : Tree ;
VAR
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
         DeclareConstantFromTree(sym, PopIntegerTree())
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


(*
   WalkDependants - walks through all dependants of, Sym,
                    calling, p, for each dependant.
*)

PROCEDURE WalkDependants (sym: CARDINAL; p: WalkAction) ;
BEGIN
   IF IsEnumeration(sym)
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
   END
END WalkDependants ;


(*
   TraverseDependants - walks, sym, dependants.  But it checks
                        to see that, sym, is not on the
                        FullyDeclared and not on the ToDoList.
*)

PROCEDURE TraverseDependants (sym: WORD) ;
BEGIN
   IF (NOT IsItemInList(FullyDeclared, sym)) AND
      (NOT IsItemInList(ToDoList, sym))
   THEN
      WatchIncludeList(sym, todolist) ;
      WalkDependants(sym, TraverseDependants)
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
            WalkTypeInfo(type)
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
   ForeachLocalSymDo(sym, TraverseDependants) ;
   ForeachLocalSymDo(sym, WalkUnboundedProcedureParameters)
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
   WalkRecordFieldDependants - 
*)

PROCEDURE WalkRecordFieldDependants (sym: CARDINAL; p: WalkAction) ;
BEGIN
   Assert(IsRecordField(sym)) ;
   p(GetType(sym))
END WalkRecordFieldDependants ;


(*
   IsRecordFieldDependants - returns TRUE if the record field
                             symbol, sym, p(dependants) all return TRUE.
*)

PROCEDURE IsRecordFieldDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
BEGIN
   RETURN( q(GetType(sym)) )
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
         DEC(i)
      END ;
      IF GetType(Sym)=NulSym
      THEN
         PreAddModGcc(Sym, BuildEndFunctionDeclaration(KeyToCharStar(GetFullSymName(Sym)),
                                                       NIL,
                                                       IsEffectivelyImported(GetMainModule(), Sym) AND
                                                       (GetModuleWhereDeclared(Sym)#GetMainModule()),
                                                       IsProcedureGccNested(Sym)))
      ELSE
         PreAddModGcc(Sym, BuildEndFunctionDeclaration(KeyToCharStar(GetFullSymName(Sym)),
                                                       Mod2Gcc(GetType(Sym)),
                                                       IsEffectivelyImported(GetMainModule(), Sym) AND
                                                       (GetModuleWhereDeclared(Sym)#GetMainModule()),
                                                       IsProcedureGccNested(Sym)))
      END
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
   DeclareTypesAndConstantsInRange - 
*)

PROCEDURE DeclareTypesAndConstantsInRange (start, end: CARDINAL) ;
VAR
   n, m: CARDINAL ;
BEGIN
   IF DisplayQuadruples
   THEN
      DisplayQuadRange(start, end)
   END ;
   REPEAT
      n := NoOfItemsInList(ToDoList) ;
      WHILE ResolveConstantExpressions(DeclareConstFully, start, end) DO
      END ;
      (* we need to evaluate some constant expressions to resolve these types *)
      IF DeclaredOutstandingTypes(FALSE, start, end)
      THEN
      END ;
      m := NoOfItemsInList(ToDoList)
   UNTIL (NOT ResolveConstantExpressions(DeclareConstFully, start, end)) AND
         (n=m)
END DeclareTypesAndConstantsInRange ;


(*
   DeclareTypesAndConstants - 
*)

PROCEDURE DeclareTypesAndConstants (scope: CARDINAL) ;
VAR
   s, t: CARDINAL ;
   sb  : ScopeBlock ;
BEGIN
   sb := InitScopeBlock(scope) ;
   REPEAT
      s := NoOfItemsInList(ToDoList) ;
      (* ForeachLocalSymDo(scope, DeclareTypeInfo) ; *)
      ForeachScopeBlockDo(sb, DeclareTypesAndConstantsInRange) ;
      t := NoOfItemsInList(ToDoList) ;
   UNTIL (s=t) ;
   sb := KillScopeBlock(sb)
END DeclareTypesAndConstants ;


(*
   AssertDeclareTypesAndConstantsInRange - 
*)

PROCEDURE AssertDeclareTypesAndConstantsInRange (start, end: CARDINAL) ;
VAR
   sym,
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := NoOfItemsInList(ToDoList) ;
   WHILE i<=n DO
      sym := GetItemFromList(ToDoList, i) ;
      (* Assert(IsVar(sym)) ; *)
      INC(i)
   END
END AssertDeclareTypesAndConstantsInRange ;


(*
   AssertAllTypesDeclared - 
*)

PROCEDURE AssertAllTypesDeclared (scope: CARDINAL) ;
VAR
   sb: ScopeBlock ;
BEGIN
   sb := InitScopeBlock(scope) ;
   ForeachScopeBlockDo(sb, AssertDeclareTypesAndConstantsInRange) ;
   sb := KillScopeBlock(sb)
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
                                       NIL, FALSE, TRUE) ;
      PreAddModGcc(sym, t) ;
      BuildStartFunctionDeclaration(FALSE) ;
      t := BuildEndFunctionDeclaration(KeyToCharStar(GetModuleFinallyName(sym)),
                                       NIL, FALSE, TRUE) ;
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
   DebugLists ;
   AddSymToWatch(1165) ;  (* watch goes here *)
   DebugLists ;
*)
   (* IncludeItemIntoList(WatchList, 92) ; *)
   (* AddSymToWatch(644) ; *)
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
      DeclareTypesAndConstants(scope) ;
      ForeachInnerModuleDo(scope, DeclareTypesAndConstants) ;
      DeclareLocalVariables(scope) ;
      ForeachInnerModuleDo(scope, DeclareModuleVariables) ;
      AssertAllTypesDeclared(scope) ;
      ForeachProcedureDo(scope, DeclareProcedure) ;
      ForeachInnerModuleDo(scope, StartDeclareScope)
   ELSIF scope#GetMainModule()
   THEN
      DeclareTypesAndConstants(scope) ;
      AssertAllTypesDeclared(scope) ;
      ForeachProcedureDo(scope, DeclareProcedure) ;
      DeclareModuleInit(scope) ;
      ForeachInnerModuleDo(scope, StartDeclareScope)
   ELSE
      ForeachModuleDo(WalkTypesInModule) ;     (* will populate the TYPE and CONST ToDo list  *)
      DeclareTypesAndConstants(scope) ;        (* will resolved TYPEs and CONSTs on the ToDo  *)
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
      ForeachInnerModuleDo(scope, DeclareTypesAndConstants) ;
      (* --end of test-- *)
      ForeachInnerModuleDo(scope, DeclareProcedure) ;
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
   AddModGcc(sym, t) ;
   DeclareAssociatedUnbounded(sym)
END PreAddModGcc ;


(*
   DeclareAssociatedUnbounded - if an unbounded symbol exists then
                                walk its dependants and declare them.
*)

PROCEDURE DeclareAssociatedUnbounded (sym: CARDINAL) ;
VAR
   unbounded: CARDINAL ;
BEGIN
   unbounded := GetUnbounded(sym) ;
   IF unbounded#NulSym
   THEN
      WalkTypeInfo(sym) ;
      WalkTypeInfo(unbounded) ;
      DeclareTypesAndConstantsInRange(0, 0) ;
      AddModGcc(unbounded, DeclareUnbounded(unbounded)) ;
      WatchRemoveList(unbounded, todolist)
   END
END DeclareAssociatedUnbounded ;


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
   IncludeItemIntoList(FullyDeclared, sym) ;
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
   PreAddModGcc(Boolean, GetBooleanType()) ;
   PreAddModGcc(True, GetBooleanTrue()) ;
   PreAddModGcc(False, GetBooleanFalse()) ;
   IncludeItemIntoList(FullyDeclared, Boolean) ;
   IncludeItemIntoList(FullyDeclared, True) ;
   IncludeItemIntoList(FullyDeclared, False)
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
         IncludeItemIntoList(FullyDeclared, typetype)
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
   IncludeItemIntoList(FullyDeclared, ZType) ;
   IncludeItemIntoList(FullyDeclared, RType) ;
   IncludeItemIntoList(FullyDeclared, CType) ;

   DeclareDefaultType(Integer     , "INTEGER"     , GetM2IntegerType()) ;
   DeclareDefaultType(Char        , "CHAR"        , GetM2CharType()) ;
   DeclareDefaultType(Cardinal    , "CARDINAL"    , GetM2CardinalType()) ;
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
   DeclareDefaultUnboundedTypes - declare the unbounded types associated with
                                  the default simple types.
*)

PROCEDURE DeclareDefaultUnboundedTypes ;
BEGIN
   DeclareAssociatedUnbounded(Integer) ;
   DeclareAssociatedUnbounded(Char) ;
   DeclareAssociatedUnbounded(Cardinal) ;

   IF Iso
   THEN
      DeclareAssociatedUnbounded(Loc) ;
      DeclareAssociatedUnbounded(Byte) ;
      DeclareAssociatedUnbounded(Word) ;
   ELSE
      DeclareAssociatedUnbounded(Byte) ;
      DeclareAssociatedUnbounded(Word) ;
   END ;
   
   DeclareAssociatedUnbounded(Proc) ;
   DeclareAssociatedUnbounded(Address) ;
   DeclareAssociatedUnbounded(LongInt) ;
   DeclareAssociatedUnbounded(LongCard) ;
   DeclareAssociatedUnbounded(ShortInt) ;
   DeclareAssociatedUnbounded(ShortCard) ;
   DeclareAssociatedUnbounded(ShortReal) ;
   DeclareAssociatedUnbounded(Real) ;
   DeclareAssociatedUnbounded(LongReal) ;
   DeclareAssociatedUnbounded(Bitnum) ;
   DeclareAssociatedUnbounded(Bitset) ;
   DeclareAssociatedUnbounded(Boolean)
   
END DeclareDefaultUnboundedTypes ;



(*
   DeclareDefaultTypes - makes default types known to GCC
*)

PROCEDURE DeclareDefaultTypes ;
BEGIN
   IF NOT HaveInitDefaultTypes
   THEN
      HaveInitDefaultTypes := TRUE ;

      DeclareDefaultSimpleTypes ;
      DeclareDefaultUnboundedTypes
   END
END DeclareDefaultTypes ;


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
          IsImported(ModSym, GetModuleWhereDeclared(sym))
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
      Assert(AllDependantsFullyDeclared(GetType(Var))) ;
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
   IF (GetModuleWhereDeclared(sym)=NulSym) OR
      (GetModuleWhereDeclared(sym)=GetMainModule())
   THEN
      field := BuildEnumerator(KeyToCharStar(GetSymName(sym)),
                               PopIntegerTree(),
                               enumlist)
   ELSE
      field := BuildEnumerator(KeyToCharStar(GetFullScopeAsmName(sym)),
                               PopIntegerTree(),
                               enumlist)
   END ;
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
   gccenum := BuildStartEnumeration(KeyToCharStar(GetFullSymName(sym))) ;
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
      IncludeType(l, sym)
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
      IncludeType(l, sym)
   ELSIF IsRecord(sym)
   THEN
      printf2('sym %d IsRecord (%a)', sym, n) ;
      PrintLocalSymbols(sym) ;
      IncludeGetNth(l, sym)
   ELSIF IsVarient(sym)
   THEN
      printf2('sym %d IsVarient (%a)', sym, n) ;
      IncludeGetNth(l, sym) ;
      IncludeGetVarient(l, sym)
   ELSIF IsFieldVarient(sym)
   THEN
      printf2('sym %d IsFieldVarient (%a)', sym, n) ;
      IncludeGetNth(l, sym) ;
      IncludeGetVarient(l, sym)
   ELSIF IsFieldEnumeration(sym)
   THEN
      printf2('sym %d IsFieldEnumeration (%a)', sym, n)
   ELSIF IsArray(sym)
   THEN
      printf2('sym %d IsArray (%a)', sym, n) ;
      IncludeSubscript(l, sym) ;
      IncludeType(l, sym)
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
   ELSIF IsRecordField(sym)
   THEN
      printf2('sym %d IsRecordField (%a)', sym, n) ;
      IncludeType(l, sym) ;
      IncludeGetVarient(l, sym)
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
      printf0(' constructor ')
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
      printf2('sym %d IsTuple (%a)', sym, n)
   ELSIF IsGnuAsmVolatile(sym)
   THEN
      printf2('sym %d IsGnuAsmVolatile (%a)', sym, n)
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


PROCEDURE stop ; BEGIN END stop ;


(*
   DeclareVarient - 
*)

PROCEDURE DeclareVarient (sym: CARDINAL) : Tree ;
VAR
   GccFieldType: Tree ;
BEGIN
   IF IsVarient(sym)
   THEN
      GccFieldType := PostDeclareVarient(sym)
   ELSE
      GccFieldType := Mod2Gcc(GetType(sym))
   END ;
   RETURN( BuildFieldRecord(KeyToCharStar(GetFullSymName(sym)), GccFieldType) )
END DeclareVarient ;


(*
   DeclareFieldVarient - 
*)

PROCEDURE DeclareFieldVarient (sym: CARDINAL) : Tree ;
VAR
   i           : CARDINAL ;
   Field       : CARDINAL ;
   FieldList,
   GccFieldType,
   GccField,
   RecordType  : Tree ;
BEGIN
   RecordType := BuildStartRecord(NIL) ;
   i := 1 ;
   FieldList := Tree(NIL) ;
   REPEAT
      Field := GetNth(sym, i) ;
      IF Field#NulSym
      THEN
         Assert(Mod2Gcc(Field)#NIL) ;
         FieldList := ChainOn(FieldList, Mod2Gcc(Field)) ;
         INC(i)
      END
   UNTIL Field=NulSym ;
   GccFieldType := BuildEndRecord(RecordType, FieldList) ;
   GccField := BuildFieldRecord(KeyToCharStar(GetFullSymName(sym)), GccFieldType) ;
   RETURN( GccField )
END DeclareFieldVarient ;


(*
   PostDeclareVarient - declares a varient record to gcc and returns the gcc representation.
*)

PROCEDURE PostDeclareVarient (sym: CARDINAL) : Tree ;
VAR
   i           : CARDINAL ;
   Field       : CARDINAL ;
   VarientList,
   VarientType : Tree ;
BEGIN
   i := 1 ;
   VarientList := Tree(NIL) ;
   VarientType := BuildStartVarientRecord(KeyToCharStar(GetFullSymName(sym))) ;
   (* no need to store the [sym, RecordType] tuple as it is stored by DeclareRecord which calls us *)
   REPEAT
      Field := GetNth(sym, i) ;
      IF Field#NulSym
      THEN
      	 Assert(IsFieldVarient(Field)) ;
         VarientList := ChainOn(VarientList, Mod2Gcc(Field))
      END ;
      INC(i)
   UNTIL Field=NulSym ;
   WatchRemoveList(sym, partiallydeclared) ;
   RETURN( BuildEndRecord(VarientType, VarientList) )
END PostDeclareVarient ;


(*
   DeclareRecord - declares a record and its fields to gcc.
                   The final gcc record type is returned.
*)

PROCEDURE DeclareRecord (Sym: CARDINAL) : Tree ;
VAR
   Field       : CARDINAL ;
   i           : CARDINAL ;
   FieldList,
   RecordType  : Tree ;
BEGIN
   i := 1 ;
   FieldList := Tree(NIL) ;
   RecordType := DoStartDeclaration(Sym, BuildStartRecord) ;
   REPEAT
      Field := GetNth(Sym, i) ;
      IF Field#NulSym
      THEN
         FieldList := ChainOn(FieldList, Mod2Gcc(Field))
      END ;
      INC(i)
   UNTIL Field=NulSym ;
   WatchRemoveList(Sym, partiallydeclared) ;
   RETURN( BuildEndRecord(RecordType, FieldList) )
END DeclareRecord ;


(*
   DeclareRecordField - 
*)

PROCEDURE DeclareRecordField (sym: CARDINAL) : Tree ;
VAR
   GccFieldType: Tree ;
BEGIN
   IF IsVarient(sym)
   THEN
      Assert(AllDependantsFullyDeclared(sym)) ;
      GccFieldType := PostDeclareVarient(sym)
   ELSIF IsFieldVarient(sym)
   THEN
      MetaError1('found unexpected field varient name {%1a}\n', sym) ;
      InternalError('unexpected varient record data structure', __FILE__, __LINE__)
   ELSE
      GccFieldType := Mod2Gcc(GetType(sym))
   END ;
   RETURN( BuildFieldRecord(KeyToCharStar(GetFullSymName(sym)), GccFieldType) )
END DeclareRecordField ;


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
   ArrayType,
   GccArray,
   GccIndex : Tree ;
   Subscript: CARDINAL ;
BEGIN
   Assert(IsArray(Sym)) ;

   Subscript := GetArraySubscript(Sym) ;
   GccArray := Mod2Gcc(GetType(Sym)) ;
   GccIndex := BuildIndex(Sym) ;

   IF GccKnowsAbout(Sym)
   THEN
      ArrayType := Mod2Gcc(Sym)
   ELSE
      ArrayType := BuildStartArrayType(GccIndex, ArrayType)
   END ;

   PreAddModGcc(Subscript, GccArray) ;       (* we save the type of this array as the subscript *)
   PushIntegerTree(BuildSize(GccArray, FALSE)) ;  (* and the size of this array so far *)
   PopSize(Subscript) ;

   GccArray := BuildEndArrayType(ArrayType, GccArray, GccIndex) ;

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
   RETURN( BuildEndRecord(RecordType, FieldList) )
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
      t := DeclareRecord(sym)
   ELSIF IsRecordField(sym)
   THEN
      t := DeclareRecordField(sym)
   ELSIF IsFieldVarient(sym)
   THEN
      t := DeclareFieldVarient(sym)
   ELSIF IsVarient(sym)
   THEN
      t := DeclareVarient(sym)
   ELSIF IsPointer(sym)
   THEN
      t := DeclarePointer(sym)
   ELSIF IsUnbounded(sym)
   THEN
      t := DeclareUnbounded(sym)
   ELSIF IsArray(sym)
   THEN
      t := DeclareArray(sym)
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
      t := DeclareType(sym)
   END ;
   IF GetSymName(sym)#NulName
   THEN
      IF Debugging
      THEN
         n := GetSymName(sym) ;
         printf1('declaring type %a\n', n)
      END ;
      t := RememberType(t) ;
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
   WalkVarDependants - walks all dependants of sym.
*)

PROCEDURE WalkVarDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   type: CARDINAL ;
BEGIN
   p(GetType(sym)) ;
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
   IsRecordDependants - returns TRUE if the symbol, sym,
                        q(dependants) all return TRUE.
*)

PROCEDURE IsRecordDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   result: BOOLEAN ;
   i     : CARDINAL ;
   field : CARDINAL ;
BEGIN
   result := TRUE ;
   i := 1 ;
   REPEAT
      field := GetNth(sym, i) ;
      IF field#NulSym
      THEN
         IF IsRecordField(field)
         THEN
            IF NOT q(field)
            THEN
               result := FALSE
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
   WalkRecordDependants - walks symbol, sym, dependants.
*)

PROCEDURE WalkRecordDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   i    : CARDINAL ;
   type,
   Field: CARDINAL ;
BEGIN
   i := 1 ;
   REPEAT
      Field := GetNth(sym, i) ;
      IF Field#NulSym
      THEN
         type := GetType(Field) ;
         IF IsRecordField(Field)
         THEN
            p(Field)
         ELSIF IsVarient(Field)
         THEN
            p(Field)
      	 ELSIF IsFieldVarient(Field)
      	 THEN
            InternalError('should not see a field varient', __FILE__, __LINE__)
         ELSE
            InternalError('unknown symbol in record', __FILE__, __LINE__)
      	 END
      END ;
      INC(i)
   UNTIL Field=NulSym
END WalkRecordDependants ;


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
   result := TRUE ;
   i := 1 ;
   REPEAT
      Field := GetNth(sym, i) ;
      IF Field#NulSym
      THEN
      	 Assert(IsFieldVarient(Field)) ;
         IF NOT q(Field)
         THEN
            result := FALSE
         END
      END ;
      INC(i)
   UNTIL Field=NulSym ;
   RETURN( result )
END IsVarientDependants ;


(*
   WalkVarientDependants - walks symbol, sym, dependants.
*)

PROCEDURE WalkVarientDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   i    : CARDINAL ;
   Field: CARDINAL ;
BEGIN
   i := 1 ;
   REPEAT
      Field := GetNth(sym, i) ;
      IF Field#NulSym
      THEN
      	 Assert(IsFieldVarient(Field)) ;
         p(Field)
      END ;
      INC(i)
   UNTIL Field=NulSym
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
   result := TRUE ;
   REPEAT
      Field := GetNth(sym, i) ;
      IF Field#NulSym
      THEN
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
      END
   UNTIL Field=NulSym ;
   RETURN( result )
END IsVarientFieldDependants ;


(*
   WalkVarientFieldDependants - 
*)

PROCEDURE WalkVarientFieldDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   i   :  CARDINAL ;
   type,
   Field: CARDINAL ;
BEGIN
   i := 1 ;
   REPEAT
      Field := GetNth(sym, i) ;
      IF Field#NulSym
      THEN
         p(Field) ;
         type := GetType(Field) ;
         IF type#NulSym
         THEN
            p(type)
         END ;
         INC(i)
      END
   UNTIL Field=NulSym
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
   DeclareDefaultTypes
END InitDeclarations ;


BEGIN
   InitList(ToDoList) ;
   InitList(FullyDeclared) ;
   InitList(PartiallyDeclared) ;
   InitList(NilTypedArrays) ;
   InitList(ToBeSolvedByQuads) ;
   InitList(WatchList) ;
   EnumerationIndex := InitIndex(1) ;
   HaveInitDefaultTypes := FALSE
END M2GCCDeclare.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I.:../gm2-libs:../gm2-libs-ch:../gm2-libiberty/ M2GCCDeclare.mod"
 * End:
 *)
