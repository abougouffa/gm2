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
FROM M2Options IMPORT GenerateDebugging, GenerateLineDebug, Iso ;
FROM NameKey IMPORT Name, MakeKey, NulName, KeyToCharStar, makekey ;
FROM M2AsmUtil IMPORT WriteAsmName, WriteName, GetAsmName, GetFullSymName, UnderScoreString, GetModuleInitName, GetFullScopeAsmName ;
FROM M2FileName IMPORT CalculateFileName ;
FROM M2Configure IMPORT PushParametersLeftToRight ;
FROM DynamicStrings IMPORT String, string, InitString, KillString, InitStringCharStar, Mark ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3 ;
FROM M2LexBuf IMPORT TokenToLineNo, FindFileNameFromToken ;
FROM M2Error IMPORT Error, NewError, FlushErrors, ErrorFormat0, ErrorFormat1, InternalError, WriteFormat1, WriteFormat3 ;
FROM M2Printf IMPORT printf0, printf1, printf2 ;

FROM Lists IMPORT List, InitList, IncludeItemIntoList,
                  PutItemIntoList, GetItemFromList,
                  RemoveItemFromList,
      	       	  IsItemInList, NoOfItemsInList ;

FROM SymbolTable IMPORT NulSym,
                        ModeOfAddr,
                        GetMode,
                        GetScope,
                        GetNth, GetType,
                        MakeType, PutType, MakeConstLit,
      	       	     	GetSubrange, PutSubrange,
      	       	     	NoOfParam, GetNthParam,
                        PushValue, PopSize,
                        IsTemporary, IsUnbounded, IsEnumeration, IsVar,
      	       	     	IsSubrange, IsPointer, IsRecord, IsArray,
                        IsProcedure, IsProcedureNested, IsModule, IsDefImp,
      	       	     	IsSubscript, IsVarient, IsFieldVarient,
      	       	     	IsType, IsProcType, IsSet, IsConst, IsConstSet,
                        IsFieldEnumeration,
                        IsExported, IsImported,
                        IsVarParam, IsRecordField, IsUnboundedParam,
                        IsValueSolved,
      	       	     	GetMainModule, GetBaseModule, GetModule,
                        IsAModula2Type,
                        GetSymName,
                        GetDeclared,
                        GetString, GetStringLength, IsConstString,
                        ForeachLocalSymDo, ForeachFieldEnumerationDo,
      	       	     	ForeachProcedureDo, ForeachModuleDo,
                        ForeachInnerModuleDo, ForeachImportedDo, ForeachExportedDo ;

FROM M2Base IMPORT IsPseudoBaseProcedure, IsPseudoBaseFunction, GetBaseTypeMinMax,
                   Cardinal, Char, Proc, Integer, Unbounded, LongInt, LongCard, Real, LongReal, ShortReal, Boolean, True, False,
                   ArrayAddress, ArrayHigh ;
FROM M2System IMPORT IsPseudoSystemFunction, IsSystemType, GetSystemTypeMinMax, Address, Word, Byte, Loc ;
FROM M2Bitset IMPORT Bitset, Bitnum ;
FROM SymbolConversion IMPORT AddModGcc, Mod2Gcc, GccKnowsAbout, Poison ;
FROM M2GenGCC IMPORT ResolveConstantExpressions ;
FROM M2Scope IMPORT ScopeBlock, InitScopeBlock, KillScopeBlock, ForeachScopeBlockDo ;

FROM M2ALU IMPORT Addn, Sub, Equ, GreEqu, Gre, Less, PushInt, PushCard,
                  PushIntegerTree, PopIntegerTree, PopRealTree, ConvertToInt, PopSetTree,
                  CollectSetDependants ;

FROM gccgm2 IMPORT Tree,
                   SetFileNameAndLineNo,
                   DeclareKnownType, DeclareKnownVariable,
                   GetDefaultType, GetCopyOfType,
                   GetIntegerType, GetCharType, GetM2CharType,
                   GetVoidType, GetIntegerZero, GetIntegerOne, GetCurrentFunction,
                   GetPointerType, GetM2LongIntType, GetM2LongCardType,
                   GetM2RealType, GetM2ShortRealType, GetM2LongRealType,
                   GetProcType, GetCardinalType, GetWordType, GetByteType,
                   GetBitsetType, GetBitnumType, GetMinFrom, GetMaxFrom, GetBitsPerWord,
                   GetM2IntegerType, GetM2CardinalType,
                   GetISOLocType, GetISOByteType, GetISOWordType,
                   BuildStartEnumeration, BuildEndEnumeration, BuildEnumerator,
                   BuildIntegerConstant, BuildStringConstant, BuildCharConstant,
                   BuildSubrangeType,
                   BuildStartRecord, BuildEndRecord, BuildStartVarientRecord, BuildFieldRecord,
                   BuildArrayIndexType, BuildArrayType, BuildSetType,
                   DebugTree,
                   ChainOn,
                   BuildPointerType,
                   BuildStartFunctionType, BuildEndFunctionType,
                   InitFunctionTypeParameters,
                   BuildParameterDeclaration,
                   BuildStartFunctionDeclaration, BuildEndFunctionDeclaration,
                   BuildStartMainModule, BuildEndMainModule,
                   AssignBooleanTrueFalse, BuildSize ;

(* %%%FORWARD%%%
PROCEDURE DeclareFileName ; FORWARD ;
PROCEDURE IsUnboundedDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE DeclareImportedVariables (Sym: WORD) ; FORWARD ;
PROCEDURE DeclareSet (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclarePointer (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclareLocalVariables (Sym: CARDINAL; i: CARDINAL) ; FORWARD ;
PROCEDURE DeclareDefaultTypes ; FORWARD ;
PROCEDURE DeclareGlobalVariables (ModSym: WORD) ; FORWARD ;
PROCEDURE DeclareType (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclareKindOfType (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclareOrFindKindOfType (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE IsEnumerationDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsSubrangeDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsPointerDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsRecordDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsVarientDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsArrayDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsSetDependantsWritten (sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsTypeDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsProcTypeDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE DeclareEnumeration (Sym: WORD) : Tree ; FORWARD ;
PROCEDURE AllDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE DeclareVarient (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE ForceDeclareType (sym: CARDINAL) : Tree ; FORWARD ;
   %%%FORWARD%%% *)

CONST
   Debugging       = FALSE ;
   DebugFinishList = FALSE ;

TYPE
   StartProcedure = PROCEDURE (ADDRESS) : Tree ;

VAR
   ToFinishList,                    (* those types which have need to *)
                                    (* be finished (but already       *)
                                    (* started: records & function    *)
                                    (* types).                        *)
   ToDoList            : List ;     (* Contains a list of all         *)
                                    (* outstanding types that need to *)
                                    (* be declared to GCC once        *)
                                    (* its dependants have            *)
                                    (* been written.                  *)
   ToDoConstants       : List ;     (* all unresolved constants go    *)
                                    (* here, M2GenGCC resolves them.  *)
   DefinedList         : List ;     (* those types which have been    *)
                                    (* declared to GCC.               *)
   AnotherType         : CARDINAL ; (* The number of AnotherTypes     *)
                                    (* that have been produced.       *)
   HaveInitDefaultTypes: BOOLEAN ;  (* have we initialized them yet?  *)



PROCEDURE mystop ; BEGIN END mystop ;


(*
   DoStartDeclaration - returns a tree representing a symbol which has
                        not yet been finished. (Useful when declaring
                        recursive types).
*)

PROCEDURE DoStartDeclaration (sym: CARDINAL; p: StartProcedure) : Tree ;
BEGIN
   IF NOT GccKnowsAbout(sym)
   THEN
      AddModGcc(sym, p(KeyToCharStar(GetSymName(sym)))) ;
      IncludeItemIntoList(ToFinishList, sym) ;
      IncludeItemIntoList(ToDoList, sym)
   END ;
   RETURN( Mod2Gcc(sym) )
END DoStartDeclaration ;


(*
   CompletelyResolved - returns TRUE if a symbols has been completely resolved
                        and is not partically declared (such as a record).
*)

PROCEDURE CompletelyResolved (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( GccKnowsAbout(sym) AND (NOT IsItemInList(ToFinishList, sym)) )
END CompletelyResolved ;


(*
   CheckToFinishList - checks the ToFinishList to determine whether we can complete
                       and partially built records or varient record.
*)

PROCEDURE CheckToFinishList (MustBeResolved: BOOLEAN) ;
VAR
   Sym,
   i, n: CARDINAL ;
BEGIN
   IF DebugFinishList
   THEN
      printf0('ToFinishList { ')
   END ;
   i := 1 ;
   n := NoOfItemsInList(ToFinishList) ;
   WHILE i<=n DO
      Sym := GetItemFromList(ToFinishList, i) ;
      IF DebugFinishList
      THEN
         printf2('%d %a, ', Sym, GetSymName(Sym))
      END ;
      INC(i)
   END ;
   IF DebugFinishList
   THEN
      printf0('}\n')
   END ;

   i := 1 ;
   n := NoOfItemsInList(ToFinishList) ;
   WHILE i<=n DO
      Sym := GetItemFromList(ToFinishList, i) ;
      IF AllDependantsWritten(Sym)
      THEN
         IF Mod2Gcc(Sym)#DeclareKindOfType(Sym)
         THEN
            InternalError('gcc has returned a different symbol on completion of a type', __FILE__, __LINE__)
            (* the solution is to allow:          AddModGcc(Sym, DeclareKindOfType(Sym)) *)
         END ;
         RemoveItemFromList(ToFinishList, Sym) ;
         n := NoOfItemsInList(ToFinishList) ;
         i := 0 ;
      END ;
      INC(i)
   END ;
   IF MustBeResolved AND (NoOfItemsInList(ToFinishList)#0)
   THEN
      printf0('cannot resolve the following: ') ;
      i := 1 ;
      n := NoOfItemsInList(ToFinishList) ;
      WHILE i<=n DO
         Sym := GetItemFromList(ToFinishList, i) ;
         IF DebugFinishList
         THEN
            printf2('%d %a, ', Sym, GetSymName(Sym))
         END ;
         INC(i)
      END ;
      IF DebugFinishList
      THEN
         printf0('}\n')
      END ;
      InternalError('partially declared types are not all resolved', __FILE__, __LINE__)
   END
END CheckToFinishList ;


(*
   DeclaredOutandingTypes - writes out any types that have their dependants
                            solved. It returns TRUE if all outstanding types
                            have been written.
*)

PROCEDURE DeclaredOutstandingTypes (MustHaveCompleted: BOOLEAN; start, end: CARDINAL) : BOOLEAN ;
VAR
   e            : Error ;
   i, n         : CARDINAL ;
   NoMoreWritten: BOOLEAN ;
   Sym          : CARDINAL ;
BEGIN
   REPEAT
      NoMoreWritten := TRUE ;
      n := NoOfItemsInList(ToDoList) ;
      i := 1 ;
      IF DebugFinishList
      THEN
         printf0('ToDoList { ')
      END ;
      WHILE i<=n DO
      	 Sym := GetItemFromList(ToDoList, i) ;
         IF DebugFinishList
         THEN
            printf2('%d %a, ', Sym, GetSymName(Sym))
         END ;
      	 IF GccKnowsAbout(Sym)
      	 THEN
            IF DebugFinishList
            THEN
               printf0('<gccknows> ')
            END
         ELSE
      	    IF AllDependantsWritten(Sym)
      	    THEN
               (* add relationship between gccSym and Sym *)
               AddModGcc(Sym, DeclareKindOfType(Sym)) ;
               IncludeItemIntoList(DefinedList, Sym) ;
               RemoveItemFromList(ToDoList, Sym) ;
               i := 0 ;
      	       NoMoreWritten := FALSE ;
               IF DebugFinishList
               THEN
                  printf0('<resolved> ')
               END
            ELSE
               IF DebugFinishList
               THEN
                  printf0('<unresolved> ')
               END
       	    END ;
            IF n#NoOfItemsInList(ToDoList)
            THEN
               i := 0 ;
               n := NoOfItemsInList(ToDoList) ;
               IF DebugFinishList
               THEN
                  printf0('}\nToDoList { ')
               END
            END
      	 END ;
         INC(i)
      END
   UNTIL NoMoreWritten ;

   IF DebugFinishList
   THEN
      printf0('}\n')
   END ;

   CheckToFinishList(MustHaveCompleted) ;

   IF MustHaveCompleted
   THEN
      (*
         self checking code
      *)
      NoMoreWritten := FALSE ;
      n := NoOfItemsInList(ToDoList) ;
      i := 1 ;
      WHILE i<=n DO
         Sym := GetItemFromList(ToDoList, i) ;
         IF (NOT GccKnowsAbout(Sym)) OR IsItemInList(ToFinishList, Sym)
         THEN
            IF Debugging
            THEN
               printf2('// need to solve %d %a ', Sym, GetSymName(Sym))
            END ;
            IF IsItemInList(ToFinishList, Sym)
            THEN
               IF Debugging
               THEN
                  printf0('partially declared\n')
               END ;
            ELSE
               IF Debugging
               THEN
                  printf0('not declared at all\n')
               END
            END ;
            FoldConstants(start, end) ;
            IF NOT AllDependantsWritten(Sym)
            THEN
               NoMoreWritten := TRUE ;
               e := NewError(GetDeclared(Sym)) ;
               IF GetSymName(Sym)=NulSym
               THEN
                  (* ErrorFormat0(e, 'circular dependancy found') *)
               ELSE
                  ErrorFormat1(e, 'circular dependancy found when trying to resolve symbol %a',
                               GetSymName(Sym))
               END
            END
         END ;
         INC(i)
      END ;
      i := 1 ;
      n := NoOfItemsInList(ToFinishList) ;
      WHILE i<=n DO
         Sym := GetItemFromList(ToFinishList, i) ;
         IF Debugging
         THEN
            printf2('// symbol type (%a) %d has only been partically declared\n',
                    GetSymName(Sym), Sym)
         END ;
         IF NOT AllDependantsWritten(Sym)
         THEN
            e := NewError(GetDeclared(Sym)) ;
            IF GetSymName(Sym)=NulSym
            THEN
               (* ErrorFormat0(e, 'circular dependancy found') *)
            ELSE
               ErrorFormat1(e, 'circular dependancy found when trying to resolve symbol %a',
                            GetSymName(Sym))
            END
         END ;
         INC(i) ;
         NoMoreWritten := TRUE
      END ;
      IF NoMoreWritten
      THEN
         FlushErrors
      END
   END ;

   RETURN( NoOfItemsInList(ToDoList)=0 )
END DeclaredOutstandingTypes ;


(*
   PrintType - prints out the type, Sym, with a leading string
      	       and its name followed by a boolean.
*)

PROCEDURE PrintType (a: ARRAY OF CHAR; Sym: CARDINAL; b: BOOLEAN) ;
BEGIN
(*
   WriteString('// ') ; WriteString(a) ;
   WriteString(' : Sym #') ; WriteCard(Sym, 4) ;
   WriteString(' Name: ') ; WriteKey(GetSymName(Sym)) ;
   IF b
   THEN
      WriteString(' TRUE')
   ELSE
      WriteString('FALSE ')
   END ;
   WriteLn
*)
END PrintType ;


(*
   DeclareType - here a type has been created via TYPE foo = bar,
                 we must tell GCC about it.
*)

PROCEDURE DeclareType (Sym: CARDINAL) : Tree ;
VAR
   t: Tree ;
BEGIN
   IF GetType(Sym)=NulSym
   THEN
      WriteFormat1('base type %a not understood', GetSymName(Sym)) ;
      InternalError('base type should have been declared', __FILE__, __LINE__)
   ELSE
      IF GetSymName(Sym)=NulName
      THEN
         RETURN( Tree(Mod2Gcc(GetType(Sym))) )
      ELSE
         t := DeclareKnownType(KeyToCharStar(GetSymName(Sym)), Mod2Gcc(GetType(Sym))) ;
         RETURN( t )
      END
   END
END DeclareType ;


(*
   DeclareIntegerConstant - declares an integer constant.
*)

PROCEDURE DeclareIntegerConstant (sym: CARDINAL; value: INTEGER) ;
BEGIN
   AddModGcc(sym, BuildIntegerConstant(value))
END DeclareIntegerConstant ;


(*
   DeclareIntegerFromTree - declares an integer constant from a Tree, value.
*)

PROCEDURE DeclareConstantFromTree (sym: CARDINAL; value: Tree) ;
BEGIN
   AddModGcc(sym, value)
END DeclareConstantFromTree ;


(*
   DeclareCharConstant - declares a character constant.
*)

PROCEDURE DeclareCharConstant (sym: CARDINAL) ;
BEGIN
   AddModGcc(sym, BuildCharConstant(KeyToCharStar(GetString(sym))))
END DeclareCharConstant ;


(*
   DeclareStringConstant - declares a string constant.
*)

PROCEDURE DeclareStringConstant (sym: CARDINAL) ;
BEGIN
   AddModGcc(sym, BuildStringConstant(KeyToCharStar(GetString(sym)), GetStringLength(sym)))
END DeclareStringConstant ;


(*
   PromoteToString - declare, sym, and then promote it to a string.
                     Note that if sym is a single character we do *not* record it as a string
                          but as a char however we always return a string constant.
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
      RETURN( BuildStringConstant(KeyToCharStar(GetString(sym)), GetStringLength(sym)) )
   END
END PromoteToString ;


(*
   ResolveConstSet - 
*)

PROCEDURE ResolveConstSet (tokenno: CARDINAL; sym: CARDINAL) ;
VAR
   type,
   size: CARDINAL ;
BEGIN
   type := GetType(sym) ;
   (* ensure that all dependants are on the various to do lists *)
   IF type#NulSym
   THEN
      IF Debugging
      THEN
         printf2('declaring const %a = SET OF %a\n', GetSymName(sym), GetSymName(type))
      END ;
      IF AllDependantsWritten(type) AND CollectSetDependants(tokenno, sym)
      THEN
         IF Debugging
         THEN
            printf2('dependants are all known for %a = SET OF %a\n', GetSymName(sym), GetSymName(type))
         END
      END
   END
END ResolveConstSet ;


(*
   DeclareConstant - checks to see whether, sym, is a constant and declares the constant to gcc.
*)

PROCEDURE DeclareConstant (tokenno: CARDINAL; sym: CARDINAL) ;
VAR
   size: CARDINAL ;
BEGIN
   IF sym=NulSym
   THEN
      InternalError('trying to declare the NulSym', __FILE__, __LINE__)
   END ;
   IF IsConst(sym) AND (NOT GccKnowsAbout(sym))
   THEN
      IF IsConstSet(sym)
      THEN
         ResolveConstSet(tokenno, sym)
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
         ELSIF GetType(sym)=LongReal
         THEN
            DeclareConstantFromTree(sym, PopRealTree())
         ELSE
            DeclareConstantFromTree(sym, PopIntegerTree())
         END
      ELSE
         IncludeItemIntoList(ToDoConstants, sym)
      END
   END
END DeclareConstant ;


(*
   IsSymTypeKnown - returns TRUE if the, type, of symbol, sym is known to GCC.
                    It adds all of syms dependants to the ToDoList if they are unknown.
*)

PROCEDURE IsSymTypeKnown (sym, type: CARDINAL) : BOOLEAN ;
BEGIN
   IF (type#NulSym) AND (NOT GccKnowsAbout(type))
   THEN
      (* legal symbol which is unknown by gcc *)
      IF NOT AllDependantsWritten(type)
      THEN
         IF NOT IsVarient(sym)
         THEN
            IncludeItemIntoList(ToDoList, sym)
         END
      END ;
      RETURN( FALSE )
   ELSE
      RETURN( TRUE )
   END
END IsSymTypeKnown ;


(*
   AllDependantsWritten - returns true if the symbol, Sym, and
      	       	     	  all its dependants have been written
      	       	     	  out to the assembly file.
*)

PROCEDURE AllDependantsWritten (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF Sym=149
   THEN
      mystop
   END ;
   IF GccKnowsAbout(Sym) AND (NOT IsItemInList(ToFinishList, Sym))
   THEN
      RETURN( TRUE )
   ELSIF IsEnumeration(Sym)
   THEN
      RETURN( IsEnumerationDependantsWritten(Sym) )
   ELSIF IsSubrange(Sym)
   THEN
      RETURN( IsSubrangeDependantsWritten(Sym) )
   ELSIF IsPointer(Sym)
   THEN
      RETURN( IsPointerDependantsWritten(Sym) )
   ELSIF IsRecord(Sym)
   THEN
      RETURN( IsRecordDependantsWritten(Sym) )
   ELSIF IsVarient(Sym)
   THEN
      RETURN( IsVarientDependantsWritten(Sym) )
   ELSIF IsArray(Sym)
   THEN
      RETURN( IsArrayDependantsWritten(Sym) )
   ELSIF IsProcType(Sym)
   THEN
      RETURN( IsProcTypeDependantsWritten(Sym) )
   ELSIF IsUnbounded(Sym)
   THEN
      RETURN( IsUnboundedDependantsWritten(Sym) )
   ELSIF IsSet(Sym)
   THEN
      RETURN( IsSetDependantsWritten(Sym) )
   ELSIF IsType(Sym)
   THEN
      RETURN( IsTypeDependantsWritten(GetType(Sym)) )
   ELSE
      RETURN( TRUE )
   END
END AllDependantsWritten ;


(*
   DeclareTypeInfo - generates type information for a type symbol, Sym.
                     A type symbol, Sym, will be transformed into its GCC equivalent.
*)

PROCEDURE DeclareTypeInfo (Sym: WORD) ;
VAR
   gcc: Tree ;
BEGIN
   IF Debugging
   THEN
      printf2('// declaring %d %a\n', Sym, GetSymName(Sym))
   END ;

   IF Sym=99
   THEN
      mystop
   END ;

   IF IsVarient(Sym)
   THEN
      InternalError('why have we reached here?', __FILE__, __LINE__)
   ELSIF IsVar(Sym)
   THEN
      DeclareTypeInfo(GetType(Sym))
   ELSIF (NOT GccKnowsAbout(Sym)) AND IsAModula2Type(Sym)
   THEN
      IncludeItemIntoList(ToDoList, Sym) ;
      IF AllDependantsWritten(Sym)
      THEN
         (* add relationship between gccSym and Sym *)
         gcc := DeclareOrFindKindOfType(Sym) ;
         IF gcc=Tree(NIL)
         THEN
            mystop ;
            gcc := DeclareOrFindKindOfType(Sym)
         END ;
         RemoveItemFromList(ToDoList, Sym) ;
         AddModGcc(Sym, gcc)
      END
   END
END DeclareTypeInfo ;


(*
   DeclareUnboundedProcedureParameters - 
*)

PROCEDURE DeclareUnboundedProcedureParameters (Sym: WORD) ;
VAR
   son,
   type,
   p, i: CARDINAL ;
BEGIN
   IF IsProcedure(Sym)
   THEN
      p := NoOfParam(Sym) ;
      i := p ;
      WHILE i>0 DO
         IF IsUnboundedParam(Sym, i)
         THEN
            son := GetNth(Sym, i) ;
            type := GetType(son) ;
            DeclareTypeInfo(type) ;
            IF (NOT GccKnowsAbout(type)) AND AllDependantsWritten(type)
      	    THEN
               AddModGcc(type, DeclareOrFindKindOfType(type)) ;
               RemoveItemFromList(ToFinishList, type) ;
               IncludeItemIntoList(DefinedList, type)
            END
         ELSE
            son := GetNth(Sym, i) ;
            type := GetType(son) ;
            DeclareTypeInfo(type)
         END ;
         DEC(i)
      END
   END
END DeclareUnboundedProcedureParameters ;


(*
   DeclareTypesInProcedure - declare all types in procedure, Sym, to GCC.
*)

PROCEDURE DeclareTypesInProcedure (Sym: WORD) ;
BEGIN
   ForeachLocalSymDo(Sym, DeclareTypeInfo) ;
   ForeachLocalSymDo(Sym, DeclareUnboundedProcedureParameters)
END DeclareTypesInProcedure ;


(*
   DeclareTypesInModule - declare all types in module, Sym, to GCC.
*)

PROCEDURE DeclareTypesInModule (Sym: WORD) ;
BEGIN
   ForeachLocalSymDo(Sym, DeclareTypeInfo) ;
   ForeachLocalSymDo(Sym, DeclareUnboundedProcedureParameters) ;
   ForeachInnerModuleDo(Sym, DeclareTypesInModule)
END DeclareTypesInModule ;


(*
   GetModuleWhereDeclared - returns the module where, Sym, was created.
*)

PROCEDURE GetModuleWhereDeclared (Sym: CARDINAL) : CARDINAL ;
VAR
   s: CARDINAL ;
BEGIN
   s := GetScope(Sym) ;
   IF (s=NulSym) OR IsDefImp(s) OR IsModule(s)
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
   DeclareProcedureToGcc - traverses all parameters and interfaces to gm2gcc.
*)

PROCEDURE DeclareProcedureToGcc (Sym: CARDINAL) ;
VAR
   GccParam: Tree ;
   Son,
   p, i    : CARDINAL ;
BEGIN
   IF (NOT GccKnowsAbout(Sym)) AND (NOT IsPseudoProcFunc(Sym)) AND
      (IsImported(GetMainModule(), Sym) OR (GetModuleWhereDeclared(Sym)=GetMainModule()) OR
       IsImported(GetBaseModule(), Sym) OR IsImported(GetModuleWhereDeclared(Sym), Sym))
   THEN
      Assert(PushParametersLeftToRight) ;
      BuildStartFunctionDeclaration ;
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
                                                  Mod2Gcc(GetType(Son)), FALSE)
         ELSE
            GccParam := BuildParameterDeclaration(KeyToCharStar(GetSymName(Son)),
                                                  Mod2Gcc(GetType(Son)), IsVarParam(Sym, i))
         END ;
         AddModGcc(Son, GccParam) ;
         DEC(i)
      END ;
      IF GetType(Sym)=NulSym
      THEN
         AddModGcc(Sym, BuildEndFunctionDeclaration(KeyToCharStar(GetFullSymName(Sym)),
                                                    NIL, IsImported(GetMainModule(), Sym),
                                                    IsProcedureNested(Sym)))
      ELSE
         AddModGcc(Sym, BuildEndFunctionDeclaration(KeyToCharStar(GetFullSymName(Sym)),
                                                    Mod2Gcc(GetType(Sym)), IsImported(GetMainModule(), Sym),
                                                    IsProcedureNested(Sym)))
      END
   END
END DeclareProcedureToGcc ;


(*
   DeclareProcedure - declares procedure, sym, or all procedures inside
                      module sym.
*)

PROCEDURE DeclareProcedure (Sym: WORD) ;
BEGIN
   IF IsProcedure(Sym)
   THEN
      DeclareProcedureToGcc(Sym)
   ELSIF IsModule(Sym) OR IsDefImp(Sym)
   THEN
      ForeachProcedureDo(Sym, DeclareProcedure)
   ELSE
      InternalError('expecting procedure, module or defimp symbol', __FILE__, __LINE__)
   END
END DeclareProcedure ;


(*
   FoldConstants - a wrapper for ResolveConstantExpressions.
*)

PROCEDURE FoldConstants (start, end: CARDINAL) ;
BEGIN
   IF ResolveConstantExpressions(ToDoConstants, start, end)
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
   REPEAT
      n := NoOfItemsInList(ToDoConstants) ;
      WHILE ResolveConstantExpressions(ToDoConstants, start, end) DO
      END ;
      m := NoOfItemsInList(ToDoConstants) ;
      (* we need to evaluate some constant expressions to resolve these types *)
      IF DeclaredOutstandingTypes(FALSE, start, end)
      THEN
      END ;
   UNTIL (NOT ResolveConstantExpressions(ToDoConstants, start, end)) AND (n=NoOfItemsInList(ToDoConstants)) AND
         (m=NoOfItemsInList(ToDoConstants));
   IF DeclaredOutstandingTypes(TRUE, start, end)
   THEN
   END
END DeclareTypesAndConstantsInRange ;


(*
   DeclareTypesAndConstants - 
*)

PROCEDURE DeclareTypesAndConstants (scope: CARDINAL) ;
VAR
   sb: ScopeBlock ;
BEGIN
   sb := InitScopeBlock(scope) ;
   ForeachScopeBlockDo(sb, DeclareTypesAndConstantsInRange) ;
   sb := KillScopeBlock(sb)
END DeclareTypesAndConstants ;


(*
   StartDeclareScope - declares types, variables associated with this scope.
*)

PROCEDURE StartDeclareScope (scope: CARDINAL) ;
BEGIN
   IF IsProcedure(scope)
   THEN
      DeclareTypesInProcedure(scope) ;
      DeclareTypesAndConstants(scope) ;
      DeclareLocalVariables(scope) ;
      ForeachProcedureDo(scope, DeclareProcedure)
   ELSE
      ForeachModuleDo(DeclareTypesInModule) ;
      DeclareTypesAndConstants(scope) ;
      ForeachModuleDo(DeclareProcedure) ;
      (* now that all types have been resolved it is safe to declare variables *)
      DeclareGlobalVariables(scope) ;
      ForeachImportedDo(scope, DeclareImportedVariables) ;
      (* now it is safe to declare all procedures *)
      ForeachProcedureDo(scope, DeclareProcedure) ;
      ForeachInnerModuleDo(scope, DeclareProcedure)
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
   DeclareDefaultType - declares a default type, sym, with, name.
*)

PROCEDURE DeclareDefaultType (sym: CARDINAL; name: ARRAY OF CHAR; gcctype: Tree) ;
VAR
   t        : Tree ;
   high, low: CARDINAL ;
BEGIN
   (* DeclareDefaultType will declare a new identifier as a type of, gcctype, if it has not already been
      declared by gccgm2.c *)
   t := GetDefaultType(KeyToCharStar(MakeKey(name)), gcctype) ;
   AddModGcc(sym, t) ;
   (*
      this is very simplistic and assumes that the caller only uses Subranges, Sets and GCC types.
      We need to declare any constants with the types so that AllDependantsWritten works.
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
   END ;
   IF NOT AllDependantsWritten(sym)
   THEN
      WriteFormat1('defining a default type (%a) before its dependants are known', MakeKey(name))
   END
END DeclareDefaultType ;


(*
   DeclareBoolean - declares the Boolean type together with true and false.
*)

PROCEDURE DeclareBoolean ;
BEGIN
   AddModGcc(Boolean, DeclareEnumeration(Boolean)) ;
   AssignBooleanTrueFalse(Mod2Gcc(Boolean), Mod2Gcc(True), Mod2Gcc(False))
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
   DeclareDefaultTypes - makes default types known to GCC
*)

PROCEDURE DeclareDefaultTypes ;
BEGIN
   IF NOT HaveInitDefaultTypes
   THEN
      HaveInitDefaultTypes := TRUE ;

      DeclareDefaultType(Integer  , "INTEGER"  , GetM2IntegerType()) ;
      DeclareDefaultType(Char     , "CHAR"     , GetM2CharType()) ;
      DeclareDefaultType(Cardinal , "CARDINAL" , GetM2CardinalType()) ;

      IF Iso
      THEN
         DeclareDefaultType(Loc   , "LOC"      , GetISOLocType()) ;
         DeclareDefaultType(Byte  , "BYTE"     , GetISOByteType()) ;
         DeclareDefaultType(Word  , "WORD"     , GetISOWordType())
      ELSE
         DeclareDefaultType(Byte  , "BYTE"     , GetByteType()) ;
         DeclareDefaultType(Word  , "WORD"     , GetWordType())
      END ;

      DeclareDefaultType(Proc     , "PROC"     , GetProcType()) ;
      DeclareDefaultType(Address  , "ADDRESS"  , GetPointerType()) ;
      DeclareDefaultType(LongInt  , "LONGINT"  , GetM2LongIntType()) ;
      DeclareDefaultType(LongCard , "LONGCARD" , GetM2LongCardType()) ;
      DeclareDefaultType(ShortReal, "SHORTREAL", GetM2ShortRealType()) ;
      DeclareDefaultType(Real     , "REAL"     , GetM2RealType()) ;
      DeclareDefaultType(LongReal , "LONGREAL" , GetM2LongRealType()) ;
      DeclareDefaultType(Bitnum   , "BITNUM"   , GetBitnumType()) ;
      DeclareDefaultType(Bitset   , "BITSET"   , GetBitsetType()) ;
      DeclareBoolean ;
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
   DeclareVariable - declares a global variable to GCC.
*)

PROCEDURE DeclareVariable (ModSym, Son: CARDINAL) ;
BEGIN
   IF NOT GccKnowsAbout(Son)
   THEN
      AlignDeclarationWithSource(Son) ;
      IF GetMode(Son)=LeftValue
      THEN
         (* really a pointer to GetType(Son) - we will tell gcc exactly this *)
         AddModGcc(Son, DeclareKnownVariable(KeyToCharStar(GetFullSymName(Son)),
                                             BuildPointerType(Mod2Gcc(GetType(Son))),
                                             IsExported(ModSym, Son),
                                             IsImported(ModSym, Son),
                                             IsTemporary(Son),
                                             TRUE,
                                             NIL))
      ELSE
         AddModGcc(Son, DeclareKnownVariable(KeyToCharStar(GetFullSymName(Son)),
                                             Mod2Gcc(GetType(Son)),
                                             IsExported(ModSym, Son),
                                             IsImported(ModSym, Son),
                                             IsTemporary(Son),
                                             TRUE,
                                             NIL))
      END
   END
END DeclareVariable ;


(*
   DeclareGlobalVariables - lists the Global variables for Module ModSym
                            together with their offset.
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

PROCEDURE DeclareImportedVariables (Sym: WORD) ;
BEGIN
   IF IsVar(Sym)
   THEN
      DeclareVariable(GetMainModule(), Sym)
   ELSIF IsDefImp(Sym)
   THEN
      ForeachExportedDo(Sym, DeclareImportedVariables)
   END
END DeclareImportedVariables ;


(*
   DeclareLocalVariables - declares Local variables for procedure Sym.
*)

PROCEDURE DeclareLocalVariables (Sym: CARDINAL) ;
VAR
   i, Var: CARDINAL ;
BEGIN
   CheckToFinishList(TRUE) ;
   i := NoOfParam(Sym)+1 ;
   Var := GetNth(Sym, i) ;
   WHILE Var#NulSym DO
      AlignDeclarationWithSource(Var) ;
      IF GetMode(Var)=LeftValue
      THEN
         (* really a pointer to GetType(Var) - we will tell gcc exactly this *)
         AddModGcc(Var, DeclareKnownVariable(KeyToCharStar(GetFullSymName(Var)),
                                             BuildPointerType(Mod2Gcc(GetType(Var))),
                                             FALSE,  (* local variables cannot be imported *)
                                             FALSE,  (* local variables cannot be exported *)
                                             IsTemporary(Var),
                                             FALSE,
                                             Mod2Gcc(Sym)))
      ELSE
         AddModGcc(Var, DeclareKnownVariable(KeyToCharStar(GetFullSymName(Var)),
                                             Mod2Gcc(GetType(Var)),
                                             FALSE,  (* local variables cannot be imported *)
                                             FALSE,  (* local variables cannot be exported *)
                                             IsTemporary(Var),
                                             FALSE,
                                             Mod2Gcc(Sym)))
      END ;
      INC(i) ;
      Var := GetNth(Sym, i)
   END
END DeclareLocalVariables ;


(*
   DeclareFieldEnumeration - declares an enumerator within the current enumeration type.
*)

PROCEDURE DeclareFieldEnumeration (Sym: WORD) ;
BEGIN
   (* add relationship between gccSym and Sym *)
   PushValue(Sym) ;
   IF (GetModuleWhereDeclared(Sym)=NulSym) OR
      (GetModuleWhereDeclared(Sym)=GetMainModule())
   THEN
      AddModGcc(Sym, BuildEnumerator(KeyToCharStar(GetSymName(Sym)), PopIntegerTree()))
   ELSE
      AddModGcc(Sym, BuildEnumerator(KeyToCharStar(GetFullScopeAsmName(Sym)), PopIntegerTree()))
   END
END DeclareFieldEnumeration ;


(*
   DeclareEnumeration - declare an enumerated type.
*)

PROCEDURE DeclareEnumeration (Sym: WORD) : Tree ;
VAR
   gccenum: Tree ;
BEGIN
   gccenum := BuildStartEnumeration(KeyToCharStar(GetSymName(Sym))) ;
   ForeachFieldEnumerationDo(Sym, DeclareFieldEnumeration) ;
   RETURN( BuildEndEnumeration(gccenum) )
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
   gccsym := BuildSubrangeType(KeyToCharStar(GetSymName(sym)),
                               Mod2Gcc(GetType(sym)), Mod2Gcc(low), Mod2Gcc(high)) ;
   RETURN( gccsym )
END DeclareSubrange ;


(*
   DeclareVarient - declares a varient record to gcc and returns the gcc representation.
*)

PROCEDURE DeclareVarient (Sym: CARDINAL) : Tree ;
VAR
   i, j        : CARDINAL ;
   Field1,
   Field2      : CARDINAL ;
   GccFieldType,
   GccField,
   FieldList,
   RecordType  : Tree ;
BEGIN
   i := 1 ;
   FieldList := Tree(NIL) ;
   RecordType := BuildStartVarientRecord(KeyToCharStar(GetSymName(Sym))) ;
   (* no need to store the [Sym, RecordType] tuple as it is stored by DeclareRecord which calls us *)
   REPEAT
      Field1 := GetNth(Sym, i) ;
      IF Field1#NulSym
      THEN
      	 Assert(IsFieldVarient(Field1)) ;
      	 j := 1 ;
      	 REPEAT
      	    Field2 := GetNth(Field1, j) ;
      	    IF Field2#NulSym
      	    THEN
               GccFieldType := ForceDeclareType(GetType(Field2)) ;
               GccField     := BuildFieldRecord(KeyToCharStar(GetSymName(Field2)), GccFieldType) ;
               FieldList    := ChainOn(FieldList, GccField) ;
               AddModGcc(Field2, GccField) ;
      	       INC(j)
      	    END
      	 UNTIL Field2=NulSym
      END ;
      INC(i)
   UNTIL Field1=NulSym ;
   RemoveItemFromList(ToFinishList, Sym) ;
   RETURN( BuildEndRecord(RecordType, FieldList) )
END DeclareVarient ;


(*
   DeclareRecord - declares a record and its fields to gcc.
                   The final gcc record type is returned.
*)

PROCEDURE DeclareRecord (Sym: CARDINAL) : Tree ;
VAR
   Field       : CARDINAL ;
   i           : CARDINAL ;
   GccField,
   GccFieldType,
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
         IF GccKnowsAbout(Field)
         THEN
            GccFieldType := Mod2Gcc(Field) ;
            GccField     := BuildFieldRecord(KeyToCharStar(GetSymName(Field)), GccFieldType) ;
            AddModGcc(Field, GccField)
         ELSE
            IF IsVarient(Field)
            THEN
               Assert(AllDependantsWritten(Field)) ;
               GccFieldType := DeclareVarient(Field)
            ELSIF IsFieldVarient(Field)
            THEN
               WriteFormat1('found unexpected field varient name %a\n', GetSymName(Field)) ;
               InternalError('should not get here', __FILE__, __LINE__)
            ELSE
               IF NOT AllDependantsWritten(GetType(Field))
               THEN
                  mystop ;
                  Assert(AllDependantsWritten(GetType(Field))) ;
               END ;
               GccFieldType := ForceDeclareType(GetType(Field))
            END ;
            GccField := BuildFieldRecord(KeyToCharStar(GetSymName(Field)), GccFieldType) ;
            AddModGcc(Field, GccField)
         END ;
         FieldList := ChainOn(FieldList, GccField)
      END ;
      INC(i)
   UNTIL Field=NulSym ;
   RemoveItemFromList(ToFinishList, Sym) ;
   RETURN( BuildEndRecord(RecordType, FieldList) )
END DeclareRecord ;


(*
   DeclarePointer - declares a pointer type to gcc and returns the Tree.
*)

PROCEDURE DeclarePointer (Sym: CARDINAL) : Tree ;
BEGIN
   IF GetSymName(Sym)=NulName
   THEN
      RETURN( BuildPointerType(DeclareOrFindKindOfType(GetType(Sym))) )
   ELSE
      RETURN( DeclareKnownType(KeyToCharStar(GetSymName(Sym)),
                               BuildPointerType(DeclareOrFindKindOfType(GetType(Sym)))) )
   END
END DeclarePointer ;


(*
   DeclareUnbounded - builds an unbounded type and returns the gcc tree.
*)

PROCEDURE DeclareUnbounded (Sym: CARDINAL) : Tree ;
VAR
   FieldList,
   GccFieldType,
   RecordType  : Tree ;
BEGIN
   IF GetType(Sym)=Char
   THEN
      RecordType := BuildStartRecord(KeyToCharStar(GetSymName(Sym))) ;
      FieldList  := ChainOn(BuildFieldRecord(KeyToCharStar(ArrayAddress),
                                             BuildPointerType(GetCharType())),
                            BuildFieldRecord(KeyToCharStar(ArrayHigh),
                                             Mod2Gcc(Cardinal))) ;
      RETURN( BuildEndRecord(RecordType, FieldList) )
   ELSE
      (* was RETURN( Mod2Gcc(Unbounded) ) *)
      RecordType := BuildStartRecord(KeyToCharStar(GetSymName(Sym))) ;
      FieldList  := ChainOn(BuildFieldRecord(KeyToCharStar(ArrayAddress),
                                             BuildPointerType(Mod2Gcc(GetType(Sym)))),
                            BuildFieldRecord(KeyToCharStar(ArrayHigh),
                                             Mod2Gcc(Cardinal))) ;
      RETURN( BuildEndRecord(RecordType, FieldList) )
   END
END DeclareUnbounded ;


(*
   DeclareArray - declares an array to gcc and returns the gcc tree.
*)

PROCEDURE DeclareArray (Sym: CARDINAL) : Tree ;
VAR
   i        : CARDINAL ;
   Subscript,
   Subrange : CARDINAL ;
   High, Low: CARDINAL ;
   Type     : CARDINAL ;
   GccArray,
   GccIndex : Tree ;
BEGIN
   Assert(IsArray(Sym)) ;

   GccArray := ForceDeclareType(GetType(Sym)) ;
   i := 1 ;
   REPEAT
      Subscript := GetNth(Sym, i) ;
      IF Subscript#NulSym
      THEN
         Assert(IsSubscript(Subscript)) ;
         AddModGcc(Subscript, GccArray) ;       (* we save the type of this array as the subscript *)
         PushIntegerTree(BuildSize(GccArray, FALSE)) ;  (* and the size of this array so far *)
         PopSize(Subscript) ;
         Subrange := GetType(Subscript) ;
         IF NOT IsSubrange(Subrange)
         THEN
            WriteFormat3('error with array (%a) subscript (%d) no subrange for this subscript, instead the type given was %a', GetSymName(Sym), i, GetSymName(Subrange))
         END ;
         Assert(IsSubrange(Subrange)) ;
         GetSubrange(Subrange, High, Low) ;
         GccIndex := BuildArrayIndexType(Mod2Gcc(Low), Mod2Gcc(High)) ;
         GccArray := BuildArrayType(GccArray, GccIndex)
      END ;
      INC(i)
   UNTIL Subscript=NulSym ;
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
   InitFunctionTypeParameters ;
   p := NoOfParam(Sym) ;
   i := p ;
   Assert(PushParametersLeftToRight) ;
   WHILE i>0 DO
      Son := GetNthParam(Sym, i) ;
      GccParam := BuildParameterDeclaration(NIL, DeclareOrFindKindOfType(GetType(Son)), IsVarParam(Sym, i)) ;
      AddModGcc(Son, GccParam) ;
      DEC(i)
   END ;
   IF ReturnType=NulSym
   THEN
      RETURN( BuildEndFunctionType(func, NIL) )
   ELSE
      RETURN( BuildEndFunctionType(func, DeclareOrFindKindOfType(ReturnType)) )
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
      WriteFormat1('unable to obtain the MIN value for type %a', GetSymName(type))
   ELSE
      RETURN( GetTypeMin(GetType(type)) )
   END
END GetTypeMin ;


(*
   GetTypeMax - 
*)

PROCEDURE GetTypeMax (type: CARDINAL) : CARDINAL ;
VAR
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
      WriteFormat1('unable to obtain the MAX value for type %a', GetSymName(type))
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
   bpw        := GetBitsPerWord() ;
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
   PushCard(GetBitsPerWord()) ;
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
   type := GetType(sym) ;
   IF IsSubrange(type)
   THEN
      GetSubrange(type, high, low) ;
      gccsym := DeclareLargeOrSmallSet(sym, GetSymName(sym), GetType(type), low, high)
   ELSE
      gccsym := DeclareLargeOrSmallSet(sym, GetSymName(sym), type, GetTypeMin(type), GetTypeMax(type))
   END ;
   RETURN( gccsym )
END DeclareSet ;


(*
   ForceDeclareType - forces the declaration of a type and records the sym and gcc
                      entities.
*)

PROCEDURE ForceDeclareType (sym: CARDINAL) : Tree ;
VAR
   t1, t2: Tree ;
BEGIN
   IF IsVarient(sym)
   THEN
      InternalError('caught a varient, it should never come through this procedure', __FILE__, __LINE__)
   END ;
   t1 := DeclareOrFindKindOfType(sym) ;
   IF IsItemInList(ToFinishList, sym) AND AllDependantsWritten(sym)
   THEN
      (* complete half built type (record, varient record) *)
      t2 := DeclareKindOfType(sym) ;
      IF t1#t2
      THEN
         printf0('problems after completing the type definition (before)\n') ;
         DebugTree(t1) ;
         printf0('problems after completing the type definition (after)\n') ;
         DebugTree(t2)
      END
   END ;
   AddModGcc(sym, t1) ;
   RETURN( t1 )
END ForceDeclareType ;


(*
   DeclareOrFindKindOfType - firstly lookup the symbol, if it is known return this symbol
                             otherwise declare the symbol. Remember that we have have
                             partially declared symbols in the Mod2Gcc lookup facility.
*)

PROCEDURE DeclareOrFindKindOfType (Sym: CARDINAL) : Tree ;
BEGIN
   IF GccKnowsAbout(Sym)
   THEN
      RETURN( Mod2Gcc(Sym) )
   ELSE
      RETURN( DeclareKindOfType(Sym) )
   END
END DeclareOrFindKindOfType ;


(*
   DeclareKindOfType - passes a symbol, sym, to GCC and returns the GCC equivelent symbol.
*)

PROCEDURE DeclareKindOfType (Sym: CARDINAL) : Tree ;
BEGIN
   IF IsEnumeration(Sym)
   THEN
      RETURN( DeclareEnumeration(Sym) )
   ELSIF IsSubrange(Sym)
   THEN
      RETURN( DeclareSubrange(Sym) )
   ELSIF IsRecord(Sym)
   THEN
      RETURN( DeclareRecord(Sym) )
   ELSIF IsFieldVarient(Sym)
   THEN
      RETURN( DeclareVarient(Sym) )
   ELSIF IsVarient(Sym)
   THEN
      InternalError('should not be solving varients here', __FILE__, __LINE__)
   ELSIF IsPointer(Sym)
   THEN
      RETURN( DeclarePointer(Sym) )
   ELSIF IsUnbounded(Sym)
   THEN
      RETURN( DeclareUnbounded(Sym) )
   ELSIF IsArray(Sym)
   THEN
      RETURN( DeclareArray(Sym) )
   ELSIF IsProcType(Sym)
   THEN
      RETURN( DeclareProcType(Sym) )
   ELSIF IsSet(Sym)
   THEN
      RETURN( DeclareSet(Sym) )
   ELSE
      RETURN( DeclareType(Sym) )
   END
END DeclareKindOfType ;


(*
   IsBaseType - returns true if a type, Sym, is a base type and
                we use predefined GDB information to represent this
                type.
*)

PROCEDURE IsBaseType (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
           (Sym=Cardinal) OR (Sym=Integer) OR
           (Sym=Char)     OR (Sym=Proc)
         )
END IsBaseType ;


(*
   IsEnumerationDependantsWritten - returns true if the enumeration
                                    dependants have been written to
                                    the assembly file.
*)

PROCEDURE IsEnumerationDependantsWritten (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   PrintType('Enumeration', Sym, TRUE) ;
   RETURN( TRUE )
END IsEnumerationDependantsWritten ;


(*
   CheckResolveSubrange - checks to see whether we can resolve the subrange type.
                          We are able to do this once low, and high are known.
*)

PROCEDURE CheckResolveSubrange (Sym: CARDINAL) ;
VAR
   size, high, low, type: CARDINAL ;
BEGIN
   GetSubrange(Sym, high, low) ;
   type := GetType(Sym) ;
   IF type#NulSym
   THEN
      IF NOT GccKnowsAbout(type)
      THEN
         IncludeItemIntoList(ToDoList, type)
      END
   ELSIF GccKnowsAbout(low) AND GccKnowsAbout(high)
   THEN
      IF IsConstString(low)
      THEN
         size := GetStringLength(low) ;
         IF size=1
         THEN
            PutSubrange(Sym, low, high, Char)
         ELSE
            WriteFormat1('cannot have a subrange of a string type %a', GetSymName(Sym)) ;
         END
      ELSIF IsFieldEnumeration(low)
      THEN
         IF GetType(low)=GetType(high)
         THEN
            PutSubrange(Sym, low, high, GetType(low))
         ELSE
            WriteFormat1('subrange limits must be of the same type %a', GetSymName(Sym)) ;
         END
      ELSIF IsValueSolved(low)
      THEN
         IF GetType(low)=LongReal
         THEN
            WriteFormat1('cannot have a subrange of a REAL or LONGREAL type %a', GetSymName(Sym)) ;
         ELSE
            PutSubrange(Sym, low, high, Integer)
         END
      END ;
      type := GetType(Sym) ;
      IF (type#NulSym) AND (NOT GccKnowsAbout(type))
      THEN
         IncludeItemIntoList(ToDoList, type)
      END
   END
END CheckResolveSubrange ;


(*
   IsSubrangeDependantsWritten - returns true if the subrange
                                 dependants have been written to the
                                 assembly file.
*)

PROCEDURE IsSubrangeDependantsWritten (Sym: CARDINAL) : BOOLEAN ;
VAR
   high, low: CARDINAL ;
BEGIN
   GetSubrange(Sym, high, low) ;
   (* low and high are not types but constants and they are resolved by M2GenGCC *)
   IF NOT GccKnowsAbout(low)
   THEN
      IncludeItemIntoList(ToDoConstants, low) ;
      IncludeItemIntoList(ToDoList, Sym)
   END ;
   IF NOT GccKnowsAbout(high)
   THEN
      IncludeItemIntoList(ToDoConstants, high) ;
      IncludeItemIntoList(ToDoList, Sym)
   END ;
   CheckResolveSubrange(Sym) ;
   RETURN(
          GccKnowsAbout(GetType(Sym)) AND
          GccKnowsAbout(low) AND GccKnowsAbout(high)
         )
END IsSubrangeDependantsWritten ;


(*
   IsPointerDependantsWritten - returns TRUE if the pointer symbol, Sym,
      	       	                dependants have been solved.
*)

PROCEDURE IsPointerDependantsWritten (Sym: CARDINAL) : BOOLEAN ;
VAR
   type: CARDINAL ;
BEGIN
   IF NOT GccKnowsAbout(Sym)
   THEN
      IncludeItemIntoList(ToDoList, Sym)
   END ;
   type := GetType(Sym) ;
   IF Debugging
   THEN
      printf2('// lets see about %d %a ', Sym, GetSymName(Sym))
   END ;
   (* is it partially known but has no name required for forward references *)
   IF IsItemInList(ToFinishList, type) AND (GetSymName(type)=NulName)
   THEN
      IF Debugging
      THEN
         printf0('no partially declared and nulname\n')
      END ;
      IncludeItemIntoList(ToDoList, type) ;
      IncludeItemIntoList(ToDoList, Sym) ;
      RETURN( FALSE )
   END ;
   IF IsSymTypeKnown(Sym, type)
   THEN
      IF Debugging
      THEN
         printf0('yes...\n')
      END ;
      RETURN( TRUE )
   ELSE
      IF Debugging
      THEN
         printf0('no its type is unknown...\n')
      END ;
      IncludeItemIntoList(ToDoList, type) ;
      RETURN( FALSE )
   END
END IsPointerDependantsWritten ;


(*
   IsRecordDependantsWritten - returns TRUE if the symbol, Sym,
      	       	               dependants have been written
      	       	               to the assembly file.
*)

PROCEDURE IsRecordDependantsWritten (Sym: CARDINAL) : BOOLEAN ;
VAR
   i     : CARDINAL ;
   type,
   Field : CARDINAL ;
   solved: BOOLEAN ;
   tree  : Tree ;
BEGIN
(*
   WriteString('// IsRecordDependantsWritten Sym=') ; WriteCard(Sym, 0) ;
   WriteString(' name ') ; WriteKey(GetSymName(Sym)) ; WriteLn ;
*)
   solved := TRUE ;
   tree := DoStartDeclaration(Sym, BuildStartRecord) ;
   i := 1 ;
   REPEAT
      Field := GetNth(Sym, i) ;
      IF Field#NulSym
      THEN
         type := GetType(Field) ;
         IF IsRecordField(Field)
         THEN
      	    IF (NOT IsSymTypeKnown(Sym, type)) AND (NOT AllDependantsWritten(type))
      	    THEN
               solved := FALSE
      	    END
         ELSIF IsVarient(Field)
         THEN
      	    IF (NOT IsSymTypeKnown(Sym, Field)) AND (NOT AllDependantsWritten(Field))
      	    THEN
               solved := FALSE
      	    END
      	 ELSIF IsFieldVarient(Field)
      	 THEN
            InternalError('should not see a field varient', __FILE__, __LINE__)
         ELSE
            InternalError('unknown symbol in record', __FILE__, __LINE__)
      	 END
      END ;
      INC(i)
   UNTIL Field=NulSym ;
   RETURN( solved )
END IsRecordDependantsWritten ;


(*
   IsVarientDependantsWritten - returns TRUE if all symbol, Sym, dependants
      	       	                have been written to the assembly file.
*)

PROCEDURE IsVarientDependantsWritten (Sym: CARDINAL) : BOOLEAN ;
VAR
   i, j  : CARDINAL ;
   type,
   Field1,
   Field2: CARDINAL ;
   solved: BOOLEAN ;
BEGIN
   solved := TRUE ;
   i := 1 ;
   REPEAT
      Field1 := GetNth(Sym, i) ;
      IF Field1#NulSym
      THEN
      	 Assert(IsFieldVarient(Field1)) ;
      	 j := 1 ;
      	 REPEAT
      	    Field2 := GetNth(Field1, j) ;
      	    IF Field2#NulSym
      	    THEN
               type := GetType(Field2) ;
       	       IF (NOT IsSymTypeKnown(Sym, type)) AND (NOT AllDependantsWritten(type))
      	       THEN
      	       	  solved := FALSE
      	       END ;
      	       INC(j)
      	    END
      	 UNTIL Field2=NulSym
      END ;
      INC(i)
   UNTIL Field1=NulSym ;
   RETURN( solved )
END IsVarientDependantsWritten ;


(*
   IsArrayDependantsWritten - returns TRUE if the symbol, Sym,
      	       	              dependants have
      	       	              been written into the assembly file.
*)

PROCEDURE IsArrayDependantsWritten (Sym: CARDINAL) : BOOLEAN ;
VAR
   solved   : BOOLEAN ;
   i        : CARDINAL ;
   Subscript,
   Subrange : CARDINAL ;
   High, Low: CARDINAL ;
   Type     : CARDINAL ;
BEGIN
   Assert(IsArray(Sym)) ;

   solved := TRUE ;
   Type := GetType(Sym) ;
 
   IF (NOT IsSymTypeKnown(Sym, Type)) OR (NOT AllDependantsWritten(Type))
   THEN
      solved := FALSE
   END ;
   i := 1 ;
   REPEAT
      Subscript := GetNth(Sym, i) ;
      IF Subscript#NulSym
      THEN
         Assert(IsSubscript(Subscript)) ;
         Subrange := GetType(Subscript) ;
         IF NOT IsSubrange(Subrange)
         THEN
            WriteFormat3('error with array (%a) subscript (%d) no subrange for this subscript, instead the type given was %a', GetSymName(Sym), i, GetSymName(Subrange))
         END ;
         Assert(IsSubrange(Subrange)) ;
         GetSubrange(Subrange, High, Low) ;

         IF NOT IsSubrangeDependantsWritten(Subrange)
         THEN
            RETURN( FALSE )
         END
      END ;
      INC(i)
   UNTIL Subscript=NulSym ;
   RETURN( solved )
END IsArrayDependantsWritten ;


(*
   IsSetDependantsWritten - returns TRUE if the symbol, Sym, dependants have
                            been written into the assembly file.
*)

PROCEDURE IsSetDependantsWritten (sym: CARDINAL) : BOOLEAN ;
VAR
   type, low, high: CARDINAL ;
BEGIN
   Assert(IsSet(sym)) ;

   type := GetType(sym) ;
   IF IsSubrange(type)
   THEN
      IF IsSymTypeKnown(sym, type)
      THEN
         RETURN( TRUE )
      ELSE
         RETURN( IsSubrangeDependantsWritten(type) )
      END
   ELSE
      IF IsSymTypeKnown(sym, type)
      THEN
         low  := GetTypeMin(type) ;
         high := GetTypeMax(type) ;
         IF NOT GccKnowsAbout(low)
         THEN
            IncludeItemIntoList(ToDoConstants, low)
         END ;
         IF NOT GccKnowsAbout(high)
         THEN
            IncludeItemIntoList(ToDoConstants, high)
         END ;
         RETURN( GccKnowsAbout(low) AND GccKnowsAbout(high) )
      END ;
      RETURN( FALSE )
   END
END IsSetDependantsWritten ;


(*
   IsProcTypeDependantsWritten - 
*)

PROCEDURE IsProcTypeDependantsWritten (Sym: CARDINAL) : BOOLEAN ;
VAR
   i, p, Son : CARDINAL ;
   ParamType,
   ReturnType: CARDINAL ;
   solved    : BOOLEAN ;
   tree      : Tree ;
BEGIN
   solved := TRUE ;
   Assert(IsProcType(Sym)) ;
   tree := DoStartDeclaration(Sym, BuildStartFunctionType) ;
   i := 1 ;
   ReturnType := GetType(Sym) ;
   p := NoOfParam(Sym) ;
   WHILE i<=p DO
      Son := GetNthParam(Sym, i) ;
      ParamType := GetType(Son) ;
      IF (NOT IsSymTypeKnown(Sym, ParamType)) AND (NOT AllDependantsWritten(ParamType))
      THEN
         solved := FALSE
      END ;
      INC(i)
   END ;
   IF (NOT IsSymTypeKnown(Sym, ReturnType)) AND (NOT AllDependantsWritten(ReturnType))
   THEN
      RETURN( FALSE )
   ELSE
      RETURN( solved )
   END
END IsProcTypeDependantsWritten ;


(*
   IsUnboundedDependantsWritten - returns TRUE if all dependants have been declared to GCC.
*)

PROCEDURE IsUnboundedDependantsWritten (Sym: CARDINAL) : BOOLEAN ;
VAR
   solved: BOOLEAN ;
BEGIN
   solved := TRUE ;
   IF GetType(Sym)=Char
   THEN
      IF NOT IsSymTypeKnown(Sym, Cardinal)
      THEN
         solved := FALSE
      END ;
      IF NOT IsSymTypeKnown(Sym, Char)
      THEN
         solved := FALSE
      END
   ELSE
      IF NOT IsSymTypeKnown(Sym, Unbounded)
      THEN
         solved := FALSE
      END ;
      IF NOT IsSymTypeKnown(Sym, GetType(Sym))
      THEN
         solved := FALSE
      END
   END ;
   RETURN( solved )
END IsUnboundedDependantsWritten ;


(*
   IsTypeDependantsWritten - returns TRUE if a type symbol, Sym, dependants
      	       	             have been written to the assembly file.
      	       	     	     If a symbol has no name then it will NOT
                             be written out by itself but as part
                             of another symbol who does have a name.
                             Hence we can return TRUE for such symbols.
*)

PROCEDURE IsTypeDependantsWritten (Sym: CARDINAL) : BOOLEAN ;
VAR
   type  : CARDINAL ;
   solved: BOOLEAN ;
BEGIN
   solved := TRUE ;
   IF Sym#NulSym
   THEN
      (*
      IF GetSymName(Sym)#NulName
      THEN
      *)
         IF NOT GccKnowsAbout(Sym)
         THEN
            IncludeItemIntoList(ToDoList, Sym) ;
            solved := FALSE ;
(*
         END
*)
      END ;
      type := GetType(Sym) ;
      IF NOT IsSymTypeKnown(Sym, type)
      THEN
         solved := FALSE
      END
   END ;
   RETURN( solved )
END IsTypeDependantsWritten ;


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
   InitDeclarations - initializes default types and the source filename.
*)

PROCEDURE InitDeclarations ;
BEGIN
   DeclareFileName ;
   DeclareDefaultTypes
END InitDeclarations ;


BEGIN
   InitList(ToDoList) ;
   InitList(ToDoConstants) ;
   InitList(ToFinishList) ;
   InitList(DefinedList) ;
   HaveInitDefaultTypes := FALSE
END M2GCCDeclare.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I.:../gm2-libs:../gm2-libs-ch:../gm2-libiberty/ M2GCCDeclare.mod"
 * End:
 *)
