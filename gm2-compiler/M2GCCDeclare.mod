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
    Last edit  : Fri Jul 16 20:10:55 1999
    Description: declares Modula-2 types to GCC, it attempts
                 to only declare a type once all subcomponents are known.
*)

FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM ASCII IMPORT nul ;
FROM M2Debug IMPORT Assert ;
FROM StdIO IMPORT Write ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StrLib IMPORT StrConCat ;
FROM NumberIO IMPORT WriteCard, WriteInt, CardToStr ;
FROM M2Options IMPORT GenerateDebugging, GenerateLineDebug ;
FROM NameKey IMPORT WriteKey, MakeKey, GetKey, NulName, KeyToCharStar ;
FROM M2AsmUtil IMPORT WriteAsmName, WriteName, GetAsmName, GetFullSymName, UnderScoreString, GetModuleInitName ;
FROM M2FileName IMPORT CalculateFileName ;
FROM M2Configure IMPORT PushParametersLeftToRight ;

FROM Lists IMPORT List, InitList, IncludeItemIntoList,
                  PutItemIntoList, GetItemFromList,
                  RemoveItemFromList,
      	       	  IsItemInList, NoOfItemsInList ;

FROM SymbolTable IMPORT NulSym,
                        ModeOfAddr,
                        GetMode,
                        GetScopeAuthor,
                        GetNth, GetType,
                        MakeType, PutType,
      	       	     	GetSubrange, PutSubrange,
      	       	     	NoOfParam, GetNthParam,
                        PushValue, PopSize,
                        IsTemporary, IsUnbounded, IsEnumeration, IsVar,
      	       	     	IsSubrange, IsPointer, IsRecord, IsArray,
                        IsProcedure, IsProcedureNested, IsModule, IsDefImp,
      	       	     	IsSubscript, IsVarient, IsFieldVarient,
      	       	     	IsType, IsProcType, IsSet, IsConst,
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
                        ForeachInnerModuleDo, ForeachImportedDo ;

FROM M2Base IMPORT IsPseudoBaseProcedure, IsPseudoBaseFunction,
                   Cardinal, Char, Proc, Integer, Unbounded, LongInt, Real, LongReal, Boolean, True, False ;
FROM M2System IMPORT IsPseudoSystemFunction, Address, Word, Bitset, Byte ;
FROM M2Math IMPORT IsPseudoMathFunction ;
FROM M2ALU IMPORT PushCard, PushIntegerTree, PopIntegerTree, PopRealTree ;
FROM M2Lexical IMPORT InternalError, TokenToLineNo, FindFileNameFromToken, WriteErrorFormat1, FormatWarningMessage2 ;
FROM SymbolConversion IMPORT AddModGcc, Mod2Gcc, GccKnowsAbout ;
FROM M2GenGCC IMPORT ResolveConstantExpressions ;

FROM gccgm2 IMPORT Tree,
                   SetFileNameAndLineNo,
                   DeclareKnownType, DeclareKnownVariable,
                   GetIntegerType, GetCharType,
                   GetVoidType, GetIntegerZero, GetIntegerOne, GetCurrentFunction,
                   GetPointerType, GetLongRealType, GetLongIntType, GetRealType,
                   GetProcType, GetCardinalType, GetWordType, GetByteType,
                   GetBitsetType, GetMinFrom, GetMaxFrom,
                   BuildStartEnumeration, BuildEndEnumeration, BuildEnumerator,
                   BuildIntegerConstant, BuildStringConstant, BuildCharConstant,
                   BuildSubrangeType,
                   BuildStartRecord, BuildEndRecord, BuildStartVarientRecord, BuildFieldRecord,
                   BuildArrayIndexType, BuildArrayType, BuildSetType,
                   DebugTree,
                   ChainOn,
                   BuildPointerType,
                   BuildStartFunctionType, BuildEndFunctionType,
                   BuildParameterDeclaration,
                   BuildStartFunctionDeclaration, BuildEndFunctionDeclaration,
                   BuildStartMainModule, BuildEndMainModule,
                   AssignBooleanTrueFalse, BuildSize ;

(* %%%FORWARD%%%
PROCEDURE IsUnboundedDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE DeclareImportedVariables (Sym: CARDINAL) ; FORWARD ;
PROCEDURE DeclareSet (sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclarePointer (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclareLocalVariables (Sym: CARDINAL; i: CARDINAL) ; FORWARD ;
PROCEDURE DeclareDefaultTypes ; FORWARD ;
PROCEDURE DeclareGlobalVariables (ModSym: CARDINAL) ; FORWARD ;
PROCEDURE DeclareType (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclareKindOfType (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE DeclareOrFindKindOfType (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE IsEnumerationDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsSubrangeDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsPointerDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsRecordDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsVarientDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsArrayDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsSetDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsTypeDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsProcTypeDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE DeclareEnumeration (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE AllDependantsWritten (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE DeclareVarient (Sym: CARDINAL) : Tree ; FORWARD ;
PROCEDURE ForceDeclareType (sym: CARDINAL) : Tree ; FORWARD ;
   %%%FORWARD%%% *)


CONST
   MaxString = 8192 ;

TYPE
   StartProcedure = PROCEDURE (ADDRESS) : Tree ;

VAR
   ToFinishList,                    (* those types which have need to *)
                                    (* be finished.                   *)
   ToDoList            : List ;     (* Contains a list of all         *)
                                    (* outstanding types that need to *)
                                    (* be written to the assembly     *)
                                    (* file when its dependants have  *)
                                    (* been written.                  *)
   ToDoConstants       : List ;     (* all unresolved constants go    *)
                                    (* here, M2GenGCC resolves them.  *)
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
      InternalError('partially declared types are not all resolved', __FILE__, __LINE__)
   END
END CheckToFinishList ;


(*
   DeclaredOutandingTypes - writes out any types that have their dependants
                            solved. It returns TRUE if all outstanding types
                            have been written.
*)

PROCEDURE DeclaredOutstandingTypes (MustHaveCompleted: BOOLEAN) : BOOLEAN ;
VAR
   i, n         : CARDINAL ;
   NoMoreWritten: BOOLEAN ;
   Sym          : CARDINAL ;
BEGIN
   REPEAT
      NoMoreWritten := TRUE ;
      n := NoOfItemsInList(ToDoList) ;
      i := 1 ;
      WHILE i<=n DO
      	 Sym := GetItemFromList(ToDoList, i) ;
      	 IF NOT GccKnowsAbout(Sym)
      	 THEN
      	    IF AllDependantsWritten(Sym)
      	    THEN
               (* add relationship between gccSym and Sym *)
               AddModGcc(Sym, DeclareKindOfType(Sym)) ;
               RemoveItemFromList(ToDoList, Sym) ;
               n := NoOfItemsInList(ToDoList) ;
               i := 0 ;
      	       NoMoreWritten := FALSE
      	    END
      	 END ;
         INC(i)
      END
   UNTIL NoMoreWritten ;

   CheckToFinishList(TRUE) ;

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
            WriteString('// need to solve ') ;
            WriteCard(Sym, 4) ; Write(' ') ; WriteKey(GetSymName(Sym)) ;
            IF IsItemInList(ToFinishList, Sym)
            THEN
               WriteString(' partially declared')
            ELSE
               WriteString(' not declared at all')
            END ;
            WriteLn ;
            mystop ;
            FoldConstants ;
            IF NOT AllDependantsWritten(Sym)
            THEN
               NoMoreWritten := TRUE ;
               FormatWarningMessage2('internal error: circular type dependancy between %s (%d)',
                                     GetSymName(Sym), Sym, GetDeclared(Sym))
            END ;
         END ;
         INC(i)
      END ;
      i := 1 ;
      n := NoOfItemsInList(ToFinishList) ;
      WHILE i<=n DO
         WriteString('// symbol type has only been partically declared ') ;
         Sym := GetItemFromList(ToFinishList, i) ;
         WriteCard(Sym, 4) ; Write(' ') ; WriteKey(GetSymName(Sym)) ;
         WriteLn ;
         IF NOT AllDependantsWritten(Sym)
         THEN
            WriteString('dependants unresolved') ; WriteLn
         END ;
         INC(i) ;
         NoMoreWritten := TRUE
      END ;
      IF NoMoreWritten
      THEN
         InternalError('circular dependancies within above types', __FILE__, __LINE__)
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
      WriteString('// Base type ') ;
      WriteKey(GetSymName(Sym)) ;
      WriteString(' not understood') ;
      WriteLn ;
      InternalError('why is this unknown?', __FILE__, __LINE__)
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
   DeclareIntegerConstantFromTree - declares an integer constant from a Tree, value.
*)

PROCEDURE DeclareIntegerConstantFromTree (sym: CARDINAL; value: Tree) ;
BEGIN
   AddModGcc(sym, value)
END DeclareIntegerConstantFromTree ;


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

PROCEDURE PromoteToString (sym: CARDINAL) : Tree ;
VAR
   size: CARDINAL ;
BEGIN
   DeclareConstant(sym) ;
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
   DeclareConstant - checks to see whether, sym, is a constant and declares the constant to gcc.
*)

PROCEDURE DeclareConstant (sym: CARDINAL) ;
VAR
   size: CARDINAL ;
BEGIN
   IF IsConst(sym) AND (NOT GccKnowsAbout(sym))
   THEN
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
         IF GetType(sym)=LongReal
         THEN
            DeclareIntegerConstantFromTree(sym, PopRealTree())
         ELSE
            DeclareIntegerConstantFromTree(sym, PopIntegerTree())
         END
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

PROCEDURE DeclareTypeInfo (Sym: CARDINAL) ;
VAR
   gcc: Tree ;
BEGIN
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
   DeclareTypesInProcedure - declare all types in procedure, Sym, to GCC.
*)

PROCEDURE DeclareTypesInProcedure (Sym: CARDINAL) ;
BEGIN
   ForeachLocalSymDo(Sym, DeclareTypeInfo)
END DeclareTypesInProcedure ;


(*
   DeclareTypesInModule - declare all types in module, Sym, to GCC.
*)

PROCEDURE DeclareTypesInModule (Sym: CARDINAL) ;
BEGIN
   ForeachLocalSymDo(Sym, DeclareTypeInfo) ;
   ForeachProcedureDo(Sym, DeclareTypesInProcedure) ;
   ForeachInnerModuleDo(Sym, DeclareTypesInModule)
END DeclareTypesInModule ;


(*
   GetModuleWhereDeclared - returns the module where, Sym, was created.
*)

PROCEDURE GetModuleWhereDeclared (Sym: CARDINAL) : CARDINAL ;
VAR
   s: CARDINAL ;
BEGIN
   s := GetScopeAuthor(Sym) ;
   IF IsDefImp(s) OR IsModule(s)
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
          IsPseudoSystemFunction(Sym) OR IsPseudoMathFunction(Sym)
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
       (IsImported(GetBaseModule(), Sym)))
   THEN
      IF IsProcedureNested(Sym) AND (NOT GccKnowsAbout(GetScopeAuthor(Sym)))
      THEN
         DeclareProcedureToGcc(GetScopeAuthor(Sym))
      END ;

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
                                                    NIL, IsImported(GetMainModule(), Sym)))
      ELSE
         AddModGcc(Sym, BuildEndFunctionDeclaration(KeyToCharStar(GetFullSymName(Sym)),
                                                    Mod2Gcc(GetType(Sym)), IsImported(GetMainModule(), Sym)))
      END
   END
END DeclareProcedureToGcc ;


(*
   DeclareProcedure - declares procedure, sym, or all procedures inside
                      module sym.
*)

PROCEDURE DeclareProcedure (Sym: CARDINAL) ;
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

PROCEDURE FoldConstants ;
BEGIN
   IF ResolveConstantExpressions(ToDoConstants)
   THEN
   END
END FoldConstants ;


(*
   StartDeclareMainModule - declares types, variables associated with main module.
*)

PROCEDURE StartDeclareMainModule ;
VAR
   ModuleName,
   FileName  : ARRAY [0..MaxString] OF CHAR ;
   n, m      : CARDINAL ;
BEGIN
   GetKey(GetSymName(GetMainModule()), ModuleName) ;
   CalculateFileName(ModuleName, 'mod', FileName) ;
   SetFileNameAndLineNo(KeyToCharStar(MakeKey(FileName)), 1) ;
   DeclareDefaultTypes ;
   ForeachModuleDo(DeclareTypesInModule) ;
   REPEAT
      n := NoOfItemsInList(ToDoConstants) ;
      WHILE ResolveConstantExpressions(ToDoConstants) DO
      END ;
      m := NoOfItemsInList(ToDoConstants) ;
      (* we need to evaluate some constant expressions to resolve these types *)
      IF DeclaredOutstandingTypes(FALSE)
      THEN
      END ;
   UNTIL (NOT ResolveConstantExpressions(ToDoConstants)) AND (n=NoOfItemsInList(ToDoConstants)) AND
         (m=NoOfItemsInList(ToDoConstants));
   IF DeclaredOutstandingTypes(TRUE)
   THEN
   END ;
   ForeachModuleDo(DeclareProcedure) ;

   (* now that all types have been resolved it is safe to declare variables *)
   DeclareGlobalVariables(GetMainModule()) ;
   ForeachImportedDo(GetMainModule(), DeclareImportedVariables) ;
   (* now it is safe to declare all procedures *)
   ForeachProcedureDo(GetMainModule(), DeclareProcedure) ;
   ForeachInnerModuleDo(GetMainModule(), DeclareProcedure) ;
   BuildStartMainModule
END StartDeclareMainModule ;


(*
   EndDeclareMainModule - removes the scope associated with main module.
*)

PROCEDURE EndDeclareMainModule ;
BEGIN
   BuildEndMainModule
END EndDeclareMainModule ;


(*
   DeclareDefaultType - declares a default type, sym, with, name.
*)

PROCEDURE DeclareDefaultType (sym: CARDINAL; name: ARRAY OF CHAR; gcctype: Tree) ;
VAR
   t        : Tree ;
   high, low: CARDINAL ;
BEGIN
   (* DeclareKnownType will declare a new identifier as a type of, gcctype *)
   t := DeclareKnownType(KeyToCharStar(MakeKey(name)), gcctype) ;
   AddModGcc(sym, t) ;
   (*
      this is very simplistic and assumes that the caller only uses Subranges, Sets and GCC types.
      We need to declare any constants with the types so that AllDependantsWritten works.
   *)
   IF IsSubrange(sym)
   THEN
      GetSubrange(sym, high, low) ;
      DeclareConstant(high) ;
      DeclareConstant(low)
   ELSIF IsSet(sym)
   THEN
      IF IsSubrange(GetType(sym))
      THEN
         GetSubrange(GetType(sym), high, low) ;
         DeclareConstant(high) ;
         DeclareConstant(low)
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
      WriteErrorFormat1('defining a default type (%s) before its dependants are known', MakeKey(name))
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
   DeclareDefaultTypes - makes default types known to GCC
*)

PROCEDURE DeclareDefaultTypes ;
BEGIN
   IF NOT HaveInitDefaultTypes
   THEN
      HaveInitDefaultTypes := TRUE ;
      (* Integer *)
      DeclareDefaultType(Integer , "INTEGER" , GetIntegerType()) ;
      (* Char *)
      DeclareDefaultType(Char    , "CHAR"    , GetCharType()) ;
      (* Cardinal *)
      DeclareDefaultType(Cardinal, "CARDINAL", GetCardinalType()) ;
      (* Word *)
      DeclareDefaultType(Word    , "WORD"    , GetWordType()) ;
      (* Proc *)
      DeclareDefaultType(Proc    , "PROC"    , GetProcType()) ;
      (* Byte *)
      DeclareDefaultType(Byte    , "BYTE"    , GetByteType()) ;
      (* Address *)
      DeclareDefaultType(Address , "ADDRESS" , GetPointerType()) ;
      (* LongInt *)
      DeclareDefaultType(LongInt , "LONGINT" , GetLongIntType()) ;
      (* Real *)
      DeclareDefaultType(Real    , "REAL"    , GetRealType()) ;
      (* LongReal *)
      DeclareDefaultType(LongReal, "LONGREAL", GetLongRealType()) ;
      (* Bitset *)
      DeclareDefaultType(Bitset  , "BITSET"  , GetBitsetType()) ;
      DeclareBoolean
   END
END DeclareDefaultTypes ;


(*
   AlignDeclarationWithSource - given a symbol, sym, set the source file and line
                                number with the declaration position of sym.
*)

PROCEDURE AlignDeclarationWithSource (sym: CARDINAL) ;
VAR
   a: ARRAY [0..MaxString] OF CHAR ;
   t: CARDINAL ;
BEGIN
   t := GetDeclared(sym) ;
   FindFileNameFromToken(a, t) ;
   SetFileNameAndLineNo(ADR(a), TokenToLineNo(t))
END AlignDeclarationWithSource ;


(*
   DeclareVariable - declares a global variable to GCC.
*)

PROCEDURE DeclareVariable (ModSym, Son: CARDINAL) ;
BEGIN
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

PROCEDURE DeclareImportedVariables (Sym: CARDINAL) ;
BEGIN
   IF IsVar(Sym)
   THEN
      DeclareVariable(GetMainModule(), Sym)
   END
END DeclareImportedVariables ;


(*
   DeclareLocalVariables - declares Local variables for procedure Sym.
*)

PROCEDURE DeclareLocalVariables (Sym: CARDINAL) ;
VAR
   i, Var: CARDINAL ;
BEGIN
   DeclareTypesInProcedure(Sym) ;
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

PROCEDURE DeclareFieldEnumeration (Sym: CARDINAL) ;
BEGIN
   (* add relationship between gccSym and Sym *)
   PushValue(Sym) ;
   AddModGcc(Sym, BuildEnumerator(KeyToCharStar(GetSymName(Sym)), PopIntegerTree()))
END DeclareFieldEnumeration ;


(*
   DeclareEnumeration - declare an enumerated type.
*)

PROCEDURE DeclareEnumeration (Sym: CARDINAL) : Tree ;
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
   IF Sym=584
   THEN
      mystop
   END ;
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
               WriteKey(GetSymName(Field)) ;
               WriteString(' found unexpected field varient') ; WriteLn ;
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
   RETURN( BuildPointerType(DeclareOrFindKindOfType(GetType(Sym))) )
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
      FieldList  := ChainOn(BuildFieldRecord(KeyToCharStar(MakeKey('_ArrayAddress')),
                                             BuildPointerType(Mod2Gcc(Char))),
                            BuildFieldRecord(KeyToCharStar(MakeKey('_ArrayHigh')),
                                             Mod2Gcc(Cardinal))) ;
      RETURN( BuildEndRecord(RecordType, FieldList) )
   ELSE
      RETURN( Mod2Gcc(Unbounded) )
   END ;
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
            WriteString('error with array ') ; WriteKey(GetSymName(Sym)) ;
            WriteString(' subscript: ') ; WriteCard(i, 0) ;
            WriteString(' no subrange for this subscript, instead the TYPE given is ') ;
            WriteKey(GetSymName(Subrange)) ; WriteLn
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
   GccParam  : Tree ;
BEGIN
   ReturnType := GetType(Sym) ;
   BuildStartFunctionType ;
   p := NoOfParam(Sym) ;
   i := p ;
   Assert(PushParametersLeftToRight) ;
   WHILE i>0 DO
      Son := GetNthParam(Sym, i) ;
      GccParam := BuildParameterDeclaration(NIL, Mod2Gcc(GetType(Son)), IsVarParam(Sym, i)) ;
      AddModGcc(Son, GccParam) ;
      DEC(i)
   END ;
   IF ReturnType=NulSym
   THEN
      RETURN( BuildEndFunctionType(NIL) )
   ELSE
      RETURN( BuildEndFunctionType(Mod2Gcc(ReturnType)) )
   END
END DeclareProcType ;


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
      type := GetType(low) ;  (* --fixme-- this should be removed once IsSubrangeDependantsWritten has been fixed *)
      gccsym := BuildSetType(KeyToCharStar(GetSymName(sym)),
                             Mod2Gcc(type), Mod2Gcc(low), Mod2Gcc(high))
   ELSE
      gccsym := BuildSetType(KeyToCharStar(GetSymName(sym)),
                             Mod2Gcc(type), GetMinFrom(Mod2Gcc(type)), GetMaxFrom(Mod2Gcc(type)));
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
         WriteString('problems after completing the type definition (before)') ; WriteLn ;
         DebugTree(t1) ;
         WriteString('problems after completing the type definition (after)') ; WriteLn ;
         DebugTree(t2)
      END
   END ;
   AddModGcc(sym, t1) ;
   RETURN( t1 )
END ForceDeclareType ;


(*
   DeclareOrFindKindOfType - firstly lookup the symbol, if it is known return this symbol
                             otherwise declare the symbol. Remember that we have have
                             particially declared symbols in the Mod2Gcc lookup facility.
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
   IsSubrangeDependantsWritten - returns true if the subrange
                                 dependants have been written to the
                                 assembly file.
*)

PROCEDURE IsSubrangeDependantsWritten (Sym: CARDINAL) : BOOLEAN ;
VAR
   high, low: CARDINAL ;
BEGIN
   GetSubrange(Sym, high, low) ;
   PutSubrange(Sym, low, high, Integer) ;  (* --testing--     when removed alter DeclareSet accordingly  *)
   (* low and high are not types but constants and they are resolved by M2GenGCC *)
   IF NOT GccKnowsAbout(low)
   THEN
      IncludeItemIntoList(ToDoConstants, low)
   END ;
   IF NOT GccKnowsAbout(high)
   THEN
      IncludeItemIntoList(ToDoConstants, high)
   END ;
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
BEGIN
   RETURN( IsSymTypeKnown(Sym, GetType(Sym)) )
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
 
   IF (NOT IsSymTypeKnown(Sym, Type)) AND (NOT AllDependantsWritten(Type))
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
            WriteString('error with array ') ; WriteKey(GetSymName(Sym)) ;
            WriteString(' subscript: ') ; WriteCard(i, 0) ;
            WriteString(' no subrange for this subscript instead the TYPE given is ') ;
            WriteKey(GetSymName(Subrange)) ; WriteLn
         END ;
         Assert(IsSubrange(Subrange)) ;
         GetSubrange(Subrange, High, Low) ;

         (*
            --fixme-- when we have fixed IsSubrangeDependantsWritten we should be able
                      to use that function rather that the rather convoluted tests below.
                      However IsSubrangeDependantsWritten alters subranges to INTEGER which
                      must be fixed first.

                      IF NOT IsSubrangeDependantsWritten(Subrange)
                      THEN
                         RETURN( FALSE )
                      END
         *)

         IF IsFieldEnumeration(High)
         THEN
            IF NOT AllDependantsWritten(GetType(High))
            THEN
               solved := FALSE
            END
         ELSIF IsConst(High)
         THEN
            IF NOT GccKnowsAbout(High)
            THEN
               IncludeItemIntoList(ToDoConstants, High) ;
               solved := FALSE
            END
         ELSE
            IF NOT IsSymTypeKnown(Sym, High)
            THEN
               solved := FALSE
            END
         END ;
         IF IsFieldEnumeration(Low)
         THEN
            IF NOT AllDependantsWritten(GetType(Low))
            THEN
               solved := FALSE
            END
         ELSIF IsConst(Low)
         THEN
            IF NOT GccKnowsAbout(Low)
            THEN
               IncludeItemIntoList(ToDoConstants, Low) ;
               solved := FALSE
            END
         ELSE
            IF NOT IsSymTypeKnown(Sym, Low)
            THEN
               solved := FALSE
            END
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

PROCEDURE IsSetDependantsWritten (Sym: CARDINAL) : BOOLEAN ;
VAR
   Type     : CARDINAL ;
   Low, High: CARDINAL ;
   solved   : BOOLEAN ;
BEGIN
   Assert(IsSet(Sym)) ;

   Type := GetType(Sym) ;

   IF IsSubrange(Type)
   THEN
      GetSubrange(Type, High, Low) ;

      (*
          --fixme--          --fixme--          --fixme--          --fixme--
          --fixme-- when we have fixed IsSubrangeDependantsWritten we should be able
                    to use that function rather that the rather convoluted tests below.
                    However IsSubrangeDependantsWritten alters subranges to INTEGER which
                    must be fixed first.
          --fixme--          --fixme--          --fixme--          --fixme--

         RETURN( IsSymTypeKnown(Sym, Subrange) AND AllDependantsWritten(Subrange) )
     *)

      solved := TRUE ;
      IF IsFieldEnumeration(High)
      THEN
         IF NOT AllDependantsWritten(GetType(High))
         THEN
            solved := FALSE
         END
      ELSIF IsConst(High)
      THEN
         IF NOT GccKnowsAbout(High)
         THEN
            IncludeItemIntoList(ToDoConstants, High) ;
            solved := FALSE
         END
      ELSE
         IF NOT IsSymTypeKnown(Sym, High)
         THEN
            solved := FALSE
         END
      END ;
      IF IsFieldEnumeration(Low)
      THEN
         IF NOT AllDependantsWritten(GetType(Low))
         THEN
            solved := FALSE
         END
      ELSIF IsConst(Low)
      THEN
         IF NOT GccKnowsAbout(Low)
         THEN
            IncludeItemIntoList(ToDoConstants, Low) ;
            solved := FALSE
         END
      ELSE
         IF NOT IsSymTypeKnown(Sym, Low)
         THEN
            solved := FALSE
         END
      END ;
      RETURN( IsSymTypeKnown(Sym, GetType(Type)) AND solved )
   ELSE
      RETURN( IsSymTypeKnown(Sym, Type) AND AllDependantsWritten(Type) )
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
BEGIN
   solved := TRUE ;
   Assert(IsProcType(Sym)) ;
   i := 1 ;
   ReturnType := GetType(Sym) ;
   p := NoOfParam(Sym) ;
   WHILE i<=p DO
      Son := GetNthParam(Sym, i) ;
      ParamType := GetType(Son) ;
      IF NOT IsSymTypeKnown(Sym, ParamType)
      THEN
         solved := FALSE
      END ;
      INC(i)
   END ;
   IF NOT IsSymTypeKnown(Sym, ReturnType)
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
      IF GetSymName(Sym)#NulName
      THEN
         IF NOT GccKnowsAbout(Sym)
         THEN
            IncludeItemIntoList(ToDoList, Sym) ;
            solved := FALSE ;
         END
      END ;
      type := GetType(Sym) ;
      IF NOT IsSymTypeKnown(Sym, type)
      THEN
         solved := FALSE
      END
   END ;
   RETURN( solved )
END IsTypeDependantsWritten ;


BEGIN
   InitList(ToDoList) ;
   InitList(ToDoConstants) ;
   InitList(ToFinishList) ;
   HaveInitDefaultTypes := FALSE
END M2GCCDeclare.
(*
 * Local variables:
 *  compile-command: "m2f -quiet -g -verbose -M \"../../libs ../ ../../../../ .\" -o M2GCCDeclare.o M2GCCDeclare.mod"
 * End:
 *)
