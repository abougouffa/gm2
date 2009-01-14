(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
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

IMPLEMENTATION MODULE SymbolTable ;


FROM SYSTEM IMPORT ADDRESS ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Debug IMPORT Assert ;

FROM M2Options IMPORT Pedantic, ExtendedOpaque ;

FROM M2ALU IMPORT InitValue, PtrToValue, PushCard, PopInto,
                  PushString, PushFrom, PushChar, PushInt,
                  IsSolved, IsValueConst ;

FROM M2Error IMPORT Error, NewError, ChainError, InternalError,
                    ErrorFormat0, ErrorFormat1, ErrorFormat2,
                    WriteFormat0, WriteFormat1, WriteFormat2, ErrorString,
                    ErrorAbort0, FlushErrors ;

FROM M2MetaError IMPORT MetaError1, MetaError2, MetaError3 ;
FROM M2LexBuf IMPORT GetTokenNo ;
FROM FormatStrings IMPORT Sprintf1 ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3, printf4 ;

FROM DynamicStrings IMPORT String, string, InitString,
                           InitStringCharStar, Mark, KillString, Length,
                           Index, char ;

FROM Lists IMPORT List, InitList, GetItemFromList, PutItemIntoList,
                  IsItemInList, IncludeItemIntoList, NoOfItemsInList,
                  RemoveItemFromList, ForeachItemInListDo ;

FROM NameKey IMPORT Name, MakeKey, makekey, NulName, WriteKey, LengthKey, GetKey, KeyToCharStar ;

FROM SymbolKey IMPORT NulKey, SymbolTree,
                      InitTree,
                      GetSymKey, PutSymKey, DelSymKey, IsEmptyTree,
                      DoesTreeContainAny, ForeachNodeDo ;

FROM M2Base IMPORT MixTypes, InitBase, Char, Integer, LongReal,
                   Cardinal, LongInt, LongCard, ZType, RType ;

FROM M2System IMPORT Address ;
FROM gccgm2 IMPORT DetermineSizeOfConstant, Tree ;
FROM StrLib IMPORT StrEqual ;

FROM M2Comp IMPORT CompilingDefinitionModule,
                   CompilingImplementationModule ;

IMPORT Indexing ;


CONST
   MaxScopes             =     50 ; (* Maximum number of scopes at any one   *)
                                    (* time.                                 *)
   MaxSymbols           = 100000 ;  (* Maximum number of symbols.            *)
   DebugUnknowns        =  FALSE ;

   (*
      The Unbounded is a pseudo type used within the compiler
      to implement dynamic parameter arrays.  It is implmented
      as a record structure which has the following fields:

      RECORD
         _m2_contents: POINTER TO type ;
         _m2_high    : CARDINAL ;
      END ;
   *)

   UnboundedAddressName = "_m2_contents" ;
   UnboundedHighName    = "_m2_high" ;

TYPE
   LRLists = ARRAY [RightValue..LeftValue] OF List ;

   TypeOfSymbol = (RecordSym, VarientSym, DummySym,
                   VarSym, EnumerationSym, SubrangeSym, ArraySym,
                   ConstStringSym, ConstVarSym, ConstLitSym,
                   VarParamSym, ParamSym, PointerSym,
                   UndefinedSym, TypeSym,
                   RecordFieldSym, VarientFieldSym, EnumerationFieldSym,
                   DefImpSym, ModuleSym, SetSym, ProcedureSym, ProcTypeSym,
                   SubscriptSym, UnboundedSym, GnuAsmSym, InterfaceSym,
                   ObjectSym, PartialUnboundedSym, TupleSym,
                   ErrorSym) ;

   Where = RECORD
              Declared,
              FirstUsed: CARDINAL ;
           END ;

   SymTuple = RECORD
                 At    : Where ;
                 nTuple: CARDINAL ;
                 list  : Indexing.Index ;
              END ;

   SymError     = RECORD
                     name      : Name ;
                     At        : Where ;      (* Where was sym declared/used *)
                  END ;

   SymObject    = RECORD
                     name      : Name ;
                     At        : Where ;      (* Where was sym declared/used *)
                  END ;

   SymUndefined = RECORD
                     name      : Name ;       (* Index into name array, name *)
                                              (* of record.                  *)
                     Unbounded : CARDINAL ;   (* The unbounded sym for this  *)
                     At        : Where ;      (* Where was sym declared/used *)
                  END ;

   SymGnuAsm    = RECORD
                     String    : CARDINAL ;   (* (ConstString) the assembly  *)
                                              (* instruction.                *)
                     At        : Where ;      (* Where was sym declared/used *)
                     Inputs,
                     Outputs,
                     Trashed   : CARDINAL ;   (* The interface symbols.      *)
                     Volatile  : BOOLEAN ;    (* Declared as ASM VOLATILE ?  *)
                  END ;

   SymInterface = RECORD
                     StringList: List ;       (* regnames or constraints     *)
                     ObjectList: List ;       (* list of M2 syms             *)
                     At        : Where ;      (* Where was sym declared/used *)
                  END ;

   SymVarient = RECORD
                   Size        : PtrToValue ; (* Size at runtime of symbol.  *)
                   ListOfSons  : List ;       (* ListOfSons contains a list  *)
                                              (* of SymRecordField and       *)
                                              (* SymVarients                 *)
                                              (* declared by the source      *)
                                              (* file.                       *)
                   Parent      : CARDINAL ;   (* Points to the parent symbol *)
                   Varient     : CARDINAL ;   (* Index into symbol table to  *)
                                              (* determine the associated    *)
                                              (* varient symbol.             *)
                   Scope       : CARDINAL ;   (* Scope of declaration.       *)
                   At          : Where ;      (* Where was sym declared/used *)
               END ;

   SymRecord = RECORD
                  name         : Name ;       (* Index into name array, name *)
                                              (* of record.                  *)
                  LocalSymbols : SymbolTree ; (* Contains all record fields. *)
                  Size         : PtrToValue ; (* Size at runtime of symbol.  *)
                  ListOfSons   : List ;       (* ListOfSons contains a list  *)
                                              (* of SymRecordField and       *)
                                              (* SymVarients                 *)
                                              (* declared by the source      *)
                                              (* file.                       *)
                  Unbounded    : CARDINAL ;   (* The unbounded sym for this  *)
                  Parent       : CARDINAL ;   (* Points to the parent symbol *)
                  Scope        : CARDINAL ;   (* Scope of declaration.       *)
                  At           : Where ;      (* Where was sym declared/used *)
               END ;

   SymSubrange = RECORD
                    name       : Name ;       (* Index into name array, name *)
                                              (* of subrange.                *)
                    Low        : CARDINAL ;   (* Index to symbol for lower   *)
                    High       : CARDINAL ;   (* Index to symbol for higher  *)
                    Size       : PtrToValue ; (* Size of subrange type.      *)
                    Type       : CARDINAL ;   (* Index to type symbol for    *)
                                              (* the type of subrange.       *)
                    Unbounded  : CARDINAL ;   (* The unbounded sym for this  *)
                    Scope      : CARDINAL ;   (* Scope of declaration.       *)
                    At         : Where ;      (* Where was sym declared/used *)
                 END ;

   SymEnumeration =
                RECORD
                   name        : Name ;       (* Index into name array, name *)
                                              (* of enumeration.             *)
                   NoOfElements: CARDINAL ;   (* No elements in enumeration  *)
                   LocalSymbols: SymbolTree ; (* Contains all enumeration    *)
                                              (* fields.                     *)
                   Size        : PtrToValue ; (* Size at runtime of symbol.  *)
                   Unbounded   : CARDINAL ;   (* The unbounded sym for this  *)
                   Scope       : CARDINAL ;   (* Scope of declaration.       *)
                   At          : Where ;      (* Where was sym declared/used *)
                END ;

   SymArray = RECORD
                 name        : Name ;         (* Index into name array, name *)
                                              (* of array.                   *)
                 Subscript   : CARDINAL ;     (* the subscript for this      *)
                                              (* array.                      *)
                 Size        : PtrToValue ;   (* Size at runtime of symbol.  *)
                 Offset      : PtrToValue ;   (* Offset at runtime of symbol *)
                 Type        : CARDINAL ;     (* Type of the Array.          *)
                 Unbounded   : CARDINAL ;     (* The unbounded sym for this  *)
                 Scope       : CARDINAL ;     (* Scope of declaration.       *)
                 At          : Where ;        (* Where was sym declared/used *)
              END ;

  SymSubscript = RECORD
                    Type       : CARDINAL ;   (* Index to a subrange symbol. *)
                    Size       : PtrToValue ; (* Size of this indice in*Size *)
                    Offset     : PtrToValue ; (* Offset at runtime of symbol *)
                                              (* Pseudo ie: Offset+Size*i    *)
                                              (* 1..n. The array offset is   *)
                                              (* the real memory offset.     *)
                                              (* This offset allows the a[i] *)
                                              (* to be calculated without    *)
                                              (* the need to perform         *)
                                              (* subtractions when a[4..10]  *)
                                              (* needs to be indexed.        *)
                    At         : Where ;      (* Where was sym declared/used *)
                 END ;

   SymUnbounded = RECORD
                     Type       : CARDINAL ;  (* Index to Simple type symbol *)
                     Size       : PtrToValue ;(* Max No of words ever        *)
                                              (* passed to this type.        *)
                     RecordType : CARDINAL ;  (* Record type used to         *)
                                              (* implement the unbounded.    *)
                     Scope      : CARDINAL ;  (* Scope of declaration.       *)
                     At         : Where ;     (* Where was sym declared/used *)
                  END ;

   SymPartialUnbounded = RECORD
                            Type: CARDINAL ;  (* Index to Simple type symbol *)
                         END ;

   SymProcedure
          = RECORD
               name          : Name ;       (* Index into name array, name   *)
                                            (* of procedure.                 *)
               ListOfParam   : List ;       (* Contains a list of all the    *)
                                            (* parameters in this procedure. *)
               ParamDefined  : BOOLEAN ;    (* Have the parameters been      *)
                                            (* defined yet?                  *)
               DefinedInDef  : BOOLEAN ;    (* Were the parameters defined   *)
                                            (* in the Definition module?     *)
                                            (* Note that this depends on     *)
                                            (* whether the compiler has read *)
                                            (* the .def or .mod first.       *)
                                            (* The second occurence is       *)
                                            (* compared to the first.        *)
               DefinedInImp  : BOOLEAN ;    (* Were the parameters defined   *)
                                            (* in the Implementation module? *)
                                            (* Note that this depends on     *)
                                            (* whether the compiler has read *)
                                            (* the .def or .mod first.       *)
                                            (* The second occurence is       *)
                                            (* compared to the first.        *)
               HasVarArgs    : BOOLEAN ;    (* Does this procedure use ... ? *)
               HasOptArg     : BOOLEAN ;    (* Does this procedure use [ ] ? *)
               OptArgInit    : CARDINAL ;   (* The optarg initial value.     *)
               IsBuiltin     : BOOLEAN ;    (* Was it declared __BUILTIN__ ? *)
               BuiltinName   : Name ;       (* name of equivalent builtin    *)
               IsInline      : BOOLEAN ;    (* Was is declared __INLINE__ ?  *)
               ReturnOptional: BOOLEAN ;    (* Is the return value optional? *)
               Unresolved    : SymbolTree ; (* All symbols currently         *)
                                            (* unresolved in this procedure. *)
               ScopeQuad     : CARDINAL ;   (* Index into quads for scope    *)
               StartQuad     : CARDINAL ;   (* Index into quads for start    *)
                                            (* of procedure.                 *)
               EndQuad       : CARDINAL ;   (* Index into quads for end of   *)
                                            (* procedure.                    *)
               Reachable     : BOOLEAN ;    (* Defines if procedure will     *)
                                            (* ever be called by the main    *)
                                            (* Module.                       *)
               SavePriority  : BOOLEAN ;    (* Does procedure need to save   *)
                                            (* and restore interrupts?       *)
               ReturnType    : CARDINAL ;   (* Return type for function.     *)
               Offset        : CARDINAL ;   (* Location of procedure used    *)
                                            (* in Pass 2 and if procedure    *)
                                            (* is a syscall.                 *)
               LocalSymbols: SymbolTree ;   (* Contains all symbols declared *)
                                            (* within this procedure.        *)
               EnumerationScopeList: List ;
                                            (* Enumeration scope list which  *)
                                            (* contains a list of all        *)
                                            (* enumerations which are        *)
                                            (* visable within this scope.    *)
               ListOfVars    : List ;       (* List of variables in this     *)
                                            (* scope.                        *)
               ListOfProcs   : List ;       (* List of all procedures        *)
                                            (* declared within this          *)
                                            (* procedure.                    *)
               NamedObjects  : SymbolTree ; (* Names of all items declared.  *)
               Size          : PtrToValue ; (* Activation record size.       *)
               TotalParamSize: PtrToValue ; (* size of all parameters.       *)
               ExceptionFinally,
               ExceptionBlock: BOOLEAN ;    (* does it have an exception?    *)
               Scope         : CARDINAL ;   (* Scope of declaration.         *)
               ListOfModules : List ;       (* List of all inner modules.    *)
               At            : Where ;      (* Where was sym declared/used   *)
            END ;

   SymProcType
          = RECORD
               name          : Name ;       (* Index into name array, name   *)
                                            (* of procedure.                 *)
               ListOfParam   : List ;       (* Contains a list of all the    *)
                                            (* parameters in this procedure. *)
               HasVarArgs    : BOOLEAN ;    (* Does this proc type use ... ? *)
               HasOptArg     : BOOLEAN ;    (* Does this procedure use [ ] ? *)
               OptArgInit    : CARDINAL ;   (* The optarg initial value.     *)
               ReturnType    : CARDINAL ;   (* Return type for function.     *)
               ReturnOptional: BOOLEAN ;    (* Is the return value optional? *)
               Scope         : CARDINAL ;   (* Scope of declaration.         *)
               Size          : PtrToValue ; (* Runtime size of symbol.       *)
               TotalParamSize: PtrToValue ; (* size of all parameters.       *)
               Unbounded     : CARDINAL ;   (* The unbounded sym for this    *)
               At            : Where ;      (* Where was sym declared/used   *)
            END ;

   SymParam = RECORD
                 name          : Name ;       (* Index into name array, name *)
                                              (* of param.                   *)
                 Type          : CARDINAL ;   (* Index to the type of param. *)
                 IsUnbounded   : BOOLEAN ;    (* ARRAY OF Type?              *)
                 ShadowVar     : CARDINAL ;   (* The local variable used to  *)
                                              (* shadow this parameter.      *)
                 At            : Where ;      (* Where was sym declared/used *)
              END ;

   SymVarParam = RECORD
                    name          : Name ;    (* Index into name array, name *)
                                              (* of param.                   *)
                    Type          : CARDINAL ;(* Index to the type of param. *)
                    IsUnbounded   : BOOLEAN ; (* ARRAY OF Type?              *)
                    ShadowVar     : CARDINAL ;(* The local variable used to  *)
                                              (* shadow this parameter.      *)
                    At            : Where ;   (* Where was sym declared/used *)
                 END ;

   SymConstString
               = RECORD
                    name     : Name ;         (* Index into name array, name *)
                                              (* of const.                   *)
                    String   : Name ;         (* Value of string.            *)
                    Length   : CARDINAL ;     (* StrLen(String)              *)
                    At       : Where ;        (* Where was sym declared/used *)
                 END ;

   SymConstLit = RECORD
                    name         : Name ;         (* Index into name array, name *)
                                                  (* of const.                   *)
                    Value        : PtrToValue ;   (* Value of the constant.      *)
                    Type         : CARDINAL ;     (* TYPE of constant, char etc  *)
                    IsSet        : BOOLEAN ;      (* is the constant a set?      *)
                    IsConstructor: BOOLEAN ;      (* is the constant a set?      *)
                    FromType     : CARDINAL ;     (* type is determined FromType *)
                    UnresFromType: BOOLEAN ;      (* is Type unresolved?         *)
                    At           : Where ;        (* Where was sym declared/used *)
                 END ;

   SymConstVar = RECORD
                    name         : Name ;     (* Index into name array, name *)
                                              (* of const.                   *)
                    Value        : PtrToValue ; (* Value of the constant     *)
                    Type         : CARDINAL ; (* TYPE of constant, char etc  *)
                    IsSet        : BOOLEAN ;  (* is the constant a set?      *)
                    IsConstructor: BOOLEAN ;  (* is the constant a set?      *)
                    FromType     : CARDINAL ; (* type is determined FromType *)
                    UnresFromType: BOOLEAN ;  (* is Type resolved?           *)
                    IsTemp       : BOOLEAN ;  (* is it a temporary?          *)
                    At           : Where ;    (* Where was sym declared/used *)
                 END ;

   SymVar = RECORD
               name          : Name ;         (* Index into name array, name *)
                                              (* of const.                   *)
               Type          : CARDINAL ;     (* Index to a type symbol.     *)
               BackType      : CARDINAL ;     (* specific back end symbol.   *)
               Size          : PtrToValue ;   (* Runtime size of symbol.     *)
               Offset        : PtrToValue ;   (* Offset at runtime of symbol *)
               AddrMode      : ModeOfAddr ;   (* Type of Addressing mode.    *)
               Scope         : CARDINAL ;     (* Scope of declaration.       *)
               AtAddress     : BOOLEAN ;      (* Is declared at address?     *)
               Address       : CARDINAL ;     (* Address at which declared   *)
               IsTemp        : BOOLEAN ;      (* Is variable a temporary?    *)
               IsParam       : BOOLEAN ;      (* Is variable a parameter?    *)
               IsPointerCheck: BOOLEAN ;      (* Is variable used to         *)
                                              (* dereference a pointer?      *)
               IsWritten     : BOOLEAN ;      (* Is variable written to?     *)
               At            : Where ;        (* Where was sym declared/used *)
               ReadUsageList,                 (* list of var read quads      *)
               WriteUsageList: LRLists ;      (* list of var write quads     *)
            END ;

   SymType = RECORD
                name     : Name ;             (* Index into name array, name *)
                                              (* of type.                    *)
                Type     : CARDINAL ;         (* Index to a type symbol.     *)
                IsHidden : BOOLEAN ;          (* Was it declared as hidden?  *)
                Size     : PtrToValue ;       (* Runtime size of symbol.     *)
                Unbounded: CARDINAL ;         (* The unbounded sym for this  *)
                Scope    : CARDINAL ;         (* Scope of declaration.       *)
                At       : Where ;            (* Where was sym declared/used *)
             END ;

   SymPointer
           = RECORD
                name     : Name ;             (* Index into name array, name *)
                                              (* of pointer.                 *)
                Type     : CARDINAL ;         (* Index to a type symbol.     *)
                Size     : PtrToValue ;       (* Runtime size of symbol.     *)
                Unbounded: CARDINAL ;         (* The unbounded sym for this  *)
                Scope    : CARDINAL ;         (* Scope of declaration.       *)
                At       : Where ;            (* Where was sym declared/used *)
             END ;

   SymRecordField =
             RECORD
                name     : Name ;             (* Index into name array, name *)
                                              (* of record field.            *)
                Type     : CARDINAL ;         (* Index to a type symbol.     *)
                Size     : PtrToValue ;       (* Runtime size of symbol.     *)
                Offset   : PtrToValue ;       (* Offset at runtime of symbol *)
                Parent   : CARDINAL ;         (* Index into symbol table to  *)
                                              (* determine the parent symbol *)
                                              (* for this record field. Used *)
                                              (* for BackPatching.           *)
                Varient  : CARDINAL ;         (* Index into symbol table to  *)
                                              (* determine the associated    *)
                                              (* varient symbol.             *)
                Scope    : CARDINAL ;         (* Scope of declaration.       *)
                At       : Where ;            (* Where was sym declared/used *)
             END ;

   SymVarientField =
             RECORD
                name     : Name ;             (* Index into name array, name *)
                                              (* of varient field (internal) *)
                Size     : PtrToValue ;       (* Runtime size of symbol.     *)
                Offset   : PtrToValue ;       (* Offset at runtime of symbol *)
                Parent   : CARDINAL ;         (* Index into symbol table to  *)
                                              (* determine the parent symbol *)
                                              (* for this record field. Used *)
                                              (* for BackPatching.           *)
                Varient  : CARDINAL ;         (* Index into symbol table to  *)
                                              (* determine the associated    *)
                                              (* varient symbol.             *)
                ListOfSons: List ;            (* Contains a list of the      *)
                                              (* RecordField symbols and     *)
                                              (* SymVarients                 *)
                Scope    : CARDINAL ;         (* Scope of declaration.       *)
                At       : Where ;            (* Where was sym declared/used *)
             END ;

   SymEnumerationField =
             RECORD
                name     : Name ;             (* Index into name array, name *)
                                              (* of enumeration field.       *)
                Value    : PtrToValue ;       (* Enumeration field value.    *)
                Type     : CARDINAL ;         (* Index to the enumeration.   *)
                Scope    : CARDINAL ;         (* Scope of declaration.       *)
                At       : Where ;            (* Where was sym declared/used *)
             END ;

   SymSet  = RECORD
      	        name     : Name ;             (* Index into name array, name *)
                                              (* of set.                     *)
                Type     : CARDINAL ;         (* Index to a type symbol.     *)
      	       	     	      	       	      (* (subrange or enumeration).  *)
                Size     : PtrToValue ;       (* Runtime size of symbol.     *)
                Unbounded: CARDINAL ;         (* The unbounded sym for this  *)
                Scope    : CARDINAL ;         (* Scope of declaration.       *)
                At       : Where ;            (* Where was sym declared/used *)
             END ;

   SymDefImp =
            RECORD
               name          : Name ;       (* Index into name array, name   *)
                                            (* of record field.              *)
               Type          : CARDINAL ;   (* Index to a type symbol.       *)
               Size          : PtrToValue ; (* Runtime size of symbol.       *)
               Offset        : PtrToValue ; (* Offset at runtime of symbol   *)
               ExportQualifiedTree: SymbolTree ;
                                            (* Holds all the export          *)
                                            (* Qualified identifiers.        *)
                                            (* This tree may be              *)
                                            (* deleted at the end of Pass 1. *)
               ExportUnQualifiedTree: SymbolTree ;
                                            (* Holds all the export          *)
                                            (* UnQualified identifiers.      *)
                                            (* This tree may be              *)
                                            (* deleted at the end of Pass 1. *)
               ExportRequest : SymbolTree ; (* Contains all identifiers that *)
                                            (* have been requested by other  *)
                                            (* modules before this module    *)
                                            (* declared its export list.     *)
                                            (* This tree should be empty at  *)
                                            (* the end of the compilation.   *)
                                            (* Each time a symbol is         *)
                                            (* exported it is removed from   *)
                                            (* this list.                    *)
               IncludeList   : List ;       (* Contains all included symbols *)
                                            (* which are included by         *)
                                            (* IMPORT modulename ;           *)
                                            (* modulename.Symbol             *)
               ImportTree    : SymbolTree ; (* Contains all IMPORTed         *)
                                            (* identifiers.                  *)
               ExportUndeclared: SymbolTree ;
                                            (* ExportUndeclared contains all *)
                                            (* the identifiers which were    *)
                                            (* exported but have not yet     *)
                                            (* been declared.                *)
               NeedToBeImplemented: SymbolTree ;
                                            (* NeedToBeImplemented contains  *)
                                            (* the identifiers which have    *)
                                            (* been exported and declared    *)
                                            (* but have not yet been         *)
                                            (* implemented.                  *)
               LocalSymbols  : SymbolTree ; (* The LocalSymbols hold all the *)
                                            (* variables declared local to   *)
                                            (* the block. It contains the    *)
                                            (* IMPORT r ;                    *)
                                            (* FROM _ IMPORT x, y, x ;       *)
                                            (*    and also                   *)
                                            (* MODULE WeAreHere ;            *)
                                            (*    x y z visible by localsym  *)
                                            (*    MODULE Inner ;             *)
                                            (*       EXPORT x, y, z ;        *)
                                            (*    END Inner ;                *)
                                            (* END WeAreHere.                *)
               EnumerationScopeList: List ; (* Enumeration scope list which  *)
                                            (* contains a list of all        *)
                                            (* enumerations which are        *)
                                            (* visible within this scope.    *)
               NamedObjects  : SymbolTree ; (* Names of all items declared.  *)
               NamedImports  : SymbolTree ; (* Names of items imported.      *)
               WhereImported : SymbolTree ; (* Sym to TokenNo where import   *)
                                            (* occurs. Error message use.    *)
               Priority      : CARDINAL ;   (* Priority of the module. This  *)
                                            (* is an index to a constant.    *)
               Unresolved    : SymbolTree ; (* All symbols currently         *)
                                            (* unresolved in this module.    *)
               StartQuad     : CARDINAL ;   (* Signify the initialization    *)
                                            (* code.                         *)
               EndQuad       : CARDINAL ;   (* EndQuad should point to a     *)
                                            (* goto quad.                    *)
               StartFinishQuad: CARDINAL ;  (* Signify the finalization      *)
                                            (* code.                         *)
               EndFinishQuad : CARDINAL ;   (* should point to a finish      *)
               FinallyFunction: Tree ;      (* The GCC function for finally  *)
               ExceptionFinally,
               ExceptionBlock: BOOLEAN ;    (* does it have an exception?    *)
               ContainsHiddenType: BOOLEAN ;(* True if this module           *)
                                            (* implements a hidden type.     *)
               ForC          : BOOLEAN ;    (* Is it a definition for "C"    *)
               NeedExportList: BOOLEAN ;    (* Must user supply export list? *)
               ListOfVars    : List ;       (* List of variables in this     *)
                                            (* scope.                        *)
               ListOfProcs   : List ;       (* List of all procedures        *)
                                            (* declared within this module.  *)
               ListOfModules : List ;       (* List of all inner modules.    *)
               At            : Where ;      (* Where was sym declared/used *)
            END ;

   SymModule =
            RECORD
               name          : Name ;       (* Index into name array, name   *)
                                            (* of record field.              *)
               Size          : PtrToValue ; (* Runtime size of symbol.       *)
               Offset        : PtrToValue ; (* Offset at runtime of symbol   *)
               LocalSymbols  : SymbolTree ; (* The LocalSymbols hold all the *)
                                            (* variables declared local to   *)
                                            (* the block. It contains the    *)
                                            (* IMPORT r ;                    *)
                                            (* FROM _ IMPORT x, y, x ;       *)
                                            (*    and also                   *)
                                            (* MODULE WeAreHere ;            *)
                                            (*    x y z visiable by localsym *)
                                            (*    MODULE Inner ;             *)
                                            (*       EXPORT x, y, z ;        *)
                                            (*    END Inner ;                *)
                                            (* END WeAreHere.                *)
               ExportTree    : SymbolTree ; (* Holds all the exported        *)
                                            (* identifiers.                  *)
                                            (* This tree may be              *)
                                            (* deleted at the end of Pass 1. *)
               IncludeList   : List ;       (* Contains all included symbols *)
                                            (* which are included by         *)
                                            (* IMPORT modulename ;           *)
                                            (* modulename.Symbol             *)
               ImportTree    : SymbolTree ; (* Contains all IMPORTed         *)
                                            (* identifiers.                  *)
               ExportUndeclared: SymbolTree ;
                                            (* ExportUndeclared contains all *)
                                            (* the identifiers which were    *)
                                            (* exported but have not yet     *)
                                            (* been declared.                *)
               EnumerationScopeList: List ; (* Enumeration scope list which  *)
                                            (* contains a list of all        *)
                                            (* enumerations which are        *)
                                            (* visable within this scope.    *)
               NamedObjects  : SymbolTree ; (* Names of all items declared.  *)
               NamedImports  : SymbolTree ; (* Names of items imported.      *)
               WhereImported : SymbolTree ; (* Sym to TokenNo where import   *)
                                            (* occurs. Error message use.    *)
               Scope         : CARDINAL ;   (* Scope of declaration.         *)
               Priority      : CARDINAL ;   (* Priority of the module. This  *)
                                            (* is an index to a constant.    *)
               Unresolved    : SymbolTree ; (* All symbols currently         *)
                                            (* unresolved in this module.    *)
               StartQuad     : CARDINAL ;   (* Signify the initialization    *)
                                            (* code.                         *)
               EndQuad       : CARDINAL ;   (* EndQuad should point to a     *)
                                            (* goto quad.                    *)
               StartFinishQuad: CARDINAL ;  (* Signify the finalization      *)
                                            (* code.                         *)
               EndFinishQuad : CARDINAL ;   (* should point to a finish      *)
               FinallyFunction: Tree ;      (* The GCC function for finally  *)
               ExceptionFinally,
               ExceptionBlock: BOOLEAN ;    (* does it have an exception?    *)
               ListOfVars    : List ;       (* List of variables in this     *)
                                            (* scope.                        *)
               ListOfProcs   : List ;       (* List of all procedures        *)
                                            (* declared within this module.  *)
               ListOfModules : List ;       (* List of all inner modules.    *)
               At            : Where ;      (* Where was sym declared/used   *)
            END ;

   SymDummy =
            RECORD
               NextFree     : CARDINAL ;    (* Link to the next free symbol. *)
            END ;


   Symbol = RECORD
               CASE SymbolType : TypeOfSymbol OF
                                            (* Determines the type of symbol *)

               ObjectSym           : Object           : SymObject |
               RecordSym           : Record           : SymRecord |
               VarientSym          : Varient          : SymVarient |
               VarSym              : Var              : SymVar |
               EnumerationSym      : Enumeration      : SymEnumeration |
               SubrangeSym         : Subrange         : SymSubrange |
               SubscriptSym        : Subscript        : SymSubscript |
               ArraySym            : Array            : SymArray |
               UnboundedSym        : Unbounded        : SymUnbounded |
               PartialUnboundedSym : PartialUnbounded : SymPartialUnbounded |
               ConstVarSym         : ConstVar         : SymConstVar |
               ConstLitSym         : ConstLit         : SymConstLit |
               ConstStringSym      : ConstString      : SymConstString |
               VarParamSym         : VarParam         : SymVarParam |
               ParamSym            : Param            : SymParam |
               ErrorSym            : Error            : SymError |
               UndefinedSym        : Undefined        : SymUndefined |
               TypeSym             : Type             : SymType |
               PointerSym          : Pointer          : SymPointer |
               RecordFieldSym      : RecordField      : SymRecordField |
               VarientFieldSym     : VarientField     : SymVarientField |
               EnumerationFieldSym : EnumerationField : SymEnumerationField |
               DefImpSym           : DefImp           : SymDefImp |
               ModuleSym           : Module           : SymModule |
               SetSym              : Set              : SymSet |
               ProcedureSym        : Procedure        : SymProcedure |
               ProcTypeSym         : ProcType         : SymProcType |
               GnuAsmSym           : GnuAsm           : SymGnuAsm |
               InterfaceSym        : Interface        : SymInterface |
               TupleSym            : Tuple            : SymTuple |
               DummySym            : Dummy            : SymDummy

               END
            END ;

   CallFrame = RECORD
                  Main  : CARDINAL ;  (* Main scope for insertions        *)
                  Search: CARDINAL ;  (* Search scope for symbol searches *)
                  Start : CARDINAL ;  (* ScopePtr value before StartScope *)
                                      (* was called.                      *)
               END ;


VAR
   Symbols       : ARRAY [1..MaxSymbols] OF Symbol ;
   ScopeCallFrame: ARRAY [1..MaxScopes] OF CallFrame ;
   FreeSymbol    : CARDINAL ;    (* The Head of the free symbol list   *)
   DefModuleTree : SymbolTree ;
   ModuleTree    : SymbolTree ;  (* Tree of all modules ever used.     *)
   ConstLitStringTree
                 : SymbolTree ;  (* String Literal Constants only need *)
                                 (* to be declared once.               *)
   ConstLitTree  : SymbolTree ;  (* Numerical Literal Constants only   *)
                                 (* need to be declared once.          *)
   CurrentModule : CARDINAL ;    (* Index into symbols determining the *)
                                 (* current module being compiled.     *)
                                 (* This maybe an inner module.        *)
   MainModule    : CARDINAL ;    (* Index into symbols determining the *)
                                 (* module the user requested to       *)
                                 (* compile.                           *)
   FileModule    : CARDINAL ;    (* Index into symbols determining     *)
                                 (* which module (file) is being       *)
                                 (* compiled. (Maybe an import def)    *)
   ScopePtr      : CARDINAL ;    (* An index to the ScopeCallFrame.    *)
                                 (* ScopePtr determines the top of the *)
                                 (* ScopeCallFrame.                    *)
   BaseScopePtr  : CARDINAL ;    (* An index to the ScopeCallFrame of  *)
                                 (* the top of BaseModule. BaseModule  *)
                                 (* is always left at the bottom of    *)
                                 (* stack since it is used so          *)
                                 (* frequently. When the BaseModule    *)
                                 (* needs to be searched the ScopePtr  *)
                                 (* is temporarily altered to          *)
                                 (* BaseScopePtr and GetScopeSym is    *)
                                 (* called.                            *)
   BaseModule    : CARDINAL ;    (* Index to the symbol table of the   *)
                                 (* Base pseudo modeule declaration.   *)
   TemporaryNo   : CARDINAL ;    (* The next temporary number.         *)
   CurrentError  : Error ;       (* Current error chain.               *)
   AddressTypes  : List ;        (* A list of type symbols which must  *)
                                 (* be declared as ADDRESS or pointer  *)
   FreeFVarientList,             (* Lists used to maintain GC of field *)
   UsedFVarientList: List ;      (* varients.                          *)
   UnresolvedConstructorType: List ;  (* all constructors whose type   *)
                                 (* is not yet known.                  *)
   AnonymousName     : CARDINAL ;(* anonymous type name unique id      *)


(* %%%FORWARD%%%
PROCEDURE stop ; FORWARD ;
PROCEDURE ResolveImport (o: WORD) ; FORWARD ;
PROCEDURE GetExportUndeclared (ModSym: CARDINAL; name: Name) : CARDINAL ; FORWARD ;
PROCEDURE IsHiddenType (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE CheckForSymbols (Tree: SymbolTree; a: ARRAY OF CHAR) ; FORWARD ;
PROCEDURE PushConstString (Sym: CARDINAL) ; FORWARD ;
PROCEDURE AddParameter (Sym: CARDINAL; ParSym: CARDINAL) ; FORWARD ;
PROCEDURE AddProcedureToList (Mod, Proc: CARDINAL) ; FORWARD ;
PROCEDURE AddSymToModuleScope (ModSym: CARDINAL; Sym: CARDINAL) ; FORWARD ;
PROCEDURE AddSymToScope (Sym: CARDINAL; name: Name) ; FORWARD ;
PROCEDURE AddSymToUnknownTree (ScopeId: INTEGER; name: Name; Sym: CARDINAL) ; FORWARD ;
PROCEDURE AddVarToList (Sym: CARDINAL) ; FORWARD ;
PROCEDURE AddVarToScopeList (scope, sym: CARDINAL) ; FORWARD ;
PROCEDURE CheckEnumerationInList (l: List; Sym: CARDINAL) ; FORWARD ;
PROCEDURE CheckForEnumerationInOuterModule (Sym: CARDINAL;
                                            OuterModule: CARDINAL) ; FORWARD ;
PROCEDURE CheckForExportedDeclaration (Sym: CARDINAL) ; FORWARD ;
PROCEDURE CheckForHiddenType (TypeName: Name) : CARDINAL ; FORWARD ;
PROCEDURE CheckForUnknowns (name: Name; Tree: SymbolTree;
                            a: ARRAY OF CHAR) ; FORWARD ;
PROCEDURE CheckIfEnumerationExported (Sym: CARDINAL; ScopeId: CARDINAL) ; FORWARD ;
PROCEDURE CheckLegal (Sym: CARDINAL) ; FORWARD ;
PROCEDURE CheckScopeForSym (ScopeSym: CARDINAL; name: Name) : CARDINAL ; FORWARD ;
PROCEDURE DeclareSym (name: Name) : CARDINAL ; FORWARD ;
PROCEDURE DisplayScopes ; FORWARD ;
PROCEDURE DisposeSym (Sym: CARDINAL) ; FORWARD ;
PROCEDURE ExamineUnresolvedTree (ScopeSym: CARDINAL; name: Name) : CARDINAL ; FORWARD ;
PROCEDURE FetchUnknownFromDefImp (ModSym: CARDINAL;
                                  SymName: Name) : CARDINAL ; FORWARD ;
PROCEDURE FetchUnknownFromModule (ModSym: CARDINAL;
                                  SymName: Name) : CARDINAL ; FORWARD ;
PROCEDURE FetchUnknownSym (name: Name) : CARDINAL ; FORWARD ;
PROCEDURE GetConstLitType (Sym: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE GetCurrentModule () : CARDINAL ; FORWARD ;
PROCEDURE GetModuleScopeId (Id: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE GetRecord (Sym: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE GetScopeSym (name: Name) : CARDINAL ; FORWARD ;
PROCEDURE GetSymFromUnknownTree (name: Name) : CARDINAL ; FORWARD ;
PROCEDURE Init ; FORWARD ;
PROCEDURE InitSymTable ; FORWARD ;
PROCEDURE GetVisibleSym (name: Name) : CARDINAL ; FORWARD ;
PROCEDURE IsAlreadyDeclaredSym (name: Name) : BOOLEAN ; FORWARD ;
PROCEDURE IsNthParamVar (Head: List; n: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE NewSym (VAR Sym: CARDINAL) ; FORWARD ;
PROCEDURE PlaceEnumerationListOntoScope (l: List) ; FORWARD ;
PROCEDURE PlaceMajorScopesEnumerationListOntoStack (Sym: CARDINAL) ; FORWARD ;
PROCEDURE PushParamSize (Sym: CARDINAL; ParamNo: CARDINAL) ; FORWARD ;
PROCEDURE PushParamSize (Sym: CARDINAL; ParamNo: CARDINAL) ;   FORWARD ;
PROCEDURE PushSumOfLocalVarSize (Sym: CARDINAL) ; FORWARD ;
PROCEDURE PutExportUndeclared (ModSym: CARDINAL; Sym: CARDINAL) ; FORWARD ;
PROCEDURE PutHiddenTypeDeclared ; FORWARD ;
PROCEDURE PutVarTypeAndSize (Sym: CARDINAL; VarType: CARDINAL; TypeSize: CARDINAL) ; FORWARD ;
PROCEDURE RemoveExportUnImplemented (ModSym: CARDINAL; Sym: CARDINAL) ; FORWARD ;
PROCEDURE RemoveExportUndeclared (ModSym: CARDINAL; Sym: CARDINAL) ; FORWARD ;
PROCEDURE RequestFromDefinition (ModSym: CARDINAL; SymName: Name) : CARDINAL ; FORWARD ;
PROCEDURE RequestFromModule (ModSym: CARDINAL; SymName: Name) : CARDINAL ; FORWARD ;
PROCEDURE SubSymFromUnknownTree (name: Name) ; FORWARD ;
PROCEDURE RemoveFromUnresolvedTree (ScopeSym: CARDINAL; name: Name) : BOOLEAN ; FORWARD ;
PROCEDURE TransparentScope (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE UnImplementedSymbolError (Sym: WORD) ; FORWARD ;
PROCEDURE UndeclaredSymbolError (Sym: WORD) ; FORWARD ;
PROCEDURE UnknownSymbolError (Sym: WORD) ; FORWARD ;
PROCEDURE GetWhereImported (Sym: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE RemoveFromExportRequest (Sym: CARDINAL) ; FORWARD ;
PROCEDURE PutUnbounded (SimpleType: CARDINAL; Sym: CARDINAL) ; FORWARD ;
PROCEDURE FillInUnboundedFields (sym: CARDINAL; SimpleType: CARDINAL) ; FORWARD ;
PROCEDURE FillInUnknownFields (sym: CARDINAL; SymName: Name) ; FORWARD ;
PROCEDURE IsConstructorResolved (sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE GetFromIndex (i: Indexing.Index; n: CARDINAL) : CARDINAL ; FORWARD ;
   %%%FORWARD%%% *)


(*
   CheckAnonymous - checks to see whether the name is NulName and if so
                    it creates a unique anonymous name.
*)

PROCEDURE CheckAnonymous (name: Name) : Name ;
BEGIN
   IF name=NulName
   THEN
      INC(AnonymousName) ;
      name := makekey(string(Mark(Sprintf1(Mark(InitString('$$%d')), AnonymousName))))
   END ;
   RETURN( name )
END CheckAnonymous ;


(*
   IsNameAnonymous - returns TRUE if the symbol, sym, has an anonymous name
                     or no name.
*)

PROCEDURE IsNameAnonymous (sym: CARDINAL) : BOOLEAN ;
VAR
   a: ARRAY [0..1] OF CHAR ;
   n: Name ;
BEGIN
   n := GetSymName(sym) ;
   IF n=NulName
   THEN
      RETURN( TRUE )
   ELSE
      GetKey(n, a) ;
      RETURN( StrEqual(a, '$$') )
   END
END IsNameAnonymous ;


(*
   InitWhereDeclared - sets the Declared and FirstUsed fields of record, at.
*)

PROCEDURE InitWhereDeclared (VAR at: Where) ;
BEGIN
   WITH at DO
      Declared := GetTokenNo() ;
      FirstUsed := Declared   (* we assign this field to something legal *)
   END
END InitWhereDeclared ;


(*
   InitWhereFirstUsed - sets the FirstUsed field of record, at.
*)

PROCEDURE InitWhereFirstUsed (VAR at: Where) ;
BEGIN
   WITH at DO
      FirstUsed := GetTokenNo()
   END
END InitWhereFirstUsed ;


(*
   FinalSymbol - returns the highest number symbol used.
*)

PROCEDURE FinalSymbol () : CARDINAL ;
BEGIN
   RETURN( FreeSymbol-1 )
END FinalSymbol ;


(*
   NewSym - Sets Sym to a new symbol index.
*)

PROCEDURE NewSym (VAR Sym: CARDINAL) ;
BEGIN
   IF FreeSymbol=MaxSymbols
   THEN
      InternalError('increase MaxSymbols', __FILE__, __LINE__)
   ELSE
      IF FreeSymbol=2766
      THEN
         stop
      END ;
      Sym := FreeSymbol ;
      WITH Symbols[Sym] DO
         SymbolType := DummySym
      END ;
      INC(FreeSymbol)
   END
END NewSym ;


(*
   DisposeSym - Places Sym onto the FreeSymbol list.
*)

PROCEDURE DisposeSym (Sym: CARDINAL) ;
BEGIN
   InternalError('DisposeSym - not really working yet??? check with Evaluate',
                __FILE__, __LINE__) ;
   HALT ;
   WITH Symbols[Sym] DO
      SymbolType := DummySym ;
      Dummy.NextFree := FreeSymbol
   END ;
   FreeSymbol := Sym
END DisposeSym ;


(*
   IsPartialUnbounded - returns TRUE if, sym, is a partially unbounded symbol.
*)

PROCEDURE IsPartialUnbounded (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF sym>0
   THEN
      WITH Symbols[sym] DO
         CASE SymbolType OF
         
         PartialUnboundedSym:  RETURN( TRUE )
      
         ELSE
            RETURN( FALSE )
         END
      END
   ELSE
      RETURN( FALSE )
   END
END IsPartialUnbounded ;


(*
   PutPartialUnbounded -
*)

PROCEDURE PutPartialUnbounded (sym: CARDINAL; type: CARDINAL) ;
BEGIN
   IF IsDummy(sym)
   THEN
      Symbols[sym].SymbolType := PartialUnboundedSym
   END ;
   WITH Symbols[sym] DO
      CASE SymbolType OF

      PartialUnboundedSym:  PartialUnbounded.Type := type

      ELSE
         InternalError('not expecting this type', __FILE__, __LINE__)
      END
   END
END PutPartialUnbounded ;


(*
   AlreadyDeclaredError - generate an error message, a, and two areas of code showing
                          the places where the symbols were declared.
*)

PROCEDURE AlreadyDeclaredError (s: String; name: Name; OtherOccurance: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   IF (OtherOccurance=0) OR (OtherOccurance=GetTokenNo())
   THEN
      e := NewError(GetTokenNo()) ;
      ErrorString(e, s)
   ELSE
      e := NewError(GetTokenNo()) ;
      ErrorString(e, s) ;
      e := ChainError(OtherOccurance, e) ;
      ErrorFormat1(e, 'and symbol (%a) is also declared here', name)
   END
END AlreadyDeclaredError ;


(*
   AlreadyImportedError - generate an error message, a, and two areas of code showing
                          the places where the symbols was imported and also declared.
*)

PROCEDURE AlreadyImportedError (s: String; name: Name; OtherOccurance: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   IF (OtherOccurance=0) OR (OtherOccurance=GetTokenNo())
   THEN
      e := NewError(GetTokenNo()) ;
      ErrorString(e, s)
   ELSE
      e := NewError(GetTokenNo()) ;
      ErrorString(e, s) ;
      e := ChainError(OtherOccurance, e) ;
      ErrorFormat1(e, 'and symbol (%a) was also seen here', name)
   END
END AlreadyImportedError ;


(*
   MakeError - creates an error node, it does assume that the caller
               will issue an appropriate error message as this symbol
               will be removed from the generic error message trees.
               It will be removed from ExportUndeclared and Unknown trees.
*)

PROCEDURE MakeError (name: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   (* if Sym is present on the unknown tree then remove it *)
   Sym := FetchUnknownSym(name) ;
   IF Sym=NulSym
   THEN
      NewSym(Sym)
   ELSE
      (*
         remove symbol from this tree as we have already generated
         a meaningful error message
      *)
      RemoveExportUndeclared(GetCurrentModuleScope(), Sym)
   END ;
   WITH Symbols[Sym] DO
      SymbolType := ErrorSym ;
      Error.name := name ;
      InitWhereDeclared(Error.At) ;
      InitWhereFirstUsed(Error.At)
   END ;
   RETURN( Sym )
END MakeError ;


(*
   IsError - returns TRUE if the symbol is an error symbol.
*)

PROCEDURE IsError (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN( Symbols[Sym].SymbolType=ErrorSym )
END IsError ;


(*
   MakeObject - creates an object node.
*)

PROCEDURE MakeObject (name: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   NewSym(Sym) ;
   WITH Symbols[Sym] DO
      SymbolType := ObjectSym ;
      Object.name := name ;
      InitWhereDeclared(Object.At) ;
      InitWhereFirstUsed(Object.At)
   END ;
   RETURN( Sym )
END MakeObject ;


(*
   IsTuple - returns TRUE if the symbol is a tuple symbol.
*)

PROCEDURE IsTuple (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN( Symbols[Sym].SymbolType=TupleSym )
END IsTuple ;


(*
   IsObject - returns TRUE if the symbol is an object symbol.
*)

PROCEDURE IsObject (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN( Symbols[Sym].SymbolType=ObjectSym )
END IsObject ;


PROCEDURE stop ; BEGIN END stop ;

(*
   DeclareSym - returns a symbol which was either in the unknown tree or
                a New symbol, since name is about to be declared.
*)

PROCEDURE DeclareSym (name: Name) : CARDINAL ;
VAR
   s  : String ;
   Sym: CARDINAL ;
BEGIN
   IF name=NulName
   THEN
      NewSym(Sym)
   ELSIF IsAlreadyDeclaredSym(name)
   THEN
      Sym := GetSym(name) ;
      IF IsImported(GetCurrentModuleScope(), Sym)
      THEN
         s := Mark(InitStringCharStar(KeyToCharStar(name))) ;
         AlreadyImportedError(Sprintf1(Mark(InitString('symbol (%s) is already present in this scope, check both definition and implementation modules, use a different name or remove the import')),
                                       s), name, GetWhereImported(Sym)) ;
      ELSE
         s := Mark(InitStringCharStar(KeyToCharStar(name))) ;
         AlreadyDeclaredError(Sprintf1(Mark(InitString('symbol (%s) is already declared in this scope, use a different name or remove the declaration')), s),
                              name,
                              GetDeclared(GetVisibleSym(name)))
      END ;
      Sym := MakeError(name)
   ELSE
      Sym := FetchUnknownSym(name) ;
      IF Sym=NulSym
      THEN
         NewSym(Sym)
      END ;
      CheckForExportedDeclaration(Sym)
   END ;
   RETURN( Sym )
END DeclareSym ;


(*
   InitSymTable - initializes the symbol table.
*)

PROCEDURE InitSymTable ;
BEGIN
   FreeSymbol := 1
END InitSymTable ;


(*
   Init - Initializes the data structures and variables in this module.
          Initialize the trees.
*)

PROCEDURE Init ;
BEGIN
   AnonymousName := 0 ;
   CurrentError := NIL ;
   InitSymTable ;
   InitTree(ConstLitTree) ;
   InitTree(ConstLitStringTree) ;
   InitTree(DefModuleTree) ;
   InitTree(ModuleTree) ;
   ScopePtr := 1 ;
   WITH ScopeCallFrame[ScopePtr] DO
      Main := NulSym ;
      Search := NulSym
   END ;
   CurrentModule     := NulSym ;
   MainModule        := NulSym ;
   FileModule        := NulSym ;
   TemporaryNo       := 0 ;
   InitList(FreeFVarientList) ;             (* Lists used to maintain GC of field *)
   InitList(UsedFVarientList) ;             (* varients.                          *)
   InitList(UnresolvedConstructorType) ;

   InitBase(BaseModule) ;
   StartScope(BaseModule) ;   (* BaseModule scope placed at the bottom of the stack *)
   BaseScopePtr := ScopePtr ; (* BaseScopePtr points to the top of the BaseModule scope *)
   InitList(AddressTypes)
END Init ;


(*
   FromModuleGetSym - attempts to find a symbol of name, n, in the
                      module, mod, scope.
*)

PROCEDURE FromModuleGetSym (n: Name; mod: CARDINAL) : CARDINAL ;
VAR
   n1         : Name ;
   sym        : CARDINAL ;
   OldScopePtr: CARDINAL ;
BEGIN
   OldScopePtr := ScopePtr ;
   StartScope(mod) ;
   sym := RequestSym(n) ;
   EndScope ;
   IF sym=NulSym
   THEN
      n1 := GetSymName(mod) ;
      WriteFormat2('cannot find procedure %a in module, %a',
                   n, n1)
   END ;
   ScopePtr := OldScopePtr ;
   RETURN( sym )
END FromModuleGetSym ;


(*
   AddSymToUnknown - 
*)

PROCEDURE AddSymToUnknown (scope: CARDINAL; name: Name; Sym: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   IF DebugUnknowns
   THEN
      n := GetSymName(scope) ;
      printf3('adding unknown %a (%d) to scope %a\n', name, Sym, n)
   END ;

   (* Add symbol to unknown tree *)
   WITH Symbols[scope] DO
      CASE SymbolType OF

      DefImpSym   : PutSymKey(DefImp.Unresolved, name, Sym) |
      ModuleSym   : PutSymKey(Module.Unresolved, name, Sym) |
      ProcedureSym: PutSymKey(Procedure.Unresolved, name, Sym)

      ELSE
         InternalError('expecting DefImp, Module or Procedure symbol', __FILE__, __LINE__)
      END
   END
END AddSymToUnknown ;


(*
   AddSymToUnknownTree - adds a symbol with name, name, and Sym to the
                         unknown tree.
*)

PROCEDURE AddSymToUnknownTree (ScopeId: INTEGER; name: Name; Sym: CARDINAL) ;
VAR
   ScopeSym: CARDINAL ;
BEGIN
   IF ScopeId>0
   THEN
      (* choose to place the unknown symbol in the first module scope scope
         outside the current scope *)
      REPEAT
         ScopeSym := ScopeCallFrame[ScopeId].Main ;
         IF (ScopeSym>0) AND (IsDefImp(ScopeSym) OR IsModule(ScopeSym))
         THEN
            AddSymToUnknown(ScopeSym, name, Sym) ;
            RETURN
         END ;
         DEC(ScopeId)
      UNTIL ScopeId=0
   END ;
   AddSymToUnknown(CurrentModule, name, Sym)
END AddSymToUnknownTree ;


(*
   SubSymFromUnknownTree - removes a symbol with name, name, from the
                           unknown tree.
*)

PROCEDURE SubSymFromUnknownTree (name: Name) ;
VAR
   ScopeSym,
   ScopeId : CARDINAL ;
BEGIN
   IF ScopePtr>0
   THEN
      ScopeId := ScopePtr ;
      REPEAT
         ScopeSym := ScopeCallFrame[ScopeId].Search ;
         IF IsModule(ScopeSym) OR IsDefImp(ScopeSym) OR IsProcedure(ScopeSym)
         THEN
            IF RemoveFromUnresolvedTree(ScopeSym, name)
            THEN
               RETURN
            END
         END ;
         DEC(ScopeId) ;
      UNTIL (ScopeId>0) AND (IsModule(ScopeSym) OR IsDefImp(ScopeSym))
   END ;
   IF RemoveFromUnresolvedTree(CurrentModule, name)
   THEN
   END
END SubSymFromUnknownTree ;


(*
   GetSymFromUnknownTree - returns a symbol with name, name, from the
                           unknown tree.
                           If no symbol with name is found then NulSym
                           is returned.
*)

PROCEDURE GetSymFromUnknownTree (name: Name) : CARDINAL ;
VAR
   ScopeSym,
   ScopeId ,
   Sym     : CARDINAL ;
BEGIN
   IF ScopePtr>0
   THEN
      ScopeId := ScopePtr ;
      REPEAT
         ScopeSym := ScopeCallFrame[ScopeId].Search ;
         IF IsModule(ScopeSym) OR IsDefImp(ScopeSym) OR IsProcedure(ScopeSym)
         THEN
            Sym := ExamineUnresolvedTree(ScopeSym, name) ;
            IF Sym#NulSym
            THEN
               RETURN( Sym )
            END
         END ;
         DEC(ScopeId) ;
      UNTIL (ScopeId>0) AND (IsModule(ScopeSym) OR IsDefImp(ScopeSym))
   END ;
   (* Get symbol from unknown tree *)
   RETURN( ExamineUnresolvedTree(CurrentModule, name) )
END GetSymFromUnknownTree ;


(*
   ExamineUnresolvedTree - returns a symbol with name, name, from the
                           unresolved tree of module, ModSym.
                           If no symbol with name is found then NulSym
                           is returned.
*)

PROCEDURE ExamineUnresolvedTree (ScopeSym: CARDINAL; name: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   (* Get symbol from unknown tree *)
   WITH Symbols[ScopeSym] DO
      CASE SymbolType OF

      DefImpSym   : Sym := GetSymKey(DefImp.Unresolved, name) |
      ModuleSym   : Sym := GetSymKey(Module.Unresolved, name) |
      ProcedureSym: Sym := GetSymKey(Procedure.Unresolved, name)

      ELSE
         InternalError('expecting DefImp, Module or Procedure symbol', __FILE__, __LINE__)
      END
   END ;
   RETURN( Sym )
END ExamineUnresolvedTree ;


(*
   TryMoveUndeclaredSymToInnerModule - attempts to move a symbol of
                                       name, name, which is
                                       currently undefined in the
                                       outer scope to the inner scope.
                                       If successful then the symbol is
                                       returned otherwise NulSym is
                                       returned.
*)

PROCEDURE TryMoveUndeclaredSymToInnerModule (OuterScope,
                                             InnerScope: CARDINAL;
                                             name: Name) : CARDINAL ;
VAR
   sym: CARDINAL ;
BEGIN
   (* assume this should not be called if OuterScope was a procedure
      as this case is handled by the caller (P1SymBuild)
   *)
   Assert(IsModule(OuterScope) OR IsDefImp(OuterScope)) ;
   sym := GetExportUndeclared(OuterScope, name) ;
   IF sym#NulSym
   THEN
      Assert(IsUnknown(sym)) ;
      RemoveExportUndeclared(OuterScope, sym) ;
      AddSymToModuleScope(OuterScope, sym) ;
      AddVarToScopeList(OuterScope, sym) ;
      WITH Symbols[OuterScope] DO
         CASE SymbolType OF

         DefImpSym: IF GetSymKey(DefImp.Unresolved, name)=sym
                    THEN
                       DelSymKey(DefImp.Unresolved, name)
                    END |
         ModuleSym: IF GetSymKey(Module.Unresolved, name)=sym
                    THEN
                       DelSymKey(Module.Unresolved, name)
                    END

         ELSE
            InternalError('expecting DefImp, Module symbol', __FILE__, __LINE__)
         END
      END ;
      AddSymToUnknown(InnerScope, name, sym) ;
      PutExportUndeclared(InnerScope, sym)
   END ;
   RETURN( sym )
END TryMoveUndeclaredSymToInnerModule ;


(*
   RemoveFromUnresolvedTree - removes a symbol with name, name, from the
                              unresolved tree of symbol, ScopeSym.
*)

PROCEDURE RemoveFromUnresolvedTree (ScopeSym: CARDINAL; name: Name) : BOOLEAN ;
BEGIN
   (* Get symbol from unknown tree *)
   WITH Symbols[ScopeSym] DO
      CASE SymbolType OF

      DefImpSym   : IF GetSymKey(DefImp.Unresolved, name)#NulKey
                    THEN
                       DelSymKey(DefImp.Unresolved, name) ;
                       RETURN( TRUE )
                    END |
      ModuleSym   : IF GetSymKey(Module.Unresolved, name)#NulKey
                    THEN
                       DelSymKey(Module.Unresolved, name) ;
                       RETURN( TRUE )
                    END |
      ProcedureSym: IF GetSymKey(Procedure.Unresolved, name)#NulKey
                    THEN
                       DelSymKey(Procedure.Unresolved, name) ;
                       RETURN( TRUE )
                    END

      ELSE
         InternalError('expecting DefImp, Module or Procedure symbol', __FILE__, __LINE__)
      END
   END ;
   RETURN( FALSE )
END RemoveFromUnresolvedTree ;


(*
   FetchUnknownSym - returns a symbol from the unknown tree if one is
                     available. It also updates the unknown tree.
*)

PROCEDURE FetchUnknownSym (name: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := GetSymFromUnknownTree(name) ;
   IF Sym#NulSym
   THEN
      SubSymFromUnknownTree(name)
   END ;
   RETURN( Sym )
END FetchUnknownSym ;


(*
   TransparentScope - returns true is the scope symbol Sym is allowed
                      to look to an outer level for a symbol.
                      ie is the symbol allowed to look to the parent
                      scope for a symbol.
*)

PROCEDURE TransparentScope (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      RETURN( (SymbolType#DefImpSym) AND (SymbolType#ModuleSym) )
   END
END TransparentScope ;


(*
   AddSymToModuleScope - adds a symbol, Sym, to the scope of the module
                         ModSym.
*)

PROCEDURE AddSymToModuleScope (ModSym: CARDINAL; Sym: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      DefImpSym   : IF GetSymKey(DefImp.LocalSymbols, GetSymName(Sym))=NulKey
                    THEN
                       PutSymKey(DefImp.LocalSymbols, GetSymName(Sym), Sym)
                    ELSE
                       n := GetSymName(Sym) ;
                       WriteFormat1('IMPORT name clash with symbol (%a) symbol already declared ', n)
                    END |
      ModuleSym   : IF GetSymKey(Module.LocalSymbols, GetSymName(Sym))=NulKey
                    THEN
                       PutSymKey(Module.LocalSymbols, GetSymName(Sym), Sym)
                    ELSE
                       n := GetSymName(Sym) ;
                       WriteFormat1('IMPORT name clash with symbol (%a) symbol already declared ', n)
                    END |
      ProcedureSym: IF GetSymKey(Procedure.LocalSymbols, GetSymName(Sym))=NulKey
                    THEN
                       PutSymKey(Procedure.LocalSymbols, GetSymName(Sym), Sym)
                    ELSE
                       n := GetSymName(Sym) ;
                       WriteFormat1('IMPORT name clash with symbol (%a) symbol already declared ', n)
                    END


      ELSE
         InternalError('expecting Module or DefImp symbol', __FILE__, __LINE__)
      END
   END
END AddSymToModuleScope ;


(*
   GetCurrentModuleScope - returns the module symbol which forms the
                           current (possibly inner most) module.
*)

PROCEDURE GetCurrentModuleScope () : CARDINAL ;
VAR
   i  : CARDINAL ;
BEGIN
   i := ScopePtr ;
   WHILE (NOT IsModule(ScopeCallFrame[i].Search)) AND
         (NOT IsDefImp(ScopeCallFrame[i].Search)) DO
      Assert(i>0) ;
      DEC(i)
   END ;
   RETURN( ScopeCallFrame[i].Search )
END GetCurrentModuleScope ;


(*
   GetLastModuleScope - returns the last module scope encountered,
                        the module scope before the Current Module Scope.
*)

PROCEDURE GetLastModuleScope () : CARDINAL ;
VAR
   i  : CARDINAL ;
BEGIN
   i := ScopePtr ;
   WHILE (NOT IsModule(ScopeCallFrame[i].Search)) AND
         (NOT IsDefImp(ScopeCallFrame[i].Search)) DO
      Assert(i>0) ;
      DEC(i)
   END ;
   (* Found module at position, i. *)
   DEC(i) ;  (* Move to an outer level module scope *)
   WHILE (NOT IsModule(ScopeCallFrame[i].Search)) AND
         (NOT IsDefImp(ScopeCallFrame[i].Search)) DO
      Assert(i>0) ;
      DEC(i)
   END ;
   (* Found module at position, i. *)
   RETURN( ScopeCallFrame[i].Search )
END GetLastModuleScope ;


(*
   GetLastModuleOrProcedureScope - returns the last module or procedure scope encountered,
                                   the scope before the current module scope.
*)

PROCEDURE GetLastModuleOrProcedureScope () : CARDINAL ;
VAR
   i  : CARDINAL ;
BEGIN
   (* find current inner module *)
   i := ScopePtr ;
   WHILE (NOT IsModule(ScopeCallFrame[i].Search)) AND
         (NOT IsDefImp(ScopeCallFrame[i].Search)) DO
      Assert(i>0) ;
      DEC(i)
   END ;
   (* found module at position, i. *)
   DEC(i) ;  (* Move to an outer level module or procedure scope *)
   WHILE (NOT IsModule(ScopeCallFrame[i].Search)) AND
         (NOT IsDefImp(ScopeCallFrame[i].Search)) AND
         (NOT IsProcedure(ScopeCallFrame[i].Search)) DO
      Assert(i>0) ;
      DEC(i)
   END ;
   (* Found module at position, i. *)
   RETURN( ScopeCallFrame[i].Search )
END GetLastModuleOrProcedureScope ;


(*
   AddSymToScope - adds a symbol Sym with name name to
                   the current scope symbol tree.
*)

PROCEDURE AddSymToScope (Sym: CARDINAL; name: Name) ;
VAR
   ScopeId: CARDINAL ;
BEGIN
   ScopeId := ScopeCallFrame[ScopePtr].Main ;
   (*
      WriteString('Adding ') ; WriteKey(name) ; WriteString(' :') ; WriteCard(Sym, 4) ; WriteString(' to scope: ') ;
      WriteKey(GetSymName(ScopeId)) ; WriteLn ;
   *)
   WITH Symbols[ScopeId] DO
      CASE SymbolType OF

      DefImpSym   : IF name#NulName
                    THEN
                       PutSymKey(DefImp.LocalSymbols, name, Sym)
                    END ;
                    IF IsEnumeration(Sym)
                    THEN
                       CheckEnumerationInList(DefImp.EnumerationScopeList, Sym)
                    END |
      ModuleSym   : IF name#NulName
                    THEN
                       PutSymKey(Module.LocalSymbols, name, Sym)
                    END ;
                    IF IsEnumeration(Sym)
                    THEN
                       CheckEnumerationInList(Module.EnumerationScopeList, Sym)
                    END |
      ProcedureSym: IF name#NulName
                    THEN
                       PutSymKey(Procedure.LocalSymbols, name, Sym)
                    END ;
                    IF IsEnumeration(Sym)
                    THEN
                       CheckEnumerationInList(Procedure.EnumerationScopeList, Sym)
                    END

      ELSE
         InternalError('should never get here', __FILE__, __LINE__)
      END
   END
END AddSymToScope ;


(*
   GetCurrentScope - returns the symbol who is responsible for the current
                     scope. Note that it ignore pseudo scopes.
*)

PROCEDURE GetCurrentScope () : CARDINAL ;
BEGIN
   RETURN( ScopeCallFrame[ScopePtr].Main )
END GetCurrentScope ;


(*
   StartScope - starts a block scope at Sym. Transparent determines
                whether the search for a symbol will look at the
                previous ScopeCallFrame if Sym does not contain the
                symbol that GetSym is searching.

                WITH statements are partially implemented by calling
                StartScope. Therefore we must retain the old Main from
                the previous ScopePtr when a record is added to the scope
                stack. (Main contains the symbol where all identifiers
                should be added.)
*)

PROCEDURE StartScope (Sym: CARDINAL) ;
BEGIN
   Sym := SkipType(Sym) ;
   IF ScopePtr=MaxScopes
   THEN
      InternalError('too many scopes - increase MaxScopes', __FILE__, __LINE__)
   ELSE
(*
      WriteString('New scope is: ') ; WriteKey(GetSymName(Sym)) ; WriteLn ;
*)
      INC(ScopePtr) ;
      WITH ScopeCallFrame[ScopePtr] DO
         Start := ScopePtr-1 ;  (* Previous ScopePtr value before StartScope *)
         Search := Sym ;

         (* If Sym is a record then maintain the old Main scope for adding   *)
         (* new symbols to ie temporary variables.                           *)
         IF IsRecord(Sym)
         THEN
            Main := ScopeCallFrame[ScopePtr-1].Main
         ELSE
            Main := Sym ;
            PlaceMajorScopesEnumerationListOntoStack(Sym)
         END
      END
   END
   (* ; DisplayScopes *)
END StartScope ;


(*
   PlaceMajorScopesEnumerationListOntoStack - places the DefImp, Module and
                                              Procedure symbols enumeration
                                              list onto the scope stack.
*)

PROCEDURE PlaceMajorScopesEnumerationListOntoStack (Sym: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      DefImpSym   : PlaceEnumerationListOntoScope(DefImp.EnumerationScopeList) |
      ModuleSym   : PlaceEnumerationListOntoScope(Module.EnumerationScopeList) |
      ProcedureSym: PlaceEnumerationListOntoScope(Procedure.EnumerationScopeList)

      ELSE
         InternalError('expecting - DefImp, Module or Procedure symbol', __FILE__, __LINE__)
      END
   END
END PlaceMajorScopesEnumerationListOntoStack ;


(*
   PlaceEnumerationListOntoScope - places an enumeration list, l, onto the
                                   scope stack. This list will automatically
                                   removed via one call to EndScope which
                                   matches the StartScope by which this
                                   procedure is invoked.
*)

PROCEDURE PlaceEnumerationListOntoScope (l: List) ;
VAR
   i, n: CARDINAL ;
BEGIN
   n := NoOfItemsInList(l) ;
   i := 1 ;
   WHILE i<=n DO
      PseudoScope(GetItemFromList(l, i)) ;
      INC(i)
   END
END PlaceEnumerationListOntoScope ;


(*
   EndScope - ends a block scope started by StartScope. The current
              head of the symbol scope reverts back to the symbol
              which was the Head of the symbol scope before the
              last StartScope was called.
*)

PROCEDURE EndScope ;
BEGIN
(*
   ; WriteString('EndScope - ending scope: ') ;
   ; WriteKey(GetSymName(ScopeCallFrame[ScopePtr].Search)) ; WriteLn ;
*)
   ScopePtr := ScopeCallFrame[ScopePtr].Start
   (* ; DisplayScopes *)
END EndScope ;


(*
   PseudoScope - starts a pseudo scope at Sym.
                 We always connect parent up to the last scope,
                 to determine the transparancy of a scope we call
                 TransparentScope.

                 A Pseudo scope has no end block,
                 but is terminated when the next EndScope is used.
                 The function of the pseudo scope is to provide an
                 automatic mechanism to solve enumeration types.
                 A declared enumeration type is a Pseudo scope and
                 identifiers used with the name of an enumeration
                 type field will find the enumeration symbol by
                 the scoping algorithm.
*)

PROCEDURE PseudoScope (Sym: CARDINAL) ;
BEGIN
   IF IsEnumeration(Sym)
   THEN
      IF ScopePtr<MaxScopes
      THEN
         INC(ScopePtr) ;
         WITH ScopeCallFrame[ScopePtr] DO
            Main := ScopeCallFrame[ScopePtr-1].Main ;
            Start := ScopeCallFrame[ScopePtr-1].Start ;
            Search := Sym
         END
      ELSE
         InternalError('increase MaxScopes', __FILE__, __LINE__)
      END
   ELSE
      InternalError('expecting EnumerationSym', __FILE__, __LINE__)
   END
END PseudoScope ;


(*
   IsDeclaredIn - returns TRUE if a symbol was declared in, scope.
*)

PROCEDURE IsDeclaredIn (scope, sym: CARDINAL) : BOOLEAN ;
VAR
   s: CARDINAL ;
BEGIN
   s := GetScope(sym) ;
   WHILE s#scope DO
      IF (s=NulSym) OR IsProcedure(s) OR IsModule(s) OR IsDefImp(s)
      THEN
         RETURN( FALSE )
      ELSE
         s := GetScope(s)
      END
   END ;
   RETURN( TRUE )
END IsDeclaredIn ;


(*
   MakeGnuAsm - create a GnuAsm symbol.
*)

PROCEDURE MakeGnuAsm () : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   NewSym(Sym) ;
   WITH Symbols[Sym] DO
      SymbolType := GnuAsmSym ;
      WITH GnuAsm DO
         String   := NulSym ;
         InitWhereDeclared(At) ;
         Inputs   := NulSym ;
         Outputs  := NulSym ;
         Trashed  := NulSym ;
         Volatile := FALSE
      END
   END ;
   RETURN( Sym )
END MakeGnuAsm ;


(*
   PutGnuAsm - places the instruction textual name into the GnuAsm symbol.
*)

PROCEDURE PutGnuAsm (sym: CARDINAL; string: CARDINAL) ;
BEGIN
   Assert(IsConstString(string)) ;
   WITH Symbols[sym] DO
      CASE SymbolType OF

      GnuAsmSym: GnuAsm.String := string

      ELSE
         InternalError('expecting PutGnuAsm symbol', __FILE__, __LINE__)
      END
   END
END PutGnuAsm ;


(*
   GetGnuAsm - returns the string symbol, representing the instruction textual
               of the GnuAsm symbol. It will return a ConstString.
*)

PROCEDURE GetGnuAsm (sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      GnuAsmSym: RETURN( GnuAsm.String )

      ELSE
         InternalError('expecting GnuAsm symbol', __FILE__, __LINE__)
      END
   END
END GetGnuAsm ;


(*
   PutGnuAsmOutput - places the interface object, out, into GnuAsm symbol, sym.
*)

PROCEDURE PutGnuAsmOutput (sym: CARDINAL; out: CARDINAL) ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      GnuAsmSym: GnuAsm.Outputs := out

      ELSE
         InternalError('expecting PutGnuAsm symbol', __FILE__, __LINE__)
      END
   END
END PutGnuAsmOutput ;


(*
   PutGnuAsmInput - places the interface object, in, into GnuAsm symbol, sym.
*)

PROCEDURE PutGnuAsmInput (sym: CARDINAL; in: CARDINAL) ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      GnuAsmSym: GnuAsm.Inputs := in

      ELSE
         InternalError('expecting PutGnuAsm symbol', __FILE__, __LINE__)
      END
   END
END PutGnuAsmInput ;


(*
   PutGnuAsmTrash - places the interface object, trash, into GnuAsm symbol, sym.
*)

PROCEDURE PutGnuAsmTrash (sym: CARDINAL; trash: CARDINAL) ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      GnuAsmSym: GnuAsm.Trashed := trash

      ELSE
         InternalError('expecting PutGnuAsm symbol', __FILE__, __LINE__)
      END
   END
END PutGnuAsmTrash ;


(*
   GetGnuAsmInput - returns the input list of registers.
*)

PROCEDURE GetGnuAsmInput (sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      GnuAsmSym: RETURN( GnuAsm.Inputs )

      ELSE
         InternalError('expecting PutGnuAsm symbol', __FILE__, __LINE__)
      END
   END
END GetGnuAsmInput ;


(*
   GetGnuAsmOutput - returns the output list of registers.
*)

PROCEDURE GetGnuAsmOutput (sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      GnuAsmSym: RETURN( GnuAsm.Outputs )

      ELSE
         InternalError('expecting PutGnuAsm symbol', __FILE__, __LINE__)
      END
   END
END GetGnuAsmOutput ;


(*
   GetGnuAsmTrash - returns the list of trashed registers.
*)

PROCEDURE GetGnuAsmTrash (sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      GnuAsmSym: RETURN( GnuAsm.Trashed )

      ELSE
         InternalError('expecting PutGnuAsm symbol', __FILE__, __LINE__)
      END
   END
END GetGnuAsmTrash ;


(*
   PutGnuAsmVolatile - defines a GnuAsm symbol as VOLATILE.
*)

PROCEDURE PutGnuAsmVolatile (Sym: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      GnuAsmSym: GnuAsm.Volatile := TRUE

      ELSE
         InternalError('expecting GnuAsm symbol', __FILE__, __LINE__)
      END
   END
END PutGnuAsmVolatile ;


(*
   MakeRegInterface - creates and returns a register interface symbol.
*)

PROCEDURE MakeRegInterface () : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   NewSym(Sym) ;
   WITH Symbols[Sym] DO
      SymbolType := InterfaceSym ;
      WITH Interface DO
         InitList(StringList) ;
         InitList(ObjectList) ;
         InitWhereDeclared(At)
      END
   END ;
   RETURN( Sym )
END MakeRegInterface ;


(*
   PutRegInterface - places a, string, and, object, into the interface list, sym.
                     The string symbol will either be a register name or a constraint.
                     The object is an optional Modula-2 variable or constant symbol.
*)

PROCEDURE PutRegInterface (sym: CARDINAL; string, object: CARDINAL) ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      InterfaceSym: PutItemIntoList(Interface.StringList, string) ;
                    PutItemIntoList(Interface.ObjectList, object)

      ELSE
         InternalError('expecting Interface symbol', __FILE__, __LINE__)
      END
   END
END PutRegInterface ;


(*
   GetRegInterface - gets a, string, and, object, from the interface list, sym.
*)

PROCEDURE GetRegInterface (sym: CARDINAL; n: CARDINAL; VAR string, object: CARDINAL) ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      InterfaceSym: string := GetItemFromList(Interface.StringList, n) ;
                    object := GetItemFromList(Interface.ObjectList, n)

      ELSE
         InternalError('expecting Interface symbol', __FILE__, __LINE__)
      END
   END
END GetRegInterface ;


(*
   GetSubrange - returns HighSym and LowSym - two constants which make up the
                 subrange.
*)

PROCEDURE GetSubrange (Sym: CARDINAL; VAR HighSym, LowSym: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      SubrangeSym: HighSym := Subrange.High ;
                   LowSym := Subrange.Low

      ELSE
         InternalError('expecting Subrange symbol', __FILE__, __LINE__)
      END
   END
END GetSubrange ;


(*
   PutSubrange - places LowSym and HighSym as two symbols
                 which provide the limits of the range.
*)

PROCEDURE PutSubrange (Sym: CARDINAL; LowSym, HighSym: CARDINAL;
                       TypeSymbol: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      SubrangeSym:  Subrange.Low := LowSym ;      (* Index to symbol for lower   *)
                    Subrange.High := HighSym ;    (* Index to symbol for higher  *)
                    Subrange.Type := TypeSymbol ; (* Index to type symbol for    *)
                                                  (* the type of subrange.       *)
      ELSE
         InternalError('expecting Subrange symbol', __FILE__, __LINE__)
      END
   END
END PutSubrange ;


(*
   SetCurrentModule - Used to set the CurrentModule to a symbol, Sym.
                      This Sym must represent the module name of the
                      file currently being compiled.
*)

PROCEDURE SetCurrentModule (Sym: CARDINAL) ;
BEGIN
   CurrentModule := Sym
END SetCurrentModule ;


(*
   GetCurrentModule - returns the current module Sym that is being
                      compiled.
*)

PROCEDURE GetCurrentModule () : CARDINAL ;
BEGIN
   RETURN( CurrentModule )
END GetCurrentModule ;


(*
   SetMainModule - Used to set the MainModule to a symbol, Sym.
                   This Sym must represent the main module which was
                   envoked by the user to be compiled.
*)

PROCEDURE SetMainModule (Sym: CARDINAL) ;
BEGIN
   MainModule := Sym
END SetMainModule ;


(*
   GetMainModule - returns the main module symbol that was requested by
                   the user to be compiled.
*) 
 
PROCEDURE GetMainModule () : CARDINAL ; 
BEGIN
   RETURN( MainModule )
END GetMainModule ;


(*
   SetFileModule - Used to set the FileModule to a symbol, Sym.
                   This Sym must represent the current program module
                   file which is being parsed.
*)

PROCEDURE SetFileModule (Sym: CARDINAL) ;
BEGIN
   FileModule := Sym
END SetFileModule ;


(*
   GetFileModule - returns the FileModule symbol that was requested by
                   the user to be compiled.
*) 
 
PROCEDURE GetFileModule () : CARDINAL ; 
BEGIN
   RETURN( FileModule )
END GetFileModule ;


(*
   GetBaseModule - returns the base module symbol that contains Modula-2
                   base types, procedures and functions.
*) 
 
PROCEDURE GetBaseModule () : CARDINAL ; 
BEGIN
   RETURN( BaseModule )
END GetBaseModule ;


(*
   GetSym - searches the current scope (and previous scopes if the
            scope tranparent allows) for a symbol with name.
*)

PROCEDURE GetSym (name: Name) : CARDINAL ;
VAR
   Sym        : CARDINAL ;
   OldScopePtr: CARDINAL ;
BEGIN
   Sym := GetScopeSym(name) ;
   IF Sym=NulSym
   THEN
      (* Check default base types for symbol *)
      OldScopePtr := ScopePtr ;  (* Save ScopePtr *)
      ScopePtr := BaseScopePtr ; (* Alter ScopePtr to point to top of BaseModule *)
      Sym := GetScopeSym(name) ; (* Search BaseModule for name *)
      ScopePtr := OldScopePtr    (* Restored ScopePtr *)
   END ;
   RETURN( Sym )
END GetSym ;


(*
   GetScopeSym - searches the current scope and below, providing that the
                 scopes are transparent, for a symbol with name, name.
*)

PROCEDURE GetScopeSym (name: Name) : CARDINAL ;
VAR
   ScopeSym,
   ScopeId ,
   Sym     : CARDINAL ;
BEGIN
   (* DisplayScopes ; *)
   ScopeId := ScopePtr ;
   ScopeSym := ScopeCallFrame[ScopeId].Search ;
   (* WriteString(' scope: ') ; WriteKey(GetSymName(ScopeSym)) ; *)
   Sym := CheckScopeForSym(ScopeSym, name) ;
   WHILE (ScopeId>0) AND (Sym=NulSym) AND TransparentScope(ScopeSym) DO
      DEC(ScopeId) ;
      ScopeSym := ScopeCallFrame[ScopeId].Search ;
      Sym := CheckScopeForSym(ScopeSym, name) ;
      (* WriteString(' scope: ') ; WriteKey(GetSymName(ScopeSym)) *)
   END ;
   (* IF Sym#NulSym THEN WriteKey(GetSymName(Sym)) END ; WriteLn ; *)
   RETURN( Sym )
END GetScopeSym ;


(*
   CheckScopeForSym - checks the scope, ScopeSym, for an identifier
                      of name, name. CheckScopeForSym checks for
                      the symbol by the GetLocalSym and also
                      ExamineUnresolvedTree.
*)

PROCEDURE CheckScopeForSym (ScopeSym: CARDINAL; name: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := GetLocalSym(ScopeSym, name) ;
   IF (Sym=NulSym) AND (IsModule(ScopeSym) OR IsDefImp(ScopeSym) OR
                        IsProcedure(ScopeSym))
   THEN
      Sym := ExamineUnresolvedTree(ScopeSym, name)
   END ;
   RETURN( Sym )
END CheckScopeForSym ;


(*
   DisplayScopes - displays the scopes that will be searched to find
                   a requested symbol.
*)

PROCEDURE DisplayScopes ;
VAR
   n  : Name ;
   i  : CARDINAL ;
   Sym: CARDINAL ;
BEGIN
   i := ScopePtr ;
   printf0('Displaying scopes\n') ;
   WHILE i>=1 DO
      Sym := ScopeCallFrame[i].Search ;
      printf1('Symbol %4d', Sym) ;
      IF Sym#NulSym
      THEN
         n := GetSymName(Sym) ;
         printf1(' : name %a is ', n) ;
         IF NOT TransparentScope(Sym)
         THEN
            printf0('not')
         END ;
         printf0(' transparent\n')
      END ;
      DEC(i)
   END ;
   printf0('\n')
END DisplayScopes ;


(*
   GetModuleScopeId - returns the scope index to the next module starting
                      at index, Id.
                      Id will either point to a null scope (NulSym) or
                      alternatively point to a Module or DefImp symbol.
*)

PROCEDURE GetModuleScopeId (Id: CARDINAL) : CARDINAL ;
VAR
   s: CARDINAL ;  (* Substitute s for ScopeCallFrame[Id], when we have better compiler! *)
BEGIN
   s := ScopeCallFrame[Id].Search ;
   WHILE (Id>0) AND (s#NulSym) AND
         ((NOT IsModule(s)) AND
          (NOT IsDefImp(s))) DO
      DEC(Id) ;
      s := ScopeCallFrame[Id].Search ;
   END ;
   RETURN( Id )
END GetModuleScopeId ;


(*
   GetVisibleSym - 
*)

PROCEDURE GetVisibleSym (name: Name) : CARDINAL ;
VAR
   Sym,
   i  : CARDINAL ;
BEGIN
   i := ScopePtr ;
   WHILE i>=1 DO
      WITH ScopeCallFrame[i] DO
         IF Search=Main
         THEN
            RETURN( GetLocalSym(Main, name) )
         ELSE
            IF IsEnumeration(Search)
            THEN
               Sym := GetLocalSym(Search, name) ;
               IF Sym#NulSym
               THEN
                  RETURN( Sym )
               END
            END
         END
      END ;
      DEC(i)
   END ;
   RETURN( NulSym )
END GetVisibleSym ;


(*
   IsAlreadyDeclaredSym - returns true if Sym has already been declared
                          in the current main scope.
*)

PROCEDURE IsAlreadyDeclaredSym (name: Name) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   i := ScopePtr ;
   WHILE i>=1 DO
      WITH ScopeCallFrame[i] DO
         IF Search=Main
         THEN
            RETURN( GetLocalSym(Main, name)#NulSym )
         ELSE
            IF IsEnumeration(Search) AND (GetLocalSym(Search, name)#NulSym)
            THEN
               RETURN( TRUE )
            END
         END
      END ;
      DEC(i)
   END ;
   RETURN( FALSE )
END IsAlreadyDeclaredSym ;


(*
   MakeModule - creates a module sym with ModuleName. It returns the
                symbol index.
*)

PROCEDURE MakeModule (ModuleName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   (*
      Make a new symbol since we are at the outer scope level.
      DeclareSym examines the current scope level for any symbols
      that have the correct name, but are yet undefined.
      Therefore we must not call DeclareSym but create a symbol
      directly.
   *)
   NewSym(Sym) ;
   WITH Symbols[Sym] DO
      SymbolType := ModuleSym ;
      WITH Module DO
         name := ModuleName ;               (* Index into name array, name   *)
                                            (* of record field.              *)
         Size := InitValue() ;              (* Runtime size of symbol.       *)
         Offset := InitValue() ;            (* Offset at runtime of symbol   *)
         InitTree(LocalSymbols) ;           (* The LocalSymbols hold all the *)
                                            (* variables declared local to   *)
                                            (* the block. It contains the    *)
                                            (* FROM _ IMPORT x, y, x ;       *)
                                            (* IMPORT A ;                    *)
                                            (*    and also                   *)
                                            (* MODULE WeAreHere ;            *)
                                            (*    x y z visiable by localsym *)
                                            (*    MODULE Inner ;             *)
                                            (*       EXPORT x, y, z ;        *)
                                            (*    END Inner ;                *)
                                            (* END WeAreHere.                *)
         InitTree(ExportTree) ;             (* Holds all the exported        *)
                                            (* identifiers.                  *)
                                            (* This tree may be              *)
                                            (* deleted at the end of Pass 1. *)
         InitTree(ImportTree) ;             (* Contains all IMPORTed         *)
                                            (* identifiers.                  *)
         InitList(IncludeList) ;            (* Contains all included symbols *)
                                            (* which are included by         *)
                                            (* IMPORT modulename ;           *)
                                            (* modulename.Symbol             *)
         InitTree(ExportUndeclared) ;       (* ExportUndeclared contains all *)
                                            (* the identifiers which were    *)
                                            (* exported but have not yet     *)
                                            (* been declared.                *)
         InitList(EnumerationScopeList) ;   (* Enumeration scope list which  *)
                                            (* contains a list of all        *)
                                            (* enumerations which are        *)
                                            (* visable within this scope.    *)
                                            (* Outer Module.                 *)
         InitTree(NamedObjects) ;           (* Names of all items declared.  *)
         InitTree(NamedImports) ;           (* Names of items imported.      *)
         InitTree(WhereImported) ;          (* Sym to TokenNo where import   *)
                                            (* occurs. Error message use.    *)
         Priority := NulSym ;               (* Priority of the module. This  *)
                                            (* is an index to a constant.    *)
         InitTree(Unresolved) ;             (* All symbols currently         *)
                                            (* unresolved in this module.    *)
         StartQuad := 0 ;                   (* Signify the initialization    *)
                                            (* code.                         *)
         EndQuad := 0 ;                     (* EndQuad should point to a     *)
                                            (* goto quad.                    *)
         StartFinishQuad := 0 ;             (* Signify the finalization      *)
                                            (* code.                         *)
         EndFinishQuad := 0 ;               (* should point to a finish      *)
         FinallyFunction := NIL ;           (* The GCC function for finally  *)
         ExceptionFinally := FALSE ;        (* does it have an exception?    *)
         ExceptionBlock := FALSE ;          (* does it have an exception?    *)
         InitList(ListOfVars) ;             (* List of variables in this     *)
                                            (* scope.                        *)
         InitList(ListOfProcs) ;            (* List of all procedures        *)
                                            (* declared within this module.  *)
         InitList(ListOfModules) ;          (* List of all inner modules.    *)
         InitWhereDeclared(At) ;            (* Where symbol declared.        *)
         InitWhereFirstUsed(At) ;           (* Where symbol first used.      *)
         IF ScopeCallFrame[ScopePtr].Main=GetBaseModule()
         THEN
            Scope := NulSym
         ELSE
            Scope := ScopeCallFrame[ScopePtr].Main
         END
      END
   END ;
   PutSymKey(ModuleTree, ModuleName, Sym) ;
   RETURN( Sym )
END MakeModule ;


(*
   AddModuleToParent - adds symbol, Sym, to module, Parent.
*)

PROCEDURE AddModuleToParent (Sym: CARDINAL; Parent: CARDINAL) ;
BEGIN
   WITH Symbols[Parent] DO
      CASE SymbolType OF

      DefImpSym   :  PutItemIntoList(DefImp.ListOfModules, Sym) |
      ModuleSym   :  PutItemIntoList(Module.ListOfModules, Sym) |
      ProcedureSym:  PutItemIntoList(Procedure.ListOfModules, Sym)

      ELSE
         InternalError('expecting DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END AddModuleToParent ;


(*
   MakeInnerModule - creates an inner module sym with ModuleName. It returns the
                     symbol index.
*)

PROCEDURE MakeInnerModule (ModuleName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := DeclareSym(ModuleName) ;
   IF NOT IsError(Sym)
   THEN
      WITH Symbols[Sym] DO
         SymbolType := ModuleSym ;
         WITH Module DO
            name := ModuleName ;            (* Index into name array, name   *)
                                            (* of record field.              *)
            Size := InitValue() ;           (* Runtime size of symbol.       *)
            Offset := InitValue() ;         (* Offset at runtime of symbol   *)
            InitTree(LocalSymbols) ;        (* The LocalSymbols hold all the *)
                                            (* variables declared local to   *)
                                            (* the block. It contains the    *)
                                            (* FROM _ IMPORT x, y, x ;       *)
                                            (* IMPORT A ;                    *)
                                            (*    and also                   *)
                                            (* MODULE WeAreHere ;            *)
                                            (*    x y z visiable by localsym *)
                                            (*    MODULE Inner ;             *)
                                            (*       EXPORT x, y, z ;        *)
                                            (*    END Inner ;                *)
                                            (* END WeAreHere.                *)
            InitTree(ExportTree) ;          (* Holds all the exported        *)
                                            (* identifiers.                  *)
                                            (* This tree may be              *)
                                            (* deleted at the end of Pass 1. *)
            InitTree(ImportTree) ;          (* Contains all IMPORTed         *)
                                            (* identifiers.                  *)
            InitList(IncludeList) ;         (* Contains all included symbols *)
                                            (* which are included by         *)
                                            (* IMPORT modulename ;           *)
                                            (* modulename.Symbol             *)
            InitTree(ExportUndeclared) ;    (* ExportUndeclared contains all *)
                                            (* the identifiers which were    *)
                                            (* exported but have not yet     *)
                                            (* been declared.                *)
            InitList(EnumerationScopeList) ;(* Enumeration scope list which  *)
                                            (* contains a list of all        *)
                                            (* enumerations which are        *)
                                            (* visable within this scope.    *)
            InitTree(NamedObjects) ;        (* Names of all items declared.  *)
            InitTree(NamedImports) ;        (* Names of items imported.      *)
            InitTree(WhereImported) ;       (* Sym to TokenNo where import   *)
                                            (* occurs. Error message use.    *)
            Priority := NulSym ;            (* Priority of the module. This  *)
                                            (* is an index to a constant.    *)
            InitTree(Unresolved) ;          (* All symbols currently         *)
                                            (* unresolved in this module.    *)
            StartQuad := 0 ;                (* Signify the initialization    *)
                                            (* code.                         *)
            EndQuad := 0 ;                  (* EndQuad should point to a     *)
                                            (* goto quad.                    *)
            StartFinishQuad := 0 ;          (* Signify the finalization      *)
                                            (* code.                         *)
            EndFinishQuad := 0 ;            (* should point to a finish      *)
            FinallyFunction := NIL ;        (* The GCC function for finally  *)
            ExceptionFinally := FALSE ;     (* does it have an exception?    *)
            ExceptionBlock := FALSE ;       (* does it have an exception?    *)
            InitList(ListOfVars) ;          (* List of variables in this     *)
                                            (* scope.                        *)
            InitList(ListOfProcs) ;         (* List of all procedures        *)
                                            (* declared within this module.  *)
            InitList(ListOfModules) ;       (* List of all inner modules.    *)
            InitWhereDeclared(At) ;         (* Where symbol declared.        *)
            InitWhereFirstUsed(At) ;        (* Where symbol first used.      *)
            IF GetCurrentScope()=GetBaseModule()
            THEN
               Scope := NulSym
            ELSE
               Scope := GetCurrentScope() ;
               AddModuleToParent(Sym, Scope)
            END
         END ;
      END ;
      AddSymToScope(Sym, ModuleName)
   END ;
   RETURN( Sym )
END MakeInnerModule ;


(*
   MakeDefImp - creates a definition and implementation module sym
                with name DefImpName. It returns the symbol index.
*)

PROCEDURE MakeDefImp (DefImpName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   (*
      Make a new symbol since we are at the outer scope level.
      DeclareSym examines the current scope level for any symbols
      that have the correct name, but are yet undefined.
      Therefore we must not call DeclareSym but create a symbol
      directly.
   *)
   NewSym(Sym) ;
   WITH Symbols[Sym] DO
      SymbolType := DefImpSym ;
      WITH DefImp DO
         name := DefImpName ;         (* Index into name array, name   *)
                                      (* of record field.              *)
         Type := NulSym ;             (* Index to a type symbol.       *)
         Size := InitValue() ;        (* Runtime size of symbol.       *)
         Offset := InitValue() ;      (* Offset at runtime of symbol   *)
         InitTree(ExportQualifiedTree) ;
                                      (* Holds all the EXPORT          *)
                                      (* QUALIFIED identifiers.        *)
                                      (* This tree may be              *)
                                      (* deleted at the end of Pass 1. *)
         InitTree(ExportUnQualifiedTree) ;
                                      (* Holds all the EXPORT          *)
                                      (* UNQUALIFIED identifiers.      *)
                                      (* This tree may be              *)
                                      (* deleted at the end of Pass 1. *)
         InitTree(ExportRequest) ;    (* Contains all identifiers that *)
                                      (* have been requested by other  *)
                                      (* modules before this module    *)
                                      (* declared its export list.     *)
                                      (* This tree should be empty at  *)
                                      (* the end of the compilation.   *)
                                      (* Each time a symbol is         *)
                                      (* exported it is removed from   *)
                                      (* this list.                    *)
         InitTree(ImportTree) ;       (* Contains all IMPORTed         *)
                                      (* identifiers.                  *)
         InitList(IncludeList) ;      (* Contains all included symbols *)
                                      (* which are included by         *)
                                      (* IMPORT modulename ;           *)
                                      (* modulename.Symbol             *)
         InitTree(ExportUndeclared) ; (* ExportUndeclared contains all *)
                                      (* the identifiers which were    *)
                                      (* exported but have not yet     *)
                                      (* been declared.                *)
         InitTree(NeedToBeImplemented) ;
                                      (* NeedToBeImplemented contains  *)
                                      (* the identifiers which have    *)
                                      (* been exported and declared    *)
                                      (* but have not yet been         *)
                                      (* implemented.                  *)
         InitTree(LocalSymbols) ;     (* The LocalSymbols hold all the *)
                                      (* variables declared local to   *)
                                      (* the block. It contains the    *)
                                      (* IMPORT r ;                    *)
                                      (* FROM _ IMPORT x, y, x ;       *)
                                      (*    and also                   *)
                                      (* MODULE WeAreHere ;            *)
                                      (*    x y z visiable by localsym *)
                                      (*    MODULE Inner ;             *)
                                      (*       EXPORT x, y, z ;        *)
                                      (*    END Inner ;                *)
                                      (* END WeAreHere.                *)
         InitList(EnumerationScopeList) ;
                                      (* Enumeration scope list which  *)
                                      (* contains a list of all        *)
                                      (* enumerations which are        *)
                                      (* visable within this scope.    *)
         InitTree(NamedObjects) ;     (* names of all items declared.  *)
         InitTree(NamedImports) ;     (* Names of items imported.      *)
         InitTree(WhereImported) ;    (* Sym to TokenNo where import   *)
                                      (* occurs. Error message use.    *)
         Priority := NulSym ;         (* Priority of the module. This  *)
                                      (* is an index to a constant.    *)
         InitTree(Unresolved) ;       (* All symbols currently         *)
                                      (* unresolved in this module.    *)
         StartQuad := 0 ;             (* Signify the initialization    *)
                                      (* code.                         *)
         EndQuad := 0 ;               (* EndQuad should point to a     *)
                                      (* goto quad.                    *)
         StartFinishQuad := 0 ;       (* Signify the finalization      *)
                                      (* code.                         *)
         EndFinishQuad := 0 ;         (* should point to a finish      *)
         FinallyFunction := NIL ;     (* The GCC function for finally  *)
         ExceptionFinally := FALSE ;  (* does it have an exception?    *)
         ExceptionBlock := FALSE ;    (* does it have an exception?    *)
         ContainsHiddenType := FALSE ;(* True if this module           *)
                                      (* implements a hidden type.     *)
         ForC := FALSE ;              (* Is it a definition for "C"    *)
         NeedExportList := FALSE ;    (* Must user supply export list? *)
         InitList(ListOfVars) ;       (* List of variables in this     *)
                                      (* scope.                        *)
         InitList(ListOfProcs) ;      (* List of all procedures        *)
                                      (* declared within this module.  *)
         InitList(ListOfModules) ;    (* List of all inner modules.    *)
         InitWhereDeclared(At) ;      (* Where symbol declared.        *)
         InitWhereFirstUsed(At) ;     (* Where symbol first used.      *)
      END
   END ;
   PutSymKey(ModuleTree, DefImpName, Sym) ;
   RETURN( Sym )
END MakeDefImp ;


(*
   MakeProcedure - creates a procedure sym with name. It returns
                   the symbol index.
*)

PROCEDURE MakeProcedure (ProcedureName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := DeclareSym(ProcedureName) ;
   IF NOT IsError(Sym)
   THEN
      WITH Symbols[Sym] DO
         SymbolType := ProcedureSym ;
         WITH Procedure DO
            name := ProcedureName ;
            InitList(ListOfParam) ;      (* Contains a list of all the    *)
                                         (* parameters in this procedure. *)
            ParamDefined := FALSE ;      (* Have the parameters been      *)
                                         (* defined yet?                  *)
            DefinedInDef := FALSE ;      (* Were the parameters defined   *)
                                         (* in the Definition module?     *)
                                         (* Note that this depends on     *)
                                         (* whether the compiler has read *)
                                         (* the .def or .mod first.       *)
                                         (* The second occurence is       *)
                                         (* compared to the first.        *)
            DefinedInImp := FALSE ;      (* Were the parameters defined   *)
                                         (* in the Implementation module? *)
                                         (* Note that this depends on     *)
                                         (* whether the compiler has read *)
                                         (* the .def or .mod first.       *)
                                         (* The second occurence is       *)
                                         (* compared to the first.        *)
            HasVarArgs := FALSE ;        (* Does the procedure use ... ?  *)
            HasOptArg := FALSE ;         (* Does this procedure use [ ] ? *)
            OptArgInit := NulSym ;       (* The optarg initial value.     *)
            IsBuiltin := FALSE ;         (* Was it declared __BUILTIN__ ? *)
            BuiltinName := NulName ;     (* name of equivalent builtin    *)
            IsInline := FALSE ;          (* Was is declared __INLINE__ ?  *)
            ReturnOptional := FALSE ;    (* Is the return value optional? *)
            Scope := GetCurrentScope() ; (* Scope of procedure.           *)
            InitTree(Unresolved) ;       (* All symbols currently         *)
                                         (* unresolved in this procedure. *)
            StartQuad := 0 ;             (* Index into list of quads.     *)
            EndQuad := 0 ;
            Reachable := FALSE ;         (* Procedure not known to be     *)
                                         (* reachable.                    *)
            SavePriority := FALSE ;      (* Does procedure need to save   *)
                                         (* and restore interrupts?       *)
            ReturnType := NulSym ;       (* Not a function yet!           *)
            Offset := 0 ;                (* Location of procedure.        *)
            InitTree(LocalSymbols) ;
            InitList(EnumerationScopeList) ;
                                         (* Enumeration scope list which  *)
                                         (* contains a list of all        *)
                                         (* enumerations which are        *)
                                         (* visable within this scope.    *)
            InitTree(NamedObjects) ;     (* Names of all items declared.  *)
            InitList(ListOfVars) ;       (* List of variables in this     *)
                                         (* scope.                        *)
            InitList(ListOfProcs) ;      (* List of all procedures        *)
                                         (* declared within this          *)
                                         (* procedure.                    *)
            InitList(ListOfModules) ;    (* List of all inner modules.    *)
            ExceptionFinally := FALSE ;  (* does it have an exception?    *)
            ExceptionBlock := FALSE ;    (* does it have an exception?    *)
            Size := InitValue() ;        (* Activation record size.       *)
            TotalParamSize
                       := InitValue() ;  (* size of all parameters.       *)
            InitWhereDeclared(At) ;      (* Where symbol declared.        *)
         END
      END ;
      (* Now add this procedure to the symbol table of the current scope *)
      AddSymToScope(Sym, ProcedureName) ;
      AddProcedureToList(GetCurrentScope(), Sym)
   END ;
   RETURN( Sym )
END MakeProcedure ;


(*
   AddProcedureToList - adds a procedure, Proc, to the list of procedures
                        in module, Mod.
*)

PROCEDURE AddProcedureToList (Mod, Proc: CARDINAL) ;
BEGIN
   WITH Symbols[Mod] DO
      CASE SymbolType OF

      DefImpSym   : PutItemIntoList(DefImp.ListOfProcs, Proc) |
      ModuleSym   : PutItemIntoList(Module.ListOfProcs, Proc) |
      ProcedureSym: PutItemIntoList(Procedure.ListOfProcs, Proc)

      ELSE
         InternalError('expecting ModuleSym, DefImpSym or ProcedureSym symbol', __FILE__, __LINE__)
      END
   END
END AddProcedureToList ;


(*
   AddVarToScopeList - adds symbol, sym, to, scope.
*)

PROCEDURE AddVarToScopeList (scope, sym: CARDINAL) ;
BEGIN
   WITH Symbols[scope] DO
      CASE SymbolType OF

      ProcedureSym: PutItemIntoList(Procedure.ListOfVars, sym) |
      ModuleSym   : PutItemIntoList(Module.ListOfVars, sym) |
      DefImpSym   : PutItemIntoList(DefImp.ListOfVars, sym)

      ELSE
         InternalError('expecting Procedure or Module symbol', __FILE__, __LINE__)
      END
   END
END AddVarToScopeList ;


(*
   AddVarToList - add a variable symbol to the list of variables maintained
                  by the inner most scope. (Procedure or Module).
*)

PROCEDURE AddVarToList (Sym: CARDINAL) ;
BEGIN
   AddVarToScopeList(ScopeCallFrame[ScopePtr].Main, Sym)
END AddVarToList ;


(*
   MakeVar - creates a variable sym with VarName. It returns the
             symbol index.
*)

PROCEDURE MakeVar (VarName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := DeclareSym(VarName) ;
   IF NOT IsError(Sym)
   THEN
      WITH Symbols[Sym] DO
         SymbolType := VarSym ;
         WITH Var DO
            name := VarName ;
            Type := NulSym ;
            BackType := NulSym ;
            Size := InitValue() ;
            Offset := InitValue() ;
            AddrMode := RightValue ;
            Scope := GetCurrentScope() ;  (* Procedure or Module ? *)
            AtAddress := FALSE ;
            Address := NulSym ;           (* Address at which declared   *)
            IsTemp := FALSE ;
            IsParam := FALSE ;
            IsPointerCheck := FALSE ;
            IsWritten := FALSE ;
            InitWhereDeclared(At) ;
            InitWhereFirstUsed(At) ;      (* Where symbol first used.      *)
            InitList(ReadUsageList[RightValue]) ;
            InitList(WriteUsageList[RightValue]) ;
            InitList(ReadUsageList[LeftValue]) ;
            InitList(WriteUsageList[LeftValue])
         END
      END ;
      (* Add Var to Procedure or Module variable list *)
      AddVarToList(Sym) ;
      (* Now add this Var to the symbol table of the current scope *)
      AddSymToScope(Sym, VarName)
   END ;
   RETURN( Sym )
END MakeVar ;


(*
   PutExceptionBlock - sets a BOOLEAN in block module/procedure/defimp,
                       sym, indicating that this block as an EXCEPT
                       statement sequence.
*)

PROCEDURE PutExceptionBlock (sym: CARDINAL) ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      ProcedureSym: Procedure.ExceptionBlock := TRUE |
      ModuleSym   : Module.ExceptionBlock := TRUE |
      DefImpSym   : DefImp.ExceptionBlock := TRUE

      ELSE
         InternalError('expecting Procedure, DefImp or Module symbol',
                       __FILE__, __LINE__)
      END
   END
END PutExceptionBlock ;


(*
   HasExceptionBlock - returns a BOOLEAN determining whether
                       module/procedure/defimp, sym, has
                       an EXCEPT statement sequence.
*)

PROCEDURE HasExceptionBlock (sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      ProcedureSym: RETURN( Procedure.ExceptionBlock ) |
      ModuleSym   : RETURN( Module.ExceptionBlock ) |
      DefImpSym   : RETURN( DefImp.ExceptionBlock )

      ELSE
         InternalError('expecting Procedure, DefImp or Module symbol',
                       __FILE__, __LINE__)
      END
   END
END HasExceptionBlock ;


(*
   PutExceptionFinally - sets a BOOLEAN in block module/defimp,
                         sym, indicating that this FINALLY block
                         as an EXCEPT statement sequence.
*)

PROCEDURE PutExceptionFinally (sym: CARDINAL) ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      ProcedureSym: Procedure.ExceptionFinally := TRUE |
      ModuleSym   : Module.ExceptionFinally := TRUE |
      DefImpSym   : DefImp.ExceptionFinally := TRUE

      ELSE
         InternalError('expecting DefImp or Module symbol',
                       __FILE__, __LINE__)
      END
   END
END PutExceptionFinally ;


(*
   HasExceptionFinally - returns a BOOLEAN determining whether
                         module/defimp, sym, has
                         an EXCEPT statement sequence.
*)

PROCEDURE HasExceptionFinally (sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      ProcedureSym: RETURN( Procedure.ExceptionFinally ) |
      ModuleSym   : RETURN( Module.ExceptionFinally ) |
      DefImpSym   : RETURN( DefImp.ExceptionFinally )

      ELSE
         InternalError('expecting DefImp or Module symbol',
                       __FILE__, __LINE__)
      END
   END
END HasExceptionFinally ;


(*
   FillInRecordFields - given a new symbol, sym, make it a record symbol
                        and initialize its fields.
*)

PROCEDURE FillInRecordFields (Sym: CARDINAL; RecordName: Name;
                              scope: CARDINAL; unbounded: CARDINAL) ;
BEGIN
   IF NOT IsError(Sym)
   THEN
      WITH Symbols[Sym] DO
         SymbolType := RecordSym ;
         WITH Record DO
            name := RecordName ;
            InitTree(LocalSymbols) ;
            Size := InitValue() ;
            InitList(ListOfSons) ;   (* List of RecordFieldSym and VarientSym *)
            Unbounded := unbounded ;
            Parent := NulSym ;
            Scope := scope ;
            InitWhereDeclared(At)
         END
      END
   END
END FillInRecordFields ;


(*
   HandleHiddenOrDeclare - 
*)

PROCEDURE HandleHiddenOrDeclare (name: Name; VAR unbounded: CARDINAL) : CARDINAL ;
VAR
   sym: CARDINAL ;
BEGIN
   sym := CheckForHiddenType(name) ;
   IF sym=NulSym
   THEN
      sym := DeclareSym(name) ;
      IF NOT IsError(sym)
      THEN
         (* Now add this type to the symbol table of the current scope *)
         AddSymToScope(sym, name)
      END
   END ;
   unbounded := GetUnbounded(sym) ;
   RETURN( sym )
END HandleHiddenOrDeclare ;


(*
   MakeRecord - makes the a Record symbol with name RecordName.
*)

PROCEDURE MakeRecord (RecordName: Name) : CARDINAL ;
VAR
   unbounded,
   sym      : CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare(RecordName, unbounded) ;
   FillInRecordFields(sym, RecordName, GetCurrentScope(), unbounded) ;
   FillInUnboundedFields(unbounded, sym) ;
   RETURN( sym )
END MakeRecord ;


(*
   MakeVarient - creates a new symbol, a varient symbol for record symbol,
                 RecOrVarFieldSym.
*)

PROCEDURE MakeVarient (RecOrVarFieldSym: CARDINAL) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   NewSym(Sym) ;
   WITH Symbols[Sym] DO
      SymbolType := VarientSym ;
      WITH Varient DO
         Size := InitValue() ;
         Parent := GetRecord(RecOrVarFieldSym) ;
         IF IsRecord(RecOrVarFieldSym)
         THEN
            Varient := NulSym
         ELSE
            Varient := RecOrVarFieldSym
         END ;
         Scope := GetCurrentScope() ;
         InitList(ListOfSons) ;
         InitWhereDeclared(At)
      END
   END ;
   (* Now add Sym to the record RecSym field list *)
   WITH Symbols[RecOrVarFieldSym] DO
      CASE SymbolType OF

      RecordSym      : PutItemIntoList(Record.ListOfSons, Sym) |
      VarientFieldSym: PutItemIntoList(VarientField.ListOfSons, Sym)

      ELSE
         InternalError('expecting Record or VarientField symbol',
                       __FILE__, __LINE__)
      END
   END ;
   RETURN( Sym )
END MakeVarient ;


(*
   GetRecord - fetches the record symbol from the parent of Sym.
               Sym maybe a varient symbol in which case its parent is searched
               etc.
*)

PROCEDURE GetRecord (Sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      RecordSym      : RETURN( Sym ) |
      VarientSym     : RETURN( GetRecord(Varient.Parent) ) |
      VarientFieldSym: RETURN( GetRecord(VarientField.Parent) )

      ELSE
         InternalError('expecting Record or Varient symbol', __FILE__, __LINE__)
      END
   END
END GetRecord ;


(*
   MakeEnumeration - places a new symbol in the current scope, the symbol
                     is an enumeration symbol. The symbol index is returned.
*)

PROCEDURE MakeEnumeration (EnumerationName: Name) : CARDINAL ;
VAR
   sym,
   unbounded: CARDINAL ;
BEGIN
   sym := CheckForHiddenType(EnumerationName) ;
   IF sym=NulSym
   THEN
      sym := DeclareSym(EnumerationName) ;
      unbounded := GetUnbounded(sym) ;
      IF NOT IsError(sym)
      THEN
         Symbols[sym].SymbolType := EnumerationSym ; (* To satisfy AddSymToScope *)
         (* Now add this type to the symbol table of the current scope *)
         AddSymToScope(sym, EnumerationName)
      END
   ELSE
      unbounded := GetUnbounded(sym)
   END ;
   IF NOT IsError(sym)
   THEN
      WITH Symbols[sym] DO
         SymbolType := EnumerationSym ;
         WITH Enumeration DO
            name := EnumerationName ;      (* Name of enumeration.   *)
            NoOfElements := 0 ;            (* No of elements in the  *)
                                           (* enumeration type.      *)
            Size := InitValue() ;          (* Size at runtime of sym *)
            InitTree(LocalSymbols) ;       (* Enumeration fields.    *)
            Unbounded := unbounded ;       (* The unbounded for this *)
            Scope := GetCurrentScope() ;   (* Which scope created it *)
            InitWhereDeclared(At)          (* Declared here          *)
         END
      END ;
      CheckIfEnumerationExported(sym, ScopePtr)
   END ;
   FillInUnboundedFields(unbounded, sym) ;
   RETURN( sym )
END MakeEnumeration ;


(*
   MakeType - makes a type symbol with name TypeName.
*)

PROCEDURE MakeType (TypeName: Name) : CARDINAL ;
VAR
   sym,
   unbounded: CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare(TypeName, unbounded) ;
   IF NOT IsError(sym)
   THEN
      WITH Symbols[sym] DO
         SymbolType := TypeSym ;
         WITH Type DO
            name := TypeName ;        (* Index into name array, name *)
                                      (* of type.                    *)
            Type := NulSym ;          (* Index to a type symbol.     *)
            IsHidden := FALSE ;       (* Was it declared as hidden?  *)
            Size := InitValue() ;     (* Runtime size of symbol.     *)
            Unbounded := unbounded ;  (* The unbounded for this *)
            Scope := GetCurrentScope() ;   (* Which scope created it *)
            InitWhereDeclared(At)     (* Declared here               *)
         END
      END
   END ;
   FillInUnboundedFields(unbounded, sym) ;
   RETURN( sym )
END MakeType ;


(*
   MakeHiddenType - makes a type symbol that is hidden from the
                    definition module.
                    This symbol is placed into the UnImplemented list of
                    the definition/implementation module.
                    The type will be filled in when the implementation module
                    is reached.
*)

PROCEDURE MakeHiddenType (TypeName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := DeclareSym(TypeName) ;
   IF NOT IsError(Sym)
   THEN
      WITH Symbols[Sym] DO
         SymbolType := TypeSym ;
         WITH Type DO
            name   := TypeName ;    (* Index into name array, name *)
                                    (* of type.                    *)
            IsHidden := GetMainModule()#GetCurrentScope() ;
            IF ExtendedOpaque OR (NOT IsHidden)
            THEN
               Type := NulSym       (* will be filled in later     *)
            ELSE
               Type := Address
            END ;
            IF NOT ExtendedOpaque
            THEN
               IncludeItemIntoList(AddressTypes, Sym)
            END ;
            Size   := InitValue() ; (* Runtime size of symbol.     *)
            InitWhereDeclared(At)   (* Declared here               *)
         END
      END ;
      PutExportUnImplemented(Sym) ;
      IF ExtendedOpaque OR (GetMainModule()=GetCurrentScope())
      THEN
         PutHiddenTypeDeclared
      END ;
      (* Now add this type to the symbol table of the current scope *)
      AddSymToScope(Sym, TypeName)
   END ;
   RETURN( Sym )
END MakeHiddenType ;


(*
   MakeConstLit - put a constant which has the string described by ConstName
                  into the ConstantTree. The symbol number is returned.
                  If the constant already exits
                  then a duplicate constant is not entered in the tree.
                  All values of constant literals
                  are ignored in Pass 1 and evaluated in Pass 2 via
                  character manipulation.
*)

PROCEDURE MakeConstLit (ConstName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := GetSymKey(ConstLitTree, ConstName) ;
   IF Sym=NulSym
   THEN
      NewSym(Sym) ;
      PutSymKey(ConstLitTree, ConstName, Sym) ;
      WITH Symbols[Sym] DO
         SymbolType := ConstLitSym ;
         CASE SymbolType OF

         ConstLitSym : ConstLit.name := ConstName ;
                       ConstLit.Value := InitValue() ;
                       PushString(ConstName) ;
                       PopInto(ConstLit.Value) ;
                       ConstLit.Type := GetConstLitType(Sym) ;
                       ConstLit.IsSet := FALSE ;
                       ConstLit.IsConstructor := FALSE ;
                       ConstLit.FromType := NulSym ;     (* type is determined FromType *)
                       ConstLit.UnresFromType := FALSE ; (* is Type resolved?           *)
                       InitWhereDeclared(ConstLit.At)

         ELSE
            InternalError('expecting ConstLit symbol', __FILE__, __LINE__)
         END
      END
   END ;
   RETURN( Sym )
END MakeConstLit ;


(*
   MakeConstVar - makes a ConstVar type with
                  name ConstVarName.
*)

PROCEDURE MakeConstVar (ConstVarName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := DeclareSym(ConstVarName) ;
   IF NOT IsError(Sym)
   THEN
      WITH Symbols[Sym] DO
         SymbolType := ConstVarSym ;
         WITH ConstVar DO
            name  := ConstVarName ;
            Value := InitValue() ;
            Type  := NulSym ;
            IsSet := FALSE ;
            IsConstructor := FALSE ;
            FromType := NulSym ;     (* type is determined FromType *)
            UnresFromType := FALSE ; (* is Type resolved?           *)
            IsTemp := FALSE ;
            InitWhereDeclared(At)
         END
      END ;
      (* Now add this constant to the symbol table of the current scope *)
      AddSymToScope(Sym, ConstVarName)
   END ;
   RETURN( Sym )
END MakeConstVar ;


(*
   MakeConstLitString - put a constant which has the string described by
                        ConstName into the ConstantTree.
                        The symbol number is returned.
                        This symbol is known as a String Constant rather than a
                        ConstLit which indicates a number.
                        If the constant already exits
                        then a duplicate constant is not entered in the tree.
                        All values of constant strings
                        are ignored in Pass 1 and evaluated in Pass 2 via
                        character manipulation.
                        In this procedure ConstName is the string.
*)

PROCEDURE MakeConstLitString (ConstName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := GetSymKey(ConstLitStringTree, ConstName) ;
   IF Sym=NulSym
   THEN
      NewSym(Sym) ;
      PutSymKey(ConstLitStringTree, ConstName, Sym) ;
      WITH Symbols[Sym] DO
         SymbolType := ConstStringSym ;
         CASE SymbolType OF

         ConstStringSym : ConstString.name := ConstName ;
                          PutConstString(Sym, ConstName) ;
                          InitWhereDeclared(ConstString.At)

         ELSE
            InternalError('expecting ConstString symbol', __FILE__, __LINE__)
         END
      END
   END ;
   RETURN( Sym )
END MakeConstLitString ;


(*
   MakeConstString - puts a constant into the symboltable which is a string.
                     The string value is unknown at this time and will be
                     filled in later by PutString.
*)
   
PROCEDURE MakeConstString (ConstName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   NewSym(Sym) ;
   PutSymKey(ConstLitStringTree, ConstName, Sym) ;
   WITH Symbols[Sym] DO
      SymbolType := ConstStringSym ;
      CASE SymbolType OF

      ConstStringSym : ConstString.name := ConstName ;
                       ConstString.Length := 0 ;
                       ConstString.String := NulKey ;
                       InitWhereDeclared(ConstString.At)

      ELSE
         InternalError('expecting ConstString symbol', __FILE__, __LINE__)
      END
   END ;
   RETURN( Sym )
END MakeConstString ;


(*
   PutConstString - places a string, String, into a constant symbol, Sym.
                    Sym maybe a ConstString or a ConstVar. If the later is
                    true then the ConstVar is converted to a ConstString.
*)

PROCEDURE PutConstString (Sym: CARDINAL; String: Name) ;
VAR
   n: Name ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ConstStringSym: ConstString.Length := LengthKey(String) ;
                      ConstString.String := String ;
                      InitWhereFirstUsed(ConstString.At) |

      ConstVarSym   : (* ok altering this to ConstString *)
                      n := ConstVar.name ;
                      (* copy name and alter symbol.     *)
                      SymbolType := ConstStringSym ;
                      ConstString.name := n ;
                      PutConstString(Sym, String)

      ELSE
         InternalError('expecting ConstString or ConstVar symbol',
                       __FILE__, __LINE__)
      END
   END
END PutConstString ;


(*
   GetString - returns the string of the symbol Sym, note that
               this is not the same as GetName since the name of a
               CONST declared string will be different to its value.
*)

PROCEDURE GetString (Sym: CARDINAL) : Name ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ConstStringSym: RETURN( ConstString.String )

      ELSE
         InternalError('expecting ConstString symbol', __FILE__, __LINE__)
      END
   END
END GetString ;


(*
   GetStringLength - returns the length of the string symbol Sym.
*)

PROCEDURE GetStringLength (Sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ConstStringSym: RETURN( ConstString.Length )

      ELSE
         InternalError('expecting ConstString symbol', __FILE__, __LINE__)
      END
   END
END GetStringLength ;


(*
   PutVariableAtAddress - determines that a variable, sym, is declared at
                          a specific address.
*)

PROCEDURE PutVariableAtAddress (sym: CARDINAL; address: CARDINAL) ;
BEGIN
   Assert(sym#NulSym) ;
   WITH Symbols[sym] DO
      CASE SymbolType OF

      VarSym:  Var.AtAddress := TRUE ;
               Var.Address := address

      ELSE
         InternalError('expecting a variable symbol', __FILE__, __LINE__)
      END
   END
END PutVariableAtAddress ;


(*
   GetVariableAtAddress - returns the address at which variable, sym, is declared.
*)

PROCEDURE GetVariableAtAddress (sym: CARDINAL) : CARDINAL ;
BEGIN
   Assert(sym#NulSym) ;
   WITH Symbols[sym] DO
      CASE SymbolType OF

      VarSym:  RETURN( Var.Address )

      ELSE
         InternalError('expecting a variable symbol', __FILE__, __LINE__)
      END
   END
END GetVariableAtAddress ;


(*
   IsVariableAtAddress - returns TRUE if a variable, sym, was declared at
                         a specific address.
*)

PROCEDURE IsVariableAtAddress (sym: CARDINAL) : BOOLEAN ;
BEGIN
   Assert(sym#NulSym) ;
   WITH Symbols[sym] DO
      CASE SymbolType OF

      VarSym:  RETURN( Var.AtAddress )

      ELSE
         InternalError('expecting a variable symbol', __FILE__, __LINE__)
      END
   END
END IsVariableAtAddress ;


(*
   PutPriority - places a interrupt, priority, value into module, module.
*)

PROCEDURE PutPriority (module: CARDINAL; priority: CARDINAL) ;
BEGIN
   Assert(module#NulSym) ;
   WITH Symbols[module] DO
      CASE SymbolType OF

      DefImpSym:  DefImp.Priority := priority |
      ModuleSym:  Module.Priority := priority

      ELSE
         InternalError('expecting DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END PutPriority ;


(*
   GetPriority - returns the interrupt priority which was assigned to
                 module, module.
*)

PROCEDURE GetPriority (module: CARDINAL) : CARDINAL ;
BEGIN
   Assert(module#NulSym) ;
   WITH Symbols[module] DO
      CASE SymbolType OF

      DefImpSym:  RETURN( DefImp.Priority ) |
      ModuleSym:  RETURN( Module.Priority )

      ELSE
         InternalError('expecting DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END GetPriority ;


(*
   PutNeedSavePriority - set a boolean flag indicating that this procedure
                         needs to save and restore interrupts.
*)

PROCEDURE PutNeedSavePriority (sym: CARDINAL) ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      ProcedureSym: Procedure.SavePriority := TRUE

      ELSE
         InternalError('expecting procedure symbol', __FILE__, __LINE__)
      END
   END
END PutNeedSavePriority ;


(*
   GetNeedSavePriority - returns the boolean flag indicating whether this procedure
                         needs to save and restore interrupts.
*)

PROCEDURE GetNeedSavePriority (sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      ProcedureSym: RETURN( Procedure.SavePriority )

      ELSE
         InternalError('expecting procedure symbol', __FILE__, __LINE__)
      END
   END
END GetNeedSavePriority ;


(*
   GetProcedureBuiltin - returns the builtin name for the equivalent procedure, Sym.
*)

PROCEDURE GetProcedureBuiltin (Sym: CARDINAL) : Name ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym   : RETURN( Procedure.BuiltinName )

      ELSE
         InternalError('expecting procedure symbol', __FILE__, __LINE__)
      END
   END
END GetProcedureBuiltin ;


(*
   PutProcedureBuiltin - assigns the builtin name for the equivalent procedure, Sym.
*)

PROCEDURE PutProcedureBuiltin (Sym: CARDINAL; name: Name) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym   : Procedure.BuiltinName := name ;
                       Procedure.IsBuiltin := TRUE ;
                       (* we use the same extra pass method as hidden types for builtins *)
                       PutHiddenTypeDeclared

      ELSE
         InternalError('expecting procedure symbol', __FILE__, __LINE__)
      END
   END
END PutProcedureBuiltin ;


(*
   IsProcedureBuiltin - returns TRUE if this procedure has a builtin equivalent.
*)

PROCEDURE IsProcedureBuiltin (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym   : RETURN( Procedure.IsBuiltin )

      ELSE
         InternalError('expecting procedure symbol', __FILE__, __LINE__)
      END
   END
END IsProcedureBuiltin ;


(*
   PutProcedureInline - determines that procedure, Sym, has been requested to be inlined.
*)

PROCEDURE PutProcedureInline (Sym: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym   : Procedure.IsInline := TRUE ;

      ELSE
         InternalError('expecting procedure symbol', __FILE__, __LINE__)
      END
   END
END PutProcedureInline ;


(*
   IsProcedureBuiltin - returns TRUE if this procedure was declared as inlined.
*)

PROCEDURE IsProcedureInline (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym   : RETURN( Procedure.IsInline )

      ELSE
         InternalError('expecting procedure symbol', __FILE__, __LINE__)
      END
   END
END IsProcedureInline ;


(*
   PutConstSet - informs the const var symbol, sym, that it is or will contain
                 a set value.
*)

PROCEDURE PutConstSet (Sym: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ConstVarSym:  ConstVar.IsSet := TRUE |
      ConstLitSym:  ConstLit.IsSet := TRUE

      ELSE
         InternalError('expecting ConstVar symbol', __FILE__, __LINE__)
      END
   END
END PutConstSet ;


(*
   IsConstSet - returns TRUE if the constant is declared as a set.
*)

PROCEDURE IsConstSet (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ConstVarSym:  RETURN( ConstVar.IsSet ) |
      ConstLitSym:  RETURN( ConstLit.IsSet )

      ELSE
         RETURN( FALSE )
      END
   END
END IsConstSet ;


(*
   PutConstructor - informs the const var symbol, sym, that it is or
                    will contain a constructor (record, set or array)
                    value.
*)

PROCEDURE PutConstructor (Sym: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ConstVarSym:  ConstVar.IsConstructor := TRUE |
      ConstLitSym:  ConstLit.IsConstructor := TRUE

      ELSE
         InternalError('expecting ConstVar or ConstLit symbol',
                       __FILE__, __LINE__)
      END
   END
END PutConstructor ;


(*
   IsConstructor - returns TRUE if the constant is declared as a
                   constant set, array or record.
*)

PROCEDURE IsConstructor (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ConstVarSym:  RETURN( ConstVar.IsConstructor ) |
      ConstLitSym:  RETURN( ConstLit.IsConstructor )

      ELSE
         RETURN( FALSE )
      END
   END
END IsConstructor ;


(*
   PutConstructorFrom - sets the from type field in constructor,
                        Sym, to, from.
*)

PROCEDURE PutConstructorFrom (Sym: CARDINAL; from: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ConstVarSym:  ConstVar.FromType := from ;
                    ConstVar.UnresFromType := TRUE |
      ConstLitSym:  ConstLit.FromType := from ;
                    ConstLit.UnresFromType := TRUE

      ELSE
         InternalError('expecting ConstVar or ConstLit symbol', __FILE__, __LINE__)
      END
   END ;
   IncludeItemIntoList(UnresolvedConstructorType, Sym)
END PutConstructorFrom ;


(*
   MakeSubrange - makes a new symbol into a subrange type with
                  name SubrangeName.
*)

PROCEDURE MakeSubrange (SubrangeName: Name) : CARDINAL ;
VAR
   sym,
   unbounded: CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare(SubrangeName, unbounded) ;
   IF NOT IsError(sym)
   THEN
      WITH Symbols[sym] DO
         SymbolType := SubrangeSym ;
         WITH Subrange DO
            name := SubrangeName ;
            Low := NulSym ;             (* Index to a symbol determining *)
                                        (* the lower bound of subrange.  *)
                                        (* Points to a constant -        *)
                                        (* possibly created by           *)
                                        (* ConstExpression.              *)
            High := NulSym ;            (* Index to a symbol determining *)
                                        (* the lower bound of subrange.  *)
                                        (* Points to a constant -        *)
                                        (* possibly created by           *)
                                        (* ConstExpression.              *)
            Type := NulSym ;            (* Index to a type. Determines   *)
                                        (* the type of subrange.         *)
            Size := InitValue() ;       (* Size determines the type size *)
            Unbounded := unbounded ;    (* The unbounded sym for this    *)
            Scope := GetCurrentScope() ;      (* Which scope created it  *)
            InitWhereDeclared(At)       (* Declared here                 *)
         END
      END
   END ;
   FillInUnboundedFields(unbounded, sym) ;
   RETURN( sym )
END MakeSubrange ;


(*
   MakeArray - makes an Array symbol with name ArrayName.
*)

PROCEDURE MakeArray (ArrayName: Name) : CARDINAL ;
VAR
   sym,
   unbounded: CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare(ArrayName, unbounded) ;
   IF NOT IsError(sym)
   THEN
      WITH Symbols[sym] DO
         SymbolType := ArraySym ;
         WITH Array DO
            name := ArrayName ;
            Subscript := NulSym ;   (* Contains the array subscripts.      *)
            Size := InitValue() ;   (* Size of array.                      *)
            Offset := InitValue() ; (* Offset of array.                    *)
            Type := NulSym ;        (* The Array Type. ARRAY OF Type.      *)
            Unbounded := unbounded ;(* The unbounded for this array        *)
            Scope := GetCurrentScope() ;        (* Which scope created it  *)
            InitWhereDeclared(At)   (* Declared here                       *)
         END
      END
   END ;
   FillInUnboundedFields(unbounded, sym) ;
   RETURN( sym )
END MakeArray ;


(*
   GetModule - Returns the Module symbol for the module with name, name.
*)

PROCEDURE GetModule (name: Name) : CARDINAL ;
BEGIN
   RETURN( GetSymKey(ModuleTree, name) )
END GetModule ;


(*
   GetLowestType - Returns the lowest type in the type chain of
                   symbol Sym.
                   If NulSym is returned then we assume type unknown.
*)

PROCEDURE GetLowestType (Sym: CARDINAL) : CARDINAL ;
VAR
   type: CARDINAL ;
BEGIN
   Assert(Sym#NulSym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarSym              : type := Var.Type |
      ConstLitSym         : type := ConstLit.Type |
      ConstVarSym         : type := ConstVar.Type |
      ConstStringSym      : type := NulSym |  (* No type for a string *)
      TypeSym             : type := Type.Type |
      RecordFieldSym      : type := RecordField.Type |
      RecordSym           : type := NulSym |  (* No type for a record *)
      EnumerationFieldSym : type := EnumerationField.Type |
      EnumerationSym      : type := NulSym |  (* No type for enumeration *)
      PointerSym          : type := Sym |     (* we don't go to Pointer.Type *)
      ProcedureSym        : type := Procedure.ReturnType |
      ProcTypeSym         : type := ProcType.ReturnType |
      ParamSym            : type := Param.Type |
      VarParamSym         : type := VarParam.Type |
      SubrangeSym         : type := Subrange.Type |
      ArraySym            : type := Array.Type |
      SubscriptSym        : type := Subscript.Type |
      SetSym              : type := Set.Type |
      UnboundedSym        : type := Unbounded.Type |
      UndefinedSym        : type := NulSym |
      DummySym            : type := NulSym

      ELSE
         InternalError('not implemented yet', __FILE__, __LINE__)
      END
   END ;
   IF (Symbols[Sym].SymbolType=TypeSym) AND (type=NulSym)
   THEN
      type := Sym             (* Base Type *)
   ELSIF type#NulSym
   THEN
      IF (IsType(type) OR IsSet(type))
      THEN
         (* ProcType is an inbuilt base type *)
         IF Symbols[type].SymbolType#ProcTypeSym
         THEN
            type := GetLowestType(type)   (* Type def *)
         END
      END
   END ;
   IF type>MaxSymbols
   THEN
      InternalError('type not declared', __FILE__, __LINE__)
   END ;
   RETURN( type )
END GetLowestType ;


(*
   GetType - Returns the symbol that is the TYPE symbol to Sym.
             If zero is returned then we assume type unknown.
*)

PROCEDURE GetType (Sym: CARDINAL) : CARDINAL ;
VAR
   type: CARDINAL ;
BEGIN
   Assert(Sym#NulSym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarSym              : type := Var.Type |
      ConstLitSym         : type := ConstLit.Type |
      ConstVarSym         : type := ConstVar.Type |
      ConstStringSym      : IF ConstString.Length=1
                            THEN
                               type := Char
                            ELSE
                               type := NulSym  (* No type for a string *)
                            END |
      TypeSym             : type := Type.Type |
      RecordFieldSym      : type := RecordField.Type |
      RecordSym           : type := NulSym |  (* No type for a record *)
      VarientSym          : type := NulSym |  (* No type for a record *)
      EnumerationFieldSym : type := EnumerationField.Type |
      EnumerationSym      : type := NulSym |  (* No type for enumeration *)
      PointerSym          : type := Pointer.Type |
      ProcedureSym        : type := Procedure.ReturnType |
      ProcTypeSym         : type := ProcType.ReturnType |
      ParamSym            : type := Param.Type |
      VarParamSym         : type := VarParam.Type |
      SubrangeSym         : type := Subrange.Type |
      ArraySym            : type := Array.Type |
      SubscriptSym        : type := Subscript.Type |
      SetSym              : type := Set.Type |
      UnboundedSym        : type := Unbounded.Type |
      UndefinedSym        : type := NulSym |
      PartialUnboundedSym : type := PartialUnbounded.Type |
      ObjectSym           : type := NulSym

      ELSE
         InternalError('not implemented yet', __FILE__, __LINE__)
      END
   END ;
   IF type>MaxSymbols
   THEN
      InternalError('type not declared', __FILE__, __LINE__)
   END ;
   RETURN( type )
END GetType ;


(*
   SkipType - if sym is a TYPE foo = bar
              then call SkipType(bar)
              else return sym

              it does not skip over hidden types.
*)

PROCEDURE SkipType (Sym: CARDINAL) : CARDINAL ;
BEGIN
   IF (Sym#NulSym) AND IsType(Sym) AND
      (NOT IsHiddenType(Sym)) AND (GetType(Sym)#NulSym)
   THEN
      RETURN( SkipType(GetType(Sym)) )
   ELSE
      RETURN( Sym )
   END
END SkipType ;


(*
   SkipTypeAndSubrange - if sym is a TYPE foo = bar OR
                            sym is declared as a subrange of bar
                         then call SkipTypeAndSubrange(bar)
                         else return sym

                         it does not skip over hidden types.
*)

PROCEDURE SkipTypeAndSubrange (Sym: CARDINAL) : CARDINAL ;
BEGIN
   IF (Sym#NulSym) AND (IsType(Sym) OR IsSubrange(Sym)) AND
      (NOT IsHiddenType(Sym)) AND (GetType(Sym)#NulSym)
   THEN
      RETURN( SkipType(GetType(Sym)) )
   ELSE
      RETURN( Sym )
   END
END SkipTypeAndSubrange ;


(*
   IsHiddenType - returns TRUE if, Sym, is a Type and is also declared as a hidden type.
*)

PROCEDURE IsHiddenType (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      TypeSym:  RETURN( Type.IsHidden )

      ELSE
         RETURN( FALSE )
      END
   END
END IsHiddenType ;


(*
   GetConstLitType - returns the type of the constant, Sym.
                     All floating point constants have type LONGREAL.
                     Character constants are type CHAR.
                     Integer values are INTEGER, LONGINT or LONGCARD
                     depending upon their value.
*)

PROCEDURE GetConstLitType (Sym: CARDINAL) : CARDINAL ;
VAR
   s            : String ;
   i, High      : CARDINAL ;
   needsLong,
   needsUnsigned: INTEGER ;
BEGIN
   s := InitStringCharStar(KeyToCharStar(GetSymName(Sym))) ;
   IF char(s, -1)='C'
   THEN
      s := KillString(s) ;
      RETURN( Char )
   ELSE
      IF Index(s, '.', 0)#-1   (* found a '.' in our constant *)
      THEN
         s := KillString(s) ;
         RETURN( RType )
      END ;
      CASE char(s, -1) OF

      'H':  DetermineSizeOfConstant(string(s), 16,
                                    needsLong, needsUnsigned) |
      'B':  DetermineSizeOfConstant(string(s), 8,
                                    needsLong, needsUnsigned) |
      'A':  DetermineSizeOfConstant(string(s), 2,
                                    needsLong, needsUnsigned)

      ELSE
         DetermineSizeOfConstant(string(s), 10,
                                 needsLong, needsUnsigned)
      END ;
      s := KillString(s) ;
      IF (needsLong=1) AND (needsUnsigned=1)
      THEN
         RETURN( LongCard )
      ELSIF (needsLong=1) AND (needsUnsigned=0)
      THEN
         RETURN( LongInt )
      END ;
      RETURN( ZType )
   END
END GetConstLitType ;


(*
   GetLocalSym - only searches the scope Sym for a symbol with name
                 and returns the index to the symbol.
*)

PROCEDURE GetLocalSym (Sym: CARDINAL; name: Name) : CARDINAL ;
VAR
   LocalSym: CARDINAL ;
BEGIN
   (*
   WriteString('Attempting to retrieve symbol from ') ; WriteKey(GetSymName(Sym)) ;
   WriteString(' local symbol table') ; WriteLn ;
   *)
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      EnumerationSym : LocalSym := GetSymKey(Enumeration.LocalSymbols, name) |
      RecordSym      : LocalSym := GetSymKey(Record.LocalSymbols, name) |
      ProcedureSym   : LocalSym := GetSymKey(Procedure.LocalSymbols, name) |
      ModuleSym      : LocalSym := GetSymKey(Module.LocalSymbols, name) |
      DefImpSym      : LocalSym := GetSymKey(DefImp.LocalSymbols, name)

      ELSE
         InternalError('symbol does not have a LocalSymbols field', __FILE__, __LINE__)
      END
   END ;
   RETURN( LocalSym )
END GetLocalSym ;


(*
   GetNth - returns the n th symbol in the list of father Sym.
            Sym may be a Module, DefImp, Procedure or Record symbol.
*)

PROCEDURE GetNth (Sym: CARDINAL; n: CARDINAL) : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      RecordSym       : i := GetItemFromList(Record.ListOfSons, n) |
      VarientSym      : i := GetItemFromList(Varient.ListOfSons, n) |
      VarientFieldSym : i := GetItemFromList(VarientField.ListOfSons, n) |
      ProcedureSym    : i := GetItemFromList(Procedure.ListOfVars, n) |
      DefImpSym       : i := GetItemFromList(DefImp.ListOfVars, n) |
      ModuleSym       : i := GetItemFromList(Module.ListOfVars, n) |
      TupleSym        : i := GetFromIndex(Tuple.list, n)

      ELSE
         InternalError('cannot GetNth from this symbol', __FILE__, __LINE__)
      END
   END ;
   RETURN( i )
END GetNth ;


(*
   GetNthParam - returns the n th parameter of a procedure Sym.
*)

PROCEDURE GetNthParam (Sym: CARDINAL; ParamNo: CARDINAL) : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   IF ParamNo=0
   THEN
      (* Demands the return type of the function *)
      i := GetType(Sym)
   ELSE
      WITH Symbols[Sym] DO
         CASE SymbolType OF

         ProcedureSym: i := GetItemFromList(Procedure.ListOfParam, ParamNo) |
         ProcTypeSym : i := GetItemFromList(ProcType.ListOfParam, ParamNo)

         ELSE
            InternalError('expecting ProcedureSym or ProcTypeSym', __FILE__, __LINE__)
         END
      END
   END ;
   RETURN( i )
END GetNthParam ;


(*
   The Following procedures fill in the symbol table with the
   symbol entities.
*)

(*
   PutVar - gives the VarSym symbol Sym a type Type.
*)

PROCEDURE PutVar (Sym: CARDINAL; VarType: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarSym     : Var.Type := VarType |
      ConstVarSym: ConstVar.Type := VarType

      ELSE
         InternalError('expecting VarSym or ConstVarSym', __FILE__, __LINE__)
      END
   END
END PutVar ;


(*
   PutLeftValueFrontBackType - gives the variable symbol a front and backend type.
                               The variable must be a LeftValue.
*)

PROCEDURE PutLeftValueFrontBackType (Sym: CARDINAL; FrontType, BackType: CARDINAL) ;
BEGIN
   Assert(GetMode(Sym)=LeftValue) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarSym : Var.Type := FrontType ;
               Var.BackType := BackType ;
               PushSize(Address) ;
               PopInto(Var.Size)

      ELSE
         InternalError('expecting VarSym', __FILE__, __LINE__)
      END
   END
END PutLeftValueFrontBackType ;


(*
   GetVarBackEndType - returns the back end type if specified.
*)

PROCEDURE GetVarBackEndType (Sym: CARDINAL) : CARDINAL ;
BEGIN
   Assert(Sym#NulSym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarSym: RETURN( Var.BackType )

      ELSE
         RETURN( NulSym )
      END
   END
END GetVarBackEndType ;


(*
   PutVarPointerCheck - marks variable, sym, as requiring (or not
                        depending upon the, value), a NIL pointer check
                        when this symbol is dereferenced.
*)

PROCEDURE PutVarPointerCheck (sym: CARDINAL; value: BOOLEAN) ;
BEGIN
   IF IsVar(sym)
   THEN
      WITH Symbols[sym].Var DO
         IsPointerCheck := value
      END
   END
END PutVarPointerCheck ;


(*
   GetVarPointerCheck - returns TRUE if this symbol is a variable and
                        has been marked as needing a pointer via NIL check.
*)

PROCEDURE GetVarPointerCheck (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsVar(sym)
   THEN
      WITH Symbols[sym].Var DO
         RETURN( IsPointerCheck )
      END
   END
END GetVarPointerCheck ;


(*
   PutVarWritten - marks variable, sym, as being written to (or not
                   depending upon the, value).
*)

PROCEDURE PutVarWritten (sym: CARDINAL; value: BOOLEAN) ;
BEGIN
   IF IsVar(sym)
   THEN
      WITH Symbols[sym].Var DO
         IsWritten := value
      END
   END
END PutVarWritten ;


(*
   GetVarWritten - returns TRUE if this symbol is a variable and
                   has been marked as being written.
*)

PROCEDURE GetVarWritten (sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      VarSym: RETURN( Var.IsWritten )

      ELSE
         InternalError('expecting VarSym', __FILE__, __LINE__)
      END
   END
END GetVarWritten ;


(*
   PutConst - gives the constant symbol Sym a type ConstType.
*)

PROCEDURE PutConst (Sym: CARDINAL; ConstType: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE Symbols[Sym].SymbolType OF

      ConstVarSym: ConstVar.Type := ConstType

      ELSE
         InternalError('expecting ConstVarSym', __FILE__, __LINE__)
      END
   END
END PutConst ;


(*
   PutFieldRecord - places a field, FieldName and FieldType into a record, Sym.
                    VarSym is a optional varient symbol which can be returned
                    by a call to GetVarient(fieldsymbol).
*)

PROCEDURE PutFieldRecord (Sym: CARDINAL;
                          FieldName: Name; FieldType: CARDINAL;
                          VarSym: CARDINAL) ;
VAR
   ParSym,
   SonSym: CARDINAL ;
BEGIN
   NewSym(SonSym) ; (* Cannot be used before declared since use occurs *)
                    (* in pass 3 and it will be declared in pass 2.    *)
   (* Fill in the SonSym and connect it to its brothers (if any) and   *)
   (* ensure that it is connected its parent.                          *)
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      RecordSym       : WITH Record DO
                           PutItemIntoList(ListOfSons, SonSym) ;
                           (* Ensure that the Field is in the Parents Local Symbols *)
                           PutSymKey(LocalSymbols, FieldName, SonSym)
                        END |
      VarientFieldSym : WITH VarientField DO
                           PutItemIntoList(ListOfSons, SonSym) ;
                           ParSym := Parent
                        END ;
                        Assert(Symbols[ParSym].SymbolType=RecordSym) ;
                        PutSymKey(Symbols[ParSym].Record.LocalSymbols, FieldName, SonSym)

(* is the same as below, but -pedantic warns against having nested WITH statements referencing the same type
   (I've been burnt by this before, so I respect -pedantic warnings..)

                        WITH Symbols[ParSym] DO
                        (* Ensure that the Field is in the parents Local Symbols *)
                           CASE SymbolType OF

                           RecordSym: PutSymKey(Record.LocalSymbols, FieldName, SonSym)
                           END
                        END
*)


      ELSE
         InternalError('expecting Record symbol', __FILE__, __LINE__)
      END    
   END ;
   (* Fill in SonSym *)
   WITH Symbols[SonSym] DO
      SymbolType := RecordFieldSym ;
      WITH RecordField DO
         Type := FieldType ;
         name := FieldName ;
         Parent := GetRecord(Sym) ;
         Varient := VarSym ;
         Size := InitValue() ;
         Offset := InitValue()
      END
   END
END PutFieldRecord ;


(*
   MakeFieldVarient - returns a FieldVarient symbol which has been
                      assigned to the Varient symbol, Sym.
*)

PROCEDURE MakeFieldVarient (n: Name; Sym: CARDINAL) : CARDINAL ;
VAR
   SonSym: CARDINAL ;
BEGIN
   IF NoOfItemsInList(FreeFVarientList)=0
   THEN
      NewSym(SonSym)
   ELSE
      SonSym := GetItemFromList(FreeFVarientList, 1) ;
      RemoveItemFromList(FreeFVarientList, SonSym)
   END ;
   (* Fill in Sym *)
   WITH Symbols[SonSym] DO
      SymbolType := VarientFieldSym ;
      WITH VarientField DO
         name := n ;
         InitList(ListOfSons) ;
         Parent := GetRecord(Sym) ;
         Varient := NulSym ;
         Size := InitValue() ;
         Offset := InitValue() ;
         Scope := GetCurrentScope() ;
         InitWhereDeclared(At)
      END
   END ;
   RETURN( SonSym )
END MakeFieldVarient ;


(*
   PutFieldVarient - places the field varient, Field, as a brother to, the
                     varient symbol, sym, and also tells Field that its varient
                     parent is Sym.
*)

PROCEDURE PutFieldVarient (Field, Sym: CARDINAL) ;
VAR
   SonSym: CARDINAL ;
BEGIN
   Assert(IsVarient(Sym)) ;
   Assert(IsFieldVarient(Field)) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarientSym : IncludeItemIntoList(Varient.ListOfSons, Field)

      ELSE
         InternalError('expecting Varient symbol', __FILE__, __LINE__)
      END    
   END ;
   WITH Symbols[Field] DO
      CASE SymbolType OF

      VarientFieldSym : VarientField.Varient := Sym

      ELSE
         InternalError('expecting VarientField symbol', __FILE__, __LINE__)
      END    
   END ;
   PutItemIntoList(UsedFVarientList, Field)
END PutFieldVarient ;


(*
   GetVarient - returns the varient symbol associated with the
                record or varient field symbol, Field.
*)

PROCEDURE GetVarient (Field: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[Field] DO
      CASE SymbolType OF

      VarientFieldSym : RETURN( VarientField.Varient ) |
      RecordFieldSym  : RETURN( RecordField.Varient ) |
      VarientSym      : RETURN( Varient.Varient )

      ELSE
         RETURN( NulSym )
      END    
   END
END GetVarient ;


(*
   GCFieldVarient - garbage collect the field varient symbol, Sym.
                    This must only be called once per Sym.
*)

PROCEDURE GCFieldVarient (Sym: CARDINAL) ;
BEGIN
   IF IsItemInList(UsedFVarientList, Sym)
   THEN
      RemoveItemFromList(UsedFVarientList, Sym)
   ELSE
      RemoveItemFromList(UsedFVarientList, Sym) ;
      PutItemIntoList(FreeFVarientList, Sym)
   END
END GCFieldVarient ;


(*
   IsFieldVarient - returns true if the symbol, Sym, is a
                    varient field.
*)

PROCEDURE IsFieldVarient (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Symbols[Sym].SymbolType=VarientFieldSym )
END IsFieldVarient ;


(*
   IsFieldEnumeration - returns true if the symbol, Sym, is an
                        enumeration field.
*)

PROCEDURE IsFieldEnumeration (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Symbols[Sym].SymbolType=EnumerationFieldSym )
END IsFieldEnumeration ;


(*
   IsVarient - returns true if the symbol, Sym, is a
               varient symbol.
*)

PROCEDURE IsVarient (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Symbols[Sym].SymbolType=VarientSym )
END IsVarient ;


(*
   PutFieldEnumeration - places a field into the enumeration type
                         Sym. The field has a name FieldName and a
                         value FieldVal.
*)

PROCEDURE PutFieldEnumeration (Sym: CARDINAL; FieldName: Name) ;
VAR
   s    : String ;
   Field: CARDINAL ;
BEGIN
   Field := CheckForHiddenType(FieldName) ;
   IF Field=NulSym
   THEN
      Field := DeclareSym(FieldName)
   END ;
   IF NOT IsError(Field)
   THEN
      WITH Symbols[Field] DO
         SymbolType := EnumerationFieldSym ;
         WITH EnumerationField DO
            name := FieldName ;  (* Index into name array, name *)
                                 (* of type.                    *)
            PushCard(Symbols[Sym].Enumeration.NoOfElements) ;
            Value := InitValue() ;
            PopInto(Value) ;
            Type := Sym ;
            Scope := GetCurrentScope() ;
            InitWhereDeclared(At)  (* Declared here *)
         END
      END ;
      WITH Symbols[Sym] DO
         CASE SymbolType OF

         EnumerationSym: WITH Enumeration DO
                            INC(NoOfElements) ;
                            IF GetSymKey(LocalSymbols, FieldName)#NulSym
                            THEN
                               s := Mark(InitStringCharStar(KeyToCharStar(FieldName))) ;
                               AlreadyDeclaredError(Sprintf1(Mark(InitString('enumeration field (%s) is already declared elsewhere, use a different name or remove the declaration')), s),
                                                    FieldName,
                                                    GetDeclared(GetSymKey(LocalSymbols, FieldName)))
                            ELSE
                               PutSymKey(LocalSymbols, FieldName, Field)
                            END
                         END

         ELSE
            InternalError('expecting Sym=Enumeration', __FILE__, __LINE__)
         END
      END
   END
END PutFieldEnumeration ;


(*
   PutType - gives a type symbol Sym type TypeSymbol.
*)

PROCEDURE PutType (Sym: CARDINAL; TypeSymbol: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: |
      TypeSym : Type.Type := TypeSymbol

      ELSE
         InternalError('expecting a Type symbol', __FILE__, __LINE__)
      END
   END
END PutType ;


(*
   IsDefImp - returns true is the Sym is a DefImp symbol.
              Definition/Implementation module symbol.
*)

PROCEDURE IsDefImp (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Symbols[Sym].SymbolType=DefImpSym )
END IsDefImp ;


(*
   IsModule - returns true is the Sym is a Module symbol.
              Program module symbol.
*)

PROCEDURE IsModule (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Symbols[Sym].SymbolType=ModuleSym )
END IsModule ;


(*
   IsInnerModule - returns true if the symbol, Sym, is an inner module.
*)

PROCEDURE IsInnerModule (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsModule(Sym)
   THEN
      RETURN( GetScope(Sym)#NulSym )
   ELSE
      RETURN( FALSE )
   END
END IsInnerModule ;


(*
   GetSymName - returns the symbol name.
*)

PROCEDURE GetSymName (Sym: CARDINAL) : Name ;
VAR
   n: Name ;
BEGIN
   IF Sym=NulSym
   THEN
      n := NulKey
   ELSE
      WITH Symbols[Sym] DO
         CASE SymbolType OF

         ErrorSym            : n := Error.name |
         ObjectSym           : n := Object.name |
         DefImpSym           : n := DefImp.name |
         ModuleSym           : n := Module.name |
         TypeSym             : n := Type.name |
         VarSym              : n := Var.name |
         ConstLitSym         : n := ConstLit.name |
         ConstVarSym         : n := ConstVar.name |
         ConstStringSym      : n := ConstString.name |
         EnumerationSym      : n := Enumeration.name |
         EnumerationFieldSym : n := EnumerationField.name |
         UndefinedSym        : n := Undefined.name |
         ProcedureSym        : n := Procedure.name |
         ProcTypeSym         : n := ProcType.name |
         RecordFieldSym      : n := RecordField.name |
         RecordSym           : n := Record.name |
         VarientSym          : n := NulName |
         VarientFieldSym     : n := VarientField.name |
         VarParamSym         : n := VarParam.name |
         ParamSym            : n := Param.name |
         PointerSym          : n := Pointer.name |
         ArraySym            : n := Array.name |
         UnboundedSym        : n := NulName |
         SubrangeSym         : n := Subrange.name |
      	 SetSym              : n := Set.name |
         SubscriptSym        : n := NulName |
         DummySym            : n := NulName |
         PartialUnboundedSym : n := GetSymName(PartialUnbounded.Type) |
         TupleSym            : n := NulName

         ELSE
            InternalError('unexpected symbol type', __FILE__, __LINE__)
         END
      END
   END ;
   RETURN( n )
END GetSymName ;


(*
   PutConstVarTemporary - indicates that constant, sym, is a temporary.
*)

PROCEDURE PutConstVarTemporary (sym: CARDINAL) ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      ConstVarSym:  ConstVar.IsTemp := TRUE

      ELSE
         InternalError('expecting a Var symbol', __FILE__, __LINE__)
      END
   END
END PutConstVarTemporary ;


(*
   MakeTemporary - Makes a new temporary variable at the highest real scope.
                   The addressing mode of the temporary is set to NoValue.
*)

PROCEDURE MakeTemporary (Mode: ModeOfAddr) : CARDINAL ;
VAR
   s  : String ;
   Sym: CARDINAL ;
BEGIN
   INC(TemporaryNo) ;
   (* Make the name *)
   s := Sprintf1(Mark(InitString('_T%d')), TemporaryNo) ;
   IF Mode=ImmediateValue
   THEN
      Sym := MakeConstVar(makekey(string(s))) ;
      PutConstVarTemporary(Sym)
   ELSE
      Sym := MakeVar(makekey(string(s))) ;
      WITH Symbols[Sym] DO
         CASE SymbolType OF

         VarSym : Var.AddrMode := Mode ;
                  Var.IsTemp := TRUE ;       (* Variable is a temporary var *)
                  InitWhereDeclared(Var.At)  (* Declared here               *)

         ELSE
            InternalError('expecting a Var symbol', __FILE__, __LINE__)
         END
      END
   END ;
   s := KillString(s) ;
   RETURN( Sym )
END MakeTemporary ;


(*
   MakeTemporaryFromExpressions - makes a new temporary variable at the
                                  highest real scope.  The addressing
                                  mode of the temporary is set and the
                                  type is determined by expressions,
                                  e1 and e2.
*)

PROCEDURE MakeTemporaryFromExpressions (e1, e2: CARDINAL;
                                        tok: CARDINAL;
                                        mode: ModeOfAddr) : CARDINAL ;
VAR
   s  : String ;
   t,
   Sym: CARDINAL ;
BEGIN
   INC(TemporaryNo) ;
   (* Make the name *)
   s := Sprintf1(Mark(InitString('_T%d')), TemporaryNo) ;
   IF mode=ImmediateValue
   THEN
      Sym := MakeConstVar(makekey(string(s))) ;
      IF IsConstructor(e1)
      THEN
         PutConstructor(Sym) ;
         PutConstructorFrom(Sym, e1)
      ELSIF IsConstructor(e2)
      THEN
         PutConstructor(Sym) ;
         PutConstructorFrom(Sym, e2)
      ELSE
         PutVar(Sym, MixTypes(GetType(e1), GetType(e2), tok))
      END ;
      PutConstVarTemporary(Sym)
   ELSE
      Sym := MakeVar(makekey(string(s))) ;
      WITH Symbols[Sym] DO
         CASE SymbolType OF

         VarSym : Var.AddrMode := mode ;
                  Var.IsTemp := TRUE ;       (* Variable is a temporary var *)
                  InitWhereDeclared(Var.At)  (* Declared here               *)

         ELSE
            InternalError('expecting a Var symbol', __FILE__, __LINE__)
         END
      END ;
      t := MixTypes(GetType(e1), GetType(e2), tok) ;
      IF t#NulSym
      THEN
         Assert(NOT IsConstructor(t)) ;
         PutVar(Sym, t)
      END
   END ;
   s := KillString(s) ;
   RETURN( Sym )
END MakeTemporaryFromExpressions ;


(*
   MakeTemporaryFromExpression - makes a new temporary variable at the
                                 highest real scope.  The addressing
                                 mode of the temporary is set and the
                                 type is determined by expressions, e.
*)

PROCEDURE MakeTemporaryFromExpression (e: CARDINAL;
                                       tok: CARDINAL;
                                       mode: ModeOfAddr) : CARDINAL ;
BEGIN
   RETURN( MakeTemporaryFromExpressions(e, e, tok, mode) )
END MakeTemporaryFromExpression ;


(*
   PutMode - Puts the addressing mode, SymMode, into symbol Sym.
             The mode may only be altered if the mode
             is None.
*)

PROCEDURE PutMode (Sym: CARDINAL; SymMode: ModeOfAddr) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: |
      VarSym  : Var.AddrMode := SymMode

      ELSE
         InternalError('Expecting VarSym', __FILE__, __LINE__)
      END
   END
END PutMode ;


(*
   GetMode - Returns the addressing mode of a symbol.
*)

PROCEDURE GetMode (Sym: CARDINAL) : ModeOfAddr ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym           : ErrorAbort0('') |
      VarSym             : RETURN( Var.AddrMode ) |
      ConstLitSym        : RETURN( ImmediateValue ) |
      ConstVarSym        : RETURN( ImmediateValue ) |
      ConstStringSym     : RETURN( ImmediateValue ) |
      EnumerationFieldSym: RETURN( ImmediateValue ) |
      ProcedureSym       : RETURN( ImmediateValue ) |
      RecordFieldSym     : RETURN( ImmediateValue ) |
      VarientFieldSym    : RETURN( ImmediateValue ) |
      TypeSym            : RETURN( NoValue ) |
      ArraySym           : RETURN( NoValue ) |
      SubrangeSym        : RETURN( NoValue ) |
      EnumerationSym     : RETURN( NoValue ) |
      RecordSym          : RETURN( NoValue ) |
      PointerSym         : RETURN( NoValue ) |
      SetSym             : RETURN( NoValue ) |
      ProcTypeSym        : RETURN( NoValue ) |
      UnboundedSym       : RETURN( NoValue ) |
      UndefinedSym       : RETURN( NoValue )

      ELSE
         InternalError('not expecting this type', __FILE__, __LINE__)
      END
   END
END GetMode ;


(*
   RenameSym - renames a symbol, Sym, with SymName.
               It also checks the unknown tree for a symbol
               with this new name. Must only be renamed in
               the same scope of being declared.
*)

PROCEDURE RenameSym (Sym: CARDINAL; SymName: Name) ;
BEGIN
   IF GetSymName(Sym)=NulName
   THEN
      WITH Symbols[Sym] DO
         CASE SymbolType OF

         ErrorSym            : ErrorAbort0('') |
         TypeSym             : Type.name      := SymName |
         VarSym              : Var.name       := SymName |
         ConstLitSym         : ConstLit.name  := SymName |
         ConstVarSym         : ConstVar.name  := SymName |
         UndefinedSym        : Undefined.name := SymName |
         RecordSym           : Record.name    := SymName |
         PointerSym          : Pointer.name   := SymName

         ELSE
            InternalError('not implemented yet', __FILE__, __LINE__)
         END
      END ;
      AddSymToScope(Sym, SymName)
   ELSE
      InternalError('old name of symbol must be nul', __FILE__, __LINE__)
   END
END RenameSym ;


(*
   IsUnknown - returns true is the symbol Sym is unknown.
*)

PROCEDURE IsUnknown (Sym: WORD) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN( Symbols[Sym].SymbolType=UndefinedSym )
END IsUnknown ;


(*
   CheckLegal - determines whether the Sym is a legal symbol.
*)

PROCEDURE CheckLegal (Sym: CARDINAL) ;
BEGIN
   IF (Sym<1) OR (Sym>FinalSymbol())
   THEN
      InternalError('illegal symbol', __FILE__, __LINE__)
   END
END CheckLegal ;


(*
   CheckForHiddenType - scans the NeedToBeImplemented tree providing
                        that we are currently compiling an implementation
                        module. If a symbol is found with TypeName
                        then its Sym is returned.
                        Otherwise NulSym is returned.
                        CheckForHiddenType is called before any type is
                        created, therefore the compiler allows hidden
                        types to be implemented using any type.
*)

PROCEDURE CheckForHiddenType (TypeName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := NulSym ;
   IF CompilingImplementationModule() AND
      IsDefImp(CurrentModule) AND
      IsHiddenTypeDeclared(CurrentModule) AND
      (TypeName#NulName)
   THEN
      (* Check to see whether we are declaring a HiddenType. *)
      WITH Symbols[CurrentModule] DO
         CASE SymbolType OF

         DefImpSym: Sym := GetSymKey(DefImp.NeedToBeImplemented, TypeName)

         ELSE
            InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
         END
      END
   END ;
   RETURN( Sym )
END CheckForHiddenType ;


(*
   IsReallyPointer - returns TRUE is sym is a pointer, address or a
                     type declared as a pointer or address.
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
   SkipHiddenType - if sym is a TYPE foo = bar
                    then call SkipType(bar)
                    else return sym

                    it does skip over hidden type.
*)

PROCEDURE SkipHiddenType (Sym: CARDINAL) : CARDINAL ;
BEGIN
   IF (Sym#NulSym) AND IsType(Sym) AND (GetType(Sym)#NulSym)
   THEN
      RETURN( SkipType(GetType(Sym)) )
   ELSE
      RETURN( Sym )
   END
END SkipHiddenType ;


(*
   IsHiddenReallyPointer - returns TRUE is sym is a pointer, address or a
                           type declared as a pointer or address.
*)

PROCEDURE IsHiddenReallyPointer (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsVar(Sym)
   THEN
      Sym := GetType(Sym)
   END ;
   Sym := SkipHiddenType(Sym) ;
   RETURN( IsPointer(Sym) OR (Sym=Address) )
END IsHiddenReallyPointer ;


(*
   CheckHiddenTypeAreAddress - checks to see that any hidden types
                               which we have declared are actually
                               of type ADDRESS or map onto a POINTER type.
*)

PROCEDURE CheckHiddenTypeAreAddress ;
VAR
   name: Name ;
   e   : Error ;
   sym,
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := NoOfItemsInList(AddressTypes) ;
   WHILE i<=n DO
      sym := GetItemFromList(AddressTypes, i) ;
      IF NOT IsHiddenReallyPointer(sym)
      THEN
         name := GetSymName(sym) ;
         e := NewError(GetDeclared(sym)) ;
         ErrorFormat1(e, 'opaque type (%a) should be equivalent to a POINTER or an ADDRESS', name) ;
         e := NewError(GetDeclared(sym)) ;
         ErrorFormat0(e, 'if you really need a non POINTER type use the -fextended-opaque switch')
      END ;
      INC(i)
   END
END CheckHiddenTypeAreAddress ;


(*
   GetLastMainScopeId - returns the, id, containing the last main scope.
*)

PROCEDURE GetLastMainScopeId (id: CARDINAL) : CARDINAL ;
VAR
   sym: CARDINAL ;
BEGIN
   IF id>0
   THEN
      sym := ScopeCallFrame[id].Main ;
      WHILE id>1 DO
         DEC(id) ;
         IF sym#ScopeCallFrame[id].Main
         THEN
            RETURN( id )
         END
      END
   END ;
   RETURN( 0 )
END GetLastMainScopeId ;



(*
   RequestSym - searches for a symbol with a name SymName in the
                current and previous scopes.
                If the symbol is found then it is returned
                else an unknown symbol is returned.
*)

PROCEDURE RequestSym (SymName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   (*
      WriteString('RequestSym for: ') ; WriteKey(SymName) ; WriteLn ;
   *)
   Sym := GetSym(SymName) ;
   IF Sym=NulSym
   THEN
      Sym := GetSymFromUnknownTree(SymName) ;
      IF Sym=NulSym
      THEN
         (* Make unknown *)
         NewSym(Sym) ;
         FillInUnknownFields(Sym, SymName) ;
         (* Add to unknown tree *)
         AddSymToUnknownTree(ScopePtr, SymName, Sym)
         (*
           ; WriteKey(SymName) ; WriteString(' unknown demanded') ; WriteLn
         *)
      END
   END ;
   RETURN( Sym )
END RequestSym ;


(*
   PutImported - places a symbol, Sym, into the current main scope.
*)

PROCEDURE PutImported (Sym: CARDINAL) ;
VAR
   ModSym: CARDINAL ;
   n     : Name ;
BEGIN
   (*
      We have currently imported Sym, now place it into the current module.
   *)
   ModSym := GetCurrentModuleScope() ;
   Assert(IsDefImp(ModSym) OR IsModule(ModSym)) ;
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      ModuleSym: IF GetSymKey(Module.ImportTree, GetSymName(Sym))=Sym
                 THEN
                    IF Pedantic
                    THEN
                       n := GetSymName(Sym) ;
                       WriteFormat1('symbol (%a) has already been imported', n)
                    END
                 ELSIF GetSymKey(Module.ImportTree, GetSymName(Sym))=NulKey
                 THEN
                    IF GetSymKey(Module.WhereImported, Sym)=NulKey
                    THEN
                       PutSymKey(Module.WhereImported, Sym, GetTokenNo())
                    END ;
                    PutSymKey(Module.ImportTree, GetSymName(Sym), Sym) ;
                    AddSymToModuleScope(ModSym, Sym)
                 ELSE
                    n := GetSymName(Sym) ;
                    WriteFormat1('name clash when trying to import (%a)', n)
                 END |
      DefImpSym: IF GetSymKey(DefImp.ImportTree, GetSymName(Sym))=Sym
                 THEN
                    IF Pedantic
                    THEN
                       n := GetSymName(Sym) ;
                       WriteFormat1('symbol (%a) has already been imported', n)
                    END
                 ELSIF GetSymKey(DefImp.ImportTree, GetSymName(Sym))=NulKey
                 THEN
                    IF GetSymKey(DefImp.WhereImported, Sym)=NulKey
                    THEN
                       PutSymKey(DefImp.WhereImported, Sym, GetTokenNo())
                    END ;
                    PutSymKey(DefImp.ImportTree, GetSymName(Sym), Sym) ;
                    AddSymToModuleScope(ModSym, Sym)
                 ELSE
                    n := GetSymName(Sym) ;
                    WriteFormat1('name clash when trying to import (%a)', n)
                 END

      ELSE
         InternalError('expecting a Module or DefImp symbol', __FILE__, __LINE__)
      END
   END
END PutImported ;


(*
   PutIncluded - places a symbol, Sym, into the included list of the
                 current module.
                 Symbols that are placed in this list are indirectly declared
                 by:

                 IMPORT modulename ;

                 modulename.identifier
*)

PROCEDURE PutIncluded (Sym: CARDINAL) ;
VAR
   ModSym: CARDINAL ;
   n1, n2: Name ;
BEGIN
   (*
      We have referenced Sym, via modulename.Sym
      now place it into the current module include list.
   *)
   ModSym := GetCurrentModuleScope() ;
   Assert(IsDefImp(ModSym) OR IsModule(ModSym)) ;
   IF DebugUnknowns
   THEN
      n1 := GetSymName(Sym) ;
      n2 := GetSymName(ModSym) ;
      printf2('including %a into scope %a\n', n1, n2)
   END ;
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      ModuleSym: IncludeItemIntoList(Module.IncludeList, Sym) |
      DefImpSym: IncludeItemIntoList(DefImp.IncludeList, Sym)

      ELSE
         InternalError('expecting a Module or DefImp symbol', __FILE__, __LINE__)
      END
   END
END PutIncluded ;


(*
   PutExported - places a symbol, Sym into the next level out module.
                 Sym is also placed in the ExportTree of the current inner
                 module.
*)

PROCEDURE PutExported (Sym: CARDINAL) ;
BEGIN
(*
   WriteString('PutExported') ; WriteLn ;
*)
   AddSymToModuleScope(GetLastModuleOrProcedureScope(), Sym) ;
   WITH Symbols[GetCurrentModuleScope()] DO
      CASE SymbolType OF

      ModuleSym: PutSymKey(Module.ExportTree, GetSymName(Sym), Sym) ;
                 IF IsUnknown(Sym)
                 THEN
                    PutExportUndeclared(GetCurrentModuleScope(), Sym)
                 END
(*
                 ; WriteKey(Module.name) ; WriteString(' exports ') ;
                 ; WriteKey(GetSymName(Sym)) ; WriteLn ;
*)

      ELSE
         InternalError('expecting a Module symbol', __FILE__, __LINE__)
      END
   END
END PutExported ;


(*
   PutExportQualified - places a symbol with the name, SymName,
                        into the export tree of the
                        Definition module being compiled.
                        The symbol with name has been EXPORT QUALIFIED
                        by the definition module and therefore any reference
                        to this symbol in the code generation phase
                        will be in the form _Module_Name.
*)

PROCEDURE PutExportQualified (SymName: Name) ;
VAR
   n     : Name ;
   Sym,
   ModSym: CARDINAL ;
BEGIN
   ModSym := GetCurrentModule() ;
   Assert(IsDefImp(ModSym)) ;
   Assert(CompilingDefinitionModule()) ;
(* printf2('module %a exporting %a\n', GetSymName(ModSym), SymName) ; *)
(*
   WriteString('1st MODULE ') ; WriteKey(GetSymName(ModSym)) ;
   WriteString(' identifier ') ; WriteKey(SymName) ; WriteLn ;
*)
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    IF (GetSymKey(ExportQualifiedTree, SymName)#NulKey) AND
                       (GetSymKey(ExportRequest, SymName)=NulKey)
                    THEN
                       n := GetSymName(ModSym) ;
                       WriteFormat2('identifier (%a) has already been exported from MODULE %a',
                                    SymName, n)
                    ELSIF GetSymKey(ExportRequest, SymName)#NulKey
                    THEN
                       Sym := GetSymKey(ExportRequest, SymName) ;
                       DelSymKey(ExportRequest, SymName) ;
                       PutSymKey(ExportQualifiedTree, SymName, Sym) ;
                       PutExportUndeclared(ModSym, Sym)
                    ELSE
                       Sym := RequestSym(SymName) ;
                       PutSymKey(ExportQualifiedTree, SymName, Sym) ;
                       PutExportUndeclared(ModSym, Sym)
                    END
                 END

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END
END PutExportQualified ;


(*
   PutExportUnQualified - places a symbol with the name, SymName,
                          into the export tree of the
                          Definition module being compiled.
                          The symbol with Name has been EXPORT UNQUALIFIED
                          by the definition module and therefore any reference
                          to this symbol in the code generation phase
                          will be in the form _Name.
*)

PROCEDURE PutExportUnQualified (SymName: Name) ;
VAR
   n     : Name ;
   Sym,
   ModSym: CARDINAL ;
BEGIN
   ModSym := GetCurrentModule() ;
   Assert(IsDefImp(ModSym)) ;
   Assert(CompilingDefinitionModule()) ;
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    IF (GetSymKey(ExportUnQualifiedTree, SymName)#NulKey) AND
                       (GetSymKey(ExportRequest, SymName)=NulKey)
                    THEN
                       n := GetSymName(ModSym) ;
                       WriteFormat2('identifier (%a) has already been exported from MODULE %a',
                                    SymName, n)
                    ELSIF GetSymKey(ExportRequest, SymName)#NulKey
                    THEN
                       Sym := GetSymKey(ExportRequest, SymName) ;
                       DelSymKey(ExportRequest, SymName) ;
                       PutSymKey(ExportUnQualifiedTree, SymName, Sym) ;
                       PutExportUndeclared(ModSym, Sym)
                    ELSE
                       Sym := RequestSym(SymName) ;
                       PutSymKey(ExportUnQualifiedTree, SymName, Sym) ;
                       PutExportUndeclared(ModSym, Sym)
                    END
                 END

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END
END PutExportUnQualified ;


(*
   GetExported - returns the symbol which has a name SymName,
                 and is exported from the definition module ModSym.

*)

PROCEDURE GetExported (ModSym: CARDINAL;
                       SymName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      DefImpSym: Sym := RequestFromDefinition(ModSym, SymName)

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END ;
   RETURN( Sym )
END GetExported ;


(*
   RequestFromModule - returns a symbol from module ModSym with name, SymName.
*)

PROCEDURE RequestFromModule (ModSym: CARDINAL; SymName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    Sym := GetSymKey(LocalSymbols, SymName) ;
                    IF Sym=NulSym
                    THEN
                       Sym := FetchUnknownFromDefImp(ModSym, SymName)
                    END
                 END |

      ModuleSym: WITH Module DO
                    Sym := GetSymKey(LocalSymbols, SymName) ;
                    IF Sym=NulSym
                    THEN
                       Sym := FetchUnknownFromModule(ModSym, SymName)
                    END
                 END

      ELSE
         InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
      END
   END ;
   RETURN( Sym )
END RequestFromModule ;


(*
   RequestFromDefinition - returns a symbol from module ModSym with name,
                           SymName.
*)

PROCEDURE RequestFromDefinition (ModSym: CARDINAL; SymName: Name) : CARDINAL ;
VAR
   Type, Sym  : CARDINAL ;
   OldScopePtr: CARDINAL ;
BEGIN
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    Sym := GetSymKey(ExportQualifiedTree, SymName) ;
                    IF Sym=NulSym
                    THEN
                       Sym := GetSymKey(ExportUnQualifiedTree, SymName) ;
                       IF Sym=NulSym
                       THEN
                          Sym := GetSymKey(ExportRequest, SymName) ;
                          IF Sym=NulSym
                          THEN
                             OldScopePtr := ScopePtr ;
                             StartScope(ModSym) ;
                             Sym := GetScopeSym(SymName) ;
                             EndScope ;
                             Assert(OldScopePtr=ScopePtr) ;
                             IF Sym=NulSym
                             THEN
                                Sym := FetchUnknownFromDefImp(ModSym, SymName)
                             ELSE
                                IF IsFieldEnumeration(Sym)
                                THEN
                                   Type := GetType(Sym) ;
                                   IF IsExported(ModSym, GetType(Sym))
                                   THEN
                                      RETURN( Sym )
                                   END
                                END
                             END ;
                             PutSymKey(ExportRequest, SymName, Sym)
                          END
                       END
                    END
                 END

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END ;
   RETURN( Sym )
END RequestFromDefinition ;


(*
   GetWhereImported - returns the token number where this symbol
                      was imported into the current module.
*)

PROCEDURE GetWhereImported (Sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[GetCurrentModuleScope()] DO
      CASE SymbolType OF

      DefImpSym:  RETURN( GetSymKey(DefImp.WhereImported, Sym) ) |
      ModuleSym:  RETURN( GetSymKey(Module.WhereImported, Sym) )

      ELSE
         InternalError('expecting DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END GetWhereImported ;


(*
   DisplayName - displays the name.
*)

PROCEDURE DisplayName (sym: WORD) ;
BEGIN
   printf1('   %a', sym)
END DisplayName ;


(*
   DisplaySymbol - displays the name of a symbol
*)

PROCEDURE DisplaySymbol (sym: WORD) ;
VAR
   s: String ;
BEGIN
   s := Mark(InitStringCharStar(KeyToCharStar(GetSymName(sym)))) ;
   printf2('   %s (%d)', s, sym)
END DisplaySymbol ;


(*
   DisplayTrees - displays the SymbolTrees for Module symbol, ModSym.
*)

PROCEDURE DisplayTrees (ModSym: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   n := GetSymName(ModSym) ;
   printf1('Symbol trees for module/procedure: %a\n', n) ;
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    n := GetSymName(ModSym) ;
                    printf1('%a  UndefinedTree', n) ;
                    ForeachNodeDo(Unresolved, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  Local symbols', n) ;
                    ForeachNodeDo(LocalSymbols, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ExportRequest', n) ;
                    ForeachNodeDo(ExportRequest, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ExportQualified', n) ;
                    ForeachNodeDo(ExportQualifiedTree, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ExportUnQualified', n) ;
                    ForeachNodeDo(ExportUnQualifiedTree, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ExportUndeclared', n) ;
                    ForeachNodeDo(ExportUndeclared, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  DeclaredObjects', n) ;
                    ForeachNodeDo(NamedObjects, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ImportedObjects', n) ;
                    ForeachNodeDo(NamedImports, DisplayName) ; printf0('\n')
                 END |
      ModuleSym: WITH Module DO
                    n := GetSymName(ModSym) ;
                    printf1('%a  UndefinedTree', n) ;
                    ForeachNodeDo(Unresolved, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  Local symbols', n) ;
                    ForeachNodeDo(LocalSymbols, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ImportTree', n) ;
                    ForeachNodeDo(ImportTree, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ExportTree', n) ;
                    ForeachNodeDo(ExportTree, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ExportUndeclared', n) ;
                    ForeachNodeDo(ExportUndeclared, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  DeclaredObjects', n) ;
                    ForeachNodeDo(NamedObjects, DisplaySymbol) ; printf0('\n') ;
                    printf1('%a  ImportedObjects', n) ;
                    ForeachNodeDo(NamedImports, DisplayName) ; printf0('\n')
                 END |
      ProcedureSym: WITH Procedure DO
                       n := GetSymName(ModSym) ;
                       printf1('%a  UndefinedTree', n) ;
                       ForeachNodeDo(Unresolved, DisplaySymbol) ; printf0('\n') ;
                       printf1('%a  Local symbols', n) ;
                       ForeachNodeDo(LocalSymbols, DisplaySymbol) ; printf0('\n') ;
                       printf1('%a  DeclaredObjects', n) ;
                       ForeachNodeDo(NamedObjects, DisplayName) ; printf0('\n')
                    END

      ELSE
         InternalError('expecting DefImp symbol', __FILE__, __LINE__)
      END
   END
END DisplayTrees ;


(*
   FetchUnknownFromModule - returns an Unknown symbol from module, ModSym.
*)

PROCEDURE FetchUnknownFromModule (ModSym: CARDINAL;
                                  SymName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   WITH Symbols[ModSym] DO
      CASE SymbolType OF
         ModuleSym: WITH Module DO
                       Sym := GetSymKey(Unresolved, SymName) ;
                       IF Sym=NulSym
                       THEN
                          NewSym(Sym) ;
                          FillInUnknownFields(Sym, SymName) ;
                          PutSymKey(Unresolved, SymName, Sym)
                       END
                    END
      ELSE
         InternalError('expecting a Module symbol', __FILE__, __LINE__)
      END
   END ;
   RETURN( Sym )
END FetchUnknownFromModule ;


(*
   FetchUnknownFromDefImp - returns an Unknown symbol from module, ModSym.
*)

PROCEDURE FetchUnknownFromDefImp (ModSym: CARDINAL;
                                  SymName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   WITH Symbols[ModSym] DO
      CASE SymbolType OF
         DefImpSym: WITH DefImp DO
                       Sym := GetSymKey(Unresolved , SymName) ;
                       IF Sym=NulSym
                       THEN
                          NewSym(Sym) ;
                          FillInUnknownFields(Sym, SymName) ;
                          PutSymKey(Unresolved, SymName, Sym)
                       END
                    END
      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END ;
   RETURN( Sym )
END FetchUnknownFromDefImp ;


PROCEDURE FetchUnknownFrom (scope: CARDINAL;
                            SymName: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   WITH Symbols[scope] DO
      CASE SymbolType OF
         DefImpSym: WITH DefImp DO
                       Sym := GetSymKey(Unresolved, SymName) ;
                       IF Sym=NulSym
                       THEN
                          NewSym(Sym) ;
                          FillInUnknownFields(Sym, SymName) ;
                          PutSymKey(Unresolved, SymName, Sym)
                       END
                    END |
         ModuleSym: WITH Module DO
                       Sym := GetSymKey(Unresolved, SymName) ;
                       IF Sym=NulSym
                       THEN
                          NewSym(Sym) ;
                          FillInUnknownFields(Sym, SymName) ;
                          PutSymKey(Unresolved, SymName, Sym)
                       END
                    END |
         ProcedureSym: WITH Procedure DO
                          Sym := GetSymKey(Unresolved, SymName) ;
                          IF Sym=NulSym
                          THEN
                             NewSym(Sym) ;
                             FillInUnknownFields(Sym, SymName) ;
                             PutSymKey(Unresolved, SymName, Sym)
                          END
                       END

      ELSE
         InternalError('expecting a DefImp or Module or Procedure symbol', __FILE__, __LINE__)
      END
   END ;
   RETURN( Sym )
END FetchUnknownFrom ;


(*
   GetFromOuterModule - returns a symbol with name, SymName, which comes
                        from outside the current module.
*)

PROCEDURE GetFromOuterModule (SymName: Name) : CARDINAL ;
VAR
   ScopeId : CARDINAL ;
   Sym,
   ScopeSym: CARDINAL ;
BEGIN
   (* -- fixme -- gaius *)
   ScopeId := ScopePtr ;
   WHILE (NOT IsModule(ScopeCallFrame[ScopeId].Search)) AND
         (NOT IsDefImp(ScopeCallFrame[ScopeId].Search)) DO
      Assert(ScopeId>0) ;
      DEC(ScopeId)
   END ;
   DEC(ScopeId) ;
   (* we are now below the current module *)
   WHILE ScopeId>0 DO
      ScopeSym := ScopeCallFrame[ScopeId].Search ;
      IF ScopeSym#NulSym
      THEN
         Sym := GetLocalSym(ScopeSym, SymName) ;
         IF Sym=NulSym
         THEN
            IF IsModule(ScopeSym) OR IsProcedure(ScopeSym) OR IsDefImp(ScopeSym)
            THEN
               IF Sym=NulSym
               THEN
                  Sym := ExamineUnresolvedTree(ScopeSym, SymName) ;
                  IF Sym#NulSym
                  THEN
                     RETURN( Sym )
                  END
               END
            END
         ELSE
            RETURN( Sym )
         END
      END ;
      DEC(ScopeId)
   END ;
   (* at this point we force an unknown from the last module scope *)
   RETURN( RequestFromModule(GetLastModuleScope(), SymName) )
END GetFromOuterModule ;


(*
   IsExportUnQualified - returns true if a symbol, Sym, was defined as
                         being EXPORT UNQUALIFIED.
                         Sym is expected to be either a procedure or a
                         variable.
*)

PROCEDURE IsExportUnQualified (Sym: CARDINAL) : BOOLEAN ;
VAR
   OuterModule: CARDINAL ;
BEGIN
   Assert(IsVar(Sym) OR IsProcedure(Sym)) ;
   OuterModule := Sym ;
   REPEAT
      OuterModule := GetScope(OuterModule)
   UNTIL GetScope(OuterModule)=NulSym ;
   WITH Symbols[OuterModule] DO
      CASE SymbolType OF

      ModuleSym: RETURN( FALSE ) |
      DefImpSym: RETURN( GetSymKey(
                                    DefImp.ExportUnQualifiedTree,
                                    GetSymName(Sym)
                                  )=Sym
                       )

      ELSE
         InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END IsExportUnQualified ;


(*
   IsExportQualified - returns true if a symbol, Sym, was defined as
                       being EXPORT QUALIFIED.
                       Sym is expected to be either a procedure or a
                       variable.
*)

PROCEDURE IsExportQualified (Sym: CARDINAL) : BOOLEAN ;
VAR
   OuterModule: CARDINAL ;
BEGIN
   OuterModule := Sym ;
   REPEAT
      OuterModule := GetScope(OuterModule)
   UNTIL GetScope(OuterModule)=NulSym ;
   WITH Symbols[OuterModule] DO
      CASE SymbolType OF

      ModuleSym: RETURN( FALSE ) |
      DefImpSym: RETURN( GetSymKey(DefImp.ExportQualifiedTree, GetSymName(Sym))=Sym )

      ELSE
         InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END IsExportQualified ;


(*
   ForeachImportedDo - calls a procedure, P, foreach imported symbol
                       in module, ModSym.
*)

PROCEDURE ForeachImportedDo (ModSym: CARDINAL; P: PerformOperation) ;
BEGIN
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    ForeachNodeDo( ImportTree, P ) ;
                    ForeachItemInListDo( IncludeList, P )
                 END |
      ModuleSym: WITH Module DO
                    ForeachNodeDo( ImportTree, P ) ;
                    ForeachItemInListDo( IncludeList, P )
                 END

      ELSE
         InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END ForeachImportedDo ;


(*
   ForeachExportedDo - calls a procedure, P, foreach imported symbol
                       in module, ModSym.
*)

PROCEDURE ForeachExportedDo (ModSym: CARDINAL; P: PerformOperation) ;
BEGIN
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    ForeachNodeDo( ExportQualifiedTree, P ) ;
                    ForeachNodeDo( ExportUnQualifiedTree, P )
                 END |
      ModuleSym: WITH Module DO
                    ForeachNodeDo( ExportTree, P )
                 END

      ELSE
         InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END ForeachExportedDo ;


(*
   ForeachLocalSymDo - foreach local symbol in module, Sym, or procedure, Sym,
                       perform the procedure, P.
*)

PROCEDURE ForeachLocalSymDo (Sym: CARDINAL; P: PerformOperation) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      DefImpSym:      WITH DefImp DO
                         ForeachNodeDo( LocalSymbols, P )
                      END |
      ModuleSym:      WITH Module DO
                         ForeachNodeDo( LocalSymbols, P )
                      END |
      ProcedureSym:   WITH Procedure DO
                         ForeachNodeDo( LocalSymbols, P )
                      END |
      RecordSym:      WITH Record DO
                         ForeachNodeDo( LocalSymbols, P )
                      END |
      EnumerationSym: WITH Enumeration DO
                         ForeachNodeDo( LocalSymbols, P )
                      END

      ELSE
         InternalError('expecting a DefImp, Module or Procedure symbol', __FILE__, __LINE__)
      END
   END
END ForeachLocalSymDo ;


(*
   CheckForUnknownInModule - checks for any unknown symbols in the
                             current module.
                             If any unknown symbols are found then
                             an error message is displayed.
*)

PROCEDURE CheckForUnknownInModule ;
BEGIN
   WITH Symbols[GetCurrentModuleScope()] DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    CheckForUnknowns( name, ExportQualifiedTree,
                                      'EXPORT QUALIFIED' ) ;
                    CheckForUnknowns( name, ExportUnQualifiedTree,
                                      'EXPORT UNQUALIFIED' ) ;
                    CheckForSymbols ( ExportRequest,
                                      'requested by another modules import (symbols have not been exported by the appropriate definition module)' ) ;
                    CheckForUnknowns( name, Unresolved, 'unresolved' ) ;
                    CheckForUnknowns( name, LocalSymbols, 'locally used' )
                 END |
      ModuleSym: WITH Module DO
                    CheckForUnknowns( name, Unresolved, 'unresolved' ) ;
                    CheckForUnknowns( name, ExportUndeclared, 'exported but undeclared' ) ;
                    CheckForUnknowns( name, ExportTree, 'exported but undeclared' ) ;
                    CheckForUnknowns( name, LocalSymbols, 'locally used' )
                 END

      ELSE
         InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END CheckForUnknownInModule ;


(*
   UnknownSymbolError - displays symbol name for symbol, Sym.
*)

PROCEDURE UnknownSymbolError (Sym: WORD) ;
VAR
   e: Error ;
   n: Name ;
BEGIN
   IF IsUnknown(Sym)
   THEN
      n := GetSymName(Sym) ;
      e := ChainError(GetFirstUsed(Sym), CurrentError) ;
      ErrorFormat1(e, "unknown symbol '%a'", n)
   END
END UnknownSymbolError ;


(*
   CheckForUnknowns - checks a binary tree, Tree, to see whether it contains
                      an unknown symbol. All unknown symbols are displayed
                      together with an error message.
*)

PROCEDURE CheckForUnknowns (name: Name; Tree: SymbolTree;
                            a: ARRAY OF CHAR) ;
VAR
   n: Name ;
BEGIN
   IF DoesTreeContainAny(Tree, IsUnknown)
   THEN
      CurrentError := NewError(GetTokenNo()) ;
      n := MakeKey(a) ;
      ErrorFormat2(CurrentError, "the following unknown symbols in module '%a' were '%a'",
                   name, n) ;
      ForeachNodeDo(Tree, UnknownSymbolError)
   END
END CheckForUnknowns ;


(*
   SymbolError - displays symbol name for symbol, Sym.
*)

PROCEDURE SymbolError (Sym: WORD) ;
VAR
   e: Error ;
   n: Name ;
BEGIN
   n := GetSymName(Sym) ;
   e := ChainError(GetFirstUsed(Sym), CurrentError) ;
   ErrorFormat1(e, "unknown symbol '%a' found", n)
END SymbolError ;


(*
   CheckForSymbols  - checks a binary tree, Tree, to see whether it contains
                      any symbol. The tree is expected to be empty, if not
                      then an error has occurred.
*)

PROCEDURE CheckForSymbols (Tree: SymbolTree; a: ARRAY OF CHAR) ;
VAR
   n1, n2: Name ;
BEGIN
   IF NOT IsEmptyTree(Tree)
   THEN
      n1 := GetSymName(MainModule) ;
      n2 := MakeKey(a) ;
      WriteFormat2("the following symbols are unknown at the end of module '%a' when %a",
                   n1, n2) ;
      ForeachNodeDo(Tree, SymbolError) ;
   END
END CheckForSymbols ;


(*
   PutExportUndeclared - places a symbol, Sym, into module, ModSym,
                         ExportUndeclared list provided that Sym
                         is unknown.
*)

PROCEDURE PutExportUndeclared (ModSym: CARDINAL; Sym: CARDINAL) ;
BEGIN
   IF IsUnknown(Sym)
   THEN
      WITH Symbols[ModSym] DO
         CASE SymbolType OF

         ModuleSym: PutSymKey(Module.ExportUndeclared, GetSymName(Sym), Sym) |
         DefImpSym: PutSymKey(DefImp.ExportUndeclared, GetSymName(Sym), Sym)

         ELSE
            InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
         END
      END
   END
END PutExportUndeclared ;


(*
   GetExportUndeclared - returns a symbol which has, name, from module, ModSym,
                         which is in the ExportUndeclared list.
*)

PROCEDURE GetExportUndeclared (ModSym: CARDINAL; name: Name) : CARDINAL ;
BEGIN
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      ModuleSym: RETURN( GetSymKey(Module.ExportUndeclared, name) ) |
      DefImpSym: RETURN( GetSymKey(DefImp.ExportUndeclared, name) )

      ELSE
         InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END GetExportUndeclared ;


(*
   RemoveExportUndeclared - removes a symbol, Sym, from the module, ModSym,
                            ExportUndeclaredTree.
*)

PROCEDURE RemoveExportUndeclared (ModSym: CARDINAL; Sym: CARDINAL) ;
BEGIN
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      ModuleSym: IF GetSymKey(Module.ExportUndeclared, GetSymName(Sym))=Sym
                 THEN
                    DelSymKey(Module.ExportUndeclared, GetSymName(Sym))
                 END |
      DefImpSym: IF GetSymKey(DefImp.ExportUndeclared, GetSymName(Sym))=Sym
                 THEN
                    DelSymKey(DefImp.ExportUndeclared, GetSymName(Sym))
                 END

      ELSE
         InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END RemoveExportUndeclared ;


(*
   CheckForExportedDeclaration - checks to see whether a definition module
                                 is currently being compiled, if so,
                                 symbol, Sym, is removed from the
                                 ExportUndeclared list.
                                 This procedure is called whenever a symbol
                                 is declared, thus attempting to reduce
                                 the ExportUndeclared list.
*)

PROCEDURE CheckForExportedDeclaration (Sym: CARDINAL) ;
BEGIN
   IF CompilingDefinitionModule()
   THEN
      RemoveExportUndeclared(GetCurrentModule(), Sym)
   END
END CheckForExportedDeclaration ;


(*
   CheckForUndeclaredExports - displays an error and the offending symbols
                               which have been exported but not declared
                               from module, ModSym.
*)

PROCEDURE CheckForUndeclaredExports (ModSym: CARDINAL) ;
BEGIN
   (* WriteString('Inside CheckForUndeclaredExports') ; WriteLn ; *)
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      ModuleSym: IF NOT IsEmptyTree(Module.ExportUndeclared)
                 THEN
                    WriteFormat0('undeclared identifier(s) in EXPORT list of MODULE') ;
                    ForeachNodeDo(Module.ExportUndeclared, UndeclaredSymbolError)
                 END |
      DefImpSym: IF NOT IsEmptyTree(DefImp.ExportUndeclared)
                 THEN
                    IF DoesNotNeedExportList(ModSym)
                    THEN
                       WriteFormat0('undeclared identifier(s) in DEFINITION MODULE')
                    ELSE
                       WriteFormat0('undeclared identifier(s) in EXPORT list of DEFINITION MODULE')
                    END ;
                    ForeachNodeDo(DefImp.ExportUndeclared, UndeclaredSymbolError)
                 END

      ELSE
         InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END CheckForUndeclaredExports ;


(*
   UndeclaredSymbolError - displays symbol name for symbol, Sym.
*)

PROCEDURE UndeclaredSymbolError (Sym: WORD) ;
VAR
   e: Error ;
   n: Name ;
BEGIN
   e := ChainError(GetFirstUsed(Sym), CurrentError) ;
   n := GetSymName(Sym) ;
   IF DebugUnknowns
   THEN
      printf2('undeclared symbol %a (%d)\n', n, Sym)
   END ;
   ErrorFormat1(e, 'undeclared symbol (%a)', n)
END UndeclaredSymbolError ;


(*
   PutExportUnImplemented - places a symbol, Sym, into the currently compiled
                            DefImp module NeedToBeImplemented list.
*)

PROCEDURE PutExportUnImplemented (Sym: CARDINAL) ;
VAR
   n1, n2: Name ;
BEGIN
   WITH Symbols[CurrentModule] DO
      CASE SymbolType OF

      DefImpSym: IF GetSymKey(DefImp.NeedToBeImplemented, GetSymName(Sym))=Sym
                 THEN
                    n1 := GetSymName(Sym) ;
                    n2 := GetSymName(CurrentModule) ;
                    WriteFormat2('symbol (%a) already exported from module (%a)', n1, n2)
                    
                 ELSE
                    PutSymKey(DefImp.NeedToBeImplemented, GetSymName(Sym), Sym)
                 END

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END
END PutExportUnImplemented ;


(*
   RemoveExportUnImplemented - removes a symbol, Sym, from the module, ModSym,
                               NeedToBeImplemented list.
*)

PROCEDURE RemoveExportUnImplemented (ModSym: CARDINAL; Sym: CARDINAL) ;
BEGIN
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      DefImpSym: IF GetSymKey(DefImp.NeedToBeImplemented, GetSymName(Sym))=Sym
                 THEN
                    DelSymKey(DefImp.NeedToBeImplemented, GetSymName(Sym))
                 END

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END
END RemoveExportUnImplemented ;


VAR
   ExportRequestModule: CARDINAL ;


(*
   RemoveFromExportRequest - 
*)

PROCEDURE RemoveFromExportRequest (Sym: CARDINAL) ;
BEGIN
   WITH Symbols[ExportRequestModule] DO
      CASE SymbolType OF

      DefImpSym: IF GetSymKey(DefImp.ExportRequest, GetSymName(Sym))=Sym
                 THEN
                    DelSymKey(DefImp.ExportRequest, GetSymName(Sym))
                 END

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END
END RemoveFromExportRequest ;


(*
   RemoveEnumerationFromExportRequest - removes enumeration symbol, sym, 
                                        (and its fields) from the ExportRequest tree.
*)

PROCEDURE RemoveEnumerationFromExportRequest (ModSym: CARDINAL; Sym: CARDINAL) ;
BEGIN
   IF IsEnumeration(Sym)
   THEN
      ExportRequestModule := ModSym ;
      RemoveFromExportRequest(Sym) ;
      ForeachLocalSymDo(Sym, RemoveFromExportRequest)
   END
END RemoveEnumerationFromExportRequest ;


(*
   CheckForExportedImplementation - checks to see whether an implementation
                                    module is currently being compiled, if so,
                                    symbol, Sym, is removed from the
                                    NeedToBeImplemented list.
                                    This procedure is called whenever a symbol
                                    is declared, thus attempting to reduce
                                    the NeedToBeImplemented list.
                                    Only needs to be called when a TYPE or
                                    PROCEDURE is built since the implementation
                                    module can only implement these objects
                                    declared in the definition module.

                                    It also checks whether a definition module
                                    is currently being compiled and, if so,
                                    it will ensure that symbol, Sym, is removed
                                    from the ExportRequest list. If Sym is an
                                    enumerated type it ensures that its fields
                                    are also removed.
*)

PROCEDURE CheckForExportedImplementation (Sym: CARDINAL) ;
BEGIN
   IF CompilingImplementationModule()
   THEN
      RemoveExportUnImplemented(GetCurrentModule(), Sym)
   END ;
   IF CompilingDefinitionModule() AND IsEnumeration(Sym)
   THEN
      RemoveEnumerationFromExportRequest(GetCurrentModule(), Sym)
   END
END CheckForExportedImplementation ;


(*
   CheckForUnImplementedExports - displays an error and the offending symbols
                                  which have been exported but not implemented
                                  from the current compiled module.
*)

PROCEDURE CheckForUnImplementedExports ;
BEGIN
   (* WriteString('Inside CheckForImplementedExports') ; WriteLn ; *)
   WITH Symbols[CurrentModule] DO
      CASE SymbolType OF

      DefImpSym: IF NOT IsEmptyTree(DefImp.NeedToBeImplemented)
                 THEN
                    CurrentError := NewError(GetTokenNo()) ;
                    ErrorFormat1(CurrentError, 'unimplemented identifier(s) in EXPORT list of DEFINITION MODULE %a\nthe implementation module fails to implement the following exported identifier(s)', DefImp.name) ;
                    ForeachNodeDo( DefImp.NeedToBeImplemented, UnImplementedSymbolError )
                 END

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END
END CheckForUnImplementedExports ;


(*
   UnImplementedSymbolError - displays symbol name for symbol, Sym.
*)

PROCEDURE UnImplementedSymbolError (Sym: WORD) ;
VAR
   n: Name ;
BEGIN
   CurrentError := ChainError(GetFirstUsed(Sym), CurrentError) ;
   IF IsType(Sym)
   THEN
      n := GetSymName(Sym) ;
      ErrorFormat1(CurrentError, 'hidden type is undeclared (%a)', n)
   ELSIF IsProcedure(Sym)
   THEN
      n := GetSymName(Sym) ;
      ErrorFormat1(CurrentError, 'procedure is undeclared (%a)', n)
   ELSE
      InternalError('expecting Type or Procedure symbols', __FILE__, __LINE__)
   END
END UnImplementedSymbolError ;


(*
   PutHiddenTypeDeclared - sets a flag in the current compiled module which
                           indicates that a Hidden Type is declared within
                           the implementation part of the module.
                           This procedure is expected to be called while
                           compiling the associated definition module.
*)

PROCEDURE PutHiddenTypeDeclared ;
BEGIN
   WITH Symbols[CurrentModule] DO
      CASE SymbolType OF

      DefImpSym: DefImp.ContainsHiddenType := TRUE

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END
END PutHiddenTypeDeclared ;


(*
   IsHiddenTypeDeclared - returns true if a Hidden Type was declared in
                          the module, Sym.
*)

PROCEDURE IsHiddenTypeDeclared (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      DefImpSym: RETURN( DefImp.ContainsHiddenType )

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END
END IsHiddenTypeDeclared ;


(*
   PutDefinitionForC - sets a flag in the current compiled module which
                       indicates that this module is a wrapper for a C
                       file. Parameters passes to procedures in this module
                       will adopt the C calling convention.
*)

PROCEDURE PutDefinitionForC (Sym: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      DefImpSym: DefImp.ForC := TRUE

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END
END PutDefinitionForC ;


(*
   IsDefinitionForC - returns true if this definition module was declared
                      as a DEFINITION MODULE FOR "C".
*)

PROCEDURE IsDefinitionForC (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      DefImpSym: RETURN( DefImp.ForC )

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END
END IsDefinitionForC ;


(*
   PutDoesNeedExportList - sets a flag in module, Sym, which
                           indicates that this module requires an explicit
                           EXPORT QUALIFIED or UNQUALIFIED list. PIM-2
*)

PROCEDURE PutDoesNeedExportList (Sym: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      DefImpSym: DefImp.NeedExportList := TRUE

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END
END PutDoesNeedExportList ;


(*
   PutDoesNotNeedExportList - sets a flag in module, Sym, which
                              indicates that this module does not require an explicit
                              EXPORT QUALIFIED or UNQUALIFIED list. PIM-3|4
*)

PROCEDURE PutDoesNotNeedExportList (Sym: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      DefImpSym: DefImp.NeedExportList := FALSE

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END
END PutDoesNotNeedExportList ;


(*
   DoesNotNeedExportList - returns TRUE if module, Sym, does not require an explicit
                           EXPORT QUALIFIED list.
*)

PROCEDURE DoesNotNeedExportList (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      DefImpSym: RETURN( NOT DefImp.NeedExportList )

      ELSE
         InternalError('expecting a DefImp symbol', __FILE__, __LINE__)
      END
   END
END DoesNotNeedExportList ;


(*
   CheckForEnumerationInCurrentModule - checks to see whether the enumeration
                                        type symbol, Sym, has been entered into
                                        the current modules scope list.
*)

PROCEDURE CheckForEnumerationInCurrentModule (Sym: CARDINAL) ;
VAR
   ModSym: CARDINAL ;
BEGIN
   IF (SkipType(Sym)#NulSym) AND IsEnumeration(SkipType(Sym))
   THEN
      Sym := SkipType(Sym)
   END ;

   IF IsEnumeration(Sym)
   THEN
      ModSym := GetCurrentModuleScope() ;
      WITH Symbols[ModSym] DO
         CASE SymbolType OF

         DefImpSym: CheckEnumerationInList(DefImp.EnumerationScopeList, Sym) |
         ModuleSym: CheckEnumerationInList(Module.EnumerationScopeList, Sym)

         ELSE
            InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
         END
      END
   END
END CheckForEnumerationInCurrentModule ;


(*
   CheckEnumerationInList - places symbol, Sym, in the list, l,
                            providing it does not already exist.
                            PseudoScope(Sym) is called if Sym needs to
                            be added to the enumeration list, l.
*)

PROCEDURE CheckEnumerationInList (l: List; Sym: CARDINAL) ;
BEGIN
   IF NOT IsItemInList(l, Sym)
   THEN
      PutItemIntoList(l, Sym) ;
      PseudoScope(Sym)
   END
END CheckEnumerationInList ;


(*
   CheckIfEnumerationExported - An outer module may use an enumeration that
                                is declared inside an inner module. The usage
                                may occur before definition. The first pass
                                exports a symbol, later the symbol is declared
                                as an emumeration type. At this stage the
                                CheckIfEnumerationExported procedure should be
                                called. This procedure ripples from the current
                                (inner) module to outer module and every time
                                it is exported it must be added to the outer
                                module EnumerationScopeList.
*)

PROCEDURE CheckIfEnumerationExported (Sym: CARDINAL; ScopeId: CARDINAL) ;
VAR
   InnerModId,
   OuterModId : CARDINAL ;
   InnerModSym,
   OuterModSym: CARDINAL ;
BEGIN
   InnerModId := GetModuleScopeId(ScopeId) ;
   IF InnerModId>0
   THEN
      OuterModId := GetModuleScopeId(InnerModId-1) ;
      IF OuterModId>0
      THEN
         InnerModSym := ScopeCallFrame[InnerModId].Search ;
         OuterModSym := ScopeCallFrame[OuterModId].Search ;
         IF (InnerModSym#NulSym) AND (OuterModSym#NulSym)
         THEN
            IF IsExported(InnerModSym, Sym)
            THEN
               CheckForEnumerationInOuterModule(Sym, OuterModSym) ;
               CheckIfEnumerationExported(Sym, OuterModId)
            END
         END
      END
   END
END CheckIfEnumerationExported ;


(*
   CheckForEnumerationInOuterModule - checks to see whether the enumeration
                                      type symbol, Sym, has been entered into
                                      the outer module, OuterModule, scope list.
                                      OuterModule may be internal to the
                                      program module.
*)

PROCEDURE CheckForEnumerationInOuterModule (Sym: CARDINAL;
                                            OuterModule: CARDINAL) ;
BEGIN
   WITH Symbols[OuterModule] DO
      CASE SymbolType OF

      DefImpSym: IncludeItemIntoList(DefImp.EnumerationScopeList, Sym) |
      ModuleSym: IncludeItemIntoList(Module.EnumerationScopeList, Sym)

      ELSE
         InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END CheckForEnumerationInOuterModule ;


(*
   IsExported - returns true if a symbol, Sym, is exported
                from module, ModSym.
                If ModSym is a DefImp symbol then its
                ExportQualified and ExportUnQualified lists are examined.
*)

PROCEDURE IsExported (ModSym: CARDINAL; Sym: CARDINAL) : BOOLEAN ;
VAR
   SymName: Name ;
BEGIN
   SymName := GetSymName(Sym) ;
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    RETURN(
                            (GetSymKey(ExportQualifiedTree, SymName)=Sym) OR
                            (GetSymKey(ExportUnQualifiedTree, SymName)=Sym)
                          )
                 END |
      ModuleSym: WITH Module DO
                    RETURN( GetSymKey(ExportTree, SymName)=Sym )
                 END

      ELSE
         InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END IsExported ;


(*
   IsImported - returns true if a symbol, Sym, in module, ModSym,
                was imported.
*)

PROCEDURE IsImported (ModSym: CARDINAL; Sym: CARDINAL) : BOOLEAN ;
VAR
   SymName: Name ;
BEGIN
   SymName := GetSymName(Sym) ;
   WITH Symbols[ModSym] DO
      CASE SymbolType OF

      DefImpSym: WITH DefImp DO
                    RETURN(
                            (GetSymKey(ImportTree, SymName)=Sym) OR
                            IsItemInList(IncludeList, Sym)
                          )
                 END |
      ModuleSym: WITH Module DO
                    RETURN(
                            (GetSymKey(ImportTree, SymName)=Sym) OR
                            IsItemInList(IncludeList, Sym)
                          )
                 END

      ELSE
         InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END IsImported ;


(*
   IsType - returns true if the Sym is a type symbol.
*)

PROCEDURE IsType (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Symbols[Sym].SymbolType=TypeSym )
END IsType ;


(*
   IsReturnOptional - returns TRUE if the return value for, sym, is
                      optional.
*)

PROCEDURE IsReturnOptional (sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      ProcedureSym: RETURN( Procedure.ReturnOptional ) |
      ProcTypeSym : RETURN( ProcType.ReturnOptional )

      ELSE
         InternalError('expecting a Procedure or ProcType symbol',
                       __FILE__, __LINE__)
      END
   END
END IsReturnOptional ;


(*
   SetReturnOptional - sets the ReturnOptional field in the Procedure or
                       ProcType symboltable entry.
*)

PROCEDURE SetReturnOptional (sym: CARDINAL; isopt: BOOLEAN) ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      ProcedureSym: Procedure.ReturnOptional := isopt |
      ProcTypeSym : ProcType.ReturnOptional := isopt

      ELSE
         InternalError('expecting a Procedure or ProcType symbol', __FILE__, __LINE__)
      END
   END
END SetReturnOptional ;


(*
   CheckOptFunction - checks to see whether the optional return value
                      has been set before and if it differs it will
                      generate an error message.  It will set the
                      new value to, isopt.
*)

PROCEDURE CheckOptFunction (sym: CARDINAL; isopt: BOOLEAN) ;
VAR
   n: Name ;
   e: Error ;
BEGIN
   IF GetType(sym)#NulSym
   THEN
      IF IsReturnOptional(sym) AND (NOT isopt)
      THEN
         n := GetSymName(sym) ;
         e := NewError(GetTokenNo()) ;
         ErrorFormat1(e, 'function (%a) has no optional return value here', n) ;
         e := ChainError(GetDeclared(sym), e) ;
         ErrorFormat1(e, 'whereas the same function (%a) was declared to have an optional return value at this point', n)
      ELSIF (NOT IsReturnOptional(sym)) AND isopt
      THEN
         n := GetSymName(sym) ;
         e := NewError(GetTokenNo()) ;
         ErrorFormat1(e, 'function (%a) has an optional return value', n) ;
         e := ChainError(GetDeclared(sym), e) ;
         ErrorFormat1(e, 'whereas the same function (%a) was declared to have no optional return value at this point', n)
      END
   END ;
   SetReturnOptional(sym, isopt)
END CheckOptFunction ;


(*
   PutFunction - Places a TypeSym as the return type to a procedure Sym.
*)

PROCEDURE PutFunction (Sym: CARDINAL; TypeSym: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: |
      ProcedureSym: CheckOptFunction(Sym, FALSE) ; Procedure.ReturnType := TypeSym |
      ProcTypeSym : CheckOptFunction(Sym, FALSE) ; ProcType.ReturnType := TypeSym

      ELSE
         InternalError('expecting a Procedure or ProcType symbol', __FILE__, __LINE__)
      END
   END
END PutFunction ;


(*
   PutOptFunction - places a TypeSym as the optional return type to a procedure Sym.
*)

PROCEDURE PutOptFunction (Sym: CARDINAL; TypeSym: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: |
      ProcedureSym: CheckOptFunction(Sym, TRUE) ; Procedure.ReturnType := TypeSym |
      ProcTypeSym : CheckOptFunction(Sym, TRUE) ; ProcType.ReturnType := TypeSym

      ELSE
         InternalError('expecting a Procedure or ProcType symbol', __FILE__, __LINE__)
      END
   END
END PutOptFunction ;


(*
   MakeVariableForParam - 
*)

PROCEDURE MakeVariableForParam (ParamName: Name;
                                ProcSym  : CARDINAL ;
                                no       : CARDINAL) : CARDINAL ;
VAR
   VariableSym: CARDINAL ;
BEGIN
   VariableSym := MakeVar(ParamName) ;
   WITH Symbols[VariableSym] DO
      CASE SymbolType OF

      ErrorSym: RETURN( NulSym ) |
      VarSym  : Var.IsParam := TRUE     (* Variable is really a parameter *)

      ELSE
         InternalError('expecting a Var symbol', __FILE__, __LINE__)
      END
   END ;
   (* Note that the parameter is now treated as a local variable *)
   PutVar(VariableSym, GetType(GetNthParam(ProcSym, no))) ;
   (*
      Normal VAR parameters have LeftValue,
      however Unbounded VAR parameters have RightValue.
      Non VAR parameters always have RightValue.
   *)
   IF IsVarParam(ProcSym, no) AND (NOT IsUnboundedParam(ProcSym, no))
   THEN
      PutMode(VariableSym, LeftValue)
   ELSE
      PutMode(VariableSym, RightValue)
   END ;
   RETURN( VariableSym )
END MakeVariableForParam ;


(*
   PutParam - Places a Non VAR parameter ParamName with type ParamType into
              procedure Sym. The parameter number is ParamNo.
              If the procedure Sym already has this parameter then
              the parameter is checked for consistancy and the
              consistancy test is returned.
*)

PROCEDURE PutParam (Sym: CARDINAL; ParamNo: CARDINAL;
                    ParamName: Name; ParamType: CARDINAL;
                    isUnbounded: BOOLEAN) : BOOLEAN ;
VAR
   ParSym     : CARDINAL ;
   VariableSym: CARDINAL ;
BEGIN
   IF ParamNo<=NoOfParam(Sym)
   THEN
      InternalError('why are we trying to put parameters again', __FILE__, __LINE__)
   ELSE
      (* Add a new parameter *)
      NewSym(ParSym) ;
      WITH Symbols[ParSym] DO
         SymbolType := ParamSym ;
         WITH Param DO
            name := ParamName ;
            Type := ParamType ;
            IsUnbounded := isUnbounded ;
            ShadowVar := NulSym ;
            InitWhereDeclared(At)
         END
      END ;
      AddParameter(Sym, ParSym) ;
      IF ParamName#NulName
      THEN
         VariableSym := MakeVariableForParam(ParamName, Sym, ParamNo) ;
         IF VariableSym=NulSym
         THEN
            RETURN( FALSE )
         ELSE
            Symbols[ParSym].Param.ShadowVar := VariableSym
         END
      END
   END ;
   RETURN( TRUE )
END PutParam ;


(*
   PutVarParam - Places a Non VAR parameter ParamName with type
                 ParamType into procedure Sym.
                 The parameter number is ParamNo.
                 If the procedure Sym already has this parameter then
                 the parameter is checked for consistancy and the
                 consistancy test is returned.
*)

PROCEDURE PutVarParam (Sym: CARDINAL; ParamNo: CARDINAL;
                       ParamName: Name; ParamType: CARDINAL;
                       isUnbounded: BOOLEAN) : BOOLEAN ;
VAR
   ParSym     : CARDINAL ;
   VariableSym: CARDINAL ;
BEGIN
   IF ParamNo<=NoOfParam(Sym)
   THEN
      InternalError('why are we trying to put parameters again',
                    __FILE__, __LINE__)
   ELSE
      (* Add a new parameter *)
      NewSym(ParSym) ;
      WITH Symbols[ParSym] DO
         SymbolType := VarParamSym ;
         WITH VarParam DO
            name := ParamName ;
            Type := ParamType ;
            IsUnbounded := isUnbounded ;
            ShadowVar := NulSym ;
            InitWhereDeclared(At)
         END
      END ;
      AddParameter(Sym, ParSym) ;
      IF ParamName#NulName
      THEN
         VariableSym := MakeVariableForParam(ParamName, Sym, ParamNo) ;
         IF VariableSym=NulSym
         THEN
            RETURN( FALSE )
         ELSE
            Symbols[ParSym].VarParam.ShadowVar := VariableSym
         END
      END ;
      RETURN( TRUE )
   END
END PutVarParam ;


(*
   PutParamName - assigns a name, name, to paramater, no, of procedure,
                  ProcSym.
*)

PROCEDURE PutParamName (ProcSym: CARDINAL; no: CARDINAL; name: Name) ;
VAR
   ParSym: CARDINAL ;
BEGIN
   WITH Symbols[ProcSym] DO
      CASE SymbolType OF

      ErrorSym    : RETURN |
      ProcedureSym: ParSym := GetItemFromList(Procedure.ListOfParam, no) |
      ProcTypeSym : ParSym := GetItemFromList(ProcType.ListOfParam, no)

      ELSE
         InternalError('expecting a Procedure symbol', __FILE__, __LINE__)
      END
   END ;
   WITH Symbols[ParSym] DO
      CASE SymbolType OF

      ParamSym:    IF Param.name=NulName
                   THEN
                      Param.name := name ;
                      Param.ShadowVar := MakeVariableForParam(name, ProcSym, no)
                   ELSE
                      InternalError('name of parameter has already been assigned',
                                    __FILE__, __LINE__)
                   END |
      VarParamSym: IF VarParam.name=NulName
                   THEN
                      VarParam.name := name ;
                      VarParam.ShadowVar := MakeVariableForParam(name, ProcSym, no)
                   ELSE
                      InternalError('name of parameter has already been assigned',
                                    __FILE__, __LINE__)
                   END

      ELSE
         InternalError('expecting a VarParam or Param symbol', __FILE__, __LINE__)
      END
   END
END PutParamName ;


(*
   AddParameter - adds a parameter ParSym to a procedure Sym.
*)

PROCEDURE AddParameter (Sym: CARDINAL; ParSym: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: |
      ProcedureSym: PutItemIntoList(Procedure.ListOfParam, ParSym) |
      ProcTypeSym : PutItemIntoList(ProcType.ListOfParam, ParSym)

      ELSE
         InternalError('expecting a Procedure symbol', __FILE__, __LINE__)
      END
   END
END AddParameter ;


(*
   IsVarParam - Returns a conditional depending whether parameter ParamNo
                is a VAR parameter.
*)

PROCEDURE IsVarParam (Sym: CARDINAL; ParamNo: CARDINAL) : BOOLEAN ;
VAR
   IsVar: BOOLEAN ;
BEGIN
   IsVar := FALSE ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym    : |
      ProcedureSym: IsVar := IsNthParamVar(Procedure.ListOfParam, ParamNo) |
      ProcTypeSym : IsVar := IsNthParamVar(ProcType.ListOfParam, ParamNo)

      ELSE
         InternalError('expecting a Procedure or ProcType symbol', __FILE__, __LINE__)
      END
   END ;
   RETURN( IsVar )
END IsVarParam ;


(*
   IsNthParamVar - returns true if the n th parameter of the parameter list,
                   List, is a VAR parameter.
*)

PROCEDURE IsNthParamVar (Head: List; n: CARDINAL) : BOOLEAN ;
VAR
   p: CARDINAL ;
BEGIN
   p := GetItemFromList(Head, n) ;
   IF p=NulSym
   THEN
      InternalError('parameter does not exist', __FILE__, __LINE__)
   ELSE
      WITH Symbols[p] DO
         CASE SymbolType OF

         ErrorSym   : RETURN( FALSE ) |
         VarParamSym: RETURN( TRUE ) |
         ParamSym   : RETURN( FALSE )

         ELSE
            InternalError('expecting Param or VarParam symbol', __FILE__, __LINE__)
         END
      END
   END
END IsNthParamVar ;


(*
   NoOfParam - Returns the number of parameters that procedure Sym contains.
*)

PROCEDURE NoOfParam (Sym: CARDINAL) : CARDINAL ;
VAR
   n: CARDINAL ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym    : n := 0 |
      ProcedureSym: n := NoOfItemsInList(Procedure.ListOfParam) |
      ProcTypeSym : n := NoOfItemsInList(ProcType.ListOfParam)

      ELSE
         InternalError('expecting a Procedure or ProcType symbol', __FILE__, __LINE__)
      END
   END ;
   RETURN( n )
END NoOfParam ;


(*
   PutUseVarArgs - tell the symbol table that this procedure, Sym,
                   uses varargs.
                   The procedure _must_ be declared inside a
                   DEFINITION FOR "C"

*)

PROCEDURE PutUseVarArgs (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: |
      ProcedureSym: Procedure.HasVarArgs := TRUE |
      ProcTypeSym : ProcType.HasVarArgs := TRUE

      ELSE
         InternalError('expecting a Procedure or ProcType symbol', __FILE__, __LINE__)
      END
   END
END PutUseVarArgs ;


(*
   UsesVarArgs - returns TRUE if procedure, Sym, uses varargs.
                 The procedure _must_ be declared inside a
                 DEFINITION FOR "C"
*)

PROCEDURE UsesVarArgs (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym    : RETURN( FALSE ) |
      ProcedureSym: RETURN( Procedure.HasVarArgs ) |
      ProcTypeSym : RETURN( ProcType.HasVarArgs )

      ELSE
         InternalError('expecting a Procedure or ProcType symbol', __FILE__, __LINE__)
      END
   END
END UsesVarArgs ;


(*
   PutUseOptArg - tell the symbol table that this procedure, Sym,
                  uses an optarg.
*)

PROCEDURE PutUseOptArg (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: |
      ProcedureSym: Procedure.HasOptArg := TRUE |
      ProcTypeSym : ProcType.HasOptArg := TRUE

      ELSE
         InternalError('expecting a Procedure or ProcType symbol', __FILE__, __LINE__)
      END
   END
END PutUseOptArg ;


(*
   UsesOptArg - returns TRUE if procedure, Sym, uses varargs.
*)

PROCEDURE UsesOptArg (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym    : RETURN( FALSE ) |
      ProcedureSym: RETURN( Procedure.HasOptArg ) |
      ProcTypeSym : RETURN( ProcType.HasOptArg )

      ELSE
         InternalError('expecting a Procedure or ProcType symbol', __FILE__, __LINE__)
      END
   END
END UsesOptArg ;


(*
   PutOptArgInit - makes symbol, Sym, the initializer value to
                   procedure, ProcSym.
*)

PROCEDURE PutOptArgInit (ProcSym, Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   IF NOT IsError(ProcSym)
   THEN
      IF UsesOptArg(ProcSym)
      THEN
         WITH Symbols[ProcSym] DO
            CASE SymbolType OF

            ErrorSym    : |
            ProcedureSym: Procedure.OptArgInit := Sym |
            ProcTypeSym : ProcType.OptArgInit := Sym
         
            ELSE
               InternalError('expecting a Procedure or ProcType symbol', __FILE__, __LINE__)
            END
         END
      END
   END
END PutOptArgInit ;


(*
   GetOptArgInit - returns the initializer value to the optional parameter in
                   procedure, ProcSym.
*)

PROCEDURE GetOptArgInit (ProcSym: CARDINAL) : CARDINAL ;
BEGIN
   IF NOT IsError(ProcSym)
   THEN
      IF UsesOptArg(ProcSym)
      THEN
         WITH Symbols[ProcSym] DO
            CASE SymbolType OF

            ErrorSym    : |
            ProcedureSym: RETURN( Procedure.OptArgInit ) |
            ProcTypeSym : RETURN( ProcType.OptArgInit )
         
            ELSE
               InternalError('expecting a Procedure or ProcType symbol', __FILE__, __LINE__)
            END
         END
      END
   END ;
   RETURN( NulSym )
END GetOptArgInit ;


(*
   NoOfLocalVar - returns the number of local variables that exist in
                  procedure Sym. Parameters are NOT included in the
                  count.
*)

PROCEDURE NoOfLocalVar (Sym: CARDINAL) : CARDINAL ;
VAR
   n: CARDINAL ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym    : n := 0 |
      ProcedureSym: n := NoOfItemsInList(Procedure.ListOfVars)
 
      ELSE
         InternalError('expecting a Procedure symbol', __FILE__, __LINE__)
      END
   END ;
   (*
      Parameters are actually included in the list of local varaibles,
      therefore we must subtract the Parameter Number from local variable
      total.
   *)
   RETURN( n-NoOfParam(Sym) )
END NoOfLocalVar ;


(*
   IsParameterVar - returns true if parameter symbol Sym
                    was declared as a VAR.
*)

PROCEDURE IsParameterVar (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ParamSym   :  RETURN( FALSE ) |
      VarParamSym:  RETURN( TRUE )

      ELSE
         InternalError('expecting Param or VarParam symbol',
                       __FILE__, __LINE__)
      END
   END
END IsParameterVar ;


(*
   IsParameterUnbounded - returns TRUE if parameter, Sym, is
                          unbounded.
*)

PROCEDURE IsParameterUnbounded (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ParamSym   :  RETURN( Param.IsUnbounded ) |
      VarParamSym:  RETURN( VarParam.IsUnbounded )

      ELSE
         InternalError('expecting Param or VarParam symbol',
                       __FILE__, __LINE__)
      END
   END
END IsParameterUnbounded ;


(*
   IsUnboundedParam - Returns a conditional depending whether parameter
                      ParamNo is an unbounded array procedure parameter.
*)

PROCEDURE IsUnboundedParam (Sym: CARDINAL; ParamNo: CARDINAL) : BOOLEAN ;
VAR
   param: CARDINAL ;
BEGIN
   Assert(IsProcedure(Sym) OR IsProcType(Sym)) ;
   param := GetNthParam(Sym, ParamNo) ;
   RETURN( IsParameterUnbounded(param) )
END IsUnboundedParam ;


(*
   IsParameter - returns true if Sym is a parameter symbol.
*)

PROCEDURE IsParameter (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ParamSym,
      VarParamSym:  RETURN( TRUE )

      ELSE
         RETURN( FALSE )
      END
   END
END IsParameter ;


(*
   GetParameterShadowVar - returns the local variable associated with the
                           parameter symbol, sym.
*)

PROCEDURE GetParameterShadowVar (sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      ParamSym   :  RETURN( Param.ShadowVar ) |
      VarParamSym:  RETURN( VarParam.ShadowVar )

      ELSE
         InternalError('expecting a ParamSym or VarParamSym',
                       __FILE__, __LINE__)
      END
   END
END GetParameterShadowVar ;


(*
   IsProcedure - returns true if Sym is a procedure symbol.
*)

PROCEDURE IsProcedure (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN( Symbols[Sym].SymbolType=ProcedureSym )
END IsProcedure ;


(*
   ProcedureParametersDefined - dictates to procedure symbol, Sym,
                                that its parameters have been defined.
*)

PROCEDURE ProcedureParametersDefined (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym    : |
      ProcedureSym: Assert(NOT Procedure.ParamDefined) ;
                    Procedure.ParamDefined := TRUE

      ELSE
         InternalError('expecting a Procedure symbol', __FILE__, __LINE__)
      END
   END
END ProcedureParametersDefined ;


(*
   AreProcedureParametersDefined - returns true if the parameters to procedure
                                   symbol, Sym, have been defined.
*)

PROCEDURE AreProcedureParametersDefined (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym    : RETURN( FALSE ) |
      ProcedureSym: RETURN( Procedure.ParamDefined )

      ELSE
         InternalError('expecting a Procedure symbol', __FILE__, __LINE__)
      END
   END
END AreProcedureParametersDefined ;


(*
   ParametersDefinedInDefinition - dictates to procedure symbol, Sym,
                                   that its parameters have been defined in
                                   a definition module.
*)

PROCEDURE ParametersDefinedInDefinition (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym    : |
      ProcedureSym: Assert(NOT Procedure.DefinedInDef) ;
                    Procedure.DefinedInDef := TRUE

      ELSE
         InternalError('expecting a Procedure symbol', __FILE__, __LINE__)
      END
   END
END ParametersDefinedInDefinition ;


(*
   AreParametersDefinedInDefinition - returns true if procedure symbol, Sym,
                                      has had its parameters been defined in
                                      a definition module.
*)

PROCEDURE AreParametersDefinedInDefinition (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym    : RETURN( FALSE ) |
      ProcedureSym: RETURN( Procedure.DefinedInDef )

      ELSE
         InternalError('expecting a Procedure symbol', __FILE__, __LINE__)
      END
   END
END AreParametersDefinedInDefinition ;


(*
   ParametersDefinedInImplementation - dictates to procedure symbol, Sym,
                                       that its parameters have been defined in
                                       a implemtation module.
*)

PROCEDURE ParametersDefinedInImplementation (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym    : |
      ProcedureSym: Assert(NOT Procedure.DefinedInImp) ;
                    Procedure.DefinedInImp := TRUE

      ELSE
         InternalError('expecting a Procedure symbol', __FILE__, __LINE__)
      END
   END
END ParametersDefinedInImplementation ;


(*
   AreParametersDefinedInImplementation - returns true if procedure symbol, Sym,
                                          has had its parameters been defined in
                                          an implementation module.
*)

PROCEDURE AreParametersDefinedInImplementation (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym    : RETURN( FALSE ) |
      ProcedureSym: RETURN( Procedure.DefinedInImp )

      ELSE
         InternalError('expecting a Procedure symbol', __FILE__, __LINE__)
      END
   END
END AreParametersDefinedInImplementation ;


(*
   FillInUnknownFields - 
*)

PROCEDURE FillInUnknownFields (sym: CARDINAL; SymName: Name) ;
BEGIN
   WITH Symbols[sym] DO
      SymbolType := UndefinedSym ;
      WITH Undefined DO
         name := SymName ;
         Unbounded := NulSym ;
         InitWhereFirstUsed(At)
      END
   END
END FillInUnknownFields ;


(*
   FillInPointerFields - given a new symbol, sym, make it a pointer symbol
                         and initialize its fields.
*)

PROCEDURE FillInPointerFields (Sym: CARDINAL; PointerName: Name;
                               scope: CARDINAL; unbounded: CARDINAL) ;
BEGIN
   IF NOT IsError(Sym)
   THEN
      WITH Symbols[Sym] DO
         SymbolType := PointerSym ;
         CASE SymbolType OF

         PointerSym: Pointer.Type := NulSym ;
                     Pointer.name := PointerName ;
                     Pointer.Unbounded := unbounded ;       (* The unbounded for this *)
                     Pointer.Scope := scope ;               (* Which scope created it *)
                     Pointer.Size := InitValue()

         ELSE
            InternalError('expecting a Pointer symbol', __FILE__, __LINE__)
         END
      END
   END
END FillInPointerFields ;


(*
   MakePointer - returns a pointer symbol with PointerName.
*)

PROCEDURE MakePointer (PointerName: Name) : CARDINAL ;
VAR
   unbounded,
   sym      : CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare(PointerName, unbounded) ;
   FillInPointerFields(sym, PointerName, GetCurrentScope(), unbounded) ;
   RETURN( sym )
END MakePointer ;


(*
   PutPointer - gives a pointer symbol a type, PointerType.
*)

PROCEDURE PutPointer (Sym: CARDINAL; PointerType: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym  : |
      PointerSym: Pointer.Type := PointerType

      ELSE
         InternalError('expecting a Pointer symbol', __FILE__, __LINE__)
      END
   END
END PutPointer ;


(*
   IsPointer - returns true is Sym is a pointer type symbol.
*)

PROCEDURE IsPointer (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN( Symbols[Sym].SymbolType=PointerSym )
END IsPointer ;


(*
   IsRecord - returns true is Sym is a record type symbol.
*)

PROCEDURE IsRecord (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN( Symbols[Sym].SymbolType=RecordSym )
END IsRecord ;


(*
   IsArray - returns true is Sym is an array type symbol.
*)

PROCEDURE IsArray (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN( Symbols[Sym].SymbolType=ArraySym )
END IsArray ;


(*
   IsEnumeration - returns true if Sym is an enumeration symbol.
*)

PROCEDURE IsEnumeration (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN( Symbols[Sym].SymbolType=EnumerationSym )
END IsEnumeration ;


(*
   IsUnbounded - returns true if Sym is an unbounded symbol.
*)

PROCEDURE IsUnbounded (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN( Symbols[Sym].SymbolType=UnboundedSym )
END IsUnbounded ;


(*
   GetVarScope - returns the symbol which is the scope of variable Sym.
                 ie a Module, DefImp or Procedure Symbol.
*)

PROCEDURE GetVarScope (Sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: RETURN( NulSym ) |
      VarSym  : RETURN( Var.Scope )

      ELSE
         InternalError('expecting a Var symbol', __FILE__, __LINE__)
      END
   END
END GetVarScope ;


(*
   NoOfElements - Returns the number of elements in array Sym,
                  or the number of elements in an enumeration Sym or
                  the number of interface symbols in an Interface list.
*)

PROCEDURE NoOfElements (Sym: CARDINAL) : CARDINAL ;
VAR
   n: CARDINAL ;                 
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym      : n := 0 |
      ArraySym      ,
      UnboundedSym  : n := 1 |   (* Standard language limitation *)
      EnumerationSym: n := Symbols[Sym].Enumeration.NoOfElements |
      InterfaceSym  : n := NoOfItemsInList(Interface.ObjectList)

      ELSE
         InternalError('expecting an Array or UnBounded symbol', __FILE__, __LINE__)
      END
   END ;
   RETURN( n )
END NoOfElements ;


(*
   PutArraySubscript - places an index field into the array Sym. The
                       index field is a subscript sym.
*)

PROCEDURE PutArraySubscript (Sym: CARDINAL; SubscriptSymbol: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: |
      ArraySym: Array.Subscript := SubscriptSymbol

      ELSE
         InternalError('expecting an Array symbol', __FILE__, __LINE__)
      END
   END
END PutArraySubscript ;


(*
   GetArraySubscript - returns the subscript symbol for array, Sym.
*)

PROCEDURE GetArraySubscript (Sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: RETURN( NulSym ) |
      ArraySym: RETURN( Array.Subscript )

      ELSE
         InternalError('expecting an Array symbol', __FILE__, __LINE__)
      END
   END
END GetArraySubscript ;


(*
   MakeSubscript - makes a subscript Symbol.
                   No name is required.
*)

PROCEDURE MakeSubscript () : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   NewSym(Sym) ;
   WITH Symbols[Sym] DO
      SymbolType := SubscriptSym ;
      WITH Subscript DO
         Type := NulSym ;        (* Index to a subrange symbol. *)
         Size := InitValue() ;   (* Size of this indice in*Size *)
         Offset := InitValue() ; (* Offset at runtime of symbol *)
                                 (* Pseudo ie: Offset+Size*i    *)
                                 (* 1..n. The array offset is   *)
                                 (* the real memory offset.     *)
                                 (* This offset allows the a[i] *)
                                 (* to be calculated without    *)
                                 (* the need to perform         *)
                                 (* subtractions when a[4..10]  *)
                                 (* needs to be indexed.        *)
         InitWhereDeclared(At)   (* Declared here               *)
      END
   END ;
   RETURN( Sym )
END MakeSubscript ;


(*
   PutSubscript - gives a subscript symbol a type, SimpleType.
*)

PROCEDURE PutSubscript (Sym: CARDINAL; SimpleType: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: |
      SubscriptSym: Subscript.Type := SimpleType ;

      ELSE
         InternalError('expecting a SubScript symbol', __FILE__, __LINE__)
      END
   END
END PutSubscript ;


(*
   MakeSet - makes a set Symbol with name, SetName.
*)

PROCEDURE MakeSet (SetName: Name) : CARDINAL ;
VAR
   unbounded,
   sym      : CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare(SetName, unbounded) ;
   IF NOT IsError(sym)
   THEN
      WITH Symbols[sym] DO
         SymbolType := SetSym ;
         WITH Set DO
            name := SetName ;          (* The name of the set.        *)
            Type := NulSym ;           (* Index to a subrange symbol. *)
            Size := InitValue() ;      (* Size of this set            *)
            Unbounded := unbounded ;   (* The unbounded sym for this  *)
            Scope := GetCurrentScope() ;    (* Which scope created it *)
            InitWhereDeclared(At)      (* Declared here               *)
         END
      END
   END ;
   RETURN( sym )
END MakeSet ;


(*
   PutSet - places SimpleType as the type for set, Sym.
*)

PROCEDURE PutSet (Sym: CARDINAL; SimpleType: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: |
      SetSym: WITH Set DO
      	         Type := SimpleType    (* Index to a subrange symbol  *)
      	       	     	      	       (* or an enumeration type.     *)
              END
      ELSE
         InternalError('expecting a Set symbol', __FILE__, __LINE__)
      END
   END
END PutSet ;


(*
   IsSet - returns TRUE if Sym is a set symbol.
*)

PROCEDURE IsSet (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN( Symbols[Sym].SymbolType=SetSym )
END IsSet ;


(*
   FillInUnboundedFields - 
*)

PROCEDURE FillInUnboundedFields (sym: CARDINAL; SimpleType: CARDINAL) ;
VAR
   Contents: CARDINAL ;
BEGIN
   IF sym#NulSym
   THEN
      WITH Symbols[sym] DO
         SymbolType := UnboundedSym ;
         WITH Unbounded DO
            Type := SimpleType ;            (* Index to a simple type.     *)
            Size := InitValue() ;           (* Size in bytes for this sym  *)
            Scope := GetScope(SimpleType) ; (* Which scope will create it  *)
            InitWhereDeclared(At) ;         (* Declared here               *)
            NewSym(RecordType) ;
            FillInRecordFields(RecordType, NulName, GetScope(SimpleType), NulSym) ;
            NewSym(Contents) ;
            FillInPointerFields(Contents, NulName, GetScope(SimpleType), NulSym) ;
            PutPointer(Contents, SimpleType) ;
            PutFieldRecord(RecordType,
                           MakeKey(UnboundedAddressName),
                           Contents, NulSym) ;
            PutFieldRecord(RecordType,
                           MakeKey(UnboundedHighName),
                           Cardinal, NulSym)
         END
      END
   END
END FillInUnboundedFields ;


(*
   MakeUnbounded - makes an unbounded array Symbol.
                   No name is required.
*)

PROCEDURE MakeUnbounded (SimpleType: CARDINAL) : CARDINAL ;
VAR
   sym: CARDINAL ;
BEGIN
   sym := GetUnbounded(SimpleType) ;
   IF sym=NulSym
   THEN
      NewSym(sym) ;
      IF IsUnknown(SimpleType)
      THEN
         PutPartialUnbounded(sym, SimpleType)
      ELSE
         FillInUnboundedFields(sym, SimpleType)
      END ;
      PutUnbounded(SimpleType, sym)
   END ;
   RETURN( sym )
END MakeUnbounded ;


(*
   GetUnbounded - returns the unbounded symbol associated with
                  SimpleType.
*)

PROCEDURE GetUnbounded (SimpleType: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[SimpleType] DO
      CASE SymbolType OF

      ErrorSym      :  RETURN( NulSym ) |
      RecordSym     :  RETURN( Record.Unbounded ) |
      SubrangeSym   :  RETURN( Subrange.Unbounded ) |
      EnumerationSym:  RETURN( Enumeration.Unbounded ) |
      ArraySym      :  RETURN( Array.Unbounded ) |
      ProcTypeSym   :  RETURN( ProcType.Unbounded ) |
      TypeSym       :  RETURN( Type.Unbounded ) |
      PointerSym    :  RETURN( Pointer.Unbounded ) |
      SetSym        :  RETURN( Set.Unbounded ) |
      UndefinedSym  :  RETURN( Undefined.Unbounded )

      ELSE
         RETURN( NulSym )
      END
   END
END GetUnbounded ;


(*
   PutUnbounded - associates the unbounded symbol, Sym, with
                  SimpleType.
*)

PROCEDURE PutUnbounded (SimpleType: CARDINAL; Sym: CARDINAL) ;
BEGIN
   WITH Symbols[SimpleType] DO
      CASE SymbolType OF

      ErrorSym      :  RETURN |
      RecordSym     :  Record.Unbounded := Sym |
      SubrangeSym   :  Subrange.Unbounded := Sym |
      EnumerationSym:  Enumeration.Unbounded := Sym |
      ArraySym      :  Array.Unbounded := Sym |
      ProcTypeSym   :  ProcType.Unbounded := Sym |
      TypeSym       :  Type.Unbounded := Sym |
      PointerSym    :  Pointer.Unbounded := Sym |
      SetSym        :  Set.Unbounded := Sym |
      UndefinedSym  :  Undefined.Unbounded := Sym

      ELSE
         InternalError('cannot associate an unbounded type with this symbol',
                       __FILE__, __LINE__)
      END
   END
END PutUnbounded ;


(*
   GetUnboundedRecordType - returns the record type used to
                            implement the unbounded array.
*)

PROCEDURE GetUnboundedRecordType (Sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      UnboundedSym: RETURN( Unbounded.RecordType )

      ELSE
         InternalError('expecting an UnBounded symbol', __FILE__, __LINE__)
      END
   END
END GetUnboundedRecordType ;


(*
   GetUnboundedAddressOffset - returns the offset of the address field
                               inside the record used to implement the
                               unbounded type.
*)

PROCEDURE GetUnboundedAddressOffset (Sym: CARDINAL) : CARDINAL ;
VAR
   field,
   rec  : CARDINAL ;
BEGIN
   rec := GetUnboundedRecordType(Sym) ;
   IF rec=NulSym
   THEN
      InternalError('expecting record type to be declared', __FILE__, __LINE__)
   ELSE
      field := GetLocalSym(rec, MakeKey(UnboundedAddressName)) ;
      IF field=NulSym
      THEN
         InternalError('expecting address field to be present inside unbounded record',
                       __FILE__, __LINE__)
      ELSE
         RETURN( field )
      END
   END
END GetUnboundedAddressOffset ;


(*
   GetUnboundedHighOffset - returns the offset of the high field
                            inside the record used to implement the
                            unbounded type.
*)

PROCEDURE GetUnboundedHighOffset (Sym: CARDINAL) : CARDINAL ;
VAR
   field,
   rec  : CARDINAL ;
BEGIN
   rec := GetUnboundedRecordType(Sym) ;
   IF rec=NulSym
   THEN
      InternalError('expecting record type to be declared', __FILE__, __LINE__)
   ELSE
      field := GetLocalSym(rec, MakeKey(UnboundedHighName)) ;
      IF field=NulSym
      THEN
         InternalError('expecting high field to be present inside unbounded record',
                       __FILE__, __LINE__)
      ELSE
         RETURN( field )
      END
   END
END GetUnboundedHighOffset ;


(*
   PutArray - places a type symbol into an Array.
*)

PROCEDURE PutArray (Sym, TypeSymbol: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: |
      ArraySym: WITH Array DO
                   Type := TypeSymbol (* The Array Type. ARRAY OF Type.      *)
                END
      ELSE
         InternalError('expecting an Array symbol', __FILE__, __LINE__)
      END
   END
END PutArray ;


(*
   ResolveConstructorType - if, sym, has an unresolved constructor type
                            then attempt to resolve it by examining the
                            from, type.
*)

PROCEDURE ResolveConstructorType (sym: CARDINAL;
                                  VAR type: CARDINAL;
                                  VAR from: CARDINAL;
                                  VAR unres: BOOLEAN) ;
BEGIN
   IF unres
   THEN
      IF IsConstructor(from)
      THEN
         IF IsConstructorResolved(from)
         THEN
            unres := FALSE ;
            type := GetType(from) ;
            IF (type#NulSym) AND IsSet(SkipType(type))
            THEN
               PutConstSet(sym)
            END
         END
      ELSIF (from#NulSym) AND IsSet(SkipType(from))
      THEN
         unres := FALSE ;
         type := from ;
         PutConstSet(sym)
      ELSIF (from#NulSym) AND (IsRecord(SkipType(from)) OR IsArray(SkipType(from)))
      THEN
         unres := FALSE ;
         type := from
      END
   END
END ResolveConstructorType ;


(*
   IsConstructorResolved - returns TRUE if the constructor does not
                           have an unresolved type.
*)

PROCEDURE IsConstructorResolved (sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[sym] DO
      CASE SymbolType OF

      ConstVarSym:  RETURN( NOT ConstVar.UnresFromType ) |
      ConstLitSym:  RETURN( NOT ConstLit.UnresFromType )

      ELSE
         InternalError('expecting ConstVar or ConstLit symbol',
                      __FILE__, __LINE__)
      END
   END
END IsConstructorResolved ;


(*
   CanResolveConstructor - returns TRUE if the type of the constructor,
                           sym, is known.
*)

PROCEDURE CanResolveConstructor (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF NOT IsConstructorResolved(sym)
   THEN
      WITH Symbols[sym] DO
         CASE SymbolType OF

         ConstVarSym:  WITH ConstVar DO
                          ResolveConstructorType(sym, Type, FromType, UnresFromType)
                       END |
         ConstLitSym:  WITH ConstLit DO
                          ResolveConstructorType(sym, Type, FromType, UnresFromType)
                       END |

         ELSE
            InternalError('expecting ConstVar or ConstLit symbol',
                          __FILE__, __LINE__)
         END
      END
   END ;
   RETURN( IsConstructorResolved(sym) )
END CanResolveConstructor ;


(*
   CheckAllConstructorsResolved - checks to see that the
                                  UnresolvedConstructorType list is
                                  empty and if it is not then it
                                  generates error messages.
*)

PROCEDURE CheckAllConstructorsResolved ;
VAR
   i, n, s: CARDINAL ;
   e      : Error ;
BEGIN
   n := NoOfItemsInList(UnresolvedConstructorType) ;
   IF n>0
   THEN
      FOR i := 1 TO n DO
         s := GetItemFromList(UnresolvedConstructorType, i) ;
         e := NewError(GetDeclared(s)) ;
         ErrorFormat0(e, 'constructor has an unknown type')
      END ;
      FlushErrors
   END
END CheckAllConstructorsResolved ;


(*
   ResolveConstructorTypes - to be called at the end of pass three.  Its
                             purpose is to fix up all constructors whose
                             types are unknown.
*)

PROCEDURE ResolveConstructorTypes ;
VAR
   finished: BOOLEAN ;
   i, n, s : CARDINAL ;
BEGIN
   REPEAT
      n := NoOfItemsInList(UnresolvedConstructorType) ;
      finished := TRUE ;
      i := 1 ;
      WHILE i<=n DO
         s := GetItemFromList(UnresolvedConstructorType, i) ;
         Assert(IsConstructor(s)) ;
         IF CanResolveConstructor(s)
         THEN
            finished := FALSE ;
            RemoveItemFromList(UnresolvedConstructorType, s) ;
            i := n
         END ;
         INC(i)
      END
   UNTIL finished ;
   CheckAllConstructorsResolved
END ResolveConstructorTypes ;


(*
   AddNameTo - adds Name, n, to tree, s.
*)

PROCEDURE AddNameTo (s: SymbolTree; o: CARDINAL) ;
BEGIN
   IF GetSymKey(s, GetSymName(o))=NulKey
   THEN
      PutSymKey(s, GetSymName(o), o)
   END
END AddNameTo ;


(*
   AddNameToScope - adds a Name, n, to the list of objects declared at the
                    current scope.
*)

PROCEDURE AddNameToScope (n: Name) ;
VAR
   scope: CARDINAL ;
BEGIN
   scope := GetCurrentScope() ;
   WITH Symbols[scope] DO
      CASE SymbolType OF

      ProcedureSym:  AddNameTo(Procedure.NamedObjects, MakeObject(n)) |
      ModuleSym   :  AddNameTo(Module.NamedObjects, MakeObject(n)) |
      DefImpSym   :  AddNameTo(DefImp.NamedObjects, MakeObject(n))

      ELSE
         InternalError('expecting - DefImp, Module or Procedure symbol',
                       __FILE__, __LINE__)
      END
   END
END AddNameToScope ;


(*
   AddNameToImportList - adds a Name, n, to the import list of the current
                         module.
*)

PROCEDURE AddNameToImportList (n: Name) ;
VAR
   scope: CARDINAL ;
BEGIN
   scope := GetCurrentScope() ;
   WITH Symbols[scope] DO
      CASE SymbolType OF

      ModuleSym:  AddNameTo(Module.NamedImports, MakeObject(n)) |
      DefImpSym:  AddNameTo(DefImp.NamedImports, MakeObject(n))

      ELSE
         InternalError('expecting - DefImp or Module symbol',
                       __FILE__, __LINE__)
      END
   END
END AddNameToImportList ;


VAR
   ResolveModule: CARDINAL ;


(*
   CollectSymbolFrom - 
*)

PROCEDURE CollectSymbolFrom (scope: CARDINAL; n: Name) : CARDINAL ;
VAR
   n1 : Name ;
   sym: CARDINAL ;
BEGIN
   n1 := GetSymName(scope) ;
   IF DebugUnknowns
   THEN
      printf2('declaring %a in %a', n, n1)
   END ;
   sym := CheckScopeForSym(scope, n) ;
   IF sym=NulSym
   THEN
      sym := FetchUnknownFrom(scope, n)
   END ;
   IF DebugUnknowns
   THEN
      printf1(' symbol created (%d)\n', sym)
   END ;
   RETURN( sym )
END CollectSymbolFrom ;


(*
   CollectUnknown - 
*)

PROCEDURE CollectUnknown (sym: CARDINAL; n: Name) : CARDINAL ;
VAR
   s: CARDINAL ;
BEGIN
   IF IsModule(sym) OR IsDefImp(sym)
   THEN
      RETURN( CollectSymbolFrom(sym, n) )
   ELSIF IsProcedure(sym)
   THEN
      s := CheckScopeForSym(sym, n) ;
      IF s=NulSym
      THEN
         WITH Symbols[sym] DO
            CASE SymbolType OF

            ProcedureSym:  IF GetSymKey(Procedure.NamedObjects, n)#NulKey
                           THEN
                              RETURN( CollectSymbolFrom(sym, n) )
                           END

            ELSE
               InternalError('expecting - Procedure symbol',
                             __FILE__, __LINE__)
            END
         END ;
         s := CollectUnknown(GetScope(sym), n)
      END ;
      RETURN( s )
   END
END CollectUnknown ;


(*
   ResolveImport - 
*)

PROCEDURE ResolveImport (o: WORD) ;
VAR
   n1, n2: Name ;
   sym   : CARDINAL ;
BEGIN
   IF DebugUnknowns
   THEN
      n1 := GetSymName(o) ;
      printf1('attempting to find out where %a was declared\n', n1) ;
      n1 := GetSymName(ResolveModule) ;
      n2 := GetSymName(GetScope(ResolveModule)) ;
      printf2('scope of module %a is %a\n', n1, n2)
   END ;
   sym := CollectUnknown(GetScope(ResolveModule), GetSymName(o)) ;
   IF sym=NulSym
   THEN
      MetaError2('unknown symbol {%1Uad} found in import list of module {%2a}',
                 o, ResolveModule)
   ELSE
      AddSymToModuleScope(ResolveModule, sym)
   END
END ResolveImport ;


(*
   ResolveRelativeImport - 
*)

PROCEDURE ResolveRelativeImport (sym: CARDINAL) ;
BEGIN
   IF IsModule(sym)
   THEN
      ResolveModule := sym ;
      WITH Symbols[sym] DO
         CASE SymbolType OF

         ModuleSym:  ForeachNodeDo(Module.NamedImports,
                                   ResolveImport)

         ELSE
            InternalError('expecting - Module symbol', __FILE__, __LINE__)
         END
      END
   END ;
   ForeachProcedureDo(sym, ResolveRelativeImport) ;
   ForeachInnerModuleDo(sym, ResolveRelativeImport)
END ResolveRelativeImport ;


(*
   ResolveImports - it examines the import list of all inner modules
                    and resolves all relative imports.
*)

PROCEDURE ResolveImports ;
VAR
   scope: CARDINAL ;
BEGIN
   scope := GetCurrentScope() ;
   IF DebugUnknowns
   THEN
      DisplayTrees(scope)
   END ;
   ForeachProcedureDo(scope, ResolveRelativeImport) ;
   ForeachInnerModuleDo(scope, ResolveRelativeImport)
END ResolveImports ;


(*
   GetScope - returns the declaration scope of the symbol.
*)

PROCEDURE GetScope (Sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym           : ErrorAbort0('') |
      DefImpSym          : RETURN( NulSym ) |
      ModuleSym          : RETURN( Module.Scope ) |
      VarSym             : RETURN( Var.Scope ) |
      ProcedureSym       : RETURN( Procedure.Scope ) |
      ProcTypeSym        : RETURN( ProcType.Scope ) |
      RecordFieldSym     : RETURN( RecordField.Scope ) |
      VarientSym         : RETURN( Varient.Scope ) |
      VarientFieldSym    : RETURN( VarientField.Scope ) |
      EnumerationSym     : RETURN( Enumeration.Scope ) |
      EnumerationFieldSym: RETURN( EnumerationField.Scope ) |
      SubrangeSym        : RETURN( Subrange.Scope ) |
      ArraySym           : RETURN( Array.Scope ) |
      TypeSym            : RETURN( Type.Scope ) |
      PointerSym         : RETURN( Pointer.Scope ) |
      RecordSym          : RETURN( Record.Scope ) |
      SetSym             : RETURN( Set.Scope ) |
      UnboundedSym       : RETURN( Unbounded.Scope )

      ELSE
         InternalError('not implemented yet', __FILE__, __LINE__)
      END
   END
END GetScope ;


(*
   GetModuleScope - returns the module scope of symbol, sym.
                    If sym was declared within a nested procedure
                    then return the module which defines the
                    procedure.
*)

PROCEDURE GetModuleScope (sym: CARDINAL) : CARDINAL ;
VAR
   mod: CARDINAL ;
BEGIN
   mod := GetScope(sym) ;
   WHILE (mod#NulSym) AND (NOT IsDefImp(mod)) AND (NOT IsModule(mod)) DO
      mod := GetScope(mod)
   END ;
   RETURN( mod )
END GetModuleScope ;


(*
   GetProcedureScope - returns the innermost procedure (if any)
                       in which the symbol, sym, resides.
                       A module inside the procedure is skipped
                       over.
*)

PROCEDURE GetProcedureScope (sym: CARDINAL) : CARDINAL ;
BEGIN
   WHILE (sym#NulSym) AND (NOT IsProcedure(sym)) DO
      sym := GetScope(sym)
   END ;
   IF (sym#NulSym) AND IsProcedure(sym)
   THEN
      RETURN( sym )
   ELSE
      RETURN( NulSym )
   END
END GetProcedureScope ;


(*
   IsModuleWithinProcedure - returns TRUE if module, sym, is
                             inside a procedure.
*)

PROCEDURE IsModuleWithinProcedure (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( GetProcedureScope(sym)#NulSym )
END IsModuleWithinProcedure ;


(*
   GetParent - returns the parent of symbol, Sym.
*)

PROCEDURE GetParent (Sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym           : ErrorAbort0('') |
      VarientSym         : RETURN( Varient.Parent ) |
      VarientFieldSym    : RETURN( VarientField.Parent ) |
      RecordFieldSym     : RETURN( RecordField.Parent ) |
      EnumerationFieldSym: RETURN( EnumerationField.Type )

      ELSE
         InternalError('not implemented yet', __FILE__, __LINE__)
      END
   END
END GetParent ;


(*
   IsRecordField - returns true if Sym is a record field.
*)

PROCEDURE IsRecordField (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Symbols[Sym].SymbolType=RecordFieldSym )
END IsRecordField ;


(*
   MakeProcType - returns a procedure type symbol with ProcTypeName.
*)

PROCEDURE MakeProcType (ProcTypeName: Name) : CARDINAL ;
VAR
   unbounded,
   sym      : CARDINAL ;
BEGIN
   sym := HandleHiddenOrDeclare(ProcTypeName, unbounded) ;
   IF NOT IsError(sym)
   THEN
      WITH Symbols[sym] DO
         SymbolType := ProcTypeSym ;
         CASE SymbolType OF

         ProcTypeSym: ProcType.ReturnType := NulSym ;
                      ProcType.name := ProcTypeName ;
                      InitList(ProcType.ListOfParam) ;
                      ProcType.HasVarArgs := FALSE ;     (* Does this proc type use ... ? *)
                      ProcType.HasOptArg  := FALSE ;     (* Does this proc type use [ ] ? *)
                      ProcType.OptArgInit := NulSym ;    (* The optarg initial value.     *)
                      ProcType.ReturnOptional := FALSE ; (* Is the return value optional? *)
                      ProcType.Scope := GetCurrentScope() ;
                                                         (* scope of procedure.           *)
                      ProcType.Size := InitValue() ;
                      ProcType.TotalParamSize := InitValue() ;  (* size of all parameters.       *)
                      ProcType.Unbounded := unbounded ;  (* The unbounded for this *)
                      InitWhereDeclared(ProcType.At)     (* Declared here *)

         ELSE
            InternalError('expecting ProcType symbol', __FILE__, __LINE__)
         END
      END
   END ;
   RETURN( sym )
END MakeProcType ;


(*
   PutProcTypeParam - Places a Non VAR parameter ParamName with type
                      ParamType into ProcType Sym.
*)

PROCEDURE PutProcTypeParam (Sym: CARDINAL;
                            ParamType: CARDINAL; isUnbounded: BOOLEAN) ;
VAR
   ParSym: CARDINAL ;
BEGIN
   NewSym(ParSym) ;
   WITH Symbols[ParSym] DO
      SymbolType := ParamSym ;
      WITH Param DO
         name := NulName ;
         Type := ParamType ;
         IsUnbounded := isUnbounded ;
         ShadowVar := NulSym ;
         InitWhereDeclared(At)
      END
   END ;
   AddParameter(Sym, ParSym)
END PutProcTypeParam ;


(*
   PutProcTypeVarParam - Places a Non VAR parameter ParamName with type
                         ParamType into ProcType Sym.
*)

PROCEDURE PutProcTypeVarParam (Sym: CARDINAL;
                               ParamType: CARDINAL; isUnbounded: BOOLEAN) ;
VAR
   ParSym: CARDINAL ;
BEGIN
   NewSym(ParSym) ;
   WITH Symbols[ParSym] DO
      SymbolType := VarParamSym ;
      WITH Param DO
         name := NulName ;
         Type := ParamType ;
         IsUnbounded := isUnbounded ;
         ShadowVar := NulSym ;
         InitWhereDeclared(At)
      END
   END ;
   AddParameter(Sym, ParSym)
END PutProcTypeVarParam ;


(*
   PutProcedureReachable - Sets the procedure, Sym, to be reachable by the
                           main Module.
*)

PROCEDURE PutProcedureReachable (Sym: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym: |
      ProcedureSym: Procedure.Reachable := TRUE

      ELSE
         InternalError('expecting Procedure symbol', __FILE__, __LINE__)
      END
   END
END PutProcedureReachable ;


(*
   PutModuleStartQuad - Places QuadNumber into the Module symbol, Sym.
                        QuadNumber is the start quad of Module,
                        Sym.
*)

PROCEDURE PutModuleStartQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ModuleSym: Module.StartQuad := QuadNumber |
      DefImpSym: DefImp.StartQuad := QuadNumber

      ELSE
         InternalError('expecting a Module or DefImp symbol',
                        __FILE__, __LINE__)
      END
   END
END PutModuleStartQuad ;


(*
   PutModuleEndQuad - Places QuadNumber into the Module symbol, Sym.
                      QuadNumber is the end quad of Module,
                      Sym.
*)

PROCEDURE PutModuleEndQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ModuleSym: Module.EndQuad := QuadNumber |
      DefImpSym: DefImp.EndQuad := QuadNumber

      ELSE
         InternalError('expecting a Module or DefImp symbol',
                       __FILE__, __LINE__)
      END
   END
END PutModuleEndQuad ;


(*
   PutModuleFinallyStartQuad - Places QuadNumber into the Module symbol, Sym.
                               QuadNumber is the finally start quad of
                               Module, Sym.
*)

PROCEDURE PutModuleFinallyStartQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ModuleSym: Module.StartFinishQuad := QuadNumber |
      DefImpSym: DefImp.StartFinishQuad := QuadNumber

      ELSE
         InternalError('expecting a Module or DefImp symbol',
                        __FILE__, __LINE__)
      END
   END
END PutModuleFinallyStartQuad ;


(*
   PutModuleFinallyEndQuad - Places QuadNumber into the Module symbol, Sym.
                             QuadNumber is the end quad of the finally block
                             in Module, Sym.
*)

PROCEDURE PutModuleFinallyEndQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ModuleSym: Module.EndFinishQuad := QuadNumber |
      DefImpSym: DefImp.EndFinishQuad := QuadNumber

      ELSE
         InternalError('expecting a Module or DefImp symbol',
                       __FILE__, __LINE__)
      END
   END
END PutModuleFinallyEndQuad ;


(*
   GetModuleQuads - Returns, StartInit EndInit StartFinish EndFinish,
                    Quads of a Module, Sym.
                    Start and End represent the initialization code
                    of the Module, Sym.
*)

PROCEDURE GetModuleQuads (Sym: CARDINAL;
                          VAR StartInit, EndInit,
                          StartFinish, EndFinish: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ModuleSym: WITH Module DO
                    StartInit := StartQuad ;
                    EndInit := EndQuad ;
                    StartFinish := StartFinishQuad ;
                    EndFinish := EndFinishQuad
                 END |
      DefImpSym: WITH DefImp DO
                    StartInit := StartQuad ;
                    EndInit := EndQuad ;
                    StartFinish := StartFinishQuad ;
                    EndFinish := EndFinishQuad
                 END

      ELSE
         InternalError('expecting a Module or DefImp symbol',
                        __FILE__, __LINE__)
      END
   END
END GetModuleQuads ;


(*
   PutModuleFinallyFunction - Places Tree, finally, into the Module symbol, Sym.
*)

PROCEDURE PutModuleFinallyFunction (Sym: CARDINAL; finally: Tree) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ModuleSym: Module.FinallyFunction := finally |
      DefImpSym: DefImp.FinallyFunction := finally

      ELSE
         InternalError('expecting a Module or DefImp symbol',
                        __FILE__, __LINE__)
      END
   END
END PutModuleFinallyFunction ;


(*
   GetModuleFinallyFunction - returns the finally tree from the Module symbol, Sym.
*)

PROCEDURE GetModuleFinallyFunction (Sym: CARDINAL) : Tree ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ModuleSym: RETURN( Module.FinallyFunction) |
      DefImpSym: RETURN( DefImp.FinallyFunction)

      ELSE
         InternalError('expecting a Module or DefImp symbol',
                        __FILE__, __LINE__)
      END
   END
END GetModuleFinallyFunction ;


(*
   PutProcedureScopeQuad - Places QuadNumber into the Procedure symbol, Sym.
                           QuadNumber is the start quad of scope for procedure,
                           Sym.
*)

PROCEDURE PutProcedureScopeQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym: Procedure.ScopeQuad := QuadNumber

      ELSE
         InternalError('expecting a Procedure symbol', __FILE__, __LINE__)
      END
   END
END PutProcedureScopeQuad ;


(*
   PutProcedureStartQuad - Places QuadNumber into the Procedure symbol, Sym.
                           QuadNumber is the start quad of procedure,
                           Sym.
*)

PROCEDURE PutProcedureStartQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym: Procedure.StartQuad := QuadNumber

      ELSE
         InternalError('expecting a Procedure symbol', __FILE__, __LINE__)
      END
   END
END PutProcedureStartQuad ;


(*
   PutProcedureEndQuad - Places QuadNumber into the Procedure symbol, Sym.
                         QuadNumber is the end quad of procedure,
                         Sym.
*)

PROCEDURE PutProcedureEndQuad (Sym: CARDINAL; QuadNumber: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym: Procedure.EndQuad := QuadNumber

      ELSE
         InternalError('expecting a Procedure symbol', __FILE__, __LINE__)
      END
   END
END PutProcedureEndQuad ;


(*
   GetProcedureQuads - Returns, Start and End, Quads of a procedure, Sym.
*)

PROCEDURE GetProcedureQuads (Sym: CARDINAL; VAR scope, start, end: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym: WITH Procedure DO
                       scope := ScopeQuad ;
                       start := StartQuad ;
                       end := EndQuad
                    END

      ELSE
         InternalError('expecting a Procedure symbol', __FILE__, __LINE__)
      END
   END
END GetProcedureQuads ;


(*
   GetReadQuads - assigns Start and End to the beginning and end of
                  symbol, Sym, read history usage.
*)

PROCEDURE GetReadQuads (Sym: CARDINAL; m: ModeOfAddr;
                        VAR Start, End: CARDINAL) ;
BEGIN
   GetReadLimitQuads(Sym, m, 0, 0, Start, End)
END GetReadQuads ;


(*
   GetWriteQuads - assigns Start and End to the beginning and end of
                   symbol, Sym, usage.
*)

PROCEDURE GetWriteQuads (Sym: CARDINAL; m: ModeOfAddr;
                         VAR Start, End: CARDINAL) ;
BEGIN
   GetWriteLimitQuads(Sym, m, 0, 0, Start, End)
END GetWriteQuads ;


(*
   Max - 
*)

PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   Min - 
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


(*
   GetQuads - assigns Start and End to the beginning and end of
              symbol, Sym, usage.
*)

PROCEDURE GetQuads (Sym: CARDINAL; m: ModeOfAddr; VAR Start, End: CARDINAL) ;
VAR
   StartRead, EndRead,
   StartWrite, EndWrite: CARDINAL ;
BEGIN
   GetReadQuads(Sym, m, StartRead, EndRead) ;
   GetWriteQuads(Sym, m, StartWrite, EndWrite) ;
   IF StartRead=0
   THEN
      Start := StartWrite
   ELSIF StartWrite=0
   THEN
      Start := StartRead
   ELSE
      Start := Min(StartRead, StartWrite)
   END ;
   IF EndRead=0
   THEN
      End := EndWrite
   ELSIF EndWrite=0
   THEN
      End := EndRead
   ELSE
      End := Max(EndRead, EndWrite)
   END
END GetQuads ;


(*
   PutReadQuad - places Quad into the list of symbol usage.
*)

PROCEDURE PutReadQuad (Sym: CARDINAL; m: ModeOfAddr; Quad: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarSym: IncludeItemIntoList(Var.ReadUsageList[m], Quad)

      ELSE
         InternalError('expecting a Var symbol', __FILE__, __LINE__)
      END
   END
END PutReadQuad ;


(*
   RemoveReadQuad - places Quad into the list of symbol usage.
*)

PROCEDURE RemoveReadQuad (Sym: CARDINAL; m: ModeOfAddr; Quad: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarSym: RemoveItemFromList(Var.ReadUsageList[m], Quad)

      ELSE
         InternalError('expecting a Var symbol', __FILE__, __LINE__)
      END
   END
END RemoveReadQuad ;


(*
   PutWriteQuad - places Quad into the list of symbol usage.
*)

PROCEDURE PutWriteQuad (Sym: CARDINAL; m: ModeOfAddr; Quad: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarSym: IncludeItemIntoList(Var.WriteUsageList[m], Quad)

      ELSE
         InternalError('expecting a Var symbol', __FILE__, __LINE__)
      END
   END
END PutWriteQuad ;


(*
   RemoveWriteQuad - places Quad into the list of symbol usage.
*)

PROCEDURE RemoveWriteQuad (Sym: CARDINAL; m: ModeOfAddr; Quad: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarSym: RemoveItemFromList(Var.WriteUsageList[m], Quad)

      ELSE
         InternalError('expecting a Var symbol', __FILE__, __LINE__)
      END
   END
END RemoveWriteQuad ;


(*
   DoFindLimits - assigns, Start, and, End, to the start and end
                  limits contained in the list, l.  It ensures that
                  Start and End are within StartLimit..EndLimit.
                  If StartLimit or EndLimit are 0 then Start is
                  is set to the first value and End to the last.
*)

PROCEDURE DoFindLimits (StartLimit, EndLimit: CARDINAL;
                        VAR Start, End: CARDINAL; l: List) ;
VAR
   i, j, n: CARDINAL ;
BEGIN
   End := 0 ;
   Start := 0 ;
   i := 1 ;
   n := NoOfItemsInList(l) ;
   WHILE i<=n DO
      j := GetItemFromList(l, i) ;
      IF (j>End) AND (j>=StartLimit) AND ((j<=EndLimit) OR (EndLimit=0))
      THEN
         End := j
      END ;
      IF ((Start=0) OR (j<Start)) AND (j#0) AND (j>=StartLimit) AND
         ((j<=EndLimit) OR (EndLimit=0))
      THEN
         Start := j
      END ;
      INC(i)
   END
END DoFindLimits ;


(*
   GetReadLimitQuads - returns Start and End which have been assigned
                       the start and end of when the symbol was read
                       to within: StartLimit..EndLimit.
*)

PROCEDURE GetReadLimitQuads (Sym: CARDINAL; m: ModeOfAddr;
                             StartLimit, EndLimit: CARDINAL;
                             VAR Start, End: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarSym: DoFindLimits(StartLimit, EndLimit, Start, End,
                           Var.ReadUsageList[m])

      ELSE
         InternalError('expecting a Var symbol', __FILE__, __LINE__)
      END
   END
END GetReadLimitQuads ;


(*
   GetWriteLimitQuads - returns Start and End which have been assigned
                        the start and end of when the symbol was written
                        to within: StartLimit..EndLimit.
*)

PROCEDURE GetWriteLimitQuads (Sym: CARDINAL; m: ModeOfAddr;
                              StartLimit, EndLimit: CARDINAL;
                              VAR Start, End: CARDINAL) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarSym     : DoFindLimits(StartLimit, EndLimit, Start, End,
                                Var.WriteUsageList[m])

      ELSE
         InternalError('expecting a Var symbol', __FILE__, __LINE__)
      END
   END
END GetWriteLimitQuads ;


(*
   GetNthProcedure - Returns the Nth procedure in Module, Sym.
*)

PROCEDURE GetNthProcedure (Sym: CARDINAL; n: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      DefImpSym: RETURN( GetItemFromList(DefImp.ListOfProcs, n) ) |
      ModuleSym: RETURN( GetItemFromList(Module.ListOfProcs, n) )

      ELSE
         InternalError('expecting a DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END GetNthProcedure ;


(*
   GetDeclared - returns the token where this symbol was declared.
*)

PROCEDURE GetDeclared (Sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym           : RETURN( Error.At.Declared ) |
      ObjectSym          : RETURN( Object.At.Declared ) |
      VarientSym         : RETURN( Varient.At.Declared ) |
      RecordSym          : RETURN( Record.At.Declared ) |
      SubrangeSym        : RETURN( Subrange.At.Declared ) |
      EnumerationSym     : RETURN( Enumeration.At.Declared ) |
      ArraySym           : RETURN( Array.At.Declared ) |
      SubscriptSym       : RETURN( Subscript.At.Declared ) |
      UnboundedSym       : RETURN( Unbounded.At.Declared ) |
      ProcedureSym       : RETURN( Procedure.At.Declared ) |
      ProcTypeSym        : RETURN( ProcType.At.Declared ) |
      ParamSym           : RETURN( Param.At.Declared ) |
      VarParamSym        : RETURN( VarParam.At.Declared ) |
      ConstStringSym     : RETURN( ConstString.At.Declared ) |
      ConstLitSym        : RETURN( ConstLit.At.Declared ) |
      ConstVarSym        : RETURN( ConstVar.At.Declared ) |
      VarSym             : RETURN( Var.At.Declared ) |
      TypeSym            : RETURN( Type.At.Declared ) |
      PointerSym         : RETURN( Pointer.At.Declared ) |
      RecordFieldSym     : RETURN( RecordField.At.Declared ) |
      VarientFieldSym    : RETURN( VarientField.At.Declared ) |
      EnumerationFieldSym: RETURN( EnumerationField.At.Declared ) |
      SetSym             : RETURN( Set.At.Declared ) |
      DefImpSym          : RETURN( DefImp.At.Declared ) |
      ModuleSym          : RETURN( Module.At.Declared ) |
      UndefinedSym       : RETURN( GetFirstUsed(Sym) ) |
      PartialUnboundedSym: RETURN( GetDeclared(PartialUnbounded.Type) )

      ELSE
         InternalError('not expecting this type of symbol', __FILE__, __LINE__)
      END
   END
END GetDeclared ;


(*
   GetFirstUsed - returns the token where this symbol was first used.
*)

PROCEDURE GetFirstUsed (Sym: CARDINAL) : CARDINAL ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ErrorSym           : RETURN( Error.At.FirstUsed ) |
      ObjectSym          : RETURN( Object.At.FirstUsed ) |
      UndefinedSym       : RETURN( Undefined.At.FirstUsed ) |
      VarientSym         : RETURN( Varient.At.FirstUsed ) |
      RecordSym          : RETURN( Record.At.FirstUsed ) |
      SubrangeSym        : RETURN( Subrange.At.FirstUsed ) |
      EnumerationSym     : RETURN( Enumeration.At.FirstUsed ) |
      ArraySym           : RETURN( Array.At.FirstUsed ) |
      SubscriptSym       : RETURN( Subscript.At.FirstUsed ) |
      UnboundedSym       : RETURN( Unbounded.At.FirstUsed ) |
      ProcedureSym       : RETURN( Procedure.At.FirstUsed ) |
      ProcTypeSym        : RETURN( ProcType.At.FirstUsed ) |
      ParamSym           : RETURN( Param.At.FirstUsed ) |
      VarParamSym        : RETURN( VarParam.At.FirstUsed ) |
      ConstStringSym     : RETURN( ConstString.At.FirstUsed ) |
      ConstLitSym        : RETURN( ConstLit.At.FirstUsed ) |
      ConstVarSym        : RETURN( ConstVar.At.FirstUsed ) |
      VarSym             : RETURN( Var.At.FirstUsed ) |
      TypeSym            : RETURN( Type.At.FirstUsed ) |
      PointerSym         : RETURN( Pointer.At.FirstUsed ) |
      RecordFieldSym     : RETURN( RecordField.At.FirstUsed ) |
      VarientFieldSym    : RETURN( VarientField.At.FirstUsed ) |
      EnumerationFieldSym: RETURN( EnumerationField.At.FirstUsed ) |
      SetSym             : RETURN( Set.At.FirstUsed ) |
      DefImpSym          : RETURN( DefImp.At.FirstUsed ) |
      ModuleSym          : RETURN( Module.At.FirstUsed )

      ELSE
         InternalError('not expecting this type of symbol', __FILE__, __LINE__)
      END
   END
END GetFirstUsed ;


(*
   ForeachProcedureDo - for each procedure in module, Sym, do procedure, P.
*)

PROCEDURE ForeachProcedureDo (Sym: CARDINAL; P: PerformOperation) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      DefImpSym   : ForeachItemInListDo( DefImp.ListOfProcs, P) |
      ModuleSym   : ForeachItemInListDo( Module.ListOfProcs, P) |
      ProcedureSym: ForeachItemInListDo( Procedure.ListOfProcs, P)

      ELSE
         InternalError('expecting DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END ForeachProcedureDo ;


(*
   ForeachInnerModuleDo - for each inner module in module, Sym,
                          do procedure, P.
*)

PROCEDURE ForeachInnerModuleDo (Sym: CARDINAL; P: PerformOperation) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      DefImpSym   : ForeachItemInListDo( DefImp.ListOfModules, P) |
      ModuleSym   : ForeachItemInListDo( Module.ListOfModules, P) |
      ProcedureSym: ForeachItemInListDo( Procedure.ListOfModules, P)

      ELSE
         InternalError('expecting DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END ForeachInnerModuleDo ;


(*
   ForeachModuleDo - for each module do procedure, P.
*)

PROCEDURE ForeachModuleDo (P: PerformOperation) ;
BEGIN
   ForeachNodeDo(ModuleTree, P)
END ForeachModuleDo ;


(*
   ForeachFieldEnumerationDo - for each field in enumeration, Sym,
                               do procedure, P.
*)

PROCEDURE ForeachFieldEnumerationDo (Sym: CARDINAL; P: PerformOperation) ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      EnumerationSym: ForeachNodeDo( Enumeration.LocalSymbols, P)

      ELSE
         InternalError('expecting Enumeration symbol', __FILE__, __LINE__)
      END
   END
END ForeachFieldEnumerationDo ;


(*
   IsProcedureReachable - Returns true if the procedure, Sym, is
                          reachable from the main Module.
*)

PROCEDURE IsProcedureReachable (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym: RETURN( Procedure.Reachable )

      ELSE
         InternalError('expecting Procedure symbol', __FILE__, __LINE__)
      END
   END
END IsProcedureReachable ;


(*
   IsProcType - returns true if Sym is a ProcType Symbol.
*)

PROCEDURE IsProcType (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Symbols[Sym].SymbolType=ProcTypeSym )
END IsProcType ;


(*
   IsVar - returns true if Sym is a Var Symbol.
*)

PROCEDURE IsVar (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Symbols[Sym].SymbolType=VarSym )
END IsVar ;


(*
   DoIsConst - returns TRUE if Sym is defined as a constant
               or is an enumeration field or string.
*)

PROCEDURE DoIsConst (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      RETURN( (SymbolType=ConstVarSym) OR
              (SymbolType=ConstLitSym) OR
              (SymbolType=ConstStringSym) OR
              ((SymbolType=VarSym) AND (Var.AddrMode=ImmediateValue)) OR
              (SymbolType=EnumerationFieldSym)
            )
   END
END DoIsConst ;


(*
   IsConst - returns true if Sym contains a constant value.
*)

PROCEDURE IsConst (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsConstructor(Sym)
   THEN
      RETURN( IsConstructorConstant(Sym) )
   ELSE
      RETURN( DoIsConst(Sym) )
   END
END IsConst ;


(*
   IsConstString - returns true if Sym is a string.
*)

PROCEDURE IsConstString (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      RETURN( SymbolType=ConstStringSym )
   END
END IsConstString ;


(*
   IsConstLit - returns true if Sym is a literal constant.
*)

PROCEDURE IsConstLit (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      RETURN( SymbolType=ConstLitSym )
   END
END IsConstLit ;


(*
   IsDummy - returns true if Sym is a Dummy symbol.
*)

PROCEDURE IsDummy (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Symbols[Sym].SymbolType=DummySym )
END IsDummy ;


(*
   IsTemporary - returns true if Sym is a Temporary symbol.
*)

PROCEDURE IsTemporary (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarSym     :  RETURN( Var.IsTemp ) |
      ConstVarSym:  RETURN( ConstVar.IsTemp )

      ELSE
         RETURN( FALSE )
      END
   END
END IsTemporary ;


(*
   IsVarAParam - returns true if Sym is a variable declared as a parameter.
*)

PROCEDURE IsVarAParam (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      VarSym: RETURN( Var.IsParam )

      ELSE
         RETURN( FALSE )
      END
   END
END IsVarAParam ;


(*
   IsSubscript - returns true if Sym is a subscript symbol.
*)

PROCEDURE IsSubscript (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Symbols[Sym].SymbolType=SubscriptSym )
END IsSubscript ;


(*
   IsSubrange - returns true if Sym is a subrange symbol.
*)

PROCEDURE IsSubrange (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Symbols[Sym].SymbolType=SubrangeSym )
END IsSubrange ;


(*
   IsProcedureVariable - returns true if a Sym is a variable and
                         it was declared within a procedure.
*)

PROCEDURE IsProcedureVariable (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN( IsVar(Sym) AND IsProcedure(GetVarScope(Sym)) )
END IsProcedureVariable ;


(*
   IsProcedureNested - returns TRUE if procedure, Sym, was
                       declared as a nested procedure.
*)

PROCEDURE IsProcedureNested (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsProcedure(Sym) AND (IsProcedure(GetScope(Sym))) )
END IsProcedureNested ;


(*
   IsAModula2Type - returns true if Sym, is a:
                    IsType, IsPointer, IsRecord, IsEnumeration,
                    IsSubrange, IsArray, IsUnbounded, IsProcType.
                    NOTE that it different from IsType.
                    IsType is used for:
                    TYPE
                       a = CARDINAL ;  (* IsType(a)=TRUE *)
*)

PROCEDURE IsAModula2Type (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   RETURN(
           IsType(Sym) OR IsRecord(Sym) OR IsPointer(Sym) OR
           IsEnumeration(Sym) OR IsSubrange(Sym) OR IsArray(Sym) OR
           IsUnbounded(Sym) OR IsProcType(Sym) OR IsSet(Sym)
         )
END IsAModula2Type ;


(*
   IsGnuAsmVolatile - returns TRUE if a GnuAsm symbol was defined as VOLATILE.
*)

PROCEDURE IsGnuAsmVolatile (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      GnuAsmSym: RETURN( GnuAsm.Volatile )

      ELSE
         InternalError('expecting GnuAsm symbol', __FILE__, __LINE__)
      END
   END
END IsGnuAsmVolatile ;


(*
   IsGnuAsm - returns TRUE if Sym is a GnuAsm symbol.
*)

PROCEDURE IsGnuAsm (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      RETURN( SymbolType=GnuAsmSym )
   END
END IsGnuAsm ;


(*
   IsRegInterface - returns TRUE if Sym is a RegInterface symbol.
*)

PROCEDURE IsRegInterface (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Symbols[Sym] DO
      RETURN( SymbolType=InterfaceSym )
   END
END IsRegInterface ;


(*
   GetParam - returns the ParamNo parameter from procedure ProcSym
*)

PROCEDURE GetParam (Sym: CARDINAL; ParamNo: CARDINAL) : CARDINAL ;
BEGIN
   CheckLegal(Sym) ;
   IF ParamNo=0
   THEN
      (* Parameter Zero is the return argument for the Function *)
      RETURN(GetType(Sym))
   ELSE
      RETURN(GetNthParam(Sym, ParamNo))
   END
END GetParam ;


(*
   GetFromIndex - return a value from list, i, at position, n.
*)

PROCEDURE GetFromIndex (i: Indexing.Index; n: CARDINAL) : CARDINAL ;
VAR
   p: POINTER TO CARDINAL ;
BEGIN
   p := Indexing.GetIndice(i, n) ;
   RETURN( p^ )
END GetFromIndex ;


(*
   PutIntoIndex - places value, v, into list, i, at position, n.
*)

PROCEDURE PutIntoIndex (VAR i: Indexing.Index; n: CARDINAL; v: CARDINAL) ;
VAR
   p: POINTER TO CARDINAL ;
BEGIN
   NEW(p) ;
   p^ := v ;
   Indexing.PutIndice(i, n, p)
END PutIntoIndex ;


(*
   Make2Tuple - creates and returns a 2 tuple from, a, and, b.
*)

PROCEDURE Make2Tuple (a, b: CARDINAL) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   NewSym(Sym) ;
   WITH Symbols[Sym] DO
      SymbolType := TupleSym ;
      WITH Tuple DO
         nTuple := 2 ;
         list := Indexing.InitIndex(1) ;
         PutIntoIndex(list, 1, a) ;
         PutIntoIndex(list, 2, b) ;
         InitWhereDeclared(At) ;
         InitWhereFirstUsed(At)
      END
   END ;
   RETURN( Sym )
END Make2Tuple ;


(*
   IsSizeSolved - returns true if the size of Sym is solved.
*)

PROCEDURE IsSizeSolved (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym    : RETURN( IsSolved(Procedure.Size) ) |
      ModuleSym       : RETURN( IsSolved(Module.Size) ) |
      VarSym          : RETURN( IsSolved(Var.Size) ) |
      TypeSym         : RETURN( IsSolved(Type.Size) ) |
      SetSym          : RETURN( IsSolved(Set.Size) ) |
      RecordSym       : RETURN( IsSolved(Record.Size) ) |
      VarientSym      : RETURN( IsSolved(Varient.Size) ) |
      EnumerationSym  : RETURN( IsSolved(Enumeration.Size) ) |
      DefImpSym       : RETURN( IsSolved(DefImp.Size) ) |
      PointerSym      : RETURN( IsSolved(Pointer.Size) ) |
      ArraySym        : RETURN( IsSolved(Array.Size) ) |
      RecordFieldSym  : RETURN( IsSolved(RecordField.Size) ) |
      VarientFieldSym : RETURN( IsSolved(VarientField.Size) ) |
      SubrangeSym     : RETURN( IsSolved(Subrange.Size) ) |
      SubscriptSym    : RETURN( IsSolved(Subscript.Size) ) |
      ProcTypeSym     : RETURN( IsSolved(ProcType.Size) ) |
      UnboundedSym    : RETURN( IsSolved(Unbounded.Size) )

      ELSE
         InternalError('not expecting this kind of symbol', __FILE__, __LINE__)
      END
   END
END IsSizeSolved ;


(*
   IsOffsetSolved - returns true if the Offset of Sym is solved.
*)

PROCEDURE IsOffsetSolved (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ModuleSym       : RETURN( IsSolved(Module.Offset) ) |
      VarSym          : RETURN( IsSolved(Var.Offset) ) |
      DefImpSym       : RETURN( IsSolved(DefImp.Offset) ) |
      RecordFieldSym  : RETURN( IsSolved(RecordField.Offset) ) |
      VarientFieldSym : RETURN( IsSolved(VarientField.Offset) )

      ELSE
         InternalError('not expecting this kind of symbol', __FILE__, __LINE__)
      END
   END
END IsOffsetSolved ;


(*
   IsValueSolved - returns true if the value of Sym is solved.
*)

PROCEDURE IsValueSolved (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ConstLitSym         : RETURN( IsSolved(ConstLit.Value) ) |
      ConstVarSym         : RETURN( IsSolved(ConstVar.Value) ) |
      EnumerationFieldSym : RETURN( IsSolved(EnumerationField.Value) ) |
      ConstStringSym      : RETURN( TRUE )

      ELSE
         InternalError('not expecting this kind of symbol', __FILE__, __LINE__)
      END
   END
END IsValueSolved ;


(*
   IsConstructorConstant - returns TRUE if constructor, Sym, is
                           defined by only constants.
*)

PROCEDURE IsConstructorConstant (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsConstructor(Sym)
   THEN
      WITH Symbols[Sym] DO
         CASE SymbolType OF

         ConstVarSym:  RETURN( IsValueConst(ConstVar.Value) ) |
         ConstLitSym:  RETURN( IsValueConst(ConstLit.Value) )

         ELSE
            InternalError('expecting Constructor', __FILE__, __LINE__)
         END
      END
   ELSE
      InternalError('expecting Constructor', __FILE__, __LINE__)
   END
END IsConstructorConstant ;


(*
   IsComposite - returns TRUE if symbol, sym, is a composite
                 type:  ie an ARRAY or RECORD.
*)

PROCEDURE IsComposite (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF sym=NulSym
   THEN
      RETURN( FALSE )
   ELSE
      sym := SkipType(sym) ;
      RETURN( IsArray(sym) OR IsRecord(sym) )
   END
END IsComposite ;


(*
   IsSumOfParamSizeSolved - has the sum of parameters been solved yet?
*)

PROCEDURE IsSumOfParamSizeSolved (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym: RETURN( IsSolved(Procedure.TotalParamSize) ) |
      ProcTypeSym : RETURN( IsSolved(ProcType.TotalParamSize) )

      ELSE
         InternalError('expecting Procedure or ProcType symbol', __FILE__, __LINE__)
      END
   END
END IsSumOfParamSizeSolved ;


(*
   PushSize - pushes the size of Sym.
*)

PROCEDURE PushSize (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym    : PushFrom(Procedure.Size) |
      ModuleSym       : PushFrom(Module.Size) |
      VarSym          : PushFrom(Var.Size) |
      TypeSym         : PushFrom(Type.Size) |
      SetSym          : PushFrom(Set.Size) |
      VarientSym      : PushFrom(Varient.Size) |
      RecordSym       : PushFrom(Record.Size) |
      EnumerationSym  : PushFrom(Enumeration.Size) |
      DefImpSym       : PushFrom(DefImp.Size) |
      PointerSym      : PushFrom(Pointer.Size) |
      ArraySym        : PushFrom(Array.Size) |
      RecordFieldSym  : PushFrom(RecordField.Size) |
      VarientFieldSym : PushFrom(VarientField.Size) |
      SubrangeSym     : PushFrom(Subrange.Size) |
      SubscriptSym    : PushFrom(Subscript.Size) |
      ProcTypeSym     : PushFrom(ProcType.Size) |
      UnboundedSym    : PushFrom(Unbounded.Size)

      ELSE
         InternalError('not expecting this kind of symbol', __FILE__, __LINE__)
      END
   END
END PushSize ;


(*
   PushOffset - pushes the Offset of Sym.
*)

PROCEDURE PushOffset (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ModuleSym       : PushFrom(Module.Offset) |
      VarSym          : PushFrom(Var.Offset) |
      DefImpSym       : PushFrom(DefImp.Offset) |
      RecordFieldSym  : PushFrom(RecordField.Offset) |
      VarientFieldSym : PushFrom(VarientField.Offset)

      ELSE
         InternalError('not expecting this kind of symbol', __FILE__, __LINE__)
      END
   END
END PushOffset ;


(*
   PushValue - pushes the Value of Sym onto the ALU stack.
*)

PROCEDURE PushValue (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ConstLitSym         : PushFrom(ConstLit.Value) |
      ConstVarSym         : PushFrom(ConstVar.Value) |
      EnumerationFieldSym : PushFrom(EnumerationField.Value) |
      ConstStringSym      : PushConstString(Sym)

      ELSE
         InternalError('not expecting this kind of symbol', __FILE__, __LINE__)
      END
   END
END PushValue ;


(*
   PushConstString - pushes the character string onto the ALU stack.
                     It assumes that the character string is only
                     one character long.
*)

PROCEDURE PushConstString (Sym: CARDINAL) ;
VAR
   a: ARRAY [0..10] OF CHAR ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ConstStringSym: WITH ConstString DO
                         IF Length=1
                         THEN
                            GetKey(String, a) ;
                            PushChar(a[0])
                         ELSE
                            WriteFormat0('ConstString must be length 1')
                         END
                      END

      ELSE
         InternalError('expecting ConstString symbol', __FILE__, __LINE__)
      END
   END
END PushConstString ;
   
   
(* 
   PushParamSize - push the size of parameter, ParamNo,
                   of procedure Sym onto the ALU stack.
*) 
 
PROCEDURE PushParamSize (Sym: CARDINAL; ParamNo: CARDINAL) ;  
VAR
   p, Type: CARDINAL ;
BEGIN
   CheckLegal(Sym) ;
   Assert(IsProcedure(Sym) OR IsProcType(Sym)) ;
   IF ParamNo=0
   THEN
      PushSize(GetType(Sym))
   ELSE
      (*
         can use GetNthParam but 1..n returns parameter.
         But 0 yields the function return type.

         Note that VAR Unbounded parameters and non VAR Unbounded parameters
              contain the unbounded descriptor. VAR unbounded parameters
              do NOT JUST contain an address re: other VAR parameters.
      *)
      IF IsVarParam(Sym, ParamNo) AND (NOT IsUnboundedParam(Sym, ParamNo))
      THEN
         PushSize(Address)     (* VAR parameters point to the variable *)
      ELSE
         p := GetNthParam(Sym, ParamNo) ; (* nth Parameter *)
         (*
            N.B. chose to get the Type of the parameter rather than the Var
            because ProcType's have Type but no Var associated with them.
         *)
         Type := GetType(p) ;  (* ie Variable from Procedure Sym *)
         Assert(p#NulSym) ;    (* If this fails then ParamNo is out of range *)
         PushSize(Type)
      END
   END
END PushParamSize ;

 
(*
   PushSumOfLocalVarSize - push the total size of all local variables
                           onto the ALU stack. 
*)
 
PROCEDURE PushSumOfLocalVarSize (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym,
      DefImpSym,
      ModuleSym   : PushSize(Sym)

      ELSE
         InternalError('expecting Procedure, DefImp or Module symbol', __FILE__, __LINE__)
      END
   END
END PushSumOfLocalVarSize ;


(*
   PushSumOfParamSize - push the total size of all parameters onto
                        the ALU stack.
*)

PROCEDURE PushSumOfParamSize (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym: PushFrom(Procedure.TotalParamSize) |
      ProcTypeSym : PushFrom(ProcType.TotalParamSize)

      ELSE
         InternalError('expecting Procedure or ProcType symbol', __FILE__, __LINE__)
      END
   END
END PushSumOfParamSize ;


(*
   PushVarSize - pushes the size of a variable, Sym.
                 The runtime size of Sym will depend upon its addressing mode,
                 RightValue has size PushSize(GetType(Sym)) and
                 LeftValue has size PushSize(Address) since it points to a
                 variable.
*)

PROCEDURE PushVarSize (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   Assert(IsVar(Sym)) ;
   IF GetMode(Sym)=LeftValue
   THEN
      PushSize(Address)
   ELSE
      Assert(GetMode(Sym)=RightValue) ;
      PushSize(GetType(Sym))
   END
END PushVarSize ;


(*
   PopValue - pops the ALU stack into Value of Sym.
*)

PROCEDURE PopValue (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ConstLitSym         : PopInto(ConstLit.Value) |
      ConstVarSym         : PopInto(ConstVar.Value) |
      EnumerationFieldSym : InternalError('cannot pop into an enumeration field', __FILE__, __LINE__)

      ELSE
         InternalError('symbol type not expected', __FILE__, __LINE__)
      END
   END
END PopValue ;


(*
   PopSize - pops the ALU stack into Size of Sym.
*)

PROCEDURE PopSize (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym    : PopInto(Procedure.Size) |
      ModuleSym       : PopInto(Module.Size) |
      VarSym          : PopInto(Var.Size) |
      TypeSym         : PopInto(Type.Size) |
      RecordSym       : PopInto(Record.Size) |
      VarientSym      : PopInto(Varient.Size) |
      EnumerationSym  : PopInto(Enumeration.Size) |
      DefImpSym       : PopInto(DefImp.Size) |
      PointerSym      : PopInto(Pointer.Size) |
      ArraySym        : PopInto(Array.Size) |
      RecordFieldSym  : PopInto(RecordField.Size) |
      VarientFieldSym : PopInto(VarientField.Size) |
      SubrangeSym     : PopInto(Subrange.Size) |
      SubscriptSym    : PopInto(Subscript.Size) |
      ProcTypeSym     : PopInto(ProcType.Size) |
      UnboundedSym    : PopInto(Unbounded.Size) |
      SetSym          : PopInto(Set.Size)

      ELSE
         InternalError('not expecting this kind of symbol', __FILE__, __LINE__)
      END
   END
END PopSize ;


(*
   PopOffset - pops the ALU stack into Offset of Sym.
*)

PROCEDURE PopOffset (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ModuleSym       : PopInto(Module.Offset) |
      VarSym          : PopInto(Var.Offset) |
      DefImpSym       : PopInto(DefImp.Offset) |
      RecordFieldSym  : PopInto(RecordField.Offset) |
      VarientFieldSym : PopInto(VarientField.Offset)

      ELSE
         InternalError('not expecting this kind of symbol', __FILE__, __LINE__)
      END
   END
END PopOffset ;


(*
   PopSumOfParamSize - pop the total value on the ALU stack as the
                       sum of all parameters.
*)

PROCEDURE PopSumOfParamSize (Sym: CARDINAL) ;
BEGIN
   CheckLegal(Sym) ;
   WITH Symbols[Sym] DO
      CASE SymbolType OF

      ProcedureSym: PopInto(Procedure.TotalParamSize) |
      ProcTypeSym : PopInto(ProcType.TotalParamSize)

      ELSE
         InternalError('expecting Procedure or ProcType symbol', __FILE__, __LINE__)
      END
   END
END PopSumOfParamSize ;


BEGIN
   Init
END SymbolTable.
