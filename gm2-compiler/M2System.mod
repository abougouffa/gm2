(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE M2System ;

(*
    Title      : M2System
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Mon Jul 10 20:24:31 2000
    Last edit  : Mon Jul 10 20:24:31 2000
    Description: gcc version of M2System. It defines the builtin types within the
                 SYSTEM.def module. Remember that these modules (SYSTEM.def, SYSTEM.mod)
                 really exist, but not all type definitions are expressed in this file.
                 We also need to tell the compiler the size of the data types.
*)

FROM SymbolTable IMPORT NulSym,
                        StartScope,
                        EndScope,
      	       	     	MakeConstLit,
                        MakeConstVar,
                        MakePointer,
                        MakeType,
                        MakeProcedure,
      	       	     	MakeSet,
      	       	     	MakeSubrange,
                        PutFunction,
                        PutType, PutPointer,
      	       	     	PutSet, PutVar,
      	       	     	PutSubrange,
                        PutExportQualified,
                        GetSym, GetSymName,
                        GetCurrentModule, SetCurrentModule,
                        PopValue,
                        PopSize ;

FROM M2Options IMPORT Iso, Pim2, Pedantic, DumpSystemExports ;
FROM NameKey IMPORT Name, MakeKey, NulName ;
FROM M2Batch IMPORT MakeDefinitionSource ;
FROM M2Base IMPORT Cardinal ;
FROM M2Size IMPORT Size, MakeSize ;
FROM M2Bitset IMPORT Bitset, GetBitsetMinMax, MakeBitset ;
FROM M2ALU IMPORT PushCard, PushIntegerTree, DivTrunc ;
FROM M2Error IMPORT InternalError ;
FROM Lists IMPORT List, InitList, IsItemInList, PutItemIntoList, GetItemFromList, NoOfItemsInList ;
FROM SymbolKey IMPORT SymbolTree, InitTree, GetSymKey, PutSymKey ;
FROM StrLib IMPORT StrEqual ;
FROM M2Printf IMPORT printf1 ;

FROM gccgm2 IMPORT Tree,
                   GetMaxFrom, GetMinFrom,
                   GetWordType, GetPointerType, GetByteType, GetISOLocType,
                   GetBitsPerUnit, GetSizeOf, BuildSize,
                   GetM2Integer8, GetM2Integer16, GetM2Integer32, GetM2Integer64,
                   GetM2Cardinal8, GetM2Cardinal16, GetM2Cardinal32, GetM2Cardinal64,
                   GetM2Word16, GetM2Word32, GetM2Word64, GetM2Real32,
                   GetM2Real64, GetM2Real96, GetM2Real128,
                   GetBitsetType, GetISOByteType, GetISOWordType ;


VAR
   MinValues,
   MaxValues  : SymbolTree ;
   SystemTypes: List ;


(*
   Init - 
*)

PROCEDURE Init ;
BEGIN
   InitList(SystemTypes) ;
   InitTree(MinValues) ;
   InitTree(MaxValues)
END Init ;


(*
   CreateMinMaxFor - creates the min and max values for, type, given gccType.
*)

PROCEDURE CreateMinMaxFor (type: CARDINAL; min, max: ARRAY OF CHAR; gccType: Tree) ;
VAR
   maxval, minval: CARDINAL ;
BEGIN
   maxval := MakeConstVar(MakeKey(max)) ;
   PushIntegerTree(GetMaxFrom(gccType)) ;
   PopValue(maxval) ;
   PutVar(maxval, type) ;
   PutSymKey(MaxValues, GetSymName(type), maxval) ;

   minval := MakeConstVar(MakeKey(min)) ;
   PushIntegerTree(GetMinFrom(gccType)) ;
   PopValue(minval) ;
   PutVar(minval, type) ;
   PutSymKey(MinValues, GetSymName(type), minval)
END CreateMinMaxFor ;


(*
   MapType - 
*)

PROCEDURE MapType (type: CARDINAL;
                   name, min, max: ARRAY OF CHAR;
                   needsExporting: BOOLEAN; t: Tree) ;
VAR
   minval,
   maxval: CARDINAL ;
   n     : Name ;
BEGIN
   PushIntegerTree(BuildSize(t, FALSE)) ;
   PopSize(type) ;
   IF IsItemInList(SystemTypes, type)
   THEN
      InternalError('not expecting system type to already be declared', __FILE__, __LINE__)
   END ;
   PutItemIntoList(SystemTypes, type) ;

   (* create min, max constants if type is ordinal *)
   IF (NOT StrEqual(min, '')) AND (NOT StrEqual(max, ''))
   THEN
      CreateMinMaxFor(type, min, max, t)
   END ;
   IF needsExporting AND DumpSystemExports
   THEN
      n := GetSymName(type) ;
      printf1('SYSTEM module creates type: %a\n', n)
   END
END MapType ;


(*
   AttemptToCreateType - 
*)

PROCEDURE AttemptToCreateType (name, min, max: ARRAY OF CHAR;
                               needsExporting: BOOLEAN; t: Tree) : CARDINAL ;
VAR
   type: CARDINAL ;
BEGIN
   IF t=NIL
   THEN
      (* GCC backend does not support this type *)
      RETURN( NulSym )
   ELSE
      (* create base type *)
      type := MakeType(MakeKey(name)) ;
      PutType(type, NulSym) ;  (* a Base Type *)
      MapType(type, name, min, max, needsExporting, t) ;
      RETURN( type )
   END
END AttemptToCreateType ;


(*
   AttemptToCreateSetType - 
*)

PROCEDURE AttemptToCreateSetType (name, bits: ARRAY OF CHAR;
                                  needsExporting: BOOLEAN; t: Tree) : CARDINAL ;
VAR
   low,
   high,
   subrange,
   type    : CARDINAL ;
BEGIN
   IF t=NIL
   THEN
      (* GCC backend does not support this type *)
      RETURN( NulSym )
   ELSE
      (* create base type *)
      type := MakeSet(MakeKey(name)) ;
      low := MakeConstLit(MakeKey('0')) ;
      high := MakeConstLit(MakeKey(bits)) ;
      subrange := MakeSubrange(NulName) ;
      PutSubrange(subrange, low, high, Cardinal) ;
      PutSet(type, subrange) ;
      MapType(type, name, '', '', needsExporting, t) ;
      RETURN( type )
   END
END AttemptToCreateSetType ;


(*
   MakeFixedSizedTypes - creates the SYSTEM fixed sized types providing the
                         gcc backend supports them.
*)

PROCEDURE MakeFixedSizedTypes ;
VAR
   type: CARDINAL ;
BEGIN
   type := AttemptToCreateType('INTEGER8', 'MinInteger8', 'MaxInteger8', TRUE, GetM2Integer8()) ;
   type := AttemptToCreateType('INTEGER16', 'MinInteger16', 'MaxInteger16', TRUE, GetM2Integer16()) ;
   type := AttemptToCreateType('INTEGER32', 'MinInteger32', 'MaxInteger32', TRUE, GetM2Integer32()) ;
   type := AttemptToCreateType('INTEGER64', 'MinInteger64', 'MaxInteger64', TRUE, GetM2Integer64()) ;

   type := AttemptToCreateType('CARDINAL8', 'MinCardinal8', 'MaxCardinal8', TRUE, GetM2Cardinal8()) ;
   type := AttemptToCreateType('CARDINAL16', 'MinCardinal16', 'MaxCardinal16', TRUE, GetM2Cardinal16()) ;
   type := AttemptToCreateType('CARDINAL32', 'MinCardinal32', 'MaxCardinal32', TRUE, GetM2Cardinal32()) ;
   type := AttemptToCreateType('CARDINAL64', 'MinCardinal64', 'MaxCardinal64', TRUE, GetM2Cardinal64()) ;

   type := AttemptToCreateType('WORD16', '', '', TRUE, GetM2Word16()) ;
   type := AttemptToCreateType('WORD32', '', '', TRUE, GetM2Word32()) ;
   type := AttemptToCreateType('WORD64', '', '', TRUE, GetM2Word64()) ;

   type := AttemptToCreateSetType('SET8' , '8' , TRUE, GetISOLocType()) ;
   type := AttemptToCreateSetType('SET16', '16', TRUE, GetM2Word16()) ;
   type := AttemptToCreateSetType('SET32', '32', TRUE, GetM2Word32()) ;

   type := AttemptToCreateType('REAL32', '', '', TRUE, GetM2Real32()) ;
   type := AttemptToCreateType('REAL64', '', '', TRUE, GetM2Real64()) ;
   type := AttemptToCreateType('REAL96', '', '', TRUE, GetM2Real96()) ;
   type := AttemptToCreateType('REAL128', '', '', TRUE, GetM2Real128())
END MakeFixedSizedTypes ;


(*
   InitPIMTypes - 
*)

PROCEDURE InitPIMTypes ;
VAR
   min, max: CARDINAL ;
BEGIN
   Loc := AttemptToCreateType('LOC', '', '', TRUE, GetISOLocType()) ;
   Word := AttemptToCreateType('WORD', '', '', TRUE, GetWordType()) ;
   Byte := AttemptToCreateType('BYTE', '', '', TRUE, GetByteType()) ;

   (* ADDRESS = POINTER TO BYTE *)

   Address := MakePointer(MakeKey('ADDRESS')) ;
   PutPointer(Address, Byte) ;                (* Base Type       *)
   MapType(Address, 'ADDRESS', '', '', TRUE, GetPointerType()) ;

   IF NOT Iso
   THEN
      MakeBitset ;
      MapType(Bitset, 'BITSET', '', '', TRUE, GetBitsetType()) ;
      GetBitsetMinMax(min, max) ;
      PutSymKey(MaxValues, GetSymName(Bitset), max) ;
      PutSymKey(MinValues, GetSymName(Bitset), min)
   END
END InitPIMTypes ;


(*
   InitISOTypes - 
*)

PROCEDURE InitISOTypes ;
VAR
   MinLoc, MaxLoc: CARDINAL ;
BEGIN
   Loc := AttemptToCreateType('LOC', 'MinLoc', 'MaxLoc', TRUE, GetISOLocType()) ;
   Address := AttemptToCreateType('ADDRESS', '', '', TRUE, GetPointerType()) ;

   Byte := AttemptToCreateType('BYTE', '', '', TRUE, GetISOByteType()) ;
   Word := AttemptToCreateType('WORD', '', '', TRUE, GetISOWordType()) ;

   (* CreateMinMaxFor(Loc, 'MinLoc', 'MaxLoc', GetISOLocType()) *)
END InitISOTypes ;


(*
   InitSystem - creates the system dependant types and procedures.
                Note that they are not exported here, but they are
                exported in the textual module: SYSTEM.def.
                We build our system types from those given in the gcc
                backend. Essentially we perform double book keeping.
*)

PROCEDURE InitSystem ;
VAR
   Previous              : CARDINAL ;
   MinWord   , MaxWord,
   MinAddress, MaxAddress,
   MinLoc    , MaxLoc,
   MinByte   , MaxByte   : CARDINAL ;
BEGIN
   Init ;

   (* create SYSTEM module *)
   System := MakeDefinitionSource(MakeKey('SYSTEM')) ;
   StartScope(System) ;
   Previous := GetCurrentModule() ;
   SetCurrentModule(System) ;

   IF Iso
   THEN
      InitISOTypes
   ELSE
      InitPIMTypes ;
      (* SIZE is declared in SYSTEM.def in PIM-2 but not PIM-[34] *)
      IF Pim2
      THEN
         MakeSize
      END
   END ;

   (* And now the predefined pseudo functions *)

   Adr := MakeProcedure(MakeKey('ADR')) ;         (* Function        *)
   PutFunction(Adr, Address) ;                    (* Return Type     *)
                                                  (* Address         *)

   TSize := MakeProcedure(MakeKey('TSIZE')) ;     (* Function        *)
   PutFunction(TSize, Cardinal) ;                 (* Return Type     *)
                                                  (* Cardinal        *)

   (* and the ISO specific predefined pseudo functions *)

   AddAdr := MakeProcedure(MakeKey('ADDADR')) ;   (* Function        *)
   PutFunction(AddAdr, Address) ;                 (* Return Type     *)

   SubAdr := MakeProcedure(MakeKey('SUBADR')) ;   (* Function        *)
   PutFunction(SubAdr, Address) ;                 (* Return Type     *)

   DifAdr := MakeProcedure(MakeKey('DIFADR')) ;   (* Function        *)
   PutFunction(DifAdr, Address) ;                 (* Return Type     *)

   MakeAdr := MakeProcedure(MakeKey('MAKEADR')) ; (* Function        *)
   PutFunction(MakeAdr, Address) ;                (* Return Type     *)

   (* the return value for ROTATE, SHIFT and CAST is actually the
      same as the first parameter, this is faked in M2Quads *)

   Rotate := MakeProcedure(MakeKey('ROTATE')) ;   (* Function        *)
   Shift := MakeProcedure(MakeKey('SHIFT')) ;     (* Function        *)
   Cast := MakeProcedure(MakeKey('CAST')) ;       (* Function        *)

   CreateMinMaxFor(Word, 'MinWord', 'MaxWord', GetWordType()) ;
   CreateMinMaxFor(Address, 'MinAddress', 'MaxAddress', GetPointerType()) ;
   CreateMinMaxFor(Byte, 'MinByte', 'MaxByte', GetByteType()) ;

   MakeFixedSizedTypes ;

   EndScope ;
   SetCurrentModule(Previous)
END InitSystem ;


(*
   GetSystemTypeMinMax - returns the minimum and maximum values for a given system type.
*)

PROCEDURE GetSystemTypeMinMax (type: CARDINAL; VAR min, max: CARDINAL) ;
BEGIN
   IF IsItemInList(SystemTypes, type)
   THEN
      min := GetSymKey(MinValues, GetSymName(type)) ;
      max := GetSymKey(MaxValues, GetSymName(type))
   ELSE
      InternalError('system does not know about this type', __FILE__, __LINE__)
   END
END GetSystemTypeMinMax ;


(*
   IsISOPseudoSystemFunction - 
*)

PROCEDURE IsISOPseudoSystemFunction (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Iso AND ((Sym=AddAdr) OR (Sym=SubAdr) OR (Sym=DifAdr) OR
                    (Sym=MakeAdr) OR (Sym=Rotate) OR (Sym=Shift) OR
                    (Sym=Cast)) )
END IsISOPseudoSystemFunction ;


(*
   IsPIMPseudoSystemFunction - returns TRUE if Sym is specifically a PIM
                               system function.
*)

PROCEDURE IsPIMPseudoSystemFunction (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (NOT Iso) AND ((Sym=Size) OR (Sym=Shift) OR (Sym=Rotate)) )
END IsPIMPseudoSystemFunction ;


(*
   IsPseudoSystemFunction - returns true if Sym is a SYSTEM pseudo function.
*)

PROCEDURE IsPseudoSystemFunction (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (Sym=Adr) OR (Sym=TSize) OR
           IsPIMPseudoSystemFunction(Sym) OR
           IsISOPseudoSystemFunction(Sym) )
END IsPseudoSystemFunction ;


(*
   IsPseudoSystemFunctionConstExpression - returns TRUE if this procedure
                                           is legal in a constant expression.
*)

PROCEDURE IsPseudoSystemFunctionConstExpression (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (Sym=Size) OR (Sym=TSize) OR (Sym=Rotate) OR (Sym=Shift) OR
          (Iso AND ((Sym=Cast) OR (Sym=MakeAdr)))
         )
END IsPseudoSystemFunctionConstExpression ;


(*
   IsSystemType - returns TRUE if Sym is a SYSTEM (inbuilt) type.
                  It does not search your SYSTEM implementation module.
*)

PROCEDURE IsSystemType (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsItemInList(SystemTypes, Sym) )
END IsSystemType ;


(*
   GetSafeSystem - 
*)

PROCEDURE GetSafeSystem (name: Name) : CARDINAL ;
VAR
   sym,
   i, n: CARDINAL ;
BEGIN
   n := NoOfItemsInList(SystemTypes) ;
   i := 1 ;
   WHILE i<=n DO
      sym := GetItemFromList(SystemTypes, i) ;
      IF GetSymName(sym)=name
      THEN
         RETURN( sym )
      END ;
      INC(i)
   END ;
   RETURN( NulSym )
END GetSafeSystem ;


(*
   IntegerN - returns the symbol associated with INTEGER[N].
              NulSym is returned if the type does not exist.
*)

PROCEDURE IntegerN (bitlength: CARDINAL) : CARDINAL ;
BEGIN
   CASE bitlength OF

   8 :  RETURN( GetSafeSystem(MakeKey('INTEGER8')) ) |
   16:  RETURN( GetSafeSystem(MakeKey('INTEGER16')) ) |
   32:  RETURN( GetSafeSystem(MakeKey('INTEGER32')) ) |
   64:  RETURN( GetSafeSystem(MakeKey('INTEGER64')) )

   ELSE
      InternalError('system does not know about this type', __FILE__, __LINE__)
   END
END IntegerN ;


(*
   CardinalN - returns the symbol associated with CARDINAL[N].
               NulSym is returned if the type does not exist.
*)

PROCEDURE CardinalN (bitlength: CARDINAL) : CARDINAL ;
BEGIN
   CASE bitlength OF

   8 :  RETURN( GetSafeSystem(MakeKey('CARDINAL8')) ) |
   16:  RETURN( GetSafeSystem(MakeKey('CARDINAL16')) ) |
   32:  RETURN( GetSafeSystem(MakeKey('CARDINAL32')) ) |
   64:  RETURN( GetSafeSystem(MakeKey('CARDINAL64')) )

   ELSE
      InternalError('system does not know about this type', __FILE__, __LINE__)
   END
END CardinalN ;


(*
   WordN - returns the symbol associated with WORD[N].
           NulSym is returned if the type does not exist.
*)

PROCEDURE WordN (bitlength: CARDINAL) : CARDINAL ;
BEGIN
   CASE bitlength OF

   16:  RETURN( GetSafeSystem(MakeKey('WORD16')) ) |
   32:  RETURN( GetSafeSystem(MakeKey('WORD32')) ) |
   64:  RETURN( GetSafeSystem(MakeKey('WORD64')) )

   ELSE
      InternalError('system does not know about this type', __FILE__, __LINE__)
   END
END WordN ;


(*
   SetN - returns the symbol associated with SET[N].
          NulSym is returned if the type does not exist.
*)

PROCEDURE SetN (bitlength: CARDINAL) : CARDINAL ;
BEGIN
   CASE bitlength OF

   8 :  RETURN( GetSafeSystem(MakeKey('SET8')) ) |
   16:  RETURN( GetSafeSystem(MakeKey('SET16')) ) |
   32:  RETURN( GetSafeSystem(MakeKey('SET32')) )

   ELSE
      InternalError('system does not know about this type', __FILE__, __LINE__)
   END
END SetN ;


(*
   RealN - returns the symbol associated with REAL[N].
           NulSym is returned if the type does not exist.
*)

PROCEDURE RealN (bitlength: CARDINAL) : CARDINAL ;
BEGIN
   CASE bitlength OF

   32 :  RETURN( GetSafeSystem(MakeKey('REAL32')) ) |
   64 :  RETURN( GetSafeSystem(MakeKey('REAL64')) ) |
   96 :  RETURN( GetSafeSystem(MakeKey('REAL96')) ) |
   128:  RETURN( GetSafeSystem(MakeKey('REAL128')) )

   ELSE
      InternalError('system does not know about this type', __FILE__, __LINE__)
   END
END RealN ;


(*
   IsIntegerN - returns the TRUE if, Sym, is one of the SYSTEM
                INTEGER types (not the base INTEGER type).
*)

PROCEDURE IsIntegerN (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (Sym#NulSym) AND
          ((Sym=IntegerN(8)) OR (Sym=IntegerN(16)) OR
           (Sym=IntegerN(32)) OR (Sym=IntegerN(64)))
         )
END IsIntegerN ;


(*
   IsCardinalN - returns the TRUE if, Sym, is one of the SYSTEM
                 CARDINAL types (not the base CARDINAL type).
*)

PROCEDURE IsCardinalN (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (Sym#NulSym) AND
          ((Sym=CardinalN(8)) OR (Sym=CardinalN(16)) OR
           (Sym=CardinalN(32)) OR (Sym=CardinalN(64)))
         )
END IsCardinalN ;


(*
   IsWordN - returns the TRUE if, Sym, is one of the SYSTEM
             WORD[n] types (not the default SYSTEM WORD type).
*)

PROCEDURE IsWordN (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (Sym#NulSym) AND
          ((Sym=WordN(16)) OR
           (Sym=WordN(32)) OR (Sym=WordN(64)))
         )
END IsWordN ;


(*
   IsSetN - returns the TRUE if, Sym, is one of the SYSTEM
            SET[n] types (not the default SYSTEM BITSET type).
*)

PROCEDURE IsSetN (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (Sym#NulSym) AND
          ((Sym=SetN(8)) OR (Sym=SetN(16)) OR
           (Sym=SetN(32)) OR (Sym=SetN(64)))
         )
END IsSetN ;


(*
   IsRealN - returns the TRUE if, Sym, is one of the SYSTEM
             REAL[n] types (not the default base REAL type).
*)

PROCEDURE IsRealN (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (Sym#NulSym) AND
          ((Sym=RealN(16)) OR
           (Sym=RealN(32)) OR (Sym=RealN(64)))
         )
END IsRealN ;


END M2System.
