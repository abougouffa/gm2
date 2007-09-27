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
                        GetSym,
                        PopValue,
                        PopSize ;

FROM M2Options IMPORT Iso, Pim2, Pedantic ;
FROM NameKey IMPORT MakeKey, NulName ;
FROM M2Batch IMPORT MakeDefinitionSource ;
FROM M2Base IMPORT Cardinal ;
FROM M2Size IMPORT Size, MakeSize ;
FROM M2Bitset IMPORT Bitset, GetBitsetMinMax, MakeBitset ;
FROM M2ALU IMPORT PushCard, PushIntegerTree, DivTrunc ;
FROM M2Error IMPORT InternalError ;
FROM gccgm2 IMPORT GetMaxFrom, GetMinFrom,
                   GetWordType, GetPointerType, GetByteType, GetISOLocType,
                   GetBitsPerUnit, GetSizeOf, BuildSize ;

VAR
   MinWord   , MaxWord,
   MinAddress, MaxAddress,
   MinLoc    , MaxLoc,
   MinByte   , MaxByte   : CARDINAL ;


(*
   InitPIMTypes - 
*)

PROCEDURE InitPIMTypes ;
BEGIN
   Loc := NulSym ;

   Word := MakeType(MakeKey('WORD')) ;
   PutType(Word, NulSym) ;                    (* Base Type       *)
   PushIntegerTree(BuildSize(GetWordType(), FALSE)) ;
   PopSize(Word) ;

   Byte := MakeType(MakeKey('BYTE')) ;
   PutType(Byte, NulSym) ;                    (* Base Type       *)
   PushCard(GetBitsPerUnit()) ;
   PushCard(8) ;
   DivTrunc ;
   PopSize(Byte) ;

   (* ADDRESS = POINTER TO BYTE *)

   Address := MakePointer(MakeKey('ADDRESS')) ;
   PutPointer(Address, Byte) ;                (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetPointerType())) ;
   PopSize(Address) ;

   IF NOT Iso
   THEN
      MakeBitset
   END
END InitPIMTypes ;


(*
   InitISOTypes - 
*)

PROCEDURE InitISOTypes ;
BEGIN
   Loc := MakeType(MakeKey('LOC')) ;
   PutType(Loc, NulSym) ;                     (* Base Type       *)
   PushCard(1) ;
   PopSize(Loc) ;

   Address := MakePointer(MakeKey('ADDRESS')) ;
   PutPointer(Address, Loc) ;                 (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetPointerType())) ;
   PopSize(Address) ;

   Byte := MakeType(MakeKey('BYTE')) ;
   PutType(Byte, NulSym) ;                    (* Base Type       *)
   PushCard(GetBitsPerUnit()) ;
   PushCard(8) ;
   DivTrunc ;
   PopSize(Byte) ;

   Word := MakeType(MakeKey('WORD')) ;
   PutType(Word, NulSym) ;                    (* Base Type       *)
   PushIntegerTree(BuildSize(GetWordType(), FALSE)) ;
   PopSize(Word) ;

   (* MaxLoc *)
   MaxLoc := MakeConstVar(MakeKey('MaxLoc')) ;
   PushIntegerTree(GetMaxFrom(GetISOLocType())) ;
   PopValue(MaxLoc) ;
   PutVar(MaxLoc, Loc) ;

   (* MinLoc *)
   MinLoc := MakeConstVar(MakeKey('MinLoc')) ;
   PushIntegerTree(GetMinFrom(GetISOLocType())) ;
   PopValue(MinLoc) ;
   PutVar(MinLoc, Loc) ;

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
   Previous: CARDINAL ;
BEGIN
   (* create SYSTEM module *)
   System := MakeDefinitionSource(MakeKey('SYSTEM')) ;
   StartScope(System) ;

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

   (* MaxWord *)
   MaxWord := MakeConstVar(MakeKey('MaxWord')) ;
   PushIntegerTree(GetMaxFrom(GetWordType())) ;
   PopValue(MaxWord) ;
   PutVar(MaxWord, Word) ;

   (* MinWord *)
   MinWord := MakeConstVar(MakeKey('MinWord')) ;
   PushIntegerTree(GetMinFrom(GetWordType())) ;
   PopValue(MinWord) ;
   PutVar(MinWord, Word) ;

   (* MaxAddress *)
   MaxAddress := MakeConstVar(MakeKey('MaxAddress')) ;
   PushIntegerTree(GetMaxFrom(GetPointerType())) ;
   PopValue(MaxAddress) ;
   PutVar(MaxAddress, Address) ;

   (* MinAddress *)
   MinAddress := MakeConstVar(MakeKey('MinAddress')) ;
   PushIntegerTree(GetMinFrom(GetPointerType())) ;
   PopValue(MinAddress) ;
   PutVar(MinAddress, Address) ;

   (* MaxByte *)
   MaxByte := MakeConstVar(MakeKey('MaxByte')) ;
   PushIntegerTree(GetMaxFrom(GetByteType())) ;
   PopValue(MaxByte) ;
   PutVar(MaxByte, Byte) ;

   (* MinByte *)
   MinByte := MakeConstVar(MakeKey('MinByte')) ;
   PushIntegerTree(GetMinFrom(GetByteType())) ;
   PopValue(MinByte) ;
   PutVar(MinByte, Byte) ;

   EndScope
END InitSystem ;


(*
   GetSystemTypeMinMax - returns the minimum and maximum values for a given system type.
*)

PROCEDURE GetSystemTypeMinMax (type: CARDINAL; VAR min, max: CARDINAL) ;
BEGIN
   IF type=Word
   THEN
      min := MinWord ;
      max := MaxWord
   ELSIF type=Byte
   THEN
      min := MinByte ;
      max := MaxByte
   ELSIF type=Address
   THEN
      min := MinAddress ;
      max := MaxAddress
   ELSIF (type=Bitset) AND (NOT Iso)
   THEN
      GetBitsetMinMax(min, max)
   ELSIF (type=Loc) AND Iso
   THEN
      min := MinLoc ;
      max := MaxLoc
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
   RETURN(
          (Sym=Word)    OR (Sym=Byte) OR
          (Sym=Address) OR ((Sym=Bitset) AND (NOT Iso)) OR
          ((Sym=Loc) AND Iso)
         )
END IsSystemType ;


END M2System.
