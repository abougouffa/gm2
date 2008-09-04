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

IMPLEMENTATION MODULE M2Base ;

(*
    Title      : M2Base
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Mon Jul 10 20:16:54 2000
    Description: gcc version of M2Base. This module initializes the front end
                 symbol table with the base types. We collect the size of the
                 base types and range of values from the gcc backend.
*)

FROM DynamicStrings IMPORT InitString, String, Mark, InitStringCharStar ;
FROM M2LexBuf IMPORT GetTokenNo ;
FROM NameKey IMPORT MakeKey, WriteKey, KeyToCharStar ;
FROM SYSTEM IMPORT WORD ;

FROM M2Error IMPORT Error, NewError, NewWarning,
                    ErrorFormat0, ErrorFormat1, ErrorFormat2,
                    InternalError, ChainError, WriteFormat1, ErrorString, FlushErrors ;

FROM FormatStrings IMPORT Sprintf2 ;
FROM StrLib IMPORT StrLen ;

FROM SymbolTable IMPORT ModeOfAddr,
                        MakeModule, MakeType, PutType,
                        MakeEnumeration, PutFieldEnumeration,
                        MakeProcType,
                        MakeProcedure, PutFunction,
                        MakeRecord, PutFieldRecord,
                        MakeConstVar, PutConst,
                        MakeConstLit, MakeTemporary,
                        MakeVar, PutVar,
                        MakeSubrange, PutSubrange, IsSubrange,
                        IsEnumeration, IsSet, IsPointer, IsType, IsUnknown,
                        IsHiddenType, IsProcType,
                        GetType, GetLowestType, GetDeclared, SkipType,
                        SetCurrentModule,
                        StartScope, EndScope, PseudoScope,
                        ForeachFieldEnumerationDo,
                        RequestSym, GetSymName, NulSym,
                        PutImported, GetExported,
                        PopSize, PopValue, PushValue,
                        FromModuleGetSym, GetSym,
                        IsExportQualified, IsExportUnQualified ;

FROM M2ALU IMPORT PushIntegerTree, PushRealTree, PushCard, Equ, Gre, Less ;
FROM M2Batch IMPORT MakeDefinitionSource ;
FROM M2Bitset IMPORT Bitset, GetBitsetMinMax, MakeBitset ;
FROM M2Size IMPORT Size, MakeSize ;

FROM M2System IMPORT Address, Byte, Word, System, Loc, InitSystem, 
                     IntegerN, CardinalN, WordN, SetN, RealN,
                     IsCardinalN, IsIntegerN ;

FROM M2Options IMPORT BoundsChecking, ReturnChecking,
                      NilChecking, CaseElseChecking,
                      DivModRemChecking,
                      Iso, Pim, Pim2, Pim3 ;

FROM gccgm2 IMPORT GetSizeOf, GetIntegerType,
                   GetM2IntegerType, GetM2CharType,
                   GetMaxFrom, GetMinFrom, GetRealType,
                   GetM2LongIntType, GetLongRealType, GetProcType,
                   GetM2ShortRealType, GetM2RealType,
                   GetM2LongRealType, GetM2LongCardType,
                   GetM2ShortIntType, GetM2ShortCardType,
                   GetM2CardinalType, GetPointerType, GetWordType,
                   GetByteType, GetISOWordType, GetISOByteType,
                   GetISOLocType, GetM2RType, GetM2ZType,
                   BuildIntegerConstant ;

TYPE
   Compatability = (expression, assignment, parameter) ;
   MetaType      = (const, word, byte, address, chr,
                    normint, shortint, longint,
                    normcard, shortcard, longcard,
                    pointer, enum,
                    real, shortreal, longreal,
                    set, opaque, loc, rtype, ztype,
                    int8, int16, int32, int64,
                    card8, card16, card32, card64,
                    word16, word32, word64,
                    real32, real64, real96, real128,
                    set8, set16, set32,
                    unknown) ;
   Compatible    = (uninitialized, no, warnfirst, warnsecond,
                    first, second) ;

(* %%%FORWARD%%%
PROCEDURE InitBaseConstants ; FORWARD ;
PROCEDURE InitBaseSimpleTypes ; FORWARD ;
PROCEDURE InitBaseFunctions ; FORWARD ;
PROCEDURE InitBaseProcedures ; FORWARD ;
PROCEDURE InitCompatibilityMatrices ; FORWARD ;
PROCEDURE IsCompatible (t1, t2: CARDINAL; kind: Compatability) : Compatible ; FORWARD ;
   %%%FORWARD%%% *)

TYPE
   CompatibilityArray = ARRAY MetaType, MetaType OF Compatible ;

VAR
   Param,
   Expr,
   Ass        : CompatibilityArray ;
   Ord,
   OrdS, OrdL,
   Float, FloatS,
   FloatL,
   Trunc, TruncS,
   TruncL,
   m2rts,
   MinReal,
   MaxReal,
   MinShortReal,
   MaxShortReal,
   MinLongReal,
   MaxLongReal,
   MinLongInt,
   MaxLongInt,
   MinLongCard,
   MaxLongCard,
   MinShortInt,
   MaxShortInt,
   MinShortCard,
   MaxShortCard,
   MinChar,
   MaxChar,
   MinCardinal,
   MaxCardinal,
   MinInteger,
   MaxInteger,
   MaxEnum,
   MinEnum    : CARDINAL ;



(*
   InitBase - initializes the base types and procedures
              used in the Modula-2 compiler.
*)

PROCEDURE InitBase (VAR Sym: CARDINAL) ;
BEGIN
   Sym := MakeModule(MakeKey('_BaseTypes')) ;
   SetCurrentModule(Sym) ;
   StartScope(Sym) ;

   InitBaseSimpleTypes ;

   (* initialise the SYSTEM module before we used CARDINAL and ADDRESS! *)
   InitSystem ;

   IF Iso
   THEN
      MakeBitset  (* we do this after SYSTEM has been created as BITSET is dependant upon WORD *)
   END ;

   InitBaseConstants ;
   InitBaseFunctions ;
   InitBaseProcedures ;

   (*
      Note: that we do end the Scope since we keep the symbol to the head
            of the base scope. This head of base scope is searched
            when all other scopes fail to deliver a symbol.
   *)
   EndScope ;
   InitCompatibilityMatrices
END InitBase ;


(*
   IsNeededAtRunTime - returns TRUE if procedure, sym, is a
                       runtime procedure. Ie a procedure which is
                       not a pseudo procedure and which is implemented
                       in M2RTS or SYSTEM and also exported.
*)

PROCEDURE IsNeededAtRunTime (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          ((FromModuleGetSym(GetSymName(sym), System)=sym) OR
           (FromModuleGetSym(GetSymName(sym), m2rts)=sym)) AND
          (IsExportQualified(sym) OR IsExportUnQualified(sym))
         )
END IsNeededAtRunTime ;


(*
   InitBaseConstants - initialises the base constant NIL.
*)

PROCEDURE InitBaseConstants ;
BEGIN
   Nil := MakeConstVar(MakeKey('NIL')) ;
   PutConst(Nil, Address) ;
   PushCard(0) ;
   PopValue(Nil)
END InitBaseConstants ;


(*
   InitBaseSimpleTypes - initialises the base simple types,
                         CARDINAL, INTEGER, CHAR, BOOLEAN.
*)

PROCEDURE InitBaseSimpleTypes ;
VAR
   Zero,
   MaxCard: CARDINAL ;
BEGIN
   ZType := MakeType(MakeKey('_M2_Ztype')) ;
   PutType(ZType, NulSym) ;                   (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2ZType())) ;
   PopSize(ZType) ;

   RType := MakeType(MakeKey('_M2_Rtype')) ;
   PutType(RType, NulSym) ;                   (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2RType())) ;
   PopSize(RType) ;

   Integer := MakeType(MakeKey('INTEGER')) ;
   PutType(Integer, NulSym) ;                 (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2IntegerType())) ;
   PopSize(Integer) ;

   Zero := MakeConstLit(MakeKey('0')) ;
   MaxCard := MakeTemporary(ImmediateValue) ;

   Cardinal := MakeType(MakeKey('CARDINAL')) ;
   PutType(Cardinal, NulSym) ;
                                              (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2CardinalType())) ;
   PopSize(Cardinal) ;

   LongInt := MakeType(MakeKey('LONGINT')) ;
   PutType(LongInt, NulSym) ;                 (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2LongIntType())) ;
   PopSize(LongInt) ;

   LongCard := MakeType(MakeKey('LONGCARD')) ;
   PutType(LongCard, NulSym) ;                (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2LongCardType())) ;
   PopSize(LongCard) ;

   ShortInt := MakeType(MakeKey('SHORTINT')) ;
   PutType(ShortInt, NulSym) ;                (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2ShortIntType())) ;
   PopSize(ShortInt) ;

   ShortCard := MakeType(MakeKey('SHORTCARD')) ;
   PutType(ShortCard, NulSym) ;               (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2ShortCardType())) ;
   PopSize(ShortCard) ;

   Real := MakeType(MakeKey('REAL')) ;
   PutType(Real, NulSym) ;                    (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2RealType())) ;
   PopSize(Real) ;

   ShortReal := MakeType(MakeKey('SHORTREAL')) ;
   PutType(ShortReal, NulSym) ;               (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2ShortRealType())) ;
   PopSize(ShortReal) ;

   LongReal := MakeType(MakeKey('LONGREAL')) ;
   PutType(LongReal, NulSym) ;                (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2LongRealType())) ;
   PopSize(LongReal) ;

   Char := MakeType(MakeKey('CHAR')) ;
   PutType(Char, NulSym) ;                    (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2CharType())) ;
   PopSize(Char) ;

   (*
      Boolean = (FALSE, TRUE) ;
   *)
   Boolean := MakeEnumeration(MakeKey('BOOLEAN')) ;

   PutFieldEnumeration(Boolean, MakeKey('FALSE')) ;
   PutFieldEnumeration(Boolean, MakeKey('TRUE')) ;

   True  := RequestSym(MakeKey('TRUE')) ;
   False := RequestSym(MakeKey('FALSE')) ;

   Proc := MakeProcType(MakeKey('PROC')) ;
   PushIntegerTree(GetSizeOf(GetProcType())) ;
   PopSize(Proc) ;

   (* MinChar *)
   MinChar := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMinFrom(GetM2CharType())) ;
   PopValue(MinChar) ;
   PutVar(MinChar, Char) ;

   (* MaxChar *)
   MaxChar := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMaxFrom(GetM2CharType())) ;
   PopValue(MaxChar) ;
   PutVar(MaxChar, Char) ;

   (* MinInteger *)
   MinInteger := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMinFrom(GetM2IntegerType())) ;
   PopValue(MinInteger) ;
   PutVar(MinInteger, Integer) ;

   (* MaxInteger *)
   MaxInteger := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMaxFrom(GetM2IntegerType())) ;
   PopValue(MaxInteger) ;
   PutVar(MaxInteger, Integer) ;

   (* MinCardinal *)
   MinCardinal := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMinFrom(GetM2CardinalType())) ;
   PopValue(MinCardinal) ;
   PutVar(MinCardinal, Cardinal) ;

   (* MaxCardinal *)
   MaxCardinal := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMaxFrom(GetM2CardinalType())) ;
   PopValue(MaxCardinal) ;
   PutVar(MaxCardinal, Cardinal) ;

   (* MinLongInt *)
   MinLongInt := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMinFrom(GetM2LongIntType())) ;
   PopValue(MinLongInt) ;
   PutVar(MinLongInt, LongInt) ;

   (* MaxLongInt *)
   MaxLongInt := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMaxFrom(GetM2LongIntType())) ;
   PopValue(MaxLongInt) ;
   PutVar(MaxLongInt, LongInt) ;

   (* MinLongCard *)
   MinLongCard := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMinFrom(GetM2LongCardType())) ;
   PopValue(MinLongCard) ;
   PutVar(MinLongCard, LongCard) ;

   (* MinLongCard *)
   MaxLongCard := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMaxFrom(GetM2LongCardType())) ;
   PopValue(MaxLongCard) ;
   PutVar(MaxLongCard, LongCard) ;

   (* MinReal *)
   MinReal := MakeTemporary(ImmediateValue) ;
   PushRealTree(GetMinFrom(GetM2RealType())) ;
   PopValue(MinReal) ;
   PutVar(MinReal, Real) ;

   (* MaxReal *)
   MaxReal := MakeTemporary(ImmediateValue) ;
   PushRealTree(GetMaxFrom(GetM2RealType())) ;
   PopValue(MaxReal) ;
   PutVar(MaxReal, Real) ;

   (* MinShortReal *)
   MinShortReal := MakeTemporary(ImmediateValue) ;
   PushRealTree(GetMinFrom(GetM2ShortRealType())) ;
   PopValue(MinShortReal) ;
   PutVar(MinShortReal, ShortReal) ;

   (* MaxShortReal *)
   MaxShortReal := MakeTemporary(ImmediateValue) ;
   PushRealTree(GetMaxFrom(GetM2ShortRealType())) ;
   PopValue(MaxShortReal) ;
   PutVar(MaxShortReal, ShortReal) ;

   (* MinLongReal *)
   MinLongReal := MakeTemporary(ImmediateValue) ;
   PushRealTree(GetMinFrom(GetM2LongRealType())) ;
   PopValue(MinLongReal) ;
   PutVar(MinLongReal, LongReal) ;

   (* MaxLongReal *)
   MaxLongReal := MakeTemporary(ImmediateValue) ;
   PushRealTree(GetMaxFrom(GetM2LongRealType())) ;
   PopValue(MaxLongReal) ;
   PutVar(MaxLongReal, LongReal)

END InitBaseSimpleTypes ;


(*
   FindMinMaxEnum - finds the minimum and maximum enumeration fields.
*)

PROCEDURE FindMinMaxEnum (field: WORD) ;
BEGIN
   IF MaxEnum=NulSym
   THEN
      MaxEnum := field
   ELSE
      PushValue(field) ;
      PushValue(MaxEnum) ;
      IF Gre(GetTokenNo())
      THEN
         MaxEnum := field
      END
   END ;
   IF MinEnum=NulSym
   THEN
      MinEnum := field
   ELSE
      PushValue(field) ;
      PushValue(MinEnum) ;
      IF Less(GetTokenNo())
      THEN
         MinEnum := field
      END
   END
END FindMinMaxEnum ;


(*
   GetBaseTypeMinMax - returns the minimum and maximum values for a
                       given base type. This procedure should only
                       be called if the type is NOT a subrange.
*)

PROCEDURE GetBaseTypeMinMax (type: CARDINAL; VAR min, max: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   IF type=Integer
   THEN
      min := MinInteger ;
      max := MaxInteger
   ELSIF type=Cardinal
   THEN
      min := MinCardinal ;
      max := MaxCardinal
   ELSIF type=Char
   THEN
      min := MinChar ;
      max := MaxChar
   ELSIF (type=Bitset) AND Iso
   THEN
      GetBitsetMinMax(min, max)
   ELSIF (type=LongInt)
   THEN
      min := MinLongInt ;
      max := MaxLongInt
   ELSIF (type=LongCard)
   THEN
      min := MinLongCard ;
      max := MaxLongCard
   ELSIF (type=ShortInt)
   THEN
      min := MinShortInt ;
      max := MaxShortInt
   ELSIF (type=ShortCard)
   THEN
      min := MinShortCard ;
      max := MaxShortCard
   ELSIF (type=Real)
   THEN
      min := MinReal ;
      max := MaxReal
   ELSIF (type=ShortReal)
   THEN
      min := MinShortReal ;
      max := MaxShortReal
   ELSIF (type=LongReal)
   THEN
      min := MinLongReal ;
      max := MaxLongReal
   ELSIF IsEnumeration(type)
   THEN
      MinEnum := NulSym ;
      MaxEnum := NulSym ;
      ForeachFieldEnumerationDo(type, FindMinMaxEnum) ;
      min := MinEnum ;
      max := MaxEnum
   ELSE
      n := GetSymName(type) ;
      WriteFormat1('unable to find MIN or MAX for the base type %a', n)
   END
END GetBaseTypeMinMax ;


(*
   ImportFrom - imports symbol, name, from module and returns the
                symbol.
*)

PROCEDURE ImportFrom (module: CARDINAL; name: ARRAY OF CHAR) : CARDINAL ;
BEGIN
   PutImported(GetExported(m2rts, MakeKey(name))) ;
   RETURN( GetSym(MakeKey(name)) )
END ImportFrom ;


(*
   InitBaseProcedures - initialises the base procedures,
                        INC, DEC, INCL, EXCL, NEW and DISPOSE.
*)

PROCEDURE InitBaseProcedures ;
VAR
   rtexceptions: CARDINAL ;
BEGIN
   (*
      The pseudo procedures NEW and DISPOSE are in fact "macro"
      substituted for ALLOCATE and DEALLOCATE.
      However they both have symbols in the base module so that
      the procedure mechanism treats all procedure calls the same.
      "Macro" substitution occurs in M2Quads.
   *)

   New := MakeProcedure(MakeKey('NEW')) ;
   Dispose := MakeProcedure(MakeKey('DISPOSE')) ;
   Inc := MakeProcedure(MakeKey('INC')) ;
   Dec := MakeProcedure(MakeKey('DEC')) ;
   Incl := MakeProcedure(MakeKey('INCL')) ;
   Excl := MakeProcedure(MakeKey('EXCL')) ;

   IF NOT Pim2
   THEN
      MakeSize  (* SIZE is declared as a standard function in *)
                (* ISO Modula-2 and PIM-[34] Modula-2 but not *)
                (* PIM-2 Modula-2                             *)
   END ;

   (*
      The procedure HALT is a real procedure which
      is defined in M2RTS. However to remain compatible
      with other Modula-2 implementations HALT can be used
      without the need to import it from M2RTS. ie it is
      within the BaseType module scope.
   *)
   m2rts := MakeDefinitionSource(MakeKey('M2RTS')) ;
   PutImported(GetExported(m2rts, MakeKey('HALT'))) ;

   ExceptionAssign       := NulSym ;
   ExceptionInc          := NulSym ;
   ExceptionDec          := NulSym ;
   ExceptionStaticArray  := NulSym ;
   ExceptionDynamicArray := NulSym ;
   ExceptionForLoopBegin := NulSym ;
   ExceptionForLoopTo    := NulSym ;
   ExceptionForLoopEnd   := NulSym ;
   ExceptionPointerNil   := NulSym ;
   ExceptionNoReturn     := NulSym ;
   ExceptionCase         := NulSym ;
   ExceptionNonPosDiv    := NulSym ;
   ExceptionNonPosMod    := NulSym ;
   ExceptionZeroDiv      := NulSym ;
   ExceptionZeroRem      := NulSym ;
   ExceptionNo           := NulSym ;

   IF BoundsChecking
   THEN
      ExceptionAssign := ImportFrom(m2rts, 'AssignmentException') ;
      ExceptionInc := ImportFrom(m2rts, 'IncException') ;
      ExceptionDec := ImportFrom(m2rts, 'DecException') ;
      ExceptionStaticArray := ImportFrom(m2rts, 'StaticArraySubscriptException') ;
      ExceptionDynamicArray := ImportFrom(m2rts, 'DynamicArraySubscriptException') ;
      ExceptionForLoopBegin := ImportFrom(m2rts, 'ForLoopBeginException') ;
      ExceptionForLoopTo := ImportFrom(m2rts, 'ForLoopToException') ;
      ExceptionForLoopEnd := ImportFrom(m2rts, 'ForLoopEndException')
   END ;
   IF NilChecking
   THEN
      ExceptionPointerNil := ImportFrom(m2rts, 'PointerNilException')
   END ;
   IF ReturnChecking
   THEN
      ExceptionNoReturn := ImportFrom(m2rts, 'NoReturnException')
   END ;
   IF CaseElseChecking
   THEN
      ExceptionCase := ImportFrom(m2rts, 'CaseException')
   END ;
   IF DivModRemChecking
   THEN
      ExceptionNonPosDiv := ImportFrom(m2rts, 'WholeNonPosDivException') ;
      ExceptionNonPosMod := ImportFrom(m2rts, 'WholeNonPosModException') ;
      ExceptionZeroDiv := ImportFrom(m2rts, 'WholeZeroDivException') ;
      ExceptionZeroRem := ImportFrom(m2rts, 'WholeZeroRemException')
   END ;
   ExceptionNo := ImportFrom(m2rts, 'NoException') ;

   (* ensure that this module is included *)
   rtexceptions := MakeDefinitionSource(MakeKey('RTExceptions')) ;
END InitBaseProcedures ;


(*
   IsOrd - returns TRUE if, sym, is ORD or its typed counterparts
           ORDL, ORDS.
*)

PROCEDURE IsOrd (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN (sym=Ord) OR (sym=OrdS) OR (sym=OrdL)
END IsOrd ;


(*
   BuildOrdFunctions - creates ORD, ORDS, ORDL.
*)

PROCEDURE BuildOrdFunctions ;
BEGIN
   Ord := MakeProcedure(MakeKey('ORD')) ;
   PutFunction(Ord, Cardinal) ;
   OrdS := MakeProcedure(MakeKey('ORDS')) ;
   PutFunction(OrdS, ShortCard) ;
   OrdL := MakeProcedure(MakeKey('ORDL')) ;
   PutFunction(OrdL, LongCard)
END BuildOrdFunctions ;


(*
   IsTrunc - returns TRUE if, sym, is TRUNC or its typed counterparts
             TRUNCL, TRUNCS.
*)

PROCEDURE IsTrunc (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN (sym=Trunc) OR (sym=TruncS) OR (sym=TruncL)
END IsTrunc ;


(*
   BuildTruncFunctions - creates TRUNC, TRUNCS, TRUNCL.
*)

PROCEDURE BuildTruncFunctions ;
BEGIN
   IF Pim2 OR Pim3 OR Iso
   THEN
      Trunc := MakeProcedure(MakeKey('TRUNC')) ;
      PutFunction(Trunc, Cardinal) ;
      TruncS := MakeProcedure(MakeKey('TRUNCS')) ;
      PutFunction(TruncS, ShortCard) ;
      TruncL := MakeProcedure(MakeKey('TRUNCL')) ;
      PutFunction(TruncL, LongCard)
   ELSE
      Trunc := MakeProcedure(MakeKey('TRUNC')) ;
      PutFunction(Trunc, Integer) ;
      TruncS := MakeProcedure(MakeKey('TRUNCS')) ;
      PutFunction(TruncS, ShortInt) ;
      TruncL := MakeProcedure(MakeKey('TRUNCL')) ;
      PutFunction(TruncL, LongInt)
   END
END BuildTruncFunctions ;


(*
   IsFloat - returns TRUE if, sym, is FLOAT or its typed counterparts
             FLOATL, FLOATS.
*)

PROCEDURE IsFloat (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN (sym=Float) OR (sym=FloatS) OR (sym=FloatL)
END IsFloat ;


(*
   BuildFloatFunctions - creates TRUNC, TRUNCS, TRUNCL.
*)

PROCEDURE BuildFloatFunctions ;
BEGIN
   Float := MakeProcedure(MakeKey('FLOAT')) ;
   PutFunction(Float, Real) ;
   FloatS := MakeProcedure(MakeKey('FLOATS')) ;
   PutFunction(FloatS, ShortReal) ;
   FloatL := MakeProcedure(MakeKey('FLOATL')) ;
   PutFunction(FloatL, LongReal)
END BuildFloatFunctions ;


(*
   InitBaseFunctions - initialises the base function, HIGH.
*)

PROCEDURE InitBaseFunctions ;
BEGIN
   (* Now declare the dynamic array components, HIGH *)
   High := MakeProcedure(MakeKey('HIGH')) ;  (* Pseudo Base function HIGH *)
   PutFunction(High, Cardinal) ;

   (*
     _TemplateProcedure is a procedure which has a local variable _ActivationPointer
      whose offset is used for all nested procedures. (The activation pointer
      being in the same relative position for all procedures).
   *)
   TemplateProcedure := MakeProcedure(MakeKey('_TemplateProcedure')) ;
   StartScope(TemplateProcedure) ;
   ActivationPointer := MakeVar(MakeKey('_ActivationPointer')) ;
   PutVar(ActivationPointer, Address) ;
   EndScope ;

   (* and the base functions *)

   Convert := MakeProcedure(MakeKey('CONVERT')) ;  (* Internal function CONVERT    *)
   IF Iso
   THEN
      LengthS := MakeProcedure(MakeKey('LENGTH'))  (* Pseudo Base function LENGTH  *)
   ELSE
      LengthS := NulSym
   END ;
   Abs     := MakeProcedure(MakeKey('ABS')) ;      (* Pseudo Base function ABS     *)
   Cap     := MakeProcedure(MakeKey('CAP')) ;      (* Pseudo Base function CAP     *)
   Odd     := MakeProcedure(MakeKey('ODD')) ;      (* Pseudo Base function ODD     *)
   Val     := MakeProcedure(MakeKey('VAL')) ;      (* Pseudo Base function VAL     *)
   Chr     := MakeProcedure(MakeKey('CHR')) ;      (* Pseudo Base function CHR     *)
   Min     := MakeProcedure(MakeKey('MIN')) ;      (* Pseudo Base function MIN     *)
   Max     := MakeProcedure(MakeKey('MAX')) ;      (* Pseudo Base function MIN     *)
   BuildFloatFunctions ;
   BuildTruncFunctions ;
   BuildOrdFunctions
END InitBaseFunctions ;


(*
   IsISOPseudoBaseFunction - 
*)

PROCEDURE IsISOPseudoBaseFunction (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (Iso AND (Sym#NulSym) AND ((Sym=LengthS) OR (Sym=Size))) )
END IsISOPseudoBaseFunction ;


(*
   IsPIMPseudoBaseFunction - 
*)

PROCEDURE IsPIMPseudoBaseFunction (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (NOT Iso) AND (NOT Pim2) AND (Sym#NulSym) AND (Sym=Size) )
END IsPIMPseudoBaseFunction ;


(*
   IsPseudoBaseFunction - returns true if Sym is a Base pseudo function.
*)

PROCEDURE IsPseudoBaseFunction (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (Sym=High) OR (Sym=Val) OR (Sym=Convert) OR IsOrd(Sym) OR
          (Sym=Chr) OR IsFloat(Sym) OR IsTrunc(Sym) OR (Sym=Min) OR
          (Sym=Max) OR (Sym=Abs) OR (Sym=Odd) OR (Sym=Cap) OR
          IsISOPseudoBaseFunction(Sym) OR IsPIMPseudoBaseFunction(Sym)
         )
END IsPseudoBaseFunction ;


(*
   IsPseudoBaseProcedure - returns true if Sym is a Base pseudo procedure.
*)

PROCEDURE IsPseudoBaseProcedure (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (Sym=New) OR (Sym=Dispose) OR (Sym=Inc) OR (Sym=Dec) OR
          (Sym=Incl) OR (Sym=Excl)
         )
END IsPseudoBaseProcedure ;


(*
   IsBaseType - returns TRUE if Sym is a Base type.
*)

PROCEDURE IsBaseType (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (Sym=Cardinal) OR (Sym=Integer)  OR (Sym=Boolean) OR
          (Sym=Char)     OR (Sym=Proc)     OR
          (Sym=LongInt)  OR (Sym=LongCard) OR
          (Sym=ShortInt) OR (Sym=ShortCard) OR
          (Sym=Real)     OR (Sym=LongReal) OR (Sym=ShortReal) OR
          ((Sym=Bitset) AND Iso)
         )
END IsBaseType ;


(*
   IsOrdinalType - returns TRUE if, sym, is an ordinal type.
                   An ordinal type is defined as:
                   a base type which contains whole numbers or
                   a subrange type or an enumeration type.
*)

PROCEDURE IsOrdinalType (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (Sym=Cardinal)  OR (Sym=Integer)   OR
          (Sym=Char)      OR (Sym=Boolean)   OR
          (Sym=LongInt)   OR (Sym=LongCard)  OR
          (Sym=ShortInt)  OR (Sym=ShortCard) OR
          (Sym=ZType)     OR
          IsSubrange(Sym) OR IsEnumeration(Sym) OR
          IsIntegerN(Sym) OR IsCardinalN(Sym)
         )
END IsOrdinalType ;


(*
   EmitTypeIncompatibleWarning - emit a type incompatibility warning.
*)

PROCEDURE EmitTypeIncompatibleWarning (e: Error; kind: Compatability) ;
BEGIN
   CASE kind OF

   expression:  ErrorFormat0(e, 'warning type incompatibility found in an expression, hint one of the expressions should be converted') |
   assignment:  ErrorFormat0(e, 'warning type incompatibility found during assignment, hint the expression should be converted') |
   parameter :  ErrorFormat0(e, 'warning type incompatibility found while passing a parameter, hint the parameter should be converted')

   ELSE
   END
END EmitTypeIncompatibleWarning ;


(*
   EmitTypeIncompatibleError - emit a type incompatibility error.
*)

PROCEDURE EmitTypeIncompatibleError (e: Error; kind: Compatability) ;
BEGIN
   CASE kind OF

   expression:  ErrorFormat0(e, 'type incompatibility found in an expression, hint one of the expressions should be converted') |
   assignment:  ErrorFormat0(e, 'type incompatibility found during assignment, hint the expression should be converted') |
   parameter :  ErrorFormat0(e, 'type incompatibility found while passing a parameter, hint the parameter should be converted')

   ELSE
   END
END EmitTypeIncompatibleError ;


(*
   CheckCompatible - returns if t1 and t2 are kind compatible
*)

PROCEDURE CheckCompatible (t1, t2: CARDINAL; kind: Compatability) ;
VAR
   n        : Name ;
   e        : Error ;
   s, s1, s2: String ;
   r        : Compatible ;
BEGIN
   r := IsCompatible(t1, t2, kind) ;
   IF (r#first) AND (r#second)
   THEN
      IF (r=warnfirst) OR (r=warnsecond)
      THEN
         e := NewWarning(GetTokenNo())
      ELSE
         e := NewError(GetTokenNo())
      END ;
      IF IsUnknown(t1) AND IsUnknown(t2)
      THEN
         ErrorFormat0(e, 'two different unknown types must be resolved (declared or imported)')
      ELSIF IsUnknown(t1) OR IsUnknown(t2)
      THEN
         ErrorFormat0(e, 'a type must be declared or imported')
      ELSE
         IF (r=warnfirst) OR (r=warnsecond)
         THEN
            EmitTypeIncompatibleWarning(e, kind)
         ELSE
            EmitTypeIncompatibleError(e, kind)
         END
      END ;

      e := ChainError(GetTokenNo(), e) ;
      s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(t1)))) ;
      s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(t2)))) ;
      s := Sprintf2(Mark(InitString('the two types are: %s and %s\n')),
                    s1, s2) ;
      ErrorString(e, s) ;
      IF IsUnknown(t1)
      THEN
         e := ChainError(GetDeclared(t1), e) ;
         n := GetSymName(t1) ;
         ErrorFormat1(e, 'hint, %a, is unknown and perhaps should be declared or imported',
                      n)
      END ;

      IF IsUnknown(t2)
      THEN
         e := ChainError(GetDeclared(t2), e) ;
         n := GetSymName(t2) ;
         ErrorFormat1(e, 'hint, %a, is unknown and perhaps should be declared or imported',
                      n)
      END
   END
END CheckCompatible ;


(*
   CheckExpressionCompatible - returns if t1 and t2 are compatible types for
                               +, -, *, DIV, >, <, =, etc.
                               If t1 and t2 are not compatible then an error
                               message is displayed.
*)

PROCEDURE CheckExpressionCompatible (t1, t2: CARDINAL) ;
BEGIN
   CheckCompatible(t1, t2, expression)
END CheckExpressionCompatible ;


(*
   CheckParameterCompatible - checks to see if types, t1, and, t2, are
                              compatible for parameter passing.
*)

PROCEDURE CheckParameterCompatible (t1, t2: CARDINAL) ;
BEGIN
   CheckCompatible(t1, t2, parameter)
END CheckParameterCompatible ;


(*
   CheckAssignmentCompatible - returns if t1 and t2 are compatible types for
                               :=, =, #.
                               If t1 and t2 are not compatible then an error
                               message is displayed.
*)

PROCEDURE CheckAssignmentCompatible (t1, t2: CARDINAL) ;
BEGIN
   IF t1#t2
   THEN
      CheckCompatible(t1, t2, assignment)
   END
END CheckAssignmentCompatible ;


(*
   FindMetaType - returns the MetaType associated with, sym.
*)

PROCEDURE FindMetaType (sym: CARDINAL) : MetaType ;
BEGIN
   IF sym=NulSym
   THEN
      RETURN( const )
   ELSIF sym=Word
   THEN
      RETURN( word )
   ELSIF sym=Byte
   THEN
      RETURN( byte )
   ELSIF sym=Loc
   THEN
      RETURN( loc )
   ELSIF sym=Address
   THEN
      RETURN( address )
   ELSIF sym=Char
   THEN
      RETURN( chr )
   ELSIF sym=Integer
   THEN
      RETURN( normint )
   ELSIF sym=ShortInt
   THEN
      RETURN( shortint )
   ELSIF sym=LongInt
   THEN
      RETURN( longint )
   ELSIF sym=Cardinal
   THEN
      RETURN( normcard )
   ELSIF sym=ShortCard
   THEN
      RETURN( shortcard )
   ELSIF sym=LongCard
   THEN
      RETURN( longcard )
   ELSIF IsPointer(sym)
   THEN
      RETURN( pointer )
   ELSIF IsEnumeration(sym)
   THEN
      RETURN( enum )
   ELSIF sym=ZType
   THEN
      RETURN( ztype )
   ELSIF sym=RType
   THEN
      RETURN( rtype )
   ELSIF sym=Real
   THEN
      RETURN( real )
   ELSIF sym=ShortReal
   THEN
      RETURN( shortreal )
   ELSIF sym=LongReal
   THEN
      RETURN( longreal )
   ELSIF IsSet(sym)
   THEN
      RETURN( set )
   ELSIF IsHiddenType(sym)
   THEN
      RETURN( opaque )
   ELSIF sym=IntegerN(8)
   THEN
      RETURN( int8 )
   ELSIF sym=IntegerN(16)
   THEN
      RETURN( int16 )
   ELSIF sym=IntegerN(32)
   THEN
      RETURN( int32 )
   ELSIF sym=IntegerN(64)
   THEN
      RETURN( int64 )
   ELSIF sym=CardinalN(8)
   THEN
      RETURN( card8 )
   ELSIF sym=CardinalN(16)
   THEN
      RETURN( card16 )
   ELSIF sym=CardinalN(32)
   THEN
      RETURN( card32 )
   ELSIF sym=CardinalN(64)
   THEN
      RETURN( card64 )
   ELSIF sym=WordN(16)
   THEN
      RETURN( word16 )
   ELSIF sym=WordN(32)
   THEN
      RETURN( word32 )
   ELSIF sym=WordN(64)
   THEN
      RETURN( word64 )
   ELSIF sym=SetN(8)
   THEN
      RETURN( set8 )
   ELSIF sym=SetN(16)
   THEN
      RETURN( set16 )
   ELSIF sym=SetN(32)
   THEN
      RETURN( set32 )
   ELSIF sym=RealN(32)
   THEN
      RETURN( real32 )
   ELSIF sym=RealN(64)
   THEN
      RETURN( real64 )
   ELSIF sym=RealN(96)
   THEN
      RETURN( real96 )
   ELSIF sym=RealN(128)
   THEN
      RETURN( real128 )
   ELSIF IsType(sym)
   THEN
      RETURN( FindMetaType(GetType(sym)) )
   ELSE
      RETURN( unknown )
   END
END FindMetaType ;


(*
   IsBaseCompatible - returns TRUE if a simple base type comparison is legal.
*)

PROCEDURE IsBaseCompatible (t1, t2: CARDINAL;
                            kind: Compatability) : Compatible ;
VAR
   mt1, mt2: MetaType ;
BEGIN
   IF ((kind=assignment) AND (t1=t2)) OR
      ((kind=parameter) AND (t1=t2))
   THEN
      RETURN( first )
   ELSE
      mt1 := FindMetaType(t1) ;
      mt2 := FindMetaType(t2) ;
      IF (mt1=unknown) OR (mt2=unknown)
      THEN
         RETURN( no )
      END ;
         
      CASE kind OF

      expression: RETURN( Expr [mt1, mt2] ) |
      assignment: RETURN( Ass  [mt1, mt2] ) |
      parameter : RETURN( Param[mt1, mt2] )

      ELSE
         InternalError('unexpected Compatibility', __FILE__, __LINE__)
      END
   END
END IsBaseCompatible ;


(*
   IsRealType - returns TRUE if, t, is a real type.
*)

PROCEDURE IsRealType (t: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (t=Real) OR (t=LongReal) OR (t=ShortReal) OR (t=RType) )
END IsRealType ;


(*
   IsCompatible - returns true if the types, t1, and, t2, are compatible.
*)

PROCEDURE IsCompatible (t1, t2: CARDINAL; kind: Compatability) : Compatible ;
BEGIN
   t1 := SkipType(t1) ;
   t2 := SkipType(t2) ;
   IF (t1=NulSym) OR (t2=NulSym)
   THEN
      RETURN( first )
   ELSIF IsSubrange(t1)
   THEN
      RETURN( IsCompatible(GetType(t1), t2, kind) )
   ELSIF IsSubrange(t2)
   THEN
      RETURN( IsCompatible(t1, GetType(t2), kind) )
   ELSIF IsSet(t1) OR IsSet(t2)
   THEN
      RETURN( first ) (* cannot test set compatibility at this point --fixme-- *)
   ELSIF (IsHiddenType(t1) OR IsProcType(t1)) AND (kind=assignment)
   THEN
      IF t1=t2
      THEN
         RETURN( first )
      ELSE
         RETURN( no )
      END
   ELSE
(*
   see M2Quads for the fixme comment at assignment.

   PIM2 says that CARDINAL and INTEGER are compatible with subranges of CARDINAL and INTEGER,
        however we do not know the type to our subranges yet (GetType(SubrangeType)=NulSym).
        An oversight which needs to be fixed...
*)

      RETURN( IsBaseCompatible(t1, t2, kind) )
   END
END IsCompatible ;


(*
   AssignmentRequiresWarning - returns TRUE if t1 and t2 can be used during
                               an assignment, but should generate a warning.
                               For example in PIM we can assign ADDRESS
                               and WORD providing they are both the
                               same size.
*)

PROCEDURE AssignmentRequiresWarning (t1, t2: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (IsCompatible(t1, t2, assignment)=warnfirst) OR
          (IsCompatible(t1, t2, assignment)=warnsecond)
         )
END AssignmentRequiresWarning ;


(*
   IsAssignmentCompatible - returns TRUE if t1 and t2 are assignment
                            compatible.
*)

PROCEDURE IsAssignmentCompatible (t1, t2: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (t1=t2) OR
          (IsCompatible(t1, t2, assignment)=first) OR
          (IsCompatible(t1, t2, assignment)=second)
         )
END IsAssignmentCompatible ;


(*
   IsExpressionCompatible - returns TRUE if t1 and t2 are expression
                            compatible.
*)

PROCEDURE IsExpressionCompatible (t1, t2: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (IsCompatible(t1, t2, expression)=first) OR
          (IsCompatible(t1, t2, expression)=second)
         )
END IsExpressionCompatible ;


(*
   IsParameterCompatible - returns TRUE if t1 and t2 are expression
                            compatible.
*)

PROCEDURE IsParameterCompatible (t1, t2: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (IsCompatible(t1, t2, parameter)=first) OR
          (IsCompatible(t1, t2, parameter)=second)
         )
END IsParameterCompatible ;


(*
   MixMetaTypes - 
*)

PROCEDURE MixMetaTypes (t1, t2: CARDINAL; NearTok: CARDINAL) : CARDINAL ;
VAR
   mt1, mt2: MetaType ;
   n1, n2  : Name ;
BEGIN
   mt1 := FindMetaType(t1) ;
   mt2 := FindMetaType(t2) ;
   CASE Expr[mt1, mt2] OF

   no        :  n1 := GetSymName(t1) ;
                n2 := GetSymName(t2) ;
                ErrorFormat2(NewError(NearTok),
                             'type incompatibility between (%a) and (%a)', n1, n2) ;
                FlushErrors  (* unrecoverable at present *) |
   warnfirst,
   first     :  RETURN( t1 ) |
   warnsecond,
   second    :  RETURN( t2 )

   ELSE
      InternalError('not expecting this metatype value', __FILE__, __LINE__)
   END
END MixMetaTypes ;


(*
   MixTypes - given types, t1 and t2, returns a type symbol that
              provides expression type compatibility.
              NearTok is used to identify the source position if a type
              incompatability occurs.
*)

PROCEDURE MixTypes (t1, t2: CARDINAL; NearTok: CARDINAL) : CARDINAL ;
BEGIN
   IF t1=t2
   THEN
      RETURN( t1 )
   ELSIF (t1=Address) AND (t2=Cardinal)
   THEN
      RETURN( Address )
   ELSIF (t1=Cardinal) AND (t2=Address)
   THEN
      RETURN( Address )
   ELSIF (t1=Address) AND (t2=Integer)
   THEN
      RETURN( Address )
   ELSIF (t1=Integer) AND (t2=Address)
   THEN
      RETURN( Address )
   ELSIF t1=NulSym
   THEN
      RETURN( t2 )
   ELSIF t2=NulSym
   THEN
      RETURN( t1 )
   ELSIF (t1=Bitset) AND IsSet(t2)
   THEN
      RETURN( t1 )
   ELSIF IsSet(t1) AND (t2=Bitset)
   THEN
      RETURN( t2 )
   ELSIF IsEnumeration(t1)
   THEN
      RETURN( MixTypes(Integer, t2, NearTok) )
   ELSIF IsEnumeration(t2)
   THEN
      RETURN( MixTypes(t1, Integer, NearTok) )
   ELSIF IsSubrange(t1)
   THEN
      RETURN( MixTypes(GetType(t1), t2, NearTok) )
   ELSIF IsSubrange(t2)
   THEN
      RETURN( MixTypes(t1, GetType(t2), NearTok) )
   ELSIF (t1=GetLowestType(t1)) AND (t2=GetLowestType(t2))
   THEN
      RETURN( MixMetaTypes(t1, t2, NearTok) )
   ELSE
      t1 := GetLowestType(t1) ;
      t2 := GetLowestType(t2) ;
      RETURN( MixTypes(t1, t2, NearTok) )
   END
END MixTypes ;


(*
   NegateType - if the type, t, is unsigned then returns the
                signed equivalent. NearTok is used to identify the
                source position if a type incompatability occurs.
*)

PROCEDURE NegateType (t: CARDINAL; NearTok: CARDINAL) : CARDINAL ;
VAR
   l: CARDINAL ;
BEGIN
   IF t#NulSym
   THEN
      l := GetLowestType(t) ;
      IF l=LongCard
      THEN
         RETURN( LongInt )
      ELSIF (l=Cardinal)
      THEN
         RETURN( Integer )
      END
   END ;
   RETURN( t )
END NegateType ;


(*
   IsMathType - returns TRUE if the type is a mathematical type.
                A mathematical type has a range larger than INTEGER.
                (Typically SHORTREAL/REAL/LONGREAL/LONGINT/LONGCARD)
*)

PROCEDURE IsMathType (type: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (type=LongCard) OR (type=LongInt) OR (type=Real) OR
          (type=LongReal) OR (type=ShortReal) OR
          (type=RType) OR (type=ZType)
         )
END IsMathType ;


(*
   PushSizeOf - pushes the size of a meta type.
*)

PROCEDURE PushSizeOf (t: MetaType) ;
BEGIN
   CASE t OF

   const    :   InternalError('do not know the size of a constant', __FILE__, __LINE__) |
   word     :   IF Iso
                THEN
                   PushIntegerTree(GetSizeOf(GetISOWordType()))
                ELSE
                   PushIntegerTree(GetSizeOf(GetWordType()))
                END |
   byte     :   IF Iso
                THEN
                   PushIntegerTree(GetSizeOf(GetISOByteType()))
                ELSE
                   PushIntegerTree(GetSizeOf(GetByteType()))
                END |
   address  :   PushIntegerTree(GetSizeOf(GetPointerType())) |
   chr      :   PushIntegerTree(GetSizeOf(GetM2CharType())) |
   normint  :   PushIntegerTree(GetSizeOf(GetM2IntegerType())) |
   shortint :   PushIntegerTree(GetSizeOf(GetM2ShortIntType())) |
   longint  :   PushIntegerTree(GetSizeOf(GetM2LongIntType())) |
   normcard :   PushIntegerTree(GetSizeOf(GetM2CardinalType())) |
   shortcard:   PushIntegerTree(GetSizeOf(GetM2ShortCardType())) |
   longcard :   PushIntegerTree(GetSizeOf(GetM2LongCardType())) |
   pointer  :   PushIntegerTree(GetSizeOf(GetPointerType())) |
   enum     :   PushIntegerTree(GetSizeOf(GetIntegerType())) |
   real     :   PushIntegerTree(GetSizeOf(GetM2RealType())) |
   shortreal:   PushIntegerTree(GetSizeOf(GetM2ShortRealType())) |
   longreal :   PushIntegerTree(GetSizeOf(GetM2LongRealType())) |
   set      :   InternalError('do not know the size of a set', __FILE__, __LINE__) |
   opaque   :   InternalError('do not know the size of an opaque', __FILE__, __LINE__) |
   loc      :   PushIntegerTree(GetSizeOf(GetISOLocType())) |
   rtype    :   PushIntegerTree(GetSizeOf(GetM2RType())) |
   ztype    :   PushIntegerTree(GetSizeOf(GetM2ZType())) |
   int8,
   card8,
   set8     :   PushIntegerTree(BuildIntegerConstant(1)) |
   word16,
   set16,
   card16,
   int16    :   PushIntegerTree(BuildIntegerConstant(2)) |
   real32,
   word32,
   set32,
   card32,
   int32    :   PushIntegerTree(BuildIntegerConstant(4)) |
   real64,
   word64,
   card64,
   int64    :   PushIntegerTree(BuildIntegerConstant(8)) |
   real96   :   PushIntegerTree(BuildIntegerConstant(12)) |
   real128  :   PushIntegerTree(BuildIntegerConstant(16)) |
   unknown  :   InternalError('should not get here', __FILE__, __LINE__) |


   ELSE
      InternalError('should not get here', __FILE__, __LINE__)
   END
END PushSizeOf ;


(*
   IsSizeSame - 
*)

PROCEDURE IsSizeSame (t1, t2: MetaType) : BOOLEAN ;
BEGIN
   PushSizeOf(t1) ;
   PushSizeOf(t2) ;
   RETURN( Equ(0) )
END IsSizeSame ;


(*
   InitArray - 
*)

PROCEDURE InitArray (VAR c: CompatibilityArray;
                     y: MetaType; a: ARRAY OF CHAR) ;
VAR
   x   : MetaType ;
   h, i: CARDINAL ;
BEGIN
   h := StrLen(a) ;
   i := 0 ;
   x := MIN(MetaType) ;
   WHILE i<h DO
      IF (c[x, y]#uninitialized) AND (x#unknown) AND (y#unknown)
      THEN
         InternalError('expecting array element to be uninitialized',
                       __FILE__, __LINE__)
      END ;
      CASE a[i] OF

      ' ':  |
      '.':  CASE c[y, x] OF

            uninitialized:  InternalError('cannot reflect value as it is unknown',
                                          __FILE__, __LINE__) |
            first        :  c[x, y] := second |
            second       :  c[x, y] := first |
            warnfirst    :  c[x, y] := warnsecond |
            warnsecond   :  c[x, y] := warnfirst

            ELSE
               c[x, y] := c[y, x]
            END ;
            INC(x) |
      'F':  c[x, y] := no ;
            INC(x) |
      'T',
      '1':  c[x, y] := first ;
            INC(x) |
      '2':  c[x, y] := second ;
            INC(x) |
      'W':  IF Pim
            THEN
               IF IsSizeSame(x, y)
               THEN
                  c[x, y] := warnsecond
               ELSE
                  c[x, y] := no
               END
            ELSE
               c[x, y] := no
            END ;
            INC(x) |
      'w':  IF Pim
            THEN
               IF IsSizeSame(x, y)
               THEN
                  c[x, y] := warnfirst
               ELSE
                  c[x, y] := no
               END
            ELSE
               c[x, y] := no
            END ;
            INC(x) |
      'P':  IF Pim
            THEN
               c[x, y] := second
            ELSE
               c[x, y] := no
            END ;
            INC(x) |
      'p':  IF Pim
            THEN
               c[x, y] := first
            ELSE
               c[x, y] := no
            END ;
            INC(x) |
      's':  IF IsSizeSame(x, y)
            THEN
               c[x, y] := first
            ELSE
               c[x, y] := no
            END ;
            INC(x) |
      'S':  IF IsSizeSame(x, y)
            THEN
               c[x, y] := second
            ELSE
               c[x, y] := no
            END ;
            INC(x) |


      ELSE
         InternalError('unexpected specifier', __FILE__, __LINE__)
      END ;
      INC(i)
   END
END InitArray ;


(*
   A - initialize the assignment array
*)

PROCEDURE A (y: MetaType; a: ARRAY OF CHAR) ;
BEGIN
   InitArray(Ass, y, a)
END A ;


(*
   E - initialize the expression array
*)

PROCEDURE E (y: MetaType; a: ARRAY OF CHAR) ;
BEGIN
   InitArray(Expr, y, a)
END E ;


(*
   P - initialize the parameter array
*)

PROCEDURE P (y: MetaType; a: ARRAY OF CHAR) ;
BEGIN
   InitArray(Param, y, a)
END P ;


(*
   InitCompatibilityMatrices - initializes the tables above.
*)

PROCEDURE InitCompatibilityMatrices ;
VAR
   i, j: MetaType ;
BEGIN
   (* initialize to a known state *)
   FOR i := MIN(MetaType) TO MAX(MetaType) DO
      FOR j := MIN(MetaType) TO MAX(MetaType) DO
         Ass[i, j]  := uninitialized ;
         Expr[i, j]  := uninitialized ;
         Param[i, j]  := uninitialized
      END
   END ;

   (* all unknowns are false *)
   FOR i := MIN(MetaType) TO MAX(MetaType) DO
      Ass[i, unknown]  := no ;
      Expr[unknown, i] := no ;
      Param[unknown, i] := no
   END ;

   (*
                                     1 p w

                 N W B A C I S L C S L P E R S L S O L R Z I I I I C C C C W W W R R R R S S S
                 u o y d h n h o a h o t n e h o e p o t t n n n n a a a a o o o e e e e e e e
                 l r t d a t o n r o n r u a o n t a c y y t t t t r r r r r r r a a a a t t t
                 S d e r r e r g d r g   m l r g   q   p p 8 1 3 6 d d d d d d d l l l l 8 1 3
                 y     e   g t i i t c       t r   u   e e   6 2 4 8 1 3 6 1 3 6 3 6 9 1   6 2
                 m     s   e i n n c a       r e   e                 6 2 4 6 2 4 2 4 6 2
                       s   r n t a a r       e a                                       8
                             t   l r d       a l                                                  
                                   d         l                                                      
      ----------------------------------------------------------------------------------------
   2
   P
   W
   *)
   A(const    , 'T T T T T T T T T T T T T T T T T T T F T T T T T T T T T T T T F F F F F F F') ;
   A(word     , '. T F W F 2 W W 2 W W W 2 F W W T T F W T F F F F F F F F F F F F F F F F F F') ;
   A(byte     , '. . T F 2 F F F F F F F F F F F F F F F T S F F F S F F F F F F F F F F S F F') ;
   A(address  , '. . . T F F F F F F F 2 F F F F F 2 2 F T F F F F F F F F F F F F F F F F F F') ;
   A(chr      , '. . . . T F F F F F F F F F F F F F 2 F F F F F F F F F F F F F F F F F F F F') ;
   A(normint  , '. . . . . T T T T T T F F F F F F F F F T T T T T T T T T F F F F F F F F F F') ;
   A(shortint , '. . . . . . T T T T T F F F F F F F F F T T T T T T T T T F F F F F F F F F F') ;
   A(longint  , '. . . . . . . T T T T F F F F F F F F F T T T T T T T T T F F F F F F F F F F') ;
   A(normcard , '. . . . . . . . T T T F F F F F F F F F T T T T T T T T T F F F F F F F F F F') ;
   A(shortcard, '. . . . . . . . . T T F F F F F F F F F T T T T T T T T T F F F F F F F F F F') ;
   A(longcard , '. . . . . . . . . . T F F F F F F F F F T T T T T T T T T F F F F F F F F F F') ;
   A(pointer  , '. . . . . . . . . . . T F F F F F F F F F F F F F F F F F F F F F F F F F F F') ;
   A(enum     , '. . . . . . . . . . . . T F F F F F F F F F F F F F F F F F F F T T F F F F F') ;
   A(real     , '. . . . . . . . . . . . . T T T F F F 2 F F F F F F F F F F F F T T T T F F F') ;
   A(shortreal, '. . . . . . . . . . . . . . T T F F F 2 F F F F F F F F F F F F T T T T F F F') ;
   A(longreal , '. . . . . . . . . . . . . . . T F F F 2 F F F F F F F F F F F F T T T T F F F') ;
   A(set      , '. . . . . . . . . . . . . . . . T F F F F F F F F F F F F F F F F F F F F F F') ;
   A(opaque   , '. . . . . . . . . . . . . . . . . T F F F F F F F F F F F F F F F F F F F F F') ;
   A(loc      , '. . . . . . . . . . . . . . . . . . T F F T F F F T F F F F F F F F F F S F F') ;
   A(rtype    , '. . . . . . . . . . . . . . . . . . . T F F F F F F F F F F F F 1 1 1 1 F F F') ;
   A(ztype    , '. . . . . . . . . . . . . . . . . . . . T T T T T T T T T T T T F F F F F F F') ;
   A(int8     , '. . . . . . . . . . . . . . . . . . . . . T T T T T T T T F F F F F F F F F F') ;
   A(int16    , '. . . . . . . . . . . . . . . . . . . . . . T T T T T T T T F F F F F F F F F') ;
   A(int32    , '. . . . . . . . . . . . . . . . . . . . . . . T T T T T T F T T F F F F F F F') ;
   A(int64    , '. . . . . . . . . . . . . . . . . . . . . . . . T T T T T F F F F F F F F F F') ;
   A(card8    , '. . . . . . . . . . . . . . . . . . . . . . . . . T T T T T F F F F F F F F F') ;
   A(card16   , '. . . . . . . . . . . . . . . . . . . . . . . . . . T T T F F F F F F F F F F') ;
   A(card32   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . T T F T F F F F F F F F') ;
   A(card64   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F T F F F F F F F') ;
   A(word16   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F F F F F') ;
   A(word32   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F T F F F F F F') ;
   A(word64   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F T F F F F F') ;
   A(real32   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F F') ;
   A(real64   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F') ;
   A(real96   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F') ;
   A(real128  , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F') ;
   A(set8     , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F') ;
   A(set16    , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F') ;
   A(set32    , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T') ;

   (* Expression compatibility *)


   (*
                                        1 p w

                 N W B A C I S L C S L P E R S L S O L R Z I I I I C C C C W W W R R R R S S S
                 u o y d h n h o a h o t n e h o e p o t t n n n n a a a a o o o e e e e e e e
                 l r t d a t o n r o n r u a o n t a c y y t t t t r r r r r r r a a a a t t t
                 S d e r r e r g d r g   m l r g   q   p p 8 1 3 6 d d d d d d d l l l l 8 1 3
                 y     e   g t i i t c       t r   u   e e   6 2 4 8 1 3 6 1 3 6 3 6 9 1   6 2
                 m     s   e i n n c a       r e   e                 6 2 4 6 2 4 2 4 6 2
                       s   r n t a a r       e a                                       8
                             t   l r d       a l                                             
                                   d         l                                                      
      ----------------------------------------------------------------------------------------
   2
   P
   W
   *)

   E(const    , 'T T T T T T T T T T T T T T T T T T F F T T T T T T T T T T T T T T T T F F F') ;
   E(word     , '. T F F F F F F F F F F F F F F F F F W F F F F F F F F F F F F F F F F F F F') ;
   E(byte     , '. . T F F F F F F F F F F F F F F F F F F F F F F F F F F F F F F F F F F F F') ;
   E(address  , '. . . T F P F F P F F T F F F F F F F F F F F F F F F F F F F F F F F F F F F') ;
   E(chr      , '. . . . T F F F F F F F F F F F F F F F F F F F F F F F F F F F F F F F F F F') ;
   E(normint  , '. . . . . T F F F F F F F F F F F F F F 2 F F F F F F F F F F F F F F F F F F') ;
   E(shortint , '. . . . . . T F F F F F F F F F F F F F 2 F F F F F F F F F F F F F F F F F F') ;
   E(longint  , '. . . . . . . T F F F F F F F F F F F F 2 F F F F F F F F F F F F F F F F F F') ;
   E(normcard , '. . . . . . . . T F F F F F F F F F F F 2 F F F F F F F F F F F F F F F F F F') ;
   E(shortcard, '. . . . . . . . . T F F F F F F F F F F 2 F F F F F F F F F F F F F F F F F F') ;
   E(longcard , '. . . . . . . . . . T F F F F F F F F F 2 F F F F F F F F F F F F F F F F F F') ;
   E(pointer  , '. . . . . . . . . . . T F F F F F F F F F F F F F F F F F F F F F F F F F F F') ;
   E(enum     , '. . . . . . . . . . . . T F F F F F F F F F F F F F F F F F F F F F F F F F F') ;
   E(real     , '. . . . . . . . . . . . . T F F F F F 2 F F F F F F F F F F F F F F F F F F F') ;
   E(shortreal, '. . . . . . . . . . . . . . T F F F F 2 F F F F F F F F F F F F F F F F F F F') ;
   E(longreal , '. . . . . . . . . . . . . . . T F F F 2 F F F F F F F F F F F F F F F F F F F') ;
   E(set      , '. . . . . . . . . . . . . . . . T F F F F F F F F F F F F F F F F F F F F F F') ;
   E(opaque   , '. . . . . . . . . . . . . . . . . T F F F F F F F F F F F F F F F F F F F F F') ;
   E(loc      , '. . . . . . . . . . . . . . . . . . F F F F F F F F F F F F F F F F F F F F F') ;
   E(rtype    , '. . . . . . . . . . . . . . . . . . . T F F F F F F F F F F F F 1 1 1 1 F F F') ;
   E(ztype    , '. . . . . . . . . . . . . . . . . . . . T 1 1 1 1 1 1 1 1 1 1 1 F F F F F F F') ;
   E(int8     , '. . . . . . . . . . . . . . . . . . . . . T F F F F F F F F F F F F F F F F F') ;
   E(int16    , '. . . . . . . . . . . . . . . . . . . . . . T F F F F F F F F F F F F F F F F') ;
   E(int32    , '. . . . . . . . . . . . . . . . . . . . . . . T F F F F F F F F F F F F F F F') ;
   E(int64    , '. . . . . . . . . . . . . . . . . . . . . . . . T F F F F F F F F F F F F F F') ;
   E(card8    , '. . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F F F F F F F F F') ;
   E(card16   , '. . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F F F F F F F F') ;
   E(card32   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F F F F F F F') ;
   E(card64   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F F F F F F') ;
   E(word16   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F F F F F') ;
   E(word32   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F F F F') ;
   E(word64   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F F F') ;
   E(real32   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F F') ;
   E(real64   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F') ;
   E(real96   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F') ;
   E(real128  , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F') ;
   E(set8     , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F') ;
   E(set16    , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F') ;
   E(set32    , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T') ;

   (*
                                     1 p w

                 N W B A C I S L C S L P E R S L S O L R Z I I I I C C C C W W W R R R R S S S
                 u o y d h n h o a h o t n e h o e p o t t n n n n a a a a o o o e e e e e e e
                 l r t d a t o n r o n r u a o n t a c y y t t t t r r r r r r r a a a a t t t
                 S d e r r e r g d r g   m l r g   q   p p 8 1 3 6 d d d d d d d l l l l 8 1 3
                 y     e   g t i i t c       t r   u   e e   6 2 4 8 1 3 6 1 3 6 3 6 9 1   6 2
                 m     s   e i n n c a       r e   e                 6 2 4 6 2 4 2 4 6 2
                       s   r n t a a r       e a                                       8
                             t   l r d       a l
                                   d         l                                           
      ----------------------------------------------------------------------------------------
   2
   P
   W
   *)
   P(const    , 'T T T T T T T T T T T T T T T T T T T F T T T T T T T T T T T T F F F F F F F') ;
   P(word     , '. T F W F 2 W W 2 W W W 2 W W W T T F W F F F F F F F F F F F F F F F F F F F') ;
   P(byte     , '. . T F 2 F F F F F F F F F F F F F F F F S F F F S F F F F F F F F F F S F F') ;
   P(address  , '. . . T F F F F F F F T F F F F F T T F F F F F F F F F F F F F F F F F F F F') ;
   P(chr      , '. . . . T F F F F F F F F F F F F F T F F F F F F F F F F F F F F F F F F F F') ;
   P(normint  , '. . . . . T F F T F F F F F F F F F F F F T T T T T T T T F F F F F F F F F F') ;
   P(shortint , '. . . . . . T F F T F F F F F F F F F F 2 T T T T T T T T F F F F F F F F F F') ;
   P(longint  , '. . . . . . . T F F T F F F F F F F F F 2 T T T T T T T T F F F F F F F F F F') ;
   P(normcard , '. . . . . . . . T F F F F F F F F F F F 2 T T T T T T T T F F F F F F F F F F') ;
   P(shortcard, '. . . . . . . . . T F F F F F F F F F F 2 T T T T T T T T F F F F F F F F F F') ;
   P(longcard , '. . . . . . . . . . T F F F F F F F F F 2 T T T T T T T T F F F F F F F F F F') ;
   P(pointer  , '. . . . . . . . . . . T F F F F F F F F F F F F F F F F F F F F F F F F F F F') ;
   P(enum     , '. . . . . . . . . . . . T F F F F F F F F F F F F F F F F F F F T T F F F F F') ;
   P(real     , '. . . . . . . . . . . . . T F F F F F 2 F F F F F F F F F F F F T T T T F F F') ;
   P(shortreal, '. . . . . . . . . . . . . . T F F F F 2 F F F F F F F F F F F F T T T T F F F') ;
   P(longreal , '. . . . . . . . . . . . . . . T F F F 2 F F F F F F F F F F F F T T T T F F F') ;
   P(set      , '. . . . . . . . . . . . . . . . T F F F F F F F F F F F F F F F F F F F F F F') ;
   P(opaque   , '. . . . . . . . . . . . . . . . . T F F F F F F F F F F F F F F F F F F F F F') ;
   P(loc      , '. . . . . . . . . . . . . . . . . . T F F T F F F T F F F F F F F F F F F F F') ;
   P(rtype    , '. . . . . . . . . . . . . . . . . . . T F F F F F F F F F F F F 1 1 1 1 F F F') ;
   P(ztype    , '. . . . . . . . . . . . . . . . . . . . T 1 1 1 1 1 1 1 1 1 1 1 F F F F F F F') ;
   P(int8     , '. . . . . . . . . . . . . . . . . . . . . T T T T T T T T F F F F F F F F F F') ;
   P(int16    , '. . . . . . . . . . . . . . . . . . . . . . T T T T T T T T F F F F F F F F F') ;
   P(int32    , '. . . . . . . . . . . . . . . . . . . . . . . T T T T T T F T T F F F F F F F') ;
   P(int64    , '. . . . . . . . . . . . . . . . . . . . . . . . T T T T T F F F F F F F F F F') ;
   P(card8    , '. . . . . . . . . . . . . . . . . . . . . . . . . T T T T T F F F F F F F F F') ;
   P(card16   , '. . . . . . . . . . . . . . . . . . . . . . . . . . T T T F F F F F F F F F F') ;
   P(card32   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . T T F T F F F F F F F F') ;
   P(card64   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F T F F F F F F F') ;
   P(word16   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F F F F F') ;
   P(word32   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F T F F F F F F') ;
   P(word64   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F T F F F F F') ;
   P(real32   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F F') ;
   P(real64   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F F') ;
   P(real96   , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F F') ;
   P(real128  , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F F') ;
   P(set8     , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F F') ;
   P(set16    , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T F') ;
   P(set32    , '. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . T') ;

END InitCompatibilityMatrices ;


END M2Base.
