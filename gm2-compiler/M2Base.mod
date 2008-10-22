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

FROM DynamicStrings IMPORT InitString, String, Mark, InitStringCharStar, ConCat ;
FROM M2LexBuf IMPORT GetTokenNo ;
FROM NameKey IMPORT MakeKey, WriteKey, KeyToCharStar ;
FROM M2Debug IMPORT Assert ;
FROM SYSTEM IMPORT WORD ;

FROM M2Error IMPORT InternalError, FlushErrors ;
FROM M2Pass IMPORT IsPassCodeGeneration ;
FROM FormatStrings IMPORT Sprintf2 ;
FROM StrLib IMPORT StrLen ;

FROM M2MetaError IMPORT MetaError1, MetaError2, MetaErrors3,
                        MetaErrorT1, MetaErrorT2,
                        MetaErrorStringT2, MetaErrorStringT1 ;

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
                        IsExportQualified, IsExportUnQualified,
                        IsParameter, IsParameterVar, IsUnbounded,
                        IsConst, IsUnboundedParam,
                        IsParameterUnbounded,  GetSubrange,
                        IsArray, IsProcedure, IsConstString,
                        IsVarient, IsRecordField, IsFieldVarient,
                        GetArraySubscript, IsRecord, NoOfParam,
                        GetNthParam, IsVarParam, GetNth ;

FROM M2ALU IMPORT PushIntegerTree, PushRealTree, PushCard, Equ, Gre, Less ;
FROM M2Batch IMPORT MakeDefinitionSource ;
FROM M2Bitset IMPORT Bitset, GetBitsetMinMax, MakeBitset ;
FROM M2Size IMPORT Size, MakeSize ;

FROM M2System IMPORT Address, Byte, Word, System, Loc, InitSystem, 
                     IntegerN, CardinalN, WordN, SetN, RealN,
                     IsCardinalN, IsIntegerN,
                     IsGenericSystemType ;

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
PROCEDURE AfterResolved (t1, t2: CARDINAL; kind: Compatability) : Compatible ; FORWARD ;
PROCEDURE BeforeResolved (t1, t2: CARDINAL; kind: Compatability) : Compatible ; FORWARD ;
PROCEDURE IsSameType (t1, t2: CARDINAL; error: BOOLEAN) : BOOLEAN ; FORWARD ;
PROCEDURE IsProcTypeSame (p1, p2: CARDINAL; error: BOOLEAN) : BOOLEAN ; FORWARD ;
PROCEDURE IsPointerSame (a, b: CARDINAL; error: BOOLEAN) : BOOLEAN ; FORWARD ;
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
      MetaError1('unable to find MIN or MAX for the base type {%1as}', type)
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
   ExceptionIncl         := NulSym ;
   ExceptionExcl         := NulSym ;
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
      ExceptionIncl := ImportFrom(m2rts, 'InclException') ;
      ExceptionExcl := ImportFrom(m2rts, 'ExclException') ;
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

PROCEDURE EmitTypeIncompatibleWarning (kind: Compatability; t1, t2: CARDINAL) ;
BEGIN
   CASE kind OF

   expression:  MetaError2('{%1W:} type incompatibility found {%1as:{%2as:between types {%1as} {%2as}}} in an expression, hint one of the expressions should be converted', t1, t2) |
   assignment:  MetaError2('{%1W:} type incompatibility found {%1as:{%2as:between types {%1as} {%2as}}} during an assignment, hint maybe the expression should be converted', t1, t2) |
   parameter :  MetaError2('{%1W:} type incompatibility found when passing a parameter {%1as:{%2as:between formal parameter and actual parameter types {%1as} {%2as}}}, hint the actual parameter {%2a} should be converted', t1, t2)

   ELSE
   END
END EmitTypeIncompatibleWarning ;


(*
   EmitTypeIncompatibleError - emit a type incompatibility error.
*)

PROCEDURE EmitTypeIncompatibleError (kind: Compatability; t1, t2: CARDINAL) ;
BEGIN
   CASE kind OF

   expression:  MetaError2('type incompatibility found {%1as:{%2as:between types {%1as} {%2as}}} in an expression, hint one of the expressions should be converted', t1, t2) |
   assignment:  MetaError2('type incompatibility found {%1as:{%2as:between types {%1as} {%2as}}} during an assignment, hint maybe the expression should be converted', t1, t2) |
   parameter :  MetaError2('type incompatibility found when passing a parameter {%1as:{%2as:between formal parameter and actual parameter types {%1as} {%2as}}}, hint the actual parameter should be converted', t1, t2)

   ELSE
   END
END EmitTypeIncompatibleError ;


(*
   CheckCompatible - returns if t1 and t2 are kind compatible
*)

PROCEDURE CheckCompatible (t1, t2: CARDINAL; kind: Compatability) ;
VAR
   n: Name ;
   s: String ;
   r: Compatible ;
BEGIN
   r := IsCompatible(t1, t2, kind) ;
   IF (r#first) AND (r#second)
   THEN
      IF (r=warnfirst) OR (r=warnsecond)
      THEN
         s := InitString('{%1W}')
      ELSE
         s := InitString('')
      END ;
      IF IsUnknown(t1) AND IsUnknown(t2)
      THEN
         s := ConCat(s, InitString('two different unknown types {%1a:{%2a:{%1a} and {%2a}}} must either be declared or imported)')) ;
         MetaErrorStringT2(GetTokenNo(), s, t1, t2)
      ELSIF IsUnknown(t1)
      THEN
         s := ConCat(s, InitString('this type {%1a} is currently unknown, it must be declared or imported')) ;
         MetaErrorStringT1(GetTokenNo(), s, t1)
      ELSIF IsUnknown(t2)
      THEN
         s := ConCat(s, InitString('this type {%1a} is currently unknown, it must be declared or imported')) ;
         MetaErrorStringT1(GetTokenNo(), s, t2)
      ELSE
         IF (r=warnfirst) OR (r=warnsecond)
         THEN
            EmitTypeIncompatibleWarning(kind, t1, t2)
         ELSE
            EmitTypeIncompatibleError(kind, t1, t2)
         END
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
   ELSIF IsSet(sym)
   THEN
      RETURN( set )
   ELSIF IsHiddenType(sym)
   THEN
      RETURN( opaque )
   ELSIF IsPointer(sym)
   THEN
      RETURN( pointer )
   ELSIF IsEnumeration(sym)
   THEN
      RETURN( enum )
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
   IF (t1=t2) AND ((kind=assignment) OR (kind=parameter))
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
   CannotCheckTypeInPass3 - returns TRUE if we are unable to check the
                            type of, e, in pass 3.
*)

PROCEDURE CannotCheckTypeInPass3 (e: CARDINAL) : BOOLEAN ;
VAR
   t : CARDINAL ;
   mt: MetaType ;
BEGIN
   t := SkipType(GetType(e)) ;
   mt := FindMetaType(t) ;
   CASE mt OF

   pointer,
   enum,
   set,
   set8,
   set16,
   set32,
   opaque :  RETURN( TRUE )

   ELSE
      RETURN( FALSE )
   END
END CannotCheckTypeInPass3 ;


(*
   IsCompatible - returns true if the types, t1, and, t2, are compatible.
*)

PROCEDURE IsCompatible (t1, t2: CARDINAL; kind: Compatability) : Compatible ;
BEGIN
   t1 := SkipType(t1) ;
   t2 := SkipType(t2) ;
   IF IsPassCodeGeneration()
   THEN
      RETURN( AfterResolved(t1, t2, kind) )
   ELSE
      RETURN( BeforeResolved(t1, t2, kind) )
   END
END IsCompatible ;


(*
   IsPointerSame - returns TRUE if pointers, a, and, b, are the same.
*)

PROCEDURE IsPointerSame (a, b: CARDINAL; error: BOOLEAN) : BOOLEAN ;
BEGIN
   RETURN( IsSameType(SkipType(GetType(a)), SkipType(GetType(b)), error) )
END IsPointerSame ;


(*
   IsSubrangeSame - checks to see whether the subranges are the same.
*)

PROCEDURE IsSubrangeSame (a, b: CARDINAL) : BOOLEAN ;
VAR
   al, ah,
   bl, bh: CARDINAL ;
BEGIN
   a := SkipType(a) ;
   b := SkipType(b) ;
   IF a#b
   THEN
      GetSubrange(a, ah, al) ;
      GetSubrange(b, bh, bl) ;
      PushValue(al) ;
      PushValue(bl) ;
      IF NOT Equ(GetDeclared(a))
      THEN
         RETURN( FALSE )
      END ;
      PushValue(ah) ;
      PushValue(bh) ;
      IF NOT Equ(GetDeclared(a))
      THEN
         RETURN( FALSE )
      END
   END ;
   RETURN( TRUE )
END IsSubrangeSame ;


(*
   IsVarientSame - returns TRUE if varient types, a, and, b, are identical.
*)

PROCEDURE IsVarientSame (a, b: CARDINAL; error: BOOLEAN) : BOOLEAN ;
VAR
   i, j  : CARDINAL ;
   ta, tb,
   fa, fb,
   ga, gb: CARDINAL ;
BEGIN
   i := 1 ;
   REPEAT
      fa := GetNth(a, i) ;
      fb := GetNth(b, i) ;
      IF (fa#NulSym) AND (fb#NulSym)
      THEN
         Assert(IsFieldVarient(fa)) ;
         Assert(IsFieldVarient(fb)) ;
         j := 1 ;
         REPEAT
            ga := GetNth(fa, j) ;
            gb := GetNth(fb, j) ;
            IF (ga#NulSym) AND (gb#NulSym)
            THEN
               IF NOT IsSameType(GetType(ga), GetType(gb), error)
               THEN
                  RETURN( FALSE )
               END ;
               INC(j)
            END
         UNTIL (ga=NulSym) OR (gb=NulSym) ;
         IF ga#gb
         THEN
            RETURN( FALSE )
         END
      END ;
      INC(i)
   UNTIL (fa=NulSym) OR (fb=NulSym) ;
   RETURN( ga=gb )
END IsVarientSame ;


(*
   IsRecordSame - 
*)

PROCEDURE IsRecordSame (a, b: CARDINAL; error: BOOLEAN) : BOOLEAN ;
VAR
   ta, tb,
   fa, fb: CARDINAL ;
   i     : CARDINAL ;
BEGIN
   i := 1 ;
   REPEAT
      fa := GetNth(a, i) ;
      fb := GetNth(b, i) ;
      IF (fa#NulSym) AND (fb#NulSym)
      THEN
         ta := GetType(fa) ;
         tb := GetType(fb) ;
         IF IsRecordField(fa) AND IsRecordField(fb)
         THEN
            IF NOT IsSameType(ta, tb, error)
            THEN
               RETURN( FALSE )
            END
         ELSIF IsVarient(fa) AND IsVarient(fb)
         THEN
            IF NOT IsVarientSame(ta, tb, error)
            THEN
               RETURN( FALSE )
            END
      	 ELSIF IsFieldVarient(fa) OR IsFieldVarient(fb)
      	 THEN
            InternalError('should not see a field varient', __FILE__, __LINE__)
         ELSE
            RETURN( FALSE )
      	 END
      END ;
      INC(i)
   UNTIL (fa=NulSym) OR (fb=NulSym) ;
   RETURN( fa=fb )
END IsRecordSame ;


(*
   IsArraySame - 
*)

PROCEDURE IsArraySame (t1, t2: CARDINAL; error: BOOLEAN) : BOOLEAN ;
VAR
   s1, s2: CARDINAL ;
BEGIN
   s1 := GetArraySubscript(t1) ;
   s2 := GetArraySubscript(t2) ;
   RETURN( IsSameType(GetType(s1), GetType(s2), error) AND
           IsSameType(GetType(t1), GetType(t2), error) )
END IsArraySame ;


(*
   IsEnumerationSame - 
*)

PROCEDURE IsEnumerationSame (t1, t2: CARDINAL; error: BOOLEAN) : BOOLEAN ;
BEGIN
   RETURN( t1=t2 )
END IsEnumerationSame ;


(*
   IsSetSame - 
*)

PROCEDURE IsSetSame (t1, t2: CARDINAL; error: BOOLEAN) : BOOLEAN ;
BEGIN
   RETURN( IsSameType(GetType(t1), GetType(t2), error) )
END IsSetSame ;


(*
   IsSameType - returns TRUE if
*)

PROCEDURE IsSameType (t1, t2: CARDINAL; error: BOOLEAN) : BOOLEAN ;
BEGIN
   t1 := SkipType(t1) ;
   t2 := SkipType(t2) ;
   IF t1=t2
   THEN
      RETURN( TRUE )
   ELSIF IsArray(t1) AND IsArray(t2)
   THEN
      RETURN( IsArraySame(t1, t2, error) )
   ELSIF IsSubrange(t1) AND IsSubrange(t2)
   THEN
      RETURN( IsSubrangeSame(t1, t2) )
   ELSIF IsProcType(t1) AND IsProcType(t2)
   THEN
      RETURN( IsProcTypeSame(t1, t2, error) )
   ELSIF IsEnumeration(t1) AND IsEnumeration(t2)
   THEN
      RETURN( IsEnumerationSame(t1, t2, error) )
   ELSIF IsRecord(t1) AND IsRecord(t2)
   THEN
      RETURN( IsRecordSame(t1, t2, error) )
   ELSIF IsSet(t1) AND IsSet(t2)
   THEN
      RETURN( IsSetSame(t1, t2, error) )
   ELSIF IsPointer(t1) AND IsPointer(t2)
   THEN
      RETURN( IsPointerSame(t1, t2, error) )
   ELSE
      RETURN( FALSE )
   END
END IsSameType ;


(*
   IsProcTypeSame - 
*)

PROCEDURE IsProcTypeSame (p1, p2: CARDINAL; error: BOOLEAN) : BOOLEAN ;
VAR
   pa, pb: CARDINAL ;
   n, i  : CARDINAL ;
BEGIN
   n := NoOfParam(p1) ;
   IF n#NoOfParam(p2)
   THEN
      IF error
      THEN
         MetaError2('parameter is incompatible as {%1Dd} was declared with {%2n} parameters', p1, NoOfParam(p1)) ;
         MetaError2('whereas {%1Dd} was declared with {%2n} parameters', p2, NoOfParam(p2))
      END ;
      RETURN( FALSE )
   END ;
   i := 1 ;
   WHILE i<=n DO
      pa := GetNthParam(p1, i) ;
      pb := GetNthParam(p2, i) ;
      IF IsVarParam(p1, i)#IsVarParam(p2, i)
      THEN
         IF error
         THEN
            MetaErrors3('the {%1n} parameter is incompatible between {%2Dad} and {%3ad} as only one was declared as VAR',
                        'the {%1n} parameter is incompatible between {%2ad} and {%3Dad} as only one was declared as VAR',
                        i, p1, p2)
         END ;
         RETURN( FALSE )
      END ;
      IF NOT IsSameType(GetType(pa), GetType(pb), error)
      THEN
         RETURN( FALSE )
      END ;
      INC(i)
   END ;
   RETURN( IsSameType(GetType(p1), GetType(p2), error) )
END IsProcTypeSame ;


(*
   doProcTypeCheck - 
*)

PROCEDURE doProcTypeCheck (p1, p2: CARDINAL; error: BOOLEAN) : BOOLEAN ;
BEGIN
   IF IsProcType(p1) AND IsProcType(p2)
   THEN
      IF p1=p2
      THEN
         RETURN( TRUE )
      ELSE
         RETURN( IsProcTypeSame(p1, p2, error) )
      END
   ELSE
      RETURN( FALSE )
   END
END doProcTypeCheck ;


(*
   AfterResolved - a though test for type compatibility.
*)

PROCEDURE AfterResolved (t1, t2: CARDINAL; kind: Compatability) : Compatible ;
VAR
   mt1, mt2: MetaType ;
BEGIN
   IF (t1=NulSym) OR (t2=NulSym)
   THEN
      RETURN( first )
   ELSIF ((kind=parameter) OR (kind=assignment)) AND (t1=t2)
   THEN
      RETURN( first )
   ELSIF IsSubrange(t1)
   THEN
      RETURN( IsCompatible(GetType(t1), t2, kind) )
   ELSIF IsSubrange(t2)
   THEN
      RETURN( IsCompatible(t1, GetType(t2), kind) )
   ELSE
      mt1 := FindMetaType(t1) ;
      mt2 := FindMetaType(t2) ;
      IF mt1=mt2
      THEN
         CASE mt1 OF

         set,
         set8,
         set16,
         set32  :  IF IsSetSame(t1, t2, FALSE)
                   THEN
                      RETURN( first )
                   ELSE
                      RETURN( no )
                   END |
         enum   :  IF IsEnumerationSame(t1, t2, FALSE)
                   THEN
                      RETURN( first )
                   ELSE
                      RETURN( no )
                   END |
         pointer:  IF IsPointerSame(t1, t2, FALSE)
                   THEN
                      RETURN( first )
                   ELSE
                      RETURN( no )
                   END |
         opaque :  RETURN( no )

         ELSE
            (* fall through *)
         END
      END ;
      RETURN( IsBaseCompatible(t1, t2, kind) )
   END
END AfterResolved ;


(*
   BeforeResolved - attempts to test for type compatibility before all types are
                    completely resolved.  In particular set types and constructor
                    types are not fully known before the end of pass 3.
                    However we can test base types.
*)

PROCEDURE BeforeResolved (t1, t2: CARDINAL; kind: Compatability) : Compatible ;
BEGIN
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
      (* cannot test set compatibility at this point so we do this again after pass 3 *)
      RETURN( first )
   ELSIF (IsHiddenType(t1) OR IsProcType(t1)) AND ((kind=assignment) OR (kind=parameter))
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
        however we do not know the type to our subranges yet as (GetType(SubrangeType)=NulSym).
        So we add type checking in the range checking module which is done post pass 3,
        when all is resolved.
*)

      RETURN( IsBaseCompatible(t1, t2, kind) )
   END
END BeforeResolved ;


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

PROCEDURE IsParameterCompatible (t1, t2: CARDINAL (* ; tokenNo: CARDINAL *) ) : BOOLEAN ;
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
BEGIN
   mt1 := FindMetaType(t1) ;
   mt2 := FindMetaType(t2) ;
   CASE Expr[mt1, mt2] OF

   no        :  MetaErrorT2(NearTok, 'type incompatibility between {%1as} and {%2as}', t1, t2) ;
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
   ELSIF IsType(t1)
   THEN
      RETURN( MixTypes(GetType(t1), t2, NearTok) )
   ELSIF IsType(t2)
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
   IsValidParameter - returns TRUE if an, actual, parameter can be passed
                      to the, formal, parameter.  This differs from
                      IsParameterCompatible as this procedure includes checks
                      for unbounded formal parameters, var parameters and
                      constant actual parameters.
*)

PROCEDURE IsValidParameter (formal, actual: CARDINAL (* ; tokenNo: CARDINAL *) ) : BOOLEAN ;
VAR
   at, ft: CARDINAL ;
BEGIN
   Assert(IsParameter(formal)) ;
   Assert(IsPassCodeGeneration()) ;
   IF IsConst(actual) AND IsParameterVar(formal)
   THEN
      RETURN( FALSE )
   ELSE
      IF IsParameterUnbounded(formal)
      THEN
         ft := SkipType(GetType(GetType(formal))) ;    (* ARRAY OF ft *)
         IF IsGenericSystemType(ft) OR (GetType(formal)=GetType(actual))
         THEN
            RETURN( TRUE )
         ELSE
            IF IsParameter(actual) AND IsParameterUnbounded(actual)
            THEN
               (* ARRAY OF ...      GetType(GetType(actual)) *)
               RETURN( IsParameterCompatible(GetType(GetType(actual)), ft) )
            ELSE
               IF IsConstString(actual)
               THEN
                  RETURN( IsParameterCompatible(Char, ft) )
               ELSE
                  at := SkipType(GetType(actual)) ;
                  IF IsArray(at)
                  THEN
                     RETURN( IsParameterCompatible(GetType(at), ft) )
                  ELSE
                     RETURN( FALSE )
                  END
               END
            END
         END
      ELSE
         ft := GetType(formal)
      END ;
      IF IsProcType(ft)
      THEN
         IF IsProcedure(actual)
         THEN
            (* we check this by calling IsValidProcedure for each and every
               parameter of actual and formal *)
            RETURN( TRUE )
         ELSE
            at := SkipType(GetType(actual)) ;
            RETURN( doProcTypeCheck(at, ft, TRUE) )
         END
      ELSE
         RETURN( IsParameterCompatible(GetType(actual), ft) )
      END
   END
END IsValidParameter ;


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
   P(word     , '. T F W F 2 W W 2 W W W 2 W W W T T F W T F F F F F F F F F F F F F F F F F F') ;
   P(byte     , '. . T F 2 F F F F F F F F F F F F F F F T S F F F S F F F F F F F F F F S F F') ;
   P(address  , '. . . T F F F F F F F T F F F F F T T F T F F F F F F F F F F F F F F F F F F') ;
   P(chr      , '. . . . T F F F F F F F F F F F F F T F F F F F F F F F F F F F F F F F F F F') ;
   P(normint  , '. . . . . T F F T F F F F F F F F F F F 2 T T T T T T T T F F F F F F F F F F') ;
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
