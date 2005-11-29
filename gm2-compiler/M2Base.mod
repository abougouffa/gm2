(* Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc. *)
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
                        RequestSym, GetSymName, NulSym,
                        PutImported, GetExported,
                        PopSize, PopValue,
                        FromModuleGetSym,
                        IsExportQualified, IsExportUnQualified ;

FROM M2ALU IMPORT PushIntegerTree, PushRealTree, PushCard, Equ ;
FROM M2Batch IMPORT MakeDefinitionSource ;
FROM M2Options IMPORT BoundsChecking, ReturnChecking,
                      NilChecking, CaseElseChecking,
                      Iso, Pim, Pim2 ;
FROM M2System IMPORT Address, Byte, Word, System, Loc, InitSystem ;
FROM M2Bitset IMPORT Bitset, GetBitsetMinMax, MakeBitset ;
FROM M2Size IMPORT Size, MakeSize ;

FROM gccgm2 IMPORT GetSizeOf, GetIntegerType, GetM2CharType,
                   GetMaxFrom, GetMinFrom, GetRealType,
                   GetLongIntType, GetLongRealType, GetProcType,
                   GetM2ShortRealType, GetM2RealType,
                   GetM2LongRealType, GetM2LongCardType,
                   GetShortIntType, GetM2ShortCardType,
                   GetM2CardinalType, GetPointerType, GetWordType,
                   GetByteType, GetISOWordType, GetISOByteType,
                   GetISOLocType ;

TYPE
   Compatability = (expression, assignment) ;
   MetaType      = (const, word, byte, address, chr,
                    normint, shortint, longint,
                    normcard, shortcard, longcard,
                    pointer, enum,
                    real, shortreal, longreal,
                    set, opaque, loc, unknown) ;
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
   Expr,
   Ass        : CompatibilityArray ;
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
   MaxInteger : CARDINAL ;


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
   Integer := MakeType(MakeKey('INTEGER')) ;
   PutType(Integer, NulSym) ;                 (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetIntegerType())) ;
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
   PushIntegerTree(GetSizeOf(GetLongIntType())) ;
   PopSize(LongInt) ;

   LongCard := MakeType(MakeKey('LONGCARD')) ;
   PutType(LongCard, NulSym) ;                (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2LongCardType())) ;
   PopSize(LongCard) ;

   ShortInt := MakeType(MakeKey('SHORTINT')) ;
   PutType(ShortInt, NulSym) ;                (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetShortIntType())) ;
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
   PushIntegerTree(GetMinFrom(GetIntegerType())) ;
   PopValue(MinInteger) ;
   PutVar(MinInteger, Integer) ;

   (* MaxInteger *)
   MaxInteger := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMaxFrom(GetIntegerType())) ;
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
   PushIntegerTree(GetMinFrom(GetLongIntType())) ;
   PopValue(MinLongInt) ;
   PutVar(MinLongInt, LongInt) ;

   (* MaxLongInt *)
   MaxLongInt := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMaxFrom(GetLongIntType())) ;
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
   GetBaseTypeMinMax - returns the minimum and maximum values for a
                       given system type. This procedure should only
                       be called if the type is NOT a subrange and NOT
                       an enumeration.
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
   ELSE
      n := GetSymName(type) ;
      WriteFormat1('unable to find MIN or MAX for the base type %a', n)
   END
END GetBaseTypeMinMax ;


(*
   InitBaseProcedures - initialises the base procedures,
                        INC, DEC, INCL, EXCL, NEW and DISPOSE.
*)

PROCEDURE InitBaseProcedures ;
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
      The procedure HALT is in fact a real procedure which
      is defined in M2RTS. However to remain compatible
      with other Modula-2 implementations HALT can be used
      without the need to import it from M2RTS. ie it is
      within the BaseType module scope.
   *)
   m2rts := MakeDefinitionSource(MakeKey('M2RTS')) ;
   PutImported(GetExported(m2rts, MakeKey('HALT'))) ;

   IF BoundsChecking
   THEN
      PutImported(GetExported(m2rts, MakeKey('SubrangeAssignmentError'))) ;
      PutImported(GetExported(m2rts, MakeKey('ArraySubscriptError')))
   END ;
   IF ReturnChecking
   THEN
      PutImported(GetExported(m2rts, MakeKey('FunctionReturnError')))
   END ;
   IF NilChecking
   THEN
      PutImported(GetExported(m2rts, MakeKey('NilPointerError')))
   END ;
   IF CaseElseChecking
   THEN
      PutImported(GetExported(m2rts, MakeKey('CaseElseError')))
   END
END InitBaseProcedures ;


(*
   InitBaseFunctions - initialises the base function,
                       HIGH and its associated Unbounded record structure.
*)

PROCEDURE InitBaseFunctions ;
BEGIN
   (* Now declare the dynamic array components, HIGH and _Unbounded *)
   High := MakeProcedure(MakeKey('HIGH')) ;  (* Pseudo Base function HIGH *)
   PutFunction(High, Cardinal) ;

   (*
     Unbounded = RECORD
                    _ArrayAddress: ADDRESS ;
                    _ArrayHigh   : CARDINAL ;
                 END ;
   *)
   Unbounded := MakeRecord(MakeKey('_Unbounded')) ;
   ArrayAddress := MakeKey('address') ;
   ArrayHigh := MakeKey('HIGH') ;
   PutFieldRecord(Unbounded, ArrayAddress, Address, NulSym) ;
   PutFieldRecord(Unbounded, ArrayHigh, Cardinal, NulSym) ;

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
   Ord     := MakeProcedure(MakeKey('ORD')) ;      (* Pseudo Base function ORD     *)
   Chr     := MakeProcedure(MakeKey('CHR')) ;      (* Pseudo Base function CHR     *)
   Float   := MakeProcedure(MakeKey('FLOAT')) ;    (* Pseudo Base function FLOAT   *)
   Trunc   := MakeProcedure(MakeKey('TRUNC')) ;    (* Pseudo Base function TRUNC   *)
   Min     := MakeProcedure(MakeKey('MIN')) ;      (* Pseudo Base function MIN     *)
   Max     := MakeProcedure(MakeKey('MAX'))        (* Pseudo Base function MIN     *)
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
          (Sym=High) OR (Sym=Val) OR (Sym=Convert) OR (Sym=Ord) OR
          (Sym=Chr) OR (Sym=Float) OR (Sym=Trunc) OR (Sym=Min) OR
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
          (Sym=Cardinal) OR (Sym=Integer)  OR (Sym=Boolean) OR
          (Sym=Char)     OR 
          (Sym=LongInt)  OR (Sym=LongCard) OR
          (Sym=ShortInt) OR (Sym=ShortCard) OR
          IsSubrange(Sym) OR IsEnumeration(Sym)
         )
END IsOrdinalType ;


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
            ErrorFormat0(e, 'warning type incompatibility, hint the types should be converted')
         ELSE
            ErrorFormat0(e, 'type incompatibility, hint the types should be converted or coerced')
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
   ELSIF (sym=Real) OR (sym=LongReal) OR (sym=ShortReal)
   THEN
      RETURN( real )
   ELSIF IsSet(sym)
   THEN
      RETURN( set )
   ELSIF IsHiddenType(sym)
   THEN
      RETURN( opaque )
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
   IF (kind=assignment) AND (t1=t2)
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

      expression: RETURN( Expr[mt1, mt2] ) |
      assignment: RETURN( Ass [mt1, mt2] )

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
   RETURN( (t=Real) OR (t=LongReal) OR (t=ShortReal) )
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
   IsIntCard - returns TRUE if, t, is of an INTEGER or CARDINAL type.
*)

PROCEDURE IsIntCard (t: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (t=Integer) OR (t=Cardinal) OR
          (t=LongCard) OR (t=ShortCard) OR
          (t=LongInt) OR (t=ShortInt)
         )
END IsIntCard ;


(*
   IsXor - returns TRUE if (a1=b1 and a2=b2) or
                           (a2=b1 and a1=b2)
*)

PROCEDURE IsXor (a1, a2, b1, b2: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( ((a1=b1) AND (a2=b2)) OR ((a2=b1) AND (a1=b2)) )
END IsXor ;


(*
   MixIntCard - promotes INTEGER and CARDINAL types.
*)

PROCEDURE MixIntCard (t1, t2: CARDINAL) : CARDINAL ;
BEGIN
   IF IsXor(t1, t2, Integer, Cardinal)
   THEN
      RETURN( Integer )
   ELSIF IsXor(t1, t2, Integer, LongCard)
   THEN
      RETURN( LongCard )
   ELSIF IsXor(t1, t2, Integer, ShortCard)
   THEN
      RETURN( Integer )
   ELSIF IsXor(t1, t2, Integer, LongInt)
   THEN
      RETURN( LongInt )
   ELSIF IsXor(t1, t2, Integer, ShortInt)
   THEN
      RETURN( Integer )
   ELSIF IsXor(t1, t2, Cardinal, LongCard)
   THEN
      RETURN( LongCard )
   ELSIF IsXor(t1, t2, Cardinal, ShortCard)
   THEN
      RETURN( Cardinal )
   ELSIF IsXor(t1, t2, Cardinal, LongInt)
   THEN
      RETURN( LongInt )
   ELSIF IsXor(t1, t2, Cardinal, ShortInt)
   THEN
      RETURN( Integer )
   ELSIF IsXor(t1, t2, ShortInt, Cardinal)
   THEN
      RETURN( Integer )
   ELSIF IsXor(t1, t2, ShortInt, LongCard)
   THEN
      RETURN( LongInt )
   ELSIF IsXor(t1, t2, ShortInt, ShortCard)
   THEN
      RETURN( ShortInt )
   ELSIF IsXor(t1, t2, ShortInt, LongInt)
   THEN
      RETURN( LongInt )
   ELSIF IsXor(t1, t2, ShortCard, Cardinal)
   THEN
      RETURN( Cardinal )
   ELSIF IsXor(t1, t2, ShortCard, LongCard)
   THEN
      RETURN( LongCard )
   ELSIF IsXor(t1, t2, ShortCard, LongInt)
   THEN
      RETURN( LongInt )
   ELSIF IsXor(t1, t2, LongInt, LongCard)
   THEN
      RETURN( LongInt )
   ELSE
      InternalError('not expecting these types', __FILE__, __LINE__)
   END
END MixIntCard ;


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
   MixTypes - returns the type symbol that corresponds to the types t1
              and t2.
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
          (type=LongReal) OR (type=ShortReal)
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
   normint  :   PushIntegerTree(GetSizeOf(GetIntegerType())) |
   shortint :   PushIntegerTree(GetSizeOf(GetShortIntType())) |
   longint  :   PushIntegerTree(GetSizeOf(GetLongIntType())) |
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
   unknown  :   InternalError('should not get here', __FILE__, __LINE__)

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
         Expr[i, j]  := uninitialized
      END
   END ;

   (* all unknowns are false *)
   FOR i := MIN(MetaType) TO MAX(MetaType) DO
      Ass[i, unknown]  := no ;
      Expr[unknown, i] := no
   END ;

   (*
                                     1 p w

                 N  W  B  A  C  I  S  L  C  S  L  P  E  R  S  L  S  O  L
                 u  o  y  d  h  n  h  o  a  h  o  t  n  e  h  o  e  p  o
                 l  r  t  d  a  t  o  n  r  o  n  r  u  a  o  n  t  a  c
                 S  d  e  r  r  e  r  g  d  r  g     m  l  r  g     q
                 y        e     g  t  i  i  t  c           t  r     u
                 m        s     e  i  n  n  c  a           r  e     e
                          s     r  n  t  a  a  r           e  a
                                   t     l  r  d           a  l
                                            d              l
      -------------------------------------------------------------------
   2
   P
   W
   *)
   A(const    , 'T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T') ;
   A(word     , '.  T  F  W  F  2  W  W  2  W  W  W  2  F  W  W  T  T  F') ;
   A(byte     , '.  .  T  F  2  F  F  F  F  F  F  F  F  F  F  F  F  F  F') ;
   A(address  , '.  .  .  T  F  F  F  F  F  F  F  2  F  F  F  F  F  2  2') ;
   A(chr      , '.  .  .  .  T  F  F  F  F  F  F  F  F  F  F  F  F  F  2') ;
   A(normint  , '.  .  .  .  .  T  2  1  2  2  1  F  F  F  F  F  F  F  F') ;
   A(shortint , '.  .  .  .  .  .  T  1  1  2  1  F  F  F  F  F  F  F  F') ;
   A(longint  , '.  .  .  .  .  .  .  T  2  2  2  F  F  F  F  F  F  F  F') ;
   A(normcard , '.  .  .  .  .  .  .  .  T  2  1  F  F  F  F  F  F  F  F') ;
   A(shortcard, '.  .  .  .  .  .  .  .  .  T  1  F  F  F  F  F  F  F  F') ;
   A(longcard , '.  .  .  .  .  .  .  .  .  .  T  F  F  F  F  F  F  F  F') ;
   A(pointer  , '.  .  .  .  .  .  .  .  .  .  .  T  F  F  F  F  F  F  F') ;
   A(enum     , '.  .  .  .  .  .  .  .  .  .  .  .  T  F  F  F  F  F  F') ;
   A(real     , '.  .  .  .  .  .  .  .  .  .  .  .  .  T  F  F  F  F  F') ;
   A(shortreal, '.  .  .  .  .  .  .  .  .  .  .  .  .  .  T  F  F  F  F') ;
   A(longreal , '.  .  .  .  .  .  .  .  .  .  .  .  .  .  .  T  F  F  F') ;
   A(set      , '.  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  T  F  F') ;
   A(opaque   , '.  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  T  F') ;
   A(loc      , '.  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  T') ;


   (* Expression compatibility *)


   (*
                                        1 p w

                 N  W  B  A  C  I  S  L  C  S  L  P  E  R  S  L  S  O  L
                 u  o  y  d  h  n  h  o  a  h  o  t  n  e  h  o  e  p  o
                 l  r  t  d  a  t  o  n  r  o  n  r  u  a  o  n  t  a  c
                 S  d  e  r  r  e  r  g  d  r  g     m  l  r  g     q
                 y        e     g  t  i  i  t  c           t  r     u
                 m        s     e  i  n  n  c  a           r  e     e
                          s     r  n  t  a  a  r           e  a
                                   t     l  r  d           a  l
                                            d              l
      -------------------------------------------------------------------
   2
   P
   W
   *)

   E(const    , 'T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  F') ;
   E(word     , '.  T  F  F  F  F  F  F  F  F  F  F  F  F  F  F  F  F  F') ;
   E(byte     , '.  .  T  F  F  F  F  F  F  F  F  F  F  F  F  F  F  F  F') ;
   E(address  , '.  .  .  T  F  P  F  F  P  F  F  T  F  F  F  F  F  F  F') ;
   E(chr      , '.  .  .  .  T  F  F  F  F  F  F  F  F  F  F  F  F  F  F') ;
   E(normint  , '.  .  .  .  .  T  2  1  2  2  1  F  F  F  F  F  F  F  F') ;
   E(shortint , '.  .  .  .  .  .  T  1  1  2  1  F  F  F  F  F  F  F  F') ;
   E(longint  , '.  .  .  .  .  .  .  T  2  2  2  F  F  F  F  F  F  F  F') ;
   E(normcard , '.  .  .  .  .  .  .  .  T  2  1  F  F  F  F  F  F  F  F') ;
   E(shortcard, '.  .  .  .  .  .  .  .  .  T  1  F  F  F  F  F  F  F  F') ;
   E(longcard , '.  .  .  .  .  .  .  .  .  .  T  F  F  F  F  F  F  F  F') ;
   E(pointer  , '.  .  .  .  .  .  .  .  .  .  .  T  F  F  F  F  F  F  F') ;
   E(enum     , '.  .  .  .  .  .  .  .  .  .  .  .  T  F  F  F  F  F  F') ;
   E(real     , '.  .  .  .  .  .  .  .  .  .  .  .  .  T  2  1  F  F  F') ;
   E(shortreal, '.  .  .  .  .  .  .  .  .  .  .  .  .  .  T  1  F  F  F') ;
   E(longreal , '.  .  .  .  .  .  .  .  .  .  .  .  .  .  .  T  F  F  F') ;
   E(set      , '.  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  T  F  F') ;
   E(opaque   , '.  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  T  F') ;
   E(loc      , '.  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  F')

END InitCompatibilityMatrices ;


END M2Base.
