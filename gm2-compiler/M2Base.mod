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

FROM M2Options IMPORT Iso ;
FROM DynamicStrings IMPORT InitString, String, Mark, InitStringCharStar ;
FROM M2LexBuf IMPORT GetTokenNo ;
FROM NameKey IMPORT MakeKey, WriteKey, KeyToCharStar ;

FROM M2Error IMPORT Error, NewError, ErrorFormat0, ErrorFormat1, ErrorFormat2,
                    InternalError, ChainError, WriteFormat1, ErrorString, FlushErrors ;

FROM FormatStrings IMPORT Sprintf2 ;

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
                        GetType, GetLowestType, GetDeclared, SkipType,
                        SetCurrentModule,
                        StartScope, EndScope, PseudoScope,
                        RequestSym, GetSymName, NulSym,
                        PutImported, GetExported,
                        PopSize, PopValue ;

FROM M2ALU IMPORT PushIntegerTree, PushCard ;
FROM M2Batch IMPORT MakeDefinitionSource ;
FROM M2Options IMPORT BoundsChecking, ReturnChecking ;
FROM M2System IMPORT Address, Byte, Word, InitSystem ;
FROM M2Bitset IMPORT Bitset, GetBitsetMinMax, MakeBitset ;
FROM M2Size IMPORT Size, MakeSize ;

FROM gccgm2 IMPORT GetSizeOf, GetIntegerType, GetM2CharType, GetMaxFrom, GetMinFrom,
                   GetRealType, GetLongIntType, GetLongRealType, GetProcType,
                   GetM2ShortRealType, GetM2RealType, GetM2LongRealType, GetM2LongCardType,
                   GetM2CardinalType ;

TYPE
   Compatability = (expression, assignment) ;
   MetaType      = (const, word, byte, address, chr, intgr, cardinal, pointer, enum, real, set) ;


(* %%%FORWARD%%%
PROCEDURE InitBaseConstants ; FORWARD ;
PROCEDURE InitBaseSimpleTypes ; FORWARD ;
PROCEDURE InitBaseFunctions ; FORWARD ;
PROCEDURE InitBaseProcedures ; FORWARD ;
PROCEDURE InitCompatibilityMatrices ; FORWARD ;
PROCEDURE IsCompatible (t1, t2: CARDINAL; kind: Compatability) : BOOLEAN ; FORWARD ;
   %%%FORWARD%%% *)


VAR
   Expr,
   Ass        : ARRAY MetaType, MetaType OF BOOLEAN ;
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


(*
   PushIntegerTree(GetMaxFrom(GetM2CardinalType())) ;
   PopValue(MaxCard) ;
   Cardinal := MakeSubrange(MakeKey('CARDINAL')) ;
   (*
      Cardinal = 0..2^BitsPerWord-1
   *)
   PutSubrange(Cardinal, Zero, MaxCard, NulSym) ;
*)
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
   PutType(LongCard, NulSym) ;                 (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2LongCardType())) ;
   PopSize(LongCard) ;

   Real := MakeType(MakeKey('REAL')) ;
   PutType(Real, NulSym) ;                    (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetM2RealType())) ;
   PopSize(Real) ;

   ShortReal := MakeType(MakeKey('SHORTREAL')) ;
   PutType(ShortReal, NulSym) ;                    (* Base Type       *)
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

END InitBaseSimpleTypes ;


(*
   GetBaseTypeMinMax - returns the minimum and maximum values for a given system type.
                       This procedure should only be called if the type is NOT a subrange
                       and NOT an enumeration.
*)

PROCEDURE GetBaseTypeMinMax (type: CARDINAL; VAR min, max: CARDINAL) ;
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
   ELSE
      WriteFormat1('unable to find MIN or MAX for the base type %a', GetSymName(type))
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

   IF Iso
   THEN
      MakeSize  (* SIZE is declared as a standard function in ISO Modula-2 *)
   END ;

   (*
      The procedure HALT is in fact a real procedure which
      is defined in M2RTS. However to remain compatible
      with other Modula-2 implementations HALT can be used
      without the need to import it from M2RTS. ie it is
      within the BaseType module scope.
   *)
   PutImported(
               GetExported(MakeDefinitionSource(MakeKey('M2RTS')),
                           MakeKey('HALT'))
              ) ;

   IF BoundsChecking
   THEN
      PutImported(
                  GetExported(MakeDefinitionSource(MakeKey('M2RTS')),
                              MakeKey('SubrangeAssignmentError'))
                 ) ;
      PutImported(
                  GetExported(MakeDefinitionSource(MakeKey('M2RTS')),
                              MakeKey('ArraySubscriptError'))
                 )
   END ;
   IF ReturnChecking
   THEN
      PutImported(
                  GetExported(MakeDefinitionSource(MakeKey('M2RTS')),
                              MakeKey('FunctionReturnError'))
                 )
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
   PutFieldRecord(Unbounded, ArrayAddress, Address) ;
   PutFieldRecord(Unbounded, ArrayHigh, Cardinal) ;

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
   RETURN( Iso AND (Sym#NulSym) AND ((Sym=LengthS) OR (Sym=Size)) )
END IsISOPseudoBaseFunction ;


(*
   IsPseudoBaseFunction - returns true if Sym is a Base pseudo function.
*)

PROCEDURE IsPseudoBaseFunction (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (Sym=High) OR (Sym=Val) OR (Sym=Convert) OR (Sym=Ord) OR
          (Sym=Chr) OR (Sym=Float) OR (Sym=Trunc) OR (Sym=Min) OR
          (Sym=Max) OR (Sym=Abs) OR (Sym=Odd) OR (Sym=Cap) OR
          IsISOPseudoBaseFunction(Sym)
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
          (Sym=Char)     OR (Sym=Proc)     OR (Sym=LongInt) OR
          (Sym=Real)     OR (Sym=LongReal) OR (Sym=ShortReal) OR
          ((Sym=Bitset) AND Iso)
         )
END IsBaseType ;


(*
   CheckCompatible - returns if t1 and t2 are kind compatible
*)

PROCEDURE CheckCompatible (t1, t2: CARDINAL; kind: Compatability) ;
VAR
   e: Error ;
   s: String ;
BEGIN
   IF NOT IsCompatible(t1, t2, kind)
   THEN
      e := NewError(GetTokenNo()) ;
      IF IsUnknown(t1) AND IsUnknown(t2)
      THEN
         ErrorFormat0(e, 'two different unknown types must be resolved (declared or imported)')
      ELSIF IsUnknown(t1) OR IsUnknown(t2)
      THEN
         ErrorFormat0(e, 'a type must be declared or imported')
      ELSE
         ErrorFormat0(e, 'type incompatibility, hint the types should be converted or coerced')
      END ;

      e := ChainError(GetTokenNo(), e) ;
      s := Sprintf2(Mark(InitString('the two types are: %s and %s\n')),
                    Mark(InitStringCharStar(KeyToCharStar(GetSymName(t1)))),
                    Mark(InitStringCharStar(KeyToCharStar(GetSymName(t2))))) ;
      ErrorString(e, s) ;
      IF IsUnknown(t1)
      THEN
         e := ChainError(GetDeclared(t1), e) ;
         ErrorFormat1(e, 'hint, %a, is unknown and perhaps should be declared or imported',
                      GetSymName(t1))
      END ;

      IF IsUnknown(t2)
      THEN
         e := ChainError(GetDeclared(t2), e) ;
         ErrorFormat1(e, 'hint, %a, is unknown and perhaps should be declared or imported',
                      GetSymName(t2))
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
   ELSIF sym=Address
   THEN
      RETURN( address )
   ELSIF sym=Char
   THEN
      RETURN( chr )
   ELSIF sym=Integer
   THEN
      RETURN( intgr )
   ELSIF sym=Cardinal
   THEN
      RETURN( cardinal )
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
   ELSIF IsType(sym)
   THEN
      RETURN( FindMetaType(GetType(sym)) )
   ELSE
      InternalError('unexpected base type', __FILE__, __LINE__)
   END
END FindMetaType ;


(*
   IsBaseCompatible - returns TRUE if a simple base type comparison is legal.
*)

PROCEDURE IsBaseCompatible (t1, t2: CARDINAL; kind: Compatability) : BOOLEAN ;
BEGIN
   CASE kind OF

   expression: RETURN( Expr[FindMetaType(t1), FindMetaType(t2)] ) |
   assignment: RETURN( Ass [FindMetaType(t1), FindMetaType(t2)] )

   ELSE
      InternalError('unexpected Compatibility', __FILE__, __LINE__)
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

PROCEDURE IsCompatible (t1, t2: CARDINAL; kind: Compatability) : BOOLEAN ;
BEGIN
   t1 := SkipType(t1) ;
   t2 := SkipType(t2) ;
   IF (t1=NulSym) OR (t2=NulSym)
   THEN
      RETURN( TRUE )
   ELSIF IsSubrange(t1)
   THEN
      (* since we check for t1 or t2 for subranges we will never ask IsCompatible about CARDINAL *)
      RETURN( IsCompatible(GetType(t1), t2, kind) )
   ELSIF IsSubrange(t2)
   THEN
      RETURN( IsCompatible(t1, GetType(t2), kind) )
   ELSIF IsSet(t1)
   THEN
      RETURN( TRUE ) (* cannot test set compatability at this point --fixme-- *)
   ELSIF IsSet(t2)
   THEN
      RETURN( TRUE ) (* cannot test set compatability at this point --fixme-- *)
   ELSIF IsBaseCompatible(t1, t2, kind)
   THEN
      RETURN( TRUE )
(*
   see M2Quads for the fixme comment at assignment.

   PIM2 says that CARDINAL and INTEGER are compatible with subranges of CARDINAL and INTEGER,
        however we do not know the type to our subranges yet (GetType(SubrangeType)=NulSym).
        An oversight which needs to be fixed...
*)
   ELSIF ((t1=Integer) AND (t2=LongInt)) OR
         ((t2=Integer) AND (t1=LongInt))
   THEN
      RETURN( TRUE )
   ELSIF IsRealType(t1) AND IsRealType(t2)
   THEN
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsCompatible ;


(*
   IsAssignmentCompatible - returns TRUE if t1 and t2 are assignment
                            compatible.
*)

PROCEDURE IsAssignmentCompatible (t1, t2: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (t1=t2) OR IsCompatible(t1, t2, assignment) )
END IsAssignmentCompatible ;


(*
   IsExpressionCompatible - returns TRUE if t1 and t2 are expression
                            compatible.
*)

PROCEDURE IsExpressionCompatible (t1, t2: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsCompatible(t1, t2, expression) )
END IsExpressionCompatible ;


(*
   MixTypes - returns the type symbol that corresponds to the types t1 and t2.
              NearTok is used to identify the source position if a type
              incompatability occurs.
*)

PROCEDURE MixTypes (t1, t2: CARDINAL; NearTok: CARDINAL) : CARDINAL ;
BEGIN
   IF t1=t2
   THEN
      RETURN( t1 )
   ELSIF (t1=Cardinal) AND (t2=Integer)
   THEN
      RETURN( Integer )
   ELSIF (t1=Integer) AND (t2=Cardinal)
   THEN
      RETURN( Integer )
   ELSIF (t1=Address) AND (t2=Cardinal)
   THEN
      RETURN( Address )
   ELSIF (t1=Cardinal) AND (t2=Address)
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
   ELSIF ((t1=Integer) AND (t2=LongInt)) OR
         ((t2=Integer) AND (t1=LongInt))
   THEN
      RETURN( LongInt )
   ELSIF ((t1=Real) AND (t2=LongReal)) OR
         ((t2=Real) AND (t1=LongReal))
   THEN
      RETURN( LongReal )
   ELSIF ((t1=Real) AND (t2=ShortReal)) OR
         ((t2=Real) AND (t1=ShortReal))
   THEN
      RETURN( Real )
   ELSIF ((t1=ShortReal) AND (t2=LongReal)) OR
         ((t2=ShortReal) AND (t1=LongReal))
   THEN
      RETURN( LongReal )
   ELSIF (t1=GetLowestType(t1)) AND (t2=GetLowestType(t2))
   THEN
      ErrorFormat2(NewError(NearTok),
                   'type incompatibility between (%a) and (%a)', GetSymName(t1), GetSymName(t2)) ;
      FlushErrors  (* unrecoverable at present *)
   ELSE
      t1 := GetLowestType(t1) ;
      t2 := GetLowestType(t2) ;
      RETURN( MixTypes(t1, t2, NearTok) )
   END
END MixTypes ;


(*
   IsMathType - returns TRUE if the type is a mathematical type.
                A mathematical type has a range larger than INTEGER.
                (Typically SHORTREAL/REAL/LONGREAL/LONGINT)
*)

PROCEDURE IsMathType (type: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (type=LongInt) OR (type=Real) OR (type=LongReal) OR (type=ShortReal) )
END IsMathType ;


(*
     The rules are:
   
     assignment compatible matrix
                                             t2
   
                    NulSym  Word  Byte  Address Char Integer Cardinal Ptr  Enum Real Set
                  +---------------------------------------------------------------------
           NulSym | T       T     T     T       T    T       T        T    T    T   T
           Word   |         T     F     T       F    T       T        T    T    F   T
           Byte   |               T     F       T    F       F        F    F    F   F
   t1   Address   |                     T       F    F       F        T    F    F   F
           Char   |                             T    F       F        F    F    F   F
        Integer   |                                  T       T        F    F    F   F
       Cardinal   |                                          T        F    F    F   F
            Ptr   |                                                   T    F    F   F
           Enum   |                                                        T    F   F
           Real   |                                                             T   F
            Set   |                                                                 T
   
   
     expression compatible matrix
                                             t2
   
                    NulSym  Word  Byte  Address Char Integer Cardinal Ptr  Enum Real Set
                  +--------------------------------------------------------------------
           NulSym | T       T     T     T       T    T       T        T    T    T    T
           Word   |         T     F     F       F    F       F        F    F    F    F
           Byte   |               T     F       F    F       F        F    F    F    F
   t1   Address   |                     T       F    F       F        T    F    F    F
           Char   |                             T    F       F        F    F    F    F
        Integer   |                                  T       T        F    F    F    F
       Cardinal   |                                          T        F    F    F    F
            Ptr   |                                                   T    F    F    F
           Enum   |                                                        T    F    F
           Real   |                                                             T    F
            Set   |                                                                  T
*)

(*
   InitCompatibilityMatrices - initializes the tables above.
*)

PROCEDURE InitCompatibilityMatrices ;
VAR
   i, j: MetaType ;
BEGIN
   FOR i := MIN(MetaType) TO MAX(MetaType) DO
      FOR j := MIN(MetaType) TO MAX(MetaType) DO
         Ass[i, j]  := FALSE ;
         Expr[i, j] := FALSE
      END
   END ;
   FOR i := MIN(MetaType) TO MAX(MetaType) DO
      Ass[i, i] := TRUE ;      (* identity *)
      Ass[const, i] := TRUE ;  (* all const are compatible *)
      Ass[i, const] := TRUE ;
   END ;
   Ass[word, address] := TRUE ; Ass[address, word] := TRUE ;
   Ass[word, intgr] := TRUE ; Ass[intgr, word] := TRUE ;
   Ass[word, cardinal] := TRUE ; Ass[cardinal, word] := TRUE ;
   Ass[word, pointer] := TRUE ; Ass[pointer, word] := TRUE ;
   Ass[byte, chr] := TRUE ; Ass[chr, byte] := TRUE ;
   Ass[address, pointer] := TRUE ; Ass[pointer, address] := TRUE ;
   Ass[intgr, cardinal] := TRUE ; Ass[cardinal, intgr] := TRUE ;
   Ass[word, enum] := TRUE ; Ass[enum, word] := TRUE ;
   Ass[word, set] := TRUE ; Ass[set, word] := TRUE ;

   (* expression matrix *)
   FOR i := MIN(MetaType) TO MAX(MetaType) DO
      Expr[i, i] := TRUE ;      (* identity *)
      Expr[const, i] := TRUE ;  (* all const are compatible *)
      Expr[i, const] := TRUE ;
   END ;

   Expr[address, pointer] := TRUE ; Expr[pointer, address] := TRUE ;
   Expr[intgr, cardinal] := TRUE ; Expr[cardinal, intgr] := TRUE
   
END InitCompatibilityMatrices ;


END M2Base.
