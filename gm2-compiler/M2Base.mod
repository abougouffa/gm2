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
    Last edit  : Mon Jul 10 20:16:54 2000
    Description: gcc version of M2Base. This module initializes the front end
                 symbol table with the base types. We collect the size of the
                 base types and range of values from the gcc backend.
*)

FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM NameKey IMPORT MakeKey, WriteKey ;

FROM M2Lexical IMPORT WriteError, WriteErrorFormat1, WriteErrorFormat2,
                      NearToken, LastError ;

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
                        GetType, GetLowestType, GetDeclared,
                        SetCurrentModule,
                        StartScope, EndScope, PseudoScope,
                        RequestSym, GetSymName, NulSym,
                        PutImported, GetExported,
                        PopSize, PopValue ;

FROM M2ALU IMPORT PushIntegerTree, PushCard ;
FROM M2Batch IMPORT MakeDefinitionSource ;
FROM M2Options IMPORT BoundsChecking, ReturnChecking ;
FROM M2System IMPORT Address, Bitset, InitSystem ;
FROM M2Math IMPORT InitMath ;

FROM gccgm2 IMPORT GetSizeOf, GetIntegerType, GetCharType, GetMaxFrom, GetMinFrom,
                   GetRealType, GetLongIntType, GetLongRealType, GetProcType ;

(* %%%FORWARD%%%
PROCEDURE InitBaseConstants ; FORWARD ;
PROCEDURE InitBaseSimpleTypes ; FORWARD ;
PROCEDURE InitBaseFunctions ; FORWARD ;
PROCEDURE InitBaseProcedures ; FORWARD ;
   %%%FORWARD%%% *)


VAR
   MinChar,
   MaxChar,
   MinInteger,
   MaxInteger: CARDINAL ;


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

   InitBaseConstants ;
   InitBaseFunctions ;
   InitBaseProcedures ;

   InitMath ;
   (*
      Note: that we do end the Scope since we keep the symbol to the head
            of the base scope. This head of base scope is searched
            when all other scopes fail to deliver a symbol.
   *)
   EndScope ;
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
   PushIntegerTree(GetMaxFrom(GetIntegerType())) ;
   PopValue(MaxCard) ;

   Cardinal := MakeSubrange(MakeKey('CARDINAL')) ;
   (*
      Cardinal = 0..MaxInt   (* subrange of INTEGER *)
   *)
   PutSubrange(Cardinal, Zero, MaxCard, Integer) ;
                                              (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetIntegerType())) ;
   PopSize(Cardinal) ;

   LongInt := MakeType(MakeKey('LONGINT')) ;
   PutType(LongInt, NulSym) ;                 (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetLongIntType())) ;
   PopSize(LongInt) ;

   Real := MakeType(MakeKey('REAL')) ;
   PutType(Real, NulSym) ;                    (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetRealType())) ;
   PopSize(Real) ;

   LongReal := MakeType(MakeKey('LONGREAL')) ;
   PutType(LongReal, NulSym) ;                (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetLongRealType())) ;
   PopSize(LongReal) ;

   Char := MakeType(MakeKey('CHAR')) ;
   PutType(Char, NulSym) ;                    (* Base Type       *)
   PushIntegerTree(GetSizeOf(GetCharType())) ;
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
   PushIntegerTree(GetMinFrom(GetCharType())) ;
   PopValue(MinChar) ;

   (* MaxChar *)
   MaxChar := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMaxFrom(GetCharType())) ;
   PopValue(MaxChar) ;

   (* MinInteger *)
   MinInteger := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMinFrom(GetIntegerType())) ;
   PopValue(MinInteger) ;

   (* MaxInteger *)
   MaxInteger := MakeTemporary(ImmediateValue) ;
   PushIntegerTree(GetMaxFrom(GetIntegerType())) ;
   PopValue(MaxInteger) ;

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
   ELSIF type=Char
   THEN
      min := MinChar ;
      max := MaxChar
   ELSE
      WriteErrorFormat1('unable to find MIN or MAX for the base type %s', GetSymName(type))
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
   ArrayAddress := MakeKey('_ArrayAddress') ;
   ArrayHigh := MakeKey('_ArrayHigh') ;
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
   Convert := MakeProcedure(MakeKey('CONVERT')) ;  (* Pseudo Base function CONVERT *)
   Val     := MakeProcedure(MakeKey('VAL')) ;      (* Pseudo Base function VAL     *)
   Ord     := MakeProcedure(MakeKey('ORD')) ;      (* Pseudo Base function ORD     *)
   Chr     := MakeProcedure(MakeKey('CHR')) ;      (* Pseudo Base function CHR     *)
   Float   := MakeProcedure(MakeKey('FLOAT')) ;    (* Pseudo Base function FLOAT   *)
   Trunc   := MakeProcedure(MakeKey('TRUNC')) ;    (* Pseudo Base function TRUNC   *)
   Min     := MakeProcedure(MakeKey('MIN')) ;      (* Pseudo Base function MIN     *)
   Max     := MakeProcedure(MakeKey('MAX'))        (* Pseudo Base function MIN     *)
END InitBaseFunctions ;


(*
   IsPseudoBaseFunction - returns true if Sym is a Base pseudo function.
*)

PROCEDURE IsPseudoBaseFunction (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (Sym=High) OR (Sym=Val) OR (Sym=Convert) OR (Sym=Ord) OR
          (Sym=Chr) OR (Sym=Float) OR (Sym=Trunc) OR (Sym=Min) OR
          (Sym=Max)
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
          (Sym=Real)     OR (Sym=LongReal)
         )
END IsBaseType ;


(*
   CheckCompatibleTypes - returns if t1 and t2 are compatible types for
                          +, -, *, DIV, >, <, =, etc.
                          If t1 and t2 are not compatible then an error
                          message is displayed.
*)

PROCEDURE CheckCompatibleTypes (t1, t2: CARDINAL) ;
BEGIN
   IF NOT IsCompatible(t1, t2)
   THEN
      WriteString('the two types are: ') ; WriteKey(GetSymName(t1)) ;
      WriteString(' and ') ; WriteKey(GetSymName(t2)) ; WriteLn ;
      IF IsUnknown(t1)
      THEN
         WriteKey(GetSymName(t1)) ;
         WriteString(' (is actually unknown and perhaps should be declared or imported)') ;
         NearToken('', GetDeclared(t1))
      END ;

      IF IsUnknown(t2)
      THEN
         WriteKey(GetSymName(t2)) ;
         WriteString(' (is actually unknown and perhaps should be declared or imported)') ;
         NearToken('', GetDeclared(t2))
      END ;
      WriteLn ;
      IF IsUnknown(t1) AND IsUnknown(t2)
      THEN
         LastError('two different unknown types must be resolved (declared or imported)')
      ELSIF IsUnknown(t1) OR IsUnknown(t2)
      THEN
         LastError('a type must be declared or imported')
      ELSE
         WriteError('type incompatability - types should be converted or coersed')
      END
   END
END CheckCompatibleTypes ;


(*
   IsBaseCompatible - returns TRUE if the simple base type comparison is legal.
*)

PROCEDURE IsBaseCompatible (t1, t2: CARDINAL) : BOOLEAN ;
BEGIN
   IF (t1=NulSym) OR (t2=NulSym)
   THEN
      (*
         NulSym type indicates a CONSTANT which could be one of many types.
         eg. BitSet, Integer, Cardinal.
      *)
      RETURN( TRUE )
   ELSIF t1=t2
   THEN
      (* compatible because same type *)
      RETURN( TRUE )
   ELSIF (t1=Address) AND (t2#NulSym) AND (IsPointer(t2))
   THEN
      RETURN( TRUE )
   ELSIF (t2=Address) AND (t1#NulSym) AND (IsPointer(t1))
   THEN
      RETURN( TRUE )
   ELSIF ((t1=Integer) AND (t2=Cardinal)) OR
         ((t2=Integer) AND (t1=Cardinal))
   THEN
      RETURN( TRUE )   (* INTEGER and CARDINAL are compatible Wirth PIM2 P152 2nd Edition *)
   ELSE
      RETURN( FALSE )
   END
END IsBaseCompatible ;


(*
   IsCompatible - returns true if the types, t1, and, t2, are compatible.
*)

PROCEDURE IsCompatible (t1, t2: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsBaseCompatible(t1, t2)
   THEN
      RETURN( TRUE )
(*
   see M2Quads for the fixme comment at assignment.

   PIM2 says that CARDINAL and INTEGER are compatible with subranges of CARDINAL and INTEGER,
        however we do not know the type to our subranges yet (GetType(SubrangeType)=NulSym).
        An oversight which needs to be fixed...
*)
   ELSIF IsSubrange(t1)
   THEN
      RETURN( IsCompatible(GetType(t1), t2) )
   ELSIF IsSubrange(t2)
   THEN
      RETURN( IsCompatible(t1, GetType(t2)) )
   ELSIF ((t1=Integer) AND (t2=LongInt)) OR
         ((t2=Integer) AND (t1=LongInt))
   THEN
      RETURN( TRUE )
   ELSIF ((t1=Real) AND (t2=LongReal)) OR
         ((t2=Real) AND (t1=LongReal))
   THEN
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsCompatible ;


(*
   MixTypes - returns the type symbol that corresponds to the types t1 and t2.
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
      RETURN( MixTypes(Integer, t2) )
   ELSIF IsEnumeration(t2)
   THEN
      RETURN( MixTypes(t1, Integer) )
   ELSIF IsSubrange(t1)
   THEN
      RETURN( MixTypes(GetType(t1), t2) )
   ELSIF IsSubrange(t2)
   THEN
      RETURN( MixTypes(t1, GetType(t2)) )
   ELSIF ((t1=Integer) AND (t2=LongInt)) OR
         ((t2=Integer) AND (t1=LongInt))
   THEN
      RETURN( LongInt )
   ELSIF ((t1=Real) AND (t2=LongReal)) OR
         ((t2=Real) AND (t1=LongReal))
   THEN
      RETURN( LongReal )
   ELSIF (t1=GetLowestType(t1)) AND (t2=GetLowestType(t2))
   THEN
      ErrorFormat2(NewError(NearTok),
                   'type incompatibility between (%a) and (%a)', GetSymName(t1), GetSymName(t2))
   ELSE
      t1 := GetLowestType(t1) ;
      t2 := GetLowestType(t2) ;
      RETURN( MixTypes(t1, t2) )
   END
END MixTypes ;


(*
   IsMathType - returns TRUE if the type is a mathematical type.
                A mathematical type has a range larger than INTEGER.
                (Typically REAL/LONGREAL/LONGINT)
*)

PROCEDURE IsMathType (type: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (type=LongInt) OR (type=Real) OR (type=LongReal) )
END IsMathType ;


END M2Base.
