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
IMPLEMENTATION MODULE P1Type ;


FROM M2Debug IMPORT Assert ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NameKey IMPORT WriteKey ;

FROM M2Lexical IMPORT TokenIs, WriteError ;

FROM P1Expression IMPORT ConstExpression ;

FROM M2Atom IMPORT Ident, PushAutoOn, PushAutoOff, PopAuto, Found ;

FROM M2Reserved IMPORT PeriodPeriodTok,
                       LSBraTok, RSBraTok, LParaTok, RParaTok,
                       CommaTok, BarTok, SemiColonTok, ColonTok,
                       ArrayTok, OfTok, EndTok, SetTok,
                       PointerTok, ToTok, ProcedureTok,
                       CaseTok, ElseTok, RecordTok,
                       VarTok,
                       NulTok ;

FROM M2Reference IMPORT Qualident, IdentList ;

FROM P1SymBuild IMPORT BuildEnumeration, BuildType,
                       BuildNulName ;

FROM M2Quads IMPORT PushT, PopT, GetPtr ;


(* %%%FORWARD%%%
PROCEDURE ProcedureParameter () : BOOLEAN ; FORWARD ;
PROCEDURE FormalReturn () : BOOLEAN ; FORWARD ;
PROCEDURE ArrayType() : BOOLEAN ; FORWARD ;
PROCEDURE CaseLabelList() : BOOLEAN ; FORWARD ;
PROCEDURE CaseLabels() : BOOLEAN ; FORWARD ;
PROCEDURE Enumeration() : BOOLEAN ; FORWARD ;
PROCEDURE FieldList() : BOOLEAN ; FORWARD ;
PROCEDURE FieldListSequence() : BOOLEAN ; FORWARD ;
PROCEDURE FormalType() : BOOLEAN ; FORWARD ;
PROCEDURE FormalTypeList() : BOOLEAN ; FORWARD ;
PROCEDURE PointerType() : BOOLEAN ; FORWARD ;
PROCEDURE ProcedureParameters() : BOOLEAN ; FORWARD ;
PROCEDURE ProcedureType() : BOOLEAN ; FORWARD ;
PROCEDURE RecordType() : BOOLEAN ; FORWARD ;
PROCEDURE SetType() : BOOLEAN ; FORWARD ;
PROCEDURE SimpleType() : BOOLEAN ; FORWARD ;
PROCEDURE SubrangeType() : BOOLEAN ; FORWARD ;
PROCEDURE Varient() : BOOLEAN ; FORWARD ;
   %%%FORWARD%%% *)


(*
   Type - Parses a TYPE declaration during Pass1.
          In Pass 1 the Name of the type is passed on the stack,
          on returning the Name will be removed.
          The type name is removed since nothing is done with types during
          Pass 1. In Pass 2 a type is declared and placed into the parent
          scope, alternatively, if a variable is being declared,
          it is assigned to a variable and thus the name of the type is
          needed.
          However in Pass 1 only enumeration types are built,
          enumeration types are stored into the FifoQueue module waiting for
          Pass 2 to fully declare the type.
          Pass 1 does place the enumeration types into the main scope list.

          The Stack is expected:

          Entry                 Exit

   Ptr ->
          +------------+
          | Name       |        Empty
          |------------|

*)

PROCEDURE Type () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
   Name   : CARDINAL ;
   Ptr,
   PtrC   : CARDINAL ;
BEGIN
   GetPtr(Ptr) ;  (* Debugging aid *)
   PopT(Name) ;
   (* WriteString('Potential Type Name is ') ; WriteKey(Name) ; WriteLn ; *)
   PushT(Name) ;
   PushAutoOff ;  (* ******** *)
   IF SimpleType()
   THEN
      Success := TRUE
   ELSIF ArrayType()
   THEN
      Success := TRUE
   ELSIF RecordType()
   THEN
      Success := TRUE
   ELSIF SetType()
   THEN
      Success := TRUE
   ELSIF PointerType()
   THEN
      Success := TRUE
   ELSIF ProcedureType()
   THEN
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   PopAuto ;  (* ******** *)
   GetPtr(PtrC) ;
   Assert(Ptr=PtrC) ;  (* Debugging aid - check stack ptr is correct *)
   PopT(Name) ;  (* Remove TYPE name from the stack *)
   RETURN( Success )
END Type ;

PROCEDURE SimpleType() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
   Ptr, PtrC: CARDINAL ;
BEGIN
   GetPtr(Ptr) ;
   IF Qualident()
   THEN
      Success := TRUE
   ELSIF Enumeration()
   THEN
      BuildType ;  (* ******** *)
      Success := TRUE
   ELSIF SubrangeType()
   THEN
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   GetPtr(PtrC) ;
   Assert(Ptr=PtrC) ;
   RETURN( Success )
END SimpleType ;

PROCEDURE Enumeration() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(LParaTok)
   THEN
      PushAutoOn ;  (* ******** *)
      IF IdentList()
      THEN
         IF TokenIs(RParaTok)
         THEN
            BuildEnumeration ;   (* ********** *)
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError(') - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('IdentList - expected')
      END ;
      PopAuto  (* ******** *)
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Enumeration ;

PROCEDURE SubrangeType() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(LSBraTok)
   THEN
      IF ConstExpression()
      THEN
         IF TokenIs(PeriodPeriodTok)
         THEN
            IF ConstExpression()
            THEN
               IF TokenIs(RSBraTok)
               THEN
                  Success := TRUE
               ELSE
                  Success := FALSE ;
                  WriteError('] - expected')
               END
            ELSE
               Success := FALSE ;
               WriteError('ConstExpression - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError('.. - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('ConstExpression - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END SubrangeType ;

PROCEDURE ArrayType() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(ArrayTok)
   THEN
      IF SimpleType()
      THEN
         WHILE TokenIs(CommaTok) DO
            IF SimpleType()
            THEN
            ELSE
               WriteError('SimpleType - expected')
            END
         END ;
         IF TokenIs(OfTok)
         THEN
            BuildNulName ;  (* ********** *)
            IF Type()
            THEN
               Success := TRUE
            ELSE
               Success := FALSE ;
               WriteError('Type - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError('OF - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('SimpleType - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ArrayType ;

PROCEDURE RecordType() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(RecordTok)
   THEN
      IF FieldListSequence()
      THEN
         IF TokenIs(EndTok)
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('END - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('FieldListSequence - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END RecordType ;

PROCEDURE FieldListSequence() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF FieldList()
   THEN
      WHILE TokenIs(SemiColonTok) DO
         IF FieldList()
         THEN
         ELSE
            WriteError('FieldList - expected')
         END
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END FieldListSequence ;

PROCEDURE FieldList() : BOOLEAN ;
BEGIN
   IF IdentList()
   THEN
      IF TokenIs(ColonTok)
      THEN
         BuildNulName ;  (* ********** *)
         IF Type()
         THEN
         ELSE
            WriteError('Type - expected')
         END
      ELSE
         WriteError(': - expected')
      END
   ELSE
      (* Massage the ambigious grammer *)
      IF TokenIs(CaseTok)
      THEN
         IF Found(ColonTok)
         THEN
            IF Ident()
            THEN
               IF TokenIs(ColonTok)
               THEN
                  IF Qualident()
                  THEN
                  ELSE
                     WriteError('Qualident - expected')
                  END
               ELSE
                  WriteError(': - expected')
               END
            ELSE
               WriteError('Ident - expected')
            END
         ELSE
            IF Qualident()
            THEN
            ELSE
               WriteError('Qualident - expected')
            END
         END ;
         IF TokenIs(OfTok)
         THEN
            IF Varient()
            THEN
               WHILE TokenIs(BarTok) DO
                  IF Varient()
                  THEN
                  ELSE
                     WriteError('Varient - expected')
                  END
               END ;
               IF TokenIs(ElseTok)
               THEN
                  IF FieldListSequence()
                  THEN
                  ELSE
                     WriteError('FieldListSequence - expected')
                  END
               END ;
               IF TokenIs(EndTok)
               THEN
               ELSE
                  WriteError('END - expected')
               END
            ELSE
               WriteError('Varient - expected')
            END
         ELSE
            WriteError('OF - expected')
         END
      END
   END ;
   RETURN( TRUE )
END FieldList ;

PROCEDURE Varient() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF CaseLabelList()
   THEN
      IF TokenIs(ColonTok)
      THEN
         IF FieldListSequence()
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('FieldListSequence - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError(': - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Varient ;

PROCEDURE CaseLabelList() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF CaseLabels()
   THEN
      WHILE TokenIs(CommaTok) DO
         IF CaseLabels()
         THEN
         ELSE
            WriteError('CaseLabels - expected')
         END
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END CaseLabelList ;

PROCEDURE CaseLabels() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF ConstExpression()
   THEN
      IF TokenIs(PeriodPeriodTok)
      THEN
         IF ConstExpression()
         THEN
         ELSE
            WriteError('ConstExpression - expected')
         END
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END CaseLabels ;

PROCEDURE SetType() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(SetTok)
   THEN
      IF TokenIs(OfTok)
      THEN
         IF SimpleType()
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('SimpleType - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('OF - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END SetType ;

PROCEDURE PointerType() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(PointerTok)
   THEN
      IF TokenIs(ToTok)
      THEN
         BuildNulName ;  (* ********** *)
         IF Type()
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('Type - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('TO - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END PointerType ;

PROCEDURE ProcedureType() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(ProcedureTok)
   THEN
      IF FormalTypeList()
      THEN
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ProcedureType ;

(*
   FormalTypeList := " ( " ( " ) " FormalReturn ) | 
                     ( ProcedureParameters " ) " FormalReturn ) 
*)

PROCEDURE FormalTypeList () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(LParaTok)
   THEN
      IF TokenIs(RParaTok)
      THEN
         IF FormalReturn()
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('FormalReturn - expected')
         END
      ELSIF ProcedureParameters()
      THEN
         IF TokenIs(RParaTok)
         THEN
            IF FormalReturn()
            THEN
               Success := TRUE
            ELSE
               Success := FALSE ;
               WriteError('FormalReturn - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError(') - expected')
         END
      ELSE
         Success := FALSE
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END FormalTypeList ;


(*
   FormalReturn := [ " : " Qualident ] 
*)

PROCEDURE FormalReturn () : BOOLEAN ;
BEGIN
   IF TokenIs(ColonTok)
   THEN
      IF Qualident()
      THEN
      ELSE
         WriteError('Qualident - expected')
      END
   END ;
   RETURN( TRUE )
END FormalReturn ;


(*
   ProcedureParameters := ProcedureParameter 
                          { " , " ProcedureParameter } 
*)

PROCEDURE ProcedureParameters () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF ProcedureParameter()
   THEN
      WHILE TokenIs(CommaTok) DO
         IF ProcedureParameter()
         THEN
         ELSE
            WriteError('ProcedureParameter - expected')
         END
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ProcedureParameters ;


(*
   ProcedureParameter := ( " VAR " FormalType ) | FormalType 
*)

PROCEDURE ProcedureParameter () : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(VarTok)
   THEN
      IF FormalType()
      THEN
         Success := TRUE
      ELSE
         Success := FALSE ;
         WriteError('FormalType - expected')
      END
   ELSIF FormalType()
   THEN
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ProcedureParameter ;

PROCEDURE FormalType() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(ArrayTok)
   THEN
      IF TokenIs(OfTok)
      THEN
         IF Qualident()
         THEN
            Success := TRUE
         ELSE
            Success := FALSE
         END
      ELSE
         Success := FALSE ;
         WriteError('OF - expected')
      END
   ELSE
      IF Qualident()
      THEN
         Success := TRUE
      ELSE
         Success := FALSE ;
         WriteError('Qualident - expected')
      END
   END ;
   RETURN( Success )
END FormalType ;


END P1Type.
