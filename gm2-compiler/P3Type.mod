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
IMPLEMENTATION MODULE P3Type ;


FROM M2Debug IMPORT Assert ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NameKey IMPORT WriteKey ;

FROM M2Lexical IMPORT TokenIs, WriteError ;

FROM P3Expression IMPORT ConstExpression ;
IMPORT P2Expression ;

FROM M2Atom IMPORT Ident, PushAutoOff, PopAuto, Found ;

FROM M2Reserved IMPORT PeriodPeriodTok,
                       LSBraTok, RSBraTok, LParaTok, RParaTok,
                       CommaTok, BarTok, SemiColonTok, ColonTok,
                       ArrayTok, OfTok, EndTok, SetTok,
                       PointerTok, ToTok, ProcedureTok,
                       CaseTok, ElseTok, RecordTok,
                       VarTok,
                       NulTok ;

FROM M2Reference IMPORT Qualident, IdentList ;

FROM P3SymBuild IMPORT BuildSubrange ;

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


PROCEDURE Type() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
   NewPtr : CARDINAL ;
   OldPtr : CARDINAL ;
BEGIN
   GetPtr(OldPtr) ;
   PushAutoOff ;
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
   PopAuto ;
   GetPtr(NewPtr) ;
   Assert(OldPtr=NewPtr) ;
   RETURN( Success )
END Type ;

PROCEDURE SimpleType() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF Qualident()
   THEN
      Success := TRUE
   ELSIF Enumeration()
   THEN
      Success := TRUE
   ELSIF SubrangeType()
   THEN
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END SimpleType ;

PROCEDURE Enumeration() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(LParaTok)
   THEN
      IF IdentList()
      THEN
         IF TokenIs(RParaTok)
         THEN
            (* BuildEnumeration ;   (* ********** *) *)
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError(') - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('IdentList - expected')
      END
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
                  BuildSubrange ;  (* ******** *)
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
      (* StartBuildArray ;  (* ******** *) *)
      (* BuildNulName ;  (* ******** *) *)
      IF SimpleType()
      THEN
         (* BuildFieldArray ;  (* ******** *) *)
         WHILE TokenIs(CommaTok) DO
            (* BuildNulName ;  (* ******** *) *)
            IF SimpleType()
            THEN
               (* BuildFieldArray  (* ******** *) *)
            ELSE
               WriteError('SimpleType - expected')
            END
         END ;
         IF TokenIs(OfTok)
         THEN
            (* BuildNulName ;  (* ******** *) *)
            IF Type()
            THEN
               (* EndBuildArray ;  (* ******** *) *)
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
      (* BuildRecord ;  (* ******** SymbolTable *) *)
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
         (* BuildNulName ;  (* ******** *) *)
         IF Type()
         THEN
            (* BuildFieldRecord  (* ******** Symbol Table *) *)
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
   IF P2Expression.ConstExpression()
   THEN
      IF TokenIs(PeriodPeriodTok)
      THEN
         IF P2Expression.ConstExpression()
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
         (* BuildNulName ;  (* ******** *) *)
         IF SimpleType()
         THEN
            (* BuildSetType ; *)
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
         (* BuildNulName ;  (* ******** *) *)
         IF Type()
         THEN
            (* BuildPointerType ;  (* ******** Build Symbol Table *) *)
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


(*
   ProcedureType -
                   The Stack:


                   Entry                       Exit

                                                                <- Ptr
                                               +--------------+
            Ptr ->                             | ProcSym      |
                   +-------------+             |--------------|
                   | Name        |             | Name         |
                   |-------------|             |--------------|
*)

PROCEDURE ProcedureType() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(ProcedureTok)
   THEN
      (* BuildProcedureType ; *)
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

END P3Type.
