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
IMPLEMENTATION MODULE P2Expression ;


FROM M2Lexical IMPORT TokenIs,
                      WriteError ;

FROM M2Atom IMPORT String, Number, Ident, Found, PushAutoOff, PopAuto ;

FROM M2Reference IMPORT Qualident, Designator ;

FROM M2Reserved IMPORT EqualTok, GreaterTok, LessTok,
                       GreaterEqualTok, LessEqualTok, LessGreaterTok,
                       HashTok, AmbersandTok, InTok,
                       PlusTok, MinusTok, TimesTok, DivideTok,
                       LParaTok, RParaTok, LCBraTok, RCBraTok,
                       CommaTok, PeriodPeriodTok, PeriodTok,
                       LSBraTok, RSBraTok, UpArrowTok,
                       AndTok, NotTok, OrTok, ModTok, DivTok ;
(*
FROM M2Quads IMPORT PushT,
                    BuildFunctionCall,
                    BuildBinaryOp, BuildUnaryOp, BuildRelOp, BuildNot,
                    RecordOp ;
*)
FROM M2Debug IMPORT WriteDebug ;


(* %%%FORWARD%%%
PROCEDURE AddOperator() : BOOLEAN ; FORWARD ;
PROCEDURE BinUnConstTerm() : BOOLEAN ; FORWARD ;
PROCEDURE BinUnTerm() : BOOLEAN ; FORWARD ;
PROCEDURE ConstFactor() : BOOLEAN ; FORWARD ;
PROCEDURE ConstTerm() : BOOLEAN ; FORWARD ;
PROCEDURE Element() : BOOLEAN ; FORWARD ;
PROCEDURE Factor() : BOOLEAN ; FORWARD ;
PROCEDURE MulOperator() : BOOLEAN ; FORWARD ;
PROCEDURE Relation() : BOOLEAN ; FORWARD ;
PROCEDURE Set() : BOOLEAN ; FORWARD ;
PROCEDURE SimpleConstExpr() : BOOLEAN ; FORWARD ;
PROCEDURE SimpleExpression() : BOOLEAN ; FORWARD ;
PROCEDURE Term() : BOOLEAN ; FORWARD ;
   %%%FORWARD%%% *)


PROCEDURE ConstExpression() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   PushAutoOff ;
   IF SimpleConstExpr()
   THEN
      IF Relation()
      THEN
         IF SimpleConstExpr()
         THEN
            (* BuildRelOp  (* ******** *) *)
         ELSE
            WriteError('SimpleConstExpr - expected')
         END
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   PopAuto ;
   RETURN( Success )
END ConstExpression ;

PROCEDURE Relation() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(EqualTok)
   THEN
      (* PushT(EqualTok) ; *)
      Success := TRUE
   ELSIF TokenIs(HashTok)
   THEN
      (* PushT(HashTok) ; *)
      Success := TRUE
   ELSIF TokenIs(LessGreaterTok)
   THEN
      (* PushT(LessGreaterTok) ; *)
      Success := TRUE
   ELSIF TokenIs(LessTok)
   THEN
      (* PushT(LessTok) ; *)
      Success := TRUE
   ELSIF TokenIs(LessEqualTok)
   THEN
      (* PushT(LessEqualTok) ; *)
      Success := TRUE
   ELSIF TokenIs(GreaterTok)
   THEN
      (* PushT(GreaterTok) ; *)
      Success := TRUE
   ELSIF TokenIs(GreaterEqualTok)
   THEN
      (* PushT(GreaterEqualTok) ; *)
      Success := TRUE
   ELSIF TokenIs(InTok)
   THEN
      (* PushT(InTok) ; *)
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Relation ;

PROCEDURE BinUnConstTerm() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(PlusTok)
   THEN
      (* PushT(PlusTok) ;  (* ******** *) *)
      IF ConstTerm()
      THEN
         Success := TRUE ;
         (* BuildUnaryOp  (* ******** *) *)
      ELSE
         Success := FALSE ;
         WriteError('ConstTerm - expected')
      END
   ELSIF TokenIs(MinusTok)
   THEN
      (* PushT(MinusTok) ;  (* ******** *) *)
      IF ConstTerm()
      THEN
         Success := TRUE ;
         (* BuildUnaryOp  (* ******** *) *)
      ELSE
         Success := FALSE ;
         WriteError('ConstTerm - expected')
      END
   ELSIF ConstTerm()
   THEN
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END BinUnConstTerm ;

PROCEDURE SimpleConstExpr() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF BinUnConstTerm()
   THEN
      WHILE AddOperator() DO
         IF ConstTerm()
         THEN
            (* BuildBinaryOp  (* ******** *) *)
         ELSE
            WriteError('ConstTerm - expected')
         END
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END SimpleConstExpr ;


(*
   AddOperator - The Stack

                 Entry                            Exit

                                                                 <- Ptr
                 Empty                            +------------+
                                                  | Operator   |
                                                  |------------|
*)

PROCEDURE AddOperator() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(PlusTok)
   THEN
      (* PushT(PlusTok) ;     (* ******** *) *)
      (* RecordOp ;           (* ******** *) *)
      Success := TRUE
   ELSIF TokenIs(MinusTok)
   THEN
      (* PushT(MinusTok) ;    (* ******** *) *)
      (* RecordOp ;           (* ******** *) *)
      Success := TRUE
   ELSIF TokenIs(OrTok)
   THEN
      (* PushT(OrTok) ;       (* ******** *) *)
      (* RecordOp ;           (* ******** *) *)
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END AddOperator ;

PROCEDURE ConstTerm() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF ConstFactor()
   THEN
      WHILE MulOperator() DO
         IF ConstFactor()
         THEN
            (* BuildBinaryOp  (* ******** *) *)
         ELSE
            WriteError('ConstFactor - expected')
         END
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ConstTerm ;


(*
   MulOperator - The Stack

                 Entry                            Exit

                                                                 <- Ptr
                 Empty                            +------------+
                                                  | Operator   |
                                                  |------------|
*)

PROCEDURE MulOperator() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(TimesTok)
   THEN
      (* PushT(TimesTok) ;          (* ******** *) *)
      (* RecordOp ;                 (* ******** *) *)
      Success := TRUE
   ELSIF TokenIs(DivideTok)
   THEN
      (* PushT(DivideTok) ;         (* ******** *) *)
      (* RecordOp ;                 (* ******** *) *)
      Success := TRUE
   ELSIF TokenIs(DivTok)
   THEN
      (* PushT(DivTok) ;            (* ******** *) *)
      (* RecordOp ;                 (* ******** *) *)
      Success := TRUE
   ELSIF TokenIs(ModTok)
   THEN
      (* PushT(ModTok) ;            (* ******** *) *)
      (* RecordOp ;                 (* ******** *) *)
      Success := TRUE
   ELSIF TokenIs(AndTok)
   THEN
      (* PushT(AndTok) ;            (* ******** *) *)
      (* RecordOp ;                 (* ******** *) *)
      Success := TRUE
   ELSIF TokenIs(AmbersandTok)
   THEN
      (* PushT(AmbersandTok) ;      (* ******** *) *)
      (* RecordOp ;                 (* ******** *) *)
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END MulOperator ;

PROCEDURE ConstFactor() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF Qualident()
   THEN
      Success := TRUE
   ELSIF Number()
   THEN
      Success := TRUE
   ELSIF String()
   THEN
      Success := TRUE
   ELSIF Set()
   THEN
      Success := TRUE
   ELSIF TokenIs(LParaTok)
   THEN
      IF ConstExpression()
      THEN
         IF TokenIs(RParaTok)
         THEN
            Success := TRUE
         ELSIF TokenIs(NotTok)
         THEN
            IF ConstFactor()
            THEN
               (* BuildNot ;  (* ******** *) *)
               Success := TRUE
            ELSE
               Success := FALSE ;
               WriteError('ConstFactor - expected')
            END
         ELSE
            Success := FALSE ;
            WriteError(') - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('ConstExpression - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ConstFactor ;

PROCEDURE Set() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF Found(LCBraTok)
   THEN
      IF Qualident()
      THEN
      END ;
      IF TokenIs(LCBraTok)
      THEN
         IF Element()
         THEN
            WHILE TokenIs(CommaTok) DO
               IF Element()
               THEN
               ELSE
                  WriteError('Element - expected')
               END
            END
         END ;
         IF TokenIs(RCBraTok)
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError('} - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('{ - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Set ;

PROCEDURE Element() : BOOLEAN ;
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
END Element ;

PROCEDURE ExpList() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF Expression()
   THEN
      WHILE TokenIs(CommaTok) DO
         IF Expression()
         THEN
         ELSE
            WriteError('Expression - expected')
         END
      END ;
      Success := TRUE ;
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ExpList ;

PROCEDURE Expression() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   PushAutoOff ;
   IF SimpleExpression()
   THEN
      IF Relation()
      THEN
         IF SimpleExpression()
         THEN
            (* BuildRelOp (* ******** *) *)
         ELSE
            WriteError('SimpleExpression - expected')
         END
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   PopAuto ;
   RETURN( Success )
END Expression ;

PROCEDURE BinUnTerm() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(PlusTok)
   THEN
      (* PushT(PlusTok) ;  (* ******** *) *)
      IF Term()
      THEN
         Success := TRUE ;
         (* BuildUnaryOp  (* ******** *) *)
      ELSE
         Success := FALSE ;
         WriteError('Term - expected')
      END
   ELSIF TokenIs(MinusTok)
   THEN
      (* PushT(MinusTok) ;  (* ******** *) *)
      IF Term()
      THEN
         Success := TRUE ;
         (* BuildUnaryOp  (* ******** *) *)
      ELSE
         Success := FALSE ;
         WriteError('Term - expected')
      END
   ELSIF Term()
   THEN
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END BinUnTerm ;

PROCEDURE SimpleExpression() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF BinUnTerm()
   THEN
      WHILE AddOperator() DO
         IF Term()
         THEN
            (* BuildBinaryOp  (* ******** *) *)
         ELSE
            WriteError('Term - expected')
         END
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END SimpleExpression ;

PROCEDURE Term() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF Factor()
   THEN
      WHILE MulOperator() DO
         IF Factor()
         THEN
            (* BuildBinaryOp  (* ******** *) *)
         ELSE
            WriteError('Factor - expected')
         END
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Term ;

PROCEDURE Factor() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF Number()
   THEN
      Success := TRUE
   ELSIF String()
   THEN
      Success := TRUE
   ELSIF Set()
   THEN
      Success := TRUE
   ELSIF Designator()
   THEN
      IF ActualParameters()
      THEN
         (* BuildFunctionCall  (* ********** *) *)
      END ;
      Success := TRUE
   ELSIF TokenIs(LParaTok)
   THEN
      IF Expression()
      THEN
         IF TokenIs(RParaTok)
         THEN
            Success := TRUE
         ELSE
            Success := FALSE ;
            WriteError(') - expected')
         END
      ELSE
         Success := FALSE ;
         WriteError('Expression - expected')
      END
   ELSIF TokenIs(NotTok)
   THEN
      IF Factor()
      THEN
         (* BuildNot ;  (* ******** *) *)
         Success := TRUE
      ELSE
         Success := FALSE ;
         WriteError('Factor - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END Factor ;

PROCEDURE ActualParameters() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF TokenIs(LParaTok)
   THEN
      IF ExpList()
      THEN
      END ;
      IF TokenIs(RParaTok)
      THEN
         Success := TRUE
      ELSE
         Success := FALSE ;
         WriteError(') - expected')
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ActualParameters ;


END P2Expression.
