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

IMPLEMENTATION MODULE M2Inline ;

FROM SymbolTable IMPORT MakeGnuAsm, PutGnuAsmVolatile, PutGnuAsm, PutGnuAsmInput,
                        PutGnuAsmOutput, PutGnuAsmTrash, PutGnuAsmVolatile,
                        MakeRegInterface,
                        PutRegInterface, GetRegInterface,
                        GetSymName,
                        NulSym ;
FROM M2Atom IMPORT String, PushAutoOn, PopAuto ;
FROM M2Quads IMPORT BuildInline, PushT, PopT ;
FROM M2Lexical IMPORT InternalError, TokenIs, WriteError ;
FROM M2Reserved IMPORT AsmTok, ColonTok, RParaTok, LParaTok, CommaTok, VolatileTok ;
FROM M2Pass IMPORT IsPass1, IsPass2, IsPass3, IsPassHidden ;
FROM NameKey IMPORT WriteKey ;
FROM StrIO IMPORT WriteLn ;

IMPORT P1Expression ;
IMPORT P2Expression ;
IMPORT P3Expression ;

(* %%%FORWARD%%%
PROCEDURE AsmElement () : BOOLEAN ; FORWARD ;
PROCEDURE AsmOperands () : BOOLEAN ; FORWARD ;
PROCEDURE AsmList () : BOOLEAN ; FORWARD ;
PROCEDURE TrashList () : BOOLEAN ; FORWARD ;
   %%%FORWARD%%% *)

VAR
   CurrentAsm: CARDINAL ;


(*
   Expression - 
*)

PROCEDURE Expression () : BOOLEAN ;
BEGIN
   IF IsPass1()
   THEN
      RETURN( P1Expression.Expression() )
   ELSIF IsPass2() OR IsPassHidden()
   THEN
      RETURN( P2Expression.Expression() )
   ELSE
      RETURN( P3Expression.Expression() )
   END
END Expression ;


(*
   String := 
*)
(*
   Expression := 
*)
(*
   Ident := 
*)
(*
   AsmStatement := % VAR CurrentAsm: CARDINAL ;
                                          %
                   'ASM' % IF IsPass3()
                                           THEN
                                              PushAutoOn ;
                                              PushT(MakeGnuAsm())
                                           END
                                          %
                   [ 'VOLATILE' % IF IsPass3()
                                           THEN
                                              PopT(CurrentAsm) ;
                                              PutGnuAsmVolatile(CurrentAsm) ;
                                              PushT(CurrentAsm)
                                           END
                                          %
                      ] '(' AsmOperands % IF IsPass3()
                                           THEN
                                              BuildInline ;
                                              PopAuto
                                           END
                                          %
                   ')' 
*)

PROCEDURE AsmStatement () : BOOLEAN ;
VAR
    CurrentAsm: CARDINAL ;
                                         
BEGIN
   
   
   IF TokenIs(AsmTok)
   THEN
      IF IsPass3()
                                           THEN
                                              PushAutoOn ;
                                              PushT(MakeGnuAsm())
                                           END
                                          ;
      
      IF TokenIs(VolatileTok)
      THEN
         IF IsPass3()
                                           THEN
                                              PopT(CurrentAsm) ;
                                              PutGnuAsmVolatile(CurrentAsm) ;
                                              PushT(CurrentAsm)
                                           END
                                          ;
         
      END ;  (* if *)
      IF TokenIs(LParaTok)
      THEN
         IF AsmOperands()
         THEN
            IF IsPass3()
                                           THEN
                                              BuildInline ;
                                              PopAuto
                                           END
                                          ;
            
            IF TokenIs(RParaTok)
            THEN
               RETURN( TRUE )
            ELSE
               WriteError("')' - expected") ;
               RETURN( FALSE )
            END ;
         ELSE
            WriteError('AsmOperands - expected') ;
            RETURN( FALSE )
         END ;
      ELSE
         WriteError("'(' - expected") ;
         RETURN( FALSE )
      END ;
   ELSE
      RETURN( FALSE )
   END ;
END AsmStatement ;


(*
   AsmOperands := % VAR CurrentAsm, outputs, inputs, trash, string: CARDINAL ;
                                          %
                  String % IF IsPass3()
                                           THEN
                                              PopT(string) ;
                                              PopT(CurrentAsm) ;
                                              (* adds the name/instruction for this asm *)
                                              PutGnuAsm(CurrentAsm, string) ;
                                              PushT(CurrentAsm)
                                           END
                                          %
                  [ ':' AsmList % IF IsPass3()
                                           THEN
                                              PopT(outputs) ;
                                              PopT(CurrentAsm) ;
                                              PutGnuAsmOutput(CurrentAsm, outputs) ;
                                              PushT(CurrentAsm)
                                           END
                                          %
                    [ ':' AsmList % IF IsPass3()
                                           THEN
                                              PopT(inputs) ;
                                              PopT(CurrentAsm) ;
                                              PutGnuAsmInput(CurrentAsm, inputs) ;
                                              PushT(CurrentAsm)
                                           END
                                          %
                      [ ':' TrashList % IF IsPass3()
                                           THEN
                                              PopT(trash) ;
                                              PopT(CurrentAsm) ;
                                              PutGnuAsmTrash(CurrentAsm, trash) ;
                                              PushT(CurrentAsm)
                                           END
                                          %
                         ]  ]  ] 
*)

PROCEDURE AsmOperands () : BOOLEAN ;
VAR
    CurrentAsm, outputs, inputs, trash, string: CARDINAL ;
                                         
BEGIN
   
   
   IF String()
   THEN
      IF IsPass3()
                                           THEN
                                              PopT(string) ;
                                              PopT(CurrentAsm) ;
                                              (* adds the name/instruction for this asm *)
                                              PutGnuAsm(CurrentAsm, string) ;
                                              PushT(CurrentAsm)
                                           END
                                          ;
      
      IF TokenIs(ColonTok)
      THEN
         IF AsmList()
         THEN
            IF IsPass3()
                                           THEN
                                              PopT(outputs) ;
                                              PopT(CurrentAsm) ;
                                              PutGnuAsmOutput(CurrentAsm, outputs) ;
                                              PushT(CurrentAsm)
                                           END
                                          ;
            
            IF TokenIs(ColonTok)
            THEN
               IF AsmList()
               THEN
                  IF IsPass3()
                                           THEN
                                              PopT(inputs) ;
                                              PopT(CurrentAsm) ;
                                              PutGnuAsmInput(CurrentAsm, inputs) ;
                                              PushT(CurrentAsm)
                                           END
                                          ;
                  
                  IF TokenIs(ColonTok)
                  THEN
                     IF TrashList()
                     THEN
                        IF IsPass3()
                                           THEN
                                              PopT(trash) ;
                                              PopT(CurrentAsm) ;
                                              PutGnuAsmTrash(CurrentAsm, trash) ;
                                              PushT(CurrentAsm)
                                           END
                                          ;
                        
                     END ;
                  END ;  (* if *)
               END ;
            END ;  (* if *)
         END ;
      END ;  (* if *)
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END ;
END AsmOperands ;


(*
   AsmList := % IF IsPass3()
                                           THEN
                                              PushT(NulSym)
                                           END
                                          %
              [ AsmElement  ] { ',' AsmElement  } 
*)

PROCEDURE AsmList () : BOOLEAN ;
BEGIN
   IF IsPass3()
                                           THEN
                                              PushT(NulSym)
                                           END
                                          ;
   
   IF AsmElement()
   THEN
   END ;  (* if *)
   WHILE TokenIs(CommaTok) DO
      IF AsmElement()
      THEN
      ELSE
         WriteError('AsmElement - expected') ;
         RETURN( FALSE )
      END ;
   END ;  (* while *)
   RETURN( TRUE )
END AsmList ;


(*
   AsmElement := % VAR str, expr, CurrentInterface: CARDINAL ;
                                          %
                 String '(' Expression % IF IsPass3()
                                           THEN
                                              PopT(expr) ;
                                              PopT(str) ;
                                              PopT(CurrentInterface) ;
                                              IF CurrentInterface=NulSym
                                              THEN
                                                 CurrentInterface := MakeRegInterface()
                                              END ;
                                              PutRegInterface(CurrentInterface, str, expr) ;
                                              PushT(CurrentInterface)
                                           END
                                          %
                 ')' 
*)

PROCEDURE AsmElement () : BOOLEAN ;
VAR
    str, expr, CurrentInterface: CARDINAL ;
                                         
BEGIN
   
   
   IF String()
   THEN
      IF TokenIs(LParaTok)
      THEN
         IF Expression()
         THEN
            IF IsPass3()
                                           THEN
                                              PopT(expr) ;
                                              PopT(str) ;
                                              PopT(CurrentInterface) ;
                                              IF CurrentInterface=NulSym
                                              THEN
                                                 CurrentInterface := MakeRegInterface()
                                              END ;
                                              PutRegInterface(CurrentInterface, str, expr) ;
                                              PushT(CurrentInterface)
                                           END
                                          ;
            
            IF TokenIs(RParaTok)
            THEN
               RETURN( TRUE )
            ELSE
               WriteError("')' - expected") ;
               RETURN( FALSE )
            END ;
         ELSE
            WriteError('Expression - expected') ;
            RETURN( FALSE )
         END ;
      ELSE
         WriteError("'(' - expected") ;
         RETURN( FALSE )
      END ;
   ELSE
      RETURN( FALSE )
   END ;
END AsmElement ;


(*
   TrashList := % VAR CurrentInterface, str: CARDINAL ;
                                          %
                [ String % IF IsPass3()
                                           THEN
                                              PopT(str) ;
                                              CurrentInterface := MakeRegInterface() ;
                                              PutRegInterface(CurrentInterface, str, NulSym) ;
                                              PushT(CurrentInterface)
                                           END
                                          %
                   ] { ',' String % IF IsPass3()
                                           THEN
                                              PopT(str) ;
                                              PopT(CurrentInterface) ;
                                              PutRegInterface(CurrentInterface, str, NulSym) ;
                                              PushT(CurrentInterface)
                                           END
                                          %
                        } 
*)

PROCEDURE TrashList () : BOOLEAN ;
VAR
    CurrentInterface, str: CARDINAL ;
                                         
BEGIN
   
   
   IF String()
   THEN
      IF IsPass3()
                                           THEN
                                              PopT(str) ;
                                              CurrentInterface := MakeRegInterface() ;
                                              PutRegInterface(CurrentInterface, str, NulSym) ;
                                              PushT(CurrentInterface)
                                           END
                                          ;
      
   END ;  (* if *)
   WHILE TokenIs(CommaTok) DO
      IF String()
      THEN
         IF IsPass3()
                                           THEN
                                              PopT(str) ;
                                              PopT(CurrentInterface) ;
                                              PutRegInterface(CurrentInterface, str, NulSym) ;
                                              PushT(CurrentInterface)
                                           END
                                          ;
         
      ELSE
         WriteError('String - expected') ;
         RETURN( FALSE )
      END ;
   END ;  (* while *)
   RETURN( TRUE )
END TrashList ;




END M2Inline.
