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
IMPLEMENTATION MODULE M2Reference ;

FROM M2Debug IMPORT WriteDebug ;
FROM NameKey IMPORT WriteKey ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;

FROM M2Lexical IMPORT WriteError, TokenIs, CurrentToken, TokenType, WriteErrorFormat1, WriteErrorFormat2 ;
FROM M2Error IMPORT BeginError, EndError ;
FROM M2Atom IMPORT Ident, IsAutoPushOn ;

FROM SymbolTable IMPORT GetSym, GetType, GetLocalSym,
                        StartScope, EndScope,
                        PutIncluded,
                        IsVarParam, IsProcedure, IsDefImp, IsModule,
                        IsRecord,
                        NulSym,
                        GetSymName, RequestSym ;

FROM M2Reserved IMPORT PeriodTok,
                       LSBraTok, RSBraTok, UpArrowTok, CommaTok ;

IMPORT P1Expression ;
IMPORT P3Expression ;

FROM M2Quads IMPORT PopT, PushT, PushTF, PopTF,
                    BuildDesignatorRecord,
                    BuildDesignatorArray,
                    BuildDesignatorPointer,
                    CheckWithReference,
                    CheckOuterScopeProcedureVariable ;

FROM M2Batch IMPORT IsModuleKnown ;


(* %%%FORWARD%%%
PROCEDURE BuildDesignator () : BOOLEAN ; FORWARD ;
PROCEDURE BuildIdentList () : BOOLEAN ; FORWARD ;
PROCEDURE BuildQualident () : BOOLEAN ; FORWARD ;
PROCEDURE DisplayIdent ; FORWARD ;
PROCEDURE ParseDesignator () : BOOLEAN ; FORWARD ;
PROCEDURE ParseIdentList () : BOOLEAN ; FORWARD ;
PROCEDURE ParseQualident () : BOOLEAN ; FORWARD ;
   %%%FORWARD%%% *)


(*
   Designator - returns true if a designator was found and
                if the Auto ident push flag is true then
                the Sym is altered to represent the Designator.

                The Stack:


                Entry                         Exit

                                                               <- Ptr
                +--------------+              +--------------+
                | Sym   | Type |              | Sym   | Type |
                |--------------|              |--------------|
*)

PROCEDURE Designator() : BOOLEAN ;
BEGIN
   IF IsAutoPushOn()
   THEN
      RETURN( BuildDesignator() )
   ELSE
      RETURN( ParseDesignator() )
   END
END Designator ;



(*
   BuildDesignator - returns true if a designator was found and
                     a symbol is placed onto the compile time stack.

                     The Stack:


                     Entry                         Exit

                                                              <- Ptr
                     +--------------+              +--------------+
                     | Sym   | Type |              | Sym   | Type |
                     |--------------|              |--------------|
*)

PROCEDURE BuildDesignator () : BOOLEAN ;
VAR
   Success  : BOOLEAN ;
   Sym, Type,
   n, Name  : CARDINAL ;
BEGIN
   IF Qualident()
   THEN
      CheckWithReference ;  (* ********* *)
      CheckOuterScopeProcedureVariable ;  (* ********* *)
      REPEAT
         (* DisplayIdent ;  (* Error tracing *) *)
         PopTF(Sym, Type) ;
         IF TokenIs(PeriodTok)
         THEN
            PushTF(Sym, Type) ;
            (* DisplayIdent ;  (* Error tracing *) *)
            n := 0 ;
            REPEAT
               IF Type=NulSym
               THEN
                  IF IsModuleKnown(GetSymName(Sym))
                  THEN
                     WriteErrorFormat2('%s looks like a module which has not been globally imported (eg. suggest that you IMPORT %s ;)',
                                       GetSymName(Sym), GetSymName(Sym))
                  ELSE
                     WriteErrorFormat1('%s is not a record variable', GetSymName(Sym))
                  END
               ELSIF IsRecord(Type)
               THEN
                  StartScope(Type) ;
                  IF Ident()
                  THEN
                     PopT(Name) ;
                     Sym := GetLocalSym(Type, Name) ;
                     IF Sym=NulSym
                     THEN
                        WriteErrorFormat2('field %s does not exist within record %s', Name, GetSymName(Type))
                     END ;
                     Type := GetType(Sym) ;
                     PushTF(Sym, Type) ;
                     (* DisplayIdent ;  (* Error tracing *) *)
                     INC(n)
                  END ;
                  EndScope
               ELSE
                  WriteErrorFormat1('%s is not a record type', GetSymName(Type))
               END
            UNTIL NOT TokenIs(PeriodTok) ;
            PushT(n) ;
            BuildDesignatorRecord ;    (* ************ *)
            (* DisplayIdent ;  (* Error tracing *) *)
            Success := TRUE
         ELSIF TokenIs(LSBraTok)
         THEN
            PushTF(Sym, Type) ;
            IF P3Expression.ExpList()
            THEN
               BuildDesignatorArray ;    (* ************ *)
               IF TokenIs(RSBraTok)
               THEN
                  Success := TRUE
               ELSE
                  WriteError('] - expected') ;
                  Success := FALSE
               END
            ELSE
               WriteError('ExpList - expected') ;
               Success := FALSE
            END
         ELSIF TokenIs(UpArrowTok)
         THEN
            PushTF(Sym, Type) ;
            (* DisplayIdent ;  (* Error tracing *) *)
            BuildDesignatorPointer ;    (* ************ *)
            (* DisplayIdent ;  (* Error tracing *) *)
            Success := TRUE
         ELSE
            PushTF(Sym, Type) ;
            Success := FALSE
         END
      UNTIL NOT Success ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END BuildDesignator ;


(*
   ParseDesignator - returns true if a designator is found.
*)

PROCEDURE ParseDesignator () : BOOLEAN ;
VAR
   Success  : BOOLEAN ;
BEGIN
   IF Qualident()
   THEN
      REPEAT
         IF TokenIs(PeriodTok)
         THEN
            REPEAT
               IF Ident()
               THEN
               END ;
            UNTIL NOT TokenIs(PeriodTok) ;
            Success := TRUE
         ELSIF TokenIs(LSBraTok)
         THEN
            IF P1Expression.ExpList()
            THEN
               IF TokenIs(RSBraTok)
               THEN
                  Success := TRUE
               ELSE
                  WriteError('] - expected') ;
                  Success := FALSE
               END
            ELSE
               WriteError('ExpList - expected') ;
               Success := FALSE
            END
         ELSIF TokenIs(UpArrowTok)
         THEN
            Success := TRUE
         ELSE
            Success := FALSE
         END
      UNTIL NOT Success ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ParseDesignator ;


(*
   Qualident - returns true if a Qualident was encountered.
               If IsAutoPushOn is true then the compile time stack
               is manipulated as follows.

               The Stack:


               Entry                         Exit

                                                              <- Ptr
               Empty                         +--------------+
                                             | Sym   | Type |
                                             |--------------|
*)

PROCEDURE Qualident () : BOOLEAN ;
BEGIN
   IF IsAutoPushOn()
   THEN
      RETURN( BuildQualident() )
   ELSE
      RETURN( ParseQualident() )
   END
END Qualident ;


(*
   BuildQualident - returns true if a Qualident was encountered.
                    The Stack:


                    Entry                         Exit

                                                                   <- Ptr
                    Empty                         +--------------+
                                                  | Sym   | Type |
                                                  |--------------|
*)

PROCEDURE BuildQualident() : BOOLEAN ;
VAR
   Success  : BOOLEAN ;
   Type,
   Name, Sym: CARDINAL ;
BEGIN
(*
   ; WriteString('BuildQualident') ;
   ; WriteToken(CurrentToken, TokenType) ;
*)
   IF Ident()
   THEN
      (* WriteString('in BuildQualident found Ident') ; *)
      PopT(Name) ;
      Sym := RequestSym(Name) ;
      IF IsDefImp(Sym) OR IsModule(Sym)
      THEN
         IF TokenIs(PeriodTok)
         THEN
            StartScope(Sym) ;
            IF Qualident()
            THEN
               Success := TRUE
            ELSE
               Success := FALSE
            END ;
            EndScope ;
            IF Success
            THEN
               PopTF(Sym, Type) ;
               PushTF(Sym, Type) ;
               PutIncluded(Sym)
            END
         ELSE
            Success := FALSE
         END
      ELSE
         PushTF(Sym, GetType(Sym)) ;
         (* DisplayIdent ;   (* Error tracing *) *)
         Success := TRUE
      END
   ELSE
      Success := FALSE
   END ;
   (* WriteString('Exit BuildQualident') ; *)
   RETURN( Success )
END BuildQualident ;


(*
   ParseQualident - returns true if a Qualident was encountered.
*)

PROCEDURE ParseQualident () : BOOLEAN ;
VAR
   Success  : BOOLEAN ;
BEGIN
   IF Ident()
   THEN
      IF TokenIs(PeriodTok)
      THEN
         IF Qualident()
         THEN
            Success := TRUE
         ELSE
            WriteError('identifier expected after .') ;
            Success := FALSE
         END
      ELSE
         Success := TRUE
      END
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ParseQualident ;


(*
   IdentList - Stacks the Ident List on the quadruple stack
               providing that Auto push is on.

               The Stack:

               Entry                     Exit

                                                      <- Ptr
               Empty                     +----------+
                                         | #        |
                                         |----------|
                                         | Ident 1  |
                                         |----------|
                                         | Ident 2  |
                                         |----------|
                                         .          .
                                         .          .
                                         .          .
                                         |----------|
                                         | Ident #  |
                                         |----------|
*)

PROCEDURE IdentList() : BOOLEAN ;
BEGIN
   IF IsAutoPushOn()
   THEN
      RETURN( BuildIdentList() )
   ELSE
      RETURN( ParseIdentList() )
   END
END IdentList ;


(*
   BuildIdentList - Stacks the Ident List on the quadruple stack.

                    The Stack:

                    Entry                     Exit

                                                           <- Ptr
                    Empty                     +----------+
                                              | #        |
                                              |----------|
                                              | Ident 1  |
                                              |----------|
                                              | Ident 2  |
                                              |----------|
                                              .          .
                                              .          .
                                              .          .
                                              |----------|
                                              | Ident #  |
                                              |----------|
*)

PROCEDURE BuildIdentList() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
   n      : CARDINAL ;
BEGIN
   IF Ident()
   THEN
      n := 1 ;
      WHILE TokenIs(CommaTok) DO
         IF Ident()
         THEN
            INC(n)
         ELSE
            WriteError('Ident - expected')
         END
      END ;
      PushT(n) ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END BuildIdentList ;


(*
   ParseIdentList - returns true if a legal IdentList was parsed.
*)

PROCEDURE ParseIdentList() : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF Ident()
   THEN
      WHILE TokenIs(CommaTok) DO
         IF Ident()
         THEN
         ELSE
            WriteError('Ident - expected')
         END
      END ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END ParseIdentList ;


(*
   DisplayIdent - displays the ident residing on the top of the compile
                  time stack.

                  The Stack:


                  Entry                         Exit

                                                           <- Ptr
                  +--------------+              +--------------+
                  | Sym   | Type |              | Sym   | Type |
                  |--------------|              |--------------|
*)

PROCEDURE DisplayIdent ;
VAR
   Sym, Type: CARDINAL ;
BEGIN
   PopTF(Sym, Type) ;
   WriteString('Tracing Sym and Type') ; WriteLn ;
   WriteString('Sym  ') ; WriteCard(Sym, 6) ; WriteString('  ') ; WriteKey(GetSymName(Sym)) ; WriteLn ;
   WriteString('Type ') ; WriteCard(Type, 6) ; WriteString('  ') ; WriteKey(GetSymName(Type)) ; WriteLn ;
   PushTF(Sym, Type)
END DisplayIdent ;


END M2Reference.
