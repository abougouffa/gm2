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
IMPLEMENTATION MODULE M2Optimize ;

(*
    Title      : M2Optimize
    Author     : Gaius Mulley
    System     : UNIX (GNU Modula-2)
    Date       : Sat Aug 14 15:07:47 1999
    Last edit  : Thurs July 13 2000
    Description: removes redundant quadruples, redundant GotoOps, redundant procedures.
*)

FROM M2Debug IMPORT Assert ;

FROM NameKey IMPORT Name, WriteKey, MakeKey, GetKey ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;

FROM M2Error IMPORT InternalError, NewError, ErrorFormat0 ;
FROM M2Batch IMPORT GetModuleNo ;
FROM M2Quiet IMPORT qprintf1 ;

FROM SymbolTable IMPORT GetSymName,
                        GetProcedureQuads, GetModuleQuads,
                        GetModule, GetNthProcedure,
                        GetSubrange,
                        PutProcedureReachable, IsProcedureReachable,
                        PutProcedureStartQuad, PutProcedureEndQuad,
                        PutProcedureScopeQuad,
                        IsProcedure,
                        GetDeclared, GetFirstUsed,
                        GetNth, NoOfElements, GetType,
                        IsExportQualified, IsExportUnQualified,
                        NulSym ;

FROM M2Quads IMPORT QuadOperator, Head, GetQuad, GetNextQuad, PutQuad, SubQuad,
                    Opposite, IsReferenced, GetRealQuad, QuadToTokenNo ;


CONST
   MaxString = 255 ;


(* %%%FORWARD%%%
PROCEDURE FoldMultipleGoto (QuadNo: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE ReduceBranch (Operator: QuadOperator;
                        CurrentQuad,
                        CurrentOperand1, CurrentOperand2,
                        CurrentOperand3: CARDINAL;
                        VAR NextQuad: CARDINAL;
                        Folded: BOOLEAN) : BOOLEAN ; FORWARD ;
PROCEDURE IsBasicBlock (From, To: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE ReduceGoto (CurrentQuad, CurrentOperand3, NextQuad: CARDINAL;
                      Folded: BOOLEAN) : BOOLEAN ; FORWARD ;
PROCEDURE KnownReachableInitCode(Start, End: CARDINAL) ; FORWARD ;
PROCEDURE MakeExportedProceduresReachable ; FORWARD ;
PROCEDURE KnownReachable (Start, End: CARDINAL) ; FORWARD ;
PROCEDURE KnownReach (Sym: CARDINAL) ; FORWARD ;
PROCEDURE Delete (Start, End: CARDINAL) ; FORWARD ;
PROCEDURE DeleteUnReachableProcedures ; FORWARD ;
   %%%FORWARD%%% *)



(*
   FoldBranches - folds unneccessary branches in the list of quadruples.
                  It searches for the following patterns:

                  [x]  GotoOp  _   _   y                GotoOp   _   _   z
                  ...                                   ...
                  [y]  GotoOp  _   _   z      "deleted"

                  WHERE ... may contain 0..n Pseudo Quads


                  OR


                  [x]  IfREL   _   _   z      If NOT REL  _   _   a
                  ...                         ...
                  [y]  Goto    _   _   a      "deleted"
                  ...                         ...
                  [z]


                  WHERE ... may contain 0..n Pseudo Quads
                  but in this case they must not be a
                      target of any other quad.
*)

PROCEDURE FoldBranches (start, end: CARDINAL) ;
VAR
   Folded     : BOOLEAN ;
   i,
   Right      : CARDINAL ;
   Operator   : QuadOperator ;
   Operand1,
   Operand2,
   Operand3   : CARDINAL ;
BEGIN
   REPEAT
      i := start ;
      Folded := FALSE ;
      WHILE (i<=end) AND (i#0) DO
         Right := GetRealQuad(GetNextQuad(i)) ;
         GetQuad(i, Operator, Operand1, Operand2, Operand3) ;
         CASE Operator OF

         GotoOp             : Folded := ReduceGoto(i, Operand3,
                                                   Right, Folded) |
         IfInOp, IfNotInOp,
         IfNotEquOp, IfEquOp,
         IfLessEquOp, IfGreEquOp,
         IfGreOp, IfLessOp  : Folded := ReduceBranch(Operator, i,
                                                     Operand1, Operand2, Operand3,
                                                     Right, Folded)

         ELSE
         END ;
         i := Right
      END
   UNTIL NOT Folded
END FoldBranches ;


(*
   ReduceBranch - searches for the following pattern:

                  [x]  IfREL   _   _   z      If NOT REL  _   _   a
                  ...                         ...
                  [y]  Goto    _   _   a      "deleted"
                  ...                         ...
                  [z]


                  WHERE ... may contain 0..n Pseudo Quads
                  but in this case they must not be a
                      target of any other quad.

*)

PROCEDURE ReduceBranch (Operator: QuadOperator;
                        CurrentQuad,
                        CurrentOperand1, CurrentOperand2,
                        CurrentOperand3: CARDINAL;
                        VAR NextQuad: CARDINAL;
                        Folded: BOOLEAN) : BOOLEAN ;
VAR
   OpNext : QuadOperator ;
   NextPlusOne,
   Op1Next,
   Op2Next,
   Op3Next,
   From,
   To     : CARDINAL ;
BEGIN
   (* If op         NextQuad+1 *)
   (* Goto          x          *)

   IF NextQuad#0
   THEN
      IF (GetNextQuad(CurrentQuad)=CurrentOperand3) OR
         (GetRealQuad(GetNextQuad(CurrentQuad))=CurrentOperand3)
      THEN
         SubQuad(CurrentQuad) ;
         Folded := TRUE
      ELSE
         From := GetNextQuad(CurrentQuad) ;  (* start after CurrentQuad *)
         To := NextQuad ;
         CurrentOperand3 := GetRealQuad(CurrentOperand3) ;

         NextPlusOne := GetRealQuad(GetNextQuad(NextQuad)) ;
         GetQuad(NextQuad, OpNext, Op1Next, Op2Next, Op3Next) ;
         IF (OpNext=GotoOp) AND (NextPlusOne=CurrentOperand3) AND
            IsBasicBlock(From, To)
         THEN
            (*       Op3Next := GetRealQuad(Op3Next) ; *)
            SubQuad(NextQuad) ;
            PutQuad(CurrentQuad, Opposite(Operator),
                    CurrentOperand1, CurrentOperand2, Op3Next) ;
            NextQuad := NextPlusOne ;
            Folded := TRUE
         END
      END ;
      IF FoldMultipleGoto(CurrentQuad)
      THEN
         Folded := TRUE
      END
   END ;
   RETURN( Folded )
END ReduceBranch ;


(*
   IsBasicBlock - returns TRUE if no other quadruple jumps inbetween
                  the range From..To.
                  It assumes that there are no jumps in the quadruples
                  From..To.
*)

PROCEDURE IsBasicBlock (From, To: CARDINAL) : BOOLEAN ;
BEGIN
   WHILE From#To DO
      IF IsReferenced(From)
      THEN
         RETURN( FALSE )
      ELSE
         IF From>To
         THEN
            InternalError('assert failed From should never be larger than To', __FILE__, __LINE__)
         END ;
         From := GetNextQuad(From)
      END
   END ;
   RETURN( TRUE )
END IsBasicBlock ;


(*
   ReduceGoto - searches for the following patterns:

                [x]  GotoOp  _   _   y                GotoOp   _   _   z
                  ...                                   ...
                [y]  GotoOp  _   _   z      "deleted"


*)

PROCEDURE ReduceGoto (CurrentQuad, CurrentOperand3, NextQuad: CARDINAL;
                      Folded: BOOLEAN) : BOOLEAN ;
BEGIN
   CurrentOperand3 := GetRealQuad(CurrentOperand3) ;
   (* IF next quad is a GotoOp *)
   IF CurrentOperand3=NextQuad
   THEN
      SubQuad(CurrentQuad) ;
      Folded := TRUE
   ELSE
      (* Does Goto point to another Goto ? *)
      IF FoldMultipleGoto(CurrentQuad)
      THEN
         Folded := TRUE
      END
   END ;
   RETURN( Folded )
END ReduceGoto ;


(*
   FoldMultipleGoto - takes a QuadNo and if it jumps to another GotoOp
                      then it takes the later target as a replacement
                      for its own.

                      NOTE  it does not remove any quadruples.
*)

PROCEDURE FoldMultipleGoto (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   Operator,
   Op      : QuadOperator ;
   Op1, Op2,
   Op3,
   Operand1,
   Operand2,
   Operand3: CARDINAL ;
BEGIN
   GetQuad(QuadNo, Operator, Operand1, Operand2, Operand3) ;
   Operand3 := GetRealQuad(Operand3) ;  (* skip pseudo quadruples *)
   GetQuad(Operand3, Op, Op1, Op2, Op3) ;
   IF Op=GotoOp
   THEN
      PutQuad(QuadNo, Operator, Operand1, Operand2, Op3) ;
      (* Dont want success to be returned if in fact the Goto *)
      (* line number has not changed... otherwise we loop     *)
      (* forever.                                             *)
      RETURN( Op3#Operand3 )
   ELSE
      RETURN( FALSE )
   END
END FoldMultipleGoto ;


(*
   RemoveProcedures - removes any procedures that are never referenced
                      by the quadruples.
*)

PROCEDURE RemoveProcedures ;
VAR
   Start,
   End   : CARDINAL ;
   n,
   Module: CARDINAL ;
BEGIN
   (* DisplayReachable ; *)
   n := 1 ;
   REPEAT
      Module := GetModuleNo(n) ;
      (* Should be at least one module *)
      IF Module#NulSym
      THEN
         (* WriteString('Module ') ; WriteKey(GetSymName(Module)) ; *)
         GetModuleQuads(Module, Start, End) ;
         IF Start#End
         THEN
            (* Module does have some initialization code *)
            (*
            WriteString(' has initialization code  ') ;
            WriteCard(Start, 0) ; WriteString('..') ; WriteCard(End, 0) ;
            WriteLn ;
            *)
            KnownReachableInitCode(Start, End)
         ELSE
            (* WriteString(' has no initialization code') ; WriteLn *)
         END ;
         INC(n)
      END
   UNTIL Module=NulSym ;
   MakeExportedProceduresReachable ;
   DeleteUnReachableProcedures
END RemoveProcedures ;


(*
   MakeExportedProceduresReachable - make all exported procedures
                                     reachable
*)

PROCEDURE MakeExportedProceduresReachable ;
VAR
   Scope,
   Start,
   End,
   n, m,
   Module,
   Proc  : CARDINAL ;
BEGIN
   m := 1 ;
   REPEAT
      Module := GetModuleNo(m) ;
      IF Module#NulSym
      THEN
         n := 1 ;
         Proc := GetNthProcedure(Module, n) ;
         WHILE Proc#NulSym DO
            IF (IsExportQualified(Proc) OR IsExportUnQualified(Proc)) AND
               (NOT IsProcedureReachable(Proc))
            THEN
               PutProcedureReachable(Proc) ;
               GetProcedureQuads(Proc, Scope, Start, End) ;
               KnownReachable(Start, End)
            END ;
            INC(n) ;
            Proc := GetNthProcedure(Module, n)
         END ;
         INC(m)
      END
   UNTIL Module=NulSym
END MakeExportedProceduresReachable ;


(*
   KnownReachableInitCode - All quadruples between "Start..End-1" and
                            non procedural code are reachable.
*)

PROCEDURE KnownReachableInitCode(Start, End: CARDINAL) ;
VAR
   Op           : QuadOperator ;
   Op1, Op2, Op3: CARDINAL ;
BEGIN
   IF Start#0
   THEN
      REPEAT
         GetQuad(Start, Op, Op1, Op2, Op3) ;
         IF Op=NewLocalVarOp
         THEN
            (* Skip all procedural code of a Modules Initialization Code *)
            REPEAT
               Start := GetNextQuad(Start) ;
               GetQuad(Start, Op, Op1, Op2, Op3)
            UNTIL (Start=End) OR (Op=ReturnOp)
         ELSE
            (* Can be sure that quads are at a base level Zero, outside procedures *)
            CASE Op OF

            AddrOp,
            CallOp,
            ParamOp,
            XIndrOp,
            BecomesOp: KnownReach(Op3)

            ELSE
            END ;
            IF Op=DummyOp
            THEN
               (* Start been deleted therefore just increment *)
               INC(Start)
            ELSE
               (* Follow the quad list *)
               Start := GetNextQuad(Start)
            END
         END
      UNTIL Start=End
   END
END KnownReachableInitCode ;


PROCEDURE KnownReachable (Start, End: CARDINAL) ;
VAR
   Op           : QuadOperator ;
   Op1, Op2, Op3: CARDINAL ;
BEGIN
   IF Start#0
   THEN
      REPEAT
         GetQuad(Start, Op, Op1, Op2, Op3) ;
         CASE Op OF

         AddrOp,
         CallOp,
         ParamOp,
         XIndrOp,
         BecomesOp: KnownReach(Op3)

         ELSE
         END ;
         IF Op=DummyOp
         THEN
            (* Start been deleted therefore just increment *)
            INC(Start)
         ELSE
            (* Follow the quad list *)
            Start := GetNextQuad(Start)
         END
      UNTIL Start=End
   END
END KnownReachable ;


PROCEDURE KnownReach (Sym: CARDINAL) ;
VAR
   Scope,
   Start,
   End  : CARDINAL ;
BEGIN
   IF IsProcedure(Sym)
   THEN
      IF NOT IsProcedureReachable(Sym)
      THEN
         (* Not reachable at present - therefore make it reachable *)
         PutProcedureReachable(Sym) ;
         GetProcedureQuads(Sym, Scope, Start, End) ;
         KnownReachable(Start, End)
      END
   END
END KnownReach ;


(*
   DeleteUnReachableProcedures - Deletes all procedures that are unreachable.
*)

PROCEDURE DeleteUnReachableProcedures ;
VAR
   ProcName: Name ;
   n, m,
   Scope,
   Start,
   End,
   Module,
   Proc    : CARDINAL ;
BEGIN
   m := 1 ;
   REPEAT
      Module := GetModuleNo(m) ;
      IF Module#NulSym
      THEN
         n := 1 ;
         Proc := GetNthProcedure(Module, n) ;
         WHILE Proc#NulSym DO
            IF IsProcedureReachable(Proc) OR
               IsExportQualified(Proc) OR IsExportUnQualified(Proc)
            THEN
               (* is reachable - do not delete it *)
            ELSE
               ProcName := GetSymName(Proc) ;
               qprintf1('[%a]\n', ProcName) ;

               GetProcedureQuads(Proc, Scope, Start, End) ;
               IF Start#0
               THEN
                  Delete(Scope, End) ;
                  (* No Longer any Quads for this Procedure *)
                  PutProcedureScopeQuad(Proc, 0) ;
                  PutProcedureStartQuad(Proc, 0) ;
                  PutProcedureEndQuad(Proc, 0)
               END
            END ;
            INC(n) ;
            Proc := GetNthProcedure(Module, n)
         END ;
         INC(m)
      END
   UNTIL Module=NulSym
END DeleteUnReachableProcedures ;


(*
   Delete - deletes all quadruples from Start..End
            or the end of the procedure.
*)

PROCEDURE Delete (Start, End: CARDINAL) ;
VAR
   Last,
   i   : CARDINAL ;
   Op  : QuadOperator ;
   Op1,
   Op2,
   Op3 : CARDINAL ;
BEGIN
   Last := GetNextQuad(End) ;
   WHILE (Head#0) AND (Start#0) AND (Last#Start) DO
      GetQuad(Start, Op, Op1, Op2, Op3) ;
      IF Op=DummyOp
      THEN
         (* Start has already been deleted - try next quad *)
         INC(Start)
      ELSIF Op=ReturnOp
      THEN
         (* Found end of procedure therefore just delete and exit *)
         (* WriteString('Deleting') ; WriteCard(Start, 6) ; WriteLn ; *)
         SubQuad(Start) ;
         Start := Last
      ELSE
         (* Following the list of quadruples to the End *)
         i := GetNextQuad(Start) ;
         (* WriteString('Deleting') ; WriteCard(Start, 6) ; WriteLn ; *)
         SubQuad(Start) ;
         Start := i
      END
   END
END Delete ;


(*
   DisplayReachable - Displays the data structures surrounding Reachablity.
*)

PROCEDURE DisplayReachable ;
VAR
   n, m,
   Scope,
   Start,
   End,
   Module,
   Proc  : CARDINAL ;
BEGIN
   m := 1 ;
   REPEAT
      Module := GetModuleNo(m) ;
      IF Module#NulSym
      THEN
         WriteString('Module') ; WriteCard(m, 3) ; WriteKey(GetSymName(Module)) ;
         GetModuleQuads(Module, Start, End) ; WriteString(' Reachable ') ;
         WriteCard(Start, 4) ; WriteCard(End, 4) ; WriteLn ;
         n := 1 ;
         Proc := GetNthProcedure(Module, n) ;
         WHILE Proc#NulSym DO
            WriteString('Procedure ') ; WriteKey(GetSymName(Proc)) ;
            GetProcedureQuads(Proc, Scope, Start, End) ;
            WriteString(' Quads: ') ; WriteCard(Start, 6) ; WriteCard(End, 6) ;
            IF NOT IsProcedureReachable(Proc)
            THEN
               WriteString(' UN reachable')
            ELSE
               WriteString(' IS reachable')
            END ;
            WriteLn ;
            INC(n) ;
            Proc := GetNthProcedure(Module, n)
         END ;
         INC(m)
      END
   UNTIL Module=NulSym
END DisplayReachable ;


END M2Optimize.
