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
IMPLEMENTATION MODULE M2Code ;


FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;

FROM M2Options IMPORT Statistics, DisplayQuadruples, OptimizeUncalledProcedures,
                      (* OptimizeDynamic, *) OptimizeCommonSubExpressions, StudentChecking ;

FROM M2Students IMPORT StudentVariableCheck ;

FROM M2Quads IMPORT CountQuads, Head, DisplayQuadList,
                    BackPatchSubranges, VariableAnalysis, LoopAnalysis, ForLoopAnalysis ;

FROM M2Pass IMPORT SetPassToNoPass ;
FROM M2SubExp IMPORT RemoveCommonSubExpressions ;

FROM M2BasicBlock IMPORT GenBasicBlocks, DestroyBasicBlocks,
                         GenBasicBlockCode, ForeachBasicBlockDo ;

FROM M2Optimize IMPORT FoldBranches, RemoveProcedures ;
FROM M2GenGCC IMPORT InitGNUM2 ;
FROM M2GCCDeclare IMPORT FoldConstants ;


CONST
   MaxOptimTimes = 10 ;   (* upper limit of no of times we run through all optimization *)
   Debugging     = FALSE ;

(*
   Percent - calculates the percentage from numerator and divisor
*)

PROCEDURE Percent (numerator, divisor: CARDINAL) ;
BEGIN
   WriteString('  (') ;
   IF divisor=0
   THEN
      WriteString('overflow error')
   ELSE
      WriteCard(numerator*100 DIV divisor, 3)
   END ;
   WriteString('%)')
END Percent ;


(*
   Code - calls procedures to generates the assembly language from
          the quadruples. All quadruple optimization is performed
          via this call.
*)

PROCEDURE Code ;
VAR
   OptimTimes,
   DeltaProc,
   Proc,
   DeltaConst,
   Const,
   DeltaJump,
   Jump,
   DeltaBasicB,
   BasicB,
   DeltaCse,
   Cse,
   Total,
   Count      : CARDINAL ;
BEGIN
   SetPassToNoPass ;
   BackPatchSubranges(Head) ;
   Proc := 0 ;
   Const := 0 ;
   Jump := 0 ;
   BasicB := 0 ;
   Cse  := 0 ;         (* optional optimizations *)
   OptimTimes := 0 ;   (* how many times have we tried to optimize *)
   DeltaProc := 0 ;    (* optional so we set this to zero *)
   DeltaCse := 0 ;     (* optional so we set this to zero *)

   Total := CountQuads(Head) ;
   Count := Total ;

   ForLoopAnalysis ;   (* must be done before any optimization as the index variable increment quad might change *)

   IF DisplayQuadruples
   THEN
      WriteString('before any optimization') ; WriteLn ;
      DisplayQuadList(Head)
   END ;

   InitGNUM2(Head) ;

   REPEAT
      INC(OptimTimes) ;
      FoldConstants ;
      DeltaConst := Count - CountQuads(Head) ;
      Count := CountQuads(Head) ;

      GenBasicBlocks(Head) ;
      DestroyBasicBlocks ;

      IF Debugging AND DisplayQuadruples
      THEN
         WriteString('start of basic block decomposition for the ') ; WriteCard(OptimTimes, 0) ;
         WriteString(' time') ; WriteLn ;
         DisplayQuadList(Head)
      END ;

      DeltaBasicB := Count - CountQuads(Head) ;
      Count := CountQuads(Head) ;

      FoldBranches(Head) ;
      DeltaJump := Count - CountQuads(Head) ;
      Count := CountQuads(Head) ;

      IF Debugging AND DisplayQuadruples
      THEN
         WriteString('after folding branches') ; WriteLn ;
         DisplayQuadList(Head)
      END ;

      GenBasicBlocks(Head) ;
      INC(DeltaBasicB, Count - CountQuads(Head)) ;
      Count := CountQuads(Head) ;

      IF Debugging AND DisplayQuadruples
      THEN
         WriteString('after second basic block decomposition for the ') ; WriteCard(OptimTimes, 0) ;
         WriteString(' time') ; WriteLn ;
         DisplayQuadList(Head)
      END ;

      IF OptimizeUncalledProcedures
      THEN
         DestroyBasicBlocks ;
         RemoveProcedures(Head) ;
         GenBasicBlocks(Head) ;

         DeltaProc := Count - CountQuads(Head) ;
         Count := CountQuads(Head) ;
         IF Debugging AND DisplayQuadruples
         THEN
            WriteString('after uncalled procedure optimization') ; WriteLn ;
            DisplayQuadList(Head)
         END
      END ;

      IF OptimTimes=1
      THEN
         ForeachBasicBlockDo(VariableAnalysis)
      END ;
      IF OptimizeCommonSubExpressions
      THEN
         ForeachBasicBlockDo(RemoveCommonSubExpressions) ;

         DestroyBasicBlocks ;
         GenBasicBlocks(Head) ;
         IF Debugging AND DisplayQuadruples
         THEN
            WriteString('after removing common sub expressions') ; WriteLn ;
            DisplayQuadList(Head)
         END ;

         DeltaCse := Count - CountQuads(Head) ;
         Count := CountQuads(Head) ;

         FoldConstants ;       (* now attempt to fold more constants *)
         INC(DeltaConst, Count-CountQuads(Head)) ;
         Count := CountQuads(Head)
      END ;
      (* now total the optimization components *)
      INC(Proc, DeltaProc) ;
      INC(Const, DeltaConst) ;
      INC(Jump, DeltaJump) ;
      INC(BasicB, DeltaBasicB) ;
      INC(Cse, DeltaCse)
   UNTIL (OptimTimes=MaxOptimTimes) OR
         ((DeltaProc=0) AND (DeltaConst=0) AND (DeltaJump=0) AND (DeltaBasicB=0) AND (DeltaCse=0)) ;

   IF Debugging AND DisplayQuadruples
   THEN
      WriteString('after all optimization') ; WriteLn ;
      DisplayQuadList(Head)
   END ;
   LoopAnalysis ;
   IF Statistics
   THEN
      IF (DeltaProc#0) OR (DeltaConst#0) OR (DeltaJump#0) OR (DeltaBasicB#0) OR (DeltaCse#0)
      THEN
         WriteString('Optimization finished - although more reduction may be possible') ; WriteLn
      END ;
      WriteString('Initial Number of Quadruples:') ; WriteCard(Total, 5) ;
      Percent(Total, Total) ; WriteLn ;
      WriteString('Constant folding achieved   :') ; WriteCard(Const, 5) ;
      Percent(Const, Total) ; WriteLn ;
      WriteString('Branch folding achieved     :') ; WriteCard(Jump, 5) ;
      Percent(Jump, Total) ; WriteLn ;
      WriteString('Basic Block optimization    :') ; WriteCard(BasicB, 5) ;
      Percent(BasicB, Total) ; WriteLn ;
      WriteString('Uncalled Procedures removed :') ; WriteCard(Proc, 5) ;
      Percent(Proc, Total) ; WriteLn ;
      WriteString('Common subexpession removed :') ; WriteCard(Cse, 5) ;
      Percent(Cse, Total) ; WriteLn ;
      WriteString('Total optimization removed  :') ; WriteCard(Const+Jump+BasicB+Proc+Cse, 5) ;
      Percent(Const+Jump+BasicB+Proc+Cse, Total) ; WriteLn ;
      WriteLn ;
      WriteString('Resultant number of quads   :') ; WriteCard(Count, 5) ;
      Percent(Count, Total) ; WriteLn
   END ;
   (* now is a suitable time to check for student errors as we know all the symbols must be resolved *)
   IF StudentChecking
   THEN
      StudentVariableCheck      
   END ;
   GenBasicBlockCode(Head)
END Code ;


END M2Code.
