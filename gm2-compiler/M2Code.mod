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


FROM SYSTEM IMPORT WORD ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;

FROM M2Options IMPORT Statistics, DisplayQuadruples, OptimizeUncalledProcedures,
                      (* OptimizeDynamic, *) OptimizeCommonSubExpressions, StudentChecking ;

FROM M2Students IMPORT StudentVariableCheck ;

FROM SymbolTable IMPORT GetMainModule, IsProcedure,
                        IsModuleWithinProcedure,
                        ForeachProcedureDo,
                        ForeachInnerModuleDo, GetSymName ;

FROM M2Printf IMPORT printf2, printf1, printf0 ;
FROM NameKey IMPORT Name ;

FROM M2Quads IMPORT CountQuads, Head, DisplayQuadList, DisplayQuadRange,
                    BackPatchSubrangesAndOptParam, VariableAnalysis,
                    LoopAnalysis, ForLoopAnalysis ;

FROM M2Pass IMPORT SetPassToNoPass ;
FROM M2SubExp IMPORT RemoveCommonSubExpressions ;

FROM M2BasicBlock IMPORT BasicBlock,
                         InitBasicBlocks, InitBasicBlocksFromRange, KillBasicBlocks,
                         ForeachBasicBlockDo ;

FROM M2Optimize IMPORT FoldBranches, RemoveProcedures ;
FROM M2GenGCC IMPORT InitGNUM2, ConvertQuadsToTree ;

FROM M2GCCDeclare IMPORT FoldConstants, StartDeclareScope,
                         DeclareProcedure, InitDeclarations,
                         DeclareModuleVariables ;

FROM M2Scope IMPORT ScopeBlock, InitScopeBlock, KillScopeBlock, ForeachScopeBlockDo ;
FROM gccgm2 IMPORT InitGlobalContext ;


CONST
   MaxOptimTimes = 10 ;   (* upper limit of no of times we run through all optimization *)
   Debugging     = TRUE ;


VAR
   Total,
   Count,
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
   Cse        : CARDINAL ;

(* %%%FORWARD%%%
PROCEDURE CodeBlock (scope: CARDINAL) ; FORWARD ;
   %%%FORWARD%%% *)


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
   OptimizationAnalysis - displays some simple front end optimization statistics.
*)

PROCEDURE OptimizationAnalysis ;
BEGIN
   IF Statistics
   THEN
      Count := CountQuads(Head) ;

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
   IF DisplayQuadruples
   THEN
      WriteString('after all front end optimization') ; WriteLn ;
      DisplayQuadList(Head)
   END
END OptimizationAnalysis ;


(*
   Code - calls procedures to generates trees from the quadruples.
          All front end quadruple optimization is performed via this call.
*)

PROCEDURE Code ;
BEGIN
   SetPassToNoPass ;
   BackPatchSubrangesAndOptParam(Head) ;
   Total := CountQuads(Head) ;

   ForLoopAnalysis ;   (* must be done before any optimization as the index variable increment quad might change *)

   IF DisplayQuadruples
   THEN
      WriteString('before any optimization') ; WriteLn ;
      DisplayQuadList(Head)
   END ;

   (* now is a suitable time to check for student errors as we know all the front end symbols must be resolved *)
   IF StudentChecking
   THEN
      StudentVariableCheck      
   END ;

   InitGNUM2(Head) ;
   InitGlobalContext ;
   InitDeclarations ;

   CodeBlock(GetMainModule()) ;

   OptimizationAnalysis
END Code ;


(*
   InitialDeclareAndCodeBlock - declares all objects within scope, 
*)

PROCEDURE InitialDeclareAndOptimize (start, end: CARDINAL) ;
VAR
   bb: BasicBlock ;
BEGIN
   Count := CountQuads(Head) ;
   bb := KillBasicBlocks(InitBasicBlocksFromRange(start, end)) ;
   BasicB := Count - CountQuads(Head) ;
   Count := CountQuads(Head) ;

   FoldBranches(start, end) ;
   Jump := Count - CountQuads(Head) ;
   Count := CountQuads(Head) ;

   IF OptimizeUncalledProcedures
   THEN
      RemoveProcedures ;
      bb := KillBasicBlocks(InitBasicBlocksFromRange(start, end)) ;

      Proc := Count - CountQuads(Head) ;
      Count := CountQuads(Head)
   END
END InitialDeclareAndOptimize ;


(*
   DeclareAndCodeBlock - declares all objects within scope, 
*)

PROCEDURE SecondDeclareAndOptimize (start, end: CARDINAL) ;
VAR
   bb: BasicBlock ;
BEGIN
   REPEAT
      FoldConstants(start, end) ;
      DeltaConst := Count - CountQuads(Head) ;
      Count := CountQuads(Head) ;

      bb := KillBasicBlocks(InitBasicBlocksFromRange(start, end)) ;

      DeltaBasicB := Count - CountQuads(Head) ;
      Count := CountQuads(Head) ;

      bb := KillBasicBlocks(InitBasicBlocksFromRange(start, end)) ;
      FoldBranches(start, end) ;
      DeltaJump := Count - CountQuads(Head) ;
      Count := CountQuads(Head) ;

      bb := KillBasicBlocks(InitBasicBlocksFromRange(start, end)) ;
      INC(DeltaBasicB, Count - CountQuads(Head)) ;
      Count := CountQuads(Head) ;

      IF OptimizeUncalledProcedures
      THEN
         bb := KillBasicBlocks(InitBasicBlocksFromRange(start, end)) ;
         RemoveProcedures ;

         DeltaProc := Count - CountQuads(Head) ;
         Count := CountQuads(Head)
      END ;

      IF FALSE AND OptimizeCommonSubExpressions
      THEN
         bb := InitBasicBlocksFromRange(start, end) ;
         ForeachBasicBlockDo(bb, RemoveCommonSubExpressions) ;
         bb := KillBasicBlocks(bb) ;

         bb := KillBasicBlocks(InitBasicBlocksFromRange(start, end)) ;

         DeltaCse := Count - CountQuads(Head) ;
         Count := CountQuads(Head) ;

         FoldConstants(start, end) ;       (* now attempt to fold more constants *)
         INC(DeltaConst, Count-CountQuads(Head)) ;
         Count := CountQuads(Head)
      END ;
      (* now total the optimization components *)
      INC(Proc, DeltaProc) ;
      INC(Const, DeltaConst) ;
      INC(Jump, DeltaJump) ;
      INC(BasicB, DeltaBasicB) ;
      INC(Cse, DeltaCse)
   UNTIL (OptimTimes>=MaxOptimTimes) OR
         ((DeltaProc=0) AND (DeltaConst=0) AND (DeltaJump=0) AND (DeltaBasicB=0) AND (DeltaCse=0)) ;

   IF (DeltaProc#0) OR (DeltaConst#0) OR (DeltaJump#0) OR (DeltaBasicB#0) OR (DeltaCse#0)
   THEN
      WriteString('optimization finished although more reduction may be possible (increase MaxOptimTimes)') ; WriteLn
   END
END SecondDeclareAndOptimize ;


(*
   InitOptimizeVariables - 
*)

PROCEDURE InitOptimizeVariables ;
BEGIN
   Count       := CountQuads(Head) ;
   OptimTimes  := 0 ;
   DeltaProc   := 0 ;
   DeltaConst  := 0 ;
   DeltaJump   := 0 ;
   DeltaBasicB := 0 ;
   DeltaCse    := 0
END InitOptimizeVariables ;


(*
   Init - 
*)

PROCEDURE Init ;
BEGIN
   Proc   := 0 ;
   Const  := 0 ;
   Jump   := 0 ;
   BasicB := 0 ;
   Cse    := 0
END Init ;


(*
   BasicBlockVariableAnalysis - 
*)

PROCEDURE BasicBlockVariableAnalysis (start, end: CARDINAL) ;
VAR
   bb: BasicBlock ;
BEGIN
   bb := InitBasicBlocksFromRange(start, end) ;
   ForeachBasicBlockDo(bb, VariableAnalysis) ;
   bb := KillBasicBlocks(bb)
END BasicBlockVariableAnalysis ;


(*
   OptimizeScopeBlock - 
*)

PROCEDURE OptimizeScopeBlock (sb: ScopeBlock) ;
VAR
   OptimTimes,
   Previous,
   Current   : CARDINAL ;
BEGIN
   InitOptimizeVariables ;
   OptimTimes := 1 ;
   Current := CountQuads(Head) ;
   ForeachScopeBlockDo(sb, InitialDeclareAndOptimize) ;
   ForeachScopeBlockDo(sb, BasicBlockVariableAnalysis) ;
   REPEAT
      ForeachScopeBlockDo(sb, SecondDeclareAndOptimize) ;
      Previous := Current ;
      Current := CountQuads(Head) ;
      INC(OptimTimes)
   UNTIL (OptimTimes=MaxOptimTimes) OR (Current=Previous) ;
   ForeachScopeBlockDo(sb, LoopAnalysis)
END OptimizeScopeBlock ;


(*
   DisplayQuadNumbers - the range, start..end.
*)

PROCEDURE DisplayQuadNumbers (start, end: CARDINAL) ;
BEGIN
   IF DisplayQuadruples
   THEN
      printf2('Coding [%d..%d]\n', start, end)
   END
END DisplayQuadNumbers ;


(*
   CodeBlock - generates all code for this block and also declares
               all types and procedures for this block. It will
               also optimize quadruples within this scope.
*)

PROCEDURE CodeBlock (scope: WORD) ;
VAR
   sb: ScopeBlock ;
   n : Name ;
BEGIN
   sb := InitScopeBlock(scope) ;
   OptimizeScopeBlock(sb) ;
   IF IsProcedure(scope)
   THEN
      DeclareProcedure(scope) ;
      IF DisplayQuadruples
      THEN
         n := GetSymName(scope) ;
         printf1('before coding procedure %a\n', n) ;
         ForeachScopeBlockDo(sb, DisplayQuadRange) ;
         printf0('===============\n')
      END ;
      ForeachScopeBlockDo(sb, ConvertQuadsToTree)
   ELSIF IsModuleWithinProcedure(scope)
   THEN
      IF DisplayQuadruples
      THEN
         n := GetSymName(scope) ;
         printf1('before coding module %a within procedure\n', n) ;
         ForeachScopeBlockDo(sb, DisplayQuadRange) ;
         printf0('===============\n')
      END ;
      ForeachInnerModuleDo(scope, CodeBlock) ;
      ForeachScopeBlockDo(sb, ConvertQuadsToTree)
   ELSE
      StartDeclareScope(scope) ;
      IF DisplayQuadruples
      THEN
         n := GetSymName(scope) ;
         printf1('before coding module %a\n', n) ;
         ForeachScopeBlockDo(sb, DisplayQuadRange) ;
         printf0('===============\n')
      END ;
      ForeachScopeBlockDo(sb, ConvertQuadsToTree) ;
      ForeachProcedureDo(scope, CodeBlock)
   END ;
   sb := KillScopeBlock(sb)
END CodeBlock ;


BEGIN
   Init
END M2Code.
