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
IMPLEMENTATION MODULE M2BasicBlock ;


FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Pass IMPORT SetPassToCodeGeneration ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM M2Debug IMPORT Assert ;
FROM M2Options IMPORT OptimizeBasicBlock ;

FROM M2Quads IMPORT IsReferenced, IsConditional, IsUnConditional, IsCall,
                    IsReturn, IsNewLocalVar, IsKillLocalVar,
                    IsPseudoQuad, IsModFile,
                    GetNextQuad, GetQuad, QuadOperator,
                    SubQuad ;

FROM M2GenGCC IMPORT ConvertQuadsToTree ;


TYPE
   PtrToBasicBlock = POINTER TO BasicBlock ;
   BasicBlock      = RECORD
                        StartQuad  : CARDINAL ;  (* First Quad in Basic Block *)
                        EndQuad    : CARDINAL ;  (* End Quad in Basic Block   *)
                        CPUCycles  : CARDINAL ;  (* Number of CPU cycles the  *)
                                                 (* Basic Block requires for  *)
                                                 (* execution.                *)
                        InsideProc : BOOLEAN ;   (* is this BB inside a proc? *)
                                                 (* Next Basic Block in list  *)
                        Right      : PtrToBasicBlock ;
                                                 (* Last Basic Block in list  *)
                        Left       : PtrToBasicBlock ;
                     END ;

VAR
   HeadOfBB    : PtrToBasicBlock ;   (* First in the list of Basic Blocks *)
   FreeBB      : PtrToBasicBlock ;   (* Free list of Basic Blocks         *)


(* %%%FORWARD%%%
PROCEDURE Add (VAR Head: PtrToBasicBlock;
               b : PtrToBasicBlock) ; FORWARD ;
PROCEDURE CodeBB (Head: CARDINAL; Inside: BOOLEAN) ; FORWARD ;
PROCEDURE ConvertQuads2BasicBlock (VAR Head: CARDINAL) ; FORWARD ;
PROCEDURE DisplayBasicBlocks ; FORWARD ;
PROCEDURE DisplayBlock (b: PtrToBasicBlock) ; FORWARD ;
PROCEDURE EndBB (b: PtrToBasicBlock; Quad: CARDINAL) ; FORWARD ;
PROCEDURE InitializeBasicBlock ; FORWARD ;
PROCEDURE InsideProcedure (VAR CurrentlyInside: BOOLEAN;
                           b: PtrToBasicBlock) : BOOLEAN ; FORWARD ;
PROCEDURE StartBB (b: PtrToBasicBlock; Quad: CARDINAL; In: BOOLEAN) ; FORWARD ;
PROCEDURE Sub (VAR Head: PtrToBasicBlock;
               b: PtrToBasicBlock) ; FORWARD ;
   %%%FORWARD%%% *)

(*
   BasicBlock - Converts a list of quadruples, Head, to a list of Basic
                Blocks, each Basic Block is then passed on to the
                specific target code generation phase.

                Analageous with a protocol:

                +------------------------------------------------+
                |     Generate Quadruples from Source            |
                +------------------------------------------------+
                     |
                     v
                +------------------------------------------------+
                | Convert List of Quadruples to Basic Blocks     |
                +------------------------------------------------+
                     |            |
                     v            v
                +-----------+
                |  Code     |    ....
                +-----------+
*)

PROCEDURE GenBasicBlocks (VAR Head: CARDINAL) ;
BEGIN
   InitializeBasicBlock ;
   ConvertQuads2BasicBlock(Head) ;
   (* DisplayBasicBlocks *)
END GenBasicBlocks ;


(*
   DestroyBasicBlocks - destroys the list of Basic Blocks - so that
                        the quadruples can be furthur optimized.
*)

PROCEDURE DestroyBasicBlocks ;
VAR
   b, c: PtrToBasicBlock ;
BEGIN
   b := HeadOfBB ;
   IF b#NIL
   THEN
      REPEAT
         c := b^.Right ;
         b^.Right := FreeBB ;
         FreeBB := b ;
         b := c
      UNTIL b=HeadOfBB
   END ;
   HeadOfBB := NIL
END DestroyBasicBlocks ;


(*
   New - returns a basic block.
*)

PROCEDURE New () : PtrToBasicBlock ;
VAR
   b: PtrToBasicBlock ;
BEGIN
   IF FreeBB=NIL
   THEN
      NEW(b)
   ELSE
      b := FreeBB ;
      FreeBB := FreeBB^.Right
   END ;
   Assert(b#NIL) ;
   RETURN( b )
END New ;


(*
   GenBasicBlockCode - converts the Quadruples within Basic Blocks into
                       Code.
*)

PROCEDURE GenBasicBlockCode (Head: CARDINAL) ;
BEGIN
   SetPassToCodeGeneration ;
   ConvertQuadsToTree(Head)
END GenBasicBlockCode ;


(*
   ConvertQuads2BasicBlock - converts a list of quadruples to a list of
                             Basic Blocks.
                             A Basic Block is defined as a list of quadruples
                             which has only has one entry and exit point.
*)

PROCEDURE ConvertQuads2BasicBlock (VAR Head: CARDINAL) ;
VAR
   LastQuadMod,
   LastQuadConditional,
   LastQuadCall,
   CurrentlyInside,
   LastQuadReturn     : BOOLEAN ;
   Quad               : CARDINAL ;
   b                  : PtrToBasicBlock ;
   CurrentBB          : PtrToBasicBlock ;
   LastBB             : PtrToBasicBlock ;
BEGIN
   (*
      Algorithm to perform Basic Block:

      For every quadruple establish a set of leaders.
      A Leader is defined as a quadruple which is
      either:

      (i)   The first quadruple.
      (ii)  Any quadruple which is the target of a jump or unconditional jump.
      (iii) Any statement which follows a conditional jump

      For each leader construct a basic block.
      A Basic Block starts with a leader quadruple and ends with either:

      (i)  Another Leader
      (ii) An unconditional Jump.

      Any quadruples that do not fall into a Basic Block can be thrown away
      since they will never be executed.
   *)
   LastBB := NIL ;
   Quad := Head ;
   LastQuadConditional := TRUE ;  (* Force Rule (i) *)
   LastQuadCall := FALSE ;
   LastQuadReturn := FALSE ;
   LastQuadMod := FALSE ;
   CurrentlyInside := FALSE ;
   (* Scan all quadruples *)
   WHILE Quad#0 DO
      IF LastQuadConditional OR LastQuadCall OR LastQuadReturn OR
         LastQuadMod OR IsReferenced(Head, Quad)
      THEN
         (* Rule (ii) *)
         IF IsNewLocalVar(Quad)
         THEN
            (* start of a procedure *)
            CurrentlyInside := TRUE
         END ;
         CurrentBB := New() ;                      (* Get a new Basic Block *)
                                  (* At least one quad in this Basic Block  *)
         StartBB(CurrentBB, Quad, CurrentlyInside) ;
         EndBB(CurrentBB, Quad) ;
         IF IsKillLocalVar(Quad)
         THEN
            (* end of procedure *)
            CurrentlyInside := FALSE
         END
      ELSIF CurrentBB#NIL
      THEN
         (* We have a Basic Block - therefore add quad to this Block  *)
         EndBB(CurrentBB, Quad) ;
         IF IsKillLocalVar(Quad)
         THEN
            (* end of procedure *)
            CurrentlyInside := FALSE
         END
      ELSIF IsPseudoQuad(Quad)
      THEN
         (* Add Quad to the Last BB since Pseudo Quads - compiler directives *)
         (* must not be thrown away.                                         *)
         EndBB(LastBB, Quad)
      ELSIF IsReturn(Quad) OR IsKillLocalVar(Quad)
      THEN
         (* we must leave the ReturnOp alone as it indictes end of procedure *)
         EndBB(LastBB, Quad) ;
         IF IsKillLocalVar(Quad)
         THEN
            (* end of procedure *)
            CurrentlyInside := FALSE
         END
      ELSE
         (* Chuck this Quad since it will never be reached by the processor *)
         SubQuad(Head, Quad)
      END ;
      LastQuadConditional := IsConditional(Quad) ;
      LastQuadCall := IsCall(Quad) ;
      LastQuadReturn := IsReturn(Quad) ;
      LastQuadMod := IsModFile(Quad) ;
      IF IsUnConditional(Quad)
      THEN
         LastBB := CurrentBB ;
         CurrentBB := NIL
      END ;
      Quad := GetNextQuad(Quad)
   END
END ConvertQuads2BasicBlock ;


(*
   ForeachBasicBlockDo - for each basic block call procedure, p.
*)

PROCEDURE ForeachBasicBlockDo (p: BasicBlockProc) ;
VAR
   b: PtrToBasicBlock ;
BEGIN
   IF HeadOfBB#NIL
   THEN
      b := HeadOfBB ;
      REPEAT
         WITH b^ DO
            p(StartQuad, EndQuad)
         END ;
         b := b^.Right
      UNTIL HeadOfBB=b
   END
END ForeachBasicBlockDo ;


(*
   StartBB - Initially fills a Basic Block, b, with a start quad Quad.
             The Basic Block is then added to the end of Basic Block list.
*)

PROCEDURE StartBB (b: PtrToBasicBlock; Quad: CARDINAL; In: BOOLEAN) ;
BEGIN
   WITH b^ DO
      StartQuad := Quad ;
      EndQuad := Quad ;
      CPUCycles := 0 ;
      InsideProc := In
   END ;
   Add(HeadOfBB, b)   (* Add b to the end of the Basic Block list *)
END StartBB ;


(*
   EndBB - Fills a Basic Block, b, with an end quad Quad.
*)

PROCEDURE EndBB (b: PtrToBasicBlock; Quad: CARDINAL) ;
BEGIN
   WITH b^ DO
      EndQuad := Quad ;
   END
END EndBB ;


(*
   Add adds a specified element to the end of a queue.
*)
 
PROCEDURE Add (VAR Head: PtrToBasicBlock;
               b : PtrToBasicBlock) ;
BEGIN
   IF Head=NIL
   THEN
      Head := b ;
      b^.Left := b ;
      b^.Right := b
   ELSE
      b^.Right := Head ;
      b^.Left := Head^.Left ;
      Head^.Left^.Right := b ;
      Head^.Left := b
   END
END Add ;

 
(*
   Sub deletes an element from the specified queue.
*)
 
PROCEDURE Sub (VAR Head: PtrToBasicBlock;
               b: PtrToBasicBlock) ;
BEGIN
   IF (b^.Right=Head) AND (b=Head)
   THEN
      Head := NIL
   ELSE
      IF Head=b
      THEN
         Head := Head^.Right
      END ;
      b^.Left^.Right := b^.Right ;
      b^.Right^.Left := b^.Left
   END
END Sub ;


PROCEDURE InitializeBasicBlock ;
BEGIN
   IF HeadOfBB#NIL
   THEN
      DestroyBasicBlocks
   END ;
   HeadOfBB := NIL
END InitializeBasicBlock ;


(*
   DisplayBasicBlocks - displays the basic block data structure.
*)

PROCEDURE DisplayBasicBlocks ;
VAR
   b: PtrToBasicBlock ;
BEGIN
   b := HeadOfBB ;
   WriteString('quadruples outside procedures') ; WriteLn ;
   IF b#NIL
   THEN
      REPEAT
         IF NOT b^.InsideProc
         THEN
            DisplayBlock(b)
         END ;
         b := b^.Right
      UNTIL b=HeadOfBB
   END ;
   b := HeadOfBB ;
   WriteString('quadruples inside procedures') ; WriteLn ;
   IF b#NIL
   THEN
      REPEAT
         IF b^.InsideProc
         THEN
            DisplayBlock(b)
         END ;
         b := b^.Right
      UNTIL b=HeadOfBB
   END
END DisplayBasicBlocks ;


PROCEDURE DisplayBlock (b: PtrToBasicBlock) ;
BEGIN
   WITH b^ DO
      WriteString(' start ') ; WriteCard(StartQuad, 6) ;
      WriteString(' end   ') ; WriteCard(EndQuad, 6) ;
      WriteString(' cycles') ; WriteCard(CPUCycles, 6) ; WriteLn
   END
END DisplayBlock ;


BEGIN
   FreeBB := NIL ;
   HeadOfBB := NIL
END M2BasicBlock.
