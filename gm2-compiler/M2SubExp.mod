(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE M2SubExp ;


FROM SymbolTable IMPORT NulSym, ModeOfAddr, GetMode, GetSymName, IsVar, IsTemporary,
                        MakeConstLit, IsModule, IsExported, GetScope, GetQuads, PushSize,
                        IsProcedure, GetVarScope, IsConst, IsConstLit, MakeConstVar, PushValue, PopValue,
                        IsValueSolved ;

FROM NameKey IMPORT WriteKey, MakeKey, NulName ;
FROM M2Error IMPORT InternalError, ErrorStringAt ;
FROM DynamicStrings IMPORT String, InitString ;
FROM StringConvert IMPORT stoi ;

FROM M2Constants IMPORT IsZero, IsOne, IsTwo, IsSame, MakeNewConstFromValue ;
FROM M2ALU IMPORT Addn, Multn, SetOr, SetAnd, Sub, DivTrunc, ModTrunc, DivFloor, ModFloor, SetSymmetricDifference, SetDifference ;
FROM M2Quads IMPORT QuadOperator, GetQuad, SubQuad, PutQuad, EraseQuad, GetNextQuad, DisplayQuad,
                    WriteOperator, IsOptimizeOn, QuadToTokenNo ;

FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT Write ;
FROM NumberIO IMPORT WriteCard, StrToCard, CardToStr ;
FROM SEnvironment IMPORT GetEnvironment ;
FROM StrLib IMPORT StrConCat ;

FROM M2Entity IMPORT Entity, InitEntities, HasLValue,
                     GetLatestEntity,
                     IsEntityClean, IsLValue,
                     MakeEntityDirty, MakeEntityClean,
                     GiveEntityIndex, GetEntityIndex, GetEntitySym, MakeNewEntity ;

FROM Lists IMPORT List, RemoveItemFromList, PutItemIntoList, NoOfItemsInList,
                  GetItemFromList, IsItemInList, InitList, KillList, IncludeItemIntoList ;


CONST
   MaxEntList  =  100 ;
   MaxNodes    = 1000 ;
   NulNode     =    0 ;
   MaxTrees    =  500 ;
   MaxQuads    =  500 ;     (* maximum no. of quads in a basic block *)

TYPE
   Nodes       = [0..MaxNodes] ;
   Trees       = [0..MaxTrees] ;

   (*
      the version of the variable might vary during a basic block as
      the variable might be altered, consider: a := b + c ; b := d ;
      We set the IndexNo to the NodeIndex for the SymId record and
      set the Node.IndexNo to the maximum operand IndexNo+1. Follow
      the rules outlined by Trembley & Sorenson P.624 of The Theory and
      Practice of Compiler Writing
   *)

   Node = RECORD
             IsLeaf   : BOOLEAN ;    (* is this a leaf node, true means left, right not used *)
             SymQuad  : CARDINAL ;
             EntList  : ARRAY [1..MaxEntList] OF Entity ;
             NoOfIds  : CARDINAL ;
             Op       : QuadOperator ;
             Left,
             Right    : Nodes ;
             IndexNo  : CARDINAL ;   (* max(left, right)+1 determines *)
          END ;                      (* execution order               *)

   ProcNode = PROCEDURE (Nodes, CARDINAL, CARDINAL) ;

   QuadState = RECORD
                  IsUsed: BOOLEAN ;
                  QuadNo: CARDINAL ;
               END ;

VAR
   NoOfNodes   : Nodes ;
   NodeList    : ARRAY [1..MaxNodes] OF Node ;
   NoOfQuads   : CARDINAL ;
   RemoveList  : ARRAY [1..MaxQuads] OF QuadState ;
   NoOfTrees   : CARDINAL ;
   TreeList    : ARRAY [1..MaxTrees] OF Nodes ;
   FreeList    : CARDINAL ;
   FunctNode   : Nodes ;
   AssertQuad  : CARDINAL ;
   Debugging   : BOOLEAN ;   (* do we want lots of debugging output?  *)


(* %%%FORWARD%%%
PROCEDURE RemoveAllOlderEntities (n: Nodes; e: Entity) ; FORWARD ;
PROCEDURE IsCommutative (op: QuadOperator) : BOOLEAN ; FORWARD ;
PROCEDURE MakeMove (q: CARDINAL; Start, End: CARDINAL;
                    op1, op3: CARDINAL) ; FORWARD ;
PROCEDURE IsUsedOutSide (Sym: CARDINAL; Start, End: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE CheckNeeded (l: List; n: Nodes; sym: CARDINAL; index: CARDINAL;
                       Start, End: CARDINAL) ; FORWARD ;
PROCEDURE ChooseCleanEntityForLabel (n: Nodes) ; FORWARD ;
PROCEDURE CheckNodesWereSorted (Start, End: CARDINAL) ; FORWARD ;
PROCEDURE CollectSymIfNotValue (tokenno: CARDINAL; sym: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE MakeQuadFromNode (q: CARDINAL; n: Nodes) ; FORWARD ;
PROCEDURE CheckEntList (n: Nodes; index: CARDINAL; Start, End: CARDINAL) ; FORWARD ;
PROCEDURE DisplayForest ; FORWARD ;
PROCEDURE MakeOpWithMakeNode (q: CARDINAL; Start, End: CARDINAL; op1: CARDINAL; op: QuadOperator; op2, op3: CARDINAL) ; FORWARD ;
PROCEDURE GetNodeIndexNo (node: Nodes) : CARDINAL ; FORWARD ;
PROCEDURE Flush (Start, End: CARDINAL) ; FORWARD ;
PROCEDURE FlushIndirection (Start, End: CARDINAL; sym: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE DisplayTree (root: Nodes; indent: CARDINAL) ; FORWARD ;
PROCEDURE AddDependent (n: Nodes; entity: Entity) ; FORWARD ;
PROCEDURE RemoveEntityFromEntList (entity: Entity; q: CARDINAL) ; FORWARD ;
PROCEDURE WriteNode (n: Nodes) ; FORWARD ;
PROCEDURE CreateTree (n: Nodes) ; FORWARD ;
PROCEDURE RemoveTree (n: CARDINAL) ; FORWARD ;
PROCEDURE MakeNode (q: CARDINAL;
                    entity: Entity; op: QuadOperator;
                    left, right: Nodes) : Nodes ; FORWARD ;
PROCEDURE MakeLeaf (entity: Entity) : Nodes ; FORWARD ;
PROCEDURE NewNode (q: CARDINAL;
                   entity: Entity; op: QuadOperator;
                   left, right: CARDINAL) : Nodes ; FORWARD ;
PROCEDURE NewLeaf (entity: Entity) : Nodes ; FORWARD ;
PROCEDURE IsNodeKnown (op: QuadOperator; left, right: Nodes) : Nodes ; FORWARD ;
   %%%FORWARD%%% *)



(*
   Max - 
*)

PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   InitForest - 
*)

PROCEDURE InitForest ;
BEGIN
   InitEntities ;
   NoOfNodes    := 0 ;
   NoOfQuads    := 0 ;
   NoOfTrees    := 0 ;
   FunctNode    := NulNode ;
   FreeList     := 1
END InitForest ;


(*
   FindNextUsedQuad - returns the next used index in the remove list.
*)

PROCEDURE FindNextUsedQuad (i: CARDINAL) : CARDINAL ;
BEGIN
   INC(i) ;
   WHILE i<=NoOfQuads DO
      IF RemoveList[i].IsUsed
      THEN
         RETURN( i )
      ELSE
         INC(i)
      END
   END ;
   RETURN( 0 )
END FindNextUsedQuad ;


(*
   FindNextUsedQuad - returns the next unused index in the remove list.
*)

PROCEDURE FindNextUnUsedQuad () : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   i := FreeList ;
   WHILE i<=NoOfQuads DO
      IF NOT RemoveList[i].IsUsed
      THEN
         FreeList := i ;
         RETURN( i )
      ELSE
         INC(i)
      END
   END ;
   InternalError('no spare quads available to flush dirty symbols?', __FILE__, __LINE__)
END FindNextUnUsedQuad ;


(*
   RemoveAllQuads - removes all quads in the RemoveList and initializes FreeList which
                    is the start of the list of free quadruples. (Used for flushing dirty symbols)
*)

PROCEDURE RemoveAllQuads ;
VAR
   op            : QuadOperator ;
   op1, op2, op3 : CARDINAL ;
   FreeList, i, j: CARDINAL ;
BEGIN
   IF Debugging
   THEN
      WriteString('CSE removing quads { ')
   END ;
   i := 1 ;
   WHILE i<=NoOfQuads DO
      IF NOT RemoveList[i].IsUsed
      THEN
         IF Debugging
         THEN
            WriteCard(RemoveList[i].QuadNo, 0) ; WriteString(', ')
         END ;
(*
         j := FindNextUsedQuad(i) ;
         IF j#0
         THEN
            GetQuad(RemoveList[j].QuadNo, op, op1, op2, op3) ;
            EraseQuad(RemoveList[j].QuadNo) ;
            EraseQuad(RemoveList[i].QuadNo) ;
            PutQuad(RemoveList[i].QuadNo, op, op1, op2, op3) ;
            RemoveList[i].IsUsed := TRUE ;
            RemoveList[j].IsUsed := FALSE
         END
*)
      END ;
      INC(i)
   END ;
   IF Debugging
   THEN
      WriteString(' }') ; WriteLn
   END
END RemoveAllQuads ;


(*
   RemoveQuad - adds an entry into the RemoveList
*)

PROCEDURE RemoveQuad (q: CARDINAL) ;
BEGIN
   IF q#0
   THEN
      IF NoOfQuads=MaxQuads
      THEN
         InternalError('increase MaxQuads', __FILE__, __LINE__)
      ELSE
         INC(NoOfQuads) ;
         WITH RemoveList[NoOfQuads] DO
            QuadNo := q ;
            IsUsed := FALSE
         END
      END
   END
END RemoveQuad ;


(*
   SaveQuad - saves a quad from being destroyed. It simply removes the
              quad from the RemoveList.
*)

PROCEDURE SaveQuad (q: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE i<=NoOfQuads DO
      IF RemoveList[i].QuadNo=q
      THEN
         RemoveList[i].IsUsed := TRUE
      END ;
      INC(i)
   END
END SaveQuad ;


(*
   GetSym - return the symbol attached to node, n.
            It uses the node label, ie EntList[1]
*)

PROCEDURE GetSym (n: Nodes) : CARDINAL ;
BEGIN
   IF n=NulNode
   THEN
      RETURN( NulSym )
   ELSE
      WITH NodeList[n] DO
         IF EntList[1]=NIL
         THEN
            ChooseCleanEntityForLabel(n)
         END ;
         RETURN( GetEntitySym(EntList[1]) )
      END
   END
END GetSym ;


(*
   GetSymOfChoice - return any clean symbol attached to node, n.
                    It prefers to return a constant, otherwise it returns
                    EntList[1].
*)

PROCEDURE GetSymOfChoice (n: Nodes) : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   IF n=NulNode
   THEN
      RETURN( NulSym )
   ELSE
      WITH NodeList[n] DO
         i := NoOfIds ;
         WHILE i>1 DO
            IF (EntList[i]#NIL) AND IsConstLit(GetEntitySym(EntList[i]))
            THEN
               RETURN( GetEntitySym(EntList[i]) )
            END ;
            DEC(i)
         END ;
         RETURN( GetSym(n) )
      END
   END
END GetSymOfChoice ;


(*
   CreateSubExpression - creates a subexpression which references node, k.
*)

PROCEDURE CreateSubExpression (entity: Entity; node: Nodes) : BOOLEAN ;
BEGIN
   WITH NodeList[node] DO
      IF Debugging
      THEN
         WriteString('common subexpression found between: ') ;
         WriteKey(GetSymName(GetEntitySym(entity))) ; Write('[') ; WriteCard(GetEntityIndex(entity), 0) ; WriteString('] and ') ;
         WriteKey(GetSymName(GetSym(node))) ; WriteLn
      END ;
      AddDependent(node, entity) ;
      RETURN( TRUE )
   END
END CreateSubExpression ;


(*
   FindLeaf - attempts to find a leaf and returns the leaf number.
              It searches all dependents.
*)

PROCEDURE FindLeaf (entity: Entity) : Nodes ;
VAR
   n   : Nodes ;
   i, j: CARDINAL ;
BEGIN
   n := NoOfNodes ;
   WHILE n>0 DO
      WITH NodeList[n] DO
         j := NoOfIds ;
         WHILE j>0 DO
            IF EntList[j]=entity
            THEN
               RETURN( n )
            END ;
            DEC(j)
         END
      END ;
      DEC(n)
   END ;
   RETURN( NulNode )
END FindLeaf ;


(*
   MakeLeaf - searches for a node, entity, if found it is returned.
              Otherwise a leaf is created.
*)

PROCEDURE MakeLeaf (entity: Entity) : Nodes ;
VAR
   l: CARDINAL ;
BEGIN
   l := FindLeaf(entity) ;
   IF l=0
   THEN
      RETURN( NewLeaf(entity) )
   ELSE
      RETURN( l )
   END
END MakeLeaf ;


(*
   MakeOp - 
*)

PROCEDURE MakeOp (q: CARDINAL; new, old: Entity; n: Nodes) ;
VAR
   index: CARDINAL ;
BEGIN
   RemoveQuad(q) ;
   IF new#old
   THEN
      RemoveEntityFromEntList(old, q)
   END ;
   AddDependent(n, new)
END MakeOp ;


(*
   PreviousQuad - returns the previous quad q-1.
*)

PROCEDURE PreviousQuad (q, start: CARDINAL) : CARDINAL ;
BEGIN
   IF q=start
   THEN
      RETURN( q )
   ELSE
      WHILE GetNextQuad(start)#q DO
         start := GetNextQuad(start)
      END ;
      RETURN( start )
   END
END PreviousQuad ;


(*
   MakeReturnValue - flush existing CSE tree, once ReturnValue is encountered
                     the basic block finishes. But op1 does is not calculated from op3
                     so it must NOT be placed into the tree otherwise previous calculations
                     of op1 will be ignored.
*)

PROCEDURE MakeReturnValue (q: CARDINAL; Start, End: CARDINAL; op1, op3: CARDINAL) ;
BEGIN
   EraseQuad(q) ;
   PutQuad(q, ReturnValueOp, op1, NulSym, op3) ;
   SaveQuad(q) ;
   Flush(Start, PreviousQuad(q, Start))
END MakeReturnValue ;


(*
   MakeFunctValue - this quadruple must come straight after the CallOp.
                    We can do this if we have an index to this node and ensure that
                    this node is emited first from the forest.
*)

PROCEDURE MakeFunctValue (q: CARDINAL; Start, End: CARDINAL; op1, op3: CARDINAL) ;
VAR
   e: Entity ;
BEGIN
   IF FunctNode=NulNode
   THEN
      e := MakeNewEntity(QuadToTokenNo(q), op1, FALSE, VAL(CARDINAL, NoOfNodes)) ;
      FunctNode := MakeNode(q, e, FunctValueOp, NulNode,
                            MakeLeaf(GetLatestEntity(QuadToTokenNo(q), op3, FALSE, VAL(CARDINAL, NoOfNodes)))) ;
      MakeOp(q, e, e, FunctNode)
   ELSE
      InternalError('only expect one FunctValueOp per basic block', __FILE__, __LINE__)
   END
END MakeFunctValue ;


(*
   MakeBec - 
*)

PROCEDURE MakeBec (q: CARDINAL; Start, End: CARDINAL;
                   op1, op3: CARDINAL) ;
BEGIN
   MakeMove(q, Start, End, op1, op3)
END MakeBec ;


(*
   MakeUnbounded - 
*)

PROCEDURE MakeUnbounded (q: CARDINAL; Start, End: CARDINAL;
                         op1, op3: CARDINAL) ;
VAR
   l, k: CARDINAL ;
BEGIN
   (* safty first *)
   Flush(Start, PreviousQuad(q, Start)) ;
   MakeOpWithMakeNode(q, Start, End, op1, UnboundedOp, NulSym, op3)
(*
   IF (GetMode(op1)=LeftValue) OR (GetMode(op3)=LeftValue)
   THEN
      (* some indirection will occur *)
      Flush(Start, PreviousQuad(q, Start)) ;
      MakeOpWithMakeNode(q, Start, End, op1, UnboundedOp, NulSym, op3)
   ELSE
      MakeMove(q, Start, End, op1, op3) ;
      IF Debugging
      THEN
         ; WriteString('------------------------------------------') ; WriteLn ;
         DisplayTree(MakeLeaf(GetLatestEntity(QuadToTokenNo(q), op3, FALSE)), 0)
         ; WriteString('------------------------------------------') ; WriteLn ;
      END
   END
*)
END MakeUnbounded ;


(*
   MakeIndrX - the IndrX quadruple behaves as follows:

               q    IndrX    op1   op2    op3             op1 := *op3      ;  Mem[op1] := Mem[Mem[op3]]    (TYPE op2)
*)

PROCEDURE MakeIndrX (q: CARDINAL; Start, End: CARDINAL;
                     op1, op2, op3: CARDINAL) ;
VAR
   l, k: CARDINAL ;
BEGIN
   (*
      we might be able to do better than this as we are copying indirect and
      not storing indirect...

      Sym1<X>   IndrX   Sym2<I>     Meaning     Mem[Sym1<I>] := Mem[constant]
      Sym1<X>   IndrX   Sym2<X>     meaning     Mem[Sym1<I>] := Mem[Mem[Sym2<I>]]

      for the moment we will safely flush
   *)
   (* it copies indirect via op3 *)
   (* some indirection will occur *)
   EraseQuad(q) ;
   PutQuad(q, IndrXOp, op1, op2, op3) ;
   SaveQuad(q) ;
   Flush(Start, PreviousQuad(q, Start)) ;
   IF Debugging
   THEN
      ; WriteString('------------------------------------------') ; WriteLn ;
      DisplayTree(MakeLeaf(GetLatestEntity(QuadToTokenNo(q), op3, FALSE, VAL(CARDINAL, NoOfNodes))), 0)
      ; WriteString('------------------------------------------') ; WriteLn ;
   END
END MakeIndrX ;


(*
   MakeXIndr - the IndrX quadruple behaves as follows:

               q    XIndr    op1   op2    op3             *op1 := op3      ;  Mem[Mem[op1]] := Mem[op3]      (TYPE op2)
*)

PROCEDURE MakeXIndr (q: CARDINAL; Start, End: CARDINAL;
                     op1, op2, op3: CARDINAL) ;
VAR
   l, k: CARDINAL ;
BEGIN
   (*
      we might be able to do better than this as we are copying indirect and
      not storing indirect...

      Sym1<I>   XIndr   Sym2<X>     Meaning     Mem[constant]     := Mem[Sym2<I>]
      Sym1<X>   XIndr   Sym2<X>     Meaning     Mem[Mem[Sym1<I>]] := Mem[Sym2<I>]

      for the moment we will safely flush
   *)
   (* it copies indirect via op1 *)
   (* some indirection will occur *)
   EraseQuad(q) ;
   PutQuad(q, XIndrOp, op1, op2, op3) ;
   SaveQuad(q) ;
   Flush(Start, PreviousQuad(q, Start)) ;
   IF Debugging
   THEN
      ; WriteString('------------------------------------------') ; WriteLn ;
      DisplayTree(MakeLeaf(GetLatestEntity(QuadToTokenNo(q), op3, FALSE, VAL(CARDINAL, NoOfNodes))), 0)
      ; WriteString('------------------------------------------') ; WriteLn ;
   END
END MakeXIndr ;


(*
   MakeCall - 
*)

PROCEDURE MakeCall (q: CARDINAL; Start, End: CARDINAL;
                    op1, op2, op3: CARDINAL) ;
BEGIN
   IF IsVar(op3)
   THEN
      (* an indirect call, therefore flush forest *)
      EraseQuad(q) ;
      PutQuad(q, CallOp, op1, op2, op3) ;
      SaveQuad(q) ;
      Flush(Start, PreviousQuad(q, Start))
   END
END MakeCall ;


(*
   MakeAddr - 


   Addr Operator  - briefly
------------------------------------------------------------------------------
   Yields the address of a variable - need to add the frame pointer if
   a variable is local to a procedure.

   Sym1<X>   Addr   Sym3<X>     meaning     Mem[Sym1<I>] := Sym3<I>
*)

PROCEDURE MakeAddr (q: CARDINAL; Start, End: CARDINAL;
                    op1, op3: CARDINAL) ;
VAR
   new, old: Entity ;
   t       : CARDINAL ;
BEGIN
   t := QuadToTokenNo(q) ;
   (* addr can operate on different sizes as it calculates the address of a symbol ADR(record) *)
   old := GetLatestEntity(t, op1, FALSE, VAL(CARDINAL, NoOfNodes)) ;
   new := MakeNewEntity(t, op1, FALSE, VAL(CARDINAL, NoOfNodes)) ;
   (* it calculates the address of op3, which might be a subexpression *)
   MakeOp(q, new, old,
          MakeNode(q, new, AddrOp, NulNode,
                   MakeLeaf(GetLatestEntity(t, op3, TRUE, VAL(CARDINAL, NoOfNodes))))
         )
END MakeAddr ;


(*
   MakeMove - moves op3 -> op1 it achieves this by adding op1 to op3 entity list.
              NOTE that this can only be called if op1 and op3 are to ignore their modes.
*)

PROCEDURE MakeMove (q: CARDINAL; Start, End: CARDINAL;
                    op1, op3: CARDINAL) ;
VAR
   new, old: Entity ;
   t       : CARDINAL ;
BEGIN
   (* just like a copy *)
   RemoveQuad(q) ;
   IF op1#op3
   THEN
      t := QuadToTokenNo(q) ;
      RemoveEntityFromEntList(GetLatestEntity(t, op1, FALSE, VAL(CARDINAL, NoOfNodes)), q) ;
      AddDependent(MakeLeaf(GetLatestEntity(t, op3, FALSE, VAL(CARDINAL, NoOfNodes))),
                   MakeNewEntity(t, op1, FALSE, VAL(CARDINAL, NoOfNodes))) ;
      IF Debugging
      THEN
         DisplayForest
      END
   END
END MakeMove ;


(*
   MakeParam - remember that the ParamOp does not take any notice of ModeOfAddr.
*)

PROCEDURE MakeParam (q: CARDINAL;
                     Start, End: CARDINAL;
                     op1, op2, op3: CARDINAL) ;
BEGIN
   IF FALSE
   THEN
      (* since ParamOp rightly ignores ModeOfAddr we can use the following method *)
      EraseQuad(q) ;
      (* can only perform this code when we actually place ParamOp's into the forest - to take advantage of the DAG *)
      PutQuad(q, ParamOp, op1, op2, GetSym(MakeLeaf(GetLatestEntity(QuadToTokenNo(q), op3, FALSE, VAL(CARDINAL, NoOfNodes))))) ;
      SaveQuad(q)
   ELSE
      (* this works, but is not very clever *)
      (* safty first *)
      EraseQuad(q) ;
      PutQuad(q, ParamOp, op1, op2, op3) ;
      SaveQuad(q) ;
      Flush(Start, PreviousQuad(q, Start))
   END
END MakeParam ;


(*
   MakeConvert - convert takes into account the symbols mode, so we must flush.
*)

PROCEDURE MakeConvert (q: CARDINAL; Start, End: CARDINAL;
                       op1, op2, op3: CARDINAL) ;
VAR
   l, k: CARDINAL ;
BEGIN
   (* safty first *)
   EraseQuad(q) ;
   PutQuad(q, ConvertOp, op1, op2, op3) ;
   SaveQuad(q) ;
   Flush(Start, PreviousQuad(q, Start))
END MakeConvert ;


(*
   MakeCoerce - coarce takes into account the symbols mode, so we must flush
                if a value mode is present in op1 or op3.
*)

PROCEDURE MakeCoerce (q: CARDINAL; Start, End: CARDINAL;
                      op1, op2, op3: CARDINAL) ;
VAR
   l, k: CARDINAL ;
BEGIN
   (* safty first *)
   EraseQuad(q) ;
   PutQuad(q, CoerceOp, op1, op2, op3) ;
   SaveQuad(q) ;
   Flush(Start, PreviousQuad(q, Start))
END MakeCoerce ;


(*
   MakeCast - cast takes into account the symbols mode, so we must flush
              if a value mode is present in op1 or op3.
*)

PROCEDURE MakeCast (q: CARDINAL; Start, End: CARDINAL;
                      op1, op2, op3: CARDINAL) ;
VAR
   l, k: CARDINAL ;
BEGIN
   (* safty first *)
   EraseQuad(q) ;
   PutQuad(q, CastOp, op1, op2, op3) ;
   SaveQuad(q) ;
   Flush(Start, PreviousQuad(q, Start))
END MakeCast ;


(*
   MakeNode - 
*)

PROCEDURE MakeNode (q: CARDINAL;
                    entity: Entity; op: QuadOperator;
                    left, right: Nodes) : Nodes ;
VAR
   r: BOOLEAN ;
   k: Nodes ;
BEGIN
   k := IsNodeKnown(op, left, right) ;
   IF k=0
   THEN
      k := NewNode(q, entity, op, left, right) ;
      RETURN( k )
   ELSE
      (* common sub expression found *)
      r := CreateSubExpression(entity, k) ;
      RETURN( k )
   END
END MakeNode ;


(*
   GetNodeIndexNo - returns the IndexNo of a node
*)

PROCEDURE GetNodeIndexNo (node: Nodes) : CARDINAL ;
BEGIN
   IF node=NulNode
   THEN
      RETURN( 0 )
   ELSE
      RETURN( NodeList[node].IndexNo )
   END
END GetNodeIndexNo ;


(*
   NewNode - 
*)

PROCEDURE NewNode (q: CARDINAL;
                   entity: Entity; op: QuadOperator;
                   left, right: Nodes) : Nodes ;
VAR
   k: Nodes ;
BEGIN
   k := NewLeaf(entity) ;
   WITH NodeList[k] DO
      IsLeaf            := FALSE ;
      NoOfIds           := 1 ;
      EntList[1]        := entity ;
      SymQuad           := q ;
      Op                := op ;
      Left              := left ;
      Right             := right ;
      IndexNo           := 0 ;
   END ;
   MakeEntityDirty(entity) ;
   CreateTree(k) ;
   RemoveTree(left) ;
   RemoveTree(right) ;
   RETURN( k )
END NewNode ;


(*
   RemoveTree - removes a tree reference to, n.
*)

PROCEDURE RemoveTree (n: Nodes) ;
VAR
   i: CARDINAL ;
BEGIN
   IF n#0
   THEN
      i := 1 ;
      WHILE i<=NoOfTrees DO
         IF TreeList[i]=n
         THEN
            TreeList[i] := NulNode
         END ;
         INC(i)
      END
   END
END RemoveTree ;


(*
   CreateTree - creates a new tree referencing node, n.
*)

PROCEDURE CreateTree (n: Nodes) ;
VAR
   i: CARDINAL ;
BEGIN
   IF n#0
   THEN
      IF NoOfTrees=MaxTrees
      THEN
         InternalError('increase MaxTrees', __FILE__, __LINE__)
      ELSE
         i := 1 ;
         WHILE i<=NoOfTrees DO
            IF TreeList[i]=n
            THEN
               RETURN
            END ;
            INC(i)
         END ;
         INC(NoOfTrees) ;
         TreeList[NoOfTrees] := n
      END
   END
END CreateTree ;


(*
   RemoveEntityFromEntList - removes, entity, from all EntLists of nodes.
                             Providing that it is not at quad, q, and the node
                             is an FALSE.
*)

PROCEDURE RemoveEntityFromEntList (entity: Entity; q: CARDINAL) ;
VAR
   i, j, k: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE i<=NoOfNodes DO
      WITH NodeList[i] DO
         IF IsLeaf
         THEN
            j := 2
         ELSE
            j := 1
         END ;
         WHILE j<=NoOfIds DO
            IF EntList[j]=entity
            THEN
               EntList[j] := NIL
            END ;
            INC(j)
         END ;
         (* consistancy check *)
         j := 1 ;
         k := 1 ;
         WHILE j<=NoOfIds DO
            IF EntList[j]=NIL
            THEN
               INC(k)
            END ;
            INC(j)
         END
      END ;
      IF k=j
      THEN
         IF Debugging
         THEN
            WriteString(' all entities are now nul during Q') ; WriteCard(q, 0) ;
            WriteString(' removing: ') ; WriteKey(GetSymName(GetEntitySym(entity))) ;
            Write('[') ; WriteCard(GetEntityIndex(entity), 0) ; Write(']') ; WriteLn
         END ;
         InternalError('all entities are now null', __FILE__, __LINE__)
      END ;
      INC(i)
   END
END RemoveEntityFromEntList ;


(*
   NewLeaf - creates a new node for symbol, id.
*)

PROCEDURE NewLeaf (entity: Entity) : Nodes ;
BEGIN
   IF NoOfNodes=MaxNodes
   THEN
      InternalError('compiler has run out of node, increase MaxNodes',
                    __FILE__, __LINE__)
   ELSE
      INC(NoOfNodes) ;
      WITH NodeList[NoOfNodes] DO
         IsLeaf            := TRUE ;
         EntList[1]        := entity ;
         NoOfIds           := 1 ;
         SymQuad           := 0 ;
         IndexNo           := 0
      END ;
      CreateTree(NoOfNodes) ;
      MakeEntityClean(entity) ;
      RETURN( NoOfNodes )
   END
END NewLeaf ;


(*
   IsNodeKnown - if symbol, id, is known then the node number is
                 returned, otherwise, 0 is returned.
*)

PROCEDURE IsNodeKnown (op: QuadOperator; left, right: Nodes) : Nodes ;
VAR
   n: Nodes ;
   i: CARDINAL ;
BEGIN
   n := NoOfNodes ;
   WHILE n>0 DO
      WITH NodeList[n] DO
         IF NOT IsLeaf
         THEN
            IF (op=Op) AND (((Left=left) AND (Right=right)) OR
                            IsCommutative(op) AND (Left=right) AND (Right=left))
            THEN
               RETURN( n )
            END
         END
      END ;
      DEC(n)
   END ;
   RETURN( NulNode )
END IsNodeKnown ;


(*
   AddDependent - places an entity, entity, into the label list node, n.
*)

PROCEDURE AddDependent (n: Nodes; entity: Entity) ;
VAR
   i: CARDINAL ;
BEGIN
   WITH NodeList[n] DO
      i := 1 ;
      WHILE i<=NoOfIds DO
         IF EntList[i]=entity
         THEN
            MakeEntityDirty(entity) ;
            RETURN
         ELSIF EntList[i]=NIL
         THEN
            EntList[i] := entity ;
            MakeEntityDirty(entity) ;
            RETURN
         END ;
         INC(i)
      END ;
      INC(NoOfIds) ;
      IF NoOfIds<=MaxEntList
      THEN
         EntList[NoOfIds] := entity ;
         MakeEntityDirty(entity)
      ELSE
         InternalError('increase MaxEntList', __FILE__, __LINE__)
      END
   END
END AddDependent ;


(*
   MakeNewConstant - given two symbols, l, r, and an operator, return
                     a new constant containing the value op(l, r).
*)

PROCEDURE MakeNewConstant (tn: CARDINAL; op: QuadOperator; l, r: CARDINAL) : CARDINAL ;
BEGIN
   PushValue(l) ;
   PushValue(r) ;
   CASE op OF

   AddOp              : Addn |
   MultOp             : Multn |
   SubOp              : Sub |
   DivTruncOp         : DivTrunc |
   ModTruncOp         : ModTrunc |
   DivFloorOp         : DivFloor |
   ModFloorOp         : ModFloor |
   LogicalOrOp        : SetOr(tn) |
   LogicalAndOp       : SetAnd(tn) |
   LogicalXorOp       : SetSymmetricDifference(tn)

   ELSE
      InternalError('quadruple operator not known as a constant folding operator', __FILE__, __LINE__)
   END ;
   RETURN( MakeNewConstFromValue(tn) )
END MakeNewConstant ;


(*
   IsAliasedTo - returns TRUE if symbol, s1, is aliased to symbol, s2.
*)

PROCEDURE IsAliasedTo (tokenno: CARDINAL; s1, s2: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          MakeLeaf(GetLatestEntity(tokenno, s1, FALSE, VAL(CARDINAL, NoOfNodes))) =
          MakeLeaf(GetLatestEntity(tokenno, s2, FALSE, VAL(CARDINAL, NoOfNodes)))
         )
END IsAliasedTo ;


(*
   IsAliasedToZero - returns TRUE if symbol, s, is aliased to zero.
*)

PROCEDURE IsAliasedToZero (tokenno: CARDINAL; s: CARDINAL) : BOOLEAN ;
VAR
   i: CARDINAL ;
   n: Nodes ;
BEGIN
   n := MakeLeaf(GetLatestEntity(tokenno, s, FALSE, VAL(CARDINAL, NoOfNodes))) ;
   WITH NodeList[n] DO
      i := 1 ;
      WHILE i<=NoOfIds DO
         IF IsZero(tokenno, GetEntitySym(EntList[i]))
         THEN
            RETURN( TRUE )
         END ;
         INC(i)
      END
   END ;
   RETURN( FALSE )
END IsAliasedToZero ;


(*
   IsAliasedToOne - returns TRUE if symbol, s, is aliased to one.
*)

PROCEDURE IsAliasedToOne (tokenno: CARDINAL; s: CARDINAL) : BOOLEAN ;
VAR
   i: CARDINAL ;
   n: Nodes ;
BEGIN
   n := MakeLeaf(GetLatestEntity(tokenno, s, FALSE, VAL(CARDINAL, NoOfNodes))) ;
   WITH NodeList[n] DO
      i := 1 ;
      WHILE i<=NoOfIds DO
         IF IsOne(tokenno, GetEntitySym(EntList[i]))
         THEN
            RETURN( TRUE )
         END ;
         INC(i)
      END
   END ;
   RETURN( FALSE )
END IsAliasedToOne ;


(*
   IsAliasedToTwo - returns TRUE if symbol, s, is aliased to one.
*)

PROCEDURE IsAliasedToTwo (tokenno: CARDINAL; s: CARDINAL) : BOOLEAN ;
VAR
   i: CARDINAL ;
   n: Nodes ;
BEGIN
   n := MakeLeaf(GetLatestEntity(tokenno, s, FALSE, VAL(CARDINAL, NoOfNodes))) ;
   WITH NodeList[n] DO
      i := 1 ;
      WHILE i<=NoOfIds DO
         IF IsTwo(tokenno, GetEntitySym(EntList[i]))
         THEN
            RETURN( TRUE )
         END ;
         INC(i)
      END
   END ;
   RETURN( FALSE )
END IsAliasedToTwo ;


(*
   CannotBeCalculated - returns TRUE if the expression op(op2, op3) cannot be calculated (is not constant).
                        If it can be calculated then FALSE is returned and we perform the operation and
                        use an assignment rather than a binary operator.
*)

PROCEDURE CannotBeCalculated (q: CARDINAL; Start, End: CARDINAL;
                              op1: CARDINAL; op: QuadOperator; op2, op3: CARDINAL) : BOOLEAN ;
BEGIN
   IF (op2#NulSym) AND (op3#NulSym) AND IsConst(op2) AND IsConst(op3)
   THEN
      (* we do not always perform folding of constants in M2Optimize (GNU Modula-2, for example,
         performs constant folding later on. *)
      (*
         IF IsConst(op1)
         THEN
            InternalError('expected constants to have been folded inside M2Optimize', __FILE__, __LINE__)
         END ;
      *)
      IF IsValueSolved(op2) AND IsValueSolved(op3)
      THEN
         MakeMove(q, Start, End, op1, MakeNewConstant(QuadToTokenNo(q), op, op2, op3)) ;
         RETURN( FALSE )
      END
   END ;
   RETURN( TRUE )
END CannotBeCalculated ;


(*
   MakeOpWithMakeNode - 
*)

PROCEDURE MakeOpWithMakeNode (q: CARDINAL; Start, End: CARDINAL; op1: CARDINAL; op: QuadOperator; op2, op3: CARDINAL) ;
VAR
   t       : CARDINAL ;
   n2, n3  : Nodes ;
   new, old: Entity ;
BEGIN
   IF CannotBeCalculated(q, Start, End, op1, op, op2, op3)
   THEN
      t := QuadToTokenNo(q) ;
      IF op2=NulSym
      THEN
         n3 := MakeLeaf(GetLatestEntity(t, op3, FALSE, VAL(CARDINAL, NoOfNodes))) ;
         old := GetLatestEntity(t, op1, FALSE, VAL(CARDINAL, NoOfNodes)) ;
         new := MakeNewEntity(t, op1, FALSE, VAL(CARDINAL, NoOfNodes)) ;
         MakeOp(q, new, old, MakeNode(q, new, op, NulNode, n3))
      ELSIF op3=NulSym
      THEN
         InternalError('not expecting op3 to be nul', __FILE__, __LINE__)
      ELSE
         n2 := MakeLeaf(GetLatestEntity(t, op2, FALSE, VAL(CARDINAL, NoOfNodes))) ;
         n3 := MakeLeaf(GetLatestEntity(t, op3, FALSE, VAL(CARDINAL, NoOfNodes))) ;
         old := GetLatestEntity(t, op1, FALSE, VAL(CARDINAL, NoOfNodes)) ;
         new := MakeNewEntity(t, op1, FALSE, VAL(CARDINAL, NoOfNodes)) ;
         MakeOp(q, new, old, MakeNode(q, new, op, n2, n3))
      END
   END
END MakeOpWithMakeNode ;


(*
   MakeMult - 
*)

PROCEDURE MakeMult (q, Start, End: CARDINAL;
                    op1, op2, op3: CARDINAL) ;
VAR
   t       : CARDINAL ;
   new, old: Entity ;
BEGIN
   t := QuadToTokenNo(q) ;
   IF IsAliasedToOne(t, op2)
   THEN
      MakeMove(q, Start, End, op1, op3)
   ELSIF IsAliasedToOne(t, op3)
   THEN
      MakeMove(q, Start, End, op1, op2)
   ELSIF IsAliasedToZero(t, op2)
   THEN
      MakeMove(q, Start, End, op1, op2)
   ELSIF IsAliasedToZero(t, op3)
   THEN
      MakeMove(q, Start, End, op1, op3)
   ELSIF IsAliasedToTwo(t, op2)
   THEN
      (*
         always simpler to add two numbers than multiply by 2. On the pentium a multiply takes
         the same time as an add, but it requires specific registers, so we still win by using
         an add.
      *)
      MakeOpWithMakeNode(q, Start, End, op1, AddOp, op3, op3)
   ELSIF IsAliasedToTwo(t, op3)
   THEN
      MakeOpWithMakeNode(q, Start, End, op1, AddOp, op2, op2)
   ELSE
      MakeOpWithMakeNode(q, Start, End, op1, MultOp, op2, op3)
   END      
END MakeMult ;


(*
   MakeDiv - 
*)

PROCEDURE MakeDiv (q, Start, End: CARDINAL;
                   op1, op2, op3: CARDINAL) ;
VAR
   t       : CARDINAL ;
   new, old: Entity ;
BEGIN
   t := QuadToTokenNo(q) ;
   IF IsAliasedToOne(t, op3)
   THEN
      MakeMove(q, Start, End, op1, op2)
   ELSIF IsAliasedTo(t, op2, op3)
   THEN
      MakeMove(q, Start, End, op1, MakeConstLit(MakeKey('1')))
   ELSIF IsAliasedToZero(t, op3)
   THEN
      ErrorStringAt(InitString('division by zero in source file (found by constant propergation and common subexpression elimination)'), t)
   ELSE
      MakeOpWithMakeNode(q, Start, End, op1, MultOp, op2, op3)
   END      
END MakeDiv ;


(*
   MakeSub - 
*)

PROCEDURE MakeSub (q, Start, End: CARDINAL;
                   op1, op2, op3: CARDINAL) ;
VAR
   t: CARDINAL ;
BEGIN
   (* later on should test for op2=0 or op3=0 and alter to copy *)
   t := QuadToTokenNo(q) ;
   IF IsAliasedToZero(t, op3)
   THEN
      MakeMove(q, Start, End, op1, op2)
   ELSIF IsAliasedTo(t, op2, op3)
   THEN
      MakeMove(q, Start, End, op1, MakeConstLit(MakeKey('0')))
   ELSE
      MakeOpWithMakeNode(q, Start, End, op1, SubOp, op2, op3)
   END      
END MakeSub ;


(*
   MakeAdd - 
*)

PROCEDURE MakeAdd (q, Start, End: CARDINAL;
                   op1, op2, op3: CARDINAL) ;
VAR
   t: CARDINAL ;
BEGIN
   t := QuadToTokenNo(q) ;
   IF IsAliasedToZero(t, op2)
   THEN
      MakeMove(q, Start, End, op1, op3)
   ELSIF IsAliasedToZero(t, op3)
   THEN
      MakeMove(q, Start, End, op1, op2)
   ELSE
      MakeOpWithMakeNode(q, Start, End, op1, AddOp, op2, op3)
   END      
END MakeAdd ;


(*
   MakeArray - makes a node in the forest which contains the ArrayOp
*)

PROCEDURE MakeArray (q, Start, End: CARDINAL;
                     op1, op2, op3: CARDINAL) ;
BEGIN
   EraseQuad(q) ;
   PutQuad(q, ArrayOp, op1, op2, op3) ;
   SaveQuad(q) ;
   Flush(Start, PreviousQuad(q, Start))
END MakeArray ;


(*
   MakeElementSize - makes a node in the forest which contains the ElementSizeOp
*)

PROCEDURE MakeElementSize (q, Start, End: CARDINAL;
                           op1, op2, op3: CARDINAL) ;
BEGIN
   (* if we reach here it is because the array type has not yet been
      declared to GCC, thus we cannot optimize any further expressions
      based on this result. We therefore flush the current forest
      and let GCC handle these subexpressions once we have declared
      the array type.
   *)
   EraseQuad(q) ;
   PutQuad(q, ElementSizeOp, op1, op2, op3) ;
   SaveQuad(q) ;
   Flush(Start, PreviousQuad(q, Start))
END MakeElementSize ;


(*
   MakeConditional - uses the DAG to find the correct quad operands for q.
                     We always know that this quad must be the last.
*)

PROCEDURE MakeConditional (q: CARDINAL; Start, End: CARDINAL;
                           op: QuadOperator; op1, op2, op3: CARDINAL) ;
VAR
   t     : CARDINAL ;
   o1, o2: CARDINAL ;
BEGIN
   (*
      we know we must flush a conditional, but we can take advantage of
      the nodes (aliases) before they are destroyed
   *)

   t  := QuadToTokenNo(q) ;
   o1 := CollectSymIfNotValue(t, op1) ;
   o2 := CollectSymIfNotValue(t, op2) ;
   SaveQuad(q) ;
   EraseQuad(q) ;
   PutQuad(q, op, o1, o2, op3) ;
   Flush(Start, PreviousQuad(q, Start))
END MakeConditional ;


(*
   PlaceQuadIntoForest - places quadruple, q, into the forest.
*)

PROCEDURE PlaceQuadIntoForest (q: CARDINAL; Start, End: CARDINAL) ;
VAR
   k            : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
   new, old     : Entity ;
BEGIN
   GetQuad(q, op, op1, op2, op3) ;
   CASE op OF

   InclOp,
   ExclOp,
   HighOp,
   OffsetOp,
   NegateOp,
   SizeOp            : MakeOpWithMakeNode(q, Start, End, op1, op, op2, op3) |

   FunctValueOp      : MakeFunctValue(q, Start, End, op1, op3) |
   ReturnValueOp     : MakeReturnValue(q, Start, End, op1, op3) |

   BecomesOp         : MakeBec(q, Start, End, op1, op3) |
   IndrXOp           : MakeIndrX(q, Start, End, op1, op2, op3) |
   XIndrOp           : MakeXIndr(q, Start, End, op1, op2, op3) |
   AddrOp            : MakeAddr(q, Start, End, op1, op3) |
   UnboundedOp       : MakeUnbounded(q, Start, End, op1, op3) |

   ElementSizeOp     : MakeElementSize(q, Start, End, op1, op2, op3) |

   IfInOp,
   IfNotInOp,
   IfNotEquOp,
   IfEquOp,
   IfLessOp,
   IfGreOp,
   IfLessEquOp,
   IfGreEquOp        : MakeConditional(q, Start, End, op, op1, op2, op3) |

   InlineOp,
   LineNumberOp,
   GotoOp            : (* WriteCard(Operand3, 4) *) |

   StartDefFileOp,
   StartModFileOp,
   EndFileOp,
   FinallyStartOp,
   FinallyEndOp,
   InitEndOp,
   InitStartOp       : (* WriteKey(GetSymName(Operand3)) *) |

   CallOp            : MakeCall(q, Start, End, op1, op2, op3) |

   ReturnOp,
   NewLocalVarOp,
   KillLocalVarOp    : (* WriteOperand(Operand3) *) |

   CoerceOp          : MakeCoerce(q, Start, End, op1, op2, op3) |
   ConvertOp         : MakeConvert(q, Start, End, op1, op2, op3) |
   CastOp            : MakeCast(q, Start, End, op1, op2, op3) |
   ParamOp           : MakeParam(q, Start, End, op1, op2, op3) |
   AddOp             : MakeAdd(q, Start, End, op1, op2, op3) |
   SubOp             : MakeSub(q, Start, End, op1, op2, op3) |
   MultOp            : MakeMult(q, Start, End, op1, op2, op3) |

   ArrayOp           : MakeArray(q, Start, End, op1, op2, op3) |

   LogicalOrOp,
   LogicalAndOp,
   LogicalXorOp,
   LogicalDiffOp,
   LogicalShiftOp,
   LogicalRotateOp,
   ModTruncOp,
   DivTruncOp,
   ModFloorOp,
   DivFloorOp        : MakeOpWithMakeNode(q, Start, End, op1, op, op2, op3) |

   DummyOp,
   CodeOnOp,
   CodeOffOp,
   ProfileOnOp,
   ProfileOffOp,
   OptimizeOnOp,
   OptimizeOffOp     :

   ELSE
      InternalError('quadruple not recognised', __FILE__, __LINE__)
   END
END PlaceQuadIntoForest ;


(*
   BuildForest - builds a forest starting at quadruple, Start, and
                 ending at, End.
*)

PROCEDURE BuildForest (Start, End: CARDINAL) ;
VAR
   q: CARDINAL ;
BEGIN
   q := Start ;
   REPEAT
      IF q#0
      THEN
         PlaceQuadIntoForest(q, Start, End) ;
         q := GetNextQuad(q)
      END
   UNTIL (q=0) OR (q>End)
END BuildForest ;


(*
   WriteNSpaces - 
*)

PROCEDURE WriteNSpaces (n: CARDINAL) ;
BEGIN
   WHILE n>0 DO
      Write(' ') ;
      DEC(n)
   END
END WriteNSpaces ;


(*
   DisplayForest - displays all the trees in the forest.
*)

PROCEDURE DisplayForest ;
VAR
   i: CARDINAL ;
BEGIN
   WriteString('^^^^^^^^^^^^^^^^ f o r e s t ^^^^^^^^^^^^^^') ; WriteLn ;
   i := NoOfTrees ;
   WHILE i>0 DO
      DisplayTree(TreeList[i], 0) ;
      DEC(i)
   END ;
   WriteString('^^^^^^^^^^^ f o r e s t    e n d ^^^^^^^^^^^^') ; WriteLn ;
END DisplayForest ;


(*
   DisplayTree - displays a tree starting from the root.
*)

PROCEDURE DisplayTree (root: Nodes; indent: CARDINAL) ;
BEGIN
   IF root#NulNode
   THEN
      WITH NodeList[root] DO
         WriteNSpaces(indent) ;
         IF IsLeaf
         THEN
            WriteNode(root) ; WriteLn
         ELSE
            Write('(') ; WriteNode(root) ; WriteLn ;
            IF Right#NulNode
            THEN
               DisplayTree(Right, indent+3) ; Write(',') ; WriteLn
            END ;
            IF Left#NulNode
            THEN
               DisplayTree(Left, indent+3) ; Write(')') ; WriteLn
            END
         END
      END
   END
END DisplayTree ;


(*
   WriteNode - 
*)

PROCEDURE WriteNode (n: Nodes) ;
VAR
   i: CARDINAL ;
BEGIN
   WITH NodeList[n] DO
      IF NOT IsLeaf
      THEN
         WriteOperator(Op) ; WriteString('(Q') ; WriteCard(SymQuad, 0) ; WriteString(')  ')
      END ;
      WriteString('[idx=') ; WriteCard(IndexNo, 0) ; WriteString(']   <n=') ; WriteCard(VAL(CARDINAL, n), 0) ; WriteString('> ') ;
      i := 1 ;
      WHILE i<=NoOfIds DO
         IF GetEntitySym(EntList[i])#NulSym
         THEN
            IF IsLValue(EntList[i])
            THEN
               WriteString(' & {')
            ELSE
               WriteString('  {')
            END ;
            WriteKey(GetSymName(GetEntitySym(EntList[i]))) ;
            Write('[') ; WriteCard(GetEntityIndex(EntList[i]), 0) ; Write(']') ;
            IF IsConst(GetEntitySym(EntList[i])) OR IsEntityClean(EntList[i])
            THEN
               WriteString('(c) ,')
            ELSE
               WriteString('(d) ,')
            END
         END ;
         INC(i)
      END ;
      Write('}')
   END
END WriteNode ;


(*
   UseQuadAtNode - uses the quadruple at node, n.
*)

PROCEDURE UseQuadAtNode (n: Nodes; index: CARDINAL; Start, End: CARDINAL) ;
VAR
   i, q: CARDINAL ;
BEGIN
   WITH NodeList[n] DO
      IF IndexNo>index
      THEN
         IF Debugging
         THEN
            WriteString('ignore for now as index is: ') ; WriteCard(index, 0) ; WriteLn
         END
      ELSIF GetSym(n)=NulSym
      THEN
(*
         IF EntList[1]=NIL
         THEN
            InternalError('why are we using an entity which is nul?', __FILE__, __LINE__)
         END ;
*)
         IF Debugging
         THEN
            WriteString('no need for quad ') ; WriteCard(SymQuad, 0) ; WriteLn
         END
      ELSE
         IF NOT IsEntityClean(EntList[1])
         THEN
            MakeEntityClean(EntList[1]) ;   (* only need to clean it once *)
            i := FindNextUnUsedQuad() ;   (* find a spare quadruple     *)
            q := RemoveList[i].QuadNo ;
            AssertQuad := Max(AssertQuad, q) ;
            SaveQuad(q) ;
            MakeQuadFromNode(q, n) ;                 (* rewrite quad using the node operator *)
            IF Debugging
            THEN
               DisplayQuad(q)
            END
         END ;
         CheckEntList(n, index, Start, End)
                           (* now is a good time to flush dirty symbols                   *)
      END                  (* we could wait until later but it is easier for the code     *)
   END                     (* generator to generate fast code if we keep symbols together *)
END UseQuadAtNode ;


(*
   IsNode - returns TRUE if node, n, is a node and not a leaf
*)

PROCEDURE IsNode (n: Nodes) : BOOLEAN ;
BEGIN
   WITH NodeList[n] DO
      RETURN( (NOT IsLeaf) )
   END
END IsNode ;


(*
   SwapEntity - 
*)

PROCEDURE SwapEntity (VAR i, j: Entity) ;
VAR
   t: Entity ;
BEGIN
   t := i ;
   i := j ;
   j := t
END SwapEntity ;


(*
   CheckDestination - checks to see whether node, n, should use a different destination
                      quadruple. Use 2 passes as we prefer constants then real variables.
*)

PROCEDURE CheckDestination (n: Nodes; Start, End: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   WITH NodeList[n] DO
      IF (EntList[1]=NIL) OR (NOT IsConst(GetEntitySym(EntList[1])))
      THEN
         (* check to see whether a constant can be found *)
         i := 2 ;
         WHILE i<=NoOfIds DO
            IF (GetEntitySym(EntList[i])#NulSym) AND IsConst(GetEntitySym(EntList[i]))
            THEN
               SwapEntity(EntList[1], EntList[i]) ;
               RETURN
            END ;
            INC(i)
         END
      END ;
      IF NOT IsLeaf
      THEN
         (* we prefer to use a variable or temporary which is used outside the block: Start..End *)
         i := 2 ;
         WHILE i<=NoOfIds DO
            IF EntList[1]=NIL
            THEN
               SwapEntity(EntList[1], EntList[i])
            ELSIF (EntList[1]#NIL) AND IsVar(GetEntitySym(EntList[1])) AND
                  (NOT IsUsedOutSide(GetEntitySym(EntList[1]), Start, End))
            THEN
               (* now we know that EntList[1] is either a temp/real variable
                  and it is not used outside Start..End.
                  We shall now see whether EntList[i] is used outside and is a variable
               *)
               IF EntList[i]#NIL
               THEN
                  IF IsVar(GetEntitySym(EntList[i])) AND
                     IsUsedOutSide(GetEntitySym(EntList[i]), Start, End)
                  THEN
                     (* better to use EntList[i] as it is used outside *)
                     SwapEntity(EntList[1], EntList[i])
                  ELSIF IsTemporary(GetEntitySym(EntList[1])) AND
                        (NOT IsTemporary(GetEntitySym(EntList[i])))
                  THEN
                     (*
                        EntList[1] is a temporary and EntList[i] is a real variable.
                        - use real variable as it might save a flush.
                     *)
                     SwapEntity(EntList[1], EntList[i])
                  END
               END ;
               INC(i)
            ELSE
               INC(i)
            END
         END
      END
   END
END CheckDestination ;


(*
   ForeachNodeDo - traverse tree nodes.
*)

PROCEDURE ForeachNodeDo (n: Nodes; p: ProcNode; Start, End: CARDINAL) ;
BEGIN
   IF n#NulNode
   THEN
      WITH NodeList[n] DO
         IF NOT IsLeaf
         THEN
            ForeachNodeDo(Right, p, Start, End) ;
            ForeachNodeDo(Left, p, Start, End) ;
            p(n, Start, End)
         END
      END
   END
END ForeachNodeDo ;


(*
   ForeachLeafDo - traverse tree leaves.
*)

PROCEDURE ForeachLeafDo (n: Nodes; p: ProcNode; Start, End: CARDINAL) ;
BEGIN
   IF n#NulNode
   THEN
      WITH NodeList[n] DO
         IF IsLeaf
         THEN
            p(n, Start, End)
         ELSE
            ForeachLeafDo(Right, p, Start, End) ;
            ForeachLeafDo(Left, p, Start, End)
         END
      END
   END
END ForeachLeafDo ;


(*
   IsCommutative - returns TRUE if the operator is commutative
*)

PROCEDURE IsCommutative (op: QuadOperator) : BOOLEAN ;
BEGIN
   RETURN( (op=AddOp) OR (op=MultOp) OR (op=LogicalOrOp) OR (op=LogicalAndOp) )
END IsCommutative ;


(*
   IsNotAlreadyUsed - returns TRUE if no other parent is using this son.
*)

PROCEDURE IsNotAlreadyUsed (son, parent: Nodes) : BOOLEAN ;
VAR
   n, i: CARDINAL ;
BEGIN
   n := 1 ;
   WHILE n<=NoOfNodes DO
      IF (n#parent) AND (n#son)
      THEN
         WITH NodeList[n] DO
            IF (NOT IsLeaf) AND ((Left=son) OR (Right=son))
            THEN
               RETURN( FALSE )   (* bother someone is already using the result of the son *)
            END
         END
      END ;
      INC(n)
   END ;
   RETURN( TRUE )
END IsNotAlreadyUsed ;


(*
   AlterReferences - 
*)

PROCEDURE AlterReferences (from, to: Nodes) ;
VAR
   i: CARDINAL ;
BEGIN
   i := NoOfNodes ;
   WHILE i>0 DO
      WITH NodeList[i] DO
         IF NOT IsLeaf
         THEN
            IF Left=from
            THEN
               Left := to
            END ;
            IF Right=from
            THEN
               Right := to
            END
         END
      END ;
      DEC(i)
   END ;
   i := NoOfTrees ;
   WHILE i>0 DO
      IF TreeList[i]=from
      THEN
         TreeList[i] := to
      END ;
      DEC(i)
   END
END AlterReferences ;


(*
   RemoveNode - remove a node, n, by making it a leaf with no ids.
                It is the responsibility of the caller to make sure, n, is not
                referenced.
*)

PROCEDURE RemoveNode (n: Nodes) ;
BEGIN
   (* should not need to perform this following check, but I'd rather know about a bug... *)
   IF IsNotAlreadyUsed(n, n)
   THEN
      WITH NodeList[n] DO
         (* now we must clobber this node - make it a leaf with no ids *)
         IsLeaf := TRUE ;
         NoOfIds := 0 ;
         EntList[1] := NIL
      END
   ELSE
      InternalError('node was thought to be unreferenced (see CheckOperator)', __FILE__, __LINE__)
   END
END RemoveNode ;


(*
   MoveNode - moves all contents of node, from, to node, to.
*)

PROCEDURE MoveNode (from, to: Nodes) ;
VAR
   i: CARDINAL ;
BEGIN
   AlterReferences(from, to) ;
   WITH NodeList[from] DO
      i := NoOfIds ;
      WHILE i>0 DO
         AddDependent(to, EntList[i]) ;
         DEC(i)
      END ;
      RemoveNode(from)
   END
END MoveNode ;


(*
   IsCseIfFlipNode - returns TRUE if a CSE is found.
*)

PROCEDURE IsCseIfFlipNode (top, bot: Nodes;
                           op: QuadOperator; l, r: Nodes) : BOOLEAN ;
VAR
   k, a: Nodes ;
BEGIN
   IF (r#NulNode) AND (l#NulNode)
   THEN
      IF Debugging
      THEN
         WriteString('do we know about (') ;
         WriteOperator(op) ;
         WriteKey(GetSymName(GetSym(l))) ; Write(' ') ;
         WriteKey(GetSymName(GetSym(r))) ; WriteString(') ? ')
      END ;
      k := IsNodeKnown(op, l, r) ;
      IF k=NulNode
      THEN
         k := IsNodeKnown(op, r, l)
      END ;
      IF (k#NulNode) AND (k#bot)
      THEN
         (*
            found a CSE and it is not bot node ie
            there is no point of destroying one CSE to create another
         *)
         IF Debugging
         THEN
            WriteString('yes another CSE found under this tree') ; WriteLn ;
            DisplayTree(top, 0) ;
            WriteString('---------------------------------') ; WriteLn
         END ;

         (* now let us graft this subexpression onto the bottom node *)
         WITH NodeList[bot] DO
            IF (Right=r) OR (Right=l)
            THEN
               Right := k
            ELSIF (Left=l) OR (Left=r)
            THEN
               Left := k
            END
         END ;
         MoveNode(top, bot) ;
         IF Debugging
         THEN
            WriteString('---------------------------------') ; WriteLn ;
            DisplayTree(bot, 0) ;
            WriteString('---------------------------------') ; WriteLn ;
         END ;
         RETURN( TRUE )
      END ;
      IF Debugging
      THEN
         WriteString('no') ; WriteLn
      END
   END ;
   RETURN( FALSE )
END IsCseIfFlipNode ;


(*
   IsEntityListNotUsedOutside - returns TRUE if all entities of node, n, are not used outside
                                the block of code: Start..End.
*)

PROCEDURE IsEntityListNotUsedOutside (n: Nodes; Start, End: CARDINAL) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   WITH NodeList[n] DO
      i := NoOfIds ;
      WHILE i>0 DO
         IF (EntList[i]#NIL) AND IsUsedOutSide(GetEntitySym(EntList[i]), Start, End)
         THEN
            RETURN( FALSE )
         END ;
         DEC(i)
      END
   END ;
   RETURN( TRUE )
END IsEntityListNotUsedOutside ;


(*
   IsConstIfFlipNode - returns TRUE if a constant expression is found.
*)

PROCEDURE IsConstIfFlipNode (top, bot: Nodes;
                             op: QuadOperator; l, r: Nodes) : BOOLEAN ;
VAR
   tn : CARDINAL ;
   sym: CARDINAL ;
   e  : Entity ;
   k  : Nodes ;
BEGIN
   IF (r#NulNode) AND (l#NulNode)
   THEN
      IF Debugging
      THEN
         WriteString('can we evaluate (') ;
         WriteOperator(op) ;
         WriteKey(GetSymName(GetSymOfChoice(l))) ; Write(' ') ;
         WriteKey(GetSymName(GetSymOfChoice(r))) ; WriteString(') ? ')
      END ;
      IF IsConst(GetSymOfChoice(l)) AND IsConst(GetSymOfChoice(r))
      THEN
         IF Debugging
         THEN
            WriteString('yes') ; WriteLn
         END ;

         tn := QuadToTokenNo(NodeList[bot].SymQuad) ;
         sym := MakeNewConstant(tn, op, GetSymOfChoice(l), GetSymOfChoice(r)) ;
         e := MakeNewEntity(tn,
                            sym, FALSE, VAL(CARDINAL, NoOfNodes)) ;
         GiveEntityIndex(e, NodeList[bot].IndexNo) ;   (* constant therefore give life at same time as bot *)
         k := MakeLeaf(e) ;

         (* now let us graft this constant onto the top node *)
         WITH NodeList[top] DO
            IF (Right=r) OR (Right=l)
            THEN
               Right := k ;
               IF (NodeList[bot].Right=r) OR (NodeList[bot].Right=l)
               THEN
                  Left := NodeList[bot].Left
               ELSIF (NodeList[bot].Left=r) OR (NodeList[bot].Left=l)
               THEN
                  Left := NodeList[bot].Right
               END
            ELSIF (Left=l) OR (Left=r)
            THEN
               Left := k ;
               IF (NodeList[bot].Right=r) OR (NodeList[bot].Right=l)
               THEN
                  Right := NodeList[bot].Left
               ELSIF (NodeList[bot].Left=r) OR (NodeList[bot].Left=l)
               THEN
                  Right := NodeList[bot].Right
               END
            END
         END ;
         RemoveNode(bot) ;
         IF Debugging
         THEN
            WriteString('---------------------------------') ; WriteLn ;
            DisplayTree(top, 0) ;
            WriteString('---------------------------------') ; WriteLn ;
         END ;
         RETURN( TRUE )
      END ;
      IF Debugging
      THEN
         WriteString('no') ; WriteLn
      END
   END ;
   RETURN( FALSE )
END IsConstIfFlipNode ;


(*
   FoundKnownNode - returns TRUE if a node was removed due to a CSE found.
                    n2 will be a child of n1.
                    It also returns TRUE if two adjacent constants are found.
                    NOTE that the CSE takes precidence, (should this be the case?).
*)

PROCEDURE FoundKnownNode (op: QuadOperator; n1, n2: Nodes) : BOOLEAN ;
BEGIN
   WITH NodeList[n1] DO
      IF Left=n2
      THEN
         RETURN(
                IsCseIfFlipNode(n1, n2, Op, Right, NodeList[n2].Right)   OR
                IsCseIfFlipNode(n1, n2, Op, Right, NodeList[n2].Left)    OR
                IsConstIfFlipNode(n1, n2, Op, Right, NodeList[n2].Right) OR
                IsConstIfFlipNode(n1, n2, Op, Right, NodeList[n2].Left)
               )
      ELSE
         RETURN(
                IsCseIfFlipNode(n1, n2, Op, Left, NodeList[n2].Right)    OR
                IsCseIfFlipNode(n1, n2, Op, Left, NodeList[n2].Left)     OR
                IsConstIfFlipNode(n1, n2, Op, Left, NodeList[n2].Right)  OR
                IsConstIfFlipNode(n1, n2, Op, Left, NodeList[n2].Left)
               )
      END
   END
END FoundKnownNode ;


(*
   CheckOperator - check to see whether if node, n, uses a commutative operator and whether
                   if we flip nodes we can uncover another CSE ie consider:

                   a := b + 1 + b + 1

                   might form the following tree

                         :=
                   a           +
                            b     +
                                1   +
                                   b 1

                   which may be transformed into:

                         :=
                   a            +
                             +      +
                            b 1    b 1

                   which then can have its CSE removed.

                   It also checks to see whether if a node is flipped we can uncover
                   two adjacent constants, consider:

                         :=
                   a           +
                            1     +
                                b   1

                   which may be transformed into:

                         :=
                   a           +
                            2     b
*)

PROCEDURE CheckOperator (n: Nodes; Start, End: CARDINAL) : BOOLEAN ;
BEGIN
   WITH NodeList[n] DO
      IF NOT IsLeaf
      THEN
         IF IsCommutative(Op)
         THEN
            IF IsNode(Left) AND (NodeList[Left].Op=Op) AND
               IsNotAlreadyUsed(Left, n) AND IsEntityListNotUsedOutside(Left, Start, End)
            THEN
               (* legally allowed to check *)
               IF FoundKnownNode(Op, n, Left)
               THEN
                  RETURN( TRUE )
               END
            END ;
            IF IsNode(Right) AND (NodeList[Right].Op=Op) AND
               IsNotAlreadyUsed(Right, n) AND IsEntityListNotUsedOutside(Right, Start, End)
            THEN
               (* legally allowed to check *)
               IF FoundKnownNode(Op, n, Right)
               THEN
                  RETURN( TRUE )
               END
            END
         END
      END
   END ;
   RETURN( FALSE )
END CheckOperator ;


(*
   CheckCommutativeTree - attempts to balance tree in an attempt to find more CSEs
*)

PROCEDURE CheckCommutativeTree (n: Nodes; Start, End: CARDINAL) : BOOLEAN ;
BEGIN
   IF n#NulNode
   THEN
      WITH NodeList[n] DO
         IF NOT IsLeaf
         THEN
            IF CheckCommutativeTree(Right, Start, End) OR
               CheckCommutativeTree(Left, Start, End) OR
               CheckOperator(n, Start, End)
            THEN
               RETURN( TRUE )
            END
         END
      END
   END ;
   RETURN( FALSE )
END CheckCommutativeTree ;


(*
   TraverseForest - 
*)

PROCEDURE TraverseForest (Start, End: CARDINAL) ;
VAR
   i          : CARDINAL ;
   RemovedNode: BOOLEAN ;
BEGIN
   i := NoOfTrees ;
   (* 1st pass over forest - continue to remove subexpressions *)
   REPEAT
      RemovedNode := FALSE ;
      WHILE (i>0) AND (NOT RemovedNode) DO
         IF Debugging
         THEN
            DisplayTree(TreeList[i], 0)
         END ;
         RemovedNode := CheckCommutativeTree(TreeList[i], Start, End) ;
         IF RemovedNode AND Debugging
         THEN
            DisplayTree(TreeList[i], 0)
         END ;
         DEC(i)
      END ;
      i := NoOfTrees ;
   UNTIL NOT RemovedNode ;
   (* 2nd pass over forest - try and use variables not temps *)
   i := NoOfTrees ;
   AssertQuad := 0 ;
   WHILE i>0 DO
      ForeachLeafDo(TreeList[i], CheckDestination, Start, End) ;
      ForeachNodeDo(TreeList[i], CheckDestination, Start, End) ;
      IF Debugging
      THEN
         DisplayTree(TreeList[i], 0)
      END ;
      DEC(i)
   END
END TraverseForest ;


(*
   UseList - uses the list to recreate the quadruples.
             This procedure slowly increments the index until it is equal
             to the sequence numbers and then emits the appropiate copies.
*)

PROCEDURE UseList (l: List; Start, End: CARDINAL) ;
VAR
   index, n, m: CARDINAL ;
BEGIN
   (* now we know the reverse order - act upon this *)
   index := 0 ;
   IF Debugging
   THEN
      WriteLn
   END ;
   WHILE index<=NoOfNodes DO
      n := NoOfItemsInList(l) ;
      WHILE n>0 DO
         m := GetItemFromList(l, n) ;
         IF Debugging
         THEN
            IF (FunctNode#NulNode) AND (FunctNode=m)
            THEN
               WriteString('[Funct') ; WriteCard(m, 0) ; Write(']')
            ELSE
               Write('[') ; WriteCard(m, 0) ; Write(']')
            END
         END ;
         CheckEntList(VAL(Nodes, m), index, Start, End) ;    (* save aliases for both leaves and nodes *)
         IF NOT NodeList[m].IsLeaf
         THEN
            UseQuadAtNode(VAL(Nodes, m), index, Start, End)
         END ;
         DEC(n)
      END ;
      INC(index)
   END ;
   IF Debugging
   THEN
      WriteLn
   END
END UseList ;


(*
   PlaceAllNodesIntoList - places the index of all known nodes into list, l.
*)

PROCEDURE PlaceAllNodesIntoList (l: List) ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE i<=NoOfNodes DO
      PutItemIntoList(l, i) ;
      INC(i)
   END
END PlaceAllNodesIntoList ;


(*
   IsParentOf - returns TRUE if the son - parent relationship exists.
*)

PROCEDURE IsParentOf (son, parent: CARDINAL) : BOOLEAN ;
BEGIN
   WITH NodeList[parent] DO
      IF IsLeaf OR (son=NulNode)
      THEN
         RETURN( FALSE )
      ELSE
         RETURN( (Right=son) OR (Left=son) )
      END
   END
END IsParentOf ;


(*
   AllParentsOf - 
*)

PROCEDURE AllParentsOf (n: CARDINAL; Listed: List) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   IF n=NulNode
   THEN
      RETURN( FALSE )
   ELSE
      i := 1 ;
      WHILE i<=NoOfNodes DO
         IF IsParentOf(n, i) AND (NOT IsItemInList(Listed, i))
         THEN
            RETURN( FALSE )
         END ;
         INC(i)
      END ;
      RETURN( TRUE )
   END
END AllParentsOf ;


(*
   GetFirstWithParents - get the first node from Unlisted whose parents are Listed.
*)

PROCEDURE GetFirstWithParentsFromWhichAre (Unlisted, Listed: List) : CARDINAL ;
VAR
   n, m: CARDINAL ;
BEGIN
(*
   (* give FunctNode, last of all - as it needs to be processed first of all *)
   n := NoOfItemsInList(Unlisted) ;
   IF (n=1) AND (FunctNode#NulNode)
   THEN
      (* last node *)
      m := GetItemFromList(Unlisted, n) ;
      IF m#FunctNode
      THEN
         InternalError('expecting FunctNode (return value) node', __FILE__, __LINE__)
      END ;
      FunctNode := NulNode ;
      IF AllParentsOf(m, Listed)
      THEN
         RETURN( m )
      ELSE
         (* this should NEVER occur, FunctNode must be top of a tree *)
         InternalError('last node is FunctNode but its parents are not listed', __FILE__, __LINE__)
      END
   END ;
   WHILE n>0 DO
      m := GetItemFromList(Unlisted, n) ;
      IF FunctNode#m
      THEN
         IF AllParentsOf(m, Listed)
         THEN
            RETURN( m )
         END
      END ;
      DEC(n)
   END ;
   RETURN( NulNode )
*)
   n := NoOfItemsInList(Unlisted) ;
   WHILE n>0 DO
      m := GetItemFromList(Unlisted, n) ;
      IF AllParentsOf(m, Listed)
      THEN
         RETURN( m )
      END ;
      DEC(n)
   END ;
   RETURN( NulNode )
END GetFirstWithParentsFromWhichAre ;


(*
   GetLeftMostWhoseParentsAreFrom - returns the left most node
*)

PROCEDURE GetLeftMostWhoseParentsAreFrom (Listed, UnListed: List; n: Nodes) : CARDINAL ;
BEGIN
   IF n=NulNode
   THEN
      RETURN( NulNode )
   ELSE
      WITH NodeList[n] DO
         IF IsLeaf
         THEN
            RETURN( NulNode )
         ELSIF AllParentsOf(Left, Listed)
         THEN
            RETURN( Left )
         ELSE
            RETURN( NulNode )
         END
      END
   END
END GetLeftMostWhoseParentsAreFrom ;


(*
   TopologicalSortTree - use algorithm in dragon book page 560.
                         We modify it slightly to use the index numbers
                         of Tremblay & Sorenson
                         and we also place all leaves into the list because
                         there maybe copies that we must flush.
*)

PROCEDURE TopologicalSortTree (Start, End: CARDINAL) ;
VAR
   m, n    : Nodes ;
   Listed,
   Unlisted: List ;
BEGIN
   InitList(Unlisted) ;
   InitList(Listed) ;
   PlaceAllNodesIntoList(Unlisted) ;
   (* work out whether we can deal with the whole forest in one turn? *)
   WHILE NoOfItemsInList(Unlisted)#0 DO
      n := GetFirstWithParentsFromWhichAre(Unlisted, Listed) ;
      RemoveItemFromList(Unlisted, VAL(CARDINAL, n)) ;
      PutItemIntoList(Listed, VAL(CARDINAL, n)) ;
      REPEAT
         m := GetLeftMostWhoseParentsAreFrom(Listed, Unlisted, n) ;
         IF m#NulNode
         THEN
            PutItemIntoList(Listed, VAL(CARDINAL, m)) ;
            RemoveItemFromList(Unlisted, VAL(CARDINAL, m)) ;
            n := m
         END
      UNTIL m=NulNode
   END ;
   UseList(Listed, Start, End) ;
   CheckNodesWereSorted(Start, End) ;
   KillList(Listed) ;
   KillList(Unlisted)   (* tidy up *)
END TopologicalSortTree ;


(*
   IsReferencedInNode - returns TRUE whether a node, n, references an entity, e.
*)

PROCEDURE IsReferencedInNode (n: Nodes; e: Entity) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   IF n#NulNode
   THEN
      WITH NodeList[n] DO
         i := NoOfIds ;
         WHILE i>=1 DO
            IF EntList[i]=e
            THEN
               RETURN( TRUE )
            ELSE
               DEC(i)
            END
         END
      END
   END ;
   RETURN( FALSE )
END IsReferencedInNode ;


(*
   MakeLeavesClean - simple procedure which makes all leaves clean.
                     Note this does nothing to aliases of leaves.
*)

PROCEDURE MakeLeavesClean ;
VAR
   i: Nodes ;
BEGIN
   i := 1 ;
   WHILE i<=NoOfNodes DO
      WITH NodeList[i] DO
         IF IsLeaf AND (EntList[1]#NIL) AND (NOT IsReferencedInNode(FunctNode, EntList[1]))
         THEN
            MakeEntityClean(EntList[1])
         END
      END ;
      INC(i)
   END ;
   IF Debugging
   THEN
      WriteString('after cleaning leaves') ; WriteLn ;
      DisplayForest
   END
END MakeLeavesClean ;


(*
   GetTreeIndex - returns the highest variable index within a tree.
                  This searches the leaves and returns the highest index.
*)

PROCEDURE GetTreeIndex (node: Nodes) : CARDINAL ;
BEGIN
   IF node=NulNode
   THEN
      RETURN( 0 )
   ELSE
      RETURN( NodeList[node].IndexNo )
   END
END GetTreeIndex ;


(*
   CalculateTreeIndex - 
*)

PROCEDURE CalculateTreeIndex (n: Nodes) ;
VAR
   i: CARDINAL ;
BEGIN
   IF n#NulNode
   THEN
      WITH NodeList[n] DO
         IF IsLeaf
         THEN
            i := 1 ;
            WHILE i<=NoOfIds DO
               IF (EntList[i]#NIL) AND IsEntityClean(EntList[i]) AND (GetEntityIndex(EntList[i])>IndexNo)
               THEN
                  IndexNo := GetEntityIndex(EntList[i])
               END ;
               INC(i)
            END
         ELSE
            CalculateTreeIndex(Right) ;
            CalculateTreeIndex(Left) ;
            (* max(left, right)+1 *)
            IF (Right#NulNode) AND (NodeList[Right].IndexNo>=IndexNo)
            THEN
               IndexNo := NodeList[Right].IndexNo+1
            END ;
            IF (Left#NulNode) AND (NodeList[Left].IndexNo>=IndexNo)
            THEN
               IndexNo := NodeList[Left].IndexNo+1
            END
         END
      END
   END
END CalculateTreeIndex ;


(*
   TopologicallySortForest - rewrites the quadruples using the topological sort algorithm.
*)

PROCEDURE TopologicallySortForest (Start, End: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   (* MakeLeavesClean ; *)
   AssertQuad := 0 ;
   (* now topologically sort ascending trees *)
   IF FunctNode#NulNode
   THEN
      CalculateTreeIndex(FunctNode)
   END ;
   i := NoOfTrees ;
   WHILE i>0 DO
      IF i#FunctNode
      THEN
         CalculateTreeIndex(TreeList[i])
      END ;
      DEC(i)
   END ;
   (* one call to TopologicalSortTree is enough *)
   TopologicalSortTree(Start, End)
END TopologicallySortForest ;


(*
   IsUsedOutSide - returns TRUE if symbol is used outside the quadruple range, Start..End.
*)

PROCEDURE IsUsedOutSide (Sym: CARDINAL; Start, End: CARDINAL) : BOOLEAN ;
VAR
   Scope,
   UsedStart, UsedEnd: CARDINAL ;
BEGIN
   Scope := GetScope(Sym) ;
   IF IsModule(Scope) AND IsExported(Scope, Sym)
   THEN
      RETURN( TRUE )
   ELSE
      GetQuads(Sym, RightValue, UsedStart, UsedEnd) ;
      IF (UsedStart#0) AND (UsedEnd#0) AND ((UsedStart<Start) OR (End<UsedEnd))
      THEN
         (* is used outside *)
         RETURN( TRUE )
      ELSIF HasLValue(Sym)
      THEN
         RETURN( TRUE )
      ELSE
         RETURN( FALSE )
      END
   END
END IsUsedOutSide ;


(*
   NeedsToBeSaved - is entity, e, used outside the quadruple range, Start..End ?
                    The entity, e, is considered to be dirty if it is a global variable
                    since even if this sym is only used within one basic block we still will
                    (almost certainaly) exit from this basic block and return later.. probably
                    to read the variable and alter it..
*)

PROCEDURE NeedsToBeSaved (e: Entity; Start, End: CARDINAL) : BOOLEAN ;
VAR
   sym: CARDINAL ;
BEGIN
   IF e=NIL
   THEN
      RETURN( FALSE )
   ELSE
      sym := GetEntitySym(e) ;
      IF IsConst(sym)
      THEN
         RETURN( FALSE )
      ELSE
         RETURN( IsUsedOutSide(sym, Start, End) )
      END
   END
END NeedsToBeSaved ;


(*
   IsDirty - is entity, i, in node, n, dirty?
*)

PROCEDURE IsDirty (n: Nodes; i: CARDINAL) : BOOLEAN ;
BEGIN
   WITH NodeList[n] DO
      RETURN( (EntList[i]#NIL) AND (IsVar(GetEntitySym(EntList[i]))) AND (NOT IsEntityClean(EntList[i])) )
   END
END IsDirty ;


(*
   IsGlobalVariable - returns TRUE if symbol, sym, is a global variable.
*)

PROCEDURE IsGlobalVariable (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsConst(sym) OR IsProcedure(sym)
   THEN
      RETURN( FALSE )
   ELSIF IsProcedure(GetVarScope(sym))
   THEN
      RETURN( FALSE )
   ELSE
      RETURN( TRUE )
   END
END IsGlobalVariable ;


(*
   CleanSymbol - clean the entity, e, using the symbol, with.
*)

PROCEDURE CleanSymbol (e: Entity; with: CARDINAL) ;
VAR
   sym,
   i  : CARDINAL ;
BEGIN
   IF e#NIL
   THEN
      sym := GetEntitySym(e) ;
      IF Debugging
      THEN
         WriteString('need to clean symbol ') ; WriteKey(GetSymName(sym)) ;
         Write('[') ; WriteCard(GetEntityIndex(e), 0) ;
         WriteString(']  with ') ; WriteKey(GetSymName(with)) ; WriteLn
      END ;

      IF sym#with
      THEN
         i := FindNextUnUsedQuad() ;
         IF RemoveList[i].QuadNo<AssertQuad
         THEN
            InternalError('attempting to use a quad for flush (possibly) before symbol is used', __FILE__, __LINE__)
         END ;
         EraseQuad(RemoveList[i].QuadNo) ;
         PutQuad(RemoveList[i].QuadNo, BecomesOp, sym, NulSym, with) ;
         RemoveList[i].IsUsed := TRUE
      END
   END
END CleanSymbol ;


(*
   ChooseCleanEntityForLabel - 
*)

PROCEDURE ChooseCleanEntityForLabel (n: Nodes) ;
VAR
   i         : CARDINAL ;
   sym, index: CARDINAL ;
BEGIN
   WITH NodeList[n] DO
      (* prefer to choose (in order) a clean constant, variable or temporary *)

      (* try for a constant first *)
      i := 2 ;
      WHILE i<=NoOfIds DO
         IF (EntList[i]#NIL) AND IsEntityClean(EntList[i]) AND IsConst(GetEntitySym(EntList[i]))
         THEN
            SwapEntity(EntList[1], EntList[i]) ;
            RETURN
         END ;
         INC(i)
      END ;

      (* try for a variable second *)
      i := 2 ;
      WHILE i<=NoOfIds DO
         IF (EntList[i]#NIL) AND IsEntityClean(EntList[i]) AND IsVar(GetEntitySym(EntList[i]))
         THEN
            SwapEntity(EntList[1], EntList[i]) ;
            RETURN
         END ;
         INC(i)
      END ;

      (* now try for a temporary (or any clean symbol) *)
      i := 2 ;
      WHILE i<=NoOfIds DO
         IF (EntList[i]#NIL) AND IsEntityClean(EntList[i])
         THEN
            SwapEntity(EntList[1], EntList[i]) ;
            RETURN
         END ;
         INC(i)
      END
   END ;
   InternalError('no more clean entities found for substitution at node or leaf', __FILE__, __LINE__)
END ChooseCleanEntityForLabel ;


(*
   ChooseAnotherEntityForLabel - 
*)

PROCEDURE ChooseAnotherEntityForLabel (n: Nodes; j: CARDINAL) ;
VAR
   i         : CARDINAL ;
   sym, index: CARDINAL ;
BEGIN
   WITH NodeList[n] DO
      i := 2 ;
      index := GetEntityIndex(EntList[j]) ;
      sym := GetEntitySym(EntList[j]) ;
      EntList[j] := NIL ;  (* remove it *)
      (* prefer to choose a clean entity *)
      WHILE i<=NoOfIds DO
         IF (EntList[i]#NIL) AND IsEntityClean(EntList[i])
         THEN
            SwapEntity(EntList[1], EntList[i]) ;
            RETURN
         END ;
         INC(i)
      END
   END ;
   IF Debugging
   THEN
      WriteString('problem on node ') ; WriteCard(VAL(CARDINAL, n), 0) ; WriteLn ;
      DisplayForest
   END ;
   (* this is not allowed - it means the topological sort was illegal *)
(* no it is now legal, it means that this node is no longer needed
   InternalError('no more entities found for substitution at node or leaf', __FILE__, __LINE__)
*)
END ChooseAnotherEntityForLabel ;


(*
   RemoveAllOlderEntities - removes all older occurances of symbols
                            GetEntitySym(e) from the DAG.
                            We are about to alter symbol GetEntitySym(e) and thus
                            we should check to see whether we can remove any previous
                            references to this symbol.
*)

PROCEDURE RemoveAllOlderEntities (n: Nodes; e: Entity) ;
VAR
   i    : Nodes ;
   j    : CARDINAL ;
   sym,
   index: CARDINAL ;
BEGIN
   index := GetEntityIndex(e) ;
   sym := GetEntitySym(e) ;
   IF Debugging
   THEN
      DisplayForest ;
      WriteKey(GetSymName(sym)) ; WriteString(' symbols older than ') ; WriteCard(index, 0) ;
      WriteString(' need to be removed') ; WriteLn
   END ;
   i := NoOfNodes ;
   WHILE i>0 DO
      IF i#n
      THEN
         WITH NodeList[i] DO
            j := NoOfIds ;
            WHILE j>0 DO
               IF (EntList[j]#NIL) AND
                  (GetEntitySym(EntList[j])=sym) AND (GetEntityIndex(EntList[j])<index)
               THEN
                  (* found an older symbol entity *)
                  IF j=1
                  THEN
                     ChooseAnotherEntityForLabel(i, j)
                  ELSE
                     (* remove older entity *)
                     EntList[j] := NIL
                  END
               END ;
               DEC(j)
            END
         END
      END ;
      DEC(i)
   END ;
   IF Debugging
   THEN
      WriteKey(GetSymName(sym)) ; WriteString(' symbols older than ') ; WriteCard(index, 0) ;
      WriteString(' have now been removed') ; WriteLn ;
      DisplayForest
   END ;
END RemoveAllOlderEntities ;


(*
   IsReachable - returns TRUE if an entity NodeList[n].EntList[i] is required
                 during this DAG. A real function to perform this operation would
                 be complex. We don't bother and simplify the problem. This
                 function simply sees whether another entity is in the EntList
                 and whether it has a < index than NodeList[n].EntList[i].
                 In which case we return TRUE. This will
                 produce working code but will cause some unnecessary flushes.
*)

PROCEDURE IsReachable (n: Nodes; i: CARDINAL) : BOOLEAN ;
VAR
   highest: CARDINAL ;
   j      : CARDINAL ;
BEGIN
   WITH NodeList[n] DO
      (* there is no problem of reachability with temporaries as they are rarely reassigned *)
      IF NOT IsTemporary(GetEntitySym(EntList[i]))
      THEN
         highest := GetEntityIndex(EntList[i]) ;
         j := 1 ;
         WHILE j<=NoOfIds DO
            IF (j#i) AND
               (EntList[j]#NIL) AND (GetEntityIndex(EntList[j])<highest)
            THEN
               RETURN( TRUE )
            END ;
            INC(j)
         END
      END
   END ;
   RETURN( FALSE )
END IsReachable ;


(*
   CheckNeeded - checks to see whether entity, e, is required to flush other
                 dirty symbols.
                 We are about to clobber entity, e, so we check to see whether
                 we must flush this entity into other symbols before it disappears.

                 NOTE: is it possible for this routine to recursively loop forever??
*)

PROCEDURE CheckNeeded (l: List; n: Nodes; sym: CARDINAL; index: CARDINAL;
                       Start, End: CARDINAL) ;
VAR
   i: Nodes ;
   j: CARDINAL ;
BEGIN
   IF Debugging
   THEN
      WriteString('do we need symbol ') ; WriteKey(GetSymName(sym)) ;
      WriteString(' index = ') ; WriteCard(index, 0) ; WriteString(' for any other cleaning?') ;
      WriteLn
   END ;
   IF index>0
   THEN
      i := NoOfNodes ;
      WHILE i>0 DO
         IF NOT IsItemInList(l, VAL(CARDINAL, i))
         THEN
            WITH NodeList[i] DO
               IF (EntList[1]#NIL) AND IsEntityClean(EntList[1]) AND
                  (GetEntitySym(EntList[1])=sym) AND (GetEntityIndex(EntList[1])<index)
               THEN
                  IF Debugging
                  THEN
                     WriteString('found node ') ; WriteCard(VAL(CARDINAL, i), 0) ; WriteString(' as a problem') ; WriteLn
                  END ;
                  j := NoOfIds ;
                  WHILE j>1 DO
                     IF IsDirty(i, j) AND (NeedsToBeSaved(EntList[j], Start, End) OR IsReachable(i, j)) AND
                        (GetEntityIndex(EntList[j])<=index)
                     THEN
                        IncludeItemIntoList(l, VAL(CARDINAL, i)) ;
                        (* found a problem case - better flush it now while we can *)
                        CheckNeeded(l, Nodes(i), sym, index, Start, End) ;
                        RemoveItemFromList(l, VAL(CARDINAL, i)) ;
                        (* now clean EntList[j] with EntList[1] *)
                        CleanSymbol(EntList[j], GetSymOfChoice(i)) ;
                        MakeEntityClean(EntList[j]) ;
                        RemoveAllOlderEntities(i, EntList[j])     (* just destroyed all entities < this variable *)
                     END ;
                     DEC(j)
                  END
               END
            END
         END ;
         DEC(i)
      END
   END
END CheckNeeded ;


(*
   CheckEntList - checks all symbols in the EntList of node, n.
*)

PROCEDURE CheckEntList (n: Nodes; index: CARDINAL; Start, End: CARDINAL) ;
VAR
   i: CARDINAL ;
   l: List ;
BEGIN
   WITH NodeList[n] DO
      (* can only clean if this node or leaf lable is clean *)
      IF (EntList[1]#NIL) AND IsEntityClean(EntList[1])
      THEN
         i := NoOfIds ;
         WHILE i>1 DO
            IF IsDirty(n, i) AND (NeedsToBeSaved(EntList[i], Start, End) OR IsReachable(n, i))
            THEN
               IF GetEntityIndex(EntList[i])<=index
               THEN
                  IF Debugging
                  THEN
                     WriteString('index = ') ; WriteCard(index, 0) ; WriteLn
                  END ;
                  InitList(l) ;
                  IncludeItemIntoList(l, VAL(CARDINAL, n)) ;
                  CheckNeeded(l, n, GetEntitySym(EntList[i]), index, Start, End) ;
                  KillList(l) ;
                  CleanSymbol(EntList[i], GetSymOfChoice(n)) ;
                  MakeEntityClean(EntList[i]) ;
                  RemoveAllOlderEntities(n, EntList[i])     (* just destroyed all entities < this variable *)
               ELSIF Debugging
               THEN
                  WriteString('no need to clean symbol yet.. ') ; WriteKey(GetSymName(GetEntitySym(EntList[i]))) ; WriteLn
               END
            ELSIF Debugging
            THEN
               WriteString('no need to clean symbol ') ; WriteKey(GetSymName(GetEntitySym(EntList[i]))) ; WriteLn
            END ;
            DEC(i)
         END
      END
   END
END CheckEntList ;


(*
   CheckDirtySymbols - checks to see whether any dirty symbols exist within tree, n.
                       A dirty symbol is one which is used outside Start..End but which
                       has not yet been saved.
*)

PROCEDURE CheckDirtySymbols (n: Nodes; index: CARDINAL; Start, End: CARDINAL) ;
BEGIN
   IF n#NulNode
   THEN
      WITH NodeList[n] DO
         IF NOT IsLeaf
         THEN
            CheckDirtySymbols(Left, index, Start, End) ;
            CheckDirtySymbols(Right, index, Start, End) ;
         END ;
         CheckEntList(n, index, Start, End)
      END
   END
END CheckDirtySymbols ;


(*
   FlushDirtySymbols - flushes all symbols which have not been stored as a
                       result of the remaining quadruples.
*)

PROCEDURE FlushDirtySymbols (index: CARDINAL; Start, End: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   i := NoOfTrees ;
   WHILE i>0 DO
      IF Debugging
      THEN
         DisplayTree(TreeList[i], 0)
      END ;
      CheckDirtySymbols(TreeList[i], index, Start, End) ;
      DEC(i)
   END
END FlushDirtySymbols ;


(*
   DestroyFreeList - destroys the remainder of the free quadruples.
*)

PROCEDURE DestroyFreeList ;
BEGIN
   WHILE FreeList<=NoOfQuads DO
      WITH RemoveList[FreeList] DO
         IF (NOT IsUsed) AND (QuadNo#0)
         THEN
            IF Debugging
            THEN
               WriteString(', ') ; WriteCard(QuadNo, 0)
            END ;
            SubQuad(QuadNo)
         END
      END ;
      INC(FreeList)
   END
END DestroyFreeList ;


(*
   MakeQuadFromNode - rewrites a quadruple, q, from a node, n.
*)

PROCEDURE MakeQuadFromNode (q: CARDINAL; n: Nodes) ;
BEGIN
   EraseQuad(q) ;
   WITH NodeList[n] DO
      CASE Op OF

      ReturnValueOp     : InternalError('should have been done elsewhere', __FILE__, __LINE__) |
      InclOp,
      ExclOp,
      HighOp,
      FunctValueOp,
      NegateOp,
      SizeOp,
      BecomesOp,
      IndrXOp,
      XIndrOp,
      AddrOp,
      UnboundedOp       : PutQuad(q, Op, GetSym(n), NulSym, GetSymOfChoice(Right)) ;
                          RemoveAllOlderEntities(n, EntList[1]) |
      ElementSizeOp     : PutQuad(q, Op, GetSym(n), GetSymOfChoice(Left), Right) ;
                          RemoveAllOlderEntities(n, EntList[1]) |

      ParamOp           : InternalError('unsure what to do here', __FILE__, __LINE__) |

      CastOp,
      CoerceOp,
      ConvertOp         : InternalError('CoerceOp, CastOp and ConvertOp should not be optimized', __FILE__, __LINE__) |

      OffsetOp,
      ArrayOp,
      AddOp,
      SubOp,
      MultOp,
      LogicalOrOp,
      LogicalAndOp,
      LogicalXorOp,
      LogicalDiffOp,
      LogicalShiftOp,
      LogicalRotateOp,
      ModFloorOp,
      DivFloorOp,
      ModTruncOp,
      DivTruncOp        : PutQuad(q, Op, GetSym(n), GetSymOfChoice(Left), GetSymOfChoice(Right)) ;
                          RemoveAllOlderEntities(n, EntList[1])

      ELSE
         InternalError('not expecting to re make this quadruple type', __FILE__, __LINE__)
      END
   END
END MakeQuadFromNode ;


(*
   CollectSymIfNotValue - searches the NodeList for a possible alias to sym
                          providing it is does not have ValueMode. We choose
                          a real variable not a temporary if possible.

                          Ideally this function should match the function
                          UseQuadAtNode. The CSE will still work fine if the
                          algorithms are not the same, but less efficient code
                          will result.
*)

PROCEDURE CollectSymIfNotValue (tokenno: CARDINAL; sym: CARDINAL) : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   IF GetMode(sym)=LeftValue
   THEN
      (*
         sym will be used for indirection
         - therefore we can only choose a symbol with the same mode
      *)
   ELSE
      WITH NodeList[MakeLeaf(GetLatestEntity(tokenno, sym, FALSE, VAL(CARDINAL, NoOfNodes)))] DO
         i := 1 ;
         (* check to see whether a constant can be found *)
         WHILE i<=NoOfIds DO
            IF (EntList[i]#NIL) AND IsConst(GetEntitySym(EntList[i]))
            THEN
               RETURN( GetEntitySym(EntList[i]) )
            END ;
            INC(i)
         END ;
         i := 1 ;
         WHILE i<=NoOfIds DO
            IF (EntList[i]#NIL) AND IsVar(GetEntitySym(EntList[i])) AND (NOT IsTemporary(GetEntitySym(EntList[i])))
            THEN
               RETURN( GetEntitySym(EntList[i]) )
            END ;
            INC(i)
         END
      END
   END ;
   RETURN( sym )
END CollectSymIfNotValue ;


(*
   CheckNodesWereSorted - checks to see that all nodes are now clean and
                          have been used to reform quadruples.
*)

PROCEDURE CheckNodesWereSorted (Start, End: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   i := NoOfNodes ;
   WHILE i>0 DO
      WITH NodeList[i] DO
         IF NOT IsLeaf
         THEN
            IF (EntList[1]#NIL) AND (NOT IsEntityClean(EntList[1]))
            THEN
               IF Debugging
               THEN
                  WriteString('problems with node (entity[1] should be clean): ') ; WriteCard(i, 4) ; WriteLn ;
                  WriteNode(VAL(Nodes, i)) ; WriteLn
               END ;
               InternalError('this entity should be clean', __FILE__, __LINE__)
            END
         END
      END ;
      DEC(i)
   END
END CheckNodesWereSorted ;


(*
   FlushIndirection - flushes current DAG since some indirection is about to
                      occur. If sym is not the cause of the indirection then
                      we look sym up in the DAG and return the alias.
*)

PROCEDURE FlushIndirection (Start, End: CARDINAL; sym: CARDINAL) : CARDINAL ;
VAR
   q: CARDINAL ;
BEGIN
   (* find End-1 *)
   q := Start ;
   WHILE (q#End) AND (GetNextQuad(q)#End) DO
      q := GetNextQuad(q)
   END ;
   SaveQuad(End) ;  (* this is the quadruple which is performing the indirection *)
   TraverseForest(Start, End) ;
   (* RemoveAllQuads ; *)
   (* FlushDirtySymbols(Start, q) ; *)
   (* ReWriteQuadruples(Start, q) ; *)
   DestroyFreeList ;
   sym := CollectSymIfNotValue(QuadToTokenNo(End), sym) ;
   InitForest ;
   RETURN( sym )
END FlushIndirection ;


(*
   Flush - called at the end of a basic block. It must remove all unused quadruples,
           flush any dirty symbols and rewrite the quadruples with active operands.
*)

PROCEDURE Flush (Start, End: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   TraverseForest(Start, End) ;
   TopologicallySortForest(Start, End) ;
   DestroyFreeList ;
   InitForest
END Flush ;


VAR
   OverrideRange: CARDINAL ;


(*
   RemoveCommonSubExpressions - removes all common subexpressions
                                from the basic block delimited by
                                the quadruples Start..End.
*)

PROCEDURE RemoveCommonSubExpressions (Start, End: CARDINAL) ;
BEGIN
   IF Start>=OverrideRange
   THEN
      InitForest ;
      BuildForest(Start, End) ;
      Flush(Start, End)
   END
END RemoveCommonSubExpressions ;


(*
   Init - detemine whether we should display the debugging
*)

PROCEDURE Init ;
VAR
   s: String ;
BEGIN
   IF GetEnvironment(InitString('OVERRIDE'), s)
   THEN
      OverrideRange := stoi(s)
   ELSE
      OverrideRange := 0
   END ;
   Debugging := GetEnvironment(InitString('DEBUGCSE'), s)
END Init ;


BEGIN
   Init
END M2SubExp.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I.:../gm2-libs:../gm2-libs-ch:../gm2-libiberty/ M2SubExp.mod"
 * End:
 *)
