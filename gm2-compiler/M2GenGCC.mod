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
IMPLEMENTATION MODULE M2GenGCC ;

FROM SYSTEM IMPORT ADDRESS ;

FROM M2Quads IMPORT QuadOperator, GetQuad, IsReferenced, GetNextQuad,
                    SubQuad, PutQuad,
                    QuadToTokenNo,
                    DisplayQuadList ;

FROM SymbolTable IMPORT PushSize, PopSize, PushValue, PopValue,
                        PushVarSize,
                        PushSumOfLocalVarSize,
                        PushSumOfParamSize,
                        MakeConstLit,
                        GetMainModule, GetScopeAuthor,
                        GetVarFather, GetSymName, ModeOfAddr, GetMode,
                        GetGnuAsm, IsGnuAsmVolatile,
                        GetGnuAsmInput, GetGnuAsmOutput, GetGnuAsmTrash,
                        GetLocalSym,
                        NoOfParam, Father,
                        IsModule, IsType,
                        IsConstString, GetString, GetStringLength,
                        IsConst, IsConstSet, IsProcedure, IsProcType, IsProcedureNested,
                        IsVar, IsVarParam,
                        IsUnbounded, IsArray, IsSet,
                        IsProcedureVariable,
                        IsUnboundedParam,
                        IsRecordField, IsFieldVarient, IsVarient, IsRecord,
                        IsExportQualified,
                        IsExported,
                        IsSubrange,
                        IsProcedureBuiltin,
                        IsValueSolved, IsSizeSolved,
                        ForeachExportedDo,
                        ForeachImportedDo,
                        ForeachProcedureDo,
                        ForeachInnerModuleDo,
                        GetType, GetNth, GetNthParam,
                        GetSubrange, NoOfElements,
                        GetFirstUsed, GetDeclared,
                        GetRegInterface,
                        GetProcedureQuads,
                        GetProcedureBuiltin,
                        PutConstString,
                        PutConst, PutConstSet,
                        NulSym ;

FROM M2GCCDeclare IMPORT GetTypeMin, GetTypeMax ;
FROM M2Debug IMPORT Assert ;
FROM M2Error IMPORT InternalError, WriteFormat0, WriteFormat1, ErrorStringAt, WarnStringAt ;
FROM M2Options IMPORT DisplayQuadruples ;
FROM M2Printf IMPORT printf0, printf2 ;

FROM M2Base IMPORT MixTypes, ActivationPointer, IsMathType, ArrayHigh, ArrayAddress, Cardinal, Char, Integer, Unbounded, Trunc ;
FROM NameKey IMPORT Name, MakeKey, KeyToCharStar, NulName ;
FROM Strings IMPORT string, InitString, KillString, String, InitStringCharStar, Mark ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2 ;
FROM M2System IMPORT Address, Word ;
FROM M2FileName IMPORT CalculateFileName ;
FROM M2AsmUtil IMPORT GetModuleInitName ;
FROM SymbolConversion IMPORT AddModGcc, Mod2Gcc, GccKnowsAbout ;
FROM Lists IMPORT RemoveItemFromList, IncludeItemIntoList, NoOfItemsInList, GetItemFromList ;

FROM M2ALU IMPORT PtrToValue,
                  IsValueTypeReal,
                  PushIntegerTree, PopIntegerTree, PopSetTree, PopRealTree, PushCard, Gre, Sub, Equ, NotEqu, LessEqu,
                  BuildRange, SetOr, SetAnd, SetNegate, SetSymmetricDifference, SetDifference,
                  AddBit, SubBit, Less, Addn, GreEqu, SetIn, GetRange, GetValue ;

FROM M2GCCDeclare IMPORT StartDeclareMainModule, EndDeclareMainModule, DeclareConstant,
                         DeclareLocalVariables, PromoteToString, CompletelyResolved ;

FROM gm2builtins IMPORT BuiltInMemCopy, BuiltInAlloca,
                        GetBuiltinConst,
                        BuiltinExists, BuildBuiltinTree ;

FROM gccgm2 IMPORT Tree, GetIntegerZero, GetIntegerOne, GetIntegerType,
                   BuildVariableArrayAndDeclare,
                   BuildBinProcedure, BuildUnaryProcedure,
                   ChainOnParamValue, AddStringToTreeList,
                   SetFileNameAndLineNo, EmitLineNote, BuildStart, BuildEnd,
                   BuildCallInnerInit,
                   BuildStartFunctionCode, BuildEndFunctionCode, BuildReturnValueCode,
                   BuildAssignment, DeclareKnownConstant,
                   BuildAdd, BuildSub, BuildMult, BuildDiv, BuildMod, BuildLSL,
                   BuildLogicalOr, BuildLogicalAnd, BuildSymmetricDifference,
                   BuildNegate, BuildAddr, BuildSize, BuildOffset,
                   BuildGoto, DeclareLabel,
                   BuildLessThan, BuildGreaterThan,
                   BuildLessThanOrEqual, BuildGreaterThanOrEqual,
                   BuildEqualTo, BuildNotEqualTo,
                   BuildIsSuperset, BuildIsNotSuperset,
                   BuildIsSubset, BuildIsNotSubset,
                   BuildIfIn, BuildIfNotIn,
                   BuildIndirect,
                   BuildConvert, BuildTrunc,
                   BuildBinaryForeachWordDo,
                   BuildUnaryForeachWordDo,
                   BuildExcludeVarConst, BuildIncludeVarConst,
                   BuildExcludeVarVar, BuildIncludeVarVar,
                   BuildIfConstInVar, BuildIfNotConstInVar,
                   BuildIfVarInVar, BuildIfNotVarInVar,
                   BuildIfInRangeGoto, BuildIfNotInRangeGoto,
                   BuildForeachWordInSetDoIfExpr,
                   AreConstantsEqual, CompareTrees,
                   DoJump,
                   BuildProcedureCall, BuildIndirectProcedureCall, BuildParam, BuildFunctValue,
                   BuildAsm, DebugTree,
                   BuildSetNegate,
                   ExpandExpressionStatement,
                   GetPointerType, GetWordType,
                   GetBitsPerWord,
                   RememberConstant ;

FROM SYSTEM IMPORT WORD ;

CONST
   Debugging = FALSE ;

TYPE
   DoProcedure      = PROCEDURE (CARDINAL) ;
   DoUnaryProcedure = PROCEDURE (CARDINAL) ;

VAR
   CurrentProcedure,
   PreviousScope            : CARDINAL ;(* the current procedure being compiled   *)
   CurrentQuad              : CARDINAL ;
   CurrentQuadToken         : CARDINAL ;
   AbsoluteHead             : CARDINAL ;
   LastLine                 : CARDINAL ;(* The Last Line number emmitted with the *)
                                        (* generated code.                        *)
   CompilingMainModule      : BOOLEAN ; (* Determines whether the main module     *)
                                        (* quadrules are being processed.         *)
   LastOperator             : QuadOperator ; (* The last operator processed.      *)
   ModuleName,
   FileName                 : String ;
   CurrentModuleInitFunction: Tree ;


(*
   Rules for Quadruples
   ====================

   Rules
   =====

   All program declared variables are given the mode, Offset.
   All constants have mode, Immediate.

   Operators
   =========

------------------------------------------------------------------------------
   Array Operators
------------------------------------------------------------------------------
   Sym<I>   Base   a            Delivers a constant result if a is a
                                Global variable. If a is a local variable
                                then the Frame pointer needs to be added.
                                Base yields the effective location in memory
                                of, a, array [0,0, .. ,0] address.
   Sym<I>   ElementSize 1       Always delivers a constant. The number
                                indicates which specified element is chosen.
                                ElementSize is the TypeSize for that element.
   Unbounded  Op1 Op3           Initializes the op1 StartAddress of the array
                                op3. Op3 can be a normal array or unbounded array.
                                op1 (is the Unbounded.ArrayAddress) := ADR(op3).
                                In GNU Modula-2 the callee saves non var unbounded
                                arrays. This is direct contrast to the M2F native
                                code generators.
------------------------------------------------------------------------------
   := Operator
------------------------------------------------------------------------------
   Sym1<I> := Sym3<I>           := produces a constant
   Sym1<O> := Sym3<O>           := has the effect Mem[Sym1<I>] := Mem[Sym3<I>]
------------------------------------------------------------------------------
   Addr Operator  - contains the address of a variable - may need to add
------------------------------------------------------------------------------
   Yields the address of a variable - need to add the frame pointer if
   a variable is local to a procedure.

   Sym1<O>   Addr   Sym2<O>     meaning     Mem[Sym1<I>] := Sym2<I>
   Sym1<V>   Addr   Sym2<O>     meaning     Mem[Sym1<I>] := Sym2<I>
   Sym1<O>   Addr   Sym2<V>     meaning     Mem[Sym1<I>] := Mem[Sym2<I>]
   Sym1<V>   Addr   Sym2<V>     meaning     Mem[Sym1<I>] := Mem[Sym2<I>]
------------------------------------------------------------------------------
   Xindr Operator  ( *a = b)
------------------------------------------------------------------------------
   Sym1<O>   Copy   Sym2<I>     Meaning     Mem[Sym1<I>] := constant
   Sym1<V>   Copy   Sym2<I>     Meaning     Mem[Sym1<I>] := constant
   Sym1<O>   Copy   Sym2<O>     meaning     Mem[Sym1<I>] := Mem[Sym2<I>]
   Sym1<V>   Copy   Sym2<O>     meaning     Mem[Sym1<I>] := Mem[Sym2<I>]
   Sym1<O>   Copy   Sym2<V>     meaning     Mem[Sym1<I>] := Mem[Mem[Sym2<I>]]
   Sym1<V>   Copy   Sym2<V>     meaning     Mem[Sym1<I>] := Mem[Mem[Sym2<I>]]
------------------------------------------------------------------------------
   IndrX Operator  (a = *b)   where <X> means any value
------------------------------------------------------------------------------
   Sym1<X>   IndrX  Sym2<I>     meaning     Mem[Sym1<I>] := Mem[constant]
   Sym1<X>   IndrX  Sym2<I>     meaning     Mem[Sym1<I>] := Mem[constant]

   Sym1<X>   IndrX  Sym2<X>     meaning     Mem[Sym1<I>] := Mem[Mem[Sym2<I>]]
   Sym1<X>   IndrX  Sym2<X>     meaning     Mem[Sym1<I>] := Mem[Mem[Sym2<I>]]
------------------------------------------------------------------------------
   + - / * Operators
------------------------------------------------------------------------------
   Sym1<I>   +      Sym2<I>  Sym3<I>  meaning Sym1<I> := Sym2<I> + Sym3<I>
   Sym1<O>   +      Sym2<O>  Sym3<I>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Sym3<I>
   Sym1<O>   +      Sym2<O>  Sym3<O>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Mem[Sym3<I>]
   Sym1<O>   +      Sym2<O>  Sym3<V>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Mem[Sym3<I>]
   Sym1<V>   +      Sym2<O>  Sym3<V>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Mem[Sym3<I>]
   Sym1<V>   +      Sym2<V>  Sym3<V>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Mem[Sym3<I>]
------------------------------------------------------------------------------
   Base Operator
------------------------------------------------------------------------------
   Sym1<O>   Base  Sym2   Sym3<O>     meaning     Mem[Sym1<I>] := Sym3<I>
   Sym1<V>   Base  Sym2   Sym3<O>     meaning     Should Never Occur But If it did..
                                                  Mem[Mem[Sym1<I>]] := Sym3<I>
   Sym1<O>   Base  Sym2   Sym3<V>     meaning     Mem[Sym1<I>] := Mem[Sym3<I>]
   Sym1<V>   Base  Sym2   Sym3<V>     meaning     Should Never Occur But If it did..
                                                  Mem[Mem[Sym1<I>]] := Mem[Sym3<I>]
                   Sym2 is the array type
------------------------------------------------------------------------------

Notes

1)   All references to memory Mem[Sym1<I>] may need to add the base
     pointer of the activation record if the symbol is a procedure variable.

2)   Addr provides the address of variables and therefore can be used to
     perform the pseudo system call function ADR( variable ), provided
     one obeys rule (1).
*)

(* To keep p2c happy we declare forward procedures *)

(* %%%FORWARD%%%
PROCEDURE CheckStop (q: CARDINAL) ; FORWARD ;
PROCEDURE stop ; FORWARD ;
PROCEDURE CodeStart ; FORWARD ;
PROCEDURE CodeEnd ; FORWARD ;
PROCEDURE CodeStartModFile ; FORWARD ;
PROCEDURE CodeStartDefFile ; FORWARD ;
PROCEDURE CodeEndFile ; FORWARD ;
PROCEDURE CodeStatement ; FORWARD ;
PROCEDURE CodeLineNumber ; FORWARD ;
PROCEDURE CodeNewLocalVar ; FORWARD ;
PROCEDURE CodeKillLocalVar ; FORWARD ;
PROCEDURE CodeReturnValue ; FORWARD ;
PROCEDURE CodeReturn ; FORWARD ;
PROCEDURE FoldBecomes (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeBecomes ; FORWARD ;
PROCEDURE FoldAdd (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeAdd ; FORWARD ;
PROCEDURE FoldSub (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeSub ; FORWARD ;
PROCEDURE FoldMult (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeMult ; FORWARD ;
PROCEDURE FoldDiv (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeDiv ; FORWARD ;
PROCEDURE FoldMod (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeMod ; FORWARD ;
PROCEDURE FoldBitRange (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE FoldBit (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeBit ; FORWARD ;
PROCEDURE CodeGoto ; FORWARD ;
PROCEDURE CheckReferenced (q: CARDINAL; op: QuadOperator) ; FORWARD ;
PROCEDURE FoldSetOr (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE FoldSetAnd (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeSymmetricDifference ; FORWARD ;
PROCEDURE FoldSymmetricDifference (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE FoldNegate (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeNegate ; FORWARD ;
PROCEDURE FoldSize (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeSize ; FORWARD ;
PROCEDURE CodeAddr ; FORWARD ;
PROCEDURE CodeIfLess ; FORWARD ;
PROCEDURE CodeIfLessEqu ; FORWARD ;
PROCEDURE CodeIfGre ; FORWARD ;
PROCEDURE CodeIfGreEqu ; FORWARD ;
PROCEDURE CodeIfEqu ; FORWARD ;
PROCEDURE CodeIfNotEqu ; FORWARD ;
PROCEDURE CodeIfNotIn ; FORWARD ;
PROCEDURE CodeIfIn ; FORWARD ;
PROCEDURE CodeIndrX ; FORWARD ;
PROCEDURE CodeXIndr ; FORWARD ;
PROCEDURE CodeCall ; FORWARD ;
PROCEDURE CodeDirectCall ; FORWARD ;
PROCEDURE CodeIndirectCall ; FORWARD ;
PROCEDURE CodeParam ; FORWARD ;
PROCEDURE CodeFunctValue ; FORWARD ;
PROCEDURE CodeInline ; FORWARD ;
PROCEDURE CodeOffset ; FORWARD ;
PROCEDURE FoldOffset (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeHigh ; FORWARD ;
PROCEDURE FoldHigh (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeUnbounded ; FORWARD ;
PROCEDURE FoldBase (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeBase ; FORWARD ;
PROCEDURE FoldElementSize (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeElementSize ; FORWARD ;
PROCEDURE FoldCoerce (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeCoerce ; FORWARD ;
PROCEDURE FoldConvert (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeConvert ; FORWARD ;
PROCEDURE CodeMath ; FORWARD ;
PROCEDURE CodeSetOr ; FORWARD ;
PROCEDURE CodeSetAnd ; FORWARD ;
PROCEDURE CodeSetSymmetricDifference ; FORWARD ;
PROCEDURE CodeIncl ; FORWARD ;
PROCEDURE CodeExcl ; FORWARD ;
PROCEDURE FoldIncl (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE FoldExcl (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE FoldIfIn (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE FoldIfNotIn (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE FoldBuiltinConst (tokenno: CARDINAL; quad: CARDINAL; l: List) ; FORWARD ;
   %%%FORWARD%%% *)


(*
   InitGNUM2 - initializes the GCC SymbolTable with Modula-2 base types.
               Also initialize the start of quadruples.
*)

PROCEDURE InitGNUM2 (Head: CARDINAL) ;
BEGIN
   AbsoluteHead := Head ;
   CurrentQuad  := Head ;
   StartDeclareMainModule
END InitGNUM2 ;


(*
   ConvertQuadsToTree - runs through the quadruple list and converts it into
                        the GCC tree structure.
*)

PROCEDURE ConvertQuadsToTree (Head: CARDINAL) ;
BEGIN
   REPEAT
      CodeStatement
   UNTIL CurrentQuad=0
END ConvertQuadsToTree ;


(*
   CodeStatement - A multi-way decision call depending on the current
                   quadruple.
*)

PROCEDURE CodeStatement ;
VAR
   Operator: QuadOperator ;
   Operand1,
   Operand2,
   Operand3: CARDINAL ;
BEGIN
   (* WriteString('// Quad') ; WriteCard(CurrentQuad, 6) ; WriteLn ; *)
   GetQuad(CurrentQuad, Operator, Operand1, Operand2, Operand3) ;
   CurrentQuadToken := QuadToTokenNo(CurrentQuad) ;
   CheckReferenced(CurrentQuad, Operator) ;
   CheckStop(CurrentQuad) ;
   IF CurrentQuad=349
   THEN
      stop
   END ;

   CASE Operator OF

   StartDefFileOp     : CodeStartDefFile |
   StartModFileOp     : CodeStartModFile |
   EndFileOp          : CodeEndFile |
   StartOp            : CodeStart |
   EndOp              : CodeEnd |
   NewLocalVarOp      : CodeNewLocalVar |
   KillLocalVarOp     : CodeKillLocalVar |
   ReturnOp           : CodeReturn |
   ReturnValueOp      : CodeReturnValue |
   DummyOp            : |
   BecomesOp          : CodeBecomes |
   AddOp              : CodeAdd |
   SubOp              : CodeSub |
   MultOp             : CodeMult |
   DivOp              : CodeDiv |
   ModOp              : CodeMod |
   GotoOp             : CodeGoto |
   InclOp             : CodeIncl |
   ExclOp             : CodeExcl |
   NegateOp           : CodeNegate |
   LogicalOrOp        : CodeSetOr |
   LogicalAndOp       : CodeSetAnd |
   LogicalXorOp       : CodeSetSymmetricDifference |
   LogicalDiffOp      : printf0('to be done\') |
   IfLessOp           : CodeIfLess |
   IfEquOp            : CodeIfEqu |
   IfNotEquOp         : CodeIfNotEqu |
   IfGreEquOp         : CodeIfGreEqu |
   IfLessEquOp        : CodeIfLessEqu |
   IfGreOp            : CodeIfGre |
   IfInOp             : CodeIfIn |
   IfNotInOp          : CodeIfNotIn |
   IndrXOp            : CodeIndrX |
   XIndrOp            : CodeXIndr |
   CallOp             : CodeCall |
   ParamOp            : CodeParam |
   FunctValueOp       : CodeFunctValue |
   AddrOp             : CodeAddr |
   SizeOp             : CodeSize |
   UnboundedOp        : CodeUnbounded |
   OffsetOp           : CodeOffset |
   HighOp             : CodeHigh |
   BaseOp             : CodeBase |
   ElementSizeOp      : CodeElementSize |
   ConvertOp          : CodeConvert |
   CoerceOp           : CodeCoerce |

   (* And the Compiler Flag Pseudo Quadruples *)
   InlineOp           : CodeInline |
   LineNumberOp       : CodeLineNumber |
   CodeOnOp           : |           (* the following make no sense with gcc *)
   CodeOffOp          : |
   ProfileOnOp        : |
   ProfileOffOp       : |
   OptimizeOnOp       : |
   OptimizeOffOp      :

   ELSE
      WriteFormat1('quadruple %d not yet implemented', CurrentQuad) ;
      InternalError('quadruple not implemented yet', __FILE__, __LINE__)
   END ;
   LastOperator := Operator ;
   CurrentQuad := GetNextQuad(CurrentQuad)
END CodeStatement ;


(*
   DeclareConstantLiterals - declares all constant literals in a list.
*)

PROCEDURE DeclareConstantLiterals (l: List) ;
VAR
   i, n: CARDINAL ;
   sym : CARDINAL ;
BEGIN
   n := NoOfItemsInList(l) ;
   i := 1 ;
   WHILE i<=n DO
      sym := GetItemFromList(l, i) ;
      IF Debugging
      THEN
         printf2('trying to declare constant %a <%d>', GetSymName(sym), sym)
      END ;
      IF (sym#NulSym) AND IsConst(sym)
      THEN
         IF (NOT IsValueSolved(sym)) AND IsConstSet(sym)
         THEN
            IF Debugging
            THEN
               printf0(' <unresolved const set>')
            END ;
            
         END ;
         IF IsValueSolved(sym)
         THEN
            DeclareConstant(GetDeclared(sym), sym) ;
            RemoveItemFromList(l, sym) ;
            IF Debugging
            THEN
               printf0(' done')
            END
         END
      END ;
      IF Debugging
      THEN
         printf0('\n')
      END ;
      INC(i)
   END
END DeclareConstantLiterals ;


(*
   ResolveConstantExpressions - resolves constant expressions from the quadruple list.
                                It returns TRUE if one or more constants were folded.
*)

PROCEDURE ResolveConstantExpressions (l: List) : BOOLEAN ;
VAR
   tokenno : CARDINAL ;
   quad    : CARDINAL ;
   Operator: QuadOperator ;
   Operand1,
   Operand2,
   Operand3: CARDINAL ;
   Orig,
   Next,
   Last    : CARDINAL ;
BEGIN
   DeclareConstantLiterals(l) ;
   Orig := NoOfItemsInList(l) ;
   Next := Orig ;
   REPEAT
      quad := AbsoluteHead ;
      Last := Next ;
      WHILE quad#0 DO
         GetQuad(quad, Operator, Operand1, Operand2, Operand3) ;
         tokenno := QuadToTokenNo(quad) ;

         CASE Operator OF

         BuiltinConstOp     : FoldBuiltinConst(tokenno, quad, l) |
         LogicalOrOp        : FoldSetOr(tokenno, quad, l) |
         LogicalAndOp       : FoldSetAnd(tokenno, quad, l) |
         LogicalXorOp       : FoldSymmetricDifference(tokenno, quad, l) |
         BecomesOp          : FoldBecomes(tokenno, quad, l) |
         AddOp              : FoldAdd(tokenno, quad, l) |
         SubOp              : FoldSub(tokenno, quad, l) |
         MultOp             : FoldMult(tokenno, quad, l) |
         DivOp              : FoldDiv(tokenno, quad, l) |
         ModOp              : FoldMod(tokenno, quad, l) |
         NegateOp           : FoldNegate(tokenno, quad, l) |
         SizeOp             : FoldSize(tokenno, quad, l) |
         OffsetOp           : FoldOffset(tokenno, quad, l) |
         HighOp             : FoldHigh(tokenno, quad, l) |
         BaseOp             : FoldBase(tokenno, quad, l) |
         ElementSizeOp      : FoldElementSize(tokenno, quad, l) |
         ConvertOp          : FoldConvert(tokenno, quad, l) |
         CoerceOp           : FoldCoerce(tokenno, quad, l) |
         InclOp             : FoldIncl(tokenno, quad, l) |
         ExclOp             : FoldExcl(tokenno, quad, l) |
         IfInOp             : FoldIfIn(tokenno, quad, l) |
         IfNotInOp          : FoldIfNotIn(tokenno, quad, l)

         ELSE
            (* ignore quadruple as it is not associated with a constant expression *)
         END ;
         quad := GetNextQuad(quad)
      END ;
      Next := NoOfItemsInList(l)
   UNTIL Last=Next ;
   IF Debugging AND DisplayQuadruples
   THEN
      printf0('after resolving expressions with gcc\n') ;
      DisplayQuadList(AbsoluteHead) ;
   END ;
   RETURN( Orig#Last )
END ResolveConstantExpressions ;


(*
   FindSize - given a Modula-2 symbol, sym, return the GCC Tree (constant) representing
              the storage size in bytes.
*)

PROCEDURE FindSize (sym: CARDINAL) : Tree ;
BEGIN
   IF IsConstString(sym)
   THEN
      PushCard(GetStringLength(sym)) ;
      RETURN( PopIntegerTree() )
   ELSIF IsSizeSolved(sym)
   THEN
      PushSize(sym) ;
      RETURN( PopIntegerTree() )
   ELSE
      IF GccKnowsAbout(sym)
      THEN
         PushIntegerTree(BuildSize(Mod2Gcc(sym), FALSE)) ;
         PopSize(sym) ;
         PushSize(sym) ;
         RETURN( PopIntegerTree() )
      ELSE
         InternalError('expecting gcc to already know about this symbol', __FILE__, __LINE__)
      END
   END
END FindSize ;


(*
   FindType - returns the type of, Sym, if Sym is a TYPE then return Sym otherwise return GetType(Sym)
*)

PROCEDURE FindType (Sym: CARDINAL) : CARDINAL ;
BEGIN
   IF IsType(Sym)
   THEN
      RETURN( Sym )
   ELSE
      RETURN( GetType(Sym) )
   END
END FindType ;


(*
   BuildTreeFromInterface - generates a GCC tree from an interface definition.
*)

PROCEDURE BuildTreeFromInterface (sym: CARDINAL) : Tree ;
VAR
   n   : CARDINAL ;
   str,
   obj : CARDINAL ;
   tree: Tree ;
BEGIN
   tree := Tree(NIL) ;
   IF sym#NulSym
   THEN
      n := 1 ;
      REPEAT
         GetRegInterface(sym, n, str, obj) ;
         IF str#NulSym
         THEN
            IF IsConstString(str)
            THEN
               DeclareConstant(GetDeclared(obj), obj) ;
               tree := ChainOnParamValue(tree, PromoteToString(GetDeclared(str), str), Mod2Gcc(obj))
            ELSE
               WriteFormat0('a constraint to the GNU ASM statement must be a constant string')
            END
         END ;
         INC(n)
      UNTIL (str=NulSym) AND (obj=NulSym) ;
   END ;
   RETURN( tree )
END BuildTreeFromInterface ;


(*
   BuildTrashTreeFromInterface - generates a GCC string tree from an interface definition.
*)

PROCEDURE BuildTrashTreeFromInterface (sym: CARDINAL) : Tree ;
VAR
   n   : CARDINAL ;
   str,
   obj : CARDINAL ;
   tree: Tree ;
BEGIN
   tree := Tree(NIL) ;
   IF sym#NulSym
   THEN
      n := 1 ;
      REPEAT
         GetRegInterface(sym, n, str, obj) ;
         IF str#NulSym
         THEN
            IF IsConstString(str)
            THEN
               tree := AddStringToTreeList(tree, PromoteToString(GetDeclared(str), str))
            ELSE
               WriteFormat0('a constraint to the GNU ASM statement must be a constant string')
            END
         END ;
         IF obj#NulSym
         THEN
            InternalError('not expecting the object to be non null in the trash list', __FILE__, __LINE__)
         END ;
         INC(n)
      UNTIL (str=NulSym) AND (obj=NulSym)
   END ;
   RETURN( tree )
END BuildTrashTreeFromInterface ;


(*
   CodeInline - InlineOp is a quadruple which has the following format:

                InlineOp   NulSym  NulSym  Sym

                The inline asm statement, Sym, is written to standard output.
*)

PROCEDURE CodeInline ;
VAR
   Operator: QuadOperator ;
   Operand1,
   Operand2,
   string,
   GnuAsm  : CARDINAL ;
   inputs,
   outputs,
   trash   : Tree ;
BEGIN
   (*
      no need to explicity flush the outstanding instructions as
      per M2GenDyn486 and M2GenAPU. The GNU ASM statements in GCC
      can handle the register dependency providing the user
      specifies VOLATILE and input/output/trash sets correctly.
   *)
   GetQuad(CurrentQuad, Operator, Operand1, Operand2, GnuAsm) ;
   inputs  := BuildTreeFromInterface(GetGnuAsmInput(GnuAsm)) ;
   outputs := BuildTreeFromInterface(GetGnuAsmOutput(GnuAsm)) ;
   trash   := BuildTrashTreeFromInterface(GetGnuAsmTrash(GnuAsm)) ;
   string  := GetGnuAsm(GnuAsm) ;
   DeclareConstant(CurrentQuadToken, string) ;
   BuildAsm(Mod2Gcc(string), IsGnuAsmVolatile(GnuAsm), inputs, outputs, trash)
END CodeInline ;


(*
   CodeLineNumber - LineNumberOp is a quadruple which has the following format:

                    LineNumberOp   NulSym  NulSym  LineNo

                    The line number, LineNo, is ommited if we are producing
                    runtime debugging information.
*)

PROCEDURE CodeLineNumber ;
VAR
   Operator  : QuadOperator ;
   Operand1,
   Operand2,
   Operand3  : CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, Operator, Operand1, Operand2, Operand3) ;
   IF LastOperator#LineNumberOp
   THEN
      FileName := KillString(FileName) ;
      FileName := InitStringCharStar(KeyToCharStar(Name(Operand1))) ;
      SetFileNameAndLineNo(string(FileName), Operand3) ;
      EmitLineNote(string(FileName), Operand3)
   END
END CodeLineNumber ;


(*
   CodeStartModFile - StartModFileOp is a quadruple which has the following
                      format:

                      StartModFileOp  _  _  ModuleSym

                      Its function is to reset the source file to another
                      file, hence all line numbers emmitted with the
                      generated code will be relative to this source file.
*)

PROCEDURE CodeStartModFile ;
VAR
   Operator: QuadOperator ;
   Operand1,
   Operand2,
   Operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, Operator, Operand1, Operand2, Operand3) ;
   LastLine := 1 ;
   CompilingMainModule := GetMainModule()=Operand3 ;
   ModuleName := KillString(ModuleName) ;
   ModuleName := InitStringCharStar(KeyToCharStar(GetSymName(Operand3))) ;
   IF CompilingMainModule
   THEN
      SetFileNameAndLineNo(KeyToCharStar(Name(Operand2)), Operand1) ;
      EmitLineNote(KeyToCharStar(Name(Operand2)), Operand1)
   END
END CodeStartModFile ;


(*
   CodeStartDefFile - StartDefFileOp is a quadruple which has the following
                      format:

                      StartDefFileOp  _  _  ModuleSym

                      Its function is to reset the source file to another
                      file, hence all line numbers emmitted with the
                      generated code will be relative to this source file.
*)

PROCEDURE CodeStartDefFile ;
VAR
   Operator: QuadOperator ;
   Operand1,
   Operand2,
   Operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, Operator, Operand1, Operand2, Operand3) ;
   LastLine := 1 ;
   CompilingMainModule := FALSE ;
   ModuleName := KillString(ModuleName) ;
   ModuleName := InitStringCharStar(KeyToCharStar(GetSymName(Operand3)))
END CodeStartDefFile ;


(*
   CodeEndFile - FileOp is a quadruple which has the following format:

                 EndFileOp  _  _  ModuleSym

                 Its function is to reset the source file to another
                 file, hence all line numbers emmitted with the
                 generated code will be relative to this source file.
*)

PROCEDURE CodeEndFile ;
BEGIN
   IF CompilingMainModule
   THEN
      EndDeclareMainModule
   END
END CodeEndFile ;


(*
   CallInnerInit - produce a call to inner module initialization routine.
*)

PROCEDURE CallInnerInit (Sym: WORD) ;
BEGIN
   BuildCallInnerInit(Mod2Gcc(Sym))
END CallInnerInit ;


(*
   CodeStart - emits starting code before the main BEGIN END of the
               current module.
*)

PROCEDURE CodeStart ;
VAR
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, op, op1, op2, op3) ;
   IF CompilingMainModule
   THEN
      SetFileNameAndLineNo(string(FileName), op1) ;
      CurrentModuleInitFunction := BuildStart(KeyToCharStar(GetModuleInitName(op3)), op1, op2#op3) ;
      AddModGcc(op3, CurrentModuleInitFunction) ;
      EmitLineNote(string(FileName), op1) ;
      ForeachInnerModuleDo(op3, CallInnerInit)
   END
END CodeStart ;


(*
   CodeEnd - emits terminating code after the main BEGIN END of the
             current module.
*)

PROCEDURE CodeEnd ;
VAR
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   IF CompilingMainModule
   THEN
      GetQuad(CurrentQuad, op, op1, op2, op3) ;
      SetFileNameAndLineNo(string(FileName), op1) ;
      EmitLineNote(string(FileName), op1) ;
      BuildEnd(CurrentModuleInitFunction)
   END
END CodeEnd ;


(*
   MakeCopyAndUse - make a copy of the unbounded array and alter all references
                    from the old unbounded array to the new unbounded array.
                    The parameter, param, contains a RECORD
                                                        ArrayAddress: ADDRESS ;
                                                        ArrayHigh   : CARDINAL ;
                                                     END
                    we simply declare a new array of size, ArrayHigh
                    and set ArrayAddress to the address of the copy.

                    Remember ArrayHigh == sizeof(Array)-sizeof(typeof(array))
                             so we add 1 for the size and add 1 for a possible <nul>
*)

PROCEDURE MakeCopyAndUse (proc, param, i: CARDINAL) ;
VAR
   ArrayType: CARDINAL ;
   t,
   Addr,
   High,
   GccIndex,
   GccArray,
   NewArray,
   Type     : Tree ;
BEGIN
   Assert(IsUnbounded(GetType(param))) ;
   ArrayType := GetType(GetType(param)) ;

   (* remember we must add one as HIGH(a) means we can legally reference a[HIGH(a)] *)
   High      := BuildMult(BuildAdd(
                                   BuildAdd(BuildIndirect(BuildAdd(BuildAddr(Mod2Gcc(param), FALSE),
                                                                   BuildOffset(Mod2Gcc(GetLocalSym(Unbounded, ArrayHigh)), FALSE),
                                                                   FALSE),
                                                          GetIntegerType()),
                                            GetIntegerOne(),
                                            FALSE),
                                   GetIntegerOne(),
                                   FALSE),
                          FindSize(ArrayType), FALSE) ;

   Addr      := BuildIndirect(BuildAdd(BuildAddr(Mod2Gcc(param), FALSE),
                                       BuildOffset(Mod2Gcc(GetLocalSym(Unbounded, ArrayAddress)), FALSE),
                                       FALSE),
                              GetIntegerType()) ;

   Type      := Tree(Mod2Gcc(GetType(param))) ;
   
   NewArray  := BuiltInAlloca(High) ;
   NewArray  := BuiltInMemCopy(NewArray, Addr, High) ;
   (* now assign  param.Addr := ADR(NewArray) *)
   t         := BuildAssignment(BuildIndirect(BuildAdd(BuildAddr(Mod2Gcc(param), FALSE),
                                                       BuildOffset(Mod2Gcc(GetLocalSym(Unbounded, ArrayAddress)), FALSE),
                                                       FALSE),
                                              GetPointerType()), NewArray)
END MakeCopyAndUse ;


(*
   IsUnboundedWrittenTo - returns TRUE if the unbounded parameter might be written to.
                          Note that we er on the side of caution. If any indirection
                          occurs during the procedure which is not accounted for then we
                          must assume the worst and that the user might be writing to the
                          array.
*)

PROCEDURE IsUnboundedWrittenTo (proc, param: CARDINAL) : BOOLEAN ;
VAR
   start, end: CARDINAL ;
BEGIN
   GetProcedureQuads(proc, start, end) ;
   RETURN( TRUE )
END IsUnboundedWrittenTo ;


(*
   CheckUnboundedNonVarParameter - if non var unbounded parameter is written to
                                   then
                                      make a copy of the contents of this parameter
                                      and use the copy
                                   fi

*)

PROCEDURE CheckUnboundedNonVarParameter (proc, param, i: CARDINAL) ;
BEGIN
   IF IsUnboundedWrittenTo(proc, param)
   THEN
      MakeCopyAndUse(proc, param, i)
   END
END CheckUnboundedNonVarParameter ;


(*
   SaveNonVarUnboundedParameters - for each parameter of procedure, sym, do
                                      if non var unbounded parameter is written to
                                      then
                                         make a copy of the contents of this parameter
                                         and use the copy
                                      fi
                                   done
*)

PROCEDURE SaveNonVarUnboundedParameters (proc: CARDINAL) ;
VAR
   i, p: CARDINAL ;
BEGIN
   i := 1 ;
   p := NoOfParam(proc) ;
   WHILE i<=p DO
      IF IsUnboundedParam(proc, i) AND (NOT IsVarParam(proc, i))
      THEN
         CheckUnboundedNonVarParameter(proc, GetNth(proc, i), i)
      END ;
      INC(i)
   END
END SaveNonVarUnboundedParameters ;


(*
   CodeNewLocalVar - Builds a new frame on the stack to contain the procedure
                     local variables.
*)

PROCEDURE CodeNewLocalVar ;
VAR
   Operator: QuadOperator ;
   Operand1: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, Operator, Operand1, PreviousScope, CurrentProcedure) ;
   ModuleName := KillString(ModuleName) ;
   ModuleName := InitStringCharStar(KeyToCharStar(GetSymName(GetMainModule()))) ;
   SetFileNameAndLineNo(string(FileName), Operand1) ;
   BuildStartFunctionCode(Mod2Gcc(CurrentProcedure), IsExported(GetMainModule(), CurrentProcedure)) ;
   DeclareLocalVariables(CurrentProcedure) ;
   (* callee saves non var unbounded parameter contents *)
   SaveNonVarUnboundedParameters(CurrentProcedure) ;
   EmitLineNote(string(FileName), Operand1)
END CodeNewLocalVar ;


(*
   CodeKillLocalVar - removes local variables and returns to previous scope.
*)

PROCEDURE CodeKillLocalVar ;
VAR
   Operator: QuadOperator ;
   Operand1,
   Operand2: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, Operator, Operand1, Operand2, CurrentProcedure) ;
   SetFileNameAndLineNo(string(FileName), Operand1) ;
   BuildEndFunctionCode(Mod2Gcc(CurrentProcedure)) ;

   CurrentProcedure := NulSym ;
   PreviousScope    := NulSym
END CodeKillLocalVar ;


(*
   CodeReturn - does nothing, as the return is done by KillLocalVar.
*)

PROCEDURE CodeReturn ;
BEGIN
END CodeReturn ;


(*
   CodeReturnValue - places the operand into the return value space
                     allocated by the function call.
*)

PROCEDURE CodeReturnValue ;
VAR
   Operator   : QuadOperator ;
   Result,
   Operand2,
   Procedure  : CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, Operator, Result, Operand2, Procedure) ;
   DeclareConstant(CurrentQuadToken, Result) ;  (* checks to see whether it is a constant and declares it *)
   BuildReturnValueCode(Mod2Gcc(Procedure), Mod2Gcc(Result))
END CodeReturnValue ;


(*
   CodeCall - determines whether the procedure call is a direct call
              or an indirect procedure call.
*)

PROCEDURE CodeCall ;
VAR
   Operator: QuadOperator ;
   Operand1,
   Operand2,
   Operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, Operator, Operand1, Operand2, Operand3) ;
   (*
      Operator : CallOp
      Operand3 : Procedure
   *)
   IF IsProcedure(Operand3)
   THEN
      CodeDirectCall
   ELSIF IsProcType(GetType(Operand3))
   THEN
      CodeIndirectCall
   ELSE
      InternalError('Expecting Procedure or ProcType', __FILE__, __LINE__)
   END
END CodeCall ;


(*
   CanUseBuiltin - returns TRUE if the procedure, Sym, can be
                   inlined via a builtin function.
*)

PROCEDURE CanUseBuiltin (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( BuiltinExists(KeyToCharStar(GetProcedureBuiltin(Sym))) OR
           BuiltinExists(KeyToCharStar(GetSymName(Sym))) )
END CanUseBuiltin ;


(*
   UseBuiltin - returns a Tree containing the builtin function
                and parameters. It should only be called if
                CanUseBuiltin returns TRUE.
*)

PROCEDURE UseBuiltin (Sym: CARDINAL) : Tree ;
BEGIN
   IF BuiltinExists(KeyToCharStar(GetProcedureBuiltin(Sym)))
   THEN
      RETURN( BuildBuiltinTree(KeyToCharStar(GetProcedureBuiltin(Sym))) )
   ELSE
      RETURN( BuildBuiltinTree(KeyToCharStar(GetSymName(Sym))) )
   END
END UseBuiltin ;


(*
   CodeDirectCall - saves all volitiles and jumps to a subroutine.
*)

PROCEDURE CodeDirectCall ;
VAR
   Operator: QuadOperator ;
   Operand1,
   Operand2,
   Operand3: CARDINAL ;
   tree    : Tree ;
BEGIN
   GetQuad(CurrentQuad, Operator, Operand1, Operand2, Operand3) ;

   IF IsProcedureBuiltin(Operand3) AND CanUseBuiltin(Operand3)
   THEN
      tree := UseBuiltin(Operand3)
   ELSE
      IF GetType(Operand3)=NulSym
      THEN
         tree := BuildProcedureCall(Mod2Gcc(Operand3), NIL)
      ELSE
         tree := BuildProcedureCall(Mod2Gcc(Operand3), Mod2Gcc(GetType(Operand3)))
      END
   END
END CodeDirectCall ;


(*
   CodeIndirectCall - saves all volitiles and jumps to a subroutine.
*)

PROCEDURE CodeIndirectCall ;
VAR
   operator  : QuadOperator ;
   operand1,
   operand2,
   operand3  : CARDINAL ;
   tree,
   ReturnType: Tree ;
   proc      : CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;

   proc := GetType(operand3) ;
   IF GetType(proc)=NulSym
   THEN
      ReturnType := Tree(NIL)
   ELSE
      ReturnType := Tree(Mod2Gcc(GetType(proc)))
   END ;

   (* now we dereference the lvalue if necessary *)

   IF GetMode(operand3)=LeftValue
   THEN
      tree := BuildIndirectProcedureCall(BuildIndirect(Mod2Gcc(operand3), Mod2Gcc(proc)),
                                         ReturnType)
   ELSE
      tree := BuildIndirectProcedureCall(Mod2Gcc(operand3), ReturnType)
   END
END CodeIndirectCall ;


(*
   CodeParam - builds a parameter list.

               NOTE that we almost can treat VAR and NON VAR parameters the same, expect for
                    some types:

                    procedure parameters
                    unbounded parameters

                    these require special attention and thus it is easier to test individually
                    for VAR and NON VAR parameters.

               NOTE that we CAN ignore ModeOfAddr though
*)

PROCEDURE CodeParam ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   IF operand1>0
   THEN
      IF (operand1<=NoOfParam(operand2)) AND
         IsVarParam(operand2, operand1) AND IsConst(operand3)
      THEN
         ErrorStringAt(Sprintf1(Mark(InitString('cannot pass a constant (%a) as a VAR parameter')), GetSymName(operand3)), CurrentQuadToken)
      ELSE
         DeclareConstant(CurrentQuadToken, operand3) ;
         (*
            ignore LeftAddr and RightAddr, but must be careful about size of Operand3.
            SIZE(operand3) will normally be TSIZE(ADDRESS) but NOT when it is unbounded
         *)
         BuildParam(Mod2Gcc(operand3))
      END
   END
END CodeParam ;


(*
   CodeFunctValue - retrieves the function return value and assigns it
                    into a variable.
*)

PROCEDURE CodeFunctValue ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;

   (*
      operator : FunctValueOp
      operand1 : The Returned Variable
      operand2 : The Function Returning this Variable
   *)
   BuildFunctValue(Mod2Gcc(operand1))
END CodeFunctValue ;


(*
   Addr Operator  - contains the address of a variable.

   Yields the address of a variable - need to add the frame pointer if
   a variable is local to a procedure.

   Sym1<X>   Addr   Sym2<X>     meaning     Mem[Sym1<I>] := Sym2<I>
*)

PROCEDURE CodeAddr ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   IF IsConst(operand3) AND (NOT IsConstString(operand3))
   THEN
      ErrorStringAt(Sprintf1(Mark(InitString('error in expression, trying to find the address of a constant (%a)')), GetSymName(operand3)), CurrentQuadToken)
   ELSE
      DeclareConstant(CurrentQuadToken, operand3) ;  (* we might be asked to find the address of a constant string *)
      t := BuildAssignment(Mod2Gcc(operand1),
                           BuildAddr(Mod2Gcc(operand3), FALSE))
   END
END CodeAddr ;


PROCEDURE stop ; BEGIN END stop ;

PROCEDURE CheckStop (q: CARDINAL) ;
BEGIN
   IF q=349
   THEN
      stop
   END
END CheckStop ;

(*
------------------------------------------------------------------------------
   := Operator
------------------------------------------------------------------------------
   Sym1<I> := Sym3<I>           := produces a constant
*)

PROCEDURE FoldBecomes (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   DeclareConstant(tokenno, operand3) ;  (* checks to see whether it is a constant literal and declares it *)
   IF IsConst(operand1) AND IsConst(operand3)
   THEN
      (* constant folding taking place, but have we resolved operand3 yet? *)
      IF GccKnowsAbout(operand3)
      THEN
         (* great, now we can tell gcc about the relationship between, op1 and op3 *)
         IF GccKnowsAbout(operand1)
         THEN
            ErrorStringAt(Sprintf1(Mark(InitString('constant, %a, should not be reassigned')), GetSymName(operand1)), tokenno)
         ELSE
            IF IsConstString(operand3)
            THEN
               PutConstString(operand1, GetString(operand3)) ;
            ELSIF GetType(operand1)=NulSym
            THEN
               Assert(GetType(operand3)#NulSym) ;
               PutConst(operand1, GetType(operand3))
            END ;
            IF GetType(operand3)=NulSym
            THEN
               AddModGcc(operand1, Mod2Gcc(operand3))
            ELSE
               IF IsValueSolved(operand3)
               THEN
                  PushValue(operand3) ;
                  IF IsValueTypeReal()
                  THEN
                     AddModGcc(operand1, PopRealTree())
                  ELSE
                     AddModGcc(operand1, PopIntegerTree())
                  END
               ELSE
                  AddModGcc(operand1,
                            DeclareKnownConstant(Mod2Gcc(GetType(operand3)),
                                                 Mod2Gcc(operand3)))
               END
            END ;
            RemoveItemFromList(l, operand1) ;
            SubQuad(AbsoluteHead, quad) ;
            t := RememberConstant(Mod2Gcc(operand1))
         END
      ELSE
         (* not to worry, we must wait until op3 is known *)
      END
   END
END FoldBecomes ;


(*
------------------------------------------------------------------------------
   := Operator
------------------------------------------------------------------------------
   Sym1<I> := Sym3<I>           := produces a constant
   Sym1<O> := Sym3<O>           := has the effect Mem[Sym1<I>] := Mem[Sym3<I>]
*)

PROCEDURE CodeBecomes ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   DeclareConstant(CurrentQuadToken, operand3) ;  (* checks to see whether it is a constant and declares it *)
   IF IsConst(operand1) AND (NOT GccKnowsAbout(operand1))
   THEN
      AddModGcc(operand1,
                DeclareKnownConstant(Mod2Gcc(GetType(operand3)),
                                     Mod2Gcc(operand3)))
   ELSIF IsConstString(operand3) AND (GetType(operand1)#Char)
   THEN
      (* handle string assignments:
         VAR
            str: ARRAY [0..10] OF CHAR ;
            ch : CHAR ;

         str := 'abcde' but not ch := 'a'
      *)
      ExpandExpressionStatement(BuiltInMemCopy(BuildAddr(Mod2Gcc(operand1), FALSE),
                                               BuildAddr(Mod2Gcc(operand3), FALSE),
                                               FindSize(operand3)))
   ELSE
      t := BuildAssignment(Mod2Gcc(operand1), Mod2Gcc(operand3))
   END
END CodeBecomes ;


(*
   FoldBinary - check whether we can fold the binop operation.
*)

PROCEDURE FoldBinary (tokenno: CARDINAL; quad: CARDINAL; l: List; binop: BuildBinProcedure) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(tokenno, operand3) ;
   DeclareConstant(tokenno, operand2) ;
   IF IsConst(operand2) AND IsConst(operand3)
   THEN
      IF GccKnowsAbout(operand2) AND GccKnowsAbout(operand3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(operand1)
         THEN
            Assert(MixTypes(FindType(operand3), FindType(operand2), tokenno)#NulSym) ;
            PutConst(operand1, MixTypes(FindType(operand3), FindType(operand2), tokenno)) ;
            AddModGcc(operand1,
                      DeclareKnownConstant(Mod2Gcc(GetType(operand3)),
                                           binop(Mod2Gcc(operand2),
                                                 Mod2Gcc(operand3),
                                                 TRUE))) ;
            RemoveItemFromList(l, operand1) ;
            SubQuad(AbsoluteHead, quad)
         ELSE
            (* we can still fold the expression, but not the assignment, however, we will
               not do this here but in CodeBinary
             *)
         END
      END
   END
END FoldBinary ;


(*
   CodeBinary - encode a binary arithmetic operation.
*)

PROCEDURE CodeBinary (binop: BuildBinProcedure) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, operand3) ;
   DeclareConstant(CurrentQuadToken, operand2) ;
   IF IsConst(operand1)
   THEN
      (* still have a constant which was not resolved, pass it to gcc *)
      Assert(MixTypes(FindType(operand3), FindType(operand2), CurrentQuadToken)#NulSym) ;

      PutConst(operand1, MixTypes(FindType(operand3), FindType(operand2), CurrentQuadToken)) ;
      AddModGcc(operand1,
                DeclareKnownConstant(Mod2Gcc(GetType(operand3)),
                                     binop(Mod2Gcc(operand2),
                                           Mod2Gcc(operand3),
                                           TRUE))) ;
   ELSE
      t := BuildAssignment(Mod2Gcc(operand1),
                           binop(Mod2Gcc(operand2), Mod2Gcc(operand3), TRUE))
   END
END CodeBinary ;


(*
   CodeBinarySet - encode a binary set arithmetic operation.
                   Set operands may be longer than a word.
*)

PROCEDURE CodeBinarySet (binop: BuildBinProcedure; doOp: DoProcedure) ;
VAR
   t       : CARDINAL ;
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, operand3) ;
   DeclareConstant(CurrentQuadToken, operand2) ;
   IF IsConst(operand1)
   THEN
      IF IsValueSolved(operand2) AND IsValueSolved(operand3)
      THEN
         Assert(MixTypes(FindType(operand3), FindType(operand2), CurrentQuadToken)#NulSym) ;
         PutConst(operand1, FindType(operand3)) ;
         PushValue(operand2) ;
         PushValue(operand3) ;
         doOp(CurrentQuadToken) ;
         PopValue(operand1) ;
         PushValue(operand1) ;
         AddModGcc(operand1,
                   DeclareKnownConstant(Mod2Gcc(GetType(operand3)),
                                        PopSetTree(CurrentQuadToken))) ;
         PutConstSet(operand1)
      ELSE
         ErrorStringAt(InitString('constant expression cannot be evaluated'),
                       CurrentQuadToken)
      END
   ELSE
      BuildBinaryForeachWordDo(Mod2Gcc(GetType(operand1)), Mod2Gcc(operand1), Mod2Gcc(operand2), Mod2Gcc(operand3), binop,
                               GetMode(operand1)=LeftValue, GetMode(operand2)=LeftValue, GetMode(operand3)=LeftValue)
   END
END CodeBinarySet ;


(*
   FoldAdd - check addition for constant folding.
*)

PROCEDURE FoldAdd (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(tokenno, quad, l, BuildAdd)
END FoldAdd ;


(*
   CodeAdd - encode subtraction.
*)

PROCEDURE CodeAdd ;
BEGIN
   CodeBinary(BuildAdd)
END CodeAdd ;


(*
   FoldSub - check subtraction for constant folding.
*)

PROCEDURE FoldSub (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(tokenno, quad, l, BuildSub)
END FoldSub ;


(*
   CodeSub - encode subtraction.
*)

PROCEDURE CodeSub ;
BEGIN
   CodeBinary(BuildSub)
END CodeSub ;


(*
   FoldMult - check multiplication for constant folding.
*)

PROCEDURE FoldMult (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(tokenno, quad, l, BuildMult)
END FoldMult ;


(*
   CodeMult - encode multiplication.
*)

PROCEDURE CodeMult ;
BEGIN
   CodeBinary(BuildMult)
END CodeMult ;


(*
   FoldDiv - check division for constant folding.
*)

PROCEDURE FoldDiv (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(tokenno, quad, l, BuildDiv)
END FoldDiv ;


(*
   CodeDiv - encode multiplication.
*)

PROCEDURE CodeDiv ;
BEGIN
   CodeBinary(BuildDiv)
END CodeDiv ;


(*
   FoldMod - check modulus for constant folding.
*)

PROCEDURE FoldMod (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(tokenno, quad, l, BuildMod)
END FoldMod ;


(*
   CodeMod - encode modulus.
*)

PROCEDURE CodeMod ;
BEGIN
   CodeBinary(BuildMod)
END CodeMod ;


(*
   FoldBuiltinConst - 
*)

PROCEDURE FoldBuiltinConst (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
VAR
   t            : Tree ;
   operator     : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, op1, op2, op3) ;
   t := GetBuiltinConst(KeyToCharStar(Name(op3))) ;
   IF t=NIL
   THEN
      ErrorStringAt(Sprintf1(Mark(InitString('unknown built in constant (%s)')),
                             Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(op3)))))),
                    tokenno)
   ELSE
      AddModGcc(op1, t) ;
      RemoveItemFromList(l, op1) ;
      SubQuad(AbsoluteHead, quad)
   END
END FoldBuiltinConst ;


(*
   FoldBinarySet - attempts to fold set arithmetic it removes the quad if successful.
*)

PROCEDURE FoldBinarySet (tokenno: CARDINAL; quad: CARDINAL; l: List; op: DoProcedure) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   (* firstly try and ensure that constants are declared *)
   DeclareConstant(tokenno, operand2) ;
   DeclareConstant(tokenno, operand3) ;
   IF IsConst(operand2) AND IsConstSet(operand2) AND
      IsConst(operand3) AND IsConstSet(operand3) AND
      IsConst(operand1)
   THEN
      IF IsValueSolved(operand2) AND IsValueSolved(operand3)
      THEN
         Assert(MixTypes(FindType(operand3), FindType(operand2), tokenno)#NulSym) ;
         PutConst(operand1, MixTypes(FindType(operand3), FindType(operand2), tokenno)) ;
         PushValue(operand2) ;
         PushValue(operand3) ;
         op(tokenno) ;
         PopValue(operand1) ;
         PushValue(operand1) ;
         AddModGcc(operand1,
                   DeclareKnownConstant(Mod2Gcc(GetType(operand3)),
                                        PopSetTree(tokenno))) ;
         PutConstSet(operand1) ;
         RemoveItemFromList(l, operand1) ;
         SubQuad(AbsoluteHead, quad)
      END
   END
END FoldBinarySet ;


(*
   FoldSetOr - check whether we can fold a set arithmetic or.
*)

PROCEDURE FoldSetOr (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
BEGIN
   FoldBinarySet(tokenno, quad, l, SetOr)
END FoldSetOr ;


(*
   CodeSetOr - encode set arithmetic or.
*)

PROCEDURE CodeSetOr ;
BEGIN
   CodeBinarySet(BuildLogicalOr, SetOr)
END CodeSetOr ;


(*
   FoldSetAnd - check whether we can fold a logical and.
*)

PROCEDURE FoldSetAnd (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
BEGIN
   FoldBinarySet(tokenno, quad, l, SetAnd)
END FoldSetAnd ;


(*
   CodeSetAnd - encode set arithmetic and.
*)

PROCEDURE CodeSetAnd ;
BEGIN
   CodeBinarySet(BuildLogicalAnd, SetAnd)
END CodeSetAnd ;


(*
   FoldSymmetricDifference - check whether we can fold a logical difference.
*)

PROCEDURE FoldSymmetricDifference (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
BEGIN
   FoldBinarySet(tokenno, quad, l, SetSymmetricDifference)
END FoldSymmetricDifference ;


(*
   CodeSetSymmetricDifference - code set difference.
*)

PROCEDURE CodeSetSymmetricDifference ;
BEGIN
   CodeBinarySet(BuildSymmetricDifference, SetSymmetricDifference)
END CodeSetSymmetricDifference ;


(*
   CodeUnarySet - encode a unary set arithmetic operation.
                  Set operands may be longer than a word.
*)

PROCEDURE CodeUnarySet (unop: BuildUnaryProcedure; doOp: DoUnaryProcedure) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, operand3) ;
   IF IsConst(operand1)
   THEN
      IF IsValueSolved(operand3)
      THEN
         Assert(FindType(operand3)#NulSym) ;
         PutConst(operand1, FindType(operand3)) ;
         PushValue(operand3) ;
         doOp(CurrentQuadToken) ;
         PopValue(operand1) ;
         PushValue(operand1) ;
         AddModGcc(operand1,
                   DeclareKnownConstant(Mod2Gcc(GetType(operand3)),
                                        PopSetTree(CurrentQuadToken))) ;
         PutConstSet(operand1)
      ELSE
         ErrorStringAt(InitString('constant expression cannot be evaluated'), CurrentQuadToken)
      END
   ELSE
      BuildUnaryForeachWordDo(Mod2Gcc(GetType(operand1)), Mod2Gcc(operand1), Mod2Gcc(operand3), unop,
                              GetMode(operand1)=LeftValue, GetMode(operand3)=LeftValue)
   END
END CodeUnarySet ;


(*
   FoldIncl - check whether we can fold the InclOp.
              op1 := op1 + (1 << op3)
*)

PROCEDURE FoldIncl (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
VAR
   operator     : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, op1, op2, op3) ;
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(tokenno, op3) ;
   IF IsConst(op1) AND IsConst(op3)
   THEN
      IF GccKnowsAbout(op3) AND IsValueSolved(op1)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         PushValue(op1) ;
         AddBit(op3) ;
         AddModGcc(op1, PopSetTree(tokenno)) ;
         RemoveItemFromList(l, op1) ;
         SubQuad(AbsoluteHead, quad)
      END
   END
END FoldIncl ;


(*
   FoldIfIn - check whether we can fold the IfInOp
              if op1 in op2 then goto op3
*)

PROCEDURE FoldIfIn (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
VAR
   operator     : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, op1, op2, op3) ;
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(tokenno, op1) ;
   DeclareConstant(tokenno, op2) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      IF IsValueSolved(op1) AND IsValueSolved(op2)
      THEN
         (* fine, we can take advantage of this and evaluate the condition *)
         PushValue(op2) ;
         IF SetIn(tokenno, op1)
         THEN
            PutQuad(quad, GotoOp, NulSym, NulSym, op3)
         ELSE
            SubQuad(AbsoluteHead, quad)
         END
      END
   END
END FoldIfIn ;


(*
   FoldIfNotIn - check whether we can fold the IfNotInOp
                 if not (op1 in op2) then goto op3
*)

PROCEDURE FoldIfNotIn (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
VAR
   operator     : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, op1, op2, op3) ;
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(tokenno, op1) ;
   DeclareConstant(tokenno, op2) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      IF IsValueSolved(op1) AND IsValueSolved(op2)
      THEN
         (* fine, we can take advantage of this and evaluate the condition *)
         PushValue(op2) ;
         IF NOT SetIn(tokenno, op1)
         THEN
            PutQuad(quad, GotoOp, NulSym, NulSym, op3)
         ELSE
            SubQuad(AbsoluteHead, quad)
         END
      END
   END
END FoldIfNotIn ;


(*
   GetSetLimits - assigns low and high to the limits of the declared, set.
*)

PROCEDURE GetSetLimits (set: CARDINAL; VAR low, high: CARDINAL) ;
VAR
   type: CARDINAL ;
BEGIN
   type := GetType(set) ;
   IF IsSubrange(type)
   THEN
      GetSubrange(type, high, low) ;
   ELSE
      low := GetTypeMin(type) ;
      high := GetTypeMax(type)
   END
END GetSetLimits ;


(*
   GetFieldNo - returns the field number in the, set, which contains, element.
*)

PROCEDURE GetFieldNo (tokenno: CARDINAL; element: CARDINAL; set: CARDINAL; VAR offset: Tree) : INTEGER ;
VAR
   type, low, high, bpw: CARDINAL ;
   i                   : INTEGER ;
BEGIN
   bpw := GetBitsPerWord() ;
   GetSetLimits(set, low, high) ;

   (* check element is legal *)

   PushValue(element) ;
   PushValue(low) ;
   IF Less(tokenno)
   THEN
      (* out of range *)
      RETURN( -1 )
   ELSE
      PushValue(element) ;
      PushValue(high) ;
      IF Gre(tokenno)
      THEN
         RETURN( -1 )
      END
   END ;

   (* all legal *)

   PushValue(low) ;
   offset := PopIntegerTree() ;
   i := 0 ;
   PushValue(element) ;
   PushValue(low) ;
   PushCard(bpw) ;
   Addn ;
   WHILE GreEqu(tokenno) DO
      INC(i) ;   (* move onto next field *)
      PushValue(element) ;
      PushCard((i+1)*bpw) ;
      PushIntegerTree(offset) ;
      PushCard(bpw) ;
      Addn ;
      offset := PopIntegerTree() ;
   END ;
   RETURN( i )
END GetFieldNo ;


(*
   CodeIncl - encode an InclOp:
              op1 := op1 + (1 << op3)
*)

PROCEDURE CodeIncl ;
VAR
   operator: QuadOperator ;
   low,
   high,
   operand1,
   operand2,
   operand3: CARDINAL ;
   offset  : Tree ;
   fieldno : INTEGER ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, operand3) ;
   IF IsConst(operand1)
   THEN
      IF IsConst(operand3)
      THEN
         InternalError('this quadruple should have been removed by FoldIncl', __FILE__, __LINE__)
      ELSE
         InternalError('should not get to here (why are we generating <incl const, var> ?)', __FILE__, __LINE__)
      END
   ELSE
      IF IsConst(operand3)
      THEN
         fieldno := GetFieldNo(CurrentQuadToken, operand3, GetType(operand1), offset) ;
         IF fieldno>=0
         THEN
            PushValue(operand3) ;
            PushIntegerTree(offset) ;
            Sub ;
            BuildIncludeVarConst(Mod2Gcc(GetType(operand1)),
                                 Mod2Gcc(operand1), PopIntegerTree(),
                                 GetMode(operand1)=LeftValue, fieldno)
         ELSE
            ErrorStringAt(Sprintf1(Mark(InitString('bit exceeded the range of set (%s)')),
                                   Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(operand1)))))),
                          CurrentQuadToken)
         END
      ELSE
         GetSetLimits(GetType(operand1), low, high) ;
         BuildIncludeVarVar(Mod2Gcc(GetType(operand1)),
                            Mod2Gcc(operand1), Mod2Gcc(operand3), GetMode(operand1)=LeftValue, Mod2Gcc(low))
      END
   END
END CodeIncl ;


(*
   FoldExcl - check whether we can fold the InclOp.
              op1 := op1 - (1 << op3)
*)

PROCEDURE FoldExcl (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
VAR
   operator     : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, op1, op2, op3) ;
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(tokenno, op3) ;
   IF IsConst(op1) AND IsConst(op3)
   THEN
      IF GccKnowsAbout(op3) AND IsValueSolved(op1)
      THEN
         PushValue(op1) ;
         SubBit(tokenno, op3) ;
         AddModGcc(op1, PopSetTree(tokenno)) ;
         RemoveItemFromList(l, op1) ;
         SubQuad(AbsoluteHead, quad)
      END
   END
END FoldExcl ;


(*
   CodeExcl - encode an ExclOp:
                op1 := op1 - (1 << op3)
*)

PROCEDURE CodeExcl ;
VAR
   operator: QuadOperator ;
   low,
   high,
   operand1,
   operand2,
   operand3: CARDINAL ;
   offset  : Tree ;
   fieldno : INTEGER ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, operand3) ;
   IF IsConst(operand1)
   THEN
      InternalError('should not get to here (if we do we should consider calling FoldInclOp)', __FILE__, __LINE__)
   ELSE
      IF IsConst(operand3)
      THEN
         fieldno := GetFieldNo(CurrentQuadToken, operand3, GetType(operand1), offset) ;
         IF fieldno>=0
         THEN
            PushValue(operand3) ;
            PushIntegerTree(offset) ;
            Sub ;
            BuildExcludeVarConst(Mod2Gcc(GetType(operand1)),
                                 Mod2Gcc(operand1), PopIntegerTree(),
                                 GetMode(operand1)=LeftValue, fieldno)
         ELSE
            ErrorStringAt(Sprintf1(Mark(InitString('bit exceeded the range of set (%s)')),
                                   Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(operand1)))))),
                          CurrentQuadToken)
         END
      ELSE
         GetSetLimits(GetType(operand1), low, high) ;
         BuildExcludeVarVar(Mod2Gcc(GetType(operand1)),
                            Mod2Gcc(operand1), Mod2Gcc(operand3), GetMode(operand1)=LeftValue, Mod2Gcc(low))
      END
   END
END CodeExcl ;


(*
   FoldUnary - check whether we can fold the unop operation.
*)

PROCEDURE FoldUnary (tokenno: CARDINAL; quad: CARDINAL; l: List; unop: BuildUnaryProcedure; CoerceConst: Tree) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(tokenno, operand3) ;
   IF IsConst(operand3)
   THEN
      IF GccKnowsAbout(operand3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(operand1)
         THEN
            IF CoerceConst=Tree(NIL)
            THEN
               CoerceConst := Tree(Mod2Gcc(GetType(operand3))) ;
               IF CoerceConst=Tree(NIL)
               THEN
                  CoerceConst := GetIntegerType()
               END
            END ;
            PutConst(operand1, FindType(operand3)) ;
            AddModGcc(operand1,
                      DeclareKnownConstant(CoerceConst,
                                           unop(Mod2Gcc(operand3), FALSE))) ;
            RemoveItemFromList(l, operand1) ;
            SubQuad(AbsoluteHead, quad)
         ELSE
            (* we can still fold the expression, but not the assignment, however, we will
               not do this here but in CodeUnary
             *)
         END
      END
   END
END FoldUnary ;


(*
   FoldUnarySet - check whether we can fold the doOp operation.
*)

PROCEDURE FoldUnarySet (tokenno: CARDINAL; quad: CARDINAL; l: List; doOp: DoUnaryProcedure) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   (* firstly try and ensure that constants are declared *)
   DeclareConstant(tokenno, operand3) ;
   IF IsConst(operand3) AND IsConstSet(operand3) AND
      IsConst(operand1)
   THEN
      IF IsValueSolved(operand3)
      THEN
         PutConst(operand1, FindType(operand3)) ;
         PushValue(operand3) ;
         doOp(tokenno) ;
         PopValue(operand1) ;
         PushValue(operand1) ;
         AddModGcc(operand1,
                   DeclareKnownConstant(Mod2Gcc(GetType(operand3)),
                                        PopSetTree(QuadToTokenNo(quad)))) ;
         PutConstSet(operand1) ;
         RemoveItemFromList(l, operand1) ;
         SubQuad(AbsoluteHead, quad)
      END
   END
END FoldUnarySet ;


(*
   CodeUnary - encode a unary arithmetic operation.
*)

PROCEDURE CodeUnary (unop: BuildUnaryProcedure; CoerceConst: Tree) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, operand3) ;
   IF IsConst(operand1)
   THEN
      IF CoerceConst=Tree(NIL)
      THEN
         CoerceConst := Tree(Mod2Gcc(GetType(operand3)))
      END ;
      (* still have a constant which was not resolved, pass it to gcc *)
      PutConst(operand1, FindType(operand3)) ;
      AddModGcc(operand1,
                DeclareKnownConstant(CoerceConst,
                                     unop(Mod2Gcc(operand3), FALSE)))
   ELSE
      t := BuildAssignment(Mod2Gcc(operand1),
                           unop(Mod2Gcc(operand3), FALSE))
   END
END CodeUnary ;


(*
   FoldNegate - check unary negate for constant folding.
*)

PROCEDURE FoldNegate (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   IF IsConstSet(operand3)
   THEN
      FoldUnarySet(tokenno, quad, l, SetNegate)
   ELSE
      FoldUnary(tokenno, quad, l, BuildNegate, NIL)
   END
END FoldNegate ;


(*
   CodeNegate - encode unary negate.
*)

PROCEDURE CodeNegate ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   IF IsConstSet(operand3) OR IsSet(GetType(operand3))
   THEN
      CodeUnarySet(BuildSetNegate, SetNegate)
   ELSE
      CodeUnary(BuildNegate, NIL)
   END
END CodeNegate ;


(*
   FoldSize - check unary SIZE for constant folding.
*)

PROCEDURE FoldSize (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   IF IsConst(operand1) AND CompletelyResolved(operand3)
   THEN
      t := BuildSize(Mod2Gcc(operand3), FALSE) ;
      PushIntegerTree(t) ;
      PopValue(operand1) ;
      PutConst(operand1, Cardinal) ;
      RemoveItemFromList(l, operand1) ;
      SubQuad(AbsoluteHead, quad) ;
      t := RememberConstant(t)
   END
END FoldSize ;


(*
   CodeSize - encode the inbuilt SIZE function.
*)

PROCEDURE CodeSize ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   PushIntegerTree(BuildSize(Mod2Gcc(operand3), FALSE)) ;
   IF IsConst(operand1)
   THEN
      PopValue(operand1) ;
      PutConst(operand1, Cardinal) ;
      PushValue(operand1) ;
      AddModGcc(operand1,
                DeclareKnownConstant(GetIntegerType(),
                                     PopIntegerTree()))
   ELSE
      t := BuildAssignment(Mod2Gcc(operand1), PopIntegerTree())
   END
END CodeSize ;


(*
   DetermineFieldOf - is sadly complicated by the way varient records are encoded in the front end
                      symbol table. The symbol, sym, is a RecordField which is either in the structure:

                      RecordSym
                         RecordField: Type ;
                         RecordField: Type ;
                      End

                      or alternatively:

                      RecordSym
                         Varient:  VarientField: RecordField: Type ;
                                                 RecordField: Type ;
                                   VarientField: RecordField: Type ;
                                                 RecordField: Type ;
                         Varient:  VarientField: RecordField: Type ;
                                                 RecordField: Type ;
                                   VarientField: RecordField: Type ;
                                                 RecordField: Type ;
                      End

                      Thus when we are asked to calculate Offset RecordField
                      we need to know which of the two alternatives we are dealing with.
                      The GCC BuildOffset calculates the offset between RecordField its
                      Varient parent. We need to add the offset between varient parent and
                      the RecordSym. This code is bridging the difference in symbol table
                      construction between the front end and GCC.

                      We return the Varient symbol if sym was declared in the second method.
*)

PROCEDURE DetermineFieldOf (sym: CARDINAL) : CARDINAL ;
VAR
   i, j, k,
   Field,
   VarientField,
   Varient,
   Record      : CARDINAL ;
BEGIN
   Assert(IsRecordField(sym)) ;
   Record := Father(sym) ;
   Assert(IsRecord(Record)) ;
   i := 1 ;
   LOOP
      Varient := GetNth(Record, i) ;
      IF (sym=Varient) OR (sym=NulSym)
      THEN
         (* short cut a simple normal record, or we have finished looking at all the fields *)
         RETURN( NulSym)
      ELSIF IsVarient(Varient)
      THEN
         (* now check each VarientField *)
         j := 1 ;
         REPEAT
            VarientField := GetNth(Varient, j) ;
            IF VarientField#NulSym
            THEN
               Assert(IsFieldVarient(VarientField)) ;
               k := 1 ;
               REPEAT
                  Field := GetNth(VarientField, k) ;
                  IF Field=sym
                  THEN
                     (* found it.. *)
                     RETURN( Varient )
                  ELSE
                     INC(k)
                  END
               UNTIL Field=NulSym
            END ;
            INC(j)
         UNTIL VarientField=NulSym
      ELSIF IsRecordField(Varient)
      THEN
         (* ok, normal field, but not ours, skip to the next *)
      ELSE
         InternalError('unexpected field in record', __FILE__, __LINE__)
      END ;
      INC(i)
   END
END DetermineFieldOf ;


(*
   FoldOffset - check whether we can fold an OffsetOp quadruple.
                Very similar to FoldUnary, except that we need to hard code
                a few parameters to the gcc backend.
*)

PROCEDURE FoldOffset (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
VAR
   operator: QuadOperator ;
   field,
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(tokenno, operand3) ;
   IF IsRecordField(operand3) OR IsFieldVarient(operand3)
   THEN
      IF GccKnowsAbout(operand3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(operand1)
         THEN
            field := DetermineFieldOf(operand3) ;
            IF field=NulSym
            THEN
               t := BuildOffset(Mod2Gcc(operand3), FALSE)
            ELSE
               (*
                  we must also add the position of operand3 father as BuildOffset
                  gives us the relative position of operand3 to its father. We need
                  to add fathers position to this in order to get correct overall offset.
                  [The operand3 father is akin to an invisible record]
               *)
               t := BuildAdd(BuildOffset(Mod2Gcc(operand3), FALSE),
                             BuildOffset(Mod2Gcc(field), FALSE),
                             FALSE)
            END ;
            IF NOT IsValueSolved(operand1)
            THEN
               PushIntegerTree(t) ;
               PopValue(operand1)
            END ;
            PutConst(operand1, Address) ;
            AddModGcc(operand1,
                      DeclareKnownConstant(GetIntegerType(),
                                           t)) ;
            RemoveItemFromList(l, operand1) ;
            SubQuad(AbsoluteHead, quad)
         ELSE
            (* we can still fold the expression, but not the assignment, however, we will
               not do this here but in CodeOffset
             *)
         END
      END
   END
END FoldOffset ;


(*
   CodeOffset - encode an OffsetOp quadruple. Very similar to CodeUnary, except that
                we need to hard code a few parameters to the gcc backend.
                operand1 is set to contain the offset (in bytes) of field operand3
                from its parent record.
*)

PROCEDURE CodeOffset ;
VAR
   operator: QuadOperator ;
   field,
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, operand3) ;
   IF IsRecordField(operand3) OR IsFieldVarient(operand3)
   THEN
      IF GccKnowsAbout(operand3)
      THEN
         field := DetermineFieldOf(operand3) ;
         IF field=NulSym
         THEN
            t := BuildOffset(Mod2Gcc(operand3), FALSE)
         ELSE
            (*
               we must also add the position of operand3 father as BuildOffset
               gives us the relative position of operand3 to its father. We need
               to add fathers position to this in order to get correct overall offset.
               [The operand3 father is akin to an invisible record]
             *)
            t := BuildAdd(BuildOffset(Mod2Gcc(operand3), FALSE),
                          BuildOffset(Mod2Gcc(field), FALSE),
                          FALSE)
         END ;
         IF IsConst(operand1)
         THEN
            (* fine, we can take advantage of this and fold constants *)
            IF NOT IsValueSolved(operand1)
            THEN
               PushIntegerTree(t) ;
               PopValue(operand1)
            END ;
            PutConst(operand1, Address) ;
            AddModGcc(operand1,
                      DeclareKnownConstant(GetIntegerType(),
                                           t))
         ELSE
            (* ok, use assignment *)
            t := BuildAssignment(Mod2Gcc(operand1), t)
         END
      ELSE
         InternalError('symbol type should have been declared by now..', __FILE__, __LINE__)
      END
   ELSE
      InternalError('not expecting this type of symbol', __FILE__, __LINE__)
   END
END CodeOffset ;


(*
   ResolveHigh - given an Modula-2 operand, it resolves the HIGH(operand)
                 and returns a GCC constant symbol containing the value of
                 HIGH(operand).
*)

PROCEDURE ResolveHigh (quad: CARDINAL; operand: CARDINAL) : Tree ;
VAR
   Type    : CARDINAL ;
   High,
   Low     : CARDINAL ;
   Subscript,
   Subrange: CARDINAL ;
BEGIN
   Type := GetType(operand) ;
   IF NoOfElements(Type)#1
   THEN
      ErrorStringAt(InitString('HIGH operator only allowed on one dimensional arrays'),
                    QuadToTokenNo(quad))
   END ;
   Subscript := GetNth(Type, 1) ;
   Subrange := GetType(Subscript) ;
   GetSubrange(Subrange, High, Low) ;
   IF GccKnowsAbout(High)
   THEN
      RETURN( Tree(Mod2Gcc(High)) )
   ELSE
      RETURN( Tree(NIL) )
   END
END ResolveHigh ;


(*
   FoldHigh - if the array is not dynamic then we should be able to
              remove the HighOp quadruple and assign Operand1 with
              the known compile time HIGH(Operand3).
*)

PROCEDURE FoldHigh (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
VAR
   t       : Tree ;
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3,
   high,
   low,
   type    : CARDINAL ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(tokenno, operand3) ;
   t := ResolveHigh(quad, operand3) ;
   IF GccKnowsAbout(operand3)
   THEN
      (* fine, we can take advantage of this and fold constants *)
      IF IsConst(operand1) AND (t#Tree(NIL))
      THEN
         PutConst(operand1, Cardinal) ;
         AddModGcc(operand1,
                   DeclareKnownConstant(GetIntegerType(),
                                        t)) ;
         RemoveItemFromList(l, operand1) ;
         SubQuad(AbsoluteHead, quad)
      ELSE
         (* we can still fold the expression, but not the assignment, however, we will
            not do this here but in CodeHigh
         *)
      END
   END
END FoldHigh ;


(*
   CodeHigh - encode a unary arithmetic operation.
*)

PROCEDURE CodeHigh ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, operand3) ;
   IF IsConst(operand1)
   THEN
      (* still have a constant which was not resolved, pass it to gcc *)
      AddModGcc(operand1,
                DeclareKnownConstant(GetIntegerType(),
                                     ResolveHigh(CurrentQuad, operand3)))
   ELSE
      t := BuildAssignment(Mod2Gcc(operand1),
                           ResolveHigh(CurrentQuad, operand3))
   END
END CodeHigh ;


(*
   CodeUnbounded - codes the creation of an unbounded parameter variable.
                   places the address of op3 into *op1
*)

PROCEDURE CodeUnbounded ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   Addr,
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;

   DeclareConstant(CurrentQuadToken, operand3) ;
   IF IsConstString(operand3)
   THEN
      t := BuildAssignment(BuildIndirect(Mod2Gcc(operand1), GetIntegerType()),
                           BuildAddr(PromoteToString(CurrentQuadToken, operand3), FALSE))
   ELSIF IsUnbounded(GetType(operand3))
   THEN
      Addr := BuildIndirect(BuildAdd(BuildAddr(Mod2Gcc(operand3), FALSE),
                                     BuildOffset(Mod2Gcc(GetLocalSym(Unbounded, ArrayAddress)), FALSE),
                                     FALSE),
                            GetIntegerType()) ;
      t := BuildAssignment(BuildIndirect(Mod2Gcc(operand1), GetIntegerType()), Addr)
   ELSIF GetMode(operand3)=RightValue
   THEN
      t := BuildAssignment(BuildIndirect(Mod2Gcc(operand1), GetIntegerType()), BuildAddr(Mod2Gcc(operand3), FALSE))
   ELSE
      t := BuildAssignment(BuildIndirect(Mod2Gcc(operand1), GetIntegerType()), Mod2Gcc(operand3))
   END
END CodeUnbounded ;


(*
   AreSubrangesKnown - returns TRUE if all subranges values used within, array, are known.
*)

PROCEDURE AreSubrangesKnown (array: CARDINAL) : BOOLEAN ;
VAR
   i,
   subscript,
   subrange,
   low, high: CARDINAL ;
BEGIN
   IF GccKnowsAbout(array)
   THEN
      i := 1 ;
      subscript := GetNth(array, i) ;
      WHILE subscript#NulSym DO
         subrange := GetType(subscript) ;
         GetSubrange(subrange, high, low) ;
         IF GccKnowsAbout(low) AND GccKnowsAbout(high)
         THEN
            INC(i) ;
            subscript := GetNth(array, i)
         ELSE
            RETURN( FALSE )
         END
      END ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END AreSubrangesKnown ;


(*
   CalculateBase - works out the constant &array[declaration] - &array[0,0,0,0,etc]
*)

PROCEDURE CalculateBase (array: CARDINAL) : Tree ;
VAR
   offset  : Tree ;
   size    : Tree ;
   i       : CARDINAL ;
   High,
   Low     : CARDINAL ;
   Type,
   Subscript,
   Subrange: CARDINAL ;
BEGIN
   i := 1 ;
   offset    := GetIntegerZero() ;
   Subscript := GetNth(array, i) ;
   WHILE Subscript#NulSym DO
      size     := BuildSize(Mod2Gcc(Subscript), FALSE) ;  (* Size for element i *)
      Subrange := GetType(Subscript) ;
      GetSubrange(Subrange, High, Low) ;
      offset   := BuildSub(offset, BuildMult(size, Mod2Gcc(Low), FALSE), FALSE) ;
      INC(i) ;
      Subscript := GetNth(array, i)
   END ;
   RETURN( offset )
END CalculateBase ;


(*
   FoldBase - operand1 is a constant and BaseOp will calculate the offset
              of the virtual start of the array  ie a[0,0,0,0..,0]
              from the address of the array &a.

              operand2 is the type of the array
              operand3 is the array
*)

PROCEDURE FoldBase (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   IF IsConst(operand1)
   THEN
      IF (NOT GccKnowsAbout(operand1)) AND AreSubrangesKnown(operand2)
      THEN
         AddModGcc(operand1,
                   DeclareKnownConstant(GetIntegerType(),
                                        CalculateBase(operand2))) ;
         RemoveItemFromList(l, operand1) ;
         SubQuad(AbsoluteHead, quad)
      ELSE
         (* we can still fold the expression, but not the assignment, however, we will
            not do this here but in CodeBase
         *)
      END
   END
END FoldBase ;


(*
   CodeBase - operand1 is a constant and BaseOp will calculate the offset
              of the virtual start of the array  ie a[0,0,0,0..,0]
              from the address of the array &a.

              operand2 is the type of the array
              operand3 is the array
*)

PROCEDURE CodeBase ;
VAR
   t       : Tree ;
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   IF IsConst(operand1)
   THEN
      IF AreSubrangesKnown(operand2)
      THEN
         AddModGcc(operand1,
                   DeclareKnownConstant(GetIntegerType(),
                                        CalculateBase(operand2)))
      ELSE
         InternalError('subranges not yet resolved', __FILE__, __LINE__)
      END
   ELSE
      t := BuildAssignment(Mod2Gcc(operand1), CalculateBase(operand2))
   END
END CodeBase ;


(*
   FoldElementSizeForArray - attempts to calculate the Subscript
                             multiplier for the index Operand3.
*)

PROCEDURE FoldElementSizeForArray (quad: CARDINAL; l: List;
                                   operand1, type, operand3: CARDINAL) ;
VAR
   Subscript: CARDINAL ;
BEGIN
   IF IsConst(operand1) AND (NOT GccKnowsAbout(operand1))
   THEN
      IF NoOfElements(type)>=operand3
      THEN
         Subscript := GetNth(type, operand3) ;
         IF IsSizeSolved(Subscript)
         THEN
            PutConst(operand1, Integer) ;
            PushSize(Subscript) ;
            AddModGcc(operand1,
                      DeclareKnownConstant(GetIntegerType(),
                                           PopIntegerTree())) ;
            RemoveItemFromList(l, operand1) ;
            SubQuad(AbsoluteHead, quad)
         END
      END
   END
END FoldElementSizeForArray ;


(*
   FoldElementSizeForUnbounded - Unbounded arrays only have one index,
                                 therefore element size will be the
                                 TSIZE(Type) where Type is defined as:
                                 ARRAY OF Type.
*)

PROCEDURE FoldElementSizeForUnbounded (quad: CARDINAL; l: List;
                                       operand1, ArrayType, operand3: CARDINAL) ;
VAR
   Type: CARDINAL ;
BEGIN
   IF IsConst(operand1)
   THEN
      IF GccKnowsAbout(operand1)
      THEN
         InternalError('cannot assign a value twice to a constant', __FILE__, __LINE__)
      ELSE
         Assert(IsUnbounded(ArrayType)) ;
         Assert(operand3=1) ;
         Assert(NoOfElements(ArrayType)=operand3) ;
         Type := GetType(ArrayType) ;
         IF GccKnowsAbout(Type)
         THEN
            PutConst(operand1, Cardinal) ;
            AddModGcc(operand1,
                      DeclareKnownConstant(GetIntegerType(),
                                           FindSize(Type))) ;
            RemoveItemFromList(l, operand1) ;
            SubQuad(AbsoluteHead, quad)
         END
      END
   END
END FoldElementSizeForUnbounded ;


(*
   FoldElementSize - folds the element size for an ArraySym or UnboundedSym.
                     ElementSize returns a constant which defines the
                     multiplier to be multiplied by this element index.
*)

PROCEDURE FoldElementSize (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   IF IsUnbounded(operand2)
   THEN
      FoldElementSizeForUnbounded(quad, l, operand1, operand2, operand3)
   ELSIF IsArray(operand2)
   THEN
      FoldElementSizeForArray(quad, l, operand1, operand2, operand3)
   ELSE
      InternalError('Expecting UnboundedSym or ArraySym', __FILE__, __LINE__)
   END
END FoldElementSize ;


(*

*)

PROCEDURE CodeElementSize ;
BEGIN
   InternalError('strange - expected ElementSizeOp to be folded via constant evaluation', __FILE__, __LINE__)
END CodeElementSize ;


(*
   FoldConvert - attempts to fold operand3 to type operand2 placing the result into
                 operand1, providing that operand1 and operand3 are constants.
                 Convert will, if need be, alter the machine representation
                 of Operand3 to comply with TYPE Operand2.
*)

PROCEDURE FoldConvert (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(tokenno, quad, l, BuildConvert)
END FoldConvert ;


(*
   CodeConvert - Converts Operand3 to type Operand2 placing the result into
                 Operand1.
                 Convert will, if need be, alter the machine representation
                 of Operand3 to comply with TYPE Operand2.
*)

PROCEDURE CodeConvert ;
BEGIN
   CodeBinary(BuildConvert)
END CodeConvert ;


(*
   CodeCoerce - Coerce Operand3 to type Operand2 placing the result into
                Operand1.
                Coerce will NOT alter the machine representation
                of Operand3 to comply with TYPE Operand2.
                Therefore it INSISTS that under all circumstances that the
                type sizes of Operand1 and Operand3 are the same.
                CONVERT will perform machine manipulation to change variable
                types, coerce does no such thing.
*)

PROCEDURE CodeCoerce ;
VAR
   t       : Tree ;
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   DeclareConstant(CurrentQuadToken, operand3) ;  (* checks to see whether it is a constant literal and declares it *)
   IF IsProcedure(operand3)
   THEN
      IF AreConstantsEqual(FindSize(operand1), FindSize(Word))
      THEN
         IF IsConst(operand1)
         THEN
            AddModGcc(operand1,
                      DeclareKnownConstant(Mod2Gcc(GetType(operand1)),
                                           Mod2Gcc(operand3)))
         ELSE
            t := BuildAssignment(Mod2Gcc(operand1), Mod2Gcc(operand3))
         END
      ELSE
         ErrorStringAt(InitString('procedure address can only be stored in a word size operand'),
                       CurrentQuadToken)
      END
   ELSIF IsConst(operand3) OR AreConstantsEqual(FindSize(operand1), FindSize(operand3))
   THEN
      IF IsConst(operand1)
      THEN
         AddModGcc(operand1,
                   DeclareKnownConstant(Mod2Gcc(GetType(operand1)),
                                        Mod2Gcc(operand3)))
      ELSE
         t := BuildAssignment(Mod2Gcc(operand1), Mod2Gcc(operand3))
      END
   ELSE
      WarnStringAt(Sprintf2(Mark(InitString('TYPE Coersion can only be achieved with types of the same size, problem with %a and %a, attempting convert via VAL instead')),
                            GetSymName(operand2), GetSymName(operand3)), CurrentQuadToken) ;
      CodeConvert
   END
END CodeCoerce ;


(*
   FoldCoerce -
*)

PROCEDURE FoldCoerce (tokenno: CARDINAL; quad: CARDINAL; l: List) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   DeclareConstant(tokenno, operand3) ;  (* checks to see whether it is a constant literal and declares it *)
   IF GccKnowsAbout(operand2) AND GccKnowsAbout(operand3)
   THEN
      IF IsProcedure(operand3)
      THEN
         IF AreConstantsEqual(FindSize(operand1), FindSize(Address))
         THEN
            IF IsConst(operand1)
            THEN
               AddModGcc(operand1,
                         DeclareKnownConstant(Mod2Gcc(GetType(operand1)),
                                              Mod2Gcc(operand3))) ;
               RemoveItemFromList(l, operand1) ;
               SubQuad(AbsoluteHead, quad)
            END
         ELSE
            ErrorStringAt(InitString('procedure address can only be stored in a word size operand'), QuadToTokenNo(quad))
         END
      ELSIF IsConst(operand3)
      THEN
         IF IsConst(operand1)
         THEN
            AddModGcc(operand1,
                      DeclareKnownConstant(Mod2Gcc(GetType(operand1)),
                                           Mod2Gcc(operand3))) ;
            RemoveItemFromList(l, operand1) ;
            SubQuad(AbsoluteHead, quad)
         END
      END
   END
END FoldCoerce ;


(*
   CodeMath - translates the MathOp into a GCC tree structure.
              Op2 := Op1(Op3)

              where:

              Op1    function
              Op2    return variable
              Op3    parameter
*)

PROCEDURE CodeMath ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   DeclareConstant(CurrentQuadToken, operand3) ;  (* checks to see whether it is a constant literal and declares it *)
   IF operand1=Trunc
   THEN
      t := BuildAssignment(Mod2Gcc(operand2), BuildTrunc(Mod2Gcc(operand3)))
   ELSE
      InternalError('unknown math operator', __FILE__, __LINE__)
   END ;
END CodeMath ;


(*
   CreateLabelName - creates a namekey from quadruple, q.
*)

PROCEDURE CreateLabelName (q: CARDINAL) : String ;
BEGIN
   (* prefixed by . to ensure that no Modula-2 identifiers clash *)
   RETURN( Sprintf1(Mark(InitString('.L%d')), q) )
END CreateLabelName ;


(*
   CodeGoto - creates a jump to a labeled quadruple.
*)

PROCEDURE CodeGoto ;
VAR
   operator : QuadOperator ;
   operand1,
   operand2,
   operand3 : CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   BuildGoto(string(CreateLabelName(operand3)))
END CodeGoto ;


(*
   CheckReferenced - checks to see whether this quadruple requires a label.
*)

PROCEDURE CheckReferenced (q: CARDINAL; op: QuadOperator) ;
VAR
   t: Tree ;
BEGIN
   (* we do not create labels for procedure entries *)
   IF (op#NewLocalVarOp) AND IsReferenced(AbsoluteHead, q)
   THEN
      t := DeclareLabel(string(CreateLabelName(q)))
   END
END CheckReferenced ;


(*
   CodeIfSetLess - 
*)

PROCEDURE CodeIfSetLess (quad: CARDINAL; operand1, operand2, operand3: CARDINAL) ;
VAR
   t         : Tree ;
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
BEGIN
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      InternalError('this should have been folded in the calling procedure', __FILE__, __LINE__)
   ELSIF IsConst(operand1)
   THEN
      settype := GetType(operand2)
   ELSE
      settype := GetType(operand1)
   END ;
   IF CompareTrees(FindSize(settype), FindSize(Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(BuildIsNotSuperset(BuildConvert(GetWordType(), Mod2Gcc(operand1), FALSE),
                                BuildConvert(GetWordType(), Mod2Gcc(operand2), FALSE)),
             NIL, string(CreateLabelName(operand3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(Mod2Gcc(settype),
                                    Mod2Gcc(operand1), Mod2Gcc(operand2),
                                    GetMode(operand1)=LeftValue,
                                    GetMode(operand2)=LeftValue,
                                    IsConst(operand1), IsConst(operand2),
                                    BuildIsSuperset,
                                    falselabel) ;

      BuildGoto(string(CreateLabelName(operand3))) ;
      t := DeclareLabel(falselabel)
   END
END CodeIfSetLess ;


(*
   CodeIfLess - codes the quadruple if op1 < op2 then goto op3
*)

PROCEDURE CodeIfLess ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, operand1) ;
   DeclareConstant(CurrentQuadToken, operand2) ;
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      PushValue(operand1) ;
      PushValue(operand2) ;
      IF Less(CurrentQuadToken)
      THEN
         BuildGoto(string(CreateLabelName(operand3)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(operand1) OR (IsVar(operand1) AND IsSet(GetType(operand1))) OR
         IsConstSet(operand2) OR (IsVar(operand2) AND IsSet(GetType(operand2)))
   THEN
      CodeIfSetLess(CurrentQuad, operand1, operand2, operand3)
   ELSE
      DoJump(BuildLessThan(Mod2Gcc(operand1), Mod2Gcc(operand2)),
             NIL, string(CreateLabelName(operand3)))
   END
END CodeIfLess ;


(*
   CodeIfSetGre - 
*)

PROCEDURE CodeIfSetGre (quad: CARDINAL; operand1, operand2, operand3: CARDINAL) ;
VAR
   t         : Tree ;
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
BEGIN
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      InternalError('this should have been folded in the calling procedure', __FILE__, __LINE__)
   ELSIF IsConst(operand1)
   THEN
      settype := GetType(operand2)
   ELSE
      settype := GetType(operand1)
   END ;
   IF CompareTrees(FindSize(settype), FindSize(Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(BuildIsNotSubset(BuildConvert(GetWordType(), Mod2Gcc(operand1), FALSE),
                              BuildConvert(GetWordType(), Mod2Gcc(operand2), FALSE)),
             NIL, string(CreateLabelName(operand3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(Mod2Gcc(settype),
                                    Mod2Gcc(operand1), Mod2Gcc(operand2),
                                    GetMode(operand1)=LeftValue,
                                    GetMode(operand2)=LeftValue,
                                    IsConst(operand1), IsConst(operand2),
                                    BuildIsSubset,
                                    falselabel) ;

      BuildGoto(string(CreateLabelName(operand3))) ;
      t := DeclareLabel(falselabel)
   END
END CodeIfSetGre ;


(*
   CodeIfGre - codes the quadruple if op1 > op2 then goto op3
*)

PROCEDURE CodeIfGre ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, operand1) ;
   DeclareConstant(CurrentQuadToken, operand2) ;
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      PushValue(operand1) ;
      PushValue(operand2) ;
      IF Gre(CurrentQuadToken)
      THEN
         BuildGoto(string(CreateLabelName(operand3)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(operand1) OR (IsVar(operand1) AND IsSet(GetType(operand1))) OR
         IsConstSet(operand2) OR (IsVar(operand2) AND IsSet(GetType(operand2)))
   THEN
      CodeIfSetGre(CurrentQuad, operand1, operand2, operand3)
   ELSE
      DoJump(BuildGreaterThan(Mod2Gcc(operand1), Mod2Gcc(operand2)),
             NIL, string(CreateLabelName(operand3)))
   END
END CodeIfGre ;


(*
   CodeIfSetLessEqu - 
*)

PROCEDURE CodeIfSetLessEqu (quad: CARDINAL; operand1, operand2, operand3: CARDINAL) ;
VAR
   t         : Tree ;
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
BEGIN
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      InternalError('this should have been folded in the calling procedure', __FILE__, __LINE__)
   ELSIF IsConst(operand1)
   THEN
      settype := GetType(operand2)
   ELSE
      settype := GetType(operand1)
   END ;
   IF CompareTrees(FindSize(settype), FindSize(Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(BuildIsSubset(BuildConvert(GetWordType(), Mod2Gcc(operand1), FALSE),
                           BuildConvert(GetWordType(), Mod2Gcc(operand2), FALSE)),
             NIL, string(CreateLabelName(operand3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(Mod2Gcc(settype),
                                    Mod2Gcc(operand1), Mod2Gcc(operand2),
                                    GetMode(operand1)=LeftValue,
                                    GetMode(operand2)=LeftValue,
                                    IsConst(operand1), IsConst(operand2),
                                    BuildIsNotSubset,
                                    falselabel) ;

      BuildGoto(string(CreateLabelName(operand3))) ;
      t := DeclareLabel(falselabel)
   END
END CodeIfSetLessEqu ;


(*
   CodeIfLessEqu - codes the quadruple if op1 <= op2 then goto op3
*)

PROCEDURE CodeIfLessEqu ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;

   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, operand1) ;
   DeclareConstant(CurrentQuadToken, operand2) ;
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      PushValue(operand1) ;
      PushValue(operand2) ;
      IF LessEqu(CurrentQuadToken)
      THEN
         BuildGoto(string(CreateLabelName(operand3)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(operand1) OR (IsVar(operand1) AND IsSet(GetType(operand1))) OR
         IsConstSet(operand2) OR (IsVar(operand2) AND IsSet(GetType(operand2)))
   THEN
      CodeIfSetLessEqu(CurrentQuad, operand1, operand2, operand3)
   ELSE
      DoJump(BuildLessThanOrEqual(Mod2Gcc(operand1), Mod2Gcc(operand2)),
             NIL, string(CreateLabelName(operand3)))
   END
END CodeIfLessEqu ;


(*
   CodeIfSetGreEqu - 
*)

PROCEDURE CodeIfSetGreEqu (quad: CARDINAL; operand1, operand2, operand3: CARDINAL) ;
VAR
   t         : Tree ;
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
BEGIN
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      InternalError('this should have been folded in the calling procedure', __FILE__, __LINE__)
   ELSIF IsConst(operand1)
   THEN
      settype := GetType(operand2)
   ELSE
      settype := GetType(operand1)
   END ;
   IF CompareTrees(FindSize(settype), FindSize(Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(BuildIsSuperset(BuildConvert(GetWordType(), Mod2Gcc(operand1), FALSE),
                             BuildConvert(GetWordType(), Mod2Gcc(operand2), FALSE)),
             NIL, string(CreateLabelName(operand3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(Mod2Gcc(settype),
                                    Mod2Gcc(operand1), Mod2Gcc(operand2),
                                    GetMode(operand1)=LeftValue,
                                    GetMode(operand2)=LeftValue,
                                    IsConst(operand1), IsConst(operand2),
                                    BuildIsNotSuperset,
                                    falselabel) ;

      BuildGoto(string(CreateLabelName(operand3))) ;
      t := DeclareLabel(falselabel)
   END
END CodeIfSetGreEqu ;


(*
   CodeIfGreEqu - codes the quadruple if op1 >= op2 then goto op3
*)

PROCEDURE CodeIfGreEqu ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, operand1) ;
   DeclareConstant(CurrentQuadToken, operand2) ;
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      PushValue(operand1) ;
      PushValue(operand2) ;
      IF GreEqu(CurrentQuadToken)
      THEN
         BuildGoto(string(CreateLabelName(operand3)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(operand1) OR (IsVar(operand1) AND IsSet(GetType(operand1))) OR
         IsConstSet(operand2) OR (IsVar(operand2) AND IsSet(GetType(operand2)))
   THEN
      CodeIfSetGreEqu(CurrentQuad, operand1, operand2, operand3)
   ELSE
      DoJump(BuildGreaterThanOrEqual(Mod2Gcc(operand1), Mod2Gcc(operand2)),
             NIL, string(CreateLabelName(operand3)))
   END
END CodeIfGreEqu ;


(*
   CodeIfSetEqu - codes if operand1 = operand2 then goto operand3
                  Note that if operand1 and operand2 are not both constants
                  since this will have been evaluated in CodeIfEqu.
*)

PROCEDURE CodeIfSetEqu (quad: CARDINAL; operand1, operand2, operand3: CARDINAL) ;
VAR
   t         : Tree ;
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
BEGIN
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      InternalError('this should have been folded in the calling procedure', __FILE__, __LINE__)
   ELSIF IsConst(operand1)
   THEN
      settype := GetType(operand2)
   ELSE
      settype := GetType(operand1)
   END ;
   IF CompareTrees(FindSize(settype), FindSize(Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(BuildEqualTo(BuildConvert(GetIntegerType(), Mod2Gcc(operand1), FALSE),
                          BuildConvert(GetIntegerType(), Mod2Gcc(operand2), FALSE)),
             NIL, string(CreateLabelName(operand3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;
      
      BuildForeachWordInSetDoIfExpr(Mod2Gcc(settype),
                                    Mod2Gcc(operand1), Mod2Gcc(operand2),
                                    GetMode(operand1)=LeftValue,
                                    GetMode(operand2)=LeftValue,
                                    IsConst(operand1), IsConst(operand2),
                                    BuildNotEqualTo,
                                    falselabel) ;

      BuildGoto(string(CreateLabelName(operand3))) ;
      t := DeclareLabel(falselabel)
   END
END CodeIfSetEqu ;


(*
   CodeIfSetNotEqu - codes if operand1 # operand2 then goto operand3
                     Note that if operand1 and operand2 are not both constants
                     since this will have been evaluated in CodeIfNotEqu.
*)

PROCEDURE CodeIfSetNotEqu (quad: CARDINAL; operand1, operand2, operand3: CARDINAL) ;
VAR
   t         : Tree ;
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
BEGIN
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      InternalError('this should have been folded in the calling procedure', __FILE__, __LINE__)
   ELSIF IsConst(operand1)
   THEN
      settype := GetType(operand2)
   ELSE
      settype := GetType(operand1)
   END ;
   IF CompareTrees(FindSize(settype), FindSize(Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(BuildNotEqualTo(BuildConvert(GetWordType(), Mod2Gcc(operand1), FALSE),
                             BuildConvert(GetWordType(), Mod2Gcc(operand2), FALSE)),
             NIL, string(CreateLabelName(operand3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(Mod2Gcc(settype),
                                    Mod2Gcc(operand1), Mod2Gcc(operand2),
                                    GetMode(operand1)=LeftValue,
                                    GetMode(operand2)=LeftValue,
                                    IsConst(operand1), IsConst(operand2),
                                    BuildEqualTo,
                                    falselabel) ;
      
      BuildGoto(string(CreateLabelName(operand3))) ;
      t := DeclareLabel(falselabel)
   END
END CodeIfSetNotEqu ;


(*
   CodeIfEqu - codes the quadruple if op1 = op2 then goto op3
*)

PROCEDURE CodeIfEqu ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, operand1) ;
   DeclareConstant(CurrentQuadToken, operand2) ;
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      PushValue(operand1) ;
      PushValue(operand2) ;
      IF Equ(CurrentQuadToken)
      THEN
         BuildGoto(string(CreateLabelName(operand3)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(operand1) OR (IsVar(operand1) AND IsSet(GetType(operand1))) OR
         IsConstSet(operand2) OR (IsVar(operand2) AND IsSet(GetType(operand2)))
   THEN
      CodeIfSetEqu(CurrentQuad, operand1, operand2, operand3)
   ELSE
      DoJump(BuildEqualTo(Mod2Gcc(operand1), Mod2Gcc(operand2)),
             NIL, string(CreateLabelName(operand3)))
   END
END CodeIfEqu ;


(*
   CodeIfNotEqu - codes the quadruple if op1 # op2 then goto op3
*)

PROCEDURE CodeIfNotEqu ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, operand1) ;
   DeclareConstant(CurrentQuadToken, operand2) ;
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      PushValue(operand1) ;
      PushValue(operand2) ;
      IF NotEqu(CurrentQuadToken)
      THEN
         BuildGoto(string(CreateLabelName(operand3)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(operand1) OR (IsVar(operand1) AND IsSet(GetType(operand1))) OR
         IsConstSet(operand2) OR (IsVar(operand2) AND IsSet(GetType(operand2)))
   THEN
      CodeIfSetNotEqu(CurrentQuad, operand1, operand2, operand3)
   ELSE
      DoJump(BuildNotEqualTo(Mod2Gcc(operand1), Mod2Gcc(operand2)),
             NIL, string(CreateLabelName(operand3)))
   END
END CodeIfNotEqu ;


(*
   BuildIfVarInConstValue - if var in constsetvalue then goto trueexit
*)

PROCEDURE BuildIfVarInConstValue (constsetvalue: PtrToValue; var, trueexit: CARDINAL) ;
VAR
   low, high, n: CARDINAL ;
   truelabel   : String ;
BEGIN
   n := 1 ;
   truelabel := string(CreateLabelName(trueexit)) ;
   WHILE GetRange(constsetvalue, n, low, high) DO
      BuildIfInRangeGoto(Mod2Gcc(var), Mod2Gcc(low), Mod2Gcc(high), truelabel) ;
      INC(n)
   END
END BuildIfVarInConstValue ;


(*
   BuildIfNotVarInConstValue - if not (var in constsetvalue) then goto trueexit
*)

PROCEDURE BuildIfNotVarInConstValue (constsetvalue: PtrToValue; var, trueexit: CARDINAL) ;
VAR
   t           : Tree ;
   low, high, n: CARDINAL ;
   falselabel,
   truelabel   : String ;
BEGIN
   truelabel := string(CreateLabelName(trueexit)) ;
   n := 1 ;
   WHILE GetRange(constsetvalue, n, low, high) DO
      INC(n)
   END ;
   IF n=2
   THEN
      (* actually only one set range, so we invert it *)
      BuildIfNotInRangeGoto(Mod2Gcc(var), Mod2Gcc(low), Mod2Gcc(high), truelabel) ;
   ELSE
      n := 1 ;
      falselabel := string(Sprintf1(Mark(InitString('.Lset%d')), CurrentQuad)) ;
      WHILE GetRange(constsetvalue, n, low, high) DO
         BuildIfInRangeGoto(Mod2Gcc(var), Mod2Gcc(low), Mod2Gcc(high), falselabel) ;
         INC(n)
      END ;
      BuildGoto(truelabel) ;
      t := DeclareLabel(falselabel)
   END
END BuildIfNotVarInConstValue ;


(*
   CodeIfIn - code the quadruple: if op1 in op2 then goto op3
*)

PROCEDURE CodeIfIn ;
VAR
   operator: QuadOperator ;
   low,
   high,
   operand1,
   operand2,
   operand3: CARDINAL ;
   lowtree,
   hightree,
   offset  : Tree ;
   fieldno : INTEGER ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, operand1) ;
   DeclareConstant(CurrentQuadToken, operand2) ;
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      InternalError('should not get to here (if we do we should consider calling FoldIfIn)', __FILE__, __LINE__)
   ELSE
      IF IsConst(operand1)
      THEN
         fieldno := GetFieldNo(CurrentQuadToken, operand1, GetType(operand2), offset) ;
         IF fieldno>=0
         THEN
            PushValue(operand1) ;
            PushIntegerTree(offset) ;
            Sub ;
            BuildIfConstInVar(Mod2Gcc(GetType(operand2)),
                              Mod2Gcc(operand2), PopIntegerTree(),
                              GetMode(operand2)=LeftValue, fieldno,
                              string(CreateLabelName(operand3)))
         ELSE
            ErrorStringAt(Sprintf1(Mark(InitString('bit exceeded the range of set (%s)')),
                                   Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(operand1)))))),
                          CurrentQuadToken)
         END
      ELSIF IsConst(operand2)
      THEN
         (* builds a cascaded list of if statements *)
         PushValue(operand2) ;
         BuildIfVarInConstValue(GetValue(CurrentQuadToken), operand1, operand3)
      ELSE
         GetSetLimits(GetType(operand2), low, high) ;

         PushValue(low) ;
         lowtree := PopIntegerTree() ;
         PushValue(high) ;
         hightree := PopIntegerTree() ;

         BuildIfVarInVar(Mod2Gcc(GetType(operand2)),
                         Mod2Gcc(operand2), Mod2Gcc(operand1),
                         GetMode(operand2)=LeftValue,
                         lowtree, hightree,
                         string(CreateLabelName(operand3)))
      END
   END
END CodeIfIn ;


(*
   CodeIfNotIn - code the quadruple: if not (op1 in op2) then goto op3
*)

PROCEDURE CodeIfNotIn ;
VAR
   operator: QuadOperator ;
   low,
   high,
   operand1,
   operand2,
   operand3: CARDINAL ;
   lowtree,
   hightree,
   offset  : Tree ;
   fieldno : INTEGER ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, operand1) ;
   DeclareConstant(CurrentQuadToken, operand2) ;
   IF IsConst(operand1) AND IsConst(operand2)
   THEN
      InternalError('should not get to here (if we do we should consider calling FoldIfIn)', __FILE__, __LINE__)
   ELSE
      IF IsConst(operand1)
      THEN
         fieldno := GetFieldNo(CurrentQuadToken, operand1, GetType(operand2), offset) ;
         IF fieldno>=0
         THEN
            PushValue(operand1) ;
            PushIntegerTree(offset) ;
            Sub ;
            BuildIfNotConstInVar(Mod2Gcc(GetType(operand2)),
                                 Mod2Gcc(operand2), PopIntegerTree(),
                                 GetMode(operand2)=LeftValue, fieldno,
                                 string(CreateLabelName(operand3)))
         ELSE
            ErrorStringAt(Sprintf1(Mark(InitString('bit exceeded the range of set (%s)')),
                                   Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(operand2)))))),
                          CurrentQuadToken)
         END
      ELSIF IsConst(operand2)
      THEN
         (* builds a cascaded list of if statements *)
         PushValue(operand2) ;
         BuildIfNotVarInConstValue(GetValue(CurrentQuadToken), operand1, operand3)
      ELSE
         GetSetLimits(GetType(operand2), low, high) ;

         PushValue(low) ;
         lowtree := PopIntegerTree() ;
         PushValue(high) ;
         hightree := PopIntegerTree() ;

         BuildIfNotVarInVar(Mod2Gcc(GetType(operand2)),
                            Mod2Gcc(operand2), Mod2Gcc(operand1),
                            GetMode(operand2)=LeftValue,
                            lowtree, hightree,
                            string(CreateLabelName(operand3)))
      END
   END
END CodeIfNotIn ;


(*
------------------------------------------------------------------------------
   IndrX Operator           a = *b
------------------------------------------------------------------------------
   Sym1<X>   IndrX   Sym2<I>     Meaning     Mem[Sym1<I>] := Mem[constant]
   Sym1<X>   IndrX   Sym2<X>     Meaning     Mem[Sym1<I>] := Mem[Mem[Sym3<I>]]

   (Operand2 is the type of the data being indirectly copied)
*)

PROCEDURE CodeIndrX ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (*
      Follow the Quadruple rules:
   *)
   DeclareConstant(CurrentQuadToken, operand3) ;  (* checks to see whether it is a constant and declares it *)
   IF IsConstString(operand3)
   THEN
      InternalError('not expecting to index through a constant string', __FILE__, __LINE__)
   ELSE
      (*
         Mem[operand1] := Mem[Mem[operand3]]
      *)
      t := BuildAssignment(Mod2Gcc(operand1), BuildIndirect(Mod2Gcc(operand3), Mod2Gcc(operand2)))
   END
END CodeIndrX ;


(*
------------------------------------------------------------------------------
   XIndr Operator           *a = b
------------------------------------------------------------------------------
   Sym1<I>   XIndr   Sym2<X>     Meaning     Mem[constant]     := Mem[Sym3<I>]
   Sym1<X>   XIndr   Sym2<X>     Meaning     Mem[Mem[Sym1<I>]] := Mem[Sym3<I>]

   (Operand2 is the type of the data being indirectly copied)
*)

PROCEDURE CodeXIndr ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   DeclareConstant(CurrentQuadToken, operand3) ;
   (*
      Follow the Quadruple rule:

      Mem[Mem[Op1]] := Mem[Op3]
   *)
   IF IsProcType(operand2)
   THEN
      t := BuildAssignment(BuildIndirect(Mod2Gcc(operand1), GetIntegerType()), Mod2Gcc(operand3))
   ELSE
      t := BuildAssignment(BuildIndirect(Mod2Gcc(operand1), Mod2Gcc(operand2)), Mod2Gcc(operand3))
   END
END CodeXIndr ;


BEGIN
   ModuleName := NIL ;
   FileName   := NIL
END M2GenGCC.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I.:../gm2-libs:../gm2-libs-ch:../gm2-libiberty/ M2GenGCC.mod"
 * End:
 *)
