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
                    SubQuad,
                    IsProfileOn, IsOptimizeOn, IsCodeOn,
                    QuadToLineNo, QuadToTokenNo,
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
                        IsConst, IsProcedure, IsProcType, IsProcedureNested,
                        IsVar, IsVarParam,
                        IsUnbounded, IsArray, IsSet,
                        IsProcedureVariable,
                        IsUnboundedParam,
                        IsRecordField, IsFieldVarient, IsVarient, IsRecord,
                        IsExportQualified,
                        IsExported,
                        IsSubrange,
                        IsValueSolved, IsSizeSolved,
                        ForeachExportedDo,
                        ForeachImportedDo,
                        ForeachProcedureDo,
                        ForeachInnerModuleDo,
                        GetType, GetNth, GetNthParam,
                        GetSubrange, NoOfElements,
                        GetFirstUsed,
                        GetRegInterface,
                        GetProcedureQuads,
                        PutConstString,
                        PutConst,
                        NulSym ;

FROM M2Lexical IMPORT WriteError, NearToken, NearTokens, InternalError, WriteErrorFormat1,
                      WriteErrorFormat2, FormatWarningMessage2 ;

FROM M2Debug IMPORT Assert ;
FROM M2Error IMPORT BeginError, EndError ;
FROM M2Base IMPORT MixTypes, ActivationPointer, IsMathType, ArrayHigh, ArrayAddress, Cardinal, Char, Integer, Unbounded, Trunc ;
FROM M2Math IMPORT pi, sin, cos, tan, atan, sqrt ;
FROM NameKey IMPORT WriteKey, GetKey, MakeKey, KeyToCharStar, NulName ;
FROM M2System IMPORT Address, Word ;
FROM M2FileName IMPORT CalculateFileName ;
FROM StrLib IMPORT StrLen, StrEqual, StrConCat, StrCopy ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard, CardToStr ;
FROM M2AsmUtil IMPORT GetModuleInitName ;
FROM SymbolConversion IMPORT AddModGcc, Mod2Gcc, GccKnowsAbout ;
FROM Lists IMPORT RemoveItemFromList, IncludeItemIntoList, NoOfItemsInList, GetItemFromList ;
FROM M2ALU IMPORT PushIntegerTree, PopIntegerTree, PushCard ;

FROM M2GCCDeclare IMPORT StartDeclareMainModule, EndDeclareMainModule, DeclareConstant,
                         DeclareLocalVariables, PromoteToString, CompletelyResolved ;

FROM gccgm2 IMPORT Tree, GetIntegerZero, GetIntegerOne, GetIntegerType,
                   BuildVariableArrayAndDeclare,
                   BuiltInMemCopy, BuiltInAlloca,
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
                   BuildIfIn, BuildIfNotIn,
                   BuildIndirect,
                   BuildConvert, BuildTrunc,
                   BuiltInAlloca,
                   AreConstantsEqual,
                   DoJump,
                   BuildProcedureCall, BuildIndirectProcedureCall, BuildParam, BuildFunctValue,
                   BuildAsm, DebugTree,
                   ExpandExpressionStatement,
                   GetPointerType,
                   RememberConstant ;


CONST
   MaxString            = 8*1024 ;

VAR
   CurrentProcedure,
   PreviousScope            : CARDINAL ;(* the current procedure being compiled   *)
   CurrentQuad              : CARDINAL ;
   AbsoluteHead             : CARDINAL ;
   LastLine                 : CARDINAL ;(* The Last Line number emmitted with the *)
                                        (* generated code.                        *)
   CompilingMainModule      : BOOLEAN ; (* Determines whether the main module     *)
                                        (* quadrules are being processed.         *)
   LastOperator             : QuadOperator ; (* The last operator processed.      *)
   ModuleName,
   FileName                 : ARRAY [0..MaxString] OF CHAR ;
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
PROCEDURE FoldBecomes (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeBecomes ; FORWARD ;
PROCEDURE FoldAdd (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeAdd ; FORWARD ;
PROCEDURE FoldSub (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeSub ; FORWARD ;
PROCEDURE FoldMult (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeMult ; FORWARD ;
PROCEDURE FoldDiv (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeDiv ; FORWARD ;
PROCEDURE FoldMod (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeMod ; FORWARD ;
PROCEDURE FoldBit (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeBit ; FORWARD ;
PROCEDURE CodeGoto ; FORWARD ;
PROCEDURE CheckReferenced (q: CARDINAL; op: QuadOperator) ; FORWARD ;
PROCEDURE CodeLogicalOr ; FORWARD ;
PROCEDURE FoldLogicalOr (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeLogicalAnd ; FORWARD ;
PROCEDURE FoldLogicalAnd (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeSymmetricDifference ; FORWARD ;
PROCEDURE FoldSymmetricDifference (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE FoldNegate (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeNegate ; FORWARD ;
PROCEDURE FoldSize (quad: CARDINAL; l: List) ; FORWARD ;
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
PROCEDURE FoldOffset (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeHigh ; FORWARD ;
PROCEDURE FoldHigh (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeUnbounded ; FORWARD ;
PROCEDURE FoldBase (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeBase ; FORWARD ;
PROCEDURE FoldElementSize (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeElementSize ; FORWARD ;
PROCEDURE FoldCoerce (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeCoerce ; FORWARD ;
PROCEDURE FoldConvert (quad: CARDINAL; l: List) ; FORWARD ;
PROCEDURE CodeConvert ; FORWARD ;
PROCEDURE CodeMath ; FORWARD ;
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
   CheckReferenced(CurrentQuad, Operator) ;

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
   BitOp              : CodeBit |
   GotoOp             : CodeGoto |
   NegateOp           : CodeNegate |
   LogicalOrOp        : CodeLogicalOr |
   LogicalAndOp       : CodeLogicalAnd |
   LogicalXorOp       : CodeSymmetricDifference |
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
   MathOp             : CodeMath |

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
      BeginError ;
      WriteString('quad') ; WriteCard(CurrentQuad, 6) ; WriteLn ;
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
      (* WriteString('trying to declare constant ') ; WriteCard(sym, 0) ; *)
      IF (sym#NulSym) AND IsConst(sym) AND IsValueSolved(sym)
      THEN
         DeclareConstant(sym) ;
         RemoveItemFromList(l, sym)
         (* ; WriteString(' done') ; *)
      END ;
      (* ; WriteLn ; *)
      INC(i)
   END
END DeclareConstantLiterals ;


(*
   ResolveConstantExpressions - resolves constant expressions from the quadruple list.
                                It returns TRUE if one or more constants were folded.
*)

PROCEDURE ResolveConstantExpressions (l: List) : BOOLEAN ;
VAR
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

         CASE Operator OF

         LogicalOrOp        : FoldLogicalOr(quad, l) |
         LogicalAndOp       : FoldLogicalAnd(quad, l) |
         LogicalXorOp       : FoldSymmetricDifference(quad, l) |
         BecomesOp          : FoldBecomes(quad, l) |
         AddOp              : FoldAdd(quad, l) |
         SubOp              : FoldSub(quad, l) |
         MultOp             : FoldMult(quad, l) |
         DivOp              : FoldDiv(quad, l) |
         ModOp              : FoldMod(quad, l) |
         BitOp              : FoldBit(quad, l) |
         NegateOp           : FoldNegate(quad, l) |
         SizeOp             : FoldSize(quad, l) |
         OffsetOp           : FoldOffset(quad, l) |
         HighOp             : FoldHigh(quad, l) |
         BaseOp             : FoldBase(quad, l) |
         ElementSizeOp      : FoldElementSize(quad, l) |
         ConvertOp          : FoldConvert(quad, l) |
         CoerceOp           : FoldCoerce(quad, l)

         ELSE
            (* ignore quadruple as it is not associated with a constant expression *)
         END ;
         quad := GetNextQuad(quad)
      END ;
      Next := NoOfItemsInList(l)
   UNTIL Last=Next ;
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
   n       : CARDINAL ;
   str, obj: CARDINAL ;
   tree    : Tree ;
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
               DeclareConstant(obj) ;
               tree := ChainOnParamValue(tree, PromoteToString(str), Mod2Gcc(obj))
            ELSE
               WriteError('a constraint to the GNU ASM statement must be a constant string')
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
   n       : CARDINAL ;
   str, obj: CARDINAL ;
   tree    : Tree ;
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
               tree := AddStringToTreeList(tree, PromoteToString(str))
            ELSE
               WriteError('a constraint to the GNU ASM statement must be a constant string')
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
   DeclareConstant(string) ;
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
      SetFileNameAndLineNo(KeyToCharStar(MakeKey(FileName)), Operand3) ;
      EmitLineNote(KeyToCharStar(MakeKey(FileName)), Operand3)
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
   GetKey(GetSymName(Operand3), ModuleName) ;
   CalculateFileName(ModuleName, 'mod', FileName) ;
   SetFileNameAndLineNo(KeyToCharStar(MakeKey(FileName)), LastLine)
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
   GetKey(GetSymName(Operand3), ModuleName) ;
   CalculateFileName(ModuleName, 'def', FileName)
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

PROCEDURE CallInnerInit (Sym: CARDINAL) ;
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
      SetFileNameAndLineNo(KeyToCharStar(MakeKey(FileName)), op1) ;
      CurrentModuleInitFunction := BuildStart(KeyToCharStar(GetModuleInitName(op3)), op2#op3) ;
      AddModGcc(op3, CurrentModuleInitFunction) ;
      EmitLineNote(KeyToCharStar(MakeKey(FileName)), op1) ;
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
      SetFileNameAndLineNo(KeyToCharStar(MakeKey(FileName)), op1) ;
      EmitLineNote(KeyToCharStar(MakeKey(FileName)), op1) ;
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
   High      := BuildMult(BuildAdd(BuildIndirect(BuildAdd(BuildAddr(Mod2Gcc(param), FALSE),
                                                          BuildOffset(Mod2Gcc(GetLocalSym(Unbounded, ArrayHigh)), FALSE),
                                                          FALSE),
                                                 GetIntegerType()),
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
   GetKey(GetSymName(GetMainModule()), ModuleName) ;
   CalculateFileName(ModuleName, 'mod', FileName) ;
   SetFileNameAndLineNo(KeyToCharStar(MakeKey(FileName)), Operand1) ;
   BuildStartFunctionCode(Mod2Gcc(CurrentProcedure), IsExported(GetMainModule(), CurrentProcedure)) ;
   DeclareLocalVariables(CurrentProcedure) ;
   (* callee saves non var unbounded parameter contents *)
   SaveNonVarUnboundedParameters(CurrentProcedure) ;
   EmitLineNote(KeyToCharStar(MakeKey(FileName)), Operand1)
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
   SetFileNameAndLineNo(KeyToCharStar(MakeKey(FileName)), Operand1) ;
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
   DeclareConstant(Result) ;  (* checks to see whether it is a constant and declares it *)
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
   CheckStop(CurrentQuad) ;

   IF GetType(Operand3)=NulSym
   THEN
      tree := BuildProcedureCall(Mod2Gcc(Operand3), NIL)
   ELSE
      tree := BuildProcedureCall(Mod2Gcc(Operand3), Mod2Gcc(GetType(Operand3)))
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
   Operator: QuadOperator ;
   Operand1,
   Operand2,
   Operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, Operator, Operand1, Operand2, Operand3) ;
   IF Operand1>0
   THEN
      IF IsVarParam(Operand2, Operand1) AND IsConst(Operand3)
      THEN
         WriteError('cannot pass a constant as a VAR parameter')
      ELSE
         DeclareConstant(Operand3) ;
         (*
            ignore LeftAddr and RightAddr, but must be careful about size of Operand3.
            SIZE(operand3) will normally be TSIZE(ADDRESS) but NOT when it is unbounded
         *)
         BuildParam(Mod2Gcc(Operand3))
      END
   END
END CodeParam ;


(*
   CodeFunctValue - retrieves the function return value and assigns it
                    into a variable.
*)

PROCEDURE CodeFunctValue ;
VAR
   Operator: QuadOperator ;
   Operand1,
   Operand2,
   Operand3: CARDINAL ;
BEGIN
   GetQuad(CurrentQuad, Operator, Operand1, Operand2, Operand3) ;

   (*
      Operator : FunctValueOp
      Operand1 : The Returned Variable
      Operand2 : The Function Returning this Variable
   *)
   BuildFunctValue(Mod2Gcc(Operand1))
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
      WriteError('not expecting to be asked to find the address of a constant')
   ELSE
      t := BuildAssignment(Mod2Gcc(operand1),
                           BuildAddr(Mod2Gcc(operand3), FALSE))
   END
END CodeAddr ;


PROCEDURE stop ; BEGIN END stop ;

PROCEDURE CheckStop (q: CARDINAL) ;
BEGIN
   IF q=736
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

PROCEDURE FoldBecomes (quad: CARDINAL; l: List) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   DeclareConstant(operand3) ;  (* checks to see whether it is a constant literal and declares it *)
   IF IsConst(operand1) AND IsConst(operand3)
   THEN
      (* constant folding taking place, but have we resolved operand3 yet? *)
      IF GccKnowsAbout(operand3)
      THEN
         (* great, now we can tell gcc about the relationship between, op1 and op3 *)
         IF GccKnowsAbout(operand1)
         THEN
            NearToken('', QuadToTokenNo(quad)) ;
            WriteErrorFormat1('constant, %s, should not be reassigned', GetSymName(operand1))
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
                  AddModGcc(operand1, PopIntegerTree())
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
   DeclareConstant(operand3) ;  (* checks to see whether it is a constant and declares it *)
   IF IsConst(operand1) AND (NOT GccKnowsAbout(operand1))
   THEN
      AddModGcc(operand1,
                DeclareKnownConstant(Mod2Gcc(GetType(operand3)),
                                     Mod2Gcc(operand3))) ;
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

PROCEDURE FoldBinary (quad: CARDINAL; l: List; binop: BuildBinProcedure) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(operand3) ;
   DeclareConstant(operand2) ;
   IF IsConst(operand2) AND IsConst(operand3)
   THEN
      IF GccKnowsAbout(operand2) AND GccKnowsAbout(operand3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(operand1)
         THEN
            Assert(MixTypes(FindType(operand3), FindType(operand2))#NulSym) ;
            PutConst(operand1, MixTypes(FindType(operand3), FindType(operand2))) ;
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
   DeclareConstant(operand3) ;
   DeclareConstant(operand2) ;
   IF IsConst(operand1)
   THEN
      (* still have a constant which was not resolved, pass it to gcc *)
      Assert(MixTypes(FindType(operand3), FindType(operand2))#NulSym) ;
      PutConst(operand1, MixTypes(FindType(operand3), FindType(operand2))) ;
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
   FoldAdd - check addition for constant folding.
*)

PROCEDURE FoldAdd (quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(quad, l, BuildAdd)
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

PROCEDURE FoldSub (quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(quad, l, BuildSub)
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

PROCEDURE FoldMult (quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(quad, l, BuildMult)
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

PROCEDURE FoldDiv (quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(quad, l, BuildDiv)
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

PROCEDURE FoldMod (quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(quad, l, BuildMod)
END FoldMod ;


(*
   CodeMod - encode modulus.
*)

PROCEDURE CodeMod ;
BEGIN
   CodeBinary(BuildMod)
END CodeMod ;


(*
   FoldLogicalOr - check whether we can fold a logical or.
*)

PROCEDURE FoldLogicalOr (quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(quad, l, BuildLogicalOr)
END FoldLogicalOr ;


(*
   CodeLogicalOr - encode logical or.
*)

PROCEDURE CodeLogicalOr ;
BEGIN
   CodeBinary(BuildLogicalOr)
END CodeLogicalOr ;


(*
   FoldLogicalAnd - check whether we can fold a logical and.
*)

PROCEDURE FoldLogicalAnd (quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(quad, l, BuildLogicalAnd)
END FoldLogicalAnd ;


(*
   CodeLogicalAnd - encode logical and.
*)

PROCEDURE CodeLogicalAnd ;
BEGIN
   CodeBinary(BuildLogicalAnd)
END CodeLogicalAnd ;


(*
   FoldSymmetricDifference - check whether we can fold a logical difference.
*)

PROCEDURE FoldSymmetricDifference (quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(quad, l, BuildSymmetricDifference)
END FoldSymmetricDifference ;


(*
   CodeSymmetricDifference - code a logical difference.
*)

PROCEDURE CodeSymmetricDifference ;
BEGIN
   CodeBinary(BuildSymmetricDifference)
END CodeSymmetricDifference ;


(*
   FoldBit - check whether we can fold the BitOp.
             op1 := (1 << op3)
*)

PROCEDURE FoldBit (quad: CARDINAL; l: List) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(operand3) ;
   IF IsConst(operand1) AND IsConst(operand3)
   THEN
      IF GccKnowsAbout(operand3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(operand1)
         THEN
            AddModGcc(operand1,
                      DeclareKnownConstant(Mod2Gcc(GetType(operand3)),
                                           BuildLSL(GetIntegerOne(),
                                                    Mod2Gcc(operand3),
                                                    FALSE))) ;
            RemoveItemFromList(l, operand1) ;
            SubQuad(AbsoluteHead, quad)
         ELSE
            (* we can still fold the expression, but not the assignment, however, we will
               not do this here but in CodeBit
             *)
         END
      END
   END
END FoldBit ;


(*
   CodeBit - encode a bitop.
*)

PROCEDURE CodeBit ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(operand3) ;
   IF IsConst(operand1)
   THEN
      (* still have a constant which was not resolved, pass it to gcc *)
      AddModGcc(operand1,
                DeclareKnownConstant(Mod2Gcc(GetType(operand3)),
                                     BuildLSL(GetIntegerOne(),
                                              Mod2Gcc(operand3),
                                              FALSE))) ;
   ELSE
      t := BuildAssignment(Mod2Gcc(operand1),
                           BuildLSL(GetIntegerOne(), Mod2Gcc(operand3), FALSE))
   END
END CodeBit ;


(*
   FoldUnary - check whether we can fold the unop operation.
*)

PROCEDURE FoldUnary (quad: CARDINAL; l: List; unop: BuildUnaryProcedure; CoerceConst: Tree) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(operand3) ;
   IF IsConst(operand3)
   THEN
      IF GccKnowsAbout(operand3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(operand1)
         THEN
            IF CoerceConst=Tree(NIL)
            THEN
               CoerceConst := Tree(Mod2Gcc(GetType(operand3)))
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
   DeclareConstant(operand3) ;
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

PROCEDURE FoldNegate (quad: CARDINAL; l: List) ;
BEGIN
   FoldUnary(quad, l, BuildNegate, NIL)
END FoldNegate ;


(*
   CodeNegate - encode unary negate.
*)

PROCEDURE CodeNegate ;
BEGIN
   CodeUnary(BuildNegate, NIL)
END CodeNegate ;


(*
   FoldSize - check unary SIZE for constant folding.
*)

PROCEDURE FoldSize (quad: CARDINAL; l: List) ;
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

PROCEDURE FoldOffset (quad: CARDINAL; l: List) ;
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
   DeclareConstant(operand3) ;
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
   DeclareConstant(operand3) ;
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
      NearToken('HIGH operator only allowed on one dimensional arrays',
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

PROCEDURE FoldHigh (quad: CARDINAL; l: List) ;
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
   DeclareConstant(operand3) ;
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
   DeclareConstant(operand3) ;
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

   DeclareConstant(operand3) ;
   IF IsConstString(operand3)
   THEN
      t := BuildAssignment(BuildIndirect(Mod2Gcc(operand1), GetIntegerType()), BuildAddr(PromoteToString(operand3), FALSE))
   ELSIF IsUnbounded(GetType(operand3))
   THEN
      Addr := BuildIndirect(BuildAdd(BuildAddr(Mod2Gcc(operand3), FALSE),
                                     BuildOffset(Mod2Gcc(GetLocalSym(Unbounded, ArrayAddress)), FALSE),
                                     FALSE),
                            GetIntegerType()) ;
      t := BuildAssignment(BuildIndirect(Mod2Gcc(operand1), GetIntegerType()), Addr)
   ELSE
      t := BuildAssignment(BuildIndirect(Mod2Gcc(operand1), GetIntegerType()), BuildAddr(Mod2Gcc(operand3), FALSE))
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

PROCEDURE FoldBase (quad: CARDINAL; l: List) ;
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

PROCEDURE FoldElementSize (quad: CARDINAL; l: List) ;
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

PROCEDURE FoldConvert (quad: CARDINAL; l: List) ;
BEGIN
   FoldBinary(quad, l, BuildConvert)
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
   DeclareConstant(operand3) ;  (* checks to see whether it is a constant literal and declares it *)
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
         WriteError('procedure address can only be stored in a word size operand')
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
      FormatWarningMessage2('TYPE Coersion can only be achieved with types of the same size, problem with %s and %s, attempting convert via VAL instead',
                            GetSymName(operand2), GetSymName(operand3), QuadToTokenNo(CurrentQuad)) ;
      CodeConvert
   END
END CodeCoerce ;


(*
   FoldCoerce -
*)

PROCEDURE FoldCoerce (quad: CARDINAL; l: List) ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
BEGIN
   GetQuad(quad, operator, operand1, operand2, operand3) ;
   DeclareConstant(operand3) ;  (* checks to see whether it is a constant literal and declares it *)
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
            WriteError('procedure address can only be stored in a word size operand')
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
   DeclareConstant(operand3) ;  (* checks to see whether it is a constant literal and declares it *)
   IF operand1=Trunc
   THEN
      t := BuildAssignment(Mod2Gcc(operand2), BuildTrunc(Mod2Gcc(operand3)))
   ELSE
      InternalError('unknown math operator', __FILE__, __LINE__)
   END ;
(*
   ELSIF Op1=sin
   THEN
      AddInstruction(fsin, 0) ;
      pop(Op2)
   ELSIF Op1=cos
   THEN
      AddInstruction(fcos, 0) ;
      pop(Op2)
   ELSIF Op1=tan
   THEN
      AddInstruction(ftan, 0) ;
      pop(Op2)
   ELSIF Op1=atan
   THEN
      AddInstruction(fatan, 0) ;
      pop(Op2)
   ELSIF Op1=sqrt
   THEN
      AddInstruction(fsqrt, 0) ;
      pop(Op2)
   ELSE
      InternalError('unknown math operator', __FILE__, __LINE__)
   END
*)
END CodeMath ;


(*
   CreateLabelName - creates a namekey from quadruple, q.
*)

PROCEDURE CreateLabelName (q: CARDINAL) : CARDINAL ;
VAR
   str: ARRAY [0..30] OF CHAR ;
BEGIN
   CardToStr(q, 0, str) ;
   StrConCat('.L', str, str) ;   (* prefixed by . to ensure that no Modula-2 identifiers clash *)
   RETURN( MakeKey(str) )
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
   BuildGoto(KeyToCharStar(CreateLabelName(operand3)))
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
      t := DeclareLabel(KeyToCharStar(CreateLabelName(q)))
   END
END CheckReferenced ;


(*
   CodeIfLess - codes the quadruple if op1 < op2 then goto op3
*)

PROCEDURE CodeIfLess ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(operand1) ;
   DeclareConstant(operand2) ;
   DoJump(BuildLessThan(Mod2Gcc(operand1), Mod2Gcc(operand2)),
          NIL, KeyToCharStar(CreateLabelName(operand3)))
END CodeIfLess ;


(*
   CodeIfGre - codes the quadruple if op1 > op2 then goto op3
*)

PROCEDURE CodeIfGre ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(operand1) ;
   DeclareConstant(operand2) ;
   DoJump(BuildGreaterThan(Mod2Gcc(operand1), Mod2Gcc(operand2)),
          NIL, KeyToCharStar(CreateLabelName(operand3)))
END CodeIfGre ;


(*
   CodeIfLessEqu - codes the quadruple if op1 <= op2 then goto op3
*)

PROCEDURE CodeIfLessEqu ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;

   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(operand1) ;
   DeclareConstant(operand2) ;
   DoJump(BuildLessThanOrEqual(Mod2Gcc(operand1), Mod2Gcc(operand2)),
          NIL, KeyToCharStar(CreateLabelName(operand3)))
END CodeIfLessEqu ;


(*
   CodeIfGreEqu - codes the quadruple if op1 >= op2 then goto op3
*)

PROCEDURE CodeIfGreEqu ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(operand1) ;
   DeclareConstant(operand2) ;
   DoJump(BuildGreaterThanOrEqual(Mod2Gcc(operand1), Mod2Gcc(operand2)),
          NIL, KeyToCharStar(CreateLabelName(operand3)))
END CodeIfGreEqu ;


(*
   CodeIfEqu - codes the quadruple if op1 = op2 then goto op3
*)

PROCEDURE CodeIfEqu ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(operand1) ;
   DeclareConstant(operand2) ;
   DoJump(BuildEqualTo(Mod2Gcc(operand1), Mod2Gcc(operand2)),
          NIL, KeyToCharStar(CreateLabelName(operand3)))
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
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(operand1) ;
   DeclareConstant(operand2) ;
   DoJump(BuildNotEqualTo(Mod2Gcc(operand1), Mod2Gcc(operand2)),
          NIL, KeyToCharStar(CreateLabelName(operand3)))
END CodeIfNotEqu ;


(*
   CodeIfIn - code the quadruple: if (op1 & op2) # 0 then goto op3
*)

PROCEDURE CodeIfIn ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(operand1) ;
   DeclareConstant(operand2) ;
   DoJump(BuildIfIn(Mod2Gcc(operand1), Mod2Gcc(operand2)),
          NIL, KeyToCharStar(CreateLabelName(operand3)))
END CodeIfIn ;


(*
   CodeIfNotIn - code the quadruple: if (op1 & op2) = 0 then goto op3
*)

PROCEDURE CodeIfNotIn ;
VAR
   operator: QuadOperator ;
   operand1,
   operand2,
   operand3: CARDINAL ;
   t       : Tree ;
BEGIN
   GetQuad(CurrentQuad, operator, operand1, operand2, operand3) ;
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(operand1) ;
   DeclareConstant(operand2) ;
   DoJump(BuildIfNotIn(Mod2Gcc(operand1), Mod2Gcc(operand2)),
          NIL, KeyToCharStar(CreateLabelName(operand3)))
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
   DeclareConstant(operand3) ;  (* checks to see whether it is a constant and declares it *)
   IF IsConstString(operand3)
   THEN
      InternalError('not expecting to index through a constant string', __FILE__, __LINE__)
   ELSE
      (*
         Mem[operand1] := Mem[Mem[operand3]]
      *)
      t := BuildAssignment(Mod2Gcc(operand1), BuildIndirect(Mod2Gcc(operand3), Mod2Gcc(operand2))) ;
(*
      IF (GetType(operand1)=LongReal) OR (GetType(operand1)=Real)
      THEN
         t := BuildAssignment(Mod2Gcc(operand1), BuildIndirect(Mod2Gcc(operand3), Mod2Gcc(GetType(operand1))))
      ELSE
         t := BuildAssignment(Mod2Gcc(operand1), BuildIndirect(Mod2Gcc(operand3), GetIntegerType()))
      END
*)
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
   DeclareConstant(operand3) ;
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


END M2GenGCC.
(*
 * Local variables:
 *  compile-command: "m2f -quiet -g -verbose -M \"../../libs ../ ../../../../ .\" -o M2GenGCC.o M2GenGCC.mod"
 * End:
 *)
