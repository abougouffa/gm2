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
IMPLEMENTATION MODULE M2Quads ;


FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Debug IMPORT Assert, WriteDebug ;
FROM NameKey IMPORT Name, NulName, MakeKey, GetKey, makekey, KeyToCharStar ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3 ;

FROM DynamicStrings IMPORT String, string, InitString, KillString, 
                           ConCat, InitStringCharStar, Dup, Mark ;

FROM SymbolTable IMPORT ModeOfAddr, GetMode, GetSymName, IsUnknown,
                        MakeTemporary, MakeConstLit, MakeConstLitString,
                        RequestSym,
                        GetType, GetLowestType, SkipType,
                        GetScope, GetCurrentScope,
                        GetSubrange,
                        GetModule, GetMainModule,
                        GetCurrentModule, GetFileModule, GetLocalSym,
                        GetStringLength, GetString,
                        GetParam,
                        GetNth, GetNthParam,
                        GetFirstUsed, GetDeclared,
                        GetVarQuads, GetVarReadQuads, GetVarWriteQuads,
                        GetVarWriteLimitQuads, GetVarReadLimitQuads,
                        GetVarScope,
                        GetModuleQuads, GetProcedureQuads,
                        PutConstString,
                        PutModuleStartQuad, PutModuleEndQuad,
                        PutProcedureStartQuad, PutProcedureEndQuad,
                        PutVar, PutConstSet,
                        PutVarReadQuad, RemoveVarReadQuad,
                        PutVarWriteQuad, RemoveVarWriteQuad,
                        IsVarParam, IsProcedure, IsPointer, IsParameter,
                        IsUnboundedParam, IsEnumeration, IsDefinitionForC,
                        UsesVarArgs,
                        NoOfElements,
                        NoOfParam,
                        StartScope, EndScope,
                        GetParent, IsRecordField, IsFieldVarient, IsRecord,
                        IsVar, IsProcType, IsType, IsSubrange, IsExported,
                        IsConst, IsConstString, IsModule, IsDefImp,
                        IsArray, IsUnbounded, IsProcedureNested,
                        IsSet, IsConstSet,
                        IsSubscript,
                        IsTemporary,
                        IsAModula2Type,
                        PutVarTypeAndSize,
                        PushSize, PushValue, PopValue,
                        ForeachFieldEnumerationDo,
                        NulSym ;

FROM M2Configure IMPORT PushParametersLeftToRight, UsingGCCBackend ;

FROM M2Comp IMPORT CompilingImplementationModule,
                   CompilingProgramModule ;

FROM M2LexBuf IMPORT currenttoken,
                     GetToken,
                     GetFileName,
                     GetTokenNo, GetLineNo, GetPreviousTokenLineNo ;

FROM M2Error IMPORT Error,
                    InternalError,
                    WriteFormat0, WriteFormat1, WriteFormat2, WriteFormat3,
                    NewError, NewWarning, ErrorFormat0, ErrorFormat1,
                    ErrorFormat2, ErrorFormat3, FlushErrors, ChainError,
                    ErrorString,
                    ErrorStringAt, ErrorStringAt2, ErrorStringsAt2,
                    WarnStringAt, WarnStringAt2, WarnStringsAt2 ;

FROM M2Printf IMPORT printf0, printf1, printf2, printf3, printf4 ;

FROM M2Reserved IMPORT PlusTok, MinusTok, TimesTok, DivTok, ModTok,
                       DivideTok,
                       OrTok, AndTok, AmbersandTok,
                       EqualTok, LessEqualTok, GreaterEqualTok,
                       LessTok, GreaterTok, HashTok, LessGreaterTok,
                       InTok,
                       UpArrowTok, RParaTok, LParaTok, CommaTok,
                       SemiColonTok, toktype ;

FROM M2Base IMPORT True, False, Boolean, Cardinal, Integer, Char,
                   Real, LongReal, ShortReal,
                   MixTypes,
                   IsExpressionCompatible, IsAssignmentCompatible,
                   CheckAssignmentCompatible, CheckExpressionCompatible,
                   Unbounded, ArrayAddress, ArrayHigh,
                   High, LengthS, New, Dispose, Inc, Dec, Incl, Excl,
                   Cap, Abs, Odd,
                   Ord, Chr, Convert, Val, Float, Trunc, Min, Max,
                   IsPseudoBaseProcedure, IsPseudoBaseFunction,
                   IsMathType,
                   IsBaseType, GetBaseTypeMinMax, ActivationPointer ;

FROM M2System IMPORT IsPseudoSystemFunction, IsSystemType, GetSystemTypeMinMax,
                     Adr, TSize, AddAdr, SubAdr, DifAdr, Cast, MakeAdr,
                     Address, Byte, Word ;

FROM M2Size IMPORT Size ;
FROM M2Bitset IMPORT Bitset ;
FROM M2ALU IMPORT PushInt, Gre, Less, PushNulSet, AddBitRange, AddBit, IsGenericNulSet ;

FROM Lists IMPORT List, InitList, GetItemFromList, NoOfItemsInList, PutItemIntoList,
                  IsItemInList, KillList, IncludeItemIntoList ;

FROM M2Constants IMPORT MakeNewConstFromValue ;

FROM M2Options IMPORT BoundsChecking, ReturnChecking,
                      Pedantic, CompilerDebugging, GenerateDebugging,
                      GenerateLineDebug,
                      Profiling, Coding, Optimizing ;

FROM M2Pass IMPORT IsPassCodeGeneration, IsNoPass ;
FROM M2Stack IMPORT Stack, InitStack, KillStack, Push, Pop, Peep, IsEmpty, NoOfItemsInStack ;


CONST
   MaxQuad        = 50000 ;
   DebugStack     = FALSE ;

TYPE
   BoolFrame = POINTER TO boolFrame ;  (* using intemediate type helps p2c *)
   boolFrame =            RECORD
                             TrueExit : CARDINAL ;
                             FalseExit: CARDINAL ;
                             BooleanOp: BOOLEAN ;
                          END ;

(* QuadFrame = POINTER TO quadFrame ; *)
   QuadFrame =            RECORD
                             Operator           : QuadOperator ;
                             Operand1           : CARDINAL ;
                             Operand2           : CARDINAL ;
                             Operand3           : CARDINAL ;
                             Next               : CARDINAL ;     (* Next quadruple                 *)
                             LineNo             : CARDINAL ;     (* Line No of source text         *)
                             TokenNo            : CARDINAL ;     (* Token No of source text        *)
                             NoOfTimesReferenced: CARDINAL ;     (* No of times quad is referenced *)
                          END ;

   WithFrame = POINTER TO withFrame ;
   withFrame =            RECORD
                             PtrSym   : CARDINAL ;
                             RecordSym: CARDINAL ;
                          END ;

   ForLoopInfo = RECORD
                    IncrementQuad,
                    StartOfForLoop,                              (* we keep a list of all for      *)
                    EndOfForLoop,                                (* loops so we can check index    *)
                    ForLoopIndex  : List ;                       (* variables are not abused       *)
                 END ;

   LineNote  = POINTER TO lineFrame ;
   lineFrame =            RECORD
                             Line: CARDINAL ;
                             File: Name ;
                             Next: LineNote ;
                          END ;
VAR
   LineStack,
   AutoStack,
   BoolStack,
   ExitStack,
   WithStack,
   ReturnStack          : Stack ;     (* Return quadruple of the procedure.      *)
   SuppressWith         : BOOLEAN ;

   Quads                : ARRAY [1..MaxQuad] OF QuadFrame ;
   NextQuad             : CARDINAL ;  (* Next quadruple number to be created.    *)
   FreeList             : CARDINAL ;  (* FreeList of quadruples.                 *)
   CurrentProc          : CARDINAL ;  (* Current procedure being compiled, used  *)
                                      (* to determine which procedure a RETURN   *)
                                      (* ReturnValueOp must have as its 3rd op.  *)
   InitQuad             : CARDINAL ;  (* Initial Quad BackPatch that starts the  *)
                                      (* suit of Modules.                        *)
   LastQuadNo           : CARDINAL ;  (* Last Quadruple accessed by GetQuad.     *)
   LogicalOrTok,                      (* Internal _LOR token.                    *)
   LogicalAndTok,                     (* Internal _LAND token.                   *)
   LogicalXorTok,                     (* Internal _LXOR token.                   *)
   LogicalDifferenceTok : Name ;      (* Internal _LDIFF token.                  *)
   NoOfDynamic          : CARDINAL ;
   IsAutoOn,                          (* should parser automatically push idents *)
   MustNotCheckBounds   : BOOLEAN ;
   ForInfo              : ForLoopInfo ;  (* start and end of all FOR loops       *)
   GrowInitialization   : CARDINAL ;  (* upper limit of where the initialized    *)
                                      (* quadruples.                             *)
   QuadrupleGeneration  : BOOLEAN ;      (* should we be generating quadruples?  *)
   FreeLineList         : LineNote ;  (* free list of line notes                 *)


(*
   Rules for file and initialization quadruples:

   StartModFileOp  - indicates that this file (module) has produced the
                     following code
   StartDefFileOp  - indicates that this definition module has produced
                     this code.
   EndFileOp       - indicates that a module has finished
   StartOp         - the start of the initialization code of a module
   EndOp           - the end of the above
*)

(* %%%FORWARD%%%
PROCEDURE PushLineNote (l: LineNote) ; FORWARD ;
PROCEDURE PopLineNo () : LineNote ; FORWARD ;
PROCEDURE UseLineNote (l: LineNote) ; FORWARD ;
PROCEDURE BuildRealFuncProcCall (IsFunc, IsForC: BOOLEAN) ; FORWARD ;
PROCEDURE CheckForIndex (Start, End, Omit: CARDINAL; IndexSym: CARDINAL) ; FORWARD ;
PROCEDURE BuildMaxFunction ; FORWARD ;
PROCEDURE BuildMinFunction ; FORWARD ;
PROCEDURE BuildAddAdrFunction ; FORWARD ;
PROCEDURE BuildSubAdrFunction ; FORWARD ;
PROCEDURE BuildDifAdrFunction ; FORWARD ;
PROCEDURE BuildCastFunction ; FORWARD ;
PROCEDURE BuildMakeAdrFunction ; FORWARD ;
PROCEDURE CheckVariablesInBlock (BlockSym: CARDINAL) ; FORWARD ;
PROCEDURE CheckRemoveVariableRead (Sym: CARDINAL; Quad: CARDINAL) ; FORWARD ;
PROCEDURE CheckRemoveVariableWrite (Sym: CARDINAL; Quad: CARDINAL) ; FORWARD ;
PROCEDURE CheckFunctionReturn (ProcSym: CARDINAL) ; FORWARD ;
PROCEDURE CheckAddVariableWrite (Sym: CARDINAL; Quad: CARDINAL) ; FORWARD ;
PROCEDURE CheckAddVariableRead (Sym: CARDINAL; Quad: CARDINAL) ; FORWARD ;
PROCEDURE ConvertBooleanToVariable (i: CARDINAL) ; FORWARD ;
PROCEDURE BuildFloatFunction ; FORWARD ;
PROCEDURE BuildTruncFunction ; FORWARD ;
PROCEDURE CheckAssignCompatible (Des, Exp: CARDINAL) ; FORWARD ;
PROCEDURE CheckForLogicalOperator (Tok: Name; e1, t1, e2, t2: CARDINAL) : Name ; FORWARD ;
PROCEDURE DisplayType (Sym: CARDINAL) ; FORWARD ;
PROCEDURE CheckProcedureParameters (IsForC: BOOLEAN) ; FORWARD ;
PROCEDURE CheckParameter (Call, Param, ProcSym: CARDINAL; i: CARDINAL; TypeList: List) ; FORWARD ;
PROCEDURE FailParameter (CurrentState : ARRAY OF CHAR;
                         Given        : CARDINAL;
                         Expecting    : CARDINAL;
                         ProcedureSym : CARDINAL;
                         ParameterNo  : CARDINAL) ; FORWARD ;
PROCEDURE DisplayType (Sym: CARDINAL) ; FORWARD ;
PROCEDURE AlterReference (Head, OldQuad, NewQuad: CARDINAL) ; FORWARD ;
PROCEDURE RemoveReference (q: CARDINAL) ; FORWARD ;
PROCEDURE ManipulateReference (q: CARDINAL; to: CARDINAL) ; FORWARD ;
PROCEDURE AreConstant (b: BOOLEAN) : ModeOfAddr ; FORWARD ;
PROCEDURE AssignUnboundedNonVar (Sym, UnboundedSym, ParamType: CARDINAL) ; FORWARD ;
PROCEDURE AssignUnboundedVar (Sym, UnboundedSym, ParamType: CARDINAL) ; FORWARD ;
PROCEDURE BackPatch (QuadNo, Value: CARDINAL) ; FORWARD ;
PROCEDURE BuildAccessWithField ; FORWARD ;
PROCEDURE BuildAdrFunction ; FORWARD ;
PROCEDURE BuildChrFunction ; FORWARD ;
PROCEDURE BuildConvertFunction ; FORWARD ;
PROCEDURE BuildOddFunction ; FORWARD ;
PROCEDURE BuildAbsFunction ; FORWARD ;
PROCEDURE BuildCapFunction ; FORWARD ;
PROCEDURE BuildDecProcedure ; FORWARD ;
PROCEDURE BuildDisposeProcedure ; FORWARD ;
PROCEDURE BuildDynamicArray ; FORWARD ;
PROCEDURE BuildHighFromChar ; FORWARD ;
PROCEDURE BuildHighFromArray ; FORWARD ;
PROCEDURE BuildHighFromString ; FORWARD ;
PROCEDURE BuildHighFromUnbounded ; FORWARD ;
PROCEDURE BuildHighFunction ; FORWARD ;
PROCEDURE BuildLengthFunction ; FORWARD ;
PROCEDURE BuildIncProcedure ; FORWARD ;
PROCEDURE BuildNewProcedure ; FORWARD ;
PROCEDURE BuildInclProcedure ; FORWARD ;
PROCEDURE BuildExclProcedure ; FORWARD ;
PROCEDURE BuildOrdFunction ; FORWARD ;
PROCEDURE ManipulatePseudoCallParameters ; FORWARD ;
PROCEDURE BuildPseudoFunctionCall ; FORWARD ;
PROCEDURE BuildPseudoProcedureCall ; FORWARD ;
PROCEDURE BuildRealFunctionCall ; FORWARD ;
PROCEDURE BuildRealProcedureCall ; FORWARD ;
PROCEDURE BuildSizeFunction ; FORWARD ;
PROCEDURE BuildStaticArray ; FORWARD ;
PROCEDURE BuildTSizeFunction ; FORWARD ;
PROCEDURE BuildTypeCoercion ; FORWARD ;
PROCEDURE BuildValFunction ; FORWARD ;
PROCEDURE CheckBooleanId ; FORWARD ;
PROCEDURE DisplayQuad (QuadNo: CARDINAL) ; FORWARD ;
PROCEDURE DisposeQuad (QuadNo: CARDINAL) ; FORWARD ;
PROCEDURE GenQuad (Operation: QuadOperator;
                   Op1, Op2, Op3: CARDINAL) ; FORWARD ;
PROCEDURE GetItemPointedTo (Sym: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE Init ; FORWARD ;
PROCEDURE InitQuads ; FORWARD ;
PROCEDURE IsBoolean (pos: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE IsReallyAPointer (Sym: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE MakeOp (t: Name) : QuadOperator ; FORWARD ;
PROCEDURE ManipulateParameters (IsForC: BOOLEAN) ; FORWARD ;
PROCEDURE Merge (QuadList1, QuadList2: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE NewQuad (VAR QuadNo: CARDINAL) ; FORWARD ;
PROCEDURE PopBool (VAR True, False: CARDINAL) ; FORWARD ;
PROCEDURE PopExit() : CARDINAL ; FORWARD ;
PROCEDURE PopInit (VAR q: CARDINAL) ; FORWARD ;
PROCEDURE PopWith ; FORWARD ;
PROCEDURE PushBool (True, False: CARDINAL) ; FORWARD ;
PROCEDURE PushExit (Exit: CARDINAL) ; FORWARD ;
PROCEDURE PushWith (Sym, Type: CARDINAL) ; FORWARD ;
PROCEDURE UnboundedNonVarLinkToArray (ArraySym, UnboundedSym, ParamType: CARDINAL) ; FORWARD ;
PROCEDURE UnboundedVarLinkToArray (ArraySym, UnboundedSym, ParamType: CARDINAL) ; FORWARD ;
PROCEDURE WriteMode (Mode: ModeOfAddr) ; FORWARD ;
PROCEDURE WriteOperand (Sym: CARDINAL) ; FORWARD ;
PROCEDURE WriteOperator (Operator: QuadOperator) ; FORWARD ;
PROCEDURE WriteQuad (BufferQuad: CARDINAL) ; FORWARD ;
PROCEDURE IsBoolean (pos: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE OperandT (pos: CARDINAL) : WORD ; FORWARD ;
PROCEDURE OperandF (pos: CARDINAL) : WORD ; FORWARD ;
PROCEDURE PopN (n: CARDINAL) ; FORWARD ;
   %%%FORWARD%%% *)


(*
   SetOptionProfiling - builds a profile quadruple if the profiling
                        option was given to the compiler.
*)

PROCEDURE SetOptionProfiling (b: BOOLEAN) ;
BEGIN
   IF b#Profiling
   THEN
      IF b
      THEN
         BuildProfileOn
      ELSE
         BuildProfileOff
      END ;
      Profiling := b
   END
END SetOptionProfiling ;


(*
   SetOptionCoding - builds a code quadruple if the profiling
                        option was given to the compiler.
*)

PROCEDURE SetOptionCoding (b: BOOLEAN) ;
BEGIN
   IF b#Coding
   THEN
      IF b
      THEN
         BuildCodeOn
      ELSE
         BuildCodeOff
      END ;
      Coding := b
   END
END SetOptionCoding ;


(*
   SetOptionOptimizing - builds a quadruple to say that the optimization option
                         has been found in a comment.
*)

PROCEDURE SetOptionOptimizing (b: BOOLEAN) ;
BEGIN
   IF b
   THEN
      BuildOptimizeOn
   ELSE
      BuildOptimizeOff
   END
END SetOptionOptimizing ;


(*
   Opposite - returns the opposite comparison operator.
*)

PROCEDURE Opposite (Operator: QuadOperator) : QuadOperator ;
VAR
   Op: QuadOperator ;
BEGIN
   CASE Operator OF

   IfNotEquOp : Op := IfEquOp |
   IfEquOp    : Op := IfNotEquOp |
   IfLessEquOp: Op := IfGreOp |
   IfGreOp    : Op := IfLessEquOp |
   IfGreEquOp : Op := IfLessOp |
   IfLessOp   : Op := IfGreEquOp |
   IfInOp     : Op := IfNotInOp |
   IfNotInOp  : Op := IfInOp

   ELSE
      InternalError('unexpected operator', __FILE__, __LINE__)
   END ;
   RETURN( Op )
END Opposite ;


(*
   IsReferenced - returns true if QuadNo is referenced inside the Quad list
                  determined by Head.
*)

PROCEDURE IsReferenced (Head: CARDINAL; QuadNo: CARDINAL) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   WITH Quads[QuadNo] DO
      RETURN( (Operator=NewLocalVarOp) OR (NoOfTimesReferenced>0) )
   END
END IsReferenced ;


(*
   IsBackReference - returns TRUE if quadruple, q, is referenced from a quad further on.
*)

PROCEDURE IsBackReference (q: CARDINAL) : BOOLEAN ;
VAR
   i            : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   i := q ;
   WHILE i#0 DO
      GetQuad(i, op, op1, op2, op3) ;
      CASE op OF

      NewLocalVarOp,
      KillLocalVarOp,
      EndOp,
      StartOp,
      EndFileOp,
      StartDefFileOp,
      StartModFileOp:  RETURN( FALSE ) |       (* run into end of procedure or module *)

      GotoOp,
      IfEquOp,
      IfLessEquOp,
      IfGreEquOp,
      IfGreOp,
      IfLessOp,
      IfNotEquOp,
      IfInOp,
      IfNotInOp     :  IF op3=q
                       THEN
                          RETURN( TRUE )
                       END

      END ;
      i := GetNextQuad(i)
   END ;
   InternalError('fix this for the sake of efficiency..', __FILE__, __LINE__)
END IsBackReference ;


(*
   IsUnConditional - returns true if QuadNo is an unconditional jump.
*)

PROCEDURE IsUnConditional (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Quads[QuadNo] DO
      CASE Operator OF

      CallOp,
      ReturnOp,
      GotoOp     : RETURN( TRUE )

      ELSE
         RETURN( FALSE )
      END ;
   END
END IsUnConditional ;


(*
   IsConditional - returns true if QuadNo is a conditional jump.
*)

PROCEDURE IsConditional (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Quads[QuadNo] DO
      CASE Operator OF

      IfInOp,
      IfNotInOp,
      IfEquOp,
      IfNotEquOp,
      IfLessOp,
      IfLessEquOp,
      IfGreOp,
      IfGreEquOp : RETURN( TRUE )

      ELSE
         RETURN( FALSE )
      END ;
   END
END IsConditional ;


(*
   IsBackReferenceConditional - returns TRUE if quadruple, q, is referenced from
                                a conditional quad further on.
*)

PROCEDURE IsBackReferenceConditional (q: CARDINAL) : BOOLEAN ;
VAR
   i            : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   i := q ;
   WHILE i#0 DO
      GetQuad(i, op, op1, op2, op3) ;
      CASE op OF

      NewLocalVarOp,
      KillLocalVarOp,
      EndOp,
      StartOp,
      EndFileOp,
      StartDefFileOp,
      StartModFileOp:  RETURN( FALSE ) |       (* run into end of procedure or module *)

      GotoOp,
      IfEquOp,
      IfLessEquOp,
      IfGreEquOp,
      IfGreOp,
      IfLessOp,
      IfNotEquOp,
      IfInOp,
      IfNotInOp     :  IF (op3=q) AND IsConditional(q)
                       THEN
                          RETURN( TRUE )
                       END

      END ;
      i := GetNextQuad(i)
   END ;
   InternalError('fix this for the sake of efficiency..', __FILE__, __LINE__)
END IsBackReferenceConditional ;


(*
   IsCall - returns true if QuadNo is a call operation.
*)

PROCEDURE IsCall (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Quads[QuadNo] DO
      RETURN( Operator=CallOp )
   END
END IsCall ;


(*
   IsReturn - returns true if QuadNo is a return operation.
*)

PROCEDURE IsReturn (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Quads[QuadNo] DO
      RETURN( Operator=ReturnOp )
   END
END IsReturn ;


(* 
   IsNewLocalVar - returns true if QuadNo is a NewLocalVar operation.
*) 
 
PROCEDURE IsNewLocalVar (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Quads[QuadNo] DO
      RETURN( Operator=NewLocalVarOp )
   END
END IsNewLocalVar ;


(* 
   IsKillLocalVar - returns true if QuadNo is a KillLocalVar operation.
*) 
 
PROCEDURE IsKillLocalVar (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Quads[QuadNo] DO
      RETURN( Operator=KillLocalVarOp )
   END
END IsKillLocalVar ;


(*
   IsOptimizeOn - returns true if the Optimize flag was true at QuadNo.
*)

PROCEDURE IsOptimizeOn (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   n,
   q : CARDINAL ;
   On: BOOLEAN ;
BEGIN
   On := Optimizing ;
   q := Head ;
   WHILE (q#0) AND (q#QuadNo) DO
      WITH Quads[q] DO
         IF Operator=OptimizeOnOp
         THEN
            On := TRUE
         ELSIF Operator=OptimizeOffOp
         THEN
            On := FALSE
         END ;
         n := Next
      END ;
      q := n
   END ;
   RETURN( On )
END IsOptimizeOn ;


(*
   IsProfileOn - returns true if the Profile flag was true at QuadNo.
*)

PROCEDURE IsProfileOn (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   n,
   q : CARDINAL ;
   On: BOOLEAN ;
BEGIN
   On := Profiling ;
   q := Head ;
   WHILE (q#0) AND (q#QuadNo) DO
      WITH Quads[q] DO
         IF Operator=ProfileOnOp
         THEN
            On := TRUE
         ELSIF Operator=ProfileOffOp
         THEN
            On := FALSE
         END ;
         n := Next
      END ;
      q := n
   END ;
   RETURN( On )
END IsProfileOn ;


(*
   IsCodeOn - returns true if the Code flag was true at QuadNo.
*)

PROCEDURE IsCodeOn (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   n,
   q : CARDINAL ;
   On: BOOLEAN ;
BEGIN
   On := Coding ;
   q := Head ;
   WHILE (q#0) AND (q#QuadNo) DO
      WITH Quads[q] DO
         IF Operator=CodeOnOp
         THEN
            On := TRUE
         ELSIF Operator=CodeOffOp
         THEN
            On := FALSE
         END ;
         n := Next
      END ;
      q := n
   END ;
   RETURN( On )
END IsCodeOn ;


(*
   IsModFile - returns TRUE if QuadNo is a start of Module file
               directive.
*)

PROCEDURE IsModFile (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Quads[QuadNo] DO
      RETURN( Operator=StartModFileOp )
   END
END IsModFile ;


(*
   IsPseudoQuad - returns true if QuadNo is a compiler directive.
                  ie code, profile and optimize.
                     StartFile, EndFile,
*)

PROCEDURE IsPseudoQuad (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Quads[QuadNo] DO
      RETURN( (Operator=CodeOnOp) OR (Operator=CodeOffOp) OR
              (Operator=ProfileOnOp) OR (Operator=ProfileOffOp) OR
              (Operator=OptimizeOnOp) OR (Operator=OptimizeOffOp) OR
              (Operator=EndFileOp) OR (Operator=EndOp) OR
              (Operator=StartDefFileOp) OR (Operator=StartModFileOp)
            )
   END
END IsPseudoQuad ;


(*
   GetLastFileQuad - returns the Quadruple number of the last StartDefFile or
                     StartModFile quadruple.
*)

PROCEDURE GetLastFileQuad (QuadNo: CARDINAL) : CARDINAL ;
VAR
   Head,
   i,
   FileQuad: CARDINAL ;
BEGIN
   Head := 1 ;  (* Warning Head assummed to start at quadruple 1 *)
   FileQuad := 0 ;
   REPEAT
      WITH Quads[Head] DO
         IF (Operator=StartModFileOp) OR (Operator=StartDefFileOp)
         THEN
            FileQuad := Head
         END ;
         i := Next
      END ;
      Head := i
   UNTIL (i=QuadNo) OR (i=0) ;
   Assert(i#0) ;  (* Should never occur - if it does - check warning above *)
   Assert(FileQuad#0) ;  (* Should never occur *)
   RETURN( FileQuad )
END GetLastFileQuad ;


(*
   GetLastQuadNo - returns the last quadruple number referenced
                   by a GetQuad.
*)

PROCEDURE GetLastQuadNo () : CARDINAL ;
BEGIN
   RETURN( LastQuadNo )
END GetLastQuadNo ;


(*
   QuadToLineNo - Converts a QuadNo into the approprate line number of the
                  source file, the line number is returned.

                  This may be used to yield an idea where abouts in the
                  source file the code generetion is
                  processing.
*)

PROCEDURE QuadToLineNo (QuadNo: CARDINAL) : CARDINAL ;
BEGIN
   IF (LastQuadNo=0) AND (NOT IsNoPass()) AND (NOT IsPassCodeGeneration())
   THEN
      RETURN( 0 )
   ELSE
      RETURN( Quads[QuadNo].LineNo )
   END
END QuadToLineNo ;


(*
   QuadToTokenNo - Converts a QuadNo into the approprate token number of the
                   source file, the line number is returned.

                   This may be used to yield an idea where abouts in the
                   source file the code generetion is
                   processing.
*)

PROCEDURE QuadToTokenNo (QuadNo: CARDINAL) : CARDINAL ;
BEGIN
   IF (LastQuadNo=0) AND (NOT IsNoPass()) AND (NOT IsPassCodeGeneration())
   THEN
      RETURN( 0 )
   ELSE
      RETURN( Quads[QuadNo].TokenNo )
   END
END QuadToTokenNo ;


(*
   GetQuad - returns the Quadruple QuadNo.
*)

PROCEDURE GetQuad (QuadNo: CARDINAL;
                   VAR Op: QuadOperator;
                   VAR Oper1, Oper2, Oper3: CARDINAL) ;
BEGIN
   LastQuadNo := QuadNo ;
   WITH Quads[QuadNo] DO
      Op := Operator ;
      Oper1 := Operand1 ;
      Oper2 := Operand2 ;
      Oper3 := Operand3
   END
END GetQuad ;


(*
   AddQuadInformation - adds variable analysis and jump analysis to the new quadruple.
*)

PROCEDURE AddQuadInformation (QuadNo: CARDINAL;
                              Op: QuadOperator;
                              Oper1, Oper2, Oper3: CARDINAL) ;
BEGIN
   CASE Op OF

   IfInOp,
   IfNotInOp,
   IfEquOp,
   IfNotEquOp,
   IfLessOp,
   IfLessEquOp,
   IfGreOp,
   IfGreEquOp : ManipulateReference(QuadNo, Oper3) ;
                CheckAddVariableRead(Oper1, QuadNo) ;
                CheckAddVariableRead(Oper2, QuadNo) |

   GotoOp     : ManipulateReference(QuadNo, Oper3) |

   (* variable references *)

   InclOp,
   ExclOp            : CheckAddVariableRead(Oper1, QuadNo) ;
                       CheckAddVariableRead(Oper3, QuadNo) ;
                       CheckAddVariableWrite(Oper1, QuadNo) |
   UnboundedOp,
   HighOp,
   FunctValueOp,
   OffsetOp,
   NegateOp,
   BecomesOp,
   SizeOp            : CheckAddVariableWrite(Oper1, QuadNo) ;
                       CheckAddVariableRead(Oper3, QuadNo) |
   AddrOp            : CheckAddVariableWrite(Oper1, QuadNo) ;   (* Addr is peculiar as we might in the future read/write *)
                       CheckAddVariableRead(Oper3, QuadNo) ;
                       CheckAddVariableWrite(Oper3, QuadNo) |
   ReturnValueOp     : CheckAddVariableRead(Oper1, QuadNo) ;
                       CheckAddVariableRead(Oper3, QuadNo) |
   ReturnOp,
   CallOp,
   NewLocalVarOp,
   KillLocalVarOp    : CheckAddVariableRead(Oper3, QuadNo) |

   ParamOp           : CheckAddVariableRead(Oper2, QuadNo) ;
                       CheckAddVariableRead(Oper3, QuadNo) ;
                       IF (Oper1>0) AND (Oper1<=NoOfParam(Oper2)) AND
                          IsVarParam(Oper2, Oper1)
                       THEN
                          CheckAddVariableWrite(Oper3, QuadNo)    (* may also write to a var parameter *)
                       END |
   BaseOp,
   LogicalOrOp,
   LogicalAndOp,
   LogicalXorOp,
   CoerceOp,
   ConvertOp,
   AddOp,
   SubOp,
   MultOp,
   ModOp,
   DivOp             : CheckAddVariableWrite(Oper1, QuadNo) ;
                       CheckAddVariableRead(Oper2, QuadNo) ;
                       CheckAddVariableRead(Oper3, QuadNo) |

   XIndrOp           : CheckAddVariableRead(Oper1, QuadNo) ;
                       CheckAddVariableRead(Oper3, QuadNo) |

   IndrXOp           : CheckAddVariableWrite(Oper1, QuadNo) ;
                       CheckAddVariableRead(Oper3, QuadNo)

   ELSE
   END
END AddQuadInformation ;


PROCEDURE stop ; BEGIN END stop ;

(*
   PutQuad - overwrites a quadruple QuadNo with Op, Oper1, Oper2, Oper3
*)

PROCEDURE PutQuad (QuadNo: CARDINAL;
                   Op: QuadOperator;
                   Oper1, Oper2, Oper3: CARDINAL) ;
BEGIN
   IF QuadrupleGeneration
   THEN
      EraseQuad(QuadNo) ;
      AddQuadInformation(QuadNo, Op, Oper1, Oper2, Oper3) ;
      WITH Quads[QuadNo] DO
         Operator := Op ;
         Operand1 := Oper1 ;
         Operand2 := Oper2 ;
         Operand3 := Oper3 ;
      END
   END
END PutQuad ;


(*
   EraseQuad - erases a quadruple QuadNo, the quaduple is still in the list
               but wiped clean.
*)

PROCEDURE EraseQuad (QuadNo: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   WITH Quads[QuadNo] DO
      CASE Operator OF

      (* jumps, calls and branches *)
      IfInOp,
      IfNotInOp,
      IfEquOp,
      IfNotEquOp,
      IfLessOp,
      IfLessEquOp,
      IfGreOp,
      IfGreEquOp        : RemoveReference(QuadNo) ;
                          CheckRemoveVariableRead(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand2, QuadNo) |

      GotoOp            : RemoveReference(QuadNo) |

      (* variable references *)

      InclOp,
      ExclOp            : CheckRemoveVariableRead(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo) ;
                          CheckRemoveVariableWrite(Operand1, QuadNo) |

      UnboundedOp,
      HighOp,
      FunctValueOp,
      OffsetOp,
      NegateOp,
      BecomesOp,
      XIndrOp,
      IndrXOp,
      SizeOp            : CheckRemoveVariableWrite(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo) |
      AddrOp            : CheckRemoveVariableWrite(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo) ;
                          CheckRemoveVariableWrite(Operand3, QuadNo) |
      ReturnValueOp     : CheckRemoveVariableRead(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo) |
      ReturnOp,
      CallOp,
      NewLocalVarOp,
      KillLocalVarOp    : CheckRemoveVariableRead(Operand3, QuadNo) |
      ParamOp           : CheckRemoveVariableRead(Operand2, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo) ;
                          IF (Operand1>0) AND IsVarParam(Operand2, Operand1)
                          THEN
                             CheckRemoveVariableWrite(Operand3, QuadNo)    (* may also write to a var parameter *)
                          END |

      BaseOp,
      LogicalOrOp,
      LogicalAndOp,
      LogicalXorOp,
      CoerceOp,
      ConvertOp,
      AddOp,
      SubOp,
      MultOp,
      ModOp,
      DivOp             : CheckRemoveVariableWrite(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand2, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo)

      ELSE
      END ;
      Operator := DummyOp ;   (* finally blank it out *)
      Operand1 := 0 ;
      Operand2 := 0 ;
      Operand3 := 0
   END
END EraseQuad ;


(*
   CheckAddVariableRead - checks to see whether symbol, Sym, is a variable and
                          if so it then adds this quadruple to the variable list.
*)

PROCEDURE CheckAddVariableRead (Sym: CARDINAL; Quad: CARDINAL) ;
BEGIN
   IF IsVar(Sym)
   THEN
      PutVarReadQuad(Sym, Quad)
   END
END CheckAddVariableRead ;


(*
   CheckRemoveVariableRead - checks to see whether, Sym, is a variable and
                             if so then it removes the quadruple from the
                             variable list.
*)

PROCEDURE CheckRemoveVariableRead (Sym: CARDINAL; Quad: CARDINAL) ;
BEGIN
   IF IsVar(Sym)
   THEN
      RemoveVarReadQuad(Sym, Quad)
   END
END CheckRemoveVariableRead ;


(*
   CheckAddVariableWrite - checks to see whether symbol, Sym, is a variable and
                           if so it then adds this quadruple to the variable list.
*)

PROCEDURE CheckAddVariableWrite (Sym: CARDINAL; Quad: CARDINAL) ;
BEGIN
   IF IsVar(Sym)
   THEN
      PutVarWriteQuad(Sym, Quad)
   END
END CheckAddVariableWrite ;


(*
   CheckRemoveVariableWrite - checks to see whether, Sym, is a variable and
                              if so then it removes the quadruple from the
                              variable list.
*)

PROCEDURE CheckRemoveVariableWrite (Sym: CARDINAL; Quad: CARDINAL) ;
BEGIN
   IF IsVar(Sym)
   THEN
      RemoveVarWriteQuad(Sym, Quad)
   END
END CheckRemoveVariableWrite ;


(*
   GetNextQuad - returns the Quadruple number following QuadNo.
*)

PROCEDURE GetNextQuad (QuadNo: CARDINAL) : CARDINAL ;
BEGIN
   QuadNo := Quads[QuadNo].Next ;
   WHILE (QuadNo#0) AND (Quads[QuadNo].Operator=DummyOp) DO
      QuadNo := Quads[QuadNo].Next
   END ;
   RETURN( QuadNo )
END GetNextQuad ;


(*
   AddQuad - adds a quadruple in between left and right then
             returns the new quad number.
*)

PROCEDURE AddQuad (VAR Left, Right: CARDINAL;
                   Op: QuadOperator;
                   Oper1, Oper2, Oper3: CARDINAL) : CARDINAL ;
VAR
   q: CARDINAL ;
BEGIN
   NewQuad(q) ;
   Assert(Quads[Left].Next=Right) ;
   Quads[Left].Next := q ;
   WITH Quads[q] DO
      Operator := Op ;
      Operand1 := Oper1 ;
      Operand2 := Oper2 ;
      Operand3 := Oper3 ;
      Next := Right
   END ;
   RETURN( q )
END AddQuad ;


(*
   SubQuad - subtracts a quadruple QuadNo from a list Head.
*)

PROCEDURE SubQuad (VAR Head: CARDINAL; QuadNo: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   AlterReference(Head, QuadNo, Quads[QuadNo].Next) ;
   WITH Quads[QuadNo] DO
      CASE Operator OF

      (* jumps, calls and branches *)
      IfInOp,
      IfNotInOp,
      IfEquOp,
      IfNotEquOp,
      IfLessOp,
      IfLessEquOp,
      IfGreOp,
      IfGreEquOp        : RemoveReference(QuadNo) ;
                          CheckRemoveVariableRead(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand2, QuadNo) |

      GotoOp            : RemoveReference(QuadNo) |

      (* variable references *)

      InclOp,
      ExclOp            : CheckRemoveVariableRead(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo) ;
                          CheckRemoveVariableWrite(Operand1, QuadNo) |
      UnboundedOp,
      HighOp,
      FunctValueOp,
      OffsetOp,
      NegateOp,
      BecomesOp,
      SizeOp            : CheckRemoveVariableWrite(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo) |
      AddrOp            : CheckRemoveVariableWrite(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo) ;
                          CheckRemoveVariableWrite(Operand3, QuadNo) |
      ReturnValueOp     : CheckRemoveVariableRead(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo) |
      ReturnOp,
      CallOp,
      NewLocalVarOp,
      KillLocalVarOp    : CheckRemoveVariableRead(Operand3, QuadNo) |
      ParamOp           : CheckRemoveVariableRead(Operand2, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo) ;
                          IF (Operand1>0) AND IsVarParam(Operand2, Operand1)
                          THEN
                             CheckRemoveVariableWrite(Operand3, QuadNo)    (* may also write to a var parameter *)
                          END |
      BaseOp,
      LogicalOrOp,
      LogicalAndOp,
      LogicalXorOp,
      CoerceOp,
      ConvertOp,
      AddOp,
      SubOp,
      MultOp,
      ModOp,
      DivOp             : CheckRemoveVariableWrite(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand2, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo) |

      XIndrOp           : CheckRemoveVariableRead(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo) |

      IndrXOp           : CheckRemoveVariableWrite(Operand1, QuadNo) ;
                          CheckRemoveVariableRead(Operand3, QuadNo)

      ELSE
      END
   END ;
   IF Head=QuadNo
   THEN
      Head := Quads[QuadNo].Next
   ELSE
      i := Head ;
      WHILE Quads[i].Next#QuadNo DO
         i := Quads[i].Next
      END ;
      Quads[i].Next := Quads[QuadNo].Next
   END ;
   Quads[QuadNo].Operator := DummyOp
END SubQuad ;


(*
   GetRealQuad - returns the Quadruple number of the real quadruple
                 at QuadNo or beyond.
*)

PROCEDURE GetRealQuad (QuadNo: CARDINAL) : CARDINAL ;
BEGIN
   WHILE QuadNo#0 DO
      WITH Quads[QuadNo] DO
         IF Operator=EndOp
         THEN
            (*
               EndOp is not a real quadruple - but it marks the end
               of the quadruple list. So we stop here.
            *)
            RETURN( QuadNo )
         ELSIF (NOT IsPseudoQuad(QuadNo)) AND
            (Operator#DummyOp) AND (Operator#LineNumberOp)
         THEN
            RETURN( QuadNo )
         END
      END ;
      INC(QuadNo)
   END ;
   RETURN( 0 )
END GetRealQuad ;


(*
   AlterReference - alters all references from OldQuad, to NewQuad in a
                    quadruple list Head.
*)

PROCEDURE AlterReference (Head, OldQuad, NewQuad: CARDINAL) ;
VAR
   OldOperand3,
   i          : CARDINAL ;
BEGIN
   WHILE (Quads[OldQuad].NoOfTimesReferenced>0) AND (Head#0) DO
      WITH Quads[Head] DO
         CASE Operator OF

         IfInOp,
         IfNotInOp,
         IfEquOp,
         IfNotEquOp,
         IfLessOp,
         IfLessEquOp,
         IfGreOp,
         IfGreEquOp,
         GotoOp     : IF Operand3=OldQuad
                      THEN
                         ManipulateReference(Head, NewQuad)
                      END

         ELSE
         END ;
         i := Next
      END ;
      Head := i
   END
END AlterReference ;


(*
   GrowQuads - grows the list of quadruples to the quadruple, to.
*)

PROCEDURE GrowQuads (to: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   IF (to#0) AND (to>GrowInitialization)
   THEN
      i := GrowInitialization+1 ;
      WHILE i<=to DO
         Quads[i].NoOfTimesReferenced := 0 ;
         INC(i)
      END ;
      GrowInitialization := to
   END
END GrowQuads ;


(*
   ManipulateReference - manipulates the quadruple, q, so that it now points to quad, to.
*)

PROCEDURE ManipulateReference (q: CARDINAL; to: CARDINAL) ;
BEGIN
   Assert((GrowInitialization>=q) OR (to=0)) ;
   GrowQuads(to) ;
   RemoveReference(q) ;
   WITH Quads[q] DO
      Operand3 := to ;
      IF to#0
      THEN
         INC(Quads[to].NoOfTimesReferenced)
      END
   END
END ManipulateReference ;
   

(*
   RemoveReference - remove the reference by quadruple, q, to wherever
                     it was pointing to.
*)

PROCEDURE RemoveReference (q: CARDINAL) ;
BEGIN
   WITH Quads[q] DO
      IF (Operand3#0) AND (Operand3<NextQuad)
      THEN
         Assert(Quads[Operand3].NoOfTimesReferenced#0) ;
         DEC(Quads[Operand3].NoOfTimesReferenced)
      END
   END
END RemoveReference ;
   
   
(*
   CountQuads - returns the number of quadruple contained within the list Head.
*)

PROCEDURE CountQuads (Head: CARDINAL) : CARDINAL ;
VAR
   n: CARDINAL ;
BEGIN
   n := 0 ;
   WHILE Head#0 DO
      IF Quads[Head].Operator#DummyOp
      THEN
         INC(n)
      END ;
      Head := Quads[Head].Next
   END ;
   RETURN( n )
END CountQuads ;


(*
   KillQuad - destroys a quadruple.
*)

PROCEDURE KillQuad (QuadNo: CARDINAL) ;
BEGIN
   DisposeQuad(QuadNo)
END KillQuad ;


(*
   NewQuad - sets QuadNo to a new quadruple.
*)

PROCEDURE NewQuad (VAR QuadNo: CARDINAL) ;
BEGIN
   IF FreeList=MaxQuad
   THEN
      InternalError('no more quads available, increase MaxQuad', __FILE__, __LINE__)
   ELSE
      QuadNo := FreeList ;
      WITH Quads[QuadNo] DO
         (* must not set NoOfTimesReferenced := 0 ; as we might overwrite it *)
         Operator := DummyOp ;
         Operand3 := 0 ;
         Next := 0
      END ;
      INC(FreeList) ;
      IF GrowInitialization<FreeList
      THEN
         GrowInitialization := FreeList
      END
   END
END NewQuad ;


(*
   DisposeQuad - returns QuadNo to the FreeList of quadruples.
*)

PROCEDURE DisposeQuad (QuadNo: CARDINAL) ;
BEGIN
   InternalError('not expecting DisposeQuad to be called', __FILE__, __LINE__) ;
   Quads[QuadNo].Next := FreeList ;
   FreeList := QuadNo
END DisposeQuad ;


(*
   StartBuildDefFile - generates a StartFileDefOp quadruple indicating the file
                       that has produced the subsequent quadruples.
                       The code generator uses the StartDefFileOp quadruples
                       to relate any error to the appropriate file.


                       Entry                   Exit
                       =====                   ====


                Ptr ->                                        <- Ptr
                       +------------+          +------------+
                       | ModuleName |          | ModuleName | 
                       |------------|          |------------|


                       Quadruples Produced

                       q     StartDefFileOp  _  _  ModuleSym
*)

PROCEDURE StartBuildDefFile ;
VAR
   ModuleName: Name ;
BEGIN
   PopT(ModuleName) ;
   PushT(ModuleName) ;
   GenQuad(StartDefFileOp, GetPreviousTokenLineNo(), NulSym, GetModule(ModuleName))
END StartBuildDefFile ;


(*
   StartBuildModFile - generates a StartModFileOp quadruple indicating the file
                       that has produced the subsequent quadruples.
                       The code generator uses the StartModFileOp quadruples
                       to relate any error to the appropriate file.


                       Entry                   Exit
                       =====                   ====


                Ptr ->                                        <- Ptr
                       +------------+          +------------+
                       | ModuleName |          | ModuleName | 
                       |------------|          |------------|


                       Quadruples Produced

                       q     StartModFileOp  lineno  filename  ModuleSym
*)

PROCEDURE StartBuildModFile ;
BEGIN
   GenQuad(StartModFileOp, GetPreviousTokenLineNo(),
           WORD(makekey(string(GetFileName()))), GetFileModule())
END StartBuildModFile ;


(*
   EndBuildFile - generates an EndFileOp quadruple indicating the file
                  that has produced the previous quadruples has ended.

                  Entry                   Exit
                  =====                   ====


           Ptr ->                                        <- Ptr
                  +------------+          +------------+
                  | ModuleName |          | ModuleName | 
                  |------------|          |------------|


                  Quadruples Produced

                  q     EndFileOp  _  _  ModuleSym
*)

PROCEDURE EndBuildFile ;
VAR
   ModuleName: Name ;
BEGIN
   PopT(ModuleName) ;
   PushT(ModuleName) ;
   GenQuad(EndFileOp, NulSym, NulSym, GetModule(ModuleName))
END EndBuildFile ;


(*
   StartBuildInit - Sets the start of initialization code of the
                    current module to the next quadruple.
*)
 
PROCEDURE StartBuildInit ;
VAR
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(name) ;
   ModuleSym := GetModule(name) ;
   Assert(IsModule(ModuleSym) OR IsDefImp(ModuleSym)) ;
   PutModuleStartQuad(ModuleSym, NextQuad) ;
   GenQuad(StartOp, GetPreviousTokenLineNo(), GetFileModule(), ModuleSym) ;
   PushT(name)
END StartBuildInit ;
 
 
(*
   EndBuildInit - Sets the end initialization code of a module.
*)
 
PROCEDURE EndBuildInit ;
BEGIN
   PutModuleEndQuad(GetCurrentModule(), NextQuad) ;
   CheckVariablesInBlock(GetCurrentModule()) ;
   GenQuad(EndOp, GetPreviousTokenLineNo(), GetFileModule(), GetCurrentModule())
END EndBuildInit ;


(*
   StartBuildInnerInit - Sets the start of initialization code of the
                         inner module to the next quadruple.
*)
 
PROCEDURE StartBuildInnerInit ;
BEGIN
   PutModuleStartQuad(GetCurrentModule(), NextQuad) ;
   GenQuad(StartOp, GetPreviousTokenLineNo(), NulSym, GetCurrentModule())
END StartBuildInnerInit ;
 
 
(*
   EndBuildInnerInit - Sets the end initialization code of a module.
*)
 
PROCEDURE EndBuildInnerInit ;
BEGIN
   PutModuleEndQuad(GetCurrentModule(), NextQuad) ;
   CheckVariablesInBlock(GetCurrentModule()) ;
   GenQuad(EndOp, GetPreviousTokenLineNo(), NulSym, GetCurrentModule())
END EndBuildInnerInit ;


(*
   ForLoopAnalysis - checks all the FOR loops for index variable manipulation
                     and dangerous usage outside the loop.
*)

PROCEDURE ForLoopAnalysis ;
VAR
   i, n: CARDINAL ;
BEGIN
   IF Pedantic
   THEN
      WITH ForInfo DO
         n := NoOfItemsInList(IncrementQuad) ;
         i := 1 ;
         WHILE i<=n DO
            CheckForIndex(GetItemFromList(StartOfForLoop, i),
                          GetItemFromList(EndOfForLoop, i),
                          GetItemFromList(IncrementQuad, i),
                          GetItemFromList(ForLoopIndex, i)) ;
            INC(i)
         END
      END
   END
END ForLoopAnalysis ;


(*
   AddForInfo - adds the description of the FOR loop into the record list.
                This is used if -pedantic is turned on to check index variable
                usage.
*)

PROCEDURE AddForInfo (Start, End, IncQuad: CARDINAL; Sym: CARDINAL) ;
BEGIN
   IF Pedantic
   THEN
      WITH ForInfo DO
         PutItemIntoList(IncrementQuad, IncQuad) ;
         PutItemIntoList(StartOfForLoop, Start) ;
         PutItemIntoList(EndOfForLoop, End) ;
         PutItemIntoList(ForLoopIndex, Sym)
      END
   END
END AddForInfo ;


(*
   CheckForIndex - checks the quadruples: Start..End to see whether a
                   for loop index is manipulated by the programmer.
                   It generates a warning if this is the case.
                   It also checks to see whether the IndexSym is read
                   immediately outside the loop in which case a warning
                   is issued.
*)

PROCEDURE CheckForIndex (Start, End, Omit: CARDINAL; IndexSym: CARDINAL) ;
VAR
   ReadStart, ReadEnd,
   WriteStart, WriteEnd: CARDINAL ;
BEGIN
   GetVarWriteLimitQuads(IndexSym, Start, End, WriteStart, WriteEnd) ;
   IF (WriteStart<Omit) AND (WriteStart>Start)
   THEN
      WarnStringAt(Sprintf1(Mark(InitString('FOR loop index variable (%s) is being manipulated inside the loop, this is considered bad practice and may cause unknown program behaviour')),
                            Mark(InitStringCharStar(KeyToCharStar(GetSymName(IndexSym))))),
                   QuadToTokenNo(WriteStart))
   END ;
   GetVarWriteLimitQuads(IndexSym, End, 0, WriteStart, WriteEnd) ;
   GetVarReadLimitQuads(IndexSym, End, 0, ReadStart, ReadEnd) ;
   IF (ReadStart#0) AND ((ReadStart<WriteStart) OR (WriteStart=0))
   THEN
      WarnStringAt(Sprintf1(Mark(InitString('FOR loop index variable (%s) is being read outside the FOR loop (without being reset first), this is considered extremely bad practice and may cause unknown program behaviour')),
                            Mark(InitStringCharStar(KeyToCharStar(GetSymName(IndexSym))))),
                   QuadToTokenNo(ReadStart))
   END
END CheckForIndex ;


(*
   CheckSubrange - providing that the user has requested -bounds and GetType(Des) is a subrange
                   then this function emits quadruples to check that Exp lies in this subrange.
*)

PROCEDURE CheckSubrange (Des, Exp: CARDINAL) ;
VAR
   num      : String ;
   low, high,
   t, f,
   type     : CARDINAL ;
   old      : BOOLEAN ;
BEGIN
   IF BoundsChecking AND (NOT IsConst(Des)) AND (NOT MustNotCheckBounds)
   THEN
      old := MustNotCheckBounds ;
      MustNotCheckBounds := TRUE ;  (* stop recursive checking *)
      type := GetType(Des) ;
      IF IsSubrange(type)
      THEN
         (*
            we cannot yet GetSubrange for low and high as these entities
            may not yet have been evaluated. P3SymBuild evaluates them,
            possibly after this procedure.

            GetSubrange(type, high, low) ;  (* high, low may not be defined yet *)

            To work around this problem we use the SubrangeLowOp and SubrangeHighOp
            which we substitute for BecomesOp once we have resolved the subrange types
            and the limits are known.
         *)
         low := MakeTemporary(ImmediateValue) ;
         PutVar(low, type) ;
         high := MakeTemporary(ImmediateValue) ;
         PutVar(high, type) ;
         GenQuad(SubrangeLowOp , low, NulSym, type) ;
         GenQuad(SubrangeHighOp, high, NulSym, type) ;
         (* q+0  if exp < low        q+4  *)
         (* q+1  goto                q+2  *)
         PushT(Exp) ;        (* BuildRelOp   1st parameter *)
         PushT(LessTok) ;    (*              2nd parameter *)
         PushT(low) ;        (*              3rd parameter *)
         BuildRelOp ;
         PopBool(t, f) ;     (*              return value  *)
         BackPatch(f, NextQuad) ;
         (* q+2  if exp > high       q+4  *)
         (* q+3  goto                q+   *)
         PushT(Exp) ;        (* BuildRelOp   1st parameter *)
         PushT(GreaterTok) ; (*              2nd parameter *)
         PushT(high) ;       (*              3rd parameter *)
         BuildRelOp ;
         BackPatch(t, NextQuad) ;
         PopBool(t, f) ;
         BackPatch(t, NextQuad) ;
         (* now call the error routine *)
         IF IsSubscript(Des)
         THEN
            (* array index is out of bounds *)
            (* assignment to a subrange variable is out of bounds *)
            PushTF(RequestSym(MakeKey('ArraySubscriptError')),
                   GetType(RequestSym(MakeKey('ArraySubscriptError'))))
         ELSE
            (* assignment to a subrange variable is out of bounds *)
            PushTF(RequestSym(MakeKey('SubrangeAssignmentError')),
                   GetType(RequestSym(MakeKey('SubrangeAssignmentError'))))
         END ;
         PushT(MakeConstLitString(makekey(string(GetFileName())))) ;
         num := Sprintf1(Mark(InitString('%d')), GetLineNo()) ;
         PushT(MakeConstLit(makekey(string(num)))) ;
         num := KillString(num) ;
         PushT(2) ;
         BuildProcedureCall ;
         (* call error function           *)
         BackPatch(f, NextQuad) ;   (* no range error *)
      END ;
      MustNotCheckBounds := old   (* stop recursive checking *)
   END
END CheckSubrange ;


(*
   CheckDynamicArray - providing that the user has requested -bounds then this function
                       emits quadruples to check that Exp is legal: 0..HIGH(Sym).
                       We assume that Sym is an unbounded array.
*)

PROCEDURE CheckDynamicArray (Sym, Exp: CARDINAL) ;
VAR
   num      : String ;
   t, f,
   high     : CARDINAL ;
   old      : BOOLEAN ;
BEGIN
   IF BoundsChecking AND (NOT MustNotCheckBounds)
   THEN
      old := MustNotCheckBounds ;
      MustNotCheckBounds := TRUE ;  (* stop recursive checking *)
      (* q+0  if exp < low        q+4  *)
      (* q+1  goto                q+2  *)
      PushT(Exp) ;            (* BuildRelOp   1st parameter *)
      PushT(LessTok) ;        (*              2nd parameter *)
      PushT(MakeConstLit(MakeKey('0'))) ;  (* 3rd parameter *)
      BuildRelOp ;
      PopBool(t, f) ;     (*              return value  *)
      BackPatch(f, NextQuad) ;

      PushTF(High, Cardinal) ;             (* High function *)
      PushT(Sym) ;                       (* unbounded array *)
      PushT(1) ;                     (* 1 parameter to HIGH *)
      BuildFunctionCall ;
      PopT(high) ;

      (* q+2  if exp > high       q+4  *)
      (* q+3  goto                q+   *)
      PushT(Exp) ;           (* BuildRelOp   1st parameter *)
      PushT(GreaterTok) ;    (*              2nd parameter *)
      PushT(high) ;          (*              3rd parameter *)
      BuildRelOp ;
      BackPatch(t, NextQuad) ;
      PopBool(t, f) ;
      BackPatch(t, NextQuad) ;

      (* now call the error routine as the array index is out of bounds *)
      PushTF(RequestSym(MakeKey('ArraySubscriptError')),
             GetType(RequestSym(MakeKey('ArraySubscriptError')))) ;
      PushT(MakeConstLitString(makekey(string(GetFileName())))) ;
      num := Sprintf1(Mark(InitString('%d')), GetLineNo()) ;
      PushT(MakeConstLit(makekey(string(num)))) ;
      num := KillString(num) ;
      PushT(2) ;
      BuildProcedureCall ;
      (* call error function           *)
      BackPatch(f, NextQuad) ;   (* no range error *)
      MustNotCheckBounds := old   (* stop recursive checking *)
   END
END CheckDynamicArray ;


(*
   CollectLow - returns the low of the subrange value.
*)

PROCEDURE CollectLow (sym: CARDINAL) : CARDINAL ;
VAR
   low, high: CARDINAL ;
BEGIN
   IF IsSubrange(sym)
   THEN
      GetSubrange(sym, high, low) ;
      RETURN( low )
   ELSE
      InternalError('expecting Subrange symbol', __FILE__, __LINE__)
   END
END CollectLow ;


(*
   CollectHigh - returns the high of the subrange value, sym.
*)

PROCEDURE CollectHigh (sym: CARDINAL) : CARDINAL ;
VAR
   low, high: CARDINAL ;
BEGIN
   IF IsSubrange(sym)
   THEN
      GetSubrange(sym, high, low) ;
      RETURN( high )
   ELSE
      InternalError('expecting Subrange symbol', __FILE__, __LINE__)
   END
END CollectHigh ;


(*
   BackPatchSubranges - runs through all the quadruples and finds SubrangeLow or SubrangeHigh
                        quadruples and replaces it by an assignment to the Low or High component
                        of the subrange type.

                        Input:

                        SubrangeLow    op1     op3         (* op3 is a subrange *)

                        Output:

                        Becomes        op1     low

                        Input:

                        SubrangeHigh   op1     op3         (* op3 is a subrange *)

                        Output:

                        Becomes        op1     high

*)

PROCEDURE BackPatchSubranges (Head: CARDINAL) ;
BEGIN
   IF Head#0
   THEN
      REPEAT
         WITH Quads[Head] DO
            CASE Operator OF

            SubrangeLowOp :  Operand3 := CollectLow(Operand3) ;
                             Operator := BecomesOp |
            SubrangeHighOp:  Operand3 := CollectHigh(Operand3) ;
                             Operator := BecomesOp

            ELSE
            END ;
            Head := Next
         END
      UNTIL Head=0
   END
END BackPatchSubranges ;


(*
   CheckCompatibleWithBecomes - checks to see that symbol, sym, is
                                compatible with the := operator.
*)

PROCEDURE CheckCompatibleWithBecomes (sym: CARDINAL) ;
BEGIN
   IF IsType(sym) OR IsProcedure(sym)
   THEN
      WriteFormat1('illegal designator (%a) it must be a variable', GetSymName(sym))
   END
END CheckCompatibleWithBecomes ;


(*
   BuildAssignmentWithoutBounds - calls BuildAssignment but makes sure we do not
                                  check bounds.
*)

PROCEDURE BuildAssignmentWithoutBounds ;
VAR
   old: BOOLEAN ;
BEGIN
   old := MustNotCheckBounds ;
   MustNotCheckBounds := TRUE ;
   BuildAssignment ;
   MustNotCheckBounds := old
END BuildAssignmentWithoutBounds ;


(*
   MoveWithMode - 
*)

PROCEDURE MoveWithMode (Des, Exp: CARDINAL) ;
VAR
   t: CARDINAL ;
BEGIN
   IF IsConstString(Exp) AND IsConst(Des)
   THEN
      GenQuad(BecomesOp, Des, NulSym, Exp) ;
      PutConstString(Des, GetString(Exp))
   ELSE
      IF GetMode(Des)=RightValue
      THEN
         IF GetMode(Exp)=LeftValue
         THEN
            GenQuad(IndrXOp, Des, GetType(Des), Exp)
         ELSE
            GenQuad(BecomesOp, Des, NulSym, Exp)
         END
      ELSIF GetMode(Des)=LeftValue
      THEN
         IF GetMode(Exp)=LeftValue
         THEN
            t := MakeTemporary(RightValue) ;
            PutVar(t, GetType(Des)) ;
            GenQuad(IndrXOp, t, GetType(Des), Exp) ;
            GenQuad(XIndrOp, Des, GetType(Des), t)
         ELSE
            GenQuad(XIndrOp, Des, GetType(Des), Exp)
         END
      ELSE
         GenQuad(BecomesOp, Des, NulSym, Exp)
      END
   END
END MoveWithMode ;


(*
   BuildBuiltinConst - makes reference to a builtin constant within gm2.

                              Entry                 Exit

                       Ptr ->
                              +------------+        +------------+
                              | Ident      |        | Sym        |
                              |------------|        |------------|

                       Quadruple produced:

                       q    Sym  BuiltinConstOp  Ident
*)

PROCEDURE BuildBuiltinConst ;
VAR
   Id : CARDINAL ;
   Sym: CARDINAL ;
BEGIN
   PopT(Id) ;
   Sym := MakeTemporary(ImmediateValue) ;
   PutVar(Sym, Integer) ;
(*
   CASE GetBuiltinConstType(KeyToCharStar(Name(Id))) OF

   0:  ErrorFormat1(NewError(GetTokenNo()),
                    '%a unrecognised builtin constant', Id) |
   1:  PutVar(Sym, Integer) |
   2:  PutVar(Sym, Real)

   ELSE
      InternalError('unrecognised value', __FILE__, __LINE__)
   END ;
*)
   GenQuad(BuiltinConstOp, Sym, NulSym, Id) ;
   PushT(Sym)
END BuildBuiltinConst ;


(*
   BuildAssignment - Builds an assignment from the values given on the
                     quad stack. Either an assignment to an
                     arithmetic expression or an assignment to a
                     boolean expression.
                     The Stack is expected to contain:


       Either

                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | Expression |
                     |------------|
                     | Designator |
                     |------------|          +------------+
                     |            |          |            |  <- Ptr
                     |------------|          |------------|


                     Quadruples Produced depend of GetMode of Designator and Expression


                     Designator = RightValue         Expression = RightValue
                     q     BecomesOp  Designator  _  Expression

                     Designator = RightValue         Expression = LeftValue
                     q     IndrX      Designator     Expression

                     Designator = LeftValue          Expression = RightValue
                     q     XIndr      Designator     Expression

                     Designator = LeftValue          Expression = LeftValue
                     q     IndrX      t              Expression
                     q+1   XIndr      Designator     t


       OR

                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | True |False|
                     |------------|
                     | Designator |
                     |------------|          +------------+
                     |            |          |            |  <- Ptr
                     |------------|          |------------|


                     Quadruples Produced depend of GetMode of Designator

                     Designator = RightValue
                     q     BecomesOp  Designator  _  TRUE
                     q+1   GotoOp                    q+3
                     q+2   BecomesOp  Designator  _  FALSE


                     Designator = LeftValue
                     q     XIndr      Designator  _  TRUE
                     q+1   GotoOp                    q+3
                     q+2   XIndr      Designator  _  FALSE
*)

PROCEDURE BuildAssignment ;
VAR
   t, f,
   Des, Exp: CARDINAL ;
BEGIN
   DumpStack ;
   IF IsBoolean(1)
   THEN
      PopBool(t, f) ;
      PopT(Des) ;
      (* Conditional Boolean Assignment *)
      BackPatch(t, NextQuad) ;
      IF GetMode(Des)=RightValue
      THEN
         GenQuad(BecomesOp, Des, NulSym, True)
      ELSE
         GenQuad(XIndrOp, Des, Boolean, True)
      END ;
      GenQuad(GotoOp, NulSym, NulSym, NextQuad+2) ;
      BackPatch(f, NextQuad) ;
      IF GetMode(Des)=RightValue
      THEN
         GenQuad(BecomesOp, Des, NulSym, False)
      ELSE
         GenQuad(XIndrOp, Des, Boolean, False)
      END
   ELSE
      PopT(Exp) ;
      PopT(Des) ;
      CheckCompatibleWithBecomes(Des) ;
      CheckSubrange(Des, Exp) ;
      (* Traditional Assignment *)
      MoveWithMode(Des, Exp) ;
      (*
         Ugly hack - to determine whether an assignment is a CONST
                     string := 'asdkjhasd' ;
                     we test Exp here and set Des to ConstStringSym.
                     This test should be done during Pass 2 and it
                     should allow CONSTs to be declared after usage.
      *)
      CheckAssignCompatible(Des, Exp)
   END
 ; DumpStack ;
END BuildAssignment ;


(*
   CheckAssignCompatible - checks to see that an assignment is compatible.
*)

PROCEDURE CheckAssignCompatible (Des, Exp: CARDINAL) ;
VAR
   DesT, ExpT, DesL: CARDINAL ;
BEGIN
   DesT := GetType(Des) ;
   ExpT := GetType(Exp) ;
   DesL := GetLowestType(Des) ;
   IF IsProcedure(Exp) AND
      ((DesT#NulSym) AND (NOT IsProcType(DesT))) AND
      ((DesL#NulSym) AND (NOT IsProcType(DesL)))
   THEN
      WriteFormat1('incorrectly assigning a procedure to a variable %a (variable is not a procedure type)', GetSymName(Des))
   ELSIF IsProcedure(Exp) AND IsProcedureNested(Exp)
   THEN
      WriteFormat1('cannot call nested procedure, %a, indirectly as the outer scope will not be known',
                        GetSymName(Exp))
   ELSIF IsConstString(Exp)
   THEN
   ELSIF (DesT#NulSym) AND (IsUnbounded(DesT))
   THEN
   ELSIF (ExpT#NulSym) AND (IsUnbounded(ExpT))
   THEN
   ELSIF (DesL#NulSym) AND IsArray(DesL)
   THEN
   ELSIF IsConst(Exp) AND (ExpT#Address) AND (NOT IsConst(Des)) AND
         (DesL#NulSym) AND ((DesL=Cardinal) OR (NOT IsSubrange(DesL))) AND
         (NOT IsEnumeration(DesL))
   THEN
      IF (IsBaseType(DesL) OR IsSystemType(DesL))
      THEN
         CheckAssignmentCompatible(ExpT, DesT)
      ELSE
         WriteFormat1('assignment of a constant (%a) can only be made to a variable which has a Modula-2 reserved type as its base',
                      GetSymName(Des))
      END
   ELSIF (DesT#NulSym) AND IsSet(DesT) AND IsConst(Exp)
   THEN
      (*
         unfortunately we cannot check types of these items yet as the Exp will be
         of type CARDINAL and DesT is a set. Exp may be generated by M2Quads.
         If we give it a set type (PutVar(Exp, DesT) I believe it will cause
         code gen problems as it looks at the type to decide whether OR or + (eg).

         So for the moment we ignore checking.
      *)
   ELSE
      IF (DesT#NulSym) AND IsProcType(DesT) AND IsProcedure(Exp)
      THEN
         DesT := GetType(DesT)  (* we can at least check RETURN values of procedure variables *)
      END ;
      (* subranges must have TSIZE of between TSIZE(CHAR)..TSIZE(CARDINAL) *)

      (* Integer and Cardinal constants have special case as long as they are assigned to subranges
         of Integer and Cardinal, the problem is that we cannot tell yet what the type of the subrange is...
         This is an oversight which should be sorted out asap.. for now any constant integer or cardinal
         assignment to a subrange is legal..

         FIXME
      *)

      CheckAssignmentCompatible(ExpT, DesT)
   END
END CheckAssignCompatible ;


(*
   CheckBooleanId - Checks to see if the top operand is a boolean.
                    If the operand is not a boolean then it is tested
                    with true and a boolean is generated.
                    The Stack:


                    Entry                     Exit
             Ptr ->                                          <- Ptr
                    +------------+            +------------+
                    | Sym        |            | t   | f    |
                    |------------|            |------------|

                        Quadruples

                        q   If=      Sym   True   _
                        q+1 GotoOp   _     _      _
*)

PROCEDURE CheckBooleanId ;
BEGIN
   IF NOT IsBoolean(1)
   THEN
      IF IsVar(OperandT(1))
      THEN
         IF GetType(OperandT(1))#Boolean
         THEN
            WriteFormat1('symbol %a is not a boolean and boolean expression is expected', GetSymName(OperandT(1)))
         END
      END ;
      PushT(EqualTok) ;
      PushT(True) ;
      BuildRelOp
   END
END CheckBooleanId ;


(*
   BuildRepeat - Builds the repeat statement from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====


                 Empty
                                                        <- Ptr
                                         +------------+
                                         | RepeatQuad |
                                         |------------|

*)

PROCEDURE BuildRepeat ;
BEGIN
   PushT(NextQuad) ;
   PushExit(0)
END BuildRepeat ;


(*
   BuildUntil - Builds the until part of the repeat statement
                from the quad stack.
                The Stack is expected to contain:


                Entry                   Exit
                =====                   ====

        Ptr ->
                +------------+
                | t   | f    |
                |------------|
                | RepeatQuad |          Empty
                |------------|
*)

PROCEDURE BuildUntil ;
VAR
   t, f,
   Repeat: CARDINAL ;
BEGIN
   CheckBooleanId ;
   PopBool(t, f) ;
   PopT(Repeat) ;
   BackPatch(f, Repeat) ;          (* If False then keep on repeating *)
   BackPatch(t, NextQuad) ;        (* If True then exit repeat        *)
   BackPatch(PopExit(), NextQuad)  (* BackPatch any EXIT statements   *)
END BuildUntil ;


(*
   BuildWhile - Builds the While part of the While statement
                from the quad stack.
                The Stack is expected to contain:


                Entry                   Exit
                =====                   ====

                                                       <- Ptr
                                        |------------|
                Empty                   | WhileQuad  |
                                        |------------|
*)

PROCEDURE BuildWhile ;
BEGIN
   PushT(NextQuad) ;
   PushExit(0)           (* EXIT statement link *)
END BuildWhile ;


(*
   BuildDoWhile - Builds the Do part of the while statement
                  from the quad stack.
                  The Stack is expected to contain:


                  Entry                   Exit
                  =====                   ====

          Ptr ->
                  +------------+          +------------+
                  | t   | f    |          | 0    | f   |
                  |------------|          |------------|
                  | WhileQuad  |          | WhileQuad  |
                  |------------|          |------------|

                  Quadruples

                  BackPatch t exit to the NextQuad
*)

PROCEDURE BuildDoWhile ;
VAR
   t, f: CARDINAL ;
BEGIN
   CheckBooleanId ;
   PopBool(t, f) ;
   BackPatch(t, NextQuad) ;
   PushBool(0, f)
END BuildDoWhile ;


(*
   BuildEndWhile - Builds the end part of the while statement
                   from the quad stack.
                   The Stack is expected to contain:


                   Entry                   Exit
                   =====                   ====

           Ptr ->
                   +------------+
                   | t   | f    |
                   |------------|
                   | WhileQuad  |          Empty
                   |------------|

                   Quadruples

                   q    GotoOp  WhileQuad
                   False exit is backpatched with q+1
*)

PROCEDURE BuildEndWhile ;
VAR
   While,
   t, f : CARDINAL ;
BEGIN
   PopBool(t, f) ;
   Assert(t=0) ;
   PopT(While) ;
   GenQuad(GotoOp, NulSym, NulSym, While) ;
   BackPatch(f, NextQuad) ;
   BackPatch(PopExit(), NextQuad)   (* EXIT statements linked outside while *)
END BuildEndWhile ;


(*
   BuildLoop - Builds the Loop part of the Loop statement
               from the quad stack.
               The Stack is expected to contain:


               Entry                   Exit
               =====                   ====

                                                      <- Ptr
               Empty                   +------------+
                                       | LoopQuad   |
                                       |------------|
*)

PROCEDURE BuildLoop ;
BEGIN
   PushT(NextQuad) ;
   PushExit(0)         (* Seperate Exit Stack for loop end *)
END BuildLoop ;


(*
   BuildExit - Builds the Exit part of the Loop statement.
*)

PROCEDURE BuildExit ;
BEGIN
   IF IsEmpty(ExitStack)
   THEN
      WriteFormat0('EXIT - only allowed in LOOP, WHILE, REPEAT, FOR statements')
   ELSE
      GenQuad(GotoOp, NulSym, NulSym, 0) ;
      PushExit(Merge(PopExit(), NextQuad-1))
   END
END BuildExit ;


(*
   BuildEndLoop - Builds the End part of the Loop statement
                  from the quad stack.
                  The Stack is expected to contain:


                  Entry                   Exit
                  =====                   ====

          Ptr ->
                  +------------+
                  | LoopQuad   |          Empty
                  |------------|

                  Quadruples

                  Goto  _  _  LoopQuad
*)

PROCEDURE BuildEndLoop ;
VAR
   Loop: CARDINAL ;
BEGIN
   PopT(Loop) ;
   GenQuad(GotoOp, NulSym, NulSym, Loop) ;
   BackPatch(PopExit(), NextQuad)
END BuildEndLoop ;


(*
   BuildThenIf - Builds the Then part of the If statement
                 from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

         Ptr ->                                          <- Ptr
                 +------------+          +------------+
                 | t   | f    |          | 0    | f   |
                 |------------|          |------------|

                 Quadruples

                 The true exit is BackPatched to point to
                 the NextQuad.
*)

PROCEDURE BuildThenIf ;
VAR
   t, f: CARDINAL ;
BEGIN
   CheckBooleanId ;
   PopBool(t, f) ;
   BackPatch(t, NextQuad) ;
   PushBool(0, f)
END BuildThenIf ;


(*
   BuildElse - Builds the Else part of the If statement
               from the quad stack.
               The Stack is expected to contain:


               Entry                   Exit
               =====                   ====

       Ptr ->
               +------------+          +------------+
               | t   | f    |          | t+q  | 0   |
               |------------|          |------------|

               Quadruples

               q    GotoOp  _  _  0
               q+1  <- BackPatched from f
*)

PROCEDURE BuildElse ;
VAR
   t, f: CARDINAL ;
BEGIN
   GenQuad(GotoOp, NulSym, NulSym, 0) ;
   PopBool(t, f) ;
   BackPatch(f, NextQuad) ;
   PushBool(Merge(t, NextQuad-1), 0)   (* NextQuad-1 = Goto Quad *)
END BuildElse ;


(*
   BuildEndIf - Builds the End part of the If statement
                from the quad stack.
                The Stack is expected to contain:


                Entry                   Exit
                =====                   ====

        Ptr ->
                +------------+
                | t   | f    |          Empty
                |------------|

                Quadruples

                Both t and f are backpatched to point to the NextQuad
*)

PROCEDURE BuildEndIf ;
VAR
   t, f: CARDINAL ;
BEGIN
   PopBool(t, f) ;
   BackPatch(t, NextQuad) ;
   BackPatch(f, NextQuad)
END BuildEndIf ;


(*
   BuildElsif1 - Builds the Elsif part of the If statement
                 from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

         Ptr ->
                 +------------+          +------------+
                 | t   | f    |          | t+q  | 0   |
                 |------------|          |------------|

                 Quadruples

                 q    GotoOp  _  _  0
                 q+1  <- BackPatched from f
*)

PROCEDURE BuildElsif1 ;
VAR
   t, f: CARDINAL ;
BEGIN
   GenQuad(GotoOp, NulSym, NulSym, 0) ;
   PopBool(t, f) ;
   BackPatch(f, NextQuad) ;
   PushBool(Merge(t, NextQuad-1), 0)   (* NextQuad-1 = Goto Quad *)
END BuildElsif1 ;


(*
   BuildElsif2 - Builds the Elsif until part of the If statement
                 from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

          Ptr ->
                 +--------------+
                 | 0    | f1    |                            <- Ptr
                 |--------------|          +---------------+
                 | t2   | f2    |          | t2    | f1+f2 |
                 |--------------|          |---------------|
*)

PROCEDURE BuildElsif2 ;
VAR
   t1, f1,
   t2, f2: CARDINAL ;
BEGIN
   PopBool(t1, f1) ;
   Assert(t1=0) ;
   PopBool(t2, f2) ;
   PushBool(t2, Merge(f1, f2))
END BuildElsif2 ;


(*
   BuildPseudoBy - Builds the Non existant part of the By
                   clause of the For statement
                   from the quad stack.
                   The Stack is expected to contain:


                   Entry                   Exit
                   =====                   ====

                                                           <- Ptr
                                           +------------+
                   Empty                   | BySym      |
                                           |------------|
*)

PROCEDURE BuildPseudoBy ;
BEGIN
   PushT(MakeConstLit(MakeKey('1')))
END BuildPseudoBy ;


(*
   BuildForToByDo - Builds the For To By Do part of the For statement
                    from the quad stack.
                    The Stack is expected to contain:


                    Entry                   Exit
                    =====                   ====


             Ptr ->                                        <- Ptr
                    +------------+          +------------+
                    | BySym      |          | t   | f    |
                    |------------|          |------------|
                    | e2         |          | ForQuad    |
                    |------------|          |------------|
                    | e1         |          | BySym      |
                    |------------|          |------------|
                    | Ident      |          | IdentSym   |
                    |------------|          |------------|

                    Quadruple

                    q   BecomesOp  IdentSym  _  e1
                    q+1 if <       by        0  q+5
                    q+2 GotoOp                  q+3
                    q+3 If >       IdentSym  e2 _
                    q+4 GotoOp                  q+7
                    q+5 If <=      IdentSym  e2 _
                    q+6 GotoOp                  q+7


                    The For Loop is regarded:

                    For ident := e1 To e2 By by Do

                    End
*)

PROCEDURE BuildForToByDo ;
VAR
   l1, l2: LineNote ;
   e1, e2,
   Id    : Name ;
   IdSym,
   BySym,
   t, f  : CARDINAL ;
   t1, f1: CARDINAL ;
BEGIN
   l2 := PopLineNo() ;
   l1 := PopLineNo() ;
   UseLineNote(l1) ;
   PushExit(0) ;  (* Allows EXIT to be used to exit the for loop *)
   PopT(BySym) ;
   PopT(e2) ;
   PopT(e1) ;
   PopT(Id) ;
   IdSym := RequestSym(Id) ;
   PushT(IdSym) ;
   PushT(e1) ;
   BuildAssignment ;

   UseLineNote(l2) ;
   PushT(IdSym) ;     (* Push information for future FOR reference *)
   PushT(BySym) ;
   PushT(NextQuad) ;
   (* q+1 if <       by        0  q+5 *)
   (* q+2 GotoOp                  q+3 *)
   PushT(BySym) ;           (* BuildRelOp  1st parameter *)
   PushT(LessTok) ;         (*             2nd parameter *)
   PushT(MakeConstLit(MakeKey('0'))) ;  (* 3rd parameter *)
   BuildRelOp ;
   PopBool(t, f) ;          (*             return value  *)
   BackPatch(f, NextQuad) ;
   (* q+3 If >       IdentSym  e2 _   *)
   (* q+4 GotoOp                  q+7 *)
   PushT(IdSym) ;           (* BuildRelOp  1st parameter *)
   PushT(GreaterTok) ;      (*             2nd parameter *)
   PushT(e2) ;              (*             3rd parameter *)
   BuildRelOp ;
   BackPatch(t, NextQuad) ; (* fixes q+1 Destimation to q+5 *)
   PopBool(t, f) ;          (* true = exits, false = in  *)
   PushT(IdSym) ;           (* BuildRelOp  1st parameter *)
   PushT(LessTok) ;         (*             2nd parameter *)
   PushT(e2) ;              (*             3nd parameter *)
   BuildRelOp ;
   BackPatch(f, NextQuad) ; (* Fixes q+4 GotoOp          *)
   PopBool(t1, f1) ;
   BackPatch(f1, NextQuad) ; (* Fixes q+6 GotoOp         *)
   PushBool(Merge(t1, t), 0) ;
         (* q+3 and q+5 provide the exit to the for loop *)
   CheckSubrange(IdSym, IdSym) ;
END BuildForToByDo ;


(*
   BuildEndFor - Builds the End part of the For statement
                 from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

         Ptr ->
                 +------------+
                 | t   | f    |
                 |------------|
                 | ForQuad    |
                 |------------|
                 | BySym      |
                 |------------|
                 | IdSym      |          Empty
                 |------------|
*)

PROCEDURE BuildEndFor ;
VAR
   tsym,
   t, f,
   IncQuad,
   ForQuad,
   BySym,
   IdSym  : CARDINAL ;
BEGIN
   PopBool(t, f) ;
   Assert(f=0) ;
   PopT(ForQuad) ;
   PopT(BySym) ;
   PopT(IdSym) ;
   IF GetMode(IdSym)=LeftValue
   THEN
      (* index variable is a LeftValue, therefore we must dereference it *)
      tsym := MakeTemporary(RightValue) ;
      PutVar(tsym, GetType(IdSym)) ;
      GenQuad(IndrXOp, tsym, GetType(IdSym), IdSym) ;
      IncQuad := NextQuad ;
      GenQuad(AddOp, tsym, tsym, BySym) ;
      GenQuad(XIndrOp, IdSym, GetType(IdSym), tsym)
   ELSE
      IncQuad := NextQuad ;
      GenQuad(AddOp, IdSym, IdSym, BySym)
   END ;
   GenQuad(GotoOp, NulSym, NulSym, ForQuad) ;
   BackPatch(t, NextQuad) ;
   BackPatch(PopExit(), NextQuad) ;
   AddForInfo(ForQuad, NextQuad-1, IncQuad, IdSym)
END BuildEndFor ;


(*
   BuildCaseStart - starts the case statement.
                    It initializes a backpatch list on the compile
                    time stack, the list is used to contain all
                    case break points. The list is later backpatched
                    and contains all positions of the case statement
                    which jump to the end of the case statement.
                    The stack also contains room for a boolean
                    expression, this is needed to allow , operator
                    in the CaseField alternatives.

                    The Stack is expected to contain:


                    Entry                   Exit
                    =====                   ====

                                                           <- Ptr
                                            +------------+
                    Empty                   | 0    | 0   |
                                            |------------|
                                            | 0    | 0   |
                                            |------------|
*)

PROCEDURE BuildCaseStart ;
BEGIN
   PushBool(0, 0) ;  (* BackPatch list initialized *)
   PushBool(0, 0)    (* Room for a boolean expression *)
END BuildCaseStart ;


(*
   BuildCaseStartStatementSequence - starts the statement sequence
                                     inside a case clause.
                                     BackPatches the true exit to the
                                     NextQuad.
                                     The Stack:

                                     Entry             Exit

                              Ptr ->                                  <- Ptr
                                     +-----------+     +------------+
                                     | t   | f   |     | 0   | f    |
                                     |-----------|     |------------|
*)

PROCEDURE BuildCaseStartStatementSequence ;
VAR
   t, f: CARDINAL ;
BEGIN
   PopBool(t, f) ;
   BackPatch(t, NextQuad) ;
   PushBool(0, f)
END BuildCaseStartStatementSequence ;


(*
   BuildCaseEndStatementSequence - ends the statement sequence
                                   inside a case clause.
                                   BackPatches the false exit f1 to the
                                   NextQuad.
                                   Asserts that t1 and f2 is 0
                                   Pushes t2+q and 0

                                   Quadruples:

                                   q  GotoOp  _  _  0

                                   The Stack:

                                   Entry             Exit

                            Ptr ->                                  <- Ptr
                                   +-----------+     +------------+
                                   | t1  | f1  |     | 0    | 0   |
                                   |-----------|     |------------|
                                   | t2  | f2  |     | t2+q | 0   |
                                   |-----------|     |------------|
*)

PROCEDURE BuildCaseEndStatementSequence ;
VAR
   t1, f1,
   t2, f2: CARDINAL ;
BEGIN
   GenQuad(GotoOp, NulSym, NulSym, 0) ;
   PopBool(t1, f1) ;
   PopBool(t2, f2) ;          (* t2 contains the break list for the case *)
   BackPatch(f1, NextQuad) ;  (* f1 no longer needed *)
   Assert(t1=0) ;
   Assert(f2=0) ;
   PushBool(Merge(t2, NextQuad-1), 0) ;  (* NextQuad-1 = Goto Quad *)
   PushBool(0, 0)             (* Room for boolean expression *)
END BuildCaseEndStatementSequence ;


(*
   BuildCaseRange - builds the range testing quaruples for
                    a case clause.

                    IF (e1>=ce1) AND (e1<=ce2)
                    THEN

                    ELS..

                    The Stack:

                    Entry             Exit

             Ptr ->
                    +-----------+
                    | ce2       |                   <- Ptr
                    |-----------|     +-----------+
                    | ce1       |     | t   | f   |
                    |-----------|     |-----------|
                    | t1  | f1  |     | t1  | f1  |
                    |-----------|     |-----------|
                    | t2  | f2  |     | t2  | f2  |
                    |-----------|     |-----------|
                    | e1        |     | e1        |
                    |-----------|     |-----------|
*)

PROCEDURE BuildCaseRange ;
VAR
   ce1, ce2,
   e1,
   t2, f2,
   t1, f1  : CARDINAL ;
BEGIN
   PopT(ce2) ;
   PopT(ce1) ;
   PopBool(t1, f1) ;
   PopBool(t2, f2) ;
   PopT(e1) ;
   PushT(e1) ;         (* leave e1 on bottom of stack when exit procedure *)
   PushBool(t2, f2) ;
   PushBool(t1, f1) ;  (* also leave t1 and f1 on the bottom of the stack *)
   PushT(e1) ;
   PushT(GreaterEqualTok) ;
   PushT(ce1) ;
   BuildRelOp ;
   PushT(AndTok) ;
   RecordOp ;
   PushT(e1) ;
   PushT(LessEqualTok) ;
   PushT(ce2) ;
   BuildRelOp ;
   BuildBinaryOp
END BuildCaseRange ;


(*
   BuildCaseEquality - builds the range testing quadruples for
                       a case clause.

                       IF e1=ce1
                       THEN

                       ELS..

                       The Stack:

                       Entry             Exit

                Ptr ->
                       +-----------+     +-----------+
                       | ce1       |     | t   | f   |
                       |-----------|     |-----------|
                       | t1  | f1  |     | t1  | f1  |
                       |-----------|     |-----------|
                       | t2  | f2  |     | t2  | f2  |
                       |-----------|     |-----------|
                       | e1        |     | e1        |
                       |-----------|     |-----------|
*)
 
PROCEDURE BuildCaseEquality ;
VAR
   ce1, e1,
   t2, f2,
   t1, f1 : CARDINAL ;
BEGIN
   PopT(ce1) ;
   PopBool(t1, f1) ;
   PopBool(t2, f2) ;
   PopT(e1) ;
   PushT(e1) ;        (* leave e1 on bottom of stack when exit procedure *)
   PushBool(t2, f2) ; (* also leave t2 and f2 on the bottom of the stack *)
   PushBool(t1, f1) ;
   PushT(e1) ;
   PushT(EqualTok) ;
   PushT(ce1) ;
   BuildRelOp
END BuildCaseEquality ;


(*
   BuildCaseList - merges two case tests into one

                   The Stack:
 
                   Entry             Exit
 
            Ptr ->
                   +-----------+
                   | t2  | f2  |
                   |-----------|     +-------------+
                   | t1  | f1  |     | t1+t2| f1+f2|
                   |-----------|     |-------------|
*)

PROCEDURE BuildCaseList ; 
VAR
   t2, f2,
   t1, f1: CARDINAL ;
BEGIN
   PopBool(t2, f2) ;
   PopBool(t1, f1) ;
   PushBool(Merge(t1, t2), Merge(f1, f2))
END BuildCaseList ;


(*
   BuildCaseOr - builds the , in the case clause.
 
                 The Stack:
 
                 Entry             Exit
 
          Ptr ->                                  <- Ptr
                 +-----------+     +------------+
                 | t   | f   |     | t    | 0   |
                 |-----------|     |------------|
*)
 
PROCEDURE BuildCaseOr ;
VAR 
   t, f: CARDINAL ;
BEGIN 
   PopBool(t, f) ; 
   BackPatch(f, NextQuad) ; 
   PushBool(t, 0)
END BuildCaseOr ; 


(*
   BuildCaseElse - builds the else of case clause.
 
                  The Stack:
 
                  Entry             Exit
 
           Ptr ->                                  <- Ptr
                  +-----------+     +------------+
                  | t   | f   |     | t    | 0   |
                  |-----------|     |------------|
*)
 
PROCEDURE BuildCaseElse ;
VAR 
   t, f: CARDINAL ;
BEGIN 
   PopBool(t, f) ; 
   BackPatch(f, NextQuad) ; 
   PushBool(t, 0)
END BuildCaseElse ; 


(* 
   BuildCaseEnd - builds the end of case clause.

                  The Stack:

                  Entry             Exit

           Ptr ->
                  +-----------+
                  | t1  | f1  |
                  |-----------|
                  | t2  | f2  |
                  |-----------|
                  | e1        |
                  |-----------|     Empty
*)

PROCEDURE BuildCaseEnd ;
VAR
   e1,
   t, f: CARDINAL ;
BEGIN
   PopBool(t, f) ;
   BackPatch(f, NextQuad) ;
   BackPatch(t, NextQuad) ;
   PopBool(t, f) ;
   BackPatch(f, NextQuad) ;
   BackPatch(t, NextQuad) ;
   PopT(e1)
END BuildCaseEnd ;


(*
   BuildNulParam - Builds a nul parameter on the stack.
                   The Stack:

                   Entry             Exit

                                                    <- Ptr
                   Empty             +------------+
                                     | 0          |
                                     |------------|
*)

PROCEDURE BuildNulParam ;
BEGIN
   PushT(0)
END BuildNulParam ;


(*
   BuildSizeCheckStart - switches off all quadruple generation if the function SIZE
                         is being "called". This should be done as SIZE only requires the
                         actual type of the expression, not its value. Consider the problem of
                         SIZE(UninitializedPointer^) quite legal and it must also be safe!


                         The Stack is expected to contain:


                         Entry                       Exit
                         =====                       ====

                 Ptr ->                                                 <- Ptr
                        +----------------+          +-----------------+
                        | ProcSym | Type |          | ProcSym | Type  |
                        |----------------|          |-----------------|
*)

PROCEDURE BuildSizeCheckStart ;
VAR
   ProcSym, Type: CARDINAL ;
BEGIN
   PopTF(ProcSym, Type) ;
   IF ProcSym=Size
   THEN
      QuadrupleGeneration := FALSE
   END ;
   PushTF(ProcSym, Type)
END BuildSizeCheckStart ;


(*
   BuildSizeCheckEnd - checks to see whether the function "called" was in fact SIZE.
                       If so then we restore quadruple generation.
*)

PROCEDURE BuildSizeCheckEnd (ProcSym: CARDINAL) ;
BEGIN
   IF ProcSym=Size
   THEN
      QuadrupleGeneration := TRUE
   END
END BuildSizeCheckEnd ;



(*
   BuildProcedureCall - builds a procedure call.
                        Although this procedure does not directly
                        destroy the procedure parameters, it calls
                        routine which will manipulate the stack and
                        so the entry and exit states of the stack are shown.

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildProcedureCall ;
VAR
   NoOfParam,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   PushT(NoOfParam) ;  (* Compile time stack restored to entry state *)
   IF IsPseudoBaseProcedure(ProcSym)
   THEN
      DumpStack ;
      ManipulatePseudoCallParameters ;
      DumpStack ;
      BuildPseudoProcedureCall ;
      DumpStack ;
   ELSIF IsUnknown(ProcSym)
   THEN
      ErrorFormat1(NewError(GetFirstUsed(ProcSym)),
                   '%a is not recognised as a procedure, check declaration or import',
                   GetSymName(ProcSym)) ;
      PopN(NoOfParam + 2)
   ELSE
      DumpStack ;
      BuildRealProcedureCall ;
      DumpStack ;
   END
END BuildProcedureCall ;


(*
   BuildRealProcedureCall - builds a real procedure call.
                            The Stack:


                            Entry                      Exit

                     Ptr ->
                            +----------------+
                            | NoOfParam      |
                            |----------------|
                            | Param 1        |
                            |----------------|
                            | Param 2        |
                            |----------------|
                            .                .
                            .                .
                            .                .
                            |----------------|
                            | Param #        |
                            |----------------|
                            | ProcSym | Type |         Empty
                            |----------------|
*)

PROCEDURE BuildRealProcedureCall ;
VAR
   NoOfParam: CARDINAL ;
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   PushT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+2) ;
   IF IsVar(ProcSym)
   THEN
      (* Procedure Variable ? *)
      ProcSym := OperandF(NoOfParam+2)
   END ;
   IF IsDefImp(GetScope(ProcSym)) AND IsDefinitionForC(GetScope(ProcSym))
   THEN
      BuildRealFuncProcCall(FALSE, TRUE)
   ELSE
      BuildRealFuncProcCall(FALSE, FALSE)
   END
END BuildRealProcedureCall ;


(*
   BuildRealProcFuncCall - builds a real procedure or function call.
                           The Stack:


                            Entry                      Exit

                     Ptr ->
                            +----------------+
                            | NoOfParam      |
                            |----------------|
                            | Param 1        |
                            |----------------|
                            | Param 2        |
                            |----------------|
                            .                .
                            .                .
                            .                .
                            |----------------|
                            | Param #        |
                            |----------------|
                            | ProcSym | Type |         Empty
                            |----------------|
*)

PROCEDURE BuildRealFuncProcCall (IsFunc, IsForC: BOOLEAN) ;
VAR
   e          : Error ;
   NoOfParam,
   i, pi,
   ReturnVar,
   ProcSym,
   Proc       : CARDINAL ;
BEGIN
   CheckProcedureParameters(IsForC) ;
   PopT(NoOfParam) ;
   PushT(NoOfParam) ;  (* Restore stack to original state *)
   ProcSym := OperandT(NoOfParam+2) ;
   IF IsVar(ProcSym)
   THEN
      (* Procedure Variable ? *)
      Proc := OperandF(NoOfParam+2)
   ELSE
      Proc := ProcSym
   END ;
   IF IsFunc
   THEN
      IF GetType(Proc)=NulSym
      THEN
         WriteFormat1('procedure %a does not have a return value - it is not a function', GetSymName(Proc))
      END
   ELSE
      (* is being called as a procedure *)
      IF GetType(Proc)#NulSym
      THEN
         e := NewError(GetTokenNo()) ;
         ErrorFormat1(e, 'trying to call function, %a, but ignoring its return value', GetSymName(Proc)) ;
         e := ChainError(GetDeclared(Proc), e) ;
         ErrorFormat1(e, 'function, %a, is being called but its return value is ignored', GetSymName(Proc))
      END
   END ;
   ManipulateParameters(IsForC) ;
   PopT(NoOfParam) ;
   IF IsFunc
   THEN
      GenQuad(ParamOp, 0, Proc, ProcSym)  (* Space for return value *)
   END ;
   IF PushParametersLeftToRight
   THEN
      i := NoOfParam ;
      pi := 1 ;     (* stack index referencing stacked parameter, i *)
      WHILE i>0 DO
         GenQuad(ParamOp, i, Proc, OperandT(pi)) ;
         DEC(i) ;
         INC(pi)
      END
   ELSE
      Assert(NOT UsingGCCBackend) ;
      i := 1 ;
      pi := NoOfParam ;   (* stack index referencing stacked parameter, i *)
      WHILE i<=NoOfParam DO
         GenQuad(ParamOp, i, Proc, OperandT(pi)) ;
         INC(i) ;
         DEC(pi)
      END
   END ;
   GenQuad(CallOp, NulSym, NulSym, ProcSym) ;
   PopN(NoOfParam+1) ; (* Destroy arguments and procedure call *)
   IF IsFunc
   THEN
      (* ReturnVar - will have the type of the procedure *)
      ReturnVar := MakeTemporary(RightValue) ;
      PutVar(ReturnVar, GetType(Proc)) ;
      GenQuad(FunctValueOp, ReturnVar, NulSym, Proc) ;
      PushTF(ReturnVar, GetType(Proc))
   END
END BuildRealFuncProcCall ;


(*
   CheckProcedureParameters - Checks the parameters which are being passed to
                              procedure ProcSym.

                              The Stack:
   
   
                              Entry                      Exit
   
                       Ptr ->                                               <- Ptr
                              +----------------+         +----------------+
                              | NoOfParam      |         | NoOfParam      |
                              |----------------|         |----------------|
                              | Param 1        |         | Param 1        |
                              |----------------|         |----------------|
                              | Param 2        |         | Param 2        |
                              |----------------|         |----------------|
                              .                .         .                .
                              .                .         .                .
                              .                .         .                .
                              |----------------|         |----------------|
                              | Param #        |         | Param #        |
                              |----------------|         |----------------|
                              | ProcSym | Type |         | ProcSym | Type |
                              |----------------|         |----------------|

*)

PROCEDURE CheckProcedureParameters (IsForC: BOOLEAN) ;
VAR
   e           : Error ;
   Unbounded   : BOOLEAN ;
   CallParam,
   ParamI,
   ParamIType,
   ParamTotal,
   TypeSym,
   pi,
   Proc,
   ProcSym,
   i, Var      : CARDINAL ;
BEGIN
   PopT(ParamTotal) ;
   PushT(ParamTotal) ;  (* Restore stack to origional state *)
   ProcSym := OperandT(ParamTotal+1+1) ;
   IF IsVar(ProcSym) AND IsProcType(GetType(ProcSym))
   THEN
      (* Procedure Variable ? *)
      Proc := OperandF(ParamTotal+1+1)
   ELSE
      Proc := ProcSym
   END ;
   IF NOT (IsProcedure(Proc) OR IsProcType(Proc))
   THEN
      IF IsUnknown(Proc)
      THEN
         ErrorFormat1(NewError(GetFirstUsed(Proc)),
                      '%a is not recognised as a procedure, check declaration or import',
                      GetSymName(Proc))
      ELSIF NOT IsTemporary(Proc)
      THEN
         e := NewError(GetTokenNo()) ;
         ErrorFormat1(e,
                      '%a is not recognised as a procedure, check declaration or import',
                      GetSymName(Proc)) ;
         e := ChainError(GetFirstUsed(Proc), e) ;
         ErrorFormat1(e,
                      '%a is not recognised as a procedure, check declaration or import',
                      GetSymName(Proc)) ;
      END
   END ;
   IF CompilerDebugging
   THEN
      printf1('  %a ( ', GetSymName(Proc))
   END ;
   i := 1 ;
   pi := ParamTotal+1 ;   (* stack index referencing stacked parameter, i *)
   WHILE i<=ParamTotal DO
      IF i<=NoOfParam(Proc)
      THEN
         ParamI := GetParam(Proc, i) ;
         IF CompilerDebugging
         THEN
            printf2('%a: %a', GetSymName(ParamI), GetSymName(GetType(ParamI)))
         END ;
         CallParam := OperandT(pi) ;
         IF IsConst(CallParam)
         THEN
            IF IsVarParam(Proc, i)
            THEN
               FailParameter('trying to pass a constant to a VAR parameter',
                             CallParam, ParamI, Proc, i)
            ELSIF IsConstString(CallParam)
            THEN
               IF (GetStringLength(CallParam) = 1)   (* if = 1 then it maybe treated as a char *)
               THEN
                  CheckParameter(CallParam, ParamI, Proc, i, NIL)
               ELSIF NOT IsUnboundedParam(Proc, i)
               THEN
                  (* we possibly need to allow passing strings of exact size to an ARRAY [0..n] OF CHAR *)
                  IF IsForC AND (GetType(GetParam(Proc, i))=Address)
                  THEN
                     FailParameter('a string constant can either be passed to an ADDRESS parameter or an ARRAY OF CHAR',
                                   CallParam, ParamI, Proc, i)
                  ELSE
                     FailParameter('cannot pass a string constant to a non unbounded array parameter',
                                   CallParam, ParamI, Proc, i)
                  END
               END
            END
         ELSE
            CheckParameter(CallParam, ParamI, Proc, i, NIL)
         END
      ELSE
         IF IsForC AND UsesVarArgs(Proc)
         THEN
            (* these are varargs, therefore we don't check them *)
            i := ParamTotal
         ELSE
            FailParameter('too many parameters', OperandT(pi), NulSym, Proc, i)
         END
      END ;
      INC(i) ;
      DEC(pi) ;
      IF CompilerDebugging
      THEN
         IF i<=ParamTotal
         THEN
            printf0('; ')
         ELSE
            printf0(' ) ; \n')
         END
      END
   END
END CheckProcedureParameters ;


(*
   CheckProcTypeAndProcedure - checks the ProcType with the call.
*)

PROCEDURE CheckProcTypeAndProcedure (ProcType: CARDINAL; call: CARDINAL; TypeList: List) ;
VAR
   i, n            : CARDINAL ;
   CheckedProcedure: CARDINAL ;
   e               : Error ;
BEGIN
   n := NoOfParam(ProcType) ;
   IF IsVar(call) OR IsTemporary(call) OR IsParameter(call)
   THEN
      CheckedProcedure := GetType(call)
   ELSE
      CheckedProcedure := call
   END ;
   IF n#NoOfParam(CheckedProcedure)
   THEN
      e := NewError(GetDeclared(ProcType)) ;
      ErrorFormat2(e, 'procedure (%a) is a parameter being passed as variable (%a) but they are declared with different number of parameters',
                   GetSymName(call), GetSymName(ProcType)) ;
      e := ChainError(GetDeclared(call), e) ;
      ErrorFormat3(e, 'procedure (%a) is being called incorrectly with (%d) parameters, declared with (%d)',
                   GetSymName(call), n, NoOfParam(CheckedProcedure))
   ELSE
      i := 1 ;
      WHILE i<=n DO
         IF IsVarParam(ProcType, i) # IsVarParam(CheckedProcedure, i)
         THEN
            e := NewError(GetDeclared(ProcType)) ;
            IF IsVarParam(ProcType, i)
            THEN
               ErrorFormat2(e, 'parameter %d in procedure type (%a) causes a mismatch it was declared it as a VAR parameter',
                            i, GetSymName(ProcType))
            ELSE
               ErrorFormat2(e, 'parameter %d in procedure type (%a) causes a mismatch it was declared it as a non VAR parameter',
                            i, GetSymName(ProcType))
            END ;
            e := ChainError(GetDeclared(call), e) ;
            IF IsVarParam(call, i)
            THEN
               ErrorFormat1(e, 'whereas procedure (%a) has declared it as a VAR parameter', GetSymName(call))
            ELSE
               ErrorFormat1(e, 'whereas procedure (%a) has declared it as a non VAR parameter', GetSymName(call))
            END ;
         END ;
         CheckParameter(GetParam(CheckedProcedure, i), GetParam(ProcType, i), call, i, TypeList) ;
         INC(i)
      END
   END
END CheckProcTypeAndProcedure ;


(*
   IsReallyPointer - returns TRUE is sym is a pointer, address or a type declared
                     as a pointer or address.
*)

PROCEDURE IsReallyPointer (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsType(sym)
   THEN
      IF GetType(sym)=NulSym
      THEN
         RETURN( FALSE )
      ELSE
         RETURN( IsReallyPointer(GetType(sym)) )
      END
   ELSE
      IF IsPointer(sym)
      THEN
         RETURN( TRUE )
      ELSE
         RETURN( sym=Address )
      END
   END
END IsReallyPointer ;


(*
   CheckParameter - checks that types CallType and ParamType are compatible for parameter
                    passing. ProcSym is the procedure and i is the parameter number.

                    We obey the following rules:

                    (1)  we allow WORD and BYTE to be compitable with any like sized
                         type.
                    (2)  we allow ADDRESS to be compatible with any pointer type.
                    (3)  we relax INTEGER and CARDINAL checking for Temporary variables.

                    Note that type sizes are checked during the code generation pass.
*)

PROCEDURE CheckParameter (Call, Param, ProcSym: CARDINAL; i: CARDINAL; TypeList: List) ;
VAR
   NewList            : BOOLEAN ;
   CallType, ParamType: CARDINAL ;
BEGIN
   ParamType := GetType(Param) ;
   IF IsConstString(Call) AND (GetStringLength(Call) = 1)   (* if = 1 then it maybe treated as a char *)
   THEN
      CallType := Char
   ELSIF Call=Boolean
   THEN
      CallType := Call
   ELSE
      CallType := GetType(Call)
   END ;
   IF TypeList=NIL
   THEN
      NewList := TRUE ;
      InitList(TypeList)
   ELSE
      NewList := FALSE
   END ;
   IF IsItemInList(TypeList, CallType)
   THEN
      (* no need to check *)
      RETURN
   END ;
   IncludeItemIntoList(TypeList, CallType) ;
   IF IsProcType(ParamType)
   THEN
      IF (NOT IsProcedure(Call)) AND ((CallType=NulSym) OR (NOT IsProcType(CallType)))
      THEN
         FailParameter('expecting a procedure or procedure variable as a parameter',
                       Call, Param, ProcSym, i) ;
         RETURN
      END ;
      IF IsProcedure(Call) AND IsProcedureNested(Call)
      THEN
         WriteFormat2('cannot pass a nested procedure, %a, as parameter number %d as the outer scope will be unknown at runtime',
                      GetSymName(Call), i)
      END ;
      (* we can check the return type of both proc types *)
      IF (CallType#NulSym) AND IsProcType(CallType)
      THEN
         IF ((GetType(CallType)#NulSym) AND (GetType(ParamType)=NulSym))
         THEN
            FailParameter('the item being passed is a function whereas the formal procedure parameter is a procedure',
                          Call, Param, ProcSym, i) ;
            RETURN
         ELSIF ((GetType(CallType)=NulSym) AND (GetType(ParamType)#NulSym))
         THEN
            FailParameter('the item being passed is a procedure whereas the formal procedure parameter is a function',
                          Call, Param, ProcSym, i) ;
            RETURN
         ELSIF NOT IsAssignmentCompatible(GetType(CallType), GetType(ParamType))
         THEN
            FailParameter('the return result of the procedure variable parameter is not compatible with the return result of the item being passed',
                          Call, Param, ProcSym, i) ;
            RETURN
         END
      END ;
      (* now to check each parameter of the proc type *)
      CheckProcTypeAndProcedure(ParamType, Call, TypeList)
   ELSIF (CallType#ParamType) AND (CallType#NulSym)
   THEN
      IF IsUnknown(ParamType)
      THEN
         FailParameter('procedure parameter type is undeclared',
                       Call, Param, ProcSym, i) ;
         RETURN
      END ;
      IF IsUnbounded(CallType) AND (NOT IsUnboundedParam(ProcSym, i))
      THEN
         FailParameter('attempting to pass an unbounded array to an NON unbounded parameter',
                       Call, Param, ProcSym, i) ;
         RETURN
      ELSIF IsUnboundedParam(ProcSym, i)
      THEN
         IF IsUnbounded(CallType) OR IsArray(CallType)
         THEN
            CallType := GetType(CallType)
         END ;
         ParamType := GetType(ParamType) ;
         IF ((ParamType=Word)           OR  (ParamType=Byte))    OR
            ((CallType=Word)            OR  (CallType=Byte))     OR
            IsAssignmentCompatible(ParamType, CallType) OR IsTemporary(Call)
(*
            (IsReallyPointer(ParamType) AND (CallType=Address))  OR
            (IsReallyPointer(CallType)  AND (ParamType=Address)) OR
*)
         THEN
            (* it is legal *)
         ELSE
            FailParameter('identifier with an incompatible type is being passed to this procedure',
                          Call, Param, ProcSym, i) ;
            RETURN
         END
      ELSIF CallType#ParamType
      THEN
         IF IsAssignmentCompatible(ParamType, CallType) OR IsTemporary(Call)
         THEN
            (* it is legal *)
         ELSE
            FailParameter('identifier with an incompatible type is being passed to this procedure',
                          Call, Param, ProcSym, i)
         END
      END
   END ;
   IF NewList
   THEN
      KillList(TypeList)
   END
END CheckParameter ;


(*
   DescribeType - returns a String describing a symbol, Sym, name and its type.
*)

PROCEDURE DescribeType (Sym: CARDINAL) : String ;
VAR
   s        : String ;
   i,
   Low, High,
   Subrange,
   Subscript,
   Type     : CARDINAL ;
BEGIN
   s := NIL ;
   IF IsConstString(Sym)
   THEN
      IF (GetStringLength(Sym) = 1)   (* if = 1 then it maybe treated as a char *)
      THEN
         s := InitString('(constant string) or CHAR')
      ELSE
         s := InitString('(constant string)')
      END
   ELSIF IsConst(Sym)
   THEN
      s := InitString('(constant)')
   ELSIF IsUnknown(Sym)
   THEN
      s := InitString('(unknown)')
   ELSE
      Type := GetType(Sym) ;
      IF Type=NulSym
      THEN
         s := InitString('(unknown)')
      ELSIF IsUnbounded(Type)
      THEN
         s := Sprintf1(Mark(InitString('ARRAY OF %s')),
                       Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(Type))))))
      ELSIF IsArray(Type)
      THEN
         s := InitString('ARRAY [') ;
         i := 1 ;
         REPEAT
            Subscript := GetNth(Type, i) ;
            IF Subscript#NulSym
            THEN
               Assert(IsSubscript(Subscript)) ;
               Subrange := GetType(Subscript) ;
               IF NOT IsSubrange(Subrange)
               THEN
                  WriteFormat3('error in definition of array (%a) in subscript (%d) which has no subrange, instead type given is (%a)',
                               GetSymName(Sym), i, GetSymName(Subrange))
               END ;
               Assert(IsSubrange(Subrange)) ;
               GetSubrange(Subrange, High, Low) ;
               s := ConCat(s, Mark(Sprintf2(Mark(InitString('%s..%s')),
                                            Mark(InitStringCharStar(KeyToCharStar(GetSymName(Low)))),
                                            Mark(InitStringCharStar(KeyToCharStar(GetSymName(High)))))))
            END ;
            INC(i)
         UNTIL Subscript=NulSym ;
         s := ConCat(ConCat(s, Mark(InitString('] OF '))), Mark(DescribeType(GetType(Type))))
      ELSE
         IF IsUnknown(Type)
         THEN
            s := Sprintf1(Mark(InitString('%s (currently unknown, check declaration or import)')),
                          Mark(InitStringCharStar(KeyToCharStar(GetSymName(Type)))))
         ELSE
            s := InitStringCharStar(KeyToCharStar(GetSymName(Type)))
         END
      END
   END ;
   RETURN( s )
END DescribeType ;


(*
   FailParameter - generates an error message indicating that a parameter
                   declaration has failed.

                   The parameters are:

                   CurrentState  - string describing the current failing state.
                   Given         - the token that the source code provided.
                   Expecting     - token or identifier that was expected.
                   ParameterNo   - parameter number that has failed.
                   ProcedureSym  - procedure symbol where parameter has failed.

                   If any parameter is Nul then it is ignored.
*)

PROCEDURE FailParameter (CurrentState : ARRAY OF CHAR;
                         Given        : CARDINAL;
                         Expecting    : CARDINAL;
                         ProcedureSym : CARDINAL;
                         ParameterNo  : CARDINAL) ;
VAR
   First,
   ExpectType,
   ReturnType: CARDINAL ;
   e         : Error ;
   s         : String ;
BEGIN
   s := InitString('') ;
   IF CompilingImplementationModule()
   THEN
      s := ConCat(s, Sprintf0(Mark(InitString('error found while compiling the implementation module\n'))))
   ELSIF CompilingProgramModule()
   THEN
      s := ConCat(s, Sprintf0(Mark(InitString('error found while compiling the program module\n'))))
   END ;
   s := ConCat(s, Mark(Sprintf2(Mark(InitString('problem in parameter %d, PROCEDURE %s (')),
                                ParameterNo,
                                Mark(InitStringCharStar(KeyToCharStar(GetSymName(ProcedureSym))))))) ;
   IF NoOfParam(ProcedureSym)>=ParameterNo
   THEN
      IF ParameterNo>1
      THEN
         s := ConCat(s, Mark(InitString('.., ')))
      END ;
      IF IsVarParam(ProcedureSym, ParameterNo)
      THEN
         s := ConCat(s, Mark(InitString('VAR ')))
      END ;

      First := GetDeclared(GetNthParam(ProcedureSym, ParameterNo)) ;
      ExpectType := GetType(Expecting) ;
      IF IsUnboundedParam(ProcedureSym, ParameterNo)
      THEN
         s := ConCat(s, Mark(Sprintf2(Mark(InitString('%s: ARRAY OF %s')),
                                      Mark(InitStringCharStar(KeyToCharStar(GetSymName(Expecting)))),
                                      Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(ExpectType))))))))
      ELSE
         s := ConCat(s, Mark(Sprintf2(Mark(InitString('%s: %s')),
                                      Mark(InitStringCharStar(KeyToCharStar(GetSymName(Expecting)))),
                                      Mark(InitStringCharStar(KeyToCharStar(GetSymName(ExpectType)))))))
      END ;
      IF ParameterNo<NoOfParam(ProcedureSym)
      THEN
         s := ConCat(s, Mark(InitString('; ... ')))
      END
   ELSE
      First := GetDeclared(ProcedureSym) ;
      IF NoOfParam(ProcedureSym)>0
      THEN
         s := ConCat(s, Mark(InitString('..')))
      END
   END ;
   ReturnType := GetType(ProcedureSym) ;
   IF ReturnType=NulSym
   THEN
      s := ConCat(s, Sprintf0(Mark(InitString(') ;\n'))))
   ELSE
      s := ConCat(s, Mark(Sprintf1(Mark(InitString(') : %s ;\n')),
                                   Mark(InitStringCharStar(KeyToCharStar(GetSymName(ReturnType)))))))
   END ;
   IF IsConstString(Given)
   THEN
      s := ConCat(s, Mark(Sprintf1(Mark(InitString("item being passed is '%s'")),
                                   Mark(InitStringCharStar(KeyToCharStar(GetSymName(Given)))))))
   ELSE
      s := ConCat(s, Mark(Sprintf1(Mark(InitString("item being passed is '%s'")),
                                   Mark(InitStringCharStar(KeyToCharStar(GetSymName(Given)))))))
   END ;
   s := ConCat(s, Mark(Sprintf2(Mark(InitString(': %s\nparameter mismatch: %s')),
                                DescribeType(Given), Mark(InitString(CurrentState))))) ;
   ErrorStringAt2(s, First, GetTokenNo())
END FailParameter ;


(*
   ExpectVariable - checks to see whether, sym, is declared as a variable.
                    If not then it generates an error message.
*)

PROCEDURE ExpectVariable (a: ARRAY OF CHAR; sym: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   IF NOT IsVar(sym)
   THEN
      e := NewError(GetTokenNo()) ;
      IF IsUnknown(sym)
      THEN
         ErrorString(e, Sprintf2(Mark(InitString('%s but was given an undeclared symbol %s')),
                                 Mark(InitString(a)),
                                 Mark(InitStringCharStar(KeyToCharStar(GetSymName(sym))))))
      ELSE
         ErrorString(e, Sprintf3(Mark(InitString('%s but was given %s: %s')),
                                 Mark(InitString(a)),
                                 Mark(InitStringCharStar(KeyToCharStar(GetSymName(sym)))),
                                 Mark(DescribeType(sym))))
      END
   END
END ExpectVariable ;


(*
   MakeRightValue - returns a temporary which will have the RightValue of symbol, Sym.
                    If Sym is a right value and has type, type, then no quadruples are
                    generated and Sym is returned. Otherwise a new temporary is created
                    and an IndrX quadruple is generated.
*)

PROCEDURE MakeRightValue (Sym: CARDINAL; type: CARDINAL) : CARDINAL ;
VAR
   t: CARDINAL ;
BEGIN
   IF GetMode(Sym)=RightValue
   THEN
      IF GetType(Sym)=type
      THEN
         RETURN( Sym )   (* already a RightValue with desired type *)
      ELSE
         (*
            type change or mode change, type changes are a pain, but I've left them here as it is
            perhaps easier to remove them later inside M2SubExp (ideally I'd like to identify fix
            this here and in the type checking routines, but it is likely to become too complex.
            So currently we are stuck with sometimes creating tempories just to change type)
         *)
         t := MakeTemporary(RightValue) ;
         PutVar(t, type) ;
         GenQuad(BecomesOp, t, NulSym, Sym) ;
         RETURN( t )
      END
   ELSE
      t := MakeTemporary(RightValue) ;
      PutVar(t, type) ;
      GenQuad(IndrXOp, t, type, Sym) ;
      RETURN( t )
   END
END MakeRightValue ;


(*
   MakeLeftValue - returns a temporary coresponding to the LeftValue of
                   symbol, Sym. No quadruple is generated if Sym is already
                   a LeftValue and has the same type.
*)

PROCEDURE MakeLeftValue (Sym: CARDINAL; with: ModeOfAddr; type: CARDINAL) : CARDINAL ;
VAR
   t: CARDINAL ;
BEGIN
   IF GetMode(Sym)=LeftValue
   THEN
      IF GetType(Sym)=type
      THEN
         (* AddrOp  t     Sym     (when GetMode(Sym)=LeftValue   results in a redundant copy) *)
         RETURN( Sym )
      ELSE
         (*
            type change or mode change, type changes are a pain, but I've left them here as it is
            perhaps easier to remove them later inside M2SubExp (ideally I'd like to identify fix
            this here and in the type checking routines, but it is likely to become too complex.
            So currently we are stuck with sometimes creating tempories just to change type)
         *)
         t := MakeTemporary(with) ;
         PutVar(t, type) ;
         GenQuad(BecomesOp, t, NulSym, Sym) ;
         RETURN( t )
      END
   ELSE
      t := MakeTemporary(with) ;
      PutVar(t, type) ;
      GenQuad(AddrOp, t, NulSym, Sym) ;
      RETURN( t )
   END
END MakeLeftValue ;


(*
   ManipulatePseudoCallParameters - manipulates the parameters to a pseudo function or
                                    procedure. It dereferences all LeftValue parameters
                                    and Boolean parameters.
                                    The Stack:


                                    Entry                      Exit

                             Ptr ->                            exactly the same
                                    +----------------+
                                    | NoOfParameters |
                                    |----------------|
                                    | Param 1        |
                                    |----------------|
                                    | Param 2        |
                                    |----------------|
                                    .                .
                                    .                .
                                    .                .
                                    |----------------|
                                    | Param #        |
                                    |----------------|
                                    | ProcSym | Type |
                                    |----------------|

*)

PROCEDURE ManipulatePseudoCallParameters ;
VAR
   NoOfParameters,
   ProcSym, Proc,
   i, pi         : CARDINAL ;
   f             : BoolFrame ;
BEGIN
   PopT(NoOfParameters) ;
   PushT(NoOfParameters) ;    (* restored to original state *)
   (* Ptr points to the ProcSym *)
   ProcSym := OperandT(NoOfParameters+1+1) ;
   IF IsVar(ProcSym)
   THEN
      InternalError('expecting a pseudo procedure or a type', __FILE__, __LINE__) ;
   ELSE
      Proc := ProcSym
   END ;
   i := 1 ;
   pi := NoOfParameters+1 ;
   WHILE i<=NoOfParameters DO
      IF (GetMode(OperandT(pi))=LeftValue) AND
         (Proc#Adr) AND (Proc#Size) AND (Proc#TSize) AND (Proc#High) AND
         (* procedures which have first parameter as a VAR param *)
         (((Proc#Inc) AND (Proc#Incl) AND (Proc#Dec) AND (Proc#Excl) AND (Proc#New) AND (Proc#Dispose)) OR (i>1))
      THEN
         (* must dereference LeftValue *)
         f := Peep(BoolStack, pi) ;
         f^.TrueExit := MakeRightValue(OperandT(pi), GetType(OperandT(pi)))
      END ;
      INC(i) ;
      DEC(pi)
   END
END ManipulatePseudoCallParameters ;


(*
   ConvertStringToC - creates a new ConstString symbol with a C style string.
*)

PROCEDURE ConvertStringToC (sym: CARDINAL) : CARDINAL ;
VAR
   s: String ;
   n: CARDINAL ;
BEGIN
   s := Sprintf0(InitStringCharStar(KeyToCharStar(GetString(sym)))) ;
   n := MakeConstLitString(makekey(string(s))) ;
   s := KillString(s) ;
   RETURN( n )
END ConvertStringToC ;


(*
   ManipulateParameters - manipulates the procedure parameters in
                          preparation for a procedure call.
                          Prepares Boolean, Unbounded and VAR parameters.
                          The Stack:


                          Entry                      Exit

                   Ptr ->                            exactly the same
                          +----------------+
                          | NoOfParameters |
                          |----------------|
                          | Param 1        |
                          |----------------|
                          | Param 2        |
                          |----------------|
                          .                .
                          .                .
                          .                .
                          |----------------|
                          | Param #        |
                          |----------------|
                          | ProcSym | Type |
                          |----------------|
*)

PROCEDURE ManipulateParameters (IsForC: BOOLEAN) ;
VAR
   s          : String ;
   ParamType,
   NoOfParameters,
   i, pi,
   ProcSym,
   Proc,
   t,
   true, false,
   Des        : CARDINAL ;
   f          : BoolFrame ;
BEGIN
   PopT(NoOfParameters) ;
   ProcSym := OperandT(NoOfParameters+1) ;
   IF IsVar(ProcSym)
   THEN
      (* Procedure Variable ? *)
      Proc := OperandF(NoOfParameters+1)
   ELSE
      Proc := ProcSym
   END ;
   IF (NOT (IsForC AND UsesVarArgs(Proc))) AND (NoOfParam(Proc)#NoOfParameters)
   THEN
      ErrorStringAt2(Sprintf3(Mark(InitString('attempting to pass (%d) parameters to procedure (%s) which was declared with (%d) parameters')),
                              NoOfParameters,
                              Mark(InitStringCharStar(KeyToCharStar(GetSymName(Proc)))),
                              NoOfParam(Proc)),
                     GetTokenNo(), GetTokenNo())
   END ;
   i := 1 ;
   pi := NoOfParameters ;
   WHILE i<=NoOfParameters DO
      f := Peep(BoolStack, pi) ;
      IF i>NoOfParam(Proc)
      THEN
         IF IsForC AND UsesVarArgs(Proc)
         THEN
            IF (GetType(OperandT(pi))#NulSym) AND IsArray(GetType(OperandT(pi)))
            THEN
               f^.TrueExit := MakeLeftValue(OperandT(pi), RightValue, Address)
            ELSIF IsConstString(OperandT(pi))
            THEN
               f^.TrueExit := MakeLeftValue(ConvertStringToC(OperandT(pi)), RightValue, Address)
            ELSIF GetMode(OperandT(pi))=LeftValue
            THEN
               (* must dereference LeftValue (even if we are passing variable as a vararg) *)
               t := MakeTemporary(RightValue) ;
               PutVar(t, GetType(OperandT(pi))) ;
               GenQuad(IndrXOp, t, GetType(OperandT(pi)), OperandT(pi)) ;
               f^.TrueExit := t
            END
         ELSE
            WriteFormat1('parameter not expected for procedure %a', GetSymName(Proc))
         END
      ELSIF IsForC AND IsUnboundedParam(Proc, i) AND
            (GetType(OperandT(pi))#NulSym) AND IsArray(GetType(OperandT(pi)))
      THEN
         f^.TrueExit := MakeLeftValue(OperandT(pi), RightValue, Address)
      ELSIF IsForC AND IsConstString(OperandT(pi))
      THEN
         f^.TrueExit := MakeLeftValue(ConvertStringToC(OperandT(pi)), RightValue, Address)
      ELSIF IsUnboundedParam(Proc, i)
      THEN
         t := MakeTemporary(RightValue) ;
         PutVar(t, Unbounded) ;
         ParamType := GetType(GetType(GetParam(Proc, i))) ;
         IF IsVarParam(Proc, i)
         THEN
            AssignUnboundedVar(OperandT(pi), t, ParamType)
         ELSE
            AssignUnboundedNonVar(OperandT(pi), t, ParamType)
         END ;
         f^.TrueExit := t
      ELSIF IsVarParam(Proc, i)
      THEN
         (*
            Ok must reference by address
            - but we contain the type of the referenced entity
            - we will actually use a RightValue here as we know we are about to call
              a procedure, we are NEVER going to actually use, t, other than in a ParamOp
              which ignores the distinction between Left and Right.
         *)
         f^.TrueExit := MakeLeftValue(OperandT(pi), RightValue, Address)
      ELSIF (NOT IsVarParam(Proc, i)) AND (GetMode(OperandT(pi))=LeftValue)
      THEN
         (* must dereference LeftValue *)
         t := MakeTemporary(RightValue) ;
         PutVar(t, GetType(OperandT(pi))) ;
         GenQuad(IndrXOp, t, GetType(OperandT(pi)), OperandT(pi)) ;
         f^.TrueExit := t
      END ;
      INC(i) ;
      DEC(pi)
   END ;
   PushT(NoOfParameters)
END ManipulateParameters ;


(*
   AssignUnboundedVar - assigns an Unbounded symbol fields,
                        ArrayAddress and ArrayHigh, from an array symbol.
                        UnboundedSym is not a VAR parameter and therefore
                        this procedure can complete both of the fields.
                        Sym can be a Variable with type Unbounded.
                        Sym can be a Variable with type Array.
                        Sym can be a String Constant.

                        ParamType is the TYPE of the paramater
*)

PROCEDURE AssignUnboundedVar (Sym, UnboundedSym, ParamType: CARDINAL) ;
VAR
   Type: CARDINAL ;
BEGIN
   IF IsConst(Sym)
   THEN
      WriteFormat1('cannot pass constant %a as a VAR parameter', GetSymName(Sym))
   ELSIF IsVar(Sym)
   THEN
      Type := GetType(Sym) ;
      IF IsUnbounded(Type)
      THEN
         (* Copy Unbounded Symbol ie. UnboundedSym := Sym *)
         PushT(UnboundedSym) ;
         PushT(Sym) ;
         BuildAssignmentWithoutBounds ;
      ELSIF IsArray(Type) OR (ParamType=Word) OR (ParamType=Byte)
      THEN
         UnboundedVarLinkToArray(Sym, UnboundedSym, ParamType)
      ELSE
         WriteFormat1('illegal type parameter %a: expecting Array or Dynamic Array', GetSymName(Sym)) ;
      END
   ELSE
      WriteFormat1('illegal parameter %a which cannot be passed as: VAR ARRAY OF Type', GetSymName(Sym))
   END
END AssignUnboundedVar ;


(*
   AssignUnboundedNonVar - assigns an Unbounded symbol fields,
                           The difference between this procedure and
                           AssignUnboundedVar is that this procedure cannot
                           set the Unbounded.Address since the data from
                           Sym will be copied because parameter is NOT a VAR
                           parameter.
                           UnboundedSym is not a VAR parameter and therefore
                           this procedure can only complete the HIGH field
                           and not the ADDRESS field.
                           Sym can be a Variable with type Unbounded.
                           Sym can be a Variable with type Array.
                           Sym can be a String Constant.

                           ParamType is the TYPE of the paramater
*)

PROCEDURE AssignUnboundedNonVar (Sym, UnboundedSym, ParamType: CARDINAL) ;
VAR
   Field,
   AddressField,
   Type        : CARDINAL ;
BEGIN
   IF IsConst(Sym)  (* was IsConstString(Sym) *)
   THEN
      UnboundedNonVarLinkToArray(Sym, UnboundedSym, ParamType)
   ELSIF IsVar(Sym)
   THEN
      Type := GetType(Sym) ;
      IF IsUnbounded(Type)
      THEN
         UnboundedNonVarLinkToArray(Sym, UnboundedSym, ParamType)
      ELSIF IsArray(Type) OR (ParamType=Word) OR (ParamType=Byte)
      THEN
         UnboundedNonVarLinkToArray(Sym, UnboundedSym, ParamType)
      ELSE
         WriteFormat1('illegal type parameter %a: expecting Array or Dynamic Array', GetSymName(Sym)) ;
      END
   ELSE
      WriteFormat1('illegal parameter %a which cannot be passed as: VAR ARRAY OF Type', GetSymName(Sym))
   END
END AssignUnboundedNonVar ;


(*
   UnboundedNonVarLinkToArray - links an array, ArraySym, to an unbounded array,
                                UnboundedSym. The parameter is a NON VAR
                                variety.
*)

PROCEDURE UnboundedNonVarLinkToArray (ArraySym, UnboundedSym, ParamType: CARDINAL) ;
VAR
   t, f,
   AddressField,
   ArrayAdr,
   Field       : CARDINAL ;
BEGIN
   (* Unbounded.ArrayAddress := ??? runtime *)
   PushTF(UnboundedSym, GetType(UnboundedSym)) ;
   Field := GetLocalSym(Unbounded, ArrayAddress) ;
   PushTF(Field, GetType(Field)) ;
   PushT(1) ;
   BuildDesignatorRecord ;
   PopT(AddressField) ;

   (* caller saves non var unbounded array contents *)
   GenQuad(UnboundedOp, AddressField, NulSym, ArraySym) ;

   (* Unbounded.ArrayHigh := HIGH(ArraySym)   *)
   PushTF(UnboundedSym, GetType(UnboundedSym)) ;
   Field := GetLocalSym(Unbounded, ArrayHigh) ;
   PushTF(Field, GetType(Field)) ;
   PushT(1) ;
   BuildDesignatorRecord ;
   IF (ParamType=Byte) OR (ParamType=Word)
   THEN
      (* SIZE(parameter) DIV TSIZE(ParamType)                   *)
      PushTF(TSize, Cardinal) ;  (* TSIZE(GetType(ArraySym))    *)
      PushT(GetType(ArraySym)) ;
      PushT(1) ;                (* 1 parameter for TSIZE()      *)
      BuildFunctionCall ;
      PopTF(t, f) ;
      PushTF(t, Cardinal) ;
      PushT(DivTok) ;           (* Divide by                    *)
      PushTF(TSize, Cardinal) ; (* TSIZE(ParamType)             *)
      PushT(ParamType) ;
      PushT(1) ;                (* 1 parameter for TSIZE()      *)
      BuildFunctionCall ;
      BuildBinaryOp
   ELSE
      PushTF(High, Cardinal) ;  (* HIGH(ArraySym)               *)
      PushT(ArraySym) ;
      PushT(1) ;                (* 1 parameter for HIGH()       *)
      BuildFunctionCall
   END ;
   BuildAssignmentWithoutBounds
END UnboundedNonVarLinkToArray ;


(*
   UnboundedVarLinkToArray - links an array, ArraySym, to an unbounded array,
                             UnboundedSym. The parameter is a VAR variety.
*)

PROCEDURE UnboundedVarLinkToArray (ArraySym, UnboundedSym, ParamType: CARDINAL) ;
VAR
   Field: CARDINAL ;
BEGIN
   (* Unbounded.ArrayAddress := ADR(Sym) *)
   PushTF(UnboundedSym, GetType(UnboundedSym)) ;
   Field := GetLocalSym(Unbounded, ArrayAddress) ;
   PushTF(Field, GetType(Field)) ;
   PushT(1) ;
   BuildDesignatorRecord ;
   PushTF(Adr, Address) ;   (* ADR(ArraySym)                *)
   PushT(ArraySym) ;
   PushT(1) ;               (* 1 parameter for ADR()        *)
   BuildFunctionCall ;
   BuildAssignmentWithoutBounds ;
   (* Unbounded.ArrayHigh := HIGH(ArraySym)   *)
   PushTF(UnboundedSym, GetType(UnboundedSym)) ;
   Field := GetLocalSym(Unbounded, ArrayHigh) ;
   PushTF(Field, GetType(Field)) ;
   PushT(1) ;
   BuildDesignatorRecord ;
   IF (ParamType=Byte) OR (ParamType=Word)
   THEN
      (* SIZE(parameter) DIV TSIZE(ParamType)                   *)
      PushTF(TSize, Cardinal) ;  (* TSIZE(GetType(ArraySym))    *)
      PushT(GetType(ArraySym)) ;
      PushT(1) ;                (* 1 parameter for TSIZE()      *)
      BuildFunctionCall ;
      PushT(DivTok) ;           (* Divide by                    *)
      PushTF(TSize, Cardinal) ; (* TSIZE(ParamType)             *)
      PushT(ParamType) ;
      PushT(1) ;                (* 1 parameter for TSIZE()      *)
      BuildFunctionCall ;
      BuildBinaryOp
   ELSE
      PushTF(High, Cardinal) ;  (* HIGH(ArraySym)               *)
      PushT(ArraySym) ;
      PushT(1) ;                (* 1 parameter for HIGH()       *)
      BuildFunctionCall
   END ;
   BuildAssignmentWithoutBounds
END UnboundedVarLinkToArray ;


(*
   BuildPseudoProcedureCall - builds a pseudo procedure call.
                              This procedure does not directly alter the
                              stack, but by calling routines the stack
                              will change in the following way when this
                              procedure returns.

                              The Stack:


                              Entry                      Exit

                       Ptr ->
                              +----------------+
                              | NoOfParam      |
                              |----------------|
                              | Param 1        |
                              |----------------|
                              | Param 2        |
                              |----------------|
                              .                .
                              .                .
                              .                .
                              |----------------|
                              | Param #        |
                              |----------------|
                              | ProcSym | Type |         Empty
                              |----------------|
*)

PROCEDURE BuildPseudoProcedureCall ;
VAR
   NoOfParam,
   ProcSym,
   Ptr      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   PushT(NoOfParam) ;
   (* Compile time stack restored to entry state *)
   IF ProcSym=New
   THEN
      BuildNewProcedure
   ELSIF ProcSym=Dispose
   THEN
      BuildDisposeProcedure
   ELSIF ProcSym=Inc
   THEN
      BuildIncProcedure
   ELSIF ProcSym=Dec
   THEN
      BuildDecProcedure
   ELSIF ProcSym=Incl
   THEN
      BuildInclProcedure
   ELSIF ProcSym=Excl
   THEN
      BuildExclProcedure
   ELSE
      InternalError('pseudo procedure not implemented yet', __FILE__, __LINE__)
   END
END BuildPseudoProcedureCall ;


(*
   IsReallyAPointer - returns true if Sym is really a pointer.
*)

PROCEDURE IsReallyAPointer (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF Sym=NulSym
   THEN
      RETURN( FALSE )
   ELSIF IsPointer(Sym)
   THEN
      RETURN( TRUE )
   ELSIF IsVar(Sym) OR IsType(Sym)
   THEN
      RETURN( IsReallyAPointer(GetType(Sym)) )
   ELSE
      RETURN( FALSE )
   END
END IsReallyAPointer ;


(*
   GetItemPointedTo - returns the symbol type that is being pointed to
                      by Sym.
*)

PROCEDURE GetItemPointedTo (Sym: CARDINAL) : CARDINAL ;
BEGIN
   IF IsPointer(Sym)
   THEN
      RETURN( GetType(Sym) )
   ELSIF IsVar(Sym) OR IsType(Sym)
   THEN
      RETURN( GetItemPointedTo(GetType(Sym)) )
   END
END GetItemPointedTo ;


(*
   BuildNewProcedure - builds the pseudo procedure call NEW.
                       This procedure is traditionally a "macro" for
                       NEW(x) --> ALLOCATE(x, SIZE(x^))
                       One method of implementation is to emulate a "macro"
                       processor by pushing the relevant input tokens
                       back onto the input stack.
                       However this causes two problems:

                       (i)  Unnecessary code is produced for x^
                       (ii) SIZE must be imported from SYSTEM
                       Therefore we chose an alternative method of
                       implementation;
                       generate quadruples for ALLOCATE(x, SIZE(x^))
                       this, although slightly more efficient,
                       is more complex and circumvents problems (i) and (ii).

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |
                       |----------------|
                       | ProcSym | Type |         Empty
                       |----------------|
*)
   
PROCEDURE BuildNewProcedure ;
VAR
   NoOfParam,
   SizeSym,
   PtrSym,
   ProcSym,
   Ptr      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      ProcSym := RequestSym(MakeKey('ALLOCATE')) ;
      IF (ProcSym#NulSym) AND IsProcedure(ProcSym)
      THEN
         PtrSym := OperandT(1) ;
         IF IsReallyAPointer(PtrSym)
         THEN
            (*
               Build macro: ALLOCATE( PtrSym, SIZE(PtrSym^) )
            *)
            PushT(TSize) ;           (* Procedure      *)
                                     (* x^             *)
            PushT(GetItemPointedTo(PtrSym)) ;
            PushT(1) ;               (* One parameter  *)
            BuildTSizeFunction ;
            PopT(SizeSym) ;

            PushT(ProcSym) ;         (* ALLOCATE       *)
            PushT(PtrSym) ;          (* x              *)
            PushT(SizeSym) ;         (* TSIZE(x^)      *)
            PushT(2) ;               (* Two parameters *)
            BuildProcedureCall

         ELSE
            WriteFormat0('argument to NEW must be a pointer')
         END
      ELSE
         WriteFormat0('ALLOCATE procedure not found for NEW substitution')
      END
   ELSE
      WriteFormat0('the pseudo procedure NEW only has one parameter')
   END ;
   PopN(NoOfParam+1)
END BuildNewProcedure ;


(*
   BuildDisposeProcedure - builds the pseudo procedure call DISPOSE.
                           This procedure is traditionally a "macro" for
                           DISPOSE(x) --> DEALLOCATE(x, SIZE(x^))
                           One method of implementation is to emulate a "macro"
                           processor by pushing the relevant input tokens
                           back onto the input stack.
                           However this causes two problems:

                           (i)  Unnecessary code is produced for x^
                           (ii) SIZE must be imported from SYSTEM
                           Therefore we chose an alternative method of
                           implementation;
                           generate quadruples for DEALLOCATE(x, SIZE(x^))
                           this, although slightly more efficient,
                           is more complex and circumvents problems (i)
                           and (ii).

                           The Stack:


                           Entry                      Exit

                    Ptr ->
                           +----------------+
                           | NoOfParam      |
                           |----------------|
                           | Param 1        |
                           |----------------|
                           | Param 2        |
                           |----------------|
                           .                .
                           .                .
                           .                .
                           |----------------|
                           | Param #        |
                           |----------------|
                           | ProcSym | Type |         Empty
                           |----------------|
*)

PROCEDURE BuildDisposeProcedure ;
VAR
   NoOfParam,
   SizeSym,
   PtrSym,
   ProcSym,
   Ptr      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      ProcSym := RequestSym(MakeKey('DEALLOCATE')) ;
      IF (ProcSym#NulSym) AND IsProcedure(ProcSym)
      THEN
         PtrSym := OperandT(1) ;
         IF IsReallyAPointer(PtrSym)
         THEN
            (*
               Build macro: DEALLOCATE( PtrSym, SIZE(PtrSym^) )
            *)
            PushT(TSize) ;           (* Procedure      *)
                                     (* x^             *)
            PushT(GetItemPointedTo(PtrSym)) ;
            PushT(1) ;               (* One parameter  *)
            BuildTSizeFunction ;
            PopT(SizeSym) ;

            PushT(ProcSym) ;         (* DEALLOCATE     *)
            PushT(PtrSym) ;          (* x              *)
            PushT(SizeSym) ;         (* TSIZE(x^)      *)
            PushT(2) ;               (* Two parameters *)
            BuildProcedureCall

         ELSE
            WriteFormat0('argument to DISPOSE must be a pointer')
         END
      ELSE
         WriteFormat0('DEALLOCATE procedure not found for DISPOSE substitution')
      END
   ELSE
      WriteFormat0('the pseudo procedure DISPOSE only has one parameter')
   END ;
   PopN(NoOfParam+1)
END BuildDisposeProcedure ;


(*
   BuildIncProcedure - builds the pseudo procedure call INC.
                       INC is a procedure which increments a variable.
                       It takes one or two parameters:
                       INC(a, b)  or  INC(a)
                       a := a+b   or  a := a+1

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |
                       |----------------|
                       | ProcSym | Type |         Empty
                       |----------------|
*)

PROCEDURE BuildIncProcedure ;
VAR
   NoOfParam,
   OperandSym,
   VarSym,
   TempSym,
   ProcSym,
   Ptr       : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF (NoOfParam=1) OR (NoOfParam=2)
   THEN
      VarSym := OperandT(NoOfParam) ;  (* bottom/first parameter *)
      IF IsVar(VarSym)
      THEN
         TempSym := MakeTemporary(RightValue) ;
         PutVar(TempSym, GetType(VarSym)) ;
         IF NoOfParam=2
         THEN
            OperandSym := OperandT(1)
         ELSE
            OperandSym := MakeConstLit(MakeKey('1'))
         END ;

         PushT(TempSym) ;
         PushT(VarSym) ;
         BuildAssignmentWithoutBounds ;

         IF IsPointer(GetType(TempSym)) AND (NoOfParam=1)
         THEN
            (* if the user uses two parameters then we insist that the user coerses *)
            PushTF(TempSym, Integer)   (* no coorsion for pointers *)
         ELSE
            PushTF(TempSym, GetType(TempSym))
         END ;

         PushT(PlusTok) ;
         PushTF(OperandSym, GetType(OperandSym)) ;
         BuildBinaryOp ;
         PopT(TempSym) ;

         PutVar(TempSym, GetType(VarSym)) ;   (* now alter the type of the temporary *)
         PushT(VarSym) ;
         PushT(TempSym) ;   (* inefficient, but understandable *)
         BuildAssignment
      ELSE
         ExpectVariable('base procedure INC expects a variable as a parameter',
                        VarSym)
      END
   ELSE
      WriteFormat0('base procedure INC expects 1 or 2 parameters')
   END ;
   PopN(NoOfParam+1)
END BuildIncProcedure ;


(*
   BuildDecProcedure - builds the pseudo procedure call DEC.
                       DEC is a procedure which decrements a variable.
                       It takes one or two parameters:
                       DEC(a, b)  or  DEC(a)
                       a := a-b   or  a := a-1

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |
                       |----------------|
                       | ProcSym | Type |         Empty
                       |----------------|
*)

PROCEDURE BuildDecProcedure ;
VAR
   NoOfParam,
   OperandSym,
   VarSym,
   TempSym,
   ProcSym   : CARDINAL ;
BEGIN
   DumpStack ;
   PopT(NoOfParam) ;
   IF (NoOfParam=1) OR (NoOfParam=2)
   THEN
      VarSym := OperandT(NoOfParam) ;   (* bottom/first parameter *)
      IF IsVar(VarSym)
      THEN
         TempSym := MakeTemporary(RightValue) ;
         PutVar(TempSym, GetType(VarSym)) ;
         IF NoOfParam=2
         THEN
            OperandSym := OperandT(1)
         ELSE
            OperandSym := MakeConstLit(MakeKey('1'))
         END ;

         PushT(TempSym) ;
         PushT(VarSym) ;
         BuildAssignmentWithoutBounds ;
         DumpStack ;

         IF IsPointer(GetType(TempSym)) AND (NoOfParam=1)
         THEN
            (* if the user uses two parameters then we insist that the user coerses *)
            PushTF(TempSym, Integer)   (* no coorsion for pointers *)
         ELSE
            PushTF(TempSym, GetType(TempSym))
         END ;
         DumpStack ;

         PushT(MinusTok) ;
         PushTF(OperandSym, GetType(OperandSym)) ;
         BuildBinaryOp ;
         PopT(TempSym) ;
         DumpStack ;

         PutVar(TempSym, GetType(VarSym)) ;   (* now alter the type of the temporary *)
         PushT(VarSym) ;
         PushT(TempSym) ;   (* inefficient, but understandable *)
         BuildAssignment
      ELSE
         ExpectVariable('base procedure DEC expects a variable as a parameter',
                        VarSym)
      END
   ELSE
      WriteFormat0('base procedure DEC expects 1 or 2 parameters')
   END ;
   DumpStack ;
   PopN(NoOfParam+1) ;
   DumpStack ;
END BuildDecProcedure ;


(*
   DereferenceLValue - checks to see whether, operand, is declare as an LValue
                       and if so it dereferences it.
*)

PROCEDURE DereferenceLValue (operand: CARDINAL) : CARDINAL ;
VAR
   sym: CARDINAL ;
BEGIN
   IF GetMode(operand)=LeftValue
   THEN
      (* dereference the pointer *)
      sym := MakeTemporary(AreConstant(IsConst(operand))) ;
      PutVar(sym, GetType(operand)) ;

      PushT(sym) ;
      PushT(operand) ;
      BuildAssignmentWithoutBounds ;
      RETURN( sym )
   ELSE
      RETURN( operand )
   END
END DereferenceLValue ;


(*
   BuildInclProcedure - builds the pseudo procedure call INCL.
                        INCL is a procedure which adds bit b into a BITSET a.
                        It takes two parameters:
                        INCL(a, b)

                        a := a + {b}

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildInclProcedure ;
VAR
   NoOfParam,
   OperandSym,
   VarSym,
   ProcSym   : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT(2) ;
      OperandSym := OperandT(1) ;
      IF IsVar(VarSym)
      THEN
         IF IsSet(GetType(VarSym))
         THEN
            GenQuad(InclOp, VarSym, NulSym, DereferenceLValue(OperandSym))
         ELSE
            ExpectVariable('the first parameter to INCL must be a SET variable',
                           VarSym)
         END
      ELSE
         WriteFormat0('base procedure INCL expects a variable as a parameter')
      END
   ELSE
      WriteFormat0('base procedure INCL expects 2 parameters')
   END ;
   PopN(NoOfParam+1)
END BuildInclProcedure ;


(*
   BuildExclProcedure - builds the pseudo procedure call EXCL.
                        INCL is a procedure which removes bit b from SET a.
                        It takes two parameters:
                        EXCL(a, b)

                        a := a - {b}

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildExclProcedure ;
VAR
   NoOfParam,
   OperandSym,
   VarSym,
   ProcSym   : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT(2) ;
      OperandSym := OperandT(1) ;
      IF IsVar(VarSym)
      THEN
         IF IsSet(GetType(VarSym))
         THEN
            GenQuad(ExclOp, VarSym, NulSym, DereferenceLValue(OperandSym))
         ELSE
            ExpectVariable('the first parameter to EXCL must be a SET variable',
                           VarSym)
         END
      ELSE
         WriteFormat0('base procedure EXCL expects a variable as a parameter')
      END
   ELSE
      WriteFormat0('base procedure EXCL expects 2 parameters')
   END ;
   PopN(NoOfParam+1)
END BuildExclProcedure ;


(*
   BuildFunctionCall - builds a function call.
                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |                        <- Ptr
                       |----------------|         +------------+
                       | ProcSym | Type |         | ReturnVar  |
                       |----------------|         |------------|

*)

PROCEDURE BuildFunctionCall ;
VAR
   NoOfParam,
   ProcSym,
   Ptr      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   PushT(NoOfParam) ;
   (* Compile time stack restored to entry state *)
   IF IsUnknown(ProcSym)
   THEN
      WriteFormat1('function %a is undefined', GetSymName(ProcSym)) ;
      PopN(NoOfParam+2) ;
      PushT(MakeConstLit(MakeKey('0')))   (* fake return value to continue compiling *)
   ELSIF IsAModula2Type(ProcSym)
   THEN
      ManipulatePseudoCallParameters ;
      BuildTypeCoercion
   ELSIF IsPseudoSystemFunction(ProcSym) OR
         IsPseudoBaseFunction(ProcSym)
   THEN
      ManipulatePseudoCallParameters ;
      BuildPseudoFunctionCall
   ELSE
      BuildRealFunctionCall
   END
END BuildFunctionCall ;


(*
   BuildConstFunctionCall - builds a function call and checks that this function can be
                            called inside a ConstExpression.

                            The Stack:


                            Entry                      Exit

                     Ptr ->
                            +----------------+
                            | NoOfParam      |
                            |----------------|
                            | Param 1        |
                            |----------------|
                            | Param 2        |
                            |----------------|
                            .                .
                            .                .
                            .                .
                            |----------------|
                            | Param #        |                        <- Ptr
                            |----------------|         +------------+
                            | ProcSym | Type |         | ReturnVar  |
                            |----------------|         |------------|

*)

PROCEDURE BuildConstFunctionCall ;
VAR
   NoOfParam,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   PushT(NoOfParam) ;
   IF (ProcSym=Cap)  OR (ProcSym=Chr)   OR (ProcSym=Float) OR
      (ProcSym=High)  OR (ProcSym=High)  OR (ProcSym=LengthS) OR (ProcSym=Max) OR 
      (ProcSym=Min)   OR (ProcSym=Odd)   OR (ProcSym=Ord) OR
      (ProcSym=Size)  OR (ProcSym=Trunc) OR (ProcSym=Val)
   THEN
      BuildFunctionCall
   ELSE
      WriteFormat0('the only functions permissible in a constant expression are: CAP, CHR, FLOAT, HIGH, LENGTH, MAX, MIN, ODD, ORD, SIZE, TRUNC and VAL') ;
      PopN(NoOfParam+2) ;
      PushT(MakeConstLit(MakeKey('0')))   (* fake return value to continue compiling *)
   END
END BuildConstFunctionCall ;


(*
   BuildTypeCoercion - builds the type coersion.
                       MODULA-2 allows types to be coersed with no runtime
                       penility.
                       It insists that the TSIZE(t1)=TSIZE(t2) where
                       t2 variable := t2(variable of type t1).
                       The ReturnVar on the stack is of type t2.

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |  
                       |----------------|
                       | Param 1        |  
                       |----------------|
                       | Param 2        |  
                       |----------------|
                       .                .  
                       .                .  
                       .                .  
                       |----------------|
                       | Param #        |                        <- Ptr
                       |----------------|         +------------+
                       | ProcSym | Type |         | ReturnVar  |
                       |----------------|         |------------|

                       Quadruples:

                       CoerceOp  ReturnVar  Type  Param1

                       A type coercion will only be legal if the different
                       types have exactly the same size.
                       Since we can only decide this after M2Eval has processed
                       the symbol table then we create a quadruple explaining
                       the coercion taking place, the code generator can test
                       this assertion and report an error if the type sizes
                       differ.
*)

PROCEDURE BuildTypeCoercion ;
VAR
   NoOfParam,
   ReturnVar,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   IF NOT IsAModula2Type(ProcSym)
   THEN
      WriteFormat0('coersion expecting a type')
   END ;
   ReturnVar := MakeTemporary(RightValue) ;
   PutVar(ReturnVar, ProcSym) ;  (* Set ReturnVar's TYPE *)
   IF NoOfParam=1
   THEN
      IF IsConst(OperandT(1)) OR IsVar(OperandT(1))
      THEN
         GenQuad(CoerceOp, ReturnVar, ProcSym, OperandT(1)) ;
         PopN(NoOfParam+1) ;
         PushTF(ReturnVar, ProcSym)
      ELSE
         ErrorStringAt2(Sprintf2(Mark(InitString('trying to coerse (%s) which is not a variable or constant into (%s)')),
                                 Mark(InitStringCharStar(KeyToCharStar(GetSymName(OperandT(1))))),
                                 Mark(InitStringCharStar(KeyToCharStar(GetSymName(ProcSym))))),
                        GetTokenNo(), GetDeclared(OperandT(1))) ;
         PopN(NoOfParam+1)
      END
   ELSE
      WriteFormat0('only one parameter expected in a TYPE coersion')
   END
END BuildTypeCoercion ;


(*
   BuildRealFunctionCall - builds a function call.
                           The Stack:


                           Entry                      Exit

                    Ptr ->
                           +----------------+
                           | NoOfParam      |
                           |----------------|
                           | Param 1        |
                           |----------------|
                           | Param 2        |
                           |----------------|
                           .                .
                           .                .
                           .                .
                           |----------------|
                           | Param #        |                        <- Ptr
                           |----------------|         +------------+
                           | ProcSym | Type |         | ReturnVar  |
                           |----------------|         |------------|
*)

PROCEDURE BuildRealFunctionCall ;
VAR
   NoOfParam,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   PushT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+2) ;
   IF IsVar(ProcSym)
   THEN
      (* Procedure Variable ? *)
      ProcSym := OperandF(NoOfParam+2)
   END ;
   IF IsDefImp(GetScope(ProcSym)) AND IsDefinitionForC(GetScope(ProcSym))
   THEN
      BuildRealFuncProcCall(TRUE, TRUE)
   ELSE
      BuildRealFuncProcCall(TRUE, FALSE)
   END
END BuildRealFunctionCall ;


(*
   BuildPseudoFunctionCall - builds the pseudo function
                             The Stack:


                             Entry                      Exit

                      Ptr ->
                             +----------------+
                             | NoOfParam      |  
                             |----------------|
                             | Param 1        |  
                             |----------------|
                             | Param 2        |  
                             |----------------|
                             .                .  
                             .                .  
                             .                .  
                             |----------------|
                             | Param #        |                        <- Ptr
                             |----------------|         +------------+
                             | ProcSym | Type |         | ReturnVar  |
                             |----------------|         |------------|

*)

PROCEDURE BuildPseudoFunctionCall ;
VAR
   NoOfParam,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   PushT(NoOfParam) ;
   (* Compile time stack restored to entry state *)
   IF ProcSym=High
   THEN
      BuildHighFunction
   ELSIF ProcSym=LengthS
   THEN
      BuildLengthFunction
   ELSIF ProcSym=Adr
   THEN
      BuildAdrFunction
   ELSIF ProcSym=Size
   THEN
      BuildSizeFunction
   ELSIF ProcSym=TSize
   THEN
      BuildTSizeFunction
   ELSIF ProcSym=Convert
   THEN
      BuildConvertFunction
   ELSIF ProcSym=Odd
   THEN
      BuildOddFunction
   ELSIF ProcSym=Abs
   THEN
      BuildAbsFunction
   ELSIF ProcSym=Cap
   THEN
      BuildCapFunction
   ELSIF ProcSym=Val
   THEN
      BuildValFunction
   ELSIF ProcSym=Chr
   THEN
      BuildChrFunction
   ELSIF ProcSym=Ord
   THEN
      BuildOrdFunction
   ELSIF ProcSym=Trunc
   THEN
      BuildTruncFunction
   ELSIF ProcSym=Float
   THEN
      BuildFloatFunction
   ELSIF ProcSym=Min
   THEN
      BuildMinFunction
   ELSIF ProcSym=Max
   THEN
      BuildMaxFunction
   ELSIF ProcSym=AddAdr
   THEN
      BuildAddAdrFunction
   ELSIF ProcSym=SubAdr
   THEN
      BuildSubAdrFunction
   ELSIF ProcSym=DifAdr
   THEN
      BuildDifAdrFunction
   ELSIF ProcSym=Cast
   THEN
      BuildCastFunction
   ELSIF ProcSym=MakeAdr
   THEN
      BuildMakeAdrFunction
   ELSE
      InternalError('pseudo function not implemented yet', __FILE__, __LINE__)
   END
END BuildPseudoFunctionCall ;


(*
   BuildAddAdrFunction - builds the pseudo procedure call ADDADR.

                         PROCEDURE ADDADR (addr: ADDRESS; offset: CARDINAL): ADDRESS ;

                         Which returns address given by (addr + offset),
                         [ the standard says that it _may_
                           "raise an exception if this address is not valid."
                           currently we do not generate any exception code ]

                         The Stack:

                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |
                         |----------------|
                         | Param 2        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|
*)

PROCEDURE BuildAddAdrFunction ;
VAR
   ReturnVar,
   NoOfParam,
   OperandSym,
   VarSym    : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT(2) ;
      OperandSym := OperandT(1) ;
      PopN(NoOfParam+1) ;
      IF IsVar(VarSym)
      THEN
         IF IsReallyPointer(VarSym) OR (GetType(VarSym)=Address)
         THEN
            ReturnVar := MakeTemporary(RightValue) ;
            PutVar(ReturnVar, Address) ;
            GenQuad(AddOp, ReturnVar, VarSym, DereferenceLValue(OperandSym)) ;
            PushTF(ReturnVar, Address)
         ELSE
            ExpectVariable('the first parameter to ADDADR must be a variable of type ADDRESS or a POINTER',
                           VarSym) ;
            PushTF(MakeConstLit(MakeKey('0')), Address)
         END
      ELSE
         WriteFormat0('SYSTEM procedure ADDADR expects a variable which has a type of ADDRESS or is a POINTER as its first parameter') ;
         PushTF(MakeConstLit(MakeKey('0')), Address)
      END
   ELSE
      WriteFormat0('SYSTEM procedure ADDADR expects 2 parameters') ;
      PopN(NoOfParam+1) ;
      PushTF(MakeConstLit(MakeKey('0')), Address)
   END
END BuildAddAdrFunction ;


(*
   BuildSubAdrFunction - builds the pseudo procedure call ADDADR.

                         PROCEDURE SUBADR (addr: ADDRESS; offset: CARDINAL): ADDRESS ;

                         Which returns address given by (addr - offset),
                         [ the standard says that it _may_
                           "raise an exception if this address is not valid."
                           currently we do not generate any exception code ]

                         The Stack:

                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |
                         |----------------|
                         | Param 2        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|
*)

PROCEDURE BuildSubAdrFunction ;
VAR
   ReturnVar,
   NoOfParam,
   OperandSym,
   VarSym    : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT(2) ;
      OperandSym := OperandT(1) ;
      PopN(NoOfParam+1) ;
      IF IsVar(VarSym)
      THEN
         IF IsReallyPointer(VarSym) OR (GetType(VarSym)=Address)
         THEN
            ReturnVar := MakeTemporary(RightValue) ;
            PutVar(ReturnVar, Address) ;
            GenQuad(SubOp, ReturnVar, VarSym, DereferenceLValue(OperandSym)) ;
            PushTF(ReturnVar, Address)
         ELSE
            ExpectVariable('the first parameter to SUBADR must be a variable of type ADDRESS or a POINTER',
                           VarSym) ;
            PushTF(MakeConstLit(MakeKey('0')), Address)
         END
      ELSE
         WriteFormat0('SYSTEM procedure SUBADR expects a variable which has a type of ADDRESS or is a POINTER as its first parameter') ;
         PushTF(MakeConstLit(MakeKey('0')), Address)
      END
   ELSE
      WriteFormat0('SYSTEM procedure SUBADR expects 2 parameters') ;
      PopN(NoOfParam+1) ;
      PushTF(MakeConstLit(MakeKey('0')), Address)
   END
END BuildSubAdrFunction ;


(*
   BuildDifAdrFunction - builds the pseudo procedure call DIFADR.

                         PROCEDURE DIFADR (addr1, addr2: ADDRESS): ADDRESS ;

                         Which returns address given by (addr1 - addr2),
                         [ the standard says that it _may_
                           "raise an exception if this address is invalid or
                            address space is non-contiguous."
                           currently we do not generate any exception code ]

                         The Stack:

                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |
                         |----------------|
                         | Param 2        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|
*)

PROCEDURE BuildDifAdrFunction ;
VAR
   ReturnVar,
   NoOfParam,
   OperandSym,
   VarSym    : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT(2) ;
      OperandSym := OperandT(1) ;
      PopN(NoOfParam+1) ;
      IF IsVar(VarSym)
      THEN
         IF IsReallyPointer(VarSym) OR (GetType(VarSym)=Address)
         THEN
            IF IsReallyPointer(OperandSym) OR (GetType(OperandSym)=Address)
            THEN
               ReturnVar := MakeTemporary(RightValue) ;
               PutVar(ReturnVar, Address) ;
               GenQuad(SubOp, ReturnVar, VarSym, DereferenceLValue(OperandSym)) ;
               PushTF(ReturnVar, Address)
            ELSE
               ExpectVariable('the second parameter to ADDADR must be a variable of type ADDRESS or a POINTER',
                              OperandSym) ;
               PushTF(MakeConstLit(MakeKey('0')), Address)
            END
         ELSE
            ExpectVariable('the first parameter to ADDADR must be a variable of type ADDRESS or a POINTER',
                           VarSym) ;
            PushTF(MakeConstLit(MakeKey('0')), Address)
         END
      ELSE
         WriteFormat0('SYSTEM procedure ADDADR expects a variable which has a type of ADDRESS or is a POINTER as its first parameter') ;
         PushTF(MakeConstLit(MakeKey('0')), Address)
      END
   ELSE
      WriteFormat0('SYSTEM procedure ADDADR expects 2 parameters') ;
      PopN(NoOfParam+1) ;
      PushTF(MakeConstLit(MakeKey('0')), Address)
   END
END BuildDifAdrFunction ;


(*
   BuildHighFunction - checks the stack in preparation for generating
                       quadruples which perform HIGH.
                       This procedure does not alter the stack but
                       determines whether, a, in HIGH(a) is an ArraySym
                       or UnboundedSym.
                       Both cases are different and appropriate quadruple
                       generating routines are called.

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |  
                       |----------------|
                       | Param 1        |  
                       |----------------|
                       | Param 2        |  
                       |----------------|
                       .                .  
                       .                .  
                       .                .  
                       |----------------|
                       | Param #        |                        <- Ptr
                       |----------------|         +------------+
                       | ProcSym | Type |         | ReturnVar  |
                       |----------------|         |------------|

*)

PROCEDURE BuildHighFunction ;
VAR
   Type,
   NoOfParam,
   Param,
   ReturnVar: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   Param := OperandT(1) ;
   (* Restore stack to origional form *)
   PushT(NoOfParam) ;
   Type  := GetType(Param) ;  (* get the type from the symbol, not the stack *)
(*
   ; WriteString('Attempting to build HIGH(') ; WriteKey(GetSymName(Param)) ;
   ; WriteString(')') ; WriteLn ;
*)
   IF NoOfParam#1
   THEN
      WriteFormat0('base procedure HIGH expects 1 parameter')
   ELSIF (NOT IsVar(Param)) AND (NOT IsConstString(Param)) AND (NOT (IsConst(Param) AND (GetType(Param)=Char)))
   THEN
      WriteFormat0('base procedure HIGH expects a variable or string constant as its parameter')
   ELSIF IsConst(Param) AND (GetType(Param)=Char)
   THEN
      BuildHighFromChar
   ELSIF IsConstString(Param)
   THEN
      BuildHighFromString
   ELSIF IsArray(Type) OR IsUnbounded(Type)
   THEN
      IF NoOfElements(Type)=1
      THEN
         IF IsArray(Type)
         THEN
            BuildHighFromArray
         ELSIF IsUnbounded(Type)
         THEN
            BuildHighFromUnbounded
         END
      ELSE
         WriteFormat0('not allowed multidemension array parameter for HIGH')
      END
   ELSE
      WriteFormat0('base procedure HIGH expects a variable of type array as its parameter')
   END
END BuildHighFunction ;


(*
   BuildHighFromArray - builds the pseudo function HIGH from an ArraySym.
                        ArraySyms are not dynamic and therefore it can be
                        calculated at compile time since the subscripts
                        are constant.

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |  
                        |----------------|
                        | Param 1        |  
                        |----------------|
                        | Param 2        |  
                        |----------------|
                        .                .  
                        .                .  
                        .                .  
                        |----------------|
                        | Param #        |                        <- Ptr
                        |----------------|         +------------+
                        | ProcSym | Type |         | ReturnVar  |
                        |----------------|         |------------|

*)

PROCEDURE BuildHighFromArray ;
VAR
   NoOfParam,
   ReturnVar: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ReturnVar := MakeTemporary(ImmediateValue) ;
   Assert(NoOfParam=1) ;
   GenQuad(HighOp, ReturnVar, NulSym, OperandT(1)) ;
   PopN(NoOfParam+1) ;
   PushT(ReturnVar)
END BuildHighFromArray ;


(*
   BuildHighFromUnbounded - builds the pseudo function HIGH from an
                            UnboundedSym.
                            This is achieved by a hidden record access.
                            HIGH(a) is performed by a._ArrayHigh and therefore
                            the compile time stack is manipulated to be
                            in the form for a record access,
                            BuildDesignatorRecord is called and the stack
                            is finally left in the form shown below.

                            The Stack:


                            Entry                      Exit

                     Ptr ->
                            +----------------+
                            | NoOfParam      |  
                            |----------------|
                            | Param 1        |  
                            |----------------|
                            | Param 2        |  
                            |----------------|
                            .                .  
                            .                .  
                            .                .  
                            |----------------|
                            | Param #        |                        <- Ptr
                            |----------------|         +------------+
                            | ProcSym | Type |         | ReturnVar  |
                            |----------------|         |------------|

*)

PROCEDURE BuildHighFromUnbounded ;
VAR
   NoOfParam,
   Param1,
   ReturnVar: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   Param1 := OperandT(1) ;
   (*
      Build record access: Param1._ArrayHigh
   *)
   Assert(NoOfParam=1) ;
   PopN(NoOfParam+1) ;
   PushTF(Param1, GetType(Param1)) ;
   PushTF(GetLocalSym(Unbounded, ArrayHigh),
          GetType(GetLocalSym(Unbounded, ArrayHigh))) ;
   PushT(1) ;             (* 1 field for Param1._ArrayHigh *)
   BuildDesignatorRecord  (* Now the generate quadruples   *)
END BuildHighFromUnbounded ;


(*
   BuildHighFromString - builds the pseudo function HIGH from an StringSym.
                         Constant Strings are not dynamic and therefore it can
                         be calculated at compile time since the subscripts
                         are constant.

                         The Stack:


                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |  
                         |----------------|
                         | Param 1        |  
                         |----------------|
                         | Param 2        |  
                         |----------------|
                         .                .  
                         .                .  
                         .                .  
                         |----------------|
                         | Param #        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|

*)

PROCEDURE BuildHighFromString ;
VAR
   High,
   NoOfParam,
   ReturnVar: CARDINAL ;
   s        : String ;
BEGIN
   PopT(NoOfParam) ;
   Assert(NoOfParam=1) ;
   High := GetStringLength(OperandT(1)) ;
   IF High>0
   THEN
      DEC(High)
   END ;
   s := Sprintf1(Mark(InitString("%d")), High) ;
   ReturnVar := MakeConstLit(makekey(string(s))) ;
   s := KillString(s) ;
   PopN(NoOfParam+1) ;
   PushT(ReturnVar)
END BuildHighFromString ;


(*
   BuildHighFromChar - builds the pseudo function HIGH from a character constant.
                       Character constants can be passed to ARRAY OF CHAR and thus
                       we need to calculate HIGH when we construct an unbounded sym.
                       This function simply manipulates the stack and places
                       constant 0 on the stack. HIGH(any char)=0

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |  
                       |----------------|
                       | Param 1        |  
                       |----------------|
                       | Param 2        |  
                       |----------------|
                       .                .  
                       .                .  
                       .                .  
                       |----------------|
                       | Param #        |                        <- Ptr
                       |----------------|         +------------+
                       | ProcSym | Type |         | ReturnVar  |
                       |----------------|         |------------|
*)

PROCEDURE BuildHighFromChar ;
VAR
   NoOfParam,
   ReturnVar: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   Assert(NoOfParam=1) ;
   ReturnVar := MakeConstLit(MakeKey('0')) ;
   PopN(NoOfParam+1) ;
   PushT(ReturnVar)
END BuildHighFromChar ;


(*
   BuildLengthFunction - builds the inline standard function LENGTH.

                         The Stack:


                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |  
                         |----------------|
                         | Param 1        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|

*)

PROCEDURE BuildLengthFunction ;
VAR
   s        : String ;
   Type,
   NoOfParam,
   Param,
   ReturnVar: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   Param := OperandT(1) ;
   (* Restore stack to origional form *)
   PushT(NoOfParam) ;
   Type  := GetType(Param) ;  (* get the type from the symbol, not the stack *)
   IF NoOfParam#1
   THEN
      WriteFormat0('base procedure LENGTH expects 1 parameter')
   END ;
   IF NoOfParam>=1
   THEN
      IF IsConst(Param) AND (GetType(Param)=Char)
      THEN
         PopT(NoOfParam) ;
         PopN(NoOfParam+1) ;
         ReturnVar := MakeConstLit(MakeKey('1')) ;
         PushT(ReturnVar)
      ELSIF IsConstString(Param)
      THEN
         PopT(NoOfParam) ;
         s := Sprintf1(Mark(InitString("%d")), GetStringLength(OperandT(1))) ;
         ReturnVar := MakeConstLit(makekey(string(s))) ;
         s := KillString(s) ;
         PopN(NoOfParam+1) ;
         PushT(ReturnVar)
      ELSE
         WriteFormat0('base procedure LENGTH expects a string constant or character constant as its parameter') ;
         PopT(NoOfParam) ;
         PopN(NoOfParam+1) ;
         ReturnVar := MakeConstLit(MakeKey('0')) ;
         PushT(ReturnVar)
      END
   ELSE
      (* NoOfParam is _very_ wrong, we flush all outstanding errors *)
      FlushErrors
   END
END BuildLengthFunction ;


(*
   BuildOddFunction - builds the pseudo procedure call ODD.
                      This procedure is actually a "macro" for
                      ORD(x) --> VAL(BOOLEAN, x MOD 2)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildOddFunction ;
VAR
   NoOfParam,
   Res, Var : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         PopN(NoOfParam+1) ;
         (*
            Build macro: VAL(BOOLEAN, (x MOD 2))
         *)

         (* compute (x MOD 2) *)
         PushTF(Var, GetType(Var)) ;
         PushT(ModTok) ;
         PushTF(MakeConstLit(MakeKey('2')), Integer) ;
         BuildBinaryOp ;
         PopT(Res) ;

         (* compute IF ...=0 *)
         PushT(Res) ;
         PushT(EqualTok) ;
         PushT(MakeConstLit(MakeKey('0'))) ;
         BuildRelOp ;
         BuildThenIf ;
         
         Res := MakeTemporary(RightValue) ;
         PutVar(Res, Boolean) ;

         PushT(Res) ;
         PushT(False) ;
         BuildAssignment ;
         BuildElse ;
         PushT(Res) ;
         PushT(True) ;
         BuildAssignment ;
         BuildEndIf ;

         PushT(Res)
      ELSE
         WriteFormat0('argument to ODD must be a variable or constant')
      END
   ELSE
      WriteFormat0('the pseudo procedure ODD only has one parameter')
   END
END BuildOddFunction ;


(*
   BuildAbsFunction - builds the pseudo procedure call ABS.

                      ABS(x) --> IF x<0
                                 THEN
                                    RETURN -x
                                 ELSE
                                    RETURN x
                                 END

                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildAbsFunction ;
VAR
   NoOfParam,
   Res, Var : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         PopN(NoOfParam+1) ;

         Res := MakeTemporary(RightValue) ;
         PutVar(Res, GetType(Var)) ;

         (* compute IF x<0 *)
         PushT(Var) ;
         PushT(LessTok) ;
         PushT(MakeConstLit(MakeKey('0'))) ;
         BuildRelOp ;

         BuildThenIf ;
         PushT(Res) ;
         PushT(MinusTok) ;
         PushT(Var) ;
         BuildUnaryOp ;
         BuildAssignment ;
         
         BuildElse ;
         PushT(Res) ;
         PushT(Var) ;
         BuildAssignment ;
         BuildEndIf ;

         PushT(Res)
      ELSE
         WriteFormat0('argument to ABS must be a variable or constant')
      END
   ELSE
      WriteFormat0('the pseudo procedure ABS only has one parameter')
   END
END BuildAbsFunction ;


(*
   BuildCapFunction - builds the pseudo procedure call CAP.

                      CAP(x) --> IF (x>='a') AND (x<='z')
                                 THEN
                                    res := CHR(ORD(x)-ORD('a')+ORD('A'))
                                 ELSE
                                    res := x
                                 END ;
                                 RETURN res

                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildCapFunction ;
VAR
   NoOfParam,
   Res, Var : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         PopN(NoOfParam+1) ;

         Res := MakeTemporary(RightValue) ;
         PutVar(Res, Char) ;

         (* compute IF (x>='a') AND (x<='z') *)
         PushTF(Var, GetType(Var)) ;
         PushT(GreaterEqualTok) ;
         PushTF(MakeConstLitString(MakeKey('a')), Char) ;
         BuildRelOp ;
         
         PushT(AndTok) ;
         RecordOp ;

         PushTF(Var, GetType(Var)) ;
         PushT(LessEqualTok) ;
         PushTF(MakeConstLitString(MakeKey('z')), Char) ;
         BuildRelOp ;
         
         BuildBinaryOp ;

         BuildThenIf ;

         (* compute THEN res := CHR(ORD(x)-ORD('a')+ORD('A')) *)
         PushT(Res) ;
         PushTF(Chr, NulSym) ;

         PushTF(Ord, NulSym) ;
         PushT(Var) ;
         PushT(1) ;
         BuildOrdFunction ;

         PushT(MinusTok) ;
         RecordOp ;

         PushTF(Ord, NulSym) ;
         PushTF(MakeConstLitString(MakeKey('a')), Char) ;
         PushT(1) ;
         BuildOrdFunction ;

         BuildBinaryOp ;

         PushT(PlusTok) ;
         RecordOp ;

         PushTF(Ord, NulSym) ;
         PushTF(MakeConstLitString(MakeKey('A')), Char) ;
         PushT(1) ;
         BuildOrdFunction ;

         BuildBinaryOp ;

         PushT(1) ;
         BuildChrFunction ;

         BuildAssignment ;

         BuildElse ;
         PushT(Res) ;
         PushT(Var) ;
         BuildAssignment ;
         BuildEndIf ;

         PushT(Res)
      ELSE
         WriteFormat0('argument to CAP must be a variable or constant')
      END
   ELSE
      WriteFormat0('the pseudo procedure CAP only has one parameter')
   END
END BuildCapFunction ;


(*
   BuildChrFunction - builds the pseudo procedure call CHR.
                      This procedure is actually a "macro" for
                      CHR(x) --> CONVERT(CHAR, x)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildChrFunction ;
VAR
   NoOfParam,
   Var      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         PopN(NoOfParam+1) ;
         (*
            Build macro: CONVERT( CHAR, Var )
         *)
         PushTF(Convert, NulSym) ;
         PushT(Char) ;
         PushT(Var) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction
      ELSE
         WriteFormat0('argument to CHR must be a variable or constant')
      END
   ELSE
      WriteFormat0('the pseudo procedure CHR only has one parameter')
   END
END BuildChrFunction ;


(*
   BuildOrdFunction - builds the pseudo procedure call ORD.
                      This procedure is actually a "macro" for
                      ORD(x) --> CONVERT(CARDINAL, x)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildOrdFunction ;
VAR
   NoOfParam,
   Var      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         PopN(NoOfParam+1) ;
         (*
            Build macro: CONVERT( CARDINAL, Var )
         *)
         PushTF(Convert, NulSym) ;
         PushT(Cardinal) ;
         PushT(Var) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction
      ELSE
         WriteFormat0('argument to ORD must be a variable or constant')
      END
   ELSE
      WriteFormat0('the pseudo procedure ORD only has one parameter')
   END
END BuildOrdFunction ;


(*
   BuildMakeAdrFunction - builds the pseudo procedure call MAKEADDR.
                          This procedure is actually a "macro" for
                          MAKEADDR(x) --> CONVERT(ADDRESS, CONVERT(WORD, x))
                          However we cannot push tokens back onto the input stack
                          because the compiler is currently building a function
                          call and expecting a ReturnVar on the stack.
                          Hence we manipulate the stack and call
                          BuildConvertFunction.

                          The Stack:


                          Entry                      Exit

                   Ptr ->
                          +----------------+
                          | NoOfParam      |
                          |----------------|
                          | Param 1        |
                          |----------------|
                          | Param 2        |
                          |----------------|
                          .                .
                          .                .
                          .                .
                          |----------------|
                          | Param #        |
                          |----------------|
                          | ProcSym | Type |         Empty
                          |----------------|
*)

PROCEDURE BuildMakeAdrFunction ;
VAR
   NoOfParam,
   Var      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      IF IsVar(Var) OR IsConst(Var)
      THEN
         PopN(NoOfParam+1) ;
         (*
            Build macro: Var := CONVERT( WORD, Var )
         *)
         PushTF(Convert, NulSym) ;
         PushT(Word) ;
         PushT(Var) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction ;

         (*
            Build macro: CONVERT( ADDRESS, Var )
         *)

         PopT(Var) ;
         PushTF(Convert, NulSym) ;
         PushT(Address) ;
         PushT(Var) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction
      ELSE
         WriteFormat0('argument to MAKEADR must be a variable or constant')
      END
   ELSE
      WriteFormat0('the pseudo procedure MAKEADR only has one parameter in GNU Modula-2')
   END
END BuildMakeAdrFunction ;


(*
   BuildValFunction - builds the pseudo procedure call VAL.
                      This procedure is actually a "macro" for
                      VAL(Type, x) --> CONVERT(Type, x)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildValFunction ;
VAR
   NoOfParam,
   Var, Type: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      Type := OperandT(2) ;
      Var := OperandT(1) ;
      IF IsUnknown(Type)
      THEN
         WriteFormat1('undeclared type found in VAL (%a)', GetSymName(Type))
      ELSIF (IsSet(Type) OR IsEnumeration(Type) OR IsSubrange(Type) OR IsType(Type)) AND
         (IsVar(Var) OR IsConst(Var))
      THEN
         PopN(NoOfParam+1) ;
         (*
            Build macro: CONVERT( Type, Var )
         *)
         PushTF(Convert, NulSym) ;
         PushT(Type) ;
         PushT(Var) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction
      ELSE
         WriteFormat0('arguments to VAL must be (Type, Variable or Constant)')
      END
   ELSE
      WriteFormat0('the pseudo procedure VAL has 2 parameters, a Type and Variable')
   END
END BuildValFunction ;


(*
   BuildCastFunction - builds the pseudo procedure call CAST.
                       This procedure is actually a "macro" for
                       CAST(Type, x) --> CONVERT(Type, x)
                       However we cannot push tokens back onto the input stack
                       because the compiler is currently building a function
                       call and expecting a ReturnVar on the stack.
                       Hence we manipulate the stack and call
                       BuildConvertFunction.

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |
                       |----------------|
                       | ProcSym | Type |         Empty
                       |----------------|
*)

PROCEDURE BuildCastFunction ;
VAR
   NoOfParam,
   Var, Type: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=2
   THEN
      Type := OperandT(2) ;
      Var := OperandT(1) ;
      IF IsUnknown(Type)
      THEN
         WriteFormat1('undeclared type found in CAST (%a)', GetSymName(Type))
      ELSIF (IsSet(Type) OR IsEnumeration(Type) OR IsSubrange(Type) OR IsType(Type)) AND
         (IsVar(Var) OR IsConst(Var))
      THEN
         PopN(NoOfParam+1) ;
         (*
            Build macro: CONVERT( Type, Var )
         *)
         PushTF(Convert, NulSym) ;
         PushT(Type) ;
         PushT(Var) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction
      ELSE
         WriteFormat0('arguments to CAST must be (Type, Variable or Constant)')
      END
   ELSE
      WriteFormat0('the pseudo procedure CAST has 2 parameters, a Type and Variable')
   END
END BuildCastFunction ;


(*
   BuildConvertFunction - builds the pseudo function CONVERT.
                          CONVERT( Type, Variable ) ;

                          The Stack:


                          Entry                      Exit

                   Ptr ->
                          +----------------+
                          | NoOfParam      |  
                          |----------------|
                          | Param 1        |  
                          |----------------|
                          | Param 2        |  
                          |----------------|
                          .                .  
                          .                .  
                          .                .  
                          |----------------|
                          | Param #        |                                 <- Ptr
                          |----------------|         +---------------------+
                          | ProcSym | Type |         | ReturnVar | Param1  |
                          |----------------|         |---------------------|

                          Quadruples:

                          ConvertOp  ReturnVar  Param1  Param2

                          Converts variable Param2 into a variable Param1
                          with a type Param1.
*)

PROCEDURE BuildConvertFunction ;
VAR
   t,
   Var, Type,
   NoOfParam,
   ProcSym,
   IntegerVar,
   ReturnVar : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   IF NoOfParam#2
   THEN
      WriteFormat0('base procedure CONVERT expects 2 parameters')
   ELSE
      Type := OperandT(2) ;
      Var := OperandT(1) ;
      IF IsUnknown(Type)
      THEN
         WriteFormat1('undeclared type found in CONVERT (%a)', GetSymName(Type))
      ELSIF IsUnknown(Var)
      THEN
         WriteFormat1('undeclared variable found in CONVERT (%a)', GetSymName(Var))
      ELSIF (IsSet(Type) OR IsEnumeration(Type) OR IsSubrange(Type) OR IsType(Type)) AND
            (IsVar(Var) OR IsConst(Var))
      THEN
         PopN(NoOfParam+1) ;    (* destroy arguments to this function *)
         (* firstly dereference Var *)
         IF GetMode(Var)=LeftValue
         THEN
            t := MakeTemporary(RightValue) ;
            PutVar(t, GetType(Var)) ;
            GenQuad(IndrXOp, t, GetType(Var), Var) ;
            Var := t
         END ;

         ReturnVar := MakeTemporary(RightValue) ;
         PutVar(ReturnVar, Type) ;
         IF (GetType(Var)#Integer) AND (GetType(Var)#Cardinal) AND
            (GetType(Var)#Word) AND (GetType(Var)#Address) AND
            (IsMathType(Type) OR IsMathType(GetType(Var))) AND
            (NOT (IsMathType(Type) AND IsMathType(GetType(Var))))
         THEN
            (* all mathematical type conversions must go through INTEGER *)
            IntegerVar := MakeTemporary(RightValue) ;
            PutVar(IntegerVar, Integer) ;
            GenQuad(ConvertOp, IntegerVar, Integer, Var) ;
            GenQuad(ConvertOp, ReturnVar, Type, IntegerVar)
         ELSE
            (* if we are converting from LONGINT to LONGREAL then we must not convert via INTEGER *)
            GenQuad(ConvertOp, ReturnVar, Type, Var)
         END ;
         PushTF(ReturnVar, Type)
      ELSE
         WriteFormat0('base procedure CONVERT expects 2 parameters, a type and a variable')
      END
   END
END BuildConvertFunction ;


VAR
   MaxEnumerationField,
   MinEnumerationField: CARDINAL ;


(*
   FindMinMaxEnum - finds the minimum and maximum enumeration fields.
*)

PROCEDURE FindMinMaxEnum (field: WORD) ;
VAR
   i: CARDINAL ;
BEGIN
   IF MaxEnumerationField=NulSym
   THEN
      MaxEnumerationField := field
   ELSE
      PushValue(field) ;
      PushValue(MaxEnumerationField) ;
      IF Gre(GetTokenNo())
      THEN
         MaxEnumerationField := field
      END
   END ;
   IF MinEnumerationField=NulSym
   THEN
      MinEnumerationField := field
   ELSE
      PushValue(field) ;
      PushValue(MinEnumerationField) ;
      IF Less(GetTokenNo())
      THEN
         MinEnumerationField := field
      END
   END
END FindMinMaxEnum ;


(*
   GetTypeMin - 
*)

PROCEDURE GetTypeMin (type: CARDINAL) : CARDINAL ;
VAR
   min, max: CARDINAL ;
BEGIN
   IF IsSubrange(type)
   THEN
      min := MakeTemporary(ImmediateValue) ;
      PutVar(min, type) ;
      GenQuad(SubrangeLowOp, min, NulSym, type) ;
      RETURN( min )
   ELSIF IsSet(type)
   THEN
      RETURN( GetTypeMin(GetType(type)) )
   ELSIF IsEnumeration(type)
   THEN
      MinEnumerationField := NulSym ;
      MaxEnumerationField := NulSym ;
      ForeachFieldEnumerationDo(type, FindMinMaxEnum) ;
      RETURN( MinEnumerationField )
   ELSIF IsBaseType(type)
   THEN
      GetBaseTypeMinMax(type, min, max) ;
      RETURN( min )
   ELSIF IsSystemType(type)
   THEN
      GetSystemTypeMinMax(type, min, max) ;
      RETURN( min )
   ELSIF GetType(type)=NulSym
   THEN
      WriteFormat1('unable to obtain the MIN value for type %a', GetSymName(type))
   ELSE
      RETURN( GetTypeMin(GetType(type)) )
   END
END GetTypeMin ;


(*
   GetTypeMax - 
*)

PROCEDURE GetTypeMax (type: CARDINAL) : CARDINAL ;
VAR
   min, max: CARDINAL ;
BEGIN
   IF IsSubrange(type)
   THEN
      max := MakeTemporary(ImmediateValue) ;
      PutVar(max, type) ;
      GenQuad(SubrangeHighOp, max, NulSym, type) ;
      RETURN( max )
   ELSIF IsSet(type)
   THEN
      RETURN( GetTypeMax(GetType(type)) )
   ELSIF IsEnumeration(type)
   THEN
      MinEnumerationField := NulSym ;
      MaxEnumerationField := NulSym ;
      ForeachFieldEnumerationDo(type, FindMinMaxEnum) ;
      RETURN( MaxEnumerationField )
   ELSIF IsBaseType(type)
   THEN
      GetBaseTypeMinMax(type, min, max) ;
      RETURN( max )
   ELSIF IsSystemType(type)
   THEN
      GetSystemTypeMinMax(type, min, max) ;
      RETURN( max )
   ELSIF GetType(type)=NulSym
   THEN
      WriteFormat1('unable to obtain the MAX value for type %a', GetSymName(type))
   ELSE
      RETURN( GetTypeMax(GetType(type)) )
   END
END GetTypeMax ;


(*
   BuildMinFunction - builds the pseudo function call Min.

                      The Stack:

                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam=1    |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildMinFunction ;
VAR
   min, max,
   NoOfParam,
   Var,
   ProcSym,
   Ptr      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      PopN(NoOfParam+1) ;    (* destroy arguments to this function *)
      IF IsAModula2Type(Var)
      THEN
         min := GetTypeMin(Var) ;
         PushTF(min, GetType(Var))
      ELSIF IsVar(Var)
      THEN
         min := GetTypeMin(GetType(Var)) ;
         PushTF(min, GetType(Var))
      ELSE
         WriteFormat0('parameter to MIN must be a type or a variable') ;
         PushT(MakeConstLit(MakeKey('0')))   (* put a legal value on the stack *)
      END
   ELSE
      WriteFormat0('the pseudo function MIN only has one parameter') ;
      PushT(MakeConstLit(MakeKey('0')))   (* put a legal value on the stack *)
   END
END BuildMinFunction ;


(*
   BuildMaxFunction - builds the pseudo function call Max.

                      The Stack:

                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam=1    |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildMaxFunction ;
VAR
   min, max,
   NoOfParam,
   Var,
   ProcSym,
   Ptr      : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      Var := OperandT(1) ;
      PopN(NoOfParam+1) ;    (* destroy arguments to this function *)
      IF IsAModula2Type(Var)
      THEN
         max := GetTypeMax(Var) ;
         PushTF(max, GetType(Var))
      ELSIF IsVar(Var)
      THEN
         max := GetTypeMax(GetType(Var)) ;
         PushTF(max, GetType(Var))
      ELSE
         WriteFormat0('parameter to MAX must be a type or a variable') ;
         PushT(MakeConstLit(MakeKey('0')))   (* put a legal value on the stack *)
      END
   ELSE
      WriteFormat0('the pseudo function MAX only has one parameter') ;
      PushT(MakeConstLit(MakeKey('0')))   (* put a legal value on the stack *)
   END
END BuildMaxFunction ;


(*
   BuildTruncFunction - builds the pseudo procedure call TRUNC.
                        This procedure is actually a "macro" for
                        TRUNC(x) --> CONVERT(INTEGER, x)
                        However we cannot push tokens back onto the input stack
                        because the compiler is currently building a function
                        call and expecting a ReturnVar on the stack.
                        Hence we manipulate the stack and call
                        BuildConvertFunction.

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildTruncFunction ;
VAR
   NoOfParam,
   Var,
   ReturnVar,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   Assert(OperandT(NoOfParam+1)=Trunc) ;
   IF NoOfParam=1
   THEN
      ProcSym := RequestSym(MakeKey('CONVERT')) ;
      IF (ProcSym#NulSym) AND IsProcedure(ProcSym)
      THEN
         Var := OperandT(1) ;
         IF IsVar(Var) OR IsConst(Var)
         THEN
            IF (GetType(Var)=LongReal) OR (GetType(Var)=Real) OR (GetType(Var)=ShortReal)
            THEN
               PopN(NoOfParam+1) ;    (* destroy arguments to this function *)
               (*
                  Build macro: CONVERT( INTEGER, Var )
               *)
               PushTF(ProcSym, NulSym) ;
               PushT(Integer) ;
               PushT(Var) ;
               PushT(2) ;          (* Two parameters *)
               BuildConvertFunction
            ELSE
               WriteFormat0('argument to TRUNC must have type REAL or LONGREAL')
            END
         ELSE
            WriteFormat0('argument to TRUNC must be a variable or constant')
         END
      ELSE
         InternalError('CONVERT procedure not found for TRUNC substitution', __FILE__, __LINE__)
      END
   ELSE
      WriteFormat0('the pseudo procedure TRUNC only has one parameter')
   END
END BuildTruncFunction ;


(*
   BuildFloatFunction - builds the pseudo procedure call FLOAT.
                        This procedure is actually a "macro" for
                        FLOAT(x) --> CONVERT(REAL, x)
                        However we cannot push tokens back onto the input stack
                        because the compiler is currently building a function
                        call and expecting a ReturnVar on the stack.
                        Hence we manipulate the stack and call
                        BuildConvertFunction.

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildFloatFunction ;
VAR
   NoOfParam,
   Var,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam=1
   THEN
      ProcSym := RequestSym(MakeKey('CONVERT')) ;
      IF (ProcSym#NulSym) AND IsProcedure(ProcSym)
      THEN
         Var := OperandT(1) ;
         IF IsVar(Var) OR IsConst(Var)
         THEN
            PopN(NoOfParam+1) ;    (* destroy arguments to this function *)
            (*
               Build macro: CONVERT( REAL, Var )
            *)
            PushTF(ProcSym, NulSym) ;
            PushT(Real) ;
            PushT(Var) ;
            PushT(2) ;          (* Two parameters *)
            BuildConvertFunction
         ELSE
            WriteFormat0('argument to FLOAT must be a variable or constant')
         END
      ELSE
         InternalError('CONVERT procedure not found for FLOAT substitution', __FILE__, __LINE__)
      END
   ELSE
      WriteFormat0('the pseudo procedure FLOAT only has one parameter')
   END
END BuildFloatFunction ;


(*
   BuildAdrFunction - builds the pseudo function ADR
                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |  
                      |----------------|
                      | Param 1        |  
                      |----------------|
                      | Param 2        |  
                      |----------------|
                      .                .  
                      .                .  
                      .                .  
                      |----------------|
                      | Param #        |                        <- Ptr
                      |----------------|         +------------+
                      | ProcSym | Type |         | ReturnVar  |
                      |----------------|         |------------|

*)

PROCEDURE BuildAdrFunction ;
VAR
   UnboundedSym,
   Field,
   NoOfParam,
   ProcSym,
   ReturnVar,
   Type        : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   IF NoOfParam#1
   THEN
      WriteFormat0('SYSTEM procedure ADR expects 1 parameter')
   ELSIF NOT IsVar(OperandT(1))
   THEN
      WriteFormat0('SYSTEM procedure ADR expects a variable as its parameter')
   ELSE
      Type := GetType(OperandT(1)) ;
      IF IsUnbounded(Type)
      THEN
         (* we will reference the address field of the unbounded structure *)
         UnboundedSym := OperandT(1) ;
         PushTF(UnboundedSym, GetType(UnboundedSym)) ;
         Field := GetLocalSym(Unbounded, ArrayAddress) ;
         PushTF(Field, GetType(Field)) ;
         PushT(1) ;
         BuildDesignatorRecord ;
         PopT(ReturnVar)
      ELSE
         ReturnVar := MakeLeftValue(OperandT(1), RightValue, GetType(ProcSym))
      END ;
      PopN(NoOfParam+1) ;    (* destroy the arguments and function *)
      PushTF(ReturnVar, GetType(ReturnVar))
   END
END BuildAdrFunction ;


(*
   BuildSizeFunction - builds the pseudo function SIZE
                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |  
                       |----------------|
                       | Param 1        |  
                       |----------------|
                       | Param 2        |  
                       |----------------|
                       .                .  
                       .                .  
                       .                .  
                       |----------------|
                       | Param #        |                        <- Ptr
                       |----------------|         +------------+
                       | ProcSym | Type |         | ReturnVar  |
                       |----------------|         |------------|

*)

PROCEDURE BuildSizeFunction ;
VAR
   Type,
   UnboundedSym,
   Field,
   NoOfParam,
   ProcSym,
   ReturnVar   : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   IF NoOfParam#1
   THEN
      WriteFormat0('SYSTEM procedure SIZE expects 1 parameter') ;
      ReturnVar := MakeConstLit(MakeKey('0'))
   ELSIF IsAModula2Type(OperandT(1))
   THEN
      BuildSizeCheckEnd(ProcSym) ;   (* quadruple generation now on *)
      ReturnVar := MakeTemporary(ImmediateValue) ;
      GenQuad(SizeOp, ReturnVar, NulSym, OperandT(1))
   ELSIF IsVar(OperandT(1))
   THEN
      BuildSizeCheckEnd(ProcSym) ;   (* quadruple generation now on *)
      Type := GetType(OperandT(1)) ;
      IF IsUnbounded(Type)
      THEN
         (* eg. SIZE(a)  ; where a is unbounded dereference HIGH and multiply by the TYPE *)
         UnboundedSym := OperandT(1) ;
         PushTF(UnboundedSym, GetType(UnboundedSym)) ;
         Field := GetLocalSym(Unbounded, ArrayHigh) ;
         PushTF(Field, GetType(Field)) ;
         PushT(1) ;
         BuildDesignatorRecord ;
         PushT(TimesTok) ;
         PushTF(TSize, Cardinal) ; (* TSIZE(GetType(Unbounded))    *)
         PushT(GetType(Type)) ;
         PushT(1) ;                (* 1 parameter for TSIZE()      *)
         BuildFunctionCall ;
         BuildBinaryOp ;
         PopT(ReturnVar)
      ELSE
         ReturnVar := MakeTemporary(ImmediateValue) ;
         IF Type=NulSym
         THEN
            WriteFormat1('cannot get the type and size of variable (%a)', GetSymName(OperandT(1)))
         END ;
         GenQuad(SizeOp, ReturnVar, NulSym, Type)
      END
   ELSE
      WriteFormat0('SYSTEM procedure SIZE expects a variable as its parameter') ;
      ReturnVar := MakeConstLit(MakeKey('0'))
   END ;
   PopN(NoOfParam+1) ;       (* destroy the arguments and function *)
   PushTF(ReturnVar, GetType(ProcSym))
END BuildSizeFunction ;


(*
   BuildTSizeFunction - builds the pseudo function TSIZE
                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |  
                        |----------------|
                        | Param 1        |  
                        |----------------|
                        | Param 2        |  
                        |----------------|
                        .                .  
                        .                .  
                        .                .  
                        |----------------|
                        | Param #        |                        <- Ptr
                        |----------------|         +------------+
                        | ProcSym | Type |         | ReturnVar  |
                        |----------------|         |------------|

*)

PROCEDURE BuildTSizeFunction ;
VAR
   NoOfParam,
   ProcSym,
   ReturnVar: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT(NoOfParam+1) ;
   IF NoOfParam#1
   THEN
      WriteFormat0('SYSTEM procedure TSIZE expects 1 parameter') ;
      ReturnVar := MakeConstLit(MakeKey('0'))
   ELSIF IsAModula2Type(OperandT(1))
   THEN
      ReturnVar := MakeTemporary(ImmediateValue) ;
      GenQuad(SizeOp, ReturnVar, NulSym, OperandT(1))
   ELSIF IsVar(OperandT(1))
   THEN
      ReturnVar := MakeTemporary(ImmediateValue) ;
      GenQuad(SizeOp, ReturnVar, NulSym, GetType(OperandT(1)))
   ELSE
      WriteFormat0('SYSTEM procedure TSIZE expects a type or variable as its parameter') ;
      ReturnVar := MakeConstLit(MakeKey('0'))
   END ;
   PopN(NoOfParam+1) ;       (* destroy the arguments and function *)
   PushTF(ReturnVar, GetType(ProcSym))
END BuildTSizeFunction ;


(*
   CheckVariablesAndParameterTypesInBlock - checks to make sure that block, BlockSym, has
                                            parameters types and variable types which are legal.
*)

PROCEDURE CheckVariablesAndParameterTypesInBlock (BlockSym: CARDINAL) ;
VAR
   i, n,
   ParamNo   : CARDINAL ;
BEGIN
   IF IsProcedure(BlockSym)
   THEN
      ParamNo := NoOfParam(BlockSym)
   ELSE
      ParamNo := 0
   END ;
   i := 1 ;
   REPEAT
      n := GetNth(BlockSym, i) ;
      IF (n#NulSym) AND (NOT IsTemporary(n)) AND
         (IsProcedure(BlockSym) OR ((IsDefImp(BlockSym) AND (GetMainModule()=BlockSym)) OR IsModule(BlockSym)))
      THEN
         IF i<=ParamNo
         THEN
            (* n is a parameter *)
            IF NOT IsAModula2Type(GetType(n))
            THEN
               ErrorStringAt2(Sprintf2(Mark(InitString('the parameter type specified (%s) in procedure (%s) was not declared as a type')),
                                       Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(n))))),
                                       Mark(InitStringCharStar(KeyToCharStar(GetSymName(BlockSym))))),
                              GetDeclared(BlockSym), GetDeclared(GetType(n)))
            END
         ELSE
            (* n is a local variable *)
            IF NOT IsAModula2Type(GetType(n))
            THEN
               ErrorStringAt(Sprintf1(Mark(InitString('the variable type specified (%s) was not declared as a type')),
                                      Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetType(n)))))),
                             GetDeclared(GetType(n)))
            END
         END
      END ;
      INC(i)
   UNTIL n=NulSym ;
END CheckVariablesAndParameterTypesInBlock ;


(*
   BuildProcedureStart - Builds start of the procedure. Creates space for
                         the local variables.
                         The Stack is expected to contain:


                         Entry                   Exit
                         =====                   ====

                 Ptr ->                                       <- Ptr
                        +------------+          +-----------+
                        | ProcSym    |          | ProcSym   |
                        |------------|          |-----------|
                        | Name       |          | Name      |
                        |------------|          |-----------|


                        Quadruples:

                        q   NewLocalVarOp  Line#  _  ProcSym
*)

PROCEDURE BuildProcedureStart ;
VAR
   ProcSym: CARDINAL ;
BEGIN
   BuildLineNo ;
   PopT(ProcSym) ;
   Assert(IsProcedure(ProcSym)) ;
   PutProcedureStartQuad(ProcSym, NextQuad) ;
   GenQuad(NewLocalVarOp, GetPreviousTokenLineNo(), GetScope(ProcSym), ProcSym) ;
   CurrentProc := ProcSym ;
   Push(ReturnStack, 0) ;
   PushT(ProcSym)
END BuildProcedureStart ;


(*
   BuildProcedureEnd - Builds end of the procedure. Destroys space for
                       the local variables.
                       The Stack is expected to contain:


                       Entry                   Exit
                       =====                   ====

                Ptr ->                                       <- Ptr
                       +------------+          +-----------+
                       | ProcSym    |          | ProcSym   |
                       |------------|          |-----------|
                       | Name       |          | Name      |
                       |------------|          |-----------|


                       Quadruples:

                       q   KillLocalVarOp  _  _  ProcSym
*)

PROCEDURE BuildProcedureEnd ;
VAR
   ProcSym : CARDINAL ;
   num     : String ;
BEGIN
   PopT(ProcSym) ;
   IF (GetType(ProcSym)#NulSym) AND ReturnChecking
   THEN
      (* function - therefore there must be a return *)
      PushTF(RequestSym(MakeKey('FunctionReturnError')),
             GetType(RequestSym(MakeKey('FunctionReturnError')))) ;
      PushT(MakeConstLitString(makekey(string(GetFileName())))) ;
      num := Sprintf1(Mark(InitString("%d")), GetLineNo()) ;
      PushT(MakeConstLit(makekey(string(num)))) ;
      num := KillString(num) ;
      PushT(2) ;
      BuildProcedureCall    (* call M2RTS_FunctionReturnError *)
   END ;
   BackPatch(Pop(ReturnStack), NextQuad) ;
   CurrentProc := NulSym ;
   GenQuad(KillLocalVarOp, GetPreviousTokenLineNo(), NulSym, ProcSym) ;
   PutProcedureEndQuad(ProcSym, NextQuad) ;
   GenQuad(ReturnOp, NulSym, NulSym, ProcSym) ;
   CheckFunctionReturn(ProcSym) ;
   CheckVariablesInBlock(ProcSym) ;
   PushT(ProcSym)
END BuildProcedureEnd ;


(*
   CheckReadBeforeInitialized - 
*)

PROCEDURE CheckReadBeforeInitialized (ProcSym: CARDINAL; Start, End: CARDINAL) ;
VAR
   i, n, ParamNo,
   ReadStart, ReadEnd,
   WriteStart, WriteEnd: CARDINAL ;
BEGIN
   ParamNo := NoOfParam(ProcSym) ;
   i := 1 ;
   REPEAT
      n := GetNth(ProcSym, i) ;
      IF (n#NulSym) AND (NOT IsTemporary(n))
      THEN
         GetVarReadQuads(n, ReadStart, ReadEnd) ;
         GetVarWriteQuads(n, WriteStart, WriteEnd) ;
         IF i>ParamNo
         THEN
            (* n is a not a parameter thus we can check *)
            IF (ReadStart>0) AND (ReadStart<End)
            THEN
               (* it is read in the first basic block *)
               IF ReadStart<WriteStart
               THEN
                  (* read before written, this is a problem which must be fixed *)
                  ErrorStringAt2(Sprintf2(Mark(InitString('reading from a variable (%s) before it is initialized in procedure (%s)')),
                                          Mark(InitStringCharStar(KeyToCharStar(GetSymName(n)))),
                                          Mark(InitStringCharStar(KeyToCharStar(GetSymName(ProcSym))))),
                                 GetDeclared(n), GetDeclared(n))
               END
            END
         END
      END ;
      INC(i)
   UNTIL n=NulSym
END CheckReadBeforeInitialized ;


(*
   VariableAnalysis - checks to see whether a variable is:

                      read before it has been initialized
*)

PROCEDURE VariableAnalysis (Start, End: CARDINAL) ;
VAR
   Op           : QuadOperator ;
   Op1, Op2, Op3: CARDINAL ;
BEGIN
   IF Pedantic
   THEN
      GetQuad(Start, Op, Op1, Op2, Op3) ;
      CASE Op OF

      NewLocalVarOp:  CheckReadBeforeInitialized(Op3, Start, End)

      ELSE
      END
   END
END VariableAnalysis ;


(*
   IsNeverAltered - returns TRUE if variable, sym, is never altered
                    between quadruples: Start..End
*)

PROCEDURE IsNeverAltered (sym: CARDINAL; Start, End: CARDINAL) : BOOLEAN ;
VAR
   WriteStart, WriteEnd: CARDINAL ;
BEGIN
   GetVarWriteLimitQuads(sym, Start, End, WriteStart, WriteEnd) ;
   RETURN( (WriteStart=0) AND (WriteEnd=0) )
END IsNeverAltered ;


(*
   IsConditionVariable - returns TRUE if the condition at quadruple, q, is variable.
*)

PROCEDURE IsConditionVariable (q: CARDINAL; Start, End: CARDINAL) : BOOLEAN ;
VAR
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
   LeftFixed,
   RightFixed   : BOOLEAN ;
BEGIN
   GetQuad(q, op, op1, op2, op3) ;
   IF op=GotoOp
   THEN
      RETURN( FALSE )
   ELSE
      LeftFixed  := IsConst(op1) ;
      RightFixed := IsConst(op2) ;
      IF NOT LeftFixed
      THEN
         LeftFixed := IsNeverAltered(op1, Start, End)
      END ;
      IF NOT RightFixed
      THEN
         RightFixed := IsNeverAltered(op2, Start, End)
      END ;
      RETURN( NOT (LeftFixed AND RightFixed) )
   END
END IsConditionVariable ;


(*
   IsInfiniteLoop - returns TRUE if an infinite loop is found.
                    Given a backwards jump at, End, it returns a BOOLEAN which depends on
                    whether a jump is found to jump beyond, End. If a conditonal jump is found
                    to pass over, End, the condition is tested for global variables, procedure variables and
                    constants.

                         constant        - ignored
                         variables       - tested to see whether they are altered inside the loop
                         global variable - the procedure tests to see whether it is altered as above
                                           but will also test to see whether this loop calls a procedure
                                           in which case it believes the loop NOT to be infinite
                                           (as this procedure call might alter the global variable)

                    Note that this procedure can easily be fooled by the user altering variables
                    with pointers.
*)

PROCEDURE IsInfiniteLoop (End: CARDINAL) : BOOLEAN ;
VAR
   SeenCall,
   IsGlobal     : BOOLEAN ;
   Current,
   Start        : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   SeenCall := FALSE ;
   IsGlobal := FALSE ;
   GetQuad(End, op, op1, op2, Start) ;
   Current := Start ;
   WHILE Current#End DO
      GetQuad(Current, op, op1, op2, op3) ;
      (* remember that this function is only called once we have optimized the redundant gotos and conditionals *)
      IF IsConditional(Current) AND (NOT IsGlobal)
      THEN
         IsGlobal := (IsVar(op1) AND (NOT IsProcedure(GetVarScope(op1)))) OR
                     (IsVar(op2) AND (NOT IsProcedure(GetVarScope(op2))))
      END ;
      IF op=CallOp
      THEN
         SeenCall := TRUE
      END ;
      IF (op=GotoOp) OR (IsConditional(Current) AND IsConditionVariable(Current, Start, End))
      THEN
         IF (op3>End) OR (op3<Start)
         THEN
            RETURN( FALSE )    (* may jump out of this loop, good *)
         END
      END ;
      Current := GetNextQuad(Current)
   END ;
   GetQuad(End, op, op1, op2, op3) ;
   IF IsConditional(End)
   THEN
      IF IsConditionVariable(End, Start, End)
      THEN
         RETURN( FALSE )
      ELSE
         IF NOT IsGlobal
         THEN
            IsGlobal := (IsVar(op1) AND (NOT IsProcedure(GetVarScope(op1)))) OR
                        (IsVar(op2) AND (NOT IsProcedure(GetVarScope(op2))))
         END
      END
   END ;
   (* we have found a likely infinite loop if no conditional uses a global and no procedure call was seen *)
   RETURN( NOT (IsGlobal AND SeenCall) )
END IsInfiniteLoop ;


(*
   LoopAnalysis - checks whether an infinite loop exists.
*)

PROCEDURE LoopAnalysis ;
VAR
   Current      : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   IF Pedantic
   THEN
      Current := Head ;
      WHILE Current#0 DO
         GetQuad(Current, op, op1, op2, op3) ;
         IF (op=GotoOp) OR IsConditional(Current)
         THEN
            IF op3<=Current
            THEN
               (* found a loop - ie a branch which goes back in quadruple numbers *)
               IF IsInfiniteLoop(Current)
               THEN
                  WarnStringAt(InitString('it is very likely (although not absolutely certain) that the top of an infinite loop is here'),
                               QuadToTokenNo(op3)) ;
                  WarnStringAt(InitString('and the bottom of the infinite loop is ends here or alternatively a component of this loop is never executed'),
                               QuadToTokenNo(Current))
               END
            END
         END ;
         Current := GetNextQuad(Current)
      END
   END
END LoopAnalysis ;


(*
   CheckUninitializedVariablesAreUsed - checks to see whether uninitialized variables are used.
*)

PROCEDURE CheckUninitializedVariablesAreUsed (BlockSym: CARDINAL) ;
VAR
   i, n,
   ParamNo   : CARDINAL ;
   ReadStart,
   ReadEnd,
   WriteStart,
   WriteEnd  : CARDINAL ;
BEGIN
   IF IsProcedure(BlockSym)
   THEN
      ParamNo := NoOfParam(BlockSym)
   ELSE
      ParamNo := 0
   END ;
   i := 1 ;
   REPEAT
      n := GetNth(BlockSym, i) ;
      IF (n#NulSym) AND (NOT IsTemporary(n)) AND
         (IsProcedure(BlockSym) OR (((IsDefImp(BlockSym) AND (GetMainModule()=BlockSym)) OR IsModule(BlockSym)) AND
                                    (NOT IsExported(BlockSym, n))))
      THEN
         GetVarReadQuads(n, ReadStart, ReadEnd) ;
         GetVarWriteQuads(n, WriteStart, WriteEnd) ;
         IF i<=ParamNo
         THEN
            (* n is a parameter *)
            IF ReadStart=0
            THEN
               IF WriteStart=0
               THEN
                  WarnStringAt(Sprintf2(Mark(InitString('unused parameter (%s) in procedure (%s)')),
                                        Mark(InitStringCharStar(KeyToCharStar(GetSymName(n)))),
                                        Mark(InitStringCharStar(KeyToCharStar(GetSymName(BlockSym))))),
                               GetDeclared(n))
               ELSE
                  IF NOT IsVarParam(BlockSym, i)
                  THEN
                     WarnStringAt(Sprintf2(Mark(InitString('writing to a non var parameter (%s) and never reading from it in procedure (%s)')),
                                           Mark(InitStringCharStar(KeyToCharStar(GetSymName(n)))),
                                           Mark(InitStringCharStar(KeyToCharStar(GetSymName(BlockSym))))),
                                  GetDeclared(n))
                  END
               END
            END
         ELSE
            (* n is a local variable *)
            IF ReadStart=0
            THEN
               IF WriteStart=0
               THEN
                  WarnStringAt(Sprintf2(Mark(InitString('unused variable (%s) in (%s)')),
                                        Mark(InitStringCharStar(KeyToCharStar(GetSymName(n)))),
                                        Mark(InitStringCharStar(KeyToCharStar(GetSymName(BlockSym))))),
                               GetDeclared(n))
               ELSE
                  WarnStringAt(Sprintf2(Mark(InitString('writing to a variable (%s) and never reading from it in (%s)')),
                                        Mark(InitStringCharStar(KeyToCharStar(GetSymName(n)))),
                                        Mark(InitStringCharStar(KeyToCharStar(GetSymName(BlockSym))))),
                               GetFirstUsed(n))
               END
            ELSE
               IF WriteStart=0
               THEN
                  ErrorStringAt(Sprintf2(Mark(InitString('variable (%s) is being used but is NEVER initialized in (%s)')),
                                         Mark(InitStringCharStar(KeyToCharStar(GetSymName(n)))),
                                         Mark(InitStringCharStar(KeyToCharStar(GetSymName(BlockSym))))),
                                GetFirstUsed(n)) ;
               END
            END
         END
      END ;
      INC(i)
   UNTIL n=NulSym
END CheckUninitializedVariablesAreUsed ;


(*
   AsmStatementsInBlock - returns TRUE if an ASM statement is found within a block, BlockSym.
*)

PROCEDURE AsmStatementsInBlock (BlockSym: CARDINAL) : BOOLEAN ;
VAR
   Start, End   : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   IF IsProcedure(BlockSym)
   THEN
      GetProcedureQuads(BlockSym, Start, End)
   ELSE
      GetModuleQuads(BlockSym, Start, End)
   END ;
   WHILE (Start#End) AND (Start#0) DO
      GetQuad(Start, op, op1, op2, op3) ;
      IF op=InlineOp
      THEN
         RETURN( TRUE )
      END ;
      Start := GetNextQuad(Start)
   END ;
   RETURN( FALSE )
END AsmStatementsInBlock ;


(*
   CheckVariablesInBlock - given a block, BlockSym, check whether all variables are used.
*)

PROCEDURE CheckVariablesInBlock (BlockSym: CARDINAL) ;
BEGIN
   CheckVariablesAndParameterTypesInBlock(BlockSym) ;
   IF Pedantic
   THEN
      IF (NOT AsmStatementsInBlock(BlockSym))
      THEN
         CheckUninitializedVariablesAreUsed(BlockSym)
      END
   END
END CheckVariablesInBlock ;


(*
   CheckFunctionReturn - checks to see that a RETURN statement was present in a function.
*)

PROCEDURE CheckFunctionReturn (ProcSym: CARDINAL) ;
VAR
   Op           : QuadOperator ;
   Op1, Op2, Op3,
   Start, End   : CARDINAL ;
BEGIN
   IF GetType(ProcSym)#NulSym
   THEN
      (* yes it is a function *)
      GetProcedureQuads(ProcSym, Start, End) ;
      GetQuad(Start, Op, Op1, Op2, Op3) ;
      IF Start=0
      THEN
         WriteFormat1('error in function %a', GetSymName(ProcSym)) ;
         InternalError('incorrect start quad', __FILE__, __LINE__)
      END ;
      WHILE (Start#End) AND (Op#ReturnValueOp) AND (Op#InlineOp) DO
         Start := GetNextQuad(Start) ;
         GetQuad(Start, Op, Op1, Op2, Op3)
      END ;
      IF (Op#ReturnValueOp) AND (Op#InlineOp)
      THEN
         (* an InlineOp can always be used to emulate a RETURN *)
         WriteFormat1('function %a does not RETURN a value', GetSymName(ProcSym))
      END
   END
END CheckFunctionReturn ;


(*
   BuildReturn - Builds the Return part of the procedure.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

         Ptr ->
                 +------------+
                 | e1         |          Empty
                 |------------|
*)

PROCEDURE BuildReturn ;
VAR
   e2, t2,
   e1, t1,
   t, f,
   Des   : CARDINAL ;
BEGIN
   IF IsBoolean(1)
   THEN
      PopBool(t, f) ;
      (* Des will be a boolean type *)
      Des := MakeTemporary(RightValue) ;
      PutVar(Des, Boolean) ;
      PushTF(Des, Boolean) ;
      PushBool(t, f) ;
      BuildAssignmentWithoutBounds ;
      PushTF(Des, Boolean)
   END ;
   PopTF(e1, t1) ;
   IF e1#NulSym
   THEN
      IF GetType(CurrentProc)=NulSym
      THEN
         WriteFormat1('attempting to RETURN a value from a procedure (%a) and not a function', GetSymName(CurrentProc))
      ELSIF (NOT IsAssignmentCompatible(t1, GetType(CurrentProc)))
      THEN
         WriteFormat2('attempting to RETURN a value with an incompatible type (%a) from a function which returns (%a)',
                      GetSymName(t1), GetSymName(GetType(CurrentProc)))
      ELSIF IsProcedure(e1) AND (NOT IsAssignmentCompatible(e1, GetType(CurrentProc)))
      THEN
         WriteFormat2('attempting to RETURN a value with an incompatible type (%a) from a function which returns (%a)',
                      GetSymName(e1), GetSymName(GetType(CurrentProc)))
      END ;
      (* dereference LeftValue if necessary *)
      IF GetMode(e1)=LeftValue
      THEN
         t2 := GetType(CurrentProc) ;
         e2 := MakeTemporary(RightValue) ;
         PutVar(e2, t2) ;
         GenQuad(IndrXOp, e2, t2, e1) ;
         GenQuad(ReturnValueOp, e2, NulSym, CurrentProc)
      ELSE
         GenQuad(ReturnValueOp, e1, NulSym, CurrentProc)
      END
   END ;
   GenQuad(GotoOp, NulSym, NulSym, Pop(ReturnStack)) ;
   Push(ReturnStack, NextQuad-1)
END BuildReturn ;


(*
   BuildDesignatorRecord - Builds the record referencing.
                           The Stack is expected to contain:


                           Entry                   Exit
                           =====                   ====

                   Ptr ->
                           +--------------+
                           | n            |
                           |--------------|
                           | fld1 | type1 |
                           |--------------|
                           .              .
                           .              .
                           .              .
                           |--------------|
                           | fldn | typen |                        <- Ptr
                           |--------------|        +-------------+
                           | Sym  | Type  |        | S    | type1|
                           |--------------|        |-------------|
*)

PROCEDURE BuildDesignatorRecord ;
VAR
   n, i,
   Sym, Type,
   adr, Res,
   t1, t2, t3: CARDINAL ;
BEGIN
   PopT(n) ;
   Sym  := OperandT(n+1) ;
   Type := OperandF(n+1) ;
   (* adr will be Address type *)
   adr := MakeLeftValue(Sym, RightValue, Address) ;
   (* No type for t1 since constant *)
   t1 := MakeTemporary(ImmediateValue) ;
   Sym  := OperandT(n) ;
   Type := OperandF(n) ;
   GenQuad(OffsetOp, t1, NulSym, Sym) ;
   IF n>1
   THEN
      FOR i := n-1 TO 1 BY -1 DO
         (* no type for t2 since constant *)
         t2 := MakeTemporary(ImmediateValue) ;
         GenQuad(OffsetOp, t2, NulSym, OperandT(i)) ;
         Type := OperandF(i) ;
         t3 := t1 ;
         (* No type for t1 since constant *)
         t1 := MakeTemporary(ImmediateValue) ;
         GenQuad(AddOp, t1, t3, t2)
      END
   END ;
   (* Res will be an Address since it is LeftValue mode *)
   Res := MakeTemporary(LeftValue) ;
   (*
      Ok must reference by address
      - but we contain the type of the referenced entity
   *)
   PutVarTypeAndSize(Res, Type, Address) ;
   GenQuad(AddOp, Res, adr, t1) ;
   PopN(n+1) ;
   PushTF(Res, Type)
END BuildDesignatorRecord ;


(*
   BuildDesignatorArray - Builds the array referencing.
                          The compile time stack is unchanged within
                          this procedure.
                          The purpose of this procedure is to work out
                          whether the DesignatorArray is a static or
                          dynamic array.
                          ie. in this compiler terms IsArray or IsUnbounded
                              In which case the appropriate routine is
                              called.
                              However these called routines DO alter
                              the compile time stack.

                          The Stack is expected to contain:


                          Entry                   Exit
                          =====                   ====

                  Ptr ->
                          +--------------+
                          | n            |
                          |--------------|
                          | e1           |
                          |--------------|
                          .              .
                          .              .
                          .              .
                          |--------------|
                          | e2           |                       <- Ptr
                          |--------------|        +------------+
                          | Sym  | Type  |        | S    | T   |
                          |--------------|        |------------|
*)

PROCEDURE BuildDesignatorArray ;
VAR
   Sym, n,
   Type  : CARDINAL ;
BEGIN
   PopT(n) ;
   IF (NOT IsVar(OperandT(n+1))) AND (NOT IsTemporary(OperandT(n+1)))
   THEN
      ErrorStringAt2(Mark(InitString('can only access arrays using variables or formal parameters')),
                     GetDeclared(OperandT(n+1)), GetTokenNo())
   END ;
   Sym := GetType(OperandT(n+1)) ;
   IF Sym=NulSym
   THEN
      IF GetSymName(Sym)=NulName
      THEN
         ErrorStringAt(Mark(InitString('type of array is undefined (no such array declared)')), GetTokenNo())
      ELSE
         ErrorStringAt(Sprintf1(Mark(InitString('type of array is undefined (%s)')),
                                Mark(InitStringCharStar(KeyToCharStar(GetSymName(Sym))))),
                       GetTokenNo())
      END
   END ;
   PushT(n) ;       (* Restore stack to origional state *)
   IF IsUnbounded(Sym)
   THEN
      IF n=1
      THEN
         BuildDynamicArray
      ELSE
         WriteFormat0('dynamic array only allowed one indice')
      END
   ELSIF IsArray(Sym)
   THEN
      BuildStaticArray
   ELSE
      WriteFormat0('can only index Static or Dynamic arrays')
   END
END BuildDesignatorArray ;


(*
   BuildStaticArray - Builds the array referencing for static arrays.
                      The Stack is expected to contain:


                      Entry                   Exit
                      =====                   ====

              Ptr ->
                      +--------------+
                      | n            |
                      |--------------|
                      | e1           |
                      |--------------|
                      .              .
                      .              .
                      .              .
                      |--------------|
                      | e2           |                       <- Ptr
                      |--------------|        +------------+
                      | Sym  | Type  |        | S    | T   |
                      |--------------|        |------------|
*)

PROCEDURE BuildStaticArray ;
VAR
   s        : Stack ;
   Sym,
   pi, i, n,
   OpI,
   Type, Adr,
   Base,
   Offset,
   ti, tj,
   tk, ta   : CARDINAL ;
BEGIN
   s := InitStack() ;
   PopT(n) ;
   Sym  := OperandT(n+1) ;
   Type := OperandF(n+1) ;
   IF NoOfElements(Type)#n
   THEN
      ErrorStringAt2(Mark(InitString('incorrect number of array indices: check [][] with [i, j]')),
                     GetDeclared(Type), GetTokenNo())
   END ;
   Offset := MakeTemporary(ImmediateValue) ;
   GenQuad(BaseOp, Offset, Type, Sym) ;
   (* Base has address type since it points to the start of the array in memory *)
   Base := MakeTemporary(RightValue) ;
   PutVar(Base, Address) ;
   Adr := MakeLeftValue(Sym, RightValue, Address) ;
   GenQuad(AddOp, Base, Adr, Offset) ;
   i := 1 ;
   pi := n ;
   WHILE i<=n DO
      (* Again ti has no type since constant *)
      ti := MakeTemporary(ImmediateValue) ;
      GenQuad(ElementSizeOp, ti, Type, i) ;
      OpI := OperandT(pi) ;
      IF IsConst(OpI)
      THEN
         (* tj has no type since constant *)
         tj := MakeTemporary(ImmediateValue) ;
         tk := MakeTemporary(ImmediateValue)
      ELSE
         (* tj has Cardinal type since we have multiplied array indices *)
         (* The problem is that OperandT(pi) might be a CHAR (or any    *)
         (* size < TSIZE(CARDINAL)) so we must coerse.                  *)

         IF GetType(OpI)#Cardinal
         THEN
            PushTF(RequestSym(MakeKey('CONVERT')), NulSym) ;
            PushT(Cardinal) ;
            PushT(OpI) ;
            PushT(2) ;          (* Two parameters *)
            BuildConvertFunction ;
            PopT(OpI)
         END ;
         tj := MakeTemporary(RightValue) ;
         PutVar(tj, MixTypes(GetType(ti), GetType(OpI), GetTokenNo())) ;
         tk := MakeTemporary(RightValue) ;
         PutVar(tk, MixTypes(GetType(ti), GetType(OpI), GetTokenNo()))
      END ;
      CheckSubrange(GetNth(Type, i), OpI) ;

      PushT(tj) ;
      PushT(OpI) ;
      BuildAssignmentWithoutBounds ;

      GenQuad(MultOp, tk, ti, tj) ;
      Push(s, tk) ;
      INC(i) ;
      DEC(pi)
   END ;
   (* ta should have type address as it points into memory *)
   ta := MakeTemporary(RightValue) ;
   PutVar(ta, Address) ;
   GenQuad(AddOp, ta, MakeConstLit(MakeKey('0')), Pop(s)) ;
   i := 2 ;
   WHILE i<=n DO
      GenQuad(AddOp, ta, ta, Pop(s)) ;
      INC(i)
   END ;
   Adr := MakeTemporary(LeftValue) ;
   (*
      Ok must reference by address
      - but we contain the type of the referenced entity
   *)
   PutVarTypeAndSize(Adr, GetType(Type), Address) ;

   GenQuad(AddOp, Adr, Base, ta) ;
   PopN(n+1) ;   (* remove all parameters to this procedure *)
   PushTF(Adr, GetType(Type)) ;
   s := KillStack(s)
END BuildStaticArray ;


(*
   BuildDynamicArray - Builds the array referencing for dynamic arrays.
                       The Stack is expected to contain:


                       Entry                   Exit
                       =====                   ====

               Ptr ->
                       +--------------+
                       | n            |
                       |--------------|
                       | e1           |
                       |--------------|
                       .              .
                       .              .
                       .              .
                       |--------------|
                       | e2           |                       <- Ptr
                       |--------------|        +------------+
                       | Sym  | Type  |        | S    | T   |
                       |--------------|        |------------|
*)

PROCEDURE BuildDynamicArray ;
VAR
   Sym,
   n, idx,
   Type, Adr,
   PtrToBase,
   Base,
   ti, tj, tk    : CARDINAL ;
BEGIN
   DumpStack ;
   PopT(n) ;
   Sym  := OperandT(n+1) ;
   Type := OperandF(n+1) ;
   IF NoOfElements(Type)#n
   THEN
      ErrorStringAt2(Mark(InitString('incorrect number of array indices: unbounded arrays have 1 indice')),
                     GetDeclared(Type), GetTokenNo())
   END ;
   (*
      Base has type address because
      BuildDesignatorRecord references by address.

      Build a record for retrieving the address of dynamic array.
      BuildDesignatorRecord will generate the required quadruples,
      therefore build set up the stack for BuildDesignatorRecord
      to generate the record access.
   *)
   (*
      Build above current current info needed for array.
      Note that, n, has gone by now.
   *)
   PushTF(Sym, Unbounded) ;
   PushTF(GetLocalSym(Unbounded, ArrayAddress),
          GetType(GetLocalSym(Unbounded, ArrayAddress))) ;
   PushT(1) ;  (* One record field to dereference *)
   BuildDesignatorRecord ;
   PopT(PtrToBase) ;
   DumpStack ;
   (* Now actually copy Unbounded.ArrayAddress into base *)
   IF GetMode(PtrToBase)=LeftValue
   THEN
      Base := MakeTemporary(RightValue) ;
      PutVar(Base, Address) ;           (* has type ADDRESS *)
      GenQuad(IndrXOp, Base, Address, PtrToBase)           (* Base = *PtrToBase *)
   ELSE
      Assert(GetMode(PtrToBase)#ImmediateValue) ;
      Base := PtrToBase
   END ;
   IF n#1
   THEN
      (* Only one dimemsion for dynamic arrays *)
      WriteFormat0('dynamic arrays can only have one dimension')
   END ;
   (* ti has no type since constant *)
   ti := MakeTemporary(ImmediateValue) ;
   GenQuad(ElementSizeOp, ti, Type, 1) ;
   idx := OperandT(1) ;
   IF IsConst(idx)
   THEN
      (* tj has no type since constant *)
      tj := MakeTemporary(ImmediateValue) ;
      tk := MakeTemporary(ImmediateValue)
   ELSE
      (* tj has Cardinal type since we have multiplied array indices *)
      tj := MakeTemporary(RightValue) ;
      (* was:
         PutVar(tj, MixTypes(GetType(ti), GetType(OperandT(1), GetTokenNo()))) ;
         we replace it by: (below). Because we know tj must be a CARDINAL.
         Also mixing types works fine when both are not constants since
         if they are constants then tj is set to NulSym, causing M2EvalSym
         to get upset.

         Although if the index variable is # TSIZE(Cardinal) then BecomesOp
         will rightly complain. Hence we must coerse to a TSIZE(Cardinal) -
         we havent solved type sizes yet so we can only compare against types.
      *)
      IF GetType(idx)#Cardinal
      THEN
         PushTF(RequestSym(MakeKey('CONVERT')), NulSym) ;
         PushT(Cardinal) ;
         PushT(idx) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction ;
         PopT(idx)
      END ;
      PutVar(tj, Cardinal) ;
      tk := MakeTemporary(RightValue) ;
      PutVar(tk, Cardinal)
   END ;
   CheckDynamicArray(Sym, idx) ;

   PushT(tj) ;
   PushT(idx) ;
   BuildAssignmentWithoutBounds ;

   GenQuad(MultOp, tk, ti, tj) ;
   Adr := MakeTemporary(LeftValue) ;
   (*
      Ok must reference by address
      - but we contain the type of the referenced entity
   *)
   PutVarTypeAndSize(Adr, GetType(Type), Address) ;

   GenQuad(AddOp, Adr, Base, tk) ;
   PopN(n+1) ;
   PushTF(Adr, GetType(Type)) ;
   DumpStack ;
END BuildDynamicArray ;


(*
   BuildDesignatorPointer - Builds the record referencing.
                            The Stack is expected to contain:


                            Entry                   Exit
                            =====                   ====

                    Ptr ->                                           <- Ptr
                            +--------------+        +--------------+
                            | Sym1  | Type1|        | Sym2  | Type2|
                            |--------------|        |--------------|
*)

PROCEDURE BuildDesignatorPointer ;
VAR
   Sym1, Type1,
   Sym2, Type2: CARDINAL ;
BEGIN
   PopTF(Sym1, Type1) ;
   IF IsUnknown(Sym1)
   THEN
      WriteFormat2('symbol (%a) is undefined and thus cannot resolve (%a^)',
                   GetSymName(Sym1), GetSymName(Sym1))
   ELSIF IsPointer(Type1)
   THEN
      Type2 := GetType(Type1) ;
      Sym2 := MakeTemporary(LeftValue) ;
      (*
         Ok must reference by address
         - but we contain the type of the referenced entity
      *)
      PutVarTypeAndSize(Sym2, Type2, Address) ;

      IF GetMode(Sym1)=LeftValue
      THEN
         GenQuad(IndrXOp, Sym2, Address, Sym1)           (* Sym2 := *Sym1 *)
      ELSE
         GenQuad(BecomesOp, Sym2, NulSym, Sym1)         (* Sym2 :=  Sym1 *)
      END ;

      PushTF(Sym2, Type2)
   ELSE
      WriteFormat1('%a is not a variable or is not a pointer type', GetSymName(Sym1))
   END
END BuildDesignatorPointer ;


(*
   StartBuildWith - performs the with statement.
                    The Stack:

                    Entry                    Exit

                    +------------+
                    | Sym | Type |           Empty
                    |------------|
*)

PROCEDURE StartBuildWith ;
VAR
   Sym, Type,
   adr      : CARDINAL ;
BEGIN
   DumpStack ;
   PopTF(Sym, Type) ;
   adr := MakeTemporary(LeftValue) ;
   PutVarTypeAndSize(adr, Type, Address) ;

   IF GetMode(Sym)=LeftValue
   THEN
      (* copy LeftValue *)
      GenQuad(BecomesOp, adr, NulSym, Sym)
   ELSE
      (* calculate the address of Sym *)
      GenQuad(AddrOp, adr, NulSym, Sym)
   END ;
   PushWith(adr, Type) ;
   IF Type=NulSym
   THEN
      IF IsTemporary(Sym)
      THEN
         WriteFormat0('unknown record variable specified in WITH statement')
      ELSE
         WriteFormat1('symbol (%a) is unknown, it should be a variable of a record type',
                           GetSymName(Sym))
      END
   ELSIF NOT IsRecord(Type)
   THEN
      IF GetSymName(Type)=NulName
      THEN
         IF GetSymName(Sym)=NulSym
         THEN
            WriteFormat0('expression in the WITH statement does must be a record type')
         ELSE
            WriteFormat1('the type being used in this WITH statement is not a record type, variable (%a)',
                              GetSymName(Sym))
         END
      ELSE
         WriteFormat1('the type being used in this WITH statement is not a record type (%a)',
                           GetSymName(Type))
      END
   END ;
   StartScope(Type)
 ; DumpStack ;
END StartBuildWith ;


(*
   EndBuildWith - terminates the innermost with scope.
*)

PROCEDURE EndBuildWith ;
BEGIN
   DumpStack ;
   EndScope ;
   PopWith
 ; DumpStack ;
END EndBuildWith ;


(*
   PushWith - pushes sym and type onto the with stack. It checks for
              previous declaration of this record type.
*)

PROCEDURE PushWith (Sym, Type: CARDINAL) ;
VAR
   i, n: CARDINAL ;
   f   : WithFrame ;
BEGIN
   (*
      actually there is no reason why nested WITH statements
      cannot reference the same record.
      - the outer WITH has presedence.
   *)
   IF Pedantic
   THEN
      n := NoOfItemsInStack(WithStack) ;
      i := 1 ;  (* top of the stack *)
      WHILE i<=n DO
         (* Search for other declarations of the with using Type *)
         f := Peep(WithStack, i) ;
         IF f^.RecordSym=Type
         THEN
            WriteFormat0('cannot have nested WITH statements referencing the same RECORD')
         END ;
         INC(i)
      END
   END ;
   NEW(f) ;
   WITH f^ DO
      PtrSym    := Sym ;
      RecordSym := Type
   END ;
   Push(WithStack, f)
END PushWith ;


PROCEDURE PopWith ;
VAR
   f: WithFrame ;
BEGIN
   f := Pop(WithStack) ;
   DISPOSE(f)
END PopWith ;


(*
   CheckWithReference - performs the with statement.
                        The Stack:

                        Entry                    Exit

                        +------------+           +------------+
                        | Sym | Type |           | Sym | Type |
                        |------------|           |------------|
*)

PROCEDURE CheckWithReference ;
VAR
   f        : WithFrame ;
   n,
   Sym, Type: CARDINAL ;
BEGIN
   n := NoOfItemsInStack(WithStack) ;
   IF (n>0) AND (NOT SuppressWith)
   THEN
      PopTF(Sym, Type) ;
      (* inner WITH always has precidence *)
      REPEAT
         (* WriteString('Checking for a with') ; *)
         f := Peep(WithStack, n) ;
         WITH f^ DO
            IF IsRecordField(Sym) AND (GetParent(Sym)=RecordSym)
            THEN
               (* Fake a RecordSym.op *)
               PushTF(PtrSym, RecordSym) ;
               PushTF(Sym, Type) ;
               BuildAccessWithField ;
               PopTF(Sym, Type) ;
               n := 0
            ELSE
               DEC(n)
            END
         END
      UNTIL n=0 ;
      PushTF(Sym, Type)
   END
END CheckWithReference ;


(*
   BuildAccessWithField - similar to BuildDesignatorRecord except it
                          does not perform the address operation.
                          The address will have been computed at the
                          beginning of the WITH statement.
                          It also stops the GenQuad procedure from examining the
                          with stack.

                          The Stack

                          Entry

                   Ptr ->
                          +--------------+
                          | Field | Type1|                          <- Ptr
                          |-------|------|          +-------------+
                          | Adr   | Type2|          | Sym  | Type1|
                          |--------------|          |-------------|
*)

PROCEDURE BuildAccessWithField ;
VAR
   OldSuppressWith: BOOLEAN ;
   Field, Type1,
   Adr, Type2,
   Res,
   t1, t2         : CARDINAL ;
BEGIN
   OldSuppressWith := SuppressWith ;
   SuppressWith := TRUE ;
   (*
      now the WITH cannot look at the stack of outstanding WITH records.
   *)
   PopTF(Field, Type1) ;
   PopTF(Adr, Type2) ;
   t1 := MakeTemporary(ImmediateValue) ;
   (* No type since t1 is constant *)
   GenQuad(OffsetOp, t1, NulSym, Field) ;
   Res := MakeTemporary(LeftValue) ;
   (*
      Ok must reference by address
      - but we contain the type of the referenced entity
   *)
   PutVarTypeAndSize(Res, Type1, Address) ;
   GenQuad(AddOp, Res, Adr, t1) ;
   PushTF(Res, Type1) ;
   SuppressWith := OldSuppressWith
END BuildAccessWithField ;


(*
   CheckOuterScopeProcedureVariable - checks whether the symbol
                                      on top of the stack is a procedure
                                      symbol from a previous scope.

                                      Entry                    Exit

                                      +------------+           +------------+
                                      | Sym | Type |           | Sym | Type |
                                      |------------|           |------------|
*)

PROCEDURE CheckOuterScopeProcedureVariable ;
VAR
   t2, t1,
   t, c,
   ScopeSym,
   DeclaredScopeSym,
   Sym, Type       : CARDINAL ;
BEGIN
   ScopeSym := GetCurrentScope() ;
   (*
    *  note that we do not need to check for nested procedure variables when
    *  using the GCC backend as GCC is intelligent to work this out for itself.
    *  So we simply use the variables and not calculate the position using the
    *  activation record pointer.
    *)
   IF (NOT UsingGCCBackend) AND IsProcedure(ScopeSym)
   THEN
      (* currently inside a procedure *)
      PopTF(Sym, Type) ;
      IF IsVar(Sym)
      THEN
         (* note we must not try to GetScope if Sym is not a variable *)
         DeclaredScopeSym := GetScope(Sym) ;

         IF CompilerDebugging
         THEN
            printf2('need to reference symbol, %a, defined in scope %a\n',
                    GetSymName(Sym), GetSymName(GetScope(Sym)))
         END ;

         IF IsProcedure(DeclaredScopeSym) AND (DeclaredScopeSym#ScopeSym)
         THEN
            (*
               so now we know that Sym is a procedure variable which was not declared in
               the current procedure but in an outer procedure.
            *)
            t := MakeTemporary(RightValue) ;
            PutVar(t, Address) ;
            GenQuad(BecomesOp, t, NulSym, ActivationPointer) ;
            ScopeSym := GetScope(ScopeSym) ;
            WHILE IsProcedure(DeclaredScopeSym) AND (ScopeSym#DeclaredScopeSym) DO
               c := MakeTemporary(ImmediateValue) ;
               (* must continue to chain backwards to find the scope where Sym was declared *)
               GenQuad(OffsetOp, c, NulSym, ActivationPointer) ;
               (* need to look indirectly to find next activation record *)
               t1 := MakeTemporary(RightValue) ;   (* we use a different variable to help the optimizer *)
               PutVar(t1, Address) ;
               GenQuad(AddOp, t1, t, c) ;
               t2 := MakeTemporary(RightValue) ;   (* we use a different variable to help the optimizer *)
               PutVar(t2, Address) ;
               GenQuad(IndrXOp, t2, Address, t1) ; (* next chained frame is now in t2 *)
               t := t2 ;
               ScopeSym := GetScope(ScopeSym)
            END ;
            IF ScopeSym=DeclaredScopeSym
            THEN
               (* finished chaining backwards, found sym in scope ScopeSym *)
               c := MakeTemporary(ImmediateValue) ;
               GenQuad(OffsetOp, c, NulSym, Sym) ;
               (* calculate address of sym *)
               t1 := MakeTemporary(RightValue) ;   (* we use a different variable to help the optimizer *)
               PutVar(t1, Address) ;
               GenQuad(AddOp, t1, t, c) ;

               (* now we create a new variable, sym, which will contain the address of the variable *)
               Sym := MakeTemporary(LeftValue) ;
               PutVarTypeAndSize(Sym, Type, Address) ;
               GenQuad(BecomesOp, Sym, NulSym, t1)
            ELSE
               InternalError('we should have found symbol in a procedure scope', __FILE__, __LINE__)
            END
         END
      END ;
      PushTF(Sym, Type)
   END
END CheckOuterScopeProcedureVariable ;


(*
   BuildNulExpression - Builds a nul expression on the stack.
                        The Stack:

                        Entry             Exit

                                                         <- Ptr
                        Empty             +------------+
                                          | NulSym     |
                                          |------------|
*)

PROCEDURE BuildNulExpression ;
BEGIN
   PushT(NulSym)
END BuildNulExpression ;


(*
   BuildBitsetStart - Pushes a Bitset type on the stack.
                      The Stack:

                      Entry             Exit

               Ptr ->                                   <- Ptr

                      Empty             +-------------+
                                        | Bitset      |
                                        |-------------|
*)

PROCEDURE BuildBitsetStart ;
BEGIN
   PushT(Bitset)
END BuildBitsetStart ;


(*
   BuildSetEnd - pops the set value and type from the stack
                 and pushes the value,type pair.

                    Entry                   Exit

             Ptr ->
                    +--------------+
                    | Set Value    |                         <- Ptr
                    |--------------|        +--------------+
                    | Set Type     |        | Value | Type |
                    |--------------|        |--------------|
*)

PROCEDURE BuildSetEnd ;
VAR
   v, t: CARDINAL ;
BEGIN
   PopT(v) ;
   PopT(t) ;
   PushTF(v, t) ;
   Assert(IsSet(t))
END BuildSetEnd ;


(*
   BuildEmptySet - Builds an empty set on the stack.
                   The Stack:

                   Entry             Exit

                                                     <- Ptr
                                     +-------------+
            Ptr ->                   | Value       |
                   +-----------+     |-------------|
      	       	   | SetType   |     | SetType     |
                   |-----------|     |-------------|

*)

PROCEDURE BuildEmptySet ;
VAR
   Type  : CARDINAL ;
   NulSet: CARDINAL ;
BEGIN
   PopT(Type) ;  (* type of set we are building *)
   IF IsUnknown(Type)
   THEN
      WriteFormat1('set type %a is undefined', GetSymName(Type)) ;
      Type := Bitset
   ELSIF NOT IsSet(SkipType(Type))
   THEN
      WriteFormat1('expecting a set type %a', GetSymName(Type)) ;
      Type := Bitset
   ELSE
      Type := SkipType(Type)
   END ;
   NulSet := MakeTemporary(ImmediateValue) ;
   Assert(Type#NulSym) ;
   PutVar(NulSet, Type) ;
   PutConstSet(NulSet) ;
   IF CompilerDebugging
   THEN
      printf1('set type = %a\n', GetSymName(Type))
   END ;
   PushNulSet(Type) ;   (* onto the ALU stack  *)
   PopValue(NulSet) ;   (* ALU -> symbol table *)

   (* and now construct the M2Quads stack as defined by the comments above *)
   PushT(Type) ;
   PushT(NulSet) ;
   IF CompilerDebugging
   THEN
      printf2('Type = %a  (%d)  built empty set\n', GetSymName(Type), Type) ;
      DisplayStack    (* Debugging info *)
   END
END BuildEmptySet ;


(*
   BuildInclRange - includes a set range with a set.


                          Entry                   Exit
                          =====                   ====


                   Ptr ->
                          +------------+
                          | El2        |
                          |------------|
                          | El1        |                                 <- Ptr
                          |------------|           +-------------------+
                          | Set Value  |           | Value + {El1..El2}|
                          |------------|           |-------------------|

                   No quadruples produced as the range info is contained within
                   the set value.
*)

PROCEDURE BuildInclRange ;
VAR
   el1, el2,
   value: CARDINAL ;
BEGIN
   PopT(el2) ;
   PopT(el1) ;
   PopT(value) ;
   IF IsConst(el1) AND IsConst(el2)
   THEN
      PushValue(value) ;  (* onto ALU stack *)
      AddBitRange(el1, el2) ;
      PopValue(value)     (* ALU -> symboltable *)
   ELSE
      IF NOT IsConst(el1)
      THEN
         WriteFormat1('must use constants as ranges when defining a set constant, problem with the low value %a', GetSymName(el1))
      END ;
      IF NOT IsConst(el2)
      THEN
         WriteFormat1('must use constants as ranges when defining a set constant, problem with the high value %a', GetSymName(el2))
      END
   END ;
   PushT(value)
END BuildInclRange ;


(*
   BuildInclBit - includes a bit into the set.

                         Entry                   Exit
                         =====                   ====


                  Ptr ->
                         +------------+
                         | Element    |                         <- Ptr
                         |------------|          +------------+
                         | Value      |          | Value      |
                         |------------|          |------------|

*)

PROCEDURE BuildInclBit ;
VAR
   e           : Error ;
   el, value, t: CARDINAL ;
BEGIN
   PopT(el) ;
   PopT(value) ;
   IF value=5504
   THEN
      stop
   END ;
   IF IsConst(el)
   THEN
      PushValue(value) ;  (* onto ALU stack *)
      AddBit(el) ;
      PopValue(value)    (* ALU -> symboltable *)
   ELSE
      IF GetMode(el)=LeftValue
      THEN
         t := MakeTemporary(RightValue) ;
         PutVar(t, GetType(el)) ;
         GenQuad(IndrXOp, t, GetType(el), el) ;
         el := t
      END ;
      IF IsConst(value)
      THEN
         (* move constant into a variable to achieve the include *)
         t := MakeTemporary(RightValue) ;
         PutVar(t, GetType(value)) ;
         GenQuad(BecomesOp, t, NulSym, value) ;
         value := t
      END ;
      GenQuad(InclOp, value, NulSym, el)
   END ;
   PushT(value)
END BuildInclBit ;


(*
   RecordOp - Records the operator passed on the stack.
              Checks for AND operator or OR operator
              if either of these operators are found then BackPatching
              takes place.
              The Expected Stack:

              Entry                        Exit

       Ptr ->                                               <- Ptr
              +-------------+               +-------------+
              | OperatorTok |               | OperatorTok |
              |-------------|               |-------------|
              | t    | f    |               | t    | f    |
              |-------------|               |-------------|


              If OperatorTok=AndTok
              Then
                 BackPatch(f, NextQuad)
              Elsif OperatorTok=OrTok
              Then
                 BackPatch(t, NextQuad)
              End
*)

PROCEDURE RecordOp ;
VAR
   Op  : Name ;
   t, f: CARDINAL ;
BEGIN
   PopT(Op) ;
   IF (Op=AndTok) OR (Op=AmbersandTok)
   THEN
      CheckBooleanId ;
      PopBool(t, f) ;
      BackPatch(t, NextQuad) ;
      PushBool(0, f)
   ELSIF Op=OrTok
   THEN
      CheckBooleanId ;
      PopBool(t, f) ;
      BackPatch(f, NextQuad) ;
      PushBool(t, 0)
   END ;
   PushT(Op)
END RecordOp ;


(*
   CheckForLogicalOperator - returns a logical operator if the operands imply
                             a logical operation should be performed.
*)

PROCEDURE CheckForLogicalOperator (Tok: Name; e1, t1, e2, t2: CARDINAL) : Name ;
BEGIN
   IF (Tok=PlusTok) OR (Tok=TimesTok) OR (Tok=DivTok) OR (Tok=MinusTok)
   THEN
      IF ((t2#NulSym) AND IsSet(t2)) OR (IsConst(e2) AND IsConstSet(e2))
      THEN
         IF Tok=PlusTok
         THEN
            RETURN( LogicalOrTok )
         ELSIF Tok=DivTok
         THEN
            RETURN( LogicalXorTok )
         ELSIF Tok=TimesTok
         THEN
            RETURN( LogicalAndTok )
         ELSIF Tok=MinusTok
         THEN
            RETURN( LogicalDifferenceTok )
         END
      END
   END ;
   RETURN( Tok )
END CheckForLogicalOperator ;


(*
   CheckGenericNulSet - checks to see whether e1 is a generic nul set and if so it alters it
                        to the nul set of t2.
*)

PROCEDURE CheckGenericNulSet (e1: CARDINAL; VAR t1: CARDINAL; t2: CARDINAL) ;
BEGIN
   IF IsConstSet(e1)
   THEN
      IF NOT IsSet(t2)
      THEN
         WriteFormat2('incompatibility between a set constant of type (%a) and an object of type (%a)',
                      GetSymName(t1), GetSymName(t2))
      END ;
      PushValue(e1) ;
      IF IsGenericNulSet()
      THEN
         PopValue(e1) ;
         PushNulSet(t2) ;
         t1 := t2
      END ;
      PopValue(e1)
   END
END CheckGenericNulSet ;


(*
   CheckForGenericNulSet - if e1 or e2 is the generic nul set then
                           alter it to the nul set of the other operands type.
*)

PROCEDURE CheckForGenericNulSet (e1, e2: CARDINAL; VAR t1, t2: CARDINAL) ;
BEGIN
   IF t1#t2
   THEN
      CheckGenericNulSet(e1, t1, t2) ;
      CheckGenericNulSet(e2, t2, t1)
   END
END CheckForGenericNulSet ;


(*
   BuildBinaryOp   - Builds a binary operation from the quad stack.
                     Be aware that this procedure will check for
                     the overloading of the bitset operators + - \ *.
                     So do NOT call this procedure if you are building
                     a reference to an array which has a bitset type or
                     the address arithmetic will be wrongly coersed into
                     logical ORs.
                     
                     The Stack is expected to contain:


                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | Sym1       |
                     |------------|
                     | Operator   |                          <- Ptr
                     |------------|          +------------+
                     | Sym2       |          | Temporary  |
                     |------------|          |------------|


                     Quadruples Produced

                     q     Operator  Temporary  Sym1  Sym2


                OR


                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | T1   | F1  |
                     |------------|
                     | OrTok      |                          <- Ptr
                     |------------|          +------------+
                     | T2   | F2  |          | T1+T2| F1  |
                     |------------|          |------------|


                     Quadruples Produced

*)

PROCEDURE BuildBinaryOp ;
VAR
   NewTok,
   Tok   : Name ;
   t1, f1,
   t2, f2,
   e1, e2,
   t,
   ta    : CARDINAL ;
BEGIN
   Tok := OperandT(2) ;
   IF Tok=OrTok
   THEN
      CheckBooleanId ;
      PopBool(t1, f1) ;
      PopT(Tok) ;
      PopBool(t2, f2) ;
      Assert(f2=0) ;
      PushBool(Merge(t1, t2), f1)
   ELSIF (Tok=AndTok) OR (Tok=AmbersandTok)
   THEN
      CheckBooleanId ;
      PopBool(t1, f1) ;
      PopT(Tok) ;
      PopBool(t2, f2) ;
      Assert(t2=0) ;
      PushBool(t1, Merge(f1, f2))
   ELSE
      PopTF(e1, t1) ;
      PopT(Tok) ;
      PopTF(e2, t2) ;
      NewTok := CheckForLogicalOperator(Tok, e1, t1, e2, t2) ;
      IF NewTok=Tok
      THEN
         (*
            BinaryOps and UnaryOps only work with immediate and offset addressing.
            which is fine for calculating array and record offsets but we need
            to get the real values to perform normal arithmetic. Not address
            arithmetic.

            However the set operators will dereference LValues (to optimize large
            set arithemetic)
         *)
         IF GetMode(e1)=LeftValue
         THEN
            t := MakeTemporary(RightValue) ;
            PutVar(t, t1) ;
            GenQuad(IndrXOp, t, t1, e1) ;
            e1 := t
         END ;
         IF GetMode(e2)=LeftValue
         THEN
            t := MakeTemporary(RightValue) ;
            PutVar(t, t2) ;
            GenQuad(IndrXOp, t, t2, e2) ;
            e2 := t
         END
      ELSE
         (* CheckForGenericNulSet(e1, e2, t1, t2) *)
      END ;
      IF (Tok=EqualTok) OR (Tok=HashTok)
      THEN
         CheckAssignmentCompatible(t1, t2)
      ELSE
         CheckExpressionCompatible(t1, t2)
      END ;
      t := MakeTemporary(AreConstant(IsConst(e1) AND IsConst(e2))) ;
      PutVar(t, MixTypes(t1, t2, GetTokenNo())) ;
      GenQuad(MakeOp(NewTok), t, e2, e1) ;
      PushTF(t, GetType(t))
   END
END BuildBinaryOp ;


(*
   BuildUnaryOp   - Builds a unary operation from the quad stack.
                    The Stack is expected to contain:


                    Entry                   Exit
                    =====                   ====

             Ptr ->
                    +------------+
                    | Sym        |
                    |------------|          +------------+
                    | Operator   |          | Temporary  | <- Ptr
                    |------------|          |------------|


                    Quadruples Produced

                    q     Operator  Temporary  _ Sym

*)

PROCEDURE BuildUnaryOp ;
VAR
   Tok     : Name ;
   type,
   Sym,
   SymT,  t: CARDINAL ;
BEGIN
   PopT(Sym) ;
   PopT(Tok) ;
   IF Tok=MinusTok
   THEN
      type := MixTypes(GetType(Sym), NulSym, GetTokenNo()) ;
      t := MakeTemporary(AreConstant(IsConst(Sym))) ;
      PutVar(t, type) ;

      (*
         variables must have a type and REAL/LONGREAL constants must
         be typed
      *)

      IF NOT IsConst(Sym)
      THEN
         IF (type#NulSym) AND IsSet(type)
         THEN
            (* do not dereference set variables *)
         ELSIF GetMode(Sym)=LeftValue
         THEN
            (* dereference symbols which are not sets and which are variables *)

            SymT := MakeTemporary(RightValue) ;
            PutVar(SymT, GetType(Sym)) ;
            GenQuad(IndrXOp, SymT, GetType(Sym), Sym) ;
            Sym := SymT
         END
      END ;
      GenQuad(NegateOp, t, NulSym, Sym) ;
      PushT(t)
   ELSIF Tok=PlusTok
   THEN
      PushT(Sym)
   ELSE
      WriteFormat1('not expecting this kind of unary operator (%a)', Tok)
   END
END BuildUnaryOp ;


(*
   AreConstant - returns immediate addressing mode if b is true else
                 offset mode is returned. b determines whether the
                 operands are all constant - in which case we can use
                 a constant temporary variable.
*)

PROCEDURE AreConstant (b: BOOLEAN) : ModeOfAddr ;
BEGIN
   IF b
   THEN
      RETURN( ImmediateValue )
   ELSE
      RETURN( RightValue )
   END
END AreConstant ;


(*
   ConvertBooleanToVariable - converts a BoolStack(i) from a Boolean True|False
                              exit pair into a variable containing the value TRUE or
                              FALSE. The parameter, i, is relative to the top
                              of the stack.
*)

PROCEDURE ConvertBooleanToVariable (i: CARDINAL) ;
VAR
   Des: CARDINAL ;
   f  : BoolFrame ;
BEGIN
   Assert(IsBoolean(i)) ;
   (*
      need to convert it to a variable containing the result.
      Des will be a boolean type
   *)
   Des := MakeTemporary(RightValue) ;
   PutVar(Des, Boolean) ;
   PushT(Des) ;   (* we have just increased the stack so we must use i+1 *)
   f := Peep(BoolStack, i+1) ;
   PushBool(f^.TrueExit, f^.FalseExit) ;
   BuildAssignmentWithoutBounds ;  (* restored stack *)
   f := Peep(BoolStack, i) ;
   WITH f^ DO
      TrueExit := Des ;  (* alter Stack(i) to contain the variable *)
      FalseExit := Boolean ;
      BooleanOp := FALSE (* no longer a Boolean True|False pair    *)
   END
END ConvertBooleanToVariable ;


(*
   BuildBooleanVariable - tests to see whether top of stack is a boolean
                          conditional and if so it converts it into a boolean
                          variable.
*)

PROCEDURE BuildBooleanVariable ;
BEGIN
   IF IsBoolean(1)
   THEN
      ConvertBooleanToVariable(1)
   END
END BuildBooleanVariable ;


(*
   BuildRelOpFromBoolean - builds a relational operator sequence of quadruples
                           instead of using a temporary boolean variable.
                           This function can only be used when we perform
                           the following translation:

                           (a=b) # (c=d)  alternatively   (a=b) = (c=d)
                                 ^                              ^

                           it only allows # = to be used as >= <= > < all
                           assume a particular value for TRUE and FALSE.
                           (In which case the user should specify ORD)


                           before

                           q    if r1      op1     op2     t2
                           q+1  Goto                       f2
                           q+2  if r2      op3     op4     t1
                           q+3  Goto                       f1

                           after (in case of =)

                           q    if r1      op1     op2     q+2
                           q+1  Goto                       q+4
                           q+2  if r2      op3     op4     t
                           q+3  Goto                       f
                           q+4  if r2      op3     op4     f
                           q+5  Goto                       t

                           after (in case of #)

                           q    if r1      op1     op2     q+2
                           q+1  Goto                       q+4
                           q+2  if r2      op3     op4     f
                           q+3  Goto                       t
                           q+4  if r2      op3     op4     t
                           q+5  Goto                       f

                           The Stack is expected to contain:


                           Entry                   Exit
                           =====                   ====

                    Ptr ->
                           +------------+
                           | t1 | f1    |
                           |------------|
                           | Operator   |                          <- Ptr
                           |------------|          +------------+
                           | t2 | f2    |          | t    | f   |
                           |------------|          |------------|


*)

PROCEDURE BuildRelOpFromBoolean ;
VAR
   Tok,
   t1, f1,
   t2, f2: CARDINAL ;
BEGIN
   Assert(IsBoolean(1) AND IsBoolean(3)) ;
   IF OperandT(2)=EqualTok
   THEN
      (* are the two boolean expressions the same? *)
      PopBool(t1, f1) ;
      PopT(Tok) ;
      PopBool(t2, f2) ;
      (* give the false exit a second chance *)
      BackPatch(t2, t1) ;        (* q    if   _     _    q+2 *)
      BackPatch(f2, NextQuad) ;  (* q+1  if   _     _    q+4 *)
      Assert(NextQuad=f1+1) ;
      WITH Quads[t1] DO
         GenQuad(Operator, Operand1, Operand2, 0)
      END ;
      GenQuad(GotoOp, NulSym, NulSym, 0) ;
      PushBool(Merge(NextQuad-1, t1), Merge(NextQuad-2, f1))
   ELSIF OperandT(2)=HashTok
   THEN
      (* are the two boolean expressions the different? *)
      PopBool(t1, f1) ;
      PopT(Tok) ;
      PopBool(t2, f2) ;
      (* give the false exit a second chance *)
      BackPatch(t2, t1) ;        (* q    if   _     _    q+2 *)
      BackPatch(f2, NextQuad) ;  (* q+1  if   _     _    q+4 *)
      Assert(NextQuad=f1+1) ;
      WITH Quads[t1] DO
         GenQuad(Operator, Operand1, Operand2, 0)
      END ;
      GenQuad(GotoOp, NulSym, NulSym, 0) ;
      PushBool(Merge(NextQuad-2, f1), Merge(NextQuad-1, t1))
   ELSE
      WriteFormat0('only allowed to use relation operators = # on BOOLEAN expressions as these do not imply a value for TRUE or FALSE')
   END
END BuildRelOpFromBoolean ;


(*
   CheckVariableOrConstant - checks to make sure sym is a variable or constant
*)

PROCEDURE CheckVariableOrConstant (sym: CARDINAL) ;
VAR
   type: CARDINAL ;
BEGIN
   type := GetType(sym) ;
   IF IsUnknown(sym)
   THEN
      ErrorStringAt(Sprintf1(Mark(InitString('symbol (%s) has not been declared')),
                             Mark(InitStringCharStar(KeyToCharStar(GetSymName(sym))))),
                    GetFirstUsed(sym))
   ELSIF (NOT IsConst(sym)) AND (NOT IsVar(sym)) AND
         (NOT IsTemporary(sym)) AND (NOT MustNotCheckBounds)
   THEN
      ErrorStringAt2(InitString('expecting a variable, constant or expression'),
                     GetTokenNo(), GetDeclared(sym))
   ELSIF (type#NulSym) AND IsArray(type)
   THEN
      ErrorStringAt2(InitString('not expecting an array variable as an operand for either comparison or binary operation'),
                     GetTokenNo(), GetDeclared(sym))
   ELSIF IsConstString(sym) AND (GetStringLength(sym)#1)
   THEN
      ErrorStringAt(InitString('not expecting a string constant as an operand for either comparison or binary operation'),
                    GetTokenNo())
   END
END CheckVariableOrConstant ;


(*
   BuildRelOp   - Builds a relative operation from the quad stack.
                  The Stack is expected to contain:


                  Entry                   Exit
                  =====                   ====

           Ptr ->
                  +------------+
                  | e1         |
                  |------------|                          <- Ptr
                  | Operator   |
                  |------------|          +------------+
                  | e2         |          | t    | f   |
                  |------------|          |------------|


                    Quadruples Produced

                    q     IFOperator  e2  e1  TrueExit    ; e2  e1 since
                    q+1   GotoOp              FalseExit   ; relation > etc
                                                          ; requires order.
*)

PROCEDURE BuildRelOp ;
VAR
   Op    : Name ;
   t,
   t1, t2,
   e1, e2: CARDINAL ;
BEGIN
   IF CompilerDebugging
   THEN
      DisplayStack    (* Debugging info *)
   END ;
   IF IsBoolean(1) AND IsBoolean(3)
   THEN
      (*
         we allow # and = to be used with Boolean expressions.
         we do not allow >  <  >=  <=  though
      *)
      BuildRelOpFromBoolean
   ELSE
      IF IsBoolean(1)
      THEN
         ConvertBooleanToVariable(1)
      END ;
      IF IsBoolean(3)
      THEN
         ConvertBooleanToVariable(3)
      END ;
      PopTF(e1, t1) ;
      PopT(Op) ;
      PopTF(e2, t2) ;

      CheckVariableOrConstant(e1) ;
      CheckVariableOrConstant(e2) ;

      IF (Op=EqualTok) OR (Op=HashTok)
      THEN
         CheckAssignmentCompatible(t1, t2)
      ELSE
         CheckExpressionCompatible(t1, t2)
      END ;

      (* must dereference LeftValue operands *)
      IF GetMode(e1)=LeftValue
      THEN
         t := MakeTemporary(RightValue) ;
         PutVar(t, GetType(e1)) ;
         GenQuad(IndrXOp, t, GetType(e1), e1) ;
         e1 := t
      END ;
      IF GetMode(e2)=LeftValue
      THEN
         t := MakeTemporary(RightValue) ;
         PutVar(t, GetType(e2)) ;
         GenQuad(IndrXOp, t, GetType(e2), e2) ;
         e2 := t
      END ;
      GenQuad(MakeOp(Op), e2, e1, 0) ;      (* True  Exit *)
      GenQuad(GotoOp, NulSym, NulSym, 0) ;  (* False Exit *)
      PushBool(NextQuad-2, NextQuad-1)
   END
END BuildRelOp ;


(*
   BuildNot   - Builds a NOT operation from the quad stack.
                The Stack is expected to contain:


                  Entry                   Exit
                  =====                   ====

           Ptr ->                                        <- Ptr
                  +------------+          +------------+
                  | t    | f   |          | f    | t   |
                  |------------|          |------------|
*)

PROCEDURE BuildNot ;
VAR
   t, f: CARDINAL ;
BEGIN
   CheckBooleanId ;
   PopBool(t, f) ;
   PushBool(f, t)
END BuildNot ;


(*
   MakeOp - returns the equalent quadruple operator to a token, t.
*)

PROCEDURE MakeOp (t: Name) : QuadOperator ;
BEGIN
   IF t=PlusTok
   THEN
      RETURN( AddOp )
   ELSIF t=MinusTok
   THEN
      RETURN( SubOp )
   ELSIF (t=DivTok) OR (t=DivideTok)
   THEN
      RETURN( DivOp )
   ELSIF t=TimesTok
   THEN
      RETURN( MultOp )
   ELSIF t=ModTok
   THEN
      RETURN( ModOp )
   ELSIF t=HashTok
   THEN
      RETURN( IfNotEquOp )
   ELSIF t=LessGreaterTok
   THEN
      RETURN( IfNotEquOp )
   ELSIF t=GreaterEqualTok
   THEN
      RETURN( IfGreEquOp )
   ELSIF t=LessEqualTok
   THEN
      RETURN( IfLessEquOp )
   ELSIF t=EqualTok
   THEN
      RETURN( IfEquOp )
   ELSIF t=LessTok
   THEN
      RETURN( IfLessOp )
   ELSIF t=GreaterTok
   THEN
      RETURN( IfGreOp )
   ELSIF t=InTok
   THEN
      RETURN( IfInOp )
   ELSIF t=LogicalOrTok
   THEN
      RETURN( LogicalOrOp )
   ELSIF t=LogicalAndTok
   THEN
      RETURN( LogicalAndOp )
   ELSIF t=LogicalXorTok
   THEN
      RETURN( LogicalXorOp )
   ELSIF t=LogicalDifferenceTok
   THEN
      RETURN( LogicalDiffOp )
   ELSE
      InternalError('binary operation not implemented yet', __FILE__, __LINE__)
   END
END MakeOp ;


(*
   GenQuad - Generate a quadruple with Operation, Op1, Op2, Op3.
*)

PROCEDURE GenQuad (Operation: QuadOperator;
                   Op1, Op2, Op3: CARDINAL) ;
BEGIN
   (* WriteString('Potential Quad: ') ; *)
   IF QuadrupleGeneration
   THEN
      IF NextQuad#Head
      THEN
         WITH Quads[NextQuad-1] DO
            Next := NextQuad
         END
      END ;
      PutQuad(NextQuad, Operation, Op1, Op2, Op3) ;
      WITH Quads[NextQuad] DO
         Next := 0 ;
         LineNo := GetLineNo() ;
         TokenNo := GetTokenNo()
      END ;
      (* DisplayQuad(NextQuad) ; *)
      NewQuad(NextQuad)
   END
END GenQuad ;


(*
   DisplayQuadList - Displays all quads in list Head.
*)

PROCEDURE DisplayQuadList (Head: CARDINAL) ;
BEGIN
   printf0('Quadruples:\n') ;
   WHILE Head#0 DO
      DisplayQuad(Head) ;
      Head := Quads[Head].Next
   END
END DisplayQuadList ;


(*
   BackPatch - Makes each of the quadruples on the list pointed to by
               StartQuad, take quadruple Value as a target.
*)

PROCEDURE BackPatch (QuadNo, Value: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   IF QuadrupleGeneration
   THEN
      WHILE QuadNo#0 DO
         WITH Quads[QuadNo] DO
            i := Operand3 ;                       (* Next Link along the BackPatch *)
            ManipulateReference(QuadNo, Value)    (* Filling in the BackPatch.     *)
         END ;
         QuadNo := i
      END
      (* ; DisplayQuadList(1) ;  (* Debugging only *) *)
   END
END BackPatch ;


(*
   Merge - joins two quad lists, QuadList2 to the end of QuadList1.
           A QuadList of value zero is a nul list.
*)

PROCEDURE Merge (QuadList1, QuadList2: CARDINAL) : CARDINAL ;
VAR
   i, j: CARDINAL ;
BEGIN
   IF QuadList1=0
   THEN
      RETURN( QuadList2 )
   ELSIF QuadList2=0
   THEN
      RETURN( QuadList1 )
   ELSE
      i := QuadList1 ;
      REPEAT
         j := i ;
         i := Quads[i].Operand3
      UNTIL i=0 ;
      ManipulateReference(j, QuadList2) ;
      RETURN( QuadList1 )
   END
END Merge ;


(*
   DisplayStack - displays the compile time symbol stack.
*)

PROCEDURE DisplayStack ;
VAR
   i, n: CARDINAL ;
   f   : BoolFrame ;
BEGIN
   printf0('+-------------------+\n') ;
   n := NoOfItemsInStack(BoolStack) ;
   i := 1 ;
   WHILE i<=n DO
      f := Peep(BoolStack, i) ;
      WITH f^ DO
         IF BooleanOp
         THEN
            printf2('| Q%6d | Q%6d |\n', TrueExit, FalseExit) ;
            printf0('|---------|---------|\n')
         ELSE
            printf1('| %6d            |\n', TrueExit) ;
            printf0('|-------------------|\n')
         END
      END ;
      INC(i)
   END
END DisplayStack ;


(*
   DisplayQuad - displays a quadruple, QuadNo.
*)

PROCEDURE DisplayQuad (QuadNo: CARDINAL) ;
BEGIN
   printf1('%4d  ', QuadNo) ; WriteQuad(QuadNo) ; printf0('\n')
END DisplayQuad ;


(*
   WriteQuad - Writes out the Quad BufferQuad.
*)

PROCEDURE WriteQuad (BufferQuad: CARDINAL) ;
BEGIN
   WITH Quads[BufferQuad] DO
      WriteOperator(Operator) ;
      printf1('  [%d]    ', NoOfTimesReferenced) ;
      CASE Operator OF

      SubrangeLowOp,
      SubrangeHighOp,
      BecomesOp,
      InclOp,
      ExclOp,
      UnboundedOp,
      HighOp,
      ReturnValueOp,
      FunctValueOp,
      OffsetOp,
      NegateOp,
      SizeOp,
      AddrOp            : WriteOperand(Operand1) ;
                          printf0('  ') ;
                          WriteOperand(Operand3) |
      ElementSizeOp,
      IfInOp,
      IfNotInOp,
      IfNotEquOp,
      IfEquOp,
      IfLessOp,
      IfGreOp,
      IfLessEquOp,
      IfGreEquOp        : WriteOperand(Operand1) ;
                          printf0('  ') ;
                          WriteOperand(Operand2) ;
                          printf1('  %4d', Operand3) |

      InlineOp,
      GotoOp            : printf1('%4d', Operand3) |

      LineNumberOp      : printf2('%a:%d', Operand1, Operand3) |

      EndFileOp         : printf1('%a', GetSymName(Operand3)) |

      ReturnOp,
      CallOp,
      KillLocalVarOp    : WriteOperand(Operand3) |

      NewLocalVarOp,
      EndOp,
      StartOp           : printf3('  %4d  %a  %a', Operand1, GetSymName(Operand2), GetSymName(Operand3)) |

      StartModFileOp    : printf3('%a:%d  %a', Operand2, Operand1, GetSymName(Operand3)) |

      StartDefFileOp    : printf2('  %4d  %a', Operand1, GetSymName(Operand3)) |

      ParamOp           : printf1('%4d  ', Operand1) ;
                          WriteOperand(Operand2) ;
                          printf0('  ') ;
                          WriteOperand(Operand3) |

      IndrXOp,
      XIndrOp,
      BaseOp,
      LogicalOrOp,
      LogicalAndOp,
      LogicalXorOp,
      CoerceOp,
      ConvertOp,
      AddOp,
      SubOp,
      MultOp,
      ModOp,
      DivOp             : WriteOperand(Operand1) ;
                          printf0('  ') ;
                          WriteOperand(Operand2) ;
                          printf0('  ') ;
                          WriteOperand(Operand3) |
      DummyOp,
      CodeOnOp,
      CodeOffOp,
      ProfileOnOp,
      ProfileOffOp,
      OptimizeOnOp,
      OptimizeOffOp     : |
      BuiltinConstOp    : WriteOperand(Operand1) ;
                          printf1('   %a', Operand3)

      ELSE
         InternalError('quadruple not recognised', __FILE__, __LINE__)
      END
   END
END WriteQuad ;


(*
   WriteOperator - writes the name of the quadruple operator.
*)

PROCEDURE WriteOperator (Operator: QuadOperator) ;
BEGIN
   CASE Operator OF

   LogicalOrOp              : printf0('Or           ') |
   LogicalAndOp             : printf0('And          ') |
   LogicalXorOp             : printf0('Xor          ') |
   LogicalDiffOp            : printf0('Ldiff        ') |
   BecomesOp                : printf0('Becomes      ') |
   IndrXOp                  : printf0('IndrXOp      ') |
   XIndrOp                  : printf0('XIndrOp      ') |
   BaseOp                   : printf0('Base         ') |
   ElementSizeOp            : printf0('ElementSize  ') |
   AddrOp                   : printf0('Addr         ') |
   SizeOp                   : printf0('Size         ') |
   OffsetOp                 : printf0('Offset       ') |
   IfInOp                   : printf0('If IN        ') |
   IfNotInOp                : printf0('If NOT IN    ') |
   IfNotEquOp               : printf0('If <>        ') |
   IfEquOp                  : printf0('If =         ') |
   IfLessEquOp              : printf0('If <=        ') |
   IfGreEquOp               : printf0('If >=        ') |
   IfGreOp                  : printf0('If >         ') |
   IfLessOp                 : printf0('If <         ') |
   GotoOp                   : printf0('Goto         ') |
   DummyOp                  : printf0('Dummy        ') |
   StartDefFileOp           : printf0('StartDefFile ') |
   StartModFileOp           : printf0('StartModFile ') |
   EndFileOp                : printf0('EndFileOp    ') |
   StartOp                  : printf0('Start        ') |
   EndOp                    : printf0('End          ') |
   AddOp                    : printf0('+            ') |
   SubOp                    : printf0('-            ') |
   DivOp                    : printf0('DIV          ') |
   ModOp                    : printf0('MOD          ') |
   MultOp                   : printf0('*            ') |
   NegateOp                 : printf0('Negate       ') |
   InclOp                   : printf0('Incl         ') |
   ExclOp                   : printf0('Excl         ') |
   ReturnOp                 : printf0('Return       ') |
   ReturnValueOp            : printf0('ReturnValue  ') |
   FunctValueOp             : printf0('FunctValue   ') |
   CallOp                   : printf0('Call         ') |
   ParamOp                  : printf0('Param        ') |
   NewLocalVarOp            : printf0('NewLocalVar  ') |
   KillLocalVarOp           : printf0('KillLocalVar ') |
   UnboundedOp              : printf0('Unbounded    ') |
   CoerceOp                 : printf0('Coerce       ') |
   ConvertOp                : printf0('Convert      ') |
   HighOp                   : printf0('High         ') |
   CodeOnOp                 : printf0('CodeOn       ') |
   CodeOffOp                : printf0('CodeOff      ') |
   ProfileOnOp              : printf0('ProfileOn    ') |
   ProfileOffOp             : printf0('ProfileOff   ') |
   OptimizeOnOp             : printf0('OptimizeOn   ') |
   OptimizeOffOp            : printf0('OptimizeOff  ') |
   InlineOp                 : printf0('Inline       ') |
   LineNumberOp             : printf0('LineNumber   ') |
   BuiltinConstOp           : printf0('BuiltinConst ')

   ELSE
      InternalError('operator not expected', __FILE__, __LINE__)
   END
END WriteOperator ;


PROCEDURE WriteOperand (Sym: CARDINAL) ;
BEGIN
   printf1('%a', GetSymName(Sym)) ;
   IF IsVar(Sym) OR IsConst(Sym)
   THEN
      printf0('[') ; WriteMode(GetMode(Sym)) ; printf0(']')
   END ;
   printf1('(%d)', Sym)
END WriteOperand ;


PROCEDURE WriteMode (Mode: ModeOfAddr) ;
BEGIN
   CASE Mode OF

   ImmediateValue: printf0('i') |
   NoValue       : printf0('n') |
   RightValue    : printf0('r') |
   LeftValue     : printf0('l')

   ELSE
      InternalError('unrecognised mode', __FILE__, __LINE__)
   END
END WriteMode ;


(*
   PushExit - pushes the exit value onto the EXIT stack.
*)

PROCEDURE PushExit (Exit: CARDINAL) ;
BEGIN
   Push(ExitStack, Exit)
END PushExit ;


(*
   PopExit - pops the exit value from the EXIT stack.
*)

PROCEDURE PopExit() : WORD ;
BEGIN
   RETURN( Pop(ExitStack) )
END PopExit ;


(*
   DumpStack - display the expression stack.
*)

PROCEDURE DumpStack ;
VAR
   i, n: CARDINAL ;
BEGIN
   IF DebugStack
   THEN
      n := NoOfItemsInStack(BoolStack) ;
      i := n ;
      printf0('quad stack: ') ;
      WHILE i>0 DO
         IF IsBoolean(i)
         THEN
            printf1('%d {boolean}  ', n-i+1)
         ELSE
            printf3('%d [%d, %d]  ', n-i+1, OperandT(i), OperandF(i))
         END ;
         DEC(i)
      END ;
      printf0('\n')
   END
END DumpStack ;


(*
   PopBool - Pops a True and a False exit quad number from the True/False
             stack.
*)

PROCEDURE PopBool (VAR True, False: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := Pop(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      Assert(BooleanOp)
   END ;
   DISPOSE(f)
END PopBool ;


(*
   PushBool - Push a True and a False exit quad numbers onto the
              True/False stack.
*)

PROCEDURE PushBool (True, False: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(True<=NextQuad) ;
   Assert(False<=NextQuad) ;
   NEW(f) ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      BooleanOp := TRUE
   END ;
   Push(BoolStack, f)
END PushBool ;


(*
   IsBoolean - returns true is the Stack position pos contains a Boolean
               Exit. False is returned if an Ident is stored.
*)

PROCEDURE IsBoolean (pos: CARDINAL) : BOOLEAN ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   f := Peep(BoolStack, pos) ;
   RETURN( f^.BooleanOp )
END IsBoolean ;


(*
   OperandT - returns the ident operand stored in the true position on the boolean stack.
*)

PROCEDURE OperandT (pos: CARDINAL) : WORD ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   Assert(NOT IsBoolean(pos)) ;
   f := Peep(BoolStack, pos) ;
   RETURN( f^.TrueExit )
END OperandT ;


(*
   OperandF - returns the ident operand stored in the false position on the boolean stack.
*)

PROCEDURE OperandF (pos: CARDINAL) : WORD ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   Assert(NOT IsBoolean(pos)) ;
   f := Peep(BoolStack, pos) ;
   RETURN( f^.FalseExit )
END OperandF ;


(*
   BuildCodeOn - generates a quadruple declaring that code should be
                 emmitted from henceforth.

                 The Stack is unnaffected.
*)

PROCEDURE BuildCodeOn ;
BEGIN
   GenQuad(CodeOnOp, NulSym, NulSym, NulSym)
END BuildCodeOn ;


(*
   BuildCodeOff - generates a quadruple declaring that code should not be
                  emmitted from henceforth.

                  The Stack is unnaffected.
*)

PROCEDURE BuildCodeOff ;
BEGIN
   GenQuad(CodeOffOp, NulSym, NulSym, NulSym)
END BuildCodeOff ;


(*
   BuildProfileOn - generates a quadruple declaring that profile timings
                    should be emmitted from henceforth.

                    The Stack is unnaffected.
*)

PROCEDURE BuildProfileOn ;
BEGIN
   GenQuad(ProfileOnOp, NulSym, NulSym, NulSym)
END BuildProfileOn ;


(*
   BuildProfileOn - generates a quadruple declaring that profile timings
                    should be emmitted from henceforth.

                    The Stack is unnaffected.
*)

PROCEDURE BuildProfileOff ;
BEGIN
   GenQuad(ProfileOffOp, NulSym, NulSym, NulSym)
END BuildProfileOff ;


(*
   BuildOptimizeOn - generates a quadruple declaring that optimization
                     should occur from henceforth.

                     The Stack is unnaffected.
*)

PROCEDURE BuildOptimizeOn ;
BEGIN
   GenQuad(OptimizeOnOp, NulSym, NulSym, NulSym)
END BuildOptimizeOn ;


(*
   BuildOptimizeOff - generates a quadruple declaring that optimization
                      should not occur from henceforth.

                      The Stack is unnaffected.
*)

PROCEDURE BuildOptimizeOff ;
BEGIN
   GenQuad(OptimizeOffOp, NulSym, NulSym, NulSym)
END BuildOptimizeOff ;


(*
   BuildInline - builds an Inline pseudo quadruple operator.
                 The inline interface, Sym, is stored as the operand
                 to the operator InlineOp.

                 The stack is expected to contain:


                        Entry                   Exit
                        =====                   ====
  
                 Ptr ->
                        +--------------+
                        | Sym          |        Empty
                        |--------------|
*)

PROCEDURE BuildInline ;
VAR
   Sym: CARDINAL ;
BEGIN
   PopT(Sym) ;
   GenQuad(InlineOp, NulSym, NulSym, Sym)
END BuildInline ;


(*
   BuildLineNo - builds a LineNumberOp pseudo quadruple operator.
                 This quadruple indicates which source line has been
                 processed, these quadruples are only generated if we
                 are producing runtime debugging information.

                 The stack is not affected, read or altered in any way.


                        Entry                   Exit
                        =====                   ====
  
                 Ptr ->                              <- Ptr
*)

PROCEDURE BuildLineNo ;
VAR
   filename: Name ;
BEGIN
   IF (NextQuad#Head) AND (GenerateLineDebug OR GenerateDebugging)
   THEN
      filename := makekey(string(GetFileName())) ;
      IF (Quads[NextQuad-1].Operator=LineNumberOp) AND
         (Quads[NextQuad-1].Operand1=WORD(filename))
      THEN
         (* PutQuad(NextQuad-1, LineNumberOp, WORD(filename), NulSym, GetLineNo()) *)
      ELSE
         GenQuad(LineNumberOp, WORD(filename), NulSym, WORD(GetLineNo()))
      END
   END
END BuildLineNo ;


(*
   UseLineNote - uses the line note and returns it to the free list.
*)

PROCEDURE UseLineNote (l: LineNote) ;
BEGIN
   WITH l^ DO
      IF (Quads[NextQuad-1].Operator=LineNumberOp) AND
         (Quads[NextQuad-1].Operand1=WORD(File))
      THEN
(*
         IF Line<Quads[NextQuad-1].Operand3
         THEN
            PutQuad(NextQuad-1, LineNumberOp, WORD(File), NulSym, Line)
         END
*)
      ELSE
         GenQuad(LineNumberOp, WORD(File), NulSym, WORD(Line))
      END ;
      Next := FreeLineList
   END ;
   FreeLineList := l
END UseLineNote ;


(*
   PopLineNo - pops a line note from the line stack.
*)

PROCEDURE PopLineNo () : LineNote ;
VAR
   l: LineNote ;
BEGIN
   l := Pop(LineStack) ;
   IF l=NIL
   THEN
      InternalError('no line note available', __FILE__, __LINE__)
   END ;
   RETURN( l  )
END PopLineNo ;


(*
   InitLineNote - creates a line note and initializes it to
                  contain, file, line.
*)

PROCEDURE InitLineNote (file: Name; line: CARDINAL) : LineNote ;
VAR
   l: LineNote ;
BEGIN
   IF FreeLineList=NIL
   THEN
      NEW(l)
   ELSE
      l := FreeLineList ;
      FreeLineList := FreeLineList^.Next
   END ;
   WITH l^ DO
      File := file ;
      Line := line
   END ;
   RETURN( l )
END InitLineNote ;


(*
   PushLineNote - 
*)

PROCEDURE PushLineNote (l: LineNote) ;
BEGIN
   Push(LineStack, l)   
END PushLineNote ;


(*
   PushLineNo - pushes the current file and line number to the stack.
*)

PROCEDURE PushLineNo ;
BEGIN
   PushLineNote(InitLineNote(makekey(string(GetFileName())), GetLineNo()))
END PushLineNo ;


(*
   NoOfDynamicQuads - returns the number of quadruples which have been
                      used by the dynamic code generator.
*)

PROCEDURE NoOfDynamicQuads () : CARDINAL ;
BEGIN
   RETURN( NoOfDynamic )
END NoOfDynamicQuads ;


(*
   IncDynamicQuads - increments the number of dynamic quadruple count
                     by Start..End quadruples.
*)

PROCEDURE IncDynamicQuads (Start, End: CARDINAL) ;
BEGIN
   INC(NoOfDynamic) ;
   WHILE Start#End DO
      Start := GetNextQuad(Start) ;
      INC(NoOfDynamic)
   END
END IncDynamicQuads ;


(*
   PushTF - Push a True and False numbers onto the True/False stack.
            True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PushTF (True, False: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      BooleanOp := FALSE
   END ;
   Push(BoolStack, f)
END PushTF ;


(*
   PopTF - Pop a True and False number from the True/False stack.
           True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PopTF (VAR True, False: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := Pop(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      Assert(NOT BooleanOp)
   END ;
   DISPOSE(f)
END PopTF ;


(*
   PushT - Push an item onto the True/False stack. The False value will be zero.
*)

PROCEDURE PushT (True: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := 0 ;
      BooleanOp := FALSE
   END ;
   Push(BoolStack, f)
END PushT ;


(*
   PopT - Pops an item from the True/False stack. The False value is ignored.
*)

PROCEDURE PopT (VAR True: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := Pop(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      Assert(NOT BooleanOp)
   END ;
   DISPOSE(f)
END PopT ;


(*
   PopNothing - pops the top element on the boolean stack.
*)

PROCEDURE PopNothing ;
VAR
   f: BoolFrame ;
BEGIN
   f := Pop(BoolStack) ;
   DISPOSE(f)
END PopNothing ;


(*
   PopN - pops multiple elements from the BoolStack.
*)

PROCEDURE PopN (n: CARDINAL) ;
BEGIN
   WHILE n>0 DO
      PopNothing ;
      DEC(n)
   END
END PopN ;


(*
   PushAutoOn - push the auto flag and then set it to TRUE.
                Any call to ident in the parser will result in the token being pushed.
*)

PROCEDURE PushAutoOn ;
BEGIN
   Push(AutoStack, IsAutoOn) ;
   IsAutoOn := TRUE
END PushAutoOn ;


(*
   PushAutoOff - push the auto flag and then set it to FALSE.
*)

PROCEDURE PushAutoOff ;
BEGIN
   Push(AutoStack, IsAutoOn) ;
   IsAutoOn := FALSE
END PushAutoOff ;


(*
   IsAutoPushOn - returns the value of the current Auto ident push flag.
*)

PROCEDURE IsAutoPushOn () : BOOLEAN ;
BEGIN
   RETURN( IsAutoOn )
END IsAutoPushOn ;


(*
   PopAuto - restores the previous value of the Auto flag.
*)

PROCEDURE PopAuto ;
BEGIN
   IsAutoOn := Pop(AutoStack)
END PopAuto ;


(*
   StressStack - 
*)

PROCEDURE StressStack ;
CONST
   Maxtries = 1000 ;
VAR
   n, i, j: CARDINAL ;
BEGIN
   FOR n := 1 TO Maxtries DO
      FOR i := n TO 1 BY -1 DO
         PushT(i)
      END ;
      FOR i := n TO 1 BY -1 DO
         Assert(OperandT(i)=i)
      END ;
      FOR i := 1 TO n DO
         Assert(OperandT(i)=i)
      END ;
      FOR i := 1 TO n BY 10 DO
         Assert(OperandT(i)=i)
      END ;
      IF (n>1) AND (n MOD 2 = 0)
      THEN
         FOR i := 1 TO n DIV 2 DO
            PopT(j) ;
            Assert(j=i)
         END ;
         FOR i := n DIV 2 TO 1 BY -1 DO
            PushT(i)
         END
      END ;
      FOR i := 1 TO n DO
         PopT(j) ;
         Assert(j=i)
      END
   END
END StressStack ;


(*
   Init - initialize the M2Quads module, all the stacks, all the lists 
          and the quads list.
*)

PROCEDURE Init ;
BEGIN
   LogicalOrTok := MakeKey('_LOR') ;
   LogicalAndTok := MakeKey('_LAND') ;
   LogicalXorTok := MakeKey('_LXOR') ;
   LogicalDifferenceTok := MakeKey('_LDIFF') ;
   FreeList := 1 ;
   NoOfDynamic := 0 ;
   NewQuad(NextQuad) ;
   Assert(NextQuad=1) ;
   BoolStack := InitStack() ;
   ExitStack := InitStack() ;
   WithStack := InitStack() ;
   ReturnStack := InitStack() ;
   LineStack := InitStack() ;
   (* StressStack ; *)
   SuppressWith := FALSE ;
   Head := 1 ;
   LastQuadNo := 0 ;
   MustNotCheckBounds := FALSE ;
   InitQuad := 0 ;
   GrowInitialization := 0 ;
   WITH ForInfo DO
      InitList(IncrementQuad) ;
      InitList(StartOfForLoop) ;
      InitList(EndOfForLoop) ;
      InitList(ForLoopIndex)
   END ;
   QuadrupleGeneration := TRUE ;
   AutoStack := InitStack() ;
   IsAutoOn := TRUE ;
   FreeLineList := NIL
END Init ;


BEGIN
   (* WriteString('Initialization of M2Quads') ; WriteLn ; *)
   Init
END M2Quads.
