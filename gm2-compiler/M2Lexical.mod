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
IMPLEMENTATION MODULE M2Lexical ;


IMPORT IO ;

FROM M2RTS IMPORT ExitOnHalt ;
FROM TimeString IMPORT GetTimeString ;
FROM StdIO IMPORT Read ;
FROM M2Error IMPORT BeginError, EndError, Title, FlushError ;
FROM SymbolTable IMPORT GetSymName ;

FROM M2Pass IMPORT IsPass1, IsPass2, IsPass3, IsPassCodeGeneration,
                   IsPassHidden,
                   IsNoPass, SetPassToNoPass, SetPassToPass1,
                   IsErrorPass, SetPassToErrorPass ;

FROM M2Options IMPORT Pedantic, StudentChecking, LineDirectives ;
FROM M2Search IMPORT FindSourceFile ;

FROM M2Reserved IMPORT IsReserved, AsmTok, EndTok, EofTok,
                       DateTok, LineTok, FileTok,
      	       	       SingleQuoteTok ;

FROM M2Options IMPORT CompilerDebugging, Verbose,
                      Profiling, Coding, Optimizing ;

FROM M2Quads IMPORT PushT, GetLastQuadNo,
      	       	    QuadToTokenNo, QuadToLineNo,
                    GetLastFileQuad, GetQuad, QuadOperator,
                    SetOptionProfiling, SetOptionCoding,
                    SetOptionOptimizing ;

FROM M2Students IMPORT CheckForVariableThatLooksLikeKeyword ;
FROM NameKey IMPORT MakeKey, GetKey, WriteKey, LengthKey, NulName ;
FROM FIO IMPORT File, OpenToRead, Close, ReadChar, IsNoError ;
FROM StdIO IMPORT Write ;
FROM StrIO IMPORT ReadString, WriteString, WriteLn ;
FROM NumberIO IMPORT CardToStr, WriteCard, WriteOct ;
FROM ASCII IMPORT nul, tab, nl, cr ;
FROM StrLib IMPORT StrConCat, StrCopy, StrEqual, StrLen ;


CONST

   (* UNIX M-2 *)
   EOL           = nl ;

   MaxSourceSize = 50000  ;
   MaxTokenSize  =  8192  ;
   MaxBuffer     =   500  ;
   MaxFileName   =  8192  ;
   eof           =   032C ;
   MaxStack      =   500  ;
   MaxString     =  8192  ;
   MaxFiles      = 10000  ;

TYPE
   TokenInfo        = RECORD
                         TokName : CARDINAL ;
                         TokType : TypeOfToken ;
                         token   : toktype ;
                      END ;

   TokenAndPosition = RECORD
                         Token     : TokenInfo ;
                         Relative,      (* relative token count since file *)
                         LineNumber: CARDINAL ; (* current line number     *)
                         CharOffset: CARDINAL ; (* character offset of     *)
                                                (* token.                  *)
                      END ;

   FileModuleToken = RECORD
                        LineNo,
                        TokenNumber: CARDINAL ; (* start token number      *)
                        FileName,               (* name of #line file      *)
                        ModuleName : CARDINAL ; (* name of module          *)
                     END ;

VAR
   f: File ;
   FileName      : ARRAY [0..MaxFileName] OF CHAR ;   (* Current file name *)
   Opened        : BOOLEAN ;
   CurrentChar   : CHAR ;
   NextChar      : CHAR ;
   Eof           : BOOLEAN ;   (* End of source file.                      *)
   InQuotes      : BOOLEAN ;   (* If we are in quotes.                     *)
   QuoteChar     : CHAR ;      (* Quote character expected.                *)
   Stack         : ARRAY [0..MaxStack] OF TokenAndPosition ;
   StackPtr      : CARDINAL ;
   TokenLength   : CARDINAL ;
   TokenSource   : ARRAY [0..MaxSourceSize] OF TokenAndPosition ;
   IndexToSource : CARDINAL ;  (* How far through the source file in P 2&3 *)
   SourceSize    : CARDINAL ;  (* SourceSize in tokens.                    *)
   FoundEofToken : BOOLEAN ;   (* End of file token found.                 *)
   TokenNumber   : CARDINAL ;  (* nth Token ever read.                     *)
   TokenFileStart: CARDINAL ;  (* token count at the start of the currnet  *)
                               (* source file.                             *)
   CurrentPass   : CARDINAL ;  (* pass number of the compiler, used to     *)
                               (* determine whether TokenNumber should be  *)
                               (* reset.                                   *)
   LastNearToken1,
   LastNearToken2: CARDINAL ;  (* the last symbol which is a canditate for *)
                               (* the next error. Used by M2EvalSym.       *)
   ErrorNewLine  : BOOLEAN ;   (* written a newline on the error stream.   *)
   NoOfFiles     : CARDINAL ;  (* number of files held in the FileList     *)
   FileList      : ARRAY [0..MaxFiles] OF FileModuleToken ;
   InError       : BOOLEAN ;   (* are we already processing an error?      *)
   MultiLineQuote: BOOLEAN ;   (* can we handle strings that span multiple *)
                               (* lines?                                   *)
   ModuleName    : CARDINAL ;  (* the name of the module being parsed      *)


(* Buffer variables - declared at top of module because of the 1 pass p2c  *)

VAR
   BufferSource  : ARRAY [0..MaxBuffer-1] OF CHAR ; (* Source buffer       *)
   BufferIndex   : CARDINAL ;  (* End of the buffer.                       *)
   BufferOffset  : CARDINAL ;  (* Where the buffer starts within the file. *)
   BufferSize    : CARDINAL ;  (* Amount of the buffer currently used.     *)
   OutBufferIndex: CARDINAL ;  (* Specifies the character where the        *)
                               (* GetBuffer routine has read.              *)
   SourceIndex   : CARDINAL ;  (* Index into the source file, nth char     *)
   CurSymLine    : CARDINAL ;  (* Line number of the CurrentSymbol         *)
   NextSymLine   : CARDINAL ;  (* Line number of the next symbol, only     *)
                               (* valid during pass 1.                     *)
   LastCurSymLine: CARDINAL ;  (* Last line of error - used if an error    *)
                               (* requires two lines of source to identify *)
                               (* it.                                      *)
   TokenIndex    : CARDINAL ;  (* Index into the source file.              *)


(* forward declarations necessary for p2c                                  *)

(* %%%FORWARD%%%
PROCEDURE WriteModuleLine (a: ARRAY OF CHAR) ; FORWARD ;
PROCEDURE ProcessFile ; FORWARD ;
PROCEDURE ProcessLine ; FORWARD ;
PROCEDURE GetTokenErrorPass ; FORWARD ;
PROCEDURE WriteFileName ; FORWARD ;
PROCEDURE FindModuleFromToken (TokNo: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE WriteToken (Token: CARDINAL; Type: TypeOfToken;
                      Line: CARDINAL) ; FORWARD ;
PROCEDURE ProcessDate ; FORWARD ;
PROCEDURE AddTokenToSource (Name: CARDINAL; Type: TypeOfToken;
                            Line: CARDINAL; Count: CARDINAL;
                            token: toktype) ; FORWARD ;
PROCEDURE AdvanceChar ; FORWARD ;
PROCEDURE BuildString (VAR a: ARRAY OF CHAR; VAR i: CARDINAL) ; FORWARD ;
PROCEDURE CheckIndexToSource (p: CARDINAL) ; FORWARD ;
PROCEDURE CodeQuads ; FORWARD ;
PROCEDURE ConsumeAndDisplayTokens ; FORWARD ;
PROCEDURE ConsumeSpaces ; FORWARD ;
PROCEDURE Delimiter() : BOOLEAN ; FORWARD ;
PROCEDURE Digit (ch: CHAR) : BOOLEAN ; FORWARD ;
PROCEDURE DisplayBuffer ; FORWARD ;
PROCEDURE DisplayBufferVars ; FORWARD ;
PROCEDURE DisplayRestOfBuffer (OffsetIntoBuffer: CARDINAL;
                               OffsetIntoSource: CARDINAL) ; FORWARD ;
PROCEDURE DisplayUpToToken (VAR OffsetIntoBuffer: CARDINAL ;
                            VAR OffsetIntoSource: CARDINAL) : CARDINAL ; FORWARD ;
PROCEDURE DoubleDelimiter () : BOOLEAN ; FORWARD ;
PROCEDURE FindNextLineInBuffer (VAR OffsetIntoBuffer: CARDINAL;
                                VAR OffsetIntoSource: CARDINAL) ; FORWARD ;
PROCEDURE GetBuffer (VAR ch: CHAR) : BOOLEAN ; FORWARD ;
PROCEDURE GetTokenPass1 ; FORWARD ;
PROCEDURE GetTokenPass2 ; FORWARD ;
PROCEDURE GetTokenPass3 ; FORWARD ;
PROCEDURE HandleQuotes (VAR a: ARRAY OF CHAR; VAR i: CARDINAL) ; FORWARD ;
PROCEDURE HexDigit (ch: CHAR) : BOOLEAN ; FORWARD ;
PROCEDURE Init ; FORWARD ;
PROCEDURE InitEofState ; FORWARD ;
PROCEDURE InitIndexToSource ; FORWARD ;
PROCEDURE InitToken ; FORWARD ;
PROCEDURE IsInRange (i, r1, r2: CARDINAL) : BOOLEAN ; FORWARD ;
PROCEDURE OpenFileDuringCodeGeneration ; FORWARD ;
PROCEDURE OpenPass1 (file: ARRAY OF CHAR; module: CARDINAL) ; FORWARD ;
PROCEDURE OpenPass2 (file: ARRAY OF CHAR; module: CARDINAL) ; FORWARD ;
PROCEDURE OpenPass3 (file: ARRAY OF CHAR; module: CARDINAL) ; FORWARD ;
PROCEDURE OptimizeCode ; FORWARD ;
PROCEDURE ParseComment ; FORWARD ;
PROCEDURE ParseIdent (VAR a: ARRAY OF CHAR; VAR i: CARDINAL) ; FORWARD ;
PROCEDURE ParseNumber (VAR a: ARRAY OF CHAR; VAR i: CARDINAL;
                       VAR Type: TypeOfToken; VAR token: toktype) ; FORWARD ;
PROCEDURE ParseOption ; FORWARD ;
PROCEDURE ProfileCode ; FORWARD ;
PROCEDURE PutBuffer (ch: CHAR) ; FORWARD ;
PROCEDURE ReadToken (VAR Name: CARDINAL;
                     VAR Type: TypeOfToken;
                     VAR Index: CARDINAL;
                     VAR TokLine: CARDINAL;
                     VAR token: toktype) ; FORWARD ;
PROCEDURE ReadTokenFromSource (VAR Name: CARDINAL; VAR Type: TypeOfToken;
                               VAR Line: CARDINAL; VAR Count: CARDINAL;
                               VAR token: toktype) ; FORWARD ;
PROCEDURE SetTokenLength ; FORWARD ;
PROCEDURE StartOfIndexInBuffer () : CARDINAL ; FORWARD ;
PROCEDURE WriteBuffer ; FORWARD ;
PROCEDURE WriteErrorAtToken (n: CARDINAL) ; FORWARD ;
PROCEDURE WriteErrorAtTokens (n1, n2: CARDINAL) ; FORWARD ;
PROCEDURE WriteNChars (ch: CHAR; n: CARDINAL) ; FORWARD ;
PROCEDURE FindModuleFileFromToken (tokno: CARDINAL;
                                   VAR modname, filename: CARDINAL) ; FORWARD ;
   %%%FORWARD%%% *)



(*
   MultiLineQuoteOn - turns on the ability to handle strings that span multiple
                      lines.
*)

PROCEDURE MultiLineQuoteOn ;
BEGIN
   MultiLineQuote := TRUE
END MultiLineQuoteOn ;


(*
   MultiLineQuoteOff - turns off the ability to handle strings that span multiple
                       lines. (The default).
*)

PROCEDURE MultiLineQuoteOff ;
BEGIN
   MultiLineQuote := FALSE
END MultiLineQuoteOff ;


(*
   GetFileName - assigns, a, to the current file name.
*)

PROCEDURE GetFileName (VAR a: ARRAY OF CHAR) ;
BEGIN
   StrCopy(FileName, a)
END GetFileName ;


(*
   AddTokenToSource - adds a Token to the source buffer, ready for Pass 2.
*)

PROCEDURE AddTokenToSource (Name: CARDINAL; Type: TypeOfToken;
                            Line: CARDINAL; Count: CARDINAL; token: toktype) ;
BEGIN
   IF IsPass1()
   THEN
      IF SourceSize>MaxSourceSize
      THEN
         InternalError('increase MaxSourceSize', __FILE__, __LINE__)
      ELSE
         WITH TokenSource[SourceSize] DO
            Token.TokName := Name ;
            Token.TokType := Type ;
            Token.token   := token ;
            LineNumber    := Line ;
            Relative      := Count-TokenFileStart
         END ;
         INC(SourceSize)
      END
   END
END AddTokenToSource ;


(*
   ReadTokenFromSource - reads a Token and Type from the source buffer.
*)

PROCEDURE ReadTokenFromSource (VAR Name : CARDINAL; VAR Type: TypeOfToken;
                               VAR Line : CARDINAL; VAR Count: CARDINAL;
                               VAR token: toktype) ;
BEGIN
   IF IndexToSource<SourceSize
   THEN
      WITH TokenSource[IndexToSource] DO
         Name  := Token.TokName ;
         Type  := Token.TokType ;
         Line  := LineNumber ;
         Count := Relative+TokenFileStart ;
         token := Token.token
      END ;
      INC(IndexToSource)
   ELSE
      WriteString('Exceeded source buffer') ; HALT
   END
END ReadTokenFromSource ;


(*
   GetPreviousTokenLineNo - returns the line number of the previous token.
*)

PROCEDURE GetPreviousTokenLineNo () : CARDINAL ;
BEGIN
   IF IndexToSource<=StackPtr+2
   THEN
      RETURN( 1 )
   ELSE
      WITH TokenSource[IndexToSource-StackPtr-2] DO
         (* -2 because current token is at position -1 *)
(*
         WriteString('Previous token is ') ; WriteKey(Token.TokName) ;
         WriteString(' on line') ; WriteCard(LineNumber, 6) ;
         WriteLn ;
*)
         RETURN( LineNumber )
      END
   END
END GetPreviousTokenLineNo ;


(*
   GetLineNo - returns the line number where the current token exists
               in the source file.
*)

PROCEDURE GetLineNo () : CARDINAL ;
BEGIN
   RETURN( CurSymLine )
END GetLineNo ;


(*
   GetTokenNo - returns the number of tokens read from
                the source file by the lexical analaysis.
*)

PROCEDURE GetTokenNo () : CARDINAL ;
BEGIN
   RETURN( TokenNumber-StackPtr )
END GetTokenNo ;


(*
   PedanticError - displays the source line and points to the symbol in error.
                   providing the Pedantic flag was set on the user command
                   line.
                   The message, a, is displayed.
*)

PROCEDURE PedanticError (a: ARRAY OF CHAR) ;
BEGIN
   IF Pedantic
   THEN
      WriteError(a)
   END
END PedanticError ;


(*
   WriteErrorCodeGeneration - writes an error for the code generation
                              pass.
*)

PROCEDURE WriteErrorCodeGeneration (a: ARRAY OF CHAR) ;
VAR
   q: CARDINAL ;
BEGIN
   (* capture last quad referenced before we examine more quads in Open below *)
   q := GetLastQuadNo() ;
   (*
      Special case since we must check the Quadruples to see
      which file produced the quadruple causing the error.
   *)
   OpenFileDuringCodeGeneration ;
   IF Verbose
   THEN
      Title(a, FileName, '', QuadToLineNo(q), 0) ;
      WriteErrorAtToken(QuadToTokenNo(q))
   ELSE
      Title(a, FileName, '', QuadToLineNo(q), 0)
   END
END WriteErrorCodeGeneration ;


(*
   WriteErrorPass123 - write an error during pass 1, 2, 3.
*)

PROCEDURE WriteErrorPass123 (a: ARRAY OF CHAR) ;
VAR
   LastFileName: ARRAY [0..MaxFileName] OF CHAR ;
BEGIN
   IF LastNearToken1=0
   THEN
      Title(a, FileName, '', CurSymLine, 0) ;
      IF Verbose
      THEN
         WriteErrorAtToken(GetTokenNo())
      END
   ELSE
      IF LastNearToken2=0
      THEN
         WriteErrorAtToken(LastNearToken1) ;
         FindFileNameFromToken(FileName, LastNearToken1) ;
         Title(a, FileName, '', TokenToLineNo(LastNearToken1), 0)
      ELSE
         WriteErrorAtTokens(LastNearToken1, LastNearToken2) ;
         FindFileNameFromToken(FileName, LastNearToken1) ;
         FindFileNameFromToken(LastFileName, LastNearToken2) ;
         Title(a, FileName, LastFileName, TokenToLineNo(LastNearToken1), TokenToLineNo(LastNearToken2))
      END
   END
END WriteErrorPass123 ;


(*
   NotAlreadyProcessingError - returns TRUE if we are not already
                               processing an error report.
*)

PROCEDURE NotAlreadyProcessingError (a: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   IF InError
   THEN
      WriteLn ;
      WriteString('error in error report - line number is probably wrong') ;
      WriteLn ;
      WriteString(a) ;
      WriteLn ;
      RETURN( FALSE )
   ELSE
      InError := TRUE ;
      RETURN( TRUE )
   END
END NotAlreadyProcessingError ;


(*
   InternalError - displays an internal error message together with the compiler source
                   file and line number.
*)

PROCEDURE InternalError (a: ARRAY OF CHAR; file: ARRAY OF CHAR; line: CARDINAL) ;
BEGIN
   BeginError ;
   WriteString(file) ; Write(':') ; WriteCard(line, 0) ;
   WriteString(': *** internal error ***  ') ; WriteString(a) ; WriteLn ;
   WriteError('internal error occurred when compiling the following source') ;
   EndError
END InternalError ;


(*
   Format1 - converts string, src, into, dest, together with encapsulated
             entity, n. It only formats the first %s or %d with n.
*)

PROCEDURE Format1 (src: ARRAY OF CHAR; n: CARDINAL; VAR dest: ARRAY OF CHAR) ;
VAR
   HighSrc,
   HighDest,
   i, j    : CARDINAL ;
   str     : ARRAY [0..MaxString] OF CHAR ;
BEGIN
   HighSrc := StrLen(src) ;
   HighDest := HIGH(dest) ;
   i := 0 ;
   j := 0 ;
   WHILE (i<HighSrc) AND (src[i]#nul) AND (j<HighDest) AND (src[i]#'%') DO
      dest[j] := src[i] ;
      INC(i) ;
      INC(j)
   END ;
   IF (i+1<HighSrc) AND (src[i]='%') AND (j<HighDest)
   THEN
      IF src[i+1]='s'
      THEN
         dest[j] := nul ;
         GetKey(n, str) ;
         StrConCat(dest, str, dest) ;
         j := StrLen(dest) ;
         INC(i, 2)
      ELSIF src[i+1]='d'
      THEN
         dest[j] := nul ;
         CardToStr(n, 0, str) ;
         StrConCat(dest, str, dest) ;
         j := StrLen(dest) ;
         INC(i, 2)
      ELSE
         dest[j] := src[i] ;
         INC(i) ;
         INC(j)
      END
   END ;
   (* and finish off copying src into dest *)
   WHILE (i<HighSrc) AND (src[i]#nul) AND (j<HighDest) DO
      dest[j] := src[i] ;
      INC(i) ;
      INC(j)
   END ;
   IF j<HighDest
   THEN
      dest[j] := nul
   END ;
END Format1 ;


(*
   WriteFormat1 - displays the source module and line together
                  with the encapsulated format string.
*)

PROCEDURE WriteFormat1 (a: ARRAY OF CHAR; n: CARDINAL) ;
VAR
   line: ARRAY [0..MaxString] OF CHAR ;
BEGIN
   Format1(a, n, line) ;
   WriteModuleLine(line)
END WriteFormat1 ;


(*
   WriteFormat2 - displays the module and line together with the encapsulated
                  format strings.
*)

PROCEDURE WriteFormat2 (a: ARRAY OF CHAR; n1: CARDINAL; n2: CARDINAL) ;
VAR
   line: ARRAY [0..MaxString] OF CHAR ;
BEGIN
   Format1(a, n1, line) ;
   Format1(line, n2, line) ;
   WriteModuleLine(line)
END WriteFormat2 ;


(*
   WriteErrorFormat1 - displays the source line together with the encapsulated
                       format string.
*)

PROCEDURE WriteErrorFormat1 (a: ARRAY OF CHAR; n: CARDINAL) ;
VAR
   line: ARRAY [0..MaxString] OF CHAR ;
BEGIN
   Format1(a, n, line) ;
   WriteError(line)
END WriteErrorFormat1 ;


(*
   WriteErrorFormat2 - displays the source line together with the encapsulated
                       format strings.
*)

PROCEDURE WriteErrorFormat2 (a: ARRAY OF CHAR; n1: CARDINAL; n2: CARDINAL) ;
VAR
   line: ARRAY [0..MaxString] OF CHAR ;
BEGIN
   Format1(a, n1, line) ;
   Format1(line, n2, line) ;
   WriteError(line)
END WriteErrorFormat2 ;


(*
   FormatWarningMessage2 - displays message, a, with formating arguments, n1, n2, it displays
                           the line number according to the, AtTokenNo.
*)

PROCEDURE FormatWarningMessage2 (a: ARRAY OF CHAR; n1, n2: CARDINAL; AtTokenNo: CARDINAL) ;
VAR
   line: ARRAY [0..MaxString] OF CHAR ;
BEGIN
   FindFileNameFromToken(line, AtTokenNo) ;
   WriteString(line) ; WriteString(':') ; WriteCard(TokenToLineNo(AtTokenNo), 0) ; WriteString(':warning ') ;
   Format1(a, n1, line) ;
   Format1(line, n2, line) ;
   WriteString(line) ; WriteLn
END FormatWarningMessage2 ;


(*
   CheckForUnexpectedEof - checks to see whether EOF has been encountered during
                           parsing and if so prepends a warning.
                           The final message is placed into message and
                           is derived from, a.
*)

PROCEDURE CheckForUnexpectedEof (a: ARRAY OF CHAR; VAR message: ARRAY OF CHAR) ;
BEGIN
   IF CurrentToken=EofTok
   THEN
      StrCopy('unexpected end of file found: ', message) ;
      StrConCat(message, a, message)
   ELSE
      StrCopy(a, message)
   END
END CheckForUnexpectedEof ;


(*
   WriteError - displays the source line and points to the symbol in error.
                The message, a, is displayed.
*)

PROCEDURE WriteError (a: ARRAY OF CHAR) ;
VAR
   i      : CARDINAL ;
   message: ARRAY [0..MaxString] OF CHAR ;
BEGIN
   IF NotAlreadyProcessingError(message)
   THEN
      BeginError ;  (* Sends all error information to the error channel *)
      IF IsPassCodeGeneration()
      THEN
         WriteErrorCodeGeneration(a)
      ELSIF Opened
      THEN
         IF IsPass1() OR IsPass2() OR IsPass3() OR IsPassHidden()
         THEN
            CheckForUnexpectedEof(a, message) ;
            WriteErrorPass123(message)
         ELSE
            WriteString('Should never reach here in M2Lexical') ;
            WriteLn ;
            WriteString('since file is Opened and we are in No pass') ; WriteLn ;
            EndError ;
            HALT
         END
      ELSE
         (* Error occurred when the compiler is not in any pass *)
         InError := FALSE ;  (* so that LastError doesn't ignore the error *)
         CheckForUnexpectedEof(a, message) ;
         LastError(message)
      END ;
      FlushError ;
      EndError ;  (* Return the output channel to the previous state *)
      HALT
   END
END WriteError ;


(*
   LastError - writes an error message at the LastNearToken1 position,
               indicated by the last call to NearToken.
*)

PROCEDURE LastError (a: ARRAY OF CHAR) ;
VAR
   LastFileName: ARRAY [0..MaxFileName] OF CHAR ;
BEGIN
   IF NotAlreadyProcessingError(a)
   THEN
      BeginError ;  (* Sends all error information to the error channel *)
      IF LastNearToken1=0
      THEN
         Title(a, FileName, '', 0, 0)
      ELSE
         WriteErrorAtTokens(LastNearToken1, LastNearToken2) ;
         FindFileNameFromToken(FileName, LastNearToken1) ;
         FindFileNameFromToken(LastFileName, LastNearToken2) ;
         Title(a, FileName, LastFileName, CurSymLine, LastCurSymLine)
      END ;
      FlushError ;
      EndError ;  (* Return the output channel to the previous state *)
      HALT
   END
END LastError ;


(*
   WriteModuleLine - writes a message at the LastNearToken1 position,
                     indicated by the last call to NearToken.
*)

PROCEDURE WriteModuleLine (a: ARRAY OF CHAR) ;
VAR
   LastFileName: ARRAY [0..MaxFileName] OF CHAR ;
BEGIN
   BeginError ;  (* Sends all error information to the error channel *)
   IF IsPass1()
   THEN
      Title(a, FileName, '', CurSymLine, 0)
   ELSIF LastNearToken1=0
   THEN
      Title(a, FileName, '', 0, 0)
   ELSE
      FindFileNameFromToken(FileName, LastNearToken1) ;
      FindFileNameFromToken(LastFileName, LastNearToken2) ;
      Title(a, FileName, LastFileName, TokenToLineNo(LastNearToken1), TokenToLineNo(LastNearToken2))
   END ;
   FlushError ;
   EndError   (* Return the output channel to the previous state *)
END WriteModuleLine ;


(*
   NearToken - informs the lexical analyser which token to examine
               when an error occurs and we are not in a pass.
               If the string, a, is not zero in length then write
               the error now.
*)

PROCEDURE NearToken (a: ARRAY OF CHAR; token: CARDINAL) ;
BEGIN
   IF StrEqual(a, '')
   THEN
      LastNearToken1 := token ;
      LastNearToken2 := 0
   ELSE
      IF NotAlreadyProcessingError(a)
      THEN
         BeginError ;  (* Sends all error information to the error channel *)
         FindFileNameFromToken(FileName, token) ;
         Title(a, FileName, '', TokenToLineNo(token), 0) ;
         WriteErrorAtToken(token) ;
         FlushError ;
         EndError ;  (* Return the output channel to the previous state *)
         HALT
      END
   END
END NearToken ;


(*
   FindFileNameFromToken - fills in the complete FileName for the appropriate
                           name given the token, Token.
*)

PROCEDURE FindFileNameFromToken (VAR FileName: ARRAY OF CHAR;
                                 Token: CARDINAL) ;
VAR
   FileKey,
   Module  : CARDINAL ;
BEGIN
   FindModuleFileFromToken(Token, Module, FileKey) ;
   GetKey(FileKey, FileName)
END FindFileNameFromToken ;


(*
   NearTokens - informs the lexical analyser which two tokens to examine
                when an error occurs and we are not in a pass.
                If the string, a, is not zero in length then write
                the error now.
*)

PROCEDURE NearTokens (a: ARRAY OF CHAR; token1, token2: CARDINAL) ;
VAR
   Module: CARDINAL ;
   File  : ARRAY [0..MaxFileName] OF CHAR ;
BEGIN
   IF StrEqual(a, '')
   THEN
      LastNearToken1 := token1 ;
      LastNearToken2 := token2 ;
   ELSE
      IF NotAlreadyProcessingError(a)
      THEN
         BeginError ;
         FindFileNameFromToken(FileName, token1) ;
         FindFileNameFromToken(File, token2) ;
         Title(a, FileName, File, TokenToLineNo(token1), TokenToLineNo(token2)) ;
         WriteErrorAtTokens(token1, token2) ;
         FlushError ;
         EndError ;
         HALT
      END
   END
END NearTokens ;


(*
   OpenFileDuringCodeGeneration - opens the source file which is causing
                                  the error during the code generation
                                  phase. Examine the last quadruple accessed
                                  scan back for the appropriate File quad
                                  and open this source file.
*)

PROCEDURE OpenFileDuringCodeGeneration ;
VAR
   Operand1,
   Operand2,
   Operand3,
   FileQuad: CARDINAL ;
   Operator: QuadOperator ;
   Name,
   FullPath: ARRAY [0..MaxFileName] OF CHAR ;
BEGIN
   FileQuad := GetLastFileQuad(GetLastQuadNo()) ;
   GetQuad(FileQuad, Operator, Operand1, Operand2, Operand3) ;
   GetKey(GetSymName(Operand3), Name) ;
   IF Operator=StartDefFileOp
   THEN
      StrConCat(Name, '.def', Name)
   ELSE
      StrConCat(Name, '.mod', Name)
   END ;
   IF FindSourceFile(Name, FullPath)
   THEN
      StrCopy(FullPath, FileName)
   ELSE
      WriteString('Failed to find source to module ') ;
      WriteString(Name) ; WriteLn ;
      WriteString('whilst writing an error message in the code generation pass') ;
      WriteLn ;
      HALT
   END ;
END OpenFileDuringCodeGeneration ;


(*
   TokenToLineNo - returns the line number of the current file for the
                   Token.
*)

PROCEDURE TokenToLineNo (Token: CARDINAL) : CARDINAL ;
BEGIN
   RETURN( TokenSource[Token].LineNumber )
END TokenToLineNo ;


(*
   WriteErrorAtToken - writes the offending piece of source text to
                       default output with the nth Token
                       underlined.
*)

PROCEDURE WriteErrorAtToken (n: CARDINAL) ;
VAR
   Count,
   i        : CARDINAL ;
   Tok, Line: CARDINAL ;
   Type     : TypeOfToken ;
   Name     : ARRAY [0..MaxFileName] OF CHAR ;
   File,
   Module   : CARDINAL ;
BEGIN
   (* WriteString('TokenNumber =') ; WriteCard(TokenNumber, 4) ; WriteLn ; *)
   FindFileNameFromToken(FileName, n) ;
   Count := TokenSource[n].Relative ;
   IF NOT IsNoPass()
   THEN
      SetPassToNoPass
   END ;
   SetPassToErrorPass ;  (* Read the source file as if pass 1 - for the text *)
   FindModuleFileFromToken(n, Module, File) ;
   IF Module=NulName
   THEN
      BeginError ;
      WriteString('error in error handler module name is nul') ; WriteLn ;
      EndError ;
      HALT
   END ;
   GetKey(Module, Name) ;
   IF StrEqual(Name, '')
   THEN
      WriteString('error in error handler module name is nul') ; WriteLn ;
      EndError ;
      HALT
   ELSE
      IF FindSourceFile(FileName, FileName)
      THEN
      END ;
      f := OpenToRead(FileName) ;
      IF IsNoError(f)
      THEN
         Init ;
         InitEofState ;
         InitToken ;
         CheckIndexToSource(0) ;  (* Guarentee to set TokenNumber to zero *)
         InError := TRUE ;

         IF Count>0
         THEN
            DEC(Count)   (* TokenNumber starts at zero, count starts at one *)
         END ;

         WHILE (TokenNumber<Count) AND (NOT Eof) DO
            GetToken
         END ;
         IF Verbose
         THEN
            WriteBuffer
         END ;
         Close(f)
      ELSE
         WriteString('unable to open ') ; WriteString(FileName) ;
         WriteString(' to display error') ; WriteLn
      END ;
      SetPassToNoPass
   END
END WriteErrorAtToken ;


(*
   WriteErrorAtTokens - writes the offending piece of source text to
                        default output with the n1 th and n2 th Token
                        underlined.
*)

PROCEDURE WriteErrorAtTokens (n1, n2: CARDINAL) ;
BEGIN
   WriteErrorAtToken(n1) ;
   IF (n2#0) AND (n1#n2)
   THEN
      LastCurSymLine := CurSymLine ;
      IF Verbose
      THEN
         WriteFileName ;
         WriteString('------------------------------------------') ; WriteLn
      END ;
      WriteErrorAtToken(n2)
   END
END WriteErrorAtTokens ;


(*
   ConsumeAndDisplayTokens - consumes the rest of the available tokens
                             in the current pass.
*)

PROCEDURE ConsumeAndDisplayTokens ;
BEGIN
   WriteString('up to:') ; WriteLn ;
   WHILE NOT FoundEofToken DO
      WriteKey(CurrentToken) ;
      GetToken
   END
END ConsumeAndDisplayTokens ;


(*
   SetTokenLength - sets the global variable, TokenLength, to the length
                    of the global variable, CurrentToken.
*)

PROCEDURE SetTokenLength ;
BEGIN
   TokenLength := LengthKey(CurrentToken)
END SetTokenLength ;


(*
   WriteToken - displays the token and its type. A debugging measure.
*)

PROCEDURE WriteToken (Token: CARDINAL; Type: TypeOfToken; Line: CARDINAL) ;
VAR
   a: ARRAY [0..MaxTokenSize] OF CHAR ;
BEGIN
   WriteString('[') ; WriteCard(TokenNumber, 0) ;
   WriteString('] **** ') ;
   GetKey(Token, a) ;
   WriteString(a) ; WriteString('  ') ;
   WriteCard(Token, 4) ;
   WriteString('   **** ') ;
   IF Type=IdentToken
   THEN
      WriteString('Ident Token')
   ELSIF Type=ReservedToken
   THEN
      WriteString('Reserved Token')
   ELSIF Type=RealToken
   THEN
      WriteString('Real Token')
   ELSE
      WriteString('Integer Token')
   END ;
   WriteString(' on line') ; WriteCard(Line, 4) ;
   WriteLn
END WriteToken ;


(*
   TypeToCard - Converts the Type to a CARDINAL.
                Care must be taken when using this procedure.
*)

PROCEDURE TypeToCard (Type: TypeOfToken) : CARDINAL ;
BEGIN
   RETURN (ORD(Type))
END TypeToCard ;


(*
   CardToType - Converts the CARDINAL to a TypeOfToken type.
                Care must be taken when using this procedure.
*)

PROCEDURE CardToType (c: CARDINAL) : TypeOfToken ;
BEGIN
   RETURN (VAL(TypeOfToken, c))
END CardToType ;


(*
   TokenIs - if Name is equal to the CurrentToken the next Token is read
             and true is returned, otherwise false is returned.
*)

PROCEDURE TokenIs (Name: CARDINAL) : BOOLEAN ;
BEGIN
   IF Name=CurrentToken
   THEN
      GetToken ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END TokenIs ;


(*
   IsToken - returns the result of the comparison between CurrentToken
             and Name.
*)

PROCEDURE IsToken (Name: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Name=CurrentToken )
END IsToken ;


(*
   AddModuleFileToken - records the current module and current filename with the token
                        start number.
*)

PROCEDURE AddModuleFileToken (modname, filename, lineno: CARDINAL; tokno: CARDINAL) ;
BEGIN
   IF NoOfFiles=MaxFiles
   THEN
      WriteString('increase MaxFiles in M2Lexical.mod') ; WriteLn ;
      HALT
   ELSE
      WITH FileList[NoOfFiles] DO
         TokenNumber := tokno ;
         ModuleName  := modname ;
         FileName    := filename ;
         LineNo      := lineno
      END ;
      INC(NoOfFiles)
   END
END AddModuleFileToken ;


(*
   FindModuleFileFromToken - returns the file name and module name from a
                             given token number, tokno.
*)

PROCEDURE FindModuleFileFromToken (tokno: CARDINAL;
                                   VAR modname, filename: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   WHILE i<NoOfFiles DO
      IF FileList[i].TokenNumber>tokno
      THEN
         IF i>0
         THEN
            modname  := FileList[i-1].ModuleName ;
            filename := FileList[i-1].FileName ;
            RETURN
         ELSE
            modname  := NulName ;
            filename := NulName
         END
      END ;
      INC(i)
   END ;
   IF NoOfFiles>0
   THEN
      modname  := FileList[NoOfFiles-1].ModuleName ;
      filename := FileList[NoOfFiles-1].FileName
   ELSE
      modname  := NulName ;
      filename := NulName
   END
END FindModuleFileFromToken ;


(*
   OpenSource - Attempts to open the source file, a.
                The success of the operation is returned.
*)

PROCEDURE OpenSource (a: ARRAY OF CHAR; module: CARDINAL) : BOOLEAN ;
VAR
   FullPath: ARRAY [0..MaxFileName] OF CHAR ;
BEGIN
   IF FindSourceFile(a, FullPath)
   THEN
   END ;
   IF IsPass1()
   THEN
      AddModuleFileToken(module, MakeKey(a), 1, GetTokenNo()) ;
      OpenPass1(FullPath, module)
   ELSIF IsPass2()
   THEN
      OpenPass2(FullPath, module)
   ELSIF IsPass3() OR IsPassHidden()
   THEN
      OpenPass3(FullPath, module)
   ELSE
      WriteError('Open called outside Pass 1, Pass 2 and Pass 3')
   END ;
   RETURN( Opened )
END OpenSource ;


(*
   DumpTokenInfo - 
*)

PROCEDURE DumpTokenInfo ;
BEGIN
   WriteString('module: ') ; WriteString(FileName) ; WriteString('  TokenNumber = ') ; WriteCard(TokenNumber, 6) ;
   WriteLn
END DumpTokenInfo ;


(*
   OpenPass1 - opens the source, file, for reading.
*)

PROCEDURE OpenPass1 (file: ARRAY OF CHAR; module: CARDINAL) ;
BEGIN
   f := OpenToRead(file) ;
   IF IsNoError(f)
   THEN
      StrCopy(file, FileName) ;
      Opened := TRUE ;
      ModuleName := module ;
      InitEofState ;
      InitToken ;
      CheckIndexToSource(1)
   ELSE
      Opened := FALSE ;
      Eof := TRUE
   END
END OpenPass1 ;


(*
   OpenPass2 - opens, file, for reading during pass 2.
               In reality the source file is not opened, the
               tokens stored in the TokenSource array during Pass 1 are
               read from this array in Pass 2.
*)

PROCEDURE OpenPass2 (file: ARRAY OF CHAR; module: CARDINAL) ;
BEGIN
   ModuleName := module ;
   StrCopy(file, FileName) ;
   InitEofState ;
   CheckIndexToSource(2) ;
   Opened := TRUE ;
   IF StackPtr>0
   THEN
      WriteError('Check this... StackPtr should be 0 at beginning of a Pass')
   END ;
   GetToken
END OpenPass2 ;


(*
   OpenPass3 - opens, file, for reading during pass 3.
               In reality the source file is not opened, the
               tokens stored in the TokenSource array during Pass 1 are
               read from this array in Pass 3.
*)

PROCEDURE OpenPass3 (file: ARRAY OF CHAR; module: CARDINAL) ;
BEGIN
   ModuleName := module ;
   StrCopy(file, FileName) ;
   InitEofState ;
   CheckIndexToSource(3) ;
   Opened := TRUE ;
   IF StackPtr>0
   THEN
      WriteError('Check this... StackPtr should be 0 at beginning of a Pass')
   END ;
   GetToken
END OpenPass3 ;


(*
   CheckIndexToSource - checks to see whether InitIndexToSource should be called
                        if the CurrentPass has changed.
*)

PROCEDURE CheckIndexToSource (p: CARDINAL) ;
BEGIN
   IF CurrentPass#p
   THEN
      CurrentPass := p ;
      InitIndexToSource
   END
END CheckIndexToSource ;


(*
   InitIndexToSource - initializes the variable IndexToSource.
                       This procedure is called whenever a Pass changes,
                       in which case we want to go back to the initial
                       source file read into the token buffer.
*)

PROCEDURE InitIndexToSource ;
BEGIN
   IndexToSource := 0 ;
   TokenNumber := 0 ;
   TokenFileStart := 0
END InitIndexToSource ;


(*
   InitEofState - initializes the end of file state to be false and
                  sets other indices to the beginning of the file.
*)

PROCEDURE InitEofState ;
BEGIN
   Eof := FALSE ;
   FoundEofToken := FALSE ;
   NextChar := ' ' ;
   CurSymLine := 0 ;
   NextSymLine := 0 ;
   TokenFileStart := TokenNumber
END InitEofState ;


(*
   Init - initializes the global variables used within this module.
*)

PROCEDURE Init ;
BEGIN
   (* WriteString('Initialization of M2Lexical') ; WriteLn ; *)
   NextChar := ' ' ;  (* Thrown away - needs to be initialised. *)
   CurSymLine := 0 ;
   NextSymLine := 0 ;
   StackPtr := 0 ;
   InQuotes := FALSE ;
   TokenLength := 0 ;
   BufferSize := 0 ;
   BufferIndex := 0 ;
   BufferOffset := 0 ;
   SourceIndex := 0 ;
   OutBufferIndex := 0 ;
   TokenIndex := 0 ;
   SourceSize := 0 ;
   CurrentPass := 0 ;
   ErrorNewLine := TRUE ;
   TokenNumber := 0 ;
   InError := FALSE ;
   MultiLineQuoteOff ;
   ExitOnHalt(1)
END Init ;


(*
  InitToken - reads the first token from the source file into CurrentToken.
*)

PROCEDURE InitToken ;
BEGIN
   PutBuffer(EOL) ; (* EOL determines end of the last line for WriteBuffer *)
   AdvanceChar ;
   AdvanceChar ;
   GetToken
END InitToken ;


(*
   CloseSource - Closes the current open file.
*)

PROCEDURE CloseSource ;
BEGIN
   IF IsPass1()
   THEN
      IF Opened=TRUE
      THEN
         WHILE NOT FoundEofToken DO
            GetToken
         END ;
         Opened := FALSE ;
         Close( f )
      END
   ELSIF IsPass2() OR IsPass3() OR IsPassHidden()
   THEN
      IF Opened=TRUE
      THEN
         WHILE NOT FoundEofToken DO
            GetToken
         END ;
         Opened := FALSE
      END
   ELSE
      WriteError('CloseFile called outside Pass 1, Pass 2 and Pass 3')
   END
END CloseSource ;


(*
   GetToken - gets the next token into CurrentToken.
*)

PROCEDURE GetToken ;
BEGIN
   IF StackPtr>0
   THEN
      DEC(StackPtr) ;
      WITH Stack[StackPtr] DO
         CurrentToken := Token.TokName ;
         TokenType    := Token.TokType ;
         CurSymLine   := LineNumber ;
         TokenIndex   := CharOffset ;
         currenttoken := Token.token
      END
   ELSE
      IF IsPass1()
      THEN
         GetTokenPass1
      ELSIF IsPass2()
      THEN
         GetTokenPass2
      ELSIF IsPass3() OR IsPassHidden()
      THEN
         GetTokenPass3
      ELSIF IsErrorPass()
      THEN
         GetTokenErrorPass
      ELSE
         WriteError('GetToken should only be called during Pass 1, Pass 2 and Pass 3')
      END
   END ;
   SetTokenLength ;
   IF CompilerDebugging
   THEN
      WriteToken(CurrentToken, TokenType, CurSymLine)
   END
END GetToken ;


(*
   GetTokenPass1 - reads the next token providing that EofTok was not found.
                   The read token is then added to the pass 2 token buffer.
*)

PROCEDURE GetTokenPass1 ;
VAR
   n: CARDINAL ;
BEGIN
   IF FoundEofToken
   THEN
      currenttoken := eoftok
   ELSE
      ReadToken(CurrentToken, TokenType, TokenIndex, CurSymLine, currenttoken) ;
      INC(TokenNumber) ;
      (* ; WriteString('TokenIndex => ') ; Write(BufferSource[TokenIndex MOD MaxBuffer]) ; WriteLn ; *)
      IF TokenType=ReservedToken
      THEN
         IF CurrentToken=DateTok
         THEN
            ProcessDate
         ELSIF CurrentToken=FileTok
         THEN
            ProcessFile
         ELSIF CurrentToken=LineTok
         THEN
            ProcessLine
         END
      END ;
      AddTokenToSource(CurrentToken, TokenType, CurSymLine, TokenNumber, currenttoken) ;
(*
      IF (TokenType=ReservedToken) AND (CurrentToken=AsmTok)
      THEN
         ProcessInlineCode
      END ;
*)
      IF CurrentToken=EofTok
      THEN
         (* WriteLn ;
         WriteString('******* EOF FOUND PASS 1 *******') ; WriteLn ;
         *)
         FoundEofToken := TRUE
      END
   END
END GetTokenPass1 ;


(*
   GetTokenErrorPass - reads the next token providing that EofTok was not found.
*)

PROCEDURE GetTokenErrorPass ;
BEGIN
   IF NOT FoundEofToken
   THEN
      ReadToken(CurrentToken, TokenType, TokenIndex, CurSymLine, currenttoken) ;
      INC(TokenNumber) ;
    (* ; WriteString('TokenIndex => ') ; Write(BufferSource[TokenIndex MOD MaxBuffer]) ; WriteLn ; *)
      IF TokenType=ReservedToken
      THEN
         IF CurrentToken=DateTok
         THEN
            ProcessDate
         ELSIF CurrentToken=FileTok
         THEN
            ProcessFile
         ELSIF CurrentToken=LineTok
         THEN
            ProcessLine
         END
      END ;
(*
      IF (TokenType=ReservedToken) AND (CurrentToken=AsmTok)
      THEN
         ProcessInlineCode
      END ;
*)
      IF CurrentToken=EofTok
      THEN
         (* WriteLn ;
         WriteString('******* EOF FOUND ERROR PASS *******') ; WriteLn ;
         *)
         FoundEofToken := TRUE
      END
   END
END GetTokenErrorPass ;


(*
   GetTokenPass2 - reads the next token providing that FoundEofToken has not
                   been found.
*)

PROCEDURE GetTokenPass2 ;
BEGIN
   IF NOT FoundEofToken
   THEN
      ReadTokenFromSource(CurrentToken, TokenType, CurSymLine, TokenNumber, currenttoken) ;
      IF CurrentToken=EofTok
      THEN
         (*
            WriteLn ;
            WriteString('******* EOF FOUND PASS 2 *******') ; WriteLn ;
         *)
         FoundEofToken := TRUE
      END
   END
END GetTokenPass2 ;


(*
   GetTokenPass3 - reads the next token providing that FoundEofToken has not
                   been found.
*)

PROCEDURE GetTokenPass3 ;
BEGIN
   IF NOT FoundEofToken
   THEN
      ReadTokenFromSource(CurrentToken, TokenType, CurSymLine, TokenNumber, currenttoken) ;
      IF CurrentToken=EofTok
      THEN
         (*
            WriteLn ;
            WriteString('******* EOF FOUND PASS 3 *******') ; WriteLn ;
         *)
         FoundEofToken := TRUE
      END
   END
END GetTokenPass3 ;


(*
   InsertToken - inserts a symbol, Name, infront of the current token
                 ready for the next pass. An attempt at syntax error correction.
*)

PROCEDURE InsertToken (Name: CARDINAL; Type: TypeOfToken; token: toktype) ;
BEGIN
   IF IsPass1()
   THEN
      IF SourceSize>MaxSourceSize
      THEN
         InternalError('increase MaxSourceSize', __FILE__, __LINE__)
      ELSE
         IF SourceSize>0
         THEN
            TokenSource[SourceSize] := TokenSource[SourceSize-1] ;
            WITH TokenSource[SourceSize-1] DO
               Token.TokName := Name ;
               Token.TokType := Type ;
               Token.token   := token
               (* we leave LineNumber and Relative alone *)
            END ;
            INC(SourceSize)
         END
      END
   END
END InsertToken ;


(*
   InsertTokenAndRewind - inserts a symbol, Name, infront of the current token
                          and then moves the token stream back onto the inserted token.
*)

PROCEDURE InsertTokenAndRewind (Name: CARDINAL; Type: TypeOfToken; token: toktype) ;
BEGIN
   IF IsPass1()
   THEN
      InsertToken(Name, Type, token) ;
      PutToken(CurrentToken, TokenType, currenttoken) ;
      PutToken(Name, Type, token) ;
      GetToken
   END
END InsertTokenAndRewind ;


(*
   PutToken - pushes a symbol, Name, back onto the input.
              GetToken will set CurrentToken to, Name.
*)

PROCEDURE PutToken (Name: CARDINAL; Type: TypeOfToken; token: toktype) ;
BEGIN
   IF StackPtr=MaxStack
   THEN
      WriteError('maximum push back symbol exceeded - increase CONST MaxStack')
   ELSE
      WITH Stack[StackPtr] DO
         Token.TokName := Name ;
         Token.TokType := Type ;
         Token.token   := token ;
         CharOffset    := TokenIndex
      END ;
      INC(StackPtr)
   END
END PutToken ;


(*
   PutTokenLineOffset - pushes a symbol, Name, back onto the input.
                        GetToken will set CurrentToken to, Name.
                        The token current line number is set to Line and
                        character offset, Chr.
*)

PROCEDURE PutTokenLineOffset (Name: CARDINAL; Type: TypeOfToken;
                              Line: CARDINAL; Chr: CARDINAL) ;
BEGIN
   IF StackPtr=MaxStack
   THEN
      WriteError('Maximum push back symbol exceeded - Increase CONST MaxStack')
   ELSE
      WITH Stack[StackPtr] DO
         Token.TokName := Name ;
         Token.TokType := Type ;
         LineNumber := Line ;
         CharOffset := Chr
      END ;
      INC(StackPtr)
   END
END PutTokenLineOffset ;


(*
   GetTokenOffset - returns the character offset of the current token.
*)

PROCEDURE GetTokenOffset () : CARDINAL ;
BEGIN
   RETURN( SourceIndex )
END GetTokenOffset ;


(*
   CodeQuads - sets a switch in the quadruples depending on the
               flags = + -.
*)

PROCEDURE CodeQuads ;
BEGIN
   IF CurrentChar='='
   THEN
      SetOptionCoding(Coding) ;
      AdvanceChar
   ELSIF CurrentChar='+'
   THEN
      SetOptionCoding(TRUE) ;
      AdvanceChar
   ELSIF CurrentChar='-'
   THEN
      SetOptionCoding(FALSE) ;
      AdvanceChar
   END
END CodeQuads ;


(*
   ProfileCode - sets a switch in the quadruples depending on the
                 flags = + -.
*)

PROCEDURE ProfileCode ;
BEGIN
   IF CurrentChar='='
   THEN
      SetOptionProfiling(Profiling) ;
      AdvanceChar
   ELSIF CurrentChar='+'
   THEN
      SetOptionProfiling(TRUE) ;
      AdvanceChar
   ELSIF CurrentChar='-'
   THEN
      SetOptionProfiling(FALSE) ;
      AdvanceChar
   END
END ProfileCode ;


(*
   OptimizeCode - sets a switch in the quadruples depending on the
                  flags = + -.
*)

PROCEDURE OptimizeCode ;
BEGIN
   IF CurrentChar='='
   THEN
      SetOptionOptimizing(Optimizing) ;
      AdvanceChar
   ELSIF CurrentChar='+'
   THEN
      SetOptionOptimizing(TRUE) ;
      AdvanceChar
   ELSIF CurrentChar='-'
   THEN
      SetOptionOptimizing(FALSE) ;
      AdvanceChar
   END
END OptimizeCode ;


(*
   ProcessDate - inserts ' GetTimeString ' into the input stream.
*)

PROCEDURE ProcessDate ;
VAR
   a: ARRAY [0..MaxString] OF CHAR ;
   k: CARDINAL ;
BEGIN
   PutToken(SingleQuoteTok, ReservedToken, singlequotetok) ;
   AddTokenToSource(SingleQuoteTok, ReservedToken, CurSymLine, TokenNumber, singlequotetok) ;
   GetTimeString(a) ;
   k := MakeKey(a) ;
   PutToken(k, IdentToken, stringtok) ;
   INC(TokenNumber) ;
   AddTokenToSource(k, IdentToken, CurSymLine, TokenNumber, stringtok) ;
   INC(TokenNumber) ;
   CurrentToken := SingleQuoteTok ;
   currenttoken := singlequotetok ;
   TokenType    := ReservedToken
END ProcessDate ;


(*
   ProcessFile - inserts ' FileName ' into the input stream.
*)

PROCEDURE ProcessFile ;
VAR
   k: CARDINAL ;
BEGIN
   PutToken(SingleQuoteTok, ReservedToken, singlequotetok) ;
   AddTokenToSource(SingleQuoteTok, ReservedToken, CurSymLine, TokenNumber, singlequotetok) ;
   k := MakeKey(FileName) ;
   PutToken(k, IdentToken, stringtok) ;
   INC(TokenNumber) ;
   AddTokenToSource(k, IdentToken, CurSymLine, TokenNumber, stringtok) ;
   INC(TokenNumber) ;
   CurrentToken := SingleQuoteTok ;
   currenttoken := singlequotetok ;
   TokenType    := ReservedToken
END ProcessFile ;


(*
   ProcessLine - inserts CurSymLine into the input stream.
*)

PROCEDURE ProcessLine ;
VAR
   k: CARDINAL ;
   a: ARRAY [0..MaxString] OF CHAR ;
BEGIN
   CardToStr(CurSymLine, 0, a) ;
   CurrentToken := MakeKey(a) ;
   TokenType    := IntegerToken ;
   currenttoken := integertok
END ProcessLine ;


(*
   ToWhite - returns the character, ch, if it is not EOL. If it is a EOL
             then it returns cr.
*)

PROCEDURE ToWhite (ch: CHAR) : CHAR ;
BEGIN
   IF ch=EOL
   THEN
      RETURN( cr )
   ELSE
      RETURN( ch )
   END
END ToWhite ;


(*
   ParseOption - parses compiler options.
*)

PROCEDURE ParseOption ;
BEGIN
   AdvanceChar ;
   IF CurrentChar='P'
   THEN
      AdvanceChar ;
      ProfileCode
   ELSIF CurrentChar='O'
   THEN
      AdvanceChar ;
      OptimizeCode
   ELSIF CurrentChar='C'
   THEN
      AdvanceChar ;
      CodeQuads
   END
END ParseOption ;


(*
   ParseComment - Skips comments and nested comments.
                  Inside comments compiler directives may be set.
                  These options are:

                  $ : C        Code Switch
                  $ : P        Profile Switch
                  $ : O        Optimization Switch
                  $ : I        Insert Assembly Language Switch

                  without spaces..
*)

PROCEDURE ParseComment ;
VAR
   Level          : CARDINAL ;  (* Comment level *)
   LastSourceToken: CARDINAL ;  (* The token before the comment start *)
   LastSourceLine : CARDINAL ;
BEGIN
   LastSourceLine := CurSymLine ;
   LastSourceToken := TokenNumber ;
   Level := 0 ;
   REPEAT
      IF (CurrentChar='(') AND (NextChar='*')
      THEN
         INC(Level)
      ELSIF (CurrentChar='*') AND (NextChar=')')
      THEN
         DEC(Level)
      ELSIF (CurrentChar='$') AND (NextChar=':') AND (Level=1)
      THEN
         (* AdvanceChar ; ParseOption no this will not work on a multi pass compiler - fix me *)
      END ;
      AdvanceChar ;
   UNTIL (Level=0) OR Eof ;
   IF (Level#0) AND Eof
   THEN
      BeginError ;
      Title('end of file reached in a comment', FileName, '', CurSymLine, 0) ;
      EndError ;
      FlushError ;
      HALT
   END
END ParseComment ;


(*
   ConsumeSpaces - gets rid of all excess spaces before the next token.
*)

PROCEDURE ConsumeSpaces ;
VAR
   ok: BOOLEAN ;
BEGIN
   REPEAT
      IF CurrentChar=' '
      THEN
         WHILE (CurrentChar=' ') AND (NOT Eof) DO
            AdvanceChar
         END ;
         ok := FALSE
      ELSIF CurrentChar=EOL
      THEN
         AdvanceChar ;
         ok := FALSE
      ELSIF (CurrentChar='(') AND (NextChar='*')
      THEN
         ParseComment ;
         AdvanceChar ;
         ok := FALSE
      ELSE
        ok := TRUE
      END
   UNTIL ok ;
   (*
      check to see we are not looking at a comment end, which
      might cause a misleading error.
   *)
   IF (CurrentChar='*') AND (NextChar=')')
   THEN
      WriteError('seen end of comment *) sequence outside a comment')
   END
END ConsumeSpaces ;


(*
   BuildString - collects characters up to but not including the
                 start quote character and places these characters in, a.
*)

PROCEDURE BuildString (VAR a: ARRAY OF CHAR; VAR i: CARDINAL) ;
VAR
   High: CARDINAL ;
BEGIN
   High := HIGH(a) ;
   (* Fill in string or character *)
   REPEAT
      a[i] := CurrentChar ;
      INC(i) ;
      AdvanceChar
   UNTIL (i=High) OR (CurrentChar=QuoteChar) OR
         ((CurrentChar=EOL) AND (NOT MultiLineQuote)) OR Eof ;
   IF CurrentChar=EOL
   THEN
      WriteError('missing quote expected: strings cannot span more than one line')
   END ;
   IF Eof
   THEN
      WriteError('missing quote expected: reached end of file')
   END
END BuildString ;


(*
   HandleQuotes - builds a string, a, from the quote character or string
                  depending whether the string has been consumed yet.
                  Firstly generate string, secondly generate quote string.
                  This procedure is called if InQuotes is true.
                  If InQuotes is true and the CurrentChar=QuoteChar
                  then a string was the last token to be built
                  and therefore the next token will be a quote.
                  If, however, CurrentChar#QuoteChar then we need to build
                  a string since InQuotes is TRUE.
*)

PROCEDURE HandleQuotes (VAR a: ARRAY OF CHAR; VAR i: CARDINAL) ;
BEGIN
   (* Have we consumed the string yet? *)
   IF CurrentChar=QuoteChar
   THEN
      (* Yes, therefore build a token from the final quote *)
      InQuotes := FALSE ;
      a[i] := QuoteChar ;
      INC(i) ;
      AdvanceChar
   ELSE
      (*
         ok build string and then the next time this procedure is called
         build a quote.
      *)
      BuildString(a, i)
      (* If all is well the CurrentChar=QuoteChar *)
   END
END HandleQuotes ;


(*
   ReadToken - checks the CurrentChar, NextChar and generates the next token.
*)

PROCEDURE ReadToken (VAR Name   : CARDINAL;
                     VAR Type   : TypeOfToken;
                     VAR Index  : CARDINAL;
                     VAR TokLine: CARDINAL;
                     VAR token  : toktype) ;
VAR
   i          : CARDINAL ;
   FoundString,
   ok         : BOOLEAN ;
   a          : ARRAY [0..MaxTokenSize] OF CHAR ;
BEGIN
   FoundString := FALSE ;
   i := 0 ;
   IF NOT Eof
   THEN
      IF InQuotes
      THEN
         TokLine := NextSymLine ;
         Index := OutBufferIndex-2 ;
         HandleQuotes(a, i) ;
         IF InQuotes
         THEN
            Type        := IdentToken ;
            token       := stringtok ;
            FoundString := TRUE
         END
      ELSE
         (* was TokLine := NextSymLine ; *)
         ConsumeSpaces ;
         TokLine := NextSymLine ;
         Index := OutBufferIndex-2 ;
         i := 0 ;
         IF (CurrentChar='"') OR (CurrentChar="'")
         THEN
            InQuotes := TRUE ;
            QuoteChar := CurrentChar ;
            a[i] := CurrentChar ;
            AdvanceChar ;
            INC(i)
         ELSIF DoubleDelimiter()
         THEN
            a[i] := CurrentChar ;
            AdvanceChar ;
            INC(i) ;
            a[i] := CurrentChar ;
            AdvanceChar ;
            INC(i)
         ELSIF Delimiter()
         THEN
            a[i] := CurrentChar ;
            AdvanceChar ;
            INC(i)
         ELSIF Digit(CurrentChar)
         THEN
            ParseNumber(a, i, Type, token)
         ELSE
            ParseIdent(a, i) ;
            Type  := IdentToken ;
            token := identtok
         END
      END
   ELSE
      TokLine := NextSymLine ;
      (* eof *)
      i := 0 ;
      a[i] := eof ;
      INC(i) ;
      token := eoftok
   END ;
   IF i<=HIGH(a)
   THEN
      a[i] := nul
   END ;
   Name := MakeKey(a) ;
   IF NOT FoundString
   THEN
      IF IsReserved(Name, token)
      THEN
         Type := ReservedToken
      ELSIF StudentChecking
      THEN
         CheckForVariableThatLooksLikeKeyword(a, Name)
      END
   END
END ReadToken ;


(*
   ParseIdent - reads an identifier into string, a, beginning at index, i.
*)

PROCEDURE ParseIdent (VAR a: ARRAY OF CHAR; VAR i: CARDINAL) ;
BEGIN
   REPEAT
      a[i] := CurrentChar ;
      AdvanceChar ;
      INC(i)
   UNTIL Delimiter() OR (i>HIGH(a)) OR (CurrentChar=' ') OR
         (CurrentChar=EOL) OR Eof
END ParseIdent ;


(*
   ParseNumber - returns with a legal number in string, a, index, i, is
                 set together with the Type of the number.
*)

PROCEDURE ParseNumber (VAR a: ARRAY OF CHAR; VAR i: CARDINAL;
                       VAR Type: TypeOfToken; VAR token: toktype) ;
BEGIN
   REPEAT
      a[i] := CurrentChar ;
      AdvanceChar ;
      INC(i)
   UNTIL NOT Digit(CurrentChar) ;
   IF (CurrentChar='.') AND (NextChar#'.')
   THEN
      a[i] := CurrentChar ;
      AdvanceChar ;
      INC(i) ;
      WHILE Digit(CurrentChar) DO
         a[i] := CurrentChar ;
         AdvanceChar ;
         INC(i)
      END ;
      IF CurrentChar='E'
      THEN
         a[i] := CurrentChar ;
         AdvanceChar ;
         INC(i) ;
         IF (CurrentChar='+') OR (CurrentChar='-')
         THEN
            a[i] := CurrentChar ;
            AdvanceChar ;
            INC(i)
         END ;
         WHILE Digit(CurrentChar) DO
            a[i] := CurrentChar ;
            AdvanceChar ;
            INC(i)
         END
      END ;
      Type  := RealToken ;
      token := realtok
   ELSE
      IF (CurrentChar='H') OR
         (HexDigit(CurrentChar) AND (HexDigit(NextChar) OR (NextChar='H')))
         (*
            The problem is that B and C are both legal hex digits!
            B and C also denote binary and octal character constants.
         *)
      THEN
         WHILE HexDigit(CurrentChar) DO
            a[i] := CurrentChar ;
            AdvanceChar ;
            INC(i)
         END ;
         IF CurrentChar='H'
         THEN
            a[i] := CurrentChar ;
            AdvanceChar ;
            INC(i)
         ELSE
            WriteError('Hex number missing H')
         END
      ELSIF CurrentChar='A'
      THEN
         a[i] := CurrentChar ;
         AdvanceChar ;
         INC(i)
      ELSIF CurrentChar='B'
      THEN
         a[i] := CurrentChar ;
         AdvanceChar ;
         INC(i)
      ELSIF CurrentChar='C'
      THEN
         a[i] := CurrentChar ;
         AdvanceChar ;
         INC(i)
      END ;
      Type  := IntegerToken ;
      token := integertok
   END
END ParseNumber ;


(* Delimiter returns true if and only if CurrentChar is a delimiter *)

PROCEDURE Delimiter() : BOOLEAN ;
BEGIN
   RETURN(
          (CurrentChar='-') OR
          (CurrentChar='+') OR (CurrentChar='*') OR (CurrentChar='/') OR
          (CurrentChar='|') OR (CurrentChar='(') OR (CurrentChar=')') OR
          (CurrentChar='"') OR (CurrentChar="'") OR (CurrentChar='{') OR
          (CurrentChar='}') OR (CurrentChar='[') OR (CurrentChar=']') OR
          (CurrentChar='#') OR (CurrentChar='=') OR (CurrentChar='<') OR
          (CurrentChar='>') OR (CurrentChar='.') OR (CurrentChar=';') OR
          (CurrentChar=':') OR (CurrentChar='^') OR (CurrentChar=',')
         )
END Delimiter ;


PROCEDURE DoubleDelimiter () : BOOLEAN ;
BEGIN
   RETURN (
           ((CurrentChar='>') AND (NextChar='=')) OR
           ((CurrentChar='<') AND (NextChar='=')) OR
           ((CurrentChar='<') AND (NextChar='>')) OR
           ((CurrentChar=':') AND (NextChar='=')) OR
           ((CurrentChar='.') AND (NextChar='.'))
          )
END DoubleDelimiter ;


PROCEDURE Digit (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch>='0') AND (ch<='9') )
END Digit ;


PROCEDURE HexDigit (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( Digit(ch) OR ((ch>='A') AND (ch<='F')) )
END HexDigit ;


(*
   SkipToEndOfLine - reads characters until an eol or eof is reached.
*)

PROCEDURE SkipToEndOfLine () : CHAR ;
VAR
   ch: CHAR ;
BEGIN
   ch := ReadChar(f) ;
   WHILE (ch#eof) AND (ch#EOL) DO
      ch := ReadChar(f)
   END ;
   RETURN( ch )
END SkipToEndOfLine ;


(*
   SkipSpaces - skips tabs and spaces but not eol or eof.
*)

PROCEDURE SkipSpaces () : CHAR ;
VAR
   ch: CHAR ;
BEGIN
   ch := ReadChar(f) ;
   WHILE (ch=' ') OR (ch=tab) DO
      ch := ReadChar(f)
   END ;
   RETURN( ch )
END SkipSpaces ;


(*
   ParseCardinal - returns the next number into a CARDINAL.
*)

PROCEDURE ParseCardinal () : CARDINAL ;
VAR
   ch: CHAR ;
   c : CARDINAL ;
BEGIN
   ch := SkipSpaces() ;
   c := 0 ;
   IF Digit(ch)
   THEN
      WHILE Digit(ch) DO
         c := (c*10) + ORD(ch)-ORD('0') ;
         ch := ReadChar(f)
      END ;
      RETURN( c )
   ELSE
      WriteFormat1('encountered non preprocessed source code statement, suggest that you preprocess by hand', NulName) ;
      RETURN( CurSymLine )
   END
END ParseCardinal ;


(*
   ParseFileName - returns the filename inside the " "
*)

PROCEDURE ParseFileName () : CARDINAL ;
VAR
   i       : CARDINAL ;
   ch      : CHAR ;
BEGIN
   ch := SkipSpaces() ;
   IF ch='"'
   THEN
      i := 0 ;
      REPEAT
         ch := ReadChar(f) ;
         FileName[i] := ch ;
         INC(i) ;
      UNTIL (i=MaxFileName) OR (ch='"') OR (ch=EOL) OR (ch=eof) ;
      IF (ch='"') AND (i>0)
      THEN
         DEC(i)
      END ;
      FileName[i] := nul ;
      RETURN( MakeKey(FileName) )
   ELSE
      RETURN( NulName )
   END
END ParseFileName ;


(*
   ParseLineDirective - handle the preprocessor line directive # line "file" directive
*)

PROCEDURE ParseLineDirective ;
VAR
   modname,
   filename: CARDINAL ;
   tokno   : CARDINAL ;
   ch      : CHAR ;
BEGIN
   CurSymLine  := ParseCardinal() ;
   NextSymLine := CurSymLine ;
   filename    := ParseFileName() ;
   AddModuleFileToken(ModuleName, filename, CurSymLine, TokenNumber) ;
   ch := SkipToEndOfLine()
END ParseLineDirective ;


(*
   AdvanceChar - advances the CurrentChar to the NextChar and reads a
                 character from the source file into NextChar.
                 AdvanceChar actually buffers the characters, to allow
                 for a reasonable snapshot of text if an error should
                 arise.
                 It reads a line at a time into the Buffer, thus allowing
                 the WriteError routine to display the complete line
                 with the erroneous symbol underlined.
*)

PROCEDURE AdvanceChar ;
VAR
   c: CARDINAL ;
BEGIN
   IF NOT Eof
   THEN
      CurrentChar := NextChar ;
      IF NOT GetBuffer(NextChar)
      THEN
         c := 0 ;
         REPEAT
            NextChar := ReadChar(f) ;
            (* Write(NextChar) ; WriteOct(ORD(NextChar), 4) ; WriteLn ; *)
            IF NOT IsNoError(f)
            THEN
               NextChar := eof ;
               PutBuffer(eof) ;
               INC(c) ;
            ELSIF NextChar=tab
            THEN
               PutBuffer(' ') ;
               PutBuffer(' ') ;
               PutBuffer(' ') ;
               PutBuffer(' ') ;
               PutBuffer(' ') ;
               PutBuffer(' ') ;
               PutBuffer(' ') ;
               NextChar := ' ' ;
               PutBuffer(' ') ;
               INC(c) ;
            ELSIF (c=0) AND (NextChar='#') AND LineDirectives AND (NOT InError)
            THEN
               ParseLineDirective
            ELSE
               PutBuffer(NextChar) ;
               INC(c)
            END
         UNTIL (NextChar=EOL) OR (NextChar=eof) ;
         IF NOT GetBuffer(NextChar)
         THEN
            InternalError('AdvanceChar has failed', __FILE__, __LINE__)
         END
      END ;
      IF CurrentChar=eof
      THEN
         Eof := TRUE
      END
   END
END AdvanceChar ;


(*
   The following routines manipulate the BufferSource which contains
   a snapshot of the source file being parsed.
*)



(*
   PutBuffer - places the character, ch, into the circular buffer.
*)

PROCEDURE PutBuffer (ch: CHAR) ;
BEGIN
   (* IF ch=EOL THEN WriteLn ELSE Write(ch) END ; *)
   BufferSource[BufferIndex] := ch ;
   BufferIndex := (BufferIndex+1) MOD MaxBuffer ;
   INC(SourceIndex) ;
   IF BufferSize=MaxBuffer
   THEN
      INC(BufferOffset)
   ELSE
      INC(BufferSize)
   END
END PutBuffer ;


(*
   GetBuffer - returns true if a character is found in the BufferSource,
               otherwise false is returned.
               It also updates the NextSymLine if a <EOL> is found.
*)

PROCEDURE GetBuffer (VAR ch: CHAR) : BOOLEAN ;
BEGIN
   IF OutBufferIndex<SourceIndex
   THEN
      ch := BufferSource[OutBufferIndex MOD MaxBuffer] ;
      INC(OutBufferIndex) ;
      IF ch=EOL
      THEN
         INC(NextSymLine)
      END ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END GetBuffer ;


(*
   WriteBuffer - writes the buffer with the CurrentSymbol underlined.
*)

PROCEDURE WriteBuffer ;
VAR
   OffsetIntoSource,
   OffsetIntoBuffer,
   Indent          : CARDINAL ;
BEGIN
   OffsetIntoBuffer := StartOfIndexInBuffer() ;
   OffsetIntoSource := BufferOffset ;
   FindNextLineInBuffer(OffsetIntoBuffer, OffsetIntoSource) ;

   (* Found end of first line, now display BufferSource *)

   Indent := DisplayUpToToken(OffsetIntoBuffer, OffsetIntoSource) ;
   WriteNChars(' ', Indent) ;
   WriteNChars('^', TokenLength) ;
   WriteLn ;
   DisplayRestOfBuffer(OffsetIntoBuffer, OffsetIntoSource) ;
   WriteLn
END WriteBuffer ;


(*
   FindNextLineInBuffer - sets the index, OffsetIntoBuffer,
                          into BufferSource which
                          indicates the start of the next available
                          textual line inside BufferSource.
                          OffsetIntoSource is set which indicates the
                          total number of characters into the source file
                          OffsetIntoBuffer represents.
*)

PROCEDURE FindNextLineInBuffer (VAR OffsetIntoBuffer: CARDINAL;
                                VAR OffsetIntoSource: CARDINAL) ;
BEGIN
   WHILE (OffsetIntoSource<=SourceIndex) AND
         (BufferSource[OffsetIntoBuffer]#EOL)
   DO
      OffsetIntoBuffer := (OffsetIntoBuffer+1) MOD MaxBuffer ;
      INC(OffsetIntoSource)
   END
END FindNextLineInBuffer ;


(*
   WriteFileName - writes the filename in GNU emacs next error
                   style.
*)

PROCEDURE WriteFileName ;
BEGIN
   WriteString(FileName) ; Write(':')
END WriteFileName ;


(*
   DisplayUpToToken - displays the BufferSource up to and including the line
                      of CurrentToken.
                      The Token indent offset is returned.
*)

PROCEDURE DisplayUpToToken (VAR OffsetIntoBuffer: CARDINAL ;
                            VAR OffsetIntoSource: CARDINAL) : CARDINAL ;
VAR
   Indent,
   EndOfToken: CARDINAL ;
BEGIN
   EndOfToken := TokenIndex + TokenLength-1 ;
   IF SourceIndex<EndOfToken
   THEN
      WriteString('unable to give precise error line info - please forward bug to gaius') ; WriteLn
   END ;
   IF OffsetIntoSource+1<SourceIndex
   THEN
      (* more characters to be printed *)
      WriteFileName
   END ;
   Indent := 0 ;
   REPEAT
      WHILE (BufferSource[OffsetIntoBuffer]#EOL) AND
            (OffsetIntoSource<SourceIndex)
      DO
         Write(BufferSource[OffsetIntoBuffer]) ;
         IF OffsetIntoSource<TokenIndex
         THEN
            INC(Indent)
         END ;
         OffsetIntoBuffer := (OffsetIntoBuffer+1) MOD MaxBuffer ;
         INC(OffsetIntoSource)
      END ;
      IF BufferSource[OffsetIntoBuffer]=EOL
      THEN
         WriteLn ;
         IF (OffsetIntoSource<TokenIndex)
         THEN
            Indent := 0 ;
            IF OffsetIntoSource+1<SourceIndex
            THEN
               (* more characters to be printed *)
               WriteFileName
            END
         END ;
         OffsetIntoBuffer := (OffsetIntoBuffer+1) MOD MaxBuffer ;
         INC(OffsetIntoSource)
      ELSE
         WriteLn ;
         (* hit a wierd condition return Indent now *)
         RETURN( Indent+1+StrLen(FileName) )
      END
   UNTIL OffsetIntoSource>=EndOfToken ;
   RETURN( Indent+1+StrLen(FileName) )
END DisplayUpToToken ;


(*
   DisplayRestOfBuffer - display the rest of the buffer, from, OffsetIntoBuffer.
*)

PROCEDURE DisplayRestOfBuffer (OffsetIntoBuffer: CARDINAL;
                               OffsetIntoSource: CARDINAL) ;
BEGIN
   IF OffsetIntoSource<SourceIndex
   THEN
      (* more characters to be printed *)
      WriteFileName
   END ;
   WHILE OffsetIntoSource<SourceIndex DO
      IF BufferSource[OffsetIntoBuffer]=EOL
      THEN
         WriteLn ;
         IF OffsetIntoSource<SourceIndex
         THEN
            (* more characters to be printed *)
            WriteFileName
         END
      ELSE
         Write(BufferSource[OffsetIntoBuffer])
      END ;
      OffsetIntoBuffer := (OffsetIntoBuffer+1) MOD MaxBuffer ;
      INC(OffsetIntoSource)
   END
END DisplayRestOfBuffer ;


(*
   DisplayBuffer - display the complete buffer of source text.
*)

PROCEDURE DisplayBuffer ;
VAR
   OffsetIntoBuffer,
   OffsetIntoSource: CARDINAL ;
BEGIN
   OffsetIntoBuffer := StartOfIndexInBuffer() ;
   OffsetIntoSource := BufferOffset ;
   DisplayRestOfBuffer(OffsetIntoBuffer, OffsetIntoSource)
END DisplayBuffer ;


(*
   StartOfIndexInBuffer - returns the start of the textual data in SourceBuffer.
*)

PROCEDURE StartOfIndexInBuffer () : CARDINAL ;
BEGIN
   RETURN( (BufferIndex+MaxBuffer-BufferSize) MOD MaxBuffer)
END StartOfIndexInBuffer ;


PROCEDURE DisplayBufferVars ;
VAR
   ch: CHAR ;
BEGIN
   WriteString('SourceIndex :') ; WriteCard(SourceIndex, 4) ; WriteLn ;
   WriteString('BufferSize  :') ; WriteCard(BufferSize, 4) ; WriteLn ;
   WriteString('CurrentToken:"') ; WriteKey(CurrentToken) ; WriteString('"') ; WriteLn ;
   WriteString('TokenLength :') ; WriteCard(TokenLength, 4) ; WriteLn ;
   IF TokenLength#LengthKey(CurrentToken) THEN WriteString('ERROR') ; WriteLn ; HALT END ;
(*
   Read(ch) ;
   IF ch='d' THEN HALT END ;
*)
END DisplayBufferVars ;


(*
   WriteNChars - writes, n, characters, ch, to the default output.
*)

PROCEDURE WriteNChars (ch: CHAR; n: CARDINAL) ;
BEGIN
   WHILE n>0 DO
      Write(ch) ;
      DEC(n)
   END
END WriteNChars ;


(*
   IsInRange - returns true if i is in the range of r1..r2
*)

PROCEDURE IsInRange (i, r1, r2: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (i>=r1) AND (i<=r2) )
END IsInRange ;


BEGIN
   NoOfFiles := 0 ;
   Init
END M2Lexical.
