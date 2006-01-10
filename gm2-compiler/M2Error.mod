(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)
IMPLEMENTATION MODULE M2Error ;

FROM ASCII IMPORT nul, nl ;
FROM NameKey IMPORT Name, KeyToCharStar ;
FROM DynamicStrings IMPORT String, InitString, InitStringCharStar, ConCat, ConCatChar, Mark, string, KillString, Dup ;
FROM FIO IMPORT StdOut, WriteNBytes, Close ;
FROM StrLib IMPORT StrLen, StrEqual ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3 ;
FROM M2LexBuf IMPORT FindFileNameFromToken, TokenToLineNo, GetTokenNo ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Printf IMPORT printf0, printf2 ;
FROM M2RTS IMPORT ExitOnHalt ;
FROM SYSTEM IMPORT BITSET, ADDRESS ;
IMPORT StdIO ;

CONST
   Debugging  =  TRUE ;
   DebugTrace = FALSE ;

TYPE
   Error = POINTER TO error ;
   error =            RECORD  (* to help p2c *)
                         parent,
                         child,
                         next  : Error ;
                         fatal : BOOLEAN ;
                         s     : String ;
                         token : CARDINAL ;  (* index of token causing the error *)
                      END ;

VAR
   head      : Error ;
   InInternal: BOOLEAN ;


(*
   Cast - casts a := b
*)

PROCEDURE Cast (VAR a: ARRAY OF BYTE; b: ARRAY OF BYTE) ;
VAR
   i: CARDINAL ;
BEGIN
   IF HIGH(a)=HIGH(b)
   THEN
      FOR i := 0 TO HIGH(a) DO
         a[i] := b[i]
      END
   END
END Cast ;


(*
   TranslateNameToString - takes a format specification string, a, and
                           if they consist of of %a then this is translated
                           into a String and %a is replaced by %s.
*)

PROCEDURE TranslateNameToCharStar (VAR a: ARRAY OF CHAR;
                                   n: CARDINAL) : BOOLEAN ;
VAR
   argno,
   i, h : CARDINAL ;
BEGIN
   argno := 1 ;
   i := 0 ;
   h := StrLen(a) ;
   WHILE i<h DO
      IF (a[i]='%') AND (i+1<h)
      THEN
         IF (a[i+1]='a') AND (argno=n)
         THEN
            a[i+1] := 's' ;
            RETURN( TRUE )
         END ;
         INC(argno) ;
         IF argno>n
         THEN
            (* all done *)
            RETURN( FALSE )
         END
      END ;
      INC(i)
   END ;
   RETURN( FALSE )
END TranslateNameToCharStar ;


(*
   OutString - writes the contents of String to stdout.
               The string, s, is destroyed.
*)

PROCEDURE OutString (file: String; line: CARDINAL; s: String) ;
VAR
   leader : String ;
   p, q   : POINTER TO CHAR ;
   newline: BOOLEAN ;
BEGIN
   leader := Sprintf2(Mark(InitString('%s:%d:')), file, line) ;
   p := string(s) ;
   newline := TRUE ;
   WHILE (p#NIL) AND (p^#nul) DO
      IF newline
      THEN
         q := string(leader) ;
         WHILE (q#NIL) AND (q^#nul) DO
            StdIO.Write(q^) ;
            INC(q)
         END
      END ;
      StdIO.Write(p^) ;
      newline := (p^=nl) ;
      INC(p)
   END ;
   IF NOT newline
   THEN
      StdIO.Write(nl)
   END ;
   IF NOT Debugging
   THEN
      s      := KillString(s) ;
      leader := KillString(leader)
   END
END OutString ;


(*
   InternalError - displays an internal error message together with the compiler source
                   file and line number.
                   This function is not buffered and is used when the compiler is about
                   to give up.
*)

PROCEDURE InternalError (a: ARRAY OF CHAR; file: ARRAY OF CHAR; line: CARDINAL) ;
BEGIN
   IF NOT InInternal
   THEN
      InInternal := TRUE ;
      FlushErrors ;
      OutString(FindFileNameFromToken(GetTokenNo(), 0),
                TokenToLineNo(GetTokenNo(), 0), Mark(InitString('*** fatal error ***')))
   END ;
   OutString(Mark(InitString(file)), line,
             ConCat(Mark(InitString('*** internal error *** ')), Mark(InitString(a)))) ;
   HALT
END InternalError ;


(* ***************************************************************************
   The following routines are used for normal syntax and semantic error reporting
   *************************************************************************** *)


(*
   WriteFormat0 - displays the source module and line together
                  with the encapsulated format string.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE WriteFormat0 (a: ARRAY OF CHAR) ;
VAR
   e: Error ;
BEGIN
   e := NewError(GetTokenNo()) ;
   WITH e^ DO
      s := Sprintf0(Mark(InitString(a)))
   END
END WriteFormat0 ;


(*
   DoFormat1 - 
*)

PROCEDURE DoFormat1 (a: ARRAY OF CHAR; w: ARRAY OF BYTE) : String ;
VAR
   s: String ;
   n: Name ;
BEGIN
   IF TranslateNameToCharStar(a, 1)
   THEN
      Cast(n, w) ;
      s := Mark(InitStringCharStar(KeyToCharStar(n))) ;
      s := Sprintf1(Mark(InitString(a)), s)
   ELSE
      s := Sprintf1(Mark(InitString(a)), w)
   END ;
   RETURN( s )
END DoFormat1 ;


(*
   WriteFormat1 - displays the source module and line together
                  with the encapsulated format string.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE WriteFormat1 (a: ARRAY OF CHAR; w: ARRAY OF BYTE) ;
VAR
   e: Error ;
BEGIN
   e := NewError(GetTokenNo()) ;
   e^.s := DoFormat1(a, w)
END WriteFormat1 ;


(*
   DoFormat2 - 
*)

PROCEDURE DoFormat2 (a: ARRAY OF CHAR; w1, w2: ARRAY OF BYTE) : String ;
VAR
   n     : Name ;
   s,
   s1, s2: String ;
   b     : BITSET ;
BEGIN
   b := {} ;
   IF TranslateNameToCharStar(a, 1)
   THEN
      Cast(n, w1) ;
      s1 := Mark(InitStringCharStar(KeyToCharStar(n))) ;
      INCL(b, 1)
   END ;
   IF TranslateNameToCharStar(a, 2)
   THEN
      Cast(n, w2) ;
      s2 := Mark(InitStringCharStar(KeyToCharStar(n))) ;
      INCL(b, 2)
   END ;
   CASE b OF

   {}   :  s := Sprintf2(Mark(InitString(a)), w1, w2) |
   {1}  :  s := Sprintf2(Mark(InitString(a)), s1, w2) |
   {2}  :  s := Sprintf2(Mark(InitString(a)), w1, s2) |
   {1,2}:  s := Sprintf2(Mark(InitString(a)), s1, s2)

   ELSE
      HALT
   END ;
   RETURN( s )
END DoFormat2 ;


(*
   WriteFormat2 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE WriteFormat2 (a: ARRAY OF CHAR; w1, w2: ARRAY OF BYTE) ;
VAR
   e: Error ;
BEGIN
   e := NewError(GetTokenNo()) ;
   e^.s := DoFormat2(a, w1, w2)
END WriteFormat2 ;


PROCEDURE DoFormat3 (a: ARRAY OF CHAR; w1, w2, w3: ARRAY OF BYTE) : String ;
VAR
   n            : Name ;
   s, s1, s2, s3: String ;
   b            : BITSET ;
BEGIN
   b := {} ;
   IF TranslateNameToCharStar(a, 1)
   THEN
      Cast(n, w1) ;
      s1 := Mark(InitStringCharStar(KeyToCharStar(n))) ;
      INCL(b, 1)
   END ;
   IF TranslateNameToCharStar(a, 2)
   THEN
      Cast(n, w2) ;
      s2 := Mark(InitStringCharStar(KeyToCharStar(n))) ;
      INCL(b, 2)
   END ;
   IF TranslateNameToCharStar(a, 3)
   THEN
      Cast(n, w3) ;
      s3 := Mark(InitStringCharStar(KeyToCharStar(n))) ;
      INCL(b, 3)
   END ;
   CASE b OF
   
   {}     :  s := Sprintf3(Mark(InitString(a)), w1, w2, w3) |
   {1}    :  s := Sprintf3(Mark(InitString(a)), s1, w2, w3) |
   {2}    :  s := Sprintf3(Mark(InitString(a)), w1, s2, w3) |
   {1,2}  :  s := Sprintf3(Mark(InitString(a)), s1, s2, w3) |
   {3}    :  s := Sprintf3(Mark(InitString(a)), w1, w2, s3) |
   {1,3}  :  s := Sprintf3(Mark(InitString(a)), s1, w2, s3) |
   {2,3}  :  s := Sprintf3(Mark(InitString(a)), w1, s2, s3) |
   {1,2,3}:  s := Sprintf3(Mark(InitString(a)), s1, s2, s3)

   ELSE
      HALT
   END ;
   RETURN( s )
END DoFormat3 ;


(*
   WriteFormat3 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE WriteFormat3 (a: ARRAY OF CHAR; w1, w2, w3: ARRAY OF BYTE) ;
VAR
   e: Error ;
BEGIN
   e := NewError(GetTokenNo()) ;
   e^.s := DoFormat3(a, w1, w2, w3)
END WriteFormat3 ;


(*
   NewError - creates and returns a new error handle.
*)

PROCEDURE NewError (AtTokenNo: CARDINAL) : Error ;
VAR
   e, f: Error ;
BEGIN
   NEW(e) ;
   WITH e^ DO
      s      := NIL ;
      token  := AtTokenNo ;
      next   := NIL ;
      parent := NIL ;
      child  := NIL ;
      fatal  := TRUE
   END ;
   IF (head=NIL) OR (head^.token>AtTokenNo)
   THEN
      e^.next := head ;
      head    := e
   ELSE
      f := head ;
      WHILE (f^.next#NIL) AND (f^.next^.token<AtTokenNo) DO
         f := f^.next
      END ;
      e^.next := f^.next ;
      f^.next := e
   END ;
   RETURN( e )
END NewError ;


(*
   NewWarning - creates and returns a new error handle suitable for a warning.
                A warning will not stop compilation.
*)

PROCEDURE NewWarning (AtTokenNo: CARDINAL) : Error ;
VAR
   e: Error ;
BEGIN
   e := NewError(AtTokenNo) ;
   e^.fatal := FALSE ;
   RETURN( e )
END NewWarning ;


(*
   ChainError - creates and returns a new error handle, this new error
                is associated with, e, and is chained onto the end of, e.
                If, e, is NIL then the result to NewError is returned.
*)

PROCEDURE ChainError (AtTokenNo: CARDINAL; e: Error) : Error ;
VAR
   f: Error ;
BEGIN
   IF e=NIL
   THEN
      RETURN( NewError(AtTokenNo) )
   ELSE
      NEW(f) ;
      WITH f^ DO
         s      := NIL ;
         token  := AtTokenNo ;
         next   := e^.child ;
         parent := e ;
         child  := NIL ;
         fatal  := e^.fatal
      END ;
      e^.child := f
   END ;
   RETURN( f )
END ChainError ;


(*
   ErrorFormat routines provide a printf capability for the error handle.
*)

PROCEDURE ErrorFormat0 (e: Error; a: ARRAY OF CHAR) ;
BEGIN
   WITH e^ DO
      IF s=NIL
      THEN
         s := Sprintf0(Mark(InitString(a)))
      ELSE
         s := ConCat(s, Mark(Sprintf0(Mark(InitString(a)))))
      END
   END
END ErrorFormat0 ;


PROCEDURE ErrorFormat1 (e: Error; a: ARRAY OF CHAR; w: ARRAY OF BYTE) ;
VAR
   s1: String ;
BEGIN
   s1 := DoFormat1(a, w) ;
   WITH e^ DO
      IF s=NIL
      THEN
         s := s1
      ELSE
         s := ConCat(s, Mark(s1))
      END
   END
END ErrorFormat1 ;


PROCEDURE ErrorFormat2 (e: Error; a: ARRAY OF CHAR; w1, w2: ARRAY OF BYTE) ;
VAR
   s1: String ;
BEGIN
   s1 := DoFormat2(a, w1, w2) ;
   WITH e^ DO
      IF s=NIL
      THEN
         s := s1
      ELSE
         s := ConCat(s, Mark(s1))
      END
   END
END ErrorFormat2 ;


PROCEDURE ErrorFormat3 (e: Error; a: ARRAY OF CHAR;
                        w1, w2, w3: ARRAY OF BYTE) ;
VAR
   s1: String ;
BEGIN
   s1 := DoFormat3(a, w1, w2, w3) ;
   WITH e^ DO
      IF s=NIL
      THEN
         s := s1
      ELSE
         s := ConCat(s, Mark(s1))
      END
   END
END ErrorFormat3 ;


PROCEDURE ErrorString (e: Error; str: String) ;
BEGIN
   WITH e^ DO
      s := str
   END
END ErrorString ;


(*
   Init - initializes the error list.
*)

PROCEDURE Init ;
BEGIN
   head := NIL ;
   InInternal := FALSE
END Init ;


(*
   CheckIncludes - generates a sequence of error messages which determine the relevant
                   included file and line number.
                   For example:

                   gcc a.c
                   In file included from b.h:1,
                                    from a.c:1:
                   c.h:1: parse error before `and'

                   where a.c is: #include "b.h"
                         b.h is: #include "c.h"
                         c.h is: and this and that

                   we attempt to follow the error messages that gcc issues.
*)

PROCEDURE CheckIncludes (token: CARDINAL; depth: CARDINAL) ;
VAR
   included: String ;
   lineno  : CARDINAL ;
BEGIN
   included := FindFileNameFromToken(token, depth+1) ;
   IF included#NIL
   THEN
      lineno := TokenToLineNo(token, depth+1) ;
      IF depth=0
      THEN
         printf2('In file included from %s:%d', included, lineno)
      ELSE
         printf2('                 from %s:%d', included, lineno)
      END ;
      IF FindFileNameFromToken(token, depth+2)=NIL
      THEN
         printf0(':\n')
      ELSE
         printf0(',\n')
      END ;
      CheckIncludes(token, depth+1)
   END
END CheckIncludes ;


(*
   FlushAll - flushes all errors in list, e.
*)

PROCEDURE FlushAll (e: Error; FatalStatus: BOOLEAN) : BOOLEAN ;
VAR
   f      : Error ;
   written: BOOLEAN ;
BEGIN
   written := FALSE ;
   IF e#NIL
   THEN
      REPEAT
         WITH e^ DO
            IF FatalStatus=fatal
            THEN
               CheckIncludes(token, 0) ;
               OutString(FindFileNameFromToken(token, 0), TokenToLineNo(token, 0), s) ;
               IF (child#NIL) AND FlushAll(child, FatalStatus)
               THEN
               END ;
               IF NOT Debugging
               THEN
                  s := NIL  (* s is destroyed by the OutString *)
               END ;
               written := TRUE
            END
         END ;
         f := e ;
         e := e^.next ;
         IF NOT Debugging
         THEN
            WITH f^ DO
               s := KillString(s)
            END ;
            DISPOSE(f)
         END ;
      UNTIL e=NIL
   END ;
   RETURN( written )
END FlushAll ;


(*
   FlushErrors - switches the output channel to the error channel
                 and then writes out all errors.
*)

PROCEDURE FlushErrors ;
BEGIN
   IF DebugTrace
   THEN
      printf0('\nFlushing all errors\n') ;
      printf0('===================\n')
   END ;
   IF FlushAll(head, TRUE)
   THEN
      ExitOnHalt(1) ;
      HALT
   END
END FlushErrors ;


(*
   FlushWarnings - switches the output channel to the error channel
                   and then writes out all warnings.
                   If an error is present the compilation is terminated,
                   if warnings only were emitted then compilation will
                   continue.
*)

PROCEDURE FlushWarnings ;
BEGIN
   IF FlushAll(head, FALSE)
   THEN
   END
END FlushWarnings ;


(*
   ErrorStringsAt2 - given error strings, s1, and, s2, it places these
                     strings at token positions, tok1 and tok2, respectively.
                     Both strings are consumed.
*)

PROCEDURE ErrorStringsAt2 (s1, s2: String; tok1, tok2: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   IF s1=s2
   THEN
      s2 := Dup(s1)
   END ;
   e := NewError(tok1) ;
   ErrorString(e, s1) ;
   ErrorString(ChainError(tok2, e), s2)
END ErrorStringsAt2 ;


(*
   ErrorStringAt2 - given an error string, s, it places this
                    string at token positions, tok1 and tok2, respectively.
                    The string is consumed.
*)

PROCEDURE ErrorStringAt2 (s: String; tok1, tok2: CARDINAL) ;
BEGIN
   ErrorStringsAt2(s, s, tok1, tok2)
END ErrorStringAt2 ;


(*
   ErrorStringAt - given an error string, s, it places this
                   string at token position, tok.
                   The string is consumed.
*)

PROCEDURE ErrorStringAt (s: String; tok: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   e := NewError(tok) ;
   ErrorString(e, s) ;
END ErrorStringAt ;


(*
   WarnStringsAt2 - given warning strings, s1, and, s2, it places these
                    strings at token positions, tok1 and tok2, respectively.
                    Both strings are consumed.
*)

PROCEDURE WarnStringsAt2 (s1, s2: String; tok1, tok2: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   IF s1=s2
   THEN
      s2 := Dup(s1)
   END ;
   e := NewWarning(tok1) ;
   ErrorString(e, s1) ;
   ErrorString(ChainError(tok2, e), s2)
END WarnStringsAt2 ;


(*
   WarnStringAt2 - given an warning string, s, it places this
                   string at token positions, tok1 and tok2, respectively.
                   The string is consumed.
*)

PROCEDURE WarnStringAt2 (s: String; tok1, tok2: CARDINAL) ;
BEGIN
   WarnStringsAt2(s, s, tok1, tok2)
END WarnStringAt2 ;


(*
   WarnStringAt - given an error string, s, it places this
                  string at token position, tok.
                  The string is consumed.
*)

PROCEDURE WarnStringAt (s: String; tok: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   e := NewWarning(tok) ;
   ErrorString(e, s) ;
END WarnStringAt ;


(*
   ErrorAbort0 - aborts compiling, it flushes all warnings and errors before aborting.
*)

PROCEDURE ErrorAbort0 (a: ARRAY OF CHAR) ;
BEGIN
   FlushWarnings ;
   IF NOT StrEqual(a, '')
   THEN
      WriteFormat0(a)
   END ;
   IF NOT FlushAll(head, TRUE)
   THEN
      WriteFormat0('unidentified error') ;
      IF FlushAll(head, TRUE)
      THEN
      END
   END ;
   ExitOnHalt(1) ;
   HALT
END ErrorAbort0 ;


BEGIN
   Init
END M2Error.
