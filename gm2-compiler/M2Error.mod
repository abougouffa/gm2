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
IMPLEMENTATION MODULE M2Error ;

FROM ASCII IMPORT nul, nl ;
FROM NameKey IMPORT KeyToCharStar ;
FROM Strings IMPORT String, InitString, InitStringCharStar, ConCat, ConCatChar, Mark, string, KillString ;
FROM FIO IMPORT StdOut, WriteNBytes, Close ;
FROM StrLib IMPORT StrLen ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3 ;
FROM M2LexBuf IMPORT FindFileNameFromToken, TokenToLineNo, GetTokenNo ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
IMPORT StdIO ;

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
   head: Error ;


(*
   TranslateNameToString - takes a format specification string, a, and
                           if they consist of of %a then this is translated
                           into a String and %a is replaced by %s.
*)

PROCEDURE TranslateNameToCharStar (VAR a: ARRAY OF CHAR; n: CARDINAL;
                                   VAR w1, w2, w3, w4: WORD) ;
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
         IF a[i+1]='a'
         THEN
            (* translate the NameKey into a String *)
            a[i+1] := 's' ;
            IF argno=1
            THEN
               w1 := Mark(InitStringCharStar(KeyToCharStar(w1)))
            ELSIF argno=2
            THEN
               w2 := Mark(InitStringCharStar(KeyToCharStar(w2)))
            ELSIF argno=3
            THEN
               w3 := Mark(InitStringCharStar(KeyToCharStar(w3)))
            ELSE
               w4 := Mark(InitStringCharStar(KeyToCharStar(w4)))
            END
         END ;
         INC(argno) ;
         IF argno>n
         THEN
            (* all done *)
            RETURN
         END
      END ;
      INC(i)
   END
END TranslateNameToCharStar ;


(*
   InternalError - displays an internal error message together with the compiler source
                   file and line number.
                   This function is not buffered and is used when the compiler is about
                   to give up.
*)

PROCEDURE InternalError (a: ARRAY OF CHAR; file: ARRAY OF CHAR; line: CARDINAL) ;
BEGIN
   OutString(Mark(InitString(file)), line,
             ConCat(Mark(InitString('*** internal error *** ')), Mark(InitString(a)))) ;
   HALT
END InternalError ;


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
   s      := KillString(s) ;
   leader := KillString(leader)
END OutString ;


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
   WriteFormat1 - displays the source module and line together
                  with the encapsulated format string.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE WriteFormat1 (a: ARRAY OF CHAR; w: WORD) ;
VAR
   e: Error ;
   n: WORD ;
BEGIN
   TranslateNameToCharStar(a, 1, w, n, n, n) ;
   e := NewError(GetTokenNo()) ;
   WITH e^ DO
      s := Sprintf1(Mark(InitString(a)), w)
   END
END WriteFormat1 ;


(*
   WriteFormat2 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE WriteFormat2 (a: ARRAY OF CHAR; w1: WORD; w2: WORD) ;
VAR
   e: Error ;
   n: WORD ;
BEGIN
   TranslateNameToCharStar(a, 2, w1, w2, n, n) ;
   e := NewError(GetTokenNo()) ;
   WITH e^ DO
      s := Sprintf2(Mark(InitString(a)), w1, w2)
   END
END WriteFormat2 ;


(*
   WriteFormat3 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE WriteFormat3 (a: ARRAY OF CHAR; w1: WORD; w2: WORD; w3: WORD) ;
VAR
   e: Error ;
   n: WORD ;
BEGIN
   TranslateNameToCharStar(a, 2, w1, w2, n, n) ;
   e := NewError(GetTokenNo()) ;
   WITH e^ DO
      s := Sprintf3(Mark(InitString(a)), w1, w2, w3)
   END
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
      head := e ;
      e^.next := head
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
*)

PROCEDURE ChainError (AtTokenNo: CARDINAL; e: Error) : Error ;
VAR
   f: Error ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      s      := NIL ;
      token  := AtTokenNo ;
      next   := e^.child ;
      parent := e ;
      child  := NIL ;
      fatal  := e^.fatal
   END ;
   e^.child := f ;
   RETURN( f )
END ChainError ;


(*
   ErrorFormat routines provide a printf capability for the error handle.
*)

PROCEDURE ErrorFormat0 (e: Error; a: ARRAY OF CHAR) ;
BEGIN
   WITH e^ DO
      s := ConCat(s, Mark(Sprintf0(Mark(InitString(a)))))
   END
END ErrorFormat0 ;


PROCEDURE ErrorFormat1 (e: Error; a: ARRAY OF CHAR; w: WORD) ;
VAR
   n: WORD ;
BEGIN
   TranslateNameToCharStar(a, 1, w, n, n, n) ;
   WITH e^ DO
      s := ConCat(s, Mark(Sprintf1(Mark(InitString(a)), w)))
   END
END ErrorFormat1 ;


PROCEDURE ErrorFormat2 (e: Error; a: ARRAY OF CHAR; w1, w2: WORD) ;
VAR
   n: WORD ;
BEGIN
   TranslateNameToCharStar(a, 2, w1, w2, n, n) ;
   WITH e^ DO
      s := ConCat(s, Mark(Sprintf2(Mark(InitString(a)), w1, w2)))
   END
END ErrorFormat2 ;


PROCEDURE ErrorFormat3 (e: Error; a: ARRAY OF CHAR; w1, w2, w3: WORD) ;
VAR
   n: WORD ;
BEGIN
   TranslateNameToCharStar(a, 3, w1, w2, w3, n) ;
   WITH e^ DO
      s := ConCat(s, Mark(Sprintf3(Mark(InitString(a)), w1, w2, w3)))
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
   head := NIL
END Init ;


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
               OutString(FindFileNameFromToken(token), TokenToLineNo(token), s) ;
               IF (child#NIL) AND FlushAll(child, FatalStatus)
               THEN
               END ;
               written := TRUE
            END
         END ;
         f := e ;
         e := e^.next ;
         WITH f^ DO
            s := KillString(s)
         END ;
         DISPOSE(f)
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
   IF FlushAll(head, TRUE)
   THEN
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
END FlushErrors ;


BEGIN
   Init
END M2Error.
