(* M2Emit.mod issue errors to the gm2 tools substructure.

Copyright (C) 2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2Emit ;

FROM ASCII IMPORT nul, nl ;
FROM M2ColorString IMPORT filenameColor, endColor, errorColor, warningColor, noteColor,
                          range1Color, range2Color ;
FROM M2LexBuf IMPORT FindFileNameFromToken, TokenToLineNo, TokenToColumnNo, GetTokenNo ;
FROM DynamicStrings IMPORT String, InitString, InitStringCharStar, ConCat, ConCatChar, Mark, string, KillString, Dup ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3 ;
FROM FIO IMPORT StdOut, WriteNBytes, Close, FlushBuffer ;
FROM M2Printf IMPORT printf0, printf1, printf2 ;
FROM M2Options IMPORT Xcode ;

IMPORT StdIO ;

CONST
   Debugging  =  TRUE ;


(*
   EmitError - pass the error to GCC.
*)

PROCEDURE EmitError (error, note: BOOLEAN; token: CARDINAL; message: String) ;
BEGIN
   IF error
   THEN
      message := ConCat (errorColor (InitString (' error ')), endColor (message))
   ELSIF note
   THEN
      message := ConCat (noteColor (InitString (' note ')), endColor (message))
   ELSE
      message := ConCat (warningColor (InitString (' warning ')), endColor (message))
   END ;
   OutString (FindFileNameFromToken (token, 0),
              TokenToLineNo (token, 0), TokenToColumnNo (token, 0), message)
END EmitError ;


(*
   OutString - writes the contents of String to stdout.
               The string, s, is destroyed.
*)

PROCEDURE OutString (file: String; line, col: CARDINAL; s: String) ;
VAR
   leader : String ;
   p, q   : POINTER TO CHAR ;
   space,
   newline: BOOLEAN ;
BEGIN
   file := ConCat(filenameColor(InitString('')), file) ;
   file := endColor(file) ;
   INC(col) ;
   leader := ConCatChar(file, ':') ;
   leader := range1Color(leader) ;
   leader := ConCat(leader, Sprintf1(Mark(InitString('%d')), line)) ;
   leader := endColor(leader) ;
   leader := ConCatChar(leader, ':') ;
   IF NOT Xcode
   THEN
      leader := range2Color(leader) ;
      leader := ConCat(leader, Sprintf1(Mark(InitString('%d')), col)) ;
      leader := endColor(leader) ;
      leader := ConCatChar(leader, ':')
   END ;
   p := string(s) ;
   newline := TRUE ;
   space := FALSE ;
   WHILE (p#NIL) AND (p^#nul) DO
      IF newline
      THEN
         q := string(leader) ;
         WHILE (q#NIL) AND (q^#nul) DO
            StdIO.Write(q^) ;
            INC(q)
         END
      END ;
      newline := (p^=nl) ;
      space := (p^=' ') ;
      IF newline AND Xcode
      THEN
         printf1('(pos: %d)', col)
      END ;
      StdIO.Write(p^) ;
      INC(p)
   END ;
   IF NOT newline
   THEN
      IF Xcode
      THEN
         IF NOT space
         THEN
            StdIO.Write(' ')
         END ;
         printf1('(pos: %d)', col)
      END ;
      StdIO.Write(nl)
   END ;
   FlushBuffer(StdOut) ;
   IF NOT Debugging
   THEN
      s      := KillString(s) ;
      leader := KillString(leader)
   END
END OutString ;


(*
   InternalError -
*)

PROCEDURE InternalError (file: ARRAY OF CHAR; line: CARDINAL; message: ARRAY OF CHAR) ;
BEGIN
   OutString (Mark (InitString (file)), line, 0,
              ConCat (Mark (InitString ('*** internal error *** ')),
                      Mark(InitString (message))))
END InternalError ;


END M2Emit.
