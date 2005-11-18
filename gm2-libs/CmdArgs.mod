(* Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA *)
IMPLEMENTATION MODULE CmdArgs ;


FROM ASCII IMPORT cr, nul ;

FROM StrLib IMPORT StrLen ;


CONST
   esc    = '\' ;
   space  = ' ' ;
   squote = "'" ;
   dquote = '"' ;
   tab    = ' ' ;

(* %%%FORWARD%%%
PROCEDURE GetArg (CmdLine: ARRAY OF CHAR ;
                  n: CARDINAL; VAR Argi: ARRAY OF CHAR) : BOOLEAN ; FORWARD ;
PROCEDURE GetNextArg (CmdLine: ARRAY OF CHAR; VAR CmdIndex: CARDINAL;
                      VAR Arg: ARRAY OF CHAR) : BOOLEAN ; FORWARD ;
PROCEDURE CopyUntilSpace (From: ARRAY OF CHAR;
                          VAR FromIndex: CARDINAL; FromHigh: CARDINAL;
                          VAR To: ARRAY OF CHAR;
                          VAR ToIndex: CARDINAL; ToHigh: CARDINAL) ; FORWARD ;
PROCEDURE CopyUntil (From: ARRAY OF CHAR;
                     VAR FromIndex: CARDINAL; FromHigh: CARDINAL;
                     VAR To: ARRAY OF CHAR;
                     VAR ToIndex: CARDINAL; ToHigh: CARDINAL;
                     UntilChar: CHAR) ; FORWARD ;
PROCEDURE CopyChar (From: ARRAY OF CHAR;
                    VAR FromIndex: CARDINAL; FromHigh: CARDINAL;
                    VAR To: ARRAY OF CHAR;
                    VAR ToIndex: CARDINAL; ToHigh: CARDINAL) ; FORWARD ;
PROCEDURE Narg (CmdLine: ARRAY OF CHAR) : CARDINAL ; FORWARD ;
PROCEDURE Escape (ch: CHAR) : BOOLEAN ; FORWARD ;
PROCEDURE Space (ch: CHAR) : BOOLEAN ; FORWARD ;
PROCEDURE DoubleQuote (ch: CHAR) : BOOLEAN ; FORWARD ;
PROCEDURE SingleQuote (ch: CHAR) : BOOLEAN ; FORWARD ;
   %%%FORWARD%%% *)


(*
   GetArg - takes a command line and attempts to extract argument, n,
            from CmdLine. The resulting argument is placed into, a.
            The result of the operation is returned.
*)

PROCEDURE GetArg (CmdLine: ARRAY OF CHAR ;
                  n: CARDINAL; VAR Argi: ARRAY OF CHAR) : BOOLEAN ;
VAR
   Index,
   i     : CARDINAL ;
   Another: BOOLEAN ;
BEGIN
   Index := 0 ;
   (* Continually retrieve an argument until we get the n th argument. *)
   i := 0 ;
   REPEAT
      Another := GetNextArg(CmdLine, Index, Argi) ;
      INC(i) ;
   UNTIL (i>n) OR (NOT Another) ;
   RETURN( i>n )
END GetArg ;


(*
   GetNextArg - Returns true if another argument may be found.
                The argument is taken from CmdLine at position Index,
                Arg is filled with the found argument.
*)

PROCEDURE GetNextArg (CmdLine: ARRAY OF CHAR; VAR CmdIndex: CARDINAL;
                      VAR Arg: ARRAY OF CHAR) : BOOLEAN ;
VAR
   ArgIndex: CARDINAL ;  (* Index into Arg *)
   HighA,
   HighC: CARDINAL ;
BEGIN
   HighA := HIGH(Arg) ;
   HighC := StrLen(CmdLine) ;
   ArgIndex := 0 ;
   (* Skip spaces *)
   WHILE (CmdIndex<HighC) AND Space(CmdLine[CmdIndex]) DO
      INC(CmdIndex)
   END ;
   IF CmdIndex<HighC
   THEN
      IF SingleQuote(CmdLine[CmdIndex])
      THEN
         (* Skip over the single quote *)
         INC(CmdIndex) ;
         CopyUntil(CmdLine, CmdIndex, HighC, Arg, ArgIndex, HighA, squote) ;
         INC(CmdIndex)
      ELSIF DoubleQuote(CmdLine[CmdIndex])
      THEN
         (* Skip over the double quote *)
         INC(CmdIndex) ;
         CopyUntil(CmdLine, CmdIndex, HighC, Arg, ArgIndex, HighA, dquote) ;
         INC(CmdIndex)
      ELSE
         CopyUntilSpace(CmdLine, CmdIndex, HighC, Arg, ArgIndex, HighA)
      END
   END ;
   (* Skip spaces *)
   WHILE (CmdIndex<HighC) AND Space(CmdLine[CmdIndex]) DO
      INC(CmdIndex)
   END ;
   IF ArgIndex<HighA
   THEN
      Arg[ArgIndex] := nul
   END ;
   RETURN( (CmdIndex<HighC) )
END GetNextArg ;


(*
   CopyUntilSpace - copies characters until a Space character is found.
*)

PROCEDURE CopyUntilSpace (From: ARRAY OF CHAR;
                          VAR FromIndex: CARDINAL; FromHigh: CARDINAL;
                          VAR To: ARRAY OF CHAR;
                          VAR ToIndex: CARDINAL; ToHigh: CARDINAL) ;
BEGIN
   WHILE (FromIndex<FromHigh) AND (ToIndex<ToHigh) AND
         (NOT Space(From[FromIndex])) DO
      CopyChar(From, FromIndex, FromHigh, To, ToIndex, ToHigh)
   END
END CopyUntilSpace ;


(*
   CopyUntil - copies characters until the UntilChar is found.
*)

PROCEDURE CopyUntil (From: ARRAY OF CHAR;
                     VAR FromIndex: CARDINAL; FromHigh: CARDINAL;
                     VAR To: ARRAY OF CHAR;
                     VAR ToIndex: CARDINAL; ToHigh: CARDINAL;
                     UntilChar: CHAR) ;
BEGIN
   WHILE (FromIndex<FromHigh) AND (ToIndex<ToHigh) AND
         (From[FromIndex]#UntilChar) DO
      CopyChar(From, FromIndex, FromHigh, To, ToIndex, ToHigh)
   END
END CopyUntil ;


(*
   CopyChar - copies a character from string From to string To and
              takes into consideration escape characters. ie \x
              Where x is any character.
*)

PROCEDURE CopyChar (From: ARRAY OF CHAR;
                    VAR FromIndex: CARDINAL; FromHigh: CARDINAL;
                    VAR To: ARRAY OF CHAR;
                    VAR ToIndex: CARDINAL; ToHigh: CARDINAL) ;
BEGIN
   IF (FromIndex<FromHigh) AND (ToIndex<ToHigh)
   THEN
      IF Escape(From[FromIndex])
      THEN
         (* Skip over Escape Character *)
         INC(FromIndex)
      END ;
      IF FromIndex<FromHigh
      THEN
         (* Copy Normal Character *)
         To[ToIndex] := From[FromIndex] ;
         INC(ToIndex) ;
         INC(FromIndex)
      END
   END
END CopyChar ;


(*
   Narg - returns the number of arguments available from
          command line, CmdLine.
*)

PROCEDURE Narg (CmdLine: ARRAY OF CHAR) : CARDINAL ;
VAR
   a    : ARRAY [0..1000] OF CHAR ;
   ArgNo: CARDINAL ;
BEGIN
   ArgNo := 0 ;
   WHILE GetArg(CmdLine, ArgNo, a) DO
      INC( ArgNo )
   END ;
(*
   IF ArgNo>0
   THEN
      DEC(ArgNo)
   END ;
*)
   RETURN( ArgNo )
END Narg ;


PROCEDURE Escape (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( ch=esc )
END Escape ;


PROCEDURE Space (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch=space) OR (ch=tab) )
END Space ;


PROCEDURE DoubleQuote (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( ch=dquote )
END DoubleQuote ;


PROCEDURE SingleQuote (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( ch=squote )
END SingleQuote ;


END CmdArgs.
