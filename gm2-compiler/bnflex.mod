(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE bnflex ;


FROM PushBackInput IMPORT GetCh, PutCh, PutString, WarnError ;
FROM SymbolKey IMPORT SymbolTree, InitTree, PutSymKey, GetSymKey ;
FROM ASCII IMPORT tab, lf, nul ;
FROM Debug IMPORT Halt ;
FROM NameKey IMPORT Name, LengthKey, MakeKey, GetKey, WriteKey, NulName ;
FROM StrLib IMPORT StrEqual, StrLen ;
FROM FIO IMPORT File, IsNoError ;
FROM StrCase IMPORT Lower ;
FROM StdIO IMPORT Write ;

IMPORT PushBackInput ;


CONST
   MaxNameLength = 8192 ;

VAR
   f            : File ;
   ReservedWords: SymbolTree ;
   CurrentToken : Name ;
   CurrentType  : TokenType ;
   Debugging    ,
   InQuote      : BOOLEAN ;
   QuoteChar    : CHAR ;


(*
   OpenSource - Attempts to open the source file, a.
                The success of the operation is returned.
*)

PROCEDURE OpenSource (a: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   f := PushBackInput.Open(a) ;
   RETURN( IsNoError(f) )
END OpenSource ;


(*
   CloseSource - Closes the current open file.
*)

PROCEDURE CloseSource ;
BEGIN
   PushBackInput.Close(f)
END CloseSource ;


(*
   GetChar - returns the current character on the input stream.
*)

PROCEDURE GetChar () : CHAR ;
BEGIN
   RETURN( PushBackInput.GetCh(f) )
END GetChar ;


(*
   PutChar - pushes a character onto the push back stack, it also
             returns the character which has been pushed.
*)

PROCEDURE PutChar (ch: CHAR) : CHAR ;
BEGIN
   RETURN( PushBackInput.PutCh(f, ch) )
END PutChar ;


(*
   IsWhite - returns TRUE if, ch, is a space or a tab.
*)

PROCEDURE IsWhite (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch=' ') OR (ch=tab) OR (ch=lf) )
END IsWhite ;


(*
   IsDigit - returns TRUE if, ch, is a digit.
*)

PROCEDURE IsDigit (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch>='0') AND (ch<='9') )
END IsDigit ;


(*
   SkipWhite - skips all white space.
*)

PROCEDURE SkipWhite ;
VAR
   ch: CHAR ;
BEGIN
   WHILE IsWhite(PutChar(GetChar())) DO
      ch := GetChar()
   END
END SkipWhite ;


(*
   SkipUntilEoln - skips until a lf is seen. It consumes the lf.
*)

PROCEDURE SkipUntilEoln ;
VAR
   ch: CHAR ;
BEGIN
   WHILE (PutChar(GetChar())#lf) AND (PutChar(GetChar())#nul) DO
      ch := GetChar()
   END ;
   IF PutChar(GetChar())=lf
   THEN
      ch := GetChar()
   END
END SkipUntilEoln ;


(*
   SkipUntilWhite - skips all characters until white space is seen.
*)

PROCEDURE SkipUntilWhite ;
VAR
   ch: CHAR ;
BEGIN
   WHILE ((NOT IsWhite(PutChar(GetChar()))) AND (PutChar(GetChar())#nul)) OR
         (PutChar(GetChar())=lf) DO
      ch := GetChar()
   END
END SkipUntilWhite ;


(*
   IsAlpha - returns TRUE if ch is 'a'..'z' or 'A'..'Z'
*)

PROCEDURE IsAlpha (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( ((ch>='a') AND (ch<='z')) OR ((ch>='A') AND (ch<='Z')) )
END IsAlpha ;


(*
   IsEqual - 
*)

PROCEDURE IsEqual (key: Name; name: ARRAY OF CHAR) : BOOLEAN ;
VAR
   KeyName: ARRAY [0..MaxNameLength] OF CHAR ;
BEGIN
   GetKey(key, KeyName) ;
   RETURN( StrEqual(KeyName, name) )
END IsEqual ;


(*
   IsString - returns TRUE if the string, a, matches the pushed back string.
              if TRUE is returned then this string is consumed, otherwise it is
              left alone.
*)

PROCEDURE IsString (a: ARRAY OF CHAR) : BOOLEAN ;
VAR
   i        : INTEGER ;
   c, Length: CARDINAL ;
BEGIN
   Length := StrLen(a) ;
   c      := 0 ;

   WHILE ((c<Length) AND (PutChar(Lower(GetChar()))=a[c])) DO
      IF GetChar()#a[c]
      THEN
         Halt('assert failed', __LINE__, __FILE__)
      END ;
      INC(c)
   END ;
   IF c=Length
   THEN
      RETURN( TRUE )
   ELSE
      i := c ;
      DEC(i) ;
      WHILE i>=0 DO
         IF PutChar(a[i])#a[i]
         THEN
            Halt('assert failed', __LINE__, __FILE__)
         END ;
         DEC(i)
      END
   END ;
   RETURN( FALSE )
END IsString ;


(*
   IsReserved - returns TRUE if the name is a reserved word.
*)

PROCEDURE IsReserved (name: Name) : BOOLEAN ;
BEGIN
   RETURN (GetSymKey(ReservedWords, name)#0)
END IsReserved ;


(*
   GetCurrentTokenType - returns the type of current token.
*)

PROCEDURE GetCurrentTokenType () : TokenType ;
BEGIN
   RETURN( CurrentType )
END GetCurrentTokenType ;


(*
   GetCurrentToken - returns the NameKey of the current token.
*)

PROCEDURE GetCurrentToken () : Name ;
BEGIN
   RETURN( CurrentToken )
END GetCurrentToken ;


(*
   SkipComments - consumes comments.
*)

PROCEDURE SkipComments ;
BEGIN
   SkipWhite ;
   WHILE PutChar(GetChar())='-' DO
      IF (GetChar()='-') AND (PutChar(GetChar())='-')
      THEN
         (* found comment, skip it *)
         SkipUntilEoln ;
         SkipWhite
      ELSE
         (* no second '-' found thus restore first '-' *)
         IF PutChar('-')='-'
         THEN
         END ;
         RETURN
      END
   END
END SkipComments ;


(*
   WriteToken - 
*)

PROCEDURE WriteToken ;
BEGIN
   WriteKey(CurrentToken) ; Write(' ')
END WriteToken ;


(*
   AdvanceToken - advances to the next token.
*)

PROCEDURE AdvanceToken ;
VAR
   a: ARRAY [0..MaxNameLength] OF CHAR ;
   i: CARDINAL ;
BEGIN
   i := 0 ;
   IF InQuote
   THEN
      IF CurrentType=literaltok
      THEN
         IF PutChar(GetChar())=QuoteChar
         THEN
            a[i] := GetChar() ;
            InQuote := FALSE ;
            INC(i) ;
            a[i] := nul ;
            CurrentToken := MakeKey(a) ;
            CurrentType := VAL(TokenType, GetSymKey(ReservedWords, CurrentToken))
         ELSE
            IF QuoteChar='"'
            THEN
               WarnError('missing " at the end of a literal')
            ELSE
               WarnError("missing ' at the end of a literal")
            END ;
            InQuote := FALSE    (* to avoid a contineous list of the same error message *)
         END
      ELSE
         WHILE (i<MaxNameLength) AND (PutChar(GetChar())#nul) AND
               (PutChar(GetChar())#lf) AND (PutChar(GetChar())#QuoteChar) DO
            a[i] := GetChar() ;
            INC(i)
         END ;
         IF PutChar(GetChar())=QuoteChar
         THEN
            CurrentType := literaltok ;
            a[i] := nul ;
            CurrentToken := MakeKey(a)
         ELSE
            IF QuoteChar='"'
            THEN
               WarnError('missing " at the end of a literal')
            ELSE
               WarnError("missing ' at the end of a literal")
            END ;
            InQuote := FALSE    (* to avoid a contineous list of the same error message *)
         END
      END
   ELSE
      SkipComments ;

      IF (PutChar(GetChar())='"') OR (PutChar(GetChar())="'")
      THEN
         a[i] := GetChar() ;
         QuoteChar := a[i] ;
         INC(i) ;
         InQuote := TRUE ;
         a[i] := nul ;
         CurrentToken := MakeKey(a) ;
         CurrentType := VAL(TokenType, GetSymKey(ReservedWords, CurrentToken))
      ELSE
         WHILE (i<MaxNameLength) AND (PutChar(GetChar())#nul) AND
               (PutChar(GetChar())#lf) AND (PutChar(GetChar())#QuoteChar) AND
               (NOT IsWhite(PutChar(GetChar()))) DO
            a[i] := GetChar() ;
            INC(i)
         END ;
         a[i] := nul ;
         CurrentToken := MakeKey(a) ;
         IF GetSymKey(ReservedWords, CurrentToken)=0
         THEN
            CurrentType := identtok
         ELSE
            CurrentType := VAL(TokenType, GetSymKey(ReservedWords, CurrentToken))
         END
      END
   END ;
   IF Debugging
   THEN
      WriteToken
   END
END AdvanceToken ;


(*
   SymIs - if t is equal to the current token the next token is read
           and true is returned, otherwise false is returned.
*)

PROCEDURE SymIs (t: TokenType) : BOOLEAN ;
BEGIN
   IF CurrentType=t
   THEN
      AdvanceToken ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END SymIs ;


(*
   IsSym - returns the result of the comparison between the current token
           type and t.
*)

PROCEDURE IsSym (t: TokenType) : BOOLEAN ;
BEGIN
   RETURN( t=CurrentType )
END IsSym ;


(*
   PushBackToken - pushes a token back onto input.
*)

PROCEDURE PushBackToken (t: Name) ;
VAR
   a: ARRAY [0..MaxNameLength] OF CHAR ;
BEGIN
   GetKey(t, a) ;
   PutString(f, a)
END PushBackToken ;


(*
   SetDebugging - sets the debugging flag.
*)

PROCEDURE SetDebugging (flag: BOOLEAN) ;
BEGIN
   Debugging := flag
END SetDebugging ;


(*
   Init - initialize the modules global variables.
*)

PROCEDURE Init ;
VAR
   a: ARRAY [0..1] OF CHAR ;
BEGIN
   InitTree(ReservedWords) ;
   Debugging := FALSE ;

   a[0] := nul ;
   PutSymKey(ReservedWords, MakeKey(a)            , ORD(eoftok)) ;
   PutSymKey(ReservedWords, MakeKey('%')          , ORD(codetok)) ;
   PutSymKey(ReservedWords, MakeKey(':=')         , ORD(lbecomestok)) ;
   PutSymKey(ReservedWords, MakeKey('=:')         , ORD(rbecomestok)) ;
   PutSymKey(ReservedWords, MakeKey('|')          , ORD(bartok)) ;
   PutSymKey(ReservedWords, MakeKey('[')          , ORD(lsparatok)) ;
   PutSymKey(ReservedWords, MakeKey(']')          , ORD(rsparatok)) ;
   PutSymKey(ReservedWords, MakeKey('{')          , ORD(lcparatok)) ;
   PutSymKey(ReservedWords, MakeKey('}')          , ORD(rcparatok)) ;
   PutSymKey(ReservedWords, MakeKey('(')          , ORD(lparatok)) ;
   PutSymKey(ReservedWords, MakeKey(')')          , ORD(rparatok)) ;
   PutSymKey(ReservedWords, MakeKey('<')          , ORD(lesstok)) ;
   PutSymKey(ReservedWords, MakeKey('>')          , ORD(gretok)) ;
   PutSymKey(ReservedWords, MakeKey('error')      , ORD(errortok)) ;
   PutSymKey(ReservedWords, MakeKey('tokenfunc')  , ORD(tfunctok)) ;
   PutSymKey(ReservedWords, MakeKey('symfunc')    , ORD(symfunctok)) ;
   PutSymKey(ReservedWords, MakeKey("'")          , ORD(squotetok)) ;
   PutSymKey(ReservedWords, MakeKey('"')          , ORD(dquotetok)) ;
   PutSymKey(ReservedWords, MakeKey('module')     , ORD(moduletok)) ;
   PutSymKey(ReservedWords, MakeKey('begin')      , ORD(begintok)) ;
   PutSymKey(ReservedWords, MakeKey('rules')      , ORD(rulestok)) ;
   PutSymKey(ReservedWords, MakeKey('end')        , ORD(endtok)) ;
   PutSymKey(ReservedWords, MakeKey('declaration'), ORD(declarationtok)) ;
   PutSymKey(ReservedWords, MakeKey('token')      , ORD(tokentok)) ;
   PutSymKey(ReservedWords, MakeKey('special')    , ORD(specialtok)) ;
   PutSymKey(ReservedWords, MakeKey('first')      , ORD(firsttok)) ;
   PutSymKey(ReservedWords, MakeKey('follow')     , ORD(followtok)) ;
   PutSymKey(ReservedWords, MakeKey('epsilon')    , ORD(epsilontok)) ;
   PutSymKey(ReservedWords, MakeKey('BNF')        , ORD(BNFtok)) ;
   PutSymKey(ReservedWords, MakeKey('FNB')        , ORD(FNBtok)) ;

   CurrentToken := NulName ;
   CurrentType  := identtok ;
   InQuote      := FALSE

END Init ;


BEGIN
   Init
END bnflex.
(*
 * Local variables:
 *  compile-command: "../bin2/m2f -quiet -g -verbose -M \"../libs ../gm2s\" bnflex.mod"
 * End:
 *)
