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

IMPLEMENTATION MODULE M2LexBuf ;

FROM SYSTEM IMPORT ADDRESS ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM Strings IMPORT string, InitStringCharStar, Equal, Mark ;
FROM NameKey IMPORT Name, makekey, KeyToCharStar ;
FROM M2Reserved IMPORT toktype ;
FROM M2Printf IMPORT printf1 ;
FROM M2Error IMPORT FormatWarningMessage2 ;

IMPORT m2lex ;

CONST
   MaxBucketSize = 100 ;

TYPE
   SourceList = POINTER TO sourcelist ;
   sourcelist =            RECORD
                              left,
                              right: SourceList ;
                              name : String ;
                           END ;

   TokenDesc = RECORD
                  token: toktype ;
                  str  : Name ;
                  int  : INTEGER ;
                  line : CARDINAL ;
                  file : SourceList ;
               END ;

   TokenBucket = POINTER TO tokenbucket ;
   tokenbucket =            RECORD
                               buf : ARRAY [0..MaxBucketSize] OF TokenDesc ;
                               len : CARDINAL ;
                               next: TokenBucket ;
                            END ;

   ListDesc = RECORD
                 head,
                 tail            : TokenBucket ;
                 LastBucketOffset: CARDINAL ;
              END ;

VAR
   CurrentSource: SourceList ;
   CurrentUsed  : BOOLEAN ;
   ListOfTokens : ListDesc ;
   CurrentTokNo : CARDINAL ;

(* %%%FORWARD%%%
PROCEDURE AddTokToList (t: toktype; n: Name;
                        i: INTEGER; l: CARDINAL; f: SourceList) ; FORWARD ;
   %%%FORWARD%%% *)


(*
   Init - initializes the token list and source list.
*)

PROCEDURE Init ;
BEGIN
   CurrentTokNo := 0 ;
   CurrentSource := NIL ;
   ListOfTokens.head := NIL ;
   ListOfTokens.tail := NIL
END Init ;


(*
   AddTo - adds a new element to the end of SourceList, CurrentSource.
*)

PROCEDURE AddTo (l: SourceList) ;
BEGIN
   l^.right := CurrentSource ;
   l^.left  := CurrentSource^.left ;
   CurrentSource^.left^.right := l ;
   CurrentSource^.left := l
END AddTo ;


(*
   SubFrom - subtracts, l, from the source list.
*)

PROCEDURE SubFrom (l: SourceList) ;
BEGIN
   l^.left^.right := l^.right ;
   l^.right^.left := l^.left
END SubFrom ;


(*
   NewElement - returns a new SourceList
*)

PROCEDURE NewElement (s: ADDRESS) : SourceList ;
VAR
   l: SourceList ;
BEGIN
   NEW(l) ;
   IF l=NIL
   THEN
      HALT
   ELSE
      WITH l^ DO
         name  := InitStringCharStar(s) ;
         left  := NIL ;
         right := NIL
      END
   END ;
   RETURN( l )
END NewElement ;


(*
   NewList - initializes an empty list with the classic dummy header element.
*)

PROCEDURE NewList () : SourceList ;
VAR
   l: SourceList ;
BEGIN
   NEW(l) ;
   WITH l^ DO
      left  := l ;
      right := l ;
      name  := NIL
   END ;
   RETURN( l )
END NewList ;


(*
   CheckIfNeedToDuplicate - checks to see whether the CurrentSource has
                            been used, if it has then duplicate the list.
*)

PROCEDURE CheckIfNeedToDuplicate ;
VAR
   l, h: SourceList ;
BEGIN
   IF CurrentUsed
   THEN
      l := CurrentSource^.right ;
      h := CurrentSource ;
      CurrentSource := NewList() ;
      WHILE l#h DO
         AddTo(NewElement(l^.name)) ;
         l := l^.right
      END
   END
END CheckIfNeedToDuplicate ;


(*
   PushFile - indicates that, filename, has just been included.
*)

PROCEDURE PushFile (filename: ADDRESS) ;
BEGIN
   CheckIfNeedToDuplicate ;
   AddTo(NewElement(filename))
END PushFile ;


(*
   PopFile - indicates that we are returning to, filename, having finished
             an include.
*)

PROCEDURE PopFile (filename: ADDRESS) ;
VAR
   l: SourceList ;
BEGIN
   CheckIfNeedToDuplicate ;
   IF (CurrentSource#NIL) AND (CurrentSource^.left#CurrentSource)
   THEN
      l := CurrentSource^.left ;  (* last element *)
      SubFrom(l) ;
      DISPOSE(l) ;
      IF (CurrentSource^.left#CurrentSource) AND
         (NOT Equal(CurrentSource^.name, Mark(InitStringCharStar(filename))))
      THEN
         (* mismatch in source file names after preprocessing files *)
      END
   ELSE
      (* source file list is empty, cannot pop an include.. *)
   END
END PopFile ;


(*
   KillList - kills the SourceList providing that it has not been used.
*)

PROCEDURE KillList ;
VAR
   l, k: SourceList ;
BEGIN
   IF (NOT CurrentUsed) AND (CurrentSource#NIL)
   THEN
      l := CurrentSource ;
      REPEAT
         k := l ;
         l := l^.right ;
         DISPOSE(k)
      UNTIL l=CurrentSource
   END
END KillList ;


(*
   SetFile - sets the current filename to, filename.
*)

PROCEDURE SetFile (filename: ADDRESS) ;
BEGIN
   KillList ;
   CurrentUsed   := FALSE ;
   CurrentSource := NewList() ;
   AddTo(NewElement(filename))
END SetFile ;


(*
   OpenSource - Attempts to open the source file, s.
                The success of the operation is returned.
*)

PROCEDURE OpenSource (s: String) : BOOLEAN ;
BEGIN
   IF m2lex.OpenSource(string(s))
   THEN
      SetFile(string(s)) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END OpenSource ;


(*
   CloseSource - closes the current open file.
*)

PROCEDURE CloseSource ;
BEGIN
   (* a subsequent call to m2lex.OpenSource will really close the file *)
END CloseSource ;


(*
   GetToken - gets the next token into currenttoken.
*)

PROCEDURE GetToken ;
VAR
   a: ADDRESS ;
BEGIN
   IF ListOfTokens.tail=NIL
   THEN
      a := m2lex.GetToken() ;
      IF ListOfTokens.tail=NIL
      THEN
         HALT
      END
   END ;
   WITH ListOfTokens.tail^ DO
      IF CurrentTokNo-ListOfTokens.LastBucketOffset<len
      THEN
         WITH buf[CurrentTokNo-ListOfTokens.LastBucketOffset] DO
            currenttoken   := token ;
            currentstring  := KeyToCharStar(str) ;
            currentinteger := int
         END ;
         INC(CurrentTokNo)
      ELSE
         a := m2lex.GetToken() ;
         GetToken
      END
   END
END GetToken ;


(*
   InsertToken - inserts a symbol, token, infront of the current token
                 ready for the next pass.
*)

PROCEDURE InsertToken (token: toktype) ;
BEGIN
   IF ListOfTokens.tail#NIL
   THEN
      WITH ListOfTokens.tail^ DO
         IF len>0
         THEN
            buf[len-1].token := token
         END
      END ;
      AddTokToList(currenttoken, NIL, 0, GetLineNo(), CurrentSource) ;
      GetToken
   END
END InsertToken ;


(*
   InsertTokenAndRewind - inserts a symbol, token, infront of the current token
                          and then moves the token stream back onto the inserted token.
*)

PROCEDURE InsertTokenAndRewind (token: toktype) ;
BEGIN
   IF ListOfTokens.tail#NIL
   THEN
      WITH ListOfTokens.tail^ DO
         IF len>0
         THEN
            buf[len-1].token := token
         END
      END ;
      AddTokToList(currenttoken, NIL, 0, GetLineNo(), CurrentSource) ;
      currenttoken := token
   END
END InsertTokenAndRewind ;


(*
   GetPreviousTokenLineNo - returns the line number of the previous token.
*)

PROCEDURE GetPreviousTokenLineNo () : CARDINAL ;
BEGIN
   IF CurrentTokNo>0
   THEN
      RETURN( TokenToLineNo(CurrentTokNo-1) )
   ELSE
      RETURN( 0 )
   END
END GetPreviousTokenLineNo ;


(*
   GetLineNo - returns the current line number where the symbol occurs in
               the source file.
*)

PROCEDURE GetLineNo () : CARDINAL ;
BEGIN
   RETURN( TokenToLineNo(CurrentTokNo) )
END GetLineNo ;


(*
   GetTokenNo - returns the number of tokens read from
                the source file by the lexical analaysis.
*)

PROCEDURE GetTokenNo () : CARDINAL ;
BEGIN
   RETURN( CurrentTokNo-1 )
END GetTokenNo ;


(*
   FindTokenBucket - returns the TokenBucket corresponding to the TokenNo.
*)

PROCEDURE FindTokenBucket (VAR TokenNo: CARDINAL) : TokenBucket ;
VAR
   b: TokenBucket ;
BEGIN
   b := ListOfTokens.head ;
   WHILE b#NIL DO
      WITH b^ DO
         IF TokenNo<len
         THEN
            RETURN( b )
         ELSE
            DEC(TokenNo, len)
         END
      END ;
      b := b^.next
   END ;
   RETURN( NIL )
END FindTokenBucket ;


(*
   TokenToLineNo - returns the line number of the current file for the
                   Token.
*)

PROCEDURE TokenToLineNo (TokenNo: CARDINAL) : CARDINAL ;
VAR
   b: TokenBucket ;
BEGIN
   b := FindTokenBucket(TokenNo) ;
   IF b=NIL
   THEN
      RETURN( 0 )
   ELSE
      RETURN( b^.buf[TokenNo].line )
   END
END TokenToLineNo ;


(*
   FindFileNameFromToken - returns the complete FileName for the appropriate
                           source file yields the token number, Token.
*)

PROCEDURE FindFileNameFromToken (TokenNo: CARDINAL) : String ;
VAR
   b: TokenBucket ;
BEGIN
   b := FindTokenBucket(TokenNo) ;
   IF b=NIL
   THEN
      RETURN( NIL )
   ELSE
      RETURN( b^.buf[TokenNo].file^.left^.name )
   END
END FindFileNameFromToken ;


(*
   GetFileName - returns a String defining the current file.
*)

PROCEDURE GetFileName () : String ;
BEGIN
   RETURN( FindFileNameFromToken(CurrentTokNo) )
END GetFileName ;


(*
   AddTokToList - adds a token to a dynamic list.
*)

PROCEDURE AddTokToList (t: toktype; n: Name;
                        i: INTEGER; l: CARDINAL; f: SourceList) ;
VAR
   b: TokenBucket ;
BEGIN
   IF ListOfTokens.head=NIL
   THEN
      NEW(ListOfTokens.head) ;
      IF ListOfTokens.head=NIL
      THEN
         (* memory error *)
      END ;
      ListOfTokens.tail := ListOfTokens.head
   ELSIF ListOfTokens.tail^.len=MaxBucketSize
   THEN
      NEW(ListOfTokens.tail^.next) ;
      IF ListOfTokens.tail^.next=NIL
      THEN
         (* memory error *)
      ELSE
         ListOfTokens.tail := ListOfTokens.tail^.next
      END ;
      INC(ListOfTokens.LastBucketOffset, MaxBucketSize)
   END ;
   WITH ListOfTokens.tail^ DO
      WITH buf[len] DO
         token := t ;
         str   := n ;
         int   := i ;
         line  := l ;
         file  := f
      END ;
      INC(len)
   END
END AddTokToList ;


(* ***********************************************************************
 *
 * These functions allow m2.lex to deliver tokens into the buffer
 *
 ************************************************************************* *)

(*
   AddTok - adds a token to the buffer.
*)

PROCEDURE AddTok (t: toktype) ;
BEGIN
   AddTokToList(t, NIL, 0, m2lex.GetLineNo(), CurrentSource)
END AddTok ;


(*
   AddTokCharStar - adds a token to the buffer and an additional string, s.
                    A copy of string, s, is made.
*)

PROCEDURE AddTokCharStar (t: toktype; s: ADDRESS) ;
BEGIN
   AddTokToList(t, makekey(s), 0, m2lex.GetLineNo(), CurrentSource)   
END AddTokCharStar ;


(*
   AddTokInteger - adds a token and an integer to the buffer.
*)

PROCEDURE AddTokInteger (t: toktype; i: INTEGER) ;
BEGIN
   AddTokToList(t, NIL, i, m2lex.GetLineNo(), CurrentSource)   
END AddTokInteger ;


BEGIN
   Init
END M2LexBuf.
