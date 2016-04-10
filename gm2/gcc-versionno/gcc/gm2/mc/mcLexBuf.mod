(* Copyright (C) 2015
                 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  *)

IMPLEMENTATION MODULE mcLexBuf ;

IMPORT mcflex ;

FROM libc IMPORT strlen ;
FROM SYSTEM IMPORT ADDRESS ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM DynamicStrings IMPORT string, InitString, InitStringCharStar, Equal, Mark, KillString ;
FROM FormatStrings IMPORT Sprintf1 ;
FROM nameKey IMPORT NulName, Name, makekey, keyToCharStar ;
FROM mcReserved IMPORT toktype ;
FROM mcPrintf IMPORT printf0, printf1, printf2, printf3 ;
FROM mcDebug IMPORT assert ;


CONST
   MaxBucketSize = 100 ;
   Debugging     = FALSE ;

TYPE
   sourceList = POINTER TO RECORD
                              left,
                              right: sourceList ;
                              name : String ;
                              line : CARDINAL ;
                              col  : CARDINAL ;
                           END ;

   tokenDesc = RECORD
                  token: toktype ;
                  str  : Name ;
                  int  : INTEGER ;
                  line : CARDINAL ;
                  col  : CARDINAL ;
                  file : sourceList ;
               END ;

   tokenBucket = POINTER TO RECORD
                               buf : ARRAY [0..MaxBucketSize] OF tokenDesc ;
                               len : CARDINAL ;
                               next: tokenBucket ;
                            END ;

   listDesc = RECORD
                 head,
                 tail            : tokenBucket ;
                 lastBucketOffset: CARDINAL ;
              END ;

VAR
   currentSource    : sourceList ;
   useBufferedTokens,
   currentUsed      : BOOLEAN ;
   listOfTokens     : listDesc ;
   currentTokNo     : CARDINAL ;



(*
   init - initializes the token list and source list.
*)

PROCEDURE init ;
BEGIN
   currenttoken := eoftok ;
   currentTokNo := 0 ;
   currentSource := NIL ;
   listOfTokens.head := NIL ;
   listOfTokens.tail := NIL ;
   useBufferedTokens := FALSE
END init ;


(*
   addTo - adds a new element to the end of sourceList, currentSource.
*)

PROCEDURE addTo (l: sourceList) ;
BEGIN
   l^.right := currentSource ;
   l^.left  := currentSource^.left ;
   currentSource^.left^.right := l ;
   currentSource^.left := l ;
   WITH l^.left^ DO
      line := mcflex.getLineNo() ;
      col  := mcflex.getColumnNo()
   END
END addTo ;


(*
   subFrom - subtracts, l, from the source list.
*)

PROCEDURE subFrom (l: sourceList) ;
BEGIN
   l^.left^.right := l^.right ;
   l^.right^.left := l^.left
END subFrom ;


(*
   newElement - returns a new sourceList
*)

PROCEDURE newElement (s: ADDRESS) : sourceList ;
VAR
   l: sourceList ;
BEGIN
   NEW (l) ;
   IF l=NIL
   THEN
      HALT
   ELSE
      WITH l^ DO
         name  := InitStringCharStar (s) ;
         left  := NIL ;
         right := NIL
      END
   END ;
   RETURN l
END newElement ;


(*
   newList - initializes an empty list with the classic dummy header element.
*)

PROCEDURE newList () : sourceList ;
VAR
   l: sourceList ;
BEGIN
   NEW (l) ;
   WITH l^ DO
      left  := l ;
      right := l ;
      name  := NIL
   END ;
   RETURN l
END newList ;


(*
   checkIfNeedToDuplicate - checks to see whether the currentSource has
                            been used, if it has then duplicate the list.
*)

PROCEDURE checkIfNeedToDuplicate ;
VAR
   l, h: sourceList ;
BEGIN
   IF currentUsed
   THEN
      l := currentSource^.right ;
      h := currentSource ;
      currentSource := newList() ;
      WHILE l#h DO
         addTo (newElement (l^.name)) ;
         l := l^.right
      END
   END
END checkIfNeedToDuplicate ;


(*
   pushFile - indicates that, filename, has just been included.
*)

PROCEDURE pushFile (filename: ADDRESS) ;
VAR
   l: sourceList ;
BEGIN
   checkIfNeedToDuplicate ;
   addTo (newElement (filename)) ;
   IF Debugging
   THEN
      IF currentSource^.right#currentSource
      THEN
         l := currentSource ;
         REPEAT
            printf3 ('name = %s, line = %d, col = %d\n', l^.name, l^.line, l^.col) ;
            l := l^.right
         UNTIL l=currentSource
      END
   END
END pushFile ;


(*
   popFile - indicates that we are returning to, filename, having finished
             an include.
*)

PROCEDURE popFile (filename: ADDRESS) ;
VAR
   l: sourceList ;
BEGIN
   checkIfNeedToDuplicate ;
   IF (currentSource#NIL) AND (currentSource^.left#currentSource)
   THEN
      l := currentSource^.left ;  (* last element *)
      subFrom (l) ;
      DISPOSE (l) ;
      IF (currentSource^.left#currentSource) AND
         (NOT Equal(currentSource^.name, Mark (InitStringCharStar (filename))))
      THEN
         (* mismatch in source file names after preprocessing files *)
      END
   ELSE
      (* source file list is empty, cannot pop an include.. *)
   END
END popFile ;


(*
   killList - kills the sourceList providing that it has not been used.
*)

PROCEDURE killList ;
VAR
   l, k: sourceList ;
BEGIN
   IF (NOT currentUsed) AND (currentSource#NIL)
   THEN
      l := currentSource ;
      REPEAT
         k := l ;
         l := l^.right ;
         DISPOSE (k)
      UNTIL l=currentSource
   END
END killList ;


(*
   reInitialize - re-initialize the all the data structures.
*)

PROCEDURE reInitialize ;
VAR
   s, t: tokenBucket ;
BEGIN
   IF listOfTokens.head#NIL
   THEN
      t := listOfTokens.head ;
      REPEAT
         s := t ;
         t := t^.next ;
         DISPOSE (s) ;
      UNTIL t=NIL ;
      currentUsed := FALSE ;
      killList
   END ;
   init
END reInitialize ;


(*
   setFile - sets the current filename to, filename.
*)

PROCEDURE setFile (filename: ADDRESS) ;
BEGIN
   killList ;
   currentUsed   := FALSE ;
   currentSource := newList() ;
   addTo (newElement (filename))
END setFile ;


(*
   openSource - attempts to open the source file, s.
                The success of the operation is returned.
*)

PROCEDURE openSource (s: String) : BOOLEAN ;
BEGIN
   IF useBufferedTokens
   THEN
      getToken ;
      RETURN TRUE
   ELSE
      IF mcflex.openSource (string (s))
      THEN
         setFile (string (s)) ;
         syncOpenWithBuffer ;
         getToken ;
         RETURN TRUE
      ELSE
         RETURN FALSE
      END
   END
END openSource ;


(*
   closeSource - closes the current open file.
*)

PROCEDURE closeSource ;
BEGIN
   IF useBufferedTokens
   THEN
      WHILE currenttoken#eoftok DO
         getToken
      END
   ELSE
      (* a subsequent call to mcflex.OpenSource will really close the file *)
   END
END closeSource ;


(*
   resetForNewPass - reset the buffer pointers to the beginning ready for
                     a new pass
*)

PROCEDURE resetForNewPass ;
BEGIN
   currentTokNo := 0 ;
   useBufferedTokens := TRUE
END resetForNewPass ;


(*
   displayToken -
*)

PROCEDURE displayToken ;
BEGIN
   IF currenttoken=identtok
   THEN
      printf1 ('currenttoken = %a\n', currentstring)
   ELSE
      CASE currenttoken OF

      eoftok: printf0('eoftok\n') |
      plustok: printf0('plustok\n') |
      minustok: printf0('minustok\n') |
      timestok: printf0('timestok\n') |
      dividetok: printf0('dividetok\n') |
      becomestok: printf0('becomestok\n') |
      ambersandtok: printf0('ambersandtok\n') |
      periodtok: printf0('periodtok\n') |
      commatok: printf0('commatok\n') |
      semicolontok: printf0('semicolontok\n') |
      lparatok: printf0('lparatok\n') |
      rparatok: printf0('rparatok\n') |
      lsbratok: printf0('lsbratok\n') |
      rsbratok: printf0('rsbratok\n') |
      lcbratok: printf0('lcbratok\n') |
      rcbratok: printf0('rcbratok\n') |
      uparrowtok: printf0('uparrowtok\n') |
      singlequotetok: printf0('singlequotetok\n') |
      equaltok: printf0('equaltok\n') |
      hashtok: printf0('hashtok\n') |
      lesstok: printf0('lesstok\n') |
      greatertok: printf0('greatertok\n') |
      lessgreatertok: printf0('lessgreatertok\n') |
      lessequaltok: printf0('lessequaltok\n') |
      greaterequaltok: printf0('greaterequaltok\n') |
      periodperiodtok: printf0('periodperiodtok\n') |
      colontok: printf0('colontok\n') |
      doublequotestok: printf0('doublequotestok\n') |
      bartok: printf0('bartok\n') |
      andtok: printf0('andtok\n') |
      arraytok: printf0('arraytok\n') |
      begintok: printf0('begintok\n') |
      bytok: printf0('bytok\n') |
      casetok: printf0('casetok\n') |
      consttok: printf0('consttok\n') |
      definitiontok: printf0('definitiontok\n') |
      divtok: printf0('divtok\n') |
      dotok: printf0('dotok\n') |
      elsetok: printf0('elsetok\n') |
      elsiftok: printf0('elsiftok\n') |
      endtok: printf0('endtok\n') |
      exittok: printf0('exittok\n') |
      exporttok: printf0('exporttok\n') |
      fortok: printf0('fortok\n') |
      fromtok: printf0('fromtok\n') |
      iftok: printf0('iftok\n') |
      implementationtok: printf0('implementationtok\n') |
      importtok: printf0('importtok\n') |
      intok: printf0('intok\n') |
      looptok: printf0('looptok\n') |
      modtok: printf0('modtok\n') |
      moduletok: printf0('moduletok\n') |
      nottok: printf0('nottok\n') |
      oftok: printf0('oftok\n') |
      ortok: printf0('ortok\n') |
      pointertok: printf0('pointertok\n') |
      proceduretok: printf0('proceduretok\n') |
      qualifiedtok: printf0('qualifiedtok\n') |
      unqualifiedtok: printf0('unqualifiedtok\n') |
      recordtok: printf0('recordtok\n') |
      repeattok: printf0('repeattok\n') |
      returntok: printf0('returntok\n') |
      settok: printf0('settok\n') |
      thentok: printf0('thentok\n') |
      totok: printf0('totok\n') |
      typetok: printf0('typetok\n') |
      untiltok: printf0('untiltok\n') |
      vartok: printf0('vartok\n') |
      whiletok: printf0('whiletok\n') |
      withtok: printf0('withtok\n') |
      asmtok: printf0('asmtok\n') |
      volatiletok: printf0('volatiletok\n') |
      periodperiodperiodtok: printf0('periodperiodperiodtok\n') |
      datetok: printf0('datetok\n') |
      linetok: printf0('linetok\n') |
      filetok: printf0('filetok\n') |
      integertok: printf0('integertok\n') |
      identtok: printf0('identtok\n') |
      realtok: printf0('realtok\n') |
      stringtok: printf0('stringtok\n')

      ELSE
      END
   END
END displayToken ;


(*
   updateFromBucket - updates the global variables:  currenttoken,
                      currentstring, currentcolumn and currentinteger
                      from tokenBucket, b, and, offset.
*)

PROCEDURE updateFromBucket (b: tokenBucket; offset: CARDINAL) ;
BEGIN
   WITH b^.buf[offset] DO
      currenttoken   := token ;
      currentstring  := keyToCharStar (str) ;
      currentcolumn  := col ;
      currentinteger := int ;
      IF Debugging
      THEN
         printf3 ('line %d (# %d  %d) ', line, offset, currentTokNo)
      END
   END
END updateFromBucket ;


(*
   getToken - gets the next token into currenttoken.
*)

PROCEDURE getToken ;
VAR
   a: ADDRESS ;
   t: CARDINAL ;
   b: tokenBucket ;
BEGIN
   IF useBufferedTokens
   THEN
      t := currentTokNo ;
      b := findtokenBucket (t) ;
      updateFromBucket (b, t)
   ELSE
      IF listOfTokens.tail=NIL
      THEN
         a := mcflex.getToken () ;
         IF listOfTokens.tail=NIL
         THEN
            HALT
         END
      END ;
      IF currentTokNo>=listOfTokens.lastBucketOffset
      THEN
         (* currentTokNo is in the last bucket or needs to be read.  *)
         IF currentTokNo-listOfTokens.lastBucketOffset<listOfTokens.tail^.len
         THEN
            updateFromBucket (listOfTokens.tail,
                              currentTokNo-listOfTokens.lastBucketOffset)
         ELSE
            (* call the lexical phase to place a new token into the last bucket.  *)
            a := mcflex.getToken () ;
            getToken ; (* and call ourselves again to collect the token from bucket.  *)
            RETURN
         END
      ELSE
         t := currentTokNo ;
         b := findtokenBucket (t) ;
         updateFromBucket (b, t)
      END
   END ;
   IF Debugging
   THEN
      displayToken
   END ;
   INC (currentTokNo)
END getToken ;


(*
   syncOpenWithBuffer - synchronise the buffer with the start of a file.
                        Skips all the tokens to do with the previous file.
*)

PROCEDURE syncOpenWithBuffer ;
BEGIN
   IF listOfTokens.tail#NIL
   THEN
      WITH listOfTokens.tail^ DO
         currentTokNo := listOfTokens.lastBucketOffset+len
      END
   END
END syncOpenWithBuffer ;


(*
   insertToken - inserts a symbol, token, infront of the current token
                 ready for the next pass.
*)

PROCEDURE insertToken (token: toktype) ;
BEGIN
   IF listOfTokens.tail#NIL
   THEN
      WITH listOfTokens.tail^ DO
         IF len>0
         THEN
            buf[len-1].token := token
         END
      END ;
      addTokToList (currenttoken, NulName, 0,
                    getLineNo (), getColumnNo (), currentSource) ;
      getToken
   END
END insertToken ;


(*
   insertTokenAndRewind - inserts a symbol, token, infront of the current token
                          and then moves the token stream back onto the inserted token.
*)

PROCEDURE insertTokenAndRewind (token: toktype) ;
BEGIN
   IF listOfTokens.tail#NIL
   THEN
      WITH listOfTokens.tail^ DO
         IF len>0
         THEN
            buf[len-1].token := token
         END
      END ;
      addTokToList (currenttoken, NulName, 0,
                    getLineNo(), getColumnNo(), currentSource) ;
      currenttoken := token
   END
END insertTokenAndRewind ;


(*
   getPreviousTokenLineNo - returns the line number of the previous token.
*)

PROCEDURE getPreviousTokenLineNo () : CARDINAL ;
BEGIN
   RETURN getLineNo()
END getPreviousTokenLineNo ;


(*
   getLineNo - returns the current line number where the symbol occurs in
               the source file.
*)

PROCEDURE getLineNo () : CARDINAL ;
BEGIN
   IF currentTokNo=0
   THEN
      RETURN 0
   ELSE
      RETURN tokenToLineNo (getTokenNo (), 0)
   END
END getLineNo ;


(*
   getColumnNo - returns the current column where the symbol occurs in
                 the source file.
*)

PROCEDURE getColumnNo () : CARDINAL ;
BEGIN
   IF currentTokNo=0
   THEN
      RETURN 0
   ELSE
      RETURN tokenToColumnNo (getTokenNo (), 0)
   END
END getColumnNo ;


(*
   getTokenNo - returns the current token number.
*)

PROCEDURE getTokenNo () : CARDINAL ;
BEGIN
   IF currentTokNo=0
   THEN
      RETURN 0
   ELSE
      RETURN currentTokNo-1
   END
END getTokenNo ;


(*
   findtokenBucket - returns the tokenBucket corresponding to the tokenNo.
*)

PROCEDURE findtokenBucket (VAR tokenNo: CARDINAL) : tokenBucket ;
VAR
   b: tokenBucket ;
BEGIN
   b := listOfTokens.head ;
   WHILE b#NIL DO
      WITH b^ DO
         IF tokenNo<len
         THEN
            RETURN b
         ELSE
            DEC (tokenNo, len)
         END
      END ;
      b := b^.next
   END ;
   RETURN NIL
END findtokenBucket ;


(*
   tokenToLineNo - returns the line number of the current file for the
                   tokenNo.  The depth refers to the include depth.
                   A depth of 0 is the current file, depth of 1 is the file
                   which included the current file.  Zero is returned if the
                   depth exceeds the file nesting level.
*)

PROCEDURE tokenToLineNo (tokenNo: CARDINAL; depth: CARDINAL) : CARDINAL ;
VAR
   b: tokenBucket ;
   l: sourceList ;
BEGIN
   b := findtokenBucket (tokenNo) ;
   IF b=NIL
   THEN
      RETURN 0
   ELSE
      IF depth=0
      THEN
         RETURN b^.buf[tokenNo].line
      ELSE
         l := b^.buf[tokenNo].file^.left ;
         WHILE depth>0 DO
            l := l^.left ;
            IF l=b^.buf[tokenNo].file^.left
            THEN
               RETURN 0
            END ;
            DEC (depth)
         END ;
         RETURN l^.line
      END
   END
END tokenToLineNo ;


(*
   tokenToColumnNo - returns the column number of the current file for the
                     tokenNo. The depth refers to the include depth.
                     A depth of 0 is the current file, depth of 1 is the file
                     which included the current file. Zero is returned if the
                     depth exceeds the file nesting level.
*)

PROCEDURE tokenToColumnNo (tokenNo: CARDINAL; depth: CARDINAL) : CARDINAL ;
VAR
   b: tokenBucket ;
   l: sourceList ;
BEGIN
   b := findtokenBucket (tokenNo) ;
   IF b=NIL
   THEN
      RETURN 0
   ELSE
      IF depth=0
      THEN
         RETURN b^.buf[tokenNo].col
      ELSE
         l := b^.buf[tokenNo].file^.left ;
         WHILE depth>0 DO
            l := l^.left ;
            IF l=b^.buf[tokenNo].file^.left
            THEN
               RETURN 0
            END ;
            DEC (depth)
         END ;
         RETURN l^.col
      END
   END
END tokenToColumnNo ;


(*
   findFileNameFromToken - returns the complete FileName for the appropriate
                           source file yields the token number, tokenNo.
                           The, Depth, indicates the include level: 0..n
                           Level 0 is the current. NIL is returned if n+1
                           is requested.
*)

PROCEDURE findFileNameFromToken (tokenNo: CARDINAL; depth: CARDINAL) : String ;
VAR
   b: tokenBucket ;
   l: sourceList ;
BEGIN
   b := findtokenBucket (tokenNo) ;
   IF b=NIL
   THEN
      RETURN NIL
   ELSE
      l := b^.buf[tokenNo].file^.left ;
      WHILE depth>0 DO
         l := l^.left ;
         IF l=b^.buf[tokenNo].file^.left
         THEN
            RETURN NIL
         END ;
         DEC (depth)
      END ;
      RETURN l^.name
   END
END findFileNameFromToken ;


(*
   getFileName - returns a String defining the current file.
*)

PROCEDURE getFileName () : String ;
BEGIN
   RETURN findFileNameFromToken (getTokenNo (), 0)
END getFileName ;


PROCEDURE stop ; BEGIN END stop ;


(*
   addTokToList - adds a token to a dynamic list.
*)

PROCEDURE addTokToList (t: toktype; n: Name;
                        i: INTEGER; l: CARDINAL; c: CARDINAL; f: sourceList) ;
VAR
   b: tokenBucket ;
BEGIN
   IF listOfTokens.head=NIL
   THEN
      NEW (listOfTokens.head) ;
      IF listOfTokens.head=NIL
      THEN
         (* list error *)
      END ;
      listOfTokens.tail := listOfTokens.head ;
      listOfTokens.tail^.len := 0
   ELSIF listOfTokens.tail^.len=MaxBucketSize
   THEN
      assert (listOfTokens.tail^.next=NIL) ;
      NEW (listOfTokens.tail^.next) ;
      IF listOfTokens.tail^.next=NIL
      THEN
         (* list error *)
      ELSE
         listOfTokens.tail := listOfTokens.tail^.next ;
         listOfTokens.tail^.len := 0
      END ;
      INC (listOfTokens.lastBucketOffset, MaxBucketSize)
   END ;
   WITH listOfTokens.tail^ DO
      next := NIL ;
      WITH buf[len] DO
         token := t ;
         str   := n ;
         int   := i ;
         line  := l ;
         col   := c ;
         file  := f
      END ;
      INC (len)
   END
END addTokToList ;


(*
   isLastTokenEof - returns TRUE if the last token was an eoftok
*)

PROCEDURE isLastTokenEof () : BOOLEAN ;
VAR
   t: CARDINAL ;
   b: tokenBucket ;
BEGIN
   IF listOfTokens.tail#NIL
   THEN
      IF listOfTokens.tail^.len=0
      THEN
         b := listOfTokens.head ;
         IF b=listOfTokens.tail
         THEN
            RETURN FALSE
         END ;
         WHILE b^.next#listOfTokens.tail DO
            b := b^.next
         END ;
      ELSE
         b := listOfTokens.tail
      END ;
      WITH b^ DO
         assert (len>0) ;     (* len should always be >0 *)
         RETURN buf[len-1].token=eoftok
      END
   END ;
   RETURN FALSE
END isLastTokenEof ;


(* ***********************************************************************
 *
 * These functions allow m2.flex to deliver tokens into the buffer
 *
 ************************************************************************* *)

(*
   addTok - adds a token to the buffer.
*)

PROCEDURE addTok (t: toktype) ;
BEGIN
   IF NOT ((t=eoftok) AND isLastTokenEof())
   THEN
      addTokToList (t, NulName, 0,
                    mcflex.getLineNo (), mcflex.getColumnNo (), currentSource) ;
      currentUsed := TRUE
   END
END addTok ;


(*
   addTokCharStar - adds a token to the buffer and an additional string, s.
                    A copy of string, s, is made.
*)

PROCEDURE addTokCharStar (t: toktype; s: ADDRESS) ;
BEGIN
   IF strlen(s)>80
   THEN
      stop
   END ;
   addTokToList (t, makekey (s), 0, mcflex.getLineNo (),
                 mcflex.getColumnNo (), currentSource) ;
   currentUsed := TRUE
END addTokCharStar ;


(*
   addTokInteger - adds a token and an integer to the buffer.
*)

PROCEDURE addTokInteger (t: toktype; i: INTEGER) ;
VAR
   s: String ;
   c,
   l: CARDINAL ;
BEGIN
   l := mcflex.getLineNo () ;
   c := mcflex.getColumnNo () ;
   s := Sprintf1 (Mark (InitString ('%d')), i) ;
   addTokToList (t, makekey(string(s)), i, l, c, currentSource) ;
   s := KillString (s) ;
   currentUsed := TRUE
END addTokInteger ;


BEGIN
   init
END mcLexBuf.
