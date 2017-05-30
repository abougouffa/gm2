(* Copyright (C) 2017 Free Software Foundation, Inc.  *)
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
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

IMPLEMENTATION MODULE mcComment ;   (*!m2pim*)

FROM Indexing IMPORT Index, InitIndex, PutIndice, GetIndice, InBounds ;
FROM DynamicStrings IMPORT String, InitString, ConCat, RemoveWhitePrefix, Mark, KillString, InitStringCharStar, EqualCharStar, Length, Slice, string, char ;
FROM Storage IMPORT ALLOCATE ;
FROM nameKey IMPORT Name, keyToCharStar, lengthKey, NulName ;
FROM mcDebug IMPORT assert ;
FROM ASCII IMPORT nl ;

TYPE
   commentType = (unknown, procedureHeading, inBody, afterStatement) ;

   commentDescriptor = POINTER TO RECORD
                                     type    :  commentType ;
				     content :  String ;
				     procName:  Name ;
                                  END ;

VAR
   incomment     : BOOLEAN ;
   currentComment,
   maxComment    : CARDINAL ;
   comments      : Index ;


(*
   beginComment - the start of a new comment has been seen by the lexical analyser.
                  A new comment block is created and all addText contents are placed
                  in this block.
*)

PROCEDURE beginComment ;
VAR
   cd: commentDescriptor ;
BEGIN
   IF NOT incomment
   THEN
      NEW (cd) ;
      assert (cd # NIL) ;
      INC (maxComment) ;
      currentComment := maxComment ;
      WITH cd^ DO
         type := unknown ;
         content := InitString ('') ;
         procName := NulName
      END ;
      PutIndice (comments, maxComment, cd) ;
      incomment := TRUE
   END
END beginComment ;


(*
   endComment - the end of the comment has been seen by the lexical analyser.
*)

PROCEDURE endComment ;
BEGIN
   incomment := FALSE
END endComment ;


(*
   addText - cs is a C string (null terminated) which contains comment text.
             This is appended to the current comment.
*)

PROCEDURE addText (cs: ADDRESS) ;
VAR
   cd: commentDescriptor ;
BEGIN
   assert (incomment) ;
   cd := GetIndice (comments, maxComment) ;
   assert (cd # NIL) ;
   cd^.content := ConCat (cd^.content, InitStringCharStar (cs))
END addText ;


(*
   newPass - resets the comment count so that we can collect the comments in order again.
*)

PROCEDURE newPass ;
BEGIN
   currentComment := 0
END newPass ;


(*
   Min - returns the lower of, a, and, b.
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a < b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END Min ;


(*
   RemoveNewlines -
*)

PROCEDURE RemoveNewlines (s: String) : String ;
BEGIN
   WHILE Length (s) > 0 DO
      IF char (s, 0) = nl
      THEN
         s := RemoveWhitePrefix (Slice (s, 1, 0))
      ELSE
         RETURN RemoveWhitePrefix (s)
      END
   END ;
   RETURN s
END RemoveNewlines ;


(*
   seenProcedure - returns TRUE if the name, procName, appears as the first word
                   in the comment.
*)

PROCEDURE seenProcedure (cd: commentDescriptor; procName: Name) : BOOLEAN ;
VAR
   s   : String ;
   a   : ADDRESS ;
   i, h: CARDINAL ;
   res : BOOLEAN ;
BEGIN
   a := keyToCharStar (procName) ;
   s := RemoveNewlines (cd^.content) ;
   s := Slice (Mark (s), 0, Min (Length (s), lengthKey (procName))) ;
   res := EqualCharStar (s, a) ;
   s := KillString (s) ;
   RETURN res
END seenProcedure ;


(*
   setProcedureComment - changes the type of the current comment to a procedure heading comment,
                         providing it has the procname as the first word.
*)

PROCEDURE setProcedureComment (procname: Name) ;
VAR
   cd: commentDescriptor ;
BEGIN
   IF (currentComment > 0) AND (currentComment <= maxComment)
   THEN
      cd := GetIndice (comments, currentComment) ;
      assert (cd # NIL) ;
      IF seenProcedure (cd, procname)
      THEN
         cd^.type := procedureHeading ;
         cd^.procName := procname
      END
   END
END setProcedureComment ;


(*
   getComment - returns the current comment.
*)

PROCEDURE getComment () : String ;
VAR
   cd: commentDescriptor ;
BEGIN
   IF currentComment <= maxComment
   THEN
      cd := GetIndice (comments, currentComment) ;
      assert (cd # NIL) ;
      RETURN cd^.content
   END ;
   RETURN NIL
END getComment ;


(*
   getCommentCharStar - returns the current comment.
*)

PROCEDURE getCommentCharStar () : ADDRESS ;
VAR
   s: String ;
BEGIN
   s := getComment () ;
   IF s = NIL
   THEN
      RETURN NIL
   ELSE
      RETURN string (s)
   END
END getCommentCharStar ;


(*
   getProcedureComment - returns the current procedure comment if available.
*)

PROCEDURE getProcedureComment () : String ;
VAR
   cd: commentDescriptor ;
BEGIN
   IF InBounds (comments, currentComment)
   THEN
      cd := GetIndice (comments, currentComment) ;
      assert (cd # NIL) ;
      IF cd^.type = procedureHeading
      THEN
         RETURN cd^.content
      END
   END ;
   RETURN NIL
END getProcedureComment ;


(*
   init -
*)

PROCEDURE init ;
BEGIN
   incomment := FALSE ;
   maxComment := 0 ;
   currentComment := 0 ;
   comments := InitIndex (1)
END init ;


BEGIN
   init
END mcComment.
