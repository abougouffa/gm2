(* Copyright (C) 2015,
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

IMPLEMENTATION MODULE wlists ;


FROM Storage IMPORT ALLOCATE, DEALLOCATE ;

CONST
   MaxnoOfelements = 5 ;

TYPE
   wlist = POINTER TO RECORD
             noOfelements: CARDINAL ;
             elements    : ARRAY [1..MaxnoOfelements] OF WORD ;
             next        : wlist ;
          END ;


(*
   initList - creates a new wlist, l.
*)

PROCEDURE initList () : wlist ;
VAR
   l: wlist ;
BEGIN
   NEW (l) ;
   WITH l^ DO
      noOfelements := 0 ;
      next := NIL
   END ;
   RETURN l
END initList ;


(*
   killList - deletes the complete wlist, l.
*)

PROCEDURE killList (VAR l: wlist) ;
BEGIN
   IF l#NIL
   THEN
      IF l^.next#NIL
      THEN
         killList (l^.next)
      END ;
      DISPOSE (l)
   END
END killList ;


(*
   putItemIntoList - places an WORD, c, into wlist, l.
*)

PROCEDURE putItemIntoList (l: wlist; c: WORD) ;
BEGIN
   WITH l^ DO
      IF noOfelements<MaxnoOfelements
      THEN
         INC (noOfelements) ;
         elements[noOfelements] := c
      ELSIF next#NIL
      THEN
         putItemIntoList (next, c)
      ELSE
         next := initList () ;
         putItemIntoList (next, c)
      END
   END
END putItemIntoList ;


(*
   getItemFromList - retrieves the nth WORD from wlist, l.
*)

PROCEDURE getItemFromList (l: wlist; n: CARDINAL) : WORD ;
BEGIN
   WHILE l#NIL DO
      WITH l^ DO
         IF n<=noOfelements
         THEN
            RETURN elements[n]
         ELSE
            DEC (n, noOfelements)
         END
      END ;
      l := l^.next
   END ;
   RETURN 0
END getItemFromList ;


(*
   getIndexOfList - returns the index for WORD, c, in wlist, l.
                    If more than one WORD, c, exists the index
                    for the first is returned.
*)

PROCEDURE getIndexOfList (l: wlist; c: WORD) : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   IF l=NIL
   THEN
      RETURN 0
   ELSE
      WITH l^ DO
         i := 1 ;
         WHILE i<=noOfelements DO
            IF elements[i]=c
            THEN
               RETURN i
            ELSE
               INC(i)
            END
         END ;
         RETURN noOfelements + getIndexOfList (next, c)
      END
   END
END getIndexOfList ;


(*
   noOfItemsInList - returns the number of items in wlist, l.
*)

PROCEDURE noOfItemsInList (l: wlist) : CARDINAL ;
VAR
   t: CARDINAL ;
BEGIN
   IF l=NIL
   THEN
      RETURN 0
   ELSE
      t := 0 ;
      REPEAT
         WITH l^ DO
            INC (t, noOfelements)
         END ;
         l := l^.next
      UNTIL l=NIL;
      RETURN t
   END
END noOfItemsInList ;


(*
   includeItemIntoList - adds an WORD, c, into a wlist providing
                         the value does not already exist.
*)

PROCEDURE includeItemIntoList (l: wlist; c: WORD) ;
BEGIN
   IF NOT isItemInList (l, c)
   THEN
      putItemIntoList (l, c)
   END
END includeItemIntoList ;


(*
   removeItem - remove an element at index, i, from the wlist data type.
*)

PROCEDURE removeItem (p, l: wlist; i: CARDINAL) ;
BEGIN
   WITH l^ DO
      DEC (noOfelements) ;
      WHILE i<=noOfelements DO
         elements[i] := elements[i+1] ;
         INC (i)
      END ;
      IF (noOfelements=0) AND (p#NIL)
      THEN
         p^.next := l^.next ;
         DISPOSE (l)
      END
   END
END removeItem ;


(*
   removeItemFromList - removes a WORD, c, from a wlist.
                        It assumes that this value only appears once.
*)

PROCEDURE removeItemFromList (l: wlist; c: WORD) ;
VAR
   p    : wlist ;
   i    : CARDINAL ;
   found: BOOLEAN ;
BEGIN
   IF l#NIL
   THEN
      found := FALSE ;
      p := NIL ;
      REPEAT
         WITH l^ DO
            i := 1 ;
            WHILE (i<=noOfelements) AND (elements[i]#c) DO
               INC (i)
            END ;
         END ;
         IF (i<=l^.noOfelements) AND (l^.elements[i]=c)
         THEN
            found := TRUE
         ELSE
            p := l ;
            l := l^.next
         END
      UNTIL (l=NIL) OR found ;
      IF found
      THEN
         removeItem (p, l, i)
      END
   END
END removeItemFromList ;


(*
   isItemInList - returns true if a WORD, c, was found in wlist, l.
*)

PROCEDURE isItemInList (l: wlist; c: WORD) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   REPEAT
      WITH l^ DO
         i := 1 ;
         WHILE i<=noOfelements DO
            IF elements[i]=c
            THEN
               RETURN TRUE
            ELSE
               INC (i)
            END
         END
      END ;
      l := l^.next
   UNTIL l=NIL ;
   RETURN FALSE
END isItemInList ;


(*
   foreachItemInListDo - calls procedure, P, foreach item in wlist, l.
*)

PROCEDURE foreachItemInListDo (l: wlist; p: performOperation) ;
VAR
   i, n: CARDINAL ;
BEGIN
   n := noOfItemsInList(l) ;
   i := 1 ;
   WHILE i<=n DO
      p (getItemFromList (l, i)) ;
      INC(i)
   END
END foreachItemInListDo ;


(*
   duplicateList - returns a duplicate wlist derived from, l.
*)

PROCEDURE duplicateList (l: wlist) : wlist ;
VAR
   m   : wlist ;
   n, i: CARDINAL ;
BEGIN
   m := initList () ;
   n := noOfItemsInList (l) ;
   i := 1 ;
   WHILE i<=n DO
      putItemIntoList (m, getItemFromList (l, i)) ;
      INC (i)
   END ;
   RETURN m
END duplicateList ;


END wlists.
