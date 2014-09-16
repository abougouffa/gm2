(* Copyright (C) 2011 Free Software Foundation, Inc. *)
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
Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE history ;

FROM Storage IMPORT ALLOCATE ;
FROM roots IMPORT nearZero ;
FROM libc IMPORT printf ;

CONST
   Debugging = FALSE ;

TYPE
   hList = POINTER TO RECORD
                         id1,
                         id2        : CARDINAL ;
                         where1,
                         where2     : whereHit ;
                         hasOccurred: BOOLEAN ;
                         t          : REAL ;
                         next       : hList ;
                      END ;

VAR
   free,
   list: hList ;



(*
   dumpHlist - 
*)

PROCEDURE dumpHlist (l: hList) ;
BEGIN
   printf("time %g id pair (%d, %d)\n", l^.t, l^.id1, l^.id2)
END dumpHlist ;


(*
   dumpList - 
*)

PROCEDURE dumpList ;
VAR
   l: hList ;
BEGIN
   l := list ;
   printf("The history of collisions found:\n") ;
   WHILE l#NIL DO
      dumpHlist(l) ;
      l := l^.next
   END
END dumpList ;


(*
   newHList - returns a new hList.
*)

PROCEDURE newHList () : hList ;
VAR
   h: hList ;
BEGIN
   IF free=NIL
   THEN
      NEW(h)
   ELSE
      h := free ;
      free := free^.next
   END ;
   RETURN h
END newHList ;


(*
   disposeHList - returns, h, to the free list.
*)

PROCEDURE disposeHList (h: hList) ;
BEGIN
   h^.next := free ;
   free := h
END disposeHList ;


(*
   isPair - are (a, b) the same as (x, y) or
             is (a, b) the same as (y, x)
*)

PROCEDURE isPair (a, b, x, y: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN ((a=x) AND (b=y)) OR
          ((a=y) AND (b=x))
END isPair ;


(*
   isSame - do, a, and, b, reference the same collision?  Note we do not use the contact
            point of collision as polygon/polygon collisions might hit on the corner or
            edge.  Instead we assume if we know the time, polygon, face this is good enough.
            twoDsim will test for multiple points on a line and we need to identify
            duplicates per polygon and line.
*)

PROCEDURE isSame (a, b: hList) : BOOLEAN ;
BEGIN
   RETURN nearZero(a^.t-b^.t) AND
           isPair(a^.id1, a^.id2, b^.id1, b^.id2) AND
           (a^.where1=b^.where1) AND (a^.where2=b^.where2)
END isSame ;


(*
   disposeAll - return NIL.  Place complete contents of, l, onto the free list.
*)

PROCEDURE disposeAll (l: hList) : hList ;
VAR
   a: hList ;
BEGIN
   WHILE l#NIL DO
      a := l ;
      l := l^.next ;
      disposeHList(a)
   END ;
   RETURN NIL
END disposeAll ;


(*
   updateList - 
*)

PROCEDURE updateList (n: hList; currentTime: REAL) ;
VAR
   h, p: hList ;
BEGIN
   IF list=NIL
   THEN
      list := n
   ELSE
      (* ok at this point we know, n, is a different collision. *)
      (* we can destroy any hList entry which is significantly older than currentTime. *)
      p := NIL ;
      h := list ;
      WHILE h#NIL DO
         IF (NOT nearZero(h^.t-currentTime)) AND (currentTime > h^.t)
         THEN
            (* safely older, delete entry *)
            IF p=NIL
            THEN
               list := h^.next ;
               h := list
            ELSE
               p^.next := h^.next ;
               disposeHList(h) ;
               h := p^.next
            END
         ELSE
            p := h ;
            h := h^.next
         END
      END ;
      (* add it to the list *)
      n^.next := list ;
      list := n
   END
END updateList ;


(*
   init - fill in the fields of, n, and return n.
*)

PROCEDURE init (n: hList; time: REAL; id1, id2: CARDINAL; w1, w2: whereHit) : hList ;
BEGIN
   n^.id1 := id1 ;
   n^.id2 := id2 ;
   n^.where1 := w1 ;
   n^.where2 := w2 ;
   n^.t := time ;
   n^.next := NIL ;
   n^.hasOccurred := FALSE ;
   RETURN n
END init ;


(*
   isDuplicate - returns TRUE if the collision at, position,
                 and, time, has occurred before.
                 The time must be the absolute time of the collision.
*)

PROCEDURE isDuplicate (currentTime, relTime: REAL;
                       id1, id2: CARDINAL; w1, w2: whereHit) : BOOLEAN ;
VAR
   h, n: hList ;
BEGIN
   IF Debugging
   THEN
      dumpList
   END ;
   n := init(newHList(), currentTime+relTime, id1, id2, w1, w2) ;
   IF Debugging
   THEN
      printf("checking against: ") ;
      dumpHlist(n)
   END ;
   h := list ;
   WHILE h#NIL DO
      IF isSame(h, n)
      THEN
         disposeHList(n) ;
         IF Debugging
         THEN
            printf("found same collision\n")
         END ;
         RETURN TRUE
      END ;
      h := h^.next
   END ;
   updateList(n, currentTime) ;
   IF Debugging
   THEN
      printf("unique collision found\n") ;
      dumpHlist(n)
   END ;
   RETURN FALSE
END isDuplicate ;


(*
   removeOlderHistory - remove older entries than, currentTime.
                        This should only be called once a collision
                        has been found, we need to do this as we assume
                        any future event may be null and void after an
                        earlier collision.
*)

PROCEDURE removeOlderHistory (currentTime: REAL) ;
VAR
   h, p: hList ;
BEGIN
   IF Debugging
   THEN
      printf("truncating the history list\n")
   END ;
   p := NIL ;
   h := list ;
   WHILE h#NIL DO
      IF (NOT nearZero(h^.t-currentTime)) AND (currentTime < h^.t)
      THEN
         (* safely older, delete entry *)
         IF p=NIL
         THEN
            list := h^.next ;
            disposeHList(h) ;
            h := list
         ELSE
            p^.next := h^.next ;
            disposeHList(h) ;
            h := p^.next
         END
      ELSE
         p := h ;
         h := h^.next
      END
   END ;
   IF Debugging
   THEN
      dumpList
   END
END removeOlderHistory ;


(*
   forgetHistory - forget the complete history list.
*)

PROCEDURE forgetHistory ;
BEGIN
   list := disposeAll(list)
END forgetHistory ;


(*
   occurred - mark the collision as having occurred.
*)

PROCEDURE occurred (currentTime: REAL; id1, id2: CARDINAL) ;
VAR
   h, n: hList ;
BEGIN
   IF Debugging
   THEN
      dumpList
   END ;
   n := init(newHList(), currentTime, id1, id2, edge, edge) ;
   IF Debugging
   THEN
      printf("checking against: ") ;
      dumpHlist(n)
   END ;
   h := list ;
   WHILE h#NIL DO
      IF isSame(h, n)
      THEN
         h^.hasOccurred := TRUE
      END ;
      h := h^.next
   END ;
   disposeHList(n) ;
   IF Debugging
   THEN
      dumpList
   END
END occurred ;


(*
   purge - remove all collision history which has never occurred.
*)

PROCEDURE purge ;
VAR
   h, p: hList ;
BEGIN
   IF Debugging
   THEN
      printf("purging the history list\n")
   END ;
   p := NIL ;
   h := list ;
   WHILE h#NIL DO
      IF NOT h^.hasOccurred
      THEN
         (* delete entry *)
         IF p=NIL
         THEN
            list := h^.next ;
            disposeHList(h) ;
            h := list
         ELSE
            p^.next := h^.next ;
            disposeHList(h) ;
            h := p^.next
         END
      ELSE
         p := h ;
         h := h^.next
      END
   END ;
   IF Debugging
   THEN
      dumpList
   END
END purge ;


BEGIN
   list := NIL ;
   free := NIL
END history.
