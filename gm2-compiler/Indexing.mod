(* Copyright (C) 2003 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE Indexing ;

FROM libc IMPORT memset ;
FROM Storage IMPORT ALLOCATE, REALLOCATE, DEALLOCATE ;
FROM SYSTEM IMPORT TSIZE, ADDRESS, WORD, BITSET ;

CONST
   MinSize = 128 ;

TYPE
   PtrToAddress = POINTER TO ADDRESS ;

   Index = POINTER TO RECORD
                         ArrayStart: ADDRESS ;
                         ArraySize : CARDINAL ;
                         Low,
                         High      : CARDINAL ;
                         Debug     : BOOLEAN ;
                         Map       : BITSET ;
                      END ;

(*
   InitIndex - creates and returns an Index.
*)

PROCEDURE InitIndex (low: CARDINAL) : Index ;
VAR
   i: Index ;
BEGIN
   NEW(i) ;
   WITH i^ DO
      Low := low ;
      High := 0 ;
      ArraySize := MinSize ;
      ALLOCATE(ArrayStart, MinSize) ;
      ArrayStart := memset(ArrayStart, 0, ArraySize) ;
      Debug := FALSE ;
      Map := BITSET{}
   END ;
   RETURN( i )
END InitIndex ;


(*
   KillIndex - returns Index to free storage.
*)

PROCEDURE KillIndex (i: Index) : Index ;
BEGIN
   WITH i^ DO
      DEALLOCATE(ArrayStart, ArraySize)
   END ;
   DISPOSE(i) ;
   RETURN( NIL )
END KillIndex ;


(*
   DebugIndex - turns on debugging within an index.
*)

PROCEDURE DebugIndex (i: Index) : Index ;
BEGIN
   i^.Debug := TRUE ;
   RETURN( i )
END DebugIndex ;


(*
   InBounds - returns TRUE if indice, n, is within the bounds
              of the dynamic array.
*)

PROCEDURE InBounds (i: Index; n: CARDINAL) : BOOLEAN ;
BEGIN
   IF i=NIL
   THEN
      HALT
   ELSE
      WITH i^ DO
         RETURN( (n>=Low) AND (n<=High) )
      END
   END
END InBounds ;


(*
   HighIndice - returns the last legally accessible indice of this array.
*)

PROCEDURE HighIndice (i: Index) : CARDINAL ;
BEGIN
   IF i=NIL
   THEN
      HALT
   ELSE
      RETURN( i^.High )
   END
END HighIndice ;


(*
   LowIndice - returns the first legally accessible indice of this array.
*)

PROCEDURE LowIndice (i: Index) : CARDINAL ;
BEGIN
   IF i=NIL
   THEN
      HALT
   ELSE
      RETURN( i^.Low )
   END
END LowIndice ;


(*
   PutIndice - places, a, into the dynamic array at position i[n]
*)

PROCEDURE PutIndice (i: Index; n: CARDINAL; a: ADDRESS) ;
VAR
   oldSize: CARDINAL ;
   b      : ADDRESS ;
   p      : POINTER TO POINTER TO WORD ;
BEGIN
   WITH i^ DO
      IF NOT InBounds(i, n)
      THEN
         IF n<Low
         THEN
            HALT
         ELSE
            oldSize := ArraySize ;
            WHILE (n-Low)*TSIZE(ADDRESS)>=ArraySize DO
               ArraySize := ArraySize * 2
            END ;
            IF oldSize#ArraySize
            THEN
(*
               IF Debug
               THEN
                  printf2('increasing memory hunk from %d to %d\n',
                          oldSize, ArraySize)
               END ;
*)
               REALLOCATE(ArrayStart, ArraySize) ;
               (* and initialize the remainder of the array to NIL *)
               b := memset(ArrayStart+oldSize, 0, ArraySize-oldSize)
            END ;
            High := n
         END
      END ;
      p := ArrayStart + ((n-Low)*TSIZE(ADDRESS)) ;
      p^ := a ;
      IF Debug
      THEN
         IF n<32
         THEN
            INCL(Map, n)
         END
      END
   END
END PutIndice ;


(*
   GetIndice - retrieves, element i[n] from the dynamic array.
*)

PROCEDURE GetIndice (i: Index; n: CARDINAL) : ADDRESS ;
VAR
   p: PtrToAddress ;
BEGIN
   WITH i^ DO
      IF NOT InBounds(i, n)
      THEN
         HALT
      END ;
      p := ArrayStart + ((n-Low)*TSIZE(ADDRESS)) ;
      IF Debug
      THEN
         IF (n<32) AND (NOT (n IN Map)) AND (p^#NIL)
         THEN
            HALT
         END
      END ;
      RETURN( p^ )
   END
END GetIndice ;


(*
   IsIndiceInIndex - returns TRUE if, a, is in the index, i.
*)

PROCEDURE IsIndiceInIndex (i: Index; a: ADDRESS) : BOOLEAN ;
VAR
   j: CARDINAL ;
   p: PtrToAddress ;
BEGIN
   WITH i^ DO
      j := Low ;
      p := ArrayStart ;
      WHILE j<=High DO
         IF p^=a
         THEN
            RETURN( TRUE )
         END ;
         INC(p, TSIZE(ADDRESS)) ;
         INC(j)
      END
   END ;
   RETURN( FALSE )
END IsIndiceInIndex ;


(*
   RemoveIndiceFromIndex - removes, a, from Index, i.
*)

PROCEDURE RemoveIndiceFromIndex (i: Index; a: ADDRESS) ;
VAR
   j, k: CARDINAL ;
   p, q, r: PtrToAddress ;
BEGIN
   WITH i^ DO
      j := Low ;
      p := ArrayStart ;
      WHILE j<=High DO
         IF p^=a
         THEN
            q := p ;
            r := p ;
            INC(q, TSIZE(ADDRESS)) ;
            k := j+1 ;
            WHILE k<=High DO
               r^ := q^ ;
               INC(r, TSIZE(ADDRESS)) ;
               INC(q, TSIZE(ADDRESS)) ;
               INC(k)
            END ;
            DEC(High)
         END ;
         INC(p, TSIZE(ADDRESS)) ;
         INC(j)
      END
   END
END RemoveIndiceFromIndex ;


(*
   IncludeIndiceIntoIndex - if the indice is not in the index, then
                            add it at the end.
*)

PROCEDURE IncludeIndiceIntoIndex (i: Index; a: ADDRESS) ;
BEGIN
   IF NOT IsIndiceInIndex(i, a)
   THEN
      PutIndice(i, HighIndice(i)+1, a)
   END
END IncludeIndiceIntoIndex ;


(*
   ForeachIndiceInIndexDo - for each j indice of i, call procedure p(i[j])
*)

PROCEDURE ForeachIndiceInIndexDo (i: Index; p: IndexProcedure) ;
VAR
   j: CARDINAL ;
BEGIN
   j := LowIndice(i) ;
   WHILE j<=HighIndice(i) DO
      p(GetIndice(i, j)) ;
      INC(j)
   END
END ForeachIndiceInIndexDo ;


END Indexing.
