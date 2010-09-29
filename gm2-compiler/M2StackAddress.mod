(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE M2StackAddress ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Error IMPORT InternalError ;
FROM M2Debug IMPORT Assert ;

CONST
   MaxBucket = 10 ;

TYPE
   StackBucket     = POINTER TO Bucket ;
   Bucket          = RECORD
                         bucket: ARRAY [0..MaxBucket-1] OF ADDRESS ;
                         items : CARDINAL ;
                         last  : StackBucket ;
                      END ;

   StackOfAddress    = POINTER TO StackDescriptor ;
   StackDescriptor = RECORD
                        tail: StackBucket ;
                     END ;


(*
   InitStackAddress - creates and returns a new stack.
*)

PROCEDURE InitStackAddress () : StackOfAddress ;
VAR
   s: StackOfAddress ;
BEGIN
   NEW(s) ;
   WITH s^ DO
      tail := NIL
   END ;
   RETURN( s )
END InitStackAddress ;


(*
   KillBucket - destroys a StackBucket and returns, NIL.
*)

PROCEDURE KillBucket (b: StackBucket) : StackBucket ;
BEGIN
   IF b#NIL
   THEN
      b := KillBucket(b^.last) ;
      DISPOSE(b)
   END ;
   RETURN( NIL )
END KillBucket ;


(*
   KillStackAddress - destroys a stack, returning NIL.
*)

PROCEDURE KillStackAddress (s: StackOfAddress) : StackOfAddress ;
BEGIN
   IF s#NIL
   THEN
      s^.tail := KillBucket(s^.tail) ;
      DISPOSE(s)
   END ;
   RETURN( NIL )
END KillStackAddress ;


(*
   InitBucket - returns an empty StackBucket.
*)

PROCEDURE InitBucket (l: StackBucket) : StackBucket ;
VAR
   b: StackBucket ;
BEGIN
   NEW(b) ;
   WITH b^ DO
      items := 0 ;
      last  := l
   END ;
   RETURN( b )
END InitBucket ;


(*
   PushAddress - pushes a word, w, onto, s.
*)

PROCEDURE PushAddress (s: StackOfAddress; w: ADDRESS) ;
VAR
   t: StackOfAddress ;
BEGIN
   IF s=NIL
   THEN
      InternalError('stack has not been initialized', __FILE__, __LINE__)
   ELSE
      WITH s^ DO
         IF (tail=NIL) OR (tail^.items=MaxBucket)
         THEN
            tail := InitBucket(tail)
         END ;
         WITH tail^ DO
            IF items<MaxBucket
            THEN
               bucket[items] := w ;
               INC(items)
            END
         END
      END
   END
END PushAddress ;


(*
   PopAddress - pops an element from stack, s.
*)

PROCEDURE PopAddress (s: StackOfAddress) : ADDRESS ;
VAR
   b: StackBucket ;
BEGIN
   IF s=NIL
   THEN
      InternalError('stack has not been initialized', __FILE__, __LINE__)
   ELSE
      IF s^.tail=NIL
      THEN
         InternalError('stack underflow', __FILE__, __LINE__)
      ELSE
         IF s^.tail^.items=0
         THEN
            b := s^.tail ;
            IF b=NIL
            THEN
               InternalError('stack underflow', __FILE__, __LINE__)
            ELSE
               s^.tail := b^.last
            END ;
            DISPOSE(b)
         END ;
         WITH s^.tail^ DO
            DEC(items) ;
            RETURN( bucket[items] )
         END
      END
   END
END PopAddress ;


(*
   IsEmptyAddress - returns TRUE if stack, s, is empty.
*)

PROCEDURE IsEmptyAddress (s: StackOfAddress) : BOOLEAN ;
BEGIN
   RETURN( (s=NIL) OR (s^.tail=NIL) OR (s^.tail^.items=0) )
END IsEmptyAddress ;


(*
   PeepAddress - returns the element at, n, items below in the stack.
                 Top of stack can be seen via Peep(s, 1)
*)

PROCEDURE PeepAddress (s: StackOfAddress; n: CARDINAL) : ADDRESS ;
VAR
   b: StackBucket ;
BEGIN
   IF s^.tail=NIL
   THEN
      InternalError('stack underflow', __FILE__, __LINE__)
   ELSE
      IF s^.tail^.items=0
      THEN
         b := s^.tail ;
         IF b=NIL
         THEN
            InternalError('stack underflow', __FILE__, __LINE__)
         ELSE
            s^.tail := b^.last
         END ;
         DISPOSE(b)
      END ;
      b := s^.tail ;
      WHILE n>=1 DO
         IF b=NIL
         THEN
            InternalError('stack underflow', __FILE__, __LINE__)
         ELSIF b^.items>=n
         THEN
            RETURN( b^.bucket[b^.items-n] )
         ELSE
            Assert(b^.items<n) ;
            DEC(n, b^.items) ;
            b := b^.last
         END
      END ;
      InternalError('stack underflow', __FILE__, __LINE__)
   END
END PeepAddress ;


(*
   ReduceAddress - reduce the stack by n elements.
*)

PROCEDURE ReduceAddress (s: StackOfAddress; n: CARDINAL) ;
VAR
   b: StackBucket ;
BEGIN
   IF s^.tail=NIL
   THEN
      InternalError('stack underflow', __FILE__, __LINE__)
   ELSE
      IF s^.tail^.items=0
      THEN
         b := s^.tail ;
         IF b=NIL
         THEN
            InternalError('stack underflow', __FILE__, __LINE__)
         ELSE
            s^.tail := b^.last
         END ;
         DISPOSE(b)
      END ;
      LOOP
         IF s^.tail=NIL
         THEN
            InternalError('stack underflow', __FILE__, __LINE__)
         ELSIF s^.tail^.items>=n
         THEN
            DEC( s^.tail^.items, n) ;
            RETURN  (* all done exit *)
         ELSE
            b := s^.tail ;
            DEC(n, b^.items) ;
            s^.tail := s^.tail^.last ;
            DISPOSE(b)
         END
      END
   END
END ReduceAddress ;


(*
   NoOfItemsInStackAddress - returns the number of items held in the stack, s.
*)

PROCEDURE NoOfItemsInStackAddress (s: StackOfAddress) : CARDINAL ;
VAR
   b: StackBucket ;
   n: CARDINAL ;
BEGIN
   IF IsEmptyAddress(s)
   THEN
      RETURN( 0 )
   ELSE
      n := 0 ;
      b := s^.tail ;
      WHILE b#NIL DO
         INC(n, b^.items) ;
         b := b^.last
      END ;
      RETURN( n )
   END
END NoOfItemsInStackAddress ;


END M2StackAddress.
