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

IMPLEMENTATION MODULE M2Stack ;


FROM M2Error IMPORT InternalError ;

CONST
   MaxBucket = 10 ;

TYPE
   StackBucket     = POINTER TO Bucket ;
   Bucket          = RECORD
                         bucket: ARRAY [0..MaxBucket-1] OF WORD ;
                         items : CARDINAL ;
                         last  : StackBucket ;
                      END ;

   Stack           = POINTER TO StackDescriptor ;
   StackDescriptor = RECORD
                        tail: StackBucket ;
                     END ;


(*
   InitStack - creates and returns a new stack.
*)

PROCEDURE InitStack () : Stack ;
VAR
   s: Stack ;
BEGIN
   NEW(s) ;
   WITH s^ DO
      tail := NIL
   END ;
   RETURN( s )
END InitStack ;


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
   KillStack - destroys a stack, returning NIL.
*)

PROCEDURE KillStack (s: Stack) : Stack ;
BEGIN
   IF s#NIL
   THEN
      s^.tail := KillBucket(s^.tail) ;
      DISPOSE(s)
   END ;
   RETURN( NIL )
END KillStack ;


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
   Push - pushes a word, w, onto, s.
*)

PROCEDURE Push (s: Stack; w: WORD) ;
VAR
   t: Stack ;
BEGIN
   IF s=NIL
   THEN
      InternalError('stack has not been initialized', __FILE__, __LINE__)
   ELSE
      WITH s^ DO
         IF tail=NIL
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
END Push ;


(*
   Pop - pops an element from stack, s.
*)

PROCEDURE Pop (s: Stack) : WORD ;
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
END Pop ;


(*
   IsEmpty - returns TRUE if stack, s, is empty.
*)

PROCEDURE IsEmpty (s: Stack) : BOOLEAN ;
BEGIN
   RETURN( (s=NIL) OR (s^.tail=NIL) OR (s^.tail^.items=0) )
END IsEmpty ;


(*
   Peep - returns the element at, n, items below in the stack.
          Top of stack can be seen via Peep(s, 1)
*)

PROCEDURE Peep (s: Stack; n: CARDINAL) : WORD ;
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
      b := s^.tail
      WHILE n>1 DO
         IF b=NIL
         THEN
            InternalError('stack underflow', __FILE__, __LINE__)
         ELSIF b^.items>=n
         THEN
            RETURN( b^.bucket[n-1] )
         ELSE
            b := b^.last ;
            DEC(n, b^.items)
         END
      END ;
      InternalError('stack underflow', __FILE__, __LINE__)
   END
END Peep ;


(*
   Reduce - reduce the stack by n elements.
*)

PROCEDURE Reduce (s: Stack; n: CARDINAL) ;
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
            DEC(n, b^.items)
            s^.tail := s^.tail^.last ;
            DISPOSE(b)
         END
      END
   END
END Reduce ;


END M2Stack.
