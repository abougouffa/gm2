(* Copyright (C) 2008 Free Software Foundation, Inc. *)
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

MODULE except ;

FROM libc IMPORT printf ;
FROM libc IMPORT longjmp, setjmp ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM SYSTEM IMPORT ADR, WORD ;

TYPE
   jmp_env    = ARRAY [0..100] OF WORD ;
   jmp_status = (jmp_normal, jmp_retry, jmp_exception) ;

   setjmp_stack = POINTER TO RECORD
                                env : jmp_env ;
                                next: setjmp_stack ;
                             END ;

VAR
   head: setjmp_stack ;


PROCEDURE pushsetjmp ;
VAR
   p: setjmp_stack ;
BEGIN
   NEW(p) ;
   WITH p^ DO
      next := head
   END ;
   head := p
END pushsetjmp ;

PROCEDURE exception ;
VAR
   r: INTEGER ;
BEGIN
   r := printf("invoking exception handler\n") ;
   longjmp(ADR(head^.env), VAL(INTEGER, jmp_exception))
END exception ;

PROCEDURE retry ;
VAR
   r: INTEGER ;
BEGIN
   r := printf("retry\n") ;
   longjmp (ADR(head^.env), VAL(INTEGER, jmp_retry))
END retry ;

PROCEDURE popsetjmp ;
VAR
   p: setjmp_stack ;
BEGIN
   p := head;
   head := head^.next ;
   DISPOSE(p)
END popsetjmp ;


PROCEDURE fly ;
VAR
   r: INTEGER ;
BEGIN
   r := printf("fly main body\n") ;
   IF ip=NIL
   THEN
      r := printf("ip = NIL\n") ;
      exception
   END ;
   IF ip^=0
   THEN
      r := printf("^ip = 0\n") ;
      exception
   END ;
   IF 4 DIV ip^ = 4
   THEN
      r := printf("yes it worked\n")
   ELSE
      r := printf("no it failed\n")
   END
END fly ;

(*
 *   a GNU M2 version of the Modula-2 example given in the ISO standard.
 *   This is a hand translation of the equivalent except.c file in this
 *   directory which is written to prove that the underlying runtime system
 *   will work with the GCC builtin longjmp/set interpretation.
 *)

PROCEDURE tryFlying ;
   PROCEDURE tryFlying_m2_exception ;
   VAR
      r: INTEGER ;
   BEGIN
      r := printf("inside tryFlying exception routine\n") ;
      IF (ip#NIL) AND (ip^=0)
      THEN
         ip^ := 1 ;
         retry
      END
   END tryFlying_m2_exception ;

VAR
   t, r: INTEGER ;
BEGIN
   pushsetjmp ;
   REPEAT
      t := setjmp (ADR(head^.env))
   UNTIL VAL(jmp_status, t)#jmp_retry ;

   IF VAL(jmp_status, t)=jmp_exception
   THEN
      (* exception called *)
      tryFlying_m2_exception ;
      (* exception has not been handled, invoke previous handler *)
      r := printf("exception not handled here\n");
      popsetjmp();
      exception();
   END ;

   r := printf("tryFlying main body\n");  
   fly ;
   popsetjmp
END tryFlying ;


PROCEDURE keepFlying ;

  PROCEDURE keepFlying_m2_exception ;
  VAR
     r: INTEGER ;
  BEGIN
     r := printf("inside keepFlying exception routine\n") ;
     IF ip=NIL
     THEN
        NEW(ip) ;
        ip^ := 0 ;
        retry
     END
  END keepFlying_m2_exception ;

VAR
   t: INTEGER ;
BEGIN
   pushsetjmp ;
   REPEAT
      t := setjmp (ADR(head^.env)) ;
   UNTIL VAL(jmp_status, t)#jmp_retry ;
  
   IF VAL(jmp_status, t)=jmp_exception
   THEN
      (* exception called *)
      keepFlying_m2_exception ;
      (* exception has not been handled, invoke previous handler *)
      popsetjmp ;
      exception
   END ;
   r := printf("keepFlying main body\n") ;
   tryFlying ;
   popsetjmp
END keepFlying ;


VAR
   r : INTEGER ;
   ip: POINTER TO INTEGER ;
BEGIN
   head := NIL ;
   keepFlying ;
   r := printf("all done\n")
END except.
