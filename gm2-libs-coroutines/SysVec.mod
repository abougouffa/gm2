(* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007
   Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA *)

IMPLEMENTATION MODULE SysVec ;


FROM M2RTS IMPORT Halt ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM pth IMPORT pth_select ;
FROM SYSTEM IMPORT PRIORITY ;
FROM libc IMPORT printf, perror ;
FROM Assertion IMPORT Assert ;

FROM Selective IMPORT InitSet, FdSet, Timeval, InitTime, KillTime, KillSet,
                      SetOfFd, FdIsSet, GetTime, FdZero, GetTimeOfDay, SetTime ;

CONST
   Microseconds = 1000000 ;
   DebugTime    = 0 ;
   Debugging    = FALSE ;

TYPE
   VectorType = (input, output, time) ;
   Vector     = POINTER TO RECORD
                              type    : VectorType ;
                              priority: CARDINAL ;
                              arg     : ADDRESS ;
                              pending,
                              exists  : Vector ;
                              no      : CARDINAL ;
                              File    : INTEGER ;
                              rel,
                              abs     : Timeval ;
                              queued  : BOOLEAN ;
                              
(*  BUG IN GM2
                              CASE type OF

                              time  : rel,
                                      abs   : Timeval ;
                                      queued: BOOLEAN ;
                                      Secs  : CARDINAL |
                              input,
                              output: File  : INTEGER

                              END
*)
                           END ;

VAR
   VecNo  : CARDINAL ;
   Exists : Vector ;
   Pending: ARRAY [MIN(PRIORITY)..MAX(PRIORITY)] OF Vector ;


(*
   Max - returns the maximum: i or j.
*)

PROCEDURE Max (i, j: INTEGER) : INTEGER ;
BEGIN
   IF i>j
   THEN
      RETURN( i )
   ELSE
      RETURN( j )
   END
END Max ;


(*
   Max - returns the minimum: i or j.
*)

PROCEDURE Min (i, j: INTEGER) : INTEGER ;
BEGIN
   IF i<j
   THEN
      RETURN( i )
   ELSE
      RETURN( j )
   END
END Min ;


(*
   FindVector - searches the exists list for a vector of type, t,
                which is associated with file descriptor, fd.
*)

PROCEDURE FindVector (fd: INTEGER; t: VectorType) : Vector ;
VAR
   v: Vector ;
BEGIN
   v := Exists ;
   WHILE v#NIL DO
      IF (v^.type=t) AND (v^.File=fd)
      THEN
         RETURN( v )
      END ;
      v := v^.exists
   END ;
   RETURN( NIL )
END FindVector ;


(*
   InitInputVector - returns an interrupt vector which is associated
                     with the file descriptor, fd.
*)

PROCEDURE InitInputVector (fd: INTEGER; pri: CARDINAL) : CARDINAL ;
VAR
   v: Vector ;
   r: INTEGER ;
BEGIN
   IF Debugging
   THEN
      r := printf("InitInputVector fd = %d priority = %d\n", fd, pri)
   END ;
   v := FindVector(fd, input) ;
   IF v=NIL
   THEN
      NEW(v) ;
      INC(VecNo) ;
      WITH v^ DO
         type     := input ;
         priority := pri ;
         arg      := NIL ;
         pending  := NIL ;
         exists   := Exists ;
         no       := VecNo ;
         File     := fd
      END ;
      Exists := v ;
      RETURN( VecNo )
   ELSE
      RETURN( v^.no )
   END
END InitInputVector ;


(*
   InitOutputVector - returns an interrupt vector which is associated
                      with the file descriptor, fd.
*)

PROCEDURE InitOutputVector (fd: INTEGER; pri: CARDINAL) : CARDINAL ;
VAR
   v: Vector ;
BEGIN
   v := FindVector(fd, output) ;
   IF v=NIL
   THEN
      NEW(v) ;
      INC(VecNo) ;
      WITH v^ DO
         type     := output ;
         priority := pri ;
         arg      := NIL ;
         pending  := NIL ;
         exists   := Exists ;
         no       := VecNo ;
         File     := fd
      END ;
      Exists := v ;
      RETURN( VecNo )
   ELSE
      RETURN( v^.no )
   END
END InitOutputVector ;


(*
   InitTimeVector - returns an interrupt vector associated with
                    the relative time.
*)

PROCEDURE InitTimeVector (micro, secs: CARDINAL; pri: CARDINAL) : CARDINAL ;
VAR
   v: Vector ;
BEGIN
   NEW(v) ;
   INC(VecNo) ;
   Assert(micro<Microseconds) ;
   WITH v^ DO
      type     := time ;
      priority := pri ;
      arg      := NIL ;
      pending  := NIL ;
      exists   := Exists ;
      no       := VecNo ;
      rel      := InitTime(secs+DebugTime, micro) ;
      abs      := InitTime(0, 0) ;
      queued   := FALSE
   END ;
   Exists := v ;
   RETURN( VecNo )
END InitTimeVector ;


(*
   FindVectorNo - searches the Exists list for vector, vec.
*)

PROCEDURE FindVectorNo (vec: CARDINAL) : Vector ;
VAR
   v: Vector ;
BEGIN
   v := Exists ;
   WHILE (v#NIL) AND (v^.no#vec) DO
      v := v^.exists
   END ;
   RETURN( v )
END FindVectorNo ;


(*
   FindPendingVector - searches the pending list for vector, vec.
*)

PROCEDURE FindPendingVector (vec: CARDINAL) : Vector ;
VAR
   i: CARDINAL ;
   v: Vector ;
BEGIN
   FOR i := MIN(PRIORITY) TO MAX(PRIORITY) DO
      v := Pending[i] ;
      WHILE (v#NIL) AND (v^.no#vec) DO
         v := v^.pending
      END ;
      IF (v#NIL) AND (v^.no=vec)
      THEN
         RETURN( v )
      END
   END ;
   RETURN( NIL )
END FindPendingVector ;


(*
   ReArmTimeVector - reprimes the vector, vec, to deliver an interrupt
                     at the new relative time.
*)

PROCEDURE ReArmTimeVector (vec: CARDINAL;
                           micro, secs: CARDINAL) ;
VAR
   v: Vector ;
BEGIN
   Assert(micro<Microseconds) ;
   v := FindVectorNo(vec) ;
   IF v=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'cannot find vector supplied')
   ELSE
      WITH v^ DO
         SetTime(rel, secs+DebugTime, micro)
      END
   END
END ReArmTimeVector ;


(*
   GetTimeVector - assigns, micro, and, secs, with the remaining
                   time before this interrupt will expire.
                   This value is only updated when a Listen
                   occurs.
*)

PROCEDURE GetTimeVector (vec: CARDINAL; VAR micro, secs: CARDINAL) ;
VAR
   v: Vector ;
BEGIN
   v := FindVectorNo(vec) ;
   IF v=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'cannot find vector supplied')
   ELSE
      WITH v^ DO
         GetTime(rel, secs, micro) ;
         Assert(micro<Microseconds)
      END
   END
END GetTimeVector ;


(*
   AttachVector - adds the pointer, p, to be associated with the interrupt
                  vector. It returns the previous value attached to this
                  vector.
*)

PROCEDURE AttachVector (vec: CARDINAL; p: ADDRESS) : ADDRESS ;
VAR
   v: Vector ;
   l: ADDRESS ;
BEGIN
   v := FindVectorNo(vec) ;
   IF v=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'cannot find vector supplied')
   ELSE
      l := v^.arg ;
      v^.arg := p ;
      RETURN( l )
   END
END AttachVector ;


(*
   IncludeVector - includes, vec, into the despatcher list of
                   possible interrupt causes.
*)

PROCEDURE IncludeVector (vec: CARDINAL) ;
VAR
   r   : INTEGER ;
   v   : Vector ;
   m, s: CARDINAL ;
BEGIN
   v := FindPendingVector(vec) ;
   IF v=NIL
   THEN
      v := FindVectorNo(vec) ;
      IF v=NIL
      THEN
         Halt(__FILE__, __LINE__, __FUNCTION__,
              'cannot find vector supplied') ;
      ELSE
         (* r := printf('including vector %d  (fd = %d)\n', vec, v^.File) ; *)
         v^.pending := Pending[v^.priority] ;
         Pending[v^.priority] := v ;
         IF (v^.type=time) AND (NOT v^.queued)
         THEN
            v^.queued := TRUE ;
            r := GetTimeOfDay(v^.abs) ;
            Assert(r=0) ;
            GetTime(v^.abs, s, m) ;
            Assert(m<Microseconds) ;
            AddTime(v^.abs, v^.rel) ;
            GetTime(v^.abs, s, m) ;
            Assert(m<Microseconds)
         END
      END 
   ELSE
(*
      r := printf('odd vector %d (fd %d) is already attached to the pending queue\n',
                  vec, v^.File) ;
*)
      stop
   END
END IncludeVector ;


(*
   ExcludeVector - excludes, vec, from the despatcher list of
                   possible interrupt causes.
*)

PROCEDURE ExcludeVector (vec: CARDINAL) ;
VAR
   v, u: Vector ;
   r  : INTEGER ;
BEGIN
   v := FindPendingVector(vec) ;
   IF v=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'cannot find pending vector supplied')
   ELSE
      (* r := printf('excluding vector %d\n', vec) ; *)
      IF Pending[v^.priority]=v
      THEN
         Pending[v^.priority] := Pending[v^.priority]^.pending
      ELSE
         u := Pending[v^.priority] ;
         WHILE u^.pending#v DO
            u := u^.pending
         END ;
         u^.pending := v^.pending
      END ;
      IF v^.type=time
      THEN
         v^.queued := FALSE
      END
   END
END ExcludeVector ;


(*
   AddFd - adds the file descriptor, fd, to set, s, updating, max.
*)

PROCEDURE AddFd (VAR s: SetOfFd; VAR max: INTEGER; fd: INTEGER) ;
VAR
   r: INTEGER ;
BEGIN
   max := Max(fd, max) ;
   IF s=NIL
   THEN
      s := InitSet() ;
      FdZero(s)
   END ;
   FdSet(fd, s)
   (* r := printf('%d, ', fd) *)
END AddFd ;


(*
   DumpPendingQueue - displays the pending queue.
*)

PROCEDURE DumpPendingQueue ;
VAR
   r   : INTEGER ;
   p   : PRIORITY ;
   v   : Vector ;
   s, m: CARDINAL ;
BEGIN
   r := printf("Pending queue\n");
   FOR p := MIN(PRIORITY) TO MAX(PRIORITY) DO
      r := printf("[%d]  ", p);
      v := Pending[p] ;
      WHILE v#NIL DO
         IF (v^.type=input) OR (v^.type=output)
         THEN
            r := printf("(fd=%d) (vec=%d)", v^.File, v^.no)
         ELSIF v^.type=time
         THEN
            GetTime(v^.rel, s, m) ;
            Assert(m<Microseconds) ;
            r := printf("time (%d.%6d secs)\n", s, m)
         END ;
         v := v^.pending
      END ;
      r := printf(" \n")
   END
END DumpPendingQueue ;


PROCEDURE stop ;
BEGIN
END stop ;


(*
   AddTime - t1 := t1 + t2
*)

PROCEDURE AddTime (t1, t2: Timeval) ;
VAR
   a, b, s, m: CARDINAL ;
BEGIN
   GetTime(t1, s, m) ;
   Assert(m<Microseconds) ;
   GetTime(t2, a, b) ;
   Assert(b<Microseconds) ;
   INC(a, s) ;
   INC(b, m) ;
   IF b>=Microseconds
   THEN
      DEC(b, Microseconds) ;
      INC(a)
   END ;
   SetTime(t1, a, b)
END AddTime ;


(*
   IsGreaterEqual - returns TRUE if, a>=b
*)

PROCEDURE IsGreaterEqual (a, b: Timeval) : BOOLEAN ;
VAR
   as, am, bs, bm: CARDINAL ;
BEGIN
   GetTime(a, as, am) ;
   Assert(am<Microseconds) ;
   GetTime(b, bs, bm) ;
   Assert(bm<Microseconds) ;
   RETURN( (as>bs) OR ((as=bs) AND (am>=bm)) )
END IsGreaterEqual ;


(*
   SubTime - assigns, s and m, to a - b.
*)

PROCEDURE SubTime (VAR s, m: CARDINAL; a, b: Timeval) ;
VAR
   as, am,
   bs, bm: CARDINAL ;
BEGIN
   GetTime(a, as, am) ;
   Assert(am<Microseconds) ;
   GetTime(b, bs, bm) ;
   Assert(bm<Microseconds) ;
   IF IsGreaterEqual(a, b)
   THEN
      s := as-bs ;
      IF am>=bm
      THEN
         m := am-bm ;
         Assert(m<Microseconds) ;
      ELSE
         Assert(s>0) ;
         DEC(s) ;
         m := (Microseconds + am) - bm ;
         Assert(m<Microseconds)
      END
   ELSE
      s := 0 ;
      m := 0
   END
END SubTime ;


(*
   Listen - will either block indefinitely (until an interrupt)
            or alteratively will test to see whether any interrupts
            are pending.
            If a pending interrupt was found then, call, is called
            and then this procedure returns.
            It only listens for interrupts > pri.
*)

PROCEDURE Listen (untilInterrupt: BOOLEAN;
                  call: DespatchVector;
                  pri: CARDINAL) ;
VAR
   found: BOOLEAN ;
   r    : INTEGER ;
   b4,
   after,
   t    : Timeval ;
   v    : Vector ;
   i, o : SetOfFd ;
   b4s,
   b4m,
   afs,
   afm,
   s, m : CARDINAL ;
   maxFd: INTEGER ;
   p    : CARDINAL ;
BEGIN
   IF pri<MAX(PRIORITY)
   THEN
      IF Debugging
      THEN
         DumpPendingQueue
      END ;
      maxFd := -1 ;
      t := NIL ;
      i := NIL ;
      o := NIL ;
      t := InitTime(MAX(INTEGER), 0) ;
      p := MAX(PRIORITY) ;
      found := FALSE ;
      WHILE p>pri DO
         v := Pending[p] ;
         WHILE v#NIL DO
            WITH v^ DO
               CASE type OF
               
               input :  AddFd(i, maxFd, File) |
               output:  AddFd(o, maxFd, File) |
               time  :  IF IsGreaterEqual(t, abs)
                        THEN
                           GetTime(abs, s, m) ;
                           Assert(m<Microseconds) ;
                           IF Debugging
                           THEN
                              r := printf("shortest delay is %d.%d\n", s, m)
                           END ;
                           SetTime(t, s, m) ;
                           found := TRUE
                        END

               END
            END ;
            v := v^.pending
         END ;
         DEC(p)
      END ;
      IF NOT untilInterrupt
      THEN
         SetTime(t, 0, 0)
      END ;
      IF untilInterrupt AND (i=NIL) AND (o=NIL) AND (NOT found)
      THEN
         Halt(__FILE__, __LINE__, __FUNCTION__,
              'deadlock found, no more processes to run and no interrupts active')
      END ;
      (* r := printf('timeval = 0x%x\n', t) ; *)
      (* r := printf('}\n') ; *)
      IF (NOT found) AND (maxFd=-1) AND (i=NIL) AND (o=NIL)
      THEN
         t := KillTime(t) ;
         RETURN
      ELSE
         GetTime(t, s, m) ;
         Assert(m<Microseconds) ;
         b4 := InitTime(0, 0) ;
         after := InitTime(0, 0) ;
         r := GetTimeOfDay(b4) ;
         Assert(r=0) ;
         SubTime(s, m, t, b4) ;
         SetTime(t, s, m) ;
         IF Debugging
         THEN
            r := printf("select waiting for %u.%u seconds\n", s, m)
         END ;
         REPEAT
            r := pth_select(maxFd+1, i, o, NIL, t) ;
            IF r=-1
            THEN
               perror("pth_select") ;
               r := pth_select(maxFd+1, i, o, NIL, NIL) ;
               IF r=-1
               THEN
                  perror("pth_select timeout argument is faulty")
               END ;
               r := pth_select(maxFd+1, i, NIL, NIL, t) ;
               IF r=-1
               THEN
                  perror("pth_select output fd argument is faulty")
               END ;
               r := pth_select(maxFd+1, NIL, o, NIL, t) ;
               IF r=-1
               THEN
                  perror("pth_select input fd argument is faulty")
               ELSE
                  perror("pth_select maxFD+1 argument is faulty")
               END
            END
         UNTIL r#-1
      END ;
      p := MAX(PRIORITY) ;
      WHILE p>pri DO
         v := Pending[p] ;
         WHILE v#NIL DO
            WITH v^ DO
               CASE type OF
               
               input :  IF (i#NIL) AND FdIsSet(File, i)
                        THEN
                           IF Debugging
                           THEN
                              r := printf('read (fd=%d) is ready (vec=%d)\n', File, no) ;
                              DumpPendingQueue
                           END ;
                           call(no, priority, arg)
                        END |
               output:  IF (o#NIL) AND FdIsSet(File, o)
                        THEN
                           IF Debugging
                           THEN
                              r := printf('write (fd=%d) is ready (vec=%d)\n', File, no) ;
                              DumpPendingQueue
                           END ;
                           call(no, priority, arg)
                        END |
               time  :  IF untilInterrupt OR TRUE
                        THEN
                           r := GetTimeOfDay(after) ;
                           Assert(r=0) ;
                           IF Debugging
                           THEN
                              GetTime(t, s, m) ;
                              Assert(m<Microseconds) ;
                              GetTime(after, afs, afm) ;
                              Assert(afm<Microseconds) ;
                              GetTime(b4, b4s, b4m) ;
                              Assert(b4m<Microseconds) ;
                              r := printf("waited %d.%d + %d.%d now is %d.%d\n",
                                          s, m, b4s, b4m, afs, afm) ;
                           END ;
                           IF IsGreaterEqual(after, abs)
                           THEN
                              IF Debugging
                              THEN
                                 DumpPendingQueue ;
                                 r := printf("time has expired calling despatcher\n")
                              END ;
                              call(no, priority, arg)
                           ELSIF Debugging
                           THEN
                              r := printf("must wait longer as time has not expired\n")
                           END
                        END

               END
            END ;
            v := v^.pending
         END ;
         DEC(p)
      END ;
      IF t#NIL
      THEN
         t := KillTime(t)
      END ;
      IF b4#NIL
      THEN
         t := KillTime(b4)
      END ;
      IF after#NIL
      THEN
         t := KillTime(after)
      END ;
      IF i#NIL
      THEN
         i := KillSet(i)
      END ;
      IF o#NIL
      THEN
         o := KillSet(o)
      END
   END
END Listen ;


(*
   Init - 
*)

PROCEDURE Init ;
VAR
   p: PRIORITY ;
BEGIN
   Exists := NIL ;
   FOR p := MIN(PRIORITY) TO MAX(PRIORITY) DO
      Pending[p] := NIL
   END
END Init ;


BEGIN
   Init
END SysVec.
