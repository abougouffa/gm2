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

IMPLEMENTATION MODULE SysVec ;


FROM M2RTS IMPORT Halt ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM pth IMPORT pth_select ;
FROM libc IMPORT printf ;

FROM Selective IMPORT InitSet, FdSet, Timeval, InitTime, KillTime, KillSet,
                      SetOfFd, FdIsSet ;

TYPE
   VectorType = (input, output, time) ;
   Vector     = POINTER TO RECORD
                              type   : VectorType ;
                              arg    : ADDRESS ;
                              pending,
                              exists : Vector ;
                              no     : CARDINAL ;
                              File   : INTEGER ;
                              Micro,
                              Secs   : CARDINAL ;
                              
(*  BUG IN GM2
                              CASE type OF

                              time  : Micro: CARDINAL ;
                                      Secs : CARDINAL |
                              input,
                              output: File : INTEGER

                              END
*)
                           END ;

VAR
   VecNo  : CARDINAL ;
   Exists,
   Pending: Vector ;


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
   WHILE (v#NIL) AND (v^.type#t) AND (v^.File#fd) DO
      v := v^.exists
   END ;
   RETURN( v )
END FindVector ;


(*
   InitInputVector - returns an interrupt vector which is associated
                     with the file descriptor, fd.
*)

PROCEDURE InitInputVector (fd: INTEGER) : CARDINAL ;
VAR
   v: Vector ;
BEGIN
   v := FindVector(fd, input) ;
   IF v=NIL
   THEN
      NEW(v) ;
      INC(VecNo) ;
      WITH v^ DO
         type    := input ;
         arg     := NIL ;
         pending := NIL ;
         exists  := Exists ;
         no      := VecNo ;
         File    := fd
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

PROCEDURE InitOutputVector (fd: INTEGER) : CARDINAL ;
VAR
   v: Vector ;
BEGIN
   v := FindVector(fd, input) ;
   IF v=NIL
   THEN
      NEW(v) ;
      INC(VecNo) ;
      WITH v^ DO
         type    := output ;
         arg     := NIL ;
         pending := NIL ;
         exists  := Exists ;
         no      := VecNo ;
         File    := fd
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

PROCEDURE InitTimeVector (micro, secs: CARDINAL) : CARDINAL ;
VAR
   v: Vector ;
BEGIN
   NEW(v) ;
   INC(VecNo) ;
   WITH v^ DO
      type    := time ;
      arg     := NIL ;
      pending := NIL ;
      exists  := Exists ;
      no      := VecNo ;
      Secs    := secs ;
      Micro   := micro
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
   v: Vector ;
BEGIN
   v := Pending ;
   WHILE (v#NIL) AND (v^.no#vec) DO
      v := v^.pending
   END ;
   RETURN( v )
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
   v := FindVectorNo(vec) ;
   IF v=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'cannot find vector supplied')
   ELSE
      WITH v^ DO
         Micro   := micro ;
         Secs    := secs
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
         micro := Micro ;
         secs  := Secs
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
   v: Vector ;
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
         v^.pending := Pending ;
         Pending := v
      END 
   END
END IncludeVector ;


(*
   ExcludeVector - excludes, vec, from the despatcher list of
                   possible interrupt causes.
*)

PROCEDURE ExcludeVector (vec: CARDINAL) ;
VAR
   v, u: Vector ;
BEGIN
   v := FindPendingVector(vec) ;
   IF v=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'cannot find pending vector supplied')
   ELSE
      IF Pending=v
      THEN
         Pending := Pending^.pending
      ELSE
         u := Pending ;
         WHILE u^.pending#v DO
            u := u^.pending
         END ;
         u^.pending := v^.pending
      END
   END
END ExcludeVector ;


(*
   AddFd - adds the file descriptor, fd, to set, s, updating, max.
*)

PROCEDURE AddFd (VAR s: SetOfFd; VAR max: INTEGER; fd: INTEGER) ;
BEGIN
   max := Max(fd, max) ;
   IF s=NIL
   THEN
      s := InitSet()
   END ;
   FdSet(fd, s)
END AddFd ;


(*
   Listen - will either block indefinitely (until an interrupt)
            or alteratively will test to see whether any interrupts
            are pending.
            If a pending interrupt was found then, call, is called
            and then this procedure returns.
*)

PROCEDURE Listen (untilInterrupt: BOOLEAN; call: DespatchVector) ;
VAR
   r    : INTEGER ;
   t    : Timeval ;
   v    : Vector ;
   i, o : SetOfFd ;
   maxFd,
   s, m : INTEGER ;
BEGIN
   maxFd := 0 ;
   t := NIL ;
   i := NIL ;
   o := NIL ;
   s := -1 ;
   m := -1 ;
   v := Pending ;
   WHILE v#NIL DO
      WITH v^ DO
         CASE type OF

         input :  AddFd(i, maxFd, File) |
         output:  AddFd(o, maxFd, File) |
         time  :  IF s=-1
                  THEN
                     s := Secs ;
                     m := Micro
                  ELSE
                     s := Min(s, Secs) ;
                     m := Min(m, Micro)
                  END

         END
      END ;
      v := v^.pending
   END ;
   IF s=-1
   THEN
      IF NOT untilInterrupt
      THEN
         t := InitTime(0, 0)
      END
   ELSE
      t := InitTime(s, m)
   END ;
   IF untilInterrupt AND (i=NIL) AND (o=NIL) AND (s=-1)
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'deadlock found, no more processes to run and no interrupts active')
   END ;
   (* r := printf('timeval = 0x%x\n', t) ; *)
   r := pth_select(maxFd+1, i, o, NIL, t) ;
(*
   IF t#NIL
   THEN
      GetTime(t, s, m) ;
      v := Pending ;
      WHILE v#NIL DO
         IF v^.type=time
         THEN
            
         END
      END
   END
*)
   v := Pending ;
   WHILE v#NIL DO
      WITH v^ DO
         CASE type OF

         input :  IF FdIsSet(File, i)
                  THEN
                     call(no, arg)
                  END |
         output:  IF FdIsSet(File, o)
                  THEN
                     call(no, arg)
                  END |
         time  :  call(no, arg) ;
                  (* think ...
                  IF NOT untilInterrupt
                  THEN
                     IF s=-1
                     THEN
                        s := Secs ;
                        m := Micro
                     ELSE
                        s := Min(s, Secs) ;
                        m := Min(m, Micro)
                     END
                  END  *)

         END
      END ;
      v := v^.pending
   END ;
   IF t#NIL
   THEN
      t := KillTime(t)
   END ;
   IF i#NIL
   THEN
      i := KillSet(i)
   END ;
   IF o#NIL
   THEN
      o := KillSet(o)
   END
END Listen ;


END SysVec.
