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

IMPLEMENTATION MODULE COROUTINES ;

FROM pth IMPORT pth_attr_t, pth_t,
                pth_suspend, pth_resume, pth_yield,
                pth_attr_set, pth_attr_new, pth_spawn,
                PTH_ATTR_PRIO, PTH_PRIO_MIN, PTH_PRIO_MAX ;

TYPE
   Status = (suspended, ready, new) ;

   COROUTINE = POINTER TO RECORD
                             attribute : pth_attr_t ;
                             thread    : pth_t ;
                             wspace    : SYSTEM.ADDRESS ;
                             nLocs     : CARDINAL ;
                             status    : Status ;
                             protection: PROTECTION ;
                          END ;

   INTERRUPTSOURCE = CARDINAL ;

VAR
   currentCoRoutine: COROUTINE ;

(*
   Suspend - suspends coroutine, c, and sets its status to suspended.
*)

PROCEDURE Suspend (c: COROUTINE) ;
VAR
   i: INTEGER ;
BEGIN
   WITH c^ DO
      i := pth_suspend(thread) ;
      IF i#0
      THEN
         HALT
      END ;
      status := suspended
   END
END Suspend ;


(*
   Resume - resumes coroutine, c, and sets its status to ready.
*)

PROCEDURE Resume (c: COROUTINE) ;
VAR
   i: INTEGER ;
BEGIN
   WITH c^ DO
      IF status=suspended
      THEN
         i := pth_resume(thread) ;
         IF i#0
         THEN
            HALT
         END ;
         status := ready
      END
   END
END Resume ;


(*
   Yield - attempts to yield to COROUTINE, c.
*)

PROCEDURE Yield (c: COROUTINE) ;
VAR
   i: INTEGER ;
BEGIN
   i := pth_yield(c^.thread) ;
   IF i#0
   THEN
      HALT
   END
END Yield ;


(*
   Priority - attempts to set proirity of COROUTINE, c, to, p.
*)

PROCEDURE Priority (c: COROUTINE; p: INTEGER) ;
BEGIN
   WITH c^ DO
      priority := p ;
      i := pth_attr_set(attribute, PTH_ATTR_PRIO, p) ;
      IF i#0
      THEN
         HALT
      END
   END
END Priority ;


PROCEDURE NEWCOROUTINE (procBody: PROC;
                        workspace: SYSTEM.ADDRESS;
                        size: CARDINAL;
                        VAR cr: COROUTINE;
                        [initProtection: PROTECTION]);

  (* Creates a new coroutine whose body is given by procBody, and
     returns the identity of the coroutine in cr. workspace is a
     pointer to the work space allocated to the coroutine; size
     specifies the size of this workspace in terms of SYSTEM.LOC.

     The optarg, initProtection, may contain a single parameter which
     specifies the initial protection level of the coroutine.
  *)
BEGIN
   NEW(cr) ;
   IF initProtection=UnassignedPriority
   THEN
      initProtection := PROT()
   END ;
   WITH cr^ DO
      attribute  := pth_attr_new() ;
      Priority(cr, PTH_PRIO_MIN) ;
      thread     := pth_spawn(attribute, procBody, NIL) ;
      wspace     := workspace ;
      nLocs      := size ;
      protection := initProtection ;
      status     := new
   END
END NEWCOROUTINE ;


PROCEDURE TRANSFER (VAR from: COROUTINE; to: COROUTINE);
  (* Returns the identity of the calling coroutine in from, and
     transfers control to the coroutine specified by to.
  *)
BEGIN
   from := currentCoRoutine ;
   Priority(currentCoRoutine, PTH_PRIO_MIN) ;
   Resume(to) ;
   currentCoRoutine := to ;
   Priority(currentCoRoutine, PTH_PRIO_MAX) ;
   Yield(currentCoRoutine) ;
   (* now we are running currentCoRoutine so we suspend, from *)
   Suspend(from)
END TRANSFER ;


PROCEDURE IOTRANSFER (VAR from: COROUTINE; to: COROUTINE);
  (* Returns the identity of the calling coroutine in from and
     transfers control to the coroutine specified by to.  On
     occurrence of an interrupt, associated with the caller, control
     is transferred back to the caller, and the identity of the
     interrupted coroutine is returned in from.  The calling coroutine
     must be associated with a source of interrupts.
  *)
BEGIN
   
END IOTRANSFER ;
    

PROCEDURE ATTACH (source: INTERRUPTSOURCE);
  (* Associates the specified source of interrupts with the calling
     coroutine. *)
BEGIN
   
END ATTACH ;


PROCEDURE DETACH (source: INTERRUPTSOURCE);
  (* Dissociates the specified source of interrupts from the calling
     coroutine. *)
BEGIN
   
END DETACH ;


PROCEDURE IsATTACHED (source: INTERRUPTSOURCE): BOOLEAN;
  (* Returns TRUE if and only if the specified source of interrupts is
     currently associated with a coroutine; otherwise returns FALSE.
  *)
BEGIN
   
END IsATTACHED ;


PROCEDURE HANDLER (source: INTERRUPTSOURCE): COROUTINE;
  (* Returns the coroutine, if any, that is associated with the source
     of interrupts. The result is undefined if IsATTACHED(source) =
     FALSE.
  *)
BEGIN
   
END HANDLER ;


PROCEDURE CURRENT (): COROUTINE;
  (* Returns the identity of the calling coroutine. *)
BEGIN
   RETURN currentCoRoutine
END CURRENT ;


PROCEDURE LISTEN (p: PROTECTION);
  (* Momentarily changes the protection of the calling coroutine to p. *)
BEGIN
   
END LISTEN ;


PROCEDURE PROT (): PROTECTION;
  (* Returns the protection of the calling coroutine. *)
BEGIN
   RETURN currentCoRoutine^.protection
END PROT ;


(*
   Init - 
*)

PROCEDURE Init ;
BEGIN
   currentCoRoutine := NIL
END Init ;


BEGIN
   Init
END COROUTINES.
