(* Copyright (C) 2009, 2010
                 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 3.1 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA *)

IMPLEMENTATION MODULE Processes ;

FROM Assertion IMPORT Assert ;
FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM COROUTINES IMPORT COROUTINE, NEWCOROUTINE, TRANSFER, CURRENT, ATTACH, DETACH, IsATTACHED, HANDLER ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM RTExceptions IMPORT IsInExceptionState, GetExceptionBlock, GetNumber, Raise ;
FROM M2EXCEPTION IMPORT M2Exceptions ;
FROM M2RTS IMPORT NoException ;

FROM EXCEPTIONS IMPORT ExceptionSource, RAISE, AllocateSource, CurrentNumber,
                       IsCurrentSource, IsExceptionalExecution ;


CONST
   defaultSpace = 1024 * 1024 * 8 ;


(* The following procedures create processes and switch control between
   them. *)

TYPE
   ProcessId = POINTER TO RECORD
                             body       : Body ;
                             workSpace  : CARDINAL ;
                             stack      : ADDRESS ;
                             urgency    : Urgency ;
                             context    : COROUTINE ;
                             params     : Parameter ;
                             state      : Status ;
                             right, left: ProcessId ;
                          END ;

   Status = (ready, waiting, passive, dead) ;

VAR
   process  : ExceptionSource ;
   pQueue   : ARRAY Status OF ProcessId ;
   free,
   currentId: ProcessId ;


(*
   New - assigns, p, to a new ProcessId.
*)

PROCEDURE New (VAR p: ProcessId) ;
BEGIN
   IF free=NIL
   THEN
      NEW(p)
   ELSE
      p := free ;
      free := free^.right
   END
END New ;


(*
   Dispose - returns, p, to the free list.
*)

PROCEDURE Dispose (VAR p: ProcessId) ;
BEGIN
   p^.right := free ;
   free := p
END Dispose ;


(*
   add - adds process, p, to queue, head.
*)

PROCEDURE add (VAR head: ProcessId; p: ProcessId) ;
BEGIN
   IF head=NIL
   THEN
      head := p ;
      p^.left := p ;
      p^.right := p
   ELSE
      p^.right := head ;
      p^.left := head^.left ;
      head^.left^.right := p ;
      head^.left := p
   END
END add ;


(*
   sub - subtracts process, p, from queue, head.
*)

PROCEDURE sub (VAR head: ProcessId; p: ProcessId) ;
BEGIN
   IF (p^.left=head) AND (p=head)
   THEN
      head := NIL
   ELSE
      IF head=p
      THEN
         head := head^.right
      END ;
      p^.left^.right := p^.right ;
      p^.right^.left := p^.left
   END
END sub ;


(*
   Add - adds, p, to the appropriate queue.
*)

PROCEDURE Add (p: ProcessId) ;
BEGIN
   add(pQueue[p^.state], p)
END Add ;


(*
   Remove - remove, p, from the appropriate queue.
*)

PROCEDURE Remove (p: ProcessId) ;
BEGIN
   sub(pQueue[p^.state], p)
END Remove ;


(*
   OnDeadQueue - removes process, p, from the queue and adds it
                 to the dead queue.
*)

PROCEDURE OnDeadQueue (p: ProcessId) ;
BEGIN
   Remove(p) ;
   p^.state := dead ;
   Add(p)
END OnDeadQueue ;


(*
   OnReadyQueue - removes process, p, from the queue and adds it
                  to the ready queue.
*)

PROCEDURE OnReadyQueue (p: ProcessId) ;
BEGIN
   Remove(p) ;
   p^.state := ready ;
   Add(p)
END OnReadyQueue ;


(*
   OnPassiveQueue - removes process, p, from the queue and adds it
                    to the passive queue.
*)

PROCEDURE OnPassiveQueue (p: ProcessId) ;
BEGIN
   Remove(p) ;
   p^.state := passive ;
   Add(p)
END OnPassiveQueue ;


(*
   OnWaitingQueue - removes process, p, from the queue and adds it
                    to the waiting queue.
*)

PROCEDURE OnWaitingQueue (p: ProcessId) ;
BEGIN
   Remove(p) ;
   p^.state := waiting ;
   Add(p)
END OnWaitingQueue ;


(*
   checkDead - check to see if any processes are on the dead queue
               and if they are not the current process deallocate
               resources.
*)

PROCEDURE checkDead ;
VAR
   p: ProcessId ;
BEGIN
   p := pQueue[dead] ;
   WHILE (p#NIL) AND (p#currentId) DO
      Remove(p) ;
      WITH p^ DO
         IF stack#NIL
         THEN
            DEALLOCATE(stack, workSpace)
         END
      END ;
      Dispose(p) ;
      p := pQueue[dead]
   END
END checkDead ;


(*
   Reschedule -
*)

PROCEDURE Reschedule ;
VAR
   p,
   best,
   head: ProcessId ;
BEGIN
   checkDead ;
   IF pQueue[ready]^.right=pQueue[ready]
   THEN
      (* only one process on the ready queue, don't bother searching *)
      Assert(currentId=pQueue[ready]) ;
      RETURN
   ELSE
      head := pQueue[ready] ;
      best := currentId ;
      p := head ;
      REPEAT
         IF p^.urgency>best^.urgency
         THEN
            best := p
         END ;
         p := p^.right
      UNTIL p=head ;
      IF best#currentId
      THEN
         p := currentId ;
         currentId := best ;
         TRANSFER(p^.context, currentId^.context)
      END
   END
END Reschedule ;


(*
   Create - creates a new process with procBody as its body,
            and with urgency and parameters given by procUrg
            and procParams.  At least as much workspace (in
            units of SYSTEM.LOC) as is specified by extraSpace
            is allocated to the process.  An identity for the
            new process is returned in procId.  The process is
            created in the passive state; it will not run
            until activated.
*)

PROCEDURE Create (procBody: Body; extraSpace: CARDINAL; procUrg: Urgency;
                  procParams: Parameter; VAR procId: ProcessId) ;
BEGIN
   New(procId) ;
   WITH procId^ DO
      body      := procBody ;
      workSpace := extraSpace + defaultSpace ;
      urgency   := procUrg ;
      ALLOCATE(stack, workSpace) ;
      NEWCOROUTINE(procBody, stack, workSpace, context) ;
      params    := procParams ;
      state     := passive ;
      right     := NIL ;
      left      := NIL
   END ;
   OnPassiveQueue(procId)
END Create ;


(*
   Creates a new process, with parameters as for Create.
   The process is created in the ready state; it is eligible to
   run immediately.
*)

PROCEDURE Start (procBody: Body; extraSpace: CARDINAL; procUrg: Urgency;
                 procParams: Parameter; VAR procId: ProcessId) ;
BEGIN
   Create(procBody, extraSpace, procUrg, procParams, procId) ;
   Activate(procId)
END Start ;


(*
   StopMe - terminates the calling process.
            The process must not be associated with a source
            of events.
*)

PROCEDURE StopMe ;
BEGIN
   OnDeadQueue(Me()) ;
   Reschedule
END StopMe ;


(*
   SuspendMe - causes the calling process to enter the passive state.
               The procedure only returns when the calling process
               is again activated by another process.
*)

PROCEDURE SuspendMe ;
BEGIN
   OnPassiveQueue(Me()) ;
   Reschedule
END SuspendMe ;


(*
   doActivate - activate procId and pass, info, in the parameter field.
*)

PROCEDURE doActivate (procId: ProcessId; info: Parameter) ;
BEGIN
   procId^.params := info ;
   OnReadyQueue(procId) ;
   Reschedule
END doActivate ;


(*
   Activate - causes the process identified by procId to enter the ready
              state, and thus to become eligible to run again.
*)

PROCEDURE Activate (procId: ProcessId) ;
BEGIN
   doActivate(procId, NIL)
END Activate ;


(*
   SuspendMeAndActivate - executes an atomic sequence of SuspendMe() and
                          Activate(procId).
*)

PROCEDURE SuspendMeAndActivate (procId: ProcessId) ;
BEGIN
   OnPassiveQueue(Me()) ;
   doActivate(procId, NIL)
END SuspendMeAndActivate ;


(*
   Switch - causes the calling process to enter the passive state; the
            process identified by procId becomes the currently executing
            process.  info is used to pass parameter information from the
            calling to the activated process.  On return, info will
            contain information from the process that chooses to switch
            back to this one (or will be NIL if Activate or
            SuspendMeAndActivate are used instead of Switch).
*)

PROCEDURE Switch (procId: ProcessId; VAR info: Parameter) ;
VAR
   p: ProcessId ;
BEGIN
   OnPassiveQueue(Me()) ;
   doActivate(procId, info) ;
   p := Me() ;
   info := p^.params
END Switch ;


(*
   Wait - causes the calling process to enter the waiting state.
          The procedure will return when the calling process is
          activated by another process, or when one of its
          associated eventSources has generated an event.
*)

PROCEDURE Wait ;
BEGIN
   OnWaitingQueue(Me()) ;
   Reschedule
END Wait ;


(* The following procedures allow the association of processes
   with sources of external events.
*)

(*
   Attach - associates the specified eventSource with the calling
            process.
*)

PROCEDURE Attach (eventSource: Sources) ;
BEGIN
   ATTACH(eventSource)
END Attach ;


(*
   Detach - dissociates the specified eventSource from the program.
*)

PROCEDURE Detach (eventSource: Sources) ;
BEGIN
   DETACH(eventSource)
END Detach ;


(*
   IsAttached - returns TRUE if and only if the specified eventSource is
                currently associated with one of the processes of the
                program.
*)

PROCEDURE IsAttached (eventSource: Sources) : BOOLEAN ;
BEGIN
   RETURN Handler(eventSource)#NIL
END IsAttached ;


(*
   Handler - returns the identity of the process, if any, that is
             associated with the specified eventSource.
*)

PROCEDURE Handler (eventSource: Sources) : ProcessId ;
VAR
   c: COROUTINE ;
   p: ProcessId ;
   s: Status ;
BEGIN
   c := HANDLER(eventSource) ;
   FOR s := MIN(Status) TO MAX(Status) DO
      p := pQueue[s] ;
      IF p#NIL
      THEN
         REPEAT
            IF p^.context=c
            THEN
               RETURN p
            ELSE
               p := p^.right
            END
         UNTIL p=pQueue[s]
      END
   END ;
   RETURN NIL
END Handler ;


(* The following procedures allow processes to obtain their
   identity, parameters, and urgency.
*)


(*
   Me - returns the identity of the calling process (as assigned
        when the process was first created).
*)

PROCEDURE Me () : ProcessId ;
BEGIN
   RETURN currentId
END Me ;


(*
   MyParam - returns the value specified as procParams when the
             calling process was created.
*)

PROCEDURE MyParam () : Parameter ;
BEGIN
   RETURN currentId^.params
END MyParam ;


(*
   UrgencyOf - returns the urgency established when the process identified
               by procId was first created.
*)

PROCEDURE UrgencyOf (procId: ProcessId) : Urgency ;
BEGIN
   RETURN currentId^.urgency
END UrgencyOf ;


(* The following procedure provides facilities for exception
   handlers. *)


(*
   ProcessException - if the current coroutine is in the exceptional
                      execution state because of the raising of a language
                      exception, returns the corresponding enumeration value,
                      and otherwise raises an exception.
*)

PROCEDURE ProcessesException () : ProcessesExceptions ;
BEGIN
   IF IsProcessesException()
   THEN
      RETURN( VAL(ProcessesExceptions, CurrentNumber(process)) )
   ELSE
      NoException(ADR(__FILE__), __LINE__,
                  __COLUMN__, ADR(__FUNCTION__),
                  ADR ("not in the exceptional execution state"))
   END
END ProcessesException ;


(*
   IsProcessException - returns TRUE if the current coroutine is
                        in the exceptional execution state because
                        of the raising of an exception in
                        a routine from this module; otherwise returns
                        FALSE.
*)

PROCEDURE IsProcessesException () : BOOLEAN ;
BEGIN
   RETURN( IsExceptionalExecution() AND IsCurrentSource(process) )
END IsProcessesException ;


(*
   setupCurrentId - sets up the initial process.
*)

PROCEDURE setupCurrentId ;
BEGIN
   NEW(currentId) ;
   WITH currentId^ DO
      workSpace := 0 ;
      stack     := NIL ;
      urgency   := 0 ;
      context   := CURRENT() ;
      params    := NIL ;
      state     := passive ;
      right     := NIL ;
      left      := NIL
   END ;
   OnReadyQueue(currentId)
END setupCurrentId ;


(*
   Init - sets up all the module data structures.
*)

PROCEDURE Init ;
BEGIN
   AllocateSource(process) ;
   free := NIL ;
   pQueue[ready] := NIL ;
   pQueue[waiting] := NIL ;
   pQueue[passive] := NIL ;
   pQueue[dead] := NIL ;
   setupCurrentId
END Init ;


BEGIN
   Init
END Processes.
