(* Copyright (C) 2009, 2010 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
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

FROM COROUTINES IMPORT COROUTINE, NEWCOROUTINE, TRANSFER ;

(* The following procedures create processes and switch control between
   them. *)

TYPE
   ProcessId = POINTER TO RECORD
                             body       : Body ;
                             workSpace  : CARDINAL ;
                             urgency    : Urgency ;
                             context    : COROUTINE ;
                             params     : Parameter ;
                             state      : Status ;
                             right, left: ProcessId ;
                          END ;

   Status = (ready, waiting, passive) ;

VAR
   pQueue: ARRAY Status OF ProcessId ;


(* Creates a new process with procBody as its body, and with urgency
   and parameters given by procUrg and procParams.  At least as
   much workspace (in units of SYSTEM.LOC) as is specified by
   extraSpace is allocated to the process.
   An identity for the new process is returned in procId.
   The process is created in the passive state; it will not run
   until activated.
*)

PROCEDURE Create (procBody: Body; extraSpace: CARDINAL; procUrg: Urgency;
                  procParams: Parameter; VAR procId: ProcessId) ;
VAR
   a: ADDRESS ;
BEGIN
   NEW(procId) ;
   WITH procId^ DO
      body      := procBody ;
      workSpace := extraSpace + defaultSpace ;
      urgency   := procUrg ;
      ALLOCATE(a, workSpace) ;
      NEWCORROUTINE(procBody, a, workSpace, context) ;
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
   Create(procBody, extraSpace, procBody, procParams, procId) ;
   Activate(procId)
END Start ;


(* Terminates the calling process.
   The process must not be associated with a source of events.
*)

PROCEDURE StopMe ;
BEGIN
   
END StopMe ;


(* Causes the calling process to enter the passive state.  The
   procedure only returns when the calling process is again
   activated by another process.
*)

PROCEDURE SuspendMe ;
BEGIN
   OnPassiveQueue(Me()) ;
   Reschedule
END SuspendMe ;


PROCEDURE Activate (procId: ProcessId) ;
  (* Causes the process identified by procId to enter the ready
     state, and thus to become eligible to run again.
  *)
BEGIN
   OnReadyQueue(procId) ;
   Reschedule
END Activate ;


PROCEDURE SuspendMeAndActivate (procId: ProcessId) ;
  (* Executes an atomic sequence of SuspendMe() and
     Activate(procId). *)
BEGIN
   OnPassiveQueue(Me()) ;
   Activate(procId)
END SuspendMeAndActivate ;


PROCEDURE Switch (procId: ProcessId; VAR info: Parameter) ;
  (* Causes the calling process to enter the passive state; the
     process identified by procId becomes the currently executing
     process.  info is used to pass parameter information from the
     calling to the activated process.  On return, info will
     contain information from the process that chooses to switch
     back to this one (or will be NIL if Activate or
     SuspendMeAndActivate are used instead of Switch).
  *)
BEGIN
   
END Switch ;


PROCEDURE Wait ;
  (* Causes the calling process to enter the waiting state.
     The procedure will return when the calling process is
     activated by another process, or when one of its associated
     eventSources has generated an event.
  *)
BEGIN
   
END Wait ;


(* The following procedures allow the association of processes
   with sources of external events.
*)

PROCEDURE Attach (eventSource: Sources) ;
  (* Associates the specified eventSource with the calling
     process. *)
BEGIN
   
END Attach ;


PROCEDURE Detach (eventSource: Sources) ;
  (* Dissociates the specified eventSource from the program. *)
BEGIN
   
END Detach ;


PROCEDURE IsAttached (eventSource: Sources) : BOOLEAN ;
  (* Returns TRUE if and only if the specified eventSource is
     currently associated with one of the processes of the
     program.
  *)
BEGIN
   
END IsAttached ;


PROCEDURE Handler (eventSource: Sources) : ProcessId ;
  (* Returns the identity of the process, if any, that is
     associated with the specified eventSource.
  *)
BEGIN
   
END Handler ;


(* The following procedures allow processes to obtain their
   identity, parameters, and urgency.
*)

PROCEDURE Me () : ProcessId ;
  (* Returns the identity of the calling process (as assigned
     when the process was first created).
  *)
BEGIN
   
END Me ;


PROCEDURE MyParam () : Parameter ;
  (* Returns the value specified as procParams when the calling
     process was created. *)
BEGIN
   
END MyParam ;


PROCEDURE UrgencyOf (procId: ProcessId) : Urgency ;
  (* Returns the urgency established when the process identified
     by procId was first created.
  *)
BEGIN
   
END UrgencyOf ;


(* The following procedure provides facilities for exception
   handlers. *)

PROCEDURE ProcessesException () : ProcessesExceptions;
  (* If the current coroutine is in the exceptional execution state
     because of the raising of a language exception, returns the
     corresponding enumeration value, and otherwise raises an
     exception.
  *)
BEGIN
   
END ProcessesException ;


PROCEDURE IsProcessesException () : BOOLEAN ;
  (* Returns TRUE if the current coroutine is in the exceptional
     execution state because of the raising of an exception in
     a routine from this module; otherwise returns FALSE.
  *)
BEGIN
   
END IsProcessesException ;


PROCEDURE Init ;
BEGIN
   pQueue[ready] := NIL ;
   pQueue[waiting] := NIL ;
   pQueue[passive] := NIL
END Init ;


BEGIN
   Init
END Processes.
