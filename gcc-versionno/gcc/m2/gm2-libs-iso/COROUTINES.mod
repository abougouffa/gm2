(* COROUTINES.mod implement the ISO COROUTINES specification.

Copyright (C) 2002-2020 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE COROUTINES ;

FROM RTco IMPORT init, initThread, transfer, initSemaphore,
                 wait, signal, currentThread, turnInterrupts,
                 currentInterruptLevel ;

FROM RTExceptions IMPORT EHBlock, InitExceptionBlock,
                         SetExceptionBlock, GetExceptionBlock,
                         SetExceptionState, IsInExceptionState,
                         SetExceptionSource, GetExceptionSource ;

FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM EXCEPTIONS IMPORT ExceptionSource ;
FROM RTint IMPORT Listen, AttachVector, IncludeVector, ExcludeVector ;
FROM Storage IMPORT ALLOCATE ;
FROM M2RTS IMPORT Halt ;
FROM libc IMPORT printf ;

IMPORT RTint ;


CONST
   MinStack = 16 * 1024 * 1024 ;

TYPE
   Status = (suspended, ready, new, running) ;

   COROUTINE = POINTER TO RECORD
                             context   : INTEGER ;
                             ehblock   : EHBlock ;
                             inexcept  : BOOLEAN ;
                             source    : ExceptionSource ;
                             wspace    : SYSTEM.ADDRESS ;
                             nLocs     : CARDINAL ;
                             status    : Status ;
                             attached  : SourceList ;
                             next      : COROUTINE ;
                          END ;

   SourceList = POINTER TO RECORD
                              next     : SourceList ;    (* next in the list of vectors which are      *)
                                                         (* attached to this coroutine.                *)
                              vec      : INTERRUPTSOURCE ;  (* the interrupt vector (source)           *)
                              curco    : COROUTINE ;     (* the coroutine which is waiting on this vec *)
                              chain    : SourceList ;    (* the next coroutine waiting on this vec     *)
                              ptrToTo,
                              ptrToFrom: POINTER TO COROUTINE ;
                           END ;


VAR
   freeList         : SourceList ;
   head             : COROUTINE ;
   currentCoRoutine : COROUTINE ;
   illegalFinish    : ADDRESS ;
   initMain,
   initCo           : BOOLEAN ;
   lock             : INTEGER ;    (* semaphore protecting module data structures.  *)


PROCEDURE NEWCOROUTINE (procBody: PROC;
                        workspace: SYSTEM.ADDRESS;
                        size: CARDINAL;
                        VAR cr: COROUTINE;
                        [initProtection: PROTECTION]);

  (* Creates a new coroutine whose body is given by procBody, and
     returns the identity of the coroutine in cr. workspace is a
     pointer to the work space allocated to the coroutine; size
     specifies the size of this workspace in terms of SYSTEM.LOC.

     The optarg, initProtection, may contain a single parameter
     which specifies the initial protection level of the coroutine.
  *)
VAR
   tp : INTEGER ;
   old: PROTECTION ;
BEGIN
   localInit ;
   old := TurnInterrupts (MAX (PROTECTION)) ;
   IF initProtection = UnassignedPriority
   THEN
      initProtection := PROT ()
   END ;
   tp := initThread (procBody, size, initProtection) ;
   IF tp = -1
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'unable to create a new thread')
   END ;
   NEW (cr) ;
   WITH cr^ DO
      context    := tp ;
      ehblock    := InitExceptionBlock () ;
      inexcept   := FALSE ;
      source     := NIL ;
      wspace     := workspace ;
      nLocs      := size ;
      status     := new ;
      attached   := NIL ;
      next       := head
   END ;
   head := cr ;
   old := TurnInterrupts (old)
END NEWCOROUTINE ;


PROCEDURE TRANSFER (VAR from: COROUTINE; to: COROUTINE);
  (* Returns the identity of the calling coroutine in from, and
     transfers control to the coroutine specified by to.
  *)
VAR
   old: PROTECTION ;
BEGIN
   localInit ;
   old := TurnInterrupts (MAX (PROTECTION)) ;
   wait (lock) ;
   from := currentCoRoutine ;
   IF to^.context = from^.context
   THEN
      Halt (__FILE__, __LINE__, __FUNCTION__,
            'error when attempting to context switch to the same process')
   END ;
   from^.inexcept := SetExceptionState (to^.inexcept) ;
   from^.source := GetExceptionSource () ;
   currentCoRoutine := to ;
   SetExceptionBlock (currentCoRoutine^.ehblock) ;
   SetExceptionSource (currentCoRoutine^.source) ;
   signal (lock) ;
   transfer (from^.context, to^.context) ;
   old := TurnInterrupts (old)
END TRANSFER ;


(*
   localMain - creates the holder for the main process.
*)

PROCEDURE localMain ;
VAR
   old: PROTECTION ;
BEGIN
   IF NOT initMain
   THEN
      initMain := TRUE ;
      lock := initSemaphore (1) ;
      wait (lock) ;
      NEW (currentCoRoutine) ;
      WITH currentCoRoutine^ DO
         context    := currentThread () ;
         ehblock    := GetExceptionBlock () ;
         inexcept   := IsInExceptionState () ;
         source     := GetExceptionSource () ;
         wspace     := NIL ;
         nLocs      := 0 ;
         status     := running ;
         attached   := NIL ;
         next       := head
      END ;
      head := currentCoRoutine ;
      old := turnInterrupts (MAX (PROTECTION)) ;    (* was UnassignedPriority *)
      signal (lock)
   END
END localMain ;


(*
   localInit - checks to see whether we need to initialize libpth.
*)

PROCEDURE localInit ;
BEGIN
   IF NOT initCo
   THEN
      IF init () # 0
      THEN
         Halt (__FILE__, __LINE__, __FUNCTION__,
               'failed to initialize RTco')
      END ;
      RTint.Init ;
      initCo := TRUE ;
   END ;
   localMain
END localInit ;


PROCEDURE IOTRANSFER (VAR from: COROUTINE; to: COROUTINE);
  (* Returns the identity of the calling coroutine in from and
     transfers control to the coroutine specified by to.  On
     occurrence of an interrupt, associated with the caller, control
     is transferred back to the caller, and the identity of the
     interrupted coroutine is returned in from.  The calling coroutine
     must be associated with a source of interrupts.
  *)
VAR
   l  : SourceList ;
   old: PROTECTION ;
BEGIN
   localInit ;
   old := TurnInterrupts (MAX (PROTECTION)) ;
   wait (lock) ;
   l := currentCoRoutine^.attached ;
   IF l=NIL
   THEN
      printf("no source of interrupts associated with coroutine\n")
   END ;
   WHILE l#NIL DO
      WITH l^ DO
         ptrToFrom := ADR(from) ;
         ptrToTo   := ADR(to) ;
         curco := currentCoRoutine ;
         chain := AttachVector(vec, l) ;
         IF chain#NIL
         THEN
            printf("not expecting multiple COROUTINES to be waiting on a single interrupt source\n")
         END ;
         IncludeVector(vec)
      END ;
      l := l^.next
   END ;
   TRANSFER(from, to) ;
   signal (lock) ;
   old := TurnInterrupts (old)
END IOTRANSFER ;


(*
   New - assigns, l, to a new SourceList.
*)

PROCEDURE New (VAR l: SourceList) ;
BEGIN
   IF freeList=NIL
   THEN
      NEW(l)
   ELSE
      l := freeList ;
      freeList := freeList^.next
   END
END New ;


(*
   Dispose - returns, l, to the freeList.
*)

PROCEDURE Dispose (l: SourceList) ;
BEGIN
   l^.next := freeList ;
   freeList := l
END Dispose ;


PROCEDURE ATTACH (source: INTERRUPTSOURCE);
  (* Associates the specified source of interrupts with the calling
     coroutine. *)
VAR
   l: SourceList ;
BEGIN
   localInit ;
   wait (lock) ;
   l := currentCoRoutine^.attached ;
   WHILE l#NIL DO
      IF l^.vec=source
      THEN
         RETURN
      ELSE
         l := l^.next
      END
   END ;
   New(l) ;
   WITH l^ DO
      vec := source ;
      next := currentCoRoutine^.attached
   END ;
   currentCoRoutine^.attached := l ;
   signal (lock)
END ATTACH ;


PROCEDURE DETACH (source: INTERRUPTSOURCE);
  (* Dissociates the specified source of interrupts from the calling
     coroutine. *)
VAR
   l, m: SourceList ;
BEGIN
   localInit ;
   wait (lock) ;
   l := currentCoRoutine^.attached ;
   m := l ;
   WHILE l#NIL DO
      IF l^.vec=source
      THEN
         IF m=currentCoRoutine^.attached
         THEN
            currentCoRoutine^.attached := currentCoRoutine^.attached^.next
         ELSE
            m^.next := l^.next
         END ;
         Dispose(l) ;
         RETURN
      ELSE
         m := l ;
         l := l^.next
      END
   END ;
   signal (lock)
END DETACH ;


(*
   getAttached - returns the first COROUTINE associated with, source.
                 It returns NIL is no COROUTINE is associated with, source.
*)

PROCEDURE getAttached (source: INTERRUPTSOURCE) : COROUTINE ;
VAR
   l: SourceList ;
   c: COROUTINE ;
BEGIN
   c := head ;
   WHILE c#NIL DO
      l := c^.attached ;
      WHILE l#NIL DO
         IF l^.vec=source
         THEN
            RETURN( c )
         ELSE
            l := l^.next
         END
      END ;
      c := c^.next
   END ;
   RETURN( NIL )
END getAttached ;


PROCEDURE IsATTACHED (source: INTERRUPTSOURCE): BOOLEAN;
  (* Returns TRUE if and only if the specified source of interrupts is
     currently associated with a coroutine; otherwise returns FALSE.
  *)
VAR
   result: BOOLEAN ;
BEGIN
   localInit ;
   wait (lock) ;
   result := getAttached(source)#NIL ;
   signal (lock) ;
   RETURN result
END IsATTACHED ;


PROCEDURE HANDLER (source: INTERRUPTSOURCE) : COROUTINE;
  (* Returns the coroutine, if any, that is associated with the source
     of interrupts. The result is undefined if IsATTACHED(source) =
     FALSE.
  *)
VAR
   co: COROUTINE ;
BEGIN
   localInit ;
   wait (lock) ;
   co := getAttached(source) ;
   signal (lock) ;
   RETURN co
END HANDLER ;


PROCEDURE CURRENT () : COROUTINE ;
  (* Returns the identity of the calling coroutine. *)
BEGIN
   localInit ;
   RETURN currentCoRoutine
END CURRENT ;


PROCEDURE LISTEN (p: PROTECTION) ;
  (* Momentarily changes the protection of the calling coroutine to p. *)
BEGIN
   localInit ;
   Listen (FALSE, IOTransferHandler, MIN (PROTECTION))
END LISTEN ;


(*
   ListenLoop - should be called instead of users writing:

                LOOP
                   LISTEN
                END

                It performs the same function but yields
                control back to the underlying operating system.
                It also checks for deadlock.
                This will yield processor to the underlying
                operating system under GNU pth.
                This function returns when an interrupt occurs.
                (File descriptor becomes ready or time event expires).
*)

PROCEDURE ListenLoop ;
BEGIN
   localInit ;
   Listen (TRUE, IOTransferHandler, MIN (PROTECTION))
END ListenLoop ;


(*
   removeAttached - removes all sources of interrupt from COROUTINE, c.
*)

PROCEDURE removeAttached (c: COROUTINE) ;
VAR
   l: SourceList ;
BEGIN
   l := c^.attached ;
   WHILE l#NIL DO
      ExcludeVector(l^.vec) ;
      l := l^.next
   END
END removeAttached ;


(*
   IOTransferHandler - handles interrupts related to a pending IOTRANSFER.
*)

PROCEDURE IOTransferHandler (InterruptNo: CARDINAL;
                             Priority: CARDINAL ;
                             l: SourceList) ;
VAR
   ourself: SourceList ;
BEGIN
   localInit ;
   IF l=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'no coroutine attached to this interrupt vector which was initiated by IOTRANSFER')
   ELSE
      wait (lock) ;
      WITH l^ DO
         ourself := AttachVector(InterruptNo, chain) ;
         IF ourself#l
         THEN
            Halt(__FILE__, __LINE__, __FUNCTION__,
                 'inconsistancy of return result')
         END ;
         IF chain=NIL
         THEN
            removeAttached(curco)
         ELSE
            printf('odd vector has been chained\n')
         END ;
         ptrToTo^^.context := currentCoRoutine^.context ;
         TRANSFER (ptrToTo^, ptrToFrom^)
      END ;
      signal (lock)
   END
END IOTransferHandler ;


PROCEDURE PROT () : PROTECTION;
  (* Returns the protection of the calling coroutine. *)
BEGIN
   localInit ;
   RETURN currentInterruptLevel ()
END PROT ;


(*
   TurnInterrupts - switches processor interrupts to the protection
                    level, to.  It returns the old value.
*)

PROCEDURE TurnInterrupts (to: PROTECTION) : PROTECTION ;
VAR
   old: PROTECTION ;
BEGIN
   localInit ;
   old := turnInterrupts (to) ;
   Listen (FALSE, IOTransferHandler, to) ;
   RETURN old
END TurnInterrupts ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   freeList := NIL ;
   initCo := FALSE ;
   initMain := FALSE ;
   currentCoRoutine := NIL
END Init ;


BEGIN
   Init
END COROUTINES.
