(* Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
                 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE COROUTINES ;

FROM pth IMPORT pth_uctx_create, pth_uctx_make, pth_uctx_t,
                pth_uctx_save, pth_uctx_switch, pth_init ;

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


CONST
   MinStack = 8 * 16 * 1024 ;

TYPE
   Status = (suspended, ready, new, running) ;

   COROUTINE = POINTER TO RECORD
                             context   : SYSTEM.ADDRESS ;
                             ehblock   : EHBlock ;
                             inexcept  : BOOLEAN ;
                             source    : ExceptionSource ;
                             wspace    : SYSTEM.ADDRESS ;
                             nLocs     : CARDINAL ;
                             status    : Status ;
                             protection: PROTECTION ;
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
   initPthreads     : BOOLEAN ;



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
TYPE
   ThreadProcess = PROCEDURE (ADDRESS) ;
VAR
   ctx: ADDRESS ;
   tp : ThreadProcess ;
BEGIN
   localInit ;
   tp := ThreadProcess(procBody) ;
   IF pth_uctx_create(ADR(ctx))=0
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'unable to create user context')
   END ;
   IF pth_uctx_make(ctx, workspace, size, NIL, tp, NIL, illegalFinish)=0
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'unable to make user context')
   END ;
   NEW(cr) ;
   IF initProtection=UnassignedPriority
   THEN
      initProtection := PROT()
   END ;
   WITH cr^ DO
      context    := ctx ;
      ehblock    := InitExceptionBlock() ;
      inexcept   := FALSE ;
      source     := NIL ;
      wspace     := workspace ;
      nLocs      := size ;
      status     := new ;
      protection := initProtection ;
      attached   := NIL ;
      next       := head ;
   END ;
   head := cr
END NEWCOROUTINE ;


PROCEDURE TRANSFER (VAR from: COROUTINE; to: COROUTINE);
  (* Returns the identity of the calling coroutine in from, and
     transfers control to the coroutine specified by to.
  *)
BEGIN
   localInit ;
   from := currentCoRoutine ;
   IF to^.context=from^.context
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'error when attempting to context switch to the same process')
   END ;
   from^.inexcept := SetExceptionState(to^.inexcept) ;
   from^.source := GetExceptionSource() ;
   currentCoRoutine := to ;
   SetExceptionBlock(currentCoRoutine^.ehblock) ;
   SetExceptionSource(currentCoRoutine^.source) ;
   IF pth_uctx_switch(from^.context, to^.context)=0
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'an error as it was unable to change the user context')
   END
END TRANSFER ;


(*
   localMain - creates the holder for the main process.
*)

PROCEDURE localMain ;
BEGIN
   IF NOT initMain
   THEN
      initMain := TRUE ;
      NEW(currentCoRoutine) ;
      WITH currentCoRoutine^ DO
         IF pth_uctx_create(ADR(context))=0
         THEN
            Halt(__FILE__, __LINE__, __FUNCTION__,
                 'unable to create context for main')
         END ;
         ehblock    := GetExceptionBlock() ;
         inexcept   := IsInExceptionState() ;
         source     := GetExceptionSource() ;
         wspace     := NIL ;
         nLocs      := 0 ;
         status     := running ;
         protection := UnassignedPriority ;
         attached   := NIL ;
         next       := head
      END ;
      head := currentCoRoutine
   END
END localMain ;


(*
   Finished - generates an error message. Modula-2 processes
              should never terminate.
*)

PROCEDURE Finished (p: ADDRESS) ;
BEGIN
   Halt(__FILE__, __LINE__, __FUNCTION__, 'process terminated illegally')
END Finished ;


(*
   localInit - checks to see whether we need to initialize libpth.
*)

PROCEDURE localInit ;
BEGIN
   IF NOT initPthreads
   THEN
      IF pth_init()=0
      THEN
         Halt(__FILE__, __LINE__, __FUNCTION__,
              'failed to initialize pthreads')
      END ;
      initPthreads := TRUE ;
      IF pth_uctx_create(ADR(illegalFinish))=0
      THEN
         Halt(__FILE__, __LINE__, __FUNCTION__,
              'unable to create user context')
      END ;
      IF pth_uctx_make(illegalFinish, NIL, MinStack, NIL, Finished, NIL, NIL)=0
      THEN
         Halt(__FILE__, __LINE__, __FUNCTION__, 'unable to make user context')
      END
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
   l: SourceList ;
BEGIN
   localInit ;
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
   TRANSFER(from, to)
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
   currentCoRoutine^.attached := l
END ATTACH ;


PROCEDURE DETACH (source: INTERRUPTSOURCE);
  (* Dissociates the specified source of interrupts from the calling
     coroutine. *)
VAR
   l, m: SourceList ;
BEGIN
   localInit ;
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
   END
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
BEGIN
   localInit ;
   RETURN getAttached(source)#NIL
END IsATTACHED ;


PROCEDURE HANDLER (source: INTERRUPTSOURCE) : COROUTINE;
  (* Returns the coroutine, if any, that is associated with the source
     of interrupts. The result is undefined if IsATTACHED(source) =
     FALSE.
  *)
BEGIN
   localInit ;
   RETURN getAttached(source)
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
   Listen(FALSE, IOTransferHandler, MIN(PROTECTION))
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
   Listen(TRUE, IOTransferHandler, MIN(PROTECTION))
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
         TRANSFER(ptrToTo^, ptrToFrom^)
      END
   END
END IOTransferHandler ;


PROCEDURE PROT () : PROTECTION;
  (* Returns the protection of the calling coroutine. *)
BEGIN
   localInit ;
   RETURN currentCoRoutine^.protection
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
   old := currentCoRoutine^.protection ;
   Listen(FALSE, IOTransferHandler, old) ;
   currentCoRoutine^.protection := to ;
   Listen(FALSE, IOTransferHandler, to) ;
   RETURN old
END TurnInterrupts ;


(*
   Init - 
*)

PROCEDURE Init ;
BEGIN
   freeList := NIL ;
   initPthreads := FALSE ;
   initMain := FALSE ;
   currentCoRoutine := NIL
END Init ;


BEGIN
   Init
END COROUTINES.
