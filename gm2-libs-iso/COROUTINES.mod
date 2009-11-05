(* Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE COROUTINES ;

FROM pth IMPORT pth_uctx_create, pth_uctx_make, pth_uctx_t,
                pth_uctx_save, pth_uctx_switch, pth_init ;

FROM SysVec IMPORT Listen, AttachVector,
                   IncludeVector, ExcludeVector ;

FROM Storage IMPORT ALLOCATE ;
FROM M2RTS IMPORT Halt ;
FROM libc IMPORT printf ;


CONST
   MinStack = 8 * 16 * 1024 ;

TYPE
   PtrToIOTransferState = POINTER TO IOTransferState ;
   IOTransferState      = RECORD
                             ptrToTo,
                             ptrToFrom: POINTER TO COROUTINE ;
                             next     : PtrToIOTransferState ;
                          END ;

   Status = (suspended, ready, new, running) ;

   COROUTINE = POINTER TO RECORD
                             context   : SYSTEM.ADDRESS ;
                             wspace    : SYSTEM.ADDRESS ;
                             nLocs     : CARDINAL ;
                             status    : Status ;
                             protection: PROTECTION ;
                          END ;


VAR
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
   localMain(to) ;
   IF to.context=from.context
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'error when attempting to context switch to the same process')
   END ;
   currentCoRoutine := to ;
   IF pth_uctx_switch(to.context, from.context)=0
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'an error as it was unable to change the user context')
   END
END TRANSFER ;


(*
   localMain - creates the holder for the main process.
*)

PROCEDURE localMain (VAR mainProcess: COROUTINE) ;
BEGIN
   IF NOT initMain
   THEN
      initMain := TRUE ;
      NEW(mainProcess) ;
      WITH mainProcess^ DO
         IF pth_uctx_create(ADR(context))=0
         THEN
            Halt(__FILE__, __LINE__, __FUNCTION__,
                 'unable to create context for main')
         END ;
         wspace     := NIL ;
         nLocs      := 0 ;
         status     := running ;
         protection := UnassignedPriority
      END
   END
END localMain ;


(*
   TurnInterrupts - switches processor interrupts to the priority, to.
                    It returns the old value.
*)

PROCEDURE TurnInterrupts (to: PRIORITY) : PRIORITY ;
VAR
   old: PRIORITY ;
BEGIN
   Listen(FALSE, IOTransferHandler, currentIntValue) ;
   old := currentIntValue ;
   currentIntValue := to ;
   Listen(FALSE, IOTransferHandler, currentIntValue) ;
   RETURN( old )
END TurnInterrupts ;


(*
   Finished - generates an error message. Modula-2 processes should never 
              terminate.
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
   END
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
   p: IOTransferState ;
   l: POINTER TO IOTransferState ;
BEGIN
   localMain(First) ;
   WITH p DO
      ptrToFrom := ADR(from) ;
      ptrToTo   := ADR(to) ;
      next      := AttachVector(InterruptNo, ADR(p))   (* --fixme-- *)
   END ;
   IncludeVector(InterruptNo) ;   (* --fixme-- *)
   TRANSFER(from, to)
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
   localInit ;
   Listen(FALSE, IOTransferHandler, MIN(PRIORITY))
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
   Listen(TRUE, IOTransferHandler, MIN(PRIORITY))
END ListenLoop ;


(*
   IOTransferHandler - handles interrupts related to a pending IOTRANSFER.
*)

PROCEDURE IOTransferHandler (InterruptNo: CARDINAL;
                             Priority: CARDINAL ;
                             l: PtrToIOTransferState) ;
VAR
   old: PtrToIOTransferState ;
BEGIN
   IF l=NIL
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'no processes attached to this interrupt vector which is associated with IOTRANSFER')
   ELSE
      WITH l^ DO
         old := AttachVector(InterruptNo, next) ;
         IF old#l
         THEN
            Halt(__FILE__, __LINE__, __FUNCTION__,
                 'inconsistancy of return result')
         END ;
         IF next=NIL
         THEN
            ExcludeVector(InterruptNo)
         ELSE
            printf('odd vector has been chained\n')
         END ;
         ptrToSecond^.context := currentContext ;
         TRANSFER(ptrToSecond^, ptrToFirst^)
      END
   END
END IOTransferHandler ;


PROCEDURE PROT () : PROTECTION;
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





(*
   TRANSFER - save the current volatile environment into, p1.
              Restore the volatile environment from, p2.
*)

PROCEDURE TRANSFER (VAR p1: PROCESS; p2: PROCESS) ;
VAR
   r: INTEGER ;
BEGIN
   localMain(p1) ;
   p1.ints := currentIntValue ;
   currentIntValue := p2.ints ;
   IF p1.context=p2.context
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'error when attempting to context switch to the same process')
   END ;
   (* r := printf('ctx\n') ; *)
   currentContext := p2.context ;
   IF pth_uctx_switch(p1.context, p2.context)=0
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'an error as it was unable to change the user context')
   END
END TRANSFER ;


(*
   NEWPROCESS - p is a parameterless procedure, a, is the origin of
                the workspace used for the process stack and containing
                the volatile environment of the process. n, is the amount
                in bytes of this workspace. new, is the new process.
*)

PROCEDURE NEWPROCESS (p: PROC; a: ADDRESS; n: CARDINAL; VAR new: PROCESS) ;
END NEWPROCESS ;


(*
   IOTRANSFER - saves the current volatile environment into, First,
                and restores volatile environment, Second.
                When an interrupt, InterruptNo, is encountered then
                the reverse takes place. (The then current volatile
                environment is shelved onto Second and First is resumed).

                NOTE: that upon interrupt the Second might not be the
                      same process as that before the original call to
                      IOTRANSFER.
*)

PROCEDURE IOTRANSFER (VAR First, Second: PROCESS; InterruptNo: CARDINAL) ;
VAR
   p: IOTransferState ;
   l: POINTER TO IOTransferState ;
BEGIN
   localMain(First) ;
   WITH p DO
      ptrToFirst  := ADR(First) ;
      ptrToSecond := ADR(Second) ;
      next        := AttachVector(InterruptNo, ADR(p))
   END ;
   IncludeVector(InterruptNo) ;
   TRANSFER(First, Second)
END IOTRANSFER ;







BEGIN
   currentContext := NIL ;
   initPthreads := FALSE ;
   initMain := FALSE ;
   currentIntValue := MIN(PRIORITY)
