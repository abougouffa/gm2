(* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA 02110-1301, USA *)

IMPLEMENTATION MODULE SYSTEM ;

FROM pth IMPORT pth_uctx_create, pth_uctx_make, pth_uctx_t,
                pth_uctx_save, pth_uctx_switch, pth_init ;

FROM RTint IMPORT Listen, AttachVector,
                  IncludeVector, ExcludeVector ;

FROM Storage IMPORT ALLOCATE ;
FROM M2RTS IMPORT Halt ;
FROM libc IMPORT printf ;


CONST
   MinStack = 8 * 16 * 1024 ;

TYPE
   PtrToIOTransferState = POINTER TO IOTransferState ;
   IOTransferState      = RECORD
                             ptrToFirst,
                             ptrToSecond: POINTER TO PROCESS ;
                             next       : PtrToIOTransferState ;
                          END ;

VAR
   currentContext,
   illegalFinish  : ADDRESS ;
   initMain,
   initPthreads   : BOOLEAN ;
   currentIntValue: CARDINAL ;


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
TYPE
   ThreadProcess = PROCEDURE (ADDRESS) ;
VAR
   ctx: ADDRESS ;
   tp : ThreadProcess ;
BEGIN
   localInit ;
   tp := ThreadProcess(p) ;
   IF pth_uctx_create(ADR(ctx))=0
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'unable to create user context')
   END ;
   IF pth_uctx_make(ctx, a, n, NIL, tp, NIL, illegalFinish)=0
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'unable to make user context')
   END ;
   WITH new DO
      context := ctx ;
      ints    := currentIntValue ;
   END
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


(*
   LISTEN - briefly listen for any interrupts.
*)

PROCEDURE LISTEN ;
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
                operating system under GNU pthreads.
                This function returns when an interrupt occurs.
                (File descriptor becomes ready or time event expires).
*)

PROCEDURE ListenLoop ;
BEGIN
   localInit ;
   Listen(TRUE, IOTransferHandler, MIN(PROTECTION))
END ListenLoop ;


(*
   TurnInterrupts - switches processor interrupts to the
                    protection level, to.  It returns the old value.
*)

PROCEDURE TurnInterrupts (to: PROTECTION) : PROTECTION ;
VAR
   old: PROTECTION ;
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


(*
   localMain - creates the holder for the main process.
*)

PROCEDURE localMain (VAR mainProcess: PROCESS) ;
BEGIN
   IF NOT initMain
   THEN
      initMain := TRUE ;
      IF pth_uctx_create(ADR(currentContext))=0
      THEN
         Halt(__FILE__, __LINE__, __FUNCTION__,
              'unable to create context for main')
      END ;
      WITH mainProcess DO
         context := currentContext ;
         ints := currentIntValue
      END
   END
END localMain ;


BEGIN
   currentContext := NIL ;
   initPthreads := FALSE ;
   initMain := FALSE ;
   currentIntValue := MIN(PROTECTION)
END SYSTEM.
