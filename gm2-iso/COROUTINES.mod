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

TYPE
   COROUTINE = POINTER TO RECORD
                             
                          END ;

   INTERRUPTSOURCE = CARDINAL ;

PROCEDURE NEWCOROUTINE (procBody: PROC; workspace: SYSTEM.ADDRESS; size: CARDINAL;
                        VAR cr: COROUTINE; [initProtection: PROTECTION]);

  (* Creates a new coroutine whose body is given by procBody, and
     returns the identity  of the coroutine in cr.  workspace is a
     pointer to the work space allocated to the coroutine; size
     specifies the size of this workspace in terms of SYSTEM.LOC.

     The optarg, initProtection, may contain a single parameter which
     specifies the initial protection level of the coroutine.
  *)
BEGIN
   
END NEWCOROUTINE ;


PROCEDURE TRANSFER (VAR from: COROUTINE; to: COROUTINE);
  (* Returns the identity of the calling coroutine in from, and transfers control to
     the coroutine specified by to.
  *)
BEGIN
   
END TRANSFER ;


PROCEDURE IOTRANSFER (VAR from: COROUTINE; to: COROUTINE);
  (* Returns the identity of the calling coroutine in from and transfers control to
     the coroutine specified by to.  On occurrence of an interrupt, associated with the
     caller, control is transferred back to the caller, and the identity of the
     interrupted coroutine is returned in from.  The calling coroutine must be
     associated with a source of interrupts.
  *)
BEGIN
   
END IOTRANSFER ;
    

PROCEDURE ATTACH (source: INTERRUPTSOURCE);
  (* Associates the specified source of interrupts with the calling coroutine. *)
BEGIN
   
END ATTACH ;


PROCEDURE DETACH (source: INTERRUPTSOURCE);
  (* Dissociates the specified source of interrupts from the calling coroutine. *)
BEGIN
   
END DETACH ;


PROCEDURE IsATTACHED (source: INTERRUPTSOURCE): BOOLEAN;
  (* Returns TRUE if and only if the specified source of interrupts is currently
     associated with a coroutine; otherwise returns FALSE.
  *)
BEGIN
   
END IsATTACHED ;


PROCEDURE HANDLER (source: INTERRUPTSOURCE): COROUTINE;
  (* Returns the coroutine, if any, that is associated with the source of interrupts.
     The result is undefined if IsATTACHED(source) = FALSE.
  *)
BEGIN
   
END HANDLER ;


PROCEDURE CURRENT (): COROUTINE;
  (* Returns the identity of the calling coroutine. *)
BEGIN
   
END CURRENT ;


PROCEDURE LISTEN (p: PROTECTION);
  (* Momentarily changes the protection of the calling coroutine to p. *)
BEGIN
   
END LISTEN ;


PROCEDURE PROT (): PROTECTION;
  (* Returns the protection of the calling coroutine. *)
BEGIN
   
END PROT ;


END COROUTINES.
