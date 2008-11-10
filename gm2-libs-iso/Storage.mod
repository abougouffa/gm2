(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
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

IMPLEMENTATION MODULE Storage ;

FROM libc IMPORT malloc, free ;
FROM M2RTS IMPORT Halt ;
FROM SYSTEM IMPORT TSIZE ;
FROM M2EXCEPTION IMPORT M2Exceptions ;
FROM RTentity IMPORT Group, InitGroup, GetKey, PutKey, DelKey, IsIn ;

FROM EXCEPTIONS IMPORT ExceptionNumber, RAISE,
                       AllocateSource, ExceptionSource, IsCurrentSource,
                       IsExceptionalExecution ;



PROCEDURE ALLOCATE (VAR addr: SYSTEM.ADDRESS; amount: CARDINAL) ;
BEGIN
   addr := malloc (amount) ;
   IF addr#NIL
   THEN
      PutKey (storageTree, addr, amount)
   END
END ALLOCATE ;


PROCEDURE DEALLOCATE (VAR addr: SYSTEM.ADDRESS; amount: CARDINAL) ;
BEGIN
   IF VerifyDeallocate (addr, amount)
   THEN
      free (addr)
   END
END DEALLOCATE ;


PROCEDURE IsStorageException () : BOOLEAN;
BEGIN
   RETURN( IsCurrentSource (storageException) )
END IsStorageException ;


PROCEDURE StorageException () : StorageExceptions ;
BEGIN
   IF NOT IsExceptionalExecution ()
   THEN
      RAISE (storageException, ORD (functionException), 'no storage exception raised')
   END ;
   RETURN (currentException)
END StorageException ;


(*
   VerifyDeallocate - 
*)

PROCEDURE VerifyDeallocate (addr: SYSTEM.ADDRESS; amount: CARDINAL) : BOOLEAN ;
VAR
   a: CARDINAL ;
BEGIN
   IF addr=NIL
   THEN
      RAISE (storageException, ORD(nilDeallocation), 'deallocating pointer whose value is NIL') ;
      RETURN (FALSE)
   ELSE
      IF NOT IsIn(storageTree, addr)
      THEN
         RAISE (storageException, ORD(pointerToUnallocatedStorage), 'trying to deallocate memory which has never been allocated') ;
         RETURN (FALSE)
      END ;
      a := GetKey (storageTree, addr) ;
      IF a#amount
      THEN
         RAISE (storageException, ORD(wrongStorageToUnallocate), 'wrong amount of storage being deallocated') ;
         RETURN (FALSE)
      END
   END ;
   DelKey (storageTree, addr) ;
   RETURN (TRUE)
END VerifyDeallocate ;


(*
   Init - 
*)

PROCEDURE Init ;
BEGIN
   storageTree := InitGroup () ;
   AllocateSource (storageException)
END Init ;


VAR
   storageException: ExceptionSource ;
   currentException: StorageExceptions ;
   storageTree     : Group ;
BEGIN
   Init
END Storage.
