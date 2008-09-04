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

FROM EXCEPTIONS IMPORT ExceptionNumber, RAISE,
                       AllocateSource, ExceptionSource, IsCurrentSource,
                       IsExceptionalExecution ;


TYPE
   BinaryTree = POINTER TO RECORD
                   Address : SYSTEM.ADDRESS ;  (* The sorted entity *)
                   Amount  : CARDINAL ;        (* amount of storage *)
                   Left    ,
                   Right   : BinaryTree
                END ;


PROCEDURE InitTree (VAR t: BinaryTree) ;
BEGIN
   t := malloc (TSIZE(t^)) ;
   WITH t^ DO
      Left := NIL ;
      Right := NIL
   END
END InitTree ;


PROCEDURE GetSymKey (t: BinaryTree; a: SYSTEM.ADDRESS) : CARDINAL ;
VAR
   father,
   child : BinaryTree ;
   SymKey: CARDINAL ;
BEGIN
   FindNodeAndParentInTree (t, a, child, father) ;
   IF child=NIL
   THEN
      RETURN (0)
   ELSE
      RETURN (child^.Amount)
   END
END GetSymKey ;


PROCEDURE PutSymKey (t: BinaryTree; a: SYSTEM.ADDRESS; amount: CARDINAL) ;
VAR
   father,
   child : BinaryTree ;
BEGIN
   FindNodeAndParentInTree (t, a, child, father) ;
   IF child=NIL
   THEN
      (* no child found, now is, a, less than father or greater? *)
      IF father=t
      THEN
         (* empty tree, add it to the left branch of t *)
         child := malloc (TSIZE (child^)) ;
         father^.Left := child
      ELSE
         IF a<father^.Address
         THEN
            child := malloc (TSIZE (child^)) ;
            father^.Left := child
         ELSIF a>father^.Address
         THEN
            child := malloc (TSIZE (child^)) ;
            father^.Right := child
         END
      END ;
      WITH child^ DO
         Right   := NIL ;
         Left    := NIL ;
         Address := a ;
         Amount  := amount
      END
   ELSE
      Halt (__FILE__, __LINE__, __FUNCTION__,
            'storage component already stored')
   END
END PutSymKey ;


(*
   DelSymKey - deletes an entry in the binary tree.

               NB in order for this to work we must ensure that the InitTree sets
               both Left and Right to NIL.
*)

PROCEDURE DelSymKey (t: BinaryTree; a: SYSTEM.ADDRESS) ;
VAR
   i, child, father: BinaryTree ;
BEGIN
   FindNodeAndParentInTree (t, a, child, father) ;  (* find father and child of the node *)
   IF (child#NIL) AND (child^.Address=a)
   THEN
      (* Have found the node to be deleted *)
      IF father^.Right=child
      THEN
         (* Node is child and this is greater than the father. *)
         (* Greater being on the right.                        *)
         (* Connect child^.Left onto the father^.Right.        *)
         (* Connect child^.Right onto the end of the right     *)
         (* most branch of child^.Left.                        *)
         IF child^.Left#NIL
         THEN
            (* Scan for Right most node of child^.Left *)
            i := child^.Left ;
            WHILE i^.Right#NIL DO
               i := i^.Right
            END ;
            i^.Right      := child^.Right ;
            father^.Right := child^.Left
         ELSE
            (* No child^.Left node therefore link over child   *)
            (* (as in a single linked list) to child^.Right    *)
            father^.Right := child^.Right
         END ;
         free (child)
      ELSE
         (* Assert that father^.Left=child will always be true *)
         (* Perform exactly the mirror image of the above code *)

         (* Connect child^.Right onto the father^.Left.        *)
         (* Connect child^.Left onto the end of the Left most  *)
         (* branch of child^.Right                             *)
         IF child^.Right#NIL
         THEN
            (* Scan for Left most node of child^.Right *)
            i := child^.Right ;
            WHILE i^.Left#NIL DO
               i := i^.Left
            END ;
            i^.Left      := child^.Left ;
            father^.Left := child^.Right
         ELSE
            (* No child^.Right node therefore link over c      *)
            (* (as in a single linked list) to child^.Left.    *)
            father^.Left := child^.Left
         END ;
         free (child)
      END
   ELSE
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'trying to delete a storage component that is not in the tree')
   END
END DelSymKey ;


(*
   FindNodeAndParentInTree - find a node, child, in a binary tree, t, with name equal to n.
                             if an entry is found, father is set to the node above child.
*)

PROCEDURE FindNodeAndParentInTree (t: BinaryTree; a: SYSTEM.ADDRESS;
                                   VAR child, father: BinaryTree) ;
BEGIN
   (* remember to skip the sentinal value and assign father and child *)
   father := t ;
   IF t=NIL
   THEN
      Halt (__FILE__, __LINE__, __FUNCTION__,
            'parameter t should never be NIL')
   END ;
   child := t^.Left ;
   IF child#NIL
   THEN
      REPEAT
         IF a<child^.Address
         THEN
            father := child ;
            child := child^.Left
         ELSIF a>child^.Address
         THEN
            father := child ;
            child := child^.Right
         END
      UNTIL (child=NIL) OR (a=child^.Address)
   END
END FindNodeAndParentInTree ;


PROCEDURE ALLOCATE (VAR addr: SYSTEM.ADDRESS; amount: CARDINAL) ;
BEGIN
   addr := malloc (amount) ;
   IF addr#NIL
   THEN
      PutSymKey (storageTree, addr, amount)
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
   RETURN(IsCurrentSource (storageException) )
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
      a := GetSymKey (storageTree, addr) ;
      IF a=0
      THEN
         RAISE (storageException, ORD(pointerToUnallocatedStorage), 'trying to deallocate memory which has never been allocated') ;
         RETURN (FALSE)
      END ;
      IF a#amount
      THEN
         RAISE (storageException, ORD(wrongStorageToUnallocate), 'wrong amount of storage being deallocated') ;
         RETURN (FALSE)
      END
   END ;
   DelSymKey (storageTree, addr) ;
   RETURN (TRUE)
END VerifyDeallocate ;


(*
   Init - 
*)

PROCEDURE Init ;
BEGIN
   AllocateSource (storageException) ;
   InitTree (storageTree)
END Init ;


VAR
   storageException: ExceptionSource ;
   currentException: StorageExceptions ;
   storageTree     : BinaryTree ;
BEGIN
   Init
END Storage.
