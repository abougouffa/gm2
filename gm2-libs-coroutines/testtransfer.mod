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

MODULE testtransfer ;

FROM SYSTEM IMPORT ADDRESS, PROCESS, NEWPROCESS, TRANSFER ;
FROM Storage IMPORT ALLOCATE ;
FROM libc IMPORT printf, exit ;


CONST
   MaxStack  = 16 * 1024 * 8 ;
   Debugging = FALSE ;
   MaxCount  = 1000000 ;


PROCEDURE p1 ;
VAR
   r: INTEGER ;
BEGIN
   LOOP
      IF Debugging
      THEN
         r := printf('hello world process 1\n')
      END ;
      TRANSFER(P1, P2) ;
      IF Debugging
      THEN
         r := printf('after TRANSFER in process 2\n')
      END
   END
END p1 ;


PROCEDURE p2 ;
VAR
   r: INTEGER ;
BEGIN
   LOOP
      IF Debugging
      THEN
         r := printf('hello world process 2  (%d)\n', count)
      END ;
      TRANSFER(P2, P1) ;
      IF Debugging
      THEN
         r := printf('after TRANSFER in process 2\n')
      END ;
      INC(count) ;
      IF count=MaxCount
      THEN
         r := printf('completed %d TRANSFERs successfully\n', count) ;
         exit(0)
      END
   END
END p2 ;


VAR
   MainP, P1, P2: PROCESS ;
   S1, S2, S3   : ADDRESS ;
   count        : CARDINAL ;
BEGIN
   count := 0 ;
   ALLOCATE(S1, MaxStack) ;
   ALLOCATE(S2, MaxStack) ;
   ALLOCATE(S3, MaxStack) ;
   NEWPROCESS(p1, S1, MaxStack, P1) ;
   NEWPROCESS(p2, S2, MaxStack, P2) ;
   TRANSFER(MainP, P1)
END testtransfer.
