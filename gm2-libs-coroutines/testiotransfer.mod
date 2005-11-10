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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

MODULE testiotransfer ;


FROM SYSTEM IMPORT ADDRESS, PROCESS, TRANSFER, NEWPROCESS,
                   BYTE, LISTEN, IOTRANSFER, PRIORITY ;

FROM SysVec IMPORT InitTimeVector, ReArmTimeVector ;
FROM Storage IMPORT ALLOCATE ;
FROM libc IMPORT printf, exit ;


PROCEDURE Timer ;
CONST
   MaxCount = 500 ;
VAR
   r: INTEGER ;
   v: CARDINAL ;
   c: CARDINAL ;
BEGIN
   r := printf('clock starting\n') ;
   v := InitTimeVector(500, 0, MAX(PRIORITY)) ;
   c := 0 ;
   LOOP
      INC(c) ;
      r := printf('%d\n', c) ;
      IOTRANSFER(p2, p1, v) ;
      ReArmTimeVector(v, 500, 0) ;
      IF c=MaxCount
      THEN
         exit(0)
      END
   END
END Timer ;


CONST
   MaxStack = 16 * 1024 * 1024 ;

VAR
   r      : INTEGER ;
   s1, s2 : ADDRESS ;
   p1, p2 : PROCESS ;
BEGIN
   ALLOCATE(s1, MaxStack) ;
   ALLOCATE(s2, MaxStack) ;
   NEWPROCESS(Timer, s2, MaxStack, p2) ;
   r := printf('now to TRANSFER...\n') ;
   TRANSFER(p1, p2) ;
   r := printf('now to LISTEN\n') ;
   LOOP
      LISTEN
   END
END testiotransfer.