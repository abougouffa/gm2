(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
IMPLEMENTATION MODULE FifoQueue ;


FROM Lists IMPORT List, InitList, PutItemIntoList, GetItemFromList ;

VAR
   Queue: List ;
   Out  : CARDINAL ;


(*
   PutIntoFifoQueue - places a CARDINAL number, c, into a fifo queue.
*)

PROCEDURE PutIntoFifoQueue (c: CARDINAL) ;
BEGIN
   PutItemIntoList(Queue, c)
END PutIntoFifoQueue ;


(*
   GetFromFifoQueue - retrieves a CARDINAL number, c, from a fifo queue.
*)

PROCEDURE GetFromFifoQueue (VAR c: CARDINAL) ;
BEGIN
   INC(Out) ;
   c := GetItemFromList(Queue, Out)
END GetFromFifoQueue ;


(*
   InitFifoQueue - initialize the fifo queue.
*)

PROCEDURE InitFifoQueue ;
BEGIN
   InitList(Queue) ;
   Out := 0
END InitFifoQueue ;


BEGIN
   InitFifoQueue
END FifoQueue.
