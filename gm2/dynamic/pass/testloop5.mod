(* Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc. *)
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
MODULE testloop5 ;


FROM StrIO IMPORT WriteString, WriteLn ;

TYPE
   PtrToNode = POINTER TO CARDINAL ;

VAR
   RemoveList: PtrToNode ;


(*
   FindNextUsedQuad - returns TRUE if node, q, is on a list defined by, p.
*)

PROCEDURE IsItemOnList (p: PtrToNode) : BOOLEAN ;
BEGIN
   WHILE p^#0 DO
   END ;
   RETURN( FALSE )
END IsItemOnList ;


BEGIN
   RemoveList := NIL ;
   IF IsItemOnList(RemoveList)
   THEN
   END
END testloop5.