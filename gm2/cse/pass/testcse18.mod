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
MODULE testcse18 ;

CONST
   NulSym = 0 ;

PROCEDURE GetScopeSym (n: CARDINAL) : CARDINAL ;
BEGIN
   RETURN( 1 )
END GetScopeSym ;


PROCEDURE GetSym (Name: CARDINAL) : CARDINAL ;
VAR
   Sym        : CARDINAL ;
   OldScopePtr: CARDINAL ;
BEGIN
   Sym := GetScopeSym(Name) ;
   IF Sym=NulSym
   THEN
      (* Check default base types for symbol *)
      OldScopePtr := ScopePtr ;  (* Save ScopePtr *)
      ScopePtr := BaseScopePtr ; (* Alter ScopePtr to point to top of BaseModule *)
      Sym := GetScopeSym(Name) ; (* Search BaseModule for Name *)
      ScopePtr := OldScopePtr    (* Restored ScopePtr *)
   END ;
   RETURN( Sym )
END GetSym ;


VAR
   sym,
   BaseScopePtr,
   OldScopePtr,
   ScopePtr,
   Sym         : CARDINAL ;
BEGIN
   sym := GetSym(123)
END testcse18.