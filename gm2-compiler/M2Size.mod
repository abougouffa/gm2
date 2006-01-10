(* Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE M2Size ;

FROM NameKey IMPORT MakeKey ;
FROM M2Base IMPORT Cardinal ;

FROM SymbolTable IMPORT NulSym, MakeProcedure, PutFunction,
                        AddSymToModuleScope, GetCurrentScope ;


(*
   MakeSize - creates and declares the standard function SIZE.
*)

PROCEDURE MakeSize ;
BEGIN
   IF Size=NulSym
   THEN
      Size := MakeProcedure(MakeKey('SIZE')) ;       (* Function        *)
      PutFunction(Size, Cardinal)                    (* Return Type     *)
                                                     (* Cardinal        *)
   ELSE
      AddSymToModuleScope(GetCurrentScope(), Size)
   END
END MakeSize ;


BEGIN
   Size := NulSym
END M2Size.
