(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                 2010
                 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE gm2 ;

(*
   Author     : Gaius Mulley
   Title      : gm2
   Date       : 1987  [$Date: 2013/02/11 14:45:17 $]
   SYSTEM     : UNIX (GNU Modula-2)
   Description: Main module of the compiler, collects arguments and
                starts the compilation.
   Version    : $Revision: 1.16 $
*)

FROM M2Comp IMPORT Compile ;
FROM DynamicStrings IMPORT String, KillString, InitStringCharStar ;


(*
   CompileFile - compile the filename.
*)

PROCEDURE CompileFile (filename: ADDRESS) ;
VAR
   f: String ;
BEGIN
   f := InitStringCharStar(filename) ;
   Compile(f) ;
   f := KillString(f) ;
END CompileFile


END gm2.
