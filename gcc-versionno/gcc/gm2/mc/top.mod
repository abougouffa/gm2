(* Copyright (C) 2015 Free Software Foundation, Inc.  *)
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
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

MODULE top ;

FROM mcOptions IMPORT handleOptions ;
FROM mcComp IMPORT compile ;
FROM M2RTS IMPORT ExitOnHalt ;


(*
   init - translate the source file after handling all the
          program arguments.
*)

PROCEDURE init ;
BEGIN
   ExitOnHalt (1) ;
   compile (handleOptions ())
END init ;


BEGIN
   init
END top.
