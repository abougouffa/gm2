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

IMPLEMENTATION MODULE keyc ;


TYPE
   dictionary = POINTER TO RECORD
                              scope: node ;
			      : symbolTree ;
                           END ;


(*
   declare - declares an object of name, s, in the current scope.
             The (possibly translated) name is returned.
*)

PROCEDURE declare (s: String) : String ;
BEGIN
   RETURN s
END declare ;


(*
   declareGlobal - declare an object of name, s, in the global scope.
                   The (possibly translated) name is returned.
*)

PROCEDURE declareGlobal (s: String) : String ;
BEGIN
   RETURN s
END declareGlobal ;


(*
   enter - enter procedure scope, n.
*)

PROCEDURE enter (n: node) ;
BEGIN

END enter ;


(*
   leave - leave scope, n.
*)

PROCEDURE leave (n: node) ;
BEGIN

END leave ;


(*
   init -
*)

PROCEDURE init ;
BEGIN

   initKeywords ;
   initMacros
END init ;


BEGIN
   init
END keyc.
