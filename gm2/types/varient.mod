(* Copyright (C) 2001 Free Software Foundation, Inc. *)
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
MODULE varient ;



TYPE
   Operator = (add, mult, sub) ;

   One  = RECORD
             First,
             Second,
             Third: CHAR ;
          END ;

   Two  = RECORD
             ch: CHAR ;
             CASE ch OF

             'a': First: CARDINAL |
             'b': Second,
                  Third : CHAR

             END
          END ;

   Three = RECORD
              First,
              Second,
              Third : CARDINAL ;
           END ;

   Node = RECORD
             Op             : Operator ;
             CASE Op OF

             add        : Add    : One |
             mult       : Mult   : Two |
             sub        : Sub    : Three

             END
          END ;


VAR
   t: Node ;
   i: INTEGER ;
BEGIN
(*
   t.Op := add ;
   t.Add.First := 'a' ;
*)
   t.Add.Third := 'b' ;

(*
   t.Op := mult ;

   t.Mult.ch := 'b' ;
   t.Mult.First  := 1234
*)
   i := 9
END varient.