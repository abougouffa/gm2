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

MODULE varient4 ;

TYPE
   this = RECORD
             CASE tag: CARDINAL OF
             1: foo: CARDINAL ;
                  CASE bar: BOOLEAN OF
                  TRUE : bt: INTEGER |
                  FALSE: bf: CARDINAL
                  END |
             2: an: REAL |
             ELSE
             END
          END ;

VAR
   hmm: this ;
   j  : CARDINAL ;
BEGIN
   hmm.foo := 99 ;
(*
   hmm.tag := 'a' ;
   hmm.bar := TRUE ;
   hmm.bt  := -1
*)
   j := SIZE(this)
END varient4.
