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
MODULE testcpp2 ;


FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM StdIO IMPORT Write ;


CONST
#if defined(SMALL)
   MaxFileLength = 14 ;
#else
   MaxFileLength = 1024 ;
#endif

(*
   let`s make this as hard as we can..   (* *) (* *)
   ' foo bar '
   ` hmm
   now what happens here /* is this seen or not ? /* and this */
   //  do we see this? \
*)

BEGIN
   WriteString('the constant value after is: ') ;
   WriteCard(MaxFileLength, 0) ;
   WriteLn ;
   Write('\') ;
   WriteLn
END testcpp2.
