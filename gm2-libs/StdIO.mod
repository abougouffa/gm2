(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA *)

IMPLEMENTATION MODULE StdIO ;

IMPORT IO ;


CONST
   MaxStack = 40 ;

VAR
   Stack   : ARRAY [0..MaxStack] OF ProcWrite ;
   StackPtr: CARDINAL ;


(*
   Read - is the generic procedure that all higher application layers
          should use to receive a character.
*)

PROCEDURE Read (VAR ch: CHAR) ;
BEGIN
   IO.Read(ch)
END Read ;


(*
   Write - is the generic procedure that all higher application layers
           should use to emit a character.
*)
 
PROCEDURE Write (ch: CHAR) ;
BEGIN
   Stack[StackPtr](ch)
END Write ;


(*
   PushOutput - pushes the current Write procedure onto a stack,
                any future references to Write will actually invoke
                procedure, p.
*)

PROCEDURE PushOutput (p: ProcWrite) ;
BEGIN
   IF StackPtr=MaxStack
   THEN
      HALT
   ELSE
      INC(StackPtr) ;
      Stack[StackPtr] := p
   END
END PushOutput ;


(*
   PopOutput - restores Write to use the previous output procedure.
*)

PROCEDURE PopOutput ;
BEGIN
   IF StackPtr=1
   THEN
      HALT
   ELSE
      DEC(StackPtr)
   END
END PopOutput ;


(*
   GetCurrentOutput - returns the current output procedure.
*)

PROCEDURE GetCurrentOutput () : ProcWrite ;
BEGIN
   IF StackPtr>0
   THEN
      RETURN( Stack[StackPtr] )
   ELSE
      HALT
   END
END GetCurrentOutput ;

 
BEGIN
   StackPtr := 0 ;
   PushOutput(IO.Write)
END StdIO.
