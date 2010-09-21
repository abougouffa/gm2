(* Copyright (C) 2008, 2009, 2010 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE M2EXCEPTION ;

FROM SYSTEM IMPORT ADR ;
FROM RTExceptions IMPORT EHBlock, GetExceptionBlock, GetNumber, Raise,
                         SetExceptionBlock, InitExceptionBlock ;


PROCEDURE M2Exception (): M2Exceptions;
  (* If the current coroutine is in the exceptional execution state because of the raising
     of a language exception, returns the corresponding enumeration value, and otherwise
     raises an exception.
  *)
VAR
   e: EHBlock ;
   n: CARDINAL ;
BEGIN
   e := GetExceptionBlock() ;
   n := GetNumber(e) ;
   IF n=MAX(CARDINAL)
   THEN
      Raise(ORD(exException), ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
            ADR('current coroutine is not in the exceptional execution state'))
   ELSE
      RETURN( VAL(M2Exceptions, n) )
   END
END M2Exception ;


PROCEDURE IsM2Exception (): BOOLEAN;
  (* If the current coroutine is in the exceptional execution state because of the raising
     of a language exception, returns TRUE, and otherwise returns FALSE.
  *)
VAR
   e: EHBlock ;
BEGIN
   e := GetExceptionBlock() ;
   RETURN( GetNumber(e)#MAX(CARDINAL) )
END IsM2Exception ;


BEGIN
   SetExceptionBlock(InitExceptionBlock())
END M2EXCEPTION.
