(* Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc. *)
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
IMPLEMENTATION MODULE SysStorage ;

FROM libc IMPORT malloc, free, realloc ;
FROM Debug IMPORT Halt ;



PROCEDURE ALLOCATE (VAR a: ADDRESS ; Size: CARDINAL) ;
BEGIN
   a := malloc(Size) ;
   IF a=NIL
   THEN
      Halt('out of memory error', __LINE__, __FILE__)
   END
END ALLOCATE ;


PROCEDURE DEALLOCATE (VAR a: ADDRESS; Size: CARDINAL);
BEGIN
   free(a) ;
   a := NIL
END DEALLOCATE ;


(*
   REALLOCATE - attempts to reallocate storage. The address,
                a, should either be NIL in which case ALLOCATE
                is called, or alternatively it should have already
                been initialized by ALLOCATE. The allocated storage
                is resized accordingly.
*)

PROCEDURE REALLOCATE (VAR a: ADDRESS; Size: CARDINAL) ;
BEGIN
   IF a=NIL
   THEN
      ALLOCATE(a, Size)
   ELSE
      a := realloc(a, Size) ;
      IF a=NIL
      THEN
         Halt('out of memory error', __LINE__, __FILE__)
      END
   END
END REALLOCATE ;


PROCEDURE Available (Size: CARDINAL) : BOOLEAN;
VAR
   a: ADDRESS ;
BEGIN
   a := malloc(Size) ;
   IF a=NIL
   THEN
      RETURN( FALSE )
   ELSE
      free(a) ;
      RETURN( TRUE )
   END
END Available ;


(*
   Init - 
*)

PROCEDURE Init ;
BEGIN
END Init ;


END SysStorage.
