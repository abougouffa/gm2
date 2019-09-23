(* SysStorage.def provides dynamic allocation for the system components.

Copyright (C) 2001-2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  *)

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
