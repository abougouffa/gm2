(* Storage.mod.

Copyright (C) 2005-2019 Free Software Foundation, Inc.
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

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE Storage;

(*
 *  this is a replacement for the ULM storage module, which utilizes the
 *  C library routines, malloc and free.
 *  Written by Gaius Mulley <gaius@glam.ac.uk>
 *)

FROM libc IMPORT malloc, free ;
FROM SysPanic IMPORT Panic ;

VAR
   mallocFailure: (returnNIL, abort) ;


PROCEDURE DEALLOCATE (VAR ptr: ADDRESS; size: CARDINAL) ;
BEGIN
   free(ptr)
END DEALLOCATE ;


PROCEDURE ALLOCATE (VAR ptr: ADDRESS; size: CARDINAL) ;
BEGIN
   ptr := malloc(size);
   IF ptr=NIL
   THEN
      IF mallocFailure#returnNIL
      THEN
         Panic("No space available.")
      END
   END
END ALLOCATE ;


PROCEDURE Setmode(m: CARDINAL);
BEGIN
   IF m=1
   THEN
      mallocFailure := abort
   ELSIF m=2
   THEN
      mallocFailure := returnNIL
   END
END Setmode;


BEGIN
   Setmode(1)
END Storage.
