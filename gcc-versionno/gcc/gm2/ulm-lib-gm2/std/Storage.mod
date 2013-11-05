(* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
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
