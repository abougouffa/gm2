(* Copyright (C) 2018
                 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE ObjectFiles ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM DynamicStrings IMPORT Dup, Mark, string ;
FROM Indexing IMPORT Index, InitIndex, KillIndex, GetIndice, PutIndice,
                     HighIndice, LowIndice, InBounds, IsIndiceInIndex,
                     RemoveIndiceFromIndex, IncludeIndiceIntoIndex,
                     ForeachIndiceInIndexDo ;

FROM wrapc IMPORT fileinode ;
FROM libc IMPORT open, close ;
FROM M2Printf IMPORT fprintf1, fprintf0 ;
FROM FIO IMPORT StdErr ;


CONST
   UNIXREADONLY = 0 ;
   Debugging    = FALSE ;

TYPE
   FileObject = POINTER TO RECORD
                              name : String ;
                              inodeLow, inodeHigh: CARDINAL ;
                           END ;

   FileObjects = POINTER TO RECORD
                               objects: Index ;
                            END ;


(*
   RegisterModuleObject - returns TRUE if location has not already been registered.
*)

PROCEDURE RegisterModuleObject (fo: FileObjects; location: String) : BOOLEAN ;
VAR
   p: FileObject ;
   r,
   f: INTEGER ;
BEGIN
   IF Debugging
   THEN
      fprintf1 (StdErr, "first time file %s has been registered... ", location)
   END ;
   IF NOT IsRegistered (fo, location)
   THEN
      NEW (p) ;
      p^.name := Dup (location) ;
      f := open (string (location), UNIXREADONLY, 0) ;
      IF fileinode (f, p^.inodeLow, p^.inodeHigh) = 0
      THEN
         r := close (f) ;
         IncludeIndiceIntoIndex (fo^.objects, p) ;
         IF Debugging
         THEN
            fprintf0 (StdErr, " yes\n")
         END ;
         RETURN TRUE
      ELSE
         IF Debugging
         THEN
            fprintf0 (StdErr, " fileinode failed\n")
         END
      END ;
      r := close (f) ;
      DISPOSE (p)
   END ;
   IF Debugging
   THEN
      fprintf0 (StdErr, " no\n")
   END ;
   RETURN FALSE
END RegisterModuleObject ;


(*
   isRegistered -
*)

PROCEDURE isRegistered (fo: FileObjects; f: INTEGER) : BOOLEAN ;
VAR
   i, h,
   low, high: CARDINAL ;
   o        : FileObject ;
BEGIN
   IF fileinode (f, low, high) = 0
   THEN
      h := HighIndice (fo^.objects) ;
      i := 1 ;
      WHILE i <= h DO
         o := GetIndice (fo^.objects, i) ;
         IF o # NIL
         THEN
            IF (o^.inodeLow = low) AND (o^.inodeHigh = high)
            THEN
               RETURN TRUE
            END
         END ;
         INC (i)
      END
   END ;
   RETURN FALSE
END isRegistered ;


(*
   IsRegistered - returns TRUE if the object at, location, is already registered.
                  It uses the physical location on the filesystem to determine the
                  uniqueness of the object file.
*)

PROCEDURE IsRegistered (fo: FileObjects; location: String) : BOOLEAN ;
VAR
   f, r  : INTEGER ;
   result: BOOLEAN ;
BEGIN
   f := open (string (location), UNIXREADONLY, 0) ;
   result := isRegistered (fo, f) ;
   r := close (f) ;
   RETURN result
END IsRegistered ;


(*
   InitFileObject - returns a new file object container.
*)

PROCEDURE InitFileObject () : FileObjects ;
VAR
   fo: FileObjects ;
BEGIN
   NEW (fo) ;
   fo^.objects := InitIndex (1) ;
   RETURN fo
END InitFileObject ;


(*
   KillFileObject - destroys a file object container.
*)

PROCEDURE KillFileObject (fo: FileObjects) : FileObjects ;
BEGIN
   fo^.objects := KillIndex (fo^.objects) ;
   DISPOSE (fo) ;
   RETURN NIL
END KillFileObject ;


END ObjectFiles.