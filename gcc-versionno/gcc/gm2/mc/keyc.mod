(* Copyright (C) 2015 Free Software Foundation, Inc.  *)
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
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

IMPLEMENTATION MODULE keyc ;

FROM mcPretty IMPORT pretty, print, prints, setNeedSpace, noSpace ;


VAR
   seenFree,
   seenMalloc,
   seenProc,
   seenTrue,
   seenFalse,
   seenNull,
   seenMemcpy: BOOLEAN ;


(*
   useFree - indicate we have used free.
*)

PROCEDURE useFree ;
BEGIN
   seenFree := TRUE
END useFree ;


(*
   useMalloc - indicate we have used malloc.
*)

PROCEDURE useMalloc ;
BEGIN
   seenMalloc := TRUE
END useMalloc ;


(*
   useProc - indicate we have used proc.
*)

PROCEDURE useProc ;
BEGIN
   seenProc := TRUE
END useProc ;


(*
   useTrue - indicate we have used TRUE.
*)

PROCEDURE useTrue ;
BEGIN
   seenTrue := TRUE
END useTrue ;


(*
   useFalse - indicate we have used FALSE.
*)

PROCEDURE useFalse ;
BEGIN
   seenFalse := TRUE
END useFalse ;


(*
   useNull - indicate we have used NULL.
*)

PROCEDURE useNull ;
BEGIN
   seenNull := TRUE
END useNull ;


(*
   useMemcpy - indicate we have used memcpy.
*)

PROCEDURE useMemcpy ;
BEGIN
   seenMemcpy := TRUE
END useMemcpy ;


(*
   checkFreeMalloc -
*)

PROCEDURE checkFreeMalloc (p: pretty) ;
BEGIN
   IF seenFree OR seenMalloc
   THEN
      print (p, "#include <stdlib.h>\n")
   END
END checkFreeMalloc ;


(*
   checkProc -
*)

PROCEDURE checkProc (p: pretty) ;
BEGIN
   IF seenProc
   THEN
      print (p, "#   if !defined (PROC_D)\n") ;
      print (p, "#      define PROC_D\n") ;
      print (p, "       typedef struct { void (*proc)(void); } PROC;\n") ;
      print (p, "#   endif\n\n")
   END
END checkProc ;


(*
   checkTrue -
*)

PROCEDURE checkTrue (p: pretty) ;
BEGIN
   IF seenTrue
   THEN
      print (p, "#   if !defined (TRUE)\n") ;
      print (p, "#      define TRUE (1==1)\n") ;
      print (p, "#   endif\n\n")
   END
END checkTrue ;


(*
   checkFalse -
*)

PROCEDURE checkFalse (p: pretty) ;
BEGIN
   IF seenTrue
   THEN
      print (p, "#   if !defined (FALSE)\n") ;
      print (p, "#      define FALSE (1==0)\n") ;
      print (p, "#   endif\n\n")
   END
END checkFalse ;


(*
   checkNull -
*)

PROCEDURE checkNull (p: pretty) ;
BEGIN
   IF seenNull
   THEN
      print (p, "#include <stddef.h>\n")
   END
END checkNull ;


(*
   checkMemcpy -
*)

PROCEDURE checkMemcpy (p: pretty) ;
BEGIN
   IF seenMemcpy
   THEN
      print (p, "#include <string.h>\n")
   END
END checkMemcpy ;


(*
   genDefs - generate definitions or includes for all
             macros and prototypes used.
*)

PROCEDURE genDefs (p: pretty) ;
BEGIN
   checkFreeMalloc (p) ;
   checkProc (p) ;
   checkTrue (p) ;
   checkFalse (p) ;
   checkNull (p) ;
   checkMemcpy (p)
END genDefs ;


(*
   init -
*)

PROCEDURE init ;
BEGIN
   seenFree := TRUE ;
   seenMalloc := TRUE ;
   seenProc := TRUE ;
   seenTrue := TRUE ;
   seenFalse := TRUE ;
   seenNull := TRUE ;
   seenMemcpy := TRUE
END init ;


BEGIN
   init
END keyc.
