(* M2ColorString.mod provides procedures for obtaining GCC color strings.

Copyright (C) 2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

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

IMPLEMENTATION MODULE M2ColorString ;

FROM m2color IMPORT colorize_start, colorize_stop ;
FROM DynamicStrings IMPORT InitString, InitStringCharStar,
                           ConCat, ConCatChar, Mark, string, KillString,
                           Dup, char, Length, Mult ;
FROM StrLib IMPORT StrLen ;


CONST
   EnableColor = TRUE ;


(*
   quoteBegin - adds an open quote to string, s.
*)

PROCEDURE quoteBegin (s: String) : String ;
VAR
   c: String ;
BEGIN
   c := InitStringCharStar (colorize_start (EnableColor, "quote", StrLen ("quote"))) ;
   s := ConCat (s, c) ;
   c := KillString (c) ;
   RETURN ConCatChar (s, '‘')
END quoteBegin ;


(*
   quoteEnd - adds a close quote to string, s.
*)

PROCEDURE quoteEnd (s: String) : String ;
VAR
   c: String ;
BEGIN
   s := ConCatChar (s, '’') ;
   c := InitStringCharStar (colorize_stop (EnableColor)) ;
   s := ConCat (s, c) ;
   c := KillString (c) ;
   RETURN s
END quoteEnd ;


(*
   endColor - stops using color.
*)

PROCEDURE endColor (s: String) : String ;
VAR
   c: String ;
BEGIN
   c := InitStringCharStar (colorize_stop (EnableColor)) ;
   s := ConCat (s, c) ;
   c := KillString (c) ;
   RETURN s
END endColor ;


(*
   errorBegin - adds error color to string, s.
*)

PROCEDURE errorBegin (s: String) : String ;
VAR
   c: String ;
BEGIN
   c := InitStringCharStar (colorize_start (EnableColor, "error", StrLen ("error"))) ;
   s := ConCat (s, c) ;
   c := KillString (c) ;
   RETURN s
END errorBegin ;


(*
   errorEnd - adds the end error color to string, s.
*)

PROCEDURE errorEnd (s: String) : String ;
BEGIN
   RETURN endColor (s)
END errorEnd ;


(*
   warningBegin - adds warning color to string, s.
*)

PROCEDURE warningBegin (s: String) : String ;
VAR
   c: String ;
BEGIN
   c := InitStringCharStar (colorize_start (EnableColor, "warning", StrLen ("warning"))) ;
   s := ConCat (s, c) ;
   c := KillString (c) ;
   RETURN s
END warningBegin ;


(*
   warningEnd - adds the end warning color to string, s.
*)

PROCEDURE warningEnd (s: String) : String ;
BEGIN
   RETURN endColor (s)
END warningEnd ;


(*
   noteBegin - adds note color to string, s.
*)

PROCEDURE noteBegin (s: String) : String ;
VAR
   c: String ;
BEGIN
   c := InitStringCharStar (colorize_start (EnableColor, "note", StrLen ("note"))) ;
   s := ConCat (s, c) ;
   c := KillString (c) ;
   RETURN s
END noteBegin ;


(*
   noteEnd - adds the end note color to string, s.
*)

PROCEDURE noteEnd (s: String) : String ;
BEGIN
   RETURN endColor (s)
END noteEnd ;


(*
   locusBegin - adds locus color to string, s.
*)

PROCEDURE locusBegin (s: String) : String ;
VAR
   c: String ;
BEGIN
   c := InitStringCharStar (colorize_start (EnableColor, "locus", StrLen ("locus"))) ;
   s := ConCat (s, c) ;
   c := KillString (c) ;
   RETURN s
END locusBegin ;


(*
   locusEnd - adds the end locus color to string, s.
*)

PROCEDURE locusEnd (s: String) : String ;
BEGIN
   RETURN endColor (s)
END locusEnd ;


(*
   fixitInsertBegin - adds fixit-insert color to string, s.
*)

PROCEDURE fixitInsertBegin (s: String) : String ;
VAR
   c: String ;
BEGIN
   c := InitStringCharStar (colorize_start (EnableColor, "fixit-insert", StrLen ("fixit-insert"))) ;
   s := ConCat (s, c) ;
   c := KillString (c) ;
   RETURN s
END fixitInsertBegin ;


(*
   fixitInsertEnd - adds the end fixit-insert color to string, s.
*)

PROCEDURE fixitInsertEnd (s: String) : String ;
BEGIN
   RETURN endColor (s)
END fixitInsertEnd ;


(*
   fixitInsertBegin - adds fixit-insert color to string, s.
*)

PROCEDURE fixitDeleteBegin (s: String) : String ;
VAR
   c: String ;
BEGIN
   c := InitStringCharStar (colorize_start (EnableColor, "fixit-delete", StrLen ("fixit-delete"))) ;
   s := ConCat (s, c) ;
   c := KillString (c) ;
   RETURN s
END fixitDeleteBegin ;


(*
   fixitDeleteEnd - adds the end fixit-delete color to string, s.
*)

PROCEDURE fixitDeleteEnd (s: String) : String ;
BEGIN
   RETURN endColor (s)
END fixitDeleteEnd ;


(*
   filenameBegin - adds filename color to string, s.
*)

PROCEDURE filenameBegin (s: String) : String ;
VAR
   c: String ;
BEGIN
   c := InitStringCharStar (colorize_start (EnableColor, "diff-filename", StrLen ("diff-filename"))) ;
   s := ConCat (s, c) ;
   c := KillString (c) ;
   RETURN s
END filenameBegin ;


(*
   filenameEnd - adds the end filename color to string, s.
*)

PROCEDURE filenameEnd (s: String) : String ;
BEGIN
   RETURN endColor (s)
END filenameEnd ;


(*
   typeBegin - adds type color to string, s.
*)

PROCEDURE typeBegin (s: String) : String ;
VAR
   c: String ;
BEGIN
   c := InitStringCharStar (colorize_start (EnableColor, "type-diff", StrLen ("type-diff"))) ;
   s := ConCat (s, c) ;
   c := KillString (c) ;
   RETURN s
END typeBegin ;


(*
   typeEnd - adds the end type color to string, s.
*)

PROCEDURE typeEnd (s: String) : String ;
BEGIN
   RETURN endColor (s)
END typeEnd ;


END M2ColorString.
