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

IMPLEMENTATION MODULE Strings ;

FROM ASCII IMPORT nul ;
IMPORT StrLib ;
IMPORT DynamicStrings ;


(*
   Assign - source := dest.
*)

PROCEDURE Assign (VAR dest: ARRAY OF CHAR; source: ARRAY OF CHAR) ;
BEGIN
   StrLib.StrCopy(source, dest)
END Assign ;


(*
   Insert - insert the string, substr, into str at position, index.
            substr, is added to the end of, str, if, index >= length(str)
*)

PROCEDURE Insert (substr: ARRAY OF CHAR; VAR str: ARRAY OF CHAR;
                  index: CARDINAL) ;
VAR
   s1, s2: DynamicStrings.String ;
BEGIN
   IF index>Length(str)
   THEN
      ConCat(str, substr, str)
   ELSE
      s1 := DynamicStrings.InitString(str) ;
      s2 := DynamicStrings.ConCat(DynamicStrings.Slice(s1, 0, index),
                                  DynamicStrings.Mark(DynamicStrings.InitString(str))) ;
      s2 := DynamicStrings.ConCat(s2, DynamicStrings.Slice(s1, index, 0)) ;
      DynamicStrings.CopyOut(str, s2) ;
      s1 := DynamicStrings.KillString(s1) ;
      s2 := DynamicStrings.KillString(s2)
   END
END Insert ;


(*
   Delete - delete len characters from, str, starting at, index.
*)

PROCEDURE Delete (VAR str: ARRAY OF CHAR; index: CARDINAL; length: CARDINAL) ;
VAR
   s: DynamicStrings.String ;
BEGIN
   s := DynamicStrings.InitString(str) ;
   s := DynamicStrings.ConCat(DynamicStrings.Mark(DynamicStrings.Slice(s, 0, index)),
                              DynamicStrings.Mark(DynamicStrings.Slice(s, index+length, 0))) ;
   DynamicStrings.CopyOut(str, s) ;
   s := DynamicStrings.KillString(s)
END Delete ;


(*
   Pos - return the first position of, substr, in, str.
*)

PROCEDURE Pos (substr, str: ARRAY OF CHAR) : CARDINAL ;
VAR
   i, k, l   : INTEGER ;
   s1, s2, s3: DynamicStrings.String ;
BEGIN
   s1 := DynamicStrings.InitString(str) ;
   s2 := DynamicStrings.InitString(substr) ;
   k := DynamicStrings.Length(s1) ;
   l := DynamicStrings.Length(s2) ;
   i := 0 ;
   REPEAT
      i := DynamicStrings.Index(s1, DynamicStrings.char(s2, 0), i) ;
      IF i>=0
      THEN
         s3 := DynamicStrings.Slice(s1, i, l) ;
         IF DynamicStrings.Equal(s3, s2)
         THEN
            s1 := DynamicStrings.KillString(s1) ;
            s2 := DynamicStrings.KillString(s2) ;
            s3 := DynamicStrings.KillString(s3) ;
            RETURN( i )
         END ;
         s3 := DynamicStrings.KillString(s3)
      END ;
      INC(i)
   UNTIL i>=k ;
   s1 := DynamicStrings.KillString(s1) ;
   s2 := DynamicStrings.KillString(s2) ;
   s3 := DynamicStrings.KillString(s3) ;
   RETURN( HIGH(str)+1 )
END Pos ;


(*
   Copy - copy at most, length, characters in, substr, to, str,
          starting at position, index.
*)

PROCEDURE Copy (str: ARRAY OF CHAR;
                index, length: CARDINAL; VAR result: ARRAY OF CHAR) ;
VAR
   s1, s2: DynamicStrings.String ;
BEGIN
   s1 := DynamicStrings.InitString(str) ;
   s2 := DynamicStrings.Slice(s1, index, index+length) ;
   DynamicStrings.CopyOut(result, s2) ;
   s1 := DynamicStrings.KillString(s1) ;
   s2 := DynamicStrings.KillString(s2)
END Copy ;


(*
   ConCat - concatenates two strings, s1, and, s2
            and places the result into, dest.
*)

PROCEDURE ConCat (s1, s2: ARRAY OF CHAR; VAR dest: ARRAY OF CHAR) ;
BEGIN
   StrLib.StrConCat(s1, s2, dest)   
END ConCat ;


(*
   Length - return the length of string, s.
*)

PROCEDURE Length (s: ARRAY OF CHAR) : CARDINAL ;
BEGIN
   RETURN( StrLib.StrLen(s) )
END Length ;


(*
   CompareStr - compare two strings, left, and, right.
*)

PROCEDURE CompareStr (left, right: ARRAY OF CHAR) : INTEGER ;
BEGIN
   IF StrLib.StrLess(left, right)
   THEN
      RETURN( -1 )
   ELSIF StrLib.StrEqual(left, right)
   THEN
      RETURN( 0 )
   ELSE
      RETURN( 1 )
   END
END CompareStr ;


END Strings.
