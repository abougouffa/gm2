(* Copyright (C) 2008 Free Software Foundation, Inc. *)
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
Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA  02110-1301  USA *)

IMPLEMENTATION MODULE WholeStr ;

FROM DynamicStrings IMPORT String, KillString, CopyOut ;
FROM StringConvert IMPORT CardinalToString, IntegerToString ;
FROM WholeConv IMPORT FormatCard, ValueCard, FormatInt, ValueInt ;


(* the string form of a signed whole number is
     ["+" | "-"], decimal digit, {decimal digit}
*)

PROCEDURE StrToInt (str: ARRAY OF CHAR; VAR int: INTEGER;
                    VAR res: ConvResults);
  (* Ignores any leading spaces in str. If the subsequent
     characters in str are in the format of a signed whole
     number, assigns a corresponding value to int. Assigns
     a value indicating the format of str to res.
  *)
BEGIN
   res := FormatInt(str) ;
   IF res=strAllRight
   THEN
      int := ValueInt(str)
   END
END StrToInt ;


PROCEDURE IntToStr (int: INTEGER; VAR str: ARRAY OF CHAR);
  (* Converts the value of int to string form and copies
     the possibly truncated result to str. *)
VAR
   s: String ;
BEGIN
   s := IntegerToString(int, 0, ' ', TRUE, 10, FALSE) ;
   CopyOut(str, s) ;
   s := KillString(s)
END IntToStr ;


(* the string form of an unsigned whole number is
     decimal digit, {decimal digit}
*)

PROCEDURE StrToCard (str: ARRAY OF CHAR;
                     VAR card: CARDINAL;
                     VAR res: ConvResults);
  (* Ignores any leading spaces in str. If the subsequent
     characters in str are in the format of an unsigned
     whole number, assigns a corresponding value to card.
     Assigns a value indicating the format of str to res.
  *)
BEGIN
   res := FormatCard(str) ;
   IF res=strAllRight
   THEN
      card := ValueCard(str)
   END
END StrToCard ;


PROCEDURE CardToStr (card: CARDINAL; VAR str: ARRAY OF CHAR);
  (* Converts the value of card to string form and copies the
     possibly truncated result to str. *)
VAR
   s: String ;
BEGIN
   s := CardinalToString(card, 0, ' ', 10, FALSE) ;
   CopyOut(str, s) ;
   s := KillString(s)
END CardToStr ;


END WholeStr.
