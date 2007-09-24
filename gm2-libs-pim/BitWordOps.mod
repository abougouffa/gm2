(* Copyright (C) 2005 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE BitWordOps ;

FROM SYSTEM IMPORT BITSET ;


(*
   GetBits - returns the bits firstBit..lastBit from source.
             Bit 0 of word maps onto the firstBit of source.
*)

PROCEDURE GetBits (source: WORD; firstBit, lastBit: CARDINAL) : WORD ;
VAR
   si  : CARDINAL ;
   sb  : BITSET ;
   mask: BITSET ;
   i   : CARDINAL ;
BEGIN
   sb := VAL(BITSET, source) ;
   mask := {} ;
   FOR i := firstBit TO lastBit DO
      INCL(mask, i)
   END ;
   sb := VAL(BITSET, source) * mask ;
   i := 1 ;
   WHILE firstBit>0 DO
      DEC(firstBit) ;
      i := i*2
   END ;
   si := VAL(CARDINAL, sb) ;
   RETURN VAL(WORD, si DIV i)
END GetBits ;


(*
   SetBits - 
*)

PROCEDURE SetBits (VAR word: WORD; firstBit, lastBit: CARDINAL;
                   pattern: WORD) ;
BEGIN
   
END SetBits ;


(*
   WordAnd - returns a bitwise (left AND right)
*)

PROCEDURE WordAnd (left, right: WORD) : WORD ;
BEGIN
   RETURN VAL(WORD, VAL(BITSET, left)*VAL(BITSET, right))
END WordAnd ;


(*
   WordOr - returns a bitwise (left OR right)
*)

PROCEDURE WordOr (left, right: WORD) : WORD ;
BEGIN
   RETURN VAL(WORD, VAL(BITSET, left)+VAL(BITSET, right))
END WordOr ;


(*
   WordXor - returns a bitwise (left XOR right)
*)

PROCEDURE WordXor (left, right: WORD) : WORD ;
BEGIN
   RETURN VAL(WORD, VAL(BITSET, left) DIV VAL(BITSET, right))
END WordXor ;


(*
   WordNot - returns a word with all bits inverted.
*)

PROCEDURE WordNot (word: WORD) : WORD ;
BEGIN
   RETURN VAL(WORD, -VAL(BITSET, word))
END WordNot ;


(*
   WordShr - returns a, word, which has been shifted, count
             bits to the right.
*)

PROCEDURE WordShr (word: WORD; count: CARDINAL) : WORD ;
VAR
   c: CARDINAL ;
BEGIN
   c := VAL(CARDINAL, word) ;
   RETURN VAL(CARDINAL, word)
END WordShr ;



END BitWordOps.
