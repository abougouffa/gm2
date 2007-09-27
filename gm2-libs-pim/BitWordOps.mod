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

FROM SYSTEM IMPORT BITSET, BYTE, ADR, SHIFT, ROTATE, TSIZE ;


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
   SetBits - sets bits in, word, starting at, firstBit, and ending at,
             lastBit, with, pattern.  The bit zero of, pattern, will
             be placed into, word, at position, firstBit.
*)

PROCEDURE SetBits (VAR word: WORD; firstBit, lastBit: CARDINAL;
                   pattern: WORD) ;
VAR
   pw, pp: BITSET ;
   i, j  : CARDINAL ;
BEGIN
   pw := VAL(BITSET, word) ;
   pp := VAL(BITSET, pattern) ;
   j := 0 ;
   FOR i := firstBit TO lastBit DO
      IF j IN pp
      THEN
         INCL(pw, i)
      ELSE
         EXCL(pw, i)
      END ;
      INC(j)
   END ;
   word := VAL(WORD, pw)
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
BEGIN
   RETURN SHIFT(VAL(BITSET, word), count)
END WordShr ;


(*
   WordShl - returns a, word, which has been shifted, count
             bits to the left.
*)

PROCEDURE WordShl (word: WORD; count: CARDINAL) : WORD ;
BEGIN
   RETURN SHIFT(VAL(BITSET, word), -VAL(INTEGER, count))
END WordShl ;


(*
   WordSar - shift word arthemetic right.  Preserves the top
             end bit and as the value is shifted right.
*)

PROCEDURE WordSar (word: WORD; count: CARDINAL) : WORD ;
VAR
   w: WORD ;
BEGIN
   IF MAX(BITSET) IN VAL(BITSET, word)
   THEN
      w := VAL(WORD, SHIFT(VAL(BITSET, word), count)) ;
      SetBits(w, MAX(BITSET)-count, MAX(BITSET), -BITSET{}) ;
      RETURN w
   ELSE
      RETURN SHIFT(VAL(BITSET, word), count)
   END
END WordSar ;


(*
   WordRor - returns a, word, which has been rotated, count
             bits to the right.
*)

PROCEDURE WordRor (word: WORD; count: CARDINAL) : WORD ;
BEGIN
   RETURN ROTATE(VAL(BITSET, word), count)
END WordRor ;


(*
   WordRol - returns a, word, which has been rotated, count
             bits to the left.
*)

PROCEDURE WordRol (word: WORD; count: CARDINAL) : WORD ;
BEGIN
   RETURN ROTATE(VAL(BITSET, word), -VAL(INTEGER, count))
END WordRol ;


(*
   HighByte - returns the top byte only from, word.
              The byte is returned in the bottom byte
              in the return value.
*)

PROCEDURE HighByte (word: WORD) : WORD ;
VAR
   p: POINTER TO ARRAY [0..TSIZE(WORD)-1] OF BYTE ;
BEGIN
   p := ADR(word) ;
   RETURN VAL(WORD, p^[TSIZE(WORD)-1])
END HighByte ;


(*
   LowByte - returns the low byte only from, word.
             The byte is returned in the bottom byte
             in the return value.
*)

PROCEDURE LowByte (word: WORD) : WORD ;
VAR
   p: POINTER TO ARRAY [0..TSIZE(WORD)-1] OF BYTE ;
BEGIN
   p := ADR(word) ;
   RETURN VAL(WORD, p^[0])
END LowByte ;


(*
   Swap - byte flips the contents of word.
*)

PROCEDURE Swap (word: WORD) : WORD ;
VAR
   p   : POINTER TO ARRAY [0..TSIZE(WORD)-1] OF BYTE ;
   i, j: CARDINAL ;
   b   : BYTE ;
BEGIN
   p := ADR(word) ;
   j := TSIZE(WORD)-1 ;
   FOR i := 0 TO (TSIZE(WORD) DIV 2)-1 DO
      b := p^[i] ;
      p^[i] := p^[j] ;
      p^[j] := b ;
      DEC(j)
   END ;
   RETURN word
END Swap ;


END BitWordOps.
