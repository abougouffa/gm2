(* Copyright (C) 2004 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE SYSTEM ;

CONST
   BitsPerBitset = MAX(BITSET)+1 ;


(*
   Max - returns the maximum of a and b.
*)

PROCEDURE (* __INLINE__ *) Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   Min - returns the minimum of a and b.
*)

PROCEDURE (* __INLINE__ *) Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


(*
   ShiftVal - is a runtime procedure whose job is to implement
              the SHIFT procedure of ISO SYSTEM. GNU Modula-2 will
              inline a SHIFT of a single WORD sized set and will only
              call this routine for larger sets.
*)

PROCEDURE ShiftVal (VAR s, d: ARRAY OF BITSET;
                    SetSizeInBits: CARDINAL;
                    ShiftCount: INTEGER) ;
BEGIN
   IF ShiftCount>0
   THEN
      ShiftLeft(s, d, SetSizeInBits, ShiftCount)
   ELSIF ShiftCount<0
   THEN
      ShiftRight(s, d, SetSizeInBits, -ShiftCount)
   END
END ShiftVal ;


(*
   ShiftLeft - performs the shift left for a multi word set.
               This procedure might be called by the back end of
               GNU Modula-2 depending whether amount is known at compile
               time.
*)

PROCEDURE ShiftLeft (VAR s, d: ARRAY OF BITSET;
                     SetSizeInBits: CARDINAL;
                     ShiftCount: INTEGER) ;
VAR
   lo, hi : BITSET ;
   i, j, h: CARDINAL ;
BEGIN
   h := HIGH(s) ;
   i := h ;
   WHILE i>0 DO
      DEC(i) ;
      lo := SHIFT(s[i], ShiftCount MOD BitsPerBitset) ;
      hi := SHIFT(s[i], -(BitsPerBitset - (ShiftCount MOD BitsPerBitset))) ;
      d[i] := BITSET{} ;
      j := i + ShiftCount DIV BitsPerBitset ;
      IF j<h
      THEN
         d[j] := d[j] + lo ;
         INC(j) ;
         IF j<h
         THEN
            d[j] := d[j] + hi
         END
      END
   END
END ShiftLeft ;


(*
   ShiftRight - performs the shift left for a multi word set.
                This procedure might be called by the back end of
                GNU Modula-2 depending whether amount is known at compile
                time.
*)

PROCEDURE ShiftRight (VAR s, d: ARRAY OF BITSET;
                      SetSizeInBits: CARDINAL;
                      ShiftCount: INTEGER) ;
VAR
   lo, hi : BITSET ;
   j      : INTEGER ;
   i, h   : CARDINAL ;
BEGIN
   h := HIGH(s) ;
   i := 0 ;
   WHILE i<h DO
      lo := SHIFT(s[i], BitsPerBitset - (ShiftCount MOD BitsPerBitset)) ;
      hi := SHIFT(s[i], -(ShiftCount MOD BitsPerBitset)) ;
      d[i] := BITSET{} ;
      j := i - ShiftCount DIV BitsPerBitset ;
      IF j>=0
      THEN
         d[j] := d[j] + hi ;
         DEC(j) ;
         IF j>=0
         THEN
            d[j] := d[j] + lo
         END
      END ;
      INC(i)
   END
END ShiftRight ;


(*
   RotateVal - is a runtime procedure whose job is to implement
               the ROTATE procedure of ISO SYSTEM. GNU Modula-2 will
               inline a ROTATE of a single WORD (or less)
               sized set and will only call this routine for larger sets.
*)

PROCEDURE RotateVal (VAR s, d: ARRAY OF BITSET;
                     SetSizeInBits: CARDINAL;
                     RotateCount: INTEGER) ;
BEGIN
   IF RotateCount>0
   THEN
      RotateLeft(s, d, SetSizeInBits, RotateCount)
   ELSIF RotateCount<0
   THEN
      RotateRight(s, d, SetSizeInBits, -RotateCount)
   END
END RotateVal ;


(*
   RotateLeft - performs the rotate left for a multi word set.
                This procedure might be called by the back end of
                GNU Modula-2 depending whether amount is known at compile
                time.
*)

PROCEDURE RotateLeft (VAR s, d: ARRAY OF BITSET;
                      SetSizeInBits: CARDINAL;
                      RotateCount: INTEGER) ;
VAR
   lo, hi : BITSET ;
   b, i, j, h: CARDINAL ;
BEGIN
   h := HIGH(s) ;
   (* firstly we set d := {} *)
   i := 0 ;
   WHILE i<=h DO
      d[i] := BITSET{} ;
      INC(i)
   END ;
   i := h+1 ;
   RotateCount := RotateCount MOD SetSizeInBits ;
   b := SetSizeInBits MOD BitsPerBitset ;
   IF b=0
   THEN
      b := BitsPerBitset
   END ;
   WHILE i>0 DO
      DEC(i) ;
      lo := SHIFT(s[i], RotateCount MOD BitsPerBitset) ;
      hi := SHIFT(s[i], -(b - (RotateCount MOD BitsPerBitset))) ;
      j := ((i*BitsPerBitset + RotateCount) MOD
            SetSizeInBits) DIV BitsPerBitset ;
      d[j] := d[j] + lo ;
      j := (((i+1)*BitsPerBitset + RotateCount) MOD
            SetSizeInBits) DIV BitsPerBitset ;
      d[j] := d[j] + hi ;
      b := BitsPerBitset
   END
END RotateLeft ;


(*
   RotateRight - performs the rotate right for a multi word set.
                 This procedure might be called by the back end of
                 GNU Modula-2 depending whether amount is known at compile
                 time.
*)

PROCEDURE RotateRight (VAR s, d: ARRAY OF BITSET;
                       SetSizeInBits: CARDINAL;
                       RotateCount: INTEGER) ;
BEGIN
   RotateLeft(s, d, SetSizeInBits, SetSizeInBits-RotateCount)
END RotateRight ;


END SYSTEM.
(*
 * Local variables:
 *  compile-command: "gm2 -Wiso -c -g -I. SYSTEM.mod"
 * End:
 *)
