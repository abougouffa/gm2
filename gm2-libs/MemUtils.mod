(* Copyright (C) 2001 Free Software Foundation, Inc. *)
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
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA *)
IMPLEMENTATION MODULE MemUtils ;


FROM SYSTEM IMPORT WORD, BYTE, TSIZE ;


(*
   MemCopy - copys a region of memory to the required destination.
*)

PROCEDURE MemCopy (from: ADDRESS; length: CARDINAL; to: ADDRESS) ;
VAR
   pwb, pwa: POINTER TO WORD ;
   pbb, pba: POINTER TO BYTE ;
BEGIN
   WHILE length>=TSIZE(WORD) DO
      pwa := from ;
      pwb := to ;
      pwb^ := pwa^ ;
      INC(from  , TSIZE(WORD)) ;
      INC(to    , TSIZE(WORD)) ;
      DEC(length, TSIZE(WORD))
   END ;
   WHILE length>0 DO
      pba := from ;
      pbb := to ;
      pbb^ := pba^ ;
      INC(from  , TSIZE(BYTE)) ;
      INC(to    , TSIZE(BYTE)) ;
      DEC(length, TSIZE(BYTE))
   END
END MemCopy ;


(*
   MemZero - sets a region of memory: a..a+length to zero.
*)

PROCEDURE MemZero (a: ADDRESS; length: CARDINAL) ;
VAR
   pwa: POINTER TO WORD ;
   pba: POINTER TO BYTE ;
BEGIN
   pwa := a ;
   WHILE length>=TSIZE(WORD) DO
      pwa^ := WORD(0) ;
      INC(pwa, TSIZE(WORD)) ;
      DEC(length, TSIZE(WORD))
   END ;
   pba := ADDRESS(pwa) ;
   WHILE length>=TSIZE(BYTE) DO
      pba^ := BYTE(0) ;
      INC(pba, TSIZE(BYTE)) ;
      DEC(length, TSIZE(BYTE))
   END
END MemZero ;


END MemUtils.
