(* Copyright (C) 2003 Free Software Foundation, Inc. *)
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
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

IMPLEMENTATION MODULE M2Bitset ;


FROM M2Debug IMPORT Assert ;
FROM gccgm2 IMPORT GetBitsPerWord, GetWordType, GetSizeOf ;
FROM M2ALU IMPORT PushCard, PushIntegerTree ;
FROM NameKey IMPORT MakeKey ;
FROM M2System IMPORT Word ;

FROM SymbolTable IMPORT NulSym,
      	       	     	MakeConstLit,
                        MakeConstVar,
      	       	     	MakeSet,
      	       	     	MakeSubrange,
      	       	     	PutSet,
      	       	     	PutSubrange,
                        PopValue,
                        PopSize ;


VAR
   MinBitset, MaxBitset : CARDINAL ;


(*
   MakeBitset - creates and declares the type BITSET.
*)

PROCEDURE MakeBitset ;
BEGIN
   Bitset := MakeSet(MakeKey('BITSET')) ;     (* Base Type       *)

   (* MinBitset *)
   MinBitset := MakeConstLit(MakeKey('0')) ;

   (* MaxBitset *)
   MaxBitset := MakeConstVar(MakeKey('MaxBitset')) ;
   PushCard(GetBitsPerWord()-1) ;
   PopValue(MaxBitset) ;

   Assert(Word#NulSym) ;
   Bitnum := MakeSubrange(MakeKey('BITNUM')) ;
   PutSubrange(Bitnum, MinBitset, MaxBitset, Word) ;
   PutSet(Bitset, Bitnum) ;

   PushIntegerTree(GetSizeOf(GetWordType())) ;
   PopSize(Bitset)
END MakeBitset ;


(*
   GetBitsetMinMax - assigns min and max to the minimum and maximum values of BITSET.
*)

PROCEDURE GetBitsetMinMax (VAR min, max: CARDINAL) ;
BEGIN
   min := MinBitset ;
   max := MaxBitset
END GetBitsetMinMax ;


END M2Bitset.
