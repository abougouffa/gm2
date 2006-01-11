(* Copyright (C) 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE realbitscast;

FROM SYSTEM IMPORT CAST, WORD ;

TYPE
    BITS32 = SET OF [0..31];
    BITS64 = SET OF [0..63];
    BITS96 = SET OF [0..95] ;
    REAL32 = SHORTREAL;
    REAL64 = REAL;
#if !defined(__sparc__) && !defined(__x86_64) && !defined(__ppc__)
    REAL96 = LONGREAL ;  (* on the __sparc__ SIZE(LONGREAL) = SIZE(REAL) *)
    (* and on the x86_64 LONGREAL is 128 bits *)
    (* for __ppc__, LONGREAL is 64 bits in gcc-3.3 *)
#endif

VAR
    b32 : BITS32;
    b64 : BITS64;
    r32 : REAL32;
    r64 : REAL64;
#if !defined(__sparc__) && !defined(__x86_64) && !defined(__ppc__)
    b96 : BITS96 ;
    r96 : REAL96 ;
#endif
    w   : WORD ;
BEGIN
   r32 := 1.0 ;
   b32 := CAST(BITS32,r32) ;
   b64 := CAST(BITS64,r64) ;
#if !defined(__sparc__) && !defined(__x86_64) && !defined(__ppc__)
   b96 := CAST(BITS96,r96)
#endif
END realbitscast.
