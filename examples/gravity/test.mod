(* Copyright (C) 2009 Free Software Foundation, Inc. *)
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

MODULE test ;

IMPORT twoDsim ;
FROM libc IMPORT printf ;

VAR
   c, b, d, e: CARDINAL ;
BEGIN
   b := twoDsim.box(0.0, 0.0, 1.0, 1.0) ;
   b := twoDsim.fix(b) ;

   c := twoDsim.circle(0.74, 0.9, 0.1) ;
   c := twoDsim.mass(c, 0.01) ;

   d := twoDsim.box(0.2, 0.8, 0.2, 0.2) ;
   d := twoDsim.mass(d, 0.02) ;
   d := twoDsim.fix(d) ;

   e := twoDsim.circle(0.9, 0.1, 0.1) ;
   e := twoDsim.fix(e) ;

   twoDsim.gravity(-9.80665) ;
   twoDsim.fps(24.0) ;
   twoDsim.addDebugging(0.34974, c, e) ;
   printf("first collision should be at 0.34974 seconds\n") ;
   twoDsim.simulateFor(1.0)
END test.
