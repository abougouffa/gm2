(* Copyright (C) 2011 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE M2DriverOptions ;

FROM DynamicStrings IMPORT String, Length, InitString, Mark, Slice, EqualArray,
                           InitStringCharStar, ConCatChar, ConCat, KillString,
                           PushAllocation, PopAllocationExemption ;

FROM SArgs IMPORT GetArg, Narg ;


VAR
   CppAndArgs: String ;


(*
   ScanCppArgs - scans the cpp arguments and builds up the cpp command line.
*)

PROCEDURE ScanCppArgs (i: CARDINAL) : CARDINAL ;
VAR
   s: String ;
BEGIN
   IF GetArg(s, i) AND EqualArray(s, '-fcppbegin')
   THEN
      INC(i) ;
      WHILE GetArg(s, i) DO
         IF EqualArray(s, '-fcppend')
         THEN
            RETURN( i )
         ELSIF NOT EqualArray(CppAndArgs, '')
         THEN
            CppAndArgs := ConCatChar(CppAndArgs, ' ')
         END ;
         CppAndArgs := ConCat(CppAndArgs, s) ;
         INC(i)
      END
   END ;
   RETURN( i )
END ScanCppArgs ;


(*
   CppCommandLine - returns the Cpp command line and all arguments.
*)

PROCEDURE CppCommandLine () : String ;
BEGIN
   RETURN( CppAndArgs )
END CppCommandLine ;


BEGIN
   CppAndArgs := InitString('')
END M2DriverOptions.
