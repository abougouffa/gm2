(* Copyright (C) 2017 Free Software Foundation, Inc.  *)
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
Boston, MA 02110-1301, USA.  *)

IMPLEMENTATION MODULE GetOpt ;  (*!m2pim+gm2*)

FROM DynamicStrings IMPORT string, InitStringCharStar ;

IMPORT getopt ;


(*
   GetOpt - call C getopt and fill in the parameters:
            optarg, optind, opterr and optop.
*)

PROCEDURE GetOpt (argc: INTEGER; argv: ADDRESS; optstring: String;
                  VAR optarg: String;
                  VAR optind, opterr, optopt: INTEGER) : CHAR ;
VAR
   r: CHAR ;
BEGIN
   r := getopt.getopt (argc, argv, string (optstring)) ;
   optarg := InitStringCharStar (getopt.optarg) ;
   opterr := getopt.opterr ;
   optopt := getopt.optopt ;
   RETURN r
END GetOpt ;


END GetOpt.
