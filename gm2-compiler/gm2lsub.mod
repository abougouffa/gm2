(* Copyright (C) 2001 Free Software Foundation, Inc. *)
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
MODULE gm2lsub ;

(*
   Author     : Gaius Mulley
   Title      : gm2lsub
   Date       : Thu Jun  7 16:33:14 BST 1990
   Description: Ensures than M2RTS is initialized before all other
                modules.
   Last update: Thu Jun  7 16:33:20 BST 1990
*)


FROM libc IMPORT exit ;
FROM StrLib IMPORT StrEqual ;
FROM ASCII IMPORT eof ;  (* UNIX *)
FROM Args IMPORT GetArg ;
FROM FIO IMPORT File, OpenToRead, OpenToWrite, StdIn, StdOut, StdErr, WriteChar,
                ReadString, WriteString, WriteLine, Close, EOF, IsNoError ;

CONST
   MaxName = 1024  ; (* Length of an identifier *)
   Comment = '#'   ; (* Comment identifier.     *)

VAR
   fi, fo: File ;


PROCEDURE IsPseudoModule (a: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   RETURN( StrEqual(a, 'libc') )
END IsPseudoModule ;


(*
   IsM2RTS - returns true if the module is M2RTS.
*)

PROCEDURE IsM2RTS (a: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   RETURN( StrEqual(a, 'M2RTS') )
END IsM2RTS ;


(*
   GenSub - writes out the modules which require compiling and linking.
*)

PROCEDURE GenSub ;
VAR
   a: ARRAY [0..MaxName] OF CHAR ;
BEGIN
   WriteString(fo, 'M2RTS') ; WriteLine(fo) ;
   ReadString(fi, a) ;
   WHILE (NOT (EOF(fi) AND StrEqual(a, ''))) DO
      IF (a[0]#Comment) AND (NOT IsM2RTS(a))
      THEN
         WriteString(fo, a) ; WriteLine(fo)
      END ;
      ReadString(fi, a)
   END ;
   WriteLine(fo) ;
   Close(fo)
END GenSub ;


(*
   OpenOutputFile - attempts to open an output file.
*)

PROCEDURE OpenOutputFile (a: ARRAY OF CHAR) ;
BEGIN
   fo := OpenToWrite(a) ;
   IF NOT IsNoError(fo)
   THEN
      WriteString(StdErr, 'cannot write to: ') ; WriteString(StdErr, a) ; WriteLine(StdErr) ;
      exit(1)
   END
END OpenOutputFile ;


(*
   ScanArgs - scans arguments.
*)

PROCEDURE ScanArgs ;
VAR
   i        : CARDINAL ;
   a        : ARRAY [0..MaxName] OF CHAR ;
   FoundFile: BOOLEAN ;
BEGIN
   FoundFile := FALSE ;
   fi        := StdIn ;
   fo        := StdOut ;
   i := 1 ;
   WHILE GetArg(a, i) DO
      IF StrEqual(a, '-o')
      THEN
         INC(i) ;
         IF GetArg(a, i)
         THEN
            OpenOutputFile(a) ;
         ELSE
            WriteString(StdErr, 'missing filename option after -o') ; WriteLine(StdErr) ;
            exit(1)
         END
      ELSE
         IF FoundFile
         THEN
            WriteString(StdErr, 'already opened one file for reading') ; WriteLine(StdErr)
         ELSE
            FoundFile := TRUE ;
            fi := OpenToRead(a) ;
            IF NOT IsNoError(fi)
            THEN
               WriteString(StdErr, 'failed to open: ') ; WriteString(StdErr, a) ;            
               WriteLine(StdErr) ;
               exit(1)
            END
         END
      END ;
      INC(i)
   END
END ScanArgs ;


BEGIN
   ScanArgs ;
   GenSub
END gm2lsub.
