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
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)
MODULE gm2lgen ;

(*
   Author     : Gaius Mulley
   Title      : gm2lgen
   Date       : Fri Sep 15 14:42:17 BST 1989
   Description: Generates the C main.c code, from a list of module names.
   Last update: Thu Nov  2 12:55:02 GMT 2000
*)

FROM libc IMPORT exit ;
FROM StrLib IMPORT StrEqual, StrCopy, IsSubString ;
FROM ASCII IMPORT eof ;
FROM Args IMPORT GetArg ;
FROM NameKey IMPORT MakeKey, GetKey ;
FROM Lists IMPORT List, InitList, IncludeItemIntoList, GetItemFromList, NoOfItemsInList ;
FROM FIO IMPORT File, OpenToRead, OpenToWrite, StdIn, StdOut, StdErr, WriteChar,
                ReadString, WriteString, EOF, IsNoError, WriteLine, Close ;


(* %%%FORWARD%%%
PROCEDURE BuildFunctionList ; FORWARD ;
PROCEDURE GenInitializationCalls (ApuFound: BOOLEAN) ; FORWARD ;
PROCEDURE GenExternals (ApuFound: BOOLEAN) ; FORWARD ;
   %%%FORWARD%%% *)


CONST
   MaxName = 255  ; (* Length of an identifier *)
   Comment = '#'  ; (* Comment identifier      *)

VAR
   ExitNeeded,
   ApuFound    : BOOLEAN ;
   MainName    : ARRAY [0..MaxName] OF CHAR ;
   FunctionList: List ;
   fi, fo      : File ;


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
   OpenInputFile - attempts to open an input file.
*)

PROCEDURE OpenInputFile (a: ARRAY OF CHAR) ;
BEGIN
   fi := OpenToRead(a) ;
   IF NOT IsNoError(fo)
   THEN
      WriteString(StdErr, 'cannot open: ') ; WriteString(StdErr, a) ; WriteLine(StdErr) ;
      exit(1)
   END
END OpenInputFile ;


(*
   ScanArgs - 
*)

PROCEDURE ScanArgs ;
VAR
   i: CARDINAL ;
   a: ARRAY [0..MaxName] OF CHAR ;
BEGIN
   i          := 1 ;
   ApuFound   := FALSE ;
   ExitNeeded := TRUE ;
   StrCopy('main', MainName) ;
   fi         := StdIn ;
   fo         := StdOut ;
   WHILE GetArg(a, i) DO
      IF StrEqual(a, '-apu')
      THEN
         ApuFound := TRUE
      ELSIF StrEqual(a, '-exit')
      THEN
         ExitNeeded := FALSE
      ELSIF StrEqual(a, '-h')
      THEN
         WriteString(StdErr, 'gm2lgen [-main function] [-o outputfile] [ inputfile ] [-apu] [-exit]') ;
         WriteLine(StdErr) ;
         exit(0)
      ELSIF StrEqual(a, '-o')
      THEN
         INC(i) ;
         IF GetArg(a, i)
         THEN
            OpenOutputFile(a) ;
         ELSE
            WriteString(StdErr, 'missing filename option after -o') ; WriteLine(StdErr) ;
            exit(1)
         END
      ELSIF StrEqual(a, '-main')
      THEN
         INC(i) ;
         IF GetArg(a, i)
         THEN
            StrCopy(a, MainName)
         ELSE
            WriteString(StdErr, 'missing functionname after option -main') ; WriteLine(StdErr) ;
            exit(1)
         END
      ELSE
         OpenInputFile(a)
      END ;
      INC(i)
   END ;
END ScanArgs ;


(*
   GenMain - writes out the main() function together with module initialization
             calls.
*)

PROCEDURE GenMain ;
BEGIN
   InitList(FunctionList) ;
   ScanArgs ;
   BuildFunctionList ;
   GenExternals(ApuFound) ;
   IF ApuFound
   THEN
      WriteString(fo, '    .global ') ; WriteString(fo, '_') ; WriteString(fo, MainName) ; WriteLine(fo) ;
      WriteString(fo, '_') ; WriteString(fo, MainName) ; WriteString(fo, ':') ; WriteLine(fo) ;
      WriteString(fo, '    load.d.d  %ra') ; WriteLine(fo) ;
      WriteString(fo, '    load.d.d  %sp') ; WriteLine(fo) ;
      WriteString(fo, '    store.d.d %ra') ; WriteLine(fo) ;
      WriteString(fo, '    load.d.d  %rb') ; WriteLine(fo) ;
      WriteString(fo, '    load.d.d  %rc') ; WriteLine(fo) ;  (* so gdb can get hold of this function *)
      GenInitializationCalls(ApuFound) ;
      WriteString(fo, '    store.d.d %rc') ; WriteLine(fo) ;
      WriteString(fo, '    store.d.d %rb') ; WriteLine(fo) ;
      WriteString(fo, '    store.d.d %ra') ; WriteLine(fo) ;
      WriteString(fo, '    store.d.d %pc') ; WriteLine(fo) ;
      WriteString(fo, '    nop') ; WriteLine(fo) ;            (* just so our PC never goes out of range *)
      WriteString(fo, '    .end') ; WriteLine(fo)
   ELSE
      WriteString(fo, 'int ') ;
      WriteString(fo, MainName) ; WriteString(fo, '(argc, argv)') ; WriteLine(fo) ;
      WriteString(fo, 'int   argc ;') ; WriteLine(fo) ;
      WriteString(fo, 'char  *argv[];') ; WriteLine(fo) ;
      WriteString(fo, '{') ; WriteLine(fo) ;
      GenInitializationCalls(ApuFound) ;
      IF ExitNeeded
      THEN
         WriteString(fo, '   ') ; WriteString(fo, 'libc_exit(0);') ; WriteLine(fo)
      END ;
      WriteString(fo, '   return(0);') ; WriteLine(fo) ;
      WriteString(fo, '}') ; WriteLine(fo)
   END;
   Close(fo);
END GenMain ;


(*
   GenExternals - writes out the external prototypes for each module initializer.
*)

PROCEDURE GenExternals (ApuFound: BOOLEAN) ;
VAR
   a   : ARRAY [0..MaxName] OF CHAR ;
   i, n: CARDINAL ;
BEGIN
   n := NoOfItemsInList(FunctionList) ;
   i := 1 ;
   WHILE i<=n DO
      GetKey(GetItemFromList(FunctionList, i), a) ;
      IF ApuFound
      THEN
         WriteString(fo, '  .extern __M2_') ; WriteString(fo, a) ; WriteString(fo, '_init') ; WriteLine(fo) ;
      ELSE
         WriteString(fo, 'extern  _M2_') ;
         WriteString(fo, a) ; WriteString(fo, '_init(int argc, char *argv[]) ;') ; WriteLine(fo)
      END ;
      INC(i)
   END
END GenExternals ;


(*
   GenInitializationCalls - writes out the initialization calls for the modules
                            in the application suit.
*)

PROCEDURE GenInitializationCalls (ApuFound: BOOLEAN) ;
VAR
   a: ARRAY [0..MaxName] OF CHAR ;
   i, n: CARDINAL ;
BEGIN
   n := NoOfItemsInList(FunctionList) ;
   i := 1 ;
   WHILE i<=n DO
      GetKey(GetItemFromList(FunctionList, i), a) ;
      IF ApuFound
      THEN
         WriteString(fo, '  load.d.d $__M2_') ; WriteString(fo, a) ; WriteString(fo, '_init') ; WriteLine(fo) ;
         WriteString(fo, '  call') ; WriteLine(fo)
      ELSE
         WriteString(fo, '   _M2_') ;
         WriteString(fo, a) ; WriteString(fo, '_init(argc, argv) ;') ; WriteLine(fo)
      END ;
      INC(i)
   END
END GenInitializationCalls ;


(*
   BuildFunctionList - reads in the list of functions and stores them.
*)

PROCEDURE BuildFunctionList ;
VAR
   a: ARRAY [0..MaxName] OF CHAR ;
BEGIN
   ReadString(fi, a) ;
   WHILE (NOT (EOF(fi) AND StrEqual(a, ''))) DO
      IF (a[0]#Comment) AND (NOT StrEqual(a, '')) AND (NOT IsSubString(a, '<onlylink>'))
      THEN
         IncludeItemIntoList(FunctionList, MakeKey(a))
      END ;
      ReadString(fi, a)
   END
END BuildFunctionList ;


BEGIN
   GenMain
END gm2lgen.
