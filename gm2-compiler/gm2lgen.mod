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
   Description: Generates the main C function, from a list of module names.
*)

FROM libc IMPORT exit ;
FROM ASCII IMPORT eof ;
FROM SArgs IMPORT GetArg ;
FROM Lists IMPORT List, InitList, IncludeItemIntoList, GetItemFromList, NoOfItemsInList ;
FROM FIO IMPORT File, StdIn, StdOut, StdErr, WriteChar,
                ReadString, WriteString, EOF, IsNoError, WriteLine, Close ;

FROM Strings IMPORT String, InitString, KillString, ConCat, RemoveWhitePrefix,
                    EqualArray, Mark, Assign, Fin, InitStringChar, Length, Slice, Equal ;

FROM M2Printf IMPORT fprintf0, fprintf1, fprintf2 ;
FROM SFIO IMPORT OpenToWrite, WriteS, ReadS, OpenToRead ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1 ;

(* %%%FORWARD%%%
PROCEDURE BuildFunctionList ; FORWARD ;
PROCEDURE GenInitializationCalls (ApuFound: BOOLEAN) ; FORWARD ;
PROCEDURE GenExternals (ApuFound: BOOLEAN) ; FORWARD ;
   %%%FORWARD%%% *)


CONST
   Comment = '#'  ; (* Comment leader      *)

VAR
   ExitNeeded,
   ApuFound    : BOOLEAN ;
   MainName    : String ;
   FunctionList: List ;
   fi, fo      : File ;


(*
   OpenOutputFile - attempts to open an output file.
*)

PROCEDURE OpenOutputFile (s: String) ;
BEGIN
   fo := OpenToWrite(s) ;
   IF NOT IsNoError(fo)
   THEN
      fprintf1(StdErr, 'cannot write to: %s\n', s) ;
      exit(1)
   END
END OpenOutputFile ;


(*
   OpenInputFile - attempts to open an input file.
*)

PROCEDURE OpenInputFile (s: String) ;
BEGIN
   fi := OpenToRead(s) ;
   IF NOT IsNoError(fo)
   THEN
      fprintf1(StdErr, 'cannot open: %s\n', s) ;
      exit(1)
   END
END OpenInputFile ;


(*
   ScanArgs - 
*)

PROCEDURE ScanArgs ;
VAR
   i: CARDINAL ;
   s: String ;
BEGIN
   i          := 1 ;
   ApuFound   := FALSE ;
   ExitNeeded := TRUE ;
   MainName   := InitString('main') ;
   fi         := StdIn ;
   fo         := StdOut ;
   WHILE GetArg(s, i) DO
      IF EqualArray(s, '-apu')
      THEN
         ApuFound := TRUE
      ELSIF EqualArray(s, '-exit')
      THEN
         ExitNeeded := FALSE
      ELSIF EqualArray(s, '-h')
      THEN
         fprintf0(StdErr, 'gm2lgen [-main function] [-o outputfile] [ inputfile ] [-apu] [-exit]\n') ;
         exit(0)
      ELSIF EqualArray(s, '-o')
      THEN
         INC(i) ;
         IF GetArg(s, i)
         THEN
            OpenOutputFile(s)
         ELSE
            fprintf0(StdErr, 'missing filename option after -o\n') ;
            exit(1)
         END
      ELSIF EqualArray(s, '-main')
      THEN
         INC(i) ;
         IF GetArg(s, i)
         THEN
            MainName := Assign(MainName, s)
         ELSE
            fprintf0(StdErr, 'missing functionname after option -main\n') ;
            exit(1)
         END
      ELSE
         OpenInputFile(s)
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
      Fin(WriteS(fo, Mark(Sprintf1(Mark(InitString('    .global _%s\n')), MainName)))) ;
      Fin(WriteS(fo, Mark(Sprintf1(Mark(InitString('_%s:\n')), MainName)))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    load.d.d   %%ra\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    load.d.d   %%ra\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    load.d.d   %%sp\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    store.d.d  %%ra\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    load.d.d   %%rb\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    load.d.d   %%rc\n')))))) ;  (* so gdb can get hold of this function *)
      GenInitializationCalls(ApuFound) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    store.d.d  %%rc\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    store.d.d  %%rb\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    store.d.d  %%ra\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    store.d.d  %%pc\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    nop\n')))))) ; (* this ensures our PC never goes out of range *)
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    .end\n'))))))
   ELSE
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('int \n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf1(Mark(InitString('%s (argc, argv)\n')), MainName)))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('int   argc ;\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('char  *argv[];\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('{\n')))))) ;
      GenInitializationCalls(ApuFound) ;
      IF ExitNeeded
      THEN
         Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   libc_exit(0);\n'))))))
      END ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   return(0);\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('}\n'))))))
   END;
   Close(fo)
END GenMain ;


(*
   GenExternals - writes out the external prototypes for each module initializer.
*)

PROCEDURE GenExternals (ApuFound: BOOLEAN) ;
VAR
   s   : String ;
   i, n: CARDINAL ;
BEGIN
   n := NoOfItemsInList(FunctionList) ;
   i := 1 ;
   WHILE i<=n DO
      IF ApuFound
      THEN
         Fin(WriteS(fo, Mark(Sprintf1(Mark(InitString('  .extern __M2_%s_init\n')), GetItemFromList(FunctionList, i)))))
      ELSE
         Fin(WriteS(fo, Mark(Sprintf1(Mark(InitString('extern _M2_%s_init(int argc, char *argv[]);\n')), GetItemFromList(FunctionList, i)))))
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
   s   : String ;
   i, n: CARDINAL ;
BEGIN
   n := NoOfItemsInList(FunctionList) ;
   i := 1 ;
   WHILE i<=n DO
      IF ApuFound
      THEN
         Fin(WriteS(fo, Mark(Sprintf1(Mark(InitString('  load.d.d $__M2_%s_init\n')),
                                      GetItemFromList(FunctionList, i))))) ;
         Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('  call\n'))))))
      ELSE
         Fin(WriteS(fo, Mark(Sprintf1(Mark(InitString('    _M2_%s_init(argc, argv);\n')),
                                      GetItemFromList(FunctionList, i)))))
      END ;
      INC(i)
   END
END GenInitializationCalls ;


(*
   BuildFunctionList - reads in the list of functions and stores them.
*)

PROCEDURE BuildFunctionList ;
VAR
   s: String ;
BEGIN
   REPEAT
      s := RemoveWhitePrefix(ReadS(fi)) ;
      IF (NOT Equal(Mark(InitStringChar(Comment)),
                    Mark(Slice(s, 0, Length(Mark(InitStringChar(Comment)))-1)))) AND
         (NOT EqualArray(s, ''))
      THEN
         IncludeItemIntoList(FunctionList, s)
      END
   UNTIL EOF(fi)
END BuildFunctionList ;


BEGIN
   GenMain
END gm2lgen.
