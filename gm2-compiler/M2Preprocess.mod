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

IMPLEMENTATION MODULE M2Preprocess ;


FROM SYSTEM IMPORT WORD ;

FROM Strings IMPORT string, InitString, Mark, KillString, EqualArray, InitStringCharStar,
                    Dup, ConCat, ConCatChar, RIndex, Slice ;

FROM choosetemp IMPORT make_temp_file ;
FROM pexecute IMPORT pexecute ;
FROM libc IMPORT system, exit, unlink ;
FROM Lists IMPORT List, InitList, KillList, IncludeItemIntoList, ForeachItemInListDo ;
FROM M2RTS IMPORT InstallTerminationProcedure, Terminate ;
FROM FIO IMPORT StdErr, StdOut ;
FROM M2Printf IMPORT fprintf1 ;
FROM M2Options IMPORT CppCommandLine, Verbose ;
FROM NameKey IMPORT MakeKey, KeyToCharStar ;


VAR
   ListOfFiles: List ;


(*
   OnExitDelete - 
*)

PROCEDURE OnExitDelete (filename: String) : String ;
BEGIN
   IncludeItemIntoList(ListOfFiles, filename) ;
   RETURN( filename )
END OnExitDelete ;


(*
   RemoveFile - removes a single file, s.
*)

PROCEDURE RemoveFile (w: WORD) ;
VAR
   s: String ;
BEGIN
   s := w ;
   IF unlink(string(s))#0
   THEN
   END
END RemoveFile ;


(*
   RemoveFiles - 
*)

PROCEDURE RemoveFiles ;
BEGIN
   ForeachItemInListDo(ListOfFiles, RemoveFile)
END RemoveFiles ;


(*
   PreprocessModule - preprocess a file, filename, returning the new filename
                      of the preprocessed file.
                      Preprocessing will only occur if requested by the user.
                      If no preprocessing was requested then filename is returned.
                      If preprocessing occurs then a temporary file is created
                      and its name is returned.
                      All temporary files will be deleted when the compiler exits.
*)

PROCEDURE PreprocessModule (filename: String) : String ;
VAR
   tempfile,
   command,
   commandLine: String ;
   pos        : CARDINAL ;
BEGIN
   command := CppCommandLine() ;
   IF EqualArray(command, '')
   THEN
      RETURN( filename )
   ELSE
      tempfile := InitStringCharStar(make_temp_file(KeyToCharStar(MakeKey('cpp')))) ;
      commandLine := Dup(command) ;
      pos := RIndex(commandLine,  ' ', 0) ;
      IF pos>0
      THEN
         commandLine := Slice(commandLine, 0, pos)
      END ;
      commandLine := ConCat(ConCatChar(ConCat(ConCatChar(Dup(commandLine), ' '), filename),
                                       ' '),
                            tempfile) ;
(*  use pexecute in the future
      res := pexecute(string(Slice(commandLine, 0, Index(commandLine, ' ', 0))), etc etc );
*)
      (* for now we'll use system *)
      IF Verbose
      THEN
         fprintf1(StdOut, "%s\n", commandLine)
      END ;
      IF system(string(commandLine))#0
      THEN
         fprintf1(StdErr, 'C preprocessor failed when preprocessing %s\n', filename) ;
         exit(1)
      END ;
      commandLine := KillString(commandLine) ;
      RETURN( OnExitDelete(tempfile) )
   END
END PreprocessModule ;


BEGIN
   InitList(ListOfFiles) ;
   InstallTerminationProcedure(RemoveFiles)
END M2Preprocess.
