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
MODULE gm2lcc ;

(*
   Author     : Gaius Mulley
   Title      : gm2lcc
   Date       : Fri Jul 24 11:45:08 BST 1992
   Description: Generates the cc command for linking all the modules.
*)

FROM libc IMPORT system, exit ;
FROM SYSTEM IMPORT ADR ;
FROM NameKey IMPORT Name, MakeKey, WriteKey, GetKey ;
FROM M2Search IMPORT FindSourceFile, PrependSearchPath ;
FROM M2FileName IMPORT CalculateFileName ;
FROM SArgs IMPORT GetArg ;
FROM StrLib IMPORT StrEqual, StrLen, StrCopy, StrConCat, StrRemoveWhitePrefix, IsSubString ;
FROM FIO IMPORT File, StdIn, StdErr, StdOut, Close, IsNoError, EOF, WriteString, WriteLine ;
FROM SFIO IMPORT OpenToRead, WriteS, ReadS ;
FROM ASCII IMPORT nul ;
FROM M2FileName IMPORT ExtractExtension ;
FROM DynamicStrings IMPORT String, InitString, KillString, ConCat, ConCatChar, Length, Slice, Equal, EqualArray, RemoveWhitePrefix, string, Mark, InitStringChar, Dup, Mult, Assign ;
FROM FormatStrings IMPORT Sprintf1 ;
FROM M2Printf IMPORT fprintf0, fprintf1, fprintf2, fprintf3, fprintf4 ;


(* %%%FORWARD%%%
PROCEDURE ScanSources ; FORWARD ;
PROCEDURE ScanImport (s: CARDINAL) ; FORWARD ;
PROCEDURE MakeModule (ModuleName: CARDINAL) ; FORWARD ;
PROCEDURE WriteFileName (FileName: ARRAY OF CHAR) ; FORWARD ;
PROCEDURE CalculateDepth ; FORWARD ;
PROCEDURE SortSources ; FORWARD ;
PROCEDURE DisplaySources ; FORWARD ;
   %%%FORWARD%%% *)

CONST
   Comment     =     '#' ;      (* Comment leader.                 *)
   MaxSpaces   =      20 ;      (* Maximum spaces after a module   *)
                                (* name.                           *)

VAR
   ApuFound      : BOOLEAN ;
   DebugFound    : BOOLEAN ;
   CheckFound    : BOOLEAN ;
   VerboseFound  : BOOLEAN ;
   ProfileFound  : BOOLEAN ;
   LibrariesFound: BOOLEAN ;
   TargetFound   : BOOLEAN ;
   PathFound     : BOOLEAN ;
   ExecCommand   : BOOLEAN ;    (* should we execute the final cmd *)
   UseAr         : BOOLEAN ;    (* use 'ar' and create archive     *)
   IgnoreMain    : BOOLEAN ;    (* ignore main module when linking *)
   Archives,
   Path,
   StartupFile,
   Libraries,
   MainModule,
   Command,
   Target        : String ;
   fi, fo        : File ;       (* the input and output files      *)


(*
   FlushCommand - flush the command to the output file,
                  or execute the command.
*)

PROCEDURE FlushCommand ;
BEGIN
   IF ExecCommand
   THEN
      IF VerboseFound
      THEN
         Command := WriteS(StdOut, Command) ;
         fprintf0(StdOut, '\n')
      END ;
      exit( system(string(Command)) )
   ELSE
      Command := WriteS(fo, Command)
   END
END FlushCommand ;


(*
   GenerateCommand - generate the appropriate linkage command
                     with the correct options.
*)

PROCEDURE GenerateCommand ;
BEGIN
   IF ApuFound
   THEN
      Command := InitString('ld-is32 -Ttext 0x0 ')
   ELSIF UseAr
   THEN
      Command := InitString('ar rc ') ;
      IF TargetFound
      THEN
         Command := ConCat(Command, Target) ;
         Command := ConCatChar(Command, ' ')
      ELSE
         WriteString(StdErr, 'need target with ar') ; WriteLine(StdErr) ; Close(StdErr) ;
         exit(1)
      END
   ELSE
      Command := InitString('gcc ') ;
      IF DebugFound
      THEN
         Command := ConCat(Command, Mark(InitString('-g ')))
      END ;
      IF ProfileFound
      THEN
         Command := ConCat(Command, Mark(InitString('-p ')))
      END ;
      IF TargetFound
      THEN
         Command := ConCat(Command, Mark(InitString('-o '))) ;
         Command := ConCat(Command, Target) ;
         Command := ConCatChar(Command, ' ')
      END ;
      IF ProfileFound
      THEN
         Command := ConCat(Command, Mark(InitString('-lgmon ')))
      END
   END
END GenerateCommand ;


(*
   RemoveLinkOnly - removes the <onlylink> prefix, if present.
                    Otherwise, s, is returned.
*)

PROCEDURE RemoveLinkOnly (s: String) : String ;
VAR
   t: String ;
BEGIN
   t := InitString('<onlylink>') ;
   IF Equal(Mark(Slice(s, 0, Length(t)-1)), t)
   THEN
      RETURN( RemoveWhitePrefix(Slice(Mark(s), Length(t), 0)) )
   ELSE
      RETURN( s )
   END
END RemoveLinkOnly ;


(*
   GenCC - writes out the linkage command for the C compiler.
*)

PROCEDURE GenCC ;
VAR
   s, t, u: String ;
BEGIN
   GenerateCommand ;
   IF ApuFound
   THEN
      Command := ConCat(Command, Mark(Sprintf1(Mark(InitString(' crt0.obj %s.obj')),
                                               StartupFile)))
   ELSE
      Command := ConCat(Command, Mark(Sprintf1(Mark(InitString('%s.o')),
                                               StartupFile)))
   END ;
   REPEAT
      s := RemoveWhitePrefix(ReadS(fi)) ;
      IF (NOT Equal(Mark(InitStringChar(Comment)), Mark(Slice(s, 0, Length(Mark(InitStringChar(Comment))))))) AND
         (NOT (IgnoreMain AND Equal(s, MainModule))) AND (NOT EqualArray(s, ''))
      THEN
         s := RemoveLinkOnly(s) ;
         t := Dup(s) ;
         IF ApuFound
         THEN
            t := CalculateFileName(s, Mark(InitString('obj')))
         ELSE
            t := CalculateFileName(s, Mark(InitString('o')))
         END ;
         IF FindSourceFile(t, u)
         THEN
            Command := ConCat(ConCatChar(Command, ' '), u) ;
            u := KillString(u)
         ELSE
            t := KillString(t) ;
            (* try finding .a archive *)
            t := CalculateFileName(s, Mark(InitString('a'))) ;
            IF FindSourceFile(t, u)
            THEN
               Archives := ConCatChar(ConCat(Archives, u), ' ') ;
               u := KillString(u)
            ELSE
               fprintf2(StdErr, 'cannot find %s.o or %s.a\n', s, s) ;
               Close(StdErr) ;
               exit(1)
            END
         END ;
         t := KillString(t)
      END
   UNTIL EOF(fi) ;
   Command := ConCat(Command, Archives) ;
   IF LibrariesFound
   THEN
      Command := ConCat(ConCatChar(Command, ' '), Libraries)
   END ;
   FlushCommand
END GenCC ;


(*
   WriteModuleName - displays a module name, ModuleName, with formatted spaces
                     after the string.
*)

PROCEDURE WriteModuleName (ModuleName: String) ;
BEGIN
   ModuleName := WriteS(fo, ModuleName) ;
   IF KillString(WriteS(fo, Mark(Mult(Mark(InitString(' ')), MaxSpaces-Length(ModuleName)))))=NIL
   THEN
   END
END WriteModuleName ;


(*
   CheckCC - checks to see whether all the object files can be found
             for each module.
*)

PROCEDURE CheckCC ;
VAR
   s, t, u: String ;
   Error  : INTEGER ;
BEGIN
   Error := 0 ;
   REPEAT
      s := RemoveWhitePrefix(ReadS(fi)) ;
      IF Equal(Mark(InitStringChar(Comment)), Mark(Slice(s, 0, Length(Mark(InitStringChar(Comment)))-1))) AND
         (NOT EqualArray(s, ''))
      THEN
         s := RemoveLinkOnly(s) ;
         t := Dup(s) ;
         IF ApuFound
         THEN
            t := CalculateFileName(s, Mark(InitString('obj')))
         ELSE
            t := CalculateFileName(s, Mark(InitString('o')))
         END ;
         IF FindSourceFile(t, u)
         THEN
            IF KillString(WriteS(fo, Mark(Sprintf1(Mark(InitString(' : %s\n')), u))))=NIL
            THEN
            END ;
            u := KillString(u)
         ELSE
            t := KillString(t) ;
            (* try finding .a archive *)
            t := Assign(t, s) ;
            t := CalculateFileName(s, Mark(InitString('a'))) ;
            IF FindSourceFile(t, u)
            THEN
               IF KillString(WriteS(fo, Mark(Sprintf1(Mark(InitString(' : %s\n')), u))))=NIL
               THEN
               END ;
               u := KillString(u)
            ELSE
               IF KillString(WriteS(fo, Mark(InitString(' : Not found\n'))))=NIL
               THEN
               END ;
               Error := 1
            END
         END
      END
   UNTIL EOF(fi) ;
   Close(fo) ;
   exit(Error)
END CheckCC ;


(*
   ProcessTarget - copies the specified target file into Target
                   and sets the boolean TargetFound.
*)

PROCEDURE ProcessTarget (i: CARDINAL) ;
BEGIN
   IF NOT GetArg(Target, i)
   THEN
      fprintf0(StdErr, 'cannot get target argument after -o\n') ;
      Close(StdErr) ;
      exit(1)
   END ;
   TargetFound := TRUE
END ProcessTarget ;


(*
   StripModuleExtension - returns a String without an extension from, s.
                          It only considers '.obj' and '.o' as extensions.
*)

PROCEDURE StripModuleExtension (s: String) : String ;
BEGIN
   RETURN(
          ExtractExtension(ExtractExtension(s, Mark(InitString('.o'))),
                           Mark(InitString('.obj')))
         )
END StripModuleExtension ;


(*
   ProcessStartupFile - copies the specified startup file name into StartupFile.
*)

PROCEDURE ProcessStartupFile (i: CARDINAL) ;
BEGIN
   IF GetArg(StartupFile, i)
   THEN
      StartupFile := StripModuleExtension(StartupFile)
   ELSE
      fprintf0(StdErr, 'cannot get startup argument after -startup\n') ;
      Close(StdErr) ;
      exit(1)
   END
END ProcessStartupFile ;


(*
   IsALibrary - returns TRUE if, a, is a library. If TRUE we add it to the
                Libraries string.
*)

PROCEDURE IsALibrary (s: String) : BOOLEAN ;
BEGIN
   IF EqualArray(Mark(Slice(s, 0, 1)), '-l')
   THEN
      LibrariesFound := TRUE ;
      Libraries := ConCat(Libraries, s) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsALibrary ;


(*
   ScanArguments - scans arguments for flags: -I -g
*)

PROCEDURE ScanArguments ;
VAR
   s        : String ;
   i        : CARDINAL ;
   FoundFile: BOOLEAN ;
BEGIN
   FoundFile := FALSE ;
   i := 1 ;
   WHILE GetArg(s, i) DO
      IF EqualArray(s, '-apu')
      THEN
         ApuFound := TRUE
      ELSIF EqualArray(s, '-g')
      THEN
         DebugFound := TRUE
      ELSIF EqualArray(s, '-c')
      THEN
         CheckFound := TRUE
      ELSIF EqualArray(s, '-main')
      THEN
         INC(i) ;
         IF NOT GetArg(MainModule, i)
         THEN
            fprintf0(StdErr, 'expecting modulename after -main option\n') ;
            Close(StdErr) ;
            exit(1)
         END
      ELSIF EqualArray(s, '-p')
      THEN
         ProfileFound := TRUE
      ELSIF EqualArray(s, '-v')
      THEN
         VerboseFound := TRUE
      ELSIF EqualArray(s, '-exec')
      THEN
         ExecCommand := TRUE
      ELSIF EqualArray(s, '-ignoremain')
      THEN
         IgnoreMain := TRUE
      ELSIF EqualArray(s, '-ar')
      THEN
         UseAr := TRUE
      ELSIF EqualArray(Mark(Slice(s, 0, 2)), '-I')
      THEN
         PrependSearchPath(Slice(s, 2, 0))
      ELSIF EqualArray(s, '-o')
      THEN
         INC(i) ;                 (* Target found *)
         ProcessTarget(i)
      ELSIF EqualArray(s, '-startup')
      THEN
         INC(i) ;                 (* Target found *)
         ProcessStartupFile(i)
      ELSIF IsALibrary(s)
      THEN
      ELSE
         IF FoundFile
         THEN
            fprintf1(StdErr, 'already specified input filename, unknown option (%s)\n', s) ;
            Close(StdErr) ;
            exit(1)
         ELSE
            (* must be input filename *)
            Close(StdIn) ;
            fi := OpenToRead(s) ;
            IF NOT IsNoError(fi)
            THEN
               fprintf1(StdErr, 'failed to open %s\n', s) ;
               Close(StdErr) ;
               exit(1)
            END ;
            FoundFile := TRUE
         END
      END ;
      INC(i)
   END
END ScanArguments ;


(*
   Init - initializes the global variables.
*)

PROCEDURE Init ;
BEGIN
   DebugFound    := FALSE ;
   CheckFound    := FALSE ;
   TargetFound   := FALSE ;
   ProfileFound  := FALSE ;
   IgnoreMain    := FALSE ;
   ApuFound      := FALSE ;
   UseAr         := FALSE ;
   VerboseFound  := FALSE ;
   MainModule    := InitString('') ;
   StartupFile   := InitString('mod_init') ;
   fi            := StdIn ;
   fo            := StdOut ;
   ExecCommand   := FALSE ;

   Archives      := NIL ;
   Path          := NIL ;
   Libraries     := NIL ;
   Command       := NIL ;
   Target        := NIL ;

   ScanArguments ;
   IF CheckFound
   THEN
      CheckCC
   ELSE
      GenCC
   END ;
   Close(fo)
END Init ;


BEGIN
   Init
END gm2lcc.
