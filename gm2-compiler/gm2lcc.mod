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
   Description: Generates the cc command for linking the suit of modules.
   Last update: Fri Sep 15 14:42:38 BST 1989
*)

FROM libc IMPORT system, exit ;
FROM SYSTEM IMPORT ADR ;
FROM NameKey IMPORT MakeKey, WriteKey, GetKey ;
FROM M2Search IMPORT SetSearchPath, FindSourceFile ;
FROM M2FileName IMPORT CalculateFileName ;
FROM Args IMPORT GetArg ;
FROM StrLib IMPORT StrEqual, StrLen, StrCopy, StrConCat, StrRemoveWhitePrefix, IsSubString ;
FROM FIO IMPORT File, StdIn, StdErr, StdOut, Close, WriteChar, OpenToRead,
                IsNoError, ReadString, WriteString, WriteLine, WriteNBytes, EOF ;
FROM ASCII IMPORT eof, nul, lf ;  (* UNIX *)
FROM M2FileName IMPORT ExtractExtension ;
FROM NumberIO IMPORT CardToStr ;


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
   MaxName     =    4096 ; (* max string              *)
   Comment     =     '#' ; (* Comment identifier.     *)
   MaxArchives =    1000 ;
   MaxCommand  = 65*1024 ;

VAR
   ApuFound      : BOOLEAN ;
   DebugFound    : BOOLEAN ;
   CheckFound    : BOOLEAN ;
   VerboseFound  : BOOLEAN ;
   ProfileFound  : BOOLEAN ;
   LibrariesFound: BOOLEAN ;
   Libraries     : ARRAY [0..MaxName] OF CHAR ;
   MainModule,
   Target        : ARRAY [0..MaxName] OF CHAR ;
   TargetFound   : BOOLEAN ;
   PathFound     : BOOLEAN ;
   ExecCommand   : BOOLEAN ;    (* should we execute the final cmd *)
   UseAr         : BOOLEAN ;    (* use 'ar' and create archive     *)
   IgnoreMain    : BOOLEAN ;    (* ignore main module when linking *)
   Path          : ARRAY [0..MaxName] OF CHAR ;
   StartupFile   : ARRAY [0..MaxName] OF CHAR ;
   Archives      : ARRAY [0..MaxArchives] OF CARDINAL ;
   NoOfArchives  : CARDINAL ;
   Command       : ARRAY [0..MaxCommand] OF CHAR ;
   CommandLength : CARDINAL ;
   fi, fo        : File ;       (* the input and output files      *)



(*
   AddCommand - adds the command, a, to buffer, Command.
*)

PROCEDURE AddCommand (a: ARRAY OF CHAR) ;
VAR
   i, n: CARDINAL ;
BEGIN
   i := 0 ;
   n := StrLen(a) ;
   WHILE i<n DO
      Command[CommandLength] := a[i] ;
      INC(i) ;
      INC(CommandLength)
   END
END AddCommand ;


(*
   FlushCommand - flush the command to the output file,
                  or execute the command.
*)

PROCEDURE FlushCommand ;
BEGIN
   Command[CommandLength] := lf ;
   INC(CommandLength) ;
   Command[CommandLength] := nul ;
   (* don't bother incrementing CommandLength as we don't wish to write nul *)
   IF ExecCommand
   THEN
      IF VerboseFound
      THEN
         IF WriteNBytes(StdErr, CommandLength, ADR(Command))=CommandLength
         THEN
         END
      END ;
      exit( system(ADR(Command)) )
   ELSE
      IF WriteNBytes(fo, CommandLength, ADR(Command))=CommandLength
      THEN
      END
   END
END FlushCommand ;


(*
   AddArchive - adds an archive into the archive list.
*)

PROCEDURE AddArchive (Name: CARDINAL) ;
BEGIN
   IF NoOfArchives=MaxArchives
   THEN
      WriteString(StdErr, 'increase MaxArchives in gm2lcc.mod') ; WriteLine(StdErr) ; Close(StdErr) ;
      exit(1)
   ELSE
      Archives[NoOfArchives] := Name ;
      INC(NoOfArchives)
   END
END AddArchive ;


(*
   Remove - removes a file, a.
*)

PROCEDURE Remove (a: ARRAY OF CHAR) ;
VAR
   buf: ARRAY [0..MaxCommand] OF CHAR ;
BEGIN
   StrCopy('rm -f ', buf) ;
   StrConCat(buf, a, buf) ;
   IF system(ADR(buf))=0
   THEN
   END
END Remove ;


(*
   GenerateCommand - generate the appropriate linkage command
                     with the correct options.
*)

PROCEDURE GenerateCommand ;
BEGIN
   IF ApuFound
   THEN
      AddCommand('ld-is32 -Ttext 0x0 ')
   ELSIF UseAr
   THEN
      AddCommand('ar rc ') ;
      IF TargetFound
      THEN
         (* Remove(Target) ; *)
         AddCommand(Target) ;
         AddCommand(' ')
      ELSE
         WriteString(StdErr, 'need target with ar') ; WriteLine(StdErr) ; Close(StdErr) ;
         exit(1)
      END
   ELSE
      AddCommand('gcc ') ;
      IF DebugFound
      THEN
         AddCommand('-g ')
      END ;
      IF ProfileFound
      THEN
         AddCommand('-p ')
      END ;
      IF TargetFound
      THEN
         AddCommand('-o ') ;
         AddCommand(Target) ;
         AddCommand(' ')
      END ;
      IF ProfileFound
      THEN
         AddCommand('-lgmon ')
      END
   END
END GenerateCommand ;


(*
   WriteArchives - writes out the archive list.
*)

PROCEDURE WriteArchives ;
VAR
   i: CARDINAL ;
   a: ARRAY [0..MaxName] OF CHAR ;
BEGIN
   i := 0 ;
   WHILE i<NoOfArchives DO
      AddCommand('  ') ;
      GetKey(Archives[i], a) ;
      AddCommand(a) ;
      INC(i)
   END ;
END WriteArchives ;


(*
   RemoveLinkOnly - removes the <onlylink> prefix, if present.
*)

PROCEDURE RemoveLinkOnly (VAR a: ARRAY OF CHAR) ;
VAR
   i, l: CARDINAL ;
BEGIN
   IF IsSubString(a, '<onlylink>')
   THEN
      l := StrLen('<onlylink>') ;
      i := 0 ;
      WHILE i<l DO
         a[i] := ' ' ;
         INC(i)
      END ;
      StrRemoveWhitePrefix(a, a)
   END
END RemoveLinkOnly ;


(*
   GenCC - writes out the linkage command for the C compiler.
*)

PROCEDURE GenCC ;
VAR
   a, b: ARRAY [0..MaxName] OF CHAR ;
BEGIN
   GenerateCommand ;
   IF ApuFound
   THEN
      AddCommand(' crt0.obj ') ; AddCommand(StartupFile) ; AddCommand('.obj')
   ELSE
      AddCommand(StartupFile) ; AddCommand('.o')
   END ;
   REPEAT
      ReadString(fi, a) ;
      StrRemoveWhitePrefix(a, a) ;
      IF (a[0]#Comment) AND
         (NOT (IgnoreMain AND StrEqual(a, MainModule))) AND
         (NOT StrEqual(a, ''))
      THEN
         RemoveLinkOnly(a) ;
         StrCopy(a, b) ;
         IF ApuFound
         THEN
            CalculateFileName(a, 'obj', a)
         ELSE
            CalculateFileName(a, 'o', a)
         END ;
         IF FindSourceFile(a, a)
         THEN
            AddCommand('  ') ;
            AddCommand(a)
         ELSE
            StrCopy(b, a) ;
            CalculateFileName(a, 'a', a) ;
            IF FindSourceFile(a, a)
            THEN
               AddArchive(MakeKey(a))
            ELSE
               WriteString(StdErr, 'have not found ') ;
               WriteString(StdErr, b) ;
               WriteString(StdErr, '.o or ') ;
               WriteString(StdErr, b) ;
               WriteString(StdErr, '.a') ; WriteLine(StdErr) ; Close(StdErr) ;
               exit(1)
            END
         END
      END
   UNTIL EOF(fi) ;
   WriteArchives ;
   IF LibrariesFound
   THEN
      AddCommand(' ') ;
      AddCommand(Libraries)
   END ;
   FlushCommand
END GenCC ;


(*
   WriteModuleName - displays a module name, ModuleName, with formatted spaces
                     after the string.
*)

PROCEDURE WriteModuleName (ModuleName: ARRAY OF CHAR) ;
CONST
   MaxSpaces = 20 ;
VAR
   i, High: CARDINAL ;
BEGIN
   WriteString(fo, ModuleName) ;
   High := StrLen(ModuleName) ;
   IF High<MaxSpaces
   THEN
      High := MaxSpaces - High ;
      i := 0 ;
      WHILE i<High DO
         WriteChar(fo, ' ') ;
         INC(i)
      END
   END
END WriteModuleName ;


(*
   CheckCC - checks to see whether all the object files can be found
             for each module.
*)

PROCEDURE CheckCC ;
VAR
   a, b : ARRAY [0..MaxName] OF CHAR ;
   Error: INTEGER ;
BEGIN
   Error := 0 ;
   ReadString(fi, a) ;
   WHILE NOT EOF(fi) DO
      RemoveLinkOnly(a) ;
      IF (a[0]#Comment) AND (NOT StrEqual(a, ''))
      THEN
         WriteString(fo, 'Module ') ;
         WriteModuleName(a) ;
         StrCopy(a, b) ;
         IF ApuFound
         THEN
            CalculateFileName(a, 'obj', a)
         ELSE
            CalculateFileName(a, 'o', a)
         END ;
         IF FindSourceFile(a, a)
         THEN
            WriteString(fo, ' : ') ;
            WriteString(fo, a)
         ELSE
            CalculateFileName(b, 'a', b) ;
            IF FindSourceFile(b, b)
            THEN
               WriteString(fo, ' : ') ;
               WriteString(fo, b)
            ELSE
               WriteString(fo, ' : Not found') ;
               Error := 1
            END
         END ;
         WriteLine(fo)
      END ;
      ReadString(fi, a)
   END ;
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
      WriteString(StdErr, 'cannot get target argument after -o') ; WriteLine(StdErr) ;
      exit(1)
   END ;
   TargetFound := TRUE
END ProcessTarget ;


(*
   StripModuleExtention - strips a .mod from off the end of string, a.
*)

PROCEDURE StripModuleExtention (VAR a: ARRAY OF CHAR) ;
BEGIN
   ExtractExtension(a, '.obj') ;
   ExtractExtension(a, '.o')
END StripModuleExtention ;


(*
   ProcessStartupFile - copies the specified startup file name into StartupFile.
*)

PROCEDURE ProcessStartupFile (i: CARDINAL) ;
BEGIN
   IF GetArg(StartupFile, i)
   THEN
      StripModuleExtention(StartupFile)
   ELSE
      WriteString(StdErr, 'cannot get startup argument after -startup') ; WriteLine(StdErr) ; Close(StdErr) ;
      exit(1)
   END
END ProcessStartupFile ;


(*
   IsALibrary - returns TRUE if, a, is a library. If TRUE we add it to the
                Libraries array.
*)

PROCEDURE IsALibrary (a: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   IF (StrLen(a)>1) AND (a[0]='-') AND (a[1]='l')
   THEN
      LibrariesFound := TRUE ;
      StrConCat(Libraries, a, Libraries) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsALibrary ;


(*
   ScanArguments - scans arguments for flags: -M -g
*)

PROCEDURE ScanArguments ;
VAR
   a        : ARRAY [0..MaxName] OF CHAR ;
   i        : CARDINAL ;
   FoundFile: BOOLEAN ;
BEGIN
   FoundFile := FALSE ;
   i := 1 ;
   WHILE GetArg(a, i) DO
      IF StrEqual(a, '-apu')
      THEN
         ApuFound := TRUE
      ELSIF StrEqual(a, '-g')
      THEN
         DebugFound := TRUE
      ELSIF StrEqual(a, '-c')
      THEN
         CheckFound := TRUE
      ELSIF StrEqual(a, '-main')
      THEN
         INC(i) ;
         IF NOT GetArg(MainModule, i)
         THEN
            WriteString(StdErr, 'expecting modulename after -main option not: ') ;
            WriteString(StdErr, MainModule) ; WriteLine(StdErr) ; Close(StdErr) ;
            exit(1)
         END
      ELSIF StrEqual(a, '-p')
      THEN
         ProfileFound := TRUE
      ELSIF StrEqual(a, '-v')
      THEN
         VerboseFound := TRUE
      ELSIF StrEqual(a, '-exec')
      THEN
         ExecCommand := TRUE
      ELSIF StrEqual(a, '-ignoremain')
      THEN
         IgnoreMain := TRUE
      ELSIF StrEqual(a, '-ar')
      THEN
         UseAr := TRUE
      ELSIF StrEqual(a, '-M')
      THEN
         INC(i) ;
         IF GetArg(a, i)
         THEN
            SetSearchPath(a)
         END
      ELSIF StrEqual(a, '-o')
      THEN
         INC(i) ;                 (* Target found *)
         ProcessTarget(i)
      ELSIF StrEqual(a, '-startup')
      THEN
         INC(i) ;                 (* Target found *)
         ProcessStartupFile(i)
      ELSIF IsALibrary(a)
      THEN
      ELSE
         IF FoundFile
         THEN
            WriteString(StdErr, 'already found filename, unknown option: ') ;
            WriteString(StdErr, a) ; WriteLine(StdErr) ; Close(StdErr) ;
            exit(1)
         ELSE
            (* must be input filename *)
            Close(StdIn) ;
            fi := OpenToRead(a) ;
            IF NOT IsNoError(fi)
            THEN
               WriteString(StdErr, 'failed to open ') ; WriteString(StdErr, a) ; WriteLine(StdErr) ; Close(StdErr) ;
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
   NoOfArchives  := 0 ;
   DebugFound    := FALSE ;
   CheckFound    := FALSE ;
   TargetFound   := FALSE ;
   ProfileFound  := FALSE ;
   IgnoreMain    := FALSE ;
   ApuFound      := FALSE ;
   UseAr         := FALSE ;
   VerboseFound  := FALSE ;
   StrCopy('', MainModule) ;
   StrCopy('mod_init', StartupFile) ;
   fi            := StdIn ;
   fo            := StdOut ;
   ExecCommand   := FALSE ;
   CommandLength := 0 ;
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
