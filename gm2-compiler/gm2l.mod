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
MODULE gm2l ;
 
(*
   Author     : Gaius Mulley
   Title      : gm2l
   Date       : Date: Sat 16-09-1989 Time: 17:49:34.18
              : [$Date: 2001/10/31 09:08:31 $]
   SYSTEM     : UNIX (GNU Modula-2)
   Description: generates the list of initialization order for the modules.
                The initialization module list is used for two purposes.
                (i)  for linking all the objects or archives
                (ii) for creating the initialization call sequence for each module.
                If a definition module contains EXPORT UNQUALIFIED and there is no
                implementation module found then we assume this definition module
                will match a foreign language, therefore we do not create an
                initialization call but we do link this object/archive.
                This allows us to write definition modules for C libraries.
   Version    : $Revision: 1.1 $
*)

(*
   Log        : $Log: gm2l.mod,v $
   Log        : Revision 1.1  2001/10/31 09:08:31  anoncvs
   Log        : major overhaul of gm2, includes:
   Log        :   moving to a flat Makefile with few if any recusive makes
   Log        :   replaced many modules with their dynamic equivalent
   Log        :   using flex to build the lexical analyser
   Log        :   using ppg to build the top down recursive descent parser
   Log        :   many modifications to p2c to handle opaque types, sets
   Log        :   and forward references.
   Log        :   using new String library which removes any fixed size
   Log        :   character array limits.
   Log        :   using new M2Error library for all error handling
   Log        :   using new print style procedures rather than WriteString NumberIO
   Log        :   combination.
   Log        :   moving sources files into a new directory layout (no more symbolic
   Log        :   links will be used) - this reduces the build time and reduces
   Log        :   confusion.
   Log        :
   Log        : the overhaul is very incomplete and this is an interim checkin
   Log        :
   Log        : Revision 1.6  2001/03/09 12:32:55  anoncvs
   Log        : added FSF copyright notice to all definition, implementation,
   Log        : program modules and Makefile.in
   Log        :
   Log        : Revision 1.5  2000/11/25 19:47:22  anoncvs
   Log        : fixed atan bug and introduced a better method for linking into C libraries
   Log        : via the EXPORT UNQUALIFIED mechanism.
   Log        :
   Log        : Revision 1.4  2000/02/03 11:47:19  gaius
   Log        : minor changes
   Log        :
   Log        : Revision 1.3  1999/12/03 16:49:02  gaius
   Log        : first checkin after writing part of the gcc backend
   Log        :
   Log        : Revision 1.2  1997/07/11 08:03:52  gaius
   Log        : altered gm2l.mod to contain an 8k char path variable and not 79 chars.
   Log        :
   Log        : Revision 1.1.1.1  1996/05/28 10:13:02  gaius
   Log        : Modula-2 compiler sources imported
   Log        :
*)

IMPORT Break ;
IMPORT DebugPMD ;
IMPORT M2Search ;
FROM libc IMPORT exit ;
FROM M2Lex IMPORT OpenSource, GetSymbol, IsSym, SymIs, CloseSource,
                  CurrentSymbol ;
FROM M2FileName IMPORT CalculateFileName ;
FROM StrLib IMPORT StrEqual, StrCopy, StrConCat, StrLen ;
FROM NumberIO IMPORT WriteCard ;
FROM FIO IMPORT File, OpenToWrite, IsNoError, File, WriteChar, Close, StdOut ;
FROM StdIO IMPORT Write, PushOutput ;
FROM StrIO IMPORT WriteString, WriteLn ;
IMPORT Args ;
FROM M2Depth IMPORT MakeDependant, GetDepth ;
FROM CmdArgs IMPORT Narg, GetArg ;
FROM NameKey IMPORT GetKey, MakeKey, WriteKey ;


(* %%%FORWARD%%%
PROCEDURE ScanSources ; FORWARD ;
PROCEDURE ScanImport (s: CARDINAL) ; FORWARD ;
PROCEDURE ScanImportsIn (s: CARDINAL; extension: ARRAY OF CHAR) : BOOLEAN ; FORWARD ;
PROCEDURE MakeModule (ModuleName: CARDINAL) ; FORWARD ;
PROCEDURE FindSourceFile (s: CARDINAL; extension: ARRAY OF CHAR) : BOOLEAN ; FORWARD ;
PROCEDURE WriteFileName (FileName: ARRAY OF CHAR) ; FORWARD ;
PROCEDURE CalculateDepth ; FORWARD ;
PROCEDURE SortSources ; FORWARD ;
PROCEDURE DisplaySources ; FORWARD ;
PROCEDURE ScanArgs() : BOOLEAN ; FORWARD ;
   %%%FORWARD%%% *)

 
CONST
   MaxStringLen = 8192 ;
   MaxNoOfFiles =  100 ;
   Comment      = '# ' ;  (* Comment identifier *)
 
TYPE
   Source = RECORD
               Name       : CARDINAL ;
               Depth      : CARDINAL ;
               OnlyDef    ,
               UnQualified: BOOLEAN ;
            END ;
 
 
VAR
   fo          : File ;
   Main        : ARRAY [0..MaxStringLen] OF CHAR ;
   SourceNo    : CARDINAL ;
   Sources     : ARRAY [1..MaxNoOfFiles] OF Source ;
   IncludeM2RTS: BOOLEAN ;   (* do we automatically include M2RTS into the top module? *)


(*
   ScanSources - scans all the source files for IMPORTs and places all
                 imports into the Sources array.
*)

PROCEDURE ScanSources ;
VAR
   i: CARDINAL ;
BEGIN
   SourceNo := 1 ;
   WITH Sources[SourceNo] DO
      Name        := MakeKey(Main) ;
      Depth       := 0 ;
      UnQualified := FALSE ;
      OnlyDef     := FALSE
   END ;
   IF IncludeM2RTS
   THEN
      (*
         we should include M2RTS as a dependant module otherwise it simply wont link
      *)
      MakeModule(MakeKey('M2RTS')) ;
      MakeDependant(MakeKey(Main), MakeKey('M2RTS'))
   END ;
   i := 0 ;
   WHILE i<SourceNo DO
      INC(i) ;
      ScanImport(i)
   END
END ScanSources ;


(*
   ScanImport - looks for .def and .mod source files and scans imports of these
                sources.
*)
 
PROCEDURE ScanImport (s: CARDINAL) ;
BEGIN
   IF ScanImportsIn(s, 'def')
   THEN
   END ;
   (* set OnlyDef to TRUE if we dont see a .mod file *)
   Sources[s].OnlyDef := NOT ScanImportsIn(s, 'mod')
END ScanImport ;


(*
   ScanImportsIn - scans Source[s] for imports. Any imported file is added
                   to the Sources array.
                   It checks to see whether the definition module
                   contains EXPORT UNQUALIFIED.
*)

PROCEDURE ScanImportsIn (s: CARDINAL; extension: ARRAY OF CHAR) : BOOLEAN ;
VAR
   done : BOOLEAN ;
   mod  : CARDINAL ;
BEGIN
   IF FindSourceFile(s, extension)
   THEN
      done := FALSE ;
      REPEAT
         GetSymbol ;
         WHILE (NOT IsSym('FROM')) AND (NOT IsSym('END')) AND
               (NOT IsSym('IMPORT')) AND (NOT IsSym(';')) AND
               (NOT IsSym('EXPORT'))
         DO
            GetSymbol
         END ;
         IF IsSym('FROM')
         THEN
            GetSymbol ;
            MakeModule(MakeKey(CurrentSymbol)) ;
            MakeDependant(Sources[s].Name, MakeKey(CurrentSymbol)) ;
            (* Eat up to end of IMPORT line *)
            WHILE NOT IsSym(';') DO
               GetSymbol
            END
         ELSIF IsSym('IMPORT')
         THEN
            REPEAT
               GetSymbol ;
               MakeModule(MakeKey(CurrentSymbol)) ;
               MakeDependant(Sources[s].Name, MakeKey(CurrentSymbol)) ;
               GetSymbol
            UNTIL IsSym(';')
         ELSIF IsSym('EXPORT')
         THEN
            GetSymbol ;
            IF IsSym('UNQUALIFIED')
            THEN
               GetSymbol ;
               Sources[s].UnQualified := TRUE
            END
         ELSIF IsSym('BEGIN') OR IsSym('END') OR IsSym('VAR') OR IsSym('TYPE')
         THEN
            done := TRUE
         END
      UNTIL done ;
      CloseSource ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END ScanImportsIn ;


(*
   MakeModule - makes a module for ModuleName.
*)

PROCEDURE MakeModule (ModuleName: CARDINAL) ;
VAR
   i    : CARDINAL ;
   Found: BOOLEAN ;
BEGIN
   Found := FALSE ;
   i := 1 ;
   WHILE (i<=SourceNo) AND (NOT Found) DO
      WITH Sources[i] DO
         IF Name=ModuleName
         THEN
            Found := TRUE
         ELSE
            INC(i)
         END
      END
   END ;
   IF NOT Found
   THEN
      (* Not found so create *)
      INC(SourceNo) ;
      WITH Sources[SourceNo] DO
         Name        := ModuleName ;
         Depth       := 0 ;
         UnQualified := FALSE ;
         OnlyDef     := FALSE
      END
   END
END MakeModule ;


(*
   FindSourceFile - attempts to find the source file for source, s.
*)

PROCEDURE FindSourceFile (s: CARDINAL; extension: ARRAY OF CHAR) : BOOLEAN ;
VAR
   FileName,
   Path    : ARRAY [0..MaxStringLen] OF CHAR ;
BEGIN
   GetKey(Sources[s].Name, FileName) ;
   WriteString(Comment) ;
   CalculateFileName(FileName, extension, FileName) ;
   IF M2Search.FindSourceFile(FileName, Path) AND OpenSource(Path)
   THEN
      WriteString('Module ') ; WriteFileName(FileName) ;
      WriteString(' In file: ') ; WriteString(Path) ; WriteLn ;
      RETURN( TRUE )
   ELSE
      (* WriteString(Path) ; WriteString(' : ') ; *)
   END ;
   RETURN( FALSE )
END FindSourceFile ;


(*
   WriteFileName - displays a file name, FileName, with formatted spaces
                   after the string.
*)

PROCEDURE WriteFileName (FileName: ARRAY OF CHAR) ;
CONST
   MaxSpaces = 20 ;
VAR
   i, High: CARDINAL ;
BEGIN
   WriteString(FileName) ;
   High := StrLen(FileName) ;
   IF High<MaxSpaces
   THEN
      High := MaxSpaces - High ;
      i := 0 ;
      WHILE i<High DO
         Write(' ') ;
         INC(i)
      END
   END
END WriteFileName ;


(*
   CalculateDepth - Calculates the modules depth.
*)

PROCEDURE CalculateDepth ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE i<=SourceNo DO
      WITH Sources[i] DO
         Depth := GetDepth(Name)
      END ;
      INC(i)
   END
END CalculateDepth ;


(*
   SortSources - sorts the module sources into the order of runtime
                 initialization.
*)

PROCEDURE SortSources ;
VAR
   i, j: CARDINAL ;
   t   : Source ;
BEGIN
   i := 1 ;
   WHILE i<=SourceNo DO
      j := 1 ;
      WHILE j<=SourceNo DO
         IF (i#j) AND (Sources[i].Depth>Sources[j].Depth)
         THEN
            t := Sources[i] ;
            Sources[i] := Sources[j] ;
            Sources[j] := t
         END ;
         INC(j)
      END ;
      INC(i)
   END
END SortSources ;


(*
   DisplaySources - displays the source names 1..SourceNo.
*)

PROCEDURE DisplaySources ;
VAR
   i: CARDINAL ;
   a: ARRAY [0..MaxStringLen] OF CHAR ;
BEGIN
   i := 1 ;
   WHILE i<=SourceNo DO
      WITH Sources[i] DO
         GetKey(Name, a) ;
         WriteString(Comment) ;
         WriteFileName(a) ; WriteCard(Depth, 4) ; WriteLn
      END ;
      INC(i)
   END ;
   WriteString(Comment) ; WriteLn ;
   WriteString(Comment) ; WriteString('Initialization order') ; WriteLn ;
   WriteString(Comment) ; WriteLn ;
   i := 1 ;
   WHILE i<=SourceNo DO
      WITH Sources[i] DO
         (* UnQualified modules do not have an initialization section *)
         IF UnQualified AND OnlyDef
         THEN
            WriteString('<onlylink> ') ; WriteKey(Name) ; WriteLn
         ELSE
            WriteKey(Name) ; WriteLn
         END
      END ;
      INC(i)
   END
END DisplaySources ;


(*
   LocalWrite - writes, ch, to the file, fo.
*)

PROCEDURE LocalWrite (ch: CHAR) ;
BEGIN
   WriteChar(fo, ch)
END LocalWrite ;


(*
   OpenOutputFile - attempts to open an output file.
*)

PROCEDURE OpenOutputFile (a: ARRAY OF CHAR) ;
BEGIN
   fo := OpenToWrite(a) ;
   IF IsNoError(fo)
   THEN
      PushOutput(LocalWrite)
   ELSE
      WriteString('cannot write to: ') ; WriteString(a) ; WriteLn ;
      exit(1)
   END
END OpenOutputFile ;


(*
   ScanArgs - scans the argument list and returns TRUE if the main source
              module is found.
*)

PROCEDURE ScanArgs () : BOOLEAN ;
VAR
   i, n: CARDINAL ;
   a   : ARRAY [0..MaxStringLen] OF CHAR ;
BEGIN
   IncludeM2RTS := TRUE ;
   StrCopy('', Main) ;
   n := Args.Narg() ;
   IF n=1
   THEN
      WriteString('Usage: gm2l [-M2RTS] [-M searchpath] [-o outputfile] modulename') ; WriteLn
   ELSE
      i := 1 ;
      REPEAT
         IF Args.GetArg(a, i)
         THEN
            (* WriteString('Argument found: ') ; WriteString(a) ; WriteLn ; *)
            IF StrEqual(a, '-M')
            THEN
               INC(i) ;
               IF Args.GetArg(a, i)
               THEN
                  M2Search.SetSearchPath(a)
               ELSE
                  WriteString('Expecting path after -M flag') ; WriteLn
               END
               (* ; WriteString('Paths are: ') ; WriteString(a) ; WriteLn *)
            ELSIF StrEqual(a, '-M2RTS')
            THEN
               (* no M2RTS to be automatically linked imported from the main module *)
               IncludeM2RTS := FALSE
            ELSIF StrEqual(a, '-o')
            THEN
               INC(i) ;
               IF Args.GetArg(a, i)
               THEN
                  OpenOutputFile(a)
               END
            ELSE
               StrCopy(a, Main)
            END
         END ;
         INC(i)
      UNTIL i>n
   END ;
   RETURN( NOT StrEqual('', Main) )
END ScanArgs ;


BEGIN
   fo := StdOut ;
   IF ScanArgs()
   THEN
      ScanSources ;
      CalculateDepth ;
      SortSources ;
      DisplaySources
   END ;
   Close(fo)
END gm2l.
