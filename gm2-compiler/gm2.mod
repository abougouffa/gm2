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
MODULE gm2 ;

(*
   Author     : Gaius Mulley
   Title      : gm2
   Date       : 1987  [$Date: 2001/10/31 09:08:31 $]
   SYSTEM     : UNIX (GNU Modula-2)
   Description: Main module of the compiler, collects arguments and
                starts the compilation.
   Version    : $Revision: 1.1 $
*)

IMPORT DebugPMD ;
IMPORT CmdArgs ;
FROM M2Search IMPORT FindSourceFile ;
FROM M2Options IMPORT IsAnOption, ParseOptions ;
FROM M2Comp IMPORT Compile ;
FROM M2Batch IMPORT MakeProgramSource, MakeImplementationSource ;
FROM SymbolTable IMPORT SetMainModule ;
FROM NameKey IMPORT MakeKey ;
FROM M2FileName IMPORT CalculateFileName, ExtractExtension ;
FROM SArgs IMPORT GetArg, Narg ;
FROM Strings IMPORT String, InitString, string, KillString, EqualArray ;
FROM M2Printf IMPORT fprintf1 ;
FROM M2Quiet IMPORT qprintf1 ;
FROM FIO IMPORT StdErr ;
FROM libc IMPORT exit ;


(*
   CompileModule - attempts to locate a module in, file, and then compile
                   this module. The, file, maybe anywhere on the search path
                   and might be a temporary file. Thus we cannot derive the
                   module name from the file name.
*)

PROCEDURE CompileModule (file, modulename: String) ;
VAR
   Sym     : CARDINAL ;
   basemod,
   basefile,
   FullPath,
   FileName: String ;
BEGIN
   FullPath := NIL ;
   basemod := ExtractExtension(modulename, Mark(InitString('.mod'))) ;
   IF basemod=NIL
   THEN
      basemod := ExtractExtension(modulename, Mark(InitString('.def')))
   END ;
   basefile := ExtractExtension(file, Mark(InitString('.mod'))) ;
   IF basefile=NIL
   THEN
      basefile := ExtractExtension(file, Mark(InitString('.def')))
   END ;
   (* look up definition module, use module name and M2 path to find definition module *)
   FileName := CalculateFileName(modulename, InitString('def')) ;
   IF FindSourceFile(FileName, FullPath)
   THEN
      Sym := MakeImplementationSource(makekey(string(modulename))) ;
      FileName := KillString(FileName) ;
      FileName := CalculateFileName(file, Mark(InitString('.mod'))) ;
      FullPath := KillString(FullPath) ;
      IF FindSourceFile(FileName, FullPath)
      THEN 
         SetMainModule(Sym) ;
         qprintf1('Compiling %s\n', modulename) ;
         Compile(Sym, FullPath)
      ELSE
         fprintf1(StdErr, 'file: %s not found\n', FileName) ;
         exit(1)
      END
   ELSE
      (* look up implementation or program module, use file name as it may have been preprocessed *)
      FileName := KillString(FileName) ;
      FileName := CalculateFileName(file, Mark(InitString('mod'))) ;
      IF FindSourceFile(FileName, FullPath)
      THEN
      	 qprintf1('Compiling %s\n', modulename) ;
         Sym := MakeProgramSource(makekey(string(modulename))) ;
         SetMainModule(Sym) ;
         Compile(Sym, FileName)
      ELSE
         fprintf1(StdErr, 'file: %s not found\n', FileName) ;
         exit(1)
      END
   END ;
   basemod  := KillString(basemod) ;
   basefile := KillString(basefile) ;
   FullPath := KillString(FullPath) ;
   FileName := KillString(FileName)
END CompileModule ;


(*
   StartParsing - scans the command line and processes the arguments.
                  It attempts to compile the first module supplied.
*)

PROCEDURE StartParsing ;
VAR
   s, module: String ;
   n        : CARDINAL ;
BEGIN
   ParseOptions ;
   n := 1 ;
   module := NIL ;
   WHILE GetArg(s, n) AND
         (IsAnOption(s) OR EqualArray(s, '-M') OR EqualArray(s, '-o') OR
          EqualArray(s, '-dumpbase') OR EqualArray(s, '-version'))
   DO
      IF EqualArray(s, '-M') OR EqualArray(s, '-o')
      THEN
         INC(n)
      ELSIF EqualArray(s, '-dumpbase') AND GetArg(module, n+1)
      THEN
         INC(n)
      ELSIF EqualArray(s, '-version')
      THEN
      END ;
      s := KillString(s) ;
      INC(n)
   END ;
   IF GetArg(s, n) AND (NOT IsAnOption(s))
   THEN
      IF EqualArray(module, '')
      THEN
         (* filename is the module *)
         CompileModule(s, s)
      ELSE
         (* filename is different to the modulename *)
         CompileModule(s, module)
      END
   END ;
   s := KillString(s) ;
   module := KillString(module)
END StartParsing ;


BEGIN
   StartParsing
END gm2.
