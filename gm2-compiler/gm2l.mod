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
              : [$Date: 2002/04/15 16:26:51 $]
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
*)

IMPORT Break ;
IMPORT DebugPMD ;
IMPORT M2Search ;
IMPORT SArgs ;
FROM libc IMPORT exit ;
FROM M2LexBuf IMPORT OpenSource, CloseSource, GetToken, currenttoken, currentstring ;
FROM M2Reserved IMPORT toktype ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3, printf4 ;
FROM M2FileName IMPORT CalculateFileName, CalculateStemName ;
FROM M2Search IMPORT InitSearchPath, FindSourceFile, PrependSearchPath ;
FROM SArgs IMPORT Narg, GetArg ;
FROM M2Defaults IMPORT GetOptions, GetSearchPath ;
FROM NameKey IMPORT Name, KeyToCharStar, WriteKey, MakeKey, GetKey, makekey, NulName ;
FROM M2Depth IMPORT MakeDependant, GetDepth ;
FROM SFIO IMPORT OpenToWrite, Exists ;
FROM FIO IMPORT File, WriteChar, IsNoError, Close, StdOut ;
FROM StdIO IMPORT PushOutput ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM SYSTEM IMPORT WORD ;
FROM Strings IMPORT String, InitString, KillString, Slice, InitStringCharStar,
                    Mark, EqualArray, string ;


CONST
   Comment      = '#' ;  (* Comment identifier *)
 
TYPE
   Source = POINTER TO source ;
   source = RECORD
               name       : Name ;
               Depth      : CARDINAL ;
               OnlyDef    ,
               ForC,
               UnQualified: BOOLEAN ;
               next       : Source ;
            END ;
 
(* %%%FORWARD%%%
PROCEDURE Open (ModuleName, ExtName: Name; IsDefinition: BOOLEAN) : BOOLEAN ; FORWARD ;
PROCEDURE ScanSources (main: Name) ; FORWARD ;
PROCEDURE ScanImport (p: Source) ; FORWARD ;
PROCEDURE ScanImportsIn (p: Source; extension: Name; IsDefinition: BOOLEAN) : BOOLEAN ; FORWARD ;
PROCEDURE MakeModule (ModuleName: Name) ; FORWARD ;
PROCEDURE WriteFileName (FileName: ARRAY OF CHAR) ; FORWARD ;
PROCEDURE CalculateDepth ; FORWARD ;
PROCEDURE SortSources ; FORWARD ;
PROCEDURE DisplaySources ; FORWARD ;
PROCEDURE ScanArgs() : BOOLEAN ; FORWARD ;
   %%%FORWARD%%% *)

VAR
   fo          : File ;
   main        : Name ;
   Head, Tail  : Source ;    (* head source list *)
   IncludeM2RTS: BOOLEAN ;   (* do we automatically include M2RTS into the top module? *)


(*
   ScanSources - scans all the source files for IMPORTs and places all
                 imports into the Sources array.
*)

PROCEDURE ScanSources (main: Name) ;
VAR
   p: Source ;
BEGIN
   Head := NIL ;
   Tail := NIL ;
   MakeModule(main) ;
   IF IncludeM2RTS
   THEN
      (*
         we should include M2RTS as a dependant module otherwise it simply wont link
      *)
      MakeModule(MakeKey('M2RTS')) ;
      MakeDependant(main, MakeKey('M2RTS'))
   END ;
   p := Head ;
   WHILE p#NIL DO
      ScanImport(p) ;
      p := p^.next
   END
END ScanSources ;


(*
   ScanImport - looks for .def and .mod source files and scans imports of these
                sources.
*)

PROCEDURE ScanImport (p: Source) ;
BEGIN
   IF ScanImportsIn(p, MakeKey('def'), TRUE)
   THEN
   END ;
   (* set OnlyDef to TRUE if we dont see a .mod file *)
   p^.OnlyDef := NOT ScanImportsIn(p, MakeKey('mod'), FALSE)
END ScanImport ;


(*
   ScanImportsIn - scans Source[s] for imports. Any imported file is added
                   to the Sources array.
                   It checks to see whether the definition module
                   contains EXPORT UNQUALIFIED.
*)

PROCEDURE ScanImportsIn (p: Source; extension: Name; IsDefinition: BOOLEAN) : BOOLEAN ;
VAR
   symbol   : Name ;
   lasttoken: toktype ;
BEGIN
   IF Open(p^.name, extension, IsDefinition)
   THEN
      GetToken ;
      LOOP
         lasttoken := eoftok ;
         WHILE (currenttoken#eoftok) AND
               (currenttoken#fromtok) AND
               (currenttoken#endtok) AND
               ((currenttoken#importtok) OR (lasttoken#semicolontok))
            DO
            lasttoken := currenttoken ;
            GetToken
         END ;
         IF currenttoken=fromtok
         THEN
            GetToken ;
            symbol := makekey(currentstring) ;
            MakeModule(symbol) ;
            MakeDependant(p^.name, symbol) ;
            (* eat up to end of IMPORT line *)
            WHILE (currenttoken#eoftok) AND (currenttoken#commatok) DO
               GetToken
            END
         ELSIF currenttoken=importtok
         THEN
            (* might be an import list of modules *)
            REPEAT
               GetToken ;
               symbol := makekey(currentstring) ;
               MakeModule(symbol) ;
               MakeDependant(p^.name, symbol) ;
               GetToken
            UNTIL currenttoken#commatok
         ELSIF currenttoken=exporttok
         THEN
            GetToken ;
            IF IsDefinition AND (currenttoken=unqualifiedtok)
            THEN
               GetToken ;
               p^.UnQualified := TRUE
            END
         ELSIF (currenttoken=begintok) OR (currenttoken=endtok) OR
               (currenttoken=vartok) OR (currenttoken=typetok) OR
               (currenttoken=eoftok)
         THEN
            CloseSource ;
            RETURN( TRUE )
         END
      END
   ELSE
      RETURN( FALSE )
   END
END ScanImportsIn ;


(*
   MakeModule - makes a module for ModuleName.
*)

PROCEDURE MakeModule (ModuleName: Name) ;
VAR
   s: Source ;
BEGIN
   s := Head ;
   WHILE s#NIL DO
      WITH s^ DO
         IF name=ModuleName
         THEN
            RETURN
         ELSE
            s := s^.next
         END
      END
   END ;
   (* not found so create new entry *)
   IF Head=NIL
   THEN
      NEW(Head) ;
      Tail := Head
   ELSE
      NEW(Tail^.next) ;
      Tail := Tail^.next
   END ;
   WITH Tail^ DO
      name        := ModuleName ;
      Depth       := 0 ;
      UnQualified := FALSE ;
      ForC        := FALSE ;
      OnlyDef     := FALSE ;
      next        := NIL
   END
END MakeModule ;


(*
   Open - attempts to open a module, it will return TRUE if the module can be found.
*)

PROCEDURE Open (ModuleName, ExtName: Name; IsDefinition: BOOLEAN) : BOOLEAN ;
VAR
   a, b: String ;
BEGIN
   a := CalculateFileName(InitStringCharStar(KeyToCharStar(ModuleName)),
                          Mark(InitStringCharStar(KeyToCharStar(ExtName)))) ;
   IF FindSourceFile(a, b) AND OpenSource(b)
   THEN
      printf3('%c Module %a in file: %s\n', WORD(Comment), ModuleName, b) ;
      a := KillString(a) ;
      b := KillString(b) ;
      RETURN( TRUE )
   ELSE
      a := KillString(a) ;
      b := KillString(b) ;
      RETURN( FALSE )
   END ;
END Open ;


(*
   CalculateDepth - for each module in the list, calculate the dependancy depth.
*)

PROCEDURE CalculateDepth ;
VAR
   p: Source ;
BEGIN
   p := Head ;
   WHILE p#NIL DO
      WITH p^ DO
         Depth := GetDepth(name)
      END ;
      p := p^.next
   END
END CalculateDepth ;


(*
   Swap - swaps the contents of, p, and, q. The next field remains the same.
*)

PROCEDURE Swap (p, q: Source) ;
VAR
   t: source ;
   n: Source ;
BEGIN
   t := p^ ;
   n := p^.next ;
   p^ := q^ ;
   p^.next := n ;  (* preserve the next field of p *)
   n := q^.next ;
   q^ := t ;
   q^.next := n    (* preserve the next field of q *)
END Swap ;


(*
   SortSources - sorts the module sources into the order of runtime
                 initialization.
*)

PROCEDURE SortSources ;
VAR
   pi, pj: Source ;
BEGIN
   pi := Head ;
   WHILE pi#NIL DO
      pj := Head ;
      WHILE pj#NIL DO
         IF (pi#pj) AND (pi^.Depth>pj^.Depth)
         THEN
            Swap(pi, pj)
         END ;
         pj := pj^.next
      END ;
      pi := pi^.next
   END
END SortSources ;


(*
   DisplaySources - displays the source names 1..SourceNo.
*)

PROCEDURE DisplaySources ;
VAR
   p: Source ;
   s: String ;
BEGIN
   p := Head ;
   WHILE p#NIL DO
      WITH p^ DO
         printf3('%c %a %4d\n', WORD(Comment), name, Depth)
      END ;
      p := p^.next
   END ;
   printf3('%c\n%c Initialization order\n%c\n', WORD(Comment), WORD(Comment), WORD(Comment)) ;
   p := Head ;
   WHILE p#NIL DO
      WITH p^ DO
         (* UnQualified modules do not have an initialization section *)
         IF UnQualified AND OnlyDef
         THEN
            printf1('<onlylink> %a\n', name)
         ELSE
            printf1('%a\n', name)
         END
      END ;
      p := p^.next
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

PROCEDURE OpenOutputFile (s: String) ;
BEGIN
   fo := OpenToWrite(s) ;
   IF IsNoError(fo)
   THEN
      PushOutput(LocalWrite)
   ELSE
      printf1('cannot write to: %s\n', s) ;
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
   s   : String ;
BEGIN
   IncludeM2RTS := TRUE ;
   main := NulName ;
   n := SArgs.Narg() ;
   IF n=1
   THEN
      printf0('Usage: gm2l [-M2RTS] [-Isearchpath] [-o outputfile] modulename\n')
   ELSE
      i := 1 ;
      REPEAT
         IF SArgs.GetArg(s, i)
         THEN
            IF EqualArray(Mark(Slice(s, 0, 2)), '-I')
            THEN
               PrependSearchPath(Slice(s, 2, 0))
            ELSIF EqualArray(s, '-M2RTS')
            THEN
               (* no M2RTS to be automatically linked imported from the main module *)
               IncludeM2RTS := FALSE
            ELSIF EqualArray(s, '-o')
            THEN
               INC(i) ;
               IF GetArg(s, i)
               THEN
                  OpenOutputFile(s)
               END
            ELSE
               main := makekey(string(s))
            END
         END ;
         INC(i)
      UNTIL i>n
   END ;
   RETURN( main#NulName )
END ScanArgs ;


BEGIN
   fo := StdOut ;
   IF ScanArgs()
   THEN
      ScanSources(main) ;
      CalculateDepth ;
      SortSources ;
      DisplaySources
   END ;
   Close(fo)
END gm2l.
