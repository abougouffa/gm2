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
IMPLEMENTATION MODULE M2Search ;


FROM SFIO IMPORT Exists ;
FROM M2Defaults IMPORT GetSearchPath ;
FROM M2Configure IMPORT DefaultLibraryPath ;
FROM Strings IMPORT InitString, KillString, ConCat, ConCatChar, Index, Slice, Add, EqualArray, Dup ;

 
CONST
   Directory    =   '/' ;
 
VAR
   UserPath,
   InitialPath: String ;


(*
   PrependSearchPath - prepends a new path to the initial search path.
*)

PROCEDURE PrependSearchPath (path: String) ;
BEGIN
   UserPath := ConCat(ConCatChar(UserPath, ':'), path)
END PrependSearchPath ;


(*
   FindSourceFile - attempts to locate the source file FileName.
                    If a file is found then TRUE is returned otherwise
                    FALSE is returned.
                    The parameter FullPath is set indicating the
                    absolute location of source FileName.
                    FindSourceFile sets FullPath to a new string if successful.                    
*)

PROCEDURE FindSourceFile (FileName: String;
                          VAR FullPath: String) : BOOLEAN ;
VAR
   CompleteSearchPath: String ;
   start, end        : INTEGER ;
   newpath           : String ;
BEGIN
   IF EqualArray(UserPath, '')
   THEN
      IF EqualArray(InitialPath, '')
      THEN
         CompleteSearchPath := InitString('.')
      ELSE
         CompleteSearchPath := Dup(InitialPath)
      END
   ELSE
      CompleteSearchPath := Add(ConCatChar(UserPath, ':'), InitialPath)
   END ;
   start := 0 ;
   end   := Index(CompleteSearchPath, ':', CARDINAL(start)) ;
   REPEAT
      IF end=-1
      THEN
         end := 0
      END ;
      newpath := Slice(CompleteSearchPath, start, end) ;
      IF EqualArray(newpath, '.')
      THEN
         newpath := KillString(newpath) ;
         newpath := Dup(FileName)
      ELSE
         newpath := ConCat(ConCatChar(newpath, Directory), FileName)
      END ;
      IF Exists(newpath)
      THEN
         FullPath := newpath ;
         CompleteSearchPath := KillString(CompleteSearchPath) ;
         RETURN( TRUE )
      END ;
      newpath := KillString(newpath) ;
      IF end#0
      THEN
         start := end+1 ;
         end   := Index(CompleteSearchPath, ':', CARDINAL(start))
      END
   UNTIL end=0 ;

   newpath := KillString(newpath) ;
   CompleteSearchPath :=  KillString(CompleteSearchPath) ;
   RETURN( FALSE )
END FindSourceFile ;


(*
   InitSearchPath - assigns the search path to Path.
                    The string Path may take the form:

                    Path           ::= IndividualPath { ":" IndividualPath }
                    IndividualPath ::= "." | DirectoryPath
                    DirectoryPath  ::= [ "/" ] Name { "/" Name }
                    Name           ::= Letter { (Letter | Number) }
                    Letter         ::= A..Z | a..z
                    Number         ::= 0..9
*)

PROCEDURE InitSearchPath (Path: String) ;
BEGIN
   IF InitialPath#NIL
   THEN
      InitialPath := KillString(InitialPath)
   END ;
   InitialPath := Path
END InitSearchPath ;


(*
   Init - initializes the search path to M2PATH if it exists otherwise the default path.
*)

PROCEDURE Init ;
BEGIN
   InitialPath := GetSearchPath() ;
   UserPath    := InitString('') ;
   IF InitialPath=NIL
   THEN
      InitialPath := InitString(DefaultLibraryPath)
   END
END Init ;


BEGIN
   Init
END M2Search.
