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
MODULE m2m ;
 
(*
   Author     : Gaius Mulley
   Title      : m2m
   Date       : 3/4/86  [$Date: 2001/10/31 09:08:31 $]
   SYSTEM     : GNU Modula-2
   Description: Generates a makefile by examining the imports within Modula-2 source.
*)
 
FROM ASCII IMPORT nul, tab ;
FROM Storage IMPORT ALLOCATE ;
FROM M2LexBuf IMPORT OpenSource, CloseSource, GetToken, currenttoken, currentstring ;
FROM M2Reserved IMPORT toktype ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3, printf4 ;
FROM M2FileName IMPORT CalculateFileName, CalculateStemName ;
FROM M2Search IMPORT InitSearchPath, FindSourceFile ;
FROM SArgs IMPORT Narg, GetArg ;
FROM M2Defaults IMPORT GetOptions, GetSearchPath ;
FROM NameKey IMPORT Name, KeyToCharStar, WriteKey, MakeKey, GetKey, makekey, NulName ;
FROM SymbolKey IMPORT SymbolTree, InitTree, GetSymKey, PutSymKey, NulKey ;
FROM Lists IMPORT List, InitList, IncludeItemIntoList, NoOfItemsInList, GetItemFromList, IsItemInList, RemoveItemFromList, KillList ;
FROM Strings IMPORT String, InitString, InitStringCharStar, KillString, Add, Length, EqualArray, Mark, string, Dup, Equal ;
FROM FormatStrings IMPORT Sprintf2 ;
FROM StrLib IMPORT StrLen ;
FROM StrIO IMPORT WriteString ;
FROM StdIO IMPORT Write ;
FROM SFIO IMPORT WriteS ;
FROM FIO IMPORT StdOut ;
FROM M2Configure IMPORT UsingGCCBackend ;
FROM libc IMPORT exit ;


CONST
   MaxLineBreak =   70 ;     (* Any line whose columns is beyond this is    *)
                             (* broken into multiple lines.                 *)

TYPE
   PtrToNode = POINTER TO node ;

   PtrToSource = POINTER TO source ;
   source      =            RECORD
                               ModName    : Name ;
                               Ext        : Name ; 
                               FullPath   : Name ;
                               Depth      : CARDINAL ;
                               SourceNode : PtrToNode ;
                               Next       : PtrToSource ;
                            END ;

   node      = RECORD
                  Source: PtrToSource ;
                  Sons  : List ;       (* sons of this node (PtrToNode) *)
                  Father: PtrToNode ;  (* Father of this node *)
               END ;

VAR
   main         : PtrToNode ;
   DefExistsTree: SymbolTree ; (* Table of definition modules found or not   *)
   ModExistsTree: SymbolTree ; (* Table of imp/program modules found or not  *)
   Files        : PtrToSource ;
   KeyFound,
   KeyNotFound,
   ExeName,
   ObjName,
   DefName,
   ModName      : Name ;
   CurrentArg   : CARDINAL ;
   ProgName,
   GM2Path,
   M2MPath,
   DestDirectory: String ;
   DependOnly,
   GM2PathFound,
   M2MPathFound : BOOLEAN ;


(*
   NewNode - creates a new node, n, with zero sons.
*)
 
PROCEDURE NewNode () : PtrToNode ;
VAR
   n: PtrToNode ;
BEGIN
   NEW(n) ;
   WITH n^ DO
      InitList(Sons) ;
      Source := NIL ;
      Father := NIL ;
   END ;
   RETURN( n )
END NewNode ;
 

(*
   NewSource - creates a new source file and returns it.
*)
 
PROCEDURE NewSource () : PtrToSource ;
VAR
   s: PtrToSource ;
BEGIN
   NEW(s) ;
   WITH s^ DO
      ModName    := NulName ;
      Ext        := NulName ;
      FullPath   := NulName ;
      Depth      := 0 ;
      SourceNode := NIL ;
   END ;
   RETURN( s )
END NewSource ;
 

(*
   CreateSon - returns TRUE if we create a new son.
*)

PROCEDURE CreateSon (VAR son: PtrToNode;
                     ModuleName, ExtName: Name;
                     n: PtrToNode; Level: CARDINAL) : BOOLEAN ;
VAR
   name: String ;
   file: PtrToSource ;
BEGIN
   name := CalculateFileName(InitStringCharStar(KeyToCharStar(ModuleName)),
                             Mark(InitStringCharStar(KeyToCharStar(ExtName)))) ;
   IF InFileTable(ModuleName, ExtName, file)
   THEN
      IF file^.Depth<Level
      THEN
         (*
            Move SubTree file^.SourceNode further down the Tree to
            be a son of n. But since file^.SourceNode is higher up
            the Tree we must make certain that n is not a SubNode of
            file^.SourceNode. Otherwise we would be in serious
            trouble if we attempted to move a SubTree under those
            conditions.
         *)
         IF NOT SubNode(file^.SourceNode, n)
         THEN
            printf4('# moving subtree %a.%a to be a SubTree of %a.%a\n',
                    file^.ModName, file^.Ext,
                    n^.Source^.ModName, n^.Source^.Ext) ;

            (* Move file^.SourceNode SubTree to be a son of n.
               Need to delete file^.SourceNode from its father.
            *)
            DeleteFromFather(file^.SourceNode) ;

            (* Delete pseudo sons - only need proper son if available *)
            DeleteSon(n, file^.SourceNode) ;

            (* Now add file^.SourceNode to be a son of n. *)
            AddSon(n, file^.SourceNode) ;
            file^.SourceNode^.Father := n ;

            (* Now increase the Depth of file^.SourceNode and children.
               They all need to increased by Level-file^.Depth.
            *)
            IncreaseLevel( file^.SourceNode, Level-file^.Depth )
         ELSE
            printf1('# found circular import: %s\n', name) ;
         END
      ELSE
         (*
            Pseudo son - son does not acknowledge his father in this case.
            We need this because the father is still dependant upon son.
            Remember that the son has another father deeper down the tree.
         *)
         IF NOT IsASon(n, file^.SourceNode)
         THEN
            son := file^.SourceNode ;
            AddSon(n, file^.SourceNode)
         END
         (* Do not set the father field here for reason above. *)
      END ;
      name := KillString(name) ;
      RETURN( FALSE )
   ELSE
      son := NewNode() ;
      AddSon(n, son) ;
      son^.Father := n ;
      file := NewSource() ;
      WITH file^ DO
         ModName    := ModuleName ;
         Ext        := ExtName ;
         Depth      := Level ;
         SourceNode := son ;
         Next       := Files ;
      END ;
      Files := file ;
      son^.Source := file ;

      (* these last few statements have been taken from ImportViaMod and ImportViaDef *)
      IF (ExtName=ModName) OR (ExtName=ObjName)
      THEN
         IF DefExists(ModuleName)
         THEN
            MakeDef(son^.Source^.ModName, son, Level+1)
         END
      END ;
      (*
         We also now need to check that the object node is
         also in the graph of dependency, since we can only *link* the
         final main module when *all* implementation modules have been
         compiled. Defs can be in the dependency without their Imps
         using a tree strategy. We must ensure that the objects are
         also included.
      *)
      MakeObject(son^.Source^.ModName, main, 1) ;   (* attach the object to the executable *)

      name := KillString(name) ;
      RETURN( TRUE )
   END
END CreateSon ;


(*
   MakeObject - makes an object level node.
*)

PROCEDURE MakeObject (ModuleName: Name; n: PtrToNode; Level: CARDINAL) ;
VAR
   son: PtrToNode ;
BEGIN
   IF CreateSon(son, ModuleName, ObjName, n, Level+1)
   THEN
      IF ModExists(ModuleName)
      THEN
         MakeMod(ModuleName, son, Level+1)
      END
   END
END MakeObject ;
 

(*
   MakeExe - makes a top level node and attempts to import any
             dependancies from this source file and it then increases
             all dependancies by this top level.
*)
 
PROCEDURE MakeExe (ModuleName: Name; n: PtrToNode; Level: CARDINAL) ;
BEGIN
   MakeObject(ModuleName, n, Level+1) ;
   ImportViaMod(ModuleName, ModName, n, Level+1)
END MakeExe ;


(*
   MakeMod - make a module level node.
*)

PROCEDURE MakeMod (ModuleName: Name; n: PtrToNode; Level: CARDINAL) ;
VAR
   son: PtrToNode ;
BEGIN
   IF CreateSon(son, ModuleName, ModName, n, Level+1)
   THEN
      IF ModExists(ModuleName)
      THEN
         IF DefExists(ModuleName)
         THEN
            MakeDef(ModuleName, n, Level)
         END ;
         ImportViaMod(ModuleName, ModName, n, Level+1)
      ELSE
         printf1('warning no matching %a.mod\n', ModuleName)
      END
   END
END MakeMod ;
 

(*
   MakeDef - make a definition module level node.
*)

PROCEDURE MakeDef (ModuleName: Name; n: PtrToNode; Level: CARDINAL) ;
VAR
   son: PtrToNode ;
BEGIN
   IF CreateSon(son, ModuleName, DefName, n, Level+1)
   THEN
      ImportViaDef(ModuleName, DefName, n, Level)
   END
END MakeDef ;
 

(*
   Open - attempts to open a module, it will terminate if the module cannot be found.
*)

PROCEDURE Open (ModuleName, ExtName: Name; IsDefinition: BOOLEAN) ;
VAR
   a, b: String ;
BEGIN
   IF IsDefinition
   THEN
      IF NOT DefExists(ModuleName)
      THEN
         printf2('# fatal error: %a.%a does not exist\n', ModuleName, ExtName) ;
      END
   ELSE
      IF NOT ModExists(ModuleName)
      THEN
         printf2('# fatal error: %a.%a does not exist\n', ModuleName, ExtName) ;
      END
   END ;
   a   := CalculateFileName(InitStringCharStar(KeyToCharStar(ModuleName)),
                            Mark(InitStringCharStar(KeyToCharStar(ExtName)))) ;
   IF NOT (FindSourceFile(a, b) AND OpenSource(b))
   THEN
      HALT
   END ;
   a := KillString(a) ;
   b := KillString(b)
END Open ;


(*
   ImportViaDef - find all imports from a definition module.
                  Build up this subtree.
*)
 
PROCEDURE ImportViaDef (ModuleName, ExtName: Name;
                        n: PtrToNode; Level: CARDINAL) ;
VAR
   symbol   : Name ;
   son      : PtrToNode ;
   lasttoken: toktype ;
   ToDo     : List ;
BEGIN
   InitList(ToDo) ;
   Open(ModuleName, ExtName, TRUE) ;
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
      IF (currenttoken=fromtok) OR (currenttoken=importtok)
      THEN
         GetToken ;
         symbol := makekey(currentstring) ;
         IF DefExists(symbol)
         THEN
            IncludeItemIntoList(ToDo, symbol)
         END
      ELSE
         CloseSource ;
         LOOP
            symbol := GetItemFromList(ToDo, 1) ;
            IF symbol=NIL
            THEN
               KillList(ToDo) ;
               RETURN
            ELSE
               RemoveItemFromList(ToDo, symbol) ;
               IF CreateSon(son, symbol, DefName, n, Level+1)
               THEN
               END
            END
         END
      END
   END
END ImportViaDef ;

 
(*
   ImportViaMod - creates a dependancy tree from the module, ModuleName and
                  ExtName.
*)

PROCEDURE ImportViaMod (ModuleName, ExtName: Name ;
                        n: PtrToNode; Level: CARDINAL) ;
VAR
   symbol   : Name ;
   son      : PtrToNode ;
   lasttoken: toktype ;
   ToDo     : List ;
BEGIN
   InitList(ToDo) ;
   Open(ModuleName, ExtName, FALSE) ;
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
      IF (currenttoken=fromtok) OR (currenttoken=importtok)
      THEN
         GetToken ;
         symbol := makekey(currentstring) ;
         IF ModExists(symbol)
         THEN
            IncludeItemIntoList(ToDo, symbol)
         END
      ELSE
         CloseSource ;
         LOOP
            symbol := GetItemFromList(ToDo, 1) ;
            IF symbol=NIL
            THEN
               KillList(ToDo) ;
               RETURN
            ELSE
               RemoveItemFromList(ToDo, symbol) ;
               IF CreateSon(son, symbol, ExtName, n, Level+1)
               THEN
               END
            END
         END
      END
   END
END ImportViaMod ;
 

(*
   Obj - creates an object level node if the module, ModuleName.mod,
         exists.
*)

PROCEDURE Obj (ModuleName: Name; son: PtrToNode; Level: CARDINAL) ;
BEGIN
   IF ModExists(ModuleName)
   THEN
      MakeMod(ModuleName, son, Level+1)
   ELSE
      printf1('warning no matching %a.mod\n', ModuleName)
   END
END Obj ;


(*
   InFileTable - returns TRUE if, Module, is to be found in the Files list.
*)

PROCEDURE InFileTable (modname, ext: Name;
                       VAR s: PtrToSource) : BOOLEAN ;
BEGIN
   s := Files ;
   WHILE s#NIL DO
      IF (s^.ModName=modname) AND (s^.Ext=ext)
      THEN
         RETURN( TRUE )
      ELSE
         s := s^.Next
      END
   END ;
   RETURN( FALSE )
END InFileTable ;


(*
   IsASon - returns TRUE if, son, is a son of, father.
*)

PROCEDURE IsASon (father, son: PtrToNode) : BOOLEAN ;
BEGIN
   RETURN( (son#NIL) AND IsItemInList(father^.Sons, son) )
END IsASon ;


(*
   DeleteSon - deletes, son, from, father.
*)

PROCEDURE DeleteSon (father, son: PtrToNode) ;
BEGIN
   RemoveItemFromList(father^.Sons, son)
END DeleteSon ;


(*
   DeleteFromFather - deletes all entries for son from its direct Father.
*)

PROCEDURE DeleteFromFather (son: PtrToNode) ;
BEGIN
   DeleteSon(son^.Father, son)
END DeleteFromFather ;


(*
   SubNode - returns TRUE if son is a desendant of father, otherwise
             FALSE is returned.
*)

PROCEDURE SubNode (father, son: PtrToNode) : BOOLEAN ;
VAR
   t   : PtrToNode ;
   i, n: CARDINAL ;
BEGIN
   IF father=son
   THEN
      RETURN( TRUE )
   ELSE
      i := 1 ;
      n := NoOfItemsInList(father^.Sons) ;
      WHILE i<=n DO
         t := GetItemFromList(father^.Sons, i) ;
         IF (t#NIL) AND (t^.Father=father)
         THEN
            IF SubNode(t, son)
            THEN
               RETURN( TRUE )
            END
         END ;
         INC(i)
      END
   END ;
   RETURN( FALSE )
END SubNode ;


(*
   IncreaseLevel - adds, l, to the Depth field for n and its SubTree
*)

PROCEDURE IncreaseLevel (n: PtrToNode ; l: CARDINAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
   INC(n^.Source^.Depth, l) ;
   i := 1 ;
   j := NoOfItemsInList(n^.Sons) ;
   WHILE i<=j DO
      IF GetItemFromList(n^.Sons, i)#NIL
      THEN
         IncreaseLevel(GetItemFromList(n^.Sons, i), l)
      END ;
      INC(i)
   END
END IncreaseLevel ;


(*
   AddSon - adds, son, to be a son of father.
*)

PROCEDURE AddSon (father, son: PtrToNode) ; 
BEGIN
   IncludeItemIntoList(father^.Sons, son)
END AddSon ;


(*
   ModExists - returns TRUE if ModuleName.mod exists in a file.
*)

PROCEDURE ModExists (ModuleName: Name) : BOOLEAN ;
VAR
   a, b : String ;
   Key  : CARDINAL ;
BEGIN
   a := InitStringCharStar(KeyToCharStar(ModuleName)) ;
   Key := GetSymKey(ModExistsTree, ModuleName) ;
   IF Key=KeyFound
   THEN
      a := KillString(a) ;
      RETURN( TRUE )
   ELSIF Key=KeyNotFound
   THEN
      a := KillString(a) ;
      RETURN( FALSE )
   ELSIF Key=NulKey
   THEN
      b := CalculateFileName(a, Mark(InitString('mod'))) ;
      IF FindSourceFile(b, b)
      THEN
         b := KillString(b) ;
         PutSymKey(ModExistsTree, ModuleName, KeyFound) ;
         RETURN( TRUE )
      ELSE
         b := KillString(b) ;
         PutSymKey(ModExistsTree, ModuleName, KeyNotFound) ;
         RETURN( FALSE )
      END
   END ;
   HALT (* incorrect value of key *)
END ModExists ;


(*
   DefExists - returns TRUE if ModuleName.def exists in a file.
*)

PROCEDURE DefExists (ModuleName: Name) : BOOLEAN ;
VAR
   a, b : String ;
   Key  : CARDINAL ;
BEGIN
   a := InitStringCharStar(KeyToCharStar(ModuleName)) ;
   Key := GetSymKey(DefExistsTree, ModuleName) ;
   IF Key=KeyFound
   THEN
      a := KillString(a) ;
      RETURN( TRUE )
   ELSIF Key=KeyNotFound
   THEN
      a := KillString(a) ;
      RETURN( FALSE )
   ELSIF Key=NulKey
   THEN
      b := CalculateFileName(a, Mark(InitString('def'))) ;
      IF FindSourceFile(b, b)
      THEN
         b := KillString(b) ;
         PutSymKey(DefExistsTree, ModuleName, KeyFound) ;
         RETURN( TRUE )
      ELSE
         b := KillString(b) ;
         PutSymKey(DefExistsTree, ModuleName, KeyNotFound) ;
         RETURN( FALSE )
      END
   END ;
   HALT (* incorrect value of key *)
END DefExists ;


(*
   DisplayTree - dumps the tree in english, used for debugging only.
*)

PROCEDURE DisplayTree (n: PtrToNode) ;
VAR
   i, j   : CARDINAL ;
   t      : PtrToNode ;
BEGIN
   printf3('%a.%a at depth (%d) has sons (',
           n^.Source^.ModName, n^.Source^.Ext, n^.Source^.Depth) ;
   i := 1 ;
   j := NoOfItemsInList(n^.Sons) ;
   WHILE i<=j DO
      t := GetItemFromList(n^.Sons, i) ;
      IF t#NIL
      THEN
         printf2('%a.%a, ', t^.Source^.ModName, t^.Source^.Ext)
      END ;
      INC(i)
   END ;
   printf0(')\n')
END DisplayTree ;
 
 
(*
   DisplaySource - displays the source files.
*)
 
PROCEDURE DisplaySource ;
VAR
   f: PtrToSource ;
BEGIN
   f := Files ;
   WHILE f#NIL DO
      WITH f^ DO
         IF (Ext=DefName) OR (Ext=ModName)
         THEN
            printf3('%d   %a.%a\n', Depth, ModName, Ext)
         END
      END ;
      f := f^.Next
   END
END DisplaySource ;
 

(*
   DisplayDeps - display all dependants of, n, in a space separated list suitable for a Makefile.
*)

PROCEDURE DisplayDeps (n: PtrToNode) ;
VAR
   i, j: CARDINAL ;
   t   : PtrToNode ;
   name,
   ext,
   s, p: String ;
BEGIN
   i := 1 ;
   j := NoOfItemsInList(n^.Sons) ;
   WHILE i<=j DO
      t := GetItemFromList(n^.Sons, i) ;
      IF t#NIL
      THEN
         WITH t^.Source^ DO
            name := InitStringCharStar(KeyToCharStar(ModName)) ;
            ext  := InitStringCharStar(KeyToCharStar(Ext)) ;
            IF Ext=ObjName
            THEN
               FormatString(' ') ;
               s := Add(DestDirectory, CalculateFileName(name, ext)) ;
               FormatS(s)
            ELSE
               FormatString(' ') ;
               s := CalculateFileName(name, ext) ;
               IF FindSourceFile(s, p)
               THEN
               END ;
               FormatS(p) ;
               p := KillString(p)
            END ;
            s    := KillString(s) ;
            name := KillString(name) ;
            ext  := KillString(ext)
         END
      END ;
      INC(i)
   END ;
   FormatLn
END DisplayDeps ;


(*
   DisplayDependants - displays the dependants of node, n.
*)

PROCEDURE DisplayDependants (n: PtrToNode) ;
VAR
   name,
   ext,
   s   : String ;
BEGIN
   name := InitStringCharStar(KeyToCharStar(n^.Source^.ModName)) ;
   ext  := InitStringCharStar(KeyToCharStar(n^.Source^.Ext)) ;
   IF n^.Source^.Ext=ObjName
   THEN
      s := Add(DestDirectory, CalculateFileName(name, ext))
   ELSIF n^.Source^.Ext=ExeName
   THEN
      s := Dup(name)
   ELSE
      s := CalculateFileName(name, ext)
   END ;
   FormatS(s) ;
   s    := KillString(s) ;
   name := KillString(name) ;
   ext  := KillString(ext) ;
   FormatString(':') ;
   DisplayDeps(n)
END DisplayDependants;


(*
   DisplayBuild - displays the command to build node, n.
*)

PROCEDURE DisplayBuild (n: PtrToNode) ;
VAR
   s,
   obj,
   ext,
   name: String ;
BEGIN
   IF NOT DependOnly
   THEN
      name := InitStringCharStar(KeyToCharStar(n^.Source^.ModName)) ;
      ext  := InitStringCharStar(KeyToCharStar(n^.Source^.Ext)) ;
      s    := NIL ;
      IF (n^.Source^.Ext=ExeName) AND ModExists(n^.Source^.ModName)
      THEN
         FormatChar(tab) ;
         FormatString('$(M2L) ') ;
         s := CalculateFileName(name, Mark(InitString('mod'))) ;
         FormatS(s) ;
         FormatLn
      ELSIF (n^.Source^.Ext=ObjName) AND ModExists(n^.Source^.ModName)
      THEN
         FormatChar(tab) ;
         FormatString('$(M2C) $(OPTIONS) ') ;
         IF NOT EqualArray(DestDirectory, '')
         THEN
            obj := InitStringCharStar(KeyToCharStar(ObjName)) ;
            s := Sprintf2(Mark(InitString(' -o %s%a ')), DestDirectory, CalculateFileName(name, obj)) ;
            FormatS(s) ;
            s    := KillString(s) ;
            obj  := KillString(obj) ;
            name := KillString(name)
         END ;
         name := InitStringCharStar(KeyToCharStar(n^.Source^.ModName)) ;
         s := CalculateFileName(name, Mark(InitString('mod'))) ;
         FormatS(s) ;
         FormatLn
      END ;
      ext  := KillString(ext) ;
      name := KillString(name) ;
      s    := KillString(s)
   END
END DisplayBuild ;


(*
   Version for GNU make - gm2
*)

PROCEDURE DisplayDependency (n: PtrToNode) ;
VAR
   i, j: CARDINAL ;
   t   : PtrToNode ;
BEGIN
   j := NoOfItemsInList(n^.Sons) ;
   IF j>0
   THEN
      DisplayDependants(n) ;
      DisplayBuild(n) ;
      i := 1 ;
      WHILE i<=j DO
         t := GetItemFromList(n^.Sons, i) ;
         IF (t#NIL) AND (t^.Father=n)
         THEN
            DisplayDependency(t)
         END ;
         INC(i)
      END
   END
END DisplayDependency ;


(*
   DisplayObjects - displays a target, OBJS, with all objects as dependants.
*)

PROCEDURE DisplayObjects (n: PtrToNode) ;
BEGIN
   FormatString('OBJS:') ;
   DisplayDeps(n)
END DisplayObjects ;


(*
   M2FMake - 
*)

PROCEDURE M2FMake ;
VAR
   s: String ;
BEGIN
   FormatString('OPTIONS= ') ;
   INC(CurrentArg) ;
   IF GetArg(s, CurrentArg)
   THEN
      s := KillString(WriteS(StdOut, s)) ;
      INC(CurrentArg)
   ELSE
      IF NOT GM2PathFound
      THEN
         GM2Path := GetSearchPath()
      END ;
      IF NOT EqualArray(GM2Path, '')
      THEN
         FormatString('-M ') ; FormatS(GM2Path) ; FormatString(' ')
      END ;
      s := GetOptions() ;
      IF NOT EqualArray(s, '')
      THEN
         FormatChar(' ') ; FormatS(s)
      END
   END ;
   FormatLn ;
   FormatString('M2C= ') ;
   IF GetArg(s, CurrentArg)
   THEN
      FormatS(s) ;
      INC(CurrentArg)
   ELSE
      FormatString('m2f')
   END ;
   FormatLn ;
   FormatString('M2L= $(M2C) $(OPTIONS) -l ') ;
   FormatLn
END M2FMake ;


(*
   GM2Make - 
*)

PROCEDURE GM2Make ;
VAR
   s: String ;
BEGIN
   FormatString('OPTIONS= ') ;
   INC(CurrentArg) ;
   IF GetArg(s, CurrentArg)
   THEN
      FormatS(s) ;
      INC(CurrentArg)
   ELSE
      IF NOT GM2PathFound
      THEN
         GM2Path := GetSearchPath()
      END ;
      IF NOT EqualArray(GM2Path, '')
      THEN
         FormatString('-M ') ; FormatS(GM2Path) ; FormatString(' ')
      END ;
      s := GetOptions() ;
      IF NOT EqualArray(s, '')
      THEN
         FormatChar(' ') ; FormatS(s)
      END
   END ;
   FormatLn ;
   FormatString('M2C= ') ;
   IF GetArg(s, CurrentArg)
   THEN
      FormatS(s) ;
      INC(CurrentArg)
   ELSE
      FormatString('gm2 -c')
   END ;
   FormatLn ;
   FormatString('M2L= gm2 $(OPTIONS) ') ;
   FormatLn
END GM2Make ;


(*
   UnixMake - generates the makefile for UNIX gm2
*)

PROCEDURE UnixMake (main: PtrToNode) ;
BEGIN
   IF NOT DependOnly
   THEN
      IF UsingGCCBackend
      THEN
         GM2Make
      ELSE
         M2FMake
      END
   END ;
   DisplayDependency(main) ;
   DisplayObjects(main)
END UnixMake ;


VAR
   CurrentLineBreak: CARDINAL ;

(*
   FormatChar - writes a character and wraps around MaxLineBreak.
*)

PROCEDURE FormatChar (ch: CHAR) ;
BEGIN
   IF ch=tab
   THEN
      INC(CurrentLineBreak, 8)
   ELSE
      INC(CurrentLineBreak)
   END ;
   Write(ch)
END FormatChar ;


(*
   FormatString - writes a string which wraps around MaxLineBreak.
*)

PROCEDURE FormatString (a: ARRAY OF CHAR) ;
BEGIN
   IF StrLen(a)+CurrentLineBreak>MaxLineBreak
   THEN
      WriteString(' \') ;
      FormatLn ;
      FormatChar(tab)
   END ;
   WriteString(a) ;
   INC(CurrentLineBreak, StrLen(a))
END FormatString ;


(*
   FormatS - writes a string, s, which wraps around MaxLineBreak.
*)

PROCEDURE FormatS (s: String) ;
BEGIN
   IF Length(s)+CurrentLineBreak>MaxLineBreak
   THEN
      WriteString(' \') ;
      FormatLn ;
      FormatChar(tab)
   END ;
   INC(CurrentLineBreak, Length(WriteS(StdOut, s)))
END FormatS ;


(*
   FormatLn - writes a newline and sets CurrentLineBreak to zero.
*)

PROCEDURE FormatLn ;
BEGIN
   printf0('\n') ;
   CurrentLineBreak := 0
END FormatLn ;


(*
   InitName - initializes the name of this program,
              normally gm2m for GNU Modula-2 and m2m for m2f.
*)

PROCEDURE InitName ;
BEGIN
   IF GetArg(ProgName, 0)
   THEN
   END
END InitName ;


(*
   ParseArgs - parses the arguments and then calls the main program.
*)

PROCEDURE ParseArgs ;
VAR
   s, e: String ;
BEGIN
   CurrentArg := 1 ;
   IF Narg()>1
   THEN
      WHILE GetArg(s, CurrentArg) DO
         IF EqualArray(s, '-M')
         THEN
            INC(CurrentArg) ;
            GM2PathFound := GetArg(GM2Path, CurrentArg)
         ELSIF EqualArray(s, '-P')
         THEN
            INC(CurrentArg) ;
            M2MPathFound := GetArg(M2MPath, CurrentArg)
         ELSIF EqualArray(s, '-o')
         THEN
            INC(CurrentArg) ;
            IF NOT GetArg(DestDirectory, CurrentArg)
            THEN
               printf0('-o expecting a destination directory\n') ;
               exit(1)
            END
         ELSIF EqualArray(s, '-e')
         THEN
            INC(CurrentArg) ;
            IF NOT GetArg(e, CurrentArg)
            THEN
               printf0('-e expecting an extension argument\n') ;
               exit(1)
            END ;
            ObjName := makekey(string(e)) ;
            e := KillString(e)
         ELSIF EqualArray(s, '-depend')
         THEN
            DependOnly := TRUE
         ELSE
            IF M2MPathFound
            THEN
               InitSearchPath(M2MPath)
            ELSIF GM2PathFound
            THEN
               InitSearchPath(GM2Path)
            END ;
            IF ModExists(makekey(string(s)))
            THEN
               main := NewNode() ;
               main^.Father := NIL ;
               main^.Source := NewSource() ;
               WITH main^.Source^ DO
                  SourceNode := main ;
                  ModName := makekey(string(s)) ;
                  main^.Source^.Ext := ExeName ;
                  (* .exe is never written out, it is just a unique extension
                     representing not matching .o .mod or .def *)
                  e := CalculateFileName(s, Mark(InitString('exe'))) ;
                  Next := Files ;
                  MakeExe(ModName, main, 0) ;
                  UnixMake(main) ;
                  e := KillString(e)
               END ;
               Files := main^.Source
            ELSE
               printf2('%s: cannot find %s.mod\n', ProgName, s)
            END
         END ;
         s := KillString(s) ;
         INC(CurrentArg)
      END
   ELSE
      printf2('Usage: %a [-e extension][-o destination directory][-depend][-P %a searchpath][-M compiler searchpath] modulename [options] [compiler]\n', ProgName, ProgName)
   END
END ParseArgs ;


(*
   Init - initializes the data structures and calls ParseArgs
*)

PROCEDURE Init ;
BEGIN
   InitName ;
   ExeName     := MakeKey('exe') ;
   ObjName     := MakeKey('o') ;
   DefName     := MakeKey('def') ;
   ModName     := MakeKey('mod') ;
   KeyFound    := MakeKey('KeyFound') ;
   KeyNotFound := MakeKey('KeyNotFound') ;

   CurrentLineBreak := 0 ;
   Files := NIL ;
   InitTree(DefExistsTree) ;
   InitTree(ModExistsTree) ;
   DestDirectory := NIL ;
   M2MPath := NIL ;
   GM2Path := NIL ;
   M2MPathFound := FALSE ;
   GM2PathFound := FALSE ;
   DependOnly := FALSE ;
   ParseArgs
END Init ;


BEGIN
   Init
END m2m.
